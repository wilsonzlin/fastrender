//! Scroll-driven animation utilities.
//!
//! This module provides lightweight timeline evaluation for scroll and view
//! timelines along with keyframe sampling helpers. It is intentionally small
//! and self contained so it can be reused by layout/paint and tests without
//! wiring a full animation engine.

use std::collections::HashMap;

use crate::css::types::{Keyframe, KeyframesRule};
use crate::geometry::{Point, Rect, Size};
use crate::paint::display_list::{Transform2D, Transform3D};
use crate::style::inline_axis_is_horizontal;
use crate::style::properties::apply_declaration_with_base;
use crate::style::types::AnimationRange;
use crate::style::types::AnimationTimeline;
use crate::style::types::BackgroundPosition;
use crate::style::types::BackgroundPositionComponent;
use crate::style::types::BackgroundSize;
use crate::style::types::BackgroundSizeComponent;
use crate::style::types::BackgroundSizeKeyword;
use crate::style::types::BasicShape;
use crate::style::types::ClipPath;
use crate::style::types::ClipRadii;
use crate::style::types::FilterColor;
use crate::style::types::FilterFunction;
use crate::style::types::RangeOffset;
use crate::style::types::ReferenceBox;
use crate::style::types::ScrollTimeline;
use crate::style::types::ShapeRadius;
use crate::style::types::TimelineAxis;
use crate::style::types::TimelineOffset;
use crate::style::types::ViewTimeline;
use crate::style::types::ViewTimelinePhase;
use crate::style::types::WritingMode;
use crate::style::values::Length;
use crate::style::ComputedStyle;
use crate::tree::fragment_tree::{FragmentNode, FragmentTree};
use std::collections::HashSet;
use std::mem::discriminant;
use std::sync::Arc;

use crate::style::color::Rgba;

/// Resolved animated property value used by interpolation/apply steps.
#[derive(Debug, Clone)]
pub enum AnimatedValue {
  Opacity(f32),
  Color(Rgba),
  Transform(Vec<crate::css::types::Transform>),
  Filter(Vec<FilterFunction>),
  BackdropFilter(Vec<FilterFunction>),
  ClipPath(ClipPath),
  BackgroundPosition(Vec<BackgroundPosition>),
  BackgroundSize(Vec<BackgroundSize>),
  BorderRadius([Length; 4]),
}

#[derive(Clone, Copy)]
struct AnimationResolveContext {
  viewport: Size,
  element_size: Size,
}

impl AnimationResolveContext {
  fn new(viewport: Size, element_size: Size) -> Self {
    Self {
      viewport,
      element_size,
    }
  }
}

fn lerp(a: f32, b: f32, t: f32) -> f32 {
  a + (b - a) * t
}

fn lerp_color(a: Rgba, b: Rgba, t: f32) -> Rgba {
  let lerp_chan =
    |ca: u8, cb: u8| -> u8 { lerp(ca as f32, cb as f32, t).round().clamp(0.0, 255.0) as u8 };
  Rgba::new(
    lerp_chan(a.r, b.r),
    lerp_chan(a.g, b.g),
    lerp_chan(a.b, b.b),
    lerp(a.a, b.a, t),
  )
}

fn resolve_length_px(
  len: &Length,
  percent_base: Option<f32>,
  style: &ComputedStyle,
  ctx: &AnimationResolveContext,
) -> f32 {
  len
    .resolve_with_context(
      percent_base,
      ctx.viewport.width,
      ctx.viewport.height,
      style.font_size,
      style.root_font_size,
    )
    .unwrap_or_else(|| len.to_px())
}

#[derive(Debug, Clone)]
struct ResolvedShadow {
  offset_x: f32,
  offset_y: f32,
  blur: f32,
  spread: f32,
  color: Rgba,
}

#[derive(Debug, Clone)]
struct ResolvedPositionComponent {
  alignment: f32,
  offset: f32,
}

#[derive(Debug, Clone)]
struct ResolvedBackgroundPosition {
  x: ResolvedPositionComponent,
  y: ResolvedPositionComponent,
}

#[derive(Debug, Clone)]
enum ResolvedSizeComponent {
  Auto,
  Length(f32),
}

#[derive(Debug, Clone)]
enum ResolvedBackgroundSize {
  Keyword(BackgroundSizeKeyword),
  Explicit(ResolvedSizeComponent, ResolvedSizeComponent),
}

#[derive(Debug, Clone)]
enum ResolvedClipPath {
  None,
  Box(ReferenceBox),
  Inset {
    top: f32,
    right: f32,
    bottom: f32,
    left: f32,
    radii: Option<[f32; 4]>,
    reference: Option<ReferenceBox>,
  },
  Circle {
    radius: f32,
    position: ResolvedBackgroundPosition,
    reference: Option<ReferenceBox>,
  },
  Ellipse {
    radius_x: f32,
    radius_y: f32,
    position: ResolvedBackgroundPosition,
    reference: Option<ReferenceBox>,
  },
}

#[derive(Debug, Clone)]
enum ResolvedFilter {
  Blur(f32),
  Brightness(f32),
  Contrast(f32),
  Grayscale(f32),
  Sepia(f32),
  Saturate(f32),
  HueRotate(f32),
  Invert(f32),
  Opacity(f32),
  DropShadow(ResolvedShadow),
  Url(String),
}

fn resolve_filter_color(color: &FilterColor, current_color: Rgba) -> Rgba {
  match color {
    FilterColor::CurrentColor => current_color,
    FilterColor::Color(c) => *c,
  }
}

fn resolve_filter_list(
  filters: &[FilterFunction],
  style: &ComputedStyle,
  ctx: &AnimationResolveContext,
) -> Vec<ResolvedFilter> {
  filters
    .iter()
    .map(|f| match f {
      FilterFunction::Blur(len) => ResolvedFilter::Blur(resolve_length_px(len, None, style, ctx)),
      FilterFunction::Brightness(v) => ResolvedFilter::Brightness(*v),
      FilterFunction::Contrast(v) => ResolvedFilter::Contrast(*v),
      FilterFunction::Grayscale(v) => ResolvedFilter::Grayscale(*v),
      FilterFunction::Sepia(v) => ResolvedFilter::Sepia(*v),
      FilterFunction::Saturate(v) => ResolvedFilter::Saturate(*v),
      FilterFunction::HueRotate(v) => ResolvedFilter::HueRotate(*v),
      FilterFunction::Invert(v) => ResolvedFilter::Invert(*v),
      FilterFunction::Opacity(v) => ResolvedFilter::Opacity(*v),
      FilterFunction::DropShadow(shadow) => ResolvedFilter::DropShadow(ResolvedShadow {
        offset_x: resolve_length_px(&shadow.offset_x, None, style, ctx),
        offset_y: resolve_length_px(&shadow.offset_y, None, style, ctx),
        blur: resolve_length_px(&shadow.blur_radius, None, style, ctx),
        spread: resolve_length_px(&shadow.spread, None, style, ctx),
        color: resolve_filter_color(&shadow.color, style.color),
      }),
      FilterFunction::Url(u) => ResolvedFilter::Url(u.clone()),
    })
    .collect()
}

fn interpolate_filters(
  a: &[ResolvedFilter],
  b: &[ResolvedFilter],
  t: f32,
) -> Option<Vec<ResolvedFilter>> {
  if a.len() != b.len() {
    return None;
  }

  let mut out = Vec::with_capacity(a.len());
  for (fa, fb) in a.iter().zip(b.iter()) {
    let next = match (fa, fb) {
      (ResolvedFilter::Blur(la), ResolvedFilter::Blur(lb)) => {
        ResolvedFilter::Blur(lerp(*la, *lb, t))
      }
      (ResolvedFilter::Brightness(la), ResolvedFilter::Brightness(lb)) => {
        ResolvedFilter::Brightness(lerp(*la, *lb, t))
      }
      (ResolvedFilter::Contrast(la), ResolvedFilter::Contrast(lb)) => {
        ResolvedFilter::Contrast(lerp(*la, *lb, t))
      }
      (ResolvedFilter::Grayscale(la), ResolvedFilter::Grayscale(lb)) => {
        ResolvedFilter::Grayscale(lerp(*la, *lb, t))
      }
      (ResolvedFilter::Sepia(la), ResolvedFilter::Sepia(lb)) => {
        ResolvedFilter::Sepia(lerp(*la, *lb, t))
      }
      (ResolvedFilter::Saturate(la), ResolvedFilter::Saturate(lb)) => {
        ResolvedFilter::Saturate(lerp(*la, *lb, t))
      }
      (ResolvedFilter::HueRotate(la), ResolvedFilter::HueRotate(lb)) => {
        ResolvedFilter::HueRotate(lerp(*la, *lb, t))
      }
      (ResolvedFilter::Invert(la), ResolvedFilter::Invert(lb)) => {
        ResolvedFilter::Invert(lerp(*la, *lb, t))
      }
      (ResolvedFilter::Opacity(la), ResolvedFilter::Opacity(lb)) => {
        ResolvedFilter::Opacity(lerp(*la, *lb, t))
      }
      (ResolvedFilter::DropShadow(sa), ResolvedFilter::DropShadow(sb)) => {
        ResolvedFilter::DropShadow(ResolvedShadow {
          offset_x: lerp(sa.offset_x, sb.offset_x, t),
          offset_y: lerp(sa.offset_y, sb.offset_y, t),
          blur: lerp(sa.blur, sb.blur, t),
          spread: lerp(sa.spread, sb.spread, t),
          color: lerp_color(sa.color, sb.color, t),
        })
      }
      (ResolvedFilter::Url(a), ResolvedFilter::Url(b)) if a == b => ResolvedFilter::Url(a.clone()),
      _ => return None,
    };
    out.push(next);
  }

  Some(out)
}

fn resolved_filters_to_functions(filters: &[ResolvedFilter]) -> Vec<FilterFunction> {
  filters
    .iter()
    .map(|f| match f {
      ResolvedFilter::Blur(v) => FilterFunction::Blur(Length::px(*v)),
      ResolvedFilter::Brightness(v) => FilterFunction::Brightness(*v),
      ResolvedFilter::Contrast(v) => FilterFunction::Contrast(*v),
      ResolvedFilter::Grayscale(v) => FilterFunction::Grayscale(*v),
      ResolvedFilter::Sepia(v) => FilterFunction::Sepia(*v),
      ResolvedFilter::Saturate(v) => FilterFunction::Saturate(*v),
      ResolvedFilter::HueRotate(v) => FilterFunction::HueRotate(*v),
      ResolvedFilter::Invert(v) => FilterFunction::Invert(*v),
      ResolvedFilter::Opacity(v) => FilterFunction::Opacity(*v),
      ResolvedFilter::DropShadow(s) => {
        FilterFunction::DropShadow(Box::new(crate::style::types::FilterShadow {
          offset_x: Length::px(s.offset_x),
          offset_y: Length::px(s.offset_y),
          blur_radius: Length::px(s.blur),
          spread: Length::px(s.spread),
          color: FilterColor::Color(s.color),
        }))
      }
      ResolvedFilter::Url(u) => FilterFunction::Url(u.clone()),
    })
    .collect()
}

fn resolved_filters_from_functions(filters: &[FilterFunction]) -> Vec<ResolvedFilter> {
  filters
    .iter()
    .map(|f| match f {
      FilterFunction::Blur(len) => ResolvedFilter::Blur(len.to_px()),
      FilterFunction::Brightness(v) => ResolvedFilter::Brightness(*v),
      FilterFunction::Contrast(v) => ResolvedFilter::Contrast(*v),
      FilterFunction::Grayscale(v) => ResolvedFilter::Grayscale(*v),
      FilterFunction::Sepia(v) => ResolvedFilter::Sepia(*v),
      FilterFunction::Saturate(v) => ResolvedFilter::Saturate(*v),
      FilterFunction::HueRotate(v) => ResolvedFilter::HueRotate(*v),
      FilterFunction::Invert(v) => ResolvedFilter::Invert(*v),
      FilterFunction::Opacity(v) => ResolvedFilter::Opacity(*v),
      FilterFunction::DropShadow(shadow) => ResolvedFilter::DropShadow(ResolvedShadow {
        offset_x: shadow.offset_x.to_px(),
        offset_y: shadow.offset_y.to_px(),
        blur: shadow.blur_radius.to_px(),
        spread: shadow.spread.to_px(),
        color: resolve_filter_color(&shadow.color, Rgba::BLACK),
      }),
      FilterFunction::Url(u) => ResolvedFilter::Url(u.clone()),
    })
    .collect()
}

fn resolve_background_position_component(
  comp: &BackgroundPositionComponent,
  axis_base: f32,
  style: &ComputedStyle,
  ctx: &AnimationResolveContext,
) -> ResolvedPositionComponent {
  ResolvedPositionComponent {
    alignment: comp.alignment,
    offset: resolve_length_px(&comp.offset, Some(axis_base), style, ctx),
  }
}

fn resolve_background_positions(
  list: &[BackgroundPosition],
  style: &ComputedStyle,
  ctx: &AnimationResolveContext,
) -> Vec<ResolvedBackgroundPosition> {
  let width = ctx.element_size.width;
  let height = ctx.element_size.height;
  list
    .iter()
    .map(|pos| match pos {
      BackgroundPosition::Position { x, y } => ResolvedBackgroundPosition {
        x: resolve_background_position_component(x, width, style, ctx),
        y: resolve_background_position_component(y, height, style, ctx),
      },
    })
    .collect()
}

fn interpolate_background_positions(
  a: &[ResolvedBackgroundPosition],
  b: &[ResolvedBackgroundPosition],
  t: f32,
) -> Option<Vec<ResolvedBackgroundPosition>> {
  if a.len() != b.len() {
    return None;
  }
  let mut out = Vec::with_capacity(a.len());
  for (pa, pb) in a.iter().zip(b.iter()) {
    out.push(ResolvedBackgroundPosition {
      x: ResolvedPositionComponent {
        alignment: lerp(pa.x.alignment, pb.x.alignment, t),
        offset: lerp(pa.x.offset, pb.x.offset, t),
      },
      y: ResolvedPositionComponent {
        alignment: lerp(pa.y.alignment, pb.y.alignment, t),
        offset: lerp(pa.y.offset, pb.y.offset, t),
      },
    });
  }
  Some(out)
}

fn resolved_positions_to_background(
  list: &[ResolvedBackgroundPosition],
) -> Vec<BackgroundPosition> {
  list
    .iter()
    .map(|p| BackgroundPosition::Position {
      x: BackgroundPositionComponent {
        alignment: p.x.alignment,
        offset: Length::px(p.x.offset),
      },
      y: BackgroundPositionComponent {
        alignment: p.y.alignment,
        offset: Length::px(p.y.offset),
      },
    })
    .collect()
}

fn background_positions_to_resolved(
  list: &[BackgroundPosition],
) -> Vec<ResolvedBackgroundPosition> {
  list
    .iter()
    .map(|p| match p {
      BackgroundPosition::Position { x, y } => ResolvedBackgroundPosition {
        x: ResolvedPositionComponent {
          alignment: x.alignment,
          offset: x.offset.to_px(),
        },
        y: ResolvedPositionComponent {
          alignment: y.alignment,
          offset: y.offset.to_px(),
        },
      },
    })
    .collect()
}

fn resolve_background_sizes(
  list: &[BackgroundSize],
  style: &ComputedStyle,
  ctx: &AnimationResolveContext,
) -> Vec<ResolvedBackgroundSize> {
  let width = ctx.element_size.width;
  let height = ctx.element_size.height;
  list
    .iter()
    .map(|size| match size {
      BackgroundSize::Keyword(k) => ResolvedBackgroundSize::Keyword(*k),
      BackgroundSize::Explicit(x, y) => ResolvedBackgroundSize::Explicit(
        match x {
          BackgroundSizeComponent::Auto => ResolvedSizeComponent::Auto,
          BackgroundSizeComponent::Length(l) => {
            ResolvedSizeComponent::Length(resolve_length_px(l, Some(width), style, ctx))
          }
        },
        match y {
          BackgroundSizeComponent::Auto => ResolvedSizeComponent::Auto,
          BackgroundSizeComponent::Length(l) => {
            ResolvedSizeComponent::Length(resolve_length_px(l, Some(height), style, ctx))
          }
        },
      ),
    })
    .collect()
}

fn interpolate_background_sizes(
  a: &[ResolvedBackgroundSize],
  b: &[ResolvedBackgroundSize],
  t: f32,
) -> Option<Vec<ResolvedBackgroundSize>> {
  if a.len() != b.len() {
    return None;
  }

  let mut out = Vec::with_capacity(a.len());
  for (sa, sb) in a.iter().zip(b.iter()) {
    let next = match (sa, sb) {
      (ResolvedBackgroundSize::Keyword(ka), ResolvedBackgroundSize::Keyword(kb)) => {
        if ka == kb {
          ResolvedBackgroundSize::Keyword(*ka)
        } else {
          return None;
        }
      }
      (ResolvedBackgroundSize::Explicit(xa, ya), ResolvedBackgroundSize::Explicit(xb, yb)) => {
        let interp_component = |ca: &ResolvedSizeComponent,
                                cb: &ResolvedSizeComponent|
         -> Option<ResolvedSizeComponent> {
          match (ca, cb) {
            (ResolvedSizeComponent::Auto, ResolvedSizeComponent::Auto) => {
              Some(ResolvedSizeComponent::Auto)
            }
            (ResolvedSizeComponent::Length(la), ResolvedSizeComponent::Length(lb)) => {
              Some(ResolvedSizeComponent::Length(lerp(*la, *lb, t)))
            }
            _ => None,
          }
        };
        let x = interp_component(xa, xb)?;
        let y = interp_component(ya, yb)?;
        ResolvedBackgroundSize::Explicit(x, y)
      }
      _ => return None,
    };
    out.push(next);
  }
  Some(out)
}

fn resolved_sizes_to_background(list: &[ResolvedBackgroundSize]) -> Vec<BackgroundSize> {
  list
    .iter()
    .map(|size| match size {
      ResolvedBackgroundSize::Keyword(k) => BackgroundSize::Keyword(*k),
      ResolvedBackgroundSize::Explicit(x, y) => {
        let to_comp = |c: &ResolvedSizeComponent| match c {
          ResolvedSizeComponent::Auto => BackgroundSizeComponent::Auto,
          ResolvedSizeComponent::Length(l) => BackgroundSizeComponent::Length(Length::px(*l)),
        };
        BackgroundSize::Explicit(to_comp(x), to_comp(y))
      }
    })
    .collect()
}

fn background_sizes_to_resolved(list: &[BackgroundSize]) -> Vec<ResolvedBackgroundSize> {
  list
    .iter()
    .map(|size| match size {
      BackgroundSize::Keyword(k) => ResolvedBackgroundSize::Keyword(*k),
      BackgroundSize::Explicit(x, y) => ResolvedBackgroundSize::Explicit(
        match x {
          BackgroundSizeComponent::Auto => ResolvedSizeComponent::Auto,
          BackgroundSizeComponent::Length(l) => ResolvedSizeComponent::Length(l.to_px()),
        },
        match y {
          BackgroundSizeComponent::Auto => ResolvedSizeComponent::Auto,
          BackgroundSizeComponent::Length(l) => ResolvedSizeComponent::Length(l.to_px()),
        },
      ),
    })
    .collect()
}

fn resolve_radius(len: &Length, style: &ComputedStyle, ctx: &AnimationResolveContext) -> f32 {
  let base = ctx.element_size.width.min(ctx.element_size.height);
  resolve_length_px(len, Some(base), style, ctx)
}

fn resolve_border_radii(style: &ComputedStyle, ctx: &AnimationResolveContext) -> [f32; 4] {
  [
    resolve_radius(&style.border_top_left_radius, style, ctx),
    resolve_radius(&style.border_top_right_radius, style, ctx),
    resolve_radius(&style.border_bottom_left_radius, style, ctx),
    resolve_radius(&style.border_bottom_right_radius, style, ctx),
  ]
}

fn interpolate_radii(a: [f32; 4], b: [f32; 4], t: f32) -> [f32; 4] {
  [
    lerp(a[0], b[0], t),
    lerp(a[1], b[1], t),
    lerp(a[2], b[2], t),
    lerp(a[3], b[3], t),
  ]
}

fn resolve_shape_radius(
  radius: &ShapeRadius,
  axis: f32,
  style: &ComputedStyle,
  ctx: &AnimationResolveContext,
) -> Option<f32> {
  match radius {
    ShapeRadius::Length(l) => Some(resolve_length_px(l, Some(axis), style, ctx)),
    // Keywords require reference box distances; fall back to discrete handling for now.
    ShapeRadius::ClosestSide | ShapeRadius::FarthestSide => None,
  }
}

fn resolve_clip_path(
  path: &ClipPath,
  style: &ComputedStyle,
  ctx: &AnimationResolveContext,
) -> Option<ResolvedClipPath> {
  match path {
    ClipPath::None => Some(ResolvedClipPath::None),
    ClipPath::Box(b) => Some(ResolvedClipPath::Box(*b)),
    ClipPath::BasicShape(shape, reference) => match shape.as_ref() {
      BasicShape::Inset {
        top,
        right,
        bottom,
        left,
        border_radius,
      } => {
        let width = ctx.element_size.width;
        let height = ctx.element_size.height;
        let radii = border_radius.as_ref().and_then(|r| {
          Some([
            resolve_length_px(&r.top_left, Some(width.min(height)), style, ctx),
            resolve_length_px(&r.top_right, Some(width.min(height)), style, ctx),
            resolve_length_px(&r.bottom_left, Some(width.min(height)), style, ctx),
            resolve_length_px(&r.bottom_right, Some(width.min(height)), style, ctx),
          ])
        });
        Some(ResolvedClipPath::Inset {
          top: resolve_length_px(top, Some(height), style, ctx),
          right: resolve_length_px(right, Some(width), style, ctx),
          bottom: resolve_length_px(bottom, Some(height), style, ctx),
          left: resolve_length_px(left, Some(width), style, ctx),
          radii,
          reference: *reference,
        })
      }
      BasicShape::Circle { radius, position } => {
        let width = ctx.element_size.width;
        let height = ctx.element_size.height;
        let radius_px = resolve_shape_radius(radius, width.min(height), style, ctx)?;
        let resolved_pos = resolve_background_positions(&[*position], style, ctx).pop()?;
        Some(ResolvedClipPath::Circle {
          radius: radius_px,
          position: resolved_pos,
          reference: *reference,
        })
      }
      BasicShape::Ellipse {
        radius_x,
        radius_y,
        position,
      } => {
        let width = ctx.element_size.width;
        let height = ctx.element_size.height;
        let rx = resolve_shape_radius(radius_x, width, style, ctx)?;
        let ry = resolve_shape_radius(radius_y, height, style, ctx)?;
        let resolved_pos = resolve_background_positions(&[*position], style, ctx).pop()?;
        Some(ResolvedClipPath::Ellipse {
          radius_x: rx,
          radius_y: ry,
          position: resolved_pos,
          reference: *reference,
        })
      }
      // Polygons and unsupported shapes fall back to discrete animation.
      BasicShape::Polygon { .. } => None,
    },
  }
}

fn interpolate_clip_paths(
  a: &ResolvedClipPath,
  b: &ResolvedClipPath,
  t: f32,
) -> Option<ResolvedClipPath> {
  match (a, b) {
    (ResolvedClipPath::None, ResolvedClipPath::None) => Some(ResolvedClipPath::None),
    (ResolvedClipPath::Box(a), ResolvedClipPath::Box(b)) if a == b => {
      Some(ResolvedClipPath::Box(*a))
    }
    (
      ResolvedClipPath::Inset {
        top: ta,
        right: ra,
        bottom: ba,
        left: la,
        radii: raadii,
        reference: refa,
      },
      ResolvedClipPath::Inset {
        top: tb,
        right: rb,
        bottom: bb,
        left: lb,
        radii: rbra,
        reference: refb,
      },
    ) if refa == refb => {
      let radii = match (raadii, rbra) {
        (Some(a), Some(b)) => Some(interpolate_radii(*a, *b, t)),
        (None, None) => None,
        _ => return None,
      };
      Some(ResolvedClipPath::Inset {
        top: lerp(*ta, *tb, t),
        right: lerp(*ra, *rb, t),
        bottom: lerp(*ba, *bb, t),
        left: lerp(*la, *lb, t),
        radii,
        reference: *refa,
      })
    }
    (
      ResolvedClipPath::Circle {
        radius: ra,
        position: pa,
        reference: refa,
      },
      ResolvedClipPath::Circle {
        radius: rb,
        position: pb,
        reference: refb,
      },
    ) if refa == refb => {
      let pos = interpolate_background_positions(&[pa.clone()], &[pb.clone()], t)?;
      Some(ResolvedClipPath::Circle {
        radius: lerp(*ra, *rb, t),
        position: pos[0].clone(),
        reference: *refa,
      })
    }
    (
      ResolvedClipPath::Ellipse {
        radius_x: rxa,
        radius_y: rya,
        position: pa,
        reference: refa,
      },
      ResolvedClipPath::Ellipse {
        radius_x: rxb,
        radius_y: ryb,
        position: pb,
        reference: refb,
      },
    ) if refa == refb => {
      let pos = interpolate_background_positions(&[pa.clone()], &[pb.clone()], t)?;
      Some(ResolvedClipPath::Ellipse {
        radius_x: lerp(*rxa, *rxb, t),
        radius_y: lerp(*rya, *ryb, t),
        position: pos[0].clone(),
        reference: *refa,
      })
    }
    _ => None,
  }
}

fn resolved_clip_to_clip_path(resolved: &ResolvedClipPath) -> ClipPath {
  match resolved {
    ResolvedClipPath::None => ClipPath::None,
    ResolvedClipPath::Box(b) => ClipPath::Box(*b),
    ResolvedClipPath::Inset {
      top,
      right,
      bottom,
      left,
      radii,
      reference,
    } => {
      let border_radius: Box<Option<ClipRadii>> = Box::new(radii.map(|r| ClipRadii {
        top_left: Length::px(r[0]),
        top_right: Length::px(r[1]),
        bottom_left: Length::px(r[2]),
        bottom_right: Length::px(r[3]),
      }));
      ClipPath::BasicShape(
        Box::new(BasicShape::Inset {
          top: Length::px(*top),
          right: Length::px(*right),
          bottom: Length::px(*bottom),
          left: Length::px(*left),
          border_radius,
        }),
        *reference,
      )
    }
    ResolvedClipPath::Circle {
      radius,
      position,
      reference,
    } => ClipPath::BasicShape(
      Box::new(BasicShape::Circle {
        radius: ShapeRadius::Length(Length::px(*radius)),
        position: BackgroundPosition::Position {
          x: BackgroundPositionComponent {
            alignment: position.x.alignment,
            offset: Length::px(position.x.offset),
          },
          y: BackgroundPositionComponent {
            alignment: position.y.alignment,
            offset: Length::px(position.y.offset),
          },
        },
      }),
      *reference,
    ),
    ResolvedClipPath::Ellipse {
      radius_x,
      radius_y,
      position,
      reference,
    } => ClipPath::BasicShape(
      Box::new(BasicShape::Ellipse {
        radius_x: ShapeRadius::Length(Length::px(*radius_x)),
        radius_y: ShapeRadius::Length(Length::px(*radius_y)),
        position: BackgroundPosition::Position {
          x: BackgroundPositionComponent {
            alignment: position.x.alignment,
            offset: Length::px(position.x.offset),
          },
          y: BackgroundPositionComponent {
            alignment: position.y.alignment,
            offset: Length::px(position.y.offset),
          },
        },
      }),
      *reference,
    ),
  }
}

fn clip_path_to_resolved(path: &ClipPath) -> Option<ResolvedClipPath> {
  match path {
    ClipPath::None => Some(ResolvedClipPath::None),
    ClipPath::Box(b) => Some(ResolvedClipPath::Box(*b)),
    ClipPath::BasicShape(shape, reference) => match shape.as_ref() {
      BasicShape::Inset {
        top,
        right,
        bottom,
        left,
        border_radius,
      } => Some(ResolvedClipPath::Inset {
        top: top.to_px(),
        right: right.to_px(),
        bottom: bottom.to_px(),
        left: left.to_px(),
        radii: border_radius.as_ref().map(|r| {
          [
            r.top_left.to_px(),
            r.top_right.to_px(),
            r.bottom_left.to_px(),
            r.bottom_right.to_px(),
          ]
        }),
        reference: *reference,
      }),
      BasicShape::Circle { radius, position } => match (radius, position) {
        (ShapeRadius::Length(len), BackgroundPosition::Position { x, y }) => {
          Some(ResolvedClipPath::Circle {
            radius: len.to_px(),
            position: ResolvedBackgroundPosition {
              x: ResolvedPositionComponent {
                alignment: x.alignment,
                offset: x.offset.to_px(),
              },
              y: ResolvedPositionComponent {
                alignment: y.alignment,
                offset: y.offset.to_px(),
              },
            },
            reference: *reference,
          })
        }
        _ => None,
      },
      BasicShape::Ellipse {
        radius_x,
        radius_y,
        position,
      } => match (radius_x, radius_y, position) {
        (
          ShapeRadius::Length(rx),
          ShapeRadius::Length(ry),
          BackgroundPosition::Position { x, y },
        ) => Some(ResolvedClipPath::Ellipse {
          radius_x: rx.to_px(),
          radius_y: ry.to_px(),
          position: ResolvedBackgroundPosition {
            x: ResolvedPositionComponent {
              alignment: x.alignment,
              offset: x.offset.to_px(),
            },
            y: ResolvedPositionComponent {
              alignment: y.alignment,
              offset: y.offset.to_px(),
            },
          },
          reference: *reference,
        }),
        _ => None,
      },
      BasicShape::Polygon { .. } => None,
    },
  }
}

struct PropertyInterpolator {
  name: &'static str,
  extract: fn(&ComputedStyle, &AnimationResolveContext) -> Option<AnimatedValue>,
  interpolate: fn(&AnimatedValue, &AnimatedValue, f32) -> Option<AnimatedValue>,
  apply: fn(&mut ComputedStyle, &AnimatedValue),
}

fn extract_opacity(style: &ComputedStyle, _ctx: &AnimationResolveContext) -> Option<AnimatedValue> {
  Some(AnimatedValue::Opacity(style.opacity))
}

fn interpolate_opacity(a: &AnimatedValue, b: &AnimatedValue, t: f32) -> Option<AnimatedValue> {
  match (a, b) {
    (AnimatedValue::Opacity(x), AnimatedValue::Opacity(y)) => {
      Some(AnimatedValue::Opacity(lerp(*x, *y, t)))
    }
    _ => None,
  }
}

fn apply_opacity(style: &mut ComputedStyle, value: &AnimatedValue) {
  if let AnimatedValue::Opacity(v) = value {
    style.opacity = clamp_progress(*v);
  }
}

fn extract_color(style: &ComputedStyle, _ctx: &AnimationResolveContext) -> Option<AnimatedValue> {
  Some(AnimatedValue::Color(style.color))
}

fn extract_background_color(
  style: &ComputedStyle,
  _ctx: &AnimationResolveContext,
) -> Option<AnimatedValue> {
  Some(AnimatedValue::Color(style.background_color))
}

fn interpolate_color(a: &AnimatedValue, b: &AnimatedValue, t: f32) -> Option<AnimatedValue> {
  match (a, b) {
    (AnimatedValue::Color(ca), AnimatedValue::Color(cb)) => {
      Some(AnimatedValue::Color(lerp_color(*ca, *cb, t)))
    }
    _ => None,
  }
}

fn apply_color(style: &mut ComputedStyle, value: &AnimatedValue) {
  if let AnimatedValue::Color(c) = value {
    style.color = *c;
  }
}

fn apply_background_color(style: &mut ComputedStyle, value: &AnimatedValue) {
  if let AnimatedValue::Color(c) = value {
    style.background_color = *c;
  }
}

fn extract_transform(
  style: &ComputedStyle,
  ctx: &AnimationResolveContext,
) -> Option<AnimatedValue> {
  Some(AnimatedValue::Transform(resolve_transform_list(
    &style.transform,
    style,
    ctx,
  )))
}

fn interpolate_transform_value(
  a: &AnimatedValue,
  b: &AnimatedValue,
  t: f32,
) -> Option<AnimatedValue> {
  match (a, b) {
    (AnimatedValue::Transform(ta), AnimatedValue::Transform(tb)) => {
      let interpolated = interpolate_transform_lists(ta, tb, t).unwrap_or_else(|| {
        let ma = compose_transform_list(ta);
        let mb = compose_transform_list(tb);
        vec![crate::css::types::Transform::Matrix3d(
          lerp_matrix(&ma, &mb, t).m,
        )]
      });
      Some(AnimatedValue::Transform(interpolated))
    }
    _ => None,
  }
}

fn apply_transform(style: &mut ComputedStyle, value: &AnimatedValue) {
  if let AnimatedValue::Transform(list) = value {
    style.transform = list.clone();
  }
}

fn extract_filter(style: &ComputedStyle, ctx: &AnimationResolveContext) -> Option<AnimatedValue> {
  let resolved = resolve_filter_list(&style.filter, style, ctx);
  Some(AnimatedValue::Filter(resolved_filters_to_functions(
    &resolved,
  )))
}

fn extract_backdrop_filter(
  style: &ComputedStyle,
  ctx: &AnimationResolveContext,
) -> Option<AnimatedValue> {
  let resolved = resolve_filter_list(&style.backdrop_filter, style, ctx);
  Some(AnimatedValue::BackdropFilter(
    resolved_filters_to_functions(&resolved),
  ))
}

fn interpolate_filters_value(
  a: &AnimatedValue,
  b: &AnimatedValue,
  t: f32,
) -> Option<AnimatedValue> {
  match (a, b) {
    (AnimatedValue::Filter(fa), AnimatedValue::Filter(fb)) => {
      let ra = resolved_filters_from_functions(fa);
      let rb = resolved_filters_from_functions(fb);
      let interpolated = interpolate_filters(&ra, &rb, t)?;
      Some(AnimatedValue::Filter(resolved_filters_to_functions(
        &interpolated,
      )))
    }
    (AnimatedValue::BackdropFilter(fa), AnimatedValue::BackdropFilter(fb)) => {
      let ra = resolved_filters_from_functions(fa);
      let rb = resolved_filters_from_functions(fb);
      let interpolated = interpolate_filters(&ra, &rb, t)?;
      Some(AnimatedValue::BackdropFilter(
        resolved_filters_to_functions(&interpolated),
      ))
    }
    _ => None,
  }
}

fn apply_filter(style: &mut ComputedStyle, value: &AnimatedValue) {
  match value {
    AnimatedValue::Filter(filters) => style.filter = filters.clone(),
    AnimatedValue::BackdropFilter(filters) => style.backdrop_filter = filters.clone(),
    _ => {}
  }
}

fn extract_clip_path(
  style: &ComputedStyle,
  ctx: &AnimationResolveContext,
) -> Option<AnimatedValue> {
  resolve_clip_path(&style.clip_path, style, ctx)
    .map(|resolved| AnimatedValue::ClipPath(resolved_clip_to_clip_path(&resolved)))
}

fn interpolate_clip_path_value(
  a: &AnimatedValue,
  b: &AnimatedValue,
  t: f32,
) -> Option<AnimatedValue> {
  let (AnimatedValue::ClipPath(pa), AnimatedValue::ClipPath(pb)) = (a, b) else {
    return None;
  };
  let ra = clip_path_to_resolved(pa)?;
  let rb = clip_path_to_resolved(pb)?;
  let interpolated = interpolate_clip_paths(&ra, &rb, t)?;
  Some(AnimatedValue::ClipPath(resolved_clip_to_clip_path(
    &interpolated,
  )))
}

fn apply_clip_path(style: &mut ComputedStyle, value: &AnimatedValue) {
  if let AnimatedValue::ClipPath(path) = value {
    style.clip_path = path.clone();
  }
}

fn extract_background_position(
  style: &ComputedStyle,
  ctx: &AnimationResolveContext,
) -> Option<AnimatedValue> {
  let resolved = resolve_background_positions(&style.background_positions, style, ctx);
  Some(AnimatedValue::BackgroundPosition(
    resolved_positions_to_background(&resolved),
  ))
}

fn interpolate_background_position_value(
  a: &AnimatedValue,
  b: &AnimatedValue,
  t: f32,
) -> Option<AnimatedValue> {
  let (AnimatedValue::BackgroundPosition(pa), AnimatedValue::BackgroundPosition(pb)) = (a, b)
  else {
    return None;
  };
  let ra = background_positions_to_resolved(pa);
  let rb = background_positions_to_resolved(pb);
  let interpolated = interpolate_background_positions(&ra, &rb, t)?;
  Some(AnimatedValue::BackgroundPosition(
    resolved_positions_to_background(&interpolated),
  ))
}

fn apply_background_position(style: &mut ComputedStyle, value: &AnimatedValue) {
  if let AnimatedValue::BackgroundPosition(pos) = value {
    style.background_positions = pos.clone();
    style.rebuild_background_layers();
  }
}

fn extract_background_size(
  style: &ComputedStyle,
  ctx: &AnimationResolveContext,
) -> Option<AnimatedValue> {
  let resolved = resolve_background_sizes(&style.background_sizes, style, ctx);
  Some(AnimatedValue::BackgroundSize(resolved_sizes_to_background(
    &resolved,
  )))
}

fn interpolate_background_size_value(
  a: &AnimatedValue,
  b: &AnimatedValue,
  t: f32,
) -> Option<AnimatedValue> {
  let (AnimatedValue::BackgroundSize(sa), AnimatedValue::BackgroundSize(sb)) = (a, b) else {
    return None;
  };
  let ra = background_sizes_to_resolved(sa);
  let rb = background_sizes_to_resolved(sb);
  let interpolated = interpolate_background_sizes(&ra, &rb, t)?;
  Some(AnimatedValue::BackgroundSize(resolved_sizes_to_background(
    &interpolated,
  )))
}

fn apply_background_size(style: &mut ComputedStyle, value: &AnimatedValue) {
  if let AnimatedValue::BackgroundSize(sizes) = value {
    style.background_sizes = sizes.clone();
    style.rebuild_background_layers();
  }
}

fn extract_border_radius(
  style: &ComputedStyle,
  ctx: &AnimationResolveContext,
) -> Option<AnimatedValue> {
  let radii = resolve_border_radii(style, ctx);
  Some(AnimatedValue::BorderRadius([
    Length::px(radii[0]),
    Length::px(radii[1]),
    Length::px(radii[2]),
    Length::px(radii[3]),
  ]))
}

fn extract_border_corner(
  style: &ComputedStyle,
  ctx: &AnimationResolveContext,
  _idx: usize,
) -> Option<AnimatedValue> {
  extract_border_radius(style, ctx)
}

fn interpolate_border_radius_value(
  a: &AnimatedValue,
  b: &AnimatedValue,
  t: f32,
) -> Option<AnimatedValue> {
  let (AnimatedValue::BorderRadius(ra), AnimatedValue::BorderRadius(rb)) = (a, b) else {
    return None;
  };
  let to_px = |len: &Length| len.to_px();
  let array_a = [to_px(&ra[0]), to_px(&ra[1]), to_px(&ra[2]), to_px(&ra[3])];
  let array_b = [to_px(&rb[0]), to_px(&rb[1]), to_px(&rb[2]), to_px(&rb[3])];
  let interpolated = interpolate_radii(array_a, array_b, t);
  Some(AnimatedValue::BorderRadius([
    Length::px(interpolated[0]),
    Length::px(interpolated[1]),
    Length::px(interpolated[2]),
    Length::px(interpolated[3]),
  ]))
}

fn apply_border_radius(style: &mut ComputedStyle, value: &AnimatedValue) {
  if let AnimatedValue::BorderRadius(r) = value {
    style.border_top_left_radius = r[0];
    style.border_top_right_radius = r[1];
    style.border_bottom_left_radius = r[2];
    style.border_bottom_right_radius = r[3];
  }
}

fn property_interpolators() -> &'static [PropertyInterpolator] {
  static PROPS: &[PropertyInterpolator] = &[
    PropertyInterpolator {
      name: "opacity",
      extract: extract_opacity,
      interpolate: interpolate_opacity,
      apply: apply_opacity,
    },
    PropertyInterpolator {
      name: "color",
      extract: extract_color,
      interpolate: interpolate_color,
      apply: apply_color,
    },
    PropertyInterpolator {
      name: "background-color",
      extract: extract_background_color,
      interpolate: interpolate_color,
      apply: apply_background_color,
    },
    PropertyInterpolator {
      name: "transform",
      extract: extract_transform,
      interpolate: interpolate_transform_value,
      apply: apply_transform,
    },
    PropertyInterpolator {
      name: "filter",
      extract: extract_filter,
      interpolate: interpolate_filters_value,
      apply: apply_filter,
    },
    PropertyInterpolator {
      name: "backdrop-filter",
      extract: extract_backdrop_filter,
      interpolate: interpolate_filters_value,
      apply: apply_filter,
    },
    PropertyInterpolator {
      name: "clip-path",
      extract: extract_clip_path,
      interpolate: interpolate_clip_path_value,
      apply: apply_clip_path,
    },
    PropertyInterpolator {
      name: "background-position",
      extract: extract_background_position,
      interpolate: interpolate_background_position_value,
      apply: apply_background_position,
    },
    PropertyInterpolator {
      name: "background-size",
      extract: extract_background_size,
      interpolate: interpolate_background_size_value,
      apply: apply_background_size,
    },
    PropertyInterpolator {
      name: "border-radius",
      extract: extract_border_radius,
      interpolate: interpolate_border_radius_value,
      apply: apply_border_radius,
    },
    PropertyInterpolator {
      name: "border-top-left-radius",
      extract: |s, c| extract_border_corner(s, c, 0),
      interpolate: interpolate_border_radius_value,
      apply: apply_border_radius,
    },
    PropertyInterpolator {
      name: "border-top-right-radius",
      extract: |s, c| extract_border_corner(s, c, 1),
      interpolate: interpolate_border_radius_value,
      apply: apply_border_radius,
    },
    PropertyInterpolator {
      name: "border-bottom-left-radius",
      extract: |s, c| extract_border_corner(s, c, 2),
      interpolate: interpolate_border_radius_value,
      apply: apply_border_radius,
    },
    PropertyInterpolator {
      name: "border-bottom-right-radius",
      extract: |s, c| extract_border_corner(s, c, 3),
      interpolate: interpolate_border_radius_value,
      apply: apply_border_radius,
    },
  ];
  PROPS
}

fn interpolator_for(name: &str) -> Option<&'static PropertyInterpolator> {
  property_interpolators().iter().find(|p| p.name == name)
}

fn resolve_transform_length(
  len: &Length,
  style: &ComputedStyle,
  ctx: &AnimationResolveContext,
  base: f32,
) -> f32 {
  let reference = if base.is_finite() && base.abs() > f32::EPSILON {
    Some(base)
  } else {
    None
  };
  resolve_length_px(len, reference, style, ctx)
}

fn resolve_transform_list(
  list: &[crate::css::types::Transform],
  style: &ComputedStyle,
  ctx: &AnimationResolveContext,
) -> Vec<crate::css::types::Transform> {
  let width = ctx.element_size.width;
  let height = ctx.element_size.height;
  list
    .iter()
    .map(|t| match t {
      crate::css::types::Transform::Translate(x, y) => crate::css::types::Transform::Translate(
        Length::px(resolve_transform_length(x, style, ctx, width)),
        Length::px(resolve_transform_length(y, style, ctx, height)),
      ),
      crate::css::types::Transform::TranslateX(x) => crate::css::types::Transform::TranslateX(
        Length::px(resolve_transform_length(x, style, ctx, width)),
      ),
      crate::css::types::Transform::TranslateY(y) => crate::css::types::Transform::TranslateY(
        Length::px(resolve_transform_length(y, style, ctx, height)),
      ),
      crate::css::types::Transform::TranslateZ(z) => crate::css::types::Transform::TranslateZ(
        Length::px(resolve_transform_length(z, style, ctx, width)),
      ),
      crate::css::types::Transform::Translate3d(x, y, z) => {
        crate::css::types::Transform::Translate3d(
          Length::px(resolve_transform_length(x, style, ctx, width)),
          Length::px(resolve_transform_length(y, style, ctx, height)),
          Length::px(resolve_transform_length(z, style, ctx, width)),
        )
      }
      crate::css::types::Transform::Scale(sx, sy) => crate::css::types::Transform::Scale(*sx, *sy),
      crate::css::types::Transform::ScaleX(sx) => crate::css::types::Transform::ScaleX(*sx),
      crate::css::types::Transform::ScaleY(sy) => crate::css::types::Transform::ScaleY(*sy),
      crate::css::types::Transform::ScaleZ(sz) => crate::css::types::Transform::ScaleZ(*sz),
      crate::css::types::Transform::Scale3d(sx, sy, sz) => {
        crate::css::types::Transform::Scale3d(*sx, *sy, *sz)
      }
      crate::css::types::Transform::Rotate(deg) | crate::css::types::Transform::RotateZ(deg) => {
        crate::css::types::Transform::Rotate(*deg)
      }
      crate::css::types::Transform::RotateX(deg) => crate::css::types::Transform::RotateX(*deg),
      crate::css::types::Transform::RotateY(deg) => crate::css::types::Transform::RotateY(*deg),
      crate::css::types::Transform::Rotate3d(x, y, z, deg) => {
        crate::css::types::Transform::Rotate3d(*x, *y, *z, *deg)
      }
      crate::css::types::Transform::SkewX(deg) => crate::css::types::Transform::SkewX(*deg),
      crate::css::types::Transform::SkewY(deg) => crate::css::types::Transform::SkewY(*deg),
      crate::css::types::Transform::Skew(ax, ay) => crate::css::types::Transform::Skew(*ax, *ay),
      crate::css::types::Transform::Perspective(len) => crate::css::types::Transform::Perspective(
        Length::px(resolve_transform_length(len, style, ctx, width)),
      ),
      crate::css::types::Transform::Matrix(a, b, c, d, e, f) => {
        crate::css::types::Transform::Matrix(*a, *b, *c, *d, *e, *f)
      }
      crate::css::types::Transform::Matrix3d(values) => {
        crate::css::types::Transform::Matrix3d(*values)
      }
    })
    .collect()
}

fn compose_transform_list(list: &[crate::css::types::Transform]) -> Transform3D {
  let mut ts = Transform3D::identity();
  const EPS: f32 = 1e-6;
  for component in list {
    let next = match component {
      crate::css::types::Transform::Translate(x, y) => {
        Transform3D::translate(x.to_px(), y.to_px(), 0.0)
      }
      crate::css::types::Transform::TranslateX(x) => Transform3D::translate(x.to_px(), 0.0, 0.0),
      crate::css::types::Transform::TranslateY(y) => Transform3D::translate(0.0, y.to_px(), 0.0),
      crate::css::types::Transform::TranslateZ(z) => Transform3D::translate(0.0, 0.0, z.to_px()),
      crate::css::types::Transform::Translate3d(x, y, z) => {
        Transform3D::translate(x.to_px(), y.to_px(), z.to_px())
      }
      crate::css::types::Transform::Scale(sx, sy) => Transform3D::scale(*sx, *sy, 1.0),
      crate::css::types::Transform::ScaleX(sx) => Transform3D::scale(*sx, 1.0, 1.0),
      crate::css::types::Transform::ScaleY(sy) => Transform3D::scale(1.0, *sy, 1.0),
      crate::css::types::Transform::ScaleZ(sz) => Transform3D::scale(1.0, 1.0, *sz),
      crate::css::types::Transform::Scale3d(sx, sy, sz) => Transform3D::scale(*sx, *sy, *sz),
      crate::css::types::Transform::Rotate(deg) | crate::css::types::Transform::RotateZ(deg) => {
        Transform3D::rotate_z(deg.to_radians())
      }
      crate::css::types::Transform::RotateX(deg) => Transform3D::rotate_x(deg.to_radians()),
      crate::css::types::Transform::RotateY(deg) => Transform3D::rotate_y(deg.to_radians()),
      crate::css::types::Transform::Rotate3d(x, y, z, deg) => {
        let len = (x * x + y * y + z * z).sqrt();
        if len < EPS {
          Transform3D::identity()
        } else {
          let ax = *x / len;
          let ay = *y / len;
          let az = *z / len;
          let angle = deg.to_radians();
          let (s, c) = angle.sin_cos();
          let t = 1.0 - c;

          let m00 = t * ax * ax + c;
          let m01 = t * ax * ay + s * az;
          let m02 = t * ax * az - s * ay;
          let m10 = t * ax * ay - s * az;
          let m11 = t * ay * ay + c;
          let m12 = t * ay * az + s * ax;
          let m20 = t * ax * az + s * ay;
          let m21 = t * ay * az - s * ax;
          let m22 = t * az * az + c;

          Transform3D {
            m: [
              m00, m10, m20, 0.0, // column 1
              m01, m11, m21, 0.0, // column 2
              m02, m12, m22, 0.0, // column 3
              0.0, 0.0, 0.0, 1.0, // column 4
            ],
          }
        }
      }
      crate::css::types::Transform::SkewX(deg) => Transform3D::skew(deg.to_radians(), 0.0),
      crate::css::types::Transform::SkewY(deg) => Transform3D::skew(0.0, deg.to_radians()),
      crate::css::types::Transform::Skew(ax, ay) => {
        Transform3D::skew(ax.to_radians(), ay.to_radians())
      }
      crate::css::types::Transform::Perspective(len) => Transform3D::perspective(len.to_px()),
      crate::css::types::Transform::Matrix(a, b, c, d, e, f) => {
        Transform3D::from_2d(&Transform2D {
          a: *a,
          b: *b,
          c: *c,
          d: *d,
          e: *e,
          f: *f,
        })
      }
      crate::css::types::Transform::Matrix3d(values) => Transform3D { m: *values },
    };
    ts = ts.multiply(&next);
  }
  ts
}

fn lerp_matrix(a: &Transform3D, b: &Transform3D, t: f32) -> Transform3D {
  let mut m = [0.0; 16];
  for i in 0..16 {
    m[i] = lerp(a.m[i], b.m[i], t);
  }
  Transform3D { m }
}

fn interpolate_transform_lists(
  a: &[crate::css::types::Transform],
  b: &[crate::css::types::Transform],
  t: f32,
) -> Option<Vec<crate::css::types::Transform>> {
  if a.len() != b.len() {
    return None;
  }
  let mut out = Vec::with_capacity(a.len());
  for (ta, tb) in a.iter().zip(b.iter()) {
    if discriminant(ta) != discriminant(tb) {
      return None;
    }

    let next = match (ta, tb) {
      (
        crate::css::types::Transform::Translate(ax, ay),
        crate::css::types::Transform::Translate(bx, by),
      ) => {
        let x = lerp(ax.to_px(), bx.to_px(), t);
        let y = lerp(ay.to_px(), by.to_px(), t);
        crate::css::types::Transform::Translate(Length::px(x), Length::px(y))
      }
      (
        crate::css::types::Transform::TranslateX(ax),
        crate::css::types::Transform::TranslateX(bx),
      ) => crate::css::types::Transform::TranslateX(Length::px(lerp(ax.to_px(), bx.to_px(), t))),
      (
        crate::css::types::Transform::TranslateY(ay),
        crate::css::types::Transform::TranslateY(by),
      ) => crate::css::types::Transform::TranslateY(Length::px(lerp(ay.to_px(), by.to_px(), t))),
      (
        crate::css::types::Transform::TranslateZ(az),
        crate::css::types::Transform::TranslateZ(bz),
      ) => crate::css::types::Transform::TranslateZ(Length::px(lerp(az.to_px(), bz.to_px(), t))),
      (
        crate::css::types::Transform::Translate3d(ax, ay, az),
        crate::css::types::Transform::Translate3d(bx, by, bz),
      ) => crate::css::types::Transform::Translate3d(
        Length::px(lerp(ax.to_px(), bx.to_px(), t)),
        Length::px(lerp(ay.to_px(), by.to_px(), t)),
        Length::px(lerp(az.to_px(), bz.to_px(), t)),
      ),
      (
        crate::css::types::Transform::Scale(ax, ay),
        crate::css::types::Transform::Scale(bx, by),
      ) => crate::css::types::Transform::Scale(lerp(*ax, *bx, t), lerp(*ay, *by, t)),
      (crate::css::types::Transform::ScaleX(ax), crate::css::types::Transform::ScaleX(bx)) => {
        crate::css::types::Transform::ScaleX(lerp(*ax, *bx, t))
      }
      (crate::css::types::Transform::ScaleY(ay), crate::css::types::Transform::ScaleY(by)) => {
        crate::css::types::Transform::ScaleY(lerp(*ay, *by, t))
      }
      (crate::css::types::Transform::ScaleZ(az), crate::css::types::Transform::ScaleZ(bz)) => {
        crate::css::types::Transform::ScaleZ(lerp(*az, *bz, t))
      }
      (
        crate::css::types::Transform::Scale3d(ax, ay, az),
        crate::css::types::Transform::Scale3d(bx, by, bz),
      ) => crate::css::types::Transform::Scale3d(
        lerp(*ax, *bx, t),
        lerp(*ay, *by, t),
        lerp(*az, *bz, t),
      ),
      (crate::css::types::Transform::Rotate(ax), crate::css::types::Transform::Rotate(bx))
      | (crate::css::types::Transform::RotateZ(ax), crate::css::types::Transform::RotateZ(bx)) => {
        crate::css::types::Transform::Rotate(lerp(*ax, *bx, t))
      }
      (crate::css::types::Transform::RotateX(ax), crate::css::types::Transform::RotateX(bx)) => {
        crate::css::types::Transform::RotateX(lerp(*ax, *bx, t))
      }
      (crate::css::types::Transform::RotateY(ay), crate::css::types::Transform::RotateY(by)) => {
        crate::css::types::Transform::RotateY(lerp(*ay, *by, t))
      }
      (
        crate::css::types::Transform::Rotate3d(ax, ay, az, aa),
        crate::css::types::Transform::Rotate3d(bx, by, bz, ba),
      ) => crate::css::types::Transform::Rotate3d(
        lerp(*ax, *bx, t),
        lerp(*ay, *by, t),
        lerp(*az, *bz, t),
        lerp(*aa, *ba, t),
      ),
      (crate::css::types::Transform::SkewX(ax), crate::css::types::Transform::SkewX(bx)) => {
        crate::css::types::Transform::SkewX(lerp(*ax, *bx, t))
      }
      (crate::css::types::Transform::SkewY(ay), crate::css::types::Transform::SkewY(by)) => {
        crate::css::types::Transform::SkewY(lerp(*ay, *by, t))
      }
      (crate::css::types::Transform::Skew(ax, ay), crate::css::types::Transform::Skew(bx, by)) => {
        crate::css::types::Transform::Skew(lerp(*ax, *bx, t), lerp(*ay, *by, t))
      }
      (
        crate::css::types::Transform::Perspective(pa),
        crate::css::types::Transform::Perspective(pb),
      ) => crate::css::types::Transform::Perspective(Length::px(lerp(pa.to_px(), pb.to_px(), t))),
      _ => return None,
    };

    out.push(next);
  }

  Some(out)
}

fn axis_is_horizontal(axis: TimelineAxis, writing_mode: WritingMode) -> bool {
  match axis {
    TimelineAxis::X => true,
    TimelineAxis::Y => false,
    TimelineAxis::Inline => inline_axis_is_horizontal(writing_mode),
    TimelineAxis::Block => crate::style::block_axis_is_horizontal(writing_mode),
  }
}

fn resolve_offset_value(
  offset: &TimelineOffset,
  scroll_range: f32,
  viewport_size: f32,
  is_end: bool,
) -> f32 {
  match offset {
    TimelineOffset::Auto => {
      if is_end {
        scroll_range.max(0.0)
      } else {
        0.0
      }
    }
    TimelineOffset::Length(len) => len
      .resolve_against(scroll_range)
      .unwrap_or_else(|| len.to_px().max(0.0)),
    TimelineOffset::Percentage(pct) => (pct / 100.0) * scroll_range,
  }
  .clamp(0.0, scroll_range.max(0.0).max(viewport_size))
}

fn resolve_progress_offset(
  offset: &RangeOffset,
  base_start: f32,
  base_end: f32,
  view_size: f32,
  phases: Option<(f32, f32, f32)>,
) -> f32 {
  match offset {
    RangeOffset::Progress(p) => base_start + (base_end - base_start) * p.clamp(0.0, 1.0),
    RangeOffset::View(phase, adj) => {
      let Some((entry, cross, exit)) = phases else {
        return base_start;
      };
      let base = match phase {
        ViewTimelinePhase::Entry => entry,
        ViewTimelinePhase::Cross => cross,
        ViewTimelinePhase::Exit => exit,
      };
      base + view_size * adj
    }
  }
}

fn clamp_progress(value: f32) -> f32 {
  value.clamp(0.0, 1.0)
}

/// Computes scroll timeline progress given the current scroll offset and
/// resolved geometry.
pub fn scroll_timeline_progress(
  timeline: &ScrollTimeline,
  scroll_position: f32,
  scroll_range: f32,
  viewport_size: f32,
  range: &AnimationRange,
) -> f32 {
  let start_base = resolve_offset_value(&timeline.start, scroll_range, viewport_size, false);
  let end_base = resolve_offset_value(&timeline.end, scroll_range, viewport_size, true);
  let start = resolve_progress_offset(range.start(), start_base, end_base, viewport_size, None);
  let end = resolve_progress_offset(range.end(), start_base, end_base, viewport_size, None);
  if (end - start).abs() < f32::EPSILON {
    return 0.0;
  }
  clamp_progress((scroll_position - start) / (end - start))
}

/// Computes view timeline progress using the target position relative to the
/// containing scroll port.
pub fn view_timeline_progress(
  _timeline: &ViewTimeline,
  target_start: f32,
  target_end: f32,
  view_size: f32,
  scroll_offset: f32,
  range: &AnimationRange,
) -> f32 {
  let entry = target_start - view_size;
  let exit = target_end;
  let cross = (target_start + target_end) * 0.5 - view_size * 0.5;
  let start_base = entry;
  let end_base = exit;
  let phases = Some((entry, cross, exit));
  let start = resolve_progress_offset(range.start(), start_base, end_base, view_size, phases);
  let end = resolve_progress_offset(range.end(), start_base, end_base, view_size, phases);
  if (end - start).abs() < f32::EPSILON {
    return clamp_progress(if scroll_offset >= end { 1.0 } else { 0.0 });
  }
  clamp_progress((scroll_offset - start) / (end - start))
}

/// Determines the scroll position and range along the requested axis given
/// container and content sizes. The returned tuple is `(position, range, size)`.
pub fn axis_scroll_state(
  axis: TimelineAxis,
  writing_mode: WritingMode,
  scroll_x: f32,
  scroll_y: f32,
  view_width: f32,
  view_height: f32,
  content_width: f32,
  content_height: f32,
) -> (f32, f32, f32) {
  let horizontal = axis_is_horizontal(axis, writing_mode);
  if horizontal {
    let unclamped = scroll_x.max(0.0);
    let range = (content_width - view_width).max(unclamped);
    (unclamped.min(range), range, view_width)
  } else {
    let unclamped = scroll_y.max(0.0);
    let range = (content_height - view_height).max(unclamped);
    (unclamped.min(range), range, view_height)
  }
}

/// Samples a @keyframes rule at the given progress, returning a property map of
/// interpolated computed values.
pub fn sample_keyframes(
  rule: &KeyframesRule,
  progress: f32,
  base_style: &ComputedStyle,
  viewport: Size,
  element_size: Size,
) -> HashMap<String, AnimatedValue> {
  let mut frames = rule.keyframes.clone();
  if frames.is_empty() {
    return HashMap::new();
  }
  frames.sort_by(|a, b| {
    a.offset
      .partial_cmp(&b.offset)
      .unwrap_or(std::cmp::Ordering::Equal)
  });
  let progress = clamp_progress(progress);

  let mut prev: &Keyframe = &frames[0];
  let mut next: &Keyframe = &frames[frames.len() - 1];
  for frame in &frames {
    if frame.offset <= progress + f32::EPSILON {
      prev = frame;
    }
    if frame.offset + f32::EPSILON >= progress {
      next = frame;
      break;
    }
  }

  let start = prev.offset;
  let end = next.offset;
  let local_t = if end - start > f32::EPSILON {
    (progress - start) / (end - start)
  } else {
    0.0
  };

  let defaults = ComputedStyle::default();
  let mut from_style = base_style.clone();
  let mut to_style = base_style.clone();
  for decl in &prev.declarations {
    apply_declaration_with_base(
      &mut from_style,
      decl,
      base_style,
      &defaults,
      None,
      base_style.font_size,
      base_style.root_font_size,
      viewport,
    );
  }
  for decl in &next.declarations {
    apply_declaration_with_base(
      &mut to_style,
      decl,
      base_style,
      &defaults,
      None,
      base_style.font_size,
      base_style.root_font_size,
      viewport,
    );
  }

  let ctx = AnimationResolveContext::new(viewport, element_size);
  let mut properties: HashSet<String> = HashSet::new();
  for decl in &prev.declarations {
    properties.insert(decl.property.to_ascii_lowercase());
  }
  for decl in &next.declarations {
    properties.insert(decl.property.to_ascii_lowercase());
  }

  let mut result = HashMap::new();
  for prop in properties {
    let Some(interpolator) = interpolator_for(&prop) else {
      continue;
    };
    let Some(from_val) = (interpolator.extract)(&from_style, &ctx) else {
      continue;
    };
    let Some(to_val) = (interpolator.extract)(&to_style, &ctx) else {
      continue;
    };
    let value = (interpolator.interpolate)(&from_val, &to_val, local_t).or_else(|| {
      if (local_t - 1.0).abs() < f32::EPSILON {
        Some(to_val.clone())
      } else {
        Some(from_val.clone())
      }
    });
    if let Some(v) = value {
      result.insert(prop.clone(), v);
    }
  }

  result
}

/// Applies animated property values to the computed style.
pub fn apply_animated_properties(
  style: &mut ComputedStyle,
  values: &HashMap<String, AnimatedValue>,
) {
  for (name, value) in values {
    if let Some(interpolator) = interpolator_for(name) {
      (interpolator.apply)(style, value);
    }
  }
}

enum TimelineState {
  Scroll {
    timeline: ScrollTimeline,
    scroll_pos: f32,
    scroll_range: f32,
    viewport_size: f32,
  },
  View {
    timeline: ViewTimeline,
    target_start: f32,
    target_end: f32,
    view_size: f32,
    scroll_offset: f32,
  },
}

fn pick<'a, T: Clone>(list: &'a [T], idx: usize, default: T) -> T {
  if list.is_empty() {
    return default;
  }
  list
    .get(idx)
    .cloned()
    .unwrap_or_else(|| list.last().cloned().unwrap_or(default))
}

fn collect_timelines(
  node: &FragmentNode,
  origin: Point,
  viewport: Rect,
  content: Rect,
  scroll: Point,
  map: &mut HashMap<String, TimelineState>,
) {
  let abs = Rect::from_xywh(
    origin.x,
    origin.y,
    node.bounds.width(),
    node.bounds.height(),
  );

  if let Some(style) = node.style.as_ref() {
    for tl in &style.scroll_timelines {
      if let Some(name) = &tl.name {
        let (scroll_pos, scroll_range, viewport_size) = axis_scroll_state(
          tl.axis,
          style.writing_mode,
          scroll.x,
          scroll.y,
          viewport.width(),
          viewport.height(),
          content.width(),
          content.height(),
        );
        map.entry(name.clone()).or_insert(TimelineState::Scroll {
          timeline: tl.clone(),
          scroll_pos,
          scroll_range,
          viewport_size,
        });
      }
    }

    for tl in &style.view_timelines {
      if let Some(name) = &tl.name {
        let horizontal = axis_is_horizontal(tl.axis, style.writing_mode);
        let target_start = if horizontal { abs.x() } else { abs.y() };
        let target_end = if horizontal {
          abs.x() + abs.width()
        } else {
          abs.y() + abs.height()
        };
        let view_size = if horizontal {
          viewport.width()
        } else {
          viewport.height()
        };
        let scroll_offset = if horizontal { scroll.x } else { scroll.y };
        map.entry(name.clone()).or_insert(TimelineState::View {
          timeline: tl.clone(),
          target_start,
          target_end,
          view_size,
          scroll_offset,
        });
      }
    }
  }

  let child_offset = Point::new(origin.x + child.bounds.x(), origin.y + child.bounds.y());
  for child in &node.children {
    collect_timelines(child, child_offset, viewport, content, scroll, map);
  }
}

fn apply_animations_to_node(
  node: &mut FragmentNode,
  origin: Point,
  viewport: Rect,
  scroll: Point,
  keyframes: &HashMap<String, KeyframesRule>,
  timelines: &HashMap<String, TimelineState>,
) {
  let abs = Rect::from_xywh(
    origin.x,
    origin.y,
    node.bounds.width(),
    node.bounds.height(),
  );

  if let Some(style_arc) = node.style.clone() {
    let names = &style_arc.animation_names;
    if !names.is_empty() {
      let timelines_list = &style_arc.animation_timelines;
      let ranges_list = &style_arc.animation_ranges;

      let mut animated = (*style_arc).clone();
      let mut changed = false;

      for (idx, name) in names.iter().enumerate() {
        let timeline_ref = pick(timelines_list, idx, AnimationTimeline::Auto);
        let range = pick(ranges_list, idx, AnimationRange::default());
        let progress = match timeline_ref {
          AnimationTimeline::Auto => None,
          AnimationTimeline::None => None,
          AnimationTimeline::Named(ref timeline_name) => {
            timelines.get(timeline_name).map(|state| match state {
              TimelineState::Scroll {
                timeline,
                scroll_pos,
                scroll_range,
                viewport_size,
              } => scroll_timeline_progress(
                timeline,
                *scroll_pos,
                *scroll_range,
                *viewport_size,
                &range,
              ),
              TimelineState::View {
                timeline,
                target_start,
                target_end,
                view_size,
                scroll_offset,
              } => view_timeline_progress(
                timeline,
                *target_start,
                *target_end,
                *view_size,
                *scroll_offset,
                &range,
              ),
            })
          }
        };

        let Some(progress) = progress else { continue }; // skip if no timeline
        if let Some(rule) = keyframes.get(name) {
          let element_size = Size::new(node.bounds.width(), node.bounds.height());
          let viewport_size = Size::new(viewport.width(), viewport.height());
          let values = sample_keyframes(rule, progress, &*style_arc, viewport_size, element_size);
          if !values.is_empty() {
            apply_animated_properties(&mut animated, &values);
            changed = true;
          }
        }
      }

      if changed {
        node.style = Some(Arc::new(animated));
      }
    }
  }

  let child_offset = Point::new(origin.x + child.bounds.x(), origin.y + child.bounds.y());
  for child in &mut node.children {
    apply_animations_to_node(child, child_offset, viewport, scroll, keyframes, timelines);
  }
}

/// Applies scroll/view timeline-driven animations to the fragment tree by sampling
/// matching @keyframes rules and applying animated properties (currently opacity).
///
/// This is a lightweight hook to make scroll-driven effects visible without a full
/// animation engine. It uses the provided scroll offsets for the root viewport.
pub fn apply_scroll_driven_animations(tree: &mut FragmentTree, scroll: Point) {
  if tree.keyframes.is_empty() {
    return;
  }

  let viewport = Rect::from_xywh(
    0.0,
    0.0,
    tree.viewport_size().width,
    tree.viewport_size().height,
  );
  let content = tree.content_size();
  let mut timelines: HashMap<String, TimelineState> = HashMap::new();
  let root_offset = Point::new(tree.root.bounds.x(), tree.root.bounds.y());
  collect_timelines(
    &tree.root,
    root_offset,
    viewport,
    content,
    scroll,
    &mut timelines,
  );
  for frag in &tree.additional_fragments {
    let offset = Point::new(frag.bounds.x(), frag.bounds.y());
    collect_timelines(frag, offset, viewport, content, scroll, &mut timelines);
  }

  apply_animations_to_node(
    &mut tree.root,
    root_offset,
    viewport,
    scroll,
    &tree.keyframes,
    &timelines,
  );
  for frag in &mut tree.additional_fragments {
    let offset = Point::new(frag.bounds.x(), frag.bounds.y());
    apply_animations_to_node(frag, offset, viewport, scroll, &tree.keyframes, &timelines);
  }
}

trait AnimationRangeExt {
  fn start(&self) -> &RangeOffset;
  fn end(&self) -> &RangeOffset;
}

impl AnimationRangeExt for AnimationRange {
  fn start(&self) -> &RangeOffset {
    &self.start
  }

  fn end(&self) -> &RangeOffset {
    &self.end
  }
}
