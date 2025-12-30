use crate::geometry::Point;
use crate::geometry::Rect;
use crate::paint::display_list::BorderRadii;
use crate::paint::pixmap::new_pixmap;
use crate::style::types::BackgroundPosition;
use crate::style::types::BasicShape;
use crate::style::types::ClipPath;
use crate::style::types::ClipRadii;
use crate::style::types::FillRule;
use crate::style::types::ReferenceBox;
use crate::style::types::ShapeRadius;
use crate::style::values::Length;
use crate::style::ComputedStyle;
use crate::text::font_loader::FontContext;
use tiny_skia::FillRule as SkFillRule;
use tiny_skia::IntSize;
use tiny_skia::Mask;
use tiny_skia::MaskType;
use tiny_skia::PathBuilder;
use tiny_skia::Rect as SkRect;
use tiny_skia::Transform;

#[derive(Debug, Clone, PartialEq)]
pub enum ResolvedClipPath {
  Inset {
    rect: Rect,
    radii: BorderRadii,
  },
  Circle {
    center: Point,
    radius: f32,
  },
  Ellipse {
    center: Point,
    radius_x: f32,
    radius_y: f32,
  },
  Polygon {
    points: Vec<Point>,
    fill_rule: SkFillRule,
  },
}

impl ResolvedClipPath {
  pub(crate) fn translate(&self, dx: f32, dy: f32) -> Self {
    let delta = Point::new(dx, dy);
    match self {
      ResolvedClipPath::Inset { rect, radii } => ResolvedClipPath::Inset {
        rect: rect.translate(delta),
        radii: *radii,
      },
      ResolvedClipPath::Circle { center, radius } => ResolvedClipPath::Circle {
        center: Point::new(center.x + dx, center.y + dy),
        radius: *radius,
      },
      ResolvedClipPath::Ellipse {
        center,
        radius_x,
        radius_y,
      } => ResolvedClipPath::Ellipse {
        center: Point::new(center.x + dx, center.y + dy),
        radius_x: *radius_x,
        radius_y: *radius_y,
      },
      ResolvedClipPath::Polygon { points, fill_rule } => ResolvedClipPath::Polygon {
        points: points
          .iter()
          .map(|p| Point::new(p.x + dx, p.y + dy))
          .collect(),
        fill_rule: *fill_rule,
      },
    }
  }

  pub(crate) fn bounds(&self) -> Rect {
    match self {
      ResolvedClipPath::Inset { rect, .. } => *rect,
      ResolvedClipPath::Circle { center, radius } => Rect::from_xywh(
        center.x - radius,
        center.y - radius,
        radius * 2.0,
        radius * 2.0,
      ),
      ResolvedClipPath::Ellipse {
        center,
        radius_x,
        radius_y,
      } => Rect::from_xywh(
        center.x - radius_x,
        center.y - radius_y,
        radius_x * 2.0,
        radius_y * 2.0,
      ),
      ResolvedClipPath::Polygon { points, .. } => {
        let mut min_x = f32::INFINITY;
        let mut min_y = f32::INFINITY;
        let mut max_x = f32::NEG_INFINITY;
        let mut max_y = f32::NEG_INFINITY;
        for p in points {
          min_x = min_x.min(p.x);
          min_y = min_y.min(p.y);
          max_x = max_x.max(p.x);
          max_y = max_y.max(p.y);
        }
        Rect::from_xywh(
          min_x,
          min_y,
          (max_x - min_x).max(0.0),
          (max_y - min_y).max(0.0),
        )
      }
    }
  }

  pub(crate) fn mask(&self, scale: f32, size: IntSize, transform: Transform) -> Option<Mask> {
    if size.width() == 0 || size.height() == 0 {
      return None;
    }
    let mut mask_pixmap = new_pixmap(size.width(), size.height())?;
    let mut paint = tiny_skia::Paint::default();
    paint.anti_alias = true;
    paint.set_color(tiny_skia::Color::from_rgba8(255, 255, 255, 255));

    match self {
      ResolvedClipPath::Inset { rect, radii } => {
        let scaled = Rect::from_xywh(
          rect.x() * scale,
          rect.y() * scale,
          rect.width() * scale,
          rect.height() * scale,
        );
        if scaled.width() <= 0.0 || scaled.height() <= 0.0 {
          return None;
        }
        let scaled_radii = BorderRadii {
          top_left: radii.top_left * scale,
          top_right: radii.top_right * scale,
          bottom_right: radii.bottom_right * scale,
          bottom_left: radii.bottom_left * scale,
        }
        .clamped(scaled.width(), scaled.height());

        if let Some(path) = crate::paint::rasterize::build_rounded_rect_path(
          scaled.x(),
          scaled.y(),
          scaled.width(),
          scaled.height(),
          &scaled_radii,
        ) {
          mask_pixmap.fill_path(&path, &paint, SkFillRule::Winding, transform, None);
        }
      }
      ResolvedClipPath::Circle { center, radius } => {
        if *radius <= 0.0 {
          return None;
        }
        let r = *radius * scale;
        let rect = SkRect::from_xywh(center.x * scale - r, center.y * scale - r, r * 2.0, r * 2.0)?;
        if let Some(path) = PathBuilder::from_oval(rect) {
          mask_pixmap.fill_path(&path, &paint, SkFillRule::Winding, transform, None);
        }
      }
      ResolvedClipPath::Ellipse {
        center,
        radius_x,
        radius_y,
      } => {
        if *radius_x <= 0.0 || *radius_y <= 0.0 {
          return None;
        }
        let rect = SkRect::from_xywh(
          (center.x - radius_x) * scale,
          (center.y - radius_y) * scale,
          radius_x * 2.0 * scale,
          radius_y * 2.0 * scale,
        )?;
        if let Some(path) = PathBuilder::from_oval(rect) {
          mask_pixmap.fill_path(&path, &paint, SkFillRule::Winding, transform, None);
        }
      }
      ResolvedClipPath::Polygon { points, fill_rule } => {
        if points.len() < 3 {
          return None;
        }
        let mut builder = PathBuilder::new();
        builder.move_to(points[0].x * scale, points[0].y * scale);
        for p in &points[1..] {
          builder.line_to(p.x * scale, p.y * scale);
        }
        builder.close();
        if let Some(path) = builder.finish() {
          mask_pixmap.fill_path(&path, &paint, *fill_rule, transform, None);
        }
      }
    }

    Some(Mask::from_pixmap(mask_pixmap.as_ref(), MaskType::Alpha))
  }
}

pub(crate) fn resolve_clip_path(
  style: &ComputedStyle,
  bounds: Rect,
  viewport: (f32, f32),
  font_ctx: &FontContext,
) -> Option<ResolvedClipPath> {
  match &style.clip_path {
    ClipPath::None => None,
    ClipPath::Box(reference) => {
      let boxes = reference_boxes(style, bounds, viewport, font_ctx);
      let rect = select_reference_box(boxes, *reference);
      let radii = resolve_reference_box_radii(style, &boxes, *reference, viewport, font_ctx);
      Some(ResolvedClipPath::Inset { rect, radii })
    }
    ClipPath::BasicShape(shape, reference_override) => {
      let boxes = reference_boxes(style, bounds, viewport, font_ctx);
      let reference = reference_override.unwrap_or(ReferenceBox::BorderBox);
      let reference_rect = select_reference_box(boxes, reference);
      resolve_basic_shape(shape, reference_rect, style, viewport, font_ctx)
    }
  }
}

pub(crate) fn resolve_basic_shape(
  shape: &BasicShape,
  reference: Rect,
  style: &ComputedStyle,
  viewport: (f32, f32),
  font_ctx: &FontContext,
) -> Option<ResolvedClipPath> {
  match shape {
    BasicShape::Inset {
      top,
      right,
      bottom,
      left,
      border_radius,
    } => {
      let top_px = resolve_clip_length(*top, style, reference.height(), viewport, font_ctx);
      let right_px = resolve_clip_length(*right, style, reference.width(), viewport, font_ctx);
      let bottom_px = resolve_clip_length(*bottom, style, reference.height(), viewport, font_ctx);
      let left_px = resolve_clip_length(*left, style, reference.width(), viewport, font_ctx);
      let rect = Rect::from_xywh(
        reference.x() + left_px,
        reference.y() + top_px,
        (reference.width() - left_px - right_px).max(0.0),
        (reference.height() - top_px - bottom_px).max(0.0),
      );
      let radii = border_radius
        .map(|r| resolve_clip_radii(&r, style, rect, viewport, font_ctx))
        .unwrap_or(BorderRadii::ZERO)
        .clamped(rect.width(), rect.height());
      Some(ResolvedClipPath::Inset { rect, radii })
    }
    BasicShape::Circle { radius, position } => {
      let center = resolve_position(position, reference, style, viewport, font_ctx);
      let resolved_radius =
        resolve_circle_radius(radius, center, reference, style, viewport, font_ctx);
      if resolved_radius <= 0.0 {
        return None;
      }
      Some(ResolvedClipPath::Circle {
        center,
        radius: resolved_radius,
      })
    }
    BasicShape::Ellipse {
      radius_x,
      radius_y,
      position,
    } => {
      let center = resolve_position(position, reference, style, viewport, font_ctx);
      let rx = resolve_ellipse_radius(radius_x, true, center, reference, style, viewport, font_ctx);
      let ry = resolve_ellipse_radius(
        radius_y, false, center, reference, style, viewport, font_ctx,
      );
      if rx <= 0.0 || ry <= 0.0 {
        return None;
      }
      Some(ResolvedClipPath::Ellipse {
        center,
        radius_x: rx,
        radius_y: ry,
      })
    }
    BasicShape::Polygon { fill, points } => {
      if points.len() < 3 {
        return None;
      }
      let resolved: Vec<Point> = points
        .iter()
        .map(|(x, y)| {
          Point::new(
            reference.x() + resolve_clip_length(*x, style, reference.width(), viewport, font_ctx),
            reference.y() + resolve_clip_length(*y, style, reference.height(), viewport, font_ctx),
          )
        })
        .collect();
      let fill_rule = match fill {
        FillRule::EvenOdd => SkFillRule::EvenOdd,
        FillRule::NonZero => SkFillRule::Winding,
      };
      Some(ResolvedClipPath::Polygon {
        points: resolved,
        fill_rule,
      })
    }
  }
}

fn resolve_circle_radius(
  radius: &ShapeRadius,
  center: Point,
  reference: Rect,
  style: &ComputedStyle,
  viewport: (f32, f32),
  font_ctx: &FontContext,
) -> f32 {
  match radius {
    ShapeRadius::Length(len) => resolve_clip_length(
      *len,
      style,
      reference.width().min(reference.height()),
      viewport,
      font_ctx,
    ),
    ShapeRadius::ClosestSide => {
      let dx = (center.x - reference.x())
        .abs()
        .min((reference.max_x() - center.x).abs());
      let dy = (center.y - reference.y())
        .abs()
        .min((reference.max_y() - center.y).abs());
      dx.min(dy)
    }
    ShapeRadius::FarthestSide => {
      let dx = (center.x - reference.x())
        .abs()
        .max((reference.max_x() - center.x).abs());
      let dy = (center.y - reference.y())
        .abs()
        .max((reference.max_y() - center.y).abs());
      dx.max(dy)
    }
  }
}

fn resolve_ellipse_radius(
  radius: &ShapeRadius,
  horizontal: bool,
  center: Point,
  reference: Rect,
  style: &ComputedStyle,
  viewport: (f32, f32),
  font_ctx: &FontContext,
) -> f32 {
  let base = if horizontal {
    reference.width()
  } else {
    reference.height()
  };
  match radius {
    ShapeRadius::Length(len) => resolve_clip_length(*len, style, base, viewport, font_ctx),
    ShapeRadius::ClosestSide => {
      if horizontal {
        (center.x - reference.x())
          .abs()
          .min((reference.max_x() - center.x).abs())
      } else {
        (center.y - reference.y())
          .abs()
          .min((reference.max_y() - center.y).abs())
      }
    }
    ShapeRadius::FarthestSide => {
      if horizontal {
        (center.x - reference.x())
          .abs()
          .max((reference.max_x() - center.x).abs())
      } else {
        (center.y - reference.y())
          .abs()
          .max((reference.max_y() - center.y).abs())
      }
    }
  }
}

fn resolve_position(
  position: &BackgroundPosition,
  reference: Rect,
  style: &ComputedStyle,
  viewport: (f32, f32),
  font_ctx: &FontContext,
) -> Point {
  let (x, y) = match position {
    BackgroundPosition::Position { x, y } => {
      let px = resolve_position_component(
        x,
        reference.x(),
        reference.width(),
        style,
        viewport,
        font_ctx,
      );
      let py = resolve_position_component(
        y,
        reference.y(),
        reference.height(),
        style,
        viewport,
        font_ctx,
      );
      (px, py)
    }
  };
  Point::new(x, y)
}

fn resolve_position_component(
  component: &crate::style::types::BackgroundPositionComponent,
  origin: f32,
  length: f32,
  style: &ComputedStyle,
  viewport: (f32, f32),
  font_ctx: &FontContext,
) -> f32 {
  let offset = resolve_clip_length(component.offset, style, length, viewport, font_ctx);
  origin + component.alignment * length + offset
}

fn resolve_clip_length(
  len: Length,
  style: &ComputedStyle,
  percentage_base: f32,
  viewport: (f32, f32),
  _font_ctx: &FontContext,
) -> f32 {
  len
    .resolve_with_context(
      Some(percentage_base),
      viewport.0,
      viewport.1,
      style.font_size,
      style.root_font_size,
    )
    .unwrap_or(len.value)
}

fn resolve_clip_radii(
  radii: &ClipRadii,
  style: &ComputedStyle,
  rect: Rect,
  viewport: (f32, f32),
  font_ctx: &FontContext,
) -> BorderRadii {
  BorderRadii {
    top_left: crate::paint::display_list::BorderRadius {
      x: resolve_clip_length(radii.top_left.x, style, rect.width(), viewport, font_ctx),
      y: resolve_clip_length(radii.top_left.y, style, rect.height(), viewport, font_ctx),
    },
    top_right: crate::paint::display_list::BorderRadius {
      x: resolve_clip_length(radii.top_right.x, style, rect.width(), viewport, font_ctx),
      y: resolve_clip_length(radii.top_right.y, style, rect.height(), viewport, font_ctx),
    },
    bottom_right: crate::paint::display_list::BorderRadius {
      x: resolve_clip_length(
        radii.bottom_right.x,
        style,
        rect.width(),
        viewport,
        font_ctx,
      ),
      y: resolve_clip_length(
        radii.bottom_right.y,
        style,
        rect.height(),
        viewport,
        font_ctx,
      ),
    },
    bottom_left: crate::paint::display_list::BorderRadius {
      x: resolve_clip_length(radii.bottom_left.x, style, rect.width(), viewport, font_ctx),
      y: resolve_clip_length(
        radii.bottom_left.y,
        style,
        rect.height(),
        viewport,
        font_ctx,
      ),
    },
  }
}

#[derive(Clone, Copy)]
struct ReferenceBoxes {
  border: Rect,
  padding: Rect,
  content: Rect,
  margin: Rect,
  border_left: f32,
  border_right: f32,
  border_top: f32,
  border_bottom: f32,
  padding_left: f32,
  padding_right: f32,
  padding_top: f32,
  padding_bottom: f32,
}

fn reference_boxes(
  style: &ComputedStyle,
  bounds: Rect,
  viewport: (f32, f32),
  font_ctx: &FontContext,
) -> ReferenceBoxes {
  let base = bounds.width().max(0.0);
  let border_left = resolve_clip_length(style.border_left_width, style, base, viewport, font_ctx);
  let border_right = resolve_clip_length(style.border_right_width, style, base, viewport, font_ctx);
  let border_top = resolve_clip_length(style.border_top_width, style, base, viewport, font_ctx);
  let border_bottom =
    resolve_clip_length(style.border_bottom_width, style, base, viewport, font_ctx);

  let padding_left = resolve_clip_length(style.padding_left, style, base, viewport, font_ctx);
  let padding_right = resolve_clip_length(style.padding_right, style, base, viewport, font_ctx);
  let padding_top = resolve_clip_length(style.padding_top, style, base, viewport, font_ctx);
  let padding_bottom = resolve_clip_length(style.padding_bottom, style, base, viewport, font_ctx);

  let border_rect = bounds;
  let padding_rect = inset_rect(
    border_rect,
    border_left,
    border_top,
    border_right,
    border_bottom,
  );
  let content_rect = inset_rect(
    padding_rect,
    padding_left,
    padding_top,
    padding_right,
    padding_bottom,
  );

  let margin_left = style
    .margin_left
    .map(|m| resolve_clip_length(m, style, base, viewport, font_ctx))
    .unwrap_or(0.0);
  let margin_right = style
    .margin_right
    .map(|m| resolve_clip_length(m, style, base, viewport, font_ctx))
    .unwrap_or(0.0);
  let margin_top = style
    .margin_top
    .map(|m| resolve_clip_length(m, style, base, viewport, font_ctx))
    .unwrap_or(0.0);
  let margin_bottom = style
    .margin_bottom
    .map(|m| resolve_clip_length(m, style, base, viewport, font_ctx))
    .unwrap_or(0.0);

  let margin_rect = Rect::from_xywh(
    border_rect.x() - margin_left,
    border_rect.y() - margin_top,
    border_rect.width() + margin_left + margin_right,
    border_rect.height() + margin_top + margin_bottom,
  );

  ReferenceBoxes {
    border: border_rect,
    padding: padding_rect,
    content: content_rect,
    margin: margin_rect,
    border_left,
    border_right,
    border_top,
    border_bottom,
    padding_left,
    padding_right,
    padding_top,
    padding_bottom,
  }
}

fn resolve_border_radii_from_style(
  style: &ComputedStyle,
  border_rect: Rect,
  viewport: (f32, f32),
  font_ctx: &FontContext,
) -> BorderRadii {
  let width = border_rect.width().max(0.0);
  let height = border_rect.height().max(0.0);
  if width <= 0.0 || height <= 0.0 {
    return BorderRadii::ZERO;
  }

  let resolve = |len: Length, reference: f32| {
    resolve_clip_length(len, style, reference, viewport, font_ctx).max(0.0)
  };

  BorderRadii {
    top_left: crate::paint::display_list::BorderRadius {
      x: resolve(style.border_top_left_radius.x, width),
      y: resolve(style.border_top_left_radius.y, height),
    },
    top_right: crate::paint::display_list::BorderRadius {
      x: resolve(style.border_top_right_radius.x, width),
      y: resolve(style.border_top_right_radius.y, height),
    },
    bottom_right: crate::paint::display_list::BorderRadius {
      x: resolve(style.border_bottom_right_radius.x, width),
      y: resolve(style.border_bottom_right_radius.y, height),
    },
    bottom_left: crate::paint::display_list::BorderRadius {
      x: resolve(style.border_bottom_left_radius.x, width),
      y: resolve(style.border_bottom_left_radius.y, height),
    },
  }
  .clamped(width, height)
}

fn resolve_reference_box_radii(
  style: &ComputedStyle,
  boxes: &ReferenceBoxes,
  reference: ReferenceBox,
  viewport: (f32, f32),
  font_ctx: &FontContext,
) -> BorderRadii {
  let base = resolve_border_radii_from_style(style, boxes.border, viewport, font_ctx);
  match reference {
    ReferenceBox::PaddingBox => {
      let shrunk = BorderRadii {
        top_left: crate::paint::display_list::BorderRadius {
          x: (base.top_left.x - boxes.border_left).max(0.0),
          y: (base.top_left.y - boxes.border_top).max(0.0),
        },
        top_right: crate::paint::display_list::BorderRadius {
          x: (base.top_right.x - boxes.border_right).max(0.0),
          y: (base.top_right.y - boxes.border_top).max(0.0),
        },
        bottom_right: crate::paint::display_list::BorderRadius {
          x: (base.bottom_right.x - boxes.border_right).max(0.0),
          y: (base.bottom_right.y - boxes.border_bottom).max(0.0),
        },
        bottom_left: crate::paint::display_list::BorderRadius {
          x: (base.bottom_left.x - boxes.border_left).max(0.0),
          y: (base.bottom_left.y - boxes.border_bottom).max(0.0),
        },
      };
      shrunk.clamped(boxes.padding.width(), boxes.padding.height())
    }
    ReferenceBox::ContentBox => {
      let shrink_left = boxes.border_left + boxes.padding_left;
      let shrink_right = boxes.border_right + boxes.padding_right;
      let shrink_top = boxes.border_top + boxes.padding_top;
      let shrink_bottom = boxes.border_bottom + boxes.padding_bottom;
      let shrunk = BorderRadii {
        top_left: crate::paint::display_list::BorderRadius {
          x: (base.top_left.x - shrink_left).max(0.0),
          y: (base.top_left.y - shrink_top).max(0.0),
        },
        top_right: crate::paint::display_list::BorderRadius {
          x: (base.top_right.x - shrink_right).max(0.0),
          y: (base.top_right.y - shrink_top).max(0.0),
        },
        bottom_right: crate::paint::display_list::BorderRadius {
          x: (base.bottom_right.x - shrink_right).max(0.0),
          y: (base.bottom_right.y - shrink_bottom).max(0.0),
        },
        bottom_left: crate::paint::display_list::BorderRadius {
          x: (base.bottom_left.x - shrink_left).max(0.0),
          y: (base.bottom_left.y - shrink_bottom).max(0.0),
        },
      };
      shrunk.clamped(boxes.content.width(), boxes.content.height())
    }
    ReferenceBox::MarginBox => base.clamped(boxes.margin.width(), boxes.margin.height()),
    ReferenceBox::BorderBox
    | ReferenceBox::FillBox
    | ReferenceBox::StrokeBox
    | ReferenceBox::ViewBox => base,
  }
}

fn select_reference_box(boxes: ReferenceBoxes, reference: ReferenceBox) -> Rect {
  match reference {
    ReferenceBox::BorderBox
    | ReferenceBox::FillBox
    | ReferenceBox::StrokeBox
    | ReferenceBox::ViewBox => boxes.border,
    ReferenceBox::PaddingBox => boxes.padding,
    ReferenceBox::ContentBox => boxes.content,
    ReferenceBox::MarginBox => boxes.margin,
  }
}

fn inset_rect(rect: Rect, left: f32, top: f32, right: f32, bottom: f32) -> Rect {
  Rect::from_xywh(
    rect.x() + left,
    rect.y() + top,
    (rect.width() - left - right).max(0.0),
    (rect.height() - top - bottom).max(0.0),
  )
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::style::types::BorderCornerRadius;

  #[test]
  fn clip_path_box_uses_border_radius() {
    let mut style = ComputedStyle::default();
    style.clip_path = ClipPath::Box(ReferenceBox::BorderBox);
    let radius = BorderCornerRadius::uniform(Length::px(6.0));
    style.border_top_left_radius = radius;
    style.border_top_right_radius = radius;
    style.border_bottom_right_radius = radius;
    style.border_bottom_left_radius = radius;

    let bounds = Rect::from_xywh(0.0, 0.0, 50.0, 40.0);
    let viewport = (100.0, 100.0);
    let clip = resolve_clip_path(&style, bounds, viewport, &FontContext::new())
      .expect("expected clip-path to resolve");

    let radii = match clip {
      ResolvedClipPath::Inset { radii, .. } => radii,
      _ => panic!("expected inset clip path"),
    };

    assert!(radii.top_left.x > 0.0);
    assert!(radii.top_left.y > 0.0);
    assert!(!radii.is_zero());
  }

  #[test]
  fn clip_path_padding_box_shrinks_radii() {
    let mut style = ComputedStyle::default();
    style.clip_path = ClipPath::Box(ReferenceBox::PaddingBox);
    let radius = BorderCornerRadius::uniform(Length::px(12.0));
    style.border_top_left_radius = radius;
    style.border_top_right_radius = radius;
    style.border_bottom_right_radius = radius;
    style.border_bottom_left_radius = radius;
    style.border_left_width = Length::px(3.0);
    style.border_top_width = Length::px(3.0);
    style.border_right_width = Length::px(3.0);
    style.border_bottom_width = Length::px(3.0);

    let bounds = Rect::from_xywh(0.0, 0.0, 40.0, 40.0);
    let viewport = (100.0, 100.0);
    let clip = resolve_clip_path(&style, bounds, viewport, &FontContext::new())
      .expect("expected clip-path to resolve");

    let radii = match clip {
      ResolvedClipPath::Inset { radii, .. } => radii,
      _ => panic!("expected inset clip path"),
    };

    assert!((radii.top_left.x - 9.0).abs() < 0.01);
    assert!((radii.top_left.y - 9.0).abs() < 0.01);
    assert!(radii.top_left.x < 12.0);
  }

  #[test]
  fn clip_path_preserves_elliptical_radii() {
    let mut style = ComputedStyle::default();
    style.clip_path = ClipPath::Box(ReferenceBox::BorderBox);
    let radius = BorderCornerRadius {
      x: Length::px(10.0),
      y: Length::px(20.0),
    };
    style.border_top_left_radius = radius;
    style.border_top_right_radius = radius;
    style.border_bottom_right_radius = radius;
    style.border_bottom_left_radius = radius;

    let bounds = Rect::from_xywh(0.0, 0.0, 80.0, 60.0);
    let viewport = (100.0, 100.0);
    let clip = resolve_clip_path(&style, bounds, viewport, &FontContext::new())
      .expect("expected clip-path to resolve");

    let radii = match clip {
      ResolvedClipPath::Inset { radii, .. } => radii,
      _ => panic!("expected inset clip path"),
    };

    assert!((radii.top_left.x - 10.0).abs() < 0.01);
    assert!((radii.top_left.y - 20.0).abs() < 0.01);
  }
}
