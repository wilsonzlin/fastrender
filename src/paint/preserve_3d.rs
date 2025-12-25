//! Preserve-3D compositor fallback logic.
//!
//! This module classifies transforms and provides a deterministic fallback path
//! when full projective warping is unavailable. It keeps rendering stable by
//! approximating perspective transforms to affine matrices when needed while
//! still sorting planes by depth when possible.

use crate::geometry::Rect;
use crate::paint::display_list::{DisplayItem, DisplayList, StackingContextItem, Transform2D, Transform3D};
use crate::style::types::{BackfaceVisibility, TransformStyle};
use std::cmp::Ordering;
use std::sync::OnceLock;

/// Classification of a 3D transform for preserve-3d composition.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TransformKind {
  /// Fully 2D affine (round-trips via [`Transform3D::to_2d`]).
  Affine2D,
  /// 3D without perspective components.
  NonPerspective3D,
  /// Contains perspective (projective) terms.
  Perspective3D,
  /// Cannot be projected stably (w≈0 or non-finite).
  Degenerate,
}

/// Selected fallback/rendering path for an item.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FallbackPath {
  /// Use projective warp (only when available and stable).
  Warp,
  /// Render using a 2D approximation of the transform.
  Approximate2d,
  /// Fall back to original paint order (no depth sorting).
  FlatOrder,
}

/// Scene item used for diagnostics and classification.
#[derive(Debug, Clone)]
pub struct SceneItem {
  /// World transform for the plane.
  pub transform: Transform3D,
  /// Bounds of the plane being projected.
  pub bounds: Rect,
  /// Whether descendants should preserve the 3D context.
  pub transform_style: TransformStyle,
  /// Backface visibility of the item.
  pub backface_visibility: BackfaceVisibility,
}

/// Options controlling preserve-3d composition.
#[derive(Debug, Clone, Copy)]
pub struct Preserve3dOptions {
  /// Whether projective warping is available.
  pub warp_available: bool,
  /// Whether to emit diagnostics to stderr.
  pub diagnostics: bool,
}

impl Default for Preserve3dOptions {
  fn default() -> Self {
    default_options()
  }
}

fn env_flag(name: &str) -> bool {
  std::env::var(name)
    .map(|v| v != "0" && !v.eq_ignore_ascii_case("false"))
    .unwrap_or(false)
}

fn default_options() -> Preserve3dOptions {
  static OPTIONS: OnceLock<Preserve3dOptions> = OnceLock::new();
  *OPTIONS.get_or_init(|| Preserve3dOptions {
    warp_available: env_flag("FASTR_PRESERVE3D_WARP"),
    diagnostics: env_flag("FASTR_PRESERVE3D_DEBUG"),
  })
}

/// Classify a scene item transform for preserve-3d composition.
pub fn classify_transform(item: &SceneItem) -> TransformKind {
  let transform = &item.transform;
  if is_degenerate_projection(transform, item.bounds) {
    return TransformKind::Degenerate;
  }
  if transform.to_2d().is_some() {
    return TransformKind::Affine2D;
  }
  if has_perspective(transform) {
    return TransformKind::Perspective3D;
  }
  TransformKind::NonPerspective3D
}

/// Compose a display list with preserve-3d fallback handling using defaults.
///
/// - Perspective transforms are approximated to 2D when warping is unavailable.
/// - Depth sorting is performed for `transform-style: preserve-3d` contexts
///   when all child depths are stable.
pub fn composite_preserve_3d(list: &DisplayList) -> DisplayList {
  composite_preserve_3d_with_options(list, default_options())
}

/// Compose a display list with custom options (useful for testing).
pub fn composite_preserve_3d_with_options(list: &DisplayList, options: Preserve3dOptions) -> DisplayList {
  if list.is_empty() {
    return list.clone();
  }

  let root_bounds = list_bounds(list.items()).unwrap_or(Rect::ZERO);
  let mut idx = 0;
  let fragments = parse_fragments(list.items(), &mut idx);
  let mut diagnostics: Vec<String> = Vec::new();

  let items = compose_fragments(
    fragments,
    Transform3D::identity(),
    Transform3D::identity(),
    TransformStyle::Flat,
    root_bounds,
    options,
    &mut diagnostics,
    0,
  );

  if options.diagnostics {
    for line in diagnostics {
      eprintln!("{line}");
    }
  }

  DisplayList::from_items(items)
}

#[derive(Debug, Clone)]
enum FragmentNode {
  Other(DisplayItem),
  Stacking {
    item: StackingContextItem,
    children: Vec<FragmentNode>,
  },
}

#[derive(Debug)]
struct RenderableFragment {
  items: Vec<DisplayItem>,
  depth: Option<f32>,
  classification: Option<TransformKind>,
  fallback: Option<FallbackPath>,
  original_index: usize,
}

fn list_bounds(items: &[DisplayItem]) -> Option<Rect> {
  let mut bounds: Option<Rect> = None;
  for item in items {
    if let Some(b) = item.bounds() {
      bounds = Some(match bounds {
        Some(existing) => existing.union(b),
        None => b,
      });
    }
  }
  bounds
}

fn parse_fragments(items: &[DisplayItem], idx: &mut usize) -> Vec<FragmentNode> {
  let mut out = Vec::new();
  while *idx < items.len() {
    match &items[*idx] {
      DisplayItem::PushStackingContext(ctx) => {
        let item = ctx.clone();
        *idx += 1;
        let children = parse_fragments(items, idx);
        out.push(FragmentNode::Stacking { item, children });
      }
      DisplayItem::PopStackingContext => {
        *idx += 1;
        break;
      }
      other => {
        out.push(FragmentNode::Other(other.clone()));
        *idx += 1;
      }
    }
  }
  out
}

fn compose_fragments(
  fragments: Vec<FragmentNode>,
  parent_world_render: Transform3D,
  parent_world_depth: Transform3D,
  parent_style: TransformStyle,
  parent_bounds: Rect,
  options: Preserve3dOptions,
  diagnostics: &mut Vec<String>,
  depth_level: usize,
) -> Vec<DisplayItem> {
  let parent_depth = evaluate_depth(&parent_world_depth, parent_bounds);
  let mut renderable: Vec<RenderableFragment> = Vec::with_capacity(fragments.len());

  for (idx, fragment) in fragments.into_iter().enumerate() {
    match fragment {
      FragmentNode::Other(item) => {
        renderable.push(RenderableFragment {
          items: vec![item],
          depth: if matches!(parent_style, TransformStyle::Preserve3d) {
            parent_depth
          } else {
            None
          },
          classification: None,
          fallback: None,
          original_index: idx,
        });
      }
      FragmentNode::Stacking { item, children } => {
        let has_transform = item.transform.is_some();
        let local_transform = item.transform.unwrap_or_else(Transform3D::identity);
        let depth_transform = parent_world_depth.multiply(&local_transform);

        let scene_item = SceneItem {
          transform: depth_transform,
          bounds: item.bounds,
          transform_style: item.transform_style,
          backface_visibility: item.backface_visibility,
        };
        let classification = classify_transform(&scene_item);
        let depth = if matches!(classification, TransformKind::Degenerate) {
          None
        } else {
          evaluate_depth(&depth_transform, item.bounds)
        };
        let fallback = if options.warp_available && matches!(classification, TransformKind::Perspective3D) {
          FallbackPath::Warp
        } else if depth.is_none() {
          FallbackPath::FlatOrder
        } else {
          FallbackPath::Approximate2d
        };

        let render_transform = match fallback {
          FallbackPath::Warp => local_transform,
          FallbackPath::Approximate2d | FallbackPath::FlatOrder => {
            Transform3D::from_2d(&local_transform.to_2d().unwrap_or_else(|| local_transform.approximate_2d()))
          }
        };
        let effective_world_render = parent_world_render.multiply(&render_transform);
        let effective_world_depth = parent_world_depth.multiply(&local_transform);
        let composed_children = compose_fragments(
          children,
          effective_world_render,
          effective_world_depth,
          item.transform_style,
          item.bounds,
          options,
          diagnostics,
          depth_level + 1,
        );

        let mut updated_item = item.clone();
        updated_item.transform = if has_transform {
          Some(render_transform)
        } else {
          None
        };

        if options.diagnostics && matches!(parent_style, TransformStyle::Preserve3d) {
          diagnostics.push(format!(
            "preserve-3d level={depth_level} idx={idx} classification={classification:?} fallback={fallback:?} depth={depth:?}"
          ));
        }

        renderable.push(RenderableFragment {
          items: {
            let mut seq = Vec::with_capacity(composed_children.len() + 2);
            seq.push(DisplayItem::PushStackingContext(updated_item));
            seq.extend(composed_children);
            seq.push(DisplayItem::PopStackingContext);
            seq
          },
          depth: if matches!(parent_style, TransformStyle::Preserve3d) {
            depth
          } else {
            None
          },
          classification: Some(classification),
          fallback: Some(fallback),
          original_index: idx,
        });
      }
    }
  }

  let mut out: Vec<DisplayItem> = Vec::new();
  let should_sort = matches!(parent_style, TransformStyle::Preserve3d)
    && !renderable.is_empty()
    && renderable.iter().all(|f| f.depth.is_some());
  if matches!(parent_style, TransformStyle::Preserve3d) && options.diagnostics && !should_sort {
    diagnostics.push(format!(
      "preserve-3d level={depth_level} fallback=FlatOrder reason=unstable-depth"
    ));
  }

  if should_sort {
    renderable.sort_by(|a, b| {
      let da = a.depth.unwrap();
      let db = b.depth.unwrap();
      match da.partial_cmp(&db).unwrap_or(Ordering::Equal) {
        Ordering::Equal => a.original_index.cmp(&b.original_index),
        other => other,
      }
    });
  }

  for fragment in renderable {
    out.extend(fragment.items);
  }

  out
}

fn evaluate_depth(transform: &Transform3D, bounds: Rect) -> Option<f32> {
  let center = (bounds.x() + bounds.width() * 0.5, bounds.y() + bounds.height() * 0.5);
  let (_tx, _ty, tz, tw) = transform.transform_point(center.0, center.1, 0.0);
  if !tz.is_finite() || !tw.is_finite() || tw.abs() < 1e-3 {
    return None;
  }
  Some(tz / tw)
}

fn has_perspective(transform: &Transform3D) -> bool {
  const EPS: f32 = 1e-6;
  transform.m[3].abs() > EPS || transform.m[7].abs() > EPS || transform.m[11].abs() > EPS
}

fn is_degenerate_projection(transform: &Transform3D, bounds: Rect) -> bool {
  const EPS_W: f32 = 1e-3;
  let corners = [
    (bounds.min_x(), bounds.min_y()),
    (bounds.max_x(), bounds.min_y()),
    (bounds.min_x(), bounds.max_y()),
    (bounds.max_x(), bounds.max_y()),
  ];

  for (x, y) in corners {
    let (tx, ty, tz, tw) = transform.transform_point(x, y, 0.0);
    if !tx.is_finite() || !ty.is_finite() || !tz.is_finite() || !tw.is_finite() {
      return true;
    }
    if tw.abs() < EPS_W {
      return true;
    }
  }
  false
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::paint::display_list::DisplayItem;
  use crate::paint::display_list::DisplayList;
  use crate::style::types::BackfaceVisibility;

  fn basic_context(transform: Transform3D, z: f32) -> StackingContextItem {
    StackingContextItem {
      z_index: 0,
      creates_stacking_context: true,
      bounds: Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
      mix_blend_mode: crate::paint::display_list::BlendMode::Normal,
      is_isolated: false,
      transform: Some(transform.multiply(&Transform3D::translate(0.0, 0.0, z))),
      transform_style: TransformStyle::Preserve3d,
      backface_visibility: BackfaceVisibility::Visible,
      filters: Vec::new(),
      backdrop_filters: Vec::new(),
      radii: crate::paint::display_list::BorderRadii::ZERO,
      mask: None,
    }
  }

  #[test]
  fn classify_affine_and_perspective() {
    let rect = Rect::from_xywh(0.0, 0.0, 10.0, 10.0);
    let affine_item = SceneItem {
      transform: Transform3D::from_2d(&Transform2D::translate(5.0, 5.0)),
      bounds: rect,
      transform_style: TransformStyle::Preserve3d,
      backface_visibility: BackfaceVisibility::Visible,
    };
    assert_eq!(classify_transform(&affine_item), TransformKind::Affine2D);

    let perspective_item = SceneItem {
      transform: Transform3D::perspective(500.0),
      bounds: rect,
      transform_style: TransformStyle::Preserve3d,
      backface_visibility: BackfaceVisibility::Visible,
    };
    assert_eq!(classify_transform(&perspective_item), TransformKind::Perspective3D);
  }

  #[test]
  fn classify_degenerate_with_zero_w() {
    let rect = Rect::from_xywh(0.0, 0.0, 10.0, 10.0);
    let mut m = Transform3D::IDENTITY.m;
    m[15] = 0.0;
    let degenerate = SceneItem {
      transform: Transform3D { m },
      bounds: rect,
      transform_style: TransformStyle::Preserve3d,
      backface_visibility: BackfaceVisibility::Visible,
    };
    assert_eq!(classify_transform(&degenerate), TransformKind::Degenerate);
  }

  #[test]
  fn preserve3d_sorts_by_depth_without_warp() {
    let mut list = DisplayList::new();
    list.push(DisplayItem::PushStackingContext(StackingContextItem {
      z_index: 0,
      creates_stacking_context: true,
      bounds: Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
      mix_blend_mode: crate::paint::display_list::BlendMode::Normal,
      is_isolated: false,
      transform: None,
      transform_style: TransformStyle::Preserve3d,
      backface_visibility: BackfaceVisibility::Visible,
      filters: Vec::new(),
      backdrop_filters: Vec::new(),
      radii: crate::paint::display_list::BorderRadii::ZERO,
      mask: None,
    }));
    let back = basic_context(Transform3D::identity(), 0.0);
    let front = basic_context(Transform3D::identity(), 50.0);
    list.push(DisplayItem::PushStackingContext(back.clone()));
    list.push(DisplayItem::PopStackingContext);
    list.push(DisplayItem::PushStackingContext(front.clone()));
    list.push(DisplayItem::PopStackingContext);
    list.push(DisplayItem::PopStackingContext);

    let composed = composite_preserve_3d_with_options(
      &list,
      Preserve3dOptions {
        warp_available: false,
        diagnostics: false,
      },
    );

    let mut stacking_transforms: Vec<Transform3D> = Vec::new();
    for item in composed.items() {
      if let DisplayItem::PushStackingContext(ctx) = item {
        if ctx.transform_style == TransformStyle::Preserve3d && ctx.transform.is_some() {
          stacking_transforms.push(ctx.transform.unwrap());
        }
      }
    }

    // Expect back (z=0) then front (z=50) in paint order.
    assert_eq!(stacking_transforms.len(), 2);
    let depth0 = stacking_transforms[0].transform_point(0.0, 0.0, 0.0).2;
    let depth1 = stacking_transforms[1].transform_point(0.0, 0.0, 0.0).2;
    assert!(depth0 < depth1, "expected back element before front element");
  }

  #[test]
  fn unstable_depth_keeps_original_order() {
    let mut list = DisplayList::new();
    list.push(DisplayItem::PushStackingContext(StackingContextItem {
      z_index: 0,
      creates_stacking_context: true,
      bounds: Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
      mix_blend_mode: crate::paint::display_list::BlendMode::Normal,
      is_isolated: false,
      transform: None,
      transform_style: TransformStyle::Preserve3d,
      backface_visibility: BackfaceVisibility::Visible,
      filters: Vec::new(),
      backdrop_filters: Vec::new(),
      radii: crate::paint::display_list::BorderRadii::ZERO,
      mask: None,
    }));

    // Degenerate child forces flat paint order.
    let mut degenerate = basic_context(Transform3D::identity(), 0.0);
    degenerate.z_index = 1;
    if let Some(t) = degenerate.transform.as_mut() {
      t.m[15] = 0.0;
    }
    let mut front = basic_context(Transform3D::identity(), 100.0);
    front.z_index = 2;
    list.push(DisplayItem::PushStackingContext(degenerate.clone()));
    list.push(DisplayItem::PopStackingContext);
    list.push(DisplayItem::PushStackingContext(front.clone()));
    list.push(DisplayItem::PopStackingContext);
    list.push(DisplayItem::PopStackingContext);

    let composed = composite_preserve_3d_with_options(
      &list,
      Preserve3dOptions {
        warp_available: false,
        diagnostics: false,
      },
    );

    let mut order: Vec<i32> = Vec::new();
    for item in composed.items() {
      if let DisplayItem::PushStackingContext(ctx) = item {
        if ctx.transform_style == TransformStyle::Preserve3d && ctx.transform.is_some() {
          order.push(ctx.z_index);
        }
      }
    }

    // Order should remain as authored (degenerate first).
    assert_eq!(order.len(), 2);
    assert_eq!(order[0], 1);
    assert_eq!(order[1], 2);
  }
}
