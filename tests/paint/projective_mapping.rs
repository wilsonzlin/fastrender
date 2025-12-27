use super::util::bounding_box_for_color;
use fastrender::geometry::Rect;
use fastrender::paint::display_list::{
  quad_from_transform3d, BlendMode, BorderRadii, DisplayItem, DisplayList, FillRectItem,
  StackingContextItem, Transform3D, TransformItem,
};
use fastrender::paint::display_list_renderer::DisplayListRenderer;
use fastrender::paint::optimize::DisplayListOptimizer;
use fastrender::style::color::Rgba;
use fastrender::style::types::{BackfaceVisibility, TransformStyle};
use fastrender::text::font_loader::FontContext;

fn near_clipping_transform() -> Transform3D {
  Transform3D::perspective(1.0).multiply(&Transform3D::translate(0.0, 0.0, 0.9995))
}

#[test]
fn project_quad_rejects_near_zero_w() {
  let rect = Rect::from_xywh(0.0, 0.0, 10.0, 10.0);
  assert!(
    quad_from_transform3d(rect, &near_clipping_transform()).is_none(),
    "expected near-clipping transform to be treated as invalid"
  );
}

#[test]
fn renderer_falls_back_for_degenerate_projection() {
  let renderer = DisplayListRenderer::new(20, 20, Rgba::WHITE, FontContext::new()).unwrap();
  let mut list = DisplayList::new();
  list.push(DisplayItem::PushStackingContext(StackingContextItem {
    z_index: 0,
    creates_stacking_context: true,
    bounds: Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
    plane_rect: Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
    mix_blend_mode: BlendMode::Normal,
    is_isolated: false,
    transform: Some(near_clipping_transform()),
    child_perspective: None,
    transform_style: TransformStyle::Flat,
    backface_visibility: BackfaceVisibility::Visible,
    filters: Vec::new(),
    backdrop_filters: Vec::new(),
    radii: BorderRadii::ZERO,
    mask: None,
  }));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(2.0, 2.0, 8.0, 8.0),
    color: Rgba::RED,
  }));
  list.push(DisplayItem::PopStackingContext);

  let pixmap = renderer.render(&list).expect("render should succeed");
  let bbox =
    bounding_box_for_color(&pixmap, |(_, _, _, a)| a > 0).expect("content should be visible");
  assert!(
    bbox.2 < pixmap.width() && bbox.3 < pixmap.height(),
    "rendered content should stay within the destination bounds, got {bbox:?}"
  );
}

#[test]
fn optimizer_keeps_items_with_degenerate_projection() {
  let mut list = DisplayList::new();
  list.push(DisplayItem::PushTransform(TransformItem {
    transform: near_clipping_transform(),
  }));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(200.0, 200.0, 5.0, 5.0),
    color: Rgba::BLUE,
  }));
  list.push(DisplayItem::PopTransform);

  let optimizer = DisplayListOptimizer::new();
  let viewport = Rect::from_xywh(0.0, 0.0, 10.0, 10.0);
  let (optimized, _) = optimizer.optimize(list, viewport);

  assert_eq!(
    optimized.len(),
    3,
    "degenerate projective transforms should disable culling"
  );
}
