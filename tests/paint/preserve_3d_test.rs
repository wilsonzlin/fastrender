use fastrender::paint::display_list::{
  BlendMode, BorderRadii, DisplayItem, DisplayList, FillRectItem, StackingContextItem, Transform3D,
};
use fastrender::paint::display_list_renderer::DisplayListRenderer;
use fastrender::style::color::Rgba;
use fastrender::style::types::{BackfaceVisibility, TransformStyle};
use fastrender::text::font_loader::FontContext;
use fastrender::Rect;

#[test]
fn preserve3d_degenerate_transform_renders() {
  let mut list = DisplayList::new();

  list.push(DisplayItem::PushStackingContext(StackingContextItem {
    z_index: 0,
    creates_stacking_context: true,
    bounds: Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
    plane_rect: Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
    mix_blend_mode: BlendMode::Normal,
    is_isolated: false,
    transform: None,
    child_perspective: None,
    transform_style: TransformStyle::Preserve3d,
    backface_visibility: BackfaceVisibility::Visible,
    filters: Vec::new(),
    backdrop_filters: Vec::new(),
    radii: BorderRadii::ZERO,
    mask: None,
  }));

  let mut degenerate = Transform3D::IDENTITY;
  // Zero out w to force degenerate projection; compositor should flatten gracefully.
  degenerate.m[15] = 0.0;

  list.push(DisplayItem::PushStackingContext(StackingContextItem {
    z_index: 1,
    creates_stacking_context: true,
    bounds: Rect::from_xywh(1.0, 1.0, 8.0, 8.0),
    plane_rect: Rect::from_xywh(1.0, 1.0, 8.0, 8.0),
    mix_blend_mode: BlendMode::Normal,
    is_isolated: false,
    transform: Some(degenerate),
    child_perspective: None,
    transform_style: TransformStyle::Preserve3d,
    backface_visibility: BackfaceVisibility::Visible,
    filters: Vec::new(),
    backdrop_filters: Vec::new(),
    radii: BorderRadii::ZERO,
    mask: None,
  }));

  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(1.0, 1.0, 8.0, 8.0),
    color: Rgba::BLACK,
  }));

  list.push(DisplayItem::PopStackingContext);
  list.push(DisplayItem::PopStackingContext);

  let renderer = DisplayListRenderer::new(16, 16, Rgba::WHITE, FontContext::new()).unwrap();
  renderer.render(&list).unwrap();
}
