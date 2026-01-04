use fastrender::paint::display_list::{
  BlendMode, BorderRadii, DisplayItem, DisplayList, FillRectItem, OpacityItem, StackingContextItem,
};
use fastrender::paint::display_list_renderer::DisplayListRenderer;
use fastrender::style::color::Rgba;
use fastrender::style::types::{BackfaceVisibility, TransformStyle};
use fastrender::text::font_loader::FontContext;
use fastrender::Rect;

fn stacking_context(bounds: Rect, transform_style: TransformStyle) -> StackingContextItem {
  StackingContextItem {
    z_index: 0,
    creates_stacking_context: true,
    bounds,
    plane_rect: bounds,
    mix_blend_mode: BlendMode::Normal,
    is_isolated: false,
    transform: None,
    child_perspective: None,
    transform_style,
    backface_visibility: BackfaceVisibility::Visible,
    filters: Vec::new(),
    backdrop_filters: Vec::new(),
    radii: BorderRadii::ZERO,
    mask: None,
  }
}

#[test]
fn preserve_3d_opacity_scope_wraps_child_stacking_context() {
  let bounds = Rect::from_xywh(0.0, 0.0, 10.0, 10.0);
  let mut list = DisplayList::new();
  list.push(DisplayItem::PushStackingContext(stacking_context(
    bounds,
    TransformStyle::Preserve3d,
  )));

  list.push(DisplayItem::PushOpacity(OpacityItem { opacity: 0.5 }));
  list.push(DisplayItem::PushStackingContext(stacking_context(
    bounds,
    TransformStyle::Flat,
  )));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: bounds,
    color: Rgba::BLACK,
  }));
  list.push(DisplayItem::PopStackingContext);
  list.push(DisplayItem::PopOpacity);
  list.push(DisplayItem::PopStackingContext);

  let pixmap = DisplayListRenderer::new(10, 10, Rgba::WHITE, FontContext::new())
    .unwrap()
    .render(&list)
    .unwrap();

  let pixel = pixmap.pixel(5, 5).expect("pixel in-bounds");
  let expected = 128i16;
  let tolerance = 5i16;
  assert!(
    (pixel.red() as i16 - expected).abs() <= tolerance
      && (pixel.green() as i16 - expected).abs() <= tolerance
      && (pixel.blue() as i16 - expected).abs() <= tolerance,
    "expected roughly gray pixel, got rgba({}, {}, {}, {})",
    pixel.red(),
    pixel.green(),
    pixel.blue(),
    pixel.alpha()
  );
  assert_eq!(pixel.alpha(), 255);
}

