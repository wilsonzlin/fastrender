use fastrender::paint::display_list::{
  BlendMode, BorderRadii, ClipItem, ClipShape, DisplayItem, DisplayList, FillRectItem,
  StackingContextItem,
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
fn preserve_3d_clip_scope_wraps_child_stacking_context() {
  let bounds = Rect::from_xywh(0.0, 0.0, 10.0, 10.0);
  let clip_rect = Rect::from_xywh(0.0, 0.0, 5.0, 10.0);

  let mut list = DisplayList::new();
  list.push(DisplayItem::PushStackingContext(stacking_context(
    bounds,
    TransformStyle::Preserve3d,
  )));

  list.push(DisplayItem::PushClip(ClipItem {
    shape: ClipShape::Rect {
      rect: clip_rect,
      radii: None,
    },
  }));
  list.push(DisplayItem::PushStackingContext(stacking_context(
    bounds,
    TransformStyle::Flat,
  )));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: bounds,
    color: Rgba::BLACK,
  }));
  list.push(DisplayItem::PopStackingContext);
  list.push(DisplayItem::PopClip);
  list.push(DisplayItem::PopStackingContext);

  let pixmap = DisplayListRenderer::new(10, 10, Rgba::WHITE, FontContext::new())
    .unwrap()
    .render(&list)
    .unwrap();

  let inside = pixmap.pixel(2, 5).expect("pixel in-bounds");
  assert_eq!(
    (inside.red(), inside.green(), inside.blue(), inside.alpha()),
    (0, 0, 0, 255),
    "expected black pixel inside clip scope, got rgba({}, {}, {}, {})",
    inside.red(),
    inside.green(),
    inside.blue(),
    inside.alpha()
  );

  let outside = pixmap.pixel(7, 5).expect("pixel in-bounds");
  assert_eq!(
    (
      outside.red(),
      outside.green(),
      outside.blue(),
      outside.alpha()
    ),
    (255, 255, 255, 255),
    "expected background pixel outside clip scope, got rgba({}, {}, {}, {})",
    outside.red(),
    outside.green(),
    outside.blue(),
    outside.alpha()
  );
}
