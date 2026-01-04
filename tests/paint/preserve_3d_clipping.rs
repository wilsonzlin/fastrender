use fastrender::paint::display_list::{
  BlendMode, BorderRadii, ClipItem, ClipShape, DisplayItem, DisplayList, FillRectItem,
  StackingContextItem,
};
use fastrender::paint::display_list_renderer::DisplayListRenderer;
use fastrender::style::color::Rgba;
use fastrender::style::types::{BackfaceVisibility, TransformStyle};
use fastrender::text::font_loader::FontContext;
use fastrender::Rect;

fn pixel(pixmap: &tiny_skia::Pixmap, x: u32, y: u32) -> (u8, u8, u8, u8) {
  let px = pixmap.pixel(x, y).expect("pixel inside viewport");
  (px.red(), px.green(), px.blue(), px.alpha())
}

fn ctx(bounds: Rect, transform_style: TransformStyle) -> StackingContextItem {
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
fn preserve_3d_clip_scope_clips_child_plane() {
  let root_bounds = Rect::from_xywh(0.0, 0.0, 40.0, 40.0);
  let clip_rect = Rect::from_xywh(0.0, 0.0, 20.0, 40.0);

  let mut list = DisplayList::new();
  list.push(DisplayItem::PushStackingContext(ctx(
    root_bounds,
    TransformStyle::Preserve3d,
  )));

  list.push(DisplayItem::PushClip(ClipItem {
    shape: ClipShape::Rect {
      rect: clip_rect,
      radii: None,
    },
  }));

  list.push(DisplayItem::PushStackingContext(ctx(
    root_bounds,
    TransformStyle::Flat,
  )));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: root_bounds,
    color: Rgba::RED,
  }));
  list.push(DisplayItem::PopStackingContext);

  list.push(DisplayItem::PopClip);
  list.push(DisplayItem::PopStackingContext);

  let pixmap = DisplayListRenderer::new(40, 40, Rgba::WHITE, FontContext::new())
    .unwrap()
    .render(&list)
    .unwrap();

  assert_eq!(pixel(&pixmap, 30, 10), (255, 255, 255, 255));
  assert_eq!(pixel(&pixmap, 10, 10), (255, 0, 0, 255));
}

#[test]
fn preserve_3d_clip_scope_respects_border_radii() {
  let root_bounds = Rect::from_xywh(0.0, 0.0, 40.0, 40.0);
  let clip_rect = Rect::from_xywh(0.0, 0.0, 20.0, 20.0);

  let mut list = DisplayList::new();
  list.push(DisplayItem::PushStackingContext(ctx(
    root_bounds,
    TransformStyle::Preserve3d,
  )));

  list.push(DisplayItem::PushClip(ClipItem {
    shape: ClipShape::Rect {
      rect: clip_rect,
      radii: Some(BorderRadii::uniform(8.0)),
    },
  }));

  list.push(DisplayItem::PushStackingContext(ctx(
    root_bounds,
    TransformStyle::Flat,
  )));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: root_bounds,
    color: Rgba::RED,
  }));
  list.push(DisplayItem::PopStackingContext);

  list.push(DisplayItem::PopClip);
  list.push(DisplayItem::PopStackingContext);

  let pixmap = DisplayListRenderer::new(40, 40, Rgba::WHITE, FontContext::new())
    .unwrap()
    .render(&list)
    .unwrap();

  assert_ne!(pixel(&pixmap, 1, 1), (255, 0, 0, 255));
  assert_eq!(pixel(&pixmap, 10, 10), (255, 0, 0, 255));
  assert_eq!(pixel(&pixmap, 30, 10), (255, 255, 255, 255));
}
