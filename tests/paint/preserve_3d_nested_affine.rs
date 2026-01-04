use super::util::bounding_box_for_color;
use fastrender::paint::display_list::{
  BlendMode, BorderRadii, DisplayItem, DisplayList, FillRectItem, StackingContextItem, Transform3D,
};
use fastrender::paint::display_list_renderer::DisplayListRenderer;
use fastrender::style::color::Rgba;
use fastrender::style::types::{BackfaceVisibility, TransformStyle};
use fastrender::text::font_loader::FontContext;
use fastrender::Rect;

fn stacking_context(
  bounds: Rect,
  plane_rect: Rect,
  transform: Option<Transform3D>,
  child_perspective: Option<Transform3D>,
  transform_style: TransformStyle,
) -> StackingContextItem {
  StackingContextItem {
    z_index: 0,
    creates_stacking_context: true,
    bounds,
    plane_rect,
    mix_blend_mode: BlendMode::Normal,
    is_isolated: false,
    transform,
    child_perspective,
    transform_style,
    backface_visibility: BackfaceVisibility::Visible,
    filters: Vec::new(),
    backdrop_filters: Vec::new(),
    radii: BorderRadii::ZERO,
    mask: None,
  }
}

#[test]
fn preserve_3d_nested_in_affine_ancestor_does_not_double_apply_transform() {
  let viewport = Rect::from_xywh(0.0, 0.0, 300.0, 250.0);
  let rect = Rect::from_xywh(10.0, 10.0, 20.0, 20.0);

  let affine = Transform3D::translate(30.0, 20.0, 0.0).multiply(&Transform3D::scale(2.0, 2.0, 1.0));

  let mut list = DisplayList::new();
  list.push(DisplayItem::PushStackingContext(stacking_context(
    viewport,
    viewport,
    Some(affine),
    None,
    TransformStyle::Flat,
  )));
  list.push(DisplayItem::PushStackingContext(stacking_context(
    viewport,
    viewport,
    None,
    None,
    TransformStyle::Preserve3d,
  )));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect,
    color: Rgba::RED,
  }));
  list.push(DisplayItem::PopStackingContext);
  list.push(DisplayItem::PopStackingContext);

  let pixmap = DisplayListRenderer::new(
    viewport.width() as u32,
    viewport.height() as u32,
    Rgba::WHITE,
    FontContext::new(),
  )
  .unwrap()
  .render(&list)
  .unwrap();

  let painted =
    bounding_box_for_color(&pixmap, |(r, g, b, a)| a > 0 && r > 200 && g < 80 && b < 80)
      .expect("rect should be painted");

  let corners = [
    (rect.min_x(), rect.min_y()),
    (rect.max_x(), rect.min_y()),
    (rect.max_x(), rect.max_y()),
    (rect.min_x(), rect.max_y()),
  ];
  let (mut min_x, mut min_y, mut max_x, mut max_y) = (
    f32::INFINITY,
    f32::INFINITY,
    f32::NEG_INFINITY,
    f32::NEG_INFINITY,
  );
  for (x, y) in corners {
    let (tx, ty, _tz, tw) = affine.transform_point(x, y, 0.0);
    let px = tx / tw;
    let py = ty / tw;
    min_x = min_x.min(px);
    min_y = min_y.min(py);
    max_x = max_x.max(px);
    max_y = max_y.max(py);
  }

  let clamp = |v: i32, max: i32| v.clamp(0, max);
  let expected = (
    clamp(min_x.floor() as i32, pixmap.width() as i32 - 1),
    clamp(min_y.floor() as i32, pixmap.height() as i32 - 1),
    clamp(max_x.ceil() as i32, pixmap.width() as i32 - 1),
    clamp(max_y.ceil() as i32, pixmap.height() as i32 - 1),
  );

  let tolerance = 1;
  assert!(
    (painted.0 as i32 - expected.0).abs() <= tolerance
      && (painted.1 as i32 - expected.1).abs() <= tolerance
      && (painted.2 as i32 - expected.2).abs() <= tolerance
      && (painted.3 as i32 - expected.3).abs() <= tolerance,
    "preserve-3d subtree should inherit the flat ancestor affine transform only once (expected {:?}, got {:?})",
    expected,
    painted
  );
}
