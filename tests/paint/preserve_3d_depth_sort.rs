use super::util::bounding_box_for_color;
use fastrender::paint::display_list::{
  quad_from_transform3d, BlendMode, BorderRadii, DisplayItem, DisplayList, FillRectItem,
  StackingContextItem, Transform3D,
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
    transform_style: TransformStyle::Preserve3d,
    backface_visibility: BackfaceVisibility::Visible,
    filters: Vec::new(),
    backdrop_filters: Vec::new(),
    radii: BorderRadii::ZERO,
    mask: None,
  }
}

#[test]
fn preserve_3d_depth_sort_respects_overlap_depth() {
  let tilted_rect = Rect::from_xywh(10.0, 10.0, 40.0, 20.0);
  let front_rect = Rect::from_xywh(5.0, 10.0, 20.0, 20.0);

  let tilt = Transform3D::translate(0.0, 0.0, 5.0)
    .multiply(&Transform3D::translate(30.0, 20.0, 0.0))
    .multiply(&Transform3D::rotate_y(60_f32.to_radians()))
    .multiply(&Transform3D::translate(-30.0, -20.0, 0.0));

  let mut list = DisplayList::new();
  list.push(DisplayItem::PushStackingContext(stacking_context(
    Rect::from_xywh(0.0, 0.0, 80.0, 50.0),
    Rect::from_xywh(0.0, 0.0, 80.0, 50.0),
    None,
    None,
  )));

  list.push(DisplayItem::PushStackingContext(stacking_context(
    tilted_rect,
    tilted_rect,
    Some(tilt),
    None,
  )));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: tilted_rect,
    color: Rgba::BLUE,
  }));
  list.push(DisplayItem::PopStackingContext);

  list.push(DisplayItem::PushStackingContext(stacking_context(
    front_rect,
    front_rect,
    Some(Transform3D::translate(0.0, 0.0, 10.0)),
    None,
  )));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: front_rect,
    color: Rgba::GREEN,
  }));
  list.push(DisplayItem::PopStackingContext);

  list.push(DisplayItem::PopStackingContext);

  let pixmap = DisplayListRenderer::new(80, 50, Rgba::WHITE, FontContext::new())
    .unwrap()
    .render(&list)
    .unwrap();

  let overlap = pixmap.pixel(22, 20).expect("overlap pixel");
  assert!(
    overlap.blue() > overlap.green(),
    "tilted plane should be painted above at the overlapping depth"
  );
  assert_eq!(overlap.alpha(), 255);
}

#[test]
fn perspective_warp_is_enabled_by_default() {
  let plane = Rect::from_xywh(10.0, 10.0, 40.0, 40.0);
  let perspective = Transform3D::perspective(50.0);
  let center = (
    plane.x() + plane.width() * 0.5,
    plane.y() + plane.height() * 0.5,
  );
  let rotate = Transform3D::translate(center.0, center.1, 0.0)
    .multiply(&Transform3D::rotate_x(60_f32.to_radians()))
    .multiply(&Transform3D::translate(-center.0, -center.1, 0.0));

  let mut list = DisplayList::new();
  list.push(DisplayItem::PushStackingContext(stacking_context(
    Rect::from_xywh(0.0, 0.0, 120.0, 120.0),
    Rect::from_xywh(0.0, 0.0, 120.0, 120.0),
    None,
    Some(perspective),
  )));
  list.push(DisplayItem::PushStackingContext(stacking_context(
    plane,
    plane,
    Some(rotate),
    None,
  )));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: plane,
    color: Rgba::RED,
  }));
  list.push(DisplayItem::PopStackingContext);
  list.push(DisplayItem::PopStackingContext);

  let renderer = DisplayListRenderer::new(120, 120, Rgba::WHITE, FontContext::new()).unwrap();
  let pixmap = renderer.render(&list).unwrap();

  let combined = perspective.multiply(&rotate);
  let quad = quad_from_transform3d(plane, &combined).expect("projection should be valid");
  let (mut min_x, mut min_y, mut max_x, mut max_y) = (
    f32::INFINITY,
    f32::INFINITY,
    f32::NEG_INFINITY,
    f32::NEG_INFINITY,
  );
  for point in quad {
    min_x = min_x.min(point.x);
    min_y = min_y.min(point.y);
    max_x = max_x.max(point.x);
    max_y = max_y.max(point.y);
  }

  let clamp = |v: i32, max: i32| v.clamp(0, max);
  let expected = (
    clamp(min_x.floor() as i32, pixmap.width() as i32 - 1),
    clamp(min_y.floor() as i32, pixmap.height() as i32 - 1),
    clamp(max_x.ceil() as i32, pixmap.width() as i32 - 1),
    clamp(max_y.ceil() as i32, pixmap.height() as i32 - 1),
  );

  let painted = bounding_box_for_color(
    &pixmap,
    |(r, g, b, a)| a > 0 && r > 200 && g < 80 && b < 80,
  )
  .expect("quad should be visible");

  let expected_size = (expected.2 - expected.0 + 1, expected.3 - expected.1 + 1);
  let painted_size = (
    painted.2 as i32 - painted.0 as i32 + 1,
    painted.3 as i32 - painted.1 as i32 + 1,
  );

  assert!(
    (painted.0 as i32 - expected.0).abs() <= 3
      && (painted.1 as i32 - expected.1).abs() <= 3
      && (painted.2 as i32 - expected.2).abs() <= 3
      && (painted.3 as i32 - expected.3).abs() <= 3,
    "projective warp should follow the perspective projection bounds (expected {:?}, got {:?})",
    expected,
    painted
  );
  assert!(
    (painted_size.0 - expected_size.0).abs() <= 4 && (painted_size.1 - expected_size.1).abs() <= 4,
    "projective warp should preserve the projected trapezoid size (expected {:?}, got {:?})",
    expected_size,
    painted_size
  );
}
