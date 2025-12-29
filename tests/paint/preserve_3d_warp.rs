use super::util::bounding_box_for_color;
use fastrender::paint::display_list::{
  BlendMode, BorderRadii, DisplayItem, DisplayList, FillRectItem, StackingContextItem, Transform3D,
};
use fastrender::paint::display_list_renderer::DisplayListRenderer;
use fastrender::style::color::Rgba;
use fastrender::style::types::{BackfaceVisibility, TransformStyle};
use fastrender::text::font_loader::FontContext;
use fastrender::Rect;

#[test]
fn perspective_plane_projects_with_warp() {
  let plane = Rect::from_xywh(20.0, 20.0, 60.0, 40.0);
  let root_bounds = Rect::from_xywh(0.0, 0.0, 120.0, 100.0);
  let perspective = Transform3D::perspective(200.0);
  let center = (
    plane.x() + plane.width() * 0.5,
    plane.y() + plane.height() * 0.5,
  );
  let rotate = Transform3D::translate(center.0, center.1, 0.0)
    .multiply(&Transform3D::rotate_y(70_f32.to_radians()))
    .multiply(&Transform3D::translate(-center.0, -center.1, 0.0));

  let mut list = DisplayList::new();
  list.push(DisplayItem::PushStackingContext(StackingContextItem {
    z_index: 0,
    creates_stacking_context: true,
    bounds: root_bounds,
    plane_rect: root_bounds,
    mix_blend_mode: BlendMode::Normal,
    is_isolated: false,
    transform: None,
    child_perspective: Some(perspective),
    transform_style: TransformStyle::Preserve3d,
    backface_visibility: BackfaceVisibility::Visible,
    filters: Vec::new(),
    backdrop_filters: Vec::new(),
    radii: BorderRadii::ZERO,
    mask: None,
  }));
  list.push(DisplayItem::PushStackingContext(StackingContextItem {
    z_index: 1,
    creates_stacking_context: true,
    bounds: plane,
    plane_rect: plane,
    mix_blend_mode: BlendMode::Normal,
    is_isolated: false,
    transform: Some(rotate),
    child_perspective: None,
    transform_style: TransformStyle::Preserve3d,
    backface_visibility: BackfaceVisibility::Visible,
    filters: Vec::new(),
    backdrop_filters: Vec::new(),
    radii: BorderRadii::ZERO,
    mask: None,
  }));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: plane,
    color: Rgba::RED,
  }));
  list.push(DisplayItem::PopStackingContext);
  list.push(DisplayItem::PopStackingContext);

  let renderer = DisplayListRenderer::new(
    root_bounds.width() as u32,
    root_bounds.height() as u32,
    Rgba::WHITE,
    FontContext::new(),
  )
  .unwrap();
  let pixmap = renderer.render(&list).unwrap();

  let adjusted = perspective
    .multiply(&rotate)
    .multiply(&Transform3D::translate(plane.x(), plane.y(), 0.0));
  let corners = [
    (0.0, 0.0),
    (plane.width(), 0.0),
    (plane.width(), plane.height()),
    (0.0, plane.height()),
  ];
  let (mut min_x, mut min_y, mut max_x, mut max_y) = (
    f32::INFINITY,
    f32::INFINITY,
    f32::NEG_INFINITY,
    f32::NEG_INFINITY,
  );
  for (x, y) in corners {
    let (tx, ty, _tz, tw) = adjusted.transform_point(x, y, 0.0);
    let px = tx / tw;
    let py = ty / tw;
    min_x = min_x.min(px);
    min_y = min_y.min(py);
    max_x = max_x.max(px);
    max_y = max_y.max(py);
  }

  let expected = (
    min_x.floor() as i32,
    min_y.floor() as i32,
    max_x.ceil() as i32,
    max_y.ceil() as i32,
  );
  let clamp = |v: i32, max: i32| v.clamp(0, max);
  let expected = (
    clamp(expected.0, pixmap.width() as i32 - 1),
    clamp(expected.1, pixmap.height() as i32 - 1),
    clamp(expected.2, pixmap.width() as i32 - 1),
    clamp(expected.3, pixmap.height() as i32 - 1),
  );

  let painted =
    bounding_box_for_color(&pixmap, |(r, g, b, a)| a > 0 && r > 200 && g < 50 && b < 50)
      .expect("plane should be painted after warping");

  let tolerance = 2;
  assert!(
    (painted.0 as i32 - expected.0).abs() <= tolerance
      && (painted.1 as i32 - expected.1).abs() <= tolerance
      && (painted.2 as i32 - expected.2).abs() <= tolerance
      && (painted.3 as i32 - expected.3).abs() <= tolerance,
    "projective warp should follow the perspective-projected bounds (expected {:?}, got {:?})",
    expected,
    painted
  );
}
