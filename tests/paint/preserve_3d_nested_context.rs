use super::util::bounding_box_for_color;
use fastrender::paint::display_list::{
  BlendMode, BorderRadii, DisplayItem, DisplayList, FillRectItem, StackingContextItem, Transform3D,
};
use fastrender::paint::display_list_renderer::DisplayListRenderer;
use fastrender::style::color::Rgba;
use fastrender::style::types::{BackfaceVisibility, TransformStyle};
use fastrender::text::font_loader::FontContext;
use fastrender::Rect;

fn context(bounds: Rect, transform_style: TransformStyle) -> StackingContextItem {
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
fn nested_preserve_3d_context_is_preserved_inside_flattened_subtree() {
  let plane = Rect::from_xywh(20.0, 20.0, 60.0, 40.0);
  let root_bounds = Rect::from_xywh(0.0, 0.0, 120.0, 100.0);
  let perspective = Transform3D::perspective(200.0);
  let center = (
    plane.x() + plane.width() * 0.5,
    plane.y() + plane.height() * 0.5,
  );
  // Compose the plane transform across multiple preserve-3d nodes. When the inner preserve-3d
  // context is rendered as `transform-style: flat` (the buggy behaviour), the inner rotation is
  // flattened into a 2D approximation and loses its perspective warp.
  let outer_rotate = Transform3D::translate(center.0, center.1, 0.0)
    .multiply(&Transform3D::rotate_y(20_f32.to_radians()))
    .multiply(&Transform3D::translate(-center.0, -center.1, 0.0));
  let inner_rotate = Transform3D::translate(center.0, center.1, 0.0)
    .multiply(&Transform3D::rotate_y(60_f32.to_radians()))
    .multiply(&Transform3D::translate(-center.0, -center.1, 0.0));

  let mut list = DisplayList::new();
  // Outer preserve-3d context, which will flatten its transform-style:flat child into a plane.
  list.push(DisplayItem::PushStackingContext(context(
    root_bounds,
    TransformStyle::Preserve3d,
  )));

  // Flat child forces the outer preserve-3d scene renderer to treat its subtree as a single
  // `FlattenedSubtree` item.
  list.push(DisplayItem::PushStackingContext(context(root_bounds, TransformStyle::Flat)));

  // Nested preserve-3d context inside the flattened subtree. This should still render its
  // own children in 3D before being flattened into the outer plane.
  let mut inner_root = context(root_bounds, TransformStyle::Preserve3d);
  inner_root.child_perspective = Some(perspective);
  list.push(DisplayItem::PushStackingContext(inner_root));

  let mut outer = context(plane, TransformStyle::Preserve3d);
  outer.transform = Some(outer_rotate);
  list.push(DisplayItem::PushStackingContext(outer));

  let mut rotated = context(plane, TransformStyle::Preserve3d);
  rotated.transform = Some(inner_rotate);
  list.push(DisplayItem::PushStackingContext(rotated));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: plane,
    color: Rgba::RED,
  }));
  list.push(DisplayItem::PopStackingContext);
  list.push(DisplayItem::PopStackingContext);
  list.push(DisplayItem::PopStackingContext);
  list.push(DisplayItem::PopStackingContext);
  list.push(DisplayItem::PopStackingContext);

  let pixmap = DisplayListRenderer::new(
    root_bounds.width() as u32,
    root_bounds.height() as u32,
    Rgba::WHITE,
    FontContext::new(),
  )
  .unwrap()
  .render(&list)
  .unwrap();

  let adjusted = perspective
    .multiply(&outer_rotate)
    .multiply(&inner_rotate)
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
      .expect("nested plane should be painted after warping");

  let tolerance = 2;
  assert!(
    (painted.0 as i32 - expected.0).abs() <= tolerance
      && (painted.1 as i32 - expected.1).abs() <= tolerance
      && (painted.2 as i32 - expected.2).abs() <= tolerance
      && (painted.3 as i32 - expected.3).abs() <= tolerance,
    "nested preserve-3d warp should follow the perspective-projected bounds (expected {:?}, got {:?})",
    expected,
    painted
  );
}
