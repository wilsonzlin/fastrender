use fastrender::css::types::{RotateValue, ScaleValue, TranslateValue};
use fastrender::geometry::Rect;
use fastrender::paint::transform_resolver::resolve_transform3d;
use fastrender::{ComputedStyle, Length, Transform};

fn assert_close(actual: f32, expected: f32) {
  let diff = (actual - expected).abs();
  assert!(
    diff < 1e-3,
    "expected {expected:.3}, got {actual:.3} (diff {diff:.3})"
  );
}

#[test]
fn translate_property_contributes_to_transform_matrix() {
  let mut style = ComputedStyle::default();
  style.translate = TranslateValue::Values {
    x: Length::px(10.0),
    y: Length::px(20.0),
    z: Length::px(0.0),
  };

  let bounds = Rect::from_xywh(100.0, 100.0, 50.0, 50.0);
  let transform = resolve_transform3d(&style, bounds, Some((800.0, 600.0)))
    .expect("translate should produce a transform matrix");
  let p = transform
    .project_point_2d(100.0, 100.0)
    .expect("project point");

  assert_close(p.x, 110.0);
  assert_close(p.y, 120.0);
}

#[test]
fn rotate_property_contributes_to_transform_matrix() {
  let mut style = ComputedStyle::default();
  // Rotate around the top-left corner so expectations are straightforward.
  style.transform_origin.x = Length::px(0.0);
  style.transform_origin.y = Length::px(0.0);
  style.rotate = RotateValue::Angle(90.0);

  let bounds = Rect::from_xywh(50.0, 50.0, 40.0, 20.0);
  let transform =
    resolve_transform3d(&style, bounds, Some((800.0, 600.0))).expect("rotate should apply");

  // Point on the top-right corner rotates to the bottom-left (clockwise 90deg).
  let p = transform
    .project_point_2d(90.0, 50.0)
    .expect("project point");
  assert_close(p.x, 50.0);
  assert_close(p.y, 90.0);
}

#[test]
fn scale_property_contributes_to_transform_matrix() {
  let mut style = ComputedStyle::default();
  style.transform_origin.x = Length::px(0.0);
  style.transform_origin.y = Length::px(0.0);
  style.scale = ScaleValue::Values {
    x: 2.0,
    y: 3.0,
    z: 1.0,
  };

  let bounds = Rect::from_xywh(10.0, 10.0, 20.0, 10.0);
  let transform =
    resolve_transform3d(&style, bounds, Some((800.0, 600.0))).expect("scale should apply");

  let p = transform
    .project_point_2d(30.0, 20.0)
    .expect("project point");
  assert_close(p.x, 50.0);
  assert_close(p.y, 40.0);
}

#[test]
fn individual_transforms_compose_before_transform_list() {
  let mut style = ComputedStyle::default();
  style.transform_origin.x = Length::px(0.0);
  style.transform_origin.y = Length::px(0.0);
  style.translate = TranslateValue::Values {
    x: Length::px(10.0),
    y: Length::px(0.0),
    z: Length::px(0.0),
  };
  style.transform.push(Transform::Scale(2.0, 2.0));

  let bounds = Rect::from_xywh(0.0, 0.0, 10.0, 10.0);
  let transform =
    resolve_transform3d(&style, bounds, Some((800.0, 600.0))).expect("transform should apply");

  // translate is applied after the transform list, so it should not be scaled.
  let p = transform.project_point_2d(0.0, 0.0).expect("project point");
  assert_close(p.x, 10.0);
  assert_close(p.y, 0.0);
}
