use crate::geometry::Point;
use crate::geometry::Rect;
use crate::paint::display_list::Transform2D;
use crate::paint::display_list::Transform3D;
use crate::style::types::TransformBox;
use crate::style::values::Length;
use crate::style::ComputedStyle;

#[derive(Clone, Copy)]
struct BackgroundRects {
  border: Rect,
  content: Rect,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct ResolvedTransforms {
  pub self_transform: Option<Transform3D>,
  pub child_perspective: Option<Transform3D>,
}

/// Resolve the computed transform (including motion path) into a 4×4 matrix.
///
/// Mirrors `DisplayListBuilder::build_transform` so painter and display list pipelines stay in
/// lockstep.
pub fn resolve_transform3d(
  style: &ComputedStyle,
  bounds: Rect,
  viewport: Option<(f32, f32)>,
) -> Option<Transform3D> {
  resolve_transforms(style, bounds, viewport).self_transform
}

pub fn resolve_transforms(
  style: &ComputedStyle,
  bounds: Rect,
  viewport: Option<(f32, f32)>,
) -> ResolvedTransforms {
  let reference = transform_reference_box(style, bounds, viewport);
  let percentage_width = reference.width();
  let percentage_height = reference.height();

  let resolved_perspective = style.perspective.as_ref().and_then(|len| {
    len
      .resolve_with_context(
        Some(percentage_width),
        viewport.map(|v| v.0).unwrap_or(0.0),
        viewport.map(|v| v.1).unwrap_or(0.0),
        style.font_size,
        style.root_font_size,
      )
      .filter(|v| *v > 0.0)
  });

  let mut transform_from_list: Option<Transform3D> = None;

  if !style.transform.is_empty() {
    let mut ts = Transform3D::identity();
    const EPS: f32 = 1e-6;
    for component in &style.transform {
      let next = match component {
        crate::css::types::Transform::Translate(x, y) => {
          let tx =
            resolve_transform_length(x, style.font_size, style.root_font_size, percentage_width);
          let ty =
            resolve_transform_length(y, style.font_size, style.root_font_size, percentage_height);
          Transform3D::translate(tx, ty, 0.0)
        }
        crate::css::types::Transform::TranslateX(x) => {
          let tx =
            resolve_transform_length(x, style.font_size, style.root_font_size, percentage_width);
          Transform3D::translate(tx, 0.0, 0.0)
        }
        crate::css::types::Transform::TranslateY(y) => {
          let ty =
            resolve_transform_length(y, style.font_size, style.root_font_size, percentage_height);
          Transform3D::translate(0.0, ty, 0.0)
        }
        crate::css::types::Transform::TranslateZ(z) => Transform3D::translate(
          0.0,
          0.0,
          resolve_transform_length(z, style.font_size, style.root_font_size, percentage_width),
        ),
        crate::css::types::Transform::Translate3d(x, y, z) => {
          let tx =
            resolve_transform_length(x, style.font_size, style.root_font_size, percentage_width);
          let ty =
            resolve_transform_length(y, style.font_size, style.root_font_size, percentage_height);
          let tz =
            resolve_transform_length(z, style.font_size, style.root_font_size, percentage_width);
          Transform3D::translate(tx, ty, tz)
        }
        crate::css::types::Transform::Scale(sx, sy) => Transform3D::scale(*sx, *sy, 1.0),
        crate::css::types::Transform::ScaleX(sx) => Transform3D::scale(*sx, 1.0, 1.0),
        crate::css::types::Transform::ScaleY(sy) => Transform3D::scale(1.0, *sy, 1.0),
        crate::css::types::Transform::ScaleZ(sz) => Transform3D::scale(1.0, 1.0, *sz),
        crate::css::types::Transform::Scale3d(sx, sy, sz) => Transform3D::scale(*sx, *sy, *sz),
        crate::css::types::Transform::Rotate(deg) | crate::css::types::Transform::RotateZ(deg) => {
          Transform3D::rotate_z(deg.to_radians())
        }
        crate::css::types::Transform::RotateX(deg) => Transform3D::rotate_x(deg.to_radians()),
        crate::css::types::Transform::RotateY(deg) => Transform3D::rotate_y(deg.to_radians()),
        crate::css::types::Transform::Rotate3d(x, y, z, deg) => {
          let len = (x * x + y * y + z * z).sqrt();
          if len < EPS {
            Transform3D::identity()
          } else {
            let ax = *x / len;
            let ay = *y / len;
            let az = *z / len;
            let angle = deg.to_radians();
            let (s, c) = angle.sin_cos();
            let t = 1.0 - c;

            let m00 = t * ax * ax + c;
            let m01 = t * ax * ay + s * az;
            let m02 = t * ax * az - s * ay;
            let m10 = t * ax * ay - s * az;
            let m11 = t * ay * ay + c;
            let m12 = t * ay * az + s * ax;
            let m20 = t * ax * az + s * ay;
            let m21 = t * ay * az - s * ax;
            let m22 = t * az * az + c;

            Transform3D {
              m: [
                m00, m10, m20, 0.0, // column 1
                m01, m11, m21, 0.0, // column 2
                m02, m12, m22, 0.0, // column 3
                0.0, 0.0, 0.0, 1.0, // column 4
              ],
            }
          }
        }
        crate::css::types::Transform::SkewX(deg) => Transform3D::skew(deg.to_radians(), 0.0),
        crate::css::types::Transform::SkewY(deg) => Transform3D::skew(0.0, deg.to_radians()),
        crate::css::types::Transform::Skew(ax, ay) => {
          Transform3D::skew(ax.to_radians(), ay.to_radians())
        }
        crate::css::types::Transform::Perspective(len) => Transform3D::perspective(
          resolve_transform_length(len, style.font_size, style.root_font_size, percentage_width),
        ),
        crate::css::types::Transform::Matrix(a, b, c, d, e, f) => {
          Transform3D::from_2d(&Transform2D {
            a: *a,
            b: *b,
            c: *c,
            d: *d,
            e: *e,
            f: *f,
          })
        }
        crate::css::types::Transform::Matrix3d(values) => Transform3D { m: *values },
      };
      ts = ts.multiply(&next);
    }

    let origin_x = resolve_transform_length(
      &style.transform_origin.x,
      style.font_size,
      style.root_font_size,
      percentage_width,
    );
    let origin_y = resolve_transform_length(
      &style.transform_origin.y,
      style.font_size,
      style.root_font_size,
      percentage_height,
    );
    let origin = Point::new(reference.x() + origin_x, reference.y() + origin_y);

    let matrix = Transform3D::translate(origin.x, origin.y, 0.0)
      .multiply(&ts)
      .multiply(&Transform3D::translate(-origin.x, -origin.y, 0.0));

    transform_from_list = Some(matrix);
  }

  let motion = crate::paint::motion_path::compute_motion_transform(style, bounds, viewport)
    .map(|t| Transform3D::from_2d(&t));

  let child_perspective = resolved_perspective.map(|perspective| {
    let po_x = resolve_transform_length(
      &style.perspective_origin.x,
      style.font_size,
      style.root_font_size,
      percentage_width,
    );
    let po_y = resolve_transform_length(
      &style.perspective_origin.y,
      style.font_size,
      style.root_font_size,
      percentage_height,
    );
    let perspective_origin = Point::new(reference.x() + po_x, reference.y() + po_y);
    Transform3D::translate(perspective_origin.x, perspective_origin.y, 0.0)
      .multiply(&Transform3D::perspective(perspective))
      .multiply(&Transform3D::translate(
        -perspective_origin.x,
        -perspective_origin.y,
        0.0,
      ))
  });

  let self_transform = match (motion, transform_from_list) {
    (None, None) => None,
    (Some(motion), None) => Some(motion),
    (None, Some(matrix)) => Some(matrix),
    (Some(motion), Some(matrix)) => Some(matrix.multiply(&motion)),
  };

  ResolvedTransforms {
    self_transform,
    child_perspective,
  }
}

/// Shared backface visibility test for 3D transforms.
pub fn backface_is_hidden(transform: &Transform3D) -> bool {
  let Some(p0) = transform.project_point(0.0, 0.0, 0.0) else {
    return false;
  };
  let Some(p1) = transform.project_point(1.0, 0.0, 0.0) else {
    return false;
  };
  let Some(p2) = transform.project_point(0.0, 1.0, 0.0) else {
    return false;
  };
  let ux = [p1[0] - p0[0], p1[1] - p0[1], p1[2] - p0[2]];
  let uy = [p2[0] - p0[0], p2[1] - p0[1], p2[2] - p0[2]];

  let normal = [
    ux[1] * uy[2] - ux[2] * uy[1],
    ux[2] * uy[0] - ux[0] * uy[2],
    ux[0] * uy[1] - ux[1] * uy[0],
  ];

  normal[2] < 0.0
}

fn transform_reference_box(
  style: &ComputedStyle,
  bounds: Rect,
  viewport: Option<(f32, f32)>,
) -> Rect {
  let rects = background_rects(bounds, style, viewport);
  match style.transform_box {
    TransformBox::ContentBox => rects.content,
    TransformBox::BorderBox
    | TransformBox::FillBox
    | TransformBox::StrokeBox
    | TransformBox::ViewBox => rects.border,
  }
}

fn background_rects(
  rect: Rect,
  style: &ComputedStyle,
  viewport: Option<(f32, f32)>,
) -> BackgroundRects {
  let font_size = style.font_size;
  let base = rect.width().max(0.0);

  let border_left = resolve_length_for_paint(
    &style.border_left_width,
    font_size,
    style.root_font_size,
    base,
    viewport,
  );
  let border_right = resolve_length_for_paint(
    &style.border_right_width,
    font_size,
    style.root_font_size,
    base,
    viewport,
  );
  let border_top = resolve_length_for_paint(
    &style.border_top_width,
    font_size,
    style.root_font_size,
    base,
    viewport,
  );
  let border_bottom = resolve_length_for_paint(
    &style.border_bottom_width,
    font_size,
    style.root_font_size,
    base,
    viewport,
  );

  let padding_left = resolve_length_for_paint(
    &style.padding_left,
    font_size,
    style.root_font_size,
    base,
    viewport,
  );
  let padding_right = resolve_length_for_paint(
    &style.padding_right,
    font_size,
    style.root_font_size,
    base,
    viewport,
  );
  let padding_top = resolve_length_for_paint(
    &style.padding_top,
    font_size,
    style.root_font_size,
    base,
    viewport,
  );
  let padding_bottom = resolve_length_for_paint(
    &style.padding_bottom,
    font_size,
    style.root_font_size,
    base,
    viewport,
  );

  let border_rect = rect;
  let padding_rect = inset_rect(
    border_rect,
    border_left,
    border_top,
    border_right,
    border_bottom,
  );
  let content_rect = inset_rect(
    padding_rect,
    padding_left,
    padding_top,
    padding_right,
    padding_bottom,
  );

  BackgroundRects { border: border_rect, content: content_rect }
}

fn resolve_length_for_paint(
  len: &Length,
  font_size: f32,
  root_font_size: f32,
  percentage_base: f32,
  viewport: Option<(f32, f32)>,
) -> f32 {
  let needs_viewport = len.unit.is_viewport_relative()
    || len
      .calc
      .as_ref()
      .map(|c| c.has_viewport_relative())
      .unwrap_or(false);
  let (vw, vh) = match viewport {
    Some(vp) => vp,
    None if needs_viewport => (f32::NAN, f32::NAN),
    None => (percentage_base, percentage_base),
  };
  let resolved = len
    .resolve_with_context(Some(percentage_base), vw, vh, font_size, root_font_size)
    .unwrap_or_else(|| {
      if len.unit.is_absolute() {
        len.to_px()
      } else {
        len.value * font_size
      }
    });
  if resolved.is_finite() {
    resolved
  } else {
    0.0
  }
}

fn inset_rect(rect: Rect, left: f32, top: f32, right: f32, bottom: f32) -> Rect {
  let new_x = rect.x() + left;
  let new_y = rect.y() + top;
  let new_w = (rect.width() - left - right).max(0.0);
  let new_h = (rect.height() - top - bottom).max(0.0);
  Rect::from_xywh(new_x, new_y, new_w, new_h)
}

fn resolve_transform_length(
  len: &Length,
  font_size: f32,
  root_font_size: f32,
  percentage_base: f32,
) -> f32 {
  let needs_viewport = len.unit.is_viewport_relative()
    || len
      .calc
      .as_ref()
      .map(|c| c.has_viewport_relative())
      .unwrap_or(false);
  let (vw, vh) = if needs_viewport {
    (f32::NAN, f32::NAN)
  } else {
    (0.0, 0.0)
  };

  len
    .resolve_with_context(Some(percentage_base), vw, vh, font_size, root_font_size)
    .unwrap_or(len.value)
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::css::types::Transform;
  use crate::style::types::{MotionPathCommand, MotionPosition, OffsetAnchor, OffsetPath};

  #[test]
  fn resolves_basic_2d_components() {
    let mut translate = ComputedStyle::default();
    translate
      .transform
      .push(Transform::Translate(Length::px(10.0), Length::px(5.0)));
    let bounds = Rect::from_xywh(0.0, 0.0, 100.0, 50.0);
    let matrix = resolve_transform3d(&translate, bounds, None)
      .expect("translate")
      .to_2d()
      .expect("affine transform");
    assert!((matrix.e - 10.0).abs() < 1e-4);
    assert!((matrix.f - 5.0).abs() < 1e-4);

    let mut rotate = ComputedStyle::default();
    rotate.transform.push(Transform::Rotate(90.0));
    let matrix = resolve_transform3d(&rotate, bounds, None)
      .expect("rotate")
      .to_2d()
      .expect("affine transform");
    assert!(matrix.a.abs() < 1e-4);
    assert!((matrix.b - 1.0).abs() < 1e-4);
    assert!((matrix.c + 1.0).abs() < 1e-4);
    assert!(matrix.d.abs() < 1e-4);

    let mut skew = ComputedStyle::default();
    skew.transform.push(Transform::Skew(45.0, 10.0));
    let matrix = resolve_transform3d(&skew, bounds, None)
      .expect("skew")
      .to_2d()
      .expect("affine transform");
    assert!((matrix.c - 1.0).abs() < 1e-4);
    assert!((matrix.b - 10.0_f32.to_radians().tan()).abs() < 1e-4);
  }

  #[test]
  fn rotate_3d_with_perspective_preserves_depth() {
    let mut style = ComputedStyle::default();
    style.perspective = Some(Length::px(500.0));
    style.transform.push(Transform::RotateX(45.0));

    let transforms = resolve_transforms(&style, Rect::from_xywh(0.0, 0.0, 20.0, 20.0), None);
    let child_perspective = transforms
      .child_perspective
      .expect("child perspective matrix");
    assert!((child_perspective.m[11] + 1.0 / 500.0).abs() < 1e-6);

    let transform = transforms.self_transform.expect("self transform");
    let expected = 45.0_f32.to_radians().cos();
    assert!((transform.m[5] - expected).abs() < 1e-6);
    assert!(
      transform.to_2d().is_none(),
      "3d transform should not collapse to 2d"
    );
  }

  #[test]
  fn matrix3d_passthrough_remains_unchanged() {
    let mut style = ComputedStyle::default();
    let values = [
      1.0, 0.0, 0.0, 0.0, //
      0.0, 1.0, 0.0, 0.0, //
      0.0, 0.0, 1.0, 0.0, //
      5.0, 6.0, 7.0, 1.0,
    ];
    style.transform.push(Transform::Matrix3d(values));

    let transform =
      resolve_transform3d(&style, Rect::from_xywh(0.0, 0.0, 10.0, 10.0), None).expect("matrix3d");
    assert_eq!(transform.m, values);
  }

  #[test]
  fn transform_and_perspective_origins_offset_matrix() {
    let mut style = ComputedStyle::default();
    style.transform_origin.x = Length::percent(50.0);
    style.transform_origin.y = Length::percent(50.0);
    style.transform.push(Transform::Scale(2.0, 1.0));

    let bounds = Rect::from_xywh(0.0, 0.0, 200.0, 100.0);
    let without_perspective = resolve_transforms(&style, bounds, None)
      .self_transform
      .expect("transform without perspective");
    let affine = without_perspective.to_2d().expect("affine");
    // Scale around center => translate by (1 - sx) * origin_x.
    assert!((affine.e + 100.0).abs() < 1e-4);

    let mut with_default_po = style.clone();
    with_default_po.perspective = Some(Length::px(400.0));
    let default_po = resolve_transforms(&with_default_po, bounds, None)
      .child_perspective
      .expect("default perspective origin");

    let mut shifted_po = with_default_po.clone();
    shifted_po.perspective_origin.x = Length::percent(25.0);
    shifted_po.perspective_origin.y = Length::percent(0.0);
    let shifted = resolve_transforms(&shifted_po, bounds, None)
      .child_perspective
      .expect("shifted perspective origin");

    assert_ne!(default_po.m, shifted.m);
  }

  #[test]
  fn motion_path_composes_with_transform_list() {
    let mut style = ComputedStyle::default();
    style.transform.push(Transform::Scale(2.0, 1.0));
    style.offset_path = OffsetPath::Path(vec![
      MotionPathCommand::MoveTo(MotionPosition {
        x: Length::px(0.0),
        y: Length::px(0.0),
      }),
      MotionPathCommand::LineTo(MotionPosition {
        x: Length::px(100.0),
        y: Length::px(0.0),
      }),
    ]);
    style.offset_anchor = OffsetAnchor::Position {
      x: Length::px(0.0),
      y: Length::px(0.0),
    };
    style.offset_distance = Length::percent(50.0);

    let bounds = Rect::from_xywh(0.0, 0.0, 20.0, 20.0);
    let transform = resolve_transform3d(&style, bounds, None).expect("transform");
    let affine = transform.to_2d().expect("affine");

    assert!((affine.e - 100.0).abs() < 1e-3);
    assert!((affine.a - 2.0).abs() < 1e-3);
  }
}
