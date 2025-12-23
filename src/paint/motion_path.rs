use crate::geometry::Point;
use crate::geometry::Rect;
use crate::paint::display_list::Transform2D;
use crate::style::types::BackgroundPosition;
use crate::style::types::BackgroundPositionComponent;
use crate::style::types::BasicShape;
use crate::style::types::MotionPathCommand;
use crate::style::types::MotionPosition;
use crate::style::types::OffsetAnchor;
use crate::style::types::OffsetPath;
use crate::style::types::OffsetRotate;
use crate::style::types::Ray;
use crate::style::types::ShapeRadius;
use crate::style::values::Length;
use crate::style::ComputedStyle;
use std::f32::consts::PI;

#[derive(Debug, Clone)]
struct MotionSegment {
  start: Point,
  end: Point,
}

impl MotionSegment {
  fn length(&self) -> f32 {
    let dx = self.end.x - self.start.x;
    let dy = self.end.y - self.start.y;
    (dx * dx + dy * dy).sqrt()
  }

  fn direction(&self) -> Option<Point> {
    let len = self.length();
    if len <= 0.0 {
      None
    } else {
      Some(Point::new(
        (self.end.x - self.start.x) / len,
        (self.end.y - self.start.y) / len,
      ))
    }
  }
}

pub fn compute_motion_transform(
  style: &ComputedStyle,
  bounds: Rect,
  viewport: Option<(f32, f32)>,
) -> Option<Transform2D> {
  let offset_path = match &style.offset_path {
    OffsetPath::None => return None,
    other => other,
  };

  let anchor = resolve_offset_anchor(style, bounds, viewport);

  let (point, tangent) = match offset_path {
    OffsetPath::Ray(ray) => point_along_ray(style, bounds, viewport, ray)?,
    OffsetPath::Path(commands) => point_along_path(style, bounds, viewport, commands)?,
    OffsetPath::BasicShape(shape) => point_along_shape(style, bounds, viewport, shape)?,
    OffsetPath::None => return None,
  };

  let translation = Point::new(point.x - anchor.x, point.y - anchor.y);
  let tangent_angle = tangent_angle(tangent);
  let rotation = resolve_offset_rotate(style.offset_rotate, tangent_angle);

  let mut transform = Transform2D::IDENTITY;
  if rotation.abs() > 0.0001 {
    let rotate = Transform2D::rotate(rotation.to_radians());
    transform = Transform2D::translate(anchor.x, anchor.y)
      .multiply(&rotate)
      .multiply(&Transform2D::translate(-anchor.x, -anchor.y));
  }

  if translation.x.abs() > 0.0001 || translation.y.abs() > 0.0001 {
    transform = Transform2D::translate(translation.x, translation.y).multiply(&transform);
  }

  if transform == Transform2D::IDENTITY {
    None
  } else {
    Some(transform)
  }
}

fn point_along_ray(
  style: &ComputedStyle,
  bounds: Rect,
  viewport: Option<(f32, f32)>,
  ray: &Ray,
) -> Option<(Point, Point)> {
  let origin = Point::new(
    bounds.x() + bounds.width() / 2.0,
    bounds.y() + bounds.height() / 2.0,
  );
  let mut base_length = ray
    .length
    .map(|l| resolve_length(&l, style, bounds.width().hypot(bounds.height()), viewport))
    .unwrap_or_else(|| bounds.width().hypot(bounds.height()));

  if ray.contain {
    let left = origin.x - bounds.x();
    let right = bounds.max_x() - origin.x;
    let top = origin.y - bounds.y();
    let bottom = bounds.max_y() - origin.y;
    base_length = base_length.min(left.min(right).min(top.min(bottom)));
  }

  let distance = resolve_length(&style.offset_distance, style, base_length.abs(), viewport);
  let angle = ray.angle.to_radians();
  let dir = Point::new(angle.cos(), angle.sin());
  let point = Point::new(origin.x + dir.x * distance, origin.y + dir.y * distance);
  Some((point, dir))
}

fn point_along_path(
  style: &ComputedStyle,
  bounds: Rect,
  viewport: Option<(f32, f32)>,
  commands: &[MotionPathCommand],
) -> Option<(Point, Point)> {
  let segments = build_segments_from_commands(style, bounds, viewport, commands)?;
  point_from_segments(style, viewport, segments)
}

fn point_along_shape(
  style: &ComputedStyle,
  bounds: Rect,
  viewport: Option<(f32, f32)>,
  shape: &BasicShape,
) -> Option<(Point, Point)> {
  let segments = build_segments_from_shape(style, bounds, viewport, shape)?;
  point_from_segments(style, viewport, segments)
}

fn point_from_segments(
  style: &ComputedStyle,
  viewport: Option<(f32, f32)>,
  segments: Vec<MotionSegment>,
) -> Option<(Point, Point)> {
  if segments.is_empty() {
    return None;
  }

  let mut total_length = 0.0;
  for seg in &segments {
    total_length += seg.length();
  }

  let distance = resolve_length(
    &style.offset_distance,
    style,
    total_length.max(0.0),
    viewport,
  );

  let mut remaining = distance;

  if remaining < 0.0 {
    if let Some(first) = segments.first() {
      let dir = first.direction()?;
      let point = Point::new(
        first.start.x + dir.x * remaining,
        first.start.y + dir.y * remaining,
      );
      return Some((point, dir));
    }
  }

  for seg in &segments {
    let len = seg.length();
    let dir = match seg.direction() {
      Some(d) => d,
      None => continue,
    };
    if remaining <= len {
      let point = Point::new(
        seg.start.x + dir.x * remaining,
        seg.start.y + dir.y * remaining,
      );
      return Some((point, dir));
    }
    remaining -= len;
  }

  let last = segments.last()?;
  let dir = last.direction().unwrap_or(Point::new(1.0, 0.0));
  let point = Point::new(
    last.end.x + dir.x * remaining,
    last.end.y + dir.y * remaining,
  );
  Some((point, dir))
}

fn build_segments_from_commands(
  style: &ComputedStyle,
  bounds: Rect,
  viewport: Option<(f32, f32)>,
  commands: &[MotionPathCommand],
) -> Option<Vec<MotionSegment>> {
  let mut segments = Vec::new();
  let mut current: Option<Point> = None;
  let mut subpath_start: Option<Point> = None;

  for command in commands {
    match command {
      MotionPathCommand::MoveTo(pos) => {
        let resolved = resolve_motion_position(*pos, style, bounds, viewport);
        current = Some(resolved);
        subpath_start = Some(resolved);
      }
      MotionPathCommand::LineTo(pos) => {
        let resolved = resolve_motion_position(*pos, style, bounds, viewport);
        if let Some(cur) = current {
          segments.push(MotionSegment {
            start: cur,
            end: resolved,
          });
        }
        current = Some(resolved);
      }
      MotionPathCommand::ClosePath => {
        if let (Some(cur), Some(start)) = (current, subpath_start) {
          segments.push(MotionSegment {
            start: cur,
            end: start,
          });
          current = Some(start);
        }
      }
    }
  }

  if segments.is_empty() {
    None
  } else {
    Some(segments)
  }
}

fn build_segments_from_shape(
  style: &ComputedStyle,
  bounds: Rect,
  viewport: Option<(f32, f32)>,
  shape: &BasicShape,
) -> Option<Vec<MotionSegment>> {
  match shape {
    BasicShape::Inset {
      top,
      right,
      bottom,
      left,
      ..
    } => {
      let top_px = resolve_length(top, style, bounds.height(), viewport);
      let right_px = resolve_length(right, style, bounds.width(), viewport);
      let bottom_px = resolve_length(bottom, style, bounds.height(), viewport);
      let left_px = resolve_length(left, style, bounds.width(), viewport);
      let rect = Rect::from_xywh(
        bounds.x() + left_px,
        bounds.y() + top_px,
        (bounds.width() - left_px - right_px).max(0.0),
        (bounds.height() - top_px - bottom_px).max(0.0),
      );
      let tl = Point::new(rect.x(), rect.y());
      let tr = Point::new(rect.max_x(), rect.y());
      let br = Point::new(rect.max_x(), rect.max_y());
      let bl = Point::new(rect.x(), rect.max_y());
      Some(vec![
        MotionSegment { start: tl, end: tr },
        MotionSegment { start: tr, end: br },
        MotionSegment { start: br, end: bl },
        MotionSegment { start: bl, end: tl },
      ])
    }
    BasicShape::Circle { radius, position } => {
      let center = resolve_background_position(position, bounds, style, viewport);
      let base = bounds.width().min(bounds.height());
      let r = resolve_shape_radius(*radius, center, bounds, style, viewport, base);
      if r <= 0.0 {
        return None;
      }
      Some(approximate_ellipse(center, r, r))
    }
    BasicShape::Ellipse {
      radius_x,
      radius_y,
      position,
    } => {
      let center = resolve_background_position(position, bounds, style, viewport);
      let rx = resolve_shape_radius(*radius_x, center, bounds, style, viewport, bounds.width());
      let ry = resolve_shape_radius(*radius_y, center, bounds, style, viewport, bounds.height());
      if rx <= 0.0 || ry <= 0.0 {
        return None;
      }
      Some(approximate_ellipse(center, rx, ry))
    }
    BasicShape::Polygon { points, .. } => {
      let mut segments = Vec::new();
      let mut iter = points.iter();
      let first = iter.next()?;
      let mut last = resolve_point_length_pair(*first, style, bounds, viewport);
      let start = last;
      for point in iter {
        let next = resolve_point_length_pair(*point, style, bounds, viewport);
        segments.push(MotionSegment {
          start: last,
          end: next,
        });
        last = next;
      }
      segments.push(MotionSegment {
        start: last,
        end: start,
      });
      Some(segments)
    }
  }
}

fn resolve_shape_radius(
  radius: ShapeRadius,
  center: Point,
  bounds: Rect,
  style: &ComputedStyle,
  viewport: Option<(f32, f32)>,
  percentage_base: f32,
) -> f32 {
  match radius {
    ShapeRadius::Length(len) => resolve_length(&len, style, percentage_base, viewport),
    ShapeRadius::ClosestSide => {
      let dx = (center.x - bounds.x())
        .abs()
        .min((bounds.max_x() - center.x).abs());
      let dy = (center.y - bounds.y())
        .abs()
        .min((bounds.max_y() - center.y).abs());
      dx.min(dy)
    }
    ShapeRadius::FarthestSide => {
      let dx = (center.x - bounds.x())
        .abs()
        .max((bounds.max_x() - center.x).abs());
      let dy = (center.y - bounds.y())
        .abs()
        .max((bounds.max_y() - center.y).abs());
      dx.max(dy)
    }
  }
}

fn approximate_ellipse(center: Point, rx: f32, ry: f32) -> Vec<MotionSegment> {
  let steps = 32;
  let mut segments = Vec::new();
  let mut prev = Point::new(center.x + rx, center.y);
  for i in 1..=steps {
    let theta = (i as f32 / steps as f32) * 2.0 * PI;
    let next = Point::new(center.x + rx * theta.cos(), center.y + ry * theta.sin());
    segments.push(MotionSegment {
      start: prev,
      end: next,
    });
    prev = next;
  }
  segments
}

fn resolve_motion_position(
  pos: MotionPosition,
  style: &ComputedStyle,
  bounds: Rect,
  viewport: Option<(f32, f32)>,
) -> Point {
  let x = resolve_length(&pos.x, style, bounds.width(), viewport);
  let y = resolve_length(&pos.y, style, bounds.height(), viewport);
  Point::new(bounds.x() + x, bounds.y() + y)
}

fn resolve_offset_anchor(
  style: &ComputedStyle,
  bounds: Rect,
  viewport: Option<(f32, f32)>,
) -> Point {
  match style.offset_anchor {
    OffsetAnchor::Auto => Point::new(
      bounds.x() + bounds.width() / 2.0,
      bounds.y() + bounds.height() / 2.0,
    ),
    OffsetAnchor::Position { x, y } => {
      let resolved_x = resolve_length(&x, style, bounds.width(), viewport);
      let resolved_y = resolve_length(&y, style, bounds.height(), viewport);
      Point::new(bounds.x() + resolved_x, bounds.y() + resolved_y)
    }
  }
}

fn resolve_length(
  len: &Length,
  style: &ComputedStyle,
  percentage_base: f32,
  viewport: Option<(f32, f32)>,
) -> f32 {
  let needs_viewport = len.unit.is_viewport_relative()
    || len
      .calc
      .as_ref()
      .map(|c| c.has_viewport_relative())
      .unwrap_or(false);
  let (vw, vh) = viewport.unwrap_or((0.0, 0.0));

  len
    .resolve_with_context(
      Some(percentage_base),
      if needs_viewport { vw } else { 0.0 },
      if needs_viewport { vh } else { 0.0 },
      style.font_size,
      style.root_font_size,
    )
    .unwrap_or(len.value)
}

fn resolve_background_position(
  position: &BackgroundPosition,
  bounds: Rect,
  style: &ComputedStyle,
  viewport: Option<(f32, f32)>,
) -> Point {
  match position {
    BackgroundPosition::Position { x, y } => {
      let resolved_x = resolve_position_component(x, bounds.width(), style, viewport);
      let resolved_y = resolve_position_component(y, bounds.height(), style, viewport);
      Point::new(bounds.x() + resolved_x, bounds.y() + resolved_y)
    }
  }
}

fn resolve_position_component(
  component: &BackgroundPositionComponent,
  size: f32,
  style: &ComputedStyle,
  viewport: Option<(f32, f32)>,
) -> f32 {
  let offset = resolve_length(&component.offset, style, size, viewport);
  component.alignment * size + offset
}

fn resolve_point_length_pair(
  pair: (Length, Length),
  style: &ComputedStyle,
  bounds: Rect,
  viewport: Option<(f32, f32)>,
) -> Point {
  let x = resolve_length(&pair.0, style, bounds.width(), viewport);
  let y = resolve_length(&pair.1, style, bounds.height(), viewport);
  Point::new(bounds.x() + x, bounds.y() + y)
}

fn tangent_angle(tangent: Point) -> f32 {
  tangent.y.atan2(tangent.x).to_degrees()
}

fn resolve_offset_rotate(rotate: OffsetRotate, tangent_angle: f32) -> f32 {
  match rotate {
    OffsetRotate::Auto { angle, reverse } => {
      tangent_angle + angle + if reverse { 180.0 } else { 0.0 }
    }
    OffsetRotate::Angle(angle) => angle,
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::style::types::Direction;
  use crate::style::types::WritingMode;

  fn rotation_degrees(transform: Transform2D) -> f32 {
    (-transform.c).atan2(transform.a).to_degrees()
  }

  fn normalize_degrees(angle: f32) -> f32 {
    let mut a = angle % 360.0;
    if a < 0.0 {
      a += 360.0;
    }
    a
  }

  #[test]
  fn line_path_moves_along_segment() {
    let mut style = ComputedStyle::default();
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

    let transform = compute_motion_transform(&style, Rect::from_xywh(0.0, 0.0, 10.0, 10.0), None)
      .expect("transform");

    assert!((transform.e - 50.0).abs() < 0.01);
    assert!(transform.f.abs() < 0.01);
  }

  #[test]
  fn line_path_animation_steps_follow_distance() {
    let mut style = ComputedStyle::default();
    style.offset_path = OffsetPath::Path(vec![
      MotionPathCommand::MoveTo(MotionPosition {
        x: Length::px(0.0),
        y: Length::px(0.0),
      }),
      MotionPathCommand::LineTo(MotionPosition {
        x: Length::px(120.0),
        y: Length::px(0.0),
      }),
    ]);
    style.offset_anchor = OffsetAnchor::Position {
      x: Length::px(0.0),
      y: Length::px(0.0),
    };

    for (pct, expected) in [(0.0, 0.0), (50.0, 60.0), (100.0, 120.0)] {
      style.offset_distance = Length::percent(pct);
      let transform = compute_motion_transform(&style, Rect::from_xywh(0.0, 0.0, 10.0, 10.0), None);
      let (tx, ty) = match transform {
        Some(t) => (t.e, t.f),
        None => (0.0, 0.0),
      };
      assert!((tx - expected).abs() < 0.01, "pct {}", pct);
      assert!(ty.abs() < 0.01);
    }
  }

  #[test]
  fn circle_path_rotates_tangent() {
    let mut style = ComputedStyle::default();
    style.offset_path = OffsetPath::BasicShape(Box::new(BasicShape::Circle {
      radius: ShapeRadius::Length(Length::px(50.0)),
      position: BackgroundPosition::Position {
        x: BackgroundPositionComponent {
          alignment: 0.5,
          offset: Length::px(0.0),
        },
        y: BackgroundPositionComponent {
          alignment: 0.5,
          offset: Length::px(0.0),
        },
      },
    }));
    style.offset_anchor = OffsetAnchor::Position {
      x: Length::px(0.0),
      y: Length::px(0.0),
    };
    style.offset_distance = Length::percent(25.0);

    let transform = compute_motion_transform(&style, Rect::from_xywh(0.0, 0.0, 100.0, 100.0), None)
      .expect("transform");

    // Quarter around the circle should place us at (50,100) with a leftward tangent
    assert!((transform.e - 50.0).abs() < 0.5);
    assert!((transform.f - 100.0).abs() < 0.5);
    let angle = normalize_degrees(rotation_degrees(transform));
    assert!(
      (angle - 180.0).abs() < 6.0,
      "angle {} transform {:?}",
      angle,
      transform
    );
  }

  #[test]
  fn circle_path_animation_reaches_halfway_point() {
    let mut style = ComputedStyle::default();
    style.offset_path = OffsetPath::BasicShape(Box::new(BasicShape::Circle {
      radius: ShapeRadius::Length(Length::px(40.0)),
      position: BackgroundPosition::Position {
        x: BackgroundPositionComponent {
          alignment: 0.5,
          offset: Length::px(0.0),
        },
        y: BackgroundPositionComponent {
          alignment: 0.5,
          offset: Length::px(0.0),
        },
      },
    }));
    style.offset_anchor = OffsetAnchor::Position {
      x: Length::px(0.0),
      y: Length::px(0.0),
    };
    style.offset_distance = Length::percent(50.0);

    let transform = compute_motion_transform(&style, Rect::from_xywh(0.0, 0.0, 100.0, 100.0), None)
      .expect("transform");

    // Halfway around the circle lands on the left side with an upward tangent.
    assert!((transform.e - 10.0).abs() < 0.5);
    assert!((transform.f - 50.0).abs() < 0.5);
    let angle = normalize_degrees(rotation_degrees(transform));
    assert!(
      (angle - 270.0).abs() < 6.0,
      "angle {} transform {:?}",
      angle,
      transform
    );
  }

  #[test]
  fn path_segments_continue_across_points() {
    let mut style = ComputedStyle::default();
    style.offset_path = OffsetPath::Path(vec![
      MotionPathCommand::MoveTo(MotionPosition {
        x: Length::px(0.0),
        y: Length::px(0.0),
      }),
      MotionPathCommand::LineTo(MotionPosition {
        x: Length::px(0.0),
        y: Length::px(100.0),
      }),
      MotionPathCommand::LineTo(MotionPosition {
        x: Length::px(100.0),
        y: Length::px(100.0),
      }),
    ]);
    style.offset_anchor = OffsetAnchor::Position {
      x: Length::px(0.0),
      y: Length::px(0.0),
    };
    style.offset_rotate = OffsetRotate::Auto {
      angle: 0.0,
      reverse: false,
    };
    style.offset_distance = Length::percent(75.0);

    let transform = compute_motion_transform(&style, Rect::from_xywh(0.0, 0.0, 10.0, 10.0), None)
      .expect("transform");

    assert!((transform.e - 50.0).abs() < 0.1);
    assert!((transform.f - 100.0).abs() < 0.1);
    let angle = normalize_degrees(rotation_degrees(transform));
    assert!(
      angle.min(360.0 - angle) < 0.5,
      "angle {} transform {:?}",
      angle,
      transform
    );
  }

  #[test]
  fn auto_reverse_adjusts_rotation_direction() {
    let mut style = ComputedStyle::default();
    style.offset_path = OffsetPath::Path(vec![
      MotionPathCommand::MoveTo(MotionPosition {
        x: Length::px(0.0),
        y: Length::px(0.0),
      }),
      MotionPathCommand::LineTo(MotionPosition {
        x: Length::px(0.0),
        y: Length::px(100.0),
      }),
      MotionPathCommand::LineTo(MotionPosition {
        x: Length::px(100.0),
        y: Length::px(100.0),
      }),
    ]);
    style.offset_anchor = OffsetAnchor::Position {
      x: Length::px(0.0),
      y: Length::px(0.0),
    };
    style.offset_rotate = OffsetRotate::Auto {
      angle: 0.0,
      reverse: true,
    };
    style.offset_distance = Length::percent(100.0);

    let transform = compute_motion_transform(&style, Rect::from_xywh(0.0, 0.0, 10.0, 10.0), None)
      .expect("transform");

    assert!((transform.e - 100.0).abs() < 0.1);
    assert!((transform.f - 100.0).abs() < 0.1);
    let angle = normalize_degrees(rotation_degrees(transform));
    assert!(
      (angle - 180.0).abs() < 1.0,
      "angle {} transform {:?}",
      angle,
      transform
    );
  }

  #[test]
  fn motion_path_respects_writing_mode() {
    let mut style = ComputedStyle::default();
    style.writing_mode = WritingMode::VerticalRl;
    style.direction = Direction::Rtl;
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
    style.offset_distance = Length::percent(100.0);

    let transform = compute_motion_transform(&style, Rect::from_xywh(0.0, 0.0, 10.0, 10.0), None)
      .expect("transform");

    assert!((transform.e - 100.0).abs() < 0.01);
    assert!(transform.f.abs() < 0.01);
  }
}
