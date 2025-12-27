use crate::geometry::Rect;
use crate::paint::display_list::Transform3D;
use std::cmp::Reverse;
use std::collections::BinaryHeap;

const EPSILON: f32 = 1e-5;

/// A single plane participating in preserve-3d depth sorting.
#[derive(Debug, Clone)]
pub struct SceneItem {
  /// Accumulated transform to screen space.
  pub transform: Transform3D,
  /// Local plane rectangle before transformation.
  pub plane_rect: Rect,
  /// Stable paint order for tie-breaking.
  pub paint_order: usize,
}

/// Plane equation coefficients in screen space (ax + by + cz + d = 0).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Plane {
  pub a: f32,
  pub b: f32,
  pub c: f32,
  pub d: f32,
}

#[derive(Debug, Clone, Copy)]
struct Vec2 {
  x: f32,
  y: f32,
}

#[derive(Debug, Clone, Copy)]
struct Vec3 {
  x: f32,
  y: f32,
  z: f32,
}

impl From<Vec3> for Vec2 {
  fn from(value: Vec3) -> Self {
    Self {
      x: value.x,
      y: value.y,
    }
  }
}

struct ProjectedQuad {
  points: [Vec3; 4],
  plane: Option<Plane>,
  average_z: f32,
}

impl ProjectedQuad {
  fn polygon(&self) -> [Vec2; 4] {
    [
      self.points[0].into(),
      self.points[1].into(),
      self.points[2].into(),
      self.points[3].into(),
    ]
  }

  fn sample_depth(&self, point: Vec2) -> Option<f32> {
    if let Some(plane) = self.plane {
      if let Some(z) = eval_plane_z(plane, point.x, point.y) {
        if z.is_finite() {
          return Some(z);
        }
      }
    }

    if self.average_z.is_finite() {
      Some(self.average_z)
    } else {
      None
    }
  }
}

/// Evaluate the depth (z) of a plane at the given screen-space (x, y).
pub fn eval_plane_z(plane: Plane, x: f32, y: f32) -> Option<f32> {
  if plane.c.abs() < EPSILON {
    return None;
  }
  Some((-(plane.a * x + plane.b * y + plane.d)) / plane.c)
}

/// Compute back-to-front draw order for a list of planes.
///
/// Returns indices into the input slice in the order they should be painted.
pub fn depth_sort(items: &[SceneItem]) -> Vec<usize> {
  let projections: Vec<_> = items.iter().map(project_item).collect();
  let n = items.len();

  let mut edges = vec![Vec::new(); n];
  let mut indegree = vec![0usize; n];

  for i in 0..n {
    let Some(pi) = &projections[i] else {
      continue;
    };
    for j in (i + 1)..n {
      let Some(pj) = &projections[j] else {
        continue;
      };

      let poly_i = pi.polygon();
      let poly_j = pj.polygon();
      let intersection = polygon_intersection(&poly_i, &poly_j);
      if intersection.is_empty() {
        continue;
      }

      let sample = centroid(&intersection);
      let za = pi.sample_depth(sample);
      let zb = pj.sample_depth(sample);
      let (Some(za), Some(zb)) = (za, zb) else {
        continue;
      };

      let diff = za - zb;
      if diff.abs() <= EPSILON {
        continue;
      }

      if diff < 0.0 {
        add_edge(&mut edges, &mut indegree, i, j);
      } else {
        add_edge(&mut edges, &mut indegree, j, i);
      }
    }
  }

  stable_topological_sort(items, &edges, &mut indegree)
}

fn project_item(item: &SceneItem) -> Option<ProjectedQuad> {
  let points = project_rect(&item.transform, item.plane_rect)?;
  let plane = compute_plane(&points);
  let average_z = points.iter().map(|p| p.z).sum::<f32>() / points.len() as f32;

  Some(ProjectedQuad {
    points,
    plane,
    average_z,
  })
}

fn project_rect(transform: &Transform3D, rect: Rect) -> Option<[Vec3; 4]> {
  let corners = [
    (rect.min_x(), rect.min_y()),
    (rect.max_x(), rect.min_y()),
    (rect.max_x(), rect.max_y()),
    (rect.min_x(), rect.max_y()),
  ];

  let mut points = Vec::new();
  for (x, y) in corners {
    points.push(project_point(transform, x, y)?);
  }

  Some([points[0], points[1], points[2], points[3]])
}

fn project_point(transform: &Transform3D, x: f32, y: f32) -> Option<Vec3> {
  let (tx, ty, tz, tw) = transform.transform_point(x, y, 0.0);
  if tw.abs() < EPSILON {
    return None;
  }

  Some(Vec3 {
    x: tx / tw,
    y: ty / tw,
    z: tz / tw,
  })
}

fn compute_plane(points: &[Vec3; 4]) -> Option<Plane> {
  const TRIPLES: [(usize, usize, usize); 4] = [(0, 1, 2), (0, 1, 3), (0, 2, 3), (1, 2, 3)];
  for (i, j, k) in TRIPLES {
    let p0 = points[i];
    let p1 = points[j];
    let p2 = points[k];

    let u = subtract(p1, p0);
    let v = subtract(p2, p0);
    let normal = cross(u, v);
    let norm = (normal.x * normal.x + normal.y * normal.y + normal.z * normal.z).sqrt();
    if norm < EPSILON {
      continue;
    }

    let a = normal.x;
    let b = normal.y;
    let c = normal.z;
    let d = -(a * p0.x + b * p0.y + c * p0.z);
    return Some(Plane { a, b, c, d });
  }
  None
}

fn subtract(a: Vec3, b: Vec3) -> Vec3 {
  Vec3 {
    x: a.x - b.x,
    y: a.y - b.y,
    z: a.z - b.z,
  }
}

fn cross(a: Vec3, b: Vec3) -> Vec3 {
  Vec3 {
    x: a.y * b.z - a.z * b.y,
    y: a.z * b.x - a.x * b.z,
    z: a.x * b.y - a.y * b.x,
  }
}

fn polygon_intersection(subject: &[Vec2], clip: &[Vec2]) -> Vec<Vec2> {
  let mut output = subject.to_vec();
  let is_ccw = polygon_area(clip) >= 0.0;
  for i in 0..clip.len() {
    let cp1 = clip[i];
    let cp2 = clip[(i + 1) % clip.len()];

    let input = output;
    output = Vec::new();

    if input.is_empty() {
      break;
    }

    let mut s = *input.last().unwrap();
    for &e in &input {
      if is_inside(e, cp1, cp2, is_ccw) {
        if !is_inside(s, cp1, cp2, is_ccw) {
          if let Some(intersection) = line_intersection(s, e, cp1, cp2) {
            output.push(intersection);
          }
        }
        output.push(e);
      } else if is_inside(s, cp1, cp2, is_ccw) {
        if let Some(intersection) = line_intersection(s, e, cp1, cp2) {
          output.push(intersection);
        }
      }
      s = e;
    }
  }
  output
}

fn is_inside(p: Vec2, a: Vec2, b: Vec2, is_ccw: bool) -> bool {
  let cross = (b.x - a.x) * (p.y - a.y) - (b.y - a.y) * (p.x - a.x);
  if is_ccw {
    cross >= -EPSILON
  } else {
    cross <= EPSILON
  }
}

fn line_intersection(s: Vec2, e: Vec2, a: Vec2, b: Vec2) -> Option<Vec2> {
  let dx1 = e.x - s.x;
  let dy1 = e.y - s.y;
  let dx2 = b.x - a.x;
  let dy2 = b.y - a.y;
  let denom = dx1 * dy2 - dy1 * dx2;
  if denom.abs() < EPSILON {
    return None;
  }
  let t = ((a.x - s.x) * dy2 - (a.y - s.y) * dx2) / denom;
  Some(Vec2 {
    x: s.x + t * dx1,
    y: s.y + t * dy1,
  })
}

fn centroid(poly: &[Vec2]) -> Vec2 {
  let (sum_x, sum_y) = poly
    .iter()
    .fold((0.0_f32, 0.0_f32), |(sx, sy), p| (sx + p.x, sy + p.y));
  let len = poly.len().max(1) as f32;
  Vec2 {
    x: sum_x / len,
    y: sum_y / len,
  }
}

fn polygon_area(poly: &[Vec2]) -> f32 {
  let mut area = 0.0;
  for i in 0..poly.len() {
    let p1 = poly[i];
    let p2 = poly[(i + 1) % poly.len()];
    area += p1.x * p2.y - p2.x * p1.y;
  }
  area * 0.5
}

fn add_edge(edges: &mut [Vec<usize>], indegree: &mut [usize], from: usize, to: usize) {
  if edges[from].contains(&to) {
    return;
  }
  edges[from].push(to);
  indegree[to] += 1;
}

fn stable_topological_sort(
  items: &[SceneItem],
  edges: &[Vec<usize>],
  indegree: &mut [usize],
) -> Vec<usize> {
  let n = items.len();
  if n == 0 {
    return Vec::new();
  }

  let mut remaining = vec![true; n];
  let mut result = Vec::with_capacity(n);
  let mut queue: BinaryHeap<Reverse<(usize, usize)>> = BinaryHeap::new();

  for (idx, degree) in indegree.iter().enumerate() {
    if *degree == 0 {
      queue.push(Reverse((items[idx].paint_order, idx)));
    }
  }

  let mut remaining_count = n;
  while remaining_count > 0 {
    let current = if let Some(Reverse((_, idx))) = queue.pop() {
      if !remaining[idx] {
        continue;
      }
      idx
    } else {
      let fallback = (0..n)
        .filter(|i| remaining[*i])
        .min_by_key(|i| (items[*i].paint_order, *i))
        .unwrap();
      fallback
    };

    if !remaining[current] {
      continue;
    }

    remaining[current] = false;
    remaining_count -= 1;
    result.push(current);

    for &to in &edges[current] {
      if indegree[to] > 0 {
        indegree[to] -= 1;
      }
      if remaining[to] && indegree[to] == 0 {
        queue.push(Reverse((items[to].paint_order, to)));
      }
    }
  }

  result
}

#[cfg(test)]
mod tests {
  use super::*;

  fn rect(x: f32, y: f32) -> Rect {
    Rect::from_xywh(x, y, 100.0, 100.0)
  }

  #[test]
  fn translate_z_orders_front_to_back() {
    let items = vec![
      SceneItem {
        transform: Transform3D::identity(),
        plane_rect: rect(0.0, 0.0),
        paint_order: 0,
      },
      SceneItem {
        transform: Transform3D::translate(0.0, 0.0, 10.0),
        plane_rect: rect(0.0, 0.0),
        paint_order: 1,
      },
    ];

    let order = depth_sort(&items);
    assert_eq!(order, vec![0, 1]);
  }

  #[test]
  fn stable_order_with_equal_depth() {
    let items = vec![
      SceneItem {
        transform: Transform3D::identity(),
        plane_rect: rect(0.0, 0.0),
        paint_order: 0,
      },
      SceneItem {
        transform: Transform3D::identity(),
        plane_rect: rect(0.0, 0.0),
        paint_order: 1,
      },
    ];

    let order = depth_sort(&items);
    assert_eq!(order, vec![0, 1]);
  }

  #[test]
  fn non_overlapping_items_preserve_order() {
    let items = vec![
      SceneItem {
        transform: Transform3D::identity(),
        plane_rect: rect(0.0, 0.0),
        paint_order: 0,
      },
      SceneItem {
        transform: Transform3D::identity(),
        plane_rect: rect(200.0, 0.0),
        paint_order: 1,
      },
    ];

    let order = depth_sort(&items);
    assert_eq!(order, vec![0, 1]);
  }

  #[test]
  fn cycle_fallback_is_deterministic() {
    let mut tilt_x = Transform3D::identity();
    tilt_x.m[2] = 0.0;
    tilt_x.m[6] = 1.0;

    let mut tilt_y = Transform3D::identity();
    tilt_y.m[2] = 1.0;
    tilt_y.m[6] = 0.0;

    let mut tilt_diag = Transform3D::identity();
    tilt_diag.m[2] = 1.0;
    tilt_diag.m[6] = -1.0;
    tilt_diag.m[14] = 0.9;

    let items = vec![
      SceneItem {
        transform: tilt_x,
        plane_rect: Rect::from_xywh(0.0, 0.0, 1.0, 1.0),
        paint_order: 0,
      },
      SceneItem {
        transform: tilt_y,
        plane_rect: Rect::from_xywh(0.5, 0.0, 1.0, 1.0),
        paint_order: 1,
      },
      SceneItem {
        transform: tilt_diag,
        plane_rect: Rect::from_xywh(0.0, 0.5, 1.0, 1.0),
        paint_order: 2,
      },
    ];

    let order = depth_sort(&items);
    assert_eq!(order, vec![0, 1, 2]);
  }
}
