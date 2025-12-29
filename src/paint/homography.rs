//! 2D projective transform utilities (homographies)
//!
//! Provides a small, dependency-free implementation for building and using 3×3
//! projective matrices. The matrix is stored in **row-major** order:
//! ```
//! [ m0 m1 m2 ]
//! [ m3 m4 m5 ]
//! [ m6 m7 m8 ]
//! ```
//! mapping a point `(x, y, 1)` to homogeneous coordinates `(x', y', w')` via:
//! ```
//! x' = m0 * x + m1 * y + m2
//! y' = m3 * x + m4 * y + m5
//! w' = m6 * x + m7 * y + m8
//! ```
//! The projected 2D point is `(x' / w', y' / w')`.

use crate::geometry::{Point, Rect};
use crate::paint::display_list::{Transform2D, Transform3D};

const EPSILON: f32 = 1e-6;

/// 2D projective transform (homography)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Homography {
  /// Row-major 3×3 matrix coefficients.
  ///
  /// The last element is commonly normalized to 1.0, but any non-zero scale
  /// factor represents the same projective transform.
  pub m: [f32; 9],
}

impl Homography {
  /// Identity homography (no transformation)
  pub const IDENTITY: Self = Self {
    m: [1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0],
  };

  /// Creates the identity homography.
  pub const fn identity() -> Self {
    Self::IDENTITY
  }

  /// Builds a homography mapping four source points to four destination points.
  ///
  /// Returns `None` if the points are degenerate (no unique solution).
  pub fn from_quad_to_quad(src: [Point; 4], dst: [Point; 4]) -> Option<Self> {
    const MIN_AREA: f32 = 1e-3;
    if !src
      .iter()
      .chain(dst.iter())
      .all(|p| p.x.is_finite() && p.y.is_finite())
    {
      return None;
    }
    let src_area = quad_area(&src);
    let dst_area = quad_area(&dst);
    if src_area.abs() < MIN_AREA || dst_area.abs() < MIN_AREA {
      return None;
    }
    // Solve A * x = b where x = [a b c d e f g h]^T and the matrix is:
    // [x y 1 0 0 0 -x*X -y*X] [a]   [X]
    // [0 0 0 x y 1 -x*Y -y*Y] [b] = [Y]
    //                           ...
    let mut a = [[0.0_f32; 8]; 8];
    let mut b = [0.0_f32; 8];

    for i in 0..4 {
      let s = src[i];
      let d = dst[i];
      let row0 = 2 * i;
      let row1 = row0 + 1;

      a[row0][0] = s.x;
      a[row0][1] = s.y;
      a[row0][2] = 1.0;
      a[row0][6] = -d.x * s.x;
      a[row0][7] = -d.x * s.y;
      b[row0] = d.x;

      a[row1][3] = s.x;
      a[row1][4] = s.y;
      a[row1][5] = 1.0;
      a[row1][6] = -d.y * s.x;
      a[row1][7] = -d.y * s.y;
      b[row1] = d.y;
    }

    let solution = solve_8x8(a, b)?;

    let mut m = [0.0_f32; 9];
    m[..8].copy_from_slice(&solution);
    m[8] = 1.0;

    if m.iter().all(|v| v.is_finite()) {
      Some(Homography { m })
    } else {
      None
    }
  }

  /// Convenience alias for `from_quad_to_quad`.
  pub fn from_quads(src: [Point; 4], dst: [Point; 4]) -> Option<Self> {
    Self::from_quad_to_quad(src, dst)
  }

  /// Projects a 3D transform onto the z=0 plane as a 2D homography.
  ///
  /// Matches `Transform3D::transform_point(x, y, 0)` semantics, preserving
  /// perspective components where `w` depends on `x`/`y`.
  pub fn from_transform3d_z0(t: &Transform3D) -> Self {
    Self {
      m: [
        t.m[0], t.m[4], t.m[12], t.m[1], t.m[5], t.m[13], t.m[3], t.m[7], t.m[15],
      ],
    }
  }

  /// Converts a 2D affine transform into a homography.
  pub fn from_affine(t: &Transform2D) -> Self {
    Self {
      m: [t.a, t.c, t.e, t.b, t.d, t.f, 0.0, 0.0, 1.0],
    }
  }

  /// Multiplies two homographies (apply `other` first, then `self`).
  pub fn multiply(&self, other: &Homography) -> Homography {
    let mut out = [0.0_f32; 9];
    for row in 0..3 {
      for col in 0..3 {
        out[row * 3 + col] = self.m[row * 3 + 0] * other.m[0 * 3 + col]
          + self.m[row * 3 + 1] * other.m[1 * 3 + col]
          + self.m[row * 3 + 2] * other.m[2 * 3 + col];
      }
    }
    Homography { m: out }
  }

  /// Determinant of the 3×3 homography matrix.
  pub fn determinant(&self) -> f32 {
    let [a, b, c, d, e, f, g, h, i] = self.m;
    a * (e * i - f * h) - b * (d * i - f * g) + c * (d * h - e * g)
  }

  /// Returns true if all coefficients are finite.
  pub fn is_finite(&self) -> bool {
    self.m.iter().all(|v| v.is_finite())
  }

  /// Returns true if the homography is invertible.
  pub fn is_invertible(&self) -> bool {
    let det = self.determinant();
    det.is_finite() && det.abs() >= EPSILON
  }

  /// Returns the inverse homography, if it exists and is numerically stable.
  pub fn invert(&self) -> Option<Self> {
    let [a, b, c, d, e, f, g, h, i] = self.m;
    let det = a * (e * i - f * h) - b * (d * i - f * g) + c * (d * h - e * g);
    if !det.is_finite() || det.abs() < EPSILON {
      return None;
    }
    let inv_det = 1.0 / det;

    let m = [
      (e * i - f * h) * inv_det,
      (c * h - b * i) * inv_det,
      (b * f - c * e) * inv_det,
      (f * g - d * i) * inv_det,
      (a * i - c * g) * inv_det,
      (c * d - a * f) * inv_det,
      (d * h - e * g) * inv_det,
      (b * g - a * h) * inv_det,
      (a * e - b * d) * inv_det,
    ];

    if m.iter().all(|v| v.is_finite()) {
      Some(Homography { m })
    } else {
      None
    }
  }

  /// Alias for `invert`.
  ///
  /// Some callers refer to the inverse as `inverse()`; keep this wrapper to
  /// avoid duplicating implementations.
  #[inline]
  pub fn inverse(&self) -> Option<Self> {
    self.invert()
  }

  /// Maps a point through the homography.
  ///
  /// Returns `None` if the homogeneous w component is zero or non-finite.
  pub fn map_point(&self, p: Point) -> Option<Point> {
    let x = self.m[0] * p.x + self.m[1] * p.y + self.m[2];
    let y = self.m[3] * p.x + self.m[4] * p.y + self.m[5];
    let w = self.m[6] * p.x + self.m[7] * p.y + self.m[8];
    if !w.is_finite() || w.abs() < EPSILON {
      return None;
    }
    let inv_w = 1.0 / w;
    let mapped = Point::new(x * inv_w, y * inv_w);
    if mapped.x.is_finite() && mapped.y.is_finite() {
      Some(mapped)
    } else {
      None
    }
  }

  /// Maps a point through the homography, returning a tuple for convenience.
  #[inline]
  pub fn transform_point(&self, x: f32, y: f32) -> Option<(f32, f32)> {
    let mapped = self.map_point(Point::new(x, y))?;
    Some((mapped.x, mapped.y))
  }

  /// Maps an axis-aligned rectangle and returns the axis-aligned bounding box.
  ///
  /// Returns `None` if any corner maps to an invalid point (e.g. w ≈ 0).
  pub fn map_rect_aabb(&self, rect: Rect) -> Option<Rect> {
    if !self.is_finite() {
      return None;
    }
    let corners = [
      rect.origin,
      Point::new(rect.max_x(), rect.min_y()),
      Point::new(rect.min_x(), rect.max_y()),
      Point::new(rect.max_x(), rect.max_y()),
    ];

    let mut mapped = [Point::ZERO; 4];
    for (out, corner) in mapped.iter_mut().zip(corners.iter()) {
      *out = self.map_point(*corner)?;
    }

    let min_x = mapped.iter().map(|p| p.x).fold(f32::INFINITY, f32::min);
    let min_y = mapped.iter().map(|p| p.y).fold(f32::INFINITY, f32::min);
    let max_x = mapped.iter().map(|p| p.x).fold(f32::NEG_INFINITY, f32::max);
    let max_y = mapped.iter().map(|p| p.y).fold(f32::NEG_INFINITY, f32::max);

    Some(Rect::from_xywh(min_x, min_y, max_x - min_x, max_y - min_y))
  }

  /// Returns true if the homography is (approximately) affine.
  pub fn is_affine(&self) -> bool {
    self.m[6].abs() < EPSILON
      && self.m[7].abs() < EPSILON
      && self.m[8].is_finite()
      && self.m[8].abs() >= EPSILON
  }

  /// Converts to a 2D affine transform if the homography is affine.
  pub fn to_affine(&self) -> Option<Transform2D> {
    if !self.is_affine() {
      return None;
    }

    Some(Transform2D {
      a: self.m[0] / self.m[8],
      b: self.m[3] / self.m[8],
      c: self.m[1] / self.m[8],
      d: self.m[4] / self.m[8],
      e: self.m[2] / self.m[8],
      f: self.m[5] / self.m[8],
    })
  }
}

fn solve_8x8(mut a: [[f32; 8]; 8], mut b: [f32; 8]) -> Option<[f32; 8]> {
  for col in 0..8 {
    let mut pivot_row = col;
    let mut pivot_val = a[pivot_row][col].abs();
    for row in (col + 1)..8 {
      let val = a[row][col].abs();
      if val > pivot_val {
        pivot_val = val;
        pivot_row = row;
      }
    }

    if !pivot_val.is_finite() || pivot_val < EPSILON {
      return None;
    }

    if pivot_row != col {
      a.swap(col, pivot_row);
      b.swap(col, pivot_row);
    }

    let inv_pivot = 1.0 / a[col][col];
    for j in col..8 {
      a[col][j] *= inv_pivot;
    }
    b[col] *= inv_pivot;

    for row in 0..8 {
      if row == col {
        continue;
      }
      let factor = a[row][col];
      if factor.abs() < EPSILON {
        continue;
      }
      for j in col..8 {
        a[row][j] -= factor * a[col][j];
      }
      b[row] -= factor * b[col];
    }
  }

  if b.iter().all(|v| v.is_finite()) {
    Some(b)
  } else {
    None
  }
}

fn quad_area(points: &[Point; 4]) -> f32 {
  let mut area = 0.0;
  for i in 0..4 {
    let p0 = points[i];
    let p1 = points[(i + 1) % 4];
    area += p0.x * p1.y - p1.x * p0.y;
  }
  0.5 * area
}

pub fn quad_bounds(points: &[Point; 4]) -> Rect {
  let mut min_x = f32::INFINITY;
  let mut min_y = f32::INFINITY;
  let mut max_x = f32::NEG_INFINITY;
  let mut max_y = f32::NEG_INFINITY;

  for p in points {
    min_x = min_x.min(p.x);
    min_y = min_y.min(p.y);
    max_x = max_x.max(p.x);
    max_y = max_y.max(p.y);
  }

  if !min_x.is_finite() || !min_y.is_finite() || !max_x.is_finite() || !max_y.is_finite() {
    return Rect::ZERO;
  }

  Rect::from_xywh(min_x, min_y, max_x - min_x, max_y - min_y)
}

pub fn rect_corners(rect: Rect) -> [Point; 4] {
  [
    Point::new(rect.min_x(), rect.min_y()),
    Point::new(rect.max_x(), rect.min_y()),
    Point::new(rect.max_x(), rect.max_y()),
    Point::new(rect.min_x(), rect.max_y()),
  ]
}

#[cfg(test)]
mod tests {
  use super::*;

  fn assert_point_close(actual: Point, expected: Point) {
    let dx = (actual.x - expected.x).abs();
    let dy = (actual.y - expected.y).abs();
    assert!(
      dx < 1e-4 && dy < 1e-4,
      "points differ: actual={:?} expected={:?}",
      actual,
      expected
    );
  }

  #[test]
  fn quad_identity() {
    let src = [
      Point::new(0.0, 0.0),
      Point::new(1.0, 0.0),
      Point::new(1.0, 1.0),
      Point::new(0.0, 1.0),
    ];
    let dst = src;

    let h = Homography::from_quad_to_quad(src, dst).expect("identity homography");
    assert!(h.is_affine());
    let rect = Rect::from_xywh(0.0, 0.0, 1.0, 1.0);
    assert_eq!(h.map_rect_aabb(rect).unwrap(), rect);
    assert_point_close(
      h.map_point(Point::new(0.25, 0.75)).unwrap(),
      Point::new(0.25, 0.75),
    );
  }

  #[test]
  fn perspective_quad_mapping_round_trip() {
    let src = [
      Point::new(0.0, 0.0),
      Point::new(1.0, 0.0),
      Point::new(1.0, 1.0),
      Point::new(0.0, 1.0),
    ];
    let dst = [
      Point::new(0.0, 0.0),
      Point::new(1.2, 0.1),
      Point::new(1.0, 0.9),
      Point::new(-0.1, 1.0),
    ];

    let h = Homography::from_quad_to_quad(src, dst).expect("perspective homography");
    for (s, d) in src.iter().zip(dst.iter()) {
      assert_point_close(h.map_point(*s).unwrap(), *d);
    }

    let inv = h.invert().expect("invertible homography");
    let interior = Point::new(0.3, 0.4);
    let mapped = h.map_point(interior).unwrap();
    let roundtrip = inv.map_point(mapped).unwrap();
    assert_point_close(roundtrip, interior);
  }

  #[test]
  fn transform3d_projection_matches() {
    let t = Transform3D {
      m: [
        1.0, 0.2, 0.0, 0.001, // column 1
        0.1, 1.1, 0.0, -0.0004, // column 2
        0.0, 0.0, 1.0, -0.002, // column 3
        0.5, -0.25, 0.1, 1.0, // column 4
      ],
    };

    let h = Homography::from_transform3d_z0(&t);
    let samples = [
      Point::new(0.0, 0.0),
      Point::new(10.0, 5.0),
      Point::new(-2.0, 3.5),
      Point::new(0.5, -0.75),
    ];

    for p in samples {
      let (tx, ty, _tz, tw) = t.transform_point(p.x, p.y, 0.0);
      let expected = Point::new(tx / tw, ty / tw);
      let mapped = h.map_point(p).expect("valid projection");
      assert_point_close(mapped, expected);
    }
  }
}
