//! 3D transform helpers shared across paint pipelines.
//!
//! This module provides utilities for working with [`Transform3D`] matrices,
//! including plane backface detection and perspective projection helpers.

use crate::paint::display_list::Transform3D;

const PERSPECTIVE_EPSILON: f32 = 1e-6;

/// Projects a 3D point by a transform, applying a perspective divide when
/// possible.
pub fn project_point(transform: &Transform3D, x: f32, y: f32, z: f32) -> [f32; 3] {
  let (tx, ty, tz, tw) = transform.transform_point(x, y, z);
  if tw.abs() < PERSPECTIVE_EPSILON {
    [tx, ty, tz]
  } else {
    [tx / tw, ty / tw, tz / tw]
  }
}

/// Returns true if a unit plane on the XY axis faces away from the viewer after
/// applying `transform`.
///
/// The check mirrors CSS `backface-visibility: hidden` semantics by projecting
/// three points on the plane (origin and unit X/Y axes), computing the resulting
/// normal, and culling when the normal points away from the camera (negative Z).
pub fn backface_is_hidden(transform: &Transform3D) -> bool {
  let p0 = project_point(transform, 0.0, 0.0, 0.0);
  let p1 = project_point(transform, 1.0, 0.0, 0.0);
  let p2 = project_point(transform, 0.0, 1.0, 0.0);

  let ux = [p1[0] - p0[0], p1[1] - p0[1], p1[2] - p0[2]];
  let uy = [p2[0] - p0[0], p2[1] - p0[1], p2[2] - p0[2]];

  let normal = [
    ux[1] * uy[2] - ux[2] * uy[1],
    ux[2] * uy[0] - ux[0] * uy[2],
    ux[0] * uy[1] - ux[1] * uy[0],
  ];

  normal[2] < 0.0
}

/// Returns the projected depth (camera-space Z) of the plane origin after
/// applying `transform`.
pub fn projected_z(transform: &Transform3D) -> f32 {
  let position = project_point(transform, 0.0, 0.0, 0.0);
  if position[2].is_finite() {
    position[2]
  } else {
    0.0
  }
}
