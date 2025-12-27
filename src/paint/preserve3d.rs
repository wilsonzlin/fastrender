//! Preserve-3D scene helpers.
//!
//! This module provides small utilities for working with flattened planes in a
//! preserve-3d context. Items carry an `accumulated_transform` so callers can
//! cull backfaces and depth-sort planes before rasterization.

use crate::paint::display_list::Transform3D;
use crate::paint::transform3d::{backface_is_hidden, projected_z};
use crate::style::types::BackfaceVisibility;

/// A plane participating in a 3D scene.
#[derive(Debug, Clone)]
pub struct SceneItem<T> {
  /// Payload carried alongside the plane.
  pub item: T,
  /// Transform accumulated through preserve-3d ancestors (already includes any
  /// flattening of inlined descendants).
  pub accumulated_transform: Transform3D,
  /// Whether the plane should be culled when facing away from the viewer.
  pub backface_visibility: BackfaceVisibility,
}

impl<T> SceneItem<T> {
  /// Returns true if this plane should be culled per `backface-visibility`.
  pub fn is_backface_hidden(&self) -> bool {
    matches!(self.backface_visibility, BackfaceVisibility::Hidden)
      && backface_is_hidden(&self.accumulated_transform)
  }

  /// Camera-space Z for depth sorting.
  pub fn depth(&self) -> f32 {
    projected_z(&self.accumulated_transform)
  }
}

/// Filters out planes whose backfaces are hidden.
pub fn cull_backfaces<T>(items: Vec<SceneItem<T>>) -> Vec<SceneItem<T>> {
  items
    .into_iter()
    .filter(|item| !item.is_backface_hidden())
    .collect()
}

/// Culls hidden backfaces and returns planes sorted back-to-front by depth.
pub fn depth_sort_scene<T>(items: Vec<SceneItem<T>>) -> Vec<SceneItem<T>> {
  let mut kept = cull_backfaces(items);
  kept.sort_by(|a, b| a.depth().total_cmp(&b.depth()));
  kept
}
