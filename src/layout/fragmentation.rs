//! Fragmentation utilities
//!
//! This module post-processes a laid out fragment tree into multiple fragmentainer
//! roots (pages or columns). It clips fragments to each fragmentainer and carries
//! break metadata from computed styles.

use std::sync::OnceLock;

use crate::geometry::Rect;
use crate::style::types::{BreakBetween, BreakInside};
use crate::style::ComputedStyle;
use crate::tree::fragment_tree::{FragmentContent, FragmentNode};

/// Options controlling how fragments are split across fragmentainers.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct FragmentationOptions {
  /// Height of each fragmentainer (page/column block-size).
  pub fragmentainer_size: f32,
  /// Number of columns in a fragmentainer. Currently informational; fragment
  /// splitting is performed vertically for each fragmentainer slot.
  pub column_count: usize,
  /// Gap between columns in CSS pixels.
  pub column_gap: f32,
}

impl FragmentationOptions {
  /// Creates a new set of fragmentation options for a given fragmentainer size.
  pub fn new(fragmentainer_size: f32) -> Self {
    Self {
      fragmentainer_size,
      column_count: 1,
      column_gap: 0.0,
    }
  }
}

impl Default for FragmentationOptions {
  fn default() -> Self {
    Self::new(0.0)
  }
}

/// Splits a fragment tree into multiple fragmentainer roots based on the given options.
///
/// The returned fragments retain the original tree structure but are clipped to the
/// fragmentainer block-size. Fragment metadata (`fragment_index`, `fragment_count`, and
/// `fragmentainer_index`) are populated so downstream stages can reason about page/column
/// membership.
pub fn fragment_tree(root: &FragmentNode, options: &FragmentationOptions) -> Vec<FragmentNode> {
  if options.fragmentainer_size <= 0.0 {
    return vec![root.clone()];
  }

  let mut boundaries = vec![0.0];
  boundaries.extend(collect_forced_boundaries(root, 0.0));
  let mut pos = options.fragmentainer_size;
  let total_height = root.bounding_box().height().max(options.fragmentainer_size);
  while pos < total_height {
    boundaries.push(pos);
    pos += options.fragmentainer_size;
  }

  // Ensure the final boundary closes the content range.
  boundaries.push(total_height);

  boundaries.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
  boundaries.dedup_by(|a, b| (*a - *b).abs() < 0.01);

  if boundaries.len() < 2 {
    return vec![root.clone()];
  }

  let fragment_count = boundaries.len() - 1;
  let mut fragments = Vec::with_capacity(fragment_count);
  for (index, window) in boundaries.windows(2).enumerate() {
    let start = window[0];
    let end = window[1];
    if end <= start {
      continue;
    }

    if let Some(mut clipped) = clip_node(
      root,
      start,
      end,
      0.0,
      start,
      index,
      fragment_count,
    ) {
      propagate_fragment_metadata(&mut clipped, index, fragment_count);
      fragments.push(clipped);
    }
  }

  if fragments.is_empty() {
    fragments.push(root.clone());
  }

  fragments
}

fn propagate_fragment_metadata(node: &mut FragmentNode, index: usize, count: usize) {
  node.fragment_index = index;
  node.fragment_count = count.max(1);
  node.fragmentainer_index = index;
  for child in &mut node.children {
    propagate_fragment_metadata(child, index, count);
  }
}

fn clip_node(
  node: &FragmentNode,
  fragment_start: f32,
  fragment_end: f32,
  parent_abs_start: f32,
  parent_clipped_abs_start: f32,
  fragment_index: usize,
  fragment_count: usize,
) -> Option<FragmentNode> {
  let node_abs_start = parent_abs_start + node.bounds.y();
  let node_abs_end = node_abs_start + node.bounds.height();

  if node_abs_end <= fragment_start || node_abs_start >= fragment_end {
    return None;
  }

  let default_style = default_style();
  let style = if let Some(style) = node.style.as_ref() {
    style.as_ref()
  } else {
    default_style
  };
  let has_line_children = node
    .children
    .iter()
    .any(|child| matches!(child.content, FragmentContent::Line { .. }));
  let avoid_inside = matches!(style.break_inside, BreakInside::Avoid)
    || ((style.widows > 1 || style.orphans > 1) && has_line_children);

  // Honor break-inside/line constraints by keeping the fragment intact within a single fragmentainer.
  if avoid_inside && (node_abs_start < fragment_start || node_abs_end > fragment_end) {
    if (fragment_start..fragment_end).contains(&node_abs_start) {
      let mut cloned = clone_without_children(node);
      cloned.fragment_index = fragment_index;
      cloned.fragment_count = fragment_count.max(1);
      cloned.fragmentainer_index = fragment_index;
      cloned.children = node.children.clone();
      return Some(cloned);
    }
    return None;
  }

  let clipped_abs_start = node_abs_start.max(fragment_start);
  let clipped_abs_end = node_abs_end.min(fragment_end);
  let new_height = (clipped_abs_end - clipped_abs_start).max(0.0);
  let new_y = clipped_abs_start - parent_clipped_abs_start;

  let mut cloned = clone_without_children(node);
  cloned.bounds = Rect::from_xywh(node.bounds.x(), new_y, node.bounds.width(), new_height);
  cloned.fragment_index = fragment_index;
  cloned.fragment_count = fragment_count.max(1);
  cloned.fragmentainer_index = fragment_index;

  for child in &node.children {
    if let Some(child_clipped) = clip_node(
      child,
      fragment_start,
      fragment_end,
      node_abs_start,
      clipped_abs_start,
      fragment_index,
      fragment_count,
    ) {
      cloned.children.push(child_clipped);
    }
  }

  Some(cloned)
}

fn clone_without_children(node: &FragmentNode) -> FragmentNode {
  FragmentNode {
    bounds: node.bounds,
    content: node.content.clone(),
    baseline: node.baseline,
    children: Vec::new(),
    style: node.style.clone(),
    fragment_index: node.fragment_index,
    fragment_count: node.fragment_count,
    fragmentainer_index: node.fragmentainer_index,
  }
}

fn collect_forced_boundaries(node: &FragmentNode, abs_start: f32) -> Vec<f32> {
  let mut breaks = Vec::new();
  let default_style = default_style();
  let style = if let Some(style) = node.style.as_ref() {
    style.as_ref()
  } else {
    default_style
  };

  if forces_break(style.break_before) {
    breaks.push(abs_start);
  }

  let abs_end = abs_start + node.bounds.height();
  if forces_break(style.break_after) {
    breaks.push(abs_end);
  }

  for child in &node.children {
    let child_abs = abs_start + child.bounds.y();
    breaks.extend(collect_forced_boundaries(child, child_abs));
  }

  breaks
}

fn forces_break(value: BreakBetween) -> bool {
  matches!(value, BreakBetween::Always | BreakBetween::Page | BreakBetween::Column)
}

fn default_style() -> &'static ComputedStyle {
  static DEFAULT: OnceLock<ComputedStyle> = OnceLock::new();
  DEFAULT.get_or_init(ComputedStyle::default)
}
