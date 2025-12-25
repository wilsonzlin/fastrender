//! Fragmentation utilities
//!
//! Pagination and multi-column output require splitting a laid-out fragment tree
//! into fragmentainers (pages/columns). Fragmentation happens in the block axis
//! and respects authored break hints (`break-before/after/inside`), widows/orphans
//! constraints, and line-level break opportunities. The fragment tree that comes
//! out of layout is treated as flow order; this module decides where to break and
//! clones the appropriate fragment subtrees for each fragmentainer.

use std::sync::OnceLock;

use crate::geometry::{Point, Rect};
use crate::style::types::{BreakBetween, BreakInside};
use crate::style::ComputedStyle;
use crate::tree::fragment_tree::{
  FragmentContent, FragmentNode, FragmentainerPath, FragmentationInfo,
};

/// Options controlling how fragments are split across fragmentainers.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct FragmentationOptions {
  /// Block-size of each fragmentainer (page/column).
  pub fragmentainer_size: f32,
  /// Gap between successive fragmentainers when translated into absolute space.
  pub fragmentainer_gap: f32,
  /// Number of columns to target during fragmentation.
  pub column_count: usize,
  /// Gap between columns.
  pub column_gap: f32,
}

impl FragmentationOptions {
  /// Creates a new set of fragmentation options for a given fragmentainer size.
  pub fn new(fragmentainer_size: f32) -> Self {
    Self {
      fragmentainer_size,
      fragmentainer_gap: 0.0,
      column_count: 1,
      column_gap: 0.0,
    }
  }

  /// Sets a gap between fragmentainers (useful for pagination).
  pub fn with_gap(mut self, gap: f32) -> Self {
    self.fragmentainer_gap = gap.max(0.0);
    self
  }

  /// Configures column count and gap.
  pub fn with_columns(mut self, count: usize, gap: f32) -> Self {
    self.column_count = count.max(1);
    self.column_gap = gap.max(0.0);
    self
  }
}

impl Default for FragmentationOptions {
  fn default() -> Self {
    Self::new(0.0)
  }
}

#[derive(Debug, Clone, Copy)]
struct BreakCandidate {
  pos: f32,
  forced: bool,
}

#[derive(Debug, Clone, Copy)]
struct ForbiddenRange {
  start: f32,
  end: f32,
}

#[derive(Default, Debug)]
struct BreakPlan {
  candidates: Vec<BreakCandidate>,
  forbidden: Vec<ForbiddenRange>,
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

  let total_height = root
    .logical_bounding_box()
    .height()
    .max(options.fragmentainer_size);
  let mut plan = BreakPlan::default();
  collect_break_plan(root, 0.0, &mut plan);
  // Always allow breaking at the end of the content range so the final fragment closes.
  plan.candidates.push(BreakCandidate {
    pos: total_height,
    forced: true,
  });

  plan.candidates.sort_by(|a, b| {
    a.pos
      .partial_cmp(&b.pos)
      .unwrap_or(std::cmp::Ordering::Equal)
  });
  plan
    .candidates
    .dedup_by(|a, b| (a.pos - b.pos).abs() < 0.01 && a.forced == b.forced);

  let boundaries = compute_boundaries(total_height, options.fragmentainer_size, &plan);
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
      options.fragmentainer_size,
    ) {
      propagate_fragment_metadata(&mut clipped, index, fragment_count);

      // Translate fragments to account for fragmentainer gaps so downstream consumers
      // can reason about the absolute position of each fragmentainer stack.
      let vertical_offset = index as f32 * (options.fragmentainer_size + options.fragmentainer_gap);
      let translated = clipped.translate(Point::new(0.0, vertical_offset));

      fragments.push(translated);
    }
  }

  if fragments.is_empty() {
    fragments.push(root.clone());
  }

  fragments
}

pub(crate) fn propagate_fragment_metadata(node: &mut FragmentNode, index: usize, count: usize) {
  let path = FragmentainerPath::new(index);
  node.fragment_index = index;
  node.fragment_count = count.max(1);
  node.fragmentainer = path;
  node.fragmentainer_index = path.flattened_index();
  for child in &mut node.children {
    propagate_fragment_metadata(child, index, count);
  }
}

pub(crate) fn clip_node(
  node: &FragmentNode,
  fragment_start: f32,
  fragment_end: f32,
  parent_abs_start: f32,
  parent_clipped_abs_start: f32,
  fragment_index: usize,
  fragment_count: usize,
  fragmentainer_size: f32,
) -> Option<FragmentNode> {
  let logical_bounds = node.logical_bounds();
  let node_abs_start = parent_abs_start + logical_bounds.y();
  let node_abs_end = node_abs_start + logical_bounds.height();

  if node_abs_end <= fragment_start || node_abs_start >= fragment_end {
    return None;
  }

  let default_style = default_style();
  let style = node
    .style
    .as_ref()
    .map(|s| s.as_ref())
    .unwrap_or(default_style);

  if let Some(info) = node
    .fragmentation
    .as_ref()
    .filter(|meta| meta.column_count > 1)
  {
    return clip_multicol_node(
      node,
      fragment_start,
      fragment_end,
      parent_abs_start,
      parent_clipped_abs_start,
      fragment_index,
      fragment_count,
      fragmentainer_size,
      info,
    );
  }
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
      fragmentainer_size,
    ) {
      cloned.children.push(child_clipped);
    }
  }

  Some(cloned)
}

fn clip_multicol_node(
  node: &FragmentNode,
  fragment_start: f32,
  fragment_end: f32,
  parent_abs_start: f32,
  parent_clipped_abs_start: f32,
  fragment_index: usize,
  fragment_count: usize,
  fragmentainer_size: f32,
  info: &FragmentationInfo,
) -> Option<FragmentNode> {
  let logical_bounds = node.logical_bounds();
  let node_abs_start = parent_abs_start + logical_bounds.y();
  let node_abs_end = node_abs_start + logical_bounds.height();

  if node_abs_end <= fragment_start || node_abs_start >= fragment_end {
    return None;
  }

  let page_span = fragment_end - fragment_start;
  let before = (node_abs_start - fragment_start).max(0.0);
  let column_height = (page_span - before).max(0.0);
  if column_height <= 0.0 {
    return None;
  }

  let column_count = info.column_count.max(1);
  let logical_start_offset = ((fragment_start - node_abs_start).max(0.0)) * column_count as f32;
  let logical_end_offset = logical_start_offset + column_height * column_count as f32;
  let slice_start = node_abs_start + logical_start_offset;
  let slice_end = node_abs_start + logical_end_offset;

  let clipped_abs_start = node_abs_start.max(fragment_start);
  let new_y = clipped_abs_start - parent_clipped_abs_start;

  let mut cloned = clone_without_children(node);
  cloned.bounds = Rect::from_xywh(node.bounds.x(), new_y, node.bounds.width(), column_height);
  cloned.logical_override = Some(Rect::from_xywh(
    logical_bounds.x(),
    logical_bounds.y() + logical_start_offset,
    logical_bounds.width(),
    column_height * column_count as f32,
  ));
  cloned.fragment_index = fragment_index;
  cloned.fragment_count = fragment_count.max(1);
  cloned.fragmentainer_index = fragment_index;

  for child in &node.children {
    if let Some(mut child_clipped) = clip_node(
      child,
      slice_start,
      slice_end,
      node_abs_start,
      slice_start,
      fragment_index,
      fragment_count,
      fragmentainer_size,
    ) {
      let offset_within_slice = child_clipped.bounds.y();
      let mut column_index = if column_height > 0.0 {
        (offset_within_slice / column_height).floor() as usize
      } else {
        0
      };
      if column_index >= column_count {
        column_index = column_count - 1;
      }
      let within_column = offset_within_slice - column_height * column_index as f32;
      let x_offset = column_index as f32 * (info.column_width + info.column_gap);
      let dx = x_offset - child_clipped.bounds.x();
      let dy = within_column - child_clipped.bounds.y();
      translate_fragment_in_place(&mut child_clipped, dx, dy);
      cloned.children.push(child_clipped);
    }
  }

  Some(cloned)
}

fn clone_without_children(node: &FragmentNode) -> FragmentNode {
  FragmentNode {
    bounds: node.bounds,
    logical_override: node.logical_override,
    content: node.content.clone(),
    baseline: node.baseline,
    children: Vec::new(),
    style: node.style.clone(),
    fragment_index: node.fragment_index,
    fragment_count: node.fragment_count,
    fragmentainer_index: node.fragmentainer_index,
    fragmentainer: node.fragmentainer,
    scroll_overflow: node.scroll_overflow,
    fragmentation: node.fragmentation.clone(),
  }
}

fn translate_fragment_in_place(node: &mut FragmentNode, dx: f32, dy: f32) {
  node.bounds = node.bounds.translate(Point::new(dx, dy));
  if let Some(logical) = node.logical_override {
    node.logical_override = Some(logical.translate(Point::new(dx, dy)));
  }
  for child in &mut node.children {
    translate_fragment_in_place(child, dx, dy);
  }
}

fn collect_break_plan(node: &FragmentNode, abs_start: f32, plan: &mut BreakPlan) {
  let default_style = default_style();
  let style = node
    .style
    .as_ref()
    .map(|s| s.as_ref())
    .unwrap_or(default_style);

  let logical_bounds = node.logical_bounds();
  let abs_end = abs_start + logical_bounds.height();

  if forces_break(style.break_before) {
    plan.candidates.push(BreakCandidate {
      pos: abs_start,
      forced: true,
    });
  } else if avoids_break(style.break_before) {
    plan.forbidden.push(ForbiddenRange {
      start: abs_start - 0.01,
      end: abs_start + 0.01,
    });
  }

  if matches!(style.break_inside, BreakInside::Avoid) {
    // Allow breaking exactly at the element boundaries while preventing inner breaks.
    let epsilon = 0.1;
    if abs_end - abs_start > epsilon * 2.0 {
      plan.forbidden.push(ForbiddenRange {
        start: abs_start + epsilon,
        end: abs_end - epsilon,
      });
    }
  }

  let line_children: Vec<_> = node
    .children
    .iter()
    .filter(|child| matches!(child.content, FragmentContent::Line { .. }))
    .collect();

  if !line_children.is_empty() {
    let line_count = line_children.len();
    let widows = style.widows.max(1);
    let orphans = style.orphans.max(1);

    for (idx, line) in line_children.iter().enumerate() {
      let line_bounds = line.logical_bounds();
      let line_start = abs_start + line_bounds.y();
      let line_end = line_start + line_bounds.height();
      let lines_before = idx + 1;
      let lines_after = line_count.saturating_sub(lines_before);
      let allow_break = lines_before >= orphans && lines_after >= widows;
      if allow_break {
        plan.candidates.push(BreakCandidate {
          pos: line_end,
          forced: false,
        });
      } else {
        plan.forbidden.push(ForbiddenRange {
          start: line_start,
          end: line_end,
        });
      }
    }
  }

  for child in &node.children {
    let child_bounds = child.logical_bounds();
    let child_abs_start = abs_start + child_bounds.y();
    let child_abs_end = child_abs_start + child_bounds.height();

    let child_is_line = matches!(child.content, FragmentContent::Line { .. });

    // Natural break opportunity after each child so pagination can split between siblings.
    if !child_is_line {
      plan.candidates.push(BreakCandidate {
        pos: child_abs_end,
        forced: false,
      });
    }

    collect_break_plan(child, child_abs_start, plan);
  }

  if forces_break(style.break_after) {
    plan.candidates.push(BreakCandidate {
      pos: abs_end,
      forced: true,
    });
  } else if avoids_break(style.break_after) {
    plan.forbidden.push(ForbiddenRange {
      start: abs_end - 0.01,
      end: abs_end + 0.01,
    });
  }
}

pub(crate) fn collect_forced_boundaries(node: &FragmentNode, abs_start: f32) -> Vec<f32> {
  let mut plan = BreakPlan::default();
  collect_break_plan(node, abs_start, &mut plan);
  plan
    .candidates
    .into_iter()
    .filter(|c| c.forced)
    .map(|c| c.pos)
    .collect()
}

fn compute_boundaries(total_height: f32, fragmentainer: f32, plan: &BreakPlan) -> Vec<f32> {
  const EPSILON: f32 = 0.01;

  let mut forced: Vec<f32> = plan
    .candidates
    .iter()
    .filter(|c| c.forced)
    .map(|c| c.pos)
    .collect();
  let mut allowed: Vec<f32> = plan.candidates.iter().map(|c| c.pos).collect();

  forced.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
  forced.dedup_by(|a, b| (*a - *b).abs() < EPSILON);
  allowed.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
  allowed.dedup_by(|a, b| (*a - *b).abs() < EPSILON);

  let mut boundaries = vec![0.0];
  let mut start = 0.0;

  while start + fragmentainer < total_height - EPSILON {
    let target = start + fragmentainer;

    // Forced breaks take precedence when they fall within the available range.
    let forced_boundary = forced.iter().copied().find(|p| {
      *p > start + EPSILON && *p <= target + EPSILON && !is_forbidden(*p, &plan.forbidden)
    });

    let preferred_target = if !is_forbidden(target, &plan.forbidden) {
      Some(target)
    } else {
      None
    };

    let boundary = if let Some(pos) = forced_boundary {
      pos
    } else {
      let natural = allowed.iter().rev().copied().find(|p| {
        *p > start + EPSILON && *p <= target + EPSILON && !is_forbidden(*p, &plan.forbidden)
      });

      if let Some(pos) = natural {
        pos
      } else if let Some(pos) = preferred_target {
        pos
      } else {
        // No candidate in range: fall back to the next allowed candidate even if it overflows.
        allowed
          .iter()
          .copied()
          .find(|p| *p > target + EPSILON && !is_forbidden(*p, &plan.forbidden))
          .unwrap_or(target.min(total_height))
      }
    };

    let clamped = boundary.min(total_height);
    if (clamped - start).abs() < EPSILON {
      // Give up on tiny slices to avoid infinite loops.
      boundaries.push(total_height);
      break;
    }
    boundaries.push(clamped);
    start = clamped;
  }

  if total_height - *boundaries.last().unwrap_or(&0.0) > EPSILON {
    boundaries.push(total_height);
  }

  boundaries.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
  boundaries.dedup_by(|a, b| (*a - *b).abs() < EPSILON);
  boundaries
}

fn is_forbidden(pos: f32, forbidden: &[ForbiddenRange]) -> bool {
  forbidden
    .iter()
    .any(|range| pos >= range.start - 0.01 && pos <= range.end + 0.01)
}

fn forces_break(value: BreakBetween) -> bool {
  matches!(
    value,
    BreakBetween::Always | BreakBetween::Page | BreakBetween::Column
  )
}

fn avoids_break(value: BreakBetween) -> bool {
  matches!(value, BreakBetween::Avoid)
}

fn default_style() -> &'static ComputedStyle {
  static DEFAULT: OnceLock<ComputedStyle> = OnceLock::new();
  DEFAULT.get_or_init(ComputedStyle::default)
}
