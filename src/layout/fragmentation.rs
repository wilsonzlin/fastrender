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
use crate::tree::fragment_tree::{FragmentContent, FragmentNode, FragmentainerPath};

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BreakStrength {
  Forced,
  Auto,
  Avoid,
}

#[derive(Debug, Clone, PartialEq)]
enum BreakKind {
  BetweenSiblings,
  LineBoundary {
    container_id: usize,
    line_index_end: usize,
  },
  EndOfContent,
}

#[derive(Debug, Clone)]
struct BreakOpportunity {
  pos: f32,
  strength: BreakStrength,
  kind: BreakKind,
}

#[derive(Debug, Clone)]
struct LineContainer {
  id: usize,
  line_ends: Vec<f32>,
  widows: usize,
  orphans: usize,
}

#[derive(Default, Debug)]
struct BreakCollection {
  opportunities: Vec<BreakOpportunity>,
  line_containers: Vec<LineContainer>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct ConstraintKey {
  /// Whether the candidate would place too few lines at the start of the fragment.
  violates_orphans: bool,
  /// Whether the candidate would place too few lines at the end of a continuation fragment.
  violates_continuation_widows: bool,
  /// Whether taking the candidate leaves too few lines for the eventual last fragment.
  violates_future_widows: bool,
}
// ConstraintKey derives Ord so candidates can be compared lexicographically, relaxing
// constraints in the order: orphans → continuation widows → future widows.

const BREAK_EPSILON: f32 = 0.01;
const LINE_FALLBACK_EPSILON: f32 = 1.0;

/// Returns the block-axis boundaries where a fragment tree should be split for a given
/// fragmentainer size.
///
/// The returned vector always starts at 0.0 and ends at the end of the content range (expanded to
/// at least one fragmentainer). When `fragmentainer_size` is non-positive, a single fragment
/// containing all content is implied.
pub fn resolve_fragmentation_boundaries(root: &FragmentNode, fragmentainer_size: f32) -> Vec<f32> {
  if fragmentainer_size <= 0.0 {
    return vec![0.0, root.logical_bounding_box().height()];
  }

  let total_height = root.logical_bounding_box().height().max(fragmentainer_size);
  let mut collection = BreakCollection::default();
  collect_break_opportunities(root, 0.0, &mut collection, 0, 0);
  collection.opportunities.push(BreakOpportunity {
    pos: total_height,
    strength: BreakStrength::Forced,
    kind: BreakKind::EndOfContent,
  });

  collection.opportunities.sort_by(|a, b| {
    a.pos
      .partial_cmp(&b.pos)
      .unwrap_or(std::cmp::Ordering::Equal)
  });
  collection.opportunities.dedup_by(|a, b| {
    (a.pos - b.pos).abs() < BREAK_EPSILON && a.kind == b.kind && a.strength == b.strength
  });

  let mut line_starts = vec![0; collection.line_containers.len()];
  let mut boundaries = vec![0.0];
  let mut start = 0.0;

  while start < total_height - BREAK_EPSILON {
    let next = select_next_boundary(
      start,
      fragmentainer_size,
      total_height,
      &collection.opportunities,
      &collection.line_containers,
      &mut line_starts,
    );
    debug_assert!(
      next + BREAK_EPSILON >= start,
      "boundaries must not move backwards"
    );
    if (next - start).abs() < BREAK_EPSILON {
      boundaries.push(total_height);
      break;
    }
    boundaries.push(next);
    start = next;
  }

  if total_height - *boundaries.last().unwrap_or(&0.0) > BREAK_EPSILON {
    boundaries.push(total_height);
  }

  boundaries.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
  boundaries.dedup_by(|a, b| (*a - *b).abs() < BREAK_EPSILON);
  boundaries
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

  let boundaries = resolve_fragmentation_boundaries(root, options.fragmentainer_size);

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

    if let Some(mut clipped) = clip_node(root, start, end, 0.0, start, index, fragment_count) {
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
) -> Option<FragmentNode> {
  let node_abs_start = parent_abs_start + node.bounds.y();
  let node_abs_end = node_abs_start + node.bounds.height();
  let node_bbox = node.logical_bounding_box();
  let node_bbox_abs_start = parent_abs_start + node_bbox.min_y();
  let node_bbox_abs_end = parent_abs_start + node_bbox.max_y();

  if node_bbox_abs_end <= fragment_start || node_bbox_abs_start >= fragment_end {
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

  if matches!(node.content, FragmentContent::Line { .. }) {
    // Line boxes are indivisible. If a break lands inside a line, move the whole
    // line to the fragment that starts within the line box (instead of letting
    // it overflow the fragment that ends mid-line).
    let overlaps = node_abs_end > fragment_start && node_abs_start < fragment_end;
    let fragment_starts_inside = fragment_start > node_abs_start && fragment_start < node_abs_end;
    let fragment_is_last = fragment_index + 1 == fragment_count;
    let fragment_contains_line_start =
      node_abs_start >= fragment_start && node_abs_start < fragment_end;
    let fully_contained = node_abs_start >= fragment_start && node_abs_end <= fragment_end;
    if !overlaps
      || (!fully_contained
        && !fragment_starts_inside
        && !(fragment_is_last && fragment_contains_line_start))
    {
      return None;
    }

    let line_y = clipped_abs_start - parent_clipped_abs_start;
    cloned.bounds = Rect::from_xywh(
      node.bounds.x(),
      line_y,
      node.bounds.width(),
      node.bounds.height(),
    );
    cloned.children = node.children.clone();
    return Some(cloned);
  }

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

fn collect_break_opportunities(
  node: &FragmentNode,
  abs_start: f32,
  collection: &mut BreakCollection,
  avoid_depth: usize,
  inline_depth: usize,
) {
  let default_style = default_style();
  let style = node
    .style
    .as_ref()
    .map(|s| s.as_ref())
    .unwrap_or(default_style);
  let inside_avoid = avoid_depth + usize::from(matches!(style.break_inside, BreakInside::Avoid));
  let inside_inline = inline_depth
    + usize::from(matches!(
      node.content,
      FragmentContent::Line { .. } | FragmentContent::Inline { .. }
    ));

  let mut line_positions: Vec<Option<(usize, f32)>> = vec![None; node.children.len()];
  let mut line_ends = Vec::new();
  for (idx, child) in node.children.iter().enumerate() {
    if matches!(child.content, FragmentContent::Line { .. }) {
      let line_start = abs_start + child.bounds.y();
      let line_end = line_start + child.bounds.height();
      line_ends.push(line_end);
      line_positions[idx] = Some((line_ends.len(), line_end));
    }
  }

  let line_container_id = if !line_ends.is_empty() {
    let container_id = collection.line_containers.len();
    collection.line_containers.push(LineContainer {
      id: container_id,
      line_ends: line_ends.clone(),
      widows: style.widows.max(1),
      orphans: style.orphans.max(1),
    });
    Some(container_id)
  } else {
    None
  };

  for (idx, child) in node.children.iter().enumerate() {
    let child_abs_start = abs_start + child.bounds.y();
    let child_abs_end = child_abs_start + child.bounds.height();
    let child_style = child
      .style
      .as_ref()
      .map(|s| s.as_ref())
      .unwrap_or(default_style);
    let next_style = node
      .children
      .get(idx + 1)
      .and_then(|c| c.style.as_ref())
      .map(|s| s.as_ref())
      .unwrap_or(default_style);

    if let (Some(container_id), Some((line_index_end, line_end))) =
      (line_container_id, line_positions[idx])
    {
      let mut strength = BreakStrength::Auto;
      if inside_avoid > 0 {
        strength = BreakStrength::Avoid;
      }
      collection.opportunities.push(BreakOpportunity {
        pos: line_end,
        strength,
        kind: BreakKind::LineBoundary {
          container_id,
          line_index_end,
        },
      });
    }

    if idx == 0 && !matches!(child_style.break_before, BreakBetween::Auto) {
      let mut strength = combine_breaks(BreakBetween::Auto, child_style.break_before);
      strength = apply_avoid_penalty(strength, inside_avoid > 0);
      if strength == BreakStrength::Auto
        && matches!(child.content, FragmentContent::Block { box_id: None })
      {
        strength = BreakStrength::Avoid;
      }
      collection.opportunities.push(BreakOpportunity {
        pos: child_abs_start,
        strength,
        kind: BreakKind::BetweenSiblings,
      });
    }

    collect_break_opportunities(
      child,
      child_abs_start,
      collection,
      inside_avoid,
      inside_inline,
    );

    let mut strength = combine_breaks(child_style.break_after, next_style.break_before);
    strength = apply_avoid_penalty(strength, inside_avoid > 0);
    if strength == BreakStrength::Auto
      && matches!(child.content, FragmentContent::Block { box_id: None })
    {
      strength = BreakStrength::Avoid;
    }
    let include_boundary = if inside_inline > 0 {
      strength != BreakStrength::Auto
    } else {
      match child.content {
        FragmentContent::Line { .. }
        | FragmentContent::Inline { .. }
        | FragmentContent::Text { .. } => strength != BreakStrength::Auto,
        _ => true,
      }
    };
    if include_boundary {
      collection.opportunities.push(BreakOpportunity {
        pos: child_abs_end,
        strength,
        kind: BreakKind::BetweenSiblings,
      });
    }
  }
}

pub(crate) fn collect_forced_boundaries(node: &FragmentNode, abs_start: f32) -> Vec<f32> {
  fn is_forced_page_break(between: BreakBetween) -> bool {
    matches!(
      between,
      BreakBetween::Always
        | BreakBetween::Page
        | BreakBetween::Left
        | BreakBetween::Right
        | BreakBetween::Recto
        | BreakBetween::Verso
    )
  }

  fn collect(
    node: &FragmentNode,
    abs_start: f32,
    forced: &mut Vec<f32>,
    default_style: &ComputedStyle,
  ) {
    for (idx, child) in node.children.iter().enumerate() {
      let child_abs_start = abs_start + child.bounds.y();
      let child_abs_end = child_abs_start + child.bounds.height();
      let child_style = child
        .style
        .as_ref()
        .map(|s| s.as_ref())
        .unwrap_or(default_style);
      let next_style = node
        .children
        .get(idx + 1)
        .and_then(|c| c.style.as_ref())
        .map(|s| s.as_ref())
        .unwrap_or(default_style);

      if idx == 0 && is_forced_page_break(child_style.break_before) {
        forced.push(child_abs_start);
      }

      if is_forced_page_break(child_style.break_after)
        || is_forced_page_break(next_style.break_before)
      {
        forced.push(child_abs_end);
      }

      collect(child, child_abs_start, forced, default_style);
    }
  }

  let default_style = default_style();
  let mut forced = Vec::new();
  collect(node, abs_start, &mut forced, default_style);
  forced
}

fn select_next_boundary(
  start: f32,
  fragmentainer: f32,
  total_height: f32,
  opportunities: &[BreakOpportunity],
  line_containers: &[LineContainer],
  line_starts: &mut [usize],
) -> f32 {
  let limit = (start + fragmentainer).min(total_height);

  if let Some(pos) = opportunities
    .iter()
    .filter(|o| matches!(o.strength, BreakStrength::Forced))
    .find(|o| o.pos > start + BREAK_EPSILON && o.pos <= limit + BREAK_EPSILON)
    .map(|o| o.pos.min(total_height))
  {
    update_line_starts(pos, line_containers, line_starts);
    return pos;
  }

  let mut best: Option<(ConstraintKey, u8, u8, f32)> = None;
  // Compare candidates by constraint satisfaction first (orphans, widows in continuations,
  // then future widows), then by strength penalty (Auto over Avoid), then by kind (between
  // siblings over line breaks), then by position.
  for opportunity in opportunities.iter() {
    if opportunity.pos <= start + BREAK_EPSILON {
      continue;
    }
    if opportunity.pos > limit + BREAK_EPSILON {
      break;
    }
    if matches!(opportunity.strength, BreakStrength::Forced) {
      continue;
    }

    let constraint_key = constraint_key_for(opportunity, line_containers, line_starts);
    let strength_penalty = match opportunity.strength {
      BreakStrength::Avoid => 1,
      _ => 0,
    };
    let kind_rank = match opportunity.kind {
      BreakKind::BetweenSiblings => 0,
      BreakKind::LineBoundary { .. } => 1,
      BreakKind::EndOfContent => 2,
    };

    match best {
      None => {
        best = Some((constraint_key, strength_penalty, kind_rank, opportunity.pos));
      }
      Some((best_key, best_penalty, best_kind, best_pos)) => {
        if constraint_key < best_key
          || (constraint_key == best_key && strength_penalty < best_penalty)
          || (constraint_key == best_key
            && strength_penalty == best_penalty
            && kind_rank < best_kind)
          || (constraint_key == best_key
            && strength_penalty == best_penalty
            && kind_rank == best_kind
            && opportunity.pos > best_pos + BREAK_EPSILON)
        {
          best = Some((constraint_key, strength_penalty, kind_rank, opportunity.pos));
        }
      }
    }
  }

  if let Some((_, _, _, pos)) = best {
    update_line_starts(pos, line_containers, line_starts);
    return pos.min(total_height);
  }

  let mut fallback = limit;
  if let Some(near_line) = opportunities.iter().find_map(|o| {
    if o.pos <= start + BREAK_EPSILON {
      return None;
    }
    if o.pos - limit > LINE_FALLBACK_EPSILON {
      return None;
    }
    match o.kind {
      BreakKind::LineBoundary { .. } if o.pos > limit => Some(o.pos),
      _ => None,
    }
  }) {
    fallback = near_line;
  }

  let clamped = fallback.min(total_height);
  let next = if clamped <= start + BREAK_EPSILON {
    (start + fragmentainer).min(total_height)
  } else {
    clamped
  };
  update_line_starts(next, line_containers, line_starts);
  next
}

fn constraint_key_for(
  opportunity: &BreakOpportunity,
  line_containers: &[LineContainer],
  line_starts: &[usize],
) -> ConstraintKey {
  if let BreakKind::LineBoundary {
    container_id,
    line_index_end,
  } = opportunity.kind
  {
    if let Some(container) = line_containers.get(container_id) {
      let start_line = line_starts
        .get(container_id)
        .copied()
        .unwrap_or(0)
        .min(line_index_end);
      let lines_in_fragment = line_index_end.saturating_sub(start_line);
      let remaining = container.line_ends.len().saturating_sub(line_index_end);
      let violates_orphans = remaining > 0 && lines_in_fragment < container.orphans;
      let violates_continuation_widows = start_line > 0 && lines_in_fragment < container.widows;
      let violates_future_widows = remaining > 0 && remaining < container.widows;
      return ConstraintKey {
        violates_orphans,
        violates_continuation_widows,
        violates_future_widows,
      };
    }
  }

  ConstraintKey {
    violates_orphans: false,
    violates_continuation_widows: false,
    violates_future_widows: false,
  }
}

fn update_line_starts(boundary: f32, line_containers: &[LineContainer], line_starts: &mut [usize]) {
  for container in line_containers {
    if let Some(slot) = line_starts.get_mut(container.id) {
      let completed = container
        .line_ends
        .iter()
        .take_while(|end| **end <= boundary + BREAK_EPSILON)
        .count();
      *slot = completed;
    }
  }
}

fn combine_breaks(after: BreakBetween, before: BreakBetween) -> BreakStrength {
  if matches!(
    after,
    BreakBetween::Always | BreakBetween::Page | BreakBetween::Column
  ) || matches!(
    before,
    BreakBetween::Always | BreakBetween::Page | BreakBetween::Column
  ) {
    return BreakStrength::Forced;
  }

  if matches!(after, BreakBetween::Avoid) || matches!(before, BreakBetween::Avoid) {
    return BreakStrength::Avoid;
  }

  BreakStrength::Auto
}

fn apply_avoid_penalty(strength: BreakStrength, inside_avoid: bool) -> BreakStrength {
  if inside_avoid && !matches!(strength, BreakStrength::Forced) {
    BreakStrength::Avoid
  } else {
    strength
  }
}

fn default_style() -> &'static ComputedStyle {
  static DEFAULT: OnceLock<ComputedStyle> = OnceLock::new();
  DEFAULT.get_or_init(ComputedStyle::default)
}
