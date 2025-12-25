//! Fragmentation utilities
//!
//! Pagination and multi-column output require splitting a laid-out fragment tree
//! into fragmentainers (pages/columns). Fragmentation happens in the block axis
//! and respects authored break hints (`break-before/after/inside`), widows/orphans
//! constraints, and line-level break opportunities. The fragment tree that comes
//! out of layout is treated as flow order; this module decides where to break and
//! clones the appropriate fragment subtrees for each fragmentainer.

use std::env;
use std::sync::OnceLock;

use crate::geometry::{Point, Rect};
use crate::style::types::WritingMode;
use crate::style::types::{BreakBetween, BreakInside};
use crate::style::ComputedStyle;
use crate::style::{block_axis_is_horizontal, block_axis_positive};
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OpportunityKind {
  BreakBefore,
  BreakAfter,
  ChildEnd,
  LineEnd,
  BreakInside,
  FinalContentEdge,
}

#[derive(Debug, Clone)]
struct BreakCandidate {
  pos: f32,
  strength: BreakStrength,
  kind: OpportunityKind,
  container_id: Option<usize>,
  details: Option<String>,
}

#[derive(Debug, Clone)]
struct ForbiddenRange {
  start: f32,
  end: f32,
  reason: ForbiddenReason,
  container_id: Option<usize>,
}

#[derive(Debug, Clone)]
enum ForbiddenReason {
  BreakBeforeAvoid,
  BreakAfterAvoid,
  BreakInsideAvoid,
  WidowsOrphans {
    line_index: usize,
    line_count: usize,
    widows: usize,
    orphans: usize,
  },
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct BlockAxis {
  pub(crate) horizontal: bool,
  positive: bool,
}

impl BlockAxis {
  fn new(writing_mode: WritingMode) -> Self {
    Self {
      horizontal: block_axis_is_horizontal(writing_mode),
      positive: block_axis_positive(writing_mode),
    }
  }

  pub(crate) fn block_size(&self, rect: &Rect) -> f32 {
    if self.horizontal {
      rect.width()
    } else {
      rect.height()
    }
  }

  pub(crate) fn block_offset_in_parent(&self, rect: &Rect, parent_block_size: f32) -> f32 {
    let start = if self.horizontal { rect.x() } else { rect.y() };
    let size = self.block_size(rect);
    if self.positive {
      start
    } else {
      parent_block_size - start - size
    }
  }

  fn set_block_position_and_size(
    &self,
    rect: &Rect,
    block_start: f32,
    block_size: f32,
    parent_block_size: f32,
  ) -> Rect {
    if self.horizontal {
      let x = if self.positive {
        block_start
      } else {
        parent_block_size - block_start - block_size
      };
      Rect::from_xywh(x, rect.y(), block_size, rect.height())
    } else {
      let y = if self.positive {
        block_start
      } else {
        parent_block_size - block_start - block_size
      };
      Rect::from_xywh(rect.x(), y, rect.width(), block_size)
    }
  }

  fn translate_along_block(&self, distance: f32) -> Point {
    let delta = if self.positive { distance } else { -distance };
    if self.horizontal {
      Point::new(delta, 0.0)
    } else {
      Point::new(0.0, delta)
    }
  }

  fn translate_along_inline(&self, distance: f32) -> Point {
    if self.horizontal {
      Point::new(0.0, distance)
    } else {
      Point::new(distance, 0.0)
    }
  }
}

pub(crate) fn fragmentation_axis(node: &FragmentNode) -> BlockAxis {
  let wm = node
    .style
    .as_ref()
    .map(|s| s.writing_mode)
    .unwrap_or(default_style().writing_mode);
  BlockAxis::new(wm)
}

#[derive(Default, Debug, Clone)]
pub(crate) struct BreakPlan {
  candidates: Vec<BreakCandidate>,
  forbidden: Vec<ForbiddenRange>,
}

#[derive(Debug, Clone)]
struct DebugOpportunity {
  pos: f32,
  strength: BreakStrength,
  kind: OpportunityKind,
  container_id: Option<usize>,
  details: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BoundaryReason {
  ForcedInRange,
  NaturalInRange,
  PreferredTarget,
  Overflowed,
  Fallback,
}

#[derive(Debug, Clone)]
struct DebugBoundary {
  start: f32,
  limit: f32,
  chosen: f32,
  reason: BoundaryReason,
  score_breakdown: String,
}

#[derive(Debug, Clone)]
struct DebugConstraintRelaxation {
  container_id: Option<usize>,
  from: String,
  to: String,
  reason: String,
}

#[derive(Debug, Clone)]
enum DebugEvent {
  Opportunity(DebugOpportunity),
  BoundaryChosen(DebugBoundary),
  ConstraintRelaxed(DebugConstraintRelaxation),
}

trait DebugSink {
  fn push(&mut self, event: DebugEvent);
}

impl DebugSink for Vec<DebugEvent> {
  fn push(&mut self, event: DebugEvent) {
    Vec::push(self, event);
  }
}

static TRACE_FRAGMENTATION: OnceLock<bool> = OnceLock::new();

fn trace_fragmentation_enabled() -> bool {
  *TRACE_FRAGMENTATION.get_or_init(|| env::var_os("FASTR_TRACE_FRAGMENTATION").is_some())
}

fn push_debug_event(debug: &mut Option<&mut dyn DebugSink>, event: DebugEvent) {
  if let Some(sink) = debug.as_deref_mut() {
    sink.push(event);
  }
}

fn fragment_debug_id(node: &FragmentNode) -> usize {
  node as *const FragmentNode as usize
}

fn describe_forbidden(reason: &ForbiddenReason) -> String {
  match reason {
    ForbiddenReason::BreakBeforeAvoid => "break-before: avoid".to_string(),
    ForbiddenReason::BreakAfterAvoid => "break-after: avoid".to_string(),
    ForbiddenReason::BreakInsideAvoid => "break-inside: avoid".to_string(),
    ForbiddenReason::WidowsOrphans {
      line_index,
      line_count,
      widows,
      orphans,
    } => format!(
      "widows/orphans (line {} of {}, widows={}, orphans={})",
      line_index + 1,
      line_count,
      widows,
      orphans
    ),
  }
}

fn format_container_id(id: Option<usize>) -> String {
  id.map(|v| format!("0x{v:x}"))
    .unwrap_or_else(|| "unknown".to_string())
}

fn print_fragmentation_trace(events: &[DebugEvent]) {
  if events.is_empty() {
    return;
  }
  eprintln!("== Fragmentation trace ==");
  for event in events {
    match event {
      DebugEvent::Opportunity(op) => {
        let container = format_container_id(op.container_id);
        eprintln!(
          "[opportunity] pos={:.2} kind={:?} strength={:?} container={} {}",
          op.pos, op.kind, op.strength, container, op.details
        );
      }
      DebugEvent::BoundaryChosen(boundary) => {
        eprintln!(
          "[boundary] start={:.2} limit={:.2} chosen={:.2} reason={:?} {}",
          boundary.start,
          boundary.limit,
          boundary.chosen,
          boundary.reason,
          boundary.score_breakdown
        );
      }
      DebugEvent::ConstraintRelaxed(relaxation) => {
        let container = format_container_id(relaxation.container_id);
        eprintln!(
          "[relaxed] container={} from={} to={} reason={}",
          container, relaxation.from, relaxation.to, relaxation.reason
        );
      }
    }
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

  let axis = fragmentation_axis(root);
  let inline_size = if axis.horizontal {
    root.bounds.height()
  } else {
    root.bounds.width()
  };
  let column_count = options.column_count.max(1);
  let column_gap = options.column_gap.max(0.0);
  let column_width = if column_count > 1 {
    let total_gap = (column_count.saturating_sub(1)) as f32 * column_gap;
    (inline_size - total_gap).max(0.0) / column_count as f32
  } else {
    inline_size
  };
  let total_block_size = axis
    .block_size(&root.bounding_box())
    .max(options.fragmentainer_size);
  let root_block_size = axis.block_size(&root.bounds);
  let mut plan = BreakPlan::default();
  let mut debug_events = if trace_fragmentation_enabled() {
    Some(Vec::new())
  } else {
    None
  };

  let boundaries = {
    let mut debug_sink: Option<&mut dyn DebugSink> =
      debug_events.as_mut().map(|v| v as &mut dyn DebugSink);

    collect_break_plan(
      root,
      0.0,
      root_block_size,
      &axis,
      &mut plan,
      &mut debug_sink,
    );
    // Always allow breaking at the end of the content range so the final fragment closes.
    plan.candidates.push(BreakCandidate {
      pos: total_block_size,
      strength: BreakStrength::Forced,
      kind: OpportunityKind::FinalContentEdge,
      container_id: Some(fragment_debug_id(root)),
      details: Some("end of content".to_string()),
    });
    push_debug_event(
      &mut debug_sink,
      DebugEvent::Opportunity(DebugOpportunity {
        pos: total_block_size,
        strength: BreakStrength::Forced,
        kind: OpportunityKind::FinalContentEdge,
        container_id: Some(fragment_debug_id(root)),
        details: "final content edge".to_string(),
      }),
    );

    plan.candidates.sort_by(|a, b| {
      a.pos
        .partial_cmp(&b.pos)
        .unwrap_or(std::cmp::Ordering::Equal)
    });
    plan
      .candidates
      .dedup_by(|a, b| (a.pos - b.pos).abs() < 0.01 && a.strength == b.strength);

    compute_boundaries(
      total_block_size,
      options.fragmentainer_size,
      &plan,
      &mut debug_sink,
    )
  };

  if let Some(events) = debug_events.as_ref() {
    print_fragmentation_trace(events);
  }
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

    let path = FragmentainerPath::new(index);
    if let Some(mut clipped) = clip_node(
      root,
      start,
      end,
      0.0,
      start,
      root_block_size,
      end - start,
      &axis,
      index,
      fragment_count,
      path,
    ) {
      if column_count > 1 {
        if axis.horizontal {
          clipped.bounds.size.height = column_width;
        } else {
          clipped.bounds.size.width = column_width;
        }
      }
      propagate_fragment_metadata(&mut clipped, index, fragment_count, path);

      // Translate fragments to account for fragmentainer gaps so downstream consumers
      // can reason about the absolute position of each fragmentainer stack. When multiple
      // columns are requested, place fragmentainers in reading order across columns then rows.
      let col = index % column_count;
      let row = index / column_count;
      let block_offset = row as f32 * (options.fragmentainer_size + options.fragmentainer_gap);
      let inline_offset = col as f32 * (column_width + column_gap);
      let block_delta = axis.translate_along_block(block_offset);
      let inline_delta = axis.translate_along_inline(inline_offset);
      let translated = clipped.translate(Point::new(
        block_delta.x + inline_delta.x,
        block_delta.y + inline_delta.y,
      ));

      fragments.push(translated);
    }
  }

  if fragments.is_empty() {
    fragments.push(root.clone());
  }

  fragments
}

pub(crate) fn propagate_fragment_metadata(
  node: &mut FragmentNode,
  index: usize,
  count: usize,
  path: FragmentainerPath,
) {
  let merged_path = path.inherit_from(&node.fragmentainer);

  node.fragment_index = index;
  node.fragment_count = count.max(1);
  node.fragmentainer = merged_path;
  node.fragmentainer_index = merged_path.flattened_index();
  for child in &mut node.children {
    propagate_fragment_metadata(child, index, count, path);
  }
}

pub(crate) fn clip_node(
  node: &FragmentNode,
  fragment_start: f32,
  fragment_end: f32,
  parent_abs_start: f32,
  parent_clipped_abs_start: f32,
  parent_block_size: f32,
  parent_clipped_block_size: f32,
  axis: &BlockAxis,
  fragment_index: usize,
  fragment_count: usize,
  fragmentainer: FragmentainerPath,
) -> Option<FragmentNode> {
  let node_block_start = axis.block_offset_in_parent(&node.bounds, parent_block_size);
  let node_block_size = axis.block_size(&node.bounds);
  let node_abs_start = parent_abs_start + node_block_start;
  let node_abs_end = node_abs_start + node_block_size;

  if node_abs_end <= fragment_start || node_abs_start >= fragment_end {
    return None;
  }

  let default_style = default_style();
  let style = node
    .style
    .as_ref()
    .map(|s| s.as_ref())
    .unwrap_or(default_style);
  let avoid_inside = matches!(style.break_inside, BreakInside::Avoid);
  let is_line = matches!(node.content, FragmentContent::Line { .. });
  let fragment_size = fragment_end - fragment_start;

  // Honor break-inside/line constraints by keeping the fragment intact within a single fragmentainer.
  if is_line && (node_abs_start < fragment_start || node_abs_end > fragment_end) {
    if node_abs_start >= fragment_start && node_abs_start < fragment_end {
      let mut cloned = clone_without_children(node);
      let merged_path = fragmentainer.inherit_from(&node.fragmentainer);
      cloned.bounds = Rect::from_xywh(
        node.bounds.x(),
        node_abs_start - parent_clipped_abs_start,
        node.bounds.width(),
        node.bounds.height(),
      );
      cloned.fragment_index = fragment_index;
      cloned.fragment_count = fragment_count.max(1);
      cloned.fragmentainer = merged_path;
      cloned.fragmentainer_index = merged_path.flattened_index();
      cloned.children = node.children.clone();
      return Some(cloned);
    }
    return None;
  }

  if avoid_inside
    && (node_abs_start < fragment_start || node_abs_end > fragment_end)
    && (node_abs_end - node_abs_start) <= fragment_size + 0.01
  {
    if (fragment_start..fragment_end).contains(&node_abs_start) {
      let mut cloned = clone_without_children(node);
      let merged_path = fragmentainer.inherit_from(&node.fragmentainer);
      cloned.fragment_index = fragment_index;
      cloned.fragment_count = fragment_count.max(1);
      cloned.fragmentainer = merged_path;
      cloned.fragmentainer_index = merged_path.flattened_index();
      cloned.children = node.children.clone();
      return Some(cloned);
    }
    return None;
  }

  let clipped_abs_start = node_abs_start.max(fragment_start);
  let clipped_abs_end = node_abs_end.min(fragment_end);
  let new_block_size = (clipped_abs_end - clipped_abs_start).max(0.0);
  let new_block_start = clipped_abs_start - parent_clipped_abs_start;

  let mut cloned = clone_without_children(node);
  let merged_path = fragmentainer.inherit_from(&node.fragmentainer);
  cloned.bounds = axis.set_block_position_and_size(
    &node.bounds,
    new_block_start,
    new_block_size,
    parent_clipped_block_size,
  );
  cloned.fragment_index = fragment_index;
  cloned.fragment_count = fragment_count.max(1);
  cloned.fragmentainer = merged_path;
  cloned.fragmentainer_index = merged_path.flattened_index();

  for child in &node.children {
    if let Some(child_clipped) = clip_node(
      child,
      fragment_start,
      fragment_end,
      node_abs_start,
      clipped_abs_start,
      node_block_size,
      new_block_size,
      axis,
      fragment_index,
      fragment_count,
      fragmentainer,
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
    fragmentainer: node.fragmentainer,
    scroll_overflow: node.scroll_overflow,
  }
}

fn normalize_break_plan(plan: &mut BreakPlan) {
  plan.candidates.sort_by(|a, b| {
    a.pos
      .partial_cmp(&b.pos)
      .unwrap_or(std::cmp::Ordering::Equal)
  });
  plan
    .candidates
    .dedup_by(|a, b| (a.pos - b.pos).abs() < 0.01 && a.strength == b.strength);
}

pub(crate) fn build_break_plan(
  root: &FragmentNode,
  abs_start: f32,
  total_block_size: f32,
) -> BreakPlan {
  let axis = fragmentation_axis(root);
  let root_block_size = axis.block_size(&root.bounds);
  let mut plan = BreakPlan::default();
  let mut debug: Option<&mut dyn DebugSink> = None;
  collect_break_plan(
    root,
    abs_start,
    root_block_size,
    &axis,
    &mut plan,
    &mut debug,
  );
  plan.candidates.push(BreakCandidate {
    pos: total_block_size,
    strength: BreakStrength::Forced,
    kind: OpportunityKind::FinalContentEdge,
    container_id: Some(fragment_debug_id(root)),
    details: Some("end of content".to_string()),
  });
  normalize_break_plan(&mut plan);
  plan
}

fn choose_boundary(
  start: f32,
  fragmentainer: f32,
  total_height: f32,
  plan: &BreakPlan,
) -> (
  f32,
  BoundaryReason,
  Option<f32>,
  Option<f32>,
  Option<f32>,
  Option<f32>,
) {
  const EPSILON: f32 = 0.01;
  let target = (start + fragmentainer).min(total_height);

  let forced_boundary = plan.candidates.iter().find_map(|candidate| {
    if candidate.strength == BreakStrength::Forced
      && candidate.pos > start + EPSILON
      && candidate.pos <= target + EPSILON
      && !is_forbidden(candidate.pos, &plan.forbidden)
    {
      Some(candidate.pos)
    } else {
      None
    }
  });

  let preferred_target = if !is_forbidden(target, &plan.forbidden) {
    Some(target)
  } else {
    None
  };

  let natural = plan.candidates.iter().rev().find_map(|candidate| {
    if candidate.pos > start + EPSILON
      && candidate.pos <= target + EPSILON
      && !is_forbidden(candidate.pos, &plan.forbidden)
    {
      Some(candidate.pos)
    } else {
      None
    }
  });

  let overflow_candidate = plan.candidates.iter().find_map(|candidate| {
    if candidate.pos > target + EPSILON && !is_forbidden(candidate.pos, &plan.forbidden) {
      Some(candidate.pos)
    } else {
      None
    }
  });

  let (boundary, reason) = if let Some(pos) = forced_boundary {
    (pos, BoundaryReason::ForcedInRange)
  } else if let Some(pos) = natural {
    (pos, BoundaryReason::NaturalInRange)
  } else if let Some(pos) = preferred_target {
    (pos, BoundaryReason::PreferredTarget)
  } else if let Some(pos) = overflow_candidate {
    (pos, BoundaryReason::Overflowed)
  } else {
    (target, BoundaryReason::Fallback)
  };

  (
    boundary.min(total_height),
    reason,
    forced_boundary,
    natural,
    preferred_target,
    overflow_candidate,
  )
}

pub(crate) fn select_fragmentainer_boundary(
  start: f32,
  fragmentainer: f32,
  total_height: f32,
  plan: &BreakPlan,
) -> f32 {
  choose_boundary(start, fragmentainer, total_height, plan).0
}

fn collect_break_plan(
  node: &FragmentNode,
  abs_start: f32,
  parent_block_size: f32,
  axis: &BlockAxis,
  plan: &mut BreakPlan,
  debug: &mut Option<&mut dyn DebugSink>,
) {
  let default_style = default_style();
  let style = node
    .style
    .as_ref()
    .map(|s| s.as_ref())
    .unwrap_or(default_style);

  let node_block_size = axis.block_size(&node.bounds);
  let node_abs_start = abs_start + axis.block_offset_in_parent(&node.bounds, parent_block_size);
  let abs_end = node_abs_start + node_block_size;
  let container_id = Some(fragment_debug_id(node));

  if forces_break(style.break_before) {
    let detail = debug
      .is_some()
      .then(|| format!("break-before={:?}", style.break_before));
    plan.candidates.push(BreakCandidate {
      pos: node_abs_start,
      strength: BreakStrength::Forced,
      kind: OpportunityKind::BreakBefore,
      container_id,
      details: detail.clone(),
    });
    if debug.is_some() {
      push_debug_event(
        debug,
        DebugEvent::Opportunity(DebugOpportunity {
          pos: node_abs_start,
          strength: BreakStrength::Forced,
          kind: OpportunityKind::BreakBefore,
          container_id,
          details: detail.unwrap_or_else(|| "break-before forces break".to_string()),
        }),
      );
    }
  } else if avoids_break(style.break_before) {
    let start = node_abs_start - 0.01;
    let end = node_abs_start + 0.01;
    plan.forbidden.push(ForbiddenRange {
      start,
      end,
      reason: ForbiddenReason::BreakBeforeAvoid,
      container_id,
    });
    if debug.is_some() {
      push_debug_event(
        debug,
        DebugEvent::Opportunity(DebugOpportunity {
          pos: node_abs_start,
          strength: BreakStrength::Avoid,
          kind: OpportunityKind::BreakBefore,
          container_id,
          details: format!("avoid break-before in range [{start:.2}, {end:.2}]"),
        }),
      );
    }
  }

  if matches!(style.break_inside, BreakInside::Avoid) {
    // Allow breaking exactly at the element boundaries while preventing inner breaks.
    let epsilon = 0.1;
    if abs_end - node_abs_start > epsilon * 2.0 {
      let start = node_abs_start + epsilon;
      let end = abs_end - epsilon;
      plan.forbidden.push(ForbiddenRange {
        start,
        end,
        reason: ForbiddenReason::BreakInsideAvoid,
        container_id,
      });
      if debug.is_some() {
        push_debug_event(
          debug,
          DebugEvent::Opportunity(DebugOpportunity {
            pos: node_abs_start,
            strength: BreakStrength::Avoid,
            kind: OpportunityKind::BreakInside,
            container_id,
            details: format!("avoid break-inside in range [{start:.2}, {end:.2}]"),
          }),
        );
      }
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
      let line_start = node_abs_start + axis.block_offset_in_parent(&line.bounds, node_block_size);
      let line_end = line_start + axis.block_size(&line.bounds);
      let lines_before = idx + 1;
      let lines_after = line_count.saturating_sub(lines_before);
      let allow_break = lines_before >= orphans && lines_after >= widows;
      if allow_break {
        let detail = debug.is_some().then(|| {
          format!(
            "line {} allows break (before={}, after={}, widows={}, orphans={})",
            idx + 1,
            lines_before,
            lines_after,
            widows,
            orphans
          )
        });
        plan.candidates.push(BreakCandidate {
          pos: line_end,
          strength: BreakStrength::Auto,
          kind: OpportunityKind::LineEnd,
          container_id,
          details: detail.clone(),
        });
        if debug.is_some() {
          push_debug_event(
            debug,
            DebugEvent::Opportunity(DebugOpportunity {
              pos: line_end,
              strength: BreakStrength::Auto,
              kind: OpportunityKind::LineEnd,
              container_id,
              details: detail
                .unwrap_or_else(|| format!("line {} allows break (widows/orphans ok)", idx + 1)),
            }),
          );
        }
      } else {
        plan.forbidden.push(ForbiddenRange {
          start: line_start,
          end: line_end,
          reason: ForbiddenReason::WidowsOrphans {
            line_index: idx,
            line_count,
            widows,
            orphans,
          },
          container_id,
        });
        if debug.is_some() {
          push_debug_event(
            debug,
            DebugEvent::Opportunity(DebugOpportunity {
              pos: line_start,
              strength: BreakStrength::Avoid,
              kind: OpportunityKind::LineEnd,
              container_id,
              details: format!(
                "widows/orphans forbid break on line {} ({} before, {} after, widows={}, orphans={})",
                idx + 1,
                lines_before,
                lines_after,
                widows,
                orphans
              ),
            }),
          );
        }
      }
    }
  }

  for child in &node.children {
    let child_abs_start =
      node_abs_start + axis.block_offset_in_parent(&child.bounds, node_block_size);
    let child_abs_end = child_abs_start + axis.block_size(&child.bounds);

    let child_is_line = matches!(child.content, FragmentContent::Line { .. });

    // Natural break opportunity after each child so pagination can split between siblings.
    if !child_is_line {
      let child_kind = match child.content {
        FragmentContent::Block { .. } => "block",
        FragmentContent::Inline { .. } => "inline",
        FragmentContent::Text { .. } => "text",
        FragmentContent::Replaced { .. } => "replaced",
        FragmentContent::Line { .. } => "line",
      };
      let detail = debug
        .is_some()
        .then(|| format!("after {} child", child_kind));
      plan.candidates.push(BreakCandidate {
        pos: child_abs_end,
        strength: BreakStrength::Auto,
        kind: OpportunityKind::ChildEnd,
        container_id: Some(fragment_debug_id(child)),
        details: detail.clone(),
      });
      if debug.is_some() {
        push_debug_event(
          debug,
          DebugEvent::Opportunity(DebugOpportunity {
            pos: child_abs_end,
            strength: BreakStrength::Auto,
            kind: OpportunityKind::ChildEnd,
            container_id: Some(fragment_debug_id(child)),
            details: detail.unwrap_or_else(|| "after child".to_string()),
          }),
        );
      }
    }

    collect_break_plan(child, child_abs_start, node_block_size, axis, plan, debug);
  }

  if forces_break(style.break_after) {
    let detail = debug
      .is_some()
      .then(|| format!("break-after={:?}", style.break_after));
    plan.candidates.push(BreakCandidate {
      pos: abs_end,
      strength: BreakStrength::Forced,
      kind: OpportunityKind::BreakAfter,
      container_id,
      details: detail.clone(),
    });
    if debug.is_some() {
      push_debug_event(
        debug,
        DebugEvent::Opportunity(DebugOpportunity {
          pos: abs_end,
          strength: BreakStrength::Forced,
          kind: OpportunityKind::BreakAfter,
          container_id,
          details: detail.unwrap_or_else(|| "break-after forces break".to_string()),
        }),
      );
    }
  } else if avoids_break(style.break_after) {
    let start = abs_end - 0.01;
    let end = abs_end + 0.01;
    plan.forbidden.push(ForbiddenRange {
      start,
      end,
      reason: ForbiddenReason::BreakAfterAvoid,
      container_id,
    });
    if debug.is_some() {
      push_debug_event(
        debug,
        DebugEvent::Opportunity(DebugOpportunity {
          pos: abs_end,
          strength: BreakStrength::Avoid,
          kind: OpportunityKind::BreakAfter,
          container_id,
          details: format!("avoid break-after in range [{start:.2}, {end:.2}]"),
        }),
      );
    }
  }
}

pub(crate) fn collect_forced_boundaries(node: &FragmentNode, abs_start: f32) -> Vec<f32> {
  let axis = fragmentation_axis(node);
  let total_block_size = axis.block_size(&node.bounding_box());
  let plan = build_break_plan(node, abs_start, total_block_size);
  plan
    .candidates
    .into_iter()
    .filter(|c| c.strength == BreakStrength::Forced)
    .map(|c| c.pos)
    .collect()
}

fn compute_boundaries(
  total_height: f32,
  fragmentainer: f32,
  plan: &BreakPlan,
  debug: &mut Option<&mut dyn DebugSink>,
) -> Vec<f32> {
  const EPSILON: f32 = 0.01;

  let mut boundaries = vec![0.0];
  let mut start = 0.0;

  while start + fragmentainer < total_height - EPSILON {
    let target = (start + fragmentainer).min(total_height);
    let (boundary, reason, forced_boundary, natural, preferred_target, overflow_candidate) =
      choose_boundary(start, fragmentainer, total_height, plan);
    let clamped = boundary.min(total_height);
    if (clamped - start).abs() < EPSILON {
      // Give up on tiny slices to avoid infinite loops.
      if debug.is_some() {
        push_debug_event(
          debug,
          DebugEvent::BoundaryChosen(DebugBoundary {
            start,
            limit: target,
            chosen: total_height,
            reason: BoundaryReason::Fallback,
            score_breakdown: "aborting tiny slice; forcing final boundary".to_string(),
          }),
        );
      }
      boundaries.push(total_height);
      break;
    }

    if debug.is_some() {
      let score_breakdown = format!(
        "forced_in_window={} natural_in_window={} preferred_target={} overflow_candidate={}",
        forced_boundary
          .map(|p| format!("{p:.2}"))
          .unwrap_or_else(|| "none".to_string()),
        natural
          .map(|p| format!("{p:.2}"))
          .unwrap_or_else(|| "none".to_string()),
        preferred_target
          .map(|p| format!("{p:.2}"))
          .unwrap_or_else(|| "forbidden".to_string()),
        overflow_candidate
          .map(|p| format!("{p:.2}"))
          .unwrap_or_else(|| "none".to_string())
      );
      push_debug_event(
        debug,
        DebugEvent::BoundaryChosen(DebugBoundary {
          start,
          limit: target,
          chosen: clamped,
          reason,
          score_breakdown,
        }),
      );
    }

    if let Some(range) = matching_forbidden(clamped, &plan.forbidden) {
      if debug.is_some() {
        push_debug_event(
          debug,
          DebugEvent::ConstraintRelaxed(DebugConstraintRelaxation {
            container_id: range.container_id,
            from: describe_forbidden(&range.reason),
            to: format!("break at {:.2}", clamped),
            reason: format!("no valid break ≤ {:.2}; using {:?}", target, reason),
          }),
        );
      }
    }

    boundaries.push(clamped);
    start = clamped;
  }

  if total_height - *boundaries.last().unwrap_or(&0.0) > EPSILON {
    let start = *boundaries.last().unwrap_or(&0.0);
    if debug.is_some() {
      push_debug_event(
        debug,
        DebugEvent::BoundaryChosen(DebugBoundary {
          start,
          limit: total_height,
          chosen: total_height,
          reason: BoundaryReason::Fallback,
          score_breakdown: "closing trailing content".to_string(),
        }),
      );
    }
    if let Some(range) = matching_forbidden(total_height, &plan.forbidden) {
      if debug.is_some() {
        push_debug_event(
          debug,
          DebugEvent::ConstraintRelaxed(DebugConstraintRelaxation {
            container_id: range.container_id,
            from: describe_forbidden(&range.reason),
            to: format!("break at {:.2}", total_height),
            reason: "closing at content end".to_string(),
          }),
        );
      }
    }
    boundaries.push(total_height);
  }

  boundaries.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
  boundaries.dedup_by(|a, b| (*a - *b).abs() < EPSILON);
  boundaries
}

fn matching_forbidden<'a>(pos: f32, forbidden: &'a [ForbiddenRange]) -> Option<&'a ForbiddenRange> {
  forbidden
    .iter()
    .find(|range| pos >= range.start - 0.01 && pos <= range.end + 0.01)
}

fn is_forbidden(pos: f32, forbidden: &[ForbiddenRange]) -> bool {
  matching_forbidden(pos, forbidden).is_some()
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

#[cfg(test)]
mod tests {
  use super::*;
  use crate::tree::fragment_tree::FragmentContent;
  use std::sync::Arc;

  #[test]
  fn fragments_use_block_axis_for_vertical_writing_mode() {
    let mut vertical_style = ComputedStyle::default();
    vertical_style.writing_mode = WritingMode::VerticalRl;
    let style = Arc::new(vertical_style);

    let child1 =
      FragmentNode::new_block_with_id(Rect::from_xywh(120.0, 0.0, 120.0, 50.0), 1, vec![]);
    let child2 = FragmentNode::new_block_with_id(Rect::from_xywh(0.0, 0.0, 120.0, 50.0), 2, vec![]);
    let root = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 240.0, 50.0),
      vec![child1, child2],
      style,
    );

    let fragments = fragment_tree(&root, &FragmentationOptions::new(120.0));
    assert_eq!(fragments.len(), 2);

    let first = &fragments[0];
    let second = &fragments[1];

    assert_eq!(first.bounds.width(), 120.0);
    assert_eq!(second.bounds.width(), 120.0);
    assert_eq!(first.bounds.height(), 50.0);
    assert_eq!(second.bounds.height(), 50.0);
    assert_eq!(first.bounds.y(), 0.0);
    assert_eq!(second.bounds.y(), 0.0);
    assert_ne!(first.bounds.x(), second.bounds.x());

    assert_eq!(first.children.len(), 1);
    assert_eq!(second.children.len(), 1);

    match first.children[0].content {
      FragmentContent::Block { box_id: Some(id) } => assert_eq!(id, 1),
      _ => panic!("expected first fragment to contain the first child block"),
    }
    match second.children[0].content {
      FragmentContent::Block { box_id: Some(id) } => assert_eq!(id, 2),
      _ => panic!("expected second fragment to contain the second child block"),
    }
  }
}
