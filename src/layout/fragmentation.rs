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
use crate::layout::axis::{FragmentAxes, PhysicalAxis};
use crate::style::display::Display;
use crate::style::page::PageSide;
use crate::style::types::{BreakBetween, BreakInside};
use crate::style::ComputedStyle;
use crate::tree::fragment_tree::{
  FragmentContent, FragmentNode, FragmentSliceInfo, FragmentainerPath,
};

/// The fragmentation context determines how break hints are interpreted.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FragmentationContext {
  /// Fragmentation across pages.
  Page,
  /// Fragmentation across columns.
  Column,
}

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

/// Computes the inline size available to each column when fragmenting into multiple columns.
///
/// The result subtracts total inter-column gaps from the initial containing block width and
/// divides the remainder evenly across columns. A minimum of one column is enforced to avoid
/// division by zero when callers construct `FragmentationOptions` manually.
pub fn column_inline_size(icb_width: f32, options: &FragmentationOptions) -> f32 {
  let count = options.column_count.max(1) as f32;
  if count <= 1.0 {
    return icb_width;
  }

  let gaps = options.column_gap.max(0.0) * (count - 1.0);
  let available = (icb_width - gaps).max(0.0);
  available / count
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

#[derive(Debug, Clone, Copy)]
pub(crate) struct AtomicRange {
  pub(crate) start: f32,
  pub(crate) end: f32,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct ForcedBoundary {
  pub position: f32,
  pub page_side: Option<PageSide>,
}

#[derive(Default, Debug)]
struct BreakCollection {
  opportunities: Vec<BreakOpportunity>,
  line_containers: Vec<LineContainer>,
  atomic: Vec<AtomicRange>,
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
  resolve_fragmentation_boundaries_with_context(
    root,
    fragmentainer_size,
    FragmentationContext::Page,
  )
}

fn resolve_fragmentation_boundaries_impl(
  root: &FragmentNode,
  fragmentainer_size: f32,
  context: FragmentationContext,
) -> Vec<f32> {
  if fragmentainer_size <= 0.0 {
    return vec![0.0, root.logical_bounding_box().height()];
  }

  let total_height = root.logical_bounding_box().height().max(fragmentainer_size);
  let mut collection = BreakCollection::default();
  collect_break_opportunities(root, 0.0, &mut collection, 0, 0, context);
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
  normalize_atomic_ranges(&mut collection.atomic);

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
      &collection.atomic,
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

pub fn resolve_fragmentation_boundaries_with_context(
  root: &FragmentNode,
  fragmentainer_size: f32,
  context: FragmentationContext,
) -> Vec<f32> {
  resolve_fragmentation_boundaries_impl(root, fragmentainer_size, context)
}

/// Resolves fragmentation boundaries while honoring the provided writing mode axes.
///
/// For the common horizontal-tb writing mode this defers to the primary pagination algorithm; other
/// writing modes currently reuse the same block-axis interpretation, which matches the existing
/// fragment geometry returned by layout.
pub fn resolve_fragmentation_boundaries_with_axes(
  root: &FragmentNode,
  fragmentainer_size: f32,
  context: FragmentationContext,
  axes: FragmentAxes,
) -> Vec<f32> {
  if axes.block_axis() == PhysicalAxis::Y && axes.block_positive() {
    return resolve_fragmentation_boundaries_impl(root, fragmentainer_size, context);
  }

  resolve_fragmentation_boundaries_impl(root, fragmentainer_size, context)
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
  let column_count = options.column_count.max(1);
  let column_step = root.bounds.width() + options.column_gap;
  let fragment_step = options.fragmentainer_size + options.fragmentainer_gap;
  let mut fragments = Vec::with_capacity(fragment_count);

  for (index, window) in boundaries.windows(2).enumerate() {
    let start = window[0];
    let end = window[1];
    if end <= start {
      continue;
    }

    if let Some(mut clipped) = clip_node(root, start, end, 0.0, start, index, fragment_count) {
      normalize_fragment_margins(&mut clipped, index == 0, index + 1 == fragment_count);
      propagate_fragment_metadata(&mut clipped, index, fragment_count);

      // Translate fragments to account for fragmentainer gaps so downstream consumers
      // can reason about the absolute position of each fragmentainer stack. When
      // columns are requested, fragments are distributed left-to-right before
      // stacking additional rows vertically.
      let column = index % column_count;
      let row = index / column_count;
      let offset = Point::new(column as f32 * column_step, row as f32 * fragment_step);
      let translated = clipped.translate(offset);
      fragments.push(translated);
    }
  }

  if fragments.is_empty() {
    fragments.push(root.clone());
  }

  fragments
}

/// Fragment a laid-out tree while respecting the provided writing mode.
pub fn fragment_tree_for_writing_mode(
  root: &FragmentNode,
  options: &FragmentationOptions,
  writing_mode: crate::style::types::WritingMode,
  direction: crate::style::types::Direction,
) -> Vec<FragmentNode> {
  let axes = FragmentAxes::from_writing_mode_and_direction(writing_mode, direction);
  if axes.block_axis() == PhysicalAxis::Y && axes.block_positive() {
    return fragment_tree(root, options);
  }

  fragment_tree(root, options)
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

  let default_style = default_style();
  let style = node
    .style
    .as_ref()
    .map(|s| s.as_ref())
    .unwrap_or(default_style);
  let is_table_row_like = matches!(
    style.display,
    Display::TableRow
      | Display::TableRowGroup
      | Display::TableHeaderGroup
      | Display::TableFooterGroup
  );
  let mut avoid_inside = matches!(style.break_inside, BreakInside::Avoid) || is_table_row_like;
  if avoid_inside && node.bounds.height() > (fragment_end - fragment_start) + 0.01 {
    avoid_inside = false;
  }

  // Honor break-inside/line constraints by keeping the fragment intact within a single fragmentainer.
  let node_overlaps = node_abs_end > fragment_start && node_abs_start < fragment_end;
  if avoid_inside
    && node_overlaps
    && (node_abs_start < fragment_start || node_abs_end > fragment_end)
  {
    if (fragment_start..fragment_end).contains(&node_abs_start) {
      let mut cloned = clone_without_children(node);
      let new_y = node_abs_start - parent_clipped_abs_start;
      cloned.bounds = Rect::from_xywh(
        node.bounds.x(),
        new_y,
        node.bounds.width(),
        node.bounds.height(),
      );
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
  const CLIP_EPSILON: f32 = 0.01;
  if let Some(meta) = cloned.block_metadata.as_mut() {
    meta.clipped_top = node_abs_start < fragment_start + CLIP_EPSILON;
    meta.clipped_bottom = node_abs_end > fragment_end - CLIP_EPSILON;
  }
  cloned.bounds = Rect::from_xywh(node.bounds.x(), new_y, node.bounds.width(), new_height);
  cloned.fragment_index = fragment_index;
  cloned.fragment_count = fragment_count.max(1);
  cloned.fragmentainer_index = fragment_index;
  let original_block_size = node
    .slice_info
    .original_block_size
    .max((node_abs_end - node_abs_start).max(0.0))
    .max(node.bounds.height());
  let base_offset = node.slice_info.slice_offset.max(0.0);
  let slice_offset = base_offset + (clipped_abs_start - node_abs_start).max(0.0);
  let slice_end_offset = base_offset + (clipped_abs_end - node_abs_start).max(0.0);
  let epsilon = 0.01;
  cloned.slice_info = FragmentSliceInfo {
    is_first: slice_offset <= epsilon,
    is_last: slice_end_offset >= original_block_size - epsilon,
    slice_offset: slice_offset.min(original_block_size),
    original_block_size,
  };

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

  if matches!(style.display, Display::Table | Display::InlineTable) {
    inject_table_headers_and_footers(node, &mut cloned, fragment_index, fragment_count);
  }

  Some(cloned)
}

/// Axis-aware wrapper around [`clip_node`]. The fragmentation algorithm currently operates in
/// physical block progression; for the common horizontal-tb writing mode this defers to the
/// primary implementation and otherwise falls back to the same logic.
pub(crate) fn clip_node_with_axes(
  node: &FragmentNode,
  fragment_start: f32,
  fragment_end: f32,
  parent_abs_start: f32,
  parent_clipped_abs_start: f32,
  _parent_block_size: f32,
  axes: FragmentAxes,
  fragment_index: usize,
  fragment_count: usize,
) -> Option<FragmentNode> {
  if axes.block_axis() == PhysicalAxis::Y && axes.block_positive() {
    return clip_node(
      node,
      fragment_start,
      fragment_end,
      parent_abs_start,
      parent_clipped_abs_start,
      fragment_index,
      fragment_count,
    );
  }

  clip_node(
    node,
    fragment_start,
    fragment_end,
    parent_abs_start,
    parent_clipped_abs_start,
    fragment_index,
    fragment_count,
  )
}

fn clone_without_children(node: &FragmentNode) -> FragmentNode {
  FragmentNode {
    bounds: node.bounds,
    block_metadata: node.block_metadata.clone(),
    logical_override: node.logical_override,
    content: node.content.clone(),
    baseline: node.baseline,
    children: Vec::new(),
    style: node.style.clone(),
    starting_style: node.starting_style.clone(),
    fragment_index: node.fragment_index,
    fragment_count: node.fragment_count,
    fragmentainer_index: node.fragmentainer_index,
    fragmentainer: node.fragmentainer,
    slice_info: node.slice_info,
    scroll_overflow: node.scroll_overflow,
    fragmentation: node.fragmentation.clone(),
  }
}

fn is_table_header_fragment(node: &FragmentNode) -> bool {
  node
    .style
    .as_ref()
    .is_some_and(|s| matches!(s.display, Display::TableHeaderGroup))
}

fn is_table_footer_fragment(node: &FragmentNode) -> bool {
  node
    .style
    .as_ref()
    .is_some_and(|s| matches!(s.display, Display::TableFooterGroup))
}

fn inject_table_headers_and_footers(
  original: &FragmentNode,
  clipped: &mut FragmentNode,
  fragment_index: usize,
  fragment_count: usize,
) {
  let headers: Vec<_> = original
    .children
    .iter()
    .filter(|c| is_table_header_fragment(c))
    .collect();
  let footers: Vec<_> = original
    .children
    .iter()
    .filter(|c| is_table_footer_fragment(c))
    .collect();
  if headers.is_empty() && footers.is_empty() {
    return;
  }

  let has_header = clipped.children.iter().any(is_table_header_fragment);
  let has_footer = clipped.children.iter().any(is_table_footer_fragment);

  let mut max_y = clipped.bounds.height();

  if !headers.is_empty() && (!has_header || fragment_index > 0) {
    let mut regions = Vec::new();
    for header in &headers {
      regions.push((header.bounds.y(), header.bounds.max_y()));
    }
    let region_height: f32 = regions.iter().map(|(s, e)| e - s).sum();
    for child in &mut clipped.children {
      child.bounds = child.bounds.translate(Point::new(0.0, region_height));
    }
    let mut offset = 0.0;
    let mut clones = Vec::new();
    for (start, end) in regions {
      for candidate in &original.children {
        let Some(style) = candidate.style.as_ref() else {
          continue;
        };
        if !matches!(
          style.display,
          Display::TableHeaderGroup
            | Display::TableFooterGroup
            | Display::TableRowGroup
            | Display::TableRow
            | Display::TableCell
        ) {
          continue;
        }
        let c_start = candidate.bounds.y();
        let c_end = candidate.bounds.max_y();
        if c_start + 0.01 >= start && c_end <= end + 0.01 {
          let mut clone = candidate.clone();
          clone.bounds = clone.bounds.translate(Point::new(0.0, offset - start));
          propagate_fragment_metadata(&mut clone, fragment_index, fragment_count);
          clones.push(clone);
        }
      }
      offset += end - start;
    }
    max_y = max_y.max(offset);
    clipped.children.splice(0..0, clones);
  }

  if !footers.is_empty() && (!has_footer || fragment_index + 1 < fragment_count) {
    let mut regions = Vec::new();
    for footer in &footers {
      regions.push((footer.bounds.y(), footer.bounds.max_y()));
    }
    let mut footer_offset = clipped
      .children
      .iter()
      .map(|c| c.bounds.max_y())
      .fold(0.0, f32::max);
    let mut clones = Vec::new();
    for (start, end) in regions {
      for candidate in &original.children {
        let Some(style) = candidate.style.as_ref() else {
          continue;
        };
        if !matches!(
          style.display,
          Display::TableHeaderGroup
            | Display::TableFooterGroup
            | Display::TableRowGroup
            | Display::TableRow
            | Display::TableCell
        ) {
          continue;
        }
        let c_start = candidate.bounds.y();
        let c_end = candidate.bounds.max_y();
        if c_start + 0.01 >= start && c_end <= end + 0.01 {
          let mut clone = candidate.clone();
          clone.bounds = clone
            .bounds
            .translate(Point::new(0.0, footer_offset - start));
          footer_offset += clone.bounds.height();
          propagate_fragment_metadata(&mut clone, fragment_index, fragment_count);
          clones.push(clone);
        }
      }
    }
    max_y = max_y.max(footer_offset);
    clipped.children.extend(clones);
  }

  let children_bottom = clipped
    .children
    .iter()
    .map(|c| c.bounds.max_y())
    .fold(0.0, f32::max);
  let new_height = clipped.bounds.height().max(max_y).max(children_bottom);
  clipped.bounds = Rect::from_xywh(
    clipped.bounds.x(),
    clipped.bounds.y(),
    clipped.bounds.width(),
    new_height,
  );
  clipped.scroll_overflow = Rect::from_xywh(
    clipped.scroll_overflow.x(),
    clipped.scroll_overflow.y(),
    clipped.scroll_overflow.width().max(clipped.bounds.width()),
    new_height.max(clipped.scroll_overflow.height()),
  );
}

fn translate_fragment_in_parent_space(node: &mut FragmentNode, offset: Point) {
  node.bounds = node.bounds.translate(offset);
  if let Some(logical) = node.logical_override {
    node.logical_override = Some(logical.translate(offset));
  }
}

pub(crate) fn normalize_fragment_margins(
  fragment: &mut FragmentNode,
  is_first_fragment: bool,
  is_last_fragment: bool,
) {
  const EPSILON: f32 = 0.01;

  // Reset carried collapsed margin from previous fragmentainer by reapplying the fragment's own
  // top margin to the first block that starts this slice.
  if !is_first_fragment {
    if let Some(min_y) = fragment
      .children
      .iter()
      .map(|c| c.bounds.y())
      .min_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
    {
      for child in fragment
        .children
        .iter_mut()
        .filter(|c| (c.bounds.y() - min_y).abs() < EPSILON)
      {
        if let Some(meta) = child.block_metadata.as_ref() {
          if meta.clipped_top {
            continue;
          }
          let desired_top = meta.margin_top;
          let delta = desired_top - child.bounds.y();
          if delta.abs() > EPSILON {
            translate_fragment_in_parent_space(child, Point::new(0.0, delta));
          }
        }
      }
    }
  }

  // Include the trailing margin of the last complete block when this slice is not the final one.
  if !is_last_fragment {
    if let Some((max_y, meta)) = fragment
      .children
      .iter()
      .filter_map(|c| c.block_metadata.as_ref().map(|m| (c.bounds.max_y(), m)))
      .max_by(|a, b| a.0.partial_cmp(&b.0).unwrap_or(std::cmp::Ordering::Equal))
    {
      let target_height = if meta.clipped_bottom {
        max_y
      } else {
        max_y + meta.margin_bottom
      };
      let new_height = fragment.bounds.height().max(target_height);
      fragment.bounds = Rect::from_xywh(
        fragment.bounds.x(),
        fragment.bounds.y(),
        fragment.bounds.width(),
        new_height,
      );
      fragment.scroll_overflow = Rect::from_xywh(
        fragment.scroll_overflow.x(),
        fragment.scroll_overflow.y(),
        fragment.scroll_overflow.width(),
        fragment.scroll_overflow.height().max(new_height),
      );
    }
  }
}

/// Axis-aware wrapper for [`normalize_fragment_margins`].
pub(crate) fn normalize_fragment_margins_with_axes(
  fragment: &mut FragmentNode,
  is_first_fragment: bool,
  is_last_fragment: bool,
  _parent_block_size: f32,
  axes: FragmentAxes,
) {
  if axes.block_axis() == PhysicalAxis::Y && axes.block_positive() {
    normalize_fragment_margins(fragment, is_first_fragment, is_last_fragment);
    return;
  }

  normalize_fragment_margins(fragment, is_first_fragment, is_last_fragment);
}

fn collect_break_opportunities(
  node: &FragmentNode,
  abs_start: f32,
  collection: &mut BreakCollection,
  avoid_depth: usize,
  inline_depth: usize,
  context: FragmentationContext,
) {
  let default_style = default_style();
  let style = node
    .style
    .as_ref()
    .map(|s| s.as_ref())
    .unwrap_or(default_style);
  let is_table_row_like = matches!(
    style.display,
    Display::TableRow
      | Display::TableRowGroup
      | Display::TableHeaderGroup
      | Display::TableFooterGroup
  );
  let inside_avoid = avoid_depth
    + usize::from(avoids_break_inside(style.break_inside, context))
    + usize::from(is_table_row_like);
  let inside_inline = inline_depth
    + usize::from(matches!(
      node.content,
      FragmentContent::Line { .. } | FragmentContent::Inline { .. }
    ));

  let abs_end = abs_start + node.bounds.height();
  if style.float.is_floating() {
    collection.atomic.push(AtomicRange {
      start: abs_start,
      end: abs_end,
    });
  }

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
    let next_child = node.children.get(idx + 1);
    let next_style = next_child
      .and_then(|c| c.style.as_ref())
      .map(|s| s.as_ref())
      .unwrap_or(default_style);
    let next_abs_start = next_child.map(|next| abs_start + next.bounds.y());

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
      let mut strength = combine_breaks(BreakBetween::Auto, child_style.break_before, context);
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
      context,
    );

    let mut strength = combine_breaks(child_style.break_after, next_style.break_before, context);
    strength = apply_avoid_penalty(strength, inside_avoid > 0);
    if strength == BreakStrength::Auto
      && matches!(child.content, FragmentContent::Block { box_id: None })
    {
      strength = BreakStrength::Avoid;
    }
    let mut boundary_pos = child_abs_end;
    if matches!(strength, BreakStrength::Forced) {
      if let Some(meta) = child.block_metadata.as_ref() {
        let mut candidate = child_abs_end + meta.margin_bottom;
        if candidate < child_abs_end {
          candidate = child_abs_end;
        }
        if let Some(next_start) = next_abs_start {
          candidate = candidate.min(next_start);
        }
        boundary_pos = candidate;
      }
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
        pos: boundary_pos,
        strength,
        kind: BreakKind::BetweenSiblings,
      });
    }
  }
}

pub(crate) fn collect_forced_boundaries(
  node: &FragmentNode,
  abs_start: f32,
) -> Vec<ForcedBoundary> {
  collect_forced_boundaries_with_axes(node, abs_start, FragmentAxes::default())
}

pub(crate) fn collect_forced_boundaries_with_axes(
  node: &FragmentNode,
  abs_start: f32,
  axes: FragmentAxes,
) -> Vec<ForcedBoundary> {
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

  fn break_side_hint(between: BreakBetween) -> Option<PageSide> {
    match between {
      BreakBetween::Left | BreakBetween::Verso => Some(PageSide::Left),
      BreakBetween::Right | BreakBetween::Recto => Some(PageSide::Right),
      _ => None,
    }
  }

  fn collect(
    node: &FragmentNode,
    abs_start: f32,
    forced: &mut Vec<ForcedBoundary>,
    default_style: &ComputedStyle,
    axes: FragmentAxes,
  ) {
    let node_block_size = axes.block_size(&node.bounds);
    for (idx, child) in node.children.iter().enumerate() {
      let child_abs_start = abs_start + axes.block_start(&child.bounds, node_block_size);
      let child_abs_end = child_abs_start + axes.block_size(&child.bounds);
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
        forced.push(ForcedBoundary {
          position: child_abs_start,
          page_side: break_side_hint(child_style.break_before),
        });
      }

      if is_forced_page_break(child_style.break_after)
        || is_forced_page_break(next_style.break_before)
      {
        let mut boundary = child_abs_end;
        if let Some(meta) = child.block_metadata.as_ref() {
          let mut candidate = child_abs_end + meta.margin_bottom;
          if candidate < child_abs_end {
            candidate = child_abs_end;
          }
          if let Some(next_child) = node.children.get(idx + 1) {
            let next_start = abs_start + axes.block_start(&next_child.bounds, node_block_size);
            candidate = candidate.min(next_start);
          }
          boundary = candidate;
        }
        forced.push(ForcedBoundary {
          position: boundary,
          page_side: break_side_hint(next_style.break_before)
            .or(break_side_hint(child_style.break_after)),
        });
      }

      collect(child, child_abs_start, forced, default_style, axes);
    }
  }

  let default_style = default_style();
  let mut boundaries = Vec::new();
  collect(node, abs_start, &mut boundaries, default_style, axes);
  boundaries
}

fn select_next_boundary(
  start: f32,
  fragmentainer: f32,
  total_height: f32,
  opportunities: &[BreakOpportunity],
  line_containers: &[LineContainer],
  line_starts: &mut [usize],
  atomic: &[AtomicRange],
) -> f32 {
  let mut limit = (start + fragmentainer).min(total_height);

  if let Some(range) = atomic_containing(start, atomic) {
    let boundary = range.end.min(total_height);
    if boundary <= start + BREAK_EPSILON {
      update_line_starts(total_height, line_containers, line_starts);
      return total_height;
    }
    update_line_starts(boundary, line_containers, line_starts);
    return boundary;
  }

  if let Some(range) = next_atomic_in_range(start, limit, atomic) {
    if range.start > start + BREAK_EPSILON {
      limit = limit.min(range.start);
    }
  }

  if let Some(pos) = opportunities
    .iter()
    .filter(|o| matches!(o.strength, BreakStrength::Forced))
    .filter(|o| !pos_is_inside_atomic(o.pos, atomic))
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
    if pos_is_inside_atomic(opportunity.pos, atomic) {
      continue;
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
    if pos_is_inside_atomic(o.pos, atomic) {
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

fn pos_is_inside_atomic(pos: f32, atomic: &[AtomicRange]) -> bool {
  atomic
    .iter()
    .any(|range| pos > range.start + BREAK_EPSILON && pos < range.end - BREAK_EPSILON)
}

fn atomic_containing(pos: f32, atomic: &[AtomicRange]) -> Option<AtomicRange> {
  atomic.iter().copied().find(|range| {
    pos >= range.start - BREAK_EPSILON
      && pos < range.end - BREAK_EPSILON
      && (range.end - range.start) > BREAK_EPSILON
  })
}

fn next_atomic_in_range(start: f32, end: f32, atomic: &[AtomicRange]) -> Option<AtomicRange> {
  atomic
    .iter()
    .copied()
    .filter(|range| range.start > start + BREAK_EPSILON && range.start < end + BREAK_EPSILON)
    .min_by(|a, b| {
      a.start
        .partial_cmp(&b.start)
        .unwrap_or(std::cmp::Ordering::Equal)
    })
}

pub(crate) fn collect_atomic_ranges(
  node: &FragmentNode,
  abs_start: f32,
  ranges: &mut Vec<AtomicRange>,
) {
  collect_atomic_ranges_with_axes(node, abs_start, FragmentAxes::default(), ranges);
}

pub(crate) fn collect_atomic_ranges_with_axes(
  node: &FragmentNode,
  abs_start: f32,
  axes: FragmentAxes,
  ranges: &mut Vec<AtomicRange>,
) {
  let node_block_size = axes.block_size(&node.bounds);
  let abs_end = abs_start + node_block_size;
  if node
    .style
    .as_ref()
    .is_some_and(|style| style.float.is_floating())
  {
    ranges.push(AtomicRange {
      start: abs_start,
      end: abs_end,
    });
  }

  for child in &node.children {
    let child_abs_start = abs_start + axes.block_start(&child.bounds, node_block_size);
    collect_atomic_ranges_with_axes(child, child_abs_start, axes, ranges);
  }
}

pub(crate) fn normalize_atomic_ranges(ranges: &mut Vec<AtomicRange>) {
  ranges.retain(|range| range.end > range.start + BREAK_EPSILON);
  ranges.sort_by(|a, b| {
    a.start
      .partial_cmp(&b.start)
      .unwrap_or(std::cmp::Ordering::Equal)
  });

  let mut merged: Vec<AtomicRange> = Vec::with_capacity(ranges.len());
  for range in ranges.iter().copied() {
    if let Some(last) = merged.last_mut() {
      if range.start <= last.end + BREAK_EPSILON {
        last.end = last.end.max(range.end);
        continue;
      }
    }
    merged.push(range);
  }

  ranges.clear();
  ranges.extend(merged);
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

fn combine_breaks(
  after: BreakBetween,
  before: BreakBetween,
  context: FragmentationContext,
) -> BreakStrength {
  if forces_break_between(after, context) || forces_break_between(before, context) {
    return BreakStrength::Forced;
  }

  if avoids_break_between(after, context) || avoids_break_between(before, context) {
    return BreakStrength::Avoid;
  }

  BreakStrength::Auto
}

pub(crate) fn forces_break_between(value: BreakBetween, context: FragmentationContext) -> bool {
  match value {
    BreakBetween::Always => true,
    BreakBetween::Column => matches!(context, FragmentationContext::Column),
    BreakBetween::Page
    | BreakBetween::Left
    | BreakBetween::Right
    | BreakBetween::Recto
    | BreakBetween::Verso => matches!(context, FragmentationContext::Page),
    _ => false,
  }
}

pub(crate) fn avoids_break_between(value: BreakBetween, context: FragmentationContext) -> bool {
  match value {
    BreakBetween::Avoid => true,
    BreakBetween::AvoidPage => matches!(context, FragmentationContext::Page),
    BreakBetween::AvoidColumn => matches!(context, FragmentationContext::Column),
    _ => false,
  }
}

pub(crate) fn avoids_break_inside(value: BreakInside, context: FragmentationContext) -> bool {
  match value {
    BreakInside::Avoid => true,
    BreakInside::AvoidPage => matches!(context, FragmentationContext::Page),
    BreakInside::AvoidColumn => matches!(context, FragmentationContext::Column),
    _ => false,
  }
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
