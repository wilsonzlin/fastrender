//! Table Layout Algorithm (CSS Tables Module Level 3)
//!
//! This module implements the CSS table layout algorithm for rendering HTML tables.
//! Tables have a unique two-pass layout model that first determines column widths
//! and then row heights.
//!
//! # Table Layout Model
//!
//! CSS tables follow a specific layout model defined in CSS 2.1 Section 17:
//!
//! 1. **Structure Analysis**: Build grid from table, row groups, rows, and cells
//! 2. **Column Width Calculation**: Determine column widths (fixed or auto)
//! 3. **Row Height Calculation**: Determine row heights based on content
//! 4. **Cell Positioning**: Place cells in their grid positions
//!
//! # Layout Algorithms
//!
//! Two table layout algorithms are supported:
//!
//! - **Fixed**: `table-layout: fixed` - Fast, predictable, uses first row only
//! - **Auto**: `table-layout: auto` (default) - Content-aware, requires full scan
//!
//! # References
//!
//! - CSS 2.1 Section 17: Tables
//! - CSS Tables Module Level 3
//! - HTML 5.2 Section 4.9: Tabular data

use crate::debug::runtime;
use crate::error::{RenderError, RenderStage};
use crate::geometry::Point;
use crate::geometry::Rect;
use crate::layout::absolute_positioning::resolve_positioned_style;
use crate::layout::absolute_positioning::AbsoluteLayout;
use crate::layout::absolute_positioning::AbsoluteLayoutInput;
use crate::layout::constraints::AvailableSpace;
use crate::layout::constraints::LayoutConstraints;
use crate::layout::contexts::block::BlockFormattingContext;
use crate::layout::contexts::factory::FormattingContextFactory;
use crate::layout::contexts::positioned::ContainingBlock;
use crate::layout::contexts::table::column_distribution::distribute_spanning_cell_width;
use crate::layout::contexts::table::column_distribution::distribute_spanning_percentage;
use crate::layout::contexts::table::column_distribution::ColumnConstraints;
use crate::layout::contexts::table::column_distribution::ColumnDistributor;
use crate::layout::contexts::table::column_distribution::DistributionMode;
use crate::layout::engine::LayoutParallelism;
use crate::layout::formatting_context::intrinsic_cache_epoch;
use crate::layout::formatting_context::layout_cache_lookup;
use crate::layout::formatting_context::layout_cache_store;
use crate::layout::formatting_context::FormattingContext;
use crate::layout::formatting_context::IntrinsicSizingMode;
use crate::layout::formatting_context::LayoutError;
use crate::layout::profile::layout_timer;
use crate::layout::profile::LayoutKind;
use crate::style::color::Rgba;
use crate::style::computed::Visibility;
use crate::style::display::Display;
use crate::style::display::FormattingContextType;
use crate::style::types::BorderCollapse;
use crate::style::types::BorderStyle;
use crate::style::types::CaptionSide;
use crate::style::types::Direction;
use crate::style::types::EmptyCells;
use crate::style::types::TableLayout;
use crate::style::types::VerticalAlign;
use crate::style::values::Length;
use crate::style::values::LengthUnit;
use crate::style::ComputedStyle;
use crate::tree::box_tree::BoxNode;
use crate::tree::box_tree::BoxType;
use crate::tree::box_tree::MarkerContent;
use crate::tree::fragment_tree::FragmentContent;
use crate::tree::fragment_tree::FragmentNode;
use crate::tree::fragment_tree::TableCollapsedBorders;
use crate::tree::table_fixup::TableStructureFixer;
use crate::{
  render_control::active_deadline, render_control::check_active_periodic,
  render_control::with_deadline,
};
use rayon::prelude::*;
use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

fn check_layout_deadline(counter: &mut usize) -> Result<(), LayoutError> {
  if let Err(RenderError::Timeout { elapsed, .. }) =
    check_active_periodic(counter, 64, RenderStage::Layout)
  {
    return Err(LayoutError::Timeout { elapsed });
  }
  Ok(())
}

// ============================================================================
// Table Structure Types
// ============================================================================

/// Represents the resolved structure of a table
///
/// A TableStructure is built by analyzing the box tree and resolving
/// the grid structure including row spans and column spans.
///
/// # Structure
///
/// ```text
/// +--------------------------------------------------+
/// | TableStructure                                   |
/// |  - columns: Vec<ColumnInfo>                      |
/// |  - rows: Vec<RowInfo>                            |
/// |  - cells: Vec<CellInfo>                          |
/// |  - grid: 2D array of cell references             |
/// +--------------------------------------------------+
/// ```
#[derive(Debug, Clone)]
pub struct TableStructure {
  /// Number of columns in the table
  pub column_count: usize,

  /// Number of rows in the table
  pub row_count: usize,

  /// Column definitions
  pub columns: Vec<ColumnInfo>,

  /// Row definitions
  pub rows: Vec<RowInfo>,

  /// Cell information
  pub cells: Vec<CellInfo>,

  /// Grid mapping: `grid[row][col]` = cell_index or None for spanned cells
  pub grid: Vec<Vec<Option<usize>>>,

  /// Table border spacing (horizontal, vertical)
  pub border_spacing: (f32, f32),

  /// Border collapsing model
  pub border_collapse: BorderCollapse,

  /// Whether table uses fixed layout
  pub is_fixed_layout: bool,
}

/// Information about a table column
#[derive(Debug, Clone)]
pub struct ColumnInfo {
  /// Column index (0-based)
  pub index: usize,

  /// Source column index prior to any filtering (0-based)
  pub source_index: usize,

  /// Author-specified minimum width (from `min-width` on <col>/<colgroup>)
  pub author_min_width: Option<crate::style::values::Length>,

  /// Author-specified maximum width (from `max-width` on <col>/<colgroup>)
  pub author_max_width: Option<crate::style::values::Length>,

  /// Font size to resolve relative column lengths (em/rem)
  pub font_size: f32,

  /// Visibility state for this column
  pub visibility: Visibility,

  /// Specified width (from col element or first cell)
  pub specified_width: Option<SpecifiedWidth>,

  /// Computed minimum width
  pub min_width: f32,

  /// Computed maximum width
  pub max_width: f32,

  /// Final computed width after distribution
  pub computed_width: f32,
}

impl ColumnInfo {
  /// Creates a new column with no width constraints
  pub fn new(index: usize) -> Self {
    Self {
      index,
      source_index: index,
      author_min_width: None,
      author_max_width: None,
      font_size: 0.0,
      visibility: Visibility::Visible,
      specified_width: None,
      min_width: 0.0,
      max_width: f32::INFINITY,
      computed_width: 0.0,
    }
  }
}

/// Distribute a spanning cell's minimum width across the covered columns.
///
/// Extra width is allocated proportionally to each column's remaining growth
/// (max - min). If no column has remaining capacity, the remainder is spread
/// evenly and max widths are lifted to stay ≥ min.
fn distribute_span_min(columns: &mut [ColumnInfo], start: usize, end: usize, required_min: f32) {
  if start >= end || end > columns.len() {
    return;
  }
  let span = &mut columns[start..end];
  let current: f32 = span.iter().map(|c| c.min_width).sum();
  if required_min <= current {
    return;
  }
  let deficit = required_min - current;

  let capacities: Vec<f32> = span
    .iter()
    .map(|c| {
      let cap = c.max_width - c.min_width;
      if cap.is_finite() && cap > 0.0 {
        cap
      } else {
        0.0
      }
    })
    .collect();
  let total_cap: f32 = capacities.iter().sum();
  let remaining = if total_cap > 0.0 {
    let mut allocated = 0.0;
    for (col, cap) in span.iter_mut().zip(capacities.iter()) {
      if *cap > 0.0 {
        let share = deficit * (*cap / total_cap);
        let growth = share.min(*cap);
        col.min_width += growth;
        allocated += growth;
      }
    }
    (deficit - allocated).max(0.0)
  } else {
    deficit
  };

  if remaining > 0.0 {
    let per = remaining / span.len() as f32;
    for col in span.iter_mut() {
      col.min_width += per;
    }
  }

  for col in span.iter_mut() {
    col.max_width = col.max_width.max(col.min_width);
  }
}

/// Distribute a spanning cell's maximum width across the covered columns.
///
/// Extra preferred width is allocated using each column's flexibility
/// (max - min). If no flexibility exists, the remainder is spread evenly.
fn distribute_span_max(columns: &mut [ColumnInfo], start: usize, end: usize, required_max: f32) {
  if !required_max.is_finite() || start >= end || end > columns.len() {
    return;
  }
  let span = &mut columns[start..end];
  let current: f32 = span.iter().map(|c| c.max_width).sum();
  if required_max <= current {
    return;
  }
  let deficit = required_max - current;

  let flex_ranges: Vec<f32> = span
    .iter()
    .map(|c| {
      let range = c.max_width - c.min_width;
      if range.is_finite() && range > 0.0 {
        range
      } else {
        0.0
      }
    })
    .collect();
  let total_flex: f32 = flex_ranges.iter().sum();
  let remaining = if total_flex > 0.0 {
    let mut allocated = 0.0;
    for (col, range) in span.iter_mut().zip(flex_ranges.iter()) {
      if *range > 0.0 {
        let growth = deficit * (*range / total_flex);
        col.max_width += growth;
        allocated += growth;
      }
    }
    (deficit - allocated).max(0.0)
  } else {
    deficit
  };

  if remaining > 0.0 {
    let per = remaining / span.len() as f32;
    for col in span.iter_mut() {
      col.max_width += per;
    }
  }

  for col in span.iter_mut() {
    col.max_width = col.max_width.max(col.min_width);
  }
}

/// Information about a table row
#[derive(Debug, Clone)]
pub struct RowInfo {
  /// Row index (0-based)
  pub index: usize,

  /// Source row index prior to any filtering (0-based)
  pub source_index: usize,

  /// Visibility state for this row
  pub visibility: Visibility,

  /// Explicit vertical-align from the row or its row group (if provided)
  pub vertical_align: Option<VerticalAlign>,

  /// Specified height (from row element)
  pub specified_height: Option<SpecifiedHeight>,

  /// Author min-height (from CSS)
  pub author_min_height: Option<SpecifiedHeight>,

  /// Author max-height (from CSS)
  pub author_max_height: Option<SpecifiedHeight>,

  /// Computed minimum height based on cell content
  pub min_height: f32,

  /// Final computed height after distribution
  pub computed_height: f32,

  /// Y position of the row
  pub y_position: f32,
}

impl RowInfo {
  /// Creates a new row with no height constraints
  pub fn new(index: usize) -> Self {
    Self {
      index,
      source_index: index,
      visibility: Visibility::Visible,
      vertical_align: None,
      specified_height: None,
      author_min_height: None,
      author_max_height: None,
      min_height: 0.0,
      computed_height: 0.0,
      y_position: 0.0,
    }
  }
}

/// Specified width for a column
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SpecifiedWidth {
  /// Fixed pixel width
  Fixed(f32),
  /// Percentage of table width
  Percent(f32),
  /// Auto width (content-based)
  Auto,
}

/// Specified height for a row
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SpecifiedHeight {
  /// Fixed pixel height
  Fixed(f32),
  /// Percentage of table height
  Percent(f32),
  /// Auto height (content-based)
  Auto,
}

/// Information about a row group's height constraints.
#[derive(Debug, Clone)]
struct RowGroupConstraints {
  /// Inclusive start row index (visible rows)
  start: usize,
  /// Exclusive end row index (visible rows)
  end: usize,
  /// Computed `height` on the row group.
  height: Option<SpecifiedHeight>,
  /// Computed `min-height` on the row group.
  min_height: Option<SpecifiedHeight>,
  /// Computed `max-height` on the row group.
  max_height: Option<SpecifiedHeight>,
}

fn distribute_rowspan_targets_range(
  start: usize,
  end: usize,
  row_heights: &mut [f32],
  rows: &[RowInfo],
  remaining: f32,
  percent_base: Option<f32>,
  row_floor: &dyn Fn(usize, f32) -> f32,
  combine: &dyn Fn(f32, f32) -> f32,
) {
  if remaining <= 0.0 {
    return;
  }

  let span_end = end.min(row_heights.len()).min(rows.len());
  let span_start = start.min(span_end);
  if span_start >= span_end {
    return;
  }
  let span_len = span_end - span_start;

  let mut weights = Vec::with_capacity(span_len);
  let mut caps = Vec::with_capacity(span_len);
  let mut total_weight = 0.0;
  for idx in span_start..span_end {
    let current = *row_heights.get(idx).unwrap_or(&0.0);
    let base = row_floor(idx, current);
    let (_, max_opt) = resolve_row_min_max(&rows[idx], percent_base);
    let headroom = max_opt.map(|m| (m - base).max(0.0));
    let weight = match headroom {
      Some(h) if h > 0.0 => h,
      Some(_) => 0.0,
      None => 1.0, // Uncapped rows share spanning contributions evenly.
    };
    total_weight += weight;
    weights.push(weight);
    caps.push(headroom);
  }

  if total_weight <= 0.0 {
    total_weight = span_len as f32;
    weights.fill(1.0);
  }

  let mut shares = vec![0.0; span_len];
  let mut allocated = 0.0;
  for ((share, weight), cap) in shares.iter_mut().zip(weights.iter()).zip(caps.iter()) {
    let mut portion = if total_weight > 0.0 {
      remaining * (*weight / total_weight)
    } else {
      0.0
    };
    if let Some(c) = cap {
      portion = portion.min(*c);
    }
    *share = portion;
    allocated += portion;
  }

  let leftover = (remaining - allocated).max(0.0);
  if leftover > 0.01 {
    let mut finite_sum = 0.0;
    let mut unbounded = 0usize;
    for (cap, share) in caps.iter().zip(shares.iter()) {
      let cap_left = cap.map(|c| (c - *share).max(0.0)).unwrap_or(f32::INFINITY);
      if cap_left <= 0.0 {
        continue;
      }
      if cap_left.is_finite() {
        finite_sum += cap_left;
      } else {
        unbounded += 1;
      }
    }

    if finite_sum > 0.0 {
      for (cap, share) in caps.iter().zip(shares.iter_mut()) {
        let cap_left = cap.map(|c| (c - *share).max(0.0)).unwrap_or(f32::INFINITY);
        if cap_left <= 0.0 || !cap_left.is_finite() {
          continue;
        }
        let extra = leftover * (cap_left / finite_sum);
        *share += extra.min(cap_left);
      }
    } else if unbounded > 0 {
      let per = leftover / unbounded as f32;
      for (cap, share) in caps.iter().zip(shares.iter_mut()) {
        let cap_left = cap.map(|c| (c - *share).max(0.0)).unwrap_or(f32::INFINITY);
        if cap_left.is_infinite() && cap_left > 0.0 {
          *share += per;
        }
      }
    }
  }

  for (offset, share) in shares.iter().enumerate() {
    let target_idx = span_start + offset;
    if let Some(slot) = row_heights.get_mut(target_idx) {
      *slot = combine(*slot, *share);
    }
  }
}

fn distribute_remaining_height_with_caps(
  computed: &mut [f32],
  rows: &[RowInfo],
  remaining: f32,
  percent_base: Option<f32>,
) {
  if remaining <= 0.0 || computed.is_empty() {
    return;
  }

  let mut rem = remaining;
  let mut iterations = 0;
  while rem > 0.01 && iterations < computed.len().saturating_mul(2).max(1) {
    let mut eligible = Vec::new();
    for (idx, row) in rows.iter().enumerate() {
      let (_, max_cap) = resolve_row_min_max(row, percent_base);
      let headroom = max_cap
        .map(|m| (m - computed[idx]).max(0.0))
        .unwrap_or(f32::INFINITY);
      if headroom <= 0.0 {
        continue;
      }
      let weight = if headroom.is_finite() {
        headroom
      } else {
        computed[idx].max(1.0)
      };
      eligible.push((idx, headroom, weight));
    }

    if eligible.is_empty() {
      break;
    }

    let total_weight: f32 = eligible.iter().map(|(_, _, w)| *w).sum();
    if total_weight <= 0.0 {
      break;
    }

    let mut consumed = 0.0;
    for (idx, headroom, weight) in &eligible {
      let mut delta = rem * (*weight / total_weight);
      if headroom.is_finite() {
        delta = delta.min(*headroom);
      }
      if let Some(slot) = computed.get_mut(*idx) {
        *slot += delta;
        consumed += delta;
      }
    }

    if consumed <= 0.001 {
      break;
    }
    rem = (rem - consumed).max(0.0);
    iterations += 1;
  }
}

fn distribute_extra_row_height_with_groups(
  row_metrics: &mut [RowMetrics],
  rows: &[RowInfo],
  flex_indices: &[usize],
  extra: f32,
  percent_base: Option<f32>,
  row_to_group: &[Option<usize>],
  groups: &[RowGroupConstraints],
  v_spacing: f32,
) {
  if flex_indices.is_empty() || extra <= 0.0 {
    return;
  }

  let mut remaining = extra;
  let mut iterations = 0;
  while remaining > 0.01 && iterations < flex_indices.len().saturating_mul(2).max(1) {
    let mut group_remaining: Vec<Option<f32>> = groups
      .iter()
      .map(|g| {
        let count = (g.start..g.end).count();
        let spacing = if count > 0 {
          v_spacing * count.saturating_sub(1) as f32
        } else {
          0.0
        };
        let current: f32 = (g.start..g.end)
          .filter_map(|i| row_metrics.get(i))
          .map(|r| r.height)
          .sum();
        let (_, max_cap) = resolve_row_group_min_max(g, percent_base);
        max_cap.map(|cap| (cap - current - spacing).max(0.0))
      })
      .collect();

    let mut eligible = Vec::new();
    for &idx in flex_indices {
      if let Some(row) = row_metrics.get(idx) {
        let (_, max_opt) = resolve_row_min_max(&rows[idx], percent_base);
        let row_headroom = max_opt
          .map(|m| (m - row.height).max(0.0))
          .unwrap_or(f32::INFINITY);
        if row_headroom <= 0.0 {
          continue;
        }
        let group_cap = row_to_group
          .get(idx)
          .and_then(|g| g.and_then(|gi| group_remaining.get(gi).copied()).flatten())
          .unwrap_or(f32::INFINITY);
        if group_cap <= 0.0 {
          continue;
        }
        let effective_headroom = row_headroom.min(group_cap);
        let weight = if effective_headroom.is_finite() {
          effective_headroom
        } else {
          row.height.max(1.0)
        };
        eligible.push((idx, effective_headroom, weight));
      }
    }

    if eligible.is_empty() {
      break;
    }

    let total_weight: f32 = eligible.iter().map(|(_, _, w)| *w).sum();
    if total_weight <= 0.0 {
      break;
    }

    let mut consumed = 0.0;
    let mut group_consumed: Vec<f32> = vec![0.0; groups.len()];
    for (idx, headroom, weight) in &eligible {
      if let Some(row) = row_metrics.get_mut(*idx) {
        let mut delta = remaining * (*weight / total_weight);
        if headroom.is_finite() {
          delta = delta.min(*headroom);
        }
        row.height += delta;
        consumed += delta;
        if let Some(g) = row_to_group
          .get(*idx)
          .and_then(|g| g.map(|gi| gi.min(group_consumed.len().saturating_sub(1))))
        {
          group_consumed[g] += delta;
        }
      }
    }

    for (g_idx, used) in group_consumed.iter().enumerate() {
      if let Some(Some(val)) = group_remaining.get_mut(g_idx) {
        *val = (*val - *used).max(0.0);
      }
    }

    if consumed <= 0.001 {
      break;
    }
    remaining = (remaining - consumed).max(0.0);
    iterations += 1;
  }
}

fn reduce_rows_with_headroom(
  computed: &mut [f32],
  rows: &[RowInfo],
  target_total: f32,
  indices: &[usize],
  percent_base: Option<f32>,
) {
  if indices.is_empty() {
    return;
  }

  let mut overflow = computed.iter().sum::<f32>() - target_total;
  if overflow <= 0.0 {
    return;
  }

  // Iteratively shave overflow proportionally to available headroom.
  let mut iterations = 0;
  while overflow > 0.01 && iterations < indices.len().saturating_mul(2).max(1) {
    let mut headrooms = Vec::new();
    for &idx in indices {
      if let Some(cur) = computed.get(idx).copied() {
        let (min_cap, _) = resolve_row_min_max(&rows[idx], percent_base);
        let min_floor = min_cap.unwrap_or(rows[idx].min_height);
        let room = (cur - min_floor).max(0.0);
        if room > 0.0 {
          headrooms.push((idx, room));
        }
      }
    }

    if headrooms.is_empty() {
      break;
    }

    let total_headroom: f32 = headrooms.iter().map(|(_, room)| *room).sum();
    if total_headroom <= 0.0 {
      break;
    }

    let mut shaved = 0.0;
    for (idx, room) in headrooms {
      if let Some(slot) = computed.get_mut(idx) {
        let mut delta = overflow * (room / total_headroom);
        delta = delta.min(*slot); // avoid negative heights
        *slot -= delta.min(room);
        shaved += delta.min(room);
      }
    }

    if shaved <= 0.001 {
      break;
    }
    overflow = (computed.iter().sum::<f32>() - target_total).max(0.0);
    iterations += 1;
  }
}

fn resolve_row_min_max(row: &RowInfo, percent_base: Option<f32>) -> (Option<f32>, Option<f32>) {
  let min = match row.author_min_height {
    Some(SpecifiedHeight::Fixed(h)) => Some(h),
    Some(SpecifiedHeight::Percent(p)) => percent_base.map(|t| (p / 100.0) * t),
    _ => None,
  };
  let max = match row.author_max_height {
    Some(SpecifiedHeight::Fixed(h)) => Some(h),
    Some(SpecifiedHeight::Percent(p)) => percent_base.map(|t| (p / 100.0) * t),
    _ => None,
  };
  (min, max)
}

fn resolve_row_group_min_max(
  group: &RowGroupConstraints,
  percent_base: Option<f32>,
) -> (Option<f32>, Option<f32>) {
  let resolve = |spec: Option<SpecifiedHeight>| match spec {
    Some(SpecifiedHeight::Fixed(h)) => Some(h),
    Some(SpecifiedHeight::Percent(p)) => percent_base.map(|t| (p / 100.0) * t),
    _ => None,
  };

  let mut min = resolve(group.height);
  if let Some(resolved_min) = resolve(group.min_height) {
    min = Some(min.map_or(resolved_min, |m| m.max(resolved_min)));
  }

  let max = resolve(group.max_height);
  (min, max)
}

fn distribute_row_group_extra_with_caps(
  row_heights: &mut [f32],
  rows: &[RowInfo],
  indices: &[usize],
  extra: f32,
  percent_base: Option<f32>,
) {
  if indices.is_empty() || extra <= 0.0 {
    return;
  }

  let mut remaining = extra;
  let mut iterations = 0;
  while remaining > 0.01 && iterations < indices.len().saturating_mul(2).max(1) {
    let mut eligible = Vec::new();
    for &idx in indices {
      if let Some(&current) = row_heights.get(idx) {
        let (_, max_cap) = resolve_row_min_max(&rows[idx], percent_base);
        let headroom = max_cap
          .map(|m| (m - current).max(0.0))
          .unwrap_or(f32::INFINITY);
        if headroom <= 0.0 {
          continue;
        }
        let weight = if headroom.is_finite() {
          headroom
        } else {
          current.max(1.0)
        };
        eligible.push((idx, headroom, weight));
      }
    }

    if eligible.is_empty() {
      break;
    }

    let total_weight: f32 = eligible.iter().map(|(_, _, w)| *w).sum();
    if total_weight <= 0.0 {
      break;
    }

    let mut consumed = 0.0;
    for (idx, headroom, weight) in &eligible {
      if let Some(slot) = row_heights.get_mut(*idx) {
        let mut delta = remaining * (*weight / total_weight);
        if headroom.is_finite() {
          delta = delta.min(*headroom);
        }
        *slot += delta;
        consumed += delta;
      }
    }

    if consumed <= 0.001 {
      break;
    }
    remaining = (remaining - consumed).max(0.0);
    iterations += 1;
  }
}

fn shrink_row_group_to_target(
  row_heights: &mut [f32],
  rows: &[RowInfo],
  indices: &[usize],
  target_total: f32,
  percent_base: Option<f32>,
  content_floors: &[f32],
) {
  if indices.is_empty() {
    return;
  }

  let mut current_total: f32 = indices.iter().filter_map(|i| row_heights.get(*i)).sum();
  let mut overflow = current_total - target_total;
  if overflow <= 0.0 {
    return;
  }

  let mut iterations = 0;
  while overflow > 0.01 && iterations < indices.len().saturating_mul(2).max(1) {
    let mut headrooms = Vec::new();
    for &idx in indices {
      let floor_from_content = content_floors.get(idx).copied().unwrap_or(0.0);
      let (min_cap, _) = resolve_row_min_max(&rows[idx], percent_base);
      let mut floor = floor_from_content;
      if let Some(spec) = rows[idx].specified_height {
        match spec {
          SpecifiedHeight::Fixed(h) => floor = floor.max(h),
          SpecifiedHeight::Percent(p) => {
            if let Some(base) = percent_base {
              floor = floor.max((p / 100.0) * base);
            }
          }
          SpecifiedHeight::Auto => {}
        }
      }
      if let Some(min) = min_cap {
        floor = floor.max(min);
      }
      if let Some(&current) = row_heights.get(idx) {
        let room = (current - floor).max(0.0);
        if room > 0.0 {
          headrooms.push((idx, room));
        }
      }
    }

    if headrooms.is_empty() {
      break;
    }

    let total_headroom: f32 = headrooms.iter().map(|(_, r)| *r).sum();
    if total_headroom <= 0.0 {
      break;
    }

    let mut shaved = 0.0;
    for (idx, room) in headrooms {
      if let Some(slot) = row_heights.get_mut(idx) {
        let mut delta = overflow * (room / total_headroom);
        delta = delta.min(*slot);
        let applied = delta.min(room);
        *slot -= applied;
        shaved += applied;
      }
    }

    if shaved <= 0.001 {
      break;
    }
    current_total = (current_total - shaved).max(0.0);
    overflow = (current_total - target_total).max(0.0);
    iterations += 1;
  }
}

fn apply_row_group_constraints(
  row_heights: &mut [f32],
  rows: &[RowInfo],
  groups: &[RowGroupConstraints],
  percent_base: Option<f32>,
  v_spacing: f32,
  content_min: &[f32],
) {
  for group in groups {
    let start = group.start.min(row_heights.len());
    let end = group.end.min(row_heights.len());
    if start >= end {
      continue;
    }
    let indices: Vec<usize> = (start..end).collect();
    let spacing_total = if v_spacing > 0.0 {
      v_spacing * indices.len().saturating_sub(1) as f32
    } else {
      0.0
    };
    let rows_sum: f32 = indices.iter().filter_map(|i| row_heights.get(*i)).sum();
    let (min_target, max_target) = resolve_row_group_min_max(group, percent_base);

    if let Some(min_h) = min_target {
      let needed = (min_h - (rows_sum + spacing_total)).max(0.0);
      if needed > 0.0 {
        distribute_row_group_extra_with_caps(row_heights, rows, &indices, needed, percent_base);
      }
    }

    if let Some(max_h) = max_target {
      let target_rows_total = (max_h - spacing_total).max(0.0);
      let current_rows_total: f32 = indices.iter().filter_map(|i| row_heights.get(*i)).sum();
      if current_rows_total > target_rows_total + 0.01 {
        shrink_row_group_to_target(
          row_heights,
          rows,
          &indices,
          target_rows_total,
          percent_base,
          content_min,
        );
      }
    }
  }
}

/// Information about a table cell
#[derive(Debug, Clone)]
pub struct CellInfo {
  /// Cell index in the cells array
  pub index: usize,

  /// Source row index prior to any filtering
  pub source_row: usize,

  /// Source column index prior to any filtering
  pub source_col: usize,

  /// Starting row (0-based)
  pub row: usize,

  /// Starting column (0-based)
  pub col: usize,

  /// Row span (default 1)
  pub rowspan: usize,

  /// Column span (default 1)
  pub colspan: usize,

  /// Reference to the cell's box node
  pub box_index: usize,

  /// Computed minimum width (intrinsic)
  pub min_width: f32,

  /// Computed maximum width (intrinsic)
  pub max_width: f32,

  /// Computed minimum height
  pub min_height: f32,

  /// Final computed bounds
  pub bounds: Rect,
}

impl CellInfo {
  /// Creates a new cell at the given position
  pub fn new(index: usize, row: usize, col: usize) -> Self {
    Self {
      index,
      source_row: row,
      source_col: col,
      row,
      col,
      rowspan: 1,
      colspan: 1,
      box_index: 0,
      min_width: 0.0,
      max_width: f32::INFINITY,
      min_height: 0.0,
      bounds: Rect::ZERO,
    }
  }

  /// Returns true if this cell spans multiple columns
  pub fn is_column_spanning(&self) -> bool {
    self.colspan > 1
  }

  /// Returns true if this cell spans multiple rows
  pub fn is_row_spanning(&self) -> bool {
    self.rowspan > 1
  }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum CellEmptinessCacheKey {
  Id { id: usize, style: usize },
  Ptr { ptr: usize, style: usize },
}

thread_local! {
  /// Per-thread memoization of cell emptiness checks keyed by box id + style.
  /// Cleared whenever the active layout epoch changes.
  static CELL_EMPTINESS_CACHE: RefCell<(usize, HashMap<CellEmptinessCacheKey, bool>)> =
    RefCell::new((0, HashMap::new()));
}

fn cell_emptiness_cache_key(cell: &BoxNode) -> CellEmptinessCacheKey {
  let style_ptr = Arc::as_ptr(&cell.style) as usize;
  let id = cell.id();
  if id != 0 {
    CellEmptinessCacheKey::Id {
      id,
      style: style_ptr,
    }
  } else {
    CellEmptinessCacheKey::Ptr {
      ptr: cell as *const BoxNode as usize,
      style: style_ptr,
    }
  }
}

fn with_cell_emptiness_cache<R>(
  epoch: usize,
  f: impl FnOnce(&mut HashMap<CellEmptinessCacheKey, bool>) -> R,
) -> R {
  CELL_EMPTINESS_CACHE.with(|cache| {
    let mut cache = cache.borrow_mut();
    if cache.0 != epoch {
      cache.0 = epoch;
      cache.1.clear();
    }
    f(&mut cache.1)
  })
}

fn node_has_visible_content(node: &BoxNode) -> bool {
  match &node.box_type {
    BoxType::Text(text) => !text.text.trim().is_empty(),
    BoxType::Marker(marker) => match &marker.content {
      MarkerContent::Text(t) => !t.trim().is_empty(),
      MarkerContent::Image(_) => true,
    },
    BoxType::Replaced(_) => true,
    _ => node.children.iter().any(node_has_visible_content),
  }
}

fn compute_cell_is_visually_empty(cell: &BoxNode) -> bool {
  if cell
    .style
    .background_layers
    .iter()
    .any(|l| l.image.is_some())
    || !cell.style.background_color.is_transparent()
  {
    return false;
  }
  !node_has_visible_content(cell)
}

fn cell_is_visually_empty(cell: &BoxNode) -> bool {
  let key = cell_emptiness_cache_key(cell);
  let epoch = intrinsic_cache_epoch();
  if let Some(hit) = with_cell_emptiness_cache(epoch, |cache| cache.get(&key).copied()) {
    return hit;
  }
  let computed = compute_cell_is_visually_empty(cell);
  with_cell_emptiness_cache(epoch, |cache| {
    cache.insert(key, computed);
  });
  computed
}

fn style_paints_background_or_border(style: &ComputedStyle, allow_borders: bool) -> bool {
  if !style.background_color.is_transparent()
    || style.background_layers.iter().any(|l| l.image.is_some())
    || !style.box_shadow.is_empty()
  {
    return true;
  }
  if !allow_borders {
    return false;
  }
  let paints_border = |style: BorderStyle, width: &Length| {
    !matches!(style, BorderStyle::None | BorderStyle::Hidden) && width.to_px() > 0.0
  };
  paints_border(style.border_top_style, &style.border_top_width)
    || paints_border(style.border_right_style, &style.border_right_width)
    || paints_border(style.border_bottom_style, &style.border_bottom_width)
    || paints_border(style.border_left_style, &style.border_left_width)
}

fn strip_borders(style: &ComputedStyle) -> Arc<ComputedStyle> {
  let mut clone = style.clone();
  clone.border_top_width = Length::px(0.0);
  clone.border_right_width = Length::px(0.0);
  clone.border_bottom_width = Length::px(0.0);
  clone.border_left_width = Length::px(0.0);
  clone.border_top_style = BorderStyle::None;
  clone.border_right_style = BorderStyle::None;
  clone.border_bottom_style = BorderStyle::None;
  clone.border_left_style = BorderStyle::None;
  Arc::new(clone)
}

fn strip_borders_cached(
  style: &Arc<ComputedStyle>,
  cache: &mut HashMap<usize, Arc<ComputedStyle>>,
) -> Arc<ComputedStyle> {
  let key = Arc::as_ptr(style) as usize;
  if let Some(cached) = cache.get(&key) {
    return cached.clone();
  }
  let stripped = strip_borders(style);
  cache.insert(key, stripped.clone());
  stripped
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct StyleOverrideFlags(u8);

impl StyleOverrideFlags {
  const NONE: Self = Self(0);
  const STRIP_PADDING_BORDERS: Self = Self(1 << 0);
  const LAYOUT_CLEAR_WIDTHS_AND_MARGINS: Self = Self(1 << 1);
  const COLLAPSE_ZERO_BORDERS: Self = Self(1 << 2);
  const HIDE_EMPTY_RESET_BG_AND_TRANSPARENT_BORDERS: Self = Self(1 << 3);

  fn is_empty(self) -> bool {
    self.0 == 0
  }

  fn contains(self, other: Self) -> bool {
    self.0 & other.0 != 0
  }
}

impl std::ops::BitOr for StyleOverrideFlags {
  type Output = Self;

  fn bitor(self, rhs: Self) -> Self::Output {
    Self(self.0 | rhs.0)
  }
}

impl std::ops::BitOrAssign for StyleOverrideFlags {
  fn bitor_assign(&mut self, rhs: Self) {
    self.0 |= rhs.0;
  }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct StyleOverrideKey {
  base_ptr: usize,
  flags: StyleOverrideFlags,
}

#[derive(Default)]
struct StyleOverrideCache {
  cache: Mutex<HashMap<StyleOverrideKey, Arc<ComputedStyle>>>,
}

impl StyleOverrideCache {
  fn derive(&self, base: &Arc<ComputedStyle>, flags: StyleOverrideFlags) -> Arc<ComputedStyle> {
    if flags.is_empty() {
      debug_assert_eq!(flags, StyleOverrideFlags::NONE);
      return base.clone();
    }
    let key = StyleOverrideKey {
      base_ptr: Arc::as_ptr(base) as usize,
      flags,
    };
    {
      let cache = self.cache.lock().expect("override cache poisoned");
      if let Some(hit) = cache.get(&key) {
        return hit.clone();
      }
    }

    let derived = Arc::new(apply_style_overrides(base, flags));
    let mut cache = self.cache.lock().expect("override cache poisoned");
    cache.entry(key).or_insert_with(|| derived.clone()).clone()
  }
}

fn apply_style_overrides(base: &ComputedStyle, flags: StyleOverrideFlags) -> ComputedStyle {
  let mut style = base.clone();
  if flags.contains(StyleOverrideFlags::STRIP_PADDING_BORDERS) {
    style.padding_left = Length::px(0.0);
    style.padding_right = Length::px(0.0);
    style.border_left_width = Length::px(0.0);
    style.border_right_width = Length::px(0.0);
  }
  if flags.contains(StyleOverrideFlags::HIDE_EMPTY_RESET_BG_AND_TRANSPARENT_BORDERS) {
    style.reset_background_to_initial();
    style.border_left_color = Rgba::TRANSPARENT;
    style.border_right_color = Rgba::TRANSPARENT;
    style.border_top_color = Rgba::TRANSPARENT;
    style.border_bottom_color = Rgba::TRANSPARENT;
  }
  if flags.contains(StyleOverrideFlags::LAYOUT_CLEAR_WIDTHS_AND_MARGINS) {
    style.width = None;
    style.min_width = None;
    style.max_width = None;
    style.margin_left = Some(Length::px(0.0));
    style.margin_right = Some(Length::px(0.0));
    style.margin_top = Some(Length::px(0.0));
    style.margin_bottom = Some(Length::px(0.0));
  }
  if flags.contains(StyleOverrideFlags::COLLAPSE_ZERO_BORDERS) {
    style.border_left_width = Length::px(0.0);
    style.border_right_width = Length::px(0.0);
    style.border_top_width = Length::px(0.0);
    style.border_bottom_width = Length::px(0.0);
  }
  style
}

impl TableStructure {
  /// Creates a new empty table structure
  pub fn new() -> Self {
    Self {
      column_count: 0,
      row_count: 0,
      columns: Vec::new(),
      rows: Vec::new(),
      cells: Vec::new(),
      grid: Vec::new(),
      // CSS 2.1 initial value is 0; UA stylesheet may override (e.g., 2px 2px).
      border_spacing: (0.0, 0.0),
      border_collapse: BorderCollapse::Separate,
      is_fixed_layout: false,
    }
  }

  /// Builds table structure from a table box node
  ///
  /// This method analyzes the box tree structure to identify:
  /// - Table rows and row groups
  /// - Table cells with their spans
  /// - Column structure
  ///
  /// # Arguments
  ///
  /// * `table_box` - The root table box node
  ///
  /// # Returns
  ///
  /// A fully resolved TableStructure ready for layout
  #[allow(clippy::cognitive_complexity)]
  pub fn from_box_tree(table_box: &BoxNode) -> Self {
    let mut structure = TableStructure::new();

    // Extract border model and spacing from style (UA stylesheet can override)
    structure.border_collapse = table_box.style.border_collapse;
    structure.border_spacing = match table_box.style.border_collapse {
      BorderCollapse::Collapse => (0.0, 0.0),
      BorderCollapse::Separate => resolve_border_spacing(&table_box.style),
    };

    // Check for fixed layout
    structure.is_fixed_layout = matches!(table_box.style.table_layout, TableLayout::Fixed);

    // Phase 1: Count rows and discover maximum column count
    let mut current_row = 0;
    let mut max_cols = 0;
    let mut explicit_columns = 0;
    let mut row_heights: Vec<Option<SpecifiedHeight>> = Vec::new();
    let mut row_min_heights: Vec<Option<SpecifiedHeight>> = Vec::new();
    let mut row_max_heights: Vec<Option<SpecifiedHeight>> = Vec::new();
    let mut row_visibilities: Vec<Visibility> = Vec::new();
    let mut row_vertical_aligns: Vec<Option<VerticalAlign>> = Vec::new();
    let mut row_span_remaining: Vec<usize> = Vec::new();
    let mut occupied_columns: Vec<bool> = Vec::new();
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    enum RowGroupKind {
      Head,
      Body,
      Foot,
    }
    let mut header_rows: Vec<usize> = Vec::new();
    let mut body_rows: Vec<usize> = Vec::new();
    let mut footer_rows: Vec<usize> = Vec::new();

    // Collect all cells first
    let mut cell_data: Vec<(usize, usize, usize, usize, usize)> = Vec::new(); // (source_row, col, rowspan, colspan, box_idx)

    // Track explicit columns from <col>/<colgroup> to honor Tables spec that the number of columns
    // is the maximum of col elements and the actual grid.
    for child in table_box
      .children
      .iter()
      .filter(|child| child.style.running_position.is_none())
    {
      match Self::get_table_element_type(child) {
        TableElementType::Column => {
          let span = child
            .debug_info
            .as_ref()
            .map(|d| d.column_span)
            .unwrap_or(1)
            .max(1);
          explicit_columns += span;
        }
        TableElementType::ColumnGroup => {
          if child.children.is_empty() {
            let span = child
              .debug_info
              .as_ref()
              .map(|d| d.column_span)
              .unwrap_or(1)
              .max(1);
            explicit_columns += span;
          } else {
            for group_child in &child.children {
              if Self::get_table_element_type(group_child) == TableElementType::Column {
                let span = group_child
                  .debug_info
                  .as_ref()
                  .map(|d| d.column_span)
                  .unwrap_or(1)
                  .max(1);
                explicit_columns += span;
              }
            }
          }
        }
        _ => {}
      }
    }

    for (_child_idx, child) in table_box
      .children
      .iter()
      .filter(|child| child.style.running_position.is_none())
      .enumerate()
    {
      match Self::get_table_element_type(child) {
        TableElementType::RowGroup
        | TableElementType::HeaderGroup
        | TableElementType::FooterGroup => {
          let kind = match Self::get_table_element_type(child) {
            TableElementType::HeaderGroup => RowGroupKind::Head,
            TableElementType::FooterGroup => RowGroupKind::Foot,
            _ => RowGroupKind::Body,
          };
          let group_visibility = child.style.visibility;
          let group_vertical_align = if child.style.vertical_align_specified {
            Some(child.style.vertical_align)
          } else {
            None
          };
          // Process rows within the group
          for row_child in &child.children {
            if matches!(
              Self::get_table_element_type(row_child),
              TableElementType::Row
            ) {
              let spec_height = Self::length_to_specified_height_opt(
                row_child.style.height.as_ref(),
                row_child.style.font_size,
              );
              let min_h = Self::length_to_specified_height_opt(
                row_child.style.min_height.as_ref(),
                row_child.style.font_size,
              );
              let max_h = Self::length_to_specified_height_opt(
                row_child.style.max_height.as_ref(),
                row_child.style.font_size,
              );
              let row_visibility = if matches!(group_visibility, Visibility::Collapse) {
                Visibility::Collapse
              } else {
                row_child.style.visibility
              };
              let row_vertical_align = if row_child.style.vertical_align_specified {
                Some(row_child.style.vertical_align)
              } else {
                group_vertical_align
              };
              row_visibilities.push(row_visibility);
              row_heights.push(spec_height);
              row_min_heights.push(min_h);
              row_max_heights.push(max_h);
              row_vertical_aligns.push(row_vertical_align);
              match kind {
                RowGroupKind::Head => header_rows.push(current_row),
                RowGroupKind::Foot => footer_rows.push(current_row),
                RowGroupKind::Body => body_rows.push(current_row),
              }
              Self::process_row(
                row_child,
                current_row,
                &mut row_span_remaining,
                &mut occupied_columns,
                &mut max_cols,
                &mut cell_data,
              );
              current_row += 1;
            }
          }
        }
        TableElementType::Row => {
          let spec_height = Self::length_to_specified_height_opt(
            child.style.height.as_ref(),
            child.style.font_size,
          );
          let min_h = Self::length_to_specified_height_opt(
            child.style.min_height.as_ref(),
            child.style.font_size,
          );
          let max_h = Self::length_to_specified_height_opt(
            child.style.max_height.as_ref(),
            child.style.font_size,
          );
          row_visibilities.push(child.style.visibility);
          let row_vertical_align = if child.style.vertical_align_specified {
            Some(child.style.vertical_align)
          } else {
            None
          };
          row_vertical_aligns.push(row_vertical_align);
          row_heights.push(spec_height);
          row_min_heights.push(min_h);
          row_max_heights.push(max_h);
          body_rows.push(current_row);
          Self::process_row(
            child,
            current_row,
            &mut row_span_remaining,
            &mut occupied_columns,
            &mut max_cols,
            &mut cell_data,
          );
          current_row += 1;
        }
        _ => {}
      }
    }

    // Reorder rows to honor header/footer placement: headers first, then bodies, then footers (each in DOM order).
    let mut row_order: Vec<usize> = Vec::with_capacity(current_row);
    row_order.extend(header_rows);
    row_order.extend(body_rows);
    row_order.extend(footer_rows);
    // Store mapping so row order influences grid mapping and painting while source indices remain DOM-ordered.
    let mut row_index_map = vec![0usize; current_row];
    for (new_idx, old_idx) in row_order.iter().enumerate() {
      row_index_map[*old_idx] = new_idx;
    }

    let reorder_vec = |vec: &mut Vec<Option<SpecifiedHeight>>| {
      let mut reordered = Vec::with_capacity(vec.len());
      for idx in &row_order {
        reordered.push(vec.get(*idx).copied().unwrap_or(None));
      }
      *vec = reordered;
    };
    reorder_vec(&mut row_heights);
    reorder_vec(&mut row_min_heights);
    reorder_vec(&mut row_max_heights);

    let mut reordered_aligns = Vec::with_capacity(row_vertical_aligns.len());
    for idx in &row_order {
      reordered_aligns.push(row_vertical_aligns.get(*idx).copied().unwrap_or(None));
    }
    row_vertical_aligns = reordered_aligns;

    let mut reordered_vis = Vec::with_capacity(row_visibilities.len());
    for idx in &row_order {
      reordered_vis.push(
        row_visibilities
          .get(*idx)
          .copied()
          .unwrap_or(Visibility::Visible),
      );
    }
    row_visibilities = reordered_vis;

    structure.row_count = current_row;
    structure.column_count = max_cols.max(explicit_columns);

    // Initialize columns
    for i in 0..structure.column_count {
      let mut col = ColumnInfo::new(i);
      col.font_size = table_box.style.font_size;
      structure.columns.push(col);
    }

    // Apply column widths from <col>/<colgroup> if present
    let mut col_cursor = 0;
    for child in table_box
      .children
      .iter()
      .filter(|child| child.style.running_position.is_none())
    {
      match Self::get_table_element_type(child) {
        TableElementType::Column => {
          let span = child
            .debug_info
            .as_ref()
            .map(|d| d.column_span)
            .unwrap_or(1)
            .max(1);
          for _ in 0..span {
            if col_cursor >= structure.columns.len() {
              let mut col = ColumnInfo::new(col_cursor);
              col.font_size = child.style.font_size;
              structure.columns.push(col);
            }
            if let Some(col) = structure.columns.get_mut(col_cursor) {
              col.visibility = child.style.visibility;
              col.author_min_width = child.style.min_width.clone();
              col.author_max_width = child.style.max_width.clone();
              col.font_size = child.style.font_size;
              if let Some(width) = &child.style.width {
                col.specified_width = Some(Self::length_to_specified_width(width));
              }
            }
            col_cursor += 1;
          }
        }
        TableElementType::ColumnGroup => {
          let group_visibility = child.style.visibility;
          // Apply group width to contained columns (or next column if none)
          if !child.children.is_empty() {
            for group_child in &child.children {
              if Self::get_table_element_type(group_child) == TableElementType::Column {
                let span = group_child
                  .debug_info
                  .as_ref()
                  .map(|d| d.column_span)
                  .unwrap_or(1)
                  .max(1);
                for _ in 0..span {
                  if col_cursor >= structure.columns.len() {
                    let mut col = ColumnInfo::new(col_cursor);
                    col.font_size = group_child.style.font_size;
                    structure.columns.push(col);
                  }
                  if let Some(col) = structure.columns.get_mut(col_cursor) {
                    col.visibility = if matches!(group_visibility, Visibility::Collapse) {
                      Visibility::Collapse
                    } else {
                      group_child.style.visibility
                    };
                    col.author_min_width = group_child.style.min_width.clone();
                    col.author_max_width = group_child.style.max_width.clone();
                    col.font_size = group_child.style.font_size;
                    if let Some(width) = &group_child.style.width {
                      col.specified_width = Some(Self::length_to_specified_width(width));
                    }
                  }
                  col_cursor += 1;
                }
              }
            }
          } else {
            let span = child
              .debug_info
              .as_ref()
              .map(|d| d.column_span)
              .unwrap_or(1)
              .max(1);
            for _ in 0..span {
              if col_cursor >= structure.columns.len() {
                let mut col = ColumnInfo::new(col_cursor);
                col.font_size = child.style.font_size;
                structure.columns.push(col);
              }
              if let Some(col) = structure.columns.get_mut(col_cursor) {
                col.visibility = child.style.visibility;
                col.author_min_width = child.style.min_width.clone();
                col.author_max_width = child.style.max_width.clone();
                col.font_size = child.style.font_size;
                if let Some(width) = &child.style.width {
                  col.specified_width = Some(Self::length_to_specified_width(width));
                }
              }
              col_cursor += 1;
            }
          }
        }
        _ => {}
      }
    }

    // Initialize rows
    for i in 0..structure.row_count {
      let mut row = RowInfo::new(i);
      row.specified_height = row_heights.get(i).copied().unwrap_or(None);
      row.author_min_height = row_min_heights.get(i).copied().unwrap_or(None);
      row.author_max_height = row_max_heights.get(i).copied().unwrap_or(None);
      row.visibility = row_visibilities
        .get(i)
        .copied()
        .unwrap_or(Visibility::Visible);
      row.vertical_align = row_vertical_aligns.get(i).copied().unwrap_or(None);
      // Preserve the original DOM order for source_index so style lookups remain stable.
      let source_idx = row_order.get(i).copied().unwrap_or(i);
      row.source_index = source_idx;
      structure.rows.push(row);
    }

    // Initialize grid
    structure.grid = vec![vec![None; structure.column_count]; structure.row_count];
    structure.cells.reserve(cell_data.len());

    // Create cells and populate grid
    for (idx, (source_row, col, rowspan, colspan, box_idx)) in cell_data.iter().enumerate() {
      let new_row = row_index_map
        .get(*source_row)
        .copied()
        .unwrap_or(*source_row);

      let mut cell = CellInfo::new(idx, *source_row, *col);
      cell.row = new_row;
      cell.rowspan = *rowspan;
      cell.colspan = *colspan;
      cell.box_index = *box_idx;

      // Mark grid cells
      for r in new_row..(new_row + *rowspan).min(structure.row_count) {
        for c in *col..(*col + *colspan).min(structure.column_count) {
          if structure.grid[r][c].is_none() {
            structure.grid[r][c] = Some(idx);
          }
        }
      }

      structure.cells.push(cell);
    }

    structure.apply_visibility_collapse()
  }

  /// Removes rows and columns with `visibility: collapse` from the structure
  /// while preserving source indices so cell lookup and styling can map back
  /// to the original table tree.
  fn apply_visibility_collapse(self) -> Self {
    let mut row_map: Vec<Option<usize>> = Vec::with_capacity(self.row_count);
    let mut next_row = 0usize;
    for row in &self.rows {
      if matches!(row.visibility, Visibility::Collapse) {
        row_map.push(None);
      } else {
        row_map.push(Some(next_row));
        next_row += 1;
      }
    }

    let mut col_map: Vec<Option<usize>> = Vec::with_capacity(self.column_count);
    let mut next_col = 0usize;
    for col in &self.columns {
      if matches!(col.visibility, Visibility::Collapse) {
        col_map.push(None);
      } else {
        col_map.push(Some(next_col));
        next_col += 1;
      }
    }

    if next_row == self.row_count && next_col == self.column_count {
      return self;
    }

    let mut new_structure = TableStructure {
      column_count: next_col,
      row_count: next_row,
      columns: Vec::with_capacity(next_col),
      rows: Vec::with_capacity(next_row),
      cells: Vec::with_capacity(self.cells.len()),
      grid: vec![vec![None; next_col]; next_row],
      border_spacing: self.border_spacing,
      border_collapse: self.border_collapse,
      is_fixed_layout: self.is_fixed_layout,
    };

    for (old_idx, mut col) in self.columns.into_iter().enumerate() {
      if let Some(new_idx) = col_map.get(old_idx).and_then(|m| *m) {
        col.index = new_idx;
        new_structure.columns.push(col);
      }
    }

    for (old_idx, mut row) in self.rows.into_iter().enumerate() {
      if let Some(new_idx) = row_map.get(old_idx).and_then(|m| *m) {
        row.index = new_idx;
        new_structure.rows.push(row);
      }
    }

    for cell in self.cells {
      let Some(new_row) = row_map.get(cell.row).and_then(|m| *m) else {
        continue;
      };
      let row_span_end = (cell.row + cell.rowspan).min(row_map.len());
      let mut visible_rows = 0usize;
      for r in cell.row..row_span_end {
        if row_map.get(r).and_then(|m| *m).is_some() {
          visible_rows += 1;
        }
      }
      if visible_rows == 0 {
        continue;
      }

      let col_span_end = (cell.col + cell.colspan).min(col_map.len());
      let mut first_visible_col = None;
      let mut visible_cols = 0usize;
      for c in cell.col..col_span_end {
        if let Some(mapped) = col_map.get(c).and_then(|m| *m) {
          if first_visible_col.is_none() {
            first_visible_col = Some(mapped);
          }
          visible_cols += 1;
        }
      }
      let Some(start_col) = first_visible_col else {
        continue;
      };

      let mut new_cell = cell;
      new_cell.row = new_row;
      new_cell.col = start_col;
      new_cell.rowspan = visible_rows;
      new_cell.colspan = visible_cols;
      new_cell.index = new_structure.cells.len();
      new_structure.cells.push(new_cell);
    }

    for cell in &new_structure.cells {
      let row_end = (cell.row + cell.rowspan).min(new_structure.row_count);
      let col_end = (cell.col + cell.colspan).min(new_structure.column_count);
      for r in cell.row..row_end {
        for c in cell.col..col_end {
          if let Some(slot) = new_structure.grid.get_mut(r).and_then(|row| row.get_mut(c)) {
            *slot = Some(cell.index);
          }
        }
      }
    }

    new_structure
  }

  /// Gets the table element type from a box node
  fn get_table_element_type(node: &BoxNode) -> TableElementType {
    use crate::style::display::Display;
    use crate::tree::box_tree::AnonymousType;
    use crate::tree::box_tree::BoxType;

    // Prefer computed display over tag hints
    match node.style.display {
      Display::Table | Display::InlineTable => return TableElementType::Table,
      Display::TableRow => return TableElementType::Row,
      Display::TableCell => return TableElementType::Cell,
      Display::TableCaption => return TableElementType::Caption,
      Display::TableRowGroup => return TableElementType::RowGroup,
      Display::TableHeaderGroup => return TableElementType::HeaderGroup,
      Display::TableFooterGroup => return TableElementType::FooterGroup,
      Display::TableColumn => return TableElementType::Column,
      Display::TableColumnGroup => return TableElementType::ColumnGroup,
      _ => {}
    }

    // Fall back to anonymous boxes
    if let BoxType::Anonymous(anon) = &node.box_type {
      return match anon.anonymous_type {
        AnonymousType::TableWrapper => TableElementType::Table,
        AnonymousType::TableRow => TableElementType::Row,
        AnonymousType::TableCell => TableElementType::Cell,
        AnonymousType::TableRowGroup => TableElementType::RowGroup,
        _ => TableElementType::Unknown,
      };
    }

    // As a last resort, use debug info tags if present
    if let Some(ref debug_info) = node.debug_info {
      if let Some(ref tag) = debug_info.tag_name {
        let tag_lower = tag.to_lowercase();
        return match tag_lower.as_str() {
          "table" => TableElementType::Table,
          "thead" => TableElementType::HeaderGroup,
          "tbody" => TableElementType::RowGroup,
          "tfoot" => TableElementType::FooterGroup,
          "tr" => TableElementType::Row,
          "td" | "th" => TableElementType::Cell,
          "caption" => TableElementType::Caption,
          "col" => TableElementType::Column,
          "colgroup" => TableElementType::ColumnGroup,
          _ => TableElementType::Unknown,
        };
      }
    }

    TableElementType::Unknown
  }

  /// Processes a table row and records cell information with collision-aware placement.
  fn process_row(
    row: &BoxNode,
    row_idx: usize,
    row_span_remaining: &mut Vec<usize>,
    occupied: &mut Vec<bool>,
    max_cols: &mut usize,
    cell_data: &mut Vec<(usize, usize, usize, usize, usize)>,
  ) {
    if occupied.len() < row_span_remaining.len() {
      occupied.resize(row_span_remaining.len(), false);
    }
    for (idx, span) in row_span_remaining.iter_mut().enumerate() {
      let active = *span > 0;
      if active {
        *span -= 1;
      }
      occupied[idx] = active;
    }
    if occupied.len() > row_span_remaining.len() {
      for idx in row_span_remaining.len()..occupied.len() {
        occupied[idx] = false;
      }
    }

    for (cell_idx, cell_child) in row.children.iter().enumerate() {
      if matches!(
        Self::get_table_element_type(cell_child),
        TableElementType::Cell
      ) {
        // Extract colspan and rowspan from debug info and normalize to at least 1.
        let (mut colspan, mut rowspan) = if let Some(ref debug_info) = cell_child.debug_info {
          (debug_info.colspan, debug_info.rowspan)
        } else {
          (1, 1)
        };
        colspan = colspan.max(1);
        rowspan = rowspan.max(1);

        // Find the first free block of columns that can fit the span in linear time.
        let mut run_len = 0usize;
        let mut col_cursor = 0usize;
        let start_col = loop {
          if col_cursor >= occupied.len() {
            occupied.push(false);
            row_span_remaining.push(0);
          }
          if !occupied[col_cursor] {
            run_len += 1;
            if run_len == colspan {
              break col_cursor + 1 - run_len;
            }
          } else {
            run_len = 0;
          }
          col_cursor += 1;
        };

        cell_data.push((row_idx, start_col, rowspan, colspan, cell_idx));
        *max_cols = (*max_cols).max(start_col + colspan);

        // Mark the covered columns as occupied for this row and future rows.
        let span_rows = rowspan.saturating_sub(1);
        let col_end = start_col + colspan;
        if occupied.len() < col_end {
          occupied.resize(col_end, false);
        }
        if row_span_remaining.len() < col_end {
          row_span_remaining.resize(col_end, 0);
        }
        for col in start_col..col_end {
          occupied[col] = true;
          if let Some(slot) = row_span_remaining.get_mut(col) {
            *slot = (*slot).max(span_rows);
          }
        }
      }
    }
  }

  /// Gets the cell at a grid position, if any
  pub fn get_cell_at(&self, row: usize, col: usize) -> Option<&CellInfo> {
    self
      .grid
      .get(row)
      .and_then(|r| r.get(col))
      .and_then(|idx| idx.as_ref())
      .and_then(|idx| self.cells.get(*idx))
  }

  /// Returns the total width of border spacing
  pub fn total_horizontal_spacing(&self) -> f32 {
    if self.column_count == 0 {
      return 0.0;
    }
    self.border_spacing.0 * self.column_count as f32
  }

  /// Returns the total height of border spacing
  pub fn total_vertical_spacing(&self) -> f32 {
    if self.row_count == 0 {
      return 0.0;
    }
    self.border_spacing.1 * self.row_count as f32
  }

  fn length_to_specified_width(length: &crate::style::values::Length) -> SpecifiedWidth {
    use crate::style::values::LengthUnit;
    match length.unit {
      LengthUnit::Percent => SpecifiedWidth::Percent(length.value),
      _ => SpecifiedWidth::Fixed(length.to_px()),
    }
  }

  fn length_to_specified_height(
    length: &crate::style::values::Length,
    font_size: f32,
  ) -> SpecifiedHeight {
    use crate::style::values::LengthUnit;
    match length.unit {
      LengthUnit::Percent => SpecifiedHeight::Percent(length.value),
      LengthUnit::Em | LengthUnit::Rem => SpecifiedHeight::Fixed(length.value * font_size),
      _ if length.unit.is_absolute() => SpecifiedHeight::Fixed(length.to_px()),
      _ => SpecifiedHeight::Auto,
    }
  }

  fn length_to_specified_height_opt(
    length: Option<&crate::style::values::Length>,
    font_size: f32,
  ) -> Option<SpecifiedHeight> {
    length.map(|len| Self::length_to_specified_height(len, font_size))
  }
}

impl Default for TableStructure {
  fn default() -> Self {
    Self::new()
  }
}

fn resolve_border_spacing_length(length: &crate::style::values::Length, font_size: f32) -> f32 {
  let resolved = if length.unit.is_absolute() {
    Some(length.to_px())
  } else {
    // Border spacing should resolve for absolute and font-relative values even when viewport
    // dimensions are unavailable; treat percentage/viewport-based spacing as zero in that case.
    length.resolve_with_context(Some(0.0), 0.0, 0.0, font_size, font_size)
  }
  .unwrap_or(0.0);

  resolved.max(0.0)
}

fn resolve_border_spacing(style: &crate::style::ComputedStyle) -> (f32, f32) {
  let font_size = style.font_size;
  (
    resolve_border_spacing_length(&style.border_spacing_horizontal, font_size),
    resolve_border_spacing_length(&style.border_spacing_vertical, font_size),
  )
}

fn resolve_length_against(
  length: &crate::style::values::Length,
  font_size: f32,
  containing_width: Option<f32>,
) -> Option<f32> {
  if length.unit == LengthUnit::Calc {
    return length.resolve_with_context(
      containing_width,
      containing_width.unwrap_or(f32::NAN),
      f32::NAN,
      font_size,
      font_size,
    );
  }
  match length.unit {
    LengthUnit::Percent => containing_width.map(|w| (length.value / 100.0) * w),
    LengthUnit::Em | LengthUnit::Rem => Some(length.value * font_size),
    _ if length.unit.is_absolute() => Some(length.to_px()),
    _ => None,
  }
}

fn resolve_opt_length_against(
  length: Option<&crate::style::values::Length>,
  font_size: f32,
  containing_width: Option<f32>,
) -> Option<f32> {
  length.and_then(|l| resolve_length_against(l, font_size, containing_width))
}

fn clamp_to_min_max(value: f32, min: Option<f32>, max: Option<f32>) -> f32 {
  let mut v = value;
  if let Some(min) = min {
    v = v.max(min);
  }
  if let Some(max) = max {
    v = v.min(max);
  }
  v
}

fn horizontal_padding_and_borders(
  style: &crate::style::ComputedStyle,
  percent_base: Option<f32>,
) -> f32 {
  let resolve_abs = |l: &crate::style::values::Length| match l.unit {
    LengthUnit::Percent => percent_base
      .map(|base| (l.value / 100.0) * base)
      .unwrap_or(0.0),
    _ if l.unit.is_absolute() => l.to_px(),
    _ => l.value,
  };

  resolve_abs(&style.padding_left)
    + resolve_abs(&style.padding_right)
    + resolve_abs(&style.border_left_width)
    + resolve_abs(&style.border_right_width)
}

fn horizontal_padding(style: &crate::style::ComputedStyle, percent_base: Option<f32>) -> f32 {
  let resolve_abs = |l: &crate::style::values::Length| match l.unit {
    LengthUnit::Percent => percent_base
      .map(|base| (l.value / 100.0) * base)
      .unwrap_or(0.0),
    _ if l.unit.is_absolute() => l.to_px(),
    _ => l.value,
  };

  resolve_abs(&style.padding_left) + resolve_abs(&style.padding_right)
}

/// Collects table row boxes in DOM/source order, matching `CellInfo::source_row`.
///
/// Running-position children are skipped to stay consistent with the table structure analysis.
fn collect_source_rows<'a>(table_box: &'a BoxNode) -> Vec<&'a BoxNode> {
  let mut rows = Vec::new();
  for child in table_box
    .children
    .iter()
    .filter(|child| child.style.running_position.is_none())
  {
    match TableStructure::get_table_element_type(child) {
      TableElementType::RowGroup
      | TableElementType::HeaderGroup
      | TableElementType::FooterGroup => {
        for row_child in &child.children {
          if TableStructure::get_table_element_type(row_child) == TableElementType::Row {
            rows.push(row_child);
          }
        }
      }
      TableElementType::Row => rows.push(child),
      _ => {}
    }
  }
  rows
}

/// Computes the grid line centers and item start offsets for the collapsed border model.
/// Line widths are centered on the grid lines; outer lines contribute half their width to
/// the measured extent so the remaining half may spill into the table's margin area
/// (CSS 2.2 §17.6.2).
fn collapsed_line_positions(
  sizes: &[f32],
  line_widths: &[f32],
  padding_start: f32,
  padding_end: f32,
) -> (Vec<f32>, Vec<f32>, f32) {
  debug_assert!(line_widths.len() >= sizes.len().saturating_add(1));

  let mut line_pos = Vec::with_capacity(sizes.len() + 1);
  let mut offsets = Vec::with_capacity(sizes.len());
  let mut cursor = padding_start;
  line_pos.push(cursor);

  for (idx, size) in sizes.iter().enumerate() {
    let prev_half = line_widths.get(idx).copied().unwrap_or(0.0) * 0.5;
    let next_half = line_widths.get(idx + 1).copied().unwrap_or(0.0) * 0.5;
    offsets.push(cursor + prev_half);
    cursor += prev_half + size + next_half;
    line_pos.push(cursor);
  }

  let extent = cursor + padding_end;
  (line_pos, offsets, extent)
}

/// Splits a spanning cell's baseline so the portion below the baseline is
/// satisfied by later rows when possible, only charging the first row for the
/// remaining descent.
fn spanning_baseline_allocation(
  cell_height: f32,
  baseline: f32,
  span_start: usize,
  span_end: usize,
  row_heights: &[f32],
) -> (f32, f32) {
  let clamped_baseline = baseline.min(cell_height);
  let below = (cell_height - clamped_baseline).max(0.0);
  let other_rows = if span_end > span_start + 1 && span_end <= row_heights.len() {
    row_heights[span_start + 1..span_end].iter().sum::<f32>()
  } else {
    0.0
  };
  let bottom_here = (below - other_rows).max(0.0);
  (clamped_baseline, bottom_here.min(cell_height))
}

type ResolvedBorder = crate::tree::fragment_tree::CollapsedBorderSegment;

#[derive(Debug, Clone)]
pub struct CollapsedBorders {
  /// Vertical grid lines: index is column boundary (0..=columns), inner vec is per row segment.
  vertical: Vec<Vec<ResolvedBorder>>,
  /// Horizontal grid lines: index is row boundary (0..=rows), inner vec is per column segment.
  horizontal: Vec<Vec<ResolvedBorder>>,
  /// Corner joins: index [row][col] for grid junctions.
  corners: Vec<Vec<ResolvedBorder>>,
}

#[cfg(test)]
thread_local! {
  static COLLAPSED_BORDER_COMPUTE_COUNT: std::cell::Cell<usize> =
    std::cell::Cell::new(0);
}

#[cfg(test)]
fn reset_collapsed_border_compute_count() {
  COLLAPSED_BORDER_COMPUTE_COUNT.with(|c| c.set(0));
}

#[cfg(test)]
fn collapsed_border_compute_count() -> usize {
  COLLAPSED_BORDER_COMPUTE_COUNT.with(|c| c.get())
}

/// Resolve the border widths for a table in collapsed border model.
/// Returns resolved borders for vertical and horizontal grid lines.
/// Lengths: vertical = columns + 1, horizontal = rows + 1.
#[allow(clippy::cognitive_complexity)]
fn compute_collapsed_borders(table_box: &BoxNode, structure: &TableStructure) -> CollapsedBorders {
  #[cfg(test)]
  COLLAPSED_BORDER_COMPUTE_COUNT.with(|c| c.set(c.get() + 1));
  #[derive(Clone, Copy)]
  enum BorderOrigin {
    Table,
    ColumnGroup,
    Column,
    RowGroup,
    Row,
    Cell,
  }

  #[derive(Clone, Copy)]
  struct BorderCandidate {
    width: f32,
    style: BorderStyle,
    color: Rgba,
    origin: BorderOrigin,
    source_order: u32,
    #[allow(dead_code)]
    row: usize,
    #[allow(dead_code)]
    col: usize,
  }

  impl BorderCandidate {
    fn none() -> Self {
      Self {
        width: 0.0,
        style: BorderStyle::None,
        color: Rgba::TRANSPARENT,
        origin: BorderOrigin::Table,
        source_order: 0,
        row: 0,
        col: 0,
      }
    }
  }

  fn style_rank(style: BorderStyle) -> u8 {
    // CSS2.1 §17.6.2.1: hidden > double > solid > dashed > dotted > ridge > outset > groove > inset > none
    match style {
      BorderStyle::Hidden => 9,
      BorderStyle::Double => 8,
      BorderStyle::Solid => 7,
      BorderStyle::Dashed => 6,
      BorderStyle::Dotted => 5,
      BorderStyle::Ridge => 4,
      BorderStyle::Outset => 3,
      BorderStyle::Groove => 2,
      BorderStyle::Inset => 1,
      BorderStyle::None => 0,
    }
  }

  fn origin_priority(origin: BorderOrigin) -> u8 {
    match origin {
      BorderOrigin::Cell => 6,
      BorderOrigin::Row => 5,
      BorderOrigin::RowGroup => 4,
      BorderOrigin::Column => 3,
      BorderOrigin::ColumnGroup => 2,
      BorderOrigin::Table => 1,
    }
  }

  fn candidate_better(
    a: &BorderCandidate,
    b: &BorderCandidate,
    direction: Direction,
    orientation: Option<bool>,
  ) -> bool {
    if a.style == BorderStyle::Hidden {
      if b.style != BorderStyle::Hidden {
        return true;
      }
    } else if b.style == BorderStyle::Hidden {
      return false;
    }

    let a_style = style_rank(a.style);
    let b_style = style_rank(b.style);
    if a_style != b_style {
      return a_style > b_style;
    }

    let width_epsilon = 1e-6f32;
    if (a.width - b.width).abs() > width_epsilon {
      return a.width > b.width;
    }

    let a_origin = origin_priority(a.origin);
    let b_origin = origin_priority(b.origin);
    if a_origin != b_origin {
      return a_origin > b_origin;
    }

    if let Some(is_vertical) = orientation {
      if is_vertical {
        match direction {
          Direction::Rtl => {
            if a.col != b.col {
              return a.col > b.col;
            }
          }
          Direction::Ltr => {
            if a.col != b.col {
              return a.col < b.col;
            }
          }
        }
      } else if a.row != b.row {
        return a.row < b.row;
      }
    }

    a.source_order > b.source_order
  }

  fn length_to_px(len: &crate::style::values::Length) -> f32 {
    if len.unit.is_absolute() {
      len.to_px()
    } else {
      len.value
    }
  }

  fn resolved_border_width(style: BorderStyle, len: &crate::style::values::Length) -> f32 {
    if matches!(style, BorderStyle::None | BorderStyle::Hidden) {
      return 0.0;
    }
    let width = length_to_px(len);
    // CSS 2.1 §17.6.2.2: double borders must be wide enough to render as three strokes.
    if matches!(style, BorderStyle::Double) {
      width.max(3.0)
    } else {
      width
    }
  }

  let direction = table_box.style.direction;
  let rows = structure.row_count;
  let columns = structure.column_count;
  let vertical_line_count = columns + 1;
  let horizontal_line_count = rows + 1;

  let mut vertical: Vec<BorderCandidate> =
    vec![BorderCandidate::none(); vertical_line_count * rows];
  let mut horizontal: Vec<BorderCandidate> =
    vec![BorderCandidate::none(); horizontal_line_count * columns];

  let vertical_index = |col: usize, row: usize| col * rows + row;
  let horizontal_index = |row: usize, col: usize| row * columns + col;

  let max_source_row = structure
    .rows
    .iter()
    .map(|r| r.source_index)
    .max()
    .unwrap_or(0);
  let mut source_row_to_visible = if structure.rows.is_empty() {
    Vec::new()
  } else {
    vec![None; max_source_row + 1]
  };
  for row in &structure.rows {
    if row.source_index >= source_row_to_visible.len() {
      source_row_to_visible.resize(row.source_index + 1, None);
    }
    source_row_to_visible[row.source_index] = Some(row.index);
  }

  let max_source_col = structure
    .columns
    .iter()
    .map(|c| c.source_index)
    .max()
    .unwrap_or(0);
  let mut source_col_to_visible = if structure.columns.is_empty() {
    Vec::new()
  } else {
    vec![None; max_source_col + 1]
  };
  for col in &structure.columns {
    if col.source_index >= source_col_to_visible.len() {
      source_col_to_visible.resize(col.source_index + 1, None);
    }
    source_col_to_visible[col.source_index] = Some(col.index);
  }

  // Gather row, row-group, column, and column-group styles with document order.
  let mut row_styles: Vec<Option<(std::sync::Arc<crate::style::ComputedStyle>, u32)>> =
    vec![None; rows];
  let mut row_groups: Vec<(
    usize,
    usize,
    std::sync::Arc<crate::style::ComputedStyle>,
    u32,
  )> = Vec::new();
  let mut column_styles: Vec<Option<(std::sync::Arc<crate::style::ComputedStyle>, u32)>> =
    vec![None; columns];
  let mut column_groups: Vec<(
    usize,
    usize,
    std::sync::Arc<crate::style::ComputedStyle>,
    u32,
  )> = Vec::new();

  let mut source_counter: u32 = 1;
  let mut source_row_idx = 0usize;
  let mut source_col_idx = 0usize;

  for child in table_box
    .children
    .iter()
    .filter(|child| child.style.running_position.is_none())
  {
    match TableStructure::get_table_element_type(child) {
      TableElementType::RowGroup
      | TableElementType::HeaderGroup
      | TableElementType::FooterGroup => {
        let group_order = source_counter;
        source_counter += 1;
        let mut first_visible = None;
        let mut last_visible = None;
        for row_child in &child.children {
          if TableStructure::get_table_element_type(row_child) == TableElementType::Row {
            if let Some(visible) = source_row_to_visible.get(source_row_idx).and_then(|m| *m) {
              row_styles[visible] = Some((row_child.style.clone(), source_counter));
              source_counter += 1;
              first_visible.get_or_insert(visible);
              last_visible = Some(visible);
            }
            source_row_idx += 1;
          }
        }
        if let (Some(start), Some(end)) = (first_visible, last_visible) {
          row_groups.push((start, end + 1, child.style.clone(), group_order));
        }
      }
      TableElementType::Row => {
        if let Some(visible) = source_row_to_visible.get(source_row_idx).and_then(|m| *m) {
          row_styles[visible] = Some((child.style.clone(), source_counter));
          source_counter += 1;
        }
        source_row_idx += 1;
      }
      TableElementType::Column => {
        if let Some(visible) = source_col_to_visible.get(source_col_idx).and_then(|m| *m) {
          column_styles[visible] = Some((child.style.clone(), source_counter));
          source_counter += 1;
        }
        source_col_idx += 1;
      }
      TableElementType::ColumnGroup => {
        let group_order = source_counter;
        source_counter += 1;
        let mut first_visible = None;
        let mut last_visible = None;
        if child.children.is_empty() {
          if let Some(visible) = source_col_to_visible.get(source_col_idx).and_then(|m| *m) {
            column_styles[visible] = column_styles[visible]
              .take()
              .or_else(|| Some((child.style.clone(), source_counter)));
            source_counter += 1;
            first_visible.get_or_insert(visible);
            last_visible = Some(visible);
          }
          source_col_idx += 1;
        } else {
          for group_child in &child.children {
            if TableStructure::get_table_element_type(group_child) == TableElementType::Column {
              if let Some(visible) = source_col_to_visible.get(source_col_idx).and_then(|m| *m) {
                column_styles[visible] = Some((group_child.style.clone(), source_counter));
                source_counter += 1;
                first_visible.get_or_insert(visible);
                last_visible = Some(visible);
              }
              source_col_idx += 1;
            }
          }
        }
        if let (Some(start), Some(end)) = (first_visible, last_visible) {
          column_groups.push((start, end + 1, child.style.clone(), group_order));
        }
      }
      _ => {}
    }
  }

  for entry in row_styles.iter_mut() {
    if entry.is_none() {
      *entry = Some((table_box.style.clone(), source_counter));
      source_counter += 1;
    }
  }

  let mut apply_vertical = |col_idx: usize,
                            row_start: usize,
                            row_end: usize,
                            style: BorderStyle,
                            width: &crate::style::values::Length,
                            color: &Rgba,
                            origin: BorderOrigin,
                            source_order: u32,
                            origin_row: usize,
                            origin_col: usize| {
    if col_idx >= vertical_line_count || row_start >= row_end || rows == 0 {
      return;
    }
    let candidate = BorderCandidate {
      width: resolved_border_width(style, width),
      style,
      color: *color,
      origin,
      source_order,
      row: origin_row,
      col: origin_col,
    };
    let end = row_end.min(rows);
    for row in row_start..end {
      let slot = &mut vertical[vertical_index(col_idx, row)];
      if candidate_better(&candidate, slot, direction, Some(true)) {
        *slot = candidate;
      }
    }
  };

  let mut apply_horizontal = |row_idx: usize,
                              col_start: usize,
                              col_end: usize,
                              style: BorderStyle,
                              width: &crate::style::values::Length,
                              color: &Rgba,
                              origin: BorderOrigin,
                              source_order: u32,
                              origin_row: usize,
                              origin_col: usize| {
    if row_idx >= horizontal_line_count || col_start >= col_end || columns == 0 {
      return;
    }
    let candidate = BorderCandidate {
      width: resolved_border_width(style, width),
      style,
      color: *color,
      origin,
      source_order,
      row: origin_row,
      col: origin_col,
    };
    let end = col_end.min(columns);
    for col in col_start..end {
      let slot = &mut horizontal[horizontal_index(row_idx, col)];
      if candidate_better(&candidate, slot, direction, Some(false)) {
        *slot = candidate;
      }
    }
  };

  // Table outer borders
  let tstyle = &table_box.style;
  apply_vertical(
    0,
    0,
    rows,
    tstyle.border_left_style,
    &tstyle.border_left_width,
    &tstyle.border_left_color,
    BorderOrigin::Table,
    0,
    0,
    0,
  );
  apply_vertical(
    columns,
    0,
    rows,
    tstyle.border_right_style,
    &tstyle.border_right_width,
    &tstyle.border_right_color,
    BorderOrigin::Table,
    0,
    0,
    columns,
  );
  apply_horizontal(
    0,
    0,
    columns,
    tstyle.border_top_style,
    &tstyle.border_top_width,
    &tstyle.border_top_color,
    BorderOrigin::Table,
    0,
    0,
    0,
  );
  apply_horizontal(
    rows,
    0,
    columns,
    tstyle.border_bottom_style,
    &tstyle.border_bottom_width,
    &tstyle.border_bottom_color,
    BorderOrigin::Table,
    0,
    rows,
    0,
  );

  // Row group borders
  for (start, end, style, order) in &row_groups {
    apply_horizontal(
      *start,
      0,
      columns,
      style.border_top_style,
      &style.border_top_width,
      &style.border_top_color,
      BorderOrigin::RowGroup,
      *order,
      *start,
      0,
    );
    apply_horizontal(
      *end,
      0,
      columns,
      style.border_bottom_style,
      &style.border_bottom_width,
      &style.border_bottom_color,
      BorderOrigin::RowGroup,
      *order,
      *start,
      0,
    );
  }

  // Row borders
  for (row_idx, entry) in row_styles.iter().enumerate() {
    if let Some((style, order)) = entry {
      apply_horizontal(
        row_idx,
        0,
        columns,
        style.border_top_style,
        &style.border_top_width,
        &style.border_top_color,
        BorderOrigin::Row,
        *order,
        row_idx,
        0,
      );
      apply_horizontal(
        row_idx + 1,
        0,
        columns,
        style.border_bottom_style,
        &style.border_bottom_width,
        &style.border_bottom_color,
        BorderOrigin::Row,
        *order,
        row_idx,
        0,
      );
    }
  }

  // Column group borders
  for (start, end, style, order) in &column_groups {
    apply_vertical(
      *start,
      0,
      rows,
      style.border_left_style,
      &style.border_left_width,
      &style.border_left_color,
      BorderOrigin::ColumnGroup,
      *order,
      0,
      *start,
    );
    apply_vertical(
      *end,
      0,
      rows,
      style.border_right_style,
      &style.border_right_width,
      &style.border_right_color,
      BorderOrigin::ColumnGroup,
      *order,
      0,
      *start,
    );
  }

  // Column borders
  for (col_idx, col_style) in column_styles.iter().enumerate() {
    if let Some((style, order)) = col_style {
      apply_vertical(
        col_idx,
        0,
        rows,
        style.border_left_style,
        &style.border_left_width,
        &style.border_left_color,
        BorderOrigin::Column,
        *order,
        0,
        col_idx,
      );
      apply_vertical(
        col_idx + 1,
        0,
        rows,
        style.border_right_style,
        &style.border_right_width,
        &style.border_right_color,
        BorderOrigin::Column,
        *order,
        0,
        col_idx,
      );
    }
  }

  // Cells
  let source_rows = collect_source_rows(table_box);
  for cell in &structure.cells {
    let Some(row) = source_rows.get(cell.source_row) else {
      continue;
    };
    let Some(cell_box) = row.children.get(cell.box_index) else {
      continue;
    };
    // In the collapsed border model the `empty-cells` property has no effect (CSS 2.1 §17.6.1),
    // so even visually empty cells participate in border conflict resolution.
    let style = &cell_box.style;
    let start_col = cell.col;
    let end_col = (cell.col + cell.colspan).min(columns);
    let start_row = cell.row;
    let end_row = (cell.row + cell.rowspan).min(rows);

    apply_vertical(
      start_col,
      start_row,
      end_row,
      style.border_left_style,
      &style.border_left_width,
      &style.border_left_color,
      BorderOrigin::Cell,
      cell.index as u32,
      cell.row,
      cell.col,
    );
    apply_vertical(
      end_col,
      start_row,
      end_row,
      style.border_right_style,
      &style.border_right_width,
      &style.border_right_color,
      BorderOrigin::Cell,
      cell.index as u32,
      cell.row,
      cell.col,
    );
    apply_horizontal(
      start_row,
      start_col,
      end_col,
      style.border_top_style,
      &style.border_top_width,
      &style.border_top_color,
      BorderOrigin::Cell,
      cell.index as u32,
      cell.row,
      cell.col,
    );
    apply_horizontal(
      end_row,
      start_col,
      end_col,
      style.border_bottom_style,
      &style.border_bottom_width,
      &style.border_bottom_color,
      BorderOrigin::Cell,
      cell.index as u32,
      cell.row,
      cell.col,
    );
  }

  let candidate_to_resolved = |candidate: BorderCandidate| {
    if matches!(candidate.style, BorderStyle::None | BorderStyle::Hidden) || candidate.width <= 0.0
    {
      ResolvedBorder::none()
    } else {
      ResolvedBorder {
        width: candidate.width,
        style: candidate.style,
        color: candidate.color,
      }
    }
  };

  let mut resolved_vertical: Vec<Vec<ResolvedBorder>> = Vec::with_capacity(vertical_line_count);
  for col in 0..vertical_line_count {
    let mut line: Vec<ResolvedBorder> = Vec::with_capacity(rows);
    for row in 0..rows {
      line.push(candidate_to_resolved(vertical[vertical_index(col, row)]));
    }
    resolved_vertical.push(line);
  }

  let mut resolved_horizontal: Vec<Vec<ResolvedBorder>> = Vec::with_capacity(horizontal_line_count);
  for row in 0..horizontal_line_count {
    let mut line: Vec<ResolvedBorder> = Vec::with_capacity(columns);
    for col in 0..columns {
      line.push(candidate_to_resolved(
        horizontal[horizontal_index(row, col)],
      ));
    }
    resolved_horizontal.push(line);
  }

  let mut resolved_corners: Vec<Vec<ResolvedBorder>> = Vec::with_capacity(rows + 1);
  for r in 0..=rows {
    let mut row_vec: Vec<ResolvedBorder> = Vec::with_capacity(columns + 1);
    for c in 0..=columns {
      let mut best: Option<BorderCandidate> = None;
      if rows > 0 && c < vertical_line_count && r > 0 {
        let candidate = vertical[vertical_index(c, r - 1)];
        if best.map_or(true, |current| {
          candidate_better(&candidate, &current, direction, None)
        }) {
          best = Some(candidate);
        }
      }
      if rows > 0 && c < vertical_line_count && r < rows {
        let candidate = vertical[vertical_index(c, r)];
        if best.map_or(true, |current| {
          candidate_better(&candidate, &current, direction, None)
        }) {
          best = Some(candidate);
        }
      }
      if columns > 0 && r < horizontal_line_count && c > 0 {
        let candidate = horizontal[horizontal_index(r, c - 1)];
        if best.map_or(true, |current| {
          candidate_better(&candidate, &current, direction, None)
        }) {
          best = Some(candidate);
        }
      }
      if columns > 0 && r < horizontal_line_count && c < columns {
        let candidate = horizontal[horizontal_index(r, c)];
        if best.map_or(true, |current| {
          candidate_better(&candidate, &current, direction, None)
        }) {
          best = Some(candidate);
        }
      }
      row_vec.push(candidate_to_resolved(
        best.unwrap_or_else(BorderCandidate::none),
      ));
    }
    resolved_corners.push(row_vec);
  }

  CollapsedBorders {
    vertical: resolved_vertical,
    horizontal: resolved_horizontal,
    corners: resolved_corners,
  }
}

#[cfg(test)]
pub fn compute_collapsed_borders_for_test(
  table_box: &BoxNode,
  structure: &TableStructure,
) -> CollapsedBorders {
  compute_collapsed_borders(table_box, structure)
}

fn build_table_collapsed_borders_metadata(
  structure: &TableStructure,
  collapsed_borders: &CollapsedBorders,
  column_line_pos: &[f32],
  row_line_pos: &[f32],
  vertical_line_max: &[f32],
  horizontal_line_max: &[f32],
) -> TableCollapsedBorders {
  let mut vertical_borders = Vec::with_capacity((structure.column_count + 1) * structure.row_count);
  for column in &collapsed_borders.vertical {
    vertical_borders.extend(column.iter().copied());
  }

  let mut horizontal_borders =
    Vec::with_capacity((structure.row_count + 1) * structure.column_count);
  for row in &collapsed_borders.horizontal {
    horizontal_borders.extend(row.iter().copied());
  }

  let mut corner_borders =
    Vec::with_capacity((structure.row_count + 1) * (structure.column_count + 1));
  for row in &collapsed_borders.corners {
    corner_borders.extend(row.iter().copied());
  }

  let max_corner_half = corner_borders
    .iter()
    .map(|c| c.width * 0.5)
    .fold(0.0f32, f32::max);
  let min_x = column_line_pos.first().copied().unwrap_or(0.0)
    - (vertical_line_max.first().copied().unwrap_or(0.0) * 0.5).max(max_corner_half);
  let max_x = column_line_pos.last().copied().unwrap_or(0.0)
    + (vertical_line_max.last().copied().unwrap_or(0.0) * 0.5).max(max_corner_half);
  let min_y = row_line_pos.first().copied().unwrap_or(0.0)
    - (horizontal_line_max.first().copied().unwrap_or(0.0) * 0.5).max(max_corner_half);
  let max_y = row_line_pos.last().copied().unwrap_or(0.0)
    + (horizontal_line_max.last().copied().unwrap_or(0.0) * 0.5).max(max_corner_half);

  TableCollapsedBorders {
    column_count: structure.column_count,
    row_count: structure.row_count,
    column_line_positions: column_line_pos.to_vec(),
    row_line_positions: row_line_pos.to_vec(),
    vertical_borders,
    horizontal_borders,
    corner_borders,
    vertical_line_base: vertical_line_max.to_vec(),
    horizontal_line_base: horizontal_line_max.to_vec(),
    paint_bounds: Rect::from_xywh(
      min_x,
      min_y,
      (max_x - min_x).max(0.0),
      (max_y - min_y).max(0.0),
    ),
  }
}

fn find_first_baseline(fragment: &FragmentNode, parent_offset: f32) -> Option<f32> {
  let origin = parent_offset + fragment.bounds.y();
  if let Some(baseline) = fragment.baseline {
    return Some(origin + baseline);
  }
  match &fragment.content {
    FragmentContent::Line { baseline } => return Some(origin + *baseline),
    FragmentContent::Text {
      baseline_offset, ..
    } => return Some(origin + *baseline_offset),
    _ => {}
  }

  for child in fragment.children.iter() {
    if let Some(b) = find_first_baseline(child, origin) {
      return Some(b);
    }
  }

  None
}

fn cell_baseline(fragment: &FragmentNode) -> Option<f32> {
  find_first_baseline(fragment, 0.0).or_else(|| {
    let origin = fragment.bounds.y();
    let height = fragment.bounds.height();
    if height.is_finite() && height > 0.0 {
      Some(origin + height)
    } else {
      None
    }
  })
}

/// Types of table elements for structure analysis
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TableElementType {
  Table,
  HeaderGroup,
  RowGroup,
  FooterGroup,
  Row,
  Cell,
  Caption,
  Column,
  ColumnGroup,
  Unknown,
}

#[derive(Debug, Clone)]
struct RowMetrics {
  height: f32,
  baseline_top: f32,
  baseline_bottom: f32,
  has_baseline: bool,
  max_cell_height: f32,
}

impl RowMetrics {
  fn new(height: f32) -> Self {
    Self {
      height,
      baseline_top: 0.0,
      baseline_bottom: 0.0,
      has_baseline: false,
      max_cell_height: height,
    }
  }

  fn baseline_height(&self) -> f32 {
    if self.has_baseline {
      self.baseline_top + self.baseline_bottom
    } else {
      // CSS 2.1 §17.5.3: if no cell contributes a baseline, the row baseline
      // falls to the bottom of the row box.
      self.height
    }
  }
}

#[test]
fn row_metrics_without_baseline_fall_back_to_row_height() {
  let mut metrics = RowMetrics::new(24.0);
  assert!(!metrics.has_baseline);
  assert!((metrics.baseline_height() - 24.0).abs() < 0.01);

  metrics.has_baseline = true;
  metrics.baseline_top = 10.0;
  metrics.baseline_bottom = 5.0;
  assert!((metrics.baseline_height() - 15.0).abs() < 0.01);
}

// ============================================================================
// Column Width Algorithms
// ============================================================================

/// Calculates column widths for fixed table layout
///
/// Fixed table layout (table-layout: fixed) determines column widths
/// from the table width and column widths specified in the first row.
/// This is faster than auto layout because it doesn't require measuring
/// all cell content.
///
/// # Algorithm
///
/// 1. Use specified widths from `<col>` elements or first row cells
/// 2. Distribute remaining space equally among auto columns
/// 3. Never shrink columns below specified widths
///
/// # Arguments
///
/// * `structure` - The table structure to update
/// * `available_width` - The available width for the table
pub fn calculate_fixed_layout_widths(structure: &mut TableStructure, available_width: f32) {
  if structure.column_count == 0 {
    return;
  }

  let spacing = structure.total_horizontal_spacing();
  let content_width = (available_width - spacing).max(0.0);

  let clamp = |width: f32, min: f32, max: f32| -> f32 {
    let mut w = width.max(min);
    if max.is_finite() {
      w = w.min(max);
    }
    w
  };

  // Phase 1: Collect specified widths
  let mut specified_total = 0.0;
  let mut auto_count = 0;

  for col in &structure.columns {
    match col.specified_width {
      Some(SpecifiedWidth::Fixed(w)) => specified_total += w,
      Some(SpecifiedWidth::Percent(p)) => specified_total += content_width * p / 100.0,
      _ => auto_count += 1,
    }
  }

  // Phase 2: Calculate auto column width
  let remaining = (content_width - specified_total).max(0.0);
  let auto_width = if auto_count > 0 {
    remaining / auto_count as f32
  } else {
    0.0
  };

  // Phase 3: Assign computed widths
  for col in &mut structure.columns {
    let raw = match col.specified_width {
      Some(SpecifiedWidth::Fixed(w)) => w,
      Some(SpecifiedWidth::Percent(p)) => content_width * p / 100.0,
      _ => auto_width,
    };
    col.computed_width = clamp(raw, col.min_width, col.max_width);
  }

  // Phase 4: Normalize to available width while respecting min/max.
  let total: f32 = structure.columns.iter().map(|c| c.computed_width).sum();
  if total > content_width + 0.01 {
    // Shrink auto columns first; if only percentages/fixed remain, allow overflow (CSS fixed layout permits overrun).
    let mut shrink = total - content_width;
    let shrink_pass =
      |cols: &mut [ColumnInfo], shrink: &mut f32, predicate: &dyn Fn(&ColumnInfo) -> bool| {
        if *shrink <= 0.0 {
          return;
        }
        let mut headroom_total = 0.0;
        let mut headrooms = Vec::with_capacity(cols.len());
        for col in cols.iter() {
          if !predicate(col) {
            headrooms.push(0.0);
            continue;
          }
          let room = (col.computed_width - col.min_width).max(0.0);
          headrooms.push(room);
          headroom_total += room;
        }
        if headroom_total <= 0.0 {
          return;
        }
        for (col, room) in cols.iter_mut().zip(headrooms.into_iter()) {
          if room <= 0.0 {
            continue;
          }
          let delta = (*shrink * (room / headroom_total)).min(room);
          col.computed_width -= delta;
          *shrink -= delta;
          if *shrink <= 0.0 {
            break;
          }
        }
      };

    // Prefer shrinking only auto columns. Percent/fixed columns may legitimately overrun.
    shrink_pass(&mut structure.columns, &mut shrink, &|c: &ColumnInfo| {
      c.specified_width.is_none()
    });
  } else if total + 0.01 < content_width {
    // Grow flexible columns up to their max, leave remaining slack unused if max caps are hit.
    let mut extra = content_width - total;
    let grow_pass =
      |cols: &mut [ColumnInfo], extra: &mut f32, predicate: &dyn Fn(&ColumnInfo) -> bool| {
        if *extra <= 0.0 {
          return;
        }
        let mut headroom_total = 0.0;
        let mut has_infinite = false;
        let mut headrooms = Vec::with_capacity(cols.len());
        for col in cols.iter() {
          if !predicate(col) {
            headrooms.push(0.0);
            continue;
          }
          let cap = if col.max_width.is_finite() {
            (col.max_width - col.computed_width).max(0.0)
          } else {
            f32::INFINITY
          };
          headrooms.push(cap);
          if cap.is_finite() {
            headroom_total += cap;
          } else {
            has_infinite = true;
          }
        }
        if headroom_total > 0.0 {
          for (col, cap) in cols.iter_mut().zip(headrooms.iter()) {
            if !cap.is_finite() || *cap <= 0.0 {
              continue;
            }
            let delta = (*extra * (*cap / headroom_total)).min(*cap);
            col.computed_width += delta;
            *extra -= delta;
          }
        } else if has_infinite {
          // Only unconstrained columns can grow; spread evenly across those.
          let eligible: Vec<_> = cols
            .iter()
            .enumerate()
            .filter(|(_, c)| predicate(c) && !c.max_width.is_finite())
            .map(|(i, _)| i)
            .collect();
          if !eligible.is_empty() {
            let per = *extra / eligible.len() as f32;
            for idx in eligible {
              cols[idx].computed_width += per;
            }
            *extra = 0.0;
          }
        }
      };

    // Grow auto/percent columns first.
    grow_pass(&mut structure.columns, &mut extra, &|c: &ColumnInfo| {
      !matches!(c.specified_width, Some(SpecifiedWidth::Fixed(_)))
    });
    // Then any columns with remaining headroom.
    grow_pass(&mut structure.columns, &mut extra, &|_c| true);
  }
}

/// Calculates column widths for automatic table layout
///
/// Auto table layout (table-layout: auto, the default) determines column
/// widths based on cell content. This requires measuring all cells but
/// produces better results for content of varying widths.
///
/// # Algorithm (based on CSS 2.1 Section 17.5.2.2)
///
/// 1. Calculate minimum and maximum widths for each cell
/// 2. For each column, take the maximum of all cell minimums
/// 3. For each column, take the maximum of all cell maximums
/// 4. Distribute available space proportionally
///
/// # Arguments
///
/// * `structure` - The table structure with cell min/max widths populated
/// * `available_width` - The available width for the table
pub fn calculate_auto_layout_widths(structure: &mut TableStructure, available_width: f32) {
  if structure.column_count == 0 {
    return;
  }

  let spacing = structure.total_horizontal_spacing();
  let content_width = (available_width - spacing).max(0.0);

  // Phase 1: Calculate column min/max from non-spanning cells
  for cell in &structure.cells {
    if cell.colspan == 1 {
      let col = &mut structure.columns[cell.col];
      col.min_width = col.min_width.max(cell.min_width);
      let cell_max = cell.max_width.max(col.min_width);
      col.max_width = if col.max_width.is_finite() {
        col.max_width.max(cell_max)
      } else {
        cell_max
      };
    }
  }

  // Phase 2: Distribute spanning cell widths
  for cell in &structure.cells {
    if cell.colspan > 1 {
      // Calculate current total for spanned columns
      let span_start = cell.col;
      let span_end = (cell.col + cell.colspan).min(structure.column_count);

      distribute_span_min(&mut structure.columns, span_start, span_end, cell.min_width);
      distribute_span_max(&mut structure.columns, span_start, span_end, cell.max_width);
    }
  }

  // Phase 3: Calculate totals
  let total_min: f32 = structure.columns.iter().map(|c| c.min_width).sum();
  let total_max: f32 = structure.columns.iter().map(|c| c.max_width).sum();
  let mut content_width = content_width;
  if !content_width.is_finite() {
    if total_max.is_finite() {
      content_width = total_max;
    } else if total_min.is_finite() {
      content_width = total_min;
    } else {
      content_width = 0.0;
    }
  }

  // Phase 4: Distribute available width
  if content_width <= total_min {
    // Not enough space - use minimum widths
    for col in &mut structure.columns {
      col.computed_width = col.min_width;
    }
  } else if content_width >= total_max {
    // More than enough space - use maximum widths
    // Give extra space proportionally to columns based on their max_width
    // This keeps narrow columns (rank, arrow) small while content columns grow
    let extra = content_width - total_max;
    if total_max > 0.0 {
      for col in &mut structure.columns {
        let proportion = col.max_width / total_max;
        col.computed_width = col.max_width + extra * proportion;
      }
    } else {
      // Fallback to equal distribution if no max widths
      let per_col = extra / structure.column_count as f32;
      for col in &mut structure.columns {
        col.computed_width = col.max_width + per_col;
      }
    }
  } else {
    // Between min and max - distribute proportionally
    let range = content_width - total_min;
    let flex_ranges: Vec<f32> = structure
      .columns
      .iter()
      .map(|c| (c.max_width - c.min_width).max(0.0))
      .collect();
    let total_flex: f32 = flex_ranges.iter().copied().filter(|f| f.is_finite()).sum();
    let infinite_indices: Vec<usize> = flex_ranges
      .iter()
      .enumerate()
      .filter_map(|(i, f)| if f.is_finite() { None } else { Some(i) })
      .collect();

    if !infinite_indices.is_empty() {
      // Allocate finite headroom first, then spread any remaining space evenly across
      // unbounded columns to avoid NaNs from inf/inf ratios.
      let mut remaining = range;
      let mut widths: Vec<f32> = structure.columns.iter().map(|c| c.min_width).collect();
      if total_flex > 0.0 {
        for (idx, flex) in flex_ranges.iter().enumerate() {
          if !flex.is_finite() || *flex <= 0.0 {
            continue;
          }
          let share = range * (*flex / total_flex);
          let delta = share.min(*flex);
          widths[idx] += delta;
          remaining -= delta;
        }
        remaining = remaining.max(0.0);
      }

      if remaining > 0.0 && !infinite_indices.is_empty() {
        let per = remaining / infinite_indices.len() as f32;
        for idx in infinite_indices {
          widths[idx] += per;
        }
      }

      for (col, width) in structure.columns.iter_mut().zip(widths.into_iter()) {
        col.computed_width = width;
      }
    } else if total_flex > 0.0 {
      for (col, flex) in structure.columns.iter_mut().zip(flex_ranges.iter()) {
        let extra = range * (*flex / total_flex);
        col.computed_width = col.min_width + extra;
      }
    } else {
      // All columns are fixed-width
      let per_col = range / structure.column_count as f32;
      for col in &mut structure.columns {
        col.computed_width = col.min_width + per_col;
      }
    }
  }
}

// ============================================================================
// Row Height Algorithm
// ============================================================================

/// Calculates row heights based on cell content and optional available height.
///
/// This is a simplified stand-alone distributor used by unit tests and legacy
/// paths. It accounts for:
/// - min-height contributions from non-spanning cells
/// - rowspan contributions spread evenly across rows
/// - specified row heights (length or percentage when a definite available height is provided)
/// - distributing remaining space to auto rows when an available height is known
#[allow(clippy::cognitive_complexity)]
pub fn calculate_row_heights(structure: &mut TableStructure, available_height: Option<f32>) {
  if structure.row_count == 0 {
    return;
  }

  let spacing = if structure.border_collapse == BorderCollapse::Collapse {
    0.0
  } else {
    structure.border_spacing.1
  };
  let content_available =
    available_height.map(|h| (h - spacing * structure.row_count as f32).max(0.0));

  let row_floor = |row: &RowInfo| -> f32 {
    let mut floor = row.min_height;
    if let Some(spec) = row.specified_height {
      match spec {
        SpecifiedHeight::Fixed(h) => floor = floor.max(h),
        SpecifiedHeight::Percent(pct) => {
          if let Some(base) = content_available {
            floor = floor.max((pct / 100.0) * base);
          }
        }
        SpecifiedHeight::Auto => {}
      }
    }
    floor
  };

  // Phase 1: Calculate row heights from non-spanning cells
  for cell in &structure.cells {
    if cell.rowspan == 1 {
      let row = &mut structure.rows[cell.row];
      row.min_height = row.min_height.max(cell.min_height);
    }
  }

  // Phase 2: Distribute spanning cell heights
  for cell in &structure.cells {
    if cell.rowspan > 1 {
      let span_start = cell.row;
      let span_end = (cell.row + cell.rowspan).min(structure.row_count);

      let spacing = structure.border_spacing.1 * (cell.rowspan - 1) as f32;
      let span_height = (cell.min_height - spacing).max(0.0);

      let targets: Vec<usize> = (span_start..span_end).collect();
      if targets.is_empty() {
        continue;
      }

      // Establish floors (min/spec heights) and caps for each target row.
      let percent_base = content_available;
      let mut floors: Vec<f32> = Vec::with_capacity(targets.len());
      let mut caps: Vec<Option<f32>> = Vec::with_capacity(targets.len());
      let mut current_sum = 0.0;
      for &idx in &targets {
        let row = &structure.rows[idx];
        let (min_cap, max_cap) = resolve_row_min_max(row, percent_base);
        let floor = row_floor(row)
          .max(row.min_height)
          .max(min_cap.unwrap_or(0.0));
        floors.push(floor);
        caps.push(max_cap);
        current_sum += floor;
      }

      let mut current = floors.clone();
      let mut remaining = (span_height - current_sum).max(0.0);
      let specified_indices: Vec<usize> = targets
        .iter()
        .enumerate()
        .filter(|(_, &idx)| {
          matches!(
            structure.rows[idx].specified_height,
            Some(SpecifiedHeight::Fixed(_)) | Some(SpecifiedHeight::Percent(_))
          )
        })
        .map(|(local_idx, _)| local_idx)
        .collect();
      let auto_indices: Vec<usize> = targets
        .iter()
        .enumerate()
        .filter(|(_, &idx)| {
          !matches!(
            structure.rows[idx].specified_height,
            Some(SpecifiedHeight::Fixed(_)) | Some(SpecifiedHeight::Percent(_))
          )
        })
        .map(|(local_idx, _)| local_idx)
        .collect();

      if remaining > 0.0 {
        if specified_indices.is_empty() {
          // No specified rows: distribute proportional to existing floors, but
          // fall back to equal shares when any row has no floor contribution.
          let weights: Vec<f32> = current.to_vec();
          if weights.iter().any(|w| *w <= 0.0) {
            let per = remaining / current.len() as f32;
            for c in &mut current {
              *c += per;
            }
            let zero_indices: Vec<usize> = weights
              .iter()
              .enumerate()
              .filter(|(_, w)| **w <= 0.0)
              .map(|(i, _)| i)
              .collect();
            if !zero_indices.is_empty() && zero_indices.len() < current.len() {
              let donors: Vec<usize> = weights
                .iter()
                .enumerate()
                .filter(|(_, w)| **w > 0.0)
                .map(|(i, _)| i)
                .collect();
              let bonus = per * 0.01;
              let total_bonus = bonus * zero_indices.len() as f32;
              if !donors.is_empty() && bonus.is_finite() && bonus > 0.0 {
                let per_donor = total_bonus / donors.len() as f32;
                for idx in zero_indices {
                  current[idx] += bonus;
                }
                for idx in donors {
                  current[idx] = (current[idx] - per_donor).max(0.0);
                }
              }
            }
          } else {
            let total_weight: f32 = weights.iter().sum();
            if total_weight > 0.0 {
              for (idx, weight) in weights.iter().enumerate() {
                let share = remaining * (*weight / total_weight);
                current[idx] += share;
              }
            }
          }
        } else {
          let equal_share = span_height / targets.len() as f32;

          // First, raise auto rows toward the equal share baseline.
          for &idx in &auto_indices {
            if remaining <= 0.0 {
              break;
            }
            let need = (equal_share - current[idx]).max(0.0);
            let take = need.min(remaining);
            current[idx] += take;
            remaining -= take;
          }

          if remaining > 0.0 {
            if !auto_indices.is_empty() {
              let per_auto = remaining / auto_indices.len() as f32;
              let overshoot_ratio = if equal_share > 0.0 {
                per_auto / equal_share
              } else {
                0.0
              };
              if overshoot_ratio > 0.25 && !specified_indices.is_empty() {
                let per_spec = remaining / specified_indices.len() as f32;
                for &idx in &specified_indices {
                  current[idx] += per_spec;
                }
              } else {
                for &idx in &auto_indices {
                  current[idx] += per_auto;
                }
              }
            } else {
              // All rows are specified; spread the remainder evenly.
              let per_spec = remaining / specified_indices.len() as f32;
              for &idx in &specified_indices {
                current[idx] += per_spec;
              }
            }
          }
        }
      }

      // Apply max caps and redirect any surplus to uncapped rows.
      let mut surplus = 0.0;
      for (idx, cap) in caps.iter().enumerate() {
        if let Some(max_h) = cap {
          if current[idx] > *max_h {
            surplus += current[idx] - *max_h;
            current[idx] = *max_h;
          }
        }
      }
      if surplus > 0.0 {
        let flex: Vec<usize> = caps
          .iter()
          .enumerate()
          .filter(|(i, cap)| {
            cap.is_none() || current[*i] + f32::EPSILON < (*cap).unwrap_or(f32::INFINITY)
          })
          .map(|(i, _)| i)
          .collect();
        if !flex.is_empty() {
          let per = surplus / flex.len() as f32;
          for idx in flex {
            current[idx] += per;
          }
        }
      }

      for (local_idx, &row_idx) in targets.iter().enumerate() {
        if let Some(row) = structure.rows.get_mut(row_idx) {
          row.min_height = row.min_height.max(current[local_idx]);
        }
      }
    }
  }

  // Phase 3: Apply specified/percentage heights when an available height is known.
  let mut fixed_sum = 0.0;
  let mut percent_rows = Vec::new();
  let mut auto_rows = Vec::new();

  for (idx, row) in structure.rows.iter().enumerate() {
    match row.specified_height {
      Some(SpecifiedHeight::Fixed(h)) => fixed_sum += h.max(row.min_height),
      Some(SpecifiedHeight::Percent(p)) => percent_rows.push((idx, p)),
      Some(SpecifiedHeight::Auto) | None => auto_rows.push(idx),
    }
  }

  let mut computed: Vec<f32> = structure.rows.iter().map(|r| r.min_height).collect();

  if let Some(base) = content_available {
    // Apply percentage rows against the definite base.
    for &(idx, pct) in &percent_rows {
      let target = (pct / 100.0) * base;
      computed[idx] = computed[idx].max(target);
    }

    // Recompute the fixed budget (fixed + percentage rows) using specified heights where applicable.
    fixed_sum = 0.0;
    for (idx, row) in structure.rows.iter().enumerate() {
      match row.specified_height {
        Some(SpecifiedHeight::Fixed(h)) => {
          computed[idx] = computed[idx].max(h);
          fixed_sum += computed[idx];
        }
        Some(SpecifiedHeight::Percent(_)) => fixed_sum += computed[idx],
        _ => {}
      }
    }

    // Fixed rows were already counted in fixed_sum via target or explicit fixed.
    // Compute remaining space for auto rows.
    let remaining = base - fixed_sum;
    if remaining > 0.0 && !auto_rows.is_empty() {
      let auto_sum: f32 = auto_rows.iter().map(|i| computed[*i]).sum();
      if auto_sum > 0.0 {
        for idx in auto_rows {
          let weight = computed[idx] / auto_sum;
          computed[idx] = computed[idx].max(remaining * weight);
        }
      } else {
        let per = remaining / auto_rows.len() as f32;
        for idx in auto_rows {
          computed[idx] = computed[idx].max(per);
        }
      }
    } else if remaining < 0.0 && !percent_rows.is_empty() {
      // Percent rows over-commit the available space; reduce them using headroom-aware weights.
      let percent_indices: Vec<usize> = percent_rows.iter().map(|(i, _)| *i).collect();
      reduce_rows_with_headroom(
        &mut computed,
        &structure.rows,
        base,
        &percent_indices,
        content_available,
      );
    }
  }

  // Fallback: no available height, just use min heights and explicit lengths.
  for (idx, row) in structure.rows.iter().enumerate() {
    if let Some(SpecifiedHeight::Fixed(h)) = row.specified_height {
      computed[idx] = computed[idx].max(h);
    }
    let (min_cap, max_cap) = resolve_row_min_max(row, content_available);
    if let Some(min_h) = min_cap {
      computed[idx] = computed[idx].max(min_h);
    }
    if let Some(max_h) = max_cap {
      computed[idx] = computed[idx].min(max_h);
    }
  }

  if let Some(base) = content_available {
    let total: f32 = computed.iter().sum();
    if total < base {
      distribute_remaining_height_with_caps(
        &mut computed,
        &structure.rows,
        base - total,
        content_available,
      );
    }
  }

  // Phase 4: calculate positions using the computed heights.
  let mut y = spacing;
  for (idx, row) in structure.rows.iter_mut().enumerate() {
    let h = computed.get(idx).copied().unwrap_or(0.0);
    row.computed_height = if h.is_finite() { h.max(0.0) } else { 0.0 };
    row.y_position = y;
    y += row.computed_height + spacing;
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct TableStructureCacheKey {
  box_id: usize,
  style_ptr: usize,
}

#[derive(Debug, Clone)]
struct TableStructureCacheEntry {
  epoch: usize,
  structure: Arc<TableStructure>,
}

thread_local! {
  static TABLE_STRUCTURE_CACHE: RefCell<HashMap<TableStructureCacheKey, TableStructureCacheEntry>> =
    RefCell::new(HashMap::new());
}

fn table_structure_cache_key(table_box: &BoxNode) -> Option<TableStructureCacheKey> {
  let box_id = table_box.id();
  if box_id == 0 {
    return None;
  }
  Some(TableStructureCacheKey {
    box_id,
    style_ptr: Arc::as_ptr(&table_box.style) as usize,
  })
}

fn table_structure_cached(table_box: &BoxNode) -> Arc<TableStructure> {
  let epoch = intrinsic_cache_epoch();
  let key = table_structure_cache_key(table_box);
  let build_structure = || Arc::new(TableStructure::from_box_tree(table_box));
  if let Some(key) = key {
    TABLE_STRUCTURE_CACHE.with(|cache| {
      if let Some(entry) = cache.borrow().get(&key) {
        if entry.epoch == epoch {
          return entry.structure.clone();
        }
      }
      let structure = build_structure();
      cache.borrow_mut().insert(
        key,
        TableStructureCacheEntry {
          epoch,
          structure: structure.clone(),
        },
      );
      structure
    })
  } else {
    build_structure()
  }
}

// ============================================================================
// Table Formatting Context
// ============================================================================

/// Table Formatting Context
///
/// Implements the FormattingContext trait for table layout. This handles
/// the complete table layout algorithm including structure analysis,
/// column width calculation, row height calculation, and cell positioning.
///
/// # Usage
///
/// ```rust,ignore
/// use fastrender::layout::table::TableFormattingContext;
/// use fastrender::FormattingContext;
///
/// let tfc = TableFormattingContext::new();
/// let fragment = tfc.layout(table_box, constraints)?;
/// ```
#[derive(Debug, Clone)]
pub struct TableFormattingContext {
  /// Table structure (lazily built during layout)
  structure: Option<TableStructure>,
  /// Formatting context factory carrying shared font resources for cell layout.
  factory: FormattingContextFactory,
  viewport_size: crate::geometry::Size,
  nearest_positioned_cb: ContainingBlock,
  parallelism: LayoutParallelism,
}

impl TableFormattingContext {
  /// Creates a new table formatting context
  pub fn new() -> Self {
    Self::with_factory(FormattingContextFactory::new())
  }

  /// Creates a table formatting context that reuses the provided factory.
  pub fn with_factory(factory: FormattingContextFactory) -> Self {
    let parallelism = factory.parallelism();
    let viewport_size = factory.viewport_size();
    let nearest_positioned_cb = factory.nearest_positioned_cb();
    Self {
      structure: None,
      factory,
      viewport_size,
      nearest_positioned_cb,
      parallelism,
    }
  }

  /// Percentages on columns rely on a definite table width. If the table width
  /// is not specified, treat percentage constraints as auto so intrinsic sizing
  /// isn't forced by an unrelated containing block.
  fn normalize_percentage_constraints(
    &self,
    constraints: &mut [ColumnConstraints],
    percent_base: Option<f32>,
  ) {
    if percent_base.is_some() {
      return;
    }

    for col in constraints.iter_mut() {
      if col.percentage.is_some() {
        col.percentage = None;
        col.fixed_width = None;
        col.is_flexible = true;
      }
    }
  }

  /// Gets the table structure, building it if necessary
  pub fn structure(&self) -> Option<&TableStructure> {
    self.structure.as_ref()
  }

  /// Returns the table box to use for layout, avoiding redundant fixup work for
  /// already-normalized trees produced by box generation.
  fn normalize_table_root<'a>(&self, box_node: &'a BoxNode) -> Cow<'a, BoxNode> {
    if TableStructureFixer::validate_table_structure(box_node) {
      Cow::Borrowed(box_node)
    } else {
      Cow::Owned(
        TableStructureFixer::fixup_table_internals(box_node.clone())
          .unwrap_or_else(|err| panic!("table structure fixup failed: {err}")),
      )
    }
  }

  /// Measures cell intrinsic widths using inline min/max content rules
  fn measure_cell_intrinsic_widths(
    &self,
    cell_box: &BoxNode,
    border_collapse: BorderCollapse,
    percent_base: Option<f32>,
    cell_bfc: &BlockFormattingContext,
    style_overrides: &StyleOverrideCache,
  ) -> (f32, f32) {
    let fc_type = cell_box
      .formatting_context()
      .unwrap_or(FormattingContextType::Block);

    // Measure intrinsic content widths without the cell's own padding/borders; we'll add them once below.
    let mut stripped_cell = cell_box.clone();
    stripped_cell.style =
      style_overrides.derive(&cell_box.style, StyleOverrideFlags::STRIP_PADDING_BORDERS);

    let measure_with_fc = |fc: &dyn FormattingContext| -> (f32, f32) {
      let min = fc
        .compute_intrinsic_inline_size(&stripped_cell, IntrinsicSizingMode::MinContent)
        .unwrap_or(0.0);
      let max = fc
        .compute_intrinsic_inline_size(&stripped_cell, IntrinsicSizingMode::MaxContent)
        .unwrap_or(min);
      (min, max)
    };
    let (mut min, mut max) = if fc_type == FormattingContextType::Block {
      measure_with_fc(cell_bfc)
    } else {
      let fc = self.factory.create(fc_type);
      measure_with_fc(fc.as_ref())
    };

    // Add horizontal padding (and borders in separate model) to intrinsic widths
    let style = &cell_box.style;
    let padding_and_borders = match border_collapse {
      BorderCollapse::Separate => horizontal_padding_and_borders(style, percent_base),
      BorderCollapse::Collapse => {
        // Collapsed borders don't add to box width; include padding only.
        horizontal_padding(style, percent_base)
      }
    };
    min += padding_and_borders;
    max += padding_and_borders;

    (min.max(0.0), max.max(min))
  }

  /// Populates column constraints from cell intrinsic widths and explicit widths
  fn populate_column_constraints(
    &self,
    table_box: &BoxNode,
    structure: &TableStructure,
    constraints: &mut [ColumnConstraints],
    mode: DistributionMode,
    percent_base: Option<f32>,
    style_overrides: &StyleOverrideCache,
  ) {
    let source_rows = collect_source_rows(table_box);
    let cell_bfc = BlockFormattingContext::with_font_context_and_viewport(
      self.factory.font_context().clone(),
      self.factory.viewport_size(),
    )
    .with_parallelism(self.parallelism);
    for cell in &structure.cells {
      if matches!(mode, DistributionMode::Fixed) && cell.row > 0 {
        // Fixed layout only inspects the first row.
        continue;
      }
      let Some(row) = source_rows.get(cell.source_row) else {
        continue;
      };
      let Some(cell_box) = row.children.get(cell.box_index) else {
        continue;
      };
      let width_decl = cell_box.style.width.as_ref();
      let width_is_percent = matches!(width_decl.map(|w| w.unit), Some(LengthUnit::Percent));
      let width_padding = if matches!(mode, DistributionMode::Fixed) {
        match structure.border_collapse {
          BorderCollapse::Separate => horizontal_padding_and_borders(&cell_box.style, percent_base),
          BorderCollapse::Collapse => horizontal_padding(&cell_box.style, percent_base),
        }
      } else {
        0.0
      };
      let (mut min_w, mut max_w) = match mode {
        DistributionMode::Fixed => (0.0, f32::INFINITY), // content is ignored in fixed layout
        DistributionMode::Auto => self.measure_cell_intrinsic_widths(
          cell_box,
          structure.border_collapse,
          percent_base,
          &cell_bfc,
          style_overrides,
        ),
      };
      let mut has_max_cap = false;
      if mode == DistributionMode::Auto {
        // Apply min-width/max-width from the cell itself. Percentages only participate when the
        // table width is definite per CSS 2.1 §10.4 (percentage on width/min/max). Clamp max to
        // remain ≥ min so constraints stay sane.
        if let Some(min_len) = cell_box
          .style
          .min_width
          .as_ref()
          .and_then(|len| resolve_length_against(len, cell_box.style.font_size, percent_base))
        {
          min_w = min_w.max(min_len);
          max_w = max_w.max(min_w);
        }
        if let Some(max_len) = cell_box
          .style
          .max_width
          .as_ref()
          .and_then(|len| resolve_length_against(len, cell_box.style.font_size, percent_base))
        {
          let cap = max_len.max(min_w);
          max_w = if max_w.is_finite() {
            max_w.min(cap)
          } else {
            cap
          };
          max_w = max_w.max(min_w);
          has_max_cap = true;
        }
      }
      let effective_max = if has_max_cap { max_w } else { f32::INFINITY };
      let specified_width = cell_box
        .style
        .width
        .as_ref()
        .and_then(|width| match width.unit {
          LengthUnit::Percent => percent_base.map(|base| {
            crate::layout::utils::clamp_with_order(
              (width.value / 100.0) * base + width_padding,
              min_w,
              effective_max,
            )
          }),
          _ => Some(crate::layout::utils::clamp_with_order(
            width.to_px() + width_padding,
            min_w,
            effective_max,
          )),
        });
      let span_specified_width = if width_is_percent && cell.colspan > 1 {
        None
      } else {
        specified_width
      };

      if cell.colspan == 1 {
        if let Some(width) = width_decl {
          match width.unit {
            LengthUnit::Percent if percent_base.is_some() => {
              let col = &mut constraints[cell.col];
              col.set_percentage(width.value);
              if let Some(px) = specified_width {
                col.min_width = col.min_width.max(px);
                col.max_width = col.max_width.max(px);
              } else {
                col.min_width = col.min_width.max(min_w);
                col.max_width = col.max_width.max(max_w);
              }
              if has_max_cap {
                col.has_max_cap = true;
              }
              continue;
            }
            LengthUnit::Percent => {
              let col = &mut constraints[cell.col];
              col.min_width = col.min_width.max(min_w);
              col.max_width = col.max_width.max(max_w);
            }
            _ => {
              let px = specified_width.unwrap_or_else(|| {
                crate::layout::utils::clamp_with_order(
                  width.to_px() + width_padding,
                  min_w,
                  effective_max,
                )
              });
              let col = &mut constraints[cell.col];
              col.fixed_width = Some(px);
              col.min_width = col.min_width.max(min_w);
              let target_max = if max_w.is_finite() { px.max(max_w) } else { px };
              col.max_width = col.max_width.max(target_max);
              if has_max_cap {
                col.has_max_cap = true;
              }
              continue;
            }
          }
        }

        let col = &mut constraints[cell.col];
        col.min_width = col.min_width.max(min_w);
        col.max_width = col.max_width.max(max_w);
        if has_max_cap {
          col.has_max_cap = true;
        }
      } else {
        let start = cell.col;
        let end = (cell.col + cell.colspan).min(constraints.len());
        if width_is_percent && percent_base.is_some() {
          distribute_spanning_percentage(constraints, start, end, width_decl.unwrap().value);
        }
        let target_min = span_specified_width.unwrap_or(min_w);
        let target_max = span_specified_width.unwrap_or(max_w);
        distribute_spanning_cell_width(constraints, start, end, target_min, target_max);
        if has_max_cap {
          for col in constraints.iter_mut().take(end).skip(start) {
            col.has_max_cap = true;
          }
        }
      }
    }

    // Apply specified widths from column info (<col>/<colgroup>)
    for (i, col_info) in structure.columns.iter().enumerate() {
      let Some(constraint) = constraints.get_mut(i) else {
        continue;
      };
      let font_size = if col_info.font_size > 0.0 {
        col_info.font_size
      } else {
        table_box.style.font_size
      };
      if let Some(specified) = col_info.specified_width {
        match specified {
          SpecifiedWidth::Fixed(px) => {
            constraint.fixed_width = Some(px.max(constraint.min_width));
            constraint.is_flexible = false;
            constraint.min_width = constraint.min_width.max(px);
            constraint.max_width = constraint.max_width.max(px);
          }
          SpecifiedWidth::Percent(pct) if percent_base.is_some() => {
            let base = percent_base.unwrap();
            let px = (pct / 100.0) * base;
            constraint.set_percentage(pct);
            constraint.min_width = constraint.min_width.max(px);
            constraint.max_width = constraint.max_width.max(px);
          }
          SpecifiedWidth::Percent(_) => {}
          SpecifiedWidth::Auto => {}
        }
      }
      if let Some(min_len) = col_info
        .author_min_width
        .as_ref()
        .and_then(|len| resolve_length_against(len, font_size, percent_base))
      {
        constraint.min_width = constraint.min_width.max(min_len);
        constraint.max_width = constraint.max_width.max(constraint.min_width);
      }
      if let Some(max_len) = col_info
        .author_max_width
        .as_ref()
        .and_then(|len| resolve_length_against(len, font_size, percent_base))
      {
        let cap = max_len.max(constraint.min_width);
        constraint.max_width = if constraint.max_width.is_finite() {
          constraint.max_width.min(cap)
        } else {
          cap
        };
        constraint.max_width = constraint.max_width.max(constraint.min_width);
        constraint.has_max_cap = true;
      }
    }
  }

  /// Layout a single cell with the given width
  fn layout_cell(
    &self,
    cell_box: &BoxNode,
    cell_width: f32,
    border_collapse: BorderCollapse,
    cell_bfc: &BlockFormattingContext,
    style_overrides: &StyleOverrideCache,
  ) -> Result<FragmentNode, LayoutError> {
    let hide_empty = border_collapse == BorderCollapse::Separate
      && cell_box.style.empty_cells == EmptyCells::Hide
      && cell_is_visually_empty(cell_box);
    let mut cloned = cell_box.clone();
    let mut flags = StyleOverrideFlags::LAYOUT_CLEAR_WIDTHS_AND_MARGINS;
    if hide_empty {
      flags |= StyleOverrideFlags::HIDE_EMPTY_RESET_BG_AND_TRANSPARENT_BORDERS;
    }
    if matches!(border_collapse, BorderCollapse::Collapse) {
      flags |= StyleOverrideFlags::COLLAPSE_ZERO_BORDERS;
    }
    cloned.style = style_overrides.derive(&cell_box.style, flags);

    let constraints = LayoutConstraints::definite_width(cell_width.max(0.0));
    cell_bfc.layout(&cloned, &constraints)
  }

  fn collect_row_group_constraints(
    &self,
    table_box: &BoxNode,
    source_row_to_visible: &[Option<usize>],
  ) -> Vec<RowGroupConstraints> {
    let mut constraints = Vec::new();
    let mut source_row_idx = 0usize;
    for child in table_box
      .children
      .iter()
      .filter(|child| child.style.running_position.is_none())
    {
      match TableStructure::get_table_element_type(child) {
        TableElementType::RowGroup
        | TableElementType::HeaderGroup
        | TableElementType::FooterGroup => {
          let mut first_visible = None;
          let mut last_visible = None;
          for row_child in &child.children {
            if TableStructure::get_table_element_type(row_child) == TableElementType::Row {
              if let Some(visible) = source_row_to_visible.get(source_row_idx).and_then(|m| *m) {
                first_visible.get_or_insert(visible);
                last_visible = Some(visible);
              }
              source_row_idx += 1;
            }
          }
          if let (Some(start), Some(end)) = (first_visible, last_visible) {
            constraints.push(RowGroupConstraints {
              start,
              end: end + 1,
              height: TableStructure::length_to_specified_height_opt(
                child.style.height.as_ref(),
                child.style.font_size,
              ),
              min_height: TableStructure::length_to_specified_height_opt(
                child.style.min_height.as_ref(),
                child.style.font_size,
              ),
              max_height: TableStructure::length_to_specified_height_opt(
                child.style.max_height.as_ref(),
                child.style.font_size,
              ),
            });
          }
        }
        TableElementType::Row => {
          source_row_idx += 1;
        }
        _ => {}
      }
    }
    constraints
  }
}

impl Default for TableFormattingContext {
  fn default() -> Self {
    Self::new()
  }
}

impl FormattingContext for TableFormattingContext {
  /// Performs full table layout following CSS table algorithms (auto layout)
  #[allow(clippy::cognitive_complexity)]
  fn layout(
    &self,
    box_node: &BoxNode,
    constraints: &LayoutConstraints,
  ) -> Result<FragmentNode, LayoutError> {
    let _profile = layout_timer(LayoutKind::Table);
    let mut deadline_counter = 0usize;
    let has_running_children_for_cache = box_node
      .children
      .iter()
      .any(|child| child.style.running_position.is_some());
    if !has_running_children_for_cache {
      if let Some(cached) = layout_cache_lookup(
        box_node,
        FormattingContextType::Table,
        constraints,
        self.viewport_size,
      ) {
        return Ok(cached);
      }
    }
    let normalized_table = self.normalize_table_root(box_node);
    let table_box = normalized_table.as_ref();
    let dump = runtime::runtime_toggles().truthy("FASTR_DUMP_TABLE");
    let mut positioned_children: Vec<&BoxNode> = Vec::new();
    let mut running_children: Vec<(usize, &BoxNode)> = Vec::new();
    for (child_idx, child) in table_box.children.iter().enumerate() {
      check_layout_deadline(&mut deadline_counter)?;
      if child.style.running_position.is_some() {
        running_children.push((child_idx, child));
        continue;
      }
      if matches!(
        child.style.position,
        crate::style::position::Position::Absolute | crate::style::position::Position::Fixed
      ) {
        positioned_children.push(child);
      }
    }
    let has_running_children = !running_children.is_empty();

    let structure = table_structure_cached(table_box);
    let style_override_cache = StyleOverrideCache::default();
    if dump {
      let cw = match constraints.available_width {
        AvailableSpace::Definite(w) => format!("{:.2}", w),
        AvailableSpace::MaxContent => "max-content".to_string(),
        AvailableSpace::MinContent => "min-content".to_string(),
        AvailableSpace::Indefinite => "indefinite".to_string(),
      };
      let ch = match constraints.available_height {
        AvailableSpace::Definite(h) => format!("{:.2}", h),
        AvailableSpace::MaxContent => "max-content".to_string(),
        AvailableSpace::MinContent => "min-content".to_string(),
        AvailableSpace::Indefinite => "indefinite".to_string(),
      };
      eprintln!(
                "table constraints: width={} height={} display={:?} id={} border_spacing=({:.2},{:.2}) collapse={:?}",
                cw,
                ch,
                table_box.style.display,
                table_box.id,
                structure.border_spacing.0,
                structure.border_spacing.1,
                structure.border_collapse
            );
    }
    let captions: Vec<&BoxNode> = table_box
      .children
      .iter()
      .filter(|child| {
        child.style.running_position.is_none()
          && !matches!(
            child.style.position,
            crate::style::position::Position::Absolute | crate::style::position::Position::Fixed
          )
          && matches!(child.style.display, Display::TableCaption)
      })
      .collect();

    let max_source_row = structure
      .rows
      .iter()
      .map(|r| r.source_index)
      .max()
      .unwrap_or(0);
    let mut source_row_to_visible = if structure.rows.is_empty() {
      Vec::new()
    } else {
      vec![None; max_source_row + 1]
    };
    for row in &structure.rows {
      check_layout_deadline(&mut deadline_counter)?;
      if row.source_index >= source_row_to_visible.len() {
        source_row_to_visible.resize(row.source_index + 1, None);
      }
      source_row_to_visible[row.source_index] = Some(row.index);
    }

    let max_source_col = structure
      .columns
      .iter()
      .map(|c| c.source_index)
      .max()
      .unwrap_or(0);
    let mut source_col_to_visible = if structure.columns.is_empty() {
      Vec::new()
    } else {
      vec![None; max_source_col + 1]
    };
    for col in &structure.columns {
      check_layout_deadline(&mut deadline_counter)?;
      if col.source_index >= source_col_to_visible.len() {
        source_col_to_visible.resize(col.source_index + 1, None);
      }
      source_col_to_visible[col.source_index] = Some(col.index);
    }

    let row_group_constraints =
      self.collect_row_group_constraints(table_box, &source_row_to_visible);
    let mut row_to_group: Vec<Option<usize>> = vec![None; structure.row_count];
    for (idx, group) in row_group_constraints.iter().enumerate() {
      check_layout_deadline(&mut deadline_counter)?;
      for row in group.start..group.end {
        if let Some(slot) = row_to_group.get_mut(row) {
          *slot = Some(idx);
        }
      }
    }

    let source_rows = collect_source_rows(table_box);

    let containing_width = match constraints.available_width {
      AvailableSpace::Definite(w) => Some(w),
      _ => None,
    };
    let containing_height = match constraints.available_height {
      AvailableSpace::Definite(h) => Some(h),
      _ => None,
    };

    // Captions impose a minimum width on the table: CSS 2.1 §17.4 requires the table box to be
    // at least as wide as its caption. Resolve authored widths first; otherwise fall back to the
    // caption's intrinsic max-content width.
    let caption_pref_width = captions
      .iter()
      .copied()
      .filter(|c| {
        !matches!(
          c.style.position,
          crate::style::position::Position::Absolute | crate::style::position::Position::Fixed
        )
      })
      .map(|caption| {
        if let Some(width) = caption.style.width.as_ref() {
          resolve_length_against(width, caption.style.font_size, containing_width).unwrap_or(0.0)
        } else {
          let fc_type = caption
            .formatting_context()
            .unwrap_or(crate::style::display::FormattingContextType::Block);
          let fc = self.factory.create(fc_type);
          fc.compute_intrinsic_inline_size(caption, IntrinsicSizingMode::MaxContent)
            .unwrap_or(0.0)
        }
      })
      .fold(0.0, f32::max);

    // Honor explicit table width if present.
    let font_size = table_box.style.font_size;
    let specified_width = table_box
      .style
      .width
      .as_ref()
      .and_then(|len| resolve_length_against(len, font_size, containing_width));
    let resolved_min_width = resolve_opt_length_against(
      table_box.style.min_width.as_ref(),
      font_size,
      containing_width,
    );
    let resolved_max_width = resolve_opt_length_against(
      table_box.style.max_width.as_ref(),
      font_size,
      containing_width,
    );
    let mut min_width = resolve_opt_length_against(
      table_box.style.min_width.as_ref(),
      font_size,
      containing_width,
    );
    let max_width = resolve_opt_length_against(
      table_box.style.max_width.as_ref(),
      font_size,
      containing_width,
    );
    if caption_pref_width > 0.0 && specified_width.is_none() {
      min_width = Some(min_width.unwrap_or(0.0).max(caption_pref_width));
    }

    // Table width for sizing; percentages on columns use a definite table width when available (specified or containing).
    let table_width = specified_width
      .or(containing_width)
      .map(|w| clamp_to_min_max(w, min_width, max_width));
    let percent_base_width = table_width;
    // Table padding and borders (ignored for box sizing under collapsed model per CSS 2.1),
    // but we still track outer borders for percentage-height resolution.
    let resolve_abs = |l: &crate::style::values::Length| match l.unit {
      LengthUnit::Percent => 0.0,
      _ if l.unit.is_absolute() => l.to_px(),
      _ => l.value,
    };
    let _outer_border_left = resolve_abs(&table_box.style.border_left_width);
    let _outer_border_right = resolve_abs(&table_box.style.border_right_width);
    let outer_border_top = resolve_abs(&table_box.style.border_top_width);
    let outer_border_bottom = resolve_abs(&table_box.style.border_bottom_width);
    let outer_border_v = outer_border_top + outer_border_bottom;
    let mut pad_left = resolve_abs(&table_box.style.padding_left);
    let mut pad_right = resolve_abs(&table_box.style.padding_right);
    let mut pad_top = resolve_abs(&table_box.style.padding_top);
    // In the collapsing border model, the table box has no padding (CSS 2.2 §17.6.2).
    let pad_bottom = if structure.border_collapse == BorderCollapse::Collapse {
      pad_left = 0.0;
      pad_right = 0.0;
      pad_top = 0.0;
      0.0
    } else {
      resolve_abs(&table_box.style.padding_bottom)
    };
    let (border_left, border_right, border_top, border_bottom) =
      if structure.border_collapse == BorderCollapse::Collapse {
        (0.0, 0.0, 0.0, 0.0)
      } else {
        (
          resolve_abs(&table_box.style.border_left_width),
          resolve_abs(&table_box.style.border_right_width),
          resolve_abs(&table_box.style.border_top_width),
          resolve_abs(&table_box.style.border_bottom_width),
        )
      };
    let padding_h = pad_left + pad_right;
    let padding_v = pad_top + pad_bottom;
    let border_h = border_left + border_right;
    let border_v = border_top + border_bottom;

    let specified_height = table_box
      .style
      .height
      .as_ref()
      .and_then(|len| resolve_length_against(len, font_size, containing_height));
    let min_height = resolve_opt_length_against(
      table_box.style.min_height.as_ref(),
      font_size,
      containing_height,
    );
    let max_height = resolve_opt_length_against(
      table_box.style.max_height.as_ref(),
      font_size,
      containing_height,
    );
    let table_height = specified_height.map(|h| clamp_to_min_max(h, min_height, max_height));

    // Helper to position out-of-flow children against a containing block.
    let place_out_of_flow =
      |fragment: &mut FragmentNode, cb: ContainingBlock| -> Result<(), LayoutError> {
        if positioned_children.is_empty() {
          return Ok(());
        }
        let abs = AbsoluteLayout::with_font_context(self.factory.font_context().clone());
        for child in positioned_children.iter().copied() {
          let original_style = child.style.clone();
          let mut layout_child = child.clone();
          let mut style = (*layout_child.style).clone();
          style.position = crate::style::position::Position::Relative;
          style.top = None;
          style.right = None;
          style.bottom = None;
          style.left = None;
          layout_child.style = Arc::new(style);

          let factory = self.factory.with_positioned_cb(cb);
          let fc_type = layout_child
            .formatting_context()
            .unwrap_or(crate::style::display::FormattingContextType::Block);
          let fc = factory.create(fc_type);
          let child_constraints = LayoutConstraints::new(
            AvailableSpace::Definite(cb.rect.size.width),
            AvailableSpace::Definite(cb.rect.size.height),
          );
          let mut child_fragment = fc.layout(&layout_child, &child_constraints)?;
          let positioned_style = resolve_positioned_style(
            &child.style,
            &cb,
            self.viewport_size,
            self.factory.font_context(),
          );
          let preferred_min_inline = fc
            .compute_intrinsic_inline_size(&layout_child, IntrinsicSizingMode::MinContent)
            .ok();
          let preferred_inline = fc
            .compute_intrinsic_inline_size(&layout_child, IntrinsicSizingMode::MaxContent)
            .ok();
          let preferred_min_block = fc
            .compute_intrinsic_block_size(&layout_child, IntrinsicSizingMode::MinContent)
            .ok();
          let preferred_block = fc
            .compute_intrinsic_block_size(&layout_child, IntrinsicSizingMode::MaxContent)
            .ok();
          // Static position should start at the containing block origin; AbsoluteLayout
          // adds padding/border offsets, so use the content origin here to avoid double
          // counting padding.
          let mut input =
            AbsoluteLayoutInput::new(positioned_style, child_fragment.bounds.size, Point::ZERO);
          input.is_replaced = child.is_replaced();
          input.preferred_min_inline_size = preferred_min_inline;
          input.preferred_inline_size = preferred_inline;
          input.preferred_min_block_size = preferred_min_block;
          input.preferred_block_size = preferred_block;
          let result = abs.layout_absolute(&input, &cb)?;
          child_fragment.bounds = Rect::new(result.position, result.size);
          child_fragment.style = Some(original_style);
          fragment.children_mut().push(child_fragment);
        }
        Ok(())
      };

    if (structure.column_count == 0 || structure.row_count == 0) && captions.is_empty() {
      let width = specified_width.or(containing_width).unwrap_or(0.0);
      let height = table_height.unwrap_or(0.0);
      let mut fragment = FragmentNode::new_with_style(
        Rect::from_xywh(0.0, 0.0, width, height),
        FragmentContent::Block { box_id: None },
        vec![],
        table_box.style.clone(),
      );
      if !running_children.is_empty() {
        let snapshot_constraints = LayoutConstraints::new(
          AvailableSpace::Definite(width.max(0.0)),
          AvailableSpace::Indefinite,
        );
        for (order, (_, running_child)) in running_children.iter().copied().enumerate() {
          let Some(name) = running_child.style.running_position.clone() else {
            continue;
          };

          let mut snapshot_node = running_child.clone();
          let mut snapshot_style = snapshot_node.style.as_ref().clone();
          snapshot_style.running_position = None;
          snapshot_style.position = crate::style::position::Position::Static;
          snapshot_node.style = Arc::new(snapshot_style);

          let fc_type = snapshot_node
            .formatting_context()
            .unwrap_or(FormattingContextType::Block);
          let fc = self.factory.create(fc_type);
          if let Ok(snapshot_fragment) = fc.layout(&snapshot_node, &snapshot_constraints) {
            let anchor_bounds = Rect::from_xywh(0.0, (order as f32) * 1e-4, 0.0, 0.01);
            let mut anchor =
              FragmentNode::new_running_anchor(anchor_bounds, name, snapshot_fragment);
            anchor.style = Some(running_child.style.clone());
            fragment.children_mut().push(anchor);
          }
        }
      }
      if !positioned_children.is_empty() {
        let cb = if table_box.style.position.is_positioned() {
          let padding_origin = Point::new(border_left + pad_left, border_top + pad_top);
          let padding_rect = Rect::new(
            padding_origin,
            crate::geometry::Size::new(
              width - border_left - border_right,
              height - border_top - border_bottom,
            ),
          );
          let block_base = if table_box.style.height.is_some() {
            Some(padding_rect.size.height)
          } else {
            None
          };
          ContainingBlock::with_viewport_and_bases(
            padding_rect,
            self.viewport_size,
            Some(padding_rect.size.width),
            block_base,
          )
        } else {
          self.nearest_positioned_cb
        };
        place_out_of_flow(&mut fragment, cb)?;
      }
      if !has_running_children {
        layout_cache_store(
          box_node,
          FormattingContextType::Table,
          constraints,
          &fragment,
          self.viewport_size,
        );
      }
      return Ok(fragment);
    }

    let mut column_constraints: Vec<ColumnConstraints> = (0..structure.column_count)
      .map(|_| ColumnConstraints::new(0.0, 0.0))
      .collect();
    let mode = if structure.is_fixed_layout {
      DistributionMode::Fixed
    } else {
      DistributionMode::Auto
    };
    let spacing = structure.total_horizontal_spacing();
    let edge_consumption = padding_h
      + if structure.border_collapse == BorderCollapse::Collapse {
        0.0
      } else {
        border_h
      };
    let percent_base = percent_base_width.map(|w| (w - spacing - edge_consumption).max(0.0));

    self.populate_column_constraints(
      table_box,
      &structure,
      &mut column_constraints,
      mode,
      percent_base,
      &style_override_cache,
    );
    self.normalize_percentage_constraints(&mut column_constraints, percent_base);

    if dump {
      let desc: String = column_constraints
        .iter()
        .enumerate()
        .map(|(i, c)| {
          format!(
            "#{} min={:.2} max={:.2} fixed={:?} pct={:?} flex={}",
            i, c.min_width, c.max_width, c.fixed_width, c.percentage, c.is_flexible
          )
        })
        .collect::<Vec<_>>()
        .join(" | ");
      eprintln!("column constraints: {}", desc);
    }

    let min_content_sum: f32 = column_constraints.iter().map(|c| c.min_width).sum();
    let max_content_sum: f32 = column_constraints.iter().map(|c| c.max_width).sum();
    let mut available_content = match (table_width, constraints.available_width) {
      (Some(w), _) => (w - spacing - edge_consumption).max(0.0),
      (None, AvailableSpace::Definite(w)) => (w - spacing - edge_consumption).max(0.0),
      (None, AvailableSpace::MinContent) => min_content_sum,
      (None, AvailableSpace::MaxContent) | (None, AvailableSpace::Indefinite) => max_content_sum,
    };
    if !available_content.is_finite() {
      // Prefer the widest finite bound; fall back to min content to stay stable.
      if max_content_sum.is_finite() {
        available_content = max_content_sum;
      } else if min_content_sum.is_finite() {
        available_content = min_content_sum;
      } else {
        available_content = 0.0;
      }
    }
    // Honor min/max width constraints even when the table width is auto: expand or clamp the content
    // box before column distribution so columns and fragment bounds agree with the final border box.
    if let Some(min_w) = min_width {
      let min_content = (min_w - spacing - edge_consumption).max(0.0);
      available_content = available_content.max(min_content);
    }
    if let Some(max_w) = max_width {
      let max_content = (max_w - spacing - edge_consumption).max(0.0);
      available_content = available_content.min(max_content);
    }
    let distributor = ColumnDistributor::new(mode).with_min_column_width(0.0);
    let distribution = distributor.distribute(&column_constraints, available_content);
    if let Some(err) = distributor.take_timeout_error() {
      return Err(err);
    }
    let mut col_widths = if distribution.widths.len() == structure.column_count {
      distribution.widths
    } else {
      vec![0.0; structure.column_count]
    };

    // Fallback: if all columns computed to zero, distribute available space equally
    if col_widths.iter().all(|w| *w == 0.0) && available_content > 0.0 && !col_widths.is_empty() {
      let per = available_content / structure.column_count as f32;
      col_widths = vec![per; structure.column_count];
    }

    // If the table width is specified and columns don't fill the available content width, expand flexible columns.
    if table_width.is_some()
      && mode == DistributionMode::Auto
      && available_content.is_finite()
      && !col_widths.is_empty()
    {
      let current: f32 = col_widths.iter().sum();
      if available_content > current + 0.01 {
        let flex_indices: Vec<usize> = column_constraints
          .iter()
          .enumerate()
          .filter(|(_, c)| c.is_flexible && c.fixed_width.is_none() && c.percentage.is_none())
          .map(|(i, _)| i)
          .collect();
        let indices = if flex_indices.is_empty() {
          (0..col_widths.len()).collect()
        } else {
          flex_indices
        };
        let extra = available_content - current;
        let mut finite_total = 0.0;
        let mut infinite_indices = Vec::new();
        for &idx in &indices {
          let flex = column_constraints
            .get(idx)
            .map(|c| c.flexibility_range())
            .unwrap_or(0.0);
          if flex.is_finite() {
            finite_total += flex;
          } else {
            infinite_indices.push(idx);
          }
        }

        if !infinite_indices.is_empty() {
          let share = extra / infinite_indices.len() as f32;
          for idx in infinite_indices {
            if let Some(col) = col_widths.get_mut(idx) {
              *col += share;
            }
          }
        } else if finite_total > 0.0 {
          for idx in indices {
            let flex = column_constraints
              .get(idx)
              .map(|c| c.flexibility_range())
              .unwrap_or(0.0);
            let weight = flex / finite_total;
            if let Some(col) = col_widths.get_mut(idx) {
              *col += extra * weight;
            }
          }
        } else {
          let share = extra / indices.len() as f32;
          for idx in indices {
            if let Some(col) = col_widths.get_mut(idx) {
              *col += share;
            }
          }
        }
      }
    }
    if dump {
      let widths: String = col_widths
        .iter()
        .map(|w| format!("{:.2}", w))
        .collect::<Vec<_>>()
        .join(", ");
      eprintln!(
        "table columns ({} cols): [{}] (available_content {:.2}, table_width {:?})",
        col_widths.len(),
        widths,
        available_content,
        table_width
      );
    }

    let h_spacing = structure.border_spacing.0;
    let v_spacing = structure.border_spacing.1;
    let mut col_prefix = Vec::with_capacity(col_widths.len() + 1);
    col_prefix.push(0.0);
    for width in &col_widths {
      check_layout_deadline(&mut deadline_counter)?;
      let next = col_prefix.last().copied().unwrap_or(0.0) + *width;
      col_prefix.push(next);
    }
    let cell_bfc = BlockFormattingContext::with_font_context_and_viewport(
      self.factory.font_context().clone(),
      self.factory.viewport_size(),
    )
    .with_parallelism(self.parallelism);
    let mut fragments = Vec::new();

    struct LaidOutCell {
      cell: CellInfo,
      fragment: FragmentNode,
      vertical_align: VerticalAlign,
      baseline: Option<f32>,
      height: f32,
    }

    // Layout all cells to obtain their fragments and measure heights, then distribute row heights with rowspans.
    enum CellOutcome {
      Success(LaidOutCell),
      Missing,
      Failed,
    }

    let layout_single_cell = |cell: &CellInfo| -> CellOutcome {
      let max_prefix_idx = col_prefix.len().saturating_sub(1);
      let start = cell.col.min(max_prefix_idx);
      let span_end = (cell.col + cell.colspan).min(max_prefix_idx);
      let sum_cols = col_prefix[span_end] - col_prefix[start];
      let width = sum_cols + h_spacing * (cell.colspan.saturating_sub(1) as f32);
      if dump {
        eprintln!(
          "  cell r{} c{} span={} width={:.2} min={:.2} max={:.2}",
          cell.row, cell.col, cell.colspan, width, cell.min_width, cell.max_width
        );
      }

      let Some(row) = source_rows.get(cell.source_row) else {
        return CellOutcome::Missing;
      };
      let Some(cell_box) = row.children.get(cell.box_index) else {
        return CellOutcome::Missing;
      };
      let row_vertical_align = structure.rows.get(cell.row).and_then(|r| r.vertical_align);
      let effective_vertical_align = if cell_box.style.vertical_align_specified {
        cell_box.style.vertical_align
      } else {
        row_vertical_align.unwrap_or(cell_box.style.vertical_align)
      };
      match self.layout_cell(
        cell_box,
        width,
        structure.border_collapse,
        &cell_bfc,
        &style_override_cache,
      ) {
        Ok(fragment) => {
          if dump {
            let b = fragment.bounds;
            eprintln!(
              "    cell fragment raw bounds: x={:.2} y={:.2} w={:.2} h={:.2}",
              b.x(),
              b.y(),
              b.width(),
              b.height()
            );
          }
          let height = fragment.bounds.height();
          let baseline = cell_baseline(&fragment);
          CellOutcome::Success(LaidOutCell {
            cell: cell.clone(),
            fragment,
            vertical_align: effective_vertical_align,
            baseline,
            height,
          })
        }
        Err(_) => CellOutcome::Failed,
      }
    };

    let outcomes: Vec<CellOutcome> =
      if self.parallelism.should_parallelize(structure.cells.len()) && !dump {
        let deadline = active_deadline();
        structure
          .cells
          .par_iter()
          .map(|cell| {
            with_deadline(deadline.as_ref(), || {
              crate::layout::engine::debug_record_parallel_work();
              layout_single_cell(cell)
            })
          })
          .collect()
      } else {
        structure.cells.iter().map(layout_single_cell).collect()
      };
    let mut laid_out_cells = Vec::new();
    let mut failed_cells = 0usize;
    for outcome in outcomes {
      check_layout_deadline(&mut deadline_counter)?;
      match outcome {
        CellOutcome::Success(cell) => laid_out_cells.push(cell),
        CellOutcome::Failed => failed_cells += 1,
        CellOutcome::Missing => {}
      }
    }

    // Compute row heights, accounting for rowspans and vertical spacing.
    let mut row_heights = vec![0.0f32; structure.row_count];
    for (idx, row) in structure.rows.iter().enumerate() {
      check_layout_deadline(&mut deadline_counter)?;
      if let Some(slot) = row_heights.get_mut(idx) {
        *slot = row.min_height.max(*slot);
      }
    }

    let baseline_split = |laid: &LaidOutCell, heights: &[f32]| {
      let span_end = (laid.cell.row + laid.cell.rowspan).min(heights.len());
      let spacing_total = v_spacing * laid.cell.rowspan.saturating_sub(1) as f32;
      let target_height = if laid.cell.rowspan > 1 && span_end > laid.cell.row {
        heights
          .get(laid.cell.row..span_end)
          .map(|rows| rows.iter().sum::<f32>() + spacing_total)
          .unwrap_or(laid.height)
      } else {
        heights.get(laid.cell.row).copied().unwrap_or(laid.height)
      };
      let raw_baseline = laid.baseline.unwrap_or(laid.height);
      let baseline = if raw_baseline >= laid.height && target_height > laid.height {
        target_height
      } else {
        raw_baseline
      };
      if laid.cell.rowspan > 1 && span_end > laid.cell.row {
        spanning_baseline_allocation(target_height, baseline, laid.cell.row, span_end, heights)
      } else {
        let clamped = baseline.min(target_height);
        (clamped, (target_height - clamped).max(0.0))
      }
    };

    // Baseline-aligned cells reserve enough height in their starting row to place the baseline.
    for laid in &laid_out_cells {
      if !laid.vertical_align.is_baseline_relative() {
        continue;
      }
      let (top, bottom) = baseline_split(laid, &row_heights);
      if let Some(slot) = row_heights.get_mut(laid.cell.row) {
        *slot = (*slot).max(top + bottom);
      }
    }

    let compute_percent_height_base = |base: f32| {
      let mut content_base = if structure.border_collapse == BorderCollapse::Collapse {
        (base - padding_v - outer_border_v).max(0.0)
      } else {
        (base - padding_v - border_v).max(0.0)
      };
      if structure.border_collapse != BorderCollapse::Collapse {
        let spacing_total = structure.total_vertical_spacing();
        content_base = (content_base - spacing_total).max(0.0);
      }
      content_base
    };

    let percent_height_base = table_height.map(|base| compute_percent_height_base(base));

    let row_floor = |idx: usize, current: f32| -> f32 {
      let row = structure.rows.get(idx);
      if row.is_none() {
        return current;
      }
      let row = row.unwrap();
      let mut floor = current;
      if let Some((Some(min_len), _)) = Some(resolve_row_min_max(row, percent_height_base)) {
        floor = floor.max(min_len);
      }
      if let Some(spec) = row.specified_height {
        match spec {
          SpecifiedHeight::Fixed(h) => floor = floor.max(h),
          SpecifiedHeight::Percent(pct) => {
            if let Some(base) = percent_height_base {
              floor = floor.max((pct / 100.0) * base);
            }
          }
          SpecifiedHeight::Auto => {}
        }
      }
      floor
    };

    for laid in &laid_out_cells {
      if laid.cell.rowspan == 1 {
        row_heights[laid.cell.row] = row_heights[laid.cell.row].max(laid.height);
      } else {
        let span_start = laid.cell.row;
        let span_end = (laid.cell.row + laid.cell.rowspan).min(structure.row_count);
        let spacing_total = v_spacing * (laid.cell.rowspan.saturating_sub(1) as f32);
        let span_height = (laid.height - spacing_total).max(0.0);
        let current_total: f32 = row_heights[span_start..span_end].iter().sum();
        let remaining = (span_height - current_total).max(0.0);
        if remaining > 0.0 && span_start < span_end {
          let percent_base = percent_height_base;
          let row_floor_fn = |idx: usize, _current: f32| -> f32 { row_floor(idx, 0.0) };
          distribute_rowspan_targets_range(
            span_start,
            span_end,
            &mut row_heights,
            &structure.rows,
            remaining,
            percent_base,
            &row_floor_fn,
            &|current, share| current + share,
          );
        }
      }
    }

    // Preserve the content-driven minimums so later distribution never shrinks below cell content.
    let content_min_heights = row_heights.clone();

    let percent_height_base = table_height.map(|base| compute_percent_height_base(base));

    // Enforce row-specified minimums (length or percentage of table height) and percent targets.
    for (idx, row) in structure.rows.iter().enumerate() {
      let (min_len, max_len) = resolve_row_min_max(row, percent_height_base);
      if let Some(min) = min_len {
        row_heights[idx] = row_heights[idx].max(min);
      }
      if let Some(max) = max_len {
        row_heights[idx] = row_heights[idx].min(max);
      }
      if let Some(SpecifiedHeight::Fixed(px)) = row.specified_height {
        row_heights[idx] = row_heights[idx].max(px);
      }
      if let (Some(base), Some(SpecifiedHeight::Percent(pct))) =
        (percent_height_base, row.specified_height)
      {
        let target = (pct / 100.0) * base;
        row_heights[idx] = row_heights[idx].max(target);
      }
    }

    apply_row_group_constraints(
      &mut row_heights,
      &structure.rows,
      &row_group_constraints,
      percent_height_base,
      v_spacing,
      &content_min_heights,
    );

    // If the table has a definite height, adjust rows so percent rows meet their targets and remaining space is distributed.
    if let Some(percent_base) = percent_height_base {
      let target_rows = percent_base.max(0.0);
      let mut percent_rows = Vec::new();
      let mut adjustable_rows = Vec::new();
      for (idx, row) in structure.rows.iter().enumerate() {
        if matches!(row.specified_height, Some(SpecifiedHeight::Percent(_))) {
          percent_rows.push(idx);
        } else {
          adjustable_rows.push(idx);
        }
      }
      let percent_total: f32 = percent_rows.iter().map(|i| row_heights[*i]).sum();
      let flex_indices = if !adjustable_rows.is_empty() {
        adjustable_rows
      } else {
        percent_rows.clone()
      };

      let available = target_rows - percent_total;
      let flex_total: f32 = flex_indices.iter().map(|i| row_heights[*i]).sum();

      if available > 0.0 {
        if flex_total > 0.0 {
          let scale = available / flex_total;
          for i in &flex_indices {
            row_heights[*i] = (row_heights[*i] * scale).max(content_min_heights[*i]);
          }
        } else if !flex_indices.is_empty() {
          let share = available / flex_indices.len() as f32;
          for i in &flex_indices {
            row_heights[*i] = share;
          }
        }
      }
      for (idx, min_content) in content_min_heights.iter().enumerate() {
        row_heights[idx] = row_heights[idx].max(*min_content);
      }
      for (idx, row) in structure.rows.iter().enumerate() {
        let (min_len, max_len) = resolve_row_min_max(row, percent_height_base);
        if let Some(min) = min_len {
          row_heights[idx] = row_heights[idx].max(min);
        }
        if let Some(max) = max_len {
          row_heights[idx] = row_heights[idx].min(max);
        }
      }
    }

    apply_row_group_constraints(
      &mut row_heights,
      &structure.rows,
      &row_group_constraints,
      percent_height_base,
      v_spacing,
      &content_min_heights,
    );

    if dump {
      let min_col = col_widths.iter().copied().fold(f32::INFINITY, f32::min);
      let max_col = col_widths.iter().copied().fold(f32::NEG_INFINITY, f32::max);
      let sum_col: f32 = col_widths.iter().sum();
      let min_row = row_heights.iter().copied().fold(f32::INFINITY, f32::min);
      let max_row = row_heights
        .iter()
        .copied()
        .fold(f32::NEG_INFINITY, f32::max);
      let sum_row: f32 = row_heights.iter().sum();
      eprintln!(
                "table layout: cols={} rows={} cells={} laid_out={} failed={} col_sum={:.2} col_min={:.2} col_max={:.2} row_sum={:.2} row_min={:.2} row_max={:.2}",
                structure.column_count,
                structure.row_count,
                structure.cells.len(),
                laid_out_cells.len(),
                failed_cells,
                sum_col,
                min_col,
                max_col,
                sum_row,
                min_row,
                max_row
            );
    }

    let mut row_metrics: Vec<RowMetrics> =
      row_heights.iter().map(|h| RowMetrics::new(*h)).collect();

    for laid in &laid_out_cells {
      let row = &mut row_metrics[laid.cell.row];
      let row_height = row_heights
        .get(laid.cell.row)
        .copied()
        .unwrap_or(row.height);
      let contribution = if laid.cell.rowspan == 1 {
        laid.height
      } else {
        row_height
      };
      row.max_cell_height = row.max_cell_height.max(contribution);

      // CSS 2.1 §17.5.3: row baselines ignore row-spanning cells.
      // Baseline alignment for rows follows CSS 2.1 §17.5.3: the cell with the
      // largest height above (and below) the baseline sets the row's baseline
      // position. Row-spanning cells don't establish a row baseline.
      if laid.vertical_align.is_baseline_relative() && laid.cell.rowspan == 1 {
        let (top, bottom) = baseline_split(laid, &row_heights);
        let clamped_top = top.min(row_height);
        let clamped_bottom = bottom.min((row_height - clamped_top).max(0.0));
        row.has_baseline = true;
        row.baseline_top = row.baseline_top.max(clamped_top);
        row.baseline_bottom = row.baseline_bottom.max(clamped_bottom);
        row.max_cell_height = row.max_cell_height.max(clamped_top + clamped_bottom);
      }
    }

    for (idx, row) in row_metrics.iter_mut().enumerate() {
      let baseline_height = row.baseline_height();
      let max_required = row.max_cell_height.max(baseline_height);
      if max_required > row.height {
        row.height = max_required;
      }
      if row.height <= 0.0 {
        row.height = 1.0;
      }
      let (min_h, max_h) = resolve_row_min_max(&structure.rows[idx], percent_height_base);
      if let Some(min) = min_h {
        row.height = row.height.max(min);
      }
      if let Some(max) = max_h {
        row.height = row.height.min(max);
      }
    }

    // If the table has a definite (or minimum) height and rows don't fill it, distribute the extra.
    if let Some(target_height) = table_height {
      let rows_space: f32 = row_metrics.iter().map(|r| r.height).sum();
      let target_rows =
        percent_height_base.unwrap_or_else(|| compute_percent_height_base(target_height));
      if target_rows > rows_space && !row_metrics.is_empty() {
        let extra = target_rows - rows_space;
        let flex_indices: Vec<usize> = structure
          .rows
          .iter()
          .enumerate()
          .filter_map(|(idx, row)| match row.specified_height {
            Some(SpecifiedHeight::Fixed(_)) | Some(SpecifiedHeight::Percent(_)) => None,
            _ => Some(idx),
          })
          .collect();
        let targets = if flex_indices.is_empty() {
          (0..structure.row_count).collect::<Vec<_>>()
        } else {
          flex_indices
        };

        distribute_extra_row_height_with_groups(
          &mut row_metrics,
          &structure.rows,
          &targets,
          extra,
          percent_height_base,
          &row_to_group,
          &row_group_constraints,
          v_spacing,
        );
      }
    }

    // Border-collapse adjustments
    let collapsed_borders: Option<CollapsedBorders> =
      if structure.border_collapse == BorderCollapse::Collapse {
        Some(compute_collapsed_borders(&table_box, &structure))
      } else {
        None
      };

    let mut vertical_line_max: Vec<f32> = Vec::with_capacity(structure.column_count + 1);
    let mut horizontal_line_max: Vec<f32> = Vec::with_capacity(structure.row_count + 1);
    let mut column_line_pos = Vec::new();
    let mut row_line_pos = Vec::new();
    let mut row_offsets = Vec::with_capacity(structure.row_count);
    let mut col_offsets = Vec::with_capacity(structure.column_count);
    let mut row_prefix_heights: Option<Vec<f32>> = None;

    // Collapsed borders place border strokes on grid lines, with half of the outer
    // borders inside the table box and half potentially spilling into the margin
    // area (CSS 2.2 §17.6.2). Compute line centers and cell start offsets from the
    // resolved line widths so that the table width/height include only half of the
    // outer collapsed borders.
    let (content_width, content_height, content_origin_x, content_origin_y) =
      match collapsed_borders.as_ref() {
        Some(collapsed_borders) => {
          for (idx, segments) in collapsed_borders.vertical.iter().enumerate() {
            let width = if idx == 0 || idx == structure.column_count {
              // Table left/right border widths are based on the first row's collapsed border
              // (CSS 2.2 §17.6.2), with any excess in later rows spilling into the margin.
              if structure.row_count > 0 {
                segments.first().map(|b| b.width).unwrap_or(0.0)
              } else {
                0.0
              }
            } else {
              segments.iter().map(|b| b.width).fold(0.0, f32::max)
            };
            vertical_line_max.push(width);
          }
          horizontal_line_max = collapsed_borders
            .horizontal
            .iter()
            .map(|segments| segments.iter().map(|b| b.width).fold(0.0, f32::max))
            .collect();

          let (cols, starts, collapsed_width) =
            collapsed_line_positions(&col_widths, &vertical_line_max, pad_left, pad_right);
          column_line_pos = cols;
          col_offsets = starts;

          let row_heights: Vec<f32> = row_metrics.iter().map(|r| r.height).collect();
          let (rows, starts, collapsed_height) =
            collapsed_line_positions(&row_heights, &horizontal_line_max, pad_top, pad_bottom);
          row_line_pos = rows;
          row_offsets = starts;

          let content_origin_x = col_offsets.first().copied().unwrap_or(pad_left)
            - vertical_line_max.first().copied().unwrap_or(0.0) * 0.5;
          let content_origin_y = row_offsets.first().copied().unwrap_or(pad_top)
            - horizontal_line_max.first().copied().unwrap_or(0.0) * 0.5;

          (
            collapsed_width,
            collapsed_height,
            content_origin_x,
            content_origin_y,
          )
        }
        None => {
          let content_origin_y = border_top + pad_top;
          let mut y = content_origin_y + v_spacing * 0.5;

          if dump {
            let preview: String = row_metrics
              .iter()
              .take(8)
              .map(|r| format!("{:.2}", r.height))
              .collect::<Vec<_>>()
              .join(", ");
            eprintln!(
              "  row heights (first {} of {}): {}",
              row_metrics.len().min(8),
              row_metrics.len(),
              preview
            );
          }

          for row in &row_metrics {
            row_offsets.push(y);
            y += row.height;
            y += v_spacing;
          }

          if dump {
            let preview: String = row_offsets
              .iter()
              .take(8)
              .map(|offset| format!("{:.2}", offset))
              .collect::<Vec<_>>()
              .join(", ");
            eprintln!(
              "  row offsets (first {} of {}): {}",
              row_offsets.len().min(8),
              row_offsets.len(),
              preview
            );
          }

          // Precompute column offsets for positioning in the separated model.
          let start_x = border_left + pad_left;
          let mut x = start_x + h_spacing * 0.5;
          for width in col_widths.iter().take(structure.column_count) {
            col_offsets.push(x);
            x += width + h_spacing;
          }

          let mut prefix = Vec::with_capacity(row_metrics.len().saturating_add(1));
          prefix.push(0.0);
          for row in &row_metrics {
            let next = prefix.last().copied().unwrap_or(0.0) + row.height + v_spacing;
            prefix.push(next);
          }
          row_prefix_heights = Some(prefix);

          let width = if col_widths.is_empty() {
            available_content
          } else {
            let total_spacing = h_spacing * structure.column_count as f32;
            col_widths.iter().sum::<f32>() + total_spacing
          };
          let height = if structure.row_count > 0 {
            row_offsets
              .last()
              .map(|start| {
                start + row_metrics.last().map(|r| r.height).unwrap_or(0.0) + v_spacing * 0.5
              })
              .unwrap_or(0.0)
              - content_origin_y
          } else {
            0.0
          };
          (width, height, start_x, content_origin_y)
        }
      };

    let mut stripped_border_cache: HashMap<usize, Arc<ComputedStyle>> = HashMap::new();

    let table_collapsed_borders = collapsed_borders.as_ref().map(|borders| {
      Arc::new(build_table_collapsed_borders_metadata(
        &structure,
        borders,
        &column_line_pos,
        &row_line_pos,
        &vertical_line_max,
        &horizontal_line_max,
      ))
    });

    // Column group and column backgrounds precede row backgrounds/cells.
    let mut column_styles: Vec<Option<Arc<ComputedStyle>>> = vec![None; structure.column_count];
    let mut column_groups: Vec<(usize, usize, Arc<ComputedStyle>)> = Vec::new();
    let mut source_col_idx = 0usize;
    for child in table_box
      .children
      .iter()
      .filter(|child| child.style.running_position.is_none())
    {
      match TableStructure::get_table_element_type(child) {
        TableElementType::Column => {
          if let Some(visible) = source_col_to_visible.get(source_col_idx).and_then(|m| *m) {
            column_styles[visible] = Some(child.style.clone());
          }
          source_col_idx += 1;
        }
        TableElementType::ColumnGroup => {
          let mut first_visible = None;
          let mut last_visible = None;
          if !child.children.is_empty() {
            for col_child in &child.children {
              if TableStructure::get_table_element_type(col_child) == TableElementType::Column {
                if let Some(visible) = source_col_to_visible.get(source_col_idx).and_then(|m| *m) {
                  column_styles[visible] = Some(col_child.style.clone());
                  first_visible.get_or_insert(visible);
                  last_visible = Some(visible);
                }
                source_col_idx += 1;
              }
            }
          } else {
            if let Some(visible) = source_col_to_visible.get(source_col_idx).and_then(|m| *m) {
              column_styles[visible] = Some(child.style.clone());
              first_visible.get_or_insert(visible);
              last_visible = Some(visible);
            }
            source_col_idx += 1;
          }
          if let (Some(start), Some(end)) = (first_visible, last_visible) {
            column_groups.push((start, end + 1, child.style.clone()));
          }
        }
        _ => {}
      }
    }

    let mut push_column_span_fragment = |fragments: &mut Vec<FragmentNode>,
                                         cache: &mut HashMap<usize, Arc<ComputedStyle>>,
                                         style: Arc<ComputedStyle>,
                                         start: usize,
                                         end: usize| {
      if start >= end || start >= col_offsets.len() {
        return;
      }
      if !style_paints_background_or_border(&style, false) {
        return;
      }
      let style = strip_borders_cached(&style, cache);
      let mut x = if structure.border_collapse == BorderCollapse::Collapse {
        col_offsets.get(start).copied().unwrap_or(0.0)
          - vertical_line_max.get(start).copied().unwrap_or(0.0)
      } else {
        (col_offsets.get(start).copied().unwrap_or(content_origin_x) - h_spacing * 0.5)
          .max(content_origin_x)
      };
      let mut right = if structure.border_collapse == BorderCollapse::Collapse {
        let base = col_offsets.get(end.saturating_sub(1)).copied().unwrap_or(x)
          + col_widths
            .get(end.saturating_sub(1))
            .copied()
            .unwrap_or(0.0);
        base + vertical_line_max.get(end).copied().unwrap_or(0.0)
      } else {
        let base = col_offsets.get(end.saturating_sub(1)).copied().unwrap_or(x)
          + col_widths
            .get(end.saturating_sub(1))
            .copied()
            .unwrap_or(0.0);
        base + h_spacing * 0.5
      };
      let max_x = content_origin_x + content_width;
      x = x.max(content_origin_x);
      right = right.min(max_x);
      let width = (right - x).max(0.0);
      if width <= 0.0 || content_height <= 0.0 {
        return;
      }
      let rect = Rect::from_xywh(x, content_origin_y, width, content_height);
      fragments.push(FragmentNode::new_with_style(
        rect,
        FragmentContent::Block { box_id: None },
        Vec::new(),
        style,
      ));
    };

    for (start, end, style) in column_groups {
      push_column_span_fragment(
        &mut fragments,
        &mut stripped_border_cache,
        style,
        start,
        end,
      );
    }
    for (idx, style) in column_styles.into_iter().enumerate() {
      if let Some(style) = style {
        push_column_span_fragment(
          &mut fragments,
          &mut stripped_border_cache,
          style,
          idx,
          idx + 1,
        );
      }
    }

    // Paint order backgrounds before cells.
    // Row groups
    let mut row_styles: Vec<Option<Arc<ComputedStyle>>> = vec![None; structure.row_count];
    let mut row_groups: Vec<(usize, usize, Arc<ComputedStyle>)> = Vec::new();
    let mut row_cursor = 0usize;
    for child in table_box
      .children
      .iter()
      .filter(|child| child.style.running_position.is_none())
    {
      match TableStructure::get_table_element_type(child) {
        TableElementType::RowGroup
        | TableElementType::HeaderGroup
        | TableElementType::FooterGroup => {
          let mut first_visible = None;
          let mut last_visible = None;
          for row_child in &child.children {
            if TableStructure::get_table_element_type(row_child) == TableElementType::Row {
              if let Some(visible) = source_row_to_visible.get(row_cursor).and_then(|m| *m) {
                row_styles[visible] = Some(row_child.style.clone());
                first_visible.get_or_insert(visible);
                last_visible = Some(visible);
              }
              row_cursor += 1;
            }
          }
          if let (Some(start), Some(end)) = (first_visible, last_visible) {
            row_groups.push((start, end + 1, child.style.clone()));
          }
        }
        TableElementType::Row => {
          if let Some(visible) = source_row_to_visible.get(row_cursor).and_then(|m| *m) {
            row_styles[visible] = Some(child.style.clone());
          }
          row_cursor += 1;
        }
        _ => {}
      }
    }

    for (start, end, style) in row_groups {
      let style = strip_borders_cached(&style, &mut stripped_border_cache);
      if start >= row_offsets.len() {
        break;
      }
      let top = row_offsets[start];
      let bottom = if end < row_offsets.len() {
        row_offsets[end]
      } else {
        row_offsets.last().copied().unwrap_or(top)
          + row_metrics.last().map(|r| r.height).unwrap_or(0.0)
          + if structure.border_collapse != BorderCollapse::Collapse {
            v_spacing
          } else {
            0.0
          }
      };
      let height = (bottom - top).max(0.0);
      let rect = Rect::from_xywh(content_origin_x, top, content_width, height);
      fragments.push(FragmentNode::new_with_style(
        rect,
        FragmentContent::Block { box_id: None },
        Vec::new(),
        style,
      ));
    }

    // Rows
    for (idx, style) in row_styles.into_iter().enumerate() {
      if let Some(style) = style {
        let style = strip_borders_cached(&style, &mut stripped_border_cache);
        let top = row_offsets.get(idx).copied().unwrap_or(0.0);
        let height = row_metrics.get(idx).map(|r| r.height).unwrap_or(0.0);
        let rect = Rect::from_xywh(content_origin_x, top, content_width, height);
        fragments.push(FragmentNode::new_with_style(
          rect,
          FragmentContent::Block { box_id: None },
          Vec::new(),
          style,
        ));
      }
    }

    // Add explicit row boundary markers to make fragmentation row-aware.
    for (idx, offset) in row_offsets.iter().enumerate() {
      let height = row_metrics.get(idx).map(|r| r.height).unwrap_or(0.0);
      let mut marker_style = crate::style::ComputedStyle::default();
      marker_style.display = Display::TableRow;
      let rect = Rect::from_xywh(content_origin_x, *offset, content_width, height);
      fragments.push(FragmentNode::new_with_style(
        rect,
        FragmentContent::Block { box_id: None },
        Vec::new(),
        Arc::new(marker_style),
      ));
    }

    // Position cell fragments with vertical alignment within their row block.
    // Preserve row/column traversal order (CSS 2.1 row baseline rules) without moving fragments.
    let row_metric_heights: Vec<f32> = row_metrics.iter().map(|r| r.height).collect();
    // Cells are collected from TableStructure in row-major order; preserve that order per row to
    // keep paint order stable without shuffling the fragments themselves.
    let mut cells_by_row: Vec<Vec<usize>> = vec![Vec::new(); structure.row_count];
    let mut fallback_indices = Vec::new();
    for (idx, laid) in laid_out_cells.iter().enumerate() {
      if let Some(bucket) = cells_by_row.get_mut(laid.cell.row) {
        bucket.push(idx);
      } else {
        fallback_indices.push(idx);
      }
    }
    if cfg!(debug_assertions) {
      for bucket in &cells_by_row {
        let mut last_col = None;
        for &idx in bucket {
          let col = laid_out_cells[idx].cell.col;
          if let Some(prev) = last_col {
            debug_assert!(prev <= col, "cells in a row should appear in column order");
          }
          last_col = Some(col);
        }
      }
    }
    let placement_order = cells_by_row
      .into_iter()
      .flatten()
      .chain(fallback_indices.into_iter())
      .collect::<Vec<_>>();

    // Compute table baseline per CSS 2.1: the baseline of the first row that has a
    // baseline-aligned, non-rowspanning cell; otherwise the bottom content edge of the first row.
    let table_baseline = row_metrics
      .iter()
      .enumerate()
      .find(|(idx, r)| r.has_baseline && *idx < row_offsets.len())
      .map(|(idx, r)| row_offsets[idx] + r.baseline_top)
      .or_else(|| {
        if !row_metrics.is_empty() && !row_offsets.is_empty() {
          Some(row_offsets[0] + row_metrics[0].height)
        } else {
          None
        }
      });

    let mut laid_out_cells: Vec<Option<LaidOutCell>> =
      laid_out_cells.into_iter().map(Some).collect();

    for idx in placement_order {
      let Some(mut laid) = laid_out_cells.get_mut(idx).and_then(Option::take) else {
        continue;
      };
      let cell = &laid.cell;
      // Compute horizontal position
      let base_x = col_offsets
        .get(cell.col)
        .copied()
        .unwrap_or(content_origin_x);
      let x = base_x;

      let row_start = cell.row;
      let span_end = (cell.row + cell.rowspan).min(row_metrics.len());
      let spanned_height: f32 = if structure.border_collapse == BorderCollapse::Collapse {
        let start = row_line_pos.get(row_start).copied().unwrap_or(0.0);
        let end = row_line_pos.get(span_end).copied().unwrap_or(start);
        (end - start).max(0.0)
      } else if let Some(prefix) = &row_prefix_heights {
        let start = prefix.get(row_start).copied().unwrap_or(0.0);
        let end = prefix.get(span_end).copied().unwrap_or(start);
        (end - start - v_spacing).max(0.0)
      } else {
        row_metrics[row_start..span_end]
          .iter()
          .map(|r| r.height)
          .sum::<f32>()
          + v_spacing * cell.rowspan.saturating_sub(1) as f32
      };

      let (y_offset, aligned_baseline) = match laid.vertical_align {
        VerticalAlign::Top => (0.0, None),
        VerticalAlign::Bottom => ((spanned_height - laid.height).max(0.0), None),
        VerticalAlign::Middle => (((spanned_height - laid.height) / 2.0).max(0.0), None),
        _ => {
          let raw_baseline = laid.baseline.unwrap_or(laid.height);
          let baseline = if raw_baseline >= laid.height && spanned_height > laid.height {
            spanned_height
          } else {
            raw_baseline
          };
          let baseline = if cell.rowspan > 1 {
            let span_end = (cell.row + cell.rowspan).min(row_metrics.len());
            spanning_baseline_allocation(
              spanned_height,
              baseline,
              row_start,
              span_end,
              &row_metric_heights,
            )
            .0
          } else {
            let row_h = row_metrics
              .get(row_start)
              .map(|r| r.height)
              .unwrap_or(baseline);
            baseline.min(row_h).min(spanned_height)
          };
          let row_base = row_metrics
            .get(row_start)
            .map(|r| {
              if r.has_baseline {
                r.baseline_top
              } else {
                r.height
              }
            })
            .unwrap_or(0.0);
          (row_base - baseline, Some(row_base))
        }
      };

      let base_y = row_offsets.get(row_start).copied().unwrap_or(0.0);
      let mut fragment = laid.fragment;
      // Position the cell box at the start of its row span and give it the full spanned height so
      // backgrounds and borders cover the row. Vertical-align is applied to the cell contents by
      // shifting children inside the cell.
      fragment.bounds =
        crate::geometry::Rect::from_xywh(x, base_y, fragment.bounds.width(), spanned_height);

      if y_offset.abs() > 0.0 {
        let delta = Point::new(0.0, y_offset);
        for child in fragment.children_mut() {
          // Shift each direct child; their descendants stay relative to the shifted
          // parent, preventing cumulative translations down the subtree.
          child.bounds = child.bounds.translate(delta);
        }
      }
      if let Some(baseline) = aligned_baseline {
        fragment.baseline = Some(baseline);
      }
      fragments.push(fragment);
    }

    let mut total_width = if structure.border_collapse == BorderCollapse::Collapse {
      content_width
    } else {
      content_width + padding_h + border_h
    };
    total_width = clamp_to_min_max(total_width, min_width, max_width);
    let mut total_height = if let Some(specified) = table_height {
      specified
    } else {
      let mut h = content_height
        + if structure.border_collapse == BorderCollapse::Collapse {
          0.0
        } else {
          padding_v + border_v
        };
      if let Some(max_h) = max_height {
        h = h.min(max_h);
      }
      if let Some(min_h) = min_height {
        h = h.max(min_h);
      }
      h
    };
    // Apply non-visual min/max clamps from the table box to the final border box per CSS 2.1 §17.5/§10.4.
    if let Some(min_w) = resolved_min_width {
      total_width = total_width.max(min_w);
    }
    if let Some(max_w) = resolved_max_width {
      total_width = total_width.min(max_w);
    }
    if let Some(min_h) = min_height {
      total_height = total_height.max(min_h);
    }
    if let Some(max_h) = max_height {
      total_height = total_height.min(max_h);
    }
    let table_bounds = Rect::from_xywh(0.0, 0.0, total_width.max(0.0), total_height);

    let mut table_style = (*table_box.style).clone();
    if structure.border_collapse == BorderCollapse::Collapse {
      table_style.border_top_width = crate::style::values::Length::px(0.0);
      table_style.border_right_width = crate::style::values::Length::px(0.0);
      table_style.border_bottom_width = crate::style::values::Length::px(0.0);
      table_style.border_left_width = crate::style::values::Length::px(0.0);
    }
    let baseline_offset = table_baseline.map(|b| b - table_bounds.y());

    if captions.is_empty() {
      let mut fragment = FragmentNode::new_with_style(
        table_bounds,
        FragmentContent::Block { box_id: None },
        fragments,
        Arc::new(table_style),
      );
      fragment.baseline = baseline_offset;
      fragment.table_borders = table_collapsed_borders.clone();
      if !running_children.is_empty() {
        let snapshot_constraints = LayoutConstraints::new(
          AvailableSpace::Definite(table_bounds.width().max(0.0)),
          AvailableSpace::Indefinite,
        );
        for (order, (_, running_child)) in running_children.iter().copied().enumerate() {
          let Some(name) = running_child.style.running_position.clone() else {
            continue;
          };

          let mut snapshot_node = running_child.clone();
          let mut snapshot_style = snapshot_node.style.as_ref().clone();
          snapshot_style.running_position = None;
          snapshot_style.position = crate::style::position::Position::Static;
          snapshot_node.style = Arc::new(snapshot_style);

          let fc_type = snapshot_node
            .formatting_context()
            .unwrap_or(FormattingContextType::Block);
          let fc = self.factory.create(fc_type);
          if let Ok(snapshot_fragment) = fc.layout(&snapshot_node, &snapshot_constraints) {
            let anchor_bounds = Rect::from_xywh(0.0, (order as f32) * 1e-4, 0.0, 0.01);
            let mut anchor =
              FragmentNode::new_running_anchor(anchor_bounds, name, snapshot_fragment);
            anchor.style = Some(running_child.style.clone());
            fragment.children_mut().push(anchor);
          }
        }
      }
      if !has_running_children {
        layout_cache_store(
          box_node,
          FormattingContextType::Table,
          constraints,
          &fragment,
          self.viewport_size,
        );
      }
      return Ok(fragment);
    }

    // Table wrapper width must accommodate both the table box and any captions (CSS 2.1 §17.4).
    let mut wrapper_width = table_bounds.width().max(caption_pref_width);
    wrapper_width = clamp_to_min_max(wrapper_width, min_width, max_width);

    // Only the table box itself should render backgrounds/borders. Element-level visual
    // effects (opacity/filters/transforms) apply to the wrapper that contains captions.
    table_style.transform.clear();
    table_style.filter.clear();
    table_style.backdrop_filter.clear();
    table_style.opacity = 1.0;
    table_style.mix_blend_mode = crate::style::types::MixBlendMode::Normal;
    table_style.isolation = crate::style::types::Isolation::Auto;

    let mut table_fragment = FragmentNode::new_with_style(
      table_bounds,
      FragmentContent::Block { box_id: None },
      fragments,
      Arc::new(table_style),
    );
    table_fragment.baseline = baseline_offset;
    table_fragment.table_borders = table_collapsed_borders.clone();

    // Layout captions relative to the table's width.
    let mut wrapper_children = Vec::new();
    let mut offset_y = 0.0;

    let layout_caption = |caption: &BoxNode, y: f32| -> Result<(FragmentNode, f32), LayoutError> {
      let fc_type = caption
        .formatting_context()
        .unwrap_or(crate::style::display::FormattingContextType::Block);
      let fc = self.factory.create(fc_type);
      let mut frag = fc.layout(
        caption,
        &LayoutConstraints::new(
          AvailableSpace::Definite(wrapper_width),
          constraints.available_height,
        ),
      )?;
      frag.bounds = Rect::from_xywh(0.0, 0.0, wrapper_width, frag.bounds.height());
      let height = frag.bounds.height();
      frag.bounds = frag.bounds.translate(Point::new(0.0, y));
      Ok((frag, height))
    };

    let mut caption_offsets: std::collections::HashMap<usize, f32> =
      std::collections::HashMap::new();
    for caption in captions
      .iter()
      .copied()
      .filter(|c| matches!(c.style.caption_side, CaptionSide::Top))
    {
      let y = offset_y;
      let (frag, h) = layout_caption(caption, y)?;
      offset_y += h;
      caption_offsets.insert(caption.id, y);
      wrapper_children.push(frag);
    }

    let table_origin_y = offset_y;
    let mut table_translated = table_fragment;
    table_translated.bounds = table_translated
      .bounds
      .translate(Point::new(0.0, table_origin_y));
    offset_y += table_translated.bounds.height();
    wrapper_children.push(table_translated);

    for caption in captions
      .iter()
      .copied()
      .filter(|c| matches!(c.style.caption_side, CaptionSide::Bottom))
    {
      let y = offset_y;
      let (frag, h) = layout_caption(caption, y)?;
      offset_y += h;
      caption_offsets.insert(caption.id, y);
      wrapper_children.push(frag);
    }

    let mut wrapper_style = (*table_box.style).clone();
    // Keep transforms/opacity/filters on the wrapper so they apply to both caption and table,
    // but avoid painting an extra background/border around the combined area.
    wrapper_style.reset_background_to_initial();
    wrapper_style.border_top_width = crate::style::values::Length::px(0.0);
    wrapper_style.border_right_width = crate::style::values::Length::px(0.0);
    wrapper_style.border_bottom_width = crate::style::values::Length::px(0.0);
    wrapper_style.border_left_width = crate::style::values::Length::px(0.0);
    wrapper_style.border_top_style = BorderStyle::None;
    wrapper_style.border_right_style = BorderStyle::None;
    wrapper_style.border_bottom_style = BorderStyle::None;
    wrapper_style.border_left_style = BorderStyle::None;
    wrapper_style.box_shadow.clear();

    let mut wrapper_fragment = FragmentNode::new_with_style(
      Rect::from_xywh(0.0, 0.0, wrapper_width, offset_y),
      FragmentContent::Block { box_id: None },
      wrapper_children,
      Arc::new(wrapper_style),
    );

    if !running_children.is_empty() {
      let snapshot_constraints = LayoutConstraints::new(
        AvailableSpace::Definite(wrapper_width.max(0.0)),
        AvailableSpace::Indefinite,
      );

      for (order, (idx, running_child)) in running_children.iter().copied().enumerate() {
        let Some(name) = running_child.style.running_position.clone() else {
          continue;
        };

        let mut anchor_y = offset_y;
        for sibling in table_box.children.iter().skip(idx.saturating_add(1)) {
          if sibling.style.running_position.is_some() {
            continue;
          }
          if matches!(
            sibling.style.position,
            crate::style::position::Position::Absolute | crate::style::position::Position::Fixed
          ) {
            continue;
          }
          anchor_y = if matches!(sibling.style.display, Display::TableCaption) {
            caption_offsets
              .get(&sibling.id)
              .copied()
              .unwrap_or(table_origin_y)
          } else {
            table_origin_y
          };
          break;
        }

        let mut snapshot_node = running_child.clone();
        let mut snapshot_style = snapshot_node.style.as_ref().clone();
        snapshot_style.running_position = None;
        snapshot_style.position = crate::style::position::Position::Static;
        snapshot_node.style = Arc::new(snapshot_style);

        let fc_type = snapshot_node
          .formatting_context()
          .unwrap_or(FormattingContextType::Block);
        let fc = self.factory.create(fc_type);
        if let Ok(snapshot_fragment) = fc.layout(&snapshot_node, &snapshot_constraints) {
          let anchor_bounds = Rect::from_xywh(0.0, anchor_y + (order as f32) * 1e-4, 0.0, 0.01);
          let mut anchor = FragmentNode::new_running_anchor(anchor_bounds, name, snapshot_fragment);
          anchor.style = Some(running_child.style.clone());
          wrapper_fragment.children_mut().push(anchor);
        }
      }
    }

    if !positioned_children.is_empty() {
      // Containing block is the table's padding box when the table itself is positioned; otherwise, inherit.
      let cb = if table_box.style.position.is_positioned() {
        let padding_origin = Point::new(
          border_left + pad_left,
          table_origin_y + border_top + pad_top,
        );
        let padding_rect = Rect::new(
          padding_origin,
          crate::geometry::Size::new(
            table_bounds.width() - border_left - border_right,
            table_bounds.height() - border_top - border_bottom,
          ),
        );
        let block_base = if table_box.style.height.is_some() {
          Some(padding_rect.size.height)
        } else {
          None
        };
        ContainingBlock::with_viewport_and_bases(
          padding_rect,
          self.viewport_size,
          Some(padding_rect.size.width),
          block_base,
        )
      } else {
        self.nearest_positioned_cb
      };

      place_out_of_flow(&mut wrapper_fragment, cb)?;
    }

    if !has_running_children {
      layout_cache_store(
        box_node,
        FormattingContextType::Table,
        constraints,
        &wrapper_fragment,
        self.viewport_size,
      );
    }

    Ok(wrapper_fragment)
  }

  /// Calculates intrinsic inline size for the table
  fn compute_intrinsic_inline_size(
    &self,
    box_node: &BoxNode,
    mode: IntrinsicSizingMode,
  ) -> Result<f32, LayoutError> {
    let normalized_table = self.normalize_table_root(box_node);
    let table_box = normalized_table.as_ref();
    let structure = table_structure_cached(table_box);
    let spacing = structure.total_horizontal_spacing();
    let font_size = table_box.style.font_size;
    let authored_width = table_box
      .style
      .width
      .as_ref()
      .and_then(|len| resolve_length_against(len, font_size, None));
    let resolve_abs_no_pct = |l: &crate::style::values::Length| match l.unit {
      LengthUnit::Percent => 0.0,
      _ if l.unit.is_absolute() => l.to_px(),
      _ => l.value,
    };
    let padding_h_base = resolve_abs_no_pct(&table_box.style.padding_left)
      + resolve_abs_no_pct(&table_box.style.padding_right);
    let border_h_base = resolve_abs_no_pct(&table_box.style.border_left_width)
      + resolve_abs_no_pct(&table_box.style.border_right_width);
    let edge_consumption = if structure.border_collapse == BorderCollapse::Collapse {
      0.0
    } else {
      padding_h_base + border_h_base
    };
    let percent_base = authored_width.map(|w| (w - spacing - edge_consumption).max(0.0));
    let distribution_mode = if structure.is_fixed_layout {
      // Fixed layout tables should not require scanning every cell. When no definite width basis
      // exists (auto widths or intrinsic contexts), percentages are normalized away to keep sizing stable.
      DistributionMode::Fixed
    } else {
      DistributionMode::Auto
    };
    let style_override_cache = StyleOverrideCache::default();
    let mut column_constraints: Vec<ColumnConstraints> = (0..structure.column_count)
      .map(|_| ColumnConstraints::new(0.0, 0.0))
      .collect();
    self.populate_column_constraints(
      table_box,
      &structure,
      &mut column_constraints,
      distribution_mode,
      percent_base,
      &style_override_cache,
    );
    self.normalize_percentage_constraints(&mut column_constraints, percent_base);

    let edges = if structure.border_collapse == BorderCollapse::Collapse {
      0.0
    } else {
      let resolve_abs = |l: &crate::style::values::Length| match l.unit {
        LengthUnit::Percent => percent_base
          .map(|base| (l.value / 100.0) * base)
          .unwrap_or(0.0),
        _ if l.unit.is_absolute() => l.to_px(),
        _ => l.value,
      };
      resolve_abs(&table_box.style.padding_left)
        + resolve_abs(&table_box.style.padding_right)
        + resolve_abs(&table_box.style.border_left_width)
        + resolve_abs(&table_box.style.border_right_width)
    };
    let min_sum: f32 = column_constraints.iter().map(|c| c.min_width).sum();
    let raw_max_sum: f32 = column_constraints.iter().map(|c| c.max_width).sum();
    let max_sum = if raw_max_sum.is_finite() {
      raw_max_sum
    } else {
      column_constraints
        .iter()
        .map(|c| {
          if c.max_width.is_finite() {
            c.max_width
          } else if let Some(fixed) = c.fixed_width {
            fixed
          } else if let (Some(pct), Some(base)) = (c.percentage, percent_base) {
            (pct / 100.0) * base
          } else {
            c.min_width
          }
        })
        .sum()
    };
    let mut base_columns = match mode {
      IntrinsicSizingMode::MinContent => min_sum,
      IntrinsicSizingMode::MaxContent => max_sum,
    };
    if !base_columns.is_finite() {
      base_columns = if min_sum.is_finite() { min_sum } else { 0.0 };
    }
    if distribution_mode == DistributionMode::Fixed {
      if let Some(base) = percent_base {
        base_columns = base_columns.max(base);
      }
    }
    let base = base_columns + spacing + edges;
    let log_ids = runtime::runtime_toggles()
      .usize_list("FASTR_LOG_INTRINSIC_IDS")
      .unwrap_or_default();
    let log_this = !log_ids.is_empty() && log_ids.contains(&table_box.id);
    if log_this {
      let selector = table_box
        .debug_info
        .as_ref()
        .map(|d| d.to_selector())
        .unwrap_or_else(|| "<anon>".to_string());
      let cols: Vec<(f32, f32)> = column_constraints
        .iter()
        .map(|c| (c.min_width, c.max_width))
        .collect();
      eprintln!(
        "[intrinsic-table] id={} selector={} mode={:?} cols={:?} spacing={:.2} edges={:.2}",
        table_box.id, selector, mode, cols, spacing, edges
      );
    }
    let mut caption_min: f32 = 0.0;
    for child in table_box.children.iter().filter(|child| {
      child.style.running_position.is_none()
        && !matches!(
          child.style.position,
          crate::style::position::Position::Absolute | crate::style::position::Position::Fixed
        )
        && matches!(child.style.display, Display::TableCaption)
    }) {
      let fc_type = child
        .formatting_context()
        .unwrap_or(crate::style::display::FormattingContextType::Block);
      let fc = self.factory.create(fc_type);
      if let Ok(w) = fc.compute_intrinsic_inline_size(child, mode) {
        caption_min = caption_min.max(w);
      }
    }
    let width = base.max(caption_min);

    // Apply authored min/max width to intrinsic size per CSS preferred width clamping.
    let font_size = table_box.style.font_size;
    let min_w = resolve_opt_length_against(
      table_box.style.min_width.as_ref(),
      font_size,
      None, /* no containing width */
    );
    let max_w = resolve_opt_length_against(
      table_box.style.max_width.as_ref(),
      font_size,
      None, /* no containing width */
    );
    let clamped = clamp_to_min_max(width, min_w, max_w);

    Ok(clamped.max(0.0))
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::layout::constraints::AvailableSpace;
  use crate::layout::constraints::LayoutConstraints;
  use crate::layout::formatting_context::intrinsic_cache_epoch;
  use crate::layout::formatting_context::intrinsic_cache_use_epoch;
  use crate::style::color::Rgba;
  use crate::style::computed::Visibility;
  use crate::style::display::Display;
  use crate::style::display::FormattingContextType;
  use crate::style::position::Position;
  use crate::style::types::BorderCollapse;
  use crate::style::types::BorderStyle;
  use crate::style::types::CaptionSide;
  use crate::style::types::Direction;
  use crate::style::types::GridTrack;
  use crate::style::types::TableLayout;
  use crate::style::types::VerticalAlign;
  use crate::style::values::CalcLength;
  use crate::style::values::Length;
  use crate::style::ComputedStyle;
  use crate::text::font_loader::FontContext;
  use crate::tree::box_tree::BoxTree;
  use crate::tree::debug::DebugInfo;
  use std::sync::Arc;

  fn create_test_style() -> Arc<ComputedStyle> {
    Arc::new(ComputedStyle::default())
  }

  fn create_table_cell(content: &str) -> BoxNode {
    let style = create_test_style();
    BoxNode::new_block(style, FormattingContextType::Block, vec![]).with_debug_info(DebugInfo::new(
      Some("td".to_string()),
      None,
      vec![content.to_string()],
    ))
  }

  fn collect_table_cell_tops(fragment: &FragmentNode, tops: &mut Vec<f32>) {
    if fragment
      .style
      .as_ref()
      .map(|s| matches!(s.display, Display::TableCell))
      .unwrap_or(false)
    {
      tops.push(fragment.bounds.y());
    }
    for child in fragment.children.iter() {
      collect_table_cell_tops(child, tops);
    }
  }

  fn collect_table_cell_fragments<'a>(
    fragment: &'a FragmentNode,
    cells: &mut Vec<&'a FragmentNode>,
  ) {
    if fragment
      .style
      .as_ref()
      .map(|s| matches!(s.display, Display::TableCell))
      .unwrap_or(false)
    {
      cells.push(fragment);
    }
    for child in fragment.children.iter() {
      collect_table_cell_fragments(child, cells);
    }
  }

  fn create_table_row(cells: Vec<BoxNode>) -> BoxNode {
    let style = create_test_style();
    BoxNode::new_block(style, FormattingContextType::Block, cells).with_debug_info(DebugInfo::new(
      Some("tr".to_string()),
      None,
      vec![],
    ))
  }

  fn create_simple_table(rows: usize, cols: usize) -> BoxNode {
    let style = create_test_style();
    let mut table_rows = Vec::new();

    for _r in 0..rows {
      let mut cells = Vec::new();
      for c in 0..cols {
        cells.push(create_table_cell(&format!("cell_{}", c)));
      }
      table_rows.push(create_table_row(cells));
    }

    BoxNode::new_block(style, FormattingContextType::Table, table_rows)
      .with_debug_info(DebugInfo::new(Some("table".to_string()), None, vec![]))
  }

  fn table_with_loose_cell() -> BoxNode {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    let cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![])
      .with_debug_info(DebugInfo::new(Some("td".to_string()), None, vec![]));

    BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![cell],
    )
    .with_debug_info(DebugInfo::new(Some("table".to_string()), None, vec![]))
  }

  fn normalized_table_with_row_group() -> BoxNode {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    let mut group_style = ComputedStyle::default();
    group_style.display = Display::TableRowGroup;
    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;
    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;

    let cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![])
      .with_debug_info(DebugInfo::new(Some("td".to_string()), None, vec![]));
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell],
    )
    .with_debug_info(DebugInfo::new(Some("tr".to_string()), None, vec![]));
    let group = BoxNode::new_block(
      Arc::new(group_style),
      FormattingContextType::Block,
      vec![row],
    )
    .with_debug_info(DebugInfo::new(Some("tbody".to_string()), None, vec![]));

    BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![group],
    )
    .with_debug_info(DebugInfo::new(Some("table".to_string()), None, vec![]))
  }

  fn table_cell_with_span(colspan: usize, rowspan: usize) -> BoxNode {
    let mut style = ComputedStyle::default();
    style.display = Display::TableCell;
    BoxNode::new_block(Arc::new(style), FormattingContextType::Block, vec![]).with_debug_info(
      DebugInfo::new(Some("td".to_string()), None, vec![]).with_spans(colspan, rowspan),
    )
  }

  fn table_row_with_visibility(cells: Vec<BoxNode>, visibility: Visibility) -> BoxNode {
    let mut style = ComputedStyle::default();
    style.display = Display::TableRow;
    style.visibility = visibility;
    BoxNode::new_block(Arc::new(style), FormattingContextType::Block, cells)
      .with_debug_info(DebugInfo::new(Some("tr".to_string()), None, vec![]))
  }

  fn table_column_with_visibility(visibility: Visibility) -> BoxNode {
    let mut style = ComputedStyle::default();
    style.display = Display::TableColumn;
    style.visibility = visibility;
    BoxNode::new_block(Arc::new(style), FormattingContextType::Block, vec![])
      .with_debug_info(DebugInfo::new(Some("col".to_string()), None, vec![]))
  }

  fn find_fragment_by_box_id<'a>(
    fragment: &'a FragmentNode,
    target: usize,
  ) -> Option<&'a FragmentNode> {
    if matches!(fragment.content, FragmentContent::Block { box_id: Some(id) } if id == target) {
      return Some(fragment);
    }
    for child in fragment.children.iter() {
      if let Some(found) = find_fragment_by_box_id(child, target) {
        return Some(found);
      }
    }
    None
  }

  fn collect_running_anchor_names(fragment: &FragmentNode, names: &mut Vec<String>) {
    if let FragmentContent::RunningAnchor { name, .. } = &fragment.content {
      names.push(name.to_string());
    }
    for child in fragment.children.iter() {
      collect_running_anchor_names(child, names);
    }
  }

  fn collect_positioned_fragments<'a>(fragment: &'a FragmentNode, out: &mut Vec<&'a FragmentNode>) {
    if fragment
      .style
      .as_ref()
      .map(|s| matches!(s.position, Position::Absolute | Position::Fixed))
      .unwrap_or(false)
    {
      out.push(fragment);
    }
    for child in fragment.children.iter() {
      collect_positioned_fragments(child, out);
    }
  }

  fn apply_uniform_border(style: &mut ComputedStyle, width: f32, color: Rgba) {
    style.border_top_style = BorderStyle::Solid;
    style.border_right_style = BorderStyle::Solid;
    style.border_bottom_style = BorderStyle::Solid;
    style.border_left_style = BorderStyle::Solid;
    style.border_top_width = Length::px(width);
    style.border_right_width = Length::px(width);
    style.border_bottom_width = Length::px(width);
    style.border_left_width = Length::px(width);
    style.border_top_color = color;
    style.border_right_color = color;
    style.border_bottom_color = color;
    style.border_left_color = color;
  }

  fn collect_collapsed_border_styles(
    fragment: &FragmentNode,
    vertical: &mut Vec<Arc<ComputedStyle>>,
    horizontal: &mut Vec<Arc<ComputedStyle>>,
    corners: &mut Vec<Arc<ComputedStyle>>,
  ) {
    if let Some(style) = &fragment.style {
      if matches!(fragment.content, FragmentContent::Block { box_id: None }) {
        let epsilon = 1e-6;
        let left = style.border_left_width.value;
        let right = style.border_right_width.value;
        let top = style.border_top_width.value;
        let bottom = style.border_bottom_width.value;
        let has_left = left > epsilon;
        let has_right = right > epsilon;
        let has_top = top > epsilon;
        let has_bottom = bottom > epsilon;
        if has_left && !has_right && !has_top && !has_bottom {
          vertical.push(style.clone());
        } else if has_top && !has_left && !has_right && !has_bottom {
          horizontal.push(style.clone());
        } else if has_left && has_right && has_top && has_bottom {
          corners.push(style.clone());
        }
      }
    }

    for child in &fragment.children {
      collect_collapsed_border_styles(child, vertical, horizontal, corners);
    }
  }

  fn unique_style_count(styles: &[Arc<ComputedStyle>]) -> usize {
    let mut uniques: Vec<*const ComputedStyle> = Vec::new();
    for style in styles {
      let ptr = Arc::as_ptr(style);
      if !uniques.iter().any(|existing| *existing == ptr) {
        uniques.push(ptr);
      }
    }
    uniques.len()
  }

  // -------------------------------------------------------------------------
  // Table fixup fast path
  // -------------------------------------------------------------------------

  #[test]
  fn malformed_table_layout_triggers_fixup() {
    let table = table_with_loose_cell();
    assert!(
      !TableStructureFixer::validate_table_structure(&table),
      "table with a loose cell should be considered malformed"
    );

    let tfc = TableFormattingContext::new();
    let _guard = TableStructureFixer::scoped_fixup_internals_counter();
    let fragment = tfc
      .layout(&table, &LayoutConstraints::definite_width(200.0))
      .expect("layout should fix up malformed tables");

    assert!(
      fragment.children.len() > 0,
      "fixup should yield at least one table fragment child"
    );
    assert_eq!(
      TableStructureFixer::fixup_internals_call_count(),
      1,
      "layout must invoke the fixup path for malformed tables"
    );
  }

  #[test]
  fn validated_table_skips_fixup_in_layout_and_intrinsic() {
    let table = normalized_table_with_row_group();
    assert!(
      TableStructureFixer::validate_table_structure(&table),
      "pre-normalized table should satisfy validation"
    );

    let tfc = TableFormattingContext::new();
    let _guard = TableStructureFixer::scoped_fixup_internals_counter();
    let constraints = LayoutConstraints::definite_width(300.0);
    tfc
      .layout(&table, &constraints)
      .expect("layout should succeed for validated table");
    tfc
      .compute_intrinsic_inline_size(&table, IntrinsicSizingMode::MaxContent)
      .expect("intrinsic sizing should succeed for validated table");

    assert_eq!(
      TableStructureFixer::fixup_internals_call_count(),
      0,
      "validated tables should use the fast path without re-running fixup"
    );
  }

  // -------------------------------------------------------------------------
  // TableStructure Tests
  // -------------------------------------------------------------------------

  #[test]
  fn test_table_structure_new() {
    let structure = TableStructure::new();
    assert_eq!(structure.column_count, 0);
    assert_eq!(structure.row_count, 0);
    assert!(structure.cells.is_empty());
    assert!(structure.columns.is_empty());
    assert!(structure.rows.is_empty());
  }

  #[test]
  fn test_table_structure_default() {
    let structure = TableStructure::default();
    assert_eq!(structure.column_count, 0);
    assert!(!structure.is_fixed_layout);
  }

  #[test]
  fn test_table_structure_respects_table_layout_property() {
    let mut style = ComputedStyle::default();
    style.display = Display::Table;
    style.table_layout = TableLayout::Fixed;
    let table = BoxNode::new_block(Arc::new(style), FormattingContextType::Table, vec![])
      .with_debug_info(DebugInfo::new(Some("table".to_string()), None, vec![]));

    let structure = TableStructure::from_box_tree(&table);
    assert!(structure.is_fixed_layout);
  }

  #[test]
  fn table_structure_cache_respects_epoch() {
    let starting_epoch = intrinsic_cache_epoch();
    intrinsic_cache_use_epoch(starting_epoch.saturating_add(1), true);

    let table = BoxTree::new(create_simple_table(1, 1)).root;
    let first = table_structure_cached(&table);
    let second = table_structure_cached(&table);
    assert!(Arc::ptr_eq(&first, &second));

    intrinsic_cache_use_epoch(intrinsic_cache_epoch().saturating_add(1), false);
    let third = table_structure_cached(&table);
    assert!(!Arc::ptr_eq(&first, &third));

    intrinsic_cache_use_epoch(starting_epoch, true);
  }

  #[test]
  fn rowspan_and_colspan_collision_places_cells_in_first_available_slots() {
    let row0 = table_row_with_visibility(
      vec![table_cell_with_span(1, 1), table_cell_with_span(1, 2)],
      Visibility::Visible,
    );
    let row1 = table_row_with_visibility(
      vec![table_cell_with_span(2, 1), table_cell_with_span(1, 1)],
      Visibility::Visible,
    );

    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row0, row1],
    )
    .with_debug_info(DebugInfo::new(Some("table".to_string()), None, vec![]));

    let structure = TableStructure::from_box_tree(&table);
    assert_eq!(structure.row_count, 2);
    assert_eq!(structure.column_count, 4);

    let span_down = structure
      .cells
      .iter()
      .find(|c| c.source_row == 0 && c.box_index == 1)
      .expect("rowspan cell");
    assert_eq!(
      span_down.col, 1,
      "rowspan cell should follow the first cell"
    );
    assert_eq!(span_down.rowspan, 2);

    let wide_cell = structure
      .cells
      .iter()
      .find(|c| c.source_row == 1 && c.box_index == 0)
      .expect("colspan cell");
    assert_eq!(
      wide_cell.col, 2,
      "colspan cell should skip occupied columns from earlier row spans"
    );
    assert_eq!(wide_cell.colspan, 2);

    let trailing_cell = structure
      .cells
      .iter()
      .find(|c| c.source_row == 1 && c.box_index == 1)
      .expect("later cell should fill earliest hole");
    assert_eq!(
      trailing_cell.col, 0,
      "later cells should reuse early free columns left by larger spans"
    );
    assert_eq!(
      structure.get_cell_at(1, 0).map(|c| c.index),
      Some(trailing_cell.index),
      "grid should point to the trailing cell in freed column"
    );
  }

  #[test]
  fn test_table_respects_explicit_width() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.width = Some(Length::px(300.0));
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    let cell_a = BoxNode::new_block(
      Arc::new(cell_style.clone()),
      FormattingContextType::Block,
      vec![],
    );
    let cell_b = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell_a, cell_b],
    );

    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let tfc = TableFormattingContext::new();
    let constraints = LayoutConstraints::definite_width(800.0);
    let fragment = tfc.layout(&table, &constraints).expect("table layout");

    assert!((fragment.bounds.width() - 300.0).abs() < 0.1);
    assert_eq!(fragment.children.len(), 2);
    assert!((fragment.children[0].bounds.width() - 150.0).abs() < 0.1);
    assert!((fragment.children[1].bounds.width() - 150.0).abs() < 0.1);
  }

  #[test]
  fn test_table_width_respects_min_max() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.width = Some(Length::px(50.0));
    table_style.min_width = Some(Length::px(200.0));
    table_style.max_width = Some(Length::px(250.0));
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    let cell_a = BoxNode::new_block(
      Arc::new(cell_style.clone()),
      FormattingContextType::Block,
      vec![],
    );
    let cell_b = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell_a, cell_b],
    );

    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let tfc = TableFormattingContext::new();
    let constraints = LayoutConstraints::definite_width(800.0);
    let fragment = tfc.layout(&table, &constraints).expect("table layout");

    assert!((fragment.bounds.width() - 200.0).abs() < 0.1);
  }

  #[test]
  fn auto_table_width_honors_min_width() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.min_width = Some(Length::px(300.0));
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    let cell_a = BoxNode::new_block(
      Arc::new(cell_style.clone()),
      FormattingContextType::Block,
      vec![],
    );
    let cell_b = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell_a, cell_b],
    );

    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let tfc = TableFormattingContext::new();
    let constraints = LayoutConstraints::definite_width(10.0);
    let fragment = tfc.layout(&table, &constraints).expect("table layout");

    assert!((fragment.bounds.width() - 300.0).abs() < 0.5);
  }

  #[test]
  fn auto_table_width_honors_max_width() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.max_width = Some(Length::px(100.0));
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut wide_cell_style = ComputedStyle::default();
    wide_cell_style.display = Display::TableCell;
    wide_cell_style.width = Some(Length::px(120.0));
    let cell_a = BoxNode::new_block(
      Arc::new(wide_cell_style.clone()),
      FormattingContextType::Block,
      vec![],
    );
    let cell_b = BoxNode::new_block(
      Arc::new(wide_cell_style),
      FormattingContextType::Block,
      vec![],
    );
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell_a, cell_b],
    );

    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let tfc = TableFormattingContext::new();
    let constraints = LayoutConstraints::definite_width(400.0);
    let fragment = tfc.layout(&table, &constraints).expect("table layout");

    assert!(fragment.bounds.width() <= 100.1);
  }

  #[test]
  fn table_min_width_clamps_border_box() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.min_width = Some(Length::px(200.0));
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;
    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;

    let cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell],
    );
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let tfc = TableFormattingContext::new();
    let fragment = tfc
      .layout(&table, &LayoutConstraints::definite(50.0, 50.0))
      .expect("table layout should succeed");

    assert!(
      fragment.bounds.width() >= 199.9,
      "table min-width should clamp the border box, got {:.2}",
      fragment.bounds.width()
    );
  }

  #[test]
  fn table_max_height_clamps_border_box() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.max_height = Some(Length::px(50.0));
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;
    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    cell_style.height = Some(Length::px(120.0));

    let cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell],
    );
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let tfc = TableFormattingContext::new();
    let fragment = tfc
      .layout(&table, &LayoutConstraints::definite(300.0, 300.0))
      .expect("table layout should succeed");

    assert!(
      fragment.bounds.height() <= 50.1,
      "table max-height should clamp the border box, got {:.2}",
      fragment.bounds.height()
    );
  }

  #[test]
  fn visibility_collapse_remaps_spanning_cells() {
    let cols = vec![
      table_column_with_visibility(Visibility::Visible),
      table_column_with_visibility(Visibility::Collapse),
      table_column_with_visibility(Visibility::Visible),
    ];
    let row0 = table_row_with_visibility(vec![table_cell_with_span(3, 2)], Visibility::Visible);
    let row1 = table_row_with_visibility(vec![table_cell_with_span(1, 1)], Visibility::Collapse);
    let row2 = table_row_with_visibility(
      vec![
        table_cell_with_span(1, 1),
        table_cell_with_span(1, 1),
        table_cell_with_span(1, 1),
      ],
      Visibility::Visible,
    );

    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);
    let mut children = cols;
    children.extend(vec![row0, row1, row2]);
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      children,
    )
    .with_debug_info(DebugInfo::new(Some("table".to_string()), None, vec![]));

    let structure = TableStructure::from_box_tree(&table);
    assert_eq!(structure.column_count, 2);
    assert_eq!(structure.row_count, 2);
    assert_eq!(structure.grid.len(), 2);
    assert_eq!(structure.grid[0].len(), 2);

    let spanning = structure
      .cells
      .iter()
      .find(|c| c.source_row == 0)
      .expect("spanning cell should remain after collapse");
    assert_eq!(spanning.col, 0);
    assert_eq!(spanning.colspan, 2);
    assert_eq!(
      spanning.rowspan, 1,
      "collapsed row should shorten the spanning cell"
    );

    let row2_cells: Vec<_> = structure
      .cells
      .iter()
      .filter(|c| c.source_row == 2)
      .collect();
    assert_eq!(
      row2_cells.len(),
      2,
      "collapsed column should remove the middle cell in the last row"
    );
    let mut cols: Vec<_> = row2_cells.iter().map(|c| c.col).collect();
    cols.sort_unstable();
    assert_eq!(cols, vec![0, 1]);
    assert!(
      structure.get_cell_at(1, 0).is_some(),
      "visible row should remain represented in the grid"
    );
  }

  #[test]
  fn empty_cells_hide_keeps_padding_and_borders_in_layout() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut padded_cell_style = ComputedStyle::default();
    padded_cell_style.display = Display::TableCell;
    padded_cell_style.padding_top = Length::px(10.0);
    padded_cell_style.padding_bottom = Length::px(10.0);
    padded_cell_style.border_top_width = Length::px(5.0);
    padded_cell_style.border_bottom_width = Length::px(5.0);
    padded_cell_style.empty_cells = EmptyCells::Hide;
    let padded_cell = BoxNode::new_block(
      Arc::new(padded_cell_style),
      FormattingContextType::Block,
      vec![],
    );

    let mut visible_cell_style = ComputedStyle::default();
    visible_cell_style.display = Display::TableCell;
    visible_cell_style.padding_top = Length::px(10.0);
    visible_cell_style.padding_bottom = Length::px(10.0);
    visible_cell_style.border_top_width = Length::px(5.0);
    visible_cell_style.border_bottom_width = Length::px(5.0);
    visible_cell_style.empty_cells = EmptyCells::Show;
    let visible_cell = BoxNode::new_block(
      Arc::new(visible_cell_style),
      FormattingContextType::Block,
      vec![],
    );

    let padded_row = BoxNode::new_block(
      Arc::new(row_style.clone()),
      FormattingContextType::Block,
      vec![padded_cell],
    );
    let visible_row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![visible_cell],
    );

    let padded_table = BoxNode::new_block(
      Arc::new(table_style.clone()),
      FormattingContextType::Table,
      vec![padded_row],
    );
    let visible_table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![visible_row],
    );

    let tfc = TableFormattingContext::new();
    let padded_fragment = tfc
      .layout(&padded_table, &LayoutConstraints::definite(200.0, 200.0))
      .expect("layout hide");
    let visible_fragment = tfc
      .layout(&visible_table, &LayoutConstraints::definite(200.0, 200.0))
      .expect("layout show");

    assert!(
      (padded_fragment.bounds.height() - visible_fragment.bounds.height()).abs() < 0.5,
      "empty_cells: hide should not affect layout sizing (only painting)"
    );
  }

  #[cfg(test)]
  fn legacy_get_cell_box<'a>(table_box: &'a BoxNode, cell: &CellInfo) -> Option<&'a BoxNode> {
    let mut row_idx = 0;
    for child in table_box
      .children
      .iter()
      .filter(|child| child.style.running_position.is_none())
    {
      match TableStructure::get_table_element_type(child) {
        TableElementType::RowGroup
        | TableElementType::HeaderGroup
        | TableElementType::FooterGroup => {
          for row_child in &child.children {
            if row_idx == cell.source_row {
              return row_child.children.get(cell.box_index);
            }
            row_idx += 1;
          }
        }
        TableElementType::Row => {
          if row_idx == cell.source_row {
            return child.children.get(cell.box_index);
          }
          row_idx += 1;
        }
        _ => {}
      }
    }
    None
  }

  #[test]
  fn source_row_lookup_matches_legacy_scan() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;

    let mut header_group_style = ComputedStyle::default();
    header_group_style.display = Display::TableHeaderGroup;
    let mut body_group_style = ComputedStyle::default();
    body_group_style.display = Display::TableRowGroup;
    let mut footer_group_style = ComputedStyle::default();
    footer_group_style.display = Display::TableFooterGroup;

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    let make_cell = || {
      BoxNode::new_block(
        Arc::new(cell_style.clone()),
        FormattingContextType::Block,
        vec![],
      )
    };

    let header_row = BoxNode::new_block(
      Arc::new(row_style.clone()),
      FormattingContextType::Block,
      vec![make_cell(), make_cell()],
    );
    let direct_row = BoxNode::new_block(
      Arc::new(row_style.clone()),
      FormattingContextType::Block,
      vec![make_cell(), make_cell(), make_cell()],
    );
    let body_row = BoxNode::new_block(
      Arc::new(row_style.clone()),
      FormattingContextType::Block,
      vec![make_cell()],
    );
    let footer_row = BoxNode::new_block(
      Arc::new(row_style.clone()),
      FormattingContextType::Block,
      vec![make_cell(), make_cell()],
    );

    let header_group = BoxNode::new_block(
      Arc::new(header_group_style),
      FormattingContextType::Block,
      vec![header_row],
    );
    let body_group = BoxNode::new_block(
      Arc::new(body_group_style),
      FormattingContextType::Block,
      vec![body_row],
    );
    let footer_group = BoxNode::new_block(
      Arc::new(footer_group_style),
      FormattingContextType::Block,
      vec![footer_row],
    );

    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![header_group, direct_row, body_group, footer_group],
    );

    let structure = TableStructure::from_box_tree(&table);
    let source_rows = collect_source_rows(&table);
    for cell in &structure.cells {
      let legacy = legacy_get_cell_box(&table, cell);
      let indexed = source_rows
        .get(cell.source_row)
        .and_then(|row| row.children.get(cell.box_index));
      assert!(
        legacy.is_some(),
        "legacy lookup missing for cell idx {} (row {}, col {})",
        cell.index,
        cell.row,
        cell.col
      );
      assert!(
        indexed.is_some(),
        "indexed lookup missing for cell idx {} (source row {}, box idx {})",
        cell.index,
        cell.source_row,
        cell.box_index
      );
      let legacy = legacy.unwrap();
      let indexed = indexed.unwrap();
      assert!(
        std::ptr::eq(legacy, indexed),
        "lookup mismatch for cell idx {} (source row {}, box idx {}): legacy id {} vs indexed id {}",
        cell.index,
        cell.source_row,
        cell.box_index,
        legacy.id,
        indexed.id
      );
    }
  }

  #[test]
  fn source_row_lookup_respects_header_footer_reorder() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;

    let mut header_group_style = ComputedStyle::default();
    header_group_style.display = Display::TableHeaderGroup;
    let mut body_group_style = ComputedStyle::default();
    body_group_style.display = Display::TableRowGroup;
    let mut footer_group_style = ComputedStyle::default();
    footer_group_style.display = Display::TableFooterGroup;

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    let make_cell = || {
      BoxNode::new_block(
        Arc::new(cell_style.clone()),
        FormattingContextType::Block,
        vec![],
      )
    };

    let footer_row = BoxNode::new_block(
      Arc::new(row_style.clone()),
      FormattingContextType::Block,
      vec![make_cell()],
    );
    let body_row = BoxNode::new_block(
      Arc::new(row_style.clone()),
      FormattingContextType::Block,
      vec![make_cell(), make_cell()],
    );
    let header_row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![make_cell()],
    );

    let footer_group = BoxNode::new_block(
      Arc::new(footer_group_style),
      FormattingContextType::Block,
      vec![footer_row],
    );
    let body_group = BoxNode::new_block(
      Arc::new(body_group_style),
      FormattingContextType::Block,
      vec![body_row],
    );
    let header_group = BoxNode::new_block(
      Arc::new(header_group_style),
      FormattingContextType::Block,
      vec![header_row],
    );

    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![footer_group, body_group, header_group],
    );

    let structure = TableStructure::from_box_tree(&table);
    assert!(
      structure
        .cells
        .iter()
        .any(|cell| cell.source_row != cell.row),
      "row reordering should change cell row indices"
    );
    let source_rows = collect_source_rows(&table);
    for cell in &structure.cells {
      let legacy = legacy_get_cell_box(&table, cell).expect("legacy lookup");
      let indexed = source_rows
        .get(cell.source_row)
        .and_then(|row| row.children.get(cell.box_index))
        .expect("indexed lookup");
      assert!(
        std::ptr::eq(legacy, indexed),
        "reordered table lookup mismatch for cell {} (source row {}, box idx {})",
        cell.index,
        cell.source_row,
        cell.box_index
      );
    }
  }

  #[test]
  fn empty_cells_hide_keeps_column_min_width() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut hidden_cell_style = ComputedStyle::default();
    hidden_cell_style.display = Display::TableCell;
    hidden_cell_style.empty_cells = EmptyCells::Hide;
    hidden_cell_style.padding_left = Length::px(20.0);
    hidden_cell_style.padding_right = Length::px(20.0);

    let mut visible_cell_style = ComputedStyle::default();
    visible_cell_style.display = Display::TableCell;
    visible_cell_style.empty_cells = EmptyCells::Show;
    visible_cell_style.padding_left = Length::px(20.0);
    visible_cell_style.padding_right = Length::px(20.0);

    let hidden_cell = BoxNode::new_block(
      Arc::new(hidden_cell_style),
      FormattingContextType::Block,
      vec![],
    );
    let visible_cell = BoxNode::new_block(
      Arc::new(visible_cell_style),
      FormattingContextType::Block,
      vec![],
    );
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![hidden_cell, visible_cell],
    );
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let structure = TableStructure::from_box_tree(&table);
    let mut constraints: Vec<ColumnConstraints> = (0..structure.column_count)
      .map(|_| ColumnConstraints::new(0.0, 0.0))
      .collect();
    let tfc = TableFormattingContext::new();
    tfc.populate_column_constraints(
      &table,
      &structure,
      &mut constraints,
      DistributionMode::Auto,
      None,
      &StyleOverrideCache::default(),
    );

    assert!(
      (constraints[0].min_width - 40.0).abs() < 1.0,
      "hidden empty cell should still reflect padding in min width (only paint suppressed): {:?}",
      constraints[0]
    );
    assert!(
      (constraints[1].min_width - 40.0).abs() < 1.0,
      "visible cell should include padding in min width: {:?}",
      constraints[1]
    );
  }

  #[test]
  fn colspan_specified_width_increases_spanned_columns() {
    // First row has a single cell spanning two columns with a fixed width; it should raise the
    // combined minimum of the spanned columns.
    let mut span_style = ComputedStyle::default();
    span_style.display = Display::TableCell;
    span_style.width = Some(Length::px(200.0));
    let span_cell = BoxNode::new_block(Arc::new(span_style), FormattingContextType::Block, vec![])
      .with_debug_info(DebugInfo::new(Some("td".to_string()), None, vec![]).with_spans(2, 1));

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    let cell_a = BoxNode::new_block(
      Arc::new(cell_style.clone()),
      FormattingContextType::Block,
      vec![],
    );
    let cell_b = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;
    let row_with_span = BoxNode::new_block(
      Arc::new(row_style.clone()),
      FormattingContextType::Block,
      vec![span_cell],
    );
    let row_normal = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell_a, cell_b],
    );

    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row_with_span, row_normal],
    );

    let structure = TableStructure::from_box_tree(&table);
    let mut constraints: Vec<ColumnConstraints> = (0..structure.column_count)
      .map(|_| ColumnConstraints::new(0.0, 0.0))
      .collect();
    let tfc = TableFormattingContext::new();
    tfc.populate_column_constraints(
      &table,
      &structure,
      &mut constraints,
      DistributionMode::Auto,
      None,
      &StyleOverrideCache::default(),
    );

    let total_min: f32 = constraints.iter().map(|c| c.min_width).sum();
    assert!(
      total_min >= 200.0,
      "spanned cell width should contribute to column minima: {:?}",
      constraints.iter().map(|c| c.min_width).collect::<Vec<_>>()
    );
  }

  #[test]
  fn colspan_percentage_width_constrains_span_without_freezing_columns() {
    // Percentage widths on spanning cells constrain the total spanned width but should not turn
    // the individual columns into percentage columns (which would freeze them as non-flex).
    let mut span_style = ComputedStyle::default();
    span_style.display = Display::TableCell;
    span_style.width = Some(Length::percent(50.0));
    let span_cell = BoxNode::new_block(Arc::new(span_style), FormattingContextType::Block, vec![])
      .with_debug_info(DebugInfo::new(Some("td".to_string()), None, vec![]).with_spans(2, 1));

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    let cell_a = BoxNode::new_block(
      Arc::new(cell_style.clone()),
      FormattingContextType::Block,
      vec![],
    );
    let cell_b = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;
    let row_with_span = BoxNode::new_block(
      Arc::new(row_style.clone()),
      FormattingContextType::Block,
      vec![span_cell],
    );
    let row_normal = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell_a, cell_b],
    );

    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);
    table_style.width = Some(Length::px(200.0));
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row_with_span, row_normal],
    );

    let structure = TableStructure::from_box_tree(&table);
    let cell_count = structure.cells.len();
    let col_count = structure.column_count;
    let spans: Vec<(usize, usize)> = structure.cells.iter().map(|c| (c.col, c.colspan)).collect();
    let percent_base = Some(200.0);
    let mut constraints: Vec<ColumnConstraints> = (0..structure.column_count)
      .map(|_| ColumnConstraints::new(0.0, 0.0))
      .collect();
    let tfc = TableFormattingContext::new();
    let style_overrides = StyleOverrideCache::default();
    let source_rows = collect_source_rows(&table);
    let cell_bfc = BlockFormattingContext::with_font_context_and_viewport(
      tfc.factory.font_context().clone(),
      tfc.factory.viewport_size(),
    )
    .with_parallelism(tfc.parallelism);
    let span_box = source_rows
      .get(structure.cells[0].source_row)
      .and_then(|row| row.children.get(structure.cells[0].box_index))
      .expect("span cell");
    let (span_min, span_max) = tfc.measure_cell_intrinsic_widths(
      span_box,
      structure.border_collapse,
      percent_base,
      &cell_bfc,
      &style_overrides,
    );
    let resolved_width = span_box
      .style
      .width
      .as_ref()
      .and_then(|width| match width.unit {
        LengthUnit::Percent => percent_base.map(|base| {
          crate::layout::utils::clamp_with_order(
            (width.value / 100.0) * base,
            span_min,
            f32::INFINITY,
          )
        }),
        _ => Some(crate::layout::utils::clamp_with_order(
          width.to_px(),
          span_min,
          f32::INFINITY,
        )),
      });
    tfc.populate_column_constraints(
      &table,
      &structure,
      &mut constraints,
      DistributionMode::Auto,
      percent_base,
      &style_overrides,
    );

    let percent_sum: f32 = constraints.iter().filter_map(|c| c.percentage).sum();
    assert!(
            (percent_sum - 50.0).abs() < 0.01,
            "50% spanning cell should split its percentage across columns (got {percent_sum}); cells={cell_count} cols={col_count} spans={spans:?} intrinsic=({span_min:.2},{span_max:.2}) resolved_width={resolved_width:?} constraints: {constraints:?}"
        );
    let total_min: f32 = constraints.iter().map(|c| c.min_width).sum();
    assert!(
            total_min < 40.0,
            "percentage spans should not harden absolute minima (got {total_min:.2}); constraints: {constraints:?}"
        );
  }

  #[test]
  fn cell_min_width_raises_column_minimum() {
    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    cell_style.min_width = Some(Length::px(80.0));
    let cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell],
    );

    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let structure = TableStructure::from_box_tree(&table);
    let mut constraints: Vec<ColumnConstraints> = (0..structure.column_count)
      .map(|_| ColumnConstraints::new(0.0, 0.0))
      .collect();
    let tfc = TableFormattingContext::new();
    tfc.populate_column_constraints(
      &table,
      &structure,
      &mut constraints,
      DistributionMode::Auto,
      None,
      &StyleOverrideCache::default(),
    );

    let min = constraints.first().map(|c| c.min_width).unwrap_or(0.0);
    assert!(
      min >= 80.0,
      "cell min-width should clamp column minimum, got {:.2}",
      min
    );
  }

  #[test]
  fn cell_percentage_min_width_uses_definite_table_width() {
    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    cell_style.min_width = Some(Length::percent(50.0));
    let cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell],
    );

    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);
    table_style.width = Some(Length::px(200.0));
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let structure = TableStructure::from_box_tree(&table);
    let mut constraints: Vec<ColumnConstraints> = (0..structure.column_count)
      .map(|_| ColumnConstraints::new(0.0, 0.0))
      .collect();
    let tfc = TableFormattingContext::new();
    tfc.populate_column_constraints(
      &table,
      &structure,
      &mut constraints,
      DistributionMode::Auto,
      Some(200.0),
      &StyleOverrideCache::default(),
    );

    let min = constraints.first().map(|c| c.min_width).unwrap_or(0.0);
    assert!(
      (min - 100.0).abs() < 0.5,
      "50% min-width should resolve against definite 200px table width (got {:.2})",
      min
    );
  }

  #[test]
  fn cell_percentage_min_width_ignored_without_definite_base() {
    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    cell_style.min_width = Some(Length::percent(50.0));
    let cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell],
    );

    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let structure = TableStructure::from_box_tree(&table);
    let mut constraints: Vec<ColumnConstraints> = (0..structure.column_count)
      .map(|_| ColumnConstraints::new(0.0, 0.0))
      .collect();
    let tfc = TableFormattingContext::new();
    tfc.populate_column_constraints(
      &table,
      &structure,
      &mut constraints,
      DistributionMode::Auto,
      None,
      &StyleOverrideCache::default(),
    );

    let min = constraints.first().map(|c| c.min_width).unwrap_or(0.0);
    assert!(
      min < 1.0,
      "percentage min-width should be ignored without a definite base (got {:.2})",
      min
    );
  }

  #[test]
  fn nested_flex_and_grid_cells_affect_intrinsic_widths() {
    let mut flex_child_a_style = ComputedStyle::default();
    flex_child_a_style.display = Display::Block;
    flex_child_a_style.width = Some(Length::px(40.0));
    let flex_child_a = BoxNode::new_block(
      Arc::new(flex_child_a_style),
      FormattingContextType::Block,
      vec![],
    );
    let mut flex_child_b_style = ComputedStyle::default();
    flex_child_b_style.display = Display::Block;
    flex_child_b_style.width = Some(Length::px(60.0));
    let flex_child_b = BoxNode::new_block(
      Arc::new(flex_child_b_style),
      FormattingContextType::Block,
      vec![],
    );
    let mut flex_style = ComputedStyle::default();
    flex_style.display = Display::Flex;
    let flex_container = BoxNode::new_block(
      Arc::new(flex_style),
      FormattingContextType::Flex,
      vec![flex_child_a, flex_child_b],
    );

    let mut flex_cell_style = ComputedStyle::default();
    flex_cell_style.display = Display::TableCell;
    let flex_cell = BoxNode::new_block(
      Arc::new(flex_cell_style),
      FormattingContextType::Block,
      vec![flex_container],
    );

    let mut grid_item_a_style = ComputedStyle::default();
    grid_item_a_style.display = Display::Block;
    grid_item_a_style.width = Some(Length::px(70.0));
    let grid_item_a = BoxNode::new_block(
      Arc::new(grid_item_a_style),
      FormattingContextType::Block,
      vec![],
    );
    let mut grid_item_b_style = ComputedStyle::default();
    grid_item_b_style.display = Display::Block;
    grid_item_b_style.width = Some(Length::px(50.0));
    let grid_item_b = BoxNode::new_block(
      Arc::new(grid_item_b_style),
      FormattingContextType::Block,
      vec![],
    );
    let mut grid_style = ComputedStyle::default();
    grid_style.display = Display::Grid;
    grid_style.grid_template_columns = vec![
      GridTrack::Length(Length::px(70.0)),
      GridTrack::Length(Length::px(50.0)),
    ];
    let grid_container = BoxNode::new_block(
      Arc::new(grid_style),
      FormattingContextType::Grid,
      vec![grid_item_a, grid_item_b],
    );

    let mut grid_cell_style = ComputedStyle::default();
    grid_cell_style.display = Display::TableCell;
    let grid_cell = BoxNode::new_block(
      Arc::new(grid_cell_style),
      FormattingContextType::Block,
      vec![grid_container],
    );

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![flex_cell, grid_cell],
    );

    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let structure = TableStructure::from_box_tree(&table);
    let mut constraints: Vec<ColumnConstraints> = (0..structure.column_count)
      .map(|_| ColumnConstraints::new(0.0, 0.0))
      .collect();
    let style_overrides = StyleOverrideCache::default();
    let tfc = TableFormattingContext::new();
    tfc.populate_column_constraints(
      &table,
      &structure,
      &mut constraints,
      DistributionMode::Auto,
      None,
      &style_overrides,
    );

    let flex_expected = 100.0;
    let grid_expected = 120.0;
    assert!(
      (constraints[0].min_width - flex_expected).abs() < 0.5,
      "flex contents should set intrinsic column minimum (expected ~{flex_expected}, got {:?})",
      constraints
    );
    assert!(
      (constraints[1].min_width - grid_expected).abs() < 0.5,
      "grid contents should set intrinsic column minimum (expected ~{grid_expected}, got {:?})",
      constraints
    );
    assert!(
      constraints[0].max_width.is_finite()
        && constraints[1].max_width.is_finite()
        && constraints[0].max_width >= constraints[0].min_width
        && constraints[1].max_width >= constraints[1].min_width,
      "intrinsic measurement should yield finite per-column maxima: {:?}",
      constraints
    );
  }

  #[test]
  fn cell_max_width_caps_column_maximum() {
    let text_style = Arc::new(ComputedStyle::default());
    let text = BoxNode::new_text(text_style, "hi hi hi hi hi hi hi hi hi".to_string());

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    cell_style.max_width = Some(Length::px(40.0));
    let cell = BoxNode::new_block(
      Arc::new(cell_style),
      FormattingContextType::Block,
      vec![text],
    );

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell],
    );

    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let structure = TableStructure::from_box_tree(&table);
    let mut constraints: Vec<ColumnConstraints> = (0..structure.column_count)
      .map(|_| ColumnConstraints::new(0.0, 0.0))
      .collect();
    let tfc = TableFormattingContext::new();
    tfc.populate_column_constraints(
      &table,
      &structure,
      &mut constraints,
      DistributionMode::Auto,
      None,
      &StyleOverrideCache::default(),
    );

    let constraint = constraints
      .first()
      .cloned()
      .unwrap_or_else(|| ColumnConstraints::new(0.0, 0.0));
    assert!(
      constraint.max_width <= 40.1,
      "cell max-width should cap column maximum: {:?}",
      constraint
    );
    assert!(
      constraint.max_width + 0.01 >= constraint.min_width,
      "column maximum should remain ≥ minimum: {:?}",
      constraint
    );
  }

  #[test]
  fn column_min_width_clamps_column_constraints() {
    let mut col_style = ComputedStyle::default();
    col_style.display = Display::TableColumn;
    col_style.min_width = Some(Length::px(120.0));
    let col = BoxNode::new_block(Arc::new(col_style), FormattingContextType::Block, vec![]);

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    let cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell],
    );

    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![col.clone(), row.clone()],
    );

    let structure = TableStructure::from_box_tree(&table);
    let mut constraints: Vec<ColumnConstraints> = (0..structure.column_count)
      .map(|_| ColumnConstraints::new(0.0, 0.0))
      .collect();
    let tfc = TableFormattingContext::new();
    tfc.populate_column_constraints(
      &table,
      &structure,
      &mut constraints,
      DistributionMode::Auto,
      None,
      &StyleOverrideCache::default(),
    );

    let min = constraints.first().map(|c| c.min_width).unwrap_or(0.0);
    assert!(
      min >= 120.0,
      "column min-width should apply to constraints, got {:.2}",
      min
    );
  }

  #[test]
  fn column_max_width_caps_column_constraints() {
    let mut col_style = ComputedStyle::default();
    col_style.display = Display::TableColumn;
    col_style.max_width = Some(Length::px(40.0));
    let col = BoxNode::new_block(Arc::new(col_style), FormattingContextType::Block, vec![]);

    let mut wide_cell_style = ComputedStyle::default();
    wide_cell_style.display = Display::TableCell;
    let text_style = Arc::new(ComputedStyle::default());
    let text = BoxNode::new_text(
      text_style,
      "hi hi hi hi hi hi hi hi hi hi hi hi".to_string(),
    );
    let cell = BoxNode::new_block(
      Arc::new(wide_cell_style),
      FormattingContextType::Block,
      vec![text],
    );

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell],
    );

    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![col.clone(), row.clone()],
    );

    let structure = TableStructure::from_box_tree(&table);
    let mut constraints: Vec<ColumnConstraints> = (0..structure.column_count)
      .map(|_| ColumnConstraints::new(0.0, 0.0))
      .collect();
    let tfc = TableFormattingContext::new();
    tfc.populate_column_constraints(
      &table,
      &structure,
      &mut constraints,
      DistributionMode::Auto,
      None,
      &StyleOverrideCache::default(),
    );

    let constraint = constraints
      .first()
      .cloned()
      .unwrap_or_else(|| ColumnConstraints::new(0.0, 0.0));
    assert!(
      constraint.max_width <= 40.1,
      "column max-width should cap column maximum: {:?}",
      constraint
    );
    assert!(
      constraint.max_width + 0.01 >= constraint.min_width,
      "column maximum should remain ≥ minimum after cap: {:?}",
      constraint
    );
  }

  #[test]
  fn column_percent_min_width_uses_definite_table_width() {
    let mut col_style = ComputedStyle::default();
    col_style.display = Display::TableColumn;
    col_style.min_width = Some(Length::percent(50.0));
    let col = BoxNode::new_block(Arc::new(col_style), FormattingContextType::Block, vec![]);

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    let cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell],
    );

    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);
    table_style.width = Some(Length::px(200.0));
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![col.clone(), row.clone()],
    );

    let structure = TableStructure::from_box_tree(&table);
    let mut constraints: Vec<ColumnConstraints> = (0..structure.column_count)
      .map(|_| ColumnConstraints::new(0.0, 0.0))
      .collect();
    let tfc = TableFormattingContext::new();
    tfc.populate_column_constraints(
      &table,
      &structure,
      &mut constraints,
      DistributionMode::Auto,
      Some(200.0),
      &StyleOverrideCache::default(),
    );

    let min = constraints.first().map(|c| c.min_width).unwrap_or(0.0);
    assert!(
      (min - 100.0).abs() < 0.5,
      "percent min-width should resolve against definite table width, got {:.2}",
      min
    );
  }

  #[test]
  fn column_percent_min_width_skipped_when_table_width_indefinite() {
    let mut col_style = ComputedStyle::default();
    col_style.display = Display::TableColumn;
    col_style.min_width = Some(Length::percent(50.0));
    let col = BoxNode::new_block(Arc::new(col_style), FormattingContextType::Block, vec![]);

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    let cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell],
    );

    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![col.clone(), row.clone()],
    );

    let structure = TableStructure::from_box_tree(&table);
    let mut constraints: Vec<ColumnConstraints> = (0..structure.column_count)
      .map(|_| ColumnConstraints::new(0.0, 0.0))
      .collect();
    let tfc = TableFormattingContext::new();
    // percent_base None to mimic auto table width
    tfc.populate_column_constraints(
      &table,
      &structure,
      &mut constraints,
      DistributionMode::Auto,
      None,
      &StyleOverrideCache::default(),
    );

    let min = constraints.first().map(|c| c.min_width).unwrap_or(0.0);
    assert!(
      min < 50.0,
      "percent min-width should be treated as auto when table width is indefinite, got {:.2}",
      min
    );
  }

  #[test]
  fn cell_percent_padding_counts_when_table_width_definite() {
    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    cell_style.padding_left = Length::percent(10.0);
    cell_style.padding_right = Length::percent(10.0);
    let cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell],
    );

    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.width = Some(Length::px(200.0));
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let structure = TableStructure::from_box_tree(&table);
    let mut constraints: Vec<ColumnConstraints> = (0..structure.column_count)
      .map(|_| ColumnConstraints::new(0.0, 0.0))
      .collect();
    let tfc = TableFormattingContext::new();
    tfc.populate_column_constraints(
      &table,
      &structure,
      &mut constraints,
      DistributionMode::Auto,
      Some(200.0),
      &StyleOverrideCache::default(),
    );

    let col = constraints.first().expect("column constraints");
    assert!(
      (col.min_width - 40.0).abs() < 0.5,
      "10% padding on each side should add 40px to intrinsic min width (got {:.2})",
      col.min_width
    );
    assert!(
      (col.max_width - 40.0).abs() < 0.5,
      "10% padding on each side should add 40px to intrinsic max width (got {:.2})",
      col.max_width
    );
  }

  #[test]
  fn cell_percent_padding_is_ignored_when_table_width_indefinite() {
    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    cell_style.padding_left = Length::percent(10.0);
    cell_style.padding_right = Length::percent(10.0);
    let cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell],
    );

    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let structure = TableStructure::from_box_tree(&table);
    let mut constraints: Vec<ColumnConstraints> = (0..structure.column_count)
      .map(|_| ColumnConstraints::new(0.0, 0.0))
      .collect();
    let tfc = TableFormattingContext::new();
    tfc.populate_column_constraints(
      &table,
      &structure,
      &mut constraints,
      DistributionMode::Auto,
      None,
      &StyleOverrideCache::default(),
    );

    let col = constraints.first().expect("column constraints");
    assert!(
      col.min_width < 0.5,
      "percent padding should be treated as auto with no width base (min={:.2})",
      col.min_width
    );
    assert!(
      col.max_width < 0.5,
      "percent padding should be treated as auto with no width base (max={:.2})",
      col.max_width
    );
  }

  #[test]
  fn large_table_structure_preserves_grid_with_spans() {
    let rows = 30;
    let cols = 15;
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);

    let mut table_rows = Vec::new();
    for r in 0..rows {
      let mut cells = Vec::new();
      if r % 3 == 0 {
        cells.push(table_cell_with_span(2, 2));
        for _ in 0..(cols - 2) {
          cells.push(table_cell_with_span(1, 1));
        }
      } else if r % 3 == 1 {
        for _ in 0..(cols - 2) {
          cells.push(table_cell_with_span(1, 1));
        }
      } else {
        cells.push(table_cell_with_span(3, 1));
        for _ in 0..(cols - 3) {
          cells.push(table_cell_with_span(1, 1));
        }
      }
      table_rows.push(table_row_with_visibility(cells, Visibility::Visible));
    }

    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      table_rows,
    )
    .with_debug_info(DebugInfo::new(Some("table".to_string()), None, vec![]));

    let structure = TableStructure::from_box_tree(&table);
    assert_eq!(structure.row_count, rows);
    assert_eq!(structure.column_count, cols);
    assert_eq!(structure.grid.len(), rows);
    assert_eq!(structure.grid[0].len(), cols);

    let spanning = structure
      .get_cell_at(1, 0)
      .expect("row span should occupy first column in second row");
    assert_eq!(spanning.source_row, 0);

    let row_one_first = structure
      .cells
      .iter()
      .find(|c| c.source_row == 1 && c.box_index == 0)
      .expect("row after span should still place its first cell");
    assert_eq!(
      row_one_first.col, 2,
      "row following a 2-column span should start after the occupied columns"
    );
  }

  #[test]
  fn collapsed_border_hidden_suppresses_other_styles() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_collapse = BorderCollapse::Collapse;

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;
    row_style.border_top_style = BorderStyle::Hidden;
    row_style.border_top_width = Length::px(10.0);

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    cell_style.border_top_style = BorderStyle::Solid;
    cell_style.border_top_width = Length::px(4.0);

    let cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell],
    );
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let structure = TableStructure::from_box_tree(&table);
    let borders = compute_collapsed_borders(&table, &structure);

    assert_eq!(borders.horizontal.len(), 2);
    assert!((borders.horizontal[0][0].width - 0.0).abs() < f32::EPSILON);
  }

  #[test]
  fn collapsed_border_width_beats_style() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_collapse = BorderCollapse::Collapse;

    let mut cell_left = ComputedStyle::default();
    cell_left.display = Display::TableCell;
    cell_left.border_right_style = BorderStyle::Solid;
    cell_left.border_right_width = Length::px(8.0);

    let mut cell_right = ComputedStyle::default();
    cell_right.display = Display::TableCell;
    cell_right.border_left_style = BorderStyle::Double;
    cell_right.border_left_width = Length::px(2.0);

    let left = BoxNode::new_block(Arc::new(cell_left), FormattingContextType::Block, vec![]);
    let right = BoxNode::new_block(Arc::new(cell_right), FormattingContextType::Block, vec![]);
    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![left, right],
    );
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let structure = TableStructure::from_box_tree(&table);
    let borders = compute_collapsed_borders(&table, &structure);

    assert_eq!(borders.vertical.len(), 3);
    // Style wins over width: double outranks solid even though it is thinner.
    // Double borders are clamped to a minimum of 3px so they can render as three strokes.
    let winning = &borders.vertical[1][0];
    assert!(winning.width >= 3.0);
    assert_eq!(winning.style, BorderStyle::Double);
  }

  #[test]
  fn collapsed_borders_prefer_cell_over_row_on_equal_style() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_collapse = BorderCollapse::Collapse;

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;
    row_style.border_top_style = BorderStyle::Solid;
    row_style.border_top_width = Length::px(4.0);
    row_style.border_top_color = Rgba::from_rgba8(0, 0, 0, 255);

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    cell_style.border_top_style = BorderStyle::Solid;
    cell_style.border_top_width = Length::px(4.0);
    cell_style.border_top_color = Rgba::from_rgba8(0, 0, 255, 255);

    let cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell],
    );
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let structure = TableStructure::from_box_tree(&table);
    let borders = compute_collapsed_borders(&table, &structure);

    assert_eq!(borders.horizontal.len(), 2);
    let top_border = &borders.horizontal[0][0];
    assert_eq!(top_border.style, BorderStyle::Solid);
    assert_eq!(top_border.color, Rgba::from_rgba8(0, 0, 255, 255));
    assert!((top_border.width - 4.0).abs() < f32::EPSILON);
  }

  #[test]
  fn collapsed_borders_tie_prefers_start_cell_in_ltr() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_collapse = BorderCollapse::Collapse;

    let mut left_cell_style = ComputedStyle::default();
    left_cell_style.display = Display::TableCell;
    left_cell_style.border_right_style = BorderStyle::Solid;
    left_cell_style.border_right_width = Length::px(3.0);
    left_cell_style.border_right_color = Rgba::from_rgba8(255, 0, 0, 255);

    let mut right_cell_style = ComputedStyle::default();
    right_cell_style.display = Display::TableCell;
    right_cell_style.border_left_style = BorderStyle::Solid;
    right_cell_style.border_left_width = Length::px(3.0);
    right_cell_style.border_left_color = Rgba::from_rgba8(0, 255, 0, 255);

    let left = BoxNode::new_block(
      Arc::new(left_cell_style),
      FormattingContextType::Block,
      vec![],
    );
    let right = BoxNode::new_block(
      Arc::new(right_cell_style),
      FormattingContextType::Block,
      vec![],
    );
    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![left, right],
    );
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let structure = TableStructure::from_box_tree(&table);
    let borders = compute_collapsed_borders(&table, &structure);

    let middle_border = &borders.vertical[1][0];
    assert_eq!(middle_border.color, Rgba::from_rgba8(255, 0, 0, 255));
  }

  #[test]
  fn collapsed_borders_tie_prefers_start_cell_in_rtl() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_collapse = BorderCollapse::Collapse;
    table_style.direction = Direction::Rtl;

    let mut left_cell_style = ComputedStyle::default();
    left_cell_style.display = Display::TableCell;
    left_cell_style.border_right_style = BorderStyle::Solid;
    left_cell_style.border_right_width = Length::px(3.0);
    left_cell_style.border_right_color = Rgba::from_rgba8(255, 0, 0, 255);

    let mut right_cell_style = ComputedStyle::default();
    right_cell_style.display = Display::TableCell;
    right_cell_style.border_left_style = BorderStyle::Solid;
    right_cell_style.border_left_width = Length::px(3.0);
    right_cell_style.border_left_color = Rgba::from_rgba8(0, 255, 0, 255);

    let left = BoxNode::new_block(
      Arc::new(left_cell_style),
      FormattingContextType::Block,
      vec![],
    );
    let right = BoxNode::new_block(
      Arc::new(right_cell_style),
      FormattingContextType::Block,
      vec![],
    );
    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![left, right],
    );
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let structure = TableStructure::from_box_tree(&table);
    let borders = compute_collapsed_borders(&table, &structure);

    let middle_border = &borders.vertical[1][0];
    // Start edge in RTL is right cell (green).
    assert_eq!(middle_border.color, Rgba::from_rgba8(0, 255, 0, 255));
  }

  #[test]
  fn collapsed_borders_ignore_empty_cells_property() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_collapse = BorderCollapse::Collapse;

    let mut left_cell_style = ComputedStyle::default();
    left_cell_style.display = Display::TableCell;
    left_cell_style.border_right_style = BorderStyle::Solid;
    left_cell_style.border_right_width = Length::px(4.0);
    left_cell_style.empty_cells = EmptyCells::Hide;

    let mut right_cell_style = ComputedStyle::default();
    right_cell_style.display = Display::TableCell;
    right_cell_style.border_left_style = BorderStyle::Solid;
    right_cell_style.border_left_width = Length::px(2.0);

    let left = BoxNode::new_block(
      Arc::new(left_cell_style),
      FormattingContextType::Block,
      vec![],
    );
    let right = BoxNode::new_block(
      Arc::new(right_cell_style),
      FormattingContextType::Block,
      vec![],
    );
    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![left, right],
    );
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let structure = TableStructure::from_box_tree(&table);
    let borders = compute_collapsed_borders(&table, &structure);

    assert_eq!(borders.vertical.len(), 3);
    let middle = &borders.vertical[1][0];
    assert!(
      (middle.width - 4.0).abs() < f32::EPSILON,
      "empty-cells is ignored when borders are collapsed"
    );
  }

  #[test]
  fn collapsed_borders_prefer_style_over_width() {
    // Dotted 5px vs solid 1px: solid wins due to style priority, even though dotted is wider.
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_collapse = BorderCollapse::Collapse;

    let mut left_cell_style = ComputedStyle::default();
    left_cell_style.display = Display::TableCell;
    left_cell_style.border_right_style = BorderStyle::Dotted;
    left_cell_style.border_right_width = Length::px(5.0);
    left_cell_style.border_right_color = Rgba::from_rgba8(200, 0, 0, 255);

    let mut right_cell_style = ComputedStyle::default();
    right_cell_style.display = Display::TableCell;
    right_cell_style.border_left_style = BorderStyle::Solid;
    right_cell_style.border_left_width = Length::px(1.0);
    right_cell_style.border_left_color = Rgba::from_rgba8(0, 0, 200, 255);

    let left = BoxNode::new_block(
      Arc::new(left_cell_style),
      FormattingContextType::Block,
      vec![],
    );
    let right = BoxNode::new_block(
      Arc::new(right_cell_style),
      FormattingContextType::Block,
      vec![],
    );
    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![left, right],
    );
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let structure = TableStructure::from_box_tree(&table);
    let borders = compute_collapsed_borders(&table, &structure);
    let border = borders
      .vertical
      .get(1)
      .and_then(|col| col.first())
      .copied()
      .unwrap_or_else(ResolvedBorder::none);
    assert_eq!(
      border.style,
      BorderStyle::Solid,
      "higher-priority style should win"
    );
    assert!(
      (border.width - 1.0).abs() < 0.01,
      "winning border keeps its own width"
    );
    assert_eq!(border.color, Rgba::from_rgba8(0, 0, 200, 255));
  }

  #[test]
  fn collapsed_borders_dimensions_scale_for_larger_tables() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_collapse = BorderCollapse::Collapse;

    let mut rows = Vec::new();
    for _ in 0..20 {
      let mut cells = Vec::new();
      for _ in 0..20 {
        let mut cell_style = ComputedStyle::default();
        cell_style.display = Display::TableCell;
        cells.push(BoxNode::new_block(
          Arc::new(cell_style),
          FormattingContextType::Block,
          Vec::new(),
        ));
      }

      let mut row_style = ComputedStyle::default();
      row_style.display = Display::TableRow;
      rows.push(BoxNode::new_block(
        Arc::new(row_style),
        FormattingContextType::Block,
        cells,
      ));
    }

    let table = BoxNode::new_block(Arc::new(table_style), FormattingContextType::Table, rows);
    let structure = TableStructure::from_box_tree(&table);
    let borders = compute_collapsed_borders(&table, &structure);

    assert_eq!(structure.row_count, 20);
    assert_eq!(structure.column_count, 20);
    assert_eq!(borders.vertical.len(), structure.column_count + 1);
    assert!(borders
      .vertical
      .iter()
      .all(|col| col.len() == structure.row_count));
    assert_eq!(borders.horizontal.len(), structure.row_count + 1);
    assert!(borders
      .horizontal
      .iter()
      .all(|row| row.len() == structure.column_count));
    assert_eq!(borders.corners.len(), structure.row_count + 1);
    assert!(borders
      .corners
      .iter()
      .all(|row| row.len() == structure.column_count + 1));
    assert!(
      borders
        .vertical
        .iter()
        .flat_map(|col| col.iter())
        .all(
          |border| border.width.abs() < f32::EPSILON && matches!(border.style, BorderStyle::None)
        ),
      "default styles should resolve to no visible borders"
    );
  }

  fn find_cell_fragment<'a>(fragment: &'a FragmentNode) -> Option<&'a FragmentNode> {
    if fragment
      .style
      .as_ref()
      .map(|s| s.display == Display::TableCell)
      .unwrap_or(false)
    {
      return Some(fragment);
    }
    for child in fragment.children.iter() {
      if let Some(found) = find_cell_fragment(child) {
        return Some(found);
      }
    }
    None
  }

  #[test]
  fn row_backgrounds_are_emitted_before_cells() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;
    row_style.background_color = Rgba::from_rgba8(200, 0, 0, 255);

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    let cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell],
    );
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let fc = TableFormattingContext::with_factory(FormattingContextFactory::new());
    let fragment = fc
      .layout(
        &table,
        &LayoutConstraints::new(AvailableSpace::Definite(100.0), AvailableSpace::Indefinite),
      )
      .expect("layout");

    assert!(
      !fragment.children.is_empty(),
      "table fragment should contain row background and cell fragments"
    );
    let row_frag = &fragment.children[0];
    let row_color = row_frag
      .style
      .as_ref()
      .map(|s| s.background_color)
      .expect("row fragment has style");
    assert_eq!(row_color, Rgba::from_rgba8(200, 0, 0, 255));
    let cell_frag = find_cell_fragment(&fragment).expect("cell fragment");
    assert!(row_frag.bounds.y() <= cell_frag.bounds.y());
  }

  #[test]
  fn row_backgrounds_reuse_stripped_styles() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);

    let row_color = Rgba::from_rgba8(10, 20, 30, 255);
    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;
    row_style.background_color = row_color;
    let shared_row_style = Arc::new(row_style);

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    let cell_style = Arc::new(cell_style);

    let mut rows = Vec::new();
    for _ in 0..3 {
      let cell = BoxNode::new_block(cell_style.clone(), FormattingContextType::Block, vec![]);
      rows.push(BoxNode::new_block(
        shared_row_style.clone(),
        FormattingContextType::Block,
        vec![cell],
      ));
    }

    let table = BoxNode::new_block(Arc::new(table_style), FormattingContextType::Table, rows);

    let fc = TableFormattingContext::with_factory(FormattingContextFactory::new());
    let fragment = fc
      .layout(
        &table,
        &LayoutConstraints::new(AvailableSpace::Definite(200.0), AvailableSpace::Indefinite),
      )
      .expect("layout");

    let row_background_styles: Vec<Arc<ComputedStyle>> = fragment
      .children
      .iter()
      .filter_map(|child| {
        child.style.as_ref().and_then(|style| {
          if matches!(style.display, Display::TableRow) && style.background_color == row_color {
            Some(style.clone())
          } else {
            None
          }
        })
      })
      .collect();

    assert_eq!(row_background_styles.len(), 3);
    assert!(
      row_background_styles
        .windows(2)
        .all(|pair| Arc::ptr_eq(&pair[0], &pair[1])),
      "row background styles should reuse stripped clones when input styles share the same Arc"
    );
  }

  #[test]

  fn collapsed_table_padding_offsets_content_and_preserves_total_height() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_collapse = BorderCollapse::Collapse;
    table_style.padding_top = Length::px(10.0);
    table_style.padding_bottom = Length::px(10.0);
    table_style.padding_left = Length::px(5.0);
    table_style.padding_right = Length::px(5.0);
    table_style.height = Some(Length::px(100.0));

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    let cell = BoxNode::new_block(
      Arc::new(cell_style),
      FormattingContextType::Block,
      vec![BoxNode::new_text(
        Arc::new(ComputedStyle::default()),
        "data".to_string(),
      )],
    );
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell],
    );
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let fc = TableFormattingContext::with_factory(FormattingContextFactory::new());
    let fragment = fc
      .layout(
        &table,
        &LayoutConstraints::new(
          AvailableSpace::Definite(200.0),
          AvailableSpace::Definite(100.0),
        ),
      )
      .expect("layout");

    // Total height honors the specified height even though padding is ignored in collapsed model.
    assert!((fragment.bounds.height() - 100.0).abs() < 0.01);

    // In the collapsed border model the table has no padding; content starts at the top.
    let cell_frag = find_cell_fragment(&fragment).expect("cell fragment");
    assert!(cell_frag.bounds.y().abs() < 0.1);
  }

  #[test]
  fn column_and_colgroup_backgrounds_paint_before_rows() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);

    let mut colgroup_style = ComputedStyle::default();
    colgroup_style.display = Display::TableColumnGroup;
    colgroup_style.background_color = Rgba::from_rgba8(255, 0, 0, 255);
    colgroup_style.border_top_width = Length::px(3.0);
    colgroup_style.border_top_style = BorderStyle::Solid;

    let mut col_style = ComputedStyle::default();
    col_style.display = Display::TableColumn;
    col_style.background_color = Rgba::from_rgba8(0, 0, 255, 255);
    col_style.border_left_width = Length::px(2.0);
    col_style.border_left_style = BorderStyle::Solid;

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;
    row_style.background_color = Rgba::from_rgba8(0, 200, 0, 255);

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    let mut child_style = ComputedStyle::default();
    child_style.display = Display::Block;
    child_style.height = Some(Length::px(10.0));
    let child = BoxNode::new_block(Arc::new(child_style), FormattingContextType::Block, vec![]);

    let cell1 = BoxNode::new_block(
      Arc::new(cell_style.clone()),
      FormattingContextType::Block,
      vec![child.clone()],
    );
    let cell2 = BoxNode::new_block(
      Arc::new(cell_style),
      FormattingContextType::Block,
      vec![child],
    );
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell1, cell2],
    );

    let col = BoxNode::new_block(Arc::new(col_style), FormattingContextType::Block, vec![]);
    let colgroup = BoxNode::new_block(
      Arc::new(colgroup_style),
      FormattingContextType::Block,
      vec![col],
    );

    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![colgroup, row],
    );

    let fc = TableFormattingContext::with_factory(FormattingContextFactory::new());
    let fragment = fc
      .layout(
        &table,
        &LayoutConstraints::new(AvailableSpace::Definite(200.0), AvailableSpace::Indefinite),
      )
      .expect("layout");

    let colors: Vec<Rgba> = fragment
      .children
      .iter()
      .filter_map(|f| f.style.as_ref().map(|s| s.background_color))
      .collect();
    assert!(
      colors.len() >= 2,
      "column group and column backgrounds should be present before cells"
    );
    assert_eq!(colors[0], Rgba::from_rgba8(255, 0, 0, 255));
    assert_eq!(colors[1], Rgba::from_rgba8(0, 0, 255, 255));
    assert!(
      colors.contains(&Rgba::from_rgba8(0, 200, 0, 255)),
      "row background should still be painted"
    );
  }

  #[test]
  fn empty_cells_hide_preserves_border_widths_in_separate_model() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    cell_style.empty_cells = EmptyCells::Hide;
    cell_style.border_top_width = Length::px(3.0);
    cell_style.border_right_width = Length::px(3.0);
    cell_style.border_bottom_width = Length::px(3.0);
    cell_style.border_left_width = Length::px(3.0);
    cell_style.border_top_style = BorderStyle::Solid;
    cell_style.border_right_style = BorderStyle::Solid;
    cell_style.border_bottom_style = BorderStyle::Solid;
    cell_style.border_left_style = BorderStyle::Solid;

    let cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell],
    );
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let fc = TableFormattingContext::with_factory(FormattingContextFactory::new());
    let fragment = fc
      .layout(
        &table,
        &LayoutConstraints::new(AvailableSpace::Definite(200.0), AvailableSpace::Indefinite),
      )
      .expect("layout");

    let cell_frag = find_cell_fragment(&fragment).expect("cell fragment");
    let style = cell_frag.style.as_ref().expect("style");
    assert!((style.border_top_width.to_px() - 3.0).abs() < 0.1);
    assert!((style.border_right_width.to_px() - 3.0).abs() < 0.1);
    assert!((style.border_bottom_width.to_px() - 3.0).abs() < 0.1);
    assert!((style.border_left_width.to_px() - 3.0).abs() < 0.1);
    assert_eq!(style.border_top_color, Rgba::TRANSPARENT);
    assert_eq!(style.border_bottom_color, Rgba::TRANSPARENT);
    assert_eq!(style.border_left_color, Rgba::TRANSPARENT);
    assert_eq!(style.border_right_color, Rgba::TRANSPARENT);
  }

  #[test]
  fn intrinsic_widths_add_padding_once() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    cell_style.padding_left = Length::px(10.0);
    cell_style.padding_right = Length::px(10.0);

    let mut child_style = ComputedStyle::default();
    child_style.display = Display::Block;
    child_style.width = Some(Length::px(50.0));
    let child = BoxNode::new_block(Arc::new(child_style), FormattingContextType::Block, vec![]);

    let cell = BoxNode::new_block(
      Arc::new(cell_style),
      FormattingContextType::Block,
      vec![child],
    );
    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell],
    );
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let structure = TableStructure::from_box_tree(&table);
    let mut constraints: Vec<ColumnConstraints> = (0..structure.column_count)
      .map(|_| ColumnConstraints::new(0.0, 0.0))
      .collect();
    let tfc = TableFormattingContext::new();
    tfc.populate_column_constraints(
      &table,
      &structure,
      &mut constraints,
      DistributionMode::Auto,
      None,
      &StyleOverrideCache::default(),
    );

    assert_eq!(constraints.len(), 1);
    let col = &constraints[0];
    // 50 content + 10+10 padding
    assert!(
      (col.min_width - 70.0).abs() < 0.5,
      "min width should include padding only once"
    );
  }

  #[test]
  fn collapsed_borders_choose_higher_style_on_equal_widths() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_collapse = BorderCollapse::Collapse;

    let mut left_cell_style = ComputedStyle::default();
    left_cell_style.display = Display::TableCell;
    left_cell_style.border_right_style = BorderStyle::Double;
    left_cell_style.border_right_width = Length::px(3.0);
    left_cell_style.border_right_color = Rgba::from_rgba8(255, 0, 0, 255);

    let mut right_cell_style = ComputedStyle::default();
    right_cell_style.display = Display::TableCell;
    right_cell_style.border_left_style = BorderStyle::Dashed;
    right_cell_style.border_left_width = Length::px(3.0);
    right_cell_style.border_left_color = Rgba::from_rgba8(0, 255, 0, 255);

    let left = BoxNode::new_block(
      Arc::new(left_cell_style),
      FormattingContextType::Block,
      vec![],
    );
    let right = BoxNode::new_block(
      Arc::new(right_cell_style),
      FormattingContextType::Block,
      vec![],
    );
    let row = BoxNode::new_block(
      Arc::new(ComputedStyle {
        display: Display::TableRow,
        ..ComputedStyle::default()
      }),
      FormattingContextType::Block,
      vec![left, right],
    );
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let structure = TableStructure::from_box_tree(&table);
    let borders = compute_collapsed_borders(&table, &structure);

    let middle_border = &borders.vertical[1][0];
    assert_eq!(middle_border.style, BorderStyle::Double);
    assert!((middle_border.width - 3.0).abs() < f32::EPSILON);
    assert_eq!(middle_border.color, Rgba::from_rgba8(255, 0, 0, 255));
  }

  #[test]
  fn collapsed_line_positions_include_half_outer_borders() {
    let sizes = vec![100.0, 50.0];
    let line_widths = vec![10.0, 4.0, 6.0];
    let (line_pos, offsets, extent) = collapsed_line_positions(&sizes, &line_widths, 0.0, 0.0);

    assert_eq!(line_pos.len(), 3);
    assert_eq!(offsets.len(), 2);
    assert!((line_pos[0] - 0.0).abs() < 1e-6);
    assert!(
      (offsets[0] - 5.0).abs() < 1e-6,
      "first column should start after half the left border"
    );

    let expected_second_line = 5.0 + 100.0 + 2.0;
    assert!((line_pos[1] - expected_second_line).abs() < 1e-6);
    assert!(
      (offsets[1] - (line_pos[1] + line_widths[1] * 0.5)).abs() < 1e-6,
      "second column should start after half of the interior line"
    );

    let expected_extent = expected_second_line + 2.0 + 50.0 + 3.0;
    assert!((line_pos[2] - expected_extent).abs() < 1e-6);
    assert!((extent - expected_extent).abs() < 1e-6);
  }

  #[test]
  fn collapsed_segments_trim_to_corner_winners() {
    let row_line_pos = [0.0, 30.0];
    let corner_top = ResolvedBorder {
      width: 10.0,
      style: BorderStyle::Solid,
      color: Rgba::BLACK,
    };
    let corner_bottom = ResolvedBorder {
      width: 6.0,
      style: BorderStyle::Solid,
      color: Rgba::BLACK,
    };

    let y_start = row_line_pos[0] + corner_top.width * 0.5;
    let y_end = row_line_pos[1] - corner_bottom.width * 0.5;
    assert_eq!(y_start, 5.0);
    assert_eq!(y_end, 27.0);
    assert_eq!(y_end - y_start, 22.0);
  }

  #[test]
  fn collapsed_borders_tie_on_horizontal_border_prefers_top() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_collapse = BorderCollapse::Collapse;

    let mut top_cell_style = ComputedStyle::default();
    top_cell_style.display = Display::TableCell;
    top_cell_style.border_bottom_style = BorderStyle::Solid;
    top_cell_style.border_bottom_width = Length::px(4.0);
    top_cell_style.border_bottom_color = Rgba::from_rgba8(255, 0, 0, 255);

    let mut bottom_cell_style = ComputedStyle::default();
    bottom_cell_style.display = Display::TableCell;
    bottom_cell_style.border_top_style = BorderStyle::Solid;
    bottom_cell_style.border_top_width = Length::px(4.0);
    bottom_cell_style.border_top_color = Rgba::from_rgba8(0, 0, 255, 255);

    let top_row = BoxNode::new_block(
      Arc::new(ComputedStyle {
        display: Display::TableRow,
        ..ComputedStyle::default()
      }),
      FormattingContextType::Block,
      vec![BoxNode::new_block(
        Arc::new(top_cell_style),
        FormattingContextType::Block,
        vec![],
      )],
    );

    let bottom_row = BoxNode::new_block(
      Arc::new(ComputedStyle {
        display: Display::TableRow,
        ..ComputedStyle::default()
      }),
      FormattingContextType::Block,
      vec![BoxNode::new_block(
        Arc::new(bottom_cell_style),
        FormattingContextType::Block,
        vec![],
      )],
    );

    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![top_row, bottom_row],
    );

    let structure = TableStructure::from_box_tree(&table);
    let borders = compute_collapsed_borders(&table, &structure);

    let horizontal_border = &borders.horizontal[1][0];
    assert_eq!(horizontal_border.style, BorderStyle::Solid);
    assert!((horizontal_border.width - 4.0).abs() < f32::EPSILON);
    // Top cell should win when all else ties.
    assert_eq!(horizontal_border.color, Rgba::from_rgba8(255, 0, 0, 255));
  }

  #[test]
  fn collapsed_borders_prefer_wider_even_if_style_lower() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_collapse = BorderCollapse::Collapse;

    let mut left_cell_style = ComputedStyle::default();
    left_cell_style.display = Display::TableCell;
    left_cell_style.border_right_style = BorderStyle::Double; // higher style rank but slightly thinner
    left_cell_style.border_right_width = Length::px(3.0);
    left_cell_style.border_right_color = Rgba::from_rgba8(255, 0, 0, 255);

    let mut right_cell_style = ComputedStyle::default();
    right_cell_style.display = Display::TableCell;
    right_cell_style.border_left_style = BorderStyle::Solid; // lower style rank but wider
    right_cell_style.border_left_width = Length::px(3.005);
    right_cell_style.border_left_color = Rgba::from_rgba8(0, 0, 255, 255);

    let left = BoxNode::new_block(
      Arc::new(left_cell_style),
      FormattingContextType::Block,
      vec![],
    );
    let right = BoxNode::new_block(
      Arc::new(right_cell_style),
      FormattingContextType::Block,
      vec![],
    );
    let row = BoxNode::new_block(
      Arc::new(ComputedStyle {
        display: Display::TableRow,
        ..ComputedStyle::default()
      }),
      FormattingContextType::Block,
      vec![left, right],
    );
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let structure = TableStructure::from_box_tree(&table);
    let borders = compute_collapsed_borders(&table, &structure);

    let middle_border = &borders.vertical[1][0];
    // Style precedence wins: double outranks solid even when slightly thinner.
    assert_eq!(middle_border.style, BorderStyle::Double);
    assert!((middle_border.width - 3.0).abs() < 0.0001);
    assert_eq!(middle_border.color, Rgba::from_rgba8(255, 0, 0, 255));
  }

  #[test]
  fn percentage_columns_apply_with_definite_available_width() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);

    let mut left_style = ComputedStyle::default();
    left_style.display = Display::TableCell;
    left_style.width = Some(Length::new(80.0, LengthUnit::Percent));

    let mut right_style = ComputedStyle::default();
    right_style.display = Display::TableCell;

    let left = BoxNode::new_block(Arc::new(left_style), FormattingContextType::Block, vec![]);
    let right = BoxNode::new_block(Arc::new(right_style), FormattingContextType::Block, vec![]);
    let row = BoxNode::new_block(
      Arc::new(ComputedStyle {
        display: Display::TableRow,
        ..ComputedStyle::default()
      }),
      FormattingContextType::Block,
      vec![left, right],
    );
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let fc = TableFormattingContext::new();
    let fragment = fc
      .layout(
        &table,
        &LayoutConstraints::new(AvailableSpace::Definite(100.0), AvailableSpace::Indefinite),
      )
      .expect("layout");

    fn collect_cell_widths(node: &FragmentNode, widths: &mut Vec<f32>) {
      if node
        .style
        .as_ref()
        .map(|s| s.display == Display::TableCell)
        .unwrap_or(false)
      {
        widths.push(node.bounds.width());
      }
      for child in node.children.iter() {
        collect_cell_widths(child, widths);
      }
    }

    let mut widths = Vec::new();
    collect_cell_widths(&fragment, &mut widths);
    assert_eq!(widths.len(), 2);
    // Percent width should consume 80% of the available space when table width resolves from containing block.
    assert!(
      (79.0..81.5).contains(&widths[0]),
      "expected ~80% width, got {:?}",
      widths
    );
    assert!((18.0..22.0).contains(&widths[1]));
  }

  #[test]
  fn test_fixed_layout_uses_first_row_only() {
    // First row specifies widths; second row should not affect fixed layout distribution.
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.table_layout = TableLayout::Fixed;

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut cell1_style = ComputedStyle::default();
    cell1_style.display = Display::TableCell;
    cell1_style.width = Some(Length::percent(50.0));
    let cell1 = BoxNode::new_block(Arc::new(cell1_style), FormattingContextType::Block, vec![]);

    let mut cell2_style = ComputedStyle::default();
    cell2_style.display = Display::TableCell;
    let cell2 = BoxNode::new_block(Arc::new(cell2_style), FormattingContextType::Block, vec![]);

    let first_row = BoxNode::new_block(
      Arc::new(row_style.clone()),
      FormattingContextType::Block,
      vec![cell1, cell2],
    );

    // Second row with explicit widths should be ignored in fixed layout sizing.
    let mut second_cell_style = ComputedStyle::default();
    second_cell_style.display = Display::TableCell;
    second_cell_style.width = Some(Length::px(400.0));
    let second_cell = BoxNode::new_block(
      Arc::new(second_cell_style),
      FormattingContextType::Block,
      vec![],
    );
    let second_row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![second_cell.clone()],
    );

    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![first_row, second_row],
    );

    let tfc = TableFormattingContext::new();
    let constraints = LayoutConstraints::definite_width(200.0);
    let structure = TableStructure::from_box_tree(&table);
    let mut column_constraints: Vec<ColumnConstraints> = (0..structure.column_count)
      .map(|_| ColumnConstraints::new(0.0, 0.0))
      .collect();
    let spacing = structure.total_horizontal_spacing();
    let edge_consumption = 0.0;
    let available_content = (200.0 - spacing - edge_consumption).max(0.0);
    tfc.populate_column_constraints(
      &table,
      &structure,
      &mut column_constraints,
      DistributionMode::Fixed,
      Some(available_content),
      &StyleOverrideCache::default(),
    );
    let distribution = ColumnDistributor::new(DistributionMode::Fixed)
      .distribute(&column_constraints, available_content);
    let fragment = tfc.layout(&table, &constraints).expect("table layout");

    // Expect first column to take ~50% of available space regardless of second row width.
    assert_eq!(distribution.widths.len(), 2);
    assert!((distribution.widths[0] - 100.0).abs() < 0.1);
    assert!((distribution.widths[1] - 100.0).abs() < 0.1);
    assert!((fragment.bounds.width() - 200.0).abs() < 0.1);
  }

  #[test]
  fn table_respects_specified_height() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.height = Some(Length::px(100.0));
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;

    let cell1 = BoxNode::new_block(
      Arc::new(cell_style.clone()),
      FormattingContextType::Block,
      vec![],
    );
    let row1 = BoxNode::new_block(
      Arc::new(row_style.clone()),
      FormattingContextType::Block,
      vec![cell1],
    );
    let cell2 = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
    let row2 = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell2],
    );

    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row1, row2],
    );
    let tfc = TableFormattingContext::new();
    let fragment = tfc
      .layout(&table, &LayoutConstraints::definite(100.0, 200.0))
      .expect("table layout");

    assert!((fragment.bounds.height() - 100.0).abs() < 0.1);
  }

  #[test]
  fn percent_row_heights_resolve_when_table_height_definite() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.height = Some(Length::px(120.0));
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);

    let mut row1_style = ComputedStyle::default();
    row1_style.display = Display::TableRow;
    row1_style.height = Some(Length::percent(25.0));

    let mut row2_style = ComputedStyle::default();
    row2_style.display = Display::TableRow;
    row2_style.height = Some(Length::percent(75.0));

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;

    let row1 = BoxNode::new_block(
      Arc::new(row1_style),
      FormattingContextType::Block,
      vec![BoxNode::new_block(
        Arc::new(cell_style.clone()),
        FormattingContextType::Block,
        vec![],
      )],
    );
    let row2 = BoxNode::new_block(
      Arc::new(row2_style),
      FormattingContextType::Block,
      vec![BoxNode::new_block(
        Arc::new(cell_style),
        FormattingContextType::Block,
        vec![],
      )],
    );

    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row1, row2],
    );
    let tfc = TableFormattingContext::new();
    let fragment = tfc
      .layout(&table, &LayoutConstraints::definite(100.0, 200.0))
      .expect("table layout");

    assert!((fragment.bounds.height() - 120.0).abs() < 0.1);
    assert!(fragment.children.len() >= 2);
    let first_y = fragment.children[0].bounds.y();
    let second_y = fragment.children[1].bounds.y();
    assert!((second_y - first_y - 30.0).abs() < 0.1);
  }

  #[test]
  fn percent_row_heights_account_for_spacing() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.height = Some(Length::px(120.0));
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(10.0);

    let mut row1_style = ComputedStyle::default();
    row1_style.display = Display::TableRow;
    row1_style.height = Some(Length::percent(50.0));

    let mut row2_style = ComputedStyle::default();
    row2_style.display = Display::TableRow;
    row2_style.height = Some(Length::percent(50.0));

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;

    let row1 = BoxNode::new_block(
      Arc::new(row1_style),
      FormattingContextType::Block,
      vec![BoxNode::new_block(
        Arc::new(cell_style.clone()),
        FormattingContextType::Block,
        vec![],
      )],
    );
    let row2 = BoxNode::new_block(
      Arc::new(row2_style),
      FormattingContextType::Block,
      vec![BoxNode::new_block(
        Arc::new(cell_style),
        FormattingContextType::Block,
        vec![],
      )],
    );

    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row1, row2],
    );
    let tfc = TableFormattingContext::new();
    let fragment = tfc
      .layout(&table, &LayoutConstraints::definite(100.0, 200.0))
      .expect("table layout");

    // Total spacing = 2 rows * 10 = 20, so rows share the remaining 100 -> 50 each.
    assert!((fragment.bounds.height() - 120.0).abs() < 0.1);
    assert!(fragment.children.len() >= 2);
    let first_y = fragment.children[0].bounds.y();
    let second_y = fragment.children[1].bounds.y();
    assert!((second_y - first_y - 60.0).abs() < 0.1); // 50 height + 10 spacing
  }

  #[test]
  fn percent_rows_ignored_without_table_height() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.min_height = Some(Length::px(100.0));
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;
    row_style.height = Some(Length::percent(50.0));

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;

    let row1 = BoxNode::new_block(
      Arc::new(row_style.clone()),
      FormattingContextType::Block,
      vec![BoxNode::new_block(
        Arc::new(cell_style.clone()),
        FormattingContextType::Block,
        vec![],
      )],
    );
    let row2 = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![BoxNode::new_block(
        Arc::new(cell_style),
        FormattingContextType::Block,
        vec![],
      )],
    );

    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row1, row2],
    );
    let tfc = TableFormattingContext::new();
    let fragment = tfc
      .layout(&table, &LayoutConstraints::definite(100.0, 400.0))
      .expect("table layout");

    // Percent rows should not use the table min-height as a percentage base; they should collapse to intrinsic size.
    assert!(fragment.children.len() >= 2);
    let first_y = fragment.children[0].bounds.y();
    let second_y = fragment.children[1].bounds.y();
    assert!((second_y - first_y) < 10.0);
    assert!(fragment.bounds.height() >= 100.0);
  }

  #[test]
  fn percent_rows_fill_target_height() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.height = Some(Length::px(200.0));
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);

    let mut row1_style = ComputedStyle::default();
    row1_style.display = Display::TableRow;
    row1_style.height = Some(Length::percent(50.0));

    let mut row2_style = ComputedStyle::default();
    row2_style.display = Display::TableRow;
    // auto row with small intrinsic height

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;

    let row1 = BoxNode::new_block(
      Arc::new(row1_style),
      FormattingContextType::Block,
      vec![BoxNode::new_block(
        Arc::new(cell_style.clone()),
        FormattingContextType::Block,
        vec![],
      )],
    );
    let row2 = BoxNode::new_block(
      Arc::new(row2_style),
      FormattingContextType::Block,
      vec![BoxNode::new_block(
        Arc::new(cell_style),
        FormattingContextType::Block,
        vec![],
      )],
    );

    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row1, row2],
    );
    let tfc = TableFormattingContext::new();
    let fragment = tfc
      .layout(&table, &LayoutConstraints::definite(100.0, 300.0))
      .expect("table layout");

    assert!((fragment.bounds.height() - 200.0).abs() < 0.1);
    assert_eq!(fragment.children.len(), 2);
    let first_y = fragment.children[0].bounds.y();
    let second_y = fragment.children[1].bounds.y();
    assert!((second_y - first_y - 100.0).abs() < 0.5);
  }

  #[test]
  fn percent_rows_overflow_do_not_shrink_content() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.height = Some(Length::px(100.0));
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);

    let mut row1_style = ComputedStyle::default();
    row1_style.display = Display::TableRow;
    row1_style.height = Some(Length::percent(60.0));

    let mut row2_style = ComputedStyle::default();
    row2_style.display = Display::TableRow;
    row2_style.height = Some(Length::percent(60.0));

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;

    let tall_child = BoxNode::new_block(
      Arc::new(ComputedStyle {
        height: Some(Length::px(80.0)),
        display: Display::Block,
        ..ComputedStyle::default()
      }),
      FormattingContextType::Block,
      vec![],
    );

    let row1 = BoxNode::new_block(
      Arc::new(row1_style),
      FormattingContextType::Block,
      vec![BoxNode::new_block(
        Arc::new(cell_style.clone()),
        FormattingContextType::Block,
        vec![tall_child.clone()],
      )],
    );
    let row2 = BoxNode::new_block(
      Arc::new(row2_style),
      FormattingContextType::Block,
      vec![BoxNode::new_block(
        Arc::new(cell_style),
        FormattingContextType::Block,
        vec![tall_child],
      )],
    );

    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row1, row2],
    );
    let tfc = TableFormattingContext::new();
    let fragment = tfc
      .layout(&table, &LayoutConstraints::definite(100.0, 300.0))
      .expect("table layout");

    assert_eq!(fragment.children.len(), 2);
    let first_y = fragment.children[0].bounds.y();
    let second_y = fragment.children[1].bounds.y();
    // Even though the percentages sum to 120% of the table height, rows must not shrink below their 80px content.
    assert!(second_y - first_y >= 79.9);
  }

  #[test]
  fn percent_rows_over_100_percent_do_not_scale_down() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.height = Some(Length::px(100.0));
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);

    let mut row1_style = ComputedStyle::default();
    row1_style.display = Display::TableRow;
    row1_style.height = Some(Length::percent(60.0));

    let mut row2_style = ComputedStyle::default();
    row2_style.display = Display::TableRow;
    row2_style.height = Some(Length::percent(60.0));

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;

    let row1 = BoxNode::new_block(
      Arc::new(row1_style),
      FormattingContextType::Block,
      vec![BoxNode::new_block(
        Arc::new(cell_style.clone()),
        FormattingContextType::Block,
        vec![],
      )],
    );
    let row2 = BoxNode::new_block(
      Arc::new(row2_style),
      FormattingContextType::Block,
      vec![BoxNode::new_block(
        Arc::new(cell_style),
        FormattingContextType::Block,
        vec![],
      )],
    );

    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row1, row2],
    );
    let tfc = TableFormattingContext::new();
    let fragment = tfc
      .layout(&table, &LayoutConstraints::definite(200.0, 200.0))
      .expect("table layout");

    assert_eq!(fragment.children.len(), 2);
    let first_y = fragment.children[0].bounds.y();
    let second_y = fragment.children[1].bounds.y();
    // Percent rows totalling 120% of the table height should keep their targets even if the table overflows.
    assert!((second_y - first_y - 60.0).abs() < 0.5);
    assert!((fragment.bounds.height() - 100.0).abs() < 0.1);
  }

  #[test]
  fn table_padding_offsets_children_and_expands_bounds() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);
    table_style.padding_left = Length::px(5.0);
    table_style.padding_right = Length::px(5.0);
    table_style.padding_top = Length::px(6.0);
    table_style.padding_bottom = Length::px(4.0);

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    cell_style.width = Some(Length::px(10.0));

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut child_style = ComputedStyle::default();
    child_style.display = Display::Block;
    child_style.height = Some(Length::px(8.0));
    child_style.width = Some(Length::px(10.0));

    let cell = BoxNode::new_block(
      Arc::new(cell_style),
      FormattingContextType::Block,
      vec![BoxNode::new_block(
        Arc::new(child_style),
        FormattingContextType::Block,
        vec![],
      )],
    );
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell],
    );
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let tfc = TableFormattingContext::new();
    let fragment = tfc
      .layout(&table, &LayoutConstraints::min_content())
      .expect("table layout");

    let child = &fragment.children[0];
    assert!((child.bounds.x() - 5.0).abs() < 0.1);
    assert!((child.bounds.y() - 6.0).abs() < 0.1);
    assert!((fragment.bounds.width() - 20.0).abs() < 0.1); // 10 content + 5+5 padding
    assert!((fragment.bounds.height() - 18.0).abs() < 0.1); // 8 content + 6+4 padding
  }

  #[test]
  fn table_borders_expand_bounds() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);
    table_style.border_left_width = Length::px(3.0);
    table_style.border_right_width = Length::px(2.0);
    table_style.border_top_width = Length::px(4.0);
    table_style.border_bottom_width = Length::px(1.0);

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    cell_style.width = Some(Length::px(10.0));

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut child_style = ComputedStyle::default();
    child_style.display = Display::Block;
    child_style.height = Some(Length::px(5.0));
    child_style.width = Some(Length::px(10.0));

    let cell = BoxNode::new_block(
      Arc::new(cell_style),
      FormattingContextType::Block,
      vec![BoxNode::new_block(
        Arc::new(child_style),
        FormattingContextType::Block,
        vec![],
      )],
    );
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell],
    );
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let tfc = TableFormattingContext::new();
    let fragment = tfc
      .layout(&table, &LayoutConstraints::min_content())
      .expect("table layout");

    let child = &fragment.children[0];
    assert!((child.bounds.x() - 3.0).abs() < 0.1);
    assert!((child.bounds.y() - 4.0).abs() < 0.1);
    assert!((fragment.bounds.width() - 15.0).abs() < 0.1); // 10 content + 3 + 2 borders
    assert!((fragment.bounds.height() - 10.0).abs() < 0.1); // 5 content + 4 + 1 borders
  }

  #[test]
  fn table_padding_and_borders_reduce_available_width() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.width = Some(Length::px(100.0));
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);
    table_style.padding_left = Length::px(5.0);
    table_style.padding_right = Length::px(5.0);
    table_style.border_left_width = Length::px(2.0);
    table_style.border_right_width = Length::px(2.0);

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    cell_style.min_width = Some(Length::px(10.0));
    cell_style.max_width = Some(Length::px(1000.0));

    let cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
    let row = BoxNode::new_block(
      Arc::new({
        let mut s = ComputedStyle::default();
        s.display = Display::TableRow;
        s
      }),
      FormattingContextType::Block,
      vec![cell],
    );
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let tfc = TableFormattingContext::new();
    let fragment = tfc
      .layout(&table, &LayoutConstraints::definite(100.0, 200.0))
      .expect("table layout");

    let child = &fragment.children[0];
    // Content width should be table width minus padding/borders: 100 - (5+5) - (2+2) = 86.
    assert!((child.bounds.width() - 86.0).abs() < 0.1);
    // Child should start after left border+padding.
    assert!((child.bounds.x() - 7.0).abs() < 0.1);
    assert!((fragment.bounds.width() - 100.0).abs() < 0.1);
  }

  #[test]
  fn table_intrinsic_width_includes_padding_and_borders() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);
    table_style.padding_left = Length::px(5.0);
    table_style.padding_right = Length::px(5.0);
    table_style.border_left_width = Length::px(2.0);
    table_style.border_right_width = Length::px(2.0);

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    cell_style.min_width = Some(Length::px(10.0));

    let cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
    let row = BoxNode::new_block(
      Arc::new({
        let mut s = ComputedStyle::default();
        s.display = Display::TableRow;
        s
      }),
      FormattingContextType::Block,
      vec![cell],
    );
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let tfc = TableFormattingContext::new();
    let min_width = tfc
      .compute_intrinsic_inline_size(&table, IntrinsicSizingMode::MinContent)
      .expect("intrinsic size");
    // Expect min-content = cell min (10) + padding (10) + borders (4) = 24.
    assert!((min_width - 24.0).abs() < 0.1);
  }

  #[test]
  fn table_max_height_clamps_bounds() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.max_height = Some(Length::px(40.0));
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;

    let tall_cell = BoxNode::new_block(
      Arc::new(cell_style.clone()),
      FormattingContextType::Block,
      vec![BoxNode::new_block(
        Arc::new(ComputedStyle {
          height: Some(Length::px(100.0)),
          display: Display::Block,
          ..ComputedStyle::default()
        }),
        FormattingContextType::Block,
        vec![],
      )],
    );
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![tall_cell],
    );
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let tfc = TableFormattingContext::new();
    let fragment = tfc
      .layout(&table, &LayoutConstraints::definite(100.0, 200.0))
      .expect("table layout");
    assert!(fragment.bounds.height() <= 40.0 + 0.01);
  }

  #[test]
  fn test_table_structure_from_simple_table() {
    let table = create_simple_table(2, 3);
    let structure = TableStructure::from_box_tree(&table);

    assert_eq!(structure.row_count, 2);
    assert_eq!(structure.column_count, 3);
    assert_eq!(structure.cells.len(), 6);
  }

  #[test]
  fn test_table_structure_empty_table() {
    let style = create_test_style();
    let table = BoxNode::new_block(style, FormattingContextType::Table, vec![])
      .with_debug_info(DebugInfo::new(Some("table".to_string()), None, vec![]));

    let structure = TableStructure::from_box_tree(&table);
    assert_eq!(structure.row_count, 0);
    assert_eq!(structure.column_count, 0);
  }

  #[test]
  fn test_table_structure_single_cell() {
    let table = create_simple_table(1, 1);
    let structure = TableStructure::from_box_tree(&table);

    assert_eq!(structure.row_count, 1);
    assert_eq!(structure.column_count, 1);
    assert_eq!(structure.cells.len(), 1);
  }

  #[test]
  fn row_group_visibility_collapse_removes_rows_and_cells() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut collapsed_group_style = ComputedStyle::default();
    collapsed_group_style.display = Display::TableRowGroup;
    collapsed_group_style.visibility = Visibility::Collapse;

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;

    let cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell],
    );
    let row_group = BoxNode::new_block(
      Arc::new(collapsed_group_style),
      FormattingContextType::Block,
      vec![row],
    );

    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row_group],
    );
    let structure = TableStructure::from_box_tree(&table);

    assert_eq!(structure.row_count, 0);
    assert!(structure.cells.is_empty());
  }

  #[test]
  fn column_group_visibility_collapse_removes_columns() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);

    let mut col_style = ComputedStyle::default();
    col_style.display = Display::TableColumn;

    let mut collapsed_col_group_style = ComputedStyle::default();
    collapsed_col_group_style.display = Display::TableColumnGroup;
    collapsed_col_group_style.visibility = Visibility::Collapse;

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    let cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![])
      .with_debug_info(DebugInfo::new(Some("td".to_string()), None, vec![]).with_spans(2, 1));
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell],
    );

    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![
        BoxNode::new_block(
          Arc::new(col_style.clone()),
          FormattingContextType::Block,
          vec![],
        ),
        BoxNode::new_block(
          Arc::new(collapsed_col_group_style),
          FormattingContextType::Block,
          vec![],
        ),
        row,
      ],
    );

    let structure = TableStructure::from_box_tree(&table);
    assert_eq!(structure.column_count, 1);
    assert_eq!(structure.cells.len(), 1);
    let cell_info = &structure.cells[0];
    assert_eq!(cell_info.colspan, 1);
    assert_eq!(cell_info.col, 0);
  }

  #[test]
  fn visibility_collapse_filters_rows_and_cells() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut collapsed_row_style = row_style.clone();
    collapsed_row_style.visibility = Visibility::Collapse;

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;

    let collapsed_cell = BoxNode::new_block(
      Arc::new(cell_style.clone()),
      FormattingContextType::Block,
      vec![],
    );
    let visible_cell =
      BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
    let collapsed_row = BoxNode::new_block(
      Arc::new(collapsed_row_style),
      FormattingContextType::Block,
      vec![collapsed_cell],
    );
    let visible_row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![visible_cell],
    );

    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![collapsed_row, visible_row],
    );

    let structure = TableStructure::from_box_tree(&table);
    assert_eq!(structure.row_count, 1);
    assert_eq!(structure.column_count, 1);
    assert_eq!(structure.rows[0].source_index, 1);
    assert_eq!(structure.cells.len(), 1);
    let cell = &structure.cells[0];
    assert_eq!(cell.row, 0);
    assert_eq!(cell.source_row, 1);
  }

  #[test]
  fn visibility_collapse_filters_columns_and_adjusts_colspan() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);

    let mut col_style = ComputedStyle::default();
    col_style.display = Display::TableColumn;

    let mut collapsed_col_style = col_style.clone();
    collapsed_col_style.visibility = Visibility::Collapse;

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;

    let spanning_cell =
      BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![])
        .with_debug_info(DebugInfo::new(Some("td".to_string()), None, vec![]).with_spans(2, 1));
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![spanning_cell],
    );

    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![
        BoxNode::new_block(Arc::new(col_style), FormattingContextType::Block, vec![]),
        BoxNode::new_block(
          Arc::new(collapsed_col_style),
          FormattingContextType::Block,
          vec![],
        ),
        row,
      ],
    );

    let structure = TableStructure::from_box_tree(&table);
    assert_eq!(structure.column_count, 1);
    assert_eq!(structure.columns[0].source_index, 0);
    assert_eq!(structure.cells.len(), 1);
    let cell = &structure.cells[0];
    assert_eq!(cell.colspan, 1);
    assert_eq!(cell.source_col, 0);
  }

  #[test]
  fn rowspan_shifts_following_row_cells() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    cell_style.width = Some(Length::px(10.0));

    let spanning_cell = BoxNode::new_block(
      Arc::new(cell_style.clone()),
      FormattingContextType::Block,
      vec![],
    )
    .with_debug_info(DebugInfo::new(Some("td".to_string()), None, vec![]).with_spans(1, 2));
    let first_row_sibling = BoxNode::new_block(
      Arc::new(cell_style.clone()),
      FormattingContextType::Block,
      vec![],
    )
    .with_debug_info(DebugInfo::new(Some("td".to_string()), None, vec![]).with_spans(1, 1));
    let first_row = BoxNode::new_block(
      Arc::new(row_style.clone()),
      FormattingContextType::Block,
      vec![spanning_cell, first_row_sibling],
    );

    let second_row_cell =
      BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![])
        .with_debug_info(DebugInfo::new(Some("td".to_string()), None, vec![]).with_spans(1, 1));
    let second_row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![second_row_cell],
    );

    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![first_row, second_row],
    );

    let structure = TableStructure::from_box_tree(&table);
    assert_eq!(structure.row_count, 2);
    assert_eq!(structure.column_count, 2);

    let spanning = structure
      .cells
      .iter()
      .find(|c| c.source_row == 0 && c.box_index == 0)
      .expect("spanning cell");
    let first_row_second = structure
      .cells
      .iter()
      .find(|c| c.source_row == 0 && c.box_index == 1)
      .expect("sibling cell");
    let second_row_only = structure
      .cells
      .iter()
      .find(|c| c.source_row == 1 && c.box_index == 0)
      .expect("second row cell");

    assert_eq!(spanning.col, 0);
    assert_eq!(spanning.rowspan, 2);
    assert_eq!(first_row_second.col, 1);
    assert_eq!(second_row_only.col, 1);
    assert_eq!(structure.grid[1][0], Some(spanning.index));
    assert_eq!(structure.grid[1][1], Some(second_row_only.index));

    let tfc = TableFormattingContext::new();
    let fragment = tfc
      .layout(&table, &LayoutConstraints::min_content())
      .expect("table layout");
    let cell_fragments: Vec<&FragmentNode> = fragment
      .children
      .iter()
      .filter(|f| {
        f.style
          .as_ref()
          .map(|s| matches!(s.display, Display::TableCell))
          .unwrap_or(false)
      })
      .collect();
    assert_eq!(cell_fragments.len(), 3);
    let col1_x = cell_fragments[1].bounds.x();
    let second_row_x = cell_fragments[2].bounds.x();
    assert!(
      (col1_x - second_row_x).abs() < 0.1,
      "second-row cell should be placed in column 1 ({} vs {})",
      col1_x,
      second_row_x
    );
    assert!(
      second_row_x > cell_fragments[0].bounds.x(),
      "rowspanning cell should reserve its column"
    );
  }

  #[test]
  fn rowspan_and_colspan_find_next_free_block() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    cell_style.width = Some(Length::px(10.0));

    let top_spanning = BoxNode::new_block(
      Arc::new(cell_style.clone()),
      FormattingContextType::Block,
      vec![],
    )
    .with_debug_info(DebugInfo::new(Some("td".to_string()), None, vec![]).with_spans(1, 2));
    let top_neighbor = BoxNode::new_block(
      Arc::new(cell_style.clone()),
      FormattingContextType::Block,
      vec![],
    )
    .with_debug_info(DebugInfo::new(Some("td".to_string()), None, vec![]).with_spans(1, 1));
    let first_row = BoxNode::new_block(
      Arc::new(row_style.clone()),
      FormattingContextType::Block,
      vec![top_spanning, top_neighbor],
    );

    let bottom_span =
      BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![])
        .with_debug_info(DebugInfo::new(Some("td".to_string()), None, vec![]).with_spans(2, 1));
    let second_row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![bottom_span],
    );

    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![first_row, second_row],
    );

    let structure = TableStructure::from_box_tree(&table);
    assert_eq!(structure.row_count, 2);
    assert_eq!(structure.column_count, 3);

    let spanning = structure
      .cells
      .iter()
      .find(|c| c.source_row == 0 && c.box_index == 0)
      .expect("top spanning cell");
    let neighbor = structure
      .cells
      .iter()
      .find(|c| c.source_row == 0 && c.box_index == 1)
      .expect("top neighbor cell");
    let spanning_two = structure
      .cells
      .iter()
      .find(|c| c.source_row == 1 && c.box_index == 0)
      .expect("bottom spanning cell");

    assert_eq!(spanning.col, 0);
    assert_eq!(neighbor.col, 1);
    assert_eq!(spanning_two.col, 1);
    assert_eq!(spanning_two.colspan, 2);
    assert_eq!(structure.grid[1][0], Some(spanning.index));
    assert_eq!(structure.grid[1][1], Some(spanning_two.index));
    assert_eq!(structure.grid[1][2], Some(spanning_two.index));

    let tfc = TableFormattingContext::new();
    let fragment = tfc
      .layout(&table, &LayoutConstraints::min_content())
      .expect("table layout");
    let cell_fragments: Vec<&FragmentNode> = fragment
      .children
      .iter()
      .filter(|f| {
        f.style
          .as_ref()
          .map(|s| matches!(s.display, Display::TableCell))
          .unwrap_or(false)
      })
      .collect();
    assert_eq!(cell_fragments.len(), 3);
    let neighbor_x = cell_fragments[1].bounds.x();
    let spanning_two_x = cell_fragments[2].bounds.x();
    assert!(
      (neighbor_x - spanning_two_x).abs() < 0.1,
      "colspan cell should start at the next free column ({:.2} vs {:.2})",
      neighbor_x,
      spanning_two_x
    );
    assert!(
      spanning_two_x > cell_fragments[0].bounds.x(),
      "rowspanning cell should keep later spans out of its column"
    );
  }

  #[test]
  fn test_column_info_new() {
    let col = ColumnInfo::new(5);
    assert_eq!(col.index, 5);
    assert_eq!(col.source_index, 5);
    assert_eq!(col.visibility, Visibility::Visible);
    assert!(col.specified_width.is_none());
    assert_eq!(col.min_width, 0.0);
    assert_eq!(col.computed_width, 0.0);
  }

  #[test]
  fn test_row_info_new() {
    let row = RowInfo::new(3);
    assert_eq!(row.index, 3);
    assert_eq!(row.source_index, 3);
    assert_eq!(row.visibility, Visibility::Visible);
    assert!(row.specified_height.is_none());
    assert!(row.author_min_height.is_none());
    assert!(row.author_max_height.is_none());
    assert_eq!(row.min_height, 0.0);
    assert_eq!(row.y_position, 0.0);
  }

  #[test]
  fn test_cell_info_new() {
    let cell = CellInfo::new(0, 2, 3);
    assert_eq!(cell.index, 0);
    assert_eq!(cell.source_row, 2);
    assert_eq!(cell.source_col, 3);
    assert_eq!(cell.row, 2);
    assert_eq!(cell.col, 3);
    assert_eq!(cell.rowspan, 1);
    assert_eq!(cell.colspan, 1);
  }

  #[test]
  fn test_cell_spanning_detection() {
    let mut cell = CellInfo::new(0, 0, 0);
    assert!(!cell.is_column_spanning());
    assert!(!cell.is_row_spanning());

    cell.colspan = 2;
    assert!(cell.is_column_spanning());
    assert!(!cell.is_row_spanning());

    cell.rowspan = 3;
    assert!(cell.is_column_spanning());
    assert!(cell.is_row_spanning());
  }

  #[test]
  fn test_horizontal_spacing_calculation() {
    let mut structure = TableStructure::new();
    structure.column_count = 3;
    structure.border_spacing = (5.0, 5.0);

    // Total spacing = column_count * spacing (includes both edges)
    assert_eq!(structure.total_horizontal_spacing(), 15.0);
  }

  #[test]
  fn test_vertical_spacing_calculation() {
    let mut structure = TableStructure::new();
    structure.row_count = 2;
    structure.border_spacing = (5.0, 10.0);

    // Total spacing = row_count * spacing (includes both edges)
    assert_eq!(structure.total_vertical_spacing(), 20.0);
  }

  #[test]
  fn border_spacing_calc_is_clamped_to_zero() {
    let len = CalcLength::single(LengthUnit::Px, -5.0);
    let spacing = resolve_border_spacing_length(&Length::calc(len), 16.0);
    assert_eq!(spacing, 0.0);
  }

  #[test]
  fn border_spacing_calc_requires_viewport_for_vw() {
    let len = CalcLength::single(LengthUnit::Vw, 10.0);
    // No viewport -> unresolved calc treated as zero.
    let spacing = resolve_border_spacing_length(&Length::calc(len), 16.0);
    assert_eq!(spacing, 0.0);

    // With viewport, 10vw of 200px => 20px spacing.
    let spacing = Length::calc(CalcLength::single(LengthUnit::Vw, 10.0))
      .resolve_with_context(None, 200.0, 200.0, 16.0, 16.0)
      .unwrap();
    assert!((spacing - 20.0).abs() < 0.01);
  }

  #[test]
  fn collapsed_double_border_is_at_least_three_px() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_collapse = BorderCollapse::Collapse;

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    cell_style.border_top_style = BorderStyle::Double;
    cell_style.border_right_style = BorderStyle::Double;
    cell_style.border_bottom_style = BorderStyle::Double;
    cell_style.border_left_style = BorderStyle::Double;
    cell_style.border_top_width = Length::px(1.0);
    cell_style.border_right_width = Length::px(1.0);
    cell_style.border_bottom_width = Length::px(1.0);
    cell_style.border_left_width = Length::px(1.0);

    let cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
    let row = BoxNode::new_block(
      Arc::new(ComputedStyle {
        display: Display::TableRow,
        ..ComputedStyle::default()
      }),
      FormattingContextType::Block,
      vec![cell],
    );
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let structure = TableStructure::from_box_tree(&table);
    let borders = compute_collapsed_borders(&table, &structure);

    // All edges should be at least 3px wide when style is double.
    assert!(borders.vertical[0][0].width >= 3.0);
    assert!(borders.vertical[1][0].width >= 3.0);
    assert!(borders.horizontal[0][0].width >= 3.0);
    assert!(borders.horizontal[1][0].width >= 3.0);
  }

  #[test]
  fn collapsed_corners_keep_winning_style_and_color() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_collapse = BorderCollapse::Collapse;

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    cell_style.border_top_style = BorderStyle::Dotted;
    cell_style.border_right_style = BorderStyle::Dotted;
    cell_style.border_bottom_style = BorderStyle::Dotted;
    cell_style.border_left_style = BorderStyle::Dotted;
    cell_style.border_top_width = Length::px(2.0);
    cell_style.border_right_width = Length::px(2.0);
    cell_style.border_bottom_width = Length::px(2.0);
    cell_style.border_left_width = Length::px(2.0);
    cell_style.border_top_color = Rgba::from_rgba8(200, 0, 0, 255);
    cell_style.border_right_color = Rgba::from_rgba8(200, 0, 0, 255);
    cell_style.border_bottom_color = Rgba::from_rgba8(200, 0, 0, 255);
    cell_style.border_left_color = Rgba::from_rgba8(200, 0, 0, 255);

    let cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
    let row = BoxNode::new_block(
      Arc::new(ComputedStyle {
        display: Display::TableRow,
        ..ComputedStyle::default()
      }),
      FormattingContextType::Block,
      vec![cell],
    );
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let structure = TableStructure::from_box_tree(&table);
    let borders = compute_collapsed_borders(&table, &structure);

    for r in 0..=structure.row_count {
      for c in 0..=structure.column_count {
        let corner = &borders.corners[r][c];
        assert!(corner.width >= 2.0);
        assert!(matches!(corner.style, BorderStyle::Dotted));
        assert_eq!(corner.color, Rgba::from_rgba8(200, 0, 0, 255));
      }
    }
  }

  #[test]
  fn test_zero_column_spacing() {
    let structure = TableStructure::new();
    assert_eq!(structure.total_horizontal_spacing(), 0.0);
  }

  // -------------------------------------------------------------------------
  // Column Width Algorithm Tests
  // -------------------------------------------------------------------------

  #[test]
  fn test_fixed_layout_equal_distribution() {
    let mut structure = TableStructure::new();
    structure.column_count = 4;
    structure.columns = (0..4).map(ColumnInfo::new).collect();
    for col in &mut structure.columns {
      col.min_width = 0.0;
      col.max_width = f32::INFINITY;
    }
    structure.border_spacing = (0.0, 0.0);

    calculate_fixed_layout_widths(&mut structure, 400.0);

    for col in &structure.columns {
      assert!((col.computed_width - 100.0).abs() < 0.01);
    }
  }

  #[test]
  fn test_fixed_layout_with_specified_widths() {
    let mut structure = TableStructure::new();
    structure.column_count = 3;
    structure.columns = vec![
      ColumnInfo {
        index: 0,
        source_index: 0,
        author_min_width: None,
        author_max_width: None,
        font_size: 0.0,
        visibility: Visibility::Visible,
        specified_width: Some(SpecifiedWidth::Fixed(100.0)),
        min_width: 0.0,
        max_width: f32::INFINITY,
        computed_width: 0.0,
      },
      ColumnInfo {
        index: 1,
        source_index: 1,
        author_min_width: None,
        author_max_width: None,
        font_size: 0.0,
        visibility: Visibility::Visible,
        specified_width: None,
        min_width: 0.0,
        max_width: f32::INFINITY,
        computed_width: 0.0,
      },
      ColumnInfo {
        index: 2,
        source_index: 2,
        author_min_width: None,
        author_max_width: None,
        font_size: 0.0,
        visibility: Visibility::Visible,
        specified_width: Some(SpecifiedWidth::Fixed(50.0)),
        min_width: 0.0,
        max_width: f32::INFINITY,
        computed_width: 0.0,
      },
    ];
    structure.border_spacing = (0.0, 0.0);

    calculate_fixed_layout_widths(&mut structure, 300.0);

    assert!((structure.columns[0].computed_width - 100.0).abs() < 0.01);
    assert!((structure.columns[1].computed_width - 150.0).abs() < 0.01);
    assert!((structure.columns[2].computed_width - 50.0).abs() < 0.01);
  }

  #[test]
  fn absolute_child_inherits_nearest_positioned_cb_in_table() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;

    let mut abs_style = ComputedStyle::default();
    abs_style.display = Display::Block;
    abs_style.position = Position::Absolute;
    abs_style.left = Some(Length::px(5.0));
    abs_style.top = Some(Length::px(7.0));
    abs_style.width = Some(Length::px(12.0));
    abs_style.height = Some(Length::px(9.0));

    let abs_child = BoxNode::new_block(Arc::new(abs_style), FormattingContextType::Block, vec![]);
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![abs_child],
    );

    let viewport = crate::geometry::Size::new(400.0, 400.0);
    let cb_rect = crate::geometry::Rect::from_xywh(30.0, 40.0, 200.0, 200.0);
    let cb = ContainingBlock::with_viewport(cb_rect, viewport);
    let factory =
      FormattingContextFactory::with_font_context_viewport_and_cb(FontContext::new(), viewport, cb);
    let fc = TableFormattingContext::with_factory(factory);

    let constraints = LayoutConstraints::definite(120.0, 120.0);
    let fragment = fc.layout(&table, &constraints).unwrap();

    assert!(!fragment.children.is_empty());
    let abs_fragment = fragment.children.last().unwrap();
    assert_eq!(abs_fragment.bounds.x(), 35.0);
    assert_eq!(abs_fragment.bounds.y(), 47.0);
    assert_eq!(abs_fragment.bounds.width(), 12.0);
    assert_eq!(abs_fragment.bounds.height(), 9.0);
  }

  #[test]
  fn test_fixed_layout_percentage_width() {
    let mut structure = TableStructure::new();
    structure.column_count = 2;
    structure.columns = vec![
      ColumnInfo {
        index: 0,
        source_index: 0,
        author_min_width: None,
        author_max_width: None,
        font_size: 0.0,
        visibility: Visibility::Visible,
        specified_width: Some(SpecifiedWidth::Percent(25.0)),
        min_width: 0.0,
        max_width: f32::INFINITY,
        computed_width: 0.0,
      },
      ColumnInfo {
        index: 1,
        source_index: 1,
        author_min_width: None,
        author_max_width: None,
        font_size: 0.0,
        visibility: Visibility::Visible,
        specified_width: Some(SpecifiedWidth::Percent(75.0)),
        min_width: 0.0,
        max_width: f32::INFINITY,
        computed_width: 0.0,
      },
    ];
    structure.border_spacing = (0.0, 0.0);

    calculate_fixed_layout_widths(&mut structure, 400.0);

    assert!((structure.columns[0].computed_width - 100.0).abs() < 0.01);
    assert!((structure.columns[1].computed_width - 300.0).abs() < 0.01);
  }

  #[test]
  fn fixed_layout_percentages_can_overrun_table_width() {
    let mut structure = TableStructure::new();
    structure.column_count = 2;
    let mut col0 = ColumnInfo::new(0);
    col0.specified_width = Some(SpecifiedWidth::Percent(75.0));
    let mut col1 = ColumnInfo::new(1);
    col1.specified_width = Some(SpecifiedWidth::Percent(75.0));
    structure.columns = vec![col0, col1];
    structure.border_spacing = (0.0, 0.0);

    calculate_fixed_layout_widths(&mut structure, 200.0);

    assert!((structure.columns[0].computed_width - 150.0).abs() < 0.01);
    assert!((structure.columns[1].computed_width - 150.0).abs() < 0.01);
    let total: f32 = structure.columns.iter().map(|c| c.computed_width).sum();
    assert!(
      total > 299.0,
      "percent columns should not be scaled down when exceeding 100%"
    );
  }

  #[test]
  fn fixed_layout_respects_min_width_caps() {
    let mut structure = TableStructure::new();
    structure.column_count = 2;
    let mut col0 = ColumnInfo::new(0);
    col0.min_width = 150.0;
    let mut col1 = ColumnInfo::new(1);
    col1.min_width = 80.0;
    structure.columns = vec![col0, col1];
    structure.border_spacing = (0.0, 0.0);

    calculate_fixed_layout_widths(&mut structure, 200.0);

    assert!((structure.columns[0].computed_width - 150.0).abs() < 0.01);
    assert!((structure.columns[1].computed_width - 80.0).abs() < 0.01);
  }

  #[test]
  fn fixed_layout_max_width_caps_stop_growth() {
    let mut structure = TableStructure::new();
    structure.column_count = 2;
    let mut col0 = ColumnInfo::new(0);
    col0.max_width = 50.0;
    let mut col1 = ColumnInfo::new(1);
    col1.max_width = 60.0;
    structure.columns = vec![col0, col1];
    structure.border_spacing = (0.0, 0.0);

    calculate_fixed_layout_widths(&mut structure, 200.0);

    assert!((structure.columns[0].computed_width - 50.0).abs() < 0.01);
    assert!((structure.columns[1].computed_width - 60.0).abs() < 0.01);
    let total: f32 = structure.columns.iter().map(|c| c.computed_width).sum();
    assert!(
      total <= 110.1,
      "table should not exceed capped widths when no headroom is available"
    );
  }

  #[test]
  fn test_auto_layout_minimum_widths() {
    let mut structure = TableStructure::new();
    structure.column_count = 2;
    structure.columns = vec![ColumnInfo::new(0), ColumnInfo::new(1)];
    structure.cells = vec![
      CellInfo {
        index: 0,
        source_row: 0,
        source_col: 0,
        row: 0,
        col: 0,
        rowspan: 1,
        colspan: 1,
        box_index: 0,
        min_width: 50.0,
        max_width: 100.0,
        min_height: 20.0,
        bounds: Rect::ZERO,
      },
      CellInfo {
        index: 1,
        source_row: 0,
        source_col: 1,
        row: 0,
        col: 1,
        rowspan: 1,
        colspan: 1,
        box_index: 1,
        min_width: 80.0,
        max_width: 150.0,
        min_height: 20.0,
        bounds: Rect::ZERO,
      },
    ];
    structure.border_spacing = (0.0, 0.0);

    calculate_auto_layout_widths(&mut structure, 100.0); // Less than total min

    // Should use minimum widths
    assert!(structure.columns[0].computed_width >= 50.0);
    assert!(structure.columns[1].computed_width >= 80.0);
  }

  #[test]
  fn test_auto_layout_maximum_widths() {
    let mut structure = TableStructure::new();
    structure.column_count = 2;
    structure.columns = vec![ColumnInfo::new(0), ColumnInfo::new(1)];
    structure.cells = vec![
      CellInfo {
        index: 0,
        source_row: 0,
        source_col: 0,
        row: 0,
        col: 0,
        rowspan: 1,
        colspan: 1,
        box_index: 0,
        min_width: 50.0,
        max_width: 100.0,
        min_height: 20.0,
        bounds: Rect::ZERO,
      },
      CellInfo {
        index: 1,
        source_row: 0,
        source_col: 1,
        row: 0,
        col: 1,
        rowspan: 1,
        colspan: 1,
        box_index: 1,
        min_width: 80.0,
        max_width: 150.0,
        min_height: 20.0,
        bounds: Rect::ZERO,
      },
    ];
    structure.border_spacing = (0.0, 0.0);

    calculate_auto_layout_widths(&mut structure, 500.0); // More than total max

    // Should use max widths + extra distribution
    assert!(structure.columns[0].computed_width >= 100.0);
    assert!(structure.columns[1].computed_width >= 150.0);
  }

  #[test]
  fn auto_layout_uses_largest_cell_max_width_per_column() {
    let mut structure = TableStructure::new();
    structure.column_count = 2;
    structure.row_count = 2;
    structure.columns = vec![ColumnInfo::new(0), ColumnInfo::new(1)];
    structure.border_spacing = (0.0, 0.0);
    structure.cells = vec![
      CellInfo {
        index: 0,
        source_row: 0,
        source_col: 0,
        row: 0,
        col: 0,
        rowspan: 1,
        colspan: 1,
        box_index: 0,
        min_width: 20.0,
        max_width: 50.0,
        min_height: 0.0,
        bounds: Rect::ZERO,
      },
      CellInfo {
        index: 1,
        source_row: 0,
        source_col: 1,
        row: 0,
        col: 1,
        rowspan: 1,
        colspan: 1,
        box_index: 1,
        min_width: 10.0,
        max_width: 100.0,
        min_height: 0.0,
        bounds: Rect::ZERO,
      },
      CellInfo {
        index: 2,
        source_row: 1,
        source_col: 0,
        row: 1,
        col: 0,
        rowspan: 1,
        colspan: 1,
        box_index: 2,
        min_width: 20.0,
        max_width: 200.0,
        min_height: 0.0,
        bounds: Rect::ZERO,
      },
      CellInfo {
        index: 3,
        source_row: 1,
        source_col: 1,
        row: 1,
        col: 1,
        rowspan: 1,
        colspan: 1,
        box_index: 3,
        min_width: 10.0,
        max_width: 100.0,
        min_height: 0.0,
        bounds: Rect::ZERO,
      },
    ];

    calculate_auto_layout_widths(&mut structure, 300.0);

    assert!((structure.columns[0].max_width - 200.0).abs() < 0.01);
    assert!((structure.columns[1].max_width - 100.0).abs() < 0.01);
    assert!((structure.columns[0].computed_width - 200.0).abs() < 0.01);
    assert!((structure.columns[1].computed_width - 100.0).abs() < 0.01);
  }

  #[test]
  fn test_auto_layout_spanning_cell() {
    let mut structure = TableStructure::new();
    structure.column_count = 2;
    structure.columns = vec![ColumnInfo::new(0), ColumnInfo::new(1)];
    structure.cells = vec![CellInfo {
      index: 0,
      source_row: 0,
      source_col: 0,
      row: 0,
      col: 0,
      rowspan: 1,
      colspan: 2,
      box_index: 0,
      min_width: 200.0, // Spans both columns
      max_width: 300.0,
      min_height: 20.0,
      bounds: Rect::ZERO,
    }];
    structure.border_spacing = (0.0, 0.0);

    calculate_auto_layout_widths(&mut structure, 200.0);

    let total_width: f32 = structure.columns.iter().map(|c| c.computed_width).sum();
    assert!(total_width >= 200.0);
  }

  #[test]
  fn auto_layout_with_infinite_max_headroom_splits_extra_without_nan() {
    let mut structure = TableStructure::new();
    structure.column_count = 2;
    structure.columns = vec![ColumnInfo::new(0), ColumnInfo::new(1)];
    structure.cells = vec![
      CellInfo {
        index: 0,
        source_row: 0,
        source_col: 0,
        row: 0,
        col: 0,
        rowspan: 1,
        colspan: 1,
        box_index: 0,
        min_width: 10.0,
        max_width: f32::INFINITY,
        min_height: 0.0,
        bounds: Rect::ZERO,
      },
      CellInfo {
        index: 1,
        source_row: 0,
        source_col: 1,
        row: 0,
        col: 1,
        rowspan: 1,
        colspan: 1,
        box_index: 1,
        min_width: 10.0,
        max_width: f32::INFINITY,
        min_height: 0.0,
        bounds: Rect::ZERO,
      },
    ];
    structure.border_spacing = (0.0, 0.0);

    calculate_auto_layout_widths(&mut structure, 100.0);

    for col in &structure.columns {
      assert!(!col.computed_width.is_nan());
      assert!(col.computed_width.is_finite());
    }
    let total: f32 = structure.columns.iter().map(|c| c.computed_width).sum();
    assert!((total - 100.0).abs() < 0.5);
    assert!(
      (structure.columns[0].computed_width - structure.columns[1].computed_width).abs() < 0.5
    );
  }

  #[test]
  fn auto_layout_allocates_finite_headroom_before_unbounded_columns() {
    let mut structure = TableStructure::new();
    structure.column_count = 2;
    structure.columns = vec![ColumnInfo::new(0), ColumnInfo::new(1)];
    structure.cells = vec![
      CellInfo {
        index: 0,
        source_row: 0,
        source_col: 0,
        row: 0,
        col: 0,
        rowspan: 1,
        colspan: 1,
        box_index: 0,
        min_width: 10.0,
        max_width: 20.0,
        min_height: 0.0,
        bounds: Rect::ZERO,
      },
      CellInfo {
        index: 1,
        source_row: 0,
        source_col: 1,
        row: 0,
        col: 1,
        rowspan: 1,
        colspan: 1,
        box_index: 1,
        min_width: 10.0,
        max_width: f32::INFINITY,
        min_height: 0.0,
        bounds: Rect::ZERO,
      },
    ];
    structure.border_spacing = (0.0, 0.0);

    calculate_auto_layout_widths(&mut structure, 60.0);

    assert!((structure.columns[0].computed_width - 20.0).abs() < 0.5); // filled finite headroom first
    assert!(!structure.columns[1].computed_width.is_nan());
    let total: f32 = structure.columns.iter().map(|c| c.computed_width).sum();
    assert!((total - 60.0).abs() < 0.5);
  }

  #[test]
  fn auto_layout_handles_infinite_available_content() {
    let mut structure = TableStructure::new();
    structure.column_count = 2;
    structure.columns = vec![ColumnInfo::new(0), ColumnInfo::new(1)];
    structure.cells = vec![
      CellInfo {
        index: 0,
        source_row: 0,
        source_col: 0,
        row: 0,
        col: 0,
        rowspan: 1,
        colspan: 1,
        box_index: 0,
        min_width: 10.0,
        max_width: 20.0,
        min_height: 0.0,
        bounds: Rect::ZERO,
      },
      CellInfo {
        index: 1,
        source_row: 0,
        source_col: 1,
        row: 0,
        col: 1,
        rowspan: 1,
        colspan: 1,
        box_index: 1,
        min_width: 15.0,
        max_width: 25.0,
        min_height: 0.0,
        bounds: Rect::ZERO,
      },
    ];
    structure.border_spacing = (0.0, 0.0);

    calculate_auto_layout_widths(&mut structure, f32::INFINITY);

    assert!((structure.columns[0].computed_width - 20.0).abs() < 0.01);
    assert!((structure.columns[1].computed_width - 25.0).abs() < 0.01);
    assert!(structure
      .columns
      .iter()
      .all(|c| c.computed_width.is_finite()));
  }

  #[test]
  fn test_auto_layout_empty_table() {
    let mut structure = TableStructure::new();
    structure.column_count = 0;

    calculate_auto_layout_widths(&mut structure, 400.0);
    // Should not panic
  }

  // -------------------------------------------------------------------------
  // Row Height Algorithm Tests
  // -------------------------------------------------------------------------

  #[test]
  fn test_row_height_from_cells() {
    let mut structure = TableStructure::new();
    structure.row_count = 2;
    structure.rows = vec![RowInfo::new(0), RowInfo::new(1)];
    structure.cells = vec![
      CellInfo {
        index: 0,
        source_row: 0,
        source_col: 0,
        row: 0,
        col: 0,
        rowspan: 1,
        colspan: 1,
        box_index: 0,
        min_width: 50.0,
        max_width: 100.0,
        min_height: 30.0,
        bounds: Rect::ZERO,
      },
      CellInfo {
        index: 1,
        source_row: 0,
        source_col: 1,
        row: 0,
        col: 1,
        rowspan: 1,
        colspan: 1,
        box_index: 1,
        min_width: 80.0,
        max_width: 150.0,
        min_height: 50.0, // Taller cell
        bounds: Rect::ZERO,
      },
      CellInfo {
        index: 2,
        source_row: 1,
        source_col: 0,
        row: 1,
        col: 0,
        rowspan: 1,
        colspan: 1,
        box_index: 0,
        min_width: 50.0,
        max_width: 100.0,
        min_height: 25.0,
        bounds: Rect::ZERO,
      },
    ];
    structure.border_spacing = (0.0, 0.0);

    calculate_row_heights(&mut structure, None);

    // Row 0 should use the taller cell (50.0)
    assert!((structure.rows[0].computed_height - 50.0).abs() < 0.01);
    // Row 1 should use its single cell (25.0)
    assert!((structure.rows[1].computed_height - 25.0).abs() < 0.01);
  }

  #[test]
  fn test_row_height_spanning_cell() {
    let mut structure = TableStructure::new();
    structure.row_count = 2;
    structure.rows = vec![RowInfo::new(0), RowInfo::new(1)];
    structure.cells = vec![CellInfo {
      index: 0,
      source_row: 0,
      source_col: 0,
      row: 0,
      col: 0,
      rowspan: 2,
      colspan: 1,
      box_index: 0,
      min_width: 50.0,
      max_width: 100.0,
      min_height: 100.0, // Spans both rows
      bounds: Rect::ZERO,
    }];
    structure.border_spacing = (0.0, 0.0);

    calculate_row_heights(&mut structure, None);

    let total_height: f32 = structure.rows.iter().map(|r| r.computed_height).sum();
    assert!(total_height >= 100.0);
  }

  #[test]
  fn test_row_percent_with_available_height() {
    let mut structure = TableStructure::new();
    structure.row_count = 2;
    structure.rows = vec![RowInfo::new(0), RowInfo::new(1)];
    structure.rows[0].specified_height = Some(SpecifiedHeight::Percent(50.0));
    structure.rows[1].min_height = 10.0;
    structure.border_spacing = (0.0, 0.0);

    calculate_row_heights(&mut structure, Some(200.0));

    assert!((structure.rows[0].computed_height - 100.0).abs() < 0.01);
    assert!((structure.rows[1].computed_height - 100.0).abs() < 0.01);
  }

  #[test]
  fn row_groups_reordered_header_body_footer() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);

    let mut head_group_style = ComputedStyle::default();
    head_group_style.display = Display::TableHeaderGroup;

    let mut body_group_style = ComputedStyle::default();
    body_group_style.display = Display::TableRowGroup;

    let mut foot_group_style = ComputedStyle::default();
    foot_group_style.display = Display::TableFooterGroup;

    let mut row_style_head = ComputedStyle::default();
    row_style_head.display = Display::TableRow;
    row_style_head.height = Some(Length::px(10.0));

    let mut row_style_body = ComputedStyle::default();
    row_style_body.display = Display::TableRow;
    row_style_body.height = Some(Length::px(30.0));

    let mut row_style_foot = ComputedStyle::default();
    row_style_foot.display = Display::TableRow;
    row_style_foot.height = Some(Length::px(15.0));

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;

    let make_row = |style: &ComputedStyle| {
      BoxNode::new_block(
        Arc::new(style.clone()),
        FormattingContextType::Block,
        vec![BoxNode::new_block(
          Arc::new(cell_style.clone()),
          FormattingContextType::Block,
          vec![],
        )],
      )
    };

    let head_group = BoxNode::new_block(
      Arc::new(head_group_style),
      FormattingContextType::Block,
      vec![make_row(&row_style_head)],
    );
    let body_group = BoxNode::new_block(
      Arc::new(body_group_style),
      FormattingContextType::Block,
      vec![make_row(&row_style_body)],
    );
    let foot_group = BoxNode::new_block(
      Arc::new(foot_group_style),
      FormattingContextType::Block,
      vec![make_row(&row_style_foot)],
    );

    // Intentionally shuffle DOM order: footer, body, header
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![foot_group, body_group, head_group],
    );

    let tfc = TableFormattingContext::new();
    let fragment = tfc
      .layout(&table, &LayoutConstraints::definite_width(100.0))
      .expect("table layout");

    let mut tops = Vec::new();
    collect_table_cell_tops(&fragment, &mut tops);
    tops.sort_by(|a, b| a.partial_cmp(b).unwrap());

    assert_eq!(tops.len(), 3, "expected three rows");
    let h0 = tops[1] - tops[0];
    let h1 = tops[2] - tops[1];
    let h2 = fragment.bounds.height() - tops[2];

    assert!(
      h0 >= 9.0 && h0 <= 11.5,
      "header row should stay first with its height"
    );
    assert!(
      h1 >= 28.0 && h1 <= 32.0,
      "body row should remain in the middle with its height"
    );
    assert!(
      h2 >= 14.0 && h2 <= 17.0,
      "footer row should move to the end with its height"
    );
  }

  #[test]
  fn multiple_row_groups_keep_dom_order_with_header_footer_reordering() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;

    let mut head_group_style = ComputedStyle::default();
    head_group_style.display = Display::TableHeaderGroup;

    let mut body_group_style = ComputedStyle::default();
    body_group_style.display = Display::TableRowGroup;

    let mut foot_group_style = ComputedStyle::default();
    foot_group_style.display = Display::TableFooterGroup;

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;

    let make_row = |style: &ComputedStyle| {
      BoxNode::new_block(
        Arc::new(style.clone()),
        FormattingContextType::Block,
        vec![BoxNode::new_block(
          Arc::new(cell_style.clone()),
          FormattingContextType::Block,
          vec![],
        )],
      )
    };

    let head_a = BoxNode::new_block(
      Arc::new(head_group_style.clone()),
      FormattingContextType::Block,
      vec![make_row(&row_style)],
    );
    let head_b = BoxNode::new_block(
      Arc::new(head_group_style),
      FormattingContextType::Block,
      vec![make_row(&row_style)],
    );
    let body = BoxNode::new_block(
      Arc::new(body_group_style),
      FormattingContextType::Block,
      vec![make_row(&row_style)],
    );
    let loose_row = make_row(&row_style);
    let foot_a = BoxNode::new_block(
      Arc::new(foot_group_style.clone()),
      FormattingContextType::Block,
      vec![make_row(&row_style)],
    );
    let foot_b = BoxNode::new_block(
      Arc::new(foot_group_style),
      FormattingContextType::Block,
      vec![make_row(&row_style)],
    );

    // DOM order: footer, header, body, loose row, header, footer.
    // Layout order should be: header rows (DOM order), body/loose rows, footer rows (DOM order).
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![foot_a, head_a, body, loose_row, head_b, foot_b],
    );

    let structure = TableStructure::from_box_tree(&table);
    assert_eq!(structure.row_count, 6);

    let row_sources: Vec<usize> = structure.rows.iter().map(|r| r.source_index).collect();
    let cell_sources: Vec<usize> = (0..structure.row_count)
      .map(|row| structure.get_cell_at(row, 0).unwrap().source_row)
      .collect();

    let expected = vec![1, 4, 2, 3, 0, 5];
    assert_eq!(
            row_sources, expected,
            "row ordering should pull headers first, then bodies/loose rows, then footers while preserving DOM order"
        );
    assert_eq!(
      cell_sources, expected,
      "grid mapping should follow the reordered rows while keeping source indices intact"
    );
  }

  #[test]
  fn calculate_row_heights_respects_collapse_spacing() {
    let mut structure = TableStructure::new();
    structure.row_count = 2;
    structure.rows = vec![RowInfo::new(0), RowInfo::new(1)];
    structure.border_spacing = (5.0, 5.0);
    structure.border_collapse = BorderCollapse::Collapse;

    calculate_row_heights(&mut structure, Some(100.0));

    // In collapsed mode, spacing is ignored for row positioning and distribution.
    assert!((structure.rows[0].computed_height - 50.0).abs() < 0.01);
    assert!((structure.rows[1].computed_height - 50.0).abs() < 0.01);
    assert!((structure.rows[0].y_position - 0.0).abs() < 0.01);
    assert!((structure.rows[1].y_position - 50.0).abs() < 0.01);
  }

  #[test]
  fn cell_baseline_falls_back_to_bottom() {
    let fragment = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 10.0, 20.0), vec![]);
    let baseline = cell_baseline(&fragment).expect("baseline");
    assert!((baseline - 20.0).abs() < 0.01);
  }

  fn make_style(display: Display) -> Arc<ComputedStyle> {
    let mut style = ComputedStyle::default();
    style.display = display;
    Arc::new(style)
  }

  #[test]
  fn caption_default_positions_above_table() {
    let cell = BoxNode::new_block(
      make_style(Display::TableCell),
      FormattingContextType::Block,
      vec![BoxNode::new_text(
        make_style(Display::Inline),
        "data".to_string(),
      )],
    );
    let row = BoxNode::new_block(
      make_style(Display::TableRow),
      FormattingContextType::Block,
      vec![cell],
    );
    let tbody = BoxNode::new_block(
      make_style(Display::TableRowGroup),
      FormattingContextType::Block,
      vec![row],
    );
    let caption = BoxNode::new_block(
      make_style(Display::TableCaption),
      FormattingContextType::Block,
      vec![BoxNode::new_text(
        make_style(Display::Inline),
        "caption".to_string(),
      )],
    );
    let table = BoxNode::new_block(
      make_style(Display::Table),
      FormattingContextType::Table,
      vec![caption, tbody],
    );

    let fc = TableFormattingContext::with_factory(FormattingContextFactory::new());
    let fragment = fc
      .layout(
        &table,
        &LayoutConstraints::new(AvailableSpace::Definite(200.0), AvailableSpace::Indefinite),
      )
      .expect("layout");

    assert_eq!(
      fragment.children.len(),
      2,
      "wrapper should contain caption and table"
    );
    let caption_frag = &fragment.children[0];
    let table_frag = &fragment.children[1];
    assert!((fragment.bounds.width() - table_frag.bounds.width()).abs() < 0.01);
    assert!((caption_frag.bounds.width() - table_frag.bounds.width()).abs() < 0.01);
    assert!(caption_frag.bounds.y() <= 0.0 + 1e-3);
    assert!((table_frag.bounds.y() - caption_frag.bounds.height()).abs() < 0.2);
    assert!(
      (fragment.bounds.height() - (caption_frag.bounds.height() + table_frag.bounds.height()))
        .abs()
        < 0.2
    );
  }

  #[test]
  fn caption_min_width_expands_table_width() {
    let mut caption_style = ComputedStyle::default();
    caption_style.display = Display::TableCaption;
    caption_style.width = Some(Length::px(400.0));
    let caption = BoxNode::new_block(
      Arc::new(caption_style),
      FormattingContextType::Block,
      vec![BoxNode::new_text(
        make_style(Display::Inline),
        "caption".to_string(),
      )],
    );

    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;
    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    cell_style.width = Some(Length::px(10.0));

    let cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell],
    );
    let tbody = BoxNode::new_block(
      make_style(Display::TableRowGroup),
      FormattingContextType::Block,
      vec![row],
    );
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![caption, tbody],
    );

    let tfc = TableFormattingContext::new();
    let fragment = tfc
      .layout(&table, &LayoutConstraints::definite_width(100.0))
      .expect("table layout");

    assert!(
      fragment.bounds.width() >= 399.0,
      "caption min width should expand the table"
    );
  }

  #[test]
  fn caption_intrinsic_width_influences_table_intrinsic_size() {
    let mut caption_style = ComputedStyle::default();
    caption_style.display = Display::TableCaption;
    caption_style.width = Some(Length::px(120.0));
    let caption = BoxNode::new_block(
      Arc::new(caption_style),
      FormattingContextType::Block,
      vec![BoxNode::new_text(
        make_style(Display::Inline),
        "caption".to_string(),
      )],
    );

    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);

    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![caption],
    );

    let tfc = TableFormattingContext::new();
    let min = tfc
      .compute_intrinsic_inline_size(&table, IntrinsicSizingMode::MinContent)
      .expect("intrinsic width");

    assert!(
      min >= 119.0,
      "table intrinsic width should be at least the caption min width"
    );
  }

  #[test]
  fn caption_only_table_expands_to_caption_width() {
    let mut caption_style = ComputedStyle::default();
    caption_style.display = Display::TableCaption;
    caption_style.width = Some(Length::px(180.0));
    let caption = BoxNode::new_block(
      Arc::new(caption_style),
      FormattingContextType::Block,
      vec![BoxNode::new_text(
        make_style(Display::Inline),
        "caption".to_string(),
      )],
    );

    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);

    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![caption],
    );
    let tfc = TableFormattingContext::new();
    let fragment = tfc
      .layout(&table, &LayoutConstraints::definite_width(50.0))
      .expect("table layout");

    assert!(
      fragment.bounds.width() >= 179.0,
      "caption-only table should size to the caption"
    );
  }

  #[test]
  fn intrinsic_width_honors_min_and_max() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);
    table_style.min_width = Some(Length::px(120.0));
    table_style.max_width = Some(Length::px(160.0));

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    cell_style.width = Some(Length::px(300.0));
    let cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell],
    );
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let tfc = TableFormattingContext::new();
    let min = tfc
      .compute_intrinsic_inline_size(&table, IntrinsicSizingMode::MinContent)
      .expect("intrinsic");
    let max = tfc
      .compute_intrinsic_inline_size(&table, IntrinsicSizingMode::MaxContent)
      .expect("intrinsic");

    assert!(min >= 119.0, "min-content should clamp to min-width");
    assert!(max <= 160.5, "max-content should clamp to max-width");
  }

  #[test]
  fn caption_respects_table_max_width() {
    let mut caption_style = ComputedStyle::default();
    caption_style.display = Display::TableCaption;
    caption_style.width = Some(Length::px(400.0));
    let caption = BoxNode::new_block(
      Arc::new(caption_style),
      FormattingContextType::Block,
      vec![BoxNode::new_text(
        make_style(Display::Inline),
        "caption".to_string(),
      )],
    );

    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.max_width = Some(Length::px(200.0));
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);

    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![caption],
    );
    let tfc = TableFormattingContext::new();
    let fragment = tfc
      .layout(&table, &LayoutConstraints::definite_width(800.0))
      .expect("table layout");

    assert!(
      fragment.bounds.width() <= 200.1,
      "caption should not force table past its max width"
    );
  }

  #[test]
  fn caption_side_bottom_positions_after_table() {
    let mut caption_style = ComputedStyle::default();
    caption_style.display = Display::TableCaption;
    caption_style.caption_side = CaptionSide::Bottom;

    let cell = BoxNode::new_block(
      make_style(Display::TableCell),
      FormattingContextType::Block,
      vec![BoxNode::new_text(
        make_style(Display::Inline),
        "data".to_string(),
      )],
    );
    let row = BoxNode::new_block(
      make_style(Display::TableRow),
      FormattingContextType::Block,
      vec![cell],
    );
    let tbody = BoxNode::new_block(
      make_style(Display::TableRowGroup),
      FormattingContextType::Block,
      vec![row],
    );
    let caption = BoxNode::new_block(
      Arc::new(caption_style),
      FormattingContextType::Block,
      vec![BoxNode::new_text(
        make_style(Display::Inline),
        "caption".to_string(),
      )],
    );
    let table = BoxNode::new_block(
      make_style(Display::Table),
      FormattingContextType::Table,
      vec![tbody, caption],
    );

    let fc = TableFormattingContext::with_factory(FormattingContextFactory::new());
    let fragment = fc
      .layout(
        &table,
        &LayoutConstraints::new(AvailableSpace::Definite(200.0), AvailableSpace::Indefinite),
      )
      .expect("layout");

    assert_eq!(
      fragment.children.len(),
      2,
      "wrapper should contain table then caption"
    );
    let table_frag = &fragment.children[0];
    let caption_frag = &fragment.children[1];
    assert!(caption_frag.bounds.y() >= table_frag.bounds.height() - 0.2);
    assert!(
      (fragment.bounds.height() - (caption_frag.bounds.height() + table_frag.bounds.height()))
        .abs()
        < 0.2
    );
  }

  #[test]
  fn calculate_row_heights_prefers_auto_rows_for_rowspan_extra() {
    let mut structure = TableStructure::new();
    structure.row_count = 2;
    structure.rows = vec![RowInfo::new(0), RowInfo::new(1)];
    structure.rows[0].specified_height = Some(SpecifiedHeight::Fixed(50.0));
    structure.cells = vec![CellInfo {
      index: 0,
      source_row: 0,
      source_col: 0,
      row: 0,
      col: 0,
      rowspan: 2,
      colspan: 1,
      box_index: 0,
      min_width: 0.0,
      max_width: 0.0,
      min_height: 120.0,
      bounds: Rect::ZERO,
    }];
    structure.border_spacing = (0.0, 0.0);

    calculate_row_heights(&mut structure, None);

    assert!((structure.rows[0].computed_height - 50.0).abs() < 0.01);
    assert!((structure.rows[1].computed_height - 70.0).abs() < 0.01);
  }

  #[test]
  fn calculate_row_heights_with_percent_row_and_rowspan_extra() {
    let mut structure = TableStructure::new();
    structure.row_count = 2;
    structure.rows = vec![RowInfo::new(0), RowInfo::new(1)];
    structure.rows[0].specified_height = Some(SpecifiedHeight::Percent(50.0));
    structure.cells = vec![CellInfo {
      index: 0,
      source_row: 0,
      source_col: 0,
      row: 0,
      col: 0,
      rowspan: 2,
      colspan: 1,
      box_index: 0,
      min_width: 0.0,
      max_width: 0.0,
      min_height: 160.0,
      bounds: Rect::ZERO,
    }];
    structure.border_spacing = (0.0, 0.0);

    calculate_row_heights(&mut structure, Some(200.0));

    assert!((structure.rows[0].computed_height - 100.0).abs() < 0.01);
    assert!((structure.rows[1].computed_height - 100.0).abs() < 0.01);
  }

  #[test]
  fn calculate_row_heights_distributes_rowspan_extra_proportionally() {
    let mut structure = TableStructure::new();
    structure.row_count = 2;
    structure.rows = vec![RowInfo::new(0), RowInfo::new(1)];
    structure.rows[0].min_height = 10.0;
    structure.rows[1].min_height = 30.0;
    structure.cells = vec![CellInfo {
      index: 0,
      source_row: 0,
      source_col: 0,
      row: 0,
      col: 0,
      rowspan: 2,
      colspan: 1,
      box_index: 0,
      min_width: 0.0,
      max_width: 0.0,
      min_height: 120.0,
      bounds: Rect::ZERO,
    }];
    structure.border_spacing = (0.0, 0.0);

    calculate_row_heights(&mut structure, None);

    // Extra 80 should distribute in 1:3 ratio from initial 10:30 heights → 20/60 split.
    assert!((structure.rows[0].computed_height - 30.0).abs() < 0.01);
    assert!((structure.rows[1].computed_height - 90.0).abs() < 0.01);
  }

  #[test]
  fn extra_table_height_respects_row_max_when_distributing() {
    let mut structure = TableStructure::new();
    structure.border_collapse = BorderCollapse::Separate;
    structure.border_spacing = (0.0, 0.0);
    structure.row_count = 2;
    structure.rows = vec![RowInfo::new(0), RowInfo::new(1)];
    structure.rows[0].min_height = 20.0;
    structure.rows[1].min_height = 20.0;
    structure.rows[0].author_max_height = Some(SpecifiedHeight::Fixed(60.0));

    calculate_row_heights(&mut structure, Some(200.0));

    assert!(
      structure.rows[0].computed_height <= 61.0,
      "row 0 should respect its max cap when filling extra space"
    );
    assert!(
      structure.rows[1].computed_height >= 130.0,
      "remaining extra space should flow to uncapped rows"
    );
  }

  #[test]
  fn calculate_row_heights_respects_headroom_for_rowspans() {
    let mut structure = TableStructure::new();
    structure.row_count = 2;
    structure.rows = vec![RowInfo::new(0), RowInfo::new(1)];
    structure.rows[0].min_height = 20.0;
    structure.rows[1].min_height = 20.0;
    structure.rows[0].author_max_height = Some(SpecifiedHeight::Fixed(60.0));
    structure.cells = vec![CellInfo {
      index: 0,
      source_row: 0,
      source_col: 0,
      row: 0,
      col: 0,
      rowspan: 2,
      colspan: 1,
      box_index: 0,
      min_width: 0.0,
      max_width: 0.0,
      min_height: 200.0,
      bounds: Rect::ZERO,
    }];
    structure.border_spacing = (0.0, 0.0);

    calculate_row_heights(&mut structure, None);

    // Row 0 caps at its max; the remaining height flows into row 1.
    assert!((structure.rows[0].computed_height - 60.0).abs() < 0.5);
    assert!(structure.rows[1].computed_height > 120.0);
  }

  #[test]
  fn calculate_row_heights_spreads_rowspan_across_specified_and_auto_rows() {
    let mut structure = TableStructure::new();
    structure.row_count = 2;
    structure.rows = vec![RowInfo::new(0), RowInfo::new(1)];
    structure.rows[0].specified_height = Some(SpecifiedHeight::Fixed(50.0));
    structure.cells = vec![CellInfo {
      index: 0,
      source_row: 0,
      source_col: 0,
      row: 0,
      col: 0,
      rowspan: 2,
      colspan: 1,
      box_index: 0,
      min_width: 0.0,
      max_width: 0.0,
      min_height: 150.0,
      bounds: Rect::ZERO,
    }];
    structure.border_spacing = (0.0, 0.0);

    calculate_row_heights(&mut structure, None);

    assert!(
      structure.rows[0].computed_height > 70.0 && structure.rows[0].computed_height < 80.0,
      "row 0 should share the spanning height despite its fixed size (got {:.2})",
      structure.rows[0].computed_height
    );
    assert!(
      structure.rows[1].computed_height > 70.0 && structure.rows[1].computed_height < 80.0,
      "row 1 should share the spanning height (got {:.2})",
      structure.rows[1].computed_height
    );
  }

  #[test]
  fn calculate_row_heights_respects_max_caps_when_distributing_rowspan() {
    let mut structure = TableStructure::new();
    structure.row_count = 2;
    structure.rows = vec![RowInfo::new(0), RowInfo::new(1)];
    structure.rows[0].min_height = 20.0;
    structure.rows[0].author_max_height = Some(SpecifiedHeight::Fixed(40.0));
    structure.rows[1].min_height = 0.0;
    structure.border_spacing = (0.0, 0.0);
    structure.cells = vec![CellInfo {
      index: 0,
      source_row: 0,
      source_col: 0,
      row: 0,
      col: 0,
      rowspan: 2,
      colspan: 1,
      box_index: 0,
      min_width: 0.0,
      max_width: 0.0,
      min_height: 120.0,
      bounds: Rect::ZERO,
    }];

    calculate_row_heights(&mut structure, None);

    assert!(
      structure.rows[0].computed_height <= 40.01,
      "row 0 should respect its max cap"
    );
    assert!(
      structure.rows[1].computed_height >= 79.0,
      "remaining spanning height should flow to uncapped rows"
    );
  }

  fn distribute_rowspan_targets_vec(
    targets: &[usize],
    row_heights: &mut [f32],
    rows: &[RowInfo],
    remaining: f32,
    percent_base: Option<f32>,
    row_floor: &dyn Fn(usize, f32) -> f32,
    combine: &dyn Fn(f32, f32) -> f32,
  ) {
    if targets.is_empty() || remaining <= 0.0 {
      return;
    }

    let mut weights = Vec::with_capacity(targets.len());
    let mut caps = Vec::with_capacity(targets.len());
    for &idx in targets {
      let current = *row_heights.get(idx).unwrap_or(&0.0);
      let base = row_floor(idx, current);
      let (_, max_opt) = resolve_row_min_max(&rows[idx], percent_base);
      let headroom = max_opt.map(|m| (m - base).max(0.0));
      let weight = match headroom {
        Some(h) if h > 0.0 => h,
        Some(_) => 0.0,
        None => 1.0, // Uncapped rows share spanning contributions evenly.
      };
      weights.push(weight);
      caps.push(headroom);
    }

    let mut total_weight: f32 = weights.iter().sum();
    if total_weight <= 0.0 {
      total_weight = targets.len() as f32;
      weights.fill(1.0);
    }

    let mut shares = vec![0.0; targets.len()];
    let mut allocated = 0.0;
    for (((share, weight), cap), _) in shares
      .iter_mut()
      .zip(weights.iter())
      .zip(caps.iter())
      .zip(targets.iter())
    {
      let mut portion = if total_weight > 0.0 {
        remaining * (*weight / total_weight)
      } else {
        0.0
      };
      if let Some(c) = cap {
        portion = portion.min(*c);
      }
      *share = portion;
      allocated += portion;
    }

    let leftover = (remaining - allocated).max(0.0);
    if leftover > 0.01 {
      let mut flexible = Vec::new();
      for (idx, cap) in caps.iter().enumerate() {
        let cap_left = cap
          .map(|c| (c - shares[idx]).max(0.0))
          .unwrap_or(f32::INFINITY);
        if cap_left > 0.0 {
          flexible.push((idx, cap_left));
        }
      }

      if !flexible.is_empty() {
        let finite_sum: f32 = flexible
          .iter()
          .filter_map(|(_, cap_left)| {
            if *cap_left == f32::INFINITY {
              None
            } else {
              Some(*cap_left)
            }
          })
          .sum();
        let unbounded: Vec<_> = flexible
          .iter()
          .filter(|(_, cap_left)| *cap_left == f32::INFINITY)
          .collect();

        if finite_sum > 0.0 {
          for (idx, cap_left) in flexible {
            if cap_left == f32::INFINITY {
              continue;
            }
            let extra = leftover * (cap_left / finite_sum);
            shares[idx] += extra.min(cap_left);
          }
        } else if !unbounded.is_empty() {
          let per = leftover / unbounded.len() as f32;
          for (idx_ref, _) in unbounded {
            shares[*idx_ref] += per;
          }
        }
      }
    }

    for (target_idx, share) in targets.iter().zip(shares.iter()) {
      if let Some(slot) = row_heights.get_mut(*target_idx) {
        *slot = combine(*slot, *share);
      }
    }
  }

  #[test]
  fn rowspan_range_distribution_matches_vec_baseline() {
    let mut rows: Vec<RowInfo> = (0..4).map(RowInfo::new).collect();
    rows[0].min_height = 10.0;
    rows[0].author_max_height = Some(SpecifiedHeight::Fixed(50.0));
    rows[1].min_height = 15.0;
    rows[2].min_height = 20.0;
    rows[2].author_max_height = Some(SpecifiedHeight::Fixed(60.0));
    rows[3].min_height = 12.0;
    rows[3].author_max_height = Some(SpecifiedHeight::Fixed(80.0));

    let percent_base = None;
    let row_floor = |idx: usize, current: f32| -> f32 {
      let row = &rows[idx];
      let mut floor = current;
      if let Some((Some(min_len), _)) = Some(resolve_row_min_max(row, percent_base)) {
        floor = floor.max(min_len);
      }
      if let Some(spec) = row.specified_height {
        match spec {
          SpecifiedHeight::Fixed(h) => floor = floor.max(h),
          SpecifiedHeight::Percent(pct) => {
            if let Some(base) = percent_base {
              floor = floor.max((pct / 100.0) * base);
            }
          }
          SpecifiedHeight::Auto => {}
        }
      }
      floor
    };
    let row_floor_fn = |idx: usize, _current: f32| -> f32 { row_floor(idx, 0.0) };
    let combine = |current: f32, share: f32| current + share;

    let spans = vec![
      (0usize, 3usize, 140.0),
      (1usize, 3usize, 180.0),
      (0usize, 4usize, 260.0),
    ];
    let v_spacing = 0.0f32;

    let mut range_heights: Vec<f32> = rows.iter().map(|r| r.min_height).collect();
    let mut vec_heights = range_heights.clone();

    for (start, span_len, cell_height) in spans {
      let end = (start + span_len).min(rows.len());
      if end <= start {
        continue;
      }
      let span_height = (cell_height - v_spacing * span_len.saturating_sub(1) as f32).max(0.0);

      let current_total_range: f32 = range_heights[start..end].iter().sum();
      let remaining_range = (span_height - current_total_range).max(0.0);
      distribute_rowspan_targets_range(
        start,
        end,
        &mut range_heights,
        &rows,
        remaining_range,
        percent_base,
        &row_floor_fn,
        &combine,
      );

      let current_total_vec: f32 = vec_heights[start..end].iter().sum();
      let remaining_vec = (span_height - current_total_vec).max(0.0);
      let targets: Vec<usize> = (start..end).collect();
      distribute_rowspan_targets_vec(
        &targets,
        &mut vec_heights,
        &rows,
        remaining_vec,
        percent_base,
        &row_floor_fn,
        &combine,
      );
    }

    assert_eq!(range_heights.len(), vec_heights.len());
    for (idx, (range_h, vec_h)) in range_heights.iter().zip(vec_heights.iter()).enumerate() {
      assert!(
        (range_h - vec_h).abs() < 0.01,
        "row {} height diverged: range {:.2} vs vec {:.2}",
        idx,
        range_h,
        vec_h
      );
    }
  }

  #[test]
  fn table_cell_child_margins_do_not_collapse_with_cell() {
    // Table cells establish a BFC; child block margins should not collapse through the cell,
    // so the cell height includes the child's top and bottom margins.
    let mut child_style = ComputedStyle::default();
    child_style.display = Display::Block;
    child_style.height = Some(Length::px(20.0));
    child_style.margin_top = Some(Length::px(10.0));
    child_style.margin_bottom = Some(Length::px(10.0));
    let child = BoxNode::new_block(Arc::new(child_style), FormattingContextType::Block, vec![]);

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    let cell = BoxNode::new_block(
      Arc::new(cell_style),
      FormattingContextType::Block,
      vec![child],
    );

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell.clone()],
    );

    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let tfc = TableFormattingContext::new();
    let fragment = tfc
      .layout(&table, &LayoutConstraints::definite(200.0, 200.0))
      .expect("table layout");

    fn find_fragment_by_box_id<'a>(
      frag: &'a FragmentNode,
      target: usize,
    ) -> Option<&'a FragmentNode> {
      match frag.content {
        FragmentContent::Block { box_id } | FragmentContent::Inline { box_id, .. } => {
          if box_id == Some(target) {
            return Some(frag);
          }
        }
        _ => {}
      }
      for child in frag.children.iter() {
        if let Some(found) = find_fragment_by_box_id(child, target) {
          return Some(found);
        }
      }
      None
    }

    let cell_fragment = find_fragment_by_box_id(&fragment, cell.id).expect("cell fragment");
    assert!(
      (cell_fragment.bounds.height() - 40.0).abs() < 0.1,
      "cell height should include child margins (got {:.2})",
      cell_fragment.bounds.height()
    );
  }

  #[test]
  fn percent_rows_use_available_height_when_table_auto() {
    let mut structure = TableStructure::new();
    structure.border_collapse = BorderCollapse::Separate;
    structure.border_spacing = (0.0, 0.0);
    structure.row_count = 2;
    structure.rows = vec![RowInfo::new(0), RowInfo::new(1)];
    structure.rows[0].specified_height = Some(SpecifiedHeight::Percent(50.0));
    structure.rows[1].specified_height = Some(SpecifiedHeight::Auto);

    // Without an explicit table height, percentage rows should not resolve against an available height.
    calculate_row_heights(&mut structure, None);

    assert!(structure.rows[0].computed_height < 5.0);
    assert!(structure.rows[1].computed_height < 5.0);
  }

  #[test]
  fn percent_rows_scale_down_when_over_budget() {
    let mut structure = TableStructure::new();
    structure.border_collapse = BorderCollapse::Separate;
    structure.border_spacing = (0.0, 0.0);
    structure.row_count = 2;
    structure.rows = vec![RowInfo::new(0), RowInfo::new(1)];
    structure.rows[0].specified_height = Some(SpecifiedHeight::Percent(80.0));
    structure.rows[1].specified_height = Some(SpecifiedHeight::Percent(80.0));

    calculate_row_heights(&mut structure, Some(100.0));

    // Both rows should be scaled down to fit within 100px.
    let total = structure.rows[0].computed_height + structure.rows[1].computed_height;
    assert!(total <= 101.0);
    assert!(structure.rows[0].computed_height > 0.0);
    assert!(structure.rows[1].computed_height > 0.0);
  }

  #[test]
  fn percent_rows_shrink_by_headroom() {
    let mut structure = TableStructure::new();
    structure.border_collapse = BorderCollapse::Separate;
    structure.border_spacing = (0.0, 0.0);
    structure.row_count = 2;
    structure.rows = vec![RowInfo::new(0), RowInfo::new(1)];
    structure.rows[0].specified_height = Some(SpecifiedHeight::Percent(60.0));
    structure.rows[1].specified_height = Some(SpecifiedHeight::Percent(60.0));
    structure.rows[0].min_height = 50.0; // less headroom to shrink

    calculate_row_heights(&mut structure, Some(100.0));

    // Row 0 should stay nearer its cap; row 1 absorbs most reduction.
    assert!(structure.rows[0].computed_height > 55.0);
    assert!(structure.rows[1].computed_height < 45.0);
    let total = structure.rows[0].computed_height + structure.rows[1].computed_height;
    assert!((total - 100.0).abs() < 0.5);
  }

  #[test]
  fn row_group_height_distributes_minimum_across_rows() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);

    let mut group_style = ComputedStyle::default();
    group_style.display = Display::TableRowGroup;
    group_style.height = Some(Length::px(100.0));

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    cell_style.min_height = Some(Length::px(10.0));

    let row0_cell = BoxNode::new_block(
      Arc::new(cell_style.clone()),
      FormattingContextType::Block,
      vec![],
    );
    let row1_cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
    let row0 = BoxNode::new_block(
      Arc::new(row_style.clone()),
      FormattingContextType::Block,
      vec![row0_cell],
    );
    let row1 = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![row1_cell],
    );
    let tbody = BoxNode::new_block(
      Arc::new(group_style),
      FormattingContextType::Block,
      vec![row0, row1],
    );
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![tbody],
    );

    let fc = TableFormattingContext::new();
    let fragment = fc
      .layout(&table, &LayoutConstraints::definite_width(120.0))
      .expect("table layout");

    let mut tops = Vec::new();
    collect_table_cell_tops(&fragment, &mut tops);
    tops.sort_by(|a, b| a.partial_cmp(b).unwrap());

    assert!(
      fragment.bounds.height() >= 99.0,
      "row group height should contribute to table height"
    );
    assert!(tops.len() >= 2);
    let row0_height = tops[1] - tops[0];
    let row1_height = fragment.bounds.height() - tops[1];
    assert!(
      row0_height > 30.0 && row1_height > 30.0,
      "extra height should be shared across rows"
    );
  }

  #[test]
  fn row_group_percent_height_uses_table_height() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);
    table_style.height = Some(Length::px(200.0));

    let mut group_style = ComputedStyle::default();
    group_style.display = Display::TableRowGroup;
    group_style.height = Some(Length::percent(50.0));

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    cell_style.min_height = Some(Length::px(5.0));

    let row0_cell = BoxNode::new_block(
      Arc::new(cell_style.clone()),
      FormattingContextType::Block,
      vec![],
    );
    let row1_cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
    let row0 = BoxNode::new_block(
      Arc::new(row_style.clone()),
      FormattingContextType::Block,
      vec![row0_cell],
    );
    let row1 = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![row1_cell],
    );
    let tbody = BoxNode::new_block(
      Arc::new(group_style),
      FormattingContextType::Block,
      vec![row0, row1],
    );
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![tbody],
    );

    let fc = TableFormattingContext::new();
    let fragment = fc
      .layout(&table, &LayoutConstraints::definite_width(160.0))
      .expect("table layout");

    let mut tops = Vec::new();
    collect_table_cell_tops(&fragment, &mut tops);
    tops.sort_by(|a, b| a.partial_cmp(b).unwrap());

    assert!(
      fragment.bounds.height() >= 199.0,
      "explicit table height should remain intact"
    );
    assert!(tops.len() >= 2);
    let rows_sum = (tops[1] - tops[0]) + (fragment.bounds.height() - tops[1]);
    assert!(
      rows_sum >= 99.0,
      "row group percentage height should resolve against the table height"
    );
  }

  #[test]
  fn row_group_max_caps_extra_table_height() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);
    table_style.height = Some(Length::px(200.0));

    let mut capped_group_style = ComputedStyle::default();
    capped_group_style.display = Display::TableRowGroup;
    capped_group_style.max_height = Some(Length::px(80.0));

    let mut free_group_style = ComputedStyle::default();
    free_group_style.display = Display::TableRowGroup;

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    cell_style.min_height = Some(Length::px(10.0));

    let make_row = |cell_style: &ComputedStyle| {
      BoxNode::new_block(
        Arc::new(row_style.clone()),
        FormattingContextType::Block,
        vec![BoxNode::new_block(
          Arc::new(cell_style.clone()),
          FormattingContextType::Block,
          vec![],
        )],
      )
    };

    let row0 = make_row(&cell_style);
    let row1 = make_row(&cell_style);
    let capped_group = BoxNode::new_block(
      Arc::new(capped_group_style),
      FormattingContextType::Block,
      vec![row0, row1],
    );

    let row2 = make_row(&cell_style);
    let row3 = make_row(&cell_style);
    let free_group = BoxNode::new_block(
      Arc::new(free_group_style),
      FormattingContextType::Block,
      vec![row2, row3],
    );

    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![capped_group, free_group],
    );

    let fc = TableFormattingContext::new();
    let fragment = fc
      .layout(&table, &LayoutConstraints::definite_width(160.0))
      .expect("table layout");

    let mut tops = Vec::new();
    collect_table_cell_tops(&fragment, &mut tops);
    tops.sort_by(|a, b| a.partial_cmp(b).unwrap());
    assert_eq!(tops.len(), 4, "expected four rows");
    let h0 = tops[1] - tops[0];
    let h1 = tops[2] - tops[1];
    let h2 = tops[3] - tops[2];
    let h3 = fragment.bounds.height() - tops[3];

    let capped_sum = h0 + h1;
    let free_sum = h2 + h3;
    assert!(
      capped_sum <= 81.0,
      "capped group should not exceed its max height: {capped_sum}"
    );
    assert!(
      free_sum >= 110.0,
      "remaining extra height should flow into uncapped rows"
    );
    assert!(
      (fragment.bounds.height() - 200.0).abs() < 0.5,
      "table height should stay at 200px"
    );
  }

  #[test]
  fn column_percentages_normalize_when_over_100() {
    let mut constraints = vec![
      ColumnConstraints::percentage(70.0, 0.0, 100.0),
      ColumnConstraints::percentage(50.0, 0.0, 100.0),
    ];
    let tfc = TableFormattingContext::new();
    // Percentages should remain authored even when over 100%.
    tfc.normalize_percentage_constraints(&mut constraints, Some(200.0));

    let sum: f32 = constraints.iter().filter_map(|c| c.percentage).sum();
    assert!((sum - 120.0).abs() < 0.01);
  }

  #[test]
  fn spanning_max_width_pushes_columns() {
    let mut structure = TableStructure::new();
    structure.column_count = 2;
    structure.columns = vec![ColumnInfo::new(0), ColumnInfo::new(1)];
    structure.cells = vec![CellInfo {
      index: 0,
      source_row: 0,
      source_col: 0,
      row: 0,
      col: 0,
      rowspan: 1,
      colspan: 2,
      box_index: 0,
      min_width: 0.0,
      max_width: 200.0,
      min_height: 0.0,
      bounds: Rect::ZERO,
    }];

    calculate_auto_layout_widths(&mut structure, 300.0);

    let total_max = structure.columns.iter().map(|c| c.max_width).sum::<f32>();
    assert!(
      total_max >= 200.0 - 0.01,
      "spanning max width should raise spanned columns (got {total_max})"
    );
    assert!(structure.columns[0].max_width > 0.0 && structure.columns[1].max_width > 0.0);
  }

  #[test]
  fn extra_table_height_distributes_proportionally_to_auto_rows() {
    let mut structure = TableStructure::new();
    structure.row_count = 2;
    structure.rows = vec![RowInfo::new(0), RowInfo::new(1)];
    // Give rows different intrinsic heights so proportional distribution is observable.
    structure.rows[0].min_height = 10.0;
    structure.rows[1].min_height = 30.0;
    structure.border_spacing = (0.0, 0.0);

    calculate_row_heights(&mut structure, Some(100.0));

    // Sum to table height.
    let total = structure.rows[0].computed_height + structure.rows[1].computed_height;
    assert!((total - 100.0).abs() < 0.01);
    // Extra space (60px) apportioned in 1:3 ratio -> 10+15 vs 30+45.
    assert!((structure.rows[0].computed_height - 25.0).abs() < 0.5);
    assert!((structure.rows[1].computed_height - 75.0).abs() < 0.5);
  }

  #[test]
  fn extra_table_height_skips_fixed_and_percent_rows() {
    let mut structure = TableStructure::new();
    structure.row_count = 3;
    structure.rows = vec![RowInfo::new(0), RowInfo::new(1), RowInfo::new(2)];
    structure.rows[0].specified_height = Some(SpecifiedHeight::Fixed(20.0));
    structure.rows[1].specified_height = Some(SpecifiedHeight::Percent(50.0));
    structure.rows[2].min_height = 10.0;
    structure.border_spacing = (0.0, 0.0);

    calculate_row_heights(&mut structure, Some(200.0));

    // Fixed row stays at 20, percent row at 100; remaining 80 goes to the auto row.
    assert!((structure.rows[0].computed_height - 20.0).abs() < 0.01);
    assert!((structure.rows[1].computed_height - 100.0).abs() < 0.01);
    assert!((structure.rows[2].computed_height - 80.0).abs() < 0.5);
  }

  #[test]
  fn fixed_height_table_accounts_for_padding_and_spacing() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.height = Some(Length::px(100.0));
    table_style.padding_top = Length::px(10.0);
    table_style.padding_bottom = Length::px(10.0);
    table_style.border_spacing_horizontal = Length::px(5.0);
    table_style.border_spacing_vertical = Length::px(5.0);

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    cell_style.min_height = Some(Length::px(40.0));

    let make_row = |cell_style: &ComputedStyle| {
      BoxNode::new_block(
        Arc::new(row_style.clone()),
        FormattingContextType::Block,
        vec![BoxNode::new_block(
          Arc::new(cell_style.clone()),
          FormattingContextType::Block,
          vec![],
        )],
      )
    };

    let row0 = make_row(&cell_style);
    let row1 = make_row(&cell_style);
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row0, row1],
    );

    let fc = TableFormattingContext::new();
    let fragment = fc
      .layout(&table, &LayoutConstraints::definite(200.0, 200.0))
      .expect("table layout");
    assert!(
      (fragment.bounds.height() - 100.0).abs() < 0.01,
      "table height should honor the specified 100px"
    );

    fn collect_cell_bottoms(fragment: &FragmentNode, bottoms: &mut Vec<f32>) {
      if fragment
        .style
        .as_ref()
        .map(|s| matches!(s.display, Display::TableCell))
        .unwrap_or(false)
      {
        bottoms.push(fragment.bounds.y() + fragment.bounds.height());
      }
      for child in fragment.children.iter() {
        collect_cell_bottoms(child, bottoms);
      }
    }

    let mut bottoms = Vec::new();
    collect_cell_bottoms(&fragment, &mut bottoms);
    assert!(!bottoms.is_empty());
    let max_bottom = bottoms.into_iter().fold(f32::NEG_INFINITY, f32::max);
    assert!(
      max_bottom <= fragment.bounds.height() + 0.01,
      "cells must fit inside the specified table height (max bottom {:.2} vs height {:.2})",
      max_bottom,
      fragment.bounds.height()
    );
  }

  #[test]
  fn spanning_percentage_width_splits_into_column_percentages() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut span_cell_style = ComputedStyle::default();
    span_cell_style.display = Display::TableCell;
    span_cell_style.width = Some(Length::percent(60.0));

    let mut auto_cell_style = ComputedStyle::default();
    auto_cell_style.display = Display::TableCell;

    let span_cell = BoxNode::new_block(
      Arc::new(span_cell_style),
      FormattingContextType::Block,
      vec![],
    )
    .with_debug_info(DebugInfo::new(Some("td".to_string()), None, vec![]).with_spans(2, 1));
    let auto_cell = BoxNode::new_block(
      Arc::new(auto_cell_style),
      FormattingContextType::Block,
      vec![],
    );
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![span_cell, auto_cell],
    );
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let tfc = TableFormattingContext::new();
    let structure = TableStructure::from_box_tree(&table);
    let mut constraints = vec![ColumnConstraints::new(0.0, 0.0); structure.column_count];
    let percent_base = Some(200.0);
    tfc.populate_column_constraints(
      &table,
      &structure,
      &mut constraints,
      DistributionMode::Auto,
      percent_base,
      &StyleOverrideCache::default(),
    );
    tfc.normalize_percentage_constraints(&mut constraints, percent_base);

    let percents: Vec<f32> = constraints
      .iter()
      .map(|c| c.percentage.unwrap_or(0.0))
      .collect();
    assert_eq!(percents.len(), 3);
    assert!((percents[0] - 30.0).abs() < 0.01);
    assert!((percents[1] - 30.0).abs() < 0.01);
    assert!(percents[2] < 0.01);

    let min_sum: f32 = constraints.iter().map(|c| c.min_width).sum();
    assert!(
      min_sum < 20.0,
      "percentage span should not lock columns to absolute minima (got {min_sum})"
    );
  }

  #[test]
  fn wide_caption_expands_table_wrapper() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.width = Some(Length::px(100.0));

    let mut caption_style = ComputedStyle::default();
    caption_style.display = Display::TableCaption;
    caption_style.width = Some(Length::px(220.0));

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;
    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;

    let cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell],
    );
    let caption = BoxNode::new_block(
      Arc::new(caption_style),
      FormattingContextType::Block,
      vec![BoxNode::new_text(
        Arc::new(ComputedStyle::default()),
        "caption".to_string(),
      )],
    );
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![caption, row],
    );

    let fc = TableFormattingContext::new();
    let fragment = fc
      .layout(&table, &LayoutConstraints::definite(400.0, 400.0))
      .expect("table layout");

    // Wrapper should expand to at least the caption width.
    assert!(
      fragment.bounds.width() >= 219.0,
      "wrapper should be wide enough for the caption (got {:.2})",
      fragment.bounds.width()
    );
    // Table box stays at its authored width (not inflated by the caption).
    let table_child = fragment.children.iter().find(|child| {
      child
        .style
        .as_ref()
        .map(|s| matches!(s.display, Display::Table))
        .unwrap_or(false)
    });
    let table_child = table_child.expect("table child");
    assert!(
      (table_child.bounds.width() - 100.0).abs() < 0.1,
      "table box width should stay at 100px (got {:.2})",
      table_child.bounds.width()
    );
  }

  #[test]
  fn test_vertical_align_with_rowspan() {
    let mut structure = TableStructure::new();
    structure.row_count = 2;
    structure.rows = vec![RowInfo::new(0), RowInfo::new(1)];
    structure.cells = vec![
      CellInfo {
        index: 0,
        source_row: 0,
        source_col: 0,
        row: 0,
        col: 0,
        rowspan: 2,
        colspan: 1,
        box_index: 0,
        min_width: 50.0,
        max_width: 100.0,
        min_height: 40.0, // spanning cell
        bounds: Rect::ZERO,
      },
      CellInfo {
        index: 1,
        source_row: 0,
        source_col: 1,
        row: 0,
        col: 1,
        rowspan: 1,
        colspan: 1,
        box_index: 1,
        min_width: 50.0,
        max_width: 100.0,
        min_height: 20.0,
        bounds: Rect::ZERO,
      },
      CellInfo {
        index: 2,
        source_row: 1,
        source_col: 1,
        row: 1,
        col: 1,
        rowspan: 1,
        colspan: 1,
        box_index: 2,
        min_width: 50.0,
        max_width: 100.0,
        min_height: 20.0,
        bounds: Rect::ZERO,
      },
    ];
    structure.border_spacing = (0.0, 0.0);

    calculate_row_heights(&mut structure, None);

    // Both rows should have positive heights and the spanning cell shouldn't collapse row 1 to zero.
    assert!(structure.rows[0].computed_height > 0.0);
    assert!(structure.rows[1].computed_height > 0.0);
    assert!((structure.rows[0].computed_height - structure.rows[1].computed_height).abs() < 25.0);
  }

  #[test]
  fn baseline_height_computation_skips_rowspanning_cells() {
    // Build a two-row table with a tall rowspanning cell; row 0 metrics should not be inflated.
    let mut structure = TableStructure::new();
    structure.row_count = 2;
    structure.rows = vec![RowInfo::new(0), RowInfo::new(1)];
    structure.border_spacing = (0.0, 0.0);
    structure.cells = vec![
      // Regular cell in row 0
      CellInfo {
        index: 0,
        source_row: 0,
        source_col: 0,
        row: 0,
        col: 0,
        rowspan: 1,
        colspan: 1,
        box_index: 0,
        min_width: 50.0,
        max_width: 100.0,
        min_height: 20.0,
        bounds: Rect::ZERO,
      },
      // Tall cell spanning both rows
      CellInfo {
        index: 1,
        source_row: 0,
        source_col: 1,
        row: 0,
        col: 1,
        rowspan: 2,
        colspan: 1,
        box_index: 1,
        min_width: 50.0,
        max_width: 100.0,
        min_height: 200.0,
        bounds: Rect::ZERO,
      },
    ];

    calculate_row_heights(&mut structure, None);

    let row0 = structure.rows[0].clone();
    let row1 = structure.rows[1].clone();

    assert!(
      row0.computed_height < 140.0,
      "row 0 height should not be over-inflated by spanning cell (got {:.2})",
      row0.computed_height
    );
    assert!(
      row1.computed_height > 90.0,
      "row 1 height should reflect spanning cell contribution (got {:.2})",
      row1.computed_height
    );
    assert!(
      row1.computed_height + 30.0 >= row0.computed_height,
      "row 0 height should stay near the spanning share: row0 {:.2} row1 {:.2}",
      row0.computed_height,
      row1.computed_height
    );
  }

  #[test]
  fn test_vertical_align_baseline_rowspan_keeps_rows_visible() {
    let mut structure = TableStructure::new();
    structure.row_count = 2;
    structure.rows = vec![RowInfo::new(0), RowInfo::new(1)];
    structure.cells = vec![CellInfo {
      index: 0,
      source_row: 0,
      source_col: 0,
      row: 0,
      col: 0,
      rowspan: 2,
      colspan: 1,
      box_index: 0,
      min_width: 50.0,
      max_width: 100.0,
      min_height: 40.0,
      bounds: Rect::ZERO,
    }];
    structure.border_spacing = (0.0, 0.0);

    calculate_row_heights(&mut structure, None);
    // Rowspanning baseline-aligned cells should still leave both rows with usable heights.
    assert!(structure.rows[0].computed_height >= 0.0);
    assert!(structure.rows[1].computed_height >= 0.0);
  }

  #[test]
  fn spanning_baseline_prefers_later_rows_for_descent() {
    let row_heights = vec![5.0, 15.0];
    // Cell height 30, baseline at 10 -> 20px below-baseline content.
    // The second row already provides 15px, so only 5px of descent remains in the first row.
    let (top, bottom) = spanning_baseline_allocation(30.0, 10.0, 0, 2, &row_heights);
    assert!((top - 10.0).abs() < 0.01);
    assert!((bottom - 5.0).abs() < 0.01);
  }

  #[test]
  fn spanning_baseline_avoids_overcharging_first_row() {
    let row_heights = vec![8.0, 25.0];
    // Below-baseline content fits entirely in later rows; first row only keeps the baseline.
    let (top, bottom) = spanning_baseline_allocation(30.0, 12.0, 0, 2, &row_heights);
    assert!((top - 12.0).abs() < 0.01);
    assert!(bottom.abs() < 0.01);
  }

  #[test]
  fn row_baseline_accumulates_baseline_cells() {
    let mut row = RowMetrics::new(30.0);
    row.max_cell_height = 0.0;
    let row_height = 30.0;

    let apply_baseline_cell = |row: &mut RowMetrics, baseline: f32, cell_height: f32| {
      row.max_cell_height = row.max_cell_height.max(cell_height);
      let clamped = baseline.min(row_height);
      let bottom = (row_height - clamped).max(0.0);
      row.has_baseline = true;
      row.baseline_top = row.baseline_top.max(clamped);
      row.baseline_bottom = row.baseline_bottom.max(bottom);
      row.max_cell_height = row.max_cell_height.max(clamped + bottom);
    };

    apply_baseline_cell(&mut row, 10.0, 30.0);
    // Additional baseline-aligned cells grow the ascent/descent maxima instead of being ignored.
    apply_baseline_cell(&mut row, 5.0, 30.0);

    assert!((row.baseline_top - 10.0).abs() < 0.01);
    assert!((row.baseline_bottom - 25.0).abs() < 0.01);
    assert!((row.max_cell_height - 30.0).abs() < 0.01);
    assert!((row.baseline_height() - 35.0).abs() < 0.01);
  }

  #[test]
  fn row_baseline_uses_largest_baseline_offset() {
    let mut row = RowMetrics::new(20.0);
    let row_height = 20.0;
    let mut cells = vec![(0usize, 0usize, 5.0f32), (0, 1, 12.0)];

    cells.sort_by_key(|(r, c, _)| (*r, *c));

    for (_, _, baseline) in cells {
      let clamped = baseline.min(row_height);
      let bottom = (row_height - clamped).max(0.0);
      row.has_baseline = true;
      row.baseline_top = row.baseline_top.max(clamped);
      row.baseline_bottom = row.baseline_bottom.max(bottom);
      row.max_cell_height = row.max_cell_height.max(clamped + bottom);
    }

    assert!((row.baseline_top - 12.0).abs() < 0.01);
    assert!((row.baseline_bottom - 15.0).abs() < 0.01);
  }

  #[test]
  fn table_baseline_falls_back_to_first_row_bottom() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;

    let mut row0_cell_style = cell_style.clone();
    row0_cell_style.height = Some(Length::px(10.0)); // No baseline content

    let mut row1_cell_style = cell_style.clone();
    row1_cell_style.height = Some(Length::px(5.0));

    let row0_cell = BoxNode::new_block(
      Arc::new(row0_cell_style),
      FormattingContextType::Block,
      vec![],
    );
    let row1_cell = BoxNode::new_block(
      Arc::new(row1_cell_style),
      FormattingContextType::Block,
      vec![],
    );

    let row0 = BoxNode::new_block(
      Arc::new(row_style.clone()),
      FormattingContextType::Block,
      vec![row0_cell],
    );
    let row1 = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![row1_cell],
    );

    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row0, row1],
    );

    let tfc = TableFormattingContext::new();
    let constraints = LayoutConstraints::definite_width(200.0);
    let fragment = tfc.layout(&table, &constraints).expect("table layout");

    // Baseline should come from the first row's bottom edge (10px) even though later rows exist.
    let baseline = find_first_baseline(&fragment, 0.0).expect("baseline");
    assert!((baseline - 10.0).abs() < 0.01);
  }

  #[test]
  fn table_baseline_ignores_rowspan_cells() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    cell_style.vertical_align = VerticalAlign::Baseline;
    cell_style.padding_top = Length::px(0.0);
    cell_style.padding_right = Length::px(0.0);
    cell_style.padding_bottom = Length::px(0.0);
    cell_style.padding_left = Length::px(0.0);
    cell_style.border_top_width = Length::px(0.0);
    cell_style.border_right_width = Length::px(0.0);
    cell_style.border_bottom_width = Length::px(0.0);
    cell_style.border_left_width = Length::px(0.0);

    let mut span_child_style = ComputedStyle::default();
    span_child_style.display = Display::Block;
    span_child_style.height = Some(Length::px(20.0));

    let mut short_child_style = ComputedStyle::default();
    short_child_style.display = Display::Block;
    short_child_style.height = Some(Length::px(10.0));

    let span_child = BoxNode::new_block(
      Arc::new(span_child_style),
      FormattingContextType::Block,
      vec![],
    );
    let span_cell = BoxNode::new_block(
      Arc::new(cell_style.clone()),
      FormattingContextType::Block,
      vec![span_child],
    )
    .with_debug_info(DebugInfo::new(Some("td".to_string()), None, vec![]).with_spans(1, 2));

    let short_child = BoxNode::new_block(
      Arc::new(short_child_style),
      FormattingContextType::Block,
      vec![],
    );
    let col1_row0 = BoxNode::new_block(
      Arc::new(cell_style.clone()),
      FormattingContextType::Block,
      vec![short_child],
    );

    let row0 = BoxNode::new_block(
      Arc::new(row_style.clone()),
      FormattingContextType::Block,
      vec![span_cell, col1_row0],
    );

    let mut tiny_child_style = ComputedStyle::default();
    tiny_child_style.display = Display::Block;
    tiny_child_style.height = Some(Length::px(4.0));

    let tiny_child = BoxNode::new_block(
      Arc::new(tiny_child_style),
      FormattingContextType::Block,
      vec![],
    );
    let row1_cell = BoxNode::new_block(
      Arc::new(cell_style.clone()),
      FormattingContextType::Block,
      vec![tiny_child],
    );
    let row1 = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![row1_cell],
    );

    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row0, row1],
    );

    let tfc = TableFormattingContext::new();
    let fragment = tfc
      .layout(&table, &LayoutConstraints::definite_width(200.0))
      .expect("table layout");

    let baseline = find_first_baseline(&fragment, 0.0).expect("baseline");
    assert!(
      (baseline - 10.0).abs() < 0.01,
      "table baseline should ignore row-spanning cells and use the next baseline cell (got {:.2})",
      baseline
    );
  }

  #[test]
  fn rowspanning_baseline_cell_aligns_to_row_baseline() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    cell_style.vertical_align = VerticalAlign::Baseline;
    cell_style.padding_top = Length::px(0.0);
    cell_style.padding_right = Length::px(0.0);
    cell_style.padding_bottom = Length::px(0.0);
    cell_style.padding_left = Length::px(0.0);
    cell_style.border_top_width = Length::px(0.0);
    cell_style.border_right_width = Length::px(0.0);
    cell_style.border_bottom_width = Length::px(0.0);
    cell_style.border_left_width = Length::px(0.0);

    let mut short_child_style = ComputedStyle::default();
    short_child_style.display = Display::Block;
    short_child_style.height = Some(Length::px(10.0));

    let mut span_child_style = ComputedStyle::default();
    span_child_style.display = Display::Block;
    span_child_style.height = Some(Length::px(40.0));

    let short_child = BoxNode::new_block(
      Arc::new(short_child_style),
      FormattingContextType::Block,
      vec![],
    );
    let short_cell = BoxNode::new_block(
      Arc::new(cell_style.clone()),
      FormattingContextType::Block,
      vec![short_child],
    );

    let span_child = BoxNode::new_block(
      Arc::new(span_child_style),
      FormattingContextType::Block,
      vec![],
    );
    let span_cell = BoxNode::new_block(
      Arc::new(cell_style.clone()),
      FormattingContextType::Block,
      vec![span_child],
    )
    .with_debug_info(DebugInfo::new(Some("td".to_string()), None, vec![]).with_spans(1, 2));

    let mut filler_child_style = ComputedStyle::default();
    filler_child_style.display = Display::Block;
    filler_child_style.height = Some(Length::px(4.0));
    let filler_child = BoxNode::new_block(
      Arc::new(filler_child_style),
      FormattingContextType::Block,
      vec![],
    );
    let filler_cell = BoxNode::new_block(
      Arc::new(cell_style.clone()),
      FormattingContextType::Block,
      vec![filler_child],
    );

    let row0 = BoxNode::new_block(
      Arc::new(row_style.clone()),
      FormattingContextType::Block,
      vec![short_cell, span_cell],
    );
    let row1 = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![filler_cell],
    );

    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row0, row1],
    );
    let tree = BoxTree::new(table);
    let table = &tree.root;

    let short_cell_id = table.children[0].children[0].id;
    let span_cell_id = table.children[0].children[1].id;

    let tfc = TableFormattingContext::new();
    let fragment = tfc
      .layout(table, &LayoutConstraints::definite_width(200.0))
      .expect("table layout");

    let short_fragment =
      find_fragment_by_box_id(&fragment, short_cell_id).expect("short cell fragment");
    let span_fragment =
      find_fragment_by_box_id(&fragment, span_cell_id).expect("spanning cell fragment");

    let short_baseline = cell_baseline(short_fragment).expect("short baseline");
    let span_baseline = cell_baseline(span_fragment).expect("spanning baseline");

    assert!(
            (short_baseline - span_baseline).abs() < 0.01,
            "rowspanning baseline-aligned cells should align to the row baseline (short {:.2}, span {:.2})",
            short_baseline,
            span_baseline
        );
  }

  #[test]
  fn rowspan_height_is_shared_across_rows() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;

    let mut tall_child_style = ComputedStyle::default();
    tall_child_style.display = Display::Block;
    tall_child_style.height = Some(Length::px(40.0));

    let mut span_child_style = ComputedStyle::default();
    span_child_style.display = Display::Block;
    span_child_style.height = Some(Length::px(100.0));

    let tall_child = BoxNode::new_block(
      Arc::new(tall_child_style),
      FormattingContextType::Block,
      vec![],
    );
    let cell_a = BoxNode::new_block(
      Arc::new(cell_style.clone()),
      FormattingContextType::Block,
      vec![tall_child],
    );

    let span_child = BoxNode::new_block(
      Arc::new(span_child_style),
      FormattingContextType::Block,
      vec![],
    );
    let span_cell = BoxNode::new_block(
      Arc::new(cell_style.clone()),
      FormattingContextType::Block,
      vec![span_child],
    )
    .with_debug_info(DebugInfo::new(Some("td".to_string()), None, vec![]).with_spans(1, 2));

    let row0 = BoxNode::new_block(
      Arc::new(row_style.clone()),
      FormattingContextType::Block,
      vec![cell_a, span_cell],
    );

    let row1_cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
    let row1 = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![row1_cell],
    );

    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row0, row1],
    );

    let tfc = TableFormattingContext::new();
    let fragment = tfc
      .layout(&table, &LayoutConstraints::definite_width(200.0))
      .expect("table layout");

    let mut tops = Vec::new();
    collect_table_cell_tops(&fragment, &mut tops);
    tops.sort_by(|a, b| a.partial_cmp(b).unwrap());

    let row0_y = tops.first().copied().unwrap_or(0.0);
    let row1_y = tops.last().copied().unwrap_or(0.0);
    assert!(row1_y > row0_y, "row starts should ascend: {:?}", tops);

    let row0_height = row1_y - row0_y;
    let total_height = fragment.bounds.height();
    let row1_height = total_height - row1_y;

    assert!(
      row0_height > 50.0 && row0_height < 120.0,
      "row 0 height should carry part of the spanning cell but not all of it: {row0_height}"
    );
    assert!(
      row1_height > 15.0,
      "row 1 should retain a meaningful share of the spanning cell: {row1_height}"
    );
  }

  #[test]
  fn colspan_cell_widths_follow_column_sums_and_order() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_collapse = BorderCollapse::Separate;
    table_style.border_spacing_horizontal = Length::px(6.0);
    table_style.border_spacing_vertical = Length::px(0.0);

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    cell_style.width = Some(Length::px(20.0));

    let row0 = BoxNode::new_block(
      Arc::new(row_style.clone()),
      FormattingContextType::Block,
      vec![
        BoxNode::new_block(
          Arc::new(cell_style.clone()),
          FormattingContextType::Block,
          vec![],
        ),
        BoxNode::new_block(
          Arc::new(cell_style.clone()),
          FormattingContextType::Block,
          vec![],
        ),
        BoxNode::new_block(
          Arc::new(cell_style.clone()),
          FormattingContextType::Block,
          vec![],
        ),
      ],
    );

    let spanning_cell = BoxNode::new_block(
      Arc::new(cell_style.clone()),
      FormattingContextType::Block,
      vec![],
    )
    .with_debug_info(DebugInfo::new(Some("td".to_string()), None, vec![]).with_spans(2, 1));
    let trailing_cell =
      BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
    let row1 = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![spanning_cell, trailing_cell],
    );

    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row0, row1],
    );
    let tree = BoxTree::new(table);
    let table = &tree.root;

    let structure = TableStructure::from_box_tree(table);
    assert_eq!(structure.column_count, 3);
    let h_spacing = structure.border_spacing.0;

    let row0 = &table.children[0];
    let col0_id = row0.children[0].id;
    let col1_id = row0.children[1].id;
    let col2_id = row0.children[2].id;
    let span_id = table.children[1].children[0].id;
    let trailing_id = table.children[1].children[1].id;

    let tfc = TableFormattingContext::new();
    let fragment = tfc
      .layout(table, &LayoutConstraints::definite_width(240.0))
      .expect("table layout");

    let cell_fragments: Vec<&FragmentNode> = fragment
      .children
      .iter()
      .filter(|f| {
        f.style
          .as_ref()
          .map(|s| matches!(s.display, Display::TableCell))
          .unwrap_or(false)
      })
      .collect();
    assert_eq!(cell_fragments.len(), 5);

    let ids: Vec<Option<usize>> = cell_fragments
      .iter()
      .map(|f| match f.content {
        FragmentContent::Block { box_id } => box_id,
        _ => None,
      })
      .collect();
    assert_eq!(
      ids,
      vec![
        Some(col0_id),
        Some(col1_id),
        Some(col2_id),
        Some(span_id),
        Some(trailing_id)
      ]
    );

    let col0_width = cell_fragments[0].bounds.width();
    let col1_width = cell_fragments[1].bounds.width();
    let col2_width = cell_fragments[2].bounds.width();
    let span_width = cell_fragments[3].bounds.width();
    let trailing_width = cell_fragments[4].bounds.width();
    let expected_span = col0_width + col1_width + h_spacing;
    assert!(
      (span_width - expected_span).abs() < 0.01,
      "spanning cell width should equal summed columns plus spacing (expected {:.2}, got {:.2})",
      expected_span,
      span_width
    );
    assert!(
      (trailing_width - col2_width).abs() < 0.01,
      "non-spanning cell should retain its column width (expected {:.2}, got {:.2})",
      col2_width,
      trailing_width
    );
  }

  #[test]

  fn test_collapsed_borders_contribute_to_table_bounds() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_collapse = BorderCollapse::Collapse;
    table_style.border_top_style = BorderStyle::Solid;
    table_style.border_bottom_style = BorderStyle::Solid;
    table_style.border_left_style = BorderStyle::Solid;
    table_style.border_right_style = BorderStyle::Solid;
    table_style.border_top_width = Length::px(2.0);
    table_style.border_bottom_width = Length::px(2.0);
    table_style.border_left_width = Length::px(3.0);
    table_style.border_right_width = Length::px(3.0);
    table_style.border_top_color = Rgba::from_rgba8(0, 0, 0, 255);
    table_style.border_bottom_color = Rgba::from_rgba8(0, 0, 0, 255);
    table_style.border_left_color = Rgba::from_rgba8(0, 0, 0, 255);
    table_style.border_right_color = Rgba::from_rgba8(0, 0, 0, 255);

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    let cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell],
    );
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let tfc = TableFormattingContext::new();
    let constraints = LayoutConstraints::definite_width(100.0);
    let fragment = tfc.layout(&table, &constraints).expect("table layout");

    // Width should include half of the outer collapsed borders (1.5 + 1.5) even with zero cell width.
    assert!(fragment.bounds.width() >= 3.0);
    // Height should include half of the outer collapsed borders (1 + 1) plus at least a minimal row height.
    assert!(fragment.bounds.height() >= 3.0);
  }

  #[test]
  fn test_collapsed_borders_affect_row_offsets() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_collapse = BorderCollapse::Collapse;
    table_style.border_top_style = BorderStyle::Solid;
    table_style.border_bottom_style = BorderStyle::Solid;
    table_style.border_top_width = Length::px(2.0);
    table_style.border_bottom_width = Length::px(2.0);

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    let cell_a = BoxNode::new_block(
      Arc::new(cell_style.clone()),
      FormattingContextType::Block,
      vec![],
    );
    let cell_b = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
    let row1 = BoxNode::new_block(
      Arc::new(row_style.clone()),
      FormattingContextType::Block,
      vec![cell_a],
    );
    let row2 = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell_b],
    );

    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row1, row2],
    );

    let tfc = TableFormattingContext::new();
    let constraints = LayoutConstraints::definite_width(10.0);
    let fragment = tfc.layout(&table, &constraints).expect("table layout");

    // Expect half the top border + two minimal rows + half the bottom border.
    assert!((fragment.bounds.height() - 4.0).abs() < 0.51);
  }

  #[test]
  fn test_collapsed_borders_offset_cell_position() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_collapse = BorderCollapse::Collapse;
    table_style.border_top_style = BorderStyle::Solid;
    table_style.border_bottom_style = BorderStyle::Solid;
    table_style.border_left_style = BorderStyle::Solid;
    table_style.border_right_style = BorderStyle::Solid;
    table_style.border_top_width = Length::px(4.0);
    table_style.border_bottom_width = Length::px(4.0);
    table_style.border_left_width = Length::px(6.0);
    table_style.border_right_width = Length::px(6.0);
    table_style.border_top_color = Rgba::from_rgba8(0, 0, 0, 255);
    table_style.border_bottom_color = Rgba::from_rgba8(0, 0, 0, 255);
    table_style.border_left_color = Rgba::from_rgba8(0, 0, 0, 255);
    table_style.border_right_color = Rgba::from_rgba8(0, 0, 0, 255);
    table_style.width = Some(Length::px(100.0));

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    let cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell],
    );
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let tfc = TableFormattingContext::new();
    let constraints = LayoutConstraints::definite_width(100.0);
    let fragment = tfc.layout(&table, &constraints).expect("table layout");

    let cell_frag = fragment
      .children
      .iter()
      .find(|f| {
        f.style
          .as_ref()
          .map(|s| matches!(s.display, Display::TableCell))
          .unwrap_or(false)
      })
      .expect("cell fragment");
    // Cell should be offset by the collapsed borders.
    assert!((cell_frag.bounds.x() - 3.0).abs() < 0.51);
    assert!((cell_frag.bounds.y() - 2.0).abs() < 0.51);
    // Table width should include both borders (half inside the bounds) plus the cell.
    assert!(fragment.bounds.width() >= cell_frag.bounds.width() + 6.0);
  }

  #[test]
  fn collapsed_border_styles_are_reused() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_collapse = BorderCollapse::Collapse;
    apply_uniform_border(&mut table_style, 2.0, Rgba::RED);

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    apply_uniform_border(&mut cell_style, 2.0, Rgba::RED);

    let make_cell = || {
      BoxNode::new_block(
        Arc::new(cell_style.clone()),
        FormattingContextType::Block,
        vec![],
      )
    };
    let row1 = BoxNode::new_block(
      Arc::new(row_style.clone()),
      FormattingContextType::Block,
      vec![make_cell(), make_cell()],
    );
    let row2 = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![make_cell(), make_cell()],
    );
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row1, row2],
    );

    let tfc = TableFormattingContext::new();
    let fragment = tfc
      .layout(&table, &LayoutConstraints::definite_width(80.0))
      .expect("table layout");

    let mut vertical_styles = Vec::new();
    let mut horizontal_styles = Vec::new();
    let mut corner_styles = Vec::new();
    collect_collapsed_border_styles(
      &fragment,
      &mut vertical_styles,
      &mut horizontal_styles,
      &mut corner_styles,
    );

    assert!(
      vertical_styles.len() > 1 && horizontal_styles.len() > 1 && corner_styles.len() > 1,
      "expected collapsed border fragments to be present in all orientations"
    );

    assert_eq!(
      unique_style_count(&vertical_styles),
      1,
      "vertical borders should reuse cached styles"
    );
    assert_eq!(
      unique_style_count(&horizontal_styles),
      1,
      "horizontal borders should reuse cached styles"
    );
    assert_eq!(
      unique_style_count(&corner_styles),
      1,
      "corner borders should reuse cached styles"
    );
  }

  #[test]
  fn test_collapsed_outer_border_spills_margin() {
    // Later rows with thicker outer borders should not widen the table; the extra spills outward.
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_collapse = BorderCollapse::Collapse;

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut thin_cell_style = ComputedStyle::default();
    thin_cell_style.display = Display::TableCell;
    thin_cell_style.border_left_style = BorderStyle::Solid;
    thin_cell_style.border_left_width = Length::px(2.0);

    let mut thick_cell_style = ComputedStyle::default();
    thick_cell_style.display = Display::TableCell;
    thick_cell_style.border_left_style = BorderStyle::Solid;
    thick_cell_style.border_left_width = Length::px(10.0);

    let row1 = BoxNode::new_block(
      Arc::new(row_style.clone()),
      FormattingContextType::Block,
      vec![BoxNode::new_block(
        Arc::new(thin_cell_style),
        FormattingContextType::Block,
        vec![],
      )],
    );
    let row2 = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![BoxNode::new_block(
        Arc::new(thick_cell_style),
        FormattingContextType::Block,
        vec![],
      )],
    );
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row1, row2],
    );

    let tfc = TableFormattingContext::new();
    let fragment = tfc
      .layout(&table, &LayoutConstraints::definite_width(0.0))
      .expect("table layout");

    let first_cell = fragment
      .children
      .iter()
      .find(|f| f.bounds.y().abs() < f32::EPSILON)
      .expect("first row cell");

    // Cell offset should reflect only half of the first row's 2px border (≈1px), not the later 10px border.
    assert!(
      first_cell.bounds.x() < 2.0,
      "cell offset should use first-row border, got {}",
      first_cell.bounds.x()
    );
    // Table width should not inflate to include the thicker second-row border.
    assert!(
      fragment.bounds.width() < 6.0,
      "table width should be bounded by the first row's border"
    );
  }

  #[test]
  fn separate_tables_skip_collapsed_border_computation() {
    reset_collapsed_border_compute_count();

    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_collapse = BorderCollapse::Separate;
    table_style.border_spacing_horizontal = Length::px(1.0);
    table_style.border_spacing_vertical = Length::px(1.0);

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;
    let row_style = Arc::new(row_style);

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    let cell_style = Arc::new(cell_style);

    let mut rows = Vec::new();
    for _ in 0..50 {
      let mut cells = Vec::new();
      for _ in 0..50 {
        cells.push(BoxNode::new_block(
          cell_style.clone(),
          FormattingContextType::Block,
          vec![],
        ));
      }
      rows.push(BoxNode::new_block(
        row_style.clone(),
        FormattingContextType::Block,
        cells,
      ));
    }

    let table = BoxNode::new_block(Arc::new(table_style), FormattingContextType::Table, rows);

    let tfc = TableFormattingContext::new();
    let constraints = LayoutConstraints::definite_width(800.0);
    tfc
      .layout(&table, &constraints)
      .expect("separate table layout");

    assert_eq!(
      collapsed_border_compute_count(),
      0,
      "separate tables should not resolve collapsed borders"
    );
  }

  #[test]
  fn percentages_ignored_without_definite_table_width() {
    let mut constraints = vec![
      ColumnConstraints::percentage(50.0, 10.0, 100.0),
      ColumnConstraints::percentage(50.0, 20.0, 100.0),
    ];

    let tfc = TableFormattingContext::new();
    tfc.normalize_percentage_constraints(&mut constraints, None);

    assert!(constraints.iter().all(|c| c.percentage.is_none()));
    assert!(constraints.iter().all(|c| c.is_flexible));
  }

  #[test]
  fn min_content_available_width_uses_column_min_widths() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    cell_style.width = Some(Length::px(20.0));

    let cell_a = BoxNode::new_block(
      Arc::new(cell_style.clone()),
      FormattingContextType::Block,
      vec![],
    );
    let cell_b = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell_a, cell_b],
    );
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let tfc = TableFormattingContext::new();
    let constraints =
      LayoutConstraints::new(AvailableSpace::MinContent, AvailableSpace::Indefinite);
    let fragment = tfc.layout(&table, &constraints).expect("table layout");

    // Two cells with 20px widths -> table min-content width should be ~40px.
    assert!((fragment.bounds.width() - 40.0).abs() < 0.5);
  }

  #[test]
  fn collapsed_borders_with_rowspan_and_vertical_align() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_collapse = BorderCollapse::Collapse;
    table_style.border_top_style = BorderStyle::Solid;
    table_style.border_bottom_style = BorderStyle::Solid;
    table_style.border_left_style = BorderStyle::Solid;
    table_style.border_right_style = BorderStyle::Solid;
    table_style.border_top_width = Length::px(2.0);
    table_style.border_bottom_width = Length::px(2.0);
    table_style.border_left_width = Length::px(2.0);
    table_style.border_right_width = Length::px(2.0);
    table_style.border_top_color = Rgba::from_rgba8(0, 0, 0, 255);
    table_style.border_bottom_color = Rgba::from_rgba8(0, 0, 0, 255);
    table_style.border_left_color = Rgba::from_rgba8(0, 0, 0, 255);
    table_style.border_right_color = Rgba::from_rgba8(0, 0, 0, 255);

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut cell_top_style = ComputedStyle::default();
    cell_top_style.display = Display::TableCell;
    cell_top_style.vertical_align = VerticalAlign::Top;
    cell_top_style.vertical_align_specified = true;
    cell_top_style.min_height = Some(Length::px(10.0));

    let mut cell_span_style = ComputedStyle::default();
    cell_span_style.display = Display::TableCell;
    cell_span_style.vertical_align = VerticalAlign::Baseline;
    cell_span_style.vertical_align_specified = true;

    let mut cell_bottom_style = ComputedStyle::default();
    cell_bottom_style.display = Display::TableCell;
    cell_bottom_style.vertical_align = VerticalAlign::Bottom;
    cell_bottom_style.vertical_align_specified = true;
    cell_bottom_style.min_height = Some(Length::px(10.0));

    let top_cell = BoxNode::new_block(
      Arc::new(cell_top_style),
      FormattingContextType::Block,
      vec![],
    );
    let span_cell = BoxNode::new_block(
      Arc::new(cell_span_style),
      FormattingContextType::Block,
      vec![],
    );
    let bottom_cell = BoxNode::new_block(
      Arc::new(cell_bottom_style),
      FormattingContextType::Block,
      vec![],
    );

    let row1 = BoxNode::new_block(
      Arc::new(row_style.clone()),
      FormattingContextType::Block,
      vec![span_cell.clone(), top_cell],
    );
    let row2 = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![bottom_cell],
    );

    // Attach debug info to give row/col placement: row1 has two cells, span_cell should be col 0, top_cell col1.
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row1, row2],
    );

    let tfc = TableFormattingContext::new();
    let constraints = LayoutConstraints::definite_width(50.0);
    let fragment = tfc.layout(&table, &constraints).expect("table layout");

    // Find child fragments; first child is collapsed border, cell fragments follow.
    let mut cells: Vec<&FragmentNode> = fragment
      .children
      .iter()
      .filter(|f| matches!(f.content, FragmentContent::Block { .. }))
      .collect();
    cells.retain(|f| {
      f.style
        .as_ref()
        .map(|s| s.display == Display::Block)
        .unwrap_or(true)
    });

    // Verify vertical offsets respect collapsed borders (should start at or below 0 due to stroke centering).
    let min_y = cells
      .iter()
      .map(|c| c.bounds.y())
      .fold(f32::INFINITY, f32::min);
    assert!(min_y <= 0.0 + 0.5);
  }

  #[test]
  fn test_row_position_calculation() {
    let mut structure = TableStructure::new();
    structure.row_count = 3;
    structure.rows = vec![RowInfo::new(0), RowInfo::new(1), RowInfo::new(2)];
    structure.rows[0].min_height = 30.0;
    structure.rows[1].min_height = 40.0;
    structure.rows[2].min_height = 50.0;
    structure.border_spacing = (0.0, 10.0);

    calculate_row_heights(&mut structure, None);

    // Verify positions
    assert!((structure.rows[0].y_position - 10.0).abs() < 0.01);
    assert!((structure.rows[1].y_position - 50.0).abs() < 0.01); // 10 + 30 + 10
    assert!((structure.rows[2].y_position - 100.0).abs() < 0.01); // 50 + 40 + 10
  }

  #[test]
  fn test_row_height_empty_table() {
    let mut structure = TableStructure::new();
    structure.row_count = 0;

    calculate_row_heights(&mut structure, None);
    // Should not panic
  }

  // -------------------------------------------------------------------------
  // TableFormattingContext Tests
  // -------------------------------------------------------------------------

  #[test]
  fn test_table_formatting_context_new() {
    let tfc = TableFormattingContext::new();
    assert!(tfc.structure().is_none());
  }

  #[test]
  fn test_table_formatting_context_default() {
    let tfc = TableFormattingContext::default();
    assert!(tfc.structure().is_none());
  }

  #[test]
  fn test_table_layout_simple() {
    let tfc = TableFormattingContext::new();
    let table = create_simple_table(2, 2);
    let constraints = LayoutConstraints::definite_width(400.0);

    let result = tfc.layout(&table, &constraints);
    assert!(result.is_ok());

    let fragment = result.unwrap();
    assert!(fragment.bounds.width() > 0.0);
    assert!(fragment.bounds.height() > 0.0);
  }

  fn running_and_positioned_children_are_preserved() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);

    let mut caption_style = ComputedStyle::default();
    caption_style.display = Display::TableCaption;
    caption_style.caption_side = CaptionSide::Top;
    caption_style.height = Some(Length::px(4.0));
    let caption = BoxNode::new_block(
      Arc::new(caption_style),
      FormattingContextType::Block,
      vec![],
    );

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    cell_style.width = Some(Length::px(20.0));
    cell_style.height = Some(Length::px(10.0));
    let cell = BoxNode::new_block(Arc::new(cell_style), FormattingContextType::Block, vec![]);
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell],
    );

    let mut running_style = ComputedStyle::default();
    running_style.display = Display::Block;
    running_style.running_position = Some("header".to_string());
    running_style.width = Some(Length::px(5.0));
    running_style.height = Some(Length::px(5.0));
    let running = BoxNode::new_block(
      Arc::new(running_style),
      FormattingContextType::Block,
      vec![],
    );

    let mut positioned_style = ComputedStyle::default();
    positioned_style.display = Display::Block;
    positioned_style.position = Position::Absolute;
    positioned_style.width = Some(Length::px(7.0));
    positioned_style.height = Some(Length::px(3.0));
    let positioned = BoxNode::new_block(
      Arc::new(positioned_style),
      FormattingContextType::Block,
      vec![],
    );

    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![running, caption, row, positioned],
    );

    let tfc = TableFormattingContext::new();
    let fragment = tfc
      .layout(&table, &LayoutConstraints::definite_width(100.0))
      .expect("table layout");

    let mut anchors = Vec::new();
    collect_running_anchor_names(&fragment, &mut anchors);
    assert!(
      anchors.iter().any(|name| name == "header"),
      "running element should produce a running anchor"
    );

    let mut positioned_fragments = Vec::new();
    collect_positioned_fragments(&fragment, &mut positioned_fragments);
    assert_eq!(
      positioned_fragments.len(),
      1,
      "absolute-positioned child should be laid out"
    );
    assert!(
      positioned_fragments
        .first()
        .map(|f| f.bounds.width() > 0.0 && f.bounds.height() > 0.0)
        .unwrap_or(false),
      "positioned child should have non-zero size"
    );
  }

  #[test]
  fn table_cell_override_styles_are_cached() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.width = Some(Length::px(200.0));
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    cell_style.width = Some(Length::px(100.0));
    cell_style.margin_left = Some(Length::px(4.0));
    cell_style.margin_right = Some(Length::px(4.0));
    let shared_cell_style = Arc::new(cell_style);

    let first_cell = BoxNode::new_block(
      shared_cell_style.clone(),
      FormattingContextType::Block,
      vec![],
    );
    let second_cell = BoxNode::new_block(
      shared_cell_style.clone(),
      FormattingContextType::Block,
      vec![],
    );
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![first_cell, second_cell],
    );
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let tfc = TableFormattingContext::new();
    let fragment = tfc
      .layout(&table, &LayoutConstraints::definite_width(200.0))
      .expect("table layout");

    let mut cell_fragments = Vec::new();
    collect_table_cell_fragments(&fragment, &mut cell_fragments);
    assert_eq!(cell_fragments.len(), 2, "expected two table cell fragments");
    let first_style = cell_fragments[0].style.as_ref().expect("first cell style");
    let second_style = cell_fragments[1].style.as_ref().expect("second cell style");
    assert!(
      Arc::ptr_eq(first_style, second_style),
      "override styles should be reused across cells sharing the same base style"
    );
    assert!(
      first_style.width.is_none(),
      "layout overrides should clear authored widths on derived styles"
    );

    let total_width: f32 = cell_fragments.iter().map(|f| f.bounds.width()).sum();
    assert!(
      (total_width - 200.0).abs() < 0.5,
      "cached styles should not alter layout width (got {:.2})",
      total_width
    );
    assert!(
      (fragment.bounds.width() - 200.0).abs() < 0.5,
      "table width should remain at the authored 200px (got {:.2})",
      fragment.bounds.width()
    );
  }

  #[test]
  fn test_vertical_align_bottom_positions_cell_at_row_end() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut tall_style = ComputedStyle::default();
    tall_style.display = Display::TableCell;
    tall_style.height = Some(Length::px(100.0));

    let tall_cell = BoxNode::new_block(Arc::new(tall_style), FormattingContextType::Block, vec![]);

    let mut short_style = ComputedStyle::default();
    short_style.display = Display::TableCell;
    short_style.height = Some(Length::px(20.0));
    short_style.vertical_align = VerticalAlign::Bottom;
    short_style.vertical_align_specified = true;

    let short_cell =
      BoxNode::new_block(Arc::new(short_style), FormattingContextType::Block, vec![]);
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![tall_cell, short_cell],
    );
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let tfc = TableFormattingContext::new();
    let constraints = LayoutConstraints::definite_width(200.0);
    let fragment = tfc.layout(&table, &constraints).expect("table layout");

    assert_eq!(fragment.children.len(), 2);
    let first = &fragment.children[0];
    let second = &fragment.children[1];

    assert!((first.bounds.y() - 0.0).abs() < 0.01);
    // Both cells occupy the full row height; bottom alignment is expressed by shifting contents, not the box.
    assert!((second.bounds.y() - 0.0).abs() < 0.01);
    assert!((second.bounds.height() - 100.0).abs() < 0.01);
  }

  #[test]
  fn test_row_vertical_align_applies_to_cells() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;
    row_style.height = Some(Length::px(40.0));
    row_style.vertical_align = VerticalAlign::Top;
    row_style.vertical_align_specified = true;

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    cell_style.vertical_align = VerticalAlign::Middle; // UA default
    cell_style.vertical_align_specified = false;
    cell_style.padding_top = Length::px(0.0);
    cell_style.padding_bottom = Length::px(0.0);

    let mut inner_style = ComputedStyle::default();
    inner_style.display = Display::Block;
    inner_style.height = Some(Length::px(10.0));
    let inner = BoxNode::new_block(Arc::new(inner_style), FormattingContextType::Block, vec![]);

    let cell = BoxNode::new_block(
      Arc::new(cell_style),
      FormattingContextType::Block,
      vec![inner],
    );
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![cell],
    );
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );

    let tfc = TableFormattingContext::new();
    let constraints = LayoutConstraints::definite_width(200.0);
    let fragment = tfc.layout(&table, &constraints).expect("table layout");

    let cell_fragment = fragment
      .children
      .iter()
      .find(|f| matches!(f.content, FragmentContent::Block { .. }) && !f.children.is_empty())
      .expect("cell fragment");
    let content = cell_fragment.children.first().expect("cell content");

    // Top alignment from the row should place the content at the start of the cell box.
    let offset = content.bounds.y() - cell_fragment.bounds.y();
    assert!(
      offset.abs() < 0.5,
      "row vertical-align should place content at top (offset {:.2})",
      offset
    );
  }

  #[test]
  fn cell_positions_and_spanned_heights_remain_stable() {
    let mut base_cell_style = ComputedStyle::default();
    base_cell_style.display = Display::TableCell;
    base_cell_style.padding_left = Length::px(0.0);
    base_cell_style.padding_right = Length::px(0.0);
    base_cell_style.padding_top = Length::px(0.0);
    base_cell_style.padding_bottom = Length::px(0.0);

    let cell = |mut style: ComputedStyle,
                color: Rgba,
                width: f32,
                height: f32,
                colspan: usize,
                rowspan: usize| {
      style.background_color = color;
      style.width = Some(Length::px(width));
      style.height = Some(Length::px(height));
      BoxNode::new_block(Arc::new(style), FormattingContextType::Block, vec![]).with_debug_info(
        DebugInfo::new(Some("td".to_string()), None, vec![]).with_spans(colspan, rowspan),
      )
    };

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;
    row_style.height = Some(Length::px(18.0));
    let row1 = BoxNode::new_block(
      Arc::new(row_style.clone()),
      FormattingContextType::Block,
      vec![
        cell(base_cell_style.clone(), Rgba::RED, 40.0, 14.0, 2, 1),
        cell(base_cell_style.clone(), Rgba::GREEN, 30.0, 10.0, 1, 1),
      ],
    );

    row_style.height = Some(Length::px(22.0));
    let row2 = BoxNode::new_block(
      Arc::new(row_style.clone()),
      FormattingContextType::Block,
      vec![
        cell(base_cell_style.clone(), Rgba::BLUE, 25.0, 18.0, 1, 2),
        cell(
          base_cell_style.clone(),
          Rgba::new(255, 165, 0, 1.0),
          20.0,
          12.0,
          1,
          1,
        ),
        cell(
          base_cell_style.clone(),
          Rgba::new(128, 0, 128, 1.0),
          15.0,
          8.0,
          1,
          1,
        ),
      ],
    );

    row_style.height = Some(Length::px(24.0));
    let row3 = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![
        cell(
          base_cell_style.clone(),
          Rgba::new(0, 128, 128, 1.0),
          22.0,
          10.0,
          1,
          1,
        ),
        cell(
          base_cell_style,
          Rgba::new(128, 128, 0, 1.0),
          18.0,
          14.0,
          1,
          1,
        ),
      ],
    );

    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.border_spacing_horizontal = Length::px(6.0);
    table_style.border_spacing_vertical = Length::px(4.0);
    table_style.border_collapse = BorderCollapse::Separate;
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row1, row2, row3],
    );
    let style_h_spacing = table.style.border_spacing_horizontal.to_px();
    let style_v_spacing = table.style.border_spacing_vertical.to_px();
    assert!(
      (style_h_spacing - 6.0).abs() < f32::EPSILON,
      "table style horizontal spacing unexpected: {}",
      style_h_spacing
    );
    assert!(
      (style_v_spacing - 4.0).abs() < f32::EPSILON,
      "table style vertical spacing unexpected: {}",
      style_v_spacing
    );
    let resolved_spacing = resolve_border_spacing(&table.style);
    assert!(
      (resolved_spacing.0 - 6.0).abs() < f32::EPSILON
        && (resolved_spacing.1 - 4.0).abs() < f32::EPSILON,
      "resolved spacing unexpected: {:?}",
      resolved_spacing
    );
    let structure = TableStructure::from_box_tree(&table);
    assert_eq!(
      structure.border_collapse,
      BorderCollapse::Separate,
      "test must exercise the separated border model"
    );
    assert!(
      (structure.border_spacing.0 - 6.0).abs() < f32::EPSILON
        && (structure.border_spacing.1 - 4.0).abs() < f32::EPSILON,
      "unexpected border spacing values in structure: {:?}",
      structure.border_spacing
    );

    let tfc = TableFormattingContext::new();
    let fragment = tfc
      .layout(&table, &LayoutConstraints::definite_width(200.0))
      .expect("table layout");

    let cells: Vec<&FragmentNode> = fragment
      .children
      .iter()
      .filter(|f| {
        f.style
          .as_ref()
          .map(|s| s.display == Display::TableCell)
          .unwrap_or(false)
      })
      .collect();
    assert_eq!(cells.len(), 7, "expected fragments for all table cells");

    let red_color = Rgba::RED;
    let green_color = Rgba::GREEN;
    let blue_color = Rgba::BLUE;
    let orange_color = Rgba::new(255, 165, 0, 1.0);
    let purple_color = Rgba::new(128, 0, 128, 1.0);
    let teal_color = Rgba::new(0, 128, 128, 1.0);
    let olive_color = Rgba::new(128, 128, 0, 1.0);
    let match_color = |style: &ComputedStyle, target: &Rgba| {
      let c = style.background_color;
      c.r == target.r && c.g == target.g && c.b == target.b && (c.a - target.a).abs() < f32::EPSILON
    };
    let find_cell = |target: &Rgba| -> &FragmentNode {
      cells
        .iter()
        .copied()
        .find(|f| {
          f.style
            .as_ref()
            .map(|s| match_color(s, target))
            .unwrap_or(false)
        })
        .unwrap_or_else(|| panic!("missing cell with distinct background color"))
    };

    let red = find_cell(&red_color);
    let green = find_cell(&green_color);
    let blue = find_cell(&blue_color);
    let orange = find_cell(&orange_color);
    let purple = find_cell(&purple_color);
    let teal = find_cell(&teal_color);
    let olive = find_cell(&olive_color);
    let tolerance = 0.1;

    let col1_width = blue.bounds.width();
    let col2_width = orange.bounds.width();
    let col3_width = purple.bounds.width();
    assert!(
      (col3_width - olive.bounds.width()).abs() < tolerance,
      "column widths should be consistent across rows"
    );

    let delta_12 = orange.bounds.x() - blue.bounds.x();
    assert!(
      (delta_12 - (col1_width + 6.0)).abs() < tolerance,
      "column 2 should start after column 1 width + spacing (got {:.2}, expected {:.2}; col1 {:.2})",
      delta_12,
      col1_width + 6.0,
      col1_width
    );
    let delta_23 = purple.bounds.x() - orange.bounds.x();
    assert!(
      (delta_23 - (col2_width + 6.0)).abs() < tolerance,
      "column 3 should start after column 2 width + spacing (got {:.2}, expected {:.2})",
      delta_23,
      col2_width + 6.0
    );
    let green_start_expected = red.bounds.x() + col1_width + col2_width + 12.0;
    assert!(
      (green.bounds.x() - green_start_expected).abs() < tolerance,
      "colspan start should match cumulative widths (got {:.2}, expected {:.2})",
      green.bounds.x(),
      green_start_expected
    );

    let row2_height = orange.bounds.height();
    let row3_height = teal.bounds.height();
    let expected_span_height = row2_height + row3_height + 4.0;
    assert!(
      (blue.bounds.height() - expected_span_height).abs() < tolerance,
      "rowspanning cell height should equal covered rows + spacing (got {:.2}, expected {:.2})",
      blue.bounds.height(),
      expected_span_height
    );

    let baseline = fragment
      .baseline
      .expect("table baseline should be computed");
    let expected_baseline = red.bounds.y() + red.bounds.height();
    assert!(
      (baseline - expected_baseline).abs() < tolerance,
      "table baseline should align with first row's baseline (got {:.2}, expected {:.2})",
      baseline,
      expected_baseline
    );
  }

  #[test]
  fn test_table_layout_empty() {
    let tfc = TableFormattingContext::new();
    let style = create_test_style();
    let table = BoxNode::new_block(style, FormattingContextType::Table, vec![])
      .with_debug_info(DebugInfo::new(Some("table".to_string()), None, vec![]));
    let constraints = LayoutConstraints::definite_width(400.0);

    let result = tfc.layout(&table, &constraints);
    assert!(result.is_ok());
  }

  #[test]
  fn test_table_intrinsic_sizes_min_content() {
    let tfc = TableFormattingContext::new();
    let table = create_simple_table(2, 3);

    let result = tfc.compute_intrinsic_inline_size(&table, IntrinsicSizingMode::MinContent);
    assert!(result.is_ok());

    let width = result.unwrap();
    assert!(width >= 0.0);
  }

  #[test]
  fn test_table_intrinsic_sizes_max_content() {
    let tfc = TableFormattingContext::new();
    let table = create_simple_table(2, 3);

    let result = tfc.compute_intrinsic_inline_size(&table, IntrinsicSizingMode::MaxContent);
    assert!(result.is_ok());

    let width = result.unwrap();
    assert!(width >= 0.0);
  }

  #[test]
  fn test_intrinsic_widths_use_column_widths_with_no_rows() {
    let tfc = TableFormattingContext::new();
    let mut col_style = ComputedStyle::default();
    col_style.display = Display::TableColumn;
    col_style.width = Some(Length::px(25.0));
    let col = BoxNode::new_block(Arc::new(col_style), FormattingContextType::Block, vec![]);
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![col],
    );

    let min = tfc
      .compute_intrinsic_inline_size(&table, IntrinsicSizingMode::MinContent)
      .unwrap();
    let max = tfc
      .compute_intrinsic_inline_size(&table, IntrinsicSizingMode::MaxContent)
      .unwrap();
    assert!(min >= 25.0 - 0.1, "min-content should include column width");
    assert!(max >= 25.0 - 0.1, "max-content should include column width");
  }

  #[test]
  fn test_intrinsic_widths_respect_column_span_widths() {
    let tfc = TableFormattingContext::new();
    let mut col_style = ComputedStyle::default();
    col_style.display = Display::TableColumn;
    col_style.width = Some(Length::px(10.0));
    let mut col = BoxNode::new_block(Arc::new(col_style), FormattingContextType::Block, vec![]);
    col.debug_info = Some(DebugInfo::new(Some("col".to_string()), None, vec![]).with_spans(1, 1));
    col.debug_info.as_mut().unwrap().column_span = 3;
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![col],
    );

    let min = tfc
      .compute_intrinsic_inline_size(&table, IntrinsicSizingMode::MinContent)
      .unwrap();
    assert!(
      min >= 30.0 - 0.1,
      "column span width should expand intrinsic min width; got {}",
      min
    );
  }

  #[test]
  fn intrinsic_fixed_layout_uses_first_row_only() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.table_layout = TableLayout::Fixed;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut narrow_cell_style = ComputedStyle::default();
    narrow_cell_style.display = Display::TableCell;
    narrow_cell_style.width = Some(Length::px(20.0));

    let first_row = BoxNode::new_block(
      Arc::new(row_style.clone()),
      FormattingContextType::Block,
      vec![
        BoxNode::new_block(
          Arc::new(narrow_cell_style.clone()),
          FormattingContextType::Block,
          vec![],
        ),
        BoxNode::new_block(
          Arc::new(narrow_cell_style),
          FormattingContextType::Block,
          vec![],
        ),
      ],
    );

    let mut wide_cell_style = ComputedStyle::default();
    wide_cell_style.display = Display::TableCell;
    wide_cell_style.width = Some(Length::px(1000.0));

    let second_row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![
        BoxNode::new_block(
          Arc::new(wide_cell_style.clone()),
          FormattingContextType::Block,
          vec![],
        ),
        BoxNode::new_block(
          Arc::new(wide_cell_style),
          FormattingContextType::Block,
          vec![],
        ),
      ],
    );

    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![first_row, second_row],
    );
    let tfc = TableFormattingContext::new();

    let width = tfc
      .compute_intrinsic_inline_size(&table, IntrinsicSizingMode::MaxContent)
      .expect("intrinsic width");
    assert!(
      width < 200.0,
      "fixed-layout intrinsic sizing should ignore later rows; got {}",
      width
    );
  }

  #[test]
  fn intrinsic_fixed_layout_without_width_is_finite() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    table_style.table_layout = TableLayout::Fixed;
    table_style.border_spacing_horizontal = Length::px(0.0);
    table_style.border_spacing_vertical = Length::px(0.0);

    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;

    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;

    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![BoxNode::new_block(
        Arc::new(cell_style),
        FormattingContextType::Block,
        vec![],
      )],
    );

    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![row],
    );
    let tfc = TableFormattingContext::new();
    let width = tfc
      .compute_intrinsic_inline_size(&table, IntrinsicSizingMode::MaxContent)
      .expect("intrinsic width");

    assert!(width.is_finite());
    assert!(width >= 0.0);
  }

  // -------------------------------------------------------------------------
  // Edge Case Tests
  // -------------------------------------------------------------------------

  #[test]
  fn test_single_column_table() {
    let table = create_simple_table(5, 1);
    let structure = TableStructure::from_box_tree(&table);

    assert_eq!(structure.column_count, 1);
    assert_eq!(structure.row_count, 5);
  }

  #[test]
  fn test_single_row_table() {
    let table = create_simple_table(1, 5);
    let structure = TableStructure::from_box_tree(&table);

    assert_eq!(structure.row_count, 1);
    assert_eq!(structure.column_count, 5);
  }

  #[test]
  fn test_explicit_columns_from_colgroup_children_expand_column_count() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    let mut colgroup_style = ComputedStyle::default();
    colgroup_style.display = Display::TableColumnGroup;
    let mut col_style = ComputedStyle::default();
    col_style.display = Display::TableColumn;
    col_style.width = Some(Length::px(10.0));
    let col = BoxNode::new_block(
      Arc::new(col_style.clone()),
      FormattingContextType::Block,
      vec![],
    );
    let colgroup = BoxNode::new_block(
      Arc::new(colgroup_style),
      FormattingContextType::Block,
      vec![col.clone(), col.clone()],
    );
    let mut row_style = ComputedStyle::default();
    row_style.display = Display::TableRow;
    let mut cell_style = ComputedStyle::default();
    cell_style.display = Display::TableCell;
    let row = BoxNode::new_block(
      Arc::new(row_style),
      FormattingContextType::Block,
      vec![BoxNode::new_block(
        Arc::new(cell_style),
        FormattingContextType::Block,
        vec![],
      )],
    );
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![colgroup, row],
    );

    let structure = TableStructure::from_box_tree(&table);
    assert_eq!(
      structure.column_count, 2,
      "explicit columns should increase table column count beyond the row cells"
    );
    assert_eq!(structure.columns.len(), 2);
    assert!(structure
      .columns
      .iter()
      .all(|c| c.specified_width.is_some()));
  }

  #[test]
  fn test_colgroup_without_columns_still_counts_as_one_column() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;
    let mut colgroup_style = ComputedStyle::default();
    colgroup_style.display = Display::TableColumnGroup;
    colgroup_style.width = Some(Length::px(7.0));
    colgroup_style.visibility = Visibility::Visible;
    let colgroup = BoxNode::new_block(
      Arc::new(colgroup_style),
      FormattingContextType::Block,
      Vec::new(),
    );
    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![colgroup],
    );

    let structure = TableStructure::from_box_tree(&table);
    assert_eq!(
      structure.column_count, 1,
      "empty colgroup implies a single column"
    );
    assert_eq!(structure.columns.len(), 1);
    assert!(structure.columns[0].specified_width.is_some());
  }

  #[test]
  fn test_large_table() {
    let table = create_simple_table(100, 50);
    let structure = TableStructure::from_box_tree(&table);

    assert_eq!(structure.row_count, 100);
    assert_eq!(structure.column_count, 50);
    assert_eq!(structure.cells.len(), 5000);
  }

  #[test]
  fn test_col_span_attribute_expands_column_count() {
    let mut table_style = ComputedStyle::default();
    table_style.display = Display::Table;

    let mut col_style = ComputedStyle::default();
    col_style.display = Display::TableColumn;
    let mut col = BoxNode::new_block(
      Arc::new(col_style.clone()),
      FormattingContextType::Block,
      vec![],
    );
    col.debug_info = Some(
      DebugInfo::new(Some("col".to_string()), None, vec![])
        .with_dom_path("")
        .with_spans(1, 1),
    );
    col.debug_info.as_mut().unwrap().column_span = 3;

    let table = BoxNode::new_block(
      Arc::new(table_style),
      FormattingContextType::Table,
      vec![col],
    );

    let structure = TableStructure::from_box_tree(&table);
    assert_eq!(
      structure.column_count, 3,
      "col span should expand column count"
    );
  }

  #[test]
  fn test_zero_available_width() {
    let mut structure = TableStructure::new();
    structure.column_count = 3;
    structure.columns = (0..3).map(ColumnInfo::new).collect();
    structure.border_spacing = (0.0, 0.0);

    calculate_auto_layout_widths(&mut structure, 0.0);

    // All columns should have zero or minimum width
    for col in &structure.columns {
      assert!(col.computed_width >= 0.0);
    }
  }

  #[test]
  fn test_negative_spacing_prevention() {
    let mut structure = TableStructure::new();
    structure.column_count = 3;
    structure.columns = (0..3).map(ColumnInfo::new).collect();
    structure.border_spacing = (10.0, 10.0);

    // Available width less than spacing
    calculate_fixed_layout_widths(&mut structure, 20.0);

    for col in &structure.columns {
      assert!(col.computed_width >= 0.0);
    }
  }
}
