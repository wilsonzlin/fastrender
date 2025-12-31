//! Grid Formatting Context - CSS Grid Layout via Taffy
//!
//! This module implements CSS Grid layout by wrapping the Taffy layout library.
//! It converts between fastrender's box/fragment tree representation and Taffy's
//! internal representation, delegating the actual grid algorithm to Taffy.
//!
//! # Architecture
//!
//! 1. **BoxNode → Taffy Tree**: Convert fastrender BoxNode tree to Taffy nodes
//! 2. **ComputedStyle → Taffy Style**: Map CSS properties to Taffy style values
//! 3. **Taffy Layout**: Run Taffy's grid layout algorithm
//! 4. **Taffy → FragmentNode**: Convert Taffy layout results to fragments
//!
//! # CSS Grid Support
//!
//! Supports core CSS Grid features:
//! - grid-template-columns/rows with track sizing functions
//! - grid-auto-columns/rows for implicit tracks
//! - grid-auto-flow (row, column, dense variants)
//! - gap (row-gap, column-gap)
//! - grid-column/row placement (line numbers, spans, auto)
//! - align-content, justify-content, align-items, justify-items
//! - align-self, justify-self
//!
//! # References
//!
//! - CSS Grid Layout Module Level 2: <https://www.w3.org/TR/css-grid-2/>
//! - Taffy: <https://github.com/DioxusLabs/taffy>

use crate::error::{RenderError, RenderStage};
use crate::geometry::Point;
use crate::geometry::Rect;
use crate::geometry::Size;
use crate::layout::constraints::AvailableSpace as CrateAvailableSpace;
use crate::layout::constraints::LayoutConstraints;
use crate::layout::contexts::factory::FormattingContextFactory;
use crate::layout::contexts::flex_cache::ShardedFlexCache;
use crate::layout::formatting_context::intrinsic_cache_epoch;
use crate::layout::formatting_context::layout_cache_lookup;
use crate::layout::formatting_context::layout_cache_store;
use crate::layout::formatting_context::FormattingContext;
use crate::layout::formatting_context::IntrinsicSizingMode;
use crate::layout::formatting_context::LayoutError;
use crate::layout::fragment_clone_profile::{self, CloneSite};
use crate::layout::profile::layout_timer;
use crate::layout::profile::LayoutKind;
use crate::layout::taffy_integration::{
  record_taffy_compute, record_taffy_invocation, record_taffy_measure_call,
  record_taffy_node_cache_hit, record_taffy_node_cache_miss, record_taffy_style_cache_hit,
  record_taffy_style_cache_miss, taffy_constraint_key, CachedTaffyTemplate, SendSyncStyle,
  TaffyAdapterKind, TaffyNodeCache, TaffyNodeCacheKey, TAFFY_ABORT_CHECK_STRIDE,
  DEFAULT_TAFFY_CACHE_LIMIT,
};
use crate::layout::utils::resolve_length_with_percentage_metrics;
use crate::layout::utils::resolve_scrollbar_width;
use crate::render_control::{active_deadline, check_active};
use crate::style::display::Display as CssDisplay;
use crate::style::display::FormattingContextType;
use crate::style::grid::validate_area_rectangles;
use crate::style::types::AlignContent;
use crate::style::types::AlignItems;
use crate::style::types::AspectRatio;
use crate::style::types::BoxSizing;
use crate::style::types::Direction;
use crate::style::types::GridAutoFlow;
use crate::style::types::GridTrack;
use crate::style::types::JustifyContent;
use crate::style::types::Overflow as CssOverflow;
use crate::style::types::WritingMode;
use crate::style::values::Length;
use crate::style::ComputedStyle;
use crate::tree::box_tree::BoxNode;
use crate::tree::fragment_tree::FragmentContent;
use crate::tree::fragment_tree::FragmentNode;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
#[cfg(test)]
use std::cell::Cell;
use std::sync::Arc;
use taffy::geometry::Line;
use taffy::prelude::TaffyFitContent;
use taffy::prelude::TaffyMaxContent;
use taffy::prelude::TaffyMinContent;
use taffy::style::AlignContent as TaffyAlignContent;
use taffy::style::Dimension;
use taffy::style::Display;
use taffy::style::GridPlacement as TaffyGridPlacement;
use taffy::style::GridTemplateArea;
use taffy::style::GridTemplateComponent;
use taffy::style::GridTemplateRepetition;
use taffy::style::LengthPercentage;
use taffy::style::LengthPercentageAuto;
use taffy::style::MaxTrackSizingFunction;
use taffy::style::MinTrackSizingFunction;
use taffy::style::Overflow as TaffyOverflow;
use taffy::style::RepetitionCount;
use taffy::style::Style as TaffyStyle;
use taffy::style::TrackSizingFunction;
use taffy::style_helpers::TaffyAuto;
use taffy::tree::DetailedLayoutInfo;
use taffy::tree::Layout as TaffyLayout;
use taffy::tree::NodeId as TaffyNodeId;
use taffy::tree::TaffyTree;
use taffy::DetailedGridTracksInfo;

const MAX_MEASURED_KEYS_PER_NODE: usize = 12;

#[derive(Clone, Copy)]
enum Axis {
  Horizontal,
  Vertical,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
enum MeasureAvailKey {
  Definite(u32),
  MinContent,
  MaxContent,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
struct MeasureKey {
  node_ptr: usize,
  known_width: Option<u32>,
  known_height: Option<u32>,
  available_width: MeasureAvailKey,
  available_height: MeasureAvailKey,
}

impl MeasureKey {
  fn quantize(val: f32) -> f32 {
    let abs = val.abs();
    let step = if abs > 4096.0 {
      64.0
    } else if abs > 2048.0 {
      32.0
    } else if abs > 1024.0 {
      16.0
    } else if abs > 512.0 {
      8.0
    } else if abs > 256.0 {
      4.0
    } else {
      2.0
    };
    (val / step).round() * step
  }

  fn quantize_to_bits(val: f32) -> u32 {
    Self::quantize(val).to_bits()
  }

  fn new(
    node_ptr: *const BoxNode,
    known_dimensions: taffy::geometry::Size<Option<f32>>,
    available_space: taffy::geometry::Size<taffy::style::AvailableSpace>,
  ) -> Self {
    fn avail_key(space: taffy::style::AvailableSpace) -> MeasureAvailKey {
      match space {
        taffy::style::AvailableSpace::Definite(v) => {
          MeasureAvailKey::Definite(MeasureKey::quantize_to_bits(v))
        }
        taffy::style::AvailableSpace::MinContent => MeasureAvailKey::MinContent,
        taffy::style::AvailableSpace::MaxContent => MeasureAvailKey::MaxContent,
      }
    }

    Self {
      node_ptr: node_ptr as usize,
      known_width: known_dimensions.width.map(Self::quantize_to_bits),
      known_height: known_dimensions.height.map(Self::quantize_to_bits),
      available_width: avail_key(available_space.width),
      available_height: avail_key(available_space.height),
    }
  }
}

fn push_measured_key(keys: &mut Vec<MeasureKey>, key: MeasureKey) {
  if keys.len() >= MAX_MEASURED_KEYS_PER_NODE {
    keys.remove(0);
  }
  keys.push(key);
}

#[cfg(test)]
thread_local! {
  // Grid measure callbacks can be triggered during many unrelated tests. Keep the counter
  // thread-local so tests can make assertions without racing other parallel test threads.
  static GRID_MEASURE_LAYOUT_CALLS: Cell<usize> = const { Cell::new(0) };
}

#[cfg(test)]
fn reset_grid_measure_layout_calls() {
  GRID_MEASURE_LAYOUT_CALLS.with(|counter| counter.set(0));
}

#[cfg(test)]
fn grid_measure_layout_calls() -> usize {
  GRID_MEASURE_LAYOUT_CALLS.with(|counter| counter.get())
}

#[cfg(test)]
fn record_measure_layout_call() {
  GRID_MEASURE_LAYOUT_CALLS.with(|counter| counter.set(counter.get() + 1));
}

#[cfg(not(test))]
fn record_measure_layout_call() {}

fn record_fragment_clone(site: CloneSite, fragment: &FragmentNode) {
  fragment_clone_profile::record_fragment_clone_from_fragment(site, fragment);
}

fn constraints_from_taffy(
  viewport_size: crate::geometry::Size,
  known: taffy::geometry::Size<Option<f32>>,
  available: taffy::geometry::Size<taffy::style::AvailableSpace>,
  inline_percentage_base: Option<f32>,
) -> LayoutConstraints {
  let clamp_def_width = |w: f32| w.min(viewport_size.width);
  let width = match (known.width, available.width) {
    (Some(w), _) => CrateAvailableSpace::Definite(clamp_def_width(w)),
    (_, taffy::style::AvailableSpace::Definite(w)) => {
      if w <= 1.0 {
        CrateAvailableSpace::Indefinite
      } else {
        CrateAvailableSpace::Definite(clamp_def_width(w))
      }
    }
    (_, taffy::style::AvailableSpace::MinContent) => CrateAvailableSpace::MinContent,
    (_, taffy::style::AvailableSpace::MaxContent) => CrateAvailableSpace::MaxContent,
  };
  let height = match (known.height, available.height) {
    (Some(h), _) => CrateAvailableSpace::Definite(h),
    (_, taffy::style::AvailableSpace::Definite(h)) => {
      if h <= 1.0 {
        CrateAvailableSpace::Indefinite
      } else {
        CrateAvailableSpace::Definite(h)
      }
    }
    (_, taffy::style::AvailableSpace::MinContent) => CrateAvailableSpace::MinContent,
    (_, taffy::style::AvailableSpace::MaxContent) => CrateAvailableSpace::MaxContent,
  };

  let mut constraints = LayoutConstraints::new(width, height);
  constraints.inline_percentage_base = constraints
    .inline_percentage_base
    .or(inline_percentage_base)
    .or(match available.width {
      taffy::style::AvailableSpace::Definite(w) => Some(w),
      _ => None,
    });
  constraints
}

/// Grid Formatting Context
///
/// Implements CSS Grid layout by delegating to the Taffy library.
/// Each layout operation creates a fresh Taffy tree, performs layout,
/// and converts results back to FragmentNode.
///
/// # Thread Safety
///
/// GridFormattingContext is stateless and can be shared across threads.
/// Each layout operation is independent and creates its own Taffy tree.
///
/// # Example
///
/// ```ignore
/// use fastrender::layout::contexts::GridFormattingContext;
/// use fastrender::{FormattingContext, LayoutConstraints};
///
/// let fc = GridFormattingContext::new();
/// let fragment = fc.layout(&box_node, &constraints)?;
/// ```
#[derive(Clone)]
pub struct GridFormattingContext {
  /// Shared factory used to create child formatting contexts without losing shared caches.
  factory: FormattingContextFactory,
  viewport_size: crate::geometry::Size,
  font_context: crate::text::font_loader::FontContext,
  nearest_positioned_cb: crate::layout::contexts::positioned::ContainingBlock,
  taffy_cache: std::sync::Arc<crate::layout::taffy_integration::TaffyNodeCache>,
}

impl GridFormattingContext {
  fn is_simple_grid(&self, style: &ComputedStyle, children: &[&BoxNode]) -> bool {
    if !matches!(style.display, CssDisplay::Grid | CssDisplay::InlineGrid) {
      return false;
    }
    if style.grid_row_subgrid || style.grid_column_subgrid {
      return false;
    }
    if !style.grid_template_columns.is_empty() || !style.grid_template_rows.is_empty() {
      return false;
    }
    if !style.grid_template_areas.is_empty()
      || !style.grid_column_names.is_empty()
      || !style.grid_row_names.is_empty()
      || !style.grid_column_line_names.is_empty()
      || !style.grid_row_line_names.is_empty()
    {
      return false;
    }
    let auto_track = |tracks: &[GridTrack]| tracks.iter().all(|t| matches!(t, GridTrack::Auto));
    if !auto_track(&style.grid_auto_rows) || !auto_track(&style.grid_auto_columns) {
      return false;
    }
    if style.grid_gap.value != 0.0
      || style.grid_row_gap.value != 0.0
      || style.grid_column_gap.value != 0.0
    {
      return false;
    }
    if style.grid_auto_flow != GridAutoFlow::Row {
      return false;
    }
    if style.align_items != AlignItems::Stretch
      || style.justify_items != AlignItems::Stretch
      || style.align_content != AlignContent::Stretch
      || style.justify_content != JustifyContent::FlexStart
    {
      return false;
    }

    for child in children {
      let cs = &child.style;
      if cs.grid_column_start != 0
        || cs.grid_column_end != 0
        || cs.grid_row_start != 0
        || cs.grid_row_end != 0
        || cs.align_self.is_some()
        || cs.justify_self.is_some()
      {
        return false;
      }
    }

    true
  }

  /// Creates a new GridFormattingContext
  pub fn new() -> Self {
    let viewport_size = crate::geometry::Size::new(800.0, 600.0);
    Self::with_viewport_and_cb(
      viewport_size,
      crate::layout::contexts::positioned::ContainingBlock::viewport(viewport_size),
      crate::text::font_loader::FontContext::new(),
    )
  }

  fn inline_axis_positive(&self, style: &ComputedStyle) -> bool {
    match style.writing_mode {
      WritingMode::HorizontalTb => style.direction != Direction::Rtl,
      WritingMode::VerticalRl
      | WritingMode::VerticalLr
      | WritingMode::SidewaysRl
      | WritingMode::SidewaysLr => true,
    }
  }

  fn block_axis_positive(&self, style: &ComputedStyle) -> bool {
    match style.writing_mode {
      WritingMode::VerticalRl | WritingMode::SidewaysRl => false,
      _ => true,
    }
  }

  fn inline_axis_is_horizontal(&self, style: &ComputedStyle) -> bool {
    matches!(style.writing_mode, WritingMode::HorizontalTb)
  }

  pub fn with_viewport(viewport_size: crate::geometry::Size) -> Self {
    Self::with_viewport_and_cb(
      viewport_size,
      crate::layout::contexts::positioned::ContainingBlock::viewport(viewport_size),
      crate::text::font_loader::FontContext::new(),
    )
  }

  pub fn with_viewport_and_cb(
    viewport_size: crate::geometry::Size,
    nearest_positioned_cb: crate::layout::contexts::positioned::ContainingBlock,
    font_context: crate::text::font_loader::FontContext,
  ) -> Self {
    Self::with_viewport_cb_and_cache(
      viewport_size,
      nearest_positioned_cb,
      font_context,
      std::sync::Arc::new(TaffyNodeCache::new(DEFAULT_TAFFY_CACHE_LIMIT)),
    )
  }

  pub fn with_viewport_cb_and_cache(
    viewport_size: crate::geometry::Size,
    nearest_positioned_cb: crate::layout::contexts::positioned::ContainingBlock,
    font_context: crate::text::font_loader::FontContext,
    taffy_cache: std::sync::Arc<crate::layout::taffy_integration::TaffyNodeCache>,
  ) -> Self {
    let factory = FormattingContextFactory::with_font_context_viewport_cb_and_cache(
      font_context.clone(),
      viewport_size,
      nearest_positioned_cb,
      std::sync::Arc::new(ShardedFlexCache::new_measure()),
      std::sync::Arc::new(ShardedFlexCache::new_layout()),
      std::sync::Arc::new(TaffyNodeCache::new(DEFAULT_TAFFY_CACHE_LIMIT)),
      taffy_cache.clone(),
    );
    Self::with_factory(factory)
  }

  pub(crate) fn with_factory(factory: FormattingContextFactory) -> Self {
    let viewport_size = factory.viewport_size();
    let nearest_positioned_cb = factory.nearest_positioned_cb();
    let font_context = factory.font_context().clone();
    let taffy_cache = factory.grid_taffy_cache();
    Self {
      factory,
      viewport_size,
      font_context,
      nearest_positioned_cb,
      taffy_cache,
    }
  }

  fn horizontal_edges_px(&self, style: &ComputedStyle) -> Option<f32> {
    let left = self.resolve_length_px(&style.padding_left, style)?;
    let right = self.resolve_length_px(&style.padding_right, style)?;
    let bl = self.resolve_length_px(&style.border_left_width, style)?;
    let br = self.resolve_length_px(&style.border_right_width, style)?;
    Some(left + right + bl + br)
  }

  fn resolve_length_for_width(
    &self,
    length: Length,
    percentage_base: f32,
    style: &ComputedStyle,
  ) -> f32 {
    let base = if percentage_base.is_finite() {
      Some(percentage_base)
    } else {
      None
    };
    resolve_length_with_percentage_metrics(
      length,
      base,
      self.viewport_size,
      style.font_size,
      style.root_font_size,
      Some(style),
      Some(&self.font_context),
    )
    .unwrap_or(0.0)
  }

  fn resolved_padding_border_for_measure(
    &self,
    style: &ComputedStyle,
    percentage_base: f32,
  ) -> (f32, f32, f32, f32, f32, f32, f32, f32) {
    let mut padding_left = self.resolve_length_for_width(style.padding_left, percentage_base, style);
    let mut padding_right =
      self.resolve_length_for_width(style.padding_right, percentage_base, style);
    let mut padding_top = self.resolve_length_for_width(style.padding_top, percentage_base, style);
    let mut padding_bottom =
      self.resolve_length_for_width(style.padding_bottom, percentage_base, style);

    // Reserve space for scrollbars when overflow/scrollbar-gutter request stable gutters. This
    // mirrors the block formatting context behaviour so grid measurement returns the true content
    // box size (excluding scrollbar gutters).
    let reserve_vertical_gutter = matches!(style.overflow_y, CssOverflow::Scroll)
      || (style.scrollbar_gutter.stable
        && matches!(style.overflow_y, CssOverflow::Auto | CssOverflow::Scroll));
    if reserve_vertical_gutter {
      let gutter = resolve_scrollbar_width(style);
      if gutter > 0.0 {
        if style.scrollbar_gutter.both_edges {
          padding_left += gutter;
          padding_right += gutter;
        } else {
          padding_right += gutter;
        }
      }
    }

    let reserve_horizontal_gutter = matches!(style.overflow_x, CssOverflow::Scroll)
      || (style.scrollbar_gutter.stable
        && matches!(style.overflow_x, CssOverflow::Auto | CssOverflow::Scroll));
    if reserve_horizontal_gutter {
      let gutter = resolve_scrollbar_width(style);
      if gutter > 0.0 {
        if style.scrollbar_gutter.both_edges {
          padding_top += gutter;
        }
        padding_bottom += gutter;
      }
    }

    let border_left =
      self.resolve_length_for_width(style.border_left_width, percentage_base, style);
    let border_right =
      self.resolve_length_for_width(style.border_right_width, percentage_base, style);
    let border_top = self.resolve_length_for_width(style.border_top_width, percentage_base, style);
    let border_bottom =
      self.resolve_length_for_width(style.border_bottom_width, percentage_base, style);

    (
      padding_left,
      padding_right,
      padding_top,
      padding_bottom,
      border_left,
      border_right,
      border_top,
      border_bottom,
    )
  }

  fn content_box_size(
    &self,
    fragment: &FragmentNode,
    style: &ComputedStyle,
    percentage_base: f32,
  ) -> Size {
    let (
      padding_left,
      padding_right,
      padding_top,
      padding_bottom,
      border_left,
      border_right,
      border_top,
      border_bottom,
    ) = self.resolved_padding_border_for_measure(style, percentage_base);

    let content_width =
      (fragment.bounds.width() - padding_left - padding_right - border_left - border_right)
        .max(0.0);
    let content_height =
      (fragment.bounds.height() - padding_top - padding_bottom - border_top - border_bottom)
        .max(0.0);

    Size::new(content_width, content_height)
  }

  /// Builds a Taffy tree from a BoxNode tree
  ///
  /// Recursively converts the BoxNode tree to Taffy nodes, returning
  /// the root node ID and a mapping from Taffy nodes to BoxNodes.
  fn build_taffy_tree(
    &self,
    taffy: &mut TaffyTree<*const BoxNode>,
    box_node: &BoxNode,
    constraints: &LayoutConstraints,
    positioned_children: &mut HashMap<TaffyNodeId, Vec<BoxNode>>,
  ) -> Result<TaffyNodeId, LayoutError> {
    let root_children: Vec<&BoxNode> = box_node.children.iter().collect();
    self.build_taffy_tree_children(
      taffy,
      box_node,
      &root_children,
      constraints,
      positioned_children,
    )
  }

  /// Builds a Taffy tree using an explicit slice of root children (used to exclude out-of-flow boxes).
  fn build_taffy_tree_children(
    &self,
    taffy: &mut TaffyTree<*const BoxNode>,
    box_node: &BoxNode,
    root_children: &[&BoxNode],
    constraints: &LayoutConstraints,
    positioned_children: &mut HashMap<TaffyNodeId, Vec<BoxNode>>,
  ) -> Result<TaffyNodeId, LayoutError> {
    let mut in_flow_children: Vec<&BoxNode> = Vec::new();
    let mut positioned: Vec<BoxNode> = Vec::new();
    for child in root_children {
      match child.style.position {
        crate::style::position::Position::Absolute | crate::style::position::Position::Fixed => {
          positioned.push((*child).clone())
        }
        _ => in_flow_children.push(*child),
      }
    }

    let has_subgrid = box_node.style.grid_row_subgrid || box_node.style.grid_column_subgrid;
    let child_has_subgrid = in_flow_children
      .iter()
      .any(|child| child.style.grid_row_subgrid || child.style.grid_column_subgrid);

    if !has_subgrid && !child_has_subgrid {
      let child_fingerprint = grid_child_fingerprint(&in_flow_children);
      let cache_key = TaffyNodeCacheKey::new(
        TaffyAdapterKind::Grid,
        box_node.id,
        std::sync::Arc::as_ptr(&box_node.style) as usize,
        child_fingerprint,
        taffy_constraint_key(constraints, self.viewport_size),
        intrinsic_cache_epoch(),
        self.viewport_size,
      );
      let cached = self.taffy_cache.get(&cache_key);
      let template: std::sync::Arc<CachedTaffyTemplate> = if let Some(template) = cached {
        record_taffy_node_cache_hit(TaffyAdapterKind::Grid, template.node_count());
        record_taffy_style_cache_hit(TaffyAdapterKind::Grid, template.node_count());
        template
      } else {
        let mut child_styles = Vec::with_capacity(in_flow_children.len());
        for child in in_flow_children.iter() {
          child_styles.push(std::sync::Arc::new(SendSyncStyle(self.convert_style(
            &child.style,
            Some(&box_node.style),
            false,
            false,
          ))));
        }
        let simple_grid = self.is_simple_grid(&box_node.style, &in_flow_children);
        let root_style = std::sync::Arc::new(SendSyncStyle(self.convert_style(
          &box_node.style,
          None,
          simple_grid,
          true,
        )));
        let template = std::sync::Arc::new(CachedTaffyTemplate {
          root_style,
          child_styles,
        });
        self.taffy_cache.insert(cache_key, template.clone());
        record_taffy_node_cache_miss(TaffyAdapterKind::Grid, template.node_count());
        record_taffy_style_cache_miss(TaffyAdapterKind::Grid, template.node_count());
        template
      };

      let mut taffy_children = Vec::with_capacity(in_flow_children.len());
      for (child_style, child) in template.child_styles.iter().zip(in_flow_children.iter()) {
        let node = taffy
          .new_leaf_with_context(child_style.0.clone(), *child as *const BoxNode)
          .map_err(|e| LayoutError::MissingContext(format!("Taffy error: {:?}", e)))?;
        taffy_children.push(node);
      }

      let node_id = if taffy_children.is_empty() {
        taffy
          .new_leaf_with_context(template.root_style.0.clone(), box_node as *const BoxNode)
          .map_err(|e| LayoutError::MissingContext(format!("Taffy error: {:?}", e)))?
      } else {
        let node_id = taffy
          .new_with_children(template.root_style.0.clone(), &taffy_children)
          .map_err(|e| LayoutError::MissingContext(format!("Taffy error: {:?}", e)))?;
        taffy
          .set_node_context(node_id, Some(box_node as *const BoxNode))
          .map_err(|e| LayoutError::MissingContext(format!("Taffy error: {:?}", e)))?;
        node_id
      };

      if !positioned.is_empty() {
        positioned_children.insert(node_id, positioned);
      }

      return Ok(node_id);
    }

    self.build_taffy_tree_inner(
      taffy,
      box_node,
      true,
      None,
      Some(root_children.to_vec()),
      positioned_children,
    )
  }

  fn build_taffy_tree_inner(
    &self,
    taffy: &mut TaffyTree<*const BoxNode>,
    box_node: &BoxNode,
    is_root: bool,
    containing_grid: Option<&ComputedStyle>,
    children_override: Option<Vec<&BoxNode>>,
    positioned_children: &mut HashMap<TaffyNodeId, Vec<BoxNode>>,
  ) -> Result<TaffyNodeId, LayoutError> {
    let mut children_iter: Vec<&BoxNode> = Vec::new();
    let mut positioned: Vec<BoxNode> = Vec::new();

    // Determine whether this grid container should be represented in the Taffy tree.
    let is_grid_container = matches!(
      box_node.formatting_context(),
      Some(FormattingContextType::Grid)
    );
    let mut include_children = is_root;

    // Prefer explicitly provided children (used to pre-filter out-of-flow for the root).
    let provided_children = children_override.unwrap_or_else(|| box_node.children.iter().collect());

    // Partition children into in-flow vs positioned for grid containers we expand in the tree.
    if is_grid_container {
      for child in provided_children {
        match child.style.position {
          crate::style::position::Position::Absolute | crate::style::position::Position::Fixed => {
            positioned.push(child.clone())
          }
          _ => children_iter.push(child),
        }
      }
    } else if is_root {
      children_iter = provided_children;
    }

    // Expand subgrids (and any grid that hosts a subgrid child) into the Taffy tree so tracks can be shared.
    if is_grid_container {
      let is_subgrid = box_node.style.grid_row_subgrid || box_node.style.grid_column_subgrid;
      let has_subgrid_child = children_iter
        .iter()
        .any(|child| child.style.grid_row_subgrid || child.style.grid_column_subgrid);
      include_children |= is_subgrid || has_subgrid_child;
    }

    let simple_grid =
      include_children && is_root && self.is_simple_grid(&box_node.style, &children_iter);
    let taffy_style = self.convert_style(
      &box_node.style,
      containing_grid,
      simple_grid,
      include_children,
    );

    let node_id = if include_children {
      let mut taffy_children = Vec::with_capacity(children_iter.len());
      for child in children_iter {
        taffy_children.push(self.build_taffy_tree_inner(
          taffy,
          child,
          false,
          Some(&*box_node.style),
          None,
          positioned_children,
        )?);
      }
      let node_id = taffy
        .new_with_children(taffy_style, &taffy_children)
        .map_err(|e| LayoutError::MissingContext(format!("Taffy error: {:?}", e)))?;
      if !positioned.is_empty() {
        positioned_children.insert(node_id, positioned);
      }
      taffy
        .set_node_context(node_id, Some(box_node as *const BoxNode))
        .map_err(|e| LayoutError::MissingContext(format!("Taffy error: {:?}", e)))?;
      node_id
    } else {
      taffy
        .new_leaf_with_context(taffy_style, box_node as *const BoxNode)
        .map_err(|e| LayoutError::MissingContext(format!("Taffy error: {:?}", e)))?
    };

    Ok(node_id)
  }

  /// Converts ComputedStyle to Taffy Style
  fn convert_style(
    &self,
    style: &ComputedStyle,
    containing_grid: Option<&ComputedStyle>,
    simple_grid: bool,
    is_grid_node: bool,
  ) -> TaffyStyle {
    let mut taffy_style = TaffyStyle::default();
    let inline_positive_container = self.inline_axis_positive(style);
    let block_positive_container = self.block_axis_positive(style);
    let inline_is_horizontal_container = self.inline_axis_is_horizontal(style);

    let reserve_scroll_x = style.scrollbar_gutter.stable
      && matches!(style.overflow_x, CssOverflow::Auto | CssOverflow::Scroll);
    let reserve_scroll_y = style.scrollbar_gutter.stable
      && matches!(style.overflow_y, CssOverflow::Auto | CssOverflow::Scroll);
    let map_overflow = |value: CssOverflow, reserve: bool| match value {
      // Taffy lacks an Auto variant; treat it like Visible unless scrollbar-gutter requests stability.
      CssOverflow::Visible | CssOverflow::Auto => {
        if reserve {
          TaffyOverflow::Scroll
        } else {
          TaffyOverflow::Visible
        }
      }
      CssOverflow::Hidden => TaffyOverflow::Hidden,
      CssOverflow::Scroll => TaffyOverflow::Scroll,
      CssOverflow::Clip => TaffyOverflow::Clip,
    };

    // Grid item axes follow the containing grid's writing mode, not the item's own.
    let item_axis_style = containing_grid.unwrap_or(style);
    let inline_positive_item = self.inline_axis_positive(item_axis_style);
    let block_positive_item = self.block_axis_positive(item_axis_style);
    let inline_is_horizontal_item = self.inline_axis_is_horizontal(item_axis_style);

    // Display mode
    let is_grid = is_grid_node && !simple_grid;
    if is_grid {
      taffy_style.display = Display::Grid;
    } else {
      taffy_style.display = Display::Block;
    }

    // Size
    taffy_style.size = taffy::geometry::Size {
      width: self.convert_opt_length_to_dimension_box_sizing(&style.width, style, Axis::Horizontal),
      height: self.convert_opt_length_to_dimension_box_sizing(&style.height, style, Axis::Vertical),
    };

    // Min/Max size
    taffy_style.min_size = taffy::geometry::Size {
      width: self.convert_opt_length_to_dimension_box_sizing(
        &style.min_width,
        style,
        Axis::Horizontal,
      ),
      height: self.convert_opt_length_to_dimension_box_sizing(
        &style.min_height,
        style,
        Axis::Vertical,
      ),
    };
    taffy_style.max_size = taffy::geometry::Size {
      width: self.convert_opt_length_to_dimension_box_sizing(
        &style.max_width,
        style,
        Axis::Horizontal,
      ),
      height: self.convert_opt_length_to_dimension_box_sizing(
        &style.max_height,
        style,
        Axis::Vertical,
      ),
    };

    // Margin
    let margin_left_auto = style.margin_left.is_none();
    let margin_right_auto = style.margin_right.is_none();
    let margin_top_auto = style.margin_top.is_none();
    let margin_bottom_auto = style.margin_bottom.is_none();
    taffy_style.margin = taffy::geometry::Rect {
      left: self.convert_opt_length_to_lpa(&style.margin_left, style),
      right: self.convert_opt_length_to_lpa(&style.margin_right, style),
      top: self.convert_opt_length_to_lpa(&style.margin_top, style),
      bottom: self.convert_opt_length_to_lpa(&style.margin_bottom, style),
    };

    // Padding
    taffy_style.padding = taffy::geometry::Rect {
      left: self.convert_length_to_lp(&style.padding_left, style),
      right: self.convert_length_to_lp(&style.padding_right, style),
      top: self.convert_length_to_lp(&style.padding_top, style),
      bottom: self.convert_length_to_lp(&style.padding_bottom, style),
    };

    // Border
    taffy_style.border = taffy::geometry::Rect {
      left: self.convert_length_to_lp(&style.border_left_width, style),
      right: self.convert_length_to_lp(&style.border_right_width, style),
      top: self.convert_length_to_lp(&style.border_top_width, style),
      bottom: self.convert_length_to_lp(&style.border_bottom_width, style),
    };
    taffy_style.aspect_ratio = self.convert_aspect_ratio(style.aspect_ratio);

    taffy_style.overflow = taffy::geometry::Point {
      x: map_overflow(style.overflow_x, reserve_scroll_x),
      y: map_overflow(style.overflow_y, reserve_scroll_y),
    };
    taffy_style.scrollbar_width = resolve_scrollbar_width(style);

    // Grid container properties
    if is_grid {
      // Grid template columns/rows (swap when inline axis is vertical)
      if inline_is_horizontal_container {
        taffy_style.grid_template_columns =
          self.convert_grid_template(&style.grid_template_columns, style);
        taffy_style.grid_template_rows =
          self.convert_grid_template(&style.grid_template_rows, style);
        taffy_style.subgrid_columns = style.grid_column_subgrid;
        taffy_style.subgrid_rows = style.grid_row_subgrid;
        if !style.subgrid_column_line_names.is_empty() {
          taffy_style.subgrid_column_names = style.subgrid_column_line_names.clone();
        }
        if !style.subgrid_row_line_names.is_empty() {
          taffy_style.subgrid_row_names = style.subgrid_row_line_names.clone();
        }
      } else {
        taffy_style.grid_template_columns =
          self.convert_grid_template(&style.grid_template_rows, style);
        taffy_style.grid_template_rows =
          self.convert_grid_template(&style.grid_template_columns, style);
        taffy_style.subgrid_columns = style.grid_row_subgrid;
        taffy_style.subgrid_rows = style.grid_column_subgrid;
        if !style.subgrid_row_line_names.is_empty() {
          taffy_style.subgrid_column_names = style.subgrid_row_line_names.clone();
        }
        if !style.subgrid_column_line_names.is_empty() {
          taffy_style.subgrid_row_names = style.subgrid_column_line_names.clone();
        }
      }

      // Line names
      if inline_is_horizontal_container {
        if !style.grid_column_line_names.is_empty() {
          taffy_style.grid_template_column_names = style.grid_column_line_names.clone();
        }
        if !style.grid_row_line_names.is_empty() {
          taffy_style.grid_template_row_names = style.grid_row_line_names.clone();
        }
      } else {
        if !style.grid_row_line_names.is_empty() {
          taffy_style.grid_template_column_names = style.grid_row_line_names.clone();
        }
        if !style.grid_column_line_names.is_empty() {
          taffy_style.grid_template_row_names = style.grid_column_line_names.clone();
        }
      }
      // Implicit track sizing
      if inline_is_horizontal_container {
        if !style.grid_auto_rows.is_empty() {
          taffy_style.grid_auto_rows = style
            .grid_auto_rows
            .iter()
            .map(|t| self.convert_track_size(t, style))
            .collect();
        }
        if !style.grid_auto_columns.is_empty() {
          taffy_style.grid_auto_columns = style
            .grid_auto_columns
            .iter()
            .map(|t| self.convert_track_size(t, style))
            .collect();
        }
      } else {
        if !style.grid_auto_columns.is_empty() {
          taffy_style.grid_auto_rows = style
            .grid_auto_columns
            .iter()
            .map(|t| self.convert_track_size(t, style))
            .collect();
        }
        if !style.grid_auto_rows.is_empty() {
          taffy_style.grid_auto_columns = style
            .grid_auto_rows
            .iter()
            .map(|t| self.convert_track_size(t, style))
            .collect();
        }
      }
      taffy_style.grid_auto_flow = match (style.grid_auto_flow, inline_is_horizontal_container) {
        (GridAutoFlow::Row, true) => taffy::style::GridAutoFlow::Row,
        (GridAutoFlow::RowDense, true) => taffy::style::GridAutoFlow::RowDense,
        (GridAutoFlow::Column, true) => taffy::style::GridAutoFlow::Column,
        (GridAutoFlow::ColumnDense, true) => taffy::style::GridAutoFlow::ColumnDense,
        (GridAutoFlow::Row, false) => taffy::style::GridAutoFlow::Column,
        (GridAutoFlow::RowDense, false) => taffy::style::GridAutoFlow::ColumnDense,
        (GridAutoFlow::Column, false) => taffy::style::GridAutoFlow::Row,
        (GridAutoFlow::ColumnDense, false) => taffy::style::GridAutoFlow::RowDense,
      };
      if !style.grid_template_areas.is_empty() {
        if let Some(mut bounds) = validate_area_rectangles(&style.grid_template_areas) {
          let mut entries: Vec<_> = bounds.drain().collect();
          entries.sort_by(|a, b| a.0.cmp(&b.0));
          let mut areas = Vec::with_capacity(entries.len());
          for (name, (top, bottom, left, right)) in entries {
            let (row_start, row_end, column_start, column_end) = if inline_is_horizontal_container {
              (
                (top as u16) + 1,
                (bottom as u16) + 2,
                (left as u16) + 1,
                (right as u16) + 2,
              )
            } else {
              // Transpose area matrix for vertical inline axis
              (
                (left as u16) + 1,
                (right as u16) + 2,
                (top as u16) + 1,
                (bottom as u16) + 2,
              )
            };
            areas.push(GridTemplateArea {
              name,
              row_start,
              row_end,
              column_start,
              column_end,
            });
          }
          taffy_style.grid_template_areas = areas;
        }
      }

      // Gap
      taffy_style.gap = taffy::geometry::Size {
        width: if inline_is_horizontal_container {
          self.convert_length_to_lp(&style.grid_column_gap, style)
        } else {
          self.convert_length_to_lp(&style.grid_row_gap, style)
        },
        height: if inline_is_horizontal_container {
          self.convert_length_to_lp(&style.grid_row_gap, style)
        } else {
          self.convert_length_to_lp(&style.grid_column_gap, style)
        },
      };

      // Alignment
      taffy_style.align_content =
        Some(self.convert_align_content(&style.align_content, block_positive_container));
      taffy_style.justify_content =
        Some(self.convert_justify_content(&style.justify_content, inline_positive_container));
    }
    taffy_style.align_items =
      Some(self.convert_align_items(&style.align_items, block_positive_container));
    taffy_style.justify_items =
      Some(self.convert_align_items(&style.justify_items, inline_positive_container));
    taffy_style.align_self = style
      .align_self
      .map(|a| self.convert_align_items(&a, block_positive_item));
    taffy_style.justify_self = style
      .justify_self
      .map(|a| self.convert_align_items(&a, inline_positive_item));

    if containing_grid.is_some() {
      // Auto margins override alignment per-axis; map them to self-alignment to keep grid items centered or pushed.
      let inline_start_auto = if inline_is_horizontal_item {
        if inline_positive_item {
          margin_left_auto
        } else {
          margin_right_auto
        }
      } else if block_positive_item {
        margin_top_auto
      } else {
        margin_bottom_auto
      };
      let inline_end_auto = if inline_is_horizontal_item {
        if inline_positive_item {
          margin_right_auto
        } else {
          margin_left_auto
        }
      } else if block_positive_item {
        margin_bottom_auto
      } else {
        margin_top_auto
      };

      let block_start_auto = if block_positive_item {
        margin_top_auto
      } else {
        margin_bottom_auto
      };
      let block_end_auto = if block_positive_item {
        margin_bottom_auto
      } else {
        margin_top_auto
      };

      let justify_override = match (inline_start_auto, inline_end_auto) {
        (true, true) => Some(AlignItems::Center),
        (true, false) => Some(if inline_positive_item {
          AlignItems::FlexEnd
        } else {
          AlignItems::FlexStart
        }),
        (false, true) => Some(if inline_positive_item {
          AlignItems::FlexStart
        } else {
          AlignItems::FlexEnd
        }),
        _ => None,
      };
      if let Some(justify) = justify_override {
        taffy_style.justify_self = Some(self.convert_align_items(&justify, inline_positive_item));
      }

      let align_override = match (block_start_auto, block_end_auto) {
        (true, true) => Some(AlignItems::Center),
        (true, false) => Some(if block_positive_item {
          AlignItems::FlexEnd
        } else {
          AlignItems::FlexStart
        }),
        (false, true) => Some(if block_positive_item {
          AlignItems::FlexStart
        } else {
          AlignItems::FlexEnd
        }),
        _ => None,
      };
      if let Some(align) = align_override {
        taffy_style.align_self = Some(self.convert_align_items(&align, block_positive_item));
      }
    }

    // Grid item properties using raw line numbers (swap placements when inline axis is vertical)
    if inline_is_horizontal_item {
      taffy_style.grid_column = self.convert_grid_placement(
        style.grid_column_raw.as_deref(),
        style.grid_column_start,
        style.grid_column_end,
      );
      taffy_style.grid_row = self.convert_grid_placement(
        style.grid_row_raw.as_deref(),
        style.grid_row_start,
        style.grid_row_end,
      );
    } else {
      // Inline axis maps to rows (y), block axis maps to columns (x)
      taffy_style.grid_row = self.convert_grid_placement(
        style.grid_column_raw.as_deref(),
        style.grid_column_start,
        style.grid_column_end,
      );
      taffy_style.grid_column = self.convert_grid_placement(
        style.grid_row_raw.as_deref(),
        style.grid_row_start,
        style.grid_row_end,
      );
    }

    taffy_style
  }

  /// Converts Option<Length> to Taffy Dimension
  fn convert_opt_length_to_dimension_box_sizing(
    &self,
    length: &Option<Length>,
    style: &ComputedStyle,
    axis: Axis,
  ) -> Dimension {
    match length {
      None => Dimension::auto(),
      Some(len) => self.dimension_for_box_sizing(len, style, axis),
    }
  }

  /// Converts Length to Taffy Dimension
  fn convert_length_to_dimension(&self, length: &Length, style: &ComputedStyle) -> Dimension {
    use crate::style::values::LengthUnit;
    match length.unit {
      LengthUnit::Percent => Dimension::percent(length.value / 100.0),
      _ => {
        if let Some(px) = self.resolve_length_px(length, style) {
          Dimension::length(px)
        } else {
          Dimension::length(length.to_px())
        }
      }
    }
  }

  /// Converts Option<Length> to Taffy LengthPercentageAuto
  fn convert_opt_length_to_lpa(
    &self,
    length: &Option<Length>,
    style: &ComputedStyle,
  ) -> LengthPercentageAuto {
    use crate::style::values::LengthUnit;
    match length {
      None => LengthPercentageAuto::auto(),
      Some(len) => match len.unit {
        LengthUnit::Percent => LengthPercentageAuto::percent(len.value / 100.0),
        _ => {
          if let Some(px) = self.resolve_length_px(len, style) {
            LengthPercentageAuto::length(px)
          } else {
            LengthPercentageAuto::length(len.to_px())
          }
        }
      },
    }
  }

  /// Converts Length to Taffy LengthPercentage
  fn convert_length_to_lp(&self, length: &Length, style: &ComputedStyle) -> LengthPercentage {
    use crate::style::values::LengthUnit;
    match length.unit {
      LengthUnit::Percent => LengthPercentage::percent(length.value / 100.0),
      _ => {
        if let Some(px) = self.resolve_length_px(length, style) {
          LengthPercentage::length(px)
        } else {
          LengthPercentage::length(length.to_px())
        }
      }
    }
  }

  fn dimension_for_box_sizing(&self, len: &Length, style: &ComputedStyle, axis: Axis) -> Dimension {
    if style.box_sizing == BoxSizing::ContentBox {
      if let Some(edges) = self.edges_px(style, axis) {
        if let Some(px) = self.resolve_length_px(len, style) {
          return Dimension::length((px + edges).max(0.0));
        }
      }
    }
    self.convert_length_to_dimension(len, style)
  }

  fn edges_px(&self, style: &ComputedStyle, axis: Axis) -> Option<f32> {
    match axis {
      Axis::Horizontal => {
        let p1 = self.resolve_length_px(&style.padding_left, style)?;
        let p2 = self.resolve_length_px(&style.padding_right, style)?;
        let b1 = self.resolve_length_px(&style.border_left_width, style)?;
        let b2 = self.resolve_length_px(&style.border_right_width, style)?;
        Some(p1 + p2 + b1 + b2)
      }
      Axis::Vertical => {
        let p1 = self.resolve_length_px(&style.padding_top, style)?;
        let p2 = self.resolve_length_px(&style.padding_bottom, style)?;
        let b1 = self.resolve_length_px(&style.border_top_width, style)?;
        let b2 = self.resolve_length_px(&style.border_bottom_width, style)?;
        Some(p1 + p2 + b1 + b2)
      }
    }
  }

  fn resolve_length_px(&self, len: &Length, style: &ComputedStyle) -> Option<f32> {
    use crate::style::values::LengthUnit::Ch;
    use crate::style::values::LengthUnit::Cm;
    use crate::style::values::LengthUnit::Em;
    use crate::style::values::LengthUnit::Ex;
    use crate::style::values::LengthUnit::In;
    use crate::style::values::LengthUnit::Mm;
    use crate::style::values::LengthUnit::Pc;
    use crate::style::values::LengthUnit::Percent;
    use crate::style::values::LengthUnit::Pt;
    use crate::style::values::LengthUnit::Px;
    use crate::style::values::LengthUnit::Rem;
    match len.unit {
      Percent => None,
      Px | Pt | In | Cm | Mm | Pc => Some(len.to_px()),
      Rem => Some(len.value * style.root_font_size),
      Em => Some(len.value * style.font_size),
      Ex => Some(len.value * style.font_size * 0.5),
      Ch => Some(len.value * style.font_size * 0.5),
      unit if unit.is_viewport_relative() => {
        len.resolve_with_viewport(self.viewport_size.width, self.viewport_size.height)
      }
      _ => None,
    }
  }

  /// Converts GridTrack Vec to Taffy track list
  fn convert_grid_template(
    &self,
    tracks: &[GridTrack],
    style: &ComputedStyle,
  ) -> Vec<GridTemplateComponent<String>> {
    let mut components = Vec::new();
    for track in tracks {
      match track {
        GridTrack::RepeatAutoFill {
          tracks: inner,
          line_names,
        } => {
          let converted: Vec<TrackSizingFunction> = inner
            .iter()
            .map(|t| self.convert_track_size(t, style))
            .collect();
          let repetition = GridTemplateRepetition {
            count: RepetitionCount::AutoFill,
            tracks: converted,
            line_names: line_names.clone(),
          };
          components.push(GridTemplateComponent::Repeat(repetition));
        }
        GridTrack::RepeatAutoFit {
          tracks: inner,
          line_names,
        } => {
          let converted: Vec<TrackSizingFunction> = inner
            .iter()
            .map(|t| self.convert_track_size(t, style))
            .collect();
          let repetition = GridTemplateRepetition {
            count: RepetitionCount::AutoFit,
            tracks: converted,
            line_names: line_names.clone(),
          };
          components.push(GridTemplateComponent::Repeat(repetition));
        }
        _ => components.push(GridTemplateComponent::Single(
          self.convert_track_size(track, style),
        )),
      }
    }
    components
  }

  /// Converts a single GridTrack to TrackSizingFunction
  fn convert_track_size(&self, track: &GridTrack, style: &ComputedStyle) -> TrackSizingFunction {
    match track {
      GridTrack::Length(len) => {
        let lp = self.convert_length_to_lp(len, style);
        TrackSizingFunction::from(lp)
      }
      GridTrack::MinContent => TrackSizingFunction::MIN_CONTENT,
      GridTrack::MaxContent => TrackSizingFunction::MAX_CONTENT,
      GridTrack::FitContent(len) => {
        TrackSizingFunction::fit_content(self.convert_length_to_lp(len, style))
      }
      GridTrack::Fr(fr) => TrackSizingFunction {
        min: MinTrackSizingFunction::AUTO,
        max: MaxTrackSizingFunction::fr(*fr),
      },
      GridTrack::Auto => TrackSizingFunction::AUTO,
      GridTrack::MinMax(min, max) => {
        let min_fn = self.convert_min_track(min, style);
        let max_fn = self.convert_max_track(max, style);
        TrackSizingFunction {
          min: min_fn,
          max: max_fn,
        }
      }
      GridTrack::RepeatAutoFill { .. } | GridTrack::RepeatAutoFit { .. } => {
        TrackSizingFunction::AUTO
      }
    }
  }

  /// Converts GridTrack to MinTrackSizingFunction
  fn convert_min_track(&self, track: &GridTrack, style: &ComputedStyle) -> MinTrackSizingFunction {
    use crate::style::values::LengthUnit;
    match track {
      GridTrack::Length(len) => match len.unit {
        LengthUnit::Percent => MinTrackSizingFunction::percent(len.value / 100.0),
        _ => {
          if let Some(px) = self.resolve_length_px(len, style) {
            MinTrackSizingFunction::length(px)
          } else {
            MinTrackSizingFunction::length(len.to_px())
          }
        }
      },
      GridTrack::MinContent => MinTrackSizingFunction::MIN_CONTENT,
      GridTrack::MaxContent => MinTrackSizingFunction::MAX_CONTENT,
      GridTrack::Auto => MinTrackSizingFunction::auto(),
      _ => MinTrackSizingFunction::auto(),
    }
  }

  /// Converts GridTrack to MaxTrackSizingFunction
  fn convert_max_track(&self, track: &GridTrack, style: &ComputedStyle) -> MaxTrackSizingFunction {
    use crate::style::values::LengthUnit;
    match track {
      GridTrack::Length(len) => match len.unit {
        LengthUnit::Percent => MaxTrackSizingFunction::percent(len.value / 100.0),
        _ => {
          if let Some(px) = self.resolve_length_px(len, style) {
            MaxTrackSizingFunction::length(px)
          } else {
            MaxTrackSizingFunction::length(len.to_px())
          }
        }
      },
      GridTrack::Fr(fr) => MaxTrackSizingFunction::fr(*fr),
      GridTrack::MinContent => MaxTrackSizingFunction::MIN_CONTENT,
      GridTrack::MaxContent => MaxTrackSizingFunction::MAX_CONTENT,
      GridTrack::FitContent(len) => {
        MaxTrackSizingFunction::fit_content(self.convert_length_to_lp(len, style))
      }
      GridTrack::Auto
      | GridTrack::MinMax(..)
      | GridTrack::RepeatAutoFill { .. }
      | GridTrack::RepeatAutoFit { .. } => MaxTrackSizingFunction::auto(),
    }
  }

  /// Converts grid placements (with optional named lines) to Taffy Line<GridPlacement>
  fn convert_grid_placement(
    &self,
    raw: Option<&str>,
    start: i32,
    end: i32,
  ) -> Line<TaffyGridPlacement<String>> {
    if let Some(raw_str) = raw {
      return parse_grid_line_placement_raw(raw_str);
    }

    Line {
      start: if start == 0 {
        TaffyGridPlacement::Auto
      } else {
        TaffyGridPlacement::Line((start as i16).into())
      },
      end: if end == 0 {
        TaffyGridPlacement::Auto
      } else {
        TaffyGridPlacement::Line((end as i16).into())
      },
    }
  }

  /// Converts AlignContent to Taffy AlignContent
  fn convert_align_content(&self, align: &AlignContent, axis_positive: bool) -> TaffyAlignContent {
    match align {
      AlignContent::FlexStart => {
        if axis_positive {
          TaffyAlignContent::Start
        } else {
          TaffyAlignContent::End
        }
      }
      AlignContent::FlexEnd => {
        if axis_positive {
          TaffyAlignContent::End
        } else {
          TaffyAlignContent::Start
        }
      }
      AlignContent::Center => TaffyAlignContent::Center,
      AlignContent::Stretch => TaffyAlignContent::Stretch,
      AlignContent::SpaceBetween => TaffyAlignContent::SpaceBetween,
      AlignContent::SpaceEvenly => TaffyAlignContent::SpaceEvenly,
      AlignContent::SpaceAround => TaffyAlignContent::SpaceAround,
    }
  }

  fn convert_justify_content(
    &self,
    justify: &JustifyContent,
    axis_positive: bool,
  ) -> TaffyAlignContent {
    match justify {
      JustifyContent::FlexStart => {
        if axis_positive {
          TaffyAlignContent::Start
        } else {
          TaffyAlignContent::End
        }
      }
      JustifyContent::FlexEnd => {
        if axis_positive {
          TaffyAlignContent::End
        } else {
          TaffyAlignContent::Start
        }
      }
      JustifyContent::Center => TaffyAlignContent::Center,
      JustifyContent::SpaceBetween => TaffyAlignContent::SpaceBetween,
      JustifyContent::SpaceAround => TaffyAlignContent::SpaceAround,
      JustifyContent::SpaceEvenly => TaffyAlignContent::SpaceEvenly,
    }
  }

  fn convert_align_items(
    &self,
    align: &AlignItems,
    axis_positive: bool,
  ) -> taffy::style::AlignItems {
    match align {
      AlignItems::Start | AlignItems::SelfStart => {
        if axis_positive {
          taffy::style::AlignItems::Start
        } else {
          taffy::style::AlignItems::End
        }
      }
      AlignItems::End | AlignItems::SelfEnd => {
        if axis_positive {
          taffy::style::AlignItems::End
        } else {
          taffy::style::AlignItems::Start
        }
      }
      AlignItems::FlexStart => taffy::style::AlignItems::FlexStart,
      AlignItems::FlexEnd => taffy::style::AlignItems::FlexEnd,
      AlignItems::Center => taffy::style::AlignItems::Center,
      AlignItems::Baseline => taffy::style::AlignItems::Baseline,
      AlignItems::Stretch => taffy::style::AlignItems::Stretch,
    }
  }

  fn convert_aspect_ratio(&self, aspect_ratio: AspectRatio) -> Option<f32> {
    match aspect_ratio {
      AspectRatio::Auto => None,
      AspectRatio::Ratio(ratio) => Some(ratio),
    }
  }

  fn take_matching_measured_fragment(
    measured_fragments: &Rc<RefCell<HashMap<MeasureKey, FragmentNode>>>,
    keys: &[MeasureKey],
    width: f32,
    height: f32,
  ) -> Option<FragmentNode> {
    let mut measured = measured_fragments.borrow_mut();
    let matched_key = keys.iter().copied().find(|key| {
      measured.get(key).map_or(false, |fragment| {
        (fragment.bounds.width() - width).abs() < 0.1
          && (fragment.bounds.height() - height).abs() < 0.1
      })
    })?;
    measured.remove(&matched_key)
  }

  /// Converts Taffy layout results to FragmentNode tree
  fn convert_to_fragments(
    &self,
    taffy: &TaffyTree<*const BoxNode>,
    node_id: TaffyNodeId,
    root_id: TaffyNodeId,
    constraints: &LayoutConstraints,
    measured_fragments: &Rc<RefCell<HashMap<MeasureKey, FragmentNode>>>,
    measured_node_keys: &HashMap<TaffyNodeId, Vec<MeasureKey>>,
    positioned_children: &HashMap<TaffyNodeId, Vec<BoxNode>>,
  ) -> Result<FragmentNode, LayoutError> {
    let layout = taffy
      .layout(node_id)
      .map_err(|e| LayoutError::MissingContext(format!("Taffy layout error: {:?}", e)))?;

    // Convert children recursively
    let children = taffy
      .children(node_id)
      .map_err(|e| LayoutError::MissingContext(format!("Taffy children error: {:?}", e)))?;

    let child_fragments: Vec<FragmentNode> = children
      .iter()
      .map(|&child_id| {
        self.convert_to_fragments(
          taffy,
          child_id,
          root_id,
          constraints,
          measured_fragments,
          measured_node_keys,
          positioned_children,
        )
      })
      .collect::<Result<_, _>>()?;

    // Create fragment bounds from Taffy layout
    let bounds = Rect::from_xywh(
      layout.location.x,
      layout.location.y,
      layout.size.width,
      layout.size.height,
    );

    // Get style from node context if available
    if let Some(&box_node_ptr) = taffy.get_node_context(node_id) {
      let box_node = unsafe { &*box_node_ptr };
      let fc_type = box_node
        .formatting_context()
        .unwrap_or(FormattingContextType::Block);
      let taffy_style = taffy.style(node_id).ok();
      let is_grid_style = matches!(taffy_style.as_ref().map(|s| s.display), Some(Display::Grid));
      if node_id == root_id || is_grid_style || !child_fragments.is_empty() {
        let mut fragment = FragmentNode::new_with_style(
          bounds,
          FragmentContent::Block {
            box_id: Some(box_node.id),
          },
          child_fragments,
          box_node.style.clone(),
        );
        if is_grid_style {
          self.apply_grid_baseline_alignment(taffy, node_id, layout, &children, &mut fragment);
        }
        if let Some(positioned) = positioned_children.get(&node_id) {
          let mut abs_children =
            self.layout_positioned_children_for_node(box_node, bounds, positioned)?;
          fragment.children_mut().append(&mut abs_children);
        }
        return Ok(fragment);
      }

      if let Some(keys) = measured_node_keys.get(&node_id) {
        if let Some(mut reused) = Self::take_matching_measured_fragment(
          measured_fragments,
          keys,
          bounds.width(),
          bounds.height(),
        ) {
          fragment_clone_profile::record_fragment_reuse_without_clone(CloneSite::GridMeasureReuse);
          debug_assert!(
            reused.bounds.x().abs() < 0.01 && reused.bounds.y().abs() < 0.01,
            "measured fragments should be normalized to the origin",
          );
          translate_fragment_tree(&mut reused, Point::new(bounds.x(), bounds.y()));
          return Ok(reused);
        }
      }

      let fc = self.factory.get(fc_type);

      let child_constraints = LayoutConstraints::new(
        CrateAvailableSpace::Definite(bounds.width()),
        CrateAvailableSpace::Definite(bounds.height()),
      )
      .with_inline_percentage_base(
        constraints
          .inline_percentage_base
          .or_else(|| Some(bounds.width())),
      );

      let mut laid_out = if fc_type == FormattingContextType::Block {
        let child_constraints =
          child_constraints.with_used_border_box_size(Some(bounds.width()), Some(bounds.height()));
        fc.layout(box_node, &child_constraints)?
      } else {
        let mut layout_child = (*box_node).clone();
        let mut layout_style = (*layout_child.style).clone();
        layout_style.width = Some(Length::px(bounds.width()));
        layout_style.height = Some(Length::px(bounds.height()));
        layout_child.style = Arc::new(layout_style);
        fc.layout(&layout_child, &child_constraints)?
      };
      translate_fragment_tree(&mut laid_out, Point::new(bounds.x(), bounds.y()));
      laid_out.content = FragmentContent::Block {
        box_id: Some(box_node.id),
      };
      laid_out.style = Some(box_node.style.clone());
      Ok(laid_out)
    } else {
      Ok(FragmentNode::new_block(bounds, child_fragments))
    }
  }

  fn layout_positioned_children_for_node(
    &self,
    box_node: &BoxNode,
    bounds: Rect,
    positioned_children: &[BoxNode],
  ) -> Result<Vec<FragmentNode>, LayoutError> {
    if positioned_children.is_empty() {
      return Ok(Vec::new());
    }

    let padding_left =
      self.resolve_length_for_width(box_node.style.padding_left, bounds.width(), &box_node.style);
    let padding_top =
      self.resolve_length_for_width(box_node.style.padding_top, bounds.width(), &box_node.style);
    let border_left = self.resolve_length_for_width(
      box_node.style.border_left_width,
      bounds.width(),
      &box_node.style,
    );
    let border_top = self.resolve_length_for_width(
      box_node.style.border_top_width,
      bounds.width(),
      &box_node.style,
    );
    let border_right = self.resolve_length_for_width(
      box_node.style.border_right_width,
      bounds.width(),
      &box_node.style,
    );
    let border_bottom = self.resolve_length_for_width(
      box_node.style.border_bottom_width,
      bounds.width(),
      &box_node.style,
    );
    let padding_origin =
      crate::geometry::Point::new(border_left + padding_left, border_top + padding_top);
    let padding_size = crate::geometry::Size::new(
      bounds.width() - border_left - border_right,
      bounds.height() - border_top - border_bottom,
    );
    let padding_rect = crate::geometry::Rect::new(padding_origin, padding_size);

    let block_base = if box_node.style.height.is_some() {
      Some(padding_rect.size.height)
    } else {
      None
    };
    let establishes_abs_cb = box_node.style.position.is_positioned()
      || !box_node.style.transform.is_empty()
      || box_node.style.perspective.is_some();
    let establishes_fixed_cb =
      !box_node.style.transform.is_empty() || box_node.style.perspective.is_some();
    let padding_cb = crate::layout::contexts::positioned::ContainingBlock::with_viewport_and_bases(
      padding_rect,
      self.viewport_size,
      Some(padding_rect.size.width),
      block_base,
    );
    let cb_for_absolute = if establishes_abs_cb {
      padding_cb
    } else {
      self.nearest_positioned_cb
    };

    let abs = crate::layout::absolute_positioning::AbsoluteLayout::with_font_context(
      self.font_context.clone(),
    );
    let mut fragments = Vec::with_capacity(positioned_children.len());
    for child in positioned_children {
      // Layout child as static for intrinsic size.
      let mut layout_child = child.clone();
      let mut style = (*layout_child.style).clone();
      style.position = crate::style::position::Position::Relative;
      style.top = None;
      style.right = None;
      style.bottom = None;
      style.left = None;
      layout_child.style = Arc::new(style);

      let cb = match child.style.position {
        crate::style::position::Position::Fixed => {
          if establishes_fixed_cb {
            padding_cb
          } else {
            crate::layout::contexts::positioned::ContainingBlock::viewport(self.viewport_size)
          }
        }
        _ => cb_for_absolute,
      };

      let factory = self.factory.with_positioned_cb(cb);
      let fc_type = layout_child
        .formatting_context()
        .unwrap_or(crate::style::display::FormattingContextType::Block);
      let fc = factory.get(fc_type);
      let child_constraints = LayoutConstraints::new(
        CrateAvailableSpace::Definite(padding_rect.size.width),
        block_base
          .map(CrateAvailableSpace::Definite)
          .unwrap_or(CrateAvailableSpace::Indefinite),
      );
      let mut child_fragment = fc.layout(&layout_child, &child_constraints)?;

      let positioned_style = crate::layout::absolute_positioning::resolve_positioned_style(
        &child.style,
        &cb,
        self.viewport_size,
        &self.font_context,
      );
      // Static position resolves to where the element would be in flow; use the
      // content origin here since AbsoluteLayout adds padding/border.
      let static_pos = crate::geometry::Point::ZERO;
      let needs_inline_intrinsics = positioned_style.width.is_auto()
        && (positioned_style.left.is_auto()
          || positioned_style.right.is_auto()
          || child.is_replaced());
      let needs_block_intrinsics = positioned_style.height.is_auto()
        && (positioned_style.top.is_auto() || positioned_style.bottom.is_auto());
      let preferred_min_inline = if needs_inline_intrinsics {
        match fc.compute_intrinsic_inline_size(&layout_child, IntrinsicSizingMode::MinContent) {
          Ok(size) => Some(size),
          Err(err @ LayoutError::Timeout { .. }) => return Err(err),
          Err(_) => None,
        }
      } else {
        None
      };
      let preferred_inline = if needs_inline_intrinsics {
        match fc.compute_intrinsic_inline_size(&layout_child, IntrinsicSizingMode::MaxContent) {
          Ok(size) => Some(size),
          Err(err @ LayoutError::Timeout { .. }) => return Err(err),
          Err(_) => None,
        }
      } else {
        None
      };
      let preferred_min_block = if needs_block_intrinsics {
        match fc.compute_intrinsic_block_size(&layout_child, IntrinsicSizingMode::MinContent) {
          Ok(size) => Some(size),
          Err(err @ LayoutError::Timeout { .. }) => return Err(err),
          Err(_) => None,
        }
      } else {
        None
      };
      let preferred_block = if needs_block_intrinsics {
        match fc.compute_intrinsic_block_size(&layout_child, IntrinsicSizingMode::MaxContent) {
          Ok(size) => Some(size),
          Err(err @ LayoutError::Timeout { .. }) => return Err(err),
          Err(_) => None,
        }
      } else {
        None
      };

      let mut input = crate::layout::absolute_positioning::AbsoluteLayoutInput::new(
        positioned_style,
        child_fragment.bounds.size,
        static_pos,
      );
      input.is_replaced = child.is_replaced();
      input.preferred_min_inline_size = preferred_min_inline;
      input.preferred_inline_size = preferred_inline;
      input.preferred_min_block_size = preferred_min_block;
      input.preferred_block_size = preferred_block;
      let result = abs.layout_absolute(&input, &cb)?;
      let needs_relayout = (result.size.width - child_fragment.bounds.width()).abs() > 0.01
        || (result.size.height - child_fragment.bounds.height()).abs() > 0.01;
      if needs_relayout {
        let relayout_constraints = LayoutConstraints::new(
          CrateAvailableSpace::Definite(result.size.width),
          CrateAvailableSpace::Definite(result.size.height),
        );
        let mut relayout_child = layout_child.clone();
        let mut relayout_style = (*relayout_child.style).clone();
        relayout_style.width = Some(Length::px(result.size.width));
        relayout_style.height = Some(Length::px(result.size.height));
        relayout_child.style = Arc::new(relayout_style);
        child_fragment = fc.layout(&relayout_child, &relayout_constraints)?;
      }
      child_fragment.bounds = crate::geometry::Rect::new(result.position, result.size);
      child_fragment.style = Some(child.style.clone());
      fragments.push(child_fragment);
    }

    Ok(fragments)
  }

  fn alignment_for_axis(
    &self,
    axis: Axis,
    child_style: &taffy::style::Style,
    container_style: &taffy::style::Style,
  ) -> taffy::style::AlignItems {
    let fallback = match axis {
      Axis::Vertical => {
        if !child_style.size.height.is_auto() || child_style.aspect_ratio.is_some() {
          taffy::style::AlignItems::Start
        } else {
          taffy::style::AlignItems::Stretch
        }
      }
      Axis::Horizontal => {
        if !child_style.size.width.is_auto() {
          taffy::style::AlignItems::Start
        } else {
          taffy::style::AlignItems::Stretch
        }
      }
    };

    match axis {
      Axis::Vertical => child_style
        .align_self
        .or(container_style.align_items)
        .unwrap_or(fallback),
      Axis::Horizontal => child_style
        .justify_self
        .or(container_style.justify_items)
        .unwrap_or(fallback),
    }
  }

  fn baseline_offset_with_fallback(&self, fragment: &FragmentNode, axis: Axis) -> Option<f32> {
    if let Some(offset) = first_baseline_offset(fragment) {
      return Some(offset);
    }

    let size = match axis {
      Axis::Vertical => fragment.bounds.height(),
      Axis::Horizontal => fragment.bounds.width(),
    };
    if size.is_finite() && size > 0.0 {
      Some(size)
    } else {
      None
    }
  }

  fn apply_baseline_group(&self, axis: Axis, group: &[BaselineItem], fragment: &mut FragmentNode) {
    if group.is_empty() {
      return;
    }

    let debug_baseline =
      crate::debug::runtime::runtime_toggles().truthy("FASTR_DEBUG_GRID_BASELINE");

    let mut target = 0.0;
    for item in group {
      let area_size = (item.area_end - item.area_start).max(0.0);
      let clamped = item.baseline.min(area_size);
      if clamped > target {
        target = clamped;
      }
    }

    if debug_baseline {
      let axis_name = match axis {
        Axis::Horizontal => "horizontal",
        Axis::Vertical => "vertical",
      };
      eprintln!(
        "[grid-baseline] axis={} group_len={} target={:.2}",
        axis_name,
        group.len(),
        target
      );
    }

    for item in group {
      let area_size = (item.area_end - item.area_start).max(0.0);
      if area_size <= 0.0 {
        if debug_baseline {
          eprintln!(
            "[grid-baseline] idx={} non-positive area (area_start={:.2} area_end={:.2})",
            item.idx, item.area_start, item.area_end
          );
        }
        continue;
      }
      let upper = (item.area_end - item.size).max(item.area_start);
      let desired_start = (item.area_start + target - item.baseline).clamp(item.area_start, upper);
      let delta = desired_start - item.start;
      if delta.abs() > 0.01 {
        if debug_baseline {
          eprintln!(
            "[grid-baseline] idx={} area=({:.2},{:.2}) baseline={:.2} start={:.2} size={:.2} desired_start={:.2} delta={:.2}",
            item.idx,
            item.area_start,
            item.area_end,
            item.baseline,
            item.start,
            item.size,
            desired_start,
            delta
          );
        }
        if let Some(child) = fragment.children_mut().get_mut(item.idx) {
          translate_along_axis(child, axis, delta);
        }
      } else if debug_baseline {
        eprintln!(
          "[grid-baseline] idx={} area=({:.2},{:.2}) baseline={:.2} start={:.2} size={:.2} desired_start={:.2} delta={:.4}",
          item.idx,
          item.area_start,
          item.area_end,
          item.baseline,
          item.start,
          item.size,
          desired_start,
          delta
        );
      }
    }
  }

  fn apply_grid_baseline_alignment(
    &self,
    taffy: &TaffyTree<*const BoxNode>,
    node_id: TaffyNodeId,
    layout: &TaffyLayout,
    child_ids: &[TaffyNodeId],
    fragment: &mut FragmentNode,
  ) {
    let detailed = match taffy.detailed_layout_info(node_id) {
      DetailedLayoutInfo::Grid(info) => info,
      _ => return,
    };
    if fragment.children.is_empty() {
      return;
    }
    if detailed.items.len() != fragment.children.len() || child_ids.len() != fragment.children.len()
    {
      return;
    }

    let container_style = match taffy.style(node_id) {
      Ok(style) => style,
      Err(_) => return,
    };

    let row_offsets = compute_track_offsets(
      &detailed.rows,
      layout.size.height,
      layout.padding.top,
      layout.padding.bottom,
      layout.border.top,
      layout.border.bottom,
      container_style
        .align_content
        .unwrap_or(TaffyAlignContent::Stretch),
    );
    let col_offsets = compute_track_offsets(
      &detailed.columns,
      layout.size.width,
      layout.padding.left,
      layout.padding.right,
      layout.border.left,
      layout.border.right,
      container_style
        .justify_content
        .unwrap_or(TaffyAlignContent::Stretch),
    );

    let debug_baseline =
      crate::debug::runtime::runtime_toggles().truthy("FASTR_DEBUG_GRID_BASELINE");
    if debug_baseline {
      eprintln!(
        "[grid-baseline] rows sizes={:?} gutters={:?} offsets={:?}",
        detailed.rows.sizes, detailed.rows.gutters, row_offsets
      );
      eprintln!(
        "[grid-baseline] cols sizes={:?} gutters={:?} offsets={:?}",
        detailed.columns.sizes, detailed.columns.gutters, col_offsets
      );
    }

    let mut row_groups: HashMap<u16, Vec<BaselineItem>> = HashMap::new();
    let mut col_groups: HashMap<u16, Vec<BaselineItem>> = HashMap::new();

    for (idx, ((child_id, item_info), child_fragment)) in child_ids
      .iter()
      .zip(detailed.items.iter())
      .zip(fragment.children.iter())
      .enumerate()
    {
      let child_style = match taffy.style(*child_id) {
        Ok(style) => style,
        Err(_) => continue,
      };

      if self.alignment_for_axis(Axis::Vertical, child_style, container_style)
        == taffy::style::AlignItems::Baseline
      {
        if let (Some((area_start, area_end)), Some(baseline)) = (
          grid_area_for_item(&row_offsets, item_info.row_start, item_info.row_end),
          self.baseline_offset_with_fallback(child_fragment, Axis::Vertical),
        ) {
          if debug_baseline {
            eprintln!(
              "[grid-baseline] idx={} row=({},{}) area=({:.2},{:.2}) start={:.2} size={:.2} baseline={:.2}",
              idx,
              item_info.row_start,
              item_info.row_end,
              area_start,
              area_end,
              child_fragment.bounds.y(),
              child_fragment.bounds.height(),
              baseline
            );
          }
          row_groups
            .entry(item_info.row_start)
            .or_default()
            .push(BaselineItem {
              idx,
              area_start,
              area_end,
              baseline,
              start: child_fragment.bounds.y(),
              size: child_fragment.bounds.height(),
            });
        }
      }

      if self.alignment_for_axis(Axis::Horizontal, child_style, container_style)
        == taffy::style::AlignItems::Baseline
      {
        if let (Some((area_start, area_end)), Some(baseline)) = (
          grid_area_for_item(&col_offsets, item_info.column_start, item_info.column_end),
          self.baseline_offset_with_fallback(child_fragment, Axis::Horizontal),
        ) {
          if debug_baseline {
            eprintln!(
              "[grid-baseline] idx={} col=({},{}) area=({:.2},{:.2}) start={:.2} size={:.2} baseline={:.2}",
              idx,
              item_info.column_start,
              item_info.column_end,
              area_start,
              area_end,
              child_fragment.bounds.x(),
              child_fragment.bounds.width(),
              baseline
            );
          }
          col_groups
            .entry(item_info.column_start)
            .or_default()
            .push(BaselineItem {
              idx,
              area_start,
              area_end,
              baseline,
              start: child_fragment.bounds.x(),
              size: child_fragment.bounds.width(),
            });
        }
      }
    }

    for group in row_groups.values() {
      self.apply_baseline_group(Axis::Vertical, group, fragment);
    }
    for group in col_groups.values() {
      self.apply_baseline_group(Axis::Horizontal, group, fragment);
    }
  }

  /// Computes intrinsic size using Taffy
  fn compute_intrinsic_size(
    &self,
    box_node: &BoxNode,
    mode: IntrinsicSizingMode,
  ) -> Result<f32, LayoutError> {
    debug_assert!(
      matches!(
        box_node.formatting_context(),
        Some(FormattingContextType::Grid)
      ),
      "GridFormattingContext must only query grid containers",
    );
    let style = &box_node.style;
    if style.containment.isolates_inline_size() {
      let edges = self.horizontal_edges_px(style).unwrap_or(0.0);
      return Ok(edges.max(0.0));
    }

    let mut taffy: TaffyTree<*const BoxNode> = TaffyTree::new();
    let mut positioned_children: HashMap<TaffyNodeId, Vec<BoxNode>> = HashMap::new();
    let intrinsic_constraints = LayoutConstraints::new(
      match mode {
        IntrinsicSizingMode::MinContent => CrateAvailableSpace::MinContent,
        IntrinsicSizingMode::MaxContent => CrateAvailableSpace::MaxContent,
      },
      CrateAvailableSpace::Indefinite,
    );
    let root_id = self.build_taffy_tree(
      &mut taffy,
      box_node,
      &intrinsic_constraints,
      &mut positioned_children,
    )?;

    // Use appropriate available space for intrinsic sizing
    let available_space = match mode {
      IntrinsicSizingMode::MinContent => taffy::geometry::Size {
        width: taffy::style::AvailableSpace::MinContent,
        height: taffy::style::AvailableSpace::MinContent,
      },
      IntrinsicSizingMode::MaxContent => taffy::geometry::Size {
        width: taffy::style::AvailableSpace::MaxContent,
        height: taffy::style::AvailableSpace::MaxContent,
      },
    };

    record_taffy_invocation(TaffyAdapterKind::Grid);
    let taffy_perf_enabled = crate::layout::taffy_integration::taffy_perf_enabled();
    let taffy_compute_start = taffy_perf_enabled.then(std::time::Instant::now);
    // Render pipeline always installs a deadline guard (even when disabled), so only enable
    // the Taffy cancellation path when the active deadline is actually configured.
    let cancel: Option<Arc<dyn Fn() -> bool + Send + Sync>> = active_deadline()
      .filter(|deadline| deadline.is_enabled())
      .map(|_| Arc::new(|| check_active(RenderStage::Layout).is_err()) as _);
    let compute_result = taffy.compute_layout_with_measure_and_cancel(
        root_id,
        available_space,
        {
          let factory = self.factory.clone();
          let viewport_size = self.viewport_size;
          let mut cache: HashMap<MeasureKey, taffy::geometry::Size<f32>> = HashMap::new();
          move |known_dimensions,
                available_space,
                node_id,
                node_context,
                _style: &taffy::style::Style| {
            if taffy_perf_enabled {
              record_taffy_measure_call(TaffyAdapterKind::Grid);
            }
            if node_id == root_id {
              return taffy::geometry::Size::ZERO;
            }
            let Some(node_ptr) = node_context.as_ref().map(|p| **p) else {
              return taffy::geometry::Size::ZERO;
            };
            let box_node = unsafe { &*node_ptr };
            let mut available_space = available_space;
            if known_dimensions.width.is_none()
              && matches!(
                available_space.width,
                taffy::style::AvailableSpace::Definite(_)
              )
              && box_node.style.width.is_none()
            {
              available_space.width = taffy::style::AvailableSpace::MaxContent;
            }

            let key = MeasureKey::new(node_ptr, known_dimensions, available_space);
            if let Some(size) = cache.get(&key) {
              return *size;
            }
            let fc_type = box_node
              .formatting_context()
              .unwrap_or(FormattingContextType::Block);
            let fc = factory.get(fc_type);
            let constraints =
              constraints_from_taffy(viewport_size, known_dimensions, available_space, None);
            let fragment = match fc.layout(box_node, &constraints) {
              Ok(fragment) => fragment,
              Err(LayoutError::Timeout { .. }) => taffy::abort_layout_now(),
              Err(_) => return taffy::geometry::Size::ZERO,
            };
            let size = taffy::geometry::Size {
              width: fragment.bounds.width().max(0.0),
              height: fragment.bounds.height().max(0.0),
            };
            cache.insert(key, size);
            size
          }
        },
        cancel,
        TAFFY_ABORT_CHECK_STRIDE,
    );
    if let Some(start) = taffy_compute_start {
      record_taffy_compute(TaffyAdapterKind::Grid, start.elapsed());
    }
    compute_result.map_err(|e| match e {
      taffy::TaffyError::LayoutAborted => match check_active(RenderStage::Layout) {
        Err(RenderError::Timeout { elapsed, .. }) => LayoutError::Timeout { elapsed },
        _ => LayoutError::MissingContext("Taffy layout aborted".to_string()),
      },
      _ => LayoutError::MissingContext(format!("Taffy compute error: {:?}", e)),
    })?;

    let layout = taffy
      .layout(root_id)
      .map_err(|e| LayoutError::MissingContext(format!("Taffy layout error: {:?}", e)))?;

    Ok(layout.size.width)
  }

  #[allow(clippy::too_many_arguments)]
  fn measure_grid_item(
    &self,
    node_ptr: *const BoxNode,
    node_id: TaffyNodeId,
    known_dimensions: taffy::geometry::Size<Option<f32>>,
    available_space: taffy::geometry::Size<taffy::style::AvailableSpace>,
    parent_inline_base: Option<f32>,
    container_justify_items: AlignItems,
    factory: &crate::layout::contexts::factory::FormattingContextFactory,
    measure_cache: &Rc<RefCell<HashMap<MeasureKey, taffy::geometry::Size<f32>>>>,
    measured_fragments: &Rc<RefCell<HashMap<MeasureKey, FragmentNode>>>,
    measured_node_keys: &Rc<RefCell<HashMap<TaffyNodeId, Vec<MeasureKey>>>>,
  ) -> taffy::geometry::Size<f32> {
    let box_node = unsafe { &*node_ptr };
    let fallback_size = |known: Option<f32>, avail_dim: taffy::style::AvailableSpace| {
      known.unwrap_or(match avail_dim {
        taffy::style::AvailableSpace::Definite(v) => v,
        _ => 0.0,
      })
    };

    let key = MeasureKey::new(node_ptr, known_dimensions, available_space);
    if let Some(size) = measure_cache.borrow().get(&key) {
      return *size;
    }
    let mut constraints = constraints_from_taffy(
      self.viewport_size,
      known_dimensions,
      available_space,
      parent_inline_base,
    );

    // Taffy frequently probes min/max-content sizes during track sizing.
    // Avoid running full layout for these probes; use intrinsic sizing APIs instead.
    #[cfg(test)]
    if let Some(mut fragment) = GRID_TEST_MEASURE_HOOK.with(|hook| {
      hook
        .borrow()
        .as_ref()
        .and_then(|hook| hook(box_node).map(normalize_fragment_origin))
    }) {
      let percentage_base = match available_space.width {
        taffy::style::AvailableSpace::Definite(w) => w,
        _ => constraints
          .width()
          .unwrap_or_else(|| fragment.bounds.width()),
      };
      fragment.content = FragmentContent::Block {
        box_id: Some(box_node.id),
      };
      fragment.style = Some(box_node.style.clone());
      let content_size = self.content_box_size(&fragment, &box_node.style, percentage_base);
      let size = taffy::geometry::Size {
        width: content_size.width.max(0.0),
        height: content_size.height.max(0.0),
      };
      push_measured_key(
        measured_node_keys.borrow_mut().entry(node_id).or_default(),
        key,
      );
      measured_fragments.borrow_mut().insert(key, fragment);
      measure_cache.borrow_mut().insert(key, size);
      return size;
    }

    let fc_type = box_node
      .formatting_context()
      .unwrap_or(FormattingContextType::Block);
    let fc = factory.get(fc_type);

    let mut intrinsic_width: Option<f32> = None;
    if known_dimensions.width.is_none() {
      intrinsic_width = match available_space.width {
        taffy::style::AvailableSpace::MinContent => Some(
          match fc.compute_intrinsic_inline_size(box_node, IntrinsicSizingMode::MinContent) {
            Ok(size) => size,
            Err(LayoutError::Timeout { .. }) => taffy::abort_layout_now(),
            Err(_) => 0.0,
          },
        ),
        taffy::style::AvailableSpace::MaxContent => Some(
          match fc.compute_intrinsic_inline_size(box_node, IntrinsicSizingMode::MaxContent) {
            Ok(size) => size,
            Err(LayoutError::Timeout { .. }) => taffy::abort_layout_now(),
            Err(_) => 0.0,
          },
        ),
        _ => None,
      };
    }

    let mut intrinsic_height: Option<f32> = None;
    if known_dimensions.height.is_none() {
      intrinsic_height = match available_space.height {
        taffy::style::AvailableSpace::MinContent => Some(
          match fc.compute_intrinsic_block_size(box_node, IntrinsicSizingMode::MinContent) {
            Ok(size) => size,
            Err(LayoutError::Timeout { .. }) => taffy::abort_layout_now(),
            Err(_) => 0.0,
          },
        ),
        taffy::style::AvailableSpace::MaxContent => Some(
          match fc.compute_intrinsic_block_size(box_node, IntrinsicSizingMode::MaxContent) {
            Ok(size) => size,
            Err(LayoutError::Timeout { .. }) => taffy::abort_layout_now(),
            Err(_) => 0.0,
          },
        ),
        _ => None,
      };
    }

    if intrinsic_width.is_some() || intrinsic_height.is_some() {
      let percentage_base = match available_space.width {
        taffy::style::AvailableSpace::Definite(w) => w,
        _ => parent_inline_base.unwrap_or(0.0),
      };
      let (
        padding_left,
        padding_right,
        padding_top,
        padding_bottom,
        border_left,
        border_right,
        border_top,
        border_bottom,
      ) = self.resolved_padding_border_for_measure(&box_node.style, percentage_base);

      let width = intrinsic_width
        .map(|border_width| {
          (border_width - padding_left - padding_right - border_left - border_right).max(0.0)
        })
        .unwrap_or_else(|| fallback_size(known_dimensions.width, available_space.width).max(0.0));

      let height = intrinsic_height
        .map(|border_height| {
          (border_height - padding_top - padding_bottom - border_top - border_bottom).max(0.0)
        })
        .unwrap_or_else(|| fallback_size(known_dimensions.height, available_space.height).max(0.0));

      let size = taffy::geometry::Size { width, height };
      measure_cache.borrow_mut().insert(key, size);
      return size;
    }

    if known_dimensions.width.is_none()
      && matches!(
        available_space.width,
        taffy::style::AvailableSpace::Definite(_)
      )
      && box_node.style.width.is_none()
    {
      if let taffy::style::AvailableSpace::Definite(area_width) = available_space.width {
        let justify = box_node
          .style
          .justify_self
          .unwrap_or(container_justify_items);
        if justify != AlignItems::Stretch {
          match fc.compute_intrinsic_inline_size(box_node, IntrinsicSizingMode::MaxContent) {
            Ok(intrinsic_width) => {
              let used_width = intrinsic_width.min(area_width);
              constraints.available_width = CrateAvailableSpace::Definite(used_width);
              constraints.inline_percentage_base = Some(area_width);
            }
            Err(LayoutError::Timeout { .. }) => taffy::abort_layout_now(),
            Err(_) => {}
          }
        }
      }
    }

    record_measure_layout_call();
    let mut fragment = match fc.layout(box_node, &constraints) {
      Ok(fragment) => fragment,
      Err(LayoutError::Timeout { .. }) => taffy::abort_layout_now(),
      Err(_) => return taffy::geometry::Size::ZERO,
    };
    let percentage_base = match available_space.width {
      taffy::style::AvailableSpace::Definite(w) => w,
      _ => constraints
        .width()
        .unwrap_or_else(|| fragment.bounds.width()),
    };
    fragment.content = FragmentContent::Block {
      box_id: Some(box_node.id),
    };
    fragment.style = Some(box_node.style.clone());
    let fragment = normalize_fragment_origin(fragment);
    let content_size = self.content_box_size(&fragment, &box_node.style, percentage_base);
    let size = taffy::geometry::Size {
      width: content_size.width.max(0.0),
      height: content_size.height.max(0.0),
    };
    push_measured_key(
      measured_node_keys.borrow_mut().entry(node_id).or_default(),
      key,
    );
    measured_fragments.borrow_mut().insert(key, fragment);
    measure_cache.borrow_mut().insert(key, size);
    size
  }
}

fn grid_child_fingerprint(children: &[&BoxNode]) -> u64 {
  use std::hash::Hasher;
  let mut h = std::collections::hash_map::DefaultHasher::new();
  h.write_usize(children.len());
  for child in children {
    h.write_usize(child.id);
    h.write_usize(std::sync::Arc::as_ptr(&child.style) as usize);
  }
  h.finish()
}

fn translate_fragment_tree(fragment: &mut FragmentNode, delta: Point) {
  fragment.bounds = Rect::new(
    Point::new(fragment.bounds.x() + delta.x, fragment.bounds.y() + delta.y),
    fragment.bounds.size,
  );
  if let Some(logical) = fragment.logical_override {
    fragment.logical_override = Some(Rect::new(
      Point::new(logical.x() + delta.x, logical.y() + delta.y),
      logical.size,
    ));
  }
  for child in fragment.children_mut() {
    translate_fragment_tree(child, delta);
  }
}

fn normalize_fragment_origin(mut fragment: FragmentNode) -> FragmentNode {
  let origin = fragment.bounds.origin;
  if origin.x != 0.0 || origin.y != 0.0 {
    translate_fragment_tree(&mut fragment, Point::new(-origin.x, -origin.y));
  }
  fragment
}

#[cfg(test)]
thread_local! {
  static GRID_TEST_MEASURE_HOOK: RefCell<Option<Box<dyn Fn(&BoxNode) -> Option<FragmentNode>>>> =
    RefCell::new(None);
}

#[cfg(test)]
struct GridTestMeasureHookGuard;

#[cfg(test)]
fn set_grid_test_measure_hook(
  hook: impl Fn(&BoxNode) -> Option<FragmentNode> + 'static,
) -> GridTestMeasureHookGuard {
  GRID_TEST_MEASURE_HOOK.with(|slot| {
    *slot.borrow_mut() = Some(Box::new(hook));
  });
  GridTestMeasureHookGuard
}

#[cfg(test)]
impl Drop for GridTestMeasureHookGuard {
  fn drop(&mut self) {
    GRID_TEST_MEASURE_HOOK.with(|slot| {
      slot.borrow_mut().take();
    });
  }
}

fn find_first_baseline_absolute(fragment: &FragmentNode) -> Option<f32> {
  if let Some(baseline) = fragment.baseline {
    return Some(fragment.bounds.y() + baseline);
  }
  match &fragment.content {
    FragmentContent::Line { baseline } => return Some(fragment.bounds.y() + *baseline),
    FragmentContent::Text {
      baseline_offset, ..
    } => return Some(fragment.bounds.y() + *baseline_offset),
    _ => {}
  }

  for child in fragment.children.iter() {
    if let Some(b) = find_first_baseline_absolute(child) {
      return Some(b);
    }
  }

  None
}

fn first_baseline_offset(fragment: &FragmentNode) -> Option<f32> {
  find_first_baseline_absolute(fragment).map(|abs| abs - fragment.bounds.y())
}

fn apply_alignment_fallback_for_grid(
  free_space: f32,
  num_items: usize,
  alignment_mode: TaffyAlignContent,
) -> TaffyAlignContent {
  // Mirrors taffy's alignment fallback, but scoped locally for grid post-processing.
  if num_items <= 1 || free_space <= 0.0 {
    return match alignment_mode {
      TaffyAlignContent::Stretch => TaffyAlignContent::FlexStart,
      TaffyAlignContent::SpaceBetween => TaffyAlignContent::FlexStart,
      TaffyAlignContent::SpaceAround => TaffyAlignContent::Center,
      TaffyAlignContent::SpaceEvenly => TaffyAlignContent::Center,
      other => other,
    };
  }

  alignment_mode
}

fn compute_alignment_offset_for_grid(
  free_space: f32,
  num_items: usize,
  alignment_mode: TaffyAlignContent,
  is_first: bool,
) -> f32 {
  if is_first {
    match alignment_mode {
      TaffyAlignContent::Start => 0.0,
      TaffyAlignContent::FlexStart => 0.0,
      TaffyAlignContent::End => free_space,
      TaffyAlignContent::FlexEnd => free_space,
      TaffyAlignContent::Center => free_space / 2.0,
      TaffyAlignContent::Stretch => 0.0,
      TaffyAlignContent::SpaceBetween => 0.0,
      TaffyAlignContent::SpaceAround => {
        if free_space >= 0.0 {
          (free_space / num_items as f32) / 2.0
        } else {
          free_space / 2.0
        }
      }
      TaffyAlignContent::SpaceEvenly => {
        if free_space >= 0.0 {
          free_space / (num_items + 1) as f32
        } else {
          free_space / 2.0
        }
      }
    }
  } else {
    let free_space = free_space.max(0.0);
    match alignment_mode {
      TaffyAlignContent::Start
      | TaffyAlignContent::FlexStart
      | TaffyAlignContent::End
      | TaffyAlignContent::FlexEnd
      | TaffyAlignContent::Center
      | TaffyAlignContent::Stretch => 0.0,
      TaffyAlignContent::SpaceBetween => free_space / (num_items.saturating_sub(1).max(1) as f32),
      TaffyAlignContent::SpaceAround => free_space / (num_items.max(1) as f32),
      TaffyAlignContent::SpaceEvenly => free_space / (num_items + 1) as f32,
    }
  }
}

fn compute_track_offsets(
  tracks: &DetailedGridTracksInfo,
  axis_size: f32,
  padding_start: f32,
  padding_end: f32,
  border_start: f32,
  border_end: f32,
  alignment: TaffyAlignContent,
) -> Vec<f32> {
  let track_count = tracks.sizes.len();
  if track_count == 0 {
    return Vec::new();
  }

  let mut gutters = tracks.gutters.clone();
  if gutters.len() < track_count + 1 {
    gutters.resize(track_count + 1, 0.0);
  }

  #[derive(Clone, Copy)]
  struct TrackEntry {
    size: f32,
    is_gutter: bool,
  }

  let mut entries: Vec<TrackEntry> = Vec::with_capacity(track_count * 2 + 1);
  entries.push(TrackEntry {
    size: gutters.get(0).copied().unwrap_or(0.0),
    is_gutter: true,
  });
  for i in 0..track_count {
    entries.push(TrackEntry {
      size: tracks.sizes[i],
      is_gutter: false,
    });
    entries.push(TrackEntry {
      size: gutters.get(i + 1).copied().unwrap_or(0.0),
      is_gutter: true,
    });
  }

  let used_size: f32 = entries.iter().map(|t| t.size).sum();
  let content_size = axis_size - padding_start - padding_end - border_start - border_end;
  let free_space = content_size - used_size;
  let aligned = apply_alignment_fallback_for_grid(free_space, track_count, alignment);

  let mut offsets = Vec::with_capacity(entries.len());
  let mut total_offset = padding_start + border_start;
  let mut first_track_seen = false;
  for entry in entries {
    let is_first_track = !first_track_seen && !entry.is_gutter;
    if is_first_track {
      first_track_seen = true;
    }
    let offset_within = if entry.is_gutter {
      0.0
    } else {
      compute_alignment_offset_for_grid(free_space, track_count.max(1), aligned, is_first_track)
    };
    offsets.push(total_offset + offset_within);
    total_offset += offset_within + entry.size;
  }

  offsets
}

fn grid_area_for_item(offsets: &[f32], start_line: u16, end_line: u16) -> Option<(f32, f32)> {
  let start_idx = (start_line.saturating_sub(1) as usize).saturating_mul(2);
  let end_idx = (end_line.saturating_sub(1) as usize).saturating_mul(2);
  if start_idx + 1 >= offsets.len() || end_idx >= offsets.len() {
    return None;
  }
  Some((offsets[start_idx + 1], offsets[end_idx]))
}

#[derive(Clone, Copy)]
struct BaselineItem {
  idx: usize,
  area_start: f32,
  area_end: f32,
  baseline: f32,
  start: f32,
  size: f32,
}

fn translate_along_axis(fragment: &mut FragmentNode, axis: Axis, delta: f32) {
  if delta == 0.0 {
    return;
  }
  let delta_point = match axis {
    Axis::Horizontal => Point::new(delta, 0.0),
    Axis::Vertical => Point::new(0.0, delta),
  };
  translate_fragment_tree(fragment, delta_point);
}

fn parse_grid_line_placement_raw(raw: &str) -> Line<TaffyGridPlacement<String>> {
  let mut parts = raw.splitn(2, '/').map(|s| s.trim());
  let start_str = parts.next().unwrap_or("auto");
  let end_str = parts.next().unwrap_or("auto");
  Line {
    start: parse_grid_line_component(start_str),
    end: parse_grid_line_component(end_str),
  }
}

fn parse_grid_line_component(token: &str) -> TaffyGridPlacement<String> {
  let trimmed = token.trim();
  if trimmed.is_empty() || trimmed.eq_ignore_ascii_case("auto") {
    return TaffyGridPlacement::Auto;
  }

  let parts: Vec<&str> = trimmed
    .split_whitespace()
    .filter(|p| !p.is_empty())
    .collect();
  if parts.is_empty() {
    return TaffyGridPlacement::Auto;
  }

  // Span syntax: span && (<integer> || <custom-ident>) in any order
  if parts[0].eq_ignore_ascii_case("span") {
    let mut name: Option<String> = None;
    let mut count: Option<u16> = None;
    for part in parts.iter().skip(1) {
      if count.is_none() {
        if let Ok(n) = part.parse::<i32>() {
          if n > 0 {
            count = Some(n as u16);
            continue;
          }
        }
      }
      if name.is_none() {
        name = Some((*part).to_string());
      }
    }

    return match (name, count) {
      (Some(name), Some(count)) => TaffyGridPlacement::NamedSpan(name, count.max(1)),
      (Some(name), None) => TaffyGridPlacement::NamedSpan(name, 1),
      (None, Some(count)) => TaffyGridPlacement::Span(count.max(1)),
      (None, None) => TaffyGridPlacement::Span(1),
    };
  }

  // Non-span grammar: <custom-ident>? <integer>? in any order (integer controls the nth occurrence)
  let mut number: Option<i16> = None;
  let mut name: Option<String> = None;
  for part in &parts {
    if number.is_none() {
      if let Ok(n) = part.parse::<i16>() {
        number = Some(n);
        continue;
      }
    }
    if name.is_none() {
      name = Some((*part).to_string());
    }
  }

  match (name, number) {
    (Some(name), Some(idx)) => TaffyGridPlacement::NamedLine(name, idx),
    (Some(name), None) => TaffyGridPlacement::NamedLine(name, 1),
    (None, Some(idx)) => TaffyGridPlacement::Line(idx.into()),
    (None, None) => TaffyGridPlacement::Auto,
  }
}

impl Default for GridFormattingContext {
  fn default() -> Self {
    Self::new()
  }
}

impl FormattingContext for GridFormattingContext {
  fn layout(
    &self,
    box_node: &BoxNode,
    constraints: &LayoutConstraints,
  ) -> Result<FragmentNode, LayoutError> {
    debug_assert!(
      matches!(
        box_node.formatting_context(),
        Some(FormattingContextType::Grid)
      ),
      "GridFormattingContext must only layout grid containers",
    );
    let _profile = layout_timer(LayoutKind::Grid);
    if let Err(RenderError::Timeout { elapsed, .. }) = check_active(RenderStage::Layout) {
      return Err(LayoutError::Timeout { elapsed });
    }
    if let Some(cached) = layout_cache_lookup(
      box_node,
      FormattingContextType::Grid,
      constraints,
      self.viewport_size,
    ) {
      return Ok(cached);
    }

    let trace_grid_layout =
      crate::debug::runtime::runtime_toggles().truthy("FASTR_TRACE_GRID_LAYOUT");
    let grid_trace_start = trace_grid_layout.then(std::time::Instant::now);

    // Create fresh Taffy tree for this layout
    let mut taffy: TaffyTree<*const BoxNode> = TaffyTree::new();
    let mut positioned_children_map: HashMap<TaffyNodeId, Vec<BoxNode>> = HashMap::new();

    // Partition children into in-flow vs. out-of-flow positioned.
    let mut in_flow_children: Vec<&BoxNode> = Vec::new();
    let mut positioned_children: Vec<BoxNode> = Vec::new();
    for child in &box_node.children {
      match child.style.position {
        crate::style::position::Position::Absolute | crate::style::position::Position::Fixed => {
          positioned_children.push(child.clone());
        }
        _ => in_flow_children.push(child),
      }
    }

    // Build Taffy tree from in-flow children
    let root_id = self.build_taffy_tree_children(
      &mut taffy,
      box_node,
      &in_flow_children,
      constraints,
      &mut positioned_children_map,
    )?;

    if trace_grid_layout {
      let selector = box_node
        .debug_info
        .as_ref()
        .map(|d| d.to_selector())
        .unwrap_or_else(|| "<anon>".to_string());
      eprintln!(
        "[grid-layout] start id={} in_flow_children={} selector={}",
        box_node.id,
        in_flow_children.len(),
        selector
      );
    }

    // Block-level grid containers with `width: auto` should stretch to the available inline size.
    // Taffy treats `size: auto` as shrink-to-fit in some grid cases, so force a definite width
    // when we have one from the parent constraints.
    if let CrateAvailableSpace::Definite(outer_width) = constraints.available_width {
      if outer_width.is_finite()
        && box_node.style.width.is_none()
        && box_node.box_type.is_block_level()
        && self.inline_axis_is_horizontal(&box_node.style)
      {
        let percentage_base = outer_width;
        let padding_left = self.resolve_length_for_width(
          box_node.style.padding_left,
          percentage_base,
          &box_node.style,
        );
        let padding_right = self.resolve_length_for_width(
          box_node.style.padding_right,
          percentage_base,
          &box_node.style,
        );
        let border_left = self.resolve_length_for_width(
          box_node.style.border_left_width,
          percentage_base,
          &box_node.style,
        );
        let border_right = self.resolve_length_for_width(
          box_node.style.border_right_width,
          percentage_base,
          &box_node.style,
        );
        let content_width =
          (outer_width - padding_left - padding_right - border_left - border_right).max(0.0);
        if let Ok(existing) = taffy.style(root_id) {
          if existing.size.width.is_auto() {
            let mut updated = existing.clone();
            updated.size.width = Dimension::length(content_width);
            taffy
              .set_style(root_id, updated)
              .map_err(|e| LayoutError::MissingContext(format!("Taffy error: {:?}", e)))?;
          }
        }
      }
    }

    // Convert constraints to Taffy available space
    let available_space = taffy::geometry::Size {
      width: match constraints.available_width {
        CrateAvailableSpace::Definite(w) => taffy::style::AvailableSpace::Definite(w),
        CrateAvailableSpace::Indefinite => taffy::style::AvailableSpace::MaxContent,
        CrateAvailableSpace::MinContent => taffy::style::AvailableSpace::MinContent,
        CrateAvailableSpace::MaxContent => taffy::style::AvailableSpace::MaxContent,
      },
      height: match constraints.available_height {
        CrateAvailableSpace::Definite(h) => taffy::style::AvailableSpace::Definite(h),
        CrateAvailableSpace::Indefinite => taffy::style::AvailableSpace::MaxContent,
        CrateAvailableSpace::MinContent => taffy::style::AvailableSpace::MinContent,
        CrateAvailableSpace::MaxContent => taffy::style::AvailableSpace::MaxContent,
      },
    };

    // Run Taffy layout
    record_taffy_invocation(TaffyAdapterKind::Grid);
    let taffy_perf_enabled = crate::layout::taffy_integration::taffy_perf_enabled();
    let taffy_compute_start = taffy_perf_enabled.then(std::time::Instant::now);
    // Render pipeline always installs a deadline guard (even when disabled), so only enable
    // the Taffy cancellation path when the active deadline is actually configured.
    let cancel: Option<Arc<dyn Fn() -> bool + Send + Sync>> = active_deadline()
      .filter(|deadline| deadline.is_enabled())
      .map(|_| Arc::new(|| check_active(RenderStage::Layout).is_err()) as _);
    let measure_cache: Rc<RefCell<HashMap<MeasureKey, taffy::geometry::Size<f32>>>> =
      Rc::new(RefCell::new(HashMap::new()));
    let measured_fragments: Rc<RefCell<HashMap<MeasureKey, FragmentNode>>> =
      Rc::new(RefCell::new(HashMap::new()));
    let measured_node_keys: Rc<RefCell<HashMap<TaffyNodeId, Vec<MeasureKey>>>> =
      Rc::new(RefCell::new(HashMap::new()));
    let compute_result = taffy.compute_layout_with_measure_and_cancel(
        root_id,
        available_space,
        {
          let this = self.clone();
          let parent_inline_base = constraints.inline_percentage_base;
          let container_justify_items = box_node.style.justify_items;
          let cache = measure_cache.clone();
          let measured = measured_fragments.clone();
          let measured_keys = measured_node_keys.clone();
          move |known_dimensions,
                available_space,
                node_id,
                node_context,
                _style: &taffy::style::Style| {
            if taffy_perf_enabled {
              record_taffy_measure_call(TaffyAdapterKind::Grid);
            }
            let fallback_size =
              |known: Option<f32>, avail_dim: taffy::style::AvailableSpace| {
                known.unwrap_or(match avail_dim {
                  taffy::style::AvailableSpace::Definite(v) => v,
                  _ => 0.0,
                })
              };

            if node_id == root_id {
              let outer_width = fallback_size(known_dimensions.width, available_space.width);
              let outer_height = known_dimensions.height.unwrap_or(0.0);
              let Some(node_ptr) = node_context.as_ref().map(|p| **p) else {
                return taffy::geometry::Size {
                  width: outer_width,
                  height: outer_height,
                };
              };
              let root_box_node = unsafe { &*node_ptr };
              let percentage_base = outer_width;
              let padding_left = this.resolve_length_for_width(
                root_box_node.style.padding_left,
                percentage_base,
                &root_box_node.style,
              );
              let padding_right = this.resolve_length_for_width(
                root_box_node.style.padding_right,
                percentage_base,
                &root_box_node.style,
              );
              let padding_top = this.resolve_length_for_width(
                root_box_node.style.padding_top,
                percentage_base,
                &root_box_node.style,
              );
              let padding_bottom = this.resolve_length_for_width(
                root_box_node.style.padding_bottom,
                percentage_base,
                &root_box_node.style,
              );
              let border_left = this.resolve_length_for_width(
                root_box_node.style.border_left_width,
                percentage_base,
                &root_box_node.style,
              );
              let border_right = this.resolve_length_for_width(
                root_box_node.style.border_right_width,
                percentage_base,
                &root_box_node.style,
              );
              let border_top = this.resolve_length_for_width(
                root_box_node.style.border_top_width,
                percentage_base,
                &root_box_node.style,
              );
              let border_bottom = this.resolve_length_for_width(
                root_box_node.style.border_bottom_width,
                percentage_base,
                &root_box_node.style,
              );
              let content_width =
                (outer_width - padding_left - padding_right - border_left - border_right).max(0.0);
              let content_height =
                (outer_height - padding_top - padding_bottom - border_top - border_bottom)
                  .max(0.0);
              return taffy::geometry::Size {
                width: content_width,
                height: content_height,
              };
            }

            let Some(node_ptr) = node_context.as_ref().map(|p| **p) else {
              return taffy::geometry::Size::ZERO;
            };
            this.measure_grid_item(
              node_ptr,
              node_id,
              known_dimensions,
              available_space,
              parent_inline_base,
              container_justify_items,
              &this.factory,
              &cache,
              &measured,
              &measured_keys,
            )
          }
        },
        cancel,
        TAFFY_ABORT_CHECK_STRIDE,
    );
    if let Some(start) = taffy_compute_start {
      record_taffy_compute(TaffyAdapterKind::Grid, start.elapsed());
    }
    compute_result.map_err(|e| match e {
      taffy::TaffyError::LayoutAborted => match check_active(RenderStage::Layout) {
        Err(RenderError::Timeout { elapsed, .. }) => LayoutError::Timeout { elapsed },
        _ => LayoutError::MissingContext("Taffy layout aborted".to_string()),
      },
      _ => LayoutError::MissingContext(format!("Taffy compute error: {:?}", e)),
    })?;

    let measured_node_keys = measured_node_keys.borrow();

    if let Some(start) = grid_trace_start {
      let elapsed_ms = start.elapsed().as_secs_f64() * 1000.0;
      eprintln!("[grid-layout] done id={} ms={elapsed_ms:.2}", box_node.id);
    }

    // Convert back to FragmentNode tree and layout each in-flow child using its formatting context.
    let mut fragment = self.convert_to_fragments(
      &taffy,
      root_id,
      root_id,
      &constraints,
      &measured_fragments,
      &measured_node_keys,
      &positioned_children_map,
    )?;

    if let Some(trace_id) = crate::debug::runtime::runtime_toggles().usize("FASTR_TRACE_GRID_TEXT")
    {
      if trace_id == box_node.id {
        fn count_texts(node: &FragmentNode) -> usize {
          let mut count = 0;
          fn walk(node: &FragmentNode, count: &mut usize) {
            if matches!(node.content, FragmentContent::Text { .. }) {
              *count += 1;
            }
            for child in node.children.iter() {
              walk(child, count);
            }
          }
          walk(node, &mut count);
          count
        }
        eprintln!(
          "[grid-text] id={} selector={:?} text_fragments={}",
          box_node.id,
          box_node.debug_info.as_ref().map(|d| d.to_selector()),
          count_texts(&fragment)
        );
      }
    }

    // Position out-of-flow children against the appropriate containing block.
    if !positioned_children.is_empty() {
      let padding_left = self.resolve_length_for_width(
        box_node.style.padding_left,
        constraints.width().unwrap_or(0.0),
        &box_node.style,
      );
      let padding_top = self.resolve_length_for_width(
        box_node.style.padding_top,
        constraints.width().unwrap_or(0.0),
        &box_node.style,
      );
      let border_left = self.resolve_length_for_width(
        box_node.style.border_left_width,
        constraints.width().unwrap_or(0.0),
        &box_node.style,
      );
      let border_top = self.resolve_length_for_width(
        box_node.style.border_top_width,
        constraints.width().unwrap_or(0.0),
        &box_node.style,
      );
      let border_right = self.resolve_length_for_width(
        box_node.style.border_right_width,
        constraints.width().unwrap_or(0.0),
        &box_node.style,
      );
      let border_bottom = self.resolve_length_for_width(
        box_node.style.border_bottom_width,
        constraints.width().unwrap_or(0.0),
        &box_node.style,
      );
      let padding_origin =
        crate::geometry::Point::new(border_left + padding_left, border_top + padding_top);
      let padding_size = crate::geometry::Size::new(
        fragment.bounds.width() - border_left - border_right,
        fragment.bounds.height() - border_top - border_bottom,
      );
      let padding_rect = crate::geometry::Rect::new(padding_origin, padding_size);
      let padding_bottom = padding_rect.size.height - padding_top;
      let padding_right = padding_rect.size.width - padding_left;

      let block_base = if box_node.style.height.is_some() {
        Some(padding_rect.size.height)
      } else {
        None
      };
      let establishes_abs_cb = box_node.style.position.is_positioned()
        || !box_node.style.transform.is_empty()
        || box_node.style.perspective.is_some()
        || box_node.style.containment.layout
        || box_node.style.containment.paint;
      let establishes_fixed_cb = !box_node.style.transform.is_empty()
        || box_node.style.perspective.is_some()
        || box_node.style.containment.layout
        || box_node.style.containment.paint;
      let padding_cb =
        crate::layout::contexts::positioned::ContainingBlock::with_viewport_and_bases(
          padding_rect,
          self.viewport_size,
          Some(padding_rect.size.width),
          block_base,
        );
      let cb_for_absolute = if establishes_abs_cb {
        padding_cb
      } else {
        self.nearest_positioned_cb
      };

      let mut static_positions: HashMap<usize, Point> = HashMap::new();
      if let DetailedLayoutInfo::Grid(info) = taffy.detailed_layout_info(root_id) {
        if let Ok(container_style) = taffy.style(root_id) {
          let row_offsets = compute_track_offsets(
            &info.rows,
            fragment.bounds.height(),
            padding_top,
            padding_bottom,
            border_top,
            border_bottom,
            container_style
              .align_content
              .unwrap_or(taffy::style::AlignContent::Stretch),
          );
          let col_offsets = compute_track_offsets(
            &info.columns,
            fragment.bounds.width(),
            padding_left,
            padding_right,
            border_left,
            border_right,
            container_style
              .justify_content
              .unwrap_or(taffy::style::AlignContent::Stretch),
          );

          for child in &positioned_children {
            let mut pos = Point::ZERO;
            if child.style.grid_column_start > 0
              && child.style.grid_column_end > 0
              && child.style.grid_column_end == child.style.grid_column_start + 1
            {
              if let Some((start, _)) = grid_area_for_item(
                &col_offsets,
                child.style.grid_column_start as u16,
                child.style.grid_column_end as u16,
              ) {
                pos.x = start - padding_origin.x;
              }
            }
            if child.style.grid_row_start > 0
              && child.style.grid_row_end > 0
              && child.style.grid_row_end == child.style.grid_row_start + 1
            {
              if let Some((start, _)) = grid_area_for_item(
                &row_offsets,
                child.style.grid_row_start as u16,
                child.style.grid_row_end as u16,
              ) {
                pos.y = start - padding_origin.y;
              }
            }
            static_positions.insert(child.id, pos);
          }
        }
      }

      let abs = crate::layout::absolute_positioning::AbsoluteLayout::with_font_context(
        self.font_context.clone(),
      );
      for child in &positioned_children {
        // Layout child as static for intrinsic size.
        let mut layout_child = child.clone();
        let mut style = (*layout_child.style).clone();
        style.position = crate::style::position::Position::Relative;
        style.top = None;
        style.right = None;
        style.bottom = None;
        style.left = None;
        layout_child.style = Arc::new(style);

        let cb = match child.style.position {
          crate::style::position::Position::Fixed => {
            if establishes_fixed_cb {
              padding_cb
            } else {
              crate::layout::contexts::positioned::ContainingBlock::viewport(self.viewport_size)
            }
          }
          _ => cb_for_absolute,
        };

        let factory = self.factory.with_positioned_cb(cb);
        let fc_type = layout_child
          .formatting_context()
          .unwrap_or(crate::style::display::FormattingContextType::Block);
        let fc = factory.get(fc_type);
        let child_constraints = LayoutConstraints::new(
          CrateAvailableSpace::Definite(padding_rect.size.width),
          block_base
            .map(CrateAvailableSpace::Definite)
            .unwrap_or(CrateAvailableSpace::Indefinite),
        );
        let mut child_fragment = fc.layout(&layout_child, &child_constraints)?;

        let positioned_style = crate::layout::absolute_positioning::resolve_positioned_style(
          &child.style,
          &cb,
          self.viewport_size,
          &self.font_context,
        );
        // Static position resolves to where the element would be in flow; use the
        // content origin here since AbsoluteLayout adds padding/border.
        let static_pos = static_positions
          .get(&child.id)
          .copied()
          .unwrap_or(crate::geometry::Point::ZERO);
        let needs_inline_intrinsics = positioned_style.width.is_auto()
          && (positioned_style.left.is_auto()
            || positioned_style.right.is_auto()
            || child.is_replaced());
        let needs_block_intrinsics = positioned_style.height.is_auto()
          && (positioned_style.top.is_auto() || positioned_style.bottom.is_auto());
        let preferred_min_inline = if needs_inline_intrinsics {
          match fc.compute_intrinsic_inline_size(&layout_child, IntrinsicSizingMode::MinContent) {
            Ok(size) => Some(size),
            Err(err @ LayoutError::Timeout { .. }) => return Err(err),
            Err(_) => None,
          }
        } else {
          None
        };
        let preferred_inline = if needs_inline_intrinsics {
          match fc.compute_intrinsic_inline_size(&layout_child, IntrinsicSizingMode::MaxContent) {
            Ok(size) => Some(size),
            Err(err @ LayoutError::Timeout { .. }) => return Err(err),
            Err(_) => None,
          }
        } else {
          None
        };
        let preferred_min_block = if needs_block_intrinsics {
          match fc.compute_intrinsic_block_size(&layout_child, IntrinsicSizingMode::MinContent) {
            Ok(size) => Some(size),
            Err(err @ LayoutError::Timeout { .. }) => return Err(err),
            Err(_) => None,
          }
        } else {
          None
        };
        let preferred_block = if needs_block_intrinsics {
          match fc.compute_intrinsic_block_size(&layout_child, IntrinsicSizingMode::MaxContent) {
            Ok(size) => Some(size),
            Err(err @ LayoutError::Timeout { .. }) => return Err(err),
            Err(_) => None,
          }
        } else {
          None
        };

        let mut input = crate::layout::absolute_positioning::AbsoluteLayoutInput::new(
          positioned_style,
          child_fragment.bounds.size,
          static_pos,
        );
        input.is_replaced = child.is_replaced();
        input.preferred_min_inline_size = preferred_min_inline;
        input.preferred_inline_size = preferred_inline;
        input.preferred_min_block_size = preferred_min_block;
        input.preferred_block_size = preferred_block;
        let result = abs.layout_absolute(&input, &cb)?;
        let needs_relayout = (result.size.width - child_fragment.bounds.width()).abs() > 0.01
          || (result.size.height - child_fragment.bounds.height()).abs() > 0.01;
        if needs_relayout {
          let relayout_constraints = LayoutConstraints::new(
            CrateAvailableSpace::Definite(result.size.width),
            CrateAvailableSpace::Definite(result.size.height),
          );
          let mut relayout_child = layout_child.clone();
          let mut relayout_style = (*relayout_child.style).clone();
          relayout_style.width = Some(Length::px(result.size.width));
          relayout_style.height = Some(Length::px(result.size.height));
          relayout_child.style = Arc::new(relayout_style);
          child_fragment = fc.layout(&relayout_child, &relayout_constraints)?;
        }
        child_fragment.bounds = crate::geometry::Rect::new(result.position, result.size);
        child_fragment.style = Some(child.style.clone());
        fragment.children_mut().push(child_fragment);
      }
    }
    layout_cache_store(
      box_node,
      FormattingContextType::Grid,
      constraints,
      &fragment,
      self.viewport_size,
    );

    Ok(fragment)
  }

  fn compute_intrinsic_inline_size(
    &self,
    box_node: &BoxNode,
    mode: IntrinsicSizingMode,
  ) -> Result<f32, LayoutError> {
    self.compute_intrinsic_size(box_node, mode)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::api::{DiagnosticsLevel, FastRender, FastRenderConfig, RenderOptions};
  use crate::debug::runtime;
  use crate::style::display::FormattingContextType;
  use crate::style::types::AlignItems;
  use crate::style::types::AspectRatio;
  use crate::style::types::GridAutoFlow;
  use crate::style::types::GridTrack;
  use crate::style::types::Overflow;
  use crate::style::types::ScrollbarWidth;
  use crate::style::types::WritingMode;
  use crate::text::font_db::FontConfig;
  use crate::tree::box_tree::BoxTree;
  use std::collections::HashMap;
  use std::sync::Arc;

  fn make_grid_style() -> Arc<ComputedStyle> {
    let mut style = ComputedStyle::default();
    style.display = CssDisplay::Grid;
    Arc::new(style)
  }

  fn make_item_style() -> Arc<ComputedStyle> {
    Arc::new(ComputedStyle::default())
  }

  fn make_grid_style_with_tracks(cols: Vec<GridTrack>, rows: Vec<GridTrack>) -> Arc<ComputedStyle> {
    let mut style = ComputedStyle::default();
    style.display = CssDisplay::Grid;
    style.grid_template_columns = cols;
    style.grid_template_rows = rows;
    Arc::new(style)
  }

  fn make_text_item(text: &str, font_size: f32) -> BoxNode {
    let mut style = ComputedStyle::default();
    style.font_size = font_size;
    let style = Arc::new(style);
    let text_child = BoxNode::new_text(style.clone(), text.to_string());
    BoxNode::new_block(style, FormattingContextType::Inline, vec![text_child])
  }

  #[test]
  fn grid_taffy_abort_surfaces_as_timeout() {
    use crate::render_control::{DeadlineGuard, RenderDeadline};
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::time::Duration;

    // Ensure the deadline is not tripped by the initial check, but is tripped during Taffy.
    let counter = Arc::new(AtomicUsize::new(0));
    let counter_clone = counter.clone();
    let deadline = RenderDeadline::new(
      None,
      Some(Arc::new(move || {
        let prev = counter_clone.fetch_add(1, Ordering::SeqCst);
        prev >= 1
      })),
    );
    let _guard = DeadlineGuard::install(Some(&deadline));

    let child = BoxNode::new_block(make_item_style(), FormattingContextType::Block, vec![]);
    let container = BoxNode::new_block(make_grid_style(), FormattingContextType::Grid, vec![child]);
    let constraints = LayoutConstraints::definite(100.0, 100.0);

    let gc = GridFormattingContext::new();
    let result = gc.layout(&container, &constraints);

    match result {
      Err(LayoutError::Timeout { elapsed }) => {
        assert!(elapsed >= Duration::from_secs(0));
      }
      other => panic!("expected LayoutError::Timeout from Taffy abort, got {other:?}"),
    }
  }

  #[test]
  fn measure_key_quantizes_definite_sizes() {
    use taffy::style::AvailableSpace;

    let node = BoxNode::new_block(make_item_style(), FormattingContextType::Block, vec![]);
    let ptr = &node as *const _;
    let known_a = taffy::geometry::Size {
      width: Some(300.3),
      height: Some(150.7),
    };
    let avail_a = taffy::geometry::Size {
      width: AvailableSpace::Definite(500.2),
      height: AvailableSpace::Definite(600.4),
    };
    let known_b = taffy::geometry::Size {
      width: Some(300.6),
      height: Some(150.4),
    };
    let avail_b = taffy::geometry::Size {
      width: AvailableSpace::Definite(500.6),
      height: AvailableSpace::Definite(600.2),
    };

    let key_a = MeasureKey::new(ptr, known_a, avail_a);
    let key_b = MeasureKey::new(ptr, known_b, avail_b);
    assert_eq!(
      key_a, key_b,
      "near-identical definite sizes should quantize to the same key"
    );

    let min_key = MeasureKey::new(
      ptr,
      known_a,
      taffy::geometry::Size {
        width: AvailableSpace::MinContent,
        height: avail_a.height,
      },
    );
    let max_key = MeasureKey::new(
      ptr,
      known_a,
      taffy::geometry::Size {
        width: AvailableSpace::MaxContent,
        height: avail_a.height,
      },
    );
    assert_ne!(min_key.available_width, max_key.available_width);
  }

  #[test]
  fn measured_node_keys_are_capped_per_node() {
    use taffy::style::AvailableSpace;

    let gc = GridFormattingContext::new();
    let factory = gc.factory.clone();
    let measure_cache: Rc<RefCell<HashMap<MeasureKey, taffy::geometry::Size<f32>>>> =
      Rc::new(RefCell::new(HashMap::new()));
    let measured_fragments: Rc<RefCell<HashMap<MeasureKey, FragmentNode>>> =
      Rc::new(RefCell::new(HashMap::new()));
    let measured_node_keys: Rc<RefCell<HashMap<TaffyNodeId, Vec<MeasureKey>>>> =
      Rc::new(RefCell::new(HashMap::new()));

    let node = BoxNode::new_block(make_item_style(), FormattingContextType::Block, vec![]);
    let node_ptr = &node as *const _;
    let node_id = TaffyNodeId::from(1u64);
    let known = taffy::geometry::Size {
      width: None,
      height: None,
    };

    for i in 0..(MAX_MEASURED_KEYS_PER_NODE + 5) {
      let avail = taffy::geometry::Size {
        width: AvailableSpace::Definite(20.0 + (i as f32 * 10.0)),
        height: AvailableSpace::Definite(40.0 + (i as f32 * 5.0)),
      };
      let _ = gc.measure_grid_item(
        node_ptr,
        node_id,
        known,
        avail,
        None,
        AlignItems::Stretch,
        &factory,
        &measure_cache,
        &measured_fragments,
        &measured_node_keys,
      );
    }

    let keys = measured_node_keys.borrow();
    let node_keys = keys.get(&node_id).expect("measured keys");
    assert!(
      node_keys.len() <= MAX_MEASURED_KEYS_PER_NODE,
      "measured keys should be capped per node"
    );
    let first_key = MeasureKey::new(
      node_ptr,
      known,
      taffy::geometry::Size {
        width: AvailableSpace::Definite(20.0),
        height: AvailableSpace::Definite(40.0),
      },
    );
    assert!(
      !node_keys.contains(&first_key),
      "oldest measured keys should be evicted when exceeding the cap"
    );
  }

  #[test]
  fn grid_measure_quantization_limits_layout_calls() {
    use taffy::style::AvailableSpace;

    let gc = GridFormattingContext::new();
    let factory = gc.factory.clone();
    let measure_cache: Rc<RefCell<HashMap<MeasureKey, taffy::geometry::Size<f32>>>> =
      Rc::new(RefCell::new(HashMap::new()));
    let measured_fragments: Rc<RefCell<HashMap<MeasureKey, FragmentNode>>> =
      Rc::new(RefCell::new(HashMap::new()));
    let measured_node_keys: Rc<RefCell<HashMap<TaffyNodeId, Vec<MeasureKey>>>> =
      Rc::new(RefCell::new(HashMap::new()));

    reset_grid_measure_layout_calls();

    let known = taffy::geometry::Size {
      width: None,
      height: None,
    };
    let widths = [300.2, 300.6, 300.1, 300.8, 300.4];
    let heights = [150.7, 150.4, 150.9];

    let nodes: Vec<BoxNode> = (0..12)
      .map(|_| BoxNode::new_block(make_item_style(), FormattingContextType::Block, vec![]))
      .collect();

    for (i, node) in nodes.iter().enumerate() {
      let node_id = TaffyNodeId::from((i + 1) as u64);
      let node_ptr = node as *const _;
      for width in widths {
        for height in heights {
          let avail = taffy::geometry::Size {
            width: AvailableSpace::Definite(width + (i as f32 * 0.01)),
            height: AvailableSpace::Definite(height),
          };
          let _ = gc.measure_grid_item(
            node_ptr,
            node_id,
            known,
            avail,
            None,
            AlignItems::Stretch,
            &factory,
            &measure_cache,
            &measured_fragments,
            &measured_node_keys,
          );
        }
      }
    }

    let calls = grid_measure_layout_calls();
    assert!(
      calls > 0 && calls <= nodes.len() * 3,
      "quantized keys should coalesce near-identical probes (calls={calls})"
    );
  }

  #[test]
  fn measure_grid_item_short_circuits_min_content_height_probes() {
    use taffy::style::AvailableSpace;

    reset_grid_measure_layout_calls();

    let gc = GridFormattingContext::new();
    let factory =
      crate::layout::contexts::factory::FormattingContextFactory::with_font_context_viewport_and_cb(
        gc.font_context.clone(),
        gc.viewport_size,
        gc.nearest_positioned_cb,
      );
    let measure_cache: Rc<RefCell<HashMap<MeasureKey, taffy::geometry::Size<f32>>>> =
      Rc::new(RefCell::new(HashMap::new()));
    let measured_fragments: Rc<RefCell<HashMap<MeasureKey, FragmentNode>>> =
      Rc::new(RefCell::new(HashMap::new()));
    let measured_node_keys: Rc<RefCell<HashMap<TaffyNodeId, Vec<MeasureKey>>>> =
      Rc::new(RefCell::new(HashMap::new()));

    let mut node = make_text_item("hello world", 16.0);
    node.id = 1;
    let node_ptr = &node as *const _;

    let size = gc.measure_grid_item(
      node_ptr,
      TaffyNodeId::from(1u64),
      taffy::geometry::Size {
        width: Some(200.0),
        height: None,
      },
      taffy::geometry::Size {
        width: AvailableSpace::Definite(200.0),
        height: AvailableSpace::MinContent,
      },
      Some(200.0),
      AlignItems::Stretch,
      &factory,
      &measure_cache,
      &measured_fragments,
      &measured_node_keys,
    );

    assert!(
      size.height > 0.0,
      "min-content height probe should return a non-zero intrinsic height"
    );
    assert_eq!(
      grid_measure_layout_calls(),
      0,
      "min-content height probe should not require full layout"
    );
  }

  #[test]
  fn measure_grid_item_short_circuits_max_content_height_probes() {
    use taffy::style::AvailableSpace;

    reset_grid_measure_layout_calls();

    let gc = GridFormattingContext::new();
    let factory =
      crate::layout::contexts::factory::FormattingContextFactory::with_font_context_viewport_and_cb(
        gc.font_context.clone(),
        gc.viewport_size,
        gc.nearest_positioned_cb,
      );
    let measure_cache: Rc<RefCell<HashMap<MeasureKey, taffy::geometry::Size<f32>>>> =
      Rc::new(RefCell::new(HashMap::new()));
    let measured_fragments: Rc<RefCell<HashMap<MeasureKey, FragmentNode>>> =
      Rc::new(RefCell::new(HashMap::new()));
    let measured_node_keys: Rc<RefCell<HashMap<TaffyNodeId, Vec<MeasureKey>>>> =
      Rc::new(RefCell::new(HashMap::new()));

    let mut node = make_text_item("hello world", 16.0);
    node.id = 1;
    let node_ptr = &node as *const _;

    let size = gc.measure_grid_item(
      node_ptr,
      TaffyNodeId::from(1u64),
      taffy::geometry::Size {
        width: Some(200.0),
        height: None,
      },
      taffy::geometry::Size {
        width: AvailableSpace::Definite(200.0),
        height: AvailableSpace::MaxContent,
      },
      Some(200.0),
      AlignItems::Stretch,
      &factory,
      &measure_cache,
      &measured_fragments,
      &measured_node_keys,
    );

    assert!(
      size.height > 0.0,
      "max-content height probe should return a non-zero intrinsic height"
    );
    assert_eq!(
      grid_measure_layout_calls(),
      0,
      "max-content height probe should not require full layout"
    );
  }

  #[test]
  fn grid_auto_rows_min_and_max_content_measure_text_height() {
    let gc = GridFormattingContext::new();
    let constraints = LayoutConstraints::definite_width(320.0);

    for (track, label) in [
      (GridTrack::MinContent, "min-content"),
      (GridTrack::MaxContent, "max-content"),
    ] {
      let mut grid_style = ComputedStyle::default();
      grid_style.display = CssDisplay::Grid;
      grid_style.grid_auto_rows = vec![track];
      let grid_style = Arc::new(grid_style);

      let item = make_text_item(
        "This grid item should contribute a non-zero intrinsic height",
        16.0,
      );

      let tree = BoxTree::new(BoxNode::new_block(
        grid_style,
        FormattingContextType::Grid,
        vec![item],
      ));

      let fragment = gc.layout(&tree.root, &constraints).expect("grid layout");
      assert_eq!(fragment.children.len(), 1);
      assert!(
        fragment.children[0].bounds.height() > 0.0,
        "{label} auto row should be sized from inline text height"
      );
    }
  }

  #[test]
  fn convert_style_sets_overflow_and_scrollbar_width() {
    let mut style = ComputedStyle::default();
    style.display = CssDisplay::Grid;
    style.overflow_x = Overflow::Scroll;
    style.overflow_y = Overflow::Clip;
    style.scrollbar_width = ScrollbarWidth::Thin;

    let node = BoxNode::new_block(Arc::new(style), FormattingContextType::Grid, vec![]);
    let gc = GridFormattingContext::new();
    let taffy_style = gc.convert_style(&node.style, None, false, true);

    assert_eq!(taffy_style.overflow.x, TaffyOverflow::Scroll);
    assert_eq!(taffy_style.overflow.y, TaffyOverflow::Clip);
    assert_eq!(
      taffy_style.scrollbar_width,
      resolve_scrollbar_width(&node.style)
    );
  }

  #[test]
  fn simple_grids_use_block_fast_path() {
    // Grid with default implicit tracks and a single child should use the simple fast path.
    let mut parent = BoxNode::new_block(
      Arc::new(ComputedStyle::default()),
      FormattingContextType::Grid,
      vec![],
    );
    let child1 = BoxNode::new_block(
      Arc::new(ComputedStyle::default()),
      FormattingContextType::Block,
      vec![],
    );
    let child2 = BoxNode::new_block(
      Arc::new(ComputedStyle::default()),
      FormattingContextType::Block,
      vec![],
    );
    parent.children.push(child1);
    parent.children.push(child2);

    let gc = GridFormattingContext::new();
    let constraints = LayoutConstraints::definite(800.0, 600.0);
    let _root_id = gc
      .build_taffy_tree(
        &mut TaffyTree::new(),
        &parent,
        &constraints,
        &mut HashMap::new(),
      )
      .expect("grid conversion");
    // If the fast path is taken, the parent container style should have been converted to block.
    let taffy_style = gc.convert_style(&parent.style, None, true, true);
    assert_eq!(taffy_style.display, Display::Block);
  }

  // Test 1: Basic grid container creation
  #[test]
  fn test_grid_fc_creation() {
    let fc = GridFormattingContext::new();
    let default_fc = GridFormattingContext::default();
    assert_eq!(
      std::mem::size_of_val(&fc),
      std::mem::size_of_val(&default_fc)
    );
  }

  // Test 2: Empty grid layout
  #[test]
  fn test_empty_grid_layout() {
    let fc = GridFormattingContext::new();
    let box_node = BoxNode::new_block(make_grid_style(), FormattingContextType::Grid, vec![]);
    let constraints = LayoutConstraints::definite(800.0, 600.0);

    let fragment = fc.layout(&box_node, &constraints).unwrap();
    assert!(fragment.bounds.width() >= 0.0);
    assert!(fragment.bounds.height() >= 0.0);
  }

  // Test 3: Grid with single child
  #[test]
  fn test_grid_single_child() {
    let fc = GridFormattingContext::new();

    let child = BoxNode::new_block(make_item_style(), FormattingContextType::Block, vec![]);
    let grid = BoxNode::new_block(make_grid_style(), FormattingContextType::Grid, vec![child]);

    let constraints = LayoutConstraints::definite(800.0, 600.0);
    let fragment = fc.layout(&grid, &constraints).unwrap();

    assert_eq!(fragment.children.len(), 1);
  }

  // Test 4: Grid with multiple children
  #[test]
  fn test_grid_multiple_children() {
    let fc = GridFormattingContext::new();

    let child1 = BoxNode::new_block(make_item_style(), FormattingContextType::Block, vec![]);
    let child2 = BoxNode::new_block(make_item_style(), FormattingContextType::Block, vec![]);
    let child3 = BoxNode::new_block(make_item_style(), FormattingContextType::Block, vec![]);

    let grid = BoxNode::new_block(
      make_grid_style(),
      FormattingContextType::Grid,
      vec![child1, child2, child3],
    );

    let constraints = LayoutConstraints::definite(800.0, 600.0);
    let fragment = fc.layout(&grid, &constraints).unwrap();

    assert_eq!(fragment.children.len(), 3);
  }

  #[test]
  fn measured_fragments_are_reused_without_cloning() {
    runtime::with_runtime_toggles(
      Arc::new(runtime::RuntimeToggles::from_map(HashMap::from([(
        "FASTR_PROFILE_FRAGMENT_CLONES".to_string(),
        "1".to_string(),
      )]))),
      || {
        fragment_clone_profile::reset_fragment_clone_profile();

        let fc = GridFormattingContext::new();

        let mut item_style = ComputedStyle::default();
        item_style.font_size = 12.0;
        item_style.grid_column_start = 2;
        let item_style = Arc::new(item_style);
        let text_child = BoxNode::new_text(item_style.clone(), "reuse-me".to_string());
        let mut item = BoxNode::new_block(
          item_style.clone(),
          FormattingContextType::Inline,
          vec![text_child],
        );
        item.id = 2;

        let mut grid_style = ComputedStyle::default();
        grid_style.display = CssDisplay::Grid;
        grid_style.grid_template_columns =
          vec![GridTrack::Length(Length::px(50.0)), GridTrack::Auto];
        let grid_style = Arc::new(grid_style);
        let mut grid = BoxNode::new_block(grid_style, FormattingContextType::Grid, vec![item]);
        grid.id = 1;

        let constraints = LayoutConstraints::definite(180.0, 100.0);

        let mut taffy: TaffyTree<*const BoxNode> = TaffyTree::new();
        let mut positioned_children_map: HashMap<TaffyNodeId, Vec<BoxNode>> = HashMap::new();
        let in_flow_children: Vec<&BoxNode> = grid.children.iter().collect();
        let root_id = fc
          .build_taffy_tree_children(
            &mut taffy,
            &grid,
            &in_flow_children,
            &constraints,
            &mut positioned_children_map,
          )
          .expect("build taffy tree");

        let measure_cache: Rc<RefCell<HashMap<MeasureKey, taffy::geometry::Size<f32>>>> =
          Rc::new(RefCell::new(HashMap::new()));
        let measured_fragments: Rc<RefCell<HashMap<MeasureKey, FragmentNode>>> =
          Rc::new(RefCell::new(HashMap::new()));
        let measured_node_keys: Rc<RefCell<HashMap<TaffyNodeId, Vec<MeasureKey>>>> =
          Rc::new(RefCell::new(HashMap::new()));

        let available_space = taffy::geometry::Size {
          width: taffy::style::AvailableSpace::Definite(constraints.width().unwrap()),
          height: taffy::style::AvailableSpace::Definite(constraints.height().unwrap()),
        };

        let factory = fc.factory.clone();
        let viewport_size = fc.viewport_size;
        let parent_inline_base = constraints.inline_percentage_base;
        let cache = measure_cache.clone();
        let measured = measured_fragments.clone();
        let measured_keys = measured_node_keys.clone();
        let this = fc.clone();
        taffy
          .compute_layout_with_measure(
            root_id,
            available_space,
            move |known_dimensions,
                  available_space,
                  node_id,
                  node_context,
                  _style: &taffy::style::Style| {
              if node_id == root_id {
                let fallback_size =
                  |known: Option<f32>, avail_dim: taffy::style::AvailableSpace| {
                    known.unwrap_or(match avail_dim {
                      taffy::style::AvailableSpace::Definite(v) => v,
                      _ => 0.0,
                    })
                  };
                return taffy::geometry::Size {
                  width: fallback_size(known_dimensions.width, available_space.width),
                  height: fallback_size(known_dimensions.height, available_space.height),
                };
              }

              let Some(node_ptr) = node_context.as_ref().map(|p| **p) else {
                return taffy::geometry::Size::ZERO;
              };
              let box_node = unsafe { &*node_ptr };

              let key = MeasureKey::new(node_ptr, known_dimensions, available_space);
              if let Some(size) = cache.borrow().get(&key) {
                return *size;
              }
              let fc_type = box_node
                .formatting_context()
                .unwrap_or(FormattingContextType::Block);
              let fc = factory.create(fc_type);

              let mut child_constraints = constraints_from_taffy(
                viewport_size,
                known_dimensions,
                available_space,
                parent_inline_base,
              );

              let mut fragment = match fc.layout(box_node, &child_constraints) {
                Ok(fragment) => fragment,
                Err(LayoutError::Timeout { .. }) => taffy::abort_layout_now(),
                Err(_) => return taffy::geometry::Size::ZERO,
              };
              let percentage_base = match available_space.width {
                taffy::style::AvailableSpace::Definite(w) => w,
                _ => child_constraints
                  .width()
                  .unwrap_or_else(|| fragment.bounds.width()),
              };
              fragment.content = FragmentContent::Block {
                box_id: Some(box_node.id),
              };
              fragment.style = Some(box_node.style.clone());
              let content_size = this.content_box_size(&fragment, &box_node.style, percentage_base);
              let size = taffy::geometry::Size {
                width: content_size.width.max(0.0),
                height: content_size.height.max(0.0),
              };
              measured_keys
                .borrow_mut()
                .entry(node_id)
                .or_default()
                .push(key);
              measured.borrow_mut().insert(key, fragment);
              cache.borrow_mut().insert(key, size);
              size
            },
          )
          .expect("taffy layout");

        let child_id = *taffy.children(root_id).unwrap().first().unwrap();
        let child_layout = taffy.layout(child_id).unwrap();
        let measured_node_keys = measured_node_keys.borrow();
        let before_len = measured_fragments.borrow().len();
        assert!(before_len > 0, "expected measured fragments to be recorded");
        let matched_key = measured_node_keys
          .get(&child_id)
          .and_then(|keys| {
            let measured = measured_fragments.borrow();
            keys.iter().copied().find(|key| {
              measured.get(key).map_or(false, |fragment| {
                (fragment.bounds.width() - child_layout.size.width).abs() < 0.1
                  && (fragment.bounds.height() - child_layout.size.height).abs() < 0.1
              })
            })
          })
          .expect("matching measured fragment should exist");

        let mut fragment = fc
          .convert_to_fragments(
            &taffy,
            root_id,
            root_id,
            &constraints,
            &measured_fragments,
            &measured_node_keys,
            &positioned_children_map,
          )
          .expect("convert fragments");

        let measured_after = measured_fragments.borrow();
        assert!(
          !measured_after.contains_key(&matched_key),
          "reused fragments should be removed from the cache"
        );
        assert!(
          measured_after.len() < before_len,
          "reusing a fragment should reduce the cache size"
        );
        assert_eq!(fragment.children.len(), 1);
        let child_fragment = fragment.children.pop().unwrap();
        match child_fragment.content {
          FragmentContent::Block { box_id } => assert_eq!(box_id, Some(2)),
          other => panic!("unexpected fragment content: {other:?}"),
        }
        assert!(child_fragment.style.is_some());
        assert!(
          (child_fragment.bounds.x() - child_layout.location.x).abs() < 0.01,
          "expected translated x to match layout"
        );
        assert!(
          (child_fragment.bounds.y() - child_layout.location.y).abs() < 0.01,
          "expected translated y to match layout"
        );

        fn has_text(node: &FragmentNode) -> bool {
          if matches!(node.content, FragmentContent::Text { .. }) {
            return true;
          }
          node.children.iter().any(has_text)
        }
        assert!(has_text(&child_fragment));

        let stats = fragment_clone_profile::fragment_clone_profile_stats();
        assert_eq!(
          stats.grid_measure_reuse.nodes, 0,
          "reuse should not record cloned nodes"
        );
        assert!(
          stats.grid_measure_reuse.events > 0,
          "reuse should be recorded when clone profiling is enabled"
        );
        fragment_clone_profile::reset_fragment_clone_profile();
      },
    );
  }

  // Test 5: Grid with explicit columns
  #[test]
  fn test_grid_explicit_columns() {
    let fc = GridFormattingContext::new();

    let style = make_grid_style_with_tracks(
      vec![GridTrack::Length(Length::px(100.0)), GridTrack::Fr(1.0)],
      vec![],
    );

    let child1 = BoxNode::new_block(make_item_style(), FormattingContextType::Block, vec![]);
    let child2 = BoxNode::new_block(make_item_style(), FormattingContextType::Block, vec![]);

    let grid = BoxNode::new_block(style, FormattingContextType::Grid, vec![child1, child2]);

    let constraints = LayoutConstraints::definite(400.0, 200.0);
    let fragment = fc.layout(&grid, &constraints).unwrap();

    assert_eq!(fragment.children.len(), 2);
  }

  // Test 6: Grid with explicit rows
  #[test]
  fn test_grid_explicit_rows() {
    let fc = GridFormattingContext::new();

    let style = make_grid_style_with_tracks(
      vec![],
      vec![
        GridTrack::Length(Length::px(50.0)),
        GridTrack::Length(Length::px(100.0)),
      ],
    );

    let child1 = BoxNode::new_block(make_item_style(), FormattingContextType::Block, vec![]);
    let child2 = BoxNode::new_block(make_item_style(), FormattingContextType::Block, vec![]);

    let grid = BoxNode::new_block(style, FormattingContextType::Grid, vec![child1, child2]);

    let constraints = LayoutConstraints::definite(400.0, 300.0);
    let fragment = fc.layout(&grid, &constraints).unwrap();

    assert_eq!(fragment.children.len(), 2);
  }

  #[test]
  fn absolute_child_inherits_positioned_cb_into_grid() {
    let mut grid_style = ComputedStyle::default();
    grid_style.display = CssDisplay::Grid;

    let mut abs_style = ComputedStyle::default();
    abs_style.display = CssDisplay::Block;
    abs_style.position = crate::style::position::Position::Absolute;
    abs_style.left = Some(Length::px(5.0));
    abs_style.top = Some(Length::px(7.0));
    abs_style.width = Some(Length::px(12.0));
    abs_style.height = Some(Length::px(9.0));

    let abs_child = BoxNode::new_block(Arc::new(abs_style), FormattingContextType::Block, vec![]);
    let grid = BoxNode::new_block(
      Arc::new(grid_style),
      FormattingContextType::Grid,
      vec![abs_child],
    );

    let viewport = crate::geometry::Size::new(300.0, 300.0);
    let cb_rect = crate::geometry::Rect::from_xywh(20.0, 30.0, 150.0, 150.0);
    let cb = crate::layout::contexts::positioned::ContainingBlock::with_viewport(cb_rect, viewport);
    let fc = GridFormattingContext::with_viewport_and_cb(
      viewport,
      cb,
      crate::text::font_loader::FontContext::new(),
    );
    let constraints = LayoutConstraints::definite(100.0, 100.0);
    let fragment = fc.layout(&grid, &constraints).unwrap();

    assert_eq!(fragment.children.len(), 1);
    let abs_fragment = &fragment.children[0];
    assert_eq!(abs_fragment.bounds.x(), 25.0);
    assert_eq!(abs_fragment.bounds.y(), 37.0);
    assert_eq!(abs_fragment.bounds.width(), 12.0);
    assert_eq!(abs_fragment.bounds.height(), 9.0);
  }

  // Test 7: Grid with gap
  #[test]
  fn test_grid_with_gap() {
    let fc = GridFormattingContext::new();

    let mut style = ComputedStyle::default();
    style.display = CssDisplay::Grid;
    style.grid_column_gap = Length::px(10.0);
    style.grid_row_gap = Length::px(20.0);
    style.grid_template_columns = vec![GridTrack::Fr(1.0), GridTrack::Fr(1.0)];
    let style = Arc::new(style);

    let child1 = BoxNode::new_block(make_item_style(), FormattingContextType::Block, vec![]);
    let child2 = BoxNode::new_block(make_item_style(), FormattingContextType::Block, vec![]);

    let grid = BoxNode::new_block(style, FormattingContextType::Grid, vec![child1, child2]);

    let constraints = LayoutConstraints::definite(410.0, 200.0);
    let fragment = fc.layout(&grid, &constraints).unwrap();

    assert_eq!(fragment.children.len(), 2);
  }

  // Test 8: Grid with multiple rows
  #[test]
  fn test_grid_multiple_rows() {
    let fc = GridFormattingContext::new();

    let mut style = ComputedStyle::default();
    style.display = CssDisplay::Grid;
    style.grid_template_columns = vec![GridTrack::Fr(1.0), GridTrack::Fr(1.0)];
    let style = Arc::new(style);

    let children: Vec<BoxNode> = (0..4)
      .map(|_| BoxNode::new_block(make_item_style(), FormattingContextType::Block, vec![]))
      .collect();

    let grid = BoxNode::new_block(style, FormattingContextType::Grid, children);

    let constraints = LayoutConstraints::definite(400.0, 200.0);
    let fragment = fc.layout(&grid, &constraints).unwrap();

    assert_eq!(fragment.children.len(), 4);
  }

  // Test 9: Grid item placement with line numbers
  #[test]
  fn test_grid_item_placement() {
    let fc = GridFormattingContext::new();

    let grid_style = make_grid_style_with_tracks(
      vec![GridTrack::Fr(1.0), GridTrack::Fr(1.0)],
      vec![GridTrack::Fr(1.0), GridTrack::Fr(1.0)],
    );

    let mut item_style = ComputedStyle::default();
    item_style.grid_column_start = 2;
    item_style.grid_row_start = 1;

    let child = BoxNode::new_block(Arc::new(item_style), FormattingContextType::Block, vec![]);
    let grid = BoxNode::new_block(grid_style, FormattingContextType::Grid, vec![child]);

    let constraints = LayoutConstraints::definite(400.0, 200.0);
    let fragment = fc.layout(&grid, &constraints).unwrap();

    assert_eq!(fragment.children.len(), 1);
  }

  // Test 10: Intrinsic sizing - min content
  #[test]
  fn test_intrinsic_min_content() {
    let fc = GridFormattingContext::new();

    let mut item_style = ComputedStyle::default();
    item_style.width = Some(Length::px(50.0));
    item_style.height = Some(Length::px(30.0));

    let child = BoxNode::new_block(Arc::new(item_style), FormattingContextType::Block, vec![]);
    let grid = BoxNode::new_block(make_grid_style(), FormattingContextType::Grid, vec![child]);

    let size = fc
      .compute_intrinsic_inline_size(&grid, IntrinsicSizingMode::MinContent)
      .unwrap();

    assert!(size >= 0.0);
  }

  // Test 11: Intrinsic sizing - max content
  #[test]
  fn test_intrinsic_max_content() {
    let fc = GridFormattingContext::new();

    let mut item_style = ComputedStyle::default();
    item_style.width = Some(Length::px(100.0));
    item_style.height = Some(Length::px(50.0));

    let child = BoxNode::new_block(Arc::new(item_style), FormattingContextType::Block, vec![]);
    let grid = BoxNode::new_block(make_grid_style(), FormattingContextType::Grid, vec![child]);

    let size = fc
      .compute_intrinsic_inline_size(&grid, IntrinsicSizingMode::MaxContent)
      .unwrap();

    assert!(size >= 0.0);
  }

  // Test 12: Grid with minmax track
  #[test]
  fn test_grid_minmax_track() {
    let fc = GridFormattingContext::new();

    let mut style = ComputedStyle::default();
    style.display = CssDisplay::Grid;
    style.grid_template_columns = vec![GridTrack::MinMax(
      Box::new(GridTrack::Length(Length::px(100.0))),
      Box::new(GridTrack::Fr(1.0)),
    )];
    let style = Arc::new(style);

    let child = BoxNode::new_block(make_item_style(), FormattingContextType::Block, vec![]);
    let grid = BoxNode::new_block(style, FormattingContextType::Grid, vec![child]);

    let constraints = LayoutConstraints::definite(500.0, 200.0);
    let fragment = fc.layout(&grid, &constraints).unwrap();

    assert_eq!(fragment.children.len(), 1);
  }

  // Test 13: Grid indefinite constraints
  #[test]
  fn test_grid_indefinite_constraints() {
    let fc = GridFormattingContext::new();

    let child = BoxNode::new_block(make_item_style(), FormattingContextType::Block, vec![]);
    let grid = BoxNode::new_block(make_grid_style(), FormattingContextType::Grid, vec![child]);

    let constraints = LayoutConstraints::indefinite();
    let fragment = fc.layout(&grid, &constraints).unwrap();

    assert!(fragment.bounds.width() >= 0.0);
    assert!(fragment.bounds.height() >= 0.0);
  }

  // Test 14: Grid with align-content
  #[test]
  fn test_grid_align_content() {
    let fc = GridFormattingContext::new();

    let mut style = ComputedStyle::default();
    style.display = CssDisplay::Grid;
    style.align_content = AlignContent::Center;
    style.grid_template_columns = vec![GridTrack::Fr(1.0)];
    let style = Arc::new(style);

    let child = BoxNode::new_block(make_item_style(), FormattingContextType::Block, vec![]);
    let grid = BoxNode::new_block(style, FormattingContextType::Grid, vec![child]);

    let constraints = LayoutConstraints::definite(400.0, 400.0);
    let fragment = fc.layout(&grid, &constraints).unwrap();

    assert_eq!(fragment.children.len(), 1);
  }

  #[test]
  fn grid_justify_items_centers_children() {
    let fc = GridFormattingContext::new();

    let mut style = ComputedStyle::default();
    style.display = CssDisplay::Grid;
    style.grid_template_columns = vec![GridTrack::Length(Length::px(200.0))];
    style.grid_template_rows = vec![GridTrack::Length(Length::px(100.0))];
    style.justify_items = AlignItems::Center;
    style.align_items = AlignItems::FlexStart;
    let style = Arc::new(style);

    let mut item_style = ComputedStyle::default();
    item_style.width = Some(Length::px(50.0));
    item_style.height = Some(Length::px(20.0));
    let item = BoxNode::new_block(Arc::new(item_style), FormattingContextType::Block, vec![]);

    let grid = BoxNode::new_block(style, FormattingContextType::Grid, vec![item]);
    let constraints = LayoutConstraints::definite(400.0, 200.0);
    let fragment = fc.layout(&grid, &constraints).unwrap();

    assert_eq!(fragment.children[0].bounds.x(), 75.0);
    assert_eq!(fragment.children[0].bounds.y(), 0.0);
  }

  #[test]
  fn grid_align_self_and_justify_self_override_container_alignment() {
    let fc = GridFormattingContext::new();

    let mut style = ComputedStyle::default();
    style.display = CssDisplay::Grid;
    style.grid_template_columns = vec![GridTrack::Length(Length::px(200.0))];
    style.grid_template_rows = vec![GridTrack::Length(Length::px(100.0))];
    style.justify_items = AlignItems::FlexStart;
    style.align_items = AlignItems::FlexStart;
    let style = Arc::new(style);

    let mut item_style = ComputedStyle::default();
    item_style.width = Some(Length::px(50.0));
    item_style.height = Some(Length::px(30.0));
    item_style.justify_self = Some(AlignItems::FlexEnd);
    item_style.align_self = Some(AlignItems::FlexEnd);
    let item = BoxNode::new_block(Arc::new(item_style), FormattingContextType::Block, vec![]);

    let grid = BoxNode::new_block(style, FormattingContextType::Grid, vec![item]);
    let constraints = LayoutConstraints::definite(400.0, 200.0);
    let fragment = fc.layout(&grid, &constraints).unwrap();

    assert_eq!(fragment.children[0].bounds.x(), 150.0);
    assert_eq!(fragment.children[0].bounds.y(), 70.0);
  }

  #[test]
  fn grid_item_aspect_ratio_sets_height_from_width() {
    let fc = GridFormattingContext::new();

    let mut grid_style = ComputedStyle::default();
    grid_style.display = CssDisplay::Grid;
    grid_style.align_items = AlignItems::FlexStart;
    grid_style.grid_template_columns = vec![GridTrack::Length(Length::px(200.0))];
    let grid_style = Arc::new(grid_style);

    let mut item_style = ComputedStyle::default();
    item_style.width = Some(Length::px(80.0));
    item_style.aspect_ratio = AspectRatio::Ratio(2.0);
    let item = BoxNode::new_block(Arc::new(item_style), FormattingContextType::Block, vec![]);

    let grid = BoxNode::new_block(grid_style, FormattingContextType::Grid, vec![item]);
    let constraints = LayoutConstraints::definite(400.0, 200.0);
    let fragment = fc.layout(&grid, &constraints).unwrap();

    assert_eq!(fragment.children[0].bounds.width(), 80.0);
    assert_eq!(fragment.children[0].bounds.height(), 40.0);
  }

  #[test]
  fn grid_item_aspect_ratio_sets_width_from_height() {
    let fc = GridFormattingContext::new();

    let mut grid_style = ComputedStyle::default();
    grid_style.display = CssDisplay::Grid;
    grid_style.align_items = AlignItems::FlexStart;
    grid_style.grid_template_rows = vec![GridTrack::Length(Length::px(60.0))];
    let grid_style = Arc::new(grid_style);

    let mut item_style = ComputedStyle::default();
    item_style.height = Some(Length::px(60.0));
    item_style.aspect_ratio = AspectRatio::Ratio(1.5);
    let item = BoxNode::new_block(Arc::new(item_style), FormattingContextType::Block, vec![]);

    let grid = BoxNode::new_block(grid_style, FormattingContextType::Grid, vec![item]);
    let constraints = LayoutConstraints::definite(400.0, 200.0);
    let fragment = fc.layout(&grid, &constraints).unwrap();

    assert_eq!(fragment.children[0].bounds.height(), 60.0);
    assert_eq!(fragment.children[0].bounds.width(), 90.0);
  }

  // Test 15: Grid with nested grid
  #[test]
  fn test_nested_grid() {
    let fc = GridFormattingContext::new();

    let inner_child = BoxNode::new_block(make_item_style(), FormattingContextType::Block, vec![]);
    let inner_grid = BoxNode::new_block(
      make_grid_style(),
      FormattingContextType::Grid,
      vec![inner_child],
    );
    let outer_grid = BoxNode::new_block(
      make_grid_style(),
      FormattingContextType::Grid,
      vec![inner_grid],
    );

    let constraints = LayoutConstraints::definite(800.0, 600.0);
    let fragment = fc.layout(&outer_grid, &constraints).unwrap();

    assert_eq!(fragment.children.len(), 1);
    assert_eq!(fragment.children[0].children.len(), 1);
  }

  // Test 16: FormattingContext trait is Send + Sync
  #[test]
  fn test_send_sync() {
    fn assert_send_sync<T: Send + Sync>() {}
    assert_send_sync::<GridFormattingContext>();
  }

  // Test 17: Grid with percentage widths
  #[test]
  fn test_grid_percentage_track() {
    let fc = GridFormattingContext::new();

    let mut style = ComputedStyle::default();
    style.display = CssDisplay::Grid;
    style.grid_template_columns = vec![
      GridTrack::Length(Length::percent(50.0)),
      GridTrack::Length(Length::percent(50.0)),
    ];
    let style = Arc::new(style);

    let child1 = BoxNode::new_block(make_item_style(), FormattingContextType::Block, vec![]);
    let child2 = BoxNode::new_block(make_item_style(), FormattingContextType::Block, vec![]);

    let grid = BoxNode::new_block(style, FormattingContextType::Grid, vec![child1, child2]);

    let constraints = LayoutConstraints::definite(400.0, 200.0);
    let fragment = fc.layout(&grid, &constraints).unwrap();

    assert_eq!(fragment.children.len(), 2);
  }

  // Test 18: Grid auto track
  #[test]
  fn test_grid_auto_track() {
    let fc = GridFormattingContext::new();

    let mut style = ComputedStyle::default();
    style.display = CssDisplay::Grid;
    style.grid_template_columns = vec![GridTrack::Auto];
    let style = Arc::new(style);

    let child = BoxNode::new_block(make_item_style(), FormattingContextType::Block, vec![]);
    let grid = BoxNode::new_block(style, FormattingContextType::Grid, vec![child]);

    let constraints = LayoutConstraints::definite(400.0, 200.0);
    let fragment = fc.layout(&grid, &constraints).unwrap();

    assert_eq!(fragment.children.len(), 1);
  }

  #[test]
  fn vertical_writing_mode_swaps_tracks_for_template_sizes() {
    let fc = GridFormattingContext::new();

    let mut style = ComputedStyle::default();
    style.display = CssDisplay::Grid;
    style.writing_mode = WritingMode::VerticalRl;
    style.grid_template_columns = vec![
      GridTrack::Length(Length::px(20.0)),
      GridTrack::Length(Length::px(20.0)),
    ];
    style.grid_template_rows = vec![GridTrack::Length(Length::px(30.0))];
    let style = Arc::new(style);

    let child = BoxNode::new_block(make_item_style(), FormattingContextType::Block, vec![]);
    let grid = BoxNode::new_block(style, FormattingContextType::Grid, vec![child]);

    let constraints = LayoutConstraints::definite(200.0, 200.0);
    let fragment = fc.layout(&grid, &constraints).unwrap();

    // Inline axis vertical: column tracks become rows (height = 20+20), row tracks become columns (width = 30).
    assert_eq!(fragment.bounds.width(), 30.0);
    assert_eq!(fragment.bounds.height(), 40.0);
    assert_eq!(fragment.children[0].bounds.width(), 30.0);
    assert_eq!(fragment.children[0].bounds.height(), 20.0);
  }

  #[test]
  fn vertical_writing_mode_swaps_placements_to_physical_axes() {
    let fc = GridFormattingContext::new();

    let mut style = ComputedStyle::default();
    style.display = CssDisplay::Grid;
    style.writing_mode = WritingMode::VerticalRl;
    style.grid_template_columns = vec![
      GridTrack::Length(Length::px(30.0)),
      GridTrack::Length(Length::px(40.0)),
    ];
    style.grid_template_rows = vec![
      GridTrack::Length(Length::px(100.0)),
      GridTrack::Length(Length::px(200.0)),
    ];
    let style = Arc::new(style);

    let mut inline_item_style = ComputedStyle::default();
    inline_item_style.grid_column_start = 2;
    inline_item_style.grid_column_end = 3;
    inline_item_style.grid_row_start = 1;
    inline_item_style.grid_row_end = 2;
    let inline_item = BoxNode::new_block(
      Arc::new(inline_item_style),
      FormattingContextType::Block,
      vec![],
    );

    let mut block_item_style = ComputedStyle::default();
    block_item_style.grid_row_start = 2;
    block_item_style.grid_row_end = 3;
    block_item_style.grid_column_start = 1;
    block_item_style.grid_column_end = 2;
    let block_item = BoxNode::new_block(
      Arc::new(block_item_style),
      FormattingContextType::Block,
      vec![],
    );

    let grid = BoxNode::new_block(
      style,
      FormattingContextType::Grid,
      vec![inline_item, block_item],
    );

    let constraints = LayoutConstraints::definite(500.0, 500.0);
    let fragment = fc.layout(&grid, &constraints).unwrap();

    // Tracks transpose: block tracks become Taffy columns (width), inline tracks become Taffy rows (height).
    assert_eq!(fragment.bounds.width(), 300.0);
    assert_eq!(fragment.bounds.height(), 70.0);

    // grid-column maps to the inline axis (vertical), so it should affect y/height.
    assert_eq!(fragment.children[0].bounds.x(), 0.0);
    assert_eq!(fragment.children[0].bounds.y(), 30.0);
    assert_eq!(fragment.children[0].bounds.width(), 100.0);
    assert_eq!(fragment.children[0].bounds.height(), 40.0);

    // grid-row maps to the block axis (horizontal), so it should affect x/width.
    assert_eq!(fragment.children[1].bounds.x(), 100.0);
    assert_eq!(fragment.children[1].bounds.y(), 0.0);
    assert_eq!(fragment.children[1].bounds.width(), 200.0);
    assert_eq!(fragment.children[1].bounds.height(), 30.0);
  }

  #[test]
  fn vertical_writing_mode_row_autoflow_fills_inline_axis_first() {
    let fc = GridFormattingContext::new();

    let mut style = ComputedStyle::default();
    style.display = CssDisplay::Grid;
    style.writing_mode = WritingMode::VerticalRl;
    style.grid_auto_flow = GridAutoFlow::Row;
    style.grid_template_columns = vec![
      GridTrack::Length(Length::px(20.0)),
      GridTrack::Length(Length::px(20.0)),
    ];
    style.grid_template_rows = vec![
      GridTrack::Length(Length::px(40.0)),
      GridTrack::Length(Length::px(40.0)),
    ];
    let style = Arc::new(style);

    let children: Vec<_> = (0..3)
      .map(|_| BoxNode::new_block(make_item_style(), FormattingContextType::Block, vec![]))
      .collect();
    let grid = BoxNode::new_block(style, FormattingContextType::Grid, children);

    let constraints = LayoutConstraints::definite(200.0, 200.0);
    let fragment = fc.layout(&grid, &constraints).unwrap();

    assert_eq!(fragment.bounds.width(), 80.0);
    assert_eq!(fragment.bounds.height(), 40.0);

    // Row auto-flow maps to column auto-flow when inline is vertical: fill inline tracks top→bottom, then start a new block track.
    assert_eq!(fragment.children[0].bounds.x(), 0.0);
    assert_eq!(fragment.children[0].bounds.y(), 0.0);
    assert_eq!(fragment.children[1].bounds.x(), 0.0);
    assert_eq!(fragment.children[1].bounds.y(), 20.0);
    assert_eq!(fragment.children[2].bounds.x(), 40.0);
    assert_eq!(fragment.children[2].bounds.y(), 0.0);
  }

  #[test]
  fn vertical_writing_mode_column_autoflow_fills_block_axis_first() {
    let fc = GridFormattingContext::new();

    let mut style = ComputedStyle::default();
    style.display = CssDisplay::Grid;
    style.writing_mode = WritingMode::VerticalRl;
    style.grid_auto_flow = GridAutoFlow::Column;
    style.grid_template_columns = vec![
      GridTrack::Length(Length::px(20.0)),
      GridTrack::Length(Length::px(20.0)),
    ];
    style.grid_template_rows = vec![
      GridTrack::Length(Length::px(40.0)),
      GridTrack::Length(Length::px(40.0)),
    ];
    let style = Arc::new(style);

    let children: Vec<_> = (0..3)
      .map(|_| BoxNode::new_block(make_item_style(), FormattingContextType::Block, vec![]))
      .collect();
    let grid = BoxNode::new_block(style, FormattingContextType::Grid, children);

    let constraints = LayoutConstraints::definite(200.0, 200.0);
    let fragment = fc.layout(&grid, &constraints).unwrap();

    assert_eq!(fragment.bounds.width(), 80.0);
    assert_eq!(fragment.bounds.height(), 40.0);

    // Column auto-flow maps to row auto-flow when inline is vertical: fill block tracks first, then wrap inline.
    assert_eq!(fragment.children[0].bounds.x(), 0.0);
    assert_eq!(fragment.children[0].bounds.y(), 0.0);
    assert_eq!(fragment.children[1].bounds.x(), 40.0);
    assert_eq!(fragment.children[1].bounds.y(), 0.0);
    assert_eq!(fragment.children[2].bounds.x(), 0.0);
    assert_eq!(fragment.children[2].bounds.y(), 20.0);
  }

  #[test]
  fn test_grid_fixed_and_fr() {
    let fc = GridFormattingContext::new();

    let mut style = ComputedStyle::default();
    style.display = CssDisplay::Grid;
    style.grid_template_columns = vec![
      GridTrack::Length(Length::px(100.0)),
      GridTrack::Fr(1.0),
      GridTrack::Fr(2.0),
    ];
    let style = Arc::new(style);

    let child1 = BoxNode::new_block(make_item_style(), FormattingContextType::Block, vec![]);
    let child2 = BoxNode::new_block(make_item_style(), FormattingContextType::Block, vec![]);
    let child3 = BoxNode::new_block(make_item_style(), FormattingContextType::Block, vec![]);

    let grid = BoxNode::new_block(
      style,
      FormattingContextType::Grid,
      vec![child1, child2, child3],
    );

    let constraints = LayoutConstraints::definite(400.0, 200.0);
    let fragment = fc.layout(&grid, &constraints).unwrap();

    assert_eq!(fragment.children.len(), 3);
  }

  #[test]
  fn grid_reuses_normalized_measured_fragments() {
    let child = BoxNode::new_block(make_item_style(), FormattingContextType::Block, vec![]);
    let child_id = child.id;
    let grid = BoxNode::new_block(
      make_grid_style_with_tracks(vec![GridTrack::Auto], vec![GridTrack::Auto]),
      FormattingContextType::Grid,
      vec![child],
    );

    let constraints = LayoutConstraints::definite(100.0, 100.0);
    let fc = GridFormattingContext::new();
    let _guard = set_grid_test_measure_hook(move |node| {
      (node.id == child_id)
        .then(|| FragmentNode::new_block(Rect::from_xywh(5.0, 7.0, 30.0, 10.0), vec![]))
    });

    let fragment = fc.layout(&grid, &constraints).unwrap();
    assert_eq!(fragment.children.len(), 1);
    let child_fragment = &fragment.children[0];
    assert_eq!(child_fragment.bounds.x(), 0.0);
    assert_eq!(child_fragment.bounds.y(), 0.0);
    assert_eq!(child_fragment.bounds.width(), 30.0);
    assert_eq!(child_fragment.bounds.height(), 10.0);
  }

  #[test]
  fn parses_named_line_with_integer_in_any_order() {
    let placement = parse_grid_line_placement_raw("foo 2");
    match placement.start {
      TaffyGridPlacement::NamedLine(name, idx) => {
        assert_eq!(name, "foo");
        assert_eq!(idx, 2);
      }
      other => panic!("expected named line, got {:?}", other),
    }

    let placement_rev = parse_grid_line_placement_raw("2 foo");
    match placement_rev.start {
      TaffyGridPlacement::NamedLine(name, idx) => {
        assert_eq!(name, "foo");
        assert_eq!(idx, 2);
      }
      other => panic!("expected named line, got {:?}", other),
    }
  }

  #[test]
  fn parses_named_span_in_any_order() {
    let placement = parse_grid_line_placement_raw("span foo 3");
    match placement.start {
      TaffyGridPlacement::NamedSpan(name, count) => {
        assert_eq!(name, "foo");
        assert_eq!(count, 3);
      }
      other => panic!("expected named span, got {:?}", other),
    }

    let placement_rev = parse_grid_line_placement_raw("span 3 foo");
    match placement_rev.start {
      TaffyGridPlacement::NamedSpan(name, count) => {
        assert_eq!(name, "foo");
        assert_eq!(count, 3);
      }
      other => panic!("expected named span, got {:?}", other),
    }
  }

  // Test 20: Grid with both row and column gaps
  #[test]
  fn test_grid_both_gaps() {
    let fc = GridFormattingContext::new();

    let mut style = ComputedStyle::default();
    style.display = CssDisplay::Grid;
    style.grid_template_columns = vec![GridTrack::Fr(1.0), GridTrack::Fr(1.0)];
    style.grid_template_rows = vec![GridTrack::Fr(1.0), GridTrack::Fr(1.0)];
    style.grid_column_gap = Length::px(10.0);
    style.grid_row_gap = Length::px(10.0);
    let style = Arc::new(style);

    let children: Vec<BoxNode> = (0..4)
      .map(|_| BoxNode::new_block(make_item_style(), FormattingContextType::Block, vec![]))
      .collect();

    let grid = BoxNode::new_block(style, FormattingContextType::Grid, children);

    let constraints = LayoutConstraints::definite(410.0, 210.0);
    let fragment = fc.layout(&grid, &constraints).unwrap();

    assert_eq!(fragment.children.len(), 4);
  }

  #[test]
  fn grid_align_items_baseline_aligns_children() {
    let fc = GridFormattingContext::new();

    let mut grid_style = ComputedStyle::default();
    grid_style.display = CssDisplay::Grid;
    grid_style.align_items = AlignItems::Baseline;
    grid_style.grid_template_columns = vec![
      GridTrack::Length(Length::px(140.0)),
      GridTrack::Length(Length::px(140.0)),
    ];
    let grid_style = Arc::new(grid_style);

    let item_small = make_text_item("small", 14.0);
    let item_large = make_text_item("large", 28.0);

    let grid = BoxNode::new_block(
      grid_style,
      FormattingContextType::Grid,
      vec![item_small, item_large],
    );

    let constraints = LayoutConstraints::definite(400.0, 200.0);
    let fragment = fc.layout(&grid, &constraints).unwrap();

    let baseline0 = super::find_first_baseline_absolute(&fragment.children[0]).unwrap();
    let baseline1 = super::find_first_baseline_absolute(&fragment.children[1]).unwrap();

    assert!(
      (baseline0 - baseline1).abs() < 0.05,
      "baselines should align: {:.2} vs {:.2}",
      baseline0,
      baseline1
    );
    assert!(
      fragment.children[0].bounds.y() > fragment.children[1].bounds.y(),
      "smaller baseline item should be offset to align"
    );
  }

  #[test]
  fn grid_justify_items_baseline_aligns_columns() {
    let fc = GridFormattingContext::new();

    let mut grid_style = ComputedStyle::default();
    grid_style.display = CssDisplay::Grid;
    grid_style.justify_items = AlignItems::Baseline;
    grid_style.grid_template_columns = vec![GridTrack::Length(Length::px(180.0))];
    grid_style.grid_template_rows = vec![GridTrack::Auto, GridTrack::Auto];
    let grid_style = Arc::new(grid_style);

    let item_small = make_text_item("small", 12.0);
    let item_large = make_text_item("large", 24.0);

    let grid = BoxNode::new_block(
      grid_style,
      FormattingContextType::Grid,
      vec![item_small, item_large],
    );

    let constraints = LayoutConstraints::definite(300.0, 300.0);
    let fragment = fc.layout(&grid, &constraints).unwrap();

    let baseline_offset0 = super::first_baseline_offset(&fragment.children[0])
      .unwrap_or_else(|| fragment.children[0].bounds.width());
    let baseline_offset1 = super::first_baseline_offset(&fragment.children[1])
      .unwrap_or_else(|| fragment.children[1].bounds.width());

    let baseline0 = fragment.children[0].bounds.x() + baseline_offset0;
    let baseline1 = fragment.children[1].bounds.x() + baseline_offset1;

    assert!(
      (baseline0 - baseline1).abs() < 0.05,
      "inline-axis baselines should align: {:.2} vs {:.2}",
      baseline0,
      baseline1
    );
    assert!(
      fragment.children[0].bounds.x() >= 0.0 || fragment.children[1].bounds.x() >= 0.0,
      "baseline alignment should not move items outside the column"
    );
  }

  #[test]
  fn taffy_perf_counters_record_grid_measure_and_compute_time() {
    let config = FastRenderConfig::default().with_font_sources(FontConfig::bundled_only());
    let mut renderer = FastRender::with_config(config).expect("renderer");
    let html = r#"<!doctype html>
      <html>
        <body>
          <div style="display:grid">
            <div>hello</div>
          </div>
        </body>
      </html>"#;
    let options = RenderOptions::default()
      .with_viewport(200, 200)
      .with_diagnostics_level(DiagnosticsLevel::Basic);
    let result = renderer
      .render_html_with_diagnostics(html, options)
      .expect("render");
    let stats = result
      .diagnostics
      .stats
      .as_ref()
      .expect("diagnostics stats should be captured");
    let measure_calls = stats
      .layout
      .taffy_grid_measure_calls
      .expect("grid measure call count should be recorded");
    assert!(measure_calls > 0, "expected grid measure calls > 0");
    let compute_ms = stats
      .layout
      .taffy_grid_compute_ms
      .expect("grid compute_ms should be recorded");
    assert!(compute_ms >= 0.0, "expected grid compute_ms >= 0");
  }
}
