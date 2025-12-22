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

use crate::geometry::Point;
use crate::geometry::Rect;
use crate::layout::constraints::AvailableSpace as CrateAvailableSpace;
use crate::layout::constraints::LayoutConstraints;
use crate::layout::formatting_context::layout_cache_lookup;
use crate::layout::formatting_context::layout_cache_store;
use crate::layout::formatting_context::FormattingContext;
use crate::layout::formatting_context::IntrinsicSizingMode;
use crate::layout::formatting_context::LayoutError;
use crate::layout::profile::layout_timer;
use crate::layout::profile::LayoutKind;
use crate::layout::utils::resolve_length_with_percentage_metrics;
use crate::layout::utils::resolve_scrollbar_width;
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
use std::collections::HashMap;
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
use taffy::tree::NodeId as TaffyNodeId;
use taffy::tree::TaffyTree;

#[derive(Clone, Copy)]
enum Axis {
  Horizontal,
  Vertical,
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
pub struct GridFormattingContext {
  viewport_size: crate::geometry::Size,
  font_context: crate::text::font_loader::FontContext,
  nearest_positioned_cb: crate::layout::contexts::positioned::ContainingBlock,
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
    Self {
      viewport_size,
      font_context,
      nearest_positioned_cb,
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

  /// Builds a Taffy tree from a BoxNode tree
  ///
  /// Recursively converts the BoxNode tree to Taffy nodes, returning
  /// the root node ID and a mapping from Taffy nodes to BoxNodes.
  fn build_taffy_tree<'a>(
    &self,
    taffy: &mut TaffyTree<()>,
    box_node: &'a BoxNode,
  ) -> Result<(TaffyNodeId, HashMap<TaffyNodeId, &'a BoxNode>), LayoutError> {
    self.build_taffy_tree_children(
      taffy,
      box_node,
      &box_node.children.iter().collect::<Vec<_>>(),
    )
  }

  /// Builds a Taffy tree using an explicit slice of root children (used to exclude out-of-flow boxes).
  fn build_taffy_tree_children<'a>(
    &self,
    taffy: &mut TaffyTree<()>,
    box_node: &'a BoxNode,
    root_children: &[&'a BoxNode],
  ) -> Result<(TaffyNodeId, HashMap<TaffyNodeId, &'a BoxNode>), LayoutError> {
    let mut node_map = HashMap::new();
    let root_id =
      self.build_node_recursive(taffy, box_node, &mut node_map, None, Some(root_children))?;
    Ok((root_id, node_map))
  }

  /// Recursively builds a Taffy node and its children
  fn build_node_recursive<'a>(
    &self,
    taffy: &mut TaffyTree<()>,
    box_node: &'a BoxNode,
    node_map: &mut HashMap<TaffyNodeId, &'a BoxNode>,
    containing_grid: Option<&'a ComputedStyle>,
    root_children: Option<&[&'a BoxNode]>,
  ) -> Result<TaffyNodeId, LayoutError> {
    // Convert children first
    let children_iter: Vec<&BoxNode> = root_children
      .map(|c| c.to_vec())
      .unwrap_or_else(|| box_node.children.iter().collect());
    let child_ids: Vec<TaffyNodeId> = children_iter
      .iter()
      .map(|child| {
        let next_containing_grid = match box_node.style.display {
          CssDisplay::Grid | CssDisplay::InlineGrid => Some(box_node.style.as_ref()),
          _ => None,
        };
        self.build_node_recursive(taffy, child, node_map, next_containing_grid, None)
      })
      .collect::<Result<_, _>>()?;

    let simple_grid = self.is_simple_grid(&box_node.style, &children_iter);

    // Convert style
    let taffy_style = self.convert_style(&box_node.style, containing_grid, simple_grid);

    // Create Taffy node
    let node_id = if child_ids.is_empty() {
      taffy
        .new_leaf(taffy_style)
        .map_err(|e| LayoutError::MissingContext(format!("Taffy error: {:?}", e)))?
    } else {
      taffy
        .new_with_children(taffy_style, &child_ids)
        .map_err(|e| LayoutError::MissingContext(format!("Taffy error: {:?}", e)))?
    };

    node_map.insert(node_id, box_node);
    Ok(node_id)
  }

  /// Converts ComputedStyle to Taffy Style
  fn convert_style(
    &self,
    style: &ComputedStyle,
    containing_grid: Option<&ComputedStyle>,
    simple_grid: bool,
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
    let is_grid =
      matches!(style.display, CssDisplay::Grid | CssDisplay::InlineGrid) && !simple_grid;
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

  /// Converts Taffy layout results to FragmentNode tree
  fn convert_to_fragments(
    &self,
    taffy: &TaffyTree<()>,
    node_id: TaffyNodeId,
    node_map: &HashMap<TaffyNodeId, &BoxNode>,
    constraints: &LayoutConstraints,
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
      .map(|&child_id| self.convert_to_fragments(taffy, child_id, node_map, constraints))
      .collect::<Result<_, _>>()?;

    // Create fragment bounds from Taffy layout
    let bounds = Rect::from_xywh(
      layout.location.x,
      layout.location.y,
      layout.size.width,
      layout.size.height,
    );

    // Get style from node_map if available
    if let Some(box_node) = node_map.get(&node_id) {
      let is_grid = matches!(
        box_node.style.display,
        CssDisplay::Grid | CssDisplay::InlineGrid
      ) || matches!(
        box_node.formatting_context(),
        Some(FormattingContextType::Grid)
      );
      if is_grid {
        return Ok(FragmentNode::new_with_style(
          bounds,
          FragmentContent::Block {
            box_id: Some(box_node.id),
          },
          child_fragments,
          box_node.style.clone(),
        ));
      }

      let fc_type = box_node
        .formatting_context()
        .unwrap_or(FormattingContextType::Block);
      let factory = crate::layout::contexts::factory::FormattingContextFactory::with_font_context_viewport_and_cb(
                self.font_context.clone(),
                self.viewport_size,
                self.nearest_positioned_cb,
            );
      let fc = factory.create(fc_type);

      let mut layout_child = (*box_node).clone();
      let mut layout_style = (*layout_child.style).clone();
      layout_style.width = Some(Length::px(bounds.width()));
      layout_style.height = Some(Length::px(bounds.height()));
      layout_child.style = Arc::new(layout_style);

      let child_constraints = LayoutConstraints::new(
        CrateAvailableSpace::Definite(bounds.width()),
        CrateAvailableSpace::Definite(bounds.height()),
      )
      .with_inline_percentage_base(
        constraints
          .inline_percentage_base
          .or_else(|| Some(bounds.width())),
      );

      let mut laid_out = fc.layout(&layout_child, &child_constraints)?;
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

  /// Computes intrinsic size using Taffy
  fn compute_intrinsic_size(
    &self,
    box_node: &BoxNode,
    mode: IntrinsicSizingMode,
  ) -> Result<f32, LayoutError> {
    let style = &box_node.style;
    if style.containment.size || style.containment.inline_size {
      let edges = self.horizontal_edges_px(style).unwrap_or(0.0);
      return Ok(edges.max(0.0));
    }

    let mut taffy = TaffyTree::new();
    let (root_id, _node_map) = self.build_taffy_tree(&mut taffy, box_node)?;

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

    taffy
      .compute_layout(root_id, available_space)
      .map_err(|e| LayoutError::MissingContext(format!("Taffy compute error: {:?}", e)))?;

    let layout = taffy
      .layout(root_id)
      .map_err(|e| LayoutError::MissingContext(format!("Taffy layout error: {:?}", e)))?;

    Ok(layout.size.width)
  }
}

fn translate_fragment_tree(fragment: &mut FragmentNode, delta: Point) {
  fragment.bounds = Rect::new(
    Point::new(fragment.bounds.x() + delta.x, fragment.bounds.y() + delta.y),
    fragment.bounds.size,
  );
  for child in &mut fragment.children {
    translate_fragment_tree(child, delta);
  }
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
    let _profile = layout_timer(LayoutKind::Grid);
    if let Some(cached) = layout_cache_lookup(
      box_node,
      FormattingContextType::Grid,
      constraints,
      self.viewport_size,
    ) {
      return Ok(cached);
    }
    // Create fresh Taffy tree for this layout
    let mut taffy = TaffyTree::new();

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
    let (root_id, node_map) =
      self.build_taffy_tree_children(&mut taffy, box_node, &in_flow_children)?;

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
    taffy
      .compute_layout(root_id, available_space)
      .map_err(|e| LayoutError::MissingContext(format!("Taffy compute error: {:?}", e)))?;

    // Convert back to FragmentNode tree and layout each in-flow child using its formatting context.
    let mut fragment = self.convert_to_fragments(&taffy, root_id, &node_map, &constraints)?;

    if let Some(trace_id) = std::env::var("FASTR_TRACE_GRID_TEXT")
      .ok()
      .and_then(|v| v.parse::<usize>().ok())
    {
      if trace_id == box_node.id {
        fn count_texts(node: &FragmentNode) -> usize {
          let mut count = 0;
          fn walk(node: &FragmentNode, count: &mut usize) {
            if matches!(node.content, FragmentContent::Text { .. }) {
              *count += 1;
            }
            for child in &node.children {
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

      let abs = crate::layout::absolute_positioning::AbsoluteLayout::with_font_context(
        self.font_context.clone(),
      );
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

        let factory =
                    crate::layout::contexts::factory::FormattingContextFactory::with_font_context_viewport_and_cb(
                        self.font_context.clone(),
                        self.viewport_size,
                        cb,
                    );
        let fc_type = layout_child
          .formatting_context()
          .unwrap_or(crate::style::display::FormattingContextType::Block);
        let fc = factory.create(fc_type);
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
        child_fragment.bounds = crate::geometry::Rect::new(result.position, result.size);
        fragment.children.push(child_fragment);
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
  use crate::style::display::FormattingContextType;
  use crate::style::types::AlignItems;
  use crate::style::types::AspectRatio;
  use crate::style::types::GridAutoFlow;
  use crate::style::types::GridTrack;
  use crate::style::types::Overflow;
  use crate::style::types::ScrollbarWidth;
  use crate::style::types::WritingMode;
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

  #[test]
  fn convert_style_sets_overflow_and_scrollbar_width() {
    let mut style = ComputedStyle::default();
    style.display = CssDisplay::Grid;
    style.overflow_x = Overflow::Scroll;
    style.overflow_y = Overflow::Clip;
    style.scrollbar_width = ScrollbarWidth::Thin;

    let node = BoxNode::new_block(Arc::new(style), FormattingContextType::Grid, vec![]);
    let gc = GridFormattingContext::new();
    let taffy_style = gc.convert_style(&node.style, None, false);

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
    let (_root_id, _map) = gc
      .build_taffy_tree(&mut TaffyTree::new(), &parent)
      .expect("grid conversion");
    // If the fast path is taken, the parent container style should have been converted to block.
    let taffy_style = gc.convert_style(&parent.style, None, true);
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
}
