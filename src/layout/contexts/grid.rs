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

use std::collections::HashMap;

use taffy::geometry::Line;
use taffy::prelude::{TaffyFitContent, TaffyMaxContent, TaffyMinContent};
use taffy::style::{
    AlignContent as TaffyAlignContent, Dimension, Display, GridPlacement as TaffyGridPlacement, GridTemplateArea,
    GridTemplateComponent, GridTemplateRepetition, LengthPercentage, LengthPercentageAuto, MaxTrackSizingFunction,
    MinTrackSizingFunction, RepetitionCount, Style as TaffyStyle, TrackSizingFunction,
};
use taffy::style_helpers::TaffyAuto;
use taffy::tree::{NodeId as TaffyNodeId, TaffyTree};

use crate::geometry::Rect;
use crate::layout::constraints::{AvailableSpace as CrateAvailableSpace, LayoutConstraints};
use crate::layout::formatting_context::{FormattingContext, IntrinsicSizingMode, LayoutError};
use crate::style::display::Display as CssDisplay;
use crate::style::types::{
    AlignContent, AlignItems, AspectRatio, BoxSizing, Direction, GridAutoFlow, GridTrack, JustifyContent, WritingMode,
};
use crate::style::values::Length;
use crate::style::ComputedStyle;
use crate::style::grid::validate_area_rectangles;
use crate::tree::box_tree::BoxNode;
use crate::tree::fragment_tree::FragmentNode;

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
}

impl GridFormattingContext {
    /// Creates a new GridFormattingContext
    pub fn new() -> Self {
        Self {
            viewport_size: crate::geometry::Size::new(800.0, 600.0),
        }
    }

    fn inline_axis_positive(&self, style: &ComputedStyle) -> bool {
        match style.writing_mode {
            WritingMode::HorizontalTb => style.direction != Direction::Rtl,
            WritingMode::SidewaysRl => false,
            WritingMode::SidewaysLr => true,
            WritingMode::VerticalRl | WritingMode::VerticalLr => true,
        }
    }

    fn block_axis_positive(&self, style: &ComputedStyle) -> bool {
        match style.writing_mode {
            WritingMode::HorizontalTb | WritingMode::SidewaysRl | WritingMode::SidewaysLr => true,
            WritingMode::VerticalRl => false,
            WritingMode::VerticalLr => true,
        }
    }

    fn inline_axis_is_horizontal(&self, style: &ComputedStyle) -> bool {
        matches!(
            style.writing_mode,
            WritingMode::HorizontalTb | WritingMode::SidewaysLr | WritingMode::SidewaysRl
        )
    }

    pub fn with_viewport(viewport_size: crate::geometry::Size) -> Self {
        Self { viewport_size }
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
        let mut node_map = HashMap::new();
        let root_id = self.build_node_recursive(taffy, box_node, &mut node_map)?;
        Ok((root_id, node_map))
    }

    /// Recursively builds a Taffy node and its children
    fn build_node_recursive<'a>(
        &self,
        taffy: &mut TaffyTree<()>,
        box_node: &'a BoxNode,
        node_map: &mut HashMap<TaffyNodeId, &'a BoxNode>,
    ) -> Result<TaffyNodeId, LayoutError> {
        // Convert children first
        let child_ids: Vec<TaffyNodeId> = box_node
            .children
            .iter()
            .map(|child| self.build_node_recursive(taffy, child, node_map))
            .collect::<Result<_, _>>()?;

        // Convert style
        let taffy_style = self.convert_style(&box_node.style);

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
    fn convert_style(&self, style: &ComputedStyle) -> TaffyStyle {
        let mut taffy_style = TaffyStyle::default();
        let inline_positive = self.inline_axis_positive(style);
        let block_positive = self.block_axis_positive(style);
        let inline_is_horizontal = self.inline_axis_is_horizontal(style);

        // Display mode
        let is_grid = matches!(style.display, CssDisplay::Grid | CssDisplay::InlineGrid);
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
            width: self.convert_opt_length_to_dimension_box_sizing(&style.min_width, style, Axis::Horizontal),
            height: self.convert_opt_length_to_dimension_box_sizing(&style.min_height, style, Axis::Vertical),
        };
        taffy_style.max_size = taffy::geometry::Size {
            width: self.convert_opt_length_to_dimension_box_sizing(&style.max_width, style, Axis::Horizontal),
            height: self.convert_opt_length_to_dimension_box_sizing(&style.max_height, style, Axis::Vertical),
        };

        // Margin
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
        taffy_style.align_items = Some(self.convert_align_items(&style.align_items, block_positive));

        // Grid container properties
        if is_grid {
            // Grid template columns/rows (swap when inline axis is vertical)
            if inline_is_horizontal {
                taffy_style.grid_template_columns = self.convert_grid_template(&style.grid_template_columns, style);
                taffy_style.grid_template_rows = self.convert_grid_template(&style.grid_template_rows, style);
            } else {
                taffy_style.grid_template_columns = self.convert_grid_template(&style.grid_template_rows, style);
                taffy_style.grid_template_rows = self.convert_grid_template(&style.grid_template_columns, style);
            }

            // Line names
            if inline_is_horizontal {
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
            if inline_is_horizontal {
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
            taffy_style.grid_auto_flow = match (style.grid_auto_flow, inline_is_horizontal) {
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
                        let (row_start, row_end, column_start, column_end) = if inline_is_horizontal {
                            ((top as u16) + 1, (bottom as u16) + 2, (left as u16) + 1, (right as u16) + 2)
                        } else {
                            // Transpose area matrix for vertical inline axis
                            ((left as u16) + 1, (right as u16) + 2, (top as u16) + 1, (bottom as u16) + 2)
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
                width: if inline_is_horizontal {
                    self.convert_length_to_lp(&style.grid_column_gap, style)
                } else {
                    self.convert_length_to_lp(&style.grid_row_gap, style)
                },
                height: if inline_is_horizontal {
                    self.convert_length_to_lp(&style.grid_row_gap, style)
                } else {
                    self.convert_length_to_lp(&style.grid_column_gap, style)
                },
            };

            // Alignment
            taffy_style.align_content = Some(self.convert_align_content(&style.align_content, block_positive));
            taffy_style.justify_content = Some(self.convert_justify_content(&style.justify_content, inline_positive));
        }
        taffy_style.align_items = Some(self.convert_align_items(&style.align_items, block_positive));
        taffy_style.justify_items = Some(self.convert_align_items(&style.justify_items, inline_positive));
        taffy_style.align_self = style.align_self.map(|a| self.convert_align_items(&a, block_positive));
        taffy_style.justify_self = style.justify_self.map(|a| self.convert_align_items(&a, inline_positive));

        // Grid item properties using raw line numbers (swap placements when inline axis is vertical)
        if inline_is_horizontal {
            taffy_style.grid_column = self.convert_grid_placement(
                style.grid_column_raw.as_deref(),
                style.grid_column_start,
                style.grid_column_end,
            );
            taffy_style.grid_row =
                self.convert_grid_placement(style.grid_row_raw.as_deref(), style.grid_row_start, style.grid_row_end);
        } else {
            taffy_style.grid_column = self.convert_grid_placement(
                style.grid_row_raw.as_deref(),
                style.grid_row_start,
                style.grid_row_end,
            );
            taffy_style.grid_row = self.convert_grid_placement(
                style.grid_column_raw.as_deref(),
                style.grid_column_start,
                style.grid_column_end,
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
    fn convert_opt_length_to_lpa(&self, length: &Option<Length>, style: &ComputedStyle) -> LengthPercentageAuto {
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
        use crate::style::values::LengthUnit::*;
        match len.unit {
            Percent => None,
            Px | Pt | In | Cm | Mm | Pc => Some(len.to_px()),
            Rem => Some(len.value * style.root_font_size),
            Em => Some(len.value * style.font_size),
            Ex => Some(len.value * style.font_size * 0.5),
            Ch => Some(len.value * style.font_size * 0.5),
            unit if unit.is_viewport_relative() => {
                Some(len.resolve_with_viewport(self.viewport_size.width, self.viewport_size.height))
            }
            _ => None,
        }
    }

    /// Converts GridTrack Vec to Taffy track list
    fn convert_grid_template(&self, tracks: &[GridTrack], style: &ComputedStyle) -> Vec<GridTemplateComponent<String>> {
        let mut components = Vec::new();
        for track in tracks {
            match track {
                GridTrack::RepeatAutoFill { tracks: inner, line_names } => {
                    let converted: Vec<TrackSizingFunction> =
                        inner.iter().map(|t| self.convert_track_size(t, style)).collect();
                    let repetition = GridTemplateRepetition {
                        count: RepetitionCount::AutoFill,
                        tracks: converted,
                        line_names: line_names.clone(),
                    };
                    components.push(GridTemplateComponent::Repeat(repetition));
                }
                GridTrack::RepeatAutoFit { tracks: inner, line_names } => {
                    let converted: Vec<TrackSizingFunction> =
                        inner.iter().map(|t| self.convert_track_size(t, style)).collect();
                    let repetition = GridTemplateRepetition {
                        count: RepetitionCount::AutoFit,
                        tracks: converted,
                        line_names: line_names.clone(),
                    };
                    components.push(GridTemplateComponent::Repeat(repetition));
                }
                _ => components.push(GridTemplateComponent::Single(self.convert_track_size(track, style))),
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
            GridTrack::FitContent(len) => TrackSizingFunction::fit_content(self.convert_length_to_lp(len, style)),
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
            GridTrack::RepeatAutoFill { .. } | GridTrack::RepeatAutoFit { .. } => TrackSizingFunction::AUTO,
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
            GridTrack::FitContent(len) => MaxTrackSizingFunction::fit_content(self.convert_length_to_lp(len, style)),
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
                if axis_positive { TaffyAlignContent::Start } else { TaffyAlignContent::End }
            }
            AlignContent::FlexEnd => {
                if axis_positive { TaffyAlignContent::End } else { TaffyAlignContent::Start }
            }
            AlignContent::Center => TaffyAlignContent::Center,
            AlignContent::Stretch => TaffyAlignContent::Stretch,
            AlignContent::SpaceBetween => TaffyAlignContent::SpaceBetween,
            AlignContent::SpaceEvenly => TaffyAlignContent::SpaceEvenly,
            AlignContent::SpaceAround => TaffyAlignContent::SpaceAround,
        }
    }

    fn convert_justify_content(&self, justify: &JustifyContent, axis_positive: bool) -> TaffyAlignContent {
        match justify {
            JustifyContent::FlexStart => {
                if axis_positive { TaffyAlignContent::Start } else { TaffyAlignContent::End }
            }
            JustifyContent::FlexEnd => {
                if axis_positive { TaffyAlignContent::End } else { TaffyAlignContent::Start }
            }
            JustifyContent::Center => TaffyAlignContent::Center,
            JustifyContent::SpaceBetween => TaffyAlignContent::SpaceBetween,
            JustifyContent::SpaceAround => TaffyAlignContent::SpaceAround,
            JustifyContent::SpaceEvenly => TaffyAlignContent::SpaceEvenly,
        }
    }

    fn convert_align_items(&self, align: &AlignItems, axis_positive: bool) -> taffy::style::AlignItems {
        match align {
            AlignItems::Start | AlignItems::SelfStart => {
                if axis_positive { taffy::style::AlignItems::Start } else { taffy::style::AlignItems::End }
            }
            AlignItems::End | AlignItems::SelfEnd => {
                if axis_positive { taffy::style::AlignItems::End } else { taffy::style::AlignItems::Start }
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
        taffy: &TaffyTree<()>,
        node_id: TaffyNodeId,
        node_map: &HashMap<TaffyNodeId, &BoxNode>,
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
            .map(|&child_id| Self::convert_to_fragments(taffy, child_id, node_map))
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
            Ok(FragmentNode::new_block_styled(
                bounds,
                child_fragments,
                box_node.style.clone(),
            ))
        } else {
            Ok(FragmentNode::new_block(bounds, child_fragments))
        }
    }

    /// Computes intrinsic size using Taffy
    fn compute_intrinsic_size(&self, box_node: &BoxNode, mode: IntrinsicSizingMode) -> Result<f32, LayoutError> {
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

    let parts: Vec<&str> = trimmed.split_whitespace().filter(|p| !p.is_empty()).collect();
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
    fn layout(&self, box_node: &BoxNode, constraints: &LayoutConstraints) -> Result<FragmentNode, LayoutError> {
        // Create fresh Taffy tree for this layout
        let mut taffy = TaffyTree::new();

        // Build Taffy tree from BoxNode
        let (root_id, node_map) = self.build_taffy_tree(&mut taffy, box_node)?;

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

        // Convert back to FragmentNode tree
        Self::convert_to_fragments(&taffy, root_id, &node_map)
    }

    fn compute_intrinsic_inline_size(&self, box_node: &BoxNode, mode: IntrinsicSizingMode) -> Result<f32, LayoutError> {
        self.compute_intrinsic_size(box_node, mode)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::style::display::FormattingContextType;
    use crate::style::types::{AlignItems, AspectRatio, GridTrack};
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

    // Test 1: Basic grid container creation
    #[test]
    fn test_grid_fc_creation() {
        let fc = GridFormattingContext::new();
        let default_fc = GridFormattingContext::default();
        assert_eq!(std::mem::size_of_val(&fc), std::mem::size_of_val(&default_fc));
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

        let style = make_grid_style_with_tracks(vec![GridTrack::Length(Length::px(100.0)), GridTrack::Fr(1.0)], vec![]);

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
        let inner_grid = BoxNode::new_block(make_grid_style(), FormattingContextType::Grid, vec![inner_child]);
        let outer_grid = BoxNode::new_block(make_grid_style(), FormattingContextType::Grid, vec![inner_grid]);

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

        let grid = BoxNode::new_block(style, FormattingContextType::Grid, vec![child1, child2, child3]);

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
