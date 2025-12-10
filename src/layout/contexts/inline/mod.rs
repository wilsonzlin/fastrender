//! Inline Formatting Context Layout
//!
//! This module implements the Inline Formatting Context (IFC) layout algorithm
//! as specified in CSS 2.1 Section 9.4.2.
//!
//! # Inline Formatting Context
//!
//! An IFC is a layout mode where inline-level boxes are laid out horizontally,
//! one after another, starting from the top of the containing block. They are
//! stacked vertically as line boxes.
//!
//! # Key Features
//!
//! - **Horizontal flow**: Inline boxes flow left-to-right (in LTR text)
//! - **Line boxes**: Content is divided into rectangular areas called line boxes
//! - **Baseline alignment**: Items align vertically based on their baselines
//! - **Line breaking**: Content wraps when it exceeds available width
//!
//! # Module Structure
//!
//! - `baseline` - Baseline alignment and vertical positioning
//! - `line_builder` - Line box construction and breaking
//!
//! # CSS Specification
//!
//! Reference: <https://www.w3.org/TR/CSS21/visuren.html#inline-formatting>
//!
//! # Example
//!
//! ```rust,ignore
//! use fastrender::layout::contexts::inline::InlineFormattingContext;
//! use fastrender::{FormattingContext, LayoutConstraints};
//!
//! let ifc = InlineFormattingContext::new();
//! let constraints = LayoutConstraints::definite_width(400.0);
//! let fragment = ifc.layout(&box_node, &constraints)?;
//! ```

pub mod baseline;
pub mod line_builder;

use crate::geometry::{Rect, Size};
use crate::layout::constraints::{AvailableSpace, LayoutConstraints};
use crate::layout::contexts::factory::FormattingContextFactory;
use crate::layout::contexts::inline::line_builder::InlineBoxItem;
use crate::layout::formatting_context::{FormattingContext, IntrinsicSizingMode, LayoutError};
use crate::layout::utils::{border_size_from_box_sizing, compute_replaced_size, resolve_length_with_percentage};
use crate::style::types::{
    FontStyle, HyphensMode, ListStylePosition, OverflowWrap, TabSize, TextAlign, TextJustify, TextTransform,
    WhiteSpace, WordBreak,
};
use crate::style::values::{Length, LengthUnit};
use crate::style::ComputedStyle;
use crate::text::font_loader::FontContext;
use crate::text::hyphenation::Hyphenator;
use crate::text::justify::is_cjk_character;
use crate::text::line_break::{find_break_opportunities, BreakType};
use crate::text::pipeline::{ShapedRun, ShapingPipeline};
use crate::tree::box_tree::{BoxNode, BoxType, MarkerContent, ReplacedBox};
use crate::tree::fragment_tree::{FragmentContent, FragmentNode};
use std::sync::Arc;

use crate::text::font_db::ScaledMetrics;
use baseline::{compute_line_height_with_metrics, BaselineMetrics};
use line_builder::{InlineBlockItem, InlineItem, Line, LineBuilder, PositionedItem, ReplacedItem, TabItem, TextItem};

/// Inline Formatting Context implementation
///
/// Implements the FormattingContext trait for inline-level layout.
/// Handles horizontal flow, line breaking, and baseline alignment.
///
/// # Layout Process
///
/// 1. **Collect inline items**: Convert BoxNode children to InlineItems
/// 2. **Build lines**: Use LineBuilder to break content into lines
/// 3. **Position items**: Calculate final positions with baseline alignment
/// 4. **Create fragments**: Generate FragmentNode tree with positioned content
///
/// # Text Handling
///
/// Text boxes are shaped using the text shaping pipeline:
/// 1. Shape text with the ShapingPipeline (bidi/script/font fallback)
/// 2. Find break opportunities using UAX #14
/// 3. Break into lines respecting available width
/// 4. Create text fragments with baseline offsets
pub struct InlineFormattingContext {
    /// Text shaping pipeline (bidi/script/fallback aware)
    pipeline: ShapingPipeline,
    font_context: FontContext,
    default_hyphenator: Option<Hyphenator>,
    viewport_size: crate::geometry::Size,
}

impl InlineFormattingContext {
    /// Creates a new InlineFormattingContext
    pub fn new() -> Self {
        Self {
            pipeline: ShapingPipeline::new(),
            font_context: FontContext::new(),
            default_hyphenator: Hyphenator::new("en-us").ok(),
            viewport_size: crate::geometry::Size::new(800.0, 600.0),
        }
    }

    pub fn with_font_context(font_context: FontContext) -> Self {
        Self::with_font_context_and_viewport(font_context, crate::geometry::Size::new(800.0, 600.0))
    }

    pub fn with_font_context_and_viewport(font_context: FontContext, viewport_size: crate::geometry::Size) -> Self {
        Self {
            pipeline: ShapingPipeline::new(),
            font_context,
            default_hyphenator: Hyphenator::new("en-us").ok(),
            viewport_size,
        }
    }

    fn hyphenator_for(&self, language: &str) -> Option<Hyphenator> {
        Hyphenator::new(language)
            .ok()
            .or_else(|| self.default_hyphenator.clone())
    }

    fn hyphen_advance(&self, style: &ComputedStyle) -> f32 {
        const INSERTED_HYPHEN: &str = "\u{2010}";
        match self.pipeline.shape(INSERTED_HYPHEN, style, &self.font_context) {
            Ok(mut runs) => {
                TextItem::apply_spacing_to_runs(&mut runs, INSERTED_HYPHEN, style.letter_spacing, style.word_spacing);
                runs.iter().map(|r| r.advance).sum()
            }
            Err(_) => style.font_size * 0.5,
        }
    }

    fn convert_vertical_align(
        &self,
        align: crate::style::types::VerticalAlign,
        font_size: f32,
        line_height: f32,
    ) -> crate::layout::contexts::inline::baseline::VerticalAlign {
        use crate::layout::contexts::inline::baseline::VerticalAlign as Align;
        use crate::style::types::VerticalAlign as StyleAlign;

        match align {
            StyleAlign::Baseline => Align::Baseline,
            StyleAlign::Sub => Align::Sub,
            StyleAlign::Super => Align::Super,
            StyleAlign::TextTop => Align::TextTop,
            StyleAlign::TextBottom => Align::TextBottom,
            StyleAlign::Middle => Align::Middle,
            StyleAlign::Top => Align::Top,
            StyleAlign::Bottom => Align::Bottom,
            StyleAlign::Length(len) => {
                let px = if len.unit.is_font_relative() {
                    len.resolve_with_font_size(font_size)
                } else if len.unit.is_percentage() {
                    len.resolve_against(line_height)
                } else if len.unit.is_absolute() {
                    len.to_px()
                } else {
                    len.value
                };
                Align::Length(px)
            }
            StyleAlign::Percentage(p) => Align::Percentage(p),
        }
    }

    /// Collects inline items from box node children
    fn collect_inline_items(
        &self,
        box_node: &BoxNode,
        available_width: f32,
        available_height: Option<f32>,
    ) -> Result<Vec<InlineItem>, LayoutError> {
        let mut pending_space: Option<PendingSpace> = None;
        self.collect_inline_items_internal(box_node, available_width, available_height, &mut pending_space)
    }

    fn collect_inline_items_internal(
        &self,
        box_node: &BoxNode,
        available_width: f32,
        available_height: Option<f32>,
        pending_space: &mut Option<PendingSpace>,
    ) -> Result<Vec<InlineItem>, LayoutError> {
        let mut items = Vec::new();

        for child in &box_node.children {
            if let Some(space) = pending_space.take() {
                let space_item = self.create_collapsed_space_item(&space.style, space.allow_soft_wrap)?;
                items.push(space_item);
            }
            match &child.box_type {
                BoxType::Text(text_box) => {
                    let normalized = normalize_text_for_white_space(
                        &apply_text_transform(&text_box.text, child.style.text_transform, child.style.white_space),
                        child.style.white_space,
                    );

                    let mut produced = self.create_inline_items_from_normalized(child, normalized.clone(), false)?;
                    items.append(&mut produced);
                    if normalized.trailing_collapsible {
                        *pending_space = Some(PendingSpace::new(child.style.clone(), normalized.allow_soft_wrap));
                    }
                }
                BoxType::Marker(marker_box) => match &marker_box.content {
                    MarkerContent::Text(text) => {
                        let normalized = NormalizedText {
                            text: text.clone(),
                            forced_breaks: Vec::new(),
                            allow_soft_wrap: false,
                            leading_collapsible: false,
                            trailing_collapsible: false,
                        };
                        let mut produced = self.create_inline_items_from_normalized(child, normalized.clone(), true)?;
                        items.append(&mut produced);
                        if normalized.trailing_collapsible {
                            *pending_space = Some(PendingSpace::new(child.style.clone(), normalized.allow_soft_wrap));
                        }
                    }
                    MarkerContent::Image(replaced_box) => {
                        let mut item =
                            self.create_replaced_item(child, replaced_box, available_width, available_height)?;
                        let raw_gap = child
                            .style
                            .margin_right
                            .as_ref()
                            .map(|m| {
                                resolve_length_for_width(
                                    *m,
                                    0.0,
                                    child.style.font_size,
                                    child.style.root_font_size,
                                    self.viewport_size,
                                )
                            })
                            .unwrap_or(0.0);
                        let gap = if raw_gap.abs() > f32::EPSILON {
                            raw_gap
                        } else {
                            child.style.font_size * 0.5
                        };
                        item = item.as_marker(gap, child.style.list_style_position, child.style.direction);
                        items.push(InlineItem::Replaced(item));
                    }
                },
                BoxType::Inline(_) => {
                    if child.formatting_context().is_some() {
                        let item = self.layout_inline_block(child, available_width, available_height)?;
                        items.push(InlineItem::InlineBlock(item));
                        continue;
                    }
                    // Regular inline box: wrap children in an inline box item that carries padding/borders
                    let percentage_base = if available_width.is_finite() {
                        available_width
                    } else {
                        0.0
                    };
                    let padding_left = resolve_length_for_width(
                        child.style.padding_left,
                        percentage_base,
                        child.style.font_size,
                        child.style.root_font_size,
                        self.viewport_size,
                    );
                    let padding_right = resolve_length_for_width(
                        child.style.padding_right,
                        percentage_base,
                        child.style.font_size,
                        child.style.root_font_size,
                        self.viewport_size,
                    );
                    let padding_top = resolve_length_for_width(
                        child.style.padding_top,
                        percentage_base,
                        child.style.font_size,
                        child.style.root_font_size,
                        self.viewport_size,
                    );
                    let padding_bottom = resolve_length_for_width(
                        child.style.padding_bottom,
                        percentage_base,
                        child.style.font_size,
                        child.style.root_font_size,
                        self.viewport_size,
                    );
                    let border_left = resolve_length_for_width(
                        child.style.border_left_width,
                        percentage_base,
                        child.style.font_size,
                        child.style.root_font_size,
                        self.viewport_size,
                    );
                    let border_right = resolve_length_for_width(
                        child.style.border_right_width,
                        percentage_base,
                        child.style.font_size,
                        child.style.root_font_size,
                        self.viewport_size,
                    );
                    let border_top = resolve_length_for_width(
                        child.style.border_top_width,
                        percentage_base,
                        child.style.font_size,
                        child.style.root_font_size,
                        self.viewport_size,
                    );
                    let border_bottom = resolve_length_for_width(
                        child.style.border_bottom_width,
                        percentage_base,
                        child.style.font_size,
                        child.style.root_font_size,
                        self.viewport_size,
                    );

                    let start_edge = padding_left + border_left;
                    let end_edge = padding_right + border_right;
                    let content_offset_y = padding_top + border_top;

                    // Recursively collect children first
                    let child_items =
                        self.collect_inline_items_internal(child, available_width, available_height, pending_space)?;
                    let fallback_metrics = self.compute_strut_metrics(&child.style);
                    let metrics = compute_inline_box_metrics(
                        &child_items,
                        content_offset_y,
                        padding_bottom + border_bottom,
                        fallback_metrics,
                    );

                    let mut inline_box = InlineBoxItem::new(
                        start_edge,
                        end_edge,
                        content_offset_y,
                        metrics,
                        child.style.clone(),
                        0,
                        child.style.direction,
                        child.style.unicode_bidi,
                    );
                    inline_box.vertical_align = self.convert_vertical_align(
                        child.style.vertical_align,
                        child.style.font_size,
                        metrics.line_height,
                    );
                    for item in child_items {
                        inline_box.add_child(item);
                    }

                    items.push(InlineItem::InlineBox(inline_box));
                }
                BoxType::Replaced(replaced_box) => {
                    let item = self.create_replaced_item(child, replaced_box, available_width, available_height)?;
                    items.push(InlineItem::Replaced(item));
                }
                _ => {
                    // Skip block-level boxes in inline context
                }
            }
        }

        Ok(items)
    }

    fn layout_inline_block(
        &self,
        box_node: &BoxNode,
        available_width: f32,
        available_height: Option<f32>,
    ) -> Result<InlineBlockItem, LayoutError> {
        let fc_type = box_node.formatting_context().ok_or_else(|| {
            LayoutError::UnsupportedBoxType("Inline-level box missing formatting context".to_string())
        })?;

        let style = &box_node.style;
        let metrics = self.resolve_scaled_metrics(style);
        let line_height = compute_line_height_with_metrics(style, metrics.as_ref());
        let factory =
            FormattingContextFactory::with_font_context_and_viewport(self.font_context.clone(), self.viewport_size);
        let fc = factory.create(fc_type);

        let percentage_base = if available_width.is_finite() {
            available_width
        } else {
            0.0
        };
        let margin_left = style
            .margin_left
            .as_ref()
            .map(|l| {
                resolve_length_for_width(
                    *l,
                    percentage_base,
                    style.font_size,
                    style.root_font_size,
                    self.viewport_size,
                )
            })
            .unwrap_or(0.0);
        let margin_right = style
            .margin_right
            .as_ref()
            .map(|l| {
                resolve_length_for_width(
                    *l,
                    percentage_base,
                    style.font_size,
                    style.root_font_size,
                    self.viewport_size,
                )
            })
            .unwrap_or(0.0);
        let available_for_box = if available_width.is_finite() {
            (available_width - margin_left - margin_right).max(0.0)
        } else {
            available_width
        };

        // CSS 2.1 §10.3.9: inline-block width auto -> shrink-to-fit:
        // min(max(preferred_min, available), preferred)
        let preferred_min_content = fc
            .compute_intrinsic_inline_size(box_node, IntrinsicSizingMode::MinContent)
            .unwrap_or(0.0);
        let preferred_content = fc
            .compute_intrinsic_inline_size(box_node, IntrinsicSizingMode::MaxContent)
            .unwrap_or(preferred_min_content);

        let horizontal_edges = horizontal_padding_and_borders(style, percentage_base, self.viewport_size);
        let preferred_min = preferred_min_content + horizontal_edges;
        let preferred = preferred_content + horizontal_edges;

        let specified_width = style
            .width
            .as_ref()
            .map(|l| {
                resolve_length_for_width(
                    *l,
                    percentage_base,
                    style.font_size,
                    style.root_font_size,
                    self.viewport_size,
                )
            })
            .map(|w| border_size_from_box_sizing(w, horizontal_edges, style.box_sizing));

        let min_width = style
            .min_width
            .as_ref()
            .map(|l| {
                resolve_length_for_width(
                    *l,
                    percentage_base,
                    style.font_size,
                    style.root_font_size,
                    self.viewport_size,
                )
            })
            .map(|w| border_size_from_box_sizing(w, horizontal_edges, style.box_sizing))
            .unwrap_or(0.0);
        let max_width = style
            .max_width
            .as_ref()
            .map(|l| {
                resolve_length_for_width(
                    *l,
                    percentage_base,
                    style.font_size,
                    style.root_font_size,
                    self.viewport_size,
                )
            })
            .map(|w| border_size_from_box_sizing(w, horizontal_edges, style.box_sizing))
            .unwrap_or(f32::INFINITY);

        let constraint_width = if let Some(content_width) = specified_width {
            // Honor specified width; use containing block width for layout to avoid over-constraining margins.
            let used = content_width.clamp(min_width, max_width);
            if available_for_box.is_finite() {
                available_for_box
            } else {
                used
            }
        } else {
            let available = if available_for_box.is_finite() {
                available_for_box
            } else {
                preferred
            };
            let shrink = preferred.min(available.max(preferred_min));
            let used = shrink.clamp(min_width, max_width);
            used
        };

        let height_space = available_height
            .filter(|h| h.is_finite())
            .map(AvailableSpace::Definite)
            .unwrap_or(AvailableSpace::Indefinite);
        let constraints = LayoutConstraints::new(AvailableSpace::Definite(constraint_width), height_space);
        let fragment = fc.layout(box_node, &constraints)?;

        let margin_left = style
            .margin_left
            .as_ref()
            .map(|l| {
                resolve_length_for_width(
                    *l,
                    percentage_base,
                    style.font_size,
                    style.root_font_size,
                    self.viewport_size,
                )
            })
            .unwrap_or(0.0);
        let margin_right = style
            .margin_right
            .as_ref()
            .map(|l| {
                resolve_length_for_width(
                    *l,
                    percentage_base,
                    style.font_size,
                    style.root_font_size,
                    self.viewport_size,
                )
            })
            .unwrap_or(0.0);

        let va = self.convert_vertical_align(style.vertical_align, style.font_size, line_height);

        Ok(InlineBlockItem::new(
            fragment,
            box_node.style.direction,
            box_node.style.unicode_bidi,
            margin_left,
            margin_right,
        )
        .with_vertical_align(va))
    }

    /// Creates inline items from a text box, splitting preserved tabs into explicit tab items.
    #[allow(dead_code)]
    fn create_inline_items_for_text(
        &self,
        box_node: &BoxNode,
        text: &str,
        is_marker: bool,
    ) -> Result<Vec<InlineItem>, LayoutError> {
        let style = &box_node.style;
        let transformed = apply_text_transform(text, style.text_transform, style.white_space);
        let normalized = normalize_text_for_white_space(&transformed, style.white_space);
        self.create_inline_items_from_normalized(box_node, normalized, is_marker)
    }

    fn create_inline_items_from_normalized(
        &self,
        box_node: &BoxNode,
        normalized: NormalizedText,
        is_marker: bool,
    ) -> Result<Vec<InlineItem>, LayoutError> {
        let NormalizedText {
            text: normalized_text,
            forced_breaks,
            allow_soft_wrap,
            ..
        } = normalized;

        if !normalized_text.contains('\t') {
            if normalized_text.is_empty() {
                return Ok(Vec::new());
            }
            let item = self.create_text_item_from_normalized(
                &box_node.style,
                &normalized_text,
                forced_breaks,
                allow_soft_wrap,
                is_marker,
            )?;
            return Ok(vec![InlineItem::Text(item)]);
        }

        let mut items = Vec::new();
        let mut segment_start = 0;

        for (idx, ch) in normalized_text.char_indices() {
            if ch != '\t' {
                continue;
            }
            if idx > segment_start {
                let segment_breaks = slice_breaks(&forced_breaks, segment_start, idx);
                let item = self.create_text_item_from_normalized(
                    &box_node.style,
                    &normalized_text[segment_start..idx],
                    segment_breaks,
                    allow_soft_wrap,
                    is_marker,
                )?;
                if !item.text.is_empty() {
                    items.push(InlineItem::Text(item));
                }
            }
            let tab = self.create_tab_item(box_node, allow_soft_wrap)?;
            items.push(tab);
            segment_start = idx + ch.len_utf8();
        }

        if segment_start < normalized_text.len() {
            let segment_breaks = slice_breaks(&forced_breaks, segment_start, normalized_text.len());
            let item = self.create_text_item_from_normalized(
                &box_node.style,
                &normalized_text[segment_start..],
                segment_breaks,
                allow_soft_wrap,
                is_marker,
            )?;
            if !item.text.is_empty() {
                items.push(InlineItem::Text(item));
            }
        }

        Ok(items)
    }

    /// Creates a text item from a text box
    #[allow(dead_code)]
    fn create_text_item(&self, box_node: &BoxNode, text: &str) -> Result<TextItem, LayoutError> {
        let style = &box_node.style;
        let transformed = apply_text_transform(text, style.text_transform, style.white_space);
        let normalized = normalize_text_for_white_space(&transformed, style.white_space);
        self.create_text_item_from_normalized(
            &box_node.style,
            &normalized.text,
            normalized.forced_breaks,
            normalized.allow_soft_wrap,
            false,
        )
    }

    fn create_text_item_from_normalized(
        &self,
        style: &Arc<ComputedStyle>,
        normalized_text: &str,
        forced_breaks: Vec<crate::text::line_break::BreakOpportunity>,
        allow_soft_wrap: bool,
        is_marker: bool,
    ) -> Result<TextItem, LayoutError> {
        let metrics = self.resolve_scaled_metrics(style);
        let line_height = compute_line_height_with_metrics(style, metrics.as_ref());
        let (hyphen_free, hyphen_breaks) = if is_marker {
            (normalized_text.to_string(), Vec::new())
        } else {
            let hyphenator = self.hyphenator_for(&style.language);
            hyphenation_breaks(normalized_text, style.hyphens, hyphenator.as_ref(), allow_soft_wrap)
        };

        let forced_break_offsets: Vec<usize> = forced_breaks.iter().map(|b| b.byte_offset).collect();

        let mut shaped_runs = self.shape_with_fallback(&hyphen_free, style)?;
        TextItem::apply_spacing_to_runs(&mut shaped_runs, &hyphen_free, style.letter_spacing, style.word_spacing);

        let metrics = TextItem::metrics_from_runs(&shaped_runs, line_height, style.font_size);

        // Find break opportunities and filter based on white-space handling
        let base_breaks = find_break_opportunities(&hyphen_free);
        let breaks = if is_marker {
            Vec::new()
        } else {
            let mut merged = merge_breaks(base_breaks, forced_breaks, allow_soft_wrap);
            merged.extend(hyphen_breaks);
            apply_break_properties(
                &hyphen_free,
                merged,
                style.word_break,
                style.overflow_wrap,
                allow_soft_wrap,
            )
        };

        let va = self.convert_vertical_align(style.vertical_align, style.font_size, line_height);

        let mut item = TextItem::new(
            shaped_runs,
            hyphen_free,
            metrics,
            breaks,
            forced_break_offsets,
            style.clone(),
        )
        .with_vertical_align(va);

        if is_marker {
            let raw_gap = style
                .margin_right
                .as_ref()
                .map(|m| resolve_length_for_width(*m, 0.0, style.font_size, style.root_font_size, self.viewport_size))
                .unwrap_or(0.0);
            let gap = if raw_gap.abs() > f32::EPSILON {
                raw_gap
            } else {
                style.font_size * 0.5
            };
            let marker_extent = item.advance + gap;
            let sign = if style.direction == crate::style::types::Direction::Rtl {
                1.0
            } else {
                -1.0
            };
            if style.list_style_position == ListStylePosition::Outside {
                item.advance_for_layout = 0.0;
                item.paint_offset = sign * marker_extent;
            } else {
                item.advance_for_layout = marker_extent;
                item.paint_offset = 0.0;
            }
            item.is_marker = true;
        }

        Ok(item)
    }

    fn create_collapsed_space_item(
        &self,
        style: &Arc<ComputedStyle>,
        allow_soft_wrap: bool,
    ) -> Result<InlineItem, LayoutError> {
        let item = self.create_text_item_from_normalized(style, " ", Vec::new(), allow_soft_wrap, false)?;
        Ok(InlineItem::Text(item))
    }

    fn shape_with_fallback(&self, text: &str, style: &ComputedStyle) -> Result<Vec<ShapedRun>, LayoutError> {
        match self.pipeline.shape(text, style, &self.font_context) {
            Ok(runs) => Ok(runs),
            Err(err) => {
                if let Some(fallback_font) = self.font_context.get_sans_serif() {
                    let mut fallback_style = style.clone();
                    fallback_style.font_family = vec![fallback_font.family.clone()];
                    self.pipeline
                        .shape(text, &fallback_style, &self.font_context)
                        .map_err(|e| {
                            LayoutError::MissingContext(format!(
                                "Shaping failed (fonts={}): {:?}",
                                self.font_context.font_count(),
                                e
                            ))
                        })
                } else {
                    Err(LayoutError::MissingContext(format!(
                        "Shaping failed (fonts={}): {:?}",
                        self.font_context.font_count(),
                        err
                    )))
                }
            }
        }
    }

    fn space_advance(&self, style: &ComputedStyle) -> Result<f32, LayoutError> {
        let mut runs = self.shape_with_fallback(" ", style)?;
        TextItem::apply_spacing_to_runs(&mut runs, " ", style.letter_spacing, style.word_spacing);
        Ok(runs.iter().map(|r| r.advance).sum())
    }

    fn create_tab_item(&self, box_node: &BoxNode, allow_wrap: bool) -> Result<InlineItem, LayoutError> {
        let style = &box_node.style;
        let metrics = self.compute_strut_metrics(style);
        let space_advance = self.space_advance(style)?;
        let tab_interval = match style.tab_size {
            TabSize::Number(n) => n.max(0.0) * space_advance,
            TabSize::Length(len) => {
                resolve_length_with_percentage(len, None, self.viewport_size, style.font_size, style.root_font_size)
                    .unwrap_or(0.0)
            }
        };
        let va = self.convert_vertical_align(style.vertical_align, style.font_size, metrics.line_height);
        Ok(InlineItem::Tab(
            TabItem::new(style.clone(), metrics, tab_interval, allow_wrap).with_vertical_align(va),
        ))
    }

    /// Creates a replaced item from a replaced box
    fn create_replaced_item(
        &self,
        box_node: &BoxNode,
        replaced_box: &ReplacedBox,
        available_width: f32,
        available_height: Option<f32>,
    ) -> Result<ReplacedItem, LayoutError> {
        let style = &box_node.style;
        let metrics = self.resolve_scaled_metrics(style);
        let line_height = compute_line_height_with_metrics(style, metrics.as_ref());
        let width_base = available_width.is_finite().then_some(available_width);
        let height_base = available_height.filter(|h| h.is_finite());
        let percentage_size = match (width_base, height_base) {
            (None, None) => None,
            (w, h) => Some(Size::new(w.unwrap_or(f32::NAN), h.unwrap_or(f32::NAN))),
        };
        let size = compute_replaced_size(style, replaced_box, percentage_size, self.viewport_size);
        let percentage_base = if available_width.is_finite() {
            available_width
        } else {
            0.0
        };
        let padding_left = resolve_length_for_width(
            style.padding_left,
            percentage_base,
            style.font_size,
            style.root_font_size,
            self.viewport_size,
        );
        let padding_right = resolve_length_for_width(
            style.padding_right,
            percentage_base,
            style.font_size,
            style.root_font_size,
            self.viewport_size,
        );
        let padding_top = resolve_length_for_width(
            style.padding_top,
            percentage_base,
            style.font_size,
            style.root_font_size,
            self.viewport_size,
        );
        let padding_bottom = resolve_length_for_width(
            style.padding_bottom,
            percentage_base,
            style.font_size,
            style.root_font_size,
            self.viewport_size,
        );

        let border_left = resolve_length_for_width(
            style.border_left_width,
            percentage_base,
            style.font_size,
            style.root_font_size,
            self.viewport_size,
        );
        let border_right = resolve_length_for_width(
            style.border_right_width,
            percentage_base,
            style.font_size,
            style.root_font_size,
            self.viewport_size,
        );
        let border_top = resolve_length_for_width(
            style.border_top_width,
            percentage_base,
            style.font_size,
            style.root_font_size,
            self.viewport_size,
        );
        let border_bottom = resolve_length_for_width(
            style.border_bottom_width,
            percentage_base,
            style.font_size,
            style.root_font_size,
            self.viewport_size,
        );

        let box_width = size.width + padding_left + padding_right + border_left + border_right;
        let box_height = size.height + padding_top + padding_bottom + border_top + border_bottom;
        let percentage_base = if available_width.is_finite() {
            available_width
        } else {
            0.0
        };
        let margin_left = style
            .margin_left
            .as_ref()
            .map(|l| {
                resolve_length_for_width(
                    *l,
                    percentage_base,
                    style.font_size,
                    style.root_font_size,
                    self.viewport_size,
                )
            })
            .unwrap_or(0.0);
        let margin_right = style
            .margin_right
            .as_ref()
            .map(|l| {
                resolve_length_for_width(
                    *l,
                    percentage_base,
                    style.font_size,
                    style.root_font_size,
                    self.viewport_size,
                )
            })
            .unwrap_or(0.0);

        let va = self.convert_vertical_align(style.vertical_align, style.font_size, line_height);

        Ok(ReplacedItem::new(
            Size::new(box_width, box_height),
            replaced_box.replaced_type.clone(),
            box_node.style.clone(),
            margin_left,
            margin_right,
        )
        .with_vertical_align(va))
    }

    /// Builds lines from inline items
    fn build_lines(
        &self,
        items: Vec<InlineItem>,
        first_line_width: f32,
        subsequent_line_width: f32,
        strut_metrics: &BaselineMetrics,
        base_level: Option<unicode_bidi::Level>,
    ) -> Vec<Line> {
        let mut builder = LineBuilder::new(
            first_line_width,
            subsequent_line_width,
            *strut_metrics,
            self.pipeline.clone(),
            self.font_context.clone(),
            base_level,
        );

        for item in items {
            // Check for mandatory breaks in text
            if let InlineItem::Text(ref text_item) = item {
                let has_mandatory = text_item.break_opportunities.iter().any(|b| {
                    if b.break_type != BreakType::Mandatory {
                        return false;
                    }
                    b.byte_offset < text_item.text.len() || text_item.forced_break_offsets.contains(&b.byte_offset)
                });

                if has_mandatory {
                    // Split at mandatory breaks
                    self.add_text_with_mandatory_breaks(&mut builder, text_item.clone());
                    continue;
                }
            }

            builder.add_item(item);
        }

        builder.finish()
    }

    fn resolve_scaled_metrics(&self, style: &ComputedStyle) -> Option<ScaledMetrics> {
        let italic = matches!(style.font_style, FontStyle::Italic);
        let oblique = matches!(style.font_style, FontStyle::Oblique(_));
        let stretch = crate::text::font_db::FontStretch::from_percentage(style.font_stretch.to_percentage());

        self.font_context
            .get_font_full(
                &style.font_family,
                style.font_weight.to_u16(),
                if italic {
                    crate::text::font_db::FontStyle::Italic
                } else if oblique {
                    crate::text::font_db::FontStyle::Oblique
                } else {
                    crate::text::font_db::FontStyle::Normal
                },
                stretch,
            )
            .or_else(|| self.font_context.get_sans_serif())
            .and_then(|font| font.metrics().ok())
            .map(|metrics| metrics.scale(style.font_size))
    }

    fn compute_strut_metrics(&self, style: &ComputedStyle) -> BaselineMetrics {
        let scaled = self.resolve_scaled_metrics(style);
        let line_height = compute_line_height_with_metrics(style, scaled.as_ref());
        if let Some(scaled) = scaled {
            return BaselineMetrics {
                baseline_offset: scaled.ascent,
                height: line_height,
                ascent: scaled.ascent,
                descent: scaled.descent,
                line_gap: scaled.line_gap,
                line_height,
            };
        }

        let font_size = style.font_size;
        BaselineMetrics::new(font_size * 0.8, line_height, font_size * 0.8, font_size * 0.2)
    }

    /// Adds text item handling mandatory breaks
    fn add_text_with_mandatory_breaks(&self, builder: &mut LineBuilder, text_item: TextItem) {
        let mut remaining = text_item;

        loop {
            // Find next mandatory break
            let mandatory_pos = remaining
                .break_opportunities
                .iter()
                .find(|b| b.break_type == BreakType::Mandatory)
                .map(|b| b.byte_offset);

            if let Some(pos) = mandatory_pos {
                if pos >= remaining.text.len() {
                    if remaining.advance > 0.0 {
                        builder.add_item(InlineItem::Text(remaining));
                    }
                    builder.force_break();
                    break;
                }

                if pos > 0 {
                    if let Some((before, after)) = remaining.split_at(pos, false, &self.pipeline, &self.font_context) {
                        if before.advance > 0.0 {
                            builder.add_item(InlineItem::Text(before));
                        }
                        builder.force_break();
                        remaining = after;
                        continue;
                    }
                    if remaining.advance > 0.0 {
                        builder.add_item(InlineItem::Text(remaining));
                    }
                    builder.force_break();
                    break;
                }
                builder.force_break();
                // Skip past the newline character
                if pos < remaining.text.len() {
                    let skip_bytes = remaining.text[pos..].chars().next().map(|c| c.len_utf8()).unwrap_or(1);
                    if pos + skip_bytes < remaining.text.len() {
                        if let Some((_, after)) =
                            remaining.split_at(pos + skip_bytes, false, &self.pipeline, &self.font_context)
                        {
                            remaining = after;
                            continue;
                        }
                    }
                }
                break;
            } else {
                // No more mandatory breaks
                if remaining.advance > 0.0 {
                    builder.add_item(InlineItem::Text(remaining));
                }
                break;
            }
        }
    }

    /// Creates fragments from lines
    fn create_fragments(
        &self,
        lines: Vec<Line>,
        start_y: f32,
        first_line_width: f32,
        subsequent_line_width: f32,
        first_line_box_width: f32,
        subsequent_line_box_width: f32,
        text_align: TextAlign,
        text_align_last: crate::style::types::TextAlignLast,
        text_justify: TextJustify,
        first_line_indent: f32,
        subsequent_line_indent: f32,
    ) -> Vec<FragmentNode> {
        let mut fragments = Vec::new();
        let mut y = start_y;

        let paragraph_info = compute_paragraph_line_flags(&lines);

        for (idx, line) in lines.into_iter().enumerate() {
            let line_direction = line.resolved_direction;
            let (is_last_line, is_single_line) = paragraph_info[idx];
            let line_width = if idx == 0 {
                first_line_width
            } else {
                subsequent_line_width
            };
            let line_box_width = if idx == 0 {
                first_line_box_width
            } else {
                subsequent_line_box_width
            };
            let indent_raw = if idx == 0 {
                first_line_indent
            } else {
                subsequent_line_indent
            };
            let indent_offset = if matches!(line_direction, crate::style::types::Direction::Rtl) {
                -indent_raw
            } else {
                indent_raw
            };
            let mut base_align = map_text_align(text_align, line_direction);
            let resolved_justify = resolve_auto_text_justify(text_justify, &line.items);
            if matches!(base_align, TextAlign::Justify) && matches!(resolved_justify, TextJustify::None) {
                base_align = map_text_align(TextAlign::Start, line_direction);
            }
            let mut effective_align = resolve_text_align_for_line(
                base_align,
                text_align_last,
                line_direction,
                is_last_line,
                is_single_line,
            );
            let justify_probe_items =
                if matches!(effective_align, TextAlign::Justify) && !matches!(resolved_justify, TextJustify::None) {
                    self.expand_items_for_justification(&line.items, resolved_justify)
                } else {
                    line.items.clone()
                };
            let has_justify = has_justify_opportunities(&justify_probe_items, resolved_justify);
            if matches!(effective_align, TextAlign::Justify) && matches!(resolved_justify, TextJustify::None) {
                effective_align = map_text_align(TextAlign::Start, line_direction);
            } else if matches!(effective_align, TextAlign::Justify) && !has_justify {
                effective_align = map_text_align(TextAlign::Start, line_direction);
            }
            let line_fragment = self.create_line_fragment(
                &line,
                y,
                line_width,
                line_box_width,
                effective_align,
                line_direction,
                resolved_justify,
                indent_offset,
            );
            y += line.height;
            fragments.push(line_fragment);
        }

        fragments
    }

    /// Creates a line fragment with positioned children
    fn create_line_fragment(
        &self,
        line: &Line,
        y: f32,
        available_width: f32,
        line_box_width: f32,
        text_align: TextAlign,
        direction: crate::style::types::Direction,
        text_justify: TextJustify,
        indent_offset: f32,
    ) -> FragmentNode {
        let text_align = map_text_align(text_align, direction);
        let should_justify = matches!(text_align, TextAlign::Justify) && !matches!(text_justify, TextJustify::None);
        let items: Vec<PositionedItem> = if should_justify {
            self.expand_items_for_justification(&line.items, text_justify)
        } else {
            line.items.clone()
        };
        let mut children = Vec::new();
        let usable_width = if available_width.is_finite() {
            available_width.max(0.0)
        } else {
            line.width
        };
        let total_width: f32 = items.iter().map(|p| p.item.width()).sum();
        let extra_space = (usable_width - total_width).max(0.0);
        let (lead, gap_extra) = match text_align {
            TextAlign::Right => (extra_space, 0.0),
            TextAlign::Center => (extra_space * 0.5, 0.0),
            TextAlign::Justify if should_justify && items.len() > 1 => {
                let gap_count = Self::count_justifiable_gaps(&items, text_justify);
                if gap_count > 0 {
                    (0.0, extra_space / gap_count as f32)
                } else {
                    (0.0, 0.0)
                }
            }
            _ => (0.0, 0.0),
        };

        let rtl = matches!(direction, crate::style::types::Direction::Rtl);
        let mut cursor = if rtl {
            indent_offset + lead + total_width
        } else {
            indent_offset + lead
        };

        for (i, positioned) in items.iter().enumerate() {
            let item_width = positioned.item.width();
            let item_x = if rtl { cursor - item_width } else { cursor };
            let item_y =
                line.baseline + positioned.baseline_offset - positioned.item.baseline_metrics().baseline_offset;

            let fragment = self.create_item_fragment(&positioned.item, item_x, item_y);
            children.push(fragment);

            if rtl {
                cursor -= item_width;
                if should_justify && gap_extra > 0.0 && Self::is_justifiable_gap(&items, i, text_justify) {
                    cursor -= gap_extra;
                }
            } else {
                cursor += item_width;
                if should_justify && gap_extra > 0.0 && Self::is_justifiable_gap(&items, i, text_justify) {
                    cursor += gap_extra;
                }
            }
        }

        let bounds = Rect::from_xywh(0.0, y, line_box_width, line.height);
        FragmentNode::new_line(bounds, line.baseline, children)
    }

    fn is_justifiable_boundary(prev: &InlineItem, next: &InlineItem) -> bool {
        let prev_last = last_char_of_item(prev);
        let next_first = first_char_of_item(next);
        matches!((prev_last, next_first),
            (Some(p), Some(n)) if is_expandable_space(p) && !is_expandable_space(n))
    }

    fn count_justifiable_gaps(items: &[PositionedItem], mode: TextJustify) -> usize {
        if matches!(mode, TextJustify::InterCharacter | TextJustify::Distribute) {
            return items.len().saturating_sub(1);
        }
        items
            .windows(2)
            .filter(|pair| {
                let prev = &pair[0].item;
                let next = &pair[1].item;
                Self::is_justifiable_boundary(prev, next)
            })
            .count()
    }

    fn is_justifiable_gap(items: &[PositionedItem], index: usize, mode: TextJustify) -> bool {
        if index + 1 >= items.len() {
            return false;
        }
        if matches!(mode, TextJustify::InterCharacter | TextJustify::Distribute) {
            return true;
        }
        let prev = &items[index].item;
        let next = &items[index + 1].item;
        Self::is_justifiable_boundary(prev, next)
    }

    fn expand_items_for_justification(&self, items: &[PositionedItem], mode: TextJustify) -> Vec<PositionedItem> {
        let mut expanded = Vec::new();
        for positioned in items {
            match &positioned.item {
                InlineItem::Text(text) => {
                    let segments = self.split_text_item_for_justify(text, mode);
                    let mut local_x = positioned.x;
                    for segment in segments {
                        let width = segment.advance;
                        expanded.push(PositionedItem {
                            item: InlineItem::Text(segment),
                            x: local_x,
                            baseline_offset: positioned.baseline_offset,
                        });
                        local_x += width;
                    }
                }
                _ => expanded.push(positioned.clone()),
            }
        }
        expanded
    }

    fn split_text_item_for_justify(&self, item: &TextItem, mode: TextJustify) -> Vec<TextItem> {
        let mut segments = Vec::new();
        let mut remaining = item.clone();
        let mut consumed = 0usize;
        let mut break_offsets: Vec<usize> = match mode {
            TextJustify::InterCharacter | TextJustify::Distribute => {
                let mut offsets: Vec<usize> = item.cluster_byte_offsets().skip(1).collect();
                if offsets.is_empty() {
                    // Fallback to per-character splitting when shaping did not emit cluster boundaries.
                    let mut acc = 0usize;
                    offsets = item
                        .text
                        .chars()
                        .skip(1)
                        .map(|c| {
                            acc += c.len_utf8();
                            acc
                        })
                        .collect();
                }
                offsets
            }
            _ => item
                .break_opportunities
                .iter()
                .filter(|b| matches!(b.break_type, crate::text::line_break::BreakType::Allowed))
                .filter_map(|b| {
                    let off = b.byte_offset;
                    if off == 0 || off >= item.text.len() {
                        return None;
                    }
                    let prev = item.text[..off].chars().rev().next();
                    let next = item.text[off..].chars().next();
                    let is_space_boundary = prev.map(is_expandable_space).unwrap_or(false)
                        && next.map(|c| !is_expandable_space(c)).unwrap_or(false);
                    let is_cjk_boundary =
                        prev.map(is_cjk_character).unwrap_or(false) || next.map(is_cjk_character).unwrap_or(false);
                    if is_space_boundary || is_cjk_boundary {
                        Some(off)
                    } else {
                        None
                    }
                })
                .collect(),
        };

        if break_offsets.is_empty() && matches!(mode, TextJustify::InterWord) {
            // Fallback: split on explicit spaces when break opportunities didn't flag them.
            let mut prev_is_space = false;
            for (idx, ch) in item.text.char_indices() {
                if idx == 0 {
                    prev_is_space = is_expandable_space(ch);
                    continue;
                }
                let is_space = is_expandable_space(ch);
                if prev_is_space && !is_space {
                    break_offsets.push(idx);
                }
                prev_is_space = is_space;
            }
        }

        for target in break_offsets {
            if target <= consumed {
                continue;
            }
            let local = target - consumed;
            if let Some((before, after)) = remaining.split_at(local, false, &self.pipeline, &self.font_context) {
                consumed = target;
                segments.push(before);
                remaining = after;
            } else {
                break;
            }
        }

        segments.push(remaining);
        segments
    }

    /// Creates a fragment for an inline item
    #[allow(clippy::only_used_in_recursion)]
    fn create_item_fragment(&self, item: &InlineItem, x: f32, y: f32) -> FragmentNode {
        match item {
            InlineItem::Text(text_item) => {
                let paint_offset = text_item.paint_offset;
                let width = text_item.advance + paint_offset.abs();
                let bounds = Rect::from_xywh(x + paint_offset, y, width, text_item.metrics.height);
                FragmentNode::new_text_shaped(
                    bounds,
                    text_item.text.clone(),
                    text_item.metrics.baseline_offset,
                    text_item.runs.clone(),
                    text_item.style.clone(),
                )
            }
            InlineItem::Tab(tab_item) => {
                let metrics = tab_item.metrics();
                let bounds = Rect::from_xywh(x, y, tab_item.width(), metrics.height);
                FragmentNode::new_text_shaped(
                    bounds,
                    String::new(),
                    metrics.baseline_offset,
                    Vec::new(),
                    tab_item.style().clone(),
                )
            }
            InlineItem::InlineBox(box_item) => {
                // Recursively create children with horizontal and vertical offsets
                let mut child_x = x + box_item.start_edge;
                let child_y = y + box_item.content_offset_y;
                let children: Vec<_> = box_item
                    .children
                    .iter()
                    .map(|child| {
                        let fragment = self.create_item_fragment(child, child_x, child_y);
                        child_x += child.width();
                        fragment
                    })
                    .collect();

                let bounds = Rect::from_xywh(x, y, box_item.width(), box_item.metrics.height);
                FragmentNode::new_inline_styled(bounds, box_item.box_index, children, box_item.style.clone())
            }
            InlineItem::InlineBlock(block_item) => {
                let mut fragment = block_item.fragment.clone();
                fragment.bounds = Rect::from_xywh(x + block_item.margin_left, y, block_item.width, block_item.height);
                fragment
            }
            InlineItem::Replaced(replaced_item) => {
                let paint_offset = replaced_item.paint_offset;
                let bounds = Rect::from_xywh(
                    x + replaced_item.margin_left + paint_offset,
                    y,
                    replaced_item.width,
                    replaced_item.height,
                );
                FragmentNode::new_with_style(
                    bounds,
                    FragmentContent::Replaced {
                        replaced_type: replaced_item.replaced_type.clone(),
                        box_id: None,
                    },
                    vec![],
                    replaced_item.style.clone(),
                )
            }
        }
    }

    /// Calculates intrinsic width for inline content
    fn calculate_intrinsic_width(&self, box_node: &BoxNode, mode: IntrinsicSizingMode) -> f32 {
        let items = match self.collect_inline_items(box_node, f32::INFINITY, None) {
            Ok(items) => items,
            Err(_) => return 0.0,
        };
        let style = &box_node.style;
        let indent_value = resolve_length_with_percentage(
            style.text_indent.length,
            None,
            self.viewport_size,
            style.font_size,
            style.root_font_size,
        )
        .unwrap_or(0.0);
        let indent_positive = indent_value.max(0.0);
        let indent_applies_first = style.text_indent.each_line || !style.text_indent.hanging;
        let indent_applies_subsequent = style.text_indent.each_line || style.text_indent.hanging;

        let mut width = match mode {
            IntrinsicSizingMode::MinContent => self.min_content_width(&items),
            IntrinsicSizingMode::MaxContent => self.max_content_width(&items),
        };

        if indent_positive > 0.0 && (indent_applies_first || indent_applies_subsequent) {
            width += indent_positive;
        }
        width
    }

    fn min_content_width(&self, items: &[InlineItem]) -> f32 {
        let mut tracker = SegmentTracker::new();
        self.accumulate_min_segments(items, &mut tracker);
        tracker.finish()
    }

    fn max_content_width(&self, items: &[InlineItem]) -> f32 {
        let mut tracker = SegmentTracker::new();
        self.accumulate_max_segments(items, &mut tracker);
        tracker.finish()
    }

    fn accumulate_min_segments(&self, items: &[InlineItem], tracker: &mut dyn SegmentConsumer) {
        for (idx, item) in items.iter().enumerate() {
            let next_item = items.get(idx + 1);
            match item {
                InlineItem::Text(text) => {
                    let next_char = next_item.and_then(first_char_of_item);
                    self.measure_text_min_content(text, tracker, next_char);
                }
                InlineItem::Tab(tab) => {
                    if tab.allow_wrap() {
                        tracker.break_segment();
                    } else {
                        tracker.add_width(tab.width());
                    }
                }
                InlineItem::InlineBox(inline_box) => {
                    let mut boxed = InlineBoxSegment::new(tracker, inline_box.start_edge, inline_box.end_edge);
                    self.accumulate_min_segments(&inline_box.children, &mut boxed);
                    boxed.finish();
                }
                InlineItem::InlineBlock(block) => {
                    tracker.break_segment();
                    tracker.add_width(block.total_width());
                    tracker.break_segment();
                }
                InlineItem::Replaced(replaced) => {
                    tracker.break_segment();
                    tracker.add_width(replaced.total_width());
                    tracker.break_segment();
                }
            }
        }
    }

    fn accumulate_max_segments(&self, items: &[InlineItem], tracker: &mut dyn SegmentConsumer) {
        for item in items {
            match item {
                InlineItem::Text(text) => {
                    self.measure_text_max_content(text, tracker);
                }
                InlineItem::Tab(tab) => {
                    tracker.add_width(tab.width());
                }
                InlineItem::InlineBox(inline_box) => {
                    let mut boxed = InlineBoxSegment::new(tracker, inline_box.start_edge, inline_box.end_edge);
                    self.accumulate_max_segments(&inline_box.children, &mut boxed);
                    boxed.finish();
                }
                InlineItem::InlineBlock(block) => {
                    tracker.add_width(block.total_width());
                }
                InlineItem::Replaced(replaced) => {
                    tracker.add_width(replaced.total_width());
                }
            }
        }
    }

    fn measure_text_min_content(
        &self,
        text_item: &TextItem,
        tracker: &mut dyn SegmentConsumer,
        next_char: Option<char>,
    ) {
        if text_item.text.is_empty() {
            return;
        }

        let len = text_item.text.len();
        let mut last_break = 0;
        let mut hyphen_width: Option<f32> = None;
        let last_char = text_item.text.chars().last();

        for brk in &text_item.break_opportunities {
            if brk.byte_offset > len {
                continue;
            }

            let is_forced = text_item
                .forced_break_offsets
                .iter()
                .any(|offset| *offset == brk.byte_offset);
            if brk.byte_offset == len
                && brk.break_type == BreakType::Allowed
                && !brk.adds_hyphen
                && !is_forced
                && !allows_boundary_break(last_char, next_char)
            {
                continue;
            }
            if brk.byte_offset == len
                && brk.break_type == BreakType::Mandatory
                && !brk.adds_hyphen
                && !is_forced
                && !allows_boundary_break(last_char, next_char)
            {
                continue;
            }

            let mut segment_width =
                (text_item.advance_at_offset(brk.byte_offset) - text_item.advance_at_offset(last_break)).max(0.0);

            if brk.adds_hyphen {
                let h = hyphen_width.get_or_insert_with(|| self.hyphen_advance(&text_item.style));
                segment_width += *h;
            }

            tracker.add_width(segment_width);
            tracker.break_segment();
            last_break = brk.byte_offset;
        }

        if last_break < len {
            let trailing = (text_item.advance_at_offset(len) - text_item.advance_at_offset(last_break)).max(0.0);
            tracker.add_width(trailing);
        }
    }

    fn measure_text_max_content(&self, text_item: &TextItem, tracker: &mut dyn SegmentConsumer) {
        if text_item.text.is_empty() {
            return;
        }

        let len = text_item.text.len();
        let mut last_break = 0;

        for brk in &text_item.break_opportunities {
            if brk.byte_offset > len {
                continue;
            }
            if brk.break_type == BreakType::Mandatory {
                let segment_width =
                    (text_item.advance_at_offset(brk.byte_offset) - text_item.advance_at_offset(last_break)).max(0.0);
                tracker.add_width(segment_width);
                tracker.break_segment();
                last_break = brk.byte_offset;
            }
        }

        if last_break < len {
            let trailing = (text_item.advance_at_offset(len) - text_item.advance_at_offset(last_break)).max(0.0);
            tracker.add_width(trailing);
        }
    }
}

fn resolve_length_for_width(
    length: Length,
    percentage_base: f32,
    font_size: f32,
    root_font_size: f32,
    viewport: crate::geometry::Size,
) -> f32 {
    if length.unit.is_percentage() {
        length.resolve_against(percentage_base)
    } else if length.unit.is_absolute() {
        length.to_px()
    } else if length.unit.is_viewport_relative() {
        length.resolve_with_viewport(viewport.width, viewport.height)
    } else {
        match length.unit {
            LengthUnit::Em => length.value * font_size,
            LengthUnit::Ex => length.value * font_size * 0.5,
            LengthUnit::Ch => length.value * font_size * 0.5,
            LengthUnit::Rem => length.value * root_font_size,
            _ => length.value,
        }
    }
}

fn horizontal_padding_and_borders(style: &ComputedStyle, percentage_base: f32, viewport: crate::geometry::Size) -> f32 {
    resolve_length_for_width(
        style.padding_left,
        percentage_base,
        style.font_size,
        style.root_font_size,
        viewport,
    ) + resolve_length_for_width(
        style.padding_right,
        percentage_base,
        style.font_size,
        style.root_font_size,
        viewport,
    ) + resolve_length_for_width(
        style.border_left_width,
        percentage_base,
        style.font_size,
        style.root_font_size,
        viewport,
    ) + resolve_length_for_width(
        style.border_right_width,
        percentage_base,
        style.font_size,
        style.root_font_size,
        viewport,
    )
}

fn compute_inline_box_metrics(
    children: &[InlineItem],
    content_offset_y: f32,
    bottom_inset: f32,
    fallback: BaselineMetrics,
) -> BaselineMetrics {
    if children.is_empty() {
        let mut metrics = fallback;
        metrics.baseline_offset += content_offset_y;
        metrics.ascent += content_offset_y;
        metrics.descent += bottom_inset;
        metrics.height += content_offset_y + bottom_inset;
        metrics.line_height = metrics.height;
        return metrics;
    }

    let mut content_height: f32 = 0.0;
    for child in children {
        content_height = content_height.max(child.baseline_metrics().height);
    }

    let baseline_child = children
        .iter()
        .find(|c| c.vertical_align().is_baseline_relative())
        .unwrap_or(&children[0]);
    let child_metrics = baseline_child.baseline_metrics();

    let baseline_offset = content_offset_y + child_metrics.baseline_offset;
    let ascent = content_offset_y + child_metrics.ascent;
    let height = content_offset_y + content_height + bottom_inset;
    let descent = height - baseline_offset;

    BaselineMetrics {
        baseline_offset,
        height,
        ascent,
        descent,
        line_gap: child_metrics.line_gap,
        line_height: height,
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
struct NormalizedText {
    text: String,
    forced_breaks: Vec<crate::text::line_break::BreakOpportunity>,
    allow_soft_wrap: bool,
    leading_collapsible: bool,
    trailing_collapsible: bool,
}

#[derive(Debug, Clone)]
struct PendingSpace {
    style: Arc<ComputedStyle>,
    allow_soft_wrap: bool,
}

impl PendingSpace {
    fn new(style: Arc<ComputedStyle>, allow_soft_wrap: bool) -> Self {
        Self { style, allow_soft_wrap }
    }
}

fn normalize_text_for_white_space(text: &str, white_space: WhiteSpace) -> NormalizedText {
    use crate::text::line_break::BreakOpportunity;

    let result = match white_space {
        WhiteSpace::Normal | WhiteSpace::Nowrap => {
            let mut out = String::with_capacity(text.len());
            let mut in_whitespace = false;
            let mut seen_content = false;
            let mut leading_collapsible = false;
            let mut iter = text.chars().peekable();

            while let Some(ch) = iter.next() {
                match ch {
                    '\r' => {
                        if matches!(iter.peek(), Some('\n')) {
                            iter.next();
                        }
                        in_whitespace = true;
                        if !seen_content {
                            leading_collapsible = true;
                        }
                    }
                    '\n' | '\u{000B}' | '\u{000C}' | ' ' | '\t' => {
                        in_whitespace = true;
                        if !seen_content {
                            leading_collapsible = true;
                        }
                    }
                    _ => {
                        if in_whitespace && seen_content {
                            out.push(' ');
                        }
                        in_whitespace = false;
                        seen_content = true;
                        out.push(ch);
                    }
                }
            }

            let trailing_collapsible = in_whitespace && seen_content;

            NormalizedText {
                text: out,
                forced_breaks: Vec::new(),
                allow_soft_wrap: white_space != WhiteSpace::Nowrap,
                leading_collapsible,
                trailing_collapsible,
            }
        }
        WhiteSpace::PreLine => {
            let mut out = String::with_capacity(text.len());
            let mut mandatory_breaks = Vec::new();
            let mut run_has_space = false;
            let mut run_has_newline = false;
            let mut seen_content = false;
            let mut iter = text.chars().peekable();
            let mut leading_collapsible = false;

            while let Some(ch) = iter.next() {
                match ch {
                    ' ' | '\t' => {
                        run_has_space = true;
                        if !seen_content && !run_has_newline {
                            leading_collapsible = true;
                        }
                    }
                    '\r' => {
                        if matches!(iter.peek(), Some('\n')) {
                            iter.next();
                        }
                        run_has_newline = true;
                    }
                    '\n' | '\u{000B}' | '\u{000C}' => {
                        run_has_newline = true;
                    }
                    _ => {
                        if run_has_newline {
                            if run_has_space && seen_content {
                                out.push(' ');
                            }
                            mandatory_breaks.push(BreakOpportunity::mandatory(out.len()));
                        } else if run_has_space && seen_content {
                            out.push(' ');
                        }
                        run_has_space = false;
                        run_has_newline = false;

                        out.push(ch);
                        seen_content = true;
                    }
                }
            }

            if run_has_newline {
                if run_has_space && seen_content {
                    out.push(' ');
                }
                mandatory_breaks.push(BreakOpportunity::mandatory(out.len()));
            } else if run_has_space && seen_content {
                out.push(' ');
            }

            NormalizedText {
                text: out,
                forced_breaks: mandatory_breaks,
                allow_soft_wrap: true,
                leading_collapsible,
                trailing_collapsible: false,
            }
        }
        WhiteSpace::Pre | WhiteSpace::PreWrap => {
            let mut out = String::with_capacity(text.len());
            let mut mandatory_breaks = Vec::new();
            let mut iter = text.chars().peekable();

            while let Some(ch) = iter.next() {
                match ch {
                    '\r' => {
                        if matches!(iter.peek(), Some('\n')) {
                            iter.next();
                        }
                        mandatory_breaks.push(BreakOpportunity::mandatory(out.len()));
                    }
                    '\n' | '\u{000B}' | '\u{000C}' => {
                        mandatory_breaks.push(BreakOpportunity::mandatory(out.len()));
                    }
                    '\t' => out.push('\t'),
                    _ => out.push(ch),
                }
            }

            NormalizedText {
                text: out,
                forced_breaks: mandatory_breaks,
                allow_soft_wrap: white_space == WhiteSpace::PreWrap,
                leading_collapsible: false,
                trailing_collapsible: false,
            }
        }
        WhiteSpace::BreakSpaces => {
            let mut out = String::with_capacity(text.len());
            let mut mandatory_breaks = Vec::new();
            let mut iter = text.chars().peekable();

            while let Some(ch) = iter.next() {
                match ch {
                    '\r' => {
                        if matches!(iter.peek(), Some('\n')) {
                            iter.next();
                        }
                        mandatory_breaks.push(BreakOpportunity::mandatory(out.len()));
                    }
                    '\n' | '\u{000B}' | '\u{000C}' => {
                        mandatory_breaks.push(BreakOpportunity::mandatory(out.len()));
                    }
                    _ => out.push(ch),
                }
            }

            // In break-spaces every preserved space creates a break opportunity; we leave that
            // to the line breaker by keeping all spaces and allowing soft wrap.
            NormalizedText {
                text: out,
                forced_breaks: mandatory_breaks,
                allow_soft_wrap: true,
                leading_collapsible: false,
                trailing_collapsible: false,
            }
        }
    };

    result
}

fn slice_breaks(
    breaks: &[crate::text::line_break::BreakOpportunity],
    start: usize,
    end: usize,
) -> Vec<crate::text::line_break::BreakOpportunity> {
    breaks
        .iter()
        .filter(|b| b.byte_offset >= start && b.byte_offset <= end)
        .map(|b| {
            let mut adjusted = *b;
            adjusted.byte_offset -= start;
            adjusted
        })
        .collect()
}

fn merge_breaks(
    mut base: Vec<crate::text::line_break::BreakOpportunity>,
    forced: Vec<crate::text::line_break::BreakOpportunity>,
    allow_soft_wrap: bool,
) -> Vec<crate::text::line_break::BreakOpportunity> {
    use crate::text::line_break::BreakType;

    if !allow_soft_wrap {
        base.retain(|b| b.break_type == BreakType::Mandatory);
    }

    base.extend(forced);
    base.sort_by(|a, b| {
        a.byte_offset
            .cmp(&b.byte_offset)
            .then_with(|| match (a.break_type, b.break_type) {
                (BreakType::Mandatory, BreakType::Allowed) => std::cmp::Ordering::Less,
                (BreakType::Allowed, BreakType::Mandatory) => std::cmp::Ordering::Greater,
                _ => std::cmp::Ordering::Equal,
            })
    });

    base.dedup_by(|a, b| {
        if a.byte_offset != b.byte_offset {
            return false;
        }
        if let BreakType::Mandatory = a.break_type {
            *b = *a;
        } else {
            b.adds_hyphen |= a.adds_hyphen;
        }
        true
    });

    base
}

fn apply_text_transform(text: &str, transform: TextTransform, white_space: WhiteSpace) -> String {
    match transform {
        TextTransform::None => text.to_string(),
        TextTransform::Uppercase => {
            let mut out = String::with_capacity(text.len());
            for ch in text.chars() {
                for up in ch.to_uppercase() {
                    out.push(up);
                }
            }
            out
        }
        TextTransform::Lowercase => {
            let mut out = String::with_capacity(text.len());
            for ch in text.chars() {
                for low in ch.to_lowercase() {
                    out.push(low);
                }
            }
            out
        }
        TextTransform::Capitalize => {
            let mut out = String::with_capacity(text.len());
            let mut start_of_word = true;
            for ch in text.chars() {
                if ch.is_whitespace() {
                    start_of_word = true;
                    out.push(ch);
                    continue;
                }
                if start_of_word {
                    for up in ch.to_uppercase() {
                        out.push(up);
                    }
                    start_of_word = false;
                } else {
                    out.push(ch);
                }
            }
            out
        }
        TextTransform::FullWidth => {
            let preserve_spaces = matches!(white_space, WhiteSpace::Pre | WhiteSpace::PreWrap | WhiteSpace::BreakSpaces);
            let mut out = String::with_capacity(text.len());
            for ch in text.chars() {
                if ch == ' ' && !preserve_spaces {
                    out.push(ch);
                    continue;
                }
                if let Some(mapped) = map_to_full_width(ch) {
                    out.push(mapped);
                } else {
                    out.push(ch);
                }
            }
            out
        }
    }
}

fn map_to_full_width(ch: char) -> Option<char> {
    const FULL_WIDTH_TABLE: &[(u32, u32)] = &[
        (0x0020, 0x3000),
        (0x0021, 0xFF01),
        (0x0022, 0xFF02),
        (0x0023, 0xFF03),
        (0x0024, 0xFF04),
        (0x0025, 0xFF05),
        (0x0026, 0xFF06),
        (0x0027, 0xFF07),
        (0x0028, 0xFF08),
        (0x0029, 0xFF09),
        (0x002A, 0xFF0A),
        (0x002B, 0xFF0B),
        (0x002C, 0xFF0C),
        (0x002D, 0xFF0D),
        (0x002E, 0xFF0E),
        (0x002F, 0xFF0F),
        (0x0030, 0xFF10),
        (0x0031, 0xFF11),
        (0x0032, 0xFF12),
        (0x0033, 0xFF13),
        (0x0034, 0xFF14),
        (0x0035, 0xFF15),
        (0x0036, 0xFF16),
        (0x0037, 0xFF17),
        (0x0038, 0xFF18),
        (0x0039, 0xFF19),
        (0x003A, 0xFF1A),
        (0x003B, 0xFF1B),
        (0x003C, 0xFF1C),
        (0x003D, 0xFF1D),
        (0x003E, 0xFF1E),
        (0x003F, 0xFF1F),
        (0x0040, 0xFF20),
        (0x0041, 0xFF21),
        (0x0042, 0xFF22),
        (0x0043, 0xFF23),
        (0x0044, 0xFF24),
        (0x0045, 0xFF25),
        (0x0046, 0xFF26),
        (0x0047, 0xFF27),
        (0x0048, 0xFF28),
        (0x0049, 0xFF29),
        (0x004A, 0xFF2A),
        (0x004B, 0xFF2B),
        (0x004C, 0xFF2C),
        (0x004D, 0xFF2D),
        (0x004E, 0xFF2E),
        (0x004F, 0xFF2F),
        (0x0050, 0xFF30),
        (0x0051, 0xFF31),
        (0x0052, 0xFF32),
        (0x0053, 0xFF33),
        (0x0054, 0xFF34),
        (0x0055, 0xFF35),
        (0x0056, 0xFF36),
        (0x0057, 0xFF37),
        (0x0058, 0xFF38),
        (0x0059, 0xFF39),
        (0x005A, 0xFF3A),
        (0x005B, 0xFF3B),
        (0x005C, 0xFF3C),
        (0x005D, 0xFF3D),
        (0x005E, 0xFF3E),
        (0x005F, 0xFF3F),
        (0x0060, 0xFF40),
        (0x0061, 0xFF41),
        (0x0062, 0xFF42),
        (0x0063, 0xFF43),
        (0x0064, 0xFF44),
        (0x0065, 0xFF45),
        (0x0066, 0xFF46),
        (0x0067, 0xFF47),
        (0x0068, 0xFF48),
        (0x0069, 0xFF49),
        (0x006A, 0xFF4A),
        (0x006B, 0xFF4B),
        (0x006C, 0xFF4C),
        (0x006D, 0xFF4D),
        (0x006E, 0xFF4E),
        (0x006F, 0xFF4F),
        (0x0070, 0xFF50),
        (0x0071, 0xFF51),
        (0x0072, 0xFF52),
        (0x0073, 0xFF53),
        (0x0074, 0xFF54),
        (0x0075, 0xFF55),
        (0x0076, 0xFF56),
        (0x0077, 0xFF57),
        (0x0078, 0xFF58),
        (0x0079, 0xFF59),
        (0x007A, 0xFF5A),
        (0x007B, 0xFF5B),
        (0x007C, 0xFF5C),
        (0x007D, 0xFF5D),
        (0x007E, 0xFF5E),
        (0x00A2, 0xFFE0),
        (0x00A3, 0xFFE1),
        (0x00A5, 0xFFE5),
        (0x00A6, 0xFFE4),
        (0x00AC, 0xFFE2),
        (0x00AF, 0xFFE3),
        (0x20A9, 0xFFE6),
        (0x2985, 0xFF5F),
        (0x2986, 0xFF60),
        (0xFF61, 0x3002),
        (0xFF62, 0x300C),
        (0xFF63, 0x300D),
        (0xFF64, 0x3001),
        (0xFF65, 0x30FB),
        (0xFF66, 0x30F2),
        (0xFF67, 0x30A1),
        (0xFF68, 0x30A3),
        (0xFF69, 0x30A5),
        (0xFF6A, 0x30A7),
        (0xFF6B, 0x30A9),
        (0xFF6C, 0x30E3),
        (0xFF6D, 0x30E5),
        (0xFF6E, 0x30E7),
        (0xFF6F, 0x30C3),
        (0xFF70, 0x30FC),
        (0xFF71, 0x30A2),
        (0xFF72, 0x30A4),
        (0xFF73, 0x30A6),
        (0xFF74, 0x30A8),
        (0xFF75, 0x30AA),
        (0xFF76, 0x30AB),
        (0xFF77, 0x30AD),
        (0xFF78, 0x30AF),
        (0xFF79, 0x30B1),
        (0xFF7A, 0x30B3),
        (0xFF7B, 0x30B5),
        (0xFF7C, 0x30B7),
        (0xFF7D, 0x30B9),
        (0xFF7E, 0x30BB),
        (0xFF7F, 0x30BD),
        (0xFF80, 0x30BF),
        (0xFF81, 0x30C1),
        (0xFF82, 0x30C4),
        (0xFF83, 0x30C6),
        (0xFF84, 0x30C8),
        (0xFF85, 0x30CA),
        (0xFF86, 0x30CB),
        (0xFF87, 0x30CC),
        (0xFF88, 0x30CD),
        (0xFF89, 0x30CE),
        (0xFF8A, 0x30CF),
        (0xFF8B, 0x30D2),
        (0xFF8C, 0x30D5),
        (0xFF8D, 0x30D8),
        (0xFF8E, 0x30DB),
        (0xFF8F, 0x30DE),
        (0xFF90, 0x30DF),
        (0xFF91, 0x30E0),
        (0xFF92, 0x30E1),
        (0xFF93, 0x30E2),
        (0xFF94, 0x30E4),
        (0xFF95, 0x30E6),
        (0xFF96, 0x30E8),
        (0xFF97, 0x30E9),
        (0xFF98, 0x30EA),
        (0xFF99, 0x30EB),
        (0xFF9A, 0x30EC),
        (0xFF9B, 0x30ED),
        (0xFF9C, 0x30EF),
        (0xFF9D, 0x30F3),
        (0xFF9E, 0x3099),
        (0xFF9F, 0x309A),
        (0xFFA0, 0x3164),
        (0xFFA1, 0x3131),
        (0xFFA2, 0x3132),
        (0xFFA3, 0x3133),
        (0xFFA4, 0x3134),
        (0xFFA5, 0x3135),
        (0xFFA6, 0x3136),
        (0xFFA7, 0x3137),
        (0xFFA8, 0x3138),
        (0xFFA9, 0x3139),
        (0xFFAA, 0x313A),
        (0xFFAB, 0x313B),
        (0xFFAC, 0x313C),
        (0xFFAD, 0x313D),
        (0xFFAE, 0x313E),
        (0xFFAF, 0x313F),
        (0xFFB0, 0x3140),
        (0xFFB1, 0x3141),
        (0xFFB2, 0x3142),
        (0xFFB3, 0x3143),
        (0xFFB4, 0x3144),
        (0xFFB5, 0x3145),
        (0xFFB6, 0x3146),
        (0xFFB7, 0x3147),
        (0xFFB8, 0x3148),
        (0xFFB9, 0x3149),
        (0xFFBA, 0x314A),
        (0xFFBB, 0x314B),
        (0xFFBC, 0x314C),
        (0xFFBD, 0x314D),
        (0xFFBE, 0x314E),
        (0xFFC2, 0x314F),
        (0xFFC3, 0x3150),
        (0xFFC4, 0x3151),
        (0xFFC5, 0x3152),
        (0xFFC6, 0x3153),
        (0xFFC7, 0x3154),
        (0xFFCA, 0x3155),
        (0xFFCB, 0x3156),
        (0xFFCC, 0x3157),
        (0xFFCD, 0x3158),
        (0xFFCE, 0x3159),
        (0xFFCF, 0x315A),
        (0xFFD2, 0x315B),
        (0xFFD3, 0x315C),
        (0xFFD4, 0x315D),
        (0xFFD5, 0x315E),
        (0xFFD6, 0x315F),
        (0xFFD7, 0x3160),
        (0xFFDA, 0x3161),
        (0xFFDB, 0x3162),
        (0xFFDC, 0x3163),
        (0xFFE8, 0x2502),
        (0xFFE9, 0x2190),
        (0xFFEA, 0x2191),
        (0xFFEB, 0x2192),
        (0xFFEC, 0x2193),
        (0xFFED, 0x25A0),
        (0xFFEE, 0x25CB),
    ];

    let code = ch as u32;
    FULL_WIDTH_TABLE
        .binary_search_by_key(&code, |(src, _)| *src)
        .ok()
        .and_then(|idx| char::from_u32(FULL_WIDTH_TABLE[idx].1))
}

fn char_boundary_breaks(text: &str) -> Vec<crate::text::line_break::BreakOpportunity> {
    use crate::text::line_break::BreakOpportunity;
    let mut breaks = Vec::new();
    for (idx, _) in text.char_indices().skip(1) {
        breaks.push(BreakOpportunity::allowed(idx));
    }
    breaks.push(BreakOpportunity::allowed(text.len()));
    breaks
}

fn apply_break_properties(
    text: &str,
    breaks: Vec<crate::text::line_break::BreakOpportunity>,
    word_break: WordBreak,
    overflow_wrap: OverflowWrap,
    allow_soft_wrap: bool,
) -> Vec<crate::text::line_break::BreakOpportunity> {
    use crate::text::line_break::BreakType;

    if !allow_soft_wrap {
        return breaks;
    }

    let mut result = breaks;

    match word_break {
        WordBreak::BreakAll => {
            result.extend(char_boundary_breaks(text));
        }
        WordBreak::BreakWord => {}
        WordBreak::KeepAll => {
            result.retain(|brk| {
                if brk.break_type == BreakType::Mandatory {
                    return true;
                }
                let prev = text[..brk.byte_offset.min(text.len())].chars().rev().next();
                prev.map(|c| c.is_whitespace() || c == '-' || c == '\u{00AD}')
                    .unwrap_or(false)
            });
        }
        WordBreak::Normal => {}
    }

    if matches!(overflow_wrap, OverflowWrap::Anywhere) {
        result.extend(char_boundary_breaks(text));
    }

    result.sort_by_key(|b| b.byte_offset);
    result.dedup_by(|a, b| {
        if a.byte_offset != b.byte_offset {
            return false;
        }
        if let BreakType::Mandatory = a.break_type {
            *b = *a;
        }
        true
    });

    result
}

fn hyphenation_breaks(
    text: &str,
    hyphens: HyphensMode,
    hyphenator: Option<&Hyphenator>,
    allow_soft_wrap: bool,
) -> (String, Vec<crate::text::line_break::BreakOpportunity>) {
    use crate::text::line_break::BreakOpportunity;

    // Remove soft hyphens while tracking their positions in the output string
    let mut cleaned = String::with_capacity(text.len());
    let mut soft_breaks = Vec::new();
    for ch in text.chars() {
        if ch == '\u{00AD}' {
            soft_breaks.push(BreakOpportunity::with_hyphen(cleaned.len(), BreakType::Allowed, true));
            continue;
        }
        cleaned.push(ch);
    }

    if !allow_soft_wrap || matches!(hyphens, HyphensMode::None) {
        return (cleaned, Vec::new());
    }

    let mut breaks = match hyphens {
        HyphensMode::Manual => soft_breaks,
        HyphensMode::Auto => {
            let mut auto_breaks = soft_breaks;
            if let Some(hyph) = hyphenator {
                let mut idx = 0;
                let chars: Vec<(usize, char)> = cleaned.char_indices().collect();
                while idx < chars.len() {
                    if chars[idx].1.is_alphabetic() {
                        let start = chars[idx].0;
                        let mut end_idx = idx + 1;
                        while end_idx < chars.len() && chars[end_idx].1.is_alphabetic() {
                            end_idx += 1;
                        }
                        let end = if end_idx < chars.len() {
                            chars[end_idx].0
                        } else {
                            cleaned.len()
                        };
                        let word = &cleaned[start..end];
                        for rel in hyph.hyphenate(word) {
                            auto_breaks.push(BreakOpportunity::with_hyphen(start + rel, BreakType::Allowed, true));
                        }
                        idx = end_idx;
                    } else {
                        idx += 1;
                    }
                }
            }
            auto_breaks
        }
        HyphensMode::None => Vec::new(),
    };

    breaks.sort_by_key(|b| b.byte_offset);
    breaks.dedup_by(|a, b| {
        if a.byte_offset != b.byte_offset {
            return false;
        }
        b.adds_hyphen |= a.adds_hyphen;
        true
    });

    (cleaned, breaks)
}

impl FormattingContext for InlineFormattingContext {
    fn layout(&self, box_node: &BoxNode, constraints: &LayoutConstraints) -> Result<FragmentNode, LayoutError> {
        let style = &box_node.style;
        let available_width = constraints.width().unwrap_or(f32::MAX);
        let available_height = constraints.height();

        // Create strut metrics from containing block style
        let strut_metrics = self.compute_strut_metrics(style);

        // Collect inline items
        let items = self.collect_inline_items(box_node, available_width, available_height)?;

        let base_direction = if matches!(style.unicode_bidi, crate::style::types::UnicodeBidi::Plaintext) {
            determine_paragraph_direction(&items).unwrap_or(style.direction)
        } else {
            style.direction
        };

        let indent_value = resolve_length_with_percentage(
            style.text_indent.length,
            constraints.width(),
            self.viewport_size,
            style.font_size,
            style.root_font_size,
        )
        .unwrap_or(0.0);
        let indent_applies_first = style.text_indent.each_line || !style.text_indent.hanging;
        let indent_applies_subsequent = style.text_indent.each_line || style.text_indent.hanging;
        let indent_positive = indent_value.max(0.0);
        let first_line_width = if indent_applies_first {
            (available_width - indent_positive).max(0.0)
        } else {
            available_width
        };
        let subsequent_line_width = if indent_applies_subsequent {
            (available_width - indent_positive).max(0.0)
        } else {
            available_width
        };

        let base_level = match style.unicode_bidi {
            crate::style::types::UnicodeBidi::Plaintext => None,
            _ => match base_direction {
                crate::style::types::Direction::Rtl => Some(unicode_bidi::Level::rtl()),
                _ => Some(unicode_bidi::Level::ltr()),
            },
        };

        let first_line_box_width = if available_width.is_finite() {
            available_width
        } else {
            first_line_width + indent_positive
        };
        let subsequent_line_box_width = if available_width.is_finite() {
            available_width
        } else {
            subsequent_line_width + indent_positive
        };

        // Build lines
        let lines = self.build_lines(
            items,
            first_line_width,
            subsequent_line_width,
            &strut_metrics,
            base_level,
        );

        // Calculate total height
        let total_height: f32 = lines.iter().map(|l| l.height).sum();
        let max_width: f32 = if available_width.is_finite() {
            available_width
        } else {
            lines
                .iter()
                .enumerate()
                .map(|(idx, l)| {
                    let box_width = if idx == 0 {
                        first_line_box_width
                    } else {
                        subsequent_line_box_width
                    };
                    box_width.max(l.width + indent_value.abs())
                })
                .fold(0.0, f32::max)
        };

        // Create fragments
        let children = self.create_fragments(
            lines,
            0.0,
            first_line_width,
            subsequent_line_width,
            first_line_box_width,
            subsequent_line_box_width,
            style.text_align,
            style.text_align_last,
            style.text_justify,
            if indent_applies_first { indent_value } else { 0.0 },
            if indent_applies_subsequent { indent_value } else { 0.0 },
        );

        // Create containing fragment
        let bounds = Rect::from_xywh(0.0, 0.0, max_width.min(available_width), total_height);
        Ok(FragmentNode::new_block(bounds, children))
    }

    fn compute_intrinsic_inline_size(&self, box_node: &BoxNode, mode: IntrinsicSizingMode) -> Result<f32, LayoutError> {
        Ok(self.calculate_intrinsic_width(box_node, mode))
    }
}

impl Default for InlineFormattingContext {
    fn default() -> Self {
        Self::new()
    }
}

trait SegmentConsumer {
    fn add_width(&mut self, width: f32);
    fn break_segment(&mut self);
}

struct SegmentTracker {
    current: f32,
    max: f32,
}

impl SegmentTracker {
    fn new() -> Self {
        Self { current: 0.0, max: 0.0 }
    }

    fn finish(mut self) -> f32 {
        self.max = self.max.max(self.current);
        self.max
    }
}

impl SegmentConsumer for SegmentTracker {
    fn add_width(&mut self, width: f32) {
        self.current += width;
    }

    fn break_segment(&mut self) {
        self.max = self.max.max(self.current);
        self.current = 0.0;
    }
}

struct InlineBoxSegment<'a> {
    inner: &'a mut dyn SegmentConsumer,
    start_edge: f32,
    end_edge: f32,
    started: bool,
}

impl<'a> InlineBoxSegment<'a> {
    fn new(inner: &'a mut dyn SegmentConsumer, start_edge: f32, end_edge: f32) -> Self {
        Self {
            inner,
            start_edge,
            end_edge,
            started: false,
        }
    }

    fn ensure_started(&mut self) {
        if !self.started {
            self.inner.add_width(self.start_edge);
            self.started = true;
        }
    }

    fn finish(mut self) {
        self.ensure_started();
        self.inner.add_width(self.end_edge);
    }
}

impl<'a> SegmentConsumer for InlineBoxSegment<'a> {
    fn add_width(&mut self, width: f32) {
        self.ensure_started();
        self.inner.add_width(width);
    }

    fn break_segment(&mut self) {
        self.ensure_started();
        self.inner.add_width(self.end_edge);
        self.inner.break_segment();
        self.inner.add_width(self.start_edge);
    }
}

fn allows_boundary_break(prev: Option<char>, next: Option<char>) -> bool {
    const OBJECT_REPLACEMENT: char = '\u{FFFC}';
    let (Some(a), Some(b)) = (prev, next) else {
        return true;
    };
    let mut probe = String::new();
    probe.push(a);
    probe.push(b);
    find_break_opportunities(&probe)
        .iter()
        .any(|b| {
            b.byte_offset == a.len_utf8() && matches!(b.break_type, BreakType::Allowed | BreakType::Mandatory)
        })
        // Treat object replacement as breakable to avoid fusing text across replaced items.
        || matches!(a, OBJECT_REPLACEMENT) || matches!(b, OBJECT_REPLACEMENT)
}

fn first_char_of_item(item: &InlineItem) -> Option<char> {
    const OBJECT_REPLACEMENT: char = '\u{FFFC}';
    match item {
        InlineItem::Text(t) => t.text.chars().next(),
        InlineItem::Tab(_) => Some('\t'),
        InlineItem::InlineBox(b) => b.children.iter().find_map(first_char_of_item),
        InlineItem::InlineBlock(_) | InlineItem::Replaced(_) => Some(OBJECT_REPLACEMENT),
    }
}

fn last_char_of_item(item: &InlineItem) -> Option<char> {
    const OBJECT_REPLACEMENT: char = '\u{FFFC}';
    match item {
        InlineItem::Text(t) => t.text.chars().rev().next(),
        InlineItem::Tab(_) => Some('\t'),
        InlineItem::InlineBox(b) => b.children.iter().rev().find_map(last_char_of_item),
        InlineItem::InlineBlock(_) | InlineItem::Replaced(_) => Some(OBJECT_REPLACEMENT),
    }
}

fn is_expandable_space(ch: char) -> bool {
    matches!(ch, ' ' | '\t')
}

fn resolve_text_align_for_line(
    text_align: TextAlign,
    text_align_last: crate::style::types::TextAlignLast,
    direction: crate::style::types::Direction,
    is_last_line: bool,
    is_single_line: bool,
) -> TextAlign {
    if !is_last_line {
        return text_align;
    }

    let mapped_start_end = |align_last: crate::style::types::TextAlignLast| match align_last {
        crate::style::types::TextAlignLast::Start => map_text_align(TextAlign::Start, direction),
        crate::style::types::TextAlignLast::End => map_text_align(TextAlign::End, direction),
        crate::style::types::TextAlignLast::Left => TextAlign::Left,
        crate::style::types::TextAlignLast::Right => TextAlign::Right,
        crate::style::types::TextAlignLast::Center => TextAlign::Center,
        crate::style::types::TextAlignLast::Justify => TextAlign::Justify,
        crate::style::types::TextAlignLast::Auto => text_align,
    };

    match text_align_last {
        crate::style::types::TextAlignLast::Auto => {
            if matches!(text_align, TextAlign::Justify) {
                if is_single_line {
                    text_align
                } else {
                    map_text_align(TextAlign::Start, direction)
                }
            } else {
                text_align
            }
        }
        _ => mapped_start_end(text_align_last),
    }
}

fn map_text_align(text_align: TextAlign, direction: crate::style::types::Direction) -> TextAlign {
    match text_align {
        TextAlign::Start => {
            if matches!(direction, crate::style::types::Direction::Rtl) {
                TextAlign::Right
            } else {
                TextAlign::Left
            }
        }
        TextAlign::End => {
            if matches!(direction, crate::style::types::Direction::Rtl) {
                TextAlign::Left
            } else {
                TextAlign::Right
            }
        }
        TextAlign::MatchParent => {
            if matches!(direction, crate::style::types::Direction::Rtl) {
                TextAlign::Right
            } else {
                TextAlign::Left
            }
        }
        _ => text_align,
    }
}

fn compute_paragraph_line_flags(lines: &[Line]) -> Vec<(bool, bool)> {
    let mut flags = Vec::with_capacity(lines.len());
    let mut para_start = 0;

    for (idx, line) in lines.iter().enumerate() {
        let is_para_end = line.ends_with_hard_break || idx + 1 == lines.len();
        if is_para_end {
            let para_len = idx - para_start + 1;
            let is_single = para_len == 1;
            for line_idx in para_start..=idx {
                flags.push((line_idx == idx, is_single));
            }
            para_start = idx + 1;
        }
    }

    flags
}

fn determine_paragraph_direction(items: &[InlineItem]) -> Option<crate::style::types::Direction> {
    const OBJECT_REPLACEMENT: char = '\u{FFFC}';

    fn collect_text(item: &InlineItem, out: &mut String) {
        match item {
            InlineItem::Text(t) => out.push_str(&t.text),
            InlineItem::Tab(_) => out.push('\t'),
            InlineItem::InlineBox(b) => {
                for child in &b.children {
                    collect_text(child, out);
                }
            }
            InlineItem::InlineBlock(_) | InlineItem::Replaced(_) => out.push(OBJECT_REPLACEMENT),
        }
    }

    let mut logical = String::new();
    for item in items {
        collect_text(item, &mut logical);
    }

    if logical.is_empty() {
        return None;
    }

    let bidi = unicode_bidi::BidiInfo::new(&logical, None);
    bidi.paragraphs.first().map(|p| {
        if p.level.is_rtl() {
            crate::style::types::Direction::Rtl
        } else {
            crate::style::types::Direction::Ltr
        }
    })
}

fn resolve_auto_text_justify(mode: TextJustify, items: &[PositionedItem]) -> TextJustify {
    if !matches!(mode, TextJustify::Auto) {
        return mode;
    }
    let counts = count_justify_scripts(items);
    if counts.total == 0 {
        return TextJustify::InterWord;
    }
    if counts.cjk * 2 >= counts.total {
        TextJustify::InterCharacter
    } else {
        TextJustify::InterWord
    }
}

fn has_justify_opportunities(items: &[PositionedItem], mode: TextJustify) -> bool {
    if matches!(mode, TextJustify::None) {
        return false;
    }

    for pos in items {
        match &pos.item {
            InlineItem::Text(t) => match mode {
                TextJustify::InterCharacter | TextJustify::Distribute => {
                    if t.cluster_byte_offsets().skip(1).next().is_some() || t.text.chars().count() > 1 {
                        return true;
                    }
                }
                _ => {
                    if t.text.chars().any(is_expandable_space) {
                        return true;
                    }
                }
            },
            InlineItem::InlineBox(b) => {
                for child in &b.children {
                    if let InlineItem::Text(t) = child {
                        match mode {
                            TextJustify::InterCharacter | TextJustify::Distribute => {
                                if t.cluster_byte_offsets().skip(1).next().is_some() || t.text.chars().count() > 1 {
                                    return true;
                                }
                            }
                            _ => {
                                if t.text.chars().any(is_expandable_space) {
                                    return true;
                                }
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }

    false
}

#[derive(Default)]
struct ScriptCounts {
    cjk: u32,
    total: u32,
}

fn count_justify_scripts(items: &[PositionedItem]) -> ScriptCounts {
    let mut counts = ScriptCounts::default();
    for pos in items {
        accumulate_scripts(&pos.item, &mut counts);
    }
    counts
}

fn accumulate_scripts(item: &InlineItem, counts: &mut ScriptCounts) {
    match item {
        InlineItem::Text(t) => {
            for ch in t.text.chars() {
                if ch.is_whitespace() {
                    continue;
                }
                counts.total += 1;
                if is_cjk_character(ch) {
                    counts.cjk += 1;
                }
            }
        }
        InlineItem::Tab(_) => {}
        InlineItem::InlineBox(b) => {
            for child in &b.children {
                accumulate_scripts(child, counts);
            }
        }
        InlineItem::InlineBlock(_) | InlineItem::Replaced(_) => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::geometry::Size;
    use crate::layout::formatting_context::IntrinsicSizingMode;
    use crate::style::display::{Display, FormattingContextType};
    use crate::style::types::{HyphensMode, ListStylePosition, OverflowWrap, TextTransform, WhiteSpace, WordBreak};
    use crate::style::values::Length;
    use crate::style::ComputedStyle;
    use crate::tree::box_tree::ReplacedType;
    use crate::tree::fragment_tree::FragmentContent;
    use std::sync::Arc;

    fn default_style() -> Arc<ComputedStyle> {
        let mut style = ComputedStyle::default();
        style.font_size = 16.0;
        Arc::new(style)
    }

    fn make_text_box(text: &str) -> BoxNode {
        BoxNode::new_text(default_style(), text.to_string())
    }

    fn make_inline_container(children: Vec<BoxNode>) -> BoxNode {
        BoxNode::new_block(default_style(), FormattingContextType::Block, children)
    }

    fn marker_and_text_positions(fragment: &FragmentNode) -> (Option<f32>, Option<f32>) {
        let mut marker_x = None;
        let mut text_x = None;
        let mut stack = vec![fragment];
        while let Some(node) = stack.pop() {
            match node.content {
                FragmentContent::Replaced { .. } => {
                    if marker_x.is_none() {
                        marker_x = Some(node.bounds.x());
                    }
                }
                FragmentContent::Text { .. } => {
                    if text_x.is_none() {
                        text_x = Some(node.bounds.x());
                    }
                }
                _ => {}
            }
            for child in &node.children {
                stack.push(child);
            }
        }
        (marker_x, text_x)
    }

    #[test]
    fn test_ifc_new() {
        let _ifc = InlineFormattingContext::new();
    }

    #[test]
    fn test_layout_empty_container() {
        let ifc = InlineFormattingContext::new();
        let root = make_inline_container(vec![]);
        let constraints = LayoutConstraints::definite_width(400.0);

        let fragment = ifc.layout(&root, &constraints).unwrap();
        assert_eq!(fragment.bounds.height(), 0.0);
    }

    #[test]
    fn test_layout_single_text() {
        let ifc = InlineFormattingContext::new();
        let text = make_text_box("Hello, world!");
        let root = make_inline_container(vec![text]);
        let constraints = LayoutConstraints::definite_width(400.0);

        let fragment = ifc.layout(&root, &constraints).unwrap();

        // Layout should succeed and produce valid output
        // Width may be 0 if no content, otherwise should be positive
        assert!(fragment.bounds.width() >= 0.0);
        assert!(fragment.bounds.height() >= 0.0);
    }

    #[test]
    fn test_layout_multiple_text_nodes() {
        let ifc = InlineFormattingContext::new();
        let text1 = make_text_box("Hello");
        let text2 = make_text_box(" ");
        let text3 = make_text_box("world!");
        let root = make_inline_container(vec![text1, text2, text3]);
        let constraints = LayoutConstraints::definite_width(400.0);

        let fragment = ifc.layout(&root, &constraints).unwrap();

        // Layout should succeed
        assert!(fragment.bounds.width() >= 0.0);
    }

    #[test]
    fn marker_image_outside_does_not_shift_content() {
        let ifc = InlineFormattingContext::new();
        let mut marker_style = ComputedStyle::default();
        marker_style.list_style_position = ListStylePosition::Outside;
        let marker_style = Arc::new(marker_style);

        let marker = BoxNode::new_marker(
            marker_style.clone(),
            MarkerContent::Image(ReplacedBox {
                replaced_type: ReplacedType::Image {
                    src: String::new(),
                    alt: None,
                },
                intrinsic_size: Some(Size::new(10.0, 10.0)),
                aspect_ratio: Some(1.0),
            }),
        );
        let text = make_text_box("content");
        let root = make_inline_container(vec![marker, text]);
        let constraints = LayoutConstraints::definite_width(200.0);

        let fragment = ifc.layout(&root, &constraints).unwrap();
        let line = fragment.children.first().expect("line fragment");
        let (marker_x, text_x) = marker_and_text_positions(line);

        assert!(text_x.unwrap_or(-100.0) >= -0.01 && text_x.unwrap() <= 0.1);
        assert!(marker_x.unwrap_or(0.0) < -5.0);
    }

    #[test]
    fn marker_image_inside_consumes_inline_space() {
        let ifc = InlineFormattingContext::new();
        let mut marker_style = ComputedStyle::default();
        marker_style.list_style_position = ListStylePosition::Inside;
        let marker_style = Arc::new(marker_style);

        let marker = BoxNode::new_marker(
            marker_style.clone(),
            MarkerContent::Image(ReplacedBox {
                replaced_type: ReplacedType::Image {
                    src: String::new(),
                    alt: None,
                },
                intrinsic_size: Some(Size::new(10.0, 10.0)),
                aspect_ratio: Some(1.0),
            }),
        );
        let text = make_text_box("content");
        let root = make_inline_container(vec![marker, text]);
        let constraints = LayoutConstraints::definite_width(200.0);

        let fragment = ifc.layout(&root, &constraints).unwrap();
        let line = fragment.children.first().expect("line fragment");
        let (marker_x, text_x) = marker_and_text_positions(line);

        assert!(marker_x.unwrap_or(-10.0) >= -0.01);
        // 10px intrinsic width + 0.5em gap (8px at 16px font size)
        let x = text_x.expect("text position");
        assert!(x > 17.0 && x < 19.5, "expected text after marker gap, got {}", x);
    }

    #[test]
    fn replaced_inline_includes_padding_and_border_in_box() {
        let ifc = InlineFormattingContext::new();
        let mut style = ComputedStyle::default();
        style.padding_left = Length::px(4.0);
        style.padding_right = Length::px(4.0);
        style.padding_top = Length::px(2.0);
        style.padding_bottom = Length::px(2.0);
        style.border_left_width = Length::px(2.0);
        style.border_right_width = Length::px(2.0);
        style.border_top_width = Length::px(1.0);
        style.border_bottom_width = Length::px(1.0);
        style.margin_left = Some(Length::px(10.0));
        style.margin_right = Some(Length::px(6.0));
        let style = Arc::new(style);

        let replaced = BoxNode::new_replaced(
            style.clone(),
            ReplacedType::Image {
                src: String::new(),
                alt: None,
            },
            Some(Size::new(50.0, 20.0)),
            Some(50.0 / 20.0),
        );
        let root = make_inline_container(vec![replaced]);
        let constraints = LayoutConstraints::definite_width(300.0);

        let fragment = ifc.layout(&root, &constraints).unwrap();
        let line = fragment.children.first().expect("line");
        let replaced_frag = line.children.first().expect("replaced fragment");

        // Content 50 + padding 8 + borders 4 = 62
        assert!((replaced_frag.bounds.width() - 62.0).abs() < 0.01);
        // margin-left applied to position
        assert!((replaced_frag.bounds.x() - 10.0).abs() < 0.01);
    }

    #[test]
    fn test_line_breaking() {
        let ifc = InlineFormattingContext::new();
        // Long text that should wrap
        let text = make_text_box("This is a long sentence that should wrap onto multiple lines.");
        let root = make_inline_container(vec![text]);
        // Very narrow container
        let constraints = LayoutConstraints::definite_width(100.0);

        let fragment = ifc.layout(&root, &constraints).unwrap();

        // Should produce valid layout
        assert!(fragment.bounds.height() >= 0.0);
    }

    #[test]
    fn test_min_content_width() {
        let ifc = InlineFormattingContext::new();
        let text = make_text_box("Hello World");
        let root = make_inline_container(vec![text]);

        let min_width = ifc
            .compute_intrinsic_inline_size(&root, IntrinsicSizingMode::MinContent)
            .unwrap();

        // Min content should be smaller than max content
        let max_width = ifc
            .compute_intrinsic_inline_size(&root, IntrinsicSizingMode::MaxContent)
            .unwrap();

        assert!(min_width <= max_width);
    }

    #[test]
    fn test_max_content_width() {
        let ifc = InlineFormattingContext::new();
        let text = make_text_box("Hello");
        let root = make_inline_container(vec![text]);

        let max_width = ifc
            .compute_intrinsic_inline_size(&root, IntrinsicSizingMode::MaxContent)
            .unwrap();

        // "Hello" = 5 chars * 0.5 * 16px = 40px
        assert!(max_width > 0.0);
    }

    #[test]
    fn intrinsic_width_accounts_for_text_indent() {
        let ifc = InlineFormattingContext::new();
        let mut parent_style = ComputedStyle::default();
        parent_style.text_indent.length = Length::px(12.0);
        let text = "word";
        let text_node = make_text_box(text);
        let root = BoxNode::new_block(Arc::new(parent_style), FormattingContextType::Block, vec![text_node]);

        let base_item = ifc.create_text_item(&make_text_box(text), text).unwrap();
        let base_width = base_item.advance;

        let min_width = ifc
            .compute_intrinsic_inline_size(&root, IntrinsicSizingMode::MinContent)
            .unwrap();
        assert!(
            min_width >= base_width + 10.0,
            "min-content should include indent; got {min_width}, base {base_width}"
        );

        let max_width = ifc
            .compute_intrinsic_inline_size(&root, IntrinsicSizingMode::MaxContent)
            .unwrap();
        assert!(
            max_width >= base_width + 10.0,
            "max-content should include indent; got {max_width}, base {base_width}"
        );
    }

    #[test]
    fn intrinsic_width_accounts_for_hanging_indent() {
        let ifc = InlineFormattingContext::new();
        let mut parent_style = ComputedStyle::default();
        parent_style.text_indent.length = Length::px(14.0);
        parent_style.text_indent.hanging = true;
        let text = "word\nword";
        let text_node = make_text_box(text);
        let root = BoxNode::new_block(Arc::new(parent_style), FormattingContextType::Block, vec![text_node]);

        let base_item = ifc.create_text_item(&make_text_box("word"), "word").unwrap();
        let base_width = base_item.advance;

        let min_width = ifc
            .compute_intrinsic_inline_size(&root, IntrinsicSizingMode::MinContent)
            .unwrap();
        assert!(
            min_width >= base_width + 10.0,
            "min-content should include hanging indent for subsequent lines; got {min_width}"
        );
    }

    #[test]
    fn text_justify_auto_uses_inter_character_for_cjk() {
        let mut root_style = ComputedStyle::default();
        root_style.text_align = TextAlign::Justify;
        root_style.text_justify = TextJustify::Auto;
        root_style.white_space = WhiteSpace::PreWrap;
        root_style.text_align_last = crate::style::types::TextAlignLast::Justify;
        let mut text_style = ComputedStyle::default();
        text_style.white_space = WhiteSpace::PreWrap;
        let root = BoxNode::new_block(
            Arc::new(root_style),
            FormattingContextType::Block,
            vec![BoxNode::new_text(Arc::new(text_style), "漢字漢字\n漢".to_string())],
        );
        let constraints = LayoutConstraints::definite_width(400.0);

        let ifc = InlineFormattingContext::new();
        let items = ifc
            .collect_inline_items(&root, constraints.width().unwrap(), constraints.height())
            .unwrap();
        let strut = ifc.compute_strut_metrics(&root.style);
        let lines = ifc.build_lines(
            items,
            constraints.width().unwrap(),
            constraints.width().unwrap(),
            &strut,
            Some(unicode_bidi::Level::ltr()),
        );
        let first = lines.first().expect("first line");
        let resolved = resolve_auto_text_justify(TextJustify::Auto, &first.items);
        assert_eq!(resolved, TextJustify::InterCharacter);
        let expanded = ifc.expand_items_for_justification(&first.items, resolved);
        assert!(
            expanded.len() >= 4,
            "expanded items should split per cluster; got {}",
            expanded.len()
        );

        let fragment = ifc.layout(&root, &constraints).expect("layout");
        assert!(
            fragment.children.len() >= 2,
            "newline should create multiple lines for justification test"
        );
        let first_line = fragment.children.first().expect("first line");
        let count = first_line.children.len();
        assert!(
            count >= 4,
            "inter-character justify should split CJK into per-cluster items; count={count}"
        );
        let a = &first_line.children[0];
        let b = &first_line.children[1];
        let gap = b.bounds.x() - (a.bounds.x() + a.bounds.width());
        assert!(
            gap > 0.5,
            "justify should expand gaps between CJK clusters; gap={gap}, count={count}"
        );
    }

    #[test]
    fn text_justify_auto_keeps_inter_word_for_latin() {
        let mut root_style = ComputedStyle::default();
        root_style.text_align = TextAlign::Justify;
        root_style.text_justify = TextJustify::Auto;
        root_style.white_space = WhiteSpace::PreWrap;
        let root = BoxNode::new_block(
            Arc::new(root_style),
            FormattingContextType::Block,
            vec![make_text_box("a b\nc")],
        );
        let constraints = LayoutConstraints::definite_width(200.0);

        let ifc = InlineFormattingContext::new();
        let fragment = ifc.layout(&root, &constraints).expect("layout");
        let first_line = fragment.children.first().expect("first line");
        assert!(
            first_line.children.len() < 4,
            "inter-word justify should not split Latin into per-character items"
        );
    }

    #[test]
    fn text_justify_auto_prefers_inter_word_when_latin_dominates() {
        let mut root_style = ComputedStyle::default();
        root_style.text_align = TextAlign::Justify;
        root_style.text_justify = TextJustify::Auto;
        root_style.white_space = WhiteSpace::PreWrap;
        let mut text_style = ComputedStyle::default();
        text_style.white_space = WhiteSpace::PreWrap;
        let root = BoxNode::new_block(
            Arc::new(root_style),
            FormattingContextType::Block,
            vec![BoxNode::new_text(
                Arc::new(text_style),
                "Latin Latin Latin 漢".to_string(),
            )],
        );
        let constraints = LayoutConstraints::definite_width(200.0);

        let ifc = InlineFormattingContext::new();
        let items = ifc
            .collect_inline_items(&root, constraints.width().unwrap(), constraints.height())
            .unwrap();
        let strut = ifc.compute_strut_metrics(&root.style);
        let lines = ifc.build_lines(
            items,
            constraints.width().unwrap(),
            constraints.width().unwrap(),
            &strut,
            Some(unicode_bidi::Level::ltr()),
        );
        let first = lines.first().expect("first line");
        let resolved = resolve_auto_text_justify(TextJustify::Auto, &first.items);
        assert_eq!(resolved, TextJustify::InterWord);
    }

    #[test]
    fn test_fc_is_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<InlineFormattingContext>();
    }

    #[test]
    fn test_line_has_baseline() {
        let ifc = InlineFormattingContext::new();
        let text = make_text_box("Test");
        let root = make_inline_container(vec![text]);
        let constraints = LayoutConstraints::definite_width(400.0);

        let fragment = ifc.layout(&root, &constraints).unwrap();

        // First child should be a line with baseline
        if let Some(line) = fragment.children.first() {
            assert!(line.bounds.height() > 0.0);
        }
    }

    #[test]
    fn test_layout_with_indefinite_width() {
        let ifc = InlineFormattingContext::new();
        let text = make_text_box("Hello world");
        let root = make_inline_container(vec![text]);
        let constraints = LayoutConstraints::indefinite();

        let fragment = ifc.layout(&root, &constraints).unwrap();

        // Should produce valid output
        assert!(fragment.bounds.width() >= 0.0);
    }

    #[test]
    fn inline_block_percentage_height_uses_containing_height() {
        let ifc = InlineFormattingContext::new();
        let mut ib_style = ComputedStyle::default();
        ib_style.display = Display::InlineBlock;
        ib_style.height = Some(Length::percent(50.0));
        ib_style.width = Some(Length::px(20.0));
        let inline_block = BoxNode::new_inline_block(Arc::new(ib_style), FormattingContextType::Block, vec![]);
        let root = make_inline_container(vec![inline_block]);
        let constraints = LayoutConstraints::definite(200.0, 120.0);

        let fragment = ifc.layout(&root, &constraints).unwrap();
        let line = fragment.children.first().expect("line fragment");
        let child = line.children.first().expect("inline-block fragment");
        assert!((child.bounds.height() - 60.0).abs() < 0.1);
    }

    #[test]
    fn test_mandatory_line_break() {
        let ifc = InlineFormattingContext::new();
        let text = make_text_box("Line one\nLine two");
        let root = make_inline_container(vec![text]);
        let constraints = LayoutConstraints::definite_width(400.0);

        let fragment = ifc.layout(&root, &constraints).unwrap();

        // Should produce valid layout with multiple lines
        // Note: actual line count depends on break handling implementation
        assert!(fragment.bounds.height() >= 0.0);
    }

    #[test]
    fn test_empty_text() {
        let ifc = InlineFormattingContext::new();
        let text = make_text_box("");
        let root = make_inline_container(vec![text]);
        let constraints = LayoutConstraints::definite_width(400.0);

        let fragment = ifc.layout(&root, &constraints).unwrap();
        // Should handle empty text gracefully
        assert!(fragment.bounds.height() >= 0.0);
    }

    #[test]
    fn test_whitespace_only() {
        let ifc = InlineFormattingContext::new();
        let text = make_text_box("   ");
        let root = make_inline_container(vec![text]);
        let constraints = LayoutConstraints::definite_width(400.0);

        let fragment = ifc.layout(&root, &constraints).unwrap();
        assert!(fragment.bounds.width() >= 0.0);
    }

    #[test]
    fn white_space_normal_collapses_runs() {
        let mut style = ComputedStyle::default();
        style.white_space = WhiteSpace::Normal;

        let normalized = normalize_text_for_white_space("  foo \n\tbar  ", style.white_space);
        assert_eq!(normalized.text, "foo bar");
        assert!(normalized.allow_soft_wrap);
        assert!(normalized.forced_breaks.is_empty());
    }

    #[test]
    fn white_space_pre_preserves_tabs_as_items() {
        let mut style = ComputedStyle::default();
        style.white_space = WhiteSpace::Pre;
        let node = BoxNode::new_text(Arc::new(style), "a\tb".to_string());
        let ifc = InlineFormattingContext::new();
        let items = ifc.create_inline_items_for_text(&node, "a\tb", false).unwrap();
        assert_eq!(items.len(), 3);
        assert!(matches!(&items[1], InlineItem::Tab(_)));
    }

    #[test]
    fn pre_collapses_crlf_to_single_break() {
        let mut style = ComputedStyle::default();
        style.white_space = WhiteSpace::Pre;
        let normalized = normalize_text_for_white_space("a\r\nb", style.white_space);
        assert_eq!(normalized.text, "ab");
        assert_eq!(normalized.forced_breaks.len(), 1);
        assert_eq!(normalized.forced_breaks[0].byte_offset, 1);
        assert!(!normalized.allow_soft_wrap);
    }

    #[test]
    fn collapsible_space_survives_across_text_nodes() {
        let mut style = ComputedStyle::default();
        style.white_space = WhiteSpace::Normal;
        let text_style = Arc::new(style);
        let root = BoxNode::new_block(
            default_style(),
            FormattingContextType::Block,
            vec![
                BoxNode::new_text(text_style.clone(), "Hello ".to_string()),
                BoxNode::new_text(text_style.clone(), "world".to_string()),
            ],
        );
        let ifc = InlineFormattingContext::new();
        let items = ifc
            .collect_inline_items(&root, 800.0, Some(800.0))
            .expect("collect items");
        let texts: Vec<String> = items
            .iter()
            .filter_map(|item| match item {
                InlineItem::Text(t) => Some(t.text.clone()),
                _ => None,
            })
            .collect();
        assert_eq!(texts, vec!["Hello", " ", "world"]);
    }

    #[test]
    fn collapsible_space_survives_across_inline_boxes() {
        let mut style = ComputedStyle::default();
        style.white_space = WhiteSpace::Normal;
        let text_style = Arc::new(style);
        let inline = BoxNode::new_inline(
            text_style.clone(),
            vec![BoxNode::new_text(text_style.clone(), "world".to_string())],
        );
        let root = BoxNode::new_block(
            default_style(),
            FormattingContextType::Block,
            vec![BoxNode::new_text(text_style.clone(), "Hello ".to_string()), inline],
        );
        let ifc = InlineFormattingContext::new();
        let items = ifc
            .collect_inline_items(&root, 800.0, Some(800.0))
            .expect("collect items");
        assert!(
            items
                .iter()
                .any(|item| matches!(item, InlineItem::Text(t) if t.text == " ")),
            "collapsed space should appear before inline box"
        );
    }

    #[test]
    fn trailing_collapsible_space_is_dropped_at_end() {
        let mut style = ComputedStyle::default();
        style.white_space = WhiteSpace::Normal;
        let text_style = Arc::new(style);
        let root = BoxNode::new_block(
            default_style(),
            FormattingContextType::Block,
            vec![BoxNode::new_text(text_style, "Hello ".to_string())],
        );
        let ifc = InlineFormattingContext::new();
        let items = ifc
            .collect_inline_items(&root, 800.0, Some(800.0))
            .expect("collect items");
        assert!(
            items.iter().all(|item| match item {
                InlineItem::Text(t) => t.text != " ",
                _ => true,
            }),
            "trailing collapsible whitespace should not emit a space item"
        );
    }

    #[test]
    fn pre_treats_form_feed_as_break() {
        let mut style = ComputedStyle::default();
        style.white_space = WhiteSpace::Pre;
        let normalized = normalize_text_for_white_space("a\u{000C}b", style.white_space);
        assert_eq!(normalized.text, "ab");
        assert_eq!(normalized.forced_breaks.len(), 1);
        assert_eq!(normalized.forced_breaks[0].byte_offset, 1);
        assert!(!normalized.allow_soft_wrap);
    }

    #[test]
    fn word_break_break_word_splits_when_overflowing() {
        let mut text_style = ComputedStyle::default();
        text_style.word_break = WordBreak::BreakWord;
        text_style.white_space = WhiteSpace::Normal;
        let root = BoxNode::new_block(
            default_style(),
            FormattingContextType::Block,
            vec![BoxNode::new_text(
                Arc::new(text_style),
                "supercalifragilisticexpialidocious".to_string(),
            )],
        );
        let constraints = LayoutConstraints::definite_width(40.0);
        let ifc = InlineFormattingContext::new();
        let fragment = ifc.layout(&root, &constraints).expect("layout");
        assert!(
            fragment.children.len() > 1,
            "break-word should allow breaking long tokens when they overflow"
        );
    }

    #[test]
    fn word_break_break_word_respects_nowrap() {
        let mut text_style = ComputedStyle::default();
        text_style.word_break = WordBreak::BreakWord;
        text_style.white_space = WhiteSpace::Nowrap;
        let root = BoxNode::new_block(
            default_style(),
            FormattingContextType::Block,
            vec![BoxNode::new_text(
                Arc::new(text_style),
                "supercalifragilisticexpialidocious".to_string(),
            )],
        );
        let constraints = LayoutConstraints::definite_width(40.0);
        let ifc = InlineFormattingContext::new();
        let fragment = ifc.layout(&root, &constraints).expect("layout");
        assert_eq!(
            fragment.children.len(),
            1,
            "nowrap should suppress break-word forced wraps"
        );
    }

    #[test]
    fn word_break_break_word_reduces_min_content_width() {
        let mut text_style = ComputedStyle::default();
        text_style.word_break = WordBreak::BreakWord;
        text_style.white_space = WhiteSpace::Normal;
        let breaking = BoxNode::new_block(
            default_style(),
            FormattingContextType::Block,
            vec![BoxNode::new_text(Arc::new(text_style.clone()), "longtoken".to_string())],
        );
        let normal = BoxNode::new_block(
            default_style(),
            FormattingContextType::Block,
            vec![BoxNode::new_text(
                Arc::new(ComputedStyle::default()),
                "longtoken".to_string(),
            )],
        );
        let ifc = InlineFormattingContext::new();
        let breaking_min = ifc.calculate_intrinsic_width(&breaking, IntrinsicSizingMode::MinContent);
        let normal_min = ifc.calculate_intrinsic_width(&normal, IntrinsicSizingMode::MinContent);
        assert!(
            (breaking_min - normal_min).abs() < 0.1,
            "break-word should not reduce min-content widths; only overflow handling differs"
        );
    }

    #[test]
    fn overflow_wrap_break_word_splits_overflowing_word() {
        let mut text_style = ComputedStyle::default();
        text_style.overflow_wrap = OverflowWrap::BreakWord;
        text_style.white_space = WhiteSpace::Normal;
        let root = BoxNode::new_block(
            default_style(),
            FormattingContextType::Block,
            vec![BoxNode::new_text(
                Arc::new(text_style),
                "supercalifragilisticexpialidocious".to_string(),
            )],
        );
        let constraints = LayoutConstraints::definite_width(40.0);
        let ifc = InlineFormattingContext::new();
        let fragment = ifc.layout(&root, &constraints).expect("layout");
        assert!(
            fragment.children.len() > 1,
            "overflow-wrap: break-word should allow breaking long tokens when they overflow"
        );
    }

    #[test]
    fn overflow_wrap_break_word_respects_nowrap() {
        let mut text_style = ComputedStyle::default();
        text_style.overflow_wrap = OverflowWrap::BreakWord;
        text_style.white_space = WhiteSpace::Nowrap;
        let root = BoxNode::new_block(
            default_style(),
            FormattingContextType::Block,
            vec![BoxNode::new_text(
                Arc::new(text_style),
                "supercalifragilisticexpialidocious".to_string(),
            )],
        );
        let constraints = LayoutConstraints::definite_width(40.0);
        let ifc = InlineFormattingContext::new();
        let fragment = ifc.layout(&root, &constraints).expect("layout");
        assert_eq!(
            fragment.children.len(),
            1,
            "nowrap should suppress overflow-wrap: break-word emergency breaks"
        );
    }

    #[test]
    fn overflow_wrap_break_word_keeps_min_content_width() {
        let mut text_style = ComputedStyle::default();
        text_style.overflow_wrap = OverflowWrap::BreakWord;
        text_style.white_space = WhiteSpace::Normal;
        let breaking = BoxNode::new_block(
            default_style(),
            FormattingContextType::Block,
            vec![BoxNode::new_text(Arc::new(text_style.clone()), "longtoken".to_string())],
        );
        let normal = BoxNode::new_block(
            default_style(),
            FormattingContextType::Block,
            vec![BoxNode::new_text(
                Arc::new(ComputedStyle::default()),
                "longtoken".to_string(),
            )],
        );
        let ifc = InlineFormattingContext::new();
        let breaking_min = ifc.calculate_intrinsic_width(&breaking, IntrinsicSizingMode::MinContent);
        let normal_min = ifc.calculate_intrinsic_width(&normal, IntrinsicSizingMode::MinContent);
        assert!(
            (breaking_min - normal_min).abs() < 0.1,
            "overflow-wrap: break-word should not reduce min-content widths"
        );
    }

    #[test]
    fn normal_collapses_vertical_tab_to_space() {
        let mut style = ComputedStyle::default();
        style.white_space = WhiteSpace::Normal;
        let normalized = normalize_text_for_white_space("a\u{000B} b", style.white_space);
        assert_eq!(normalized.text, "a b");
        assert!(normalized.forced_breaks.is_empty());
        assert!(normalized.allow_soft_wrap);
    }

    #[test]
    fn break_spaces_preserves_sequences() {
        let mut style = ComputedStyle::default();
        style.white_space = WhiteSpace::BreakSpaces;
        let normalized = normalize_text_for_white_space("  a  \n b ", style.white_space);
        assert_eq!(normalized.text, "  a   b ");
        assert_eq!(normalized.forced_breaks.len(), 1);
        assert_eq!(normalized.forced_breaks[0].byte_offset, 5);
        assert!(normalized.allow_soft_wrap);
        assert!(!normalized.leading_collapsible);
        assert!(!normalized.trailing_collapsible);
    }

    #[test]
    fn pre_line_treats_crlf_as_single_break() {
        let mut style = ComputedStyle::default();
        style.white_space = WhiteSpace::PreLine;
        let normalized = normalize_text_for_white_space("a\r\n b", style.white_space);
        assert_eq!(normalized.text, "a b");
        assert_eq!(normalized.forced_breaks.len(), 1);
        assert_eq!(normalized.forced_breaks[0].byte_offset, 2);
        assert!(normalized.allow_soft_wrap);
    }

    #[test]
    fn tabs_advance_to_next_stop() {
        let mut style = ComputedStyle::default();
        style.white_space = WhiteSpace::Pre;
        style.tab_size = TabSize::Number(4.0);
        let node = BoxNode::new_text(Arc::new(style.clone()), "ab\tc".to_string());

        let ifc = InlineFormattingContext::new();
        let mut items = ifc.create_inline_items_for_text(&node, "ab\tc", false).unwrap();
        let strut = ifc.compute_strut_metrics(&node.style);
        let lines = ifc.build_lines(std::mem::take(&mut items), 1000.0, 1000.0, &strut, None);
        let line = &lines[0];
        assert_eq!(line.items.len(), 3);

        let interval = match &line.items[1].item {
            InlineItem::Tab(tab) => tab.interval(),
            _ => panic!("expected tab item"),
        };
        let first_width = line.items[0].item.width();
        let tab_width = line.items[1].item.width();
        let expected = if interval > 0.0 {
            let remainder = first_width.rem_euclid(interval);
            if remainder == 0.0 {
                interval
            } else {
                interval - remainder
            }
        } else {
            0.0
        };
        assert!((tab_width - expected).abs() < 0.05);
        let following_start = line.items[2].x;
        assert!((following_start - (first_width + tab_width)).abs() < 0.05);
    }

    #[test]
    fn white_space_nowrap_strips_soft_breaks() {
        let mut style = ComputedStyle::default();
        style.white_space = WhiteSpace::Nowrap;
        let text = "foo bar";

        let ifc = InlineFormattingContext::new();
        let node = BoxNode::new_text(Arc::new(style), text.to_string());
        let item = ifc.create_text_item(&node, text).unwrap();

        assert!(item
            .break_opportunities
            .iter()
            .all(|b| b.break_type == BreakType::Mandatory));
        assert_eq!(item.text, "foo bar");
    }

    #[test]
    fn white_space_pre_preserves_spaces_and_breaks() {
        let mut style = ComputedStyle::default();
        style.white_space = WhiteSpace::Pre;
        let text = "a  \n b";

        let ifc = InlineFormattingContext::new();
        let node = BoxNode::new_text(Arc::new(style), text.to_string());
        let item = ifc.create_text_item(&node, text).unwrap();

        assert_eq!(item.text, "a   b");
        assert!(item
            .break_opportunities
            .iter()
            .all(|b| b.break_type == BreakType::Mandatory));
        assert!(item
            .break_opportunities
            .iter()
            .any(|b| b.byte_offset == 3 && b.break_type == BreakType::Mandatory));
    }

    #[test]
    fn text_align_right_offsets_child() {
        let mut root_style = ComputedStyle::default();
        root_style.font_size = 16.0;
        root_style.text_align = TextAlign::Right;
        let root = BoxNode::new_block(
            Arc::new(root_style),
            FormattingContextType::Block,
            vec![make_text_box("hi")],
        );
        let constraints = LayoutConstraints::definite_width(200.0);

        let ifc = InlineFormattingContext::new();
        let fragment = ifc.layout(&root, &constraints).expect("layout");
        let line = fragment.children.first().expect("line fragment");
        let child = line.children.first().expect("text fragment");

        assert!(child.bounds.x() > 0.0);
        let offset = child.bounds.x();
        let remaining = (line.bounds.width() - child.bounds.width()).abs();
        assert!((offset - remaining).abs() < 0.5);
    }

    #[test]
    fn text_align_center_shifts_child_from_origin() {
        let mut root_style = ComputedStyle::default();
        root_style.font_size = 16.0;
        root_style.text_align = TextAlign::Center;
        let root = BoxNode::new_block(
            Arc::new(root_style),
            FormattingContextType::Block,
            vec![make_text_box("hi")],
        );
        let constraints = LayoutConstraints::definite_width(200.0);

        let ifc = InlineFormattingContext::new();
        let fragment = ifc.layout(&root, &constraints).expect("layout");
        let line = fragment.children.first().expect("line fragment");
        let child = line.children.first().expect("text fragment");

        assert!(child.bounds.x() > 0.0);
        assert!(child.bounds.x() < line.bounds.width() - child.bounds.width());
    }

    #[test]
    fn text_align_justify_expands_word_gaps() {
        let mut root_style = ComputedStyle::default();
        root_style.font_size = 16.0;
        root_style.text_align = TextAlign::Justify;
        let mut text_style = ComputedStyle::default();
        text_style.font_size = 16.0;
        text_style.white_space = WhiteSpace::Pre;
        let word = |text: &str| BoxNode::new_text(Arc::new(text_style.clone()), text.to_string());
        let root = BoxNode::new_block(
            Arc::new(root_style),
            FormattingContextType::Block,
            vec![word("a "), word("b "), word("c")],
        );
        let constraints = LayoutConstraints::definite_width(30.0);

        let ifc = InlineFormattingContext::new();
        let fragment = ifc.layout(&root, &constraints).expect("layout");
        assert!(fragment.children.len() >= 2, "justify should create multiple lines");
        let line = &fragment.children[0];
        assert!(line.children.len() >= 2, "first line should have two fragments");
        let a = &line.children[0];
        let b = &line.children[1];

        let gap = b.bounds.x() - (a.bounds.x() + a.bounds.width());
        assert!(gap > 3.0, "gap too small for justify: {}", gap);
    }

    #[test]
    fn text_align_last_end_forces_last_line_end_when_justify() {
        let mut root_style = ComputedStyle::default();
        root_style.font_size = 16.0;
        root_style.text_align = TextAlign::Justify;
        root_style.text_align_last = crate::style::types::TextAlignLast::End;
        let mut text_style = ComputedStyle::default();
        text_style.white_space = WhiteSpace::PreWrap;
        let root = BoxNode::new_block(
            Arc::new(root_style),
            FormattingContextType::Block,
            vec![BoxNode::new_text(Arc::new(text_style), "word word\nword".to_string())],
        );
        let constraints = LayoutConstraints::definite_width(200.0);

        let ifc = InlineFormattingContext::new();
        let fragment = ifc.layout(&root, &constraints).expect("layout");
        assert!(fragment.children.len() >= 2, "should wrap into multiple lines");
        let last_line = fragment.children.last().expect("last line");
        let last_child = last_line.children.first().expect("text fragment");
        // End alignment in LTR pushes text towards right edge
        assert!(last_child.bounds.x() > constraints.width().unwrap() * 0.3);
    }

    #[test]
    fn text_align_start_respects_direction() {
        let mut root_style = ComputedStyle::default();
        root_style.direction = crate::style::types::Direction::Rtl;
        root_style.text_align = TextAlign::Start;
        let root = BoxNode::new_block(
            Arc::new(root_style),
            FormattingContextType::Block,
            vec![make_text_box("hi")],
        );
        let constraints = LayoutConstraints::definite_width(200.0);

        let ifc = InlineFormattingContext::new();
        let fragment = ifc.layout(&root, &constraints).expect("layout");
        let line = fragment.children.first().expect("line fragment");
        let child = line.children.first().expect("text fragment");

        let expected = line.bounds.width() - child.bounds.width();
        assert!((child.bounds.x() - expected).abs() < 1.0);
    }

    #[test]
    fn text_align_match_parent_uses_parent_direction() {
        let mut root_style = ComputedStyle::default();
        root_style.direction = crate::style::types::Direction::Rtl;
        root_style.text_align = TextAlign::MatchParent;
        let root = BoxNode::new_block(
            Arc::new(root_style),
            FormattingContextType::Block,
            vec![make_text_box("hi")],
        );
        let constraints = LayoutConstraints::definite_width(200.0);

        let ifc = InlineFormattingContext::new();
        let fragment = ifc.layout(&root, &constraints).expect("layout");
        let line = fragment.children.first().expect("line fragment");
        let child = line.children.first().expect("text fragment");

        // Match-parent should align to the parent's start (right in RTL)
        let expected = line.bounds.width() - child.bounds.width();
        assert!((child.bounds.x() - expected).abs() < 1.0);
    }

    #[test]
    fn rtl_lines_position_items_from_right_edge() {
        let mut root_style = ComputedStyle::default();
        root_style.direction = crate::style::types::Direction::Rtl;
        let text_style = Arc::new(ComputedStyle::default());
        let root = BoxNode::new_block(
            Arc::new(root_style),
            FormattingContextType::Block,
            vec![
                BoxNode::new_text(text_style.clone(), "abc".to_string()),
                BoxNode::new_text(text_style.clone(), "def".to_string()),
            ],
        );
        let constraints = LayoutConstraints::definite_width(200.0);

        let ifc = InlineFormattingContext::new();
        let fragment = ifc.layout(&root, &constraints).expect("layout");
        let line = fragment.children.first().expect("line fragment");
        assert_eq!(line.children.len(), 2, "expected two text fragments");
        let first = &line.children[0];
        let second = &line.children[1];

        assert!(
            first.bounds.x() > second.bounds.x(),
            "in RTL the first fragment should be positioned to the right of the second"
        );
        let right_edge = first.bounds.x() + first.bounds.width();
        assert!(
            right_edge > line.bounds.width() * 0.5,
            "RTL start alignment should push the first fragment toward the right edge"
        );
    }

    #[test]
    fn plaintext_blocks_use_first_strong_direction_for_alignment() {
        let mut root_style = ComputedStyle::default();
        root_style.unicode_bidi = crate::style::types::UnicodeBidi::Plaintext;
        root_style.text_align = TextAlign::Start;
        let root = BoxNode::new_block(
            Arc::new(root_style),
            FormattingContextType::Block,
            vec![BoxNode::new_text(
                Arc::new(ComputedStyle::default()),
                "שלום".to_string(),
            )],
        );
        let constraints = LayoutConstraints::definite_width(200.0);

        let ifc = InlineFormattingContext::new();
        let fragment = ifc.layout(&root, &constraints).expect("layout");
        let line = fragment.children.first().expect("line fragment");
        let child = line.children.first().expect("text fragment");

        let expected = line.bounds.width() - child.bounds.width();
        assert!(
            (child.bounds.x() - expected).abs() < 1.0,
            "plaintext with RTL first-strong should align start at the right edge"
        );
    }

    #[test]
    fn plaintext_blocks_align_ltr_text_to_start() {
        let mut root_style = ComputedStyle::default();
        root_style.unicode_bidi = crate::style::types::UnicodeBidi::Plaintext;
        root_style.text_align = TextAlign::Start;
        let root = BoxNode::new_block(
            Arc::new(root_style),
            FormattingContextType::Block,
            vec![BoxNode::new_text(
                Arc::new(ComputedStyle::default()),
                "hello".to_string(),
            )],
        );
        let constraints = LayoutConstraints::definite_width(200.0);

        let ifc = InlineFormattingContext::new();
        let fragment = ifc.layout(&root, &constraints).expect("layout");
        let line = fragment.children.first().expect("line fragment");
        let child = line.children.first().expect("text fragment");
        assert!(
            child.bounds.x() < 1.0,
            "plaintext with LTR text should keep start alignment at the left edge"
        );
    }

    #[test]
    fn text_align_last_auto_uses_start_on_final_justify_line() {
        let mut root_style = ComputedStyle::default();
        root_style.font_size = 16.0;
        root_style.text_align = TextAlign::Justify;
        root_style.text_align_last = crate::style::types::TextAlignLast::Auto;
        let mut text_style = ComputedStyle::default();
        text_style.white_space = WhiteSpace::PreWrap;
        let root = BoxNode::new_block(
            Arc::new(root_style),
            FormattingContextType::Block,
            vec![BoxNode::new_text(Arc::new(text_style), "word word\nword".to_string())],
        );
        let constraints = LayoutConstraints::definite_width(60.0);

        let ifc = InlineFormattingContext::new();
        let fragment = ifc.layout(&root, &constraints).expect("layout");
        let last_line = fragment.children.last().expect("last line");
        let child = last_line.children.first().expect("text fragment");
        assert!(child.bounds.x() < 1.0, "last line should start-align under auto");
    }

    #[test]
    fn text_align_last_auto_justifies_single_line_when_align_is_justify() {
        let mut root_style = ComputedStyle::default();
        root_style.font_size = 16.0;
        root_style.text_align = TextAlign::Justify;
        root_style.text_align_last = crate::style::types::TextAlignLast::Auto;

        let text_style = Arc::new(ComputedStyle::default());
        let root = BoxNode::new_block(
            Arc::new(root_style),
            FormattingContextType::Block,
            vec![
                BoxNode::new_text(text_style.clone(), "word ".to_string()),
                BoxNode::new_text(text_style.clone(), "word".to_string()),
            ],
        );
        let constraints = LayoutConstraints::definite_width(200.0);

        let ifc = InlineFormattingContext::new();
        let fragment = ifc.layout(&root, &constraints).expect("layout");
        assert_eq!(fragment.children.len(), 1, "single-line paragraph expected");
        let line = fragment.children.first().unwrap();
        assert!(line.children.len() >= 2, "justification should keep multiple fragments");
        let last_child = line.children.last().unwrap();
        let right_edge = last_child.bounds.x() + last_child.bounds.width();
        assert!(
            right_edge > constraints.width().unwrap() * 0.8,
            "single-line auto+justify should still justify; right_edge={}, width={}",
            right_edge,
            constraints.width().unwrap()
        );
    }

    #[test]
    fn text_align_all_resets_last_line_alignment() {
        let mut root_style = ComputedStyle::default();
        root_style.font_size = 16.0;
        root_style.text_align = TextAlign::Start;
        root_style.text_align_last = crate::style::types::TextAlignLast::Auto;
        let mut text_style = ComputedStyle::default();
        text_style.white_space = WhiteSpace::PreWrap;
        let root = BoxNode::new_block(
            Arc::new(root_style),
            FormattingContextType::Block,
            vec![BoxNode::new_text(Arc::new(text_style), "word word".to_string())],
        );
        let constraints = LayoutConstraints::definite_width(200.0);

        let ifc = InlineFormattingContext::new();
        let fragment = ifc.layout(&root, &constraints).expect("layout");
        let last_line = fragment.children.last().expect("last line");
        let child = last_line.children.first().expect("text fragment");
        assert!(
            child.bounds.x() < 1.0,
            "text-align-all should reset last-line alignment to auto/start"
        );
    }

    #[test]
    fn text_align_justify_all_justifies_last_line() {
        let mut root_style = ComputedStyle::default();
        root_style.font_size = 16.0;
        root_style.text_align = TextAlign::Justify;
        root_style.text_align_last = crate::style::types::TextAlignLast::Justify;
        let mut text_style = ComputedStyle::default();
        text_style.white_space = WhiteSpace::PreWrap;
        let root = BoxNode::new_block(
            Arc::new(root_style),
            FormattingContextType::Block,
            vec![BoxNode::new_text(
                Arc::new(text_style),
                "word word\nword word".to_string(),
            )],
        );
        let constraints = LayoutConstraints::definite_width(60.0);

        let ifc = InlineFormattingContext::new();
        let fragment = ifc.layout(&root, &constraints).expect("layout");
        let last_line = fragment.children.last().expect("last line");
        assert!(last_line.children.len() >= 2);
        let last_child = last_line.children.last().expect("child");
        let right_edge = last_child.bounds.x() + last_child.bounds.width();
        assert!(
            right_edge > last_line.bounds.width() * 0.8,
            "justify-all should stretch content toward the end of the line; right_edge={}, line_width={}",
            right_edge,
            last_line.bounds.width()
        );
    }

    #[test]
    fn text_align_last_applies_to_paragraphs_split_by_hard_break() {
        let mut root_style = ComputedStyle::default();
        root_style.font_size = 16.0;
        root_style.text_align_last = crate::style::types::TextAlignLast::Right;
        root_style.white_space = WhiteSpace::PreWrap;
        let mut text_style = ComputedStyle::default();
        text_style.white_space = WhiteSpace::PreWrap;
        let root = BoxNode::new_block(
            Arc::new(root_style),
            FormattingContextType::Block,
            vec![BoxNode::new_text(Arc::new(text_style), "one\none".to_string())],
        );
        let constraints = LayoutConstraints::definite_width(100.0);

        let ifc = InlineFormattingContext::new();
        let fragment = ifc.layout(&root, &constraints).expect("layout");
        assert_eq!(fragment.children.len(), 2, "two lines expected");
        let first_x = fragment.children[0].children[0].bounds.x();
        let second_x = fragment.children[1].children[0].bounds.x();
        assert!(
            first_x > 40.0,
            "first paragraph line should be right-aligned by text-align-last"
        );
        assert!(
            (first_x - second_x).abs() < 1.0,
            "both paragraphs' last lines should align the same way"
        );
    }

    #[test]
    fn text_align_last_justify_without_spaces_falls_back_to_start() {
        let mut root_style = ComputedStyle::default();
        root_style.font_size = 16.0;
        root_style.text_align_last = crate::style::types::TextAlignLast::Justify;
        root_style.text_align = TextAlign::Justify;
        root_style.white_space = WhiteSpace::PreWrap;

        let mut text_style = ComputedStyle::default();
        text_style.white_space = WhiteSpace::PreWrap;
        let root = BoxNode::new_block(
            Arc::new(root_style),
            FormattingContextType::Block,
            vec![BoxNode::new_text(Arc::new(text_style), "word\nword".to_string())],
        );
        let constraints = LayoutConstraints::definite_width(120.0);

        let ifc = InlineFormattingContext::new();
        let fragment = ifc.layout(&root, &constraints).expect("layout");
        assert_eq!(fragment.children.len(), 2, "two lines expected");
        let first_x = fragment.children[0].children[0].bounds.x();
        let second_x = fragment.children[1].children[0].bounds.x();
        assert!(
            first_x < 1.0 && second_x < 1.0,
            "justify-last with no expansion opportunities should fall back to start alignment"
        );
    }

    #[test]
    fn text_indent_shifts_first_line_start() {
        let mut root_style = ComputedStyle::default();
        root_style.text_indent.length = Length::px(10.0);
        root_style.font_size = 16.0;
        let mut text_style = ComputedStyle::default();
        text_style.white_space = WhiteSpace::PreWrap;
        let root = BoxNode::new_block(
            Arc::new(root_style),
            FormattingContextType::Block,
            vec![BoxNode::new_text(Arc::new(text_style), "first\nsecond".to_string())],
        );
        let constraints = LayoutConstraints::definite_width(80.0);

        let ifc = InlineFormattingContext::new();
        let fragment = ifc.layout(&root, &constraints).expect("layout");
        assert!(
            fragment.children.len() >= 2,
            "indent + width should force wrapping into multiple lines"
        );
        let first_line = &fragment.children[0];
        let second_line = &fragment.children[1];
        let first_child = first_line.children.first().expect("first line text");
        let second_child = second_line.children.first().expect("second line text");
        assert!(
            first_child.bounds.x() > second_child.bounds.x(),
            "first line should be indented relative to subsequent lines"
        );
    }

    #[test]
    fn text_indent_each_line_applies_to_following_lines() {
        let mut root_style = ComputedStyle::default();
        root_style.text_indent.length = Length::px(8.0);
        root_style.text_indent.each_line = true;
        root_style.font_size = 16.0;
        let mut text_style = ComputedStyle::default();
        text_style.white_space = WhiteSpace::PreWrap;
        let root = BoxNode::new_block(
            Arc::new(root_style),
            FormattingContextType::Block,
            vec![BoxNode::new_text(
                Arc::new(text_style),
                "line one\nline two".to_string(),
            )],
        );
        let constraints = LayoutConstraints::definite_width(80.0);

        let ifc = InlineFormattingContext::new();
        let fragment = ifc.layout(&root, &constraints).expect("layout");
        assert!(fragment.children.len() >= 2, "should wrap into multiple lines");
        for line in &fragment.children {
            let first_child = line.children.first().expect("text fragment");
            assert!(
                first_child.bounds.x() >= 7.0,
                "each-line indent should shift every line start"
            );
        }
    }

    #[test]
    fn text_indent_hanging_skips_first_line() {
        let mut root_style = ComputedStyle::default();
        root_style.text_indent.length = Length::px(10.0);
        root_style.text_indent.hanging = true;
        root_style.font_size = 16.0;
        let mut text_style = ComputedStyle::default();
        text_style.white_space = WhiteSpace::PreWrap;
        let root = BoxNode::new_block(
            Arc::new(root_style),
            FormattingContextType::Block,
            vec![BoxNode::new_text(Arc::new(text_style), "first\nsecond".to_string())],
        );
        let constraints = LayoutConstraints::definite_width(80.0);

        let ifc = InlineFormattingContext::new();
        let fragment = ifc.layout(&root, &constraints).expect("layout");
        assert!(fragment.children.len() >= 2, "two lines expected");
        let first_x = fragment.children[0].children[0].bounds.x();
        let second_x = fragment.children[1].children[0].bounds.x();
        assert!(first_x < 1.0, "first line should not be indented under hanging");
        assert!(second_x >= 9.0, "subsequent lines should be indented under hanging");
    }

    #[test]
    fn text_indent_hanging_each_line_indents_all_lines() {
        let mut root_style = ComputedStyle::default();
        root_style.text_indent.length = Length::px(12.0);
        root_style.text_indent.hanging = true;
        root_style.text_indent.each_line = true;
        root_style.font_size = 16.0;
        let mut text_style = ComputedStyle::default();
        text_style.white_space = WhiteSpace::PreWrap;
        let root = BoxNode::new_block(
            Arc::new(root_style),
            FormattingContextType::Block,
            vec![BoxNode::new_text(Arc::new(text_style), "first\nsecond".to_string())],
        );
        let constraints = LayoutConstraints::definite_width(80.0);

        let ifc = InlineFormattingContext::new();
        let fragment = ifc.layout(&root, &constraints).expect("layout");
        assert!(fragment.children.len() >= 2, "two lines expected");
        let first_x = fragment.children[0].children[0].bounds.x();
        let second_x = fragment.children[1].children[0].bounds.x();
        assert!(
            first_x >= 11.0,
            "first line should be indented when each-line is present alongside hanging"
        );
        assert!(
            second_x >= 11.0,
            "subsequent lines should be indented under hanging + each-line"
        );
    }

    #[test]
    fn negative_text_indent_does_not_expand_line_width() {
        let mut root_style = ComputedStyle::default();
        root_style.text_indent.length = Length::px(-20.0);
        root_style.font_size = 16.0;
        let mut text_style = ComputedStyle::default();
        text_style.white_space = WhiteSpace::PreWrap;
        let root = BoxNode::new_block(
            Arc::new(root_style),
            FormattingContextType::Block,
            vec![BoxNode::new_text(
                Arc::new(text_style),
                "averyverylongwordthatshouldwrapproperly averyverylongwordthatshouldwrapproperly".to_string(),
            )],
        );
        // With a narrow width and long words, negative indent must not expand the
        // available width; expect wrap into two lines.
        let constraints = LayoutConstraints::definite_width(30.0);

        let ifc = InlineFormattingContext::new();
        let fragment = ifc.layout(&root, &constraints).expect("layout");
        assert!(
            fragment.children.len() >= 2,
            "negative indent should not increase available width enough to prevent wrapping"
        );
    }

    #[test]
    fn text_transform_uppercase_applies_before_layout() {
        let mut style = ComputedStyle::default();
        style.text_transform = TextTransform::Uppercase;
        let text = "Hello World";

        let ifc = InlineFormattingContext::new();
        let node = BoxNode::new_text(Arc::new(style), text.to_string());
        let item = ifc.create_text_item(&node, text).unwrap();

        assert_eq!(item.text, "HELLO WORLD");
    }

    #[test]
    fn text_transform_capitalize_only_leading_letters() {
        let mut style = ComputedStyle::default();
        style.text_transform = TextTransform::Capitalize;
        let text = "foo bar";

        let ifc = InlineFormattingContext::new();
        let node = BoxNode::new_text(Arc::new(style), text.to_string());
        let item = ifc.create_text_item(&node, text).unwrap();

        assert_eq!(item.text, "Foo Bar");
    }

    #[test]
    fn text_transform_full_width_widens_ascii_but_keeps_collapsed_space_narrow() {
        let mut style = ComputedStyle::default();
        style.text_transform = TextTransform::FullWidth;
        style.white_space = WhiteSpace::Normal;
        let text = "a b";

        let ifc = InlineFormattingContext::new();
        let node = BoxNode::new_text(Arc::new(style), text.to_string());
        let item = ifc.create_text_item(&node, text).unwrap();

        assert_eq!(item.text, "\u{FF41}\u{0020}\u{FF42}");
    }

    #[test]
    fn text_transform_full_width_converts_preserved_space_to_ideographic() {
        let mut style = ComputedStyle::default();
        style.text_transform = TextTransform::FullWidth;
        style.white_space = WhiteSpace::Pre;
        let text = "a b";

        let ifc = InlineFormattingContext::new();
        let node = BoxNode::new_text(Arc::new(style), text.to_string());
        let item = ifc.create_text_item(&node, text).unwrap();

        assert_eq!(item.text, "\u{FF41}\u{3000}\u{FF42}");
    }

    #[test]
    fn text_transform_full_width_maps_halfwidth_katakana() {
        let mut style = ComputedStyle::default();
        style.text_transform = TextTransform::FullWidth;
        let text = "\u{FF76}\u{FF9E}"; // halfwidth GA

        let ifc = InlineFormattingContext::new();
        let node = BoxNode::new_text(Arc::new(style), text.to_string());
        let item = ifc.create_text_item(&node, text).unwrap();

        assert_eq!(item.text, "\u{30AB}\u{3099}");
    }

    #[test]
    fn word_break_break_all_adds_cluster_breaks() {
        let mut style = ComputedStyle::default();
        style.word_break = WordBreak::BreakAll;
        let text = "abcd";

        let ifc = InlineFormattingContext::new();
        let node = BoxNode::new_text(Arc::new(style), text.to_string());
        let item = ifc.create_text_item(&node, text).unwrap();

        assert!(item.break_opportunities.len() >= text.len());
    }

    #[test]
    fn overflow_wrap_anywhere_adds_breaks_when_wrapping_allowed() {
        let mut style = ComputedStyle::default();
        style.overflow_wrap = OverflowWrap::Anywhere;
        let text = "longword";

        let ifc = InlineFormattingContext::new();
        let node = BoxNode::new_text(Arc::new(style), text.to_string());
        let item = ifc.create_text_item(&node, text).unwrap();

        assert!(item.break_opportunities.iter().any(|b| b.byte_offset == 4));
    }

    #[test]
    fn hyphens_auto_adds_breaks() {
        let mut style = ComputedStyle::default();
        style.hyphens = HyphensMode::Auto;
        let text = "hyphenation";

        let ifc = InlineFormattingContext::new();
        let node = BoxNode::new_text(Arc::new(style), text.to_string());
        let item = ifc.create_text_item(&node, text).unwrap();

        assert!(item
            .break_opportunities
            .iter()
            .any(|b| b.byte_offset > 0 && b.byte_offset < text.len()));
    }

    #[test]
    fn min_content_treats_adjacent_text_as_single_word() {
        let ifc = InlineFormattingContext::new();
        let hel = make_text_box("Hel");
        let lo = make_text_box("lo");
        let hel_width = ifc.create_text_item(&hel, "Hel").unwrap().advance;
        let lo_width = ifc.create_text_item(&lo, "lo").unwrap().advance;

        let root = make_inline_container(vec![hel, lo]);
        let min_width = ifc
            .compute_intrinsic_inline_size(&root, IntrinsicSizingMode::MinContent)
            .unwrap();

        let expected = hel_width + lo_width;
        assert!(
            (min_width - expected).abs() < 0.5,
            "min_width={min_width}, expected={expected}"
        );
    }

    #[test]
    fn min_content_accounts_for_hyphen_width() {
        let ifc = InlineFormattingContext::new();
        let mut style = ComputedStyle::default();
        style.hyphens = HyphensMode::Auto;
        let text = "hy\u{00AD}phen";

        let node = BoxNode::new_text(Arc::new(style), text.to_string());
        let item = ifc.create_text_item(&node, text).unwrap();
        let hyphen_break = item
            .break_opportunities
            .iter()
            .find(|b| b.adds_hyphen)
            .copied()
            .expect("expected hyphen break");

        let before = item.advance_at_offset(hyphen_break.byte_offset);
        let after = (item.advance - before).max(0.0);
        let hyphen_width = ifc.hyphen_advance(&item.style);
        let expected = (before + hyphen_width).max(after);

        let root = make_inline_container(vec![node]);
        let min_width = ifc
            .compute_intrinsic_inline_size(&root, IntrinsicSizingMode::MinContent)
            .unwrap();

        assert!(
            (min_width - expected).abs() < 0.25,
            "min_width={min_width}, expected={expected}"
        );
    }

    #[test]
    fn max_content_respects_mandatory_breaks() {
        let ifc = InlineFormattingContext::new();
        let mut style = ComputedStyle::default();
        style.white_space = WhiteSpace::Pre;
        let style = Arc::new(style);
        let text = "foo\nbar";

        let node = BoxNode::new_text(style.clone(), text.to_string());
        let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![node.clone()]);

        let item = ifc.create_text_item(&node, text).unwrap();
        let width_foo = item.advance_at_offset(3);
        let width_bar = (item.advance_at_offset(item.text.len()) - item.advance_at_offset(3)).max(0.0);
        let expected = width_foo.max(width_bar);

        let max_width = ifc
            .compute_intrinsic_inline_size(&root, IntrinsicSizingMode::MaxContent)
            .unwrap();

        assert!(
            (max_width - expected).abs() < 0.25,
            "max_width={max_width}, expected={expected}"
        );
    }
}
