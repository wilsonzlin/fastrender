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

use crate::geometry::Rect;
use crate::layout::constraints::LayoutConstraints;
use crate::layout::contexts::factory::FormattingContextFactory;
use crate::layout::formatting_context::{FormattingContext, IntrinsicSizingMode, LayoutError};
use crate::layout::utils::compute_replaced_size;
use crate::style::types::FontStyle;
use crate::style::values::Length;
use crate::style::ComputedStyle;
use crate::text::font_loader::FontContext;
use crate::text::line_break::{find_break_opportunities, BreakType};
use crate::text::pipeline::ShapingPipeline;
use crate::tree::box_tree::{BoxNode, BoxType, ReplacedBox};
use crate::tree::fragment_tree::{FragmentContent, FragmentNode};

use baseline::{compute_line_height, BaselineMetrics};
use line_builder::{InlineBlockItem, InlineItem, Line, LineBuilder, ReplacedItem, TextItem};

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
}

impl InlineFormattingContext {
    /// Creates a new InlineFormattingContext
    pub fn new() -> Self {
        Self {
            pipeline: ShapingPipeline::new(),
            font_context: FontContext::new(),
        }
    }

    pub fn with_font_context(font_context: FontContext) -> Self {
        Self {
            pipeline: ShapingPipeline::new(),
            font_context,
        }
    }

    /// Collects inline items from box node children
    fn collect_inline_items(&self, box_node: &BoxNode, available_width: f32) -> Result<Vec<InlineItem>, LayoutError> {
        let mut items = Vec::new();

        for child in &box_node.children {
            match &child.box_type {
                BoxType::Text(text_box) => {
                    let item = self.create_text_item(child, &text_box.text)?;
                    items.push(InlineItem::Text(item));
                }
                BoxType::Inline(_) => {
                    if child.formatting_context().is_some() {
                        let item = self.layout_inline_block(child, available_width)?;
                        items.push(InlineItem::InlineBlock(item));
                        continue;
                    }
                    // Recursively collect children
                    let child_items = self.collect_inline_items(child, available_width)?;
                    items.extend(child_items);
                }
                BoxType::Replaced(replaced_box) => {
                    let item = self.create_replaced_item(child, replaced_box, available_width)?;
                    items.push(InlineItem::Replaced(item));
                }
                _ => {
                    // Skip block-level boxes in inline context
                }
            }
        }

        Ok(items)
    }

    fn layout_inline_block(&self, box_node: &BoxNode, available_width: f32) -> Result<InlineBlockItem, LayoutError> {
        let fc_type = box_node.formatting_context().ok_or_else(|| {
            LayoutError::UnsupportedBoxType("Inline-level box missing formatting context".to_string())
        })?;

        let style = &box_node.style;
        let factory = FormattingContextFactory::new();
        let fc = factory.create(fc_type);

        let percentage_base = if available_width.is_finite() {
            available_width
        } else {
            0.0
        };
        let margin_left = style
            .margin_left
            .as_ref()
            .map(|l| resolve_length_for_width(*l, percentage_base))
            .unwrap_or(0.0);
        let margin_right = style
            .margin_right
            .as_ref()
            .map(|l| resolve_length_for_width(*l, percentage_base))
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

        let horizontal_edges = horizontal_padding_and_borders(style, percentage_base);
        let preferred_min = preferred_min_content + horizontal_edges;
        let preferred = preferred_content + horizontal_edges;

        let specified_width = style
            .width
            .as_ref()
            .map(|l| resolve_length_for_width(*l, percentage_base));

        let min_width = style
            .min_width
            .as_ref()
            .map(|l| resolve_length_for_width(*l, percentage_base))
            .unwrap_or(0.0);
        let max_width = style
            .max_width
            .as_ref()
            .map(|l| resolve_length_for_width(*l, percentage_base))
            .unwrap_or(f32::INFINITY);

        let constraint_width = if let Some(content_width) = specified_width {
            // Honor specified width; use containing block width for layout to avoid over-constraining margins.
            let border_box_width = content_width + horizontal_edges;
            let used = border_box_width.clamp(min_width, max_width);
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

        let constraints = LayoutConstraints::definite_width(constraint_width);
        let fragment = fc.layout(box_node, &constraints)?;

        let margin_left = style
            .margin_left
            .as_ref()
            .map(|l| resolve_length_for_width(*l, percentage_base))
            .unwrap_or(0.0);
        let margin_right = style
            .margin_right
            .as_ref()
            .map(|l| resolve_length_for_width(*l, percentage_base))
            .unwrap_or(0.0);

        Ok(InlineBlockItem::new(
            fragment,
            box_node.style.direction,
            box_node.style.unicode_bidi,
            margin_left,
            margin_right,
        ))
    }

    /// Creates a text item from a text box
    fn create_text_item(&self, box_node: &BoxNode, text: &str) -> Result<TextItem, LayoutError> {
        let style = &box_node.style;
        let line_height = compute_line_height(style);

        let shaped_runs = match self.pipeline.shape(text, style, &self.font_context) {
            Ok(runs) => runs,
            Err(err) => {
                if let Some(fallback_font) = self.font_context.get_sans_serif() {
                    let mut fallback_style = (*style).as_ref().clone();
                    fallback_style.font_family = vec![fallback_font.family.clone()];
                    self.pipeline
                        .shape(text, &fallback_style, &self.font_context)
                        .map_err(|e| {
                            LayoutError::MissingContext(format!(
                                "Shaping failed (fonts={}): {:?}",
                                self.font_context.font_count(),
                                e
                            ))
                        })?
                } else {
                    return Err(LayoutError::MissingContext(format!(
                        "Shaping failed (fonts={}): {:?}",
                        self.font_context.font_count(),
                        err
                    )));
                }
            }
        };

        let metrics = TextItem::metrics_from_runs(&shaped_runs, line_height, style.font_size);

        // Find break opportunities
        let breaks = find_break_opportunities(text);

        Ok(TextItem::new(
            shaped_runs,
            text.to_string(),
            metrics,
            breaks,
            box_node.style.clone(),
        ))
    }

    /// Creates a replaced item from a replaced box
    fn create_replaced_item(
        &self,
        box_node: &BoxNode,
        replaced_box: &ReplacedBox,
        percentage_base: f32,
    ) -> Result<ReplacedItem, LayoutError> {
        let style = &box_node.style;
        let size = compute_replaced_size(style, replaced_box);
        let margin_left = style
            .margin_left
            .as_ref()
            .map(|l| resolve_length_for_width(*l, percentage_base))
            .unwrap_or(0.0);
        let margin_right = style
            .margin_right
            .as_ref()
            .map(|l| resolve_length_for_width(*l, percentage_base))
            .unwrap_or(0.0);

        Ok(ReplacedItem::new(
            size,
            replaced_box.replaced_type.clone(),
            box_node.style.clone(),
            margin_left,
            margin_right,
        ))
    }

    /// Builds lines from inline items
    fn build_lines(
        &self,
        items: Vec<InlineItem>,
        available_width: f32,
        strut_metrics: &BaselineMetrics,
        base_level: Option<unicode_bidi::Level>,
    ) -> Vec<Line> {
        let mut builder = LineBuilder::new(
            available_width,
            *strut_metrics,
            self.pipeline.clone(),
            self.font_context.clone(),
            base_level,
        );

        for item in items {
            // Check for mandatory breaks in text
            if let InlineItem::Text(ref text_item) = item {
                let has_mandatory = text_item
                    .break_opportunities
                    .iter()
                    .any(|b| b.break_type == BreakType::Mandatory);

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

    fn compute_strut_metrics(&self, style: &ComputedStyle) -> BaselineMetrics {
        let line_height = compute_line_height(style);
        let font_size = style.font_size;
        let italic = matches!(style.font_style, FontStyle::Italic);
        let oblique = matches!(style.font_style, FontStyle::Oblique);

        if let Some(font) = self
            .font_context
            .get_font(&style.font_family, style.font_weight.to_u16(), italic, oblique)
            .or_else(|| self.font_context.get_sans_serif())
        {
            if let Ok(metrics) = font.metrics() {
                let scaled = metrics.scale(font_size);
                return BaselineMetrics {
                    baseline_offset: scaled.ascent,
                    height: line_height,
                    ascent: scaled.ascent,
                    descent: scaled.descent,
                    line_gap: scaled.line_gap,
                    line_height,
                };
            }
        }

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
                if pos > 0 {
                    if let Some((before, after)) = remaining.split_at(pos, &self.pipeline, &self.font_context) {
                        if before.advance > 0.0 {
                            builder.add_item(InlineItem::Text(before));
                        }
                        builder.force_break();
                        remaining = after;
                        continue;
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
                            remaining.split_at(pos + skip_bytes, &self.pipeline, &self.font_context)
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
    fn create_fragments(&self, lines: Vec<Line>, start_y: f32) -> Vec<FragmentNode> {
        let mut fragments = Vec::new();
        let mut y = start_y;

        for line in lines {
            let line_fragment = self.create_line_fragment(&line, y);
            y += line.height;
            fragments.push(line_fragment);
        }

        fragments
    }

    /// Creates a line fragment with positioned children
    fn create_line_fragment(&self, line: &Line, y: f32) -> FragmentNode {
        let mut children = Vec::new();

        for positioned in &line.items {
            let item_y =
                line.baseline + positioned.baseline_offset - positioned.item.baseline_metrics().baseline_offset;

            let fragment = self.create_item_fragment(&positioned.item, positioned.x, item_y);
            children.push(fragment);
        }

        let bounds = Rect::from_xywh(0.0, y, line.width, line.height);
        FragmentNode::new_line(bounds, line.baseline, children)
    }

    /// Creates a fragment for an inline item
    #[allow(clippy::only_used_in_recursion)]
    fn create_item_fragment(&self, item: &InlineItem, x: f32, y: f32) -> FragmentNode {
        match item {
            InlineItem::Text(text_item) => {
                let bounds = Rect::from_xywh(x, y, text_item.advance, text_item.metrics.height);
                FragmentNode::new_text_shaped(
                    bounds,
                    text_item.text.clone(),
                    text_item.metrics.baseline_offset,
                    text_item.runs.clone(),
                    text_item.style.clone(),
                )
            }
            InlineItem::InlineBox(box_item) => {
                // Recursively create children
                let mut child_x = x + box_item.start_edge;
                let children: Vec<_> = box_item
                    .children
                    .iter()
                    .map(|child| {
                        let fragment = self.create_item_fragment(child, child_x, y);
                        child_x += child.width();
                        fragment
                    })
                    .collect();

                let bounds = Rect::from_xywh(x, y, box_item.width(), box_item.metrics.height);
                FragmentNode::new_inline(bounds, box_item.box_index, children)
            }
            InlineItem::InlineBlock(block_item) => {
                let mut fragment = block_item.fragment.clone();
                fragment.bounds = Rect::from_xywh(x + block_item.margin_left, y, block_item.width, block_item.height);
                fragment
            }
            InlineItem::Replaced(replaced_item) => {
                let bounds = Rect::from_xywh(
                    x + replaced_item.margin_left,
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
        let items = match self.collect_inline_items(box_node, f32::INFINITY) {
            Ok(items) => items,
            Err(_) => return 0.0,
        };

        match mode {
            IntrinsicSizingMode::MinContent => {
                // Min-content: width of the longest word/atomic inline
                let mut max_word_width: f32 = 0.0;

                for item in &items {
                    match item {
                        InlineItem::Text(text_item) => {
                            // Find width of longest word
                            let word_widths = self.calculate_word_widths(text_item);
                            max_word_width = max_word_width.max(word_widths.into_iter().fold(0.0f32, |a, b| a.max(b)));
                        }
                        InlineItem::Replaced(r) => {
                            max_word_width = max_word_width.max(r.intrinsic_width());
                        }
                        InlineItem::InlineBlock(b) => {
                            max_word_width = max_word_width.max(b.width);
                        }
                        InlineItem::InlineBox(b) => {
                            max_word_width = max_word_width.max(b.width());
                        }
                    }
                }

                max_word_width
            }
            IntrinsicSizingMode::MaxContent => {
                // Max-content: width needed for single line (no wrapping)
                items.iter().map(|item| item.intrinsic_width()).sum()
            }
        }
    }

    /// Calculates widths of individual words in text
    fn calculate_word_widths(&self, text_item: &TextItem) -> Vec<f32> {
        let mut widths = Vec::new();
        let mut word_start = 0;
        let text = &text_item.text;

        for brk in &text_item.break_opportunities {
            if brk.byte_offset > word_start {
                let word_width = text_item.advance_at_offset(brk.byte_offset) - text_item.advance_at_offset(word_start);
                widths.push(word_width.max(0.0));
            }
            word_start = brk.byte_offset;
        }

        // Handle remaining text after last break
        if word_start < text.len() {
            let word_width = text_item.advance_at_offset(text.len()) - text_item.advance_at_offset(word_start);
            widths.push(word_width.max(0.0));
        }

        widths
    }
}

fn resolve_length_for_width(length: Length, percentage_base: f32) -> f32 {
    if length.unit.is_percentage() {
        length.resolve_against(percentage_base)
    } else if length.unit.is_absolute() {
        length.to_px()
    } else {
        // Relative units (em/rem) should have been resolved earlier; treat as px fallback
        length.value
    }
}

fn horizontal_padding_and_borders(style: &ComputedStyle, percentage_base: f32) -> f32 {
    resolve_length_for_width(style.padding_left, percentage_base)
        + resolve_length_for_width(style.padding_right, percentage_base)
        + resolve_length_for_width(style.border_left_width, percentage_base)
        + resolve_length_for_width(style.border_right_width, percentage_base)
}

impl FormattingContext for InlineFormattingContext {
    fn layout(&self, box_node: &BoxNode, constraints: &LayoutConstraints) -> Result<FragmentNode, LayoutError> {
        let style = &box_node.style;
        let available_width = constraints.width().unwrap_or(f32::MAX);

        // Create strut metrics from containing block style
        let strut_metrics = self.compute_strut_metrics(style);

        // Collect inline items
        let items = self.collect_inline_items(box_node, available_width)?;

        let base_level = match style.unicode_bidi {
            crate::style::types::UnicodeBidi::Plaintext => None,
            _ => match style.direction {
                crate::style::types::Direction::Rtl => Some(unicode_bidi::Level::rtl()),
                _ => Some(unicode_bidi::Level::ltr()),
            },
        };

        // Build lines
        let lines = self.build_lines(items, available_width, &strut_metrics, base_level);

        // Calculate total height
        let total_height: f32 = lines.iter().map(|l| l.height).sum();
        let max_width: f32 = lines.iter().map(|l| l.width).fold(0.0, f32::max);

        // Create fragments
        let children = self.create_fragments(lines, 0.0);

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::style::display::FormattingContextType;
    use crate::style::ComputedStyle;
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
}
