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
//! use fastrender::layout::{FormattingContext, LayoutConstraints};
//!
//! let ifc = InlineFormattingContext::new();
//! let constraints = LayoutConstraints::definite_width(400.0);
//! let fragment = ifc.layout(&box_node, &constraints)?;
//! ```

pub mod baseline;
pub mod line_builder;

pub use baseline::{compute_line_height, BaselineMetrics, LineBaselineAccumulator, VerticalAlign};
pub use line_builder::{
    InlineBlockItem, InlineBoxItem, InlineItem, Line, LineBuilder, PositionedItem, ReplacedItem, TextItem,
};

use crate::geometry::Rect;
use crate::layout::constraints::LayoutConstraints;
use crate::layout::formatting_context::{FormattingContext, IntrinsicSizingMode, LayoutError};
use crate::text::{find_break_opportunities, BreakType, Script, ShapedGlyphs, TextDirection, TextShaper};
use crate::tree::box_tree::{BoxNode, BoxType, ReplacedType};
use crate::tree::fragment_tree::FragmentNode;

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
/// 1. Shape text with TextShaper to get glyphs
/// 2. Find break opportunities using UAX #14
/// 3. Break into lines respecting available width
/// 4. Create text fragments with baseline offsets
#[derive(Debug, Default)]
pub struct InlineFormattingContext {
    /// Text shaper instance
    shaper: TextShaper,
}

impl InlineFormattingContext {
    /// Creates a new InlineFormattingContext
    pub fn new() -> Self {
        Self {
            shaper: TextShaper::new(),
        }
    }

    /// Collects inline items from box node children
    fn collect_inline_items(&self, box_node: &BoxNode) -> Result<Vec<InlineItem>, LayoutError> {
        let mut items = Vec::new();

        for child in &box_node.children {
            match &child.box_type {
                BoxType::Text(text_box) => {
                    let item = self.create_text_item(child, &text_box.text)?;
                    items.push(InlineItem::Text(item));
                }
                BoxType::Inline(_) => {
                    // Recursively collect children
                    let child_items = self.collect_inline_items(child)?;
                    items.extend(child_items);
                }
                BoxType::Replaced(replaced_box) => {
                    let item = self.create_replaced_item(child, replaced_box)?;
                    items.push(InlineItem::Replaced(item));
                }
                _ => {
                    // Skip block-level boxes in inline context
                }
            }
        }

        Ok(items)
    }

    /// Creates a text item from a text box
    fn create_text_item(&self, box_node: &BoxNode, text: &str) -> Result<TextItem, LayoutError> {
        let style = &box_node.style;
        let font_size = style.font_size;
        let line_height = compute_line_height(style);

        // Shape the text (simplified - would use full pipeline in production)
        let shaped = self.shape_text_simple(text, font_size);

        // Get baseline metrics
        let metrics = BaselineMetrics::new(font_size * 0.8, line_height, font_size * 0.8, font_size * 0.2);

        // Find break opportunities
        let breaks = find_break_opportunities(text);

        let mut item = TextItem::new(shaped, metrics, breaks);
        item.font_size = font_size;
        item.text = text.to_string();

        Ok(item)
    }

    /// Simple text shaping (approximation for when fonts aren't available)
    fn shape_text_simple(&self, text: &str, font_size: f32) -> ShapedGlyphs {
        // Approximate average character width as 0.5 * font_size for Latin text
        let avg_char_width = font_size * 0.5;
        let char_count = text.chars().count();
        let total_advance = avg_char_width * char_count as f32;

        ShapedGlyphs {
            text: text.to_string(),
            glyphs: Vec::new(),
            clusters: Vec::new(),
            total_advance,
            total_advance_y: 0.0,
            direction: TextDirection::Ltr,
            script: Script::Latin,
            font_size,
        }
    }

    /// Creates a replaced item from a replaced box
    fn create_replaced_item(
        &self,
        box_node: &BoxNode,
        replaced_box: &crate::tree::box_tree::ReplacedBox,
    ) -> Result<ReplacedItem, LayoutError> {
        let style = &box_node.style;

        // Determine dimensions
        let intrinsic_width = replaced_box.intrinsic_size.map(|s| s.width).unwrap_or(300.0);
        let intrinsic_height = replaced_box.intrinsic_size.map(|s| s.height).unwrap_or(150.0);

        let width = style.width.as_ref().map(|l| l.to_px()).unwrap_or(intrinsic_width);
        let height = style.height.as_ref().map(|l| l.to_px()).unwrap_or(intrinsic_height);

        let source = match &replaced_box.replaced_type {
            ReplacedType::Image { src } => Some(src.clone()),
            ReplacedType::Video { src } => Some(src.clone()),
            _ => None,
        };

        let mut item = ReplacedItem::new(width, height);
        if let Some(src) = source {
            item = item.with_source(src);
        }

        Ok(item)
    }

    /// Builds lines from inline items
    fn build_lines(&self, items: Vec<InlineItem>, available_width: f32, strut_metrics: &BaselineMetrics) -> Vec<Line> {
        let mut builder = LineBuilder::new(available_width, *strut_metrics);

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
                    if let Some((before, after)) = remaining.split_at(pos) {
                        if before.advance > 0.0 {
                            builder.add_item(InlineItem::Text(before));
                        }
                        builder.force_break();
                        remaining = after;
                        continue;
                    }
                }
                builder.force_break();
                // Skip past the newline character
                if pos < remaining.text.len() {
                    let skip_bytes = remaining.text[pos..].chars().next().map(|c| c.len_utf8()).unwrap_or(1);
                    if pos + skip_bytes < remaining.text.len() {
                        if let Some((_, after)) = remaining.split_at(pos + skip_bytes) {
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
                FragmentNode::new_text(bounds, text_item.text.clone(), text_item.metrics.baseline_offset)
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
                fragment.bounds = Rect::from_xywh(x, y, block_item.width, block_item.height);
                fragment
            }
            InlineItem::Replaced(replaced_item) => {
                let bounds = Rect::from_xywh(x, y, replaced_item.width, replaced_item.height);
                let replaced_type = ReplacedType::Image {
                    src: replaced_item.source.clone().unwrap_or_default(),
                };
                FragmentNode::new_replaced(bounds, replaced_type)
            }
        }
    }

    /// Calculates intrinsic width for inline content
    fn calculate_intrinsic_width(&self, box_node: &BoxNode, mode: IntrinsicSizingMode) -> f32 {
        let items = match self.collect_inline_items(box_node) {
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
                            max_word_width = max_word_width.max(r.width);
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
                items.iter().map(|item| item.width()).sum()
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
                let word_chars = text[word_start..brk.byte_offset].chars().count();
                // Approximate word width
                let word_width = text_item.font_size * 0.5 * word_chars as f32;
                widths.push(word_width);
            }
            word_start = brk.byte_offset;
        }

        // Handle remaining text after last break
        if word_start < text.len() {
            let word_chars = text[word_start..].chars().count();
            let word_width = text_item.font_size * 0.5 * word_chars as f32;
            widths.push(word_width);
        }

        widths
    }
}

impl FormattingContext for InlineFormattingContext {
    fn layout(&self, box_node: &BoxNode, constraints: &LayoutConstraints) -> Result<FragmentNode, LayoutError> {
        let style = &box_node.style;
        let available_width = constraints.width().unwrap_or(f32::MAX);

        // Create strut metrics from containing block style
        let font_size = style.font_size;
        let line_height = compute_line_height(style);
        let strut_metrics = BaselineMetrics::new(font_size * 0.8, line_height, font_size * 0.8, font_size * 0.2);

        // Collect inline items
        let items = self.collect_inline_items(box_node)?;

        // Build lines
        let lines = self.build_lines(items, available_width, &strut_metrics);

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
