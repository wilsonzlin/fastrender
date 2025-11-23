//! Inline Formatting Context Layout
//!
//! This module implements the Inline Formatting Context (IFC) layout algorithm
//! as specified in CSS 2.1 Section 9.4.2 and CSS Inline Layout Module Level 3.
//!
//! # Inline Formatting Context
//!
//! An IFC is a layout mode where inline boxes are laid out horizontally, one after
//! another, starting at the top of the containing block. When inline boxes cannot
//! fit on a single line, they are distributed across multiple lines (line boxes).
//!
//! # Key Features
//!
//! - **Horizontal flow**: Inline boxes flow left-to-right (or RTL)
//! - **Line breaking**: Content wraps to multiple lines when necessary
//! - **Baseline alignment**: Text aligns on the baseline
//! - **Mixed content**: Text, inline boxes, and atomic inline-level boxes coexist
//!
//! # Module Structure
//!
//! - `line_builder` - Line building algorithm (collecting inline items into lines)
//! - `baseline` - Baseline alignment calculations (CSS 2.1 Section 10.8)
//!
//! Reference: <https://www.w3.org/TR/CSS21/visuren.html#inline-formatting>

mod baseline;
mod line_builder;

pub use baseline::{align_on_baseline, compute_line_baseline, ItemMetrics};
pub use line_builder::{InlineItem, Line, LineBuilder, TextRun};

use crate::geometry::{Point, Rect, Size};
use crate::layout::constraints::LayoutConstraints;
use crate::layout::formatting_context::{FormattingContext, IntrinsicSizingMode, LayoutError};
use crate::style::LineHeight;
use crate::text::FontContext;
use crate::tree::box_tree::{BoxNode, BoxType};
use crate::tree::fragment_tree::FragmentNode;
use std::sync::Arc;

/// Inline Formatting Context implementation
///
/// Implements the FormattingContext trait for inline-level layout.
/// Handles horizontal flow, line breaking, and baseline alignment.
///
/// # Usage
///
/// ```rust,ignore
/// use fastrender::layout::contexts::InlineFormattingContext;
///
/// let ifc = InlineFormattingContext::new();
/// let fragment = ifc.layout(&box_node, &constraints)?;
/// ```
#[derive(Default)]
pub struct InlineFormattingContext {
    /// Font context for text measurement
    /// When None, uses estimated widths
    font_context: Option<Arc<FontContext>>,
}

impl std::fmt::Debug for InlineFormattingContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("InlineFormattingContext")
            .field("font_context", &self.font_context.as_ref().map(|_| "FontContext"))
            .finish()
    }
}

impl InlineFormattingContext {
    /// Creates a new InlineFormattingContext without font context
    ///
    /// Text measurement will use estimated widths based on font size.
    pub fn new() -> Self {
        Self { font_context: None }
    }

    /// Creates a new InlineFormattingContext with font context
    ///
    /// Text measurement will use actual font metrics via FontContext.
    pub fn with_font_context(font_context: Arc<FontContext>) -> Self {
        Self {
            font_context: Some(font_context),
        }
    }

    /// Builds inline items from a box node and its children
    ///
    /// Traverses the box tree and creates a flat list of inline items:
    /// - Text runs for text boxes
    /// - Inline box markers for inline elements
    /// - Atomic inline boxes for inline-block elements
    fn build_inline_items(&self, box_node: &BoxNode) -> Vec<InlineItem> {
        let mut items = Vec::new();
        self.collect_inline_items(box_node, &mut items);
        items
    }

    /// Recursively collects inline items from the box tree
    fn collect_inline_items(&self, box_node: &BoxNode, items: &mut Vec<InlineItem>) {
        match &box_node.box_type {
            BoxType::Text(text_box) => {
                let text = &text_box.text;
                if text.is_empty() {
                    return;
                }

                // Split text at whitespace boundaries for line breaking
                let font_size = box_node.style.font_size;
                let line_height = self.compute_line_height(&box_node.style.line_height, font_size);

                // Split text into words and whitespace segments
                let mut last_was_word = false;
                for segment in self.split_text_for_breaking(text) {
                    let (segment_width, baseline) = self.measure_text(&segment, font_size);

                    let text_run = TextRun {
                        text: segment,
                        width: segment_width,
                        height: line_height,
                        baseline,
                        font_size,
                        style: box_node.style.clone(),
                    };

                    items.push(InlineItem::Text(text_run));

                    // Add soft break opportunity after each word (whitespace acts as break point)
                    if last_was_word {
                        items.push(InlineItem::SoftBreakOpportunity);
                    }
                    last_was_word = true;
                }

                // Add trailing break opportunity
                if !items.is_empty() {
                    items.push(InlineItem::SoftBreakOpportunity);
                }
            }

            BoxType::Inline(_inline_box) => {
                // Mark start of inline box
                items.push(InlineItem::StartInlineBox {
                    style: box_node.style.clone(),
                });

                // Process children
                for child in &box_node.children {
                    self.collect_inline_items(child, items);
                }

                // Mark end of inline box
                items.push(InlineItem::EndInlineBox);
            }

            BoxType::Block(_) | BoxType::Anonymous(_) => {
                // For blocks that establish inline formatting context,
                // process their inline children
                for child in &box_node.children {
                    if child.is_inline_level() || child.is_text() {
                        self.collect_inline_items(child, items);
                    }
                }
            }

            BoxType::Replaced(replaced_box) => {
                // Replaced elements are atomic inline-level boxes
                let width = replaced_box.intrinsic_size.map(|s| s.width).unwrap_or(150.0);
                let height = replaced_box.intrinsic_size.map(|s| s.height).unwrap_or(150.0);

                items.push(InlineItem::Atomic {
                    width,
                    height,
                    baseline: height, // Bottom edge aligns to baseline by default
                    style: box_node.style.clone(),
                });
            }
        }
    }

    /// Splits text at whitespace boundaries for line breaking
    ///
    /// Returns an iterator over text segments that can be broken between.
    /// Whitespace is included as part of segments to preserve spacing.
    fn split_text_for_breaking(&self, text: &str) -> Vec<String> {
        let mut segments = Vec::new();
        let mut current = String::new();

        for word in text.split_inclusive(char::is_whitespace) {
            if !word.is_empty() {
                // Trim trailing whitespace for the segment, add as separate item
                let trimmed = word.trim_end();
                if !trimmed.is_empty() {
                    current.push_str(trimmed);
                    segments.push(std::mem::take(&mut current));
                }

                // Add whitespace as its own segment (for spacing)
                let ws: String = word.chars().filter(|c| c.is_whitespace()).collect();
                if !ws.is_empty() {
                    segments.push(ws);
                }
            }
        }

        // Handle any remaining text
        if !current.is_empty() {
            segments.push(current);
        }

        segments
    }

    /// Measures text and returns (width, baseline)
    fn measure_text(&self, text: &str, font_size: f32) -> (f32, f32) {
        if let Some(ref font_ctx) = self.font_context {
            // Use actual font metrics
            if let Some(font) = font_ctx.get_sans_serif() {
                let width = font_ctx.measure_text(text, &font, font_size);
                // Baseline is approximately 80% of font size (typical for Latin fonts)
                let baseline = font_size * 0.8;
                return (width, baseline);
            }
        }

        // Fallback: estimate based on font size
        // Average character width is approximately 0.5 * font_size
        let width = text.len() as f32 * font_size * 0.5;
        let baseline = font_size * 0.8;
        (width, baseline)
    }

    /// Computes line height from LineHeight enum and font size
    fn compute_line_height(&self, line_height: &LineHeight, font_size: f32) -> f32 {
        match line_height {
            LineHeight::Normal => font_size * 1.2,
            LineHeight::Number(n) => font_size * n,
            LineHeight::Length(len) => len.to_px(),
        }
    }

    /// Breaks inline items into lines based on available width
    fn break_into_lines(&self, items: &[InlineItem], available_width: f32) -> Vec<Line> {
        let mut line_builder = LineBuilder::new(available_width);

        for item in items {
            line_builder.add_item(item.clone());
        }

        line_builder.finish()
    }

    /// Performs baseline alignment and creates positioned fragments
    fn align_and_position_lines(&self, lines: Vec<Line>) -> (Vec<FragmentNode>, f32) {
        let mut fragments = Vec::new();
        let mut current_y: f32 = 0.0;

        for line in lines {
            // Compute line metrics
            let (line_height, baseline) = self.compute_line_metrics(&line);

            // Create fragments for items on this line
            let (line_fragments, _line_width) = self.create_line_fragments(&line, current_y, line_height, baseline);

            // Create line box fragment
            let line_box = FragmentNode::new_line(
                Rect::from_xywh(0.0, current_y, line.width, line_height),
                baseline,
                line_fragments,
            );

            fragments.push(line_box);
            current_y += line_height;
        }

        (fragments, current_y)
    }

    /// Computes line height and baseline position for a line
    fn compute_line_metrics(&self, line: &Line) -> (f32, f32) {
        let mut max_height: f32 = 0.0;
        let mut max_ascent: f32 = 0.0;
        let mut max_descent: f32 = 0.0;

        for item in &line.items {
            match item {
                InlineItem::Text(text_run) => {
                    let ascent = text_run.baseline;
                    let descent = text_run.height - text_run.baseline;
                    max_ascent = max_ascent.max(ascent);
                    max_descent = max_descent.max(descent);
                    max_height = max_height.max(text_run.height);
                }
                InlineItem::Atomic { height, baseline, .. } => {
                    let ascent = *baseline;
                    let descent = height - baseline;
                    max_ascent = max_ascent.max(ascent);
                    max_descent = max_descent.max(descent);
                    max_height = max_height.max(*height);
                }
                _ => {}
            }
        }

        // Line height is max of all items' heights, or calculated from ascent + descent
        let line_height = max_height.max(max_ascent + max_descent);
        // Baseline position from top of line box
        let baseline = max_ascent;

        (line_height, baseline)
    }

    /// Creates fragments for items on a line
    fn create_line_fragments(
        &self,
        line: &Line,
        line_y: f32,
        _line_height: f32,
        line_baseline: f32,
    ) -> (Vec<FragmentNode>, f32) {
        let mut fragments = Vec::new();
        let mut current_x: f32 = 0.0;

        for item in &line.items {
            match item {
                InlineItem::Text(text_run) => {
                    // Align text baseline to line baseline
                    let y_offset = line_baseline - text_run.baseline;
                    let bounds = Rect::from_xywh(current_x, line_y + y_offset, text_run.width, text_run.height);

                    let fragment = FragmentNode::new_text(bounds, text_run.text.clone(), text_run.baseline);

                    fragments.push(fragment);
                    current_x += text_run.width;
                }
                InlineItem::Atomic {
                    width,
                    height,
                    baseline,
                    ..
                } => {
                    // Align bottom edge to baseline (default vertical-align: baseline for replaced)
                    let y_offset = line_baseline - baseline;
                    let bounds = Rect::from_xywh(current_x, line_y + y_offset, *width, *height);

                    let fragment = FragmentNode::new_inline(bounds, 0, vec![]);
                    fragments.push(fragment);
                    current_x += width;
                }
                InlineItem::StartInlineBox { .. } | InlineItem::EndInlineBox => {
                    // Inline box boundaries don't have width themselves
                    // They affect styling but not positioning in this simple implementation
                }
                InlineItem::SoftBreakOpportunity | InlineItem::HardBreak => {
                    // Break opportunities don't have width
                }
            }
        }

        (fragments, current_x)
    }
}

impl FormattingContext for InlineFormattingContext {
    /// Lays out inline content within a containing block
    ///
    /// # Algorithm
    ///
    /// 1. Build flat list of inline items from box tree
    /// 2. Break items into lines based on available width
    /// 3. Perform baseline alignment within each line
    /// 4. Create fragment tree with positioned line boxes
    fn layout(&self, box_node: &BoxNode, constraints: &LayoutConstraints) -> Result<FragmentNode, LayoutError> {
        // Get available width from constraints
        let available_width = constraints
            .width()
            .ok_or_else(|| LayoutError::MissingContext("Inline layout requires definite width".to_string()))?;

        // Phase 1: Build inline items
        let inline_items = self.build_inline_items(box_node);

        if inline_items.is_empty() {
            // Empty inline context
            return Ok(FragmentNode::new_block(
                Rect::from_xywh(0.0, 0.0, available_width, 0.0),
                vec![],
            ));
        }

        // Phase 2: Break into lines
        let lines = self.break_into_lines(&inline_items, available_width);

        // Phase 3: Align and position
        let (line_fragments, total_height) = self.align_and_position_lines(lines);

        // Phase 4: Create containing fragment
        let bounds = Rect::from_xywh(0.0, 0.0, available_width, total_height);
        Ok(FragmentNode::new_block(bounds, line_fragments))
    }

    /// Computes intrinsic size for inline content
    ///
    /// - MinContent: Width of longest word (unbreakable unit)
    /// - MaxContent: Width without any line breaks
    fn compute_intrinsic_inline_size(&self, box_node: &BoxNode, mode: IntrinsicSizingMode) -> Result<f32, LayoutError> {
        let inline_items = self.build_inline_items(box_node);

        match mode {
            IntrinsicSizingMode::MinContent => {
                // Find the width of the widest unbreakable piece
                let mut max_word_width: f32 = 0.0;
                let mut current_word_width: f32 = 0.0;

                for item in &inline_items {
                    match item {
                        InlineItem::Text(text_run) => {
                            // Split text into words and find the longest
                            for word in text_run.text.split_whitespace() {
                                let (word_width, _) = self.measure_text(word, text_run.font_size);
                                max_word_width = max_word_width.max(word_width);
                            }
                            current_word_width = 0.0;
                        }
                        InlineItem::Atomic { width, .. } => {
                            // Atomic boxes are unbreakable
                            max_word_width = max_word_width.max(*width);
                            current_word_width = 0.0;
                        }
                        InlineItem::SoftBreakOpportunity | InlineItem::HardBreak => {
                            max_word_width = max_word_width.max(current_word_width);
                            current_word_width = 0.0;
                        }
                        _ => {}
                    }
                }

                max_word_width = max_word_width.max(current_word_width);
                Ok(max_word_width)
            }
            IntrinsicSizingMode::MaxContent => {
                // Sum all content widths (no line breaks)
                let mut total_width: f32 = 0.0;

                for item in &inline_items {
                    match item {
                        InlineItem::Text(text_run) => {
                            total_width += text_run.width;
                        }
                        InlineItem::Atomic { width, .. } => {
                            total_width += width;
                        }
                        InlineItem::HardBreak => {
                            // Hard break resets to new line in max-content
                            // Keep the maximum so far
                        }
                        _ => {}
                    }
                }

                Ok(total_width)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::style::display::FormattingContextType;
    use crate::style::ComputedStyles;

    fn default_style() -> Arc<ComputedStyles> {
        Arc::new(ComputedStyles::default())
    }

    fn style_with_font_size(size: f32) -> Arc<ComputedStyles> {
        let mut style = ComputedStyles::default();
        style.font_size = size;
        Arc::new(style)
    }

    #[test]
    fn test_ifc_new() {
        let _ifc = InlineFormattingContext::new();
    }

    #[test]
    fn test_layout_empty_inline() {
        let ifc = InlineFormattingContext::new();
        let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
        let constraints = LayoutConstraints::definite(800.0, 600.0);

        let fragment = ifc.layout(&root, &constraints).unwrap();

        assert_eq!(fragment.bounds.width(), 800.0);
        assert_eq!(fragment.bounds.height(), 0.0);
    }

    #[test]
    fn test_layout_single_text() {
        let ifc = InlineFormattingContext::new();

        let text_box = BoxNode::new_text(style_with_font_size(16.0), "Hello, world!".to_string());
        let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![text_box]);

        let constraints = LayoutConstraints::definite(800.0, 600.0);
        let fragment = ifc.layout(&root, &constraints).unwrap();

        assert_eq!(fragment.bounds.width(), 800.0);
        assert!(fragment.bounds.height() > 0.0);
        assert_eq!(fragment.children.len(), 1); // One line
    }

    #[test]
    fn test_layout_line_wrapping() {
        let ifc = InlineFormattingContext::new();

        // Create text that should wrap
        let long_text =
            "This is a very long line of text that should wrap to multiple lines when the containing block is narrow";
        let text_box = BoxNode::new_text(style_with_font_size(16.0), long_text.to_string());
        let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![text_box]);

        // Narrow width to force wrapping
        let constraints = LayoutConstraints::definite(200.0, 600.0);
        let fragment = ifc.layout(&root, &constraints).unwrap();

        assert_eq!(fragment.bounds.width(), 200.0);
        // Should have multiple lines
        assert!(fragment.children.len() > 1);
    }

    #[test]
    fn test_layout_multiple_text_nodes() {
        let ifc = InlineFormattingContext::new();

        let text1 = BoxNode::new_text(style_with_font_size(16.0), "Hello ".to_string());
        let text2 = BoxNode::new_text(style_with_font_size(16.0), "World".to_string());
        let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![text1, text2]);

        let constraints = LayoutConstraints::definite(800.0, 600.0);
        let fragment = ifc.layout(&root, &constraints).unwrap();

        assert!(fragment.bounds.height() > 0.0);
    }

    #[test]
    fn test_layout_inline_box() {
        let ifc = InlineFormattingContext::new();

        let text = BoxNode::new_text(style_with_font_size(16.0), "Wrapped text".to_string());
        let inline_box = BoxNode::new_inline(default_style(), vec![text]);
        let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![inline_box]);

        let constraints = LayoutConstraints::definite(800.0, 600.0);
        let fragment = ifc.layout(&root, &constraints).unwrap();

        assert!(fragment.bounds.height() > 0.0);
    }

    #[test]
    fn test_intrinsic_min_content() {
        let ifc = InlineFormattingContext::new();

        let text_box = BoxNode::new_text(style_with_font_size(16.0), "Hello World Test".to_string());
        let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![text_box]);

        let min_width = ifc
            .compute_intrinsic_inline_size(&root, IntrinsicSizingMode::MinContent)
            .unwrap();

        // Min content should be width of longest word
        assert!(min_width > 0.0);
        assert!(min_width < 200.0); // Should be much less than full text width
    }

    #[test]
    fn test_intrinsic_max_content() {
        let ifc = InlineFormattingContext::new();

        let text_box = BoxNode::new_text(style_with_font_size(16.0), "Hello World".to_string());
        let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![text_box]);

        let max_width = ifc
            .compute_intrinsic_inline_size(&root, IntrinsicSizingMode::MaxContent)
            .unwrap();

        // Max content should be total width without wrapping
        assert!(max_width > 0.0);
    }

    #[test]
    fn test_baseline_alignment_different_sizes() {
        let ifc = InlineFormattingContext::new();

        let text1 = BoxNode::new_text(style_with_font_size(12.0), "Small ".to_string());
        let text2 = BoxNode::new_text(style_with_font_size(24.0), "Large ".to_string());
        let text3 = BoxNode::new_text(style_with_font_size(12.0), "Small".to_string());

        let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![text1, text2, text3]);

        let constraints = LayoutConstraints::definite(800.0, 600.0);
        let fragment = ifc.layout(&root, &constraints).unwrap();

        // Should have created line fragments
        assert!(!fragment.children.is_empty());

        // Line height should accommodate the largest font
        let line = &fragment.children[0];
        assert!(line.bounds.height() >= 24.0 * 1.2); // At least large font's line height
    }

    #[test]
    fn test_empty_text_box() {
        let ifc = InlineFormattingContext::new();

        let text_box = BoxNode::new_text(style_with_font_size(16.0), "".to_string());
        let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![text_box]);

        let constraints = LayoutConstraints::definite(800.0, 600.0);
        let fragment = ifc.layout(&root, &constraints).unwrap();

        // Empty text should produce no lines
        assert_eq!(fragment.bounds.height(), 0.0);
    }

    #[test]
    fn test_whitespace_only_text() {
        let ifc = InlineFormattingContext::new();

        let text_box = BoxNode::new_text(style_with_font_size(16.0), "   ".to_string());
        let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![text_box]);

        let constraints = LayoutConstraints::definite(800.0, 600.0);
        let fragment = ifc.layout(&root, &constraints).unwrap();

        // Whitespace-only text should still produce output
        // (though in a full implementation, we might collapse whitespace)
        assert!(fragment.bounds.height() >= 0.0);
    }

    #[test]
    fn test_fc_is_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<InlineFormattingContext>();
    }

    #[test]
    fn test_missing_width_constraint() {
        let ifc = InlineFormattingContext::new();
        let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
        let constraints = LayoutConstraints::indefinite();

        let result = ifc.layout(&root, &constraints);

        assert!(result.is_err());
        match result {
            Err(LayoutError::MissingContext(_)) => {}
            _ => panic!("Expected MissingContext error"),
        }
    }

    #[test]
    fn test_nested_inline_boxes() {
        let ifc = InlineFormattingContext::new();

        // Create nested inline structure: <span><span>text</span></span>
        let text = BoxNode::new_text(style_with_font_size(16.0), "Nested text".to_string());
        let inner_span = BoxNode::new_inline(default_style(), vec![text]);
        let outer_span = BoxNode::new_inline(default_style(), vec![inner_span]);
        let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![outer_span]);

        let constraints = LayoutConstraints::definite(800.0, 600.0);
        let fragment = ifc.layout(&root, &constraints).unwrap();

        assert!(fragment.bounds.height() > 0.0);
    }

    #[test]
    fn test_very_long_word() {
        let ifc = InlineFormattingContext::new();

        // Very long word that can't break
        let long_word = "Supercalifragilisticexpialidocious";
        let text_box = BoxNode::new_text(style_with_font_size(16.0), long_word.to_string());
        let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![text_box]);

        // Very narrow width
        let constraints = LayoutConstraints::definite(50.0, 600.0);
        let fragment = ifc.layout(&root, &constraints).unwrap();

        // Should still layout (word overflows but doesn't crash)
        assert!(fragment.bounds.height() > 0.0);
    }
}
