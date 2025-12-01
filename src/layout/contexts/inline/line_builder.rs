//! Line building for inline formatting context
//!
//! This module handles the construction of line boxes from inline-level content.
//! It manages line breaking, inline item accumulation, and fragment positioning.
//!
//! # CSS Specification
//!
//! CSS 2.1 Section 9.4.2 - Inline formatting contexts:
//! <https://www.w3.org/TR/CSS21/visuren.html#inline-formatting>
//!
//! # Line Box Construction
//!
//! The line builder accumulates inline items (text runs, inline boxes, replaced elements)
//! and breaks them into lines when:
//!
//! 1. A mandatory line break occurs (newline character, `<br>`)
//! 2. Content exceeds available width and a break opportunity exists
//! 3. The inline content ends
//!
//! # Algorithm Overview
//!
//! ```text
//! For each inline item:
//!   1. Measure item width
//!   2. If item fits on current line -> add to line
//!   3. If item doesn't fit:
//!      a. Find break opportunity within item (for text)
//!      b. If no break found, check if line is empty
//!      c. If line empty, force item onto line (overflow)
//!      d. Otherwise, finalize line and start new one
//!   4. After all items, finalize last line
//! ```

use super::baseline::{BaselineMetrics, LineBaselineAccumulator, VerticalAlign};
use crate::text::line_break::BreakOpportunity;
use crate::text::shaper::ShapedGlyphs;
use crate::tree::fragment_tree::FragmentNode;

/// An item in the inline formatting context
///
/// Represents different types of content that can appear inline.
#[derive(Debug, Clone)]
pub enum InlineItem {
    /// Shaped text ready for layout
    Text(TextItem),

    /// An inline box (span, a, em, etc.) with children
    InlineBox(InlineBoxItem),

    /// An inline-block box (atomic inline)
    InlineBlock(InlineBlockItem),

    /// A replaced element (img, canvas, etc.)
    Replaced(ReplacedItem),
}

impl InlineItem {
    /// Returns the width of this item
    pub fn width(&self) -> f32 {
        match self {
            InlineItem::Text(t) => t.advance,
            InlineItem::InlineBox(b) => b.width(),
            InlineItem::InlineBlock(b) => b.width,
            InlineItem::Replaced(r) => r.width,
        }
    }

    /// Returns baseline metrics for this item
    pub fn baseline_metrics(&self) -> BaselineMetrics {
        match self {
            InlineItem::Text(t) => t.metrics,
            InlineItem::InlineBox(b) => b.metrics,
            InlineItem::InlineBlock(b) => b.metrics,
            InlineItem::Replaced(r) => r.metrics,
        }
    }

    /// Returns the vertical alignment for this item
    pub fn vertical_align(&self) -> VerticalAlign {
        match self {
            InlineItem::Text(t) => t.vertical_align,
            InlineItem::InlineBox(b) => b.vertical_align,
            InlineItem::InlineBlock(b) => b.vertical_align,
            InlineItem::Replaced(r) => r.vertical_align,
        }
    }

    /// Returns true if this item can be broken (for text)
    pub fn is_breakable(&self) -> bool {
        matches!(self, InlineItem::Text(_))
    }
}

/// A shaped text item
#[derive(Debug, Clone)]
pub struct TextItem {
    /// The shaped glyphs
    pub glyphs: ShapedGlyphs,

    /// Total horizontal advance
    pub advance: f32,

    /// Baseline metrics
    pub metrics: BaselineMetrics,

    /// Vertical alignment
    pub vertical_align: VerticalAlign,

    /// Break opportunities within this text
    pub break_opportunities: Vec<BreakOpportunity>,

    /// Original text for fragment creation
    pub text: String,

    /// Font size used
    pub font_size: f32,
}

impl TextItem {
    /// Creates a new text item
    pub fn new(glyphs: ShapedGlyphs, metrics: BaselineMetrics, break_opportunities: Vec<BreakOpportunity>) -> Self {
        let advance = glyphs.total_advance;
        let text = glyphs.text.clone();
        let font_size = glyphs.font_size;
        Self {
            glyphs,
            advance,
            metrics,
            vertical_align: VerticalAlign::Baseline,
            break_opportunities,
            text,
            font_size,
        }
    }

    /// Sets the vertical alignment
    pub fn with_vertical_align(mut self, align: VerticalAlign) -> Self {
        self.vertical_align = align;
        self
    }

    /// Splits this text item at a byte offset, returning (before, after)
    ///
    /// This is used for line breaking within text.
    pub fn split_at(&self, byte_offset: usize) -> Option<(TextItem, TextItem)> {
        if byte_offset == 0 || byte_offset >= self.text.len() {
            return None;
        }

        // Split the text
        let (before_text, after_text) = self.text.split_at(byte_offset);

        // Calculate advance for the 'before' portion using clusters
        let before_advance = self.advance_at_offset(byte_offset);
        let after_advance = self.advance - before_advance;

        // Create new items (simplified - in production would re-shape)
        let before_item = TextItem {
            glyphs: ShapedGlyphs::empty(), // Would need to split glyphs properly
            advance: before_advance,
            metrics: self.metrics,
            vertical_align: self.vertical_align,
            break_opportunities: self
                .break_opportunities
                .iter()
                .filter(|b| b.byte_offset <= byte_offset)
                .copied()
                .collect(),
            text: before_text.to_string(),
            font_size: self.font_size,
        };

        let after_item = TextItem {
            glyphs: ShapedGlyphs::empty(),
            advance: after_advance,
            metrics: self.metrics,
            vertical_align: self.vertical_align,
            break_opportunities: self
                .break_opportunities
                .iter()
                .filter(|b| b.byte_offset > byte_offset)
                .map(|b| BreakOpportunity::new(b.byte_offset - byte_offset, b.break_type))
                .collect(),
            text: after_text.to_string(),
            font_size: self.font_size,
        };

        Some((before_item, after_item))
    }

    /// Gets the horizontal advance at a given byte offset
    fn advance_at_offset(&self, byte_offset: usize) -> f32 {
        if byte_offset == 0 {
            return 0.0;
        }
        if byte_offset >= self.text.len() {
            return self.advance;
        }

        // Use cluster information if available
        if !self.glyphs.clusters.is_empty() {
            self.glyphs.x_position_for_text_offset(byte_offset)
        } else {
            // Approximate based on character proportion
            let char_count = self.text.chars().count();
            let offset_chars = self.text[..byte_offset].chars().count();
            self.advance * (offset_chars as f32 / char_count as f32)
        }
    }

    /// Finds the best break point that fits within max_width
    pub fn find_break_point(&self, max_width: f32) -> Option<usize> {
        // Find the last break opportunity that fits
        let mut best_break = None;

        for brk in &self.break_opportunities {
            let width_at_break = self.advance_at_offset(brk.byte_offset);
            if width_at_break <= max_width {
                best_break = Some(brk.byte_offset);
            } else {
                break;
            }
        }

        best_break
    }
}

/// An inline box item (non-atomic, contains children)
#[derive(Debug, Clone)]
pub struct InlineBoxItem {
    /// Child items within this inline box
    pub children: Vec<InlineItem>,

    /// Opening edge width (left border + padding)
    pub start_edge: f32,

    /// Closing edge width (right border + padding)
    pub end_edge: f32,

    /// Baseline metrics for this box
    pub metrics: BaselineMetrics,

    /// Vertical alignment
    pub vertical_align: VerticalAlign,

    /// Index for fragment creation
    pub box_index: usize,
}

impl InlineBoxItem {
    /// Creates a new inline box item
    pub fn new(start_edge: f32, end_edge: f32, metrics: BaselineMetrics, box_index: usize) -> Self {
        Self {
            children: Vec::new(),
            start_edge,
            end_edge,
            metrics,
            vertical_align: VerticalAlign::Baseline,
            box_index,
        }
    }

    /// Adds a child item
    pub fn add_child(&mut self, child: InlineItem) {
        self.children.push(child);
    }

    /// Returns the total width of this inline box
    pub fn width(&self) -> f32 {
        let content_width: f32 = self.children.iter().map(|c| c.width()).sum();
        self.start_edge + content_width + self.end_edge
    }
}

/// An inline-block item (atomic inline)
#[derive(Debug, Clone)]
pub struct InlineBlockItem {
    /// The laid-out fragment
    pub fragment: FragmentNode,

    /// Width of the inline-block
    pub width: f32,

    /// Height of the inline-block
    pub height: f32,

    /// Baseline metrics
    pub metrics: BaselineMetrics,

    /// Vertical alignment
    pub vertical_align: VerticalAlign,
}

impl InlineBlockItem {
    /// Creates a new inline-block item
    pub fn new(fragment: FragmentNode) -> Self {
        let width = fragment.bounds.width();
        let height = fragment.bounds.height();
        let metrics = BaselineMetrics::for_replaced(height);

        Self {
            fragment,
            width,
            height,
            metrics,
            vertical_align: VerticalAlign::Baseline,
        }
    }

    /// Sets the vertical alignment
    pub fn with_vertical_align(mut self, align: VerticalAlign) -> Self {
        self.vertical_align = align;
        self
    }
}

/// A replaced element item (img, canvas, etc.)
#[derive(Debug, Clone)]
pub struct ReplacedItem {
    /// Width of the element
    pub width: f32,

    /// Height of the element
    pub height: f32,

    /// Baseline metrics
    pub metrics: BaselineMetrics,

    /// Vertical alignment
    pub vertical_align: VerticalAlign,

    /// Optional source identifier
    pub source: Option<String>,
}

impl ReplacedItem {
    /// Creates a new replaced item
    pub fn new(width: f32, height: f32) -> Self {
        let metrics = BaselineMetrics::for_replaced(height);
        Self {
            width,
            height,
            metrics,
            vertical_align: VerticalAlign::Baseline,
            source: None,
        }
    }

    /// Sets the vertical alignment
    pub fn with_vertical_align(mut self, align: VerticalAlign) -> Self {
        self.vertical_align = align;
        self
    }

    /// Sets the source identifier
    pub fn with_source(mut self, source: String) -> Self {
        self.source = Some(source);
        self
    }
}

/// A positioned item within a line
#[derive(Debug, Clone)]
pub struct PositionedItem {
    /// The inline item
    pub item: InlineItem,

    /// X position relative to line start
    pub x: f32,

    /// Y offset from line baseline (positive = down)
    pub baseline_offset: f32,
}

/// A completed line box
#[derive(Debug, Clone)]
pub struct Line {
    /// Positioned items in this line
    pub items: Vec<PositionedItem>,

    /// Total width used by items
    pub width: f32,

    /// Line height
    pub height: f32,

    /// Baseline position from top of line box
    pub baseline: f32,

    /// Whether this line ends with a hard break
    pub ends_with_hard_break: bool,
}

impl Line {
    /// Creates an empty line
    pub fn new() -> Self {
        Self {
            items: Vec::new(),
            width: 0.0,
            height: 0.0,
            baseline: 0.0,
            ends_with_hard_break: false,
        }
    }

    /// Returns true if the line has no items
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }
}

impl Default for Line {
    fn default() -> Self {
        Self::new()
    }
}

/// Builder for constructing lines from inline items
///
/// Handles line breaking and item positioning.
pub struct LineBuilder {
    /// Available width for lines
    available_width: f32,

    /// Current line being built
    current_line: Line,

    /// Current X position
    current_x: f32,

    /// Completed lines
    lines: Vec<Line>,

    /// Baseline accumulator for current line
    baseline_acc: LineBaselineAccumulator,

    /// Default strut metrics (from containing block)
    strut_metrics: BaselineMetrics,
}

impl LineBuilder {
    /// Creates a new line builder
    pub fn new(available_width: f32, strut_metrics: BaselineMetrics) -> Self {
        Self {
            available_width,
            current_line: Line::new(),
            current_x: 0.0,
            lines: Vec::new(),
            baseline_acc: LineBaselineAccumulator::new(&strut_metrics),
            strut_metrics,
        }
    }

    /// Adds an inline item to the builder
    pub fn add_item(&mut self, item: InlineItem) {
        let item_width = item.width();

        // Check if item fits
        if self.current_x + item_width <= self.available_width || self.current_line.is_empty() {
            // Item fits (or line is empty, so we must take it)
            self.place_item(item);
        } else if item.is_breakable() {
            // Try to break the item
            self.add_breakable_item(item);
        } else {
            // Item doesn't fit and can't be broken - start new line
            self.finish_line();
            self.place_item(item);
        }
    }

    /// Adds a breakable item (text), handling line breaking
    fn add_breakable_item(&mut self, item: InlineItem) {
        if let InlineItem::Text(text_item) = item {
            let remaining_width = (self.available_width - self.current_x).max(0.0);

            if let Some(break_point) = text_item.find_break_point(remaining_width) {
                // Split at break point
                if let Some((before, after)) = text_item.split_at(break_point) {
                    // Place the part that fits
                    if before.advance > 0.0 {
                        self.place_item(InlineItem::Text(before));
                    }

                    // Start new line for the rest
                    self.finish_line();

                    // Add remaining text (may need further breaking)
                    self.add_item(InlineItem::Text(after));
                }
            } else {
                // No break point found within remaining width
                if self.current_line.is_empty() {
                    // Line is empty, must place item (overflow)
                    self.place_item(InlineItem::Text(text_item));
                } else {
                    // Start new line and try again
                    self.finish_line();
                    self.add_item(InlineItem::Text(text_item));
                }
            }
        }
    }

    /// Places an item on the current line without breaking
    fn place_item(&mut self, item: InlineItem) {
        let metrics = item.baseline_metrics();
        let vertical_align = item.vertical_align();

        // Calculate baseline offset
        let baseline_offset = if vertical_align.is_line_relative() {
            self.baseline_acc.add_line_relative(&metrics, vertical_align);
            0.0 // Will be adjusted in finalization
        } else {
            self.baseline_acc.add_baseline_relative(&metrics, vertical_align, None)
        };

        let positioned = PositionedItem {
            item,
            x: self.current_x,
            baseline_offset,
        };

        self.current_x += positioned.item.width();
        self.current_line.items.push(positioned);
    }

    /// Forces a line break (e.g., from mandatory break)
    pub fn force_break(&mut self) {
        self.current_line.ends_with_hard_break = true;
        self.finish_line();
    }

    /// Finishes the current line and starts a new one
    fn finish_line(&mut self) {
        if !self.current_line.is_empty() {
            // Calculate final line metrics
            self.current_line.width = self.current_x;
            self.current_line.height = self.baseline_acc.line_height();
            self.current_line.baseline = self.baseline_acc.baseline_position();

            // Adjust Y positions for top/bottom aligned items
            for positioned in &mut self.current_line.items {
                let align = positioned.item.vertical_align();
                match align {
                    VerticalAlign::Top => {
                        positioned.baseline_offset =
                            positioned.item.baseline_metrics().baseline_offset - self.current_line.baseline;
                    }
                    VerticalAlign::Bottom => {
                        let metrics = positioned.item.baseline_metrics();
                        positioned.baseline_offset = self.current_line.height
                            - metrics.height
                            - (self.current_line.baseline - metrics.baseline_offset);
                    }
                    _ => {}
                }
            }

            self.lines.push(std::mem::take(&mut self.current_line));
        }

        // Reset for new line
        self.current_x = 0.0;
        self.baseline_acc = LineBaselineAccumulator::new(&self.strut_metrics);
        self.current_line = Line::new();
    }

    /// Finishes building and returns all lines
    pub fn finish(mut self) -> Vec<Line> {
        // Finish any remaining line
        self.finish_line();
        self.lines
    }

    /// Returns the current line width
    pub fn current_width(&self) -> f32 {
        self.current_x
    }

    /// Returns true if current line is empty
    pub fn is_current_line_empty(&self) -> bool {
        self.current_line.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::geometry::Rect;
    use crate::text::line_break::find_break_opportunities;

    fn make_strut_metrics() -> BaselineMetrics {
        BaselineMetrics::new(12.0, 16.0, 12.0, 4.0)
    }

    fn make_text_item(text: &str, advance: f32) -> TextItem {
        let mut item = TextItem {
            glyphs: ShapedGlyphs::empty(),
            advance,
            metrics: make_strut_metrics(),
            vertical_align: VerticalAlign::Baseline,
            break_opportunities: find_break_opportunities(text),
            text: text.to_string(),
            font_size: 16.0,
        };
        item.glyphs.text = text.to_string();
        item
    }

    #[test]
    fn test_line_builder_single_item_fits() {
        let strut = make_strut_metrics();
        let mut builder = LineBuilder::new(100.0, strut);

        let item = make_text_item("Hello", 50.0);
        builder.add_item(InlineItem::Text(item));

        let lines = builder.finish();
        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].items.len(), 1);
        assert!(lines[0].width <= 100.0);
    }

    #[test]
    fn test_line_builder_multiple_items_fit() {
        let strut = make_strut_metrics();
        let mut builder = LineBuilder::new(200.0, strut);

        builder.add_item(InlineItem::Text(make_text_item("Hello", 50.0)));
        builder.add_item(InlineItem::Text(make_text_item(" ", 5.0)));
        builder.add_item(InlineItem::Text(make_text_item("World", 50.0)));

        let lines = builder.finish();
        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].items.len(), 3);
    }

    #[test]
    fn test_line_builder_item_exceeds_width() {
        let strut = make_strut_metrics();
        let mut builder = LineBuilder::new(80.0, strut);

        builder.add_item(InlineItem::Text(make_text_item("Hello", 50.0)));
        builder.add_item(InlineItem::Text(make_text_item("World", 50.0)));

        let lines = builder.finish();
        // Second item should go to new line
        assert_eq!(lines.len(), 2);
    }

    #[test]
    fn test_line_builder_force_break() {
        let strut = make_strut_metrics();
        let mut builder = LineBuilder::new(200.0, strut);

        builder.add_item(InlineItem::Text(make_text_item("Hello", 50.0)));
        builder.force_break();
        builder.add_item(InlineItem::Text(make_text_item("World", 50.0)));

        let lines = builder.finish();
        assert_eq!(lines.len(), 2);
        assert!(lines[0].ends_with_hard_break);
    }

    #[test]
    fn test_line_builder_empty_result() {
        let strut = make_strut_metrics();
        let builder = LineBuilder::new(100.0, strut);

        let lines = builder.finish();
        assert!(lines.is_empty());
    }

    #[test]
    fn test_line_has_baseline() {
        let strut = make_strut_metrics();
        let mut builder = LineBuilder::new(200.0, strut);

        builder.add_item(InlineItem::Text(make_text_item("Hello", 50.0)));

        let lines = builder.finish();
        assert_eq!(lines.len(), 1);
        assert!(lines[0].baseline > 0.0);
        assert!(lines[0].height > 0.0);
    }

    #[test]
    fn test_replaced_item() {
        let strut = make_strut_metrics();
        let mut builder = LineBuilder::new(200.0, strut);

        let replaced = ReplacedItem::new(100.0, 50.0);
        builder.add_item(InlineItem::Replaced(replaced));

        let lines = builder.finish();
        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].items[0].item.width(), 100.0);
    }

    #[test]
    fn test_inline_block_item() {
        let strut = make_strut_metrics();
        let mut builder = LineBuilder::new(200.0, strut);

        let fragment = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 80.0, 40.0), vec![]);
        let inline_block = InlineBlockItem::new(fragment);
        builder.add_item(InlineItem::InlineBlock(inline_block));

        let lines = builder.finish();
        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].items[0].item.width(), 80.0);
    }

    #[test]
    fn test_overflow_on_empty_line() {
        let strut = make_strut_metrics();
        let mut builder = LineBuilder::new(30.0, strut);

        // Item too wide but line is empty, so it must fit
        builder.add_item(InlineItem::Text(make_text_item("VeryLongWord", 100.0)));

        let lines = builder.finish();
        assert_eq!(lines.len(), 1);
        assert!(lines[0].width > 30.0); // Overflow allowed
    }

    #[test]
    fn test_positioned_item_x_position() {
        let strut = make_strut_metrics();
        let mut builder = LineBuilder::new(200.0, strut);

        builder.add_item(InlineItem::Text(make_text_item("Hello", 50.0)));
        builder.add_item(InlineItem::Text(make_text_item(" ", 5.0)));
        builder.add_item(InlineItem::Text(make_text_item("World", 50.0)));

        let lines = builder.finish();
        assert_eq!(lines[0].items[0].x, 0.0);
        assert_eq!(lines[0].items[1].x, 50.0);
        assert_eq!(lines[0].items[2].x, 55.0);
    }

    #[test]
    fn test_text_item_break_opportunities() {
        let item = make_text_item("Hello World Test", 160.0);

        // Should have break opportunities after spaces
        assert!(!item.break_opportunities.is_empty());
    }

    #[test]
    fn test_vertical_align_default() {
        let item = make_text_item("Test", 40.0);
        assert_eq!(item.vertical_align, VerticalAlign::Baseline);
    }

    #[test]
    fn test_line_default() {
        let line = Line::default();
        assert!(line.is_empty());
        assert_eq!(line.width, 0.0);
    }
}
