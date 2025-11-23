//! Line Builder - Line Breaking Algorithm
//!
//! This module implements the line breaking algorithm for inline formatting context.
//! It takes a flat list of inline items and distributes them across line boxes.
//!
//! # Algorithm
//!
//! The line breaking algorithm follows CSS 2.1 Section 9.4.2 and UAX #14:
//!
//! 1. Process items left-to-right
//! 2. Add items to current line if they fit
//! 3. Break at soft break opportunities when necessary
//! 4. Always break at hard breaks (e.g., `<br>`)
//! 5. If no break opportunity exists, overflow occurs
//!
//! # References
//!
//! - CSS 2.1 Section 9.4.2: <https://www.w3.org/TR/CSS21/visuren.html#inline-formatting>
//! - Unicode Line Breaking: <https://www.unicode.org/reports/tr14/>

use crate::style::ComputedStyles;
use std::sync::Arc;

/// A text run - contiguous text with uniform styling
///
/// Text runs are created from text boxes and represent
/// the smallest unit of inline text content.
#[derive(Debug, Clone)]
pub struct TextRun {
    /// The text content
    pub text: String,

    /// Computed width of the text (in CSS pixels)
    pub width: f32,

    /// Height of the text run (line-height)
    pub height: f32,

    /// Baseline offset from top (ascent)
    pub baseline: f32,

    /// Font size used for this text
    pub font_size: f32,

    /// Style applied to this text
    pub style: Arc<ComputedStyles>,
}

impl TextRun {
    /// Creates a new text run with the given parameters
    pub fn new(
        text: String,
        width: f32,
        height: f32,
        baseline: f32,
        font_size: f32,
        style: Arc<ComputedStyles>,
    ) -> Self {
        Self {
            text,
            width,
            height,
            baseline,
            font_size,
            style,
        }
    }

    /// Returns true if this text run is empty
    pub fn is_empty(&self) -> bool {
        self.text.is_empty()
    }

    /// Returns true if this text run consists only of whitespace
    pub fn is_whitespace_only(&self) -> bool {
        self.text.chars().all(|c| c.is_whitespace())
    }
}

/// An inline item - the smallest unit for line breaking
///
/// Inline items represent different types of content that
/// participate in inline layout. They are collected into
/// a flat list before line breaking.
#[derive(Debug, Clone)]
pub enum InlineItem {
    /// A text run
    Text(TextRun),

    /// Start of an inline box (e.g., `<span>`)
    ///
    /// Marks where an inline element begins. Used for applying
    /// borders, padding, and backgrounds.
    StartInlineBox {
        /// Style of the inline box
        style: Arc<ComputedStyles>,
    },

    /// End of an inline box
    ///
    /// Marks where an inline element ends.
    EndInlineBox,

    /// An atomic inline-level box (inline-block, replaced element)
    ///
    /// Cannot be broken across lines.
    Atomic {
        /// Width of the atomic box
        width: f32,
        /// Height of the atomic box
        height: f32,
        /// Baseline offset from top
        baseline: f32,
        /// Style of the atomic box
        style: Arc<ComputedStyles>,
    },

    /// A soft break opportunity (where line may break)
    ///
    /// Typically inserted between words or at certain Unicode
    /// line breaking opportunities.
    SoftBreakOpportunity,

    /// A hard break (forced line break, e.g., `<br>`)
    ///
    /// Forces a line break at this point.
    HardBreak,
}

impl InlineItem {
    /// Returns the width of this inline item
    pub fn width(&self) -> f32 {
        match self {
            InlineItem::Text(text_run) => text_run.width,
            InlineItem::Atomic { width, .. } => *width,
            InlineItem::StartInlineBox { style } => {
                // Add left border + padding if present
                style.border_left_width.to_px() + style.padding_left.to_px()
            }
            InlineItem::EndInlineBox => 0.0, // Handled at the inline box level
            InlineItem::SoftBreakOpportunity | InlineItem::HardBreak => 0.0,
        }
    }

    /// Returns true if this item can be broken at
    pub fn is_break_opportunity(&self) -> bool {
        matches!(self, InlineItem::SoftBreakOpportunity | InlineItem::HardBreak)
    }

    /// Returns true if this is a hard (forced) break
    pub fn is_hard_break(&self) -> bool {
        matches!(self, InlineItem::HardBreak)
    }
}

/// A line box - a single line of inline content
///
/// Contains the items that appear on this line along with
/// computed width and break information.
#[derive(Debug, Clone)]
pub struct Line {
    /// Items on this line
    pub items: Vec<InlineItem>,

    /// Total width of items on this line
    pub width: f32,

    /// Whether this line ends with a hard break
    pub has_hard_break: bool,
}

impl Line {
    /// Creates a new empty line
    pub fn new() -> Self {
        Self {
            items: Vec::new(),
            width: 0.0,
            has_hard_break: false,
        }
    }

    /// Returns true if this line has no content
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    /// Adds an item to this line
    pub fn push(&mut self, item: InlineItem) {
        self.width += item.width();
        if item.is_hard_break() {
            self.has_hard_break = true;
        }
        self.items.push(item);
    }

    /// Returns the number of items on this line
    pub fn len(&self) -> usize {
        self.items.len()
    }
}

impl Default for Line {
    fn default() -> Self {
        Self::new()
    }
}

/// Line builder - constructs lines from inline items
///
/// The line builder implements the line breaking algorithm,
/// distributing inline items across lines based on available width.
///
/// # Usage
///
/// ```rust,ignore
/// let mut builder = LineBuilder::new(available_width);
/// for item in items {
///     builder.add_item(item);
/// }
/// let lines = builder.finish();
/// ```
#[derive(Debug)]
pub struct LineBuilder {
    /// Available width for each line
    available_width: f32,

    /// Completed lines
    lines: Vec<Line>,

    /// Current line being built
    current_line: Line,

    /// Items pending since the last break opportunity
    pending_items: Vec<InlineItem>,

    /// Width of pending items
    pending_width: f32,
}

impl LineBuilder {
    /// Creates a new line builder with the specified available width
    pub fn new(available_width: f32) -> Self {
        Self {
            available_width,
            lines: Vec::new(),
            current_line: Line::new(),
            pending_items: Vec::new(),
            pending_width: 0.0,
        }
    }

    /// Adds an inline item to be processed
    pub fn add_item(&mut self, item: InlineItem) {
        match &item {
            InlineItem::SoftBreakOpportunity => {
                self.handle_soft_break();
            }
            InlineItem::HardBreak => {
                self.handle_hard_break();
            }
            _ => {
                self.pending_items.push(item.clone());
                self.pending_width += item.width();
            }
        }
    }

    /// Handles a soft break opportunity
    fn handle_soft_break(&mut self) {
        // Try to commit pending items to current line
        if self.current_line.width + self.pending_width <= self.available_width {
            // Items fit - commit them
            self.commit_pending();
        } else if self.current_line.is_empty() {
            // Line is empty - we have to take the pending items (overflow)
            self.commit_pending();
        } else {
            // Items don't fit - break here
            self.finish_line();
            self.commit_pending();
        }
    }

    /// Handles a hard break (forced line break)
    fn handle_hard_break(&mut self) {
        // Commit any pending items
        self.commit_pending();
        // Force a line break
        self.current_line.has_hard_break = true;
        self.finish_line();
    }

    /// Commits pending items to the current line
    fn commit_pending(&mut self) {
        for item in self.pending_items.drain(..) {
            self.current_line.push(item);
        }
        self.pending_width = 0.0;
    }

    /// Finishes the current line and starts a new one
    fn finish_line(&mut self) {
        if !self.current_line.is_empty() || self.current_line.has_hard_break {
            let line = std::mem::take(&mut self.current_line);
            self.lines.push(line);
        }
    }

    /// Finishes line building and returns all lines
    pub fn finish(mut self) -> Vec<Line> {
        // Commit any remaining pending items
        self.commit_pending();

        // Finish the last line
        if !self.current_line.is_empty() {
            self.lines.push(self.current_line);
        }

        self.lines
    }

    /// Returns the current available width
    pub fn available_width(&self) -> f32 {
        self.available_width
    }

    /// Returns the width remaining on the current line
    pub fn remaining_width(&self) -> f32 {
        (self.available_width - self.current_line.width - self.pending_width).max(0.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::style::ComputedStyles;

    fn default_style() -> Arc<ComputedStyles> {
        Arc::new(ComputedStyles::default())
    }

    fn text_run(text: &str, width: f32) -> InlineItem {
        InlineItem::Text(TextRun {
            text: text.to_string(),
            width,
            height: 20.0,
            baseline: 16.0,
            font_size: 16.0,
            style: default_style(),
        })
    }

    #[test]
    fn test_empty_builder() {
        let builder = LineBuilder::new(100.0);
        let lines = builder.finish();
        assert!(lines.is_empty());
    }

    #[test]
    fn test_single_item_fits() {
        let mut builder = LineBuilder::new(100.0);
        builder.add_item(text_run("Hello", 50.0));
        builder.add_item(InlineItem::SoftBreakOpportunity);

        let lines = builder.finish();
        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].width, 50.0);
    }

    #[test]
    fn test_multiple_items_single_line() {
        let mut builder = LineBuilder::new(200.0);
        builder.add_item(text_run("Hello", 50.0));
        builder.add_item(InlineItem::SoftBreakOpportunity);
        builder.add_item(text_run("World", 50.0));
        builder.add_item(InlineItem::SoftBreakOpportunity);

        let lines = builder.finish();
        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].width, 100.0);
    }

    #[test]
    fn test_line_wrapping() {
        let mut builder = LineBuilder::new(80.0);
        builder.add_item(text_run("Hello", 50.0));
        builder.add_item(InlineItem::SoftBreakOpportunity);
        builder.add_item(text_run("World", 50.0));
        builder.add_item(InlineItem::SoftBreakOpportunity);

        let lines = builder.finish();
        assert_eq!(lines.len(), 2);
        assert_eq!(lines[0].width, 50.0);
        assert_eq!(lines[1].width, 50.0);
    }

    #[test]
    fn test_hard_break() {
        let mut builder = LineBuilder::new(200.0);
        builder.add_item(text_run("First", 50.0));
        builder.add_item(InlineItem::HardBreak);
        builder.add_item(text_run("Second", 50.0));

        let lines = builder.finish();
        assert_eq!(lines.len(), 2);
        assert!(lines[0].has_hard_break);
        assert!(!lines[1].has_hard_break);
    }

    #[test]
    fn test_overflow_single_item() {
        let mut builder = LineBuilder::new(50.0);
        // Item wider than available width
        builder.add_item(text_run("VeryLongWord", 100.0));
        builder.add_item(InlineItem::SoftBreakOpportunity);

        let lines = builder.finish();
        // Item should still be placed (overflow)
        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].width, 100.0);
    }

    #[test]
    fn test_text_run_creation() {
        let run = TextRun::new("Test".to_string(), 40.0, 20.0, 16.0, 16.0, default_style());
        assert_eq!(run.text, "Test");
        assert_eq!(run.width, 40.0);
        assert!(!run.is_empty());
        assert!(!run.is_whitespace_only());
    }

    #[test]
    fn test_text_run_whitespace() {
        let run = TextRun::new("   ".to_string(), 20.0, 20.0, 16.0, 16.0, default_style());
        assert!(run.is_whitespace_only());

        let run2 = TextRun::new("".to_string(), 0.0, 20.0, 16.0, 16.0, default_style());
        assert!(run2.is_empty());
    }

    #[test]
    fn test_inline_item_width() {
        let text_item = text_run("Test", 40.0);
        assert_eq!(text_item.width(), 40.0);

        let atomic = InlineItem::Atomic {
            width: 100.0,
            height: 50.0,
            baseline: 40.0,
            style: default_style(),
        };
        assert_eq!(atomic.width(), 100.0);

        let soft_break = InlineItem::SoftBreakOpportunity;
        assert_eq!(soft_break.width(), 0.0);
    }

    #[test]
    fn test_break_opportunities() {
        let text_item = text_run("Test", 40.0);
        assert!(!text_item.is_break_opportunity());
        assert!(!text_item.is_hard_break());

        let soft_break = InlineItem::SoftBreakOpportunity;
        assert!(soft_break.is_break_opportunity());
        assert!(!soft_break.is_hard_break());

        let hard_break = InlineItem::HardBreak;
        assert!(hard_break.is_break_opportunity());
        assert!(hard_break.is_hard_break());
    }

    #[test]
    fn test_line_operations() {
        let mut line = Line::new();
        assert!(line.is_empty());
        assert_eq!(line.len(), 0);

        line.push(text_run("Hello", 50.0));
        assert!(!line.is_empty());
        assert_eq!(line.len(), 1);
        assert_eq!(line.width, 50.0);
    }

    #[test]
    fn test_remaining_width() {
        let mut builder = LineBuilder::new(100.0);
        assert_eq!(builder.remaining_width(), 100.0);

        builder.add_item(text_run("Hello", 30.0));
        assert_eq!(builder.remaining_width(), 70.0);
    }

    #[test]
    fn test_atomic_inline() {
        let mut builder = LineBuilder::new(200.0);
        builder.add_item(text_run("Before", 50.0));
        builder.add_item(InlineItem::Atomic {
            width: 60.0,
            height: 40.0,
            baseline: 30.0,
            style: default_style(),
        });
        builder.add_item(InlineItem::SoftBreakOpportunity);
        builder.add_item(text_run("After", 50.0));
        builder.add_item(InlineItem::SoftBreakOpportunity);

        let lines = builder.finish();
        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].width, 160.0); // 50 + 60 + 50
    }

    #[test]
    fn test_inline_box_markers() {
        let mut builder = LineBuilder::new(200.0);

        builder.add_item(InlineItem::StartInlineBox { style: default_style() });
        builder.add_item(text_run("Inside", 50.0));
        builder.add_item(InlineItem::EndInlineBox);
        builder.add_item(InlineItem::SoftBreakOpportunity);

        let lines = builder.finish();
        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].len(), 3); // Start, text, end
    }

    #[test]
    fn test_multiple_hard_breaks() {
        let mut builder = LineBuilder::new(200.0);
        builder.add_item(InlineItem::HardBreak);
        builder.add_item(InlineItem::HardBreak);
        builder.add_item(text_run("Text", 40.0));

        let lines = builder.finish();
        // Two empty lines from hard breaks, plus one with text
        assert_eq!(lines.len(), 3);
    }
}
