# Phase 2: Inline Layout

**Duration:** Week 2-3 of Phase 2 (10-14 days)
**Prerequisites:**
- Phase 1 complete (type system, formatting contexts, box generation)
- Block layout complete
**Dependencies:**
- FormattingContext trait
- BoxNode, FragmentNode types
- LayoutConstraints
- FontContext (critical for baseline alignment)
**Output:** Fully functional Inline Formatting Context implementation

## Objectives

Implement the Inline Formatting Context (IFC) - the complex layout algorithm for horizontal text flow, line breaking, and baseline alignment.

This is the **most complex formatting context** and is essential for rendering any text content in CSS.

## Context

The Inline Formatting Context is how browsers layout inline-level content:

- **Inline boxes flow horizontally** until they run out of space
- **Line breaking** occurs at soft wrap opportunities
- **Baseline alignment** positions boxes vertically within a line
- **Mixed inline/inline-block** boxes coexist on the same line
- **Multiple lines** stack vertically with line-height spacing

**From CSS 2.1 Section 9.4.2:**
> "In an inline formatting context, boxes are laid out horizontally, one after the other, beginning at the top of a containing block. Horizontal margins, borders, and padding are respected between these boxes."

**Examples of what creates an IFC:**
- Block boxes containing inline-level children
- Any box with text content
- Boxes with `display: inline`, `inline-block`, `inline-table`

## The Problem V1 Has

V1 uses Taffy, which has no concept of:
- Line breaking
- Baseline alignment
- Font metrics (ascent, descent, line-height)
- Inline box splitting across lines
- Text measurement

This makes proper text layout impossible.

## The Solution

Implement a dedicated Inline Formatting Context that follows CSS 2.1 Section 9.4.2 and CSS Inline Layout Module Level 3.

## CSS Specification References

**Primary:**
- **CSS 2.1 Section 9.4.2:** Inline Formatting Contexts
- **CSS 2.1 Section 10.8:** Line height calculations
- **CSS Inline Layout Module Level 3:** Modern inline layout specification

**Key Concepts:**
- **Line box:** The rectangular area that contains inline boxes on a single line
- **Inline box:** A box that participates in an IFC (text, `<span>`, etc.)
- **Text run:** A sequence of text in the same font/style
- **Baseline:** The line on which text sits
- **Leading:** Extra space added to line-height

## Step-by-Step Implementation

### Step 1: Create Inline FC Module (Day 1)

```bash
mkdir -p /home/user/fastrender/src/layout/contexts/inline
touch /home/user/fastrender/src/layout/contexts/inline/mod.rs
```

**File: `src/layout/contexts/inline/mod.rs`**

```rust
//! Inline Formatting Context implementation
//!
//! CSS Specification: CSS 2.1 Section 9.4.2 - Inline Formatting Contexts
//! https://www.w3.org/TR/CSS21/visuren.html#inline-formatting
//!
//! Also references CSS Inline Layout Module Level 3:
//! https://www.w3.org/TR/css-inline-3/

use super::{FormattingContext, FormattingContextType, Axis};
use crate::tree::{BoxNode, FragmentNode, BoxType};
use crate::layout::LayoutConstraints;
use crate::text::{FontContext, TextMetrics, ShapedText};
use crate::geometry::{Point, Size, Rect};
use crate::error::{Result, Error};

pub mod line_breaker;
pub mod baseline;
pub mod text_shaping;

use line_breaker::LineBreaker;
use baseline::BaselineAligner;

/// Inline Formatting Context
///
/// Lays out inline-level boxes horizontally with line breaking and baseline alignment.
#[derive(Debug)]
pub struct InlineFormattingContext {
    /// Text shaper for measuring and shaping text
    shaper: text_shaping::TextShaper,
}

impl InlineFormattingContext {
    /// Creates a new inline formatting context
    pub fn new() -> Self {
        Self {
            shaper: text_shaping::TextShaper::new(),
        }
    }
}

impl FormattingContext for InlineFormattingContext {
    fn context_type(&self) -> FormattingContextType {
        FormattingContextType::Inline
    }

    fn layout(
        &self,
        box_node: &BoxNode,
        constraints: &LayoutConstraints,
        font_context: &FontContext,
    ) -> Result<FragmentNode> {
        // The inline FC operates on a block box that contains inline children
        // It creates line boxes and positions inline boxes within them

        let containing_width = constraints.available_width
            .ok_or_else(|| Error::Layout("Inline FC requires containing block width".into()))?;

        // Phase 1: Build inline items (text runs, inline boxes, line break opportunities)
        let inline_items = self.build_inline_items(box_node, font_context)?;

        // Phase 2: Break into lines
        let lines = self.break_into_lines(&inline_items, containing_width, font_context)?;

        // Phase 3: Perform baseline alignment within each line
        let positioned_lines = self.align_lines(&lines, font_context)?;

        // Phase 4: Create fragment tree
        let fragment = self.create_inline_fragment(
            box_node,
            positioned_lines,
            containing_width,
        )?;

        Ok(fragment)
    }

    fn compute_intrinsic_size(
        &self,
        box_node: &BoxNode,
        axis: Axis,
        font_context: &FontContext,
    ) -> Result<f32> {
        match axis {
            Axis::Horizontal => {
                // Intrinsic width = width if text never wrapped
                let inline_items = self.build_inline_items(box_node, font_context)?;
                let mut max_width = 0.0;
                let mut current_width = 0.0;

                for item in &inline_items {
                    match item {
                        InlineItem::Text(text_run) => {
                            current_width += text_run.width;
                        }
                        InlineItem::HardBreak => {
                            max_width = max_width.max(current_width);
                            current_width = 0.0;
                        }
                        _ => {}
                    }
                }
                max_width = max_width.max(current_width);
                Ok(max_width)
            }
            Axis::Vertical => {
                // Intrinsic height = height with max-content width
                let max_width = self.compute_intrinsic_size(box_node, Axis::Horizontal, font_context)?;
                let constraints = LayoutConstraints {
                    available_width: Some(max_width),
                    ..Default::default()
                };
                let fragment = self.layout(box_node, &constraints, font_context)?;
                Ok(fragment.rect().size.height)
            }
        }
    }
}

impl InlineFormattingContext {
    /// Builds the flat list of inline items from the box tree
    ///
    /// Inline items include:
    /// - Text runs (shaped text with metrics)
    /// - Inline boxes (spans, etc.)
    /// - Atomic inline boxes (inline-block, images)
    /// - Line break opportunities
    fn build_inline_items(
        &self,
        box_node: &BoxNode,
        font_context: &FontContext,
    ) -> Result<Vec<InlineItem>> {
        let mut items = Vec::new();
        self.collect_inline_items(box_node, font_context, &mut items)?;
        Ok(items)
    }

    /// Recursively collects inline items from box tree
    fn collect_inline_items(
        &self,
        box_node: &BoxNode,
        font_context: &FontContext,
        items: &mut Vec<InlineItem>,
    ) -> Result<()> {
        match box_node.box_type {
            BoxType::Text => {
                // Shape text and create text runs
                let text = self.get_text_content(box_node);
                let shaped = self.shaper.shape(&text, &box_node.style, font_context)?;

                // Split shaped text at soft wrap opportunities
                for text_run in self.split_into_runs(shaped) {
                    items.push(InlineItem::Text(text_run));
                    items.push(InlineItem::SoftBreakOpportunity);
                }
            }
            BoxType::Inline => {
                // Inline box (e.g., <span>)
                items.push(InlineItem::StartInlineBox {
                    style: box_node.style.clone(),
                });

                // Process children
                for child in &box_node.children {
                    self.collect_inline_items(child, font_context, items)?;
                }

                items.push(InlineItem::EndInlineBox);
            }
            BoxType::InlineBlock => {
                // Atomic inline box - layout as block, participate as inline
                // This is like a single character from the inline layout perspective
                let block_fc = FormattingContextFactory::get_for_box(child);
                let child_constraints = LayoutConstraints {
                    available_width: None, // Shrink-to-fit
                    ..Default::default()
                };
                let child_fragment = block_fc.layout(box_node, &child_constraints, font_context)?;

                items.push(InlineItem::Atomic {
                    fragment: child_fragment,
                    style: box_node.style.clone(),
                });
            }
            BoxType::LineBreak => {
                items.push(InlineItem::HardBreak);
            }
            _ => {
                return Err(Error::Layout(format!(
                    "Box type {:?} cannot participate in inline layout",
                    box_node.box_type
                )));
            }
        }

        Ok(())
    }

    /// Breaks inline items into lines based on available width
    ///
    /// CSS 2.1 Section 9.4.2: "When several inline boxes cannot fit
    /// horizontally within a single line box, they are distributed among
    /// two or more vertically-stacked line boxes."
    fn break_into_lines(
        &self,
        items: &[InlineItem],
        max_width: f32,
        font_context: &FontContext,
    ) -> Result<Vec<Line>> {
        let mut line_breaker = LineBreaker::new(max_width);
        line_breaker.break_lines(items, font_context)
    }

    /// Performs baseline alignment and positions boxes within each line
    ///
    /// CSS 2.1 Section 10.8: Line height and baseline alignment
    fn align_lines(
        &self,
        lines: &[Line],
        font_context: &FontContext,
    ) -> Result<Vec<PositionedLine>> {
        let mut result = Vec::new();
        let mut current_y = 0.0;

        for line in lines {
            let aligner = BaselineAligner::new(line, font_context);
            let positioned = aligner.align()?;

            // Position line at current Y
            let mut positioned_line = positioned;
            positioned_line.y_offset = current_y;

            current_y += positioned_line.height;
            result.push(positioned_line);
        }

        Ok(result)
    }

    /// Creates the final fragment tree from positioned lines
    fn create_inline_fragment(
        &self,
        box_node: &BoxNode,
        lines: Vec<PositionedLine>,
        width: f32,
    ) -> Result<FragmentNode> {
        let total_height = lines.iter().map(|l| l.height).sum();

        let mut children = Vec::new();
        for line in lines {
            children.extend(line.fragments);
        }

        Ok(FragmentNode::new(
            Rect::new(Point::zero(), Size::new(width, total_height)),
            box_node.style.clone(),
            children,
        ))
    }

    /// Gets text content from a text box node
    fn get_text_content(&self, box_node: &BoxNode) -> String {
        // TODO: Get actual text from DOM
        "Sample text".to_string()
    }

    /// Splits shaped text into runs at soft wrap opportunities
    fn split_into_runs(&self, shaped: ShapedText) -> Vec<TextRun> {
        // TODO: Split at word boundaries
        vec![TextRun {
            shaped_text: shaped,
            width: 100.0, // TODO: Actual width
        }]
    }
}

/// An item in the inline formatting context
#[derive(Debug, Clone)]
pub enum InlineItem {
    /// A run of shaped text
    Text(TextRun),

    /// Start of an inline box (e.g., <span>)
    StartInlineBox {
        style: Arc<ComputedStyle>,
    },

    /// End of an inline box
    EndInlineBox,

    /// An atomic inline-level box (inline-block, replaced element)
    Atomic {
        fragment: FragmentNode,
        style: Arc<ComputedStyle>,
    },

    /// A hard line break (<br>)
    HardBreak,

    /// A soft wrap opportunity (e.g., after a space)
    SoftBreakOpportunity,
}

/// A run of text in a single font/style
#[derive(Debug, Clone)]
pub struct TextRun {
    /// The shaped text with glyph information
    pub shaped_text: ShapedText,

    /// Total width of this text run
    pub width: f32,
}

/// A line of text (before baseline alignment)
#[derive(Debug)]
pub struct Line {
    /// Items on this line
    pub items: Vec<InlineItem>,

    /// Total width used on this line
    pub width: f32,
}

/// A line of text after baseline alignment
#[derive(Debug)]
pub struct PositionedLine {
    /// Y offset from top of containing block
    pub y_offset: f32,

    /// Total height of this line (line-height)
    pub height: f32,

    /// Positioned fragments on this line
    pub fragments: Vec<FragmentNode>,
}

impl Default for InlineFormattingContext {
    fn default() -> Self {
        Self::new()
    }
}
```

### Step 2: Implement Line Breaking (Day 2-3)

**File: `src/layout/contexts/inline/line_breaker.rs`**

```rust
//! Line breaking algorithm
//!
//! Implements line breaking according to CSS Text Module Level 3
//! https://www.w3.org/TR/css-text-3/#line-breaking

use super::{InlineItem, Line, TextRun};
use crate::text::FontContext;
use crate::error::Result;

/// Line breaker
///
/// Breaks a sequence of inline items into lines based on available width.
pub struct LineBreaker {
    /// Maximum line width
    max_width: f32,
}

impl LineBreaker {
    pub fn new(max_width: f32) -> Self {
        Self { max_width }
    }

    /// Breaks items into lines
    ///
    /// Algorithm:
    /// 1. Add items to current line while they fit
    /// 2. When an item doesn't fit, look for the last break opportunity
    /// 3. Break there and start a new line
    /// 4. Handle special cases (hard breaks, unbreakable items)
    pub fn break_lines(
        &mut self,
        items: &[InlineItem],
        font_context: &FontContext,
    ) -> Result<Vec<Line>> {
        let mut lines = Vec::new();
        let mut current_line = LineBuilder::new();
        let mut last_break_point = None;

        for (i, item) in items.iter().enumerate() {
            match item {
                InlineItem::HardBreak => {
                    // Forced break - finish current line immediately
                    lines.push(current_line.finish());
                    current_line = LineBuilder::new();
                    last_break_point = None;
                }
                InlineItem::SoftBreakOpportunity => {
                    // Remember this as a potential break point
                    last_break_point = Some((i, current_line.width));
                }
                InlineItem::Text(text_run) => {
                    let new_width = current_line.width + text_run.width;

                    if new_width <= self.max_width {
                        // Fits on current line
                        current_line.add_item(item.clone());
                    } else {
                        // Doesn't fit - need to break
                        if let Some((break_index, break_width)) = last_break_point {
                            // Break at last opportunity
                            let line = current_line.finish_at_width(break_width);
                            lines.push(line);

                            // Start new line with items after break point
                            current_line = LineBuilder::new();
                            // TODO: Add items from break_index to i
                        } else {
                            // No break opportunity - forced break
                            lines.push(current_line.finish());
                            current_line = LineBuilder::new();
                            current_line.add_item(item.clone());
                        }
                        last_break_point = None;
                    }
                }
                InlineItem::Atomic { fragment, .. } => {
                    let new_width = current_line.width + fragment.rect().size.width;

                    if new_width <= self.max_width {
                        current_line.add_item(item.clone());
                    } else {
                        // Start new line for atomic item
                        if !current_line.is_empty() {
                            lines.push(current_line.finish());
                            current_line = LineBuilder::new();
                        }
                        current_line.add_item(item.clone());
                    }
                }
                _ => {
                    current_line.add_item(item.clone());
                }
            }
        }

        // Finish last line
        if !current_line.is_empty() {
            lines.push(current_line.finish());
        }

        Ok(lines)
    }
}

/// Helper for building a single line
struct LineBuilder {
    items: Vec<InlineItem>,
    width: f32,
}

impl LineBuilder {
    fn new() -> Self {
        Self {
            items: Vec::new(),
            width: 0.0,
        }
    }

    fn add_item(&mut self, item: InlineItem) {
        // Update width
        match &item {
            InlineItem::Text(text_run) => {
                self.width += text_run.width;
            }
            InlineItem::Atomic { fragment, .. } => {
                self.width += fragment.rect().size.width;
            }
            _ => {}
        }

        self.items.push(item);
    }

    fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    fn finish(self) -> Line {
        Line {
            items: self.items,
            width: self.width,
        }
    }

    fn finish_at_width(self, width: f32) -> Line {
        Line {
            items: self.items,
            width,
        }
    }
}
```

### Step 3: Implement Baseline Alignment (Day 4-5)

**File: `src/layout/contexts/inline/baseline.rs`**

```rust
//! Baseline alignment algorithm
//!
//! CSS 2.1 Section 10.8: Line height calculations
//! https://www.w3.org/TR/CSS21/visudet.html#line-height

use super::{Line, InlineItem, PositionedLine};
use crate::tree::FragmentNode;
use crate::text::{FontContext, FontMetrics};
use crate::geometry::{Point, Rect};
use crate::error::Result;

/// Baseline aligner
///
/// Aligns inline boxes within a line according to their baseline.
pub struct BaselineAligner<'a> {
    line: &'a Line,
    font_context: &'a FontContext,
}

impl<'a> BaselineAligner<'a> {
    pub fn new(line: &'a Line, font_context: &'a FontContext) -> Self {
        Self { line, font_context }
    }

    /// Performs baseline alignment
    ///
    /// Algorithm (CSS 2.1 Section 10.8):
    /// 1. Compute line box height (max of all inline box line-heights)
    /// 2. Find dominant baseline (usually the text baseline)
    /// 3. Align each inline box:
    ///    - vertical-align: baseline → align box baseline to line baseline
    ///    - vertical-align: top → align top to line box top
    ///    - vertical-align: bottom → align bottom to line box bottom
    ///    - vertical-align: middle → align box middle to line middle
    ///    - vertical-align: <length> → shift baseline by offset
    pub fn align(&self) -> Result<PositionedLine> {
        // Step 1: Compute metrics for each item
        let mut item_metrics = Vec::new();
        for item in &self.line.items {
            let metrics = self.compute_item_metrics(item)?;
            item_metrics.push(metrics);
        }

        // Step 2: Compute line box height
        let line_height = self.compute_line_height(&item_metrics);

        // Step 3: Find baseline position
        // The baseline is positioned such that the tallest box fits
        let baseline_y = self.compute_baseline_position(&item_metrics, line_height);

        // Step 4: Position each item
        let mut fragments = Vec::new();
        let mut current_x = 0.0;

        for (item, metrics) in self.line.items.iter().zip(item_metrics.iter()) {
            match item {
                InlineItem::Text(text_run) => {
                    // Position text at baseline
                    let y = baseline_y - metrics.ascent;

                    let fragment = FragmentNode::new_text(
                        Rect::new(
                            Point::new(current_x, y),
                            Size::new(text_run.width, metrics.height),
                        ),
                        text_run.shaped_text.clone(),
                    );

                    current_x += text_run.width;
                    fragments.push(fragment);
                }
                InlineItem::Atomic { fragment, .. } => {
                    // Position atomic box
                    let y = self.compute_atomic_y_position(
                        fragment,
                        metrics,
                        baseline_y,
                        line_height,
                    );

                    let mut positioned_fragment = fragment.clone();
                    positioned_fragment.set_position(Point::new(current_x, y));

                    current_x += fragment.rect().size.width;
                    fragments.push(positioned_fragment);
                }
                _ => {}
            }
        }

        Ok(PositionedLine {
            y_offset: 0.0, // Will be set by caller
            height: line_height,
            fragments,
        })
    }

    /// Computes metrics for a single inline item
    fn compute_item_metrics(&self, item: &InlineItem) -> Result<ItemMetrics> {
        match item {
            InlineItem::Text(text_run) => {
                // Get font metrics
                let font_metrics = self.font_context.get_metrics(&text_run.shaped_text.font);

                Ok(ItemMetrics {
                    ascent: font_metrics.ascent,
                    descent: font_metrics.descent,
                    line_height: font_metrics.line_height,
                    height: font_metrics.ascent + font_metrics.descent,
                    baseline_offset: 0.0,
                })
            }
            InlineItem::Atomic { fragment, .. } => {
                // Atomic boxes align their bottom edge to the baseline by default
                let height = fragment.rect().size.height;

                Ok(ItemMetrics {
                    ascent: height, // Entire height is above baseline
                    descent: 0.0,
                    line_height: height,
                    height,
                    baseline_offset: 0.0,
                })
            }
            _ => Ok(ItemMetrics::default()),
        }
    }

    /// Computes the total line box height
    ///
    /// This is the maximum of all inline boxes' line-heights.
    fn compute_line_height(&self, metrics: &[ItemMetrics]) -> f32 {
        metrics.iter()
            .map(|m| m.line_height)
            .fold(0.0, f32::max)
    }

    /// Computes the Y position of the baseline within the line box
    ///
    /// The baseline is positioned so that all boxes fit within the line box.
    fn compute_baseline_position(&self, metrics: &[ItemMetrics], line_height: f32) -> f32 {
        // Find the maximum ascent
        let max_ascent = metrics.iter()
            .map(|m| m.ascent)
            .fold(0.0, f32::max);

        // Baseline is positioned at max_ascent from top
        // This ensures all text baselines align and everything fits
        max_ascent
    }

    /// Computes Y position for an atomic inline box
    fn compute_atomic_y_position(
        &self,
        fragment: &FragmentNode,
        metrics: &ItemMetrics,
        baseline_y: f32,
        line_height: f32,
    ) -> f32 {
        // TODO: Handle vertical-align property
        // For now, align bottom to baseline
        baseline_y - fragment.rect().size.height
    }
}

/// Metrics for a single inline item
#[derive(Debug, Clone)]
struct ItemMetrics {
    /// Distance from baseline to top (positive)
    ascent: f32,

    /// Distance from baseline to bottom (positive)
    descent: f32,

    /// Line height (may be larger than ascent + descent due to leading)
    line_height: f32,

    /// Total height (ascent + descent)
    height: f32,

    /// Baseline offset (for vertical-align)
    baseline_offset: f32,
}

impl Default for ItemMetrics {
    fn default() -> Self {
        Self {
            ascent: 0.0,
            descent: 0.0,
            line_height: 0.0,
            height: 0.0,
            baseline_offset: 0.0,
        }
    }
}
```

### Step 4: Text Shaping Integration (Day 6-7)

**File: `src/layout/contexts/inline/text_shaping.rs`**

```rust
//! Text shaping
//!
//! Converts text strings into shaped glyph runs using font metrics.

use crate::text::{FontContext, ShapedText, GlyphInfo};
use crate::style::ComputedStyle;
use crate::error::Result;

/// Text shaper
///
/// Responsible for measuring text and converting it to positioned glyphs.
pub struct TextShaper {
    // TODO: Cache shaped text runs
}

impl TextShaper {
    pub fn new() -> Self {
        Self {}
    }

    /// Shape a text string
    ///
    /// This is where we integrate with HarfBuzz or rustybuzz for real shaping.
    pub fn shape(
        &self,
        text: &str,
        style: &ComputedStyle,
        font_context: &FontContext,
    ) -> Result<ShapedText> {
        // Get font from style
        let font = font_context.get_font(&style.font_family, &style.font_weight, &style.font_style);

        // TODO: Use real text shaping library (rustybuzz)
        // For now, simple character-by-character layout
        let glyphs = self.simple_shape(text, &font)?;

        Ok(ShapedText {
            text: text.to_string(),
            font,
            glyphs,
            font_size: style.font_size,
        })
    }

    /// Simple character-by-character shaping (placeholder)
    ///
    /// Real implementation would use rustybuzz for proper text shaping.
    fn simple_shape(&self, text: &str, font: &Font) -> Result<Vec<GlyphInfo>> {
        let mut glyphs = Vec::new();
        let mut x_offset = 0.0;

        for ch in text.chars() {
            let glyph_id = font.get_glyph_id(ch);
            let advance = font.get_advance(glyph_id);

            glyphs.push(GlyphInfo {
                glyph_id,
                x_offset,
                y_offset: 0.0,
                x_advance: advance,
                y_advance: 0.0,
                cluster: 0, // TODO: Track clusters
            });

            x_offset += advance;
        }

        Ok(glyphs)
    }
}
```

### Step 5: Comprehensive Tests (Day 8-10)

**File: `tests/layout/inline_layout_test.rs`**

```rust
//! Tests for Inline Formatting Context

use fastrender::layout::contexts::InlineFormattingContext;
use fastrender::tree::BoxNode;
use fastrender::text::FontContext;
use fastrender::style::ComputedStyle;

#[test]
fn test_single_line_text() {
    let fc = InlineFormattingContext::new();
    let font_ctx = FontContext::new();

    // Create a text box
    let text_box = BoxNode::new_text("Hello, world!");
    let container = BoxNode::new_block(
        default_style(),
        vec![text_box],
    );

    let constraints = LayoutConstraints::from_containing_block(400.0, 600.0);
    let fragment = fc.layout(&container, &constraints, &font_ctx).unwrap();

    // Should create a single line
    assert_eq!(fragment.children.len(), 1);
}

#[test]
fn test_line_breaking() {
    let fc = InlineFormattingContext::new();
    let font_ctx = FontContext::new();

    // Create text that's too long for one line
    let long_text = "This is a very long line of text that should wrap to multiple lines";
    let text_box = BoxNode::new_text(long_text);
    let container = BoxNode::new_block(
        default_style(),
        vec![text_box],
    );

    // Narrow containing block
    let constraints = LayoutConstraints::from_containing_block(200.0, 600.0);
    let fragment = fc.layout(&container, &constraints, &font_ctx).unwrap();

    // Should create multiple lines
    assert!(fragment.children.len() > 1);
}

#[test]
fn test_hard_line_break() {
    let fc = InlineFormattingContext::new();
    let font_ctx = FontContext::new();

    // Text with <br>
    let text1 = BoxNode::new_text("Line 1");
    let br = BoxNode::new_line_break();
    let text2 = BoxNode::new_text("Line 2");

    let container = BoxNode::new_block(
        default_style(),
        vec![text1, br, text2],
    );

    let constraints = LayoutConstraints::from_containing_block(400.0, 600.0);
    let fragment = fc.layout(&container, &constraints, &font_ctx).unwrap();

    // Should create exactly 2 lines
    assert_eq!(fragment.children.len(), 2);
}

#[test]
fn test_inline_box_wrapping() {
    let fc = InlineFormattingContext::new();
    let font_ctx = FontContext::new();

    // <span>Long text...</span>
    let text = BoxNode::new_text("This is a very long span that wraps");
    let span = BoxNode::new_inline(
        style_with_background(Color::red()),
        vec![text],
    );

    let container = BoxNode::new_block(default_style(), vec![span]);

    // Narrow width forces wrapping
    let constraints = LayoutConstraints::from_containing_block(100.0, 600.0);
    let fragment = fc.layout(&container, &constraints, &font_ctx).unwrap();

    // Span should be split across multiple lines
    // Each line segment should have the red background
    assert!(fragment.children.len() > 1);
}

#[test]
fn test_inline_block_on_line() {
    let fc = InlineFormattingContext::new();
    let font_ctx = FontContext::new();

    // Text before
    let text1 = BoxNode::new_text("Before ");

    // Inline-block box
    let inline_block = BoxNode::new_inline_block(
        style_with_size(50.0, 30.0),
        vec![],
    );

    // Text after
    let text2 = BoxNode::new_text(" after");

    let container = BoxNode::new_block(
        default_style(),
        vec![text1, inline_block, text2],
    );

    let constraints = LayoutConstraints::from_containing_block(400.0, 600.0);
    let fragment = fc.layout(&container, &constraints, &font_ctx).unwrap();

    // Should be on same line
    let line = &fragment.children[0];
    assert_eq!(line.children.len(), 3);

    // Inline-block should be baseline-aligned
    // (Bottom edge aligned to text baseline)
    let text_baseline = get_baseline_y(&line.children[0]);
    let inline_block_bottom = line.children[1].rect().bottom();
    assert!((text_baseline - inline_block_bottom).abs() < 1.0);
}

#[test]
fn test_baseline_alignment_with_different_font_sizes() {
    let fc = InlineFormattingContext::new();
    let font_ctx = FontContext::new();

    // Normal text
    let text1 = BoxNode::new_text_with_style(
        "Normal ",
        style_with_font_size(16.0),
    );

    // Large text
    let text2 = BoxNode::new_text_with_style(
        "LARGE",
        style_with_font_size(32.0),
    );

    // Small text
    let text3 = BoxNode::new_text_with_style(
        " small",
        style_with_font_size(10.0),
    );

    let container = BoxNode::new_block(
        default_style(),
        vec![text1, text2, text3],
    );

    let constraints = LayoutConstraints::from_containing_block(400.0, 600.0);
    let fragment = fc.layout(&container, &constraints, &font_ctx).unwrap();

    // All text should be baseline-aligned despite different sizes
    let line = &fragment.children[0];
    let baseline1 = get_baseline_y(&line.children[0]);
    let baseline2 = get_baseline_y(&line.children[1]);
    let baseline3 = get_baseline_y(&line.children[2]);

    assert!((baseline1 - baseline2).abs() < 0.1);
    assert!((baseline2 - baseline3).abs() < 0.1);
}

// Helper functions
fn default_style() -> ComputedStyle {
    ComputedStyle {
        display: Display::Block,
        font_size: 16.0,
        line_height: LineHeight::Normal,
        ..Default::default()
    }
}

fn get_baseline_y(fragment: &FragmentNode) -> f32 {
    // TODO: Get actual baseline from fragment
    fragment.rect().origin.y + fragment.rect().size.height * 0.8
}
```

## Acceptance Criteria

- [ ] Single line of text lays out correctly
- [ ] Line breaking works at word boundaries
- [ ] Hard breaks (`<br>`) force line breaks
- [ ] Inline boxes (`<span>`) participate in inline layout
- [ ] Inline-block boxes are treated as atomic inline boxes
- [ ] Baseline alignment works for text of different sizes
- [ ] Text with different fonts aligns correctly
- [ ] Line-height property is respected
- [ ] Vertical-align property works (at least: baseline, top, bottom, middle)
- [ ] Text measurement integrates with FontContext
- [ ] All tests pass: `cargo test inline_layout`
- [ ] Code follows 10-code-standards.md

## Common Pitfalls

### Pitfall 1: Forgetting to Split Inline Boxes Across Lines

**Wrong:**
```rust
// Inline box only appears on first line
if box is inline && line wraps {
    // BUG: Forgot to create continuation on second line
}
```

**Right:**
```rust
// Inline box must be split into fragments for each line
// Each line gets a fragment with appropriate padding/border/margin
for line in lines {
    create_inline_box_fragment_for_line(box, line);
}
```

### Pitfall 2: Not Handling Empty Lines Correctly

**Wrong:**
```rust
if text.is_empty() {
    // Skip creating line box - BUG!
}
```

**Right:**
```rust
// Empty lines still create line boxes with line-height
// <br> creates an empty line, but it should still have height
create_line_box_with_strut_height();
```

### Pitfall 3: Incorrect Baseline for Inline-Block

**Wrong:**
```rust
// Align top of inline-block to baseline
y = baseline_y;
```

**Right:**
```rust
// Align bottom margin edge of inline-block to baseline (CSS 2.1 Section 10.8.1)
y = baseline_y - inline_block.height - inline_block.margin_bottom;
```

### Pitfall 4: Not Accounting for Line-Height Leading

**Wrong:**
```rust
line_height = font.ascent + font.descent;
```

**Right:**
```rust
// Line height is NOT just ascent + descent
// It includes leading (extra space above and below)
let leading = line_height - (font.ascent + font.descent);
let half_leading = leading / 2.0;
// Add half-leading above and below
```

## Performance Considerations

Inline layout can be expensive for large documents:

1. **Cache shaped text runs** - Don't re-shape on every layout
2. **Incremental line breaking** - Only re-break changed lines
3. **Lazy glyph shaping** - Shape on demand, not upfront
4. **Use spatial indices** - For hit testing and selection

## Integration with Text Rendering

The inline layout must produce information needed for text rendering:

```rust
pub struct TextFragment {
    /// Positioned glyphs
    pub glyphs: Vec<PositionedGlyph>,

    /// Bounding box
    pub rect: Rect,

    /// Font and size
    pub font: Font,
    pub font_size: f32,

    /// Color
    pub color: Color,
}
```

## Next Steps

After inline layout is complete:
- **02-flex-layout.md** - Flexbox (Taffy wrapper)
- **02-positioned-layout.md** - For absolute positioning within inline context

## References

- **CSS 2.1 Section 9.4.2:** Inline Formatting Contexts
- **CSS 2.1 Section 10.8:** Line height calculations
- **CSS Inline Layout Module Level 3:** https://www.w3.org/TR/css-inline-3/
- **CSS Text Module Level 3:** https://www.w3.org/TR/css-text-3/ (line breaking)
- **Servo's inline layout:** `components/layout_2020/flow/inline.rs`
- **WebKit's inline layout:** `Source/WebCore/layout/integration/inline/`

---

**Last Updated:** 2025-01-19
**Status:** Ready for Implementation
