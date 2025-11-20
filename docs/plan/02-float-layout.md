# Phase 2: Float Layout

**Duration:** Week 5-6 of Phase 2 (7-10 days)
**Prerequisites:**
- Phase 1 complete (type system, formatting contexts, box generation)
- Block layout complete
- Inline layout complete
**Dependencies:**
- FormattingContext trait (for integrating with block/inline layout)
- BoxNode, FragmentNode types
- InlineFormattingContext (floats interact with line boxes)
**Output:** CSS float positioning implementation

## Objectives

Implement CSS float positioning - one of the most complex CSS features. Floats are **not** a formatting context themselves, but rather a positioning scheme that affects block and inline layout.

**Key Insight:** Floats are a property of boxes, not a layout mode. They affect how other boxes flow around them.

## Context

Floats remove boxes from normal flow and shift them to the left or right edge of their containing block. Other content flows around the float.

**From CSS 2.1 Section 9.5:**
> "A float is a box that is shifted to the left or right on the current line. The most interesting characteristic of a float (or 'floated' or 'floating' box) is that content may flow along its side (or be prohibited from doing so by the 'clear' property)."

**Examples of float usage:**
- `float: left` - Float box to left, content flows on right
- `float: right` - Float box to right, content flows on left
- `float: none` - Normal flow (default)
- `clear: both` - Box moves below all floats

**Common use cases:**
- Text wrapping around images
- Multi-column layouts (historical, now use grid/flex)
- Sidebars

## The Problem V1 Has

V1 uses Taffy, which has **no concept of floats**. This makes proper float layout impossible.

## The Solution

Implement float positioning as an **extension to block and inline layout**:
1. Track floats during layout
2. Adjust available space for inline layout when floats are present
3. Implement clear property to move boxes below floats

## CSS Specification References

**Primary:**
- **CSS 2.1 Section 9.5:** Floats
- **CSS 2.1 Section 10.3.5:** Floating, non-replaced elements width
- **CSS 2.1 Section 10.6.2:** Floating, non-replaced elements height

**Key Concepts:**
- **Float positioning rules** - Where a float can be placed
- **Clearance** - Extra space above cleared element
- **Shrink-to-fit** - Width computation for floats
- **Float stacking** - Multiple floats in sequence

## Step-by-Step Implementation

### Step 1: Float Context Data Structure (Day 1)

Floats must be tracked during layout so that subsequent boxes can avoid them.

**File: `src/layout/float_context.rs`**

```rust
//! Float context
//!
//! Tracks floating boxes during layout so content can flow around them.

use crate::geometry::{Rect, Point};
use crate::tree::FragmentNode;

/// Float context
///
/// Maintains the list of active floats in the current block formatting context.
/// Used to compute available space for inline layout and clearance for blocks.
#[derive(Debug, Clone)]
pub struct FloatContext {
    /// All active floats, in order they were encountered
    floats: Vec<FloatInfo>,

    /// Current Y position in the block formatting context
    current_y: f32,
}

/// Information about a single float
#[derive(Debug, Clone)]
pub struct FloatInfo {
    /// The float's fragment (positioned rectangle)
    pub fragment: FragmentNode,

    /// Which side the float is on
    pub side: FloatSide,

    /// Top edge Y position
    pub top: f32,

    /// Bottom edge Y position
    pub bottom: f32,

    /// Left edge X position (for left floats, this is containing block edge)
    pub left: f32,

    /// Right edge X position (for right floats, this is containing block edge)
    pub right: f32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FloatSide {
    Left,
    Right,
}

impl FloatContext {
    /// Creates a new float context
    pub fn new() -> Self {
        Self {
            floats: Vec::new(),
            current_y: 0.0,
        }
    }

    /// Adds a float to the context
    pub fn add_float(&mut self, fragment: FragmentNode, side: FloatSide) {
        let rect = fragment.rect();

        let float_info = FloatInfo {
            fragment,
            side,
            top: rect.origin.y,
            bottom: rect.origin.y + rect.size.height,
            left: rect.origin.x,
            right: rect.origin.x + rect.size.width,
        };

        self.floats.push(float_info);
    }

    /// Updates the current Y position
    ///
    /// Floats above this position can be discarded (no longer affect layout).
    pub fn advance_y(&mut self, y: f32) {
        self.current_y = y;
    }

    /// Computes available width for a line at given Y position
    ///
    /// Returns (available_left_edge, available_width)
    ///
    /// This is used by inline layout to determine where line boxes can be placed.
    pub fn available_width_at_y(
        &self,
        y: f32,
        containing_width: f32,
    ) -> (f32, f32) {
        let mut left_edge = 0.0;
        let mut right_edge = containing_width;

        // Find all floats that overlap this Y position
        for float_info in &self.floats {
            if float_info.top <= y && y < float_info.bottom {
                match float_info.side {
                    FloatSide::Left => {
                        // Left float reduces available space from the left
                        left_edge = left_edge.max(float_info.right);
                    }
                    FloatSide::Right => {
                        // Right float reduces available space from the right
                        right_edge = right_edge.min(float_info.left);
                    }
                }
            }
        }

        let available_width = (right_edge - left_edge).max(0.0);
        (left_edge, available_width)
    }

    /// Computes the Y position to place a float
    ///
    /// CSS 2.1 Section 9.5.1: Rules for positioning floats
    ///
    /// Rules:
    /// 1. Float's top must be at or below the top of any earlier floats
    /// 2. Left float placed as far left as possible
    /// 3. Right float placed as far right as possible
    /// 4. Must not overlap other floats (unless no space available)
    pub fn compute_float_position(
        &self,
        float_width: f32,
        float_height: f32,
        side: FloatSide,
        current_y: f32,
        containing_width: f32,
    ) -> Point {
        // Start at current Y position
        let mut y = current_y;

        // Try to place float at this Y position
        // If it doesn't fit, move down until it does
        loop {
            let (left_edge, available_width) = self.available_width_at_y(y, containing_width);

            if available_width >= float_width {
                // Float fits at this Y position
                let x = match side {
                    FloatSide::Left => left_edge,
                    FloatSide::Right => left_edge + available_width - float_width,
                };

                return Point::new(x, y);
            }

            // Doesn't fit - move down to next float boundary
            match self.next_float_boundary(y) {
                Some(next_y) => y = next_y,
                None => {
                    // No more floats - place at current position anyway
                    // (Float will extend outside containing block if necessary)
                    let x = match side {
                        FloatSide::Left => 0.0,
                        FloatSide::Right => containing_width - float_width,
                    };
                    return Point::new(x, y);
                }
            }
        }
    }

    /// Finds the Y position of the next float boundary below given Y
    ///
    /// Float boundaries are the bottom edges of floats, where available
    /// space might change.
    fn next_float_boundary(&self, y: f32) -> Option<f32> {
        self.floats
            .iter()
            .filter(|f| f.bottom > y)
            .map(|f| f.bottom)
            .min_by(|a, b| a.partial_cmp(b).unwrap())
    }

    /// Computes clearance for a cleared element
    ///
    /// Returns the Y position where the cleared element should be placed.
    ///
    /// CSS 2.1 Section 9.5.2: Controlling flow next to floats: the 'clear' property
    pub fn compute_clearance(&self, clear: ClearSide, current_y: f32) -> f32 {
        let mut max_bottom = current_y;

        for float_info in &self.floats {
            match clear {
                ClearSide::Left => {
                    if float_info.side == FloatSide::Left {
                        max_bottom = max_bottom.max(float_info.bottom);
                    }
                }
                ClearSide::Right => {
                    if float_info.side == FloatSide::Right {
                        max_bottom = max_bottom.max(float_info.bottom);
                    }
                }
                ClearSide::Both => {
                    max_bottom = max_bottom.max(float_info.bottom);
                }
                ClearSide::None => {}
            }
        }

        max_bottom
    }

    /// Gets all active floats
    pub fn floats(&self) -> &[FloatInfo] {
        &self.floats
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClearSide {
    None,
    Left,
    Right,
    Both,
}

impl Default for FloatContext {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_available_width_no_floats() {
        let ctx = FloatContext::new();
        let (left, width) = ctx.available_width_at_y(0.0, 400.0);

        assert_eq!(left, 0.0);
        assert_eq!(width, 400.0);
    }

    #[test]
    fn test_available_width_with_left_float() {
        let mut ctx = FloatContext::new();

        // Add a 100px wide left float at (0, 0)
        let float_fragment = FragmentNode::new(
            Rect::new(Point::new(0.0, 0.0), Size::new(100.0, 50.0)),
            Arc::new(ComputedStyle::default()),
            vec![],
        );
        ctx.add_float(float_fragment, FloatSide::Left);

        // At Y=25 (inside float), available space starts at X=100
        let (left, width) = ctx.available_width_at_y(25.0, 400.0);
        assert_eq!(left, 100.0);
        assert_eq!(width, 300.0); // 400 - 100

        // At Y=60 (below float), available space is full width
        let (left, width) = ctx.available_width_at_y(60.0, 400.0);
        assert_eq!(left, 0.0);
        assert_eq!(width, 400.0);
    }

    #[test]
    fn test_available_width_with_left_and_right_floats() {
        let mut ctx = FloatContext::new();

        // Left float: 100px wide at (0, 0)
        ctx.add_float(
            FragmentNode::new(
                Rect::new(Point::new(0.0, 0.0), Size::new(100.0, 50.0)),
                Arc::new(ComputedStyle::default()),
                vec![],
            ),
            FloatSide::Left,
        );

        // Right float: 80px wide at (320, 0)
        ctx.add_float(
            FragmentNode::new(
                Rect::new(Point::new(320.0, 0.0), Size::new(80.0, 50.0)),
                Arc::new(ComputedStyle::default()),
                vec![],
            ),
            FloatSide::Right,
        );

        // Available space is between the two floats
        let (left, width) = ctx.available_width_at_y(25.0, 400.0);
        assert_eq!(left, 100.0);
        assert_eq!(width, 220.0); // 320 - 100
    }

    #[test]
    fn test_clearance_both() {
        let mut ctx = FloatContext::new();

        // Add two floats at different heights
        ctx.add_float(
            FragmentNode::new(
                Rect::new(Point::new(0.0, 0.0), Size::new(100.0, 50.0)),
                Arc::new(ComputedStyle::default()),
                vec![],
            ),
            FloatSide::Left,
        );

        ctx.add_float(
            FragmentNode::new(
                Rect::new(Point::new(300.0, 20.0), Size::new(100.0, 60.0)),
                Arc::new(ComputedStyle::default()),
                vec![],
            ),
            FloatSide::Right,
        );

        // Clear both should go below the tallest float (80px)
        let clear_y = ctx.compute_clearance(ClearSide::Both, 0.0);
        assert_eq!(clear_y, 80.0); // 20 + 60
    }
}
```

### Step 2: Integrate Floats with Block Layout (Day 2-3)

Block layout must be aware of floats for positioning and clearance.

**File: `src/layout/contexts/block.rs` (modifications)**

```rust
// Add to BlockFormattingContext

use crate::layout::float_context::{FloatContext, FloatSide, ClearSide};

impl BlockFormattingContext {
    /// Lays out block children, handling floats
    fn layout_children(
        &self,
        box_node: &BoxNode,
        containing_width: f32,
        constraints: &LayoutConstraints,
        font_context: &FontContext,
    ) -> Result<Vec<FragmentNode>> {
        let mut fragments = Vec::new();
        let mut current_y = 0.0;
        let mut previous_margin_bottom = 0.0;

        // Float context tracks active floats
        let mut float_ctx = FloatContext::new();

        for child in &box_node.children {
            // Handle clearance
            if let Some(clear) = self.get_clear(&child.style) {
                let clear_y = float_ctx.compute_clearance(clear, current_y);
                if clear_y > current_y {
                    // Add clearance
                    current_y = clear_y;
                    previous_margin_bottom = 0.0; // Clearance prevents margin collapsing
                }
            }

            // Check if child is floated
            if let Some(float_side) = self.get_float(&child.style) {
                // Layout the float
                let float_fragment = self.layout_float(
                    child,
                    containing_width,
                    &float_ctx,
                    current_y,
                    font_context,
                )?;

                // Add to float context
                float_ctx.add_float(float_fragment.clone(), float_side);

                // Floats don't affect current_y or margin collapsing
                fragments.push(float_fragment);
                continue;
            }

            // Normal (non-floated) child
            let margin_top = self.get_margin_top(&child.style, containing_width);
            let collapsed_margin = self.collapse_margins(previous_margin_bottom, margin_top);
            current_y += collapsed_margin;

            // Layout child (may need to avoid floats)
            let child_fragment = self.layout_child_with_floats(
                child,
                containing_width,
                current_y,
                &float_ctx,
                constraints,
                font_context,
            )?;

            current_y += child_fragment.rect().size.height;
            previous_margin_bottom = self.get_margin_bottom(&child.style, containing_width);

            fragments.push(child_fragment);
        }

        Ok(fragments)
    }

    /// Layouts a floated box
    fn layout_float(
        &self,
        box_node: &BoxNode,
        containing_width: f32,
        float_ctx: &FloatContext,
        current_y: f32,
        font_context: &FontContext,
    ) -> Result<FragmentNode> {
        // Floats use shrink-to-fit width (CSS 2.1 Section 10.3.5)
        let float_width = self.compute_shrink_to_fit_width(
            box_node,
            containing_width,
            font_context,
        )?;

        // Layout float content
        let float_constraints = LayoutConstraints {
            available_width: Some(float_width),
            available_height: None,
            percentage_base_width: float_width,
            percentage_base_height: 0.0,
        };

        let fc = FormattingContextFactory::get_for_box(box_node);
        let mut fragment = fc.layout(box_node, &float_constraints, font_context)?;

        // Compute float position
        let float_side = self.get_float(&box_node.style).unwrap();
        let position = float_ctx.compute_float_position(
            float_width,
            fragment.rect().size.height,
            float_side,
            current_y,
            containing_width,
        );

        // Position the float
        fragment.set_position(position);

        Ok(fragment)
    }

    /// Computes shrink-to-fit width for a float
    ///
    /// CSS 2.1 Section 10.3.5:
    /// shrink-to-fit width = min(max(preferred minimum width, available width), preferred width)
    fn compute_shrink_to_fit_width(
        &self,
        box_node: &BoxNode,
        available_width: f32,
        font_context: &FontContext,
    ) -> Result<f32> {
        let fc = FormattingContextFactory::get_for_box(box_node);

        // Preferred minimum width (min-content)
        let min_width = fc.compute_intrinsic_size(box_node, Axis::Horizontal, font_context)?;

        // Preferred width (max-content)
        let max_width = fc.compute_intrinsic_size(box_node, Axis::Horizontal, font_context)?;

        // Apply formula
        Ok(min_width.max(available_width).min(max_width))
    }

    /// Gets the float value from style
    fn get_float(&self, style: &ComputedStyle) -> Option<FloatSide> {
        match style.float {
            Float::Left => Some(FloatSide::Left),
            Float::Right => Some(FloatSide::Right),
            Float::None => None,
        }
    }

    /// Gets the clear value from style
    fn get_clear(&self, style: &ComputedStyle) -> Option<ClearSide> {
        match style.clear {
            Clear::Left => Some(ClearSide::Left),
            Clear::Right => Some(ClearSide::Right),
            Clear::Both => Some(ClearSide::Both),
            Clear::None => None,
        }
    }

    /// Layouts a child, taking floats into account
    fn layout_child_with_floats(
        &self,
        box_node: &BoxNode,
        containing_width: f32,
        y: f32,
        float_ctx: &FloatContext,
        constraints: &LayoutConstraints,
        font_context: &FontContext,
    ) -> Result<FragmentNode> {
        // For inline content, available width is reduced by floats
        // For block boxes, they still take full width but inline content inside them is affected

        let fc = FormattingContextFactory::get_for_box(box_node);

        // Pass float context to child layout
        let child_constraints = LayoutConstraints {
            available_width: Some(containing_width),
            available_height: constraints.available_height,
            percentage_base_width: containing_width,
            percentage_base_height: constraints.percentage_base_height,
        };

        // TODO: Pass float_ctx through constraints for inline layout
        let mut fragment = fc.layout(box_node, &child_constraints, font_context)?;

        // Position at Y
        fragment.set_position(Point::new(0.0, y));

        Ok(fragment)
    }
}
```

### Step 3: Integrate Floats with Inline Layout (Day 4-5)

Inline layout must avoid floats when breaking lines.

**File: `src/layout/contexts/inline/line_breaker.rs` (modifications)**

```rust
use crate::layout::float_context::FloatContext;

impl LineBreaker {
    /// Breaks items into lines, avoiding floats
    pub fn break_lines_with_floats(
        &mut self,
        items: &[InlineItem],
        containing_width: f32,
        float_ctx: &FloatContext,
        start_y: f32,
        font_context: &FontContext,
    ) -> Result<Vec<Line>> {
        let mut lines = Vec::new();
        let mut current_line = LineBuilder::new();
        let mut current_y = start_y;

        for item in items {
            // Get available width at current Y position
            let (left_offset, available_width) = float_ctx.available_width_at_y(
                current_y,
                containing_width,
            );

            match item {
                InlineItem::Text(text_run) => {
                    let new_width = current_line.width + text_run.width;

                    if new_width <= available_width {
                        // Fits on current line
                        current_line.add_item(item.clone());
                    } else {
                        // Doesn't fit - finish current line and start new one
                        let line = current_line.finish_with_offset(left_offset);
                        current_y += line.height;
                        lines.push(line);

                        // Start new line
                        current_line = LineBuilder::new();
                        current_line.add_item(item.clone());
                    }
                }
                InlineItem::HardBreak => {
                    let line = current_line.finish_with_offset(left_offset);
                    current_y += line.height;
                    lines.push(line);

                    current_line = LineBuilder::new();
                }
                _ => {
                    current_line.add_item(item.clone());
                }
            }
        }

        // Finish last line
        if !current_line.is_empty() {
            let (left_offset, _) = float_ctx.available_width_at_y(current_y, containing_width);
            lines.push(current_line.finish_with_offset(left_offset));
        }

        Ok(lines)
    }
}

struct LineBuilder {
    items: Vec<InlineItem>,
    width: f32,
    height: f32,
}

impl LineBuilder {
    fn finish_with_offset(self, left_offset: f32) -> Line {
        Line {
            items: self.items,
            width: self.width,
            height: self.height,
            left_offset, // NEW: Offset from containing block left edge
        }
    }
}

pub struct Line {
    pub items: Vec<InlineItem>,
    pub width: f32,
    pub height: f32,
    pub left_offset: f32, // NEW: For floats
}
```

### Step 4: Comprehensive Tests (Day 6-7)

**File: `tests/layout/float_layout_test.rs`**

```rust
//! Tests for float layout

use fastrender::layout::contexts::BlockFormattingContext;
use fastrender::tree::BoxNode;
use fastrender::style::*;

#[test]
fn test_left_float_basic() {
    let fc = BlockFormattingContext::new();
    let font_ctx = FontContext::new();

    // Left-floated box
    let float_box = create_float_box(100.0, 50.0, Float::Left);

    // Normal block after float
    let normal_box = create_block(200.0, 100.0);

    let container = BoxNode::new_block(
        default_style(),
        vec![float_box, normal_box],
    );

    let constraints = LayoutConstraints::from_containing_block(400.0, 600.0);
    let fragment = fc.layout(&container, &constraints, &font_ctx).unwrap();

    // Float should be at (0, 0)
    assert_eq!(fragment.children[0].rect().origin.x, 0.0);
    assert_eq!(fragment.children[0].rect().origin.y, 0.0);

    // Normal box still at left edge (floats don't affect block boxes)
    assert_eq!(fragment.children[1].rect().origin.x, 0.0);

    // But inline content in normal box would be offset
    // (tested separately)
}

#[test]
fn test_right_float_basic() {
    let fc = BlockFormattingContext::new();
    let font_ctx = FontContext::new();

    let float_box = create_float_box(100.0, 50.0, Float::Right);
    let normal_box = create_block(200.0, 100.0);

    let container = BoxNode::new_block(
        default_style(),
        vec![float_box, normal_box],
    );

    let constraints = LayoutConstraints::from_containing_block(400.0, 600.0);
    let fragment = fc.layout(&container, &constraints, &font_ctx).unwrap();

    // Right float at right edge
    assert_eq!(fragment.children[0].rect().origin.x, 300.0); // 400 - 100
}

#[test]
fn test_multiple_left_floats() {
    let fc = BlockFormattingContext::new();
    let font_ctx = FontContext::new();

    let float1 = create_float_box(100.0, 50.0, Float::Left);
    let float2 = create_float_box(80.0, 40.0, Float::Left);

    let container = BoxNode::new_block(
        default_style(),
        vec![float1, float2],
    );

    let constraints = LayoutConstraints::from_containing_block(400.0, 600.0);
    let fragment = fc.layout(&container, &constraints, &font_ctx).unwrap();

    // First float at left
    assert_eq!(fragment.children[0].rect().origin.x, 0.0);

    // Second float next to first
    assert_eq!(fragment.children[1].rect().origin.x, 100.0);
    assert_eq!(fragment.children[1].rect().origin.y, 0.0);
}

#[test]
fn test_float_stacking_when_no_space() {
    let fc = BlockFormattingContext::new();
    let font_ctx = FontContext::new();

    // Three floats, total width > container
    let float1 = create_float_box(200.0, 50.0, Float::Left);
    let float2 = create_float_box(200.0, 50.0, Float::Left);
    let float3 = create_float_box(200.0, 50.0, Float::Left);

    let container = BoxNode::new_block(
        default_style(),
        vec![float1, float2, float3],
    );

    let constraints = LayoutConstraints::from_containing_block(400.0, 600.0);
    let fragment = fc.layout(&container, &constraints, &font_ctx).unwrap();

    // First two fit on first line
    assert_eq!(fragment.children[0].rect().origin.y, 0.0);
    assert_eq!(fragment.children[1].rect().origin.y, 0.0);

    // Third drops to next line
    assert_eq!(fragment.children[2].rect().origin.y, 50.0);
    assert_eq!(fragment.children[2].rect().origin.x, 0.0);
}

#[test]
fn test_clear_left() {
    let fc = BlockFormattingContext::new();
    let font_ctx = FontContext::new();

    let float_box = create_float_box(100.0, 50.0, Float::Left);
    let cleared_box = create_block_with_clear(200.0, 100.0, Clear::Left);

    let container = BoxNode::new_block(
        default_style(),
        vec![float_box, cleared_box],
    );

    let constraints = LayoutConstraints::from_containing_block(400.0, 600.0);
    let fragment = fc.layout(&container, &constraints, &font_ctx).unwrap();

    // Cleared box should be below float
    assert_eq!(fragment.children[1].rect().origin.y, 50.0);
}

#[test]
fn test_clear_both_with_left_and_right_floats() {
    let fc = BlockFormattingContext::new();
    let font_ctx = FontContext::new();

    let left_float = create_float_box(100.0, 50.0, Float::Left);
    let right_float = create_float_box(100.0, 80.0, Float::Right);
    let cleared_box = create_block_with_clear(200.0, 100.0, Clear::Both);

    let container = BoxNode::new_block(
        default_style(),
        vec![left_float, right_float, cleared_box],
    );

    let constraints = LayoutConstraints::from_containing_block(400.0, 600.0);
    let fragment = fc.layout(&container, &constraints, &font_ctx).unwrap();

    // Cleared box should be below both floats (tallest is 80px)
    assert_eq!(fragment.children[2].rect().origin.y, 80.0);
}

#[test]
fn test_text_wraps_around_float() {
    // This tests inline layout integration
    let fc = BlockFormattingContext::new();
    let font_ctx = FontContext::new();

    let float_box = create_float_box(100.0, 100.0, Float::Left);

    // Block with text content
    let text = BoxNode::new_text("This is a long line of text that should wrap around the floated box");
    let text_block = BoxNode::new_block(default_style(), vec![text]);

    let container = BoxNode::new_block(
        default_style(),
        vec![float_box, text_block],
    );

    let constraints = LayoutConstraints::from_containing_block(400.0, 600.0);
    let fragment = fc.layout(&container, &constraints, &font_ctx).unwrap();

    // Text block should have its inline content offset by 100px
    // (for first 100px of height, then full width after)
    // This is tested in the inline layout line positions
    let text_fragment = &fragment.children[1];

    // First line should be offset
    if let Some(first_line) = text_fragment.children.first() {
        assert_eq!(first_line.rect().origin.x, 100.0); // After float
    }
}

// Helper functions
fn create_float_box(width: f32, height: f32, float: Float) -> BoxNode {
    BoxNode::new_block(
        ComputedStyle {
            width: Some(Length::px(width)),
            height: Some(Length::px(height)),
            float,
            ..Default::default()
        },
        vec![],
    )
}

fn create_block_with_clear(width: f32, height: f32, clear: Clear) -> BoxNode {
    BoxNode::new_block(
        ComputedStyle {
            width: Some(Length::px(width)),
            height: Some(Length::px(height)),
            clear,
            ..Default::default()
        },
        vec![],
    )
}
```

## Acceptance Criteria

- [ ] Left floats position correctly
- [ ] Right floats position correctly
- [ ] Multiple floats stack horizontally
- [ ] Floats wrap to next line when no space
- [ ] `clear: left` works
- [ ] `clear: right` works
- [ ] `clear: both` works
- [ ] Text wraps around floats (inline layout integration)
- [ ] Shrink-to-fit width works for floats
- [ ] Floats don't affect block box positioning (only inline content)
- [ ] All tests pass: `cargo test float_layout`
- [ ] Code follows 10-code-standards.md

## Common Pitfalls

### Pitfall 1: Confusing Block Box Position vs Inline Content

**Wrong:**
```rust
// Moving block box to avoid float
block_x = float.right;
```

**Right:**
```rust
// Block box still at left edge (0), but its inline content is offset
block_x = 0;
// Inline content inside block starts at float.right
```

### Pitfall 2: Not Implementing Shrink-to-Fit

**Wrong:**
```rust
// Float takes full containing width
float_width = containing_width;
```

**Right:**
```rust
// Float uses shrink-to-fit (CSS 2.1 Section 10.3.5)
float_width = min(max(min_content, available), max_content);
```

### Pitfall 3: Forgetting Float Stacking Rules

Floats have complex placement rules (CSS 2.1 Section 9.5.1):
1. Left edge of left float must be at or to the right of the left edge of any earlier left floats
2. Top of float must be at or below top of any earlier floats
3. Float must be as high as possible (small Y)
4. Left float must be as far left as possible, right float as far right as possible

### Pitfall 4: Clear Not Creating Clearance

**Wrong:**
```rust
// Just positioning below floats
if clear {
    y = max_float_bottom;
}
```

**Right:**
```rust
// Clear creates "clearance" - extra space above the element
// This prevents margin collapsing
let clearance = max_float_bottom - y;
if clearance > 0 {
    // Add clearance
    y += clearance;
    previous_margin = 0; // Prevents margin collapsing
}
```

## Edge Cases

### Empty Floats

```html
<div style="float: left; width: 0; height: 0"></div>
```

Should still participate in float logic.

### Negative Margins on Floats

```html
<div style="float: left; width: 100px; margin-left: -50px"></div>
```

Can cause float to overlap containing block edge.

### Floats Inside Inline Boxes

```html
<span>Text <div style="float: left">Float</div> more text</span>
```

Float breaks out of the inline box.

## Performance Considerations

1. **Float context is O(n)** for n floats when computing available width
2. **Optimize with spatial index** - Divide space into bands
3. **Prune old floats** - Remove floats that are no longer relevant

## Next Steps

After float layout:
- **02-positioned-layout.md** - Absolute/relative/fixed positioning
- Test floats with all other layout modes
- Optimize float placement algorithm

## References

- **CSS 2.1 Section 9.5:** Floats
- **CSS 2.1 Section 10.3.5:** Floating, non-replaced elements
- **CSS 2.1 Section 10.6.2:** Floating, replaced elements (images)
- **Servo's float implementation:** `components/layout_2020/flow/float.rs`
- **WebKit's float implementation:** `Source/WebCore/rendering/RenderBlockFlow.cpp`

---

**Last Updated:** 2025-01-19
**Status:** Ready for Implementation
