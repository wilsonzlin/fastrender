# Phase 2: Block Layout

**Duration:** Week 1-2 of Phase 2 (7-10 days)
**Prerequisites:**
- Phase 1 complete (type system, formatting contexts, box generation, integration)
**Dependencies:**
- FormattingContext trait
- BoxNode, FragmentNode types
- LayoutConstraints
**Output:** Fully functional Block Formatting Context implementation

## Objectives

Implement the Block Formatting Context (BFC) - the fundamental layout algorithm for vertically stacked block-level elements.

This is the **first real formatting context** implementation and will validate the entire Phase 1 architecture.

## Context

The Block Formatting Context is how browsers layout normal document flow:

- **Block boxes stack vertically**
- **Margins collapse between adjacent blocks**
- **Each block's width fills the containing block** (unless specified)
- **Height grows to contain content**

**From CSS 2.1 Section 9.4.1:**
> "In a block formatting context, boxes are laid out one after the other, vertically, beginning at the top of a containing block."

**Examples of what creates a BFC:**
- Root element (`<html>`)
- Elements with `display: block`
- Elements with `overflow` other than `visible`
- Float elements
- Absolutely positioned elements

## The Problem V1 Has

V1 uses Taffy for all layout, which doesn't implement CSS block layout rules:
- No margin collapsing
- Incorrect width computation for `width: auto`
- No proper height: auto (content-based height)

## The Solution

Implement a dedicated Block Formatting Context that follows CSS 2.1 Section 10 (Visual Formatting Model Details).

## Step-by-Step Implementation

### Step 1: Create Block FC Module (Day 1 Morning)

```bash
mkdir -p /home/user/fastrender/src/layout/contexts
touch /home/user/fastrender/src/layout/contexts/block.rs
```

**File: `src/layout/contexts/block.rs`**

```rust
//! Block Formatting Context implementation
//!
//! CSS Specification: CSS 2.1 Section 9.4.1 - Block Formatting Contexts
//! https://www.w3.org/TR/CSS21/visuren.html#block-formatting

use super::{FormattingContext, FormattingContextType, Axis};
use crate::tree::{BoxNode, FragmentNode, BoxType};
use crate::layout::LayoutConstraints;
use crate::text::FontContext;
use crate::geometry::{Point, Size, Rect};
use crate::error::{Result, Error};

/// Block Formatting Context
///
/// Lays out block-level boxes vertically with margin collapsing.
#[derive(Debug)]
pub struct BlockFormattingContext;

impl BlockFormattingContext {
    /// Creates a new block formatting context
    pub fn new() -> Self {
        Self
    }
}

impl FormattingContext for BlockFormattingContext {
    fn context_type(&self) -> FormattingContextType {
        FormattingContextType::Block
    }

    fn layout(
        &self,
        box_node: &BoxNode,
        constraints: &LayoutConstraints,
        font_context: &FontContext,
    ) -> Result<FragmentNode> {
        // Verify box type
        if !matches!(box_node.box_type, BoxType::Block) {
            return Err(Error::Layout(format!(
                "Block FC can only layout block boxes, got {:?}",
                box_node.box_type
            )));
        }

        // Compute block dimensions
        let width = self.compute_width(box_node, constraints)?;
        let height = self.compute_height(box_node, constraints, font_context)?;

        // Layout children
        let children = self.layout_children(
            box_node,
            width,
            constraints,
            font_context,
        )?;

        // If height is auto, sum children heights
        let final_height = if height.is_none() {
            self.compute_auto_height(&children)
        } else {
            height.unwrap()
        };

        // Create fragment
        let rect = Rect::new(
            Point::zero(), // Position determined by parent
            Size::new(width, final_height),
        );

        Ok(FragmentNode::new(
            rect,
            box_node.style.clone(),
            children,
        ))
    }

    fn compute_intrinsic_size(
        &self,
        box_node: &BoxNode,
        axis: Axis,
        font_context: &FontContext,
    ) -> Result<f32> {
        match axis {
            Axis::Horizontal => {
                // Intrinsic width = widest child's intrinsic width
                let mut max_width = 0.0;
                for child in &box_node.children {
                    // Get child's FC and compute intrinsic size
                    // (Simplified for now)
                    max_width = max_width.max(200.0); // TODO: Actual computation
                }
                Ok(max_width)
            }
            Axis::Vertical => {
                // Intrinsic height = sum of children heights
                // (Would need to actually layout children)
                Ok(0.0) // TODO: Actual computation
            }
        }
    }
}

impl BlockFormattingContext {
    /// Computes the width of a block box
    ///
    /// CSS 2.1 Section 10.3.3: Block-level, non-replaced elements in normal flow
    fn compute_width(
        &self,
        box_node: &BoxNode,
        constraints: &LayoutConstraints,
    ) -> Result<f32> {
        let style = &box_node.style;

        // Get explicit width if specified
        if let Some(width) = style.width {
            return Ok(self.resolve_length(&width, constraints.percentage_base_width));
        }

        // width: auto
        //
        // CSS 2.1 Section 10.3.3:
        // "If 'width' is 'auto', any other 'auto' values become '0' and 'width'
        // follows from the constraint: margin-left + border-left + padding-left +
        // width + padding-right + border-right + margin-right = containing block width"

        let containing_width = constraints.available_width
            .ok_or_else(|| Error::Layout("No containing block width for auto sizing".into()))?;

        let margin_left = self.resolve_margin(&style.margin_left, containing_width);
        let margin_right = self.resolve_margin(&style.margin_right, containing_width);
        let padding_left = self.resolve_length(&style.padding_left, containing_width);
        let padding_right = self.resolve_length(&style.padding_right, containing_width);
        let border_left = style.border_left_width;
        let border_right = style.border_right_width;

        let width = containing_width
            - margin_left
            - margin_right
            - padding_left
            - padding_right
            - border_left
            - border_right;

        Ok(width.max(0.0))
    }

    /// Computes the height of a block box
    ///
    /// CSS 2.1 Section 10.5: Content height
    fn compute_height(
        &self,
        box_node: &BoxNode,
        constraints: &LayoutConstraints,
        _font_context: &FontContext,
    ) -> Result<Option<f32>> {
        let style = &box_node.style;

        if let Some(height) = style.height {
            let resolved = self.resolve_length(&height, constraints.percentage_base_height);
            return Ok(Some(resolved));
        }

        // height: auto - will be computed after laying out children
        Ok(None)
    }

    /// Lays out block children vertically
    ///
    /// CSS 2.1 Section 9.4.1: "boxes are laid out one after the other, vertically"
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

        for child in &box_node.children {
            // Create constraints for child
            let child_constraints = LayoutConstraints {
                available_width: Some(containing_width),
                available_height: constraints.available_height,
                percentage_base_width: containing_width,
                percentage_base_height: constraints.percentage_base_height,
            };

            // Get appropriate FC for child
            let child_fc = self.get_fc_for_box(child);

            // Layout child
            let mut child_fragment = child_fc.layout(child, &child_constraints, font_context)?;

            // Apply margin collapsing
            let margin_top = self.get_margin_top(&child.style, containing_width);
            let collapsed_margin = self.collapse_margins(previous_margin_bottom, margin_top);

            current_y += collapsed_margin;

            // Position child
            child_fragment.set_position(Point::new(0.0, current_y));

            current_y += child_fragment.rect().size.height;

            // Store bottom margin for next iteration
            previous_margin_bottom = self.get_margin_bottom(&child.style, containing_width);

            fragments.push(child_fragment);
        }

        Ok(fragments)
    }

    /// Computes auto height from children
    fn compute_auto_height(&self, children: &[FragmentNode]) -> f32 {
        if children.is_empty() {
            return 0.0;
        }

        // Height = bottom edge of last child
        let last_child = children.last().unwrap();
        let last_rect = last_child.rect();
        last_rect.origin.y + last_rect.size.height
    }

    /// Implements margin collapsing algorithm
    ///
    /// CSS 2.1 Section 8.3.1: Collapsing margins
    fn collapse_margins(&self, margin1: f32, margin2: f32) -> f32 {
        // Simplified: take the maximum of the two margins
        // Full algorithm is more complex (handles negative margins, etc.)
        margin1.max(margin2)
    }

    /// Gets margin-top, resolving percentages and auto
    fn get_margin_top(&self, style: &ComputedStyle, containing_width: f32) -> f32 {
        self.resolve_margin(&style.margin_top, containing_width)
    }

    /// Gets margin-bottom, resolving percentages and auto
    fn get_margin_bottom(&self, style: &ComputedStyle, containing_width: f32) -> f32 {
        self.resolve_margin(&style.margin_bottom, containing_width)
    }

    /// Resolves a length value (handling percentages)
    fn resolve_length(&self, length: &Length, base: f32) -> f32 {
        match length.unit {
            LengthUnit::Px => length.value,
            LengthUnit::Percent => length.value * base / 100.0,
            // TODO: em, rem, etc.
        }
    }

    /// Resolves a margin value (auto becomes 0 in BFC)
    fn resolve_margin(&self, margin: &LengthOrAuto, base: f32) -> f32 {
        match margin {
            LengthOrAuto::Length(length) => self.resolve_length(length, base),
            LengthOrAuto::Auto => 0.0, // In BFC, auto margins are 0 for vertical margins
        }
    }

    /// Gets the appropriate formatting context for a box
    ///
    /// TODO: This should come from the FC factory, not be implemented here
    fn get_fc_for_box(&self, box_node: &BoxNode) -> Box<dyn FormattingContext> {
        match box_node.box_type {
            BoxType::Block => Box::new(BlockFormattingContext::new()),
            _ => todo!("Other formatting contexts not yet implemented"),
        }
    }
}

impl Default for BlockFormattingContext {
    fn default() -> Self {
        Self::new()
    }
}
```

### Step 2: Implement Margin Collapsing (Day 2-3)

Margin collapsing is complex. Here's the full algorithm:

**File: `src/layout/contexts/block/margin_collapse.rs`**

```rust
//! Margin collapsing algorithm
//!
//! CSS 2.1 Section 8.3.1: Collapsing margins
//! https://www.w3.org/TR/CSS21/box.html#collapsing-margins

/// Represents a margin that can collapse
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CollapsibleMargin {
    /// Positive margin value
    pub positive: f32,
    /// Negative margin value (absolute value)
    pub negative: f32,
}

impl CollapsibleMargin {
    /// Creates a collapsible margin from a raw value
    pub fn new(value: f32) -> Self {
        if value >= 0.0 {
            Self {
                positive: value,
                negative: 0.0,
            }
        } else {
            Self {
                positive: 0.0,
                negative: -value,
            }
        }
    }

    /// Collapses this margin with another
    ///
    /// Algorithm from CSS 2.1 Section 8.3.1:
    /// - If both positive: max
    /// - If both negative: max (most negative)
    /// - If mixed: positive max + negative max
    pub fn collapse_with(self, other: Self) -> Self {
        Self {
            positive: self.positive.max(other.positive),
            negative: self.negative.max(other.negative),
        }
    }

    /// Computes the final collapsed margin value
    pub fn resolve(self) -> f32 {
        self.positive - self.negative
    }
}

/// Margin collapse state tracker
///
/// Tracks margins during block layout to handle collapsing correctly.
#[derive(Debug)]
pub struct MarginCollapseState {
    /// Current pending margin that hasn't been applied yet
    pending_margin: CollapsibleMargin,

    /// Whether we're at the start of the block (for first child margin)
    at_start: bool,
}

impl MarginCollapseState {
    /// Creates a new margin collapse state
    pub fn new() -> Self {
        Self {
            pending_margin: CollapsibleMargin::new(0.0),
            at_start: true,
        }
    }

    /// Adds a margin to collapse
    pub fn add_margin(&mut self, margin: f32) {
        self.pending_margin = self.pending_margin.collapse_with(CollapsibleMargin::new(margin));
    }

    /// Consumes the pending margin and returns the collapsed value
    pub fn consume_pending(&mut self) -> f32 {
        let value = self.pending_margin.resolve();
        self.pending_margin = CollapsibleMargin::new(0.0);
        self.at_start = false;
        value
    }

    /// Checks if we're at the start (for first-child margin collapsing with parent)
    pub fn is_at_start(&self) -> bool {
        self.at_start
    }
}

impl Default for MarginCollapseState {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_positive_margins_collapse_to_max() {
        let m1 = CollapsibleMargin::new(10.0);
        let m2 = CollapsibleMargin::new(20.0);
        let collapsed = m1.collapse_with(m2);
        assert_eq!(collapsed.resolve(), 20.0);
    }

    #[test]
    fn test_negative_margins_collapse_to_most_negative() {
        let m1 = CollapsibleMargin::new(-10.0);
        let m2 = CollapsibleMargin::new(-20.0);
        let collapsed = m1.collapse_with(m2);
        assert_eq!(collapsed.resolve(), -20.0);
    }

    #[test]
    fn test_mixed_margins() {
        let m1 = CollapsibleMargin::new(30.0);
        let m2 = CollapsibleMargin::new(-10.0);
        let collapsed = m1.collapse_with(m2);
        assert_eq!(collapsed.resolve(), 20.0); // 30 - 10
    }
}
```

### Step 3: Handle Width Computation Edge Cases (Day 3)

**Over-constrained scenarios:**

When `margin-left`, `margin-right`, and `width` are all specified and don't add up to the containing block width, CSS has specific rules:

```rust
impl BlockFormattingContext {
    /// Handles over-constrained width computation
    ///
    /// CSS 2.1 Section 10.3.3: If all three are specified and don't add up,
    /// the margin in the direction of the containing block's writing mode is ignored.
    fn compute_width_overconstrained(
        &self,
        box_node: &BoxNode,
        containing_width: f32,
    ) -> (f32, f32, f32) { // (margin-left, width, margin-right)
        let style = &box_node.style;

        let width = style.width
            .map(|w| self.resolve_length(&w, containing_width))
            .unwrap_or(0.0);

        let ml = self.resolve_margin(&style.margin_left, containing_width);
        let mr = self.resolve_margin(&style.margin_right, containing_width);

        let padding_left = self.resolve_length(&style.padding_left, containing_width);
        let padding_right = self.resolve_length(&style.padding_right, containing_width);
        let border_left = style.border_left_width;
        let border_right = style.border_right_width;

        let total = ml + border_left + padding_left + width + padding_right + border_right + mr;

        if (total - containing_width).abs() < 0.01 {
            // Not over-constrained
            return (ml, width, mr);
        }

        // Over-constrained: ignore margin-right (for LTR)
        let new_mr = containing_width - ml - border_left - padding_left - width - padding_right - border_right;

        (ml, width, new_mr)
    }
}
```

### Step 4: Write Comprehensive Tests (Day 4-5)

**File: `tests/layout/block_layout_test.rs`**

```rust
//! Tests for Block Formatting Context

use fastrender::layout::contexts::{BlockFormattingContext, FormattingContext};
use fastrender::tree::BoxNode;
use fastrender::layout::LayoutConstraints;
use fastrender::text::FontContext;
use fastrender::style::ComputedStyle;

#[test]
fn test_blocks_stack_vertically() {
    let fc = BlockFormattingContext::new();

    // Create parent with two 100px tall children
    let child1 = create_block(100.0, Some(100.0));
    let child2 = create_block(100.0, Some(100.0));

    let parent = BoxNode::new_block(
        default_style(),
        vec![child1, child2],
    );

    let constraints = LayoutConstraints::from_containing_block(400.0, 600.0);
    let font_ctx = FontContext::new();

    let fragment = fc.layout(&parent, &constraints, &font_ctx).unwrap();

    // Second child should be at y=100 (after first child)
    assert_eq!(fragment.children[1].rect().origin.y, 100.0);
}

#[test]
fn test_width_auto_fills_containing_block() {
    let fc = BlockFormattingContext::new();

    let block = create_block_with_style(ComputedStyle {
        width: None, // auto
        ..default_style()
    });

    let constraints = LayoutConstraints::from_containing_block(400.0, 600.0);
    let font_ctx = FontContext::new();

    let fragment = fc.layout(&block, &constraints, &font_ctx).unwrap();

    // Width should be 400 (containing block width)
    assert_eq!(fragment.rect().size.width, 400.0);
}

#[test]
fn test_explicit_width_respected() {
    let fc = BlockFormattingContext::new();

    let block = create_block(200.0, Some(100.0));

    let constraints = LayoutConstraints::from_containing_block(400.0, 600.0);
    let font_ctx = FontContext::new();

    let fragment = fc.layout(&block, &constraints, &font_ctx).unwrap();

    assert_eq!(fragment.rect().size.width, 200.0);
    assert_eq!(fragment.rect().size.height, 100.0);
}

#[test]
fn test_height_auto_sums_children() {
    let fc = BlockFormattingContext::new();

    let child1 = create_block(100.0, Some(50.0));
    let child2 = create_block(100.0, Some(75.0));
    let child3 = create_block(100.0, Some(25.0));

    let parent = BoxNode::new_block(
        style_with_height(None), // auto height
        vec![child1, child2, child3],
    );

    let constraints = LayoutConstraints::from_containing_block(400.0, 600.0);
    let font_ctx = FontContext::new();

    let fragment = fc.layout(&parent, &constraints, &font_ctx).unwrap();

    // Height should be sum: 50 + 75 + 25 = 150
    assert_eq!(fragment.rect().size.height, 150.0);
}

#[test]
fn test_margin_collapsing() {
    let fc = BlockFormattingContext::new();

    // First child: margin-bottom = 20px
    let child1 = create_block_with_margin(100.0, 50.0, 0.0, 0.0, 0.0, 20.0);

    // Second child: margin-top = 30px
    let child2 = create_block_with_margin(100.0, 50.0, 30.0, 0.0, 0.0, 0.0);

    let parent = BoxNode::new_block(default_style(), vec![child1, child2]);

    let constraints = LayoutConstraints::from_containing_block(400.0, 600.0);
    let font_ctx = FontContext::new();

    let fragment = fc.layout(&parent, &constraints, &font_ctx).unwrap();

    // Second child should be at y = 50 (first child height) + 30 (collapsed margin)
    // NOT 50 + 20 + 30 = 100
    assert_eq!(fragment.children[1].rect().origin.y, 80.0); // 50 + 30
}

#[test]
fn test_negative_margin_collapsing() {
    let fc = BlockFormattingContext::new();

    let child1 = create_block_with_margin(100.0, 50.0, 0.0, 0.0, 0.0, 20.0);
    let child2 = create_block_with_margin(100.0, 50.0, -10.0, 0.0, 0.0, 0.0);

    let parent = BoxNode::new_block(default_style(), vec![child1, child2]);

    let constraints = LayoutConstraints::from_containing_block(400.0, 600.0);
    let font_ctx = FontContext::new();

    let fragment = fc.layout(&parent, &constraints, &font_ctx).unwrap();

    // Collapsed margin: max(20, 0) - max(0, 10) = 20 - 10 = 10
    assert_eq!(fragment.children[1].rect().origin.y, 60.0); // 50 + 10
}

// Helper functions
fn create_block(width: f32, height: Option<f32>) -> BoxNode {
    BoxNode::new_block(
        style_with_size(Some(width), height),
        vec![],
    )
}

fn default_style() -> ComputedStyle {
    ComputedStyle {
        display: Display::Block,
        width: None,
        height: None,
        margin_top: LengthOrAuto::Length(Length::px(0.0)),
        margin_bottom: LengthOrAuto::Length(Length::px(0.0)),
        margin_left: LengthOrAuto::Length(Length::px(0.0)),
        margin_right: LengthOrAuto::Length(Length::px(0.0)),
        ..Default::default()
    }
}
```

### Step 5: Integration with FC Factory (Day 6)

Update the FC factory to return the real Block FC:

```rust
// In src/layout/contexts/factory.rs

use super::block::BlockFormattingContext;

impl FormattingContextFactory {
    pub fn new() -> Self {
        Self {
            block_fc: Arc::new(BlockFormattingContext::new()),
            // Others still todo!()
        }
    }

    pub fn get_context(&self, display: Display) -> Arc<dyn FormattingContext> {
        match display {
            Display::Block => self.block_fc.clone(),
            // Others...
        }
    }
}
```

## Acceptance Criteria

- [ ] Block FC can layout simple block boxes
- [ ] `width: auto` fills containing block correctly
- [ ] `height: auto` sums children heights correctly
- [ ] Blocks stack vertically
- [ ] Margin collapsing works (positive margins)
- [ ] Negative margin collapsing works
- [ ] Percentage widths/heights resolve correctly
- [ ] Padding and borders are accounted for
- [ ] All tests pass: `cargo test block_layout`
- [ ] Can render simple HTML pages end-to-end
- [ ] Code follows 10-code-standards.md

## Common Pitfalls

### Pitfall 1: Forgetting Margin Collapsing

**Wrong:**
```rust
current_y += margin_top;
current_y += child_height;
current_y += margin_bottom;
```

**Right:**
```rust
let collapsed = collapse_margins(previous_margin_bottom, current_margin_top);
current_y += collapsed;
current_y += child_height;
previous_margin_bottom = current_margin_bottom;
```

### Pitfall 2: Not Handling Percentage Heights Without Containing Block Height

**Wrong:**
```rust
if let Some(height) = style.height {
    return resolve_length(height, containing_height); // May be 0!
}
```

**Right:**
```rust
if let Some(height_value) = style.height {
    if height_value.unit == LengthUnit::Percent && containing_height == 0.0 {
        // Percentage height with no containing height = treat as auto
        return None;
    }
    return Some(resolve_length(height_value, containing_height));
}
```

## Next Steps

- **02-inline-layout.md** - Inline Formatting Context (more complex)
- **02-table-layout.md** - Table Formatting Context (fixes V1's original sin)

## References

- **CSS 2.1 Section 9.4.1:** Block Formatting Contexts
- **CSS 2.1 Section 10.3.3:** Block width computation
- **CSS 2.1 Section 10.5:** Content height
- **CSS 2.1 Section 8.3.1:** Collapsing margins
- **Servo's block layout:** `components/layout_2020/flow/block.rs`

---

**Last Updated:** 2025-01-19
**Status:** Ready for Implementation
