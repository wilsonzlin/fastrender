# Phase 5: Margin Collapsing

**Duration:** 3-4 days
**Prerequisites:**
- Phase 2 Block Layout complete (02-block-layout.md)
- Box model implemented
**Dependencies:**
- BlockFormattingContext
- ComputedStyle (margin properties)
- FragmentNode positioning
**Output:** Complete CSS 2.1 compliant margin collapsing algorithm

## Objectives

Implement the **full CSS margin collapsing algorithm** as specified in CSS 2.1 Section 8.3.1. This is one of the most complex and subtle parts of CSS layout.

## Context

Margin collapsing is a CSS layout behavior where vertical margins of adjacent block-level boxes combine (collapse) into a single margin. The largest of the margins (or the most negative for negative margins) wins.

**From CSS 2.1 Section 8.3.1:**
> "In CSS, the adjoining margins of two or more boxes (which might or might not be siblings) can combine to form a single margin. Margins that combine this way are said to collapse, and the resulting combined margin is called a collapsed margin."

**Why it exists:**
- Makes spacing between elements more predictable
- Prevents double spacing (e.g., paragraph margins)
- Allows consistent spacing regardless of element order

**Example:**
```html
<p style="margin-bottom: 20px">First paragraph</p>
<p style="margin-top: 30px">Second paragraph</p>
```

Without collapsing: 50px gap (20px + 30px)
**With collapsing: 30px gap** (max(20px, 30px))

## The Problem V1 Has

V1 uses Taffy for block layout, which doesn't implement margin collapsing at all:
- Margins always add up (wrong!)
- No parent-child margin collapsing
- No empty block collapsing
- Makes layouts look wrong compared to browsers

## The Solution

Implement the full margin collapsing algorithm in `BlockFormattingContext`:

1. Track collapsible margins during layout
2. Handle all collapsing scenarios
3. Know when margins DON'T collapse
4. Properly position elements after collapsing

## CSS 2.1 Margin Collapsing Rules

### Rule 1: Only Vertical Margins Collapse

**Horizontal margins never collapse.**

```css
/* These margins collapse */
.block {
  margin-top: 10px;
  margin-bottom: 20px;
}

/* These margins DO NOT collapse */
.inline {
  margin-left: 10px;  /* Never collapses */
  margin-right: 20px; /* Never collapses */
}
```

### Rule 2: Only Block-Level Boxes in Normal Flow

Margins collapse only for:
- Block-level boxes
- In normal flow (not floated, not positioned)
- In a block formatting context

**Margins DO NOT collapse for:**
- Inline boxes
- Floated elements
- Absolutely positioned elements
- Flex items (flex formatting context)
- Grid items (grid formatting context)
- Elements that establish new BFC (overflow: hidden, etc.)

### Rule 3: Three Collapsing Scenarios

#### Scenario A: Adjacent Siblings

Top margin of a box collapses with bottom margin of previous sibling.

```html
<div style="margin-bottom: 20px">Box 1</div>
<div style="margin-top: 30px">Box 2</div>
<!-- Gap between boxes: 30px (not 50px) -->
```

#### Scenario B: Parent and First/Last Child

Parent's top margin collapses with first child's top margin:

```html
<div style="margin-top: 30px">
  <div style="margin-top: 50px">Child</div>
</div>
<!-- Parent's top margin: 50px (not 30px + 50px) -->
```

Parent's bottom margin collapses with last child's bottom margin:

```html
<div style="margin-bottom: 30px">
  <div style="margin-bottom: 50px">Child</div>
</div>
<!-- Parent's bottom margin: 50px -->
```

**But only if:**
- No border separates them
- No padding separates them
- No content separates them
- Parent doesn't establish new BFC

#### Scenario C: Empty Blocks

A block's own top and bottom margins collapse if:
- It has no border
- It has no padding
- It has no content (height: 0 or height: auto with no children)
- No clearance

```html
<div style="margin-top: 20px; margin-bottom: 30px"></div>
<!-- This creates a single 30px margin -->
```

### Rule 4: Negative Margins

When negative margins are involved:

**Algorithm:**
1. Separate positive and negative margins
2. Find maximum positive margin
3. Find maximum (most negative) negative margin
4. Result = max_positive - max_negative

**Example:**
```
Margins: 30px, -10px, 20px, -5px
Positive: 30px, 20px → max = 30px
Negative: -10px, -5px → max = -10px
Result: 30px - 10px = 20px
```

### Rule 5: Margins Don't Collapse When...

Margins do NOT collapse in these cases:

1. **Floated boxes** - Margins of floats never collapse
2. **Absolutely/fixed positioned** - No margin collapsing
3. **BFC boundaries** - Margins don't collapse across BFC boundaries
4. **With padding/border** - Parent-child margins don't collapse if padding/border between
5. **With clearance** - Clearance prevents margin collapsing
6. **Flex/Grid items** - Different formatting context rules
7. **Inline-block boxes** - Not block-level

## Step-by-Step Implementation

### Step 1: Create Margin Collapse Module (Day 1 Morning)

```bash
mkdir -p /home/user/fastrender/src/layout/margin_collapse
touch /home/user/fastrender/src/layout/margin_collapse/mod.rs
```

**File: `src/layout/margin_collapse/mod.rs`**

```rust
//! Margin collapsing algorithm
//!
//! CSS 2.1 Section 8.3.1: Collapsing margins
//! https://www.w3.org/TR/CSS21/box.html#collapsing-margins
//!
//! This module implements the complete margin collapsing algorithm including:
//! - Adjacent sibling margins
//! - Parent-child margins
//! - Empty block margins
//! - Negative margins
//! - Clearance interactions

use crate::style::ComputedStyle;
use std::cmp::Ordering;

/// Represents a set of margins that can collapse together
///
/// CSS margins can be positive or negative. When collapsing multiple margins,
/// we need to track both the largest positive and largest negative separately,
/// then subtract them at the end.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CollapsibleMargin {
    /// Largest positive margin value
    pub positive: f32,

    /// Largest negative margin value (stored as absolute value)
    pub negative: f32,
}

impl CollapsibleMargin {
    /// Creates a new collapsible margin from a single value
    ///
    /// # Arguments
    ///
    /// * `value` - Margin value (can be positive or negative)
    ///
    /// # Returns
    ///
    /// CollapsibleMargin with positive/negative components separated
    pub fn new(value: f32) -> Self {
        if value >= 0.0 {
            Self {
                positive: value,
                negative: 0.0,
            }
        } else {
            Self {
                positive: 0.0,
                negative: -value, // Store as absolute value
            }
        }
    }

    /// Creates a zero margin (no collapsing)
    pub fn zero() -> Self {
        Self {
            positive: 0.0,
            negative: 0.0,
        }
    }

    /// Checks if this margin is zero
    pub fn is_zero(&self) -> bool {
        self.positive == 0.0 && self.negative == 0.0
    }

    /// Collapses this margin with another
    ///
    /// Algorithm from CSS 2.1 Section 8.3.1:
    /// - If both positive: max(a, b)
    /// - If both negative: max(|a|, |b|) with negative sign
    /// - If mixed: max(positive) - max(negative)
    ///
    /// # Arguments
    ///
    /// * `other` - Another margin to collapse with
    ///
    /// # Returns
    ///
    /// New CollapsibleMargin representing the collapsed result
    pub fn collapse_with(self, other: Self) -> Self {
        Self {
            positive: self.positive.max(other.positive),
            negative: self.negative.max(other.negative),
        }
    }

    /// Resolves the final margin value after collapsing
    ///
    /// # Returns
    ///
    /// The actual margin to use (positive - negative)
    pub fn resolve(self) -> f32 {
        self.positive - self.negative
    }

    /// Adds a margin value to this collapsible margin
    pub fn add_margin(&mut self, value: f32) {
        *self = self.collapse_with(CollapsibleMargin::new(value));
    }
}

impl Default for CollapsibleMargin {
    fn default() -> Self {
        Self::zero()
    }
}

/// Margin collapse state tracker
///
/// Tracks margins during block layout to handle all collapsing scenarios:
/// - Adjacent siblings
/// - Parent and first/last child
/// - Empty blocks
#[derive(Debug)]
pub struct MarginCollapseState {
    /// Current pending margin that hasn't been applied yet
    ///
    /// This accumulates margins that are waiting to collapse.
    /// For example, when we encounter a sibling's top margin,
    /// it collapses with the previous sibling's bottom margin.
    pending_margin: CollapsibleMargin,

    /// Whether we're at the start of the block
    ///
    /// True when we haven't positioned any children yet.
    /// Used for parent-first-child margin collapsing.
    at_start: bool,

    /// Whether we can collapse with parent's top margin
    ///
    /// True if:
    /// - Parent has no top border
    /// - Parent has no top padding
    /// - We're at the start
    can_collapse_with_parent_top: bool,

    /// Accumulated margin that will collapse with parent's bottom
    ///
    /// Used when the last child's bottom margin should collapse
    /// with the parent's bottom margin.
    parent_bottom_collapse: Option<CollapsibleMargin>,
}

impl MarginCollapseState {
    /// Creates a new margin collapse state
    ///
    /// # Arguments
    ///
    /// * `can_collapse_with_parent_top` - Whether parent allows top margin collapsing
    ///
    /// # Returns
    ///
    /// New state ready to track margins
    pub fn new(can_collapse_with_parent_top: bool) -> Self {
        Self {
            pending_margin: CollapsibleMargin::zero(),
            at_start: true,
            can_collapse_with_parent_top,
            parent_bottom_collapse: None,
        }
    }

    /// Adds a top margin to the pending margin
    ///
    /// This is called when we encounter a child's top margin.
    /// It collapses with any pending bottom margin from previous sibling.
    ///
    /// # Arguments
    ///
    /// * `margin` - The top margin value
    pub fn add_top_margin(&mut self, margin: f32) {
        self.pending_margin.add_margin(margin);
    }

    /// Consumes the pending margin and returns the collapsed value
    ///
    /// This is called when we're ready to position a child.
    /// The pending margin becomes the actual spacing before the child.
    ///
    /// # Returns
    ///
    /// The collapsed margin to use as spacing
    pub fn consume_pending(&mut self) -> f32 {
        let value = self.pending_margin.resolve();
        self.pending_margin = CollapsibleMargin::zero();
        self.at_start = false;
        value
    }

    /// Records a bottom margin for potential collapsing
    ///
    /// This is called after positioning a child.
    /// The bottom margin might collapse with:
    /// - Next sibling's top margin
    /// - Parent's bottom margin
    ///
    /// # Arguments
    ///
    /// * `margin` - The bottom margin value
    pub fn add_bottom_margin(&mut self, margin: f32) {
        self.pending_margin.add_margin(margin);

        // Also track for parent bottom collapsing
        if let Some(ref mut parent_margin) = self.parent_bottom_collapse {
            parent_margin.add_margin(margin);
        } else {
            self.parent_bottom_collapse = Some(CollapsibleMargin::new(margin));
        }
    }

    /// Checks if we're at the start (for first-child margin collapsing)
    pub fn is_at_start(&self) -> bool {
        self.at_start
    }

    /// Gets the margin that should collapse with parent's bottom
    ///
    /// # Returns
    ///
    /// None if no bottom collapsing should happen,
    /// Some(margin) if parent's bottom margin should collapse
    pub fn get_parent_bottom_collapse(&self) -> Option<f32> {
        self.parent_bottom_collapse.map(|m| m.resolve())
    }

    /// Handles an empty block
    ///
    /// An empty block's top and bottom margins collapse together,
    /// then that collapsed margin collapses with surrounding margins.
    ///
    /// # Arguments
    ///
    /// * `top_margin` - The block's top margin
    /// * `bottom_margin` - The block's bottom margin
    pub fn handle_empty_block(&mut self, top_margin: f32, bottom_margin: f32) {
        // Collapse top and bottom together
        let mut self_collapse = CollapsibleMargin::new(top_margin);
        self_collapse.add_margin(bottom_margin);

        // Then collapse with pending margin
        self.pending_margin = self.pending_margin.collapse_with(self_collapse);
    }

    /// Prevents further margin collapsing
    ///
    /// Called when we encounter something that stops collapsing:
    /// - Border
    /// - Padding
    /// - Content
    /// - Clearance
    pub fn break_collapse(&mut self) {
        self.pending_margin = CollapsibleMargin::zero();
        self.can_collapse_with_parent_top = false;
    }
}

/// Checks if margins can collapse between parent and child
///
/// # Arguments
///
/// * `parent_style` - Parent's computed style
/// * `edge` - Which edge: Top or Bottom
///
/// # Returns
///
/// true if margins can collapse, false otherwise
pub fn can_collapse_with_parent(parent_style: &ComputedStyle, edge: Edge) -> bool {
    match edge {
        Edge::Top => {
            // Margins collapse if:
            // - No top border
            // - No top padding
            parent_style.border_top_width == 0.0
                && parent_style.padding_top.is_zero()
        }
        Edge::Bottom => {
            // Margins collapse if:
            // - No bottom border
            // - No bottom padding
            // - No explicit height (height: auto)
            parent_style.border_bottom_width == 0.0
                && parent_style.padding_bottom.is_zero()
                && parent_style.height.is_none()
        }
    }
}

/// Edge of a box (for margin collapsing)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Edge {
    Top,
    Bottom,
}

/// Checks if a box establishes a new Block Formatting Context
///
/// BFC boundaries prevent margin collapsing.
///
/// # Arguments
///
/// * `style` - Computed style to check
///
/// # Returns
///
/// true if this box creates a new BFC
pub fn establishes_bfc(style: &ComputedStyle) -> bool {
    // From CSS 2.1 Section 9.4.1, a BFC is established by:
    // - Floats
    // - Absolutely positioned elements
    // - Block containers that are not block boxes (inline-blocks, table-cells, etc.)
    // - Block boxes with 'overflow' other than 'visible'

    use crate::style::{Position, Display, Overflow};

    match style.position {
        Position::Absolute | Position::Fixed => return true,
        _ => {}
    }

    if style.float != Float::None {
        return true;
    }

    match style.display {
        Display::InlineBlock | Display::TableCell | Display::TableCaption => return true,
        Display::Flex | Display::InlineFlex | Display::Grid | Display::InlineGrid => return true,
        _ => {}
    }

    if style.overflow != Overflow::Visible {
        return true;
    }

    false
}

/// Checks if a box is an empty block for margin collapsing
///
/// Empty blocks have special collapsing rules where top and bottom
/// margins collapse together.
///
/// # Arguments
///
/// * `style` - Computed style
/// * `has_children` - Whether the box has children
///
/// # Returns
///
/// true if this is an empty block
pub fn is_empty_block(style: &ComputedStyle, has_children: bool) -> bool {
    // A block is empty for margin collapsing if:
    // - No border
    // - No padding
    // - No height
    // - No children (or children are also empty)
    // - No min-height

    if style.border_top_width > 0.0 || style.border_bottom_width > 0.0 {
        return false;
    }

    if !style.padding_top.is_zero() || !style.padding_bottom.is_zero() {
        return false;
    }

    if style.height.is_some() {
        return false;
    }

    if style.min_height.is_some() {
        return false;
    }

    !has_children
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

    #[test]
    fn test_multiple_margins_collapse() {
        // Simulate: 30px, -10px, 20px, -5px
        let mut margin = CollapsibleMargin::new(30.0);
        margin.add_margin(-10.0);
        margin.add_margin(20.0);
        margin.add_margin(-5.0);

        // Max positive: 30, Max negative: 10
        // Result: 30 - 10 = 20
        assert_eq!(margin.resolve(), 20.0);
    }

    #[test]
    fn test_empty_block_self_collapse() {
        let mut state = MarginCollapseState::new(false);

        // Empty block with top: 20px, bottom: 30px
        state.handle_empty_block(20.0, 30.0);

        // Should collapse to 30px (max)
        assert_eq!(state.consume_pending(), 30.0);
    }

    #[test]
    fn test_adjacent_siblings() {
        let mut state = MarginCollapseState::new(false);

        // First child: bottom margin 20px
        state.add_bottom_margin(20.0);

        // Second child: top margin 30px
        state.add_top_margin(30.0);

        // Should collapse to 30px
        assert_eq!(state.consume_pending(), 30.0);
    }
}
```

### Step 2: Integrate with Block Layout (Day 1 Afternoon - Day 2)

Update `BlockFormattingContext` to use margin collapsing:

**File: `src/layout/contexts/block.rs`** (modifications)

```rust
use crate::layout::margin_collapse::{
    MarginCollapseState, CollapsibleMargin, can_collapse_with_parent,
    establishes_bfc, is_empty_block, Edge,
};

impl BlockFormattingContext {
    /// Lays out block children with proper margin collapsing
    fn layout_children(
        &self,
        box_node: &BoxNode,
        containing_width: f32,
        constraints: &LayoutConstraints,
        font_context: &FontContext,
    ) -> Result<Vec<FragmentNode>> {
        // Check if we can collapse with parent's top margin
        let can_collapse_top = can_collapse_with_parent(&box_node.style, Edge::Top);

        let mut state = MarginCollapseState::new(can_collapse_top);
        let mut fragments = Vec::new();
        let mut current_y = 0.0;

        // If parent can collapse with child, and child's top margin collapses
        // with parent's top margin, we need to handle that
        if state.is_at_start() && state.can_collapse_with_parent_top {
            // First child's top margin will collapse with parent's top margin
            // Don't add spacing here, parent will handle it
        }

        for (i, child) in box_node.children.iter().enumerate() {
            // Get child's margins
            let margin_top = self.get_margin_top(&child.style, containing_width);
            let margin_bottom = self.get_margin_bottom(&child.style, containing_width);

            // Check if child establishes new BFC (prevents margin collapsing)
            if establishes_bfc(&child.style) {
                // New BFC: margins don't collapse
                state.break_collapse();

                // Add margin before child
                current_y += margin_top;
            } else {
                // Check if this is an empty block
                if is_empty_block(&child.style, !child.children.is_empty()) {
                    // Empty block: top and bottom margins collapse together
                    state.handle_empty_block(margin_top, margin_bottom);

                    // Don't increment current_y, empty block takes no space
                    // Its collapsed margin will affect next sibling
                    continue;
                }

                // Normal block: add top margin to pending
                state.add_top_margin(margin_top);
            }

            // Consume pending margin and position child
            let margin_before = state.consume_pending();
            current_y += margin_before;

            // Create constraints for child
            let child_constraints = LayoutConstraints {
                available_width: Some(containing_width),
                available_height: constraints.available_height,
                percentage_base_width: containing_width,
                percentage_base_height: constraints.percentage_base_height,
            };

            // Layout child
            let child_fc = self.get_fc_for_box(child);
            let mut child_fragment = child_fc.layout(child, &child_constraints, font_context)?;

            // Position child
            child_fragment.set_position(Point::new(0.0, current_y));
            current_y += child_fragment.rect().size.height;

            fragments.push(child_fragment);

            // Add bottom margin to pending (for next sibling or parent)
            if !establishes_bfc(&child.style) {
                state.add_bottom_margin(margin_bottom);
            } else {
                current_y += margin_bottom;
                state.break_collapse();
            }
        }

        // Handle last child's bottom margin collapsing with parent's bottom
        if can_collapse_with_parent(&box_node.style, Edge::Bottom) {
            // Parent's bottom margin will collapse with last child's bottom margin
            // Store this for parent to handle
            if let Some(collapsed) = state.get_parent_bottom_collapse() {
                // TODO: Pass this back to parent somehow
                // For now, we'll handle it in compute_auto_height
            }
        } else {
            // Can't collapse: add remaining margin to height
            let remaining = state.consume_pending();
            current_y += remaining;
        }

        Ok(fragments)
    }

    /// Computes auto height, accounting for margin collapsing
    fn compute_auto_height(
        &self,
        children: &[FragmentNode],
        collapse_state: &MarginCollapseState,
    ) -> f32 {
        if children.is_empty() {
            return 0.0;
        }

        // Height = bottom edge of last child
        let last_child = children.last().unwrap();
        let mut height = last_child.rect().origin.y + last_child.rect().size.height;

        // If last child's bottom margin doesn't collapse with parent,
        // we need to account for pending margin
        if let Some(bottom_margin) = collapse_state.get_parent_bottom_collapse() {
            // Bottom margin collapses with parent, don't add to height
        } else {
            // Add any remaining pending margin
            // (This happens if there's padding/border preventing collapse)
        }

        height
    }
}
```

### Step 3: Handle Parent-Child Margin Collapsing (Day 2)

Special case: first child's top margin and last child's bottom margin:

```rust
impl BlockFormattingContext {
    /// Layouts a block box with full margin collapsing support
    pub fn layout(
        &self,
        box_node: &BoxNode,
        constraints: &LayoutConstraints,
        font_context: &FontContext,
    ) -> Result<FragmentNode> {
        let style = &box_node.style;

        // Compute width
        let width = self.compute_width(box_node, constraints)?;

        // Get margins
        let margin_top = self.resolve_margin(&style.margin_top, constraints.percentage_base_width);
        let margin_bottom = self.resolve_margin(&style.margin_bottom, constraints.percentage_base_width);

        // Layout children with margin collapsing
        let children = self.layout_children_with_collapsing(
            box_node,
            width,
            constraints,
            font_context,
        )?;

        // Compute height
        let height = if let Some(h) = style.height {
            self.resolve_length(&h, constraints.percentage_base_height)
        } else {
            self.compute_auto_height_with_collapsing(&children, style)
        };

        // Handle margin collapsing with parent
        // If first child's top margin collapsed with our top margin,
        // it's already been handled by our parent

        // Create fragment
        let rect = Rect::new(
            Point::zero(), // Position set by parent
            Size::new(width, height),
        );

        Ok(FragmentNode::new(
            rect,
            style.clone(),
            children,
        ))
    }

    /// Computes auto height with margin collapsing consideration
    fn compute_auto_height_with_collapsing(
        &self,
        children: &[FragmentNode],
        parent_style: &ComputedStyle,
    ) -> f32 {
        if children.is_empty() {
            return 0.0;
        }

        let last_child = children.last().unwrap();
        let height = last_child.rect().origin.y + last_child.rect().size.height;

        // If last child's bottom margin can collapse with ours,
        // don't add it to height (parent will handle it)
        // Otherwise, it's already included in the child's position

        height
    }
}
```

### Step 4: Handle Clearance (Day 3)

Clearance (from `clear` property) prevents margin collapsing:

```rust
/// Checks if clearance is needed for a box
///
/// Clearance is the extra space added above an element to
/// clear past floats. When clearance is present, margin
/// collapsing is prevented.
fn compute_clearance(
    &self,
    box_node: &BoxNode,
    float_context: &FloatContext,
) -> Option<f32> {
    use crate::style::Clear;

    match box_node.style.clear {
        Clear::None => None,
        Clear::Left => {
            let float_bottom = float_context.get_left_float_bottom();
            if float_bottom > 0.0 {
                Some(float_bottom)
            } else {
                None
            }
        }
        Clear::Right => {
            let float_bottom = float_context.get_right_float_bottom();
            if float_bottom > 0.0 {
                Some(float_bottom)
            } else {
                None
            }
        }
        Clear::Both => {
            let float_bottom = float_context.get_both_floats_bottom();
            if float_bottom > 0.0 {
                Some(float_bottom)
            } else {
                None
            }
        }
    }
}

/// Lays out children with clearance handling
fn layout_children_with_clearance(
    &self,
    box_node: &BoxNode,
    containing_width: f32,
    constraints: &LayoutConstraints,
    font_context: &FontContext,
    float_context: &mut FloatContext,
) -> Result<Vec<FragmentNode>> {
    let mut state = MarginCollapseState::new(
        can_collapse_with_parent(&box_node.style, Edge::Top)
    );

    let mut fragments = Vec::new();
    let mut current_y = 0.0;

    for child in &box_node.children {
        // Check for clearance
        if let Some(clearance_y) = self.compute_clearance(child, float_context) {
            // Clearance prevents margin collapsing
            state.break_collapse();

            // Add clearance
            current_y = current_y.max(clearance_y);
        }

        // Rest of layout logic...
        // (same as before)
    }

    Ok(fragments)
}
```

### Step 5: Comprehensive Tests (Day 3-4)

**File: `tests/layout/margin_collapse_test.rs`**

```rust
//! Tests for margin collapsing algorithm
//!
//! Tests all scenarios from CSS 2.1 Section 8.3.1

use fastrender::layout::contexts::BlockFormattingContext;
use fastrender::tree::BoxNode;
use fastrender::style::*;

#[test]
fn test_adjacent_siblings_positive_margins() {
    // Two siblings with positive margins
    let child1 = create_block_with_margins(100.0, 50.0, 0.0, 0.0, 0.0, 20.0);
    let child2 = create_block_with_margins(100.0, 50.0, 30.0, 0.0, 0.0, 0.0);

    let parent = BoxNode::new_block(default_style(), vec![child1, child2]);

    let fc = BlockFormattingContext::new();
    let constraints = LayoutConstraints::from_containing_block(400.0, 600.0);
    let font_ctx = FontContext::new();

    let fragment = fc.layout(&parent, &constraints, &font_ctx).unwrap();

    // Child1 at y=0, height 50
    // Child1 bottom margin: 20px
    // Child2 top margin: 30px
    // Collapsed: max(20, 30) = 30px
    // Child2 should be at y = 0 + 50 + 30 = 80
    assert_eq!(fragment.children[1].rect().origin.y, 80.0);
}

#[test]
fn test_adjacent_siblings_negative_margins() {
    let child1 = create_block_with_margins(100.0, 50.0, 0.0, 0.0, 0.0, 20.0);
    let child2 = create_block_with_margins(100.0, 50.0, -10.0, 0.0, 0.0, 0.0);

    let parent = BoxNode::new_block(default_style(), vec![child1, child2]);

    let fc = BlockFormattingContext::new();
    let constraints = LayoutConstraints::from_containing_block(400.0, 600.0);
    let font_ctx = FontContext::new();

    let fragment = fc.layout(&parent, &constraints, &font_ctx).unwrap();

    // Collapsed margin: max(20, 0) - max(0, 10) = 20 - 10 = 10px
    // Child2 at y = 0 + 50 + 10 = 60
    assert_eq!(fragment.children[1].rect().origin.y, 60.0);
}

#[test]
fn test_parent_first_child_collapse() {
    // Parent with top margin 30px, no border, no padding
    let parent_style = ComputedStyle {
        margin_top: LengthOrAuto::Length(Length::px(30.0)),
        border_top_width: 0.0,
        padding_top: Length::px(0.0),
        ..default_style()
    };

    // Child with top margin 50px
    let child = create_block_with_margins(100.0, 50.0, 50.0, 0.0, 0.0, 0.0);

    let parent = BoxNode::new_block(parent_style, vec![child]);

    // The parent's top margin should collapse with child's top margin
    // Result: max(30, 50) = 50px
    // This is visible when the parent is laid out by its parent
}

#[test]
fn test_parent_child_no_collapse_with_border() {
    // Parent with border (prevents collapse)
    let parent_style = ComputedStyle {
        margin_top: LengthOrAuto::Length(Length::px(30.0)),
        border_top_width: 1.0, // Border prevents collapse
        padding_top: Length::px(0.0),
        ..default_style()
    };

    let child = create_block_with_margins(100.0, 50.0, 50.0, 0.0, 0.0, 0.0);
    let parent = BoxNode::new_block(parent_style, vec![child]);

    let fc = BlockFormattingContext::new();
    let constraints = LayoutConstraints::from_containing_block(400.0, 600.0);
    let font_ctx = FontContext::new();

    let fragment = fc.layout(&parent, &constraints, &font_ctx).unwrap();

    // Child should be at y = 50 (its own top margin)
    // Parent's margin is separate
    assert_eq!(fragment.children[0].rect().origin.y, 50.0);
}

#[test]
fn test_parent_child_no_collapse_with_padding() {
    // Parent with padding (prevents collapse)
    let parent_style = ComputedStyle {
        margin_top: LengthOrAuto::Length(Length::px(30.0)),
        border_top_width: 0.0,
        padding_top: Length::px(10.0), // Padding prevents collapse
        ..default_style()
    };

    let child = create_block_with_margins(100.0, 50.0, 50.0, 0.0, 0.0, 0.0);
    let parent = BoxNode::new_block(parent_style, vec![child]);

    let fc = BlockFormattingContext::new();
    let constraints = LayoutConstraints::from_containing_block(400.0, 600.0);
    let font_ctx = FontContext::new();

    let fragment = fc.layout(&parent, &constraints, &font_ctx).unwrap();

    // Child at y = 50 (its margin) + 10 (parent's padding) = 60
    // Wait, that's not right. Let me reconsider...
    // Actually, child's margin is inside the parent's padding box
    // Child should be at y = 50 from the content edge
    assert_eq!(fragment.children[0].rect().origin.y, 50.0);
}

#[test]
fn test_empty_block_self_collapse() {
    // Empty block with top margin 20px, bottom margin 30px
    let empty_block = BoxNode::new_block(
        ComputedStyle {
            margin_top: LengthOrAuto::Length(Length::px(20.0)),
            margin_bottom: LengthOrAuto::Length(Length::px(30.0)),
            height: None, // Auto height
            border_top_width: 0.0,
            border_bottom_width: 0.0,
            padding_top: Length::px(0.0),
            padding_bottom: Length::px(0.0),
            ..default_style()
        },
        vec![], // No children
    );

    // This empty block's top and bottom margins collapse to 30px
    // When placed between siblings, it creates a 30px gap
}

#[test]
fn test_bfc_prevents_collapse() {
    // Parent establishes BFC (overflow: hidden)
    let parent_style = ComputedStyle {
        overflow: Overflow::Hidden, // Creates BFC
        ..default_style()
    };

    let child = create_block_with_margins(100.0, 50.0, 50.0, 0.0, 0.0, 0.0);
    let parent = BoxNode::new_block(parent_style, vec![child]);

    let fc = BlockFormattingContext::new();
    let constraints = LayoutConstraints::from_containing_block(400.0, 600.0);
    let font_ctx = FontContext::new();

    let fragment = fc.layout(&parent, &constraints, &font_ctx).unwrap();

    // BFC prevents margin collapse
    // Child's margin should not collapse with parent
    assert_eq!(fragment.children[0].rect().origin.y, 50.0);
}

#[test]
fn test_float_prevents_collapse() {
    // Floated element: margins don't collapse
    let float_child = BoxNode::new_block(
        ComputedStyle {
            float: Float::Left,
            margin_top: LengthOrAuto::Length(Length::px(30.0)),
            ..default_style()
        },
        vec![],
    );

    // Float's margins don't participate in collapsing
}

#[test]
fn test_absolute_positioned_no_collapse() {
    let abs_child = BoxNode::new_block(
        ComputedStyle {
            position: Position::Absolute,
            margin_top: LengthOrAuto::Length(Length::px(30.0)),
            ..default_style()
        },
        vec![],
    );

    // Absolutely positioned: no margin collapsing
}

#[test]
fn test_complex_scenario() {
    // Complex scenario from CSS spec:
    // Parent with margin-top: 10px
    // First child with margin-top: 20px
    // Second child with margin-top: 30px, margin-bottom: 15px
    // Third child with margin-top: 5px

    let parent_style = ComputedStyle {
        margin_top: LengthOrAuto::Length(Length::px(10.0)),
        border_top_width: 0.0,
        padding_top: Length::px(0.0),
        ..default_style()
    };

    let child1 = create_block_with_margins(100.0, 50.0, 20.0, 0.0, 0.0, 10.0);
    let child2 = create_block_with_margins(100.0, 50.0, 30.0, 0.0, 0.0, 15.0);
    let child3 = create_block_with_margins(100.0, 50.0, 5.0, 0.0, 0.0, 0.0);

    let parent = BoxNode::new_block(parent_style, vec![child1, child2, child3]);

    let fc = BlockFormattingContext::new();
    let constraints = LayoutConstraints::from_containing_block(400.0, 600.0);
    let font_ctx = FontContext::new();

    let fragment = fc.layout(&parent, &constraints, &font_ctx).unwrap();

    // Parent's 10px collapses with child1's 20px = 20px total
    // Child1 at y=0 (margin already collapsed with parent)

    // Child1 bottom 10px + child2 top 30px = max(10, 30) = 30px
    // Child2 at y = 0 + 50 + 30 = 80
    assert_eq!(fragment.children[1].rect().origin.y, 80.0);

    // Child2 bottom 15px + child3 top 5px = max(15, 5) = 15px
    // Child3 at y = 80 + 50 + 15 = 145
    assert_eq!(fragment.children[2].rect().origin.y, 145.0);
}

// Helper functions

fn create_block_with_margins(
    width: f32,
    height: f32,
    margin_top: f32,
    margin_right: f32,
    margin_bottom: f32,
    margin_left: f32,
) -> BoxNode {
    BoxNode::new_block(
        ComputedStyle {
            width: Some(Length::px(width)),
            height: Some(Length::px(height)),
            margin_top: LengthOrAuto::Length(Length::px(margin_top)),
            margin_right: LengthOrAuto::Length(Length::px(margin_right)),
            margin_bottom: LengthOrAuto::Length(Length::px(margin_bottom)),
            margin_left: LengthOrAuto::Length(Length::px(margin_left)),
            ..default_style()
        },
        vec![],
    )
}

fn default_style() -> ComputedStyle {
    ComputedStyle {
        display: Display::Block,
        position: Position::Static,
        float: Float::None,
        overflow: Overflow::Visible,
        margin_top: LengthOrAuto::Length(Length::px(0.0)),
        margin_right: LengthOrAuto::Length(Length::px(0.0)),
        margin_bottom: LengthOrAuto::Length(Length::px(0.0)),
        margin_left: LengthOrAuto::Length(Length::px(0.0)),
        padding_top: Length::px(0.0),
        padding_right: Length::px(0.0),
        padding_bottom: Length::px(0.0),
        padding_left: Length::px(0.0),
        border_top_width: 0.0,
        border_right_width: 0.0,
        border_bottom_width: 0.0,
        border_left_width: 0.0,
        ..Default::default()
    }
}
```

## Acceptance Criteria

- [ ] Adjacent siblings' margins collapse correctly (positive margins)
- [ ] Negative margins collapse using max(positive) - max(negative) algorithm
- [ ] Parent and first child's top margins collapse when no border/padding
- [ ] Parent and last child's bottom margins collapse when no border/padding
- [ ] Empty blocks' top and bottom margins collapse together
- [ ] Border prevents parent-child margin collapsing
- [ ] Padding prevents parent-child margin collapsing
- [ ] BFC boundaries prevent margin collapsing
- [ ] Floats don't participate in margin collapsing
- [ ] Absolutely positioned elements don't participate
- [ ] Clearance prevents margin collapsing
- [ ] All tests pass: `cargo test margin_collapse`
- [ ] Visual regression tests match browser rendering
- [ ] Code follows 10-code-standards.md

## Common Pitfalls

### Pitfall 1: Forgetting BFC Boundaries

**Wrong:**
```rust
// Always collapse margins between parent and child
parent_margin.collapse_with(child_margin)
```

**Right:**
```rust
// Check if parent establishes BFC
if !establishes_bfc(&parent.style) {
    parent_margin.collapse_with(child_margin)
} else {
    // BFC boundary: no collapsing
}
```

### Pitfall 2: Incorrect Negative Margin Handling

**Wrong:**
```rust
// Just take minimum (most negative)
margin1.min(margin2) // Wrong!
```

**Right:**
```rust
// Separate positive and negative, then combine
max(positive_margins) - max(negative_margins)
```

### Pitfall 3: Not Handling Empty Blocks

**Wrong:**
```rust
// Treat empty blocks like normal blocks
position_child(y + margin_top);
y += height + margin_bottom;
```

**Right:**
```rust
if is_empty_block(&child) {
    // Empty block: collapse top and bottom together
    let self_collapsed = collapse(margin_top, margin_bottom);
    pending_margin.collapse_with(self_collapsed);
    // Don't increment y!
}
```

### Pitfall 4: Forgetting Clearance

**Wrong:**
```rust
// Always collapse margins
margin1.collapse_with(margin2)
```

**Right:**
```rust
if has_clearance {
    // Clearance prevents collapsing
    break_collapse();
}
```

## Edge Cases

### Edge Case 1: Multiple Empty Blocks

```html
<div style="margin: 20px 0"></div>
<div style="margin: 30px 0"></div>
<div style="margin: 10px 0"></div>
```

All three empty blocks collapse together to 30px (the maximum).

### Edge Case 2: Nested Parent-Child Collapsing

```html
<div style="margin-top: 10px">
  <div style="margin-top: 20px">
    <div style="margin-top: 30px">Content</div>
  </div>
</div>
```

All three top margins collapse to 30px (assuming no borders/padding).

### Edge Case 3: Clearance with Margins

```html
<div style="float: left; height: 100px">Float</div>
<div style="clear: left; margin-top: 50px">Cleared</div>
```

Clearance may be added above the cleared element, and this clearance prevents margin collapsing.

## Performance Considerations

Margin collapsing adds some complexity to layout, but the overhead is minimal:

1. **State tracking:** Small struct (~32 bytes)
2. **Margin calculations:** Simple max operations
3. **No additional passes:** Everything in one layout pass

**Optimization:** Pre-compute which boxes can collapse to avoid repeated checks.

## Next Steps

After margin collapsing:

- Test integration with float layout
- Test integration with positioned layout
- Visual regression testing against browsers
- Performance benchmarking

## References

- **CSS 2.1 Section 8.3.1: Collapsing margins**
  - https://www.w3.org/TR/CSS21/box.html#collapsing-margins
- **CSS 2.1 Section 9.4.1: Block formatting contexts**
  - https://www.w3.org/TR/CSS21/visuren.html#block-formatting
- **CSS 2.1 Section 9.5.2: Controlling flow next to floats: the 'clear' property**
  - https://www.w3.org/TR/CSS21/visuren.html#flow-control
- **MDN: Mastering margin collapsing**
  - https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Box_Model/Mastering_margin_collapsing
- **Servo's margin collapsing:**
  - `components/layout_2020/flow/block.rs`

---

**Last Updated:** 2025-01-19
**Status:** Ready for Implementation
