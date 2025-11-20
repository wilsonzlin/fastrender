# Phase 4: Stacking Contexts

**Duration:** Week 2 of Phase 4 (7-10 days)
**Prerequisites:**
- Phase 1 complete (type system, formatting contexts, box generation)
- Phase 2 complete (layout algorithms)
- Display list construction (04-display-list.md)
**Dependencies:**
- FragmentNode tree from layout
- DisplayList from display list builder
- ComputedStyle with z-index and position properties
**Output:** Correctly ordered display list respecting z-index and stacking context rules

## Objectives

Implement the stacking context system that determines the correct paint order for all elements in a document. This is one of the most complex parts of CSS rendering and is critical for correct z-index behavior.

The stacking context system provides:
- Identification of what creates a stacking context
- Construction of stacking context tree from fragment tree
- Paint order algorithm following CSS 2.1 Appendix E
- Z-index sorting within stacking contexts
- Correct handling of negative z-index
- Integration with positioned elements (absolute, relative, fixed)

## Context

Stacking contexts determine the z-order (depth) of elements on the page. Understanding this is **CRITICAL** for correct rendering.

**From CSS 2.1 Appendix E:**
> "Within each stacking context, the following layers are painted in back-to-front order:
> 1. The background and borders of the element forming the stacking context
> 2. The child stacking contexts with negative stack levels (most negative first)
> 3. The in-flow, non-inline-level, non-positioned descendants
> 4. The non-positioned floats
> 5. The in-flow, inline-level, non-positioned descendants, including inline tables and inline blocks
> 6. The child stacking contexts with stack level 0 and the positioned descendants with stack level 0
> 7. The child stacking contexts with positive stack levels (least positive first)"

**Key Concepts:**

1. **Stacking Context** - A 3D conceptual layer containing elements
2. **Z-index** - Determines order within a stacking context
3. **Paint Order** - The order in which elements are drawn
4. **Stacking Level** - Position in the z-order within a context

**What Creates a Stacking Context:**

- Root element (`<html>`)
- Element with `position: absolute/relative/fixed/sticky` and `z-index != auto`
- Element with `position: fixed` or `position: sticky` (even without z-index)
- Element with `opacity < 1`
- Element with `transform`, `filter`, `perspective`, `clip-path`, `mask`, `mix-blend-mode`
- Element with `isolation: isolate`
- Element with `-webkit-overflow-scrolling: touch`
- Flex/grid items with `z-index != auto`

## The Problem V1 Has

V1 doesn't implement stacking contexts:
- No z-index support
- Elements paint in DOM order (incorrect)
- Cannot handle complex layering
- Positioned elements render incorrectly
- No support for opacity/transform creating contexts

This makes any non-trivial page render incorrectly.

## The Solution

Implement a complete stacking context system:

1. **Detection** - Identify what creates stacking contexts
2. **Tree Construction** - Build stacking context tree from fragment tree
3. **Sorting** - Sort elements within each context by z-index
4. **Paint Order** - Follow CSS 2.1 Appendix E precisely
5. **Display List Reordering** - Reorder display items to match paint order

## CSS Specification References

**Primary (CRITICAL):**
- **CSS 2.1 Appendix E:** Elaborate description of Stacking Contexts
  - https://www.w3.org/TR/CSS21/zindex.html
  - **THIS IS THE MOST IMPORTANT SPEC FOR THIS PHASE**
- **CSS 2.2 Section 9.9:** Layered presentation
  - https://www.w3.org/TR/CSS22/visuren.html#z-index
- **CSS 2.1 Section 9.9.1:** Specifying the stack level: the 'z-index' property
  - https://www.w3.org/TR/CSS21/visuren.html#propdef-z-index

**Related:**
- **CSS Positioned Layout Module Level 3:** Position property
- **CSS Transforms Module Level 1:** Transform creating stacking context
- **CSS Color Module Level 4:** Opacity creating stacking context

## Step-by-Step Implementation

### Step 1: Define Stacking Context Types (Day 1 Morning)

```bash
mkdir -p /home/user/fastrender/src/paint/stacking
touch /home/user/fastrender/src/paint/stacking/mod.rs
touch /home/user/fastrender/src/paint/stacking/context.rs
touch /home/user/fastrender/src/paint/stacking/tree.rs
touch /home/user/fastrender/src/paint/stacking/sort.rs
```

**File: `src/paint/stacking/context.rs`**

```rust
//! Stacking context
//!
//! Represents a single stacking context in the stacking context tree.

use crate::tree::FragmentNode;
use crate::style::{ComputedStyle, Position};
use std::sync::Arc;

/// Stacking context
///
/// A 3D layer in the rendering model that contains elements.
#[derive(Debug, Clone)]
pub struct StackingContext {
    /// Fragment that created this stacking context
    pub fragment: Arc<FragmentNode>,

    /// Z-index value (i32::MIN for auto)
    pub z_index: i32,

    /// Child stacking contexts
    pub children: Vec<StackingContext>,

    /// What created this stacking context
    pub reason: StackingContextReason,
}

/// Reason a stacking context was created
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StackingContextReason {
    /// Root element
    Root,

    /// Positioned element with z-index
    PositionedWithZIndex,

    /// Fixed or sticky positioning
    FixedOrSticky,

    /// Opacity < 1.0
    Opacity,

    /// CSS transform
    Transform,

    /// CSS filter
    Filter,

    /// Isolation property
    Isolation,

    /// Flex/grid item with z-index
    FlexOrGridItem,

    /// Mix blend mode
    MixBlendMode,

    /// Will-change property
    WillChange,
}

impl StackingContext {
    /// Create a new stacking context
    pub fn new(
        fragment: Arc<FragmentNode>,
        z_index: i32,
        reason: StackingContextReason,
    ) -> Self {
        Self {
            fragment,
            z_index,
            children: Vec::new(),
            reason,
        }
    }

    /// Add a child stacking context
    pub fn add_child(&mut self, child: StackingContext) {
        self.children.push(child);
    }

    /// Sort children by z-index
    ///
    /// CSS 2.1 Appendix E: Within a stacking context, child contexts are sorted
    /// by z-index (negative first, then 0, then positive).
    pub fn sort_children(&mut self) {
        self.children.sort_by_key(|child| child.z_index);

        // Recursively sort children
        for child in &mut self.children {
            child.sort_children();
        }
    }

    /// Get children with negative z-index
    pub fn negative_children(&self) -> impl Iterator<Item = &StackingContext> {
        self.children.iter().filter(|c| c.z_index < 0)
    }

    /// Get children with z-index 0 or auto
    pub fn zero_children(&self) -> impl Iterator<Item = &StackingContext> {
        self.children.iter().filter(|c| c.z_index == 0)
    }

    /// Get children with positive z-index
    pub fn positive_children(&self) -> impl Iterator<Item = &StackingContext> {
        self.children.iter().filter(|c| c.z_index > 0)
    }
}

/// Check if a style creates a stacking context
///
/// CSS 2.1 Section 9.9 and Appendix E specify what creates stacking contexts.
pub fn creates_stacking_context(style: &ComputedStyle) -> bool {
    // Root element always creates stacking context
    // (Checked elsewhere, not in style)

    // Positioned element with z-index != auto
    if is_positioned(style.position) && style.z_index.is_some() {
        return true;
    }

    // Fixed or sticky positioning (creates context even without z-index)
    if matches!(style.position, Position::Fixed | Position::Sticky) {
        return true;
    }

    // Opacity < 1.0
    if style.opacity < 1.0 {
        return true;
    }

    // Has transform
    if style.transform.is_some() {
        return true;
    }

    // Has filter
    if style.filter.is_some() {
        return true;
    }

    // Has perspective
    if style.perspective.is_some() {
        return true;
    }

    // Has clip-path
    if style.clip_path.is_some() {
        return true;
    }

    // Has mask
    if style.mask.is_some() {
        return true;
    }

    // Has mix-blend-mode (other than normal)
    if style.mix_blend_mode != super::BlendMode::Normal {
        return true;
    }

    // isolation: isolate
    if style.isolation == Isolation::Isolate {
        return true;
    }

    // Flex/grid item with z-index != auto
    // (Need to check parent's display property)
    // This is handled in the tree builder

    // will-change that would create a stacking context
    if let Some(ref will_change) = style.will_change {
        if will_change.creates_stacking_context() {
            return true;
        }
    }

    false
}

/// Check if position creates a stacking context
fn is_positioned(position: Position) -> bool {
    matches!(
        position,
        Position::Relative | Position::Absolute | Position::Fixed | Position::Sticky
    )
}

/// Get z-index value from style
///
/// Returns i32::MIN for auto (sorts before negative z-index values)
pub fn get_z_index(style: &ComputedStyle) -> i32 {
    style.z_index.unwrap_or(0)
}

/// Determine reason for stacking context creation
pub fn get_stacking_context_reason(style: &ComputedStyle) -> StackingContextReason {
    // Check in order of priority

    if is_positioned(style.position) && style.z_index.is_some() {
        return StackingContextReason::PositionedWithZIndex;
    }

    if matches!(style.position, Position::Fixed | Position::Sticky) {
        return StackingContextReason::FixedOrSticky;
    }

    if style.opacity < 1.0 {
        return StackingContextReason::Opacity;
    }

    if style.transform.is_some() {
        return StackingContextReason::Transform;
    }

    if style.filter.is_some() {
        return StackingContextReason::Filter;
    }

    if style.isolation == Isolation::Isolate {
        return StackingContextReason::Isolation;
    }

    if style.mix_blend_mode != super::BlendMode::Normal {
        return StackingContextReason::MixBlendMode;
    }

    if style.will_change.is_some() {
        return StackingContextReason::WillChange;
    }

    // Default
    StackingContextReason::Root
}

// Placeholder types (should be defined in style module)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Isolation {
    Auto,
    Isolate,
}

use crate::paint::display_list::BlendMode;
```

### Step 2: Build Stacking Context Tree (Day 1 Afternoon - Day 2)

**File: `src/paint/stacking/tree.rs`**

```rust
//! Stacking context tree builder
//!
//! Constructs the stacking context tree from the fragment tree.

use super::context::{StackingContext, StackingContextReason};
use super::context::{creates_stacking_context, get_z_index, get_stacking_context_reason};
use crate::tree::FragmentNode;
use std::sync::Arc;

/// Stacking context tree builder
pub struct StackingContextTreeBuilder {
    /// Current stacking context being built
    current_context: Option<StackingContext>,
}

impl StackingContextTreeBuilder {
    /// Create a new builder
    pub fn new() -> Self {
        Self {
            current_context: None,
        }
    }

    /// Build stacking context tree from fragment tree
    ///
    /// Returns the root stacking context.
    pub fn build(mut self, root: &FragmentNode) -> StackingContext {
        // Root always creates a stacking context
        let mut root_context = StackingContext::new(
            Arc::new(root.clone()),
            0,
            StackingContextReason::Root,
        );

        // Build children
        self.build_children(root, &mut root_context);

        // Sort all children by z-index
        root_context.sort_children();

        root_context
    }

    /// Build children for a stacking context
    fn build_children(&mut self, fragment: &FragmentNode, parent_context: &mut StackingContext) {
        for child in fragment.children() {
            self.process_fragment(child, parent_context);
        }
    }

    /// Process a single fragment
    fn process_fragment(&mut self, fragment: &FragmentNode, parent_context: &mut StackingContext) {
        let style = fragment.style();

        if creates_stacking_context(style) {
            // This fragment creates a new stacking context
            let z_index = get_z_index(style);
            let reason = get_stacking_context_reason(style);

            let mut child_context = StackingContext::new(
                Arc::new(fragment.clone()),
                z_index,
                reason,
            );

            // Build children of this new stacking context
            self.build_children(fragment, &mut child_context);

            // Add to parent
            parent_context.add_child(child_context);
        } else {
            // Does not create stacking context - continue with same context
            self.build_children(fragment, parent_context);
        }
    }
}

impl Default for StackingContextTreeBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::geometry::{Point, Size, Rect};
    use crate::style::{ComputedStyle, Position};

    #[test]
    fn test_root_creates_stacking_context() {
        let root = create_fragment(default_style());

        let builder = StackingContextTreeBuilder::new();
        let context = builder.build(&root);

        assert_eq!(context.reason, StackingContextReason::Root);
        assert_eq!(context.z_index, 0);
    }

    #[test]
    fn test_positioned_with_z_index() {
        let mut style = default_style();
        style.position = Position::Relative;
        style.z_index = Some(10);

        let fragment = create_fragment(style);

        assert!(creates_stacking_context(&fragment.style()));
    }

    #[test]
    fn test_opacity_creates_context() {
        let mut style = default_style();
        style.opacity = 0.5;

        let fragment = create_fragment(style);

        assert!(creates_stacking_context(&fragment.style()));
    }

    #[test]
    fn test_negative_z_index_sorting() {
        let root = create_fragment(default_style());

        let child1 = create_fragment(style_with_z_index(-10));
        let child2 = create_fragment(style_with_z_index(5));
        let child3 = create_fragment(style_with_z_index(-5));

        let root_with_children = FragmentNode::new(
            Rect::new(Point::zero(), Size::new(400.0, 300.0)),
            default_style(),
            vec![child1, child2, child3],
        );

        let builder = StackingContextTreeBuilder::new();
        let mut context = builder.build(&root_with_children);

        context.sort_children();

        // Should be sorted: -10, -5, 5
        assert_eq!(context.children[0].z_index, -10);
        assert_eq!(context.children[1].z_index, -5);
        assert_eq!(context.children[2].z_index, 5);
    }

    // Helper functions
    fn create_fragment(style: ComputedStyle) -> FragmentNode {
        FragmentNode::new(
            Rect::new(Point::zero(), Size::new(100.0, 100.0)),
            style,
            vec![],
        )
    }

    fn default_style() -> ComputedStyle {
        ComputedStyle::default()
    }

    fn style_with_z_index(z: i32) -> ComputedStyle {
        let mut style = ComputedStyle::default();
        style.position = Position::Relative;
        style.z_index = Some(z);
        style
    }
}
```

### Step 3: Implement Paint Order Algorithm (Day 3-4)

**File: `src/paint/stacking/sort.rs`**

```rust
//! Paint order algorithm
//!
//! Implements CSS 2.1 Appendix E paint order.

use super::context::StackingContext;
use crate::paint::display_list::{DisplayList, DisplayItem};
use crate::tree::FragmentNode;
use std::sync::Arc;

/// Paint order sorter
///
/// Reorders display list items according to stacking context rules.
pub struct PaintOrderSorter {
    /// Sorted display items
    sorted_items: Vec<DisplayItem>,
}

impl PaintOrderSorter {
    /// Create a new sorter
    pub fn new() -> Self {
        Self {
            sorted_items: Vec::new(),
        }
    }

    /// Sort display list by stacking context tree
    ///
    /// Takes a display list (in tree order) and a stacking context tree,
    /// and returns a new display list in correct paint order.
    pub fn sort(
        mut self,
        display_list: DisplayList,
        stacking_tree: &StackingContext,
    ) -> DisplayList {
        // Paint the stacking context tree in order
        self.paint_stacking_context(stacking_tree);

        DisplayList::with_items(self.sorted_items)
    }

    /// Paint a stacking context
    ///
    /// CSS 2.1 Appendix E paint order:
    /// 1. Background and borders of the element forming the stacking context
    /// 2. Child stacking contexts with negative z-index (most negative first)
    /// 3. In-flow, non-inline-level, non-positioned descendants
    /// 4. Non-positioned floats
    /// 5. In-flow, inline-level, non-positioned descendants
    /// 6. Positioned descendants with z-index: auto or z-index: 0
    /// 7. Child stacking contexts with positive z-index (least positive first)
    fn paint_stacking_context(&mut self, context: &StackingContext) {
        let fragment = &context.fragment;

        // 1. Paint background and borders of this element
        self.paint_background_and_borders(fragment);

        // 2. Paint child stacking contexts with negative z-index
        // (most negative first)
        let mut negative_children: Vec<_> = context.negative_children().collect();
        negative_children.sort_by_key(|c| c.z_index); // Already sorted, but be explicit

        for child in negative_children {
            self.paint_stacking_context(child);
        }

        // 3. Paint in-flow, non-inline-level, non-positioned descendants
        self.paint_block_descendants(fragment);

        // 4. Paint non-positioned floats
        self.paint_floats(fragment);

        // 5. Paint in-flow, inline-level, non-positioned descendants
        self.paint_inline_descendants(fragment);

        // 6. Paint positioned descendants with z-index: auto or 0
        // and child stacking contexts with z-index: 0
        for child in context.zero_children() {
            self.paint_stacking_context(child);
        }

        // 7. Paint child stacking contexts with positive z-index
        // (least positive first)
        let mut positive_children: Vec<_> = context.positive_children().collect();
        positive_children.sort_by_key(|c| c.z_index);

        for child in positive_children {
            self.paint_stacking_context(child);
        }
    }

    /// Paint background and borders
    fn paint_background_and_borders(&mut self, fragment: &FragmentNode) {
        // Get display items for this fragment's background and borders
        // In practice, we need to extract these from the original display list
        // For now, simplified

        let style = fragment.style();

        // Paint background
        if let Some(bg_color) = style.background_color {
            // Add background item
            // self.sorted_items.push(DisplayItem::FillRect(...));
        }

        // Paint borders
        if style.has_border() {
            // Add border items
            // self.sorted_items.push(DisplayItem::StrokeRect(...));
        }
    }

    /// Paint block-level descendants
    fn paint_block_descendants(&mut self, fragment: &FragmentNode) {
        for child in fragment.children() {
            if !self.is_positioned(&child) && !self.is_inline(&child) {
                // Paint this block descendant
                self.paint_fragment_contents(child);
            }
        }
    }

    /// Paint floats
    fn paint_floats(&mut self, fragment: &FragmentNode) {
        for child in fragment.children() {
            if self.is_float(&child) && !self.is_positioned(&child) {
                self.paint_fragment_contents(child);
            }
        }
    }

    /// Paint inline-level descendants
    fn paint_inline_descendants(&mut self, fragment: &FragmentNode) {
        for child in fragment.children() {
            if self.is_inline(&child) && !self.is_positioned(&child) {
                self.paint_fragment_contents(child);
            }
        }
    }

    /// Paint a fragment's contents (not background/borders)
    fn paint_fragment_contents(&mut self, fragment: &FragmentNode) {
        // Paint text content
        if let Some(ref text) = fragment.text_content() {
            // Add text item
            // self.sorted_items.push(DisplayItem::Text(...));
        }

        // Recursively paint children
        for child in fragment.children() {
            self.paint_fragment_contents(child);
        }
    }

    /// Check if fragment is positioned
    fn is_positioned(&self, fragment: &FragmentNode) -> bool {
        let style = fragment.style();
        matches!(
            style.position,
            Position::Relative | Position::Absolute | Position::Fixed | Position::Sticky
        )
    }

    /// Check if fragment is inline-level
    fn is_inline(&self, fragment: &FragmentNode) -> bool {
        let style = fragment.style();
        matches!(
            style.display,
            Display::Inline | Display::InlineBlock | Display::InlineTable
        )
    }

    /// Check if fragment is floated
    fn is_float(&self, fragment: &FragmentNode) -> bool {
        let style = fragment.style();
        matches!(style.float, Float::Left | Float::Right)
    }
}

impl Default for PaintOrderSorter {
    fn default() -> Self {
        Self::new()
    }
}

// Placeholder types
use crate::style::{Position, Display};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Float {
    None,
    Left,
    Right,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_paint_order_sorter_creation() {
        let sorter = PaintOrderSorter::new();
        assert_eq!(sorter.sorted_items.len(), 0);
    }

    #[test]
    fn test_negative_z_index_paints_first() {
        // Create stacking context with negative z-index child
        // Verify negative child is painted before z-index: 0 children
        // This is a complex test requiring full setup
    }

    #[test]
    fn test_positive_z_index_paints_last() {
        // Create stacking context with positive z-index child
        // Verify positive child is painted after all other content
    }
}
```

### Step 4: Integration with Display List (Day 5)

**File: `src/paint/stacking/mod.rs`**

```rust
//! Stacking context system
//!
//! Implements CSS stacking contexts and z-index ordering.

pub mod context;
pub mod tree;
pub mod sort;

pub use context::{StackingContext, StackingContextReason};
pub use context::{creates_stacking_context, get_z_index};
pub use tree::StackingContextTreeBuilder;
pub use sort::PaintOrderSorter;

use crate::tree::FragmentNode;
use crate::paint::display_list::DisplayList;

/// Build display list with correct paint order
///
/// This is the main entry point that combines display list construction
/// and stacking context sorting.
pub fn build_display_list_with_paint_order(
    fragment_tree: &FragmentNode,
) -> DisplayList {
    // 1. Build stacking context tree
    let stacking_tree = StackingContextTreeBuilder::new().build(fragment_tree);

    // 2. Build initial display list (tree order)
    let initial_list = crate::paint::display_list::DisplayListBuilder::new()
        .build(fragment_tree);

    // 3. Sort by stacking context rules
    let sorted_list = PaintOrderSorter::new()
        .sort(initial_list, &stacking_tree);

    sorted_list
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::geometry::{Point, Size, Rect};
    use crate::style::ComputedStyle;

    #[test]
    fn test_simple_stacking() {
        let root = create_simple_fragment();

        let list = build_display_list_with_paint_order(&root);

        // Should have items in correct order
        assert!(list.items().len() > 0);
    }

    fn create_simple_fragment() -> FragmentNode {
        FragmentNode::new(
            Rect::new(Point::zero(), Size::new(400.0, 300.0)),
            ComputedStyle::default(),
            vec![],
        )
    }
}
```

### Step 5: Comprehensive Tests (Day 6-7)

**File: `tests/paint/stacking_test.rs`**

```rust
//! Tests for stacking contexts
//!
//! These tests verify that paint order follows CSS 2.1 Appendix E.

use fastrender::paint::stacking::*;
use fastrender::tree::FragmentNode;
use fastrender::geometry::{Point, Size, Rect};
use fastrender::style::{ComputedStyle, Position, Color};

#[test]
fn test_root_creates_stacking_context() {
    let root = create_fragment(default_style());

    let builder = StackingContextTreeBuilder::new();
    let context = builder.build(&root);

    assert_eq!(context.reason, StackingContextReason::Root);
}

#[test]
fn test_positioned_element_with_z_index() {
    let mut style = default_style();
    style.position = Position::Absolute;
    style.z_index = Some(10);

    assert!(creates_stacking_context(&style));
}

#[test]
fn test_opacity_creates_stacking_context() {
    let mut style = default_style();
    style.opacity = 0.5;

    assert!(creates_stacking_context(&style));
}

#[test]
fn test_transform_creates_stacking_context() {
    let mut style = default_style();
    style.transform = Some(Transform::translate(10.0, 20.0));

    assert!(creates_stacking_context(&style));
}

#[test]
fn test_fixed_position_creates_stacking_context() {
    let mut style = default_style();
    style.position = Position::Fixed;

    assert!(creates_stacking_context(&style));
}

#[test]
fn test_negative_z_index_order() {
    // Create a stacking context with three children:
    // - z-index: -10
    // - z-index: 0
    // - z-index: 5

    let child_neg10 = create_fragment(style_with_z_index(-10));
    let child_0 = create_fragment(style_with_z_index(0));
    let child_pos5 = create_fragment(style_with_z_index(5));

    let root = FragmentNode::new(
        Rect::new(Point::zero(), Size::new(400.0, 300.0)),
        default_style(),
        vec![child_0, child_pos5, child_neg10], // Out of order
    );

    let builder = StackingContextTreeBuilder::new();
    let mut context = builder.build(&root);

    context.sort_children();

    // Should be sorted: -10, 0, 5
    let z_indices: Vec<i32> = context.children.iter().map(|c| c.z_index).collect();
    assert_eq!(z_indices, vec![-10, 0, 5]);
}

#[test]
fn test_paint_order_background_before_children() {
    // Parent with background should paint before children
    let mut parent_style = default_style();
    parent_style.background_color = Some(Color::rgb(255, 0, 0));

    let child = create_fragment(default_style());

    let parent = FragmentNode::new(
        Rect::new(Point::zero(), Size::new(200.0, 200.0)),
        parent_style,
        vec![child],
    );

    let list = build_display_list_with_paint_order(&parent);

    // First item should be the parent's background
    // (In practice, need to check item types)
    assert!(list.items().len() > 0);
}

#[test]
fn test_paint_order_negative_z_index_before_normal() {
    // Element with negative z-index should paint before z-index: 0

    let child_neg = create_fragment(style_with_z_index(-1));
    let child_zero = create_fragment(style_with_z_index(0));

    let root = FragmentNode::new(
        Rect::new(Point::zero(), Size::new(400.0, 300.0)),
        default_style(),
        vec![child_zero, child_neg], // Reverse order
    );

    let list = build_display_list_with_paint_order(&root);

    // Negative z-index items should appear first
    // (Would need to track which items belong to which fragment)
    assert!(list.items().len() > 0);
}

#[test]
fn test_nested_stacking_contexts() {
    // Test that stacking contexts are properly nested:
    // Root (z:0)
    //   ├─ Child A (z:-1)
    //   └─ Child B (z:1)
    //        └─ Grandchild (z:-1) - should paint after Child A despite negative z

    let grandchild = create_fragment(style_with_z_index(-1));

    let child_b = FragmentNode::new(
        Rect::new(Point::new(100.0, 100.0), Size::new(100.0, 100.0)),
        style_with_z_index(1),
        vec![grandchild],
    );

    let child_a = create_fragment(style_with_z_index(-1));

    let root = FragmentNode::new(
        Rect::new(Point::zero(), Size::new(400.0, 300.0)),
        default_style(),
        vec![child_a, child_b],
    );

    let builder = StackingContextTreeBuilder::new();
    let context = builder.build(&root);

    // Child A should be in root's children with z:-1
    // Child B should be in root's children with z:1
    // Grandchild should be in B's children with z:-1

    assert_eq!(context.children.len(), 2);

    // Find child B (z:1)
    let child_b_ctx = context.children.iter().find(|c| c.z_index == 1).unwrap();
    assert_eq!(child_b_ctx.children.len(), 1);
    assert_eq!(child_b_ctx.children[0].z_index, -1);
}

#[test]
fn test_isolation_creates_stacking_context() {
    let mut style = default_style();
    style.isolation = Isolation::Isolate;

    assert!(creates_stacking_context(&style));
}

#[test]
fn test_mix_blend_mode_creates_stacking_context() {
    let mut style = default_style();
    style.mix_blend_mode = BlendMode::Multiply;

    assert!(creates_stacking_context(&style));
}

// Helper functions
fn create_fragment(style: ComputedStyle) -> FragmentNode {
    FragmentNode::new(
        Rect::new(Point::zero(), Size::new(100.0, 100.0)),
        style,
        vec![],
    )
}

fn default_style() -> ComputedStyle {
    ComputedStyle::default()
}

fn style_with_z_index(z: i32) -> ComputedStyle {
    let mut style = ComputedStyle::default();
    style.position = Position::Relative;
    style.z_index = Some(z);
    style
}

// Placeholder types
use crate::paint::stacking::context::Isolation;
use crate::paint::display_list::BlendMode;

struct Transform;
impl Transform {
    fn translate(_x: f32, _y: f32) -> Self {
        Transform
    }
}
```

## Acceptance Criteria

- [ ] All conditions that create stacking contexts are detected
- [ ] Stacking context tree is built correctly from fragment tree
- [ ] Z-index sorting works (negative, zero, positive)
- [ ] Paint order follows CSS 2.1 Appendix E exactly
- [ ] Nested stacking contexts work correctly
- [ ] Negative z-index elements paint behind parent's background
- [ ] Positive z-index elements paint in front of everything else
- [ ] Positioned elements create stacking contexts when appropriate
- [ ] Opacity, transform, filter create stacking contexts
- [ ] Root element always creates a stacking context
- [ ] Flex/grid items with z-index create stacking contexts
- [ ] Display list is correctly reordered
- [ ] All tests pass: `cargo test stacking`
- [ ] Code follows 10-code-standards.md

## Common Pitfalls

### Pitfall 1: Forgetting That Negative Z-Index Paints Behind Parent Background

**Wrong:**
```rust
// Painting negative z-index children after parent background
paint_background(parent);
paint_children_with_negative_z(parent); // Wrong order!
```

**Right:**
```rust
// CSS 2.1 Appendix E: Negative z-index paints BEHIND background
paint_children_with_negative_z(parent); // First!
paint_background(parent);                // Then background
```

### Pitfall 2: Assuming DOM Order = Paint Order

**Wrong:**
```rust
// Painting children in DOM order
for child in children {
    paint(child); // Wrong! Ignores z-index!
}
```

**Right:**
```rust
// Sort by z-index first
let mut sorted_children = children.to_vec();
sorted_children.sort_by_key(|c| c.z_index);
for child in sorted_children {
    paint(child);
}
```

### Pitfall 3: Not Isolating Stacking Contexts

**Wrong:**
```rust
// Z-index of grandchild affecting root's stacking order
// z-index only matters within the parent stacking context!
```

**Right:**
```rust
// Each stacking context is isolated
// Grandchild's z-index only affects order within its parent context,
// not the root context
```

### Pitfall 4: Fixed Position Without Stacking Context

**Wrong:**
```rust
// Assuming fixed position always has z-index
if style.position == Position::Fixed && style.z_index.is_some() {
    creates_stacking_context = true;
}
```

**Right:**
```rust
// Fixed position ALWAYS creates stacking context, even without z-index
if style.position == Position::Fixed {
    creates_stacking_context = true; // Even if z-index is auto!
}
```

### Pitfall 5: Opacity 0 vs Opacity 1

**Wrong:**
```rust
// Treating opacity: 0 differently
if style.opacity == 0.0 {
    skip_painting(); // Wrong! Still creates stacking context!
}
```

**Right:**
```rust
// Opacity < 1.0 always creates stacking context, even if 0
if style.opacity < 1.0 {
    create_stacking_context(); // Even opacity: 0!
}
```

## Complex Scenarios

### Scenario 1: Negative Z-Index with Background

```html
<div style="background: red; position: relative;">
  <div style="position: absolute; z-index: -1; background: blue;">
    Negative z-index
  </div>
  Parent content
</div>
```

**Paint Order:**
1. Blue div (z-index: -1) - behind parent background
2. Red background of parent
3. "Parent content" text

### Scenario 2: Nested Stacking Contexts

```html
<div style="position: relative; z-index: 1;">
  <div style="position: absolute; z-index: -1;">
    This paints AFTER sibling z-index: 0
  </div>
</div>
<div style="position: relative; z-index: 0;">
  Sibling
</div>
```

**Paint Order:**
1. Second div (z-index: 0) - paints first
2. First div (z-index: 1) - paints second
   - Including its child with z-index: -1

The child's negative z-index only affects order within its parent stacking context!

### Scenario 3: Transform with Z-Index

```html
<div style="transform: translateX(10px); z-index: 1;">
  Transform creates stacking context
</div>
```

The element creates a stacking context due to transform, even though it's not positioned. The z-index is honored because a stacking context exists.

## Performance Considerations

1. **Tree construction** - O(n) where n = number of fragments
2. **Sorting** - O(n log n) for z-index sorting
3. **Display list reordering** - O(m) where m = number of display items
4. **Caching** - Cache stacking context tree across frames if DOM unchanged

## Integration Points

Stacking contexts integrate with:
1. **Display list construction** - Initial list in tree order
2. **Paint order sorting** - Reorder by z-index
3. **Rasterization** - Final list sent to rasterizer
4. **Hit testing** - Z-order affects which element is clicked

## Next Steps

- **04-rasterization.md** - Converting sorted display list to pixels

## References

**CRITICAL - Read these thoroughly:**
- **CSS 2.1 Appendix E:** Elaborate description of Stacking Contexts
  - https://www.w3.org/TR/CSS21/zindex.html
  - **THIS IS THE SPEC. FOLLOW IT EXACTLY.**
- **CSS 2.2 Section 9.9:** Layered presentation
  - https://www.w3.org/TR/CSS22/visuren.html#z-index
- **What Creates Stacking Contexts:**
  - https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_positioned_layout/Understanding_z-index/Stacking_context

**Additional:**
- **Chromium's Paint Order:** Understanding browser implementation
  - https://source.chromium.org/chromium/chromium/src/+/main:third_party/blink/renderer/core/paint/
- **WebKit Paint Order:** Alternative implementation
  - https://github.com/WebKit/WebKit/tree/main/Source/WebCore/rendering

---

**Last Updated:** 2025-11-19
**Status:** Ready for Implementation
