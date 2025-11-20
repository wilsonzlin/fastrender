# Phase 2: Flex Layout (Taffy Wrapper)

**Duration:** Week 4 of Phase 2 (3-5 days)
**Prerequisites:**
- Phase 1 complete (type system, formatting contexts, box generation)
- Block layout complete
- Inline layout complete
**Dependencies:**
- FormattingContext trait
- BoxNode, FragmentNode types
- Taffy library
**Output:** Flexbox layout via Taffy integration

## Objectives

Implement Flexbox layout by wrapping Taffy's flex algorithm. This is **much simpler** than implementing from scratch because Taffy already has a spec-compliant flexbox implementation.

**Key Insight:** Unlike tables and text, flexbox is algorithmic and well-suited to being delegated to a library. We just need to:
1. Convert our BoxNode tree to Taffy nodes
2. Let Taffy compute the layout
3. Convert Taffy's output back to FragmentNodes

## Context

Flexbox is a one-dimensional layout system for distributing space along a single axis (row or column).

**From CSS Flexbox Module Level 1:**
> "The flex layout model is a box model optimized for user interface design. In the flex layout model, the children of a flex container can be laid out in any direction, and can 'flex' their sizes, either growing to fill unused space or shrinking to avoid overflowing the parent."

**Examples of flex usage:**
- `display: flex` - Creates a flex container
- `flex-direction: row | column` - Main axis direction
- `justify-content` - Alignment along main axis
- `align-items` - Alignment along cross axis
- `flex-grow`, `flex-shrink`, `flex-basis` - Item flexibility

## Why Use Taffy?

Taffy is a battle-tested layout library that implements:
- **CSS Flexbox** (complete spec compliance)
- **CSS Grid** (complete spec compliance)
- Used by Dioxus, Bevy UI, and other Rust UI frameworks

**Benefits:**
- Saves months of implementation time
- Already handles edge cases
- Well-tested against WPT (Web Platform Tests)
- Active maintenance

**Trade-offs:**
- External dependency
- Need to convert between our types and Taffy's
- Some overhead from type conversion

## The Problem V1 Has

V1 **already uses Taffy** but incorrectly:
- Uses Taffy for block layout (doesn't match CSS block layout rules)
- Uses Taffy for table layout (completely wrong)
- Doesn't properly integrate with text layout

## The Solution

Use Taffy **only for what it's good at**: Flexbox and Grid.

## Step-by-Step Implementation

### Step 1: Create Flex FC Module (Day 1 Morning)

```bash
mkdir -p /home/user/fastrender/src/layout/contexts
touch /home/user/fastrender/src/layout/contexts/flex.rs
```

**File: `src/layout/contexts/flex.rs`**

```rust
//! Flexbox Formatting Context (via Taffy)
//!
//! CSS Specification: CSS Flexible Box Layout Module Level 1
//! https://www.w3.org/TR/css-flexbox-1/
//!
//! This is a thin wrapper around Taffy's flexbox implementation.

use super::{FormattingContext, FormattingContextType, Axis};
use crate::tree::{BoxNode, FragmentNode, BoxType};
use crate::layout::LayoutConstraints;
use crate::text::FontContext;
use crate::geometry::{Point, Size, Rect};
use crate::error::{Result, Error};
use taffy::prelude::*;
use std::collections::HashMap;

/// Flexbox Formatting Context
///
/// Delegates layout to Taffy's flexbox algorithm.
#[derive(Debug)]
pub struct FlexFormattingContext {
    /// Taffy tree instance
    /// We create a fresh tree for each layout to avoid state issues
    _phantom: std::marker::PhantomData<()>,
}

impl FlexFormattingContext {
    pub fn new() -> Self {
        Self {
            _phantom: std::marker::PhantomData,
        }
    }
}

impl FormattingContext for FlexFormattingContext {
    fn context_type(&self) -> FormattingContextType {
        FormattingContextType::Flex
    }

    fn layout(
        &self,
        box_node: &BoxNode,
        constraints: &LayoutConstraints,
        font_context: &FontContext,
    ) -> Result<FragmentNode> {
        // Create a fresh Taffy tree for this layout
        let mut taffy_tree = TaffyTree::new();

        // Phase 1: Convert BoxNode tree to Taffy tree
        let (root_node, box_to_taffy) = self.build_taffy_tree(
            &mut taffy_tree,
            box_node,
            font_context,
        )?;

        // Phase 2: Compute layout using Taffy
        let available_space = self.constraints_to_available_space(constraints);
        taffy_tree.compute_layout(root_node, available_space)
            .map_err(|e| Error::Layout(format!("Taffy layout failed: {:?}", e)))?;

        // Phase 3: Convert Taffy layout back to FragmentNode
        let fragment = self.taffy_to_fragment(
            &taffy_tree,
            root_node,
            box_node,
            &box_to_taffy,
        )?;

        Ok(fragment)
    }

    fn compute_intrinsic_size(
        &self,
        box_node: &BoxNode,
        axis: Axis,
        font_context: &FontContext,
    ) -> Result<f32> {
        // Use Taffy to compute intrinsic size
        let mut taffy_tree = TaffyTree::new();
        let (root_node, _) = self.build_taffy_tree(&mut taffy_tree, box_node, font_context)?;

        let available_space = match axis {
            Axis::Horizontal => Size {
                width: AvailableSpace::MaxContent,
                height: AvailableSpace::Definite(0.0),
            },
            Axis::Vertical => Size {
                width: AvailableSpace::Definite(0.0),
                height: AvailableSpace::MaxContent,
            },
        };

        taffy_tree.compute_layout(root_node, available_space)
            .map_err(|e| Error::Layout(format!("Taffy intrinsic size failed: {:?}", e)))?;

        let layout = taffy_tree.layout(root_node)
            .map_err(|e| Error::Layout(format!("Failed to get layout: {:?}", e)))?;

        match axis {
            Axis::Horizontal => Ok(layout.size.width),
            Axis::Vertical => Ok(layout.size.height),
        }
    }
}

impl FlexFormattingContext {
    /// Builds a Taffy tree from BoxNode tree
    ///
    /// Returns:
    /// - The root Taffy node
    /// - A map from BoxNode IDs to Taffy nodes (for later conversion back)
    fn build_taffy_tree(
        &self,
        taffy_tree: &mut TaffyTree,
        box_node: &BoxNode,
        font_context: &FontContext,
    ) -> Result<(NodeId, HashMap<BoxNodeId, NodeId>)> {
        let mut box_to_taffy = HashMap::new();
        let root = self.create_taffy_node(taffy_tree, box_node, font_context, &mut box_to_taffy)?;
        Ok((root, box_to_taffy))
    }

    /// Creates a Taffy node for a BoxNode and its children
    fn create_taffy_node(
        &self,
        taffy_tree: &mut TaffyTree,
        box_node: &BoxNode,
        font_context: &FontContext,
        box_to_taffy: &mut HashMap<BoxNodeId, NodeId>,
    ) -> Result<NodeId> {
        // Convert style to Taffy style
        let taffy_style = self.box_style_to_taffy(&box_node.style);

        // Create Taffy node
        let taffy_node = if box_node.children.is_empty() {
            // Leaf node
            taffy_tree.new_leaf(taffy_style)
                .map_err(|e| Error::Layout(format!("Failed to create Taffy leaf: {:?}", e)))?
        } else {
            // Create children first
            let mut taffy_children = Vec::new();
            for child in &box_node.children {
                let child_node = self.create_taffy_node(
                    taffy_tree,
                    child,
                    font_context,
                    box_to_taffy,
                )?;
                taffy_children.push(child_node);
            }

            // Create parent with children
            taffy_tree.new_with_children(taffy_style, &taffy_children)
                .map_err(|e| Error::Layout(format!("Failed to create Taffy node: {:?}", e)))?
        };

        // Record mapping
        box_to_taffy.insert(box_node.id(), taffy_node);

        Ok(taffy_node)
    }

    /// Converts our ComputedStyle to Taffy's Style
    fn box_style_to_taffy(&self, style: &ComputedStyle) -> Style {
        Style {
            // Display and positioning
            display: self.display_to_taffy(style.display),
            position: self.position_to_taffy(style.position),

            // Flex container properties
            flex_direction: self.flex_direction_to_taffy(style.flex_direction),
            flex_wrap: self.flex_wrap_to_taffy(style.flex_wrap),
            justify_content: self.justify_content_to_taffy(style.justify_content),
            align_items: self.align_items_to_taffy(style.align_items),
            align_content: self.align_content_to_taffy(style.align_content),
            gap: Size {
                width: self.length_to_taffy(style.column_gap),
                height: self.length_to_taffy(style.row_gap),
            },

            // Flex item properties
            flex_grow: style.flex_grow,
            flex_shrink: style.flex_shrink,
            flex_basis: self.flex_basis_to_taffy(style.flex_basis),
            align_self: self.align_self_to_taffy(style.align_self),

            // Sizing
            size: Size {
                width: self.dimension_to_taffy(style.width),
                height: self.dimension_to_taffy(style.height),
            },
            min_size: Size {
                width: self.dimension_to_taffy(style.min_width),
                height: self.dimension_to_taffy(style.min_height),
            },
            max_size: Size {
                width: self.dimension_to_taffy(style.max_width),
                height: self.dimension_to_taffy(style.max_height),
            },

            // Spacing
            padding: Rect {
                left: self.length_to_taffy(style.padding_left),
                right: self.length_to_taffy(style.padding_right),
                top: self.length_to_taffy(style.padding_top),
                bottom: self.length_to_taffy(style.padding_bottom),
            },
            margin: Rect {
                left: self.length_or_auto_to_taffy(style.margin_left),
                right: self.length_or_auto_to_taffy(style.margin_right),
                top: self.length_or_auto_to_taffy(style.margin_top),
                bottom: self.length_or_auto_to_taffy(style.margin_bottom),
            },
            border: Rect {
                left: self.length_to_taffy(style.border_left_width),
                right: self.length_to_taffy(style.border_right_width),
                top: self.length_to_taffy(style.border_top_width),
                bottom: self.length_to_taffy(style.border_bottom_width),
            },

            ..Default::default()
        }
    }

    /// Converts Taffy layout back to FragmentNode tree
    fn taffy_to_fragment(
        &self,
        taffy_tree: &TaffyTree,
        taffy_node: NodeId,
        box_node: &BoxNode,
        box_to_taffy: &HashMap<BoxNodeId, NodeId>,
    ) -> Result<FragmentNode> {
        // Get layout from Taffy
        let layout = taffy_tree.layout(taffy_node)
            .map_err(|e| Error::Layout(format!("Failed to get Taffy layout: {:?}", e)))?;

        // Create fragment rect
        let rect = Rect::new(
            Point::new(layout.location.x, layout.location.y),
            Size::new(layout.size.width, layout.size.height),
        );

        // Convert children
        let mut children = Vec::new();
        for child_box in &box_node.children {
            if let Some(&child_taffy) = box_to_taffy.get(&child_box.id()) {
                let child_fragment = self.taffy_to_fragment(
                    taffy_tree,
                    child_taffy,
                    child_box,
                    box_to_taffy,
                )?;
                children.push(child_fragment);
            }
        }

        Ok(FragmentNode::new(
            rect,
            box_node.style.clone(),
            children,
        ))
    }

    /// Converts layout constraints to Taffy available space
    fn constraints_to_available_space(&self, constraints: &LayoutConstraints) -> Size<AvailableSpace> {
        Size {
            width: match constraints.available_width {
                Some(w) => AvailableSpace::Definite(w),
                None => AvailableSpace::MaxContent,
            },
            height: match constraints.available_height {
                Some(h) => AvailableSpace::Definite(h),
                None => AvailableSpace::MaxContent,
            },
        }
    }

    // Type conversion helpers

    fn display_to_taffy(&self, display: Display) -> taffy::Display {
        match display {
            Display::Flex => taffy::Display::Flex,
            Display::None => taffy::Display::None,
            // Block, inline, etc. should not use FlexFC
            _ => taffy::Display::Flex,
        }
    }

    fn flex_direction_to_taffy(&self, dir: FlexDirection) -> taffy::FlexDirection {
        match dir {
            FlexDirection::Row => taffy::FlexDirection::Row,
            FlexDirection::RowReverse => taffy::FlexDirection::RowReverse,
            FlexDirection::Column => taffy::FlexDirection::Column,
            FlexDirection::ColumnReverse => taffy::FlexDirection::ColumnReverse,
        }
    }

    fn flex_wrap_to_taffy(&self, wrap: FlexWrap) -> taffy::FlexWrap {
        match wrap {
            FlexWrap::NoWrap => taffy::FlexWrap::NoWrap,
            FlexWrap::Wrap => taffy::FlexWrap::Wrap,
            FlexWrap::WrapReverse => taffy::FlexWrap::WrapReverse,
        }
    }

    fn justify_content_to_taffy(&self, justify: JustifyContent) -> Option<taffy::JustifyContent> {
        Some(match justify {
            JustifyContent::FlexStart => taffy::JustifyContent::FlexStart,
            JustifyContent::FlexEnd => taffy::JustifyContent::FlexEnd,
            JustifyContent::Center => taffy::JustifyContent::Center,
            JustifyContent::SpaceBetween => taffy::JustifyContent::SpaceBetween,
            JustifyContent::SpaceAround => taffy::JustifyContent::SpaceAround,
            JustifyContent::SpaceEvenly => taffy::JustifyContent::SpaceEvenly,
        })
    }

    fn align_items_to_taffy(&self, align: AlignItems) -> Option<taffy::AlignItems> {
        Some(match align {
            AlignItems::FlexStart => taffy::AlignItems::FlexStart,
            AlignItems::FlexEnd => taffy::AlignItems::FlexEnd,
            AlignItems::Center => taffy::AlignItems::Center,
            AlignItems::Baseline => taffy::AlignItems::Baseline,
            AlignItems::Stretch => taffy::AlignItems::Stretch,
        })
    }

    fn dimension_to_taffy(&self, dim: Option<Length>) -> Dimension {
        match dim {
            Some(length) => match length.unit {
                LengthUnit::Px => Dimension::Length(length.value),
                LengthUnit::Percent => Dimension::Percent(length.value / 100.0),
                // TODO: Other units
                _ => Dimension::Auto,
            },
            None => Dimension::Auto,
        }
    }

    fn length_to_taffy(&self, length: Length) -> LengthPercentage {
        match length.unit {
            LengthUnit::Px => LengthPercentage::Length(length.value),
            LengthUnit::Percent => LengthPercentage::Percent(length.value / 100.0),
            _ => LengthPercentage::Length(0.0),
        }
    }

    fn length_or_auto_to_taffy(&self, length: LengthOrAuto) -> LengthPercentageAuto {
        match length {
            LengthOrAuto::Auto => LengthPercentageAuto::Auto,
            LengthOrAuto::Length(len) => match len.unit {
                LengthUnit::Px => LengthPercentageAuto::Length(len.value),
                LengthUnit::Percent => LengthPercentageAuto::Percent(len.value / 100.0),
                _ => LengthPercentageAuto::Auto,
            },
        }
    }

    // Add other conversion helpers...
}

impl Default for FlexFormattingContext {
    fn default() -> Self {
        Self::new()
    }
}
```

### Step 2: Handle Flex Item Intrinsic Sizing (Day 1 Afternoon)

One tricky part: flex items with `width: auto` need to compute their intrinsic size, which may involve different layout modes.

**File: `src/layout/contexts/flex.rs` (addition)**

```rust
impl FlexFormattingContext {
    /// Computes intrinsic size for a flex item
    ///
    /// This is called by Taffy when it needs to know how big a flex item
    /// wants to be (for flex-basis: auto, etc.)
    fn compute_flex_item_size(
        &self,
        box_node: &BoxNode,
        axis: Axis,
        font_context: &FontContext,
    ) -> Result<f32> {
        // Get the appropriate FC for this box
        let fc = FormattingContextFactory::get_for_box(box_node);

        // Compute intrinsic size using that FC
        fc.compute_intrinsic_size(box_node, axis, font_context)
    }

    /// Custom measurement function for Taffy
    ///
    /// This is called when Taffy needs to measure a node.
    /// We use it to integrate with our other layout algorithms.
    fn measure_function(
        &self,
        known_dimensions: Size<Option<f32>>,
        available_space: Size<AvailableSpace>,
        box_node: &BoxNode,
        font_context: &FontContext,
    ) -> Size<f32> {
        // If size is fully known, return it
        if let (Some(width), Some(height)) = (known_dimensions.width, known_dimensions.height) {
            return Size { width, height };
        }

        // Otherwise, compute intrinsic size
        let width = known_dimensions.width.unwrap_or_else(|| {
            self.compute_flex_item_size(box_node, Axis::Horizontal, font_context)
                .unwrap_or(0.0)
        });

        let height = known_dimensions.height.unwrap_or_else(|| {
            self.compute_flex_item_size(box_node, Axis::Vertical, font_context)
                .unwrap_or(0.0)
        });

        Size { width, height }
    }
}
```

### Step 3: Integration Tests (Day 2)

**File: `tests/layout/flex_layout_test.rs`**

```rust
//! Tests for Flexbox layout via Taffy

use fastrender::layout::contexts::FlexFormattingContext;
use fastrender::tree::BoxNode;
use fastrender::style::*;

#[test]
fn test_flex_row_basic() {
    let fc = FlexFormattingContext::new();
    let font_ctx = FontContext::new();

    // Three items, each 100px wide
    let item1 = create_flex_item(100.0, 50.0);
    let item2 = create_flex_item(100.0, 50.0);
    let item3 = create_flex_item(100.0, 50.0);

    let container = BoxNode::new_flex(
        flex_container_style(FlexDirection::Row),
        vec![item1, item2, item3],
    );

    let constraints = LayoutConstraints::from_containing_block(400.0, 600.0);
    let fragment = fc.layout(&container, &constraints, &font_ctx).unwrap();

    // Items should be laid out horizontally
    assert_eq!(fragment.children[0].rect().origin.x, 0.0);
    assert_eq!(fragment.children[1].rect().origin.x, 100.0);
    assert_eq!(fragment.children[2].rect().origin.x, 200.0);

    // All at same Y
    assert_eq!(fragment.children[0].rect().origin.y, 0.0);
    assert_eq!(fragment.children[1].rect().origin.y, 0.0);
    assert_eq!(fragment.children[2].rect().origin.y, 0.0);
}

#[test]
fn test_flex_column_basic() {
    let fc = FlexFormattingContext::new();
    let font_ctx = FontContext::new();

    let item1 = create_flex_item(100.0, 50.0);
    let item2 = create_flex_item(100.0, 75.0);
    let item3 = create_flex_item(100.0, 25.0);

    let container = BoxNode::new_flex(
        flex_container_style(FlexDirection::Column),
        vec![item1, item2, item3],
    );

    let constraints = LayoutConstraints::from_containing_block(400.0, 600.0);
    let fragment = fc.layout(&container, &constraints, &font_ctx).unwrap();

    // Items should stack vertically
    assert_eq!(fragment.children[0].rect().origin.y, 0.0);
    assert_eq!(fragment.children[1].rect().origin.y, 50.0);
    assert_eq!(fragment.children[2].rect().origin.y, 125.0);

    // All at same X
    assert_eq!(fragment.children[0].rect().origin.x, 0.0);
    assert_eq!(fragment.children[1].rect().origin.x, 0.0);
    assert_eq!(fragment.children[2].rect().origin.x, 0.0);
}

#[test]
fn test_flex_grow() {
    let fc = FlexFormattingContext::new();
    let font_ctx = FontContext::new();

    // Two items: one with flex-grow: 1, one without
    let item1 = create_flex_item_with_grow(100.0, 50.0, 1.0);
    let item2 = create_flex_item(100.0, 50.0);

    let container = BoxNode::new_flex(
        flex_container_style(FlexDirection::Row),
        vec![item1, item2],
    );

    let constraints = LayoutConstraints::from_containing_block(400.0, 600.0);
    let fragment = fc.layout(&container, &constraints, &font_ctx).unwrap();

    // First item should grow to fill available space
    // Container width: 400
    // Item2 width: 100
    // Available for growth: 300
    // Item1 should be: 100 (base) + 300 (growth) = 400? No...
    // Actually: Item1 base = 100, Item2 = 100, total base = 200
    // Extra space = 400 - 200 = 200
    // Item1 gets all extra space (flex-grow = 1)
    assert_eq!(fragment.children[0].rect().size.width, 300.0);
    assert_eq!(fragment.children[1].rect().size.width, 100.0);
}

#[test]
fn test_flex_shrink() {
    let fc = FlexFormattingContext::new();
    let font_ctx = FontContext::new();

    // Two items, total wider than container
    let item1 = create_flex_item(250.0, 50.0);
    let item2 = create_flex_item(250.0, 50.0);

    let container = BoxNode::new_flex(
        flex_container_style(FlexDirection::Row),
        vec![item1, item2],
    );

    // Container only 400px wide, but items total 500px
    let constraints = LayoutConstraints::from_containing_block(400.0, 600.0);
    let fragment = fc.layout(&container, &constraints, &font_ctx).unwrap();

    // Items should shrink equally (default flex-shrink: 1)
    // Deficit: 500 - 400 = 100
    // Each shrinks by 50
    assert_eq!(fragment.children[0].rect().size.width, 200.0);
    assert_eq!(fragment.children[1].rect().size.width, 200.0);
}

#[test]
fn test_justify_content_space_between() {
    let fc = FlexFormattingContext::new();
    let font_ctx = FontContext::new();

    let item1 = create_flex_item(100.0, 50.0);
    let item2 = create_flex_item(100.0, 50.0);
    let item3 = create_flex_item(100.0, 50.0);

    let container = BoxNode::new_flex(
        flex_container_style_with_justify(
            FlexDirection::Row,
            JustifyContent::SpaceBetween,
        ),
        vec![item1, item2, item3],
    );

    let constraints = LayoutConstraints::from_containing_block(500.0, 600.0);
    let fragment = fc.layout(&container, &constraints, &font_ctx).unwrap();

    // Space between: first at start, last at end, equal spacing between
    // Items: 100 + 100 + 100 = 300
    // Container: 500
    // Space: 200
    // Gaps: 2 (between 3 items)
    // Gap size: 100
    assert_eq!(fragment.children[0].rect().origin.x, 0.0);
    assert_eq!(fragment.children[1].rect().origin.x, 200.0); // 100 + 100 gap
    assert_eq!(fragment.children[2].rect().origin.x, 400.0); // 200 + 100 width + 100 gap
}

#[test]
fn test_align_items_center() {
    let fc = FlexFormattingContext::new();
    let font_ctx = FontContext::new();

    // Different height items
    let item1 = create_flex_item(100.0, 50.0);
    let item2 = create_flex_item(100.0, 100.0);
    let item3 = create_flex_item(100.0, 75.0);

    let container = BoxNode::new_flex(
        flex_container_style_with_align(
            FlexDirection::Row,
            AlignItems::Center,
        ),
        vec![item1, item2, item3],
    );

    let constraints = LayoutConstraints::from_containing_block(400.0, 200.0);
    let fragment = fc.layout(&container, &constraints, &font_ctx).unwrap();

    // Container height determined by tallest item (100px)
    // Items should be vertically centered
    assert_eq!(fragment.children[0].rect().origin.y, 25.0); // (100 - 50) / 2
    assert_eq!(fragment.children[1].rect().origin.y, 0.0);  // Tallest, at top
    assert_eq!(fragment.children[2].rect().origin.y, 12.5); // (100 - 75) / 2
}

// Helper functions
fn create_flex_item(width: f32, height: f32) -> BoxNode {
    BoxNode::new_block(
        ComputedStyle {
            width: Some(Length::px(width)),
            height: Some(Length::px(height)),
            ..Default::default()
        },
        vec![],
    )
}

fn create_flex_item_with_grow(width: f32, height: f32, grow: f32) -> BoxNode {
    BoxNode::new_block(
        ComputedStyle {
            width: Some(Length::px(width)),
            height: Some(Length::px(height)),
            flex_grow: grow,
            ..Default::default()
        },
        vec![],
    )
}

fn flex_container_style(direction: FlexDirection) -> ComputedStyle {
    ComputedStyle {
        display: Display::Flex,
        flex_direction: direction,
        ..Default::default()
    }
}
```

### Step 4: WPT Test Integration (Day 3)

Run Web Platform Tests for flexbox:

```bash
# Download WPT flexbox tests
cd /home/user/fastrender
mkdir -p tests/wpt
git clone --depth 1 --filter=blob:none --sparse https://github.com/web-platform-tests/wpt.git tests/wpt/repo
cd tests/wpt/repo
git sparse-checkout set css/css-flexbox

# Run tests
cd /home/user/fastrender
cargo test --test wpt_flexbox
```

**File: `tests/wpt_flexbox.rs`**

```rust
//! Web Platform Tests for Flexbox
//!
//! Runs WPT flexbox tests against our implementation

use fastrender::test_utils::run_wpt_test;

#[test]
fn wpt_flexbox_basic() {
    run_wpt_test("css/css-flexbox/flexbox_basic.html");
}

#[test]
fn wpt_flexbox_flex_direction() {
    run_wpt_test("css/css-flexbox/flex-direction-001.html");
}

#[test]
fn wpt_flexbox_justify_content() {
    run_wpt_test("css/css-flexbox/justify-content-001.html");
}

// Add more WPT tests...
```

## Acceptance Criteria

- [ ] Basic flex row layout works
- [ ] Basic flex column layout works
- [ ] `flex-grow` distributes space correctly
- [ ] `flex-shrink` reduces space correctly
- [ ] `flex-basis` is respected
- [ ] `justify-content` values work (flex-start, center, space-between, etc.)
- [ ] `align-items` values work (flex-start, center, baseline, stretch)
- [ ] `align-content` works for multi-line flex containers
- [ ] `flex-wrap` works (wrap, nowrap, wrap-reverse)
- [ ] Nested flex containers work
- [ ] Passes WPT flexbox tests (at least 80%)
- [ ] Code follows 10-code-standards.md

## Common Pitfalls

### Pitfall 1: Not Creating Fresh Taffy Tree

**Wrong:**
```rust
struct FlexFormattingContext {
    taffy_tree: TaffyTree, // Reused across layouts - BUG!
}
```

**Right:**
```rust
// Create fresh tree for each layout
fn layout(&self, ...) -> Result<FragmentNode> {
    let mut taffy_tree = TaffyTree::new(); // Fresh tree
    // ...
}
```

### Pitfall 2: Forgetting to Convert Coordinates

Taffy uses relative coordinates (relative to parent), but we might use absolute coordinates.

**Wrong:**
```rust
let x = taffy_layout.location.x; // Relative to parent
// Use directly as absolute - BUG!
```

**Right:**
```rust
// Convert relative to absolute by adding parent offset
let absolute_x = parent_x + taffy_layout.location.x;
```

### Pitfall 3: Not Handling Percentage Sizes

**Wrong:**
```rust
fn dimension_to_taffy(&self, dim: Option<Length>) -> Dimension {
    match dim {
        Some(length) => Dimension::Length(length.value), // Assumes px!
        None => Dimension::Auto,
    }
}
```

**Right:**
```rust
fn dimension_to_taffy(&self, dim: Option<Length>) -> Dimension {
    match dim {
        Some(length) => match length.unit {
            LengthUnit::Px => Dimension::Length(length.value),
            LengthUnit::Percent => Dimension::Percent(length.value / 100.0),
            _ => Dimension::Auto,
        },
        None => Dimension::Auto,
    }
}
```

## Performance Considerations

Taffy is quite efficient, but:

1. **Don't create unnecessary nodes** - Taffy tree should mirror BoxNode structure
2. **Cache Taffy tree if box tree unchanged** - Advanced optimization for incremental layout
3. **Use Taffy's measure functions** - For text and replaced elements

## Next Steps

After flex layout:
- **02-grid-layout.md** - Grid layout (also via Taffy)
- Test flex + block/inline interaction
- Test nested flex containers

## References

- **CSS Flexible Box Layout Module Level 1:** https://www.w3.org/TR/css-flexbox-1/
- **Taffy documentation:** https://docs.rs/taffy/
- **Taffy flexbox implementation:** https://github.com/DioxusLabs/taffy
- **MDN Flexbox Guide:** https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Flexible_Box_Layout

---

**Last Updated:** 2025-01-19
**Status:** Ready for Implementation
