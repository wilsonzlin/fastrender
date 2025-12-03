# Phase 2: Grid Layout (Taffy Wrapper)

**Duration:** Week 4-5 of Phase 2 (3-5 days)
**Prerequisites:**
- Phase 1 complete (type system, formatting contexts, box generation)
- Block layout complete
- Flex layout complete (establishes Taffy integration pattern)
**Dependencies:**
- FormattingContext trait
- BoxNode, FragmentNode types
- Taffy library
**Output:** CSS Grid layout via Taffy integration

## Objectives

Implement CSS Grid layout by wrapping Taffy's grid algorithm. Like flexbox, this is **much simpler** than implementing from scratch because Taffy has a complete, spec-compliant grid implementation.

**Key Insight:** Grid is even more complex than flexbox (two-dimensional layout, track sizing, placement algorithm), making Taffy's implementation invaluable.

## Context

Grid is a two-dimensional layout system for creating complex layouts with rows and columns.

**From CSS Grid Layout Module Level 2:**
> "Grid Layout is a layout model optimized for two-dimensional layouts: those in which alignment of content is desired in both dimensions. Grid Layout divides space into rows and columns, and enables an author to specify the size and position of grid items either by referencing the lines that divide the grid into cells or by specifying areas of the grid."

**Examples of grid usage:**
- `display: grid` - Creates a grid container
- `grid-template-columns`, `grid-template-rows` - Define tracks
- `grid-template-areas` - Named grid areas
- `grid-column`, `grid-row` - Item placement
- `gap` - Spacing between tracks

## Why Use Taffy?

Grid layout is **extremely complex**:
- **Track sizing algorithm** - 12+ steps in the specification
- **Automatic placement** - Complex algorithm for placing items
- **Spanning items** - Items can span multiple tracks
- **Named grid lines and areas** - Complex naming system
- **Implicit grid** - Automatically creates tracks as needed

Implementing this from scratch would take months. Taffy already has it working.

## The Solution

Follow the same pattern as flex layout:
1. Convert BoxNode tree to Taffy tree
2. Let Taffy compute grid layout
3. Convert results back to FragmentNodes

## Step-by-Step Implementation

### Step 1: Create Grid FC Module (Day 1 Morning)

```bash
touch /home/user/fastrender/src/layout/contexts/grid.rs
```

**File: `src/layout/contexts/grid.rs`**

```rust
//! Grid Formatting Context (via Taffy)
//!
//! CSS Specification: CSS Grid Layout Module Level 2
//! https://www.w3.org/TR/css-grid-2/
//!
//! This is a thin wrapper around Taffy's grid implementation.

use super::{FormattingContext, FormattingContextType, Axis};
use crate::tree::{BoxNode, FragmentNode, BoxType};
use crate::layout::LayoutConstraints;
use crate::text::FontContext;
use crate::geometry::{Point, Size, Rect};
use crate::error::{Result, Error};
use taffy::prelude::*;
use std::collections::HashMap;

/// Grid Formatting Context
///
/// Delegates layout to Taffy's grid algorithm.
#[derive(Debug)]
pub struct GridFormattingContext {
    _phantom: std::marker::PhantomData<()>,
}

impl GridFormattingContext {
    pub fn new() -> Self {
        Self {
            _phantom: std::marker::PhantomData,
        }
    }
}

impl FormattingContext for GridFormattingContext {
    fn context_type(&self) -> FormattingContextType {
        FormattingContextType::Grid
    }

    fn layout(
        &self,
        box_node: &BoxNode,
        constraints: &LayoutConstraints,
        font_context: &FontContext,
    ) -> Result<FragmentNode> {
        // Same three-phase approach as flex layout
        let mut taffy_tree = TaffyTree::new();

        // Phase 1: Convert to Taffy tree
        let (root_node, box_to_taffy) = self.build_taffy_tree(
            &mut taffy_tree,
            box_node,
            font_context,
        )?;

        // Phase 2: Compute layout
        let available_space = self.constraints_to_available_space(constraints);
        taffy_tree.compute_layout(root_node, available_space)
            .map_err(|e| Error::Layout(format!("Taffy grid layout failed: {:?}", e)))?;

        // Phase 3: Convert back to FragmentNode
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
            .map_err(|e| Error::Layout(format!("Taffy grid intrinsic size failed: {:?}", e)))?;

        let layout = taffy_tree.layout(root_node)
            .map_err(|e| Error::Layout(format!("Failed to get layout: {:?}", e)))?;

        match axis {
            Axis::Horizontal => Ok(layout.size.width),
            Axis::Vertical => Ok(layout.size.height),
        }
    }
}

impl GridFormattingContext {
    /// Builds Taffy tree from BoxNode tree
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

    /// Creates a Taffy node for a BoxNode
    fn create_taffy_node(
        &self,
        taffy_tree: &mut TaffyTree,
        box_node: &BoxNode,
        font_context: &FontContext,
        box_to_taffy: &mut HashMap<BoxNodeId, NodeId>,
    ) -> Result<NodeId> {
        let taffy_style = self.box_style_to_taffy(&box_node.style);

        let taffy_node = if box_node.children.is_empty() {
            taffy_tree.new_leaf(taffy_style)
                .map_err(|e| Error::Layout(format!("Failed to create Taffy leaf: {:?}", e)))?
        } else {
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

            taffy_tree.new_with_children(taffy_style, &taffy_children)
                .map_err(|e| Error::Layout(format!("Failed to create Taffy node: {:?}", e)))?
        };

        box_to_taffy.insert(box_node.id(), taffy_node);
        Ok(taffy_node)
    }

    /// Converts ComputedStyle to Taffy Style
    fn box_style_to_taffy(&self, style: &ComputedStyle) -> Style {
        Style {
            display: Display::Grid, // Grid container

            // Grid container properties
            grid_template_rows: self.grid_template_to_taffy(&style.grid_template_rows),
            grid_template_columns: self.grid_template_to_taffy(&style.grid_template_columns),
            grid_auto_rows: self.grid_auto_tracks_to_taffy(&style.grid_auto_rows),
            grid_auto_columns: self.grid_auto_tracks_to_taffy(&style.grid_auto_columns),
            grid_auto_flow: self.grid_auto_flow_to_taffy(style.grid_auto_flow),

            // Grid item properties (for children)
            grid_row: self.grid_placement_to_taffy(&style.grid_row),
            grid_column: self.grid_placement_to_taffy(&style.grid_column),

            // Alignment
            justify_items: self.justify_items_to_taffy(style.justify_items),
            align_items: self.align_items_to_taffy(style.align_items),
            justify_content: self.justify_content_to_taffy(style.justify_content),
            align_content: self.align_content_to_taffy(style.align_content),

            // Gap
            gap: Size {
                width: self.length_to_taffy(style.column_gap),
                height: self.length_to_taffy(style.row_gap),
            },

            // Sizing (same as flex)
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

    /// Converts Taffy layout back to FragmentNode
    fn taffy_to_fragment(
        &self,
        taffy_tree: &TaffyTree,
        taffy_node: NodeId,
        box_node: &BoxNode,
        box_to_taffy: &HashMap<BoxNodeId, NodeId>,
    ) -> Result<FragmentNode> {
        let layout = taffy_tree.layout(taffy_node)
            .map_err(|e| Error::Layout(format!("Failed to get Taffy layout: {:?}", e)))?;

        let rect = Rect::new(
            Point::new(layout.location.x, layout.location.y),
            Size::new(layout.size.width, layout.size.height),
        );

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

    // Grid-specific type conversions

    /// Converts grid-template-columns/rows to Taffy
    ///
    /// Examples:
    /// - "200px 1fr 20%" → [Length(200), Fraction(1), Percent(20)]
    /// - "repeat(3, 1fr)" → [Fraction(1), Fraction(1), Fraction(1)]
    fn grid_template_to_taffy(&self, template: &GridTemplate) -> Vec<TrackSizingFunction> {
        match template {
            GridTemplate::None => vec![],
            GridTemplate::TrackList(tracks) => {
                tracks.iter().map(|track| self.track_to_taffy(track)).collect()
            }
            GridTemplate::Subgrid => {
                // TODO: Subgrid support
                vec![]
            }
        }
    }

    /// Converts a single track definition to Taffy
    fn track_to_taffy(&self, track: &GridTrack) -> TrackSizingFunction {
        match track {
            GridTrack::Length(len) => {
                let taffy_len = self.length_to_track_size(len);
                TrackSizingFunction::Single(taffy_len)
            }
            GridTrack::Flex(fr) => {
                TrackSizingFunction::Single(TrackSize::Flex(*fr))
            }
            GridTrack::MinMax(min, max) => {
                let min_size = self.length_to_track_size(min);
                let max_size = self.length_to_track_size(max);
                TrackSizingFunction::MinMax(min_size, max_size)
            }
            GridTrack::FitContent(len) => {
                let size = self.length_to_track_size(len);
                TrackSizingFunction::FitContent(size)
            }
            GridTrack::Auto => {
                TrackSizingFunction::Single(TrackSize::Auto)
            }
        }
    }

    fn length_to_track_size(&self, length: &Length) -> TrackSize {
        match length.unit {
            LengthUnit::Px => TrackSize::Points(length.value),
            LengthUnit::Percent => TrackSize::Percent(length.value / 100.0),
            LengthUnit::Fr => TrackSize::Flex(length.value),
            _ => TrackSize::Auto,
        }
    }

    /// Converts grid-auto-rows/columns
    fn grid_auto_tracks_to_taffy(&self, tracks: &[GridTrack]) -> Vec<TrackSizingFunction> {
        if tracks.is_empty() {
            vec![TrackSizingFunction::Single(TrackSize::Auto)]
        } else {
            tracks.iter().map(|t| self.track_to_taffy(t)).collect()
        }
    }

    /// Converts grid-auto-flow
    fn grid_auto_flow_to_taffy(&self, flow: GridAutoFlow) -> taffy::GridAutoFlow {
        match flow {
            GridAutoFlow::Row => taffy::GridAutoFlow::Row,
            GridAutoFlow::Column => taffy::GridAutoFlow::Column,
            GridAutoFlow::RowDense => taffy::GridAutoFlow::RowDense,
            GridAutoFlow::ColumnDense => taffy::GridAutoFlow::ColumnDense,
        }
    }

    /// Converts grid-row/column placement
    ///
    /// Examples:
    /// - "1 / 3" → start: 1, end: 3
    /// - "span 2" → span 2 tracks
    /// - "auto" → automatic placement
    fn grid_placement_to_taffy(&self, placement: &GridPlacement) -> Line<GridPlacement> {
        Line {
            start: self.grid_line_to_taffy(&placement.start),
            end: self.grid_line_to_taffy(&placement.end),
        }
    }

    fn grid_line_to_taffy(&self, line: &GridLine) -> taffy::GridPlacement {
        match line {
            GridLine::Auto => taffy::GridPlacement::Auto,
            GridLine::Line(n) => taffy::GridPlacement::Line(*n),
            GridLine::Span(n) => taffy::GridPlacement::Span(*n),
        }
    }

    // Reuse alignment conversions from flex layout
    fn justify_items_to_taffy(&self, justify: JustifyItems) -> Option<taffy::AlignItems> {
        Some(match justify {
            JustifyItems::Start => taffy::AlignItems::Start,
            JustifyItems::End => taffy::AlignItems::End,
            JustifyItems::Center => taffy::AlignItems::Center,
            JustifyItems::Stretch => taffy::AlignItems::Stretch,
        })
    }

    fn align_items_to_taffy(&self, align: AlignItems) -> Option<taffy::AlignItems> {
        Some(match align {
            AlignItems::Start => taffy::AlignItems::Start,
            AlignItems::End => taffy::AlignItems::End,
            AlignItems::Center => taffy::AlignItems::Center,
            AlignItems::Baseline => taffy::AlignItems::Baseline,
            AlignItems::Stretch => taffy::AlignItems::Stretch,
        })
    }

    // ... other conversion helpers (similar to flex layout)
    fn dimension_to_taffy(&self, dim: Option<Length>) -> Dimension {
        // Same as flex layout
        match dim {
            Some(length) => match length.unit {
                LengthUnit::Px => Dimension::Length(length.value),
                LengthUnit::Percent => Dimension::Percent(length.value / 100.0),
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
}

impl Default for GridFormattingContext {
    fn default() -> Self {
        Self::new()
    }
}
```

### Step 2: Grid Template Parsing (Day 1 Afternoon - Day 2)

Grid templates can be complex. Need to handle:
- Simple sizes: `200px 1fr 20%`
- Repeat notation: `repeat(3, 1fr)`
- Named lines: `[header-start] 1fr [header-end]`
- minmax(): `minmax(100px, 1fr)`
- fit-content(): `fit-content(500px)`

**File: `src/style/properties/grid.rs`**

```rust
//! Grid property types

use super::Length;

/// grid-template-columns / grid-template-rows
#[derive(Debug, Clone, PartialEq)]
pub enum GridTemplate {
    None,
    TrackList(Vec<GridTrack>),
    Subgrid,
}

/// A single track in a grid template
#[derive(Debug, Clone, PartialEq)]
pub enum GridTrack {
    /// Fixed length (px, %, em, etc.)
    Length(Length),

    /// Flexible fraction (fr units)
    Flex(f32),

    /// minmax(min, max)
    MinMax(Box<GridTrack>, Box<GridTrack>),

    /// fit-content(size)
    FitContent(Length),

    /// auto
    Auto,
}

/// grid-row / grid-column
#[derive(Debug, Clone, PartialEq)]
pub struct GridPlacement {
    pub start: GridLine,
    pub end: GridLine,
}

/// A grid line reference
#[derive(Debug, Clone, PartialEq)]
pub enum GridLine {
    /// auto (automatic placement)
    Auto,

    /// Line number (1-indexed)
    Line(i16),

    /// Span N tracks
    Span(u16),
}

/// grid-auto-flow
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum GridAutoFlow {
    Row,
    Column,
    RowDense,
    ColumnDense,
}

impl Default for GridTemplate {
    fn default() -> Self {
        Self::None
    }
}

impl Default for GridPlacement {
    fn default() -> Self {
        Self {
            start: GridLine::Auto,
            end: GridLine::Auto,
        }
    }
}

impl Default for GridAutoFlow {
    fn default() -> Self {
        Self::Row
    }
}
```

### Step 3: Comprehensive Tests (Day 3)

**File: `tests/layout/grid_layout_test.rs`**

```rust
//! Tests for Grid layout via Taffy

use fastrender::layout::contexts::GridFormattingContext;
use fastrender::tree::BoxNode;
use fastrender::style::*;

#[test]
fn test_grid_basic_2x2() {
    let fc = GridFormattingContext::new();
    let font_ctx = FontContext::new();

    // 2x2 grid with fixed track sizes
    let item1 = create_grid_item();
    let item2 = create_grid_item();
    let item3 = create_grid_item();
    let item4 = create_grid_item();

    let container = BoxNode::new_grid(
        grid_container_style(
            vec![GridTrack::Length(Length::px(100.0)), GridTrack::Length(Length::px(100.0))],
            vec![GridTrack::Length(Length::px(50.0)), GridTrack::Length(Length::px(50.0))],
        ),
        vec![item1, item2, item3, item4],
    );

    let constraints = LayoutConstraints::from_containing_block(400.0, 600.0);
    let fragment = fc.layout(&container, &constraints, &font_ctx).unwrap();

    // Check item positions in grid
    // Item 1: (0, 0)
    assert_eq!(fragment.children[0].rect().origin.x, 0.0);
    assert_eq!(fragment.children[0].rect().origin.y, 0.0);

    // Item 2: (100, 0)
    assert_eq!(fragment.children[1].rect().origin.x, 100.0);
    assert_eq!(fragment.children[1].rect().origin.y, 0.0);

    // Item 3: (0, 50)
    assert_eq!(fragment.children[2].rect().origin.x, 0.0);
    assert_eq!(fragment.children[2].rect().origin.y, 50.0);

    // Item 4: (100, 50)
    assert_eq!(fragment.children[3].rect().origin.x, 100.0);
    assert_eq!(fragment.children[3].rect().origin.y, 50.0);
}

#[test]
fn test_grid_fr_units() {
    let fc = GridFormattingContext::new();
    let font_ctx = FontContext::new();

    // Grid with fr units: 1fr 2fr (container 300px wide)
    let item1 = create_grid_item();
    let item2 = create_grid_item();

    let container = BoxNode::new_grid(
        grid_container_style(
            vec![GridTrack::Flex(1.0), GridTrack::Flex(2.0)],
            vec![GridTrack::Length(Length::px(100.0))],
        ),
        vec![item1, item2],
    );

    let constraints = LayoutConstraints::from_containing_block(300.0, 600.0);
    let fragment = fc.layout(&container, &constraints, &font_ctx).unwrap();

    // 1fr + 2fr = 3fr total
    // 300px / 3 = 100px per fr
    // Column 1: 1fr = 100px
    // Column 2: 2fr = 200px

    assert_eq!(fragment.children[0].rect().size.width, 100.0);
    assert_eq!(fragment.children[1].rect().size.width, 200.0);

    assert_eq!(fragment.children[1].rect().origin.x, 100.0);
}

#[test]
fn test_grid_span() {
    let fc = GridFormattingContext::new();
    let font_ctx = FontContext::new();

    // Item that spans 2 columns
    let item1 = create_grid_item_with_placement(
        GridPlacement {
            start: GridLine::Line(1),
            end: GridLine::Line(3), // Span from line 1 to 3 (2 columns)
        },
        GridPlacement::default(),
    );

    let item2 = create_grid_item();

    let container = BoxNode::new_grid(
        grid_container_style(
            vec![
                GridTrack::Length(Length::px(100.0)),
                GridTrack::Length(Length::px(100.0)),
                GridTrack::Length(Length::px(100.0)),
            ],
            vec![GridTrack::Length(Length::px(50.0))],
        ),
        vec![item1, item2],
    );

    let constraints = LayoutConstraints::from_containing_block(400.0, 600.0);
    let fragment = fc.layout(&container, &constraints, &font_ctx).unwrap();

    // Item 1 should span 200px (2 columns)
    assert_eq!(fragment.children[0].rect().size.width, 200.0);

    // Item 2 should be placed in third column
    assert_eq!(fragment.children[1].rect().origin.x, 200.0);
}

#[test]
fn test_grid_auto_flow() {
    let fc = GridFormattingContext::new();
    let font_ctx = FontContext::new();

    // 2-column grid with more items than explicit tracks
    // Should create implicit rows
    let items: Vec<_> = (0..6).map(|_| create_grid_item()).collect();

    let container = BoxNode::new_grid(
        grid_container_style_with_flow(
            vec![GridTrack::Length(Length::px(100.0)), GridTrack::Length(Length::px(100.0))],
            vec![GridTrack::Length(Length::px(50.0))], // Only 1 explicit row
            GridAutoFlow::Row,
        ),
        items,
    );

    let constraints = LayoutConstraints::from_containing_block(400.0, 600.0);
    let fragment = fc.layout(&container, &constraints, &font_ctx).unwrap();

    // Should create 3 rows (1 explicit + 2 implicit)
    // Check positions
    assert_eq!(fragment.children[0].rect().origin.y, 0.0);   // Row 0
    assert_eq!(fragment.children[2].rect().origin.y, 50.0);  // Row 1
    assert_eq!(fragment.children[4].rect().origin.y, 100.0); // Row 2
}

#[test]
fn test_grid_gap() {
    let fc = GridFormattingContext::new();
    let font_ctx = FontContext::new();

    let item1 = create_grid_item();
    let item2 = create_grid_item();
    let item3 = create_grid_item();
    let item4 = create_grid_item();

    let container = BoxNode::new_grid(
        grid_container_style_with_gap(
            vec![GridTrack::Length(Length::px(100.0)), GridTrack::Length(Length::px(100.0))],
            vec![GridTrack::Length(Length::px(50.0)), GridTrack::Length(Length::px(50.0))],
            Length::px(10.0), // column gap
            Length::px(5.0),  // row gap
        ),
        vec![item1, item2, item3, item4],
    );

    let constraints = LayoutConstraints::from_containing_block(400.0, 600.0);
    let fragment = fc.layout(&container, &constraints, &font_ctx).unwrap();

    // Item 2 should be offset by column gap
    assert_eq!(fragment.children[1].rect().origin.x, 110.0); // 100 + 10 gap

    // Item 3 should be offset by row gap
    assert_eq!(fragment.children[2].rect().origin.y, 55.0); // 50 + 5 gap
}

#[test]
fn test_grid_minmax() {
    let fc = GridFormattingContext::new();
    let font_ctx = FontContext::new();

    // Track with minmax(100px, 1fr)
    let item1 = create_grid_item();

    let container = BoxNode::new_grid(
        grid_container_style(
            vec![GridTrack::MinMax(
                Box::new(GridTrack::Length(Length::px(100.0))),
                Box::new(GridTrack::Flex(1.0)),
            )],
            vec![GridTrack::Length(Length::px(50.0))],
        ),
        vec![item1],
    );

    // Wide container - track should grow beyond min
    let constraints = LayoutConstraints::from_containing_block(400.0, 600.0);
    let fragment = fc.layout(&container, &constraints, &font_ctx).unwrap();

    assert_eq!(fragment.children[0].rect().size.width, 400.0); // Grows to fill

    // Narrow container - track should respect min
    let constraints = LayoutConstraints::from_containing_block(50.0, 600.0);
    let fragment = fc.layout(&container, &constraints, &font_ctx).unwrap();

    assert_eq!(fragment.children[0].rect().size.width, 100.0); // Min width
}

// Helper functions
fn create_grid_item() -> BoxNode {
    BoxNode::new_block(ComputedStyle::default(), vec![])
}

fn create_grid_item_with_placement(
    column: GridPlacement,
    row: GridPlacement,
) -> BoxNode {
    BoxNode::new_block(
        ComputedStyle {
            grid_column: column,
            grid_row: row,
            ..Default::default()
        },
        vec![],
    )
}

fn grid_container_style(
    columns: Vec<GridTrack>,
    rows: Vec<GridTrack>,
) -> ComputedStyle {
    ComputedStyle {
        display: Display::Grid,
        grid_template_columns: GridTemplate::TrackList(columns),
        grid_template_rows: GridTemplate::TrackList(rows),
        ..Default::default()
    }
}

fn grid_container_style_with_gap(
    columns: Vec<GridTrack>,
    rows: Vec<GridTrack>,
    column_gap: Length,
    row_gap: Length,
) -> ComputedStyle {
    ComputedStyle {
        display: Display::Grid,
        grid_template_columns: GridTemplate::TrackList(columns),
        grid_template_rows: GridTemplate::TrackList(rows),
        column_gap,
        row_gap,
        ..Default::default()
    }
}
```

### Step 4: WPT Test Integration (Day 4)

```bash
# Download WPT grid tests
cd /home/user/fastrender/tests/wpt/repo
git sparse-checkout add css/css-grid

# Run tests
cd /home/user/fastrender
cargo test --test wpt_grid
```

## Acceptance Criteria

- [ ] Basic grid layout works (rows and columns)
- [ ] `fr` units distribute space correctly
- [ ] `grid-column` and `grid-row` placement works
- [ ] Spanning items work (`grid-column: 1 / 3`)
- [ ] `minmax()` sizing works
- [ ] `repeat()` notation works
- [ ] `grid-auto-flow` creates implicit tracks
- [ ] `gap` property adds spacing
- [ ] Named grid lines work (stretch goal)
- [ ] Grid template areas work (stretch goal)
- [ ] Nested grids work
- [ ] Passes WPT grid tests (at least 70%)
- [ ] Code follows 10-code-standards.md

## Common Pitfalls

### Pitfall 1: Confusing Line Numbers and Track Numbers

**Wrong:**
```rust
// Grid lines are 1-indexed, but thinking in 0-indexed tracks
grid_column: 1 / 2  // This is the FIRST track, not second!
```

**Right:**
```rust
// Grid lines:  1    2    3
// Tracks:     [ 0 ][ 1 ]
// grid-column: 1 / 2 means first track (between lines 1 and 2)
// grid-column: 1 / 3 means span tracks 0 and 1
```

### Pitfall 2: Not Handling Negative Line Numbers

Grid lines can be negative (counting from the end):

```rust
// -1 is the last line
// -2 is the second-to-last line
grid_column: 1 / -1  // Span all columns
```

### Pitfall 3: Forgetting Implicit Grid

**Wrong:**
```rust
// Only defining 2 columns but placing item in column 5
// Ignoring the implicit grid creation
```

**Right:**
```rust
// Taffy will create implicit tracks as needed
// Just need to set grid-auto-columns for sizing
```

## Performance Considerations

Grid layout is expensive:
1. **Track sizing is O(n*m)** where n=tracks, m=items
2. **Avoid deep nesting** - Each level multiplies complexity
3. **Cache grid structures** - Don't rebuild for small changes

## Advanced Features (Stretch Goals)

### Named Grid Lines

```css
grid-template-columns: [start] 1fr [middle] 1fr [end];
grid-column: start / middle;
```

### Grid Template Areas

```css
grid-template-areas:
  "header header"
  "sidebar content"
  "footer footer";

grid-area: header;
```

These require additional parsing but Taffy supports them.

## Next Steps

After grid layout:
- **02-float-layout.md** - Float positioning
- Test grid + other layout modes
- Performance optimization for large grids

## References

- **CSS Grid Layout Module Level 2:** https://www.w3.org/TR/css-grid-2/
- **CSS Grid Layout Module Level 1:** https://www.w3.org/TR/css-grid-1/
- **Taffy documentation:** https://docs.rs/taffy/
- **MDN Grid Guide:** https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Grid_Layout
- **Grid by Example:** https://gridbyexample.com/

---

**Last Updated:** 2025-01-19
**Status:** Ready for Implementation
