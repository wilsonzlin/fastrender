# Phase 1.1: Type System

**Duration:** 1 week
**Prerequisites:** Phase 0 complete, architecture decisions made
**Dependencies:** None
**Output:** Core type system for boxes, fragments, and layout

## Objectives

Create the fundamental type system that will underpin the entire rendering engine:
1. Box Tree types (CSS boxes)
2. Fragment Tree types (layout results)
3. Formatting Context types
4. Supporting types (Rect, Size, etc.)

## Context

Current code conflates styled DOM nodes with layout results in a single `LayoutBox` type. This violates separation of concerns and makes the code hard to reason about.

We need to establish the correct type hierarchy before implementing any layout algorithms.

## Step 1: Create Core Geometry Types

**File:** `src/geom.rs` (NEW)

**Implementation:**

```rust
//! Core geometry types for layout and painting
//!
//! These types represent geometric concepts used throughout the engine.
//! All units are in CSS pixels unless otherwise noted.

use std::fmt;

/// A 2D point in CSS pixel space
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Point {
    pub x: f32,
    pub y: f32,
}

impl Point {
    pub const ZERO: Self = Self { x: 0.0, y: 0.0 };

    pub const fn new(x: f32, y: f32) -> Self {
        Self { x, y }
    }
}

/// A 2D size in CSS pixels
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Size {
    pub width: f32,
    pub height: f32,
}

impl Size {
    pub const ZERO: Self = Self { width: 0.0, height: 0.0 };

    pub const fn new(width: f32, height: f32) -> Self {
        Self { width, height }
    }

    pub fn area(&self) -> f32 {
        self.width * self.height
    }
}

/// An axis-aligned rectangle in CSS pixel space
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Rect {
    pub origin: Point,
    pub size: Size,
}

impl Rect {
    pub const ZERO: Self = Self {
        origin: Point::ZERO,
        size: Size::ZERO,
    };

    pub const fn new(origin: Point, size: Size) -> Self {
        Self { origin, size }
    }

    pub const fn from_xywh(x: f32, y: f32, width: f32, height: f32) -> Self {
        Self {
            origin: Point::new(x, y),
            size: Size::new(width, height),
        }
    }

    pub fn x(&self) -> f32 {
        self.origin.x
    }

    pub fn y(&self) -> f32 {
        self.origin.y
    }

    pub fn width(&self) -> f32 {
        self.size.width
    }

    pub fn height(&self) -> f32 {
        self.size.height
    }

    pub fn min_x(&self) -> f32 {
        self.origin.x
    }

    pub fn max_x(&self) -> f32 {
        self.origin.x + self.size.width
    }

    pub fn min_y(&self) -> f32 {
        self.origin.y
    }

    pub fn max_y(&self) -> f32 {
        self.origin.y + self.size.height
    }

    pub fn contains_point(&self, point: Point) -> bool {
        point.x >= self.min_x()
            && point.x <= self.max_x()
            && point.y >= self.min_y()
            && point.y <= self.max_y()
    }

    pub fn intersects(&self, other: &Rect) -> bool {
        self.min_x() < other.max_x()
            && self.max_x() > other.min_x()
            && self.min_y() < other.max_y()
            && self.max_y() > other.min_y()
    }
}

/// Edge offsets (top, right, bottom, left)
/// Used for margin, padding, border
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct EdgeOffsets {
    pub top: f32,
    pub right: f32,
    pub bottom: f32,
    pub left: f32,
}

impl EdgeOffsets {
    pub const ZERO: Self = Self {
        top: 0.0,
        right: 0.0,
        bottom: 0.0,
        left: 0.0,
    };

    pub const fn all(value: f32) -> Self {
        Self {
            top: value,
            right: value,
            bottom: value,
            left: value,
        }
    }

    pub const fn new(top: f32, right: f32, bottom: f32, left: f32) -> Self {
        Self { top, right, bottom, left }
    }

    pub fn horizontal(&self) -> f32 {
        self.left + self.right
    }

    pub fn vertical(&self) -> f32 {
        self.top + self.bottom
    }
}
```

**Tests:** `src/geom.rs` (include inline)

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rect_contains_point() {
        let rect = Rect::from_xywh(10.0, 10.0, 20.0, 20.0);
        assert!(rect.contains_point(Point::new(15.0, 15.0)));
        assert!(!rect.contains_point(Point::new(5.0, 5.0)));
    }

    #[test]
    fn test_rect_intersects() {
        let rect1 = Rect::from_xywh(0.0, 0.0, 10.0, 10.0);
        let rect2 = Rect::from_xywh(5.0, 5.0, 10.0, 10.0);
        let rect3 = Rect::from_xywh(20.0, 20.0, 10.0, 10.0);

        assert!(rect1.intersects(&rect2));
        assert!(!rect1.intersects(&rect3));
    }

    #[test]
    fn test_edge_offsets() {
        let edges = EdgeOffsets::all(10.0);
        assert_eq!(edges.horizontal(), 20.0);
        assert_eq!(edges.vertical(), 20.0);
    }
}
```

## Step 2: Create Box Tree Types

**File:** `src/tree/box_tree.rs` (NEW)

**Implementation:**

```rust
//! Box Tree - Represents CSS boxes
//!
//! The box tree is generated from the styled DOM tree and represents
//! the CSS box model. It's independent of layout - it only represents
//! what boxes exist and their styling, not where they're positioned.
//!
//! Reference: CSS Display Module Level 3
//! https://www.w3.org/TR/css-display-3/

use crate::style::ComputedStyle;
use std::sync::Arc;

/// A tree of CSS boxes
///
/// Generated from styled DOM, consumed by layout algorithms.
/// Immutable after construction (layout never modifies boxes).
#[derive(Debug, Clone)]
pub struct BoxTree {
    pub root: BoxNode,
}

impl BoxTree {
    pub fn new(root: BoxNode) -> Self {
        Self { root }
    }
}

/// A single box in the box tree
///
/// Represents a CSS box (could be element, text, anonymous, etc.)
#[derive(Debug, Clone)]
pub struct BoxNode {
    /// Computed style for this box (shared with fragments)
    pub style: Arc<ComputedStyle>,

    /// What kind of box is this?
    pub box_type: BoxType,

    /// Child boxes
    pub children: Vec<BoxNode>,

    /// Debug information (element name, class, id)
    pub debug_info: Option<DebugInfo>,
}

/// Different types of boxes in the box tree
#[derive(Debug, Clone)]
pub enum BoxType {
    /// Block-level box (div, p, h1, etc.)
    Block(BlockBox),

    /// Inline-level box (span, a, em, etc.)
    Inline(InlineBox),

    /// Text box (actual text content)
    Text(TextBox),

    /// Replaced element (img, video, canvas, etc.)
    Replaced(ReplacedBox),

    /// Anonymous box (generated by layout algorithm)
    /// Example: table generates anonymous table boxes
    Anonymous(AnonymousBox),
}

/// Block-level box
#[derive(Debug, Clone)]
pub struct BlockBox {
    /// What formatting context does this establish?
    pub formatting_context: FormattingContextType,
}

/// Inline-level box
#[derive(Debug, Clone)]
pub struct InlineBox {
    /// For inline-block, what FC does it establish?
    pub formatting_context: Option<FormattingContextType>,
}

/// Text box
#[derive(Debug, Clone)]
pub struct TextBox {
    /// The actual text content
    pub text: String,
}

/// Replaced element box
#[derive(Debug, Clone)]
pub struct ReplacedBox {
    /// Type of replaced element
    pub replaced_type: ReplacedType,

    /// Intrinsic size (if known)
    pub intrinsic_size: Option<crate::geom::Size>,
}

#[derive(Debug, Clone)]
pub enum ReplacedType {
    Image { src: String },
    Video { src: String },
    Canvas,
    Svg { content: String },
}

/// Anonymous box
#[derive(Debug, Clone)]
pub struct AnonymousBox {
    /// What kind of anonymous box?
    pub anonymous_type: AnonymousType,
}

#[derive(Debug, Clone)]
pub enum AnonymousType {
    /// Anonymous block box
    Block,
    /// Anonymous inline box
    Inline,
    /// Anonymous table wrapper
    TableWrapper,
    /// Anonymous table row
    TableRow,
    /// Other anonymous boxes as needed
}

/// Types of formatting contexts
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FormattingContextType {
    /// Block Formatting Context
    Block,

    /// Inline Formatting Context
    Inline,

    /// Flex Formatting Context
    Flex,

    /// Grid Formatting Context
    Grid,

    /// Table Formatting Context
    Table,

    /// Ruby Formatting Context (future)
    Ruby,
}

/// Debug information for a box
#[derive(Debug, Clone)]
pub struct DebugInfo {
    /// Element tag name (if from element)
    pub tag_name: Option<String>,

    /// Element classes
    pub classes: Vec<String>,

    /// Element ID
    pub id: Option<String>,
}

impl BoxNode {
    /// Create a new block box
    pub fn new_block(
        style: Arc<ComputedStyle>,
        fc: FormattingContextType,
        children: Vec<BoxNode>,
    ) -> Self {
        Self {
            style,
            box_type: BoxType::Block(BlockBox {
                formatting_context: fc,
            }),
            children,
            debug_info: None,
        }
    }

    /// Create a new inline box
    pub fn new_inline(
        style: Arc<ComputedStyle>,
        children: Vec<BoxNode>,
    ) -> Self {
        Self {
            style,
            box_type: BoxType::Inline(InlineBox {
                formatting_context: None,
            }),
            children,
            debug_info: None,
        }
    }

    /// Create a new text box
    pub fn new_text(style: Arc<ComputedStyle>, text: String) -> Self {
        Self {
            style,
            box_type: BoxType::Text(TextBox { text }),
            children: Vec::new(),
            debug_info: None,
        }
    }

    /// Create a new replaced box
    pub fn new_replaced(
        style: Arc<ComputedStyle>,
        replaced_type: ReplacedType,
        intrinsic_size: Option<crate::geom::Size>,
    ) -> Self {
        Self {
            style,
            box_type: BoxType::Replaced(ReplacedBox {
                replaced_type,
                intrinsic_size,
            }),
            children: Vec::new(),
            debug_info: None,
        }
    }

    /// Add debug information
    pub fn with_debug_info(mut self, info: DebugInfo) -> Self {
        self.debug_info = Some(info);
        self
    }

    /// Get the formatting context this box establishes
    pub fn formatting_context(&self) -> Option<FormattingContextType> {
        match &self.box_type {
            BoxType::Block(block) => Some(block.formatting_context),
            BoxType::Inline(inline) => inline.formatting_context,
            BoxType::Replaced(_) => Some(FormattingContextType::Block),
            _ => None,
        }
    }

    /// Is this box a block-level box?
    pub fn is_block_level(&self) -> bool {
        matches!(
            &self.box_type,
            BoxType::Block(_) | BoxType::Replaced(_) | BoxType::Anonymous(_)
        )
    }

    /// Is this box an inline-level box?
    pub fn is_inline_level(&self) -> bool {
        matches!(&self.box_type, BoxType::Inline(_) | BoxType::Text(_))
    }
}
```

**Tests:** `tests/unit/tree/box_tree_test.rs` (NEW)

```rust
use fastrender::tree::box_tree::*;
use fastrender::style::ComputedStyle;
use std::sync::Arc;

#[test]
fn test_create_block_box() {
    let style = Arc::new(ComputedStyle::default());
    let box_node = BoxNode::new_block(
        style,
        FormattingContextType::Block,
        vec![],
    );

    assert!(box_node.is_block_level());
    assert!(!box_node.is_inline_level());
    assert_eq!(
        box_node.formatting_context(),
        Some(FormattingContextType::Block)
    );
}

#[test]
fn test_create_text_box() {
    let style = Arc::new(ComputedStyle::default());
    let box_node = BoxNode::new_text(style, "Hello, World!".to_string());

    assert!(box_node.is_inline_level());
    assert!(!box_node.is_block_level());
    assert_eq!(box_node.formatting_context(), None);

    if let BoxType::Text(text_box) = &box_node.box_type {
        assert_eq!(text_box.text, "Hello, World!");
    } else {
        panic!("Expected text box");
    }
}

#[test]
fn test_box_tree_hierarchy() {
    let style = Arc::new(ComputedStyle::default());

    let text1 = BoxNode::new_text(style.clone(), "Text 1".to_string());
    let text2 = BoxNode::new_text(style.clone(), "Text 2".to_string());

    let inline_box = BoxNode::new_inline(style.clone(), vec![text1, text2]);
    let block_box = BoxNode::new_block(
        style.clone(),
        FormattingContextType::Block,
        vec![inline_box],
    );

    assert_eq!(block_box.children.len(), 1);
    assert_eq!(block_box.children[0].children.len(), 2);
}
```

## Step 3: Create Fragment Tree Types

**File:** `src/tree/fragment.rs` (NEW)

**Implementation:**

```rust
//! Fragment Tree - Represents layout results
//!
//! Fragments are the result of laying out boxes. A single box may produce
//! multiple fragments (e.g., text broken across multiple lines).
//!
//! Unlike boxes, fragments have concrete positions and sizes.
//!
//! Reference: CSS Fragmentation Module Level 3
//! https://www.w3.org/TR/css-break-3/

use crate::geom::{Point, Rect, Size};
use crate::style::ComputedStyle;
use crate::text::ShapedText;
use std::sync::Arc;

/// A tree of fragments (layout results)
#[derive(Debug, Clone)]
pub struct FragmentTree {
    pub root: Fragment,
}

impl FragmentTree {
    pub fn new(root: Fragment) -> Self {
        Self { root }
    }
}

/// A single fragment (laid-out box or portion of a box)
#[derive(Debug, Clone)]
pub struct Fragment {
    /// Position and size of this fragment
    pub bounds: Rect,

    /// What's inside this fragment?
    pub content: FragmentContent,

    /// Style (shared with BoxNode)
    pub style: Arc<ComputedStyle>,

    /// Child fragments
    pub children: Vec<Fragment>,
}

/// Content types for fragments
#[derive(Debug, Clone)]
pub enum FragmentContent {
    /// Block box content
    Block,

    /// Inline box content
    Inline,

    /// Text content (with shaping information)
    Text {
        shaped: Arc<ShapedText>,
        /// Range within the shaped text [start, end)
        range: (usize, usize),
    },

    /// Replaced element content
    Replaced {
        replaced_type: ReplacedContentType,
    },

    /// Anonymous box (no content)
    Anonymous,
}

#[derive(Debug, Clone)]
pub enum ReplacedContentType {
    Image {
        src: String,
    },
    Video {
        src: String,
    },
    Canvas,
    Svg {
        content: String,
    },
}

impl Fragment {
    /// Create a new block fragment
    pub fn new_block(
        bounds: Rect,
        style: Arc<ComputedStyle>,
        children: Vec<Fragment>,
    ) -> Self {
        Self {
            bounds,
            content: FragmentContent::Block,
            style,
            children,
        }
    }

    /// Create a new inline fragment
    pub fn new_inline(
        bounds: Rect,
        style: Arc<ComputedStyle>,
        children: Vec<Fragment>,
    ) -> Self {
        Self {
            bounds,
            content: FragmentContent::Inline,
            style,
            children,
        }
    }

    /// Create a new text fragment
    pub fn new_text(
        bounds: Rect,
        style: Arc<ComputedStyle>,
        shaped: Arc<ShapedText>,
        range: (usize, usize),
    ) -> Self {
        Self {
            bounds,
            content: FragmentContent::Text { shaped, range },
            style,
            children: Vec::new(),
        }
    }

    /// Create a new replaced fragment
    pub fn new_replaced(
        bounds: Rect,
        style: Arc<ComputedStyle>,
        replaced_type: ReplacedContentType,
    ) -> Self {
        Self {
            bounds,
            content: FragmentContent::Replaced { replaced_type },
            style,
            children: Vec::new(),
        }
    }

    /// Get the position of this fragment
    pub fn position(&self) -> Point {
        self.bounds.origin
    }

    /// Get the size of this fragment
    pub fn size(&self) -> Size {
        self.bounds.size
    }

    /// Get the bounding box that contains this fragment and all children
    pub fn bounding_box(&self) -> Rect {
        let mut bbox = self.bounds;

        for child in &self.children {
            let child_bbox = child.bounding_box();
            bbox = union_rects(bbox, child_bbox);
        }

        bbox
    }

    /// Translate this fragment and all children by an offset
    pub fn translate(&mut self, offset: Point) {
        self.bounds.origin.x += offset.x;
        self.bounds.origin.y += offset.y;

        for child in &mut self.children {
            child.translate(offset);
        }
    }
}

/// Compute the union of two rectangles
fn union_rects(a: Rect, b: Rect) -> Rect {
    let min_x = a.min_x().min(b.min_x());
    let min_y = a.min_y().min(b.min_y());
    let max_x = a.max_x().max(b.max_x());
    let max_y = a.max_y().max(b.max_y());

    Rect::from_xywh(min_x, min_y, max_x - min_x, max_y - min_y)
}
```

**Tests:** `tests/unit/tree/fragment_test.rs` (NEW)

```rust
use fastrender::geom::{Point, Rect};
use fastrender::tree::fragment::*;
use fastrender::style::ComputedStyle;
use std::sync::Arc;

#[test]
fn test_create_block_fragment() {
    let style = Arc::new(ComputedStyle::default());
    let bounds = Rect::from_xywh(10.0, 10.0, 100.0, 50.0);

    let fragment = Fragment::new_block(bounds, style, vec![]);

    assert_eq!(fragment.position(), Point::new(10.0, 10.0));
    assert_eq!(fragment.size().width, 100.0);
    assert_eq!(fragment.size().height, 50.0);
}

#[test]
fn test_fragment_translate() {
    let style = Arc::new(ComputedStyle::default());
    let bounds = Rect::from_xywh(0.0, 0.0, 10.0, 10.0);

    let mut fragment = Fragment::new_block(bounds, style, vec![]);
    fragment.translate(Point::new(5.0, 3.0));

    assert_eq!(fragment.position(), Point::new(5.0, 3.0));
}

#[test]
fn test_fragment_bounding_box() {
    let style = Arc::new(ComputedStyle::default());

    let parent = Fragment::new_block(
        Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
        style.clone(),
        vec![
            Fragment::new_block(
                Rect::from_xywh(10.0, 10.0, 20.0, 20.0),
                style.clone(),
                vec![],
            ),
            Fragment::new_block(
                Rect::from_xywh(50.0, 50.0, 60.0, 60.0),
                style.clone(),
                vec![],
            ),
        ],
    );

    let bbox = parent.bounding_box();
    assert_eq!(bbox.min_x(), 0.0);
    assert_eq!(bbox.min_y(), 0.0);
    assert_eq!(bbox.max_x(), 110.0); // 50 + 60
    assert_eq!(bbox.max_y(), 110.0);
}
```

## Step 4: Create Formatting Context Types

**File:** `src/layout/formatting_context.rs` (NEW)

**Implementation:**

```rust
//! Formatting Contexts
//!
//! A formatting context is an environment in which boxes are laid out.
//! Different formatting contexts have different layout rules.
//!
//! Reference: CSS Display Module Level 3, Section 3
//! https://www.w3.org/TR/css-display-3/#formatting-context

use crate::geom::Size;
use crate::tree::box_tree::BoxNode;
use crate::tree::fragment::Fragment;

/// Constraints passed to layout algorithms
#[derive(Debug, Clone, Copy)]
pub struct Constraints {
    /// Available width (may be infinite)
    pub available_width: AvailableSpace,

    /// Available height (may be infinite)
    pub available_height: AvailableSpace,

    /// Containing block size (for percentage resolution)
    pub containing_block: Size,
}

/// Available space in a dimension
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AvailableSpace {
    /// Definite size (e.g., width: 800px)
    Definite(f32),

    /// Min-content size (shrink to fit content)
    MinContent,

    /// Max-content size (expand to fit content)
    MaxContent,
}

impl AvailableSpace {
    pub fn definite(size: f32) -> Self {
        Self::Definite(size)
    }

    pub fn is_definite(&self) -> bool {
        matches!(self, Self::Definite(_))
    }

    pub fn to_option(&self) -> Option<f32> {
        match self {
            Self::Definite(size) => Some(*size),
            _ => None,
        }
    }
}

/// Common trait for all formatting contexts
pub trait FormattingContext {
    /// Layout a box within this formatting context
    ///
    /// Takes a box and constraints, returns a fragment tree.
    fn layout(
        &mut self,
        box_node: &BoxNode,
        constraints: Constraints,
    ) -> Fragment;
}

/// Result of layout computation
#[derive(Debug, Clone)]
pub struct LayoutResult {
    /// The root fragment
    pub fragment: Fragment,

    /// Actual size used (may differ from available space)
    pub used_size: Size,
}

impl Constraints {
    pub fn new(width: AvailableSpace, height: AvailableSpace) -> Self {
        Self {
            available_width: width,
            available_height: height,
            containing_block: Size::ZERO,
        }
    }

    pub fn with_containing_block(mut self, size: Size) -> Self {
        self.containing_block = size;
        self
    }

    pub fn definite(width: f32, height: f32) -> Self {
        Self {
            available_width: AvailableSpace::Definite(width),
            available_height: AvailableSpace::Definite(height),
            containing_block: Size::new(width, height),
        }
    }
}
```

**Tests:** `tests/unit/layout/formatting_context_test.rs` (NEW)

```rust
use fastrender::layout::formatting_context::*;

#[test]
fn test_available_space() {
    let definite = AvailableSpace::Definite(100.0);
    assert!(definite.is_definite());
    assert_eq!(definite.to_option(), Some(100.0));

    let min_content = AvailableSpace::MinContent;
    assert!(!min_content.is_definite());
    assert_eq!(min_content.to_option(), None);
}

#[test]
fn test_constraints() {
    let constraints = Constraints::definite(800.0, 600.0);
    assert_eq!(
        constraints.available_width,
        AvailableSpace::Definite(800.0)
    );
    assert_eq!(
        constraints.available_height,
        AvailableSpace::Definite(600.0)
    );
    assert_eq!(constraints.containing_block.width, 800.0);
}
```

## Step 5: Update Module Structure

**File:** `src/lib.rs` (UPDATE)

```rust
// Geometry primitives
pub mod geom;

// Tree structures
pub mod tree {
    pub mod box_tree;
    pub mod fragment;
}

// Layout
pub mod layout {
    pub mod formatting_context;
    // More modules will be added in later phases
}

// Existing modules (keep these)
pub mod dom;
pub mod css;
pub mod style;
pub mod text;
pub mod paint;
pub mod image_output;
pub mod image_loader;
pub mod error;
pub mod renderer;

// Re-exports
pub use error::{Error, Result};
pub use geom::{Point, Rect, Size, EdgeOffsets};
pub use tree::box_tree::{BoxTree, BoxNode};
pub use tree::fragment::{FragmentTree, Fragment};
```

## Step 6: Cargo.toml Updates

**File:** `Cargo.toml` (NO CHANGES NEEDED)

The existing dependencies are sufficient for the type system.

## Acceptance Criteria

This phase is complete when:

- [ ] All files compile without warnings
- [ ] All tests pass (`cargo test`)
- [ ] Documentation is complete (`cargo doc`)
- [ ] Types are immutable where appropriate
- [ ] Arc is used correctly for shared data
- [ ] No `pub` fields that should be private
- [ ] Debug implementations work correctly

## Verification Commands

```bash
# Compile
cargo build

# Run tests
cargo test

# Check documentation
cargo doc --no-deps --open

# Check for warnings
cargo clippy -- -D warnings

# Format code
cargo fmt -- --check
```

## Common Issues for AI Agents

1. **Don't mix Box and Fragment**: They're separate! Boxes don't have positions.
2. **Use Arc correctly**: ComputedStyle should be Arc<> not cloned
3. **Immutability**: BoxNode should be immutable (no interior mutability)
4. **Debug info**: Don't forget to include DebugInfo for debugging
5. **Tests**: Write tests as you implement, not after

## Next Steps

After completing this phase:
1. Review code with human
2. Proceed to [`docs/core/formatting-contexts.md`](01-formatting-contexts.md)

## Notes for Future Phases

These types will be used extensively in:
- Box generation (Phase 1.3)
- Layout algorithms (Phase 2)
- Paint (Phase 4)

Get them right now to avoid refactoring later.
