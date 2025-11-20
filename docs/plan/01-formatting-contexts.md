# Phase 1: Formatting Contexts

**Duration:** Week 2 of Phase 1 (5-7 days)
**Prerequisites:** Type system implementation complete (01-type-system.md)
**Dependencies:**
- 00-architecture-decisions.md (ADR-004)
- 01-type-system.md (BoxNode, FragmentNode types)
**Output:** Generic formatting context trait and implementations

## Objectives

Create the abstraction layer that allows different layout algorithms (block, inline, flex, grid, table) to be implemented independently while sharing a common interface.

## Context

In CSS, different `display` values create different **formatting contexts** that have completely different layout rules:

- **Block Formatting Context (BFC)**: Elements stack vertically, margins collapse
- **Inline Formatting Context (IFC)**: Text and inline boxes flow horizontally, wrap at line boundaries
- **Flex Formatting Context (FFC)**: Taffy handles this
- **Grid Formatting Context (GFC)**: Taffy handles this
- **Table Formatting Context (TFC)**: Complex column-based layout

**From CSS 2.1 Section 9.4:**
> "Boxes in the normal flow belong to a formatting context, which may be block or inline, but not both simultaneously."

## The Problem V1 Has

V1 tries to use a single layout engine (Taffy) for everything, forcing table→flex translation. This violates the formatting context model.

## The Solution

Create a trait that all formatting contexts implement:

```rust
/// A formatting context performs layout for a specific display type
pub trait FormattingContext: std::fmt::Debug {
    /// Returns the formatting context type
    fn context_type(&self) -> FormattingContextType;

    /// Performs layout on the given box tree node
    ///
    /// # Arguments
    /// * `box_node` - The box to layout (must have matching display type)
    /// * `constraints` - Available space and parent constraints
    /// * `font_context` - Font metrics for text measurement
    ///
    /// # Returns
    /// Fragment tree representing the laid-out content
    fn layout(
        &self,
        box_node: &BoxNode,
        constraints: &LayoutConstraints,
        font_context: &FontContext,
    ) -> Result<FragmentNode>;

    /// Computes the intrinsic (preferred) size of content
    /// Used for auto-sizing and flex/grid intrinsic sizing
    fn compute_intrinsic_size(
        &self,
        box_node: &BoxNode,
        axis: Axis,
        font_context: &FontContext,
    ) -> Result<f32>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FormattingContextType {
    Block,
    Inline,
    Flex,
    Grid,
    Table,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Axis {
    Horizontal,
    Vertical,
}
```

## Step-by-Step Implementation

### Step 1: Create Module Structure (Day 1 Morning)

```bash
mkdir -p /home/user/fastrender/src/layout/contexts
touch /home/user/fastrender/src/layout/contexts/mod.rs
touch /home/user/fastrender/src/layout/contexts/trait.rs
```

**File: `src/layout/contexts/mod.rs`**
```rust
//! Formatting context implementations
//!
//! This module contains the trait definition and implementations for all CSS
//! formatting contexts. Each formatting context has completely different layout rules.

mod r#trait;

pub use r#trait::{FormattingContext, FormattingContextType, Axis};

// These will be implemented in Phase 2
// pub mod block;
// pub mod inline;
// pub mod flex;
// pub mod grid;
// pub mod table;
```

**File: `src/layout/contexts/trait.rs`**
```rust
//! Formatting context trait definition
//!
//! This trait is the core abstraction that allows different layout algorithms
//! to be plugged into the rendering engine.

use crate::tree::{BoxNode, FragmentNode};
use crate::layout::LayoutConstraints;
use crate::text::FontContext;
use crate::error::Result;

/// A formatting context performs layout for boxes with a specific display type
///
/// CSS Specification: CSS 2.1 Section 9.4
/// https://www.w3.org/TR/CSS21/visuren.html#block-formatting
///
/// Different display types create different formatting contexts:
/// - `display: block` creates a Block Formatting Context (BFC)
/// - `display: inline` creates an Inline Formatting Context (IFC)
/// - `display: flex` creates a Flex Formatting Context (FFC)
/// - `display: grid` creates a Grid Formatting Context (GFC)
/// - `display: table` creates a Table Formatting Context (TFC)
///
/// Each has completely different layout rules.
pub trait FormattingContext: std::fmt::Debug + Send + Sync {
    /// Returns the type of formatting context
    fn context_type(&self) -> FormattingContextType;

    /// Performs layout on the given box tree node
    ///
    /// This is the main entry point for layout. It takes a box tree node and
    /// produces a fragment tree representing the laid-out content.
    ///
    /// # Arguments
    ///
    /// * `box_node` - The box to layout. Must have a display type matching this FC.
    /// * `constraints` - Available space and constraints from parent
    /// * `font_context` - Font metrics provider for text measurement
    ///
    /// # Returns
    ///
    /// A fragment tree rooted at a FragmentNode. The fragment contains:
    /// - Final position (x, y)
    /// - Final size (width, height)
    /// - Child fragments (if any)
    ///
    /// # Errors
    ///
    /// Returns error if:
    /// - Box type doesn't match this formatting context
    /// - Layout constraints are invalid
    /// - Font loading fails
    fn layout(
        &self,
        box_node: &BoxNode,
        constraints: &LayoutConstraints,
        font_context: &FontContext,
    ) -> Result<FragmentNode>;

    /// Computes the intrinsic (preferred) size of content
    ///
    /// Used for:
    /// - Auto-sizing: `width: auto` on a block
    /// - Flex intrinsic sizing: `flex-basis: content`
    /// - Grid intrinsic sizing: `grid-template-columns: min-content`
    ///
    /// # Arguments
    ///
    /// * `box_node` - The box to measure
    /// * `axis` - Which dimension to measure (horizontal or vertical)
    /// * `font_context` - Font metrics for text measurement
    ///
    /// # Returns
    ///
    /// The intrinsic size in CSS pixels (f32)
    fn compute_intrinsic_size(
        &self,
        box_node: &BoxNode,
        axis: Axis,
        font_context: &FontContext,
    ) -> Result<f32>;
}

/// The type of formatting context
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FormattingContextType {
    /// Block Formatting Context - vertical stacking, margin collapse
    Block,
    /// Inline Formatting Context - horizontal flow, line breaking
    Inline,
    /// Flex Formatting Context - flexbox layout (Taffy)
    Flex,
    /// Grid Formatting Context - grid layout (Taffy)
    Grid,
    /// Table Formatting Context - table layout algorithm
    Table,
}

/// Axis for intrinsic size calculation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Axis {
    /// Horizontal (inline) axis - width
    Horizontal,
    /// Vertical (block) axis - height
    Vertical,
}

impl FormattingContextType {
    /// Returns true if this FC is handled by Taffy
    pub fn is_taffy_based(self) -> bool {
        matches!(self, Self::Flex | Self::Grid)
    }

    /// Returns true if this FC is implemented in FastRender
    pub fn is_native(self) -> bool {
        matches!(self, Self::Block | Self::Inline | Self::Table)
    }
}
```

### Step 2: Create Formatting Context Factory (Day 1 Afternoon)

We need a way to select which formatting context to use based on the box's display type.

**File: `src/layout/contexts/factory.rs`**
```rust
//! Formatting context factory
//!
//! Selects the appropriate formatting context based on display type.

use super::{FormattingContext, FormattingContextType};
use crate::style::Display;
use std::sync::Arc;

/// Factory for creating formatting contexts
///
/// This holds instances of all formatting contexts and returns the correct one
/// based on the display type.
pub struct FormattingContextFactory {
    // Phase 2 will implement these
    // block_fc: Arc<dyn FormattingContext>,
    // inline_fc: Arc<dyn FormattingContext>,
    // flex_fc: Arc<dyn FormattingContext>,
    // grid_fc: Arc<dyn FormattingContext>,
    // table_fc: Arc<dyn FormattingContext>,
}

impl FormattingContextFactory {
    /// Creates a new factory with all formatting contexts initialized
    pub fn new() -> Self {
        Self {
            // Phase 2 implementation
        }
    }

    /// Returns the formatting context for the given display type
    ///
    /// # Arguments
    ///
    /// * `display` - The CSS display value
    ///
    /// # Returns
    ///
    /// The appropriate formatting context implementation
    pub fn get_context(&self, display: Display) -> Arc<dyn FormattingContext> {
        match display {
            Display::Block => {
                // Phase 2: return self.block_fc.clone()
                todo!("Block FC not yet implemented")
            }
            Display::Inline => {
                // Phase 2: return self.inline_fc.clone()
                todo!("Inline FC not yet implemented")
            }
            Display::Flex => {
                // Phase 2: return self.flex_fc.clone()
                todo!("Flex FC not yet implemented")
            }
            Display::Grid => {
                // Phase 2: return self.grid_fc.clone()
                todo!("Grid FC not yet implemented")
            }
            Display::Table | Display::TableRow | Display::TableCell => {
                // Phase 2: return self.table_fc.clone()
                todo!("Table FC not yet implemented")
            }
            _ => {
                // Fallback to block for unknown display types
                todo!("Unknown display type")
            }
        }
    }
}

impl Default for FormattingContextFactory {
    fn default() -> Self {
        Self::new()
    }
}
```

### Step 3: Integration with Layout Pipeline (Day 2)

Update `src/layout/mod.rs` to use the formatting context abstraction:

```rust
//! Layout module
//!
//! This module coordinates the layout process using formatting contexts.

pub mod contexts;
pub mod constraints;

use crate::tree::{BoxNode, FragmentNode};
use crate::text::FontContext;
use crate::error::Result;
use contexts::FormattingContextFactory;

pub use constraints::LayoutConstraints;

/// Main layout engine
///
/// This orchestrates the layout process by delegating to appropriate
/// formatting contexts based on display type.
pub struct LayoutEngine {
    /// Factory for getting formatting contexts
    fc_factory: FormattingContextFactory,

    /// Font context for text measurement
    font_context: FontContext,
}

impl LayoutEngine {
    /// Creates a new layout engine
    pub fn new(font_context: FontContext) -> Self {
        Self {
            fc_factory: FormattingContextFactory::new(),
            font_context,
        }
    }

    /// Performs layout on a box tree
    ///
    /// # Arguments
    ///
    /// * `root` - Root box node to layout
    /// * `viewport_width` - Viewport width in CSS pixels
    /// * `viewport_height` - Viewport height in CSS pixels
    ///
    /// # Returns
    ///
    /// Fragment tree with final positions and sizes
    pub fn layout(
        &self,
        root: &BoxNode,
        viewport_width: f32,
        viewport_height: f32,
    ) -> Result<FragmentNode> {
        // Create root constraints (containing block)
        let constraints = LayoutConstraints {
            available_width: Some(viewport_width),
            available_height: Some(viewport_height),
            percentage_base_width: viewport_width,
            percentage_base_height: viewport_height,
        };

        // Get the appropriate formatting context
        let fc = self.fc_factory.get_context(root.style.display);

        // Perform layout
        fc.layout(root, &constraints, &self.font_context)
    }
}
```

### Step 4: Define LayoutConstraints (Day 2)

**File: `src/layout/constraints.rs`**
```rust
//! Layout constraints
//!
//! Defines the available space and constraints passed from parent to child during layout.

/// Constraints for layout
///
/// Represents the containing block and available space for layout.
///
/// CSS Specification: CSS 2.1 Section 10.1 - Containing Block
/// https://www.w3.org/TR/CSS21/visudet.html#containing-block-details
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct LayoutConstraints {
    /// Available width for content (None = infinite)
    ///
    /// For `width: auto`, this is the maximum width available.
    pub available_width: Option<f32>,

    /// Available height for content (None = infinite)
    ///
    /// For `height: auto`, this is the maximum height available.
    pub available_height: Option<f32>,

    /// Base for percentage width calculations
    ///
    /// When a child has `width: 50%`, it's 50% of this value.
    pub percentage_base_width: f32,

    /// Base for percentage height calculations
    ///
    /// When a child has `height: 50%`, it's 50% of this value.
    pub percentage_base_height: f32,
}

impl LayoutConstraints {
    /// Creates constraints from a containing block size
    pub fn from_containing_block(width: f32, height: f32) -> Self {
        Self {
            available_width: Some(width),
            available_height: Some(height),
            percentage_base_width: width,
            percentage_base_height: height,
        }
    }

    /// Creates unconstrained layout (infinite available space)
    pub fn unconstrained() -> Self {
        Self {
            available_width: None,
            available_height: None,
            percentage_base_width: 0.0,
            percentage_base_height: 0.0,
        }
    }

    /// Returns a copy with new available width
    pub fn with_available_width(mut self, width: Option<f32>) -> Self {
        self.available_width = width;
        self
    }

    /// Returns a copy with new available height
    pub fn with_available_height(mut self, height: Option<f32>) -> Self {
        self.available_height = height;
        self
    }
}
```

### Step 5: Write Tests (Day 3)

**File: `tests/layout/formatting_contexts_test.rs`**
```rust
//! Tests for formatting context abstraction

use fastrender::layout::contexts::{FormattingContext, FormattingContextType, Axis};
use fastrender::layout::LayoutConstraints;
use fastrender::tree::BoxNode;
use fastrender::text::FontContext;
use fastrender::error::Result;

/// Mock formatting context for testing
#[derive(Debug)]
struct MockFormattingContext {
    context_type: FormattingContextType,
}

impl FormattingContext for MockFormattingContext {
    fn context_type(&self) -> FormattingContextType {
        self.context_type
    }

    fn layout(
        &self,
        box_node: &BoxNode,
        _constraints: &LayoutConstraints,
        _font_context: &FontContext,
    ) -> Result<FragmentNode> {
        // Return a simple fragment for testing
        Ok(FragmentNode::new(
            Rect::new(Point::zero(), Size::new(100.0, 100.0)),
            box_node.style.clone(),
            vec![],
        ))
    }

    fn compute_intrinsic_size(
        &self,
        _box_node: &BoxNode,
        _axis: Axis,
        _font_context: &FontContext,
    ) -> Result<f32> {
        Ok(100.0)
    }
}

#[test]
fn test_formatting_context_trait_compiles() {
    let mock = MockFormattingContext {
        context_type: FormattingContextType::Block,
    };

    assert_eq!(mock.context_type(), FormattingContextType::Block);
}

#[test]
fn test_formatting_context_type_is_taffy_based() {
    assert!(FormattingContextType::Flex.is_taffy_based());
    assert!(FormattingContextType::Grid.is_taffy_based());
    assert!(!FormattingContextType::Block.is_taffy_based());
    assert!(!FormattingContextType::Inline.is_taffy_based());
    assert!(!FormattingContextType::Table.is_taffy_based());
}

#[test]
fn test_formatting_context_type_is_native() {
    assert!(FormattingContextType::Block.is_native());
    assert!(FormattingContextType::Inline.is_native());
    assert!(FormattingContextType::Table.is_native());
    assert!(!FormattingContextType::Flex.is_native());
    assert!(!FormattingContextType::Grid.is_native());
}

#[test]
fn test_layout_constraints_from_containing_block() {
    let constraints = LayoutConstraints::from_containing_block(800.0, 600.0);

    assert_eq!(constraints.available_width, Some(800.0));
    assert_eq!(constraints.available_height, Some(600.0));
    assert_eq!(constraints.percentage_base_width, 800.0);
    assert_eq!(constraints.percentage_base_height, 600.0);
}

#[test]
fn test_layout_constraints_unconstrained() {
    let constraints = LayoutConstraints::unconstrained();

    assert_eq!(constraints.available_width, None);
    assert_eq!(constraints.available_height, None);
}

#[test]
fn test_layout_constraints_with_available_width() {
    let constraints = LayoutConstraints::from_containing_block(800.0, 600.0)
        .with_available_width(Some(400.0));

    assert_eq!(constraints.available_width, Some(400.0));
    assert_eq!(constraints.percentage_base_width, 800.0); // Unchanged
}
```

## Acceptance Criteria

- [ ] `FormattingContext` trait compiles and has all required methods
- [ ] `FormattingContextType` enum has all 5 types (Block, Inline, Flex, Grid, Table)
- [ ] `FormattingContextFactory` can be instantiated (even with `todo!()` implementations)
- [ ] `LayoutConstraints` struct exists with all fields
- [ ] `LayoutEngine` can be created and call `layout()` (will panic in Phase 1, that's OK)
- [ ] All tests pass: `cargo test formatting_contexts`
- [ ] Documentation is complete with spec references
- [ ] Code follows 10-code-standards.md

## Common Pitfalls

### Pitfall 1: Trying to Share Code Between FCs

**Wrong:**
```rust
// Shared helper that works for "most" formatting contexts
fn layout_children(children: &[BoxNode]) -> Vec<FragmentNode> {
    // This won't work - block stacks vertically, inline flows horizontally
}
```

**Right:**
```rust
// Each FC implements its own child layout
impl FormattingContext for BlockFormattingContext {
    fn layout(&self, ...) -> Result<FragmentNode> {
        // Block-specific child layout (vertical stacking)
    }
}

impl FormattingContext for InlineFormattingContext {
    fn layout(&self, ...) -> Result<FragmentNode> {
        // Inline-specific child layout (horizontal flow)
    }
}
```

**Why:** Each formatting context has completely different rules. Shared code leads to special cases and hacks.

### Pitfall 2: Storing Mutable State in FC Implementations

**Wrong:**
```rust
struct BlockFormattingContext {
    current_y_position: f32, // WRONG - mutable state
}
```

**Right:**
```rust
struct BlockFormattingContext {
    // No mutable state - the FC is stateless
}

impl FormattingContext for BlockFormattingContext {
    fn layout(&self, ...) -> Result<FragmentNode> {
        // Local variables for layout state
        let mut current_y = 0.0;
        // ...
    }
}
```

**Why:** Formatting contexts should be stateless and reusable. Layout state lives in local variables, not in the FC struct.

### Pitfall 3: Not Handling Intrinsic Sizing

**Wrong:**
```rust
fn compute_intrinsic_size(...) -> Result<f32> {
    Ok(0.0) // Just return 0
}
```

**Right:**
```rust
fn compute_intrinsic_size(
    &self,
    box_node: &BoxNode,
    axis: Axis,
    font_context: &FontContext,
) -> Result<f32> {
    // Actually measure content
    match axis {
        Axis::Horizontal => {
            // Measure width of content (e.g., text width, image width)
        }
        Axis::Vertical => {
            // Measure height of content
        }
    }
}
```

**Why:** Intrinsic sizing is critical for `width: auto`, flexbox `flex-basis: content`, etc. Getting this wrong breaks layout.

## Next Steps

After this phase is complete:

1. **Phase 2 begins** - Implement actual formatting contexts
   - Start with `02-block-layout.md`
   - Then `02-inline-layout.md`
   - Then `02-table-layout.md` (most critical)
   - Taffy wrappers for flex/grid

2. **Testing** - Write comprehensive tests for the abstraction
   - Test that FCs can be swapped
   - Test constraints flow correctly
   - Test intrinsic sizing

3. **Documentation** - Update architecture docs
   - Add diagrams showing FC selection
   - Document when each FC is used

## References

- **CSS 2.1 Section 9.4:** Formatting Contexts
  - https://www.w3.org/TR/CSS21/visuren.html#block-formatting
- **Servo's FormattingContext trait:**
  - `components/layout_2020/formatting_contexts.rs`
- **WebKit's LayoutFormattingContext:**
  - `Source/WebCore/layout/FormattingContext.h`

## Timeline

- **Day 1:** Create trait definition and factory structure
- **Day 2:** Integrate with layout pipeline, define constraints
- **Day 3:** Write tests and documentation
- **Day 4-5:** Code review, polish, ensure all tests pass

**Total: 5-7 days**

---

**Last Updated:** 2025-01-19
**Status:** Ready for Implementation
**Blocking:** Phase 2 implementations
