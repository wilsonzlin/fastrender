//! Layout algorithms
//!
//! This module contains all CSS layout algorithms that transform the box tree
//! into a positioned fragment tree.
//!
//! # Supported Layout Modes
//!
//! FastRender supports all major CSS layout modes:
//!
//! - **Block Layout**: Vertical stacking of block-level boxes
//! - **Inline Layout**: Horizontal flow of inline-level boxes with line breaking
//! - **Flex Layout**: Flexbox container layout (delegated to Taffy)
//! - **Grid Layout**: Grid container layout (delegated to Taffy)
//! - **Table Layout**: Table, row, and cell layout
//! - **Positioned Layout**: Absolute and fixed positioning
//!
//! # Architecture
//!
//! Layout is performed in a top-down, recursive tree walk:
//!
//! 1. **Box Generation**: Create anonymous boxes as needed
//! 2. **Size Computation**: Calculate intrinsic and definite sizes
//! 3. **Positioning**: Determine fragment positions
//! 4. **Fragment Creation**: Generate positioned fragments
//!
//! # Module Organization
//!
//! This module will contain:
//! - `block.rs` - Block layout algorithm
//! - `inline.rs` - Inline layout and line breaking
//! - `flex.rs` - Flexbox integration with Taffy
//! - `grid.rs` - Grid integration with Taffy
//! - `table.rs` - Table layout algorithm
//! - `positioned.rs` - Absolute/fixed positioning
//! - `context.rs` - Layout context shared across algorithms
//!
//! # Example
//!
//! ```rust,ignore
//! use fastrender::layout::LayoutContext;
//! use fastrender::geometry::Size;
//!
//! let viewport = Size::new(1024.0, 768.0);
//! let mut ctx = LayoutContext::new(viewport);
//!
//! // Run layout on box tree root
//! let fragment_tree = ctx.layout(&box_tree_root);
//! ```

// Module declarations will be added in Wave 2+
// pub mod block;
// pub mod inline;
// pub mod flex;
// pub mod grid;
// pub mod table;
// pub mod positioned;
// pub mod context;

// Temporary re-export of V1 implementation for compatibility
// This will be removed as Wave 2+ tasks are completed
#[path = "../layout_v1.rs"]
mod layout_v1;
pub use layout_v1::*;
