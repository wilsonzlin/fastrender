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
//! Layout is performed through the FormattingContext abstraction:
//!
//! 1. **Formatting Context Selection**: Choose appropriate FC based on display type
//! 2. **Constraint Propagation**: Pass available space down the tree
//! 3. **Recursive Layout**: Each FC lays out its children using child FCs
//! 4. **Fragment Tree Construction**: Build positioned fragment tree bottom-up
//!
//! # Module Organization
//!
//! - `contexts/` - Formatting context factory (W2.T09)
//! - `constraints.rs` - LayoutConstraints and AvailableSpace (W2.T04)
//! - `formatting_context.rs` - FormattingContext trait (W2.T07)
//! - `engine.rs` - LayoutEngine orchestrator (W2.T10)
//! - `float_context.rs` - Float context and placement algorithm (W3.T10/W3.T11)
//! - `block.rs` - Block layout algorithm (W3.T04)
//! - `inline.rs` - Inline layout and line breaking (W4.T12)
//! - `flex.rs` - Flexbox integration with Taffy (W3.T08)
//! - `grid.rs` - Grid integration with Taffy (W3.T09)
//! - `table.rs` - Table layout algorithm (W3.T06)
//!
//! # Example
//!
//! ```rust,ignore
//! use fastrender::layout::{LayoutEngine, LayoutConfig};
//! use fastrender::tree::BoxTree;
//! use fastrender::geometry::Size;
//!
//! let config = LayoutConfig::for_viewport(Size::new(1024.0, 768.0));
//! let engine = LayoutEngine::new(config);
//! let fragment_tree = engine.layout_tree(&box_tree)?;
//! ```

// Shared layout utilities
pub mod utils;

// W2.T09 - Formatting context factory
pub mod contexts;

// W2.T04 - Layout constraints
pub mod constraints;

// W2.T07 - FormattingContext trait
pub mod formatting_context;

// W2.T10 - Layout engine orchestrator
pub mod engine;

// W3.T10 - Float context for BFC float management
pub mod float_context;

// W3.T13 - Absolute positioning algorithm
pub mod absolute_positioning;

// W4.T13 - Inline layout baseline alignment
pub mod inline;

// Re-exports
pub use absolute_positioning::{AbsoluteLayout, AbsoluteLayoutInput, AbsoluteLayoutResult, ResolvedMargins};
pub use constraints::{AvailableSpace, LayoutConstraints};
pub use contexts::{
    ContainingBlock, FormattingContextFactory, GridFormattingContext, PositionedLayout, StickyConstraints,
};
pub use engine::{LayoutConfig, LayoutEngine, LayoutStats};
pub use float_context::{FloatContext, FloatInfo, FloatSide};
pub use formatting_context::{FormattingContext, IntrinsicSizingMode, LayoutError};
pub use inline::{BaselineAligner, InlineBoxMetrics, LineMetrics, PositionedInlineBox, VerticalAlign};

// W3.T06 - Table layout algorithm
pub mod table;

// Future modules (to be implemented in Wave 3+):
// pub mod block;        // W3.T04
// pub mod inline;       // W4.T12
// pub mod flex;         // W3.T08
// pub mod grid;         // W3.T09
// pub mod positioned;   // W3.T12

// Temporary re-export of V1 implementation for compatibility
// This will be removed as Wave 2+ tasks are completed
#[path = "../layout_v1.rs"]
mod layout_v1;
pub use layout_v1::*;
