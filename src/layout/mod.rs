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
//! - `contexts/` - Formatting context types and derivation (W2.T08)
//! - `constraints.rs` - LayoutConstraints and AvailableSpace (W2.T04)
//! - `formatting_context.rs` - FormattingContext trait (W2.T07)
//! - `block.rs` - Block layout algorithm (W3.T04)
//! - `inline.rs` - Inline layout and line breaking (W4.T12)
//! - `flex.rs` - Flexbox integration with Taffy (W3.T08)
//! - `grid.rs` - Grid integration with Taffy (W3.T09)
//! - `table.rs` - Table layout algorithm (W3.T06)
//!
//! # Example
//!
//! ```rust,ignore
//! use fastrender::layout::{FormattingContext, LayoutConstraints};
//! use fastrender::tree::BoxNode;
//!
//! // Get appropriate formatting context for box
//! let fc = get_formatting_context(&box_node);
//!
//! // Create constraints (viewport size)
//! let constraints = LayoutConstraints::definite(1024.0, 768.0);
//!
//! // Perform layout
//! let fragment = fc.layout(&box_node, &constraints)?;
//! ```

// W2.T08 - Formatting context types and derivation
pub mod contexts;

// W2.T04 - Layout constraints
pub mod constraints;

// W2.T07 - FormattingContext trait
pub mod formatting_context;

// Re-exports
pub use constraints::{AvailableSpace, LayoutConstraints};
pub use contexts::FormattingContextType;
pub use formatting_context::{FormattingContext, IntrinsicSizingMode, LayoutError};

// Future modules (to be implemented in Wave 3+):
// pub mod block;        // W3.T04
// pub mod inline;       // W4.T12
// pub mod flex;         // W3.T08
// pub mod grid;         // W3.T09
// pub mod table;        // W3.T06
// pub mod positioned;   // W3.T12

// Temporary re-export of V1 implementation for compatibility
// This will be removed as Wave 2+ tasks are completed
#[path = "../layout_v1.rs"]
mod layout_v1;
pub use layout_v1::*;
