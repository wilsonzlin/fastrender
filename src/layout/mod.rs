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
//! use fastrender::{LayoutEngine, LayoutConfig};
//! use fastrender::tree::BoxTree;
//! use fastrender::Size;
//!
//! let config = LayoutConfig::for_viewport(Size::new(1024.0, 768.0));
//! let engine = LayoutEngine::new(config);
//! let fragment_tree = engine.layout_tree(&box_tree)?;
//! ```

// Shared layout utilities
pub mod utils;

// W2.T09 - Formatting context factory
pub mod contexts;

// Optional layout profiling
pub mod flex_profile;
pub mod profile;

// Debug-only guardrails for Taffy usage
pub mod taffy_integration;

// W2.T04 - Layout constraints
pub mod constraints;

// W2.T07 - FormattingContext trait
pub mod formatting_context;

// W2.T10 - Layout engine orchestrator
pub mod engine;

// W3.T10 - Float context for BFC float management
pub mod float_context;

// W4 - Inline layout module (W4.T13 Baseline, W4.T14 Text Run, W4.T15 Float Integration)
pub mod inline;

// Pagination helpers for @page and multi-page outputs
pub mod pagination;

// W3.T13 - Absolute positioning algorithm
pub mod absolute_positioning;

// W3.T06 - Table layout algorithm
pub mod table;

// Fragmentation / pagination helpers
pub mod fragmentation;

// Future modules (to be implemented in Wave 3+):
// pub mod block;        // W3.T04
// pub mod flex;         // W3.T08
// pub mod grid;         // W3.T09
// pub mod positioned;   // W3.T12
