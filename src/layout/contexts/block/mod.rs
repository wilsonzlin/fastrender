//! Block Formatting Context implementation
//!
//! This module contains the Block Formatting Context (BFC) implementation,
//! which is the fundamental layout algorithm for vertically stacked block boxes.
//!
//! # CSS Specification Reference
//!
//! CSS 2.1 Section 9.4.1: Block formatting contexts
//! https://www.w3.org/TR/CSS21/visuren.html#block-formatting
//!
//! # Module Contents
//!
//! - `margin_collapse`: Margin collapsing algorithm (CSS 2.1 Section 8.3.1)
//! - Future: `block.rs` for full BFC implementation (W3.T04)
//!
//! # Block Layout Overview
//!
//! In a block formatting context:
//!
//! 1. Boxes stack vertically from top to bottom
//! 2. Adjacent margins collapse (see `margin_collapse` module)
//! 3. Each box's width fills the containing block (unless specified)
//! 4. Height is determined by content (unless specified)
//!
//! # Example
//!
//! ```ignore
//! use fastrender::layout::contexts::block::margin_collapse::CollapsibleMargin;
//!
//! // Collapse two margins
//! let m1 = CollapsibleMargin::from_margin(20.0);
//! let m2 = CollapsibleMargin::from_margin(30.0);
//! let collapsed = m1.collapse_with(m2);
//! assert_eq!(collapsed.resolve(), 30.0); // Takes the max
//! ```

pub mod margin_collapse;

// Re-export commonly used types
pub use margin_collapse::{CollapsibleMargin, Float, MarginCollapseContext, Overflow, ParentChildCollapseRules};
