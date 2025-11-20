//! CSS style computation and cascade
//!
//! This module handles CSS parsing, selector matching, cascade resolution,
//! and computed style generation.
//!
//! # Responsibilities
//!
//! - **Parsing**: Parse CSS stylesheets into rule trees
//! - **Matching**: Match CSS selectors against DOM nodes
//! - **Cascade**: Resolve specificity, importance, and origin
//! - **Inheritance**: Propagate inherited properties
//! - **Computation**: Convert specified values to computed values
//!
//! # Architecture
//!
//! Style computation happens in phases:
//!
//! 1. **Parse**: CSS text → `Stylesheet` with `CssRule`s
//! 2. **Match**: For each DOM node, find matching rules
//! 3. **Cascade**: Resolve conflicts, apply specificity
//! 4. **Compute**: Convert relative/percentage values to absolute
//! 5. **Inherit**: Propagate inherited properties to children
//!
//! # Module Organization
//!
//! This module will contain:
//! - `stylesheet.rs` - Stylesheet and rule structures
//! - `selector.rs` - Selector matching logic
//! - `cascade.rs` - Cascade resolution
//! - `computed.rs` - ComputedStyle type
//! - `properties/` - Individual CSS property definitions
//!   - `display.rs`, `position.rs`, `color.rs`, etc.
//!
//! # Example
//!
//! ```rust,ignore
//! use fastrender::style::{Stylesheet, ComputedStyle};
//!
//! // Parse CSS
//! let stylesheet = Stylesheet::parse("div { color: red; }");
//!
//! // Compute styles for a DOM node
//! let computed = ComputedStyle::compute(&dom_node, &stylesheet);
//!
//! assert_eq!(computed.color(), Color::rgb(255, 0, 0));
//! ```

// Module declarations will be added in Wave 2+
// pub mod stylesheet;
// pub mod selector;
// pub mod cascade;
// pub mod computed;
// pub mod properties;

// Temporary re-export of V1 implementation for compatibility
// This will be removed as Wave 2+ tasks are completed
#[path = "../style_v1.rs"]
mod style_v1;
pub use style_v1::*;
