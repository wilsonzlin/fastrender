//! Inline Layout Module
//!
//! This module contains implementations for inline formatting context (IFC)
//! layout and related functionality.
//!
//! # Overview
//!
//! Inline layout handles the horizontal flow of inline-level content including:
//! - Text runs
//! - Inline boxes (`<span>`, etc.)
//! - Inline-block elements
//! - Line breaking and wrapping
//! - Baseline alignment
//!
//! # Float Integration
//!
//! The `float_integration` submodule provides the integration layer between
//! inline layout and float positioning. When inline content is laid out in a
//! block formatting context containing floats, lines must wrap around those
//! floats. See CSS 2.1 Section 9.5 for float specification.
//!
//! # CSS Specification References
//!
//! - CSS 2.1 Section 9.4.2: Inline formatting contexts
//! - CSS 2.1 Section 10.8: Line height calculations
//! - CSS Inline Layout Module Level 3: https://www.w3.org/TR/css-inline-3/
//!
//! # Example
//!
//! ```rust,ignore
//! use fastrender::layout::FloatContext;
//! use fastrender::layout::inline::{InlineFloatIntegration, LineSpace};
//!
//! // During inline layout, use InlineFloatIntegration to query available space
//! let float_ctx = FloatContext::new(800.0);
//! let integration = InlineFloatIntegration::new(&float_ctx);
//!
//! // Find space for a line at y=0
//! let space = integration.get_line_space(0.0);
//! println!("Line can start at x={} with width {}", space.left_edge, space.width);
//! ```

pub mod float_integration;

// Re-export main types for convenience
pub use float_integration::{
    line_spaces, InlineFloatIntegration, InlineFloatIntegrationMut, LineSpace, LineSpaceIterator, LineSpaceOptions,
    PlacedInlineFloat,
};
