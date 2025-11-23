//! Inline layout module
//!
//! This module implements inline formatting context (IFC) components
//! for horizontal text flow, line breaking, and baseline alignment.
//!
//! # CSS Specification
//!
//! Implements CSS 2.1 Section 9.4.2 - Inline Formatting Contexts:
//! <https://www.w3.org/TR/CSS21/visuren.html#inline-formatting>
//!
//! And CSS Inline Layout Module Level 3:
//! <https://www.w3.org/TR/css-inline-3/>
//!
//! # Module Organization
//!
//! - `baseline` - Baseline alignment algorithm (vertical-align property)

pub mod baseline;

// Re-export primary types
pub use baseline::{BaselineAligner, InlineBoxMetrics, LineMetrics, PositionedInlineBox, VerticalAlign};
