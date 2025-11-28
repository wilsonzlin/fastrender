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
//! - `baseline` - Baseline alignment algorithm (W4.T13)
//! - `text_run` - Text run generation from shaped glyphs (W4.T14)

// W4.T13 - Baseline alignment algorithm
pub mod baseline;

// W4.T14 - Text run generation
pub mod text_run;

// Re-export primary types
pub use baseline::{BaselineAligner, InlineBoxMetrics, LineMetrics, PositionedInlineBox, VerticalAlign};
pub use text_run::{GlyphInfo, InlineItem, TextRun, TextRunBuilder};
