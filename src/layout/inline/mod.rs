//! Inline layout module
//!
//! This module implements inline formatting context (IFC) components
//! for horizontal text flow, line breaking, and baseline alignment.
//!
//! # Overview
//!
//! Inline layout handles the horizontal flow of inline-level content including:
//! - Text runs
//! - Inline boxes (`<span>`, etc.)
//! - Inline-block elements
//! - Line breaking and wrapping
//! - Baseline alignment
//! - Float integration
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
//! - `float_integration` - Float integration for inline layout (W4.T15)

// W4.T13 - Baseline alignment algorithm
pub mod baseline;

// W4.T15 - Float integration
pub mod float_integration;
