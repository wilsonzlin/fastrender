//! CSS parsing and types
//!
//! This module handles parsing CSS stylesheets and provides types
//! for representing CSS rules, selectors, and values.

pub mod parser;
pub mod properties;
pub mod selectors;
pub mod types;

// Re-exports for convenience
pub use parser::{extract_css, parse_declarations, parse_stylesheet};
pub use properties::{parse_length, parse_property_value};
pub use selectors::{FastRenderSelectorImpl, PseudoClass, PseudoElement};
pub use types::{
    BoxShadow, ColorStop, CssString, Declaration, PropertyValue, StyleRule, StyleSheet,
    TextShadow, Transform,
};

// Also re-export selectors crate types we use
pub use ::selectors::matching::matches_selector;
pub use ::selectors::parser::SelectorImpl;
