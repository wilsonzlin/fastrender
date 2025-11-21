//! Box tree types
//!
//! The box tree represents CSS boxes before layout. It's generated from the DOM
//! tree plus computed styles, and serves as input to layout algorithms.
//!
//! **Note**: This is a minimal stub implementation to support W2.T07 (FormattingContext trait).
//! The full implementation will be provided by W2.T01.

use crate::geometry::{Point, Rect, Size};
use std::sync::Arc;

/// Formatting context type for a box
///
/// Determines which layout algorithm will be used for this box's children.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FormattingContextType {
    /// Block formatting context - vertical stacking
    Block,
    /// Inline formatting context - horizontal flow with line breaking
    Inline,
    /// Flex formatting context - flexbox layout
    Flex,
    /// Grid formatting context - grid layout
    Grid,
    /// Table formatting context - table layout
    Table,
}

/// Computed style values for a box
///
/// **Note**: This is a minimal stub. Full implementation in W2.T02.
#[derive(Debug, Clone, Default)]
pub struct ComputedStyle {
    // Stub - will be filled in by W2.T02
}

/// A box in the box tree
///
/// Represents a CSS box before layout. Contains computed styles and child boxes.
///
/// **Note**: This is a minimal stub implementation to support W2.T07.
/// The full implementation will be provided by W2.T01.
#[derive(Debug, Clone)]
pub struct BoxNode {
    /// Computed style values
    pub style: Arc<ComputedStyle>,

    /// Formatting context type for this box
    pub formatting_context: FormattingContextType,

    /// Child boxes
    pub children: Vec<BoxNode>,
}

impl BoxNode {
    /// Creates a new block box (stub implementation)
    pub fn new_block(
        style: Arc<ComputedStyle>,
        formatting_context: FormattingContextType,
        children: Vec<BoxNode>,
    ) -> Self {
        Self {
            style,
            formatting_context,
            children,
        }
    }
}
