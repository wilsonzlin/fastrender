//! Fragment tree types
//!
//! The fragment tree represents boxes after layout, with final positions and sizes.
//! Fragments are the output of layout algorithms and the input to painting.
//!
//! **Note**: This is a minimal stub implementation to support W2.T07 (FormattingContext trait).
//! The full implementation will be provided by W2.T03.

use crate::geometry::{Point, Rect, Size};
use crate::tree::box_tree::ComputedStyle;
use std::sync::Arc;

/// A positioned, sized fragment ready for painting
///
/// Represents the result of layout: a box with concrete position and size.
///
/// **Note**: This is a minimal stub implementation to support W2.T07.
/// The full implementation will be provided by W2.T03.
#[derive(Debug, Clone)]
pub struct Fragment {
    /// Bounding rectangle (position and size)
    pub bounds: Rect,

    /// Computed style (shared with box tree)
    pub style: Arc<ComputedStyle>,

    /// Child fragments
    pub children: Vec<Fragment>,
}

impl Fragment {
    /// Creates a new block fragment (stub implementation)
    pub fn new_block(bounds: Rect, style: Arc<ComputedStyle>, children: Vec<Fragment>) -> Self {
        Self {
            bounds,
            style,
            children,
        }
    }

    /// Returns the size of this fragment
    pub fn size(&self) -> Size {
        self.bounds.size
    }

    /// Returns the position of this fragment
    pub fn position(&self) -> Point {
        self.bounds.origin
    }
}
