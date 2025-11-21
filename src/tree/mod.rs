//! Tree structures for boxes and fragments
//!
//! This module contains the tree structures used in the rendering pipeline:
//! - Box Tree: CSS boxes before layout
//! - Fragment Tree: Laid-out boxes with positions (defined elsewhere)

pub mod box_tree;

// Re-exports
pub use box_tree::{BoxNode, BoxTree, BoxType, FormattingContextType};
