//! Tree structures for boxes and fragments
//!
//! This module contains the tree structures used in the rendering pipeline:
//! - Box Tree: CSS boxes before layout
//! - Fragment Tree: Laid-out boxes with positions

pub mod box_tree;
pub mod fragment_tree;

// Re-exports
pub use box_tree::{
    AnonymousBox, AnonymousType, BlockBox, BoxNode, BoxTree, BoxType, DebugInfo, FormattingContextType, InlineBox,
    ReplacedBox, ReplacedType, TextBox,
};

pub use fragment_tree::{Fragment, FragmentContent, FragmentIterator, FragmentNode, FragmentTree};
