//! Tree structures for boxes and fragments
//!
//! This module contains the tree structures used in the rendering pipeline:
//! - **Box Tree**: CSS boxes before layout (input to layout)
//! - **Fragment Tree**: Positioned, sized boxes after layout (output of layout)
//!
//! # Architecture
//!
//! ```text
//! DOM + Styles → Box Tree → Layout → Fragment Tree → Paint
//! ```
//!
//! The box tree represents what to layout (CSS boxes with display, dimensions).
//! The fragment tree represents where things ended up (positioned rectangles).
//!
//! # Fragment Splitting
//!
//! One box can generate multiple fragments:
//! - Inline boxes split across lines
//! - Blocks split across columns or pages

pub mod box_tree;
pub mod debug;
pub mod fragment_tree;

// Re-exports
pub use box_tree::{AnonymousType, BoxNode, BoxTree, BoxType, FormattingContextType, ReplacedType};
pub use debug::{DebugInfo, TreePrinter};
pub use fragment_tree::{FragmentContent, FragmentIterator, FragmentNode, FragmentTree};
