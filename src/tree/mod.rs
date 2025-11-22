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

// Module declarations
pub mod box_generation;
pub mod box_tree;
pub mod debug;
pub mod fragment_tree;
pub mod table_fixup;

// Re-exports from box_tree
pub use box_tree::{AnonymousType, BoxNode, BoxTree, BoxType, ReplacedType};

// Re-export FormattingContextType from style (single source of truth)
pub use crate::style::display::FormattingContextType;

// Re-exports from debug
pub use debug::{DebugInfo, TreePrinter};

// Re-exports from fragment_tree
pub use fragment_tree::{FragmentContent, FragmentIterator, FragmentNode, FragmentTree};

// Re-exports from box_generation
pub use box_generation::{BoxGenerationConfig, BoxGenerationError, BoxGenerator, DOMNode};

// Re-exports from table_fixup
pub use table_fixup::TableStructureFixer;
