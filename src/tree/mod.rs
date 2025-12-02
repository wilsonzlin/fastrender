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
pub mod anonymous;
pub mod box_generation;
pub mod box_tree;
pub mod debug;
pub mod fragment_tree;
pub mod pseudo_elements;
pub mod table_fixup;
