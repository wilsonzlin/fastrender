//! Formatting context implementations
//!
//! This module contains:
//! - FormattingContext factory (creates appropriate FC)
//! - BlockFormattingContext (W3.T04, W3.T05)
//! - InlineFormattingContext (W4.T12)
//! - FlexFormattingContext (Taffy-backed flexbox layout)
//! - GridFormattingContext (Taffy-backed grid layout)
//! - Table layout module (column distribution, etc.)
//! - PositionedLayout for CSS positioned elements (relative, absolute, fixed, sticky)

pub mod block;
pub mod factory;
pub mod flex;
pub mod flex_cache;
pub mod grid;
pub mod inline;
pub mod node_ref;
pub mod positioned;
pub mod table;
