//! Formatting context implementations
//!
//! This module contains:
//! - FormattingContext factory (creates appropriate FC)
//! - BlockFormattingContext (W3.T04, W3.T05)
//! - FlexFormattingContext (Taffy-backed flexbox layout)
//! - Table layout module (column distribution, etc.)

pub mod block;
pub mod factory;
pub mod flex;
pub mod table;

// Re-exports
pub use block::BlockFormattingContext;
pub use factory::FormattingContextFactory;
pub use flex::FlexFormattingContext;
pub use table::{ColumnConstraints, ColumnDistributor, ColumnWidthDistributionResult, DistributionMode};
