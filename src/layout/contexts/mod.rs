//! Formatting context factory and implementations
//!
//! This module contains:
//! - FormattingContext factory (creates appropriate FC)
//! - Block Formatting Context implementation (W3.T04, W3.T05)
//! - Table layout module (column distribution, etc.)

pub mod block;
pub mod factory;
pub mod table;

// Re-exports
pub use block::BlockFormattingContext;
pub use factory::FormattingContextFactory;
pub use table::{ColumnConstraints, ColumnDistributor, ColumnWidthDistributionResult, DistributionMode};
