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
pub mod grid;
pub mod inline;
pub mod positioned;
pub mod table;

// Re-exports
pub use block::BlockFormattingContext;
pub use factory::FormattingContextFactory;
pub use flex::FlexFormattingContext;
pub use grid::GridFormattingContext;
pub use inline::InlineFormattingContext;
pub use positioned::{ContainingBlock, PositionedLayout, StickyConstraints};
pub use table::{ColumnConstraints, ColumnDistributor, ColumnWidthDistributionResult, DistributionMode};
