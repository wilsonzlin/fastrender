//! Formatting context factory and implementations
//!
//! This module contains:
//! - FormattingContext factory (creates appropriate FC)
//! - Block Formatting Context implementation (W3.T04)

pub mod block;
pub mod factory;

// Re-exports
pub use block::BlockFormattingContext;
pub use factory::FormattingContextFactory;
