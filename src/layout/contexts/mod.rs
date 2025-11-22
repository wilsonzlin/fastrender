//! Formatting context implementations and factory
//!
//! This module contains:
//! - FormattingContext factory (creates appropriate FC)
//! - Block formatting context components (margin collapsing)
//!
//! # Module Organization
//!
//! - `factory` - FC factory for creating formatting contexts
//! - `block` - Block formatting context components (W3.T04, W3.T05)

pub mod block;
pub mod factory;

// Re-exports
pub use factory::FormattingContextFactory;
