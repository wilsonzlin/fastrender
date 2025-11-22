//! Formatting context types and factory
//!
//! This module contains:
//! - FormattingContextType enum (discriminator)
//! - FormattingContext factory (creates appropriate FC)

pub mod types;

// Re-exports
pub use types::FormattingContextType;
pub use types::{derive_fc_from_display, establishes_independent_bfc, BfcTriggers};
