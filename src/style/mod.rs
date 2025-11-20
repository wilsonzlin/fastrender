//! Style system types

pub mod display;

// Re-export commonly used types from display module
pub use display::{Display, DisplayParseError, FormattingContextType, InnerDisplay, OuterDisplay};

// Re-export all types from the old style module for backwards compatibility
// This allows existing code to continue working while we migrate to the new structure
pub use crate::style_old::*;
