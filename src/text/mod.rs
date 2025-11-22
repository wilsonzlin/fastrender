//! Text shaping, font handling, and line breaking
//!
//! This module handles all text-related operations:
//! - Font loading and fallback
//! - Text shaping (glyph generation)
//! - Bidirectional text (bidi) reordering
//! - Line breaking and word wrapping
//!
//! # Responsibilities
//!
//! - **Font Loading**: Load system fonts, parse font files
//! - **Font Matching**: Find fonts matching CSS font-family, weight, style
//! - **Shaping**: Convert Unicode text to positioned glyphs
//! - **Bidi**: Handle right-to-left and mixed-direction text
//! - **Line Breaking**: Find line break opportunities, wrap text
//!
//! # Architecture
//!
//! Text processing pipeline:
//!
//! 1. **Font Resolution**: Match CSS font properties to actual fonts
//! 2. **Bidi Analysis**: Analyze Unicode bidirectional properties
//! 3. **Shaping**: Run HarfBuzz (via rustybuzz) to get glyphs
//! 4. **Line Breaking**: Find break opportunities using UAX #14
//! 5. **Fragment Creation**: Create InlineTextFragment for each run
//!
//! # Module Organization
//!
//! - `font_db` - Font database and discovery
//! - `font_loader` - High-level font context
//! - `shaper` - Text shaping integration (future)
//! - `bidi` - Bidirectional text handling (future)
//! - `linebreak` - Line breaking algorithm (future)
//!
//! # Example
//!
//! ```rust,ignore
//! use fastrender::text::{FontContext, FontWeight, FontStyle};
//!
//! // Create font context (loads system fonts)
//! let ctx = FontContext::new();
//!
//! // Get a font
//! let families = vec!["Arial".to_string(), "sans-serif".to_string()];
//! if let Some(font) = ctx.get_font(&families, 400, false, false) {
//!     println!("Using font: {} ({} bytes)", font.family, font.data.len());
//! }
//! ```

// ============================================================================
// Font System (Wave 3)
// ============================================================================

pub mod font_db;
pub mod font_loader;

// Re-export primary types for convenience
pub use font_db::{FontDatabase, FontStretch, FontStyle, FontWeight, GenericFamily, LoadedFont};
pub use font_loader::FontContext;

// ============================================================================
// Legacy V1 Implementation (to be refactored in Wave 4+)
// ============================================================================

// The V1 text module provides temporary compatibility
// This will be removed as Wave 4 tasks complete the text shaping implementation
#[path = "../text_v1.rs"]
mod text_v1;
pub use text_v1::*;
