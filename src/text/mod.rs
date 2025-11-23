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
//! - `font_db` - Font database, discovery, and loading
//! - `font_loader` - High-level font context
//! - `font_fallback` - Per-character font fallback chain
//! - Future: `shaper` - Text shaping integration
//! - Future: `bidi` - Bidirectional text handling
//! - Future: `linebreak` - Line breaking algorithm
//!
//! # Example
//!
//! ```rust,ignore
//! use fastrender::text::{FontContext, FallbackChain, GenericFamily};
//!
//! // Create font context (loads system fonts)
//! let ctx = FontContext::new();
//!
//! // Get a font by family list
//! let families = vec!["Arial".to_string(), "sans-serif".to_string()];
//! if let Some(font) = ctx.get_font(&families, 400, false, false) {
//!     println!("Using font: {}", font.family);
//! }
//!
//! // Or use fallback chain for per-character resolution
//! let db = FontDatabase::new();
//! let chain = FallbackChain::new()
//!     .add_family("Roboto")
//!     .add_generic(GenericFamily::SansSerif);
//!
//! if let Some(font_id) = chain.resolve('A', &db) {
//!     println!("Found font for 'A'");
//! }
//! ```

// ============================================================================
// Font System (Wave 3)
// ============================================================================

pub mod font_db;
pub mod font_fallback;
pub mod font_loader;

// ============================================================================
// Line Breaking (Wave 4)
// ============================================================================

pub mod line_break;

// Re-export primary types for convenience
pub use font_db::{
    FontDatabase, FontMetrics, FontStretch, FontStyle, FontWeight, GenericFamily, LoadedFont, ScaledMetrics,
};
pub use font_fallback::{FallbackChain, FallbackChainBuilder, FamilyEntry, FontId};
pub use font_loader::{FontContext, TextMeasurement};
pub use line_break::{
    find_break_opportunities, find_interior_breaks, find_mandatory_breaks, has_break_at, BreakIterator,
    BreakOpportunity, BreakType,
};

// ============================================================================
// Legacy V1 Implementation (to be refactored in Wave 4+)
// ============================================================================

// The V1 text module provides temporary compatibility
// This will be removed as Wave 4 tasks complete the text shaping implementation
#[path = "../text_v1.rs"]
mod text_v1;
pub use text_v1::*;
