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
//! - `bidi` - Bidirectional text handling (UAX #9)
//! - `script` - Script itemization (UAX #24)
//! - `shaper` - Text shaping using HarfBuzz (rustybuzz)
//! - `emoji` - Emoji detection and sequence parsing (UTS #51)
//! - `clustering` - Cursor positioning and hit testing utilities
//! - `pipeline` - Unified text shaping pipeline
//! - `line_break` - Line break opportunity detection (UAX #14)
//! - `hyphenation` - Word hyphenation for improved line breaking
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

// Text Shaping Pipeline (Wave 4)
// ============================================================================

pub mod bidi;
pub mod clustering;
pub mod emoji;
pub mod hyphenation;
pub mod justify;
pub mod line_break;
pub mod pipeline;
pub mod script;
pub mod shaper;

// ============================================================================
// Re-exports
// ============================================================================

// Font types
pub use font_db::{
    FontDatabase, FontMetrics, FontStretch, FontStyle, FontWeight, GenericFamily, LoadedFont, ScaledMetrics,
};
pub use font_fallback::{FallbackChain, FallbackChainBuilder, FamilyEntry, FontId};
pub use font_loader::{FontContext, TextMeasurement};

// Justification types (Note: GlyphPosition not re-exported to avoid conflict with shaper::GlyphPosition)
pub use justify::{
    apply_alignment, calculate_line_width, detect_justification_mode, is_cjk_character, justify_line,
    justify_line_with_options, justify_lines, mark_word_boundaries, mark_word_boundaries_by_glyph_id,
    JustificationMode, JustificationOptions, JustificationResult, TextAlignment,
};

// Shaper types
pub use shaper::{GlyphCluster, GlyphPosition, Script, ShapedGlyphs, TextDirection, TextShaper};

// Script itemization types
pub use script::{itemize_scripts, ScriptRun};

// Bidi types
pub use bidi::{analyze_bidi, BidiAnalysis, BidiAnalyzer, BidiRun, Direction};

// Emoji types
pub use emoji::{
    find_emoji_sequences, is_emoji_modifier, is_emoji_modifier_base, is_emoji_presentation, is_regional_indicator,
    EmojiSequence, EmojiSequenceType,
};

// Hyphenation types
pub use hyphenation::{
    find_soft_hyphens, is_soft_hyphen, remove_soft_hyphens, HyphenationPatterns, Hyphenator, HyphensMode,
    SupportedLanguage,
};

// Line break types
pub use line_break::{
    break_lines, break_lines_greedy, find_break_opportunities, find_interior_breaks, find_mandatory_breaks,
    has_break_at, BreakIterator, BreakOpportunity, BreakType, GreedyLineBreaker, Line, LineSegment,
};

// Clustering utilities
pub use clustering::{
    byte_offset_to_x, cluster_end_position, cluster_range_advance, cluster_start_position, count_chars_in_range,
    find_cluster_at_byte, find_cluster_for_glyph, get_selection, x_to_byte_offset, ClusterLookup, ClusterSelection,
};

// Pipeline types
pub use pipeline::{assign_fonts, itemize_text, FontRun, ItemizedRun, ShapedRun, ShapingPipeline};

// ============================================================================
// Legacy V1 Implementation (to be refactored in Wave 4+)
// ============================================================================

// The V1 text module provides temporary compatibility
// This will be removed as Wave 4 tasks complete the text shaping implementation
#[path = "../text_v1.rs"]
mod text_v1;
pub use text_v1::*;
