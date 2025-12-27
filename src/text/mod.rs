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
//! - `emoji` - Emoji detection and sequence parsing (UTS #51)
//! - `pipeline` - Unified text shaping pipeline (bidi, script, shaping)
//! - `line_break` - Line break opportunity detection (UAX #14)
//! - `hyphenation` - Word hyphenation for improved line breaking
//!
//! # Example
//!
//! ```rust,no_run
//! # use fastrender::text::{FallbackChain, FontContext, FontDatabase, GenericFamily};
//! # fn main() -> fastrender::Result<()> {
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
//! # Ok(())
//! # }
//! ```

// ============================================================================
// Font System (Wave 3)
// ============================================================================

pub mod color_fonts;
pub mod cpal;
pub mod font_db;
pub mod font_fallback;
pub mod font_loader;
pub mod font_resolver;
pub mod glyph_path;

// Text Shaping Pipeline (Wave 4)
// ============================================================================

pub mod cluster;
pub mod emoji;
pub mod hyphenation;
pub mod justify;
pub mod line_break;
pub mod pipeline;
<<<<<<< HEAD
pub mod segmentation;
=======
pub mod variations;
>>>>>>> 944f6f1 (feat: apply font variations during paint)

pub use pipeline::itemize_text;
pub use pipeline::BidiAnalysis;
pub use pipeline::BidiRun;
pub use pipeline::ClusterMap;
pub use pipeline::Direction;
pub use pipeline::ExplicitBidiContext;
pub use pipeline::GlyphPosition;
pub use pipeline::ItemizedRun;
pub use pipeline::ParagraphBoundary;
pub use pipeline::RunRotation;
pub use pipeline::Script;
pub use pipeline::ShapedRun;
pub use pipeline::ShapingPipeline;
<<<<<<< HEAD
pub use segmentation::segment_grapheme_clusters;
=======
pub use variations::{apply_rustybuzz_variations, apply_variations_to_face, FontVariation};
>>>>>>> 944f6f1 (feat: apply font variations during paint)
