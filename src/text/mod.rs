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
//! This module contains:
//! - `font` - Font loading, metrics, and database
//! - `shaper.rs` - Text shaping integration (future)
//! - `bidi.rs` - Bidirectional text handling (future)
//! - `linebreak.rs` - Line breaking algorithm (future)
//! - `run.rs` - Text run representation (future)
//!
//! # Example
//!
//! ```rust,ignore
//! use fastrender::text::{FontDatabase, Shaper};
//!
//! let font_db = FontDatabase::load_system_fonts();
//! let shaper = Shaper::new(&font_db);
//!
//! let shaped = shaper.shape("Hello, world!", &computed_style);
//! for glyph in shaped.glyphs() {
//!     println!("Glyph {} at ({}, {})", glyph.id, glyph.x, glyph.y);
//! }
//! ```

// Font system module (Wave 3)
pub mod font;

// Re-export font types for convenience
pub use font::{extract_metrics, FontMetrics, ScaledFontMetrics};

// Future module declarations (Wave 3+)
// pub mod shaper;
// pub mod bidi;
// pub mod linebreak;
// pub mod run;

// Temporary re-export of V1 implementation for compatibility
// This will be removed as Wave 3+ tasks are completed
#[path = "../text_v1.rs"]
mod text_v1;
pub use text_v1::*;
