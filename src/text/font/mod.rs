//! Font system
//!
//! This module provides font handling for text layout and rendering:
//!
//! - **Font Metrics**: Extraction of font dimensions (ascent, descent, etc.)
//! - **Font Loading**: Loading fonts from system or files (future)
//! - **Font Matching**: CSS font matching algorithm (future)
//! - **Font Caching**: Efficient font data caching (future)
//!
//! # Architecture
//!
//! The font system is organized as follows:
//!
//! - `metrics.rs` - Font metrics extraction from TTF/OTF files
//! - `database.rs` - Font database and matching (future)
//! - `loader.rs` - High-level font loading API (future)
//! - `cache.rs` - Font caching (future)
//!
//! # CSS Specification
//!
//! This module implements concepts from:
//! - CSS Fonts Module Level 4: https://www.w3.org/TR/css-fonts-4/
//! - OpenType Specification: https://learn.microsoft.com/en-us/typography/opentype/spec/
//!
//! # Examples
//!
//! ```rust,ignore
//! use fastrender::text::font::{FontMetrics, extract_metrics};
//!
//! // Load font and extract metrics
//! let font_data = std::fs::read("font.ttf")?;
//! let metrics = FontMetrics::from_font_data(&font_data, 0)?;
//!
//! // Scale to pixel size
//! let scaled = metrics.scale(16.0);
//! println!("Ascent: {}px", scaled.ascent);
//! println!("Descent: {}px", scaled.descent);
//! println!("Line height: {}px", scaled.line_height);
//! ```

pub mod metrics;

// Re-export main types
pub use metrics::{extract_metrics, FontMetrics, ScaledFontMetrics};
