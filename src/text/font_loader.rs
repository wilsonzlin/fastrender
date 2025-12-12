//! Font loading utilities
//!
//! This module provides high-level APIs for font access during layout and rendering.
//! It combines the font database with caching for efficient font access.
//!
//! # Overview
//!
//! The font loader provides:
//! - `FontContext`: High-level interface for font access during layout
//! - Convenient methods for resolving CSS font properties to actual fonts
//! - Integration with the style system
//!
//! # Example
//!
//! ```rust,ignore
//! use fastrender::text::font_loader::FontContext;
//!
//! let ctx = FontContext::new();
//!
//! // Get a font by family list (CSS font-family style)
//! let families = vec!["Arial".to_string(), "sans-serif".to_string()];
//! if let Some(font) = ctx.get_font(&families, 400, false, false) {
//!     println!("Using font: {}", font.family);
//! }
//! ```

use crate::text::font_db::{FontDatabase, FontStretch, FontStyle, FontWeight, LoadedFont, ScaledMetrics};
use rustybuzz::{Direction, Face, UnicodeBuffer};
use std::fs;
use std::sync::Arc;

const EMBEDDED_FALLBACK_FONT: &[u8] = include_bytes!("../../resources/fonts/Roboto-Regular.ttf");

/// Font context for text operations
///
/// This is the main interface for font access during layout and rendering.
/// It holds a reference to the font database and provides convenient methods
/// for resolving fonts from CSS properties.
///
/// # Thread Safety
///
/// FontContext is thread-safe and can be shared across threads. The underlying
/// font database uses interior mutability for caching.
///
/// # Example
///
/// ```rust,ignore
/// use fastrender::text::font_loader::FontContext;
///
/// // Create a new font context (loads system fonts)
/// let ctx = FontContext::new();
///
/// // Get font count
/// println!("Available fonts: {}", ctx.font_count());
///
/// // Find a font
/// let families = vec!["Roboto".to_string(), "Arial".to_string(), "sans-serif".to_string()];
/// if let Some(font) = ctx.get_font(&families, 400, false, false) {
///     println!("Found: {} (weight {})", font.family, font.weight.value());
/// }
/// ```
#[derive(Clone)]
pub struct FontContext {
    db: Arc<FontDatabase>,
}

impl FontContext {
    /// Creates a new font context with system fonts loaded
    ///
    /// This will scan system font directories and load all available fonts.
    /// The operation may take a moment on systems with many fonts.
    ///
    /// # Example
    ///
    /// ```rust,ignore
    /// let ctx = FontContext::new();
    /// println!("Loaded {} fonts", ctx.font_count());
    /// ```
    pub fn new() -> Self {
        let mut db = FontDatabase::new();

        // Some minimal container environments ship without system fonts.
        // Attempt to load a few common sans-serif files so text shaping can proceed.
        if db.font_count() == 0 {
            let _ = db.load_font_data(EMBEDDED_FALLBACK_FONT.to_vec());
        }

        if db.font_count() == 0 {
            const FALLBACK_FONT_FILES: &[&str] = &[
                "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf",
                "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf",
                "/usr/share/fonts/truetype/freefont/FreeSans.ttf",
            ];

            for path in FALLBACK_FONT_FILES {
                if let Ok(data) = fs::read(path) {
                    let _ = db.load_font_data(data);
                    if db.font_count() > 0 {
                        break;
                    }
                }
            }
        }

        Self { db: Arc::new(db) }
    }

    /// Creates a font context with a custom font database
    ///
    /// Useful for testing or when you need to share a database between contexts.
    ///
    /// # Example
    ///
    /// ```rust,ignore
    /// let db = Arc::new(FontDatabase::empty());
    /// let ctx = FontContext::with_database(db);
    /// ```
    pub fn with_database(db: Arc<FontDatabase>) -> Self {
        Self { db }
    }

    /// Creates an empty font context (no fonts loaded)
    ///
    /// Useful for testing.
    pub fn empty() -> Self {
        Self {
            db: Arc::new(FontDatabase::empty()),
        }
    }

    /// Gets a reference to the underlying font database
    #[inline]
    pub fn database(&self) -> &FontDatabase {
        &self.db
    }

    /// Returns the number of available fonts
    #[inline]
    pub fn font_count(&self) -> usize {
        self.db.font_count()
    }

    /// Returns whether any fonts are available
    #[inline]
    pub fn has_fonts(&self) -> bool {
        !self.db.is_empty()
    }

    /// Gets a font matching the given CSS properties
    ///
    /// This is the main method for resolving fonts from CSS.
    ///
    /// # Arguments
    ///
    /// * `families` - List of font family names in priority order
    /// * `weight` - Numeric weight (100-900, 400=normal, 700=bold)
    /// * `italic` - Whether italic style is requested
    /// * `oblique` - Whether oblique style is requested (used if italic is false)
    ///
    /// # Returns
    ///
    /// The loaded font data, or None if no matching font is found.
    ///
    /// # Example
    ///
    /// ```rust,ignore
    /// let ctx = FontContext::new();
    ///
    /// // Get normal weight Arial or fallback
    /// let font = ctx.get_font(
    ///     &["Arial".to_string(), "sans-serif".to_string()],
    ///     400,    // normal weight
    ///     false,  // not italic
    ///     false,  // not oblique
    /// );
    ///
    /// // Get bold italic
    /// let font = ctx.get_font(
    ///     &["Georgia".to_string(), "serif".to_string()],
    ///     700,    // bold
    ///     true,   // italic
    ///     false,
    /// );
    /// ```
    pub fn get_font(&self, families: &[String], weight: u16, italic: bool, oblique: bool) -> Option<LoadedFont> {
        let requested_style = if italic {
            FontStyle::Italic
        } else if oblique {
            FontStyle::Oblique
        } else {
            FontStyle::Normal
        };

        let weights = crate::text::pipeline::weight_preference_order(weight);
        for weight_choice in &weights {
            let font_weight = FontWeight::new(*weight_choice);
            for slope in crate::text::pipeline::slope_preference_order(requested_style) {
                if let Some(id) = self.db.resolve_family_list(families, font_weight, *slope) {
                    if let Some(font) = self.db.load_font(id) {
                        return Some(font);
                    }
                }
            }
        }
        None
    }

    /// Gets a font with full CSS properties
    ///
    /// Extended version that also handles font-stretch.
    ///
    /// # Arguments
    ///
    /// * `families` - List of font family names
    /// * `weight` - Numeric weight (100-900)
    /// * `style` - Font style
    /// * `stretch` - Font stretch/width
    ///
    /// # Example
    ///
    /// ```rust,ignore
    /// use fastrender::text::font_db::{FontStyle, FontStretch};
    ///
    /// let font = ctx.get_font_full(
    ///     &["Arial Narrow".to_string(), "Arial".to_string()],
    ///     400,
    ///     FontStyle::Normal,
    ///     FontStretch::Condensed,
    /// );
    /// ```
    pub fn get_font_full(
        &self,
        families: &[String],
        weight: u16,
        style: FontStyle,
        stretch: FontStretch,
    ) -> Option<LoadedFont> {
        let stretches = crate::text::pipeline::stretch_preference_order(stretch.into());
        let weights = crate::text::pipeline::weight_preference_order(weight);
        for weight_choice in &weights {
            let font_weight = FontWeight::new(*weight_choice);
            for slope in crate::text::pipeline::slope_preference_order(style) {
                for stretch_choice in &stretches {
                    if let Some(id) = self
                        .db
                        .resolve_family_list_full(families, font_weight, *slope, *stretch_choice)
                    {
                        if let Some(font) = self.db.load_font(id) {
                            return Some(font);
                        }
                    }
                }
            }
        }
        None
    }

    /// Gets a font by simple family name query
    ///
    /// Convenience method for simple queries without fallback chain.
    ///
    /// # Example
    ///
    /// ```rust,ignore
    /// let font = ctx.get_font_simple("Arial", 400, FontStyle::Normal);
    /// ```
    pub fn get_font_simple(&self, family: &str, weight: u16, style: FontStyle) -> Option<LoadedFont> {
        let font_weight = FontWeight::new(weight);
        let id = self.db.query(family, font_weight, style)?;
        self.db.load_font(id)
    }

    /// Gets a sans-serif fallback font
    ///
    /// Returns a generic sans-serif font. Useful as a last resort fallback.
    pub fn get_sans_serif(&self) -> Option<LoadedFont> {
        self.get_font_simple("sans-serif", 400, FontStyle::Normal)
            .or_else(|| self.db.first_font())
    }

    /// Gets a serif fallback font
    pub fn get_serif(&self) -> Option<LoadedFont> {
        self.get_font_simple("serif", 400, FontStyle::Normal)
            .or_else(|| self.db.first_font())
    }

    /// Gets a monospace fallback font
    pub fn get_monospace(&self) -> Option<LoadedFont> {
        self.get_font_simple("monospace", 400, FontStyle::Normal)
            .or_else(|| self.db.first_font())
    }

    /// Clears the font data cache
    ///
    /// Frees memory used by cached font data. Fonts will be reloaded
    /// from disk on next access.
    pub fn clear_cache(&self) {
        self.db.clear_cache();
    }

    // ========================================================================
    // Text Measurement (W3.T18)
    // ========================================================================

    /// Measures the width of text when rendered with the given font
    ///
    /// Uses HarfBuzz (via rustybuzz) for accurate text shaping that accounts
    /// for kerning, ligatures, and other OpenType features.
    ///
    /// # Arguments
    ///
    /// * `text` - The text to measure
    /// * `font` - The loaded font to use for measurement
    /// * `font_size` - Font size in pixels
    ///
    /// # Returns
    ///
    /// The width of the text in pixels.
    ///
    /// # Example
    ///
    /// ```rust,ignore
    /// let ctx = FontContext::new();
    /// if let Some(font) = ctx.get_sans_serif() {
    ///     let width = ctx.measure_text("Hello, world!", &font, 16.0);
    ///     println!("Text width: {}px", width);
    /// }
    /// ```
    pub fn measure_text(&self, text: &str, font: &LoadedFont, font_size: f32) -> f32 {
        if text.is_empty() {
            return 0.0;
        }

        // Get rustybuzz face for shaping
        let rb_face = match Face::from_slice(&font.data, font.index) {
            Some(face) => face,
            None => return self.estimate_text_width(text, font_size),
        };

        // Shape the text
        let mut buffer = UnicodeBuffer::new();
        buffer.push_str(text);
        buffer.set_direction(Direction::LeftToRight);

        let output = rustybuzz::shape(&rb_face, &[], buffer);
        let positions = output.glyph_positions();

        // Calculate scale from font units to pixels
        let units_per_em = rb_face.units_per_em() as f32;
        let scale = font_size / units_per_em;

        // Sum up horizontal advances
        let mut width: f32 = 0.0;
        for pos in positions {
            width += pos.x_advance as f32 * scale;
        }

        width
    }

    /// Measures text and returns detailed information
    ///
    /// Returns both width and additional shaping information.
    ///
    /// # Example
    ///
    /// ```rust,ignore
    /// let ctx = FontContext::new();
    /// if let Some(font) = ctx.get_sans_serif() {
    ///     let measurement = ctx.measure_text_detailed("Hello", &font, 16.0);
    ///     println!("Width: {}px, Glyphs: {}", measurement.width, measurement.glyph_count);
    /// }
    /// ```
    pub fn measure_text_detailed(&self, text: &str, font: &LoadedFont, font_size: f32) -> TextMeasurement {
        if text.is_empty() {
            return TextMeasurement {
                width: 0.0,
                glyph_count: 0,
                is_shaped: false,
            };
        }

        // Get rustybuzz face for shaping
        let rb_face = match Face::from_slice(&font.data, font.index) {
            Some(face) => face,
            None => {
                return TextMeasurement {
                    width: self.estimate_text_width(text, font_size),
                    glyph_count: text.chars().count(),
                    is_shaped: false,
                };
            }
        };

        // Shape the text
        let mut buffer = UnicodeBuffer::new();
        buffer.push_str(text);
        buffer.set_direction(Direction::LeftToRight);

        let output = rustybuzz::shape(&rb_face, &[], buffer);
        let positions = output.glyph_positions();

        // Calculate scale from font units to pixels
        let units_per_em = rb_face.units_per_em() as f32;
        let scale = font_size / units_per_em;

        // Sum up horizontal advances
        let mut width: f32 = 0.0;
        for pos in positions {
            width += pos.x_advance as f32 * scale;
        }

        TextMeasurement {
            width,
            glyph_count: positions.len(),
            is_shaped: true,
        }
    }

    /// Estimates text width without shaping
    ///
    /// Used as fallback when font shaping is unavailable.
    fn estimate_text_width(&self, text: &str, font_size: f32) -> f32 {
        // Rough estimate: average character width is about 0.5 * font size
        let char_count = text.chars().count() as f32;
        char_count * font_size * 0.5
    }

    /// Gets scaled metrics for a font at a specific size
    ///
    /// Convenience method that combines font loading with metric scaling.
    ///
    /// # Example
    ///
    /// ```rust,ignore
    /// let ctx = FontContext::new();
    /// if let Some(font) = ctx.get_sans_serif() {
    ///     let metrics = ctx.get_scaled_metrics(&font, 16.0);
    ///     if let Some(m) = metrics {
    ///         println!("Line height: {}px", m.line_height);
    ///     }
    /// }
    /// ```
    pub fn get_scaled_metrics(&self, font: &LoadedFont, font_size: f32) -> Option<ScaledMetrics> {
        font.metrics().ok().map(|m| m.scale(font_size))
    }
}

impl Default for FontContext {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// TextMeasurement
// ============================================================================

/// Result of detailed text measurement
///
/// Contains width and additional information about the shaped text.
#[derive(Debug, Clone)]
pub struct TextMeasurement {
    /// Total width in pixels
    pub width: f32,

    /// Number of glyphs after shaping
    ///
    /// May differ from character count due to ligatures, combining marks, etc.
    pub glyph_count: usize,

    /// Whether text was shaped with HarfBuzz
    ///
    /// If false, width is an estimate.
    pub is_shaped: bool,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_font_context_creation() {
        let ctx = FontContext::new();
        // May have 0 fonts in minimal CI environments
        // font_count() returns usize which is always >= 0
        let _ = ctx.font_count(); // Just verify it works
    }

    #[test]
    fn test_font_context_empty() {
        let ctx = FontContext::empty();
        assert!(!ctx.has_fonts());
        assert_eq!(ctx.font_count(), 0);
    }

    #[test]
    fn test_font_context_with_database() {
        let db = Arc::new(FontDatabase::new());
        let count = db.font_count();

        let ctx = FontContext::with_database(db);
        assert_eq!(ctx.font_count(), count);
    }

    #[test]
    fn test_get_font_fallback() {
        let ctx = FontContext::new();
        if !ctx.has_fonts() {
            return;
        }

        let families = vec!["NonExistentFont123456".to_string(), "sans-serif".to_string()];

        let font = ctx.get_font(&families, 400, false, false);
        if let Some(font) = font {
            assert!(!font.data.is_empty());
        }
    }

    #[test]
    fn test_get_sans_serif() {
        let ctx = FontContext::new();
        if !ctx.has_fonts() {
            return;
        }

        let font = ctx.get_sans_serif();
        if font.is_some() {
            assert!(!font.unwrap().family.is_empty());
        }
    }

    #[test]
    fn test_get_serif() {
        let ctx = FontContext::new();
        if !ctx.has_fonts() {
            return;
        }

        let font = ctx.get_serif();
        if font.is_some() {
            assert!(!font.unwrap().data.is_empty());
        }
    }

    #[test]
    fn test_get_monospace() {
        let ctx = FontContext::new();
        if !ctx.has_fonts() {
            return;
        }

        let font = ctx.get_monospace();
        if font.is_some() {
            assert!(!font.unwrap().data.is_empty());
        }
    }

    #[test]
    fn test_font_context_clone() {
        let ctx1 = FontContext::new();
        let ctx2 = ctx1.clone();

        // Both should point to same database
        assert_eq!(ctx1.font_count(), ctx2.font_count());
    }

    #[test]
    fn test_get_font_italic() {
        let ctx = FontContext::new();
        if !ctx.has_fonts() {
            return;
        }

        let families = vec!["sans-serif".to_string()];
        let font = ctx.get_font(&families, 400, true, false);
        // May or may not find italic version
        if font.is_some() {
            assert!(!font.unwrap().data.is_empty());
        }
    }

    #[test]
    fn test_get_font_bold() {
        let ctx = FontContext::new();
        if !ctx.has_fonts() {
            return;
        }

        let families = vec!["sans-serif".to_string()];
        let font = ctx.get_font(&families, 700, false, false);
        if let Some(font) = font {
            // Weight may not be exactly 700 due to fuzzy matching
            assert!(font.weight.value() >= 400);
        }
    }

    #[test]
    fn test_get_font_full() {
        let ctx = FontContext::new();
        if !ctx.has_fonts() {
            return;
        }

        let families = vec!["sans-serif".to_string()];
        let font = ctx.get_font_full(&families, 400, FontStyle::Normal, FontStretch::Normal);
        if let Some(font) = font {
            assert!(!font.data.is_empty());
        }
    }

    #[test]
    fn test_get_font_falls_back_when_slope_missing() {
        let mut db = FontDatabase::empty();
        db.load_font_data(EMBEDDED_FALLBACK_FONT.to_vec())
            .expect("load embedded font");
        let ctx = FontContext::with_database(Arc::new(db));

        let families = vec!["Roboto".to_string()];
        // Request italic + heavier weight even though only a normal face exists; should still return a font.
        let font = ctx.get_font(&families, 700, true, false);
        assert!(font.is_some());
    }

    #[test]
    fn test_clear_cache() {
        let ctx = FontContext::new();
        if !ctx.has_fonts() {
            return;
        }

        // Load a font to populate cache
        let _ = ctx.get_sans_serif();

        // Clear cache
        ctx.clear_cache();

        // Should still work (reloads from disk)
        let font = ctx.get_sans_serif();
        if font.is_some() {
            assert!(!font.unwrap().data.is_empty());
        }
    }

    #[test]
    fn test_embedded_fallback_font_loads() {
        let mut db = FontDatabase::empty();
        let before = db.font_count();
        let _ = db.load_font_data(EMBEDDED_FALLBACK_FONT.to_vec());
        assert!(db.font_count() > before);
    }

    // ========================================================================
    // Text Measurement Tests (W3.T18)
    // ========================================================================

    #[test]
    fn test_measure_text_empty() {
        let ctx = FontContext::new();
        if !ctx.has_fonts() {
            return;
        }

        if let Some(font) = ctx.get_sans_serif() {
            let width = ctx.measure_text("", &font, 16.0);
            assert_eq!(width, 0.0);
        }
    }

    #[test]
    fn test_measure_text_single_char() {
        let ctx = FontContext::new();
        if !ctx.has_fonts() {
            return;
        }

        if let Some(font) = ctx.get_sans_serif() {
            let width = ctx.measure_text("A", &font, 16.0);
            // Single character should have positive width
            assert!(width > 0.0);
            // And reasonable width (not more than font size for most chars)
            assert!(width < 20.0);
        }
    }

    #[test]
    fn test_measure_text_multiple_chars() {
        let ctx = FontContext::new();
        if !ctx.has_fonts() {
            return;
        }

        if let Some(font) = ctx.get_sans_serif() {
            let width1 = ctx.measure_text("A", &font, 16.0);
            let width5 = ctx.measure_text("AAAAA", &font, 16.0);

            // 5 chars should be wider than 1 char
            assert!(width5 > width1);
            // Should be roughly 5x (with some tolerance for kerning)
            assert!(width5 > width1 * 4.0);
            assert!(width5 < width1 * 6.0);
        }
    }

    #[test]
    fn test_measure_text_font_size_scaling() {
        let ctx = FontContext::new();
        if !ctx.has_fonts() {
            return;
        }

        if let Some(font) = ctx.get_sans_serif() {
            let width16 = ctx.measure_text("Hello", &font, 16.0);
            let width32 = ctx.measure_text("Hello", &font, 32.0);

            // Double font size should roughly double width
            assert!(width32 > width16 * 1.8);
            assert!(width32 < width16 * 2.2);
        }
    }

    #[test]
    fn test_measure_text_detailed_empty() {
        let ctx = FontContext::new();
        if !ctx.has_fonts() {
            return;
        }

        if let Some(font) = ctx.get_sans_serif() {
            let measurement = ctx.measure_text_detailed("", &font, 16.0);
            assert_eq!(measurement.width, 0.0);
            assert_eq!(measurement.glyph_count, 0);
            assert!(!measurement.is_shaped);
        }
    }

    #[test]
    fn test_measure_text_detailed_shaped() {
        let ctx = FontContext::new();
        if !ctx.has_fonts() {
            return;
        }

        if let Some(font) = ctx.get_sans_serif() {
            let measurement = ctx.measure_text_detailed("Hello", &font, 16.0);
            assert!(measurement.width > 0.0);
            assert!(measurement.glyph_count > 0);
            assert!(measurement.is_shaped);
        }
    }

    #[test]
    fn test_measure_text_detailed_glyph_count() {
        let ctx = FontContext::new();
        if !ctx.has_fonts() {
            return;
        }

        if let Some(font) = ctx.get_sans_serif() {
            let measurement = ctx.measure_text_detailed("Hello", &font, 16.0);
            // For simple ASCII text, glyph count should equal char count
            assert_eq!(measurement.glyph_count, 5);
        }
    }

    #[test]
    fn test_get_scaled_metrics() {
        let ctx = FontContext::new();
        if !ctx.has_fonts() {
            return;
        }

        if let Some(font) = ctx.get_sans_serif() {
            let metrics = ctx.get_scaled_metrics(&font, 16.0);
            assert!(metrics.is_some());

            let m = metrics.unwrap();
            assert_eq!(m.font_size, 16.0);
            assert!(m.ascent > 0.0);
            assert!(m.descent > 0.0);
            assert!(m.line_height > 0.0);
        }
    }

    #[test]
    fn test_get_scaled_metrics_different_sizes() {
        let ctx = FontContext::new();
        if !ctx.has_fonts() {
            return;
        }

        if let Some(font) = ctx.get_sans_serif() {
            let m16 = ctx.get_scaled_metrics(&font, 16.0);
            let m32 = ctx.get_scaled_metrics(&font, 32.0);

            assert!(m16.is_some());
            assert!(m32.is_some());

            let m16 = m16.unwrap();
            let m32 = m32.unwrap();

            // Double size should roughly double metrics
            assert!(m32.ascent > m16.ascent * 1.8);
            assert!(m32.line_height > m16.line_height * 1.8);
        }
    }

    #[test]
    fn test_measure_text_unicode() {
        let ctx = FontContext::new();
        if !ctx.has_fonts() {
            return;
        }

        if let Some(font) = ctx.get_sans_serif() {
            // Test with Unicode characters
            let width = ctx.measure_text("Héllo", &font, 16.0);
            assert!(width > 0.0);
        }
    }

    #[test]
    fn test_measure_text_spaces() {
        let ctx = FontContext::new();
        if !ctx.has_fonts() {
            return;
        }

        if let Some(font) = ctx.get_sans_serif() {
            let width_no_space = ctx.measure_text("AB", &font, 16.0);
            let width_with_space = ctx.measure_text("A B", &font, 16.0);

            // Text with space should be wider
            assert!(width_with_space > width_no_space);
        }
    }
}
