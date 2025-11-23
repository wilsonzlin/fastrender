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

use crate::text::font_db::{FontDatabase, FontStretch, FontStyle, FontWeight, LoadedFont};
use std::sync::Arc;

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
        Self {
            db: Arc::new(FontDatabase::new()),
        }
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
        let font_weight = FontWeight::new(weight);
        let font_style = if italic {
            FontStyle::Italic
        } else if oblique {
            FontStyle::Oblique
        } else {
            FontStyle::Normal
        };

        let id = self.db.resolve_family_list(families, font_weight, font_style)?;
        self.db.load_font(id)
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
        let font_weight = FontWeight::new(weight);
        let id = self
            .db
            .resolve_family_list_full(families, font_weight, style, stretch)?;
        self.db.load_font(id)
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
    }

    /// Gets a serif fallback font
    pub fn get_serif(&self) -> Option<LoadedFont> {
        self.get_font_simple("serif", 400, FontStyle::Normal)
    }

    /// Gets a monospace fallback font
    pub fn get_monospace(&self) -> Option<LoadedFont> {
        self.get_font_simple("monospace", 400, FontStyle::Normal)
    }

    /// Clears the font data cache
    ///
    /// Frees memory used by cached font data. Fonts will be reloaded
    /// from disk on next access.
    pub fn clear_cache(&self) {
        self.db.clear_cache();
    }
}

impl Default for FontContext {
    fn default() -> Self {
        Self::new()
    }
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
        if font.is_some() {
            let font = font.unwrap();
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
        if font.is_some() {
            let font = font.unwrap();
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
        if font.is_some() {
            assert!(!font.unwrap().data.is_empty());
        }
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
}
