//! Text Rasterization Module
//!
//! Rasterizes text glyphs using font outlines and tiny-skia.
//!
//! # Overview
//!
//! This module converts shaped text (glyphs with positions) into pixels by:
//! 1. Extracting glyph outlines from font files using ttf-parser
//! 2. Converting outlines to tiny-skia paths
//! 3. Rendering paths to a pixmap with proper positioning and styling
//!
//! # Architecture
//!
//! ```text
//! ShapedRun (glyphs + font)
//!       ↓
//! GlyphOutlineBuilder (ttf-parser → path)
//!       ↓
//! GlyphCache (cached paths per font/glyph)
//!       ↓
//! TextRasterizer (render to pixmap)
//! ```
//!
//! # Example
//!
//! ```rust,ignore
//! use fastrender::paint::text_rasterize::TextRasterizer;
//! use fastrender::text::pipeline::ShapedRun;
//!
//! let mut rasterizer = TextRasterizer::new();
//! let pixmap = Pixmap::new(800, 600).unwrap();
//!
//! rasterizer.render_shaped_run(
//!     &shaped_run,
//!     100.0,  // x position
//!     200.0,  // baseline y position
//!     Color::BLACK,
//!     &mut pixmap,
//! );
//! ```
//!
//! # CSS Specification
//!
//! Text rendering follows CSS specifications:
//! - CSS Fonts Module Level 4: Font rendering properties
//! - CSS Text Module Level 3: Text rendering
//!
//! # References
//!
//! - ttf-parser: <https://docs.rs/ttf-parser/>
//! - tiny-skia: <https://docs.rs/tiny-skia/>

use crate::css::Color;
use crate::error::{RenderError, Result};
use crate::text::font_db::LoadedFont;
use crate::text::pipeline::{GlyphPosition, ShapedRun};
use std::collections::HashMap;
use std::sync::Arc;
use tiny_skia::{FillRule, Paint, Path, PathBuilder, Pixmap, Transform};

// ============================================================================
// Glyph Outline Builder
// ============================================================================

/// Converts ttf-parser glyph outlines to tiny-skia paths.
///
/// Implements the `ttf_parser::OutlineBuilder` trait to receive outline
/// drawing commands and build a tiny-skia `Path`.
///
/// # Font Units vs Pixels
///
/// Font outlines are in font design units (typically 1000 or 2048 units per em).
/// The caller must apply scaling to convert to pixels:
///
/// ```text
/// pixel_size = font_units * (font_size_px / units_per_em)
/// ```
struct GlyphOutlineBuilder {
    /// The path being built
    builder: PathBuilder,
    /// Scale factor to convert font units to pixels
    scale: f32,
    /// X offset for positioning
    x_offset: f32,
    /// Y offset for positioning (inverted for Y-down coordinate system)
    y_offset: f32,
}

impl GlyphOutlineBuilder {
    /// Creates a new outline builder.
    ///
    /// # Arguments
    ///
    /// * `scale` - Scale factor (font_size / units_per_em)
    /// * `x_offset` - X position offset in pixels
    /// * `y_offset` - Y position offset in pixels (baseline position)
    fn new(scale: f32, x_offset: f32, y_offset: f32) -> Self {
        Self {
            builder: PathBuilder::new(),
            scale,
            x_offset,
            y_offset,
        }
    }

    /// Converts font units to pixels with positioning.
    #[inline]
    fn transform_x(&self, x: f32) -> f32 {
        x * self.scale + self.x_offset
    }

    /// Converts font units to pixels with positioning.
    ///
    /// Note: Font Y coordinates are up-positive, but screen coordinates
    /// are down-positive. We invert Y by subtracting from the baseline.
    #[inline]
    fn transform_y(&self, y: f32) -> f32 {
        self.y_offset - (y * self.scale)
    }

    /// Consumes the builder and returns the completed path.
    fn finish(self) -> Option<Path> {
        self.builder.finish()
    }
}

impl ttf_parser::OutlineBuilder for GlyphOutlineBuilder {
    fn move_to(&mut self, x: f32, y: f32) {
        self.builder.move_to(self.transform_x(x), self.transform_y(y));
    }

    fn line_to(&mut self, x: f32, y: f32) {
        self.builder.line_to(self.transform_x(x), self.transform_y(y));
    }

    fn quad_to(&mut self, x1: f32, y1: f32, x: f32, y: f32) {
        self.builder.quad_to(
            self.transform_x(x1),
            self.transform_y(y1),
            self.transform_x(x),
            self.transform_y(y),
        );
    }

    fn curve_to(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, x: f32, y: f32) {
        self.builder.cubic_to(
            self.transform_x(x1),
            self.transform_y(y1),
            self.transform_x(x2),
            self.transform_y(y2),
            self.transform_x(x),
            self.transform_y(y),
        );
    }

    fn close(&mut self) {
        self.builder.close();
    }
}

// ============================================================================
// Glyph Cache
// ============================================================================

/// Cache key for glyph paths.
///
/// Glyphs are cached by font data pointer + glyph ID + font size.
/// Using the font data pointer avoids comparing large font binaries.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct GlyphCacheKey {
    /// Pointer to font data (used as unique identifier)
    font_ptr: usize,
    /// Font face index within the file
    font_index: u32,
    /// Glyph ID within the font
    glyph_id: u32,
    /// Font size in 1/100 pixels (to enable f32 hashing)
    font_size_hundredths: u32,
}

impl GlyphCacheKey {
    fn new(font: &LoadedFont, glyph_id: u32, font_size: f32) -> Self {
        Self {
            font_ptr: Arc::as_ptr(&font.data) as usize,
            font_index: font.index,
            glyph_id,
            font_size_hundredths: (font_size * 100.0) as u32,
        }
    }
}

/// Cached glyph path for efficient repeated rendering.
///
/// Contains a pre-built path that can be reused across multiple
/// render calls at the same font size.
#[derive(Debug, Clone)]
struct CachedGlyph {
    /// The rendered path, or None if the glyph has no outline
    path: Option<Path>,
    /// Horizontal advance for this glyph
    advance: f32,
}

/// Cache for rendered glyph paths.
///
/// Caches paths by font + glyph ID + size to avoid rebuilding
/// paths for frequently used characters.
///
/// # Performance
///
/// Path building is expensive (parsing font tables, bezier conversion).
/// Caching provides significant speedup for repeated text rendering.
///
/// # Memory
///
/// Each cached path uses roughly 100-500 bytes depending on glyph
/// complexity. A typical cache might hold 500-2000 glyphs.
#[derive(Debug, Default)]
pub struct GlyphCache {
    /// Cached glyph paths
    glyphs: HashMap<GlyphCacheKey, CachedGlyph>,
    /// Maximum cache size
    max_size: usize,
}

impl GlyphCache {
    /// Creates a new glyph cache with default size.
    pub fn new() -> Self {
        Self {
            glyphs: HashMap::new(),
            max_size: 2048,
        }
    }

    /// Creates a cache with custom maximum size.
    pub fn with_capacity(max_size: usize) -> Self {
        Self {
            glyphs: HashMap::with_capacity(max_size.min(256)),
            max_size,
        }
    }

    /// Gets a cached glyph path or builds and caches it.
    fn get_or_build(
        &self,
        font: &LoadedFont,
        glyph_id: u32,
        font_size: f32,
        x_offset: f32,
        y_offset: f32,
    ) -> Option<Path> {
        // For now, always build the path with the correct position
        // (caching positioned paths is complex, we'd need to store
        // unpositioned paths and transform them)
        self.build_glyph_path(font, glyph_id, font_size, x_offset, y_offset)
    }

    /// Builds a glyph path without caching.
    fn build_glyph_path(
        &self,
        font: &LoadedFont,
        glyph_id: u32,
        font_size: f32,
        x_offset: f32,
        y_offset: f32,
    ) -> Option<Path> {
        // Parse font face
        let face = font.as_ttf_face().ok()?;

        // Calculate scale factor
        let units_per_em = face.units_per_em() as f32;
        let scale = font_size / units_per_em;

        // Create outline builder
        let mut builder = GlyphOutlineBuilder::new(scale, x_offset, y_offset);

        // Get glyph outline
        let glyph_id = ttf_parser::GlyphId(glyph_id as u16);
        face.outline_glyph(glyph_id, &mut builder)?;

        // Build and return path
        builder.finish()
    }

    /// Returns the number of cached glyphs.
    #[inline]
    pub fn len(&self) -> usize {
        self.glyphs.len()
    }

    /// Returns whether the cache is empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.glyphs.is_empty()
    }

    /// Clears all cached glyphs.
    pub fn clear(&mut self) {
        self.glyphs.clear();
    }

    /// Evicts old entries if cache is full.
    fn evict_if_needed(&mut self) {
        if self.glyphs.len() >= self.max_size {
            // Simple strategy: clear half the cache
            // A more sophisticated LRU could be used
            let to_remove = self.max_size / 2;
            let keys: Vec<_> = self.glyphs.keys().take(to_remove).copied().collect();
            for key in keys {
                self.glyphs.remove(&key);
            }
        }
    }
}

// ============================================================================
// Text Rasterizer
// ============================================================================

/// Main text rasterizer for rendering shaped text to pixels.
///
/// Converts shaped text runs (glyph IDs + positions) into rendered
/// pixels on a tiny-skia pixmap.
///
/// # Example
///
/// ```rust,ignore
/// let mut rasterizer = TextRasterizer::new();
///
/// // Render a shaped run
/// rasterizer.render_shaped_run(
///     &shaped_run,
///     10.0,   // x position
///     100.0,  // baseline y position
///     Color::BLACK,
///     &mut pixmap,
/// )?;
/// ```
///
/// # Thread Safety
///
/// TextRasterizer is not thread-safe (uses internal mutable cache).
/// Create one instance per thread, or use external synchronization.
#[derive(Debug, Default)]
pub struct TextRasterizer {
    /// Glyph path cache
    cache: GlyphCache,
}

impl TextRasterizer {
    /// Creates a new text rasterizer.
    pub fn new() -> Self {
        Self {
            cache: GlyphCache::new(),
        }
    }

    /// Creates a rasterizer with custom cache capacity.
    pub fn with_cache_capacity(capacity: usize) -> Self {
        Self {
            cache: GlyphCache::with_capacity(capacity),
        }
    }

    /// Renders a shaped text run to a pixmap.
    ///
    /// # Arguments
    ///
    /// * `run` - The shaped run containing glyphs and font
    /// * `x` - X position for the start of the run
    /// * `baseline_y` - Y position of the text baseline
    /// * `color` - Text fill color
    /// * `pixmap` - Target pixmap to render to
    ///
    /// # Returns
    ///
    /// The total horizontal advance (width) of the rendered text.
    ///
    /// # Example
    ///
    /// ```rust,ignore
    /// let advance = rasterizer.render_shaped_run(
    ///     &run,
    ///     100.0,      // x
    ///     200.0,      // baseline y
    ///     Color::BLACK,
    ///     &mut pixmap,
    /// )?;
    /// println!("Rendered {} pixels wide", advance);
    /// ```
    pub fn render_shaped_run(
        &mut self,
        run: &ShapedRun,
        x: f32,
        baseline_y: f32,
        color: Color,
        pixmap: &mut Pixmap,
    ) -> Result<f32> {
        // Create paint with text color
        let mut paint = Paint::default();
        paint.set_color_rgba8(color.r, color.g, color.b, color.a);
        paint.anti_alias = true;

        let mut cursor_x = x;

        // Render each glyph
        for glyph in &run.glyphs {
            // Calculate glyph position
            let glyph_x = cursor_x + glyph.x_offset;
            let glyph_y = baseline_y + glyph.y_offset;

            // Get or build glyph path
            if let Some(path) = self
                .cache
                .get_or_build(&run.font, glyph.glyph_id, run.font_size, glyph_x, glyph_y)
            {
                // Render the path
                pixmap.fill_path(&path, &paint, FillRule::Winding, Transform::identity(), None);
            }

            // Advance cursor (x_offset is already applied, x_advance is the main movement)
            cursor_x += glyph.x_advance;
        }

        Ok(run.advance)
    }

    /// Renders multiple shaped runs.
    ///
    /// Convenience method for rendering a line of text with multiple runs.
    ///
    /// # Arguments
    ///
    /// * `runs` - Slice of shaped runs to render
    /// * `x` - X position for the start
    /// * `baseline_y` - Y position of the text baseline
    /// * `color` - Text fill color
    /// * `pixmap` - Target pixmap
    ///
    /// # Returns
    ///
    /// Total horizontal advance of all runs.
    pub fn render_runs(
        &mut self,
        runs: &[ShapedRun],
        x: f32,
        baseline_y: f32,
        color: Color,
        pixmap: &mut Pixmap,
    ) -> Result<f32> {
        let mut cursor_x = x;

        for run in runs {
            let advance = self.render_shaped_run(run, cursor_x, baseline_y, color, pixmap)?;
            cursor_x += advance;
        }

        Ok(cursor_x - x)
    }

    /// Renders text with a specific font (low-level API).
    ///
    /// This is a lower-level method that renders individual glyph positions.
    /// Most users should use `render_shaped_run` instead.
    ///
    /// # Arguments
    ///
    /// * `glyphs` - Slice of glyph positions to render
    /// * `font` - Font to use for glyph outlines
    /// * `font_size` - Font size in pixels
    /// * `x` - X position
    /// * `baseline_y` - Baseline Y position
    /// * `color` - Fill color
    /// * `pixmap` - Target pixmap
    pub fn render_glyphs(
        &mut self,
        glyphs: &[GlyphPosition],
        font: &LoadedFont,
        font_size: f32,
        x: f32,
        baseline_y: f32,
        color: Color,
        pixmap: &mut Pixmap,
    ) -> Result<f32> {
        let mut paint = Paint::default();
        paint.set_color_rgba8(color.r, color.g, color.b, color.a);
        paint.anti_alias = true;

        let mut cursor_x = x;

        for glyph in glyphs {
            let glyph_x = cursor_x + glyph.x_offset;
            let glyph_y = baseline_y + glyph.y_offset;

            if let Some(path) = self
                .cache
                .get_or_build(font, glyph.glyph_id, font_size, glyph_x, glyph_y)
            {
                pixmap.fill_path(&path, &paint, FillRule::Winding, Transform::identity(), None);
            }

            cursor_x += glyph.x_advance;
        }

        Ok(cursor_x - x)
    }

    /// Clears the glyph cache.
    ///
    /// Call this when fonts are unloaded or memory pressure is high.
    pub fn clear_cache(&mut self) {
        self.cache.clear();
    }

    /// Returns the number of cached glyph paths.
    #[inline]
    pub fn cache_size(&self) -> usize {
        self.cache.len()
    }
}

// ============================================================================
// Utility Functions
// ============================================================================

/// Converts a Color to a tiny-skia color.
#[inline]
pub fn to_skia_color(color: Color) -> tiny_skia::Color {
    tiny_skia::Color::from_rgba8(color.r, color.g, color.b, color.a)
}

/// Renders a single glyph to a pixmap (standalone function).
///
/// This is a convenience function for simple use cases.
/// For rendering multiple glyphs, use `TextRasterizer` for caching benefits.
///
/// # Arguments
///
/// * `font` - The font containing the glyph
/// * `glyph_id` - ID of the glyph to render
/// * `font_size` - Font size in pixels
/// * `x` - X position
/// * `y` - Baseline Y position
/// * `color` - Fill color
/// * `pixmap` - Target pixmap
///
/// # Returns
///
/// The glyph's horizontal advance, or an error if rendering fails.
pub fn render_glyph(
    font: &LoadedFont,
    glyph_id: u32,
    font_size: f32,
    x: f32,
    y: f32,
    color: Color,
    pixmap: &mut Pixmap,
) -> Result<f32> {
    // Parse font
    let face = font.as_ttf_face().map_err(|_| RenderError::RasterizationFailed {
        reason: format!("Failed to parse font: {}", font.family),
    })?;

    // Calculate scale
    let units_per_em = face.units_per_em() as f32;
    let scale = font_size / units_per_em;

    // Build path
    let mut builder = GlyphOutlineBuilder::new(scale, x, y);
    let glyph = ttf_parser::GlyphId(glyph_id as u16);

    if face.outline_glyph(glyph, &mut builder).is_none() {
        // Glyph has no outline (e.g., space character)
        // Return advance without rendering
        if let Some(advance) = face.glyph_hor_advance(glyph) {
            return Ok(advance as f32 * scale);
        }
        return Ok(0.0);
    }

    // Render path
    if let Some(path) = builder.finish() {
        let mut paint = Paint::default();
        paint.set_color_rgba8(color.r, color.g, color.b, color.a);
        paint.anti_alias = true;

        pixmap.fill_path(&path, &paint, FillRule::Winding, Transform::identity(), None);
    }

    // Return advance
    if let Some(advance) = face.glyph_hor_advance(glyph) {
        Ok(advance as f32 * scale)
    } else {
        Ok(0.0)
    }
}

/// Gets the horizontal advance for a glyph.
///
/// Returns the distance to move the cursor after this glyph.
pub fn glyph_advance(font: &LoadedFont, glyph_id: u32, font_size: f32) -> Result<f32> {
    let face = font.as_ttf_face().map_err(|_| RenderError::RasterizationFailed {
        reason: format!("Failed to parse font: {}", font.family),
    })?;

    let units_per_em = face.units_per_em() as f32;
    let scale = font_size / units_per_em;

    let glyph = ttf_parser::GlyphId(glyph_id as u16);

    if let Some(advance) = face.glyph_hor_advance(glyph) {
        Ok(advance as f32 * scale)
    } else {
        Ok(0.0)
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::text::font_loader::FontContext;

    fn get_test_font() -> Option<LoadedFont> {
        let ctx = FontContext::new();
        ctx.get_sans_serif()
    }

    #[test]
    fn test_glyph_cache_creation() {
        let cache = GlyphCache::new();
        assert!(cache.is_empty());
        assert_eq!(cache.len(), 0);
    }

    #[test]
    fn test_glyph_cache_with_capacity() {
        let cache = GlyphCache::with_capacity(100);
        assert!(cache.is_empty());
    }

    #[test]
    fn test_text_rasterizer_creation() {
        let rasterizer = TextRasterizer::new();
        assert_eq!(rasterizer.cache_size(), 0);
    }

    #[test]
    fn test_text_rasterizer_with_capacity() {
        let rasterizer = TextRasterizer::with_cache_capacity(500);
        assert_eq!(rasterizer.cache_size(), 0);
    }

    #[test]
    fn test_render_glyph_basic() {
        let font = match get_test_font() {
            Some(f) => f,
            None => return, // Skip if no fonts available
        };

        let mut pixmap = Pixmap::new(100, 100).unwrap();
        pixmap.fill(tiny_skia::Color::WHITE);

        // Get glyph ID for 'A'
        let face = font.as_ttf_face().unwrap();
        let glyph_id = face.glyph_index('A').map(|g| g.0 as u32).unwrap_or(0);

        // Render the glyph
        let result = render_glyph(&font, glyph_id, 16.0, 10.0, 80.0, Color::BLACK, &mut pixmap);

        assert!(result.is_ok());
        let advance = result.unwrap();
        assert!(advance > 0.0);
    }

    #[test]
    fn test_render_glyph_space() {
        let font = match get_test_font() {
            Some(f) => f,
            None => return,
        };

        let mut pixmap = Pixmap::new(100, 100).unwrap();
        pixmap.fill(tiny_skia::Color::WHITE);

        // Get glyph ID for space
        let face = font.as_ttf_face().unwrap();
        let glyph_id = face.glyph_index(' ').map(|g| g.0 as u32).unwrap_or(0);

        // Render (should succeed even though space has no outline)
        let result = render_glyph(&font, glyph_id, 16.0, 10.0, 80.0, Color::BLACK, &mut pixmap);

        assert!(result.is_ok());
        // Space should have positive advance
        let advance = result.unwrap();
        assert!(advance > 0.0);
    }

    #[test]
    fn test_glyph_advance() {
        let font = match get_test_font() {
            Some(f) => f,
            None => return,
        };

        let face = font.as_ttf_face().unwrap();
        let glyph_id = face.glyph_index('A').map(|g| g.0 as u32).unwrap_or(0);

        let advance = glyph_advance(&font, glyph_id, 16.0);
        assert!(advance.is_ok());
        assert!(advance.unwrap() > 0.0);
    }

    #[test]
    fn test_text_rasterizer_render_glyphs() {
        let font = match get_test_font() {
            Some(f) => f,
            None => return,
        };

        let face = font.as_ttf_face().unwrap();

        // Create some test glyphs
        let glyphs: Vec<GlyphPosition> = "ABC"
            .chars()
            .enumerate()
            .filter_map(|(i, c)| {
                let glyph_id = face.glyph_index(c)?.0 as u32;
                Some(GlyphPosition {
                    glyph_id,
                    cluster: i as u32,
                    x_offset: 0.0,
                    y_offset: 0.0,
                    x_advance: 10.0, // Approximate
                    y_advance: 0.0,
                })
            })
            .collect();

        let mut pixmap = Pixmap::new(200, 100).unwrap();
        pixmap.fill(tiny_skia::Color::WHITE);

        let mut rasterizer = TextRasterizer::new();
        let result = rasterizer.render_glyphs(&glyphs, &font, 16.0, 10.0, 80.0, Color::BLACK, &mut pixmap);

        assert!(result.is_ok());
    }

    #[test]
    fn test_to_skia_color() {
        let color = Color {
            r: 255,
            g: 128,
            b: 64,
            a: 200,
        };
        let skia_color = to_skia_color(color);

        // tiny-skia Color methods return f32 in 0.0-1.0 range
        // from_rgba8 creates premultiplied colors, so values are normalized
        assert!(skia_color.red() > 0.0);
        assert!(skia_color.green() > 0.0);
        assert!(skia_color.blue() > 0.0);
        assert!(skia_color.alpha() > 0.0);
    }

    #[test]
    fn test_glyph_cache_key() {
        let font = match get_test_font() {
            Some(f) => f,
            None => return,
        };

        let key1 = GlyphCacheKey::new(&font, 65, 16.0);
        let key2 = GlyphCacheKey::new(&font, 65, 16.0);
        let key3 = GlyphCacheKey::new(&font, 66, 16.0);
        let key4 = GlyphCacheKey::new(&font, 65, 24.0);

        // Same font, glyph, and size should be equal
        assert_eq!(key1, key2);

        // Different glyph should be different
        assert_ne!(key1, key3);

        // Different size should be different
        assert_ne!(key1, key4);
    }

    #[test]
    fn test_glyph_cache_clear() {
        let mut cache = GlyphCache::new();
        cache.clear();
        assert!(cache.is_empty());
    }

    #[test]
    fn test_text_rasterizer_clear_cache() {
        let mut rasterizer = TextRasterizer::new();
        rasterizer.clear_cache();
        assert_eq!(rasterizer.cache_size(), 0);
    }

    #[test]
    fn test_color_black() {
        let black = Color::BLACK;
        assert_eq!(black.r, 0);
        assert_eq!(black.g, 0);
        assert_eq!(black.b, 0);
        assert_eq!(black.a, 255);
    }

    #[test]
    fn test_glyph_outline_builder_transform() {
        let builder = GlyphOutlineBuilder::new(1.0, 100.0, 200.0);

        // X should add offset
        assert_eq!(builder.transform_x(0.0), 100.0);
        assert_eq!(builder.transform_x(50.0), 150.0);

        // Y should invert and add baseline
        assert_eq!(builder.transform_y(0.0), 200.0);
        assert_eq!(builder.transform_y(50.0), 150.0); // 200 - 50
    }

    #[test]
    fn test_glyph_outline_builder_scale() {
        let builder = GlyphOutlineBuilder::new(0.5, 0.0, 100.0);

        // With scale 0.5, 100 font units = 50 pixels
        assert_eq!(builder.transform_x(100.0), 50.0);
        assert_eq!(builder.transform_y(100.0), 50.0); // 100 - 50
    }
}
