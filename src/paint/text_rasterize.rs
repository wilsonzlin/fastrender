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
//!     Rgba::BLACK,
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

use crate::error::RenderError;
use crate::error::Result;
use crate::style::color::Rgba;
use crate::text::font_db::LoadedFont;
use crate::text::pipeline::GlyphPosition;
use crate::text::pipeline::ShapedRun;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::sync::Arc;
use tiny_skia::FillRule;
use tiny_skia::Paint;
use tiny_skia::Path;
use tiny_skia::PathBuilder;
use tiny_skia::Pixmap;
use tiny_skia::Transform;

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
  /// Number of outline commands
  verb_count: usize,
  /// Number of points added to the outline
  point_count: usize,
}

impl GlyphOutlineBuilder {
  /// Creates a new outline builder.
  ///
  /// # Arguments
  ///
  /// Outlines are recorded in font design units with no positioning
  /// or scaling. The caller is responsible for applying any required
  /// transforms when rasterizing the path.
  fn new() -> Self {
    Self {
      builder: PathBuilder::new(),
      verb_count: 0,
      point_count: 0,
    }
  }

  /// Consumes the builder and returns the completed path and metrics.
  fn finish(self) -> (Option<Path>, GlyphOutlineMetrics) {
    (
      self.builder.finish(),
      GlyphOutlineMetrics {
        verb_count: self.verb_count,
        point_count: self.point_count,
      },
    )
  }
}

impl ttf_parser::OutlineBuilder for GlyphOutlineBuilder {
  fn move_to(&mut self, x: f32, y: f32) {
    self.verb_count += 1;
    self.point_count += 1;
    self.builder.move_to(x, y);
  }

  fn line_to(&mut self, x: f32, y: f32) {
    self.verb_count += 1;
    self.point_count += 1;
    self.builder.line_to(x, y);
  }

  fn quad_to(&mut self, x1: f32, y1: f32, x: f32, y: f32) {
    self.verb_count += 1;
    self.point_count += 2;
    self.builder.quad_to(x1, y1, x, y);
  }

  fn curve_to(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, x: f32, y: f32) {
    self.verb_count += 1;
    self.point_count += 3;
    self.builder.cubic_to(x1, y1, x2, y2, x, y);
  }

  fn close(&mut self) {
    self.verb_count += 1;
    self.builder.close();
  }
}

/// Simple metrics captured while building an outline.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
struct GlyphOutlineMetrics {
  verb_count: usize,
  point_count: usize,
}

// ============================================================================
// Glyph Cache
// ============================================================================

/// Cache key for glyph paths.
///
/// Glyphs are cached by font data pointer + glyph ID + skew.
/// Using the font data pointer avoids comparing large font binaries.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct GlyphCacheKey {
  /// Pointer to font data (used as unique identifier)
  font_ptr: usize,
  /// Font face index within the file
  font_index: u32,
  /// Glyph ID within the font
  glyph_id: u32,
  /// Quantized synthetic skew (tan(angle) * 1e4)
  skew_units: i32,
}

impl GlyphCacheKey {
  fn new(font: &LoadedFont, glyph_id: u32, skew: f32) -> Self {
    Self {
      font_ptr: Arc::as_ptr(&font.data) as usize,
      font_index: font.index,
      glyph_id,
      skew_units: quantize_skew(skew),
    }
  }
}

/// Cached glyph path for efficient repeated rendering.
///
/// Contains a pre-built path that can be reused across multiple
/// render calls at the same font size.
/// (Reserved for glyph caching optimization)
#[derive(Debug, Clone)]
struct CachedGlyph {
  /// The rendered path, or None if the glyph has no outline
  path: Option<Path>,
  /// Horizontal advance for this glyph
  advance: f32,
  /// LRU timestamp (monotonic counter)
  last_used: u64,
  /// Rough estimate of memory usage for budgeting/eviction
  estimated_size: usize,
}

/// Lightweight cache metrics for profiling.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct GlyphCacheStats {
  /// Number of cache hits (outline reused)
  pub hits: u64,
  /// Number of cache misses (outline had to be built)
  pub misses: u64,
  /// Number of entries evicted by the cache policy
  pub evictions: u64,
}

/// Cache for rendered glyph paths.
///
/// Glyph outlines are cached in font design units (unpositioned) and
/// transformed at draw time. The cache key includes the font identity,
/// face index, glyph id, and synthetic oblique skew.
///
/// The cache uses an LRU eviction policy with an optional memory budget
/// to keep footprint predictable while still delivering reuse on text-
/// heavy pages.
#[derive(Debug)]
pub struct GlyphCache {
  /// Cached glyph paths
  glyphs: HashMap<GlyphCacheKey, CachedGlyph>,
  /// Usage order for LRU eviction (key + generation)
  usage_queue: VecDeque<(GlyphCacheKey, u64)>,
  /// Maximum cache size
  max_size: usize,
  /// Optional memory budget (bytes) for cached paths
  max_bytes: Option<usize>,
  /// Current estimated memory used by cached paths
  current_bytes: usize,
  /// Monotonic counter used for LRU
  generation: u64,
  /// Cache hit count (for profiling)
  hits: u64,
  /// Cache miss count (for profiling)
  misses: u64,
  /// Number of evicted glyphs
  evictions: u64,
}

impl Default for GlyphCache {
  fn default() -> Self {
    Self::new()
  }
}

impl GlyphCache {
  /// Creates a new glyph cache with default size.
  pub fn new() -> Self {
    let max_size = 2048;
    Self {
      glyphs: HashMap::new(),
      max_size,
      max_bytes: None,
      usage_queue: VecDeque::new(),
      current_bytes: 0,
      generation: 0,
      hits: 0,
      misses: 0,
      evictions: 0,
    }
  }

  /// Creates a cache with custom maximum size.
  pub fn with_capacity(max_size: usize) -> Self {
    let max_size = max_size.max(1);
    Self {
      glyphs: HashMap::with_capacity(max_size.min(256)),
      max_size,
      max_bytes: None,
      usage_queue: VecDeque::new(),
      current_bytes: 0,
      generation: 0,
      hits: 0,
      misses: 0,
      evictions: 0,
    }
  }

  /// Creates a cache with both glyph count and memory budget.
  pub fn with_limits(max_size: usize, max_bytes: Option<usize>) -> Self {
    let max_size = max_size.max(1);
    Self {
      glyphs: HashMap::with_capacity(max_size.min(256)),
      max_size,
      max_bytes,
      usage_queue: VecDeque::new(),
      current_bytes: 0,
      generation: 0,
      hits: 0,
      misses: 0,
      evictions: 0,
    }
  }

  /// Updates the maximum number of cached glyphs.
  pub fn set_max_size(&mut self, max_size: usize) {
    self.max_size = max_size.max(1);
    self.evict_if_needed();
  }

  /// Sets an optional memory budget (in bytes) for cached glyphs.
  pub fn set_max_bytes(&mut self, max_bytes: Option<usize>) {
    self.max_bytes = max_bytes;
    self.evict_if_needed();
  }

  /// Gets a cached glyph path or builds and caches it.
  fn get_or_build(&mut self, font: &LoadedFont, glyph_id: u32, skew: f32) -> Option<&CachedGlyph> {
    let key = GlyphCacheKey::new(font, glyph_id, skew);
    let generation = self.bump_generation();

    if let Some(entry) = self.glyphs.get_mut(&key) {
      self.hits += 1;
      entry.last_used = generation;
      self.usage_queue.push_back((key, generation));
      return Some(entry);
    }

    self.misses += 1;
    let mut cached = self.build_glyph_path(font, glyph_id)?;
    cached.last_used = generation;
    self.current_bytes = self.current_bytes.saturating_add(cached.estimated_size);
    self.glyphs.insert(key, cached);
    self.usage_queue.push_back((key, generation));
    self.evict_if_needed();

    self.glyphs.get(&key)
  }

  /// Builds a glyph path without caching.
  fn build_glyph_path(&self, font: &LoadedFont, glyph_id: u32) -> Option<CachedGlyph> {
    // Parse font face
    let face = font.as_ttf_face().ok()?;
    let mut builder = GlyphOutlineBuilder::new();

    let glyph_id = ttf_parser::GlyphId(glyph_id as u16);
    let has_outline = face.outline_glyph(glyph_id, &mut builder).is_some();
    let (path, metrics) = builder.finish();

    let advance = face
      .glyph_hor_advance(glyph_id)
      .map(|v| v as f32)
      .unwrap_or(0.0);

    let estimated_size = if has_outline {
      estimate_glyph_size(&metrics)
    } else {
      0
    };

    Some(CachedGlyph {
      path: if has_outline { path } else { None },
      advance,
      last_used: 0,
      estimated_size,
    })
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
    self.usage_queue.clear();
    self.current_bytes = 0;
  }

  /// Returns cache statistics (hits, misses, evictions).
  pub fn stats(&self) -> GlyphCacheStats {
    GlyphCacheStats {
      hits: self.hits,
      misses: self.misses,
      evictions: self.evictions,
    }
  }

  /// Resets cache statistics without clearing cached glyphs.
  pub fn reset_stats(&mut self) {
    self.hits = 0;
    self.misses = 0;
    self.evictions = 0;
  }

  /// Bumps the generation counter used for LRU ordering.
  fn bump_generation(&mut self) -> u64 {
    self.generation = self.generation.wrapping_add(1);
    self.generation
  }

  /// Evicts old entries if cache is full.
  fn evict_if_needed(&mut self) {
    let max_bytes = self.max_bytes.unwrap_or(usize::MAX);

    while self.glyphs.len() > self.max_size || self.current_bytes > max_bytes {
      if let Some((key, generation)) = self.usage_queue.pop_front() {
        if let Some(entry) = self.glyphs.get(&key) {
          if entry.last_used == generation {
            let removed = self.glyphs.remove(&key).unwrap();
            self.current_bytes = self.current_bytes.saturating_sub(removed.estimated_size);
            self.evictions += 1;
          }
        }
      } else {
        break;
      }
    }
  }
}

fn quantize_skew(skew: f32) -> i32 {
  if !skew.is_finite() {
    return 0;
  }

  let scaled = (skew * 10_000.0).round();
  scaled.clamp(i32::MIN as f32, i32::MAX as f32).trunc() as i32
}

fn estimate_glyph_size(metrics: &GlyphOutlineMetrics) -> usize {
  let point_bytes = metrics.point_count * std::mem::size_of::<tiny_skia::Point>();
  // Each verb typically expands to a few bytes; use a small constant
  // so the estimate scales with command count without relying on
  // private tiny-skia details.
  let verb_bytes = metrics.verb_count * std::mem::size_of::<u8>();
  point_bytes.saturating_add(verb_bytes)
}

fn glyph_transform(scale: f32, skew: f32, x: f32, y: f32) -> Transform {
  Transform::from_row(scale, 0.0, skew * scale, -scale, x, y)
}

fn rotation_transform(
  rotation: crate::text::pipeline::RunRotation,
  origin_x: f32,
  origin_y: f32,
) -> Option<Transform> {
  let angle = match rotation {
    crate::text::pipeline::RunRotation::Ccw90 => -90.0_f32.to_radians(),
    crate::text::pipeline::RunRotation::Cw90 => 90.0_f32.to_radians(),
    crate::text::pipeline::RunRotation::None => return None,
  };

  let (sin, cos) = angle.sin_cos();
  // Rotate around the provided origin, matching previous behavior that
  // rotated around the run start and baseline position.
  let tx = origin_x - origin_x * cos + origin_y * sin;
  let ty = origin_y - origin_x * sin - origin_y * cos;
  Some(Transform::from_row(cos, sin, -sin, cos, tx, ty))
}

fn concat_transforms(a: Transform, b: Transform) -> Transform {
  Transform::from_row(
    a.sx * b.sx + a.kx * b.ky,
    a.ky * b.sx + a.sy * b.ky,
    a.sx * b.kx + a.kx * b.sy,
    a.ky * b.kx + a.sy * b.sy,
    a.sx * b.tx + a.kx * b.ty + a.tx,
    a.ky * b.tx + a.sy * b.ty + a.ty,
  )
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
///     Rgba::BLACK,
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
  ///     Rgba::BLACK,
  ///     &mut pixmap,
  /// )?;
  /// println!("Rendered {} pixels wide", advance);
  /// ```
  pub fn render_shaped_run(
    &mut self,
    run: &ShapedRun,
    x: f32,
    baseline_y: f32,
    color: Rgba,
    pixmap: &mut Pixmap,
  ) -> Result<f32> {
    // Create paint with text color
    let mut paint = Paint::default();
    paint.set_color_rgba8(color.r, color.g, color.b, color.alpha_u8());
    paint.anti_alias = true;

    let face = run
      .font
      .as_ttf_face()
      .map_err(|_| RenderError::RasterizationFailed {
        reason: format!("Failed to parse font: {}", run.font.family),
      })?;

    let units_per_em = face.units_per_em() as f32;
    if units_per_em == 0.0 {
      return Err(
        RenderError::RasterizationFailed {
          reason: format!("Font {} has invalid units_per_em", run.font.family),
        }
        .into(),
      );
    }

    let scale = run.font_size * run.scale / units_per_em;
    let rotation = rotation_transform(run.rotation, x, baseline_y);
    let mut cursor_x = x;

    // Render each glyph
    for glyph in &run.glyphs {
      // Calculate glyph position
      let glyph_x = cursor_x + glyph.x_offset;
      let glyph_y = baseline_y + glyph.y_offset;

      // Get or build glyph path
      if let Some(cached) =
        self
          .cache
          .get_or_build(&run.font, glyph.glyph_id, run.synthetic_oblique)
      {
        if let Some(path) = cached.path.as_ref() {
          let mut transform = glyph_transform(scale, run.synthetic_oblique, glyph_x, glyph_y);
          if let Some(rotation) = rotation {
            transform = concat_transforms(rotation, transform);
          }

          // Render the path
          pixmap.fill_path(&path, &paint, FillRule::Winding, transform, None);
          if run.synthetic_bold > 0.0 {
            let mut stroke = tiny_skia::Stroke::default();
            stroke.width = run.synthetic_bold * 2.0;
            stroke.line_join = tiny_skia::LineJoin::Round;
            stroke.line_cap = tiny_skia::LineCap::Round;
            pixmap.stroke_path(&path, &paint, &stroke, transform, None);
          }
        }
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
    color: Rgba,
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
    color: Rgba,
    pixmap: &mut Pixmap,
  ) -> Result<f32> {
    let mut paint = Paint::default();
    paint.set_color_rgba8(color.r, color.g, color.b, color.alpha_u8());
    paint.anti_alias = true;

    let face = font
      .as_ttf_face()
      .map_err(|_| RenderError::RasterizationFailed {
        reason: format!("Failed to parse font: {}", font.family),
      })?;

    let units_per_em = face.units_per_em() as f32;
    if units_per_em == 0.0 {
      return Err(
        RenderError::RasterizationFailed {
          reason: format!("Font {} has invalid units_per_em", font.family),
        }
        .into(),
      );
    }

    let scale = font_size / units_per_em;
    let mut cursor_x = x;

    for glyph in glyphs {
      let glyph_x = cursor_x + glyph.x_offset;
      let glyph_y = baseline_y + glyph.y_offset;

      if let Some(cached) = self.cache.get_or_build(font, glyph.glyph_id, 0.0) {
        if let Some(path) = cached.path.as_ref() {
          let transform = glyph_transform(scale, 0.0, glyph_x, glyph_y);
          pixmap.fill_path(&path, &paint, FillRule::Winding, transform, None);
        }
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

  /// Sets the maximum number of cached glyph outlines.
  pub fn set_cache_capacity(&mut self, max_glyphs: usize) {
    self.cache.set_max_size(max_glyphs);
  }

  /// Sets an optional memory budget (in bytes) for cached outlines.
  pub fn set_cache_memory_budget(&mut self, max_bytes: Option<usize>) {
    self.cache.set_max_bytes(max_bytes);
  }

  /// Returns the number of cached glyph paths.
  #[inline]
  pub fn cache_size(&self) -> usize {
    self.cache.len()
  }

  /// Returns glyph cache statistics (hits/misses/evictions).
  #[inline]
  pub fn cache_stats(&self) -> GlyphCacheStats {
    self.cache.stats()
  }

  /// Resets cache statistics without dropping cached outlines.
  pub fn reset_cache_stats(&mut self) {
    self.cache.reset_stats();
  }
}

// ============================================================================
// Utility Functions
// ============================================================================

/// Converts an Rgba to a tiny-skia color.
#[inline]
pub fn to_skia_color(color: Rgba) -> tiny_skia::Color {
  tiny_skia::Color::from_rgba8(color.r, color.g, color.b, color.alpha_u8())
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
  color: Rgba,
  pixmap: &mut Pixmap,
) -> Result<f32> {
  // Parse font
  let face = font
    .as_ttf_face()
    .map_err(|_| RenderError::RasterizationFailed {
      reason: format!("Failed to parse font: {}", font.family),
    })?;

  // Calculate scale
  let units_per_em = face.units_per_em() as f32;
  let scale = font_size / units_per_em;

  // Build path in font units and transform at draw time
  let mut builder = GlyphOutlineBuilder::new();
  let glyph = ttf_parser::GlyphId(glyph_id as u16);

  if face.outline_glyph(glyph, &mut builder).is_none() {
    // Glyph has no outline (e.g., space character)
    // Return advance without rendering
    if let Some(advance) = face.glyph_hor_advance(glyph) {
      return Ok(advance as f32 * scale);
    }
    return Ok(0.0);
  }

  let (path, _) = builder.finish();

  // Render path
  if let Some(path) = path {
    let mut paint = Paint::default();
    paint.set_color_rgba8(color.r, color.g, color.b, color.alpha_u8());
    paint.anti_alias = true;

    let transform = glyph_transform(scale, 0.0, x, y);

    pixmap.fill_path(&path, &paint, FillRule::Winding, transform, None);
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
  let face = font
    .as_ttf_face()
    .map_err(|_| RenderError::RasterizationFailed {
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
    let result = render_glyph(&font, glyph_id, 16.0, 10.0, 80.0, Rgba::BLACK, &mut pixmap);

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
    let result = render_glyph(&font, glyph_id, 16.0, 10.0, 80.0, Rgba::BLACK, &mut pixmap);

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
    let result =
      rasterizer.render_glyphs(&glyphs, &font, 16.0, 10.0, 80.0, Rgba::BLACK, &mut pixmap);

    assert!(result.is_ok());
  }

  #[test]
  fn test_to_skia_color() {
    let color = Rgba::from_rgba8(255, 128, 64, 200);
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

    let key1 = GlyphCacheKey::new(&font, 65, 0.0);
    let key2 = GlyphCacheKey::new(&font, 65, 0.0);
    let key3 = GlyphCacheKey::new(&font, 66, 0.0);
    let key4 = GlyphCacheKey::new(&font, 65, 0.25);

    // Same font, glyph, and size should be equal
    assert_eq!(key1, key2);

    // Different glyph should be different
    assert_ne!(key1, key3);

    // Different skew should be different
    assert_ne!(key1, key4);
  }

  #[test]
  fn test_glyph_cache_hit_miss_and_eviction() {
    let font = match get_test_font() {
      Some(f) => f,
      None => return,
    };

    let face = font.as_ttf_face().unwrap();
    let glyph_a = face.glyph_index('A').map(|g| g.0 as u32).unwrap_or(0);
    let glyph_b = face.glyph_index('B').map(|g| g.0 as u32).unwrap_or(1);

    let mut cache = GlyphCache::with_capacity(1);

    assert!(cache.get_or_build(&font, glyph_a, 0.0).is_some());
    let stats = cache.stats();
    assert_eq!(stats.misses, 1);
    assert_eq!(stats.hits, 0);

    assert!(cache.get_or_build(&font, glyph_a, 0.0).is_some());
    let stats = cache.stats();
    assert_eq!(stats.hits, 1);

    // Insert another glyph to trigger eviction
    assert!(cache.get_or_build(&font, glyph_b, 0.0).is_some());
    let stats = cache.stats();
    assert!(stats.evictions >= 1);
    assert!(cache.len() <= 1);
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
  fn test_text_rasterizer_cache_hits_on_reuse() {
    let font = match get_test_font() {
      Some(f) => f,
      None => return,
    };

    let face = font.as_ttf_face().unwrap();
    let glyph_id = face.glyph_index('A').map(|g| g.0 as u32).unwrap_or(0);

    let glyphs = vec![GlyphPosition {
      glyph_id,
      cluster: 0,
      x_offset: 0.0,
      y_offset: 0.0,
      x_advance: 10.0,
      y_advance: 0.0,
    }];

    let mut rasterizer = TextRasterizer::new();
    rasterizer.reset_cache_stats();

    let mut pixmap = Pixmap::new(50, 50).unwrap();
    rasterizer
      .render_glyphs(&glyphs, &font, 16.0, 10.0, 35.0, Rgba::BLACK, &mut pixmap)
      .unwrap();

    let mut pixmap2 = Pixmap::new(50, 50).unwrap();
    rasterizer
      .render_glyphs(&glyphs, &font, 16.0, 20.0, 40.0, Rgba::BLACK, &mut pixmap2)
      .unwrap();

    let stats = rasterizer.cache_stats();
    assert_eq!(stats.misses, 1);
    assert!(stats.hits >= 1);
  }

  #[test]
  fn test_color_black() {
    let black = Rgba::BLACK;
    assert_eq!(black.r, 0);
    assert_eq!(black.g, 0);
    assert_eq!(black.b, 0);
    assert_eq!(black.a, 1.0);
  }

  #[test]
  fn test_glyph_outline_builder_metrics() {
    use ttf_parser::OutlineBuilder;

    let mut builder = GlyphOutlineBuilder::new();
    OutlineBuilder::move_to(&mut builder, 0.0, 0.0);
    OutlineBuilder::line_to(&mut builder, 10.0, 0.0);
    OutlineBuilder::quad_to(&mut builder, 15.0, 5.0, 20.0, 0.0);
    OutlineBuilder::curve_to(&mut builder, 20.0, 5.0, 25.0, 5.0, 30.0, 0.0);
    OutlineBuilder::close(&mut builder);

    let (_path, metrics) = builder.finish();
    assert_eq!(metrics.verb_count, 5);
    assert_eq!(metrics.point_count, 7);
  }

  #[test]
  fn test_glyph_transform_matrix() {
    let transform = glyph_transform(2.0, 0.25, 10.0, 20.0);
    assert!((transform.sx - 2.0).abs() < 1e-6);
    assert!((transform.kx - 0.5).abs() < 1e-6);
    assert!((transform.sy + 2.0).abs() < 1e-6);
    assert_eq!(transform.tx, 10.0);
    assert_eq!(transform.ty, 20.0);
  }
}
