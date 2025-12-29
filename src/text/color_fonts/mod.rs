//! Color font helpers
//!
//! Provides color glyph rasterization for bitmap, SVG, and COLR/CPAL fonts.

mod bitmap;
mod colr_v0;
mod colr_v1;
mod cpal;
mod limits;
mod svg;

use crate::style::color::Rgba;
use crate::text::face_cache;
use crate::text::font_db::LoadedFont;
use crate::text::font_instance::FontInstance;
use crate::text::variations::apply_rustybuzz_variations;
use limits::GlyphRasterLimits;
use lru::LruCache;
use rustybuzz::Variation;
use std::num::NonZeroUsize;
use std::sync::{Arc, Mutex};

pub use bitmap::render_bitmap_glyph;
pub use bitmap::MAX_SBIX_GLYPH_BYTES;
pub use cpal::{parse_cpal_palette, select_cpal_palette, ParsedPalette};
pub use svg::render_svg_glyph;
pub use svg::sanitize_svg_glyph_for_tests;
pub use svg::MAX_SVG_GLYPH_BYTES;

/// Rasterized color glyph image with positioning relative to the glyph origin.
#[derive(Debug, Clone)]
pub struct ColorGlyphRaster {
  /// Premultiplied BGRA8 pixmap for the glyph.
  pub image: Arc<tiny_skia::Pixmap>,
  /// X offset from glyph origin to the top-left corner (pixels, Y-down).
  pub left: f32,
  /// Y offset from baseline to the top-left corner (pixels, Y-down).
  pub top: f32,
}

/// Renders color glyphs from OpenType color tables.
#[derive(Debug, Clone)]
pub struct ColorFontRenderer {
  caches: Arc<Mutex<ColorFontCaches>>,
}

impl Default for ColorFontRenderer {
  fn default() -> Self {
    Self::new()
  }
}

impl ColorFontRenderer {
  pub fn new() -> Self {
    Self {
      caches: Arc::new(Mutex::new(ColorFontCaches::new())),
    }
  }

  /// Attempts to rasterize a color glyph for the given font and glyph id.
  pub fn render(
    &self,
    font: &LoadedFont,
    instance: &FontInstance,
    glyph_id: u32,
    font_size: f32,
    palette_index: u16,
    palette_overrides: &[(u16, Rgba)],
    text_color: Rgba,
    synthetic_oblique: f32,
    variations: &[Variation],
    target_size: Option<(u32, u32)>,
  ) -> Option<ColorGlyphRaster> {
    let cached_face = face_cache::get_ttf_face(font)?;
    let mut face = cached_face.clone_face();
    apply_rustybuzz_variations(&mut face, variations);
    let gid = ttf_parser::GlyphId(glyph_id as u16);
    let font_key = FontKey::new(font);
    let limits = GlyphRasterLimits::for_target_surface(target_size);

    // Prefer embedded bitmaps (CBDT/CBLC or sbix)
    if let Some(bitmap) = bitmap::render_bitmap_glyph(&face, gid, font_size, &limits) {
      return Some(bitmap);
    }

    // SVG-in-OT
    if let Some(svg) = svg::render_svg_glyph(&face, gid, font_size, text_color, &limits) {
      return Some(svg);
    }

    // COLR v1 paint graphs
    if let Some(colr) = colr_v1::render_colr_glyph(
      font,
      instance,
      &face,
      font_key,
      gid,
      font_size,
      palette_index,
      palette_overrides,
      text_color,
      synthetic_oblique,
      &limits,
      &self.caches,
    ) {
      return Some(colr);
    }

    // COLR/CPAL (v0)
    if let Some(colr) = colr_v0::render_colr_glyph(
      instance,
      &face,
      font_key,
      gid,
      font_size,
      palette_index,
      palette_overrides,
      text_color,
      synthetic_oblique,
      &limits,
      &self.caches,
    ) {
      return Some(colr);
    }

    None
  }
}

// ----------------------------------------------------------------------------
// Shared cache plumbing
// ----------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct FontKey {
  font_ptr: usize,
  font_index: u32,
}

impl FontKey {
  fn new(font: &LoadedFont) -> Self {
    Self {
      font_ptr: Arc::as_ptr(&font.data) as usize,
      font_index: font.index,
    }
  }
}

#[derive(Debug)]
struct ColorFontCaches {
  palette_cache: LruCache<PaletteCacheKey, Option<Arc<cpal::ParsedPalette>>>,
  colr_v0_headers: LruCache<FontKey, Option<colr_v0::ColrV0Header>>,
  colr_v0_base_glyphs: LruCache<colr_v0::BaseGlyphCacheKey, Option<colr_v0::BaseGlyphRecord>>,
  colr_v1_fonts: LruCache<FontKey, Option<Arc<colr_v1::ColrV1CacheEntry>>>,
  colr_v1_base_glyphs:
    LruCache<colr_v1::BaseGlyphCacheKey, colr_v1::BaseGlyphCacheValue>,
}

impl ColorFontCaches {
  fn new() -> Self {
    Self {
      palette_cache: LruCache::new(Self::cap(64)),
      colr_v0_headers: LruCache::new(Self::cap(64)),
      colr_v0_base_glyphs: LruCache::new(Self::cap(1024)),
      colr_v1_fonts: LruCache::new(Self::cap(64)),
      colr_v1_base_glyphs: LruCache::new(Self::cap(1024)),
    }
  }

  fn cap(size: usize) -> NonZeroUsize {
    NonZeroUsize::new(size.max(1)).unwrap()
  }

  fn palette(
    &mut self,
    font_key: FontKey,
    face: &ttf_parser::Face<'_>,
    palette_index: u16,
  ) -> Option<Arc<cpal::ParsedPalette>> {
    let key = PaletteCacheKey {
      font: font_key,
      palette_index,
    };
    if let Some(palette) = self.palette_cache.get(&key) {
      return palette.clone();
    }
    let palette = face
      .raw_face()
      .table(ttf_parser::Tag::from_bytes(b"CPAL"))
      .and_then(|data| cpal::parse_cpal_palette(data, palette_index).map(Arc::new));
    self.palette_cache.put(key, palette.clone());
    palette
  }

  fn colr_v0_header(&mut self, font_key: FontKey, data: &[u8]) -> Option<colr_v0::ColrV0Header> {
    if let Some(header) = self.colr_v0_headers.get(&font_key) {
      return *header;
    }
    let header = colr_v0::parse_colr_header(data);
    self.colr_v0_headers.put(font_key, header);
    header
  }

  fn colr_v0_base_record(
    &mut self,
    key: colr_v0::BaseGlyphCacheKey,
    data: &[u8],
    header: colr_v0::ColrV0Header,
  ) -> Option<Option<colr_v0::BaseGlyphRecord>> {
    if let Some(record) = self.colr_v0_base_glyphs.get(&key) {
      return Some(*record);
    }
    let record = colr_v0::find_base_glyph(data, header, key.glyph_id);
    self.colr_v0_base_glyphs.put(key, record);
    Some(record)
  }

  fn colr_v1_font(
    &mut self,
    font_key: FontKey,
    font: &LoadedFont,
  ) -> Option<Arc<colr_v1::ColrV1CacheEntry>> {
    if let Some(entry) = self.colr_v1_fonts.get(&font_key) {
      return entry.clone();
    }
    let entry = colr_v1::ColrV1CacheEntry::parse(font);
    self.colr_v1_fonts.put(font_key, entry.clone());
    entry
  }

  fn colr_v1_base_glyph(
    &mut self,
    key: colr_v1::BaseGlyphCacheKey,
    entry: &colr_v1::ColrV1CacheEntry,
  ) -> Option<Option<colr_v1::CachedBaseGlyph>> {
    if let Some(glyph) = self.colr_v1_base_glyphs.get(&key) {
      return Some(glyph.as_ref().map(|(_, glyph)| glyph.clone()));
    }
    let glyph = entry.base_glyph(key.glyph_id);
    self.colr_v1_base_glyphs.put(key, glyph.clone());
    Some(glyph.map(|(_, glyph)| glyph))
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct PaletteCacheKey {
  font: FontKey,
  palette_index: u16,
}

// ----------------------------------------------------------------------------
// Utility parsing helpers
// ----------------------------------------------------------------------------

fn read_u16(data: &[u8], offset: usize) -> Option<u16> {
  let bytes = data.get(offset..offset + 2)?;
  Some(u16::from_be_bytes([bytes[0], bytes[1]]))
}

fn read_u24(data: &[u8], offset: usize) -> Option<u32> {
  let bytes = data.get(offset..offset + 3)?;
  Some(((bytes[0] as u32) << 16) | ((bytes[1] as u32) << 8) | bytes[2] as u32)
}

fn read_u32(data: &[u8], offset: usize) -> Option<u32> {
  let bytes = data.get(offset..offset + 4)?;
  Some(u32::from_be_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]))
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn lru_cache_hits_do_not_grow_unbounded() {
    let mut cache: LruCache<u32, u32> = LruCache::new(NonZeroUsize::new(2).unwrap());
    cache.put(1, 42);

    for _ in 0..10_000 {
      assert_eq!(cache.get(&1), Some(&42));
    }

    assert_eq!(cache.len(), 1);
  }
}
