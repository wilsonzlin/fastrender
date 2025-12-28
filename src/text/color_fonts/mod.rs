//! Color font helpers
//!
//! Provides color glyph rasterization for bitmap, SVG, and COLR/CPAL fonts.

mod bitmap;
mod colr_v0;
mod colr_v1;
mod cpal;
mod svg;

use crate::style::color::Rgba;
use crate::text::font_db::LoadedFont;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::sync::{Arc, Mutex};

pub use bitmap::render_bitmap_glyph;
pub use svg::render_svg_glyph;

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
    glyph_id: u32,
    font_size: f32,
    palette_index: u16,
    text_color: Rgba,
    synthetic_oblique: f32,
  ) -> Option<ColorGlyphRaster> {
    let face = font.as_ttf_face().ok()?;
    let gid = ttf_parser::GlyphId(glyph_id as u16);
    let font_key = FontKey::new(font);

    // Prefer embedded bitmaps (CBDT/CBLC or sbix)
    if let Some(bitmap) = bitmap::render_bitmap_glyph(&face, gid, font_size) {
      return Some(bitmap);
    }

    // SVG-in-OT
    if let Some(svg) = svg::render_svg_glyph(&face, gid, font_size, text_color) {
      return Some(svg);
    }

    // COLR v1 paint graphs
    if let Some(colr) = colr_v1::render_colr_glyph(
      font,
      &face,
      font_key,
      gid,
      font_size,
      palette_index,
      text_color,
      synthetic_oblique,
      &self.caches,
    ) {
      return Some(colr);
    }

    // COLR/CPAL (v0)
    if let Some(colr) = colr_v0::render_colr_glyph(
      &face,
      font_key,
      gid,
      font_size,
      palette_index,
      text_color,
      synthetic_oblique,
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
}

impl ColorFontCaches {
  fn new() -> Self {
    Self {
      palette_cache: LruCache::new(64),
      colr_v0_headers: LruCache::new(64),
      colr_v0_base_glyphs: LruCache::new(1024),
    }
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
    self
      .palette_cache
      .get_or_insert_with(key, || {
        let palette = face
          .raw_face()
          .table(ttf_parser::Tag::from_bytes(b"CPAL"))
          .and_then(|data| cpal::parse_cpal_palette(data, palette_index).map(Arc::new));
        Some(palette)
      })
      .and_then(|palette| palette)
  }

  fn colr_v0_header(&mut self, font_key: FontKey, data: &[u8]) -> Option<colr_v0::ColrV0Header> {
    self
      .colr_v0_headers
      .get_or_insert_with(font_key, || Some(colr_v0::parse_colr_header(data)))
      .flatten()
  }

  fn colr_v0_base_record(
    &mut self,
    key: colr_v0::BaseGlyphCacheKey,
    data: &[u8],
    header: colr_v0::ColrV0Header,
  ) -> Option<Option<colr_v0::BaseGlyphRecord>> {
    self.colr_v0_base_glyphs.get_or_insert_with(key, || {
      // Cache negative lookups too so repeated attempts avoid rescans.
      Some(colr_v0::find_base_glyph(data, header, key.glyph_id))
    })
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct PaletteCacheKey {
  font: FontKey,
  palette_index: u16,
}

#[derive(Debug)]
struct CacheEntry<V> {
  value: V,
  last_used: u64,
}

/// Lightweight, bounded LRU cache used by color font parsing.
#[derive(Debug)]
struct LruCache<K, V> {
  entries: HashMap<K, CacheEntry<V>>,
  usage: VecDeque<(K, u64)>,
  capacity: usize,
  generation: u64,
}

impl<K, V> LruCache<K, V>
where
  K: Eq + std::hash::Hash + Copy,
  V: Clone,
{
  fn new(capacity: usize) -> Self {
    let capacity = capacity.max(1);
    Self {
      entries: HashMap::with_capacity(capacity.min(128)),
      usage: VecDeque::new(),
      capacity,
      generation: 0,
    }
  }

  fn get(&mut self, key: &K) -> Option<V> {
    let generation = self.bump_generation();
    let result = self.entries.get_mut(key).map(|entry| {
      entry.last_used = generation;
      entry.value.clone()
    });

    if result.is_some() {
      self.usage.push_back((*key, generation));
      self.evict_if_needed();
    }

    result
  }

  fn insert(&mut self, key: K, value: V) -> V {
    let generation = self.bump_generation();
    self.entries.insert(
      key,
      CacheEntry {
        value: value.clone(),
        last_used: generation,
      },
    );
    self.usage.push_back((key, generation));
    self.evict_if_needed();
    value
  }

  fn get_or_insert_with<F>(&mut self, key: K, build: F) -> Option<V>
  where
    F: FnOnce() -> Option<V>,
  {
    if let Some(v) = self.get(&key) {
      return Some(v);
    }
    let value = build()?;
    Some(self.insert(key, value))
  }

  fn bump_generation(&mut self) -> u64 {
    self.generation = self.generation.wrapping_add(1);
    self.generation
  }

  fn evict_if_needed(&mut self) {
    while self.entries.len() > self.capacity {
      if let Some((key, generation)) = self.usage.pop_front() {
        let should_remove = self
          .entries
          .get(&key)
          .map(|entry| entry.last_used == generation)
          .unwrap_or(false);
        if should_remove {
          self.entries.remove(&key);
        }
      } else {
        break;
      }
    }
  }
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
