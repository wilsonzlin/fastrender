use super::limits::{log_glyph_limit, round_dimension, GlyphRasterLimits};
use super::{cpal, read_u16, read_u32, ColorFontCaches, ColorGlyphRaster, FontKey};
use crate::style::color::Rgba;
use crate::text::font_instance::{glyph_transform, FontInstance};
use std::sync::{Arc, Mutex};
use tiny_skia::{FillRule, Paint, Path, Pixmap, Transform};

#[derive(Clone, Copy, Debug)]
pub struct ColrV0Header {
  pub base_offset: usize,
  pub num_base: usize,
  pub layer_offset: usize,
  pub num_layers: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct BaseGlyphCacheKey {
  pub font: FontKey,
  pub glyph_id: u16,
}

impl BaseGlyphCacheKey {
  pub fn new(font: FontKey, glyph_id: u16) -> Self {
    Self { font, glyph_id }
  }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct BaseGlyphRecord {
  pub first_layer: usize,
  pub layer_count: usize,
}

#[derive(Clone, Copy, Debug)]
pub struct LayerRecord {
  pub glyph_id: u16,
  pub palette_index: u16,
}

pub fn parse_colr_header(data: &[u8]) -> Option<ColrV0Header> {
  if data.len() < 14 {
    return None;
  }
  let version = read_u16(data, 0)?;
  if version != 0 {
    return None;
  }
  let num_base = read_u16(data, 2)? as usize;
  let base_offset = read_u32(data, 4)? as usize;
  let layer_offset = read_u32(data, 8)? as usize;
  let num_layers = read_u16(data, 12)? as usize;
  Some(ColrV0Header {
    base_offset,
    num_base,
    layer_offset,
    num_layers,
  })
}

pub fn find_base_glyph(
  data: &[u8],
  header: ColrV0Header,
  glyph_id: u16,
) -> Option<BaseGlyphRecord> {
  let record_size = 6;
  for i in 0..header.num_base {
    let offset = header.base_offset.checked_add(i * record_size)?;
    let gid = read_u16(data, offset)?;
    if gid == glyph_id {
      let first_layer = read_u16(data, offset + 2)? as usize;
      let count = read_u16(data, offset + 4)? as usize;
      return Some(BaseGlyphRecord {
        first_layer,
        layer_count: count,
      });
    }
  }
  None
}

fn parse_layer_records(
  data: &[u8],
  header: ColrV0Header,
  base: BaseGlyphRecord,
) -> Option<Vec<LayerRecord>> {
  if base.layer_count == 0 {
    return None;
  }
  if base.first_layer.checked_add(base.layer_count)? > header.num_layers {
    return None;
  }

  let mut layers = Vec::with_capacity(base.layer_count);
  for i in 0..base.layer_count {
    let offset = header
      .layer_offset
      .checked_add((base.first_layer + i) * 4)?;
    let glyph_id = read_u16(data, offset)?;
    let palette_index = read_u16(data, offset + 2)?;
    layers.push(LayerRecord {
      glyph_id,
      palette_index,
    });
  }
  Some(layers)
}

fn resolve_layer_color(idx: u16, palette: &[Rgba], text_color: Rgba) -> Rgba {
  if idx == 0xFFFF {
    // currentColor paints pick up text/ancestor opacity at draw time. Keep the
    // cached raster opaque so global alpha is only applied once.
    return Rgba {
      a: 1.0,
      ..text_color
    };
  }
  palette.get(idx as usize).copied().unwrap_or(text_color)
}

pub fn render_colr_glyph(
  instance: &FontInstance,
  face: &ttf_parser::Face<'_>,
  font_key: FontKey,
  glyph_id: ttf_parser::GlyphId,
  font_size: f32,
  palette_index: u16,
  overrides: &[(u16, Rgba)],
  text_color: Rgba,
  _synthetic_oblique: f32,
  limits: &GlyphRasterLimits,
  caches: &Arc<Mutex<ColorFontCaches>>,
) -> Option<ColorGlyphRaster> {
  let colr_data = face
    .raw_face()
    .table(ttf_parser::Tag::from_bytes(b"COLR"))?;
  let header = {
    let mut caches = caches.lock().unwrap();
    caches.colr_v0_header(font_key, colr_data)?
  };

  let base_record = {
    let key = BaseGlyphCacheKey::new(font_key, glyph_id.0);
    let mut caches = caches.lock().unwrap();
    match caches.colr_v0_base_record(key, colr_data, header) {
      Some(Some(record)) => record,
      Some(None) => return None,
      None => return None,
    }
  };

  if header.base_offset + header.num_base * 6 > colr_data.len()
    || header.layer_offset + header.num_layers * 4 > colr_data.len()
  {
    return None;
  }

  let palette = {
    let mut caches = caches.lock().unwrap();
    caches
      .palette(font_key, face, palette_index)
      .unwrap_or_else(|| Arc::new(cpal::ParsedPalette::default()))
  };
  let mut colors = palette.colors.clone();
  for (idx, color) in overrides {
    if let Some(slot) = colors.get_mut(*idx as usize) {
      *slot = *color;
    }
  }

  let layer_records = parse_layer_records(colr_data, header, base_record)?;
  let units_per_em = instance.units_per_em();
  if units_per_em == 0.0 {
    return None;
  }
  let scale = font_size / units_per_em;
  if !scale.is_finite() || !font_size.is_finite() {
    return None;
  }
  // Synthetic oblique is applied when painting color glyphs to keep the cached
  // rasters shared across runs.
  let transform = glyph_transform(scale, 0.0, 0.0, 0.0);

  let mut paths: Vec<(Path, Rgba)> = Vec::new();
  for layer in layer_records {
    let color = resolve_layer_color(layer.palette_index, &colors, text_color);
    if let Some(outline) = instance.glyph_outline(layer.glyph_id as u32) {
      if let Some(path) = outline.path.and_then(|p| p.transform(transform)) {
        paths.push((path, color));
      }
    }
  }

  if paths.is_empty() {
    return None;
  }

  rasterize_colr_layers(paths, limits, glyph_id.0 as u32)
}

fn rasterize_colr_layers(
  mut paths: Vec<(Path, Rgba)>,
  limits: &GlyphRasterLimits,
  glyph_id: u32,
) -> Option<ColorGlyphRaster> {
  // Compute bounds
  let mut min_x = f32::MAX;
  let mut min_y = f32::MAX;
  let mut max_x = f32::MIN;
  let mut max_y = f32::MIN;

  for (path, _) in &paths {
    let bounds = path.bounds();
    min_x = min_x.min(bounds.left());
    min_y = min_y.min(bounds.top());
    max_x = max_x.max(bounds.right());
    max_y = max_y.max(bounds.bottom());
  }

  if !min_x.is_finite() || !min_y.is_finite() || !max_x.is_finite() || !max_y.is_finite() {
    return None;
  }

  let pad = 1.0;
  min_x = (min_x - pad).floor();
  min_y = (min_y - pad).floor();
  max_x = (max_x + pad).ceil();
  max_y = (max_y + pad).ceil();

  let width = round_dimension(max_x - min_x)?;
  let height = round_dimension(max_y - min_y)?;
  if let Err(err) = limits.validate(width, height) {
    log_glyph_limit("colr", glyph_id, &err);
    return None;
  }
  let mut pixmap = Pixmap::new(width, height)?;

  for (path, color) in paths.drain(..) {
    let mut paint = Paint::default();
    paint.set_color_rgba8(color.r, color.g, color.b, color.alpha_u8());
    paint.anti_alias = true;
    let translate = Transform::from_translate(-min_x, -min_y);
    pixmap.fill_path(&path, &paint, FillRule::Winding, translate, None);
  }

  Some(ColorGlyphRaster {
    image: Arc::new(pixmap),
    left: min_x,
    top: min_y,
  })
}

#[cfg(test)]
mod tests {
  use super::super::limits::GlyphRasterLimits;
  use super::*;
  use tiny_skia::PathBuilder;

  #[test]
  fn parse_header_rejects_nonzero_version() {
    let data = [0x00, 0x01, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
    assert!(parse_colr_header(&data).is_none());
  }

  #[test]
  fn parse_header_basic() {
    let data = [
      0x00, 0x00, // version
      0x00, 0x02, // num base
      0x00, 0x00, 0x00, 0x08, // base offset
      0x00, 0x00, 0x00, 0x20, // layer offset
      0x00, 0x04, // num layers
    ];
    let header = parse_colr_header(&data).unwrap();
    assert_eq!(header.base_offset, 8);
    assert_eq!(header.layer_offset, 32);
    assert_eq!(header.num_base, 2);
    assert_eq!(header.num_layers, 4);
  }

  #[test]
  fn find_base_glyph_locates_record() {
    // Build a minimal COLR table with two base glyphs.
    let mut data = vec![
      0x00, 0x00, // version
      0x00, 0x02, // num base
      0x00, 0x00, 0x00, 0x0E, // base offset = 14
      0x00, 0x00, 0x00, 0x20, // layer offset = 32
      0x00, 0x04, // num layers
    ];
    // Pad to base offset (14)
    data.resize(14, 0);
    // Base glyph records (glyph id, first layer, count)
    data.extend_from_slice(&[0x00, 0x05, 0x00, 0x01, 0x00, 0x02]); // glyph 5
    data.extend_from_slice(&[0x00, 0x06, 0x00, 0x03, 0x00, 0x01]); // glyph 6

    let header = parse_colr_header(&data).unwrap();
    let record = find_base_glyph(&data, header, 5).unwrap();
    assert_eq!(record.first_layer, 1);
    assert_eq!(record.layer_count, 2);
    assert!(find_base_glyph(&data, header, 7).is_none());
  }

  #[test]
  fn colr_layers_exceeding_limits_are_skipped() {
    let mut builder = PathBuilder::new();
    builder.move_to(0.0, 0.0);
    builder.line_to(20000.0, 0.0);
    builder.line_to(20000.0, 20000.0);
    builder.line_to(0.0, 20000.0);
    builder.close();
    let path = builder.finish().unwrap();
    let limits = GlyphRasterLimits::new(512, 512_u64 * 512_u64);

    assert!(rasterize_colr_layers(vec![(path, Rgba::RED)], &limits, 1).is_none());
  }
}
