use super::{read_u16, read_u24, read_u32, ColorFontCaches, ColorGlyphRaster, FontKey};
use crate::style::color::Rgba;
use std::sync::{Arc, Mutex};

#[derive(Clone, Copy, Debug)]
pub struct ColrV1Header {
  pub base_glyph_list_offset: usize,
  pub layer_list_offset: usize,
  pub num_layer_records: usize,
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
  pub glyph_id: u16,
  pub paint_offset: usize,
}

pub fn parse_colr_header(data: &[u8]) -> Option<ColrV1Header> {
  if data.len() < 34 {
    return None;
  }
  let version = read_u16(data, 0)?;
  if version != 1 {
    return None;
  }

  let num_layer_records = read_u16(data, 12)? as usize;
  let base_glyph_list_offset = read_u32(data, 14)? as usize;
  let layer_list_offset = read_u32(data, 18)? as usize;

  Some(ColrV1Header {
    base_glyph_list_offset,
    layer_list_offset,
    num_layer_records,
  })
}

fn find_base_glyph_with_size<F>(
  data: &[u8],
  start: usize,
  count: usize,
  record_size: usize,
  target: u16,
  read_offset: F,
) -> Option<BaseGlyphRecord>
where
  F: Fn(&[u8], usize) -> Option<u32>,
{
  for i in 0..count {
    let offset = start.checked_add(i * record_size)?;
    let gid = read_u16(data, offset)?;
    if gid == target {
      let paint_offset = read_offset(data, offset + 2)? as usize;
      return Some(BaseGlyphRecord {
        glyph_id: target,
        paint_offset,
      });
    }
  }
  None
}

pub fn find_base_glyph(
  data: &[u8],
  header: ColrV1Header,
  glyph_id: u16,
) -> Option<BaseGlyphRecord> {
  let base_offset = header.base_glyph_list_offset;
  if base_offset == 0 || base_offset + 2 > data.len() {
    return None;
  }

  let count = read_u16(data, base_offset)? as usize;
  let start = base_offset + 2;

  // Try 32-bit offsets first; fall back to 24-bit offsets if needed.
  let record_size_32 = 6;
  if start + count * record_size_32 <= data.len() {
    if let Some(record) =
      find_base_glyph_with_size(data, start, count, record_size_32, glyph_id, read_u32)
    {
      return Some(record);
    }
  }

  let record_size_24 = 5;
  if start + count * record_size_24 <= data.len() {
    if let Some(record) =
      find_base_glyph_with_size(data, start, count, record_size_24, glyph_id, read_u24)
    {
      return Some(record);
    }
  }

  None
}

pub fn render_colr_glyph(
  face: &ttf_parser::Face<'_>,
  font_key: FontKey,
  glyph_id: ttf_parser::GlyphId,
  _font_size: f32,
  _palette_index: u16,
  _text_color: Rgba,
  _synthetic_oblique: f32,
  caches: &Arc<Mutex<ColorFontCaches>>,
) -> Option<ColorGlyphRaster> {
  let colr_data = face
    .raw_face()
    .table(ttf_parser::Tag::from_bytes(b"COLR"))?;
  let header = {
    let mut caches = caches.lock().unwrap();
    caches.colr_v1_header(font_key, colr_data)?
  };

  let _base_record = {
    let key = BaseGlyphCacheKey::new(font_key, glyph_id.0);
    let mut caches = caches.lock().unwrap();
    caches.colr_v1_base_record(key, colr_data, header)
  };

  // COLR v1 rendering is not implemented yet. We still parse and cache base
  // glyph records so repeated lookups avoid re-walking the table and callers
  // can gracefully fall back to monochrome outlines.
  None
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn parse_v1_header() {
    // version=1, numLayerRecords=2, baseGlyphListOffset=20, layerListOffset=40
    let data = [
      0x00, 0x01, // version
      0x00, 0x00, // num base glyph records (v0 compatibility)
      0x00, 0x00, 0x00, 0x00, // baseGlyphRecordsOffset
      0x00, 0x00, 0x00, 0x00, // layerRecordsOffset
      0x00, 0x02, // numLayerRecords
      0x00, 0x00, 0x00, 0x14, // baseGlyphListOffset = 20
      0x00, 0x00, 0x00, 0x28, // layerListOffset = 40
      0x00, 0x00, 0x00, 0x00, // clipListOffset
      0x00, 0x00, 0x00, 0x00, // varIndexMapOffset
      0x00, 0x00, 0x00, 0x00, // itemVariationStoreOffset
    ];
    let header = parse_colr_header(&data).unwrap();
    assert_eq!(header.base_glyph_list_offset, 20);
    assert_eq!(header.layer_list_offset, 40);
    assert_eq!(header.num_layer_records, 2);
  }

  #[test]
  fn find_base_glyph_with_32bit_offsets() {
    // Build a BaseGlyphList with one record using 32-bit offsets.
    let mut data = vec![0; 60];
    data[0] = 0;
    data[1] = 1; // version=1
                 // baseGlyphListOffset = 34 (0x22)
    data[14] = 0x00;
    data[15] = 0x00;
    data[16] = 0x00;
    data[17] = 0x22;
    // layerListOffset (unused here)
    data[18] = 0x00;
    data[19] = 0x00;
    data[20] = 0x00;
    data[21] = 0x30;

    // BaseGlyphList at offset 34
    data[34] = 0x00;
    data[35] = 0x01; // count
    data[36] = 0x00;
    data[37] = 0x05; // glyph id 5
    data[38] = 0x00;
    data[39] = 0x00;
    data[40] = 0x00;
    data[41] = 0x10; // paint offset

    let header = parse_colr_header(&data).unwrap();
    let record = find_base_glyph(&data, header, 5).unwrap();
    assert_eq!(record.paint_offset, 16);
    assert!(find_base_glyph(&data, header, 2).is_none());
  }
}
