use super::limits::{log_glyph_limit, GlyphRasterLimits};
use super::ColorGlyphRaster;
use image::ImageFormat;
use std::sync::Arc;
use tiny_skia::{IntSize, Pixmap};

pub const MAX_SBIX_GLYPH_BYTES: usize = 512 * 1024;

/// Render embedded bitmap glyphs (CBDT/CBLC or sbix).
pub fn render_bitmap_glyph(
  face: &ttf_parser::Face<'_>,
  glyph_id: ttf_parser::GlyphId,
  font_size: f32,
  limits: &GlyphRasterLimits,
) -> Option<ColorGlyphRaster> {
  if !font_size.is_finite() || font_size <= 0.0 {
    return None;
  }

  let ppem = font_size.ceil();
  if ppem <= 0.0 || ppem > u16::MAX as f32 {
    return None;
  }
  if let Some(raster) = face.glyph_raster_image(glyph_id, ppem as u16) {
    let width = u32::from(raster.width);
    let height = u32::from(raster.height);
    if let Err(err) = limits.validate(width, height) {
      log_glyph_limit("bitmap", glyph_id.0 as u32, &err);
    } else if let Some(pixmap) = decode_bitmap_pixmap(&raster) {
      let image = Arc::new(pixmap);
      let left = raster.x as f32;
      let top = -(raster.y as f32);

      return Some(ColorGlyphRaster { image, left, top });
    }
  }

  render_sbix_bitmap_glyph(face, glyph_id, ppem as u16, limits)
}

fn decode_bitmap_pixmap(raster: &ttf_parser::RasterGlyphImage<'_>) -> Option<Pixmap> {
  match raster.format {
    ttf_parser::RasterImageFormat::PNG => Pixmap::decode_png(raster.data)
      .ok()
      .or_else(|| decode_image_with_format(raster.data, None)),
    ttf_parser::RasterImageFormat::BitmapPremulBgra32 => {
      let size = IntSize::from_wh(raster.width as u32, raster.height as u32)?;
      Pixmap::from_vec(raster.data.to_vec(), size)
    }
    _ => decode_image_with_format(raster.data, None),
  }
}

fn render_sbix_bitmap_glyph(
  face: &ttf_parser::Face<'_>,
  glyph_id: ttf_parser::GlyphId,
  pixels_per_em: u16,
  limits: &GlyphRasterLimits,
) -> Option<ColorGlyphRaster> {
  let sbix = face
    .raw_face()
    .table(ttf_parser::Tag::from_bytes(b"sbix"))?;
  let glyph_count = face.number_of_glyphs() as usize;
  let strike_offset = select_sbix_strike(sbix, glyph_count, pixels_per_em)?;
  let glyph = parse_sbix_glyph(sbix, strike_offset, glyph_count, glyph_id, 0)?;

  let (width, height) = preflight_sbix_image(&glyph.tag, glyph.data)?;

  if let Err(err) = limits.validate(width, height) {
    log_glyph_limit("bitmap", glyph_id.0 as u32, &err);
    return None;
  }

  let pixmap = match &glyph.tag {
    b"jpg " | b"jpeg" => decode_image_with_format(glyph.data, Some(ImageFormat::Jpeg))?,
    b"png " => decode_image_with_format(glyph.data, Some(ImageFormat::Png))?,
    _ => return None,
  };

  if let Err(err) = limits.validate(pixmap.width(), pixmap.height()) {
    log_glyph_limit("bitmap", glyph_id.0 as u32, &err);
    return None;
  }

  Some(ColorGlyphRaster {
    image: Arc::new(pixmap),
    left: glyph.x as f32,
    top: -(glyph.y as f32),
  })
}

fn select_sbix_strike(sbix: &[u8], glyph_count: usize, pixels_per_em: u16) -> Option<usize> {
  if sbix.len() < 8 {
    return None;
  }

  let strike_count = read_u32(sbix, 4)? as usize;
  let offsets_start = 8usize;
  let offsets_len = strike_count.checked_mul(4)?;
  let offsets_end = offsets_start.checked_add(offsets_len)?;
  if offsets_end > sbix.len() {
    return None;
  }

  let mut best_offset: Option<usize> = None;
  let mut best_ppem: u16 = 0;

  for i in 0..strike_count {
    let strike_offset = read_u32(sbix, offsets_start + i * 4)? as usize;

    // Strike header (ppem, ppi) + glyph offset array.
    let offsets_bytes = (glyph_count + 1).checked_mul(4)?;
    let needed = strike_offset.checked_add(4)?.checked_add(offsets_bytes)?;
    if needed > sbix.len() {
      continue;
    }

    let strike_ppem = read_u16(sbix, strike_offset)?;

    if (pixels_per_em <= strike_ppem && (strike_ppem < best_ppem || best_offset.is_none()))
      || (pixels_per_em > best_ppem && strike_ppem > best_ppem)
    {
      best_offset = Some(strike_offset);
      best_ppem = strike_ppem;
    }
  }

  best_offset
}

#[derive(Clone, Copy)]
struct SbixGlyphImage<'a> {
  x: i16,
  y: i16,
  tag: [u8; 4],
  data: &'a [u8],
}

fn parse_sbix_glyph<'a>(
  sbix: &'a [u8],
  strike_offset: usize,
  glyph_count: usize,
  glyph_id: ttf_parser::GlyphId,
  depth: u8,
) -> Option<SbixGlyphImage<'a>> {
  if depth > 4 {
    return None;
  }

  let glyph_idx = glyph_id.0 as usize;
  if glyph_idx >= glyph_count {
    return None;
  }

  let offsets_base = strike_offset.checked_add(4)?;
  let offsets_len = (glyph_count + 1).checked_mul(4)?;
  if offsets_base.checked_add(offsets_len)? > sbix.len() {
    return None;
  }

  let start = read_u32(sbix, offsets_base + glyph_idx * 4)? as usize;
  let end = read_u32(sbix, offsets_base + (glyph_idx + 1) * 4)? as usize;
  if start == end {
    return None;
  }

  let glyph_start = strike_offset.checked_add(start)?;
  let glyph_end = strike_offset.checked_add(end)?;
  let header_end = glyph_start.checked_add(8)?;
  if header_end > glyph_end || glyph_end > sbix.len() {
    return None;
  }

  let x = read_i16(sbix, glyph_start)?;
  let y = read_i16(sbix, glyph_start + 2)?;
  let tag = read_tag(sbix, glyph_start + 4)?;
  let data = sbix.get(header_end..glyph_end)?;

  if &tag == b"dupe" {
    let ref_glyph_id = ttf_parser::GlyphId(read_u16(data, 0)?);
    return parse_sbix_glyph(sbix, strike_offset, glyph_count, ref_glyph_id, depth + 1);
  }

  Some(SbixGlyphImage { x, y, tag, data })
}

fn preflight_sbix_image(tag: &[u8; 4], data: &[u8]) -> Option<(u32, u32)> {
  if data.len() > MAX_SBIX_GLYPH_BYTES {
    return None;
  }

  match tag {
    b"jpg " | b"jpeg" => parse_jpeg_dimensions(data),
    b"png " => parse_png_dimensions(data),
    _ => None,
  }
}

fn parse_png_dimensions(data: &[u8]) -> Option<(u32, u32)> {
  const PNG_SIGNATURE: [u8; 8] = [137, 80, 78, 71, 13, 10, 26, 10];
  if !data.starts_with(&PNG_SIGNATURE) {
    return None;
  }

  let ihdr_length = read_u32(data, 8)? as usize;
  let chunk_type = read_tag(data, 12)?;
  if &chunk_type != b"IHDR" {
    return None;
  }

  let chunk_data_start = 16usize;
  let chunk_data_end = chunk_data_start.checked_add(ihdr_length)?;
  let crc_end = chunk_data_end.checked_add(4)?;
  if crc_end > data.len() || ihdr_length < 8 {
    return None;
  }

  let width = read_u32(data, chunk_data_start)?;
  let height = read_u32(data, chunk_data_start + 4)?;
  if width == 0 || height == 0 {
    return None;
  }

  Some((width, height))
}

fn parse_jpeg_dimensions(data: &[u8]) -> Option<(u32, u32)> {
  if data.len() < 4 || data[0] != 0xFF || data[1] != 0xD8 {
    return None;
  }

  let mut offset = 2usize;

  while offset + 1 < data.len() {
    while offset < data.len() && data[offset] == 0xFF {
      offset += 1;
    }
    if offset >= data.len() {
      break;
    }

    let marker = data[offset];
    offset += 1;

    if marker == 0xD9 || marker == 0xDA {
      break;
    }
    if marker == 0x01 || (0xD0..=0xD7).contains(&marker) {
      continue;
    }

    let length = read_u16(data, offset)? as usize;
    if length < 2 {
      return None;
    }
    let segment_start = offset.checked_add(2)?;
    let segment_end = offset.checked_add(length)?;
    if segment_end > data.len() {
      return None;
    }

    if is_jpeg_start_of_frame(marker) && length >= 7 {
      let height_offset = segment_start.checked_add(1)?;
      let width_offset = segment_start.checked_add(3)?;
      let height = read_u16(data, height_offset)? as u32;
      let width = read_u16(data, width_offset)? as u32;
      if width == 0 || height == 0 {
        return None;
      }
      return Some((width, height));
    }

    offset = segment_end;
  }

  None
}

fn is_jpeg_start_of_frame(marker: u8) -> bool {
  matches!(
    marker,
    0xC0 | 0xC1 | 0xC2 | 0xC3 | 0xC5 | 0xC6 | 0xC7 | 0xC9 | 0xCA | 0xCB | 0xCD | 0xCE | 0xCF
  )
}

fn decode_image_with_format(data: &[u8], format: Option<ImageFormat>) -> Option<Pixmap> {
  let image = match format {
    Some(fmt) => image::load_from_memory_with_format(data, fmt).ok()?,
    None => image::load_from_memory(data).ok()?,
  };
  let rgba = image.to_rgba8();
  let (width, height) = rgba.dimensions();
  let mut pixels = rgba.into_vec();

  for chunk in pixels.chunks_exact_mut(4) {
    let r = chunk[0];
    let g = chunk[1];
    let b = chunk[2];
    let a = chunk[3];
    let alpha = a as u16;
    let premultiply = |c: u8| ((c as u16 * alpha + 127) / 255) as u8;

    chunk[0] = premultiply(b);
    chunk[1] = premultiply(g);
    chunk[2] = premultiply(r);
    chunk[3] = a;
  }

  let size = IntSize::from_wh(width, height)?;
  Pixmap::from_vec(pixels, size)
}

fn read_u16(data: &[u8], offset: usize) -> Option<u16> {
  let bytes = data.get(offset..offset + 2)?;
  Some(u16::from_be_bytes([bytes[0], bytes[1]]))
}

fn read_u32(data: &[u8], offset: usize) -> Option<u32> {
  let bytes = data.get(offset..offset + 4)?;
  Some(u32::from_be_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]))
}

fn read_i16(data: &[u8], offset: usize) -> Option<i16> {
  let bytes = data.get(offset..offset + 2)?;
  Some(i16::from_be_bytes([bytes[0], bytes[1]]))
}

fn read_tag(data: &[u8], offset: usize) -> Option<[u8; 4]> {
  let bytes = data.get(offset..offset + 4)?;
  Some([bytes[0], bytes[1], bytes[2], bytes[3]])
}

#[cfg(test)]
mod tests {
  use super::{
    parse_jpeg_dimensions, parse_png_dimensions, preflight_sbix_image, MAX_SBIX_GLYPH_BYTES,
  };

  fn minimal_png(width: u32, height: u32) -> Vec<u8> {
    let mut data = Vec::new();
    data.extend_from_slice(&[137, 80, 78, 71, 13, 10, 26, 10]);
    data.extend_from_slice(&13u32.to_be_bytes());
    data.extend_from_slice(b"IHDR");
    data.extend_from_slice(&width.to_be_bytes());
    data.extend_from_slice(&height.to_be_bytes());
    data.extend_from_slice(&[8, 6, 0, 0, 0]);
    data.extend_from_slice(&0u32.to_be_bytes());
    data
  }

  fn minimal_jpeg(width: u16, height: u16) -> Vec<u8> {
    let mut data = vec![0xFF, 0xD8];
    data.extend_from_slice(&[
      0xFF,
      0xC0,
      0x00,
      0x11,
      0x08,
      (height >> 8) as u8,
      height as u8,
      (width >> 8) as u8,
      width as u8,
      0x03,
      0x01,
      0x11,
      0x00,
      0x02,
      0x11,
      0x00,
      0x03,
      0x11,
      0x00,
    ]);
    data.extend_from_slice(&[0xFF, 0xD9]);
    data
  }

  #[test]
  fn png_preflight_parses_dimensions() {
    let data = minimal_png(16, 32);
    assert_eq!(parse_png_dimensions(&data), Some((16, 32)));
  }

  #[test]
  fn jpeg_preflight_parses_dimensions() {
    let data = minimal_jpeg(32, 64);
    assert_eq!(parse_jpeg_dimensions(&data), Some((32, 64)));
  }

  #[test]
  fn preflight_rejects_corrupt_headers() {
    assert!(parse_png_dimensions(&[0u8; 16]).is_none());
    assert!(parse_jpeg_dimensions(&[0xFF, 0xD8, 0x00, 0x00]).is_none());
  }

  #[test]
  fn oversized_sbix_payload_is_rejected() {
    let mut data = minimal_png(1, 1);
    data.resize(MAX_SBIX_GLYPH_BYTES + 1, 0);
    assert!(preflight_sbix_image(b"png ", &data).is_none());
  }
}
