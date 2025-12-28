use super::ColorGlyphRaster;
use image::ImageFormat;
use std::sync::Arc;
use tiny_skia::{IntSize, Pixmap};

/// Render embedded bitmap glyphs (CBDT/CBLC or sbix).
pub fn render_bitmap_glyph(
  face: &ttf_parser::Face<'_>,
  glyph_id: ttf_parser::GlyphId,
  font_size: f32,
) -> Option<ColorGlyphRaster> {
  let ppem = font_size.ceil() as u16;
  if let Some(raster) = face.glyph_raster_image(glyph_id, ppem) {
    if let Some(pixmap) = decode_bitmap_pixmap(&raster) {
      let image = Arc::new(pixmap);
      let left = raster.x as f32;
      let top = -(raster.y as f32);

      return Some(ColorGlyphRaster { image, left, top });
    }
  }

  render_sbix_bitmap_glyph(face, glyph_id, ppem)
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
) -> Option<ColorGlyphRaster> {
  let sbix = face
    .raw_face()
    .table(ttf_parser::Tag::from_bytes(b"sbix"))?;
  let glyph_count = face.number_of_glyphs() as usize;
  let strike_offset = select_sbix_strike(sbix, glyph_count, pixels_per_em)?;
  let glyph = parse_sbix_glyph(sbix, strike_offset, glyph_count, glyph_id, 0)?;

  let pixmap = match &glyph.tag {
    b"jpg " | b"jpeg" => decode_image_with_format(glyph.data, Some(ImageFormat::Jpeg))?,
    b"png " => decode_image_with_format(glyph.data, Some(ImageFormat::Png))?,
    _ => return None,
  };

  Some(ColorGlyphRaster {
    image: Arc::new(pixmap),
    left: glyph.x as f32,
    top: -(glyph.y as f32),
  })
}

fn select_sbix_strike(
  sbix: &[u8],
  glyph_count: usize,
  pixels_per_em: u16,
) -> Option<usize> {
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
    let needed = strike_offset
      .checked_add(4)?
      .checked_add(offsets_bytes)?;
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
