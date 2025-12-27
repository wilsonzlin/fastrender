use super::ColorGlyphRaster;
use std::sync::Arc;
use tiny_skia::Pixmap;

/// Render embedded bitmap glyphs (CBDT/CBLC or sbix).
pub fn render_bitmap_glyph(
  face: &ttf_parser::Face<'_>,
  glyph_id: ttf_parser::GlyphId,
  font_size: f32,
) -> Option<ColorGlyphRaster> {
  let ppem = font_size.ceil() as u16;
  let raster = face.glyph_raster_image(glyph_id, ppem)?;

  let pixmap = match raster.format {
    ttf_parser::RasterImageFormat::PNG => Pixmap::decode_png(raster.data).ok()?,
    _ => return None,
  };

  let image = Arc::new(pixmap);
  let left = raster.x as f32;
  let top = -(raster.y as f32);

  Some(ColorGlyphRaster { image, left, top })
}
