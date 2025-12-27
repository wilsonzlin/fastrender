//! Color font helpers
//!
//! Provides color glyph rasterization for bitmap, SVG, and COLR/CPAL fonts.

use crate::style::color::Rgba;
use crate::text::cpal::parse_cpal_palette;
use crate::svg::{svg_root_view_box, svg_view_box_root_transform, SvgViewBox};
use crate::text::font_db::LoadedFont;
use std::sync::Arc;
use tiny_skia::{FillRule, Paint, Path, PathBuilder, Pixmap, Transform};

/// Rasterized color glyph image with positioning relative to the glyph origin.
#[derive(Debug, Clone)]
pub struct ColorGlyphRaster {
  /// Premultiplied BGRA8 pixmap for the glyph.
  pub image: Arc<Pixmap>,
  /// X offset from glyph origin to the top-left corner (pixels, Y-down).
  pub left: f32,
  /// Y offset from baseline to the top-left corner (pixels, Y-down).
  pub top: f32,
}

/// Renders color glyphs from OpenType color tables.
#[derive(Debug, Default)]
pub struct ColorFontRenderer;

impl ColorFontRenderer {
  pub fn new() -> Self {
    Self
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

    // Prefer embedded bitmaps (CBDT/CBLC or sbix)
    if let Some(bitmap) = render_bitmap_glyph(&face, gid, font_size) {
      return Some(bitmap);
    }

    // SVG-in-OT
    if let Some(svg) = render_svg_glyph(&face, gid, font_size, text_color) {
      return Some(svg);
    }

    // COLR/CPAL (v0). COLR v1 falls back to outline for now.
    if let Some(colr) = render_colr_glyph(
      &face,
      gid,
      font_size,
      palette_index,
      text_color,
      synthetic_oblique,
    ) {
      return Some(colr);
    }

    None
  }
}

// ----------------------------------------------------------------------------
// Bitmap glyphs (CBDT/CBLC, sbix)
// ----------------------------------------------------------------------------

fn render_bitmap_glyph(
  face: &ttf_parser::Face<'_>,
  glyph_id: ttf_parser::GlyphId,
  font_size: f32,
) -> Option<ColorGlyphRaster> {
  let ppem = font_size.ceil() as u16;
  let raster = face.glyph_raster_image(glyph_id, ppem)?;

  let pixmap = match raster.format {
    ttf_parser::RasterImageFormat::PNG => tiny_skia::Pixmap::decode_png(raster.data).ok()?,
    _ => return None,
  };

  let image = Arc::new(pixmap);
  let left = raster.x as f32;
  let top = -(raster.y as f32);

  Some(ColorGlyphRaster { image, left, top })
}

// ----------------------------------------------------------------------------
// SVG glyphs
// ----------------------------------------------------------------------------

fn render_svg_glyph(
  face: &ttf_parser::Face<'_>,
  glyph_id: ttf_parser::GlyphId,
  font_size: f32,
  text_color: Rgba,
) -> Option<ColorGlyphRaster> {
  let svg_doc = face.glyph_svg_image(glyph_id)?;
  let svg_str = std::str::from_utf8(svg_doc.data).ok()?;
  let svg_with_color = inject_current_color(svg_str, text_color)?;

  let options = resvg::usvg::Options::default();

  let tree = resvg::usvg::Tree::from_str(&svg_with_color, &options).ok()?;
  let size = tree.size();
  let source_width = size.width() as f32;
  let source_height = size.height() as f32;
  if source_width <= 0.0 || source_height <= 0.0 {
    return None;
  }

  let view_box = svg_root_view_box(&svg_with_color).unwrap_or(SvgViewBox {
    min_x: 0.0,
    min_y: 0.0,
    width: source_width,
    height: source_height,
  });
  if view_box.width <= 0.0 || view_box.height <= 0.0 {
    return None;
  }

  let units_per_em = face.units_per_em() as f32;
  if units_per_em <= 0.0 {
    return None;
  }

  let scale = font_size / units_per_em;
  let width = (view_box.width * scale).max(1.0).ceil() as u32;
  let height = (view_box.height * scale).max(1.0).ceil() as u32;

  let mut pixmap = Pixmap::new(width, height)?;

  // Map the root SVG viewport into the glyph viewBox while respecting preserveAspectRatio,
  // then flip the Y axis so SVG glyph coordinates (y-up) align with font coordinates.
  let view_box_transform = svg_view_box_root_transform(
    &svg_with_color,
    source_width,
    source_height,
    view_box.width,
    view_box.height,
  )
  .unwrap_or_else(|| {
    Transform::from_scale(
      view_box.width / source_width,
      view_box.height / source_height,
    )
  });

  let max_y = view_box.min_y + view_box.height;
  let glyph_transform = Transform::from_row(
    scale,
    0.0,
    0.0,
    -scale,
    -view_box.min_x * scale,
    max_y * scale,
  );
  let transform = concat_transforms(glyph_transform, view_box_transform);

  resvg::render(&tree, transform, &mut pixmap.as_mut());

  Some(ColorGlyphRaster {
    image: Arc::new(pixmap),
    left: view_box.min_x * scale,
    top: -max_y * scale,
  })
}

fn inject_current_color(svg: &str, text_color: Rgba) -> Option<String> {
  let alpha = text_color.a.clamp(0.0, 1.0);
  let color_value = format!(
    "rgba({},{},{},{})",
    text_color.r, text_color.g, text_color.b, alpha
  );

  let svg_start = svg.to_ascii_lowercase().find("<svg")?;
  let tag_end = svg[svg_start..].find('>')? + svg_start;

  let mut injected = String::with_capacity(svg.len() + color_value.len() + 16);
  injected.push_str(&svg[..svg_start + 4]);
  injected.push(' ');
  injected.push_str("color=\"");
  injected.push_str(&color_value);
  injected.push('"');
  injected.push_str(&svg[svg_start + 4..=tag_end]);
  injected.push_str(&svg[tag_end + 1..]);
  Some(injected)
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

// ----------------------------------------------------------------------------
// COLR/CPAL v0 layered glyphs
// ----------------------------------------------------------------------------

fn render_colr_glyph(
  face: &ttf_parser::Face<'_>,
  glyph_id: ttf_parser::GlyphId,
  font_size: f32,
  palette_index: u16,
  text_color: Rgba,
  synthetic_oblique: f32,
) -> Option<ColorGlyphRaster> {
  let colr_data = face
    .raw_face()
    .table(ttf_parser::Tag::from_bytes(b"COLR"))?;
  let (base_offset, num_base, layer_offset, num_layers) = parse_colr_header(colr_data)?;

  let base_size = 6usize;
  if base_offset + num_base * base_size > colr_data.len()
    || layer_offset + num_layers * 4 > colr_data.len()
  {
    return None;
  }

  let (first_layer, layer_count) = find_base_glyph(colr_data, base_offset, num_base, glyph_id.0)?;
  if layer_count == 0 {
    return None;
  }
  if first_layer + layer_count > num_layers {
    return None;
  }

  let palette = parse_cpal_palette(face, palette_index).unwrap_or_default();
  let layer_records = parse_layer_records(colr_data, layer_offset, first_layer, layer_count)?;
  let units_per_em = face.units_per_em() as f32;
  let scale = font_size / units_per_em;

  let mut paths: Vec<(Path, Rgba)> = Vec::new();
  for layer in layer_records {
    let color = resolve_layer_color(layer.palette_index, &palette, text_color);
    if let Some(path) = build_glyph_path(face, layer.glyph_id, scale, synthetic_oblique) {
      paths.push((path, color));
    }
  }

  if paths.is_empty() {
    return None;
  }

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

  let width = (max_x - min_x).max(1.0).round() as u32;
  let height = (max_y - min_y).max(1.0).round() as u32;
  let mut pixmap = Pixmap::new(width, height)?;

  for (path, color) in paths {
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

fn build_glyph_path(
  face: &ttf_parser::Face<'_>,
  glyph_id: u16,
  scale: f32,
  synthetic_oblique: f32,
) -> Option<Path> {
  let glyph = ttf_parser::GlyphId(glyph_id);
  let mut builder = GlyphOutlineBuilder::new(scale, synthetic_oblique, 0.0, 0.0);
  face.outline_glyph(glyph, &mut builder)?;
  builder.finish()
}

fn resolve_layer_color(idx: u16, palette: &[Rgba], text_color: Rgba) -> Rgba {
  if idx == 0xFFFF {
    return text_color;
  }
  palette.get(idx as usize).copied().unwrap_or(text_color)
}

fn parse_layer_records(
  data: &[u8],
  layer_offset: usize,
  first_layer: usize,
  layer_count: usize,
) -> Option<Vec<LayerRecord>> {
  let mut layers = Vec::with_capacity(layer_count);
  for i in 0..layer_count {
    let offset = layer_offset.checked_add((first_layer + i) * 4)?;
    let glyph_id = read_u16(data, offset)?;
    let palette_index = read_u16(data, offset + 2)?;
    layers.push(LayerRecord {
      glyph_id,
      palette_index,
    });
  }
  Some(layers)
}

fn parse_colr_header(data: &[u8]) -> Option<(usize, usize, usize, usize)> {
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
  Some((base_offset, num_base, layer_offset, num_layers))
}

fn find_base_glyph(
  data: &[u8],
  base_offset: usize,
  num_base: usize,
  glyph_id: u16,
) -> Option<(usize, usize)> {
  let record_size = 6;
  for i in 0..num_base {
    let offset = base_offset.checked_add(i * record_size)?;
    let gid = read_u16(data, offset)?;
    if gid == glyph_id {
      let first_layer = read_u16(data, offset + 2)? as usize;
      let count = read_u16(data, offset + 4)? as usize;
      return Some((first_layer, count));
    }
  }
  None
}

#[derive(Clone, Copy)]
struct LayerRecord {
  glyph_id: u16,
  palette_index: u16,
}

// ----------------------------------------------------------------------------
// Utility parsing helpers and outline builder
// ----------------------------------------------------------------------------

fn read_u16(data: &[u8], offset: usize) -> Option<u16> {
  let bytes = data.get(offset..offset + 2)?;
  Some(u16::from_be_bytes([bytes[0], bytes[1]]))
}

fn read_u32(data: &[u8], offset: usize) -> Option<u32> {
  let bytes = data.get(offset..offset + 4)?;
  Some(u32::from_be_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]))
}

/// Outline builder reused for COLR layers.
struct GlyphOutlineBuilder {
  builder: PathBuilder,
  scale: f32,
  skew: f32,
  x_offset: f32,
  y_offset: f32,
}

impl GlyphOutlineBuilder {
  fn new(scale: f32, skew: f32, x_offset: f32, y_offset: f32) -> Self {
    Self {
      builder: PathBuilder::new(),
      scale,
      skew,
      x_offset,
      y_offset,
    }
  }

  #[inline]
  fn transform_x(&self, x: f32, y: f32) -> f32 {
    (x + self.skew * y) * self.scale + self.x_offset
  }

  #[inline]
  fn transform_y(&self, y: f32) -> f32 {
    self.y_offset - (y * self.scale)
  }

  fn finish(self) -> Option<Path> {
    self.builder.finish()
  }
}

impl ttf_parser::OutlineBuilder for GlyphOutlineBuilder {
  fn move_to(&mut self, x: f32, y: f32) {
    self
      .builder
      .move_to(self.transform_x(x, y), self.transform_y(y));
  }

  fn line_to(&mut self, x: f32, y: f32) {
    self
      .builder
      .line_to(self.transform_x(x, y), self.transform_y(y));
  }

  fn quad_to(&mut self, x1: f32, y1: f32, x: f32, y: f32) {
    self.builder.quad_to(
      self.transform_x(x1, y1),
      self.transform_y(y1),
      self.transform_x(x, y),
      self.transform_y(y),
    );
  }

  fn curve_to(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, x: f32, y: f32) {
    self.builder.cubic_to(
      self.transform_x(x1, y1),
      self.transform_y(y1),
      self.transform_x(x2, y2),
      self.transform_y(y2),
      self.transform_x(x, y),
      self.transform_y(y),
    );
  }

  fn close(&mut self) {
    self.builder.close();
  }
}
