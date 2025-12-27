use super::{read_u16, read_u24, read_u32, ColorFontCaches, ColorGlyphRaster, FontKey};
use crate::style::color::Rgba;
use crate::text::glyph_path::build_glyph_path;
use std::cmp::Ordering;
use std::sync::{Arc, Mutex};
use tiny_skia::{
  FillRule, GradientStop, LinearGradient, Paint, Path, Pixmap, Point, SpreadMode, Transform,
};
use ttf_parser::colr::{GradientExtend, Paint as ColrPaint, Painter as ColrPainter};

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

#[derive(Clone, Copy)]
struct GlyphBounds {
  min_x: f32,
  min_y: f32,
  max_x: f32,
  max_y: f32,
}

impl GlyphBounds {
  fn include(&mut self, bounds: tiny_skia::Rect) {
    self.min_x = self.min_x.min(bounds.left());
    self.min_y = self.min_y.min(bounds.top());
    self.max_x = self.max_x.max(bounds.right());
    self.max_y = self.max_y.max(bounds.bottom());
  }

  fn expand(&mut self, pad: f32) {
    self.min_x = (self.min_x - pad).floor();
    self.min_y = (self.min_y - pad).floor();
    self.max_x = (self.max_x + pad).ceil();
    self.max_y = (self.max_y + pad).ceil();
  }
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

fn map_spread_mode(extend: GradientExtend) -> SpreadMode {
  match extend {
    GradientExtend::Pad => SpreadMode::Pad,
    GradientExtend::Repeat => SpreadMode::Repeat,
    GradientExtend::Reflect => SpreadMode::Reflect,
  }
}

fn build_outline(face: &ttf_parser::Face<'_>, glyph_id: ttf_parser::GlyphId) -> Option<Path> {
  build_glyph_path(face, glyph_id.0 as u32).and_then(|(path, _)| Some(path))
}

enum PaintStage<'a> {
  Bounds,
  Raster {
    pixmap: &'a mut Pixmap,
    offset: (f32, f32),
  },
}

struct ColrV1Painter<'a, 'b> {
  face: &'a ttf_parser::Face<'a>,
  palette: u16,
  stage: PaintStage<'b>,
  transform_stack: Vec<Transform>,
  current_path: Option<Path>,
  bounds: Option<GlyphBounds>,
}

impl<'a, 'b> ColrV1Painter<'a, 'b> {
  fn new(
    face: &'a ttf_parser::Face<'a>,
    palette: u16,
    base_transform: Transform,
    stage: PaintStage<'b>,
  ) -> Self {
    Self {
      face,
      palette,
      stage,
      transform_stack: vec![base_transform],
      current_path: None,
      bounds: None,
    }
  }

  fn current_transform(&self) -> Transform {
    *self
      .transform_stack
      .last()
      .unwrap_or(&Transform::identity())
  }

  fn transformed_path(&self) -> Option<Path> {
    let transform = self.current_transform();
    let path = self.current_path.as_ref()?.clone();
    path.transform(transform)
  }

  fn update_bounds(&mut self, path: &Path) {
    let rect = path.bounds();
    if !rect.width().is_finite() || !rect.height().is_finite() {
      return;
    }
    match &mut self.bounds {
      Some(bounds) => bounds.include(rect),
      None => {
        self.bounds = Some(GlyphBounds {
          min_x: rect.left(),
          min_y: rect.top(),
          max_x: rect.right(),
          max_y: rect.bottom(),
        });
      }
    }
  }

  fn paint_solid(&mut self, color: ttf_parser::RgbaColor) {
    let Some(path) = self.transformed_path() else {
      return;
    };
    self.update_bounds(&path);

    if let PaintStage::Raster { pixmap, offset } = &mut self.stage {
      let mut paint = Paint::default();
      paint.set_color_rgba8(color.red, color.green, color.blue, color.alpha);
      paint.anti_alias = true;
      let translate = Transform::from_translate(offset.0, offset.1);
      pixmap.fill_path(&path, &paint, FillRule::Winding, translate, None);
    }
  }

  fn paint_linear(&mut self, gradient: ttf_parser::colr::LinearGradient<'_>) {
    let Some(path) = self.transformed_path() else {
      return;
    };
    self.update_bounds(&path);

    let transform = self.current_transform();

    if let PaintStage::Raster { pixmap, offset } = &mut self.stage {
      let mut stops: Vec<_> = gradient.stops(self.palette, &[]).collect();
      stops.sort_by(|a, b| {
        a.stop_offset
          .partial_cmp(&b.stop_offset)
          .unwrap_or(Ordering::Equal)
      });
      if stops.len() < 2 {
        return;
      }

      let spread = map_spread_mode(gradient.extend);
      let shader = LinearGradient::new(
        Point::from_xy(gradient.x0, gradient.y0),
        Point::from_xy(gradient.x1, gradient.y1),
        stops
          .into_iter()
          .map(|stop| {
            GradientStop::new(
              stop.stop_offset,
              tiny_skia::Color::from_rgba8(
                stop.color.red,
                stop.color.green,
                stop.color.blue,
                stop.color.alpha,
              ),
            )
          })
          .collect(),
        spread,
        concat_transforms(Transform::from_translate(offset.0, offset.1), transform),
      );

      if let Some(shader) = shader {
        let mut paint = Paint::default();
        paint.anti_alias = true;
        paint.shader = shader;
        let translate = Transform::from_translate(offset.0, offset.1);
        pixmap.fill_path(&path, &paint, FillRule::Winding, translate, None);
      }
    }
  }
}

impl<'a, 'b> ColrPainter<'a> for ColrV1Painter<'a, 'b> {
  fn outline_glyph(&mut self, glyph_id: ttf_parser::GlyphId) {
    self.current_path = build_outline(self.face, glyph_id);
  }

  fn paint(&mut self, paint: ColrPaint<'a>) {
    match paint {
      ColrPaint::Solid(color) => self.paint_solid(color),
      ColrPaint::LinearGradient(gradient) => self.paint_linear(gradient),
      ColrPaint::RadialGradient(_) | ColrPaint::SweepGradient(_) => {}
    }
  }

  fn push_clip(&mut self) {}

  fn push_clip_box(&mut self, _: ttf_parser::colr::ClipBox) {}

  fn pop_clip(&mut self) {}

  fn push_layer(&mut self, _: ttf_parser::colr::CompositeMode) {}

  fn pop_layer(&mut self) {}

  fn push_transform(&mut self, transform: ttf_parser::Transform) {
    let ts = Transform::from_row(
      transform.a,
      transform.b,
      transform.c,
      transform.d,
      transform.e,
      transform.f,
    );
    let combined = concat_transforms(self.current_transform(), ts);
    self.transform_stack.push(combined);
  }

  fn pop_transform(&mut self) {
    self.transform_stack.pop();
  }
}

pub fn render_colr_glyph(
  face: &ttf_parser::Face<'_>,
  font_key: FontKey,
  glyph_id: ttf_parser::GlyphId,
  font_size: f32,
  palette_index: u16,
  text_color: Rgba,
  synthetic_oblique: f32,
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

  let units_per_em = face.units_per_em() as f32;
  if units_per_em <= 0.0 {
    return None;
  }
  let scale = font_size / units_per_em;
  let base_transform = Transform::from_row(scale, 0.0, synthetic_oblique * scale, -scale, 0.0, 0.0);

  let mut bounds_painter =
    ColrV1Painter::new(face, palette_index, base_transform, PaintStage::Bounds);
  let foreground = ttf_parser::RgbaColor::new(
    text_color.r,
    text_color.g,
    text_color.b,
    text_color.alpha_u8(),
  );
  face.paint_color_glyph(glyph_id, palette_index, foreground, &mut bounds_painter)?;
  let mut bounds = bounds_painter.bounds?;
  bounds.expand(1.0);

  let width = (bounds.max_x - bounds.min_x).max(1.0).round() as u32;
  let height = (bounds.max_y - bounds.min_y).max(1.0).round() as u32;
  let mut pixmap = Pixmap::new(width, height)?;

  let offset = (-bounds.min_x, -bounds.min_y);
  let mut raster_painter = ColrV1Painter::new(
    face,
    palette_index,
    base_transform,
    PaintStage::Raster {
      pixmap: &mut pixmap,
      offset,
    },
  );
  face.paint_color_glyph(glyph_id, palette_index, foreground, &mut raster_painter)?;

  Some(ColorGlyphRaster {
    image: Arc::new(pixmap),
    left: bounds.min_x,
    top: bounds.min_y,
  })
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
