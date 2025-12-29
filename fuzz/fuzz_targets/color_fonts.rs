#![no_main]

use std::sync::Arc;

use arbitrary::Unstructured;
use fastrender::style::color::Rgba;
use fastrender::text::color_fonts::ColorFontRenderer;
use fastrender::text::font_db::{FontStretch, FontStyle, FontWeight, LoadedFont};
use fastrender::text::font_instance::FontInstance;
use libfuzzer_sys::fuzz_target;
use rustybuzz::Variation;
use ttf_parser::Tag;

const MIN_FONT_SIZE: f32 = 1.0;
const MAX_FONT_SIZE: f32 = 128.0;
const MIN_OBLIQUE: f32 = -1.0;
const MAX_OBLIQUE: f32 = 1.0;
// Keep font allocations bounded so malformed inputs can't exhaust memory.
const MAX_FONT_BYTES: usize = 2 * 1024 * 1024;

fn bounded_font_size(raw: f32) -> f32 {
  let size = if raw.is_finite() {
    raw.abs()
  } else {
    MIN_FONT_SIZE
  };
  size.clamp(MIN_FONT_SIZE, MAX_FONT_SIZE)
}

fn bounded_oblique(raw: f32) -> f32 {
  if raw.is_finite() {
    raw.clamp(MIN_OBLIQUE, MAX_OBLIQUE)
  } else {
    0.0
  }
}

fn make_font(data: &[u8]) -> LoadedFont {
  let clamped = if data.len() > MAX_FONT_BYTES {
    &data[..MAX_FONT_BYTES]
  } else {
    data
  };

  LoadedFont {
    id: None,
    data: Arc::new(clamped.to_vec()),
    index: 0,
    family: "fuzz-font".to_string(),
    weight: FontWeight::NORMAL,
    style: FontStyle::Normal,
    stretch: FontStretch::Normal,
  }
}

#[derive(Clone, Copy)]
struct AxisRange {
  tag: Tag,
  min: f32,
  default: f32,
  max: f32,
}

fn variation_axes(font: &LoadedFont) -> Vec<AxisRange> {
  if let Ok(face) = ttf_parser::Face::parse(font.data.as_slice(), font.index) {
    return face
      .variation_axes()
      .into_iter()
      .map(|axis| AxisRange {
        tag: axis.tag,
        min: axis.min_value,
        default: axis.def_value,
        max: axis.max_value,
      })
      .collect();
  }
  Vec::new()
}

fn sanitize_variation_value(raw: f32) -> f32 {
  if raw.is_finite() {
    raw.clamp(-1000.0, 1000.0)
  } else {
    0.0
  }
}

fn pick_axis_value(unstructured: &mut Unstructured, axis: AxisRange, raw: f32) -> f32 {
  let clamped = sanitize_variation_value(raw).clamp(axis.min, axis.max);
  let candidates = [axis.min, axis.max, axis.default, clamped];
  *unstructured.choose(&candidates).unwrap_or(&clamped)
}

fn build_variation(
  unstructured: &mut Unstructured,
  axes: &[AxisRange],
) -> Option<Variation> {
  let mut axis = None;
  let tag = if axes.is_empty() {
    let bytes = unstructured.arbitrary::<[u8; 4]>().ok()?;
    Tag::from_bytes(&bytes)
  } else if let Ok(chosen) = unstructured.choose(axes) {
    axis = Some(*chosen);
    chosen.tag
  } else {
    let bytes = unstructured.arbitrary::<[u8; 4]>().ok()?;
    let tag = Tag::from_bytes(&bytes);
    axis = axes.iter().copied().find(|a| a.tag == tag);
    tag
  };

  let raw_value = unstructured.arbitrary::<f32>().unwrap_or(0.0);
  let value = axis
    .map(|axis| pick_axis_value(unstructured, axis, raw_value))
    .unwrap_or_else(|| sanitize_variation_value(raw_value));

  Some(Variation { tag, value })
}

fn build_variations(font: &LoadedFont) -> Vec<Variation> {
  let axes = variation_axes(font);
  let mut unstructured = Unstructured::new(font.data.as_slice());
  let count: u8 = unstructured.int_in_range(0..=4).unwrap_or(0);
  let mut variations = Vec::with_capacity(count as usize);
  for _ in 0..count {
    if let Some(var) = build_variation(&mut unstructured, &axes) {
      variations.push(var);
    }
  }
  variations
}

fuzz_target!(|data: &[u8]| {
  let mut unstructured = Unstructured::new(data);

  let glyph_id = match unstructured.arbitrary::<u32>() {
    Ok(id) => id,
    Err(_) => return,
  };
  let palette_index = match unstructured.arbitrary::<u16>() {
    Ok(idx) => idx,
    Err(_) => return,
  };
  let font_size = match unstructured.arbitrary::<f32>() {
    Ok(size) => bounded_font_size(size),
    Err(_) => return,
  };
  let synthetic_oblique = match unstructured.arbitrary::<f32>() {
    Ok(oblique) => bounded_oblique(oblique),
    Err(_) => 0.0,
  };
  let color_bytes = unstructured
    .arbitrary::<[u8; 4]>()
    .unwrap_or([0, 0, 0, 255]);

  let font_bytes = unstructured.take_rest();
  let font_data = if font_bytes.is_empty() {
    data
  } else {
    font_bytes
  };

  let font = make_font(font_data);
  let variations = build_variations(&font);
  let instance = match FontInstance::new(&font, &variations) {
    Some(instance) => instance,
    None => return,
  };
  let renderer = ColorFontRenderer::new();
  let text_color = Rgba::from_rgba8(
    color_bytes[0],
    color_bytes[1],
    color_bytes[2],
    color_bytes[3],
  );

  let _ = renderer.render(
    &font,
    &instance,
    glyph_id,
    font_size,
    palette_index,
    &[],
    text_color,
    synthetic_oblique,
    &variations,
    None,
  );
});
