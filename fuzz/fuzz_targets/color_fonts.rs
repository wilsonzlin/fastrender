#![no_main]

use std::sync::Arc;

use arbitrary::Unstructured;
use fastrender::style::color::Rgba;
use fastrender::text::color_fonts::ColorFontRenderer;
use fastrender::text::font_db::{FontStretch, FontStyle, FontWeight, LoadedFont};
use libfuzzer_sys::fuzz_target;

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
    data: Arc::new(clamped.to_vec()),
    index: 0,
    family: "fuzz-font".to_string(),
    weight: FontWeight::NORMAL,
    style: FontStyle::Normal,
    stretch: FontStretch::Normal,
  }
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
  let renderer = ColorFontRenderer::new();
  let text_color = Rgba::from_rgba8(
    color_bytes[0],
    color_bytes[1],
    color_bytes[2],
    color_bytes[3],
  );

  let _ = renderer.render(
    &font,
    glyph_id,
    font_size,
    palette_index,
    text_color,
    synthetic_oblique,
  );
});
