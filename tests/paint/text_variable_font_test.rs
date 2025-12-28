use fastrender::paint::text_rasterize::{
  glyph_advance_with_variations, render_glyph_with_variations,
};
use fastrender::style::color::Rgba;
use fastrender::text::font_db::{FontStretch, FontStyle, FontWeight, LoadedFont};
use rustybuzz::ttf_parser::Tag;
use rustybuzz::Variation;
use std::sync::Arc;
use tiny_skia::Pixmap;

fn load_variable_font() -> Option<LoadedFont> {
  let data = std::fs::read("tests/fixtures/fonts/VariableTestFont-AmstelvarAlpha.ttf").ok()?;
  Some(LoadedFont {
    data: Arc::new(data),
    index: 0,
    family: "AmstelvarAlpha".to_string(),
    weight: FontWeight::NORMAL,
    style: FontStyle::Normal,
    stretch: FontStretch::Normal,
  })
}

fn axis_extremes(font: &LoadedFont, tag: [u8; 4]) -> Option<(Variation, Variation)> {
  let mut face = font.as_ttf_face().ok()?;
  let axis = face
    .variation_axes()
    .into_iter()
    .find(|axis| axis.tag == Tag::from_bytes(&tag))?;

  Some((
    Variation {
      tag: axis.tag,
      value: axis.min_value,
    },
    Variation {
      tag: axis.tag,
      value: axis.max_value,
    },
  ))
}

fn glyph_for_char(font: &LoadedFont, ch: char) -> Option<u32> {
  let face = font.as_ttf_face().ok()?;
  face.glyph_index(ch).map(|g| g.0 as u32)
}

fn painted_pixels(pixmap: &Pixmap) -> usize {
  pixmap
    .data()
    .chunks(4)
    .filter(|p| p.iter().any(|b| *b != 0))
    .count()
}

#[test]
fn glyph_advance_respects_variations_when_hvar_present() {
  let font = match load_variable_font() {
    Some(font) => font,
    None => return,
  };

  let glyph_id = match glyph_for_char(&font, 'A') {
    Some(id) => id,
    None => return,
  };

  let (min_wght, max_wght) = match axis_extremes(&font, *b"wght") {
    Some(v) => v,
    None => return,
  };

  let light =
    glyph_advance_with_variations(&font, glyph_id, 48.0, &[min_wght]).expect("advance should work");
  let bold =
    glyph_advance_with_variations(&font, glyph_id, 48.0, &[max_wght]).expect("advance should work");

  assert!(
    (light - bold).abs() > 0.01,
    "HVAR should adjust advances across variation coordinates"
  );
}

#[test]
fn render_glyph_with_variations_changes_pixels() {
  let font = match load_variable_font() {
    Some(font) => font,
    None => return,
  };
  let glyph_id = match glyph_for_char(&font, 'A') {
    Some(id) => id,
    None => return,
  };

  let (min_wght, max_wght) = match axis_extremes(&font, *b"wght") {
    Some(v) => v,
    None => return,
  };

  let mut light = Pixmap::new(200, 200).unwrap();
  render_glyph_with_variations(
    &font,
    glyph_id,
    72.0,
    30.0,
    150.0,
    Rgba::BLACK,
    &[min_wght],
    &mut light,
  )
  .unwrap();

  let mut bold = Pixmap::new(200, 200).unwrap();
  render_glyph_with_variations(
    &font,
    glyph_id,
    72.0,
    30.0,
    150.0,
    Rgba::BLACK,
    &[max_wght],
    &mut bold,
  )
  .unwrap();

  let light_pixels = painted_pixels(&light);
  let bold_pixels = painted_pixels(&bold);
  assert!(light_pixels > 0);
  assert!(bold_pixels > 0);
  assert_ne!(light.data(), bold.data());
}
