use fastrender::paint::text_rasterize::{TextRasterizer, TextRenderState};
use fastrender::style::color::Rgba;
use fastrender::text::font_db::{FontStretch, FontStyle, FontWeight, LoadedFont};
use fastrender::text::pipeline::GlyphPosition;
use rustybuzz::Variation;
use std::sync::Arc;
use tiny_skia::Pixmap;
use ttf_parser::Tag;

const VAR_FONT: &[u8] = include_bytes!("../fixtures/fonts/AmstelvarAlpha-VF.ttf");

fn load_variable_font() -> Option<LoadedFont> {
  Some(LoadedFont {
    id: None,
    data: Arc::new(VAR_FONT.to_vec()),
    index: 0,
    family: "AmstelvarAlpha".to_string(),
    weight: FontWeight::NORMAL,
    style: FontStyle::Normal,
    stretch: FontStretch::Normal,
  })
}

fn axis_extremes(font: &LoadedFont, tag: [u8; 4]) -> Option<(Variation, Variation)> {
  let face = font.as_ttf_face().ok()?;
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

fn glyph_for_char(font: &LoadedFont, ch: char) -> Option<(u32, f32)> {
  let face = font.as_ttf_face().ok()?;
  let units_per_em = face.units_per_em() as f32;
  let glyph = face.glyph_index(ch)?;
  let advance = face
    .glyph_hor_advance(glyph)
    .map(|a| a as f32 / units_per_em)
    .unwrap_or(0.0);
  Some((glyph.0 as u32, advance))
}

fn painted_pixels(pixmap: &Pixmap) -> usize {
  pixmap
    .data()
    .chunks(4)
    .filter(|p| p.iter().any(|b| *b != 0))
    .count()
}

#[test]
fn rendering_respects_variation_coordinates() {
  let font = match load_variable_font() {
    Some(font) => font,
    None => return,
  };
  let (glyph_id, advance) = match glyph_for_char(&font, 'A') {
    Some(data) => data,
    None => return,
  };
  let (min_wght, max_wght) = match axis_extremes(&font, *b"wght") {
    Some(v) => v,
    None => return,
  };

  let glyphs = vec![GlyphPosition {
    glyph_id,
    cluster: 0,
    x_offset: 0.0,
    y_offset: 0.0,
    x_advance: advance * 72.0,
    y_advance: 0.0,
  }];

  let mut rasterizer = TextRasterizer::new();
  let mut light = Pixmap::new(200, 200).unwrap();
  rasterizer
    .render_glyphs_with_state_and_palette(
      &glyphs,
      &font,
      72.0,
      30.0,
      150.0,
      Rgba::BLACK,
      0.0,
      0.0,
      0,
      &[min_wght],
      None,
      TextRenderState::default(),
      &mut light,
    )
    .unwrap();

  let mut bold = Pixmap::new(200, 200).unwrap();
  rasterizer
    .render_glyphs_with_state_and_palette(
      &glyphs,
      &font,
      72.0,
      30.0,
      150.0,
      Rgba::BLACK,
      0.0,
      0.0,
      0,
      &[max_wght],
      None,
      TextRenderState::default(),
      &mut bold,
    )
    .unwrap();

  let light_pixels = painted_pixels(&light);
  let bold_pixels = painted_pixels(&bold);
  assert_ne!(
    light_pixels, bold_pixels,
    "variation coordinates should influence rendered glyphs"
  );
}
