use fastrender::image_compare::{compare_png, CompareConfig};
use fastrender::text::font_db::{FontStretch, FontStyle, FontWeight, LoadedFont};
use fastrender::text::pipeline::GlyphPosition;
use fastrender::{Rgba, TextRasterizer};
use std::path::Path;
use std::sync::Arc;
use tiny_skia::{Color, Pixmap};

fn load_color_font(path: &str, family: &str) -> LoadedFont {
  let full_path = Path::new(env!("CARGO_MANIFEST_DIR")).join(path);
  let data = std::fs::read(&full_path).expect("font bytes");
  LoadedFont {
    data: Arc::new(data),
    index: 0,
    family: family.to_string(),
    weight: FontWeight::NORMAL,
    style: FontStyle::Normal,
    stretch: FontStretch::Normal,
  }
}

fn render_color_glyph(font: &LoadedFont, color: Rgba) -> Pixmap {
  let face = font.as_ttf_face().expect("parse font");
  let glyph_id = face.glyph_index('A').expect("glyph A").0 as u32;
  let glyphs = [GlyphPosition {
    glyph_id,
    cluster: 0,
    x_offset: 0.0,
    y_offset: 0.0,
    x_advance: 0.0,
    y_advance: 0.0,
  }];

  let mut pixmap = Pixmap::new(96, 96).expect("pixmap");
  pixmap.fill(Color::from_rgba8(0, 0, 0, 0));

  let mut rasterizer = TextRasterizer::new();
  rasterizer
    .render_glyphs(&glyphs, font, 64.0, 12.0, 72.0, color, &mut pixmap)
    .expect("render glyph");
  pixmap
}

fn assert_matches_golden(name: &str, png: &[u8]) {
  let golden_path = Path::new(env!("CARGO_MANIFEST_DIR"))
    .join("tests/fixtures/golden")
    .join(name);
  if std::env::var("UPDATE_GOLDENS").is_ok() {
    std::fs::write(&golden_path, png).expect("write golden");
    return;
  }
  let expected = std::fs::read(&golden_path).expect("read golden");
  let diff = compare_png(png, &expected, &CompareConfig::strict()).expect("compare");
  assert!(diff.is_match(), "{}", diff.summary());
}

#[test]
fn palette_glyph_tracks_text_alpha() {
  let font = load_color_font("tests/fonts/ColorTestCOLR.ttf", "ColorTestCOLR");

  let full = render_color_glyph(&font, Rgba::BLACK);
  let quarter = render_color_glyph(
    &font,
    Rgba {
      a: 0.25,
      ..Rgba::BLACK
    },
  );

  assert_matches_golden("color_glyph_palette_full.png", &full.encode_png().unwrap());
  assert_matches_golden(
    "color_glyph_palette_quarter.png",
    &quarter.encode_png().unwrap(),
  );
}

#[test]
fn current_color_alpha_applied_once() {
  let font = load_color_font(
    "tests/fonts/ColorTestCOLRCurrentColor.ttf",
    "ColorTestCOLRCurrentColor",
  );
  let color = Rgba::from_rgba8(0, 255, 0, 128);
  let pixmap = render_color_glyph(&font, color);
  assert_matches_golden(
    "color_glyph_current_color_half.png",
    &pixmap.encode_png().unwrap(),
  );

  let mut min_y = u32::MAX;
  for y in 0..pixmap.height() {
    for x in 0..pixmap.width() {
      if pixmap.pixel(x, y).unwrap().alpha() > 0 {
        min_y = min_y.min(y);
      }
    }
  }

  assert!(
    min_y < u32::MAX,
    "expected to find painted pixels for currentColor glyph"
  );

  let mut top_band_alpha = 0;
  for y in min_y..=min_y.saturating_add(2).min(pixmap.height() - 1) {
    for x in 0..pixmap.width() {
      top_band_alpha = top_band_alpha.max(pixmap.pixel(x, y).unwrap().alpha());
    }
  }

  assert!(
    top_band_alpha >= 120 && top_band_alpha <= 130,
    "top band should keep single application of text alpha, got {}",
    top_band_alpha
  );
}
