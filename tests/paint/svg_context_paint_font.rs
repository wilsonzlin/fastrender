use crate::r#ref::compare::{compare_images, load_png, CompareConfig};
use fastrender::style::color::Rgba;
use fastrender::text::color_fonts::ColorFontRenderer;
use fastrender::text::font_db::FontDatabase;
use fastrender::text::font_instance::FontInstance;
use std::path::PathBuf;

#[test]
fn svg_context_paint_uses_text_color() {
  let font_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("tests/fixtures/fonts/DejaVuSans-context-paint.ttf");
  let bytes = std::fs::read(&font_path).expect("font bytes");

  let mut db = FontDatabase::empty();
  db.load_font_data(bytes).expect("load font");
  let font = db.first_font().expect("loaded font");
  let face = font.as_ttf_face().expect("parse font face");
  let glyph_id = face.glyph_index('F').expect("glyph id").0 as u32;

  let renderer = ColorFontRenderer::new();
  let text_color = Rgba::from_rgba8(0, 180, 220, 255);
  let instance = FontInstance::new(&font, &[]).expect("font instance");
  let colored = renderer
    .render(
      &font,
      &instance,
      glyph_id,
      64.0,
      0,
      &[],
      text_color,
      0.0,
      &[],
      None,
    )
    .expect("svg glyph");
  let black = renderer
    .render(
      &font,
      &instance,
      glyph_id,
      64.0,
      0,
      &[],
      Rgba::BLACK,
      0.0,
      &[],
      None,
    )
    .expect("svg glyph");

  let golden_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("tests/fixtures/golden/svg_context_paint_font.png");
  if std::env::var("UPDATE_GOLDEN").is_ok() {
    colored
      .image
      .as_ref()
      .save_png(&golden_path)
      .expect("write golden");
  }

  let expected = load_png(&golden_path).expect("load golden");
  let diff = compare_images(colored.image.as_ref(), &expected, &CompareConfig::strict());
  assert!(
    diff.is_match(),
    "context-fill/stroke should resolve to text color: {}",
    diff.summary()
  );

  let color_diff = compare_images(
    colored.image.as_ref(),
    black.image.as_ref(),
    &CompareConfig::strict(),
  );
  assert!(
    !color_diff.is_match(),
    "context paint should change when text color changes"
  );
}
