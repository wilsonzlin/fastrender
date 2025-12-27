use fastrender::image_compare::{compare_images, decode_png, CompareConfig};
use fastrender::style::color::Rgba;
use fastrender::style::types::FontPalette;
use fastrender::style::ComputedStyle;
use fastrender::text::color_fonts::ColorFontRenderer;
use fastrender::text::font_db::FontDatabase;
use fastrender::text::font_loader::FontContext;
use fastrender::text::pipeline::ShapingPipeline;
use std::path::PathBuf;
use std::sync::Arc;

fn font_context() -> FontContext {
  let mut db = FontDatabase::empty();
  let font_path =
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/fonts/PaletteTestCOLRv1.ttf");
  let bytes = std::fs::read(&font_path).expect("fixture font should exist");
  db.load_font_data(bytes).expect("load fixture font");
  FontContext::with_database(Arc::new(db))
}

#[test]
fn font_palette_selects_cpal_palettes_and_renders() {
  let ctx = font_context();
  let pipeline = ShapingPipeline::new();
  let mut base_style = ComputedStyle::default();
  base_style.font_family = vec!["PaletteTestCOLRv1".to_string()];
  base_style.font_size = 72.0;

  let mut normal = base_style.clone();
  normal.font_palette = FontPalette::Normal;
  let normal_runs = pipeline
    .shape("A", &normal, &ctx)
    .expect("shape normal palette");
  assert_eq!(
    normal_runs.len(),
    1,
    "expected a single run for normal palette"
  );
  let normal_run = &normal_runs[0];
  assert_eq!(
    normal_run.palette_index, 0,
    "normal palette should pick first CPAL entry"
  );

  let mut light = base_style.clone();
  light.font_palette = FontPalette::Light;
  let light_runs = pipeline
    .shape("A", &light, &ctx)
    .expect("shape light palette");
  assert_eq!(
    light_runs.len(),
    1,
    "expected a single run for light palette"
  );
  assert_eq!(
    light_runs[0].palette_index, 0,
    "light palette should prefer the light CPAL entry"
  );

  let mut dark = base_style.clone();
  dark.font_palette = FontPalette::Dark;
  let dark_runs = pipeline
    .shape("A", &dark, &ctx)
    .expect("shape dark palette");
  assert_eq!(dark_runs.len(), 1, "expected a single run for dark palette");
  let dark_run = &dark_runs[0];
  assert_eq!(
    dark_run.palette_index, 1,
    "dark palette should prefer dark CPAL entry"
  );

  let renderer = ColorFontRenderer::new();
  let glyph_id = normal_run
    .glyphs
    .first()
    .map(|g| g.glyph_id)
    .expect("glyph id");

  let normal_raster = renderer
    .render(
      &normal_run.font,
      glyph_id,
      normal_run.font_size,
      normal_run.palette_index,
      Rgba::BLACK,
      normal_run.synthetic_oblique,
    )
    .expect("render normal palette");
  let dark_raster = renderer
    .render(
      &dark_run.font,
      glyph_id,
      dark_run.font_size,
      dark_run.palette_index,
      Rgba::BLACK,
      dark_run.synthetic_oblique,
    )
    .expect("render dark palette");

  let normal_png = normal_raster.image.encode_png().expect("encode normal png");
  let dark_png = dark_raster.image.encode_png().expect("encode dark png");
  assert_ne!(
    normal_png, dark_png,
    "palettes should produce distinct renders"
  );

  let golden_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/golden");
  let normal_path = golden_dir.join("font_palette_normal.png");
  let dark_path = golden_dir.join("font_palette_dark.png");

  if std::env::var("UPDATE_GOLDEN").is_ok() {
    std::fs::write(&normal_path, &normal_png).expect("write normal golden");
    std::fs::write(&dark_path, &dark_png).expect("write dark golden");
  }

  let expected_normal = std::fs::read(&normal_path).expect("normal golden should exist");
  let expected_dark = std::fs::read(&dark_path).expect("dark golden should exist");

  let config = CompareConfig::strict();
  let normal_actual = decode_png(&normal_png).expect("decode normal render");
  let normal_expected = decode_png(&expected_normal).expect("decode normal golden");
  let dark_actual = decode_png(&dark_png).expect("decode dark render");
  let dark_expected = decode_png(&expected_dark).expect("decode dark golden");

  let normal_diff = compare_images(&normal_actual, &normal_expected, &config);
  assert!(
    normal_diff.is_match(),
    "normal palette render should match golden: {}",
    normal_diff.summary()
  );

  let dark_diff = compare_images(&dark_actual, &dark_expected, &config);
  assert!(
    dark_diff.is_match(),
    "dark palette render should match golden: {}",
    dark_diff.summary()
  );
}
