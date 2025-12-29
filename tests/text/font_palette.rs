use fastrender::css::types::{FontPaletteBase, FontPaletteOverride, FontPaletteValuesRule};
use fastrender::image_compare::{compare_images, decode_png, CompareConfig};
use fastrender::style::color::{Color, Rgba};
use fastrender::style::font_palette::FontPaletteRegistry;
use fastrender::style::types::FontPalette;
use fastrender::style::ComputedStyle;
use fastrender::text::color_fonts::ColorFontRenderer;
use fastrender::text::font_db::FontDatabase;
use fastrender::text::font_instance::FontInstance;
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
  let normal_instance =
    FontInstance::new(&normal_run.font, &normal_run.variations).expect("normal instance");
  let dark_instance =
    FontInstance::new(&dark_run.font, &dark_run.variations).expect("dark instance");

  let normal_raster = renderer
    .render(
      &normal_run.font,
      &normal_instance,
      glyph_id,
      normal_run.font_size,
      normal_run.palette_index,
      &[],
      Rgba::BLACK,
      normal_run.synthetic_oblique,
      &normal_run.variations,
      None,
    )
    .expect("render normal palette");
  let dark_raster = renderer
    .render(
      &dark_run.font,
      &dark_instance,
      glyph_id,
      dark_run.font_size,
      dark_run.palette_index,
      &[],
      Rgba::BLACK,
      dark_run.synthetic_oblique,
      &dark_run.variations,
      None,
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

#[test]
fn font_palette_overrides_recolor_colr_glyphs() {
  let ctx = font_context();

  let fixed_override = Rgba::from_rgba8(0, 200, 0, 255);
  let mut registry = FontPaletteRegistry::default();
  let mut rule = FontPaletteValuesRule::new("--override-test");
  rule.font_families = vec!["PaletteTestCOLRv1".to_string()];
  rule.base_palette = FontPaletteBase::Index(0);
  rule.overrides = vec![
    FontPaletteOverride {
      index: 0,
      color: Color::Rgba(fixed_override),
    },
    FontPaletteOverride {
      index: 1,
      color: Color::CurrentColor,
    },
  ];
  registry.register(rule);
  let registry = Arc::new(registry);

  let pipeline = ShapingPipeline::new();
  let mut base_style = ComputedStyle::default();
  base_style.font_family = vec!["PaletteTestCOLRv1".to_string()];
  base_style.font_size = 72.0;
  base_style.font_palette = FontPalette::Normal;
  base_style.font_palettes = registry.clone();

  let current_color_a = Rgba::from_rgba8(40, 180, 200, 255);
  let current_color_b = Rgba::from_rgba8(220, 80, 60, 255);
  base_style.color = current_color_a;

  let normal_runs = pipeline
    .shape("A", &base_style, &ctx)
    .expect("shape normal palette");
  assert_eq!(
    normal_runs.len(),
    1,
    "expected a single run for normal palette"
  );
  let normal_run = &normal_runs[0];
  assert!(
    normal_run.palette_overrides.is_empty(),
    "normal palette should not apply overrides"
  );

  let mut override_style_a = base_style.clone();
  override_style_a.font_palette = FontPalette::Named("--override-test".into());
  let mut override_style_b = override_style_a.clone();
  override_style_b.color = current_color_b;

  let override_runs_a = pipeline
    .shape("A", &override_style_a, &ctx)
    .expect("shape override palette (color A)");
  let override_runs_b = pipeline
    .shape("A", &override_style_b, &ctx)
    .expect("shape override palette (color B)");
  assert_eq!(
    override_runs_a.len(),
    1,
    "expected a single run for override palette"
  );
  assert_eq!(
    override_runs_b.len(),
    1,
    "expected a single run for override palette with alternate color"
  );

  let override_run_a = &override_runs_a[0];
  let override_run_b = &override_runs_b[0];

  assert_eq!(
    override_run_a.palette_overrides.as_ref(),
    &vec![(0, fixed_override), (1, current_color_a)],
    "override palette should resolve colors including currentColor"
  );
  assert_eq!(
    override_run_b.palette_overrides.as_ref(),
    &vec![(0, fixed_override), (1, current_color_b)],
    "override palette should re-resolve currentColor when style color changes"
  );

  let renderer = ColorFontRenderer::new();
  let render_run =
    |run: &fastrender::text::pipeline::ShapedRun, text_color: Rgba| -> Vec<u8> {
      let glyph_id = run
        .glyphs
        .first()
        .map(|g| g.glyph_id)
        .expect("glyph id for run");
      let instance = FontInstance::new(&run.font, &run.variations).expect("font instance");
      renderer
        .render(
          &run.font,
          &instance,
          glyph_id,
          run.font_size,
          run.palette_index,
          run.palette_overrides.as_ref(),
          text_color,
          run.synthetic_oblique,
          &run.variations,
          None,
        )
        .expect("render color glyph")
        .image
        .encode_png()
        .expect("encode override png")
    };

  let normal_png = render_run(normal_run, base_style.color);
  let override_png_a = render_run(override_run_a, override_style_a.color);
  let override_png_b = render_run(override_run_b, override_style_b.color);

  assert_ne!(
    normal_png, override_png_a,
    "palette overrides should recolor COLR glyphs"
  );
  assert_ne!(
    override_png_a, override_png_b,
    "currentColor overrides should respond to style color"
  );

  let golden_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/golden");
  let override_path_a = golden_dir.join("font_palette_override_current_color_a.png");
  let override_path_b = golden_dir.join("font_palette_override_current_color_b.png");

  if std::env::var("UPDATE_GOLDEN").is_ok() {
    std::fs::write(&override_path_a, &override_png_a).expect("write override golden A");
    std::fs::write(&override_path_b, &override_png_b).expect("write override golden B");
  }

  let expected_override_a =
    std::fs::read(&override_path_a).expect("override golden A should exist");
  let expected_override_b =
    std::fs::read(&override_path_b).expect("override golden B should exist");

  let config = CompareConfig::strict();
  let override_actual_a = decode_png(&override_png_a).expect("decode override render A");
  let override_expected_a = decode_png(&expected_override_a).expect("decode override golden A");
  let override_actual_b = decode_png(&override_png_b).expect("decode override render B");
  let override_expected_b = decode_png(&expected_override_b).expect("decode override golden B");

  let diff_a = compare_images(&override_actual_a, &override_expected_a, &config);
  assert!(
    diff_a.is_match(),
    "override render should match fixed+currentColor golden A: {}",
    diff_a.summary()
  );

  let diff_b = compare_images(&override_actual_b, &override_expected_b, &config);
  assert!(
    diff_b.is_match(),
    "override render should match fixed+currentColor golden B: {}",
    diff_b.summary()
  );
}
