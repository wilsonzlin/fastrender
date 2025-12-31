use fastrender::paint::text_rasterize::TextRasterizer;
use fastrender::style::color::Rgba;
use fastrender::{
  ComputedStyle, FallbackChain, FontConfig, FontContext, FontDatabase, FontStyleDb as FontStyle,
  FontWeightDb as FontWeight, GenericFamily, ShapingPipeline,
};
use tiny_skia::Pixmap;

#[test]
fn bundled_fonts_render_common_scripts() {
  let mut pipeline = ShapingPipeline::new();
  let font_ctx = FontContext::with_config(FontConfig::bundled_only());

  let samples = [
    ("arabic", "مرحبا بالعالم ١٢٣"),
    ("hebrew", "שלום עולם"),
    ("cyrillic", "Привет мир"),
    ("greek", "Καλημέρα κόσμε"),
    ("devanagari", "नमस्ते दुनिया"),
    ("bengali", "বাংলা লেখা পরীক্ষা"),
    ("thai", "สวัสดีชาวโลก"),
    ("cjk", "界面元素テスト日本語韓国어"),
    ("symbols", "←→↔ ✓✕★⚠"),
  ];

  for (label, text) in samples {
    let mut style = ComputedStyle::default();
    style.font_family = vec!["sans-serif".to_string()];
    style.font_size = 32.0;

    let runs = pipeline
      .shape(text, &style, &font_ctx)
      .unwrap_or_else(|_| panic!("shape {label} sample"));
    assert!(!runs.is_empty(), "{label} sample should produce runs");
    assert!(
      runs
        .iter()
        .flat_map(|run| run.glyphs.iter())
        .all(|g| g.glyph_id != 0),
      "{label} sample should not shape missing glyphs with bundled fonts"
    );

    let mut pixmap = Pixmap::new(512, 160).expect("pixmap");
    let mut rasterizer = TextRasterizer::new();
    rasterizer
      .render_runs(&runs, 8.0, 96.0, Rgba::BLACK, &mut pixmap)
      .unwrap_or_else(|_| panic!("rasterize {label} sample"));

    assert!(
      pixmap.data().iter().any(|&b| b != 0),
      "{label} sample should render with bundled fonts"
    );
  }
}

#[test]
fn bundled_fonts_render_pageset_blocker_clusters() {
  let mut pipeline = ShapingPipeline::new();
  let font_ctx = FontContext::with_config(FontConfig::bundled_only());

  let samples = [
    ("bengali_cluster", "বাং"),
    ("cjk_ideograph", "碅"),
    ("cjk_ideograph_with_combining_mark", "碅\u{036B}"),
    ("arabic_marks", "\u{061A}\u{06DA}"),
  ];

  for (label, text) in samples {
    let mut style = ComputedStyle::default();
    style.font_family = vec!["sans-serif".to_string()];
    style.font_size = 32.0;

    let runs = pipeline
      .shape(text, &style, &font_ctx)
      .unwrap_or_else(|_| panic!("shape {label} sample"));
    assert!(!runs.is_empty(), "{label} sample should produce runs");

    let mut pixmap = Pixmap::new(256, 128).expect("pixmap");
    let mut rasterizer = TextRasterizer::new();
    rasterizer
      .render_runs(&runs, 8.0, 80.0, Rgba::BLACK, &mut pixmap)
      .unwrap_or_else(|_| panic!("rasterize {label} sample"));

    assert!(
      pixmap.data().iter().any(|&b| b != 0),
      "{label} sample should render with bundled fonts"
    );
  }
}

#[test]
fn bundled_fonts_cover_spacing_modifier_letters() {
  let mut pipeline = ShapingPipeline::new();
  let font_ctx = FontContext::with_config(FontConfig::bundled_only());

  let samples = [
    ("sans-serif", "Noto Sans"),
    ("serif", "Noto Serif"),
  ];

  // pageset regression: phoronix.com surfaced clusters containing U+02FD (˽) with combining marks.
  let text = "˽\u{0365}";

  for (family, expected_font) in samples {
    let mut style = ComputedStyle::default();
    style.font_family = vec![family.to_string()];
    style.font_size = 32.0;

    let runs = pipeline
      .shape(text, &style, &font_ctx)
      .unwrap_or_else(|_| panic!("shape {family} sample"));
    assert!(!runs.is_empty(), "{family} sample should produce runs");

    let mut saw_modifier = false;
    for run in &runs {
      if run.text.contains('˽') {
        saw_modifier = true;
        assert_eq!(
          run.font.family, expected_font,
          "U+02FD should be covered by the primary bundled {family} font"
        );
        assert!(
          run.glyphs.iter().all(|g| g.glyph_id != 0),
          "U+02FD cluster should shape without .notdef glyphs"
        );
      }
    }
    assert!(
      saw_modifier,
      "expected a shaped run containing U+02FD for {family}"
    );
  }
}

#[test]
fn bundled_generics_prefer_bundled_text_fonts() {
  let db = FontDatabase::with_config(&FontConfig::bundled_only());

  let sans = db
    .query("sans-serif", FontWeight::NORMAL, FontStyle::Normal)
    .expect("sans-serif bundled fallback");
  let serif = db
    .query("serif", FontWeight::NORMAL, FontStyle::Normal)
    .expect("serif bundled fallback");
  let mono = db
    .query("monospace", FontWeight::NORMAL, FontStyle::Normal)
    .expect("monospace bundled fallback");

  assert_eq!(db.load_font(sans).unwrap().family, "Noto Sans");
  assert_eq!(db.load_font(serif).unwrap().family, "Noto Serif");
  assert_eq!(db.load_font(mono).unwrap().family, "Noto Sans Mono");

  let hebrew = db
    .query("Noto Sans Hebrew", FontWeight::NORMAL, FontStyle::Normal)
    .expect("Noto Sans Hebrew bundled face");
  let thai = db
    .query("Noto Sans Thai", FontWeight::NORMAL, FontStyle::Normal)
    .expect("Noto Sans Thai bundled face");
  assert_eq!(db.load_font(hebrew).unwrap().family, "Noto Sans Hebrew");
  assert_eq!(db.load_font(thai).unwrap().family, "Noto Sans Thai");

  let math = FallbackChain::new()
    .add_generic(GenericFamily::Math)
    .resolve('∫', &db)
    .expect("math generic should resolve");
  let math_family = db.load_font(math.inner()).unwrap().family.to_lowercase();
  assert!(
    math_family.contains("stix"),
    "math generic should use bundled math face"
  );

  let emoji_fonts = db.find_emoji_fonts();
  assert!(
    !emoji_fonts.is_empty(),
    "emoji font should still be discoverable when bundling"
  );
}
