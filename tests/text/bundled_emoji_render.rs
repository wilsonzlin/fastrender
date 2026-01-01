use fastrender::paint::text_rasterize::TextRasterizer;
use fastrender::style::color::Rgba;
use fastrender::{
  ComputedStyle, FontConfig, FontContext, FontDatabase, FontStyleDb as FontStyle,
  FontWeightDb as FontWeight, ShapingPipeline,
};
use tiny_skia::Pixmap;

fn fnv1a64(data: &[u8]) -> u64 {
  const OFFSET: u64 = 0xcbf29ce484222325;
  const PRIME: u64 = 0x100000001b3;
  let mut hash = OFFSET;
  for byte in data {
    hash ^= *byte as u64;
    hash = hash.wrapping_mul(PRIME);
  }
  hash
}

#[test]
fn bundled_emoji_renders_with_bundled_fonts() {
  let mut pipeline = ShapingPipeline::new();
  let font_ctx = FontContext::with_config(FontConfig::bundled_only());
  let mut style = ComputedStyle::default();
  style.font_family = vec!["emoji".to_string(), "sans-serif".to_string()].into();
  style.font_size = 64.0;

  let runs = pipeline
    .shape("😀", &style, &font_ctx)
    .expect("emoji should shape");
  assert!(!runs.is_empty());

  let mut pixmap = Pixmap::new(128, 128).expect("pixmap");
  let mut rasterizer = TextRasterizer::new();
  rasterizer
    .render_runs(&runs, 16.0, 96.0, Rgba::BLACK, &mut pixmap)
    .expect("rasterize emoji");

  assert!(
    pixmap.data().iter().any(|&b| b != 0),
    "emoji render should produce pixels"
  );

  let hash = fnv1a64(&pixmap.encode_png().expect("encode emoji render"));
  const EXPECTED_HASH: u64 = 14_075_767_298_342_112_933;
  assert_eq!(hash, EXPECTED_HASH);
}

#[test]
fn bundled_emoji_shapes_us_flag_sequence_as_single_glyph() {
  let mut pipeline = ShapingPipeline::new();
  let font_ctx = FontContext::with_config(FontConfig::bundled_only());
  let mut style = ComputedStyle::default();
  style.font_family = vec!["emoji".to_string(), "sans-serif".to_string()].into();
  style.font_size = 64.0;

  let runs = pipeline
    .shape("🇺🇸", &style, &font_ctx)
    .expect("flag should shape");

  assert!(!runs.is_empty(), "flag should yield at least one shaped run");
  assert_eq!(
    runs[0].font.family, "FastRender Emoji",
    "bundled emoji font should shape the flag sequence"
  );
  assert_eq!(
    runs[0].glyphs.len(),
    1,
    "expected 🇺🇸 to ligate into a single glyph"
  );
}

#[test]
fn generic_fallback_uses_text_font_when_emoji_bundled() {
  let db = FontDatabase::with_config(&FontConfig::bundled_only());
  let emoji_fonts = db.find_emoji_fonts();
  assert!(
    !emoji_fonts.is_empty(),
    "bundled emoji font should be discoverable"
  );

  let primary = db
    .query("sans-serif", FontWeight::NORMAL, FontStyle::Normal)
    .expect("sans-serif fallback present");

  assert!(
    !emoji_fonts.contains(&primary),
    "emoji font must not become the generic fallback"
  );

  let font = db.load_font(primary).expect("load primary font");
  assert!(
    font.family.to_lowercase().contains("noto sans"),
    "text generic should continue using the bundled text face"
  );
  assert!(db.has_glyph(primary, 'A'));
}
