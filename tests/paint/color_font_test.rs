use fastrender::style::color::Rgba;
use fastrender::text::font_db::FontDatabase;
use fastrender::text::font_db::LoadedFont;
use fastrender::text::pipeline::{Direction, GlyphPosition, RunRotation, ShapedRun};
use fastrender::TextRasterizer;
use std::path::PathBuf;
use std::sync::Arc;
use tiny_skia::Pixmap;

use crate::r#ref::image_compare::{compare_config_from_env, compare_pngs, CompareEnvVars};

fn fixtures_dir() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures")
}

fn fonts_dir() -> PathBuf {
  fixtures_dir().join("fonts")
}

fn golden_dir() -> PathBuf {
  fixtures_dir().join("golden")
}

fn diff_dir() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("target/color_font_diffs")
}

fn font_path(name: &str) -> PathBuf {
  fonts_dir().join(name)
}

fn load_fixture_font(name: &str) -> LoadedFont {
  let bytes = std::fs::read(font_path(name)).expect("load font bytes");
  let mut db = FontDatabase::empty();
  db.load_font_data(bytes).expect("load fixture font");
  db.first_font().expect("fixture font present")
}

fn shaped_run(font: &LoadedFont, ch: char, font_size: f32, palette_index: u16) -> ShapedRun {
  let face = font.as_ttf_face().expect("parse font face");
  let glyph_id = face
    .glyph_index(ch)
    .unwrap_or_else(|| panic!("missing glyph for {}", ch))
    .0 as u32;
  let units_per_em = face.units_per_em() as f32;
  let advance = face
    .glyph_hor_advance(ttf_parser::GlyphId(glyph_id as u16))
    .unwrap_or(face.units_per_em()) as f32
    * (font_size / units_per_em);

  let glyphs = vec![GlyphPosition {
    glyph_id,
    cluster: 0,
    x_offset: 0.0,
    y_offset: 0.0,
    x_advance: advance,
    y_advance: 0.0,
  }];

  ShapedRun {
    text: ch.to_string(),
    start: 0,
    end: 1,
    glyphs,
    direction: Direction::LeftToRight,
    level: 0,
    advance,
    font: Arc::new(font.clone()),
    font_size,
    baseline_shift: 0.0,
    language: None,
    synthetic_bold: 0.0,
    synthetic_oblique: 0.0,
    rotation: RunRotation::None,
    palette_index,
    variations: Vec::new(),
    scale: 1.0,
  }
}

fn render_to_png(run: &ShapedRun, color: Rgba, width: u32, height: u32, x: f32, baseline: f32) -> Vec<u8> {
  let mut pixmap = Pixmap::new(width, height).expect("pixmap");
  pixmap.fill(tiny_skia::Color::WHITE);
  let mut rasterizer = TextRasterizer::new();
  rasterizer
    .render_shaped_run(run, x, baseline, color, &mut pixmap)
    .expect("render run");
  pixmap.encode_png().expect("encode png")
}

fn compare_with_golden(name: &str, rendered: &[u8]) {
  let golden_path = golden_dir().join(format!("{}.png", name));
  if std::env::var("UPDATE_GOLDEN").is_ok() {
    std::fs::write(&golden_path, rendered).expect("write golden");
    return;
  }

  let expected = std::fs::read(&golden_path).expect("read golden");
  let config = compare_config_from_env(CompareEnvVars::fixtures()).expect("compare config");
  compare_pngs(name, rendered, &expected, &config, &diff_dir()).expect("pngs match");
}

#[test]
fn colrv1_linear_gradient_matches_golden() {
  let font = load_fixture_font("colrv1-test.ttf");
  let run = shaped_run(&font, 'G', 96.0, 0);
  let rendered = render_to_png(&run, Rgba::BLACK, 180, 180, 20.0, 150.0);
  compare_with_golden("colrv1_light_palette", &rendered);
}

#[test]
fn colrv1_dark_palette_matches_golden() {
  let font = load_fixture_font("colrv1-test.ttf");
  let run = shaped_run(&font, 'G', 96.0, 1);
  let rendered = render_to_png(&run, Rgba::BLACK, 180, 180, 20.0, 150.0);
  compare_with_golden("colrv1_dark_palette", &rendered);
}

#[test]
fn svg_glyph_uses_current_color() {
  let font = load_fixture_font("svg-color-glyph-test.ttf");
  let run = shaped_run(&font, 'A', 64.0, 0);
  let rendered = render_to_png(
    &run,
    Rgba::new(200, 30, 30, 0.8),
    120,
    120,
    10.0,
    90.0,
  );
  compare_with_golden("svg_color_glyph_red", &rendered);
}
