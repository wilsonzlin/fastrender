use criterion::{black_box, criterion_group, criterion_main, Criterion};
use fastrender::style::color::Rgba;
use fastrender::text::font_db::{FontDatabase, LoadedFont};
use fastrender::text::pipeline::{Direction, GlyphPosition, RunRotation, ShapedRun};
use fastrender::TextRasterizer;
use std::path::PathBuf;
use std::sync::Arc;
use tiny_skia::{Color, Pixmap};
use ttf_parser::GlyphId;

fn fixtures_dir() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/fonts")
}

fn load_fixture_font(name: &str) -> LoadedFont {
  let bytes = std::fs::read(fixtures_dir().join(name)).expect("load font bytes");
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
    .glyph_hor_advance(GlyphId(glyph_id as u16))
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
    palette_overrides: Arc::new(Vec::new()),
    palette_override_hash: 0,
    variations: Vec::new(),
    scale: 1.0,
  }
}

fn bench_colr_sweep(c: &mut Criterion) {
  let font = load_fixture_font("colrv1-sweep-test.ttf");
  let run = shaped_run(&font, 'G', 96.0, 0);
  let mut rasterizer = TextRasterizer::new();
  let mut pixmap = Pixmap::new(180, 180).expect("pixmap");

  c.bench_function("colrv1_sweep_gradient_1000x", |b| {
    b.iter(|| {
      pixmap.fill(Color::TRANSPARENT);
      for _ in 0..1000 {
        black_box(
          rasterizer
            .render_shaped_run(&run, 20.0, 150.0, Rgba::BLACK, &mut pixmap)
            .expect("render run"),
        );
      }
    });
  });
}

criterion_group!(colr_sweep, bench_colr_sweep);
criterion_main!(colr_sweep);
