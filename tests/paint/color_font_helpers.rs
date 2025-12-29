use fastrender::geometry::Rect;
use fastrender::paint::display_list_builder::DisplayListBuilder;
use fastrender::paint::display_list_renderer::DisplayListRenderer;
use fastrender::paint::text_rasterize::TextRasterizer;
use fastrender::style::color::Rgba;
use fastrender::text::font_db::FontDatabase;
use fastrender::text::font_db::LoadedFont;
use fastrender::text::font_loader::FontContext;
use fastrender::text::pipeline::{Direction, GlyphPosition, RunRotation, ShapedRun};
use fastrender::tree::fragment_tree::FragmentNode;
use fastrender::ComputedStyle;
use std::path::PathBuf;
use std::sync::Arc;
use tiny_skia::{Color, Pixmap};

fn fixtures_dir() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures")
}

fn fonts_dir() -> PathBuf {
  fixtures_dir().join("fonts")
}

fn font_path(name: &str) -> PathBuf {
  fonts_dir().join(name)
}

pub fn load_fixture_font(name: &str) -> LoadedFont {
  let bytes = std::fs::read(font_path(name)).expect("load font bytes");
  let mut db = FontDatabase::empty();
  db.load_font_data(bytes).expect("load fixture font");
  db.first_font().expect("fixture font present")
}

pub fn shaped_run(font: &LoadedFont, ch: char, font_size: f32, palette_index: u16) -> ShapedRun {
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
    palette_overrides: Arc::new(Vec::new()),
    palette_override_hash: 0,
    variations: Vec::new(),
    scale: 1.0,
  }
}

pub fn render_backends_for_run(
  run: &ShapedRun,
  color: Rgba,
  width: u32,
  height: u32,
  start_x: f32,
  baseline: f32,
) -> (Pixmap, Pixmap) {
  let mut style = ComputedStyle::default();
  style.color = color;
  style.font_size = run.font_size;
  let fragment = FragmentNode::new_text_shaped(
    Rect::from_xywh(start_x, 0.0, width as f32 - start_x, height as f32),
    run.text.clone(),
    baseline,
    vec![run.clone()],
    Arc::new(style),
  );
  let list = DisplayListBuilder::new().build(&fragment);
  let dl_pixmap = DisplayListRenderer::new(width, height, Rgba::WHITE, FontContext::new())
    .and_then(|renderer| renderer.render(&list))
    .expect("render display list text");

  let mut legacy_pixmap = Pixmap::new(width, height).expect("pixmap allocation");
  legacy_pixmap.fill(Color::WHITE);
  let mut rasterizer = TextRasterizer::new();
  rasterizer
    .render_shaped_run(run, start_x, baseline, color, &mut legacy_pixmap)
    .expect("legacy text render");

  (dl_pixmap, legacy_pixmap)
}

pub fn non_white_colors(pixmap: &Pixmap) -> usize {
  pixmap
    .data()
    .chunks_exact(4)
    .filter(|px| {
      let alpha = px[3];
      alpha > 0 && !(px[0] == 255 && px[1] == 255 && px[2] == 255 && alpha == 255)
    })
    .map(|px| (px[2], px[1], px[0]))
    .collect::<std::collections::HashSet<_>>()
    .len()
}
