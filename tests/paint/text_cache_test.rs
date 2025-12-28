use fastrender::paint::display_list::{FontVariation as DlFontVariation, GlyphInstance};
use fastrender::{
  Canvas, FontDatabase, FontStyleDb, FontWeightDb, GlyphPosition, LoadedFont, Point, Rect, Rgba,
};

const TEST_FONT_PATH: &str = "tests/fixtures/fonts/DejaVuSans-subset.ttf";

fn load_subset_font() -> LoadedFont {
  let mut db = FontDatabase::empty();
  let data = std::fs::read(TEST_FONT_PATH).expect("test font should be readable");
  db.load_font_data(data).expect("font data should load");
  let id = db
    .query("DejaVu Sans", FontWeightDb::NORMAL, FontStyleDb::Normal)
    .expect("font should be present in database");
  db.load_font(id).expect("font should load")
}

fn glyphs_for_char(font: &LoadedFont, ch: char, font_size: f32) -> Vec<GlyphPosition> {
  let face = font.as_ttf_face().expect("parse font face");
  let glyph_id = face
    .glyph_index(ch)
    .expect("glyph should exist in test font")
    .0 as u32;
  let advance = face
    .glyph_hor_advance(ttf_parser::GlyphId(glyph_id as u16))
    .map(|v| v as f32 * (font_size / face.units_per_em() as f32))
    .unwrap_or(font_size);
  vec![GlyphPosition {
    glyph_id,
    cluster: 0,
    x_offset: 0.0,
    y_offset: 0.0,
    x_advance: advance,
    y_advance: 0.0,
  }]
}

#[test]
fn caches_glyph_outlines_between_draws() {
  let font = load_subset_font();
  let glyphs = glyphs_for_char(&font, 'A', 24.0);
  let instances: Vec<GlyphInstance> = glyphs
    .iter()
    .map(|g| GlyphInstance {
      glyph_id: g.glyph_id,
      offset: Point::new(g.x_offset, g.y_offset),
      advance: g.x_advance,
    })
    .collect();
  let variations: Vec<DlFontVariation> = Vec::new();

  let mut canvas = Canvas::new(64, 64, Rgba::WHITE).expect("canvas");
  assert_eq!(canvas.text_cache_stats().hits, 0);

  canvas.draw_text(
    Point::new(5.0, 40.0),
    &instances,
    &font,
    24.0,
    Rgba::BLACK,
    0.0,
    0.0,
    0,
    &variations,
  );
  let first = canvas.text_cache_stats();
  assert_eq!(first.hits, 0);
  assert!(first.misses > 0);

  canvas.draw_text(
    Point::new(5.0, 40.0),
    &instances,
    &font,
    24.0,
    Rgba::BLACK,
    0.0,
    0.0,
    0,
    &variations,
  );
  let second = canvas.text_cache_stats();
  assert!(
    second.hits > first.hits,
    "expected cache hits to increase after reusing a glyph"
  );
  assert_eq!(second.misses, first.misses);
}

#[test]
fn respects_clip_mask_for_text() {
  let font = load_subset_font();
  let glyphs = glyphs_for_char(&font, 'W', 20.0);
  let instances: Vec<GlyphInstance> = glyphs
    .iter()
    .map(|g| GlyphInstance {
      glyph_id: g.glyph_id,
      offset: Point::new(g.x_offset, g.y_offset),
      advance: g.x_advance,
    })
    .collect();
  let variations: Vec<DlFontVariation> = Vec::new();

  let mut canvas = Canvas::new(80, 50, Rgba::WHITE).expect("canvas");
  canvas.set_clip(Rect::from_xywh(0.0, 0.0, 5.0, 50.0));
  canvas.draw_text(
    Point::new(0.0, 30.0),
    &instances,
    &font,
    20.0,
    Rgba::BLACK,
    0.0,
    0.0,
    0,
    &variations,
  );

  let pixmap = canvas.into_pixmap();
  let width = pixmap.width();
  let mut painted = 0;
  for (idx, px) in pixmap.data().chunks_exact(4).enumerate() {
    if px[0] == 255 && px[1] == 255 && px[2] == 255 && px[3] == 255 {
      continue;
    }
    let x = (idx as u32) % width;
    assert!(
      x < 5,
      "painted pixel at x={x} should be within the clipped region"
    );
    painted += 1;
  }

  assert!(painted > 0, "text should render within the clip mask");
}
