use std::collections::HashSet;
use std::path::Path;
use std::sync::Arc;

use fastrender::paint::display_list::{
  DisplayItem, DisplayList, FontVariation, GlyphInstance, TextItem,
};
use fastrender::paint::display_list_renderer::DisplayListRenderer;
use fastrender::style::color::Rgba;
use fastrender::text::font_db::FontDatabase;
use fastrender::text::pipeline::{Direction, ShapedRun, ShapingPipeline};
use fastrender::{ComputedStyle, FontContext, Point};

fn load_color_font_context() -> (FontContext, String) {
  let font_path = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fonts/ColorTestCOLR.ttf");
  let data = std::fs::read(&font_path).expect("read color test font");

  let mut db = FontDatabase::empty();
  db.load_font_data(data)
    .expect("load color test font into database");

  let family = db
    .faces()
    .next()
    .and_then(|face| face.families.first().map(|(name, _)| name.clone()))
    .expect("font family name");

  assert_eq!(
    db.font_count(),
    1,
    "font database should only contain the color test font"
  );

  (FontContext::with_database(Arc::new(db)), family)
}

fn text_item_from_run(run: &ShapedRun, origin: Point, text_color: Rgba) -> TextItem {
  let glyphs = run
    .glyphs
    .iter()
    .map(|glyph| {
      let x = match run.direction {
        Direction::RightToLeft => origin.x - glyph.x_offset,
        Direction::LeftToRight => origin.x + glyph.x_offset,
      };
      let y = origin.y - glyph.y_offset;
      GlyphInstance {
        glyph_id: glyph.glyph_id,
        offset: Point::new(x - origin.x, y - origin.y),
        advance: glyph.x_advance,
      }
    })
    .collect();

  let mut variations: Vec<FontVariation> = run
    .variations
    .iter()
    .map(|v| FontVariation::new(v.tag, v.value))
    .collect();
  variations.sort_by_key(|v| v.tag);

  TextItem {
    origin,
    cached_bounds: None,
    glyphs,
    color: text_color,
    palette_index: run.palette_index,
    shadows: Vec::new(),
    font_size: run.font_size,
    advance_width: run.advance,
    font: Some(run.font.clone()),
    font_id: None,
    variations,
    synthetic_bold: run.synthetic_bold,
    synthetic_oblique: run.synthetic_oblique,
    emphasis: None,
    decorations: Vec::new(),
  }
}

#[test]
fn paints_colr_v0_glyphs_through_display_list() {
  let (font_ctx, family) = load_color_font_context();

  let mut style = ComputedStyle::default();
  style.font_family = vec![family].into();
  style.font_size = 32.0;
  style.root_font_size = style.font_size;

  let shaper = ShapingPipeline::new();
  let runs = shaper
    .shape("A", &style, &font_ctx)
    .expect("shape color font text");
  let run = runs.first().expect("shaped run exists");
  assert_eq!(
    run.font.family, style.font_family[0],
    "shaping used fallback font"
  );
  assert!(!run.glyphs.is_empty(), "shaping should emit glyphs");

  let text_color = Rgba::BLACK;
  let origin = Point::new(8.0, 40.0);
  let text_item = text_item_from_run(run, origin, text_color);

  let mut list = DisplayList::new();
  list.push(DisplayItem::Text(text_item));

  let renderer =
    DisplayListRenderer::new(64, 64, Rgba::TRANSPARENT, font_ctx).expect("renderer creation");
  let pixmap = renderer.render(&list).expect("render display list");

  let mut painted_colors = HashSet::new();
  let mut has_colored_pixel = false;

  for y in 0..pixmap.height() {
    for x in 0..pixmap.width() {
      let px = pixmap.pixel(x, y).unwrap();
      if px.alpha() == 0 {
        continue;
      }
      let rgb = (px.red(), px.green(), px.blue());
      painted_colors.insert(rgb);
      if rgb.0 != rgb.1 || rgb.1 != rgb.2 {
        has_colored_pixel = true;
      }
    }
  }

  assert!(
    !painted_colors.is_empty(),
    "expected glyph pixels to be painted"
  );
  assert!(
    has_colored_pixel,
    "COLR v0 glyphs should paint palette colors rather than grayscale outlines"
  );
}
