use fastrender::geometry::Point;
use fastrender::paint::display_list::{
  DisplayItem, DisplayList, FontVariation, GlyphInstance, ListMarkerItem, TextItem,
};
use fastrender::paint::display_list_renderer::DisplayListRenderer;
use fastrender::style::types::FontVariationSetting;
use fastrender::text::font_db::FontDatabase;
use fastrender::text::font_loader::FontContext;
use fastrender::text::pipeline::{ShapedRun, ShapingPipeline};
use fastrender::{ComputedStyle, Rgba, TextRasterizer};
use rustybuzz::ttf_parser::Tag;
use std::sync::Arc;
use tiny_skia::Pixmap;

const VARIABLE_FONT_BYTES: &[u8] =
  include_bytes!("fixtures/fonts/VariableTestFont-AmstelvarAlpha.ttf");
const VARIABLE_FAMILY: &str = "AmstelvarAlpha Default";
const FONT_SIZE: f32 = 96.0;
const PIXMAP_SIZE: u32 = 220;
const TEXT: &str = "H";

fn variable_font_context() -> FontContext {
  let mut db = FontDatabase::empty();
  db.load_font_data(VARIABLE_FONT_BYTES.to_vec())
    .expect("variable font fixture loads");
  FontContext::with_database(Arc::new(db))
}

fn style_with_weight(weight: f32) -> ComputedStyle {
  let mut style = ComputedStyle::default();
  style.font_family = vec![VARIABLE_FAMILY.to_string()];
  style.font_size = FONT_SIZE;
  style.font_variation_settings = vec![FontVariationSetting {
    tag: *b"wght",
    value: weight,
  }];
  style
}

fn shape_text(
  shaper: &ShapingPipeline,
  style: &ComputedStyle,
  font_ctx: &FontContext,
) -> ShapedRun {
  let runs = shaper
    .shape(TEXT, style, font_ctx)
    .expect("shape variable font text");
  runs
    .into_iter()
    .next()
    .expect("variable font produces a shaped run")
}

fn wght_variation_value(run: &ShapedRun) -> f32 {
  let wght_tag = Tag::from_bytes(b"wght");
  run
    .variations
    .iter()
    .find(|var| var.tag == wght_tag)
    .map(|var| var.value)
    .expect("shaped run should carry wght variation")
}

fn assert_wght_variation(run: &ShapedRun, expected: f32) {
  let actual = wght_variation_value(run);
  assert!(
    (actual - expected).abs() < f32::EPSILON,
    "expected wght variation {expected} in shaped run, got {actual}"
  );
}

fn text_item_from_run(run: &ShapedRun, origin: Point) -> TextItem {
  let glyphs = run
    .glyphs
    .iter()
    .map(|glyph| GlyphInstance {
      glyph_id: glyph.glyph_id,
      offset: Point::new(glyph.x_offset, glyph.y_offset),
      advance: glyph.x_advance,
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
    glyphs,
    color: Rgba::BLACK,
    palette_index: run.palette_index,
    shadows: Vec::new(),
    font_size: run.font_size,
    advance_width: run.advance,
    font: Some(run.font.clone()),
    font_id: None,
    synthetic_bold: run.synthetic_bold,
    synthetic_oblique: run.synthetic_oblique,
    variations,
    emphasis: None,
    decorations: Vec::new(),
  }
}

fn list_marker_from_run(run: &ShapedRun, origin: Point) -> ListMarkerItem {
  let text = text_item_from_run(run, origin);
  ListMarkerItem {
    origin: text.origin,
    glyphs: text.glyphs,
    color: text.color,
    palette_index: text.palette_index,
    shadows: text.shadows,
    font_size: text.font_size,
    advance_width: text.advance_width,
    font: text.font,
    font_id: None,
    variations: text.variations,
    synthetic_bold: text.synthetic_bold,
    synthetic_oblique: text.synthetic_oblique,
    emphasis: text.emphasis,
    background: None,
  }
}

fn render_display_list(font_ctx: &FontContext, run: &ShapedRun) -> Pixmap {
  let mut list = DisplayList::new();
  let text = text_item_from_run(run, Point::new(20.0, 150.0));
  list.push(DisplayItem::Text(text));

  let renderer = DisplayListRenderer::new(PIXMAP_SIZE, PIXMAP_SIZE, Rgba::WHITE, font_ctx.clone())
    .expect("display list renderer");
  renderer.render(&list).expect("render display list")
}

fn render_list_marker(font_ctx: &FontContext, run: &ShapedRun) -> Pixmap {
  let mut list = DisplayList::new();
  let marker = list_marker_from_run(run, Point::new(20.0, 150.0));
  list.push(DisplayItem::ListMarker(marker));

  let renderer = DisplayListRenderer::new(PIXMAP_SIZE, PIXMAP_SIZE, Rgba::WHITE, font_ctx.clone())
    .expect("display list renderer");
  renderer
    .render(&list)
    .expect("render list marker display list")
}

fn render_with_rasterizer(run: &ShapedRun) -> Pixmap {
  let mut pixmap = Pixmap::new(PIXMAP_SIZE, PIXMAP_SIZE).expect("pixmap");
  pixmap.fill(tiny_skia::Color::WHITE);

  let mut rasterizer = TextRasterizer::new();
  rasterizer
    .render_shaped_run(run, 20.0, 150.0, Rgba::BLACK, &mut pixmap)
    .expect("render shaped run");
  pixmap
}

fn differing_pixels(a: &Pixmap, b: &Pixmap, threshold: u8) -> usize {
  assert_eq!(a.width(), b.width());
  assert_eq!(a.height(), b.height());

  a.data()
    .chunks_exact(4)
    .zip(b.data().chunks_exact(4))
    .filter(|(pa, pb)| {
      pa.iter()
        .zip(pb.iter())
        .any(|(a, b)| a.abs_diff(*b) > threshold)
    })
    .count()
}

#[test]
fn display_list_renderer_applies_variable_font_axes() {
  let font_ctx = variable_font_context();
  let shaper = ShapingPipeline::new();

  let light_run = shape_text(&shaper, &style_with_weight(40.0), &font_ctx);
  let heavy_run = shape_text(&shaper, &style_with_weight(250.0), &font_ctx);
  assert_wght_variation(&light_run, 40.0);
  assert_wght_variation(&heavy_run, 250.0);

  let light = render_display_list(&font_ctx, &light_run);
  let heavy = render_display_list(&font_ctx, &heavy_run);

  let diff = differing_pixels(&light, &heavy, 1);
  assert!(
    diff > 200,
    "variable font text should render differently with distinct axes (diff={diff})"
  );
}

#[test]
fn list_marker_renderer_applies_variable_font_axes() {
  let font_ctx = variable_font_context();
  let shaper = ShapingPipeline::new();

  let light_run = shape_text(&shaper, &style_with_weight(40.0), &font_ctx);
  let heavy_run = shape_text(&shaper, &style_with_weight(250.0), &font_ctx);
  assert_wght_variation(&light_run, 40.0);
  assert_wght_variation(&heavy_run, 250.0);

  let light_marker = render_list_marker(&font_ctx, &light_run);
  let heavy_marker = render_list_marker(&font_ctx, &heavy_run);

  let diff = differing_pixels(&light_marker, &heavy_marker, 1);
  assert!(
    diff > 200,
    "variable font list markers should render differently with distinct axes (diff={diff})"
  );

  let light_text = render_display_list(&font_ctx, &light_run);
  let heavy_text = render_display_list(&font_ctx, &heavy_run);

  assert_eq!(
    light_marker.data(),
    light_text.data(),
    "list marker rendering should match text rendering for the same variations"
  );
  assert_eq!(
    heavy_marker.data(),
    heavy_text.data(),
    "list marker rendering should match text rendering for the same variations"
  );
}

#[test]
fn text_rasterizer_applies_variable_font_axes() {
  let font_ctx = variable_font_context();
  let shaper = ShapingPipeline::new();

  let light_run = shape_text(&shaper, &style_with_weight(40.0), &font_ctx);
  let heavy_run = shape_text(&shaper, &style_with_weight(250.0), &font_ctx);
  assert_wght_variation(&light_run, 40.0);
  assert_wght_variation(&heavy_run, 250.0);

  let light = render_with_rasterizer(&light_run);
  let heavy = render_with_rasterizer(&heavy_run);

  let diff = differing_pixels(&light, &heavy, 1);
  assert!(
    diff > 200,
    "variable font text should render differently with distinct axes (diff={diff})"
  );
}
