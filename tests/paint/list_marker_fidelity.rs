use fastrender::geometry::Point;
use fastrender::paint::display_list::{
  DisplayItem, DisplayList, FontVariation, GlyphInstance, ListMarkerItem, TextItem,
};
use fastrender::paint::display_list_renderer::DisplayListRenderer;
use fastrender::text::font_db::FontDatabase;
use fastrender::text::font_loader::FontContext;
use fastrender::text::pipeline::RunRotation;
use fastrender::text::pipeline::ShapedRun;
use fastrender::Rgba;
use rustc_hash::FxHasher;
use std::hash::Hash;
use std::hash::Hasher;
use std::path::PathBuf;
use std::sync::Arc;

fn hash_palette_overrides(overrides: &[(u16, Rgba)]) -> u64 {
  let mut hasher = FxHasher::default();
  for (idx, color) in overrides {
    idx.hash(&mut hasher);
    color.r.hash(&mut hasher);
    color.g.hash(&mut hasher);
    color.b.hash(&mut hasher);
    color.alpha_u8().hash(&mut hasher);
  }
  hasher.finish()
}

fn glyph_instances_from_run(run: &ShapedRun) -> Vec<GlyphInstance> {
  run
    .glyphs
    .iter()
    .map(|glyph| GlyphInstance {
      glyph_id: glyph.glyph_id,
      cluster: glyph.cluster,
      x_offset: glyph.x_offset,
      // Display-list glyph offsets use a y-down coordinate system.
      y_offset: -glyph.y_offset,
      x_advance: glyph.x_advance,
      y_advance: glyph.y_advance,
    })
    .collect()
}

fn variations_from_run(run: &ShapedRun) -> Vec<FontVariation> {
  let mut variations: Vec<FontVariation> = run
    .variations
    .iter()
    .map(|v| FontVariation::new(v.tag, v.value))
    .collect();
  variations.sort_by_key(|v| v.tag);
  variations
}

fn text_item_from_run(run: &ShapedRun, origin: Point) -> TextItem {
  TextItem {
    origin,
    cached_bounds: None,
    glyphs: glyph_instances_from_run(run),
    color: Rgba::BLACK,
    palette_index: run.palette_index,
    palette_overrides: run.palette_overrides.clone(),
    palette_override_hash: run.palette_override_hash,
    rotation: run.rotation,
    scale: run.scale,
    shadows: Vec::new(),
    font_size: run.font_size,
    advance_width: run.advance,
    font: Some(run.font.clone()),
    font_id: None,
    variations: variations_from_run(run),
    synthetic_bold: run.synthetic_bold,
    synthetic_oblique: run.synthetic_oblique,
    emphasis: None,
    decorations: Vec::new(),
  }
}

fn list_marker_from_run(run: &ShapedRun, origin: Point) -> ListMarkerItem {
  ListMarkerItem {
    origin,
    cached_bounds: None,
    glyphs: glyph_instances_from_run(run),
    color: Rgba::BLACK,
    palette_index: run.palette_index,
    palette_overrides: run.palette_overrides.clone(),
    palette_override_hash: run.palette_override_hash,
    rotation: run.rotation,
    scale: run.scale,
    shadows: Vec::new(),
    font_size: run.font_size,
    advance_width: run.advance,
    font: Some(run.font.clone()),
    font_id: None,
    variations: variations_from_run(run),
    synthetic_bold: run.synthetic_bold,
    synthetic_oblique: run.synthetic_oblique,
    emphasis: None,
    background: None,
  }
}

fn render_single_item(item: DisplayItem, size: u32) -> tiny_skia::Pixmap {
  let mut list = DisplayList::new();
  list.push(item);
  let font_ctx = FontContext::with_database(Arc::new(FontDatabase::empty()));
  DisplayListRenderer::new(size, size, Rgba::WHITE, font_ctx)
    .expect("display list renderer")
    .render(&list)
    .expect("render display list")
}

#[test]
fn list_marker_renders_identically_to_text_with_palette_overrides_rotation_and_scale() {
  let font_path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fonts/ColorTestCOLR.ttf");
  let bytes = std::fs::read(font_path).expect("read color test font");
  let mut db = FontDatabase::empty();
  db.load_font_data(bytes).expect("load color test font");
  let font = db.first_font().expect("font should be present");

  let mut run = super::color_font_helpers::shaped_run(&font, 'A', 64.0, 0);
  run.rotation = RunRotation::Cw90;

  let compress = 0.7;
  for glyph in &mut run.glyphs {
    glyph.x_offset *= compress;
    glyph.y_offset *= compress;
    glyph.x_advance *= compress;
    glyph.y_advance *= compress;
  }
  run.advance *= compress;
  run.scale *= compress;

  let overrides = vec![(1, Rgba::GREEN)];
  run.palette_override_hash = hash_palette_overrides(&overrides);
  run.palette_overrides = Arc::new(overrides);

  let origin = Point::new(20.0, 100.0);
  let text_item = text_item_from_run(&run, origin);
  let marker_item = list_marker_from_run(&run, origin);

  let text_pixmap = render_single_item(DisplayItem::Text(text_item), 200);
  let marker_pixmap = render_single_item(DisplayItem::ListMarker(marker_item), 200);

  assert_eq!(
    marker_pixmap.data(),
    text_pixmap.data(),
    "list marker rendering should match text rendering when run metadata is preserved"
  );

  let mut base_run = run.clone();
  base_run.palette_overrides = Arc::new(Vec::new());
  base_run.palette_override_hash = 0;
  let base_text = text_item_from_run(&base_run, origin);
  let base_pixmap = render_single_item(DisplayItem::Text(base_text), 200);
  assert_ne!(
    base_pixmap.data(),
    text_pixmap.data(),
    "palette overrides should affect color glyph rasterization"
  );
}

#[test]
fn list_marker_builder_preserves_run_metadata() {
  let font = super::color_font_helpers::load_fixture_font("DejaVuSans-subset.ttf");
  let mut run = super::color_font_helpers::shaped_run(&font, 'F', 32.0, 0);
  run.rotation = RunRotation::Cw90;
  run.scale = 0.5;

  let overrides = vec![(0, Rgba::GREEN), (1, Rgba::RED)];
  run.palette_override_hash = hash_palette_overrides(&overrides);
  run.palette_overrides = Arc::new(overrides.clone());

  let style = Arc::new(fastrender::ComputedStyle::default());
  let fragment = fastrender::tree::fragment_tree::FragmentNode::new_with_style(
    fastrender::geometry::Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
    fastrender::tree::fragment_tree::FragmentContent::Text {
      text: "F".to_string().into(),
      box_id: None,
      baseline_offset: 50.0,
      shaped: Some(Arc::new(vec![run.clone()])),
      is_marker: true,
    },
    Vec::new(),
    style,
  );

  let list = fastrender::paint::display_list_builder::DisplayListBuilder::new().build(&fragment);
  let marker = list
    .items()
    .iter()
    .find_map(|item| match item {
      DisplayItem::ListMarker(marker) => Some(marker),
      _ => None,
    })
    .expect("expected a list marker display item");

  assert_eq!(marker.rotation, run.rotation);
  assert!((marker.scale - run.scale).abs() < f32::EPSILON);
  assert_eq!(marker.palette_override_hash, run.palette_override_hash);
  assert_eq!(marker.palette_overrides.as_ref(), overrides.as_slice());
}
