use fastrender::text::font_db::{FontStretch, FontStyle, FontWeight, LoadedFont};
use fastrender::text::font_instance::FontInstance;
use rustybuzz::Variation;
use std::sync::Arc;
use ttf_parser::Tag;

const VAR_FONT: &[u8] = include_bytes!("fixtures/fonts/AmstelvarAlpha-VF.ttf");

fn parse_face<'a>() -> ttf_parser::Face<'a> {
  ttf_parser::Face::parse(VAR_FONT, 0).expect("variable font fixture should parse")
}

fn loaded_font() -> LoadedFont {
  LoadedFont {
    data: Arc::new(VAR_FONT.to_vec()),
    index: 0,
    family: "AmstelvarAlpha".to_string(),
    weight: FontWeight::NORMAL,
    style: FontStyle::Normal,
    stretch: FontStretch::Normal,
  }
}

#[test]
fn ttf_parser_reports_variable_axes() {
  let face = parse_face();
  let axes: Vec<_> = face.variation_axes().into_iter().map(|a| a.tag).collect();
  assert!(!axes.is_empty(), "fixture must expose variation axes");
  assert!(axes.contains(&Tag::from_bytes(b"wght")));
}

#[test]
fn ttf_parser_set_variation_has_no_effect_on_bbox_or_outline() {
  // ttf-parser 0.25 exposes set_variation, but it appears to be a no-op for gvar/HVAR.
  let mut face = parse_face();
  let gid = face.glyph_index('A').expect("glyph should exist");

  let bbox_default = face
    .glyph_bounding_box(gid)
    .expect("bbox available without variations");

  let metrics_default = outline_metrics(&face, gid);
  let advance_default = face
    .glyph_hor_advance(gid)
    .expect("advance available without variations");

  face
    .set_variation(Tag::from_bytes(b"wght"), 900.0)
    .expect("axis should be settable");

  let bbox_varied = face
    .glyph_bounding_box(gid)
    .expect("bbox available after variations");
  let metrics_varied = outline_metrics(&face, gid);
  let advance_varied = face
    .glyph_hor_advance(gid)
    .expect("advance available after variations");

  assert_eq!(
    bbox_default, bbox_varied,
    "HVAR/gvar not applied by ttf-parser"
  );
  assert_eq!(
    metrics_default, metrics_varied,
    "outline stays unchanged when applying variations via ttf-parser"
  );
  assert_eq!(
    advance_default, advance_varied,
    "HVAR data should influence advances, but ttf-parser leaves them unchanged"
  );
}

#[test]
fn font_instance_applies_variations_to_outlines_and_metrics() {
  let font = loaded_font();
  let face = parse_face();
  let gid = face.glyph_index('A').expect("glyph should exist").0 as u32;
  let variations = vec![Variation {
    tag: Tag::from_bytes(b"wght"),
    value: 900.0,
  }];

  let default = FontInstance::new(&font, &[]).expect("default instance");
  let varied = FontInstance::new(&font, &variations).expect("varied instance");

  let default_outline = default
    .glyph_outline(gid)
    .expect("default outline must resolve");
  let varied_outline = varied
    .glyph_outline(gid)
    .expect("varied outline must resolve");

  let default_bbox = default_outline.bbox.expect("default bbox");
  let varied_bbox = varied_outline.bbox.expect("varied bbox");

  assert_ne!(
    default_bbox, varied_bbox,
    "gvar-adjusted outlines should differ when variations change"
  );
  assert_ne!(
    default_outline.advance, varied_outline.advance,
    "HVAR-adjusted advance widths should reflect variation changes"
  );
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
struct OutlineMetrics {
  verbs: usize,
  points: usize,
  min_x: i32,
  min_y: i32,
  max_x: i32,
  max_y: i32,
}

struct CountingBuilder {
  metrics: OutlineMetrics,
}

impl CountingBuilder {
  fn new() -> Self {
    Self {
      metrics: OutlineMetrics {
        min_x: i32::MAX,
        min_y: i32::MAX,
        max_x: i32::MIN,
        max_y: i32::MIN,
        ..OutlineMetrics::default()
      },
    }
  }

  fn record_point(&mut self, x: f32, y: f32) {
    let x = x as i32;
    let y = y as i32;
    self.metrics.min_x = self.metrics.min_x.min(x);
    self.metrics.max_x = self.metrics.max_x.max(x);
    self.metrics.min_y = self.metrics.min_y.min(y);
    self.metrics.max_y = self.metrics.max_y.max(y);
  }
}

impl ttf_parser::OutlineBuilder for CountingBuilder {
  fn move_to(&mut self, x: f32, y: f32) {
    self.metrics.verbs += 1;
    self.metrics.points += 1;
    self.record_point(x, y);
  }

  fn line_to(&mut self, x: f32, y: f32) {
    self.metrics.verbs += 1;
    self.metrics.points += 1;
    self.record_point(x, y);
  }

  fn quad_to(&mut self, x1: f32, y1: f32, x: f32, y: f32) {
    self.metrics.verbs += 1;
    self.metrics.points += 2;
    self.record_point(x1, y1);
    self.record_point(x, y);
  }

  fn curve_to(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, x: f32, y: f32) {
    self.metrics.verbs += 1;
    self.metrics.points += 3;
    self.record_point(x1, y1);
    self.record_point(x2, y2);
    self.record_point(x, y);
  }

  fn close(&mut self) {
    self.metrics.verbs += 1;
  }
}

fn outline_metrics(face: &ttf_parser::Face<'_>, gid: ttf_parser::GlyphId) -> OutlineMetrics {
  let mut builder = CountingBuilder::new();
  face
    .outline_glyph(gid, &mut builder)
    .expect("outline should be present");
  builder.metrics
}
