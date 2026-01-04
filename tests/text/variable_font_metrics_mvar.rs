use fastrender::text::font_db::{FontStretch, FontStyle, FontWeight, LoadedFont};
use fastrender::text::font_loader::FontContext;
use rustybuzz::Variation;
use std::sync::Arc;
use ttf_parser::Tag;

const VAR_FONT: &[u8] = include_bytes!("../fixtures/fonts/mvar-metrics-test.ttf");

fn loaded_font() -> LoadedFont {
  LoadedFont {
    id: None,
    data: Arc::new(VAR_FONT.to_vec()),
    index: 0,
    family: "MVAR Metrics Test".to_string(),
    weight: FontWeight::NORMAL,
    style: FontStyle::Normal,
    stretch: FontStretch::Normal,
    face_metrics_overrides: Default::default(),
  }
}

#[test]
fn variable_font_metrics_apply_mvar_variations() {
  let font = loaded_font();
  let ctx = FontContext::empty();

  let font_size = 48.0;
  let light = vec![Variation {
    tag: Tag::from_bytes(b"wght"),
    value: 100.0,
  }];
  let heavy = vec![Variation {
    tag: Tag::from_bytes(b"wght"),
    value: 900.0,
  }];

  let a = ctx
    .get_scaled_metrics_with_variations(&font, font_size, &light)
    .expect("scaled metrics for wght=100");
  let b = ctx
    .get_scaled_metrics_with_variations(&font, font_size, &heavy)
    .expect("scaled metrics for wght=900");

  let coords_a: Vec<_> = light.iter().map(|v| (v.tag, v.value)).collect();
  let coords_b: Vec<_> = heavy.iter().map(|v| (v.tag, v.value)).collect();
  let raw_a = font
    .metrics_with_variations(&coords_a)
    .expect("raw metrics for first coordinate set");
  let raw_b = font
    .metrics_with_variations(&coords_b)
    .expect("raw metrics for second coordinate set");

  let changed = (a.ascent - b.ascent).abs() > 0.01
    || (a.descent - b.descent).abs() > 0.01
    || (a.line_gap - b.line_gap).abs() > 0.01
    || (a.line_height - b.line_height).abs() > 0.01
    || (a.underline_position - b.underline_position).abs() > 0.01
    || (a.underline_thickness - b.underline_thickness).abs() > 0.01;

  assert!(
    changed,
    "expected variable font MVAR metrics to change across variations.\nA={a:?}\nB={b:?}\nraw A={raw_a:?}\nraw B={raw_b:?}"
  );
}
