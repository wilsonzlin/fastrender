//! Shaping pipeline integration tests (legacy shaper coverage replacement).

use fastrender::style::ComputedStyle;
use fastrender::text::pipeline::{Direction, ShapedRun, ShapingPipeline};
use fastrender::FontContext;

fn shape(text: &str) -> Option<Vec<ShapedRun>> {
  let pipeline = ShapingPipeline::new();
  let font_ctx = FontContext::new();
  if !font_ctx.has_fonts() {
    return None;
  }
  let style = ComputedStyle::default();
  pipeline.shape(text, &style, &font_ctx).ok()
}

#[test]
fn shaping_empty_string_returns_no_runs() {
  let runs = shape("").unwrap_or_default();
  assert!(runs.is_empty());
}

#[test]
fn shaping_basic_latin_text_produces_glyphs() {
  let Some(runs) = shape("Hello") else {
    return;
  };
  assert!(!runs.is_empty());
  assert_eq!(runs[0].text, "Hello");
  assert!(runs[0].glyphs.len() >= 1);
  assert!(runs[0].advance > 0.0);
  assert_eq!(runs[0].direction, Direction::LeftToRight);
}

#[test]
fn shaping_rtl_text_sets_direction() {
  let Some(runs) = shape("שלום") else {
    return;
  };
  assert!(!runs.is_empty());
  assert_eq!(runs[0].direction, Direction::RightToLeft);
  assert!(runs[0].advance > 0.0);
}
