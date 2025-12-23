//! Bidirectional text analysis using the shaping pipeline.

use fastrender::style::ComputedStyle;
use fastrender::text::pipeline::BidiAnalysis;
use fastrender::text::pipeline::BidiRun;
use fastrender::text::pipeline::Direction;
use fastrender::text::pipeline::ShapingPipeline;
use fastrender::FontContext;

fn analyze(text: &str, base: Direction) -> BidiAnalysis {
  let style = ComputedStyle::default();
  BidiAnalysis::analyze_with_base(text, &style, base, None)
}

fn run_texts<'a>(runs: &[BidiRun], text: &'a str) -> Vec<&'a str> {
  runs.iter().map(|r| r.text_slice(text)).collect()
}

#[test]
fn pure_ltr_has_single_run() {
  let analysis = analyze("Hello world", Direction::LeftToRight);
  assert!(!analysis.needs_reordering());

  let runs = analysis.logical_runs();
  assert_eq!(runs.len(), 1);
  assert_eq!(runs[0].direction, Direction::LeftToRight);
  assert_eq!(runs[0].text_slice(analysis.text()), "Hello world");
}

#[test]
fn mixed_text_produces_rtl_and_ltr_runs() {
  let analysis = analyze("Hello שלום world", Direction::LeftToRight);
  assert!(analysis.needs_reordering());

  let runs = analysis.logical_runs();
  assert!(runs.iter().any(|r| r.direction == Direction::RightToLeft));
  assert!(runs.iter().any(|r| r.direction == Direction::LeftToRight));
}

#[test]
fn visual_runs_reorder_mixed_content() {
  let analysis = analyze("ABC שלום GHI", Direction::LeftToRight);
  let runs = analysis.visual_runs();

  assert!(runs.len() >= 3);
  let ordered: String = run_texts(&runs, analysis.text()).join("");
  assert!(ordered.contains("ABC"));
  assert!(ordered.contains("שלום"));
  assert!(ordered.contains("GHI"));
  assert!(runs.iter().any(|r| r.direction == Direction::RightToLeft));
}

#[test]
fn paragraph_boundaries_split_runs() {
  let text = "\u{202E}ABC\u{202C}\n\u{202A}DEF\u{202C}";
  let analysis = analyze(text, Direction::LeftToRight);
  let runs = analysis.visual_runs();

  // Runs should stay within their paragraphs.
  assert!(runs.iter().any(|r| r.text_slice(text).contains("ABC")));
  assert!(runs.iter().any(|r| r.text_slice(text).contains("DEF")));
  assert!(runs
    .iter()
    .all(|r| !(r.text_slice(text).contains("ABC") && r.text_slice(text).contains("DEF"))));
}

#[test]
fn inline_bidi_runs_position_in_visual_order() {
  let pipeline = ShapingPipeline::new();
  let font_context = FontContext::new();
  if !font_context.has_fonts() {
    return;
  }
  let style = ComputedStyle::default();
  let text = "Hello שלום world";
  let analysis = analyze(text, Direction::LeftToRight);
  let runs = analysis.visual_runs();
  let mut cursor = 0.0f32;

  for run in runs {
    let shaped = match pipeline.shape(run.text_slice(text), &style, &font_context) {
      Ok(r) => r,
      Err(_) => return,
    };

    for shaped_run in shaped {
      for glyph in shaped_run.glyphs.iter() {
        assert!(glyph.x_offset + cursor >= 0.0);
      }
      cursor += shaped_run.advance;
    }
  }
}
