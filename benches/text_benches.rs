use criterion::{black_box, criterion_group, criterion_main, Criterion};
use fastrender::layout::contexts::inline::line_builder::{InlineItem, LineBuilder, TextItem};
use fastrender::style::types::{Direction, TextWrap, UnicodeBidi, WordBreak};
use fastrender::text::face_cache;
use fastrender::text::font_loader::FontContext;
use fastrender::text::line_break::find_break_opportunities;
use fastrender::text::pipeline::ShapedRun;
use fastrender::text::pipeline::ShapingPipeline;
use fastrender::ComputedStyle;
use fastrender::TextRasterizer;
use std::sync::Arc;
use tiny_skia::Pixmap;
use unicode_bidi::Level;

fn prepare_runs() -> Option<(Vec<ShapedRun>, Pixmap)> {
  let ctx = FontContext::new();
  let mut style = ComputedStyle::default();
  style.font_family = vec!["sans-serif".to_string()];
  style.font_size = 16.0;
  let pipeline = ShapingPipeline::new();
  let text = "FastRender caches parsed faces to avoid reparsing fonts for every run. ".repeat(16);
  let runs = pipeline.shape(&text, &style, &ctx).ok()?;
  if runs.is_empty() {
    return None;
  }
  let pixmap = Pixmap::new(1024, 256)?;
  Some((runs, pixmap))
}

fn bench_rasterize_cached_faces(c: &mut Criterion) {
  let Some((runs, mut pixmap)) = prepare_runs() else {
    return;
  };
  let mut rasterizer = TextRasterizer::new();

  c.bench_function("text_rasterize_cached_faces", |b| {
    b.iter(|| {
      #[cfg(debug_assertions)]
      let _guard = face_cache::FaceParseCountGuard::start();
      pixmap.fill(tiny_skia::Color::TRANSPARENT);
      let _ = black_box(rasterizer.render_runs(
        &runs,
        0.0,
        48.0,
        fastrender::style::color::Rgba::BLACK,
        &mut pixmap,
      ));
      #[cfg(debug_assertions)]
      black_box(face_cache::face_parse_count());
    })
  });
}

fn bench_line_break_dense_paragraph(c: &mut Criterion) {
  let mut style = ComputedStyle::default();
  style.font_family = vec!["sans-serif".to_string()];
  style.font_size = 14.0;
  style.word_break = WordBreak::BreakWord;
  let style = Arc::new(style);
  let font_ctx = FontContext::new();
  let pipeline = ShapingPipeline::new();
  let text = "Benchmarking fast line breaking with dense opportunities. ".repeat(64);

  let mut runs = match pipeline.shape(&text, &style, &font_ctx) {
    Ok(runs) => runs,
    Err(_) => return,
  };
  TextItem::apply_spacing_to_runs(&mut runs, &text, style.letter_spacing, style.word_spacing);
  let metrics = TextItem::metrics_from_runs(&runs, style.font_size, style.font_size);
  let mut item = TextItem::new(
    runs,
    text.clone(),
    metrics,
    find_break_opportunities(&text),
    Vec::new(),
    style,
    Direction::Ltr,
  );
  item.add_breaks_at_clusters();
  let line_width = 120.0;

  c.bench_function("inline_line_break_dense_paragraph", move |b| {
    b.iter(|| {
      let mut builder = LineBuilder::new(
        line_width,
        line_width,
        true,
        TextWrap::Auto,
        0.0,
        false,
        false,
        item.metrics,
        pipeline.clone(),
        font_ctx.clone(),
        Some(Level::ltr()),
        UnicodeBidi::Normal,
        Direction::Ltr,
        None,
        0.0,
      );
      builder.add_item(InlineItem::Text(item.clone()));
      black_box(builder.finish());
    });
  });
}

criterion_group!(
  text_benches,
  bench_rasterize_cached_faces,
  bench_line_break_dense_paragraph
);
criterion_main!(text_benches);
