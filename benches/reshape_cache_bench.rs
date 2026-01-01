use criterion::{black_box, criterion_group, criterion_main, Criterion};
use fastrender::layout::contexts::inline::line_builder::{ReshapeCache, TextItem};
use fastrender::style::types::Direction;
use fastrender::text::font_loader::FontContext;
use fastrender::text::pipeline::ShapingPipeline;
use fastrender::ComputedStyle;
use std::ops::Range;
use std::sync::Arc;

fn prepare_item() -> Option<(TextItem, ShapingPipeline, FontContext, Vec<Range<usize>>)> {
  let mut style = ComputedStyle::default();
  style.font_family = vec!["sans-serif".to_string()];
  style.font_size = 14.0;
  let style = Arc::new(style);

  let font_ctx = FontContext::new();
  let pipeline = ShapingPipeline::new();

  // Keep the string ASCII-only so we can slice at arbitrary byte offsets.
  let text = "ReshapeCache bench text: inline layout splits and retries. ".repeat(256);

  let runs = pipeline.shape(&text, &style, &font_ctx).ok()?;
  if runs.is_empty() {
    return None;
  }

  let metrics = TextItem::metrics_from_runs(&runs, style.font_size, style.font_size);
  let item = TextItem::new(runs, text.clone(), metrics, Vec::new(), Vec::new(), style, Direction::Ltr);

  const SLICE_LEN: usize = 32;
  let ranges: Vec<Range<usize>> = (0..text.len())
    .step_by(SLICE_LEN)
    .take_while(|start| start + SLICE_LEN <= text.len())
    .map(|start| start..start + SLICE_LEN)
    .collect();

  Some((item, pipeline, font_ctx, ranges))
}

fn bench_inline_reshape_cache_hits(c: &mut Criterion) {
  let Some((item, pipeline, font_ctx, ranges)) = prepare_item() else {
    return;
  };

  let mut cache = ReshapeCache::default();
  for r in &ranges {
    black_box(cache.shape(&item, r.clone(), &pipeline, &font_ctx));
  }

  c.bench_function("inline_reshape_cache_hits", |b| {
    b.iter(|| {
      for r in &ranges {
        black_box(cache.shape(&item, r.clone(), &pipeline, &font_ctx));
      }
    })
  });
}

fn bench_inline_reshape_cache_misses(c: &mut Criterion) {
  let Some((item, pipeline, font_ctx, ranges)) = prepare_item() else {
    return;
  };

  // Only sample a small prefix; full reshaping dominates otherwise.
  let ranges: Vec<_> = ranges.into_iter().take(32).collect();

  c.bench_function("inline_reshape_cache_misses", |b| {
    b.iter(|| {
      let mut cache = ReshapeCache::default();
      for r in &ranges {
        black_box(cache.shape(&item, r.clone(), &pipeline, &font_ctx));
      }
    })
  });
}

criterion_group!(
  reshape_cache_benches,
  bench_inline_reshape_cache_hits,
  bench_inline_reshape_cache_misses
);
criterion_main!(reshape_cache_benches);

