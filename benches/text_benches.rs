use criterion::{black_box, criterion_group, criterion_main, Criterion};
use fastrender::text::face_cache;
use fastrender::text::font_loader::FontContext;
use fastrender::text::pipeline::ShapingPipeline;
use fastrender::text::ShapedRun;
use fastrender::ComputedStyle;
use fastrender::TextRasterizer;
use tiny_skia::Pixmap;

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

criterion_group!(text_benches, bench_rasterize_cached_faces);
criterion_main!(text_benches);
