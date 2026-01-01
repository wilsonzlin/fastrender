use criterion::{black_box, criterion_group, criterion_main, Criterion, Throughput};
use fastrender::geometry::Point;
use fastrender::paint::gradient::{gradient_bucket, rasterize_linear_gradient, GradientLutCache};
use fastrender::style::color::Rgba;
use tiny_skia::SpreadMode;

fn bench_linear_gradient_large(c: &mut Criterion) {
  // ~8.4M pixels: representative of the `about.gitlab.com` hotspot (~10M gradient pixels).
  let width: u32 = 4096;
  let height: u32 = 2048;
  let pixels = u64::from(width) * u64::from(height);

  let stops = vec![
    (0.0, Rgba::new(0, 102, 255, 1.0)),
    (0.4, Rgba::new(245, 64, 64, 1.0)),
    (1.0, Rgba::new(255, 220, 0, 1.0)),
  ];
  let start = Point::new(0.0, 0.0);
  let end = Point::new(width as f32, 0.0);
  let cache = GradientLutCache::default();
  let bucket = gradient_bucket(width.max(height));

  let mut group = c.benchmark_group("gradient_raster");
  group.sample_size(10);
  group.throughput(Throughput::Elements(pixels));
  group.bench_function("linear_pad_cached_lut_4096x2048", |b| {
    b.iter(|| {
      black_box(
        rasterize_linear_gradient(
          width,
          height,
          start,
          end,
          SpreadMode::Pad,
          &stops,
          &cache,
          bucket,
        )
        .expect("rasterize")
        .expect("pixmap"),
      );
    })
  });
  group.finish();
}

criterion_group!(benches, bench_linear_gradient_large);
criterion_main!(benches);
