use criterion::{black_box, criterion_group, criterion_main, Criterion};
use fastrender::layout::float_context::{FloatContext, FloatSide};

fn build_float_context(count: usize) -> FloatContext {
  let mut ctx = FloatContext::new(200.0);
  for i in 0..count {
    let y = i as f32;
    if i % 2 == 0 {
      ctx.add_float_at(FloatSide::Left, 0.0, y, 80.0, 1.0);
    } else {
      ctx.add_float_at(FloatSide::Right, 120.0, y, 80.0, 1.0);
    }
  }
  ctx
}

fn bench_available_width(c: &mut Criterion) {
  let ctx = build_float_context(5_000);
  c.bench_function("float_available_width_dense", |b| {
    b.iter(|| {
      let mut y = 0.0f32;
      while y < 5_000.0 {
        black_box(ctx.available_width_at_y(y));
        y += 0.5;
      }
    })
  });
}

fn bench_compute_float_position(c: &mut Criterion) {
  c.bench_function("float_place_many", |b| {
    b.iter(|| {
      let mut ctx = build_float_context(2_500);
      let mut y = 0.0f32;
      for _ in 0..200 {
        let (x, placed_y) = ctx.compute_float_position(FloatSide::Left, 40.0, 2.0, y);
        ctx.add_float_at(FloatSide::Left, x, placed_y, 40.0, 2.0);
        y = placed_y;
      }
    })
  });
}

criterion_group!(
  float_benches,
  bench_available_width,
  bench_compute_float_position
);
criterion_main!(float_benches);
