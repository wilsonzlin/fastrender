use criterion::{black_box, criterion_group, criterion_main, Criterion};
use fastrender::{FormattingContextFactory, FormattingContextType};

fn bench_factory_cached_vs_create(c: &mut Criterion) {
  let mut group = c.benchmark_group("formatting_context_factory");
  for &fc_type in &[
    FormattingContextType::Block,
    FormattingContextType::Inline,
    FormattingContextType::Flex,
    FormattingContextType::Grid,
  ] {
    let create_factory = FormattingContextFactory::new();
    let cached_factory = FormattingContextFactory::new();
    // Warm the cached path so the benchmark measures Arc clones instead of the first allocation.
    cached_factory.get(fc_type);

    group.bench_function(format!("{:?}/create", fc_type), |b| {
      b.iter(|| {
        let fc = create_factory.create(fc_type);
        black_box(fc);
      })
    });

    group.bench_function(format!("{:?}/get_cached", fc_type), |b| {
      b.iter(|| {
        let fc = cached_factory.get(fc_type);
        black_box(fc.as_ref());
      })
    });
  }
  group.finish();
}

criterion_group!(benches, bench_factory_cached_vs_create);
criterion_main!(benches);
