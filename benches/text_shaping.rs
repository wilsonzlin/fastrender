use criterion::black_box;
use criterion::criterion_group;
use criterion::criterion_main;
use criterion::Criterion;
use fastrender::style::ComputedStyle;
use fastrender::text::font_loader::FontContext;
use fastrender::text::pipeline::ShapingPipeline;

fn mixed_script_text() -> String {
  let sample =
    "The quick brown fox jumps over 13 lazy dogs. Καλημέρα κόσμε. Привет мир. مرحبا بالعالم.";
  sample.repeat(48)
}

fn shape_long_mixed_text(c: &mut Criterion) {
  let mut style = ComputedStyle::default();
  style.font_family = vec!["serif".to_string(), "sans-serif".to_string()];

  let ctx = FontContext::new();
  let pipeline = ShapingPipeline::new();
  let base = mixed_script_text();
  let mut text = base.clone();
  let base_len = text.len();
  let mut counter = 0u64;

  c.bench_function("shape long mixed-script text", |b| {
    b.iter(|| {
      counter = counter.wrapping_add(1);
      text.truncate(base_len);
      text.push_str(&counter.to_string());
      let shaped = pipeline
        .shape(black_box(&text), &style, &ctx)
        .expect("shape long text");
      black_box(shaped.len());
    });
  });
}

criterion_group!(text_benches, shape_long_mixed_text);
criterion_main!(text_benches);
