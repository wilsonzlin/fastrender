use criterion::black_box;
use criterion::criterion_group;
use criterion::criterion_main;
use criterion::Criterion;
use fastrender::css::parser::parse_stylesheet;
use fastrender::dom::parse_html;
use fastrender::style::cascade::SelectorCandidateBench;
use fastrender::style::media::MediaContext;
use std::fmt::Write;

fn long_ident(prefix: &str, idx: usize, total_len: usize) -> String {
  let mut out = format!("{prefix}{idx:x}_");
  while out.len() < total_len {
    out.push_str("abcdef0123456789");
  }
  out.truncate(total_len);
  out
}

fn generate_html(
  node_count: usize,
  class_variants: usize,
  classes_per_node: usize,
  class_len: usize,
  attrs_per_node: usize,
  attr_len: usize,
) -> String {
  let mut html = String::from("<html><body><div id=\"root\">");
  for i in 0..node_count {
    let mut class_attr = String::new();
    for j in 0..classes_per_node {
      if j > 0 {
        class_attr.push(' ');
      }
      let cls_idx = (i.wrapping_mul(31).wrapping_add(j * 7)) % class_variants;
      class_attr.push_str(&long_ident("c", cls_idx, class_len));
    }

    let mut attrs = String::new();
    for j in 0..attrs_per_node {
      let attr_idx = (i.wrapping_mul(13).wrapping_add(j * 11)) % class_variants;
      let name = long_ident("data-a", attr_idx, attr_len);
      let _ = write!(attrs, " {name}=\"{i}\"", name = name, i = i);
    }

    let _ = write!(
      html,
      "<div class=\"{classes}\"{attrs}></div>",
      classes = class_attr,
      attrs = attrs
    );
  }
  html.push_str("</div></body></html>");
  html
}

fn generate_css(
  class_variants: usize,
  class_len: usize,
  attr_len: usize,
) -> String {
  let mut css = String::from("div { display: block; }\n");
  for i in 0..class_variants {
    let cls = long_ident("c", i, class_len);
    let _ = writeln!(css, ".{cls} {{ color: rgb({},{},{}); }}", i % 255, (i * 3) % 255, (i * 7) % 255);
    let attr = long_ident("data-a", i, attr_len);
    let _ = writeln!(css, "[{attr}] {{ margin-left: {}px; }}", (i % 9) + 1);
  }
  css
}

fn selector_candidates_benchmark(c: &mut Criterion) {
  let node_count = 2000;
  let class_variants = 256;
  let classes_per_node = 8;
  let class_len = 48;
  let attrs_per_node = 24;
  let attr_len = 40;

  let html = generate_html(
    node_count,
    class_variants,
    classes_per_node,
    class_len,
    attrs_per_node,
    attr_len,
  );
  let css = generate_css(class_variants, class_len, attr_len);

  let dom = parse_html(&html).expect("parse html");
  let stylesheet = parse_stylesheet(&css).expect("parse stylesheet");
  let media = MediaContext::screen(1280.0, 720.0);

  let mut bench = SelectorCandidateBench::new(&dom, &stylesheet, &media);
  let reps = 10usize;

  let mut group = c.benchmark_group("selector_candidates");
  group.bench_function("cached", |b| {
    b.iter(|| black_box(bench.run_cached(black_box(reps))));
  });
  group.bench_function("uncached", |b| {
    b.iter(|| black_box(bench.run_uncached(black_box(reps))));
  });
  group.finish();
}

criterion_group!(benches, selector_candidates_benchmark);
criterion_main!(benches);

