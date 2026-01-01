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

fn generate_css_with_required_and_compounds(
  class_variants: usize,
  class_len: usize,
  attr_len: usize,
) -> String {
  let mut css = generate_css(class_variants, class_len, attr_len);

  // Stress required-AND selector indexing by creating compound selectors with many keys.
  let compound_class_rules = class_variants * 8;
  for i in 0..compound_class_rules {
    let cls_a = long_ident("c", (i.wrapping_mul(17)) % class_variants, class_len);
    let cls_b = long_ident("c", (i.wrapping_mul(31).wrapping_add(1)) % class_variants, class_len);
    let cls_c = long_ident("c", (i.wrapping_mul(47).wrapping_add(2)) % class_variants, class_len);
    let cls_d = long_ident("c", (i.wrapping_mul(71).wrapping_add(3)) % class_variants, class_len);
    let _ = writeln!(
      css,
      ".{cls_a}.{cls_b}.{cls_c}.{cls_d} {{ padding-left: {}px; }}",
      (i % 8) + 1
    );
  }

  let compound_attr_rules = class_variants * 4;
  for i in 0..compound_attr_rules {
    let a = long_ident("data-a", (i.wrapping_mul(13)) % class_variants, attr_len);
    let b = long_ident(
      "data-a",
      (i.wrapping_mul(19).wrapping_add(1)) % class_variants,
      attr_len,
    );
    let c = long_ident(
      "data-a",
      (i.wrapping_mul(23).wrapping_add(2)) % class_variants,
      attr_len,
    );
    let d = long_ident(
      "data-a",
      (i.wrapping_mul(29).wrapping_add(3)) % class_variants,
      attr_len,
    );
    let _ = writeln!(
      css,
      "[{a}][{b}][{c}][{d}] {{ padding-top: {}px; }}",
      (i % 8) + 1
    );
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

  let dom = parse_html(&html).expect("parse html");
  let media = MediaContext::screen(1280.0, 720.0);

  let reps = 10usize;

  for (name, css) in [
    ("simple", generate_css(class_variants, class_len, attr_len)),
    (
      "required_and_compounds",
      generate_css_with_required_and_compounds(class_variants, class_len, attr_len),
    ),
  ] {
    let stylesheet = parse_stylesheet(&css).expect("parse stylesheet");
    let mut bench = SelectorCandidateBench::new(&dom, &stylesheet, &media);
    let mut group = c.benchmark_group(format!("selector_candidates/{name}"));
    group.bench_function("cached", |b| {
      b.iter(|| black_box(bench.run_cached(black_box(reps))));
    });
    group.bench_function("uncached", |b| {
      b.iter(|| black_box(bench.run_uncached(black_box(reps))));
    });
    group.finish();
  }
}

criterion_group!(benches, selector_candidates_benchmark);
criterion_main!(benches);
