use criterion::black_box;
use criterion::criterion_group;
use criterion::criterion_main;
use criterion::Criterion;
use fastrender::css::parser::parse_stylesheet;
use fastrender::dom::parse_html;
use fastrender::dom::set_selector_bloom_enabled;
use fastrender::style::cascade::apply_styles_with_media;
use fastrender::style::media::MediaContext;
use std::fmt::Write;

fn build_branching_tree_html(depth: usize, branching: usize, class_variants: usize) -> String {
  fn build_level(
    html: &mut String,
    level: usize,
    max_depth: usize,
    branching: usize,
    class_variants: usize,
  ) {
    if level >= max_depth {
      return;
    }

    let tag = match level % 4 {
      0 => "section",
      1 => "div",
      2 => "article",
      _ => "nav",
    };

    let class_primary = level % class_variants;
    let class_secondary = (level * 7) % class_variants;
    let class_tertiary = (level * 11 + branching) % class_variants;
    let _ = write!(
      html,
      "<{tag} id=\"node{level}\" data-depth=\"{level}\" class=\"c{class_primary} c{class_secondary} c{class_tertiary} layer{layer}\">",
      tag = tag,
      level = level,
      class_primary = class_primary,
      class_secondary = class_secondary,
      class_tertiary = class_tertiary,
      layer = level % 8,
    );

    build_level(html, level + 1, max_depth, branching, class_variants);

    for branch in 1..branching {
      let leaf_class = (level + branch * 5) % class_variants;
      let _ = write!(
        html,
        "<div class=\"c{leaf_class} leaf layer{layer}\" data-branch=\"{branch}\" data-depth=\"{level}\"></div>",
        leaf_class = leaf_class,
        layer = (level + branch) % 8,
        branch = branch,
        level = level
      );
    }

    let _ = write!(html, "</{tag}>", tag = tag);
  }

  let mut html = String::from("<html><head></head><body>");
  build_level(&mut html, 0, depth, branching, class_variants);
  html.push_str("</body></html>");
  html
}

fn generate_descendant_styles(class_variants: usize, chain_lengths: &[usize]) -> String {
  let mut css = String::from("body { margin: 0; }\n");
  for &len in chain_lengths {
    for start in 0..class_variants {
      let chain_a = (0..len)
        .map(|step| format!(".c{}", (start + step) % class_variants))
        .collect::<Vec<_>>()
        .join(" ");
      let _ = write!(
        css,
        "{chain} {{ padding-left: {}px; }}\n",
        (start + len) % 12 + 1,
        chain = chain_a
      );

      let chain_b = (0..len)
        .map(|step| format!(".c{}", (start + step * 5) % class_variants))
        .collect::<Vec<_>>()
        .join(" ");
      let _ = write!(
        css,
        "{chain} {{ border-bottom: {}px solid #ccc; }}\n",
        (start % 5) + 1,
        chain = chain_b
      );
    }
  }

  for start in 0..class_variants {
    let selector = format!(
      "[data-depth] .c{} .layer{} .c{} .c{}",
      start % class_variants,
      start % 8,
      (start * 3 + 7) % class_variants,
      (start * 5 + 11) % class_variants
    );
    let _ = write!(
      css,
      "{selector} {{ margin-inline-start: {}px; }}\n",
      (start % 9) + 2
    );
  }

  for depth in (0..class_variants).step_by(3) {
    let selector = format!(
      "#node{} .c{} .c{}",
      depth,
      (depth + 9) % class_variants,
      (depth + 21) % class_variants
    );
    let _ = write!(
      css,
      "{selector} {{ color: rgb({},{},{}); }}\n",
      (depth * 13) % 255,
      (depth * 7) % 255,
      (depth * 3) % 255
    );
  }

  css
}

fn selector_bloom_benchmark(c: &mut Criterion) {
  let depth = 200;
  let branching = 5;
  let class_variants = 48;
  let chain_lengths = [4, 6, 8];

  let html = build_branching_tree_html(depth, branching, class_variants);
  let css = generate_descendant_styles(class_variants, &chain_lengths);

  let dom = parse_html(&html).expect("parse html");
  let stylesheet = parse_stylesheet(&css).expect("parse stylesheet");
  let media = MediaContext::screen(1280.0, 720.0);

  let mut group = c.benchmark_group("selector_bloom_filter");
  group.bench_function("apply_styles_with_bloom", |b| {
    set_selector_bloom_enabled(true);
    b.iter(|| {
      let styled = apply_styles_with_media(black_box(&dom), black_box(&stylesheet), &media);
      black_box(styled);
    });
  });
  group.bench_function("apply_styles_without_bloom", |b| {
    set_selector_bloom_enabled(false);
    b.iter(|| {
      let styled = apply_styles_with_media(black_box(&dom), black_box(&stylesheet), &media);
      black_box(styled);
    });
  });
  group.finish();
  set_selector_bloom_enabled(true);
}

criterion_group!(benches, selector_bloom_benchmark);
criterion_main!(benches);
