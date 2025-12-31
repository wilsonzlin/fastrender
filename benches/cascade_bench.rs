use criterion::black_box;
use criterion::criterion_group;
use criterion::criterion_main;
use criterion::Criterion;
use fastrender::css::parser::parse_stylesheet;
use fastrender::dom::parse_html;
use fastrender::style::cascade::{
  apply_styles_with_media, capture_cascade_profile, cascade_profile_enabled, reset_cascade_profile,
  set_cascade_profile_enabled,
};
use fastrender::style::media::MediaContext;
use std::fmt::Write;
use std::time::Duration;

fn generate_cascade_html(nodes: usize, class_variants: usize) -> String {
  let mut html = String::from("<html><head><style>body{margin:0;padding:0;}</style></head><body>");
  html.push_str("<div id=\"root\" class=\"container\">");
  for i in 0..nodes {
    html.push_str("<div class=\"");
    html.push_str(&format!(
      "item c{} c{}\" id=\"item{}\">",
      i % class_variants,
      (i * 3) % class_variants,
      i
    ));
    html.push_str("<span class=\"label\">");
    html.push_str(&format!("Item {}", i));
    html.push_str("</span><p class=\"copy\">Lorem ipsum dolor sit amet</p></div>");
  }
  html.push_str("</div></body></html>");
  html
}

fn generate_cascade_css(class_variants: usize) -> String {
  let mut css = String::from(
    r#"
        body { font-family: sans-serif; line-height: 1.4; }
        #root.container { display: block; padding: 8px; }
        .container > .item { margin: 4px 0; }
        .item .label { font-weight: 600; }
        .item .copy { color: #444; }
    "#,
  );

  for i in 0..class_variants {
    css.push_str(&format!(
            ".c{0} {{ padding: {1}px; border: {2}px solid hsl({3}, 50%, 60%); color: rgb({4}, {5}, {6}); }}\n",
            i,
            (i % 4) + 1,
            (i % 3) + 1,
            (i * 23) % 360,
            (i * 31) % 255,
            (i * 47) % 255,
            (i * 59) % 255
        ));
    css.push_str(&format!(
      ".c{0} .copy {{ font-size: {1}px; }}\n",
      i,
      12 + (i % 6)
    ));
    css.push_str(&format!(
      ".container .c{0} .label {{ text-transform: uppercase; letter-spacing: {1}px; }}\n",
      i,
      (i % 3) as f32 * 0.25
    ));
  }

  // A few compound selectors to exercise matching work
  css.push_str(
    r#"
        .container .item:hover .label { color: #222; }
        .container .item:nth-child(2n) .copy { text-decoration: underline; }
        .container .item:nth-child(3n) .copy { font-style: italic; }
        .container .item:nth-child(5n) { background: linear-gradient(#fafafa, #f0f0f0); }
    "#,
  );

  css
}

fn generate_not_heavy_html(nodes: usize, class_variants: usize) -> String {
  let mut html = String::from("<html><body><div id=\"root\" class=\"container\">");
  for i in 0..nodes {
    html.push_str("<div class=\"");
    html.push_str(&format!(
      "item c{} c{} block{}\" id=\"item{}\">",
      i % class_variants,
      (i * 3) % class_variants,
      (i * 5) % class_variants,
      i
    ));
    html.push_str("<span class=\"label\">");
    html.push_str(&format!("Item {}", i));
    html.push_str("</span></div>");
  }
  html.push_str("</div></body></html>");
  html
}

fn generate_not_heavy_css(class_variants: usize) -> String {
  let mut css = String::new();

  for i in 0..class_variants {
    css.push_str(&format!(
      ":not(:not(.c{0})) {{ border-width: {1}px; }}\n",
      i,
      (i % 3) + 1
    ));
    css.push_str(&format!(
      ":not(:not(.block{0}), :not(.c{0})) {{ padding: {1}px; }}\n",
      i,
      (i % 5) + 1
    ));
    css.push_str(&format!(
      ":not(:not(.c{0})):not(.block{1}) {{ margin: {2}px; }}\n",
      i,
      (i + 1) % class_variants,
      (i % 7) + 1
    ));
  }

  css
}

fn generate_pseudo_selector_css(rule_count: usize) -> String {
  let mut css = String::new();
  for idx in 0..rule_count {
    let _ = write!(
      css,
      ".c{idx}::before {{ content: \"x\"; color: rgb({r},{g},{b}); }}\n",
      idx = idx,
      r = (idx * 17) % 255,
      g = (idx * 31) % 255,
      b = (idx * 43) % 255
    );
  }
  css
}

fn generate_pseudo_selector_html(nodes: usize, class_variants: usize) -> String {
  let mut html = String::from("<html><body><div id=\"root\">");
  for idx in 0..nodes {
    let _ = write!(
      html,
      "<div class=\"c{cls}\" id=\"n{idx}\">text</div>",
      cls = idx % class_variants,
      idx = idx
    );
  }
  html.push_str("</div></body></html>");
  html
}

fn pseudo_selector_candidate_benchmark(c: &mut Criterion) {
  // Many pseudo-element selectors with distinct classes. With correct indexing, each DOM element
  // should only consider the selectors for its own classes (instead of falling back to universal).
  let rule_count = 10_000usize;
  let node_count = 250usize;
  let class_variants = 10usize;
  let html = generate_pseudo_selector_html(node_count, class_variants);
  let css = generate_pseudo_selector_css(rule_count);

  let dom = parse_html(&html).expect("parse html");
  let stylesheet = parse_stylesheet(&css).expect("parse stylesheet");
  let empty_stylesheet = parse_stylesheet("").expect("parse empty stylesheet");
  let media = MediaContext::screen(1280.0, 720.0);

  // Validate candidate counts once outside the timed loop.
  let prior_profile = cascade_profile_enabled();
  set_cascade_profile_enabled(true);
  reset_cascade_profile();
  let _ = apply_styles_with_media(&dom, &empty_stylesheet, &media);
  let baseline = capture_cascade_profile();

  reset_cascade_profile();
  let _ = apply_styles_with_media(&dom, &stylesheet, &media);
  let stats = capture_cascade_profile();
  set_cascade_profile_enabled(prior_profile);

  let delta_candidates = stats.rule_candidates.saturating_sub(baseline.rule_candidates);
  let delta_universal = stats
    .rule_candidates_universal
    .saturating_sub(baseline.rule_candidates_universal);

  // If pseudo-element selectors are indexed on the pseudo-element compound (usually empty) they'll
  // land in the universal bucket and cause `node_count * rule_count` candidates. Indexing on the
  // originating element's compound keeps this bounded by touched classes.
  assert!(
    delta_universal < 50,
    "pseudo selectors should not contribute many universal candidates (delta_universal={delta_universal})"
  );
  assert!(
    delta_candidates < (node_count as u64) * 20,
    "pseudo selector candidate explosion (delta_candidates={delta_candidates})"
  );

  c.bench_function("cascade apply_styles pseudo selectors 10k rules/250 nodes", |b| {
    b.iter(|| {
      let styled = apply_styles_with_media(black_box(&dom), black_box(&stylesheet), &media);
      black_box(styled);
    });
  });
}

fn cascade_benchmark(c: &mut Criterion) {
  let node_count = 400;
  let class_variants = 24;
  let html = generate_cascade_html(node_count, class_variants);
  let css = generate_cascade_css(class_variants);

  let dom = parse_html(&html).expect("parse html");
  let stylesheet = parse_stylesheet(&css).expect("parse stylesheet");
  let media = MediaContext::screen(1280.0, 720.0);

  c.bench_function("cascade apply_styles 400 nodes/24 classes", |b| {
    b.iter(|| {
      let styled = apply_styles_with_media(black_box(&dom), black_box(&stylesheet), &media);
      black_box(styled);
    });
  });
}

fn cascade_not_benchmark(c: &mut Criterion) {
  let node_count = 300;
  let class_variants = 24;
  let html = generate_not_heavy_html(node_count, class_variants);
  let css = generate_not_heavy_css(class_variants);

  let dom = parse_html(&html).expect("parse html");
  let stylesheet = parse_stylesheet(&css).expect("parse stylesheet");
  let media = MediaContext::screen(1280.0, 720.0);

  c.bench_function("cascade apply_styles not-heavy :not selectors", |b| {
    b.iter(|| {
      let styled = apply_styles_with_media(black_box(&dom), black_box(&stylesheet), &media);
      black_box(styled);
    });
  });
}

fn generate_has_bench_html(depth: usize, fan_out: usize) -> String {
  fn build_branch(
    level: usize,
    depth: usize,
    fan_out: usize,
    place_target: bool,
    html: &mut String,
  ) {
    html.push_str(&format!("<div class=\"branch level{}\">", level));
    if level + 1 == depth {
      if place_target {
        html.push_str("<span class=\"target\"></span>");
      }
    } else {
      for i in 0..fan_out {
        build_branch(
          level + 1,
          depth,
          fan_out,
          place_target && i == fan_out - 1,
          html,
        );
      }
    }
    html.push_str("</div>");
  }

  let mut html = String::from("<div class=\"has-bench\">");
  for i in 0..fan_out {
    build_branch(0, depth, fan_out, i == fan_out - 1, &mut html);
  }
  html.push_str("</div>");
  html
}

fn has_selector_css() -> &'static str {
  r#"
      .has-bench:has(.target) { outline: 1px solid #ccc; }
      .branch:has(.target) { color: #111; }
      .branch:has(> .branch .target) { border-left: 1px solid #ddd; }
      .branch:has(+ .branch .target) { background: #fafafa; }
      .branch:has(.missing) { padding-left: 2px; }
  "#
}

fn has_selector_benchmark(c: &mut Criterion) {
  let depth = 6;
  let fan_out = 3;
  let html = generate_has_bench_html(depth, fan_out);
  let css = has_selector_css();

  let dom = parse_html(&html).expect("parse html");
  let stylesheet = parse_stylesheet(css).expect("parse stylesheet");
  let media = MediaContext::screen(1280.0, 720.0);

  c.bench_function("cascade apply_styles :has depth6 fanout3", |b| {
    b.iter(|| {
      let styled = apply_styles_with_media(black_box(&dom), black_box(&stylesheet), &media);
      black_box(styled);
    });
  });
}

fn generate_candidate_heavy_html(nodes: usize, class_variants: usize) -> String {
  let mut class_attr = String::new();
  for i in 0..class_variants {
    class_attr.push_str(&format!("c{} ", i));
  }
  if !class_attr.is_empty() {
    class_attr.pop();
  }

  let mut html = String::from("<html><body><div id=\"root\">");
  for i in 0..nodes {
    html.push_str(&format!(
      "<div class=\"{}\" id=\"item{}\"></div>",
      class_attr, i
    ));
  }
  html.push_str("</div></body></html>");
  html
}

fn generate_candidate_heavy_css(rules: usize, class_variants: usize) -> String {
  let mut selector_list = String::new();
  for i in 0..class_variants {
    if i > 0 {
      selector_list.push_str(", ");
    }
    selector_list.push_str(&format!(".c{}", i));
  }

  let mut css = String::new();
  for i in 0..rules {
    css.push_str(&selector_list);
    css.push_str(&format!(
      " {{ color: rgb({}, {}, {}); }}\n",
      i % 255,
      (i * 3) % 255,
      (i * 7) % 255
    ));
  }
  css
}

fn candidate_heavy_benchmark(c: &mut Criterion) {
  let node_count = 200;
  let class_variants = 32;
  let rule_count = 256;
  let html = generate_candidate_heavy_html(node_count, class_variants);
  let css = generate_candidate_heavy_css(rule_count, class_variants);

  let dom = parse_html(&html).expect("parse html");
  let stylesheet = parse_stylesheet(&css).expect("parse stylesheet");
  let media = MediaContext::screen(1280.0, 720.0);

  c.bench_function(
    "cascade apply_styles candidate-heavy 200 nodes 256x32 selectors",
    |b| {
      b.iter(|| {
        let styled = apply_styles_with_media(black_box(&dom), black_box(&stylesheet), &media);
        black_box(styled);
      });
    },
  );
}

fn cascade_bench_config() -> Criterion {
  if std::env::var("FASTR_CASCADE_PROFILE")
    .map(|value| !value.trim().is_empty() && value.trim() != "0")
    .unwrap_or(false)
  {
    Criterion::default()
      .sample_size(10)
      .warm_up_time(Duration::from_millis(100))
      .measurement_time(Duration::from_millis(300))
  } else {
    Criterion::default()
  }
}

criterion_group!(
  name = benches;
  config = cascade_bench_config();
  targets =
    cascade_benchmark,
    cascade_not_benchmark,
    has_selector_benchmark,
    pseudo_selector_candidate_benchmark,
    candidate_heavy_benchmark
);
criterion_main!(benches);
