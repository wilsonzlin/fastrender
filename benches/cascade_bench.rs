use criterion::black_box;
use criterion::criterion_group;
use criterion::criterion_main;
use criterion::Criterion;
use fastrender::css::parser::parse_stylesheet;
use fastrender::css::types::PropertyValue;
use fastrender::dom::{parse_html, DomNode, DomNodeType, HTML_NAMESPACE};
use fastrender::style::cascade::{
  apply_starting_style_set_with_media_target_and_imports_cached_with_deadline,
  apply_styles_with_media, capture_cascade_profile, cascade_profile_enabled, reset_cascade_profile,
  set_cascade_profile_enabled,
};
use fastrender::style::media::MediaContext;
use fastrender::style::style_set::StyleSet;
use selectors::context::QuirksMode;
use std::alloc::{GlobalAlloc, Layout, System};
use std::fmt::Write;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::time::{Duration, Instant};

struct CountingAllocator;

static ALLOC_CALLS: AtomicUsize = AtomicUsize::new(0);
static ALLOC_BYTES: AtomicUsize = AtomicUsize::new(0);

unsafe impl GlobalAlloc for CountingAllocator {
  unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
    ALLOC_CALLS.fetch_add(1, Ordering::Relaxed);
    ALLOC_BYTES.fetch_add(layout.size(), Ordering::Relaxed);
    System.alloc(layout)
  }

  unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
    ALLOC_CALLS.fetch_add(1, Ordering::Relaxed);
    ALLOC_BYTES.fetch_add(layout.size(), Ordering::Relaxed);
    System.alloc_zeroed(layout)
  }

  unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
    ALLOC_CALLS.fetch_add(1, Ordering::Relaxed);
    ALLOC_BYTES.fetch_add(new_size, Ordering::Relaxed);
    System.realloc(ptr, layout, new_size)
  }

  unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
    System.dealloc(ptr, layout)
  }
}

#[global_allocator]
static GLOBAL: CountingAllocator = CountingAllocator;

fn allocation_counts() -> (usize, usize) {
  (
    ALLOC_CALLS.load(Ordering::Relaxed),
    ALLOC_BYTES.load(Ordering::Relaxed),
  )
}

fn generate_inline_style_html(nodes: usize) -> String {
  let mut html = String::from("<html><body><div id=\"root\">");
  for i in 0..nodes {
    let padding = (i % 8) + 1;
    let margin = (i % 6) + 1;
    let r = (i * 31) % 255;
    let g = (i * 47) % 255;
    let b = (i * 59) % 255;
    html.push_str(&format!(
      "<div class=\"item\" style=\"padding: {padding}px {padding}px; margin: {margin}px; font-size: 12px; line-height: 1.2; color: rgb({r}, {g}, {b}); background-color: rgb({b}, {r}, {g});\">item {i}</div>"
    ));
  }
  html.push_str("</div></body></html>");
  html
}

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

fn generate_balanced_dom(depth: usize, fan_out: usize, payload_bytes: usize) -> (DomNode, usize) {
  let payload = "x".repeat(payload_bytes);

  fn build_subtree(
    current_depth: usize,
    max_depth: usize,
    fan_out: usize,
    payload: &str,
    node_count: &mut usize,
  ) -> DomNode {
    *node_count += 1;
    let children = if current_depth < max_depth {
      (0..fan_out)
        .map(|_| build_subtree(current_depth + 1, max_depth, fan_out, payload, node_count))
        .collect()
    } else {
      Vec::new()
    };

    DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("data-payload".to_string(), payload.to_string())],
      },
      children,
    }
  }

  let mut node_count = 0usize;
  let body_child = build_subtree(0, depth, fan_out, &payload, &mut node_count);
  let dom = DomNode {
    node_type: DomNodeType::Document {
      quirks_mode: QuirksMode::NoQuirks,
    },
    children: vec![DomNode {
      node_type: DomNodeType::Element {
        tag_name: "html".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: Vec::new(),
      },
      children: vec![DomNode {
        node_type: DomNodeType::Element {
          tag_name: "body".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: Vec::new(),
        },
        children: vec![body_child],
      }],
    }],
  };

  (dom, node_count)
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

  let delta_candidates = stats
    .rule_candidates
    .saturating_sub(baseline.rule_candidates);
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

  c.bench_function(
    "cascade apply_styles pseudo selectors 10k rules/250 nodes",
    |b| {
      b.iter(|| {
        let styled = apply_styles_with_media(black_box(&dom), black_box(&stylesheet), &media);
        black_box(styled);
      });
    },
  );
}

fn generate_layer_heavy_css(layers: usize, rules_per_layer: usize) -> String {
  let mut css = String::new();

  for layer_idx in 0..layers {
    css.push_str(&format!("@layer l{layer_idx} {{ @layer a {{ @layer b {{\n"));
    for rule_idx in 0..rules_per_layer {
      // Use a mix of custom properties and a couple of normal properties to exercise both
      // cascade passes in `apply_cascaded_declarations`.
      css.push_str("#target{");
      css.push_str(&format!("--l{layer_idx}_r{rule_idx}: {rule_idx};"));
      css.push_str(&format!(
        "color: rgb({}, {}, {});",
        (layer_idx * 31) % 255,
        (rule_idx * 47) % 255,
        (layer_idx * 11 + rule_idx * 13) % 255
      ));
      css.push_str("margin: 0; padding: 0;}");
      css.push('\n');
    }
    css.push_str("}}}\n}\n");
  }

  css
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

fn cascade_layer_heavy_benchmark(c: &mut Criterion) {
  let layers = 64;
  let rules_per_layer = 16;
  let html = "<html><body><div id=\"target\"></div></body></html>";
  let css = generate_layer_heavy_css(layers, rules_per_layer);

  let dom = parse_html(html).expect("parse html");
  let stylesheet = parse_stylesheet(&css).expect("parse stylesheet");
  let media = MediaContext::screen(1280.0, 720.0);
  static PRINTED: AtomicBool = AtomicBool::new(false);

  c.bench_function("cascade apply_styles layer-heavy 64x16", |b| {
    b.iter_custom(|iters| {
      let (calls_start, bytes_start) = allocation_counts();
      let start = Instant::now();
      for _ in 0..iters {
        let styled = apply_styles_with_media(black_box(&dom), black_box(&stylesheet), &media);
        black_box(styled);
      }
      let duration = start.elapsed();
      let (calls_end, bytes_end) = allocation_counts();
      let calls = calls_end.saturating_sub(calls_start);
      let bytes = bytes_end.saturating_sub(bytes_start);
      black_box((calls, bytes));

      if !PRINTED.swap(true, Ordering::Relaxed) {
        let iters_usize = iters as usize;
        let per_call_calls = calls / iters_usize.max(1);
        let per_call_bytes = bytes / iters_usize.max(1);
        eprintln!("layer-heavy allocations/call: calls={per_call_calls} bytes={per_call_bytes}");
      }

      duration
    });
  });
}

fn cascade_deep_dom_benchmark(c: &mut Criterion) {
  // Stress the cost of cloning DOM subtrees by constructing a balanced tree where each node carries
  // a large attribute payload. A balanced tree keeps recursion depth bounded while still producing
  // thousands of nodes.
  let depth = std::env::var("FASTR_CASCADE_DEEP_DEPTH")
    .ok()
    .and_then(|v| v.trim().parse::<usize>().ok())
    .unwrap_or(10);
  let fan_out = std::env::var("FASTR_CASCADE_DEEP_FAN_OUT")
    .ok()
    .and_then(|v| v.trim().parse::<usize>().ok())
    .unwrap_or(2);
  let payload_bytes = std::env::var("FASTR_CASCADE_DEEP_PAYLOAD_BYTES")
    .ok()
    .and_then(|v| v.trim().parse::<usize>().ok())
    .unwrap_or(512);

  let (dom, node_count) = generate_balanced_dom(depth, fan_out, payload_bytes);
  let stylesheet = parse_stylesheet("div { color: #222; }").expect("parse stylesheet");
  let media = MediaContext::screen(1280.0, 720.0);
  static PRINTED: AtomicBool = AtomicBool::new(false);

  let id = format!(
    "cascade apply_styles deep-dom depth{depth} fan{fan_out} nodes{node_count} payload{payload_bytes}B"
  );
  c.bench_function(&id, |b| {
    b.iter_custom(|iters| {
      let (calls_start, bytes_start) = allocation_counts();
      let start = Instant::now();
      for _ in 0..iters {
        let styled = apply_styles_with_media(black_box(&dom), black_box(&stylesheet), &media);
        black_box(styled);
      }
      let duration = start.elapsed();
      let (calls_end, bytes_end) = allocation_counts();
      let calls = calls_end.saturating_sub(calls_start);
      let bytes = bytes_end.saturating_sub(bytes_start);
      black_box((calls, bytes));

      if !PRINTED.swap(true, Ordering::Relaxed) {
        let iters_usize = iters as usize;
        let per_call_calls = calls / iters_usize.max(1);
        let per_call_bytes = bytes / iters_usize.max(1);
        eprintln!("deep-dom allocations/call: calls={per_call_calls} bytes={per_call_bytes}");
      }

      duration
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

fn inline_style_starting_style_benchmark(c: &mut Criterion) {
  let node_count = 1500;
  let html = generate_inline_style_html(node_count);
  // Include a @starting-style rule to trigger the second cascade pass that previously
  // re-parsed each node's inline `style=""` attribute.
  let css = r#"
    @starting-style {
      .item { opacity: 0.5; }
    }
  "#;

  let dom = parse_html(&html).expect("parse html");
  let stylesheet = parse_stylesheet(css).expect("parse stylesheet");
  let style_set = StyleSet::from_document(stylesheet);
  let media = MediaContext::screen(1280.0, 720.0);

  c.bench_function(
    "cascade apply_styles @starting-style inline-style heavy",
    |b| {
      b.iter(|| {
        let styled = apply_starting_style_set_with_media_target_and_imports_cached_with_deadline(
          black_box(&dom),
          black_box(&style_set),
          &media,
          None,
          None,
          None,
          None,
          None,
          None,
          None,
          None,
        )
        .expect("starting-style cascade");
        black_box(styled);
      });
    },
  );
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
        html.push_str("<span class=\"target c\"></span>");
      } else {
        html.push_str("<span class=\"c\"></span>");
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
  // Include an `.a` element to defeat subtree bloom-summary pruning for `.a .b .c`
  // relative selector benches, while ensuring it can never be an ancestor of `.c`.
  html.push_str("<div class=\"a\"></div>");
  for i in 0..fan_out {
    html.push_str("<div class=\"b\">");
    build_branch(0, depth, fan_out, i == fan_out - 1, &mut html);
    html.push_str("</div>");
  }
  html.push_str("</div>");
  html
}

fn has_selector_css() -> &'static str {
  r#"
      .has-bench:has(.target) { outline: 1px solid #ccc; }
      .has-bench:has(.a .b .c) { outline: 2px dotted #999; }
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

fn generate_many_selectors_css(rule_count: usize, selectors_per_rule: usize) -> String {
  let mut css = String::new();
  // Use long class names to amplify any selector serialization overhead in cascade setup.
  const PAD: &str = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx";
  for rule in 0..rule_count {
    for idx in 0..selectors_per_rule {
      if idx > 0 {
        css.push_str(", ");
      }
      css.push('.');
      css.push_str("rule");
      css.push_str(&rule.to_string());
      css.push('_');
      css.push_str(&idx.to_string());
      css.push('_');
      css.push_str(PAD);
    }
    css.push_str(" { color: red; }\n");
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

fn cascade_setup_many_selectors_benchmark(c: &mut Criterion) {
  // Minimal DOM so selector matching work stays tiny and RuleIndex construction dominates.
  let dom = parse_html("<div></div>").expect("parse html");
  let css = generate_many_selectors_css(1500, 8);
  let stylesheet = parse_stylesheet(&css).expect("parse stylesheet");
  let media = MediaContext::screen(1280.0, 720.0);

  c.bench_function("cascade apply_styles setup 1500 rules/8 selectors", |b| {
    b.iter(|| {
      let styled = apply_styles_with_media(black_box(&dom), black_box(&stylesheet), &media);
      black_box(styled);
    });
  });
}

fn var_resolution_width_benchmark(c: &mut Criterion) {
  use fastrender::style::custom_property_store::CustomPropertyStore;
  use fastrender::style::values::CustomPropertyValue;
  use fastrender::style::var_resolution::{resolve_var_for_property, VarResolutionResult};

  let mut custom_properties = CustomPropertyStore::new();
  custom_properties.insert("--x".to_string(), CustomPropertyValue::new("10px", None));
  let value = PropertyValue::Keyword("var(--x)".to_string());

  // Ensure the benchmark keeps exercising the typed post-resolution parser.
  let VarResolutionResult::Resolved {
    value: resolved, ..
  } = resolve_var_for_property(&value, &custom_properties, "width")
  else {
    panic!("expected var() resolution to succeed");
  };
  assert!(matches!(resolved.as_ref(), PropertyValue::Length(_)));

  static PRINTED: AtomicBool = AtomicBool::new(false);
  c.bench_function("var_resolution width var(--x)->10px", |b| {
    b.iter_custom(|iters| {
      let (calls_start, bytes_start) = allocation_counts();
      let start = Instant::now();
      for _ in 0..iters {
        let resolved =
          resolve_var_for_property(black_box(&value), black_box(&custom_properties), "width");
        black_box(resolved);
      }
      let duration = start.elapsed();

      let (calls_end, bytes_end) = allocation_counts();
      let calls = calls_end.saturating_sub(calls_start);
      let bytes = bytes_end.saturating_sub(bytes_start);
      black_box((calls, bytes));

      if !PRINTED.swap(true, Ordering::Relaxed) {
        let iters_usize = iters as usize;
        let per_call_calls = calls / iters_usize.max(1);
        let per_call_bytes = bytes / iters_usize.max(1);
        eprintln!(
          "var() resolution allocations/call: calls={per_call_calls} bytes={per_call_bytes}"
        );
      }

      duration
    });
  });
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
    cascade_layer_heavy_benchmark,
    cascade_deep_dom_benchmark,
    cascade_not_benchmark,
    inline_style_starting_style_benchmark,
    has_selector_benchmark,
    pseudo_selector_candidate_benchmark,
    candidate_heavy_benchmark,
    cascade_setup_many_selectors_benchmark,
    var_resolution_width_benchmark
);
criterion_main!(benches);
