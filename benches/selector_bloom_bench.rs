use criterion::black_box;
use criterion::criterion_group;
use criterion::criterion_main;
use criterion::Criterion;
use fastrender::css::parser::parse_stylesheet;
use fastrender::dom::build_selector_bloom_store;
use fastrender::dom::enumerate_dom_ids;
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

fn build_has_tree_html(depth: usize, branching: usize, needle_stride: usize) -> String {
  fn estimated_has_tree_element_count(depth: usize, branching: usize) -> Option<u128> {
    // This builder expands an exponential tree:
    // - 1 <section> at every recursion node (levels 0..=depth)
    // - At non-leaf nodes (levels 0..depth-1):
    //   - `branching` gate <div> wrappers
    //   - 1 filler <div>
    // - At leaf nodes (level == depth): 2 <div> children (leaf + trail)
    //
    // These counts are approximate but good enough to prevent accidental OOM.
    if branching == 0 {
      return Some(0);
    }

    let b = branching as u128;
    let d = depth as u32;

    // geometric sums
    let pow_d = b.checked_pow(d)?;
    let pow_d1 = b.checked_pow(d.saturating_add(1))?;

    // sections: sum_{i=0..d} b^i = (b^(d+1)-1)/(b-1)   (or d+1 when b==1)
    let sections = if b == 1 {
      (depth as u128).saturating_add(1)
    } else {
      pow_d1.saturating_sub(1) / (b - 1)
    };

    // non-leaf nodes: sum_{i=0..d-1} b^i
    let non_leaf = if depth == 0 {
      0
    } else if b == 1 {
      depth as u128
    } else {
      pow_d.saturating_sub(1) / (b - 1)
    };

    let gate_divs = b.checked_mul(non_leaf)?;
    let filler_divs = non_leaf;
    let leaf_divs = 2u128.checked_mul(pow_d)?;

    sections
      .checked_add(gate_divs)?
      .checked_add(filler_divs)?
      .checked_add(leaf_divs)
  }

  fn build_level(
    html: &mut String,
    level: usize,
    max_depth: usize,
    branching: usize,
    needle_stride: usize,
    counter: &mut usize,
  ) {
    let _ = write!(
      html,
      "<section class=\"layer{level}\" data-level=\"{level}\">",
      level = level
    );

    if level >= max_depth {
      let leaf_id = *counter;
      *counter += 1;
      let needle = if leaf_id % needle_stride == 0 {
        " needle"
      } else {
        ""
      };
      let _ = write!(
        html,
        "<div class=\"leaf{needle}\" data-leaf=\"{leaf_id}\"></div><div class=\"trail trail{level}\"></div>",
        needle = needle,
        leaf_id = leaf_id,
        level = level
      );
      html.push_str("</section>");
      return;
    }

    for branch in 0..branching {
      let branch_id = *counter;
      *counter += 1;
      let gate_needle = if (branch_id + branch) % (needle_stride.saturating_mul(2).max(1)) == 0 {
        " gate-needle"
      } else {
        ""
      };
      let _ = write!(
        html,
        "<div class=\"gate{level}{gate_needle}\" data-branch=\"{branch}\">",
        level = level,
        gate_needle = gate_needle,
        branch = branch
      );
      build_level(
        html,
        level + 1,
        max_depth,
        branching,
        needle_stride,
        counter,
      );
      html.push_str("</div>");
    }

    let _ = write!(
      html,
      "<div class=\"filler filler{level}\"></div>",
      level = level
    );
    html.push_str("</section>");
  }

  let mut html = String::from("<html><head></head><body>");
  let mut counter = 0usize;

  // Guardrail: prevent accidental runaway allocations during benches.
  // If you need a larger stress case, explicitly raise this cap.
  let max_elems: u128 = std::env::var("FASTR_BLOOM_BENCH_MAX_ELEMS")
    .ok()
    .and_then(|v| v.trim().parse::<u128>().ok())
    .unwrap_or(2_000_000);
  if let Some(estimated) = estimated_has_tree_element_count(depth, branching) {
    assert!(
      estimated <= max_elems,
      "has-tree bench would generate ~{estimated} elements (depth={depth}, branching={branching}); \
set FASTR_BLOOM_BENCH_MAX_ELEMS to raise the cap"
    );
  }

  build_level(&mut html, 0, depth, branching, needle_stride, &mut counter);
  html.push_str("</body></html>");
  html
}

fn generate_has_styles(max_depth: usize, needle_stride: usize) -> String {
  let mut css = String::from("body { margin: 0; }\n");
  for level in 0..=max_depth {
    let _ = write!(
      css,
      ".layer{level}:has(.needle) {{ padding-left: {}px; }}\n",
      (level % 5) + 1,
      level = level
    );
    let _ = write!(
      css,
      ".layer{level}:has(:scope > .gate{level}.gate-needle) {{ border-left: {}px solid #ccc; }}\n",
      (level % 7) + 1,
      level = level
    );
    let _ = write!(
      css,
      ".layer{level}:has(.filler{level} ~ .needle) {{ margin-left: {}px; }}\n",
      (level % 9) + 2,
      level = level
    );
    let target_leaf = (level + 1) * needle_stride;
    let _ = write!(
      css,
      ".layer{level}:has([data-leaf=\"{target_leaf}\"]) {{ margin-bottom: {}px; }}\n",
      (level % 6) + 1,
      level = level,
      target_leaf = target_leaf
    );
  }
  css
}

fn selector_bloom_benchmark(c: &mut Criterion) {
  let depth = 64;
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

fn has_selector_bloom_benchmark(c: &mut Criterion) {
  let depth = 7;
  let branching = 5;
  let needle_stride = 37;

  let html = build_has_tree_html(depth, branching, needle_stride);
  let css = generate_has_styles(depth, needle_stride);

  let dom = parse_html(&html).expect("parse html");
  let stylesheet = parse_stylesheet(&css).expect("parse stylesheet");
  let media = MediaContext::screen(1280.0, 720.0);

  let mut group = c.benchmark_group("has_selector_bloom_filter");
  group.bench_function("has_with_bloom", |b| {
    set_selector_bloom_enabled(true);
    b.iter(|| {
      let styled = apply_styles_with_media(black_box(&dom), black_box(&stylesheet), &media);
      black_box(styled);
    });
  });
  group.bench_function("has_without_bloom", |b| {
    set_selector_bloom_enabled(false);
    b.iter(|| {
      let styled = apply_styles_with_media(black_box(&dom), black_box(&stylesheet), &media);
      black_box(styled);
    });
  });
  group.finish();
  set_selector_bloom_enabled(true);
}

fn has_selector_summary_benchmark(c: &mut Criterion) {
  // Keep this benchmark representative but *safe* by default.
  // Previous values could generate hundreds of millions of elements and OOM machines.
  let depth = 8;
  let branching = 4;
  let needle_stride = 23;

  let html = build_has_tree_html(depth, branching, needle_stride);
  let css = generate_has_styles(depth, needle_stride);

  let dom = parse_html(&html).expect("parse html");
  let stylesheet = parse_stylesheet(&css).expect("parse stylesheet");
  let media = MediaContext::screen(1280.0, 720.0);

  let mut group = c.benchmark_group("has_selector_summary");
  group.bench_function("has_summary_with_bloom", |b| {
    set_selector_bloom_enabled(true);
    b.iter(|| {
      let styled = apply_styles_with_media(black_box(&dom), black_box(&stylesheet), &media);
      black_box(styled);
    });
  });
  group.bench_function("has_summary_without_bloom", |b| {
    set_selector_bloom_enabled(false);
    b.iter(|| {
      let styled = apply_styles_with_media(black_box(&dom), black_box(&stylesheet), &media);
      black_box(styled);
    });
  });
  group.finish();
  set_selector_bloom_enabled(true);
}

fn selector_bloom_lookup_benchmark(c: &mut Criterion) {
  // Focused micro-benchmark: compare per-element bloom summary lookups using the dense
  // node-id indexed store vs a legacy pointer-keyed HashMap.
  let depth = 9;
  let branching = 4;
  let needle_stride = 29;

  set_selector_bloom_enabled(true);
  let html = build_has_tree_html(depth, branching, needle_stride);
  let dom = parse_html(&html).expect("parse html");

  let id_map = enumerate_dom_ids(&dom);
  let store = build_selector_bloom_store(&dom, &id_map).expect("build selector bloom store");

  // Collect stable traversal order of element nodes and construct a legacy HashMap to compare.
  let mut elements: Vec<(usize, *const fastrender::dom::DomNode)> = Vec::new();
  for (ptr, id) in id_map.iter() {
    // Safety: pointers originate from the immutable DOM tree.
    let node = unsafe { &**ptr };
    if !node.is_element() {
      continue;
    }
    elements.push((*id, *ptr));
  }

  elements.sort_unstable_by_key(|(id, _)| *id);
  let mut node_ids: Vec<usize> = Vec::with_capacity(elements.len());
  let mut node_ptrs: Vec<*const fastrender::dom::DomNode> = Vec::with_capacity(elements.len());
  for (id, ptr) in elements.iter().copied() {
    node_ids.push(id);
    node_ptrs.push(ptr);
  }

  let mut legacy = std::collections::HashMap::with_capacity(node_ptrs.len());
  for (id, ptr) in elements.iter().copied() {
    if let Some(summary) = store.summary_for_id(id).copied() {
      legacy.insert(ptr, summary);
    }
  }

  let mut group = c.benchmark_group("selector_bloom_summary_lookup");
  group.bench_function("dense_store_by_id", |b| {
    b.iter(|| {
      let mut acc = 0usize;
      for id in node_ids.iter().copied() {
        if let Some(summary) = store.summary_for_id(id) {
          acc ^= std::ptr::from_ref(summary) as usize;
        }
      }
      black_box(acc);
    });
  });
  group.bench_function("hashmap_by_ptr", |b| {
    b.iter(|| {
      let mut acc = 0usize;
      for node_ptr in node_ptrs.iter().copied() {
        if let Some(summary) = legacy.get(&node_ptr) {
          acc ^= std::ptr::from_ref(summary) as usize;
        }
      }
      black_box(acc);
    });
  });
  group.finish();
}

criterion_group!(
  benches,
  selector_bloom_benchmark,
  has_selector_bloom_benchmark,
  has_selector_summary_benchmark,
  selector_bloom_lookup_benchmark
);
criterion_main!(benches);
