//! HTML parse + RcDom → DomNode conversion benchmark
//!
//! This benchmark times `dom::parse_html_with_options` end-to-end:
//! - html5ever parsing into an `RcDom`
//! - `RcDom` → `DomNode` conversion
//! - declarative shadow root attachment (`<template shadowroot=...>`)
//!
//! It is intentionally synthetic (no committed real-site HTML) but includes stressors inspired by
//! large real pages:
//! - lots of repeated elements + attributes (`class`, `id`, `data-*`)
//! - some long attribute payloads (SVG `d` paths + JSON-like `data-*`)
//! - large inline `<script>` payloads
//!
//! Running:
//! ```bash
//! cargo bench --bench html_parse_bench -- --noplot
//! ```
//!
//! Tip: for quick smoke-checks, shrink the run:
//! ```bash
//! cargo bench --bench html_parse_bench -- --noplot --sample-size 10 --measurement-time 0.2
//! ```
//!
//! The benchmark prints input size + parsed node counts once up-front so the results are
//! interpretable even as the generator evolves.
//!
//! Note: DOM node counts include `Document`, `ShadowRoot`, element, slot, and text nodes.
//! Comments/processing instructions are not represented in `DomNode` today.
//! (html5ever still parses them; we just drop them during conversion.)
//!
use std::fmt::Write;

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use fastrender::dom::{self, DomNode, DomNodeType, DomParseOptions};

mod common;

#[derive(Debug, Clone, Copy)]
struct DomNodeCounts {
  total: usize,
  elements: usize,
  text: usize,
  shadow_roots: usize,
}

fn count_dom_nodes(root: &DomNode) -> DomNodeCounts {
  let mut total = 0usize;
  let mut elements = 0usize;
  let mut text = 0usize;
  let mut shadow_roots = 0usize;

  let mut stack: Vec<&DomNode> = vec![root];
  while let Some(node) = stack.pop() {
    total += 1;
    match node.node_type {
      DomNodeType::Document { .. } => {}
      DomNodeType::ShadowRoot { .. } => shadow_roots += 1,
      DomNodeType::Element { .. } | DomNodeType::Slot { .. } => elements += 1,
      DomNodeType::Text { .. } => text += 1,
    }

    for child in node.children.iter() {
      stack.push(child);
    }
  }

  DomNodeCounts {
    total,
    elements,
    text,
    shadow_roots,
  }
}

fn svg_path_payload(target_len: usize) -> String {
  let mut s = String::with_capacity(target_len.saturating_add(16));
  s.push_str("M0 0");
  let mut x = 0u32;
  let mut y = 0u32;
  while s.len() < target_len {
    x = (x + 13) % 1024;
    y = (y + 29) % 1024;
    let _ = write!(s, " L{} {}", x, y);
  }
  s
}

fn json_like_payload(target_len: usize) -> String {
  let mut s = String::with_capacity(target_len.saturating_add(32));
  s.push_str("{\"type\":\"bench\",\"items\":[");
  let mut i = 0usize;
  while s.len() < target_len.saturating_sub(4) {
    let _ = write!(
      s,
      "{{\"id\":{},\"k\":\"k{}\",\"v\":{}}},",
      i,
      i % 97,
      (i * 7) % 10_000
    );
    i += 1;
  }
  if s.ends_with(',') {
    s.pop();
  }
  s.push_str("]}");
  s
}

fn script_payload(target_len: usize) -> String {
  let mut s = String::with_capacity(target_len.saturating_add(64));
  s.push_str("/* fastrender synthetic benchmark payload */\n");
  let mut i = 0usize;
  while s.len() < target_len {
    let _ = writeln!(
      s,
      "var v{0} = {0}; function f{0}(){{return v{0};}}",
      i
    );
    i += 1;
  }
  s
}

fn script_len_for_target_nodes(target_nodes: usize) -> usize {
  match target_nodes {
    0..=15_000 => 32 * 1024,
    15_001..=75_000 => 96 * 1024,
    _ => 160 * 1024,
  }
}

fn build_synthetic_html(target_nodes: usize) -> (String, usize) {
  const EST_NODES_PER_CARD: usize = 22;
  let card_count = (target_nodes / EST_NODES_PER_CARD).max(1);

  let max_cards: usize = std::env::var("FASTR_HTML_PARSE_BENCH_MAX_CARDS")
    .ok()
    .and_then(|v| v.trim().parse().ok())
    .unwrap_or(8_000);
  assert!(
    card_count <= max_cards,
    "html_parse_bench would generate {card_count} cards; set FASTR_HTML_PARSE_BENCH_MAX_CARDS to raise the cap"
  );

  let long_svg = svg_path_payload(2048);
  let short_svg = "M0 0 L10 10 L5 7";
  let long_json = json_like_payload(1024);

  let big_script = script_payload(script_len_for_target_nodes(target_nodes));
  let medium_script = script_payload(4 * 1024);

  let mut html = String::with_capacity(card_count * 320 + big_script.len() * 2 + 8192);

  html.push_str("<!doctype html><html><head>");
  html.push_str("<meta charset=\"utf-8\">");
  html.push_str("<title>fastrender html_parse_bench</title>");

  let _ = write!(html, "<script>{}</script>", big_script);
  html.push_str("</head><body>");
  html.push_str("<main id=\"root\" class=\"root\" data-kind=\"bench\">");

  for idx in 0..card_count {
    let include_id = idx % 128 == 0;
    let include_long_attr = idx % 32 == 0;
    let include_shadow = idx % 64 == 0;
    let include_medium_script = idx % 512 == 0;

    let tag = match idx % 4 {
      0 => "section",
      1 => "div",
      2 => "article",
      _ => "nav",
    };

    let _ = write!(
      html,
      "<{tag} class=\"card item-{item} theme-{theme} layer{layer}\" data-id=\"{idx}\" data-kind=\"card\"",
      tag = tag,
      item = idx % 64,
      theme = idx % 7,
      layer = idx % 13,
      idx = idx
    );
    if include_id {
      let _ = write!(html, " id=\"card-{idx}\"", idx = idx);
    }
    if include_long_attr {
      let _ = write!(
        html,
        " data-props='{props}' data-long=\"true\"",
        props = long_json
      );
    } else {
      let _ = write!(html, " data-long=\"false\"");
    }
    html.push('>');

    let _ = write!(
      html,
      "<div class=\"card-header\" data-h=\"{h}\"><h2>Item {idx}</h2><button class=\"card-action\" type=\"button\" data-action=\"a{action}\">…</button></div>",
      h = idx % 17,
      idx = idx,
      action = idx % 11
    );

    html.push_str("<div class=\"card-content\">");
    let _ = write!(
      html,
      "<p data-p=\"{p}\">FastRender synthetic parse bench paragraph {idx}.</p>",
      p = idx % 29,
      idx = idx
    );

    html.push_str("<div class=\"media\">");
    let _ = write!(
      html,
      "<img src=\"image-{idx}.png\" alt=\"thumb\" width=\"64\" height=\"64\">",
      idx = idx
    );
    html.push_str("<span class=\"caption\">caption</span>");
    html.push_str("</div>");

    let d = if include_long_attr { &long_svg } else { short_svg };
    let _ = write!(
      html,
      "<svg viewBox=\"0 0 10 10\" class=\"icon icon-{icon}\"><path d=\"{d}\" fill=\"none\" stroke=\"#000\"></path></svg>",
      icon = idx % 23,
      d = d
    );
    html.push_str("</div>");

    html.push_str("<ul class=\"items\">");
    for j in 0..3 {
      let _ = write!(
        html,
        "<li data-row=\"{j}\" data-col=\"{col}\">row {j}</li>",
        j = j,
        col = (idx + j) % 5
      );
    }
    html.push_str("</ul>");

    if include_shadow {
      let _ = write!(
        html,
        "<div class=\"shadow-host\" id=\"shadow-{idx}\">",
        idx = idx
      );
      html.push_str("<template shadowroot=\"open\" shadowrootdelegatesfocus>");
      html.push_str("<style>.shadow{display:block}.title{font-weight:bold}</style>");
      html.push_str(
        "<div class=\"shadow\"><slot name=\"title\"></slot><slot></slot><div class=\"deep\"><span>deep</span></div></div>",
      );
      html.push_str("</template>");
      let _ = write!(
        html,
        "<span slot=\"title\" class=\"title\">Title {idx}</span><span class=\"light\">light {idx}</span>",
        idx = idx
      );
      html.push_str("</div>");
    }

    if include_medium_script {
      let _ = write!(html, "<script>{}</script>", medium_script);
    }

    let _ = write!(html, "</{tag}>\n", tag = tag);
  }

  html.push_str("</main>");

  let _ = write!(html, "<script>{}</script>", big_script);

  html.push_str("</body></html>");
  (html, card_count)
}

fn html_parse_benchmarks(c: &mut Criterion) {
  let sizes = [
    ("10k_nodes", 10_000usize),
    ("50k_nodes", 50_000usize),
    ("100k_nodes", 100_000usize),
  ];

  let mut group = c.benchmark_group("html_parse");

  for (label, target_nodes) in sizes {
    let (html, card_count) = build_synthetic_html(target_nodes);

    let dom = dom::parse_html_with_options(&html, DomParseOptions::default()).expect("parse html");
    let counts = count_dom_nodes(&dom);

    eprintln!(
      "html_parse_bench {label}: target_nodes~{target_nodes}, cards={card_count}, html_bytes={}, dom_nodes(total/elements/text/shadow)={}/{}/{}/{}",
      html.len(),
      counts.total,
      counts.elements,
      counts.text,
      counts.shadow_roots
    );

    group.throughput(Throughput::Elements(counts.total as u64));
    group.bench_with_input(
      BenchmarkId::new("parse_html_with_options", label),
      &html,
      |b, html| {
        b.iter(|| {
          let dom =
            dom::parse_html_with_options(black_box(html), DomParseOptions::default()).unwrap();
          black_box(dom);
        })
      },
    );
  }

  group.finish();
}

criterion_group!(
  name = html_parse_benches;
  config = common::perf_criterion();
  targets = html_parse_benchmarks
);
criterion_main!(html_parse_benches);
