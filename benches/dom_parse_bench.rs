//! DOM parse benchmark
//!
//! This benchmark measures HTML parsing + RcDom→DomNode conversion.
//! It is intended to catch regressions in the `dom_parse` stage, especially
//! allocation-heavy work while converting markup5ever's RcDom into FastRender's
//! internal `DomNode` tree.
//!
//! Running:
//! ```bash
//! cargo bench --bench dom_parse_bench -- --noplot
//! ```
//!
//! Notes:
//! - The HTML is generated programmatically (no committed real-site HTML).
//! - Two variants are included:
//!   - plain: mostly HTML elements/attributes (stresses HTML namespace storage and vector growth)
//!   - template: includes `<template>` contents (stresses template_contents conversion)

use std::fmt::Write;

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use fastrender::dom::parse_html;

fn build_dom_parse_html(card_count: usize, include_templates: bool) -> String {
  let mut html = String::from("<!doctype html><html><head><title>bench</title></head><body>");
  for idx in 0..card_count {
    html.push_str("<div class=\"card item-");
    let _ = write!(html, "{}", idx % 64);
    html.push_str("\" data-id=\"");
    let _ = write!(html, "{idx}");
    html.push_str("\"><span class=\"label\">Item ");
    let _ = write!(html, "{idx}");
    html.push_str("</span>");

    if include_templates && idx % 8 == 0 {
      html.push_str("<template><div class=\"tpl\" data-tpl=\"");
      let _ = write!(html, "{idx}");
      html.push_str("\"><p>Template</p></div></template>");
    }

    html.push_str("</div>");
  }
  html.push_str("</body></html>");
  html
}

fn dom_parse_benchmarks(c: &mut Criterion) {
  let mut group = c.benchmark_group("dom_parse");

  // 10k cards: ~20k elements (plain) and ~23k elements (template variant, depending on stride),
  // plus text nodes. This stays representative but avoids blowing up memory on constrained CI.
  let card_counts = [2_000usize, 5_000, 10_000];

  for &count in &card_counts {
    let plain = build_dom_parse_html(count, false);
    group.bench_with_input(BenchmarkId::new("parse_html_plain", count), &plain, |b, html| {
      b.iter(|| {
        let dom = parse_html(black_box(html)).expect("parse html");
        black_box(dom);
      })
    });

    let with_template = build_dom_parse_html(count, true);
    group.bench_with_input(
      BenchmarkId::new("parse_html_template", count),
      &with_template,
      |b, html| {
        b.iter(|| {
          let dom = parse_html(black_box(html)).expect("parse html");
          black_box(dom);
        })
      },
    );
  }

  group.finish();
}

criterion_group!(benches, dom_parse_benchmarks);
criterion_main!(benches);

