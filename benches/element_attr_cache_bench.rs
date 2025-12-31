use criterion::black_box;
use criterion::criterion_group;
use criterion::criterion_main;
use criterion::Criterion;
use cssparser::Parser;
use cssparser::ParserInput;
use fastrender::css::selectors::FastRenderSelectorImpl;
use fastrender::css::selectors::PseudoClassParser;
use fastrender::css::selectors::ShadowMatchData;
use fastrender::dom::DomNode;
use fastrender::dom::DomNodeType;
use fastrender::dom::ElementAttrCache;
use fastrender::dom::ElementRef;
use fastrender::dom::HTML_NAMESPACE;
use selectors::context::QuirksMode;
use selectors::matching::matches_selector;
use selectors::matching::MatchingForInvalidation;
use selectors::matching::MatchingContext;
use selectors::matching::MatchingMode;
use selectors::matching::NeedsSelectorFlags;
use selectors::matching::SelectorCaches;
use selectors::parser::ParseRelative;
use selectors::parser::SelectorList;
use std::fmt::Write;

fn build_test_node(num_classes: usize, num_attrs: usize) -> DomNode {
  let mut attrs = Vec::with_capacity(num_attrs + 1);
  let mut class_attr = String::new();
  for idx in 0..num_classes {
    if idx > 0 {
      class_attr.push(' ');
    }
    let _ = write!(class_attr, "c{}", idx);
  }
  attrs.push(("class".to_string(), class_attr));
  for idx in 0..num_attrs {
    attrs.push((format!("data-a{idx}"), "1".to_string()));
  }

  DomNode {
    node_type: DomNodeType::Element {
      tag_name: "div".to_string(),
      namespace: HTML_NAMESPACE.to_string(),
      attributes: attrs,
    },
    children: vec![],
  }
}

fn build_selector_list(
  num_selectors: usize,
  classes_per_selector: usize,
  attrs_per_selector: usize,
  num_classes: usize,
  num_attrs: usize,
) -> SelectorList<FastRenderSelectorImpl> {
  let mut css = String::new();
  for idx in 0..num_selectors {
    if idx > 0 {
      css.push(',');
    }
    css.push_str("div");
    for offset in 0..classes_per_selector {
      let class_idx = (idx + offset) % num_classes;
      let _ = write!(css, ".c{}", class_idx);
    }
    for offset in 0..attrs_per_selector {
      let attr_idx = (idx + offset * 7) % num_attrs;
      let _ = write!(css, "[data-a{}]", attr_idx);
    }
  }

  let mut input = ParserInput::new(&css);
  let mut parser = Parser::new(&mut input);
  SelectorList::parse(&PseudoClassParser, &mut parser, ParseRelative::No)
    .expect("parse selector list")
}

fn bench_element_attr_cache(c: &mut Criterion) {
  const NUM_CLASSES: usize = 64;
  const NUM_ATTRS: usize = 64;
  const NUM_SELECTORS: usize = 2048;
  const CLASSES_PER_SELECTOR: usize = 8;
  const ATTRS_PER_SELECTOR: usize = 4;

  let node = build_test_node(NUM_CLASSES, NUM_ATTRS);
  let selectors = build_selector_list(
    NUM_SELECTORS,
    CLASSES_PER_SELECTOR,
    ATTRS_PER_SELECTOR,
    NUM_CLASSES,
    NUM_ATTRS,
  );

  let mut group = c.benchmark_group("element_attr_cache");
  group.bench_function("no_cache", |b| {
    let mut selector_caches = SelectorCaches::default();
    b.iter(|| {
      let element_ref = ElementRef::new(&node);
      let mut context = MatchingContext::new(
        MatchingMode::Normal,
        None,
        &mut selector_caches,
        QuirksMode::NoQuirks,
        NeedsSelectorFlags::No,
        MatchingForInvalidation::No,
      );
      context.extra_data = ShadowMatchData::for_document();

      let mut matched = 0usize;
      for selector in selectors.slice() {
        if matches_selector(selector, 0, None, &element_ref, &mut context) {
          matched += 1;
        }
      }
      black_box(matched);
    });
  });

  group.bench_function("with_cache", |b| {
    let cache = ElementAttrCache::new(0);
    let mut selector_caches = SelectorCaches::default();
    b.iter(|| {
      cache.clear();
      let element_ref = ElementRef::new(&node).with_attr_cache(Some(&cache));
      let mut context = MatchingContext::new(
        MatchingMode::Normal,
        None,
        &mut selector_caches,
        QuirksMode::NoQuirks,
        NeedsSelectorFlags::No,
        MatchingForInvalidation::No,
      );
      context.extra_data = ShadowMatchData::for_document().with_element_attr_cache(&cache);

      let mut matched = 0usize;
      for selector in selectors.slice() {
        if matches_selector(selector, 0, None, &element_ref, &mut context) {
          matched += 1;
        }
      }
      black_box(matched);
    });
  });
  group.finish();
}

criterion_group!(benches, bench_element_attr_cache);
criterion_main!(benches);

