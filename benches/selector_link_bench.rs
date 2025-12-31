use criterion::black_box;
use criterion::criterion_group;
use criterion::criterion_main;
use criterion::Criterion;
use cssparser::ParserInput;
use fastrender::css::selectors::PseudoClassParser;
use fastrender::dom::DomNode;
use fastrender::dom::DomNodeType;
use fastrender::dom::ElementRef;
use fastrender::dom::HTML_NAMESPACE;
use selectors::matching::matches_selector;
use selectors::matching::MatchingContext;
use selectors::matching::MatchingForInvalidation;
use selectors::matching::MatchingMode;
use selectors::matching::NeedsSelectorFlags;
use selectors::matching::QuirksMode;
use selectors::matching::SelectorCaches;
use selectors::parser::ParseRelative;
use selectors::parser::SelectorList;

fn build_link_list(count: usize) -> DomNode {
  let mut children = Vec::with_capacity(count);
  for _ in 0..count {
    children.push(DomNode {
      node_type: DomNodeType::Element {
        tag_name: "a".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("href".to_string(), "#".to_string())],
      },
      children: vec![],
    });
  }

  DomNode {
    node_type: DomNodeType::Element {
      tag_name: "div".to_string(),
      namespace: HTML_NAMESPACE.to_string(),
      attributes: vec![],
    },
    children,
  }
}

fn build_deep_link_chain(depth: usize) -> DomNode {
  let mut node = DomNode {
    node_type: DomNodeType::Element {
      tag_name: "a".to_string(),
      namespace: HTML_NAMESPACE.to_string(),
      attributes: vec![("href".to_string(), "#".to_string())],
    },
    children: vec![],
  };

  for _ in 0..depth {
    node = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "a".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("href".to_string(), "#".to_string())],
      },
      children: vec![node],
    };
  }

  DomNode {
    node_type: DomNodeType::Element {
      tag_name: "section".to_string(),
      namespace: HTML_NAMESPACE.to_string(),
      attributes: vec![],
    },
    children: vec![node],
  }
}

fn parse_selector(
  selector: &str,
) -> SelectorList<fastrender::css::selectors::FastRenderSelectorImpl> {
  let mut input = ParserInput::new(selector);
  let mut parser = cssparser::Parser::new(&mut input);
  SelectorList::parse(&PseudoClassParser, &mut parser, ParseRelative::No)
    .expect("parse selector")
}

fn selector_link_bench(c: &mut Criterion) {
  let count = std::env::var("FASTR_LINK_BENCH_COUNT")
    .ok()
    .and_then(|value| value.parse::<usize>().ok())
    .unwrap_or(8192);

  let list_root = build_link_list(count);
  let list_ancestors: [&DomNode; 1] = [&list_root];
  let list_elements: Vec<ElementRef<'_>> = list_root
    .children
    .iter()
    .map(|node| ElementRef::with_ancestors(node, &list_ancestors))
    .collect();

  let list_selector_list = parse_selector("a:link");
  let list_selector = list_selector_list
    .slice()
    .first()
    .expect("selector list empty");

  let mut list_caches = SelectorCaches::default();
  let mut list_context = MatchingContext::new(
    MatchingMode::Normal,
    None,
    &mut list_caches,
    QuirksMode::NoQuirks,
    NeedsSelectorFlags::No,
    MatchingForInvalidation::No,
  );

  c.bench_function("selectors/link-pseudo-flat", |b| {
    b.iter(|| {
      let mut matched = 0usize;
      for element in list_elements.iter() {
        if matches_selector(
          list_selector,
          0,
          None,
          black_box(element),
          &mut list_context,
        ) {
          matched += 1;
        }
      }
      black_box(matched);
    })
  });

  let depth = std::env::var("FASTR_LINK_BENCH_DEPTH")
    .ok()
    .and_then(|value| value.parse::<usize>().ok())
    .unwrap_or(4096);

  let root = build_deep_link_chain(depth);
  let mut ancestors: Vec<&DomNode> = Vec::with_capacity(depth.saturating_add(1));
  let mut current: &DomNode = &root;
  loop {
    let Some(child) = current.children.first() else {
      break;
    };
    ancestors.push(current);
    current = child;
  }
  let leaf = current;

  let element = ElementRef::with_ancestors(leaf, &ancestors);
  let selector_list = parse_selector("section a:link");
  let selector = selector_list.slice().first().expect("selector list empty");

  let mut caches = SelectorCaches::default();
  let mut context = MatchingContext::new(
    MatchingMode::Normal,
    None,
    &mut caches,
    QuirksMode::NoQuirks,
    NeedsSelectorFlags::No,
    MatchingForInvalidation::No,
  );

  c.bench_function("selectors/link-heavy-descendant", |b| {
    b.iter(|| {
      // Match multiple times per iteration to smooth out overhead.
      let mut matched = false;
      for _ in 0..64 {
        matched = matches_selector(selector, 0, None, black_box(&element), &mut context);
      }
      black_box(matched);
    })
  });
}

criterion_group!(benches, selector_link_bench);
criterion_main!(benches);
