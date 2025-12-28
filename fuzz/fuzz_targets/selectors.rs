#![no_main]

use arbitrary::Arbitrary;
use cssparser::{Parser, ParserInput};
use fastrender::css::selectors::{FastRenderSelectorImpl, PseudoClassParser};
use fastrender::dom::{
  next_selector_cache_epoch, DomNode, DomNodeType, ElementRef, HTML_NAMESPACE, SVG_NAMESPACE,
};
use libfuzzer_sys::fuzz_target;
use selectors::matching::{
  matches_selector, MatchingContext, MatchingForInvalidation, MatchingMode, NeedsSelectorFlags,
  QuirksMode, SelectorCaches,
};
use selectors::parser::{Selector, SelectorList};

const MAX_TEXT: usize = 1024;
const MAX_CHILDREN: usize = 8;
const MAX_DEPTH: usize = 4;
const MAX_ATTRS: usize = 6;
const MAX_SELECTOR_LEN: usize = 8 * 1024;

#[derive(Arbitrary, Debug)]
struct Attr {
  name: String,
  value: String,
}

#[derive(Arbitrary, Debug)]
struct RandomElement {
  tag: String,
  id: Option<String>,
  classes: Vec<String>,
  attributes: Vec<Attr>,
  children: Vec<RandomNode>,
  svg_namespace: bool,
}

#[derive(Arbitrary, Debug)]
enum RandomNode {
  Element(RandomElement),
  Text(String),
}

#[derive(Arbitrary, Debug)]
struct SelectorCase {
  selector_bytes: Vec<u8>,
  dom: RandomNode,
}

fn lossy_truncate(bytes: &[u8], limit: usize) -> String {
  let slice = if bytes.len() > limit {
    &bytes[..limit]
  } else {
    bytes
  };
  String::from_utf8_lossy(slice).into_owned()
}

fn truncate_text(s: &str) -> String {
  s.chars().take(MAX_TEXT).collect()
}

fn sanitize_ident(s: &str, fallback: &str) -> String {
  let filtered: String = s
    .chars()
    .filter(|c| c.is_ascii_alphanumeric() || matches!(c, '-' | '_' | ':'))
    .take(32)
    .collect();
  if filtered.is_empty() {
    fallback.to_string()
  } else {
    filtered
  }
}

impl RandomNode {
  fn to_dom(&self, depth: usize) -> DomNode {
    match self {
      RandomNode::Text(content) => DomNode {
        node_type: DomNodeType::Text {
          content: truncate_text(content),
        },
        children: vec![],
      },
      RandomNode::Element(el) => {
        let tag_name = sanitize_ident(&el.tag, "div");
        let namespace = if el.svg_namespace {
          SVG_NAMESPACE
        } else {
          HTML_NAMESPACE
        };

        let mut attributes = Vec::new();

        if let Some(id) = &el.id {
          let id_value = sanitize_ident(id, "seed-id");
          attributes.push(("id".to_string(), id_value));
        }

        let class_values: Vec<String> = el
          .classes
          .iter()
          .map(|c| sanitize_ident(c, ""))
          .filter(|c| !c.is_empty())
          .take(MAX_ATTRS)
          .collect();
        if !class_values.is_empty() {
          attributes.push(("class".to_string(), class_values.join(" ")));
        }

        for attr in el.attributes.iter().take(MAX_ATTRS) {
          let name = sanitize_ident(&attr.name, "");
          if name.is_empty() {
            continue;
          }
          let value = truncate_text(&attr.value);
          attributes.push((name, value));
        }

        let child_limit = if depth >= MAX_DEPTH { 0 } else { MAX_CHILDREN };
        let children = el
          .children
          .iter()
          .take(child_limit)
          .map(|child| child.to_dom(depth + 1))
          .collect();

        DomNode {
          node_type: DomNodeType::Element {
            tag_name,
            namespace: namespace.to_string(),
            attributes,
          },
          children,
        }
      }
    }
  }
}

fuzz_target!(|case: SelectorCase| {
  let selector_text = lossy_truncate(&case.selector_bytes, MAX_SELECTOR_LEN);
  let dom_root = DomNode {
    node_type: DomNodeType::Document,
    children: vec![case.dom.to_dom(0)],
  };

  let mut input = ParserInput::new(&selector_text);
  let mut parser = Parser::new(&mut input);
  if let Ok(list) = SelectorList::parse(&PseudoClassParser, &mut parser) {
    let selectors = list.slice();
    if selectors.is_empty() {
      return;
    }

    let mut caches = SelectorCaches::default();
    caches.set_epoch(next_selector_cache_epoch());
    let mut context = MatchingContext::new(
      MatchingMode::Normal,
      None,
      &mut caches,
      QuirksMode::NoQuirks,
      NeedsSelectorFlags::No,
      MatchingForInvalidation::No,
    );

    let mut ancestors: Vec<&DomNode> = Vec::new();
    walk_dom(&dom_root, selectors, &mut context, &mut ancestors);
  }
});

fn walk_dom<'a>(
  node: &'a DomNode,
  selectors: &'a [Selector<FastRenderSelectorImpl>],
  context: &mut MatchingContext<FastRenderSelectorImpl>,
  ancestors: &mut Vec<&'a DomNode>,
) {
  if node.is_element() {
    let element_ref = ElementRef::with_ancestors(node, ancestors);
    for selector in selectors.iter() {
      let _ = matches_selector(selector, 0, None, &element_ref, context);
    }
  }

  ancestors.push(node);
  for child in &node.children {
    walk_dom(child, selectors, context, ancestors);
  }
  ancestors.pop();
}
