use fastrender::dom;
use fastrender::dom::DomNode;
use fastrender::dom::DomNodeType;
<<<<<<< HEAD
use selectors::context::QuirksMode;
=======
use selectors::context::QuirksMode;
>>>>>>> e44f0c5 (feat: enforce CSS timeouts during parsing and imports)

fn text_node(content: &str) -> DomNode {
  DomNode {
    node_type: DomNodeType::Text {
      content: content.to_string(),
    },
    children: vec![],
  }
}

fn element(tag: &str, attrs: Vec<(&str, &str)>, children: Vec<DomNode>) -> DomNode {
  DomNode {
    node_type: DomNodeType::Element {
      tag_name: tag.to_string(),
      namespace: String::new(),
      attributes: attrs
        .into_iter()
        .map(|(k, v)| (k.to_string(), v.to_string()))
        .collect(),
    },
    children,
  }
}

#[test]
fn collect_text_codepoints_skips_hidden_and_inert() {
  let visible = element("div", vec![], vec![text_node("A")]);
  let hidden = element("div", vec![("hidden", "")], vec![text_node("B")]);
  let inert = element(
    "div",
    vec![("data-fastr-inert", "true")],
    vec![text_node("C")],
  );
  let root = DomNode {
    node_type: DomNodeType::Document {
      quirks_mode: QuirksMode::NoQuirks,
    },
    children: vec![visible, hidden, inert],
  };

  let codepoints = dom::collect_text_codepoints(&root);
  assert!(codepoints.contains(&(b'A' as u32)));
  assert!(!codepoints.contains(&(b'B' as u32)));
  assert!(!codepoints.contains(&(b'C' as u32)));
}
