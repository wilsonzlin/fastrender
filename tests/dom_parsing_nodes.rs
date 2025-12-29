use fastrender::dom::{parse_html, DomNode, DomNodeType};

fn count_document_nodes(node: &DomNode) -> usize {
  let mut count = matches!(node.node_type, DomNodeType::Document { .. }) as usize;
  for child in &node.children {
    count += count_document_nodes(child);
  }
  count
}

#[test]
fn omits_non_content_nodes_from_dom_tree() {
  let html = "<!doctype html><!--before--><html><head><!--head comment--></head><body><!--body comment--><div>text</div></body></html>";
  let dom = parse_html(html).expect("parse html");

  assert!(matches!(dom.node_type, DomNodeType::Document { .. }));
  assert_eq!(
    count_document_nodes(&dom),
    1,
    "only the root should be marked as a document node"
  );

  let html_element = dom
    .children
    .iter()
    .find(|child| {
      matches!(
        &child.node_type,
        DomNodeType::Element { tag_name, .. } if tag_name.eq_ignore_ascii_case("html")
      )
    })
    .expect("parsed html element");

  assert!(
    html_element
      .children
      .iter()
      .all(|child| !matches!(child.node_type, DomNodeType::Document { .. })),
    "html element children should not contain document nodes"
  );
}
