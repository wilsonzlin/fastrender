use fastrender::css::types::StyleSheet;
use fastrender::dom::{parse_html, DomNode, DomNodeType};
use fastrender::style::cascade::{apply_styles, StyledNode};

fn assert_styled_shape_matches_dom(dom: &DomNode, styled: &StyledNode) {
  match (&dom.node_type, &styled.node.node_type) {
    (DomNodeType::Document { .. }, DomNodeType::Document { .. }) => {}
    (
      DomNodeType::ShadowRoot {
        mode: left_mode, ..
      },
      DomNodeType::ShadowRoot {
        mode: right_mode, ..
      },
    ) => {
      assert_eq!(left_mode, right_mode);
    }
    (
      DomNodeType::Slot {
        namespace: left_ns,
        attributes: left_attrs,
        ..
      },
      DomNodeType::Slot {
        namespace: right_ns,
        attributes: right_attrs,
        ..
      },
    ) => {
      assert_eq!(left_ns, right_ns);
      assert_eq!(left_attrs, right_attrs);
    }
    (
      DomNodeType::Element {
        tag_name: left_tag,
        namespace: left_ns,
        attributes: left_attrs,
      },
      DomNodeType::Element {
        tag_name: right_tag,
        namespace: right_ns,
        attributes: right_attrs,
      },
    ) => {
      assert_eq!(left_tag, right_tag);
      assert_eq!(left_ns, right_ns);
      assert_eq!(left_attrs, right_attrs);
    }
    (DomNodeType::Text { content: left }, DomNodeType::Text { content: right }) => {
      assert_eq!(left, right);
    }
    (left, right) => panic!("DOM/styled node type mismatch: {left:?} vs {right:?}"),
  }

  assert_eq!(
    dom.children.len(),
    styled.children.len(),
    "mismatched child count for node_id {}",
    styled.node_id
  );
  for (dom_child, styled_child) in dom.children.iter().zip(styled.children.iter()) {
    assert_styled_shape_matches_dom(dom_child, styled_child);
  }
}

#[test]
fn styled_nodes_store_shallow_dom_nodes() {
  let html =
    "<html><head></head><body><div id=\"outer\"><span>Hi</span><p></p></div></body></html>";
  let dom = parse_html(html).expect("parse html");
  let styled = apply_styles(&dom, &StyleSheet::new());

  assert_styled_shape_matches_dom(&dom, &styled);

  assert!(
    !styled.children.is_empty(),
    "expected root styled node to have children"
  );
  assert!(
    styled.node.children.is_empty(),
    "expected StyledNode.node to not clone DOM descendants"
  );

  let html_node = styled.children.first().expect("html child");
  assert!(
    !html_node.children.is_empty(),
    "expected html element to have children"
  );
  assert!(
    html_node.node.children.is_empty(),
    "expected html StyledNode.node to not clone DOM descendants"
  );
}
