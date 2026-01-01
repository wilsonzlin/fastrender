use fastrender::css::parser::parse_stylesheet;
use fastrender::dom::{DomNode, DomNodeType, HTML_NAMESPACE};
use fastrender::style::cascade::apply_styles;
use fastrender::LengthUnit;

fn dom_with_child() -> DomNode {
  DomNode {
    node_type: DomNodeType::Element {
      tag_name: "div".to_string(),
      namespace: HTML_NAMESPACE.to_string(),
      attributes: vec![("id".to_string(), "root".to_string())],
    },
    children: vec![DomNode {
      node_type: DomNodeType::Element {
        tag_name: "span".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("id".to_string(), "child".to_string())],
      },
      children: vec![],
    }],
  }
}

#[test]
fn registered_custom_property_inherits_false_does_not_inherit_parent_value() {
  let css = r#"
    @property --x {
      syntax: "<length>";
      inherits: false;
      initial-value: 5px;
    }
    #root { --x: 10px; }
    #child { padding-left: var(--x); }
  "#;
  let sheet = parse_stylesheet(css).unwrap();
  let dom = dom_with_child();
  let styled = apply_styles(&dom, &sheet);
  let child = styled.children.first().expect("child");

  assert_eq!(child.styles.padding_left.value, 5.0);
  assert_eq!(child.styles.padding_left.unit, LengthUnit::Px);
}

#[test]
fn registered_custom_property_inherits_true_inherits_parent_value() {
  let css = r#"
    @property --x {
      syntax: "<length>";
      inherits: true;
      initial-value: 5px;
    }
    #root { --x: 10px; }
    #child { padding-left: var(--x); }
  "#;
  let sheet = parse_stylesheet(css).unwrap();
  let dom = dom_with_child();
  let styled = apply_styles(&dom, &sheet);
  let child = styled.children.first().expect("child");

  assert_eq!(child.styles.padding_left.value, 10.0);
  assert_eq!(child.styles.padding_left.unit, LengthUnit::Px);
}

#[test]
fn unregistered_custom_property_inherits_by_default() {
  let css = r#"
    #root { --x: 10px; }
    #child { padding-left: var(--x); }
  "#;
  let sheet = parse_stylesheet(css).unwrap();
  let dom = dom_with_child();
  let styled = apply_styles(&dom, &sheet);
  let child = styled.children.first().expect("child");

  assert_eq!(child.styles.padding_left.value, 10.0);
  assert_eq!(child.styles.padding_left.unit, LengthUnit::Px);
}
