use fastrender::css::parser::parse_stylesheet;
use fastrender::dom::{DomNode, DomNodeType, HTML_NAMESPACE};
use fastrender::style::cascade::apply_styles;
use fastrender::style::color::Rgba;
use fastrender::style::values::{CustomPropertyTypedValue, LengthUnit};

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

fn simple_div_dom() -> DomNode {
  DomNode {
    node_type: DomNodeType::Element {
      tag_name: "div".to_string(),
      namespace: HTML_NAMESPACE.to_string(),
      attributes: vec![],
    },
    children: vec![],
  }
}

#[test]
fn non_inheriting_property_resets_to_initial_value() {
  let css = r#"
    @property --size {
      syntax: "<length>";
      inherits: false;
      initial-value: 10px;
    }
    #root { --size: 5px; }
  "#;
  let sheet = parse_stylesheet(css).unwrap();
  let dom = dom_with_child();
  let styled = apply_styles(&dom, &sheet);
  let child = styled.children.first().expect("child");

  let rule = styled
    .styles
    .custom_property_registry
    .get("--size")
    .expect("registered property");
  assert!(!rule.inherits);

  let parent_value = styled
    .styles
    .custom_properties
    .get("--size")
    .expect("root value");
  match &parent_value.typed {
    Some(CustomPropertyTypedValue::Length(len)) => {
      assert_eq!(len.value, 5.0);
      assert_eq!(len.unit, LengthUnit::Px);
    }
    other => panic!("expected typed length for parent, got {other:?}"),
  }

  let child_value = child
    .styles
    .custom_properties
    .get("--size")
    .expect("child value");
  match &child_value.typed {
    Some(CustomPropertyTypedValue::Length(len)) => {
      assert_eq!(len.value, 10.0);
      assert_eq!(len.unit, LengthUnit::Px);
    }
    other => panic!("expected initial length on child, got {other:?}"),
  }
}

#[test]
fn initial_value_applied_without_declaration() {
  let css = r#"
    @property --tone {
      syntax: "<color>";
      inherits: true;
      initial-value: rgb(1 2 3);
    }
  "#;
  let sheet = parse_stylesheet(css).unwrap();
  let dom = simple_div_dom();
  let styled = apply_styles(&dom, &sheet);

  let value = styled
    .styles
    .custom_properties
    .get("--tone")
    .expect("initial value");
  assert_eq!(value.value.trim(), "rgb(1 2 3)");
  match &value.typed {
    Some(CustomPropertyTypedValue::Color(color)) => {
      assert_eq!(color.to_rgba(Rgba::BLACK), Rgba::rgb(1, 2, 3));
    }
    other => panic!("expected typed color, got {other:?}"),
  }
}
