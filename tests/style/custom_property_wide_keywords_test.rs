use fastrender::css::parser::parse_stylesheet;
use fastrender::dom::{DomNode, DomNodeType, HTML_NAMESPACE};
use fastrender::style::cascade::apply_styles;
use fastrender::style::color::Rgba;
use fastrender::LengthUnit;

fn simple_div_with_id(id: &str) -> DomNode {
  DomNode {
    node_type: DomNodeType::Element {
      tag_name: "div".to_string(),
      namespace: HTML_NAMESPACE.to_string(),
      attributes: vec![("id".to_string(), id.to_string())],
    },
    children: vec![],
  }
}

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
fn registered_custom_property_initial_without_initial_value_is_guaranteed_invalid() {
  let css = r#"
    @property --x {
      syntax: "*";
      inherits: true;
    }
    div {
      --x: initial;
      color: var(--x, red);
    }
  "#;

  let sheet = parse_stylesheet(css).unwrap();
  let dom = simple_div_with_id("box");
  let styled = apply_styles(&dom, &sheet);

  assert_eq!(styled.styles.color, Rgba::RED);
  assert!(
    styled.styles.custom_properties.get("--x").is_none(),
    "registered custom property without an initial value should compute to the guaranteed-invalid value"
  );
}

#[test]
fn registered_custom_property_initial_resets_to_registered_initial_value() {
  let css = r#"
    @property --x {
      syntax: "<length>";
      inherits: false;
      initial-value: 5px;
    }
    #box {
      --x: 10px;
      --x: initial;
      padding-left: var(--x);
    }
  "#;

  let sheet = parse_stylesheet(css).unwrap();
  let dom = simple_div_with_id("box");
  let styled = apply_styles(&dom, &sheet);

  assert_eq!(styled.styles.padding_left.value, 5.0);
  assert_eq!(styled.styles.padding_left.unit, LengthUnit::Px);
}

#[test]
fn registered_custom_property_inherit_keyword_forces_inheritance() {
  let css = r#"
    @property --x {
      syntax: "<length>";
      inherits: false;
      initial-value: 5px;
    }
    #root { --x: 10px; }
    #child { --x: inherit; padding-left: var(--x); }
  "#;

  let sheet = parse_stylesheet(css).unwrap();
  let dom = dom_with_child();
  let styled = apply_styles(&dom, &sheet);
  let child = styled.children.first().expect("child");

  assert_eq!(child.styles.padding_left.value, 10.0);
  assert_eq!(child.styles.padding_left.unit, LengthUnit::Px);
}

#[test]
fn registered_custom_property_unset_behaves_like_inherit_when_inheritable() {
  let css = r#"
    @property --x {
      syntax: "<length>";
      inherits: true;
      initial-value: 5px;
    }
    #root { --x: 10px; }
    #child { --x: 20px; }
    #child { --x: unset; padding-left: var(--x); }
  "#;

  let sheet = parse_stylesheet(css).unwrap();
  let dom = dom_with_child();
  let styled = apply_styles(&dom, &sheet);
  let child = styled.children.first().expect("child");

  assert_eq!(child.styles.padding_left.value, 10.0);
  assert_eq!(child.styles.padding_left.unit, LengthUnit::Px);
}

#[test]
fn registered_custom_property_unset_behaves_like_initial_when_not_inheritable() {
  let css = r#"
    @property --x {
      syntax: "<length>";
      inherits: false;
      initial-value: 5px;
    }
    #child { --x: 20px; }
    #child { --x: unset; padding-left: var(--x); }
  "#;

  let sheet = parse_stylesheet(css).unwrap();
  let dom = simple_div_with_id("child");
  let styled = apply_styles(&dom, &sheet);

  assert_eq!(styled.styles.padding_left.value, 5.0);
  assert_eq!(styled.styles.padding_left.unit, LengthUnit::Px);
}

