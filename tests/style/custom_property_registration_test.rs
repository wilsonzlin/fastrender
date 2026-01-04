use fastrender::css::parser::parse_stylesheet;
use fastrender::dom::{DomNode, DomNodeType, HTML_NAMESPACE};
use fastrender::style::cascade::apply_styles;
use fastrender::style::color::Rgba;
use fastrender::style::values::{CustomPropertySyntax, CustomPropertyTypedValue, Length, LengthUnit};

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

#[test]
fn tailwind_style_property_registration_allows_inherits_false_without_initial_value() {
  // Tailwind registers many `syntax:"*"` properties without an explicit initial value, then
  // assigns `initial` in a universal reset so `var(--x,)` falls back to the empty string.
  let css = r#"
    @property --x {
      syntax: "*";
      inherits: false;
    }
    #root { --x: initial; width: var(--x, 10px); }
  "#;
  let sheet = parse_stylesheet(css).unwrap();
  let dom = DomNode {
    node_type: DomNodeType::Element {
      tag_name: "div".to_string(),
      namespace: HTML_NAMESPACE.to_string(),
      attributes: vec![("id".to_string(), "root".to_string())],
    },
    children: vec![],
  };
  let styled = apply_styles(&dom, &sheet);

  let rule = styled
    .styles
    .custom_property_registry
    .get("--x")
    .expect("registered property");
  assert_eq!(rule.syntax, CustomPropertySyntax::Universal);
  assert!(!rule.inherits);
  assert!(rule.initial_value.is_none());

  assert_eq!(styled.styles.width, Some(Length::px(10.0)));
  assert!(
    styled.styles.custom_properties.get("--x").is_none(),
    "global keyword `initial` should reset to the guaranteed-invalid value"
  );
}

#[test]
fn property_rule_missing_syntax_descriptor_is_rejected() {
  let css = r#"
    @property --missing-syntax {
      inherits: false;
      initial-value: 0;
    }
  "#;
  let sheet = parse_stylesheet(css).unwrap();
  let dom = simple_div_dom();
  let styled = apply_styles(&dom, &sheet);
  assert!(styled
    .styles
    .custom_property_registry
    .get("--missing-syntax")
    .is_none());
}

#[test]
fn property_rule_with_invalid_syntax_descriptor_is_rejected() {
  let css = r#"
    @property --bad-syntax {
      syntax: "<definitely-not-a-syntax>";
      inherits: false;
      initial-value: 0;
    }
  "#;
  let sheet = parse_stylesheet(css).unwrap();
  let dom = simple_div_dom();
  let styled = apply_styles(&dom, &sheet);
  assert!(styled
    .styles
    .custom_property_registry
    .get("--bad-syntax")
    .is_none());
}

#[test]
fn custom_property_syntax_length_percentage_parses_length_and_percentage() {
  assert_eq!(
    CustomPropertySyntax::parse("<length-percentage>"),
    Some(CustomPropertySyntax::LengthPercentage)
  );
  assert_eq!(
    CustomPropertySyntax::LengthPercentage.parse_value("10px"),
    Some(CustomPropertyTypedValue::Length(Length::px(10.0)))
  );
  assert_eq!(
    CustomPropertySyntax::LengthPercentage.parse_value("25%"),
    Some(CustomPropertyTypedValue::Length(Length::new(25.0, LengthUnit::Percent)))
  );
  assert!(CustomPropertySyntax::LengthPercentage
    .parse_value("not-a-length")
    .is_none());
}

#[test]
fn length_percentage_property_registration_parses_initial_value() {
  let css = r#"
    @property --pos {
      syntax: "<length-percentage>";
      inherits: false;
      initial-value: 50%;
    }
    div { --pos: 10px; }
  "#;
  let sheet = parse_stylesheet(css).unwrap();
  let dom = simple_div_dom();
  let styled = apply_styles(&dom, &sheet);

  let rule = styled
    .styles
    .custom_property_registry
    .get("--pos")
    .expect("registered property");
  assert_eq!(rule.syntax, CustomPropertySyntax::LengthPercentage);
  let initial = rule.initial_value.as_ref().expect("initial value");
  assert_eq!(initial.value.trim(), "50%");
  assert_eq!(
    initial.typed,
    Some(CustomPropertyTypedValue::Length(Length::new(50.0, LengthUnit::Percent)))
  );

  let value = styled
    .styles
    .custom_properties
    .get("--pos")
    .expect("computed property");
  assert_eq!(value.value.trim(), "10px");
  assert_eq!(
    value.typed,
    Some(CustomPropertyTypedValue::Length(Length::px(10.0)))
  );
}
