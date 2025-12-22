use fastrender::css::parser::parse_stylesheet;
use fastrender::dom::{DomNode, DomNodeType, HTML_NAMESPACE};
use fastrender::style::cascade::apply_styles;
use fastrender::style::content::{ContentContext, ContentGenerator};
use fastrender::style::counter_styles::CounterStyleName;
use fastrender::style::types::ListStyleType;

fn simple_list_dom() -> DomNode {
  DomNode {
    node_type: DomNodeType::Element {
      tag_name: "ul".to_string(),
      namespace: HTML_NAMESPACE.to_string(),
      attributes: vec![],
    },
    children: vec![DomNode {
      node_type: DomNodeType::Element {
        tag_name: "li".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
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
fn layered_counter_style_prefers_later_layer() {
  let css = r#"
    @layer base { @counter-style foo { system: cyclic; symbols: "a"; } }
    @layer override { @counter-style foo { system: cyclic; symbols: "b"; } }
    li { list-style-type: foo; }
  "#;
  let sheet = parse_stylesheet(css).expect("stylesheet");
  let dom = simple_list_dom();
  let styled = apply_styles(&dom, &sheet);
  let li = styled.children.first().expect("li");

  assert!(matches!(
    li.styles.list_style_type,
    ListStyleType::Custom(ref name) if name == "foo"
  ));
  assert_eq!(
    li.styles
      .counter_styles
      .format_value(1, CounterStyleName::from("foo")),
    "b"
  );
}

#[test]
fn additive_counter_style_parses_and_formats() {
  let css = r#"
    @counter-style additive {
      system: additive;
      additive-symbols: 5 "V", 1 "I";
      negative: "(" ")";
      range: -10 10;
    }
  "#;
  let sheet = parse_stylesheet(css).expect("stylesheet");
  let dom = simple_list_dom();
  let registry = &apply_styles(&dom, &sheet).styles.counter_styles;

  assert_eq!(
    registry.format_value(6, CounterStyleName::from("additive")),
    "VI"
  );
  assert_eq!(
    registry.format_value(-3, CounterStyleName::from("additive")),
    "(III)"
  );
  assert_eq!(
    registry.format_value(0, CounterStyleName::from("additive")),
    "0"
  );
}

#[test]
fn extends_chain_inherits_pad_and_negative() {
  let css = r#"
    @counter-style base {
      system: numeric;
      symbols: "0" "1";
      negative: "<" ">";
    }
    @counter-style derived {
      system: extends base;
      pad: 4 "0";
    }
    @counter-style leaf {
      system: extends derived;
    }
  "#;
  let sheet = parse_stylesheet(css).expect("stylesheet");
  let dom = simple_list_dom();
  let registry = &apply_styles(&dom, &sheet).styles.counter_styles;

  assert_eq!(
    registry.format_value(1, CounterStyleName::from("leaf")),
    "0001"
  );
  assert_eq!(
    registry.format_value(-3, CounterStyleName::from("leaf")),
    "<0011>"
  );
}

#[test]
fn range_descriptor_triggers_fallback_style() {
  let css = r#"
    @counter-style limited {
      system: numeric;
      symbols: "0" "1" "2";
      range: 1 3;
      fallback: lower-alpha;
    }
  "#;
  let sheet = parse_stylesheet(css).expect("stylesheet");
  let dom = simple_list_dom();
  let registry = &apply_styles(&dom, &sheet).styles.counter_styles;

  assert_eq!(
    registry.format_value(2, CounterStyleName::from("limited")),
    "2"
  );
  assert_eq!(
    registry.format_value(5, CounterStyleName::from("limited")),
    "e"
  );
}

#[test]
fn unlayered_counter_style_overrides_layered_definition() {
  let css = r#"
    @layer themed { @counter-style badge { system: cyclic; symbols: "x"; } }
    @counter-style badge { system: cyclic; symbols: "y"; }
    li { list-style-type: badge; }
  "#;
  let sheet = parse_stylesheet(css).expect("stylesheet");
  let dom = simple_list_dom();
  let styled = apply_styles(&dom, &sheet);
  let li = styled.children.first().expect("li");

  assert_eq!(
    li.styles
      .counter_styles
      .format_value(1, CounterStyleName::from("badge")),
    "y"
  );
}

#[test]
fn counter_function_uses_custom_counter_style() {
  let css = r#"
    @counter-style arrows {
      system: cyclic;
      symbols: "→" "↗" "↑";
    }
    div { counter-reset: step 0; }
    div::before { content: counter(step, arrows); }
  "#;
  let sheet = parse_stylesheet(css).expect("stylesheet");
  let dom = simple_div_dom();
  let styled = apply_styles(&dom, &sheet);
  let before = styled.before_styles.as_ref().expect("before style");

  let mut ctx = ContentContext::new();
  ctx.set_counter("step", 2);
  let gen = ContentGenerator::with_counter_styles(before.counter_styles.clone());
  assert_eq!(gen.generate(&before.content_value, &mut ctx), "↗");
}

#[test]
fn parsed_counter_style_extends_builtin_and_formats() {
  let css = r#"
    @counter-style padded {
      system: extends decimal;
      pad: 3 "0";
      negative: "<" ">";
    }
  "#;
  let sheet = parse_stylesheet(css).expect("stylesheet");
  let dom = simple_list_dom();
  let styled = apply_styles(&dom, &sheet);
  let registry = &styled.styles.counter_styles;

  assert_eq!(
    registry.format_value(7, CounterStyleName::from("padded")),
    "007"
  );
  assert_eq!(
    registry.format_value(-4, CounterStyleName::from("padded")),
    "<004>"
  );
}

#[test]
fn custom_list_style_type_uses_counter_style_registry() {
  let css = r#"
    @counter-style stars {
      system: cyclic;
      symbols: "★";
    }
    li { list-style-type: stars; }
  "#;
  let sheet = parse_stylesheet(css).expect("stylesheet");
  let dom = simple_list_dom();
  let styled = apply_styles(&dom, &sheet);
  let li = styled.children.first().expect("li");

  assert_eq!(
    li.styles
      .counter_styles
      .format_value(1, CounterStyleName::from("stars")),
    "★"
  );
}
