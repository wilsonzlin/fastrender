use cssparser::ToCss;
use fastrender::css::parser::parse_stylesheet;
use fastrender::css::types::StyleRule;
use fastrender::dom::{DomNode, DomNodeType, HTML_NAMESPACE};
use fastrender::style::cascade::apply_styles_with_media;
use fastrender::style::color::Rgba;
use fastrender::style::media::MediaContext;

fn selector_strings(rule: &StyleRule) -> Vec<String> {
  rule
    .selectors
    .slice()
    .iter()
    .map(|s| s.to_css_string())
    .collect()
}

#[test]
fn nested_rules_expand_with_parent_selectors() {
  let css = ".foo { color: red; .bar { color: blue; } }";
  let sheet = parse_stylesheet(css).expect("stylesheet");
  let collected = sheet.collect_style_rules(&MediaContext::default());

  assert_eq!(
    selector_strings(collected[0].rule),
    vec![".foo".to_string()]
  );
  assert_eq!(
    selector_strings(collected[1].rule),
    vec![".foo .bar".to_string()]
  );
}

#[test]
fn nested_rules_cross_product_selector_lists() {
  let css = ".a, .b { &.c, .d { color: green; } }";
  let sheet = parse_stylesheet(css).expect("stylesheet");
  let collected = sheet.collect_style_rules(&MediaContext::default());

  assert_eq!(
    selector_strings(collected[0].rule),
    vec![".a".to_string(), ".b".to_string()]
  );

  let mut nested = selector_strings(collected[1].rule);
  nested.sort();
  let mut expected = vec![".a .d", ".a.c", ".b .d", ".b.c"]
    .into_iter()
    .map(String::from)
    .collect::<Vec<_>>();
  expected.sort();
  assert_eq!(nested, expected);
}

#[test]
fn nested_rules_inside_media_inherit_parent_selector() {
  let css = ".wrap { @media (min-width: 500px) { .child { color: blue; } } }";
  let sheet = parse_stylesheet(css).expect("stylesheet");

  let narrow = sheet.collect_style_rules(&MediaContext::screen(320.0, 480.0));
  assert_eq!(narrow.len(), 1);
  assert_eq!(selector_strings(narrow[0].rule), vec![".wrap".to_string()]);

  let wide = sheet.collect_style_rules(&MediaContext::screen(800.0, 600.0));
  assert_eq!(wide.len(), 2);
  assert_eq!(
    selector_strings(wide[1].rule),
    vec![".wrap .child".to_string()]
  );
}

#[test]
fn nested_rule_order_follows_parent() {
  let css = ".item { color: red; & { color: rgb(0, 255, 0); } }";
  let sheet = parse_stylesheet(css).expect("stylesheet");
  let dom = DomNode {
    node_type: DomNodeType::Element {
      tag_name: "div".into(),
      namespace: HTML_NAMESPACE.to_string(),
      attributes: vec![("class".into(), "item".into())],
    },
    children: vec![],
  };

  let styled = apply_styles_with_media(&dom, &sheet, &MediaContext::default());
  assert_eq!(styled.styles.color, Rgba::rgb(0, 255, 0));
}

#[test]
fn nest_alias_and_ampersand_mid_selector_work() {
  let css = ".parent { @nest & > .child { color: black; } & + & { color: red; } }";
  let sheet = parse_stylesheet(css).expect("stylesheet");
  let collected = sheet.collect_style_rules(&MediaContext::default());

  let selectors: Vec<Vec<String>> = collected.iter().map(|r| selector_strings(r.rule)).collect();

  assert!(selectors.contains(&vec![".parent > .child".into()]));
  assert!(selectors.contains(&vec![".parent + .parent".into()]));
}

#[test]
fn nesting_with_is_parent_preserves_selector() {
  let css = ":is(.foo, .bar) { .child { color: blue; } }";
  let sheet = parse_stylesheet(css).expect("stylesheet");
  let collected = sheet.collect_style_rules(&MediaContext::default());

  assert_eq!(
    selector_strings(collected[0].rule),
    vec![":is(.foo, .bar)".to_string()]
  );
  assert_eq!(
    selector_strings(collected[1].rule),
    vec![":is(.foo, .bar) .child".to_string()]
  );
}
