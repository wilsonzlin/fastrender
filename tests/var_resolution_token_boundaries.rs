use cssparser::{Parser, ParserInput, Token};
use fastrender::css::types::PropertyValue;
use fastrender::style::custom_property_store::CustomPropertyStore;
use fastrender::style::values::CustomPropertyValue;
use fastrender::style::var_resolution::{resolve_var_for_property, VarResolutionResult};

fn make_props(pairs: &[(&str, &str)]) -> CustomPropertyStore {
  let mut store = CustomPropertyStore::default();
  for (name, value) in pairs.iter().copied() {
    store.insert(name.into(), CustomPropertyValue::new(value, None));
  }
  store
}

#[test]
fn adjacent_var_substitution_preserves_token_boundaries() {
  let props = make_props(&[("--a", "0"), ("--b", "calc(1px)")]);
  let value = PropertyValue::Keyword("var(--a)var(--b)".to_string());

  let resolved = resolve_var_for_property(&value, &props, "");
  let VarResolutionResult::Resolved { css_text, .. } = resolved else {
    panic!("expected successful var() resolution, got {resolved:?}");
  };

  let resolved_text = css_text.as_ref();
  let mut input = ParserInput::new(resolved_text);
  let mut parser = Parser::new(&mut input);

  let first = parser.next().expect("expected first token");
  assert!(
    matches!(first, Token::Number { value, .. } if *value == 0.0),
    "expected first token to be number 0, got {first:?} (resolved: {resolved_text:?})"
  );

  let second = parser.next().expect("expected second token");
  assert!(
    matches!(second, Token::Function(name) if name.eq_ignore_ascii_case("calc")),
    "expected second token to be calc() function, got {second:?} (resolved: {resolved_text:?})"
  );
}

#[test]
fn tailwind_empty_fallback_var_calls_keep_transform_parseable() {
  let props = CustomPropertyStore::default();
  let value = PropertyValue::Keyword(
    "var(--tw-rotate-x,)var(--tw-rotate-y,)translate(0px,0px)".to_string(),
  );

  let resolved = resolve_var_for_property(&value, &props, "transform");
  let VarResolutionResult::Resolved { value, css_text } = resolved else {
    panic!("expected successful var() resolution, got {resolved:?}");
  };

  assert!(
    !css_text.as_ref().contains("var("),
    "expected resolved transform value to contain no var(), got {css_text:?}"
  );

  match value.as_ref() {
    PropertyValue::Transform(list) => assert_eq!(list.len(), 1),
    other => panic!("expected parsed transform list, got {other:?}"),
  }
}
