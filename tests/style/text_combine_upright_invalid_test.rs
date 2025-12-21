use fastrender::css::types::Declaration;
use fastrender::css::types::PropertyValue;
use fastrender::style::properties::apply_declaration;
use fastrender::style::types::TextCombineUpright;
use fastrender::style::ComputedStyle;

#[test]
fn text_combine_upright_rejects_out_of_range_digits() {
  let mut style = ComputedStyle::default();
  apply_declaration(
    &mut style,
    &Declaration {
      property: "text-combine-upright".into(),
      value: PropertyValue::Multiple(vec![
        PropertyValue::Keyword("digits".into()),
        PropertyValue::Number(5.0),
      ]),
      raw_value: String::new(),
      important: false,
    },
    &ComputedStyle::default(),
    16.0,
    16.0,
  );

  assert_eq!(style.text_combine_upright, TextCombineUpright::None);
}
