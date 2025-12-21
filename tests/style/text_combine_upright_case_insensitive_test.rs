use fastrender::css::types::{Declaration, PropertyValue};
use fastrender::style::properties::apply_declaration;
use fastrender::style::types::TextCombineUpright;
use fastrender::style::ComputedStyle;

#[test]
fn text_combine_upright_keywords_are_case_insensitive() {
    let mut style = ComputedStyle::default();

    apply_declaration(
        &mut style,
        &Declaration {
            property: "text-combine-upright".into(),
            value: PropertyValue::Keyword("DIGITS".into()),
            raw_value: String::new(),
            important: false,
        },
        &ComputedStyle::default(),
        16.0,
        16.0,
    );
    assert_eq!(style.text_combine_upright, TextCombineUpright::Digits(2));

    apply_declaration(
        &mut style,
        &Declaration {
            property: "text-combine-upright".into(),
            value: PropertyValue::Keyword("ALL".into()),
            raw_value: String::new(),
            important: false,
        },
        &ComputedStyle::default(),
        16.0,
        16.0,
    );
    assert_eq!(style.text_combine_upright, TextCombineUpright::All);
}
