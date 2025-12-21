use fastrender::css::types::{Declaration, PropertyValue};
use fastrender::style::properties::apply_declaration;
use fastrender::style::types::TextOrientation;
use fastrender::style::ComputedStyle;

#[test]
fn text_orientation_keywords_are_case_insensitive() {
    let mut style = ComputedStyle::default();

    apply_declaration(
        &mut style,
        &Declaration {
            property: "text-orientation".into(),
            value: PropertyValue::Keyword("UPRIGHT".into()),
            raw_value: String::new(),
            important: false,
        },
        &ComputedStyle::default(),
        16.0,
        16.0,
    );
    assert_eq!(style.text_orientation, TextOrientation::Upright);

    apply_declaration(
        &mut style,
        &Declaration {
            property: "text-orientation".into(),
            value: PropertyValue::Keyword("SIDEWAYS-LEFT".into()),
            raw_value: String::new(),
            important: false,
        },
        &ComputedStyle::default(),
        16.0,
        16.0,
    );
    assert_eq!(style.text_orientation, TextOrientation::SidewaysLeft);
}
