use fastrender::css::properties::parse_property_value;
use fastrender::css::types::PropertyValue;

fn as_number(prop: &str, value: &str) -> Option<f32> {
    match parse_property_value(prop, value)? {
        PropertyValue::Number(n) => Some(n),
        _ => None,
    }
}

#[test]
fn numeric_properties_accept_calc_min_max_clamp() {
    assert_eq!(as_number("opacity", "calc(0.5)"), Some(0.5));
    assert_eq!(as_number("opacity", "min(0.5, 0.7)"), Some(0.5));
    assert_eq!(as_number("opacity", "max(0.3, 0.7)"), Some(0.7));
    assert_eq!(as_number("opacity", "clamp(0.2, 0.4, 0.6)"), Some(0.4));

    assert_eq!(as_number("z-index", "calc(2)"), Some(2.0));
    assert_eq!(as_number("z-index", "min(1, 3)"), Some(1.0));
    assert_eq!(as_number("z-index", "max(5, 3)"), Some(5.0));
    assert_eq!(as_number("z-index", "clamp(1, 4, 6)"), Some(4.0));
}
