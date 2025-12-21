use fastrender::style::media::MediaQuery;

#[test]
fn rejects_unknown_media_type() {
    // CSS spec: unknown/unsupported media types are invalid and cause the query to fail.
    let q = MediaQuery::parse("@media foobar and (min-width: 500px)");
    assert!(q.is_err(), "unknown media type should be invalid");
}

#[test]
fn unitless_zero_parses_for_numeric_properties() {
    use fastrender::css::properties::parse_property_value;
    use fastrender::css::types::PropertyValue;

    assert!(matches!(
        parse_property_value("opacity", "0"),
        Some(PropertyValue::Number(n)) if (n - 0.0).abs() < f32::EPSILON
    ));
    assert!(
        matches!(parse_property_value("z-index", "0"), Some(PropertyValue::Number(n)) if (n - 0.0).abs() < f32::EPSILON)
    );
}
