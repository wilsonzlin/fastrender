use fastrender::css::types::{Declaration, PropertyValue};
use fastrender::style::color::Rgba;
use fastrender::style::properties::{apply_declaration, resolve_pending_logical_properties};
use fastrender::style::types::{Direction, WritingMode};
use fastrender::style::values::Length;
use fastrender::style::ComputedStyle;

fn decl(property: &str, value: PropertyValue) -> Declaration {
    Declaration {
        property: property.to_string(),
        value,
        raw_value: String::new(),
        important: false,
    }
}

#[test]
fn logical_margins_map_inline_and_block_axes() {
    let mut style = ComputedStyle::default();
    apply_declaration(
        &mut style,
        &decl("margin-inline-start", PropertyValue::Length(Length::px(10.0))),
        16.0,
        16.0,
    );
    apply_declaration(
        &mut style,
        &decl("margin-block-end", PropertyValue::Length(Length::px(20.0))),
        16.0,
        16.0,
    );
    resolve_pending_logical_properties(&mut style);
    assert_eq!(style.margin_left, Some(Length::px(10.0)));
    assert_eq!(style.margin_bottom, Some(Length::px(20.0)));
}

#[test]
fn logical_inline_respects_direction_rtl() {
    let mut style = ComputedStyle::default();
    style.direction = Direction::Rtl;
    apply_declaration(
        &mut style,
        &decl("margin-inline-start", PropertyValue::Length(Length::px(5.0))),
        16.0,
        16.0,
    );
    resolve_pending_logical_properties(&mut style);
    assert_eq!(style.margin_right, Some(Length::px(5.0)));
    assert_eq!(style.margin_left, Some(Length::px(0.0)));
}

#[test]
fn logical_padding_maps_in_vertical_writing_mode() {
    let mut style = ComputedStyle::default();
    style.writing_mode = WritingMode::VerticalRl;
    apply_declaration(
        &mut style,
        &decl(
            "padding-inline",
            PropertyValue::Multiple(vec![PropertyValue::Length(Length::px(8.0)), PropertyValue::Length(Length::px(10.0))]),
        ),
        16.0,
        16.0,
    );
    apply_declaration(
        &mut style,
        &decl(
            "padding-block",
            PropertyValue::Multiple(vec![PropertyValue::Length(Length::px(2.0)), PropertyValue::Length(Length::px(4.0))]),
        ),
        16.0,
        16.0,
    );
    resolve_pending_logical_properties(&mut style);
    // Inline axis is vertical in vertical-rl, block axis is horizontal right->left.
    assert_eq!(style.padding_top, Length::px(8.0));
    assert_eq!(style.padding_bottom, Length::px(10.0));
    assert_eq!(style.padding_right, Length::px(2.0));
    assert_eq!(style.padding_left, Length::px(4.0));
}

#[test]
fn logical_border_respects_cascade_order() {
    let mut style = ComputedStyle::default();
    apply_declaration(
        &mut style,
        &decl("margin-left", PropertyValue::Length(Length::px(3.0))),
        16.0,
        16.0,
    );
    apply_declaration(
        &mut style,
        &decl("margin-inline-start", PropertyValue::Length(Length::px(9.0))),
        16.0,
        16.0,
    );
    resolve_pending_logical_properties(&mut style);
    assert_eq!(style.margin_left, Some(Length::px(9.0)));

    apply_declaration(
        &mut style,
        &decl("margin-left", PropertyValue::Length(Length::px(1.5))),
        16.0,
        16.0,
    );
    resolve_pending_logical_properties(&mut style);
    assert_eq!(style.margin_left, Some(Length::px(1.5)));
}

#[test]
fn logical_border_inline_colors_follow_writing_mode() {
    let mut style = ComputedStyle::default();
    style.writing_mode = WritingMode::VerticalRl;
    apply_declaration(
        &mut style,
        &decl(
            "border-inline-color",
            PropertyValue::Multiple(vec![
                PropertyValue::Color(Rgba::RED),
                PropertyValue::Color(Rgba::GREEN),
            ]),
        ),
        16.0,
        16.0,
    );
    resolve_pending_logical_properties(&mut style);
    // Inline axis is vertical in vertical-rl -> start is top, end is bottom.
    assert_eq!(style.border_top_color, Rgba::RED);
    assert_eq!(style.border_bottom_color, Rgba::GREEN);
}
