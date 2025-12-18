use fastrender::css::properties::parse_length;
use fastrender::css::types::{Declaration, PropertyValue};
use fastrender::style::color::{Color, Rgba};
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
    apply_declaration(&mut style, &decl("margin-inline-start", PropertyValue::Length(Length::px(10.0))), &ComputedStyle::default(), 16.0, 16.0);
    apply_declaration(&mut style, &decl("margin-block-end", PropertyValue::Length(Length::px(20.0))), &ComputedStyle::default(), 16.0, 16.0);
    resolve_pending_logical_properties(&mut style);
    assert_eq!(style.margin_left, Some(Length::px(10.0)));
    assert_eq!(style.margin_bottom, Some(Length::px(20.0)));
}

#[test]
fn logical_inline_respects_direction_rtl() {
    let mut style = ComputedStyle::default();
    style.direction = Direction::Rtl;
    apply_declaration(&mut style, &decl("margin-inline-start", PropertyValue::Length(Length::px(5.0))), &ComputedStyle::default(), 16.0, 16.0);
    resolve_pending_logical_properties(&mut style);
    assert_eq!(style.margin_right, Some(Length::px(5.0)));
    assert_eq!(style.margin_left, Some(Length::px(0.0)));
}

#[test]
fn logical_padding_maps_in_vertical_writing_mode() {
    let mut style = ComputedStyle::default();
    style.writing_mode = WritingMode::VerticalRl;
    apply_declaration(&mut style, &decl(
            "padding-inline",
            PropertyValue::Multiple(vec![PropertyValue::Length(Length::px(8.0)), PropertyValue::Length(Length::px(10.0))]),
        ), &ComputedStyle::default(), 16.0, 16.0);
    apply_declaration(&mut style, &decl(
            "padding-block",
            PropertyValue::Multiple(vec![PropertyValue::Length(Length::px(2.0)), PropertyValue::Length(Length::px(4.0))]),
        ), &ComputedStyle::default(), 16.0, 16.0);
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
    apply_declaration(&mut style, &decl("margin-left", PropertyValue::Length(Length::px(3.0))), &ComputedStyle::default(), 16.0, 16.0);
    apply_declaration(&mut style, &decl("margin-inline-start", PropertyValue::Length(Length::px(9.0))), &ComputedStyle::default(), 16.0, 16.0);
    resolve_pending_logical_properties(&mut style);
    assert_eq!(style.margin_left, Some(Length::px(9.0)));

    apply_declaration(&mut style, &decl("margin-left", PropertyValue::Length(Length::px(1.5))), &ComputedStyle::default(), 16.0, 16.0);
    resolve_pending_logical_properties(&mut style);
    assert_eq!(style.margin_left, Some(Length::px(1.5)));
}

#[test]
fn margin_and_inset_accept_calc_zero() {
    let mut style = ComputedStyle::default();
    let zero = parse_length("calc(0)").expect("calc zero");
    apply_declaration(
        &mut style,
        &decl("margin", PropertyValue::Multiple(vec![PropertyValue::Length(zero.clone())])),
        &ComputedStyle::default(),
        16.0,
        16.0,
    );
    assert_eq!(style.margin_top, Some(Length::px(0.0)));
    assert_eq!(style.margin_right, Some(Length::px(0.0)));
    assert_eq!(style.margin_bottom, Some(Length::px(0.0)));
    assert_eq!(style.margin_left, Some(Length::px(0.0)));

    apply_declaration(
        &mut style,
        &decl("inset", PropertyValue::Multiple(vec![PropertyValue::Length(zero)])),
        &ComputedStyle::default(),
        16.0,
        16.0,
    );
    assert_eq!(style.top, Some(Length::px(0.0)));
    assert_eq!(style.right, Some(Length::px(0.0)));
    assert_eq!(style.bottom, Some(Length::px(0.0)));
    assert_eq!(style.left, Some(Length::px(0.0)));
}

#[test]
fn logical_border_inline_colors_follow_writing_mode() {
    let mut style = ComputedStyle::default();
    style.writing_mode = WritingMode::VerticalRl;
    apply_declaration(&mut style, &decl(
            "border-inline-color",
            PropertyValue::Multiple(vec![
                PropertyValue::Color(Color::Rgba(Rgba::RED)),
                PropertyValue::Color(Color::Rgba(Rgba::GREEN)),
            ]),
        ), &ComputedStyle::default(), 16.0, 16.0);
    resolve_pending_logical_properties(&mut style);
    // Inline axis is vertical in vertical-rl -> start is top, end is bottom.
    assert_eq!(style.border_top_color, Rgba::RED);
    assert_eq!(style.border_bottom_color, Rgba::GREEN);
}

#[test]
fn inline_and_block_sizes_map_to_physical_axes() {
    let mut style = ComputedStyle::default();
    apply_declaration(&mut style, &decl("inline-size", PropertyValue::Length(Length::px(40.0))), &ComputedStyle::default(), 16.0, 16.0);
    apply_declaration(&mut style, &decl("block-size", PropertyValue::Length(Length::px(60.0))), &ComputedStyle::default(), 16.0, 16.0);
    resolve_pending_logical_properties(&mut style);
    assert_eq!(style.width, Some(Length::px(40.0)));
    assert_eq!(style.height, Some(Length::px(60.0)));

    // Inline-size should map to block axis in vertical writing.
    style = ComputedStyle::default();
    style.writing_mode = WritingMode::VerticalRl;
    apply_declaration(&mut style, &decl("inline-size", PropertyValue::Length(Length::px(25.0))), &ComputedStyle::default(), 16.0, 16.0);
    resolve_pending_logical_properties(&mut style);
    assert_eq!(style.height, Some(Length::px(25.0)));
    assert_eq!(style.width, None);
}

#[test]
fn logical_inset_maps_to_physical_sides() {
    let mut style = ComputedStyle::default();
    style.writing_mode = WritingMode::VerticalRl;
    apply_declaration(&mut style, &decl("inset-inline-start", PropertyValue::Length(Length::px(5.0))), &ComputedStyle::default(), 16.0, 16.0);
    apply_declaration(&mut style, &decl("inset-block-end", PropertyValue::Length(Length::px(7.0))), &ComputedStyle::default(), 16.0, 16.0);
    resolve_pending_logical_properties(&mut style);

    // Vertical-rl: inline axis is vertical -> start at top, block axis horizontal right->left -> end is left.
    assert_eq!(style.top, Some(Length::px(5.0)));
    assert_eq!(style.left, Some(Length::px(7.0)));
}

#[test]
fn logical_border_radius_maps_corners() {
    let mut style = ComputedStyle::default();
    style.direction = Direction::Rtl;
    apply_declaration(&mut style, &decl("border-start-start-radius", PropertyValue::Length(Length::px(9.0))), &ComputedStyle::default(), 16.0, 16.0);
    apply_declaration(&mut style, &decl("border-end-end-radius", PropertyValue::Length(Length::px(4.0))), &ComputedStyle::default(), 16.0, 16.0);
    resolve_pending_logical_properties(&mut style);
    // RTL inline-start maps to right, block-start maps to top.
    assert_eq!(style.border_top_right_radius, Length::px(9.0));
    // block-end with rtl inline-end -> bottom-left
    assert_eq!(style.border_bottom_left_radius, Length::px(4.0));

    // Vertical writing: inline axis vertical, block axis horizontal.
    let mut vertical = ComputedStyle::default();
    vertical.writing_mode = WritingMode::VerticalRl;
    apply_declaration(&mut vertical, &decl("border-start-end-radius", PropertyValue::Length(Length::px(3.0))), &ComputedStyle::default(), 16.0, 16.0);
    resolve_pending_logical_properties(&mut vertical);
    // block-start -> right, inline-end -> bottom in vertical-rl
    assert_eq!(vertical.border_bottom_right_radius, Length::px(3.0));
}
