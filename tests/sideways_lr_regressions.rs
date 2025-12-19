use fastrender::css::types::{Declaration, PropertyValue};
use fastrender::style::properties::{apply_declaration, resolve_pending_logical_properties};
use fastrender::style::values::Length;
use fastrender::style::ComputedStyle;
use fastrender::style::types::WritingMode;

fn decl(property: &str, value: PropertyValue) -> Declaration {
    Declaration {
        property: property.to_string(),
        value,
        raw_value: String::new(),
        important: false,
    }
}

#[test]
fn logical_padding_maps_in_sideways_lr_writing_mode() {
    let mut style = ComputedStyle::default();
    style.writing_mode = WritingMode::SidewaysLr;
    apply_declaration(
        &mut style,
        &decl(
            "padding-inline",
            PropertyValue::Multiple(vec![
                PropertyValue::Length(Length::px(6.0)),
                PropertyValue::Length(Length::px(8.0)),
            ]),
        ),
        &ComputedStyle::default(),
        16.0,
        16.0,
    );
    apply_declaration(
        &mut style,
        &decl(
            "padding-block",
            PropertyValue::Multiple(vec![
                PropertyValue::Length(Length::px(3.0)),
                PropertyValue::Length(Length::px(9.0)),
            ]),
        ),
        &ComputedStyle::default(),
        16.0,
        16.0,
    );
    resolve_pending_logical_properties(&mut style);

    // Sideways-lr: inline axis is vertical (top → bottom), block axis is horizontal left → right.
    assert_eq!(style.padding_top, Length::px(6.0));
    assert_eq!(style.padding_bottom, Length::px(8.0));
    assert_eq!(style.padding_left, Length::px(3.0));
    assert_eq!(style.padding_right, Length::px(9.0));
}

#[test]
fn inline_and_block_sizes_map_in_sideways_lr_writing_mode() {
    let mut style = ComputedStyle::default();
    style.writing_mode = WritingMode::SidewaysLr;
    apply_declaration(
        &mut style,
        &decl("inline-size", PropertyValue::Length(Length::px(24.0))),
        &ComputedStyle::default(),
        16.0,
        16.0,
    );
    apply_declaration(
        &mut style,
        &decl("block-size", PropertyValue::Length(Length::px(44.0))),
        &ComputedStyle::default(),
        16.0,
        16.0,
    );
    resolve_pending_logical_properties(&mut style);

    // Sideways modes map inline-size to the vertical axis and block-size to the horizontal axis.
    assert_eq!(style.height, Some(Length::px(24.0)));
    assert_eq!(style.width, Some(Length::px(44.0)));
}

#[test]
fn logical_inset_maps_in_sideways_lr_writing_mode() {
    let mut style = ComputedStyle::default();
    style.writing_mode = WritingMode::SidewaysLr;
    apply_declaration(
        &mut style,
        &decl("inset-inline-start", PropertyValue::Length(Length::px(5.0))),
        &ComputedStyle::default(),
        16.0,
        16.0,
    );
    apply_declaration(
        &mut style,
        &decl("inset-block-end", PropertyValue::Length(Length::px(7.0))),
        &ComputedStyle::default(),
        16.0,
        16.0,
    );
    resolve_pending_logical_properties(&mut style);

    // Sideways-lr: inline axis vertical -> start maps to top; block axis horizontal left→right -> end maps to right.
    assert_eq!(style.top, Some(Length::px(5.0)));
    assert_eq!(style.right, Some(Length::px(7.0)));
}

#[test]
fn logical_margins_map_in_sideways_lr_writing_mode() {
    let mut style = ComputedStyle::default();
    style.writing_mode = WritingMode::SidewaysLr;
    apply_declaration(
        &mut style,
        &decl("margin-inline-start", PropertyValue::Length(Length::px(12.0))),
        &ComputedStyle::default(),
        16.0,
        16.0,
    );
    apply_declaration(
        &mut style,
        &decl("margin-block-end", PropertyValue::Length(Length::px(4.0))),
        &ComputedStyle::default(),
        16.0,
        16.0,
    );
    resolve_pending_logical_properties(&mut style);

    // Sideways-lr: inline axis vertical -> start maps to top; block axis horizontal left→right -> end maps to right.
    assert_eq!(style.margin_top, Some(Length::px(12.0)));
    assert_eq!(style.margin_right, Some(Length::px(4.0)));
}

#[test]
fn border_inline_color_maps_in_sideways_lr_writing_mode() {
    let mut style = ComputedStyle::default();
    style.writing_mode = WritingMode::SidewaysLr;
    apply_declaration(
        &mut style,
        &decl(
            "border-inline-color",
            PropertyValue::Multiple(vec![
                PropertyValue::Color(fastrender::style::color::Color::Rgba(fastrender::style::color::Rgba::RED)),
                PropertyValue::Color(fastrender::style::color::Color::Rgba(fastrender::style::color::Rgba::GREEN)),
            ]),
        ),
        &ComputedStyle::default(),
        16.0,
        16.0,
    );
    resolve_pending_logical_properties(&mut style);

    // Sideways-lr: inline axis is vertical -> start maps to top, end maps to bottom.
    assert_eq!(style.border_top_color, fastrender::style::color::Rgba::RED);
    assert_eq!(style.border_bottom_color, fastrender::style::color::Rgba::GREEN);
}

#[test]
fn border_inline_style_maps_in_sideways_lr_writing_mode() {
    let mut style = ComputedStyle::default();
    style.writing_mode = WritingMode::SidewaysLr;
    apply_declaration(
        &mut style,
        &decl(
            "border-inline-style",
            PropertyValue::Multiple(vec![
                PropertyValue::Keyword("dashed".into()),
                PropertyValue::Keyword("dotted".into()),
            ]),
        ),
        &ComputedStyle::default(),
        16.0,
        16.0,
    );
    resolve_pending_logical_properties(&mut style);

    // Sideways-lr inline axis is vertical -> start is top, end is bottom.
    assert_eq!(style.border_top_style, fastrender::style::types::BorderStyle::Dashed);
    assert_eq!(style.border_bottom_style, fastrender::style::types::BorderStyle::Dotted);
}

#[test]
fn border_inline_width_maps_in_sideways_lr_writing_mode() {
    let mut style = ComputedStyle::default();
    style.writing_mode = WritingMode::SidewaysLr;
    apply_declaration(
        &mut style,
        &decl(
            "border-inline-width",
            PropertyValue::Multiple(vec![
                PropertyValue::Length(Length::px(2.0)),
                PropertyValue::Length(Length::px(5.0)),
            ]),
        ),
        &ComputedStyle::default(),
        16.0,
        16.0,
    );
    resolve_pending_logical_properties(&mut style);

    // Sideways-lr inline axis is vertical -> start width maps to top, end width maps to bottom.
    assert_eq!(style.border_top_width, Length::px(2.0));
    assert_eq!(style.border_bottom_width, Length::px(5.0));
}

#[test]
fn logical_border_radius_maps_in_sideways_lr_writing_mode() {
    let mut style = ComputedStyle::default();
    style.writing_mode = WritingMode::SidewaysLr;
    apply_declaration(
        &mut style,
        &decl("border-start-start-radius", PropertyValue::Length(Length::px(9.0))),
        &ComputedStyle::default(),
        16.0,
        16.0,
    );
    apply_declaration(
        &mut style,
        &decl("border-end-end-radius", PropertyValue::Length(Length::px(4.0))),
        &ComputedStyle::default(),
        16.0,
        16.0,
    );
    resolve_pending_logical_properties(&mut style);

    // Sideways-lr: inline axis vertical, block axis horizontal left→right.
    // start-start → top-left, end-end → bottom-right.
    assert_eq!(style.border_top_left_radius, Length::px(9.0));
    assert_eq!(style.border_bottom_right_radius, Length::px(4.0));
}

#[test]
fn background_position_logical_maps_in_sideways_lr_writing_mode() {
    let mut style = ComputedStyle::default();
    style.writing_mode = WritingMode::SidewaysLr;
    apply_declaration(
        &mut style,
        &decl(
            "background-position-inline",
            PropertyValue::Multiple(vec![
                PropertyValue::Length(Length::px(10.0)),
                PropertyValue::Length(Length::px(20.0)),
            ]),
        ),
        &ComputedStyle::default(),
        16.0,
        16.0,
    );
    apply_declaration(
        &mut style,
        &decl(
            "background-position-block",
            PropertyValue::Multiple(vec![
                PropertyValue::Length(Length::px(3.0)),
                PropertyValue::Length(Length::px(7.0)),
            ]),
        ),
        &ComputedStyle::default(),
        16.0,
        16.0,
    );
    resolve_pending_logical_properties(&mut style);

    // Note: logical background-position properties are not yet supported; ensure defaults remain stable.
    assert_eq!(style.background_positions.len(), 1);
    let fastrender::style::types::BackgroundPosition::Position { x, y } = style.background_positions[0];
    assert_eq!(x.offset, Length::percent(0.0));
    assert_eq!(y.offset, Length::percent(0.0));
}

#[test]
fn background_size_logical_is_not_mapped_in_sideways_lr() {
    let mut style = ComputedStyle::default();
    style.writing_mode = WritingMode::SidewaysLr;
    // Author uses logical size props; currently unsupported -> should retain defaults.
    apply_declaration(
        &mut style,
        &decl(
            "background-size-inline",
            PropertyValue::Multiple(vec![
                PropertyValue::Length(Length::px(30.0)),
                PropertyValue::Length(Length::px(40.0)),
            ]),
        ),
        &ComputedStyle::default(),
        16.0,
        16.0,
    );
    apply_declaration(
        &mut style,
        &decl(
            "background-size-block",
            PropertyValue::Multiple(vec![
                PropertyValue::Length(Length::px(10.0)),
                PropertyValue::Length(Length::px(20.0)),
            ]),
        ),
        &ComputedStyle::default(),
        16.0,
        16.0,
    );
    resolve_pending_logical_properties(&mut style);

    // Logical size props are currently ignored; we should still have exactly one layer with default size.
    assert_eq!(style.background_sizes.len(), 1);
    // Default background-size is auto/auto represented as Explicit(auto, auto).
    assert_eq!(style.background_sizes[0], fastrender::style::types::BackgroundSize::Explicit(
        fastrender::style::types::BackgroundSizeComponent::Auto,
        fastrender::style::types::BackgroundSizeComponent::Auto,
    ));
}
