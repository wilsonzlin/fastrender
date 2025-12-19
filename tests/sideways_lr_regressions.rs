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
