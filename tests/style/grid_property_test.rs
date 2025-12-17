use fastrender::css::types::{Declaration, PropertyValue};
use fastrender::style::properties::apply_declaration;
use fastrender::style::types::GridTrack;
use fastrender::style::ComputedStyle;

fn decl(name: &str, value: PropertyValue) -> Declaration {
    Declaration {
        property: name.to_string(),
        value,
    }
}

#[test]
fn grid_template_areas_create_line_names() {
    let mut style = ComputedStyle::default();

    apply_declaration(
        &mut style,
        &decl("grid-template-areas", PropertyValue::Keyword("\"a a\" \"b c\"".into())),
        &ComputedStyle::default(),
        16.0,
        16.0,
    );

    // grid-template-areas should synthesize track sizing when absent
    assert_eq!(style.grid_template_columns, vec![GridTrack::Auto, GridTrack::Auto]);
    assert_eq!(style.grid_template_rows, vec![GridTrack::Auto, GridTrack::Auto]);
    assert_eq!(style.grid_column_line_names.len(), 3);
    assert_eq!(style.grid_row_line_names.len(), 3);

    // Area names define start/end line names on the corresponding boundaries
    assert!(style.grid_column_line_names[0].contains(&"a-start".to_string()));
    assert!(style.grid_column_line_names[0].contains(&"b-start".to_string()));
    assert!(style.grid_column_line_names[2].contains(&"a-end".to_string()));
    assert!(style.grid_column_line_names[2].contains(&"c-end".to_string()));

    assert!(style.grid_row_line_names[0].contains(&"a-start".to_string()));
    assert!(style.grid_row_line_names[1].contains(&"a-end".to_string()));
    assert!(style.grid_row_line_names[1].contains(&"b-start".to_string()));
    assert!(style.grid_row_line_names[2].contains(&"b-end".to_string()));
    assert!(style.grid_row_line_names[1].contains(&"c-start".to_string()));
    assert!(style.grid_row_line_names[2].contains(&"c-end".to_string()));
}

#[test]
fn grid_template_shorthand_synthesizes_area_line_names() {
    let mut style = ComputedStyle::default();

    apply_declaration(
        &mut style,
        &decl(
            "grid-template",
            PropertyValue::Keyword("\"a a\" \"b c\" / 20px 30px".into()),
        ),
        &ComputedStyle::default(),
        16.0,
        16.0,
    );

    assert_eq!(style.grid_template_columns.len(), 2);
    assert_eq!(style.grid_template_rows.len(), 2);
    assert_eq!(style.grid_column_line_names.len(), 3);
    assert_eq!(style.grid_row_line_names.len(), 3);

    assert!(style.grid_column_line_names[0].contains(&"a-start".to_string()));
    assert!(style.grid_column_line_names[2].contains(&"a-end".to_string()));
    assert!(style.grid_row_line_names[1].contains(&"b-start".to_string()));
    assert!(style.grid_row_line_names[2].contains(&"b-end".to_string()));
}

#[test]
fn grid_shorthand_template_synthesizes_area_line_names() {
    let mut style = ComputedStyle::default();

    apply_declaration(
        &mut style,
        &decl("grid", PropertyValue::Keyword("\"x x\" \"y z\"".into())),
        &ComputedStyle::default(),
        16.0,
        16.0,
    );

    assert_eq!(style.grid_template_columns.len(), 2);
    assert_eq!(style.grid_template_rows.len(), 2);
    assert_eq!(style.grid_column_line_names.len(), 3);
    assert_eq!(style.grid_row_line_names.len(), 3);

    assert!(style.grid_column_line_names[0].contains(&"x-start".to_string()));
    assert!(style.grid_column_line_names[2].contains(&"x-end".to_string()));
    assert!(style.grid_row_line_names[1].contains(&"y-start".to_string()));
    assert!(style.grid_row_line_names[2].contains(&"y-end".to_string()));
}
