use fastrender::css::parser::parse_stylesheet;
use fastrender::dom;
use fastrender::style::cascade::{apply_styles_with_media, StyledNode};
use fastrender::style::media::MediaContext;
use fastrender::style::types::{CaseTransform, OutlineColor, TextTransform};
use fastrender::{ComputedStyle, Display};

fn find_first<'a>(node: &'a StyledNode, tag: &str) -> Option<&'a StyledNode> {
    if node.node.tag_name() == Some(tag) {
        return Some(node);
    }
    for child in &node.children {
        if let Some(found) = find_first(child, tag) {
            return Some(found);
        }
    }
    None
}

fn styled_list_item(css: &str) -> (StyledNode, ComputedStyle) {
    let dom = dom::parse_html("<ul><li>item</li></ul>").unwrap();
    let stylesheet = parse_stylesheet(css).unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));
    let li = find_first(&styled, "li").expect("li");
    let marker = li.marker_styles.as_ref().expect("marker styles").clone();
    (li.clone(), marker)
}

#[test]
fn marker_disallows_box_and_background_properties() {
    let css = r#"
        li::marker {
            background: red;
            padding: 10px;
            margin: 5px 7px;
            width: 50px;
            height: 40px;
            border: 2px solid blue;
            background-position: center;
            background-size: 10px 10px;
            background-repeat: repeat-x;
            background-clip: content-box;
            border-radius: 4px;
            box-shadow: 0 0 5px black;
            padding-left: 12px;
            padding-top: 8px;
            padding-bottom: 6px;
            display: block;
        }
    "#;

    let (li, marker) = styled_list_item(css);

    // Marker should still be inline and ignore box/background overrides
    assert_eq!(marker.display, Display::Inline);
    assert_eq!(marker.padding_top.to_px(), 0.0);
    assert_eq!(marker.padding_left.to_px(), 0.0);
    assert_eq!(marker.width, None);
    assert_eq!(marker.height, None);
    assert_eq!(marker.background_color, ComputedStyle::default().background_color);
    assert!(marker.background_images.iter().all(|img| img.is_none()));
    assert!(marker.margin_top.map_or(true, |m| m.is_zero()));
    assert!(marker.margin_left.map_or(true, |m| m.is_zero()));
    assert!(marker.border_top_width.is_zero());
    assert!(marker.border_left_width.is_zero());
    assert_eq!(marker.box_shadow.len(), 0);

    // The parent list item should remain a list item
    assert_eq!(li.styles.list_style_type, ComputedStyle::default().list_style_type);
}

#[test]
fn marker_allows_text_affecting_properties() {
    let css = "li::marker { color: rgb(10,20,30); font-size: 24px; text-transform: uppercase; text-shadow: 1px 1px red; letter-spacing: 2px; }";
    let (_li, marker) = styled_list_item(css);

    assert_eq!(marker.color, fastrender::Rgba::rgb(10, 20, 30));
    assert_eq!(marker.font_size, 24.0);
    assert_eq!(
        marker.text_transform,
        TextTransform::with_case(CaseTransform::Uppercase)
    );
    assert_eq!(marker.text_shadow.len(), 1);
    assert!((marker.letter_spacing - 2.0).abs() < f32::EPSILON);
}

#[test]
fn marker_ignores_outline_properties() {
    let css =
        "li { outline: 2px solid rgb(1,2,3); } li::marker { outline: 4px dotted rgb(4,5,6); outline-offset: 8px; }";
    let (_li, marker) = styled_list_item(css);

    // Outline on markers should be cleared to defaults and not inherit from the list item.
    assert!(matches!(
        marker.outline_style,
        fastrender::style::types::OutlineStyle::None
    ));
    assert_eq!(marker.outline_width, ComputedStyle::default().outline_width);
    assert!(matches!(marker.outline_color, OutlineColor::Invert));
    assert_eq!(marker.outline_offset, ComputedStyle::default().outline_offset);
}

#[test]
fn marker_ignores_opacity_and_transform() {
    let css = "li::marker { opacity: 0.2; transform: rotate(45deg); }";
    let (_li, marker) = styled_list_item(css);

    assert_eq!(marker.opacity, 1.0, "marker opacity should remain at the default");
    assert!(marker.transform.is_empty(), "marker transform should be cleared");
}

#[test]
fn marker_ignores_vertical_align() {
    let css = "li::marker { vertical-align: middle; }";
    let (_li, marker) = styled_list_item(css);

    assert!(matches!(
        marker.vertical_align,
        fastrender::style::types::VerticalAlign::Baseline
    ));
}
