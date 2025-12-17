use fastrender::dom::DomNode;
use fastrender::geometry::Size;
use fastrender::style::cascade::CascadeContext;
use fastrender::style::computed::ComputedStyle;
use fastrender::style::types::{Display, ListStylePosition, PseudoElement};

fn build_list_item() -> DomNode {
    let mut li = DomNode::new("li");
    li
}

#[test]
fn marker_disallows_box_and_background_properties() {
    let mut ctx = CascadeContext::default();
    let mut li_style = ComputedStyle::default();
    li_style.display = Display::ListItem;
    li_style.list_style_position = ListStylePosition::Inside;

    // Apply forbidden properties to the marker via authored styles
    let mut style_rules = String::from("li::marker { background: red; padding: 10px; width: 50px; height: 40px; }");
    ctx.add_author_stylesheet(&style_rules).unwrap();

    let li = build_list_item();
    let viewport = Size::new(800.0, 600.0);
    let (style, marker) = ctx.compute_styles_for_node(&li, viewport);
    let marker_style = marker.expect("marker styles");

    // Marker should still be inline and ignore box/background overrides
    assert_eq!(marker_style.display, Display::Inline);
    assert_eq!(marker_style.padding_top.to_px(), 0.0);
    assert_eq!(marker_style.padding_left.to_px(), 0.0);
    assert_eq!(marker_style.width, None);
    assert_eq!(marker_style.height, None);
    assert_eq!(marker_style.background_color, ComputedStyle::default().background_color);
    assert!(marker_style.background_images.is_empty());

    // The parent list item should remain a list item
    assert_eq!(style.display, Display::ListItem);
}

#[test]
fn marker_allows_text_affecting_properties() {
    let mut ctx = CascadeContext::default();
    let mut li_style = ComputedStyle::default();
    li_style.display = Display::ListItem;

    let rules = "li::marker { color: rgb(10,20,30); font-size: 24px; text-transform: uppercase; }";
    ctx.add_author_stylesheet(rules).unwrap();

    let li = DomNode::new("li");
    let viewport = Size::new(800.0, 600.0);
    let (_style, marker) = ctx.compute_styles_for_node(&li, viewport);
    let marker_style = marker.expect("marker styles");

    assert_eq!(marker_style.color, fastrender::Rgba::rgb(10, 20, 30));
    assert_eq!(marker_style.font_size, 24.0);
    assert_eq!(marker_style.text_transform, fastrender::style::types::TextTransform::Uppercase);
}
