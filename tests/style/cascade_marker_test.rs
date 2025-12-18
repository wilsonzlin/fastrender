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
    let style_rules = String::from(
        "li::marker { background: red; padding: 10px; margin: 5px 7px; width: 50px; height: 40px; border: 2px solid blue; }
         li::marker { background-position: center; background-size: 10px 10px; }
         li::marker { background-repeat: repeat-x; background-clip: content-box; }
         li::marker { border-radius: 4px; box-shadow: 0 0 5px black; }
         li::marker { padding-left: 12px; padding-top: 8px; padding-bottom: 6px; }
         li::marker { display: block; }
    ",
    );
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
    assert!(marker_style.margin_top.is_none());
    assert!(marker_style.margin_left.is_none());
    assert!(marker_style.border_top_width.is_zero());
    assert!(marker_style.border_left_width.is_zero());
    assert_eq!(marker_style.box_shadow.len(), 0);
    assert_eq!(marker_style.background_repeat, ComputedStyle::default().background_repeat);
    assert_eq!(marker_style.background_clip, ComputedStyle::default().background_clip);
    assert_eq!(marker_style.background_origin, ComputedStyle::default().background_origin);

    // The parent list item should remain a list item
    assert_eq!(style.display, Display::ListItem);
}

#[test]
fn marker_allows_text_affecting_properties() {
    let mut ctx = CascadeContext::default();
    let mut li_style = ComputedStyle::default();
    li_style.display = Display::ListItem;
    li_style.list_style_position = ListStylePosition::Inside;

    let rules = "li::marker { color: rgb(10,20,30); font-size: 24px; text-transform: uppercase; text-shadow: 1px 1px red; letter-spacing: 2px; }";
    ctx.add_author_stylesheet(rules).unwrap();

    let li = DomNode::new("li");
    let viewport = Size::new(800.0, 600.0);
    let (_style, marker) = ctx.compute_styles_for_node(&li, viewport);
    let marker_style = marker.expect("marker styles");

    assert_eq!(marker_style.color, fastrender::Rgba::rgb(10, 20, 30));
    assert_eq!(marker_style.font_size, 24.0);
    assert_eq!(marker_style.text_transform, fastrender::style::types::TextTransform::Uppercase);
}

#[test]
fn marker_ignores_outline_properties() {
    let mut ctx = CascadeContext::default();
    let mut li_style = ComputedStyle::default();
    li_style.display = Display::ListItem;

    let rules = "li { outline: 2px solid rgb(1,2,3); } li::marker { outline: 4px dotted rgb(4,5,6); outline-offset: 8px; }";
    ctx.add_author_stylesheet(rules).unwrap();

    let li = build_list_item();
    let viewport = Size::new(800.0, 600.0);
    let (_style, marker) = ctx.compute_styles_for_node(&li, viewport);
    let marker_style = marker.expect("marker styles");

    // Outline on markers should be cleared to defaults and not inherit from the list item.
    assert!(matches!(marker_style.outline_style, fastrender::style::types::OutlineStyle::None));
    assert_eq!(marker_style.outline_width, ComputedStyle::default().outline_width);
    assert!(matches!(marker_style.outline_color, fastrender::style::types::OutlineColor::Invert));
    assert_eq!(marker_style.outline_offset, ComputedStyle::default().outline_offset);
}

#[test]
fn marker_ignores_opacity_and_transform() {
    let mut ctx = CascadeContext::default();
    let mut li_style = ComputedStyle::default();
    li_style.display = Display::ListItem;

    let rules = "li::marker { opacity: 0.2; transform: rotate(45deg); }";
    ctx.add_author_stylesheet(rules).unwrap();

    let li = build_list_item();
    let viewport = Size::new(800.0, 600.0);
    let (_style, marker) = ctx.compute_styles_for_node(&li, viewport);
    let marker_style = marker.expect("marker styles");

    assert_eq!(marker_style.opacity, 1.0, "marker opacity should remain at the default");
    assert!(marker_style.transform.is_empty(), "marker transform should be cleared");
}

#[test]
fn marker_ignores_vertical_align() {
    let mut ctx = CascadeContext::default();
    let mut li_style = ComputedStyle::default();
    li_style.display = Display::ListItem;

    let rules = "li::marker { vertical-align: middle; }";
    ctx.add_author_stylesheet(rules).unwrap();

    let li = build_list_item();
    let viewport = Size::new(800.0, 600.0);
    let (_style, marker) = ctx.compute_styles_for_node(&li, viewport);
    let marker_style = marker.expect("marker styles");

    assert!(matches!(marker_style.vertical_align, fastrender::style::types::VerticalAlign::Baseline));
}
