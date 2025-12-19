use fastrender::css::types::TextShadow;
use fastrender::geometry::Rect;
use fastrender::paint::display_list::{DisplayItem, ListMarkerItem};
use fastrender::paint::display_list_builder::DisplayListBuilder;
use fastrender::style::color::Rgba;
use fastrender::style::ComputedStyle;
use fastrender::tree::fragment_tree::{FragmentContent, FragmentNode, FragmentTree};
use std::sync::Arc;

#[test]
fn marker_shadow_respects_opacity() {
    let mut style = ComputedStyle::default();
    style.color = Rgba::BLACK;
    style.text_shadow.push(TextShadow {
        offset_x: fastrender::style::values::Length::px(2.0),
        offset_y: fastrender::style::values::Length::px(0.0),
        blur_radius: fastrender::style::values::Length::px(0.0),
        color: Some(Rgba::from_rgba8(255, 0, 0, 255)),
    });
    style.opacity = 0.5;
    let style = Arc::new(style);

    let marker = FragmentNode::new_with_style(
        Rect::from_xywh(10.0, 10.0, 10.0, 10.0),
        FragmentContent::Text {
            text: "•".to_string(),
            box_id: None,
            baseline_offset: 10.0,
            shaped: None,
            is_marker: true,
        },
        vec![],
        style,
    );
    let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 30.0, 30.0), vec![marker]);
    let tree = FragmentTree::new(root);

    let list = DisplayListBuilder::new().build_tree(&tree);
    let mut marker: Option<&ListMarkerItem> = None;
    for item in list.items() {
        if let DisplayItem::ListMarker(m) = item {
            marker = Some(m);
        }
    }

    let marker = marker.expect("marker display item");
    assert!(
        !marker.shadows.is_empty(),
        "marker should carry text shadows even when opacity is set"
    );
}
