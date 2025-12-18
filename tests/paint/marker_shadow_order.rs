use fastrender::geometry::Rect;
use fastrender::paint::display_list_renderer::DisplayListRenderer;
use fastrender::style::color::Rgba;
use fastrender::style::ComputedStyle;
use fastrender::tree::fragment_tree::{FragmentContent, FragmentNode, FragmentTree};
use std::sync::Arc;

#[test]
fn marker_shadow_paints_after_background() {
    let mut style = ComputedStyle::default();
    style.color = Rgba::BLACK;
    style.background_color = Rgba::WHITE;
    style.text_shadow.push(fastrender::style::types::TextShadow {
        offset_x: fastrender::style::values::Length::px(2.0),
        offset_y: fastrender::style::values::Length::px(0.0),
        blur_radius: fastrender::style::values::Length::px(0.0),
        color: Some(Rgba::from_rgba8(255, 0, 0, 255)),
    });
    let style = Arc::new(style);

    let marker = FragmentNode::new_with_style(
        Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
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
    let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 20.0, 20.0), vec![marker]);
    let tree = FragmentTree::new(root);

    let (list, ..) = DisplayListRenderer::build_display_list(&tree, 20, 20).expect("display list");

    let mut shadow_seen = false;
    let mut background_seen = false;
    for item in &list.items {
        match item {
            fastrender::paint::display_list::DisplayItem::Shadow(_shadow) => {
                shadow_seen = true;
                assert!(background_seen, "marker background should precede shadow");
            }
            fastrender::paint::display_list::DisplayItem::ListMarker(marker) => {
                background_seen = marker.background.is_some();
            }
            _ => {}
        }
    }

    assert!(shadow_seen, "marker shadow should be emitted");
}

