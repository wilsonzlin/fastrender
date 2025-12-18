use fastrender::geometry::Rect;
use fastrender::paint::display_list::{DisplayItem, ListMarkerItem};
use fastrender::paint::display_list_renderer::DisplayListRenderer;
use fastrender::style::color::Rgba;
use fastrender::style::ComputedStyle;
use fastrender::tree::fragment_tree::{FragmentContent, FragmentNode, FragmentTree};
use std::sync::Arc;

#[test]
fn marker_paints_after_background() {
    // Build a marker with background and text color to ensure paint order keeps text above background.
    let mut style = ComputedStyle::default();
    style.color = Rgba::WHITE;
    style.background_color = Rgba::BLACK;
    let style = Arc::new(style);

    let marker = FragmentNode::new_with_style(
        Rect::from_xywh(0.0, 0.0, 20.0, 20.0),
        FragmentContent::Text {
            text: "•".to_string(),
            box_id: None,
            baseline_offset: 16.0,
            shaped: None,
            is_marker: true,
        },
        vec![],
        style,
    );
    let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 40.0, 30.0), vec![marker]);
    let tree = FragmentTree::new(root);

    let (list, _glyph_cache, _images) = DisplayListRenderer::build_display_list(&tree, 40, 30).expect("list");
    let mut saw_marker = false;
    for item in &list.items {
        match item {
            DisplayItem::ListMarker(ListMarkerItem { color, background, .. }) => {
                saw_marker = true;
                assert_eq!(*background, Some(Rgba::BLACK), "marker background should be present");
                assert_eq!(*color, Rgba::WHITE, "marker text should paint over background");
            }
            _ => {}
        }
    }
    assert!(saw_marker, "marker display item should be emitted");
}

