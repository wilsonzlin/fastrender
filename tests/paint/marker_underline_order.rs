use fastrender::geometry::Rect;
use fastrender::paint::painter::paint_tree;
use fastrender::style::color::Rgba;
use fastrender::style::ComputedStyle;
use fastrender::tree::fragment_tree::{FragmentContent, FragmentNode, FragmentTree};
use std::sync::Arc;

#[test]
fn marker_underline_paints_with_text() {
    let mut style = ComputedStyle::default();
    style.color = Rgba::BLACK;
    style.text_decoration.decoration = Some(fastrender::style::types::TextDecorationLine::UNDERLINE);
    style.text_decoration.color = Some(Rgba::from_rgba8(0, 0, 255, 255));
    style.text_decoration.thickness = fastrender::style::types::TextDecorationThickness::Length(
        fastrender::style::values::Length::px(2.0),
    );
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

    let pixmap = paint_tree(&tree, 50, 50, Rgba::WHITE).expect("paint");

    let underline_bbox = fastrender::tests::display_list_test::bounding_box_for_color(
        &pixmap,
        |(r, g, b, a)| a > 0 && b > r + 20 && b > g + 20,
    )
    .expect("underline");
    let glyph_bbox = fastrender::tests::display_list_test::bounding_box_for_color(&pixmap, |(r, g, b, a)| {
        a > 0 && r < 32 && g < 32 && b < 32
    })
    .expect("glyph");

    assert!(underline_bbox.1 > glyph_bbox.1 + 5, "underline should sit below glyph");
}

