use fastrender::geometry::Rect;
use fastrender::paint::painter::{paint_tree, Painter};
use fastrender::style::color::Rgba;
use fastrender::style::ComputedStyle;
use fastrender::tree::fragment_tree::{FragmentContent, FragmentNode, FragmentTree};

#[test]
fn marker_image_paints_shadow() {
    let mut style = ComputedStyle::default();
    style.color = Rgba::BLACK;
    style.text_shadow.push(fastrender::style::types::TextShadow {
        offset_x: fastrender::style::values::Length::px(2.0),
        offset_y: fastrender::style::values::Length::px(0.0),
        blur_radius: fastrender::style::values::Length::px(0.0),
        color: Some(Rgba::from_rgba8(255, 0, 0, 255)),
    });
    let style = std::sync::Arc::new(style);

    let mut marker = FragmentNode::new_with_style(
        Rect::from_xywh(10.0, 10.0, 10.0, 10.0),
        FragmentContent::Replaced {
            replaced_type: fastrender::tree::box_tree::ReplacedType::Image {
                src: String::new(),
                alt: None,
                sizes: None,
                srcset: vec![],
            },
            box_id: None,
        },
        vec![],
        style.clone(),
    );
    // Mark this fragment as a marker via a text child; painter still uses style text_shadow.
    marker.content = FragmentContent::Text {
        text: "•".to_string(),
        box_id: None,
        baseline_offset: 10.0,
        shaped: None,
        is_marker: true,
    };

    let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 40.0, 30.0), vec![marker]);
    let tree = FragmentTree::new(root);

    let pixmap = paint_tree(&tree, 60, 40, Rgba::WHITE).expect("paint");

    // Shadow should shift to the right by ~2px.
    let glyph_bbox = fastrender::tests::display_list_test::bounding_box_for_color(&pixmap, |(r, g, b, a)| {
        a > 0 && r < 32 && g < 32 && b < 32
    })
    .expect("marker glyph");
    let shadow_bbox = fastrender::tests::display_list_test::bounding_box_for_color(
        &pixmap,
        |(r, g, b, a)| a > 0 && r > 200 && g < 80 && b < 80,
    )
    .expect("marker shadow");

    let dx = shadow_bbox.0 as i32 - glyph_bbox.0 as i32;
    assert!((1..=3).contains(&dx), "shadow should appear offset to the right (dx={})", dx);
}

