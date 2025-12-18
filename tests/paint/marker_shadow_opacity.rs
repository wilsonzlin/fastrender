use fastrender::geometry::Rect;
use fastrender::paint::painter::paint_tree;
use fastrender::style::color::Rgba;
use fastrender::style::ComputedStyle;
use fastrender::tree::fragment_tree::{FragmentContent, FragmentNode, FragmentTree};
use std::sync::Arc;

#[test]
fn marker_shadow_respects_opacity() {
    let mut style = ComputedStyle::default();
    style.color = Rgba::BLACK;
    style.text_shadow.push(fastrender::style::types::TextShadow {
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

    let pixmap = paint_tree(&tree, 50, 50, Rgba::WHITE).expect("paint");

    // Count opaque red shadow pixels and ensure they are half-transparent (alpha around 128)
    let mut min_alpha = 255u8;
    let mut max_alpha = 0u8;
    let mut red_seen = false;
    for y in 0..pixmap.height() {
        for x in 0..pixmap.width() {
            let pixel = pixmap.pixel(x as i32, y as i32).unwrap();
            let (r, g, b, a) = (pixel.red(), pixel.green(), pixel.blue(), pixel.alpha());
            if r > 200 && g < 50 && b < 50 {
                red_seen = true;
                min_alpha = min_alpha.min(a);
                max_alpha = max_alpha.max(a);
            }
        }
    }

    assert!(red_seen, "shadow pixels should be present");
    assert!(min_alpha < 200, "shadow alpha should be reduced by opacity");
    assert!(max_alpha <= 160, "shadow alpha should be roughly half (max_alpha={})", max_alpha);
}

