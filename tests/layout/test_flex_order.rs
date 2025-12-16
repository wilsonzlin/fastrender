use fastrender::layout::contexts::flex::FlexFormattingContext;
use fastrender::layout::formatting_context::LayoutConstraints;
use fastrender::style::display::Display;
use fastrender::style::values::Length;
use fastrender::ComputedStyle;
use fastrender::BoxNode;
use fastrender::FormattingContextType;
use std::sync::Arc;

fn flex_child(order: i32, width: f32, height: f32) -> BoxNode {
    let mut style = ComputedStyle::default();
    style.display = Display::Block;
    style.width = Some(Length::px(width));
    style.height = Some(Length::px(height));
    style.flex_shrink = 0.0;
    style.order = order;
    BoxNode::new_block(Arc::new(style), FormattingContextType::Block, vec![])
}

#[test]
fn flex_items_follow_order_property() {
    let mut parent_style = ComputedStyle::default();
    parent_style.display = Display::Flex;
    parent_style.width = Some(Length::px(300.0));
    parent_style.height = Some(Length::px(50.0));
    let parent = BoxNode::new_block(
        Arc::new(parent_style),
        FormattingContextType::Flex,
        vec![
            flex_child(2, 80.0, 20.0),
            flex_child(-1, 40.0, 20.0),
            flex_child(1, 60.0, 20.0),
        ],
    );

    let fc = FlexFormattingContext::new();
    let fragment = fc
        .layout(&parent, &LayoutConstraints::definite(300.0, 200.0))
        .expect("layout succeeds");

    let positions: Vec<(f32, f32)> = fragment
        .children
        .iter()
        .map(|child| (child.bounds.x(), child.bounds.width()))
        .collect();

    // Order: -1 should come first, then 1, then 2 (DOM order is preserved only within equal orders).
    assert!((positions[0].0 - 0.0).abs() < 1e-3);
    assert!((positions[0].1 - 40.0).abs() < 1e-3);
    assert!((positions[1].0 - 40.0).abs() < 1e-3);
    assert!((positions[1].1 - 60.0).abs() < 1e-3);
    assert!((positions[2].0 - 100.0).abs() < 1e-3);
    assert!((positions[2].1 - 80.0).abs() < 1e-3);
}

