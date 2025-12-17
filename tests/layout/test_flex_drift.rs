use fastrender::layout::constraints::LayoutConstraints;
use fastrender::layout::contexts::flex::FlexFormattingContext;
use fastrender::style::display::Display;
use fastrender::style::values::Length;
use fastrender::{BoxNode, ComputedStyle, FormattingContext, FormattingContextType};
use std::sync::Arc;

fn make_child(id: usize, margin_left: f32) -> BoxNode {
    let mut style = ComputedStyle::default();
    style.display = Display::Block;
    style.width = Some(Length::px(10.0));
    style.height = Some(Length::px(10.0));
    style.margin_left = Some(Length::px(margin_left));
    let mut node = BoxNode::new_block(Arc::new(style), FormattingContextType::Block, vec![]);
    node.id = id;
    node
}

#[test]
fn flex_row_clamps_pathological_inline_drift() {
    let mut parent_style = ComputedStyle::default();
    parent_style.display = Display::Flex;
    parent_style.width = Some(Length::px(100.0));
    parent_style.height = Some(Length::px(20.0));

    let parent = BoxNode::new_block(
        Arc::new(parent_style),
        FormattingContextType::Flex,
        vec![make_child(1, 5000.0), make_child(2, 5000.0)],
    );

    let fc = FlexFormattingContext::new();
    let fragment = fc
        .layout(&parent, &LayoutConstraints::definite(100.0, 50.0))
        .expect("layout succeeds");

    let positions: Vec<f32> = fragment.children.iter().map(|c| c.bounds.x()).collect();
    assert!(positions.iter().all(|&x| x.abs() < 1.0), "children should be translated back toward the origin: {:?}", positions);
}
