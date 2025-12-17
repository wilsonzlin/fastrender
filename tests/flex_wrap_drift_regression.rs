use fastrender::layout::constraints::LayoutConstraints;
use fastrender::layout::contexts::flex::FlexFormattingContext;
use fastrender::style::display::Display;
use fastrender::style::types::FlexWrap;
use fastrender::style::values::Length;
use fastrender::{BoxNode, ComputedStyle, FormattingContext, FormattingContextType};
use std::sync::Arc;

fn child(width: f32, margin_left: f32) -> BoxNode {
    let mut style = ComputedStyle::default();
    style.display = Display::Block;
    style.width = Some(Length::px(width));
    style.height = Some(Length::px(10.0));
    style.margin_left = Some(Length::px(margin_left));
    BoxNode::new_block(Arc::new(style), FormattingContextType::Block, vec![])
}

#[test]
fn wrapped_row_repositions_offscreen_items() {
    let mut parent_style = ComputedStyle::default();
    parent_style.display = Display::Flex;
    parent_style.flex_wrap = FlexWrap::Wrap;
    parent_style.width = Some(Length::px(60.0));
    parent_style.height = Some(Length::px(40.0));

    let parent = BoxNode::new_block(
        Arc::new(parent_style),
        FormattingContextType::Flex,
        vec![child(20.0, 2000.0), child(20.0, 2000.0), child(20.0, 2000.0)],
    );

    let fc = FlexFormattingContext::new();
    let fragment = fc
        .layout(&parent, &LayoutConstraints::definite(60.0, 40.0))
        .expect("layout succeeds");

    for (idx, child) in fragment.children.iter().enumerate() {
        assert!(child.bounds.x() >= -0.5 && child.bounds.x() <= 60.0,
            "child {} should be within container after wrap clamp: x={}", idx, child.bounds.x());
    }
    assert!(fragment.children[0].bounds.x() <= 1.0, "first item should start near origin");
}
