use fastrender::layout::constraints::LayoutConstraints;
use fastrender::layout::contexts::flex::FlexFormattingContext;
use fastrender::style::display::Display;
use fastrender::style::types::JustifyContent;
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

#[test]
fn flex_wrap_reflows_far_inline_offsets() {
    let mut parent_style = ComputedStyle::default();
    parent_style.display = Display::Flex;
    parent_style.flex_wrap = fastrender::style::types::FlexWrap::Wrap;
    parent_style.width = Some(Length::px(50.0));
    parent_style.height = Some(Length::px(40.0));

    // Two items with huge inline offsets should be reflowed to start at the row origin when wrapping.
    let parent = BoxNode::new_block(
        Arc::new(parent_style),
        FormattingContextType::Flex,
        vec![make_child(1, 4000.0), make_child(2, 4000.0)],
    );

    let fc = FlexFormattingContext::new();
    let fragment = fc
        .layout(&parent, &LayoutConstraints::definite(50.0, 40.0))
        .expect("layout succeeds");

    let xs: Vec<f32> = fragment.children.iter().map(|c| c.bounds.x()).collect();
    assert!(xs.iter().all(|&x| x >= -0.5 && x <= 50.0), "wrapped items should start within the container: {:?}", xs);
    assert!(xs[0] <= 1.0, "first item should start at row origin: {:?}", xs);
}

#[test]
fn nested_flex_children_use_local_origins() {
    // Center a flex item inside a larger flex container, then ensure the grandchildren start at
    // the parent's origin instead of inheriting the ancestor's offset.
    let mut outer_style = ComputedStyle::default();
    outer_style.display = Display::Flex;
    outer_style.width = Some(Length::px(500.0));
    outer_style.height = Some(Length::px(200.0));
    outer_style.justify_content = JustifyContent::Center;

    let mut mid_style = ComputedStyle::default();
    mid_style.display = Display::Flex;
    mid_style.width = Some(Length::px(300.0));
    mid_style.height = Some(Length::px(150.0));
    // Default justify-content: flex-start keeps children at x=0 within the mid container.

    let mut inner_style = ComputedStyle::default();
    inner_style.display = Display::Block;
    inner_style.width = Some(Length::px(100.0));
    inner_style.height = Some(Length::px(50.0));

    let mut inner = BoxNode::new_block(Arc::new(inner_style), FormattingContextType::Block, vec![]);
    inner.id = 3;

    let mut mid = BoxNode::new_block(Arc::new(mid_style), FormattingContextType::Flex, vec![inner]);
    mid.id = 2;

    let mut outer = BoxNode::new_block(Arc::new(outer_style), FormattingContextType::Flex, vec![mid]);
    outer.id = 1;

    let fc = FlexFormattingContext::new();
    let fragment = fc
        .layout(&outer, &LayoutConstraints::definite(500.0, 200.0))
        .expect("layout succeeds");

    let mid_frag = &fragment.children[0];
    let inner_frag = &mid_frag.children[0];
    assert!(mid_frag.bounds.x() > 90.0 && mid_frag.bounds.x() < 110.0);
    assert!(inner_frag.bounds.x().abs() < 1e-3, "grandchild should start at the mid container's origin: {}", inner_frag.bounds.x());
}
