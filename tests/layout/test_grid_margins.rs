use fastrender::layout::constraints::LayoutConstraints;
use fastrender::layout::contexts::grid::GridFormattingContext;
use fastrender::style::display::Display;
use fastrender::style::types::GridTrack;
use fastrender::style::values::Length;
use fastrender::{BoxNode, ComputedStyle, FormattingContext, FormattingContextType};
use std::sync::Arc;

fn grid_container(children: Vec<BoxNode>) -> BoxNode {
    let mut style = ComputedStyle::default();
    style.display = Display::Grid;
    style.width = Some(Length::px(200.0));
    style.height = Some(Length::px(100.0));
    style.grid_template_columns = vec![GridTrack::Fr(1.0)];

    BoxNode::new_block(Arc::new(style), FormattingContextType::Grid, children)
}

fn grid_container_with_padding(children: Vec<BoxNode>, padding_top: f32, padding_bottom: f32) -> BoxNode {
    let mut style = ComputedStyle::default();
    style.display = Display::Grid;
    style.width = Some(Length::px(200.0));
    style.height = Some(Length::px(100.0));
    style.grid_template_columns = vec![GridTrack::Fr(1.0)];
    style.padding_top = Length::px(padding_top);
    style.padding_bottom = Length::px(padding_bottom);

    BoxNode::new_block(Arc::new(style), FormattingContextType::Grid, children)
}

fn grid_item() -> BoxNode {
    let mut style = ComputedStyle::default();
    style.display = Display::Block;
    style.width = Some(Length::px(50.0));
    style.height = Some(Length::px(10.0));
    // Auto inline margins should absorb remaining inline space and center the item.
    style.margin_left = None;
    style.margin_right = None;

    BoxNode::new_block(Arc::new(style), FormattingContextType::Block, vec![])
}

fn grid_item_with_block_margins() -> BoxNode {
    let mut style = ComputedStyle::default();
    style.display = Display::Block;
    style.width = Some(Length::px(50.0));
    style.height = Some(Length::px(20.0));
    // Auto block margins should absorb remaining block space and center the item vertically.
    style.margin_top = None;
    style.margin_bottom = None;

    BoxNode::new_block(Arc::new(style), FormattingContextType::Block, vec![])
}

#[test]
fn grid_item_auto_margins_center_item() {
    let container = grid_container(vec![grid_item()]);
    let fc = GridFormattingContext::new();

    let fragment = fc
        .layout(&container, &LayoutConstraints::definite(200.0, 100.0))
        .expect("layout succeeds");

    assert_eq!(fragment.children.len(), 1);
    let child = &fragment.children[0];

    // With a 200px container, 50px item, and auto inline margins, the item should be centered.
    assert!((child.bounds.x() - 75.0).abs() <= 0.5, "expected centered item, got x={}" , child.bounds.x());
    assert_eq!(child.bounds.width(), 50.0);
}

#[test]
fn grid_item_auto_block_margins_center_vertically() {
    let container = grid_container_with_padding(vec![grid_item_with_block_margins()], 10.0, 10.0);

    let fc = GridFormattingContext::new();
    let fragment = fc
        .layout(&container, &LayoutConstraints::definite(200.0, 100.0))
        .expect("layout succeeds");

    let child = &fragment.children[0];
    // Content height is 100 (content-box) with 10px padding on each side, so the padding pushes the child down.
    // With a 20px tall item and auto block margins, it should center in the 100px content and start around y=50.
    assert!((child.bounds.y() - 50.0).abs() <= 0.5, "expected vertically centered item, got y={}", child.bounds.y());
    assert_eq!(child.bounds.height(), 20.0);
}
