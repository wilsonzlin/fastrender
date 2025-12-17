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
