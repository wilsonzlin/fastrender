use std::sync::Arc;

use fastrender::layout::constraints::{AvailableSpace, LayoutConstraints};
use fastrender::layout::contexts::flex::FlexFormattingContext;
use fastrender::layout::formatting_context::FormattingContext;
use fastrender::style::display::{Display, FormattingContextType};
use fastrender::style::types::BoxSizing;
use fastrender::style::values::Length;
use fastrender::style::ComputedStyle;
use fastrender::tree::box_tree::BoxNode;

fn style_with_display(display: Display) -> ComputedStyle {
    let mut style = ComputedStyle::default();
    style.display = display;
    style
}

#[test]
fn flex_item_border_box_width_uses_content_size() {
    let mut container_style = style_with_display(Display::Flex);
    container_style.width = Some(Length::px(400.0));

    let mut child_style = ComputedStyle::default();
    child_style.display = Display::Block;
    child_style.box_sizing = BoxSizing::BorderBox;
    child_style.width = Some(Length::px(200.0));
    child_style.padding_left = Length::px(10.0);
    child_style.padding_right = Length::px(10.0);
    child_style.border_left_width = Length::px(5.0);
    child_style.border_right_width = Length::px(5.0);
    let edge_sum = child_style.padding_left.to_px()
        + child_style.padding_right.to_px()
        + child_style.border_left_width.to_px()
        + child_style.border_right_width.to_px();

    let container = BoxNode::new_block(
        Arc::new(container_style),
        FormattingContextType::Flex,
        vec![BoxNode::new_block(Arc::new(child_style.clone()), FormattingContextType::Block, vec![])],
    );

    let fc = FlexFormattingContext::new();
    let constraints = LayoutConstraints::new(AvailableSpace::Definite(400.0), AvailableSpace::Indefinite);
    let fragment = fc.layout(&container, &constraints).expect("layout should succeed");

    let child = fragment.children.first().expect("child fragment");
    let width = child.bounds.width();
    assert!(
        (width - 200.0).abs() < 0.1,
        "border-box width should be honored (got {width})"
    );
    assert!((child.bounds.height() - 0.0).abs() < 0.1);
    assert!((child.bounds.x() - 0.0).abs() < 0.1);
    assert!((child.bounds.y() - 0.0).abs() < 0.1);

    let content_box_width = child.bounds.width() - edge_sum;

    assert!(
        (content_box_width - 170.0).abs() < 0.1,
        "content box should shrink by padding+borders under border-box"
    );
}
