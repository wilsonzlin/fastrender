use std::sync::Arc;

use fastrender::geometry::Size;
use fastrender::layout::constraints::LayoutConstraints;
use fastrender::layout::contexts::inline::InlineFormattingContext;
use fastrender::style::display::FormattingContextType;
use fastrender::style::values::Length;
use fastrender::style::ComputedStyle;
use fastrender::tree::box_tree::{BoxNode, MarkerContent};
use fastrender::tree::fragment_tree::FragmentContent;

#[test]
fn marker_background_does_not_overlap_text() {
    let ifc = InlineFormattingContext::new();

    let mut marker_style = ComputedStyle::default();
    marker_style.background_color = fastrender::style::color::Rgba::BLACK;
    marker_style.padding_left = Length::px(6.0);
    let marker = BoxNode::new_marker(Arc::new(marker_style), MarkerContent::Text("•".to_string()));

    let mut text_style = ComputedStyle::default();
    text_style.color = fastrender::style::color::Rgba::WHITE;
    let text = BoxNode::new_text(Arc::new(text_style), "text".to_string());

    let root = BoxNode::new_block(
        Arc::new(ComputedStyle::default()),
        FormattingContextType::Block,
        vec![marker, text],
    );
    let constraints = LayoutConstraints::definite_size(Size::new(200.0, 100.0));

    let fragment = ifc.layout(&root, &constraints).expect("layout");
    let line = fragment.children.first().expect("line fragment");

    let mut marker_bounds = None;
    let mut text_bounds = None;
    let mut stack = vec![line];
    while let Some(node) = stack.pop() {
        match node.content {
            FragmentContent::Text { is_marker, .. } => {
                if is_marker {
                    marker_bounds = Some(node.bounds);
                } else {
                    text_bounds = Some(node.bounds);
                }
            }
            FragmentContent::Replaced { .. } => {
                marker_bounds = Some(node.bounds);
            }
            _ => {}
        }
        for child in &node.children {
            stack.push(child);
        }
    }

    let marker_bounds = marker_bounds.expect("marker bounds");
    let text_bounds = text_bounds.expect("text bounds");

    assert!(
        marker_bounds.max_x() <= text_bounds.x() - 0.1,
        "marker background should end before text starts"
    );
}

