use std::sync::Arc;

use fastrender::layout::constraints::LayoutConstraints;
use fastrender::layout::contexts::inline::InlineFormattingContext;
use fastrender::style::display::FormattingContextType;
use fastrender::style::values::Length;
use fastrender::style::ComputedStyle;
use fastrender::tree::box_tree::{BoxNode, MarkerContent};
use fastrender::tree::fragment_tree::FragmentContent;

#[test]
fn marker_negative_margin_does_not_overlap_text() {
    let ifc = InlineFormattingContext::new();

    let mut marker_style = ComputedStyle::default();
    marker_style.margin_right = Some(Length::px(-5.0));
    let marker = BoxNode::new_marker(Arc::new(marker_style), MarkerContent::Text("•".to_string()));

    let text = BoxNode::new_text(Arc::new(ComputedStyle::default()), "content".to_string());

    let root = BoxNode::new_block(
        Arc::new(ComputedStyle::default()),
        FormattingContextType::Block,
        vec![marker, text],
    );

    let constraints = LayoutConstraints::definite(200.0, 100.0);
    let fragment = ifc.layout(&root, &constraints).expect("layout");
    let line = fragment.children.first().expect("line fragment");

    let mut marker_x = None;
    let mut text_x = None;
    let mut stack = vec![line];
    while let Some(node) = stack.pop() {
        match node.content {
            FragmentContent::Text { is_marker, .. } => {
                if is_marker {
                    marker_x.get_or_insert(node.bounds.x());
                } else {
                    text_x.get_or_insert(node.bounds.x());
                }
            }
            FragmentContent::Replaced { .. } => {
                marker_x.get_or_insert(node.bounds.x());
            }
            _ => {}
        }
        for child in &node.children {
            stack.push(child);
        }
    }

    let marker_x = marker_x.expect("marker x");
    let text_x = text_x.expect("text x");

    assert!(text_x > marker_x, "text should still start after marker even with negative margin");
}

