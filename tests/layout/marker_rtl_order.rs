use std::sync::Arc;

use fastrender::layout::constraints::LayoutConstraints;
use fastrender::layout::contexts::inline::InlineFormattingContext;
use fastrender::style::display::FormattingContextType;
use fastrender::style::types::Direction;
use fastrender::style::ComputedStyle;
use fastrender::tree::box_tree::{BoxNode, MarkerContent};
use fastrender::tree::fragment_tree::FragmentContent;

fn marker_and_text_positions(fragment: &fastrender::tree::fragment_tree::FragmentNode) -> (Option<f32>, Option<f32>) {
    let mut marker_x = None;
    let mut text_x = None;
    let mut stack = vec![fragment];
    while let Some(node) = stack.pop() {
        match node.content {
            FragmentContent::Text { is_marker, .. } => {
                if is_marker && marker_x.is_none() {
                    marker_x = Some(node.bounds.x());
                } else if !is_marker && text_x.is_none() {
                    text_x = Some(node.bounds.x());
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
    (marker_x, text_x)
}

#[test]
fn rtl_outside_marker_stays_inline_start() {
    let ifc = InlineFormattingContext::new();

    let mut marker_style = ComputedStyle::default();
    marker_style.direction = Direction::Rtl;
    let marker = BoxNode::new_marker(Arc::new(marker_style), MarkerContent::Text("•".to_string()));

    let mut text_style = ComputedStyle::default();
    text_style.direction = Direction::Rtl;
    let text = BoxNode::new_text(Arc::new(text_style), "שלום".to_string());

    let mut root_style = ComputedStyle::default();
    root_style.direction = Direction::Rtl;
    let root = BoxNode::new_block(
        Arc::new(root_style),
        FormattingContextType::Block,
        vec![marker, text],
    );
    let constraints = LayoutConstraints::definite(200.0, 100.0);
    let fragment = ifc.layout(&root, &constraints).expect("layout");
    let line = fragment.children.first().expect("line fragment");

    let (marker_x, text_x) = marker_and_text_positions(line);
    let marker_x = marker_x.expect("marker x");
    let text_x = text_x.expect("text x");

    assert!(marker_x > text_x + 5.0, "RTL outside marker should sit on inline-start (right)");
}

