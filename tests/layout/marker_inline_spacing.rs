use std::sync::Arc;

use fastrender::geometry::Size;
use fastrender::layout::constraints::LayoutConstraints;
use fastrender::layout::contexts::inline::InlineFormattingContext;
use fastrender::style::display::FormattingContextType;
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
fn marker_inline_spacing_respects_gap() {
    let ifc = InlineFormattingContext::new();

    let mut marker_style = ComputedStyle::default();
    marker_style.margin_right = Some(fastrender::style::values::Length::px(12.0));
    let marker_style = Arc::new(marker_style);

    let text_style = Arc::new(ComputedStyle::default());

    let marker = BoxNode::new_marker(marker_style, MarkerContent::Text("•".to_string()));
    let text = BoxNode::new_text(text_style, "content".to_string());
    let root = BoxNode::new_block(
        Arc::new(ComputedStyle::default()),
        FormattingContextType::Block,
        vec![marker, text],
    );
    let constraints = LayoutConstraints::definite(200.0, 200.0);

    let fragment = ifc.layout(&root, &constraints).unwrap();
    let line = fragment.children.first().expect("line fragment");
    let (marker_x, text_x) = marker_and_text_positions(line);
    let marker_x = marker_x.expect("marker x");
    let text_x = text_x.expect("text x");

    // marker gap should be roughly marker width (0) + margin-right (12px default gap ~8px fallback when margin zero)
    assert!(text_x > marker_x + 10.0 && text_x < marker_x + 14.0, "text should start after marker gap");
}

