use std::sync::Arc;

use fastrender::layout::constraints::LayoutConstraints;
use fastrender::layout::contexts::inline::InlineFormattingContext;
use fastrender::style::display::FormattingContextType;
use fastrender::style::ComputedStyle;
use fastrender::tree::box_tree::{BoxNode, MarkerContent};
use fastrender::tree::fragment_tree::FragmentContent;

fn marker_and_text_baselines(fragment: &fastrender::tree::fragment_tree::FragmentNode) -> (Option<f32>, Option<f32>) {
    let mut marker_base = None;
    let mut text_base = None;
    let mut stack = vec![fragment];
    while let Some(node) = stack.pop() {
        if let FragmentContent::Text {
            is_marker,
            baseline_offset,
            ..
        } = node.content
        {
            let base = node.bounds.y() + baseline_offset;
            if is_marker && marker_base.is_none() {
                marker_base = Some(base);
            } else if !is_marker && text_base.is_none() {
                text_base = Some(base);
            }
        }
        for child in &node.children {
            stack.push(child);
        }
    }
    (marker_base, text_base)
}

#[test]
fn outside_marker_baseline_aligns_with_text() {
    let ifc = InlineFormattingContext::new();

    let marker = BoxNode::new_marker(Arc::new(ComputedStyle::default()), MarkerContent::Text("•".to_string()));
    let text = BoxNode::new_text(Arc::new(ComputedStyle::default()), "content".to_string());

    let root = BoxNode::new_block(
        Arc::new(ComputedStyle::default()),
        FormattingContextType::Block,
        vec![marker, text],
    );
    let constraints = LayoutConstraints::definite(200.0, 100.0);
    let fragment = ifc.layout(&root, &constraints).expect("layout");
    let line = fragment.children.first().expect("line fragment");

    let (marker_base, text_base) = marker_and_text_baselines(line);
    let marker_base = marker_base.expect("marker baseline");
    let text_base = text_base.expect("text baseline");

    assert!(
        (marker_base - text_base).abs() < 0.5,
        "outside marker baseline should align with text (marker={} text={})",
        marker_base,
        text_base
    );
}

