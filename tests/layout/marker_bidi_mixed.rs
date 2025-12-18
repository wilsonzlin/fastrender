use std::sync::Arc;

use fastrender::layout::constraints::LayoutConstraints;
use fastrender::layout::contexts::inline::InlineFormattingContext;
use fastrender::style::display::FormattingContextType;
use fastrender::style::types::Direction;
use fastrender::style::ComputedStyle;
use fastrender::tree::box_tree::{BoxNode, MarkerContent};
use fastrender::tree::fragment_tree::FragmentContent;

fn collect_visual(fragment: &fastrender::tree::fragment_tree::FragmentNode) -> Vec<(String, f32)> {
    let mut out = Vec::new();
    let mut stack = vec![fragment];
    while let Some(node) = stack.pop() {
        if let FragmentContent::Text { ref text, .. } = node.content {
            out.push((text.clone(), node.bounds.x()));
        }
        for child in &node.children {
            stack.push(child);
        }
    }
    out.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap_or(std::cmp::Ordering::Equal));
    out
}

#[test]
fn marker_remains_at_visual_start_with_mixed_bidi() {
    let ifc = InlineFormattingContext::new();

    let mut marker_style = ComputedStyle::default();
    marker_style.direction = Direction::Rtl;
    let marker = BoxNode::new_marker(Arc::new(marker_style), MarkerContent::Text("•".to_string()));

    let mut text_style = ComputedStyle::default();
    text_style.direction = Direction::Rtl;
    let text = BoxNode::new_text(Arc::new(text_style), "שלום abc".to_string());

    let mut root_style = ComputedStyle::default();
    root_style.direction = Direction::Rtl;
    let root = BoxNode::new_block(
        Arc::new(root_style),
        FormattingContextType::Block,
        vec![marker, text],
    );
    let constraints = LayoutConstraints::definite(200.0, 100.0);
    let fragment = ifc.layout(&root, &constraints).expect("layout");

    let visual = collect_visual(&fragment);
    assert!(visual.first().map(|(t, _)| t.contains('•')).unwrap_or(false), "marker should be first visually");
}

