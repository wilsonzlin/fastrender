use std::sync::Arc;

use fastrender::layout::constraints::LayoutConstraints;
use fastrender::layout::contexts::inline::InlineFormattingContext;
use fastrender::style::display::FormattingContextType;
use fastrender::style::types::Direction;
use fastrender::style::ComputedStyle;
use fastrender::tree::box_tree::{BoxNode, MarkerContent};
use fastrender::tree::fragment_tree::FragmentContent;

fn collect_texts(fragment: &fastrender::tree::fragment_tree::FragmentNode) -> Vec<String> {
    let mut out = Vec::new();
    let mut stack = vec![fragment];
    while let Some(node) = stack.pop() {
        if let FragmentContent::Text { ref text, .. } = node.content {
            out.push(text.clone());
        }
        for child in &node.children {
            stack.push(child);
        }
    }
    out
}

#[test]
fn marker_appears_before_text_in_visual_order() {
    let ifc = InlineFormattingContext::new();

    let mut marker_style = ComputedStyle::default();
    marker_style.direction = Direction::Rtl;
    let marker = BoxNode::new_marker(Arc::new(marker_style), MarkerContent::Text("•".to_string()));

    let mut text_style = ComputedStyle::default();
    text_style.direction = Direction::Rtl;
    let text = BoxNode::new_text(Arc::new(text_style), "abc".to_string());

    let mut root_style = ComputedStyle::default();
    root_style.direction = Direction::Rtl;
    let root = BoxNode::new_block(
        Arc::new(root_style),
        FormattingContextType::Block,
        vec![marker, text],
    );
    let constraints = LayoutConstraints::definite(200.0, 100.0);
    let fragment = ifc.layout(&root, &constraints).expect("layout");

    let texts = collect_texts(&fragment);
    let combined = texts.join("");
    // Visual order should keep marker first even in RTL.
    assert!(combined.starts_with('•'), "marker should appear before text in visual order: {}", combined);
}

