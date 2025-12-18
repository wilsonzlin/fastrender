use std::sync::Arc;

use fastrender::layout::constraints::LayoutConstraints;
use fastrender::layout::contexts::inline::InlineFormattingContext;
use fastrender::style::display::FormattingContextType;
use fastrender::style::types::ListStyleImage;
use fastrender::style::ComputedStyle;
use fastrender::tree::box_tree::{BoxNode, MarkerContent};
use fastrender::tree::fragment_tree::FragmentContent;

fn collect_replaced(fragment: &fastrender::tree::fragment_tree::FragmentNode) -> bool {
    let mut stack = vec![fragment];
    while let Some(node) = stack.pop() {
        if matches!(node.content, FragmentContent::Replaced { .. }) {
            return true;
        }
        for child in &node.children {
            stack.push(child);
        }
    }
    false
}

#[test]
fn list_style_image_svg_produces_replaced_marker() {
    let mut marker_style = ComputedStyle::default();
    marker_style.list_style_image = ListStyleImage::Url("data:image/svg+xml,<svg></svg>".to_string());
    let marker = BoxNode::new_marker(Arc::new(marker_style), MarkerContent::Text(String::new()));
    let text = BoxNode::new_text(Arc::new(ComputedStyle::default()), "content".to_string());
    let root = BoxNode::new_block(
        Arc::new(ComputedStyle::default()),
        FormattingContextType::Block,
        vec![marker, text],
    );

    let constraints = LayoutConstraints::definite(200.0, 200.0);
    let ifc = InlineFormattingContext::new();
    let fragment = ifc.layout(&root, &constraints).expect("layout");
    let line = fragment.children.first().expect("line fragment");

    assert!(collect_replaced(line), "SVG marker image should produce a replaced fragment");
}

