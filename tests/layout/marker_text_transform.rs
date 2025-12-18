use std::sync::Arc;

use fastrender::layout::constraints::LayoutConstraints;
use fastrender::layout::contexts::inline::InlineFormattingContext;
use fastrender::style::display::FormattingContextType;
use fastrender::style::types::{CaseTransform, TextTransform};
use fastrender::style::ComputedStyle;
use fastrender::tree::box_tree::{BoxNode, MarkerContent};
use fastrender::tree::fragment_tree::FragmentContent;

#[test]
fn marker_ignores_parent_text_transform() {
    let ifc = InlineFormattingContext::new();

    let mut li_style = ComputedStyle::default();
    li_style.text_transform = TextTransform::with_case(CaseTransform::Uppercase);
    let li_style = Arc::new(li_style);

    let marker = BoxNode::new_marker(li_style.clone(), MarkerContent::Text("•".to_string()));
    let text = BoxNode::new_text(li_style.clone(), "content".to_string());
    let root = BoxNode::new_block(li_style, FormattingContextType::Block, vec![marker, text]);
    let constraints = LayoutConstraints::definite(200.0, 100.0);

    let fragment = ifc.layout(&root, &constraints).expect("layout");
    let mut marker_text = None;
    let mut stack = vec![&fragment];
    while let Some(node) = stack.pop() {
        if let FragmentContent::Text { is_marker, ref text, .. } = node.content {
            if is_marker {
                marker_text = Some(text.clone());
            }
        }
        for child in &node.children {
            stack.push(child);
        }
    }

    assert_eq!(marker_text, Some("•".to_string()), "marker text should not be transformed");
}

