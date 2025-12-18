use std::sync::Arc;

use fastrender::layout::constraints::LayoutConstraints;
use fastrender::layout::contexts::inline::InlineFormattingContext;
use fastrender::style::display::FormattingContextType;
use fastrender::style::types::FontWeight;
use fastrender::style::ComputedStyle;
use fastrender::tree::box_tree::{BoxNode, MarkerContent};
use fastrender::tree::fragment_tree::FragmentContent;

#[test]
fn marker_inherits_font_weight_from_list_item() {
    let ifc = InlineFormattingContext::new();

    let mut li_style = ComputedStyle::default();
    li_style.font_weight = FontWeight::Bolder;
    let li_style = Arc::new(li_style);

    let marker = BoxNode::new_marker(li_style.clone(), MarkerContent::Text("•".to_string()));
    let text = BoxNode::new_text(li_style.clone(), "content".to_string());
    let root = BoxNode::new_block(li_style, FormattingContextType::Block, vec![marker, text]);
    let constraints = LayoutConstraints::definite(200.0, 100.0);

    let fragment = ifc.layout(&root, &constraints).expect("layout");
    let mut marker_weight = None;
    let mut stack = vec![&fragment];
    while let Some(node) = stack.pop() {
        if let FragmentContent::Text { is_marker, .. } = node.content {
            if is_marker {
                if let Some(style) = node.style.as_ref() {
                    marker_weight = Some(style.font_weight);
                }
            }
        }
        for child in &node.children {
            stack.push(child);
        }
    }

    assert_eq!(marker_weight, Some(FontWeight::Bolder));
}

