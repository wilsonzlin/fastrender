use std::sync::Arc;

use fastrender::layout::constraints::LayoutConstraints;
use fastrender::layout::contexts::inline::InlineFormattingContext;
use fastrender::style::display::FormattingContextType;
use fastrender::style::types::{ListStyleType, FontWeight};
use fastrender::style::ComputedStyle;
use fastrender::tree::box_tree::{BoxNode, MarkerContent};
use fastrender::tree::fragment_tree::FragmentContent;

#[test]
fn marker_inherits_counter_font() {
    let ifc = InlineFormattingContext::new();

    let mut li_style = ComputedStyle::default();
    li_style.list_style_type = ListStyleType::UpperAlpha;
    li_style.font_weight = FontWeight::Bold;
    li_style.font_family = "fantasy".to_string();
    let li_style = Arc::new(li_style);

    let text = BoxNode::new_text(li_style.clone(), "item".to_string());
    let marker = BoxNode::new_marker(li_style.clone(), MarkerContent::Text(String::new()));
    let root = BoxNode::new_block(li_style, FormattingContextType::Block, vec![marker, text]);
    let constraints = LayoutConstraints::definite(200.0, 100.0);

    let fragment = ifc.layout(&root, &constraints).expect("layout");
    let mut marker_font = None;
    let mut marker_weight = None;
    let mut stack = vec![&fragment];
    while let Some(node) = stack.pop() {
        if let FragmentContent::Text { is_marker, .. } = node.content {
            if is_marker {
                if let Some(style) = node.style.as_ref() {
                    marker_font = Some(style.font_family.clone());
                    marker_weight = Some(style.font_weight);
                }
            }
        }
        for child in &node.children {
            stack.push(child);
        }
    }

    assert_eq!(marker_font, Some("fantasy".to_string()));
    assert_eq!(marker_weight, Some(FontWeight::Bold));
}

