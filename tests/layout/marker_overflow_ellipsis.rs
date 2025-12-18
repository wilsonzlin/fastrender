use std::sync::Arc;

use fastrender::layout::constraints::LayoutConstraints;
use fastrender::layout::contexts::inline::InlineFormattingContext;
use fastrender::style::display::FormattingContextType;
use fastrender::style::types::{Overflow, TextOverflow, TextOverflowSide, WhiteSpace};
use fastrender::style::ComputedStyle;
use fastrender::tree::box_tree::{BoxNode, MarkerContent};
use fastrender::tree::fragment_tree::FragmentContent;

#[test]
fn ellipsis_respects_outside_marker() {
    let mut container_style = ComputedStyle::default();
    container_style.white_space = WhiteSpace::Nowrap;
    container_style.overflow_x = Overflow::Hidden;
    container_style.text_overflow = TextOverflow {
        inline_start: TextOverflowSide::Clip,
        inline_end: TextOverflowSide::Ellipsis,
    };

    let mut marker_style = ComputedStyle::default();
    marker_style.list_style_position = fastrender::style::types::ListStylePosition::Outside;

    let marker = BoxNode::new_marker(Arc::new(marker_style), MarkerContent::Text("•".to_string()));
    let text = BoxNode::new_text(Arc::new(ComputedStyle::default()), "long content that will overflow".to_string());
    let root = BoxNode::new_block(Arc::new(container_style), FormattingContextType::Block, vec![marker, text]);

    let constraints = LayoutConstraints::definite_width(80.0);
    let ifc = InlineFormattingContext::new();
    let fragment = ifc.layout(&root, &constraints).expect("layout");

    let mut texts = Vec::new();
    let mut marker_x = None;
    let mut ellipsis_x = None;
    let mut stack = vec![&fragment];
    while let Some(node) = stack.pop() {
        match node.content {
            FragmentContent::Text { ref text, is_marker, .. } => {
                if is_marker {
                    marker_x.get_or_insert(node.bounds.x());
                } else if text.contains('…') {
                    ellipsis_x.get_or_insert(node.bounds.x());
                }
                texts.push(text.clone());
            }
            _ => {}
        }
        for child in &node.children {
            stack.push(child);
        }
    }

    assert!(texts.iter().any(|t| t.contains('…')), "expected ellipsis with overflow");
    let marker_x = marker_x.expect("marker x");
    let ellipsis_x = ellipsis_x.expect("ellipsis x");

    assert!(marker_x < -5.0, "outside marker should remain before text");
    assert!(ellipsis_x >= 0.0, "ellipsis should appear in the content region, not at marker origin");
}

