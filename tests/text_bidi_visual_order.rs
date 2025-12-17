use fastrender::layout::contexts::inline::InlineFormattingContext;
use fastrender::{FormattingContext, FormattingContextType, LayoutConstraints};
use fastrender::style::types::{Direction, UnicodeBidi};
use fastrender::tree::box_tree::BoxNode;
use std::sync::Arc;

fn default_style() -> Arc<fastrender::ComputedStyle> {
    Arc::new(fastrender::ComputedStyle::default())
}

fn collect_text_with_x(fragment: &fastrender::tree::fragment_tree::FragmentNode) -> Vec<(String, f32)> {
    let mut out = Vec::new();
    let mut stack = vec![fragment];
    while let Some(node) = stack.pop() {
        if let fastrender::tree::fragment_tree::FragmentContent::Text { text, .. } = &node.content {
            out.push((text.clone(), node.bounds.x()));
        }
        for child in &node.children {
            stack.push(child);
        }
    }
    out
}

#[test]
fn bidi_visual_order_handles_mixed_arabic_ltr() {
    // Sequence: LTR "A ", RTL arabic digits, LTR trailing.
    let mut rtl_style = fastrender::ComputedStyle::default();
    rtl_style.direction = Direction::Rtl;
    rtl_style.unicode_bidi = UnicodeBidi::Embed;
    let rtl_style = Arc::new(rtl_style);

    let root = BoxNode::new_block(
        default_style(),
        FormattingContextType::Block,
        vec![
            BoxNode::new_text(default_style(), "A ".to_string()),
            BoxNode::new_inline(
                rtl_style.clone(),
                vec![BoxNode::new_text(rtl_style.clone(), "123".to_string())],
            ),
            BoxNode::new_text(default_style(), " B".to_string()),
        ],
    );

    let ifc = InlineFormattingContext::new();
    let constraints = LayoutConstraints::definite_width(400.0);
    let fragment = ifc.layout(&root, &constraints).expect("layout");

    let mut texts = collect_text_with_x(&fragment);
    texts.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap_or(std::cmp::Ordering::Equal));
    let labels: String = texts.into_iter().map(|(t, _)| t).collect();
    // Visual order should keep the LTR run first, then the embedded digits together, then trailing B.
    assert_eq!(labels, "A 123B");
}
