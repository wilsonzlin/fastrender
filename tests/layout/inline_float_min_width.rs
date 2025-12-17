use std::sync::Arc;

use fastrender::layout::contexts::inline::InlineFormattingContext;
use fastrender::layout::constraints::LayoutConstraints;
use fastrender::style::float::Float;
use fastrender::style::values::Length;
use fastrender::{BoxNode, ComputedStyle, FragmentContent};

/// Inline-level floats should still participate in shrink-to-fit and honor min/max
/// width constraints. This ensures that inline floats don't collapse to the
/// available line width when only a min-width is authored.
#[test]
fn inline_float_honors_min_width() {
    let mut float_style = ComputedStyle::default();
    float_style.display = fastrender::style::types::Display::Inline;
    float_style.float = Float::Left;
    float_style.min_width = Some(Length::px(80.0));
    float_style.height = Some(Length::px(10.0));
    let float_node = BoxNode::new_inline(Arc::new(float_style), vec![]);

    let text_style = Arc::new(ComputedStyle::default());
    let text = BoxNode::new_text(text_style.clone(), "text".to_string());
    let root = BoxNode::new_inline(text_style, vec![float_node, text]);

    let constraints = LayoutConstraints::definite_width(50.0); // narrower than the float min-width
    let mut float_ctx = fastrender::layout::float_context::FloatContext::new(50.0);
    let ifc = InlineFormattingContext::new();
    let fragment = ifc
        .layout_with_floats(&root, &constraints, Some(&mut float_ctx), 0.0)
        .expect("layout with inline float");

    // First child should be the float fragment clamped to min-width (80px).
    assert!(!fragment.children.is_empty());
    let float_frag = &fragment.children[0];
    assert!(matches!(float_frag.content, FragmentContent::Block { .. }));
    assert!(float_frag.bounds.width() >= 79.9, "float width should honor min-width");
}
