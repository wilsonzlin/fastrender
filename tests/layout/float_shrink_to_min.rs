use std::sync::Arc;

use fastrender::layout::contexts::block::BlockFormattingContext;
use fastrender::layout::constraints::LayoutConstraints;
use fastrender::style::float::Float;
use fastrender::style::values::Length;
use fastrender::{BoxNode, ComputedStyle, FormattingContextType};

/// Floats with `width: auto` should use the CSS shrink-to-fit formula and then
/// honor `min-width`/`max-width` caps. When the available width is smaller than
/// the authored minimum, the used width must clamp to the min-width instead of
/// collapsing to the available space.
#[test]
fn float_auto_width_honors_min_width() {
    // Container 100px wide with a single floating child that has only a min-width.
    let container_style = Arc::new(ComputedStyle::default());

    let mut float_style = ComputedStyle::default();
    float_style.float = Float::Left;
    float_style.min_width = Some(Length::px(150.0));
    let float_box = BoxNode::new_block(Arc::new(float_style), FormattingContextType::Block, vec![]);

    let container = BoxNode::new_block(container_style, FormattingContextType::Block, vec![float_box]);

    let mut bfc = BlockFormattingContext::new();
    let constraints = LayoutConstraints::definite(100.0, 1000.0);
    let fragment = bfc.layout(&container, &constraints).expect("layout should succeed");

    // The float should clamp up to its min-width even though the available width is smaller.
    assert_eq!(fragment.children.len(), 1);
    let float_fragment = &fragment.children[0];
    assert!((float_fragment.bounds.width() - 150.0).abs() < 0.01);
}
