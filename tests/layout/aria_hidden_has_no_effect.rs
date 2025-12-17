use fastrender::layout::{FormattingContext, FormattingContextFactory, LayoutConstraints};
use fastrender::style::display::Display;
use fastrender::tree::box_tree::BoxNode;
use fastrender::{ComputedStyle, FormattingContextType};
use std::sync::Arc;

fn default_style() -> Arc<ComputedStyle> {
    Arc::new(ComputedStyle::default())
}

#[test]
fn aria_hidden_does_not_hide_layout() {
    // aria-hidden should not remove boxes from layout or hide them.
    let mut style = ComputedStyle::default();
    style.display = Display::Block;
    let styled = Arc::new(style);
    let child = BoxNode::new_block(styled.clone(), FormattingContextType::Block, vec![]);
    let root = BoxNode::new_block(styled.clone(), FormattingContextType::Block, vec![child]);

    // Simulate aria-hidden by leaving style unchanged; we just ensure layout still runs.
    let factory = FormattingContextFactory::new();
    let ctx = factory.create(FormattingContextType::Block);
    let constraints = LayoutConstraints::definite(100.0, 100.0);
    let fragment = ctx.layout(&root, &constraints).expect("layout");

    assert_eq!(fragment.children.len(), 1);
    assert!(fragment.bounds.width() > 0.0);
    assert!(fragment.children[0].bounds.width() > 0.0);
}

