use std::sync::Arc;

use fastrender::layout::constraints::LayoutConstraints;
use fastrender::layout::contexts::block::BlockFormattingContext;
use fastrender::layout::contexts::flex::FlexFormattingContext;
use fastrender::layout::formatting_context::FormattingContext;
use fastrender::style::display::{Display, FormattingContextType};
use fastrender::style::position::Position;
use fastrender::style::ComputedStyle;
use fastrender::style::values::Length;
use fastrender::tree::box_tree::BoxNode;

#[test]
fn nested_absolute_descendant_uses_positioned_parent_padding_block_in_block_layout() {
    let mut root_style = ComputedStyle::default();
    root_style.position = Position::Relative;

    let mut parent_style = ComputedStyle::default();
    parent_style.position = Position::Absolute;
    parent_style.width = Some(Length::px(100.0));
    parent_style.height = Some(Length::px(40.0));

    let mut child_style = ComputedStyle::default();
    child_style.position = Position::Absolute;
    child_style.left = Some(Length::percent(50.0));
    child_style.width = Some(Length::px(10.0));
    child_style.height = Some(Length::px(10.0));

    let child = BoxNode::new_block(Arc::new(child_style), FormattingContextType::Block, vec![]);
    let parent = BoxNode::new_block(Arc::new(parent_style), FormattingContextType::Block, vec![child]);
    let root = BoxNode::new_block(Arc::new(root_style), FormattingContextType::Block, vec![parent]);

    let constraints = LayoutConstraints::definite(200.0, 200.0);
    let fc = BlockFormattingContext::new();
    let fragment = fc.layout(&root, &constraints).expect("block layout");

    assert_eq!(fragment.children.len(), 1, "parent fragment should be present");
    let parent_fragment = &fragment.children[0];
    assert!((parent_fragment.bounds.width() - 100.0).abs() < 0.1);

    assert_eq!(parent_fragment.children.len(), 1, "nested abs child should be laid out");
    let grandchild = &parent_fragment.children[0];

    assert!(
        (grandchild.bounds.x() - 50.0).abs() < 0.1,
        "grandchild should resolve left:50% against parent's 100px padding box; got {}",
        grandchild.bounds.x()
    );
}

#[test]
fn nested_absolute_descendant_uses_positioned_parent_padding_block_in_flex_layout() {
    let mut container_style = ComputedStyle::default();
    container_style.display = Display::Flex;
    container_style.position = Position::Relative;

    let mut parent_style = ComputedStyle::default();
    parent_style.position = Position::Absolute;
    parent_style.width = Some(Length::px(100.0));
    parent_style.height = Some(Length::px(30.0));

    let mut child_style = ComputedStyle::default();
    child_style.position = Position::Absolute;
    child_style.left = Some(Length::percent(50.0));
    child_style.width = Some(Length::px(8.0));
    child_style.height = Some(Length::px(8.0));

    let child = BoxNode::new_block(Arc::new(child_style), FormattingContextType::Block, vec![]);
    let parent = BoxNode::new_block(Arc::new(parent_style), FormattingContextType::Block, vec![child]);
    let container = BoxNode::new_block(Arc::new(container_style), FormattingContextType::Flex, vec![parent]);

    let constraints = LayoutConstraints::definite(200.0, 150.0);
    let fc = FlexFormattingContext::new();
    let fragment = fc.layout(&container, &constraints).expect("flex layout");

    assert_eq!(fragment.children.len(), 1, "absolute child should produce a fragment");
    let parent_fragment = &fragment.children[0];
    assert!((parent_fragment.bounds.width() - 100.0).abs() < 0.1);

    assert_eq!(parent_fragment.children.len(), 1, "nested abs child should be laid out");
    let grandchild = &parent_fragment.children[0];

    assert!(
        (grandchild.bounds.x() - 50.0).abs() < 0.1,
        "grandchild should resolve left:50% against parent's 100px padding box; got {}",
        grandchild.bounds.x()
    );
}
