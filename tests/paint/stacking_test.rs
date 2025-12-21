//! Integration tests for Stacking Context Tree
//!
//! These tests verify the stacking context implementation for CSS paint ordering.
//!
//! CSS References:
//! - CSS 2.1 Appendix E: https://www.w3.org/TR/CSS21/zindex.html
//! - CSS 2.1 Section 9.9: https://www.w3.org/TR/CSS21/visuren.html#layered-presentation

use fastrender::geometry::Rect;
use fastrender::FragmentNode;
use fastrender::{
    build_stacking_tree, creates_stacking_context, get_stacking_context_reason, StackingContext, StackingContextReason,
};
use fastrender::{ComputedStyle, Display, Float, Overflow, Position};
use std::sync::Arc;

// Helper functions

fn block_fragment(x: f32, y: f32, w: f32, h: f32) -> FragmentNode {
    FragmentNode::new_block(Rect::from_xywh(x, y, w, h), vec![])
}

fn block_fragment_with_children(x: f32, y: f32, w: f32, h: f32, children: Vec<FragmentNode>) -> FragmentNode {
    FragmentNode::new_block(Rect::from_xywh(x, y, w, h), children)
}

fn text_fragment(x: f32, y: f32, w: f32, h: f32, text: &str) -> FragmentNode {
    FragmentNode::new_text(Rect::from_xywh(x, y, w, h), text.to_string(), 16.0)
}

// Test: Root element always creates stacking context

#[test]
fn test_root_creates_stacking_context() {
    let style = ComputedStyle::default();
    assert!(creates_stacking_context(&style, None, true));
}

// Test: Normal elements don't create stacking context

#[test]
fn test_normal_element_no_stacking_context() {
    let style = ComputedStyle::default();
    assert!(!creates_stacking_context(&style, None, false));
}

// Test: Fixed positioning creates stacking context

#[test]
fn test_fixed_positioning_creates_stacking_context() {
    let mut style = ComputedStyle::default();
    style.position = Position::Fixed;
    assert!(creates_stacking_context(&style, None, false));
    assert_eq!(
        get_stacking_context_reason(&style, None, false),
        Some(StackingContextReason::FixedPositioning)
    );
}

// Test: Sticky positioning creates stacking context

#[test]
fn test_sticky_positioning_creates_stacking_context() {
    let mut style = ComputedStyle::default();
    style.position = Position::Sticky;
    assert!(creates_stacking_context(&style, None, false));
    assert_eq!(
        get_stacking_context_reason(&style, None, false),
        Some(StackingContextReason::StickyPositioning)
    );
}

// Test: Positioned element with z-index creates stacking context

#[test]
fn test_positioned_with_z_index_creates_stacking_context() {
    let mut style = ComputedStyle::default();
    style.position = Position::Relative;
    style.z_index = Some(10);
    assert!(creates_stacking_context(&style, None, false));
    assert_eq!(
        get_stacking_context_reason(&style, None, false),
        Some(StackingContextReason::PositionedWithZIndex)
    );
}

// Test: Positioned element with z-index: 0 does create stacking context when specified

#[test]
fn test_positioned_with_zero_z_index_creates_stacking_context() {
    let mut style = ComputedStyle::default();
    style.position = Position::Relative;
    style.z_index = Some(0);
    assert!(creates_stacking_context(&style, None, false));
}

// Test: Opacity < 1 creates stacking context

#[test]
fn test_opacity_creates_stacking_context() {
    let mut style = ComputedStyle::default();
    style.opacity = 0.5;
    assert!(creates_stacking_context(&style, None, false));
    assert_eq!(
        get_stacking_context_reason(&style, None, false),
        Some(StackingContextReason::Opacity)
    );
}

// Test: Opacity = 0 creates stacking context

#[test]
fn test_opacity_zero_creates_stacking_context() {
    let mut style = ComputedStyle::default();
    style.opacity = 0.0;
    assert!(creates_stacking_context(&style, None, false));
}

// Test: Opacity = 1 doesn't create stacking context

#[test]
fn test_opacity_one_no_stacking_context() {
    let mut style = ComputedStyle::default();
    style.opacity = 1.0;
    assert!(!creates_stacking_context(&style, None, false));
}

// Test: Flex item with z-index creates stacking context

#[test]
fn test_flex_item_with_z_index_creates_stacking_context() {
    let mut parent_style = ComputedStyle::default();
    parent_style.display = Display::Flex;

    let mut child_style = ComputedStyle::default();
    child_style.z_index = Some(5);

    assert!(creates_stacking_context(&child_style, Some(&parent_style), false));
    assert_eq!(
        get_stacking_context_reason(&child_style, Some(&parent_style), false),
        Some(StackingContextReason::FlexItemWithZIndex)
    );
}

// Test: Grid item with z-index creates stacking context

#[test]
fn test_grid_item_with_z_index_creates_stacking_context() {
    let mut parent_style = ComputedStyle::default();
    parent_style.display = Display::Grid;

    let mut child_style = ComputedStyle::default();
    child_style.z_index = Some(5);

    assert!(creates_stacking_context(&child_style, Some(&parent_style), false));
    assert_eq!(
        get_stacking_context_reason(&child_style, Some(&parent_style), false),
        Some(StackingContextReason::GridItemWithZIndex)
    );
}

// Test: Transform creates stacking context

#[test]
fn test_transform_creates_stacking_context() {
    use fastrender::Transform;

    let mut style = ComputedStyle::default();
    style.transform.push(Transform::Rotate(45.0));
    assert!(creates_stacking_context(&style, None, false));
    assert_eq!(
        get_stacking_context_reason(&style, None, false),
        Some(StackingContextReason::Transform)
    );
}

// Test: Build stacking tree from single fragment

#[test]
fn test_build_stacking_tree_single() {
    let fragment = block_fragment(0.0, 0.0, 100.0, 100.0);
    let tree = build_stacking_tree(&fragment, None, true);

    assert_eq!(tree.z_index, 0);
    assert_eq!(tree.reason, StackingContextReason::Root);
    assert!(!tree.fragments.is_empty());
}

// Test: Build stacking tree from nested fragments

#[test]
fn test_build_stacking_tree_nested() {
    let child1 = block_fragment(0.0, 0.0, 50.0, 50.0);
    let child2 = block_fragment(50.0, 0.0, 50.0, 50.0);
    let root = block_fragment_with_children(0.0, 0.0, 100.0, 100.0, vec![child1, child2]);

    let tree = build_stacking_tree(&root, None, true);

    assert_eq!(tree.reason, StackingContextReason::Root);
    // Should have root fragment and children
    assert!(tree.total_fragment_count() >= 3);
}

// Test: Stacking context with opacity creates child context

#[test]
fn test_stacking_tree_opacity_child() {
    let fragment = block_fragment(0.0, 0.0, 100.0, 100.0);

    let mut style = ComputedStyle::default();
    style.opacity = 0.8;

    let tree = build_stacking_tree(&fragment, Some(&style), false);

    assert_eq!(tree.reason, StackingContextReason::Opacity);
    assert_eq!(tree.z_index, 0);
}

// Test: Z-index sorting of child stacking contexts

#[test]
fn test_z_index_sorting() {
    let mut parent = StackingContext::root();

    // Add children with various z-indices
    parent.add_child(StackingContext::with_reason(
        5,
        StackingContextReason::PositionedWithZIndex,
        1,
    ));
    parent.add_child(StackingContext::with_reason(
        -3,
        StackingContextReason::PositionedWithZIndex,
        2,
    ));
    parent.add_child(StackingContext::with_reason(0, StackingContextReason::Opacity, 3));
    parent.add_child(StackingContext::with_reason(
        2,
        StackingContextReason::PositionedWithZIndex,
        4,
    ));
    parent.add_child(StackingContext::with_reason(
        -1,
        StackingContextReason::PositionedWithZIndex,
        5,
    ));

    parent.sort_children();

    // Should be sorted: -3, -1, 0, 2, 5
    assert_eq!(parent.children[0].z_index, -3);
    assert_eq!(parent.children[1].z_index, -1);
    assert_eq!(parent.children[2].z_index, 0);
    assert_eq!(parent.children[3].z_index, 2);
    assert_eq!(parent.children[4].z_index, 5);
}

// Test: Negative z-index children sorted correctly

#[test]
fn test_negative_z_children() {
    let mut parent = StackingContext::root();

    parent.add_child(StackingContext::with_reason(
        -1,
        StackingContextReason::PositionedWithZIndex,
        1,
    ));
    parent.add_child(StackingContext::with_reason(
        -10,
        StackingContextReason::PositionedWithZIndex,
        2,
    ));
    parent.add_child(StackingContext::with_reason(
        -5,
        StackingContextReason::PositionedWithZIndex,
        3,
    ));
    parent.add_child(StackingContext::with_reason(
        1,
        StackingContextReason::PositionedWithZIndex,
        4,
    ));

    let negative = parent.negative_z_children();

    assert_eq!(negative.len(), 3);
    // Most negative first
    assert_eq!(negative[0].z_index, -10);
    assert_eq!(negative[1].z_index, -5);
    assert_eq!(negative[2].z_index, -1);
}

// Test: Positive z-index children sorted correctly

#[test]
fn test_positive_z_children() {
    let mut parent = StackingContext::root();

    parent.add_child(StackingContext::with_reason(
        10,
        StackingContextReason::PositionedWithZIndex,
        1,
    ));
    parent.add_child(StackingContext::with_reason(
        1,
        StackingContextReason::PositionedWithZIndex,
        2,
    ));
    parent.add_child(StackingContext::with_reason(
        5,
        StackingContextReason::PositionedWithZIndex,
        3,
    ));
    parent.add_child(StackingContext::with_reason(
        -1,
        StackingContextReason::PositionedWithZIndex,
        4,
    ));

    let positive = parent.positive_z_children();

    assert_eq!(positive.len(), 3);
    // Least positive first
    assert_eq!(positive[0].z_index, 1);
    assert_eq!(positive[1].z_index, 5);
    assert_eq!(positive[2].z_index, 10);
}

// Test: Tree order for equal z-index

#[test]
fn test_tree_order_for_equal_z_index() {
    let mut parent = StackingContext::root();

    // All same z-index, different tree orders
    parent.add_child(StackingContext::with_reason(0, StackingContextReason::Opacity, 3));
    parent.add_child(StackingContext::with_reason(0, StackingContextReason::Transform, 1));
    parent.add_child(StackingContext::with_reason(
        0,
        StackingContextReason::FixedPositioning,
        2,
    ));

    parent.sort_children();

    // Should be sorted by tree_order when z-index is equal
    assert_eq!(parent.children[0].tree_order, 1);
    assert_eq!(parent.children[1].tree_order, 2);
    assert_eq!(parent.children[2].tree_order, 3);
}

// Test: Paint order iterator

#[test]
fn test_paint_order_iterator() {
    let mut root = StackingContext::root();
    root.fragments.push(block_fragment(0.0, 0.0, 100.0, 100.0));

    // Add a negative z-index child
    let mut neg_child = StackingContext::with_reason(-1, StackingContextReason::PositionedWithZIndex, 1);
    neg_child.fragments.push(block_fragment(10.0, 10.0, 20.0, 20.0));
    root.add_child(neg_child);

    // Add a positive z-index child
    let mut pos_child = StackingContext::with_reason(1, StackingContextReason::PositionedWithZIndex, 2);
    pos_child.fragments.push(block_fragment(30.0, 30.0, 20.0, 20.0));
    root.add_child(pos_child);

    let fragments: Vec<_> = root.iter_paint_order().collect();

    // Should have 3 fragments in order: root, negative child, positive child
    assert_eq!(fragments.len(), 3);
    assert_eq!(fragments[0].bounds.x(), 0.0); // Root (layer 1)
    assert_eq!(fragments[1].bounds.x(), 10.0); // Negative child (layer 2)
    assert_eq!(fragments[2].bounds.x(), 30.0); // Positive child (layer 7)
}

// Test: Layer classification - blocks

#[test]
fn test_layer_classification_blocks() {
    let mut sc = StackingContext::root();
    let fragment = block_fragment(0.0, 0.0, 100.0, 100.0);
    let mut style = ComputedStyle::default();
    style.display = Display::Block;

    sc.add_fragment_to_layer(fragment, Some(&style));

    assert_eq!(sc.layer3_blocks.len(), 1);
    assert!(sc.layer4_floats.is_empty());
    assert!(sc.layer5_inlines.is_empty());
    assert!(sc.layer6_positioned.is_empty());
}

// Test: Layer classification - inlines

#[test]
fn test_layer_classification_inlines() {
    let mut sc = StackingContext::root();
    let fragment = text_fragment(0.0, 0.0, 50.0, 20.0, "Hello");
    let mut style = ComputedStyle::default();
    style.display = Display::Inline;

    sc.add_fragment_to_layer(fragment, Some(&style));

    assert!(sc.layer3_blocks.is_empty());
    assert!(sc.layer5_inlines.len() == 1);
}

// Test: Layer classification - positioned

#[test]
fn test_layer_classification_positioned() {
    let mut sc = StackingContext::root();
    let fragment = block_fragment(0.0, 0.0, 100.0, 100.0);
    let mut style = ComputedStyle::default();
    style.display = Display::Block;
    style.position = Position::Relative;
    // z_index = 0 (default), goes to layer 6

    sc.add_fragment_to_layer(fragment, Some(&style));

    assert_eq!(sc.layer6_positioned.len(), 1);
}

// Test: Layer classification - floats

#[test]
fn test_layer_classification_floats() {
    let mut sc = StackingContext::root();
    let fragment = block_fragment(0.0, 0.0, 80.0, 20.0);
    let mut style = ComputedStyle::default();
    style.display = Display::Block;
    style.float = Float::Left;

    sc.add_fragment_to_layer(fragment, Some(&style));

    assert!(sc.layer3_blocks.is_empty());
    assert_eq!(sc.layer4_floats.len(), 1);
    assert!(sc.layer5_inlines.is_empty());
    assert!(sc.layer6_positioned.is_empty());
}

// Test: Floats on positioned elements stay in positioned layer

#[test]
fn test_layer_classification_floats_ignored_for_positioned() {
    let mut sc = StackingContext::root();
    let fragment = block_fragment(0.0, 0.0, 80.0, 20.0);
    let mut style = ComputedStyle::default();
    style.display = Display::Block;
    style.float = Float::Left;
    style.position = Position::Absolute;

    sc.add_fragment_to_layer(fragment, Some(&style));

    assert!(sc.layer4_floats.is_empty());
    assert_eq!(sc.layer6_positioned.len(), 1);
}

// Test: Paint order includes floats between blocks and inlines

#[test]
fn test_paint_order_with_float_layer() {
    let mut sc = StackingContext::root();
    sc.fragments.push(block_fragment(0.0, 0.0, 10.0, 10.0));

    let mut block_style = ComputedStyle::default();
    block_style.display = Display::Block;
    sc.add_fragment_to_layer(block_fragment(10.0, 0.0, 10.0, 10.0), Some(&block_style));

    let mut float_style = ComputedStyle::default();
    float_style.display = Display::Block;
    float_style.float = Float::Left;
    sc.add_fragment_to_layer(block_fragment(20.0, 0.0, 10.0, 10.0), Some(&float_style));

    let mut inline_style = ComputedStyle::default();
    inline_style.display = Display::Inline;
    sc.add_fragment_to_layer(text_fragment(30.0, 0.0, 10.0, 10.0, "x"), Some(&inline_style));

    let mut positioned_style = ComputedStyle::default();
    positioned_style.position = Position::Relative;
    sc.add_fragment_to_layer(block_fragment(40.0, 0.0, 10.0, 10.0), Some(&positioned_style));

    let order: Vec<f32> = sc.iter_paint_order().map(|f| f.bounds.x()).collect();
    assert_eq!(order, vec![0.0, 10.0, 20.0, 30.0, 40.0]);
}

// Test: Bounds computation

#[test]
fn test_bounds_computation() {
    let mut sc = StackingContext::root();
    sc.fragments.push(block_fragment(0.0, 0.0, 50.0, 50.0));
    sc.layer3_blocks.push(block_fragment(40.0, 40.0, 60.0, 60.0));

    sc.compute_bounds();

    // Should encompass both: (0,0) to (100,100)
    assert_eq!(sc.bounds.min_x(), 0.0);
    assert_eq!(sc.bounds.min_y(), 0.0);
    assert_eq!(sc.bounds.max_x(), 100.0);
    assert_eq!(sc.bounds.max_y(), 100.0);
}

// Test: Fragment count

#[test]
fn test_fragment_count() {
    let mut sc = StackingContext::root();
    sc.fragments.push(block_fragment(0.0, 0.0, 10.0, 10.0));
    sc.layer3_blocks.push(block_fragment(10.0, 0.0, 10.0, 10.0));
    sc.layer3_blocks.push(block_fragment(20.0, 0.0, 10.0, 10.0));
    sc.layer5_inlines.push(text_fragment(30.0, 0.0, 10.0, 10.0, "hi"));

    assert_eq!(sc.fragment_count(), 4);
}

// Test: Total fragment count with children

#[test]
fn test_total_fragment_count() {
    let mut parent = StackingContext::root();
    parent.fragments.push(block_fragment(0.0, 0.0, 10.0, 10.0));

    let mut child = StackingContext::with_reason(1, StackingContextReason::PositionedWithZIndex, 1);
    child.fragments.push(block_fragment(10.0, 0.0, 10.0, 10.0));
    child.layer3_blocks.push(block_fragment(20.0, 0.0, 10.0, 10.0));

    parent.add_child(child);

    assert_eq!(parent.fragment_count(), 1);
    assert_eq!(parent.total_fragment_count(), 3);
}

// Test: Positioned with overflow creates stacking context

#[test]
fn test_positioned_overflow_creates_stacking_context() {
    let mut style = ComputedStyle::default();
    style.position = Position::Relative;
    style.overflow_x = Overflow::Hidden;

    assert!(creates_stacking_context(&style, None, false));
    assert_eq!(
        get_stacking_context_reason(&style, None, false),
        Some(StackingContextReason::OverflowClip)
    );
}

#[test]
fn test_positioned_overflow_clip_creates_stacking_context() {
    let mut style = ComputedStyle::default();
    style.position = Position::Relative;
    style.overflow_y = Overflow::Clip;

    assert!(creates_stacking_context(&style, None, false));
    assert_eq!(
        get_stacking_context_reason(&style, None, false),
        Some(StackingContextReason::OverflowClip)
    );
}

// Test: Multiple stacking context triggers (opacity takes precedence order)

#[test]
fn test_multiple_triggers_precedence() {
    let mut style = ComputedStyle::default();
    style.position = Position::Fixed;
    style.opacity = 0.5;

    // Fixed positioning comes first in the check order
    assert!(creates_stacking_context(&style, None, false));
    // The first matching reason is returned
    let reason = get_stacking_context_reason(&style, None, false);
    assert!(reason == Some(StackingContextReason::FixedPositioning) || reason == Some(StackingContextReason::Opacity));
}

// Test: Empty stacking context

#[test]
fn test_empty_stacking_context() {
    let sc = StackingContext::root();

    assert_eq!(sc.fragment_count(), 0);
    assert!(sc.children.is_empty());
    assert!(sc.iter_paint_order().next().is_none());
}

// Test: InlineFlex parent with z-index child

#[test]
fn test_inline_flex_item_with_z_index() {
    let mut parent_style = ComputedStyle::default();
    parent_style.display = Display::InlineFlex;

    let mut child_style = ComputedStyle::default();
    child_style.z_index = Some(2);

    assert!(creates_stacking_context(&child_style, Some(&parent_style), false));
}

// Test: InlineGrid parent with z-index child

#[test]
fn test_inline_grid_item_with_z_index() {
    let mut parent_style = ComputedStyle::default();
    parent_style.display = Display::InlineGrid;

    let mut child_style = ComputedStyle::default();
    child_style.z_index = Some(2);

    assert!(creates_stacking_context(&child_style, Some(&parent_style), false));
}

// Test: Absolute positioning with z-index

#[test]
fn test_absolute_with_z_index() {
    let mut style = ComputedStyle::default();
    style.position = Position::Absolute;
    style.z_index = Some(100);

    assert!(creates_stacking_context(&style, None, false));
    assert_eq!(
        get_stacking_context_reason(&style, None, false),
        Some(StackingContextReason::PositionedWithZIndex)
    );
}

// Test: Negative z-index

#[test]
fn test_negative_z_index() {
    let mut style = ComputedStyle::default();
    style.position = Position::Relative;
    style.z_index = Some(-5);

    assert!(creates_stacking_context(&style, None, false));
    assert_eq!(
        get_stacking_context_reason(&style, None, false),
        Some(StackingContextReason::PositionedWithZIndex)
    );
}

// Test: Build stacking tree with styled lookup

#[test]
fn test_build_stacking_tree_with_styles() {
    use fastrender::build_stacking_tree_with_styles;

    let fragment = block_fragment(0.0, 0.0, 100.0, 100.0);

    let style_lookup = |_: &FragmentNode| -> Option<Arc<ComputedStyle>> {
        let mut style = ComputedStyle::default();
        style.opacity = 0.9;
        Some(Arc::new(style))
    };

    let tree = build_stacking_tree_with_styles(&fragment, style_lookup);

    // Root element always has Root reason (is_root = true takes precedence)
    // The opacity would trigger stacking context, but root takes priority
    assert_eq!(tree.reason, StackingContextReason::Root);
    assert_eq!(tree.z_index, 0);
    assert!(!tree.fragments.is_empty());
}
