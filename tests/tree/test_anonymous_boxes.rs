//! Comprehensive tests for anonymous box creation
//!
//! Tests the AnonymousBoxCreator implementation per CSS 2.1 Section 9.2.1.1 and 9.2.2.1

use fastrender::tree::anonymous::AnonymousBoxCreator;
use fastrender::tree::box_tree::AnonymousType;
use fastrender::tree::box_tree::BoxType;
use fastrender::BoxNode;
use fastrender::ComputedStyle;
use fastrender::Display;
use fastrender::FormattingContextType;
use std::sync::Arc;

// =============================================================================
// Helper Functions
// =============================================================================

fn default_style() -> Arc<ComputedStyle> {
  Arc::new(ComputedStyle::default())
}

fn style_with_display(display: Display) -> Arc<ComputedStyle> {
  let mut style = ComputedStyle::default();
  style.display = display;
  Arc::new(style)
}

fn fixup_tree(node: BoxNode) -> BoxNode {
  fastrender::tree::anonymous::AnonymousBoxCreator::fixup_tree(node)
    .expect("anonymous box fixup")
}

fn subtree_contains_text(node: &BoxNode, needle: &str) -> bool {
  if let Some(text) = node.text() {
    if text == needle {
      return true;
    }
  }
  node
    .children
    .iter()
    .any(|child| subtree_contains_text(child, needle))
}

// =============================================================================
// Basic Fixup Tests
// =============================================================================

#[test]
fn test_empty_container_no_crash() {
  let empty_block = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);

  let fixed = fixup_tree(empty_block);
  assert_eq!(fixed.children.len(), 0);
  assert!(!fixed.is_anonymous());
}

#[test]
fn test_single_block_child_no_change() {
  let child = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);

  let container = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![child]);

  let fixed = fixup_tree(container);

  assert_eq!(fixed.children.len(), 1);
  assert!(!fixed.children[0].is_anonymous());
  assert!(fixed.children[0].is_block_level());
}

#[test]
fn test_all_block_children_no_change() {
  let child1 = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
  let child2 = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
  let child3 = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);

  let container = BoxNode::new_block(
    default_style(),
    FormattingContextType::Block,
    vec![child1, child2, child3],
  );

  let fixed = fixup_tree(container);

  assert_eq!(fixed.children.len(), 3);
  for child in &fixed.children {
    assert!(!child.is_anonymous());
    assert!(child.is_block_level());
  }
}

// =============================================================================
// Text Wrapping Tests
// =============================================================================

#[test]
fn test_single_text_in_block_wrapped() {
  let text = BoxNode::new_text(default_style(), "Hello".to_string());

  let container = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![text]);

  let fixed = fixup_tree(container);

  // Text should be wrapped in anonymous inline
  assert_eq!(fixed.children.len(), 1);
  assert!(fixed.children[0].is_anonymous());
  assert!(fixed.children[0].is_inline_level());

  // The anonymous inline should contain the text
  assert_eq!(fixed.children[0].children.len(), 1);
  assert!(fixed.children[0].children[0].is_text());
}

#[test]
fn test_multiple_text_nodes_each_wrapped() {
  let text1 = BoxNode::new_text(default_style(), "Hello".to_string());
  let text2 = BoxNode::new_text(default_style(), "World".to_string());

  let container = BoxNode::new_block(
    default_style(),
    FormattingContextType::Block,
    vec![text1, text2],
  );

  let fixed = fixup_tree(container);

  // Each text should be wrapped in its own anonymous inline
  assert_eq!(fixed.children.len(), 2);
  assert!(fixed.children[0].is_anonymous());
  assert!(fixed.children[0].is_inline_level());
  assert!(fixed.children[1].is_anonymous());
  assert!(fixed.children[1].is_inline_level());
}

#[test]
fn test_text_in_inline_container_wrapped() {
  let text = BoxNode::new_text(default_style(), "Text in span".to_string());

  let inline = BoxNode::new_inline(default_style(), vec![text]);

  let fixed = fixup_tree(inline);

  // Text inside inline should be wrapped in anonymous inline
  assert_eq!(fixed.children.len(), 1);
  assert!(fixed.children[0].is_anonymous());
  assert!(fixed.children[0].is_inline_level());
}

// =============================================================================
// Mixed Content Tests (Block + Inline)
// =============================================================================

#[test]
fn test_text_before_block_wrapped_in_anonymous_block() {
  let text = BoxNode::new_text(default_style(), "Before".to_string());
  let block = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);

  let container = BoxNode::new_block(
    default_style(),
    FormattingContextType::Block,
    vec![text, block],
  );

  let fixed = fixup_tree(container);

  // Should have 2 children: anonymous block (wrapping text), original block
  assert_eq!(fixed.children.len(), 2);

  // First child is anonymous block
  assert!(fixed.children[0].is_anonymous());
  assert!(fixed.children[0].is_block_level());

  // Second child is original block
  assert!(!fixed.children[1].is_anonymous());
  assert!(fixed.children[1].is_block_level());
}

#[test]
fn test_text_after_block_wrapped_in_anonymous_block() {
  let block = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
  let text = BoxNode::new_text(default_style(), "After".to_string());

  let container = BoxNode::new_block(
    default_style(),
    FormattingContextType::Block,
    vec![block, text],
  );

  let fixed = fixup_tree(container);

  // Should have 2 children: original block, anonymous block (wrapping text)
  assert_eq!(fixed.children.len(), 2);

  // First child is original block
  assert!(!fixed.children[0].is_anonymous());
  assert!(fixed.children[0].is_block_level());

  // Second child is anonymous block
  assert!(fixed.children[1].is_anonymous());
  assert!(fixed.children[1].is_block_level());
}

#[test]
fn test_text_between_blocks_wrapped() {
  let block1 = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
  let text = BoxNode::new_text(default_style(), "Middle".to_string());
  let block2 = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);

  let container = BoxNode::new_block(
    default_style(),
    FormattingContextType::Block,
    vec![block1, text, block2],
  );

  let fixed = fixup_tree(container);

  // Should have 3 children: block, anonymous block, block
  assert_eq!(fixed.children.len(), 3);

  assert!(!fixed.children[0].is_anonymous());
  assert!(fixed.children[1].is_anonymous());
  assert!(!fixed.children[2].is_anonymous());
}

#[test]
fn test_mixed_content_text_block_text() {
  // Classic case: text, block, text
  let text1 = BoxNode::new_text(default_style(), "Before".to_string());
  let block = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
  let text2 = BoxNode::new_text(default_style(), "After".to_string());

  let container = BoxNode::new_block(
    default_style(),
    FormattingContextType::Block,
    vec![text1, block, text2],
  );

  let fixed = fixup_tree(container);

  // Should have 3 children: anon block, block, anon block
  assert_eq!(fixed.children.len(), 3);

  // First: anonymous block containing text1
  assert!(fixed.children[0].is_anonymous());
  assert!(fixed.children[0].is_block_level());
  assert_eq!(fixed.children[0].children.len(), 1);
  assert!(fixed.children[0].children[0].is_text());

  // Middle: original block
  assert!(!fixed.children[1].is_anonymous());

  // Last: anonymous block containing text2
  assert!(fixed.children[2].is_anonymous());
  assert!(fixed.children[2].is_block_level());
  assert_eq!(fixed.children[2].children.len(), 1);
  assert!(fixed.children[2].children[0].is_text());
}

// =============================================================================
// Consecutive Inline Grouping Tests
// =============================================================================

#[test]
fn test_consecutive_inlines_grouped_in_single_anon_block() {
  let inline1 = BoxNode::new_inline(default_style(), vec![]);
  let inline2 = BoxNode::new_inline(default_style(), vec![]);
  let block = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
  let inline3 = BoxNode::new_inline(default_style(), vec![]);

  let container = BoxNode::new_block(
    default_style(),
    FormattingContextType::Block,
    vec![inline1, inline2, block, inline3],
  );

  let fixed = fixup_tree(container);

  // Should have 3 children: anon[inline1, inline2], block, anon[inline3]
  assert_eq!(fixed.children.len(), 3);

  // First anonymous block should contain 2 inlines
  assert!(fixed.children[0].is_anonymous());
  assert_eq!(fixed.children[0].children.len(), 2);

  // Second is original block
  assert!(!fixed.children[1].is_anonymous());

  // Third anonymous block should contain 1 inline
  assert!(fixed.children[2].is_anonymous());
  assert_eq!(fixed.children[2].children.len(), 1);
}

#[test]
fn test_text_and_inline_grouped_together() {
  let text = BoxNode::new_text(default_style(), "Text".to_string());
  let inline = BoxNode::new_inline(default_style(), vec![]);
  let block = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);

  let container = BoxNode::new_block(
    default_style(),
    FormattingContextType::Block,
    vec![text, inline, block],
  );

  let fixed = fixup_tree(container);

  // Text and inline should be grouped in same anonymous block
  assert_eq!(fixed.children.len(), 2);
  assert!(fixed.children[0].is_anonymous());
  assert_eq!(fixed.children[0].children.len(), 2); // text + inline
}

#[test]
fn test_alternating_block_inline_pattern() {
  let block1 = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
  let inline1 = BoxNode::new_inline(default_style(), vec![]);
  let block2 = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
  let inline2 = BoxNode::new_inline(default_style(), vec![]);
  let block3 = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);

  let container = BoxNode::new_block(
    default_style(),
    FormattingContextType::Block,
    vec![block1, inline1, block2, inline2, block3],
  );

  let fixed = fixup_tree(container);

  // Pattern: block, anon, block, anon, block
  assert_eq!(fixed.children.len(), 5);

  assert!(!fixed.children[0].is_anonymous()); // block1
  assert!(fixed.children[1].is_anonymous()); // anon[inline1]
  assert!(!fixed.children[2].is_anonymous()); // block2
  assert!(fixed.children[3].is_anonymous()); // anon[inline2]
  assert!(!fixed.children[4].is_anonymous()); // block3
}

// =============================================================================
// Nested Structure Tests
// =============================================================================

#[test]
fn test_nested_fixup_two_levels() {
  // Create nested structure that needs fixup at multiple levels
  let inner_text = BoxNode::new_text(default_style(), "Inner".to_string());
  let inner_block = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);

  let inner_container = BoxNode::new_block(
    default_style(),
    FormattingContextType::Block,
    vec![inner_text, inner_block],
  );

  let outer_text = BoxNode::new_text(default_style(), "Outer".to_string());

  let outer_container = BoxNode::new_block(
    default_style(),
    FormattingContextType::Block,
    vec![outer_text, inner_container],
  );

  let fixed = fixup_tree(outer_container);

  // Outer level: anon block, block (inner container)
  assert_eq!(fixed.children.len(), 2);
  assert!(fixed.children[0].is_anonymous());
  assert!(!fixed.children[1].is_anonymous());

  // Inner level also fixed: anon block, block
  let inner_fixed = &fixed.children[1];
  assert_eq!(inner_fixed.children.len(), 2);
  assert!(inner_fixed.children[0].is_anonymous());
  assert!(!inner_fixed.children[1].is_anonymous());
}

#[test]
fn test_deeply_nested_structure() {
  // Create: block > block > block > (text, block)
  let text = BoxNode::new_text(default_style(), "Deep".to_string());
  let inner_block = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);

  let level3 = BoxNode::new_block(
    default_style(),
    FormattingContextType::Block,
    vec![text, inner_block],
  );

  let level2 = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![level3]);

  let level1 = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![level2]);

  let fixed = fixup_tree(level1);

  // Navigate to level 3 and verify it was fixed
  let level3_fixed = &fixed.children[0].children[0];
  assert_eq!(level3_fixed.children.len(), 2);
  assert!(level3_fixed.children[0].is_anonymous());
  assert!(!level3_fixed.children[1].is_anonymous());
}

#[test]
fn test_nested_inline_in_block() {
  // block > inline > text
  let text = BoxNode::new_text(default_style(), "Nested text".to_string());
  let inline = BoxNode::new_inline(default_style(), vec![text]);

  let container = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![inline]);

  let fixed = fixup_tree(container);

  // Inline is wrapped in anonymous inline (since it's the only inline child)
  // Actually, the inline itself shouldn't change but text inside should be wrapped
  assert_eq!(fixed.children.len(), 1);

  // The inline's text should be wrapped
  let inline_fixed = &fixed.children[0];
  assert_eq!(inline_fixed.children.len(), 1);
  assert!(inline_fixed.children[0].is_anonymous());
}

// =============================================================================
// Inline boxes containing block descendants
// =============================================================================

#[test]
fn test_inline_with_block_descendant_is_split_into_block_flow() {
  let style = default_style();
  let text_before = BoxNode::new_text(style.clone(), "Before".to_string());
  let text_after = BoxNode::new_text(style.clone(), "After".to_string());
  let block = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![]);
  let inline = BoxNode::new_inline(style.clone(), vec![text_before, block.clone(), text_after]);

  let container = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![inline]);
  let fixed = fixup_tree(container);

  assert_eq!(fixed.children.len(), 3);
  assert!(fixed.children[0].is_anonymous());
  assert!(fixed.children[1].is_block_level());
  assert!(fixed.children[2].is_anonymous());

  assert!(subtree_contains_text(&fixed.children[0], "Before"));
  assert!(subtree_contains_text(&fixed.children[2], "After"));

  // Inline fragments should preserve the inline's style.
  assert!(!fixed.children[0].children.is_empty());
  assert!(std::sync::Arc::ptr_eq(
    &fixed.children[0].children[0].style,
    &style
  ));
}

#[test]
fn test_nested_inline_with_block_descendant_splits_at_outer() {
  let style = default_style();
  let before = BoxNode::new_text(style.clone(), "Before".to_string());
  let block = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![]);
  let inner_inline = BoxNode::new_inline(style.clone(), vec![before, block.clone()]);
  let after = BoxNode::new_text(style.clone(), "After".to_string());
  let outer_inline = BoxNode::new_inline(style.clone(), vec![inner_inline, after]);

  let container = BoxNode::new_block(
    style.clone(),
    FormattingContextType::Block,
    vec![outer_inline],
  );
  let fixed = fixup_tree(container);

  assert_eq!(fixed.children.len(), 3);
  assert!(fixed.children[0].is_anonymous());
  assert!(fixed.children[1].is_block_level());
  assert!(fixed.children[2].is_anonymous());

  assert!(subtree_contains_text(&fixed.children[0], "Before"));
  assert!(subtree_contains_text(&fixed.children[2], "After"));
}

#[test]
fn test_inline_block_with_block_children_is_not_split() {
  let inline_block_style = style_with_display(Display::InlineBlock);
  let block_child = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
  let inline_block = BoxNode::new_inline_block(
    inline_block_style.clone(),
    FormattingContextType::Block,
    vec![block_child],
  );

  let container = BoxNode::new_block(
    default_style(),
    FormattingContextType::Block,
    vec![inline_block.clone()],
  );
  let fixed = fixup_tree(container);

  assert_eq!(fixed.children.len(), 1);
  assert!(fixed.children[0].is_inline_level());
  assert_eq!(fixed.children[0].children.len(), 1);
}

// =============================================================================
// All Inline Content Tests (No Block Mixing)
// =============================================================================

#[test]
fn test_all_inline_children_text_gets_wrapped() {
  let inline = BoxNode::new_inline(default_style(), vec![]);
  let text = BoxNode::new_text(default_style(), "Text".to_string());

  let container = BoxNode::new_block(
    default_style(),
    FormattingContextType::Block,
    vec![inline, text],
  );

  let fixed = fixup_tree(container);

  // All inline content - text should be wrapped in anonymous inline
  assert_eq!(fixed.children.len(), 2);

  // First is inline (unchanged)
  assert!(!fixed.children[0].is_anonymous());
  assert!(fixed.children[0].is_inline_level());

  // Second is text wrapped in anonymous inline
  assert!(fixed.children[1].is_anonymous());
  assert!(fixed.children[1].is_inline_level());
}

#[test]
fn test_multiple_inlines_no_block_mixing() {
  let inline1 = BoxNode::new_inline(default_style(), vec![]);
  let inline2 = BoxNode::new_inline(default_style(), vec![]);
  let inline3 = BoxNode::new_inline(default_style(), vec![]);

  let container = BoxNode::new_block(
    default_style(),
    FormattingContextType::Block,
    vec![inline1, inline2, inline3],
  );

  let fixed = fixup_tree(container);

  // All inline content - no anonymous blocks needed
  assert_eq!(fixed.children.len(), 3);
  for child in &fixed.children {
    assert!(!child.is_anonymous());
    assert!(child.is_inline_level());
  }
}

// =============================================================================
// Helper Function Tests
// =============================================================================

#[test]
fn test_has_mixed_content() {
  let block = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
  let inline = BoxNode::new_inline(default_style(), vec![]);

  let mixed = vec![block.clone(), inline.clone()];
  let all_block = vec![block.clone(), block.clone()];
  let all_inline = vec![inline.clone(), inline.clone()];

  assert!(AnonymousBoxCreator::has_mixed_content(&mixed));
  assert!(!AnonymousBoxCreator::has_mixed_content(&all_block));
  assert!(!AnonymousBoxCreator::has_mixed_content(&all_inline));
}

#[test]
fn test_all_block_level() {
  let block1 = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
  let block2 = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
  let inline = BoxNode::new_inline(default_style(), vec![]);

  let all_blocks = vec![block1.clone(), block2.clone()];
  let mixed = vec![block1.clone(), inline];

  assert!(AnonymousBoxCreator::all_block_level(&all_blocks));
  assert!(!AnonymousBoxCreator::all_block_level(&mixed));
}

#[test]
fn test_all_inline_level() {
  let inline1 = BoxNode::new_inline(default_style(), vec![]);
  let inline2 = BoxNode::new_inline(default_style(), vec![]);
  let text = BoxNode::new_text(default_style(), "text".to_string());
  let block = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);

  let all_inline = vec![inline1.clone(), inline2.clone(), text];
  let mixed = vec![inline1, block];

  assert!(AnonymousBoxCreator::all_inline_level(&all_inline));
  assert!(!AnonymousBoxCreator::all_inline_level(&mixed));
}

#[test]
fn test_count_anonymous_boxes() {
  let text1 = BoxNode::new_text(default_style(), "Text 1".to_string());
  let block = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
  let text2 = BoxNode::new_text(default_style(), "Text 2".to_string());

  let container = BoxNode::new_block(
    default_style(),
    FormattingContextType::Block,
    vec![text1, block, text2],
  );

  let fixed = fixup_tree(container);

  // Should have 2 anonymous blocks (one for each text)
  let count = AnonymousBoxCreator::count_anonymous_boxes(&fixed);
  assert_eq!(count, 2);
}

// =============================================================================
// Edge Cases
// =============================================================================

#[test]
fn test_single_inline_child_not_wrapped_in_anon_block() {
  let inline = BoxNode::new_inline(default_style(), vec![]);

  let container = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![inline]);

  let fixed = fixup_tree(container);

  // Single inline child - no block wrapping needed (no mixed content)
  assert_eq!(fixed.children.len(), 1);
  assert!(!fixed.children[0].is_anonymous());
}

#[test]
fn test_empty_text_node() {
  let text = BoxNode::new_text(default_style(), "".to_string());

  let container = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![text]);

  let fixed = fixup_tree(container);

  // Empty text is still wrapped
  assert_eq!(fixed.children.len(), 1);
  assert!(fixed.children[0].is_anonymous());
}

#[test]
fn test_whitespace_only_text_node() {
  let text = BoxNode::new_text(default_style(), "   \n\t  ".to_string());

  let container = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![text]);

  let fixed = fixup_tree(container);

  // Whitespace text is still wrapped (collapsing is a later phase)
  assert_eq!(fixed.children.len(), 1);
  assert!(fixed.children[0].is_anonymous());
}

#[test]
fn test_anonymous_type_is_block() {
  let text = BoxNode::new_text(default_style(), "Text".to_string());
  let block = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);

  let container = BoxNode::new_block(
    default_style(),
    FormattingContextType::Block,
    vec![text, block],
  );

  let fixed = fixup_tree(container);

  // Verify the anonymous box type is Block
  match &fixed.children[0].box_type {
    BoxType::Anonymous(anon) => {
      assert_eq!(anon.anonymous_type, AnonymousType::Block);
    }
    _ => panic!("Expected anonymous box"),
  }
}

#[test]
fn test_anonymous_inline_type() {
  let text = BoxNode::new_text(default_style(), "Text".to_string());

  // All inline content in block container
  let container = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![text]);

  let fixed = fixup_tree(container);

  // Text should be wrapped in anonymous inline
  match &fixed.children[0].box_type {
    BoxType::Anonymous(anon) => {
      assert_eq!(anon.anonymous_type, AnonymousType::Inline);
    }
    _ => panic!("Expected anonymous box"),
  }
}
