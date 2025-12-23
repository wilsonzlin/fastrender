//! Comprehensive tests for pseudo-element box generation
//!
//! Tests the pseudo-element implementation per CSS 2.1 Section 12.1
//! (The :before and :after pseudo-elements)
//!
//! These tests verify:
//! - Pseudo-element type handling (::before, ::after)
//! - Content property parsing and generation
//! - Display property handling for pseudo-elements
//! - Box insertion order (before at start, after at end)
//! - Integration with box generation pipeline

use fastrender::count_pseudo_boxes;
use fastrender::find_pseudo_boxes;
use fastrender::get_pseudo_type;
use fastrender::is_pseudo_box;
use fastrender::BoxNode;
use fastrender::ComputedStyle;
use fastrender::Display;
use fastrender::FormattingContextType;
use fastrender::PseudoContent;
use fastrender::PseudoContentItem;
use fastrender::PseudoElementConfig;
use fastrender::PseudoElementGenerator;
use fastrender::PseudoElementStyle;
use fastrender::PseudoElementType;
use std::sync::Arc;

// =============================================================================
// Helper Functions
// =============================================================================

fn default_style() -> Arc<ComputedStyle> {
  Arc::new(ComputedStyle::default())
}

// =============================================================================
// PseudoElementType Tests
// =============================================================================

#[test]
fn test_pseudo_type_before_properties() {
  let pseudo_type = PseudoElementType::Before;
  assert_eq!(pseudo_type.css_name(), "::before");
  assert_eq!(pseudo_type.short_name(), "before");
  assert_eq!(format!("{}", pseudo_type), "::before");
}

#[test]
fn test_pseudo_type_after_properties() {
  let pseudo_type = PseudoElementType::After;
  assert_eq!(pseudo_type.css_name(), "::after");
  assert_eq!(pseudo_type.short_name(), "after");
  assert_eq!(format!("{}", pseudo_type), "::after");
}

#[test]
fn test_pseudo_type_equality() {
  assert_eq!(PseudoElementType::Before, PseudoElementType::Before);
  assert_eq!(PseudoElementType::After, PseudoElementType::After);
  assert_ne!(PseudoElementType::Before, PseudoElementType::After);
}

#[test]
fn test_pseudo_type_copy() {
  let before = PseudoElementType::Before;
  let copied = before; // Copy trait
  assert_eq!(before, copied);
}

// =============================================================================
// PseudoContent Tests
// =============================================================================

#[test]
fn test_content_none() {
  let content = PseudoContent::None;
  assert!(!content.generates_box());
  assert_eq!(content.to_text(), None);
}

#[test]
fn test_content_normal() {
  let content = PseudoContent::Normal;
  assert!(!content.generates_box());
  assert_eq!(content.to_text(), None);
}

#[test]
fn test_content_string() {
  let content = PseudoContent::String("Hello, World!".to_string());
  assert!(content.generates_box());
  assert_eq!(content.to_text(), Some("Hello, World!".to_string()));
}

#[test]
fn test_content_string_helper() {
  let content = PseudoContent::string("Test content");
  assert_eq!(content, PseudoContent::String("Test content".to_string()));
  assert!(content.generates_box());
}

#[test]
fn test_content_open_quote() {
  let content = PseudoContent::OpenQuote;
  assert!(content.generates_box());
  assert!(content.to_text().is_some());
}

#[test]
fn test_content_close_quote() {
  let content = PseudoContent::CloseQuote;
  assert!(content.generates_box());
  assert!(content.to_text().is_some());
}

#[test]
fn test_content_no_open_quote() {
  let content = PseudoContent::NoOpenQuote;
  assert!(!content.generates_box());
  assert_eq!(content.to_text(), None);
}

#[test]
fn test_content_no_close_quote() {
  let content = PseudoContent::NoCloseQuote;
  assert!(!content.generates_box());
  assert_eq!(content.to_text(), None);
}

#[test]
fn test_content_multiple_items() {
  let items = vec![
    PseudoContentItem::String("Hello".to_string()),
    PseudoContentItem::String(", ".to_string()),
    PseudoContentItem::String("World!".to_string()),
  ];
  let content = PseudoContent::Multiple(items);
  assert!(content.generates_box());
  assert_eq!(content.to_text(), Some("Hello, World!".to_string()));
}

#[test]
fn test_content_multiple_empty() {
  let items: Vec<PseudoContentItem> = vec![];
  let content = PseudoContent::Multiple(items);
  assert!(content.generates_box()); // Multiple still generates
  assert_eq!(content.to_text(), None); // But no text
}

#[test]
fn test_content_default() {
  let content: PseudoContent = Default::default();
  assert_eq!(content, PseudoContent::None);
}

// =============================================================================
// PseudoContentItem Tests
// =============================================================================

#[test]
fn test_content_item_string() {
  let item = PseudoContentItem::String("Test".to_string());
  assert_eq!(item.to_text(), Some("Test".to_string()));
}

#[test]
fn test_content_item_open_quote() {
  let item = PseudoContentItem::OpenQuote;
  assert!(item.to_text().is_some());
}

#[test]
fn test_content_item_close_quote() {
  let item = PseudoContentItem::CloseQuote;
  assert!(item.to_text().is_some());
}

#[test]
fn test_content_item_attr() {
  // Attr requires context to resolve
  let item = PseudoContentItem::Attr("data-value".to_string());
  assert_eq!(item.to_text(), None);
}

#[test]
fn test_content_item_counter() {
  // Counter requires context to resolve
  let item = PseudoContentItem::Counter("section".to_string());
  assert_eq!(item.to_text(), None);
}

#[test]
fn test_content_item_url() {
  // URL items don't have text
  let item = PseudoContentItem::Url("image.png".to_string());
  assert_eq!(item.to_text(), None);
}

// =============================================================================
// PseudoElementStyle Tests
// =============================================================================

#[test]
fn test_style_with_string_content() {
  let style = PseudoElementStyle::with_string("Content here", default_style());
  assert!(style.generates_box());
  assert_eq!(style.content.to_text(), Some("Content here".to_string()));
}

#[test]
fn test_style_none() {
  let style = PseudoElementStyle::none(default_style());
  assert!(!style.generates_box());
  assert_eq!(style.display, Display::Inline);
}

#[test]
fn test_style_with_display_block() {
  let style =
    PseudoElementStyle::with_string("Block", default_style()).with_display(Display::Block);
  assert_eq!(style.display, Display::Block);
  assert!(style.generates_box());
}

#[test]
fn test_style_with_display_none() {
  let style = PseudoElementStyle {
    content: PseudoContent::String("Hidden".to_string()),
    display: Display::None,
    computed: default_style(),
  };
  assert!(!style.generates_box());
}

#[test]
fn test_style_inline_block() {
  let style = PseudoElementStyle::with_string("Inline-block", default_style())
    .with_display(Display::InlineBlock);
  assert_eq!(style.display, Display::InlineBlock);
}

#[test]
fn test_style_flex_display() {
  let style = PseudoElementStyle::with_string("Flex", default_style()).with_display(Display::Flex);
  assert_eq!(style.display, Display::Flex);
}

#[test]
fn test_style_grid_display() {
  let style = PseudoElementStyle::with_string("Grid", default_style()).with_display(Display::Grid);
  assert_eq!(style.display, Display::Grid);
}

// =============================================================================
// PseudoElementConfig Tests
// =============================================================================

#[test]
fn test_config_new_empty() {
  let config = PseudoElementConfig::new();
  assert!(!config.has_generated_content());
  assert!(!config.has_before());
  assert!(!config.has_after());
  assert!(config.get(PseudoElementType::Before).is_none());
  assert!(config.get(PseudoElementType::After).is_none());
}

#[test]
fn test_config_with_before_only() {
  let style = PseudoElementStyle::with_string("Before", default_style());
  let config = PseudoElementConfig::with_before(style);

  assert!(config.has_generated_content());
  assert!(config.has_before());
  assert!(!config.has_after());
  assert!(config.get(PseudoElementType::Before).is_some());
}

#[test]
fn test_config_with_after_only() {
  let style = PseudoElementStyle::with_string("After", default_style());
  let config = PseudoElementConfig::with_after(style);

  assert!(config.has_generated_content());
  assert!(!config.has_before());
  assert!(config.has_after());
  assert!(config.get(PseudoElementType::After).is_some());
}

#[test]
fn test_config_with_both() {
  let before = PseudoElementStyle::with_string("Before", default_style());
  let after = PseudoElementStyle::with_string("After", default_style());
  let config = PseudoElementConfig::with_both(before, after);

  assert!(config.has_generated_content());
  assert!(config.has_before());
  assert!(config.has_after());
}

#[test]
fn test_config_set_before() {
  let mut config = PseudoElementConfig::new();
  assert!(!config.has_before());

  let style = PseudoElementStyle::with_string("Before", default_style());
  config.set_before(style);

  assert!(config.has_before());
}

#[test]
fn test_config_set_after() {
  let mut config = PseudoElementConfig::new();
  assert!(!config.has_after());

  let style = PseudoElementStyle::with_string("After", default_style());
  config.set_after(style);

  assert!(config.has_after());
}

#[test]
fn test_config_get_before() {
  let style = PseudoElementStyle::with_string("Test", default_style());
  let config = PseudoElementConfig::with_before(style);

  let retrieved = config.get(PseudoElementType::Before);
  assert!(retrieved.is_some());
  assert_eq!(
    retrieved.unwrap().content.to_text(),
    Some("Test".to_string())
  );
}

#[test]
fn test_config_get_after() {
  let style = PseudoElementStyle::with_string("Test", default_style());
  let config = PseudoElementConfig::with_after(style);

  let retrieved = config.get(PseudoElementType::After);
  assert!(retrieved.is_some());
  assert_eq!(
    retrieved.unwrap().content.to_text(),
    Some("Test".to_string())
  );
}

#[test]
fn test_config_none_content_no_generation() {
  let style = PseudoElementStyle::none(default_style());
  let config = PseudoElementConfig::with_before(style);

  // has_generated_content should return false when content is none
  assert!(!config.has_generated_content());
  assert!(!config.has_before());
}

// =============================================================================
// PseudoElementGenerator Tests
// =============================================================================

#[test]
fn test_generator_new() {
  let generator = PseudoElementGenerator::new();
  let style = PseudoElementStyle::with_string("Test", default_style());
  // Generator should be created and work successfully
  let result = generator.generate_box(PseudoElementType::Before, &style);
  assert!(result.is_some());
}

#[test]
fn test_generator_default() {
  let generator: PseudoElementGenerator = Default::default();
  let style = PseudoElementStyle::with_string("Test", default_style());
  // Default should work same as new
  let result = generator.generate_box(PseudoElementType::Before, &style);
  assert!(result.is_some());
}

#[test]
fn test_generator_without_debug_info() {
  let generator = PseudoElementGenerator::without_debug_info();
  let style = PseudoElementStyle::with_string("Test", default_style());

  let result = generator.generate_box(PseudoElementType::Before, &style);
  assert!(result.is_some());
  assert!(result.unwrap().debug_info.is_none());
}

#[test]
fn test_generate_box_returns_none_for_none_content() {
  let generator = PseudoElementGenerator::new();
  let style = PseudoElementStyle::none(default_style());

  let result = generator.generate_box(PseudoElementType::Before, &style);
  assert!(result.is_none());
}

#[test]
fn test_generate_box_returns_box_for_string_content() {
  let generator = PseudoElementGenerator::new();
  let style = PseudoElementStyle::with_string("Content", default_style());

  let result = generator.generate_box(PseudoElementType::Before, &style);
  assert!(result.is_some());

  let box_node = result.unwrap();
  assert!(box_node.is_inline_level());
}

#[test]
fn test_generate_box_inline_display() {
  let generator = PseudoElementGenerator::new();
  let style =
    PseudoElementStyle::with_string("Inline", default_style()).with_display(Display::Inline);

  let result = generator.generate_box(PseudoElementType::Before, &style);
  let box_node = result.unwrap();

  assert!(box_node.is_inline_level());
  assert!(!box_node.is_block_level());
}

#[test]
fn test_generate_box_block_display() {
  let generator = PseudoElementGenerator::new();
  let style =
    PseudoElementStyle::with_string("Block", default_style()).with_display(Display::Block);

  let result = generator.generate_box(PseudoElementType::Before, &style);
  let box_node = result.unwrap();

  assert!(box_node.is_block_level());
  assert_eq!(
    box_node.formatting_context(),
    Some(FormattingContextType::Block)
  );
}

#[test]
fn test_generate_box_inline_block_display() {
  let generator = PseudoElementGenerator::new();
  let style = PseudoElementStyle::with_string("Inline-block", default_style())
    .with_display(Display::InlineBlock);

  let result = generator.generate_box(PseudoElementType::After, &style);
  let box_node = result.unwrap();

  assert!(box_node.is_inline_level());
  assert_eq!(
    box_node.formatting_context(),
    Some(FormattingContextType::Block)
  );
}

#[test]
fn test_generate_box_flex_display() {
  let generator = PseudoElementGenerator::new();
  let style = PseudoElementStyle::with_string("Flex", default_style()).with_display(Display::Flex);

  let result = generator.generate_box(PseudoElementType::Before, &style);
  let box_node = result.unwrap();

  assert!(box_node.is_block_level());
  assert_eq!(
    box_node.formatting_context(),
    Some(FormattingContextType::Flex)
  );
}

#[test]
fn test_generate_box_grid_display() {
  let generator = PseudoElementGenerator::new();
  let style = PseudoElementStyle::with_string("Grid", default_style()).with_display(Display::Grid);

  let result = generator.generate_box(PseudoElementType::Before, &style);
  let box_node = result.unwrap();

  assert!(box_node.is_block_level());
  assert_eq!(
    box_node.formatting_context(),
    Some(FormattingContextType::Grid)
  );
}

#[test]
fn test_generate_box_inline_flex_display() {
  let generator = PseudoElementGenerator::new();
  let style = PseudoElementStyle::with_string("InlineFlex", default_style())
    .with_display(Display::InlineFlex);

  let result = generator.generate_box(PseudoElementType::Before, &style);
  let box_node = result.unwrap();

  assert!(box_node.is_inline_level());
  assert_eq!(
    box_node.formatting_context(),
    Some(FormattingContextType::Flex)
  );
}

#[test]
fn test_generate_box_inline_grid_display() {
  let generator = PseudoElementGenerator::new();
  let style = PseudoElementStyle::with_string("InlineGrid", default_style())
    .with_display(Display::InlineGrid);

  let result = generator.generate_box(PseudoElementType::Before, &style);
  let box_node = result.unwrap();

  assert!(box_node.is_inline_level());
  assert_eq!(
    box_node.formatting_context(),
    Some(FormattingContextType::Grid)
  );
}

#[test]
fn test_generate_box_contains_text_child() {
  let generator = PseudoElementGenerator::new();
  let style = PseudoElementStyle::with_string("Text content", default_style());

  let result = generator.generate_box(PseudoElementType::Before, &style);
  let box_node = result.unwrap();

  assert_eq!(box_node.children.len(), 1);
  assert!(box_node.children[0].is_text());
  assert_eq!(box_node.children[0].text(), Some("Text content"));
}

#[test]
fn test_generate_box_debug_info() {
  let generator = PseudoElementGenerator::new();
  let style = PseudoElementStyle::with_string("Debug", default_style());

  let before = generator
    .generate_box(PseudoElementType::Before, &style)
    .unwrap();
  let after = generator
    .generate_box(PseudoElementType::After, &style)
    .unwrap();

  // Before debug info
  assert!(before.debug_info.is_some());
  let before_debug = before.debug_info.as_ref().unwrap();
  assert_eq!(before_debug.tag_name, Some("before".to_string()));
  assert!(before_debug.classes.contains(&"pseudo-element".to_string()));

  // After debug info
  assert!(after.debug_info.is_some());
  let after_debug = after.debug_info.as_ref().unwrap();
  assert_eq!(after_debug.tag_name, Some("after".to_string()));
  assert!(after_debug.classes.contains(&"pseudo-element".to_string()));
}

// =============================================================================
// Insert Pseudo Boxes Tests
// =============================================================================

#[test]
fn test_insert_pseudo_boxes_empty_config() {
  let generator = PseudoElementGenerator::new();
  let config = PseudoElementConfig::new();

  let children = vec![
    BoxNode::new_text(default_style(), "Child 1".to_string()),
    BoxNode::new_text(default_style(), "Child 2".to_string()),
  ];

  let result = generator.insert_pseudo_boxes(children, &config);
  assert_eq!(result.len(), 2);
}

#[test]
fn test_insert_pseudo_boxes_before() {
  let generator = PseudoElementGenerator::new();
  let style = PseudoElementStyle::with_string("Before", default_style());
  let config = PseudoElementConfig::with_before(style);

  let children = vec![BoxNode::new_text(default_style(), "Child".to_string())];

  let result = generator.insert_pseudo_boxes(children, &config);

  assert_eq!(result.len(), 2);
  assert!(is_pseudo_box(&result[0])); // First is ::before
  assert!(!is_pseudo_box(&result[1])); // Second is original
}

#[test]
fn test_insert_pseudo_boxes_after() {
  let generator = PseudoElementGenerator::new();
  let style = PseudoElementStyle::with_string("After", default_style());
  let config = PseudoElementConfig::with_after(style);

  let children = vec![BoxNode::new_text(default_style(), "Child".to_string())];

  let result = generator.insert_pseudo_boxes(children, &config);

  assert_eq!(result.len(), 2);
  assert!(!is_pseudo_box(&result[0])); // First is original
  assert!(is_pseudo_box(&result[1])); // Second is ::after
}

#[test]
fn test_insert_pseudo_boxes_both() {
  let generator = PseudoElementGenerator::new();
  let before = PseudoElementStyle::with_string("Before", default_style());
  let after = PseudoElementStyle::with_string("After", default_style());
  let config = PseudoElementConfig::with_both(before, after);

  let children = vec![
    BoxNode::new_text(default_style(), "Child 1".to_string()),
    BoxNode::new_text(default_style(), "Child 2".to_string()),
  ];

  let result = generator.insert_pseudo_boxes(children, &config);

  assert_eq!(result.len(), 4);
  assert_eq!(get_pseudo_type(&result[0]), Some(PseudoElementType::Before));
  assert!(!is_pseudo_box(&result[1]));
  assert!(!is_pseudo_box(&result[2]));
  assert_eq!(get_pseudo_type(&result[3]), Some(PseudoElementType::After));
}

#[test]
fn test_insert_pseudo_boxes_empty_children() {
  let generator = PseudoElementGenerator::new();
  let before = PseudoElementStyle::with_string("Before", default_style());
  let after = PseudoElementStyle::with_string("After", default_style());
  let config = PseudoElementConfig::with_both(before, after);

  let children: Vec<BoxNode> = vec![];
  let result = generator.insert_pseudo_boxes(children, &config);

  assert_eq!(result.len(), 2);
  assert_eq!(get_pseudo_type(&result[0]), Some(PseudoElementType::Before));
  assert_eq!(get_pseudo_type(&result[1]), Some(PseudoElementType::After));
}

#[test]
fn test_insert_pseudo_boxes_none_content_ignored() {
  let generator = PseudoElementGenerator::new();
  let before = PseudoElementStyle::none(default_style());
  let after = PseudoElementStyle::with_string("After", default_style());
  let config = PseudoElementConfig::with_both(before, after);

  let children = vec![BoxNode::new_text(default_style(), "Child".to_string())];

  let result = generator.insert_pseudo_boxes(children, &config);

  // Only ::after is inserted (::before has content: none)
  assert_eq!(result.len(), 2);
  assert!(!is_pseudo_box(&result[0])); // Original child
  assert!(is_pseudo_box(&result[1])); // ::after only
}

// =============================================================================
// Helper Function Tests
// =============================================================================

#[test]
fn test_count_pseudo_boxes_empty_tree() {
  let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
  assert_eq!(count_pseudo_boxes(&root), 0);
}

#[test]
fn test_count_pseudo_boxes_with_pseudos() {
  let generator = PseudoElementGenerator::new();
  let style = PseudoElementStyle::with_string("Pseudo", default_style());

  let before = generator
    .generate_box(PseudoElementType::Before, &style)
    .unwrap();
  let after = generator
    .generate_box(PseudoElementType::After, &style)
    .unwrap();
  let normal = BoxNode::new_text(default_style(), "Normal".to_string());

  let root = BoxNode::new_block(
    default_style(),
    FormattingContextType::Block,
    vec![before, normal, after],
  );

  assert_eq!(count_pseudo_boxes(&root), 2);
}

#[test]
fn test_count_pseudo_boxes_nested() {
  let generator = PseudoElementGenerator::new();
  let style = PseudoElementStyle::with_string("Pseudo", default_style());

  let inner_before = generator
    .generate_box(PseudoElementType::Before, &style)
    .unwrap();
  let inner_block = BoxNode::new_block(
    default_style(),
    FormattingContextType::Block,
    vec![inner_before],
  );

  let outer_after = generator
    .generate_box(PseudoElementType::After, &style)
    .unwrap();
  let root = BoxNode::new_block(
    default_style(),
    FormattingContextType::Block,
    vec![inner_block, outer_after],
  );

  assert_eq!(count_pseudo_boxes(&root), 2);
}

#[test]
fn test_find_pseudo_boxes_empty_tree() {
  let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
  assert!(find_pseudo_boxes(&root).is_empty());
}

#[test]
fn test_find_pseudo_boxes_with_pseudos() {
  let generator = PseudoElementGenerator::new();
  let style = PseudoElementStyle::with_string("Pseudo", default_style());

  let before = generator
    .generate_box(PseudoElementType::Before, &style)
    .unwrap();
  let after = generator
    .generate_box(PseudoElementType::After, &style)
    .unwrap();

  let root = BoxNode::new_block(
    default_style(),
    FormattingContextType::Block,
    vec![before, after],
  );

  let found = find_pseudo_boxes(&root);
  assert_eq!(found.len(), 2);
}

#[test]
fn test_is_pseudo_box_true() {
  let generator = PseudoElementGenerator::new();
  let style = PseudoElementStyle::with_string("Test", default_style());

  let pseudo = generator
    .generate_box(PseudoElementType::Before, &style)
    .unwrap();
  assert!(is_pseudo_box(&pseudo));
}

#[test]
fn test_is_pseudo_box_false() {
  let normal = BoxNode::new_text(default_style(), "Normal".to_string());
  assert!(!is_pseudo_box(&normal));
}

#[test]
fn test_get_pseudo_type_before() {
  let generator = PseudoElementGenerator::new();
  let style = PseudoElementStyle::with_string("Test", default_style());

  let before = generator
    .generate_box(PseudoElementType::Before, &style)
    .unwrap();
  assert_eq!(get_pseudo_type(&before), Some(PseudoElementType::Before));
}

#[test]
fn test_get_pseudo_type_after() {
  let generator = PseudoElementGenerator::new();
  let style = PseudoElementStyle::with_string("Test", default_style());

  let after = generator
    .generate_box(PseudoElementType::After, &style)
    .unwrap();
  assert_eq!(get_pseudo_type(&after), Some(PseudoElementType::After));
}

#[test]
fn test_get_pseudo_type_none() {
  let normal = BoxNode::new_text(default_style(), "Normal".to_string());
  assert_eq!(get_pseudo_type(&normal), None);
}

// =============================================================================
// Edge Case Tests
// =============================================================================

#[test]
fn test_empty_string_content() {
  let generator = PseudoElementGenerator::new();
  let style = PseudoElementStyle::with_string("", default_style());

  // Empty string still generates a box per CSS spec
  let result = generator.generate_box(PseudoElementType::Before, &style);
  assert!(result.is_some());
}

#[test]
fn test_whitespace_only_content() {
  let generator = PseudoElementGenerator::new();
  let style = PseudoElementStyle::with_string("   \n\t   ", default_style());

  let result = generator.generate_box(PseudoElementType::Before, &style);
  assert!(result.is_some());
}

#[test]
fn test_unicode_content() {
  let generator = PseudoElementGenerator::new();
  let style = PseudoElementStyle::with_string("« 日本語 中文 한국어 »", default_style());

  let result = generator.generate_box(PseudoElementType::Before, &style);
  assert!(result.is_some());

  let box_node = result.unwrap();
  assert_eq!(box_node.children[0].text(), Some("« 日本語 中文 한국어 »"));
}

#[test]
fn test_emoji_content() {
  let generator = PseudoElementGenerator::new();
  let style = PseudoElementStyle::with_string("🎉 ✨ 🚀", default_style());

  let result = generator.generate_box(PseudoElementType::Before, &style);
  assert!(result.is_some());

  let box_node = result.unwrap();
  assert_eq!(box_node.children[0].text(), Some("🎉 ✨ 🚀"));
}

#[test]
fn test_special_characters_content() {
  let generator = PseudoElementGenerator::new();
  let style = PseudoElementStyle::with_string("&copy; &mdash; &nbsp;", default_style());

  let result = generator.generate_box(PseudoElementType::Before, &style);
  assert!(result.is_some());
}

#[test]
fn test_deeply_nested_pseudo() {
  let generator = PseudoElementGenerator::new();
  let style = PseudoElementStyle::with_string("Deep", default_style());

  // Create deep nesting
  let pseudo = generator
    .generate_box(PseudoElementType::Before, &style)
    .unwrap();
  let level3 = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![pseudo]);
  let level2 = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![level3]);
  let level1 = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![level2]);

  // Should find the deeply nested pseudo
  assert_eq!(count_pseudo_boxes(&level1), 1);
}

#[test]
fn test_multiple_pseudo_in_siblings() {
  let generator = PseudoElementGenerator::new();
  let style = PseudoElementStyle::with_string("Sibling", default_style());

  let before1 = generator
    .generate_box(PseudoElementType::Before, &style)
    .unwrap();
  let after1 = generator
    .generate_box(PseudoElementType::After, &style)
    .unwrap();
  let sibling1 = BoxNode::new_block(
    default_style(),
    FormattingContextType::Block,
    vec![before1, after1],
  );

  let before2 = generator
    .generate_box(PseudoElementType::Before, &style)
    .unwrap();
  let after2 = generator
    .generate_box(PseudoElementType::After, &style)
    .unwrap();
  let sibling2 = BoxNode::new_block(
    default_style(),
    FormattingContextType::Block,
    vec![before2, after2],
  );

  let root = BoxNode::new_block(
    default_style(),
    FormattingContextType::Block,
    vec![sibling1, sibling2],
  );

  assert_eq!(count_pseudo_boxes(&root), 4);
}

#[test]
fn test_generator_reuse() {
  let generator = PseudoElementGenerator::new();

  // Generator should be reusable for multiple boxes
  for i in 0..10 {
    let style = PseudoElementStyle::with_string(format!("Content {}", i), default_style());
    let result = generator.generate_box(PseudoElementType::Before, &style);
    assert!(result.is_some());
  }
}

// =============================================================================
// CSS Spec Compliance Tests
// =============================================================================

#[test]
fn test_css_spec_before_is_first_child() {
  // CSS 2.1 Section 12.1: ::before content is inserted as first child
  let generator = PseudoElementGenerator::new();
  let style = PseudoElementStyle::with_string("First", default_style());
  let config = PseudoElementConfig::with_before(style);

  let children = vec![
    BoxNode::new_text(default_style(), "Original first".to_string()),
    BoxNode::new_text(default_style(), "Original second".to_string()),
  ];

  let result = generator.insert_pseudo_boxes(children, &config);

  // ::before should be at index 0
  assert!(is_pseudo_box(&result[0]));
  assert_eq!(get_pseudo_type(&result[0]), Some(PseudoElementType::Before));
}

#[test]
fn test_css_spec_after_is_last_child() {
  // CSS 2.1 Section 12.1: ::after content is inserted as last child
  let generator = PseudoElementGenerator::new();
  let style = PseudoElementStyle::with_string("Last", default_style());
  let config = PseudoElementConfig::with_after(style);

  let children = vec![
    BoxNode::new_text(default_style(), "Original first".to_string()),
    BoxNode::new_text(default_style(), "Original second".to_string()),
  ];

  let result = generator.insert_pseudo_boxes(children, &config);

  // ::after should be at last index
  let last_idx = result.len() - 1;
  assert!(is_pseudo_box(&result[last_idx]));
  assert_eq!(
    get_pseudo_type(&result[last_idx]),
    Some(PseudoElementType::After)
  );
}

#[test]
fn test_css_spec_default_display_inline() {
  // CSS 2.1: Pseudo-elements default to inline display
  let generator = PseudoElementGenerator::new();
  let style = PseudoElementStyle::with_string("Inline", default_style());

  let result = generator.generate_box(PseudoElementType::Before, &style);
  let box_node = result.unwrap();

  assert!(box_node.is_inline_level());
}

#[test]
fn test_css_spec_content_none_no_box() {
  // CSS 2.1 Section 12.2: content: none means no box is generated
  let generator = PseudoElementGenerator::new();
  let style = PseudoElementStyle::none(default_style());

  let result = generator.generate_box(PseudoElementType::Before, &style);
  assert!(result.is_none());
}

#[test]
fn test_css_spec_display_none_no_box() {
  // CSS 2.1: display: none means no box is generated
  let generator = PseudoElementGenerator::new();
  let style = PseudoElementStyle {
    content: PseudoContent::String("Hidden".to_string()),
    display: Display::None,
    computed: default_style(),
  };

  let result = generator.generate_box(PseudoElementType::Before, &style);
  assert!(result.is_none());
}
