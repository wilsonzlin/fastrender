//! Integration tests for positioned layout
//!
//! Tests CSS positioned layout (position: relative, absolute, fixed, sticky).
//!
//! CSS References:
//! - CSS 2.1 Section 9.3: Positioning schemes
//! - CSS 2.1 Section 10.3.7: Absolutely positioned width
//! - CSS 2.1 Section 10.6.4: Absolutely positioned height
//! - CSS Position Module Level 3: Sticky positioning

use fastrender::geometry::EdgeOffsets;
use fastrender::geometry::Point;
use fastrender::geometry::Rect;
use fastrender::geometry::Size;
use fastrender::layout::contexts::block::BlockFormattingContext;
use fastrender::layout::contexts::flex::FlexFormattingContext;
use fastrender::layout::contexts::grid::GridFormattingContext;
use fastrender::layout::contexts::inline::InlineFormattingContext;
use fastrender::paint::stacking::build_stacking_tree_from_fragment_tree;
use fastrender::paint::stacking::StackingContextReason;
use fastrender::style::display::FormattingContextType;
use fastrender::style::types::FontSizeAdjust;
use fastrender::text::font_loader::FontContext;
use fastrender::BoxNode;
use fastrender::ComputedStyle;
use fastrender::ContainingBlock;
use fastrender::Display;
use fastrender::FormattingContext;
use fastrender::FragmentContent;
use fastrender::FragmentNode;
use fastrender::LayoutConstraints;
use fastrender::Length;
use fastrender::LengthOrAuto;
use fastrender::LengthUnit;
use fastrender::Position;
use fastrender::PositionedLayout;
use fastrender::PositionedStyle;
use fastrender::StickyConstraints;
use std::sync::Arc;

// ============================================================================
// Test Fixtures
// ============================================================================

fn default_style() -> PositionedStyle {
  // Reset border width to 0 for cleaner test expectations
  PositionedStyle {
    border_width: EdgeOffsets::ZERO,
    ..Default::default()
  }
}

fn create_fragment(x: f32, y: f32, width: f32, height: f32) -> FragmentNode {
  FragmentNode::new_block(Rect::from_xywh(x, y, width, height), vec![])
}

fn create_containing_block(width: f32, height: f32) -> ContainingBlock {
  ContainingBlock::viewport(Size::new(width, height))
}

fn create_containing_block_at(x: f32, y: f32, width: f32, height: f32) -> ContainingBlock {
  ContainingBlock::new(Rect::from_xywh(x, y, width, height))
}

fn find_fragment_by_box_id<'a>(
  fragment: &'a FragmentNode,
  box_id: usize,
) -> Option<&'a FragmentNode> {
  let mut stack = vec![fragment];
  while let Some(node) = stack.pop() {
    let matches_id = match &node.content {
      FragmentContent::Block { box_id: Some(id) }
      | FragmentContent::Inline {
        box_id: Some(id), ..
      }
      | FragmentContent::Text {
        box_id: Some(id), ..
      }
      | FragmentContent::Replaced {
        box_id: Some(id), ..
      } => *id == box_id,
      _ => false,
    };
    if matches_id {
      return Some(node);
    }
    for child in node.children.iter() {
      stack.push(child);
    }
  }
  None
}

fn count_line_fragments(fragment: &FragmentNode) -> usize {
  fragment
    .iter_fragments()
    .filter(|f| matches!(f.content, FragmentContent::Line { .. }))
    .count()
}

fn max_text_right(fragment: &FragmentNode) -> f32 {
  fragment
    .iter_fragments()
    .filter(|f| matches!(f.content, FragmentContent::Text { .. }))
    .map(|f| f.bounds.max_x())
    .fold(fragment.bounds.max_x(), f32::max)
}

// ============================================================================
// ContainingBlock Tests
// ============================================================================

#[test]
fn test_containing_block_new() {
  let rect = Rect::from_xywh(10.0, 20.0, 300.0, 200.0);
  let cb = ContainingBlock::new(rect);

  assert_eq!(cb.width(), 300.0);
  assert_eq!(cb.height(), 200.0);
  assert_eq!(cb.origin(), Point::new(10.0, 20.0));
}

#[test]
fn test_containing_block_viewport() {
  let cb = ContainingBlock::viewport(Size::new(1920.0, 1080.0));

  assert_eq!(cb.width(), 1920.0);
  assert_eq!(cb.height(), 1080.0);
  assert_eq!(cb.origin(), Point::ZERO);
}

#[test]
fn test_containing_block_from_origin_size() {
  let origin = Point::new(50.0, 100.0);
  let size = Size::new(400.0, 300.0);
  let cb = ContainingBlock::from_origin_size(origin, size);

  assert_eq!(cb.width(), 400.0);
  assert_eq!(cb.height(), 300.0);
  assert_eq!(cb.origin(), origin);
}

// ============================================================================
// StickyConstraints Tests
// ============================================================================

#[test]
fn test_sticky_constraints_none() {
  let constraints = StickyConstraints::none();

  assert_eq!(constraints.top, None);
  assert_eq!(constraints.right, None);
  assert_eq!(constraints.bottom, None);
  assert_eq!(constraints.left, None);
  assert!(!constraints.has_constraints());
}

#[test]
fn test_sticky_constraints_from_style_with_values() {
  let mut style = default_style();
  style.position = Position::Sticky;
  style.top = LengthOrAuto::px(10.0);
  style.right = LengthOrAuto::percent(10.0);
  style.bottom = LengthOrAuto::Auto;
  style.left = LengthOrAuto::px(5.0);

  let cb = create_containing_block(800.0, 600.0);
  let constraints = StickyConstraints::from_style(&style, &cb, &FontContext::new());

  assert_eq!(constraints.top, Some(10.0));
  assert_eq!(constraints.right, Some(80.0)); // 10% of 800
  assert_eq!(constraints.bottom, None);
  assert_eq!(constraints.left, Some(5.0));
  assert!(constraints.has_constraints());
}

#[test]
fn test_sticky_constraints_has_constraints() {
  let mut constraints = StickyConstraints::none();
  assert!(!constraints.has_constraints());

  constraints.top = Some(0.0);
  assert!(constraints.has_constraints());
}

#[test]
fn sticky_constraints_use_font_metrics_for_relative_units() {
  let font_context = FontContext::new();
  let Some(font) = font_context.get_sans_serif() else {
    return;
  };
  let Ok(metrics) = font.metrics() else { return };
  let font_size = 20.0;
  let Some(x_height) = metrics.scale(font_size).x_height else {
    return;
  };

  let mut style = default_style();
  style.position = Position::Sticky;
  style.font_family = vec![font.family.clone()].into();
  style.font_size = font_size;
  style.root_font_size = font_size;
  style.top = LengthOrAuto::Length(Length::new(1.0, LengthUnit::Ex));

  let cb = create_containing_block(800.0, 600.0);
  let constraints = StickyConstraints::from_style(&style, &cb, &font_context);

  assert!((constraints.top.unwrap_or(0.0) - x_height).abs() < 1e-3);
}

#[test]
fn absolute_static_position_in_flex_respects_padding_once() {
  let mut container_style = ComputedStyle::default();
  container_style.display = Display::Flex;
  container_style.position = Position::Relative;
  container_style.padding_left = Length::px(12.0);
  container_style.padding_top = Length::px(6.0);
  container_style.padding_right = Length::px(0.0);
  container_style.padding_bottom = Length::px(0.0);
  container_style.border_left_width = Length::px(0.0);
  container_style.border_top_width = Length::px(0.0);
  container_style.border_right_width = Length::px(0.0);
  container_style.border_bottom_width = Length::px(0.0);

  let mut abs_style = ComputedStyle::default();
  abs_style.position = Position::Absolute;
  abs_style.width = Some(Length::px(10.0));
  abs_style.height = Some(Length::px(8.0));
  let abs_child = BoxNode::new_block(Arc::new(abs_style), FormattingContextType::Block, vec![]);

  let container = BoxNode::new_block(
    Arc::new(container_style),
    FormattingContextType::Flex,
    vec![abs_child],
  );
  let constraints = LayoutConstraints::definite(200.0, 100.0);
  let fc = FlexFormattingContext::new();
  let fragment = fc.layout(&container, &constraints).expect("flex layout");

  assert_eq!(
    fragment.children.len(),
    1,
    "absolute child should produce one fragment"
  );
  let abs_fragment = &fragment.children[0];
  assert!(
    (abs_fragment.bounds.x() - 12.0).abs() < 0.1,
    "static position should start at padding-left; got {}",
    abs_fragment.bounds.x()
  );
  assert!(
    (abs_fragment.bounds.y() - 6.0).abs() < 0.1,
    "static position should start at padding-top; got {}",
    abs_fragment.bounds.y()
  );
}

#[test]
fn absolute_static_position_in_empty_inline_respects_padding_once() {
  let mut inline_style = ComputedStyle::default();
  inline_style.display = Display::Inline;
  inline_style.position = Position::Relative;
  inline_style.padding_left = Length::px(10.0);
  inline_style.padding_top = Length::px(4.0);
  inline_style.padding_right = Length::px(0.0);
  inline_style.padding_bottom = Length::px(0.0);
  inline_style.border_left_width = Length::px(0.0);
  inline_style.border_top_width = Length::px(0.0);
  inline_style.border_right_width = Length::px(0.0);
  inline_style.border_bottom_width = Length::px(0.0);

  let mut abs_style = ComputedStyle::default();
  abs_style.position = Position::Absolute;
  abs_style.width = Some(Length::px(5.0));
  abs_style.height = Some(Length::px(5.0));
  let abs_child = BoxNode::new_inline(Arc::new(abs_style), vec![]);

  let inline = BoxNode::new_inline(Arc::new(inline_style), vec![abs_child]);
  let constraints = LayoutConstraints::definite_width(100.0);
  let ifc = InlineFormattingContext::new();
  let fragment = ifc.layout(&inline, &constraints).expect("inline layout");

  assert_eq!(
    fragment.children.len(),
    1,
    "absolute child should be laid out"
  );
  let abs_fragment = &fragment.children[0];
  assert!(
    (abs_fragment.bounds.x() - 10.0).abs() < 0.1,
    "static position should start at padding-left; got {}",
    abs_fragment.bounds.x()
  );
  assert!(
    (abs_fragment.bounds.y() - 4.0).abs() < 0.1,
    "static position should start at padding-top; got {}",
    abs_fragment.bounds.y()
  );
}

#[test]
fn absolute_inline_child_reflows_to_used_width() {
  let mut container_style = ComputedStyle::default();
  container_style.display = Display::Inline;
  container_style.position = Position::Relative;
  container_style.width = Some(Length::px(200.0));

  let mut abs_style = ComputedStyle::default();
  abs_style.position = Position::Absolute;
  abs_style.left = Some(Length::px(70.0));
  abs_style.right = Some(Length::px(70.0));
  abs_style.font_size = 10.0;
  let abs_style = Arc::new(abs_style);

  let mut text_style = ComputedStyle::default();
  text_style.font_size = 10.0;
  let text_style = Arc::new(text_style);

  let abs_id = 1usize;
  let text = BoxNode::new_text(
    text_style,
    "Positioned text should wrap correctly".to_string(),
  );
  let mut abs_child =
    BoxNode::new_block(abs_style.clone(), FormattingContextType::Inline, vec![text]);
  abs_child.id = abs_id;

  let mut container = BoxNode::new_block(
    Arc::new(container_style),
    FormattingContextType::Inline,
    vec![abs_child],
  );
  container.id = 10;

  let constraints = LayoutConstraints::definite_width(200.0);
  let fragment = InlineFormattingContext::new()
    .layout(&container, &constraints)
    .expect("inline layout");

  let abs_fragment = find_fragment_by_box_id(&fragment, abs_id).expect("abs fragment present");
  assert!(
    (abs_fragment.bounds.width() - 60.0).abs() < 0.5,
    "used width should subtract insets (got {:.1})",
    abs_fragment.bounds.width()
  );
  let line_count = count_line_fragments(abs_fragment);
  assert!(
    line_count >= 2,
    "positioned inline child should wrap when used width shrinks (lines={line_count})"
  );
  let text_right = max_text_right(abs_fragment);
  assert!(
    text_right <= abs_fragment.bounds.max_x() + 0.5,
    "text should not overflow used width (max_x {:.1} > {:.1})",
    text_right,
    abs_fragment.bounds.max_x()
  );
}

#[test]
fn absolute_flex_child_reflows_to_used_width() {
  let mut container_style = ComputedStyle::default();
  container_style.display = Display::Flex;
  container_style.position = Position::Relative;
  container_style.width = Some(Length::px(240.0));

  let mut filler_style = ComputedStyle::default();
  filler_style.display = Display::Block;
  filler_style.height = Some(Length::px(10.0));
  let filler = BoxNode::new_block(Arc::new(filler_style), FormattingContextType::Block, vec![]);

  let mut abs_style = ComputedStyle::default();
  abs_style.position = Position::Absolute;
  abs_style.left = Some(Length::px(60.0));
  abs_style.right = Some(Length::px(60.0));
  abs_style.font_size = 10.0;
  let abs_style = Arc::new(abs_style);

  let mut text_style = ComputedStyle::default();
  text_style.font_size = 10.0;
  let text_style = Arc::new(text_style);

  let abs_id = 2usize;
  let text = BoxNode::new_text(
    text_style,
    "Positioned text should wrap correctly".to_string(),
  );
  let mut abs_child =
    BoxNode::new_block(abs_style.clone(), FormattingContextType::Inline, vec![text]);
  abs_child.id = abs_id;

  let mut container = BoxNode::new_block(
    Arc::new(container_style),
    FormattingContextType::Flex,
    vec![filler, abs_child],
  );
  container.id = 11;

  let constraints = LayoutConstraints::definite(240.0, 200.0);
  let fragment = FlexFormattingContext::new()
    .layout(&container, &constraints)
    .expect("flex layout");

  let abs_fragment = find_fragment_by_box_id(&fragment, abs_id).expect("abs fragment present");
  assert!(
    (abs_fragment.bounds.width() - 120.0).abs() < 0.5,
    "used width should subtract left/right insets (got {:.1})",
    abs_fragment.bounds.width()
  );
  let line_count = count_line_fragments(abs_fragment);
  assert!(
    line_count >= 2,
    "positioned flex child should wrap when used width shrinks (lines={line_count})"
  );
  let text_right = max_text_right(abs_fragment);
  assert!(
    text_right <= abs_fragment.bounds.max_x() + 0.5,
    "text should not overflow used width (max_x {:.1} > {:.1})",
    text_right,
    abs_fragment.bounds.max_x()
  );
}

#[test]
fn absolute_grid_child_reflows_to_used_width() {
  let mut container_style = ComputedStyle::default();
  container_style.display = Display::Grid;
  container_style.position = Position::Relative;
  container_style.width = Some(Length::px(240.0));

  let mut filler_style = ComputedStyle::default();
  filler_style.display = Display::Block;
  filler_style.height = Some(Length::px(10.0));
  let filler = BoxNode::new_block(Arc::new(filler_style), FormattingContextType::Block, vec![]);

  let mut abs_style = ComputedStyle::default();
  abs_style.position = Position::Absolute;
  abs_style.left = Some(Length::px(60.0));
  abs_style.right = Some(Length::px(60.0));
  abs_style.font_size = 10.0;
  let abs_style = Arc::new(abs_style);

  let mut text_style = ComputedStyle::default();
  text_style.font_size = 10.0;
  let text_style = Arc::new(text_style);

  let abs_id = 3usize;
  let text = BoxNode::new_text(
    text_style,
    "Positioned text should wrap correctly".to_string(),
  );
  let mut abs_child =
    BoxNode::new_block(abs_style.clone(), FormattingContextType::Inline, vec![text]);
  abs_child.id = abs_id;

  let mut container = BoxNode::new_block(
    Arc::new(container_style),
    FormattingContextType::Grid,
    vec![filler, abs_child],
  );
  container.id = 12;

  let constraints = LayoutConstraints::definite(240.0, 200.0);
  let fragment = GridFormattingContext::new()
    .layout(&container, &constraints)
    .expect("grid layout");

  let abs_fragment = find_fragment_by_box_id(&fragment, abs_id).expect("abs fragment present");
  assert!(
    (abs_fragment.bounds.width() - 120.0).abs() < 0.5,
    "used width should subtract left/right insets (got {:.1})",
    abs_fragment.bounds.width()
  );
  let line_count = count_line_fragments(abs_fragment);
  assert!(
    line_count >= 2,
    "positioned grid child should wrap when used width shrinks (lines={line_count})"
  );
  let text_right = max_text_right(abs_fragment);
  assert!(
    text_right <= abs_fragment.bounds.max_x() + 0.5,
    "text should not overflow used width (max_x {:.1} > {:.1})",
    text_right,
    abs_fragment.bounds.max_x()
  );
}

// ============================================================================
// Relative Positioning Tests
// ============================================================================

#[test]
fn test_relative_position_zero_offset() {
  let layout = PositionedLayout::new();
  let fragment = create_fragment(50.0, 50.0, 100.0, 100.0);

  let mut style = default_style();
  style.position = Position::Relative;
  // All offsets default to auto

  let cb = create_containing_block(800.0, 600.0);
  let result = layout
    .apply_relative_positioning(&fragment, &style, &cb)
    .unwrap();

  // Position unchanged when all offsets are auto
  assert_eq!(result.bounds.x(), 50.0);
  assert_eq!(result.bounds.y(), 50.0);
  assert_eq!(result.bounds.width(), 100.0);
  assert_eq!(result.bounds.height(), 100.0);
}

#[test]
fn test_relative_position_top_left_positive() {
  let layout = PositionedLayout::new();
  let fragment = create_fragment(0.0, 0.0, 100.0, 100.0);

  let mut style = default_style();
  style.position = Position::Relative;
  style.top = LengthOrAuto::px(25.0);
  style.left = LengthOrAuto::px(50.0);

  let cb = create_containing_block(800.0, 600.0);
  let result = layout
    .apply_relative_positioning(&fragment, &style, &cb)
    .unwrap();

  assert_eq!(result.bounds.x(), 50.0);
  assert_eq!(result.bounds.y(), 25.0);
}

#[test]
fn test_relative_position_bottom_right_moves_opposite() {
  let layout = PositionedLayout::new();
  let fragment = create_fragment(100.0, 100.0, 100.0, 100.0);

  let mut style = default_style();
  style.position = Position::Relative;
  style.bottom = LengthOrAuto::px(20.0);
  style.right = LengthOrAuto::px(30.0);

  let cb = create_containing_block(800.0, 600.0);
  let result = layout
    .apply_relative_positioning(&fragment, &style, &cb)
    .unwrap();

  // Bottom/right move in opposite direction
  assert_eq!(result.bounds.x(), 70.0); // 100 - 30
  assert_eq!(result.bounds.y(), 80.0); // 100 - 20
}

#[test]
fn test_relative_position_top_wins_over_bottom() {
  let layout = PositionedLayout::new();
  let fragment = create_fragment(0.0, 0.0, 100.0, 100.0);

  let mut style = default_style();
  style.position = Position::Relative;
  style.top = LengthOrAuto::px(10.0);
  style.bottom = LengthOrAuto::px(999.0); // Should be ignored

  let cb = create_containing_block(800.0, 600.0);
  let result = layout
    .apply_relative_positioning(&fragment, &style, &cb)
    .unwrap();

  assert_eq!(result.bounds.y(), 10.0); // Uses top, not bottom
}

#[test]
fn test_relative_position_left_wins_over_right() {
  let layout = PositionedLayout::new();
  let fragment = create_fragment(0.0, 0.0, 100.0, 100.0);

  let mut style = default_style();
  style.position = Position::Relative;
  style.left = LengthOrAuto::px(15.0);
  style.right = LengthOrAuto::px(888.0); // Should be ignored

  let cb = create_containing_block(800.0, 600.0);
  let result = layout
    .apply_relative_positioning(&fragment, &style, &cb)
    .unwrap();

  assert_eq!(result.bounds.x(), 15.0); // Uses left, not right
}

#[test]
fn test_relative_position_percentage_values() {
  let layout = PositionedLayout::new();
  let fragment = create_fragment(0.0, 0.0, 100.0, 100.0);

  let mut style = default_style();
  style.position = Position::Relative;
  style.top = LengthOrAuto::percent(10.0); // 10% of 600 = 60
  style.left = LengthOrAuto::percent(25.0); // 25% of 800 = 200

  let cb = create_containing_block(800.0, 600.0);
  let result = layout
    .apply_relative_positioning(&fragment, &style, &cb)
    .unwrap();

  assert_eq!(result.bounds.x(), 200.0);
  assert_eq!(result.bounds.y(), 60.0);
}

#[test]
fn test_relative_position_negative_values() {
  let layout = PositionedLayout::new();
  let fragment = create_fragment(200.0, 200.0, 100.0, 100.0);

  let mut style = default_style();
  style.position = Position::Relative;
  style.top = LengthOrAuto::px(-50.0);
  style.left = LengthOrAuto::px(-75.0);

  let cb = create_containing_block(800.0, 600.0);
  let result = layout
    .apply_relative_positioning(&fragment, &style, &cb)
    .unwrap();

  assert_eq!(result.bounds.x(), 125.0); // 200 - 75
  assert_eq!(result.bounds.y(), 150.0); // 200 - 50
}

#[test]
fn relative_offsets_use_font_metrics_and_adjust() {
  let font_context = FontContext::new();
  let Some(font) = font_context.get_sans_serif() else {
    return;
  };
  let Ok(metrics) = font.metrics() else { return };
  let Some(aspect) = metrics.aspect_ratio() else {
    return;
  };
  let font_size = 18.0;
  let adjust = aspect * 1.25;
  let used_size = if aspect > 0.0 {
    font_size * (adjust / aspect)
  } else {
    font_size
  };
  let Some(x_height) = metrics.scale(used_size).x_height else {
    return;
  };

  let layout = PositionedLayout::new();
  let fragment = create_fragment(0.0, 0.0, 50.0, 50.0);

  let mut style = default_style();
  style.position = Position::Relative;
  style.font_family = vec![font.family.clone()].into();
  style.font_size = font_size;
  style.root_font_size = font_size;
  style.font_size_adjust = FontSizeAdjust::Number(adjust);
  style.top = LengthOrAuto::Length(Length::new(1.0, LengthUnit::Ex));

  let cb = create_containing_block(800.0, 600.0);
  let result = layout
    .apply_relative_positioning(&fragment, &style, &cb)
    .unwrap();

  assert!((result.bounds.y() - x_height).abs() < 1e-3);
}

#[test]
fn test_static_position_ignores_offsets() {
  let layout = PositionedLayout::new();
  let fragment = create_fragment(100.0, 100.0, 100.0, 100.0);

  let mut style = default_style();
  style.position = Position::Static;
  style.top = LengthOrAuto::px(50.0);
  style.left = LengthOrAuto::px(50.0);

  let cb = create_containing_block(800.0, 600.0);
  let result = layout
    .apply_relative_positioning(&fragment, &style, &cb)
    .unwrap();

  // Static position - offsets should NOT be applied
  assert_eq!(result.bounds.x(), 100.0);
  assert_eq!(result.bounds.y(), 100.0);
}

// ============================================================================
// Absolute Positioning Tests
// ============================================================================

#[test]
fn test_absolute_position_left_top_width_height() {
  let layout = PositionedLayout::new();

  let mut style = default_style();
  style.position = Position::Absolute;
  style.left = LengthOrAuto::px(20.0);
  style.top = LengthOrAuto::px(30.0);
  style.width = LengthOrAuto::px(150.0);
  style.height = LengthOrAuto::px(100.0);

  let cb = create_containing_block(800.0, 600.0);
  let intrinsic = Size::new(50.0, 50.0);

  let (pos, size) = layout
    .compute_absolute_position(&style, &cb, intrinsic)
    .unwrap();

  assert_eq!(pos.x, 20.0);
  assert_eq!(pos.y, 30.0);
  assert_eq!(size.width, 150.0);
  assert_eq!(size.height, 100.0);
}

#[test]
fn test_absolute_position_right_bottom() {
  let layout = PositionedLayout::new();

  let mut style = default_style();
  style.position = Position::Absolute;
  style.right = LengthOrAuto::px(50.0);
  style.bottom = LengthOrAuto::px(50.0);
  style.width = LengthOrAuto::px(100.0);
  style.height = LengthOrAuto::px(100.0);

  let cb = create_containing_block(400.0, 300.0);
  let intrinsic = Size::new(50.0, 50.0);

  let (pos, size) = layout
    .compute_absolute_position(&style, &cb, intrinsic)
    .unwrap();

  // x = 400 - 50 - 100 = 250
  // y = 300 - 50 - 100 = 150
  assert_eq!(pos.x, 250.0);
  assert_eq!(pos.y, 150.0);
  assert_eq!(size.width, 100.0);
  assert_eq!(size.height, 100.0);
}

#[test]
fn test_absolute_position_left_right_shrink_to_fit() {
  let layout = PositionedLayout::new();

  let mut style = default_style();
  style.position = Position::Absolute;
  style.left = LengthOrAuto::px(100.0);
  style.right = LengthOrAuto::px(100.0);
  // width is auto - fills the available space between insets

  let cb = create_containing_block(500.0, 300.0);
  let intrinsic = Size::new(50.0, 50.0);

  let (pos, size) = layout
    .compute_absolute_position(&style, &cb, intrinsic)
    .unwrap();

  // Width fills the available space: 500 - 100 - 100 = 300
  assert_eq!(pos.x, 100.0);
  assert_eq!(size.width, 300.0);
}

#[test]
fn test_absolute_position_top_bottom_stretch() {
  let layout = PositionedLayout::new();

  let mut style = default_style();
  style.position = Position::Absolute;
  style.top = LengthOrAuto::px(50.0);
  style.bottom = LengthOrAuto::px(50.0);
  // height is auto

  let cb = create_containing_block(400.0, 400.0);
  let intrinsic = Size::new(50.0, 50.0);

  let (pos, size) = layout
    .compute_absolute_position(&style, &cb, intrinsic)
    .unwrap();

  // Height = 400 - 50 - 50 = 300
  assert_eq!(pos.y, 50.0);
  assert_eq!(size.height, 300.0);
}

#[test]
fn test_absolute_position_uses_intrinsic_when_auto() {
  let layout = PositionedLayout::new();

  let style = default_style();
  // All positioning values default to auto

  let cb = create_containing_block(800.0, 600.0);
  let intrinsic = Size::new(200.0, 150.0);

  let (_pos, size) = layout
    .compute_absolute_position(&style, &cb, intrinsic)
    .unwrap();

  // Should use intrinsic size
  assert_eq!(size.width, 200.0);
  assert_eq!(size.height, 150.0);
}

#[test]
fn absolute_auto_height_uses_aspect_ratio_with_specified_width() {
  let layout = PositionedLayout::new();
  let mut style = default_style();
  style.position = Position::Absolute;
  style.width = LengthOrAuto::px(120.0);
  style.height = LengthOrAuto::Auto;
  style.aspect_ratio = fastrender::style::types::AspectRatio::Ratio(2.0);

  let cb = create_containing_block(300.0, 300.0);
  let intrinsic = Size::new(0.0, 0.0);

  let (_pos, size) = layout
    .compute_absolute_position(&style, &cb, intrinsic)
    .unwrap();
  assert!(
    (size.width - 120.0).abs() < 0.01,
    "width was {}",
    size.width
  );
  assert!(
    (size.height - 60.0).abs() < 0.01,
    "height was {}",
    size.height
  );
}

#[test]
fn absolute_auto_width_uses_aspect_ratio_with_specified_height() {
  let layout = PositionedLayout::new();
  let mut style = default_style();
  style.position = Position::Absolute;
  style.height = LengthOrAuto::px(50.0);
  style.width = LengthOrAuto::Auto;
  style.aspect_ratio = fastrender::style::types::AspectRatio::Ratio(2.0);

  let cb = create_containing_block(300.0, 300.0);
  let intrinsic = Size::new(0.0, 0.0);

  let (_pos, size) = layout
    .compute_absolute_position(&style, &cb, intrinsic)
    .unwrap();
  assert!(
    (size.height - 50.0).abs() < 0.01,
    "height was {}",
    size.height
  );
  assert!(
    (size.width - 100.0).abs() < 0.01,
    "width was {}",
    size.width
  );
}

#[test]
fn absolute_auto_both_uses_aspect_ratio_and_intrinsic() {
  let layout = PositionedLayout::new();
  let mut style = default_style();
  style.position = Position::Absolute;
  style.width = LengthOrAuto::Auto;
  style.height = LengthOrAuto::Auto;
  style.aspect_ratio = fastrender::style::types::AspectRatio::Ratio(2.0);

  let cb = create_containing_block(300.0, 300.0);
  let intrinsic = Size::new(80.0, 40.0);

  let (_pos, size) = layout
    .compute_absolute_position(&style, &cb, intrinsic)
    .unwrap();
  assert!((size.width - 80.0).abs() < 0.01, "width was {}", size.width);
  assert!(
    (size.height - 40.0).abs() < 0.01,
    "height was {}",
    size.height
  );
}

#[test]
fn test_absolute_position_overconstrained_ignores_right() {
  let layout = PositionedLayout::new();

  let mut style = default_style();
  style.position = Position::Absolute;
  style.left = LengthOrAuto::px(10.0);
  style.width = LengthOrAuto::px(100.0);
  style.right = LengthOrAuto::px(9999.0); // Should be ignored

  let cb = create_containing_block(400.0, 300.0);
  let intrinsic = Size::new(50.0, 50.0);

  let (pos, size) = layout
    .compute_absolute_position(&style, &cb, intrinsic)
    .unwrap();

  // Uses left and width, ignores right
  assert_eq!(pos.x, 10.0);
  assert_eq!(size.width, 100.0);
}

#[test]
fn absolute_position_auto_margins_center_between_insets() {
  let layout = PositionedLayout::new();

  let mut style = default_style();
  style.position = Position::Absolute;
  style.left = LengthOrAuto::px(0.0);
  style.right = LengthOrAuto::px(0.0);
  style.width = LengthOrAuto::px(100.0);
  style.margin_left_auto = true;
  style.margin_right_auto = true;

  let cb = create_containing_block(300.0, 200.0);
  let intrinsic = Size::new(10.0, 10.0);

  let (pos, size) = layout
    .compute_absolute_position(&style, &cb, intrinsic)
    .unwrap();

  assert!((pos.x - 100.0).abs() < 1e-3, "x was {}", pos.x);
  assert_eq!(size.width, 100.0);
}

#[test]
fn test_absolute_position_with_containing_block_offset() {
  let layout = PositionedLayout::new();

  let mut style = default_style();
  style.position = Position::Absolute;
  style.left = LengthOrAuto::px(10.0);
  style.top = LengthOrAuto::px(10.0);
  style.width = LengthOrAuto::px(50.0);
  style.height = LengthOrAuto::px(50.0);

  // Containing block at (100, 100)
  let cb = create_containing_block_at(100.0, 100.0, 400.0, 300.0);
  let intrinsic = Size::new(50.0, 50.0);

  let (pos, _size) = layout
    .compute_absolute_position(&style, &cb, intrinsic)
    .unwrap();

  // Position should be relative to containing block origin
  assert_eq!(pos.x, 110.0); // 100 + 10
  assert_eq!(pos.y, 110.0); // 100 + 10
}

// ============================================================================
// Containing Block Determination Tests
// ============================================================================

#[test]
fn test_determine_cb_static_uses_block_ancestor() {
  let layout = PositionedLayout::new();
  let viewport = Size::new(1024.0, 768.0);
  let block_rect = Rect::from_xywh(10.0, 10.0, 400.0, 300.0);

  let cb = layout.determine_containing_block(Position::Static, viewport, None, Some(block_rect));

  assert_eq!(cb.rect, block_rect);
}

#[test]
fn test_determine_cb_relative_uses_block_ancestor() {
  let layout = PositionedLayout::new();
  let viewport = Size::new(1024.0, 768.0);
  let block_rect = Rect::from_xywh(20.0, 20.0, 500.0, 400.0);

  let cb = layout.determine_containing_block(Position::Relative, viewport, None, Some(block_rect));

  assert_eq!(cb.rect, block_rect);
}

#[test]
fn test_determine_cb_absolute_uses_positioned_ancestor() {
  let layout = PositionedLayout::new();
  let viewport = Size::new(1024.0, 768.0);
  let positioned_rect = Rect::from_xywh(50.0, 50.0, 300.0, 200.0);
  let block = Some(Rect::from_xywh(10.0, 10.0, 800.0, 600.0));

  let cb =
    layout.determine_containing_block(Position::Absolute, viewport, Some(positioned_rect), block);

  // Absolute uses positioned ancestor, not block ancestor
  assert_eq!(cb.rect, positioned_rect);
}

#[test]
fn test_determine_cb_absolute_falls_back_to_viewport() {
  let layout = PositionedLayout::new();
  let viewport = Size::new(1024.0, 768.0);

  // No positioned ancestor
  let cb = layout.determine_containing_block(Position::Absolute, viewport, None, None);

  // Falls back to viewport (initial containing block)
  assert_eq!(cb.rect.size, viewport);
  assert_eq!(cb.rect.origin, Point::ZERO);
}

#[test]
fn test_determine_cb_fixed_always_uses_viewport() {
  let layout = PositionedLayout::new();
  let viewport = Size::new(1920.0, 1080.0);
  let positioned = Some(Rect::from_xywh(100.0, 100.0, 500.0, 500.0));
  let block = Some(Rect::from_xywh(50.0, 50.0, 800.0, 600.0));

  let cb = layout.determine_containing_block(Position::Fixed, viewport, positioned, block);

  // Fixed ALWAYS uses viewport regardless of ancestors
  assert_eq!(cb.rect.size, viewport);
  assert_eq!(cb.rect.origin, Point::ZERO);
}

#[test]
fn test_determine_cb_sticky_uses_block_ancestor() {
  let layout = PositionedLayout::new();
  let viewport = Size::new(1024.0, 768.0);
  let block_rect = Rect::from_xywh(30.0, 30.0, 600.0, 400.0);

  let cb = layout.determine_containing_block(Position::Sticky, viewport, None, Some(block_rect));

  // Sticky uses block container like relative
  assert_eq!(cb.rect, block_rect);
}

// ============================================================================
// Stacking Context Tests
// ============================================================================

#[test]
fn test_stacking_context_positioned_with_z_index() {
  let layout = PositionedLayout::new();

  let mut style = default_style();
  style.position = Position::Absolute;
  style.z_index = Some(10);

  assert!(layout.creates_stacking_context(&style));
}

#[test]
fn test_stacking_context_relative_with_z_index() {
  let layout = PositionedLayout::new();

  let mut style = default_style();
  style.position = Position::Relative;
  style.z_index = Some(0); // Even z-index: 0 creates stacking context

  assert!(layout.creates_stacking_context(&style));
}

#[test]
fn test_stacking_context_positioned_without_z_index() {
  let layout = PositionedLayout::new();

  let mut style = default_style();
  style.position = Position::Relative;
  // z_index is None

  assert!(!layout.creates_stacking_context(&style));
}

#[test]
fn test_stacking_context_opacity_less_than_one() {
  let layout = PositionedLayout::new();

  let mut style = default_style();
  style.opacity = 0.99;

  assert!(layout.creates_stacking_context(&style));
}

#[test]
fn test_stacking_context_opacity_one() {
  let layout = PositionedLayout::new();

  let mut style = default_style();
  style.opacity = 1.0;

  assert!(!layout.creates_stacking_context(&style));
}

#[test]
fn test_stacking_context_static_with_z_index() {
  let layout = PositionedLayout::new();

  let mut style = default_style();
  style.position = Position::Static;
  style.z_index = Some(5);

  // Static position does NOT create stacking context even with z-index
  assert!(!layout.creates_stacking_context(&style));
}

// ============================================================================
// Sticky Positioning Constraint Tests
// ============================================================================

#[test]
fn test_sticky_constraints_not_sticky() {
  let layout = PositionedLayout::new();

  let mut style = default_style();
  style.position = Position::Relative;
  style.top = LengthOrAuto::px(10.0);

  let cb = create_containing_block(800.0, 600.0);
  let constraints = layout.compute_sticky_constraints(&style, &cb);

  // Non-sticky elements return empty constraints
  assert!(!constraints.has_constraints());
}

#[test]
fn test_sticky_constraints_computed() {
  let layout = PositionedLayout::new();

  let mut style = default_style();
  style.position = Position::Sticky;
  style.top = LengthOrAuto::px(20.0);
  style.bottom = LengthOrAuto::percent(5.0);

  let cb = create_containing_block(1000.0, 800.0);
  let constraints = layout.compute_sticky_constraints(&style, &cb);

  assert_eq!(constraints.top, Some(20.0));
  assert_eq!(constraints.bottom, Some(40.0)); // 5% of 800
  assert!(constraints.has_constraints());
}

// ============================================================================
// Edge Cases and Error Handling Tests
// ============================================================================

#[test]
fn test_zero_size_containing_block() {
  let layout = PositionedLayout::new();

  let mut style = default_style();
  style.position = Position::Relative;
  style.top = LengthOrAuto::percent(50.0);
  style.left = LengthOrAuto::percent(50.0);

  let fragment = create_fragment(0.0, 0.0, 100.0, 100.0);
  let cb = create_containing_block(0.0, 0.0);

  let result = layout
    .apply_relative_positioning(&fragment, &style, &cb)
    .unwrap();

  // Percentages of 0 should be 0
  assert_eq!(result.bounds.x(), 0.0);
  assert_eq!(result.bounds.y(), 0.0);
}

#[test]
fn test_large_offset_values() {
  let layout = PositionedLayout::new();

  let mut style = default_style();
  style.position = Position::Relative;
  style.top = LengthOrAuto::px(1_000_000.0);
  style.left = LengthOrAuto::px(1_000_000.0);

  let fragment = create_fragment(0.0, 0.0, 100.0, 100.0);
  let cb = create_containing_block(800.0, 600.0);

  let result = layout
    .apply_relative_positioning(&fragment, &style, &cb)
    .unwrap();

  assert_eq!(result.bounds.x(), 1_000_000.0);
  assert_eq!(result.bounds.y(), 1_000_000.0);
}

#[test]
fn test_fragment_preserves_size_on_relative_position() {
  let layout = PositionedLayout::new();
  let fragment = create_fragment(50.0, 50.0, 200.0, 150.0);

  let mut style = default_style();
  style.position = Position::Relative;
  style.top = LengthOrAuto::px(100.0);
  style.left = LengthOrAuto::px(100.0);

  let cb = create_containing_block(800.0, 600.0);
  let result = layout
    .apply_relative_positioning(&fragment, &style, &cb)
    .unwrap();

  // Size should be preserved
  assert_eq!(result.bounds.width(), 200.0);
  assert_eq!(result.bounds.height(), 150.0);
}

#[test]
fn positioned_fragments_keep_computed_style_for_stacking() {
  let mut fixed_style = ComputedStyle::default();
  fixed_style.position = Position::Fixed;
  fixed_style.width = Some(Length::px(20.0));
  fixed_style.height = Some(Length::px(20.0));

  let fixed = BoxNode::new_block(Arc::new(fixed_style), FormattingContextType::Block, vec![]);
  let root = BoxNode::new_block(
    Arc::new(ComputedStyle::default()),
    FormattingContextType::Block,
    vec![fixed],
  );

  let constraints = LayoutConstraints::definite(200.0, 200.0);
  let fc = BlockFormattingContext::new();
  let fragment = fc.layout(&root, &constraints).expect("block layout");

  assert_eq!(
    fragment.children.len(),
    1,
    "fixed fragment should be present"
  );
  let fixed_fragment = &fragment.children[0];
  let style = fixed_fragment
    .style
    .as_ref()
    .expect("fragment should carry style");
  assert_eq!(style.position, Position::Fixed);

  let stacking = build_stacking_tree_from_fragment_tree(&fragment);
  assert!(stacking
    .children
    .iter()
    .any(|ctx| matches!(ctx.reason, StackingContextReason::FixedPositioning)));
}
