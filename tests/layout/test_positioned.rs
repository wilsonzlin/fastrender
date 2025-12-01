//! Integration tests for positioned layout
//!
//! Tests CSS positioned layout (position: relative, absolute, fixed, sticky).
//!
//! CSS References:
//! - CSS 2.1 Section 9.3: Positioning schemes
//! - CSS 2.1 Section 10.3.7: Absolutely positioned width
//! - CSS 2.1 Section 10.6.4: Absolutely positioned height
//! - CSS Position Module Level 3: Sticky positioning

use fastrender::geometry::{EdgeOffsets, Point, Rect, Size};
use fastrender::layout::{ContainingBlock, PositionedLayout, StickyConstraints};
use fastrender::style::{LengthOrAuto, Position, PositionedStyle};
use fastrender::FragmentNode;

// ============================================================================
// Test Fixtures
// ============================================================================

fn default_style() -> PositionedStyle {
    let mut style = PositionedStyle::default();
    // Reset border width to 0 for cleaner test expectations
    style.border_width = EdgeOffsets::ZERO;
    style
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
    let constraints = StickyConstraints::from_style(&style, &cb);

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
    let result = layout.apply_relative_positioning(&fragment, &style, &cb).unwrap();

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
    let result = layout.apply_relative_positioning(&fragment, &style, &cb).unwrap();

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
    let result = layout.apply_relative_positioning(&fragment, &style, &cb).unwrap();

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
    let result = layout.apply_relative_positioning(&fragment, &style, &cb).unwrap();

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
    let result = layout.apply_relative_positioning(&fragment, &style, &cb).unwrap();

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
    let result = layout.apply_relative_positioning(&fragment, &style, &cb).unwrap();

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
    let result = layout.apply_relative_positioning(&fragment, &style, &cb).unwrap();

    assert_eq!(result.bounds.x(), 125.0); // 200 - 75
    assert_eq!(result.bounds.y(), 150.0); // 200 - 50
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
    let result = layout.apply_relative_positioning(&fragment, &style, &cb).unwrap();

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

    let (pos, size) = layout.compute_absolute_position(&style, &cb, intrinsic).unwrap();

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

    let (pos, size) = layout.compute_absolute_position(&style, &cb, intrinsic).unwrap();

    // x = 400 - 50 - 100 = 250
    // y = 300 - 50 - 100 = 150
    assert_eq!(pos.x, 250.0);
    assert_eq!(pos.y, 150.0);
    assert_eq!(size.width, 100.0);
    assert_eq!(size.height, 100.0);
}

#[test]
fn test_absolute_position_left_right_stretch() {
    let layout = PositionedLayout::new();

    let mut style = default_style();
    style.position = Position::Absolute;
    style.left = LengthOrAuto::px(100.0);
    style.right = LengthOrAuto::px(100.0);
    // width is auto - should stretch

    let cb = create_containing_block(500.0, 300.0);
    let intrinsic = Size::new(50.0, 50.0);

    let (pos, size) = layout.compute_absolute_position(&style, &cb, intrinsic).unwrap();

    // Width = 500 - 100 - 100 = 300
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

    let (pos, size) = layout.compute_absolute_position(&style, &cb, intrinsic).unwrap();

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

    let (pos, size) = layout.compute_absolute_position(&style, &cb, intrinsic).unwrap();

    // Should use intrinsic size
    assert_eq!(size.width, 200.0);
    assert_eq!(size.height, 150.0);
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

    let (pos, size) = layout.compute_absolute_position(&style, &cb, intrinsic).unwrap();

    // Uses left and width, ignores right
    assert_eq!(pos.x, 10.0);
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

    let (pos, size) = layout.compute_absolute_position(&style, &cb, intrinsic).unwrap();

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
    let block = Some(Rect::from_xywh(10.0, 10.0, 400.0, 300.0));

    let cb = layout.determine_containing_block(Position::Static, viewport, None, block);

    assert_eq!(cb.rect, block.unwrap());
}

#[test]
fn test_determine_cb_relative_uses_block_ancestor() {
    let layout = PositionedLayout::new();
    let viewport = Size::new(1024.0, 768.0);
    let block = Some(Rect::from_xywh(20.0, 20.0, 500.0, 400.0));

    let cb = layout.determine_containing_block(Position::Relative, viewport, None, block);

    assert_eq!(cb.rect, block.unwrap());
}

#[test]
fn test_determine_cb_absolute_uses_positioned_ancestor() {
    let layout = PositionedLayout::new();
    let viewport = Size::new(1024.0, 768.0);
    let positioned = Some(Rect::from_xywh(50.0, 50.0, 300.0, 200.0));
    let block = Some(Rect::from_xywh(10.0, 10.0, 800.0, 600.0));

    let cb = layout.determine_containing_block(Position::Absolute, viewport, positioned, block);

    // Absolute uses positioned ancestor, not block ancestor
    assert_eq!(cb.rect, positioned.unwrap());
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
    let block = Some(Rect::from_xywh(30.0, 30.0, 600.0, 400.0));

    let cb = layout.determine_containing_block(Position::Sticky, viewport, None, block);

    // Sticky uses block container like relative
    assert_eq!(cb.rect, block.unwrap());
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

    let result = layout.apply_relative_positioning(&fragment, &style, &cb).unwrap();

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

    let result = layout.apply_relative_positioning(&fragment, &style, &cb).unwrap();

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
    let result = layout.apply_relative_positioning(&fragment, &style, &cb).unwrap();

    // Size should be preserved
    assert_eq!(result.bounds.width(), 200.0);
    assert_eq!(result.bounds.height(), 150.0);
}
