//! Comprehensive integration tests for absolute positioning
//!
//! Tests CSS absolute positioning algorithm (W3.T13).
//!
//! CSS References:
//! - CSS 2.1 Section 10.3.7: Absolutely positioned, non-replaced elements (width)
//! - CSS 2.1 Section 10.6.4: Absolutely positioned, non-replaced elements (height)
//! - CSS 2.1 Section 10.1: Definition of "containing block"

use fastrender::geometry::{EdgeOffsets, Point, Rect, Size};
use fastrender::FragmentNode;
use fastrender::{AbsoluteLayout, AbsoluteLayoutInput, AbsoluteLayoutResult, ContainingBlock, ResolvedMargins};
use fastrender::{LengthOrAuto, Position, PositionedStyle};

// ============================================================================
// Test Fixtures
// ============================================================================

fn default_style() -> PositionedStyle {
    PositionedStyle {
        border_width: EdgeOffsets::ZERO,
        ..Default::default()
    }
}

fn create_cb(width: f32, height: f32) -> ContainingBlock {
    ContainingBlock::viewport(Size::new(width, height))
}

fn create_cb_at(x: f32, y: f32, width: f32, height: f32) -> ContainingBlock {
    ContainingBlock::new(Rect::from_xywh(x, y, width, height))
}

fn create_cb_with_auto_block(width: f32) -> ContainingBlock {
    ContainingBlock::with_viewport_and_bases(
        Rect::from_xywh(0.0, 0.0, width, 0.0),
        Size::new(width, 0.0),
        Some(width),
        None,
    )
}

fn create_input(style: PositionedStyle, intrinsic: Size) -> AbsoluteLayoutInput {
    AbsoluteLayoutInput::new(style, intrinsic, Point::ZERO)
}

fn create_input_with_static(style: PositionedStyle, intrinsic: Size, static_pos: Point) -> AbsoluteLayoutInput {
    AbsoluteLayoutInput::new(style, intrinsic, static_pos)
}

// ============================================================================
// Basic Absolute Positioning Tests
// ============================================================================

#[test]
fn test_basic_absolute_all_offsets_and_size() {
    let layout = AbsoluteLayout::new();

    let mut style = default_style();
    style.position = Position::Absolute;
    style.left = LengthOrAuto::px(10.0);
    style.top = LengthOrAuto::px(20.0);
    style.width = LengthOrAuto::px(200.0);
    style.height = LengthOrAuto::px(150.0);

    let input = create_input(style, Size::new(100.0, 100.0));
    let cb = create_cb(800.0, 600.0);

    let result = layout.layout_absolute(&input, &cb).unwrap();

    assert_eq!(result.position.x, 10.0);
    assert_eq!(result.position.y, 20.0);
    assert_eq!(result.size.width, 200.0);
    assert_eq!(result.size.height, 150.0);
}

#[test]
fn test_basic_absolute_right_bottom_positioning() {
    let layout = AbsoluteLayout::new();

    let mut style = default_style();
    style.position = Position::Absolute;
    style.right = LengthOrAuto::px(30.0);
    style.bottom = LengthOrAuto::px(40.0);
    style.width = LengthOrAuto::px(100.0);
    style.height = LengthOrAuto::px(80.0);

    let input = create_input(style, Size::new(50.0, 50.0));
    let cb = create_cb(500.0, 400.0);

    let result = layout.layout_absolute(&input, &cb).unwrap();

    // x = 500 - 30 - 100 = 370
    // y = 400 - 40 - 80 = 280
    assert_eq!(result.position.x, 370.0);
    assert_eq!(result.position.y, 280.0);
    assert_eq!(result.size.width, 100.0);
    assert_eq!(result.size.height, 80.0);
}

#[test]
fn test_absolute_left_and_right_stretch_width() {
    let layout = AbsoluteLayout::new();

    let mut style = default_style();
    style.position = Position::Absolute;
    style.left = LengthOrAuto::px(50.0);
    style.right = LengthOrAuto::px(50.0);
    // width is auto - shrink-to-fit the intrinsic width between insets

    let input = create_input(style, Size::new(100.0, 100.0));
    let cb = create_cb(400.0, 300.0);

    let result = layout.layout_absolute(&input, &cb).unwrap();

    // Width = shrink-to-fit 100px within the 300px slot
    assert_eq!(result.position.x, 50.0);
    assert_eq!(result.size.width, 100.0);
}

#[test]
fn test_absolute_top_and_bottom_stretch_height() {
    let layout = AbsoluteLayout::new();

    let mut style = default_style();
    style.position = Position::Absolute;
    style.top = LengthOrAuto::px(75.0);
    style.bottom = LengthOrAuto::px(75.0);
    // height is auto

    let input = create_input(style, Size::new(100.0, 50.0));
    let cb = create_cb(800.0, 600.0);

    let result = layout.layout_absolute(&input, &cb).unwrap();

    // Height = 600 - 75 - 75 = 450
    assert_eq!(result.position.y, 75.0);
    assert_eq!(result.size.height, 450.0);
}

// ============================================================================
// Width/Height Auto Tests (Intrinsic Size)
// ============================================================================

#[test]
fn test_absolute_auto_width_uses_intrinsic() {
    let layout = AbsoluteLayout::new();

    let mut style = default_style();
    style.position = Position::Absolute;
    style.left = LengthOrAuto::px(25.0);
    // width auto

    let input = create_input(style, Size::new(180.0, 100.0));
    let cb = create_cb(800.0, 600.0);

    let result = layout.layout_absolute(&input, &cb).unwrap();

    assert_eq!(result.position.x, 25.0);
    assert_eq!(result.size.width, 180.0); // intrinsic
}

#[test]
fn test_absolute_auto_height_uses_intrinsic() {
    let layout = AbsoluteLayout::new();

    let mut style = default_style();
    style.position = Position::Absolute;
    style.top = LengthOrAuto::px(50.0);
    // height auto

    let input = create_input(style, Size::new(100.0, 120.0));
    let cb = create_cb(800.0, 600.0);

    let result = layout.layout_absolute(&input, &cb).unwrap();

    assert_eq!(result.position.y, 50.0);
    assert_eq!(result.size.height, 120.0); // intrinsic
}

#[test]
fn test_absolute_all_auto_uses_intrinsic_and_static_position() {
    let layout = AbsoluteLayout::new();

    let style = default_style();
    // All auto

    let input = create_input_with_static(style, Size::new(150.0, 100.0), Point::new(80.0, 120.0));
    let cb = create_cb(800.0, 600.0);

    let result = layout.layout_absolute(&input, &cb).unwrap();

    assert_eq!(result.position.x, 80.0); // static
    assert_eq!(result.position.y, 120.0); // static
    assert_eq!(result.size.width, 150.0);
    assert_eq!(result.size.height, 100.0);
}

#[test]
fn percent_offsets_ignore_auto_block_base() {
    let layout = AbsoluteLayout::new();

    let mut style = default_style();
    style.position = Position::Absolute;
    style.top = LengthOrAuto::Length(fastrender::Length::percent(50.0));
    style.height = LengthOrAuto::Length(fastrender::Length::percent(50.0));

    let intrinsic = Size::new(40.0, 30.0);
    let static_pos = Point::new(5.0, 10.0);
    let input = create_input_with_static(style, intrinsic, static_pos);
    let cb = create_cb_with_auto_block(200.0);

    let result = layout.layout_absolute(&input, &cb).unwrap();

    // With an auto block-size on the containing block, percentage top/height behave as auto.
    assert_eq!(result.position.y, 10.0);
    assert_eq!(result.size.height, 30.0);
}

// ============================================================================
// Constraint Equation Tests
// ============================================================================

#[test]
fn test_constraint_overconstrained_ignores_right() {
    let layout = AbsoluteLayout::new();

    let mut style = default_style();
    style.position = Position::Absolute;
    style.left = LengthOrAuto::px(20.0);
    style.width = LengthOrAuto::px(100.0);
    style.right = LengthOrAuto::px(9999.0); // Ignored for LTR

    let input = create_input(style, Size::new(50.0, 50.0));
    let cb = create_cb(400.0, 300.0);

    let result = layout.layout_absolute(&input, &cb).unwrap();

    assert_eq!(result.position.x, 20.0);
    assert_eq!(result.size.width, 100.0);
}

#[test]
fn test_constraint_overconstrained_ignores_bottom() {
    let layout = AbsoluteLayout::new();

    let mut style = default_style();
    style.position = Position::Absolute;
    style.top = LengthOrAuto::px(30.0);
    style.height = LengthOrAuto::px(80.0);
    style.bottom = LengthOrAuto::px(9999.0); // Ignored

    let input = create_input(style, Size::new(50.0, 50.0));
    let cb = create_cb(400.0, 300.0);

    let result = layout.layout_absolute(&input, &cb).unwrap();

    assert_eq!(result.position.y, 30.0);
    assert_eq!(result.size.height, 80.0);
}

#[test]
fn test_constraint_left_width_computes_position() {
    let layout = AbsoluteLayout::new();

    let mut style = default_style();
    style.left = LengthOrAuto::px(40.0);
    style.width = LengthOrAuto::px(150.0);
    // right is auto

    let input = create_input(style, Size::new(100.0, 100.0));
    let cb = create_cb(500.0, 400.0);

    let result = layout.layout_absolute(&input, &cb).unwrap();

    assert_eq!(result.position.x, 40.0);
    assert_eq!(result.size.width, 150.0);
}

#[test]
fn test_constraint_right_width_computes_left() {
    let layout = AbsoluteLayout::new();

    let mut style = default_style();
    style.right = LengthOrAuto::px(60.0);
    style.width = LengthOrAuto::px(120.0);
    // left is auto

    let input = create_input(style, Size::new(100.0, 100.0));
    let cb = create_cb(500.0, 400.0);

    let result = layout.layout_absolute(&input, &cb).unwrap();

    // x = 500 - 60 - 120 = 320
    assert_eq!(result.position.x, 320.0);
    assert_eq!(result.size.width, 120.0);
}

// ============================================================================
// Static Position Tests
// ============================================================================

#[test]
fn test_static_position_with_only_width() {
    let layout = AbsoluteLayout::new();

    let mut style = default_style();
    style.width = LengthOrAuto::px(200.0);
    // left, right auto

    let input = create_input_with_static(style, Size::new(100.0, 100.0), Point::new(50.0, 100.0));
    let cb = create_cb(800.0, 600.0);

    let result = layout.layout_absolute(&input, &cb).unwrap();

    // Uses static position for x
    assert_eq!(result.position.x, 50.0);
    assert_eq!(result.size.width, 200.0);
}

#[test]
fn test_static_position_with_only_height() {
    let layout = AbsoluteLayout::new();

    let mut style = default_style();
    style.height = LengthOrAuto::px(150.0);
    // top, bottom auto

    let input = create_input_with_static(style, Size::new(100.0, 100.0), Point::new(30.0, 80.0));
    let cb = create_cb(800.0, 600.0);

    let result = layout.layout_absolute(&input, &cb).unwrap();

    // Uses static position for y
    assert_eq!(result.position.y, 80.0);
    assert_eq!(result.size.height, 150.0);
}

// ============================================================================
// Percentage Value Tests
// ============================================================================

#[test]
fn test_percentage_left_and_top() {
    let layout = AbsoluteLayout::new();

    let mut style = default_style();
    style.left = LengthOrAuto::percent(10.0); // 10% of 500 = 50
    style.top = LengthOrAuto::percent(25.0); // 25% of 400 = 100
    style.width = LengthOrAuto::px(100.0);
    style.height = LengthOrAuto::px(80.0);

    let input = create_input(style, Size::new(50.0, 50.0));
    let cb = create_cb(500.0, 400.0);

    let result = layout.layout_absolute(&input, &cb).unwrap();

    assert_eq!(result.position.x, 50.0);
    assert_eq!(result.position.y, 100.0);
}

#[test]
fn test_percentage_right_and_bottom() {
    let layout = AbsoluteLayout::new();

    let mut style = default_style();
    style.right = LengthOrAuto::percent(20.0); // 20% of 400 = 80
    style.bottom = LengthOrAuto::percent(10.0); // 10% of 300 = 30
    style.width = LengthOrAuto::px(100.0);
    style.height = LengthOrAuto::px(50.0);

    let input = create_input(style, Size::new(50.0, 50.0));
    let cb = create_cb(400.0, 300.0);

    let result = layout.layout_absolute(&input, &cb).unwrap();

    // x = 400 - 80 - 100 = 220
    // y = 300 - 30 - 50 = 220
    assert_eq!(result.position.x, 220.0);
    assert_eq!(result.position.y, 220.0);
}

#[test]
fn test_percentage_width_and_height() {
    let layout = AbsoluteLayout::new();

    let mut style = default_style();
    style.left = LengthOrAuto::px(0.0);
    style.top = LengthOrAuto::px(0.0);
    style.width = LengthOrAuto::percent(50.0); // 50% of 800 = 400
    style.height = LengthOrAuto::percent(25.0); // 25% of 600 = 150

    let input = create_input(style, Size::new(100.0, 100.0));
    let cb = create_cb(800.0, 600.0);

    let result = layout.layout_absolute(&input, &cb).unwrap();

    assert_eq!(result.size.width, 400.0);
    assert_eq!(result.size.height, 150.0);
}

// ============================================================================
// Containing Block Offset Tests
// ============================================================================

#[test]
fn test_containing_block_at_offset() {
    let layout = AbsoluteLayout::new();

    let mut style = default_style();
    style.left = LengthOrAuto::px(15.0);
    style.top = LengthOrAuto::px(25.0);
    style.width = LengthOrAuto::px(100.0);
    style.height = LengthOrAuto::px(80.0);

    let input = create_input(style, Size::new(50.0, 50.0));
    // Containing block at (100, 150)
    let cb = create_cb_at(100.0, 150.0, 400.0, 300.0);

    let result = layout.layout_absolute(&input, &cb).unwrap();

    // Position should be relative to CB origin
    assert_eq!(result.position.x, 115.0); // 100 + 15
    assert_eq!(result.position.y, 175.0); // 150 + 25
}

#[test]
fn test_containing_block_right_bottom_at_offset() {
    let layout = AbsoluteLayout::new();

    let mut style = default_style();
    style.right = LengthOrAuto::px(50.0);
    style.bottom = LengthOrAuto::px(30.0);
    style.width = LengthOrAuto::px(100.0);
    style.height = LengthOrAuto::px(60.0);

    let input = create_input(style, Size::new(50.0, 50.0));
    let cb = create_cb_at(200.0, 100.0, 400.0, 300.0);

    let result = layout.layout_absolute(&input, &cb).unwrap();

    // x = 200 + (400 - 50 - 100) = 200 + 250 = 450
    // y = 100 + (300 - 30 - 60) = 100 + 210 = 310
    assert_eq!(result.position.x, 450.0);
    assert_eq!(result.position.y, 310.0);
}

// ============================================================================
// Edge Cases Tests
// ============================================================================

#[test]
fn test_zero_size_containing_block() {
    let layout = AbsoluteLayout::new();

    let mut style = default_style();
    style.left = LengthOrAuto::percent(50.0); // 50% of 0 = 0
    style.top = LengthOrAuto::percent(50.0);

    let input = create_input(style, Size::new(100.0, 80.0));
    let cb = create_cb(0.0, 0.0);

    let result = layout.layout_absolute(&input, &cb).unwrap();

    assert_eq!(result.position.x, 0.0);
    assert_eq!(result.position.y, 0.0);
}

#[test]
fn test_negative_computed_width_clamped() {
    let layout = AbsoluteLayout::new();

    let mut style = default_style();
    style.left = LengthOrAuto::px(200.0);
    style.right = LengthOrAuto::px(200.0);
    // In 300px CB, width would be -100

    let input = create_input(style, Size::new(50.0, 50.0));
    let cb = create_cb(300.0, 300.0);

    let result = layout.layout_absolute(&input, &cb).unwrap();

    // Width should shrink to the intrinsic 50px rather than clamping to 0
    assert_eq!(result.size.width, 50.0);
}

#[test]
fn test_negative_computed_height_clamped() {
    let layout = AbsoluteLayout::new();

    let mut style = default_style();
    style.top = LengthOrAuto::px(150.0);
    style.bottom = LengthOrAuto::px(200.0);
    // In 300px CB, height would be -50

    let input = create_input(style, Size::new(50.0, 50.0));
    let cb = create_cb(300.0, 300.0);

    let result = layout.layout_absolute(&input, &cb).unwrap();

    // Height should be clamped to 0
    assert_eq!(result.size.height, 0.0);
}

#[test]
fn test_very_large_values() {
    let layout = AbsoluteLayout::new();

    let mut style = default_style();
    style.left = LengthOrAuto::px(1_000_000.0);
    style.top = LengthOrAuto::px(2_000_000.0);
    style.width = LengthOrAuto::px(500_000.0);
    style.height = LengthOrAuto::px(300_000.0);

    let input = create_input(style, Size::new(100.0, 100.0));
    let cb = create_cb(800.0, 600.0);

    let result = layout.layout_absolute(&input, &cb).unwrap();

    assert_eq!(result.position.x, 1_000_000.0);
    assert_eq!(result.position.y, 2_000_000.0);
    assert_eq!(result.size.width, 500_000.0);
    assert_eq!(result.size.height, 300_000.0);
}

#[test]
fn test_zero_offset_values() {
    let layout = AbsoluteLayout::new();

    let mut style = default_style();
    style.left = LengthOrAuto::px(0.0);
    style.top = LengthOrAuto::px(0.0);
    style.width = LengthOrAuto::px(100.0);
    style.height = LengthOrAuto::px(100.0);

    let input = create_input(style, Size::new(50.0, 50.0));
    let cb = create_cb(800.0, 600.0);

    let result = layout.layout_absolute(&input, &cb).unwrap();

    assert_eq!(result.position.x, 0.0);
    assert_eq!(result.position.y, 0.0);
}

// ============================================================================
// Fragment Creation Tests
// ============================================================================

#[test]
fn test_create_fragment_from_result() {
    let layout = AbsoluteLayout::new();

    let result = AbsoluteLayoutResult {
        position: Point::new(100.0, 200.0),
        size: Size::new(150.0, 120.0),
        margins: ResolvedMargins::zero(),
    };

    let fragment = layout.create_fragment(&result, vec![]);

    assert_eq!(fragment.bounds.x(), 100.0);
    assert_eq!(fragment.bounds.y(), 200.0);
    assert_eq!(fragment.bounds.width(), 150.0);
    assert_eq!(fragment.bounds.height(), 120.0);
}

#[test]
fn test_create_fragment_with_children() {
    let layout = AbsoluteLayout::new();

    let result = AbsoluteLayoutResult {
        position: Point::new(50.0, 50.0),
        size: Size::new(200.0, 200.0),
        margins: ResolvedMargins::zero(),
    };

    let child = FragmentNode::new_block(Rect::from_xywh(10.0, 10.0, 50.0, 50.0), vec![]);

    let fragment = layout.create_fragment(&result, vec![child]);

    assert_eq!(fragment.children.len(), 1);
    assert_eq!(fragment.bounds.width(), 200.0);
}

// ============================================================================
// ResolvedMargins Tests
// ============================================================================

#[test]
fn test_resolved_margins_new() {
    let margins = ResolvedMargins::new(5.0, 10.0, 15.0, 20.0);

    assert_eq!(margins.top, 5.0);
    assert_eq!(margins.right, 10.0);
    assert_eq!(margins.bottom, 15.0);
    assert_eq!(margins.left, 20.0);
}

#[test]
fn test_resolved_margins_zero() {
    let margins = ResolvedMargins::zero();

    assert_eq!(margins.horizontal(), 0.0);
    assert_eq!(margins.vertical(), 0.0);
}

#[test]
fn test_resolved_margins_horizontal_vertical() {
    let margins = ResolvedMargins::new(10.0, 20.0, 30.0, 40.0);

    assert_eq!(margins.horizontal(), 60.0); // 20 + 40
    assert_eq!(margins.vertical(), 40.0); // 10 + 30
}

// ============================================================================
// Static Method Tests
// ============================================================================

#[test]
fn test_is_absolutely_positioned() {
    let mut style = default_style();

    style.position = Position::Static;
    assert!(!AbsoluteLayout::is_absolutely_positioned(&style));

    style.position = Position::Relative;
    assert!(!AbsoluteLayout::is_absolutely_positioned(&style));

    style.position = Position::Absolute;
    assert!(AbsoluteLayout::is_absolutely_positioned(&style));

    style.position = Position::Fixed;
    assert!(AbsoluteLayout::is_absolutely_positioned(&style));

    style.position = Position::Sticky;
    assert!(!AbsoluteLayout::is_absolutely_positioned(&style));
}

#[test]
fn test_should_layout_absolute() {
    let mut style = default_style();

    style.position = Position::Absolute;
    assert!(AbsoluteLayout::should_layout_absolute(&style));

    style.position = Position::Fixed;
    assert!(AbsoluteLayout::should_layout_absolute(&style));

    style.position = Position::Relative;
    assert!(!AbsoluteLayout::should_layout_absolute(&style));
}

// ============================================================================
// Combination Tests (Complex Scenarios)
// ============================================================================

#[test]
fn test_complex_all_sides_and_size() {
    let layout = AbsoluteLayout::new();

    let mut style = default_style();
    style.position = Position::Absolute;
    style.left = LengthOrAuto::px(10.0);
    style.top = LengthOrAuto::px(20.0);
    style.right = LengthOrAuto::px(30.0); // Ignored (overconstrained)
    style.bottom = LengthOrAuto::px(40.0); // Ignored (overconstrained)
    style.width = LengthOrAuto::px(100.0);
    style.height = LengthOrAuto::px(80.0);

    let input = create_input(style, Size::new(50.0, 50.0));
    let cb = create_cb(500.0, 400.0);

    let result = layout.layout_absolute(&input, &cb).unwrap();

    // Should use left, top, width, height - ignoring right, bottom
    assert_eq!(result.position.x, 10.0);
    assert_eq!(result.position.y, 20.0);
    assert_eq!(result.size.width, 100.0);
    assert_eq!(result.size.height, 80.0);
}

#[test]
fn test_fixed_positioning_uses_viewport() {
    let layout = AbsoluteLayout::new();

    let mut style = default_style();
    style.position = Position::Fixed;
    style.left = LengthOrAuto::px(0.0);
    style.top = LengthOrAuto::px(0.0);
    style.width = LengthOrAuto::px(100.0);
    style.height = LengthOrAuto::px(50.0);

    let input = create_input(style, Size::new(50.0, 50.0));
    // For fixed, the containing block IS the viewport
    let cb = create_cb(1920.0, 1080.0);

    let result = layout.layout_absolute(&input, &cb).unwrap();

    assert_eq!(result.position.x, 0.0);
    assert_eq!(result.position.y, 0.0);
}

#[test]
fn test_mixed_percentages_and_pixels() {
    let layout = AbsoluteLayout::new();

    let mut style = default_style();
    style.position = Position::Absolute;
    style.left = LengthOrAuto::percent(10.0); // 10% of 600 = 60
    style.top = LengthOrAuto::px(50.0);
    style.width = LengthOrAuto::px(200.0);
    style.height = LengthOrAuto::percent(20.0); // 20% of 400 = 80

    let input = create_input(style, Size::new(100.0, 100.0));
    let cb = create_cb(600.0, 400.0);

    let result = layout.layout_absolute(&input, &cb).unwrap();

    assert_eq!(result.position.x, 60.0);
    assert_eq!(result.position.y, 50.0);
    assert_eq!(result.size.width, 200.0);
    assert_eq!(result.size.height, 80.0);
}
