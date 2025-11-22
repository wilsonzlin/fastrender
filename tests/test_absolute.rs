//! Comprehensive tests for absolute positioning
//!
//! Tests CSS 2.1 Sections 10.3.7 and 10.6.4 for absolutely positioned elements.

use fastrender::geometry::{Point, Rect, Size};
use fastrender::layout::absolute_positioning::{
    compute_absolute_horizontal, compute_absolute_vertical, compute_centered_horizontal, compute_centered_vertical,
    get_initial_containing_block, is_out_of_flow, layout_absolute,
};
use fastrender::style::display::FormattingContextType;
use fastrender::style::{ComputedStyles, Length, Position};
use fastrender::tree::BoxNode;
use std::sync::Arc;

// ============================================================================
// Helper Functions
// ============================================================================

fn default_style() -> ComputedStyles {
    ComputedStyles::default()
}

fn make_box(style: ComputedStyles) -> BoxNode {
    BoxNode::new_block(Arc::new(style), FormattingContextType::Block, vec![])
}

fn make_absolute_box(
    left: Option<Length>,
    top: Option<Length>,
    right: Option<Length>,
    bottom: Option<Length>,
    width: Option<Length>,
    height: Option<Length>,
) -> BoxNode {
    let mut style = ComputedStyles::default();
    style.position = Position::Absolute;
    style.left = left;
    style.top = top;
    style.right = right;
    style.bottom = bottom;
    style.width = width;
    style.height = height;
    make_box(style)
}

// ============================================================================
// Basic Absolute Positioning Tests
// ============================================================================

#[test]
fn test_basic_absolute_positioning() {
    let box_node = make_absolute_box(
        Some(Length::px(10.0)),
        Some(Length::px(20.0)),
        None,
        None,
        Some(Length::px(100.0)),
        Some(Length::px(50.0)),
    );

    let containing_block = Rect::from_xywh(0.0, 0.0, 800.0, 600.0);
    let result = layout_absolute(&box_node, containing_block).unwrap();

    assert_eq!(result.fragment.bounds.x(), 10.0);
    assert_eq!(result.fragment.bounds.y(), 20.0);
    assert_eq!(result.fragment.bounds.width(), 100.0);
    assert_eq!(result.fragment.bounds.height(), 50.0);
}

#[test]
fn test_absolute_with_auto_width() {
    let box_node = make_absolute_box(
        Some(Length::px(50.0)),
        Some(Length::px(50.0)),
        Some(Length::px(50.0)),
        None,
        None, // Width should be computed
        Some(Length::px(100.0)),
    );

    let containing_block = Rect::from_xywh(0.0, 0.0, 400.0, 300.0);
    let result = layout_absolute(&box_node, containing_block).unwrap();

    // width = cb_width - left - right = 400 - 50 - 50 = 300
    assert_eq!(result.horizontal.width, 300.0);
}

#[test]
fn test_absolute_with_auto_height() {
    let box_node = make_absolute_box(
        Some(Length::px(0.0)),
        Some(Length::px(100.0)),
        None,
        Some(Length::px(100.0)),
        Some(Length::px(200.0)),
        None, // Height should be computed
    );

    let containing_block = Rect::from_xywh(0.0, 0.0, 800.0, 600.0);
    let result = layout_absolute(&box_node, containing_block).unwrap();

    // height = cb_height - top - bottom = 600 - 100 - 100 = 400
    assert_eq!(result.vertical.height, 400.0);
}

// ============================================================================
// Horizontal Constraint Equation Tests
// ============================================================================

#[test]
fn test_horizontal_left_and_width_specified() {
    let mut style = default_style();
    style.left = Some(Length::px(100.0));
    style.width = Some(Length::px(200.0));
    style.right = None;

    let result = compute_absolute_horizontal(&style, 800.0);

    assert_eq!(result.x, 100.0);
    assert_eq!(result.width, 200.0);
}

#[test]
fn test_horizontal_width_and_right_specified() {
    let mut style = default_style();
    style.left = None;
    style.width = Some(Length::px(200.0));
    style.right = Some(Length::px(100.0));

    let result = compute_absolute_horizontal(&style, 800.0);

    // x = cb_width - right - width = 800 - 100 - 200 = 500
    assert_eq!(result.x, 500.0);
    assert_eq!(result.width, 200.0);
}

#[test]
fn test_horizontal_left_and_right_specified_compute_width() {
    let mut style = default_style();
    style.left = Some(Length::px(150.0));
    style.right = Some(Length::px(150.0));
    style.width = None;

    let result = compute_absolute_horizontal(&style, 800.0);

    // width = cb_width - left - right = 800 - 150 - 150 = 500
    assert_eq!(result.x, 150.0);
    assert_eq!(result.width, 500.0);
}

#[test]
fn test_horizontal_overconstrained() {
    // All three specified - for LTR, right is ignored
    let mut style = default_style();
    style.left = Some(Length::px(50.0));
    style.width = Some(Length::px(300.0));
    style.right = Some(Length::px(50.0)); // Ignored in LTR

    let result = compute_absolute_horizontal(&style, 400.0);

    // Uses left and width, ignores right
    assert_eq!(result.x, 50.0);
    assert_eq!(result.width, 300.0);
}

#[test]
fn test_horizontal_only_width() {
    let mut style = default_style();
    style.left = None;
    style.width = Some(Length::px(200.0));
    style.right = None;

    let result = compute_absolute_horizontal(&style, 800.0);

    // Uses static position (0) for left
    assert_eq!(result.x, 0.0);
    assert_eq!(result.width, 200.0);
}

#[test]
fn test_horizontal_only_left() {
    let mut style = default_style();
    style.left = Some(Length::px(100.0));
    style.width = None;
    style.right = None;

    let result = compute_absolute_horizontal(&style, 800.0);

    assert_eq!(result.x, 100.0);
    // Should use shrink-to-fit width
    assert!(result.width > 0.0);
}

#[test]
fn test_horizontal_only_right() {
    let mut style = default_style();
    style.left = None;
    style.width = None;
    style.right = Some(Length::px(100.0));

    let result = compute_absolute_horizontal(&style, 800.0);

    // Should use shrink-to-fit width and compute left
    assert!(result.width > 0.0);
}

#[test]
fn test_horizontal_all_auto() {
    let style = default_style();

    let result = compute_absolute_horizontal(&style, 800.0);

    // Uses static position (0) and shrink-to-fit width
    assert_eq!(result.x, 0.0);
    assert!(result.width > 0.0);
}

// ============================================================================
// Vertical Constraint Equation Tests
// ============================================================================

#[test]
fn test_vertical_top_and_height_specified() {
    let mut style = default_style();
    style.top = Some(Length::px(50.0));
    style.height = Some(Length::px(200.0));
    style.bottom = None;

    let result = compute_absolute_vertical(&style, 600.0);

    assert_eq!(result.y, 50.0);
    assert_eq!(result.height, 200.0);
}

#[test]
fn test_vertical_height_and_bottom_specified() {
    let mut style = default_style();
    style.top = None;
    style.height = Some(Length::px(150.0));
    style.bottom = Some(Length::px(50.0));

    let result = compute_absolute_vertical(&style, 600.0);

    // y = cb_height - bottom - height = 600 - 50 - 150 = 400
    assert_eq!(result.y, 400.0);
    assert_eq!(result.height, 150.0);
}

#[test]
fn test_vertical_top_and_bottom_specified_compute_height() {
    let mut style = default_style();
    style.top = Some(Length::px(75.0));
    style.bottom = Some(Length::px(75.0));
    style.height = None;

    let result = compute_absolute_vertical(&style, 600.0);

    // height = cb_height - top - bottom = 600 - 75 - 75 = 450
    assert_eq!(result.y, 75.0);
    assert_eq!(result.height, 450.0);
}

#[test]
fn test_vertical_overconstrained() {
    let mut style = default_style();
    style.top = Some(Length::px(30.0));
    style.height = Some(Length::px(200.0));
    style.bottom = Some(Length::px(30.0)); // Ignored

    let result = compute_absolute_vertical(&style, 300.0);

    // Uses top and height, ignores bottom
    assert_eq!(result.y, 30.0);
    assert_eq!(result.height, 200.0);
}

#[test]
fn test_vertical_only_height() {
    let mut style = default_style();
    style.top = None;
    style.height = Some(Length::px(150.0));
    style.bottom = None;

    let result = compute_absolute_vertical(&style, 600.0);

    // Uses static position (0) for top
    assert_eq!(result.y, 0.0);
    assert_eq!(result.height, 150.0);
}

#[test]
fn test_vertical_only_top() {
    let mut style = default_style();
    style.top = Some(Length::px(100.0));
    style.height = None;
    style.bottom = None;

    let result = compute_absolute_vertical(&style, 600.0);

    assert_eq!(result.y, 100.0);
    // Should use auto height
    assert!(result.height > 0.0);
}

#[test]
fn test_vertical_only_bottom() {
    let mut style = default_style();
    style.top = None;
    style.height = None;
    style.bottom = Some(Length::px(100.0));

    let result = compute_absolute_vertical(&style, 600.0);

    // Should use auto height and compute top
    assert!(result.height > 0.0);
}

#[test]
fn test_vertical_all_auto() {
    let style = default_style();

    let result = compute_absolute_vertical(&style, 600.0);

    // Uses static position (0) and auto height
    assert_eq!(result.y, 0.0);
    assert!(result.height > 0.0);
}

// ============================================================================
// Percentage Value Tests
// ============================================================================

#[test]
fn test_percentage_left() {
    let mut style = default_style();
    style.left = Some(Length::percent(25.0)); // 25% of 800 = 200
    style.width = Some(Length::px(100.0));

    let result = compute_absolute_horizontal(&style, 800.0);

    assert_eq!(result.x, 200.0);
}

#[test]
fn test_percentage_width() {
    let mut style = default_style();
    style.left = Some(Length::px(100.0));
    style.width = Some(Length::percent(50.0)); // 50% of 800 = 400

    let result = compute_absolute_horizontal(&style, 800.0);

    assert_eq!(result.width, 400.0);
}

#[test]
fn test_percentage_top() {
    let mut style = default_style();
    style.top = Some(Length::percent(10.0)); // 10% of 600 = 60
    style.height = Some(Length::px(100.0));

    let result = compute_absolute_vertical(&style, 600.0);

    assert_eq!(result.y, 60.0);
}

#[test]
fn test_percentage_height() {
    let mut style = default_style();
    style.top = Some(Length::px(50.0));
    style.height = Some(Length::percent(75.0)); // 75% of 600 = 450

    let result = compute_absolute_vertical(&style, 600.0);

    assert_eq!(result.height, 450.0);
}

// ============================================================================
// Centering Tests
// ============================================================================

#[test]
fn test_horizontal_centering() {
    let mut style = default_style();
    style.left = Some(Length::px(0.0));
    style.right = Some(Length::px(0.0));
    style.width = Some(Length::px(200.0));
    // margins are None (auto)

    let result = compute_centered_horizontal(&style, 800.0);

    if let Some(h) = result {
        assert_eq!(h.width, 200.0);
        // Equal margins: (800 - 200) / 2 = 300
        assert_eq!(h.margin_left, 300.0);
        assert_eq!(h.margin_right, 300.0);
    }
}

#[test]
fn test_vertical_centering() {
    let mut style = default_style();
    style.top = Some(Length::px(0.0));
    style.bottom = Some(Length::px(0.0));
    style.height = Some(Length::px(100.0));

    let result = compute_centered_vertical(&style, 600.0);

    if let Some(v) = result {
        assert_eq!(v.height, 100.0);
        // Equal margins: (600 - 100) / 2 = 250
        assert_eq!(v.margin_top, 250.0);
        assert_eq!(v.margin_bottom, 250.0);
    }
}

#[test]
fn test_centered_box_both_dimensions() {
    let mut style = default_style();
    style.left = Some(Length::px(0.0));
    style.right = Some(Length::px(0.0));
    style.top = Some(Length::px(0.0));
    style.bottom = Some(Length::px(0.0));
    style.width = Some(Length::px(400.0));
    style.height = Some(Length::px(300.0));

    let h = compute_centered_horizontal(&style, 800.0);
    let v = compute_centered_vertical(&style, 600.0);

    if let (Some(h), Some(v)) = (h, v) {
        assert_eq!(h.width, 400.0);
        assert_eq!(v.height, 300.0);
        assert_eq!(h.margin_left, 200.0);
        assert_eq!(h.margin_right, 200.0);
        assert_eq!(v.margin_top, 150.0);
        assert_eq!(v.margin_bottom, 150.0);
    }
}

// ============================================================================
// Padding and Border Tests
// ============================================================================

#[test]
fn test_with_padding() {
    let mut style = default_style();
    style.left = Some(Length::px(50.0));
    style.width = Some(Length::px(200.0));
    style.padding_left = Length::px(20.0);

    let result = compute_absolute_horizontal(&style, 800.0);

    // x = left + margin + border + padding = 50 + 0 + 0 + 20 = 70
    assert_eq!(result.x, 70.0);
    assert_eq!(result.width, 200.0);
}

#[test]
fn test_with_border() {
    let mut style = default_style();
    style.left = Some(Length::px(50.0));
    style.width = Some(Length::px(200.0));
    style.border_left_width = Length::px(5.0);

    let result = compute_absolute_horizontal(&style, 800.0);

    // x = left + margin + border + padding = 50 + 0 + 5 + 0 = 55
    assert_eq!(result.x, 55.0);
}

#[test]
fn test_with_padding_and_border() {
    let mut style = default_style();
    style.left = Some(Length::px(100.0));
    style.width = Some(Length::px(300.0));
    style.padding_left = Length::px(15.0);
    style.border_left_width = Length::px(5.0);

    let result = compute_absolute_horizontal(&style, 800.0);

    // x = left + margin + border + padding = 100 + 0 + 5 + 15 = 120
    assert_eq!(result.x, 120.0);
    assert_eq!(result.width, 300.0);
}

#[test]
fn test_left_right_with_padding_computes_width() {
    let mut style = default_style();
    style.left = Some(Length::px(50.0));
    style.right = Some(Length::px(50.0));
    style.width = None;
    style.padding_left = Length::px(10.0);
    style.padding_right = Length::px(10.0);
    style.border_left_width = Length::px(5.0);
    style.border_right_width = Length::px(5.0);

    let result = compute_absolute_horizontal(&style, 800.0);

    // width = cb_width - left - right - padding - border
    // = 800 - 50 - 50 - 10 - 10 - 5 - 5 = 670
    let expected_width = 800.0 - 50.0 - 50.0 - 10.0 - 10.0 - 5.0 - 5.0;
    assert_eq!(result.width, expected_width);
}

// ============================================================================
// Containing Block Tests
// ============================================================================

#[test]
fn test_offset_containing_block() {
    let box_node = make_absolute_box(
        Some(Length::px(20.0)),
        Some(Length::px(20.0)),
        None,
        None,
        Some(Length::px(100.0)),
        Some(Length::px(100.0)),
    );

    // Containing block at (200, 150)
    let containing_block = Rect::from_xywh(200.0, 150.0, 400.0, 300.0);
    let result = layout_absolute(&box_node, containing_block).unwrap();

    // Position should be relative to containing block origin
    assert_eq!(result.fragment.bounds.x(), 220.0); // 200 + 20
    assert_eq!(result.fragment.bounds.y(), 170.0); // 150 + 20
}

#[test]
fn test_initial_containing_block() {
    let viewport = Size::new(1920.0, 1080.0);
    let icb = get_initial_containing_block(viewport);

    assert_eq!(icb.origin, Point::ZERO);
    assert_eq!(icb.size.width, 1920.0);
    assert_eq!(icb.size.height, 1080.0);
}

// ============================================================================
// Position Type Tests
// ============================================================================

#[test]
fn test_is_out_of_flow_absolute() {
    assert!(is_out_of_flow(Position::Absolute));
}

#[test]
fn test_is_out_of_flow_fixed() {
    assert!(is_out_of_flow(Position::Fixed));
}

#[test]
fn test_is_not_out_of_flow_static() {
    assert!(!is_out_of_flow(Position::Static));
}

#[test]
fn test_is_not_out_of_flow_relative() {
    assert!(!is_out_of_flow(Position::Relative));
}

#[test]
fn test_is_not_out_of_flow_sticky() {
    assert!(!is_out_of_flow(Position::Sticky));
}

// ============================================================================
// Edge Case Tests
// ============================================================================

#[test]
fn test_zero_width_containing_block() {
    let mut style = default_style();
    style.left = Some(Length::px(0.0));
    style.width = None;
    style.right = Some(Length::px(0.0));

    let result = compute_absolute_horizontal(&style, 0.0);

    // Width should be clamped to 0
    assert_eq!(result.width, 0.0);
}

#[test]
fn test_negative_computed_width_clamped() {
    let mut style = default_style();
    style.left = Some(Length::px(500.0));
    style.right = Some(Length::px(500.0));
    style.width = None;

    let result = compute_absolute_horizontal(&style, 800.0);

    // width = 800 - 500 - 500 = -200 → clamped to 0
    assert_eq!(result.width, 0.0);
}

#[test]
fn test_negative_computed_height_clamped() {
    let mut style = default_style();
    style.top = Some(Length::px(400.0));
    style.bottom = Some(Length::px(400.0));
    style.height = None;

    let result = compute_absolute_vertical(&style, 600.0);

    // height = 600 - 400 - 400 = -200 → clamped to 0
    assert_eq!(result.height, 0.0);
}

#[test]
fn test_very_large_values() {
    let mut style = default_style();
    style.left = Some(Length::px(0.0));
    style.width = Some(Length::px(1_000_000.0));

    let result = compute_absolute_horizontal(&style, 800.0);

    assert_eq!(result.width, 1_000_000.0);
}

#[test]
fn test_small_percentage() {
    let mut style = default_style();
    style.width = Some(Length::percent(0.5)); // 0.5% of 800 = 4

    let result = compute_absolute_horizontal(&style, 800.0);

    assert_eq!(result.width, 4.0);
}
