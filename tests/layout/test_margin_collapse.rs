//! Comprehensive tests for margin collapsing algorithm
//!
//! These tests verify the implementation of CSS 2.1 Section 8.3.1.
//!
//! # Test Categories
//!
//! 1. **Basic Collapse**: Positive, negative, and mixed margins
//! 2. **Empty Blocks**: Margins collapsing through empty elements
//! 3. **Sibling Collapse**: Adjacent sibling margin collapse
//! 4. **Parent-Child Collapse**: Conditions for parent-child collapse
//! 5. **BFC Boundaries**: Margin isolation at BFC boundaries
//! 6. **Clearance**: Margin collapse prevention with clearance
//! 7. **Edge Cases**: Extreme values and unusual combinations

use fastrender::layout::contexts::block::margin_collapse::{
    CollapsibleMargin, Float, MarginCollapseContext, Overflow, ParentChildCollapseRules,
    style_establishes_bfc,
};
use fastrender::style::Position;

// =============================================================================
// Basic CollapsibleMargin Tests
// =============================================================================

mod collapsible_margin {
    use super::*;

    #[test]
    fn zero_margin_is_zero() {
        let m = CollapsibleMargin::ZERO;
        assert!(m.is_zero());
        assert_eq!(m.resolve(), 0.0);
        assert_eq!(m.positive, 0.0);
        assert_eq!(m.negative, 0.0);
    }

    #[test]
    fn from_margin_positive() {
        let m = CollapsibleMargin::from_margin(25.0);
        assert_eq!(m.positive, 25.0);
        assert_eq!(m.negative, 0.0);
        assert_eq!(m.resolve(), 25.0);
    }

    #[test]
    fn from_margin_negative() {
        let m = CollapsibleMargin::from_margin(-17.5);
        assert_eq!(m.positive, 0.0);
        assert_eq!(m.negative, 17.5);
        assert_eq!(m.resolve(), -17.5);
    }

    #[test]
    fn from_margin_zero_value() {
        let m = CollapsibleMargin::from_margin(0.0);
        assert!(m.is_zero());
    }

    #[test]
    fn new_with_components() {
        let m = CollapsibleMargin::new(40.0, 15.0);
        assert_eq!(m.positive, 40.0);
        assert_eq!(m.negative, 15.0);
        assert_eq!(m.resolve(), 25.0);
    }

    #[test]
    fn default_is_zero() {
        let m = CollapsibleMargin::default();
        assert!(m.is_zero());
    }
}

// =============================================================================
// Positive Margin Collapse Tests
// =============================================================================

mod positive_margins {
    use super::*;

    #[test]
    fn two_positive_take_max() {
        let m1 = CollapsibleMargin::from_margin(20.0);
        let m2 = CollapsibleMargin::from_margin(30.0);
        let result = m1.collapse_with(m2);
        assert_eq!(result.resolve(), 30.0);
    }

    #[test]
    fn smaller_with_larger() {
        let m1 = CollapsibleMargin::from_margin(10.0);
        let m2 = CollapsibleMargin::from_margin(50.0);
        let result = m1.collapse_with(m2);
        assert_eq!(result.resolve(), 50.0);
    }

    #[test]
    fn equal_positive() {
        let m1 = CollapsibleMargin::from_margin(25.0);
        let m2 = CollapsibleMargin::from_margin(25.0);
        let result = m1.collapse_with(m2);
        assert_eq!(result.resolve(), 25.0);
    }

    #[test]
    fn three_positive() {
        let m1 = CollapsibleMargin::from_margin(10.0);
        let m2 = CollapsibleMargin::from_margin(20.0);
        let m3 = CollapsibleMargin::from_margin(15.0);
        let result = m1.collapse_with(m2).collapse_with(m3);
        assert_eq!(result.resolve(), 20.0);
    }

    #[test]
    fn five_positive() {
        let margins = [10.0, 20.0, 30.0, 25.0, 15.0];
        let mut result = CollapsibleMargin::ZERO;
        for m in margins {
            result = result.add_margin(m);
        }
        assert_eq!(result.resolve(), 30.0);
    }
}

// =============================================================================
// Negative Margin Collapse Tests
// =============================================================================

mod negative_margins {
    use super::*;

    #[test]
    fn two_negative_take_most_negative() {
        let m1 = CollapsibleMargin::from_margin(-20.0);
        let m2 = CollapsibleMargin::from_margin(-30.0);
        let result = m1.collapse_with(m2);
        assert_eq!(result.resolve(), -30.0);
    }

    #[test]
    fn smaller_negative_with_larger() {
        let m1 = CollapsibleMargin::from_margin(-5.0);
        let m2 = CollapsibleMargin::from_margin(-40.0);
        let result = m1.collapse_with(m2);
        assert_eq!(result.resolve(), -40.0);
    }

    #[test]
    fn equal_negative() {
        let m1 = CollapsibleMargin::from_margin(-15.0);
        let m2 = CollapsibleMargin::from_margin(-15.0);
        let result = m1.collapse_with(m2);
        assert_eq!(result.resolve(), -15.0);
    }

    #[test]
    fn three_negative() {
        let m1 = CollapsibleMargin::from_margin(-10.0);
        let m2 = CollapsibleMargin::from_margin(-25.0);
        let m3 = CollapsibleMargin::from_margin(-15.0);
        let result = m1.collapse_with(m2).collapse_with(m3);
        assert_eq!(result.resolve(), -25.0);
    }
}

// =============================================================================
// Mixed Margin Collapse Tests
// =============================================================================

mod mixed_margins {
    use super::*;

    #[test]
    fn positive_and_negative_sum() {
        let m1 = CollapsibleMargin::from_margin(30.0);
        let m2 = CollapsibleMargin::from_margin(-10.0);
        let result = m1.collapse_with(m2);
        assert_eq!(result.resolve(), 20.0); // 30 + (-10) = 20
    }

    #[test]
    fn larger_negative_wins() {
        let m1 = CollapsibleMargin::from_margin(20.0);
        let m2 = CollapsibleMargin::from_margin(-30.0);
        let result = m1.collapse_with(m2);
        assert_eq!(result.resolve(), -10.0); // 20 + (-30) = -10
    }

    #[test]
    fn equal_and_opposite() {
        let m1 = CollapsibleMargin::from_margin(25.0);
        let m2 = CollapsibleMargin::from_margin(-25.0);
        let result = m1.collapse_with(m2);
        assert_eq!(result.resolve(), 0.0);
    }

    #[test]
    fn multiple_mixed() {
        // 20, -10, 30, -5 => max(20,30) - max(10,5) = 30 - 10 = 20
        let margins = [20.0, -10.0, 30.0, -5.0];
        let mut result = CollapsibleMargin::ZERO;
        for m in margins {
            result = result.add_margin(m);
        }
        assert_eq!(result.resolve(), 20.0);
    }

    #[test]
    fn many_mixed_margins() {
        // Simulate complex collapse scenario
        let margins = [10.0, -5.0, 20.0, -15.0, 30.0, -10.0, 25.0, -20.0];
        let mut result = CollapsibleMargin::ZERO;
        for m in margins {
            result = result.add_margin(m);
        }
        // max positive = 30, max negative = 20
        assert_eq!(result.resolve(), 10.0);
    }
}

// =============================================================================
// MarginCollapseContext Tests
// =============================================================================

mod context_basic {
    use super::*;

    #[test]
    fn new_context_is_at_start() {
        let ctx = MarginCollapseContext::new();
        assert!(ctx.is_at_start());
        assert!(ctx.pending_margin().is_zero());
    }

    #[test]
    fn with_initial_margin() {
        let ctx = MarginCollapseContext::with_initial_margin(25.0);
        assert!(ctx.is_at_start());
        assert_eq!(ctx.pending_margin().resolve(), 25.0);
    }

    #[test]
    fn consume_pending() {
        let mut ctx = MarginCollapseContext::new();
        ctx.add_margin(30.0);
        let consumed = ctx.consume_pending();
        assert_eq!(consumed, 30.0);
        assert!(ctx.pending_margin().is_zero());
        assert!(!ctx.is_at_start());
    }
}

// =============================================================================
// Sibling Collapse Tests
// =============================================================================

mod sibling_collapse {
    use super::*;

    #[test]
    fn first_child_top_margin() {
        let mut ctx = MarginCollapseContext::new();
        let (offset, _) = ctx.process_child_margins(20.0, 10.0, false);
        assert_eq!(offset, 20.0);
        assert!(!ctx.is_at_start());
    }

    #[test]
    fn two_siblings_positive() {
        let mut ctx = MarginCollapseContext::new();

        // First child: margin-bottom 20px
        let (offset1, _) = ctx.process_child_margins(0.0, 20.0, false);
        assert_eq!(offset1, 0.0);

        // Second child: margin-top 30px
        let (offset2, _) = ctx.process_child_margins(30.0, 0.0, false);
        assert_eq!(offset2, 30.0); // max(20, 30)
    }

    #[test]
    fn two_siblings_first_larger() {
        let mut ctx = MarginCollapseContext::new();

        // First child: margin-bottom 40px
        ctx.process_child_margins(0.0, 40.0, false);

        // Second child: margin-top 25px
        let (offset, _) = ctx.process_child_margins(25.0, 0.0, false);
        assert_eq!(offset, 40.0); // max(40, 25)
    }

    #[test]
    fn three_siblings() {
        let mut ctx = MarginCollapseContext::new();

        // First child
        ctx.process_child_margins(10.0, 20.0, false);

        // Second child
        let (offset2, _) = ctx.process_child_margins(30.0, 15.0, false);
        assert_eq!(offset2, 30.0); // max(20, 30)

        // Third child
        let (offset3, _) = ctx.process_child_margins(10.0, 5.0, false);
        assert_eq!(offset3, 15.0); // max(15, 10)
    }

    #[test]
    fn siblings_with_negative_margins() {
        let mut ctx = MarginCollapseContext::new();

        // First child: margin-bottom 20px
        ctx.process_child_margins(0.0, 20.0, false);

        // Second child: margin-top -10px (overlaps)
        let (offset, _) = ctx.process_child_margins(-10.0, 0.0, false);
        assert_eq!(offset, 10.0); // 20 + (-10) = 10
    }
}

// =============================================================================
// Empty Block Collapse Tests
// =============================================================================

mod empty_block_collapse {
    use super::*;

    #[test]
    fn empty_block_zero_offset() {
        let mut ctx = MarginCollapseContext::new();
        let (offset, _) = ctx.process_child_margins(20.0, 30.0, true);
        assert_eq!(offset, 0.0);
    }

    #[test]
    fn empty_block_pending_margin() {
        let mut ctx = MarginCollapseContext::new();
        let (_, pending) = ctx.process_child_margins(20.0, 30.0, true);
        assert_eq!(pending.resolve(), 30.0); // max(20, 30)
    }

    #[test]
    fn empty_block_stays_at_start() {
        let mut ctx = MarginCollapseContext::new();
        ctx.process_child_margins(20.0, 30.0, true);
        assert!(ctx.is_at_start());
    }

    #[test]
    fn multiple_empty_blocks() {
        let mut ctx = MarginCollapseContext::new();

        // Three empty blocks with various margins
        ctx.process_child_margins(10.0, 15.0, true);
        ctx.process_child_margins(25.0, 5.0, true);
        let (_, pending) = ctx.process_child_margins(20.0, 30.0, true);

        // All collapse together: max(10, 15, 25, 5, 20, 30) = 30
        assert_eq!(pending.resolve(), 30.0);
    }

    #[test]
    fn empty_block_then_content() {
        let mut ctx = MarginCollapseContext::new();

        // Empty block with margins
        ctx.process_child_margins(20.0, 25.0, true);

        // Non-empty block with margin-top 15px
        let (offset, _) = ctx.process_child_margins(15.0, 10.0, false);

        // max(20, 25, 15) = 25
        assert_eq!(offset, 25.0);
    }

    #[test]
    fn content_then_empty_then_content() {
        let mut ctx = MarginCollapseContext::new();

        // First non-empty block
        ctx.process_child_margins(0.0, 20.0, false);

        // Empty block
        ctx.process_child_margins(15.0, 25.0, true);

        // Second non-empty block
        let (offset, _) = ctx.process_child_margins(10.0, 0.0, false);

        // Collapse: max(20, 15, 25, 10) = 25
        assert_eq!(offset, 25.0);
    }

    #[test]
    fn empty_block_with_negative_margins() {
        let mut ctx = MarginCollapseContext::new();

        // Empty block with positive and negative
        let (_, pending) = ctx.process_child_margins(30.0, -10.0, true);

        // max(30) - max(10) = 20
        assert_eq!(pending.resolve(), 20.0);
    }
}

// =============================================================================
// Parent-Child Collapse Rules Tests
// =============================================================================

mod parent_child_rules {
    use super::*;

    // --- Top margin collapse ---

    #[test]
    fn top_collapse_allowed() {
        assert!(ParentChildCollapseRules::can_collapse_top_margins(
            0.0, 0.0, false
        ));
    }

    #[test]
    fn top_collapse_prevented_by_border() {
        assert!(!ParentChildCollapseRules::can_collapse_top_margins(
            1.0, 0.0, false
        ));
        assert!(!ParentChildCollapseRules::can_collapse_top_margins(
            0.5, 0.0, false
        ));
    }

    #[test]
    fn top_collapse_prevented_by_padding() {
        assert!(!ParentChildCollapseRules::can_collapse_top_margins(
            0.0, 1.0, false
        ));
        assert!(!ParentChildCollapseRules::can_collapse_top_margins(
            0.0, 0.5, false
        ));
    }

    #[test]
    fn top_collapse_prevented_by_clearance() {
        assert!(!ParentChildCollapseRules::can_collapse_top_margins(
            0.0, 0.0, true
        ));
    }

    // --- Bottom margin collapse ---

    #[test]
    fn bottom_collapse_allowed() {
        assert!(ParentChildCollapseRules::can_collapse_bottom_margins(
            0.0, 0.0, true
        ));
    }

    #[test]
    fn bottom_collapse_prevented_by_explicit_height() {
        assert!(!ParentChildCollapseRules::can_collapse_bottom_margins(
            0.0, 0.0, false
        ));
    }

    #[test]
    fn bottom_collapse_prevented_by_border() {
        assert!(!ParentChildCollapseRules::can_collapse_bottom_margins(
            1.0, 0.0, true
        ));
    }

    #[test]
    fn bottom_collapse_prevented_by_padding() {
        assert!(!ParentChildCollapseRules::can_collapse_bottom_margins(
            0.0, 1.0, true
        ));
    }

    // --- Collapse through ---

    #[test]
    fn collapse_through_allowed() {
        assert!(ParentChildCollapseRules::can_collapse_through(
            None, 0.0, 0.0, 0.0, 0.0, 0.0, false, false
        ));
    }

    #[test]
    fn collapse_through_allowed_zero_height() {
        assert!(ParentChildCollapseRules::can_collapse_through(
            Some(0.0), 0.0, 0.0, 0.0, 0.0, 0.0, false, false
        ));
    }

    #[test]
    fn collapse_through_prevented_by_height() {
        assert!(!ParentChildCollapseRules::can_collapse_through(
            Some(10.0), 0.0, 0.0, 0.0, 0.0, 0.0, false, false
        ));
    }

    #[test]
    fn collapse_through_prevented_by_min_height() {
        assert!(!ParentChildCollapseRules::can_collapse_through(
            None, 5.0, 0.0, 0.0, 0.0, 0.0, false, false
        ));
    }

    #[test]
    fn collapse_through_prevented_by_padding_top() {
        assert!(!ParentChildCollapseRules::can_collapse_through(
            None, 0.0, 1.0, 0.0, 0.0, 0.0, false, false
        ));
    }

    #[test]
    fn collapse_through_prevented_by_padding_bottom() {
        assert!(!ParentChildCollapseRules::can_collapse_through(
            None, 0.0, 0.0, 1.0, 0.0, 0.0, false, false
        ));
    }

    #[test]
    fn collapse_through_prevented_by_border_top() {
        assert!(!ParentChildCollapseRules::can_collapse_through(
            None, 0.0, 0.0, 0.0, 1.0, 0.0, false, false
        ));
    }

    #[test]
    fn collapse_through_prevented_by_border_bottom() {
        assert!(!ParentChildCollapseRules::can_collapse_through(
            None, 0.0, 0.0, 0.0, 0.0, 1.0, false, false
        ));
    }

    #[test]
    fn collapse_through_prevented_by_children() {
        assert!(!ParentChildCollapseRules::can_collapse_through(
            None, 0.0, 0.0, 0.0, 0.0, 0.0, true, false
        ));
    }

    #[test]
    fn collapse_through_prevented_by_bfc() {
        assert!(!ParentChildCollapseRules::can_collapse_through(
            None, 0.0, 0.0, 0.0, 0.0, 0.0, false, true
        ));
    }
}

// =============================================================================
// BFC Boundary Tests
// =============================================================================

mod bfc_boundaries {
    use super::*;

    #[test]
    fn overflow_hidden_establishes_bfc() {
        assert!(style_establishes_bfc(
            Overflow::Hidden,
            Overflow::Visible,
            Float::None,
            Position::Static,
        ));
    }

    #[test]
    fn overflow_scroll_establishes_bfc() {
        assert!(style_establishes_bfc(
            Overflow::Scroll,
            Overflow::Visible,
            Float::None,
            Position::Static,
        ));
    }

    #[test]
    fn overflow_auto_establishes_bfc() {
        assert!(style_establishes_bfc(
            Overflow::Auto,
            Overflow::Visible,
            Float::None,
            Position::Static,
        ));
    }

    #[test]
    fn overflow_y_hidden_establishes_bfc() {
        assert!(style_establishes_bfc(
            Overflow::Visible,
            Overflow::Hidden,
            Float::None,
            Position::Static,
        ));
    }

    #[test]
    fn float_left_establishes_bfc() {
        assert!(style_establishes_bfc(
            Overflow::Visible,
            Overflow::Visible,
            Float::Left,
            Position::Static,
        ));
    }

    #[test]
    fn float_right_establishes_bfc() {
        assert!(style_establishes_bfc(
            Overflow::Visible,
            Overflow::Visible,
            Float::Right,
            Position::Static,
        ));
    }

    #[test]
    fn position_absolute_establishes_bfc() {
        assert!(style_establishes_bfc(
            Overflow::Visible,
            Overflow::Visible,
            Float::None,
            Position::Absolute,
        ));
    }

    #[test]
    fn position_fixed_establishes_bfc() {
        assert!(style_establishes_bfc(
            Overflow::Visible,
            Overflow::Visible,
            Float::None,
            Position::Fixed,
        ));
    }

    #[test]
    fn normal_block_no_bfc() {
        assert!(!style_establishes_bfc(
            Overflow::Visible,
            Overflow::Visible,
            Float::None,
            Position::Static,
        ));
    }

    #[test]
    fn relative_position_no_bfc() {
        assert!(!style_establishes_bfc(
            Overflow::Visible,
            Overflow::Visible,
            Float::None,
            Position::Relative,
        ));
    }
}

// =============================================================================
// Clearance Tests
// =============================================================================

mod clearance {
    use super::*;

    #[test]
    fn clearance_breaks_collapse() {
        let mut ctx = MarginCollapseContext::new();

        // First child with margin-bottom
        ctx.process_child_margins(0.0, 20.0, false);

        // Second child with clearance
        let (offset, _) = ctx.process_child_with_clearance(50.0, 10.0, 5.0, false);

        // Offset = pending(20) + clearance(50) + margin_top(10) = 80
        assert_eq!(offset, 80.0);
    }

    #[test]
    fn clearance_on_first_child() {
        let mut ctx = MarginCollapseContext::with_initial_margin(30.0);

        // First child with clearance
        let (offset, _) = ctx.process_child_with_clearance(40.0, 15.0, 10.0, false);

        // Offset = pending(30) + clearance(40) + margin_top(15) = 85
        assert_eq!(offset, 85.0);
    }

    #[test]
    fn clearance_on_empty_block() {
        let mut ctx = MarginCollapseContext::new();

        ctx.process_child_margins(0.0, 20.0, false);

        // Empty block with clearance
        let (offset, pending) = ctx.process_child_with_clearance(30.0, 15.0, 25.0, true);

        // Empty block still gets positioned
        assert_eq!(offset, 50.0); // 20 + 30

        // Pending margin is collapsed margins of empty block
        assert_eq!(pending.resolve(), 25.0); // max(15, 25)
    }
}

// =============================================================================
// Edge Cases Tests
// =============================================================================

mod edge_cases {
    use super::*;

    #[test]
    fn very_large_positive_margin() {
        let m1 = CollapsibleMargin::from_margin(1_000_000.0);
        let m2 = CollapsibleMargin::from_margin(1.0);
        let result = m1.collapse_with(m2);
        assert_eq!(result.resolve(), 1_000_000.0);
    }

    #[test]
    fn very_large_negative_margin() {
        let m1 = CollapsibleMargin::from_margin(-1_000_000.0);
        let m2 = CollapsibleMargin::from_margin(-1.0);
        let result = m1.collapse_with(m2);
        assert_eq!(result.resolve(), -1_000_000.0);
    }

    #[test]
    fn fractional_margins() {
        let m1 = CollapsibleMargin::from_margin(10.5);
        let m2 = CollapsibleMargin::from_margin(10.25);
        let result = m1.collapse_with(m2);
        assert_eq!(result.resolve(), 10.5);
    }

    #[test]
    fn tiny_margins() {
        let m1 = CollapsibleMargin::from_margin(0.001);
        let m2 = CollapsibleMargin::from_margin(0.0001);
        let result = m1.collapse_with(m2);
        assert!((result.resolve() - 0.001).abs() < 0.0001);
    }

    #[test]
    fn collapse_commutative() {
        let m1 = CollapsibleMargin::from_margin(30.0);
        let m2 = CollapsibleMargin::from_margin(-10.0);

        let result1 = m1.collapse_with(m2);
        let result2 = m2.collapse_with(m1);

        assert_eq!(result1.resolve(), result2.resolve());
    }

    #[test]
    fn collapse_associative() {
        let m1 = CollapsibleMargin::from_margin(20.0);
        let m2 = CollapsibleMargin::from_margin(-5.0);
        let m3 = CollapsibleMargin::from_margin(30.0);

        let result1 = m1.collapse_with(m2).collapse_with(m3);
        let result2 = m1.collapse_with(m2.collapse_with(m3));

        assert_eq!(result1.resolve(), result2.resolve());
    }

    #[test]
    fn many_siblings_simulation() {
        let mut ctx = MarginCollapseContext::new();

        // Simulate 10 siblings with various margins
        let margins = [
            (10.0, 20.0),
            (30.0, 15.0),
            (25.0, 40.0),
            (10.0, 10.0),
            (50.0, 5.0),
            (0.0, 30.0),
            (20.0, 20.0),
            (15.0, 15.0),
            (35.0, 10.0),
            (5.0, 25.0),
        ];

        let mut expected_offsets = vec![10.0]; // First child top margin
        let mut prev_bottom = 20.0;

        for i in 1..margins.len() {
            let current_top = margins[i].0;
            expected_offsets.push(prev_bottom.max(current_top));
            prev_bottom = margins[i].1;
        }

        for (i, (top, bottom)) in margins.iter().enumerate() {
            let (offset, _) = ctx.process_child_margins(*top, *bottom, false);
            assert!(
                (offset - expected_offsets[i]).abs() < 0.001,
                "Child {} offset: expected {}, got {}",
                i,
                expected_offsets[i],
                offset
            );
        }
    }

    #[test]
    fn real_world_scenario_nested_divs() {
        // Simulating:
        // <div style="margin-bottom: 20px">
        //   Content
        // </div>
        // <div style="margin-top: 30px; margin-bottom: 40px">
        //   <div style="margin-top: 50px; margin-bottom: 25px">
        //     Content
        //   </div>
        // </div>
        // <div style="margin-top: 35px">
        //   Content
        // </div>

        // Between first and second outer div: max(20, 30) = 30
        // Second div's margin collapses with its first child: max(30, 50) = 50
        // (This is parent-child collapse)
        // Between inner div bottom and second outer bottom: max(25, 40) = 40
        // Between second outer and third: max(40, 35) = 40

        let mut outer_ctx = MarginCollapseContext::new();

        // First outer div
        let (_, _) = outer_ctx.process_child_margins(0.0, 20.0, false);

        // Second outer div
        let (offset2, _) = outer_ctx.process_child_margins(30.0, 40.0, false);
        assert_eq!(offset2, 30.0); // max(20, 30)

        // Third outer div
        let (offset3, _) = outer_ctx.process_child_margins(35.0, 0.0, false);
        assert_eq!(offset3, 40.0); // max(40, 35)
    }
}

// =============================================================================
// Display Tests
// =============================================================================

mod display_tests {
    use super::*;

    #[test]
    fn display_positive_only() {
        let m = CollapsibleMargin::from_margin(20.0);
        let s = format!("{}", m);
        assert!(s.contains("+20"));
    }

    #[test]
    fn display_negative_only() {
        let m = CollapsibleMargin::from_margin(-15.0);
        let s = format!("{}", m);
        assert!(s.contains("-15"));
    }

    #[test]
    fn display_mixed() {
        let m = CollapsibleMargin::new(30.0, 10.0);
        let s = format!("{}", m);
        assert!(s.contains("+30"));
        assert!(s.contains("-10"));
    }

    #[test]
    fn display_zero() {
        let m = CollapsibleMargin::ZERO;
        let s = format!("{}", m);
        assert!(s.contains("0"));
    }
}
