//! Tests for inline layout float integration
//!
//! These tests verify that inline content correctly wraps around floats
//! according to CSS 2.1 Section 9.5.

use fastrender::layout::inline::{
    line_spaces, InlineFloatIntegration, InlineFloatIntegrationMut, LineSpace, LineSpaceOptions,
    PlacedInlineFloat,
};
use fastrender::layout::{FloatContext, FloatSide};
use fastrender::style::Clear;

// ==================== LineSpace Tests ====================

mod line_space_tests {
    use super::*;

    #[test]
    fn test_line_space_construction() {
        let space = LineSpace::new(10.0, 50.0, 200.0);

        assert_eq!(space.y, 10.0);
        assert_eq!(space.left_edge, 50.0);
        assert_eq!(space.width, 200.0);
        assert_eq!(space.right_edge, 250.0);
    }

    #[test]
    fn test_line_space_full_width() {
        let space = LineSpace::full_width(0.0, 800.0);

        assert_eq!(space.y, 0.0);
        assert_eq!(space.left_edge, 0.0);
        assert_eq!(space.width, 800.0);
        assert_eq!(space.right_edge, 800.0);
    }

    #[test]
    fn test_line_space_has_space() {
        assert!(LineSpace::new(0.0, 0.0, 100.0).has_space());
        assert!(LineSpace::new(0.0, 0.0, 0.001).has_space());
        assert!(!LineSpace::new(0.0, 0.0, 0.0).has_space());
    }

    #[test]
    fn test_line_space_fits() {
        let space = LineSpace::new(0.0, 0.0, 100.0);

        assert!(space.fits(0.0));
        assert!(space.fits(50.0));
        assert!(space.fits(100.0));
        assert!(!space.fits(100.1));
        assert!(!space.fits(200.0));
    }

    #[test]
    fn test_line_space_zero_width_not_fits() {
        let space = LineSpace::new(0.0, 400.0, 0.0);

        assert!(!space.has_space());
        assert!(!space.fits(1.0));
        // Zero width fits zero width
        assert!(space.fits(0.0));
    }
}

// ==================== LineSpaceOptions Tests ====================

mod line_space_options_tests {
    use super::*;

    #[test]
    fn test_options_default() {
        let opts = LineSpaceOptions::default();

        assert_eq!(opts.min_width, 0.0);
        assert_eq!(opts.line_height, 0.0);
        assert!(opts.allow_zero_width);
    }

    #[test]
    fn test_options_with_min_width() {
        let opts = LineSpaceOptions::with_min_width(150.0);

        assert_eq!(opts.min_width, 150.0);
        assert_eq!(opts.line_height, 0.0);
    }

    #[test]
    fn test_options_with_line_height() {
        let opts = LineSpaceOptions::with_line_height(20.0);

        assert_eq!(opts.min_width, 0.0);
        assert_eq!(opts.line_height, 20.0);
    }

    #[test]
    fn test_options_builder_chain() {
        let opts = LineSpaceOptions::default()
            .min_width(100.0)
            .line_height(24.0);

        assert_eq!(opts.min_width, 100.0);
        assert_eq!(opts.line_height, 24.0);
    }
}

// ==================== InlineFloatIntegration Basic Tests ====================

mod integration_basic_tests {
    use super::*;

    #[test]
    fn test_integration_empty_context() {
        let ctx = FloatContext::new(800.0);
        let integration = InlineFloatIntegration::new(&ctx);

        assert_eq!(integration.containing_width(), 800.0);
        assert!(!integration.has_floats());
        assert_eq!(integration.floats_bottom(), 0.0);
    }

    #[test]
    fn test_integration_full_width_no_floats() {
        let ctx = FloatContext::new(1024.0);
        let integration = InlineFloatIntegration::new(&ctx);

        let space = integration.get_line_space(0.0);
        assert_eq!(space.left_edge, 0.0);
        assert_eq!(space.width, 1024.0);
        assert_eq!(space.right_edge, 1024.0);
    }

    #[test]
    fn test_integration_with_single_left_float() {
        let mut ctx = FloatContext::new(800.0);
        ctx.add_float_at(FloatSide::Left, 0.0, 0.0, 200.0, 100.0);

        let integration = InlineFloatIntegration::new(&ctx);
        assert!(integration.has_floats());

        // Within float range
        let space = integration.get_line_space(50.0);
        assert_eq!(space.left_edge, 200.0);
        assert_eq!(space.width, 600.0);

        // Below float
        let space = integration.get_line_space(150.0);
        assert_eq!(space.left_edge, 0.0);
        assert_eq!(space.width, 800.0);
    }

    #[test]
    fn test_integration_with_single_right_float() {
        let mut ctx = FloatContext::new(800.0);
        ctx.add_float_at(FloatSide::Right, 600.0, 0.0, 200.0, 100.0);

        let integration = InlineFloatIntegration::new(&ctx);

        let space = integration.get_line_space(50.0);
        assert_eq!(space.left_edge, 0.0);
        assert_eq!(space.width, 600.0);
        assert_eq!(space.right_edge, 600.0);
    }

    #[test]
    fn test_integration_with_both_floats() {
        let mut ctx = FloatContext::new(800.0);
        ctx.add_float_at(FloatSide::Left, 0.0, 0.0, 200.0, 100.0);
        ctx.add_float_at(FloatSide::Right, 600.0, 0.0, 200.0, 100.0);

        let integration = InlineFloatIntegration::new(&ctx);

        let space = integration.get_line_space(50.0);
        assert_eq!(space.left_edge, 200.0);
        assert_eq!(space.width, 400.0);
        assert_eq!(space.right_edge, 600.0);
    }
}

// ==================== Range Queries Tests ====================

mod range_query_tests {
    use super::*;

    #[test]
    fn test_line_space_in_range_no_floats() {
        let ctx = FloatContext::new(800.0);
        let integration = InlineFloatIntegration::new(&ctx);

        let space = integration.get_line_space_in_range(0.0, 100.0);
        assert_eq!(space.width, 800.0);
    }

    #[test]
    fn test_line_space_in_range_single_float() {
        let mut ctx = FloatContext::new(800.0);
        ctx.add_float_at(FloatSide::Left, 0.0, 0.0, 200.0, 100.0);

        let integration = InlineFloatIntegration::new(&ctx);

        // Range fully within float
        let space = integration.get_line_space_in_range(10.0, 50.0);
        assert_eq!(space.left_edge, 200.0);
        assert_eq!(space.width, 600.0);

        // Range partially overlapping float
        let space = integration.get_line_space_in_range(50.0, 150.0);
        assert_eq!(space.left_edge, 200.0); // Most constrained

        // Range fully below float
        let space = integration.get_line_space_in_range(150.0, 200.0);
        assert_eq!(space.left_edge, 0.0);
        assert_eq!(space.width, 800.0);
    }

    #[test]
    fn test_line_space_in_range_multiple_floats() {
        let mut ctx = FloatContext::new(800.0);
        ctx.add_float_at(FloatSide::Left, 0.0, 0.0, 200.0, 50.0);
        ctx.add_float_at(FloatSide::Left, 0.0, 25.0, 300.0, 50.0);

        let integration = InlineFloatIntegration::new(&ctx);

        // Range that spans both floats - should use most constrained
        let space = integration.get_line_space_in_range(0.0, 100.0);
        assert_eq!(space.left_edge, 300.0);
        assert_eq!(space.width, 500.0);
    }
}

// ==================== Find Line Space Tests ====================

mod find_line_space_tests {
    use super::*;

    #[test]
    fn test_find_space_no_floats() {
        let ctx = FloatContext::new(800.0);
        let integration = InlineFloatIntegration::new(&ctx);

        let opts = LineSpaceOptions::with_min_width(500.0);
        let space = integration.find_line_space(0.0, opts);

        assert_eq!(space.y, 0.0);
        assert_eq!(space.width, 800.0);
    }

    #[test]
    fn test_find_space_fits_immediately() {
        let mut ctx = FloatContext::new(800.0);
        ctx.add_float_at(FloatSide::Left, 0.0, 0.0, 200.0, 100.0);

        let integration = InlineFloatIntegration::new(&ctx);

        // 500px fits in the 600px available
        let opts = LineSpaceOptions::with_min_width(500.0).line_height(20.0);
        let space = integration.find_line_space(0.0, opts);

        assert_eq!(space.y, 0.0);
        assert_eq!(space.width, 600.0);
    }

    #[test]
    fn test_find_space_pushes_down() {
        let mut ctx = FloatContext::new(800.0);
        ctx.add_float_at(FloatSide::Left, 0.0, 0.0, 700.0, 100.0);

        let integration = InlineFloatIntegration::new(&ctx);

        // 200px doesn't fit in the 100px available at y=0
        let opts = LineSpaceOptions::with_min_width(200.0).line_height(20.0);
        let space = integration.find_line_space(0.0, opts);

        // Should be pushed to y=100 where float ends
        assert_eq!(space.y, 100.0);
        assert_eq!(space.width, 800.0);
    }

    #[test]
    fn test_find_space_multiple_float_boundaries() {
        let mut ctx = FloatContext::new(800.0);
        ctx.add_float_at(FloatSide::Left, 0.0, 0.0, 600.0, 50.0);
        ctx.add_float_at(FloatSide::Left, 0.0, 50.0, 500.0, 50.0);

        let integration = InlineFloatIntegration::new(&ctx);

        // Need 400px, available: 200 at y=0, 300 at y=50, 800 at y=100
        let opts = LineSpaceOptions::with_min_width(400.0).line_height(20.0);
        let space = integration.find_line_space(0.0, opts);

        // Should skip both floats
        assert_eq!(space.y, 100.0);
        assert_eq!(space.width, 800.0);
    }
}

// ==================== Clear Tests ====================

mod clear_tests {
    use super::*;

    #[test]
    fn test_find_space_with_clear_none() {
        let mut ctx = FloatContext::new(800.0);
        ctx.add_float_at(FloatSide::Left, 0.0, 0.0, 200.0, 100.0);

        let integration = InlineFloatIntegration::new(&ctx);
        let opts = LineSpaceOptions::default();
        let space = integration.find_line_space_with_clear(50.0, Clear::None, opts);

        // No clearance applied
        assert_eq!(space.y, 50.0);
    }

    #[test]
    fn test_find_space_with_clear_left() {
        let mut ctx = FloatContext::new(800.0);
        ctx.add_float_at(FloatSide::Left, 0.0, 0.0, 200.0, 100.0);

        let integration = InlineFloatIntegration::new(&ctx);
        let opts = LineSpaceOptions::default();
        let space = integration.find_line_space_with_clear(50.0, Clear::Left, opts);

        // Should be pushed below the left float
        assert_eq!(space.y, 100.0);
    }

    #[test]
    fn test_find_space_with_clear_right() {
        let mut ctx = FloatContext::new(800.0);
        ctx.add_float_at(FloatSide::Right, 600.0, 0.0, 200.0, 100.0);

        let integration = InlineFloatIntegration::new(&ctx);
        let opts = LineSpaceOptions::default();
        let space = integration.find_line_space_with_clear(50.0, Clear::Right, opts);

        assert_eq!(space.y, 100.0);
    }

    #[test]
    fn test_find_space_with_clear_both() {
        let mut ctx = FloatContext::new(800.0);
        ctx.add_float_at(FloatSide::Left, 0.0, 0.0, 200.0, 100.0);
        ctx.add_float_at(FloatSide::Right, 600.0, 0.0, 200.0, 150.0);

        let integration = InlineFloatIntegration::new(&ctx);
        let opts = LineSpaceOptions::default();
        let space = integration.find_line_space_with_clear(50.0, Clear::Both, opts);

        // Should clear past both floats (right float is taller)
        assert_eq!(space.y, 150.0);
    }

    #[test]
    fn test_clear_when_already_below_floats() {
        let mut ctx = FloatContext::new(800.0);
        ctx.add_float_at(FloatSide::Left, 0.0, 0.0, 200.0, 100.0);

        let integration = InlineFloatIntegration::new(&ctx);
        let opts = LineSpaceOptions::default();
        let space = integration.find_line_space_with_clear(150.0, Clear::Left, opts);

        // Already below float, no change
        assert_eq!(space.y, 150.0);
    }
}

// ==================== Edge Queries Tests ====================

mod edge_query_tests {
    use super::*;

    #[test]
    fn test_left_edge_at_no_floats() {
        let ctx = FloatContext::new(800.0);
        let integration = InlineFloatIntegration::new(&ctx);

        assert_eq!(integration.left_edge_at(0.0), 0.0);
        assert_eq!(integration.left_edge_at(100.0), 0.0);
    }

    #[test]
    fn test_right_edge_at_no_floats() {
        let ctx = FloatContext::new(800.0);
        let integration = InlineFloatIntegration::new(&ctx);

        assert_eq!(integration.right_edge_at(0.0), 800.0);
        assert_eq!(integration.right_edge_at(100.0), 800.0);
    }

    #[test]
    fn test_edges_with_floats() {
        let mut ctx = FloatContext::new(800.0);
        ctx.add_float_at(FloatSide::Left, 0.0, 0.0, 200.0, 100.0);
        ctx.add_float_at(FloatSide::Right, 600.0, 50.0, 200.0, 100.0);

        let integration = InlineFloatIntegration::new(&ctx);

        // At y=25: only left float
        assert_eq!(integration.left_edge_at(25.0), 200.0);
        assert_eq!(integration.right_edge_at(25.0), 800.0);

        // At y=75: both floats
        assert_eq!(integration.left_edge_at(75.0), 200.0);
        assert_eq!(integration.right_edge_at(75.0), 600.0);

        // At y=125: only right float
        assert_eq!(integration.left_edge_at(125.0), 0.0);
        assert_eq!(integration.right_edge_at(125.0), 600.0);

        // At y=175: no floats
        assert_eq!(integration.left_edge_at(175.0), 0.0);
        assert_eq!(integration.right_edge_at(175.0), 800.0);
    }
}

// ==================== InlineFloatIntegrationMut Tests ====================

mod integration_mut_tests {
    use super::*;

    #[test]
    fn test_place_inline_float_left() {
        let mut ctx = FloatContext::new(800.0);
        let mut integration = InlineFloatIntegrationMut::new(&mut ctx);

        let result = integration.place_inline_float(FloatSide::Left, 200.0, 100.0, 50.0);

        assert_eq!(result.rect.x(), 0.0);
        assert_eq!(result.rect.y(), 50.0); // Respects current line Y
        assert_eq!(result.rect.width(), 200.0);
        assert_eq!(result.rect.height(), 100.0);
        assert_eq!(result.side, FloatSide::Left);

        // Verify float affects available space
        let space = integration.get_line_space(75.0);
        assert_eq!(space.left_edge, 200.0);
    }

    #[test]
    fn test_place_inline_float_right() {
        let mut ctx = FloatContext::new(800.0);
        let mut integration = InlineFloatIntegrationMut::new(&mut ctx);

        let result = integration.place_inline_float(FloatSide::Right, 200.0, 100.0, 0.0);

        assert_eq!(result.rect.x(), 600.0);
        assert_eq!(result.rect.y(), 0.0);
        assert_eq!(result.side, FloatSide::Right);

        let space = integration.get_line_space(50.0);
        assert_eq!(space.right_edge, 600.0);
    }

    #[test]
    fn test_place_multiple_inline_floats() {
        let mut ctx = FloatContext::new(800.0);
        let mut integration = InlineFloatIntegrationMut::new(&mut ctx);

        // First left float
        let result1 = integration.place_inline_float(FloatSide::Left, 200.0, 100.0, 0.0);
        assert_eq!(result1.rect.x(), 0.0);

        // Second left float stacks next to first
        let result2 = integration.place_inline_float(FloatSide::Left, 200.0, 100.0, 0.0);
        assert_eq!(result2.rect.x(), 200.0);

        // Right float on opposite side
        let result3 = integration.place_inline_float(FloatSide::Right, 200.0, 100.0, 0.0);
        assert_eq!(result3.rect.x(), 600.0);

        // Check remaining space
        let space = integration.get_line_space(50.0);
        assert_eq!(space.left_edge, 400.0);
        assert_eq!(space.right_edge, 600.0);
        assert_eq!(space.width, 200.0);
    }

    #[test]
    fn test_place_float_that_drops_down() {
        let mut ctx = FloatContext::new(400.0);
        let mut integration = InlineFloatIntegrationMut::new(&mut ctx);

        // First float takes half the width
        integration.place_inline_float(FloatSide::Left, 250.0, 100.0, 0.0);

        // Second float too wide, must drop down
        let result = integration.place_inline_float(FloatSide::Left, 250.0, 50.0, 0.0);
        assert_eq!(result.rect.x(), 0.0);
        assert_eq!(result.rect.y(), 100.0);
    }

    #[test]
    fn test_clearance_computation() {
        let mut ctx = FloatContext::new(800.0);
        let mut integration = InlineFloatIntegrationMut::new(&mut ctx);

        integration.place_inline_float(FloatSide::Left, 200.0, 100.0, 0.0);

        assert_eq!(integration.compute_clearance(50.0, Clear::Left), 100.0);
        assert_eq!(integration.compute_clearance(50.0, Clear::Right), 50.0);
        assert_eq!(integration.clearance_amount(50.0, Clear::Left), 50.0);
        assert_eq!(integration.clearance_amount(50.0, Clear::Right), 0.0);
    }

    #[test]
    fn test_current_y_tracking() {
        let mut ctx = FloatContext::new(800.0);
        let mut integration = InlineFloatIntegrationMut::new(&mut ctx);

        assert_eq!(integration.current_y(), 0.0);

        integration.set_current_y(50.0);
        assert_eq!(integration.current_y(), 50.0);

        integration.set_current_y(100.0);
        assert_eq!(integration.current_y(), 100.0);
    }
}

// ==================== LineSpaceIterator Tests ====================

mod iterator_tests {
    use super::*;

    #[test]
    fn test_iterator_no_floats() {
        let ctx = FloatContext::new(800.0);
        let spaces: Vec<_> = line_spaces(&ctx, 0.0, 100.0).collect();

        assert_eq!(spaces.len(), 1);
        assert_eq!(spaces[0].y, 0.0);
        assert_eq!(spaces[0].width, 800.0);
    }

    #[test]
    fn test_iterator_single_float() {
        let mut ctx = FloatContext::new(800.0);
        ctx.add_float_at(FloatSide::Left, 0.0, 0.0, 200.0, 100.0);

        let spaces: Vec<_> = line_spaces(&ctx, 0.0, 200.0).collect();

        assert_eq!(spaces.len(), 2);

        // Before float ends
        assert_eq!(spaces[0].y, 0.0);
        assert_eq!(spaces[0].left_edge, 200.0);
        assert_eq!(spaces[0].width, 600.0);

        // After float ends
        assert_eq!(spaces[1].y, 100.0);
        assert_eq!(spaces[1].left_edge, 0.0);
        assert_eq!(spaces[1].width, 800.0);
    }

    #[test]
    fn test_iterator_multiple_floats() {
        let mut ctx = FloatContext::new(800.0);
        ctx.add_float_at(FloatSide::Left, 0.0, 0.0, 200.0, 100.0);
        ctx.add_float_at(FloatSide::Right, 600.0, 50.0, 200.0, 100.0);

        let spaces: Vec<_> = line_spaces(&ctx, 0.0, 200.0).collect();

        assert_eq!(spaces.len(), 3);

        // 0-100: left float
        assert_eq!(spaces[0].y, 0.0);

        // 100-150: right float only
        assert_eq!(spaces[1].y, 100.0);

        // 150-200: no floats
        assert_eq!(spaces[2].y, 150.0);
    }

    #[test]
    fn test_iterator_empty_range() {
        let mut ctx = FloatContext::new(800.0);
        ctx.add_float_at(FloatSide::Left, 0.0, 0.0, 200.0, 100.0);

        // Start >= end should yield nothing
        let spaces: Vec<_> = line_spaces(&ctx, 100.0, 100.0).collect();
        assert!(spaces.is_empty());

        let spaces: Vec<_> = line_spaces(&ctx, 200.0, 100.0).collect();
        assert!(spaces.is_empty());
    }

    #[test]
    fn test_iterator_range_after_floats() {
        let mut ctx = FloatContext::new(800.0);
        ctx.add_float_at(FloatSide::Left, 0.0, 0.0, 200.0, 100.0);

        // Range entirely after floats
        let spaces: Vec<_> = line_spaces(&ctx, 150.0, 300.0).collect();

        assert_eq!(spaces.len(), 1);
        assert_eq!(spaces[0].left_edge, 0.0);
        assert_eq!(spaces[0].width, 800.0);
    }
}

// ==================== Complex Scenario Tests ====================

mod complex_scenarios {
    use super::*;

    #[test]
    fn test_narrow_passage_between_floats() {
        let mut ctx = FloatContext::new(800.0);
        ctx.add_float_at(FloatSide::Left, 0.0, 0.0, 350.0, 100.0);
        ctx.add_float_at(FloatSide::Right, 450.0, 0.0, 350.0, 100.0);

        let integration = InlineFloatIntegration::new(&ctx);

        let space = integration.get_line_space(50.0);
        assert_eq!(space.left_edge, 350.0);
        assert_eq!(space.width, 100.0);
        assert_eq!(space.right_edge, 450.0);
    }

    #[test]
    fn test_no_space_between_floats() {
        let mut ctx = FloatContext::new(800.0);
        ctx.add_float_at(FloatSide::Left, 0.0, 0.0, 400.0, 100.0);
        ctx.add_float_at(FloatSide::Right, 400.0, 0.0, 400.0, 100.0);

        let integration = InlineFloatIntegration::new(&ctx);

        let space = integration.get_line_space(50.0);
        assert_eq!(space.width, 0.0);
        assert!(!space.has_space());
    }

    #[test]
    fn test_overlapping_floats_different_widths() {
        let mut ctx = FloatContext::new(800.0);
        // First float: narrow, tall
        ctx.add_float_at(FloatSide::Left, 0.0, 0.0, 100.0, 200.0);
        // Second float: wide, short, starts later
        ctx.add_float_at(FloatSide::Left, 0.0, 50.0, 300.0, 50.0);

        let integration = InlineFloatIntegration::new(&ctx);

        // At y=25: only narrow float
        let space = integration.get_line_space(25.0);
        assert_eq!(space.left_edge, 100.0);

        // At y=75: both floats, use wider
        let space = integration.get_line_space(75.0);
        assert_eq!(space.left_edge, 300.0);

        // At y=125: back to narrow float only
        let space = integration.get_line_space(125.0);
        assert_eq!(space.left_edge, 100.0);

        // At y=250: no floats
        let space = integration.get_line_space(250.0);
        assert_eq!(space.left_edge, 0.0);
    }

    #[test]
    fn test_staircase_floats() {
        let mut ctx = FloatContext::new(800.0);
        // Staircase of progressively smaller floats
        ctx.add_float_at(FloatSide::Left, 0.0, 0.0, 400.0, 50.0);
        ctx.add_float_at(FloatSide::Left, 0.0, 50.0, 300.0, 50.0);
        ctx.add_float_at(FloatSide::Left, 0.0, 100.0, 200.0, 50.0);
        ctx.add_float_at(FloatSide::Left, 0.0, 150.0, 100.0, 50.0);

        let integration = InlineFloatIntegration::new(&ctx);

        // Need 500px, must wait until all floats pass
        let opts = LineSpaceOptions::with_min_width(500.0).line_height(20.0);
        let space = integration.find_line_space(0.0, opts);

        assert_eq!(space.y, 200.0);
        assert_eq!(space.width, 800.0);
    }

    #[test]
    fn test_line_fits_at_boundary() {
        let mut ctx = FloatContext::new(800.0);
        ctx.add_float_at(FloatSide::Left, 0.0, 0.0, 200.0, 100.0);

        let integration = InlineFloatIntegration::new(&ctx);

        // Line that spans float boundary
        assert!(!integration.line_fits_at(80.0, 700.0, 40.0)); // Crosses boundary
        assert!(integration.line_fits_at(100.0, 700.0, 40.0)); // Starts at float end
    }

    #[test]
    fn test_mixed_float_placement() {
        let mut ctx = FloatContext::new(800.0);
        let mut integration = InlineFloatIntegrationMut::new(&mut ctx);

        // Simulate inline layout encountering floats
        let line_y = 0.0;

        // Place first float encountered in content
        let float1 = integration.place_inline_float(FloatSide::Left, 150.0, 80.0, line_y);
        assert_eq!(float1.rect.y(), 0.0);

        // Continue text layout - check available space
        let space = integration.get_line_space(line_y);
        assert_eq!(space.left_edge, 150.0);

        // Encounter another float on same line
        let float2 = integration.place_inline_float(FloatSide::Right, 150.0, 80.0, line_y);
        assert_eq!(float2.rect.x(), 650.0);

        // Space for text is now reduced
        let space = integration.get_line_space(line_y);
        assert_eq!(space.left_edge, 150.0);
        assert_eq!(space.right_edge, 650.0);
        assert_eq!(space.width, 500.0);
    }

    #[test]
    fn test_float_at_line_boundary() {
        let mut ctx = FloatContext::new(800.0);
        let mut integration = InlineFloatIntegrationMut::new(&mut ctx);

        // First line at y=0, place float
        integration.place_inline_float(FloatSide::Left, 200.0, 50.0, 0.0);

        // Second line at y=20 (overlaps with float)
        let space = integration.get_line_space(20.0);
        assert_eq!(space.left_edge, 200.0);

        // Third line at y=60 (below float)
        let space = integration.get_line_space(60.0);
        assert_eq!(space.left_edge, 0.0);
    }
}

// ==================== Integration with Clear Tests ====================

mod clear_integration_tests {
    use super::*;

    #[test]
    fn test_clear_left_only_clears_left() {
        let mut ctx = FloatContext::new(800.0);
        ctx.add_float_at(FloatSide::Left, 0.0, 0.0, 200.0, 100.0);
        ctx.add_float_at(FloatSide::Right, 600.0, 0.0, 200.0, 150.0);

        let integration = InlineFloatIntegration::new(&ctx);
        let opts = LineSpaceOptions::default();

        let space = integration.find_line_space_with_clear(50.0, Clear::Left, opts);

        // Should clear left float (to 100) but not right (at 150)
        assert_eq!(space.y, 100.0);
    }

    #[test]
    fn test_clear_right_only_clears_right() {
        let mut ctx = FloatContext::new(800.0);
        ctx.add_float_at(FloatSide::Left, 0.0, 0.0, 200.0, 150.0);
        ctx.add_float_at(FloatSide::Right, 600.0, 0.0, 200.0, 100.0);

        let integration = InlineFloatIntegration::new(&ctx);
        let opts = LineSpaceOptions::default();

        let space = integration.find_line_space_with_clear(50.0, Clear::Right, opts);

        // Should clear right float (to 100) but not left (at 150)
        assert_eq!(space.y, 100.0);
    }

    #[test]
    fn test_clear_with_space_requirement() {
        let mut ctx = FloatContext::new(800.0);
        ctx.add_float_at(FloatSide::Left, 0.0, 0.0, 700.0, 100.0);
        ctx.add_float_at(FloatSide::Left, 0.0, 100.0, 600.0, 50.0);

        let integration = InlineFloatIntegration::new(&ctx);

        // Clear left, but then need 300px which doesn't fit until y=150
        let opts = LineSpaceOptions::with_min_width(300.0).line_height(20.0);
        let space = integration.find_line_space_with_clear(50.0, Clear::Left, opts);

        // First clears to 100, then finds 300px doesn't fit (only 200 available)
        // Must continue to 150 where second float ends
        assert_eq!(space.y, 150.0);
    }
}

// ==================== Floats Bottom Tests ====================

mod floats_bottom_tests {
    use super::*;

    #[test]
    fn test_floats_bottom_no_floats() {
        let ctx = FloatContext::new(800.0);
        let integration = InlineFloatIntegration::new(&ctx);

        assert_eq!(integration.floats_bottom(), 0.0);
    }

    #[test]
    fn test_floats_bottom_single_float() {
        let mut ctx = FloatContext::new(800.0);
        ctx.add_float_at(FloatSide::Left, 0.0, 0.0, 200.0, 100.0);

        let integration = InlineFloatIntegration::new(&ctx);

        assert_eq!(integration.floats_bottom(), 100.0);
    }

    #[test]
    fn test_floats_bottom_multiple_floats() {
        let mut ctx = FloatContext::new(800.0);
        ctx.add_float_at(FloatSide::Left, 0.0, 0.0, 200.0, 100.0);
        ctx.add_float_at(FloatSide::Right, 600.0, 50.0, 200.0, 200.0);

        let integration = InlineFloatIntegration::new(&ctx);

        // Right float: 50 + 200 = 250
        assert_eq!(integration.floats_bottom(), 250.0);
    }

    #[test]
    fn test_floats_bottom_offset_floats() {
        let mut ctx = FloatContext::new(800.0);
        ctx.add_float_at(FloatSide::Left, 0.0, 100.0, 200.0, 50.0);

        let integration = InlineFloatIntegration::new(&ctx);

        // Float starts at 100, height 50, so bottom at 150
        assert_eq!(integration.floats_bottom(), 150.0);
    }
}
