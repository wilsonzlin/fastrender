//! Tests for the path rendering (rasterize) module
//!
//! These tests verify that path rendering primitives work correctly
//! for rectangles, rounded rectangles, borders, and box shadows.

use fastrender::paint::rasterize::{
    draw_line, fill_circle, fill_ellipse, fill_rect, fill_rounded_rect, render_borders, render_box_shadow, stroke_rect,
    stroke_rounded_rect, BorderColors, BorderRadii, BorderWidths, BoxShadow,
};
use fastrender::Rgba;
use tiny_skia::Pixmap;

// ============================================================================
// Helper Functions
// ============================================================================

/// Creates a test pixmap of the given size
fn create_pixmap(width: u32, height: u32) -> Pixmap {
    Pixmap::new(width, height).expect("Failed to create test pixmap")
}

/// Checks if a pixel at (x, y) is non-transparent
fn pixel_is_colored(pixmap: &Pixmap, x: u32, y: u32) -> bool {
    let idx = (y * pixmap.width() + x) as usize * 4;
    pixmap.data()[idx + 3] > 0 // Check alpha channel
}

/// Gets the color at a pixel
fn get_pixel(pixmap: &Pixmap, x: u32, y: u32) -> (u8, u8, u8, u8) {
    let idx = (y * pixmap.width() + x) as usize * 4;
    let data = pixmap.data();
    // tiny-skia uses premultiplied alpha, so we need to unpremultiply
    let a = data[idx + 3];
    if a == 0 {
        return (0, 0, 0, 0);
    }
    let r = ((data[idx] as u16 * 255) / a as u16) as u8;
    let g = ((data[idx + 1] as u16 * 255) / a as u16) as u8;
    let b = ((data[idx + 2] as u16 * 255) / a as u16) as u8;
    (r, g, b, a)
}

/// Counts non-transparent pixels in the pixmap
fn count_colored_pixels(pixmap: &Pixmap) -> usize {
    let mut count = 0;
    for y in 0..pixmap.height() {
        for x in 0..pixmap.width() {
            if pixel_is_colored(pixmap, x, y) {
                count += 1;
            }
        }
    }
    count
}

// ============================================================================
// BorderRadii Tests
// ============================================================================

mod border_radii {
    use super::*;

    #[test]
    fn test_uniform_radii() {
        let radii = BorderRadii::uniform(10.0);
        assert_eq!(radii.top_left, 10.0);
        assert_eq!(radii.top_right, 10.0);
        assert_eq!(radii.bottom_right, 10.0);
        assert_eq!(radii.bottom_left, 10.0);
    }

    #[test]
    fn test_zero_radii() {
        let radii = BorderRadii::zero();
        assert!(radii.is_zero());
        assert!(!radii.has_radius());
    }

    #[test]
    fn test_individual_radii() {
        let radii = BorderRadii::new(5.0, 10.0, 15.0, 20.0);
        assert_eq!(radii.top_left, 5.0);
        assert_eq!(radii.top_right, 10.0);
        assert_eq!(radii.bottom_right, 15.0);
        assert_eq!(radii.bottom_left, 20.0);
        assert!(radii.has_radius());
    }

    #[test]
    fn test_clamped_radii_no_overflow() {
        let radii = BorderRadii::uniform(10.0);
        let clamped = radii.clamped(100.0, 100.0);
        // Should not change when there's plenty of room
        assert_eq!(clamped.top_left, 10.0);
    }

    #[test]
    fn test_clamped_radii_with_overflow() {
        // Radii sum to 200 on each edge, but box is only 100x100
        let radii = BorderRadii::uniform(100.0);
        let clamped = radii.clamped(100.0, 100.0);
        // Should be scaled down to 50
        assert!(clamped.top_left <= 50.0);
        assert!(clamped.top_left + clamped.top_right <= 100.0);
    }

    #[test]
    fn test_clamped_radii_zero_dimensions() {
        let radii = BorderRadii::uniform(10.0);
        let clamped = radii.clamped(0.0, 100.0);
        assert!(clamped.is_zero());
    }

    #[test]
    fn test_shrink_radii() {
        let radii = BorderRadii::uniform(10.0);
        let shrunk = radii.shrink(3.0);
        assert_eq!(shrunk.top_left, 7.0);
    }

    #[test]
    fn test_shrink_radii_cannot_go_negative() {
        let radii = BorderRadii::uniform(10.0);
        let shrunk = radii.shrink(15.0);
        assert_eq!(shrunk.top_left, 0.0);
        assert!(shrunk.is_zero());
    }

    #[test]
    fn test_partial_radius() {
        let radii = BorderRadii::new(10.0, 0.0, 0.0, 0.0);
        assert!(radii.has_radius());
        assert!(!radii.is_zero());
    }
}

// ============================================================================
// BorderWidths Tests
// ============================================================================

mod border_widths {
    use super::*;

    #[test]
    fn test_uniform_widths() {
        let widths = BorderWidths::uniform(5.0);
        assert_eq!(widths.top, 5.0);
        assert_eq!(widths.right, 5.0);
        assert_eq!(widths.bottom, 5.0);
        assert_eq!(widths.left, 5.0);
        assert!(widths.has_border());
    }

    #[test]
    fn test_individual_widths() {
        let widths = BorderWidths::new(1.0, 2.0, 3.0, 4.0);
        assert_eq!(widths.top, 1.0);
        assert_eq!(widths.right, 2.0);
        assert_eq!(widths.bottom, 3.0);
        assert_eq!(widths.left, 4.0);
    }

    #[test]
    fn test_no_border() {
        let widths = BorderWidths::new(0.0, 0.0, 0.0, 0.0);
        assert!(!widths.has_border());
    }

    #[test]
    fn test_partial_border() {
        let widths = BorderWidths::new(5.0, 0.0, 0.0, 0.0);
        assert!(widths.has_border());
    }
}

// ============================================================================
// Rectangle Rendering Tests
// ============================================================================

mod fill_rect_tests {
    use super::*;

    #[test]
    fn test_basic_fill_rect() {
        let mut pixmap = create_pixmap(100, 100);
        let result = fill_rect(&mut pixmap, 10.0, 10.0, 50.0, 50.0, Rgba::rgb(255, 0, 0));
        assert!(result);

        // Check that center of rectangle is colored
        assert!(pixel_is_colored(&pixmap, 35, 35));

        // Check corners of rectangle
        assert!(pixel_is_colored(&pixmap, 10, 10));
        assert!(pixel_is_colored(&pixmap, 59, 10));
        assert!(pixel_is_colored(&pixmap, 10, 59));
        assert!(pixel_is_colored(&pixmap, 59, 59));
    }

    #[test]
    fn test_fill_rect_transparent() {
        let mut pixmap = create_pixmap(100, 100);
        let result = fill_rect(&mut pixmap, 10.0, 10.0, 50.0, 50.0, Rgba::TRANSPARENT);
        assert!(!result);
        assert_eq!(count_colored_pixels(&pixmap), 0);
    }

    #[test]
    fn test_fill_rect_zero_width() {
        let mut pixmap = create_pixmap(100, 100);
        let result = fill_rect(&mut pixmap, 10.0, 10.0, 0.0, 50.0, Rgba::rgb(255, 0, 0));
        assert!(!result);
    }

    #[test]
    fn test_fill_rect_zero_height() {
        let mut pixmap = create_pixmap(100, 100);
        let result = fill_rect(&mut pixmap, 10.0, 10.0, 50.0, 0.0, Rgba::rgb(255, 0, 0));
        assert!(!result);
    }

    #[test]
    fn test_fill_rect_negative_dimensions() {
        let mut pixmap = create_pixmap(100, 100);
        let result = fill_rect(&mut pixmap, 10.0, 10.0, -50.0, 50.0, Rgba::rgb(255, 0, 0));
        assert!(!result);
    }

    #[test]
    fn test_fill_rect_semi_transparent() {
        let mut pixmap = create_pixmap(100, 100);
        let result = fill_rect(&mut pixmap, 10.0, 10.0, 50.0, 50.0, Rgba::from_rgba8(255, 0, 0, 128));
        assert!(result);
        assert!(pixel_is_colored(&pixmap, 35, 35));
    }

    #[test]
    fn test_fill_rect_at_origin() {
        let mut pixmap = create_pixmap(100, 100);
        let result = fill_rect(&mut pixmap, 0.0, 0.0, 10.0, 10.0, Rgba::rgb(0, 255, 0));
        assert!(result);
        assert!(pixel_is_colored(&pixmap, 0, 0));
        assert!(pixel_is_colored(&pixmap, 5, 5));
    }

    #[test]
    fn test_fill_rect_full_canvas() {
        let mut pixmap = create_pixmap(50, 50);
        let result = fill_rect(&mut pixmap, 0.0, 0.0, 50.0, 50.0, Rgba::rgb(0, 0, 255));
        assert!(result);
        // All pixels should be colored
        assert_eq!(count_colored_pixels(&pixmap), 50 * 50);
    }
}

mod stroke_rect_tests {
    use super::*;

    #[test]
    fn test_basic_stroke_rect() {
        let mut pixmap = create_pixmap(100, 100);
        let result = stroke_rect(&mut pixmap, 10.0, 10.0, 50.0, 50.0, Rgba::rgb(0, 0, 255), 2.0);
        assert!(result);
        // Center should be empty (just an outline)
        // Note: actual behavior depends on stroke implementation
    }

    #[test]
    fn test_stroke_rect_transparent() {
        let mut pixmap = create_pixmap(100, 100);
        let result = stroke_rect(&mut pixmap, 10.0, 10.0, 50.0, 50.0, Rgba::TRANSPARENT, 2.0);
        assert!(!result);
    }

    #[test]
    fn test_stroke_rect_zero_width_stroke() {
        let mut pixmap = create_pixmap(100, 100);
        let result = stroke_rect(&mut pixmap, 10.0, 10.0, 50.0, 50.0, Rgba::rgb(0, 0, 255), 0.0);
        assert!(!result);
    }
}

// ============================================================================
// Rounded Rectangle Tests
// ============================================================================

mod rounded_rect_tests {
    use super::*;

    #[test]
    fn test_fill_rounded_rect_uniform_radii() {
        let mut pixmap = create_pixmap(100, 100);
        let radii = BorderRadii::uniform(10.0);
        let result = fill_rounded_rect(&mut pixmap, 10.0, 10.0, 60.0, 60.0, &radii, Rgba::rgb(0, 255, 0));
        assert!(result);

        // Center should be colored
        assert!(pixel_is_colored(&pixmap, 40, 40));
    }

    #[test]
    fn test_fill_rounded_rect_zero_radii() {
        let mut pixmap = create_pixmap(100, 100);
        let radii = BorderRadii::zero();
        let result = fill_rounded_rect(&mut pixmap, 10.0, 10.0, 50.0, 50.0, &radii, Rgba::rgb(255, 0, 0));
        assert!(result);
        // Should behave like regular rectangle
    }

    #[test]
    fn test_fill_rounded_rect_different_radii() {
        let mut pixmap = create_pixmap(100, 100);
        let radii = BorderRadii::new(5.0, 10.0, 15.0, 20.0);
        let result = fill_rounded_rect(&mut pixmap, 10.0, 10.0, 60.0, 60.0, &radii, Rgba::rgb(0, 0, 255));
        assert!(result);
    }

    #[test]
    fn test_fill_rounded_rect_large_radii() {
        let mut pixmap = create_pixmap(100, 100);
        // Radii larger than box - should be clamped
        let radii = BorderRadii::uniform(50.0);
        let result = fill_rounded_rect(&mut pixmap, 20.0, 20.0, 40.0, 40.0, &radii, Rgba::rgb(128, 128, 0));
        assert!(result);
    }

    #[test]
    fn test_fill_rounded_rect_transparent() {
        let mut pixmap = create_pixmap(100, 100);
        let radii = BorderRadii::uniform(10.0);
        let result = fill_rounded_rect(&mut pixmap, 10.0, 10.0, 50.0, 50.0, &radii, Rgba::TRANSPARENT);
        assert!(!result);
    }

    #[test]
    fn test_stroke_rounded_rect() {
        let mut pixmap = create_pixmap(100, 100);
        let radii = BorderRadii::uniform(10.0);
        let result = stroke_rounded_rect(&mut pixmap, 10.0, 10.0, 60.0, 60.0, &radii, Rgba::rgb(0, 0, 0), 2.0);
        assert!(result);
    }

    #[test]
    fn test_stroke_rounded_rect_zero_radii() {
        let mut pixmap = create_pixmap(100, 100);
        let radii = BorderRadii::zero();
        let result = stroke_rounded_rect(&mut pixmap, 10.0, 10.0, 50.0, 50.0, &radii, Rgba::rgb(0, 0, 0), 2.0);
        assert!(result);
    }
}

// ============================================================================
// Border Rendering Tests
// ============================================================================

mod border_tests {
    use super::*;

    #[test]
    fn test_render_uniform_borders() {
        let mut pixmap = create_pixmap(100, 100);
        let widths = BorderWidths::uniform(5.0);
        let colors = BorderColors::uniform(Rgba::rgb(0, 0, 0));
        let radii = BorderRadii::zero();
        let result = render_borders(&mut pixmap, 10.0, 10.0, 80.0, 80.0, &widths, &colors, &radii);
        assert!(result);
    }

    #[test]
    fn test_render_different_width_borders() {
        let mut pixmap = create_pixmap(100, 100);
        let widths = BorderWidths::new(2.0, 4.0, 6.0, 8.0);
        let colors = BorderColors::uniform(Rgba::rgb(0, 0, 0));
        let radii = BorderRadii::zero();
        let result = render_borders(&mut pixmap, 10.0, 10.0, 80.0, 80.0, &widths, &colors, &radii);
        assert!(result);
    }

    #[test]
    fn test_render_different_color_borders() {
        let mut pixmap = create_pixmap(100, 100);
        let widths = BorderWidths::uniform(5.0);
        let colors = BorderColors::new(
            Rgba::rgb(255, 0, 0),   // top - red
            Rgba::rgb(0, 255, 0),   // right - green
            Rgba::rgb(0, 0, 255),   // bottom - blue
            Rgba::rgb(255, 255, 0), // left - yellow
        );
        let radii = BorderRadii::zero();
        let result = render_borders(&mut pixmap, 10.0, 10.0, 80.0, 80.0, &widths, &colors, &radii);
        assert!(result);
    }

    #[test]
    fn test_render_rounded_borders() {
        let mut pixmap = create_pixmap(100, 100);
        let widths = BorderWidths::uniform(5.0);
        let colors = BorderColors::uniform(Rgba::rgb(0, 0, 0));
        let radii = BorderRadii::uniform(10.0);
        let result = render_borders(&mut pixmap, 10.0, 10.0, 80.0, 80.0, &widths, &colors, &radii);
        assert!(result);
    }

    #[test]
    fn test_render_no_borders() {
        let mut pixmap = create_pixmap(100, 100);
        let widths = BorderWidths::new(0.0, 0.0, 0.0, 0.0);
        let colors = BorderColors::uniform(Rgba::rgb(0, 0, 0));
        let radii = BorderRadii::zero();
        let result = render_borders(&mut pixmap, 10.0, 10.0, 80.0, 80.0, &widths, &colors, &radii);
        assert!(!result);
    }

    #[test]
    fn test_render_partial_borders() {
        let mut pixmap = create_pixmap(100, 100);
        let widths = BorderWidths::new(5.0, 0.0, 5.0, 0.0); // Top and bottom only
        let colors = BorderColors::uniform(Rgba::rgb(0, 0, 0));
        let radii = BorderRadii::zero();
        let result = render_borders(&mut pixmap, 10.0, 10.0, 80.0, 80.0, &widths, &colors, &radii);
        assert!(result);
    }

    #[test]
    fn test_render_transparent_borders() {
        let mut pixmap = create_pixmap(100, 100);
        let widths = BorderWidths::uniform(5.0);
        let colors = BorderColors::uniform(Rgba::TRANSPARENT);
        let radii = BorderRadii::zero();
        let result = render_borders(&mut pixmap, 10.0, 10.0, 80.0, 80.0, &widths, &colors, &radii);
        assert!(!result); // Should return false for transparent
    }
}

// ============================================================================
// Box Shadow Tests
// ============================================================================

mod box_shadow_tests {
    use super::*;

    #[test]
    fn test_render_basic_shadow() {
        let mut pixmap = create_pixmap(100, 100);
        let radii = BorderRadii::zero();
        let shadow = BoxShadow::new(5.0, 5.0, 0.0, 0.0, Rgba::from_rgba8(0, 0, 0, 128));
        let result = render_box_shadow(&mut pixmap, 20.0, 20.0, 40.0, 40.0, &radii, &shadow);
        assert!(result);
    }

    #[test]
    fn test_render_shadow_with_blur() {
        let mut pixmap = create_pixmap(100, 100);
        let radii = BorderRadii::zero();
        let shadow = BoxShadow::new(5.0, 5.0, 10.0, 0.0, Rgba::from_rgba8(0, 0, 0, 128));
        let result = render_box_shadow(&mut pixmap, 20.0, 20.0, 40.0, 40.0, &radii, &shadow);
        assert!(result);
    }

    #[test]
    fn test_render_shadow_with_spread() {
        let mut pixmap = create_pixmap(100, 100);
        let radii = BorderRadii::zero();
        let shadow = BoxShadow::new(0.0, 0.0, 0.0, 10.0, Rgba::from_rgba8(0, 0, 0, 100));
        let result = render_box_shadow(&mut pixmap, 20.0, 20.0, 40.0, 40.0, &radii, &shadow);
        assert!(result);
    }

    #[test]
    fn test_render_inset_shadow() {
        let mut pixmap = create_pixmap(100, 100);
        let radii = BorderRadii::zero();
        let shadow = BoxShadow::inset(5.0, 5.0, 10.0, 5.0, Rgba::from_rgba8(0, 0, 0, 128));
        let result = render_box_shadow(&mut pixmap, 10.0, 10.0, 80.0, 80.0, &radii, &shadow);
        assert!(result);
    }

    #[test]
    fn test_render_transparent_shadow() {
        let mut pixmap = create_pixmap(100, 100);
        let radii = BorderRadii::zero();
        let shadow = BoxShadow::new(5.0, 5.0, 10.0, 0.0, Rgba::TRANSPARENT);
        let result = render_box_shadow(&mut pixmap, 20.0, 20.0, 40.0, 40.0, &radii, &shadow);
        assert!(!result);
    }

    #[test]
    fn test_render_rounded_shadow() {
        let mut pixmap = create_pixmap(100, 100);
        let radii = BorderRadii::uniform(10.0);
        let shadow = BoxShadow::new(5.0, 5.0, 10.0, 0.0, Rgba::from_rgba8(0, 0, 0, 128));
        let result = render_box_shadow(&mut pixmap, 20.0, 20.0, 40.0, 40.0, &radii, &shadow);
        assert!(result);
    }

    #[test]
    fn test_render_shadow_negative_offset() {
        let mut pixmap = create_pixmap(100, 100);
        let radii = BorderRadii::zero();
        let shadow = BoxShadow::new(-5.0, -5.0, 5.0, 0.0, Rgba::from_rgba8(0, 0, 0, 128));
        let result = render_box_shadow(&mut pixmap, 30.0, 30.0, 40.0, 40.0, &radii, &shadow);
        assert!(result);
    }
}

// ============================================================================
// Line and Shape Tests
// ============================================================================

mod line_tests {
    use super::*;

    #[test]
    fn test_draw_horizontal_line() {
        let mut pixmap = create_pixmap(100, 100);
        let result = draw_line(&mut pixmap, 10.0, 50.0, 90.0, 50.0, Rgba::rgb(255, 0, 0), 2.0);
        assert!(result);
    }

    #[test]
    fn test_draw_vertical_line() {
        let mut pixmap = create_pixmap(100, 100);
        let result = draw_line(&mut pixmap, 50.0, 10.0, 50.0, 90.0, Rgba::rgb(0, 255, 0), 2.0);
        assert!(result);
    }

    #[test]
    fn test_draw_diagonal_line() {
        let mut pixmap = create_pixmap(100, 100);
        let result = draw_line(&mut pixmap, 0.0, 0.0, 100.0, 100.0, Rgba::rgb(0, 0, 255), 2.0);
        assert!(result);
    }

    #[test]
    fn test_draw_line_transparent() {
        let mut pixmap = create_pixmap(100, 100);
        let result = draw_line(&mut pixmap, 0.0, 0.0, 100.0, 100.0, Rgba::TRANSPARENT, 2.0);
        assert!(!result);
    }

    #[test]
    fn test_draw_line_zero_width() {
        let mut pixmap = create_pixmap(100, 100);
        let result = draw_line(&mut pixmap, 0.0, 0.0, 100.0, 100.0, Rgba::rgb(0, 0, 0), 0.0);
        assert!(!result);
    }
}

mod circle_tests {
    use super::*;

    #[test]
    fn test_fill_circle_basic() {
        let mut pixmap = create_pixmap(100, 100);
        let result = fill_circle(&mut pixmap, 50.0, 50.0, 30.0, Rgba::rgb(255, 0, 255));
        assert!(result);

        // Center should be colored
        assert!(pixel_is_colored(&pixmap, 50, 50));
    }

    #[test]
    fn test_fill_circle_edge_positions() {
        let mut pixmap = create_pixmap(100, 100);
        let result = fill_circle(&mut pixmap, 10.0, 10.0, 10.0, Rgba::rgb(0, 128, 128));
        assert!(result);
    }

    #[test]
    fn test_fill_circle_transparent() {
        let mut pixmap = create_pixmap(100, 100);
        let result = fill_circle(&mut pixmap, 50.0, 50.0, 30.0, Rgba::TRANSPARENT);
        assert!(!result);
    }

    #[test]
    fn test_fill_circle_zero_radius() {
        let mut pixmap = create_pixmap(100, 100);
        let result = fill_circle(&mut pixmap, 50.0, 50.0, 0.0, Rgba::rgb(255, 0, 0));
        assert!(!result);
    }
}

mod ellipse_tests {
    use super::*;

    #[test]
    fn test_fill_ellipse_basic() {
        let mut pixmap = create_pixmap(100, 100);
        let result = fill_ellipse(&mut pixmap, 50.0, 50.0, 40.0, 20.0, Rgba::rgb(128, 0, 128));
        assert!(result);

        // Center should be colored
        assert!(pixel_is_colored(&pixmap, 50, 50));
    }

    #[test]
    fn test_fill_ellipse_tall() {
        let mut pixmap = create_pixmap(100, 100);
        let result = fill_ellipse(&mut pixmap, 50.0, 50.0, 20.0, 40.0, Rgba::rgb(0, 128, 0));
        assert!(result);
    }

    #[test]
    fn test_fill_ellipse_transparent() {
        let mut pixmap = create_pixmap(100, 100);
        let result = fill_ellipse(&mut pixmap, 50.0, 50.0, 40.0, 20.0, Rgba::TRANSPARENT);
        assert!(!result);
    }

    #[test]
    fn test_fill_ellipse_zero_rx() {
        let mut pixmap = create_pixmap(100, 100);
        let result = fill_ellipse(&mut pixmap, 50.0, 50.0, 0.0, 20.0, Rgba::rgb(255, 0, 0));
        assert!(!result);
    }

    #[test]
    fn test_fill_ellipse_zero_ry() {
        let mut pixmap = create_pixmap(100, 100);
        let result = fill_ellipse(&mut pixmap, 50.0, 50.0, 40.0, 0.0, Rgba::rgb(255, 0, 0));
        assert!(!result);
    }
}

// ============================================================================
// Integration Tests
// ============================================================================

mod integration {
    use super::*;

    #[test]
    fn test_layered_rendering() {
        // Test that rendering multiple shapes works correctly
        let mut pixmap = create_pixmap(200, 200);

        // Draw background
        fill_rect(&mut pixmap, 0.0, 0.0, 200.0, 200.0, Rgba::rgb(240, 240, 240));

        // Draw a shadow
        let shadow = BoxShadow::new(5.0, 5.0, 10.0, 0.0, Rgba::from_rgba8(0, 0, 0, 64));
        render_box_shadow(&mut pixmap, 50.0, 50.0, 100.0, 100.0, &BorderRadii::zero(), &shadow);

        // Draw main box
        fill_rounded_rect(
            &mut pixmap,
            50.0,
            50.0,
            100.0,
            100.0,
            &BorderRadii::uniform(10.0),
            Rgba::rgb(255, 255, 255),
        );

        // Draw border
        render_borders(
            &mut pixmap,
            50.0,
            50.0,
            100.0,
            100.0,
            &BorderWidths::uniform(2.0),
            &BorderColors::uniform(Rgba::rgb(0, 0, 0)),
            &BorderRadii::uniform(10.0),
        );

        // Check that we have colored pixels
        assert!(count_colored_pixels(&pixmap) > 0);
    }

    #[test]
    fn test_button_style_rendering() {
        // Simulate rendering a styled button
        let mut pixmap = create_pixmap(120, 40);

        let radii = BorderRadii::uniform(6.0);

        // Button background
        fill_rounded_rect(&mut pixmap, 0.0, 0.0, 120.0, 40.0, &radii, Rgba::rgb(66, 133, 244));

        // Button border
        render_borders(
            &mut pixmap,
            0.0,
            0.0,
            120.0,
            40.0,
            &BorderWidths::uniform(1.0),
            &BorderColors::uniform(Rgba::rgb(41, 98, 195)),
            &radii,
        );

        // Verify rendering occurred
        assert!(count_colored_pixels(&pixmap) > 100);
    }

    #[test]
    fn test_card_style_rendering() {
        // Simulate rendering a card with shadow
        let mut pixmap = create_pixmap(300, 200);

        let radii = BorderRadii::uniform(8.0);

        // Shadow
        let shadow = BoxShadow::new(0.0, 2.0, 8.0, 0.0, Rgba::from_rgba8(0, 0, 0, 50));
        render_box_shadow(&mut pixmap, 20.0, 20.0, 260.0, 160.0, &radii, &shadow);

        // Card background
        fill_rounded_rect(&mut pixmap, 20.0, 20.0, 260.0, 160.0, &radii, Rgba::rgb(255, 255, 255));

        // Verify rendering occurred
        assert!(count_colored_pixels(&pixmap) > 1000);
    }
}
