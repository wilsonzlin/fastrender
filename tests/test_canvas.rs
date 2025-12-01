//! Integration tests for the Canvas module
//!
//! These tests verify the Canvas wrapper for tiny-skia works correctly
//! for real-world rendering scenarios.

use fastrender::geometry::{Point, Rect};
use fastrender::{BlendMode, BorderRadii, Canvas};
use fastrender::Rgba;
use fastrender::{FontContext, Script, TextDirection, TextShaper};

// ============================================================================
// Canvas Creation Tests
// ============================================================================

#[test]
fn test_canvas_creation_various_sizes() {
    // Small canvas
    let canvas = Canvas::new(1, 1, Rgba::WHITE);
    assert!(canvas.is_ok());

    // Medium canvas
    let canvas = Canvas::new(800, 600, Rgba::WHITE);
    assert!(canvas.is_ok());

    // Large canvas
    let canvas = Canvas::new(4096, 4096, Rgba::WHITE);
    assert!(canvas.is_ok());
}

#[test]
fn test_canvas_creation_with_colors() {
    // White background
    let canvas = Canvas::new(10, 10, Rgba::WHITE).unwrap();
    let data = canvas.pixmap().data();
    assert_eq!(data[0], 255);
    assert_eq!(data[1], 255);
    assert_eq!(data[2], 255);
    assert_eq!(data[3], 255);

    // Black background
    let canvas = Canvas::new(10, 10, Rgba::BLACK).unwrap();
    let data = canvas.pixmap().data();
    assert_eq!(data[0], 0);
    assert_eq!(data[1], 0);
    assert_eq!(data[2], 0);
    assert_eq!(data[3], 255);

    // Transparent background
    let canvas = Canvas::new(10, 10, Rgba::TRANSPARENT).unwrap();
    let data = canvas.pixmap().data();
    assert_eq!(data[0], 0);
    assert_eq!(data[1], 0);
    assert_eq!(data[2], 0);
    assert_eq!(data[3], 0);
}

// ============================================================================
// Rectangle Drawing Tests
// ============================================================================

#[test]
fn test_draw_rect_basic() {
    let mut canvas = Canvas::new(100, 100, Rgba::WHITE).unwrap();

    // Draw a red rectangle
    let rect = Rect::from_xywh(10.0, 10.0, 30.0, 20.0);
    canvas.draw_rect(rect, Rgba::rgb(255, 0, 0));

    // Verify the canvas was modified (just checking it doesn't crash)
    let pixmap = canvas.into_pixmap();
    assert_eq!(pixmap.width(), 100);
    assert_eq!(pixmap.height(), 100);
}

#[test]
fn test_draw_rect_at_origin() {
    let mut canvas = Canvas::new(50, 50, Rgba::WHITE).unwrap();

    let rect = Rect::from_xywh(0.0, 0.0, 10.0, 10.0);
    canvas.draw_rect(rect, Rgba::rgb(0, 255, 0));

    let data = canvas.pixmap().data();
    // First pixel should be green
    assert_eq!(data[0], 0); // R
    assert_eq!(data[1], 255); // G
    assert_eq!(data[2], 0); // B
    assert_eq!(data[3], 255); // A
}

#[test]
fn test_draw_multiple_rects() {
    let mut canvas = Canvas::new(100, 100, Rgba::WHITE).unwrap();

    // Draw overlapping rectangles
    canvas.draw_rect(Rect::from_xywh(0.0, 0.0, 50.0, 50.0), Rgba::rgb(255, 0, 0));
    canvas.draw_rect(Rect::from_xywh(25.0, 25.0, 50.0, 50.0), Rgba::rgb(0, 255, 0));
    canvas.draw_rect(Rect::from_xywh(50.0, 50.0, 50.0, 50.0), Rgba::rgb(0, 0, 255));

    // Just verify it completes without crashing
    let _ = canvas.into_pixmap();
}

#[test]
fn test_stroke_rect() {
    let mut canvas = Canvas::new(100, 100, Rgba::WHITE).unwrap();

    let rect = Rect::from_xywh(20.0, 20.0, 60.0, 40.0);
    canvas.stroke_rect(rect, Rgba::BLACK, 2.0);

    let _ = canvas.into_pixmap();
}

// ============================================================================
// Rounded Rectangle Tests
// ============================================================================

#[test]
fn test_draw_rounded_rect_uniform() {
    let mut canvas = Canvas::new(100, 100, Rgba::WHITE).unwrap();

    let rect = Rect::from_xywh(10.0, 10.0, 80.0, 60.0);
    let radii = BorderRadii::uniform(10.0);
    canvas.draw_rounded_rect(rect, radii, Rgba::rgb(100, 150, 200));

    let _ = canvas.into_pixmap();
}

#[test]
fn test_draw_rounded_rect_different_radii() {
    let mut canvas = Canvas::new(100, 100, Rgba::WHITE).unwrap();

    let rect = Rect::from_xywh(10.0, 10.0, 80.0, 60.0);
    let radii = BorderRadii::new(5.0, 10.0, 15.0, 20.0);
    canvas.draw_rounded_rect(rect, radii, Rgba::rgb(200, 100, 50));

    let _ = canvas.into_pixmap();
}

#[test]
fn test_draw_rounded_rect_large_radius() {
    let mut canvas = Canvas::new(100, 100, Rgba::WHITE).unwrap();

    // Radius larger than half the height - should be clamped
    let rect = Rect::from_xywh(10.0, 10.0, 80.0, 30.0);
    let radii = BorderRadii::uniform(50.0); // Will be clamped to 15
    canvas.draw_rounded_rect(rect, radii, Rgba::rgb(50, 100, 200));

    let _ = canvas.into_pixmap();
}

#[test]
fn test_stroke_rounded_rect() {
    let mut canvas = Canvas::new(100, 100, Rgba::WHITE).unwrap();

    let rect = Rect::from_xywh(15.0, 15.0, 70.0, 50.0);
    let radii = BorderRadii::uniform(8.0);
    canvas.stroke_rounded_rect(rect, radii, Rgba::BLACK, 3.0);

    let _ = canvas.into_pixmap();
}

// ============================================================================
// Circle Drawing Tests
// ============================================================================

#[test]
fn test_draw_circle() {
    let mut canvas = Canvas::new(100, 100, Rgba::WHITE).unwrap();

    canvas.draw_circle(Point::new(50.0, 50.0), 30.0, Rgba::rgb(255, 128, 0));

    let _ = canvas.into_pixmap();
}

#[test]
fn test_stroke_circle() {
    let mut canvas = Canvas::new(100, 100, Rgba::WHITE).unwrap();

    canvas.stroke_circle(Point::new(50.0, 50.0), 40.0, Rgba::BLACK, 2.0);

    let _ = canvas.into_pixmap();
}

// ============================================================================
// Line Drawing Tests
// ============================================================================

#[test]
fn test_draw_line() {
    let mut canvas = Canvas::new(100, 100, Rgba::WHITE).unwrap();

    canvas.draw_line(Point::new(10.0, 10.0), Point::new(90.0, 90.0), Rgba::BLACK, 1.0);

    let _ = canvas.into_pixmap();
}

#[test]
fn test_draw_line_thick() {
    let mut canvas = Canvas::new(100, 100, Rgba::WHITE).unwrap();

    canvas.draw_line(
        Point::new(0.0, 50.0),
        Point::new(100.0, 50.0),
        Rgba::rgb(128, 0, 255),
        5.0,
    );

    let _ = canvas.into_pixmap();
}

// ============================================================================
// State Management Tests
// ============================================================================

#[test]
fn test_save_restore_opacity() {
    let mut canvas = Canvas::new(100, 100, Rgba::WHITE).unwrap();

    assert_eq!(canvas.opacity(), 1.0);

    canvas.save();
    canvas.set_opacity(0.5);
    assert_eq!(canvas.opacity(), 0.5);

    canvas.save();
    canvas.set_opacity(0.25);
    assert_eq!(canvas.opacity(), 0.25);

    canvas.restore();
    assert_eq!(canvas.opacity(), 0.5);

    canvas.restore();
    assert_eq!(canvas.opacity(), 1.0);
}

#[test]
fn test_save_restore_transform() {
    let mut canvas = Canvas::new(100, 100, Rgba::WHITE).unwrap();

    canvas.save();
    canvas.translate(10.0, 20.0);

    let transform = canvas.transform();
    assert!((transform.tx - 10.0).abs() < 0.001);
    assert!((transform.ty - 20.0).abs() < 0.001);

    canvas.restore();

    let transform = canvas.transform();
    assert!((transform.tx).abs() < 0.001);
    assert!((transform.ty).abs() < 0.001);
}

#[test]
fn test_nested_transforms() {
    let mut canvas = Canvas::new(100, 100, Rgba::WHITE).unwrap();

    canvas.save();
    canvas.translate(10.0, 0.0);

    canvas.save();
    canvas.translate(20.0, 0.0);

    // Combined translation should be 30
    let transform = canvas.transform();
    assert!((transform.tx - 30.0).abs() < 0.001);

    canvas.restore();
    canvas.restore();
}

#[test]
fn test_scale_transform() {
    let mut canvas = Canvas::new(100, 100, Rgba::WHITE).unwrap();

    canvas.scale(2.0, 2.0);

    // Draw a 10x10 rect - should appear as 20x20
    canvas.draw_rect(Rect::from_xywh(0.0, 0.0, 10.0, 10.0), Rgba::rgb(255, 0, 0));

    let _ = canvas.into_pixmap();
}

// ============================================================================
// Opacity Tests
// ============================================================================

#[test]
fn test_draw_with_opacity() {
    let mut canvas = Canvas::new(100, 100, Rgba::WHITE).unwrap();

    canvas.set_opacity(0.5);
    canvas.draw_rect(Rect::from_xywh(10.0, 10.0, 80.0, 80.0), Rgba::rgb(255, 0, 0));

    let _ = canvas.into_pixmap();
}

#[test]
fn test_opacity_clamping() {
    let mut canvas = Canvas::new(10, 10, Rgba::WHITE).unwrap();

    canvas.set_opacity(1.5);
    assert_eq!(canvas.opacity(), 1.0);

    canvas.set_opacity(-0.5);
    assert_eq!(canvas.opacity(), 0.0);
}

// ============================================================================
// Clipping Tests
// ============================================================================

#[test]
fn test_clip_rect() {
    let mut canvas = Canvas::new(100, 100, Rgba::WHITE).unwrap();

    // Set clip to center region
    canvas.set_clip(Rect::from_xywh(25.0, 25.0, 50.0, 50.0));

    // Draw rectangle that extends beyond clip
    canvas.draw_rect(Rect::from_xywh(0.0, 0.0, 100.0, 100.0), Rgba::rgb(255, 0, 0));

    // Clear clip
    canvas.clear_clip();

    // Draw another rect - should not be clipped
    canvas.draw_rect(Rect::from_xywh(0.0, 0.0, 10.0, 10.0), Rgba::rgb(0, 0, 255));

    let _ = canvas.into_pixmap();
}

// ============================================================================
// Blend Mode Tests
// ============================================================================

#[test]
fn test_blend_mode_multiply() {
    let mut canvas = Canvas::new(100, 100, Rgba::WHITE).unwrap();

    // Draw background
    canvas.draw_rect(Rect::from_xywh(0.0, 0.0, 100.0, 100.0), Rgba::rgb(200, 200, 200));

    // Set multiply blend
    canvas.set_blend_mode(BlendMode::Multiply);

    // Draw overlapping rect
    canvas.draw_rect(Rect::from_xywh(25.0, 25.0, 50.0, 50.0), Rgba::rgb(255, 100, 100));

    let _ = canvas.into_pixmap();
}

#[test]
fn test_blend_mode_screen() {
    let mut canvas = Canvas::new(100, 100, Rgba::BLACK).unwrap();

    canvas.set_blend_mode(BlendMode::Screen);
    canvas.draw_rect(Rect::from_xywh(20.0, 20.0, 60.0, 60.0), Rgba::rgb(100, 100, 200));

    let _ = canvas.into_pixmap();
}

// ============================================================================
// Border Radii Tests
// ============================================================================

#[test]
fn test_border_radii_constructors() {
    let zero = BorderRadii::ZERO;
    assert!(!zero.has_radius());

    let uniform = BorderRadii::uniform(10.0);
    assert!(uniform.has_radius());
    assert!(uniform.is_uniform());

    let different = BorderRadii::new(1.0, 2.0, 3.0, 4.0);
    assert!(different.has_radius());
    assert!(!different.is_uniform());
}

#[test]
fn test_border_radii_max_radius() {
    let radii = BorderRadii::new(5.0, 10.0, 15.0, 20.0);
    assert_eq!(radii.max_radius(), 20.0);
}

// ============================================================================
// Text Drawing Tests
// ============================================================================

#[test]
fn test_draw_text_empty() {
    let mut canvas = Canvas::new(200, 100, Rgba::WHITE).unwrap();

    // Get a font
    let font_ctx = FontContext::new();

    // Skip if no fonts available
    if font_ctx.get_sans_serif().is_none() {
        return;
    }

    let font = font_ctx.get_sans_serif().unwrap();

    // Draw empty glyphs - should not crash
    canvas.draw_text(Point::new(10.0, 50.0), &[], &font, 16.0, Rgba::BLACK);

    let _ = canvas.into_pixmap();
}

#[test]
fn test_draw_text_with_glyphs() {
    let mut canvas = Canvas::new(200, 50, Rgba::WHITE).unwrap();

    // Get a font
    let font_ctx = FontContext::new();

    // Skip if no fonts available
    if font_ctx.get_sans_serif().is_none() {
        return;
    }

    let font = font_ctx.get_sans_serif().unwrap();

    // Shape some text
    let shaper = TextShaper::new();
    let shaped = shaper
        .shape_text("Hello, World!", &font, 16.0, Script::Latin, TextDirection::Ltr)
        .unwrap();

    // Draw the glyphs
    canvas.draw_text(Point::new(10.0, 30.0), &shaped.glyphs, &font, 16.0, Rgba::BLACK);

    let pixmap = canvas.into_pixmap();
    assert!(pixmap.data().iter().any(|&b| b != 255)); // Some non-white pixels
}

#[test]
fn test_draw_text_colored() {
    let mut canvas = Canvas::new(200, 50, Rgba::WHITE).unwrap();

    let font_ctx = FontContext::new();
    if font_ctx.get_sans_serif().is_none() {
        return;
    }

    let font = font_ctx.get_sans_serif().unwrap();
    let shaper = TextShaper::new();

    let shaped = shaper
        .shape_text("Red Text", &font, 20.0, Script::Latin, TextDirection::Ltr)
        .unwrap();
    canvas.draw_text(
        Point::new(10.0, 35.0),
        &shaped.glyphs,
        &font,
        20.0,
        Rgba::rgb(255, 0, 0),
    );

    let _ = canvas.into_pixmap();
}

#[test]
fn test_draw_text_with_opacity() {
    let mut canvas = Canvas::new(200, 50, Rgba::WHITE).unwrap();

    let font_ctx = FontContext::new();
    if font_ctx.get_sans_serif().is_none() {
        return;
    }

    let font = font_ctx.get_sans_serif().unwrap();
    let shaper = TextShaper::new();

    canvas.set_opacity(0.5);

    let shaped = shaper
        .shape_text("Faded", &font, 24.0, Script::Latin, TextDirection::Ltr)
        .unwrap();
    canvas.draw_text(Point::new(10.0, 35.0), &shaped.glyphs, &font, 24.0, Rgba::BLACK);

    let _ = canvas.into_pixmap();
}

// ============================================================================
// Edge Cases
// ============================================================================

#[test]
fn test_draw_transparent() {
    let mut canvas = Canvas::new(50, 50, Rgba::WHITE).unwrap();

    // Draw with transparent color should be a no-op
    canvas.draw_rect(Rect::from_xywh(0.0, 0.0, 50.0, 50.0), Rgba::TRANSPARENT);

    // Canvas should still be white
    let data = canvas.pixmap().data();
    assert_eq!(data[0], 255);
    assert_eq!(data[1], 255);
    assert_eq!(data[2], 255);
    assert_eq!(data[3], 255);
}

#[test]
fn test_draw_zero_size_rect() {
    let mut canvas = Canvas::new(50, 50, Rgba::WHITE).unwrap();

    // Zero-width rect
    canvas.draw_rect(Rect::from_xywh(10.0, 10.0, 0.0, 20.0), Rgba::rgb(255, 0, 0));

    // Zero-height rect
    canvas.draw_rect(Rect::from_xywh(10.0, 10.0, 20.0, 0.0), Rgba::rgb(255, 0, 0));

    let _ = canvas.into_pixmap();
}

#[test]
fn test_draw_outside_bounds() {
    let mut canvas = Canvas::new(50, 50, Rgba::WHITE).unwrap();

    // Rect completely outside canvas
    canvas.draw_rect(Rect::from_xywh(100.0, 100.0, 20.0, 20.0), Rgba::rgb(255, 0, 0));

    // Rect partially outside
    canvas.draw_rect(Rect::from_xywh(-10.0, -10.0, 30.0, 30.0), Rgba::rgb(0, 0, 255));

    let _ = canvas.into_pixmap();
}

#[test]
fn test_draw_negative_radius_circle() {
    let mut canvas = Canvas::new(50, 50, Rgba::WHITE).unwrap();

    // Should not crash with negative radius
    canvas.draw_circle(Point::new(25.0, 25.0), -10.0, Rgba::rgb(255, 0, 0));

    let _ = canvas.into_pixmap();
}

// ============================================================================
// Complex Rendering Tests
// ============================================================================

#[test]
fn test_complex_scene() {
    let mut canvas = Canvas::new(200, 200, Rgba::rgb(240, 240, 240)).unwrap();

    // Background rectangle
    canvas.draw_rect(Rect::from_xywh(10.0, 10.0, 180.0, 180.0), Rgba::WHITE);

    // Rounded header
    let radii = BorderRadii::new(5.0, 5.0, 0.0, 0.0);
    canvas.draw_rounded_rect(Rect::from_xywh(10.0, 10.0, 180.0, 40.0), radii, Rgba::rgb(51, 102, 204));

    // Content area with border
    canvas.stroke_rect(Rect::from_xywh(20.0, 60.0, 160.0, 120.0), Rgba::rgb(200, 200, 200), 1.0);

    // Decorative circles
    canvas.draw_circle(Point::new(50.0, 100.0), 15.0, Rgba::rgb(255, 100, 100));
    canvas.draw_circle(Point::new(100.0, 100.0), 15.0, Rgba::rgb(100, 255, 100));
    canvas.draw_circle(Point::new(150.0, 100.0), 15.0, Rgba::rgb(100, 100, 255));

    // Lines
    canvas.draw_line(
        Point::new(30.0, 140.0),
        Point::new(170.0, 140.0),
        Rgba::rgb(150, 150, 150),
        1.0,
    );
    canvas.draw_line(
        Point::new(30.0, 160.0),
        Point::new(170.0, 160.0),
        Rgba::rgb(150, 150, 150),
        1.0,
    );

    let pixmap = canvas.into_pixmap();
    assert_eq!(pixmap.width(), 200);
    assert_eq!(pixmap.height(), 200);
}

#[test]
fn test_layered_opacity() {
    let mut canvas = Canvas::new(100, 100, Rgba::WHITE).unwrap();

    // Layer 1: Base rectangle
    canvas.draw_rect(Rect::from_xywh(10.0, 10.0, 80.0, 80.0), Rgba::rgb(255, 0, 0));

    // Layer 2: 50% opacity rectangle
    canvas.save();
    canvas.set_opacity(0.5);
    canvas.draw_rect(Rect::from_xywh(20.0, 20.0, 60.0, 60.0), Rgba::rgb(0, 255, 0));
    canvas.restore();

    // Layer 3: Back to full opacity
    canvas.draw_rect(Rect::from_xywh(35.0, 35.0, 30.0, 30.0), Rgba::rgb(0, 0, 255));

    let _ = canvas.into_pixmap();
}

#[test]
fn test_transformed_rendering() {
    let mut canvas = Canvas::new(100, 100, Rgba::WHITE).unwrap();

    // Draw with translation
    canvas.save();
    canvas.translate(50.0, 50.0);
    canvas.draw_rect(Rect::from_xywh(-10.0, -10.0, 20.0, 20.0), Rgba::rgb(255, 0, 0));
    canvas.restore();

    // Draw with scale
    canvas.save();
    canvas.translate(25.0, 25.0);
    canvas.scale(0.5, 0.5);
    canvas.draw_rect(Rect::from_xywh(0.0, 0.0, 50.0, 50.0), Rgba::rgb(0, 0, 255));
    canvas.restore();

    let _ = canvas.into_pixmap();
}
