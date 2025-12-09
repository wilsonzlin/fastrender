//! Integration tests for text rasterization
//!
//! Tests the text rasterization module which converts shaped text
//! (glyph IDs + positions) into rendered pixels using font outlines
//! and tiny-skia.

use fastrender::text::font_loader::FontContext;
use fastrender::text::pipeline::{Direction, GlyphPosition, ShapedRun, ShapingPipeline};
use fastrender::{ComputedStyle, Rgba};
use fastrender::{GlyphCache, TextRasterizer};
use std::sync::Arc;
use tiny_skia::Pixmap;

// ============================================================================
// Test Helpers
// ============================================================================

fn get_test_font() -> Option<fastrender::text::font_db::LoadedFont> {
    let ctx = FontContext::new();
    ctx.get_sans_serif()
}

fn create_test_pixmap(width: u32, height: u32) -> Pixmap {
    let mut pixmap = Pixmap::new(width, height).unwrap();
    pixmap.fill(tiny_skia::Color::WHITE);
    pixmap
}

fn has_changed_pixels(pixmap: &Pixmap) -> bool {
    // Check if any pixel is not white (indicating something was rendered)
    pixmap
        .data()
        .chunks(4)
        .any(|pixel| pixel[0] != 255 || pixel[1] != 255 || pixel[2] != 255)
}

// ============================================================================
// TextRasterizer Tests
// ============================================================================

#[test]
fn test_rasterizer_creation() {
    let rasterizer = TextRasterizer::new();
    assert_eq!(rasterizer.cache_size(), 0);
}

#[test]
fn test_rasterizer_with_custom_capacity() {
    let rasterizer = TextRasterizer::with_cache_capacity(1000);
    assert_eq!(rasterizer.cache_size(), 0);
}

#[test]
fn test_rasterizer_clear_cache() {
    let mut rasterizer = TextRasterizer::new();
    rasterizer.clear_cache();
    assert_eq!(rasterizer.cache_size(), 0);
}

// ============================================================================
// GlyphCache Tests
// ============================================================================

#[test]
fn test_glyph_cache_creation() {
    let cache = GlyphCache::new();
    assert!(cache.is_empty());
    assert_eq!(cache.len(), 0);
}

#[test]
fn test_glyph_cache_with_capacity() {
    let cache = GlyphCache::with_capacity(500);
    assert!(cache.is_empty());
}

#[test]
fn test_glyph_cache_clear() {
    let mut cache = GlyphCache::new();
    cache.clear();
    assert!(cache.is_empty());
}

// ============================================================================
// Glyph Rendering Tests
// ============================================================================

#[test]
fn test_render_single_glyph() {
    let font = match get_test_font() {
        Some(f) => f,
        None => return, // Skip if no fonts available
    };

    let mut pixmap = create_test_pixmap(100, 100);

    // Get glyph ID for 'A'
    let face = font.as_ttf_face().unwrap();
    let glyph_id = face.glyph_index('A').map(|g| g.0 as u32).unwrap_or(0);

    let glyphs = vec![GlyphPosition {
        glyph_id,
        cluster: 0,
        x_offset: 0.0,
        y_offset: 0.0,
        x_advance: 10.0,
        y_advance: 0.0,
    }];

    let mut rasterizer = TextRasterizer::new();
    let result = rasterizer.render_glyphs(&glyphs, &font, 24.0, 20.0, 60.0, Rgba::BLACK, &mut pixmap);

    assert!(result.is_ok());
    // The glyph should have been rendered (pixels changed)
    assert!(has_changed_pixels(&pixmap));
}

#[test]
fn test_render_multiple_glyphs() {
    let font = match get_test_font() {
        Some(f) => f,
        None => return,
    };

    let mut pixmap = create_test_pixmap(200, 100);
    let face = font.as_ttf_face().unwrap();

    let text = "Hello";
    let glyphs: Vec<GlyphPosition> = text
        .chars()
        .enumerate()
        .filter_map(|(i, c)| {
            let glyph_id = face.glyph_index(c)?.0 as u32;
            Some(GlyphPosition {
                glyph_id,
                cluster: i as u32,
                x_offset: 0.0,
                y_offset: 0.0,
                x_advance: 12.0,
                y_advance: 0.0,
            })
        })
        .collect();

    let mut rasterizer = TextRasterizer::new();
    let result = rasterizer.render_glyphs(&glyphs, &font, 24.0, 10.0, 60.0, Rgba::BLACK, &mut pixmap);

    assert!(result.is_ok());
    assert!(has_changed_pixels(&pixmap));
}

#[test]
fn test_render_with_different_colors() {
    let font = match get_test_font() {
        Some(f) => f,
        None => return,
    };

    let face = font.as_ttf_face().unwrap();
    let glyph_id = face.glyph_index('X').map(|g| g.0 as u32).unwrap_or(0);

    let glyphs = vec![GlyphPosition {
        glyph_id,
        cluster: 0,
        x_offset: 0.0,
        y_offset: 0.0,
        x_advance: 10.0,
        y_advance: 0.0,
    }];

    // Test with red color
    let red = Rgba::from_rgba8(255, 0, 0, 255);
    let mut pixmap_red = create_test_pixmap(50, 50);
    let mut rasterizer = TextRasterizer::new();
    let _ = rasterizer.render_glyphs(&glyphs, &font, 20.0, 10.0, 35.0, red, &mut pixmap_red);

    // Test with blue color
    let blue = Rgba::from_rgba8(0, 0, 255, 255);
    let mut pixmap_blue = create_test_pixmap(50, 50);
    let _ = rasterizer.render_glyphs(&glyphs, &font, 20.0, 10.0, 35.0, blue, &mut pixmap_blue);

    // Both should have rendered something
    assert!(has_changed_pixels(&pixmap_red));
    assert!(has_changed_pixels(&pixmap_blue));
}

#[test]
fn test_render_at_different_sizes() {
    let font = match get_test_font() {
        Some(f) => f,
        None => return,
    };

    let face = font.as_ttf_face().unwrap();
    let glyph_id = face.glyph_index('M').map(|g| g.0 as u32).unwrap_or(0);

    let glyphs = vec![GlyphPosition {
        glyph_id,
        cluster: 0,
        x_offset: 0.0,
        y_offset: 0.0,
        x_advance: 10.0,
        y_advance: 0.0,
    }];

    let mut rasterizer = TextRasterizer::new();

    // Small size
    let mut pixmap_small = create_test_pixmap(30, 20);
    let _ = rasterizer.render_glyphs(&glyphs, &font, 12.0, 5.0, 15.0, Rgba::BLACK, &mut pixmap_small);

    // Large size
    let mut pixmap_large = create_test_pixmap(100, 80);
    let _ = rasterizer.render_glyphs(&glyphs, &font, 48.0, 10.0, 60.0, Rgba::BLACK, &mut pixmap_large);

    assert!(has_changed_pixels(&pixmap_small));
    assert!(has_changed_pixels(&pixmap_large));
}

#[test]
fn test_render_space_character() {
    let font = match get_test_font() {
        Some(f) => f,
        None => return,
    };

    let face = font.as_ttf_face().unwrap();
    let glyph_id = face.glyph_index(' ').map(|g| g.0 as u32).unwrap_or(0);

    let glyphs = vec![GlyphPosition {
        glyph_id,
        cluster: 0,
        x_offset: 0.0,
        y_offset: 0.0,
        x_advance: 8.0,
        y_advance: 0.0,
    }];

    let mut pixmap = create_test_pixmap(50, 50);
    let mut rasterizer = TextRasterizer::new();
    let result = rasterizer.render_glyphs(&glyphs, &font, 16.0, 10.0, 30.0, Rgba::BLACK, &mut pixmap);

    // Should succeed even though space has no visible outline
    assert!(result.is_ok());
}

#[test]
fn test_render_with_offsets() {
    let font = match get_test_font() {
        Some(f) => f,
        None => return,
    };

    let face = font.as_ttf_face().unwrap();
    let glyph_id = face.glyph_index('Q').map(|g| g.0 as u32).unwrap_or(0);

    // Glyph with x and y offsets
    let glyphs = vec![GlyphPosition {
        glyph_id,
        cluster: 0,
        x_offset: 5.0,
        y_offset: -3.0,
        x_advance: 12.0,
        y_advance: 0.0,
    }];

    let mut pixmap = create_test_pixmap(100, 100);
    let mut rasterizer = TextRasterizer::new();
    let result = rasterizer.render_glyphs(&glyphs, &font, 24.0, 20.0, 60.0, Rgba::BLACK, &mut pixmap);

    assert!(result.is_ok());
    assert!(has_changed_pixels(&pixmap));
}

#[test]
fn test_render_empty_glyph_list() {
    let font = match get_test_font() {
        Some(f) => f,
        None => return,
    };

    let glyphs: Vec<GlyphPosition> = vec![];
    let mut pixmap = create_test_pixmap(50, 50);
    let mut rasterizer = TextRasterizer::new();

    let result = rasterizer.render_glyphs(&glyphs, &font, 16.0, 10.0, 30.0, Rgba::BLACK, &mut pixmap);

    assert!(result.is_ok());
    assert_eq!(result.unwrap(), 0.0); // No advance for empty list
}

// ============================================================================
// ShapedRun Rendering Tests
// ============================================================================

#[test]
fn test_render_shaped_run() {
    let font = match get_test_font() {
        Some(f) => f,
        None => return,
    };

    let face = font.as_ttf_face().unwrap();

    let glyphs: Vec<GlyphPosition> = "ABC"
        .chars()
        .enumerate()
        .filter_map(|(i, c)| {
            let glyph_id = face.glyph_index(c)?.0 as u32;
            Some(GlyphPosition {
                glyph_id,
                cluster: i as u32,
                x_offset: 0.0,
                y_offset: 0.0,
                x_advance: 10.0,
                y_advance: 0.0,
            })
        })
        .collect();

    let run = ShapedRun {
        text: "ABC".to_string(),
        start: 0,
        end: 3,
        glyphs,
        direction: Direction::LeftToRight,
        level: 0,
        advance: 30.0,
        font: Arc::new(font),
        font_size: 24.0,
        language: None,
    };

    let mut pixmap = create_test_pixmap(150, 100);
    let mut rasterizer = TextRasterizer::new();

    let result = rasterizer.render_shaped_run(&run, 10.0, 60.0, Rgba::BLACK, &mut pixmap);

    assert!(result.is_ok());
    assert!(has_changed_pixels(&pixmap));
}

#[test]
fn test_render_multiple_runs() {
    let font = match get_test_font() {
        Some(f) => f,
        None => return,
    };

    let face = font.as_ttf_face().unwrap();

    let create_run = |text: &str, start: usize| -> ShapedRun {
        let glyphs: Vec<GlyphPosition> = text
            .chars()
            .enumerate()
            .filter_map(|(i, c)| {
                let glyph_id = face.glyph_index(c)?.0 as u32;
                Some(GlyphPosition {
                    glyph_id,
                    cluster: (start + i) as u32,
                    x_offset: 0.0,
                    y_offset: 0.0,
                    x_advance: 10.0,
                    y_advance: 0.0,
                })
            })
            .collect();

        let advance = glyphs.len() as f32 * 10.0;

        ShapedRun {
            text: text.to_string(),
            start,
            end: start + text.len(),
            glyphs,
            direction: Direction::LeftToRight,
            level: 0,
            advance,
            font: Arc::new(font.clone()),
            font_size: 20.0,
            language: None,
        }
    };

    let runs = vec![create_run("Hello", 0), create_run(" ", 5), create_run("World", 6)];

    let mut pixmap = create_test_pixmap(300, 100);
    let mut rasterizer = TextRasterizer::new();

    let result = rasterizer.render_runs(&runs, 10.0, 60.0, Rgba::BLACK, &mut pixmap);

    assert!(result.is_ok());
    assert!(has_changed_pixels(&pixmap));
}

// ============================================================================
// Integration with Shaping Pipeline Tests
// ============================================================================

#[test]
fn test_render_pipeline_output() {
    let font_ctx = FontContext::new();
    if !font_ctx.has_fonts() {
        return;
    }

    let pipeline = ShapingPipeline::new();
    let style = ComputedStyle::default();

    let shaped_runs = match pipeline.shape("Hello, World!", &style, &font_ctx) {
        Ok(runs) => runs,
        Err(_) => return,
    };

    let mut pixmap = create_test_pixmap(300, 100);
    let mut rasterizer = TextRasterizer::new();

    let result = rasterizer.render_runs(&shaped_runs, 10.0, 60.0, Rgba::BLACK, &mut pixmap);

    assert!(result.is_ok());
    assert!(has_changed_pixels(&pixmap));
}

#[test]
fn test_render_unicode_text() {
    let font_ctx = FontContext::new();
    if !font_ctx.has_fonts() {
        return;
    }

    let pipeline = ShapingPipeline::new();
    let style = ComputedStyle::default();

    // Test with accented characters (common in European languages)
    let text = "Café résumé";
    let shaped_runs = match pipeline.shape(text, &style, &font_ctx) {
        Ok(runs) => runs,
        Err(_) => return,
    };

    let mut pixmap = create_test_pixmap(250, 100);
    let mut rasterizer = TextRasterizer::new();

    let result = rasterizer.render_runs(&shaped_runs, 10.0, 60.0, Rgba::BLACK, &mut pixmap);

    assert!(result.is_ok());
}

#[test]
fn test_render_numbers_and_punctuation() {
    let font_ctx = FontContext::new();
    if !font_ctx.has_fonts() {
        return;
    }

    let pipeline = ShapingPipeline::new();
    let style = ComputedStyle::default();

    let text = "123, 456.789!";
    let shaped_runs = match pipeline.shape(text, &style, &font_ctx) {
        Ok(runs) => runs,
        Err(_) => return,
    };

    let mut pixmap = create_test_pixmap(250, 100);
    let mut rasterizer = TextRasterizer::new();

    let result = rasterizer.render_runs(&shaped_runs, 10.0, 60.0, Rgba::BLACK, &mut pixmap);

    assert!(result.is_ok());
    assert!(has_changed_pixels(&pixmap));
}

// ============================================================================
// Color Tests
// ============================================================================

#[test]
fn test_color_constants() {
    assert_eq!(Rgba::BLACK.r, 0);
    assert_eq!(Rgba::BLACK.g, 0);
    assert_eq!(Rgba::BLACK.b, 0);
    assert_eq!(Rgba::BLACK.a, 1.0);

    assert_eq!(Rgba::WHITE.r, 255);
    assert_eq!(Rgba::WHITE.g, 255);
    assert_eq!(Rgba::WHITE.b, 255);
    assert_eq!(Rgba::WHITE.a, 1.0);
}

#[test]
fn test_color_rgba_constructor() {
    let color = Rgba::from_rgba8(100, 150, 200, 128);
    assert_eq!(color.r, 100);
    assert_eq!(color.g, 150);
    assert_eq!(color.b, 200);
    // from_rgba8 converts u8 alpha to f32: 128/255 ≈ 0.502
    assert!((color.a - 128.0 / 255.0).abs() < 0.01);
}

#[test]
fn test_color_rgb_constructor() {
    let color = Rgba::rgb(50, 100, 150);
    assert_eq!(color.r, 50);
    assert_eq!(color.g, 100);
    assert_eq!(color.b, 150);
    assert_eq!(color.a, 1.0); // Full opacity
}

#[test]
fn test_render_with_transparency() {
    let font = match get_test_font() {
        Some(f) => f,
        None => return,
    };

    let face = font.as_ttf_face().unwrap();
    let glyph_id = face.glyph_index('T').map(|g| g.0 as u32).unwrap_or(0);

    let glyphs = vec![GlyphPosition {
        glyph_id,
        cluster: 0,
        x_offset: 0.0,
        y_offset: 0.0,
        x_advance: 10.0,
        y_advance: 0.0,
    }];

    // Semi-transparent color
    let semi_transparent = Rgba::from_rgba8(0, 0, 0, 128);
    let mut pixmap = create_test_pixmap(50, 50);
    let mut rasterizer = TextRasterizer::new();

    let result = rasterizer.render_glyphs(&glyphs, &font, 20.0, 10.0, 35.0, semi_transparent, &mut pixmap);

    assert!(result.is_ok());
    assert!(has_changed_pixels(&pixmap));
}

// ============================================================================
// Edge Cases and Boundary Tests
// ============================================================================

#[test]
fn test_render_very_small_font_size() {
    let font = match get_test_font() {
        Some(f) => f,
        None => return,
    };

    let face = font.as_ttf_face().unwrap();
    let glyph_id = face.glyph_index('X').map(|g| g.0 as u32).unwrap_or(0);

    let glyphs = vec![GlyphPosition {
        glyph_id,
        cluster: 0,
        x_offset: 0.0,
        y_offset: 0.0,
        x_advance: 2.0,
        y_advance: 0.0,
    }];

    let mut pixmap = create_test_pixmap(20, 20);
    let mut rasterizer = TextRasterizer::new();

    // Very small font size (4px)
    let result = rasterizer.render_glyphs(&glyphs, &font, 4.0, 5.0, 15.0, Rgba::BLACK, &mut pixmap);

    assert!(result.is_ok());
}

#[test]
fn test_render_large_font_size() {
    let font = match get_test_font() {
        Some(f) => f,
        None => return,
    };

    let face = font.as_ttf_face().unwrap();
    let glyph_id = face.glyph_index('X').map(|g| g.0 as u32).unwrap_or(0);

    let glyphs = vec![GlyphPosition {
        glyph_id,
        cluster: 0,
        x_offset: 0.0,
        y_offset: 0.0,
        x_advance: 50.0,
        y_advance: 0.0,
    }];

    let mut pixmap = create_test_pixmap(200, 150);
    let mut rasterizer = TextRasterizer::new();

    // Large font size (72px)
    let result = rasterizer.render_glyphs(&glyphs, &font, 72.0, 30.0, 100.0, Rgba::BLACK, &mut pixmap);

    assert!(result.is_ok());
    assert!(has_changed_pixels(&pixmap));
}

#[test]
fn test_render_at_edge_of_pixmap() {
    let font = match get_test_font() {
        Some(f) => f,
        None => return,
    };

    let face = font.as_ttf_face().unwrap();
    let glyph_id = face.glyph_index('X').map(|g| g.0 as u32).unwrap_or(0);

    let glyphs = vec![GlyphPosition {
        glyph_id,
        cluster: 0,
        x_offset: 0.0,
        y_offset: 0.0,
        x_advance: 10.0,
        y_advance: 0.0,
    }];

    let mut pixmap = create_test_pixmap(50, 50);
    let mut rasterizer = TextRasterizer::new();

    // Render near the edge - should not crash even if partially clipped
    let result = rasterizer.render_glyphs(&glyphs, &font, 16.0, 45.0, 10.0, Rgba::BLACK, &mut pixmap);

    assert!(result.is_ok());
}

// ============================================================================
// Performance / Cache Tests
// ============================================================================

#[test]
fn test_repeated_rendering_uses_cache() {
    let font = match get_test_font() {
        Some(f) => f,
        None => return,
    };

    let face = font.as_ttf_face().unwrap();
    let glyph_id = face.glyph_index('A').map(|g| g.0 as u32).unwrap_or(0);

    let glyphs = vec![GlyphPosition {
        glyph_id,
        cluster: 0,
        x_offset: 0.0,
        y_offset: 0.0,
        x_advance: 10.0,
        y_advance: 0.0,
    }];

    let mut rasterizer = TextRasterizer::new();

    // Render the same glyph multiple times
    for _ in 0..10 {
        let mut pixmap = create_test_pixmap(50, 50);
        let _ = rasterizer.render_glyphs(&glyphs, &font, 16.0, 10.0, 35.0, Rgba::BLACK, &mut pixmap);
    }

    // Cache should have been used (though we can't directly verify hits,
    // we ensure no crashes and consistent behavior)
}
