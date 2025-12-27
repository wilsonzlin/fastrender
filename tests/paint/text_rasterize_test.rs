//! Integration tests for text rasterization
//!
//! Tests the text rasterization module which converts shaped text
//! (glyph IDs + positions) into rendered pixels using font outlines
//! and tiny-skia.

use fastrender::text::font_loader::FontContext;
use fastrender::text::pipeline::Direction;
use fastrender::text::pipeline::GlyphPosition;
use fastrender::text::pipeline::RunRotation;
use fastrender::text::pipeline::ShapedRun;
use fastrender::text::pipeline::ShapingPipeline;
use fastrender::ComputedStyle;
use fastrender::GlyphCache;
use fastrender::Rgba;
use fastrender::TextRasterizer;
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

fn painted_bounds(pixmap: &Pixmap) -> Option<(u32, u32, u32, u32)> {
  let mut min_x = u32::MAX;
  let mut min_y = u32::MAX;
  let mut max_x = 0;
  let mut max_y = 0;

  for (idx, chunk) in pixmap.data().chunks(4).enumerate() {
    if chunk[0] == 255 && chunk[1] == 255 && chunk[2] == 255 {
      continue;
    }
    let x = (idx as u32) % pixmap.width();
    let y = (idx as u32) / pixmap.width();
    min_x = min_x.min(x);
    min_y = min_y.min(y);
    max_x = max_x.max(x);
    max_y = max_y.max(y);
  }

  if min_x == u32::MAX {
    None
  } else {
    Some((min_x, max_x, min_y, max_y))
  }
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
fn renders_rotated_runs_for_vertical_text() {
  if get_test_font().is_none() {
    return;
  }

  let mut style = ComputedStyle::default();
  style.writing_mode = fastrender::style::types::WritingMode::HorizontalTb;
  let style = Arc::new(style);

  let pipeline = ShapingPipeline::new();
  let font_ctx = FontContext::new();
  let runs = pipeline.shape("HI", &style, &font_ctx).unwrap();
  let run = &runs[0];

  let mut rasterizer = TextRasterizer::new();
  let mut pixmap_normal = create_test_pixmap(120, 120);
  let mut pixmap_rotated = create_test_pixmap(120, 120);

  rasterizer
    .render_shaped_run(run, 20.0, 60.0, Rgba::BLACK, &mut pixmap_normal)
    .unwrap();

  let mut rotated = run.clone();
  rotated.rotation = RunRotation::Cw90;
  rasterizer
    .render_shaped_run(&rotated, 20.0, 60.0, Rgba::BLACK, &mut pixmap_rotated)
    .unwrap();

  let bbox_normal = painted_bounds(&pixmap_normal).expect("normal run should paint");
  let bbox_rotated = painted_bounds(&pixmap_rotated).expect("rotated run should paint");

  let normal_w = bbox_normal.1 - bbox_normal.0;
  let normal_h = bbox_normal.3 - bbox_normal.2;
  let rotated_w = bbox_rotated.1 - bbox_rotated.0;
  let rotated_h = bbox_rotated.3 - bbox_rotated.2;

  assert!(
    rotated_h > normal_h,
    "rotated text should extend further vertically"
  );
  assert!(
    rotated_w < normal_w,
    "rotated text should shrink horizontally when rotated"
  );
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
  let _ = rasterizer.render_glyphs(
    &glyphs,
    &font,
    12.0,
    5.0,
    15.0,
    Rgba::BLACK,
    &mut pixmap_small,
  );

  // Large size
  let mut pixmap_large = create_test_pixmap(100, 80);
  let _ = rasterizer.render_glyphs(
    &glyphs,
    &font,
    48.0,
    10.0,
    60.0,
    Rgba::BLACK,
    &mut pixmap_large,
  );

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
    baseline_shift: 0.0,
    language: None,
    synthetic_bold: 0.0,
    synthetic_oblique: 0.0,
    rotation: RunRotation::None,
    palette_index: 0,
    variations: Vec::new(),
    scale: 1.0,
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
      baseline_shift: 0.0,
      language: None,
      synthetic_bold: 0.0,
      synthetic_oblique: 0.0,
      rotation: RunRotation::None,
      palette_index: 0,
      variations: Vec::new(),
      scale: 1.0,
    }
  };

  let runs = vec![
    create_run("Hello", 0),
    create_run(" ", 5),
    create_run("World", 6),
  ];

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

  let result = rasterizer.render_glyphs(
    &glyphs,
    &font,
    20.0,
    10.0,
    35.0,
    semi_transparent,
    &mut pixmap,
  );

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
  let result =
    rasterizer.render_glyphs(&glyphs, &font, 72.0, 30.0, 100.0, Rgba::BLACK, &mut pixmap);

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

#[test]
fn test_vertical_rendering_extents() {
  let pipeline = ShapingPipeline::new();
  let font_ctx = FontContext::new();

  let mut vertical_style = ComputedStyle::default();
  vertical_style.writing_mode = fastrender::style::types::WritingMode::VerticalRl;
  vertical_style.text_orientation = fastrender::style::types::TextOrientation::Upright;

  let vertical_runs = match pipeline.shape("縦書き", &vertical_style, &font_ctx) {
    Ok(runs) => runs,
    Err(_) => return,
  };
  let horizontal_runs = match pipeline.shape("縦書き", &ComputedStyle::default(), &font_ctx) {
    Ok(runs) => runs,
    Err(_) => return,
  };

  let mut rasterizer = TextRasterizer::new();
  let mut vertical = create_test_pixmap(120, 120);
  let mut horizontal = create_test_pixmap(120, 120);

  for run in &vertical_runs {
    let _ = rasterizer.render_shaped_run(run, 60.0, 10.0, Rgba::BLACK, &mut vertical);
  }
  for run in &horizontal_runs {
    let _ = rasterizer.render_shaped_run(run, 10.0, 70.0, Rgba::BLACK, &mut horizontal);
  }

  let Some((h_min_x, h_max_x, h_min_y, h_max_y)) = painted_bounds(&horizontal) else {
    return;
  };
  let Some((v_min_x, v_max_x, v_min_y, v_max_y)) = painted_bounds(&vertical) else {
    return;
  };

  let h_width = h_max_x.saturating_sub(h_min_x);
  let h_height = h_max_y.saturating_sub(h_min_y);
  let v_width = v_max_x.saturating_sub(v_min_x);
  let v_height = v_max_y.saturating_sub(v_min_y);

  assert!(v_height > h_height, "vertical text should extend along the y axis");
  assert!(v_width < h_width, "vertical text should occupy less horizontal span than horizontal text");
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
  rasterizer.reset_cache_stats();

  // Render the same glyph multiple times
  for _ in 0..10 {
    let mut pixmap = create_test_pixmap(50, 50);
    let _ = rasterizer.render_glyphs(&glyphs, &font, 16.0, 10.0, 35.0, Rgba::BLACK, &mut pixmap);
  }

  let stats = rasterizer.cache_stats();
  assert_eq!(stats.misses, 1);
  assert!(stats.hits >= 9);
}
