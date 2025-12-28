//! Integration tests for the Canvas module
//!
//! These tests verify the Canvas wrapper for tiny-skia works correctly
//! for real-world rendering scenarios.

use fastrender::geometry::Point;
use fastrender::geometry::Rect;
use fastrender::image_compare::{compare_images, decode_png, encode_png, CompareConfig};
use fastrender::paint::display_list::BorderRadius;
use fastrender::style::types::FontPalette;
use fastrender::text::color_fonts::ColorFontRenderer;
use fastrender::text::font_db::FontDatabase;
use fastrender::text::variations::FontVariation;
use fastrender::BlendMode;
use fastrender::BorderRadii;
use fastrender::Canvas;
use fastrender::ComputedStyle;
use fastrender::FontContext;
use fastrender::Rgba;
use fastrender::ShapingPipeline;
use image::RgbaImage;
use std::collections::HashSet;
use std::path::PathBuf;
use std::sync::Arc;
use tiny_skia::Pixmap;

fn pixmap_to_rgba_image(pixmap: &Pixmap) -> RgbaImage {
  let width = pixmap.width();
  let height = pixmap.height();
  let mut rgba = RgbaImage::new(width, height);

  for (dst, src) in rgba
    .as_mut()
    .chunks_exact_mut(4)
    .zip(pixmap.data().chunks_exact(4))
  {
    let b = src[0];
    let g = src[1];
    let r = src[2];
    let a = src[3];

    if a == 0 {
      dst.copy_from_slice(&[0, 0, 0, 0]);
      continue;
    }

    let alpha = a as f32 / 255.0;
    dst[0] = ((r as f32 / alpha).min(255.0)) as u8;
    dst[1] = ((g as f32 / alpha).min(255.0)) as u8;
    dst[2] = ((b as f32 / alpha).min(255.0)) as u8;
    dst[3] = a;
  }

  rgba
}

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
  canvas.draw_rect(
    Rect::from_xywh(25.0, 25.0, 50.0, 50.0),
    Rgba::rgb(0, 255, 0),
  );
  canvas.draw_rect(
    Rect::from_xywh(50.0, 50.0, 50.0, 50.0),
    Rgba::rgb(0, 0, 255),
  );

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
  let radii = BorderRadii::new(
    BorderRadius::uniform(5.0),
    BorderRadius::uniform(10.0),
    BorderRadius::uniform(15.0),
    BorderRadius::uniform(20.0),
  );
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

  canvas.draw_line(
    Point::new(10.0, 10.0),
    Point::new(90.0, 90.0),
    Rgba::BLACK,
    1.0,
  );

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
  canvas.draw_rect(
    Rect::from_xywh(10.0, 10.0, 80.0, 80.0),
    Rgba::rgb(255, 0, 0),
  );

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
  canvas.draw_rect(
    Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
    Rgba::rgb(255, 0, 0),
  );

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
  canvas.draw_rect(
    Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
    Rgba::rgb(200, 200, 200),
  );

  // Set multiply blend
  canvas.set_blend_mode(BlendMode::Multiply);

  // Draw overlapping rect
  canvas.draw_rect(
    Rect::from_xywh(25.0, 25.0, 50.0, 50.0),
    Rgba::rgb(255, 100, 100),
  );

  let _ = canvas.into_pixmap();
}

#[test]
fn test_blend_mode_screen() {
  let mut canvas = Canvas::new(100, 100, Rgba::BLACK).unwrap();

  canvas.set_blend_mode(BlendMode::Screen);
  canvas.draw_rect(
    Rect::from_xywh(20.0, 20.0, 60.0, 60.0),
    Rgba::rgb(100, 100, 200),
  );

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

  let different = BorderRadii::new(
    BorderRadius::uniform(1.0),
    BorderRadius::uniform(2.0),
    BorderRadius::uniform(3.0),
    BorderRadius::uniform(4.0),
  );
  assert!(different.has_radius());
  assert!(!different.is_uniform());
}

#[test]
fn test_border_radii_max_radius() {
  let radii = BorderRadii::new(
    BorderRadius::uniform(5.0),
    BorderRadius::uniform(10.0),
    BorderRadius::uniform(15.0),
    BorderRadius::uniform(20.0),
  );
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
  canvas.draw_text(
    Point::new(10.0, 50.0),
    &[],
    &font,
    16.0,
    Rgba::BLACK,
    0.0,
    0.0,
    0,
    &[],
  );

  let _ = canvas.into_pixmap();
}

#[test]
fn test_draw_text_with_glyphs() {
  let mut canvas = Canvas::new(200, 50, Rgba::WHITE).unwrap();

  // Get a font
  let font_ctx = FontContext::new();

  if !font_ctx.has_fonts() {
    return;
  }

  let pipeline = ShapingPipeline::new();
  let style = ComputedStyle::default();
  let shaped = match pipeline.shape("Hello, World!", &style, &font_ctx) {
    Ok(runs) => runs,
    Err(_) => return,
  };
  let Some(run) = shaped.first() else {
    return;
  };
  let variations: Vec<_> = run
    .variations
    .iter()
    .copied()
    .map(FontVariation::from)
    .collect();

  // Draw the glyphs
  canvas.draw_text(
    Point::new(10.0, 30.0),
    &run.glyphs,
    &run.font,
    run.font_size,
    Rgba::BLACK,
    run.synthetic_bold,
    run.synthetic_oblique,
    run.palette_index,
    &variations,
  );

  let pixmap = canvas.into_pixmap();
  assert!(pixmap.data().iter().any(|&b| b != 255)); // Some non-white pixels
}

#[test]
fn test_draw_text_colored() {
  let mut canvas = Canvas::new(200, 50, Rgba::WHITE).unwrap();

  let font_ctx = FontContext::new();
  if !font_ctx.has_fonts() {
    return;
  }

  let pipeline = ShapingPipeline::new();
  let style = ComputedStyle::default();
  let shaped = match pipeline.shape("Red Text", &style, &font_ctx) {
    Ok(runs) => runs,
    Err(_) => return,
  };
  let Some(run) = shaped.first() else {
    return;
  };
  let variations: Vec<_> = run
    .variations
    .iter()
    .copied()
    .map(FontVariation::from)
    .collect();
  canvas.draw_text(
    Point::new(10.0, 35.0),
    &run.glyphs,
    &run.font,
    run.font_size,
    Rgba::rgb(255, 0, 0),
    run.synthetic_bold,
    run.synthetic_oblique,
    run.palette_index,
    &variations,
  );

  let _ = canvas.into_pixmap();
}

#[test]
fn test_draw_text_with_opacity() {
  let mut canvas = Canvas::new(200, 50, Rgba::WHITE).unwrap();

  let font_ctx = FontContext::new();
  if !font_ctx.has_fonts() {
    return;
  }

  let pipeline = ShapingPipeline::new();
  let style = ComputedStyle::default();

  canvas.set_opacity(0.5);

  let shaped = match pipeline.shape("Faded", &style, &font_ctx) {
    Ok(runs) => runs,
    Err(_) => return,
  };
  let Some(run) = shaped.first() else {
    return;
  };
  let variations: Vec<_> = run
    .variations
    .iter()
    .copied()
    .map(FontVariation::from)
    .collect();
  canvas.draw_text(
    Point::new(10.0, 35.0),
    &run.glyphs,
    &run.font,
    run.font_size,
    Rgba::BLACK,
    run.synthetic_bold,
    run.synthetic_oblique,
    run.palette_index,
    &variations,
  );

  let _ = canvas.into_pixmap();
}

#[test]
fn canvas_renders_color_fonts() {
  let font_path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fonts/ColorTestCOLR.ttf");
  let font_bytes = std::fs::read(&font_path).expect("read color font fixture");
  let mut db = FontDatabase::empty();
  db.load_font_data(font_bytes)
    .expect("load color font into database");
  let font_ctx = FontContext::with_database(Arc::new(db));

  let mut style = ComputedStyle::default();
  style.font_family = vec!["ColorTestCOLR".to_string()];
  style.font_size = 32.0;

  let pipeline = ShapingPipeline::new();
  let shaped = pipeline
    .shape("A", &style, &font_ctx)
    .expect("shape color glyph");
  let run = shaped.first().expect("color run");
  let variations: Vec<_> = run
    .variations
    .iter()
    .copied()
    .map(FontVariation::from)
    .collect();

  let mut canvas = Canvas::new(64, 64, Rgba::WHITE).unwrap();
  canvas.draw_text(
    Point::new(12.0, 48.0),
    &run.glyphs,
    &run.font,
    run.font_size * run.scale,
    Rgba::BLACK,
    run.synthetic_bold,
    run.synthetic_oblique,
    run.palette_index,
    &variations,
  );
  let pixmap = canvas.into_pixmap();
  let actual_image = pixmap_to_rgba_image(&pixmap);
  let actual_png = encode_png(&actual_image).expect("encode actual png");

  let golden_path =
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/golden/canvas_color_font.png");
  if std::env::var("UPDATE_GOLDEN").is_ok() {
    std::fs::write(&golden_path, &actual_png).expect("write golden");
  }
  let expected_png = std::fs::read(&golden_path).expect("read golden");
  let expected_image = decode_png(&expected_png).expect("decode golden");

  let diff = compare_images(&actual_image, &expected_image, &CompareConfig::lenient());
  assert!(
    diff.is_match(),
    "color font raster mismatch: {}",
    diff.summary()
  );

  let unique_colors: HashSet<(u8, u8, u8)> = pixmap
    .data()
    .chunks_exact(4)
    .map(|c| (c[2], c[1], c[0]))
    .collect();
  assert!(
    unique_colors.len() > 2,
    "expected multiple colors in color glyph rendering"
  );
}

#[test]
fn canvas_respects_font_palette() {
  let font_path =
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/fonts/PaletteTestCOLRv1.ttf");
  let font_bytes = std::fs::read(&font_path).expect("read palette font fixture");
  let mut db = FontDatabase::empty();
  db.load_font_data(font_bytes)
    .expect("load palette font into database");
  let font_ctx = FontContext::with_database(Arc::new(db));

  let mut style = ComputedStyle::default();
  style.font_family = vec!["PaletteTestCOLRv1".to_string()];
  style.font_size = 48.0;
  style.root_font_size = style.font_size;
  style.font_palette = FontPalette::Dark;

  let pipeline = ShapingPipeline::new();
  let runs = pipeline
    .shape("A", &style, &font_ctx)
    .expect("shape palette font glyph");
  let palette_run = runs.first().expect("palette run");
  let palette_glyph = palette_run.glyphs.first().expect("palette glyph");
  let palette_raster = ColorFontRenderer::new()
    .render(
      &palette_run.font,
      palette_glyph.glyph_id,
      palette_run.font_size,
      palette_run.palette_index,
      Rgba::BLACK,
      palette_run.synthetic_oblique,
    )
    .expect("color glyph raster");

  let origin = Point::new(
    -(palette_glyph.x_offset + palette_raster.left as f32),
    -(palette_glyph.y_offset + palette_raster.top as f32),
  );
  let mut canvas = Canvas::new(
    palette_raster.image.width(),
    palette_raster.image.height(),
    Rgba::TRANSPARENT,
  )
  .unwrap();
  canvas.draw_shaped_run(palette_run, origin, Rgba::BLACK);
  let palette_pixmap = canvas.into_pixmap();
  let palette_image = pixmap_to_rgba_image(&palette_pixmap);
  let palette_png = encode_png(&palette_image).expect("encode palette png");

  let golden_path =
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/golden/font_palette_dark.png");
  let expected_png = std::fs::read(&golden_path).expect("read palette golden");
  let expected_image = decode_png(&expected_png).expect("decode palette golden");

  let diff = compare_images(&palette_image, &expected_image, &CompareConfig::lenient());
  assert!(
    diff.is_match(),
    "palette color glyph raster mismatch: {}",
    diff.summary()
  );

  let mut normal_style = style.clone();
  normal_style.font_palette = FontPalette::Normal;
  let normal_runs = pipeline
    .shape("A", &normal_style, &font_ctx)
    .expect("shape normal palette glyph");
  let normal_run = normal_runs.first().expect("normal palette run");
  let normal_glyph = normal_run.glyphs.first().expect("normal glyph");
  let normal_raster = ColorFontRenderer::new()
    .render(
      &normal_run.font,
      normal_glyph.glyph_id,
      normal_run.font_size,
      normal_run.palette_index,
      Rgba::BLACK,
      normal_run.synthetic_oblique,
    )
    .expect("normal palette raster");
  let normal_origin = Point::new(
    -(normal_glyph.x_offset + normal_raster.left as f32),
    -(normal_glyph.y_offset + normal_raster.top as f32),
  );
  let mut normal_canvas = Canvas::new(
    normal_raster.image.width(),
    normal_raster.image.height(),
    Rgba::TRANSPARENT,
  )
  .unwrap();
  normal_canvas.draw_shaped_run(normal_run, normal_origin, Rgba::BLACK);
  let normal_png =
    encode_png(&pixmap_to_rgba_image(&normal_canvas.into_pixmap())).expect("encode normal png");

  assert_ne!(
    normal_png, palette_png,
    "different palettes should render different canvas outputs"
  );
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
  canvas.draw_rect(
    Rect::from_xywh(100.0, 100.0, 20.0, 20.0),
    Rgba::rgb(255, 0, 0),
  );

  // Rect partially outside
  canvas.draw_rect(
    Rect::from_xywh(-10.0, -10.0, 30.0, 30.0),
    Rgba::rgb(0, 0, 255),
  );

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
  let radii = BorderRadii::new(
    BorderRadius::uniform(5.0),
    BorderRadius::uniform(5.0),
    BorderRadius::ZERO,
    BorderRadius::ZERO,
  );
  canvas.draw_rounded_rect(
    Rect::from_xywh(10.0, 10.0, 180.0, 40.0),
    radii,
    Rgba::rgb(51, 102, 204),
  );

  // Content area with border
  canvas.stroke_rect(
    Rect::from_xywh(20.0, 60.0, 160.0, 120.0),
    Rgba::rgb(200, 200, 200),
    1.0,
  );

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
  canvas.draw_rect(
    Rect::from_xywh(10.0, 10.0, 80.0, 80.0),
    Rgba::rgb(255, 0, 0),
  );

  // Layer 2: 50% opacity rectangle
  canvas.save();
  canvas.set_opacity(0.5);
  canvas.draw_rect(
    Rect::from_xywh(20.0, 20.0, 60.0, 60.0),
    Rgba::rgb(0, 255, 0),
  );
  canvas.restore();

  // Layer 3: Back to full opacity
  canvas.draw_rect(
    Rect::from_xywh(35.0, 35.0, 30.0, 30.0),
    Rgba::rgb(0, 0, 255),
  );

  let _ = canvas.into_pixmap();
}

#[test]
fn test_transformed_rendering() {
  let mut canvas = Canvas::new(100, 100, Rgba::WHITE).unwrap();

  // Draw with translation
  canvas.save();
  canvas.translate(50.0, 50.0);
  canvas.draw_rect(
    Rect::from_xywh(-10.0, -10.0, 20.0, 20.0),
    Rgba::rgb(255, 0, 0),
  );
  canvas.restore();

  // Draw with scale
  canvas.save();
  canvas.translate(25.0, 25.0);
  canvas.scale(0.5, 0.5);
  canvas.draw_rect(Rect::from_xywh(0.0, 0.0, 50.0, 50.0), Rgba::rgb(0, 0, 255));
  canvas.restore();

  let _ = canvas.into_pixmap();
}
