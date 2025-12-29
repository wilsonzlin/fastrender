//! Integration tests for text rasterization
//!
//! Tests the text rasterization module which converts shaped text
//! (glyph IDs + positions) into rendered pixels using font outlines
//! and tiny-skia.

#[path = "../ref/mod.rs"]
mod r#ref;

use fastrender::text::color_fonts::{ColorFontRenderer, ColorGlyphRaster};
use fastrender::text::font_db::{FontStretch, FontStyle, FontWeight, LoadedFont};
use fastrender::text::font_instance::{glyph_transform, FontInstance};
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
use r#ref::compare::{compare_images, load_png, CompareConfig};
use rustybuzz::Variation;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tiny_skia::Paint;
use tiny_skia::Pixmap;
use tiny_skia::Transform;
use ttf_parser::GlyphId;
use ttf_parser::Tag;

// ============================================================================
// Test Helpers
// ============================================================================

fn get_test_font() -> Option<fastrender::text::font_db::LoadedFont> {
  let ctx = FontContext::new();
  ctx.get_sans_serif()
}

const VAR_FONT: &[u8] = include_bytes!("../fixtures/fonts/AmstelvarAlpha-VF.ttf");

fn variable_font() -> LoadedFont {
  LoadedFont {
    id: None,
    data: Arc::new(VAR_FONT.to_vec()),
    index: 0,
    family: "AmstelvarAlpha".to_string(),
    weight: FontWeight::NORMAL,
    style: FontStyle::Normal,
    stretch: FontStretch::Normal,
  }
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

fn row_leftmost(pixmap: &Pixmap, row: u32) -> Option<u32> {
  if row >= pixmap.height() {
    return None;
  }
  let width = pixmap.width() as usize;
  let start = row as usize * width * 4;
  let end = start + width * 4;
  for (idx, chunk) in pixmap.data()[start..end].chunks(4).enumerate() {
    if chunk[0] != 255 || chunk[1] != 255 || chunk[2] != 255 {
      return Some(idx as u32);
    }
  }
  None
}

fn fixtures_path() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("tests")
    .join("fixtures")
}

fn load_color_font(path: &str, family: &str) -> LoadedFont {
  let data = fs::read(path).expect("failed to read test font");
  LoadedFont {
    id: None,
    data: Arc::new(data),
    index: 0,
    family: family.to_string(),
    weight: FontWeight::NORMAL,
    style: FontStyle::Normal,
    stretch: FontStretch::Normal,
  }
}

fn single_glyph_run(
  font: &Arc<LoadedFont>,
  glyph_id: u32,
  font_size: f32,
  synthetic_oblique: f32,
) -> ShapedRun {
  let face = font.as_ttf_face().unwrap();
  let advance_units = face
    .glyph_hor_advance(GlyphId(glyph_id as u16))
    .unwrap_or(0) as f32;
  let units_per_em = face.units_per_em() as f32;
  let scale = font_size / units_per_em;
  let advance = advance_units * scale;

  ShapedRun {
    text: "A".to_string(),
    start: 0,
    end: 1,
    glyphs: vec![GlyphPosition {
      glyph_id,
      cluster: 0,
      x_offset: 0.0,
      y_offset: 0.0,
      x_advance: advance,
      y_advance: 0.0,
    }],
    direction: Direction::LeftToRight,
    level: 0,
    advance,
    font: Arc::clone(font),
    font_size,
    baseline_shift: 0.0,
    language: None,
    synthetic_bold: 0.0,
    synthetic_oblique,
    rotation: RunRotation::None,
    palette_index: 0,
    palette_overrides: Arc::new(Vec::new()),
    palette_override_hash: 0,
    variations: Vec::new(),
    scale: 1.0,
  }
}

fn single_glyph_run_with_variations(
  font: &Arc<LoadedFont>,
  glyph_id: u32,
  font_size: f32,
  synthetic_oblique: f32,
  variations: Vec<Variation>,
) -> ShapedRun {
  let mut run = single_glyph_run(font, glyph_id, font_size, synthetic_oblique);
  run.variations = variations;
  run
}

fn get_color_test_font() -> Option<LoadedFont> {
  let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fonts/ColorTestCOLR.ttf");
  let bytes = std::fs::read(path).ok()?;
  Some(LoadedFont {
    id: None,
    data: Arc::new(bytes),
    index: 0,
    family: "ColorTestCOLR".into(),
    weight: FontWeight::NORMAL,
    style: FontStyle::Normal,
    stretch: FontStretch::Normal,
  })
}

fn rotation_matrix(rotation: RunRotation, origin_x: f32, origin_y: f32) -> Option<Transform> {
  let angle = match rotation {
    RunRotation::Ccw90 => -90.0_f32.to_radians(),
    RunRotation::Cw90 => 90.0_f32.to_radians(),
    RunRotation::None => return None,
  };

  let (sin, cos) = angle.sin_cos();
  let tx = origin_x - origin_x * cos + origin_y * sin;
  let ty = origin_y - origin_x * sin - origin_y * cos;
  Some(Transform::from_row(cos, sin, -sin, cos, tx, ty))
}

fn concat_transforms(a: Transform, b: Transform) -> Transform {
  Transform::from_row(
    a.sx * b.sx + a.kx * b.ky,
    a.ky * b.sx + a.sy * b.ky,
    a.sx * b.kx + a.kx * b.sy,
    a.ky * b.kx + a.sy * b.sy,
    a.sx * b.tx + a.kx * b.ty + a.tx,
    a.ky * b.tx + a.sy * b.ty + a.ty,
  )
}

fn color_transform(
  glyph: &ColorGlyphRaster,
  glyph_x: f32,
  glyph_y: f32,
  skew: f32,
  rotation: Option<Transform>,
) -> Transform {
  let mut transform = Transform::from_row(
    1.0,
    0.0,
    -skew,
    1.0,
    glyph_x + glyph.left,
    glyph_y + glyph.top,
  );
  if let Some(rotation) = rotation {
    transform = concat_transforms(rotation, transform);
  }
  transform
}

fn transformed_bounds(transform: Transform, width: u32, height: u32) -> (i32, i32, i32, i32) {
  let corners = [
    (0.0, 0.0),
    (width as f32, 0.0),
    (0.0, height as f32),
    (width as f32, height as f32),
  ];
  let mut min_x = f32::MAX;
  let mut min_y = f32::MAX;
  let mut max_x = f32::MIN;
  let mut max_y = f32::MIN;
  for (x, y) in corners {
    let tx = transform.sx * x + transform.kx * y + transform.tx;
    let ty = transform.ky * x + transform.sy * y + transform.ty;
    min_x = min_x.min(tx);
    min_y = min_y.min(ty);
    max_x = max_x.max(tx);
    max_y = max_y.max(ty);
  }

  (
    min_x.floor() as i32,
    max_x.ceil() as i32 - 1,
    min_y.floor() as i32,
    max_y.ceil() as i32 - 1,
  )
}

fn assert_bounds_close(actual: (u32, u32, u32, u32), expected: (i32, i32, i32, i32)) {
  let (ax0, ax1, ay0, ay1) = (
    actual.0 as i32,
    actual.1 as i32,
    actual.2 as i32,
    actual.3 as i32,
  );
  assert!(
    (ax0 - expected.0).abs() <= 1,
    "min x differs: actual {}, expected {}",
    ax0,
    expected.0
  );
  assert!(
    (ax1 - expected.1).abs() <= 1,
    "max x differs: actual {}, expected {}",
    ax1,
    expected.1
  );
  assert!(
    (ay0 - expected.2).abs() <= 1,
    "min y differs: actual {}, expected {}",
    ay0,
    expected.2
  );
  assert!(
    (ay1 - expected.3).abs() <= 1,
    "max y differs: actual {}, expected {}",
    ay1,
    expected.3
  );
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

#[test]
fn glyph_cache_keys_include_variations() {
  let font = match get_test_font() {
    Some(f) => f,
    None => return,
  };
  let face = match font.as_ttf_face() {
    Ok(f) => f,
    Err(_) => return,
  };
  let glyph_id = match face.glyph_index('A') {
    Some(glyph) => glyph,
    None => return,
  };
  let units_per_em = face.units_per_em();
  if units_per_em == 0 {
    return;
  }
  let advance = face
    .glyph_hor_advance(glyph_id)
    .map(|v| v as f32 * (16.0 / units_per_em as f32))
    .unwrap_or(0.0);

  let glyphs = vec![GlyphPosition {
    glyph_id: glyph_id.0 as u32,
    cluster: 0,
    x_offset: 0.0,
    y_offset: 0.0,
    x_advance: advance,
    y_advance: 0.0,
  }];

  let mut rasterizer = TextRasterizer::new();
  rasterizer
    .positioned_glyph_paths(&glyphs, &font, 16.0, 0.0, 0.0, 0.0, None, &[])
    .expect("default variation glyph path");
  let stats = rasterizer.cache_stats();
  assert_eq!(stats.misses, 1);
  assert_eq!(stats.hits, 0);

  let bold_variation = Variation {
    tag: Tag::from_bytes(b"wght"),
    value: 900.0,
  };
  rasterizer
    .positioned_glyph_paths(&glyphs, &font, 16.0, 0.0, 0.0, 0.0, None, &[bold_variation])
    .expect("varied glyph path");
  let stats = rasterizer.cache_stats();
  assert_eq!(
    stats.misses, 2,
    "different variation should use a distinct cache entry"
  );
  assert_eq!(stats.hits, 0);

  rasterizer
    .positioned_glyph_paths(&glyphs, &font, 16.0, 0.0, 0.0, 0.0, None, &[bold_variation])
    .expect("cached varied glyph path");
  let stats = rasterizer.cache_stats();
  assert_eq!(stats.misses, 2);
  assert_eq!(stats.hits, 1);
}

#[test]
fn positioned_paths_use_variations_for_variable_fonts() {
  let font = variable_font();
  let face = ttf_parser::Face::parse(VAR_FONT, 0).expect("variable font parses");
  let glyph_id = face.glyph_index('A').expect("glyph should exist").0 as u32;

  let glyphs = vec![GlyphPosition {
    glyph_id,
    cluster: 0,
    x_offset: 0.0,
    y_offset: 0.0,
    x_advance: 0.0,
    y_advance: 0.0,
  }];

  let mut rasterizer = TextRasterizer::new();
  let default_paths = rasterizer
    .positioned_glyph_paths(&glyphs, &font, 48.0, 0.0, 0.0, 0.0, None, &[])
    .expect("default outlines");
  let varied_paths = rasterizer
    .positioned_glyph_paths(
      &glyphs,
      &font,
      48.0,
      0.0,
      0.0,
      0.0,
      None,
      &[Variation {
        tag: Tag::from_bytes(b"wght"),
        value: 900.0,
      }],
    )
    .expect("varied outlines");

  assert_eq!(default_paths.len(), 1);
  assert_eq!(varied_paths.len(), 1);

  let default_bounds = default_paths[0].bounds();
  let varied_bounds = varied_paths[0].bounds();
  assert_ne!(
    (
      default_bounds.left(),
      default_bounds.right(),
      default_bounds.top(),
      default_bounds.bottom()
    ),
    (
      varied_bounds.left(),
      varied_bounds.right(),
      varied_bounds.top(),
      varied_bounds.bottom()
    ),
    "variation coordinates should change extracted outlines"
  );
  assert!(
    rasterizer.cache_size() >= 2,
    "glyph cache should store separate entries per variation"
  );
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
fn synthetic_oblique_slants_bitmap_color_glyphs() {
  let font = Arc::new(load_color_font(
    "tests/fonts/ColorBitmapTest.ttf",
    "Test Bitmap Color",
  ));
  let glyph_id = font
    .as_ttf_face()
    .ok()
    .and_then(|face| face.glyph_index('A'))
    .map(|g| g.0 as u32)
    .expect("bitmap color font should contain 'A'");

  let mut rasterizer = TextRasterizer::new();
  let mut upright = create_test_pixmap(160, 160);
  let mut oblique = create_test_pixmap(160, 160);

  let upright_run = single_glyph_run(&font, glyph_id, 32.0, 0.0);
  let oblique_run = single_glyph_run(&font, glyph_id, 32.0, 0.25);

  rasterizer
    .render_shaped_run(&upright_run, 20.0, 120.0, Rgba::BLACK, &mut upright)
    .unwrap();
  rasterizer
    .render_shaped_run(&oblique_run, 20.0, 120.0, Rgba::BLACK, &mut oblique)
    .unwrap();

  assert_ne!(upright.data(), oblique.data());
  let oblique_bounds = painted_bounds(&oblique).expect("oblique bitmap glyph should render");
  let top_left = row_leftmost(&oblique, oblique_bounds.2).unwrap();
  let bottom_left = row_leftmost(&oblique, oblique_bounds.3).unwrap();
  assert!(
    bottom_left > top_left,
    "positive synthetic oblique should slant bitmap glyphs to the right"
  );
}

#[test]
fn synthetic_oblique_slants_svg_color_glyphs() {
  let font = Arc::new(load_color_font(
    "tests/fonts/ColorSvgTest.ttf",
    "Test SVG Color",
  ));
  let glyph_id = font
    .as_ttf_face()
    .ok()
    .and_then(|face| face.glyph_index('A'))
    .map(|g| g.0 as u32)
    .expect("svg color font should contain 'A'");

  let mut rasterizer = TextRasterizer::new();
  let mut upright = create_test_pixmap(160, 160);
  let mut oblique = create_test_pixmap(160, 160);

  let upright_run = single_glyph_run(&font, glyph_id, 32.0, 0.0);
  let oblique_run = single_glyph_run(&font, glyph_id, 32.0, 0.25);

  rasterizer
    .render_shaped_run(&upright_run, 20.0, 120.0, Rgba::BLACK, &mut upright)
    .unwrap();
  rasterizer
    .render_shaped_run(&oblique_run, 20.0, 120.0, Rgba::BLACK, &mut oblique)
    .unwrap();

  assert_ne!(upright.data(), oblique.data());
  let oblique_bounds = painted_bounds(&oblique).expect("oblique svg glyph should render");
  let top_left = row_leftmost(&oblique, oblique_bounds.2).unwrap();
  let bottom_left = row_leftmost(&oblique, oblique_bounds.3).unwrap();
  assert!(
    bottom_left > top_left,
    "positive synthetic oblique should slant SVG glyphs to the right"
  );
}

#[test]
fn color_glyph_rasters_follow_outline_transforms() {
  let font = match get_color_test_font() {
    Some(f) => Arc::new(f),
    None => return,
  };

  let face = match font.as_ttf_face() {
    Ok(face) => face,
    Err(_) => return,
  };

  let glyph_id = match face.glyph_index('A') {
    Some(id) => id.0 as u32,
    None => return,
  };

  let font_size = 32.0;
  let synthetic_oblique = 0.3;
  let x_offset = 1.5;
  let y_offset = -0.75;
  let units_per_em = face.units_per_em() as f32;
  let scale = font_size / units_per_em;
  let advance = face
    .glyph_hor_advance(GlyphId(glyph_id as u16))
    .map(|a| a as f32 * scale)
    .unwrap_or(font_size);
  let glyph_pos = GlyphPosition {
    glyph_id,
    cluster: 0,
    x_offset,
    y_offset,
    x_advance: advance,
    y_advance: 0.0,
  };
  let origin_x = 24.4;
  let origin_y = 48.8;
  let glyph_x = origin_x + glyph_pos.x_offset;
  let glyph_y = origin_y + glyph_pos.y_offset;
  let variations: Vec<Variation> = Vec::new();
  let instance = FontInstance::new(&font, &variations).expect("font instance");
  let text_color = Rgba::BLACK;
  let color_raster = ColorFontRenderer::new()
    .render(
      &font,
      &instance,
      glyph_id,
      font_size,
      0,
      &[],
      0,
      text_color,
      0.0,
      &variations,
      None,
    )
    .expect("color glyph raster");
  let expected = transformed_bounds(
    color_transform(
      &color_raster,
      glyph_x,
      glyph_y,
      synthetic_oblique,
      rotation_matrix(RunRotation::None, origin_x, origin_y),
    ),
    color_raster.image.width(),
    color_raster.image.height(),
  );

  let mut run = ShapedRun {
    text: "A".to_string(),
    start: 0,
    end: 1,
    glyphs: vec![glyph_pos],
    direction: Direction::LeftToRight,
    level: 0,
    advance,
    font: Arc::clone(&font),
    font_size,
    baseline_shift: 0.0,
    language: None,
    synthetic_bold: 0.0,
    synthetic_oblique,
    rotation: RunRotation::None,
    palette_index: 0,
    palette_overrides: Arc::new(Vec::new()),
    palette_override_hash: 0,
    variations: variations.clone(),
    scale: 1.0,
  };

  let mut rasterizer = TextRasterizer::new();
  let mut pixmap = create_test_pixmap(200, 200);
  rasterizer
    .render_shaped_run(&run, origin_x, origin_y, text_color, &mut pixmap)
    .unwrap();

  let actual = painted_bounds(&pixmap).expect("color glyph should be drawn");
  assert_bounds_close(actual, expected);

  let outline_path = instance
    .glyph_outline(glyph_id)
    .and_then(|outline| outline.path)
    .expect("color font should provide outlines for comparison");
  let mut outline_paint = Paint::default();
  outline_paint.set_color_rgba8(0, 0, 0, 255);
  outline_paint.anti_alias = true;
  let render_outline_bounds = |rotation: RunRotation, width: u32, height: u32| {
    let mut outline_pixmap = create_test_pixmap(width, height);
    let mut outline_transform = glyph_transform(scale, synthetic_oblique, glyph_x, glyph_y);
    if let Some(rotation) = rotation_matrix(rotation, origin_x, origin_y) {
      outline_transform = concat_transforms(rotation, outline_transform);
    }
    outline_pixmap.fill_path(
      &outline_path,
      &outline_paint,
      tiny_skia::FillRule::Winding,
      outline_transform,
      None,
    );
    painted_bounds(&outline_pixmap).expect("outline glyph should be drawn")
  };

  let outline_bounds = render_outline_bounds(RunRotation::None, 200, 200);
  assert_bounds_close(
    actual,
    (
      outline_bounds.0 as i32,
      outline_bounds.1 as i32,
      outline_bounds.2 as i32,
      outline_bounds.3 as i32,
    ),
  );

  let mut rotated = run.clone();
  rotated.rotation = RunRotation::Cw90;
  let rotated_expected = transformed_bounds(
    color_transform(
      &color_raster,
      glyph_x,
      glyph_y,
      synthetic_oblique,
      rotation_matrix(rotated.rotation, origin_x, origin_y),
    ),
    color_raster.image.width(),
    color_raster.image.height(),
  );

  let mut pixmap_rotated = create_test_pixmap(220, 220);
  rasterizer
    .render_shaped_run(
      &rotated,
      origin_x,
      origin_y,
      text_color,
      &mut pixmap_rotated,
    )
    .unwrap();
  let rotated_bounds = painted_bounds(&pixmap_rotated).expect("rotated color glyph should paint");
  assert_bounds_close(rotated_bounds, rotated_expected);

  let rotated_outline_bounds = render_outline_bounds(rotated.rotation, 220, 220);
  assert_bounds_close(
    rotated_bounds,
    (
      rotated_outline_bounds.0 as i32,
      rotated_outline_bounds.1 as i32,
      rotated_outline_bounds.2 as i32,
      rotated_outline_bounds.3 as i32,
    ),
  );

  let base_dims = (actual.1 - actual.0, actual.3 - actual.2);
  let rotated_dims = (
    rotated_bounds.1 - rotated_bounds.0,
    rotated_bounds.3 - rotated_bounds.2,
  );
  assert!(
    (rotated_dims.0 as i32 - base_dims.1 as i32).abs() <= 2,
    "rotation should swap glyph span horizontally: {:?} vs {:?}",
    rotated_dims,
    base_dims
  );
  assert!(
    (rotated_dims.1 as i32 - base_dims.0 as i32).abs() <= 2,
    "rotation should swap glyph span vertically: {:?} vs {:?}",
    rotated_dims,
    base_dims
  );
}

#[test]
fn colrv1_color_glyph_respects_variations_in_rasterizer() {
  let font = Arc::new(load_color_font(
    "tests/fixtures/fonts/colrv1-var-test.ttf",
    "COLRv1 Var Test",
  ));
  let Some(glyph_id) = font
    .as_ttf_face()
    .ok()
    .and_then(|face| face.glyph_index('A'))
    .map(|g| g.0 as u32)
  else {
    return;
  };

  let text_color = Rgba::from_rgba8(30, 40, 220, 255);
  let renderer = ColorFontRenderer::new();

  let base_variations: Vec<Variation> = Vec::new();
  let varied_variations = vec![Variation {
    tag: Tag::from_bytes(b"wght"),
    value: 1.0,
  }];
  let base_instance =
    FontInstance::new(&font, &base_variations).expect("expected base font instance");
  let varied_instance =
    FontInstance::new(&font, &varied_variations).expect("expected varied font instance");

  let base_raster = renderer
    .render(
      &font,
      &base_instance,
      glyph_id,
      64.0,
      0,
      &[],
      0,
      text_color,
      0.0,
      &base_variations,
      None,
    )
    .expect("expected default variation colr glyph");
  let varied_raster = renderer
    .render(
      &font,
      &varied_instance,
      glyph_id,
      64.0,
      0,
      &[],
      0,
      text_color,
      0.0,
      &varied_variations,
      None,
    )
    .expect("expected varied colr glyph");

  let golden_dir = fixtures_path().join("golden");
  let base_golden =
    load_png(&golden_dir.join("colrv1_var_default.png")).expect("missing default variation golden");
  let varied_golden =
    load_png(&golden_dir.join("colrv1_var_wght1.png")).expect("missing wght=1 golden");

  let mut rasterizer = TextRasterizer::new();
  let mut render_run =
    |variations: Vec<Variation>, left: f32, top: f32, width: u32, height: u32| {
      let run = single_glyph_run_with_variations(&font, glyph_id, 64.0, 0.0, variations);
      let mut pixmap = Pixmap::new(width, height).expect("failed to allocate test pixmap");
      rasterizer
        .render_shaped_run(&run, -left, -top, text_color, &mut pixmap)
        .expect("failed to render varied colr glyph");
      pixmap
    };

  let base_pixmap = render_run(
    base_variations.clone(),
    base_raster.left,
    base_raster.top,
    base_raster.image.width(),
    base_raster.image.height(),
  );
  let varied_pixmap = render_run(
    varied_variations.clone(),
    varied_raster.left,
    varied_raster.top,
    varied_raster.image.width(),
    varied_raster.image.height(),
  );

  let config = CompareConfig::strict();
  let base_diff = compare_images(&base_pixmap, &base_golden, &config);
  assert!(
    base_diff.is_match(),
    "default COLRv1 variation did not match golden: {:?}",
    base_diff.statistics
  );

  let varied_diff = compare_images(&varied_pixmap, &varied_golden, &config);
  assert!(
    varied_diff.is_match(),
    "wght=1 COLRv1 variation did not match golden: {:?}",
    varied_diff.statistics
  );

  let diff = compare_images(&base_pixmap, &varied_pixmap, &config);
  assert!(
    !diff.is_match(),
    "COLRv1 color glyph raster should differ between variations"
  );
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
    palette_overrides: Arc::new(Vec::new()),
    palette_override_hash: 0,
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
      palette_overrides: Arc::new(Vec::new()),
      palette_override_hash: 0,
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

  assert!(
    v_height > h_height,
    "vertical text should extend along the y axis"
  );
  assert!(
    v_width < h_width,
    "vertical text should occupy less horizontal span than horizontal text"
  );
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
