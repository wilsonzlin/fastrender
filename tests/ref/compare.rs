//! Image comparison utilities for reference-style tests.
//!
//! This module wraps the shared `fastrender::image_compare` APIs with helpers
//! for converting to/from tiny-skia pixmaps used by the renderer.

use fastrender::image_compare::{
  compare_images as compare_rgba_images, decode_png, encode_png, CompareConfig, DiffStatistics,
  ImageDiff,
};
use image::RgbaImage;
use std::path::Path;
use tiny_skia::Pixmap;

pub use fastrender::image_compare::{CompareConfig, DiffStatistics, ImageDiff};

/// Compare two pixmaps using the shared image comparison module.
pub fn compare_images(actual: &Pixmap, expected: &Pixmap, config: &CompareConfig) -> ImageDiff {
  let actual_rgba = pixmap_to_rgba_image(actual);
  let expected_rgba = pixmap_to_rgba_image(expected);
  compare_rgba_images(&actual_rgba, &expected_rgba, config)
}

/// Load a PNG image from disk into a pixmap.
pub fn load_png(path: &Path) -> Result<Pixmap, String> {
  let rgba = decode_png(
    &std::fs::read(path).map_err(|e| format!("Failed to read {}: {e}", path.display()))?,
  )
  .map_err(|e| e.to_string())?;
  pixmap_from_rgba_image(rgba)
}

/// Load a PNG from bytes into a pixmap.
pub fn load_png_from_bytes(data: &[u8]) -> Result<Pixmap, String> {
  let rgba = decode_png(data).map_err(|e| e.to_string())?;
  pixmap_from_rgba_image(rgba)
}

/// Save a pixmap as a PNG file.
#[allow(dead_code)]
pub fn save_png(pixmap: &Pixmap, path: &Path) -> Result<(), String> {
  pixmap
    .save_png(path)
    .map_err(|e| format!("Failed to save PNG '{}': {}", path.display(), e))
}

/// Create a solid color pixmap (premultiplied BGRA).
pub fn create_solid_pixmap(width: u32, height: u32, r: u8, g: u8, b: u8, a: u8) -> Option<Pixmap> {
  let mut pixmap = Pixmap::new(width, height)?;

  let alpha = a as f32 / 255.0;
  let pm_r = (r as f32 * alpha) as u8;
  let pm_g = (g as f32 * alpha) as u8;
  let pm_b = (b as f32 * alpha) as u8;

  for chunk in pixmap.data_mut().chunks_exact_mut(4) {
    chunk[0] = pm_b;
    chunk[1] = pm_g;
    chunk[2] = pm_r;
    chunk[3] = a;
  }

  Some(pixmap)
}

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

fn pixmap_from_rgba_image(rgba: image::RgbaImage) -> Result<Pixmap, String> {
  let width = rgba.width();
  let height = rgba.height();

  let mut pixmap = Pixmap::new(width, height)
    .ok_or_else(|| format!("Failed to create pixmap {}x{}", width, height))?;

  let src_data = rgba.as_raw();
  let dst_data = pixmap.data_mut();

  for i in 0..(width * height) as usize {
    let src_idx = i * 4;
    let dst_idx = i * 4;

    let r = src_data[src_idx];
    let g = src_data[src_idx + 1];
    let b = src_data[src_idx + 2];
    let a = src_data[src_idx + 3];

    let alpha = a as f32 / 255.0;
    let pm_r = (r as f32 * alpha) as u8;
    let pm_g = (g as f32 * alpha) as u8;
    let pm_b = (b as f32 * alpha) as u8;

    dst_data[dst_idx] = pm_b;
    dst_data[dst_idx + 1] = pm_g;
    dst_data[dst_idx + 2] = pm_r;
    dst_data[dst_idx + 3] = a;
  }

  Ok(pixmap)
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn compare_identical_images() {
    let pixmap1 = create_solid_pixmap(4, 4, 255, 0, 0, 255).unwrap();
    let pixmap2 = create_solid_pixmap(4, 4, 255, 0, 0, 255).unwrap();

    let diff = compare_images(&pixmap1, &pixmap2, &CompareConfig::strict());
    assert!(diff.is_match());
    assert!(diff.statistics.perceptual_distance < 0.0001);
  }

  #[test]
  fn compare_different_images() {
    let pixmap1 = create_solid_pixmap(2, 2, 255, 0, 0, 255).unwrap();
    let pixmap2 = create_solid_pixmap(2, 2, 0, 255, 0, 255).unwrap();

    let diff = compare_images(&pixmap1, &pixmap2, &CompareConfig::strict());
    assert!(!diff.is_match());
    assert_eq!(diff.statistics.different_pixels, 4);
    assert!(diff.statistics.perceptual_distance > 0.1);
    assert!(diff.diff_image.is_some());
  }

  #[test]
  fn compare_with_tolerance_and_percent() {
    let mut pixmap = create_solid_pixmap(10, 10, 100, 100, 100, 255).unwrap();
    pixmap.data_mut()[0] = 0;

    let config = CompareConfig::strict()
      .with_channel_tolerance(5)
      .with_max_different_percent(1.1);
    let diff = compare_images(
      &pixmap,
      &create_solid_pixmap(10, 10, 100, 100, 100, 255).unwrap(),
      &config,
    );

    assert!(diff.is_match());
    assert_eq!(diff.statistics.different_pixels, 1);
  }

  #[test]
  fn config_presets_include_perceptual_limits() {
    assert!(CompareConfig::lenient().max_perceptual_distance.is_some());
    assert!(CompareConfig::fuzzy().max_perceptual_distance.is_some());
  }

  #[test]
  fn load_png_round_trip() {
    let pixmap = create_solid_pixmap(2, 2, 50, 60, 70, 255).unwrap();
    let buffer = encode_png(&pixmap_to_rgba_image(&pixmap)).expect("failed to encode test png");

    let loaded = load_png_from_bytes(&buffer).expect("failed to load png");
    let diff = compare_images(&pixmap, &loaded, &CompareConfig::strict());
    assert!(diff.is_match());
  }
}
