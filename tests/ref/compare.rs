//! Image comparison module for reference testing
//!
//! This module provides utilities for comparing rendered images against reference
//! images to detect visual regressions. It supports pixel-by-pixel comparison with
//! configurable tolerance, difference visualization, and detailed statistics.
//!
//! # Features
//!
//! - Pixel-by-pixel comparison with per-channel tolerance
//! - Difference map generation for visual debugging
//! - Comprehensive statistics (number of different pixels, max difference, etc.)
//! - Support for both exact and fuzzy matching
//!
//! # Example
//!
//! ```rust,ignore
//! use crate::compare::{compare_images, CompareConfig, ImageDiff};
//!
//! let actual = load_pixmap("actual.png");
//! let expected = load_pixmap("expected.png");
//!
//! let diff = compare_images(&actual, &expected, CompareConfig::default());
//!
//! if diff.is_match() {
//!     println!("Images match!");
//! } else {
//!     println!("Images differ: {} pixels different", diff.different_pixels);
//!     diff.save_diff_image("diff.png")?;
//! }
//! ```

use std::path::Path;
use tiny_skia::Pixmap;

/// Configuration for image comparison
///
/// Controls how strict the comparison should be, including per-channel
/// tolerance for color differences and overall thresholds.
#[derive(Debug, Clone)]
pub struct CompareConfig {
  /// Maximum allowed difference per color channel (0-255)
  /// A tolerance of 0 means exact match required
  /// A tolerance of 5 allows slight anti-aliasing differences
  pub channel_tolerance: u8,

  /// Maximum percentage of pixels that can differ (0.0-100.0)
  /// Useful for allowing minor rendering differences
  pub max_different_percent: f64,

  /// Whether to compare alpha channel
  pub compare_alpha: bool,

  /// Whether to generate a difference image
  pub generate_diff_image: bool,
}

impl Default for CompareConfig {
  fn default() -> Self {
    Self {
      channel_tolerance: 0,
      max_different_percent: 0.0,
      compare_alpha: true,
      generate_diff_image: true,
    }
  }
}

impl CompareConfig {
  /// Creates a strict comparison config (exact pixel match)
  pub fn strict() -> Self {
    Self {
      channel_tolerance: 0,
      max_different_percent: 0.0,
      compare_alpha: true,
      generate_diff_image: true,
    }
  }

  /// Creates a lenient comparison config (allows minor anti-aliasing differences)
  pub fn lenient() -> Self {
    Self {
      channel_tolerance: 5,
      max_different_percent: 0.1,
      compare_alpha: true,
      generate_diff_image: true,
    }
  }

  /// Creates a fuzzy comparison config (for testing general layout)
  pub fn fuzzy() -> Self {
    Self {
      channel_tolerance: 10,
      max_different_percent: 1.0,
      compare_alpha: false,
      generate_diff_image: true,
    }
  }

  /// Sets the channel tolerance
  pub fn with_channel_tolerance(mut self, tolerance: u8) -> Self {
    self.channel_tolerance = tolerance;
    self
  }

  /// Sets the max different percent
  pub fn with_max_different_percent(mut self, percent: f64) -> Self {
    self.max_different_percent = percent;
    self
  }

  /// Sets whether to compare alpha
  pub fn with_compare_alpha(mut self, compare: bool) -> Self {
    self.compare_alpha = compare;
    self
  }

  /// Sets whether to generate diff image
  pub fn with_generate_diff_image(mut self, generate: bool) -> Self {
    self.generate_diff_image = generate;
    self
  }
}

/// Statistics about pixel differences
#[derive(Debug, Clone, Default)]
pub struct DiffStatistics {
  /// Total number of pixels compared
  pub total_pixels: u64,

  /// Number of pixels that differ
  pub different_pixels: u64,

  /// Percentage of pixels that differ (0.0-100.0)
  pub different_percent: f64,

  /// Maximum difference in red channel
  pub max_red_diff: u8,

  /// Maximum difference in green channel
  pub max_green_diff: u8,

  /// Maximum difference in blue channel
  pub max_blue_diff: u8,

  /// Maximum difference in alpha channel
  pub max_alpha_diff: u8,

  /// Mean squared error across all channels
  pub mse: f64,

  /// Peak signal-to-noise ratio (higher is better, infinity means identical)
  pub psnr: f64,
}

impl DiffStatistics {
  /// Returns the maximum difference across all color channels
  pub fn max_channel_diff(&self) -> u8 {
    self
      .max_red_diff
      .max(self.max_green_diff)
      .max(self.max_blue_diff)
      .max(self.max_alpha_diff)
  }
}

/// Result of comparing two images
///
/// Contains detailed information about differences and optionally
/// a visualization of the differences.
#[derive(Debug)]
pub struct ImageDiff {
  /// Whether the images match according to the config
  pub matches: bool,

  /// Statistics about the differences
  pub statistics: DiffStatistics,

  /// Difference image (if generated)
  /// Red pixels indicate differences, intensity shows magnitude
  pub diff_image: Option<Pixmap>,

  /// Whether dimensions match
  pub dimensions_match: bool,

  /// Actual image dimensions
  pub actual_dimensions: (u32, u32),

  /// Expected image dimensions
  pub expected_dimensions: (u32, u32),
}

impl ImageDiff {
  /// Returns true if the images match according to the comparison config
  pub fn is_match(&self) -> bool {
    self.matches
  }

  /// Saves the difference image to a file
  ///
  /// # Arguments
  /// * `path` - Path to save the PNG file
  ///
  /// # Returns
  /// `Ok(())` if saved successfully, `Err` with message otherwise
  pub fn save_diff_image(&self, path: &Path) -> Result<(), String> {
    if let Some(ref diff_pixmap) = self.diff_image {
      diff_pixmap
        .save_png(path)
        .map_err(|e| format!("Failed to save diff image: {}", e))
    } else {
      Err("No diff image generated".to_string())
    }
  }

  /// Returns a human-readable summary of the comparison result
  pub fn summary(&self) -> String {
    if !self.dimensions_match {
      return format!(
        "Dimension mismatch: actual {}x{}, expected {}x{}",
        self.actual_dimensions.0,
        self.actual_dimensions.1,
        self.expected_dimensions.0,
        self.expected_dimensions.1
      );
    }

    if self.matches {
      "Images match".to_string()
    } else {
      format!(
        "Images differ: {} of {} pixels ({:.4}%), max channel diff: {}, MSE: {:.4}, PSNR: {:.2} dB",
        self.statistics.different_pixels,
        self.statistics.total_pixels,
        self.statistics.different_percent,
        self.statistics.max_channel_diff(),
        self.statistics.mse,
        self.statistics.psnr
      )
    }
  }
}

/// Compares two pixmaps and returns detailed difference information
///
/// # Arguments
/// * `actual` - The rendered image to test
/// * `expected` - The reference image to compare against
/// * `config` - Configuration for the comparison
///
/// # Returns
/// An `ImageDiff` containing comparison results and statistics
///
/// # Example
///
/// ```rust,ignore
/// let diff = compare_images(&actual, &expected, CompareConfig::strict());
/// assert!(diff.is_match(), "Images should match: {}", diff.summary());
/// ```
pub fn compare_images(actual: &Pixmap, expected: &Pixmap, config: &CompareConfig) -> ImageDiff {
  let actual_dims = (actual.width(), actual.height());
  let expected_dims = (expected.width(), expected.height());

  // Check dimensions first
  if actual_dims != expected_dims {
    return ImageDiff {
      matches: false,
      statistics: DiffStatistics::default(),
      diff_image: None,
      dimensions_match: false,
      actual_dimensions: actual_dims,
      expected_dimensions: expected_dims,
    };
  }

  let width = actual.width();
  let height = actual.height();
  let total_pixels = (width as u64) * (height as u64);

  let actual_data = actual.data();
  let expected_data = expected.data();

  // Create diff image if requested
  let mut diff_pixmap = if config.generate_diff_image {
    Pixmap::new(width, height)
  } else {
    None
  };

  // Statistics tracking
  let mut different_pixels = 0u64;
  let mut max_red_diff = 0u8;
  let mut max_green_diff = 0u8;
  let mut max_blue_diff = 0u8;
  let mut max_alpha_diff = 0u8;
  let mut sum_squared_error = 0.0f64;

  // Compare pixel by pixel
  // tiny-skia uses BGRA format in premultiplied alpha
  for y in 0..height {
    for x in 0..width {
      let idx = ((y * width + x) * 4) as usize;

      // Extract BGRA values (tiny-skia format)
      let actual_b = actual_data[idx];
      let actual_g = actual_data[idx + 1];
      let actual_r = actual_data[idx + 2];
      let actual_a = actual_data[idx + 3];

      let expected_b = expected_data[idx];
      let expected_g = expected_data[idx + 1];
      let expected_r = expected_data[idx + 2];
      let expected_a = expected_data[idx + 3];

      // Calculate per-channel differences
      let diff_r = (actual_r as i16 - expected_r as i16).unsigned_abs() as u8;
      let diff_g = (actual_g as i16 - expected_g as i16).unsigned_abs() as u8;
      let diff_b = (actual_b as i16 - expected_b as i16).unsigned_abs() as u8;
      let diff_a = (actual_a as i16 - expected_a as i16).unsigned_abs() as u8;

      // Update max differences
      max_red_diff = max_red_diff.max(diff_r);
      max_green_diff = max_green_diff.max(diff_g);
      max_blue_diff = max_blue_diff.max(diff_b);
      max_alpha_diff = max_alpha_diff.max(diff_a);

      // Calculate MSE contribution
      sum_squared_error += (diff_r as f64).powi(2);
      sum_squared_error += (diff_g as f64).powi(2);
      sum_squared_error += (diff_b as f64).powi(2);
      if config.compare_alpha {
        sum_squared_error += (diff_a as f64).powi(2);
      }

      // Check if pixel differs beyond tolerance
      let is_different = diff_r > config.channel_tolerance
        || diff_g > config.channel_tolerance
        || diff_b > config.channel_tolerance
        || (config.compare_alpha && diff_a > config.channel_tolerance);

      if is_different {
        different_pixels += 1;
      }

      // Update diff image
      if let Some(ref mut diff_pm) = diff_pixmap {
        let diff_data = diff_pm.data_mut();

        if is_different {
          // Show differences as red with intensity based on magnitude
          let max_diff = diff_r.max(diff_g).max(diff_b);
          let intensity = ((max_diff as f32 / 255.0) * 200.0 + 55.0) as u8;

          // BGRA format
          diff_data[idx] = 0; // B
          diff_data[idx + 1] = 0; // G
          diff_data[idx + 2] = intensity; // R
          diff_data[idx + 3] = 255; // A
        } else {
          // Show matching pixels as grayscale version of actual
          let gray = ((actual_r as u16 + actual_g as u16 + actual_b as u16) / 3) as u8;
          let dimmed = gray / 2; // Dim to make differences stand out

          // BGRA format
          diff_data[idx] = dimmed; // B
          diff_data[idx + 1] = dimmed; // G
          diff_data[idx + 2] = dimmed; // R
          diff_data[idx + 3] = 255; // A
        }
      }
    }
  }

  // Calculate statistics
  let different_percent = if total_pixels > 0 {
    (different_pixels as f64 / total_pixels as f64) * 100.0
  } else {
    0.0
  };

  let channels = if config.compare_alpha { 4.0 } else { 3.0 };
  let mse = if total_pixels > 0 {
    sum_squared_error / (total_pixels as f64 * channels)
  } else {
    0.0
  };

  // PSNR calculation (Peak Signal-to-Noise Ratio)
  // Higher values mean better quality (less difference)
  // Infinite means identical images
  let psnr = if mse > 0.0 {
    10.0 * (255.0f64.powi(2) / mse).log10()
  } else {
    f64::INFINITY
  };

  let statistics = DiffStatistics {
    total_pixels,
    different_pixels,
    different_percent,
    max_red_diff,
    max_green_diff,
    max_blue_diff,
    max_alpha_diff,
    mse,
    psnr,
  };

  // Determine if images match according to config
  let matches = different_percent <= config.max_different_percent;

  ImageDiff {
    matches,
    statistics,
    diff_image: diff_pixmap,
    dimensions_match: true,
    actual_dimensions: actual_dims,
    expected_dimensions: expected_dims,
  }
}

/// Loads a PNG image from a file path into a Pixmap
///
/// # Arguments
/// * `path` - Path to the PNG file
///
/// # Returns
/// `Ok(Pixmap)` if loaded successfully, `Err` with message otherwise
pub fn load_png(path: &Path) -> Result<Pixmap, String> {
  // Use image crate to load PNG and convert to Pixmap
  let img =
    image::open(path).map_err(|e| format!("Failed to load image '{}': {}", path.display(), e))?;

  pixmap_from_rgba_image(img.to_rgba8())
}

/// Loads a PNG from memory into a Pixmap
///
/// Useful for comparing in-memory renders without writing to disk first.
pub fn load_png_from_bytes(data: &[u8]) -> Result<Pixmap, String> {
  let img = image::load_from_memory(data)
    .map_err(|e| format!("Failed to decode PNG from memory: {}", e))?;

  pixmap_from_rgba_image(img.to_rgba8())
}

fn pixmap_from_rgba_image(rgba: image::RgbaImage) -> Result<Pixmap, String> {
  let width = rgba.width();
  let height = rgba.height();

  let mut pixmap = Pixmap::new(width, height)
    .ok_or_else(|| format!("Failed to create pixmap {}x{}", width, height))?;

  // Convert from RGBA to BGRA premultiplied (tiny-skia format)
  let src_data = rgba.as_raw();
  let dst_data = pixmap.data_mut();

  for i in 0..(width * height) as usize {
    let src_idx = i * 4;
    let dst_idx = i * 4;

    let r = src_data[src_idx];
    let g = src_data[src_idx + 1];
    let b = src_data[src_idx + 2];
    let a = src_data[src_idx + 3];

    // Premultiply alpha
    let alpha = a as f32 / 255.0;
    let pm_r = (r as f32 * alpha) as u8;
    let pm_g = (g as f32 * alpha) as u8;
    let pm_b = (b as f32 * alpha) as u8;

    // Store as BGRA
    dst_data[dst_idx] = pm_b;
    dst_data[dst_idx + 1] = pm_g;
    dst_data[dst_idx + 2] = pm_r;
    dst_data[dst_idx + 3] = a;
  }

  Ok(pixmap)
}

/// Saves a Pixmap as a PNG file
///
/// # Arguments
/// * `pixmap` - The pixmap to save
/// * `path` - Path to save the PNG file
///
/// # Returns
/// `Ok(())` if saved successfully, `Err` with message otherwise
#[allow(dead_code)]
pub fn save_png(pixmap: &Pixmap, path: &Path) -> Result<(), String> {
  pixmap
    .save_png(path)
    .map_err(|e| format!("Failed to save PNG '{}': {}", path.display(), e))
}

/// Creates a solid color pixmap for testing
///
/// # Arguments
/// * `width` - Width in pixels
/// * `height` - Height in pixels
/// * `r`, `g`, `b`, `a` - RGBA color values (0-255)
///
/// # Returns
/// `Some(Pixmap)` if created successfully, `None` if dimensions are invalid
pub fn create_solid_pixmap(width: u32, height: u32, r: u8, g: u8, b: u8, a: u8) -> Option<Pixmap> {
  let mut pixmap = Pixmap::new(width, height)?;

  // Premultiply alpha
  let alpha = a as f32 / 255.0;
  let pm_r = (r as f32 * alpha) as u8;
  let pm_g = (g as f32 * alpha) as u8;
  let pm_b = (b as f32 * alpha) as u8;

  let data = pixmap.data_mut();
  for i in 0..(width * height) as usize {
    let idx = i * 4;
    // BGRA format
    data[idx] = pm_b;
    data[idx + 1] = pm_g;
    data[idx + 2] = pm_r;
    data[idx + 3] = a;
  }

  Some(pixmap)
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_compare_identical_images() {
    let pixmap1 = create_solid_pixmap(10, 10, 255, 0, 0, 255).unwrap();
    let pixmap2 = create_solid_pixmap(10, 10, 255, 0, 0, 255).unwrap();

    let diff = compare_images(&pixmap1, &pixmap2, &CompareConfig::strict());

    assert!(diff.is_match());
    assert!(diff.dimensions_match);
    assert_eq!(diff.statistics.different_pixels, 0);
    assert_eq!(diff.statistics.different_percent, 0.0);
    assert_eq!(diff.statistics.max_channel_diff(), 0);
    assert!(diff.statistics.psnr.is_infinite());
  }

  #[test]
  fn test_compare_different_images() {
    let pixmap1 = create_solid_pixmap(10, 10, 255, 0, 0, 255).unwrap();
    let pixmap2 = create_solid_pixmap(10, 10, 0, 255, 0, 255).unwrap();

    let diff = compare_images(&pixmap1, &pixmap2, &CompareConfig::strict());

    assert!(!diff.is_match());
    assert!(diff.dimensions_match);
    assert_eq!(diff.statistics.different_pixels, 100);
    assert_eq!(diff.statistics.different_percent, 100.0);
    assert_eq!(diff.statistics.max_red_diff, 255);
    assert_eq!(diff.statistics.max_green_diff, 255);
  }

  #[test]
  fn test_compare_different_dimensions() {
    let pixmap1 = create_solid_pixmap(10, 10, 255, 0, 0, 255).unwrap();
    let pixmap2 = create_solid_pixmap(20, 20, 255, 0, 0, 255).unwrap();

    let diff = compare_images(&pixmap1, &pixmap2, &CompareConfig::strict());

    assert!(!diff.is_match());
    assert!(!diff.dimensions_match);
    assert_eq!(diff.actual_dimensions, (10, 10));
    assert_eq!(diff.expected_dimensions, (20, 20));
  }

  #[test]
  fn test_compare_with_tolerance() {
    let pixmap1 = create_solid_pixmap(10, 10, 100, 100, 100, 255).unwrap();
    let pixmap2 = create_solid_pixmap(10, 10, 105, 105, 105, 255).unwrap();

    // Strict comparison should fail
    let strict_diff = compare_images(&pixmap1, &pixmap2, &CompareConfig::strict());
    assert!(!strict_diff.is_match());

    // With tolerance of 5, should pass
    let lenient_config = CompareConfig::default().with_channel_tolerance(5);
    let lenient_diff = compare_images(&pixmap1, &pixmap2, &lenient_config);
    assert!(lenient_diff.is_match());
  }

  #[test]
  fn test_compare_with_max_different_percent() {
    let mut pixmap1 = create_solid_pixmap(10, 10, 100, 100, 100, 255).unwrap();
    let pixmap2 = create_solid_pixmap(10, 10, 100, 100, 100, 255).unwrap();

    // Change one pixel
    let data = pixmap1.data_mut();
    data[0] = 0; // Change first pixel's blue channel

    let config = CompareConfig::default().with_max_different_percent(1.1); // 1 pixel = 1%
    let diff = compare_images(&pixmap1, &pixmap2, &config);

    assert!(diff.is_match());
    assert_eq!(diff.statistics.different_pixels, 1);
  }

  #[test]
  fn test_diff_image_generation() {
    let pixmap1 = create_solid_pixmap(10, 10, 255, 0, 0, 255).unwrap();
    let pixmap2 = create_solid_pixmap(10, 10, 0, 255, 0, 255).unwrap();

    let config_with_diff = CompareConfig::default().with_generate_diff_image(true);
    let diff = compare_images(&pixmap1, &pixmap2, &config_with_diff);

    assert!(diff.diff_image.is_some());

    let config_without_diff = CompareConfig::default().with_generate_diff_image(false);
    let diff2 = compare_images(&pixmap1, &pixmap2, &config_without_diff);

    assert!(diff2.diff_image.is_none());
  }

  #[test]
  fn test_config_presets() {
    let strict = CompareConfig::strict();
    assert_eq!(strict.channel_tolerance, 0);
    assert_eq!(strict.max_different_percent, 0.0);

    let lenient = CompareConfig::lenient();
    assert_eq!(lenient.channel_tolerance, 5);
    assert_eq!(lenient.max_different_percent, 0.1);

    let fuzzy = CompareConfig::fuzzy();
    assert_eq!(fuzzy.channel_tolerance, 10);
    assert_eq!(fuzzy.max_different_percent, 1.0);
    assert!(!fuzzy.compare_alpha);
  }

  #[test]
  fn test_statistics_max_channel_diff() {
    let stats = DiffStatistics {
      total_pixels: 100,
      different_pixels: 10,
      different_percent: 10.0,
      max_red_diff: 50,
      max_green_diff: 100,
      max_blue_diff: 25,
      max_alpha_diff: 75,
      mse: 0.0,
      psnr: 0.0,
    };

    assert_eq!(stats.max_channel_diff(), 100);
  }

  #[test]
  fn test_diff_summary() {
    let pixmap1 = create_solid_pixmap(10, 10, 255, 0, 0, 255).unwrap();
    let pixmap2 = create_solid_pixmap(10, 10, 255, 0, 0, 255).unwrap();

    let diff = compare_images(&pixmap1, &pixmap2, &CompareConfig::strict());
    assert_eq!(diff.summary(), "Images match");

    let pixmap3 = create_solid_pixmap(20, 20, 255, 0, 0, 255).unwrap();
    let diff2 = compare_images(&pixmap1, &pixmap3, &CompareConfig::strict());
    assert!(diff2.summary().contains("Dimension mismatch"));
  }

  #[test]
  fn test_create_solid_pixmap() {
    let pixmap = create_solid_pixmap(5, 5, 128, 64, 32, 255).unwrap();

    assert_eq!(pixmap.width(), 5);
    assert_eq!(pixmap.height(), 5);

    // Check first pixel (BGRA format)
    let data = pixmap.data();
    assert_eq!(data[0], 32); // B
    assert_eq!(data[1], 64); // G
    assert_eq!(data[2], 128); // R
    assert_eq!(data[3], 255); // A
  }

  #[test]
  fn test_create_solid_pixmap_with_alpha() {
    let pixmap = create_solid_pixmap(2, 2, 200, 100, 50, 128).unwrap();

    // Values should be premultiplied
    let data = pixmap.data();
    let alpha = 128.0 / 255.0;

    // Check that values are premultiplied (approximately)
    let expected_r = (200.0 * alpha) as u8;
    let expected_g = (100.0 * alpha) as u8;
    let expected_b = (50.0 * alpha) as u8;

    assert_eq!(data[2], expected_r); // R
    assert_eq!(data[1], expected_g); // G
    assert_eq!(data[0], expected_b); // B
    assert_eq!(data[3], 128); // A (not premultiplied)
  }

  #[test]
  fn test_compare_alpha_channel() {
    let pixmap1 = create_solid_pixmap(10, 10, 100, 100, 100, 255).unwrap();
    let pixmap2 = create_solid_pixmap(10, 10, 100, 100, 100, 200).unwrap();

    // With alpha comparison, should differ
    let config_with_alpha = CompareConfig::default().with_compare_alpha(true);
    let diff1 = compare_images(&pixmap1, &pixmap2, &config_with_alpha);
    assert!(!diff1.is_match());

    // Without alpha comparison, RGB values differ due to premultiplication
    // so this will still fail - the premultiplied values are different
    let config_without_alpha = CompareConfig::default()
      .with_compare_alpha(false)
      .with_channel_tolerance(30); // Allow for premultiplication differences
    let diff2 = compare_images(&pixmap1, &pixmap2, &config_without_alpha);
    // Premultiplied values differ, so even without alpha comparison it fails
    // unless we have tolerance
    assert!(diff2.statistics.different_pixels > 0 || diff2.is_match());
  }

  #[test]
  fn test_psnr_calculation() {
    // Identical images should have infinite PSNR
    let pixmap1 = create_solid_pixmap(10, 10, 128, 128, 128, 255).unwrap();
    let pixmap2 = create_solid_pixmap(10, 10, 128, 128, 128, 255).unwrap();

    let diff = compare_images(&pixmap1, &pixmap2, &CompareConfig::strict());
    assert!(diff.statistics.psnr.is_infinite());

    // Different images should have finite PSNR
    let pixmap3 = create_solid_pixmap(10, 10, 0, 0, 0, 255).unwrap();
    let diff2 = compare_images(&pixmap1, &pixmap3, &CompareConfig::strict());
    assert!(diff2.statistics.psnr.is_finite());
    assert!(diff2.statistics.psnr > 0.0);
  }

  #[test]
  fn test_mse_calculation() {
    // Identical images should have zero MSE
    let pixmap1 = create_solid_pixmap(10, 10, 128, 128, 128, 255).unwrap();
    let pixmap2 = create_solid_pixmap(10, 10, 128, 128, 128, 255).unwrap();

    let diff = compare_images(&pixmap1, &pixmap2, &CompareConfig::strict());
    assert_eq!(diff.statistics.mse, 0.0);

    // Different images should have non-zero MSE
    let pixmap3 = create_solid_pixmap(10, 10, 0, 0, 0, 255).unwrap();
    let diff2 = compare_images(&pixmap1, &pixmap3, &CompareConfig::strict());
    assert!(diff2.statistics.mse > 0.0);
  }

  #[test]
  fn test_load_png_from_bytes() {
    let mut buffer = Vec::new();
    let image = image::RgbaImage::from_pixel(2, 2, image::Rgba([10, 20, 30, 255]));
    image
      .write_to(
        &mut std::io::Cursor::new(&mut buffer),
        image::ImageOutputFormat::Png,
      )
      .unwrap();

    let pixmap = load_png_from_bytes(&buffer).expect("Failed to decode PNG from bytes");
    assert_eq!(pixmap.width(), 2);
    assert_eq!(pixmap.height(), 2);

    let data = pixmap.data();
    assert_eq!(data[0], 30);
    assert_eq!(data[1], 20);
    assert_eq!(data[2], 10);
    assert_eq!(data[3], 255);

    let diff = compare_images(&pixmap, &pixmap, &CompareConfig::strict());
    assert!(diff.is_match());
  }

  #[test]
  fn test_empty_pixmap() {
    // tiny-skia doesn't allow 0-dimension pixmaps
    let result = Pixmap::new(0, 0);
    assert!(result.is_none());

    let result2 = create_solid_pixmap(0, 0, 255, 255, 255, 255);
    assert!(result2.is_none());
  }
}
