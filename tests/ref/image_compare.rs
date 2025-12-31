#![allow(dead_code)]
//! Shared helpers for pixel comparison-based tests.
//!
//! This module keeps the image comparison configuration and artifact writing
//! logic in one place so visual regression tests can reuse the same behavior.

use crate::r#ref::compare::{compare_images, load_png_from_bytes, CompareConfig, ImageDiff};
use std::fs;
use std::path::{Path, PathBuf};

/// Environment variable names controlling comparison strictness.
#[derive(Clone, Copy)]
pub struct CompareEnvVars<'a> {
  /// When set, use the fuzzy preset (tolerance 10, up to 1% different).
  pub fuzzy: &'a str,
  /// Optional per-channel tolerance override.
  pub tolerance: &'a str,
  /// Optional percentage override for the number of differing pixels.
  pub max_diff_percent: &'a str,
  /// When set, ignore alpha differences even without fuzzy mode.
  pub ignore_alpha: &'a str,
  /// Optional perceptual distance override.
  pub max_perceptual_distance: &'a str,
}

impl CompareEnvVars<'_> {
  /// Standard fixture env vars.
  pub const fn fixtures() -> Self {
    Self {
      fuzzy: "FIXTURE_FUZZY",
      tolerance: "FIXTURE_TOLERANCE",
      max_diff_percent: "FIXTURE_MAX_DIFFERENT_PERCENT",
      ignore_alpha: "FIXTURE_IGNORE_ALPHA",
      max_perceptual_distance: "FIXTURE_MAX_PERCEPTUAL_DISTANCE",
    }
  }

  /// Env vars for the offline page regression suite.
  pub const fn pages() -> Self {
    Self {
      fuzzy: "PAGES_FUZZY",
      tolerance: "PAGES_TOLERANCE",
      max_diff_percent: "PAGES_MAX_DIFFERENT_PERCENT",
      ignore_alpha: "PAGES_IGNORE_ALPHA",
      max_perceptual_distance: "PAGES_MAX_PERCEPTUAL_DISTANCE",
    }
  }
}

/// Build a comparison config honoring common fuzz/tolerance env vars.
pub fn compare_config_from_env(env: CompareEnvVars<'_>) -> Result<CompareConfig, String> {
  let mut config = if std::env::var(env.fuzzy).is_ok() {
    CompareConfig::fuzzy()
  } else {
    CompareConfig::strict()
  };

  if let Ok(tolerance) = std::env::var(env.tolerance) {
    let parsed = tolerance
      .parse::<u8>()
      .map_err(|e| format!("Invalid {} '{}': {}", env.tolerance, tolerance, e))?;
    config = config.with_channel_tolerance(parsed);
  }

  if let Ok(percent) = std::env::var(env.max_diff_percent) {
    let parsed = percent
      .parse::<f64>()
      .map_err(|e| format!("Invalid {} '{}': {}", env.max_diff_percent, percent, e))?;
    config = config.with_max_different_percent(parsed);
  }

  if std::env::var(env.ignore_alpha).is_ok() {
    config = config.with_compare_alpha(false);
  }

  if let Ok(distance) = std::env::var(env.max_perceptual_distance) {
    let parsed = distance.parse::<f64>().map_err(|e| {
      format!(
        "Invalid {} '{}': {}",
        env.max_perceptual_distance, distance, e
      )
    })?;
    config = config.with_max_perceptual_distance(Some(parsed));
  }

  // Always generate diff images to aid debugging.
  config.generate_diff_image = true;

  Ok(config)
}

/// Artifact paths saved when a comparison fails.
pub struct ArtifactPaths {
  pub output_dir: PathBuf,
  pub actual: PathBuf,
  pub expected: PathBuf,
  pub diff: Option<PathBuf>,
}

/// Save actual/expected/diff PNGs for debugging a mismatch.
pub fn save_artifacts(
  name: &str,
  rendered_png: &[u8],
  golden_png: &[u8],
  diff: &ImageDiff,
  output_dir: &Path,
) -> Result<ArtifactPaths, String> {
  fs::create_dir_all(output_dir).map_err(|e| {
    format!(
      "Failed to create diff output directory {}: {}",
      output_dir.display(),
      e
    )
  })?;

  let actual_path = output_dir.join(format!("{}_actual.png", name));
  fs::write(&actual_path, rendered_png).map_err(|e| {
    format!(
      "Failed to write actual image to {}: {}",
      actual_path.display(),
      e
    )
  })?;

  let expected_path = output_dir.join(format!("{}_expected.png", name));
  fs::write(&expected_path, golden_png).map_err(|e| {
    format!(
      "Failed to write expected image to {}: {}",
      expected_path.display(),
      e
    )
  })?;

  let diff_path = output_dir.join(format!("{}_diff.png", name));
  let saved_diff_path = if diff.diff_image.is_some() {
    diff.save_diff_image(&diff_path).map_err(|e| {
      format!(
        "Failed to write diff image to {}: {}",
        diff_path.display(),
        e
      )
    })?;
    Some(diff_path)
  } else {
    None
  };

  Ok(ArtifactPaths {
    output_dir: output_dir.to_path_buf(),
    actual: actual_path,
    expected: expected_path,
    diff: saved_diff_path,
  })
}

/// Decode two PNGs, compare them, and write artifacts on mismatch.
pub fn compare_pngs(
  name: &str,
  rendered_png: &[u8],
  golden_png: &[u8],
  config: &CompareConfig,
  output_dir: &Path,
) -> Result<(), String> {
  let actual = load_png_from_bytes(rendered_png)
    .map_err(|e| format!("Failed to decode rendered PNG for {}: {}", name, e))?;
  let expected = load_png_from_bytes(golden_png)
    .map_err(|e| format!("Failed to decode golden PNG for {}: {}", name, e))?;

  let image_diff = compare_images(&actual, &expected, config);

  if image_diff.is_match() {
    return Ok(());
  }

  let artifact_result = save_artifacts(name, rendered_png, golden_png, &image_diff, output_dir);

  let mut message = format!("Image mismatch for '{}': {}", name, image_diff.summary());

  match artifact_result {
    Ok(paths) => {
      message.push_str(&format!(
        "\nSaved artifacts to {} (actual: {}, expected: {})",
        paths.output_dir.display(),
        paths.actual.display(),
        paths.expected.display()
      ));

      if let Some(diff_path) = paths.diff {
        message.push_str(&format!("\nDiff image: {}", diff_path.display()));
      } else if !image_diff.dimensions_match {
        message.push_str("\nDiff image not generated due to dimension mismatch");
      }
    }
    Err(e) => {
      message.push_str(&format!("\nFailed to save diff artifacts: {}", e));
    }
  }

  Err(message)
}
