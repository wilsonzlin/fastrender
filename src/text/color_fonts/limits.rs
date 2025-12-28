//! Limits for color glyph rasterization to prevent excessive allocations.
use std::cmp::max;

/// Hard cap for the width or height (in pixels) of a rasterized color glyph.
pub const MAX_COLOR_GLYPH_DIMENSION: u32 = 4096;
/// Hard cap for the total pixel count of a rasterized color glyph.
pub const MAX_COLOR_GLYPH_PIXELS: u64 =
  MAX_COLOR_GLYPH_DIMENSION as u64 * MAX_COLOR_GLYPH_DIMENSION as u64;
/// When a target surface size is known, allow glyph rasters up to this multiple of it.
pub const MAX_COLOR_GLYPH_SURFACE_SCALE: u32 = 4;

/// Bounding limits for rasterizing a single color glyph.
#[derive(Debug, Clone, Copy)]
pub struct GlyphRasterLimits {
  pub max_dimension: u32,
  pub max_pixels: u64,
}

impl GlyphRasterLimits {
  /// Construct limits, optionally tightening them based on the target surface size.
  pub fn for_target_surface(surface: Option<(u32, u32)>) -> Self {
    let mut limits = Self::new(MAX_COLOR_GLYPH_DIMENSION, MAX_COLOR_GLYPH_PIXELS);
    if let Some((width, height)) = surface {
      let scaled_w = width.saturating_mul(MAX_COLOR_GLYPH_SURFACE_SCALE).max(1);
      let scaled_h = height.saturating_mul(MAX_COLOR_GLYPH_SURFACE_SCALE).max(1);
      let scaled_dim = max(scaled_w, scaled_h).max(1);
      limits.max_dimension = limits.max_dimension.min(scaled_dim);
      let surface_pixels = u64::from(scaled_w) * u64::from(scaled_h);
      limits.max_pixels = limits.max_pixels.min(surface_pixels);
    }
    limits
  }

  /// Construct limits with explicit caps.
  pub const fn new(max_dimension: u32, max_pixels: u64) -> Self {
    Self {
      max_dimension,
      max_pixels,
    }
  }

  /// Returns `Ok(())` when the provided dimensions fit within the limits.
  pub fn validate(&self, width: u32, height: u32) -> Result<(), GlyphLimitError> {
    if width == 0 || height == 0 {
      return Err(GlyphLimitError::Empty);
    }
    if width > self.max_dimension || height > self.max_dimension {
      return Err(GlyphLimitError::DimensionExceeded {
        width,
        height,
        max_dimension: self.max_dimension,
      });
    }
    let pixels = u64::from(width) * u64::from(height);
    if pixels > self.max_pixels {
      return Err(GlyphLimitError::PixelBudgetExceeded {
        width,
        height,
        pixels,
        max_pixels: self.max_pixels,
      });
    }
    Ok(())
  }
}

/// Reasons a candidate glyph raster was rejected.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GlyphLimitError {
  Empty,
  DimensionExceeded {
    width: u32,
    height: u32,
    max_dimension: u32,
  },
  PixelBudgetExceeded {
    width: u32,
    height: u32,
    pixels: u64,
    max_pixels: u64,
  },
}

pub(crate) fn round_dimension(value: f32) -> Option<u32> {
  if !value.is_finite() || value <= 0.0 {
    return None;
  }
  let clamped = value.max(1.0);
  if clamped > u32::MAX as f32 {
    return None;
  }
  Some(clamped.round() as u32)
}

pub(crate) fn log_glyph_limit(kind: &str, glyph_id: u32, error: &GlyphLimitError) {
  #[cfg(debug_assertions)]
  {
    let reason = match error {
      GlyphLimitError::Empty => "invalid dimensions".to_string(),
      GlyphLimitError::DimensionExceeded {
        width,
        height,
        max_dimension,
      } => format!(
        "dimensions {}x{} exceed limit {}",
        width, height, max_dimension
      ),
      GlyphLimitError::PixelBudgetExceeded {
        width,
        height,
        pixels,
        max_pixels,
      } => format!(
        "{}x{} pixels ({}) exceed budget {}",
        width, height, pixels, max_pixels
      ),
    };
    eprintln!(
      "[fastrender] Skipping {} color glyph {}: {}",
      kind, glyph_id, reason
    );
  }
}
