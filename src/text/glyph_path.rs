//! Shared glyph outline utilities.
//!
//! This module centralizes glyph outline building so color fonts and text
//! rasterization use the same path construction logic. Paths are emitted in
//! font design units (y-up). Callers can apply transforms (scale, skew, flip)
//! at render time.

use tiny_skia::{Path, PathBuilder, Transform};

/// Simple metrics captured while building an outline.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct GlyphOutlineMetrics {
  pub verb_count: usize,
  pub point_count: usize,
}

/// Converts ttf-parser glyph outlines to tiny-skia paths.
///
/// Implements the `ttf_parser::OutlineBuilder` trait to receive outline
/// drawing commands and build a tiny-skia `Path`.
pub struct GlyphOutlineBuilder {
  builder: PathBuilder,
  metrics: GlyphOutlineMetrics,
}

impl GlyphOutlineBuilder {
  /// Creates a new outline builder.
  ///
  /// Outlines are recorded in font design units with no positioning or
  /// scaling. The caller is responsible for applying any required transforms
  /// when rasterizing the path.
  pub fn new() -> Self {
    Self {
      builder: PathBuilder::new(),
      metrics: GlyphOutlineMetrics::default(),
    }
  }

  /// Consumes the builder and returns the completed path and metrics.
  pub fn finish(self) -> (Option<Path>, GlyphOutlineMetrics) {
    (self.builder.finish(), self.metrics)
  }
}

impl ttf_parser::OutlineBuilder for GlyphOutlineBuilder {
  fn move_to(&mut self, x: f32, y: f32) {
    self.metrics.verb_count += 1;
    self.metrics.point_count += 1;
    self.builder.move_to(x, y);
  }

  fn line_to(&mut self, x: f32, y: f32) {
    self.metrics.verb_count += 1;
    self.metrics.point_count += 1;
    self.builder.line_to(x, y);
  }

  fn quad_to(&mut self, x1: f32, y1: f32, x: f32, y: f32) {
    self.metrics.verb_count += 1;
    self.metrics.point_count += 2;
    self.builder.quad_to(x1, y1, x, y);
  }

  fn curve_to(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, x: f32, y: f32) {
    self.metrics.verb_count += 1;
    self.metrics.point_count += 3;
    self.builder.cubic_to(x1, y1, x2, y2, x, y);
  }

  fn close(&mut self) {
    self.metrics.verb_count += 1;
    self.builder.close();
  }
}

/// Builds a glyph path and metrics from the provided face.
pub fn build_glyph_path(
  face: &ttf_parser::Face<'_>,
  glyph_id: u32,
) -> Option<(Path, GlyphOutlineMetrics)> {
  let mut builder = GlyphOutlineBuilder::new();
  let glyph = ttf_parser::GlyphId(glyph_id as u16);
  face.outline_glyph(glyph, &mut builder)?;
  let (path, metrics) = builder.finish();
  path.map(|p| (p, metrics))
}

/// Transform used for mapping font design units to device pixels.
///
/// `scale` converts design units to pixels; `skew` applies synthetic oblique;
/// translation is applied after scaling. The resulting transform flips the Y
/// axis to match tiny-skia's Y-down coordinate system.
#[inline]
pub fn glyph_transform(scale: f32, skew: f32, x: f32, y: f32) -> Transform {
  Transform::from_row(scale, 0.0, skew * scale, -scale, x, y)
}

#[cfg(test)]
mod tests {
  use super::*;
  use ttf_parser::OutlineBuilder;

  #[test]
  fn outline_metrics_track_commands() {
    let mut builder = GlyphOutlineBuilder::new();
    OutlineBuilder::move_to(&mut builder, 0.0, 0.0);
    OutlineBuilder::line_to(&mut builder, 10.0, 0.0);
    OutlineBuilder::quad_to(&mut builder, 15.0, 5.0, 20.0, 0.0);
    OutlineBuilder::curve_to(&mut builder, 20.0, 5.0, 25.0, 5.0, 30.0, 0.0);
    OutlineBuilder::close(&mut builder);

    let (_path, metrics) = builder.finish();
    assert_eq!(metrics.verb_count, 5);
    assert_eq!(metrics.point_count, 7);
  }

  #[test]
  fn glyph_transform_matches_expected_matrix() {
    let transform = glyph_transform(2.0, 0.25, 10.0, 20.0);
    assert!((transform.sx - 2.0).abs() < 1e-6);
    assert!((transform.kx - 0.5).abs() < 1e-6);
    assert!((transform.sy + 2.0).abs() < 1e-6);
    assert_eq!(transform.tx, 10.0);
    assert_eq!(transform.ty, 20.0);
  }
}
