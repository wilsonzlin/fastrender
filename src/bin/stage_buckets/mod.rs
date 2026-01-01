//! Shared stage-bucket attribution helpers for CLI tools.
//!
//! These buckets are meant to reflect **wall-clock stage time** (i.e. stage timers that are
//! approximately additive and should roughly sum to a render's `total_ms`).
//!
//! Subsystem/CPU-sum accumulators (e.g. `*_cpu_ms`) are intentionally excluded because they can
//! exceed wall time (parallelism / overlap) and can also double-count work that is already included
//! in the stage wall timers.
#![allow(dead_code)]

use fastrender::api::{RenderStageTimings, RenderStats};
use serde::{Deserialize, Serialize};

/// Coarse wall-clock stage buckets (ms).
///
/// Used by:
/// - `pageset_progress` (`progress/pages/*.json` -> `stages_ms`)
/// - `perf_smoke` (`target/perf_smoke.json` -> `stage_ms`)
#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize, PartialEq)]
pub(crate) struct StageBuckets {
  #[serde(default)]
  pub(crate) fetch: f64,
  #[serde(default)]
  pub(crate) css: f64,
  #[serde(default)]
  pub(crate) cascade: f64,
  #[serde(default)]
  pub(crate) layout: f64,
  #[serde(default)]
  pub(crate) paint: f64,
}

impl StageBuckets {
  pub(crate) fn sum(&self) -> f64 {
    self.fetch + self.css + self.cascade + self.layout + self.paint
  }

  pub(crate) fn rescale_to_total(&mut self, total_ms: f64) {
    if !total_ms.is_finite() || total_ms <= 0.0 {
      return;
    }
    let sum = self.sum();
    if !sum.is_finite() || sum <= 0.0 {
      return;
    }
    let scale = total_ms / sum;
    if !scale.is_finite() || scale <= 0.0 {
      return;
    }
    self.fetch *= scale;
    self.css *= scale;
    self.cascade *= scale;
    self.layout *= scale;
    self.paint *= scale;
  }

  pub(crate) fn entries(&self) -> [(&'static str, f64); 5] {
    [
      ("fetch", self.fetch),
      ("css", self.css),
      ("cascade", self.cascade),
      ("layout", self.layout),
      ("paint", self.paint),
    ]
  }

  pub(crate) fn add_assign(&mut self, other: &Self) {
    self.fetch += other.fetch;
    self.css += other.css;
    self.cascade += other.cascade;
    self.layout += other.layout;
    self.paint += other.paint;
  }

  pub(crate) fn rounded(&self) -> Self {
    Self {
      fetch: round_ms(self.fetch),
      css: round_ms(self.css),
      cascade: round_ms(self.cascade),
      layout: round_ms(self.layout),
      paint: round_ms(self.paint),
    }
  }
}

pub(crate) fn wall_clock_stage_buckets_from_timings(
  timings: &RenderStageTimings,
) -> StageBuckets {
  // Bucket composition MUST remain consistent between:
  // - `pageset_progress` (`progress/pages/*.json` -> `stages_ms`)
  // - `perf_smoke` (`target/perf_smoke.json` -> `stage_ms`)
  //
  // These are stage wall-clock timers only.
  let fetch = timings.html_decode_ms.unwrap_or(0.0)
    + timings.dom_parse_ms.unwrap_or(0.0)
    + timings.dom_meta_viewport_ms.unwrap_or(0.0)
    + timings.dom_clone_ms.unwrap_or(0.0)
    + timings.dom_top_layer_ms.unwrap_or(0.0);
  // The CSS stage is split into two non-overlapping wall-clock timers:
  // - `css_inlining_ms`: collect/inline stylesheets (including fetch/import work)
  // - `css_parse_ms`: post-collection style prep (font face extraction, codepoint collection, etc)
  //
  // `pageset_progress` expects these to be summed.
  let css = timings.css_inlining_ms.unwrap_or(0.0) + timings.css_parse_ms.unwrap_or(0.0);
  let cascade = timings.cascade_ms.unwrap_or(0.0) + timings.box_tree_ms.unwrap_or(0.0);
  let layout = timings.layout_ms.unwrap_or(0.0);
  let paint = timings.paint_build_ms.unwrap_or(0.0)
    + timings.paint_optimize_ms.unwrap_or(0.0)
    + timings.paint_rasterize_ms.unwrap_or(0.0)
    + timings.encode_ms.unwrap_or(0.0);
  StageBuckets {
    fetch,
    css,
    cascade,
    layout,
    paint,
  }
}

pub(crate) fn wall_clock_stage_buckets_from_stats(stats: &RenderStats) -> StageBuckets {
  wall_clock_stage_buckets_from_timings(&stats.timings)
}

fn round_ms(value: f64) -> f64 {
  let rounded = (value * 1000.0).round() / 1000.0;
  if rounded == 0.0 { 0.0 } else { rounded }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn wall_clock_stage_buckets_exclude_cpu_sum_timers() {
    let timings = RenderStageTimings {
      html_decode_ms: Some(1.0),
      dom_parse_ms: Some(2.0),
      dom_meta_viewport_ms: Some(0.5),
      dom_clone_ms: Some(0.25),
      dom_top_layer_ms: Some(0.75),
      css_inlining_ms: Some(3.0),
      css_parse_ms: Some(4.0),
      cascade_ms: Some(5.0),
      box_tree_ms: Some(6.0),
      layout_ms: Some(7.0),
      text_fallback_cpu_ms: Some(1000.0),
      text_shape_cpu_ms: Some(2000.0),
      paint_build_ms: Some(9.0),
      paint_optimize_ms: Some(10.0),
      paint_rasterize_ms: Some(11.0),
      text_rasterize_cpu_ms: Some(3000.0),
      encode_ms: Some(13.0),
      ..RenderStageTimings::default()
    };

    let buckets = wall_clock_stage_buckets_from_timings(&timings);
    assert_eq!(buckets.fetch, 4.5);
    assert_eq!(buckets.css, 7.0);
    assert_eq!(buckets.cascade, 11.0);
    assert_eq!(buckets.layout, 7.0);
    assert_eq!(buckets.paint, 43.0);
  }
}
