//! Shared stage-bucket attribution helpers for CLI tools.
//!
//! These buckets are meant to reflect **wall-clock stage time** (i.e. stage timers that are
//! approximately additive and should roughly sum to a render's `total_ms`).
//!
//! Subsystem/CPU-sum accumulators (e.g. `*_cpu_ms`) are intentionally excluded because they can
//! exceed wall time (parallelism / overlap) and can also double-count work that is already included
//! in the stage wall timers.

use fastrender::api::{RenderStageTimings, RenderStats};

/// Coarse wall-clock stage buckets (ms).
#[derive(Debug, Clone, Copy, Default, PartialEq)]
pub(crate) struct WallClockStageBucketsMs {
  pub(crate) fetch: f64,
  pub(crate) css: f64,
  pub(crate) cascade: f64,
  pub(crate) layout: f64,
  pub(crate) paint: f64,
}

pub(crate) fn wall_clock_stage_buckets_from_stats(stats: &RenderStats) -> WallClockStageBucketsMs {
  wall_clock_stage_buckets_from_timings(&stats.timings)
}

pub(crate) fn wall_clock_stage_buckets_from_timings(
  timings: &RenderStageTimings,
) -> WallClockStageBucketsMs {
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
  // `css_parse_ms` is the wall-clock timer for the overall CSS stage; `css_inlining_ms` is a
  // sub-stage timer and can overlap. Prefer the stage timer and only fall back to inlining when the
  // parse stage timing is absent.
  let css = timings
    .css_parse_ms
    .or(timings.css_inlining_ms)
    .unwrap_or(0.0);
  let cascade = timings.cascade_ms.unwrap_or(0.0) + timings.box_tree_ms.unwrap_or(0.0);
  let layout = timings.layout_ms.unwrap_or(0.0);
  let paint = timings.paint_build_ms.unwrap_or(0.0)
    + timings.paint_optimize_ms.unwrap_or(0.0)
    + timings.paint_rasterize_ms.unwrap_or(0.0)
    + timings.encode_ms.unwrap_or(0.0);
  WallClockStageBucketsMs {
    fetch,
    css,
    cascade,
    layout,
    paint,
  }
}
