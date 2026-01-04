use fastrender::debug::runtime::RuntimeToggles;
use fastrender::paint::display_list_renderer::PaintParallelism;
use fastrender::{DiagnosticsLevel, FastRender, FontConfig, RenderOptions};
use std::collections::HashMap;

#[test]
fn paint_diagnostics_capture_blur_from_parallel_tiles() {
  // Force the display-list renderer and a dedicated paint rayon pool. The point of this test is to
  // ensure paint diagnostics are attributed correctly when raster work executes on worker threads.
  let toggles = RuntimeToggles::from_map(HashMap::from([
    (
      "FASTR_PAINT_BACKEND".to_string(),
      "display_list".to_string(),
    ),
    ("FASTR_PAINT_THREADS".to_string(), "2".to_string()),
    // Avoid display list parallelism (builder stage) so `parallel_tasks/threads` attribution is
    // dominated by the parallel tiling raster stage that this test is targeting.
    ("FASTR_DISPLAY_LIST_PARALLEL".to_string(), "0".to_string()),
  ]));

  let mut renderer = FastRender::builder()
    .font_sources(FontConfig::bundled_only())
    .build()
    .expect("renderer should construct");

  let options = RenderOptions::new()
    .with_viewport(512, 512)
    .with_diagnostics_level(DiagnosticsLevel::Basic)
    .with_runtime_toggles(toggles)
    .with_paint_parallelism(PaintParallelism {
      tile_size: 64,
      max_threads: Some(2),
      ..PaintParallelism::enabled()
    });

  // Ensure the blur work runs during rasterization inside the per-tile tasks.
  // Avoid backdrop-filter because it forces serial painting.
  let html = r#"<!doctype html>
    <style>
      html, body { margin: 0; padding: 0; background: white; }
      .target {
        width: 512px;
        height: 512px;
        background: rgba(0, 0, 0, 0.75);
        filter: blur(2px);
      }
    </style>
    <div class="target"></div>
  "#;

  let report = renderer
    .render_html_with_diagnostics(html, options)
    .expect("render should succeed");
  let stats = report
    .diagnostics
    .stats
    .as_ref()
    .expect("diagnostics stats should be present");

  let parallel_tasks = stats.paint.parallel_tasks.unwrap_or(0);
  assert!(
    parallel_tasks > 0,
    "expected parallel paint tasks to be recorded, got {parallel_tasks}"
  );

  let parallel_threads = stats.paint.parallel_threads.unwrap_or(0);
  assert!(
    parallel_threads >= 2,
    "expected parallel paint to use >=2 threads (dedicated pool), got {parallel_threads}"
  );

  let blur_calls = stats.paint.blur_calls.unwrap_or(0);
  assert!(
    blur_calls > 0,
    "expected blur diagnostics to be recorded from parallel tile workers, got {blur_calls}"
  );

  let blur_ms = stats.paint.blur_ms.unwrap_or(0.0);
  assert!(
    blur_ms > 0.0,
    "expected blur timing to be non-zero from parallel tile workers, got {blur_ms}"
  );
}
