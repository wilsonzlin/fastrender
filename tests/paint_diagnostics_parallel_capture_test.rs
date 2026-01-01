use fastrender::paint::display_list_renderer::PaintParallelism;
use fastrender::{DiagnosticsLevel, FastRender, FontConfig, RenderOptions};
use std::ffi::{OsStr, OsString};

struct EnvVarGuard {
  key: &'static str,
  previous: Option<OsString>,
}

impl EnvVarGuard {
  fn set(key: &'static str, value: impl AsRef<OsStr>) -> Self {
    let previous = std::env::var_os(key);
    std::env::set_var(key, value);
    Self { key, previous }
  }
}

impl Drop for EnvVarGuard {
  fn drop(&mut self) {
    match self.previous.take() {
      Some(value) => std::env::set_var(self.key, value),
      None => std::env::remove_var(self.key),
    }
  }
}

#[test]
fn paint_diagnostics_capture_blur_from_parallel_tiles() {
  // Force the display-list renderer and a dedicated paint rayon pool. The point of this test is to
  // ensure paint diagnostics are attributed correctly when raster work executes on worker threads.
  let _backend_guard = EnvVarGuard::set("FASTR_PAINT_BACKEND", "display_list");
  let _paint_threads_guard = EnvVarGuard::set("FASTR_PAINT_THREADS", "2");
  // Avoid display list parallelism (builder stage) so `parallel_tasks/threads` attribution is
  // dominated by the parallel tiling raster stage that this test is targeting.
  let _display_list_parallel_guard = EnvVarGuard::set("FASTR_DISPLAY_LIST_PARALLEL", "0");

  let mut renderer = FastRender::builder()
    .font_sources(FontConfig::bundled_only())
    .build()
    .expect("renderer should construct");

  let options = RenderOptions::new()
    .with_viewport(512, 512)
    .with_diagnostics_level(DiagnosticsLevel::Basic)
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

