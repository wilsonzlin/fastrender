use fastrender::paint::display_list_renderer::PaintParallelism;
use fastrender::{DiagnosticsLevel, FastRender, RenderOptions};

const PNG_1X1_BASE64: &str =
  "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR4nGNoaGj4DwAFhAKAjM1mJgAAAABJRU5ErkJggg==";

#[test]
fn display_list_parallel_aggregates_paint_and_image_diagnostics_across_threads() {
  // Ensure paint work runs on a dedicated rayon pool so diagnostics collectors must aggregate
  // across worker threads (thread-local collectors would undercount).
  std::env::set_var("FASTR_PAINT_BACKEND", "display_list");
  std::env::set_var("FASTR_PAINT_THREADS", "4");

  let data_url = format!("data:image/png;base64,{}", PNG_1X1_BASE64);

  let mut html = String::from(
    r#"<!doctype html>
      <style>
        html, body { margin: 0; padding: 0; background: white; }
        .box {
          width: 64px;
          height: 64px;
          display: inline-block;
          margin: 8px;
          background-image: url(""#,
  );
  html.push_str(&data_url);
  html.push_str(
    r#"");
          background-size: 8px 8px;
          background-repeat: repeat;
          box-shadow: 0 0 12px rgba(0, 0, 0, 0.6);
        }
      </style>
      <div id="root">"#,
  );
  for _ in 0..64 {
    html.push_str(r#"<div class="box"></div>"#);
  }
  html.push_str("</div>");

  let mut renderer = FastRender::new().expect("renderer should construct");
  let options = RenderOptions::new()
    .with_viewport(512, 512)
    .with_diagnostics_level(DiagnosticsLevel::Basic)
    .with_paint_parallelism(PaintParallelism {
      tile_size: 64,
      log_timing: false,
      min_display_items: 1,
      min_tiles: 1,
      min_build_fragments: 1,
      build_chunk_size: 1,
      ..PaintParallelism::enabled()
    });

  let result = renderer
    .render_html_with_diagnostics(&html, options)
    .expect("render should succeed");
  let stats = result
    .diagnostics
    .stats
    .as_ref()
    .expect("diagnostics stats should be present");

  assert!(
    stats.paint.parallel_tasks.unwrap_or(0) > 0,
    "expected parallel tasks to be recorded"
  );
  assert!(
    stats.paint.parallel_threads.unwrap_or(0) > 1,
    "expected paint work to run on multiple threads"
  );

  assert!(
    stats.paint.blur_calls.unwrap_or(0) > 0,
    "expected blur diagnostics from worker threads to be recorded"
  );

  assert!(
    stats.resources.image_cache_misses.unwrap_or(0) > 0,
    "expected image cache misses to be recorded from worker threads"
  );
  assert!(
    stats.resources.image_cache_hits.unwrap_or(0) > 0,
    "expected image cache hits to be recorded from worker threads"
  );
}
