use fastrender::api::{DiagnosticsLevel, FastRender, RenderOptions};
use fastrender::LayoutParallelism;
use std::fmt::Write;

#[test]
fn render_stats_records_layout_parallel_counters() {
  let mut html = String::from(
    "<!DOCTYPE html><style>.item{padding:4px;margin:2px;border:1px solid #000;}</style><body>",
  );
  for idx in 0..256 {
    let _ = writeln!(html, "<div class=\"item\">row-{idx}</div>");
  }
  html.push_str("</body>");

  let mut renderer = FastRender::new().expect("create renderer");
  let options = RenderOptions::new()
    .with_viewport(800, 2400)
    .with_diagnostics_level(DiagnosticsLevel::Basic)
    .with_layout_parallelism(LayoutParallelism::enabled(2).with_max_threads(Some(2)));
  let result = renderer
    .render_html_with_diagnostics(&html, options)
    .expect("render should succeed");
  let stats = result
    .diagnostics
    .stats
    .expect("render should capture stats");

  let work_items = stats
    .layout
    .layout_parallel_work_items
    .expect("work items should be recorded");
  let worker_threads = stats
    .layout
    .layout_parallel_worker_threads
    .expect("worker threads should be recorded");
  assert!(
    work_items > 0,
    "expected layout parallelism work items to be >0, got {work_items}"
  );
  assert!(
    worker_threads >= 1,
    "expected layout parallelism worker_threads >= 1, got {worker_threads}"
  );
}
