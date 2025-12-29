use fastrender::api::{FastRender, FastRenderConfig, RenderOptions};
use fastrender::layout::engine::{
  enable_layout_parallel_debug_counters, layout_parallel_debug_counters,
  reset_layout_parallel_debug_counters,
};
use fastrender::LayoutParallelism;
use std::fmt::Write;

#[test]
fn parallel_layout_matches_serial_and_uses_threads() {
  let mut html = String::from(
    "<!DOCTYPE html><style>.item{padding:4px;margin:2px;border:1px solid #000;}</style><body>",
  );
  for idx in 0..96 {
    let _ = writeln!(
      html,
      "<div class=\"item\">block-{idx}</div><div class=\"item\">extra-{idx}</div>"
    );
  }
  html.push_str("</body>");

  let mut renderer =
    FastRender::with_config(FastRenderConfig::new().with_default_viewport(800, 2000))
      .expect("renderer");

  enable_layout_parallel_debug_counters(true);
  reset_layout_parallel_debug_counters();
  let serial = renderer
    .render_html_with_options(&html, RenderOptions::new())
    .expect("serial render");
  let serial_counters = layout_parallel_debug_counters();
  assert_eq!(serial_counters.work_items, 0);

  let parallelism = LayoutParallelism::enabled(2).with_max_threads(Some(2));
  reset_layout_parallel_debug_counters();
  let parallel = renderer
    .render_html_with_options(
      &html,
      RenderOptions::new().with_layout_parallelism(parallelism),
    )
    .expect("parallel render");
  let parallel_counters = layout_parallel_debug_counters();
  enable_layout_parallel_debug_counters(false);
  reset_layout_parallel_debug_counters();

  assert_eq!(serial.width(), parallel.width());
  assert_eq!(serial.height(), parallel.height());
  assert_eq!(serial.data(), parallel.data());
  assert!(
    parallel_counters.work_items > 0,
    "expected parallel layout work to be recorded"
  );
  assert!(
    parallel_counters.worker_threads > 1,
    "expected layout to use multiple rayon threads"
  );
}
