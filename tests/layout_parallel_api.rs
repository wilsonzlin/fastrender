use fastrender::api::{FastRender, FastRenderConfig, RenderOptions};
use fastrender::layout::engine::{
  enable_layout_parallel_debug_counters, layout_parallel_debug_counters,
  reset_layout_parallel_debug_counters, DEFAULT_LAYOUT_MIN_FANOUT,
};
use fastrender::snapshot_fragment_tree;
use fastrender::{LayoutParallelism, RenderArtifactRequest};
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

  let config = FastRenderConfig::new().with_default_viewport(800, 2000);
  let mut renderer = FastRender::with_config(config.clone()).expect("renderer");

  enable_layout_parallel_debug_counters(true);
  reset_layout_parallel_debug_counters();
  let serial = renderer
    .render_html_with_stylesheets_report(
      &html,
      "https://example.com/",
      RenderOptions::new(),
      RenderArtifactRequest::full(),
    )
    .expect("serial render");
  let serial_counters = layout_parallel_debug_counters();
  assert_eq!(serial_counters.work_items, 0);

  let parallelism = LayoutParallelism::enabled(2).with_max_threads(Some(2));
  reset_layout_parallel_debug_counters();
  let mut parallel_renderer = FastRender::with_config(config).expect("renderer");
  let parallel = parallel_renderer
    .render_html_with_stylesheets_report(
      &html,
      "https://example.com/",
      RenderOptions::new().with_layout_parallelism(parallelism),
      RenderArtifactRequest::full(),
    )
    .expect("parallel render");
  let parallel_counters = layout_parallel_debug_counters();
  enable_layout_parallel_debug_counters(false);
  reset_layout_parallel_debug_counters();

  let serial_tree = snapshot_fragment_tree(
    &serial
      .artifacts
      .fragment_tree
      .as_ref()
      .expect("serial fragment tree"),
  );
  let parallel_tree = snapshot_fragment_tree(
    &parallel
      .artifacts
      .fragment_tree
      .as_ref()
      .expect("parallel fragment tree"),
  );
  assert_eq!(serial_tree, parallel_tree);

  assert_eq!(serial.pixmap.width(), parallel.pixmap.width());
  assert_eq!(serial.pixmap.height(), parallel.pixmap.height());
  let diff = serial
    .pixmap
    .data()
    .iter()
    .zip(parallel.pixmap.data())
    .filter(|(a, b)| a != b)
    .count();
  assert_eq!(diff, 0, "pixmap diff count {diff}");
  assert!(
    parallel_counters.work_items > 0,
    "expected parallel layout work to be recorded"
  );
  assert!(
    parallel_counters.worker_threads > 1,
    "expected layout to use multiple rayon threads"
  );
}

#[test]
fn auto_parallel_layout_engages_on_wide_html() {
  let mut html = String::from("<!DOCTYPE html><body>");
  for idx in 0..1024 {
    let _ = writeln!(html, "<div class=\"item\">row-{idx}</div>");
  }
  html.push_str("</body>");

  let mut renderer =
    FastRender::with_config(FastRenderConfig::new().with_default_viewport(1200, 2400))
      .expect("renderer");

  enable_layout_parallel_debug_counters(true);
  reset_layout_parallel_debug_counters();
  let options = RenderOptions::new()
    .with_layout_parallelism(LayoutParallelism::auto(DEFAULT_LAYOUT_MIN_FANOUT));
  let _ = renderer
    .render_html_with_stylesheets_report(
      &html,
      "https://example.com/",
      options,
      RenderArtifactRequest::none(),
    )
    .expect("auto parallel render");
  let counters = layout_parallel_debug_counters();
  enable_layout_parallel_debug_counters(false);
  reset_layout_parallel_debug_counters();

  assert!(
    counters.worker_threads > 1,
    "expected auto layout parallelism to use multiple workers, counters={counters:?}"
  );
  assert!(
    counters.work_items > 0,
    "expected auto layout parallelism to record work items"
  );
}
