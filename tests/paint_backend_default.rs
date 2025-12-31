use fastrender::{DiagnosticsLevel, FastRender, RenderOptions};

#[test]
fn default_paint_backend_uses_display_list_pipeline() {
  // Ensure we exercise the default selection logic.
  std::env::remove_var("FASTR_PAINT_BACKEND");

  let mut renderer = FastRender::new().expect("renderer should construct");
  let html = r#"
    <style>
      body { margin: 0; }
      .box { width: 10px; height: 10px; background: #f00; }
    </style>
    <div class="box"></div>
  "#;
  let options = RenderOptions::new()
    .with_viewport(20, 20)
    .with_diagnostics_level(DiagnosticsLevel::Basic);

  let result = renderer
    .render_html_with_diagnostics(html, options)
    .expect("render should succeed");
  let stats = result
    .diagnostics
    .stats
    .as_ref()
    .expect("stats should be present");

  assert!(
    stats.paint.optimized_items.is_some(),
    "expected default paint backend to run the display-list optimizer; paint_optimize_ms={:?} optimized_items={:?}",
    stats.timings.paint_optimize_ms,
    stats.paint.optimized_items
  );
}

