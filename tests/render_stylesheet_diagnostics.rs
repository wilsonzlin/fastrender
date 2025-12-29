use fastrender::api::{DiagnosticsLevel, FastRender, RenderOptions};

#[test]
fn stylesheets_path_populates_stage_timings() {
  let mut renderer = FastRender::new().expect("renderer should initialize");
  let options = RenderOptions::new()
    .with_viewport(64, 48)
    .with_diagnostics_level(DiagnosticsLevel::Basic);
  let html = r#"
    <!doctype html>
    <html>
      <head><style>body { margin: 0; }</style></head>
      <body><div>hello</div></body>
    </html>
  "#;

  let result = renderer
    .render_html_with_stylesheets(html, "https://example.com", options)
    .expect("render should succeed with diagnostics");

  let stats = result
    .diagnostics
    .stats
    .expect("stats should be populated on diagnostics");
  assert!(
    stats.timings.css_inlining_ms.is_some()
      || stats.timings.dom_parse_ms.is_some()
      || stats.timings.layout_ms.is_some()
      || stats.timings.paint_build_ms.is_some(),
    "expected at least one timing to be recorded"
  );
}
