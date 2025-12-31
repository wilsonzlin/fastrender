use fastrender::api::{DiagnosticsLevel, FastRender, RenderOptions};

#[test]
fn resource_cache_diagnostics_surface_hits_and_misses() {
  let mut renderer = FastRender::new().expect("create renderer");
  let options = RenderOptions::new()
    .with_viewport(200, 200)
    .with_diagnostics_level(DiagnosticsLevel::Basic);

  let html = r#"<!doctype html>
<html>
  <head>
    <link rel="stylesheet" href="data:text/css,body{color:red}">
  </head>
  <body>hello</body>
</html>"#;

  let first = renderer
    .render_html_with_diagnostics(html, options.clone())
    .expect("first render");
  let first_stats = first
    .diagnostics
    .stats
    .expect("first render should include stats");
  assert!(
    first_stats
      .resources
      .resource_cache_misses
      .expect("resource cache misses should be present")
      >= 1
  );

  let second = renderer
    .render_html_with_diagnostics(html, options)
    .expect("second render");
  let second_stats = second
    .diagnostics
    .stats
    .expect("second render should include stats");
  assert!(
    second_stats
      .resources
      .resource_cache_fresh_hits
      .expect("resource cache hits should be present")
      >= 1
  );
}
