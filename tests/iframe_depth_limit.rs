use fastrender::api::{FastRender, FastRenderConfig, RenderOptions, ResourceKind};
use tiny_skia::Pixmap;

fn pixel(pixmap: &Pixmap, x: u32, y: u32) -> (u8, u8, u8, u8) {
  let px = pixmap.pixel(x, y).unwrap();
  (px.red(), px.green(), px.blue(), px.alpha())
}

#[test]
fn records_depth_exhaustion_for_nested_iframe() {
  let inner = "<!doctype html><style>html, body { margin: 0; background: rgb(255, 0, 0); }</style>";
  let middle = format!(
    "<!doctype html><style>html, body {{ margin: 0; background: rgb(0, 200, 0); }}</style>\
     <iframe srcdoc=\"{}\" style=\"width: 16px; height: 16px; border: 0; display: block;\"></iframe>",
    inner.replace('"', "&quot;")
  );
  let outer = format!(
    "<!doctype html><body style=\"margin:0; background: rgb(0, 0, 200);\">\
     <iframe srcdoc=\"{}\" style=\"width: 24px; height: 24px; border: 0; display: block;\"></iframe>\
     </body>",
    middle.replace('"', "&quot;")
  );

  let mut config = FastRenderConfig::new();
  config.max_iframe_depth = 1;
  let mut renderer = FastRender::with_config(config).expect("renderer");
  let result = renderer
    .render_html_with_stylesheets(
      &outer,
      "https://example.test/page",
      RenderOptions::new().with_viewport(32, 32),
    )
    .expect("render");

  // The inner iframe should be skipped at depth 0, leaving the middle document's background visible.
  assert_eq!(
    pixel(&result.pixmap, 5, 5),
    (0, 200, 0, 255),
    "inner iframe paint should be skipped when nesting depth is exhausted"
  );

  assert!(
    result.diagnostics.fetch_errors.iter().any(|e| {
      e.kind == ResourceKind::Document
        && e.url == "about:srcdoc"
        && e.message.contains("iframe nesting limit exceeded")
    }),
    "expected diagnostics to record iframe nesting limit exhaustion"
  );
}
