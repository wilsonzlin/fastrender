use fastrender::api::{FastRender, FastRenderConfig, RenderOptions, ResourceKind};
use tiny_skia::Pixmap;

fn pixel(pixmap: &Pixmap, x: u32, y: u32) -> (u8, u8, u8, u8) {
  let px = pixmap.pixel(x, y).unwrap();
  (px.red(), px.green(), px.blue(), px.alpha())
}

#[test]
fn blocks_file_iframe_in_https_document() {
  let temp = tempfile::tempdir().unwrap();
  let inner_path = temp.path().join("inner.html");
  std::fs::write(
    &inner_path,
    "<!doctype html><style>html, body { margin: 0; background: rgb(255, 0, 0); }</style>",
  )
  .unwrap();

  let file_url = format!("file://{}", inner_path.display());
  let parent = format!(
    r#"<!doctype html><html><body style="margin:0; background: rgb(0, 200, 0);">
<iframe src="{file_url}" style="width:48px; height:48px; border:0;"></iframe>
</body></html>"#
  );

  let mut renderer = FastRender::new().unwrap();
  let result = renderer
    .render_html_with_stylesheets(&parent, "https://example.test/page", RenderOptions::new())
    .unwrap();

  let diagnostics = result.diagnostics;
  assert!(
    diagnostics
      .fetch_errors
      .iter()
      .any(|e| e.kind == ResourceKind::Document && e.url == file_url),
    "should record blocked iframe load"
  );

  // The parent background should remain visible outside the iframe.
  assert_eq!(pixel(&result.pixmap, 60, 1), (0, 200, 0, 255));
  // The blocked iframe must not render the file document's red background.
  assert_ne!(pixel(&result.pixmap, 1, 1), (255, 0, 0, 255));
}

#[test]
fn allows_file_iframe_when_opted_in() {
  let temp = tempfile::tempdir().unwrap();
  let inner_path = temp.path().join("inner.html");
  std::fs::write(
    &inner_path,
    "<!doctype html><style>html, body { margin: 0; background: rgb(255, 0, 0); }</style>",
  )
  .unwrap();

  let file_url = format!("file://{}", inner_path.display());
  let parent = format!(
    r#"<!doctype html><html><body style="margin:0; background: rgb(0, 200, 0);">
<iframe src="{file_url}" style="width:48px; height:48px; border:0;"></iframe>
</body></html>"#
  );

  let config = FastRenderConfig::new().with_allow_file_from_http(true);
  let mut renderer = FastRender::with_config(config).unwrap();
  let result = renderer
    .render_html_with_stylesheets(&parent, "https://example.test/page", RenderOptions::new())
    .unwrap();

  assert!(
    result
      .diagnostics
      .fetch_errors
      .iter()
      .all(|e| e.url != file_url),
    "opt-in should permit file iframe loads"
  );
  assert_eq!(pixel(&result.pixmap, 1, 1), (255, 0, 0, 255));
}
