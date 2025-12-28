use std::sync::Arc;
use std::time::Duration;

use fastrender::api::{FastRender, FastRenderBuilder, RenderOptions, ResourceKind};
use fastrender::error::{Error, RenderStage, ResourceError};
use fastrender::resource::{FetchedResource, ResourceFetcher};

#[derive(Clone, Default)]
struct AlwaysErrorFetcher;

impl ResourceFetcher for AlwaysErrorFetcher {
  fn fetch(&self, url: &str) -> fastrender::Result<FetchedResource> {
    Err(Error::Resource(ResourceError::new(
      url.to_string(),
      "not found",
    )))
  }
}

struct EnvVarGuard {
  key: &'static str,
}

impl EnvVarGuard {
  fn set(key: &'static str, value: &str) -> Self {
    std::env::set_var(key, value);
    Self { key }
  }
}

impl Drop for EnvVarGuard {
  fn drop(&mut self) {
    std::env::remove_var(self.key);
  }
}

#[test]
fn missing_stylesheet_keeps_rendering_with_diagnostics() {
  let fetcher = Arc::new(AlwaysErrorFetcher::default()) as Arc<dyn ResourceFetcher>;
  let mut renderer = FastRenderBuilder::new().fetcher(fetcher).build().unwrap();
  let html = r#"
    <html>
      <head>
        <link rel="stylesheet" href="https://example.invalid/missing.css">
      </head>
      <body><p>content</p></body>
    </html>
  "#;

  let options = RenderOptions::new()
    .with_viewport(64, 48)
    .allow_partial(true);
  let result = renderer
    .render_html_with_stylesheets(html, "https://example.invalid/", options)
    .expect("missing stylesheet should not abort render");

  assert_eq!(result.pixmap.width(), 64);
  assert_eq!(result.pixmap.height(), 48);
  assert_eq!(result.diagnostics.failure_stage, Some(RenderStage::Css));
  let stylesheet_error = result
    .diagnostics
    .fetch_errors
    .iter()
    .find(|entry| entry.kind == ResourceKind::Stylesheet)
    .expect("stylesheet error recorded");
  assert!(stylesheet_error.url.contains("missing.css"));
}

#[test]
fn missing_image_renders_placeholder() {
  let fetcher = Arc::new(AlwaysErrorFetcher::default()) as Arc<dyn ResourceFetcher>;
  let mut renderer = FastRenderBuilder::new().fetcher(fetcher).build().unwrap();
  let html = r#"
    <style>
      body { margin: 0; }
      img { display: block; width: 20px; height: 20px; }
    </style>
    <img src="https://example.invalid/missing.png" alt="missing">
  "#;
  let options = RenderOptions::new()
    .with_viewport(32, 32)
    .allow_partial(true);
  let result = renderer
    .render_html_with_diagnostics(html, options)
    .expect("missing image should not abort render");

  let pixel = result.pixmap.pixel(1, 1).expect("sample pixel");
  assert_eq!((pixel.red(), pixel.green(), pixel.blue()), (200, 200, 200));

  let image_error = result
    .diagnostics
    .fetch_errors
    .iter()
    .find(|entry| entry.kind == ResourceKind::Image)
    .expect("image fetch error recorded");
  assert!(image_error.url.contains("missing.png"));
}

#[test]
fn timeouts_produce_overlay_with_stage_info() {
  let _guard = EnvVarGuard::set("FASTR_TEST_RENDER_DELAY_MS", "20");
  let mut renderer = FastRender::new().unwrap();
  let options = RenderOptions::new()
    .with_viewport(16, 12)
    .with_timeout(Some(Duration::from_millis(1)))
    .allow_partial(true);

  let result = renderer
    .render_html_with_stylesheets("<div>slow</div>", "https://example.com", options)
    .expect("allow_partial should return overlay on timeout");

  let overlay = result.pixmap.pixel(0, 0).expect("overlay pixel");
  assert_eq!(
    (overlay.red(), overlay.green(), overlay.blue()),
    (255, 235, 238)
  );
  assert_eq!(
    result.diagnostics.timeout_stage,
    Some(RenderStage::DomParse)
  );
  assert_eq!(
    result.diagnostics.failure_stage,
    Some(RenderStage::DomParse)
  );
}
