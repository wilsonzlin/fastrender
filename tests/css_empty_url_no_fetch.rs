use std::sync::Arc;

use fastrender::resource::{FetchedResource, ResourceFetcher};
use fastrender::{DiagnosticsLevel, FastRender, RenderOptions};

#[derive(Clone)]
struct PanicFetcher;

impl ResourceFetcher for PanicFetcher {
  fn fetch(&self, url: &str) -> fastrender::Result<FetchedResource> {
    panic!("unexpected resource fetch for {url}");
  }
}

#[test]
fn empty_css_url_values_do_not_trigger_fetches() {
  let mut renderer = FastRender::builder()
    .fetcher(Arc::new(PanicFetcher) as Arc<dyn ResourceFetcher>)
    .build()
    .expect("renderer should build");

  let html = r#"<!doctype html>
    <style>
      html, body { margin: 0; padding: 0; }
      #empty-quoted { width: 8px; height: 8px; background-image: url(""); }
      #empty-unquoted { width: 8px; height: 8px; background: #fff url() repeat; }
    </style>
    <div id="empty-quoted"></div>
    <div id="empty-unquoted"></div>
  "#;

  let options = RenderOptions::new()
    .with_viewport(32, 16)
    .with_diagnostics_level(DiagnosticsLevel::Basic);
  let result = renderer
    .render_html_with_stylesheets(html, "https://example.test/", options)
    .expect("render should succeed");

  assert!(
    result.diagnostics.fetch_errors.is_empty(),
    "expected no fetch errors but got {:?}",
    result.diagnostics.fetch_errors
  );
}

