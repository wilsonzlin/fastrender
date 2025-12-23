use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use fastrender::{
  error::Error, FastRenderBuilder, FetchedResource, RenderOptions, ResourceFetcher, ResourceKind,
};

#[derive(Clone, Default)]
struct MockFetcher {
  responses: Arc<Mutex<HashMap<String, MockResponse>>>,
}

#[derive(Clone)]
enum MockResponse {
  Success {
    bytes: Vec<u8>,
    content_type: Option<String>,
  },
  Error(String),
}

impl MockFetcher {
  fn with_html(self, url: &str, body: &str) -> Self {
    self.with_response(url, body, Some("text/html"))
  }

  fn with_response(mut self, url: &str, body: &str, content_type: Option<&str>) -> Self {
    self.responses.lock().unwrap().insert(
      url.to_string(),
      MockResponse::Success {
        bytes: body.as_bytes().to_vec(),
        content_type: content_type.map(str::to_string),
      },
    );
    self
  }

  fn with_error(mut self, url: &str, message: &str) -> Self {
    self
      .responses
      .lock()
      .unwrap()
      .insert(url.to_string(), MockResponse::Error(message.to_string()));
    self
  }
}

impl ResourceFetcher for MockFetcher {
  fn fetch(&self, url: &str) -> fastrender::Result<FetchedResource> {
    let responses = self.responses.lock().unwrap();
    match responses.get(url) {
      Some(MockResponse::Success {
        bytes,
        content_type,
      }) => Ok(FetchedResource::with_final_url(
        bytes.clone(),
        content_type.clone(),
        Some(url.to_string()),
      )),
      Some(MockResponse::Error(message)) => Err(Error::Other(message.clone())),
      None => Err(Error::Other(format!("no mock response for {url}"))),
    }
  }
}

#[test]
fn partial_render_returns_placeholder_with_diagnostics_on_document_failure() {
  let url = "https://example.test/missing";
  let fetcher = MockFetcher::default().with_error(url, "network down");
  let fetcher = Arc::new(fetcher) as Arc<dyn ResourceFetcher>;

  let mut renderer = FastRenderBuilder::new().fetcher(fetcher).build().unwrap();
  let options = RenderOptions::new()
    .with_viewport(32, 24)
    .allow_partial(true);

  let result = renderer
    .render_url_with_options(url, options)
    .expect("partial render should return placeholder");

  assert_eq!(result.pixmap.width(), 32);
  assert_eq!(result.pixmap.height(), 24);

  let diagnostics = result.diagnostics;
  assert!(diagnostics.document_error.is_some());
  assert_eq!(diagnostics.fetch_errors.len(), 1);
  let entry = &diagnostics.fetch_errors[0];
  assert_eq!(entry.kind, ResourceKind::Document);
  assert_eq!(entry.url, url);
  assert!(entry.message.contains("network down"));
}

#[test]
fn stylesheet_fetch_failure_is_recorded_without_stopping_render() {
  let doc_url = "https://example.test/page.html";
  let css_url = "https://example.test/styles.css";
  let html = format!(
    r#"<html><head><link rel="stylesheet" href="{css_url}"></head><body><p>Hello</p></body></html>"#
  );

  let fetcher = MockFetcher::default()
    .with_html(doc_url, &html)
    .with_error(css_url, "css missing");
  let fetcher = Arc::new(fetcher) as Arc<dyn ResourceFetcher>;

  let mut renderer = FastRenderBuilder::new().fetcher(fetcher).build().unwrap();
  let result = renderer.render_url(doc_url).expect("render should succeed");

  let diagnostics = &result.diagnostics;
  assert!(diagnostics.document_error.is_none());
  let stylesheet_error = diagnostics
    .fetch_errors
    .iter()
    .find(|entry| entry.kind == ResourceKind::Stylesheet && entry.url == css_url)
    .expect("stylesheet error recorded");
  assert!(stylesheet_error.message.contains("css missing"));

  assert!(result.pixmap.width() > 0);
  assert!(result.pixmap.height() > 0);
}
