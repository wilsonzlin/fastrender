use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use base64::{engine::general_purpose, Engine as _};
use fastrender::{
  error::{Error, ResourceError},
  DiagnosticsLevel, FastRender, FastRenderBuilder, FetchedResource, OutputFormat, RenderDiagnostics,
  RenderOptions, ResourceFetcher, ResourceKind,
};

fn run_with_large_stack(f: impl FnOnce() + Send + 'static) {
  std::thread::Builder::new()
    .stack_size(8 * 1024 * 1024)
    .spawn(f)
    .expect("spawn thread")
    .join()
    .expect("join thread");
}

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

  fn with_bytes(mut self, url: &str, bytes: Vec<u8>, content_type: Option<&str>) -> Self {
    self.responses.lock().unwrap().insert(
      url.to_string(),
      MockResponse::Success {
        bytes,
        content_type: content_type.map(str::to_string),
      },
    );
    self
  }

  fn with_response(mut self, url: &str, body: &str, content_type: Option<&str>) -> Self {
    self = self.with_bytes(url, body.as_bytes().to_vec(), content_type);
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
  run_with_large_stack(|| {
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
    assert_eq!(entry.status, None);
    assert_eq!(entry.final_url.as_deref(), Some(url));
  });
}

#[test]
fn stylesheet_fetch_failure_is_recorded_without_stopping_render() {
  run_with_large_stack(|| {
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
    assert_eq!(stylesheet_error.status, None);
    assert_eq!(stylesheet_error.final_url.as_deref(), Some(css_url));

    assert!(result.pixmap.width() > 0);
    assert!(result.pixmap.height() > 0);
  });
}

#[test]
fn resource_error_metadata_is_captured() {
  let mut diagnostics = RenderDiagnostics::new();
  let err = Error::Resource(
    ResourceError::new("https://example.test/style.css", "not found")
      .with_status(404)
      .with_final_url("https://example.test/style.css?v=1")
      .with_validators(
        Some("etag-123".to_string()),
        Some("Tue, 20 Feb 2024 20:00:00 GMT".to_string()),
      ),
  );
  diagnostics.record_error(
    ResourceKind::Stylesheet,
    "https://example.test/style.css",
    &err,
  );

  let entry = diagnostics
    .fetch_errors
    .first()
    .expect("recorded diagnostics entry");
  assert_eq!(entry.status, Some(404));
  assert_eq!(
    entry.final_url.as_deref(),
    Some("https://example.test/style.css?v=1")
  );
  assert_eq!(entry.etag.as_deref(), Some("etag-123"));
  assert_eq!(
    entry.last_modified.as_deref(),
    Some("Tue, 20 Feb 2024 20:00:00 GMT")
  );
  assert!(entry.message.contains("not found"));
}

#[test]
fn diagnostics_report_includes_stage_stats() {
  run_with_large_stack(|| {
    let url = "https://example.test/diag";
    let html = "<!doctype html><html><head><style>p { margin: 0; }</style></head><body><p>Hi</p></body></html>";
    let fetcher =
      Arc::new(MockFetcher::default().with_html(url, html)) as Arc<dyn ResourceFetcher>;

    let mut renderer = FastRenderBuilder::new().fetcher(fetcher).build().unwrap();
    let options = RenderOptions::new()
      .with_viewport(64, 48)
      .with_diagnostics_level(DiagnosticsLevel::Basic);

    let result = renderer.render_url_with_options(url, options).unwrap();
    let stats = result
      .diagnostics
      .stats
      .as_ref()
      .expect("stats should be populated");

    assert!(stats.timings.dom_parse_ms.is_some());
    assert!(stats.timings.layout_ms.is_some());
    assert!(stats.counts.dom_nodes.unwrap_or(0) > 0);
    assert!(stats.counts.box_nodes.unwrap_or(0) > 0);
    assert!(stats.counts.fragments.unwrap_or(0) > 0);
    assert!(stats.paint.display_items.unwrap_or(0) > 0);
    assert_eq!(
      stats.resources.fetch_counts.get(&ResourceKind::Document),
      Some(&1usize)
    );
  });
}

#[test]
fn diagnostics_track_resource_fetch_counts() {
  run_with_large_stack(|| {
    let doc_url = "https://example.test/counts.html";
    let css_url = "https://example.test/styles.css";
    let image_url = "https://example.test/image.png";
    let png_bytes = general_purpose::STANDARD
      .decode("iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMBAFXX5uoAAAAASUVORK5CYII=")
      .expect("decode base64 png");
    let html = format!(
      r#"<html><head><link rel="stylesheet" href="{css_url}"></head><body><img src="{image_url}"></body></html>"#
    );

    let fetcher = Arc::new(
      MockFetcher::default()
        .with_html(doc_url, &html)
        .with_response(css_url, "body {{ margin: 0; }}", Some("text/css"))
        .with_bytes(image_url, png_bytes, Some("image/png")),
    ) as Arc<dyn ResourceFetcher>;

    let mut renderer = FastRenderBuilder::new().fetcher(fetcher).build().unwrap();
    let options = RenderOptions::new()
      .with_viewport(64, 48)
      .with_diagnostics_level(DiagnosticsLevel::Basic);
    let result = renderer
      .render_url_with_options(doc_url, options)
      .expect("render should succeed");
    let stats = result
      .diagnostics
      .stats
      .as_ref()
      .expect("stats should be present");

    assert_eq!(
      stats.resources.fetch_counts.get(&ResourceKind::Document),
      Some(&1usize)
    );
    assert_eq!(
      stats.resources.fetch_counts.get(&ResourceKind::Stylesheet),
      Some(&1usize)
    );
    assert_eq!(
      stats.resources.fetch_counts.get(&ResourceKind::Image),
      Some(&1usize)
    );
    assert_eq!(stats.resources.image_cache_misses, Some(1));
  });
}

#[test]
fn encode_timing_is_recorded() {
  run_with_large_stack(|| {
    let mut renderer = FastRender::new().unwrap();
    let options = RenderOptions::new()
      .with_viewport(16, 12)
      .with_diagnostics_level(DiagnosticsLevel::Basic);
    let result = renderer
      .render_html_with_diagnostics("<p>encode</p>", options)
      .expect("render should succeed");

    let (png, diagnostics) = result.encode(OutputFormat::Png).expect("encode should work");
    assert!(!png.is_empty());

    let stats = diagnostics.stats.expect("stats should be preserved");
    assert!(stats.timings.encode_ms.is_some());
  });
}
