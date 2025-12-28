use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use base64::{engine::general_purpose, Engine as _};
use fastrender::api::{FastRenderBuilder, ResourceKind};
use fastrender::{Error, FetchedResource, RenderResult, ResourceFetcher};

#[derive(Clone, Default)]
struct MockFetcher {
  responses: Arc<Mutex<HashMap<String, FetchedResource>>>,
}

impl MockFetcher {
  fn with_bytes(self, url: &str, bytes: Vec<u8>, content_type: Option<&str>) -> Self {
    self.responses.lock().unwrap().insert(
      url.to_string(),
      FetchedResource::with_final_url(
        bytes,
        content_type.map(str::to_string),
        Some(url.to_string()),
      ),
    );
    self
  }

  fn with_html(self, url: &str, body: &str) -> Self {
    self.with_bytes(url, body.as_bytes().to_vec(), Some("text/html"))
  }
}

impl ResourceFetcher for MockFetcher {
  fn fetch(&self, url: &str) -> fastrender::Result<FetchedResource> {
    self
      .responses
      .lock()
      .unwrap()
      .get(url)
      .cloned()
      .ok_or_else(|| Error::Other(format!("no mock response for {url}")))
  }
}

fn tiny_png() -> Vec<u8> {
  general_purpose::STANDARD
    .decode("iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMBAFXX5uoAAAAASUVORK5CYII=")
    .expect("decode base64 png")
}

fn render_with_fetcher(url: &str, fetcher: MockFetcher) -> RenderResult {
  let fetcher = Arc::new(fetcher) as Arc<dyn ResourceFetcher>;
  let mut renderer = FastRenderBuilder::new().fetcher(fetcher).build().unwrap();
  renderer.render_url(url).unwrap()
}

#[test]
fn blocks_mixed_content_images_in_https_documents() {
  let doc_url = "https://example.test/page.html";
  let image_url = "http://mixed.test/image.png";
  let html = format!(r#"<!doctype html><img src="{image_url}">"#);

  let fetcher = MockFetcher::default().with_html(doc_url, &html);
  let fetcher = Arc::new(fetcher) as Arc<dyn ResourceFetcher>;
  let mut renderer = FastRenderBuilder::new()
    .block_mixed_content(true)
    .fetcher(fetcher)
    .build()
    .unwrap();

  let result = renderer.render_url(doc_url).unwrap();
  assert!(result.pixmap.width() > 0);
  assert!(result.pixmap.height() > 0);

  assert!(
    result
      .diagnostics
      .fetch_errors
      .iter()
      .any(|entry| entry.kind == ResourceKind::Image
        && entry.url == image_url
        && entry.message.contains("Blocked mixed HTTP content")),
    "expected mixed-content image fetch to be blocked"
  );
}

#[test]
fn blocks_file_images_in_http_documents() {
  let doc_url = "http://example.test/page.html";
  let temp = tempfile::tempdir().unwrap();
  let file_url = format!("file://{}", temp.path().join("image.png").display());
  let html = format!(r#"<!doctype html><img src="{file_url}">"#);

  let fetcher = MockFetcher::default().with_html(doc_url, &html).with_bytes(
    &file_url,
    tiny_png(),
    Some("image/png"),
  );

  let result = render_with_fetcher(doc_url, fetcher);

  assert!(
    result
      .diagnostics
      .fetch_errors
      .iter()
      .any(|entry| entry.kind == ResourceKind::Image
        && entry.url == file_url
        && entry
          .message
          .contains("Blocked file:// resource from HTTP(S) document")),
    "expected file image to be blocked in http document"
  );
}

#[test]
fn blocks_cached_mixed_content_images() {
  let http_doc_url = "http://example.test/page.html";
  let https_doc_url = "https://example.test/secure.html";
  let image_url = "http://mixed.test/image.png";
  let png_bytes = tiny_png();

  let fetcher = MockFetcher::default()
    .with_html(
      http_doc_url,
      &format!(r#"<!doctype html><img src="{image_url}">"#),
    )
    .with_html(
      https_doc_url,
      &format!(r#"<!doctype html><img src="{image_url}">"#),
    )
    .with_bytes(image_url, png_bytes, Some("image/png"));

  let fetcher = Arc::new(fetcher) as Arc<dyn ResourceFetcher>;
  let mut renderer = FastRenderBuilder::new()
    .block_mixed_content(true)
    .fetcher(fetcher)
    .build()
    .unwrap();

  // Warm the image cache from an HTTP document where mixed content is allowed.
  renderer.render_url(http_doc_url).unwrap();

  let result = renderer.render_url(https_doc_url).unwrap();
  assert!(
    result
      .diagnostics
      .fetch_errors
      .iter()
      .any(|entry| entry.kind == ResourceKind::Image
        && entry.url == image_url
        && entry.message.contains("Blocked mixed HTTP content")),
    "expected cached mixed-content image to be blocked for HTTPS documents"
  );
}

#[test]
fn blocks_cached_file_images_in_http_documents() {
  let temp = tempfile::tempdir().unwrap();
  let file_doc_url = format!("file://{}", temp.path().join("page.html").display());
  let http_doc_url = "http://example.test/page.html";
  let file_url = format!("file://{}", temp.path().join("image.png").display());

  let fetcher = MockFetcher::default()
    .with_html(
      &file_doc_url,
      &format!(r#"<!doctype html><img src="{file_url}">"#),
    )
    .with_html(
      http_doc_url,
      &format!(r#"<!doctype html><img src="{file_url}">"#),
    )
    .with_bytes(&file_url, tiny_png(), Some("image/png"));

  let fetcher = Arc::new(fetcher) as Arc<dyn ResourceFetcher>;
  let mut renderer = FastRenderBuilder::new().fetcher(fetcher).build().unwrap();

  // Cache the file:// image from a file document first.
  renderer.render_url(&file_doc_url).unwrap();

  let result = renderer.render_url(http_doc_url).unwrap();
  assert!(
    result
      .diagnostics
      .fetch_errors
      .iter()
      .any(|entry| entry.kind == ResourceKind::Image
        && entry.url == file_url
        && entry
          .message
          .contains("Blocked file:// resource from HTTP(S) document")),
    "expected cached file images to be blocked in HTTP documents"
  );
}
