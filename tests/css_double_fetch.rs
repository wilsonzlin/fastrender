use fastrender::api::{FastRender, RenderOptions};
use fastrender::resource::{FetchedResource, ResourceFetcher};
use fastrender::Error;
use std::collections::HashMap;
use std::io;
use std::sync::{Arc, Mutex};

#[derive(Clone, Default)]
struct MapFetcher {
  map: HashMap<String, (Vec<u8>, Option<String>)>,
}

impl MapFetcher {
  fn with_css(mut self, url: &str, css: &str) -> Self {
    self.map.insert(
      url.to_string(),
      (css.as_bytes().to_vec(), Some("text/css".to_string())),
    );
    self
  }
}

impl ResourceFetcher for MapFetcher {
  fn fetch(&self, url: &str) -> fastrender::Result<FetchedResource> {
    self
      .map
      .get(url)
      .map(|(bytes, content_type)| FetchedResource::new(bytes.clone(), content_type.clone()))
      .ok_or_else(|| {
        Error::Io(io::Error::new(
          io::ErrorKind::NotFound,
          format!("missing resource: {url}"),
        ))
      })
  }
}

#[derive(Clone)]
struct CountingFetcher {
  inner: Arc<dyn ResourceFetcher>,
  counts: Arc<Mutex<HashMap<String, usize>>>,
}

impl CountingFetcher {
  fn new(inner: Arc<dyn ResourceFetcher>) -> (Self, Arc<Mutex<HashMap<String, usize>>>) {
    let counts = Arc::new(Mutex::new(HashMap::new()));
    (
      Self {
        inner,
        counts: Arc::clone(&counts),
      },
      counts,
    )
  }
}

impl ResourceFetcher for CountingFetcher {
  fn fetch(&self, url: &str) -> fastrender::Result<FetchedResource> {
    {
      let mut guard = self.counts.lock().unwrap();
      *guard.entry(url.to_string()).or_default() += 1;
    }
    self.inner.fetch(url)
  }

  fn fetch_with_validation(
    &self,
    url: &str,
    etag: Option<&str>,
    last_modified: Option<&str>,
  ) -> fastrender::Result<FetchedResource> {
    {
      let mut guard = self.counts.lock().unwrap();
      *guard.entry(url.to_string()).or_default() += 1;
    }
    self.inner.fetch_with_validation(url, etag, last_modified)
  }
}

#[test]
fn render_html_with_stylesheets_does_not_double_fetch_linked_css() {
  let css_url = "https://example.com/style.css";
  let css = "#target { width: 10px; height: 10px; background: rgb(255, 0, 0); }";
  let base_fetcher =
    Arc::new(MapFetcher::default().with_css(css_url, css)) as Arc<dyn ResourceFetcher>;
  let (counting_fetcher, counts) = CountingFetcher::new(base_fetcher);

  let mut renderer = FastRender::builder()
    .fetcher(Arc::new(counting_fetcher) as Arc<dyn ResourceFetcher>)
    .build()
    .expect("renderer should build");

  let html = r#"
    <!doctype html>
    <html>
      <head>
        <style>html, body { margin: 0; padding: 0; }</style>
        <link rel="stylesheet" href="style.css">
      </head>
      <body><div id="target"></div></body>
    </html>
  "#;

  let result = renderer
    .render_html_with_stylesheets(
      html,
      "https://example.com/",
      RenderOptions::new().with_viewport(20, 20),
    )
    .expect("render should succeed");

  let pixel = result
    .pixmap
    .pixel(1, 1)
    .expect("expected pixel inside viewport");
  assert_eq!(
    pixel.red(),
    255,
    "expected red pixel from stylesheet background"
  );
  assert_eq!(
    pixel.green(),
    0,
    "expected red pixel from stylesheet background"
  );
  assert_eq!(
    pixel.blue(),
    0,
    "expected red pixel from stylesheet background"
  );

  let fetches = counts.lock().unwrap();
  assert_eq!(
    fetches.get(css_url).copied().unwrap_or(0),
    1,
    "linked stylesheet should be fetched exactly once per render"
  );
}
