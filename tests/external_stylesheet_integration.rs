use fastrender::api::FastRender;
use fastrender::error::Error;
use fastrender::resource::{CachingFetcher, FetchedResource, ResourceFetcher};
use fastrender::style::color::Rgba;
use fastrender::tree::fragment_tree::{FragmentContent, FragmentTree};
use std::collections::HashMap;
use std::fs;
use std::io;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::time::Duration;
use tempfile::tempdir;

fn text_color_for(tree: &FragmentTree, needle: &str) -> Option<Rgba> {
  tree.iter_fragments().find_map(|frag| match &frag.content {
    FragmentContent::Text { text, .. } if text.contains(needle) => {
      frag.style.as_ref().map(|s| s.color)
    }
    _ => None,
  })
}

#[test]
fn external_file_stylesheet_affects_layout() {
  let dir = tempdir().unwrap();
  let css_path = dir.path().join("style.css");
  fs::write(&css_path, "body { color: rgb(12, 34, 56); }").unwrap();

  let base_url = format!("file://{}/index.html", dir.path().display());
  let html = r#"
    <html>
      <head>
        <link rel="STYLESHEET" href="style.css">
      </head>
      <body>
        <div id="text">Hello external</div>
      </body>
    </html>
  "#;

  let mut renderer = FastRender::builder().base_url(base_url).build().unwrap();

  let dom = renderer.parse_html(html).unwrap();
  let tree = renderer.layout_document(&dom, 200, 200).unwrap();

  let color = text_color_for(&tree, "Hello external").expect("text fragment");
  assert_eq!(color, Rgba::rgb(12, 34, 56));
}

#[derive(Clone)]
struct DelayedStylesheetFetcher {
  entries: Arc<HashMap<String, (Vec<u8>, Option<String>, Duration)>>,
}

impl DelayedStylesheetFetcher {
  fn new(entries: Vec<(&str, &str, Duration)>) -> Self {
    let map: HashMap<String, (Vec<u8>, Option<String>, Duration)> = entries
      .into_iter()
      .map(|(url, body, delay)| {
        (
          url.to_string(),
          (
            body.as_bytes().to_vec(),
            Some("text/css".to_string()),
            delay,
          ),
        )
      })
      .collect();
    Self {
      entries: Arc::new(map),
    }
  }
}

impl ResourceFetcher for DelayedStylesheetFetcher {
  fn fetch(&self, url: &str) -> fastrender::Result<FetchedResource> {
    let Some((bytes, content_type, delay)) = self.entries.get(url) else {
      return Err(Error::Io(io::Error::new(
        io::ErrorKind::NotFound,
        format!("missing mocked resource: {url}"),
      )));
    };
    if !delay.is_zero() {
      std::thread::sleep(*delay);
    }
    Ok(FetchedResource::with_final_url(
      bytes.clone(),
      content_type.clone(),
      Some(url.to_string()),
    ))
  }
}

#[test]
fn external_stylesheets_preserve_document_order_under_parallel_fetch() {
  let first_url = "https://example.test/slow.css";
  let second_url = "https://example.test/fast.css";
  let fetcher = Arc::new(DelayedStylesheetFetcher::new(vec![
    (
      first_url,
      "#text { color: rgb(1, 2, 3); }",
      Duration::from_millis(40),
    ),
    (
      second_url,
      "#text { color: rgb(4, 5, 6); }",
      Duration::from_millis(0),
    ),
  ])) as Arc<dyn ResourceFetcher>;

  let html = format!(
    r#"
    <html>
      <head>
        <link rel="stylesheet" href="{first_url}">
        <link rel="stylesheet" href="{second_url}">
      </head>
      <body>
        <div id="text">Hello ordering</div>
      </body>
    </html>
  "#
  );

  let pool = rayon::ThreadPoolBuilder::new()
    .num_threads(4)
    .build()
    .expect("rayon pool");

  for _ in 0..25 {
    let mut renderer = FastRender::builder()
      .fetcher(fetcher.clone())
      .build()
      .unwrap();
    let dom = renderer.parse_html(&html).unwrap();
    let tree = pool
      .install(|| renderer.layout_document(&dom, 200, 200))
      .unwrap();
    let color = text_color_for(&tree, "Hello ordering").expect("text fragment");
    assert_eq!(
      color,
      Rgba::rgb(4, 5, 6),
      "later stylesheet should win deterministically"
    );
  }
}

#[derive(Clone)]
struct CountingSlowFetcher {
  calls: Arc<AtomicUsize>,
  url: String,
  body: Vec<u8>,
}

impl CountingSlowFetcher {
  fn new(url: &str, body: &str) -> Self {
    Self {
      calls: Arc::new(AtomicUsize::new(0)),
      url: url.to_string(),
      body: body.as_bytes().to_vec(),
    }
  }
}

impl ResourceFetcher for CountingSlowFetcher {
  fn fetch(&self, url: &str) -> fastrender::Result<FetchedResource> {
    if url != self.url {
      return Err(Error::Io(io::Error::new(
        io::ErrorKind::NotFound,
        format!("unexpected url: {url}"),
      )));
    }
    self.calls.fetch_add(1, Ordering::SeqCst);
    // Make the initial call slow enough that concurrent fetches overlap, exercising single-flight.
    std::thread::sleep(Duration::from_millis(60));
    Ok(FetchedResource::with_final_url(
      self.body.clone(),
      Some("text/css".to_string()),
      Some(url.to_string()),
    ))
  }
}

#[test]
fn caching_fetcher_single_flights_parallel_stylesheet_requests() {
  let url = "https://example.test/shared.css";
  let inner = CountingSlowFetcher::new(url, "#text { color: rgb(7, 8, 9); }");
  let calls = inner.calls.clone();
  let caching = CachingFetcher::new(inner);
  let fetcher = Arc::new(caching) as Arc<dyn ResourceFetcher>;

  let links = (0..64)
    .map(|_| format!(r#"<link rel="stylesheet" href="{url}">"#))
    .collect::<Vec<_>>()
    .join("\n");
  let html = format!(
    r#"
    <html>
      <head>{links}</head>
      <body><div id="text">Hello caching</div></body>
    </html>
  "#
  );

  let pool = rayon::ThreadPoolBuilder::new()
    .num_threads(8)
    .build()
    .expect("rayon pool");

  let mut renderer = FastRender::builder().fetcher(fetcher).build().unwrap();
  let dom = renderer.parse_html(&html).unwrap();
  let _ = pool
    .install(|| renderer.layout_document(&dom, 200, 200))
    .unwrap();

  assert_eq!(
    calls.load(Ordering::SeqCst),
    1,
    "CachingFetcher should de-duplicate concurrent fetches"
  );
}
