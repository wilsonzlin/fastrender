use std::collections::HashMap;
use std::io;
use std::sync::{Arc, Mutex};

use fastrender::api::FastRender;
use fastrender::css::parser::parse_stylesheet_with_media;
use fastrender::css::types::{CssImportLoader, StyleSheet};
use fastrender::debug::runtime::RuntimeToggles;
use fastrender::resource::{FetchedResource, ResourceFetcher};
use fastrender::style::media::{MediaContext, MediaQueryCache};
use fastrender::Error;

struct RecordingFetcher {
  map: HashMap<String, (Vec<u8>, Option<String>)>,
  requests: Mutex<Vec<String>>,
}

impl RecordingFetcher {
  fn new(map: HashMap<String, (Vec<u8>, Option<String>)>) -> Self {
    Self {
      map,
      requests: Mutex::new(Vec::new()),
    }
  }

  fn requests(&self) -> Vec<String> {
    self.requests.lock().unwrap().clone()
  }
}

impl ResourceFetcher for RecordingFetcher {
  fn fetch(&self, url: &str) -> fastrender::Result<FetchedResource> {
    self.requests.lock().unwrap().push(url.to_string());
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

struct MapLoader {
  map: HashMap<String, String>,
}

impl MapLoader {
  fn new(map: HashMap<String, String>) -> Self {
    Self { map }
  }
}

impl CssImportLoader for MapLoader {
  fn load(&self, url: &str) -> fastrender::Result<String> {
    self
      .map
      .get(url)
      .cloned()
      .ok_or_else(|| Error::Io(io::Error::new(io::ErrorKind::NotFound, url)))
  }
}

#[test]
fn collect_document_style_set_matches_resolved_stylesheet_rules() {
  let document_url = "https://example.com/index.html";
  let inline_a_url = "https://example.com/inline_a.css";
  let inline_b_url = "https://example.com/inline_b.css";
  let external_url = "https://example.com/external.css";
  let external_import_url = "https://example.com/external_import.css";
  let skipped_print_url = "https://example.com/skip_print.css";
  let skipped_supports_url = "https://example.com/skip_supports.css";

  let inline_css = r#"
    @import "inline_a.css";
    #inline { color: rgb(1, 2, 3); }
  "#;
  let inline_a_css = r#"
    @supports (display: block) {
      @import "inline_b.css";
    }
    .inline-a { color: rgb(10, 20, 30); }
  "#;
  let inline_b_css = ".inline-b { color: rgb(40, 50, 60); }";

  let external_css = r#"
    @import "external_import.css";
    @media print {
      @import "skip_print.css";
    }
    @supports (not-a-real-prop: 1) {
      @import "skip_supports.css";
    }
    #external { color: rgb(4, 5, 6); }
  "#;
  let external_import_css = ".external-import { color: rgb(7, 8, 9); }";

  let mut fetch_map: HashMap<String, (Vec<u8>, Option<String>)> = HashMap::new();
  fetch_map.insert(
    inline_a_url.to_string(),
    (inline_a_css.as_bytes().to_vec(), Some("text/css".to_string())),
  );
  fetch_map.insert(
    inline_b_url.to_string(),
    (inline_b_css.as_bytes().to_vec(), Some("text/css".to_string())),
  );
  fetch_map.insert(
    external_url.to_string(),
    (external_css.as_bytes().to_vec(), Some("text/css".to_string())),
  );
  fetch_map.insert(
    external_import_url.to_string(),
    (
      external_import_css.as_bytes().to_vec(),
      Some("text/css".to_string()),
    ),
  );

  let fetcher = Arc::new(RecordingFetcher::new(fetch_map));
  let fetcher_for_renderer: Arc<dyn ResourceFetcher> = fetcher.clone();
  let runtime_toggles = RuntimeToggles::from_map(HashMap::new());

  let renderer = FastRender::builder()
    .base_url(document_url)
    .fetcher(fetcher_for_renderer)
    .runtime_toggles(runtime_toggles)
    .build()
    .expect("renderer should build");

  let html = format!(
    r#"
      <!doctype html>
      <html>
        <head>
          <style>{inline_css}</style>
          <link rel="stylesheet" href="external.css">
        </head>
        <body></body>
      </html>
    "#
  );
  let dom = renderer.parse_html(&html).expect("parse html");

  let media_ctx = MediaContext::screen(800.0, 600.0);
  let mut pipeline_cache = MediaQueryCache::default();
  let style_set = renderer
    .collect_document_style_set(&dom, &media_ctx, &mut pipeline_cache, None)
    .expect("collect document style set");

  let mut loader_map = HashMap::new();
  loader_map.insert(inline_a_url.to_string(), inline_a_css.to_string());
  loader_map.insert(inline_b_url.to_string(), inline_b_css.to_string());
  loader_map.insert(
    external_import_url.to_string(),
    external_import_css.to_string(),
  );
  let loader = MapLoader::new(loader_map);

  let mut expected_cache = MediaQueryCache::default();
  let inline_sheet =
    parse_stylesheet_with_media(inline_css, &media_ctx, Some(&mut expected_cache))
      .expect("parse inline sheet");
  let resolved_inline = if inline_sheet.contains_imports() {
    inline_sheet
      .resolve_imports_with_cache(
        &loader,
        Some(document_url),
        &media_ctx,
        Some(&mut expected_cache),
      )
      .expect("resolve inline imports")
  } else {
    inline_sheet
  };

  let external_sheet =
    parse_stylesheet_with_media(external_css, &media_ctx, Some(&mut expected_cache))
      .expect("parse external sheet");
  let resolved_external = if external_sheet.contains_imports() {
    external_sheet
      .resolve_imports_with_cache(
        &loader,
        Some(external_url),
        &media_ctx,
        Some(&mut expected_cache),
      )
      .expect("resolve external imports")
  } else {
    external_sheet
  };

  let mut expected_rules = Vec::new();
  expected_rules.extend(resolved_inline.rules);
  expected_rules.extend(resolved_external.rules);
  let expected = StyleSheet {
    rules: expected_rules,
  };

  assert_eq!(format!("{:?}", style_set.document), format!("{expected:?}"));

  let requests = fetcher.requests();
  assert!(
    requests.contains(&inline_a_url.to_string()),
    "expected inline import to be fetched; got {requests:?}"
  );
  assert!(
    requests.contains(&inline_b_url.to_string()),
    "expected inline import chain to be fetched; got {requests:?}"
  );
  assert!(
    requests.contains(&external_url.to_string()),
    "expected external stylesheet to be fetched; got {requests:?}"
  );
  assert!(
    requests.contains(&external_import_url.to_string()),
    "expected external import to be fetched; got {requests:?}"
  );
  assert!(
    !requests.contains(&skipped_print_url.to_string()),
    "media-gated @import should not be fetched; got {requests:?}"
  );
  assert!(
    !requests.contains(&skipped_supports_url.to_string()),
    "supports-gated @import should not be fetched; got {requests:?}"
  );
}
