use std::collections::HashMap;
use std::io;
use std::sync::atomic::{AtomicUsize, Ordering};

use fastrender::css::parser::parse_stylesheet;
use fastrender::css::types::CssImportLoader;
use fastrender::style::media::MediaContext;

#[derive(Default)]
struct CountingLoader {
  calls: AtomicUsize,
}

impl CountingLoader {
  fn calls(&self) -> usize {
    self.calls.load(Ordering::Relaxed)
  }
}

impl CssImportLoader for CountingLoader {
  fn load(&self, url: &str) -> fastrender::error::Result<String> {
    self.calls.fetch_add(1, Ordering::Relaxed);
    Err(fastrender::error::Error::Io(io::Error::new(
      io::ErrorKind::NotFound,
      format!("unexpected import load for {url}"),
    )))
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
  fn load(&self, url: &str) -> fastrender::error::Result<String> {
    self
      .map
      .get(url)
      .cloned()
      .ok_or_else(|| fastrender::error::Error::Io(io::Error::new(io::ErrorKind::NotFound, url)))
  }
}

#[test]
fn resolve_imports_owned_is_identity_without_imports() {
  let css = r#"
    body { color: red; }
    @layer base { .layered { color: blue; } }
  "#;
  let sheet = parse_stylesheet(css).expect("parse stylesheet");
  let expected = format!("{sheet:?}");
  let loader = CountingLoader::default();
  let media_ctx = MediaContext::screen(800.0, 600.0);

  let resolved = sheet
    .clone()
    .resolve_imports_owned_with_cache(
      &loader,
      Some("https://example.com/style.css"),
      &media_ctx,
      None,
    )
    .expect("resolve imports");

  assert_eq!(format!("{resolved:?}"), expected);
  assert_eq!(loader.calls(), 0, "import loader should not run");
}

#[test]
fn resolve_imports_owned_matches_borrowed_resolver() {
  let base_url = "https://example.com/dir/base.css";
  let base_css = r#"
    @layer base {
      @import "a.css";
      .in-layer { color: black; }
    }
    body { color: red; }
  "#;
  let a_css = r#"
    @supports (display: block) {
      @import "b.css";
      .a { color: green; }
    }
  "#;
  let b_css = ".b { color: blue; }";
  let mut map = HashMap::new();
  map.insert(
    "https://example.com/dir/a.css".to_string(),
    a_css.to_string(),
  );
  map.insert(
    "https://example.com/dir/b.css".to_string(),
    b_css.to_string(),
  );
  let loader = MapLoader::new(map);
  let media_ctx = MediaContext::screen(800.0, 600.0);

  let sheet = parse_stylesheet(base_css).expect("parse base stylesheet");
  let borrowed = sheet
    .resolve_imports_with_cache(&loader, Some(base_url), &media_ctx, None)
    .expect("resolve imports (borrowed)");
  let owned = sheet
    .clone()
    .resolve_imports_owned_with_cache(&loader, Some(base_url), &media_ctx, None)
    .expect("resolve imports (owned)");

  assert_eq!(format!("{owned:?}"), format!("{borrowed:?}"));
}
