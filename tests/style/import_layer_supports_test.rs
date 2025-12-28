use fastrender::css::parser::parse_stylesheet;
use fastrender::css::types::CssImportLoader;
use fastrender::dom;
use fastrender::style::cascade::{apply_styles_with_media, StyledNode};
use fastrender::style::display::Display;
use fastrender::style::media::MediaContext;
use fastrender::Rgba;
use std::collections::HashMap;
use std::io;

const BASE_URL: &str = "https://example.com/main.css";

struct MapImportLoader {
  styles: HashMap<String, String>,
}

impl MapImportLoader {
  fn new() -> Self {
    Self {
      styles: HashMap::new(),
    }
  }

  fn with(mut self, url: &str, css: &str) -> Self {
    self.styles.insert(url.to_string(), css.to_string());
    self
  }
}

impl CssImportLoader for MapImportLoader {
  fn load(&self, url: &str) -> fastrender::error::Result<String> {
    self
      .styles
      .get(url)
      .cloned()
      .ok_or(fastrender::error::Error::Io(io::Error::new(
        io::ErrorKind::NotFound,
        format!("missing import {url}"),
      )))
  }
}

fn find_by_id<'a>(node: &'a StyledNode, id: &str) -> Option<&'a StyledNode> {
  if node
    .node
    .get_attribute_ref("id")
    .map(|value| value == id)
    .unwrap_or(false)
  {
    return Some(node);
  }
  for child in &node.children {
    if let Some(found) = find_by_id(child, id) {
      return Some(found);
    }
  }
  None
}

fn styled_target(css: &str, loader: &MapImportLoader) -> StyledNode {
  let media = MediaContext::screen(800.0, 600.0);
  let stylesheet = parse_stylesheet(css).unwrap();
  let resolved = stylesheet.resolve_imports(loader, Some(BASE_URL), &media);
  let dom = dom::parse_html(r#"<div id="t"></div>"#).unwrap();
  let styled = apply_styles_with_media(&dom, &resolved, &media);
  find_by_id(&styled, "t")
    .expect("target element should be styled")
    .clone()
}

#[test]
fn supports_modifier_allows_import_when_true() {
  let loader = MapImportLoader::new().with("https://example.com/a.css", "#t { display: inline; }");
  let target = styled_target(
    r#"
      @import "a.css" supports((display: grid));
    "#,
    &loader,
  );

  assert_eq!(target.styles.display, Display::Inline);
}

#[test]
fn supports_modifier_skips_import_when_false() {
  let loader = MapImportLoader::new().with("https://example.com/a.css", "#t { display: inline; }");
  let target = styled_target(
    r#"
      @import "a.css" supports((display: definitely-not-a-display));
    "#,
    &loader,
  );

  assert_eq!(target.styles.display, Display::Block);
}

#[test]
fn import_layer_participates_in_layer_ordering() {
  let loader =
    MapImportLoader::new().with("https://example.com/a.css", "#t { color: rgb(1, 2, 3); }");
  let target = styled_target(
    r#"
      @import "a.css" layer(a);
      @layer b { #t { color: rgb(9, 9, 9); } }
    "#,
    &loader,
  );

  assert_eq!(target.styles.color, Rgba::rgb(9, 9, 9));
}

#[test]
fn import_modifiers_are_order_insensitive() {
  let loader =
    MapImportLoader::new().with("https://example.com/a.css", "#t { color: rgb(1, 2, 3); }");
  let target = styled_target(
    r#"
      @import "a.css" supports((display: grid)) layer(a);
      @layer b { #t { color: rgb(9, 9, 9); } }
    "#,
    &loader,
  );

  assert_eq!(target.styles.color, Rgba::rgb(9, 9, 9));
}
