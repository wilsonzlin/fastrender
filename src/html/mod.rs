//! HTML-specific helpers (encoding, parsing utilities)

pub mod encoding;
pub mod asset_discovery;
pub mod images;
pub mod meta_refresh;
pub mod viewport;

use crate::css::loader::resolve_href;
use crate::dom::{DomNode, DomNodeType};
use url::Url;

/// Find the first `<base href>` value within the document `<head>`.
///
/// Returns `None` when no `<base>` is present or the element is missing an
/// `href` attribute.
pub fn find_base_href(dom: &DomNode) -> Option<String> {
  fn find_head(node: &DomNode) -> Option<&DomNode> {
    if matches!(node.node_type, DomNodeType::ShadowRoot { .. }) {
      return None;
    }
    if let Some(tag) = node.tag_name() {
      if tag.eq_ignore_ascii_case("head") {
        return Some(node);
      }
      if tag.eq_ignore_ascii_case("template") {
        return None;
      }
    }
    for child in node.children.iter() {
      if let Some(head) = find_head(child) {
        return Some(head);
      }
    }
    None
  }

  fn find_base(node: &DomNode) -> Option<String> {
    if matches!(node.node_type, DomNodeType::ShadowRoot { .. }) {
      return None;
    }
    if let Some(tag) = node.tag_name() {
      if tag.eq_ignore_ascii_case("template") {
        return None;
      }
      if tag.eq_ignore_ascii_case("base") {
        if let Some(href) = node.get_attribute_ref("href") {
          let trimmed = href.trim();
          if !trimmed.is_empty() && !trimmed.starts_with('#') {
            return Some(trimmed.to_string());
          }
        }
      }
    }
    for child in node.children.iter() {
      if let Some(found) = find_base(child) {
        return Some(found);
      }
    }
    None
  }

  let head = find_head(dom)?;
  if let Some(from_head) = find_base(head) {
    return Some(from_head);
  }
  find_base(dom)
}

/// Compute the document base URL given the parsed DOM and an optional
/// document URL.
///
/// When a `<base href>` is present in the head, it is resolved against the
/// provided `document_url` and returned. Invalid or fragment-only base values
/// are ignored. When no `<base>` is present or resolution fails, the document
/// URL itself is used as the base if provided.
pub fn document_base_url(dom: &DomNode, document_url: Option<&str>) -> Option<String> {
  if let Some(href) = find_base_href(dom) {
    if let Some(doc_url) = document_url {
      if let Some(resolved) = resolve_href(doc_url, &href) {
        return Some(resolved);
      }
    } else if let Ok(abs) = Url::parse(&href) {
      return Some(abs.to_string());
    }
  }

  document_url.map(|url| url.to_string())
}

#[cfg(test)]
mod tests {
  use super::{document_base_url, find_base_href};
  use crate::dom::parse_html;

  #[test]
  fn finds_first_base_href_in_head_document_order() {
    let dom = parse_html(
      "<html><head><base href=\"https://first.example/\"><base href=\"https://second.example/\"></head><body></body></html>",
    )
    .unwrap();

    assert_eq!(
      find_base_href(&dom),
      Some("https://first.example/".to_string())
    );
  }

  #[test]
  fn find_base_href_skips_missing_or_empty_values() {
    let dom = parse_html(
      "<html><head><base><base href=\"   \" /><base href=\"https://valid.example/\"></head></html>",
    )
    .unwrap();

    assert_eq!(
      find_base_href(&dom),
      Some("https://valid.example/".to_string())
    );
  }

  #[test]
  fn find_base_href_ignores_fragment_only_href() {
    let dom = parse_html(
      "<html><head><base href=\"#frag\"><base href=\"https://example.com/base/\"></head></html>",
    )
    .unwrap();

    assert_eq!(
      find_base_href(&dom),
      Some("https://example.com/base/".to_string())
    );
  }

  #[test]
  fn find_base_href_ignores_template_contents() {
    let dom = parse_html(
      "<html><head><template><base href=\"https://bad.example/\"></template><base href=\"https://good.example/\"></head></html>",
    )
    .unwrap();

    assert_eq!(
      find_base_href(&dom),
      Some("https://good.example/".to_string())
    );
  }

  #[test]
  fn find_base_href_ignores_shadow_roots_in_head() {
    let dom = parse_html(
      "<html><head>
        <div id=\"host\"><template shadowroot=\"open\"><base href=\"https://bad.example/\"></template></div>
        <base href=\"https://good.example/\">
      </head><body></body></html>",
    )
    .unwrap();

    assert_eq!(
      find_base_href(&dom),
      Some("https://good.example/".to_string())
    );
  }

  #[test]
  fn document_base_url_resolves_relative_base_against_document_url() {
    let dom = parse_html("<html><head><base href=\"assets/\"></head></html>").unwrap();
    let base = document_base_url(&dom, Some("https://example.com/dir/page.html"));

    assert_eq!(base, Some("https://example.com/dir/assets/".to_string()));
  }

  #[test]
  fn document_base_url_uses_absolute_base_without_document_url() {
    let dom =
      parse_html("<html><head><base href=\"https://cdn.example.com/static/\"></head></html>")
        .unwrap();

    assert_eq!(
      document_base_url(&dom, None),
      Some("https://cdn.example.com/static/".to_string())
    );
  }
}
