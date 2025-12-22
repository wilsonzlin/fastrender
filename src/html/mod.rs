//! HTML-specific helpers (encoding, parsing utilities)

pub mod encoding;
pub mod meta_refresh;

use crate::css::loader::resolve_href;
use crate::dom::DomNode;
use url::Url;

/// Find the first `<base href>` value within the document `<head>`.
///
/// Returns `None` when no `<base>` is present or the element is missing an
/// `href` attribute.
pub fn find_base_href(dom: &DomNode) -> Option<String> {
  let mut stack = vec![dom];
  let mut head: Option<&DomNode> = None;

  while let Some(node) = stack.pop() {
    if let Some(tag) = node.tag_name() {
      if tag.eq_ignore_ascii_case("head") {
        head = Some(node);
        break;
      }
    }
    for child in node.children.iter().rev() {
      stack.push(child);
    }
  }

  let Some(head) = head else {
    return None;
  };

  let mut stack = vec![head];
  while let Some(node) = stack.pop() {
    if let Some(tag) = node.tag_name() {
      if tag.eq_ignore_ascii_case("base") {
        if let Some(href) = node.get_attribute_ref("href") {
          let trimmed = href.trim();
          if !trimmed.is_empty() && !trimmed.starts_with('#') {
            return Some(trimmed.to_string());
          }
        }
      }
    }

    for child in node.children.iter().rev() {
      stack.push(child);
    }
  }

  None
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
