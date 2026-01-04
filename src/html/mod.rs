//! HTML-specific helpers (encoding, parsing utilities)

pub mod asset_discovery;
pub mod encoding;
pub mod image_attrs;
pub mod image_prefetch;
pub mod images;
pub mod meta_refresh;
pub mod viewport;

use crate::css::loader::resolve_href;
use crate::dom::{DomNode, DomNodeType, HTML_NAMESPACE};
use memchr::memchr;
use std::borrow::Cow;
use url::Url;

fn is_tag_name_char(b: u8) -> bool {
  b.is_ascii_alphanumeric() || b == b'-' || b == b':'
}

fn find_tag_end(bytes: &[u8], start: usize) -> usize {
  let mut quote: Option<u8> = None;
  let mut i = start + 1;
  while i < bytes.len() {
    let b = bytes[i];
    match quote {
      Some(q) => {
        if b == q {
          quote = None;
        }
      }
      None => {
        if b == b'"' || b == b'\'' {
          quote = Some(b);
        } else if b == b'>' {
          return i + 1;
        }
      }
    }
    i += 1;
  }
  bytes.len()
}

fn parse_tag_name_range(bytes: &[u8], start: usize, end: usize) -> Option<(bool, usize, usize)> {
  if bytes.get(start)? != &b'<' {
    return None;
  }
  let mut i = start + 1;
  while i < end && bytes[i].is_ascii_whitespace() {
    i += 1;
  }
  if i >= end {
    return None;
  }
  let mut is_end = false;
  if bytes[i] == b'/' {
    is_end = true;
    i += 1;
    while i < end && bytes[i].is_ascii_whitespace() {
      i += 1;
    }
  }
  let name_start = i;
  while i < end && is_tag_name_char(bytes[i]) {
    i += 1;
  }
  if i == name_start {
    return None;
  }
  Some((is_end, name_start, i))
}

fn find_bytes(bytes: &[u8], start: usize, needle: &[u8]) -> Option<usize> {
  if needle.is_empty() {
    return Some(start);
  }
  let first = needle[0];
  let mut idx = start;
  while let Some(rel) = memchr(first, &bytes[idx..]) {
    let pos = idx + rel;
    if pos + needle.len() <= bytes.len() && bytes[pos..pos + needle.len()] == *needle {
      return Some(pos);
    }
    idx = pos + 1;
  }
  None
}

fn find_raw_text_element_end(bytes: &[u8], start: usize, tag: &'static [u8]) -> usize {
  let mut idx = start;
  while let Some(rel) = memchr(b'<', &bytes[idx..]) {
    let pos = idx + rel;
    if bytes.get(pos + 1) == Some(&b'/') {
      let name_start = pos + 2;
      let name_end = name_start + tag.len();
      if name_end <= bytes.len()
        && bytes[name_start..name_end].eq_ignore_ascii_case(tag)
        && !bytes
          .get(name_end)
          .map(|b| is_tag_name_char(*b))
          .unwrap_or(false)
      {
        return find_tag_end(bytes, pos);
      }
    }
    idx = pos + 1;
  }
  bytes.len()
}

/// Return HTML source with the contents of `<template>` elements removed.
///
/// This is used by best-effort HTML scanners (meta refresh / asset discovery) to ensure the inert
/// template subtrees cannot influence behavior such as subresource prefetching or redirect
/// inference.
pub(crate) fn strip_template_contents(html: &str) -> Cow<'_, str> {
  let bytes = html.as_bytes();
  let mut template_depth: usize = 0;
  let mut copy_from: usize = 0;
  let mut out: Option<String> = None;
  let mut i: usize = 0;

  while let Some(rel) = memchr(b'<', &bytes[i..]) {
    let tag_start = i + rel;

    if bytes
      .get(tag_start..tag_start + 4)
      .is_some_and(|head| head == b"<!--")
    {
      let end = find_bytes(bytes, tag_start + 4, b"-->")
        .map(|pos| pos + 3)
        .unwrap_or(bytes.len());
      i = end;
      continue;
    }

    if bytes
      .get(tag_start..tag_start + 9)
      .is_some_and(|head| head.eq_ignore_ascii_case(b"<![cdata["))
    {
      let end = find_bytes(bytes, tag_start + 9, b"]]>")
        .map(|pos| pos + 3)
        .unwrap_or(bytes.len());
      i = end;
      continue;
    }

    // Markup declarations / processing instructions (`<!doctype ...>`, `<?xml ...?>`, etc.)
    if bytes.get(tag_start + 1).is_some_and(|b| *b == b'!' || *b == b'?') {
      i = find_tag_end(bytes, tag_start);
      continue;
    }

    let tag_end = find_tag_end(bytes, tag_start);
    if tag_end == bytes.len() {
      break;
    }

    let Some((is_end, name_start, name_end)) = parse_tag_name_range(bytes, tag_start, tag_end) else {
      i = tag_start + 1;
      continue;
    };
    let name = &bytes[name_start..name_end];

    let raw_text_tag: Option<&'static [u8]> = if !is_end && name.eq_ignore_ascii_case(b"script") {
      Some(b"script")
    } else if !is_end && name.eq_ignore_ascii_case(b"style") {
      Some(b"style")
    } else if !is_end && name.eq_ignore_ascii_case(b"textarea") {
      Some(b"textarea")
    } else if !is_end && name.eq_ignore_ascii_case(b"title") {
      Some(b"title")
    } else if !is_end && name.eq_ignore_ascii_case(b"xmp") {
      Some(b"xmp")
    } else {
      None
    };

    if !is_end && name.eq_ignore_ascii_case(b"plaintext") {
      // `<plaintext>` consumes the remainder of the document as text; stop scanning to avoid
      // treating anything following it as markup.
      break;
    }

    if let Some(tag) = raw_text_tag {
      i = find_raw_text_element_end(bytes, tag_end, tag);
      continue;
    }

    if name.eq_ignore_ascii_case(b"template") {
      if is_end {
        if template_depth > 0 {
          template_depth -= 1;
          if template_depth == 0 {
            let buf = out.as_mut().expect("closing template without output buffer");
            buf.push_str(&html[tag_start..tag_end]);
            copy_from = tag_end;
          }
        }
      } else if template_depth == 0 {
        let buf = out.get_or_insert_with(|| String::with_capacity(html.len()));
        buf.push_str(&html[copy_from..tag_start]);
        buf.push_str(&html[tag_start..tag_end]);
        copy_from = tag_end;
        template_depth = 1;
      } else {
        template_depth += 1;
      }
    }

    i = tag_end;
  }

  match out {
    None => Cow::Borrowed(html),
    Some(mut buf) => {
      if template_depth == 0 {
        buf.push_str(&html[copy_from..]);
      }
      Cow::Owned(buf)
    }
  }
}

/// Find the first `<base href>` value within the document `<head>`.
///
/// Returns `None` when no `<base>` is present or the element is missing an
/// `href` attribute.
pub fn find_base_href(dom: &DomNode) -> Option<String> {
  fn find_head(node: &DomNode) -> Option<&DomNode> {
    if matches!(node.node_type, DomNodeType::ShadowRoot { .. }) {
      return None;
    }
    if node.is_template_element() {
      return None;
    }
    if let Some(tag) = node.tag_name() {
      if tag.eq_ignore_ascii_case("head")
        && matches!(node.namespace(), Some(ns) if ns.is_empty() || ns == HTML_NAMESPACE)
      {
        return Some(node);
      }
    }
    for child in node.traversal_children() {
      if let Some(head) = find_head(child) {
        return Some(head);
      }
    }
    None
  }

  fn find_base(node: &DomNode, in_foreign_namespace: bool) -> Option<String> {
    if matches!(node.node_type, DomNodeType::ShadowRoot { .. }) {
      return None;
    }
    if node.is_template_element() {
      return None;
    }
    let next_in_foreign_namespace = in_foreign_namespace
      || matches!(
        node.namespace(),
        Some(ns) if !(ns.is_empty() || ns == HTML_NAMESPACE)
      );
    if let Some(tag) = node.tag_name() {
      if !in_foreign_namespace
        && tag.eq_ignore_ascii_case("base")
        && matches!(node.namespace(), Some(ns) if ns.is_empty() || ns == HTML_NAMESPACE)
      {
        if let Some(href) = node.get_attribute_ref("href") {
          let trimmed = href.trim();
          if !trimmed.is_empty() && !trimmed.starts_with('#') {
            return Some(trimmed.to_string());
          }
        }
      }
    }
    for child in node.traversal_children() {
      if let Some(found) = find_base(child, next_in_foreign_namespace) {
        return Some(found);
      }
    }
    None
  }

  let head = find_head(dom)?;
  if let Some(from_head) = find_base(head, false) {
    return Some(from_head);
  }
  find_base(dom, false)
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
  fn find_base_href_ignores_base_like_text_in_scripts() {
    let dom = parse_html(
      "<html><head><script>var s='<base href=\"https://bad.example/\">'</script></head></html>",
    )
    .unwrap();

    assert_eq!(find_base_href(&dom), None);
  }

  #[test]
  fn find_base_href_ignores_template_only_base() {
    let dom = parse_html(
      "<html><head><template><base href=\"https://bad.example/\"></template></head></html>",
    )
    .unwrap();

    assert_eq!(find_base_href(&dom), None);
  }

  #[test]
  fn find_base_href_ignores_shadow_roots_in_document_body() {
    let dom = parse_html(
      "<html><head><base href=\"https://good.example/\"></head><body>
        <div id=\"host\"><template shadowroot=\"open\"><base href=\"https://bad.example/\"></template></div>
      </body></html>",
    )
    .unwrap();

    assert_eq!(
      find_base_href(&dom),
      Some("https://good.example/".to_string())
    );
  }

  #[test]
  fn find_base_href_ignores_shadow_root_only_base() {
    let dom = parse_html(
      "<html><head></head><body><div id=\"host\"><template shadowroot=\"open\"><base href=\"https://bad.example/\"></template></div></body></html>",
    )
    .unwrap();

    assert_eq!(find_base_href(&dom), None);
  }

  #[test]
  fn find_base_href_ignores_base_inside_svg() {
    let dom = parse_html(
      "<html><head><base href=\"https://good.example/\"></head><body><svg><base href=\"https://bad.example/\"></base></svg></body></html>",
    )
    .unwrap();

    assert_eq!(
      find_base_href(&dom),
      Some("https://good.example/".to_string())
    );
  }

  #[test]
  fn find_base_href_ignores_base_inside_foreignobject_outside_head() {
    let dom = parse_html(
      "<html><head></head><body><svg><foreignObject><base href=\"https://bad.example/\"></base></foreignObject></svg></body></html>",
    )
    .unwrap();

    assert_eq!(find_base_href(&dom), None);
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
