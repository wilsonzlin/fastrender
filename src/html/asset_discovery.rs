//! Best-effort HTML subresource discovery for prefetching/crawling tools.
//!
//! This module intentionally uses regex-based extraction instead of a full HTML parser.
//! It is designed for developer tooling (e.g. `prefetch_assets`, `bundle_page --no-render`)
//! where "good enough" coverage is preferable to strict spec parsing.

use crate::css::loader::resolve_href;
use regex::Regex;
use std::collections::HashSet;
use std::sync::OnceLock;

/// URLs discovered from an HTML document that are likely to be fetched during paint.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct HtmlAssetUrls {
  /// Image-like assets (img/src/srcset/source/srcset, video posters, icons/manifests).
  pub images: Vec<String>,
  /// Embedded documents (iframes, objects, embeds).
  pub documents: Vec<String>,
}

const MAX_SRCSET_CANDIDATES: usize = 16;

// Keep discovery bounded so pathological HTML doesn't explode memory usage.
const MAX_DISCOVERED_IMAGES: usize = 4096;
const MAX_DISCOVERED_DOCUMENTS: usize = 1024;

fn regex(pattern: &'static str, desc: &'static str) -> Regex {
  Regex::new(pattern).unwrap_or_else(|err| panic!("invalid {desc} regex: {err}"))
}

fn capture_first<'t>(caps: &regex::Captures<'t>, groups: &[usize]) -> Option<&'t str> {
  groups
    .iter()
    .find_map(|idx| caps.get(*idx).map(|m| m.as_str()))
}

fn parse_srcset_urls(srcset: &str, max_candidates: usize) -> Vec<&str> {
  let mut out = Vec::new();
  for candidate in srcset.split(',') {
    if out.len() >= max_candidates {
      break;
    }
    let trimmed = candidate.trim();
    if trimmed.is_empty() {
      continue;
    }
    let url_part = trimmed.split_whitespace().next().unwrap_or("").trim();
    if url_part.is_empty() {
      continue;
    }
    out.push(url_part);
  }
  out
}

fn link_rel_is_icon_or_manifest(rel_value: &str) -> bool {
  rel_value.split_whitespace().any(|token| {
    matches!(
      token.to_ascii_lowercase().as_str(),
      "icon" | "apple-touch-icon" | "apple-touch-icon-precomposed" | "manifest"
    )
  })
}

/// Best-effort extraction of subresource URLs from raw HTML.
///
/// All returned URLs are resolved against `base_url` using [`resolve_href`]. Discovery is
/// deterministic (input-order, first occurrence wins) and bounded (per-category caps and
/// `srcset` candidate limits).
pub fn discover_html_asset_urls(html: &str, base_url: &str) -> HtmlAssetUrls {
  static IMG_SRC: OnceLock<Regex> = OnceLock::new();
  static IMG_SRCSET: OnceLock<Regex> = OnceLock::new();
  static SOURCE_SRCSET: OnceLock<Regex> = OnceLock::new();
  static VIDEO_POSTER: OnceLock<Regex> = OnceLock::new();
  static IFRAME_SRC: OnceLock<Regex> = OnceLock::new();
  static OBJECT_DATA: OnceLock<Regex> = OnceLock::new();
  static EMBED_SRC: OnceLock<Regex> = OnceLock::new();
  static LINK_TAG: OnceLock<Regex> = OnceLock::new();
  static ATTR_REL: OnceLock<Regex> = OnceLock::new();
  static ATTR_HREF: OnceLock<Regex> = OnceLock::new();

  let img_src = IMG_SRC.get_or_init(|| {
    regex(
      "(?is)<img[^>]*\\ssrc\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)'|([^\\s>]+))",
      "img src",
    )
  });
  let img_srcset = IMG_SRCSET.get_or_init(|| {
    regex(
      "(?is)<img[^>]*\\ssrcset\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)')",
      "img srcset",
    )
  });
  let source_srcset = SOURCE_SRCSET.get_or_init(|| {
    regex(
      "(?is)<source[^>]*\\ssrcset\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)')",
      "source srcset",
    )
  });
  let video_poster = VIDEO_POSTER.get_or_init(|| {
    regex(
      "(?is)<video[^>]*\\sposter\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)'|([^\\s>]+))",
      "video poster",
    )
  });
  let iframe_src = IFRAME_SRC.get_or_init(|| {
    regex(
      "(?is)<iframe[^>]*\\ssrc\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)'|([^\\s>]+))",
      "iframe src",
    )
  });
  let object_data = OBJECT_DATA.get_or_init(|| {
    regex(
      "(?is)<object[^>]*\\sdata\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)'|([^\\s>]+))",
      "object data",
    )
  });
  let embed_src = EMBED_SRC.get_or_init(|| {
    regex(
      "(?is)<embed[^>]*\\ssrc\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)'|([^\\s>]+))",
      "embed src",
    )
  });
  let link_tag = LINK_TAG.get_or_init(|| regex("(?is)<link\\b[^>]*>", "link tag"));
  let attr_rel = ATTR_REL.get_or_init(|| {
    regex(
      "(?is)(?:^|\\s)rel\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)'|([^\\s>]+))",
      "rel attr",
    )
  });
  let attr_href = ATTR_HREF.get_or_init(|| {
    regex(
      "(?is)(?:^|\\s)href\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)'|([^\\s>]+))",
      "href attr",
    )
  });

  let mut out = HtmlAssetUrls::default();
  let mut seen_images: HashSet<String> = HashSet::new();
  let mut seen_documents: HashSet<String> = HashSet::new();

  let mut push_image = |raw: &str| {
    if out.images.len() >= MAX_DISCOVERED_IMAGES {
      return;
    }
    if let Some(resolved) = resolve_href(base_url, raw) {
      if seen_images.insert(resolved.clone()) {
        out.images.push(resolved);
      }
    }
  };
  let mut push_document = |raw: &str| {
    if out.documents.len() >= MAX_DISCOVERED_DOCUMENTS {
      return;
    }
    if let Some(resolved) = resolve_href(base_url, raw) {
      if seen_documents.insert(resolved.clone()) {
        out.documents.push(resolved);
      }
    }
  };

  for caps in img_src.captures_iter(html) {
    if let Some(raw) = capture_first(&caps, &[1, 2, 3]) {
      push_image(raw);
    }
  }

  for caps in img_srcset.captures_iter(html) {
    let Some(raw_srcset) = capture_first(&caps, &[1, 2]) else {
      continue;
    };
    for candidate in parse_srcset_urls(raw_srcset, MAX_SRCSET_CANDIDATES) {
      push_image(candidate);
    }
  }

  for caps in source_srcset.captures_iter(html) {
    let Some(raw_srcset) = capture_first(&caps, &[1, 2]) else {
      continue;
    };
    for candidate in parse_srcset_urls(raw_srcset, MAX_SRCSET_CANDIDATES) {
      push_image(candidate);
    }
  }

  for caps in video_poster.captures_iter(html) {
    if let Some(raw) = capture_first(&caps, &[1, 2, 3]) {
      push_image(raw);
    }
  }

  for caps in iframe_src.captures_iter(html) {
    if let Some(raw) = capture_first(&caps, &[1, 2, 3]) {
      push_document(raw);
    }
  }

  for caps in object_data.captures_iter(html) {
    if let Some(raw) = capture_first(&caps, &[1, 2, 3]) {
      push_document(raw);
    }
  }

  for caps in embed_src.captures_iter(html) {
    if let Some(raw) = capture_first(&caps, &[1, 2, 3]) {
      push_document(raw);
    }
  }

  for caps in link_tag.captures_iter(html) {
    let tag = caps.get(0).map(|m| m.as_str()).unwrap_or("");
    if tag.is_empty() {
      continue;
    }
    let rel = attr_rel
      .captures(tag)
      .and_then(|c| capture_first(&c, &[1, 2, 3]))
      .unwrap_or("");
    if rel.is_empty() || !link_rel_is_icon_or_manifest(rel) {
      continue;
    }
    let href = attr_href
      .captures(tag)
      .and_then(|c| capture_first(&c, &[1, 2, 3]))
      .unwrap_or("");
    if href.is_empty() {
      continue;
    }
    push_image(href);
  }

  out
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn discovers_img_sources_and_srcset_candidates() {
    let html = r#"
      <img src="img.png">
      <img srcset="a1.png 1x, a2.png 2x">
      <picture><source srcset="s1.png 1x, s2.png 2x"></picture>
    "#;
    let out = discover_html_asset_urls(html, "https://example.com/dir/page.html");
    assert_eq!(
      out.images,
      vec![
        "https://example.com/dir/img.png",
        "https://example.com/dir/a1.png",
        "https://example.com/dir/a2.png",
        "https://example.com/dir/s1.png",
        "https://example.com/dir/s2.png",
      ]
      .into_iter()
      .map(str::to_string)
      .collect::<Vec<_>>()
    );
  }

  #[test]
  fn caps_srcset_candidates() {
    let srcset = (0..20)
      .map(|i| format!("img{i}.png {i}x"))
      .collect::<Vec<_>>()
      .join(", ");
    let html = format!(r#"<img srcset="{srcset}">"#);
    let out = discover_html_asset_urls(&html, "https://example.com/");
    assert_eq!(out.images.len(), MAX_SRCSET_CANDIDATES);
    assert_eq!(out.images[0], "https://example.com/img0.png");
    assert_eq!(
      out.images[MAX_SRCSET_CANDIDATES - 1],
      "https://example.com/img15.png"
    );
  }

  #[test]
  fn discovers_iframe_object_embed_documents() {
    let html = r#"
      <iframe src="frame.html"></iframe>
      <object data="/obj.html"></object>
      <embed src='embed.html'>
    "#;
    let out = discover_html_asset_urls(html, "https://example.com/base/");
    assert_eq!(
      out.documents,
      vec![
        "https://example.com/base/frame.html",
        "https://example.com/obj.html",
        "https://example.com/base/embed.html",
      ]
      .into_iter()
      .map(str::to_string)
      .collect::<Vec<_>>()
    );
  }

  #[test]
  fn discovers_video_posters() {
    let html = r#"<video poster="/poster.jpg"></video>"#;
    let out = discover_html_asset_urls(html, "https://example.com/page.html");
    assert_eq!(
      out.images,
      vec!["https://example.com/poster.jpg".to_string()]
    );
  }

  #[test]
  fn discovers_icons_and_manifest_link_rel() {
    let html = r#"
      <link rel="stylesheet" href="style.css">
      <link rel="icon" href="favicon.ico">
      <link rel="apple-touch-icon" href="/touch.png">
      <link href="favicon2.ico" rel="shortcut icon">
      <link href="manifest.json" rel="manifest">
    "#;
    let out = discover_html_asset_urls(html, "https://example.com/base/page.html");
    assert_eq!(
      out.images,
      vec![
        "https://example.com/base/favicon.ico",
        "https://example.com/touch.png",
        "https://example.com/base/favicon2.ico",
        "https://example.com/base/manifest.json",
      ]
      .into_iter()
      .map(str::to_string)
      .collect::<Vec<_>>()
    );
  }

  #[test]
  fn does_not_match_data_attributes() {
    let html = r#"
      <img data-src="lazy.png">
      <iframe data-src="frame.html"></iframe>
      <link data-href="bad.ico" rel="icon" href="good.ico">
    "#;
    let out = discover_html_asset_urls(html, "https://example.com/base/");
    assert_eq!(
      out.images,
      vec!["https://example.com/base/good.ico".to_string()]
    );
    assert!(
      out.documents.is_empty(),
      "data-src iframe should not be discovered"
    );
  }
}
