//! Best-effort HTML image URL discovery for cache warming.
//!
//! This module is used by developer tooling (e.g. `prefetch_assets`) to find
//! image URLs in HTML without executing layout. Selection is aligned with the
//! renderer's responsive image algorithm (`srcset`/`sizes`/`picture`) so the
//! warmed cache matches what paint would request.

use crate::css::loader::resolve_href_with_base;
use crate::css::parser::tokenize_rel_list;
use crate::dom::{DomNode, HTML_NAMESPACE};
use crate::html::image_attrs::{parse_sizes, parse_srcset};
use crate::html::images::{image_sources_with_fallback, ImageSelectionContext};
use crate::style::media::MediaQuery;
use crate::tree::box_tree::PictureSource;
use std::collections::HashSet;

/// Hard limits for image prefetch discovery.
#[derive(Debug, Clone, Copy)]
pub struct ImagePrefetchLimits {
  /// Maximum number of image-like elements to consider per document.
  pub max_image_elements: usize,
  /// Maximum number of URLs to emit per image element (primary + fallbacks).
  pub max_urls_per_element: usize,
}

impl Default for ImagePrefetchLimits {
  fn default() -> Self {
    Self {
      max_image_elements: 150,
      max_urls_per_element: 2,
    }
  }
}

/// Result of HTML image prefetch discovery.
#[derive(Debug, Clone)]
pub struct ImagePrefetchDiscovery {
  /// Number of image elements walked (bounded by `max_image_elements`).
  pub image_elements: usize,
  /// Discovered URLs, in deterministic priority order.
  pub urls: Vec<String>,
  /// True when discovery stopped early due to `max_image_elements`.
  pub limited: bool,
}

fn normalize_mime_type(value: &str) -> Option<String> {
  let base = value.split(';').next().unwrap_or("").trim();
  if base.is_empty() {
    None
  } else {
    Some(base.to_ascii_lowercase())
  }
}

fn is_inert_template(node: &DomNode) -> bool {
  // Template contents are inert unless the template declares a shadow root.
  if !node
    .tag_name()
    .map(|tag| tag.eq_ignore_ascii_case("template"))
    .unwrap_or(false)
  {
    return false;
  }

  if let Some(namespace) = node.namespace() {
    if !(namespace.is_empty() || namespace == HTML_NAMESPACE) {
      return true;
    }
  }

  let Some(mode_attr) = node
    .get_attribute_ref("shadowroot")
    .or_else(|| node.get_attribute_ref("shadowrootmode"))
  else {
    return true;
  };

  !(mode_attr.eq_ignore_ascii_case("open") || mode_attr.eq_ignore_ascii_case("closed"))
}

fn resolve_prefetch_url(ctx: ImageSelectionContext<'_>, raw: &str) -> Option<String> {
  let resolved = resolve_href_with_base(ctx.base_url, raw)?;
  if resolved.starts_with("data:") {
    return None;
  }
  Some(resolved)
}

fn push_unique_url(
  ctx: ImageSelectionContext<'_>,
  seen: &mut HashSet<String>,
  out: &mut Vec<String>,
  raw: &str,
) {
  let Some(resolved) = resolve_prefetch_url(ctx, raw) else {
    return;
  };
  if seen.insert(resolved.clone()) {
    out.push(resolved);
  }
}

fn link_rel_is_icon_candidate(rel_tokens: &[String], as_attr: Option<&str>) -> bool {
  if rel_tokens.iter().any(|t| t.eq_ignore_ascii_case("icon")) {
    return true;
  }

  if rel_tokens
    .iter()
    .any(|t| t.eq_ignore_ascii_case("apple-touch-icon") || t.eq_ignore_ascii_case("mask-icon"))
  {
    return true;
  }

  if rel_tokens
    .iter()
    .any(|t| t.eq_ignore_ascii_case("apple-touch-icon-precomposed") || t.eq_ignore_ascii_case("manifest"))
  {
    return true;
  }

  if rel_tokens.iter().any(|t| t.eq_ignore_ascii_case("preload")) {
    return as_attr
      .map(|v| v.trim().eq_ignore_ascii_case("image"))
      .unwrap_or(false);
  }

  false
}

fn picture_sources_and_fallback_img<'a>(
  picture: &'a DomNode,
) -> Option<(Vec<PictureSource>, &'a DomNode)> {
  let mut sources: Vec<PictureSource> = Vec::new();

  for child in &picture.children {
    let Some(tag) = child.tag_name() else {
      continue;
    };

    if tag.eq_ignore_ascii_case("source") {
      let Some(srcset_attr) = child.get_attribute_ref("srcset") else {
        continue;
      };
      let parsed_srcset = parse_srcset(srcset_attr);
      if parsed_srcset.is_empty() {
        continue;
      }

      let sizes = child.get_attribute_ref("sizes").and_then(parse_sizes);
      let media = child
        .get_attribute_ref("media")
        .and_then(|m| MediaQuery::parse_list(m).ok());
      let mime_type = child
        .get_attribute_ref("type")
        .and_then(normalize_mime_type);

      sources.push(PictureSource {
        srcset: parsed_srcset,
        sizes,
        media,
        mime_type,
      });
      continue;
    }

    if tag.eq_ignore_ascii_case("img") {
      return Some((sources, child));
    }
  }

  None
}

/// Discover image URLs in a DOM tree using the renderer's responsive image selection.
///
/// This intentionally does **not** execute layout, so slot widths are treated as
/// unknown and `sizes` evaluation uses the viewport fallback logic.
pub fn discover_image_prefetch_urls(
  dom: &DomNode,
  ctx: ImageSelectionContext<'_>,
  limits: ImagePrefetchLimits,
) -> ImagePrefetchDiscovery {
  let mut urls: Vec<String> = Vec::new();
  let mut seen_urls: HashSet<String> = HashSet::new();
  let mut image_elements = 0usize;
  let mut limited = false;

  fn walk(
    node: &DomNode,
    ctx: ImageSelectionContext<'_>,
    limits: ImagePrefetchLimits,
    image_elements: &mut usize,
    limited: &mut bool,
    seen_urls: &mut HashSet<String>,
    urls: &mut Vec<String>,
  ) -> bool {
    if limits.max_image_elements == 0 || limits.max_urls_per_element == 0 {
      return false;
    }
    if *image_elements >= limits.max_image_elements {
      *limited = true;
      return false;
    }

    if is_inert_template(node) {
      return true;
    }

    if let Some(tag) = node.tag_name() {
      if tag.eq_ignore_ascii_case("picture") {
        if let Some((picture_sources, img)) = picture_sources_and_fallback_img(node) {
          if *image_elements >= limits.max_image_elements {
            *limited = true;
            return false;
          }
          *image_elements += 1;

          let img_src = img.get_attribute_ref("src").unwrap_or("");
          let img_srcset = img
            .get_attribute_ref("srcset")
            .map(parse_srcset)
            .unwrap_or_default();
          let img_sizes = img.get_attribute_ref("sizes").and_then(parse_sizes);

          for selected in image_sources_with_fallback(
            img_src,
            &img_srcset,
            img_sizes.as_ref(),
            &picture_sources,
            ctx,
          )
          .into_iter()
          .take(limits.max_urls_per_element)
          {
            push_unique_url(ctx, seen_urls, urls, selected.url);
          }
          if *image_elements >= limits.max_image_elements {
            *limited = true;
            return false;
          }
          return true;
        }
      } else if tag.eq_ignore_ascii_case("img") {
        if *image_elements >= limits.max_image_elements {
          *limited = true;
          return false;
        }
        let img_src = node.get_attribute_ref("src").unwrap_or("");
        let has_src = !img_src.trim().is_empty();
        let has_srcset = node
          .get_attribute_ref("srcset")
          .map(|s| !s.trim().is_empty())
          .unwrap_or(false);
        if has_src || has_srcset {
          *image_elements += 1;

          let img_srcset = node
            .get_attribute_ref("srcset")
            .map(parse_srcset)
            .unwrap_or_default();
          let img_sizes = node.get_attribute_ref("sizes").and_then(parse_sizes);

          for selected in
            image_sources_with_fallback(img_src, &img_srcset, img_sizes.as_ref(), &[], ctx)
              .into_iter()
              .take(limits.max_urls_per_element)
          {
            push_unique_url(ctx, seen_urls, urls, selected.url);
          }
          if *image_elements >= limits.max_image_elements {
            *limited = true;
            return false;
          }
        }
      } else if tag.eq_ignore_ascii_case("video") {
        if let Some(poster) = node.get_attribute_ref("poster") {
          if !poster.trim().is_empty() {
            if *image_elements >= limits.max_image_elements {
              *limited = true;
              return false;
            }
            *image_elements += 1;
            push_unique_url(ctx, seen_urls, urls, poster);
            if *image_elements >= limits.max_image_elements {
              *limited = true;
              return false;
            }
          }
        }
      } else if tag.eq_ignore_ascii_case("link") {
        // Note: do not apply srcset selection logic to `<link>` elements.
        // This tool intentionally fetches only `href` for icon-like links
        // (and `rel=preload as=image`). See `imagesrcset` / `imagesizes` for
        // responsive preloads, which are intentionally not parsed here.
        if let Some(rel_attr) = node.get_attribute_ref("rel") {
          let rel_tokens = tokenize_rel_list(rel_attr);
          if !rel_tokens.is_empty() {
            let href = node.get_attribute_ref("href").unwrap_or("");
            if !href.trim().is_empty() {
              let as_attr = node.get_attribute_ref("as");
              if link_rel_is_icon_candidate(&rel_tokens, as_attr) {
                if *image_elements >= limits.max_image_elements {
                  *limited = true;
                  return false;
                }
                *image_elements += 1;
                push_unique_url(ctx, seen_urls, urls, href);
                if *image_elements >= limits.max_image_elements {
                  *limited = true;
                  return false;
                }
              }
            }
          }
        }
      }
    }

    for child in &node.children {
      if !walk(child, ctx, limits, image_elements, limited, seen_urls, urls) {
        return false;
      }
    }
    true
  }

  walk(
    dom,
    ctx,
    limits,
    &mut image_elements,
    &mut limited,
    &mut seen_urls,
    &mut urls,
  );

  ImagePrefetchDiscovery {
    image_elements,
    urls,
    limited,
  }
}

#[cfg(test)]
mod tests {
  use super::{discover_image_prefetch_urls, ImagePrefetchLimits};
  use crate::dom::parse_html;
  use crate::geometry::Size;
  use crate::html::images::ImageSelectionContext;
  use crate::style::media::MediaContext;

  fn media_ctx_for(viewport: (f32, f32), dpr: f32) -> MediaContext {
    MediaContext::screen(viewport.0, viewport.1)
      .with_device_pixel_ratio(dpr)
      .with_env_overrides()
  }

  fn ctx_for<'a>(
    viewport: (f32, f32),
    dpr: f32,
    media_ctx: &'a MediaContext,
    base_url: &'a str,
  ) -> ImageSelectionContext<'a> {
    ImageSelectionContext {
      device_pixel_ratio: dpr,
      slot_width: None,
      viewport: Some(Size::new(viewport.0, viewport.1)),
      media_context: Some(media_ctx),
      font_size: None,
      base_url: Some(base_url),
    }
  }

  #[test]
  fn selects_picture_source_matching_media_and_caps_urls() {
    let html = r#"
      <picture>
        <source media="(max-width: 600px)" srcset="small.jpg 1x, small@2x.jpg 2x">
        <source media="(min-width: 601px)" srcset="large.jpg 1x, large@2x.jpg 2x">
        <img src="fallback.jpg" srcset="fallback1.jpg 1x, fallback2.jpg 2x">
      </picture>
    "#;
    let dom = parse_html(html).unwrap();

    let media_ctx = media_ctx_for((500.0, 800.0), 2.0);
    let ctx = ctx_for((500.0, 800.0), 2.0, &media_ctx, "https://example.com/");
    let out = discover_image_prefetch_urls(
      &dom,
      ctx,
      ImagePrefetchLimits {
        max_image_elements: 10,
        max_urls_per_element: 2,
      },
    );

    assert_eq!(out.image_elements, 1);
    assert!(!out.limited);
    assert_eq!(
      out.urls,
      vec![
        "https://example.com/small@2x.jpg".to_string(),
        "https://example.com/fallback2.jpg".to_string(),
      ]
    );
  }

  #[test]
  fn caps_images_per_page_in_dom_order() {
    let html = r#"
      <img src="a.jpg">
      <img src="b.jpg">
    "#;
    let dom = parse_html(html).unwrap();

    let media_ctx = media_ctx_for((1200.0, 800.0), 1.0);
    let ctx = ctx_for((1200.0, 800.0), 1.0, &media_ctx, "https://example.com/");
    let out = discover_image_prefetch_urls(
      &dom,
      ctx,
      ImagePrefetchLimits {
        max_image_elements: 1,
        max_urls_per_element: 3,
      },
    );

    assert_eq!(out.image_elements, 1);
    assert!(out.limited);
    assert_eq!(out.urls, vec!["https://example.com/a.jpg".to_string()]);
  }
}
