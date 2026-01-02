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
use crate::html::images::{image_sources_with_fallback, select_image_source, ImageSelectionContext};
use crate::resource::is_data_url;
use crate::style::media::MediaQuery;
use crate::style::media::MediaContext;
use crate::tree::box_tree::{PictureSource, SizesList, SrcsetDescriptor};
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

const WIDTH_DESCRIPTOR_SECONDARY_SLOT_SCALE: f32 = 0.75;

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
  if is_data_url(&resolved) {
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

fn normalized_image_mime(mime: &str) -> String {
  mime
    .split(';')
    .next()
    .unwrap_or("")
    .trim()
    .to_ascii_lowercase()
}

fn is_supported_image_mime(mime: &str) -> bool {
  let normalized = normalized_image_mime(mime);
  if normalized == "image/svg+xml" {
    return true;
  }

  image::ImageFormat::from_mime_type(&normalized).is_some()
}

fn picture_source_matches(source: &PictureSource, ctx: ImageSelectionContext<'_>) -> bool {
  if source.srcset.is_empty() {
    return false;
  }

  if let Some(mime) = &source.mime_type {
    if !is_supported_image_mime(mime) {
      return false;
    }
  }

  match &source.media {
    Some(queries) => ctx
      .media_context
      .map(|m| m.evaluate_list(queries))
      .unwrap_or(true),
    None => true,
  }
}

fn select_picture_source<'a>(
  sources: &'a [PictureSource],
  ctx: ImageSelectionContext<'_>,
) -> Option<&'a PictureSource> {
  sources.iter().find(|source| picture_source_matches(source, ctx))
}

fn estimate_source_size(sizes: Option<&SizesList>, ctx: ImageSelectionContext<'_>) -> Option<f32> {
  let viewport = ctx.viewport?;
  let font_size = ctx.font_size.unwrap_or(16.0);
  let media_ctx = ctx.media_context.cloned().unwrap_or_else(|| {
    MediaContext::screen(viewport.width, viewport.height)
      .with_device_pixel_ratio(ctx.device_pixel_ratio)
      .with_env_overrides()
  });

  let resolved = if let Some(list) = sizes {
    list.evaluate(&media_ctx, viewport, font_size)
  } else {
    viewport.width
  };

  resolved
    .is_finite()
    .then_some(resolved)
    .filter(|v| *v > 0.0)
}

fn srcset_has_width_descriptors(srcset: &[crate::tree::box_tree::SrcsetCandidate]) -> bool {
  srcset
    .iter()
    .any(|c| matches!(c.descriptor, SrcsetDescriptor::Width(_)))
}

fn link_rel_is_preload_image(rel_tokens: &[String], as_attr: Option<&str>) -> bool {
  rel_tokens.iter().any(|t| t.eq_ignore_ascii_case("preload"))
    && as_attr
      .map(|v| v.trim().eq_ignore_ascii_case("image"))
      .unwrap_or(false)
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

fn push_prefetch_selection(
  ctx: ImageSelectionContext<'_>,
  picture_sources: &[PictureSource],
  img_src: &str,
  img_srcset: &[crate::tree::box_tree::SrcsetCandidate],
  img_sizes: Option<&SizesList>,
  limits: ImagePrefetchLimits,
  seen_urls: &mut HashSet<String>,
  urls: &mut Vec<String>,
) {
  if limits.max_urls_per_element == 0 {
    return;
  }

  let srcset_to_consider = select_picture_source(picture_sources, ctx)
    .map(|source| source.srcset.as_slice())
    .unwrap_or(img_srcset);
  let uses_width_descriptors = srcset_has_width_descriptors(srcset_to_consider);

  if uses_width_descriptors {
    let sizes_for_estimate = select_picture_source(picture_sources, ctx)
      .and_then(|source| source.sizes.as_ref())
      .or(img_sizes);

    let Some(source_size) = estimate_source_size(sizes_for_estimate, ctx) else {
      // Fallback to the renderer-aligned selection which will pick a candidate based on `sizes`
      // evaluation when slot widths are unknown.
      for selected in image_sources_with_fallback(img_src, img_srcset, img_sizes, picture_sources, ctx)
        .into_iter()
        .take(limits.max_urls_per_element)
      {
        push_unique_url(ctx, seen_urls, urls, selected.url);
      }
      return;
    };

    let mut emitted = 0usize;
    for slot_width in [
      source_size,
      source_size * WIDTH_DESCRIPTOR_SECONDARY_SLOT_SCALE,
    ] {
      if emitted >= limits.max_urls_per_element {
        break;
      }
      if !slot_width.is_finite() || slot_width <= 0.0 {
        continue;
      }

      let selection_ctx = ImageSelectionContext {
        slot_width: Some(slot_width),
        ..ctx
      };
      let selected = select_image_source(img_src, img_srcset, img_sizes, picture_sources, selection_ctx);
      if selected.url.trim().is_empty() {
        continue;
      }
      let before_len = urls.len();
      push_unique_url(ctx, seen_urls, urls, selected.url);
      if urls.len() != before_len {
        emitted += 1;
      }
    }

    // Ensure a plain `src` is still captured when we didn't fill the cap (e.g. malformed srcset).
    if emitted < limits.max_urls_per_element && !img_src.trim().is_empty() {
      push_unique_url(ctx, seen_urls, urls, img_src);
    }
    return;
  }

  for selected in image_sources_with_fallback(img_src, img_srcset, img_sizes, picture_sources, ctx)
    .into_iter()
    .take(limits.max_urls_per_element)
  {
    push_unique_url(ctx, seen_urls, urls, selected.url);
  }
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

          let img_src = img
            .get_attribute_ref("src")
            .filter(|value| !value.trim().is_empty())
            .or_else(|| {
              img
                .get_attribute_ref("data-gl-src")
                .filter(|value| !value.trim().is_empty())
            })
            .unwrap_or("");
          let img_srcset = img
            .get_attribute_ref("srcset")
            .filter(|value| !value.trim().is_empty())
            .or_else(|| {
              img
                .get_attribute_ref("data-gl-srcset")
                .filter(|value| !value.trim().is_empty())
            })
            .map(parse_srcset)
            .unwrap_or_default();
          let img_sizes = img.get_attribute_ref("sizes").and_then(parse_sizes);

          push_prefetch_selection(
            ctx,
            &picture_sources,
            img_src,
            &img_srcset,
            img_sizes.as_ref(),
            limits,
            seen_urls,
            urls,
          );
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
        let img_src = node
          .get_attribute_ref("src")
          .filter(|value| !value.trim().is_empty())
          .or_else(|| {
            node
              .get_attribute_ref("data-gl-src")
              .filter(|value| !value.trim().is_empty())
          })
          .unwrap_or("");
        let has_src = !img_src.trim().is_empty();
        let img_srcset_attr = node
          .get_attribute_ref("srcset")
          .filter(|value| !value.trim().is_empty())
          .or_else(|| {
            node
              .get_attribute_ref("data-gl-srcset")
              .filter(|value| !value.trim().is_empty())
          });
        let has_srcset = img_srcset_attr.is_some();
        if has_src || has_srcset {
          *image_elements += 1;

          let img_srcset = img_srcset_attr.map(parse_srcset).unwrap_or_default();
          let img_sizes = node.get_attribute_ref("sizes").and_then(parse_sizes);

          push_prefetch_selection(
            ctx,
            &[],
            img_src,
            &img_srcset,
            img_sizes.as_ref(),
            limits,
            seen_urls,
            urls,
          );
          if *image_elements >= limits.max_image_elements {
            *limited = true;
            return false;
          }
        }
      } else if tag.eq_ignore_ascii_case("video") {
        let poster = node
          .get_attribute_ref("poster")
          .filter(|value| !value.trim().is_empty())
          .or_else(|| {
            node
              .get_attribute_ref("gnt-gl-ps")
              .filter(|value| !value.trim().is_empty())
          });
        if let Some(poster) = poster {
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
      } else if tag.eq_ignore_ascii_case("link") {
        if let Some(rel_attr) = node.get_attribute_ref("rel") {
          let rel_tokens = tokenize_rel_list(rel_attr);
          if !rel_tokens.is_empty() {
            let href = node.get_attribute_ref("href").unwrap_or("").trim();
            let as_attr = node.get_attribute_ref("as");

            let media_matches = match node.get_attribute_ref("media") {
              Some(media) => MediaQuery::parse_list(media)
                .ok()
                .map(|list| {
                  ctx
                    .media_context
                    .map(|m| m.evaluate_list(&list))
                    .unwrap_or(true)
                })
                .unwrap_or(true),
              None => true,
            };
            if !media_matches {
              return true;
            }

            if link_rel_is_preload_image(&rel_tokens, as_attr) {
              let parsed_srcset = node
                .get_attribute_ref("imagesrcset")
                .map(parse_srcset)
                .unwrap_or_default();
              let parsed_sizes = node.get_attribute_ref("imagesizes").and_then(parse_sizes);
              if href.is_empty() && parsed_srcset.is_empty() {
                return true;
              }

              if *image_elements >= limits.max_image_elements {
                *limited = true;
                return false;
              }
              *image_elements += 1;
              push_prefetch_selection(
                ctx,
                &[],
                href,
                &parsed_srcset,
                parsed_sizes.as_ref(),
                limits,
                seen_urls,
                urls,
              );
              if *image_elements >= limits.max_image_elements {
                *limited = true;
                return false;
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

  #[test]
  fn selects_link_preload_imagesrcset() {
    let html = r#"
      <link rel="preload" as="image"
        href="fallback.jpg"
        imagesrcset="a1.jpg 1x, a2.jpg 2x">
    "#;
    let dom = parse_html(html).unwrap();

    let media_ctx = media_ctx_for((800.0, 600.0), 2.0);
    let ctx = ctx_for((800.0, 600.0), 2.0, &media_ctx, "https://example.com/");
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
        "https://example.com/a2.jpg".to_string(),
        "https://example.com/fallback.jpg".to_string(),
      ]
    );
  }

  #[test]
  fn discovers_img_src_from_data_gl_src() {
    let html = r#"<img data-gl-src="a.jpg">"#;
    let dom = parse_html(html).unwrap();

    let media_ctx = media_ctx_for((800.0, 600.0), 1.0);
    let ctx = ctx_for((800.0, 600.0), 1.0, &media_ctx, "https://example.com/");
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
    assert_eq!(out.urls, vec!["https://example.com/a.jpg".to_string()]);
  }

  #[test]
  fn discovers_img_srcset_from_data_gl_srcset() {
    let html = r#"<img data-gl-srcset="a1.jpg 1x, a2.jpg 2x">"#;
    let dom = parse_html(html).unwrap();

    let media_ctx = media_ctx_for((800.0, 600.0), 2.0);
    let ctx = ctx_for((800.0, 600.0), 2.0, &media_ctx, "https://example.com/");
    let out = discover_image_prefetch_urls(
      &dom,
      ctx,
      ImagePrefetchLimits {
        max_image_elements: 10,
        max_urls_per_element: 1,
      },
    );

    assert_eq!(out.image_elements, 1);
    assert!(!out.limited);
    assert_eq!(out.urls, vec!["https://example.com/a2.jpg".to_string()]);
  }

  #[test]
  fn discovers_video_poster_from_gnt_gl_ps() {
    let html = r#"<video gnt-gl-ps="poster.jpg"></video>"#;
    let dom = parse_html(html).unwrap();

    let media_ctx = media_ctx_for((800.0, 600.0), 1.0);
    let ctx = ctx_for((800.0, 600.0), 1.0, &media_ctx, "https://example.com/");
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
    assert_eq!(out.urls, vec!["https://example.com/poster.jpg".to_string()]);
  }

  #[test]
  fn hedges_width_descriptor_srcset_by_slot_width_guess() {
    let html = r#"
      <img
        src="fallback.jpg"
        srcset="small.jpg 600w, large.jpg 800w"
      >
    "#;
    let dom = parse_html(html).unwrap();

    let media_ctx = media_ctx_for((800.0, 600.0), 1.0);
    let ctx = ctx_for((800.0, 600.0), 1.0, &media_ctx, "https://example.com/");
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
        "https://example.com/large.jpg".to_string(),
        "https://example.com/small.jpg".to_string(),
      ]
    );
  }
}
