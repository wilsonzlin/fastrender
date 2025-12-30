use crate::api::{render_html_with_shared_resources, ResourceKind};
use crate::geometry::Rect;
use crate::html::encoding::decode_html_bytes;
use crate::image_loader::ImageCache;
use crate::paint::pixmap::new_pixmap;
use crate::resource::origin_from_url;
use crate::style::color::Rgba;
use crate::style::ComputedStyle;
use crate::text::font_loader::FontContext;
use std::sync::Arc;
use tiny_skia::Pixmap;

const IFRAME_NESTING_LIMIT_MESSAGE: &str = "iframe nesting limit exceeded";

pub(crate) fn render_iframe_srcdoc(
  html: &str,
  src: Option<&str>,
  content_rect: Rect,
  style: Option<&ComputedStyle>,
  image_cache: &ImageCache,
  font_ctx: &FontContext,
  device_pixel_ratio: f32,
  max_iframe_depth: usize,
) -> Option<Pixmap> {
  let width = content_rect.width().ceil() as u32;
  let height = content_rect.height().ceil() as u32;
  if width == 0 || height == 0 {
    return None;
  }

  let iframe_url = src
    .filter(|s| !s.is_empty())
    .map(|s| image_cache.resolve_url(s))
    .unwrap_or_else(|| "about:srcdoc".to_string());
  let context = image_cache.resource_context();
  let remaining_depth = context
    .as_ref()
    .and_then(|ctx| ctx.iframe_depth_remaining)
    .unwrap_or(max_iframe_depth);
  if remaining_depth == 0 {
    if let Some(ctx) = context.as_ref() {
      ctx.record_violation(
        ResourceKind::Document,
        &iframe_url,
        None,
        IFRAME_NESTING_LIMIT_MESSAGE.to_string(),
      );
    }
    let device_width = ((width as f32) * device_pixel_ratio).round().max(1.0) as u32;
    let device_height = ((height as f32) * device_pixel_ratio).round().max(1.0) as u32;
    return new_pixmap(device_width, device_height);
  }

  let background = style.map(|s| s.background_color).unwrap_or(Rgba::WHITE);
  let base_url = image_cache.base_url();
  let mut cache = image_cache.clone();
  if let Some(base_url) = base_url.clone() {
    cache.set_base_url(base_url);
  }
  let context = cache.resource_context();
  let nested_depth = remaining_depth.saturating_sub(1);
  let nested_context = context
    .as_ref()
    .map(|ctx| ctx.clone().with_iframe_depth(nested_depth));
  let policy = nested_context
    .as_ref()
    .map(|c| c.policy.clone())
    .or_else(|| context.as_ref().map(|c| c.policy.clone()))
    .unwrap_or_default();

  render_html_with_shared_resources(
    html,
    width,
    height,
    background,
    font_ctx,
    &cache,
    Arc::clone(cache.fetcher()),
    base_url,
    device_pixel_ratio,
    policy,
    nested_context,
    nested_depth,
  )
  .ok()
}

pub(crate) fn render_iframe_src(
  src: &str,
  content_rect: Rect,
  style: Option<&ComputedStyle>,
  image_cache: &ImageCache,
  font_ctx: &FontContext,
  device_pixel_ratio: f32,
  max_iframe_depth: usize,
) -> Option<Pixmap> {
  if src.is_empty() {
    return None;
  }
  let width = content_rect.width().ceil() as u32;
  let height = content_rect.height().ceil() as u32;
  if width == 0 || height == 0 {
    return None;
  }

  let context = image_cache.resource_context();
  let resolved = image_cache.resolve_url(src);
  let remaining_depth = context
    .as_ref()
    .and_then(|ctx| ctx.iframe_depth_remaining)
    .unwrap_or(max_iframe_depth);
  if remaining_depth == 0 {
    if let Some(ctx) = context.as_ref() {
      ctx.record_violation(
        ResourceKind::Document,
        &resolved,
        Some(&resolved),
        IFRAME_NESTING_LIMIT_MESSAGE.to_string(),
      );
    }
    let device_width = ((width as f32) * device_pixel_ratio).round().max(1.0) as u32;
    let device_height = ((height as f32) * device_pixel_ratio).round().max(1.0) as u32;
    return new_pixmap(device_width, device_height);
  }
  let nested_depth = remaining_depth.saturating_sub(1);
  let fetcher = Arc::clone(image_cache.fetcher());
  if let Some(ctx) = context.as_ref() {
    if ctx
      .check_allowed(ResourceKind::Document, &resolved)
      .is_err()
    {
      return None;
    }
  }
  let resource = fetcher.fetch(&resolved).ok()?;
  if let Some(ctx) = context.as_ref() {
    if ctx
      .check_allowed_with_final(
        ResourceKind::Document,
        &resolved,
        resource.final_url.as_deref(),
      )
      .is_err()
    {
      return None;
    }
  }
  let content_type = resource.content_type.as_deref();
  let is_html = content_type
    .map(|ct| {
      let ct = ct.to_ascii_lowercase();
      ct.starts_with("text/html")
        || ct.starts_with("application/xhtml+xml")
        || ct.starts_with("application/html")
        || ct.contains("+html")
    })
    .unwrap_or_else(|| {
      let lower = resolved.to_ascii_lowercase();
      lower.ends_with(".html") || lower.ends_with(".htm") || lower.ends_with(".xhtml")
    });
  if !is_html {
    return None;
  }

  let html = decode_html_bytes(&resource.bytes, content_type);
  let background = style.map(|s| s.background_color).unwrap_or(Rgba::WHITE);
  let mut cache = image_cache.clone();
  cache.set_base_url(resolved.clone());
  let nested_origin = origin_from_url(resource.final_url.as_deref().unwrap_or(&resolved));
  let nested_context = context.as_ref().map(|ctx| {
    ctx
      .for_origin(nested_origin)
      .with_iframe_depth(nested_depth)
  });
  let policy = nested_context
    .as_ref()
    .map(|ctx| ctx.policy.clone())
    .or_else(|| context.as_ref().map(|ctx| ctx.policy.clone()))
    .unwrap_or_default();

  render_html_with_shared_resources(
    &html,
    width,
    height,
    background,
    font_ctx,
    &cache,
    fetcher,
    Some(resolved),
    device_pixel_ratio,
    policy,
    nested_context,
    nested_depth,
  )
  .ok()
}
