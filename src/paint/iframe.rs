use crate::api::{render_html_with_shared_resources, ResourceKind};
use crate::geometry::Rect;
use crate::html::encoding::decode_html_bytes;
use crate::image_loader::ImageCache;
use crate::paint::display_list::ImageData;
use crate::paint::pixmap::new_pixmap;
use crate::resource::origin_from_url;
use crate::resource::ResourceAccessPolicy;
use crate::style::color::Rgba;
use crate::style::ComputedStyle;
use crate::text::font_loader::FontContext;
use lru::LruCache;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::sync::Arc;
use std::sync::{Mutex, OnceLock};
use tiny_skia::Pixmap;

const IFRAME_NESTING_LIMIT_MESSAGE: &str = "iframe nesting limit exceeded";

const IFRAME_RENDER_CACHE_MAX_ENTRIES: usize = 128;
const IFRAME_RENDER_CACHE_MAX_BYTES: usize = 128 * 1024 * 1024;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct BackgroundKey {
  r: u8,
  g: u8,
  b: u8,
  a_bits: u32,
}

impl From<Rgba> for BackgroundKey {
  fn from(value: Rgba) -> Self {
    Self {
      r: value.r,
      g: value.g,
      b: value.b,
      a_bits: value.a.to_bits(),
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum IframeRenderCacheContent {
  Src { url: String },
  Srcdoc { html_hash: u64, base_url: Option<String> },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct IframeRenderCacheKey {
  content: IframeRenderCacheContent,
  css_width: u32,
  css_height: u32,
  device_pixel_ratio_bits: u32,
  nested_depth: usize,
  background: BackgroundKey,
  policy_hash: u64,
}

struct CachedIframeRenderEntry {
  image: Arc<ImageData>,
  bytes: usize,
}

struct IframeRenderCache {
  inner: LruCache<IframeRenderCacheKey, CachedIframeRenderEntry>,
  max_entries: usize,
  max_bytes: usize,
  current_bytes: usize,
}

impl IframeRenderCache {
  fn new(max_entries: usize, max_bytes: usize) -> Self {
    Self {
      inner: LruCache::unbounded(),
      max_entries,
      max_bytes,
      current_bytes: 0,
    }
  }

  fn get(&mut self, key: &IframeRenderCacheKey) -> Option<Arc<ImageData>> {
    self.inner.get(key).map(|entry| Arc::clone(&entry.image))
  }

  fn insert(&mut self, key: IframeRenderCacheKey, image: Arc<ImageData>) {
    let bytes = image.pixels.len();
    if self.max_bytes > 0 && bytes > self.max_bytes {
      // Skip caching entries that would evict the entire cache on their own.
      return;
    }
    if let Some(entry) = self.inner.pop(&key) {
      self.current_bytes = self.current_bytes.saturating_sub(entry.bytes);
    }
    self.inner.put(key, CachedIframeRenderEntry { image, bytes });
    self.current_bytes = self.current_bytes.saturating_add(bytes);
    self.evict_if_needed();
  }

  fn evict_if_needed(&mut self) {
    while (self.max_entries > 0 && self.inner.len() > self.max_entries)
      || (self.max_bytes > 0 && self.current_bytes > self.max_bytes)
    {
      if let Some((_key, entry)) = self.inner.pop_lru() {
        self.current_bytes = self.current_bytes.saturating_sub(entry.bytes);
      } else {
        break;
      }
    }
  }
}

static IFRAME_RENDER_CACHE: OnceLock<Mutex<IframeRenderCache>> = OnceLock::new();

fn iframe_render_cache() -> &'static Mutex<IframeRenderCache> {
  IFRAME_RENDER_CACHE.get_or_init(|| {
    Mutex::new(IframeRenderCache::new(
      IFRAME_RENDER_CACHE_MAX_ENTRIES,
      IFRAME_RENDER_CACHE_MAX_BYTES,
    ))
  })
}

fn stable_hash_bytes(bytes: &[u8]) -> u64 {
  let mut hasher = DefaultHasher::new();
  bytes.hash(&mut hasher);
  hasher.finish()
}

fn policy_fingerprint(policy: &ResourceAccessPolicy) -> u64 {
  // The nested document origin is derived from the iframe's URL/base URL. That value is already
  // part of the cache key, so we avoid hashing it here. This allows identical iframe URLs to share
  // cached renders across different outer documents while still respecting policy knobs.
  let mut hasher = DefaultHasher::new();
  policy.allow_file_from_http.hash(&mut hasher);
  policy.block_mixed_content.hash(&mut hasher);
  policy.same_origin_only.hash(&mut hasher);
  let mut allowed: Vec<String> = policy.allowed_origins.iter().map(ToString::to_string).collect();
  allowed.sort();
  allowed.hash(&mut hasher);
  hasher.finish()
}

fn image_data_from_pixmap(pixmap: &Pixmap, css_width: u32, css_height: u32) -> Arc<ImageData> {
  Arc::new(ImageData::from_pixmap(
    pixmap,
    css_width as f32,
    css_height as f32,
  ))
}

#[cfg(test)]
thread_local! {
  static LAST_IFRAME_CACHE_HIT: std::cell::Cell<Option<bool>> = std::cell::Cell::new(None);
}

#[cfg(test)]
fn record_iframe_cache_hit(hit: bool) {
  LAST_IFRAME_CACHE_HIT.with(|cell| cell.set(Some(hit)));
}

#[cfg(test)]
pub(crate) fn take_last_iframe_cache_hit() -> Option<bool> {
  LAST_IFRAME_CACHE_HIT.with(|cell| cell.replace(None))
}

pub(crate) fn render_iframe_srcdoc(
  html: &str,
  src: Option<&str>,
  content_rect: Rect,
  style: Option<&ComputedStyle>,
  image_cache: &ImageCache,
  font_ctx: &FontContext,
  device_pixel_ratio: f32,
  max_iframe_depth: usize,
) -> Option<Arc<ImageData>> {
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
    let pixmap = new_pixmap(device_width, device_height)?;
    let image = image_data_from_pixmap(&pixmap, width, height);
    #[cfg(test)]
    record_iframe_cache_hit(false);
    return Some(image);
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

  let key = IframeRenderCacheKey {
    content: IframeRenderCacheContent::Srcdoc {
      html_hash: stable_hash_bytes(html.as_bytes()),
      base_url: base_url.clone(),
    },
    css_width: width,
    css_height: height,
    device_pixel_ratio_bits: device_pixel_ratio.to_bits(),
    nested_depth,
    background: background.into(),
    policy_hash: policy_fingerprint(&policy),
  };

  if let Ok(mut guard) = iframe_render_cache().lock() {
    if let Some(image) = guard.get(&key) {
      #[cfg(test)]
      record_iframe_cache_hit(true);
      return Some(image);
    }
  }

  let pixmap = render_html_with_shared_resources(
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
  .ok()?;
  let image = image_data_from_pixmap(&pixmap, width, height);

  if let Ok(mut guard) = iframe_render_cache().lock() {
    guard.insert(key, Arc::clone(&image));
  }
  #[cfg(test)]
  record_iframe_cache_hit(false);
  Some(image)
}

pub(crate) fn render_iframe_src(
  src: &str,
  content_rect: Rect,
  style: Option<&ComputedStyle>,
  image_cache: &ImageCache,
  font_ctx: &FontContext,
  device_pixel_ratio: f32,
  max_iframe_depth: usize,
) -> Option<Arc<ImageData>> {
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
    let pixmap = new_pixmap(device_width, device_height)?;
    let image = image_data_from_pixmap(&pixmap, width, height);
    #[cfg(test)]
    record_iframe_cache_hit(false);
    return Some(image);
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

  let background = style.map(|s| s.background_color).unwrap_or(Rgba::WHITE);
  let policy = context
    .as_ref()
    .map(|ctx| ctx.policy.clone())
    .unwrap_or_default();

  let key = IframeRenderCacheKey {
    content: IframeRenderCacheContent::Src { url: resolved.clone() },
    css_width: width,
    css_height: height,
    device_pixel_ratio_bits: device_pixel_ratio.to_bits(),
    nested_depth,
    background: background.into(),
    policy_hash: policy_fingerprint(&policy),
  };
  if let Ok(mut guard) = iframe_render_cache().lock() {
    if let Some(image) = guard.get(&key) {
      #[cfg(test)]
      record_iframe_cache_hit(true);
      return Some(image);
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
  let mut cache = image_cache.clone();
  cache.set_base_url(resolved.clone());
  let nested_origin = origin_from_url(resource.final_url.as_deref().unwrap_or(&resolved));
  let nested_context = context.as_ref().map(|ctx| {
    ctx
      .for_origin(nested_origin)
      .with_iframe_depth(nested_depth)
  });
  let policy_for_render = nested_context
    .as_ref()
    .map(|ctx| ctx.policy.clone())
    .or_else(|| context.as_ref().map(|ctx| ctx.policy.clone()))
    .unwrap_or_default();

  let pixmap = render_html_with_shared_resources(
    &html,
    width,
    height,
    background,
    font_ctx,
    &cache,
    fetcher,
    Some(resolved),
    device_pixel_ratio,
    policy_for_render,
    nested_context,
    nested_depth,
  )
  .ok()?;

  let image = image_data_from_pixmap(&pixmap, width, height);
  if let Ok(mut guard) = iframe_render_cache().lock() {
    guard.insert(key, Arc::clone(&image));
  }
  #[cfg(test)]
  record_iframe_cache_hit(false);
  Some(image)
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::error::Error;
  use crate::resource::FetchedResource;
  use crate::resource::ResourceFetcher;
  use std::io;
  use std::sync::Arc;
 
  #[derive(Clone, Default)]
  struct RejectingFetcher;
 
  impl ResourceFetcher for RejectingFetcher {
    fn fetch(&self, url: &str) -> crate::error::Result<FetchedResource> {
      Err(Error::Io(io::Error::new(
        io::ErrorKind::NotFound,
        format!("unexpected fetch: {url}"),
      )))
    }
  }
 
  #[test]
  fn iframe_render_cache_hits_for_repeated_srcdoc() {
    let font_ctx = FontContext::new();
    let image_cache = ImageCache::with_fetcher(Arc::new(RejectingFetcher::default()));
    let rect = Rect::from_xywh(0.0, 0.0, 16.0, 16.0);
    let html = r#"
      <style>html, body { margin: 0; padding: 0; background: rgb(0, 255, 0); }</style>
      <div data-fastr-test="iframe-render-cache-113"></div>
    "#;
 
    let first = render_iframe_srcdoc(
      html,
      None,
      rect,
      None,
      &image_cache,
      &font_ctx,
      1.0,
      3,
    )
    .expect("first iframe render");
    assert_eq!(
      take_last_iframe_cache_hit(),
      Some(false),
      "first render should miss cache"
    );
 
    let second = render_iframe_srcdoc(
      html,
      None,
      rect,
      None,
      &image_cache,
      &font_ctx,
      1.0,
      3,
    )
    .expect("second iframe render");
    assert_eq!(
      take_last_iframe_cache_hit(),
      Some(true),
      "second render should hit cache"
    );
    assert!(
      Arc::ptr_eq(&first, &second),
      "cache hit should return the same Arc<ImageData>"
    );
  }
}
