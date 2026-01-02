use crate::api::{render_html_with_shared_resources, ResourceContext, ResourceKind};
use crate::error::{Error, RenderStage};
use crate::geometry::Rect;
use crate::html::encoding::decode_html_bytes;
use crate::image_loader::ImageCache;
use crate::paint::display_list::ImageData;
use crate::paint::pixmap::new_pixmap;
use crate::render_control;
use crate::resource::{
  ensure_http_success, origin_from_url, FetchDestination, FetchRequest, ResourceAccessPolicy,
};
use crate::style::color::Rgba;
use crate::style::ComputedStyle;
use crate::text::font_loader::FontContext;
use lru::LruCache;
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::sync::{Arc, Condvar, Mutex, OnceLock};
use std::time::Duration;
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

#[derive(Clone)]
enum SharedIframeResult {
  Success(Arc<ImageData>),
  None,
}

impl SharedIframeResult {
  fn as_option(&self) -> Option<Arc<ImageData>> {
    match self {
      Self::Success(image) => Some(Arc::clone(image)),
      Self::None => None,
    }
  }
}

struct IframeInFlight {
  result: Mutex<Option<SharedIframeResult>>,
  cv: Condvar,
}

impl IframeInFlight {
  fn new() -> Self {
    Self {
      result: Mutex::new(None),
      cv: Condvar::new(),
    }
  }

  fn set(&self, result: SharedIframeResult) {
    let mut slot = self.result.lock().unwrap_or_else(|e| e.into_inner());
    *slot = Some(result);
    self.cv.notify_all();
  }

  fn wait(&self) -> Option<Arc<ImageData>> {
    let mut guard = self.result.lock().unwrap_or_else(|e| e.into_inner());
    let deadline = render_control::active_deadline().filter(|d| d.is_enabled());
    while guard.is_none() {
      if let Some(deadline) = deadline.as_ref() {
        if deadline.check(RenderStage::Paint).is_err() {
          return None;
        }
        let wait_for = if deadline.timeout_limit().is_some() {
          match deadline.remaining_timeout() {
            Some(remaining) if !remaining.is_zero() => remaining.min(Duration::from_millis(10)),
            _ => return None,
          }
        } else {
          Duration::from_millis(10)
        };
        guard = self
          .cv
          .wait_timeout(guard, wait_for)
          .unwrap_or_else(|e| e.into_inner())
          .0;
      } else {
        guard = self
          .cv
          .wait(guard)
          .unwrap_or_else(|e| e.into_inner());
      }
    }
    guard.as_ref()?.as_option()
  }
}

struct CachedIframeRenderEntry {
  image: Arc<ImageData>,
  bytes: usize,
}

struct IframeRenderCache {
  inner: LruCache<IframeRenderCacheKey, CachedIframeRenderEntry>,
  in_flight: HashMap<IframeRenderCacheKey, Arc<IframeInFlight>>,
  max_entries: usize,
  max_bytes: usize,
  current_bytes: usize,
}

impl IframeRenderCache {
  fn new(max_entries: usize, max_bytes: usize) -> Self {
    Self {
      inner: LruCache::unbounded(),
      in_flight: HashMap::new(),
      max_entries,
      max_bytes,
      current_bytes: 0,
    }
  }

  fn get(&mut self, key: &IframeRenderCacheKey) -> Option<Arc<ImageData>> {
    self.inner.get(key).map(|entry| Arc::clone(&entry.image))
  }

  fn join_inflight(&mut self, key: &IframeRenderCacheKey) -> (Arc<IframeInFlight>, bool) {
    if let Some(existing) = self.in_flight.get(key) {
      return (Arc::clone(existing), false);
    }

    let flight = Arc::new(IframeInFlight::new());
    self
      .in_flight
      .insert(key.clone(), Arc::clone(&flight));
    (flight, true)
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

fn finish_iframe_inflight(
  key: IframeRenderCacheKey,
  flight: &Arc<IframeInFlight>,
  result: Option<Arc<ImageData>>,
) {
  match result {
    Some(image) => {
      let mut cache = iframe_render_cache().lock().unwrap_or_else(|e| e.into_inner());
      cache.in_flight.remove(&key);
      cache.insert(key, Arc::clone(&image));
      drop(cache);
      flight.set(SharedIframeResult::Success(image));
    }
    None => {
      let mut cache = iframe_render_cache().lock().unwrap_or_else(|e| e.into_inner());
      cache.in_flight.remove(&key);
      drop(cache);
      flight.set(SharedIframeResult::None);
    }
  }
}

struct IframeInFlightOwnerGuard {
  key: Option<IframeRenderCacheKey>,
  flight: Arc<IframeInFlight>,
}

impl IframeInFlightOwnerGuard {
  fn new(key: IframeRenderCacheKey, flight: Arc<IframeInFlight>) -> Self {
    Self {
      key: Some(key),
      flight,
    }
  }

  fn finish(&mut self, result: Option<Arc<ImageData>>) {
    let Some(key) = self.key.take() else {
      return;
    };
    finish_iframe_inflight(key, &self.flight, result);
  }
}

impl Drop for IframeInFlightOwnerGuard {
  fn drop(&mut self) {
    let Some(key) = self.key.take() else {
      return;
    };
    finish_iframe_inflight(key, &self.flight, None);
  }
}

fn stable_hash_bytes(bytes: &[u8]) -> u64 {
  let mut hasher = DefaultHasher::new();
  bytes.hash(&mut hasher);
  hasher.finish()
}

fn is_about_blank(url: &str) -> bool {
  const PREFIX: &str = "about:blank";
  let Some(head) = url.get(..PREFIX.len()) else {
    return false;
  };
  if !head.eq_ignore_ascii_case(PREFIX) {
    return false;
  }
  matches!(url.as_bytes().get(PREFIX.len()), None | Some(b'#') | Some(b'?'))
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

#[cfg(test)]
type IframeJoinHook = Arc<dyn Fn(&IframeRenderCacheKey, bool) + Send + Sync>;

#[cfg(test)]
static IFRAME_RENDER_CACHE_JOIN_HOOK: OnceLock<Mutex<Option<IframeJoinHook>>> = OnceLock::new();

#[cfg(test)]
fn iframe_render_cache_join_hook() -> &'static Mutex<Option<IframeJoinHook>> {
  IFRAME_RENDER_CACHE_JOIN_HOOK.get_or_init(|| Mutex::new(None))
}

#[cfg(test)]
fn set_iframe_render_cache_join_hook(hook: Option<IframeJoinHook>) {
  let mut guard = iframe_render_cache_join_hook()
    .lock()
    .unwrap_or_else(|e| e.into_inner());
  *guard = hook;
}

#[cfg(test)]
fn run_iframe_render_cache_join_hook(key: &IframeRenderCacheKey, is_owner: bool) {
  let hook = iframe_render_cache_join_hook()
    .lock()
    .ok()
    .and_then(|guard| guard.as_ref().cloned());
  if let Some(hook) = hook {
    hook(key, is_owner);
  }
}

fn record_resource_error(
  ctx: &ResourceContext,
  kind: ResourceKind,
  requested_url: &str,
  err: &Error,
) {
  match err {
    Error::Resource(res) => {
      let final_url = res.final_url.as_deref().or(Some(res.url.as_str()));
      let mut message = res.message.clone();
      if let Some(status) = res.status {
        let lower = message.to_ascii_lowercase();
        if !lower.contains("status") {
          message = format!("{message} (status {status})");
        }
      }
      ctx.record_violation(kind, requested_url, final_url, message);
    }
    other => ctx.record_violation(kind, requested_url, None, other.to_string()),
  }
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

  let (flight, is_owner) = {
    let mut cache = iframe_render_cache().lock().unwrap_or_else(|e| e.into_inner());
    if let Some(image) = cache.get(&key) {
      #[cfg(test)]
      record_iframe_cache_hit(true);
      return Some(image);
    }
    cache.join_inflight(&key)
  };
  #[cfg(test)]
  run_iframe_render_cache_join_hook(&key, is_owner);
  if !is_owner {
    let image = flight.wait();
    #[cfg(test)]
    record_iframe_cache_hit(image.is_some());
    return image;
  }
  let mut owner_guard = IframeInFlightOwnerGuard::new(key, flight);

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
  owner_guard.finish(Some(Arc::clone(&image)));
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
  let background = style.map(|s| s.background_color).unwrap_or(Rgba::WHITE);

  if is_about_blank(&resolved) {
    // about:blank is a browser-provided empty document. Treat it as an empty iframe instead of a
    // resource fetch so offline fixtures do not record spurious fetch errors.
    let device_width = ((width as f32) * device_pixel_ratio).round().max(1.0) as u32;
    let device_height = ((height as f32) * device_pixel_ratio).round().max(1.0) as u32;
    let mut pixmap = new_pixmap(device_width, device_height)?;
    let color = tiny_skia::Color::from_rgba8(
      background.r,
      background.g,
      background.b,
      background.alpha_u8(),
    );
    pixmap.fill(color);
    let image = image_data_from_pixmap(&pixmap, width, height);
    #[cfg(test)]
    record_iframe_cache_hit(false);
    return Some(image);
  }
  if let Some(ctx) = context.as_ref() {
    if ctx
      .check_allowed(ResourceKind::Document, &resolved)
      .is_err()
    {
      return None;
    }
  }

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
  let (flight, is_owner) = {
    let mut cache = iframe_render_cache().lock().unwrap_or_else(|e| e.into_inner());
    if let Some(image) = cache.get(&key) {
      #[cfg(test)]
      record_iframe_cache_hit(true);
      return Some(image);
    }
    cache.join_inflight(&key)
  };
  #[cfg(test)]
  run_iframe_render_cache_join_hook(&key, is_owner);
  if !is_owner {
    let image = flight.wait();
    #[cfg(test)]
    record_iframe_cache_hit(image.is_some());
    return image;
  }
  let mut owner_guard = IframeInFlightOwnerGuard::new(key, flight);

  let base_url = image_cache.base_url();
  let referrer = context
    .as_ref()
    .and_then(|ctx| ctx.document_url.as_deref())
    .or(base_url.as_deref());
  let mut request = FetchRequest::new(&resolved, FetchDestination::Document);
  if let Some(referrer) = referrer {
    request = request.with_referrer(referrer);
  }
  let resource = match fetcher.fetch_with_request(request) {
    Ok(resource) => resource,
    Err(err) => {
      if let Some(ctx) = context.as_ref() {
        record_resource_error(ctx, ResourceKind::Document, &resolved, &err);
      }
      return None;
    }
  };
  let final_url = resource
    .final_url
    .clone()
    .unwrap_or_else(|| resolved.clone());
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
  if let Err(err) = ensure_http_success(&resource, &resolved) {
    if let Some(ctx) = context.as_ref() {
      record_resource_error(ctx, ResourceKind::Document, &resolved, &err);
    }
    return None;
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
    if let Some(ctx) = context.as_ref() {
      let content_type = content_type.unwrap_or("<missing>");
      let status = resource
        .status
        .map(|s| s.to_string())
        .unwrap_or_else(|| "<missing>".to_string());
      let final_url = resource.final_url.as_deref().unwrap_or(&resolved);
      ctx.record_violation(
        ResourceKind::Document,
        &resolved,
        resource.final_url.as_deref(),
        format!("unexpected content-type {content_type} (status {status}, final_url {final_url})"),
      );
    }
    return None;
  }

  let html = decode_html_bytes(&resource.bytes, content_type);
  let mut cache = image_cache.clone();
  cache.set_base_url(final_url.clone());
  let nested_origin = origin_from_url(&final_url);
  let nested_context = context.as_ref().map(|ctx| {
    let mut nested = ctx.for_origin(nested_origin).with_iframe_depth(nested_depth);
    nested.document_url = resource
      .final_url
      .as_deref()
      .map(|u| u.to_string())
      .or_else(|| Some(resolved.clone()));
    nested
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
    Some(final_url),
    device_pixel_ratio,
    policy_for_render,
    nested_context,
    nested_depth,
  )
  .ok()?;

  let image = image_data_from_pixmap(&pixmap, width, height);
  owner_guard.finish(Some(Arc::clone(&image)));
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
  use std::sync::Mutex as StdMutex;
  use std::sync::atomic::{AtomicUsize, Ordering};
  use std::sync::{Arc, Barrier};
 
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

  struct HtmlFetcher {
    expected_url: String,
    body: Vec<u8>,
    calls: AtomicUsize,
  }

  impl ResourceFetcher for HtmlFetcher {
    fn fetch(&self, url: &str) -> crate::error::Result<FetchedResource> {
      self.calls.fetch_add(1, Ordering::SeqCst);
      if url != self.expected_url {
        return Err(Error::Io(io::Error::new(
          io::ErrorKind::NotFound,
          format!("unexpected fetch: {url}"),
        )));
      }
      Ok(FetchedResource::new(
        self.body.clone(),
        Some("text/html; charset=utf-8".to_string()),
      ))
    }
  }

  struct RedirectingStylesheetFetcher {
    doc_url: String,
    final_url: String,
    css_url: String,
    requests: StdMutex<Vec<String>>,
  }

  impl ResourceFetcher for RedirectingStylesheetFetcher {
    fn fetch(&self, url: &str) -> crate::error::Result<FetchedResource> {
      self
        .requests
        .lock()
        .unwrap_or_else(|e| e.into_inner())
        .push(url.to_string());

      if url == self.doc_url {
        let html = format!(
          r#"
            <link rel="stylesheet" href="style-redirect-113.css">
            <div data-fastr-test="iframe-redirect-base-url-113"></div>
          "#
        );
        return Ok(FetchedResource::with_final_url(
          html.into_bytes(),
          Some("text/html; charset=utf-8".to_string()),
          Some(self.final_url.clone()),
        ));
      }
      if url == self.css_url {
        return Ok(FetchedResource::new(
          b"html, body { margin: 0; padding: 0; }".to_vec(),
          Some("text/css; charset=utf-8".to_string()),
        ));
      }

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

  #[test]
  fn iframe_render_cache_hits_for_repeated_src() {
    let font_ctx = FontContext::new();
    let url = "https://example.com/iframe-render-cache-src-113.html";
    let html = r#"
      <style>html, body { margin: 0; padding: 0; background: rgb(255, 0, 0); }</style>
      <div data-fastr-test="iframe-render-cache-src-113"></div>
    "#;
    let fetcher = Arc::new(HtmlFetcher {
      expected_url: url.to_string(),
      body: html.as_bytes().to_vec(),
      calls: AtomicUsize::new(0),
    });
    let image_cache = ImageCache::with_fetcher(fetcher.clone());
    let rect = Rect::from_xywh(0.0, 0.0, 16.0, 16.0);

    let first = render_iframe_src(
      url,
      rect,
      None,
      &image_cache,
      &font_ctx,
      1.0,
      3,
    )
    .expect("first iframe src render");
    assert_eq!(
      take_last_iframe_cache_hit(),
      Some(false),
      "first render should miss cache"
    );

    let second = render_iframe_src(
      url,
      rect,
      None,
      &image_cache,
      &font_ctx,
      1.0,
      3,
    )
    .expect("second iframe src render");
    assert_eq!(
      take_last_iframe_cache_hit(),
      Some(true),
      "second render should hit cache"
    );
    assert!(
      Arc::ptr_eq(&first, &second),
      "cache hit should return the same Arc<ImageData>"
    );
    assert_eq!(
      fetcher.calls.load(Ordering::SeqCst),
      1,
      "cache hit should avoid re-fetching iframe HTML"
    );
  }

  #[test]
  fn iframe_src_uses_final_url_for_relative_resolution() {
    let font_ctx = FontContext::new();
    let requested_url = "https://example.com/original/iframe-redirect-113.html";
    let final_url = "https://example.com/final/iframe-redirect-113.html";
    let expected_css_url = "https://example.com/final/style-redirect-113.css";
    let fetcher = Arc::new(RedirectingStylesheetFetcher {
      doc_url: requested_url.to_string(),
      final_url: final_url.to_string(),
      css_url: expected_css_url.to_string(),
      requests: StdMutex::new(Vec::new()),
    });
    let image_cache = ImageCache::with_fetcher(fetcher.clone());
    let rect = Rect::from_xywh(0.0, 0.0, 16.0, 16.0);

    render_iframe_src(
      requested_url,
      rect,
      None,
      &image_cache,
      &font_ctx,
      1.0,
      3,
    )
    .expect("iframe src render should succeed");

    let urls = fetcher
      .requests
      .lock()
      .unwrap_or_else(|e| e.into_inner())
      .clone();
    assert!(
      urls.iter().any(|u| u == expected_css_url),
      "expected stylesheet URL {expected_css_url} to be requested; got {urls:?}"
    );
    assert!(
      !urls
        .iter()
        .any(|u| u == "https://example.com/original/style-redirect-113.css"),
      "stylesheet should resolve relative to the final URL, not the requested URL; got {urls:?}"
    );
  }

  #[test]
  fn iframe_render_cache_deduplicates_inflight_srcdoc_renders() {
    let font_ctx = FontContext::new();
    let image_cache = ImageCache::with_fetcher(Arc::new(RejectingFetcher::default()));
    let rect = Rect::from_xywh(0.0, 0.0, 16.0, 16.0);
    let html = r#"
      <style>html, body { margin: 0; padding: 0; background: rgb(0, 0, 255); }</style>
      <div data-fastr-test="iframe-render-cache-inflight-113"></div>
    "#;

    let key = IframeRenderCacheKey {
      content: IframeRenderCacheContent::Srcdoc {
        html_hash: stable_hash_bytes(html.as_bytes()),
        base_url: None,
      },
      css_width: 16,
      css_height: 16,
      device_pixel_ratio_bits: 1.0f32.to_bits(),
      nested_depth: 2,
      background: Rgba::WHITE.into(),
      policy_hash: policy_fingerprint(&ResourceAccessPolicy::default()),
    };
    let barrier = Arc::new(Barrier::new(2));
    let owners = Arc::new(AtomicUsize::new(0));
    let waiters = Arc::new(AtomicUsize::new(0));

    struct HookReset;
    impl Drop for HookReset {
      fn drop(&mut self) {
        set_iframe_render_cache_join_hook(None);
      }
    }
    let _reset = HookReset;

    let key_for_hook = key.clone();
    let barrier_for_hook = Arc::clone(&barrier);
    let owners_for_hook = Arc::clone(&owners);
    let waiters_for_hook = Arc::clone(&waiters);
    set_iframe_render_cache_join_hook(Some(Arc::new(move |hook_key, is_owner| {
      if hook_key != &key_for_hook {
        return;
      }
      if is_owner {
        owners_for_hook.fetch_add(1, Ordering::SeqCst);
      } else {
        waiters_for_hook.fetch_add(1, Ordering::SeqCst);
      }
      barrier_for_hook.wait();
    })));

    let cache1 = image_cache.clone();
    let cache2 = image_cache.clone();
    let font1 = font_ctx.clone();
    let font2 = font_ctx.clone();
    let t1 = std::thread::spawn(move || {
      render_iframe_srcdoc(html, None, rect, None, &cache1, &font1, 1.0, 3)
        .expect("thread1 iframe render")
    });
    let t2 = std::thread::spawn(move || {
      render_iframe_srcdoc(html, None, rect, None, &cache2, &font2, 1.0, 3)
        .expect("thread2 iframe render")
    });
    let first = t1.join().expect("join thread1");
    let second = t2.join().expect("join thread2");

    assert_eq!(
      owners.load(Ordering::SeqCst),
      1,
      "exactly one thread should render the iframe"
    );
    assert_eq!(
      waiters.load(Ordering::SeqCst),
      1,
      "the other thread should wait on the in-flight render"
    );
    assert!(
      Arc::ptr_eq(&first, &second),
      "in-flight waiters should receive the same Arc<ImageData>"
    );
  }
}

#[cfg(test)]
mod diagnostics_tests {
  use super::*;
  use crate::api::{SharedRenderDiagnostics, ResourceContext, ResourceKind};
  use crate::error::{Error, ResourceError, Result};
  use crate::geometry::{Point, Size};
  use crate::resource::{FetchedResource, ResourceFetcher};
  use crate::text::font_db::FontDatabase;
  use std::sync::Arc;

  struct MockFetcher {
    handler: Box<dyn Fn(&str) -> Result<FetchedResource> + Send + Sync>,
  }

  impl MockFetcher {
    fn new<F>(handler: F) -> Self
    where
      F: Fn(&str) -> Result<FetchedResource> + Send + Sync + 'static,
    {
      Self {
        handler: Box::new(handler),
      }
    }
  }

  impl ResourceFetcher for MockFetcher {
    fn fetch(&self, url: &str) -> Result<FetchedResource> {
      (self.handler)(url)
    }
  }

  fn test_font_context() -> FontContext {
    FontContext::with_database(Arc::new(FontDatabase::empty()))
  }

  fn test_image_cache(fetcher: Arc<dyn ResourceFetcher>, diagnostics: SharedRenderDiagnostics) -> ImageCache {
    let mut cache = ImageCache::with_fetcher(fetcher);
    cache.set_resource_context(Some(ResourceContext {
      diagnostics: Some(diagnostics),
      ..ResourceContext::default()
    }));
    cache
  }

  #[test]
  fn iframe_fetch_network_error_records_diagnostics() {
    let diagnostics = SharedRenderDiagnostics::new();
    let fetcher = Arc::new(MockFetcher::new(|url| {
      Err(Error::Resource(ResourceError::new(
        url.to_string(),
        "network error".to_string(),
      )))
    }));
    let cache = test_image_cache(fetcher, diagnostics.clone());
    let rect = Rect::new(Point::ZERO, Size::new(10.0, 10.0));

    let result = render_iframe_src(
      "/bad",
      rect,
      None,
      &cache,
      &test_font_context(),
      1.0,
      1,
    );
    assert!(result.is_none());

    let diag = diagnostics.into_inner();
    assert!(
      diag
        .fetch_errors
        .iter()
        .any(|e| e.kind == ResourceKind::Document && e.url == "/bad"),
      "expected iframe fetch error diagnostic"
    );
  }

  #[test]
  fn iframe_http_error_status_records_diagnostics() {
    let diagnostics = SharedRenderDiagnostics::new();
    let fetcher = Arc::new(MockFetcher::new(|_url| {
      let mut resource = FetchedResource::new(b"<html></html>".to_vec(), Some("text/html".to_string()));
      resource.status = Some(403);
      Ok(resource)
    }));
    let cache = test_image_cache(fetcher, diagnostics.clone());
    let rect = Rect::new(Point::ZERO, Size::new(10.0, 10.0));

    let result = render_iframe_src(
      "/bad",
      rect,
      None,
      &cache,
      &test_font_context(),
      1.0,
      1,
    );
    assert!(result.is_none());

    let diag = diagnostics.into_inner();
    let entry = diag
      .fetch_errors
      .iter()
      .find(|e| e.kind == ResourceKind::Document && e.url == "/bad")
      .expect("expected iframe fetch error diagnostic");
    assert_eq!(entry.status, Some(403));
  }

  #[test]
  fn iframe_about_blank_does_not_record_fetch_errors() {
    let diagnostics = SharedRenderDiagnostics::new();
    let fetcher = Arc::new(MockFetcher::new(|url| panic!("unexpected fetch: {url}")));
    let cache = test_image_cache(fetcher, diagnostics.clone());
    let rect = Rect::new(Point::ZERO, Size::new(10.0, 10.0));

    let result = render_iframe_src(
      "about:blank",
      rect,
      None,
      &cache,
      &test_font_context(),
      1.0,
      1,
    );
    assert!(result.is_some());

    let diag = diagnostics.into_inner();
    assert!(
      diag.fetch_errors.is_empty(),
      "expected no diagnostics for about:blank, got {diag:?}"
    );
  }
}
