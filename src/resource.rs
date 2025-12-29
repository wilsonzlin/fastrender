//! Resource fetching abstraction
//!
//! This module provides a trait-based abstraction for fetching external resources
//! (images, CSS, etc.) from various sources. This allows the core library to remain
//! agnostic about how resources are retrieved, enabling:
//!
//! - Custom caching strategies (in test/dev tooling, not the library)
//! - Offline modes
//! - Mocking for tests
//! - Rate limiting
//!
//! # Example
//!
//! ```rust,no_run
//! # use fastrender::resource::{ResourceFetcher, HttpFetcher};
//! # fn main() -> fastrender::Result<()> {
//!
//! let fetcher = HttpFetcher::new();
//! let resource = fetcher.fetch("https://example.com/image.png")?;
//! println!("Got {} bytes", resource.bytes.len());
//! # Ok(())
//! # }
//! ```

use crate::error::{Error, ImageError, ResourceError, Result};
use http::HeaderMap;
use httpdate::parse_http_date;
use lru::LruCache;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::convert::TryFrom;
use std::io;
use std::path::Path;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::sync::Condvar;
use std::sync::Mutex;
use std::time::Duration;
use std::time::SystemTime;
use ureq::ResponseExt;
use url::Url;

pub mod bundle;
mod data_url;
#[cfg(feature = "disk_cache")]
pub mod disk_cache;
#[cfg(feature = "disk_cache")]
pub use disk_cache::{DiskCacheConfig, DiskCachingFetcher};

// ============================================================================
// Origin and resource policy
// ============================================================================

/// Origin model capturing scheme, host, and port.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DocumentOrigin {
  scheme: String,
  host: Option<String>,
  port: Option<u16>,
}

impl DocumentOrigin {
  fn new(scheme: String, host: Option<String>, port: Option<u16>) -> Self {
    Self { scheme, host, port }
  }

  fn from_parsed_url(url: &Url) -> Self {
    let scheme = url.scheme().to_ascii_lowercase();
    let host = url.host_str().map(|h| h.to_ascii_lowercase());
    let port = match scheme.as_str() {
      "http" | "https" => url.port_or_known_default(),
      _ => url.port(),
    };
    Self::new(scheme, host, port)
  }

  /// Return the scheme string for this origin.
  pub fn scheme(&self) -> &str {
    &self.scheme
  }

  /// Host portion of the origin, if present.
  pub fn host(&self) -> Option<&str> {
    self.host.as_deref()
  }

  /// Port portion of the origin, if present.
  pub fn port(&self) -> Option<u16> {
    self.port
  }

  fn effective_port(&self) -> Option<u16> {
    match (self.scheme.as_str(), self.port) {
      ("http", None) => Some(80),
      ("https", None) => Some(443),
      _ => self.port,
    }
  }

  fn same_origin(&self, other: &DocumentOrigin) -> bool {
    if self.scheme != other.scheme {
      return false;
    }
    if self.is_http_like() {
      return self.host == other.host && self.effective_port() == other.effective_port();
    }
    if self.scheme == "file" {
      return true;
    }
    self.host == other.host && self.port == other.port
  }

  /// True for HTTPS documents.
  pub fn is_secure_http(&self) -> bool {
    self.scheme == "https"
  }

  /// True for HTTP/HTTPS documents.
  pub fn is_http_like(&self) -> bool {
    matches!(self.scheme.as_str(), "http" | "https")
  }
}

impl std::fmt::Display for DocumentOrigin {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    if self.scheme == "file" {
      return write!(f, "file://");
    }
    let host = self.host.as_deref().unwrap_or("<unknown>");
    match self.effective_port() {
      Some(port) => write!(f, "{}://{}:{}", self.scheme, host, port),
      None => write!(f, "{}://{}", self.scheme, host),
    }
  }
}

/// Attempt to derive a document origin from a URL string.
pub fn origin_from_url(url: &str) -> Option<DocumentOrigin> {
  let parsed = Url::parse(url).ok()?;
  Some(DocumentOrigin::from_parsed_url(&parsed))
}

/// Policy controlling which subresources can be loaded for a document.
#[derive(Debug, Clone)]
pub struct ResourceAccessPolicy {
  /// Origin of the current document (if known).
  pub document_origin: Option<DocumentOrigin>,
  /// Allow loading file:// resources from HTTP(S) documents.
  pub allow_file_from_http: bool,
  /// Block mixed HTTP content when the document is HTTPS.
  pub block_mixed_content: bool,
  /// Restrict subresources to the document origin unless explicitly allowlisted.
  pub same_origin_only: bool,
  /// Additional origins allowed when enforcing same-origin subresource loading.
  pub allowed_origins: Vec<DocumentOrigin>,
}

impl Default for ResourceAccessPolicy {
  fn default() -> Self {
    Self {
      document_origin: None,
      allow_file_from_http: false,
      block_mixed_content: false,
      same_origin_only: false,
      allowed_origins: Vec::new(),
    }
  }
}

impl ResourceAccessPolicy {
  /// Return a copy of this policy with a different document origin.
  pub fn for_origin(&self, origin: Option<DocumentOrigin>) -> Self {
    let mut cloned = self.clone();
    cloned.document_origin = origin;
    cloned
  }

  /// Check whether a subresource URL is allowed under this policy.
  pub fn allows(&self, target_url: &str) -> std::result::Result<(), PolicyError> {
    self.allows_with_final(target_url, None)
  }

  /// Check whether a subresource URL is allowed, considering any final URL after redirects.
  pub fn allows_with_final(
    &self,
    target_url: &str,
    final_url: Option<&str>,
  ) -> std::result::Result<(), PolicyError> {
    let Some(origin) = &self.document_origin else {
      return Ok(());
    };

    let effective_url = final_url.unwrap_or(target_url);
    let parsed = match Url::parse(effective_url) {
      Ok(parsed) => parsed,
      Err(_) => {
        if self.same_origin_only
          && (effective_url.starts_with("http://") || effective_url.starts_with("https://"))
        {
          return Err(PolicyError {
            reason: format!("Blocked subresource with invalid or missing host: {effective_url}"),
          });
        }
        return Ok(());
      }
    };

    let target_origin = DocumentOrigin::from_parsed_url(&parsed);
    if target_origin.is_http_like() && target_origin.host().is_none() && self.same_origin_only {
      return Err(PolicyError {
        reason: format!("Blocked subresource with missing host: {effective_url}"),
      });
    }

    // Parse the target URL scheme; if unparseable, allow to avoid over-blocking.
    let scheme = parsed.scheme().to_ascii_lowercase();

    if scheme == "data" {
      return Ok(());
    }

    if origin.is_http_like() && scheme == "file" && !self.allow_file_from_http {
      return Err(PolicyError {
        reason: "Blocked file:// resource from HTTP(S) document".to_string(),
      });
    }

    if origin.is_secure_http() && self.block_mixed_content && scheme == "http" {
      return Err(PolicyError {
        reason: "Blocked mixed HTTP content from HTTPS document".to_string(),
      });
    }

    if !self.same_origin_only {
      return Ok(());
    }

    if self
      .allowed_origins
      .iter()
      .any(|allowed| allowed.same_origin(&target_origin))
    {
      return Ok(());
    }

    if origin.same_origin(&target_origin) {
      return Ok(());
    }

    Err(PolicyError {
      reason: format!(
        "Blocked cross-origin resource: document origin {} does not match {} ({})",
        origin, target_origin, effective_url
      ),
    })
  }
}

/// Subresource load blocked by policy.
#[derive(Debug, Clone)]
pub struct PolicyError {
  pub reason: String,
}

/// Normalize a page identifier (full URL or hostname) to a cache/output stem.
///
/// Strips schemes and leading "www.", lowercases the host, and sanitizes for filenames.
pub fn normalize_page_name(raw: &str) -> Option<String> {
  let trimmed = raw.trim();
  if trimmed.is_empty() {
    return None;
  }
  // First try full URL parsing so we can normalize host casing and strip www. reliably.
  if let Ok(parsed) = Url::parse(trimmed) {
    let host = parsed.host_str().unwrap_or("").to_ascii_lowercase();
    let host = host.strip_prefix("www.").unwrap_or(&host);
    // Treat trailing dots as punctuation so inputs like "example.com./path" normalize to the same
    // cache stem as "example.com/path".
    let host = host.trim_end_matches('.');
    let mut stem = String::from(host);
    stem.push_str(&parsed[url::Position::BeforePath..url::Position::AfterQuery]);
    return Some(sanitize_filename(&stem));
  }

  // Fallback: case-insensitive scheme + www stripping for bare hosts or host+path strings.
  let mut without_scheme = trimmed;
  for scheme in ["https://", "http://"] {
    if trimmed.len() >= scheme.len() && trimmed[..scheme.len()].eq_ignore_ascii_case(scheme) {
      without_scheme = &trimmed[scheme.len()..];
      break;
    }
  }

  let without_www = if without_scheme.len() >= 4 && without_scheme[..4].eq_ignore_ascii_case("www.")
  {
    &without_scheme[4..]
  } else {
    without_scheme
  };

  let (host, rest) = match without_www.find('/') {
    Some(idx) => (&without_www[..idx], &without_www[idx..]),
    None => (without_www, ""),
  };

  let host = host.trim_end_matches('.');
  let lowered = format!("{}{}", host.to_ascii_lowercase(), rest);
  let no_fragment = lowered
    .split_once('#')
    .map(|(before, _)| before.to_string())
    .unwrap_or(lowered);

  Some(sanitize_filename(&no_fragment))
}

/// Normalize a URL into a filename-safe stem used for caches and outputs.
pub fn url_to_filename(url: &str) -> String {
  let trimmed = url.trim();
  // First, try to parse the URL so we can lowercase the hostname (case-insensitive per URL
  // spec) and strip the scheme regardless of casing. If parsing fails, fall back to a best-effort
  // scheme-stripping path similar to the old behavior.
  if let Ok(parsed) = Url::parse(trimmed) {
    let mut stem = String::new();
    if let Some(host) = parsed.host_str() {
      stem.push_str(&host.to_ascii_lowercase());
    }
    // Preserve path/query casing while normalizing separators later.
    stem.push_str(&parsed[url::Position::BeforePath..url::Position::AfterQuery]);
    return sanitize_filename(&stem);
  }

  // Fallback: remove common schemes case-insensitively and lowercase only the hostname portion.
  let mut trimmed = trimmed;
  for scheme in ["https://", "http://"] {
    if trimmed.len() >= scheme.len() && trimmed[..scheme.len()].eq_ignore_ascii_case(scheme) {
      trimmed = &trimmed[scheme.len()..];
      break;
    }
  }

  let (host, rest) = match trimmed.find('/') {
    Some(idx) => (&trimmed[..idx], &trimmed[idx..]),
    None => (trimmed, ""),
  };
  let lowered = format!("{}{}", host.to_ascii_lowercase(), rest);
  let no_fragment = lowered
    .split_once('#')
    .map(|(before, _)| before.to_string())
    .unwrap_or(lowered);
  sanitize_filename(&no_fragment)
}

fn sanitize_filename(input: &str) -> String {
  // Trim trailing slashes so we don’t leave a dangling underscore after replacement.
  let trimmed_slash = input.trim_end_matches('/');
  let sanitized: String = trimmed_slash
    .replace('/', "_")
    .chars()
    .map(|c| {
      if c.is_alphanumeric() || c == '.' || c == '_' || c == '-' {
        c
      } else {
        '_'
      }
    })
    .collect();

  // Trim trailing underscores/dots introduced by trailing slashes or punctuation so that
  // "https://example.com/" normalizes to "example.com" instead of "example.com_".
  // If trimming would produce an empty string, fall back to the sanitized value.
  let mut result = sanitized.trim_end_matches(['_', '.']).to_string();
  if result.is_empty() {
    result = sanitized;
  }

  // Avoid a trailing underscore when the path ends with a slash (common for canonical URLs).
  while result.ends_with('_') {
    result.pop();
  }

  result
}

/// Default User-Agent string used by HTTP fetchers
pub const DEFAULT_USER_AGENT: &str =
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0 Safari/537.36 fastrender/0.1";

/// Default Accept-Language header value
pub const DEFAULT_ACCEPT_LANGUAGE: &str = "en-US,en;q=0.9";

/// Allowed URL schemes for resource fetching.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AllowedSchemes {
  /// Whether `http://` URLs are permitted.
  pub http: bool,
  /// Whether `https://` URLs are permitted.
  pub https: bool,
  /// Whether `file://` URLs (and bare filesystem paths) are permitted.
  pub file: bool,
  /// Whether `data:` URLs are permitted.
  pub data: bool,
}

impl AllowedSchemes {
  /// Allow all supported schemes.
  pub const fn all() -> Self {
    Self {
      http: true,
      https: true,
      file: true,
      data: true,
    }
  }

  fn allows(&self, scheme: ResourceScheme) -> bool {
    match scheme {
      ResourceScheme::Http => self.http,
      ResourceScheme::Https => self.https,
      ResourceScheme::File | ResourceScheme::Relative => self.file,
      ResourceScheme::Data => self.data,
      ResourceScheme::Other => false,
    }
  }
}

#[derive(Debug)]
struct ResourceBudget {
  consumed: AtomicUsize,
}

impl ResourceBudget {
  fn new() -> Self {
    Self {
      consumed: AtomicUsize::new(0),
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ResourceScheme {
  Http,
  Https,
  File,
  Data,
  Relative,
  Other,
}

/// Policy controlling which resources may be fetched and how large they may be.
///
/// The policy is clonable; clones share the same underlying byte budget so limits apply across
/// wrapper fetchers (e.g., [`CachingFetcher`]) that share a policy instance.
#[derive(Debug, Clone)]
pub struct ResourcePolicy {
  /// Permitted URL schemes.
  pub allowed_schemes: AllowedSchemes,
  /// Maximum bytes to read per response. Defaults to 50MB.
  pub max_response_bytes: usize,
  /// Total bytes budget across all responses. `None` disables the budget.
  pub total_bytes_budget: Option<usize>,
  /// Per-request timeout.
  pub request_timeout: Duration,
  /// Maximum number of request attempts when following redirects.
  pub max_redirects: usize,
  /// Optional hostname allowlist applied to HTTP(S) requests.
  pub host_allowlist: Option<HashSet<String>>,
  /// Optional hostname denylist applied to HTTP(S) requests.
  pub host_denylist: Option<HashSet<String>>,
  budget: Arc<ResourceBudget>,
}

/// Alias for callers that prefer fetch-oriented terminology.
pub type FetchPolicy = ResourcePolicy;

impl Default for ResourcePolicy {
  fn default() -> Self {
    Self {
      allowed_schemes: AllowedSchemes::all(),
      max_response_bytes: 50 * 1024 * 1024,
      total_bytes_budget: None,
      request_timeout: Duration::from_secs(30),
      max_redirects: 10,
      host_allowlist: None,
      host_denylist: None,
      budget: Arc::new(ResourceBudget::new()),
    }
  }
}

impl ResourcePolicy {
  /// Create a new policy with defaults matching the historical behavior.
  pub fn new() -> Self {
    Self::default()
  }

  /// Override allowed schemes.
  pub fn with_allowed_schemes(mut self, allowed: AllowedSchemes) -> Self {
    self.allowed_schemes = allowed;
    self
  }

  /// Allow or disallow `http://` URLs.
  pub fn allow_http(mut self, allow: bool) -> Self {
    self.allowed_schemes.http = allow;
    self
  }

  /// Allow or disallow `https://` URLs.
  pub fn allow_https(mut self, allow: bool) -> Self {
    self.allowed_schemes.https = allow;
    self
  }

  /// Allow or disallow `file://` URLs and bare filesystem paths.
  pub fn allow_file(mut self, allow: bool) -> Self {
    self.allowed_schemes.file = allow;
    self
  }

  /// Allow or disallow `data:` URLs.
  pub fn allow_data(mut self, allow: bool) -> Self {
    self.allowed_schemes.data = allow;
    self
  }

  /// Override the maximum response size for a single resource.
  pub fn with_max_response_bytes(mut self, max: usize) -> Self {
    self.max_response_bytes = max;
    self
  }

  /// Override the total byte budget across all fetched resources.
  pub fn with_total_bytes_budget(mut self, budget: Option<usize>) -> Self {
    self.total_bytes_budget = budget;
    self.budget = Arc::new(ResourceBudget::new());
    self
  }

  /// Override the per-request timeout.
  pub fn with_request_timeout(mut self, timeout: Duration) -> Self {
    self.request_timeout = timeout;
    self
  }

  /// Override the maximum number of request attempts when following redirects.
  ///
  /// This is the number of HTTP requests allowed per fetch, including the initial request.
  pub fn with_max_redirects(mut self, max_redirects: usize) -> Self {
    self.max_redirects = max_redirects.max(1);
    self
  }

  /// Restrict fetches to the provided hostnames (case-insensitive).
  pub fn with_host_allowlist<I, S>(mut self, hosts: I) -> Self
  where
    I: IntoIterator<Item = S>,
    S: Into<String>,
  {
    self.host_allowlist = normalize_hosts(hosts);
    self
  }

  /// Deny fetches to the provided hostnames (case-insensitive).
  pub fn with_host_denylist<I, S>(mut self, hosts: I) -> Self
  where
    I: IntoIterator<Item = S>,
    S: Into<String>,
  {
    self.host_denylist = normalize_hosts(hosts);
    self
  }

  fn remaining_budget(&self) -> Option<usize> {
    self
      .total_bytes_budget
      .map(|limit| limit.saturating_sub(self.budget.consumed.load(Ordering::Relaxed)))
  }

  fn reserve_budget(&self, amount: usize) -> Result<()> {
    if let Some(limit) = self.total_bytes_budget {
      let mut current = self.budget.consumed.load(Ordering::Relaxed);
      loop {
        let next = current.saturating_add(amount);
        if next > limit {
          return Err(policy_error(format!(
            "total bytes budget exceeded ({} of {} bytes)",
            next, limit
          )));
        }
        match self.budget.consumed.compare_exchange(
          current,
          next,
          Ordering::SeqCst,
          Ordering::SeqCst,
        ) {
          Ok(_) => break,
          Err(actual) => current = actual,
        }
      }
    }
    Ok(())
  }

  fn ensure_url_allowed(&self, url: &str) -> Result<ResourceScheme> {
    let scheme = classify_scheme(url);
    if !self.allowed_schemes.allows(scheme) {
      return Err(policy_error(format!("scheme {:?} is not allowed", scheme)));
    }

    if matches!(scheme, ResourceScheme::Http | ResourceScheme::Https) {
      let parsed = Url::parse(url).map_err(|e| policy_error(format!("invalid URL: {e}")))?;
      let host = parsed.host_str();
      self.check_host_lists(host)?;
    }

    Ok(scheme)
  }

  fn check_host_lists(&self, host: Option<&str>) -> Result<()> {
    let Some(host) = host.map(str::to_ascii_lowercase) else {
      if self.host_allowlist.is_some() {
        return Err(policy_error(
          "host missing and host allowlist is configured",
        ));
      }
      return Ok(());
    };

    if let Some(allowlist) = &self.host_allowlist {
      if !allowlist.contains(&host) {
        return Err(policy_error(format!("host {host} is not in the allowlist")));
      }
    }

    if let Some(denylist) = &self.host_denylist {
      if denylist.contains(&host) {
        return Err(policy_error(format!("host {host} is denied")));
      }
    }

    Ok(())
  }

  fn allowed_response_limit(&self) -> Result<usize> {
    if self.max_response_bytes == 0 {
      return Err(policy_error("max_response_bytes is zero"));
    }

    let limit = match self.remaining_budget() {
      Some(0) => {
        return Err(policy_error("total bytes budget exhausted"));
      }
      Some(remaining) => remaining.min(self.max_response_bytes),
      None => self.max_response_bytes,
    };

    Ok(limit)
  }
}

fn normalize_hosts<I, S>(hosts: I) -> Option<HashSet<String>>
where
  I: IntoIterator<Item = S>,
  S: Into<String>,
{
  let normalized: HashSet<String> = hosts
    .into_iter()
    .map(|h| h.into().to_ascii_lowercase())
    .filter(|h| !h.is_empty())
    .collect();
  if normalized.is_empty() {
    None
  } else {
    Some(normalized)
  }
}

fn classify_scheme(url: &str) -> ResourceScheme {
  let lower = url.to_ascii_lowercase();
  if lower.starts_with("data:") {
    return ResourceScheme::Data;
  }
  if lower.starts_with("file://") {
    return ResourceScheme::File;
  }
  if lower.starts_with("http://") {
    return ResourceScheme::Http;
  }
  if lower.starts_with("https://") {
    return ResourceScheme::Https;
  }

  match Url::parse(url) {
    Ok(parsed) => match parsed.scheme() {
      "http" => ResourceScheme::Http,
      "https" => ResourceScheme::Https,
      "file" => ResourceScheme::File,
      "data" => ResourceScheme::Data,
      _ => ResourceScheme::Other,
    },
    Err(_) => ResourceScheme::Relative,
  }
}

fn policy_error(reason: impl Into<String>) -> Error {
  Error::Other(format!("fetch blocked by policy: {}", reason.into()))
}

/// Strip a leading "User-Agent:" prefix so logs don't double-prefix when callers
/// pass a full header value. Case-insensitive and trims surrounding whitespace after the prefix.
pub fn normalize_user_agent_for_log(ua: &str) -> &str {
  const PREFIX: &str = "user-agent:";
  if ua.len() >= PREFIX.len() && ua[..PREFIX.len()].eq_ignore_ascii_case(PREFIX) {
    let trimmed = ua[PREFIX.len()..].trim();
    if !trimmed.is_empty() {
      return trimmed;
    }
  }
  ua
}

// ============================================================================
// Core types
// ============================================================================

/// Parsed HTTP caching policy extracted from response headers.
#[derive(Debug, Clone, Default)]
pub struct HttpCachePolicy {
  pub max_age: Option<u64>,
  pub no_cache: bool,
  pub no_store: bool,
  pub must_revalidate: bool,
  pub expires: Option<SystemTime>,
}

impl HttpCachePolicy {
  fn is_empty(&self) -> bool {
    self.max_age.is_none()
      && !self.no_cache
      && !self.no_store
      && !self.must_revalidate
      && self.expires.is_none()
  }
}

#[derive(Debug, Clone)]
struct CachedHttpMetadata {
  stored_at: SystemTime,
  max_age: Option<Duration>,
  expires: Option<SystemTime>,
  no_cache: bool,
  no_store: bool,
  must_revalidate: bool,
}

impl CachedHttpMetadata {
  fn from_policy(policy: &HttpCachePolicy, stored_at: SystemTime) -> Option<Self> {
    if policy.is_empty() {
      return None;
    }
    Some(Self {
      stored_at,
      max_age: policy.max_age.map(Duration::from_secs),
      expires: policy.expires,
      no_cache: policy.no_cache,
      no_store: policy.no_store,
      must_revalidate: policy.must_revalidate,
    })
  }

  fn with_updated_policy(
    &self,
    policy: Option<&HttpCachePolicy>,
    stored_at: SystemTime,
  ) -> Option<Self> {
    match policy {
      Some(policy) if policy.no_store => None,
      Some(policy) => CachedHttpMetadata::from_policy(policy, stored_at),
      None => Some(Self {
        stored_at,
        ..self.clone()
      }),
    }
  }

  fn requires_revalidation(&self) -> bool {
    self.no_cache || self.must_revalidate
  }

  fn expires_at(&self) -> Option<SystemTime> {
    if let Some(max_age) = self.max_age {
      return self.stored_at.checked_add(max_age);
    }
    self.expires
  }

  fn is_fresh(&self, now: SystemTime, freshness_cap: Option<Duration>) -> bool {
    let mut expires_at = self.expires_at();
    if let Some(cap) = freshness_cap {
      if let Some(capped) = self.stored_at.checked_add(cap) {
        expires_at = match expires_at {
          Some(exp) if exp < capped => Some(exp),
          _ => Some(capped),
        };
      }
    }
    expires_at.map(|t| t > now).unwrap_or(false)
  }
}

/// Result of fetching an external resource
#[derive(Debug, Clone)]
pub struct FetchedResource {
  /// Raw bytes of the resource
  pub bytes: Vec<u8>,
  /// Content-Type header value, if available (e.g., "image/png", "text/css")
  pub content_type: Option<String>,
  /// HTTP status code when fetched over HTTP(S)
  pub status: Option<u16>,
  /// HTTP ETag header (weak or strong) when present
  pub etag: Option<String>,
  /// HTTP Last-Modified header when present
  pub last_modified: Option<String>,
  /// The final URL after redirects, when available
  pub final_url: Option<String>,
  /// Parsed HTTP caching policy when fetched over HTTP(S).
  pub cache_policy: Option<HttpCachePolicy>,
}

/// Parsed metadata stored alongside cached HTML documents.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct CachedHtmlMetadata {
  pub content_type: Option<String>,
  pub url: Option<String>,
  pub status: Option<u16>,
}

impl FetchedResource {
  /// Create a new FetchedResource
  pub fn new(bytes: Vec<u8>, content_type: Option<String>) -> Self {
    Self {
      bytes,
      content_type,
      status: None,
      etag: None,
      last_modified: None,
      final_url: None,
      cache_policy: None,
    }
  }

  /// Create a new FetchedResource while recording the final URL.
  pub fn with_final_url(
    bytes: Vec<u8>,
    content_type: Option<String>,
    final_url: Option<String>,
  ) -> Self {
    Self {
      bytes,
      content_type,
      status: None,
      etag: None,
      last_modified: None,
      final_url,
      cache_policy: None,
    }
  }

  /// Returns true when this response represents a 304 Not Modified HTTP response.
  pub fn is_not_modified(&self) -> bool {
    matches!(self.status, Some(304))
  }

  /// Check if this resource appears to be an image based on content-type
  pub fn is_image(&self) -> bool {
    self
      .content_type
      .as_ref()
      .map(|ct| ct.starts_with("image/"))
      .unwrap_or(false)
  }

  /// Check if this resource appears to be CSS based on content-type
  pub fn is_css(&self) -> bool {
    self
      .content_type
      .as_ref()
      .map(|ct| ct.contains("text/css"))
      .unwrap_or(false)
  }

  /// Check if this resource appears to be SVG
  pub fn is_svg(&self) -> bool {
    self
      .content_type
      .as_ref()
      .map(|ct| ct.contains("image/svg"))
      .unwrap_or(false)
  }
}

/// Parses cached HTML metadata sidecars.
///
/// Supports the legacy format where the meta file contains only the content-type
/// string, and a key/value format where lines are prefixed with `content-type:`,
/// `status:`, and `url:`.
pub fn parse_cached_html_meta(meta: &str) -> CachedHtmlMetadata {
  let trimmed = meta.trim();
  if trimmed.is_empty() {
    return CachedHtmlMetadata::default();
  }

  let mut parsed = CachedHtmlMetadata::default();

  for line in meta.lines() {
    let mut parts = line.splitn(2, ':');
    let key = parts.next().map(|s| s.trim().to_ascii_lowercase());
    let value = parts.next().map(|s| s.trim());
    match (key.as_deref(), value) {
      (Some("content-type"), Some(v)) if !v.is_empty() => parsed.content_type = Some(v.to_string()),
      (Some("url"), Some(v)) if !v.is_empty() => parsed.url = Some(v.to_string()),
      (Some("status"), Some(v)) => {
        if let Ok(code) = v.parse::<u16>() {
          parsed.status = Some(code);
        }
      }
      _ => {}
    }
  }

  if parsed.content_type.is_none()
    && parsed.url.is_none()
    && parsed.status.is_none()
    && !trimmed.contains('\n')
  {
    return CachedHtmlMetadata {
      content_type: Some(trimmed.to_string()),
      ..CachedHtmlMetadata::default()
    };
  }

  parsed
}

// ============================================================================
// ResourceFetcher trait
// ============================================================================

/// Trait for fetching external resources
///
/// This abstraction allows different fetch implementations:
/// - [`HttpFetcher`]: Default HTTP implementation with timeouts
/// - Custom implementations for caching, mocking, offline mode, etc.
///
/// URLs can be:
/// - `http://` or `https://` - fetch over network
/// - `file://` - read from filesystem
/// - `data:` - decode data URL inline
///
/// # Thread Safety
///
/// Implementations must be `Send + Sync` to allow sharing across threads.
pub trait ResourceFetcher: Send + Sync {
  /// Fetch a resource from the given URL
  ///
  /// # Arguments
  ///
  /// * `url` - The URL to fetch (http://, https://, file://, or data:)
  ///
  /// # Returns
  ///
  /// Returns `Ok(FetchedResource)` containing the bytes and optional content-type,
  /// or an error if the fetch fails.
  fn fetch(&self, url: &str) -> Result<FetchedResource>;

  /// Fetch a resource with optional cache validators for HTTP requests.
  ///
  /// Implementors can ignore the validators if they do not support conditional
  /// requests. The default implementation delegates to [`ResourceFetcher::fetch`].
  fn fetch_with_validation(
    &self,
    url: &str,
    etag: Option<&str>,
    last_modified: Option<&str>,
  ) -> Result<FetchedResource> {
    let _ = (etag, last_modified);
    self.fetch(url)
  }
}

// Allow Arc<dyn ResourceFetcher> to be used as ResourceFetcher
impl<T: ResourceFetcher + ?Sized> ResourceFetcher for Arc<T> {
  fn fetch(&self, url: &str) -> Result<FetchedResource> {
    (**self).fetch(url)
  }

  fn fetch_with_validation(
    &self,
    url: &str,
    etag: Option<&str>,
    last_modified: Option<&str>,
  ) -> Result<FetchedResource> {
    (**self).fetch_with_validation(url, etag, last_modified)
  }
}

// ============================================================================
// HttpFetcher - Default implementation
// ============================================================================

/// Default HTTP resource fetcher
///
/// Fetches resources over HTTP/HTTPS with configurable timeouts and user agent.
/// Also handles `file://` URLs and `data:` URLs.
/// Clones share an internal `ureq::Agent` so connections and TLS state can be reused.
///
/// # Example
///
/// ```rust,no_run
/// use fastrender::resource::HttpFetcher;
/// use std::time::Duration;
///
/// let fetcher = HttpFetcher::new()
///     .with_timeout(Duration::from_secs(60))
///     .with_user_agent("MyApp/1.0");
/// ```
#[derive(Clone)]
pub struct HttpFetcher {
  user_agent: String,
  accept_language: String,
  policy: ResourcePolicy,
  agent: Arc<ureq::Agent>,
}

#[derive(Clone, Copy, Debug)]
struct HttpCacheValidators<'a> {
  etag: Option<&'a str>,
  last_modified: Option<&'a str>,
}

impl std::fmt::Debug for HttpFetcher {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("HttpFetcher")
      .field("user_agent", &self.user_agent)
      .field("accept_language", &self.accept_language)
      .field("policy", &self.policy)
      .finish()
  }
}

impl HttpFetcher {
  fn build_agent(policy: &ResourcePolicy) -> Arc<ureq::Agent> {
    let config = ureq::Agent::config_builder()
      .http_status_as_error(false)
      .timeout_global(Some(policy.request_timeout))
      .build();
    Arc::new(config.into())
  }

  fn rebuild_agent(&mut self) {
    self.agent = Self::build_agent(&self.policy);
  }

  /// Create a new HttpFetcher with default settings
  pub fn new() -> Self {
    Self::default()
  }

  /// Set the request timeout
  pub fn with_timeout(mut self, timeout: Duration) -> Self {
    self.policy.request_timeout = timeout;
    self.rebuild_agent();
    self
  }

  /// Set the User-Agent header
  pub fn with_user_agent(mut self, user_agent: impl Into<String>) -> Self {
    self.user_agent = user_agent.into();
    self
  }

  /// Set the Accept-Language header
  pub fn with_accept_language(mut self, accept_language: impl Into<String>) -> Self {
    self.accept_language = accept_language.into();
    self
  }

  /// Set the maximum response size in bytes
  pub fn with_max_size(mut self, max_size: usize) -> Self {
    self.policy.max_response_bytes = max_size;
    self
  }

  /// Apply a complete resource policy.
  pub fn with_policy(mut self, policy: ResourcePolicy) -> Self {
    self.policy = policy;
    self.rebuild_agent();
    self
  }

  /// Override the maximum redirect attempts.
  pub fn with_max_redirects(mut self, max_redirects: usize) -> Self {
    self.policy.max_redirects = max_redirects.max(1);
    self
  }

  /// Fetch from an HTTP/HTTPS URL
  fn fetch_http(&self, url: &str) -> Result<FetchedResource> {
    self.fetch_http_with_accept(url, None, None)
  }

  fn fetch_http_with_accept(
    &self,
    url: &str,
    accept_encoding: Option<&str>,
    validators: Option<HttpCacheValidators<'_>>,
  ) -> Result<FetchedResource> {
    let mut current = url.to_string();
    let mut validators = validators;
    let agent = &self.agent;
    for _ in 0..self.policy.max_redirects {
      self.policy.ensure_url_allowed(&current)?;
      let allowed_limit = self.policy.allowed_response_limit()? as u64;

      let mut request = agent
        .get(&current)
        .header("User-Agent", &self.user_agent)
        .header("Accept-Language", &self.accept_language)
        .header("Accept-Encoding", "gzip, deflate, br");

      if let Some(v) = validators {
        if let Some(tag) = v.etag {
          request = request.header("If-None-Match", tag);
        }
        if let Some(modified) = v.last_modified {
          request = request.header("If-Modified-Since", modified);
        }
      }

      if let Some(enc) = accept_encoding {
        request = request.header("Accept-Encoding", enc);
      }

      let mut response = match request.call() {
        Ok(resp) => resp,
        Err(err) => {
          let err = ResourceError::new(current.clone(), err.to_string())
            .with_final_url(current.clone())
            .with_source(err);
          return Err(Error::Resource(err));
        }
      };

      let status = response.status();
      if (300..400).contains(&status.as_u16()) {
        if let Some(loc) = response
          .headers()
          .get("location")
          .and_then(|h| h.to_str().ok())
        {
          let next = Url::parse(&current)
            .ok()
            .and_then(|base| base.join(loc).ok())
            .map(|u| u.to_string())
            .unwrap_or_else(|| loc.to_string());
          self.policy.ensure_url_allowed(&next)?;
          current = next;
          validators = None;
          continue;
        }
      }

      let content_type = response
        .headers()
        .get("content-type")
        .and_then(|h| h.to_str().ok())
        .map(|s| s.to_string());
      let etag = response
        .headers()
        .get("etag")
        .and_then(|h| h.to_str().ok())
        .map(|s| s.to_string());
      let last_modified = response
        .headers()
        .get("last-modified")
        .and_then(|h| h.to_str().ok())
        .map(|s| s.to_string());
      let cache_policy = parse_http_cache_policy(response.headers());

      let body_result = response
        .body_mut()
        .with_config()
        .limit(allowed_limit)
        .read_to_vec()
        .map_err(|e| e.into_io());

      match body_result {
        Ok(bytes) => {
          if bytes.is_empty() && status.as_u16() != 304 {
            let err = ResourceError::new(current.clone(), "empty HTTP response body")
              .with_status(status.as_u16())
              .with_final_url(response.get_uri().to_string());
            return Err(Error::Resource(err));
          }
          self.policy.reserve_budget(bytes.len())?;
          let final_url = response.get_uri().to_string();
          let mut resource = FetchedResource::with_final_url(bytes, content_type, Some(final_url));
          resource.status = Some(status.as_u16());
          resource.etag = etag;
          resource.last_modified = last_modified;
          resource.cache_policy = cache_policy;
          return Ok(resource);
        }
        Err(err) if accept_encoding.is_none() && is_decompression_error(&err) => {
          return self.fetch_http_with_accept(&current, Some("identity"), validators);
        }
        Err(err) => {
          let err = ResourceError::new(current.clone(), err.to_string())
            .with_status(status.as_u16())
            .with_final_url(response.get_uri().to_string())
            .with_source(err);
          return Err(Error::Resource(err));
        }
      }
    }

    Err(Error::Resource(
      ResourceError::new(
        url,
        format!("too many redirects (limit {})", self.policy.max_redirects),
      )
      .with_final_url(current),
    ))
  }

  /// Fetch from a file:// URL
  fn fetch_file(&self, url: &str) -> Result<FetchedResource> {
    let path = url.strip_prefix("file://").unwrap_or(url);
    let limit = self.policy.allowed_response_limit()?;
    if let Ok(meta) = std::fs::metadata(path) {
      if let Ok(len) = usize::try_from(meta.len()) {
        if len > limit {
          if let Some(remaining) = self.policy.remaining_budget() {
            if len > remaining {
              return Err(policy_error(format!(
                "total bytes budget exceeded ({} > {} bytes remaining)",
                len, remaining
              )));
            }
          }
          return Err(policy_error(format!(
            "response too large ({} > {} bytes)",
            len, limit
          )));
        }
      } else {
        return Err(policy_error("file is larger than supported limit"));
      }
    }
    let bytes = std::fs::read(path).map_err(|e| {
      Error::Resource(
        ResourceError::new(url.to_string(), e.to_string())
          .with_final_url(url.to_string())
          .with_source(e),
      )
    })?;

    if bytes.len() > limit {
      if let Some(remaining) = self.policy.remaining_budget() {
        if bytes.len() > remaining {
          return Err(policy_error(format!(
            "total bytes budget exceeded ({} > {} bytes remaining)",
            bytes.len(),
            remaining
          )));
        }
      }
      return Err(policy_error(format!(
        "response too large ({} > {} bytes)",
        bytes.len(),
        limit
      )));
    }
    self.policy.reserve_budget(bytes.len())?;

    let content_type = guess_content_type_from_path(path);
    Ok(FetchedResource::with_final_url(
      bytes,
      content_type,
      Some(url.to_string()),
    ))
  }

  /// Decode a data: URL
  fn fetch_data(&self, url: &str) -> Result<FetchedResource> {
    let limit = self.policy.allowed_response_limit()?;
    let resource = data_url::decode_data_url(url)?;
    let len = resource.bytes.len();
    if len > limit {
      if let Some(remaining) = self.policy.remaining_budget() {
        if len > remaining {
          return Err(policy_error(format!(
            "total bytes budget exceeded ({} > {} bytes remaining)",
            len, remaining
          )));
        }
      }
      return Err(policy_error(format!(
        "response too large ({} > {} bytes)",
        len, limit
      )));
    }
    self.policy.reserve_budget(len)?;
    Ok(resource)
  }
}

impl Default for HttpFetcher {
  fn default() -> Self {
    let policy = ResourcePolicy::default();
    Self {
      user_agent: DEFAULT_USER_AGENT.to_string(),
      accept_language: DEFAULT_ACCEPT_LANGUAGE.to_string(),
      agent: Self::build_agent(&policy),
      policy,
    }
  }
}

impl ResourceFetcher for HttpFetcher {
  fn fetch(&self, url: &str) -> Result<FetchedResource> {
    match self.policy.ensure_url_allowed(url)? {
      ResourceScheme::Data => self.fetch_data(url),
      ResourceScheme::File => self.fetch_file(url),
      ResourceScheme::Http | ResourceScheme::Https => self.fetch_http(url),
      ResourceScheme::Relative => self.fetch_file(&format!("file://{}", url)),
      ResourceScheme::Other => Err(policy_error("unsupported URL scheme")),
    }
  }

  fn fetch_with_validation(
    &self,
    url: &str,
    etag: Option<&str>,
    last_modified: Option<&str>,
  ) -> Result<FetchedResource> {
    match self.policy.ensure_url_allowed(url)? {
      ResourceScheme::Http | ResourceScheme::Https => self.fetch_http_with_accept(
        url,
        None,
        Some(HttpCacheValidators {
          etag,
          last_modified,
        }),
      ),
      _ => self.fetch(url),
    }
  }
}

fn parse_http_cache_policy(headers: &HeaderMap) -> Option<HttpCachePolicy> {
  let mut policy = HttpCachePolicy::default();
  if let Some(value) = headers.get("cache-control").and_then(|h| h.to_str().ok()) {
    for directive in value.split(',') {
      let directive = directive.trim();
      if directive.is_empty() {
        continue;
      }
      let (name, value) = directive
        .split_once('=')
        .map(|(k, v)| (k.trim(), Some(v.trim())))
        .unwrap_or((directive, None));
      match name.to_ascii_lowercase().as_str() {
        "max-age" => {
          if let Some(raw) = value {
            let raw = raw.trim_matches('"');
            if let Ok(parsed) = raw.parse::<u64>() {
              policy.max_age = Some(parsed);
            }
          }
        }
        "no-cache" => policy.no_cache = true,
        "no-store" => policy.no_store = true,
        "must-revalidate" => policy.must_revalidate = true,
        _ => {}
      }
    }
  }

  if let Some(expires) = headers
    .get("expires")
    .and_then(|h| h.to_str().ok())
    .and_then(|v| parse_http_date(v).ok())
  {
    policy.expires = Some(expires);
  }

  if policy.is_empty() {
    None
  } else {
    Some(policy)
  }
}

// ============================================================================
// CachingFetcher - in-memory cache + single-flight
// ============================================================================

/// Configuration for [`CachingFetcher`].
#[derive(Debug, Clone, Copy)]
pub struct CachingFetcherConfig {
  /// Maximum total bytes to keep in-memory across cached entries. `0` disables the limit.
  pub max_bytes: usize,
  /// Maximum number of cached entries. `0` disables the limit.
  pub max_items: usize,
  /// Whether to cache failed fetch results (by error string) to avoid hammering endpoints.
  pub cache_errors: bool,
  /// Whether to use HTTP validators (ETag/Last-Modified) when present.
  pub honor_http_cache_headers: bool,
  /// Whether to honor HTTP freshness metadata (Cache-Control/Expires) to avoid revalidation.
  pub honor_http_cache_freshness: bool,
}

impl Default for CachingFetcherConfig {
  fn default() -> Self {
    Self {
      max_bytes: 64 * 1024 * 1024,
      max_items: 512,
      cache_errors: true,
      honor_http_cache_headers: true,
      honor_http_cache_freshness: false,
    }
  }
}

#[derive(Clone)]
enum CacheValue {
  Resource(FetchedResource),
  Error(Error),
}

impl CacheValue {
  fn size(&self) -> usize {
    match self {
      Self::Resource(res) => res.bytes.len(),
      Self::Error(_) => 0,
    }
  }

  fn as_result(&self) -> Result<FetchedResource> {
    match self {
      Self::Resource(res) => Ok(res.clone()),
      Self::Error(err) => Err(err.clone()),
    }
  }
}

#[derive(Clone)]
struct CacheEntry {
  value: CacheValue,
  etag: Option<String>,
  last_modified: Option<String>,
  http_cache: Option<CachedHttpMetadata>,
}

impl CacheEntry {
  fn weight(&self) -> usize {
    self.value.size()
  }
}

#[derive(Clone)]
struct CachedSnapshot {
  value: CacheValue,
  etag: Option<String>,
  last_modified: Option<String>,
  http_cache: Option<CachedHttpMetadata>,
}

impl CachedSnapshot {
  #[cfg(feature = "disk_cache")]
  pub(crate) fn as_resource(&self) -> Option<FetchedResource> {
    match &self.value {
      CacheValue::Resource(res) => Some(res.clone()),
      CacheValue::Error(_) => None,
    }
  }
}

#[derive(Clone)]
pub(crate) enum CacheAction {
  UseCached,
  Validate {
    etag: Option<String>,
    last_modified: Option<String>,
  },
  Fetch,
}

#[derive(Clone)]
pub(crate) struct CachePlan {
  pub(crate) cached: Option<CachedSnapshot>,
  pub(crate) action: CacheAction,
}

#[derive(Debug, Default, Clone)]
pub struct ResourceCacheDiagnostics {
  pub fresh_hits: usize,
  pub revalidated_hits: usize,
  pub misses: usize,
}

thread_local! {
  static RESOURCE_CACHE_DIAGNOSTICS: RefCell<Option<ResourceCacheDiagnostics>> =
    RefCell::new(None);
}

pub(crate) fn enable_resource_cache_diagnostics() {
  RESOURCE_CACHE_DIAGNOSTICS.with(|cell| {
    *cell.borrow_mut() = Some(ResourceCacheDiagnostics::default());
  });
}

pub(crate) fn take_resource_cache_diagnostics() -> Option<ResourceCacheDiagnostics> {
  RESOURCE_CACHE_DIAGNOSTICS.with(|cell| cell.borrow_mut().take())
}

fn record_cache_fresh_hit() {
  RESOURCE_CACHE_DIAGNOSTICS.with(|cell| {
    if let Some(stats) = cell.borrow_mut().as_mut() {
      stats.fresh_hits += 1;
    }
  });
}

fn record_cache_revalidated_hit() {
  RESOURCE_CACHE_DIAGNOSTICS.with(|cell| {
    if let Some(stats) = cell.borrow_mut().as_mut() {
      stats.revalidated_hits += 1;
    }
  });
}

fn record_cache_miss() {
  RESOURCE_CACHE_DIAGNOSTICS.with(|cell| {
    if let Some(stats) = cell.borrow_mut().as_mut() {
      stats.misses += 1;
    }
  });
}

/// Reserve bytes against a configured policy for a resource being returned to a caller.
///
/// The same [`ResourcePolicy`] can be shared across fetchers and caches; clones share the
/// underlying [`ResourceBudget`], so cached hits and cloned results are accounted for even when
/// they avoid network I/O.
fn reserve_policy_bytes(policy: &Option<ResourcePolicy>, resource: &FetchedResource) -> Result<()> {
  if let Some(policy) = policy {
    policy.reserve_budget(resource.bytes.len())?;
  }
  Ok(())
}

struct CacheState {
  lru: LruCache<String, CacheEntry>,
  current_bytes: usize,
  aliases: HashMap<String, String>,
}

impl CacheState {
  fn new() -> Self {
    Self {
      lru: LruCache::unbounded(),
      current_bytes: 0,
      aliases: HashMap::new(),
    }
  }
}

const MAX_ALIAS_HOPS: usize = 8;

#[derive(Clone)]
enum SharedResult {
  Success(FetchedResource),
  Error(Error),
}

impl SharedResult {
  fn as_result(&self) -> Result<FetchedResource> {
    match self {
      Self::Success(res) => Ok(res.clone()),
      Self::Error(err) => Err(err.clone()),
    }
  }
}

struct InFlight {
  result: Mutex<Option<SharedResult>>,
  cv: Condvar,
}

impl InFlight {
  fn new() -> Self {
    Self {
      result: Mutex::new(None),
      cv: Condvar::new(),
    }
  }

  fn set(&self, result: SharedResult) {
    if let Ok(mut slot) = self.result.lock() {
      *slot = Some(result);
      self.cv.notify_all();
    }
  }

  fn wait(&self) -> Result<FetchedResource> {
    let mut guard = self.result.lock().unwrap();
    while guard.is_none() {
      guard = self.cv.wait(guard).unwrap();
    }
    guard.as_ref().unwrap().as_result()
  }
}

/// In-memory caching [`ResourceFetcher`] with LRU eviction and single-flight
/// de-duplication of concurrent requests.
#[derive(Clone)]
pub struct CachingFetcher<F: ResourceFetcher> {
  inner: F,
  state: Arc<Mutex<CacheState>>,
  in_flight: Arc<Mutex<HashMap<String, Arc<InFlight>>>>,
  config: CachingFetcherConfig,
  policy: Option<ResourcePolicy>,
}

impl<F: ResourceFetcher> std::fmt::Debug for CachingFetcher<F> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("CachingFetcher")
      .field("config", &self.config)
      .field("policy", &self.policy)
      .finish_non_exhaustive()
  }
}

impl<F: ResourceFetcher> CachingFetcher<F> {
  /// Creates a new caching wrapper with default limits.
  pub fn new(inner: F) -> Self {
    Self::with_config(inner, CachingFetcherConfig::default())
  }

  /// Creates a new caching wrapper with a custom configuration.
  pub fn with_config(inner: F, config: CachingFetcherConfig) -> Self {
    Self {
      inner,
      state: Arc::new(Mutex::new(CacheState::new())),
      in_flight: Arc::new(Mutex::new(HashMap::new())),
      config,
      policy: None,
    }
  }

  /// Updates the maximum total bytes retained in the cache.
  pub fn with_max_bytes(mut self, max_bytes: usize) -> Self {
    self.config.max_bytes = max_bytes;
    self
  }

  /// Updates the maximum number of cached entries.
  pub fn with_max_items(mut self, max_items: usize) -> Self {
    self.config.max_items = max_items;
    self
  }

  /// Enables or disables caching of failed fetches.
  pub fn with_cache_errors(mut self, cache_errors: bool) -> Self {
    self.config.cache_errors = cache_errors;
    self
  }

  /// Enables or disables conditional requests using cached ETag/Last-Modified headers.
  pub fn with_http_cache_validation(mut self, enabled: bool) -> Self {
    self.config.honor_http_cache_headers = enabled;
    self
  }

  /// Enables or disables honoring HTTP freshness headers (Cache-Control/Expires).
  ///
  /// When enabled, fresh cached entries are served without revalidation. Stale
  /// or `no-cache`/`must-revalidate` entries will trigger conditional requests
  /// when validators are available.
  pub fn with_http_cache_freshness(mut self, enabled: bool) -> Self {
    self.config.honor_http_cache_freshness = enabled;
    self
  }

  /// Apply a resource policy. When set, disallowed URLs are rejected before consulting the cache
  /// and total byte budgets are enforced for cached responses returned to callers.
  pub fn with_policy(mut self, policy: ResourcePolicy) -> Self {
    self.policy = Some(policy);
    self
  }

  fn allowed_alias_target<'a>(
    &self,
    requested: &str,
    final_url: Option<&'a str>,
  ) -> Option<&'a str> {
    let final_url = final_url?;
    if final_url == requested {
      return None;
    }
    if let Some(policy) = &self.policy {
      if policy.ensure_url_allowed(final_url).is_err() {
        return None;
      }
    }
    Some(final_url)
  }

  fn canonical_url(&self, requested: &str, final_url: Option<&str>) -> String {
    self
      .allowed_alias_target(requested, final_url)
      .unwrap_or(requested)
      .to_string()
  }

  fn plan_cache_use(
    &self,
    url: &str,
    cached: Option<CachedSnapshot>,
    freshness_cap: Option<Duration>,
  ) -> CachePlan {
    let is_http = matches!(
      classify_scheme(url),
      ResourceScheme::Http | ResourceScheme::Https
    );
    let Some(snapshot) = cached else {
      return CachePlan {
        cached: None,
        action: CacheAction::Fetch,
      };
    };

    let has_validators = snapshot.etag.is_some() || snapshot.last_modified.is_some();
    let http_cache = snapshot.http_cache.clone();
    let etag = snapshot.etag.clone();
    let last_modified = snapshot.last_modified.clone();
    let cached = Some(snapshot);

    if !is_http {
      return CachePlan {
        cached,
        action: CacheAction::UseCached,
      };
    }

    if self.config.honor_http_cache_freshness {
      if let Some(meta) = http_cache.as_ref() {
        if meta.no_store {
          return CachePlan {
            cached: None,
            action: CacheAction::Fetch,
          };
        }
        if meta.is_fresh(SystemTime::now(), freshness_cap) && !meta.requires_revalidation() {
          return CachePlan {
            cached,
            action: CacheAction::UseCached,
          };
        }
        if self.config.honor_http_cache_headers && has_validators {
          return CachePlan {
            cached,
            action: CacheAction::Validate {
              etag,
              last_modified,
            },
          };
        }
        return CachePlan {
          cached,
          action: CacheAction::Fetch,
        };
      } else if !self.config.honor_http_cache_headers {
        return CachePlan {
          cached,
          action: CacheAction::UseCached,
        };
      }
    }

    if self.config.honor_http_cache_headers && has_validators {
      CachePlan {
        cached,
        action: CacheAction::Validate {
          etag,
          last_modified,
        },
      }
    } else {
      CachePlan {
        cached,
        action: CacheAction::UseCached,
      }
    }
  }

  fn insert_canonical_locked(&self, state: &mut CacheState, url: &str, entry: CacheEntry) {
    if let Some(existing) = state.lru.peek(url) {
      state.current_bytes = state.current_bytes.saturating_sub(existing.weight());
    }
    state.current_bytes = state.current_bytes.saturating_add(entry.weight());
    state.aliases.remove(url);
    state.lru.put(url.to_string(), entry);
    self.evict_locked(state);
  }

  fn set_alias_locked(&self, state: &mut CacheState, alias: &str, canonical: &str) {
    if alias == canonical {
      state.aliases.remove(alias);
      return;
    }

    let mut target = canonical.to_string();
    let mut hops = 0usize;
    let mut visited: HashSet<String> = HashSet::new();
    while let Some(next) = state.aliases.get(&target) {
      if hops >= MAX_ALIAS_HOPS || !visited.insert(target.clone()) {
        return;
      }
      target = next.clone();
      hops += 1;
    }

    if target == alias {
      return;
    }

    state.aliases.insert(alias.to_string(), target);
  }

  fn remove_aliases_targeting(&self, state: &mut CacheState, key: &str) {
    state
      .aliases
      .retain(|alias, target| alias != key && target != key);
  }

  fn cache_entry(&self, requested_url: &str, entry: CacheEntry, final_url: Option<&str>) -> String {
    if self.config.max_bytes > 0 && entry.weight() > self.config.max_bytes {
      return self.canonical_url(requested_url, final_url);
    }

    let canonical = self.canonical_url(requested_url, final_url);

    if let Ok(mut state) = self.state.lock() {
      self.insert_canonical_locked(&mut state, &canonical, entry);
      if requested_url != canonical {
        self.set_alias_locked(&mut state, requested_url, &canonical);
      }
    }

    canonical
  }

  fn remove_cached(&self, url: &str) {
    if let Ok(mut state) = self.state.lock() {
      let canonical = self.resolve_alias_locked(&mut state, url);
      if let Some((_k, entry)) = state.lru.pop_entry(&canonical) {
        state.current_bytes = state.current_bytes.saturating_sub(entry.weight());
      }
      self.remove_aliases_targeting(&mut state, &canonical);
      if canonical != url {
        state.aliases.remove(url);
      }
    }
  }

  fn build_cache_entry(
    &self,
    resource: &FetchedResource,
    stored_at: SystemTime,
  ) -> Option<CacheEntry> {
    if resource
      .cache_policy
      .as_ref()
      .map(|p| p.no_store)
      .unwrap_or(false)
    {
      return None;
    }

    let http_cache = resource
      .cache_policy
      .as_ref()
      .and_then(|policy| CachedHttpMetadata::from_policy(policy, stored_at));

    Some(CacheEntry {
      etag: resource.etag.clone(),
      last_modified: resource.last_modified.clone(),
      http_cache,
      value: CacheValue::Resource(resource.clone()),
    })
  }

  fn evict_locked(&self, state: &mut CacheState) {
    while (self.config.max_items > 0 && state.lru.len() > self.config.max_items)
      || (self.config.max_bytes > 0 && state.current_bytes > self.config.max_bytes)
    {
      if let Some((key, entry)) = state.lru.pop_lru() {
        state.current_bytes = state.current_bytes.saturating_sub(entry.weight());
        self.remove_aliases_targeting(state, &key);
      } else {
        break;
      }
    }
  }

  fn resolve_alias_locked(&self, state: &mut CacheState, url: &str) -> String {
    let origin = url.to_string();
    let mut current = origin.clone();
    let mut hops = 0usize;
    let mut visited: HashSet<String> = HashSet::new();
    let mut removed = false;

    while let Some(next) = state.aliases.get(&current).cloned() {
      if hops >= MAX_ALIAS_HOPS || !visited.insert(current.clone()) || next == current {
        state.aliases.remove(&origin);
        removed = true;
        break;
      }
      current = next;
      hops += 1;
    }

    if !removed && current != origin {
      state.aliases.insert(origin, current.clone());
    }

    current
  }

  fn cached_entry(&self, url: &str) -> Option<CachedSnapshot> {
    self
      .state
      .lock()
      .ok()
      .and_then(|mut state| {
        let canonical = self.resolve_alias_locked(&mut state, url);
        let snapshot = state.lru.get(&canonical).cloned();
        if snapshot.is_none() && canonical != url {
          state.aliases.remove(url);
        }
        snapshot
      })
      .map(|entry| CachedSnapshot {
        value: entry.value,
        etag: entry.etag,
        last_modified: entry.last_modified,
        http_cache: entry.http_cache,
      })
  }

  #[cfg(feature = "disk_cache")]
  pub(crate) fn cached_snapshot(&self, url: &str) -> Option<CachedSnapshot> {
    self.cached_entry(url)
  }

  #[cfg(feature = "disk_cache")]
  pub(crate) fn prime_cache_with_snapshot(&self, url: &str, snapshot: CachedSnapshot) -> String {
    let final_url = match &snapshot.value {
      CacheValue::Resource(res) => res.final_url.as_deref(),
      CacheValue::Error(_) => None,
    };
    self.cache_entry(
      url,
      CacheEntry {
        etag: snapshot.etag,
        last_modified: snapshot.last_modified,
        http_cache: snapshot.http_cache,
        value: snapshot.value,
      },
      final_url,
    )
  }

  #[cfg(feature = "disk_cache")]
  pub(crate) fn prime_cache_with_resource(&self, url: &str, resource: FetchedResource) {
    let stored_at = SystemTime::now();
    if let Some(entry) = self.build_cache_entry(&resource, stored_at) {
      self.cache_entry(url, entry, resource.final_url.as_deref());
    }
  }

  fn join_inflight(&self, url: &str) -> (Arc<InFlight>, bool) {
    let mut map = self.in_flight.lock().unwrap();
    if let Some(existing) = map.get(url) {
      return (Arc::clone(existing), false);
    }

    let flight = Arc::new(InFlight::new());
    map.insert(url.to_string(), Arc::clone(&flight));
    (flight, true)
  }

  fn finish_inflight(&self, url: &str, flight: &Arc<InFlight>, result: SharedResult) {
    flight.set(result);
    if let Ok(mut map) = self.in_flight.lock() {
      map.remove(url);
    }
  }
}

impl<F: ResourceFetcher> ResourceFetcher for CachingFetcher<F> {
  fn fetch(&self, url: &str) -> Result<FetchedResource> {
    if let Some(policy) = &self.policy {
      policy.ensure_url_allowed(url)?;
    }

    let cached = self.cached_entry(url);
    let plan = self.plan_cache_use(url, cached.clone(), None);
    if let CacheAction::UseCached = plan.action {
      if let Some(snapshot) = plan.cached.as_ref() {
        record_cache_fresh_hit();
        let result = snapshot.value.as_result();
        if let Ok(ref res) = result {
          reserve_policy_bytes(&self.policy, res)?;
        }
        return result;
      }
    }

    let (flight, is_owner) = self.join_inflight(url);
    if !is_owner {
      let result = flight.wait();
      if let Ok(ref res) = result {
        reserve_policy_bytes(&self.policy, res)?;
      }
      return result;
    }

    let validators = match &plan.action {
      CacheAction::Validate {
        etag,
        last_modified,
      } => Some((etag.as_deref(), last_modified.as_deref())),
      _ => None,
    };

    let fetch_result = match validators {
      Some((etag, last_modified)) => self.inner.fetch_with_validation(url, etag, last_modified),
      None => self.inner.fetch(url),
    };

    let (mut result, charge_budget) = match fetch_result {
      Ok(res) => {
        if res.is_not_modified() {
          if let Some(snapshot) = plan.cached.as_ref() {
            let value = snapshot.value.as_result();
            if let Ok(ref ok) = value {
              let stored_at = SystemTime::now();
              let should_store = !res
                .cache_policy
                .as_ref()
                .map(|p| p.no_store)
                .unwrap_or(false);
              let updated_meta = snapshot
                .http_cache
                .as_ref()
                .and_then(|meta| meta.with_updated_policy(res.cache_policy.as_ref(), stored_at))
                .or_else(|| {
                  res
                    .cache_policy
                    .as_ref()
                    .and_then(|policy| CachedHttpMetadata::from_policy(policy, stored_at))
                });

              if should_store {
                self.cache_entry(
                  url,
                  CacheEntry {
                    value: CacheValue::Resource(ok.clone()),
                    etag: res.etag.clone().or_else(|| snapshot.etag.clone()),
                    last_modified: res
                      .last_modified
                      .clone()
                      .or_else(|| snapshot.last_modified.clone()),
                    http_cache: updated_meta,
                  },
                  ok.final_url.as_deref(),
                );
              } else {
                self.remove_cached(url);
              }
            }
            record_cache_revalidated_hit();
            let is_ok = value.is_ok();
            (value, is_ok)
          } else {
            (
              Err(Error::Resource(
                ResourceError::new(url.to_string(), "received 304 without cached entry")
                  .with_final_url(url.to_string()),
              )),
              false,
            )
          }
        } else {
          let stored_at = SystemTime::now();
          if let Some(entry) = self.build_cache_entry(&res, stored_at) {
            self.cache_entry(url, entry, res.final_url.as_deref());
          } else if res
            .cache_policy
            .as_ref()
            .map(|p| p.no_store)
            .unwrap_or(false)
          {
            self.remove_cached(url);
          }
          record_cache_miss();
          (Ok(res), false)
        }
      }
      Err(err) => {
        if let Some(snapshot) = plan.cached.as_ref() {
          let fallback = snapshot.value.as_result();
          let is_ok = fallback.is_ok();
          (fallback, is_ok)
        } else {
          if self.config.cache_errors {
            self.cache_entry(
              url,
              CacheEntry {
                value: CacheValue::Error(err.clone()),
                etag: None,
                last_modified: None,
                http_cache: None,
              },
              None,
            );
          }
          (Err(err), false)
        }
      }
    };

    if charge_budget {
      if let Ok(ref res) = result {
        if let Err(err) = reserve_policy_bytes(&self.policy, res) {
          result = Err(err);
        }
      }
    }

    let notify = match &result {
      Ok(res) => SharedResult::Success(res.clone()),
      Err(err) => SharedResult::Error(err.clone()),
    };
    self.finish_inflight(url, &flight, notify);

    result
  }
}

fn is_decompression_error(err: &io::Error) -> bool {
  if let Some(inner) = err.get_ref() {
    if let Some(ureq_err) = inner.downcast_ref::<ureq::Error>() {
      return matches!(ureq_err, ureq::Error::Decompress(_, _));
    }
  }

  err.to_string().contains("decompression failed")
}

// ============================================================================
// Helper functions
// ============================================================================

/// Guess content-type from file path extension
fn guess_content_type_from_path(path: &str) -> Option<String> {
  let ext = Path::new(path)
    .extension()
    .and_then(|e| e.to_str())
    .map(|e| e.to_lowercase())?;

  let mime = match ext.as_str() {
    "png" => "image/png",
    "jpg" | "jpeg" => "image/jpeg",
    "gif" => "image/gif",
    "webp" => "image/webp",
    "avif" => "image/avif",
    "svg" => "image/svg+xml",
    "ico" => "image/x-icon",
    "bmp" => "image/bmp",
    "css" => "text/css",
    "html" | "htm" => "text/html",
    "js" => "application/javascript",
    "json" => "application/json",
    "woff" => "font/woff",
    "woff2" => "font/woff2",
    "ttf" => "font/ttf",
    "otf" => "font/otf",
    _ => return None,
  };

  Some(mime.to_string())
}

/// Decode a data: URL into bytes
fn decode_data_url(url: &str) -> Result<FetchedResource> {
  if !url.starts_with("data:") {
    return Err(Error::Image(ImageError::InvalidDataUrl {
      reason: "URL does not start with 'data:'".to_string(),
    }));
  }

  data_url::decode_data_url(url)
}
// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
  use super::data_url;
  use super::*;
  use std::collections::VecDeque;
  use std::io;
  use std::io::Read;
  use std::io::Write;
  use std::net::TcpListener;
  use std::net::TcpStream;
  use std::sync::atomic::AtomicUsize;
  use std::sync::atomic::Ordering;
  use std::sync::Arc;
  use std::sync::Barrier;
  use std::sync::Mutex;
  use std::thread;

  fn try_bind_localhost(context: &str) -> Option<TcpListener> {
    match TcpListener::bind("127.0.0.1:0") {
      Ok(listener) => Some(listener),
      Err(err) if err.kind() == std::io::ErrorKind::PermissionDenied => {
        eprintln!("skipping {context}: cannot bind localhost in this environment: {err}");
        None
      }
      Err(err) => panic!("bind {context}: {err}"),
    }
  }

  #[test]
  fn url_to_filename_normalizes_scheme_and_slashes() {
    assert_eq!(
      url_to_filename("https://example.com/foo/bar"),
      "example.com_foo_bar"
    );
    assert_eq!(url_to_filename("http://example.com"), "example.com");
  }

  #[test]
  fn url_to_filename_strips_www_and_replaces_invalid_chars() {
    assert_eq!(
      url_to_filename("https://www.exa mple.com/path?x=1"),
      "www.exa_mple.com_path_x_1"
    );
  }

  #[test]
  fn url_to_filename_trims_trailing_slash() {
    assert_eq!(url_to_filename("https://example.com/"), "example.com");
    assert_eq!(
      url_to_filename("HTTP://WWW.Example.COM/"),
      "www.example.com"
    );
  }

  #[test]
  fn url_to_filename_trims_whitespace_and_fragment() {
    assert_eq!(
      url_to_filename("  https://Example.com/Path#Frag  "),
      "example.com_Path"
    );
  }

  #[test]
  fn url_to_filename_trims_trailing_punctuation() {
    assert_eq!(url_to_filename("https://example.com./"), "example.com");
    assert_eq!(url_to_filename("https://example.com_"), "example.com");
  }

  #[test]
  fn url_to_filename_is_case_insensitive_for_scheme_and_host() {
    assert_eq!(
      url_to_filename("HTTP://WWW.Example.COM/Path/Up"),
      "www.example.com_Path_Up"
    );
  }

  #[test]
  fn url_to_filename_parses_uppercase_scheme() {
    assert_eq!(
      url_to_filename("HTTPS://Example.com/q?a=1"),
      "example.com_q_a_1"
    );
  }

  #[test]
  fn url_to_filename_trims_trailing_slashes() {
    assert_eq!(url_to_filename("https://example.com/"), "example.com");
    assert_eq!(
      url_to_filename("https://example.com/foo/"),
      "example.com_foo"
    );
  }

  #[test]
  fn normalize_page_name_handles_urls_and_hosts() {
    assert_eq!(
      normalize_page_name("https://www.w3.org").as_deref(),
      Some("w3.org")
    );
    assert_eq!(
      normalize_page_name("http://example.com/foo").as_deref(),
      Some("example.com_foo")
    );
    assert_eq!(
      normalize_page_name("example.com").as_deref(),
      Some("example.com")
    );
  }

  #[test]
  fn normalize_page_name_rejects_empty() {
    assert!(normalize_page_name("").is_none());
    assert!(normalize_page_name("   ").is_none());
  }

  #[test]
  fn normalize_page_name_strips_fragment_and_whitespace() {
    assert_eq!(
      normalize_page_name("  https://example.com/path#section  ").as_deref(),
      Some("example.com_path")
    );
  }

  #[test]
  fn normalize_page_name_handles_query_and_uppercase_scheme() {
    assert_eq!(
      normalize_page_name("HTTP://Example.com/Path?foo=1").as_deref(),
      Some("example.com_Path_foo_1")
    );
  }

  #[test]
  fn normalize_page_name_handles_uppercase_host_and_trailing_slash() {
    assert_eq!(
      normalize_page_name("HTTP://WWW.Example.COM/").as_deref(),
      Some("example.com")
    );
  }

  #[test]
  fn normalize_page_name_strips_trailing_punctuation_with_whitespace() {
    assert_eq!(
      normalize_page_name("  HTTPS://Example.com./  ").as_deref(),
      Some("example.com")
    );
  }

  #[test]
  fn normalize_page_name_trims_trailing_punctuation() {
    assert_eq!(
      normalize_page_name("https://example.com./").as_deref(),
      Some("example.com")
    );
    assert_eq!(
      normalize_page_name("example.com_").as_deref(),
      Some("example.com")
    );
  }

  #[test]
  fn normalize_user_agent_for_log_strips_prefix_and_whitespace() {
    assert_eq!(
      normalize_user_agent_for_log("User-Agent: Foo/1.0"),
      "Foo/1.0"
    );
    assert_eq!(
      normalize_user_agent_for_log("User-Agent:   Foo/1.0   "),
      "Foo/1.0"
    );
    assert_eq!(
      normalize_user_agent_for_log("user-agent: Foo/1.0"),
      "Foo/1.0"
    );
    assert_eq!(
      normalize_user_agent_for_log("USER-AGENT:Foo/1.0"),
      "Foo/1.0"
    );
    assert_eq!(normalize_user_agent_for_log("Mozilla/5.0"), "Mozilla/5.0");
    assert_eq!(normalize_user_agent_for_log(""), "");
  }

  #[test]
  fn normalize_page_name_strips_www_case_insensitively() {
    assert_eq!(
      normalize_page_name("WWW.Example.com").as_deref(),
      Some("example.com")
    );
    assert_eq!(
      normalize_page_name("www.example.com").as_deref(),
      Some("example.com")
    );
  }

  #[test]
  fn url_to_filename_drops_fragments() {
    assert_eq!(
      url_to_filename("https://example.com/path#frag"),
      "example.com_path"
    );
    assert_eq!(
      url_to_filename("https://example.com/path?q=1#frag"),
      "example.com_path_q_1"
    );
  }

  #[test]
  fn url_to_filename_drops_fragments_without_scheme() {
    assert_eq!(url_to_filename("Example.Com/path#frag"), "example.com_path");
    assert_eq!(
      url_to_filename("Example.Com/path?q=1#frag"),
      "example.com_path_q_1"
    );
  }

  #[test]
  fn url_to_filename_strips_fragments_with_uppercase_scheme() {
    assert_eq!(
      url_to_filename("HTTP://Example.com/Path#Frag"),
      "example.com_Path"
    );
    assert_eq!(
      url_to_filename("HTTP://Example.com/Path?q=1#Frag"),
      "example.com_Path_q_1"
    );
  }

  #[test]
  fn url_to_filename_handles_data_urls() {
    // Url::parse understands data: URLs, so the scheme is stripped and we sanitize the payload.
    assert_eq!(url_to_filename("data:text/html,<p>hi"), "text_html__p_hi");
  }

  #[test]
  fn sanitize_filename_trims_trailing_separators_and_punctuation() {
    assert_eq!(sanitize_filename("example.com///"), "example.com");
    assert_eq!(sanitize_filename("foo_bar.."), "foo_bar");
  }

  #[test]
  fn fetched_resource_new_defaults_metadata() {
    let resource = FetchedResource::new(vec![1, 2, 3], Some("text/plain".to_string()));

    assert_eq!(resource.status, None);
    assert_eq!(resource.etag, None);
    assert_eq!(resource.last_modified, None);
    assert_eq!(resource.final_url, None);
  }

  #[test]
  fn fetched_resource_with_final_url_sets_only_final_url() {
    let resource = FetchedResource::with_final_url(
      b"bytes".to_vec(),
      Some("text/plain".to_string()),
      Some("http://example.com/data".to_string()),
    );

    assert_eq!(resource.status, None);
    assert_eq!(resource.etag, None);
    assert_eq!(resource.last_modified, None);
    assert_eq!(
      resource.final_url.as_deref(),
      Some("http://example.com/data")
    );
  }

  #[test]
  fn test_fetched_resource_is_image() {
    let resource = FetchedResource::new(vec![], Some("image/png".to_string()));
    assert!(resource.is_image());

    let resource = FetchedResource::new(vec![], Some("text/css".to_string()));
    assert!(!resource.is_image());
  }

  #[test]
  fn test_fetched_resource_is_css() {
    let resource = FetchedResource::new(vec![], Some("text/css".to_string()));
    assert!(resource.is_css());

    let resource = FetchedResource::new(vec![], Some("text/css; charset=utf-8".to_string()));
    assert!(resource.is_css());
  }

  #[test]
  fn test_guess_content_type() {
    assert_eq!(
      guess_content_type_from_path("/path/to/image.png"),
      Some("image/png".to_string())
    );
    assert_eq!(
      guess_content_type_from_path("/path/to/style.CSS"),
      Some("text/css".to_string())
    );
    assert_eq!(guess_content_type_from_path("/path/to/file"), None);
  }

  #[test]
  fn test_decode_data_url_base64() {
    let url = "data:image/png;base64,aGVsbG8="; // "hello" in base64
    let resource = data_url::decode_data_url(url).unwrap();
    assert_eq!(resource.bytes, b"hello");
    assert_eq!(resource.content_type, Some("image/png".to_string()));
  }

  #[test]
  fn test_decode_data_url_percent() {
    let url = "data:text/plain,hello%20world";
    let resource = data_url::decode_data_url(url).unwrap();
    assert_eq!(resource.bytes, b"hello world");
    assert_eq!(resource.content_type, Some("text/plain".to_string()));
  }

  #[test]
  fn test_decode_data_url_no_mediatype() {
    let url = "data:,hello";
    let resource = data_url::decode_data_url(url).unwrap();
    assert_eq!(resource.bytes, b"hello");
    assert_eq!(
      resource.content_type,
      Some("text/plain;charset=US-ASCII".to_string())
    );
  }

  #[test]
  fn data_url_default_mediatype_can_be_overridden_by_parameter() {
    let resource = data_url::decode_data_url("data:;charset=utf-8,hi").unwrap();
    assert_eq!(resource.bytes, b"hi");
    assert_eq!(
      resource.content_type,
      Some("text/plain;charset=utf-8".to_string())
    );
  }

  #[test]
  fn data_url_plus_is_literal() {
    let resource = data_url::decode_data_url("data:,a+b").unwrap();
    assert_eq!(resource.bytes, b"a+b");
    assert_eq!(
      resource.content_type,
      Some("text/plain;charset=US-ASCII".to_string())
    );
  }

  #[test]
  fn data_url_percent_decoding_roundtrips_hex() {
    let resource = data_url::decode_data_url("data:text/plain,a%2Bb%20c").unwrap();
    assert_eq!(resource.bytes, b"a+b c");
  }

  #[test]
  fn data_url_rejects_malformed_percent_escape() {
    let err = data_url::decode_data_url("data:text/plain,abc%2").unwrap_err();
    assert!(matches!(
      err,
      Error::Image(ImageError::InvalidDataUrl { ref reason })
        if reason.contains("Incomplete percent-escape")
    ));

    let err = data_url::decode_data_url("data:text/plain,%2G").unwrap_err();
    assert!(matches!(
      err,
      Error::Image(ImageError::InvalidDataUrl { ref reason })
        if reason.contains("Invalid percent-escape")
    ));
  }

  #[test]
  fn data_url_base64_is_case_insensitive_and_tolerates_whitespace() {
    let url = "data:text/plain;BASE64,aGVs bG8=\n";
    let resource = data_url::decode_data_url(url).unwrap();
    assert_eq!(resource.bytes, b"hello");
    assert_eq!(resource.content_type, Some("text/plain".to_string()));
  }

  #[test]
  fn data_url_preserves_additional_parameters() {
    let url = "data:text/plain;charset=UTF-8;param=value;base64,aA==";
    let resource = data_url::decode_data_url(url).unwrap();
    assert_eq!(resource.bytes, b"h");
    assert_eq!(
      resource.content_type,
      Some("text/plain;charset=UTF-8;param=value".to_string())
    );
  }

  #[test]
  fn parse_cached_meta_supports_legacy_content_type() {
    let meta = parse_cached_html_meta("text/html; charset=utf-8");
    assert_eq!(
      meta.content_type.as_deref(),
      Some("text/html; charset=utf-8")
    );
    assert_eq!(meta.url, None);
    assert_eq!(meta.status, None);
  }

  #[test]
  fn parse_cached_meta_reads_key_value_lines() {
    let meta = "content-type: text/html\nurl: https://example.com/page\n";
    let parsed = parse_cached_html_meta(meta);
    assert_eq!(parsed.content_type.as_deref(), Some("text/html"));
    assert_eq!(parsed.url.as_deref(), Some("https://example.com/page"));
    assert_eq!(parsed.status, None);
  }

  #[test]
  fn parse_cached_meta_reads_status_code() {
    let meta = "content-type: text/html\nstatus: 302\nurl: https://example.com/page\n";
    let parsed = parse_cached_html_meta(meta);
    assert_eq!(parsed.content_type.as_deref(), Some("text/html"));
    assert_eq!(parsed.url.as_deref(), Some("https://example.com/page"));
    assert_eq!(parsed.status, Some(302));
  }

  #[test]
  fn http_fetcher_follows_redirects() {
    let Some(listener) = try_bind_localhost("http_fetcher_follows_redirects") else {
      return;
    };
    let addr = listener.local_addr().unwrap();
    let handle = thread::spawn(move || {
      let mut conn_count = 0;
      for stream in listener.incoming() {
        let mut stream = stream.unwrap();
        conn_count += 1;
        let mut buf = [0u8; 1024];
        let _ = stream.read(&mut buf);

        if conn_count == 1 {
          let resp = format!(
            "HTTP/1.1 302 Found\r\nLocation: http://{}\r\nContent-Length: 0\r\n\r\n",
            addr
          );
          let _ = stream.write_all(resp.as_bytes());
        } else {
          let body = b"ok";
          let headers = format!(
            "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: {}\r\n\r\n",
            body.len()
          );
          let _ = stream.write_all(headers.as_bytes());
          let _ = stream.write_all(body);
          break;
        }
      }
    });

    let fetcher = HttpFetcher::new().with_timeout(Duration::from_secs(5));
    let url = format!("http://{}/", addr);
    let res = fetcher.fetch(&url).expect("fetch redirect");
    handle.join().unwrap();

    assert_eq!(res.bytes, b"ok");
    assert_eq!(res.content_type, Some("text/plain".to_string()));
    assert!(
      res
        .final_url
        .as_deref()
        .unwrap_or("")
        .starts_with(&format!("http://{}", addr)),
      "final_url should record redirect destination: {:?}",
      res.final_url
    );
  }

  #[test]
  fn http_fetcher_sets_accept_language() {
    let Some(listener) = try_bind_localhost("http_fetcher_sets_accept_language") else {
      return;
    };
    let addr = listener.local_addr().unwrap();
    let captured = Arc::new(Mutex::new(String::new()));
    let captured_req = Arc::clone(&captured);
    let handle = thread::spawn(move || {
      if let Some(stream) = listener.incoming().next() {
        let mut stream = stream.unwrap();
        let mut buf = [0u8; 1024];
        let _ = stream.read(&mut buf);
        let req = String::from_utf8_lossy(&buf);
        if let Ok(mut slot) = captured_req.lock() {
          *slot = req.to_string();
        }

        let body = b"hi";
        let headers = format!(
          "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: {}\r\n\r\n",
          body.len()
        );
        let _ = stream.write_all(headers.as_bytes());
        let _ = stream.write_all(body);
      }
    });

    let fetcher = HttpFetcher::new();
    let url = format!("http://{}/", addr);
    let res = fetcher.fetch(&url).expect("fetch lang");
    handle.join().unwrap();

    assert_eq!(res.bytes, b"hi");
    let req = captured.lock().unwrap().to_lowercase();
    assert!(
      req.contains("accept-language: en-us,en;q=0.9"),
      "missing header: {}",
      req
    );
  }

  #[test]
  fn http_fetcher_reuses_keep_alive_connections() {
    let Some(listener) = try_bind_localhost("http_fetcher_reuses_keep_alive_connections") else {
      return;
    };
    let addr = listener.local_addr().unwrap();

    let handle = thread::spawn(move || {
      let mut conn_count = 0;
      let mut handled = 0;

      while handled < 2 {
        let (mut stream, _) = listener.accept().unwrap();
        conn_count += 1;
        stream
          .set_read_timeout(Some(Duration::from_millis(500)))
          .unwrap();

        loop {
          match read_http_request(&mut stream) {
            Ok(Some(_)) => {
              let body = b"pong";
              let headers = format!(
                "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: {}\r\nConnection: keep-alive\r\n\r\n",
                body.len()
              );
              stream.write_all(headers.as_bytes()).unwrap();
              stream.write_all(body).unwrap();
              handled += 1;
              if handled >= 2 {
                return conn_count;
              }
            }
            Ok(None) => break,
            Err(_) => break,
          }
        }
      }

      conn_count
    });

    let fetcher = HttpFetcher::new().with_timeout(Duration::from_secs(2));
    let url = format!("http://{}/", addr);
    fetcher.fetch(&url).expect("first fetch");
    fetcher.fetch(&url).expect("second fetch");

    let conn_count = handle.join().unwrap();
    assert_eq!(
      conn_count, 1,
      "expected keep-alive reuse across fetches (got {conn_count})"
    );
  }

  fn read_http_request(stream: &mut TcpStream) -> io::Result<Option<Vec<u8>>> {
    let mut buf = [0u8; 1024];
    let mut request = Vec::new();
    loop {
      match stream.read(&mut buf) {
        Ok(0) => return Ok(None),
        Ok(n) => {
          request.extend_from_slice(&buf[..n]);
          if request.windows(4).any(|w| w == b"\r\n\r\n") {
            return Ok(Some(request));
          }
        }
        Err(ref e)
          if matches!(
            e.kind(),
            io::ErrorKind::WouldBlock | io::ErrorKind::TimedOut
          ) =>
        {
          if request.is_empty() {
            return Ok(None);
          }
          return Err(io::Error::new(
            e.kind(),
            "incomplete HTTP request before timeout",
          ));
        }
        Err(e) => return Err(e),
      }
    }
  }

  #[test]
  fn test_http_fetcher_defaults() {
    let fetcher = HttpFetcher::new();
    assert_eq!(fetcher.policy.request_timeout, Duration::from_secs(30));
    assert!(fetcher.user_agent.contains("fastrender"));
  }

  #[test]
  fn test_http_fetcher_builder() {
    let fetcher = HttpFetcher::new()
      .with_timeout(Duration::from_secs(60))
      .with_user_agent("Test/1.0")
      .with_max_size(1024);

    assert_eq!(fetcher.policy.request_timeout, Duration::from_secs(60));
    assert_eq!(fetcher.user_agent, "Test/1.0");
    assert_eq!(fetcher.policy.max_response_bytes, 1024);
  }

  #[test]
  fn http_fetcher_rebuilds_agent_and_shares_across_clones() {
    let fetcher = HttpFetcher::new();
    let original_agent = Arc::clone(&fetcher.agent);

    let cloned = fetcher.clone();
    assert_eq!(Arc::as_ptr(&original_agent), Arc::as_ptr(&cloned.agent));

    let updated = fetcher.with_timeout(Duration::from_millis(50));
    assert_ne!(Arc::as_ptr(&original_agent), Arc::as_ptr(&updated.agent));
    assert_eq!(updated.policy.request_timeout, Duration::from_millis(50));

    let updated_clone = updated.clone();
    assert_eq!(
      Arc::as_ptr(&updated.agent),
      Arc::as_ptr(&updated_clone.agent)
    );

    let updated_agent = Arc::clone(&updated.agent);
    let policy = ResourcePolicy::default().with_request_timeout(Duration::from_millis(75));
    let rebuilt = updated.with_policy(policy);
    assert_eq!(rebuilt.policy.request_timeout, Duration::from_millis(75));
    assert_ne!(Arc::as_ptr(&updated_agent), Arc::as_ptr(&rebuilt.agent));
  }

  #[test]
  fn http_fetcher_is_send_and_sync() {
    fn assert_bounds<T: Send + Sync>() {}
    assert_bounds::<HttpFetcher>();
  }

  #[test]
  fn resource_policy_blocks_disallowed_scheme() {
    let fetcher = HttpFetcher::new().with_policy(
      ResourcePolicy::default()
        .allow_http(false)
        .allow_https(false),
    );
    let err = fetcher.fetch("http://example.test/").unwrap_err();
    assert!(
      err.to_string().contains("not allowed"),
      "unexpected error: {err:?}"
    );
  }

  #[test]
  fn resource_policy_blocks_denied_host() {
    let fetcher = HttpFetcher::new()
      .with_policy(ResourcePolicy::default().with_host_denylist(["example.test"]));
    let err = fetcher.fetch("http://example.test/blocked").unwrap_err();
    assert!(
      err.to_string().contains("denied"),
      "unexpected error: {err:?}"
    );
  }

  #[test]
  fn resource_policy_enforces_response_size_for_data_urls() {
    let fetcher =
      HttpFetcher::new().with_policy(ResourcePolicy::default().with_max_response_bytes(4));
    let err = fetcher
      .fetch("data:text/plain,hello")
      .expect_err("fetch should fail due to size limit");
    assert!(
      err.to_string().contains("response too large"),
      "unexpected error: {err:?}"
    );
  }

  #[test]
  fn resource_policy_tracks_total_budget() {
    let fetcher =
      HttpFetcher::new().with_policy(ResourcePolicy::default().with_total_bytes_budget(Some(5)));
    fetcher
      .fetch("data:text/plain,abc")
      .expect("first fetch within budget");
    let err = fetcher
      .fetch("data:text/plain,def")
      .expect_err("budget should be exhausted");
    assert!(
      err.to_string().contains("budget"),
      "unexpected error: {err:?}"
    );
  }

  #[test]
  fn caching_fetcher_counts_cached_hits_against_budget() {
    let policy = ResourcePolicy::default().with_total_bytes_budget(Some(5));
    let fetcher =
      CachingFetcher::new(HttpFetcher::new().with_policy(policy.clone())).with_policy(policy);
    let url = "data:text/plain,abcd";

    let first = fetcher.fetch(url).expect("first fetch within budget");
    assert_eq!(first.bytes, b"abcd");

    let err = fetcher
      .fetch(url)
      .expect_err("cached fetch should honor total budget");
    assert!(
      err.to_string().contains("budget"),
      "unexpected error: {err:?}"
    );
  }

  #[test]
  fn test_fetch_data_url() {
    let fetcher = HttpFetcher::new();
    let resource = fetcher.fetch("data:text/plain,test").unwrap();
    assert_eq!(resource.bytes, b"test");
  }

  #[test]
  fn fetch_http_retries_on_bad_gzip() {
    use std::io::Read;
    use std::io::Write;
    use std::thread;
    use std::time::Duration;
    use std::time::Instant;

    let Some(listener) = try_bind_localhost("fetch_http_retries_on_bad_gzip") else {
      return;
    };
    let addr = listener.local_addr().unwrap();
    listener.set_nonblocking(true).unwrap();

    let handle = thread::spawn(move || {
      let mut handled = 0;
      let start = Instant::now();
      while handled < 2 && start.elapsed() < Duration::from_secs(5) {
        match listener.accept() {
          Ok((mut stream, _)) => {
            let mut buf = [0u8; 1024];
            let mut req = Vec::new();
            loop {
              match stream.read(&mut buf) {
                Ok(0) => break,
                Ok(n) => {
                  req.extend_from_slice(&buf[..n]);
                  if req.windows(4).any(|w| w == b"\r\n\r\n") {
                    break;
                  }
                }
                Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => {
                  thread::sleep(Duration::from_millis(10));
                  continue;
                }
                Err(_) => break,
              }
            }

            let req_str = String::from_utf8_lossy(&req).to_lowercase();
            let body = b"hello world";
            let encoding_header = if req_str.contains("accept-encoding: identity") {
              ""
            } else {
              "Content-Encoding: gzip\r\n"
            };
            let response = format!(
                            "HTTP/1.1 200 OK\r\n{}Content-Length: {}\r\nContent-Type: text/plain\r\nConnection: close\r\n\r\n",
                            encoding_header,
                            body.len()
                        );
            stream.write_all(response.as_bytes()).unwrap();
            stream.write_all(body).unwrap();
            handled += 1;
          }
          Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => {
            thread::sleep(Duration::from_millis(10));
          }
          Err(_) => break,
        }
      }
    });

    let fetcher = HttpFetcher::new().with_timeout(Duration::from_secs(2));
    let url = format!("http://{}", addr);
    let resource = fetcher.fetch(&url).unwrap();
    assert_eq!(resource.bytes, b"hello world");

    handle.join().unwrap();
  }

  #[test]
  fn fetch_http_errors_on_empty_body() {
    use std::io::Write;

    let Some(listener) = try_bind_localhost("fetch_http_errors_on_empty_body") else {
      return;
    };
    let addr = listener.local_addr().unwrap();

    let handle = std::thread::spawn(move || {
      if let Some(stream) = listener.incoming().next() {
        let mut stream = stream.unwrap();
        let headers = b"HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nContent-Length: 0\r\n\r\n";
        let _ = stream.write_all(headers);
      }
    });

    let fetcher = HttpFetcher::new().with_timeout(Duration::from_secs(2));
    let url = format!("http://{}", addr);
    let res = fetcher.fetch(&url);
    assert!(res.is_err(), "expected empty response to error: {res:?}");

    handle.join().unwrap();
  }

  #[test]
  fn normalize_user_agent_for_log_strips_prefix() {
    assert_eq!(normalize_user_agent_for_log("User-Agent: Foo"), "Foo");
    assert_eq!(normalize_user_agent_for_log("Foo"), "Foo");
    assert_eq!(normalize_user_agent_for_log(""), "");
  }

  #[derive(Clone)]
  struct CountingFetcher {
    count: Arc<AtomicUsize>,
    body: Vec<u8>,
  }

  impl ResourceFetcher for CountingFetcher {
    fn fetch(&self, _url: &str) -> Result<FetchedResource> {
      self.count.fetch_add(1, Ordering::SeqCst);
      Ok(FetchedResource::new(
        self.body.clone(),
        Some("text/plain".to_string()),
      ))
    }
  }

  #[test]
  fn caching_fetcher_hits_cache_and_evicts() {
    let counter = Arc::new(AtomicUsize::new(0));
    let inner = CountingFetcher {
      count: Arc::clone(&counter),
      body: b"hello".to_vec(),
    };

    let cache = CachingFetcher::new(inner)
      .with_max_items(1)
      .with_max_bytes(8);

    let _ = cache.fetch("http://example.com/a").unwrap();
    assert_eq!(counter.load(Ordering::SeqCst), 1);

    // Second fetch should hit cache
    let _ = cache.fetch("http://example.com/a").unwrap();
    assert_eq!(counter.load(Ordering::SeqCst), 1);

    // Different URL should evict previous entry due to max_items = 1
    let _ = cache.fetch("http://example.com/b").unwrap();
    assert_eq!(counter.load(Ordering::SeqCst), 2);

    // First URL should require a re-fetch after eviction
    let _ = cache.fetch("http://example.com/a").unwrap();
    assert_eq!(counter.load(Ordering::SeqCst), 3);
  }

  #[test]
  fn caching_fetcher_coalesces_inflight_requests() {
    #[derive(Clone)]
    struct SlowFetcher {
      count: Arc<AtomicUsize>,
    }

    impl ResourceFetcher for SlowFetcher {
      fn fetch(&self, _url: &str) -> Result<FetchedResource> {
        self.count.fetch_add(1, Ordering::SeqCst);
        std::thread::sleep(std::time::Duration::from_millis(50));
        Ok(FetchedResource::new(
          b"ok".to_vec(),
          Some("text/plain".to_string()),
        ))
      }
    }

    let counter = Arc::new(AtomicUsize::new(0));
    let fetcher = SlowFetcher {
      count: Arc::clone(&counter),
    };
    let cache = Arc::new(CachingFetcher::new(fetcher));
    let barrier = Arc::new(Barrier::new(4));

    let mut handles = Vec::new();
    for _ in 0..4 {
      let f = Arc::clone(&cache);
      let b = Arc::clone(&barrier);
      handles.push(thread::spawn(move || {
        b.wait();
        f.fetch("http://example.com/shared").unwrap();
      }));
    }

    for handle in handles {
      handle.join().unwrap();
    }

    assert_eq!(counter.load(Ordering::SeqCst), 1);
  }

  #[test]
  fn caching_fetcher_can_cache_errors() {
    #[derive(Clone)]
    struct ErrorFetcher {
      count: Arc<AtomicUsize>,
    }

    impl ResourceFetcher for ErrorFetcher {
      fn fetch(&self, _url: &str) -> Result<FetchedResource> {
        self.count.fetch_add(1, Ordering::SeqCst);
        Err(Error::Other("boom".to_string()))
      }
    }

    let counter = Arc::new(AtomicUsize::new(0));
    let fetcher = ErrorFetcher {
      count: Arc::clone(&counter),
    };
    let cache = CachingFetcher::new(fetcher).with_cache_errors(true);

    assert!(cache.fetch("http://example.com/error").is_err());
    assert!(cache.fetch("http://example.com/error").is_err());
    assert_eq!(counter.load(Ordering::SeqCst), 1);
  }

  #[derive(Clone, Debug)]
  struct MockResponse {
    status: u16,
    body: Vec<u8>,
    etag: Option<String>,
    last_modified: Option<String>,
    cache_policy: Option<HttpCachePolicy>,
  }

  #[derive(Clone, Debug)]
  struct MockCall {
    url: String,
    etag: Option<String>,
    last_modified: Option<String>,
  }

  #[derive(Clone)]
  struct ScriptedFetcher {
    responses: Arc<Mutex<VecDeque<MockResponse>>>,
    calls: Arc<Mutex<Vec<MockCall>>>,
  }

  impl ScriptedFetcher {
    fn new(responses: Vec<MockResponse>) -> Self {
      Self {
        responses: Arc::new(Mutex::new(VecDeque::from(responses))),
        calls: Arc::new(Mutex::new(Vec::new())),
      }
    }

    fn record_call(&self, url: &str, etag: Option<&str>, last_modified: Option<&str>) {
      let mut calls = self.calls.lock().unwrap();
      calls.push(MockCall {
        url: url.to_string(),
        etag: etag.map(|s| s.to_string()),
        last_modified: last_modified.map(|s| s.to_string()),
      });
    }

    fn next_response(&self) -> Result<FetchedResource> {
      let mut responses = self.responses.lock().unwrap();
      let resp = responses
        .pop_front()
        .expect("scripted fetcher ran out of responses");

      let mut resource = FetchedResource::new(resp.body.clone(), Some("text/plain".to_string()));
      resource.status = Some(resp.status);
      resource.etag = resp.etag.clone();
      resource.last_modified = resp.last_modified.clone();
      resource.cache_policy = resp.cache_policy.clone();
      Ok(resource)
    }

    fn calls(&self) -> Vec<MockCall> {
      self.calls.lock().unwrap().clone()
    }
  }

  impl ResourceFetcher for ScriptedFetcher {
    fn fetch(&self, url: &str) -> Result<FetchedResource> {
      self.record_call(url, None, None);
      self.next_response()
    }

    fn fetch_with_validation(
      &self,
      url: &str,
      etag: Option<&str>,
      last_modified: Option<&str>,
    ) -> Result<FetchedResource> {
      self.record_call(url, etag, last_modified);
      self.next_response()
    }
  }

  #[test]
  fn caching_fetcher_uses_cached_body_on_not_modified() {
    let fetcher = ScriptedFetcher::new(vec![
      MockResponse {
        status: 200,
        body: b"cached".to_vec(),
        etag: Some("etag1".to_string()),
        last_modified: Some("lm1".to_string()),
        cache_policy: None,
      },
      MockResponse {
        status: 304,
        body: Vec::new(),
        etag: None,
        last_modified: None,
        cache_policy: None,
      },
    ]);

    let cache = CachingFetcher::new(fetcher.clone());
    let url = "http://example.com/resource";

    let first = cache.fetch(url).expect("initial fetch");
    assert_eq!(first.bytes, b"cached");

    let second = cache.fetch(url).expect("revalidated fetch");
    assert_eq!(second.bytes, b"cached", "should return cached body on 304");

    let calls = fetcher.calls();
    assert_eq!(calls.len(), 2);
    assert_eq!(calls[0].etag, None);
    assert_eq!(calls[1].etag.as_deref(), Some("etag1"));
    assert_eq!(calls[1].last_modified.as_deref(), Some("lm1"));
  }

  #[test]
  fn caching_fetcher_updates_validators_on_not_modified() {
    let fetcher = ScriptedFetcher::new(vec![
      MockResponse {
        status: 200,
        body: b"cached".to_vec(),
        etag: Some("etag1".to_string()),
        last_modified: Some("lm1".to_string()),
        cache_policy: None,
      },
      MockResponse {
        status: 304,
        body: Vec::new(),
        etag: Some("etag2".to_string()),
        last_modified: Some("lm2".to_string()),
        cache_policy: None,
      },
      MockResponse {
        status: 304,
        body: Vec::new(),
        etag: None,
        last_modified: None,
        cache_policy: None,
      },
    ]);

    let cache = CachingFetcher::new(fetcher.clone());
    let url = "http://example.com/resource";

    let first = cache.fetch(url).expect("initial fetch");
    assert_eq!(first.bytes, b"cached");

    let second = cache.fetch(url).expect("first revalidation");
    assert_eq!(second.bytes, b"cached");

    let third = cache.fetch(url).expect("second revalidation");
    assert_eq!(third.bytes, b"cached");

    let calls = fetcher.calls();
    assert_eq!(calls.len(), 3);
    assert_eq!(calls[1].etag.as_deref(), Some("etag1"));
    assert_eq!(calls[1].last_modified.as_deref(), Some("lm1"));
    assert_eq!(
      calls[2].etag.as_deref(),
      Some("etag2"),
      "validators should update after 304 with new etag"
    );
    assert_eq!(calls[2].last_modified.as_deref(), Some("lm2"));
  }

  #[test]
  fn honors_fresh_cache_control_without_revalidating() {
    let fetcher = ScriptedFetcher::new(vec![
      MockResponse {
        status: 200,
        body: b"cached".to_vec(),
        etag: Some("etag1".to_string()),
        last_modified: Some("lm1".to_string()),
        cache_policy: Some(HttpCachePolicy {
          max_age: Some(3600),
          ..Default::default()
        }),
      },
      MockResponse {
        status: 304,
        body: b"stale".to_vec(),
        etag: None,
        last_modified: None,
        cache_policy: None,
      },
    ]);

    let cache = CachingFetcher::with_config(
      fetcher.clone(),
      CachingFetcherConfig {
        honor_http_cache_freshness: true,
        ..CachingFetcherConfig::default()
      },
    );
    let url = "http://example.com/fresh";

    let first = cache.fetch(url).expect("initial fetch");
    assert_eq!(first.bytes, b"cached");

    let second = cache.fetch(url).expect("cached fetch");
    assert_eq!(second.bytes, b"cached");
    assert_eq!(
      fetcher.calls().len(),
      1,
      "fresh cached entries should not be revalidated"
    );
  }

  #[test]
  fn no_cache_policy_triggers_validation_even_when_fresh() {
    let fetcher = ScriptedFetcher::new(vec![
      MockResponse {
        status: 200,
        body: b"cached".to_vec(),
        etag: Some("etag1".to_string()),
        last_modified: Some("lm1".to_string()),
        cache_policy: Some(HttpCachePolicy {
          max_age: Some(3600),
          no_cache: true,
          ..Default::default()
        }),
      },
      MockResponse {
        status: 304,
        body: Vec::new(),
        etag: None,
        last_modified: None,
        cache_policy: None,
      },
    ]);

    let cache = CachingFetcher::with_config(
      fetcher.clone(),
      CachingFetcherConfig {
        honor_http_cache_freshness: true,
        ..CachingFetcherConfig::default()
      },
    );
    let url = "http://example.com/no-cache";

    cache.fetch(url).expect("initial fetch");
    cache.fetch(url).expect("revalidated fetch");

    let calls = fetcher.calls();
    assert_eq!(calls.len(), 2);
    assert_eq!(calls[0].etag, None);
    assert_eq!(
      calls[1].etag.as_deref(),
      Some("etag1"),
      "no-cache should still force conditional requests when validators exist"
    );
  }

  #[test]
  fn freshness_cap_limits_http_freshness() {
    let cache = CachingFetcher::with_config(
      ScriptedFetcher::new(vec![]),
      CachingFetcherConfig {
        honor_http_cache_freshness: true,
        ..CachingFetcherConfig::default()
      },
    );

    let stored_at = SystemTime::now()
      .checked_sub(Duration::from_secs(2))
      .unwrap();
    let http_cache = CachedHttpMetadata {
      stored_at,
      max_age: Some(Duration::from_secs(3600)),
      expires: None,
      no_cache: false,
      no_store: false,
      must_revalidate: false,
    };
    let snapshot = CachedSnapshot {
      value: CacheValue::Resource(FetchedResource::new(
        b"cached".to_vec(),
        Some("text/plain".to_string()),
      )),
      etag: Some("etag1".to_string()),
      last_modified: Some("lm1".to_string()),
      http_cache: Some(http_cache),
    };

    let plan = cache.plan_cache_use(
      "http://example.com/capped",
      Some(snapshot),
      Some(Duration::from_secs(1)),
    );

    assert!(
      matches!(plan.action, CacheAction::Validate { .. }),
      "freshness cap should force validation even when HTTP max-age is longer"
    );
  }

  #[test]
  fn caching_fetcher_aliases_final_url() {
    #[derive(Clone)]
    struct RedirectingFetcher {
      calls: Arc<Mutex<Vec<String>>>,
      target: String,
    }

    impl ResourceFetcher for RedirectingFetcher {
      fn fetch(&self, url: &str) -> Result<FetchedResource> {
        self.calls.lock().unwrap().push(url.to_string());
        let mut res = FetchedResource::new(b"alias".to_vec(), Some("text/plain".to_string()));
        res.final_url = Some(self.target.clone());
        Ok(res)
      }
    }

    let calls = Arc::new(Mutex::new(Vec::new()));
    let target = "http://example.com/final".to_string();
    let fetcher = RedirectingFetcher {
      calls: Arc::clone(&calls),
      target: target.clone(),
    };
    let cache = CachingFetcher::new(fetcher);

    let initial = cache
      .fetch("http://example.com/start")
      .expect("first fetch");
    assert_eq!(initial.final_url.as_deref(), Some(target.as_str()));

    let second = cache.fetch(&target).expect("aliased fetch");
    assert_eq!(second.bytes, initial.bytes);
    assert_eq!(calls.lock().unwrap().len(), 1, "alias should hit cache");
  }
}
