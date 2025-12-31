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

use crate::error::{Error, ImageError, RenderError, RenderStage, ResourceError, Result};
use crate::render_control::{self, check_active_periodic};
use brotli::Decompressor;
use flate2::read::{DeflateDecoder, GzDecoder, ZlibDecoder};
use http::HeaderMap;
use httpdate::parse_http_date;
use lru::LruCache;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::convert::TryFrom;
use std::io::{self, Cursor, Read};
use std::path::Path;
use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering};
use std::sync::Arc;
use std::sync::Condvar;
use std::sync::Mutex;
use std::sync::OnceLock;
use std::thread;
use std::time::Duration;
use std::time::Instant;
use std::time::SystemTime;
use std::time::UNIX_EPOCH;
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

  /// Check whether a document URL is allowed under this policy, ignoring same-origin restrictions.
  pub fn allows_document(&self, target_url: &str) -> std::result::Result<(), PolicyError> {
    self.allows_document_with_final(target_url, None)
  }

  /// Check whether a subresource URL is allowed, considering any final URL after redirects.
  pub fn allows_with_final(
    &self,
    target_url: &str,
    final_url: Option<&str>,
  ) -> std::result::Result<(), PolicyError> {
    self.allows_internal(target_url, final_url, self.same_origin_only)
  }

  /// Check whether a document URL is allowed, considering any final URL after redirects, while
  /// skipping same-origin enforcement.
  pub fn allows_document_with_final(
    &self,
    target_url: &str,
    final_url: Option<&str>,
  ) -> std::result::Result<(), PolicyError> {
    self.allows_internal(target_url, final_url, false)
  }

  fn allows_internal(
    &self,
    target_url: &str,
    final_url: Option<&str>,
    enforce_same_origin: bool,
  ) -> std::result::Result<(), PolicyError> {
    let Some(origin) = &self.document_origin else {
      return Ok(());
    };

    let effective_url = final_url.unwrap_or(target_url);
    let parsed = match Url::parse(effective_url) {
      Ok(parsed) => parsed,
      Err(_) => {
        if enforce_same_origin
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
    if target_origin.is_http_like() && target_origin.host().is_none() && enforce_same_origin {
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

    if !enforce_same_origin {
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
    None => match without_www.find('_') {
      // When called with an already-sanitized cache stem (e.g. `example.com_Path`), `without_www`
      // will not contain `/` separators. Treat the first underscore as the boundary between the
      // hostname and the rest of the stem so we preserve case-sensitive path/query segments.
      //
      // This keeps normalization idempotent for `normalize_page_name` outputs and matches how the
      // CLI tools name cached pages.
      Some(idx) => (&without_www[..idx], &without_www[idx..]),
      None => (without_www, ""),
    },
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

/// Default Accept header value.
///
/// This is intentionally "browser-like" while remaining broadly compatible with non-HTML
/// subresource requests (it includes `*/*`).
const DEFAULT_ACCEPT: &str = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8";

/// Content-Encoding algorithms this fetcher can decode.
const SUPPORTED_ACCEPT_ENCODING: &str = "gzip, deflate, br";

/// Safety buffer subtracted from deadline-derived HTTP timeouts.
///
/// When rendering under a cooperative timeout, we must ensure that HTTP fetches never extend past
/// the remaining render budget. Leaving a small buffer gives the renderer time to unwind and
/// return a structured timeout/error before any external hard-kill triggers.
const HTTP_DEADLINE_BUFFER: Duration = Duration::from_millis(25);

/// Stride for cooperative deadline checks while decoding compressed bodies.
const CONTENT_DECODE_DEADLINE_STRIDE: usize = 16;

/// Retry/backoff policy for [`HttpFetcher`].
#[derive(Debug, Clone)]
pub struct HttpRetryPolicy {
  /// Total number of attempts (initial request + retries).
  ///
  /// Set to `1` to disable retries.
  pub max_attempts: usize,
  /// Base delay used for exponential backoff.
  pub backoff_base: Duration,
  /// Maximum delay between retries.
  pub backoff_cap: Duration,
  /// When true, honor `Retry-After` for retryable responses.
  ///
  /// `backoff_cap` still caps the computed exponential backoff, but `Retry-After` can exceed it.
  /// Any active render deadline (timeout) remains the final cap so we never sleep past the
  /// remaining budget.
  pub respect_retry_after: bool,
}

impl Default for HttpRetryPolicy {
  fn default() -> Self {
    Self {
      max_attempts: 6,
      backoff_base: Duration::from_millis(200),
      backoff_cap: Duration::from_secs(2),
      respect_retry_after: true,
    }
  }
}

fn retryable_http_status(status: u16) -> bool {
  is_transient_http_status(status)
}

fn is_transient_http_status(status: u16) -> bool {
  matches!(status, 202 | 429 | 500 | 502 | 503 | 504)
}

fn http_status_allows_empty_body(status: u16) -> bool {
  matches!(status, 204 | 205 | 304) || (100..200).contains(&status)
}

fn http_retry_logging_enabled() -> bool {
  static ENABLED: OnceLock<bool> = OnceLock::new();
  *ENABLED.get_or_init(|| {
    std::env::var("FASTR_HTTP_LOG_RETRIES")
      .ok()
      .map(|raw| {
        !matches!(
          raw.trim().to_ascii_lowercase().as_str(),
          "0" | "false" | "no" | "off"
        )
      })
      .unwrap_or(false)
  })
}

fn log_http_retry(reason: &str, attempt: usize, max_attempts: usize, url: &str, backoff: Duration) {
  if !http_retry_logging_enabled() {
    return;
  }
  eprintln!(
    "http retry {attempt}/{max_attempts} {url}: {reason} (sleep {}ms)",
    backoff.as_millis()
  );
}

fn format_attempt_suffix(attempt: usize, max_attempts: usize) -> String {
  if max_attempts <= 1 {
    String::new()
  } else {
    format!(" (attempt {attempt}/{max_attempts})")
  }
}

fn parse_retry_after(headers: &HeaderMap) -> Option<Duration> {
  let value = headers.get("retry-after")?.to_str().ok()?.trim();
  if value.is_empty() {
    return None;
  }
  if let Ok(secs) = value.parse::<u64>() {
    return Some(Duration::from_secs(secs));
  }
  parse_http_date(value)
    .ok()
    .and_then(|when| when.duration_since(SystemTime::now()).ok())
}

fn hash_u64(input: &str) -> u64 {
  // 64-bit FNV-1a.
  let mut hash: u64 = 0xcbf29ce484222325;
  for &b in input.as_bytes() {
    hash ^= u64::from(b);
    hash = hash.wrapping_mul(0x100000001b3);
  }
  hash
}

fn pseudo_rand_u64(mut x: u64) -> u64 {
  // xorshift64*
  x ^= x >> 12;
  x ^= x << 25;
  x ^= x >> 27;
  x.wrapping_mul(0x2545F4914F6CDD1D)
}

fn jitter_duration(max: Duration, seed: u64) -> Duration {
  if max.is_zero() {
    return Duration::ZERO;
  }
  let max_ns = max.as_nanos();
  // Avoid division by zero / overflow in the modulo below.
  let denom = max_ns.saturating_add(1);
  let rand = pseudo_rand_u64(seed) as u128;
  let jitter_ns = rand % denom;
  let secs = (jitter_ns / 1_000_000_000) as u64;
  let nanos = (jitter_ns % 1_000_000_000) as u32;
  Duration::new(secs, nanos)
}

fn compute_backoff(policy: &HttpRetryPolicy, retry_count: usize, url: &str) -> Duration {
  // Exponential backoff with "equal jitter":
  //   sleep = backoff/2 + rand(0..backoff/2)
  let retry_count = retry_count.max(1);
  let shift = (retry_count - 1).min(30) as u32;
  let factor = 1u32 << shift;
  let backoff = policy
    .backoff_base
    .checked_mul(factor)
    .unwrap_or(policy.backoff_cap)
    .min(policy.backoff_cap);
  let half = backoff / 2;
  let now = SystemTime::now()
    .duration_since(UNIX_EPOCH)
    .map(|d| d.as_nanos() as u64)
    .unwrap_or(0);
  let seed = now ^ hash_u64(url) ^ (retry_count as u64).wrapping_mul(0x9E3779B97F4A7C15);
  half + jitter_duration(half, seed)
}

fn is_retryable_io_error(err: &io::Error) -> bool {
  matches!(
    err.kind(),
    io::ErrorKind::TimedOut
      | io::ErrorKind::ConnectionReset
      | io::ErrorKind::ConnectionAborted
      | io::ErrorKind::NotConnected
      | io::ErrorKind::BrokenPipe
      | io::ErrorKind::UnexpectedEof
      | io::ErrorKind::WouldBlock
      | io::ErrorKind::Interrupted
  )
}

fn is_retryable_ureq_error(err: &ureq::Error) -> bool {
  let mut source: Option<&(dyn std::error::Error + 'static)> = Some(err);
  while let Some(current) = source {
    if let Some(io_err) = current.downcast_ref::<io::Error>() {
      if is_retryable_io_error(io_err) {
        return true;
      }
    }
    source = current.source();
  }

  // Fallback: match common transient error strings when we can't downcast.
  let msg = err.to_string().to_ascii_lowercase();
  msg.contains("timeout")
    || msg.contains("timed out")
    || msg.contains("connection reset")
    || msg.contains("connection aborted")
    || msg.contains("broken pipe")
    || msg.contains("temporary failure")
    || msg.contains("dns")
}

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
  retry_policy: HttpRetryPolicy,
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
      .field("retry_policy", &self.retry_policy)
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

  /// Override the retry/backoff policy used for HTTP(S) fetches.
  pub fn with_retry_policy(mut self, policy: HttpRetryPolicy) -> Self {
    self.retry_policy = policy;
    self
  }

  /// Override the maximum redirect attempts.
  pub fn with_max_redirects(mut self, max_redirects: usize) -> Self {
    self.policy.max_redirects = max_redirects.max(1);
    self
  }

  fn deadline_aware_timeout(
    &self,
    deadline: Option<&render_control::RenderDeadline>,
    url: &str,
  ) -> Result<Option<Duration>> {
    let Some(deadline) = deadline else {
      return Ok(None);
    };
    if deadline.timeout_limit().is_none() {
      return Ok(None);
    }
    let Some(remaining) = deadline.remaining_timeout() else {
      return Err(Error::Render(RenderError::Timeout {
        stage: render_stage_hint_from_url(url),
        elapsed: deadline.elapsed(),
      }));
    };
    if remaining <= HTTP_DEADLINE_BUFFER {
      return Err(Error::Resource(
        ResourceError::new(
          url.to_string(),
          "render deadline exceeded before HTTP request (timeout)",
        )
        .with_final_url(url.to_string()),
      ));
    }
    let budget = remaining
      .saturating_sub(HTTP_DEADLINE_BUFFER)
      .min(self.policy.request_timeout);
    Ok(Some(budget))
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
    let deadline = render_control::active_deadline();
    let mut current = url.to_string();
    let mut validators = validators;
    let agent = &self.agent;
    let max_attempts = if deadline
      .as_ref()
      .and_then(render_control::RenderDeadline::timeout_limit)
      .is_some()
    {
      1
    } else {
      self.retry_policy.max_attempts.max(1)
    };

    'redirects: for _ in 0..self.policy.max_redirects {
      self.policy.ensure_url_allowed(&current)?;
      for attempt in 1..=max_attempts {
        self.policy.ensure_url_allowed(&current)?;
        let allowed_limit = self.policy.allowed_response_limit()?;
        let per_request_timeout = self.deadline_aware_timeout(deadline.as_ref(), &current)?;
        let effective_timeout = per_request_timeout.unwrap_or(self.policy.request_timeout);

        let accept_encoding_value = accept_encoding.unwrap_or(SUPPORTED_ACCEPT_ENCODING);
        let mut request = agent
          .get(&current)
          .header("User-Agent", &self.user_agent)
          .header("Accept", DEFAULT_ACCEPT)
          .header("Accept-Language", &self.accept_language)
          .header("Accept-Encoding", accept_encoding_value);

        if !effective_timeout.is_zero() {
          request = request
            .config()
            .timeout_global(Some(effective_timeout))
            .build();
        }

        if let Some(v) = validators {
          if let Some(tag) = v.etag {
            request = request.header("If-None-Match", tag);
          }
          if let Some(modified) = v.last_modified {
            request = request.header("If-Modified-Since", modified);
          }
        }

        let mut network_timer = start_network_fetch_diagnostics();
        let mut response = match request.call() {
          Ok(resp) => resp,
          Err(err) => {
            finish_network_fetch_diagnostics(network_timer.take());
            if attempt < max_attempts && is_retryable_ureq_error(&err) {
              let mut backoff = compute_backoff(&self.retry_policy, attempt, &current);
              let mut can_retry = true;
              if let Some(deadline) = deadline.as_ref() {
                if deadline.timeout_limit().is_some() {
                  match deadline.remaining_timeout() {
                    Some(remaining) if !remaining.is_zero() => {
                      // Ensure we never sleep past the deadline; cap and allow immediate retry if the
                      // remaining budget is tiny.
                      let max_sleep = remaining.saturating_sub(Duration::from_millis(1));
                      backoff = backoff.min(max_sleep);
                    }
                    _ => can_retry = false,
                  }
                }
              }
              if can_retry {
                let reason = err.to_string();
                log_http_retry(&reason, attempt, max_attempts, &current, backoff);
                if !backoff.is_zero() {
                  thread::sleep(backoff);
                }
                continue;
              }
            }

            let overall_timeout = deadline.as_ref().and_then(|d| d.timeout_limit());
            let mut message = err.to_string();
            let lower = message.to_ascii_lowercase();
            if lower.contains("timeout") || lower.contains("timed out") {
              if let Some(overall) = overall_timeout {
                message.push_str(&format!(
                  " (attempt {attempt}/{max_attempts}, per_attempt_timeout={effective_timeout:?}, overall_timeout={overall:?})"
                ));
              } else {
                message.push_str(&format!(
                  " (attempt {attempt}/{max_attempts}, per_attempt_timeout={effective_timeout:?})"
                ));
              }
            } else {
              message.push_str(&format_attempt_suffix(attempt, max_attempts));
            }

            let err = ResourceError::new(current.clone(), message)
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
            finish_network_fetch_diagnostics(network_timer.take());
            self.policy.ensure_url_allowed(&next)?;
            current = next;
            validators = None;
            continue 'redirects;
          }
        }

        let status_code = status.as_u16();
        let retry_after =
          if self.retry_policy.respect_retry_after && retryable_http_status(status_code) {
            parse_retry_after(response.headers())
          } else {
            None
          };

        let encodings = parse_content_encodings(response.headers());
        let content_type = response
          .headers()
          .get("content-type")
          .and_then(|h| h.to_str().ok())
          .map(|s| s.to_string());
        let decode_stage = decode_stage_for_content_type(content_type.as_deref());
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
        let final_url = response.get_uri().to_string();

        let body_result = response
          .body_mut()
          .with_config()
          .limit(allowed_limit as u64)
          .read_to_vec()
          .map_err(|e| e.into_io());

        match body_result {
          Ok(bytes) => {
            let bytes =
              match decode_content_encodings(bytes, &encodings, allowed_limit, decode_stage) {
                Ok(decoded) => decoded,
                Err(ContentDecodeError::DeadlineExceeded { stage, elapsed, .. }) => {
                  finish_network_fetch_diagnostics(network_timer.take());
                  return Err(Error::Render(RenderError::Timeout { stage, elapsed }));
                }
                Err(ContentDecodeError::DecompressionFailed { .. })
                  if accept_encoding.is_none() =>
                {
                  finish_network_fetch_diagnostics(network_timer.take());
                  return self.fetch_http_with_accept(&current, Some("identity"), validators);
                }
                Err(err) => {
                  finish_network_fetch_diagnostics(network_timer.take());
                  let err =
                    err.into_resource_error(current.clone(), status.as_u16(), final_url.clone());
                  return Err(Error::Resource(err));
                }
              };

            record_network_fetch_bytes(bytes.len());
            let is_retryable_status = retryable_http_status(status_code);

            if bytes.is_empty() && !http_status_allows_empty_body(status_code) {
              finish_network_fetch_diagnostics(network_timer.take());
              let mut can_retry = attempt < max_attempts;
              if can_retry {
                let mut backoff = compute_backoff(&self.retry_policy, attempt, &current);
                if let Some(retry_after) = retry_after {
                  backoff = backoff.max(retry_after);
                }

                if let Some(deadline) = deadline.as_ref() {
                  if deadline.timeout_limit().is_some() {
                    match deadline.remaining_timeout() {
                      Some(remaining) if !remaining.is_zero() => {
                        let max_sleep = remaining.saturating_sub(Duration::from_millis(1));
                        backoff = backoff.min(max_sleep);
                      }
                      _ => can_retry = false,
                    }
                  }
                }

                if can_retry {
                  log_http_retry(
                    &format!("empty body (status {status_code})"),
                    attempt,
                    max_attempts,
                    &current,
                    backoff,
                  );
                  if !backoff.is_zero() {
                    thread::sleep(backoff);
                  }
                  continue;
                }
              }

              let mut message = "empty HTTP response body".to_string();
              if attempt < max_attempts {
                message.push_str(" (retry aborted: render deadline exceeded)");
              }
              message.push_str(&format_attempt_suffix(attempt, max_attempts));
              let err = ResourceError::new(current.clone(), message)
                .with_status(status_code)
                .with_final_url(final_url.clone());
              return Err(Error::Resource(err));
            }

            if is_retryable_status {
              finish_network_fetch_diagnostics(network_timer.take());
              let mut can_retry = attempt < max_attempts;
              if can_retry {
                let mut backoff = compute_backoff(&self.retry_policy, attempt, &current);
                if let Some(retry_after) = retry_after {
                  backoff = backoff.max(retry_after);
                }
                if let Some(deadline) = deadline.as_ref() {
                  if deadline.timeout_limit().is_some() {
                    match deadline.remaining_timeout() {
                      Some(remaining) if !remaining.is_zero() => {
                        let max_sleep = remaining.saturating_sub(Duration::from_millis(1));
                        backoff = backoff.min(max_sleep);
                      }
                      _ => can_retry = false,
                    }
                  }
                }
                if can_retry {
                  log_http_retry(
                    &format!("status {status_code}"),
                    attempt,
                    max_attempts,
                    &current,
                    backoff,
                  );
                  if !backoff.is_zero() {
                    thread::sleep(backoff);
                  }
                  continue;
                }
              }

              let mut message = if attempt < max_attempts {
                "retryable HTTP status (retry aborted: render deadline exceeded)".to_string()
              } else {
                "retryable HTTP status (retries exhausted)".to_string()
              };
              message.push_str(&format_attempt_suffix(attempt, max_attempts));
              let err = ResourceError::new(current.clone(), message)
                .with_status(status_code)
                .with_final_url(final_url.clone());
              return Err(Error::Resource(err));
            }

            if bytes.len() > allowed_limit {
              finish_network_fetch_diagnostics(network_timer.take());
              if let Some(remaining) = self.policy.remaining_budget() {
                if bytes.len() > remaining {
                  let err = ResourceError::new(
                    current.clone(),
                    format!(
                      "total bytes budget exceeded ({} > {} bytes remaining)",
                      bytes.len(),
                      remaining
                    ),
                  )
                  .with_status(status.as_u16())
                  .with_final_url(final_url.clone());
                  return Err(Error::Resource(err));
                }
              }

              let err = ResourceError::new(
                current.clone(),
                format!(
                  "response too large ({} > {} bytes)",
                  bytes.len(),
                  allowed_limit
                ),
              )
              .with_status(status.as_u16())
              .with_final_url(final_url.clone());
              return Err(Error::Resource(err));
            }

            finish_network_fetch_diagnostics(network_timer.take());
            self.policy.reserve_budget(bytes.len())?;
            let mut resource =
              FetchedResource::with_final_url(bytes, content_type, Some(final_url));
            resource.status = Some(status.as_u16());
            resource.etag = etag;
            resource.last_modified = last_modified;
            resource.cache_policy = cache_policy;
            return Ok(resource);
          }
          Err(err) => {
            finish_network_fetch_diagnostics(network_timer.take());
            if attempt < max_attempts && is_retryable_io_error(&err) {
              let mut backoff = compute_backoff(&self.retry_policy, attempt, &current);
              let mut can_retry = true;
              if let Some(deadline) = deadline.as_ref() {
                if deadline.timeout_limit().is_some() {
                  match deadline.remaining_timeout() {
                    Some(remaining) if !remaining.is_zero() => {
                      let max_sleep = remaining.saturating_sub(Duration::from_millis(1));
                      backoff = backoff.min(max_sleep);
                    }
                    _ => can_retry = false,
                  }
                }
              }
              if can_retry {
                let reason = err.to_string();
                log_http_retry(&reason, attempt, max_attempts, &current, backoff);
                if !backoff.is_zero() {
                  thread::sleep(backoff);
                }
                continue;
              }
            }

            let overall_timeout = deadline.as_ref().and_then(|d| d.timeout_limit());
            let mut message = err.to_string();
            if err.kind() == io::ErrorKind::TimedOut {
              if let Some(overall) = overall_timeout {
                message.push_str(&format!(
                  " (attempt {attempt}/{max_attempts}, per_attempt_timeout={effective_timeout:?}, overall_timeout={overall:?})"
                ));
              } else {
                message.push_str(&format!(
                  " (attempt {attempt}/{max_attempts}, per_attempt_timeout={effective_timeout:?})"
                ));
              }
            } else {
              message.push_str(&format_attempt_suffix(attempt, max_attempts));
            }

            let err = ResourceError::new(current.clone(), message)
              .with_status(status.as_u16())
              .with_final_url(final_url)
              .with_source(err);
            return Err(Error::Resource(err));
          }
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
      retry_policy: HttpRetryPolicy::default(),
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

/// Policy controlling how stale cached entries are handled.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CacheStalePolicy {
  /// Preserve existing behavior: stale entries trigger revalidation (conditional request) when
  /// validators are available, falling back to cached bytes only after the network attempt fails.
  Revalidate,
  /// When a render deadline with a timeout is active, serve cached bytes immediately even if the
  /// entry is stale or requires revalidation.
  UseStaleWhenDeadline,
}

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
  /// Policy controlling whether stale cached entries are served without revalidation when a
  /// render deadline is active.
  pub stale_policy: CacheStalePolicy,
}

impl Default for CachingFetcherConfig {
  fn default() -> Self {
    Self {
      max_bytes: 64 * 1024 * 1024,
      max_items: 512,
      cache_errors: true,
      honor_http_cache_headers: true,
      honor_http_cache_freshness: false,
      stale_policy: CacheStalePolicy::Revalidate,
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
  pub(crate) is_stale: bool,
}

#[derive(Debug, Default, Clone)]
pub struct ResourceCacheDiagnostics {
  pub fresh_hits: usize,
  pub stale_hits: usize,
  pub revalidated_hits: usize,
  pub misses: usize,
  /// Total bytes returned from the caching layer (fresh/stale/revalidated hits only).
  ///
  /// This is intentionally "bytes served from cache", not "bytes stored in cache", and excludes
  /// cache misses that hit the network.
  pub resource_cache_bytes: usize,
  pub disk_cache_hits: usize,
  pub disk_cache_misses: usize,
  pub disk_cache_bytes: usize,
  pub disk_cache_ms: f64,
  pub network_fetches: usize,
  pub network_fetch_bytes: usize,
  pub network_fetch_ms: f64,
}

struct ResourceCacheDiagnosticsState {
  fresh_hits: AtomicUsize,
  stale_hits: AtomicUsize,
  revalidated_hits: AtomicUsize,
  misses: AtomicUsize,
  resource_cache_bytes: AtomicUsize,
  disk_cache_hits: AtomicUsize,
  disk_cache_misses: AtomicUsize,
  disk_cache_bytes: AtomicUsize,
  disk_cache_ns: AtomicU64,
  network_fetches: AtomicUsize,
  network_fetch_bytes: AtomicUsize,
  network_fetch_ns: AtomicU64,
}

#[derive(Clone, Copy)]
struct ResourceCacheDiagnosticsSnapshot {
  fresh_hits: usize,
  stale_hits: usize,
  revalidated_hits: usize,
  misses: usize,
  resource_cache_bytes: usize,
  disk_cache_hits: usize,
  disk_cache_misses: usize,
  disk_cache_bytes: usize,
  disk_cache_ns: u64,
  network_fetches: usize,
  network_fetch_bytes: usize,
  network_fetch_ns: u64,
}

#[derive(Default)]
struct ResourceCacheDiagnosticsBaseline {
  baseline: Option<ResourceCacheDiagnosticsSnapshot>,
}

impl Drop for ResourceCacheDiagnosticsBaseline {
  fn drop(&mut self) {
    if self.baseline.is_some() {
      end_resource_cache_diagnostics_session();
    }
  }
}

thread_local! {
  static RESOURCE_CACHE_DIAGNOSTICS_BASELINE: RefCell<ResourceCacheDiagnosticsBaseline> =
    RefCell::new(ResourceCacheDiagnosticsBaseline::default());
}

static RESOURCE_CACHE_DIAGNOSTICS_ACTIVE_SESSIONS: AtomicUsize = AtomicUsize::new(0);

static RESOURCE_CACHE_DIAGNOSTICS: ResourceCacheDiagnosticsState = ResourceCacheDiagnosticsState {
  fresh_hits: AtomicUsize::new(0),
  stale_hits: AtomicUsize::new(0),
  revalidated_hits: AtomicUsize::new(0),
  misses: AtomicUsize::new(0),
  resource_cache_bytes: AtomicUsize::new(0),
  disk_cache_hits: AtomicUsize::new(0),
  disk_cache_misses: AtomicUsize::new(0),
  disk_cache_bytes: AtomicUsize::new(0),
  disk_cache_ns: AtomicU64::new(0),
  network_fetches: AtomicUsize::new(0),
  network_fetch_bytes: AtomicUsize::new(0),
  network_fetch_ns: AtomicU64::new(0),
};

fn resource_cache_diagnostics_snapshot() -> ResourceCacheDiagnosticsSnapshot {
  ResourceCacheDiagnosticsSnapshot {
    fresh_hits: RESOURCE_CACHE_DIAGNOSTICS
      .fresh_hits
      .load(Ordering::Relaxed),
    stale_hits: RESOURCE_CACHE_DIAGNOSTICS
      .stale_hits
      .load(Ordering::Relaxed),
    revalidated_hits: RESOURCE_CACHE_DIAGNOSTICS
      .revalidated_hits
      .load(Ordering::Relaxed),
    misses: RESOURCE_CACHE_DIAGNOSTICS.misses.load(Ordering::Relaxed),
    resource_cache_bytes: RESOURCE_CACHE_DIAGNOSTICS
      .resource_cache_bytes
      .load(Ordering::Relaxed),
    disk_cache_hits: RESOURCE_CACHE_DIAGNOSTICS
      .disk_cache_hits
      .load(Ordering::Relaxed),
    disk_cache_misses: RESOURCE_CACHE_DIAGNOSTICS
      .disk_cache_misses
      .load(Ordering::Relaxed),
    disk_cache_bytes: RESOURCE_CACHE_DIAGNOSTICS
      .disk_cache_bytes
      .load(Ordering::Relaxed),
    disk_cache_ns: RESOURCE_CACHE_DIAGNOSTICS
      .disk_cache_ns
      .load(Ordering::Relaxed),
    network_fetches: RESOURCE_CACHE_DIAGNOSTICS
      .network_fetches
      .load(Ordering::Relaxed),
    network_fetch_bytes: RESOURCE_CACHE_DIAGNOSTICS
      .network_fetch_bytes
      .load(Ordering::Relaxed),
    network_fetch_ns: RESOURCE_CACHE_DIAGNOSTICS
      .network_fetch_ns
      .load(Ordering::Relaxed),
  }
}

fn resource_cache_diagnostics_enabled() -> bool {
  RESOURCE_CACHE_DIAGNOSTICS_ACTIVE_SESSIONS.load(Ordering::Relaxed) > 0
}

fn end_resource_cache_diagnostics_session() {
  // Avoid underflow: diagnostics state can leak when callers enable collection but never call
  // `take_resource_cache_diagnostics()` (e.g. panic during a diagnostics-enabled render). Use
  // `fetch_update` so a leaked session doesn't permanently keep diagnostics enabled.
  let _ = RESOURCE_CACHE_DIAGNOSTICS_ACTIVE_SESSIONS.fetch_update(
    Ordering::Relaxed,
    Ordering::Relaxed,
    |value| value.checked_sub(1),
  );
}

pub(crate) fn enable_resource_cache_diagnostics() {
  let baseline = resource_cache_diagnostics_snapshot();
  let enabled = RESOURCE_CACHE_DIAGNOSTICS_BASELINE.with(|cell| {
    let mut guard = cell.borrow_mut();
    let was_enabled = guard.baseline.is_some();
    // Reset the baseline every time enable is called so a panic that skips
    // `take_resource_cache_diagnostics()` doesn't permanently poison subsequent sessions (e.g.
    // pageset dump capture after a panic).
    guard.baseline = Some(baseline);
    !was_enabled
  });
  if enabled {
    RESOURCE_CACHE_DIAGNOSTICS_ACTIVE_SESSIONS.fetch_add(1, Ordering::Relaxed);
  }
}

pub(crate) fn take_resource_cache_diagnostics() -> Option<ResourceCacheDiagnostics> {
  let baseline = RESOURCE_CACHE_DIAGNOSTICS_BASELINE.with(|cell| {
    let mut guard = cell.borrow_mut();
    guard.baseline.take()
  })?;
  end_resource_cache_diagnostics_session();

  let current = resource_cache_diagnostics_snapshot();
  let disk_cache_ns = current.disk_cache_ns.saturating_sub(baseline.disk_cache_ns);
  let network_fetch_ns = current
    .network_fetch_ns
    .saturating_sub(baseline.network_fetch_ns);
  Some(ResourceCacheDiagnostics {
    fresh_hits: current.fresh_hits.saturating_sub(baseline.fresh_hits),
    stale_hits: current.stale_hits.saturating_sub(baseline.stale_hits),
    revalidated_hits: current
      .revalidated_hits
      .saturating_sub(baseline.revalidated_hits),
    misses: current.misses.saturating_sub(baseline.misses),
    resource_cache_bytes: current
      .resource_cache_bytes
      .saturating_sub(baseline.resource_cache_bytes),
    disk_cache_hits: current
      .disk_cache_hits
      .saturating_sub(baseline.disk_cache_hits),
    disk_cache_misses: current
      .disk_cache_misses
      .saturating_sub(baseline.disk_cache_misses),
    disk_cache_bytes: current
      .disk_cache_bytes
      .saturating_sub(baseline.disk_cache_bytes),
    disk_cache_ms: (disk_cache_ns as f64) / 1_000_000.0,
    network_fetches: current
      .network_fetches
      .saturating_sub(baseline.network_fetches),
    network_fetch_bytes: current
      .network_fetch_bytes
      .saturating_sub(baseline.network_fetch_bytes),
    network_fetch_ms: (network_fetch_ns as f64) / 1_000_000.0,
  })
}

fn record_cache_fresh_hit() {
  if !resource_cache_diagnostics_enabled() {
    return;
  }
  RESOURCE_CACHE_DIAGNOSTICS
    .fresh_hits
    .fetch_add(1, Ordering::Relaxed);
}

fn record_cache_stale_hit() {
  if !resource_cache_diagnostics_enabled() {
    return;
  }
  RESOURCE_CACHE_DIAGNOSTICS
    .stale_hits
    .fetch_add(1, Ordering::Relaxed);
}

fn record_cache_revalidated_hit() {
  if !resource_cache_diagnostics_enabled() {
    return;
  }
  RESOURCE_CACHE_DIAGNOSTICS
    .revalidated_hits
    .fetch_add(1, Ordering::Relaxed);
}

fn record_cache_miss() {
  if !resource_cache_diagnostics_enabled() {
    return;
  }
  RESOURCE_CACHE_DIAGNOSTICS
    .misses
    .fetch_add(1, Ordering::Relaxed);
}

fn record_resource_cache_bytes(bytes: usize) {
  if !resource_cache_diagnostics_enabled() {
    return;
  }
  RESOURCE_CACHE_DIAGNOSTICS
    .resource_cache_bytes
    .fetch_add(bytes, Ordering::Relaxed);
}

#[cfg(feature = "disk_cache")]
fn record_disk_cache_hit() {
  if !resource_cache_diagnostics_enabled() {
    return;
  }
  RESOURCE_CACHE_DIAGNOSTICS
    .disk_cache_hits
    .fetch_add(1, Ordering::Relaxed);
}

#[cfg(feature = "disk_cache")]
fn record_disk_cache_miss() {
  if !resource_cache_diagnostics_enabled() {
    return;
  }
  RESOURCE_CACHE_DIAGNOSTICS
    .disk_cache_misses
    .fetch_add(1, Ordering::Relaxed);
}

#[cfg(feature = "disk_cache")]
fn record_disk_cache_bytes(bytes: usize) {
  if !resource_cache_diagnostics_enabled() {
    return;
  }
  RESOURCE_CACHE_DIAGNOSTICS
    .disk_cache_bytes
    .fetch_add(bytes, Ordering::Relaxed);
}

#[cfg(feature = "disk_cache")]
fn start_disk_cache_diagnostics() -> Option<Instant> {
  resource_cache_diagnostics_enabled().then(Instant::now)
}

#[cfg(feature = "disk_cache")]
fn finish_disk_cache_diagnostics(start: Option<Instant>) {
  let Some(start) = start else {
    return;
  };
  let elapsed = start.elapsed();
  let nanos = u64::try_from(elapsed.as_nanos()).unwrap_or(u64::MAX);
  RESOURCE_CACHE_DIAGNOSTICS
    .disk_cache_ns
    .fetch_add(nanos, Ordering::Relaxed);
}

fn start_network_fetch_diagnostics() -> Option<Instant> {
  if !resource_cache_diagnostics_enabled() {
    return None;
  }
  RESOURCE_CACHE_DIAGNOSTICS
    .network_fetches
    .fetch_add(1, Ordering::Relaxed);
  Some(Instant::now())
}

fn record_network_fetch_bytes(bytes: usize) {
  if !resource_cache_diagnostics_enabled() {
    return;
  }
  RESOURCE_CACHE_DIAGNOSTICS
    .network_fetch_bytes
    .fetch_add(bytes, Ordering::Relaxed);
}

fn finish_network_fetch_diagnostics(start: Option<Instant>) {
  let Some(start) = start else {
    return;
  };
  let elapsed = start.elapsed();
  let nanos = u64::try_from(elapsed.as_nanos()).unwrap_or(u64::MAX);
  RESOURCE_CACHE_DIAGNOSTICS
    .network_fetch_ns
    .fetch_add(nanos, Ordering::Relaxed);
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

  fn wait(&self, url: &str) -> Result<FetchedResource> {
    let mut guard = self.result.lock().unwrap();
    let deadline = render_control::active_deadline().filter(|d| d.timeout_limit().is_some());

    while guard.is_none() {
      if let Some(deadline) = deadline.as_ref() {
        match deadline.remaining_timeout() {
          Some(remaining) if !remaining.is_zero() => {
            let wait_for = remaining.min(Duration::from_millis(10));
            guard = self.cv.wait_timeout(guard, wait_for).unwrap().0;
          }
          _ => {
            return Err(Error::Render(RenderError::Timeout {
              stage: render_stage_hint_from_url(url),
              elapsed: deadline.elapsed(),
            }));
          }
        }
      } else {
        guard = self.cv.wait(guard).unwrap();
      }
    }
    guard.as_ref().unwrap().as_result()
  }
}

fn render_stage_hint_from_url(url: &str) -> RenderStage {
  let path_hint = Url::parse(url)
    .ok()
    .map(|parsed| parsed.path().to_string())
    .unwrap_or_else(|| {
      url
        .split(|c| c == '?' || c == '#')
        .next()
        .unwrap_or(url)
        .to_string()
    });
  let content_type = guess_content_type_from_path(&path_hint);
  decode_stage_for_content_type(content_type.as_deref())
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
        is_stale: false,
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
        is_stale: false,
      };
    }

    if self.config.honor_http_cache_freshness {
      if let Some(meta) = http_cache.as_ref() {
        if meta.no_store {
          return CachePlan {
            cached: None,
            action: CacheAction::Fetch,
            is_stale: false,
          };
        }
        let now = SystemTime::now();
        let is_fresh = meta.is_fresh(now, freshness_cap);
        let requires_revalidation = meta.requires_revalidation();
        if is_fresh && !requires_revalidation {
          return CachePlan {
            cached,
            action: CacheAction::UseCached,
            is_stale: false,
          };
        }
        if self.config.stale_policy == CacheStalePolicy::UseStaleWhenDeadline
          && render_control::active_deadline()
            .as_ref()
            .and_then(|deadline| deadline.timeout_limit())
            .is_some()
        {
          return CachePlan {
            cached,
            action: CacheAction::UseCached,
            is_stale: true,
          };
        }
        if self.config.honor_http_cache_headers && has_validators {
          return CachePlan {
            cached,
            action: CacheAction::Validate {
              etag,
              last_modified,
            },
            is_stale: false,
          };
        }
        return CachePlan {
          cached,
          action: CacheAction::Fetch,
          is_stale: false,
        };
      } else if !self.config.honor_http_cache_headers {
        return CachePlan {
          cached,
          action: CacheAction::UseCached,
          is_stale: false,
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
        is_stale: false,
      }
    } else {
      CachePlan {
        cached,
        action: CacheAction::UseCached,
        is_stale: false,
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
      CacheValue::Resource(res) => res.final_url.clone(),
      CacheValue::Error(_) => None,
    };
    self.cache_entry(
      url,
      CacheEntry {
        etag: snapshot.etag.clone(),
        last_modified: snapshot.last_modified.clone(),
        http_cache: snapshot.http_cache.clone(),
        value: snapshot.value,
      },
      final_url.as_deref(),
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

struct InFlightOwnerGuard<'a, F: ResourceFetcher> {
  fetcher: &'a CachingFetcher<F>,
  url: &'a str,
  flight: Arc<InFlight>,
  finished: bool,
}

impl<'a, F: ResourceFetcher> InFlightOwnerGuard<'a, F> {
  fn new(fetcher: &'a CachingFetcher<F>, url: &'a str, flight: Arc<InFlight>) -> Self {
    Self {
      fetcher,
      url,
      flight,
      finished: false,
    }
  }

  fn finish(&mut self, result: SharedResult) {
    if self.finished {
      return;
    }
    self.finished = true;
    self.fetcher.finish_inflight(self.url, &self.flight, result);
  }
}

impl<'a, F: ResourceFetcher> Drop for InFlightOwnerGuard<'a, F> {
  fn drop(&mut self) {
    if self.finished {
      return;
    }

    self.finished = true;
    let err = Error::Resource(ResourceError::new(
      self.url.to_string(),
      "in-flight fetch owner dropped without resolving",
    ));
    self
      .fetcher
      .finish_inflight(self.url, &self.flight, SharedResult::Error(err));
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
        if plan.is_stale {
          record_cache_stale_hit();
        } else {
          record_cache_fresh_hit();
        }
        let result = snapshot.value.as_result();
        if let Ok(ref res) = result {
          record_resource_cache_bytes(res.bytes.len());
          reserve_policy_bytes(&self.policy, res)?;
        }
        return result;
      }
    }

    let (flight, is_owner) = self.join_inflight(url);
    if !is_owner {
      let result = flight.wait(url);
      if let Ok(ref res) = result {
        reserve_policy_bytes(&self.policy, res)?;
      }
      return result;
    }

    let mut inflight_guard = InFlightOwnerGuard::new(self, url, flight);

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
            if let Ok(ref ok) = value {
              record_resource_cache_bytes(ok.bytes.len());
            }
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
            if res.status.map(is_transient_http_status).unwrap_or(false) {
              if let Some(snapshot) = plan.cached.as_ref() {
                record_cache_stale_hit();
                let fallback = snapshot.value.as_result();
                if let Ok(ref ok) = fallback {
                  record_resource_cache_bytes(ok.bytes.len());
                }
                let is_ok = fallback.is_ok();
                (fallback, is_ok)
              } else {
                record_cache_miss();
              (Ok(res), false)
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
      }
      Err(err) => {
        if let Some(snapshot) = plan.cached.as_ref() {
          record_cache_stale_hit();
          let fallback = snapshot.value.as_result();
          if let Ok(ref ok) = fallback {
            record_resource_cache_bytes(ok.bytes.len());
          }
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
    inflight_guard.finish(notify);

    result
  }
}

fn parse_content_encodings(headers: &HeaderMap) -> Vec<String> {
  headers
    .get_all("content-encoding")
    .iter()
    .filter_map(|h| h.to_str().ok())
    .flat_map(|value| value.split(','))
    .map(|value| value.trim().to_ascii_lowercase())
    .filter(|value| !value.is_empty())
    .collect()
}

fn decode_stage_for_content_type(content_type: Option<&str>) -> RenderStage {
  let mime = content_type
    .and_then(|ct| ct.split(';').next())
    .map(|ct| ct.trim().to_ascii_lowercase())
    .unwrap_or_else(String::new);

  if mime.contains("text/css") || mime.contains("font/") {
    return RenderStage::Css;
  }
  if mime.contains("html") {
    return RenderStage::DomParse;
  }
  RenderStage::Paint
}

#[derive(Debug)]
enum ContentDecodeError {
  UnsupportedEncoding(String),
  DecompressionFailed {
    encoding: String,
    source: io::Error,
  },
  SizeLimitExceeded {
    decoded: usize,
    limit: usize,
  },
  DeadlineExceeded {
    encoding: String,
    stage: RenderStage,
    elapsed: Duration,
  },
}

impl std::fmt::Display for ContentDecodeError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::UnsupportedEncoding(encoding) => write!(f, "unsupported content encoding: {encoding}"),
      Self::DecompressionFailed { encoding, source } => {
        write!(f, "{encoding} decompression failed: {source}")
      }
      Self::SizeLimitExceeded { decoded, limit } => write!(
        f,
        "decoded body exceeds limit ({} > {} bytes)",
        decoded, limit
      ),
      Self::DeadlineExceeded {
        encoding,
        stage,
        elapsed,
      } => write!(
        f,
        "rendering timed out during {stage} while decompressing {encoding} after {elapsed:?}"
      ),
    }
  }
}

impl ContentDecodeError {
  fn into_resource_error(self, url: String, status: u16, final_url: String) -> ResourceError {
    let mut err = ResourceError::new(url, self.to_string())
      .with_status(status)
      .with_final_url(final_url);
    if let Self::DecompressionFailed { source, .. } = self {
      err = err.with_source(source);
    }
    err
  }
}

fn decode_content_encodings(
  body: Vec<u8>,
  encodings: &[String],
  limit: usize,
  stage: RenderStage,
) -> std::result::Result<Vec<u8>, ContentDecodeError> {
  if encodings.is_empty() {
    ensure_within_limit(body.len(), limit)?;
    return Ok(body);
  }

  let mut decoded = body;
  for encoding in encodings.iter().rev() {
    decoded = decode_single_encoding(encoding, &decoded, limit, stage)?;
  }

  Ok(decoded)
}

fn decode_single_encoding(
  encoding: &str,
  input: &[u8],
  limit: usize,
  stage: RenderStage,
) -> std::result::Result<Vec<u8>, ContentDecodeError> {
  match encoding {
    "" | "identity" => {
      ensure_within_limit(input.len(), limit)?;
      Ok(input.to_vec())
    }
    "gzip" => decode_with_reader("gzip", GzDecoder::new(Cursor::new(input)), limit, stage),
    "deflate" => decode_deflate(input, limit, stage),
    "br" => decode_with_reader(
      "br",
      Decompressor::new(Cursor::new(input), 4096),
      limit,
      stage,
    ),
    other => Err(ContentDecodeError::UnsupportedEncoding(other.to_string())),
  }
}

fn decode_deflate(
  input: &[u8],
  limit: usize,
  stage: RenderStage,
) -> std::result::Result<Vec<u8>, ContentDecodeError> {
  match decode_with_reader(
    "deflate",
    ZlibDecoder::new(Cursor::new(input)),
    limit,
    stage,
  ) {
    Ok(decoded) => Ok(decoded),
    Err(ContentDecodeError::DecompressionFailed { .. }) => decode_with_reader(
      "deflate",
      DeflateDecoder::new(Cursor::new(input)),
      limit,
      stage,
    ),
    Err(err) => Err(err),
  }
}

fn decode_with_reader<R: Read>(
  encoding: &str,
  mut reader: R,
  limit: usize,
  stage: RenderStage,
) -> std::result::Result<Vec<u8>, ContentDecodeError> {
  let mut decoded = Vec::new();
  let mut buf = [0u8; 8192];
  let mut deadline_counter = 0usize;

  loop {
    if let Err(err) =
      check_active_periodic(&mut deadline_counter, CONTENT_DECODE_DEADLINE_STRIDE, stage)
    {
      if let RenderError::Timeout { stage, elapsed } = err {
        return Err(ContentDecodeError::DeadlineExceeded {
          encoding: encoding.to_string(),
          stage,
          elapsed,
        });
      }
      return Err(ContentDecodeError::DecompressionFailed {
        encoding: encoding.to_string(),
        source: io::Error::new(io::ErrorKind::Other, err.to_string()),
      });
    }
    let read = reader
      .read(&mut buf)
      .map_err(|source| ContentDecodeError::DecompressionFailed {
        encoding: encoding.to_string(),
        source,
      })?;
    if read == 0 {
      break;
    }

    let next_len = decoded.len().saturating_add(read);
    if next_len > limit {
      return Err(ContentDecodeError::SizeLimitExceeded {
        decoded: next_len,
        limit,
      });
    }

    decoded.extend_from_slice(&buf[..read]);
  }

  Ok(decoded)
}

fn ensure_within_limit(len: usize, limit: usize) -> std::result::Result<(), ContentDecodeError> {
  if len > limit {
    return Err(ContentDecodeError::SizeLimitExceeded {
      decoded: len,
      limit,
    });
  }
  Ok(())
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
  use brotli::CompressorWriter;
  use flate2::write::GzEncoder;
  use flate2::Compression;
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
  use std::sync::OnceLock;
  use std::thread;
  use std::time::{Duration, Instant};

  fn try_bind_localhost(context: &str) -> Option<TcpListener> {
    match TcpListener::bind("127.0.0.1:0") {
      Ok(listener) => Some(listener),
      Err(err)
        if matches!(
          err.kind(),
          std::io::ErrorKind::PermissionDenied | std::io::ErrorKind::AddrNotAvailable
        ) =>
      {
        eprintln!("skipping {context}: cannot bind localhost in this environment: {err}");
        None
      }
      Err(err) => panic!("bind {context}: {err}"),
    }
  }

  static RESOURCE_CACHE_DIAGNOSTICS_TEST_LOCK: OnceLock<Mutex<()>> = OnceLock::new();

  fn resource_cache_diagnostics_test_lock() -> std::sync::MutexGuard<'static, ()> {
    RESOURCE_CACHE_DIAGNOSTICS_TEST_LOCK
      .get_or_init(|| Mutex::new(()))
      .lock()
      .unwrap()
  }

  struct ResourceCacheDiagnosticsGuard;

  impl ResourceCacheDiagnosticsGuard {
    fn new() -> Self {
      enable_resource_cache_diagnostics();
      Self
    }
  }

  impl Drop for ResourceCacheDiagnosticsGuard {
    fn drop(&mut self) {
      let _ = take_resource_cache_diagnostics();
    }
  }

  #[test]
  fn resource_cache_diagnostics_aggregate_across_threads() {
    let _lock = resource_cache_diagnostics_test_lock();
    let _guard = ResourceCacheDiagnosticsGuard::new();

    let threads = 4usize;
    let fresh_per_thread = 5000usize;
    let revalidated_per_thread = 3000usize;
    let misses_per_thread = 2000usize;
    let barrier = Arc::new(Barrier::new(threads + 1));
    let mut handles = Vec::new();
    for _ in 0..threads {
      let barrier = Arc::clone(&barrier);
      handles.push(thread::spawn(move || {
        barrier.wait();
        for _ in 0..fresh_per_thread {
          record_cache_fresh_hit();
        }
        for _ in 0..revalidated_per_thread {
          record_cache_revalidated_hit();
        }
        for _ in 0..misses_per_thread {
          record_cache_miss();
        }
      }));
    }
    barrier.wait();

    for handle in handles {
      handle.join().unwrap();
    }

    let stats = take_resource_cache_diagnostics().expect("diagnostics should be enabled");
    assert!(
      stats.fresh_hits >= threads * fresh_per_thread,
      "expected fresh hit counters to aggregate across threads"
    );
    assert!(
      stats.revalidated_hits >= threads * revalidated_per_thread,
      "expected revalidated hit counters to aggregate across threads"
    );
    assert!(
      stats.misses >= threads * misses_per_thread,
      "expected miss counters to aggregate across threads"
    );
  }

  #[test]
  fn resource_cache_diagnostics_record_network_fetches() {
    let _lock = resource_cache_diagnostics_test_lock();
    let Some(listener) = try_bind_localhost("resource_cache_diagnostics_record_network_fetches")
    else {
      return;
    };

    let addr = listener.local_addr().unwrap();
    let _guard = ResourceCacheDiagnosticsGuard::new();
    let requests = 5usize;
    let handle = thread::spawn(move || {
      for _ in 0..requests {
        let (mut stream, _) = listener.accept().unwrap();
        let mut buf = [0u8; 1024];
        let _ = stream.read(&mut buf);
        let body = b"ok";
        let headers = format!(
          "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
          body.len()
        );
        stream.write_all(headers.as_bytes()).unwrap();
        stream.write_all(body).unwrap();
      }
    });

    let fetcher = HttpFetcher::new().with_timeout(Duration::from_secs(2));
    let url = format!("http://{}/", addr);
    for _ in 0..requests {
      let res = fetcher.fetch(&url).expect("fetch should succeed");
      assert_eq!(res.bytes, b"ok");
    }
    handle.join().unwrap();

    let stats = take_resource_cache_diagnostics().expect("diagnostics should be enabled");
    assert!(
      stats.network_fetches >= requests,
      "expected network fetch counter to increment for each request (got {})",
      stats.network_fetches
    );
    assert!(
      stats.network_fetch_ms.is_finite() && stats.network_fetch_ms >= 0.0,
      "expected finite network fetch duration, got {}",
      stats.network_fetch_ms
    );
    assert!(
      stats.network_fetch_bytes >= requests * 2,
      "expected network fetch byte counter to include response bodies (got {})",
      stats.network_fetch_bytes
    );
  }

  #[test]
  fn resource_cache_diagnostics_record_stale_hits_under_deadline() {
    let _lock = resource_cache_diagnostics_test_lock();

    #[derive(Clone)]
    struct CountingFetcher {
      calls: Arc<AtomicUsize>,
    }

    impl ResourceFetcher for CountingFetcher {
      fn fetch(&self, url: &str) -> Result<FetchedResource> {
        self.calls.fetch_add(1, Ordering::SeqCst);
        let mut resource = FetchedResource::new(b"cached".to_vec(), Some("text/plain".to_string()));
        resource.final_url = Some(url.to_string());
        resource.cache_policy = Some(HttpCachePolicy {
          max_age: Some(0),
          ..Default::default()
        });
        Ok(resource)
      }
    }

    let calls = Arc::new(AtomicUsize::new(0));
    let fetcher = CachingFetcher::with_config(
      CountingFetcher {
        calls: Arc::clone(&calls),
      },
      CachingFetcherConfig {
        honor_http_cache_freshness: true,
        stale_policy: CacheStalePolicy::UseStaleWhenDeadline,
        ..CachingFetcherConfig::default()
      },
    );

    let url = "https://example.com/stale";
    let first = fetcher.fetch(url).expect("seed fetch");
    assert_eq!(first.bytes, b"cached");
    assert_eq!(calls.load(Ordering::SeqCst), 1);

    enable_resource_cache_diagnostics();
    let deadline = render_control::RenderDeadline::new(Some(Duration::from_secs(1)), None);
    let second = render_control::with_deadline(Some(&deadline), || fetcher.fetch(url))
      .expect("deadline stale hit should succeed");
    assert_eq!(second.bytes, b"cached");
    assert_eq!(
      calls.load(Ordering::SeqCst),
      1,
      "stale_policy should serve cached bytes under deadline"
    );

    let stats = take_resource_cache_diagnostics().expect("diagnostics should be enabled");
    assert!(
      stats.stale_hits >= 1,
      "expected stale hit counter to increment (got {})",
      stats.stale_hits
    );
  }

  #[cfg(feature = "disk_cache")]
  #[test]
  fn resource_cache_diagnostics_record_disk_cache_hits() {
    let _lock = resource_cache_diagnostics_test_lock();

    #[derive(Clone)]
    struct SeedFetcher;

    impl ResourceFetcher for SeedFetcher {
      fn fetch(&self, url: &str) -> Result<FetchedResource> {
        let mut resource = FetchedResource::new(b"cached".to_vec(), Some("text/plain".to_string()));
        resource.final_url = Some(url.to_string());
        Ok(resource)
      }
    }

    #[derive(Clone)]
    struct PanicFetcher;

    impl ResourceFetcher for PanicFetcher {
      fn fetch(&self, _url: &str) -> Result<FetchedResource> {
        panic!("inner fetcher should not be called for disk cache hit");
      }
    }

    let tmp = tempfile::tempdir().unwrap();
    let url = "https://example.com/resource";
    let seed = DiskCachingFetcher::new(SeedFetcher, tmp.path());
    let first = seed.fetch(url).expect("seed disk cache");
    assert_eq!(first.bytes, b"cached");

    let disk = DiskCachingFetcher::new(PanicFetcher, tmp.path());
    let _guard = ResourceCacheDiagnosticsGuard::new();
    let second = disk.fetch(url).expect("disk cache hit fetch");
    assert_eq!(second.bytes, b"cached");

    let stats = take_resource_cache_diagnostics().expect("diagnostics should be enabled");
    assert!(
      stats.disk_cache_hits >= 1,
      "expected disk cache hit counter to increment (got {})",
      stats.disk_cache_hits
    );
    assert!(
      stats.disk_cache_ms.is_finite() && stats.disk_cache_ms >= 0.0,
      "expected finite disk cache duration, got {}",
      stats.disk_cache_ms
    );
    assert!(
      stats.disk_cache_bytes >= b"cached".len(),
      "expected disk cache byte counter to include cached payload bytes (got {})",
      stats.disk_cache_bytes
    );
    assert!(
      stats.resource_cache_bytes >= b"cached".len(),
      "expected resource cache byte counter to include returned cached bytes (got {})",
      stats.resource_cache_bytes
    );
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
  fn http_fetcher_persists_cookies_across_requests_and_clones() {
    let Some(listener) =
      try_bind_localhost("http_fetcher_persists_cookies_across_requests_and_clones")
    else {
      return;
    };
    let addr = listener.local_addr().unwrap();
    let handle = thread::spawn(move || {
      // First request sets a cookie.
      let (mut stream, _) = listener.accept().unwrap();
      stream
        .set_read_timeout(Some(Duration::from_millis(500)))
        .unwrap();
      let _ = read_http_request(&mut stream).unwrap();
      let body = b"first";
      let headers = format!(
        "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nSet-Cookie: a=b; Path=/\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
        body.len()
      );
      stream.write_all(headers.as_bytes()).unwrap();
      stream.write_all(body).unwrap();
      drop(stream);

      // Second request must send the cookie.
      let (mut stream, _) = listener.accept().unwrap();
      stream
        .set_read_timeout(Some(Duration::from_millis(500)))
        .unwrap();
      let request = read_http_request(&mut stream)
        .unwrap()
        .expect("expected second HTTP request");
      let req = String::from_utf8_lossy(&request).to_ascii_lowercase();
      assert!(
        req.contains("cookie: a=b"),
        "expected cookie header in follow-up request, got:\n{req}"
      );

      let body = b"second";
      let headers = format!(
        "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
        body.len()
      );
      stream.write_all(headers.as_bytes()).unwrap();
      stream.write_all(body).unwrap();
    });

    let fetcher = HttpFetcher::new().with_timeout(Duration::from_secs(2));
    let cloned = fetcher.clone();
    let url = format!("http://{}/set", addr);
    let res = fetcher.fetch(&url).expect("fetch initial cookie setter");
    assert_eq!(res.bytes, b"first");

    let url = format!("http://{}/check", addr);
    let res = cloned
      .fetch(&url)
      .expect("fetch should include cookie from prior request");
    assert_eq!(res.bytes, b"second");
    handle.join().unwrap();
  }

  #[test]
  fn http_fetcher_sends_cookies_on_redirect_followups() {
    let Some(listener) = try_bind_localhost("http_fetcher_sends_cookies_on_redirect_followups")
    else {
      return;
    };
    let addr = listener.local_addr().unwrap();
    let handle = thread::spawn(move || {
      // First request triggers a redirect and sets a cookie.
      let (mut stream, _) = listener.accept().unwrap();
      stream
        .set_read_timeout(Some(Duration::from_millis(500)))
        .unwrap();
      let _ = read_http_request(&mut stream).unwrap();
      let headers = "HTTP/1.1 302 Found\r\nLocation: /final\r\nSet-Cookie: a=b; Path=/\r\nContent-Length: 0\r\nConnection: close\r\n\r\n";
      stream.write_all(headers.as_bytes()).unwrap();
      drop(stream);

      // Redirect follow-up request must include the cookie.
      let (mut stream, _) = listener.accept().unwrap();
      stream
        .set_read_timeout(Some(Duration::from_millis(500)))
        .unwrap();
      let request = read_http_request(&mut stream)
        .unwrap()
        .expect("expected redirect follow-up request");
      let req = String::from_utf8_lossy(&request).to_ascii_lowercase();
      assert!(
        req.contains("cookie: a=b"),
        "expected cookie header in redirect follow-up request, got:\n{req}"
      );

      let body = b"ok";
      let headers = format!(
        "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
        body.len()
      );
      stream.write_all(headers.as_bytes()).unwrap();
      stream.write_all(body).unwrap();
    });

    let fetcher = HttpFetcher::new().with_timeout(Duration::from_secs(2));
    let url = format!("http://{}/start", addr);
    let res = fetcher.fetch(&url).expect("fetch should follow redirect");
    assert_eq!(res.bytes, b"ok");
    handle.join().unwrap();
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
  fn http_fetcher_sets_accept_header() {
    let Some(listener) = try_bind_localhost("http_fetcher_sets_accept_header") else {
      return;
    };
    let addr = listener.local_addr().unwrap();
    let captured = Arc::new(Mutex::new(String::new()));
    let captured_req = Arc::clone(&captured);
    let handle = thread::spawn(move || {
      let (mut stream, _) = listener.accept().unwrap();
      stream
        .set_read_timeout(Some(Duration::from_millis(500)))
        .unwrap();
      let request = read_http_request(&mut stream)
        .unwrap()
        .expect("expected HTTP request");
      if let Ok(mut slot) = captured_req.lock() {
        *slot = String::from_utf8_lossy(&request).to_string();
      }

      let body = b"hi";
      let headers = format!(
        "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
        body.len()
      );
      stream.write_all(headers.as_bytes()).unwrap();
      stream.write_all(body).unwrap();
    });

    let fetcher = HttpFetcher::new();
    let url = format!("http://{}/", addr);
    let res = fetcher.fetch(&url).expect("fetch accept header");
    handle.join().unwrap();

    assert_eq!(res.bytes, b"hi");
    let req = captured.lock().unwrap().to_ascii_lowercase();
    assert!(
      req.contains(&format!("accept: {DEFAULT_ACCEPT}").to_ascii_lowercase()),
      "missing accept header: {req}"
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
    let first = fetcher.fetch(&url).expect("first fetch");
    assert_eq!(first.bytes, b"pong");
    let second = fetcher.fetch(&url).expect("second fetch");
    assert_eq!(second.bytes, b"pong");

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
  fn http_fetch_does_not_run_past_render_deadline() {
    let Some(listener) = try_bind_localhost("http_fetch_does_not_run_past_render_deadline") else {
      return;
    };
    let addr = listener.local_addr().unwrap();
    listener.set_nonblocking(true).unwrap();

    let handle = thread::spawn(move || {
      let start = Instant::now();
      while start.elapsed() < Duration::from_secs(2) {
        match listener.accept() {
          Ok((mut stream, _)) => {
            stream
              .set_read_timeout(Some(Duration::from_millis(500)))
              .unwrap();
            let _ = read_http_request(&mut stream);

            // Delay the response well past the render deadline so the client must time out.
            thread::sleep(Duration::from_millis(200));

            let body = b"ok";
            let headers = format!(
              "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
              body.len()
            );
            let _ = stream.write_all(headers.as_bytes());
            let _ = stream.write_all(body);
            return;
          }
          Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => {
            thread::sleep(Duration::from_millis(5));
          }
          Err(err) => panic!("accept http_fetch_does_not_run_past_render_deadline: {err}"),
        }
      }

      panic!(
        "server did not receive request within {:?}",
        start.elapsed()
      );
    });

    let fetcher = HttpFetcher::new().with_timeout(Duration::from_secs(5));
    let url = format!("http://{}/", addr);
    let deadline = render_control::RenderDeadline::new(Some(Duration::from_millis(50)), None);

    let start = Instant::now();
    let res = render_control::with_deadline(Some(&deadline), || fetcher.fetch(&url));
    let elapsed = start.elapsed();

    handle.join().unwrap();

    assert!(
      res.is_err(),
      "expected fetch to fail under render deadline, got: {res:?}"
    );
    assert!(
      elapsed < Duration::from_millis(150),
      "fetch should fail quickly under deadline (elapsed={elapsed:?})"
    );

    let err = res.unwrap_err().to_string().to_ascii_lowercase();
    assert!(
      err.contains("timeout") || err.contains("deadline"),
      "expected error to mention timeout/deadline, got: {err}"
    );
  }

  #[test]
  fn http_fetch_disables_retries_under_deadline() {
    let Some(listener) = try_bind_localhost("http_fetch_disables_retries_under_deadline") else {
      return;
    };
    let addr = listener.local_addr().unwrap();
    listener.set_nonblocking(true).unwrap();

    let request_count = Arc::new(AtomicUsize::new(0));
    let seen = Arc::clone(&request_count);

    let handle = thread::spawn(move || {
      let start = Instant::now();
      let mut last_request = None::<Instant>;

      while start.elapsed() < Duration::from_secs(2) {
        match listener.accept() {
          Ok((mut stream, _)) => {
            seen.fetch_add(1, Ordering::SeqCst);
            last_request = Some(Instant::now());

            stream
              .set_read_timeout(Some(Duration::from_millis(500)))
              .unwrap();
            let _ = read_http_request(&mut stream);

            let body = b"unavailable";
            let headers = format!(
              "HTTP/1.1 503 Service Unavailable\r\nContent-Type: text/plain\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
              body.len()
            );
            let _ = stream.write_all(headers.as_bytes());
            let _ = stream.write_all(body);
          }
          Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => {
            if let Some(last) = last_request {
              if last.elapsed() > Duration::from_millis(250) {
                break;
              }
            }
            thread::sleep(Duration::from_millis(5));
          }
          Err(err) => panic!("accept http_fetch_disables_retries_under_deadline: {err}"),
        }
      }
    });

    let retry_policy = HttpRetryPolicy {
      max_attempts: 6,
      backoff_base: Duration::ZERO,
      backoff_cap: Duration::ZERO,
      respect_retry_after: false,
    };
    let fetcher = HttpFetcher::new()
      .with_timeout(Duration::from_secs(5))
      .with_retry_policy(retry_policy);
    let url = format!("http://{}/", addr);
    let deadline = render_control::RenderDeadline::new(Some(Duration::from_millis(200)), None);

    let res = render_control::with_deadline(Some(&deadline), || fetcher.fetch(&url));
    assert!(
      res.is_err(),
      "expected 503 to surface as an error when retries are disabled, got: {res:?}"
    );

    handle.join().unwrap();

    assert_eq!(
      request_count.load(Ordering::SeqCst),
      1,
      "expected retries to be disabled under render deadline"
    );
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
  fn inflight_wait_respects_render_deadline() {
    use std::sync::mpsc;

    struct BlockingFetcher {
      started: Arc<Barrier>,
      release: Arc<Barrier>,
    }

    impl ResourceFetcher for BlockingFetcher {
      fn fetch(&self, url: &str) -> Result<FetchedResource> {
        if url == "https://example.com/blocked" {
          self.started.wait();
          self.release.wait();
          return Ok(FetchedResource::new(
            b"ok".to_vec(),
            Some("text/plain".to_string()),
          ));
        }

        Err(Error::Resource(ResourceError::new(
          url.to_string(),
          "unexpected url",
        )))
      }
    }

    let started = Arc::new(Barrier::new(2));
    let release = Arc::new(Barrier::new(2));
    let url = "https://example.com/blocked".to_string();
    let fetcher = Arc::new(CachingFetcher::new(BlockingFetcher {
      started: Arc::clone(&started),
      release: Arc::clone(&release),
    }));

    let fetcher_owner = Arc::clone(&fetcher);
    let url_owner = url.clone();
    let owner_handle = thread::spawn(move || fetcher_owner.fetch(&url_owner));

    // Wait until the owner thread has started the inner fetch and registered the in-flight entry.
    started.wait();

    let fetcher_waiter = Arc::clone(&fetcher);
    let url_waiter = url.clone();
    let (tx, rx) = mpsc::channel();
    let waiter_handle = thread::spawn(move || {
      let deadline = render_control::RenderDeadline::new(Some(Duration::from_millis(50)), None);
      let start = Instant::now();
      let result =
        render_control::with_deadline(Some(&deadline), || fetcher_waiter.fetch(&url_waiter));
      let elapsed = start.elapsed();
      tx.send((result, elapsed)).unwrap();
    });

    let (result, elapsed) = match rx.recv_timeout(Duration::from_secs(1)) {
      Ok(value) => value,
      Err(err) => {
        // Unblock the owner thread so the test can terminate even if the waiter path is broken.
        release.wait();
        let _ = owner_handle.join();
        waiter_handle.join().unwrap();
        panic!("waiter fetch did not complete under deadline: {err}");
      }
    };

    let err = result.expect_err("waiter fetch should fail under deadline");
    match err {
      Error::Render(RenderError::Timeout { stage, .. }) => {
        assert_eq!(stage, RenderStage::Paint);
      }
      other => panic!("unexpected error after {elapsed:?}: {other:?}"),
    }

    // Let the owner thread finish so it can clean up the in-flight entry.
    release.wait();

    let owner_result = owner_handle.join().unwrap();
    assert!(
      owner_result.is_ok(),
      "owner fetch should complete after being released: {owner_result:?}"
    );
    waiter_handle.join().unwrap();
  }

  #[test]
  fn http_fetch_deadline_exceeded_before_request_surfaces_as_timeout() {
    let fetcher = HttpFetcher::new();
    let url = "http://example.com/style.css";
    let deadline = render_control::RenderDeadline::new(Some(Duration::from_millis(0)), None);
    let err = render_control::with_deadline(Some(&deadline), || fetcher.fetch(url))
      .expect_err("expected deadline-exceeded fetch to fail");
    match err {
      Error::Render(RenderError::Timeout { stage, .. }) => {
        assert_eq!(stage, RenderStage::Css);
      }
      other => panic!("expected timeout error, got {other:?}"),
    }
  }

  #[test]
  fn test_fetch_data_url() {
    let fetcher = HttpFetcher::new();
    let resource = fetcher.fetch("data:text/plain,test").unwrap();
    assert_eq!(resource.bytes, b"test");
  }

  #[test]
  fn http_fetcher_retries_with_identity_accept_encoding_on_decompression_error() {
    use std::time::{Duration, Instant};

    let Some(listener) = try_bind_localhost(
      "http_fetcher_retries_with_identity_accept_encoding_on_decompression_error",
    ) else {
      return;
    };
    let addr = listener.local_addr().unwrap();
    listener.set_nonblocking(true).unwrap();

    let captured = Arc::new(Mutex::new(Vec::<Vec<String>>::new()));
    let captured_headers = Arc::clone(&captured);
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

            let req_str = String::from_utf8_lossy(&req);
            let headers: Vec<String> = req_str
              .lines()
              .filter(|line| line.to_ascii_lowercase().starts_with("accept-encoding:"))
              .map(|line| line.trim().to_ascii_lowercase())
              .collect();
            captured_headers.lock().unwrap().push(headers);

            if handled == 0 {
              let body = b"not brotli";
              let response = format!(
                "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Encoding: br\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
                body.len()
              );
              let _ = stream.write_all(response.as_bytes());
              let _ = stream.write_all(body);
            } else {
              let body = b"plain body";
              let response = format!(
                "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
                body.len()
              );
              let _ = stream.write_all(response.as_bytes());
              let _ = stream.write_all(body);
            }

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
    let res = fetcher
      .fetch(&url)
      .expect("fetch after decompression retry");
    handle.join().unwrap();

    assert_eq!(res.bytes, b"plain body");

    let captured = captured.lock().unwrap();
    assert_eq!(
      captured.len(),
      2,
      "decompression error should trigger a retry with a second request: {:?}",
      *captured
    );

    let initial_headers = &captured[0];
    assert_eq!(
      initial_headers,
      &vec!["accept-encoding: gzip, deflate, br".to_string()],
      "default request should advertise gzip/deflate/br"
    );

    let retry_headers = &captured[1];
    assert_eq!(
      retry_headers.len(),
      1,
      "retry should send exactly one Accept-Encoding header: {:?}",
      retry_headers
    );
    assert_eq!(
      retry_headers[0], "accept-encoding: identity",
      "retry Accept-Encoding should be identity"
    );
  }

  #[test]
  fn fetch_http_decodes_brotli_body() {
    let Some(listener) = try_bind_localhost("fetch_http_decodes_brotli_body") else {
      return;
    };
    let addr = listener.local_addr().unwrap();

    let handle = std::thread::spawn(move || {
      if let Some(stream) = listener.incoming().next() {
        let mut stream = stream.unwrap();
        let mut buf = [0u8; 1024];
        let _ = stream.read(&mut buf);

        let body = b"hello brotli";
        let mut compressed = Vec::new();
        {
          let mut writer = CompressorWriter::new(&mut compressed, 4096, 5, 22);
          writer.write_all(body).unwrap();
          writer.flush().unwrap();
        }

        let headers = format!(
          "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Encoding: br\r\nContent-Length: {}\r\n\r\n",
          compressed.len()
        );
        let _ = stream.write_all(headers.as_bytes());
        let _ = stream.write_all(&compressed);
      }
    });

    let fetcher = HttpFetcher::new().with_timeout(Duration::from_secs(2));
    let url = format!("http://{}", addr);
    let res = fetcher.fetch(&url).expect("brotli fetch");
    handle.join().unwrap();

    assert_eq!(res.bytes, b"hello brotli");
  }

  #[test]
  fn fetch_http_decodes_gzip_body() {
    let Some(listener) = try_bind_localhost("fetch_http_decodes_gzip_body") else {
      return;
    };
    let addr = listener.local_addr().unwrap();

    let handle = std::thread::spawn(move || {
      if let Some(stream) = listener.incoming().next() {
        let mut stream = stream.unwrap();
        let mut buf = [0u8; 1024];
        let _ = stream.read(&mut buf);

        let body = b"hello gzip";
        let mut encoder = GzEncoder::new(Vec::new(), Compression::default());
        encoder.write_all(body).unwrap();
        let compressed = encoder.finish().unwrap();

        let headers = format!(
          "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Encoding: gzip\r\nContent-Length: {}\r\n\r\n",
          compressed.len()
        );
        let _ = stream.write_all(headers.as_bytes());
        let _ = stream.write_all(&compressed);
      }
    });

    let fetcher = HttpFetcher::new().with_timeout(Duration::from_secs(2));
    let url = format!("http://{}", addr);
    let res = fetcher.fetch(&url).expect("gzip fetch");
    handle.join().unwrap();

    assert_eq!(res.bytes, b"hello gzip");
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
  fn fetch_http_retries_on_bad_brotli() {
    use std::io::Read;
    use std::io::Write;
    use std::thread;
    use std::time::Duration;
    use std::time::Instant;

    let Some(listener) = try_bind_localhost("fetch_http_retries_on_bad_brotli") else {
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
            let body = b"hello brotli";
            let encoding_header = if req_str.contains("accept-encoding: identity") {
              ""
            } else {
              "Content-Encoding: br\r\n"
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
    assert_eq!(resource.bytes, b"hello brotli");

    handle.join().unwrap();
  }

  #[test]
  fn http_fetcher_allows_204_empty_body() {
    let Some(listener) = try_bind_localhost("http_fetcher_allows_204_empty_body") else {
      return;
    };
    let addr = listener.local_addr().unwrap();

    let handle = thread::spawn(move || {
      if let Some(stream) = listener.incoming().next() {
        let mut stream = stream.unwrap();
        let mut buf = [0u8; 1024];
        let _ = stream.read(&mut buf);
        let headers = b"HTTP/1.1 204 No Content\r\nContent-Length: 0\r\nConnection: close\r\n\r\n";
        let _ = stream.write_all(headers);
      }
    });

    let fetcher = HttpFetcher::new().with_timeout(Duration::from_secs(2));
    let url = format!("http://{}", addr);
    let res = fetcher.fetch(&url).expect("fetch 204");
    handle.join().unwrap();

    assert!(res.bytes.is_empty());
    assert_eq!(res.status, Some(204));
  }

  #[test]
  fn http_fetcher_retries_202_empty_body_then_succeeds() {
    let Some(listener) = try_bind_localhost("http_fetcher_retries_202_empty_body_then_succeeds")
    else {
      return;
    };
    let addr = listener.local_addr().unwrap();

    let handle = thread::spawn(move || {
      for (idx, stream) in listener.incoming().take(2).enumerate() {
        let mut stream = stream.unwrap();
        let mut buf = [0u8; 1024];
        let _ = stream.read(&mut buf);

        if idx == 0 {
          let headers =
            b"HTTP/1.1 202 Accepted\r\nRetry-After: 0\r\nContent-Length: 0\r\nConnection: close\r\n\r\n";
          let _ = stream.write_all(headers);
        } else {
          let body = b"ok";
          let headers = format!(
            "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
            body.len()
          );
          stream.write_all(headers.as_bytes()).unwrap();
          stream.write_all(body).unwrap();
        }
      }
    });

    let retry_policy = HttpRetryPolicy {
      max_attempts: 2,
      backoff_base: Duration::ZERO,
      backoff_cap: Duration::ZERO,
      respect_retry_after: true,
    };
    let fetcher = HttpFetcher::new()
      .with_timeout(Duration::from_secs(2))
      .with_retry_policy(retry_policy);
    let url = format!("http://{}", addr);
    let res = fetcher.fetch(&url).expect("fetch after 202 retry");
    handle.join().unwrap();

    assert_eq!(res.bytes, b"ok");
    assert_eq!(res.status, Some(200));
  }

  #[test]
  fn http_fetcher_returns_ok_on_persistent_202_empty_body_after_retries() {
    let Some(listener) =
      try_bind_localhost("http_fetcher_returns_ok_on_persistent_202_empty_body_after_retries")
    else {
      return;
    };
    let addr = listener.local_addr().unwrap();

    let handle = thread::spawn(move || {
      for stream in listener.incoming().take(2) {
        let mut stream = stream.unwrap();
        let mut buf = [0u8; 1024];
        let _ = stream.read(&mut buf);
        let headers =
          b"HTTP/1.1 202 Accepted\r\nRetry-After: 0\r\nContent-Length: 0\r\nConnection: close\r\n\r\n";
        let _ = stream.write_all(headers);
      }
    });

    let retry_policy = HttpRetryPolicy {
      max_attempts: 2,
      backoff_base: Duration::ZERO,
      backoff_cap: Duration::ZERO,
      respect_retry_after: true,
    };
    let fetcher = HttpFetcher::new()
      .with_timeout(Duration::from_secs(2))
      .with_retry_policy(retry_policy);
    let url = format!("http://{}", addr);
    let res = fetcher
      .fetch(&url)
      .expect("persistent 202 empty body should return Ok");
    handle.join().unwrap();

    assert!(res.bytes.is_empty());
    assert_eq!(res.status, Some(202));
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
  fn fetch_http_honors_retry_after_over_backoff_cap() {
    let Some(listener) = try_bind_localhost("fetch_http_honors_retry_after_over_backoff_cap")
    else {
      return;
    };
    let addr = listener.local_addr().unwrap();

    let handle = std::thread::spawn(move || {
      let mut iter = listener.incoming();

      // First response: 429 with Retry-After.
      if let Some(stream) = iter.next() {
        let mut stream = stream.unwrap();
        let _ = stream.read(&mut [0u8; 1024]);
        let body = b"too many";
        let headers = format!(
          "HTTP/1.1 429 Too Many Requests\r\nRetry-After: 1\r\nContent-Type: text/plain\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
          body.len()
        );
        let _ = stream.write_all(headers.as_bytes());
        let _ = stream.write_all(body);
      }

      // Second response: OK.
      if let Some(stream) = iter.next() {
        let mut stream = stream.unwrap();
        let _ = stream.read(&mut [0u8; 1024]);
        let body = b"ok";
        let headers = format!(
          "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
          body.len()
        );
        let _ = stream.write_all(headers.as_bytes());
        let _ = stream.write_all(body);
      }
    });

    // Use a tiny exponential backoff so the only way to sleep ~1s is via Retry-After.
    let retry_policy = HttpRetryPolicy {
      max_attempts: 2,
      backoff_base: Duration::from_millis(1),
      backoff_cap: Duration::from_millis(5),
      respect_retry_after: true,
    };
    let fetcher = HttpFetcher::new()
      .with_timeout(Duration::from_secs(2))
      .with_retry_policy(retry_policy);

    let url = format!("http://{}/", addr);
    let start = Instant::now();
    let res = fetcher.fetch(&url).expect("fetch should retry after 429");
    let elapsed = start.elapsed();
    handle.join().unwrap();

    assert_eq!(res.bytes, b"ok");
    assert!(
      elapsed >= Duration::from_millis(900),
      "expected Retry-After delay to be respected (elapsed={elapsed:?})"
    );
    assert!(
      elapsed < Duration::from_secs(10),
      "unexpectedly slow retry-after test (elapsed={elapsed:?})"
    );
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
  fn caching_fetcher_falls_back_on_transient_status() {
    let fetcher = ScriptedFetcher::new(vec![
      MockResponse {
        status: 200,
        body: b"cached".to_vec(),
        etag: Some("etag1".to_string()),
        last_modified: None,
        cache_policy: None,
      },
      MockResponse {
        status: 503,
        body: b"transient".to_vec(),
        etag: None,
        last_modified: None,
        cache_policy: None,
      },
      MockResponse {
        status: 200,
        body: b"fresh".to_vec(),
        etag: Some("etag2".to_string()),
        last_modified: None,
        cache_policy: None,
      },
    ]);

    let cache = CachingFetcher::new(fetcher.clone());
    let url = "http://example.com/resource";

    let first = cache.fetch(url).expect("initial fetch");
    assert_eq!(first.bytes, b"cached");

    let second = cache.fetch(url).expect("fallback fetch");
    assert_eq!(
      second.bytes, b"cached",
      "should fall back to cached bytes on transient status"
    );

    let third = cache.fetch(url).expect("fresh fetch");
    assert_eq!(third.bytes, b"fresh");

    assert_eq!(fetcher.calls().len(), 3, "fetcher should be called 3 times");
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
