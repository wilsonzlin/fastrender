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
//! ```rust,ignore
//! use fastrender::resource::{ResourceFetcher, HttpFetcher};
//!
//! let fetcher = HttpFetcher::new();
//! let resource = fetcher.fetch("https://example.com/image.png")?;
//! println!("Got {} bytes", resource.bytes.len());
//! ```

use crate::error::{Error, ImageError, Result};
use std::io;
use std::path::Path;
use std::sync::Arc;
use std::time::Duration;
use url::Url;

/// Normalize a page identifier (full URL or hostname) to a cache/output stem.
///
/// Strips schemes and leading "www.", lowercases the host, and sanitizes for filenames.
pub fn normalize_page_name(raw: &str) -> Option<String> {
    let trimmed = raw.trim();
    if trimmed.is_empty() {
        return None;
    }

    // Remove scheme prefixes if present, case-insensitively.
    let no_scheme = trimmed.trim_start_matches("https://").trim_start_matches("http://");

    // Strip a leading www. (case-insensitive) to align with cache naming expectations.
    let without_www = if no_scheme.len() >= 4 && no_scheme[..4].eq_ignore_ascii_case("www.") {
        &no_scheme[4..]
    } else {
        no_scheme
    };

    Some(url_to_filename(without_www))
}

/// Normalize a URL into a filename-safe stem used for caches and outputs.
pub fn url_to_filename(url: &str) -> String {
    // First, try to parse the URL so we can lowercase the hostname (case-insensitive per URL
    // spec) and strip the scheme regardless of casing. If parsing fails, fall back to a best-effort
    // scheme-stripping path similar to the old behavior.
    if let Ok(parsed) = Url::parse(url) {
        let mut stem = String::new();
        if let Some(host) = parsed.host_str() {
            stem.push_str(&host.to_ascii_lowercase());
        }
        // Preserve path/query casing while normalizing separators later.
        stem.push_str(&parsed[url::Position::BeforePath..url::Position::AfterQuery]);
        return sanitize_filename(&stem);
    }

    // Fallback: remove common schemes case-insensitively and lowercase only the hostname portion.
    let mut trimmed = url;
    for scheme in ["https://", "http://"] {
        if url.len() >= scheme.len() && url[..scheme.len()].eq_ignore_ascii_case(scheme) {
            trimmed = &url[scheme.len()..];
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
    let trimmed = input.trim_end_matches('/');
    let mut sanitized: String = trimmed
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

    // Avoid a trailing underscore when the path ends with a slash (common for canonical URLs).
    while sanitized.ends_with('_') {
        sanitized.pop();
    }

    sanitized
}

/// Default User-Agent string used by HTTP fetchers
pub const DEFAULT_USER_AGENT: &str =
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0 Safari/537.36 fastrender/0.1";

/// Default Accept-Language header value
pub const DEFAULT_ACCEPT_LANGUAGE: &str = "en-US,en;q=0.9";

/// Strip a leading "User-Agent:" prefix so logs don't double-prefix when callers
/// pass a full header value.
pub fn normalize_user_agent_for_log(ua: &str) -> &str {
    ua.strip_prefix("User-Agent:")
        .map(str::trim)
        .filter(|s| !s.is_empty())
        .unwrap_or(ua)
}

// ============================================================================
// Core types
// ============================================================================

/// Result of fetching an external resource
#[derive(Debug, Clone)]
pub struct FetchedResource {
    /// Raw bytes of the resource
    pub bytes: Vec<u8>,
    /// Content-Type header value, if available (e.g., "image/png", "text/css")
    pub content_type: Option<String>,
}

impl FetchedResource {
    /// Create a new FetchedResource
    pub fn new(bytes: Vec<u8>, content_type: Option<String>) -> Self {
        Self { bytes, content_type }
    }

    /// Check if this resource appears to be an image based on content-type
    pub fn is_image(&self) -> bool {
        self.content_type
            .as_ref()
            .map(|ct| ct.starts_with("image/"))
            .unwrap_or(false)
    }

    /// Check if this resource appears to be CSS based on content-type
    pub fn is_css(&self) -> bool {
        self.content_type
            .as_ref()
            .map(|ct| ct.contains("text/css"))
            .unwrap_or(false)
    }

    /// Check if this resource appears to be SVG
    pub fn is_svg(&self) -> bool {
        self.content_type
            .as_ref()
            .map(|ct| ct.contains("image/svg"))
            .unwrap_or(false)
    }
}

/// Parses cached HTML metadata sidecars.
///
/// Supports the legacy format where the meta file contains only the content-type
/// string, and a key/value format where lines are prefixed with `content-type:`
/// and `url:`. Returns `(content_type, url)`.
pub fn parse_cached_html_meta(meta: &str) -> (Option<String>, Option<String>) {
    let trimmed = meta.trim();
    if trimmed.is_empty() {
        return (None, None);
    }

    let mut content_type: Option<String> = None;
    let mut url: Option<String> = None;

    for line in meta.lines() {
        let mut parts = line.splitn(2, ':');
        let key = parts.next().map(|s| s.trim().to_ascii_lowercase());
        let value = parts.next().map(|s| s.trim());
        match (key.as_deref(), value) {
            (Some("content-type"), Some(v)) if !v.is_empty() => content_type = Some(v.to_string()),
            (Some("url"), Some(v)) if !v.is_empty() => url = Some(v.to_string()),
            _ => {}
        }
    }

    if content_type.is_none() && url.is_none() && !trimmed.contains('\n') {
        return (Some(trimmed.to_string()), None);
    }

    (content_type, url)
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
}

// Allow Arc<dyn ResourceFetcher> to be used as ResourceFetcher
impl<T: ResourceFetcher + ?Sized> ResourceFetcher for Arc<T> {
    fn fetch(&self, url: &str) -> Result<FetchedResource> {
        (**self).fetch(url)
    }
}

// ============================================================================
// HttpFetcher - Default implementation
// ============================================================================

/// Default HTTP resource fetcher
///
/// Fetches resources over HTTP/HTTPS with configurable timeouts and user agent.
/// Also handles `file://` URLs and `data:` URLs.
///
/// # Example
///
/// ```rust,ignore
/// use fastrender::resource::HttpFetcher;
/// use std::time::Duration;
///
/// let fetcher = HttpFetcher::new()
///     .with_timeout(Duration::from_secs(60))
///     .with_user_agent("MyApp/1.0");
/// ```
#[derive(Debug, Clone)]
pub struct HttpFetcher {
    timeout: Duration,
    user_agent: String,
    accept_language: String,
    max_size: usize,
}

impl HttpFetcher {
    /// Create a new HttpFetcher with default settings
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the request timeout
    pub fn with_timeout(mut self, timeout: Duration) -> Self {
        self.timeout = timeout;
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
        self.max_size = max_size;
        self
    }

    /// Fetch from an HTTP/HTTPS URL
    fn fetch_http(&self, url: &str) -> Result<FetchedResource> {
        self.fetch_http_with_accept(url, None)
    }

    fn fetch_http_with_accept(&self, url: &str, accept_encoding: Option<&str>) -> Result<FetchedResource> {
        let config = ureq::Agent::config_builder().timeout_global(Some(self.timeout)).build();
        let agent: ureq::Agent = config.into();

        let mut current = url.to_string();
        for _ in 0..10 {
            let mut request = agent
                .get(&current)
                .header("User-Agent", &self.user_agent)
                .header("Accept-Language", &self.accept_language);

            if let Some(enc) = accept_encoding {
                request = request.header("Accept-Encoding", enc);
            }

            let mut response = request
                .call()
                .map_err(|e| Error::Io(io::Error::new(io::ErrorKind::Other, e.to_string())))?;

            let status = response.status();
            if (300..400).contains(&status.as_u16()) {
                if let Some(loc) = response.headers().get("location").and_then(|h| h.to_str().ok()) {
                    let next = Url::parse(&current)
                        .ok()
                        .and_then(|base| base.join(loc).ok())
                        .map(|u| u.to_string())
                        .unwrap_or_else(|| loc.to_string());
                    current = next;
                    continue;
                }
            }

            let content_type = response
                .headers()
                .get("content-type")
                .and_then(|h| h.to_str().ok())
                .map(|s| s.to_string());

            let body_result = response
                .body_mut()
                .with_config()
                .limit(self.max_size as u64)
                .read_to_vec()
                .map_err(|e| e.into_io());

            match body_result {
                Ok(bytes) => {
                    if bytes.is_empty() {
                        return Err(Error::Io(io::Error::new(
                            io::ErrorKind::UnexpectedEof,
                            "Empty HTTP response body",
                        )));
                    }
                    return Ok(FetchedResource::new(bytes, content_type));
                }
                Err(err) if accept_encoding.is_none() && is_decompression_error(&err) => {
                    return self.fetch_http_with_accept(&current, Some("identity"));
                }
                Err(err) => return Err(Error::Io(err)),
            }
        }

        Err(Error::Io(io::Error::new(io::ErrorKind::Other, "too many redirects")))
    }

    /// Fetch from a file:// URL
    fn fetch_file(&self, url: &str) -> Result<FetchedResource> {
        let path = url.strip_prefix("file://").unwrap_or(url);
        let bytes = std::fs::read(path).map_err(|e| {
            Error::Image(ImageError::LoadFailed {
                url: url.to_string(),
                reason: e.to_string(),
            })
        })?;

        let content_type = guess_content_type_from_path(path);
        Ok(FetchedResource::new(bytes, content_type))
    }

    /// Decode a data: URL
    fn fetch_data(&self, url: &str) -> Result<FetchedResource> {
        decode_data_url(url)
    }
}

impl Default for HttpFetcher {
    fn default() -> Self {
        Self {
            timeout: Duration::from_secs(30),
            user_agent: DEFAULT_USER_AGENT.to_string(),
            accept_language: DEFAULT_ACCEPT_LANGUAGE.to_string(),
            max_size: 50 * 1024 * 1024, // 50MB default limit
        }
    }
}

impl ResourceFetcher for HttpFetcher {
    fn fetch(&self, url: &str) -> Result<FetchedResource> {
        if url.starts_with("data:") {
            self.fetch_data(url)
        } else if url.starts_with("file://") {
            self.fetch_file(url)
        } else if url.starts_with("http://") || url.starts_with("https://") {
            self.fetch_http(url)
        } else {
            // Treat as local file path
            self.fetch_file(&format!("file://{}", url))
        }
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

    let rest = &url["data:".len()..];
    let comma_pos = rest.find(',').ok_or_else(|| {
        Error::Image(ImageError::InvalidDataUrl {
            reason: "Missing comma in data URL".to_string(),
        })
    })?;

    let header = &rest[..comma_pos];
    let data = &rest[comma_pos + 1..];

    // Parse header: [mediatype][;base64]
    let is_base64 = header.ends_with(";base64") || header.contains(";base64;");
    let media_type = header
        .split(';')
        .next()
        .filter(|s| !s.is_empty() && s.contains('/'))
        .map(|s| s.to_string());

    let bytes = if is_base64 {
        use base64::Engine;
        base64::engine::general_purpose::STANDARD.decode(data).map_err(|e| {
            Error::Image(ImageError::InvalidDataUrl {
                reason: format!("Invalid base64: {}", e),
            })
        })?
    } else {
        // URL-encoded
        percent_decode(data)?
    };

    Ok(FetchedResource::new(bytes, media_type))
}

/// Percent-decode a string to bytes
fn percent_decode(input: &str) -> Result<Vec<u8>> {
    let mut out = Vec::with_capacity(input.len());
    let bytes = input.as_bytes();
    let mut i = 0;

    while i < bytes.len() {
        if bytes[i] == b'%' {
            if i + 2 >= bytes.len() {
                return Err(Error::Image(ImageError::InvalidDataUrl {
                    reason: "Incomplete percent-escape".to_string(),
                }));
            }
            let hi = (bytes[i + 1] as char).to_digit(16);
            let lo = (bytes[i + 2] as char).to_digit(16);
            match (hi, lo) {
                (Some(hi), Some(lo)) => {
                    out.push(((hi << 4) | lo) as u8);
                    i += 3;
                }
                _ => {
                    return Err(Error::Image(ImageError::InvalidDataUrl {
                        reason: "Invalid percent-escape".to_string(),
                    }))
                }
            }
        } else if bytes[i] == b'+' {
            out.push(b' ');
            i += 1;
        } else {
            out.push(bytes[i]);
            i += 1;
        }
    }

    Ok(out)
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::{Read, Write};
    use std::net::TcpListener;
    use std::sync::{Arc, Mutex};
    use std::thread;

    #[test]
    fn url_to_filename_normalizes_scheme_and_slashes() {
        assert_eq!(url_to_filename("https://example.com/foo/bar"), "example.com_foo_bar");
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
    fn url_to_filename_is_case_insensitive_for_scheme_and_host() {
        assert_eq!(
            url_to_filename("HTTP://WWW.Example.COM/Path/Up"),
            "www.example.com_Path_Up"
        );
    }

    #[test]
    fn url_to_filename_parses_uppercase_scheme() {
        assert_eq!(url_to_filename("HTTPS://Example.com/q?a=1"), "example.com_q_a_1");
    }

    #[test]
    fn url_to_filename_trims_trailing_slashes() {
        assert_eq!(url_to_filename("https://example.com/"), "example.com");
        assert_eq!(url_to_filename("https://example.com/foo/"), "example.com_foo");
    }

    #[test]
    fn normalize_page_name_handles_urls_and_hosts() {
        assert_eq!(normalize_page_name("https://www.w3.org").as_deref(), Some("w3.org"));
        assert_eq!(
            normalize_page_name("http://example.com/foo").as_deref(),
            Some("example.com_foo")
        );
        assert_eq!(normalize_page_name("example.com").as_deref(), Some("example.com"));
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
    fn normalize_page_name_strips_www_case_insensitively() {
        assert_eq!(normalize_page_name("WWW.Example.com").as_deref(), Some("example.com"));
        assert_eq!(normalize_page_name("www.example.com").as_deref(), Some("example.com"));
    }

    #[test]
    fn url_to_filename_drops_fragments() {
        assert_eq!(url_to_filename("https://example.com/path#frag"), "example.com_path");
        assert_eq!(
            url_to_filename("https://example.com/path?q=1#frag"),
            "example.com_path_q_1"
        );
    }

    #[test]
    fn url_to_filename_drops_fragments_without_scheme() {
        assert_eq!(url_to_filename("Example.Com/path#frag"), "example.com_path");
        assert_eq!(url_to_filename("Example.Com/path?q=1#frag"), "example.com_path_q_1");
    }

    #[test]
    fn url_to_filename_strips_fragments_with_uppercase_scheme() {
        assert_eq!(url_to_filename("HTTP://Example.com/Path#Frag"), "example.com_Path");
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
        let resource = decode_data_url(url).unwrap();
        assert_eq!(resource.bytes, b"hello");
        assert_eq!(resource.content_type, Some("image/png".to_string()));
    }

    #[test]
    fn test_decode_data_url_percent() {
        let url = "data:text/plain,hello%20world";
        let resource = decode_data_url(url).unwrap();
        assert_eq!(resource.bytes, b"hello world");
        assert_eq!(resource.content_type, Some("text/plain".to_string()));
    }

    #[test]
    fn test_decode_data_url_no_mediatype() {
        let url = "data:,hello";
        let resource = decode_data_url(url).unwrap();
        assert_eq!(resource.bytes, b"hello");
        assert_eq!(resource.content_type, None);
    }

    #[test]
    fn parse_cached_meta_supports_legacy_content_type() {
        let (ct, url) = parse_cached_html_meta("text/html; charset=utf-8");
        assert_eq!(ct.as_deref(), Some("text/html; charset=utf-8"));
        assert_eq!(url, None);
    }

    #[test]
    fn parse_cached_meta_reads_key_value_lines() {
        let meta = "content-type: text/html\nurl: https://example.com/page\n";
        let (ct, url) = parse_cached_html_meta(meta);
        assert_eq!(ct.as_deref(), Some("text/html"));
        assert_eq!(url.as_deref(), Some("https://example.com/page"));
    }

    #[test]
    fn http_fetcher_follows_redirects() {
        let listener = TcpListener::bind("127.0.0.1:0").expect("bind redirect server");
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
    }

    #[test]
    fn http_fetcher_sets_accept_language() {
        let listener = TcpListener::bind("127.0.0.1:0").expect("bind lang server");
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
    fn test_http_fetcher_defaults() {
        let fetcher = HttpFetcher::new();
        assert_eq!(fetcher.timeout, Duration::from_secs(30));
        assert!(fetcher.user_agent.contains("fastrender"));
    }

    #[test]
    fn test_http_fetcher_builder() {
        let fetcher = HttpFetcher::new()
            .with_timeout(Duration::from_secs(60))
            .with_user_agent("Test/1.0")
            .with_max_size(1024);

        assert_eq!(fetcher.timeout, Duration::from_secs(60));
        assert_eq!(fetcher.user_agent, "Test/1.0");
        assert_eq!(fetcher.max_size, 1024);
    }

    #[test]
    fn test_fetch_data_url() {
        let fetcher = HttpFetcher::new();
        let resource = fetcher.fetch("data:text/plain,test").unwrap();
        assert_eq!(resource.bytes, b"test");
    }

    #[test]
    fn fetch_http_retries_on_bad_gzip() {
        use std::io::{Read, Write};
        use std::net::TcpListener;
        use std::thread;
        use std::time::{Duration, Instant};

        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
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
        use std::net::TcpListener;

        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
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
}
