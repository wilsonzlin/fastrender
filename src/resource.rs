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

/// Default User-Agent string used by HTTP fetchers
pub const DEFAULT_USER_AGENT: &str =
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0 Safari/537.36 fastrender/0.1";

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

    /// Set the maximum response size in bytes
    pub fn with_max_size(mut self, max_size: usize) -> Self {
        self.max_size = max_size;
        self
    }

    /// Fetch from an HTTP/HTTPS URL
    fn fetch_http(&self, url: &str) -> Result<FetchedResource> {
        let config = ureq::Agent::config_builder().timeout_global(Some(self.timeout)).build();
        let agent: ureq::Agent = config.into();

        let mut response = agent
            .get(url)
            .header("User-Agent", &self.user_agent)
            .call()
            .map_err(|e| Error::Io(io::Error::new(io::ErrorKind::Other, e.to_string())))?;

        let content_type = response
            .headers()
            .get("content-type")
            .and_then(|h| h.to_str().ok())
            .map(|s| s.to_string());

        let bytes = response
            .body_mut()
            .with_config()
            .limit(self.max_size as u64)
            .read_to_vec()
            .map_err(|e| Error::Io(io::Error::new(io::ErrorKind::Other, e.to_string())))?;

        Ok(FetchedResource::new(bytes, content_type))
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
}
