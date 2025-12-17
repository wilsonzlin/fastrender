//! Caching wrapper for ResourceFetcher
//!
//! This module provides a disk-caching wrapper around any ResourceFetcher.
//! It's intended for use in test/dev binaries, NOT the library itself.

use fastrender::resource::{FetchedResource, ResourceFetcher};
use fastrender::Result;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::fs;
use std::path::{Path, PathBuf};

/// A caching wrapper around any ResourceFetcher
///
/// Caches fetched resources to disk and serves them from cache on subsequent requests.
/// This is useful for speeding up repeated renders of the same pages.
///
/// # Example
///
/// ```rust,ignore
/// use fastrender::resource::HttpFetcher;
/// use std::sync::Arc;
///
/// let inner = HttpFetcher::new();
/// let caching = CachingFetcher::new(inner, "fetches/assets");
/// let fetcher: Arc<dyn ResourceFetcher> = Arc::new(caching);
/// ```
pub struct CachingFetcher<F: ResourceFetcher> {
    inner: F,
    cache_dir: PathBuf,
}

impl<F: ResourceFetcher> CachingFetcher<F> {
    /// Create a new caching fetcher
    ///
    /// The cache directory will be created if it doesn't exist.
    pub fn new(inner: F, cache_dir: impl Into<PathBuf>) -> Self {
        let cache_dir = cache_dir.into();
        let _ = std::fs::create_dir_all(&cache_dir);
        Self { inner, cache_dir }
    }

    /// Convert a URL to a safe cache filename
    ///
    /// Uses a hash of the URL for uniqueness, preserving the extension for MIME hints.
    fn url_to_cache_path(&self, url: &str) -> PathBuf {
        let mut hasher = DefaultHasher::new();
        url.hash(&mut hasher);
        let hash = hasher.finish();

        // Try to preserve extension
        let ext = url
            .rsplit('/')
            .next()
            .and_then(|s| s.split('?').next())
            .and_then(|s| s.rsplit('.').next())
            .filter(|e| e.len() <= 5 && e.chars().all(|c| c.is_ascii_alphanumeric()))
            .unwrap_or("bin");

        self.cache_dir.join(format!("{:016x}.{}", hash, ext))
    }

    /// Guess content-type from file extension
    fn content_type_from_ext(path: &Path) -> Option<String> {
        let ext = path.extension()?.to_str()?.to_lowercase();
        let mime = match ext.as_str() {
            "png" => "image/png",
            "jpg" | "jpeg" => "image/jpeg",
            "gif" => "image/gif",
            "webp" => "image/webp",
            "svg" => "image/svg+xml",
            "ico" => "image/x-icon",
            "css" => "text/css",
            "js" => "application/javascript",
            "html" | "htm" => "text/html",
            "json" => "application/json",
            "woff" => "font/woff",
            "woff2" => "font/woff2",
            "ttf" => "font/ttf",
            _ => return None,
        };
        Some(mime.to_string())
    }

    fn meta_path(cache_path: &Path) -> PathBuf {
        let mut meta = cache_path.to_owned();
        let meta_ext = cache_path
            .extension()
            .and_then(|e| e.to_str())
            .map(|ext| format!("{ext}.meta"))
            .unwrap_or_else(|| "meta".to_string());
        meta.set_extension(meta_ext);
        meta
    }
}

impl<F: ResourceFetcher> ResourceFetcher for CachingFetcher<F> {
    fn fetch(&self, url: &str) -> Result<FetchedResource> {
        // Don't cache data: URLs (they're already embedded)
        if url.starts_with("data:") {
            return self.inner.fetch(url);
        }

        let cache_path = self.url_to_cache_path(url);

        // Try to load from cache
        if cache_path.exists() {
            if let Ok(bytes) = fs::read(&cache_path) {
                let content_type = fs::read_to_string(Self::meta_path(&cache_path))
                    .ok()
                    .map(|s| s.trim().to_string())
                    .filter(|s| !s.is_empty())
                    .or_else(|| Self::content_type_from_ext(&cache_path));
                return Ok(FetchedResource::new(bytes, content_type));
            }
        }

        // Fetch from inner fetcher
        let resource = self.inner.fetch(url)?;

        // Save to cache (best effort - don't fail if write fails)
        let _ = fs::write(&cache_path, &resource.bytes);
        if let Some(ct) = &resource.content_type {
            let _ = fs::write(Self::meta_path(&cache_path), ct);
        }

        Ok(resource)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fastrender::resource::FetchedResource;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Arc;

    #[derive(Clone)]
    struct DummyFetcher {
        count: Arc<AtomicUsize>,
        bytes: Vec<u8>,
        content_type: Option<String>,
    }

    impl ResourceFetcher for DummyFetcher {
        fn fetch(&self, _url: &str) -> fastrender::Result<FetchedResource> {
            self.count.fetch_add(1, Ordering::SeqCst);
            Ok(FetchedResource::new(
                self.bytes.clone(),
                self.content_type.clone(),
            ))
        }
    }

    #[test]
    fn caches_and_restores_content_type_without_extension() {
        let tmp = tempfile::tempdir().expect("temp dir");
        let counter = Arc::new(AtomicUsize::new(0));
        let fetcher = DummyFetcher {
            count: counter.clone(),
            bytes: b"hi".to_vec(),
            content_type: Some("text/plain".to_string()),
        };

        let caching = CachingFetcher::new(fetcher, tmp.path());
        let url = "https://example.com/resource"; // no extension -> would map to .bin

        let first = caching.fetch(url).expect("first fetch");
        assert_eq!(first.content_type.as_deref(), Some("text/plain"));
        assert_eq!(counter.load(Ordering::SeqCst), 1);

        let second = caching.fetch(url).expect("cached fetch");
        assert_eq!(second.content_type.as_deref(), Some("text/plain"));
        assert_eq!(counter.load(Ordering::SeqCst), 1, "should hit cache");
    }

    #[test]
    fn falls_back_to_extension_when_no_content_type() {
        let tmp = tempfile::tempdir().expect("temp dir");
        let counter = Arc::new(AtomicUsize::new(0));
        let fetcher = DummyFetcher {
            count: counter.clone(),
            bytes: b"pngdata".to_vec(),
            content_type: None,
        };

        let caching = CachingFetcher::new(fetcher, tmp.path());
        let url = "https://example.com/image.png";

        let first = caching.fetch(url).expect("first fetch");
        assert_eq!(first.content_type.as_deref(), None);
        assert_eq!(counter.load(Ordering::SeqCst), 1);

        let second = caching.fetch(url).expect("cached fetch");
        // Meta is absent, but extension should still provide a MIME guess.
        assert_eq!(second.content_type.as_deref(), Some("image/png"));
        assert_eq!(counter.load(Ordering::SeqCst), 1, "should hit cache");
    }
}

fn main() {
    eprintln!("caching_fetcher is a library-only helper; use it by wrapping a ResourceFetcher in code.");
}
