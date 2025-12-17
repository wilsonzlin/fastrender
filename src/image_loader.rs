//! Image loading and caching
//!
//! This module provides image loading from various sources (HTTP, file, data URLs)
//! with in-memory caching and support for various image formats including SVG.

use crate::error::{Error, ImageError, RenderError, Result};
use crate::resource::{FetchedResource, HttpFetcher, ResourceFetcher};
use crate::style::types::{ImageResolution, OrientationTransform};
use exif;
use image::{imageops, DynamicImage, GenericImageView, RgbaImage};
use std::collections::HashMap;
use std::path::Path;
use std::sync::{Arc, Mutex};
use url::Url;

// ============================================================================
// CachedImage
// ============================================================================

/// Decoded image plus orientation metadata.
pub struct CachedImage {
    pub image: Arc<DynamicImage>,
    pub orientation: Option<OrientationTransform>,
    /// Resolution in image pixels per CSS px (dppx) when provided by metadata.
    pub resolution: Option<f32>,
    /// Whether this image originated from a vector source (SVG).
    pub is_vector: bool,
}

impl CachedImage {
    pub fn dimensions(&self) -> (u32, u32) {
        self.image.dimensions()
    }

    pub fn width(&self) -> u32 {
        self.image.width()
    }

    pub fn height(&self) -> u32 {
        self.image.height()
    }

    pub fn oriented_dimensions(&self, transform: OrientationTransform) -> (u32, u32) {
        let (w, h) = self.dimensions();
        transform.oriented_dimensions(w, h)
    }

    /// Computes CSS pixel dimensions after applying orientation and the provided image-resolution.
    pub fn css_dimensions(
        &self,
        transform: OrientationTransform,
        resolution: &ImageResolution,
        device_pixel_ratio: f32,
        override_resolution: Option<f32>,
    ) -> Option<(f32, f32)> {
        let (w, h) = self.oriented_dimensions(transform);
        if w == 0 || h == 0 {
            return None;
        }
        if self.is_vector {
            return Some((w as f32, h as f32));
        }
        let used = resolution.used_resolution(override_resolution, self.resolution, device_pixel_ratio);
        if used <= 0.0 || !used.is_finite() {
            return None;
        }
        Some((w as f32 / used, h as f32 / used))
    }

    pub fn to_oriented_rgba(&self, transform: OrientationTransform) -> RgbaImage {
        let mut rgba = self.image.to_rgba8();

        match transform.quarter_turns % 4 {
            0 => {}
            1 => rgba = imageops::rotate90(&rgba),
            2 => rgba = imageops::rotate180(&rgba),
            3 => rgba = imageops::rotate270(&rgba),
            _ => {}
        }

        if transform.flip_x {
            rgba = imageops::flip_horizontal(&rgba);
        }

        rgba
    }
}

// ============================================================================
// ImageCache
// ============================================================================

/// Cache for loaded images
///
/// `ImageCache` provides in-memory caching of decoded images, with support for
/// loading from URLs, files, and data URLs. It uses a [`ResourceFetcher`] for
/// the actual byte fetching, allowing custom fetching strategies (caching,
/// mocking, etc.) to be injected.
///
/// # Example
///
/// ```rust,ignore
/// use fastrender::image_loader::ImageCache;
/// use fastrender::resource::HttpFetcher;
/// use std::sync::Arc;
///
/// let fetcher = Arc::new(HttpFetcher::new());
/// let cache = ImageCache::with_fetcher(fetcher);
/// let image = cache.load("https://example.com/image.png")?;
/// ```
pub struct ImageCache {
    /// In-memory cache of decoded images (keyed by resolved URL)
    cache: Arc<Mutex<HashMap<String, Arc<CachedImage>>>>,
    /// Base URL for resolving relative image sources
    base_url: Option<String>,
    /// Resource fetcher for loading bytes from URLs
    fetcher: Arc<dyn ResourceFetcher>,
}

impl ImageCache {
    /// Create a new ImageCache with the default HTTP fetcher
    pub fn new() -> Self {
        Self {
            cache: Arc::new(Mutex::new(HashMap::new())),
            base_url: None,
            fetcher: Arc::new(HttpFetcher::new()),
        }
    }

    /// Create a new ImageCache with a custom fetcher
    pub fn with_fetcher(fetcher: Arc<dyn ResourceFetcher>) -> Self {
        Self {
            cache: Arc::new(Mutex::new(HashMap::new())),
            base_url: None,
            fetcher,
        }
    }

    /// Create a new ImageCache with a base URL and the default HTTP fetcher
    pub fn with_base_url(base_url: String) -> Self {
        Self {
            cache: Arc::new(Mutex::new(HashMap::new())),
            base_url: Some(base_url),
            fetcher: Arc::new(HttpFetcher::new()),
        }
    }

    /// Create a new ImageCache with both a base URL and a custom fetcher
    pub fn with_base_url_and_fetcher(base_url: String, fetcher: Arc<dyn ResourceFetcher>) -> Self {
        Self {
            cache: Arc::new(Mutex::new(HashMap::new())),
            base_url: Some(base_url),
            fetcher,
        }
    }

    /// Sets or replaces the base URL used to resolve relative image sources.
    pub fn set_base_url(&mut self, base_url: impl Into<String>) {
        self.base_url = Some(base_url.into());
    }

    /// Clears any previously configured base URL.
    pub fn clear_base_url(&mut self) {
        self.base_url = None;
    }

    /// Returns the configured base URL for resolving relative paths.
    pub fn base_url(&self) -> Option<String> {
        self.base_url.clone()
    }

    /// Sets the resource fetcher
    pub fn set_fetcher(&mut self, fetcher: Arc<dyn ResourceFetcher>) {
        self.fetcher = fetcher;
    }

    /// Returns a reference to the current fetcher
    pub fn fetcher(&self) -> &Arc<dyn ResourceFetcher> {
        &self.fetcher
    }

    /// Resolve a potentially relative URL to an absolute URL
    fn resolve_url(&self, url: &str) -> String {
        if url.is_empty() {
            return String::new();
        }

        // Absolute or data URLs can be returned directly.
        if url.starts_with("data:") {
            return url.to_string();
        }
        if let Ok(parsed) = url::Url::parse(url) {
            return parsed.to_string();
        }

        // Resolve against the configured base URL when present.
        if let Some(base) = &self.base_url {
            if let Some(resolved) = resolve_against_base(base, url) {
                return resolved;
            }
        }

        // No usable base; return the reference unchanged.
        url.to_string()
    }

    /// Load an image from a URL or file path
    ///
    /// The URL is first resolved against the base URL if one is configured.
    /// Results are cached in memory, so subsequent loads of the same URL
    /// return the cached image.
    pub fn load(&self, url: &str) -> Result<Arc<CachedImage>> {
        // Resolve the URL first
        let resolved_url = self.resolve_url(url);

        // Check cache first (using resolved URL as key)
        {
            let cache = self.cache.lock().unwrap();
            if let Some(img) = cache.get(&resolved_url) {
                return Ok(Arc::clone(img));
            }
        }

        // Fetch the resource bytes
        let resource = self.fetcher.fetch(&resolved_url)?;

        // Decode the image
        let (img, orientation, resolution, is_vector) = self.decode_resource(&resource, &resolved_url)?;

        // Cache it (using resolved URL as key)
        let img_arc = Arc::new(CachedImage {
            image: Arc::new(img),
            orientation,
            resolution,
            is_vector,
        });
        {
            let mut cache = self.cache.lock().unwrap();
            cache.insert(resolved_url, Arc::clone(&img_arc));
        }

        Ok(img_arc)
    }

    /// Render raw SVG content to an image (uncached).
    pub fn render_svg(&self, svg_content: &str) -> Result<Arc<CachedImage>> {
        let img = self.render_svg_to_image(svg_content)?;
        Ok(Arc::new(CachedImage {
            image: Arc::new(img),
            orientation: None,
            resolution: None,
            is_vector: true,
        }))
    }

    /// Decode a fetched resource into an image
    fn decode_resource(
        &self,
        resource: &FetchedResource,
        url: &str,
    ) -> Result<(DynamicImage, Option<OrientationTransform>, Option<f32>, bool)> {
        let bytes = &resource.bytes;
        let content_type = resource.content_type.as_deref();

        // Check if this is SVG
        let mime_is_svg = content_type.map(|m| m.contains("image/svg")).unwrap_or(false);
        let is_svg = mime_is_svg
            || std::str::from_utf8(bytes)
                .ok()
                .map(|s| s.trim_start().starts_with("<svg") || s.trim_start().starts_with("<?xml"))
                .unwrap_or(false);

        if is_svg {
            let content = std::str::from_utf8(bytes).map_err(|e| {
                Error::Image(ImageError::DecodeFailed {
                    url: url.to_string(),
                    reason: format!("SVG not valid UTF-8: {}", e),
                })
            })?;
            return self.render_svg_to_image(content).map(|img| (img, None, None, true));
        }

        // Regular image - extract EXIF metadata and decode
        let (orientation, resolution) = Self::exif_metadata(bytes);
        image::load_from_memory(bytes)
            .map(|img| (img, orientation, resolution, false))
            .map_err(|e| {
                Error::Image(ImageError::DecodeFailed {
                    url: url.to_string(),
                    reason: e.to_string(),
                })
            })
    }

    fn orientation_from_exif(value: u16) -> Option<OrientationTransform> {
        match value {
            1 => Some(OrientationTransform::IDENTITY),
            2 => Some(OrientationTransform {
                quarter_turns: 0,
                flip_x: true,
            }),
            3 => Some(OrientationTransform {
                quarter_turns: 2,
                flip_x: false,
            }),
            4 => Some(OrientationTransform {
                quarter_turns: 2,
                flip_x: true,
            }),
            5 => Some(OrientationTransform {
                quarter_turns: 1,
                flip_x: true,
            }),
            6 => Some(OrientationTransform {
                quarter_turns: 1,
                flip_x: false,
            }),
            7 => Some(OrientationTransform {
                quarter_turns: 3,
                flip_x: true,
            }),
            8 => Some(OrientationTransform {
                quarter_turns: 3,
                flip_x: false,
            }),
            _ => None,
        }
    }

    fn exif_metadata(bytes: &[u8]) -> (Option<OrientationTransform>, Option<f32>) {
        let mut cursor = std::io::Cursor::new(bytes);
        let Ok(exif) = exif::Reader::new().read_from_container(&mut cursor) else {
            return (None, None);
        };

        let orientation = exif
            .get_field(exif::Tag::Orientation, exif::In::PRIMARY)
            .and_then(|f| f.value.get_uint(0))
            .and_then(|v| Self::orientation_from_exif(v as u16));

        let resolution_unit = exif
            .get_field(exif::Tag::ResolutionUnit, exif::In::PRIMARY)
            .and_then(|f| f.value.get_uint(0))
            .unwrap_or(0);

        let rational_to_f32 = |r: exif::Rational| -> Option<f32> {
            if r.denom == 0 {
                None
            } else {
                Some(r.num as f32 / r.denom as f32)
            }
        };

        let x_res = exif
            .get_field(exif::Tag::XResolution, exif::In::PRIMARY)
            .and_then(|f| {
                if let exif::Value::Rational(ref vals) = f.value {
                    vals.get(0).cloned()
                } else {
                    None
                }
            })
            .and_then(rational_to_f32);
        let y_res = exif
            .get_field(exif::Tag::YResolution, exif::In::PRIMARY)
            .and_then(|f| {
                if let exif::Value::Rational(ref vals) = f.value {
                    vals.get(0).cloned()
                } else {
                    None
                }
            })
            .and_then(rational_to_f32);
        let avg_res = match (x_res, y_res) {
            (Some(x), Some(y)) if x.is_finite() && y.is_finite() && x > 0.0 && y > 0.0 => Some((x + y) / 2.0),
            (Some(v), None) | (None, Some(v)) if v.is_finite() && v > 0.0 => Some(v),
            _ => None,
        };

        let resolution = avg_res.and_then(|res| match resolution_unit {
            2 => Some(res / 96.0),          // inch -> dppx
            3 => Some((res * 2.54) / 96.0), // cm -> dppx
            _ => None,
        });

        (orientation, resolution)
    }

    #[allow(dead_code)]
    fn exif_orientation(bytes: &[u8]) -> Option<OrientationTransform> {
        Self::exif_metadata(bytes).0
    }

    /// Renders raw SVG content to a raster image.
    pub fn render_svg_to_image(&self, svg_content: &str) -> Result<DynamicImage> {
        use resvg::usvg;

        // Parse SVG
        let options = usvg::Options::default();
        let tree = usvg::Tree::from_str(svg_content, &options).map_err(|e| {
            Error::Image(ImageError::DecodeFailed {
                url: "SVG content".to_string(),
                reason: format!("Failed to parse SVG: {}", e),
            })
        })?;

        let size = tree.size();
        let width = size.width() as u32;
        let height = size.height() as u32;

        // Handle zero-size SVGs
        if width == 0 || height == 0 {
            return Err(Error::Render(RenderError::CanvasCreationFailed { width, height }));
        }

        // Render SVG to pixmap
        let mut pixmap = tiny_skia::Pixmap::new(width, height)
            .ok_or(Error::Render(RenderError::CanvasCreationFailed { width, height }))?;

        resvg::render(&tree, tiny_skia::Transform::identity(), &mut pixmap.as_mut());

        // Convert pixmap to image
        let rgba_data = pixmap.take();
        let img = image::RgbaImage::from_raw(width, height, rgba_data).ok_or_else(|| {
            Error::Image(ImageError::DecodeFailed {
                url: "SVG content".to_string(),
                reason: "Failed to create image from SVG pixmap".to_string(),
            })
        })?;

        Ok(image::DynamicImage::ImageRgba8(img))
    }
}

// ============================================================================
// URL Resolution
// ============================================================================

fn resolve_against_base(base: &str, reference: &str) -> Option<String> {
    // Normalize file:// bases that point to directories so Url::join keeps the directory segment.
    let mut base_candidate = base.to_string();
    if base_candidate.starts_with("file://") {
        let path = &base_candidate["file://".len()..];
        if Path::new(path).is_dir() && !base_candidate.ends_with('/') {
            base_candidate.push('/');
        }
    }

    let mut base_url = Url::parse(&base_candidate)
        .or_else(|_| Url::from_file_path(&base_candidate).map_err(|_| url::ParseError::RelativeUrlWithoutBase))
        .ok()?;

    if base_url.scheme() == "file" {
        if let Ok(path) = base_url.to_file_path() {
            if path.is_dir() && !base_url.path().ends_with('/') {
                let mut path_str = base_url.path().to_string();
                path_str.push('/');
                base_url.set_path(&path_str);
            }
        }
    }

    base_url.join(reference).ok().map(|u| u.to_string())
}

// ============================================================================
// Trait Implementations
// ============================================================================

impl Default for ImageCache {
    fn default() -> Self {
        Self::new()
    }
}

impl Clone for ImageCache {
    fn clone(&self) -> Self {
        Self {
            cache: Arc::clone(&self.cache),
            base_url: self.base_url.clone(),
            fetcher: Arc::clone(&self.fetcher),
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use image::RgbaImage;
    use std::path::PathBuf;
    use std::time::SystemTime;

    #[test]
    fn render_inline_svg_returns_image() {
        let cache = ImageCache::new();
        let svg = r#"<svg xmlns="http://www.w3.org/2000/svg" width="10" height="5"></svg>"#;
        let image = cache.render_svg_to_image(svg).expect("svg render");
        assert_eq!(image.width(), 10);
        assert_eq!(image.height(), 5);
    }

    #[test]
    fn load_svg_data_url() {
        let cache = ImageCache::new();
        let data_url =
            "data:image/svg+xml,%3Csvg%20xmlns=%22http://www.w3.org/2000/svg%22%20width=%221%22%20height=%221%22%3E%3C/svg%3E";
        let image = cache.load(data_url).expect("decode data URL");
        assert_eq!(image.width(), 1);
        assert_eq!(image.height(), 1);
    }

    #[test]
    fn exposes_exif_orientation() {
        let cache = ImageCache::new();
        let image = cache
            .load("tests/fixtures/image_orientation/orientation-6.jpg")
            .expect("load oriented image");
        assert_eq!(
            image.orientation,
            Some(OrientationTransform {
                quarter_turns: 1,
                flip_x: false
            })
        );
    }

    #[test]
    fn resolves_relative_urls_against_base() {
        let mut cache = ImageCache::new();
        let mut path: PathBuf = std::env::temp_dir();
        path.push(format!(
            "fastrender_base_url_test_{}_{}.png",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        let dir = path.parent().unwrap().to_path_buf();
        let image = RgbaImage::from_raw(1, 1, vec![255, 0, 0, 255]).expect("build 1x1");
        image.save(&path).expect("encode png");
        let base_url = format!("file://{}", dir.display());
        cache.set_base_url(base_url);

        let image = cache
            .load(path.file_name().unwrap().to_str().unwrap())
            .expect("load via base");
        assert_eq!(image.width(), 1);
        assert_eq!(image.height(), 1);
    }

    #[test]
    fn resolves_relative_paths_against_http_base() {
        let cache = ImageCache::with_base_url("https://example.com/a/b/".to_string());
        assert_eq!(
            cache.resolve_url("../img.png"),
            "https://example.com/a/img.png".to_string()
        );
        assert_eq!(
            cache.resolve_url("./nested/icon.png"),
            "https://example.com/a/b/nested/icon.png".to_string()
        );
    }

    #[test]
    fn resolves_protocol_relative_urls_using_base_scheme() {
        let cache = ImageCache::with_base_url("https://example.com/base/".to_string());
        assert_eq!(
            cache.resolve_url("//cdn.example.com/asset.png"),
            "https://cdn.example.com/asset.png".to_string()
        );
    }

    #[test]
    fn resolves_file_base_without_trailing_slash_as_directory() {
        let mut dir: PathBuf = std::env::temp_dir();
        dir.push(format!(
            "fastrender_url_base_{}_{}",
            std::process::id(),
            SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        std::fs::create_dir_all(dir.join("assets")).expect("create temp dir");
        let base = format!("file://{}", dir.display());
        let cache = ImageCache::with_base_url(base);

        let resolved = cache.resolve_url("assets/image.png");
        assert!(
            resolved.ends_with("/assets/image.png"),
            "resolved path should keep directory: {}",
            resolved
        );

        std::fs::remove_dir_all(&dir).ok();
    }

    #[test]
    fn with_fetcher_uses_custom_fetcher() {
        use std::sync::atomic::{AtomicUsize, Ordering};

        struct CountingFetcher {
            count: AtomicUsize,
        }

        impl ResourceFetcher for CountingFetcher {
            fn fetch(&self, _url: &str) -> Result<FetchedResource> {
                self.count.fetch_add(1, Ordering::SeqCst);
                // Return a minimal valid PNG
                let png_data = vec![
                    0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A, // PNG signature
                    0x00, 0x00, 0x00, 0x0D, 0x49, 0x48, 0x44, 0x52, // IHDR chunk
                    0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01, // 1x1
                    0x08, 0x02, 0x00, 0x00, 0x00, 0x90, 0x77, 0x53, 0xDE, 0x00, 0x00, 0x00, 0x0C, 0x49, 0x44, 0x41,
                    0x54, 0x08, 0xD7, 0x63, 0xF8, 0xFF, 0xFF, 0x3F, 0x00, 0x05, 0xFE, 0x02, 0xFE, 0xDC, 0xCC, 0x59,
                    0xE7, 0x00, 0x00, 0x00, 0x00, 0x49, 0x45, 0x4E, 0x44, 0xAE, 0x42, 0x60, 0x82,
                ];
                Ok(FetchedResource::new(png_data, Some("image/png".to_string())))
            }
        }

        let fetcher = Arc::new(CountingFetcher {
            count: AtomicUsize::new(0),
        });
        let cache = ImageCache::with_fetcher(Arc::clone(&fetcher) as Arc<dyn ResourceFetcher>);

        let _ = cache.load("test://image.png");
        assert_eq!(fetcher.count.load(Ordering::SeqCst), 1);

        // Second load should use cache
        let _ = cache.load("test://image.png");
        assert_eq!(fetcher.count.load(Ordering::SeqCst), 1);

        // Different URL should fetch again
        let _ = cache.load("test://other.png");
        assert_eq!(fetcher.count.load(Ordering::SeqCst), 2);
    }
}
