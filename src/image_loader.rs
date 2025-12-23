//! Image loading and caching
//!
//! This module provides image loading from various sources (HTTP, file, data URLs)
//! with in-memory caching and support for various image formats including SVG.

use crate::error::Error;
use crate::error::ImageError;
use crate::error::RenderError;
use crate::error::Result;
use crate::resource::CachingFetcher;
use crate::resource::CachingFetcherConfig;
use crate::resource::FetchedResource;
use crate::resource::HttpFetcher;
use crate::resource::ResourceFetcher;
use crate::style::types::ImageResolution;
use crate::style::types::OrientationTransform;
use avif_decode::Decoder as AvifDecoder;
use avif_decode::Image as AvifImage;
use exif;
use image::imageops;
use image::DynamicImage;
use image::GenericImageView;
use image::ImageDecoder;
use image::ImageFormat;
use image::RgbaImage;
use std::convert::TryFrom;
use roxmltree::Document;
use std::collections::HashMap;
use std::io::Cursor;
use std::path::Path;
use std::sync::Arc;
use std::sync::Condvar;
use std::sync::Mutex;
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
  /// Intrinsic aspect ratio when known. SVGs that opt out of aspect-ratio preservation keep this
  /// as `None` and set `aspect_ratio_none` to true.
  pub intrinsic_ratio: Option<f32>,
  /// True when the resource explicitly disables aspect-ratio preservation (e.g., SVG
  /// `preserveAspectRatio="none"`).
  pub aspect_ratio_none: bool,
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

  /// Intrinsic aspect ratio, adjusted for EXIF orientation when present.
  pub fn intrinsic_ratio(&self, transform: OrientationTransform) -> Option<f32> {
    if self.aspect_ratio_none {
      return None;
    }

    let mut ratio = self.intrinsic_ratio;
    if ratio.is_none() {
      let (w, h) = self.oriented_dimensions(transform);
      if h > 0 {
        ratio = Some(w as f32 / h as f32);
      }
    }

    if let Some(r) = ratio {
      if transform.quarter_turns % 2 == 1 {
        return Some(1.0 / r);
      }
    }

    ratio
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

/// Configuration for [`ImageCache`].
#[derive(Debug, Clone, Copy)]
pub struct ImageCacheConfig {
  /// Maximum number of decoded pixels (width * height). `0` disables the limit.
  pub max_decoded_pixels: u64,
  /// Maximum allowed width or height for a decoded image. `0` disables the limit.
  pub max_decoded_dimension: u32,
}

impl Default for ImageCacheConfig {
  fn default() -> Self {
    Self {
      max_decoded_pixels: 100_000_000,
      max_decoded_dimension: 32768,
    }
  }
}

impl ImageCacheConfig {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn with_max_decoded_pixels(mut self, max: u64) -> Self {
    self.max_decoded_pixels = max;
    self
  }

  pub fn with_max_decoded_dimension(mut self, max: u32) -> Self {
    self.max_decoded_dimension = max;
    self
  }
}

#[derive(Clone)]
enum SharedImageResult {
  Success(Arc<CachedImage>),
  Error(Error),
}

impl SharedImageResult {
  fn as_result(&self) -> Result<Arc<CachedImage>> {
    match self {
      Self::Success(img) => Ok(Arc::clone(img)),
      Self::Error(err) => Err(err.clone()),
    }
  }
}

struct DecodeInFlight {
  result: Mutex<Option<SharedImageResult>>,
  cv: Condvar,
}

impl DecodeInFlight {
  fn new() -> Self {
    Self {
      result: Mutex::new(None),
      cv: Condvar::new(),
    }
  }

  fn set(&self, result: SharedImageResult) {
    if let Ok(mut slot) = self.result.lock() {
      *slot = Some(result);
      self.cv.notify_all();
    }
  }

  fn wait(&self) -> Result<Arc<CachedImage>> {
    let mut guard = self.result.lock().unwrap();
    while guard.is_none() {
      guard = self.cv.wait(guard).unwrap();
    }
    guard.as_ref().unwrap().as_result()
  }
}

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
  /// In-flight decodes keyed by resolved URL to de-duplicate concurrent loads.
  in_flight: Arc<Mutex<HashMap<String, Arc<DecodeInFlight>>>>,
  /// Base URL for resolving relative image sources
  base_url: Option<String>,
  /// Resource fetcher for loading bytes from URLs
  fetcher: Arc<dyn ResourceFetcher>,
  /// Decode limits.
  config: ImageCacheConfig,
}

impl ImageCache {
  /// Create a new ImageCache with the default HTTP fetcher
  pub fn new() -> Self {
    Self::with_config(ImageCacheConfig::default())
  }

  /// Create a new ImageCache with a custom fetcher
  pub fn with_fetcher(fetcher: Arc<dyn ResourceFetcher>) -> Self {
    Self::with_fetcher_and_config(fetcher, ImageCacheConfig::default())
  }

  /// Create a new ImageCache with the default HTTP fetcher and custom limits.
  pub fn with_config(config: ImageCacheConfig) -> Self {
    Self::with_base_url_fetcher_and_config(
      None,
      Arc::new(CachingFetcher::with_config(
        HttpFetcher::new(),
        CachingFetcherConfig::default(),
      )),
      config,
    )
  }

  /// Create a new ImageCache with a custom fetcher and limits.
  pub fn with_fetcher_and_config(
    fetcher: Arc<dyn ResourceFetcher>,
    config: ImageCacheConfig,
  ) -> Self {
    Self::with_base_url_fetcher_and_config(None, fetcher, config)
  }

  /// Create a new ImageCache with a base URL and the default HTTP fetcher
  pub fn with_base_url(base_url: String) -> Self {
    Self::with_base_url_and_config(base_url, ImageCacheConfig::default())
  }

  /// Create a new ImageCache with a base URL, default fetcher, and custom limits.
  pub fn with_base_url_and_config(base_url: String, config: ImageCacheConfig) -> Self {
    Self::with_base_url_fetcher_and_config(
      Some(base_url),
      Arc::new(CachingFetcher::with_config(
        HttpFetcher::new(),
        CachingFetcherConfig::default(),
      )),
      config,
    )
  }

  /// Create a new ImageCache with both a base URL and a custom fetcher
  pub fn with_base_url_and_fetcher(base_url: String, fetcher: Arc<dyn ResourceFetcher>) -> Self {
    Self::with_base_url_fetcher_and_config(
      Some(base_url),
      fetcher,
      ImageCacheConfig::default(),
    )
  }

  /// Create a new ImageCache with a base URL, custom fetcher, and limits.
  pub fn with_base_url_and_fetcher_and_config(
    base_url: String,
    fetcher: Arc<dyn ResourceFetcher>,
    config: ImageCacheConfig,
  ) -> Self {
    Self::with_base_url_fetcher_and_config(Some(base_url), fetcher, config)
  }

  fn with_base_url_fetcher_and_config(
    base_url: Option<String>,
    fetcher: Arc<dyn ResourceFetcher>,
    config: ImageCacheConfig,
  ) -> Self {
    Self {
      cache: Arc::new(Mutex::new(HashMap::new())),
      in_flight: Arc::new(Mutex::new(HashMap::new())),
      base_url,
      fetcher,
      config,
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
  pub fn resolve_url(&self, url: &str) -> String {
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
    if let Some(img) = self.get_cached(&resolved_url) {
      return Ok(img);
    }

    let (flight, is_owner) = self.join_inflight(&resolved_url);
    if !is_owner {
      return flight.wait();
    }

    let result = self.fetch_and_decode(&resolved_url);
    let shared = match &result {
      Ok(img) => SharedImageResult::Success(Arc::clone(img)),
      Err(err) => SharedImageResult::Error(err.clone()),
    };
    self.finish_inflight(&resolved_url, &flight, shared);

    result
  }

  fn get_cached(&self, resolved_url: &str) -> Option<Arc<CachedImage>> {
    self
      .cache
      .lock()
      .ok()
      .and_then(|cache| cache.get(resolved_url).cloned())
  }

  fn fetch_and_decode(&self, resolved_url: &str) -> Result<Arc<CachedImage>> {
    let resource = self.fetcher.fetch(resolved_url)?;
    let (img, orientation, resolution, is_vector, intrinsic_ratio, aspect_ratio_none) =
      self.decode_resource(&resource, resolved_url)?;

    let img_arc = Arc::new(CachedImage {
      image: Arc::new(img),
      orientation,
      resolution,
      is_vector,
      intrinsic_ratio,
      aspect_ratio_none,
    });

    if let Ok(mut cache) = self.cache.lock() {
      cache.insert(resolved_url.to_string(), Arc::clone(&img_arc));
    }

    Ok(img_arc)
  }

  fn join_inflight(&self, resolved_url: &str) -> (Arc<DecodeInFlight>, bool) {
    let mut map = self.in_flight.lock().unwrap();
    if let Some(existing) = map.get(resolved_url) {
      return (Arc::clone(existing), false);
    }

    let flight = Arc::new(DecodeInFlight::new());
    map.insert(resolved_url.to_string(), Arc::clone(&flight));
    (flight, true)
  }

  fn finish_inflight(
    &self,
    resolved_url: &str,
    flight: &Arc<DecodeInFlight>,
    result: SharedImageResult,
  ) {
    flight.set(result);
    if let Ok(mut map) = self.in_flight.lock() {
      map.remove(resolved_url);
    }
  }

  /// Render raw SVG content to an image (uncached).
  pub fn render_svg(&self, svg_content: &str) -> Result<Arc<CachedImage>> {
    let (img, intrinsic_ratio, aspect_ratio_none) = self.render_svg_to_image(svg_content)?;
    Ok(Arc::new(CachedImage {
      image: Arc::new(img),
      orientation: None,
      resolution: None,
      is_vector: true,
      intrinsic_ratio,
      aspect_ratio_none,
    }))
  }

  /// Decode a fetched resource into an image
  fn decode_resource(
    &self,
    resource: &FetchedResource,
    url: &str,
  ) -> Result<(
    DynamicImage,
    Option<OrientationTransform>,
    Option<f32>,
    bool,
    Option<f32>,
    bool,
  )> {
    let bytes = &resource.bytes;
    let content_type = resource.content_type.as_deref();

    // Check if this is SVG
    let mime_is_svg = content_type
      .map(|m| m.contains("image/svg"))
      .unwrap_or(false);
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
      return self
        .render_svg_to_image_with_url(content, url)
        .map(|(img, ratio, aspect_none)| (img, None, None, true, ratio, aspect_none));
    }

    // Regular image - extract EXIF metadata and decode
    let (orientation, resolution) = Self::exif_metadata(bytes);
    self
      .decode_bitmap(bytes, content_type, url)
      .map(|img| (img, orientation, resolution, false, None, false))
  }

  fn decode_bitmap(
    &self,
    bytes: &[u8],
    content_type: Option<&str>,
    url: &str,
  ) -> Result<DynamicImage> {
    let format_from_content_type = Self::format_from_content_type(content_type);
    let sniffed_format = Self::sniff_image_format(bytes);
    if let Some((width, height)) =
      self.predecoded_dimensions(bytes, format_from_content_type, sniffed_format)
    {
      self.enforce_decode_limits(width, height, url)?;
    }
    let mut last_error: Option<image::ImageError> = None;

    if matches!(format_from_content_type, Some(ImageFormat::Avif))
      || matches!(sniffed_format, Some(ImageFormat::Avif))
    {
      match Self::decode_avif(bytes) {
        Ok(img) => return self.finish_bitmap_decode(img, url),
        Err(err) => last_error = Some(err),
      }
    }

    if let Some(format) = format_from_content_type {
      if format != ImageFormat::Avif {
        match image::load_from_memory_with_format(bytes, format) {
          Ok(img) => return self.finish_bitmap_decode(img, url),
          Err(err) => last_error = Some(err),
        }
      }
    }

    if let Some(format) = sniffed_format {
      if Some(format) != format_from_content_type && format != ImageFormat::Avif {
        match image::load_from_memory_with_format(bytes, format) {
          Ok(img) => return self.finish_bitmap_decode(img, url),
          Err(err) => last_error = Some(err),
        }
      }
    }

    match image::load_from_memory(bytes) {
      Ok(img) => self.finish_bitmap_decode(img, url),
      Err(err) => Err(self.decode_error(url, last_error.unwrap_or(err))),
    }
  }

  fn finish_bitmap_decode(&self, img: DynamicImage, url: &str) -> Result<DynamicImage> {
    self.enforce_decode_limits(img.width(), img.height(), url)?;
    Ok(img)
  }

  fn format_from_content_type(content_type: Option<&str>) -> Option<ImageFormat> {
    let mime = content_type?
      .split(';')
      .next()
      .map(|ct| ct.trim().to_ascii_lowercase())?;
    ImageFormat::from_mime_type(mime)
  }

  fn sniff_image_format(bytes: &[u8]) -> Option<ImageFormat> {
    image::guess_format(bytes).ok()
  }

  fn decode_error(&self, url: &str, err: image::ImageError) -> Error {
    Error::Image(ImageError::DecodeFailed {
      url: url.to_string(),
      reason: err.to_string(),
    })
  }

  fn predecoded_dimensions(
    &self,
    bytes: &[u8],
    format_from_content_type: Option<ImageFormat>,
    sniffed_format: Option<ImageFormat>,
  ) -> Option<(u32, u32)> {
    format_from_content_type
      .and_then(|format| Self::dimensions_for_format(bytes, format))
      .or_else(|| sniffed_format.and_then(|format| Self::dimensions_for_format(bytes, format)))
  }

  fn dimensions_for_format(bytes: &[u8], format: ImageFormat) -> Option<(u32, u32)> {
    match format {
      ImageFormat::Png => image::codecs::png::PngDecoder::new(Cursor::new(bytes))
        .ok()
        .map(|d| d.dimensions()),
      ImageFormat::Jpeg => image::codecs::jpeg::JpegDecoder::new(Cursor::new(bytes))
        .ok()
        .map(|d| d.dimensions()),
      ImageFormat::Gif => image::codecs::gif::GifDecoder::new(Cursor::new(bytes))
        .ok()
        .map(|d| d.dimensions()),
      ImageFormat::WebP => image::codecs::webp::WebPDecoder::new(Cursor::new(bytes))
        .ok()
        .map(|d| d.dimensions()),
      _ => None,
    }
  }

  fn enforce_decode_limits(&self, width: u32, height: u32, url: &str) -> Result<()> {
    if self.config.max_decoded_dimension > 0
      && (width > self.config.max_decoded_dimension || height > self.config.max_decoded_dimension)
    {
      return Err(Error::Image(ImageError::DecodeFailed {
        url: url.to_string(),
        reason: format!(
          "Image dimensions {}x{} exceed maximum dimension {}",
          width, height, self.config.max_decoded_dimension
        ),
      }));
    }

    if self.config.max_decoded_pixels > 0 {
      let pixels = u64::from(width) * u64::from(height);
      if pixels > self.config.max_decoded_pixels {
        return Err(Error::Image(ImageError::DecodeFailed {
          url: url.to_string(),
          reason: format!(
            "Image dimensions {}x{} exceed pixel budget of {}",
            width, height, self.config.max_decoded_pixels
          ),
        }));
      }
    }

    Ok(())
  }

  fn decode_avif(bytes: &[u8]) -> std::result::Result<DynamicImage, image::ImageError> {
    let decoder = AvifDecoder::from_avif(bytes).map_err(|err| Self::avif_error(err))?;
    let image = decoder
      .to_image()
      .map_err(|err| Self::avif_error(err))?;
    Self::avif_image_to_dynamic(image)
  }

  fn avif_image_to_dynamic(image: AvifImage) -> std::result::Result<DynamicImage, image::ImageError> {
    let dimension_error = || {
      image::ImageError::Parameter(image::error::ParameterError::from_kind(
        image::error::ParameterErrorKind::DimensionMismatch,
      ))
    };

    match image {
      AvifImage::Rgb8(img) => {
        let (width, height) = Self::avif_dimensions(img.width(), img.height())?;
        let mut buf = Vec::with_capacity(width as usize * height as usize * 3);
        for px in img.buf() {
          buf.extend_from_slice(&[px.r, px.g, px.b]);
        }
        image::RgbImage::from_vec(width, height, buf)
          .map(DynamicImage::ImageRgb8)
          .ok_or_else(dimension_error)
      }
      AvifImage::Rgb16(img) => {
        let (width, height) = Self::avif_dimensions(img.width(), img.height())?;
        let mut buf = Vec::with_capacity(width as usize * height as usize * 3);
        for px in img.buf() {
          buf.extend_from_slice(&[px.r, px.g, px.b]);
        }
        image::ImageBuffer::from_vec(width, height, buf)
          .map(DynamicImage::ImageRgb16)
          .ok_or_else(dimension_error)
      }
      AvifImage::Rgba8(img) => {
        let (width, height) = Self::avif_dimensions(img.width(), img.height())?;
        let mut buf = Vec::with_capacity(width as usize * height as usize * 4);
        for px in img.buf() {
          buf.extend_from_slice(&[px.r, px.g, px.b, px.a]);
        }
        image::RgbaImage::from_vec(width, height, buf)
          .map(DynamicImage::ImageRgba8)
          .ok_or_else(dimension_error)
      }
      AvifImage::Rgba16(img) => {
        let (width, height) = Self::avif_dimensions(img.width(), img.height())?;
        let mut buf = Vec::with_capacity(width as usize * height as usize * 4);
        for px in img.buf() {
          buf.extend_from_slice(&[px.r, px.g, px.b, px.a]);
        }
        image::ImageBuffer::from_vec(width, height, buf)
          .map(DynamicImage::ImageRgba16)
          .ok_or_else(dimension_error)
      }
      AvifImage::Gray8(img) => {
        let (width, height) = Self::avif_dimensions(img.width(), img.height())?;
        let mut buf = Vec::with_capacity(width as usize * height as usize);
        for px in img.buf() {
          buf.push(px.value());
        }
        image::ImageBuffer::from_vec(width, height, buf)
          .map(DynamicImage::ImageLuma8)
          .ok_or_else(dimension_error)
      }
      AvifImage::Gray16(img) => {
        let (width, height) = Self::avif_dimensions(img.width(), img.height())?;
        let mut buf = Vec::with_capacity(width as usize * height as usize);
        for px in img.buf() {
          buf.push(px.value());
        }
        image::ImageBuffer::from_vec(width, height, buf)
          .map(DynamicImage::ImageLuma16)
          .ok_or_else(dimension_error)
      }
    }
  }

  fn avif_dimensions(
    width: usize,
    height: usize,
  ) -> std::result::Result<(u32, u32), image::ImageError> {
    let to_u32 = |v: usize| {
      u32::try_from(v).map_err(|_| {
        image::ImageError::Parameter(image::error::ParameterError::from_kind(
          image::error::ParameterErrorKind::DimensionMismatch,
        ))
      })
    };
    Ok((to_u32(width)?, to_u32(height)?))
  }

  fn avif_error(err: impl std::fmt::Display) -> image::ImageError {
    image::ImageError::Decoding(image::error::DecodingError::new(
      ImageFormat::Avif.into(),
      err.to_string(),
    ))
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
          vals.first().copied()
        } else {
          None
        }
      })
      .and_then(rational_to_f32);
    let y_res = exif
      .get_field(exif::Tag::YResolution, exif::In::PRIMARY)
      .and_then(|f| {
        if let exif::Value::Rational(ref vals) = f.value {
          vals.first().copied()
        } else {
          None
        }
      })
      .and_then(rational_to_f32);
    let avg_res = match (x_res, y_res) {
      (Some(x), Some(y)) if x.is_finite() && y.is_finite() && x > 0.0 && y > 0.0 => {
        Some((x + y) / 2.0)
      }
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

  /// Renders raw SVG content to a raster image, returning any parsed intrinsic aspect ratio
  /// information.
  pub fn render_svg_to_image(
    &self,
    svg_content: &str,
  ) -> Result<(DynamicImage, Option<f32>, bool)> {
    self.render_svg_to_image_with_url(svg_content, "SVG content")
  }

  fn render_svg_to_image_with_url(
    &self,
    svg_content: &str,
    url: &str,
  ) -> Result<(DynamicImage, Option<f32>, bool)> {
    use resvg::usvg;

    const DEFAULT_WIDTH: f32 = 300.0;
    const DEFAULT_HEIGHT: f32 = 150.0;

    let (meta_width, meta_height, meta_ratio, aspect_ratio_none) =
      svg_intrinsic_metadata(svg_content).unwrap_or((None, None, None, false));

    // Parse SVG
    let options = usvg::Options::default();
    let tree = usvg::Tree::from_str(svg_content, &options).map_err(|e| {
      Error::Image(ImageError::DecodeFailed {
        url: url.to_string(),
        reason: format!("Failed to parse SVG: {}", e),
      })
    })?;

    let size = tree.size();
    let source_width = size.width();
    let source_height = size.height();
    if source_width <= 0.0 || source_height <= 0.0 {
      return Err(Error::Render(RenderError::CanvasCreationFailed {
        width: source_width as u32,
        height: source_height as u32,
      }));
    }

    let ratio = meta_ratio.filter(|r| *r > 0.0);
    let (target_width, target_height) = match (
      meta_width.filter(|w| *w > 0.0),
      meta_height.filter(|h| *h > 0.0),
      ratio,
    ) {
      (Some(w), Some(h), _) => (w, h),
      (Some(w), None, Some(r)) => (w, (w / r).max(1.0)),
      (None, Some(h), Some(r)) => ((h * r).max(1.0), h),
      (Some(w), None, None) => (w, DEFAULT_HEIGHT),
      (None, Some(h), None) => (DEFAULT_WIDTH, h),
      (None, None, Some(r)) => (DEFAULT_WIDTH, (DEFAULT_WIDTH / r).max(1.0)),
      (None, None, None) => (DEFAULT_WIDTH, DEFAULT_HEIGHT),
    };

    let render_width = target_width.max(1.0).round() as u32;
    let render_height = target_height.max(1.0).round() as u32;

    self.enforce_decode_limits(render_width, render_height, url)?;

    // Render SVG to pixmap, scaling to the target intrinsic dimensions when needed
    let mut pixmap = tiny_skia::Pixmap::new(render_width, render_height).ok_or(Error::Render(
      RenderError::CanvasCreationFailed {
        width: render_width,
        height: render_height,
      },
    ))?;

    let scale_x = target_width / source_width;
    let scale_y = target_height / source_height;
    let transform = tiny_skia::Transform::from_scale(scale_x, scale_y);
    resvg::render(&tree, transform, &mut pixmap.as_mut());

    // Convert pixmap to image
    let rgba_data = pixmap.take();
    let img =
      image::RgbaImage::from_raw(render_width, render_height, rgba_data).ok_or_else(|| {
        Error::Image(ImageError::DecodeFailed {
          url: url.to_string(),
          reason: "Failed to create image from SVG pixmap".to_string(),
        })
      })?;

    let ratio = if aspect_ratio_none {
      None
    } else {
      ratio.or_else(|| {
        if render_height > 0 {
          Some(render_width as f32 / render_height as f32)
        } else {
          None
        }
      })
    };

    Ok((
      image::DynamicImage::ImageRgba8(img),
      ratio,
      aspect_ratio_none,
    ))
  }
}

fn parse_svg_length(value: &str) -> Option<f32> {
  let trimmed = value.trim();
  if trimmed.is_empty() || trimmed.ends_with('%') {
    return None;
  }

  let mut end = 0;
  for (idx, ch) in trimmed.char_indices() {
    if matches!(ch, '0'..='9' | '+' | '-' | '.' | 'e' | 'E') {
      end = idx + ch.len_utf8();
    } else {
      break;
    }
  }

  if end == 0 {
    return None;
  }

  trimmed[..end].parse::<f32>().ok()
}

/// Returns intrinsic metadata extracted from the SVG root element: explicit width/height when
/// present (absolute units only), an intrinsic aspect ratio (if not disabled), and whether
/// preserveAspectRatio="none" was specified.
fn svg_intrinsic_metadata(
  svg_content: &str,
) -> Option<(Option<f32>, Option<f32>, Option<f32>, bool)> {
  let doc = Document::parse(svg_content).ok()?;
  let root = doc.root_element();
  if !root.tag_name().name().eq_ignore_ascii_case("svg") {
    return None;
  }

  let aspect_ratio_none = root
    .attribute("preserveAspectRatio")
    .and_then(|par| par.split_whitespace().next())
    .map(|v| v.eq_ignore_ascii_case("none"))
    .unwrap_or(false);

  let width = root.attribute("width").and_then(parse_svg_length);
  let height = root.attribute("height").and_then(parse_svg_length);

  let mut ratio = None;
  if !aspect_ratio_none {
    if let (Some(w), Some(h)) = (width, height) {
      if h > 0.0 {
        ratio = Some(w / h);
      }
    }

    if ratio.is_none() {
      if let Some(view_box) = root.attribute("viewBox") {
        let mut parts = view_box
          .split(|c: char| c == ',' || c.is_whitespace())
          .filter(|s| !s.is_empty())
          .filter_map(|s| s.parse::<f32>().ok());
        let _ = parts.next();
        let _ = parts.next();
        if let (Some(w), Some(h)) = (parts.next(), parts.next()) {
          if h > 0.0 {
            ratio = Some(w / h);
          }
        }
      }
    }
  }

  Some((width, height, ratio, aspect_ratio_none))
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
    .or_else(|_| {
      Url::from_file_path(&base_candidate).map_err(|()| url::ParseError::RelativeUrlWithoutBase)
    })
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
      in_flight: Arc::clone(&self.in_flight),
      base_url: self.base_url.clone(),
      fetcher: Arc::clone(&self.fetcher),
      config: self.config,
    }
  }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
  use super::*;
  use crate::style::types::OrientationTransform;
  use image::RgbaImage;
  use std::path::PathBuf;
  use std::time::SystemTime;

  #[test]
  fn svg_width_height_set_intrinsic_size_and_ratio() {
    let cache = ImageCache::new();
    let svg = "<svg xmlns='http://www.w3.org/2000/svg' width='200' height='100'></svg>";
    let img = cache.render_svg(svg).expect("rendered");

    assert_eq!(img.width(), 200);
    assert_eq!(img.height(), 100);
    assert_eq!(
      img.intrinsic_ratio(OrientationTransform::IDENTITY),
      Some(2.0)
    );
    assert!(!img.aspect_ratio_none);
  }

  #[test]
  fn svg_viewbox_defaults_to_300x150_with_ratio() {
    let cache = ImageCache::new();
    let svg = "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 40 20'></svg>";
    let img = cache.render_svg(svg).expect("rendered");

    assert_eq!(img.width(), 300);
    assert_eq!(img.height(), 150);
    assert_eq!(
      img.intrinsic_ratio(OrientationTransform::IDENTITY),
      Some(2.0)
    );
    assert!(!img.aspect_ratio_none);
  }

  #[test]
  fn svg_preserve_aspect_ratio_none_disables_ratio() {
    let cache = ImageCache::new();
    let svg = "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 50' preserveAspectRatio='none'></svg>";
    let img = cache.render_svg(svg).expect("rendered");

    assert_eq!(img.width(), 300);
    assert_eq!(img.height(), 150);
    assert!(img.aspect_ratio_none);
    assert_eq!(img.intrinsic_ratio(OrientationTransform::IDENTITY), None);
  }

  #[test]
  fn svg_percent_width_height_ignored_defaults_with_viewbox_ratio() {
    let cache = ImageCache::new();
    let svg = "<svg xmlns='http://www.w3.org/2000/svg' width='50%' height='25%' viewBox='0 0 200 100'></svg>";
    let img = cache.render_svg(svg).expect("rendered");

    // Percent lengths are ignored; fall back to 300x150 but keep the viewBox ratio (2:1).
    assert_eq!(img.width(), 300);
    assert_eq!(img.height(), 150);
    assert_eq!(
      img.intrinsic_ratio(OrientationTransform::IDENTITY),
      Some(2.0)
    );
  }

  #[test]
  fn render_inline_svg_returns_image() {
    let cache = ImageCache::new();
    let svg = r#"<svg xmlns="http://www.w3.org/2000/svg" width="10" height="5"></svg>"#;
    let (image, ratio, aspect_none) = cache.render_svg_to_image(svg).expect("svg render");
    assert_eq!(image.width(), 10);
    assert_eq!(image.height(), 5);
    assert_eq!(ratio, Some(2.0));
    assert!(!aspect_none);
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
  fn svg_mask_application() {
    let cache = ImageCache::new();
    let svg = r#"
      <svg xmlns='http://www.w3.org/2000/svg' width='20' height='20'>
        <defs>
          <mask id='m'>
            <rect width='100%' height='100%' fill='white'/>
            <rect width='10' height='20' fill='black'/>
          </mask>
        </defs>
        <rect width='20' height='20' fill='red' mask='url(#m)' />
      </svg>
    "#;

    let image = cache.render_svg(svg).expect("render svg mask");
    let rgba = image.image.to_rgba8();
    let left_alpha = rgba.get_pixel(5, 10)[3];
    let right_alpha = rgba.get_pixel(15, 10)[3];
    assert!(left_alpha < right_alpha, "mask should reduce left side opacity");
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
    use std::sync::atomic::AtomicUsize;
    use std::sync::atomic::Ordering;

    struct CountingFetcher {
      count: AtomicUsize,
    }

    impl ResourceFetcher for CountingFetcher {
      fn fetch(&self, _url: &str) -> Result<FetchedResource> {
        self.count.fetch_add(1, Ordering::SeqCst);
        // Return a minimal valid PNG
        let png_data = vec![
          0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a, // PNG signature
          0x00, 0x00, 0x00, 0x0d, 0x49, 0x48, 0x44, 0x52, // IHDR chunk
          0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01, // 1x1
          0x08, 0x02, 0x00, 0x00, 0x00, 0x90, 0x77, 0x53, 0xde, 0x00, 0x00, 0x00, 0x0c, 0x49, 0x44,
          0x41, 0x54, 0x08, 0xd7, 0x63, 0xf8, 0xff, 0xff, 0x3f, 0x00, 0x05, 0xfe, 0x02, 0xfe, 0xdc,
          0xcc, 0x59, 0xe7, 0x00, 0x00, 0x00, 0x00, 0x49, 0x45, 0x4e, 0x44, 0xae, 0x42, 0x60, 0x82,
        ];
        Ok(FetchedResource::new(
          png_data,
          Some("image/png".to_string()),
        ))
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

  struct StaticFetcher {
    bytes: Vec<u8>,
    content_type: Option<String>,
  }

  impl ResourceFetcher for StaticFetcher {
    fn fetch(&self, _url: &str) -> Result<FetchedResource> {
      Ok(FetchedResource::new(
        self.bytes.clone(),
        self.content_type.clone(),
      ))
    }
  }

  fn avif_fixture_bytes() -> Vec<u8> {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("tests/fixtures/avif/solid.avif");
    std::fs::read(&path).expect("read avif fixture")
  }

  fn assert_green_pixel(pixel: [u8; 4]) {
    assert!(pixel[1] >= 180, "expected green channel, got {pixel:?}");
    assert!(pixel[0] < 50 && pixel[2] < 50, "expected low red/blue, got {pixel:?}");
  }

  #[test]
  fn decodes_avif_with_declared_content_type() {
    let bytes = avif_fixture_bytes();
    let fetcher = Arc::new(StaticFetcher {
      bytes: bytes.clone(),
      content_type: Some("image/avif".to_string()),
    });
    let cache = ImageCache::with_fetcher(fetcher);

    let image = cache.load("test://avif.declared").expect("decode avif");
    assert_eq!(image.width(), 4);
    assert_eq!(image.height(), 4);

    let pixel = image.image.to_rgba8().get_pixel(0, 0).0;
    assert_green_pixel(pixel);
  }

  #[test]
  fn decodes_avif_when_content_type_is_incorrect() {
    let bytes = avif_fixture_bytes();
    let fetcher = Arc::new(StaticFetcher {
      bytes: bytes.clone(),
      content_type: Some("image/png".to_string()),
    });
    let cache = ImageCache::with_fetcher(fetcher);

    let image = cache.load("test://avif.sniff").expect("decode avif via sniffing");
    assert_eq!(image.width(), 4);
    assert_eq!(image.height(), 4);

    let pixel = image.image.to_rgba8().get_pixel(2, 2).0;
    assert_green_pixel(pixel);
  }
}
