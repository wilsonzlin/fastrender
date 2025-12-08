use crate::error::{Error, ImageError, RenderError, Result};
use image::DynamicImage;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

/// Cache for loaded images
pub struct ImageCache {
    cache: Arc<Mutex<HashMap<String, Arc<DynamicImage>>>>,
    base_url: Option<String>,
}

impl ImageCache {
    pub fn new() -> Self {
        Self {
            cache: Arc::new(Mutex::new(HashMap::new())),
            base_url: None,
        }
    }

    pub fn with_base_url(base_url: String) -> Self {
        Self {
            cache: Arc::new(Mutex::new(HashMap::new())),
            base_url: Some(base_url),
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

    /// Resolve a potentially relative URL to an absolute URL
    fn resolve_url(&self, url: &str) -> String {
        // DEBUG: Log URL resolution for y18.svg
        if url.contains("y18") {
            eprintln!(
                "DEBUG: Resolving Y logo URL '{}' with base_url {:?}",
                url, self.base_url
            );
        }

        // If it's already absolute, return as-is
        if url.starts_with("http://")
            || url.starts_with("https://")
            || url.starts_with("data:")
            || url.starts_with("file://")
        {
            return url.to_string();
        }

        // If we have a base URL, resolve relative to it
        if let Some(base) = &self.base_url {
            if url.starts_with("//") {
                // Protocol-relative URL
                let protocol = if base.starts_with("https://") {
                    "https:"
                } else {
                    "http:"
                };
                return format!("{}{}", protocol, url);
            } else if url.starts_with('/') {
                // Absolute path - extract protocol and domain from base
                // Find the third slash (after protocol://)
                let after_protocol = if base.starts_with("https://") {
                    &base[8..]
                } else if base.starts_with("http://") {
                    &base[7..]
                } else {
                    base
                };

                if let Some(slash_pos) = after_protocol.find('/') {
                    let protocol_len = base.len() - after_protocol.len();
                    let domain = &base[..protocol_len + slash_pos];
                    return format!("{}{}", domain, url);
                } else {
                    return format!("{}{}", base, url);
                }
            } else if url.starts_with("./") {
                // Relative path with ./
                let path = &url[2..];
                if let Some(last_slash) = base.rfind('/') {
                    return format!("{}/{}", &base[..last_slash], path);
                } else {
                    return format!("{}/{}", base, path);
                }
            } else {
                // Relative path without ./
                // For base URLs like <https://news.ycombinator.com,> append directly
                let resolved_url = format!("{}/{}", base.trim_end_matches('/'), url);
                return resolved_url;
            }
        }

        // No base URL, return as-is
        url.to_string()
    }

    /// Load an image from a URL or file path
    pub fn load(&self, url: &str) -> Result<Arc<DynamicImage>> {
        // Resolve the URL first
        let resolved_url = self.resolve_url(url);

        // Check cache first (using original URL as key)
        {
            let cache = self.cache.lock().unwrap();
            if let Some(img) = cache.get(url) {
                return Ok(Arc::clone(img));
            }
        }

        // Load image using resolved URL
        let img = if resolved_url.starts_with("http://") || resolved_url.starts_with("https://") {
            // Fetch from network
            self.load_from_url(&resolved_url)?
        } else if resolved_url.starts_with("file://") {
            // Load from file system
            let path = resolved_url.strip_prefix("file://").unwrap();
            self.load_from_file(path)?
        } else if resolved_url.starts_with("data:") {
            // Data URL
            self.load_from_data_url(&resolved_url)?
        } else {
            // Assume it's a file path
            self.load_from_file(&resolved_url)?
        };

        // Cache it (using original URL as key)
        let img_arc = Arc::new(img);
        {
            let mut cache = self.cache.lock().unwrap();
            cache.insert(url.to_string(), Arc::clone(&img_arc));
        }

        Ok(img_arc)
    }

    fn load_from_url(&self, url: &str) -> Result<DynamicImage> {
        // Configure agent with timeout
        let config = ureq::Agent::config_builder()
            .timeout_global(Some(std::time::Duration::from_secs(30)))
            .build();
        let agent: ureq::Agent = config.into();

        let mut response = agent
            .get(url)
            .call()
            .map_err(|e| Error::Io(std::io::Error::new(std::io::ErrorKind::Other, e.to_string())))?;

        // Read response body into bytes (limit to 50MB for images)
        let bytes = response
            .body_mut()
            .with_config()
            .limit(50 * 1024 * 1024)
            .read_to_vec()
            .map_err(|e| Error::Io(std::io::Error::new(std::io::ErrorKind::Other, e.to_string())))?;

        // Decode image
        image::load_from_memory(&bytes).map_err(|e| {
            Error::Image(ImageError::DecodeFailed {
                url: url.to_string(),
                reason: e.to_string(),
            })
        })
    }

    fn load_from_file(&self, path: &str) -> Result<DynamicImage> {
        image::open(path).map_err(|e| {
            Error::Image(ImageError::LoadFailed {
                url: path.to_string(),
                reason: e.to_string(),
            })
        })
    }

    fn load_from_data_url(&self, data_url: &str) -> Result<DynamicImage> {
        // Parse data URL: data:image/png;base64,iVBORw0KG... or data:image/svg+xml,%3Csvg...
        if !data_url.starts_with("data:") {
            return Err(Error::Image(ImageError::InvalidDataUrl {
                reason: "URL does not start with 'data:'".to_string(),
            }));
        }

        let parts: Vec<&str> = data_url.splitn(2, ',').collect();
        if parts.len() != 2 {
            return Err(Error::Image(ImageError::InvalidDataUrl {
                reason: "Missing comma separator".to_string(),
            }));
        }

        let header = parts[0];
        let data = parts[1];

        // Check if base64 encoded
        if header.contains("base64") {
            use base64::Engine;
            let bytes = base64::engine::general_purpose::STANDARD.decode(data).map_err(|e| {
                Error::Image(ImageError::InvalidDataUrl {
                    reason: format!("Failed to decode base64: {}", e),
                })
            })?;

            image::load_from_memory(&bytes).map_err(|e| {
                Error::Image(ImageError::DecodeFailed {
                    url: "data URL".to_string(),
                    reason: e.to_string(),
                })
            })
        } else if header.contains("image/svg+xml") {
            // Handle URL-encoded SVG
            let decoded = urlencoding::decode(data).map_err(|e| {
                Error::Image(ImageError::InvalidDataUrl {
                    reason: format!("Failed to URL decode SVG: {}", e),
                })
            })?;

            // Use resvg to render SVG to bitmap
            self.render_svg_to_image(&decoded)
        } else {
            Err(Error::Image(ImageError::InvalidDataUrl {
                reason: "Unsupported data URL format (not base64 or SVG)".to_string(),
            }))
        }
    }

    fn render_svg_to_image(&self, svg_content: &str) -> Result<DynamicImage> {
        use resvg::usvg;

        // Parse SVG
        let options = usvg::Options::default();
        let tree = usvg::Tree::from_str(svg_content, &options).map_err(|e| {
            Error::Image(ImageError::DecodeFailed {
                url: "SVG data URL".to_string(),
                reason: format!("Failed to parse SVG: {}", e),
            })
        })?;

        let size = tree.size();
        let width = size.width() as u32;
        let height = size.height() as u32;

        // Render SVG to pixmap
        let mut pixmap = tiny_skia::Pixmap::new(width, height)
            .ok_or(Error::Render(RenderError::CanvasCreationFailed { width, height }))?;

        resvg::render(&tree, tiny_skia::Transform::identity(), &mut pixmap.as_mut());

        // Convert pixmap to image
        let rgba_data = pixmap.take();
        let img = image::RgbaImage::from_raw(width, height, rgba_data).ok_or_else(|| {
            Error::Image(ImageError::DecodeFailed {
                url: "SVG data URL".to_string(),
                reason: "Failed to create image from SVG pixmap".to_string(),
            })
        })?;

        Ok(image::DynamicImage::ImageRgba8(img))
    }
}

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
        }
    }
}
