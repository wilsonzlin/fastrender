use crate::error::{Error, RenderError, Result};
use image::{ImageFormat, RgbaImage};
use std::io::Cursor;
use tiny_skia::Pixmap;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum OutputFormat {
    Png,
    Jpeg(u8), // quality 0-100
    WebP(u8), // quality 0-100
}

impl Default for OutputFormat {
    fn default() -> Self {
        OutputFormat::Png
    }
}

pub fn encode_image(pixmap: &Pixmap, format: OutputFormat) -> Result<Vec<u8>> {
    let width = pixmap.width();
    let height = pixmap.height();
    let pixels = pixmap.data();

    // Convert from premultiplied RGBA to straight RGBA
    let mut rgba_data = Vec::with_capacity(pixels.len());
    for chunk in pixels.chunks_exact(4) {
        let b = chunk[0];
        let g = chunk[1];
        let r = chunk[2];
        let a = chunk[3];

        // Unpremultiply alpha
        let (r, g, b) = if a > 0 {
            let alpha = a as f32 / 255.0;
            (
                ((r as f32 / alpha).min(255.0)) as u8,
                ((g as f32 / alpha).min(255.0)) as u8,
                ((b as f32 / alpha).min(255.0)) as u8,
            )
        } else {
            (0, 0, 0)
        };

        rgba_data.push(r);
        rgba_data.push(g);
        rgba_data.push(b);
        rgba_data.push(a);
    }

    let mut buffer = Vec::new();

    match format {
        OutputFormat::Png => {
            let img = RgbaImage::from_raw(width, height, rgba_data).ok_or_else(|| {
                Error::Render(RenderError::EncodeFailed {
                    format: "PNG".to_string(),
                    reason: "Failed to create RGBA image".to_string(),
                })
            })?;

            let mut cursor = Cursor::new(&mut buffer);
            img.write_to(&mut cursor, ImageFormat::Png).map_err(|e| {
                Error::Render(RenderError::EncodeFailed {
                    format: "PNG".to_string(),
                    reason: e.to_string(),
                })
            })?;
        }
        OutputFormat::Jpeg(quality) => {
            // Convert RGBA to RGB for JPEG
            let rgb_data: Vec<u8> = rgba_data
                .chunks_exact(4)
                .flat_map(|chunk| vec![chunk[0], chunk[1], chunk[2]])
                .collect();

            let rgb_img = image::RgbImage::from_raw(width, height, rgb_data).ok_or_else(|| {
                Error::Render(RenderError::EncodeFailed {
                    format: "JPEG".to_string(),
                    reason: "Failed to create RGB image".to_string(),
                })
            })?;

            let mut cursor = Cursor::new(&mut buffer);
            let encoder = image::codecs::jpeg::JpegEncoder::new_with_quality(&mut cursor, quality);
            rgb_img.write_with_encoder(encoder).map_err(|e| {
                Error::Render(RenderError::EncodeFailed {
                    format: "JPEG".to_string(),
                    reason: e.to_string(),
                })
            })?;
        }
        OutputFormat::WebP(_quality) => {
            let img = RgbaImage::from_raw(width, height, rgba_data).ok_or_else(|| {
                Error::Render(RenderError::EncodeFailed {
                    format: "WebP".to_string(),
                    reason: "Failed to create RGBA image".to_string(),
                })
            })?;

            // WebP encoding
            let mut cursor = Cursor::new(&mut buffer);
            let encoder = image::codecs::webp::WebPEncoder::new_lossless(&mut cursor);
            img.write_with_encoder(encoder).map_err(|e| {
                Error::Render(RenderError::EncodeFailed {
                    format: "WebP".to_string(),
                    reason: e.to_string(),
                })
            })?;
        }
    }

    Ok(buffer)
}
