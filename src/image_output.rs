use crate::error::Error;
use crate::error::RenderError;
use crate::error::Result;
use crate::image_compare::{self, CompareConfig};
use image::ImageFormat;
use image::Rgba;
use image::RgbaImage;
use std::io::Cursor;
use tiny_skia::Pixmap;
use webp::Encoder as WebpEncoder;

/// Summary of a pixel diff operation.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct DiffMetrics {
  pub pixel_diff: u64,
  pub total_pixels: u64,
  pub diff_percentage: f64,
}

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

  // tiny-skia stores premultiplied RGBA pixels in RGBA byte order.
  // Convert to straight RGBA for image encoding.
  let mut rgba_data = Vec::with_capacity(pixels.len());
  for chunk in pixels.chunks_exact(4) {
    let r = chunk[0];
    let g = chunk[1];
    let b = chunk[2];
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
    OutputFormat::WebP(quality) => {
      let quality = (quality as f32).clamp(0.0, 100.0);
      let encoder = WebpEncoder::from_rgba(&rgba_data, width, height);
      let webp = encoder.encode_simple(false, quality).map_err(|e| {
        Error::Render(RenderError::EncodeFailed {
          format: "WebP".to_string(),
          reason: format!("WebP encode failed: {e:?}"),
        })
      })?;
      buffer.extend_from_slice(&webp);
    }
  }

  Ok(buffer)
}

/// Computes a diff image between two PNG byte buffers.
///
/// Returns the diff metrics along with a PNG highlighting differing pixels.
pub fn diff_png(rendered: &[u8], expected: &[u8], tolerance: u8) -> Result<(DiffMetrics, Vec<u8>)> {
  let mut config = CompareConfig::strict().with_channel_tolerance(tolerance);
  config.max_different_percent = 100.0;

  let diff = image_compare::compare_png(rendered, expected, &config)?;
  if !diff.dimensions_match {
    return Err(Error::Render(RenderError::InvalidParameters {
      message: format!(
        "Image dimensions differ: rendered {}x{}, expected {}x{}",
        diff.actual_dimensions.0,
        diff.actual_dimensions.1,
        diff.expected_dimensions.0,
        diff.expected_dimensions.1
      ),
    }));
  }

  let diff_png = diff.diff_png()?.unwrap_or_default();

  let metrics = DiffMetrics {
    pixel_diff: diff.statistics.different_pixels,
    total_pixels: diff.statistics.total_pixels,
    diff_percentage: diff.statistics.different_percent,
  };

  Ok((metrics, diff_png))
}
