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
  /// SSIM-derived perceptual distance (0.0 = identical, higher = more different).
  pub perceptual_distance: f64,
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
  if diff.dimensions_match {
    let diff_png = diff.diff_png()?.unwrap_or_default();

    let metrics = DiffMetrics {
      pixel_diff: diff.statistics.different_pixels,
      total_pixels: diff.statistics.total_pixels,
      diff_percentage: diff.statistics.different_percent,
      perceptual_distance: diff.statistics.perceptual_distance,
    };

    return Ok((metrics, diff_png));
  }

  // When dimensions differ, fall back to a padded diff so reports remain usable (mirrors the old
  // `cargo xtask diff-renders` behaviour). Missing pixels are treated as differences.
  let rendered_img = image_compare::decode_png(rendered)?;
  let expected_img = image_compare::decode_png(expected)?;

  let max_width = rendered_img.width().max(expected_img.width());
  let max_height = rendered_img.height().max(expected_img.height());
  let total_pixels = (max_width as u64) * (max_height as u64);

  let mut diff_image = RgbaImage::new(max_width, max_height);
  let mut different_pixels = 0u64;

  for y in 0..max_height {
    for x in 0..max_width {
      let rendered_px = if x < rendered_img.width() && y < rendered_img.height() {
        Some(*rendered_img.get_pixel(x, y))
      } else {
        None
      };
      let expected_px = if x < expected_img.width() && y < expected_img.height() {
        Some(*expected_img.get_pixel(x, y))
      } else {
        None
      };

      match (rendered_px, expected_px) {
        (Some(rendered_px), Some(expected_px)) => {
          let diff_r = rendered_px[0].abs_diff(expected_px[0]);
          let diff_g = rendered_px[1].abs_diff(expected_px[1]);
          let diff_b = rendered_px[2].abs_diff(expected_px[2]);
          let diff_a = rendered_px[3].abs_diff(expected_px[3]);
          let max_diff = diff_r.max(diff_g).max(diff_b).max(diff_a);

          let is_different =
            diff_r > tolerance || diff_g > tolerance || diff_b > tolerance || diff_a > tolerance;
          if is_different {
            different_pixels += 1;
            let alpha = max_diff.saturating_mul(2).min(255);
            diff_image.put_pixel(x, y, Rgba([255, 0, 0, alpha]));
          } else {
            diff_image.put_pixel(x, y, Rgba([0, 0, 0, 0]));
          }
        }
        (Some(_), None) | (None, Some(_)) => {
          different_pixels += 1;
          diff_image.put_pixel(x, y, Rgba([255, 0, 255, 255]));
        }
        (None, None) => unreachable!("loop bounds ensure at least one pixel is present"),
      }
    }
  }

  let diff_percentage = if total_pixels > 0 {
    (different_pixels as f64 / total_pixels as f64) * 100.0
  } else {
    0.0
  };

  let diff_png = image_compare::encode_png(&diff_image)?;
  let metrics = DiffMetrics {
    pixel_diff: different_pixels,
    total_pixels,
    diff_percentage,
    // Perceptual distance is ill-defined when dimensions don't match; treat this as maximally
    // different for now.
    perceptual_distance: 1.0,
  };

  Ok((metrics, diff_png))
}
