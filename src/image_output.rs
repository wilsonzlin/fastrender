use crate::error::Error;
use crate::error::RenderError;
use crate::error::Result;
use image::ImageFormat;
use image::Rgba;
use image::RgbaImage;
use std::io::Cursor;
use tiny_skia::Pixmap;

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
      let img = RgbaImage::from_raw(width, height, rgba_data).ok_or_else(|| {
        Error::Render(RenderError::EncodeFailed {
          format: "WebP".to_string(),
          reason: "Failed to create RGBA image".to_string(),
        })
      })?;

      // WebP encoding
      let mut cursor = Cursor::new(&mut buffer);
      // Clamp to the supported range and interpret 100 as a request for lossless
      // encoding to preserve alpha without further loss. Values above 100 are
      // normalized to 100, matching the documented quality contract.
      let normalized_quality = quality.clamp(0, 100);

      if normalized_quality == 100 {
        let encoder = image::codecs::webp::WebPEncoder::new_lossless(&mut cursor);
        img.write_with_encoder(encoder).map_err(|e| {
          Error::Render(RenderError::EncodeFailed {
            format: "WebP".to_string(),
            reason: e.to_string(),
          })
        })?;
      } else {
        let encoder = image::codecs::webp::WebPEncoder::new_with_quality(
          &mut cursor,
          f32::from(normalized_quality),
        );
        img.write_with_encoder(encoder).map_err(|e| {
          Error::Render(RenderError::EncodeFailed {
            format: "WebP".to_string(),
            reason: e.to_string(),
          })
        })?;
      }
    }
  }

  Ok(buffer)
}

/// Computes a diff image between two PNG byte buffers.
///
/// Returns the diff metrics along with a PNG highlighting differing pixels.
pub fn diff_png(rendered: &[u8], expected: &[u8], tolerance: u8) -> Result<(DiffMetrics, Vec<u8>)> {
  let rendered_img = image::load_from_memory(rendered).map_err(|e| {
    Error::Render(RenderError::InvalidParameters {
      message: format!("Failed to decode rendered PNG: {e}"),
    })
  })?;
  let expected_img = image::load_from_memory(expected).map_err(|e| {
    Error::Render(RenderError::InvalidParameters {
      message: format!("Failed to decode expected PNG: {e}"),
    })
  })?;

  let rendered_img = rendered_img.to_rgba8();
  let expected_img = expected_img.to_rgba8();

  if rendered_img.dimensions() != expected_img.dimensions() {
    return Err(Error::Render(RenderError::InvalidParameters {
      message: format!(
        "Image dimensions differ: rendered {}x{}, expected {}x{}",
        rendered_img.width(),
        rendered_img.height(),
        expected_img.width(),
        expected_img.height()
      ),
    }));
  }

  let (width, height) = rendered_img.dimensions();
  let mut diff_image = RgbaImage::new(width, height);
  let mut diff_pixels = 0u64;

  for ((dst, rendered_px), expected_px) in diff_image
    .pixels_mut()
    .zip(rendered_img.pixels())
    .zip(expected_img.pixels())
  {
    let channel_diffs = [
      rendered_px[0].abs_diff(expected_px[0]),
      rendered_px[1].abs_diff(expected_px[1]),
      rendered_px[2].abs_diff(expected_px[2]),
      rendered_px[3].abs_diff(expected_px[3]),
    ];

    let max_channel = *channel_diffs.iter().max().unwrap_or(&0);

    if max_channel > tolerance {
      diff_pixels += 1;
      // Encode the difference magnitude into the alpha channel so thin diffs still show up.
      let intensity = max_channel.saturating_mul(2).min(255);
      *dst = Rgba([255, 0, 0, intensity]);
    } else {
      *dst = Rgba([0, 0, 0, 0]);
    }
  }

  let total_pixels = (width * height) as u64;
  let diff_percentage = if total_pixels == 0 {
    0.0
  } else {
    (diff_pixels as f64 / total_pixels as f64) * 100.0
  };

  let metrics = DiffMetrics {
    pixel_diff: diff_pixels,
    total_pixels,
    diff_percentage,
  };

  let mut buffer = Vec::new();
  diff_image
    .write_to(&mut Cursor::new(&mut buffer), ImageFormat::Png)
    .map_err(|e| {
      Error::Render(RenderError::EncodeFailed {
        format: "PNG".to_string(),
        reason: e.to_string(),
      })
    })?;

  Ok((metrics, buffer))
}
