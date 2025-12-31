use base64::{engine::general_purpose, Engine as _};
use fastrender::{DiagnosticsLevel, FastRender, RenderOptions};
use image::codecs::png::PngEncoder;
use image::{ColorType, ImageEncoder};

fn solid_png_data_url(width: u32, height: u32, rgba: [u8; 4]) -> String {
  let mut pixels = vec![0u8; (width * height * 4) as usize];
  for chunk in pixels.chunks_exact_mut(4) {
    chunk.copy_from_slice(&rgba);
  }
  let mut png = Vec::new();
  PngEncoder::new(&mut png)
    .write_image(&pixels, width, height, ColorType::Rgba8.into())
    .expect("encode png");
  let b64 = general_purpose::STANDARD.encode(png);
  format!("data:image/png;base64,{b64}")
}

fn restore_backend(prev: Option<std::ffi::OsString>) {
  if let Some(value) = prev {
    std::env::set_var("FASTR_PAINT_BACKEND", value);
  } else {
    std::env::remove_var("FASTR_PAINT_BACKEND");
  }
}

#[test]
fn paint_diagnostics_include_attribution_counters() {
  let prev_backend = std::env::var_os("FASTR_PAINT_BACKEND");

  let data_url = solid_png_data_url(32, 32, [255, 0, 0, 255]);
  let html = format!(
    r#"
      <style>
        html, body {{ margin: 0; }}
        .bg {{
          width: 100px;
          height: 100px;
          background-image: url("{data_url}");
          background-size: 20px 20px;
          background-repeat: repeat;
        }}
        .bgx {{
          width: 100px;
          height: 20px;
          background-image: url("{data_url}");
          background-size: 20px 20px;
          background-repeat: repeat-x;
        }}
        .clip {{
          width: 60px;
          height: 60px;
          overflow: hidden;
          border-radius: 8px;
          opacity: 0.99;
        }}
        .inner {{
          width: 120px;
          height: 120px;
          background: rgb(0, 120, 255);
        }}
        img {{ display: block; width: 32px; height: 32px; }}
      </style>
      <div class="bg"></div>
      <div class="bgx"></div>
      <div class="clip"><div class="inner"></div></div>
      <img src="{data_url}">
      <img src="{data_url}">
    "#
  );

  let options = RenderOptions::new()
    .with_viewport(140, 300)
    .with_diagnostics_level(DiagnosticsLevel::Basic);

  std::env::set_var("FASTR_PAINT_BACKEND", "legacy");
  let mut legacy = FastRender::new().expect("renderer");
  let legacy_report = legacy
    .render_html_with_diagnostics(&html, options.clone())
    .expect("legacy render");
  let legacy_stats = legacy_report
    .diagnostics
    .stats
    .as_ref()
    .expect("legacy stats should be present");
  let legacy_paint = &legacy_stats.paint;
  assert!(
    legacy_paint.background_tiles.unwrap_or(0) > 0,
    "expected background tile count"
  );
  assert!(
    legacy_paint.background_ms.unwrap_or(0.0) > 0.0,
    "expected background timing"
  );
  assert!(
    legacy_paint.image_pixmap_ms.unwrap_or(0.0) > 0.0,
    "expected image pixmap conversion time"
  );
  assert!(
    legacy_paint.clip_mask_calls.unwrap_or(0) > 0,
    "expected clip mask calls"
  );
  assert!(
    legacy_paint.clip_mask_pixels.unwrap_or(0) > 0,
    "expected clip mask pixel accounting"
  );
  assert!(
    legacy_paint.layer_allocations.unwrap_or(0) > 0,
    "expected layer allocations"
  );
  assert!(
    legacy_paint.layer_alloc_bytes.unwrap_or(0) > 0,
    "expected layer allocation bytes"
  );

  std::env::set_var("FASTR_PAINT_BACKEND", "display_list");
  let mut display_list = FastRender::new().expect("renderer");
  let dl_report = display_list
    .render_html_with_diagnostics(&html, options)
    .expect("display list render");
  let dl_stats = dl_report
    .diagnostics
    .stats
    .as_ref()
    .expect("display list stats should be present");
  let dl_paint = &dl_stats.paint;
  assert!(
    dl_paint.background_tiles.unwrap_or(0) > 0,
    "expected display list background tiles"
  );
  assert!(
    dl_paint.background_ms.unwrap_or(0.0) > 0.0,
    "expected display list background timing"
  );
  assert!(
    dl_paint.image_pixmap_cache_misses.unwrap_or(0) > 0,
    "expected image pixmap cache misses"
  );
  assert!(
    dl_paint.image_pixmap_cache_hits.unwrap_or(0) > 0,
    "expected image pixmap cache hits"
  );
  assert!(
    dl_paint.image_pixmap_ms.unwrap_or(0.0) > 0.0,
    "expected image pixmap ms in display list renderer"
  );
  assert!(
    dl_paint.clip_mask_calls.unwrap_or(0) > 0,
    "expected clip mask calls in display list renderer"
  );
  assert!(
    dl_paint.clip_mask_pixels.unwrap_or(0) > 0,
    "expected clip mask pixel accounting in display list renderer"
  );
  assert!(
    dl_paint.layer_allocations.unwrap_or(0) > 0,
    "expected layer allocations in display list renderer"
  );
  assert!(
    dl_paint.layer_alloc_bytes.unwrap_or(0) > 0,
    "expected layer allocation bytes in display list renderer"
  );

  restore_backend(prev_backend);
}
