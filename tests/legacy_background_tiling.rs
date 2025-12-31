use base64::engine::general_purpose::STANDARD;
use base64::Engine;
use fastrender::{DiagnosticsLevel, FastRender, RenderOptions};
use image::codecs::png::PngEncoder;
use image::ColorType;
use image::ImageEncoder;
use std::sync::{Mutex, OnceLock};

fn backend_lock() -> std::sync::MutexGuard<'static, ()> {
  static LOCK: OnceLock<Mutex<()>> = OnceLock::new();
  LOCK.get_or_init(|| Mutex::new(())).lock().unwrap()
}

fn set_legacy_backend() -> Option<std::ffi::OsString> {
  let prev = std::env::var_os("FASTR_PAINT_BACKEND");
  std::env::set_var("FASTR_PAINT_BACKEND", "legacy");
  prev
}

fn restore_backend(prev: Option<std::ffi::OsString>) {
  if let Some(value) = prev {
    std::env::set_var("FASTR_PAINT_BACKEND", value);
  } else {
    std::env::remove_var("FASTR_PAINT_BACKEND");
  }
}

fn make_test_png_data_url() -> String {
  let img = image::RgbaImage::from_pixel(1, 1, image::Rgba([255, 0, 0, 255]));
  let mut bytes = Vec::new();
  PngEncoder::new(&mut bytes)
    .write_image(
      img.as_raw(),
      img.width(),
      img.height(),
      ColorType::Rgba8.into(),
    )
    .expect("encode png");
  format!("data:image/png;base64,{}", STANDARD.encode(bytes))
}

#[test]
fn legacy_background_tiling_clamped_to_canvas_and_uses_pattern_fast_path() {
  let _guard = backend_lock();
  let prev_backend = set_legacy_backend();

  let img_data_url = make_test_png_data_url();

  let html = format!(
    r#"<!doctype html>
<style>
  html, body {{ margin: 0; }}
  .big {{
    width: 800px;
    height: 200000px;
    background-image: url("{img_data_url}");
    background-repeat: repeat;
    background-size: 50px 50px;
  }}
</style>
<div class="big"></div>
"#
  );

  let mut renderer = FastRender::new().expect("renderer");
  let options = RenderOptions::new()
    .with_viewport(800, 600)
    .with_diagnostics_level(DiagnosticsLevel::Basic);
  let result = renderer
    .render_html_with_diagnostics(&html, options)
    .expect("render should succeed");

  restore_backend(prev_backend);
  assert!(
    result.diagnostics.fetch_errors.is_empty(),
    "expected data URL image to decode successfully (errors={:?})",
    result.diagnostics.fetch_errors
  );

  let stats = result
    .diagnostics
    .stats
    .as_ref()
    .expect("render stats should be present");
  let tiles = stats
    .paint
    .background_tiles_painted
    .expect("background tile count should be recorded");
  let fast_paths = stats.paint.background_pattern_fast_paths.unwrap_or(0);

  assert!(
    tiles < 10_000,
    "expected background tile work to be clamped to the 800x600 canvas (tiles_painted={tiles})"
  );
  assert!(
    fast_paths > 0,
    "expected repeat/repeat background to use pattern fast path (fast_paths={fast_paths})"
  );
}
