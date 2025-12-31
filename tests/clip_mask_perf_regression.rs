use fastrender::api::DiagnosticsLevel;
use fastrender::RenderOptions;
use fastrender::{FastRender, Rgba};

#[test]
fn clip_mask_diagnostics_are_reported() {
  std::env::set_var("FASTR_PAINT_BACKEND", "legacy");
  let html = r#"
    <style>
      html, body { margin: 0; padding: 0; background: white; }
      .clip {
        width: 20px;
        height: 20px;
        overflow: hidden;
        border-radius: 6px;
        position: relative;
        z-index: 0;
      }
      .child {
        width: 40px;
        height: 40px;
        background: red;
      }
    </style>
    <div class="clip"><div class="child"></div></div>
  "#;

  let mut renderer = FastRender::with_config(
    fastrender::FastRenderConfig::new().with_default_background(Rgba::WHITE),
  )
  .expect("renderer");

  let options = RenderOptions::default()
    .with_viewport(64, 64)
    .with_diagnostics_level(DiagnosticsLevel::Basic);

  let result = renderer
    .render_html_with_diagnostics(html, options)
    .expect("render");

  let stats = result.diagnostics.stats.expect("stats");
  let calls = stats.paint.clip_mask_calls.expect("clip_mask_calls");
  let pixels = stats.paint.clip_mask_pixels.expect("clip_mask_pixels");

  assert!(
    calls > 0,
    "expected at least one clip mask call, got {calls}"
  );
  assert!(pixels > 0, "expected clip mask pixels > 0, got {pixels}");
  assert!(
    stats.paint.clip_mask_ms.expect("clip_mask_ms") >= 0.0,
    "clip_mask_ms should be present"
  );
}
