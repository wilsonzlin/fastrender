use fastrender::api::DiagnosticsLevel;
use fastrender::{FastRender, FontConfig, RenderOptions, ResourcePolicy};
use std::fs;
use std::path::PathBuf;
use std::time::Instant;
use url::Url;

fn fixtures_dir() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/pages/fixtures")
}

#[test]
fn blog_rust_lang_text_diagnostics_are_reasonable() {
  let html_path = fixtures_dir().join("blog.rust-lang.org").join("index.html");
  let html = fs::read_to_string(&html_path).expect("read fixture html");
  let base_url = Url::from_directory_path(html_path.parent().expect("fixture dir"))
    .expect("fixture base url")
    .to_string();

  let policy = ResourcePolicy::default()
    .allow_http(false)
    .allow_https(false)
    .allow_file(true)
    .allow_data(true);

  let mut renderer = FastRender::builder()
    .base_url(base_url)
    .font_sources(FontConfig::bundled_only())
    .resource_policy(policy)
    .build()
    .expect("renderer build");

  let options = RenderOptions::new()
    .with_viewport(1040, 800)
    .with_diagnostics_level(DiagnosticsLevel::Basic);

  let start = Instant::now();
  let result = renderer
    .render_html_with_diagnostics(&html, options)
    .expect("render blog fixture");
  let wall_ms = start.elapsed().as_secs_f64() * 1000.0;

  let stats = result
    .diagnostics
    .stats
    .as_ref()
    .expect("expected diagnostics stats");

  let fallback_ms = stats.timings.text_fallback_ms.unwrap_or(0.0);
  let shape_ms = stats.timings.text_shape_ms.unwrap_or(0.0);
  let rasterize_ms = stats.timings.text_rasterize_ms.unwrap_or(0.0);

  assert!(
    fallback_ms <= wall_ms * 2.0,
    "text fallback time should be in the same order of magnitude as render wall time \
     (fallback_ms={fallback_ms:.3} wall_ms={wall_ms:.3})"
  );
  assert!(
    shape_ms <= wall_ms * 2.0,
    "text shaping time should be in the same order of magnitude as render wall time \
     (shape_ms={shape_ms:.3} wall_ms={wall_ms:.3})"
  );
  assert!(
    rasterize_ms <= wall_ms * 2.0,
    "text rasterization time should be in the same order of magnitude as render wall time \
     (rasterize_ms={rasterize_ms:.3} wall_ms={wall_ms:.3})"
  );
}
