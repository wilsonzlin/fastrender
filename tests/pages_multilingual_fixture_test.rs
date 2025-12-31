use fastrender::api::DiagnosticsLevel;
use fastrender::{FastRender, FontConfig, RenderOptions, ResourcePolicy};
use std::fs;
use std::path::PathBuf;
use url::Url;

fn fixtures_dir() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/pages/fixtures")
}

#[test]
fn multilingual_fixture_uses_bundled_fonts_without_last_resort_fallbacks() {
  let html_path = fixtures_dir().join("multilingual_text").join("index.html");
  let html = fs::read_to_string(&html_path).expect("read multilingual fixture html");
  for (label, snippet) in [
    ("arabic", "مرحبا بالعالم"),
    ("hebrew", "שלום עולם"),
    ("devanagari", "नमस्ते दुनिया"),
    ("bengali", "বাংলা লেখা পরীক্ষা"),
    ("tamil", "தமிழ் வணக்கம்"),
    ("thai", "สวัสดีชาวโลก"),
  ] {
    assert!(
      html.contains(snippet),
      "fixture missing {label} snippet: {snippet}"
    );
  }

  let base_url = Url::from_directory_path(html_path.parent().expect("fixture dir"))
    .expect("fixture base url")
    .to_string();

  let policy = ResourcePolicy::default()
    .allow_http(false)
    .allow_https(false)
    // The fixture is self-contained (no file:// subresources), but allow file URLs so relative URL
    // resolution remains consistent with other fixture tests.
    .allow_file(true)
    .allow_data(true);

  let mut renderer = FastRender::builder()
    .base_url(base_url)
    .font_sources(FontConfig::bundled_only())
    .resource_policy(policy)
    .build()
    .expect("renderer build");

  let options = RenderOptions::new()
    .with_viewport(920, 980)
    .with_diagnostics_level(DiagnosticsLevel::Basic);

  let result = renderer
    .render_html_with_diagnostics(&html, options)
    .expect("render multilingual fixture");

  assert!(
    result.diagnostics.failure_stage.is_none(),
    "fixture render should succeed without failures: {:?}",
    result.diagnostics.failure_stage
  );

  let stats = result
    .diagnostics
    .stats
    .as_ref()
    .expect("expected diagnostics stats");
  assert_eq!(
    stats.counts.last_resort_font_fallbacks,
    Some(0),
    "fixture should not hit last-resort font fallbacks (counts: {:?})",
    stats.counts
  );
}
