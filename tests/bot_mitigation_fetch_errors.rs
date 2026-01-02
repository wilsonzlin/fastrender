use fastrender::api::{DiagnosticsLevel, FastRender, RenderOptions};
use fastrender::error::RenderStage;
use fastrender::resource::{FetchedResource, ResourceFetcher};
use fastrender::text::font_db::FontConfig;
use std::sync::Arc;

#[derive(Clone)]
struct StaticFetcher {
  resource: FetchedResource,
}

impl ResourceFetcher for StaticFetcher {
  fn fetch(&self, _url: &str) -> fastrender::Result<FetchedResource> {
    Ok(self.resource.clone())
  }
}

#[test]
fn bot_mitigation_captcha_subresource_does_not_set_failure_stage() {
  let mut res = FetchedResource::new(
    b"<html>captcha</html>".to_vec(),
    Some("text/html; charset=utf-8".to_string()),
  );
  res.status = Some(405);
  res.final_url = Some("https://example.com/image.jpg?captcha=deadbeef".to_string());

  let fetcher = Arc::new(StaticFetcher { resource: res });
  let mut renderer = FastRender::builder()
    .fetcher(fetcher)
    .font_sources(FontConfig::bundled_only())
    .build()
    .expect("build renderer");

  let html = r#"<!doctype html><html><body><img src="https://example.com/image.jpg" width="10" height="10"></body></html>"#;
  let options = RenderOptions::new()
    .with_viewport(64, 64)
    .with_diagnostics_level(DiagnosticsLevel::None);
  let result = renderer
    .render_html_with_diagnostics(html, options)
    .expect("render should succeed");

  assert!(
    result.diagnostics.failure_stage.is_none(),
    "captcha-blocked subresources should not set failure_stage"
  );
  assert!(
    result.diagnostics.fetch_errors.is_empty(),
    "captcha-blocked subresources should be excluded from fetch_errors"
  );
  assert!(
    !result.diagnostics.blocked_fetch_errors.is_empty(),
    "captcha-blocked subresources should be captured in blocked_fetch_errors"
  );
  assert!(
    result
      .diagnostics
      .blocked_fetch_errors
      .iter()
      .any(|e| e.status == Some(405) && e.final_url.as_deref().is_some_and(|u| u.contains("captcha="))),
    "blocked_fetch_errors should include the HTTP 405 captcha response"
  );
}

#[test]
fn decode_failure_still_sets_failure_stage_paint() {
  let mut res = FetchedResource::new(
    // Invalid PNG header/body.
    b"not a png".to_vec(),
    Some("image/png".to_string()),
  );
  res.status = Some(200);
  res.final_url = Some("https://example.com/bad.png".to_string());

  let fetcher = Arc::new(StaticFetcher { resource: res });
  let mut renderer = FastRender::builder()
    .fetcher(fetcher)
    .font_sources(FontConfig::bundled_only())
    .build()
    .expect("build renderer");

  let html = r#"<!doctype html><html><body><img src="https://example.com/bad.png" width="10" height="10"></body></html>"#;
  let options = RenderOptions::new()
    .with_viewport(64, 64)
    .with_diagnostics_level(DiagnosticsLevel::None);
  let result = renderer
    .render_html_with_diagnostics(html, options)
    .expect("render should succeed");

  assert_eq!(
    result.diagnostics.failure_stage,
    Some(RenderStage::Paint),
    "decode failures should still attribute a paint failure_stage"
  );
  assert!(
    !result.diagnostics.fetch_errors.is_empty(),
    "decode failures should remain in fetch_errors"
  );
  assert!(
    result.diagnostics.blocked_fetch_errors.is_empty(),
    "decode failures should not be classified as bot mitigation"
  );
}
