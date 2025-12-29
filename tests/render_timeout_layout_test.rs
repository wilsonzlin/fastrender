use fastrender::api::{FastRender, RenderOptions};
use fastrender::error::{Error, RenderError, RenderStage};
use std::time::Duration;

struct EnvVarGuard {
  key: &'static str,
}

impl EnvVarGuard {
  fn set(key: &'static str, value: &str) -> Self {
    std::env::set_var(key, value);
    Self { key }
  }
}

impl Drop for EnvVarGuard {
  fn drop(&mut self) {
    std::env::remove_var(self.key);
  }
}

fn heavy_inline_html(count: usize) -> String {
  let mut html = String::from("<div>");
  for i in 0..count {
    html.push_str(&format!("<span class=\"c{i}\">item</span>"));
  }
  html.push_str("</div>");
  html
}

#[test]
fn layout_loops_respect_timeout() {
  let _guard = EnvVarGuard::set("FASTR_TEST_RENDER_DELAY_MS", "5");
  let mut renderer = FastRender::new().unwrap();
  let html = heavy_inline_html(4000);
  let options = RenderOptions::new()
    .with_viewport(64, 64)
    .with_timeout(Some(Duration::from_millis(20)));

  let err = renderer
    .render_html_with_options(&html, options)
    .expect_err("layout should time out cooperatively");

  match err {
    Error::Render(RenderError::Timeout { stage, .. }) => assert_eq!(stage, RenderStage::Layout),
    other => panic!("unexpected error: {other:?}"),
  }
}

#[test]
fn layout_timeout_records_diagnostics() {
  let _guard = EnvVarGuard::set("FASTR_TEST_RENDER_DELAY_MS", "5");
  let mut renderer = FastRender::new().unwrap();
  let html = heavy_inline_html(4000);
  let options = RenderOptions::new()
    .with_viewport(32, 24)
    .with_timeout(Some(Duration::from_millis(20)))
    .allow_partial(true);

  let result = renderer
    .render_html_with_stylesheets(&html, "https://example.com", options)
    .expect("layout timeout should produce diagnostics");

  assert_eq!(result.diagnostics.timeout_stage, Some(RenderStage::Layout));
  assert_eq!(result.pixmap.width(), 32);
  assert_eq!(result.pixmap.height(), 24);
}
