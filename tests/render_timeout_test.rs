use fastrender::api::FastRender;
use fastrender::api::RenderOptions;
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

#[test]
fn render_times_out_with_error() {
  let _guard = EnvVarGuard::set("FASTR_TEST_RENDER_DELAY_MS", "20");
  let mut renderer = FastRender::new().unwrap();
  let options = RenderOptions::default().with_timeout(Some(Duration::from_millis(1)));

  let err = renderer
    .render_html_with_options("<div>slow</div>", options)
    .unwrap_err();

  match err {
    Error::Render(RenderError::Timeout { stage, .. }) => assert_eq!(stage, RenderStage::DomParse),
    other => panic!("unexpected error: {other:?}"),
  }
}

#[test]
fn render_timeout_allows_partial_placeholder() {
  let _guard = EnvVarGuard::set("FASTR_TEST_RENDER_DELAY_MS", "20");
  let mut renderer = FastRender::new().unwrap();
  let options = RenderOptions::default()
    .with_viewport(40, 30)
    .with_timeout(Some(Duration::from_millis(1)))
    .allow_partial(true);

  let result = renderer
    .render_html_with_stylesheets("<div>slow</div>", "https://example.com", options)
    .unwrap();

  assert_eq!(result.pixmap.width(), 40);
  assert_eq!(result.pixmap.height(), 30);
  assert!(result.diagnostics.timeout_stage.is_some());
}
