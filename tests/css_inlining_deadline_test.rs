use std::path::PathBuf;
use std::time::Duration;

use fastrender::api::{FastRender, RenderOptions};
use fastrender::error::{Error, RenderError, RenderStage};
use url::Url;

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
fn css_inlining_respects_deadline() {
  let _guard = EnvVarGuard::set("FASTR_TEST_RENDER_DELAY_MS", "20");
  let mut renderer = FastRender::new().unwrap();
  let fixtures = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures");
  let outer_path = fixtures.join("css_timeout_outer.css");
  let outer_url = Url::from_file_path(&outer_path).unwrap().to_string();
  let html = format!(
    r#"<html><head><link rel="stylesheet" href="{outer_url}"></head><body>slow css</body></html>"#
  );
  let options = RenderOptions::default().with_timeout(Some(Duration::from_millis(1)));

  let err = renderer
    .render_html_with_stylesheets(&html, "file:///css-timeout.html", options)
    .unwrap_err();

  match err {
    Error::Render(RenderError::Timeout { stage, .. }) => assert_eq!(stage, RenderStage::Css),
    other => panic!("expected CSS timeout, got {:?}", other),
  }
}
