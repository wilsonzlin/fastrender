use fastrender::api::FastRender;
use fastrender::api::RenderOptions;
use fastrender::error::{Error, RenderError, RenderStage};
use std::time::{Duration, Instant};

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
fn cascade_selector_matching_obeys_timeout() {
  // Inject a delay into each deadline check so a single cooperative check guarantees a timeout.
  let _guard = EnvVarGuard::set("FASTR_TEST_RENDER_DELAY_MS", "10");
  let mut renderer = FastRender::new().unwrap();

  let mut css = String::new();
  for i in 0..2048 {
    css.push_str(&format!(
      "*:has(.child-{i} .grandchild-{i}) {{ color: #{:06x}; }}\n",
      i
    ));
  }

  let mut html = String::from("<style>");
  html.push_str(&css);
  html.push_str("</style><div class=\"root\">");
  for i in 0..30 {
    html.push_str(&format!(
      "<div class=\"child-{i}\"><div class=\"grandchild-{i}\"></div></div>"
    ));
  }
  html.push_str("</div>");

  let options = RenderOptions::default()
    .with_viewport(64, 64)
    .with_timeout(Some(Duration::from_millis(1)));

  let start = Instant::now();
  let err = renderer
    .render_html_with_options(&html, options)
    .expect_err("render should time out during cascade");
  let elapsed = start.elapsed();
  assert!(
    elapsed < Duration::from_millis(500),
    "cascade timeout took too long: {elapsed:?}"
  );

  match err {
    Error::Render(RenderError::Timeout { stage, .. }) => assert_eq!(stage, RenderStage::Cascade),
    other => panic!("unexpected error: {other:?}"),
  }
}
