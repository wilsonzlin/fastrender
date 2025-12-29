use fastrender::css::types::StyleSheet;
use fastrender::dom;
use fastrender::error::{Error, RenderError, RenderStage};
use fastrender::render_control::{DeadlineGuard, RenderDeadline};
use fastrender::style::cascade::apply_styles;
use fastrender::tree::box_generation::generate_box_tree_with_anonymous_fixup;
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
fn box_generation_times_out_with_active_deadline() {
  let _guard = EnvVarGuard::set("FASTR_TEST_RENDER_DELAY_MS", "5");

  let mut repeated = String::new();
  for _ in 0..5000 {
    repeated.push_str("<div class=\"item\">content</div>");
  }
  let html = format!("<html><body>{repeated}</body></html>");

  let dom = dom::parse_html(&html).expect("parse html");
  let styled = apply_styles(&dom, &StyleSheet::new());

  let deadline = RenderDeadline::new(Some(Duration::from_millis(1)), None);
  let _deadline_guard = DeadlineGuard::install(Some(&deadline));

  let err = generate_box_tree_with_anonymous_fixup(&styled).unwrap_err();
  match err {
    Error::Render(RenderError::Timeout { stage, .. }) => assert_eq!(stage, RenderStage::Cascade),
    other => panic!("expected cascade timeout, got {other:?}"),
  }
}
