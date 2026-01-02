use std::path::PathBuf;
use std::time::Duration;

use fastrender::api::{FastRender, RenderOptions};
use fastrender::render_control::{set_stage_listener, StageHeartbeat};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::Arc;
use url::Url;

struct StageListenerGuard;

impl Drop for StageListenerGuard {
  fn drop(&mut self) {
    set_stage_listener(None);
  }
}

#[test]
fn css_inlining_respects_deadline() {
  let css_active = Arc::new(AtomicBool::new(false));
  let css_checks = Arc::new(AtomicUsize::new(0));
  let cancel_fired = Arc::new(AtomicBool::new(false));

  // Use stage heartbeats to trip cancellation while CSS inlining/parsing is active, avoiding
  // fragile wall-clock thresholds.
  let css_active_listener = Arc::clone(&css_active);
  set_stage_listener(Some(Arc::new(move |stage| {
    match stage {
      StageHeartbeat::CssInline => css_active_listener.store(true, Ordering::Relaxed),
      StageHeartbeat::Cascade => css_active_listener.store(false, Ordering::Relaxed),
      _ => {}
    }
  })));
  let _stage_listener_guard = StageListenerGuard;

  let css_active_cancel = Arc::clone(&css_active);
  let css_checks_cancel = Arc::clone(&css_checks);
  let cancel_fired = Arc::clone(&cancel_fired);
  let cancel_callback: Arc<fastrender::CancelCallback> = Arc::new(move || {
    if !css_active_cancel.load(Ordering::Relaxed) {
      return false;
    }
    if cancel_fired.swap(true, Ordering::Relaxed) {
      return false;
    }
    css_checks_cancel.fetch_add(1, Ordering::Relaxed);
    true
  });

  let mut renderer = FastRender::new().unwrap();
  let fixtures = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures");
  let outer_path = fixtures.join("css_timeout_outer.css");
  let outer_url = Url::from_file_path(&outer_path).unwrap().to_string();
  let html = format!(
    r#"<html><head><link rel="stylesheet" href="{outer_url}"></head><body>slow css</body></html>"#
  );
  let options = RenderOptions::default()
    .with_timeout(Some(Duration::from_secs(1)))
    .with_cancel_callback(Some(cancel_callback));

  let result = renderer
    .render_html_with_stylesheets(&html, "file:///css-timeout.html", options)
    .expect("render should continue after stylesheet timeout");

  assert_eq!(
    css_checks.load(Ordering::Relaxed),
    1,
    "expected exactly one CSS cancellation-triggering deadline check"
  );

  assert!(result.diagnostics.failure_stage.is_none());
  assert!(
    result
      .diagnostics
      .fetch_errors
      .iter()
      .any(|err| err.message.contains("timed out during css")),
    "expected stylesheet timeout error, got: {:?}",
    result.diagnostics.fetch_errors
  );
}
