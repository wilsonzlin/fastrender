use fastrender::api::FastRender;
use fastrender::api::RenderOptions;
use fastrender::error::{Error, RenderError, RenderStage};
use fastrender::render_control::{set_stage_listener, StageHeartbeat};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};

struct StageListenerGuard;

impl Drop for StageListenerGuard {
  fn drop(&mut self) {
    set_stage_listener(None);
  }
}

#[test]
fn cascade_selector_matching_obeys_timeout() {
  let cascade_active = Arc::new(AtomicBool::new(false));
  let cascade_checks = Arc::new(AtomicUsize::new(0));

  // Use stage heartbeats to only trip the cancel callback once cascade begins. This avoids relying
  // on fragile wall-clock thresholds and makes the test independent of HTML parse speed.
  let cascade_active_listener = Arc::clone(&cascade_active);
  set_stage_listener(Some(Arc::new(move |stage| {
    cascade_active_listener.store(stage == StageHeartbeat::Cascade, Ordering::Relaxed);
  })));
  let _stage_listener_guard = StageListenerGuard;

  // Cancel on the *second* deadline check observed during the cascade stage. This ensures that at
  // least one deadline check happened within cascade selector matching (not only at stage
  // boundaries).
  let cascade_active_cancel = Arc::clone(&cascade_active);
  let cascade_checks_cancel = Arc::clone(&cascade_checks);
  let cancel_callback: Arc<fastrender::CancelCallback> = Arc::new(move || {
    if !cascade_active_cancel.load(Ordering::Relaxed) {
      return false;
    }
    let seen = cascade_checks_cancel.fetch_add(1, Ordering::Relaxed) + 1;
    seen >= 2
  });

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
    .with_cancel_callback(Some(cancel_callback));

  let start = Instant::now();
  let err = renderer
    .render_html_with_options(&html, options)
    .expect_err("render should time out during cascade");
  let elapsed = start.elapsed();
  assert!(
    elapsed < Duration::from_millis(500),
    "cascade timeout took too long: {elapsed:?}"
  );

  assert!(
    cascade_checks.load(Ordering::Relaxed) >= 2,
    "expected at least 2 cascade deadline checks before cancellation"
  );

  match err {
    Error::Render(RenderError::Timeout { stage, .. }) => assert_eq!(stage, RenderStage::Cascade),
    other => panic!("unexpected error: {other:?}"),
  }
}
