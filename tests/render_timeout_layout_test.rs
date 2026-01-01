use fastrender::api::{FastRender, RenderOptions};
use fastrender::error::{Error, RenderError, RenderStage};
use fastrender::render_control::{set_stage_listener, StageHeartbeat};
use fastrender::LayoutParallelism;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Mutex, OnceLock};

fn stage_listener_lock() -> std::sync::MutexGuard<'static, ()> {
  static LOCK: OnceLock<Mutex<()>> = OnceLock::new();
  LOCK
    .get_or_init(|| Mutex::new(()))
    .lock()
    .unwrap_or_else(|poisoned| poisoned.into_inner())
}

struct StageListenerGuard;

impl Drop for StageListenerGuard {
  fn drop(&mut self) {
    set_stage_listener(None);
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
  let _lock = stage_listener_lock();

  let layout_active = Arc::new(AtomicBool::new(false));
  let layout_checks = Arc::new(AtomicUsize::new(0));
  let layout_active_listener = Arc::clone(&layout_active);
  let layout_checks_listener = Arc::clone(&layout_checks);
  set_stage_listener(Some(Arc::new(move |stage| {
    let active = stage == StageHeartbeat::Layout;
    layout_active_listener.store(active, Ordering::Relaxed);
    if active {
      layout_checks_listener.store(0, Ordering::Relaxed);
    }
  })));
  let _stage_guard = StageListenerGuard;

  // Cancel on the second deadline check observed during layout. This exercises layout's periodic
  // deadline checks while avoiding fragile wall-clock thresholds (DOM parse speed varies).
  let layout_active_cancel = Arc::clone(&layout_active);
  let layout_checks_cancel = Arc::clone(&layout_checks);
  let cancel_callback: Arc<fastrender::CancelCallback> = Arc::new(move || {
    if !layout_active_cancel.load(Ordering::Relaxed) {
      return false;
    }
    let seen = layout_checks_cancel.fetch_add(1, Ordering::Relaxed) + 1;
    seen >= 2
  });

  let mut renderer = FastRender::new().unwrap();
  let html = heavy_inline_html(4000);
  let options = RenderOptions::new()
    .with_viewport(64, 64)
    // Disable layout parallelism so deadline checks are deterministic (and we don't have to care
    // which Rayon worker first trips cancellation).
    .with_layout_parallelism(LayoutParallelism::disabled())
    .with_cancel_callback(Some(cancel_callback));

  let err = renderer
    .render_html_with_options(&html, options)
    .expect_err("layout should time out cooperatively");

  assert!(
    layout_checks.load(Ordering::Relaxed) >= 2,
    "expected at least 2 layout deadline checks before cancellation"
  );

  match err {
    Error::Render(RenderError::Timeout { stage, .. }) => assert_eq!(stage, RenderStage::Layout),
    other => panic!("unexpected error: {other:?}"),
  }
}

#[test]
fn layout_timeout_records_diagnostics() {
  let _lock = stage_listener_lock();

  let layout_active = Arc::new(AtomicBool::new(false));
  let layout_checks = Arc::new(AtomicUsize::new(0));
  let layout_active_listener = Arc::clone(&layout_active);
  let layout_checks_listener = Arc::clone(&layout_checks);
  set_stage_listener(Some(Arc::new(move |stage| {
    let active = stage == StageHeartbeat::Layout;
    layout_active_listener.store(active, Ordering::Relaxed);
    if active {
      layout_checks_listener.store(0, Ordering::Relaxed);
    }
  })));
  let _stage_guard = StageListenerGuard;

  let layout_active_cancel = Arc::clone(&layout_active);
  let layout_checks_cancel = Arc::clone(&layout_checks);
  let cancel_callback: Arc<fastrender::CancelCallback> = Arc::new(move || {
    if !layout_active_cancel.load(Ordering::Relaxed) {
      return false;
    }
    let seen = layout_checks_cancel.fetch_add(1, Ordering::Relaxed) + 1;
    seen >= 2
  });

  let mut renderer = FastRender::new().unwrap();
  let html = heavy_inline_html(4000);
  let options = RenderOptions::new()
    .with_viewport(32, 24)
    .with_layout_parallelism(LayoutParallelism::disabled())
    .with_cancel_callback(Some(cancel_callback))
    .allow_partial(true);

  let result = renderer
    .render_html_with_stylesheets(&html, "https://example.com", options)
    .expect("layout timeout should produce diagnostics");

  assert!(
    layout_checks.load(Ordering::Relaxed) >= 2,
    "expected at least 2 layout deadline checks before cancellation"
  );

  assert_eq!(result.diagnostics.timeout_stage, Some(RenderStage::Layout));
  assert_eq!(result.pixmap.width(), 32);
  assert_eq!(result.pixmap.height(), 24);
}
