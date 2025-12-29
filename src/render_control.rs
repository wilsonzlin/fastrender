use crate::error::{RenderError, RenderStage};
use std::cell::RefCell;
use std::sync::{Arc, Mutex, OnceLock};
use std::time::{Duration, Instant};

thread_local! {
  static ACTIVE_DEADLINE: RefCell<Option<RenderDeadline>> = RefCell::new(None);
}

/// Callback type used to cooperatively cancel rendering work.
pub type CancelCallback = dyn Fn() -> bool + Send + Sync;

/// Tracks render start time and enforces optional timeouts or external cancellation.
#[derive(Clone)]
pub struct RenderDeadline {
  start: Instant,
  timeout: Option<Duration>,
  cancel: Option<Arc<CancelCallback>>,
}

/// Guard that installs an active deadline for the duration of a render stage.
pub struct DeadlineGuard {
  previous: Option<RenderDeadline>,
}

/// Stages surfaced via heartbeat callbacks.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StageHeartbeat {
  ReadCache,
  FollowRedirects,
  CssInline,
  DomParse,
  Cascade,
  Layout,
  PaintBuild,
  PaintRasterize,
  Done,
}

impl StageHeartbeat {
  pub fn as_str(self) -> &'static str {
    match self {
      StageHeartbeat::ReadCache => "read_cache",
      StageHeartbeat::FollowRedirects => "follow_redirects",
      StageHeartbeat::CssInline => "css_inline",
      StageHeartbeat::DomParse => "dom_parse",
      StageHeartbeat::Cascade => "cascade",
      StageHeartbeat::Layout => "layout",
      StageHeartbeat::PaintBuild => "paint_build",
      StageHeartbeat::PaintRasterize => "paint_rasterize",
      StageHeartbeat::Done => "done",
    }
  }

  pub fn from_str(raw: &str) -> Option<Self> {
    match raw.trim() {
      "read_cache" => Some(StageHeartbeat::ReadCache),
      "follow_redirects" => Some(StageHeartbeat::FollowRedirects),
      "css_inline" => Some(StageHeartbeat::CssInline),
      "dom_parse" => Some(StageHeartbeat::DomParse),
      "cascade" => Some(StageHeartbeat::Cascade),
      "layout" => Some(StageHeartbeat::Layout),
      "paint_build" => Some(StageHeartbeat::PaintBuild),
      "paint_rasterize" => Some(StageHeartbeat::PaintRasterize),
      "done" => Some(StageHeartbeat::Done),
      _ => None,
    }
  }

  pub fn hotspot(self) -> &'static str {
    match self {
      StageHeartbeat::ReadCache | StageHeartbeat::FollowRedirects | StageHeartbeat::DomParse => {
        "fetch"
      }
      StageHeartbeat::CssInline => "css",
      StageHeartbeat::Cascade => "cascade",
      StageHeartbeat::Layout => "layout",
      StageHeartbeat::PaintBuild | StageHeartbeat::PaintRasterize => "paint",
      StageHeartbeat::Done => "unknown",
    }
  }
}

type StageListener = Arc<dyn Fn(StageHeartbeat) + Send + Sync>;

fn stage_listener() -> &'static Mutex<Option<StageListener>> {
  static LISTENER: OnceLock<Mutex<Option<StageListener>>> = OnceLock::new();
  LISTENER.get_or_init(|| Mutex::new(None))
}

pub fn set_stage_listener(listener: Option<StageListener>) {
  if let Ok(mut guard) = stage_listener().lock() {
    *guard = listener;
  }
}

pub fn record_stage(stage: StageHeartbeat) {
  let maybe_listener = stage_listener()
    .lock()
    .ok()
    .and_then(|guard| guard.as_ref().cloned());
  if let Some(listener) = maybe_listener {
    listener(stage);
  }
}

impl RenderDeadline {
  /// Creates a new deadline tracker starting at the current instant.
  pub fn new(timeout: Option<Duration>, cancel: Option<Arc<CancelCallback>>) -> Self {
    Self {
      start: Instant::now(),
      timeout,
      cancel,
    }
  }

  /// Returns a disabled deadline that never triggers.
  pub fn none() -> Self {
    Self::new(None, None)
  }

  /// Returns true when either timeout or cancellation is configured.
  pub fn is_enabled(&self) -> bool {
    self.timeout.is_some() || self.cancel.is_some()
  }

  /// Elapsed time since the deadline started.
  pub fn elapsed(&self) -> Duration {
    self.start.elapsed()
  }

  /// Returns the configured timeout duration, if any.
  pub fn timeout_limit(&self) -> Option<Duration> {
    self.timeout
  }

  /// Remaining time until the configured timeout elapses, if any.
  ///
  /// Returns `None` when no timeout is configured or the deadline has already expired.
  pub fn remaining_timeout(&self) -> Option<Duration> {
    self
      .timeout
      .and_then(|limit| limit.checked_sub(self.elapsed()))
  }

  /// Check for timeout or cancellation at the given stage.
  pub fn check(&self, stage: RenderStage) -> Result<(), RenderError> {
    #[cfg(test)]
    if let Some(delay) = std::env::var("FASTR_TEST_RENDER_DELAY_MS")
      .ok()
      .and_then(|v| v.parse::<u64>().ok())
    {
      std::thread::sleep(Duration::from_millis(delay));
    }
    if let Some(cb) = &self.cancel {
      if cb() {
        return Err(RenderError::Timeout {
          stage,
          elapsed: self.elapsed(),
        });
      }
    }
    if let Some(limit) = self.timeout {
      let elapsed = self.elapsed();
      if elapsed >= limit {
        return Err(RenderError::Timeout { stage, elapsed });
      }
    }
    Ok(())
  }

  /// Periodically checks for timeout/cancellation every `stride` invocations.
  pub fn check_periodic(
    &self,
    counter: &mut usize,
    stride: usize,
    stage: RenderStage,
  ) -> Result<(), RenderError> {
    if !self.is_enabled() || stride == 0 {
      return Ok(());
    }
    *counter = counter.wrapping_add(1);
    if *counter % stride == 0 {
      self.check(stage)?;
    }
    Ok(())
  }
}

impl DeadlineGuard {
  /// Installs the provided deadline as the active deadline for the current thread.
  pub fn install(deadline: Option<&RenderDeadline>) -> Self {
    let cloned = deadline.cloned();
    let previous = ACTIVE_DEADLINE.with(|active| active.replace(cloned));
    Self { previous }
  }
}

impl Drop for DeadlineGuard {
  fn drop(&mut self) {
    let previous = self.previous.take();
    ACTIVE_DEADLINE.with(|active| {
      *active.borrow_mut() = previous;
    });
  }
}

/// Check against any active deadline stored for the current thread.
pub fn check_active(stage: RenderStage) -> Result<(), RenderError> {
  ACTIVE_DEADLINE.with(|active| {
    if let Some(deadline) = active.borrow().as_ref() {
      deadline.check(stage)
    } else {
      Ok(())
    }
  })
}

/// Periodically check against any active deadline stored for the current thread.
///
/// This is a low-friction helper for hot loops: call it with a local counter and a stride
/// to amortize deadline checks while still making `RenderOptions::timeout` effective.
///
/// Example:
/// ```rust,no_run
/// # use fastrender::error::RenderStage;
/// # let mut counter = 0usize;
/// fastrender::render_control::check_active_periodic(&mut counter, 1024, RenderStage::Layout)?;
/// # Ok::<(), fastrender::error::RenderError>(())
/// ```
pub fn check_active_periodic(
  counter: &mut usize,
  stride: usize,
  stage: RenderStage,
) -> Result<(), RenderError> {
  if stride == 0 {
    return Ok(());
  }
  ACTIVE_DEADLINE.with(|active| -> Result<(), RenderError> {
    if let Some(deadline) = active.borrow().as_ref() {
      deadline.check_periodic(counter, stride, stage)
    } else {
      Ok(())
    }
  })
}

/// Returns the currently installed deadline for this thread, if any.
pub fn active_deadline() -> Option<RenderDeadline> {
  ACTIVE_DEADLINE.with(|active| active.borrow().clone())
}

/// Installs `deadline` for the duration of the provided closure.
pub fn with_deadline<T>(deadline: Option<&RenderDeadline>, f: impl FnOnce() -> T) -> T {
  let _guard = DeadlineGuard::install(deadline);
  f()
}
