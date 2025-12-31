//! Cooperative, periodic cancellation support for long-running layout computations.
//!
//! This module provides a very lightweight mechanism to request that layout abort early:
//! - A caller installs a cancellation callback in a scoped manner (RAII).
//! - Layout code periodically calls [`check_layout_abort`].
//! - When the callback indicates cancellation, layout aborts by panicking with a dedicated payload
//!   (`LayoutAbort`). The high-level API catches this and converts it to a structured error.
//!
//! The cancellation mechanism is behind the `std` feature. In `no_std` builds, periodic
//! cancellation is disabled (installing a callback isn't supported and [`check_layout_abort`]
//! is a no-op) so layout behaviour remains unchanged unless callers explicitly invoke
//! [`abort_now`].

#[cfg(feature = "std")]
use std::cell::{Cell, RefCell};
#[cfg(feature = "std")]
use std::sync::atomic::{AtomicUsize, Ordering};
#[cfg(feature = "std")]
use std::sync::Arc;

#[cfg(feature = "std")]
type CancelCallback = Arc<dyn Fn() -> bool + Send + Sync + 'static>;

/// Panic payload used to cooperatively abort an in-progress layout run.
///
/// This is only meant to be caught by the high-level `TaffyTree` cancellation API.
#[cfg(feature = "std")]
#[derive(Debug)]
pub(crate) struct LayoutAbort;

/// Global count of active cancellation guards. When zero, [`check_layout_abort`] returns immediately
/// without touching thread-local state. This keeps the hot path cheap when cancellation isn't used.
#[cfg(feature = "std")]
static ACTIVE_LAYOUT_ABORT_GUARDS: AtomicUsize = AtomicUsize::new(0);

/// Thread-local cancellation state.
#[cfg(feature = "std")]
struct LayoutAbortState {
  /// Whether cancellation is enabled for this thread.
  enabled: Cell<bool>,
  /// How many `check_layout_abort` calls to skip between invoking the callback.
  stride: Cell<usize>,
  /// Countdown until the next callback invocation.
  remaining: Cell<usize>,
  /// Cancellation callback (stored separately from the `Cell` fields).
  callback: RefCell<Option<CancelCallback>>,
}

#[cfg(feature = "std")]
impl LayoutAbortState {
  fn new() -> Self {
    Self {
      enabled: Cell::new(false),
      stride: Cell::new(1),
      remaining: Cell::new(1),
      callback: RefCell::new(None),
    }
  }

  fn snapshot(&self) -> SavedLayoutAbortState {
    let callback = self.callback.borrow_mut().take();
    SavedLayoutAbortState {
      enabled: self.enabled.get(),
      stride: self.stride.get(),
      remaining: self.remaining.get(),
      callback,
    }
  }

  fn restore(&self, saved: SavedLayoutAbortState) {
    self.enabled.set(saved.enabled);
    self.stride.set(saved.stride);
    self.remaining.set(saved.remaining);
    *self.callback.borrow_mut() = saved.callback;
  }

  fn install(&self, callback: CancelCallback, check_stride: usize) {
    let stride = check_stride.max(1);

    self.enabled.set(true);
    self.stride.set(stride);
    // Call the callback on the first check to allow an immediate abort if the deadline
    // has already been exceeded.
    self.remaining.set(1);
    *self.callback.borrow_mut() = Some(callback);
  }
}

#[cfg(feature = "std")]
struct SavedLayoutAbortState {
  enabled: bool,
  stride: usize,
  remaining: usize,
  callback: Option<CancelCallback>,
}

#[cfg(feature = "std")]
thread_local! {
  static LAYOUT_ABORT_STATE: LayoutAbortState = LayoutAbortState::new();
}

/// A scoped guard that installs a cancellation callback for the current thread.
///
/// Nested guards are supported. When the guard is dropped, the previously-installed callback
/// (if any) is restored.
#[cfg(feature = "std")]
pub(crate) struct LayoutAbortGuard {
  previous: Option<SavedLayoutAbortState>,
}

#[cfg(feature = "std")]
impl LayoutAbortGuard {
  pub(crate) fn new(callback: CancelCallback, check_stride: usize) -> Self {
    ACTIVE_LAYOUT_ABORT_GUARDS.fetch_add(1, Ordering::Relaxed);

    let previous = LAYOUT_ABORT_STATE.with(|state| {
      let previous = state.snapshot();
      state.install(callback, check_stride);
      previous
    });

    Self { previous: Some(previous) }
  }
}

#[cfg(feature = "std")]
impl Drop for LayoutAbortGuard {
  fn drop(&mut self) {
    if let Some(previous) = self.previous.take() {
      LAYOUT_ABORT_STATE.with(|state| state.restore(previous));
    }

    ACTIVE_LAYOUT_ABORT_GUARDS.fetch_sub(1, Ordering::Relaxed);
  }
}

/// Install a cancellation callback for the duration of the provided closure.
#[cfg(feature = "std")]
pub(crate) fn with_layout_abort<R>(
  callback: CancelCallback,
  check_stride: usize,
  f: impl FnOnce() -> R,
) -> R {
  let _guard = LayoutAbortGuard::new(callback, check_stride);
  f()
}

/// Check whether the current layout run should be aborted.
///
/// When a cancellation callback is installed for the current thread and it indicates that we should
/// abort, this function panics with [`LayoutAbort`] (which is intended to be caught at the outer
/// layout boundary and converted into a structured error).
#[cfg(feature = "std")]
#[inline(always)]
pub(crate) fn check_layout_abort() {
  if ACTIVE_LAYOUT_ABORT_GUARDS.load(Ordering::Relaxed) == 0 {
    return;
  }

  LAYOUT_ABORT_STATE.with(|state| {
    if !state.enabled.get() {
      return;
    }

    let remaining = state.remaining.get();
    if remaining > 1 {
      state.remaining.set(remaining - 1);
      return;
    }

    state.remaining.set(state.stride.get());

    // Clone the callback so we don't hold a RefCell borrow while invoking it (allowing
    // reentrancy, e.g. if the callback itself triggers layout).
    let callback = state.callback.borrow().as_ref().cloned();
    let Some(callback) = callback else { return };

    if callback() {
      std::panic::panic_any(LayoutAbort);
    }
  });
}

/// No-op cancellation check for `no_std` builds.
#[cfg(not(feature = "std"))]
#[inline(always)]
pub(crate) fn check_layout_abort() {}

/// Immediately abort the current Taffy layout computation.
///
/// This is intended for use by callers (for example, measure functions) that
/// detect a cancellation condition and want to stop the current layout run
/// *immediately* rather than returning a dummy size and waiting for the next
/// periodic [`check_layout_abort`] call.
///
/// # Panics
///
/// When built with the `std` feature, this function panics with the same
/// internal payload used by [`check_layout_abort`]. Layout entrypoints that
/// support cooperative cancellation (such as
/// [`TaffyTree::compute_layout_with_measure_and_cancel`](crate::TaffyTree::compute_layout_with_measure_and_cancel))
/// catch this panic and return [`TaffyError::LayoutAborted`](crate::tree::TaffyError::LayoutAborted).
///
/// If called outside of a cancellation-aware entrypoint, the panic will
/// propagate.
#[cfg(feature = "std")]
#[inline]
pub fn abort_now() -> ! {
  std::panic::panic_any(LayoutAbort)
}

/// Abort the current layout computation in `no_std` builds.
///
/// Cooperative cancellation (installing a callback and catching the abort at the outer boundary)
/// requires the `std` feature. However, callers may still want a convenient way to stop layout
/// from inside a measure function. In `no_std` builds we can't convert this into a structured
/// `TaffyError`, so we unconditionally panic.
#[cfg(not(feature = "std"))]
#[inline]
pub fn abort_now() -> ! {
  panic!("Layout computation was aborted")
}
