//! Instrumentation hooks for Taffy-backed layout
//!
//! This module provides lightweight counters that track when the vendored
//! Taffy engine is invoked. The counters are compiled only in debug/test
//! builds to avoid any release overhead. They backstop the invariant that
//! Taffy must only be used for flex/grid layout, never for tables.

#[cfg(any(test, debug_assertions))]
use std::cell::RefCell;

/// Count of Taffy layout invocations per adapter.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct TaffyUsageCounters {
  /// Number of flex layout executions routed through Taffy.
  pub flex: u64,
  /// Number of grid layout executions routed through Taffy.
  pub grid: u64,
}

/// Which adapter recorded a Taffy layout pass.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum TaffyAdapterKind {
  Flex,
  Grid,
}

#[cfg(any(test, debug_assertions))]
thread_local! {
  static COUNTERS: RefCell<TaffyUsageCounters> = RefCell::new(TaffyUsageCounters { flex: 0, grid: 0 });
}

/// Records that a Taffy layout was executed. No-ops in release builds.
#[inline]
pub(crate) fn record_taffy_invocation(_kind: TaffyAdapterKind) {
  #[cfg(any(test, debug_assertions))]
  COUNTERS.with(|counts| {
    let mut counts = counts.borrow_mut();
    match _kind {
      TaffyAdapterKind::Flex => counts.flex += 1,
      TaffyAdapterKind::Grid => counts.grid += 1,
    }
  });
}

/// Resets counters for the current thread. No-ops in release builds.
#[inline]
pub fn reset_taffy_counters() {
  #[cfg(any(test, debug_assertions))]
  COUNTERS.with(|counts| *counts.borrow_mut() = TaffyUsageCounters::default());
}

/// Returns the current counters for the current thread.
#[inline]
pub fn taffy_counters() -> TaffyUsageCounters {
  #[cfg(any(test, debug_assertions))]
  return COUNTERS.with(|counts| *counts.borrow());

  #[allow(unreachable_code)]
  TaffyUsageCounters::default()
}

/// Convenience accessor for the total number of Taffy invocations observed.
#[inline]
pub fn total_taffy_invocations() -> u64 {
  let counts = taffy_counters();
  counts.flex + counts.grid
}
