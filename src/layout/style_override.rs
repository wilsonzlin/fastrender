use crate::style::ComputedStyle;
use std::cell::RefCell;
use std::sync::Arc;

thread_local! {
  /// Stack of temporary `ComputedStyle` overrides keyed by `BoxNode` id.
  ///
  /// This is used in hot flex/grid measurement paths where we need to measure the same box with
  /// different effective sizing hints (e.g. treating percentage widths as `auto` during
  /// min-/max-content probes, or clearing an authored `width` when computing content-based
  /// minimum sizes for flexbox).
  ///
  /// Storing overrides in TLS avoids allocating/cloning entire `BoxNode` subtrees just to swap a
  /// style pointer, while still keeping the overrides scoped to the current thread and call stack.
  static STYLE_OVERRIDES: RefCell<Vec<(usize, Arc<ComputedStyle>)>> = RefCell::new(Vec::new());
}

pub(crate) struct StyleOverrideGuard {
  node_id: usize,
}

impl Drop for StyleOverrideGuard {
  fn drop(&mut self) {
    STYLE_OVERRIDES.with(|stack| {
      let mut stack = stack.borrow_mut();
      let popped = stack.pop();
      debug_assert!(
        popped
          .as_ref()
          .is_some_and(|(node_id, _)| *node_id == self.node_id),
        "style override stack corrupted (expected node_id={})",
        self.node_id
      );
    });
  }
}

/// Installs a temporary style override for the duration of the returned guard.
pub(crate) fn push_style_override(node_id: usize, style: Arc<ComputedStyle>) -> StyleOverrideGuard {
  STYLE_OVERRIDES.with(|stack| stack.borrow_mut().push((node_id, style)));
  StyleOverrideGuard { node_id }
}

/// Runs `f` with a temporary style override installed.
pub(crate) fn with_style_override<R>(
  node_id: usize,
  style: Arc<ComputedStyle>,
  f: impl FnOnce() -> R,
) -> R {
  let _guard = push_style_override(node_id, style);
  f()
}

/// Returns the currently active override style for `node_id`, if any.
pub(crate) fn style_override_for(node_id: usize) -> Option<Arc<ComputedStyle>> {
  STYLE_OVERRIDES.with(|stack| {
    stack
      .borrow()
      .iter()
      .rev()
      .find_map(|(id, style)| (*id == node_id).then(|| style.clone()))
  })
}

/// Returns true when `node_id` currently has an active style override.
pub(crate) fn has_style_override(node_id: usize) -> bool {
  STYLE_OVERRIDES.with(|stack| stack.borrow().iter().rev().any(|(id, _)| *id == node_id))
}
