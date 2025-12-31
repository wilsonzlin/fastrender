//! Instrumentation hooks for Taffy-backed layout
//!
//! This module provides lightweight counters that track when the vendored
//! Taffy engine is invoked. Counters are available in release builds but are
//! disabled by default; call sites can enable them at runtime when collecting
//! render statistics.
//!
//! When enabled, the counters can be used to attribute slow flex/grid layouts by recording:
//! - wall time spent inside `taffy::TaffyTree::compute_layout_with_measure(_and_cancel)`, and
//! - how many times the measure callback is invoked,
//! split by Flex vs Grid.

use crate::geometry::Size;
use crate::layout::constraints::AvailableSpace as CrateAvailableSpace;
use crate::layout::constraints::LayoutConstraints;
use rustc_hash::{FxHashMap, FxHasher};
use std::collections::VecDeque;
use std::hash::{Hash, Hasher};
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Duration;
use taffy::style::Style as TaffyStyle;

/// Count of Taffy layout invocations per adapter.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct TaffyUsageCounters {
  /// Number of flex layout executions routed through Taffy.
  pub flex: u64,
  /// Number of grid layout executions routed through Taffy.
  pub grid: u64,
  /// Number of Taffy nodes whose styles were freshly built for flex containers.
  pub flex_nodes_built: u64,
  /// Number of Taffy nodes whose cached styles were reused for flex containers.
  pub flex_nodes_reused: u64,
  /// Number of Taffy nodes whose styles were freshly built for grid containers.
  pub grid_nodes_built: u64,
  /// Number of Taffy nodes whose cached styles were reused for grid containers.
  pub grid_nodes_reused: u64,
  /// Flex style conversions avoided via cache hits.
  pub flex_style_cache_hits: u64,
  /// Flex style conversions executed because of cache misses.
  pub flex_style_cache_misses: u64,
  /// Grid style conversions avoided via cache hits.
  pub grid_style_cache_hits: u64,
  /// Grid style conversions executed because of cache misses.
  pub grid_style_cache_misses: u64,
}

/// Which adapter recorded a Taffy layout pass.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum TaffyAdapterKind {
  Flex,
  Grid,
}

/// Taffy perf counters captured during a render when diagnostics are enabled.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub(crate) struct TaffyPerfCounters {
  pub flex_compute_ns: u64,
  pub grid_compute_ns: u64,
  pub flex_measure_calls: u64,
  pub grid_measure_calls: u64,
}

static TAFFY_PERF_ENABLED: AtomicBool = AtomicBool::new(false);
static TAFFY_PERF_RESET_REQUESTED: AtomicBool = AtomicBool::new(false);
static TAFFY_FLEX_COMPUTE_NS: AtomicU64 = AtomicU64::new(0);
static TAFFY_GRID_COMPUTE_NS: AtomicU64 = AtomicU64::new(0);
static TAFFY_FLEX_MEASURE_CALLS: AtomicU64 = AtomicU64::new(0);
static TAFFY_GRID_MEASURE_CALLS: AtomicU64 = AtomicU64::new(0);

/// Enables Taffy perf counters for the duration of a diagnostics-enabled render.
///
/// The counters are global and not intended for per-thread attribution.
pub(crate) struct TaffyPerfCountersGuard {
  previous: bool,
}

impl TaffyPerfCountersGuard {
  pub(crate) fn new() -> Self {
    let previous = TAFFY_PERF_ENABLED.swap(true, Ordering::Relaxed);
    if !previous {
      TAFFY_PERF_RESET_REQUESTED.store(true, Ordering::Relaxed);
    }
    Self { previous }
  }
}

impl Drop for TaffyPerfCountersGuard {
  fn drop(&mut self) {
    TAFFY_PERF_ENABLED.store(self.previous, Ordering::Relaxed);
  }
}

#[inline]
pub(crate) fn taffy_perf_enabled() -> bool {
  TAFFY_PERF_ENABLED.load(Ordering::Relaxed)
}

/// Resets Taffy perf counters at the start of a layout run when requested.
///
/// Layout runs can occur outside of diagnostics-enabled renders; this should be cheap to call in
/// hot paths by doing nothing unless perf counters are enabled for an active diagnostics capture.
#[inline]
pub(crate) fn reset_taffy_perf_counters() {
  if !taffy_perf_enabled() {
    return;
  }
  if !TAFFY_PERF_RESET_REQUESTED.swap(false, Ordering::Relaxed) {
    return;
  }
  TAFFY_FLEX_COMPUTE_NS.store(0, Ordering::Relaxed);
  TAFFY_GRID_COMPUTE_NS.store(0, Ordering::Relaxed);
  TAFFY_FLEX_MEASURE_CALLS.store(0, Ordering::Relaxed);
  TAFFY_GRID_MEASURE_CALLS.store(0, Ordering::Relaxed);
}

#[inline]
pub(crate) fn record_taffy_compute(kind: TaffyAdapterKind, duration: Duration) {
  if !taffy_perf_enabled() {
    return;
  }
  let ns = duration.as_nanos().min(u64::MAX as u128) as u64;
  match kind {
    TaffyAdapterKind::Flex => {
      TAFFY_FLEX_COMPUTE_NS.fetch_add(ns, Ordering::Relaxed);
    }
    TaffyAdapterKind::Grid => {
      TAFFY_GRID_COMPUTE_NS.fetch_add(ns, Ordering::Relaxed);
    }
  }
}

#[inline]
pub(crate) fn record_taffy_measure_call(kind: TaffyAdapterKind) {
  if !taffy_perf_enabled() {
    return;
  }
  match kind {
    TaffyAdapterKind::Flex => {
      TAFFY_FLEX_MEASURE_CALLS.fetch_add(1, Ordering::Relaxed);
    }
    TaffyAdapterKind::Grid => {
      TAFFY_GRID_MEASURE_CALLS.fetch_add(1, Ordering::Relaxed);
    }
  }
}

#[inline]
pub(crate) fn taffy_perf_counters() -> TaffyPerfCounters {
  TaffyPerfCounters {
    flex_compute_ns: TAFFY_FLEX_COMPUTE_NS.load(Ordering::Relaxed),
    grid_compute_ns: TAFFY_GRID_COMPUTE_NS.load(Ordering::Relaxed),
    flex_measure_calls: TAFFY_FLEX_MEASURE_CALLS.load(Ordering::Relaxed),
    grid_measure_calls: TAFFY_GRID_MEASURE_CALLS.load(Ordering::Relaxed),
  }
}

static TAFFY_COUNTERS_ENABLED: AtomicBool = AtomicBool::new(false);

struct TaffyUsageAtomicCounters {
  flex: AtomicU64,
  grid: AtomicU64,
  flex_nodes_built: AtomicU64,
  flex_nodes_reused: AtomicU64,
  grid_nodes_built: AtomicU64,
  grid_nodes_reused: AtomicU64,
  flex_style_cache_hits: AtomicU64,
  flex_style_cache_misses: AtomicU64,
  grid_style_cache_hits: AtomicU64,
  grid_style_cache_misses: AtomicU64,
}

impl TaffyUsageAtomicCounters {
  const fn new() -> Self {
    Self {
      flex: AtomicU64::new(0),
      grid: AtomicU64::new(0),
      flex_nodes_built: AtomicU64::new(0),
      flex_nodes_reused: AtomicU64::new(0),
      grid_nodes_built: AtomicU64::new(0),
      grid_nodes_reused: AtomicU64::new(0),
      flex_style_cache_hits: AtomicU64::new(0),
      flex_style_cache_misses: AtomicU64::new(0),
      grid_style_cache_hits: AtomicU64::new(0),
      grid_style_cache_misses: AtomicU64::new(0),
    }
  }

  fn reset(&self) {
    self.flex.store(0, Ordering::Relaxed);
    self.grid.store(0, Ordering::Relaxed);
    self.flex_nodes_built.store(0, Ordering::Relaxed);
    self.flex_nodes_reused.store(0, Ordering::Relaxed);
    self.grid_nodes_built.store(0, Ordering::Relaxed);
    self.grid_nodes_reused.store(0, Ordering::Relaxed);
    self.flex_style_cache_hits.store(0, Ordering::Relaxed);
    self.flex_style_cache_misses.store(0, Ordering::Relaxed);
    self.grid_style_cache_hits.store(0, Ordering::Relaxed);
    self.grid_style_cache_misses.store(0, Ordering::Relaxed);
  }

  fn snapshot(&self) -> TaffyUsageCounters {
    TaffyUsageCounters {
      flex: self.flex.load(Ordering::Relaxed),
      grid: self.grid.load(Ordering::Relaxed),
      flex_nodes_built: self.flex_nodes_built.load(Ordering::Relaxed),
      flex_nodes_reused: self.flex_nodes_reused.load(Ordering::Relaxed),
      grid_nodes_built: self.grid_nodes_built.load(Ordering::Relaxed),
      grid_nodes_reused: self.grid_nodes_reused.load(Ordering::Relaxed),
      flex_style_cache_hits: self.flex_style_cache_hits.load(Ordering::Relaxed),
      flex_style_cache_misses: self.flex_style_cache_misses.load(Ordering::Relaxed),
      grid_style_cache_hits: self.grid_style_cache_hits.load(Ordering::Relaxed),
      grid_style_cache_misses: self.grid_style_cache_misses.load(Ordering::Relaxed),
    }
  }
}

static TAFFY_COUNTERS: TaffyUsageAtomicCounters = TaffyUsageAtomicCounters::new();

pub struct TaffyUsageCountersGuard {
  previous: bool,
}

impl Drop for TaffyUsageCountersGuard {
  fn drop(&mut self) {
    TAFFY_COUNTERS_ENABLED.store(self.previous, Ordering::Relaxed);
  }
}

pub fn enable_taffy_counters(enabled: bool) -> TaffyUsageCountersGuard {
  let previous = TAFFY_COUNTERS_ENABLED.swap(enabled, Ordering::Relaxed);
  TaffyUsageCountersGuard { previous }
}

pub fn set_taffy_counters_enabled(enabled: bool) {
  TAFFY_COUNTERS_ENABLED.store(enabled, Ordering::Relaxed);
}

#[inline]
fn taffy_counters_enabled() -> bool {
  TAFFY_COUNTERS_ENABLED.load(Ordering::Relaxed)
}

/// Records that a Taffy layout was executed.
#[inline]
pub(crate) fn record_taffy_invocation(kind: TaffyAdapterKind) {
  if !taffy_counters_enabled() {
    return;
  }
  match kind {
    TaffyAdapterKind::Flex => {
      TAFFY_COUNTERS.flex.fetch_add(1, Ordering::Relaxed);
    }
    TaffyAdapterKind::Grid => {
      TAFFY_COUNTERS.grid.fetch_add(1, Ordering::Relaxed);
    }
  }
}

#[inline]
pub(crate) fn record_taffy_node_cache_hit(kind: TaffyAdapterKind, count: u64) {
  if !taffy_counters_enabled() || count == 0 {
    return;
  }
  match kind {
    TaffyAdapterKind::Flex => {
      TAFFY_COUNTERS
        .flex_nodes_reused
        .fetch_add(count, Ordering::Relaxed);
    }
    TaffyAdapterKind::Grid => {
      TAFFY_COUNTERS
        .grid_nodes_reused
        .fetch_add(count, Ordering::Relaxed);
    }
  }
}

#[inline]
pub(crate) fn record_taffy_node_cache_miss(kind: TaffyAdapterKind, count: u64) {
  if !taffy_counters_enabled() || count == 0 {
    return;
  }
  match kind {
    TaffyAdapterKind::Flex => {
      TAFFY_COUNTERS
        .flex_nodes_built
        .fetch_add(count, Ordering::Relaxed);
    }
    TaffyAdapterKind::Grid => {
      TAFFY_COUNTERS
        .grid_nodes_built
        .fetch_add(count, Ordering::Relaxed);
    }
  }
}

#[inline]
pub(crate) fn record_taffy_style_cache_hit(kind: TaffyAdapterKind, count: u64) {
  if !taffy_counters_enabled() || count == 0 {
    return;
  }
  match kind {
    TaffyAdapterKind::Flex => {
      TAFFY_COUNTERS
        .flex_style_cache_hits
        .fetch_add(count, Ordering::Relaxed);
    }
    TaffyAdapterKind::Grid => {
      TAFFY_COUNTERS
        .grid_style_cache_hits
        .fetch_add(count, Ordering::Relaxed);
    }
  }
}

#[inline]
pub(crate) fn record_taffy_style_cache_miss(kind: TaffyAdapterKind, count: u64) {
  if !taffy_counters_enabled() || count == 0 {
    return;
  }
  match kind {
    TaffyAdapterKind::Flex => {
      TAFFY_COUNTERS
        .flex_style_cache_misses
        .fetch_add(count, Ordering::Relaxed);
    }
    TaffyAdapterKind::Grid => {
      TAFFY_COUNTERS
        .grid_style_cache_misses
        .fetch_add(count, Ordering::Relaxed);
    }
  }
}

/// Resets counters for the current process.
#[inline]
pub fn reset_taffy_counters() {
  TAFFY_COUNTERS.reset();
}

/// Returns a snapshot of the current counters for the current process.
#[inline]
pub fn taffy_counters() -> TaffyUsageCounters {
  TAFFY_COUNTERS.snapshot()
}

/// Stride used when wiring FastRender's render deadline into Taffy's cooperative abort checks.
///
/// This amortizes deadline checks while still ensuring long-running grid/flex layout can be aborted
/// within a reasonable time budget.
pub(crate) const TAFFY_ABORT_CHECK_STRIDE: usize = 256;

/// Convenience accessor for the total number of Taffy invocations observed.
#[inline]
pub fn total_taffy_invocations() -> u64 {
  let counts = taffy_counters();
  counts.flex + counts.grid
}

fn quantize(value: f32) -> f32 {
  let abs = value.abs();
  if abs > 4096.0 {
    (value / 64.0).round() * 64.0
  } else if abs > 2048.0 {
    (value / 32.0).round() * 32.0
  } else if abs > 1024.0 {
    (value / 16.0).round() * 16.0
  } else if abs > 512.0 {
    (value / 8.0).round() * 8.0
  } else if abs > 256.0 {
    (value / 4.0).round() * 4.0
  } else {
    (value / 2.0).round() * 2.0
  }
}

fn map_space(space: CrateAvailableSpace, viewport: f32, neg_offset: f32) -> Option<u32> {
  match space {
    CrateAvailableSpace::Definite(v) => Some(quantize(v).to_bits()),
    CrateAvailableSpace::MinContent => Some(quantize(-viewport - neg_offset).to_bits()),
    CrateAvailableSpace::MaxContent => Some(quantize(-viewport - (neg_offset + 1.0)).to_bits()),
    CrateAvailableSpace::Indefinite => None,
  }
}

/// Quantized key for the available space used to build a Taffy tree.
#[inline]
pub(crate) fn taffy_constraint_key(
  constraints: &LayoutConstraints,
  viewport: Size,
) -> (Option<u32>, Option<u32>) {
  (
    map_space(constraints.available_width, viewport.width.max(0.0), 1.0),
    map_space(constraints.available_height, viewport.height.max(0.0), 1.0),
  )
}

fn viewport_hash(viewport: Size) -> u64 {
  let mut h = FxHasher::default();
  quantize(viewport.width).to_bits().hash(&mut h);
  quantize(viewport.height).to_bits().hash(&mut h);
  h.finish()
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub(crate) struct TaffyNodeCacheKey {
  adapter: TaffyAdapterKind,
  root_id: usize,
  root_style: usize,
  child_fingerprint: u64,
  constraints: (Option<u32>, Option<u32>),
  intrinsic_epoch: usize,
  viewport_hash: u64,
}

impl TaffyNodeCacheKey {
  pub(crate) fn new(
    adapter: TaffyAdapterKind,
    root_id: usize,
    root_style: usize,
    child_fingerprint: u64,
    constraints: (Option<u32>, Option<u32>),
    intrinsic_epoch: usize,
    viewport: Size,
  ) -> Self {
    Self {
      adapter,
      root_id,
      root_style,
      child_fingerprint,
      constraints,
      intrinsic_epoch,
      viewport_hash: viewport_hash(viewport),
    }
  }
}

#[derive(Clone)]
pub(crate) struct SendSyncStyle(pub TaffyStyle);

// Taffy's Style uses `DefaultCheapStr` internally, which resolves to owned `String` in std builds.
// The raw pointer stored alongside the string to enable cheap cloning is only derived from that
// owned storage, so it is safe to share between threads.
unsafe impl Send for SendSyncStyle {}
unsafe impl Sync for SendSyncStyle {}

impl std::ops::Deref for SendSyncStyle {
  type Target = TaffyStyle;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

#[derive(Clone)]
pub(crate) struct CachedTaffyTemplate {
  pub root_style: Arc<SendSyncStyle>,
  pub child_styles: Vec<Arc<SendSyncStyle>>,
}

impl CachedTaffyTemplate {
  pub(crate) fn node_count(&self) -> u64 {
    (self.child_styles.len() + 1) as u64
  }
}

#[derive(Default)]
struct TaffyNodeCacheInner {
  templates: FxHashMap<TaffyNodeCacheKey, Arc<CachedTaffyTemplate>>,
  order: VecDeque<TaffyNodeCacheKey>,
}

struct TaffyNodeCacheShard {
  capacity: usize,
  inner: Mutex<TaffyNodeCacheInner>,
}

impl TaffyNodeCacheShard {
  fn new(capacity: usize) -> Self {
    Self {
      capacity: capacity.max(1),
      inner: Mutex::new(TaffyNodeCacheInner::default()),
    }
  }
}

/// Cache-line padded wrapper used to keep shard locks on distinct cache lines.
///
/// Without padding, adjacent shard locks frequently share cache lines and can exhibit severe
/// false-sharing under rayon fan-out (threads contending even when they touch different shards).
#[repr(align(64))]
struct CacheLinePadded<T>(T);

impl<T> std::ops::Deref for CacheLinePadded<T> {
  type Target = T;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

/// Stores Taffy style templates keyed by container and constraint fingerprints.
pub(crate) struct TaffyNodeCache {
  shards: Box<[CacheLinePadded<TaffyNodeCacheShard>]>,
  shard_mask: usize,
}

pub(crate) const DEFAULT_TAFFY_CACHE_LIMIT: usize = 512;

impl TaffyNodeCache {
  pub(crate) fn new(capacity: usize) -> Self {
    let capacity = capacity.max(1);
    let available = std::thread::available_parallelism()
      .map(|threads| threads.get())
      .unwrap_or(1);
    let desired = available.next_power_of_two();
    let max_shards = capacity.min(desired).max(1);
    let next_pow2 = max_shards.next_power_of_two();
    let shard_count = if next_pow2 == max_shards {
      next_pow2
    } else {
      next_pow2 >> 1
    }
    .max(1);

    let per_shard = capacity / shard_count;
    let remainder = capacity % shard_count;
    let shards: Vec<CacheLinePadded<TaffyNodeCacheShard>> = (0..shard_count)
      .map(|idx| {
        let cap = per_shard + usize::from(idx < remainder);
        CacheLinePadded(TaffyNodeCacheShard::new(cap))
      })
      .collect();

    Self {
      shards: shards.into_boxed_slice(),
      shard_mask: shard_count - 1,
    }
  }

  #[inline]
  fn shard_index(&self, key: &TaffyNodeCacheKey) -> usize {
    debug_assert!(!self.shards.is_empty());
    let mut h = FxHasher::default();
    key.hash(&mut h);
    (h.finish() as usize) & self.shard_mask
  }

  pub(crate) fn clear(&self) {
    for shard in self.shards.iter() {
      let mut guard = match shard.inner.lock() {
        Ok(guard) => guard,
        Err(poisoned) => poisoned.into_inner(),
      };
      guard.templates.clear();
      guard.order.clear();
    }
  }

  pub(crate) fn get(&self, key: &TaffyNodeCacheKey) -> Option<Arc<CachedTaffyTemplate>> {
    let shard = self.shards.get(self.shard_index(key))?;
    let guard = match shard.inner.lock() {
      Ok(guard) => guard,
      Err(poisoned) => poisoned.into_inner(),
    };
    guard.templates.get(key).cloned()
  }

  pub(crate) fn insert(&self, key: TaffyNodeCacheKey, template: Arc<CachedTaffyTemplate>) {
    let shard_idx = self.shard_index(&key);
    let Some(shard) = self.shards.get(shard_idx) else {
      return;
    };
    let mut guard = match shard.inner.lock() {
      Ok(guard) => guard,
      Err(poisoned) => poisoned.into_inner(),
    };
    if let Some(pos) = guard.order.iter().position(|existing| existing == &key) {
      guard.order.remove(pos);
    }
    guard.order.push_back(key);
    guard.templates.insert(key, template);
    while guard.order.len() > shard.capacity {
      if let Some(evicted) = guard.order.pop_front() {
        guard.templates.remove(&evicted);
      }
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  fn template() -> Arc<CachedTaffyTemplate> {
    Arc::new(CachedTaffyTemplate {
      root_style: Arc::new(SendSyncStyle(TaffyStyle::default())),
      child_styles: Vec::new(),
    })
  }

  #[test]
  fn taffy_node_cache_shard_capacities_sum_to_configured_capacity() {
    let capacity = 7;
    let cache = TaffyNodeCache::new(capacity);
    assert!(cache.shards.len().is_power_of_two());
    assert!(cache.shards.len() <= capacity);
    assert_eq!(cache.shard_mask + 1, cache.shards.len());

    let total_capacity: usize = cache.shards.iter().map(|shard| shard.capacity).sum();
    assert_eq!(total_capacity, capacity);
  }

  #[test]
  fn taffy_node_cache_is_bounded_by_capacity() {
    let cache = TaffyNodeCache::new(1);
    let viewport = Size::new(800.0, 600.0);
    let tpl = template();

    let key1 = TaffyNodeCacheKey::new(
      TaffyAdapterKind::Flex,
      1,
      1,
      0,
      (Some(1), Some(1)),
      1,
      viewport,
    );
    let key2 = TaffyNodeCacheKey::new(
      TaffyAdapterKind::Flex,
      2,
      1,
      0,
      (Some(1), Some(1)),
      1,
      viewport,
    );

    cache.insert(key1, tpl.clone());
    cache.insert(key2, tpl.clone());

    assert!(cache.get(&key1).is_none());
    assert!(cache.get(&key2).is_some());

    let entry_count: usize = cache
      .shards
      .iter()
      .map(|shard| shard.inner.lock().unwrap().templates.len())
      .sum();
    assert_eq!(entry_count, 1);
  }

  #[test]
  fn taffy_node_cache_supports_parallel_reads() {
    // Ensure shard-local eviction cannot remove entries during this test. With per-shard capacity
    // distribution, a small total capacity can still evict entries if multiple keys hash into the
    // same shard. Using a generous capacity makes the test deterministic across CPU counts.
    let cache = Arc::new(TaffyNodeCache::new(8192));
    let viewport = Size::new(800.0, 600.0);

    let mut expected = Vec::new();
    for idx in 0..32usize {
      let tpl = template();
      let key = TaffyNodeCacheKey::new(
        TaffyAdapterKind::Flex,
        idx + 1,
        1,
        idx as u64,
        (Some(1), Some(1)),
        1,
        viewport,
      );
      cache.insert(key, tpl.clone());
      expected.push((key, tpl));
    }

    std::thread::scope(|scope| {
      for _ in 0..8 {
        let cache = cache.clone();
        let expected = &expected;
        scope.spawn(move || {
          for _ in 0..64 {
            for (key, tpl) in expected.iter() {
              let got = cache.get(key).expect("template present");
              assert!(Arc::ptr_eq(&got, tpl));
            }
          }
        });
      }
    });
  }
}
