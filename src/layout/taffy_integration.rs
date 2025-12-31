//! Instrumentation hooks for Taffy-backed layout
//!
//! This module provides lightweight counters that track when the vendored
//! Taffy engine is invoked. The counters are compiled only in debug/test
//! builds to avoid any release overhead. They backstop the invariant that
//! Taffy must only be used for flex/grid layout, never for tables.

use crate::geometry::Size;
use crate::layout::constraints::AvailableSpace as CrateAvailableSpace;
use crate::layout::constraints::LayoutConstraints;
#[cfg(any(test, debug_assertions))]
use std::cell::RefCell;
use std::collections::{HashMap, VecDeque};
use std::hash::{Hash, Hasher};
use std::sync::{Arc, Mutex};
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

#[cfg(any(test, debug_assertions))]
thread_local! {
  static COUNTERS: RefCell<TaffyUsageCounters> = RefCell::new(TaffyUsageCounters::default());
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

#[inline]
pub(crate) fn record_taffy_node_cache_hit(_kind: TaffyAdapterKind, count: u64) {
  #[cfg(any(test, debug_assertions))]
  COUNTERS.with(|counts| {
    let mut counts = counts.borrow_mut();
    match _kind {
      TaffyAdapterKind::Flex => counts.flex_nodes_reused += count,
      TaffyAdapterKind::Grid => counts.grid_nodes_reused += count,
    }
  });
}

#[inline]
pub(crate) fn record_taffy_node_cache_miss(_kind: TaffyAdapterKind, count: u64) {
  #[cfg(any(test, debug_assertions))]
  COUNTERS.with(|counts| {
    let mut counts = counts.borrow_mut();
    match _kind {
      TaffyAdapterKind::Flex => counts.flex_nodes_built += count,
      TaffyAdapterKind::Grid => counts.grid_nodes_built += count,
    }
  });
}

#[inline]
pub(crate) fn record_taffy_style_cache_hit(_kind: TaffyAdapterKind, count: u64) {
  #[cfg(any(test, debug_assertions))]
  COUNTERS.with(|counts| {
    let mut counts = counts.borrow_mut();
    match _kind {
      TaffyAdapterKind::Flex => counts.flex_style_cache_hits += count,
      TaffyAdapterKind::Grid => counts.grid_style_cache_hits += count,
    }
  });
}

#[inline]
pub(crate) fn record_taffy_style_cache_miss(_kind: TaffyAdapterKind, count: u64) {
  #[cfg(any(test, debug_assertions))]
  COUNTERS.with(|counts| {
    let mut counts = counts.borrow_mut();
    match _kind {
      TaffyAdapterKind::Flex => counts.flex_style_cache_misses += count,
      TaffyAdapterKind::Grid => counts.grid_style_cache_misses += count,
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
  let mut h = std::collections::hash_map::DefaultHasher::new();
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
  templates: HashMap<TaffyNodeCacheKey, Arc<CachedTaffyTemplate>>,
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

/// Stores Taffy style templates keyed by container and constraint fingerprints.
pub(crate) struct TaffyNodeCache {
  shards: Box<[TaffyNodeCacheShard]>,
  shard_mask: usize,
}

pub(crate) const DEFAULT_TAFFY_CACHE_LIMIT: usize = 512;

impl TaffyNodeCache {
  pub(crate) fn new(capacity: usize) -> Self {
    let capacity = capacity.max(1);
    let available = std::thread::available_parallelism()
      .map(|threads| threads.get())
      .unwrap_or(1);
    let desired = available.next_power_of_two().clamp(1, 64);
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
    let shards: Vec<TaffyNodeCacheShard> = (0..shard_count)
      .map(|idx| {
        let cap = per_shard + usize::from(idx < remainder);
        TaffyNodeCacheShard::new(cap)
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
    let mut h = std::collections::hash_map::DefaultHasher::new();
    key.hash(&mut h);
    (h.finish() as usize) & self.shard_mask
  }

  pub(crate) fn clear(&self) {
    for shard in self.shards.iter() {
      if let Ok(mut guard) = shard.inner.lock() {
        guard.templates.clear();
        guard.order.clear();
      }
    }
  }

  pub(crate) fn get(&self, key: &TaffyNodeCacheKey) -> Option<Arc<CachedTaffyTemplate>> {
    let shard = self.shards.get(self.shard_index(key))?;
    shard
      .inner
      .lock()
      .ok()
      .and_then(|guard| guard.templates.get(key).cloned())
  }

  pub(crate) fn insert(&self, key: TaffyNodeCacheKey, template: Arc<CachedTaffyTemplate>) {
    let shard_idx = self.shard_index(&key);
    let Some(shard) = self.shards.get(shard_idx) else {
      return;
    };
    if let Ok(mut guard) = shard.inner.lock() {
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
    assert!(cache.shards.len() <= 64);
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
}
