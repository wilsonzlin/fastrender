use crate::geometry::Size;
use crate::layout::flex_profile::{self, CacheKind};
use crate::tree::fragment_tree::FragmentNode;
use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::sync::RwLock;

pub(crate) type FlexCacheKey = (Option<u32>, Option<u32>);
pub(crate) type FlexCacheEntry = HashMap<FlexCacheKey, (Size, Arc<FragmentNode>)>;

const DEFAULT_SHARD_COUNT: usize = 64;

#[derive(Default)]
struct FlexCacheShard {
  map: RwLock<HashMap<u64, FlexCacheEntry>>,
  hits: AtomicU64,
  misses: AtomicU64,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub(crate) struct ShardStats {
  pub hits: u64,
  pub misses: u64,
}

pub(crate) struct ShardedFlexCache {
  kind: CacheKind,
  shards: Vec<FlexCacheShard>,
}

impl ShardedFlexCache {
  pub(crate) fn new_measure() -> Self {
    Self::with_shard_count(CacheKind::Measure, DEFAULT_SHARD_COUNT)
  }

  pub(crate) fn new_layout() -> Self {
    Self::with_shard_count(CacheKind::Layout, DEFAULT_SHARD_COUNT)
  }

  pub(crate) fn with_shard_count(kind: CacheKind, shard_count: usize) -> Self {
    let shards = shard_count.max(1);
    flex_profile::ensure_cache_shards(kind, shards);
    Self {
      kind,
      shards: (0..shards).map(|_| FlexCacheShard::default()).collect(),
    }
  }

  #[inline]
  fn shard_index(&self, node_key: u64) -> usize {
    // node_key is already a hash; mix the upper and lower bits to spread across shards.
    let mixed = node_key ^ (node_key >> 32);
    (mixed as usize) % self.shards.len()
  }

  pub(crate) fn clear(&self) {
    for shard in &self.shards {
      if let Ok(mut map) = shard.map.write() {
        map.clear();
      }
      shard.hits.store(0, Ordering::Relaxed);
      shard.misses.store(0, Ordering::Relaxed);
    }
  }

  fn record_lookup(&self, shard_idx: usize, hit: bool) {
    if !flex_profile::flex_profile_enabled() {
      return;
    }
    if let Some(shard) = self.shards.get(shard_idx) {
      if hit {
        shard.hits.fetch_add(1, Ordering::Relaxed);
      } else {
        shard.misses.fetch_add(1, Ordering::Relaxed);
      }
    }
    flex_profile::record_cache_shard_lookup(self.kind, shard_idx, hit);
  }

  pub(crate) fn get(&self, node_key: u64, key: &FlexCacheKey) -> Option<(Size, Arc<FragmentNode>)> {
    let shard_idx = self.shard_index(node_key);
    let result = self.shards.get(shard_idx).and_then(|shard| {
      shard
        .map
        .read()
        .ok()
        .and_then(|map| map.get(&node_key).and_then(|entry| entry.get(key)).cloned())
    });
    self.record_lookup(shard_idx, result.is_some());
    result
  }

  pub(crate) fn find_fragment(
    &self,
    node_key: u64,
    target_size: Size,
  ) -> Option<(Size, Arc<FragmentNode>)> {
    let shard_idx = self.shard_index(node_key);
    let result = self.shards.get(shard_idx).and_then(|shard| {
      shard
        .map
        .read()
        .ok()
        .and_then(|map| map.get(&node_key))
        .and_then(|entry| find_cached_fragment(entry, target_size))
    });
    self.record_lookup(shard_idx, result.is_some());
    result
  }

  pub(crate) fn insert(
    &self,
    node_key: u64,
    key: FlexCacheKey,
    value: (Size, Arc<FragmentNode>),
    per_node_cap: usize,
  ) -> bool {
    let shard_idx = self.shard_index(node_key);
    let Some(shard) = self.shards.get(shard_idx) else {
      return false;
    };
    let mut map = match shard.map.write() {
      Ok(guard) => guard,
      Err(poisoned) => poisoned.into_inner(),
    };
    let entry = map.entry(node_key).or_default();
    if entry.contains_key(&key) {
      return false;
    }
    if entry.len() >= per_node_cap {
      if let Some(first_key) = entry.keys().next().copied() {
        entry.remove(&first_key);
      }
    }
    entry.insert(key, value);
    true
  }

  pub(crate) fn shard_stats(&self) -> Vec<ShardStats> {
    self
      .shards
      .iter()
      .map(|shard| ShardStats {
        hits: shard.hits.load(Ordering::Relaxed),
        misses: shard.misses.load(Ordering::Relaxed),
      })
      .collect()
  }
}

pub(crate) fn cache_tolerances(target_size: Size) -> (f32, f32) {
  let band = |v: f32| -> f32 {
    if v > 4096.0 {
      32.0
    } else if v > 2048.0 {
      16.0
    } else if v > 1024.0 {
      8.0
    } else if v > 512.0 {
      6.0
    } else {
      4.0
    }
  };

  let min_eps = 1.0;
  let eps_w = f32::max(
    f32::max(band(target_size.width), band(target_size.height) * 0.75),
    min_eps,
  );
  let eps_h = f32::max(
    f32::max(band(target_size.height), band(target_size.width) * 0.5),
    min_eps,
  );

  (eps_w, eps_h)
}

pub(crate) fn find_cached_fragment(
  cache: &FlexCacheEntry,
  target_size: Size,
) -> Option<(Size, Arc<FragmentNode>)> {
  let mut best: Option<(Size, Arc<FragmentNode>)> = None;
  let mut best_score = f32::MAX;
  let (eps_w, eps_h) = cache_tolerances(target_size);
  for (size, frag) in cache.values() {
    if !size.width.is_finite() || !size.height.is_finite() {
      continue;
    }
    let dw = (size.width - target_size.width).abs();
    let dh = (size.height - target_size.height).abs();
    if dw <= eps_w && dh <= eps_h {
      let score = dw + dh;
      if score < best_score {
        best_score = score;
        best = Some((*size, frag.clone()));
      }
    }
  }
  best
}

pub(crate) fn find_layout_cache_fragment(
  cache: &FlexCacheEntry,
  target_size: Size,
) -> Option<(Size, Arc<FragmentNode>)> {
  find_cached_fragment(cache, target_size)
}
