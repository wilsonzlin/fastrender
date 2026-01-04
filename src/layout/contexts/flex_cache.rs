use crate::geometry::Size;
use crate::layout::flex_profile::{self, CacheKind};
use crate::tree::fragment_tree::FragmentNode;
use parking_lot::RwLock;
use rustc_hash::FxHashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;

pub(crate) type FlexCacheKey = (Option<u32>, Option<u32>);

#[derive(Clone)]
pub(crate) struct FlexCacheValue {
  /// The size returned to Taffy’s measure function (typically a content-box size).
  pub measured_size: Size,
  /// The corresponding used border-box size that Taffy will position (measured size + padding/border
  /// + any reserved scrollbar gutter).
  ///
  /// Note: this may intentionally diverge from `fragment.bounds.size` when we clamp runaway
  /// measurements. In those cases we still want to reuse the fragment tree, but need a stable size
  /// key that matches what Taffy used for layout.
  pub border_size: Size,
  pub fragment: Arc<FragmentNode>,
}

pub(crate) type FlexCacheEntry = FxHashMap<FlexCacheKey, FlexCacheValue>;

const DEFAULT_SHARD_COUNT: usize = 64;

#[derive(Default)]
struct FlexCacheShard {
  map: RwLock<FxHashMap<u64, FlexCacheEntry>>,
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
      shard.map.write().clear();
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

  pub(crate) fn get(&self, node_key: u64, key: &FlexCacheKey) -> Option<FlexCacheValue> {
    let shard_idx = self.shard_index(node_key);
    let result = self.shards.get(shard_idx).and_then(|shard| {
      shard
        .map
        .read()
        .get(&node_key)
        .and_then(|entry| entry.get(key))
        .cloned()
    });
    self.record_lookup(shard_idx, result.is_some());
    result
  }

  pub(crate) fn find_fragment(&self, node_key: u64, target_size: Size) -> Option<FlexCacheValue> {
    let shard_idx = self.shard_index(node_key);
    let result = self.shards.get(shard_idx).and_then(|shard| {
      let map = shard.map.read();
      let entry = map.get(&node_key)?;
      find_cached_fragment(entry, target_size)
    });
    self.record_lookup(shard_idx, result.is_some());
    result
  }

  /// Finds a cached fragment by comparing the stored *used border-box* size (`value.border_size`)
  /// against the provided target size.
  pub(crate) fn find_fragment_by_border_size(
    &self,
    node_key: u64,
    target_size: Size,
  ) -> Option<FlexCacheValue> {
    let shard_idx = self.shard_index(node_key);
    let result = self.shards.get(shard_idx).and_then(|shard| {
      let map = shard.map.read();
      let entry = map.get(&node_key)?;
      find_cached_fragment_by_border_size(entry, target_size)
    });
    self.record_lookup(shard_idx, result.is_some());
    result
  }

  pub(crate) fn insert(
    &self,
    node_key: u64,
    key: FlexCacheKey,
    value: FlexCacheValue,
    per_node_cap: usize,
  ) -> bool {
    let per_node_cap = per_node_cap.max(1);
    let shard_idx = self.shard_index(node_key);
    let Some(shard) = self.shards.get(shard_idx) else {
      return false;
    };
    let mut map = shard.map.write();
    let entry = map.entry(node_key).or_default();
    if entry.contains_key(&key) {
      return false;
    }
    entry.insert(key, value);
    while entry.len() > per_node_cap {
      if let Some(max_key) = entry.keys().max().copied() {
        entry.remove(&max_key);
      } else {
        break;
      }
    }
    entry.contains_key(&key)
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
) -> Option<FlexCacheValue> {
  let mut best: Option<&FlexCacheValue> = None;
  let mut best_key: Option<FlexCacheKey> = None;
  let mut best_score = f32::MAX;
  let mut best_dw = f32::MAX;
  let mut best_dh = f32::MAX;
  let (eps_w, eps_h) = cache_tolerances(target_size);
  for (key, value) in cache.iter() {
    let size = value.measured_size;
    if !size.width.is_finite() || !size.height.is_finite() {
      continue;
    }
    let dw = (size.width - target_size.width).abs();
    let dh = (size.height - target_size.height).abs();
    if dw <= eps_w && dh <= eps_h {
      let score = dw + dh;
      let better = match best {
        None => true,
        Some(_) => match score.total_cmp(&best_score) {
          std::cmp::Ordering::Less => true,
          std::cmp::Ordering::Greater => false,
          std::cmp::Ordering::Equal => match dw.total_cmp(&best_dw) {
            std::cmp::Ordering::Less => true,
            std::cmp::Ordering::Greater => false,
            std::cmp::Ordering::Equal => match dh.total_cmp(&best_dh) {
              std::cmp::Ordering::Less => true,
              std::cmp::Ordering::Greater => false,
              std::cmp::Ordering::Equal => match best_key {
                None => true,
                Some(best_key) => *key < best_key,
              },
            },
          },
        },
      };
      if better {
        best_score = score;
        best_dw = dw;
        best_dh = dh;
        best_key = Some(*key);
        best = Some(value);
      }
    }
  }
  best.cloned()
}

pub(crate) fn find_layout_cache_fragment(
  cache: &FlexCacheEntry,
  target_size: Size,
) -> Option<FlexCacheValue> {
  find_cached_fragment(cache, target_size)
}

pub(crate) fn find_cached_fragment_by_border_size(
  cache: &FlexCacheEntry,
  target_size: Size,
) -> Option<FlexCacheValue> {
  let mut best: Option<&FlexCacheValue> = None;
  let mut best_key: Option<FlexCacheKey> = None;
  let mut best_score = f32::MAX;
  let mut best_dw = f32::MAX;
  let mut best_dh = f32::MAX;
  let (eps_w, eps_h) = cache_tolerances(target_size);
  for (key, value) in cache.iter() {
    let size = value.border_size;
    if !size.width.is_finite() || !size.height.is_finite() {
      continue;
    }
    let dw = (size.width - target_size.width).abs();
    let dh = (size.height - target_size.height).abs();
    if dw <= eps_w && dh <= eps_h {
      let score = dw + dh;
      let better = match best {
        None => true,
        Some(_) => match score.total_cmp(&best_score) {
          std::cmp::Ordering::Less => true,
          std::cmp::Ordering::Greater => false,
          std::cmp::Ordering::Equal => match dw.total_cmp(&best_dw) {
            std::cmp::Ordering::Less => true,
            std::cmp::Ordering::Greater => false,
            std::cmp::Ordering::Equal => match dh.total_cmp(&best_dh) {
              std::cmp::Ordering::Less => true,
              std::cmp::Ordering::Greater => false,
              std::cmp::Ordering::Equal => match best_key {
                None => true,
                Some(best_key) => *key < best_key,
              },
            },
          },
        },
      };
      if better {
        best_score = score;
        best_dw = dw;
        best_dh = dh;
        best_key = Some(*key);
        best = Some(value);
      }
    }
  }
  best.cloned()
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::geometry::Rect;
  use crate::tree::fragment_tree::FragmentContent;
  use crate::tree::fragment_tree::FragmentNode;
  use rustc_hash::FxHasher;
  use std::collections::BTreeMap;
  use std::hash::{Hash, Hasher};
  use std::sync::Arc;

  #[test]
  fn find_fragment_by_border_size_matches_used_border_box_size() {
    let cache = ShardedFlexCache::new_measure();
    let node_key = 123u64;
    let key: FlexCacheKey = (Some(1), None);

    let fragment_bounds = Rect::from_xywh(0.0, 0.0, 500.0, 300.0);
    let fragment = Arc::new(FragmentNode::new(
      fragment_bounds,
      FragmentContent::Block { box_id: None },
      vec![],
    ));

    // Simulate a measured entry where the stored size is the *content-box* size (smaller than the
    // fragment's border-box bounds). This mirrors flex/grid measurement, which returns a content
    // size to Taffy but stores the full fragment tree for later reuse, potentially clamping the
    // returned size without reflowing the fragment tree.
    let content_size = Size::new(100.0, 20.0);
    let used_border_size = Size::new(120.0, 40.0);
    assert!(cache.insert(
      node_key,
      key,
      FlexCacheValue {
        measured_size: content_size,
        border_size: used_border_size,
        fragment: Arc::clone(&fragment),
      },
      16
    ));

    // The legacy lookup matches on stored content-box size, so searching by border-box size should fail.
    assert!(cache.find_fragment(node_key, used_border_size).is_none());

    // The border-box lookup should find the fragment when searching by the used border size.
    let found = cache
      .find_fragment_by_border_size(node_key, used_border_size)
      .expect("expected border-box cache hit");
    assert_eq!(found.border_size, used_border_size);
    assert_eq!(found.fragment.bounds.size, fragment_bounds.size);
  }

  fn make_test_fragment(bounds: Rect) -> Arc<FragmentNode> {
    Arc::new(FragmentNode::new(
      bounds,
      FragmentContent::Block { box_id: None },
      vec![],
    ))
  }

  fn first_key_after_inserts(a: FlexCacheKey, b: FlexCacheKey) -> FlexCacheKey {
    let mut map: FxHashMap<FlexCacheKey, u8> = FxHashMap::default();
    map.insert(a, 0);
    map.insert(b, 0);
    *map.keys().next().expect("expected non-empty map")
  }

  fn find_insertion_order_sensitive_flex_cache_keys(
    mask_bits: u32,
  ) -> (FlexCacheKey, FlexCacheKey) {
    let mask = (1u64 << mask_bits) - 1;
    let mut seen: BTreeMap<u64, Vec<FlexCacheKey>> = BTreeMap::new();
    let max = 1u32 << (mask_bits + 3);
    for i in 0..max {
      let key: FlexCacheKey = (Some(i), Some(0));
      let mut hasher = FxHasher::default();
      key.hash(&mut hasher);
      let slot = hasher.finish() & mask;
      if let Some(keys) = seen.get(&slot) {
        for &prev in keys.iter() {
          if first_key_after_inserts(prev, key) != first_key_after_inserts(key, prev) {
            return if prev <= key {
              (prev, key)
            } else {
              (key, prev)
            };
          }
        }
      }
      seen.entry(slot).or_default().push(key);
    }

    panic!("failed to find insertion-order-sensitive flex cache keys");
  }

  #[test]
  fn find_fragment_tie_break_is_deterministic() {
    let (key_small, key_large) = find_insertion_order_sensitive_flex_cache_keys(10);

    let cache_a = ShardedFlexCache::with_shard_count(CacheKind::Measure, 1);
    let cache_b = ShardedFlexCache::with_shard_count(CacheKind::Measure, 1);
    let node_key = 999u64;

    let target_size = Size::new(100.0, 100.0);
    let value_small = FlexCacheValue {
      measured_size: Size::new(99.0, 100.0),
      border_size: Size::new(99.0, 100.0),
      fragment: make_test_fragment(Rect::from_xywh(0.0, 0.0, 99.0, 100.0)),
    };
    let value_large = FlexCacheValue {
      measured_size: Size::new(101.0, 100.0),
      border_size: Size::new(101.0, 100.0),
      fragment: make_test_fragment(Rect::from_xywh(0.0, 0.0, 101.0, 100.0)),
    };

    assert!(cache_a.insert(node_key, key_small, value_small.clone(), 16));
    assert!(cache_a.insert(node_key, key_large, value_large.clone(), 16));

    assert!(cache_b.insert(node_key, key_large, value_large, 16));
    assert!(cache_b.insert(node_key, key_small, value_small, 16));

    let found_a = cache_a
      .find_fragment(node_key, target_size)
      .expect("expected cache hit");
    let found_b = cache_b
      .find_fragment(node_key, target_size)
      .expect("expected cache hit");

    assert_eq!(found_a.measured_size, found_b.measured_size);
    assert_eq!(found_a.measured_size, Size::new(99.0, 100.0));
  }

  #[test]
  fn insert_eviction_is_deterministic() {
    let cache_a = ShardedFlexCache::with_shard_count(CacheKind::Measure, 1);
    let cache_b = ShardedFlexCache::with_shard_count(CacheKind::Measure, 1);
    let node_key = 456u64;
    let per_node_cap = 2;

    let (key_a, key_b) = find_insertion_order_sensitive_flex_cache_keys(10);
    let key_c = (Some(key_b.0.unwrap_or(0) + 1), Some(0));
    let keys = [key_a, key_b, key_c];

    let make_value = |w: f32| FlexCacheValue {
      measured_size: Size::new(w, 10.0),
      border_size: Size::new(w, 10.0),
      fragment: make_test_fragment(Rect::from_xywh(0.0, 0.0, w, 10.0)),
    };

    assert!(cache_a.insert(node_key, keys[0], make_value(1.0), per_node_cap));
    assert!(cache_a.insert(node_key, keys[1], make_value(2.0), per_node_cap));
    // The third (largest) key should be deterministically evicted so we keep the smallest N keys.
    cache_a.insert(node_key, keys[2], make_value(3.0), per_node_cap);

    assert!(cache_b.insert(node_key, keys[2], make_value(3.0), per_node_cap));
    assert!(cache_b.insert(node_key, keys[1], make_value(2.0), per_node_cap));
    assert!(cache_b.insert(node_key, keys[0], make_value(1.0), per_node_cap));

    let surviving_a: Vec<FlexCacheKey> = keys
      .iter()
      .copied()
      .filter(|key| cache_a.get(node_key, key).is_some())
      .collect();
    let surviving_b: Vec<FlexCacheKey> = keys
      .iter()
      .copied()
      .filter(|key| cache_b.get(node_key, key).is_some())
      .collect();

    assert_eq!(surviving_a, surviving_b);
    assert_eq!(surviving_a, vec![keys[0], keys[1]]);
  }
}
