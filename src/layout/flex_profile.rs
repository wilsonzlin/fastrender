use crate::debug::runtime;
use std::collections::HashMap;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;
use std::sync::OnceLock;
use std::sync::RwLock;
use std::time::Duration;
use std::time::Instant;

static PROGRESS_NEXT: AtomicU64 = AtomicU64::new(0);
static HISTOGRAM: OnceLock<
  std::sync::Mutex<std::collections::HashMap<(usize, (Option<u32>, Option<u32>)), u64>>,
> = OnceLock::new();
static NODE_STATS: OnceLock<std::sync::Mutex<HashMap<usize, NodeStats>>> = OnceLock::new();

static MEASURE_LOOKUPS: AtomicU64 = AtomicU64::new(0);
static MEASURE_HITS: AtomicU64 = AtomicU64::new(0);
static MEASURE_STORES: AtomicU64 = AtomicU64::new(0);
static MEASURE_UNIQUE_KEYS: AtomicU64 = AtomicU64::new(0);
static MEASURE_BUCKETS: [AtomicU64; 9] = [
  AtomicU64::new(0),
  AtomicU64::new(0),
  AtomicU64::new(0),
  AtomicU64::new(0),
  AtomicU64::new(0),
  AtomicU64::new(0),
  AtomicU64::new(0),
  AtomicU64::new(0),
  AtomicU64::new(0),
];
static MEASURE_TIME_NS: AtomicU64 = AtomicU64::new(0);
static BUILD_TIME_NS: AtomicU64 = AtomicU64::new(0);
static COMPUTE_TIME_NS: AtomicU64 = AtomicU64::new(0);
static CONVERT_TIME_NS: AtomicU64 = AtomicU64::new(0);
static LAYOUT_CACHE_HITS: AtomicU64 = AtomicU64::new(0);
static LAYOUT_CACHE_STORES: AtomicU64 = AtomicU64::new(0);
static MATERIALIZE_COUNT: AtomicU64 = AtomicU64::new(0);
static MEASURE_SHARD_STATS: OnceLock<RwLock<CacheShardStats>> = OnceLock::new();
static LAYOUT_SHARD_STATS: OnceLock<RwLock<CacheShardStats>> = OnceLock::new();

#[derive(Clone, Copy, Debug)]
pub enum CacheKind {
  Measure,
  Layout,
}

#[derive(Default)]
struct CacheShardStats {
  hits: Vec<AtomicU64>,
  misses: Vec<AtomicU64>,
}

impl CacheShardStats {
  fn new(count: usize) -> Self {
    Self {
      hits: (0..count).map(|_| AtomicU64::new(0)).collect(),
      misses: (0..count).map(|_| AtomicU64::new(0)).collect(),
    }
  }

  fn ensure_len(&mut self, count: usize) {
    if self.hits.len() >= count {
      return;
    }
    let additional = count - self.hits.len();
    self.hits.extend((0..additional).map(|_| AtomicU64::new(0)));
    self
      .misses
      .extend((0..additional).map(|_| AtomicU64::new(0)));
  }

  fn clear(&mut self) {
    for hit in &self.hits {
      hit.store(0, Ordering::Relaxed);
    }
    for miss in &self.misses {
      miss.store(0, Ordering::Relaxed);
    }
  }
}

#[derive(Default, Clone)]
struct NodeStats {
  selector: Option<String>,
  layouts: u64,
  total_ns: u64,
  max_ns: u64,
  lookups: u64,
  measure_hits: u64,
  measure_stores: u64,
  key_counts: Option<HashMap<(Option<u32>, Option<u32>), u64>>,
}

fn enabled() -> bool {
  runtime::runtime_toggles().truthy("FASTR_FLEX_PROFILE")
}

fn histogram_enabled() -> bool {
  runtime::runtime_toggles().truthy("FASTR_FLEX_PROFILE_HIST")
}

fn node_profile_enabled() -> bool {
  runtime::runtime_toggles().truthy("FASTR_FLEX_PROFILE_NODES")
}

fn node_key_counts_enabled() -> bool {
  runtime::runtime_toggles().truthy("FASTR_FLEX_PROFILE_NODE_KEYS")
}

fn node_key_cap() -> usize {
  runtime::runtime_toggles().usize_with_default("FASTR_FLEX_PROFILE_NODE_KEY_CAP", 20000)
}

fn node_profile_limit() -> usize {
  runtime::runtime_toggles().usize_with_default("FASTR_FLEX_PROFILE_NODES_TOP", 10)
}

fn progress_interval() -> Option<u64> {
  let val = runtime::runtime_toggles().u64("FASTR_FLEX_PROFILE_PROGRESS")?;
  if val == 0 {
    None
  } else {
    Some(val)
  }
}

pub fn flex_profile_enabled() -> bool {
  enabled()
}

pub fn ensure_cache_shards(kind: CacheKind, count: usize) {
  if !enabled() {
    return;
  }
  let target_len = count.max(1);
  let store = match kind {
    CacheKind::Measure => {
      MEASURE_SHARD_STATS.get_or_init(|| RwLock::new(CacheShardStats::new(target_len)))
    }
    CacheKind::Layout => {
      LAYOUT_SHARD_STATS.get_or_init(|| RwLock::new(CacheShardStats::new(target_len)))
    }
  };
  if let Ok(mut guard) = store.write() {
    guard.ensure_len(target_len);
  }
}

pub fn record_cache_shard_lookup(kind: CacheKind, shard_idx: usize, hit: bool) {
  if !enabled() {
    return;
  }
  ensure_cache_shards(kind, shard_idx + 1);
  let store = match kind {
    CacheKind::Measure => MEASURE_SHARD_STATS.get(),
    CacheKind::Layout => LAYOUT_SHARD_STATS.get(),
  };
  if let Some(lock) = store {
    if let Ok(guard) = lock.read() {
      if shard_idx < guard.hits.len() {
        let counter = if hit {
          &guard.hits[shard_idx]
        } else {
          &guard.misses[shard_idx]
        };
        counter.fetch_add(1, Ordering::Relaxed);
      }
    }
  }
}

pub fn cache_shard_stats(kind: CacheKind) -> Option<Vec<(u64, u64)>> {
  if !enabled() {
    return None;
  }
  let store = match kind {
    CacheKind::Measure => MEASURE_SHARD_STATS.get()?,
    CacheKind::Layout => LAYOUT_SHARD_STATS.get()?,
  };
  let guard = store.read().ok()?;
  Some(
    guard
      .hits
      .iter()
      .zip(guard.misses.iter())
      .map(|(hit, miss)| (hit.load(Ordering::Relaxed), miss.load(Ordering::Relaxed)))
      .collect(),
  )
}

pub fn reset_flex_profile() {
  MEASURE_LOOKUPS.store(0, Ordering::Relaxed);
  MEASURE_HITS.store(0, Ordering::Relaxed);
  MEASURE_STORES.store(0, Ordering::Relaxed);
  MEASURE_UNIQUE_KEYS.store(0, Ordering::Relaxed);
  for bucket in MEASURE_BUCKETS.iter() {
    bucket.store(0, Ordering::Relaxed);
  }
  for bucket in MEASURE_HIT_BUCKETS.iter() {
    bucket.store(0, Ordering::Relaxed);
  }
  if histogram_enabled() {
    if let Some(map) = HISTOGRAM.get() {
      if let Ok(mut guard) = map.lock() {
        guard.clear();
      }
    }
  }
  if let Some(map) = NODE_STATS.get() {
    if let Ok(mut guard) = map.lock() {
      for stats in guard.values_mut() {
        stats.layouts = 0;
        stats.total_ns = 0;
        stats.max_ns = 0;
        stats.lookups = 0;
        if let Some(keys) = stats.key_counts.as_mut() {
          keys.clear();
        }
      }
      guard.clear();
    }
  }
  MEASURE_TIME_NS.store(0, Ordering::Relaxed);
  BUILD_TIME_NS.store(0, Ordering::Relaxed);
  COMPUTE_TIME_NS.store(0, Ordering::Relaxed);
  CONVERT_TIME_NS.store(0, Ordering::Relaxed);
  LAYOUT_CACHE_HITS.store(0, Ordering::Relaxed);
  LAYOUT_CACHE_STORES.store(0, Ordering::Relaxed);
  MATERIALIZE_COUNT.store(0, Ordering::Relaxed);
  PROGRESS_NEXT.store(0, Ordering::Relaxed);
  if let Some(lock) = MEASURE_SHARD_STATS.get() {
    if let Ok(mut guard) = lock.write() {
      guard.clear();
    }
  }
  if let Some(lock) = LAYOUT_SHARD_STATS.get() {
    if let Ok(mut guard) = lock.write() {
      guard.clear();
    }
  }
}

fn maybe_log_progress(current: u64) {
  let Some(interval) = progress_interval() else {
    return;
  };
  let mut next = PROGRESS_NEXT.load(Ordering::Relaxed);
  if next == 0 {
    next = interval;
    PROGRESS_NEXT.store(next, Ordering::Relaxed);
  }
  if current < next {
    return;
  }

  let hits = MEASURE_HITS.load(Ordering::Relaxed);
  let stores = MEASURE_STORES.load(Ordering::Relaxed);
  let unique = MEASURE_UNIQUE_KEYS.load(Ordering::Relaxed);
  let measure_ms = MEASURE_TIME_NS.load(Ordering::Relaxed) as f64 / 1_000_000.0;
  let rate = if current > 0 {
    (hits as f64 / current as f64) * 100.0
  } else {
    0.0
  };
  eprintln!(
        "flex profile progress: lookups={} hits={} hit_rate={:.2}% stores={} unique_keys={} measure_ms={:.2}",
        current, hits, rate, stores, unique, measure_ms
    );
  PROGRESS_NEXT.store(current.saturating_add(interval), Ordering::Relaxed);
}

pub fn record_measure_lookup() {
  if enabled() {
    let current = MEASURE_LOOKUPS.fetch_add(1, Ordering::Relaxed) + 1;
    maybe_log_progress(current);
  }
}

pub fn record_measure_hit() {
  if enabled() {
    MEASURE_HITS.fetch_add(1, Ordering::Relaxed);
  }
}

pub fn record_measure_store(new_key: bool) {
  if !enabled() {
    return;
  }
  MEASURE_STORES.fetch_add(1, Ordering::Relaxed);
  if new_key {
    MEASURE_UNIQUE_KEYS.fetch_add(1, Ordering::Relaxed);
  }
}

pub fn record_layout_cache_hit() {
  if enabled() {
    LAYOUT_CACHE_HITS.fetch_add(1, Ordering::Relaxed);
  }
}

pub fn record_layout_cache_store() {
  if enabled() {
    LAYOUT_CACHE_STORES.fetch_add(1, Ordering::Relaxed);
  }
}

pub fn record_fragment_materialize() {
  if enabled() {
    MATERIALIZE_COUNT.fetch_add(1, Ordering::Relaxed);
  }
}

#[derive(Copy, Clone)]
pub enum DimState {
  Known,
  Definite,
  Other,
}

fn bucket_index(w: DimState, h: DimState) -> usize {
  let w_idx = match w {
    DimState::Known => 0,
    DimState::Definite => 1,
    DimState::Other => 2,
  };
  let h_idx = match h {
    DimState::Known => 0,
    DimState::Definite => 1,
    DimState::Other => 2,
  };
  w_idx * 3 + h_idx
}

pub fn record_measure_bucket(w: DimState, h: DimState) {
  if !enabled() {
    return;
  }
  let idx = bucket_index(w, h);
  MEASURE_BUCKETS[idx].fetch_add(1, Ordering::Relaxed);
}

pub fn record_measure_bucket_hit(w: DimState, h: DimState) {
  if !enabled() {
    return;
  }
  let idx = bucket_index(w, h);
  MEASURE_HIT_BUCKETS[idx].fetch_add(1, Ordering::Relaxed);
}

pub fn record_histogram(bucket: usize, key: (Option<u32>, Option<u32>)) {
  if !histogram_enabled() {
    return;
  }
  let map = HISTOGRAM.get_or_init(|| std::sync::Mutex::new(std::collections::HashMap::new()));
  if let Ok(mut guard) = map.lock() {
    *guard.entry((bucket, key)).or_insert(0) += 1;
  }
}

pub fn record_measure_time(start: Option<Instant>) {
  if let Some(s) = start {
    if enabled() {
      MEASURE_TIME_NS.fetch_add(s.elapsed().as_nanos() as u64, Ordering::Relaxed);
    }
  }
}

pub fn record_build_time(start: Option<Instant>) {
  if let Some(s) = start {
    if enabled() {
      BUILD_TIME_NS.fetch_add(s.elapsed().as_nanos() as u64, Ordering::Relaxed);
    }
  }
}

pub fn record_compute_time(start: Option<Instant>) {
  if let Some(s) = start {
    if enabled() {
      COMPUTE_TIME_NS.fetch_add(s.elapsed().as_nanos() as u64, Ordering::Relaxed);
    }
  }
}

pub fn record_convert_time(start: Option<Instant>) {
  if let Some(s) = start {
    if enabled() {
      CONVERT_TIME_NS.fetch_add(s.elapsed().as_nanos() as u64, Ordering::Relaxed);
    }
  }
}

pub fn timer() -> Option<Instant> {
  if enabled() {
    Some(Instant::now())
  } else {
    None
  }
}

pub fn node_timer() -> Option<Instant> {
  if node_profile_enabled() {
    Some(Instant::now())
  } else {
    None
  }
}

pub fn record_node_layout(id: usize, selector: Option<&str>, start: Option<Instant>) {
  let Some(timer) = start else { return };
  if !node_profile_enabled() {
    return;
  }
  let elapsed = timer.elapsed();
  let map = NODE_STATS.get_or_init(|| std::sync::Mutex::new(HashMap::new()));
  if let Ok(mut guard) = map.lock() {
    let entry = guard.entry(id).or_insert_with(NodeStats::default);
    entry.layouts += 1;
    entry.total_ns += elapsed.as_nanos() as u64;
    entry.max_ns = entry.max_ns.max(elapsed.as_nanos() as u64);
    if entry.selector.is_none() {
      if let Some(sel) = selector {
        if !sel.is_empty() {
          entry.selector = Some(sel.to_string());
        }
      }
    }
  }
}

pub fn record_node_lookup(id: usize, key: (Option<u32>, Option<u32>)) {
  if !node_profile_enabled() {
    return;
  }
  let map = NODE_STATS.get_or_init(|| std::sync::Mutex::new(HashMap::new()));
  if let Ok(mut guard) = map.lock() {
    let entry = guard.entry(id).or_insert_with(NodeStats::default);
    entry.lookups += 1;
    if node_key_counts_enabled() {
      let cap = node_key_cap();
      let counts = entry.key_counts.get_or_insert_with(HashMap::new);
      if counts.len() < cap || counts.contains_key(&key) {
        *counts.entry(key).or_insert(0) += 1;
      }
    }
  }
}

pub fn record_node_measure_hit(id: usize) {
  if !node_profile_enabled() {
    return;
  }
  if let Some(map) = NODE_STATS.get() {
    if let Ok(mut guard) = map.lock() {
      let entry = guard.entry(id).or_insert_with(NodeStats::default);
      entry.measure_hits += 1;
    }
  }
}

pub fn record_node_measure_store(id: usize) {
  if !node_profile_enabled() {
    return;
  }
  if let Some(map) = NODE_STATS.get() {
    if let Ok(mut guard) = map.lock() {
      let entry = guard.entry(id).or_insert_with(NodeStats::default);
      entry.measure_stores += 1;
    }
  }
}

pub fn log_flex_profile(total: Duration) {
  let enabled = enabled();
  let nodes_enabled = node_profile_enabled();
  if !enabled && !nodes_enabled {
    return;
  }
  let measure_lookups = MEASURE_LOOKUPS.load(Ordering::Relaxed);
  let measure_hits = MEASURE_HITS.load(Ordering::Relaxed);
  let measure_stores = MEASURE_STORES.load(Ordering::Relaxed);
  let measure_unique = MEASURE_UNIQUE_KEYS.load(Ordering::Relaxed);
  let layout_hits = LAYOUT_CACHE_HITS.load(Ordering::Relaxed);
  let layout_stores = LAYOUT_CACHE_STORES.load(Ordering::Relaxed);
  let materialize_count = MATERIALIZE_COUNT.load(Ordering::Relaxed);
  let measure_shards = cache_shard_stats(CacheKind::Measure);
  let layout_shards = cache_shard_stats(CacheKind::Layout);
  let measure_ms = MEASURE_TIME_NS.load(Ordering::Relaxed) as f64 / 1_000_000.0;
  let build_ms = BUILD_TIME_NS.load(Ordering::Relaxed) as f64 / 1_000_000.0;
  let compute_ms = COMPUTE_TIME_NS.load(Ordering::Relaxed) as f64 / 1_000_000.0;
  let convert_ms = CONVERT_TIME_NS.load(Ordering::Relaxed) as f64 / 1_000_000.0;
  let buckets = MEASURE_BUCKETS
    .iter()
    .map(|b| b.load(Ordering::Relaxed))
    .collect::<Vec<_>>();
  let hit_buckets = MEASURE_HIT_BUCKETS
    .iter()
    .map(|b| b.load(Ordering::Relaxed))
    .collect::<Vec<_>>();
  let bucket_rates: Vec<f64> = buckets
    .iter()
    .zip(hit_buckets.iter())
    .map(|(&total, &hits)| {
      if total > 0 {
        (hits as f64 / total as f64) * 100.0
      } else {
        0.0
      }
    })
    .collect();
  let shard_totals = |stats: &Option<Vec<(u64, u64)>>| -> (u64, u64) {
    stats
      .as_ref()
      .map(|entries| {
        entries.iter().fold((0, 0), |(hit_acc, miss_acc), (h, m)| {
          (hit_acc + h, miss_acc + m)
        })
      })
      .unwrap_or((0, 0))
  };
  let (measure_shard_hits, measure_shard_misses) = shard_totals(&measure_shards);
  let (layout_shard_hits, layout_shard_misses) = shard_totals(&layout_shards);
  if enabled {
    eprintln!(
            "flex profile: total_ms={:.2} build_ms={:.2} compute_ms={:.2} convert_ms={:.2} measure_ms={:.2} lookups={} hits={} hit_rate={:.2}% stores={} unique_keys={} layout_cache_hits={} layout_cache_stores={} materializations={} buckets=[kw/kh:{} kw/dh:{} kw/oh:{} dw/kh:{} dw/dh:{} dw/oh:{} ow/kh:{} ow/dh:{} ow/oh:{}] bucket_hits=[kw/kh:{} kw/dh:{} kw/oh:{} dw/kh:{} dw/dh:{} dw/oh:{} ow/kh:{} ow/dh:{} ow/oh:{}] bucket_hit_rate%=[kw/kh:{:.1} kw/dh:{:.1} kw/oh:{:.1} dw/kh:{:.1} dw/dh:{:.1} dw/oh:{:.1} ow/kh:{:.1} ow/dh:{:.1} ow/oh:{:.1}]",
            total.as_secs_f64() * 1000.0,
            build_ms,
            compute_ms,
            convert_ms,
            measure_ms,
            measure_lookups,
            measure_hits,
            if measure_lookups > 0 {
                (measure_hits as f64 / measure_lookups as f64) * 100.0
            } else {
                0.0
            },
            measure_stores,
            measure_unique,
            layout_hits,
            layout_stores,
            materialize_count,
            buckets.first().copied().unwrap_or(0),
            buckets.get(1).copied().unwrap_or(0),
            buckets.get(2).copied().unwrap_or(0),
            buckets.get(3).copied().unwrap_or(0),
            buckets.get(4).copied().unwrap_or(0),
            buckets.get(5).copied().unwrap_or(0),
            buckets.get(6).copied().unwrap_or(0),
            buckets.get(7).copied().unwrap_or(0),
            buckets.get(8).copied().unwrap_or(0),
            hit_buckets.first().copied().unwrap_or(0),
            hit_buckets.get(1).copied().unwrap_or(0),
            hit_buckets.get(2).copied().unwrap_or(0),
            hit_buckets.get(3).copied().unwrap_or(0),
            hit_buckets.get(4).copied().unwrap_or(0),
            hit_buckets.get(5).copied().unwrap_or(0),
            hit_buckets.get(6).copied().unwrap_or(0),
            hit_buckets.get(7).copied().unwrap_or(0),
            hit_buckets.get(8).copied().unwrap_or(0),
            bucket_rates.first().copied().unwrap_or(0.0),
            bucket_rates.get(1).copied().unwrap_or(0.0),
            bucket_rates.get(2).copied().unwrap_or(0.0),
            bucket_rates.get(3).copied().unwrap_or(0.0),
            bucket_rates.get(4).copied().unwrap_or(0.0),
            bucket_rates.get(5).copied().unwrap_or(0.0),
            bucket_rates.get(6).copied().unwrap_or(0.0),
            bucket_rates.get(7).copied().unwrap_or(0.0),
            bucket_rates.get(8).copied().unwrap_or(0.0)
        );
    if measure_shard_hits + measure_shard_misses + layout_shard_hits + layout_shard_misses > 0 {
      eprintln!(
        "flex cache shards: measure_hits={} measure_misses={} layout_hits={} layout_misses={}",
        measure_shard_hits, measure_shard_misses, layout_shard_hits, layout_shard_misses
      );
    }
  }

  if histogram_enabled() {
    if let Some(map) = HISTOGRAM.get() {
      if let Ok(guard) = map.lock() {
        let mut entries: Vec<_> = guard.iter().collect();
        entries.sort_by_key(|e| std::cmp::Reverse(*e.1));
        let top_n = entries.into_iter().take(8).collect::<Vec<_>>();
        if !top_n.is_empty() {
          let formatted: Vec<String> = top_n
            .iter()
            .map(|((bucket, key), count)| {
              format!(
                "bucket={} key=({:?},{:?}) count={}",
                bucket, key.0, key.1, count
              )
            })
            .collect();
          eprintln!("flex profile hist (top8): {}", formatted.join(" | "));
        }
      }
    }
  }

  if nodes_enabled {
    log_node_profile();
  }
}
static MEASURE_HIT_BUCKETS: [AtomicU64; 9] = [
  AtomicU64::new(0),
  AtomicU64::new(0),
  AtomicU64::new(0),
  AtomicU64::new(0),
  AtomicU64::new(0),
  AtomicU64::new(0),
  AtomicU64::new(0),
  AtomicU64::new(0),
  AtomicU64::new(0),
];

fn log_node_profile() {
  let Some(map) = NODE_STATS.get() else { return };
  let Ok(guard) = map.lock() else { return };
  if guard.is_empty() {
    return;
  }
  let mut entries: Vec<_> = guard.iter().collect();
  entries.sort_by_key(|(_, stats)| std::cmp::Reverse(stats.total_ns));
  let limit = node_profile_limit();
  let summary: Vec<String> = entries
        .into_iter()
        .take(limit)
        .map(|(id, stats)| {
            let selector = stats.selector.as_deref().unwrap_or("<unknown>");
            let avg_ns = if stats.layouts > 0 {
                stats.total_ns as f64 / stats.layouts as f64
            } else {
                0.0
            };
            let unique_keys = stats.key_counts.as_ref().map(|m| m.len()).unwrap_or(0);
            let hit_rate = if stats.lookups > 0 {
                (stats.measure_hits as f64 / stats.lookups as f64) * 100.0
            } else {
                0.0
            };
            format!(
                "id={} selector={} layouts={} total_ms={:.2} avg_ms={:.3} max_ms={:.2} lookups={} hits={} hit_rate={:.1}% stores={} unique_keys={}",
                id,
                selector,
                stats.layouts,
                stats.total_ns as f64 / 1_000_000.0,
                avg_ns / 1_000_000.0,
                stats.max_ns as f64 / 1_000_000.0,
                stats.lookups,
                stats.measure_hits,
                hit_rate,
                stats.measure_stores,
                unique_keys
            )
        })
        .collect();
  let mut lookup_entries: Vec<_> = guard
    .iter()
    .filter(|(_, stats)| stats.lookups > 0)
    .collect();
  lookup_entries.sort_by_key(|(_, stats)| std::cmp::Reverse(stats.lookups));
  let lookup_summary: Vec<String> = lookup_entries
    .into_iter()
    .take(limit)
    .map(|(id, stats)| {
      let selector = stats.selector.as_deref().unwrap_or("<unknown>");
      let unique_keys = stats.key_counts.as_ref().map(|m| m.len()).unwrap_or(0);
      format!(
        "id={} selector={} lookups={} unique_keys={}",
        id, selector, stats.lookups, unique_keys
      )
    })
    .collect();
  eprintln!(
    "flex profile nodes (top{}): {}",
    limit.min(summary.len()),
    summary.join(" | ")
  );
  if !lookup_summary.is_empty() {
    eprintln!(
      "flex profile node lookups (top{}): {}",
      limit.min(lookup_summary.len()),
      lookup_summary.join(" | ")
    );
  }

  if histogram_enabled() {
    if let Some(map) = HISTOGRAM.get() {
      if let Ok(guard) = map.lock() {
        if !guard.is_empty() {
          let mut entries: Vec<_> = guard.iter().collect();
          entries.sort_by_key(|(_, count)| std::cmp::Reverse(*count));
          let top = entries.iter().take(10).map(|((bucket, key), count)| {
            let decode = |v: Option<u32>| -> String {
              v.map(|b| f32::from_bits(b))
                .map(|f| {
                  if f.is_sign_negative() {
                    format!("{:.1} (sentinel)", f)
                  } else {
                    format!("{:.1}", f)
                  }
                })
                .unwrap_or_else(|| "None".into())
            };
            format!(
              "bucket={} key=({},{}) count={}",
              bucket,
              decode(key.0),
              decode(key.1),
              count
            )
          });
          eprintln!(
            "flex profile histogram top10: {}",
            top.collect::<Vec<_>>().join(" | ")
          );
        }
      }
    }
  }
}
