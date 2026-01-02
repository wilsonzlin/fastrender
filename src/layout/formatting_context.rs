//! Formatting Context trait - core layout abstraction
//!
//! This module defines the FormattingContext trait, which is the fundamental
//! abstraction for all layout algorithms in FastRender. Every layout mode
//! (block, inline, flex, grid, table) implements this trait.
//!
//! # The FormattingContext Abstraction
//!
//! In CSS, different display values create different "formatting contexts" with
//! completely different layout rules:
//!
//! - **Block FC**: Boxes stack vertically, margins collapse (CSS 2.1 Section 9.4.1)
//! - **Inline FC**: Boxes flow horizontally, wrap at line boundaries (CSS 2.1 Section 9.4.2)
//! - **Flex FC**: Flexbox layout algorithm (CSS Flexbox spec)
//! - **Grid FC**: Grid layout algorithm (CSS Grid spec)
//! - **Table FC**: Table layout algorithm (CSS 2.1 Section 17)
//!
//! This trait provides a uniform interface that allows the layout engine to
//! dispatch to the appropriate algorithm without knowing implementation details.
//!
//! # Benefits of This Abstraction
//!
//! - **Modularity**: Each FC is independently implementable and testable
//! - **Extensibility**: New FCs can be added without modifying core engine
//! - **Parallelization**: Different FCs can be implemented concurrently in different tasks
//! - **Clarity**: Separates concerns - each FC focuses on one layout mode
//!
//! # References
//!
//! - CSS 2.1 Section 9.4 - Normal Flow: <https://www.w3.org/TR/CSS21/visuren.html#normal-flow>
//! - CSS Flexbox: <https://www.w3.org/TR/css-flexbox-1/>
//! - CSS Grid: <https://www.w3.org/TR/css-grid-1/>

use crate::debug::runtime;
use crate::geometry::Size;
use crate::layout::constraints::AvailableSpace;
use crate::layout::constraints::LayoutConstraints;
use crate::layout::fragment_clone_profile::{self, CloneSite};
use crate::layout::fragmentation::FragmentationOptions;
use crate::style::display::FormattingContextType;
use crate::style::float::Float;
use crate::style::position::Position;
use crate::style::ComputedStyle;
use crate::style::values::{CalcLength, Length};
use crate::tree::box_tree::BoxNode;
use crate::tree::fragment_tree::FragmentNode;
use parking_lot::RwLock;
use rustc_hash::FxHashMap;
use rustc_hash::FxHasher as DefaultHasher;
use std::cell::Cell;
use std::cell::RefCell;
use std::hash::Hash;
use std::hash::Hasher;
use std::sync::atomic::{AtomicBool, AtomicU64, AtomicUsize, Ordering};
use std::sync::Arc;
use std::sync::LazyLock;
use std::time::Duration;

/// Intrinsic sizing mode for content-based size queries
///
/// CSS defines two fundamental intrinsic sizes:
/// - **Min-content**: The smallest size at which content fits without overflow
/// - **Max-content**: The largest size needed to fit content without wrapping
///
/// These are used for:
/// - `width: min-content` and `width: max-content` CSS properties
/// - Flexbox intrinsic sizing (`flex-basis: content`)
/// - Grid track sizing with `min-content` and `max-content`
/// - Auto table column sizing
///
/// # Examples
///
/// For a text box containing "Hello World":
/// - **MinContent**: Width of longest word ("World") - narrowest without overflow
/// - **MaxContent**: Width of entire text ("Hello World") - widest without wrapping
///
/// # Reference
///
/// CSS Sizing Module Level 3: <https://www.w3.org/TR/css-sizing-3/>
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntrinsicSizingMode {
  /// Minimum content size (narrowest possible without overflow)
  ///
  /// Corresponds to CSS `min-content` keyword.
  /// For text, this is typically the width of the longest word.
  MinContent,

  /// Maximum content size (widest without line breaking)
  ///
  /// Corresponds to CSS `max-content` keyword.
  /// For text, this is the width needed to fit all content on one line.
  MaxContent,
}

type IntrinsicCacheKey = (usize, usize, IntrinsicSizingMode);

/// Number of shards used for intrinsic sizing caches.
///
/// The intrinsic sizing hot path is extremely sensitive to cache locality: layout fan-out can
/// distribute work across rayon threads, and thread-local caches cause the same expensive intrinsic
/// measurements to be recomputed on multiple workers. Using a sharded shared cache avoids this
/// amplification while still allowing concurrent reads.
const INTRINSIC_CACHE_SHARDS: usize = 64;

struct ShardedIntrinsicCache {
  shards: [RwLock<FxHashMap<IntrinsicCacheKey, (usize, f32)>>; INTRINSIC_CACHE_SHARDS],
}

impl ShardedIntrinsicCache {
  fn new() -> Self {
    Self {
      shards: std::array::from_fn(|_| RwLock::new(FxHashMap::default())),
    }
  }

  #[inline]
  fn shard_index_for_box_id(box_id: usize) -> usize {
    // Box ids are dense and already act like a good hash for sharding.
    box_id % INTRINSIC_CACHE_SHARDS
  }

  #[inline]
  fn get(&self, key: &IntrinsicCacheKey, epoch: usize) -> Option<f32> {
    let box_id = key.0;
    let shard = &self.shards[Self::shard_index_for_box_id(box_id)];
    let map = shard.read();
    map.get(key)
      .and_then(|(entry_epoch, value)| (*entry_epoch == epoch).then_some(*value))
  }

  #[inline]
  fn insert(&self, key: IntrinsicCacheKey, epoch: usize, value: f32) {
    let shard = &self.shards[Self::shard_index_for_box_id(key.0)];
    shard.write().insert(key, (epoch, value));
  }

  fn clear(&self) {
    for shard in &self.shards {
      shard.write().clear();
    }
  }
}

static GLOBAL_INTRINSIC_INLINE_CACHE: LazyLock<ShardedIntrinsicCache> =
  LazyLock::new(ShardedIntrinsicCache::new);
static GLOBAL_INTRINSIC_BLOCK_CACHE: LazyLock<ShardedIntrinsicCache> =
  LazyLock::new(ShardedIntrinsicCache::new);

thread_local! {
  /// Intrinsic sizing cache for callers that temporarily override the effective `ComputedStyle`
  /// via `layout::style_override`.
  ///
  /// The primary intrinsic cache keys off the `Arc<ComputedStyle>` pointer stored on the
  /// `BoxNode`. Style overrides intentionally avoid allocating/cloning entire `BoxNode` subtrees,
  /// so the style pointer does not change even when the effective style does. To keep intrinsic
  /// sizing cached (and safe) under overrides, we maintain a parallel cache keyed by a stable
  /// signature of the override style values.
  static INTRINSIC_INLINE_OVERRIDE_CACHE: RefCell<
    FxHashMap<(usize, u64, IntrinsicSizingMode), (usize, f32)>,
  > = RefCell::new(FxHashMap::default());
}

thread_local! {
  /// Block-axis counterpart to `INTRINSIC_INLINE_OVERRIDE_CACHE`.
  static INTRINSIC_BLOCK_OVERRIDE_CACHE: RefCell<
    FxHashMap<(usize, u64, IntrinsicSizingMode), (usize, f32)>,
  > = RefCell::new(FxHashMap::default());
}

thread_local! {
  static FRAGMENTAINER_BLOCK_SIZE_HINT: Cell<Option<f32>> = Cell::new(None);
}

thread_local! {
  /// Per-thread fast path for intrinsic sizing lookups.
  ///
  /// The global intrinsic caches are shared across rayon workers, but every lookup still requires
  /// taking a (sharded) read lock. Hot grid/flex pages can issue hundreds of thousands of intrinsic
  /// lookups; a tiny per-thread layer avoids repeated lock acquisitions once a value has been
  /// observed on that worker.
  static INTRINSIC_INLINE_CACHE_TL: RefCell<FxHashMap<IntrinsicCacheKey, f32>> =
    RefCell::new(FxHashMap::default());
  static INTRINSIC_BLOCK_CACHE_TL: RefCell<FxHashMap<IntrinsicCacheKey, f32>> =
    RefCell::new(FxHashMap::default());
  static INTRINSIC_CACHE_TL_EPOCH: Cell<usize> = const { Cell::new(0) };

  /// Subgrid dependency memoization keyed by node id within a cache epoch.
  static SUBGRID_DEPENDENT_CACHE: RefCell<FxHashMap<usize, (usize, bool)>> =
    RefCell::new(FxHashMap::default());
  static SUBGRID_CACHE_EPOCH: Cell<usize> = const { Cell::new(0) };
}

const COUNTER_SHARDS: usize = 64;

#[repr(align(64))]
struct PaddedAtomicUsize(AtomicUsize);

impl PaddedAtomicUsize {
  const fn new(value: usize) -> Self {
    Self(AtomicUsize::new(value))
  }

  fn fetch_add(&self, value: usize) {
    self.0.fetch_add(value, Ordering::Relaxed);
  }

  fn store(&self, value: usize) {
    self.0.store(value, Ordering::Relaxed);
  }

  fn load(&self) -> usize {
    self.0.load(Ordering::Relaxed)
  }
}

struct ShardedCounter {
  shards: [PaddedAtomicUsize; COUNTER_SHARDS],
}

impl ShardedCounter {
  fn new() -> Self {
    Self {
      shards: std::array::from_fn(|_| PaddedAtomicUsize::new(0)),
    }
  }

  #[inline]
  fn shard_index() -> usize {
    rayon::current_thread_index().unwrap_or(0) % COUNTER_SHARDS
  }

  #[inline]
  fn inc(&self) {
    self.add(1);
  }

  #[inline]
  fn add(&self, value: usize) {
    self.shards[Self::shard_index()].fetch_add(value);
  }

  fn store(&self, value: usize) {
    for shard in &self.shards {
      shard.store(value);
    }
  }

  fn load_sum(&self) -> usize {
    self.shards.iter().map(|shard| shard.load()).sum()
  }
}

static CACHE_LOOKUPS: LazyLock<ShardedCounter> = LazyLock::new(ShardedCounter::new);
static CACHE_HITS: LazyLock<ShardedCounter> = LazyLock::new(ShardedCounter::new);
static CACHE_STORES: LazyLock<ShardedCounter> = LazyLock::new(ShardedCounter::new);
static CACHE_EPOCH: AtomicUsize = AtomicUsize::new(1);
static BLOCK_INTRINSIC_CALLS: LazyLock<ShardedCounter> = LazyLock::new(ShardedCounter::new);
static FLEX_INTRINSIC_CALLS: LazyLock<ShardedCounter> = LazyLock::new(ShardedCounter::new);
static INLINE_INTRINSIC_CALLS: LazyLock<ShardedCounter> = LazyLock::new(ShardedCounter::new);
static LAYOUT_CACHE_LOOKUPS: LazyLock<ShardedCounter> = LazyLock::new(ShardedCounter::new);
static LAYOUT_CACHE_HITS: LazyLock<ShardedCounter> = LazyLock::new(ShardedCounter::new);
static LAYOUT_CACHE_STORES: LazyLock<ShardedCounter> = LazyLock::new(ShardedCounter::new);
static LAYOUT_CACHE_EVICTIONS: LazyLock<ShardedCounter> = LazyLock::new(ShardedCounter::new);
static LAYOUT_CACHE_CLONE_RETURNS: LazyLock<ShardedCounter> = LazyLock::new(ShardedCounter::new);

// Layout cache configuration is stored globally so all rayon worker threads observe the same
// enablement + epoch for a layout run. The cached fragments themselves remain thread-local to avoid
// locking during parallel layout fan-out.
static LAYOUT_CACHE_GLOBAL_ENABLED: AtomicBool = AtomicBool::new(false);
static LAYOUT_CACHE_GLOBAL_EPOCH: AtomicUsize = AtomicUsize::new(1);
static LAYOUT_CACHE_GLOBAL_FRAGMENTATION: AtomicU64 = AtomicU64::new(0);
/// Stores the per-thread entry cap for `LAYOUT_RESULT_CACHE` (0 means "no cap / caching disabled").
static LAYOUT_CACHE_GLOBAL_ENTRY_LIMIT: AtomicUsize = AtomicUsize::new(0);

#[cfg(test)]
static INTRINSIC_CACHE_TEST_LOCK: LazyLock<parking_lot::ReentrantMutex<()>> =
  LazyLock::new(|| parking_lot::ReentrantMutex::new(()));

#[cfg(test)]
pub(crate) fn intrinsic_cache_test_lock() -> parking_lot::ReentrantMutexGuard<'static, ()> {
  INTRINSIC_CACHE_TEST_LOCK.lock()
}

#[cfg(test)]
static LAYOUT_CACHE_TEST_LOCK: LazyLock<parking_lot::ReentrantMutex<()>> =
  LazyLock::new(|| parking_lot::ReentrantMutex::new(()));

#[cfg(test)]
pub(crate) fn layout_cache_test_lock() -> parking_lot::ReentrantMutexGuard<'static, ()> {
  LAYOUT_CACHE_TEST_LOCK.lock()
}

fn clear_subgrid_cache() {
  SUBGRID_DEPENDENT_CACHE.with(|cache| cache.borrow_mut().clear());
}

fn subgrid_cache_use_epoch(epoch: usize, force_clear: bool) {
  let epoch = epoch.max(1);
  SUBGRID_CACHE_EPOCH.with(|cell| {
    let previous = cell.get();
    if force_clear || previous != epoch {
      clear_subgrid_cache();
      cell.set(epoch);
    }
  });
}

#[inline]
fn f32_to_canonical_bits(value: f32) -> u32 {
  if value == 0.0 {
    0.0f32.to_bits()
  } else {
    value.to_bits()
  }
}

fn hash_enum_discriminant<T>(value: &T, hasher: &mut DefaultHasher) {
  std::mem::discriminant(value).hash(hasher);
}

fn hash_calc_length(calc: &CalcLength, hasher: &mut DefaultHasher) {
  let terms = calc.terms();
  (terms.len() as u8).hash(hasher);
  for term in terms {
    term.unit.hash(hasher);
    f32_to_canonical_bits(term.value).hash(hasher);
  }
}

fn hash_length(len: &Length, hasher: &mut DefaultHasher) {
  len.unit.hash(hasher);
  f32_to_canonical_bits(len.value).hash(hasher);
  match &len.calc {
    Some(calc) => {
      1u8.hash(hasher);
      hash_calc_length(calc, hasher);
    }
    None => 0u8.hash(hasher),
  }
}

fn hash_option_length(len: &Option<Length>, hasher: &mut DefaultHasher) {
  match len {
    Some(len) => {
      1u8.hash(hasher);
      hash_length(len, hasher);
    }
    None => 0u8.hash(hasher),
  }
}

fn style_override_signature(box_node: &BoxNode, style: &ComputedStyle) -> u64 {
  // Style overrides are used in hot flex/grid measurement paths to temporarily adjust sizing
  // hints (e.g., clearing percentage widths for intrinsic probes). We incorporate the base style
  // pointer so overrides remain distinct across cascades, then hash the subset of style fields
  // that influence sizing. This stays significantly cheaper than hashing the entire
  // `ComputedStyle`, while remaining correct for the intended override use cases.
  let mut h = DefaultHasher::default();
  (Arc::as_ptr(&box_node.style) as usize).hash(&mut h);
  hash_enum_discriminant(&style.display, &mut h);
  hash_enum_discriminant(&style.position, &mut h);
  hash_enum_discriminant(&style.box_sizing, &mut h);
  hash_enum_discriminant(&style.writing_mode, &mut h);
  hash_enum_discriminant(&style.direction, &mut h);
  hash_option_length(&style.width, &mut h);
  hash_option_length(&style.height, &mut h);
  hash_option_length(&style.min_width, &mut h);
  hash_option_length(&style.max_width, &mut h);
  hash_option_length(&style.min_height, &mut h);
  hash_option_length(&style.max_height, &mut h);
  hash_length(&style.padding_top, &mut h);
  hash_length(&style.padding_right, &mut h);
  hash_length(&style.padding_bottom, &mut h);
  hash_length(&style.padding_left, &mut h);
  hash_length(&style.border_top_width, &mut h);
  hash_length(&style.border_right_width, &mut h);
  hash_length(&style.border_bottom_width, &mut h);
  hash_length(&style.border_left_width, &mut h);
  hash_enum_discriminant(&style.overflow_x, &mut h);
  hash_enum_discriminant(&style.overflow_y, &mut h);
  hash_enum_discriminant(&style.scrollbar_width, &mut h);
  hash_enum_discriminant(&style.line_height, &mut h);
  style.font_size.to_bits().hash(&mut h);
  style.root_font_size.to_bits().hash(&mut h);
  h.finish()
}

fn cache_key(node: &BoxNode, mode: IntrinsicSizingMode, epoch: usize) -> Option<IntrinsicCacheKey> {
  let id = node.id();
  if id == 0 || subtree_contains_subgrid(node, epoch) {
    return None;
  }
  let style_ptr = Arc::as_ptr(&node.style) as usize;
  Some((id, style_ptr, mode))
}

fn override_cache_key(
  node: &BoxNode,
  mode: IntrinsicSizingMode,
  epoch: usize,
  override_style: &ComputedStyle,
) -> Option<(usize, u64, IntrinsicSizingMode)> {
  let id = node.id();
  if id == 0 || subtree_contains_subgrid(node, epoch) {
    return None;
  }
  let signature = style_override_signature(node, override_style);
  Some((id, signature, mode))
}

#[inline]
fn ensure_intrinsic_thread_epoch(epoch: usize) {
  INTRINSIC_CACHE_TL_EPOCH.with(|cell| {
    if cell.get() == epoch {
      return;
    }
    INTRINSIC_INLINE_CACHE_TL.with(|cache| cache.borrow_mut().clear());
    INTRINSIC_BLOCK_CACHE_TL.with(|cache| cache.borrow_mut().clear());
    INTRINSIC_INLINE_OVERRIDE_CACHE.with(|cache| cache.borrow_mut().clear());
    INTRINSIC_BLOCK_OVERRIDE_CACHE.with(|cache| cache.borrow_mut().clear());
    cell.set(epoch);
  });
}

pub(crate) fn intrinsic_cache_lookup(node: &BoxNode, mode: IntrinsicSizingMode) -> Option<f32> {
  CACHE_LOOKUPS.inc();
  let epoch = CACHE_EPOCH.load(Ordering::Relaxed);
  ensure_intrinsic_thread_epoch(epoch);
  let id = node.id();
  if let Some(style_override) = crate::layout::style_override::style_override_for(id) {
    let key = override_cache_key(node, mode, epoch, style_override.as_ref())?;
    let hit = INTRINSIC_INLINE_OVERRIDE_CACHE.with(|cache| {
      cache
        .borrow()
        .get(&key)
        .and_then(|(entry_epoch, value)| (*entry_epoch == epoch).then_some(*value))
    });
    if hit.is_some() {
      CACHE_HITS.inc();
    }
    return hit;
  }

  let key = cache_key(node, mode, epoch)?;

  if let Some(hit) = INTRINSIC_INLINE_CACHE_TL.with(|cache| cache.borrow().get(&key).copied()) {
    CACHE_HITS.inc();
    return Some(hit);
  }

  let hit = GLOBAL_INTRINSIC_INLINE_CACHE.get(&key, epoch)?;
  INTRINSIC_INLINE_CACHE_TL.with(|cache| {
    cache.borrow_mut().insert(key, hit);
  });
  CACHE_HITS.inc();
  Some(hit)
}

pub(crate) fn intrinsic_cache_store(node: &BoxNode, mode: IntrinsicSizingMode, value: f32) {
  let epoch = CACHE_EPOCH.load(Ordering::Relaxed);
  ensure_intrinsic_thread_epoch(epoch);

  let id = node.id();
  if let Some(style_override) = crate::layout::style_override::style_override_for(id) {
    if let Some(key) = override_cache_key(node, mode, epoch, style_override.as_ref()) {
      INTRINSIC_INLINE_OVERRIDE_CACHE.with(|cache| {
        cache.borrow_mut().insert(key, (epoch, value));
      });
      CACHE_STORES.inc();
    }
    return;
  }

  if let Some(key) = cache_key(node, mode, epoch) {
    INTRINSIC_INLINE_CACHE_TL.with(|cache| {
      cache.borrow_mut().insert(key, value);
    });
    GLOBAL_INTRINSIC_INLINE_CACHE.insert(key, epoch, value);
    CACHE_STORES.inc();
  }
}

pub(crate) fn intrinsic_cache_clear() {
  GLOBAL_INTRINSIC_INLINE_CACHE.clear();
  GLOBAL_INTRINSIC_BLOCK_CACHE.clear();
  INTRINSIC_INLINE_CACHE_TL.with(|cache| cache.borrow_mut().clear());
  INTRINSIC_BLOCK_CACHE_TL.with(|cache| cache.borrow_mut().clear());
  INTRINSIC_INLINE_OVERRIDE_CACHE.with(|cache| cache.borrow_mut().clear());
  INTRINSIC_BLOCK_OVERRIDE_CACHE.with(|cache| cache.borrow_mut().clear());
  INTRINSIC_CACHE_TL_EPOCH.with(|cell| cell.set(0));
  clear_subgrid_cache();
}

/// Sets the active intrinsic cache epoch, clearing stale entries when it changes.
///
/// Epochs are used instead of raw clears so callers can opt-in to cache reuse (by
/// reusing the same epoch) or enforce invalidation (by bumping the epoch).
pub(crate) fn intrinsic_cache_use_epoch(epoch: usize, reset_counters: bool) {
  #[cfg(test)]
  let _guard = intrinsic_cache_test_lock();

  let requested_epoch = epoch;
  let epoch = epoch.max(1);
  let previous = CACHE_EPOCH.swap(epoch, Ordering::Relaxed);
  let epoch_changed = previous != requested_epoch;
  if epoch_changed {
    intrinsic_cache_clear();
  }
  if reset_counters {
    intrinsic_cache_reset_counters();
  }
  subgrid_cache_use_epoch(epoch, reset_counters || epoch_changed);
}

/// Returns the active epoch used for intrinsic and layout-adjacent caches.
pub(crate) fn intrinsic_cache_epoch() -> usize {
  CACHE_EPOCH.load(Ordering::Relaxed)
}

/// Resets tracked counts for intrinsic sizing cache metrics.
pub(crate) fn intrinsic_cache_reset_counters() {
  CACHE_LOOKUPS.store(0);
  CACHE_HITS.store(0);
  CACHE_STORES.store(0);
  BLOCK_INTRINSIC_CALLS.store(0);
  FLEX_INTRINSIC_CALLS.store(0);
  INLINE_INTRINSIC_CALLS.store(0);
}

pub(crate) fn intrinsic_cache_stats() -> (usize, usize, usize, usize, usize, usize) {
  (
    CACHE_LOOKUPS.load_sum(),
    CACHE_HITS.load_sum(),
    CACHE_STORES.load_sum(),
    BLOCK_INTRINSIC_CALLS.load_sum(),
    FLEX_INTRINSIC_CALLS.load_sum(),
    INLINE_INTRINSIC_CALLS.load_sum(),
  )
}

pub(crate) fn count_block_intrinsic_call() {
  BLOCK_INTRINSIC_CALLS.inc();
}

pub(crate) fn count_flex_intrinsic_call() {
  FLEX_INTRINSIC_CALLS.inc();
}

pub(crate) fn count_inline_intrinsic_call() {
  INLINE_INTRINSIC_CALLS.inc();
}

// === Layout result cache ====================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct LayoutCacheKeyNoStyle {
  box_id: usize,
  fc_type: FormattingContextType,
  constraints_hash: u64,
  fragmentation_hash: u64,
  viewport_hash: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct LayoutCacheKeyParts {
  key: LayoutCacheKeyNoStyle,
  style_hash: u64,
  subgrid_dependent: bool,
}

#[derive(Debug, Clone)]
struct LayoutCacheEntry {
  epoch: usize,
  fragment: Arc<FragmentNode>,
  style_hash: u64,
}

/// Number of shards used for the global (cross-thread) layout result cache.
///
/// We keep a small sharded cache in addition to the existing per-thread cache so that layout
/// parallelism doesn't devolve into cache fragmentation. A thread-local cache is still the
/// fast-path (no locking), but a shared cache allows different rayon workers to reuse expensive
/// subtree layouts when they happen to touch the same nodes under identical constraints.
const GLOBAL_LAYOUT_CACHE_SHARDS: usize = 64;

struct ShardedLayoutResultCache {
  shards: [RwLock<FxHashMap<LayoutCacheKeyNoStyle, LayoutCacheEntry>>; GLOBAL_LAYOUT_CACHE_SHARDS],
}

impl ShardedLayoutResultCache {
  fn new() -> Self {
    Self {
      shards: std::array::from_fn(|_| RwLock::new(FxHashMap::default())),
    }
  }

  #[inline]
  fn shard_index_for_box_id(box_id: usize) -> usize {
    box_id % GLOBAL_LAYOUT_CACHE_SHARDS
  }

  #[inline]
  fn get(
    &self,
    key: &LayoutCacheKeyNoStyle,
    epoch: usize,
    style_hash: u64,
  ) -> Option<Arc<FragmentNode>> {
    let shard = &self.shards[Self::shard_index_for_box_id(key.box_id)];
    {
      let map = shard.read();
      if let Some(entry) = map.get(key) {
        if entry.epoch == epoch && entry.style_hash == style_hash {
          return Some(entry.fragment.clone());
        }
        // Stale entries should be rare because we clear on epoch changes, but drop them lazily
        // to avoid unbounded growth if something goes wrong with epoch management.
        if entry.epoch != epoch {
          // Upgrade to write lock outside the read scope.
        } else {
          return None;
        }
      } else {
        return None;
      }
    }

    // Slow path: remove stale entries under a write lock.
    let mut map = shard.write();
    if let Some(entry) = map.get(key) {
      if entry.epoch != epoch {
        map.remove(key);
      } else if entry.epoch == epoch && entry.style_hash == style_hash {
        return Some(entry.fragment.clone());
      }
    }
    None
  }

  #[inline]
  fn insert(&self, key: LayoutCacheKeyNoStyle, entry: LayoutCacheEntry) {
    let shard = &self.shards[Self::shard_index_for_box_id(key.box_id)];
    shard.write().insert(key, entry);
  }

  fn clear(&self) {
    for shard in &self.shards {
      shard.write().clear();
    }
  }

  fn evict_box(&self, box_id: usize) -> usize {
    let shard = &self.shards[Self::shard_index_for_box_id(box_id)];
    let mut map = shard.write();
    let before = map.len();
    map.retain(|key, _| key.box_id != box_id);
    before.saturating_sub(map.len())
  }
}

static GLOBAL_LAYOUT_RESULT_CACHE: LazyLock<ShardedLayoutResultCache> =
  LazyLock::new(ShardedLayoutResultCache::new);
static GLOBAL_LAYOUT_CACHE_EPOCH: AtomicUsize = AtomicUsize::new(0);

/// Default per-thread layout cache entry cap. Override via `FASTR_LAYOUT_CACHE_MAX_ENTRIES`.
const DEFAULT_LAYOUT_CACHE_MAX_ENTRIES: Option<usize> = Some(8192);
/// Upper bound for the adaptive per-thread layout cache entry cap.
///
/// Layout cache entries store `Arc<FragmentNode>` values (deep-cloned subtrees), so keeping the
/// default cap bounded avoids runaway memory usage on extremely large documents. Callers that
/// want to trade memory for reuse can still override this via `FASTR_LAYOUT_CACHE_MAX_ENTRIES`.
const MAX_LAYOUT_CACHE_MAX_ENTRIES: usize = 32_768;
/// Hysteresis window for enforcing the per-thread layout cache entry cap.
///
/// `LAYOUT_RESULT_CACHE` is capped to avoid unbounded growth on large documents, but enforcing the
/// cap *exactly* on every insert causes eviction work to run on essentially every store once the
/// cache becomes full. Instead, we allow the cache to grow slightly past the configured limit and
/// then trim it back down once it exceeds the limit by this many entries.
const LAYOUT_CACHE_EVICTION_BATCH: usize = 64;

thread_local! {
  /// Layout result cache kept per-thread to stay contention-free during rayon-powered fan-out.
  static LAYOUT_RESULT_CACHE: RefCell<FxHashMap<LayoutCacheKeyNoStyle, LayoutCacheEntry>> =
    RefCell::new(FxHashMap::default());
  static LAYOUT_CACHE_ENABLED: Cell<bool> = const { Cell::new(false) };
  static LAYOUT_CACHE_EPOCH: Cell<usize> = const { Cell::new(1) };
  static LAYOUT_CACHE_FRAGMENTATION: Cell<u64> = const { Cell::new(0) };
  static LAYOUT_CACHE_ENTRY_LIMIT: Cell<Option<usize>> = const { Cell::new(None) };
}

fn pack_viewport_size(size: Size) -> u64 {
  let w = size.width.to_bits() as u64;
  let h = size.height.to_bits() as u64;
  (w << 32) | h
}

#[cfg(test)]
thread_local! {
  static SUBGRID_WALK_COUNT: Cell<usize> = const { Cell::new(0) };
}

#[cfg(test)]
fn increment_subgrid_walk_count() {
  SUBGRID_WALK_COUNT.with(|counter| counter.set(counter.get() + 1));
}

#[cfg(test)]
fn reset_subgrid_walk_count() {
  SUBGRID_WALK_COUNT.with(|counter| counter.set(0));
}

#[cfg(test)]
fn subgrid_walk_count() -> usize {
  SUBGRID_WALK_COUNT.with(|counter| counter.get())
}

fn subtree_contains_subgrid_uncached(node: &BoxNode, epoch: usize) -> bool {
  #[cfg(test)]
  increment_subgrid_walk_count();

  if node.style.grid_row_subgrid
    || node.style.grid_column_subgrid
    || !node.style.subgrid_row_line_names.is_empty()
    || !node.style.subgrid_column_line_names.is_empty()
  {
    return true;
  }
  node
    .children
    .iter()
    .any(|child| subtree_contains_subgrid(child, epoch))
}

fn subtree_contains_subgrid(node: &BoxNode, epoch: usize) -> bool {
  let id = node.id();
  if id == 0 {
    return subtree_contains_subgrid_uncached(node, epoch);
  }

  if let Some(cached) = SUBGRID_DEPENDENT_CACHE.with(|cache| {
    cache
      .borrow()
      .get(&id)
      .and_then(|(cached_epoch, contains)| (*cached_epoch == epoch).then_some(*contains))
  }) {
    return cached;
  }

  let contains = subtree_contains_subgrid_uncached(node, epoch);
  SUBGRID_DEPENDENT_CACHE.with(|cache| {
    cache.borrow_mut().insert(id, (epoch, contains));
  });
  contains
}

fn layout_constraints_hash(constraints: &LayoutConstraints) -> u64 {
  let mut h = DefaultHasher::default();
  let hash_f32 = |val: f32, hasher: &mut DefaultHasher| {
    // Canonicalize -0.0 so cache keys don't splinter on harmless sign differences.
    let bits = if val == 0.0 { 0.0f32.to_bits() } else { val.to_bits() };
    bits.hash(hasher);
  };
  let hash_space = |space: &crate::layout::constraints::AvailableSpace,
                    hasher: &mut DefaultHasher| match space {
    crate::layout::constraints::AvailableSpace::Definite(v) => {
      0u8.hash(hasher);
      hash_f32(*v, hasher);
    }
    crate::layout::constraints::AvailableSpace::Indefinite => 1u8.hash(hasher),
    crate::layout::constraints::AvailableSpace::MinContent => 2u8.hash(hasher),
    crate::layout::constraints::AvailableSpace::MaxContent => 3u8.hash(hasher),
  };
  hash_space(&constraints.available_width, &mut h);
  hash_space(&constraints.available_height, &mut h);
  match constraints.used_border_box_width {
    Some(v) => {
      1u8.hash(&mut h);
      hash_f32(v, &mut h);
    }
    None => 0u8.hash(&mut h),
  }
  match constraints.used_border_box_height {
    Some(v) => {
      1u8.hash(&mut h);
      hash_f32(v, &mut h);
    }
    None => 0u8.hash(&mut h),
  }
  match constraints.inline_percentage_base {
    Some(v) => {
      1u8.hash(&mut h);
      hash_f32(v, &mut h);
    }
    None => 0u8.hash(&mut h),
  }
  h.finish()
}

/// Returns a conservative fingerprint for layout cache invalidation.
///
/// FastRender treats each `Arc<ComputedStyle>` allocation as immutable once shared. When layout
/// needs to tweak styles (e.g., for layout-only normalization) it does so via `Arc::make_mut`,
/// which results in a distinct allocation and therefore a distinct pointer for cache keys.
///
/// That lets us use the style pointer itself as a cheap, conservative fingerprint.
pub(crate) fn layout_style_fingerprint(style: &Arc<ComputedStyle>) -> u64 {
  Arc::as_ptr(style) as usize as u64
}

fn fragmentation_fingerprint(options: Option<FragmentationOptions>) -> u64 {
  options
    .map(|opts| {
      let mut h = DefaultHasher::default();
      opts.fragmentainer_size.to_bits().hash(&mut h);
      opts.fragmentainer_gap.to_bits().hash(&mut h);
      opts.column_count.hash(&mut h);
      opts.column_gap.to_bits().hash(&mut h);
      h.finish()
    })
    .unwrap_or(0)
}

fn layout_cache_sync_thread_state() {
  // Pull the run-scoped layout cache configuration from globals into this thread's TLS state.
  let epoch = LAYOUT_CACHE_GLOBAL_EPOCH.load(Ordering::Relaxed).max(1);
  let enabled = LAYOUT_CACHE_GLOBAL_ENABLED.load(Ordering::Relaxed);
  let fragmentation = LAYOUT_CACHE_GLOBAL_FRAGMENTATION.load(Ordering::Relaxed);
  let entry_limit = match LAYOUT_CACHE_GLOBAL_ENTRY_LIMIT.load(Ordering::Relaxed) {
    0 => None,
    v => Some(v),
  };

  let local_epoch = LAYOUT_CACHE_EPOCH.with(|cell| cell.get());
  let local_enabled = LAYOUT_CACHE_ENABLED.with(|cell| cell.get());
  let local_fragmentation = LAYOUT_CACHE_FRAGMENTATION.with(|cell| cell.get());
  let local_entry_limit = LAYOUT_CACHE_ENTRY_LIMIT.with(|cell| cell.get());

  let epoch_changed = local_epoch != epoch;
  let enabled_changed = local_enabled != enabled;
  let fragmentation_changed = local_fragmentation != fragmentation;
  let entry_limit_changed = local_entry_limit != entry_limit;

  if !(epoch_changed || enabled_changed || fragmentation_changed || entry_limit_changed) {
    return;
  }

  // When a new epoch starts (or caching is disabled), drop any cached fragments/fingerprints on
  // this thread. The global epoch is monotonically increasing (see `LayoutCache::start_run`), so
  // this ensures worker-thread TLS caches don't leak across renders even when the engine is
  // recreated per render.
  if epoch_changed || !enabled || fragmentation_changed {
    LAYOUT_RESULT_CACHE.with(|cache| cache.borrow_mut().clear());
    subgrid_cache_use_epoch(epoch, true);
  }

  LAYOUT_CACHE_EPOCH.with(|cell| cell.set(epoch));
  LAYOUT_CACHE_ENABLED.with(|cell| cell.set(enabled));
  LAYOUT_CACHE_FRAGMENTATION.with(|cell| cell.set(fragmentation));
  LAYOUT_CACHE_ENTRY_LIMIT.with(|cell| cell.set(entry_limit));
}

fn is_layout_cacheable(box_node: &BoxNode, fc_type: FormattingContextType) -> bool {
  if !LAYOUT_CACHE_ENABLED.with(|enabled| enabled.get()) {
    return false;
  }
  if box_node.id() == 0 || box_node.is_anonymous() {
    return false;
  }
  if matches!(
    box_node.style.position,
    Position::Absolute | Position::Fixed | Position::Sticky
  ) {
    return false;
  }
  if box_node.style.float != Float::None {
    return false;
  }

  match fc_type {
    FormattingContextType::Inline => false,
    FormattingContextType::Block => {
      crate::layout::contexts::block::margin_collapse::establishes_bfc(&box_node.style)
    }
    FormattingContextType::Flex | FormattingContextType::Grid | FormattingContextType::Table => {
      true
    }
  }
}

fn layout_cache_key(
  box_node: &BoxNode,
  fc_type: FormattingContextType,
  constraints: &LayoutConstraints,
  viewport: Size,
  epoch: usize,
) -> Option<LayoutCacheKeyParts> {
  if !is_layout_cacheable(box_node, fc_type) {
    return None;
  }

  let style_hash = layout_style_fingerprint(&box_node.style);
  let constraints_hash = layout_constraints_hash(constraints);
  let fragmentation_hash = LAYOUT_CACHE_FRAGMENTATION.with(|hash| hash.get());
  Some(LayoutCacheKeyParts {
    key: LayoutCacheKeyNoStyle {
      box_id: box_node.id(),
      fc_type,
      constraints_hash,
      fragmentation_hash,
      viewport_hash: pack_viewport_size(viewport),
    },
    style_hash,
    subgrid_dependent: subtree_contains_subgrid(box_node, epoch),
  })
}

fn resolve_layout_cache_entry_limit(default: Option<usize>) -> Option<usize> {
  let limit = runtime::runtime_toggles()
    .usize("FASTR_LAYOUT_CACHE_MAX_ENTRIES")
    .or(default);
  limit.filter(|limit| *limit > 0)
}

/// Returns a default layout cache entry cap derived from the box tree size.
///
/// Large pages can build many cacheable formatting contexts (notably flex/grid) and will churn the
/// default 8k entry cap, evicting reusable fragments during Taffy measurement passes. Scaling the
/// cap with the overall box count keeps reuse stable on these pages while still bounding memory.
pub(crate) fn layout_cache_entry_limit_for_box_tree(box_tree_nodes: usize) -> Option<usize> {
  let base = DEFAULT_LAYOUT_CACHE_MAX_ENTRIES.unwrap_or(0);
  Some(box_tree_nodes.max(base).min(MAX_LAYOUT_CACHE_MAX_ENTRIES)).filter(|v| *v > 0)
}

fn enforce_layout_cache_entry_limit(
  map: &mut FxHashMap<LayoutCacheKeyNoStyle, LayoutCacheEntry>,
  preserve: &LayoutCacheKeyNoStyle,
) {
  let limit = LAYOUT_CACHE_ENTRY_LIMIT.with(|limit| limit.get());
  let Some(limit) = limit else {
    return;
  };
  // Avoid eviction churn by allowing the cache to exceed the cap by a small amount before we
  // perform cleanup.
  if map.len() <= limit.saturating_add(LAYOUT_CACHE_EVICTION_BATCH) {
    return;
  }

  let over_limit = map.len().saturating_sub(limit);
  if over_limit == 0 {
    return;
  }
  let to_remove = over_limit;
  let mut removed = 0usize;
  let keys: Vec<_> = map
    .keys()
    .filter(|key| *key != preserve)
    .take(to_remove)
    .cloned()
    .collect();
  for key in keys {
    if map.remove(&key).is_some() {
      removed += 1;
    }
    if removed >= to_remove || map.len() <= limit {
      break;
    }
  }

  if removed > 0 {
    LAYOUT_CACHE_EVICTIONS.add(removed);
  }
}

fn layout_cache_clear() {
  LAYOUT_RESULT_CACHE.with(|cache| {
    let mut map = cache.borrow_mut();
    let evicted = map.len();
    if evicted > 0 {
      LAYOUT_CACHE_EVICTIONS.add(evicted);
    }
    map.clear();
  });
}

fn evict_cache_entries_for_box(box_id: usize) {
  LAYOUT_RESULT_CACHE.with(|cache| {
    let mut map = cache.borrow_mut();
    let before = map.len();
    map.retain(|existing_key, _| existing_key.box_id != box_id);
    let evicted = before.saturating_sub(map.len());
    if evicted > 0 {
      LAYOUT_CACHE_EVICTIONS.add(evicted);
    }
  });
  let evicted_global = GLOBAL_LAYOUT_RESULT_CACHE.evict_box(box_id);
  if evicted_global > 0 {
    LAYOUT_CACHE_EVICTIONS.add(evicted_global);
  }
}

pub(crate) fn layout_cache_reset_counters() {
  #[cfg(test)]
  let _guard = layout_cache_test_lock();

  layout_cache_sync_thread_state();
  LAYOUT_RESULT_CACHE.with(|cache| {
    cache.borrow_mut().clear();
  });
  LAYOUT_CACHE_LOOKUPS.store(0);
  LAYOUT_CACHE_HITS.store(0);
  LAYOUT_CACHE_STORES.store(0);
  LAYOUT_CACHE_EVICTIONS.store(0);
  let epoch = LAYOUT_CACHE_EPOCH.with(|cell| cell.get());
  subgrid_cache_use_epoch(epoch, true);
  LAYOUT_CACHE_CLONE_RETURNS.store(0);
}

/// Configures the layout cache epoch and run-scoped parameters.
pub(crate) fn layout_cache_use_epoch(
  epoch: usize,
  enabled: bool,
  reset_counters: bool,
  fragmentation: Option<FragmentationOptions>,
  default_entry_limit: Option<usize>,
) {
  #[cfg(test)]
  let _guard = layout_cache_test_lock();

  let epoch = epoch.max(1);
  let fragmentation_hash = fragmentation_fingerprint(fragmentation);
  let entry_limit = if enabled {
    resolve_layout_cache_entry_limit(default_entry_limit.or(DEFAULT_LAYOUT_CACHE_MAX_ENTRIES))
  } else {
    None
  };

  let previous = LAYOUT_CACHE_EPOCH.with(|cell| cell.get());

  // Global layout cache entries are only valid within a single render/layout run. Clear the shared
  // cache whenever the run epoch changes to avoid unbounded growth.
  let previous_global_epoch = GLOBAL_LAYOUT_CACHE_EPOCH.swap(epoch, Ordering::Relaxed);
  if previous_global_epoch != epoch {
    GLOBAL_LAYOUT_RESULT_CACHE.clear();
  }

  // Publish run configuration so rayon worker threads can observe it.
  LAYOUT_CACHE_GLOBAL_EPOCH.store(epoch, Ordering::Relaxed);
  LAYOUT_CACHE_GLOBAL_ENABLED.store(enabled, Ordering::Relaxed);
  LAYOUT_CACHE_GLOBAL_FRAGMENTATION.store(fragmentation_hash, Ordering::Relaxed);
  LAYOUT_CACHE_GLOBAL_ENTRY_LIMIT.store(entry_limit.unwrap_or(0), Ordering::Relaxed);

  // Sync the calling thread immediately so subsequent cache lookups don't need to pay the sync
  // cost repeatedly.
  layout_cache_sync_thread_state();

  let should_clear = reset_counters || previous != epoch || !enabled;
  if should_clear {
    layout_cache_reset_counters();
    layout_cache_clear();
  } else {
    subgrid_cache_use_epoch(epoch, false);
  }
}

/// Attempts to retrieve a cached layout result for the given formatting context.
pub(crate) fn layout_cache_lookup(
  box_node: &BoxNode,
  fc_type: FormattingContextType,
  constraints: &LayoutConstraints,
  viewport: Size,
) -> Option<FragmentNode> {
  layout_cache_sync_thread_state();
  let epoch = LAYOUT_CACHE_EPOCH.with(|cell| cell.get());
  let key = layout_cache_key(box_node, fc_type, constraints, viewport, epoch)?;
  if key.subgrid_dependent {
    evict_cache_entries_for_box(key.key.box_id);
    return None;
  }
  LAYOUT_CACHE_LOOKUPS.inc();

  let result = LAYOUT_RESULT_CACHE.with(|cache| {
    let mut map = cache.borrow_mut();
    if let Some(entry) = map.get(&key.key) {
      if entry.epoch == epoch && entry.style_hash == key.style_hash {
        return Some(entry.fragment.clone());
      }
      // Drop stale entries tied to an old epoch.
      if entry.epoch != epoch {
        map.remove(&key.key);
        LAYOUT_CACHE_EVICTIONS.inc();
      }
    }
    None
  });

  if let Some(fragment) = result {
    LAYOUT_CACHE_HITS.inc();
    LAYOUT_CACHE_CLONE_RETURNS.inc();
    fragment_clone_profile::record_fragment_clone_from_fragment(
      CloneSite::LayoutCacheHit,
      fragment.as_ref(),
    );
    return Some((*fragment).clone());
  }

  // Second-level cache: cross-thread shared layout results.
  if let Some(fragment) = GLOBAL_LAYOUT_RESULT_CACHE.get(&key.key, epoch, key.style_hash) {
    // Populate the thread-local cache so repeated lookups on this worker don't pay the global lock.
    LAYOUT_RESULT_CACHE.with(|cache| {
      let mut map = cache.borrow_mut();
      let entry = LayoutCacheEntry {
        epoch,
        fragment: fragment.clone(),
        style_hash: key.style_hash,
      };
      map.insert(key.key, entry);
      enforce_layout_cache_entry_limit(&mut map, &key.key);
    });

    LAYOUT_CACHE_HITS.inc();
    LAYOUT_CACHE_CLONE_RETURNS.inc();
    fragment_clone_profile::record_fragment_clone_from_fragment(
      CloneSite::LayoutCacheHit,
      fragment.as_ref(),
    );
    return Some((*fragment).clone());
  }
  None
}

/// Stores a layout result in the cache for reuse within the current epoch.
pub(crate) fn layout_cache_store(
  box_node: &BoxNode,
  fc_type: FormattingContextType,
  constraints: &LayoutConstraints,
  fragment: &FragmentNode,
  viewport: Size,
) {
  layout_cache_sync_thread_state();
  let epoch = LAYOUT_CACHE_EPOCH.with(|cell| cell.get());
  let key = match layout_cache_key(box_node, fc_type, constraints, viewport, epoch) {
    Some(k) => k,
    None => return,
  };
  if key.subgrid_dependent {
    evict_cache_entries_for_box(key.key.box_id);
    return;
  }

  let fragment = Arc::new(fragment.clone());
  let entry = LayoutCacheEntry {
    epoch,
    fragment: fragment.clone(),
    style_hash: key.style_hash,
  };

  LAYOUT_RESULT_CACHE.with(|cache| {
    let mut map = cache.borrow_mut();
    if let Some(previous) = map.insert(key.key, entry.clone()) {
      if previous.epoch != epoch || previous.style_hash != key.style_hash {
        LAYOUT_CACHE_EVICTIONS.inc();
      }
    }
    enforce_layout_cache_entry_limit(&mut map, &key.key);
  });
  GLOBAL_LAYOUT_RESULT_CACHE.insert(key.key, entry);

  LAYOUT_CACHE_STORES.inc();
}

pub(crate) fn layout_cache_stats() -> (usize, usize, usize, usize, usize) {
  (
    LAYOUT_CACHE_LOOKUPS.load_sum(),
    LAYOUT_CACHE_HITS.load_sum(),
    LAYOUT_CACHE_STORES.load_sum(),
    LAYOUT_CACHE_EVICTIONS.load_sum(),
    LAYOUT_CACHE_CLONE_RETURNS.load_sum(),
  )
}

pub(crate) struct FragmentainerBlockSizeHintGuard {
  previous: Option<f32>,
}

impl Drop for FragmentainerBlockSizeHintGuard {
  fn drop(&mut self) {
    FRAGMENTAINER_BLOCK_SIZE_HINT.with(|hint| {
      hint.set(self.previous);
    });
  }
}

pub(crate) fn fragmentainer_block_size_hint() -> Option<f32> {
  FRAGMENTAINER_BLOCK_SIZE_HINT.with(|hint| hint.get())
}

pub(crate) fn set_fragmentainer_block_size_hint(
  hint: Option<f32>,
) -> FragmentainerBlockSizeHintGuard {
  let previous = FRAGMENTAINER_BLOCK_SIZE_HINT.with(|cell| {
    let previous = cell.get();
    cell.set(hint);
    previous
  });
  FragmentainerBlockSizeHintGuard { previous }
}

/// Common trait for all formatting contexts
///
/// This trait abstracts the layout algorithm. Every formatting context
/// (block, inline, flex, grid, table) implements this trait to provide
/// a uniform interface for the layout engine.
///
/// # Contract
///
/// Implementers must:
/// 1. Take a BoxNode and LayoutConstraints as input
/// 2. Recursively layout children using appropriate child formatting contexts
/// 3. Return a positioned FragmentNode tree with final sizes and positions
/// 4. Handle all AvailableSpace variants (Definite, Indefinite, MinContent, MaxContent)
/// 5. Support intrinsic size queries (min-content, max-content)
/// 6. Be stateless and reusable (no mutable state in the FC struct)
///
/// # Thread Safety
///
/// Formatting contexts must be `Send + Sync` to allow:
/// - Sharing across threads
/// - Use in trait objects (`Box<dyn FormattingContext>`)
/// - Parallel layout operations in the future
///
/// # Example Implementation
///
/// ```rust,ignore
/// use fastrender::layout::{FormattingContext, LayoutConstraints, IntrinsicSizingMode, LayoutError};
/// use fastrender::tree::{BoxNode, FragmentNode};
///
/// struct BlockFormattingContext;
///
/// impl FormattingContext for BlockFormattingContext {
///     fn layout(&self, box_node: &BoxNode, constraints: &LayoutConstraints)
///         -> Result<FragmentNode, LayoutError>
///     {
///         // 1. Compute box's own size based on constraints
///         // 2. Layout children vertically
///         // 3. Handle margin collapsing
///         // 4. Return positioned fragment
///         todo!("Implementation in W3.T04")
///     }
///
///     fn compute_intrinsic_inline_size(
///         &self,
///         box_node: &BoxNode,
///         mode: IntrinsicSizingMode,
///     ) -> Result<f32, LayoutError> {
///         // Measure content width
///         todo!("Implementation in W3.T04")
///     }
/// }
/// ```
///
/// # See Also
///
/// - W3.T04: BlockFormattingContext implementation
/// - W4.T12: InlineFormattingContext implementation
/// - W2.T10: LayoutEngine (dispatches to FCs)
pub trait FormattingContext: Send + Sync {
  /// Lays out a box within this formatting context
  ///
  /// This is the main entry point for layout. It takes a box tree node and
  /// available space, performs layout according to this FC's rules, and
  /// returns a positioned fragment tree.
  ///
  /// # Arguments
  ///
  /// * `box_node` - The box to layout (must be compatible with this FC)
  /// * `constraints` - Available space constraints
  ///
  /// # Returns
  ///
  /// A positioned fragment tree with:
  /// - Final size (width and height)
  /// - Final position (relative to parent's content box)
  /// - Laid out children (if any)
  ///
  /// # Errors
  ///
  /// Returns error if:
  /// - Box type is not supported by this formatting context
  /// - Circular dependency detected in sizing
  /// - Required context (fonts, images, etc.) is missing
  fn layout(
    &self,
    box_node: &BoxNode,
    constraints: &LayoutConstraints,
  ) -> Result<FragmentNode, LayoutError>;

  /// Computes intrinsic size for a box in the inline axis
  ///
  /// Used when parent needs to know content-based size before layout.
  /// This is critical for:
  /// - Auto-sizing: `width: auto` on shrink-to-fit boxes
  /// - Flexbox: `flex-basis: content`
  /// - Grid: `grid-template-columns: min-content`
  /// - Table: Auto column width calculation
  ///
  /// # Arguments
  ///
  /// * `box_node` - The box to measure
  /// * `mode` - Whether to compute min-content or max-content size
  ///
  /// # Returns
  ///
  /// Intrinsic inline size in CSS pixels. The inline axis is:
  /// - Horizontal in horizontal writing modes (most common)
  /// - Vertical in vertical writing modes
  ///
  /// # Errors
  ///
  /// Returns error if:
  /// - Content cannot be measured (missing fonts, images, etc.)
  /// - Box type not supported
  ///
  /// # Performance Note
  ///
  /// This method may be called multiple times during layout (e.g., for table
  /// column sizing). Implementations should consider caching if expensive.
  fn compute_intrinsic_inline_size(
    &self,
    box_node: &BoxNode,
    mode: IntrinsicSizingMode,
  ) -> Result<f32, LayoutError>;

  /// Computes both the min-content and max-content intrinsic inline sizes for a box.
  ///
  /// Shrink-to-fit algorithms typically need both values (floats, inline-blocks, abspos). Default
  /// implementation calls `compute_intrinsic_inline_size` twice, but formatting contexts can
  /// override this to share work between the two measurements.
  fn compute_intrinsic_inline_sizes(&self, box_node: &BoxNode) -> Result<(f32, f32), LayoutError> {
    let min = self.compute_intrinsic_inline_size(box_node, IntrinsicSizingMode::MinContent)?;
    let max = match self.compute_intrinsic_inline_size(box_node, IntrinsicSizingMode::MaxContent) {
      Ok(value) => value,
      Err(err @ LayoutError::Timeout { .. }) => return Err(err),
      Err(_) => min,
    };
    Ok((min, max))
  }

  /// Computes intrinsic size for a box in the block axis
  ///
  /// Used for shrink-to-fit resolution of absolutely positioned height when
  /// both vertical insets are specified, mirroring the inline shrink path.
  fn compute_intrinsic_block_size(
    &self,
    box_node: &BoxNode,
    mode: IntrinsicSizingMode,
  ) -> Result<f32, LayoutError> {
    if let Some(cached) = intrinsic_block_cache_lookup(box_node, mode) {
      return Ok(cached);
    }

    // The intrinsic block-size depends on layout in the opposite axis. Mirror the intrinsic inline
    // size probes by laying out the box with an intrinsic constraint in the inline axis and an
    // indefinite constraint in the block axis, then returning the resulting border-box block size.
    let inline_is_horizontal = crate::style::inline_axis_is_horizontal(box_node.style.writing_mode);
    let intrinsic_inline_space = match mode {
      IntrinsicSizingMode::MinContent => AvailableSpace::MinContent,
      IntrinsicSizingMode::MaxContent => AvailableSpace::MaxContent,
    };
    let constraints = if inline_is_horizontal {
      LayoutConstraints::new(intrinsic_inline_space, AvailableSpace::Indefinite)
    } else {
      LayoutConstraints::new(AvailableSpace::Indefinite, intrinsic_inline_space)
    };
    let fragment = self.layout(box_node, &constraints)?;
    let block_size = if inline_is_horizontal {
      fragment.bounds.height()
    } else {
      fragment.bounds.width()
    };
    intrinsic_block_cache_store(box_node, mode, block_size);
    Ok(block_size)
  }
}

pub(crate) fn intrinsic_block_cache_lookup(
  node: &BoxNode,
  mode: IntrinsicSizingMode,
) -> Option<f32> {
  let epoch = CACHE_EPOCH.load(Ordering::Relaxed);
  ensure_intrinsic_thread_epoch(epoch);
  let id = node.id();
  if let Some(style_override) = crate::layout::style_override::style_override_for(id) {
    let key = override_cache_key(node, mode, epoch, style_override.as_ref())?;
    return INTRINSIC_BLOCK_OVERRIDE_CACHE.with(|cache| {
      cache
        .borrow()
        .get(&key)
        .and_then(|(entry_epoch, value)| (*entry_epoch == epoch).then_some(*value))
    });
  }

  let key = cache_key(node, mode, epoch)?;
  if let Some(hit) = INTRINSIC_BLOCK_CACHE_TL.with(|cache| cache.borrow().get(&key).copied()) {
    return Some(hit);
  }

  let hit = GLOBAL_INTRINSIC_BLOCK_CACHE.get(&key, epoch)?;
  INTRINSIC_BLOCK_CACHE_TL.with(|cache| {
    cache.borrow_mut().insert(key, hit);
  });
  Some(hit)
}

pub(crate) fn intrinsic_block_cache_store(node: &BoxNode, mode: IntrinsicSizingMode, value: f32) {
  let epoch = CACHE_EPOCH.load(Ordering::Relaxed);
  ensure_intrinsic_thread_epoch(epoch);
  let id = node.id();
  if let Some(style_override) = crate::layout::style_override::style_override_for(id) {
    if let Some(key) = override_cache_key(node, mode, epoch, style_override.as_ref()) {
      INTRINSIC_BLOCK_OVERRIDE_CACHE.with(|cache| {
        cache.borrow_mut().insert(key, (epoch, value));
      });
    }
    return;
  }

  if let Some(key) = cache_key(node, mode, epoch) {
    INTRINSIC_BLOCK_CACHE_TL.with(|cache| {
      cache.borrow_mut().insert(key, value);
    });
    GLOBAL_INTRINSIC_BLOCK_CACHE.insert(key, epoch, value);
  }
}

/// Layout errors
///
/// Errors that can occur during layout operations.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LayoutError {
  /// Box type not supported by this formatting context
  ///
  /// For example, trying to layout a table box with BlockFormattingContext.
  UnsupportedBoxType(String),

  /// Circular dependency in sizing
  ///
  /// Occurs when box's size depends on itself, creating infinite recursion.
  /// Example: percentage height when parent height depends on child height.
  CircularDependency,

  /// Missing required context
  ///
  /// Required resources (fonts, images, etc.) are not available.
  MissingContext(String),

  /// Layout aborted due to timeout or cancellation.
  Timeout { elapsed: Duration },
}

impl std::fmt::Display for LayoutError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::UnsupportedBoxType(msg) => write!(f, "Unsupported box type: {}", msg),
      Self::CircularDependency => write!(f, "Circular dependency in layout"),
      Self::MissingContext(msg) => write!(f, "Missing context: {}", msg),
      Self::Timeout { elapsed } => write!(f, "Layout timed out after {elapsed:?}"),
    }
  }
}

impl std::error::Error for LayoutError {}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::geometry::{Rect, Size};
  use crate::style::display::{Display, FormattingContextType};
  use crate::style::values::Length;
  use crate::style::ComputedStyle;
  use rayon::ThreadPoolBuilder;
  use std::sync::atomic::Ordering;
  use std::sync::Arc;
  use std::time::Duration;

  /// Stub formatting context for testing trait requirements
  ///
  /// This demonstrates the minimal implementation needed to satisfy
  /// the FormattingContext trait contract.
  #[derive(Debug)]
  struct StubFormattingContext;

  impl FormattingContext for StubFormattingContext {
    fn layout(
      &self,
      _box_node: &BoxNode,
      _constraints: &LayoutConstraints,
    ) -> Result<FragmentNode, LayoutError> {
      // Stub: just return a fixed-size fragment
      Ok(FragmentNode::new_block(
        Rect::from_xywh(0.0, 0.0, 100.0, 50.0),
        vec![],
      ))
    }

    fn compute_intrinsic_inline_size(
      &self,
      _box_node: &BoxNode,
      _mode: IntrinsicSizingMode,
    ) -> Result<f32, LayoutError> {
      // Stub: return fixed width
      Ok(100.0)
    }
  }

  #[test]
  fn test_stub_fc_implements_trait() {
    let fc = StubFormattingContext;
    let style = Arc::new(ComputedStyle::default());
    let box_node = BoxNode::new_block(style, FormattingContextType::Block, vec![]);

    let constraints = LayoutConstraints::definite(800.0, 600.0);
    let fragment = fc.layout(&box_node, &constraints).unwrap();

    assert_eq!(fragment.bounds.width(), 100.0);
    assert_eq!(fragment.bounds.height(), 50.0);
  }

  #[test]
  fn test_intrinsic_sizing_min_content() {
    let fc = StubFormattingContext;
    let style = Arc::new(ComputedStyle::default());
    let box_node = BoxNode::new_block(style, FormattingContextType::Block, vec![]);

    let size = fc
      .compute_intrinsic_inline_size(&box_node, IntrinsicSizingMode::MinContent)
      .unwrap();

    assert_eq!(size, 100.0);
  }

  #[test]
  fn test_intrinsic_sizing_max_content() {
    let fc = StubFormattingContext;
    let style = Arc::new(ComputedStyle::default());
    let box_node = BoxNode::new_block(style, FormattingContextType::Block, vec![]);

    let size = fc
      .compute_intrinsic_inline_size(&box_node, IntrinsicSizingMode::MaxContent)
      .unwrap();

    assert_eq!(size, 100.0);
  }

  #[test]
  fn test_layout_with_definite_constraints() {
    let fc = StubFormattingContext;
    let style = Arc::new(ComputedStyle::default());
    let box_node = BoxNode::new_block(style, FormattingContextType::Block, vec![]);

    let constraints = LayoutConstraints::definite(1024.0, 768.0);
    let fragment = fc.layout(&box_node, &constraints).unwrap();

    // Fragment should be positioned at origin
    assert_eq!(fragment.bounds.x(), 0.0);
    assert_eq!(fragment.bounds.y(), 0.0);
  }

  #[test]
  fn test_layout_with_indefinite_constraints() {
    let fc = StubFormattingContext;
    let style = Arc::new(ComputedStyle::default());
    let box_node = BoxNode::new_block(style, FormattingContextType::Block, vec![]);

    let constraints = LayoutConstraints::indefinite();
    let fragment = fc.layout(&box_node, &constraints).unwrap();

    // Should still produce valid fragment
    assert!(fragment.bounds.width() > 0.0);
  }

  #[test]
  fn test_layout_error_display() {
    let err = LayoutError::UnsupportedBoxType("test".to_string());
    assert_eq!(err.to_string(), "Unsupported box type: test");

    let err = LayoutError::CircularDependency;
    assert_eq!(err.to_string(), "Circular dependency in layout");

    let err = LayoutError::MissingContext("fonts".to_string());
    assert_eq!(err.to_string(), "Missing context: fonts");
  }

  #[test]
  fn test_intrinsic_sizing_mode_equality() {
    assert_eq!(
      IntrinsicSizingMode::MinContent,
      IntrinsicSizingMode::MinContent
    );
    assert_eq!(
      IntrinsicSizingMode::MaxContent,
      IntrinsicSizingMode::MaxContent
    );
    assert_ne!(
      IntrinsicSizingMode::MinContent,
      IntrinsicSizingMode::MaxContent
    );
  }

  #[test]
  fn test_formatting_context_is_send_sync() {
    // Verify that our trait requires Send + Sync
    fn assert_send_sync<T: Send + Sync>() {}
    assert_send_sync::<StubFormattingContext>();
  }

  #[test]
  fn intrinsic_cache_epoch_invalidation() {
    let _guard = intrinsic_cache_test_lock();
    intrinsic_cache_use_epoch(1, true);

    let mut node = BoxNode::new_block(
      Arc::new(ComputedStyle::default()),
      FormattingContextType::Block,
      vec![],
    );
    node.id = 1;

    intrinsic_cache_store(&node, IntrinsicSizingMode::MinContent, 5.0);
    assert_eq!(
      intrinsic_cache_lookup(&node, IntrinsicSizingMode::MinContent),
      Some(5.0)
    );

    intrinsic_cache_use_epoch(2, false);
    assert_eq!(
      intrinsic_cache_lookup(&node, IntrinsicSizingMode::MinContent),
      None
    );

    intrinsic_cache_use_epoch(1, true);
  }

  #[test]
  fn subgrid_memoization_matches_recursive_and_reuses() {
    let _guard = intrinsic_cache_test_lock();
    intrinsic_cache_use_epoch(1, true);
    reset_subgrid_walk_count();

    let mut subgrid_style = ComputedStyle::default();
    subgrid_style.grid_row_subgrid = true;

    let mut leaf = BoxNode::new_block(
      Arc::new(subgrid_style),
      FormattingContextType::Block,
      vec![],
    );
    leaf.id = 3;

    let mut middle = BoxNode::new_block(
      Arc::new(ComputedStyle::default()),
      FormattingContextType::Block,
      vec![leaf],
    );
    middle.id = 2;

    let mut root = BoxNode::new_block(
      Arc::new(ComputedStyle::default()),
      FormattingContextType::Block,
      vec![middle],
    );
    root.id = 1;

    let epoch = CACHE_EPOCH.load(Ordering::Relaxed);
    let memoized = subtree_contains_subgrid(&root, epoch);
    let raw = {
      fn raw_scan(node: &BoxNode) -> bool {
        if node.style.grid_row_subgrid
          || node.style.grid_column_subgrid
          || !node.style.subgrid_row_line_names.is_empty()
          || !node.style.subgrid_column_line_names.is_empty()
        {
          return true;
        }
        node.children.iter().any(raw_scan)
      }
      raw_scan(&root)
    };
    assert_eq!(memoized, raw);
    assert_eq!(subgrid_walk_count(), 3);

    reset_subgrid_walk_count();
    assert_eq!(subtree_contains_subgrid(&root, epoch), memoized);
    assert_eq!(subgrid_walk_count(), 0);
  }

  #[test]
  fn subgrid_memoization_invalidation_on_epoch_change() {
    let _guard = intrinsic_cache_test_lock();
    intrinsic_cache_use_epoch(7, true);

    let child = BoxNode::new_block(
      Arc::new(ComputedStyle::default()),
      FormattingContextType::Block,
      vec![],
    );
    let mut root = BoxNode::new_block(
      Arc::new(ComputedStyle::default()),
      FormattingContextType::Block,
      vec![child],
    );
    root.id = 1;
    root.children[0].id = 2;

    let epoch = CACHE_EPOCH.load(Ordering::Relaxed);
    reset_subgrid_walk_count();
    assert!(!subtree_contains_subgrid(&root, epoch));
    assert_eq!(subgrid_walk_count(), 2);

    let mut new_style = ComputedStyle::default();
    new_style.grid_column_subgrid = true;
    root.children[0].style = Arc::new(new_style);

    reset_subgrid_walk_count();
    assert!(!subtree_contains_subgrid(&root, epoch));
    assert_eq!(subgrid_walk_count(), 0);

    intrinsic_cache_use_epoch(epoch + 1, false);
    let new_epoch = CACHE_EPOCH.load(Ordering::Relaxed);
    reset_subgrid_walk_count();
    assert!(subtree_contains_subgrid(&root, new_epoch));
    assert!(subgrid_walk_count() > 0);

    intrinsic_cache_use_epoch(1, true);
  }

  #[test]
  fn layout_epoch_clears_subgrid_cache() {
    let _guard = layout_cache_test_lock();
    layout_cache_use_epoch(9, false, true, None, None);

    let child = BoxNode::new_block(
      Arc::new(ComputedStyle::default()),
      FormattingContextType::Block,
      vec![],
    );
    let mut root = BoxNode::new_block(
      Arc::new(ComputedStyle::default()),
      FormattingContextType::Block,
      vec![child],
    );
    root.id = 10;
    root.children[0].id = 11;

    let epoch = LAYOUT_CACHE_EPOCH.with(|cell| cell.get());
    reset_subgrid_walk_count();
    assert!(!subtree_contains_subgrid(&root, epoch));
    assert_eq!(subgrid_walk_count(), 2);

    let mut subgrid_style = ComputedStyle::default();
    subgrid_style.grid_row_subgrid = true;
    root.children[0].style = Arc::new(subgrid_style);

    reset_subgrid_walk_count();
    assert!(!subtree_contains_subgrid(&root, epoch));
    assert_eq!(subgrid_walk_count(), 0);

    layout_cache_use_epoch(epoch + 1, false, true, None, None);
    let new_epoch = LAYOUT_CACHE_EPOCH.with(|cell| cell.get());
    reset_subgrid_walk_count();
    assert!(subtree_contains_subgrid(&root, new_epoch));
    assert!(subgrid_walk_count() > 0);

    layout_cache_use_epoch(1, false, true, None, None);
  }

  #[test]
  fn layout_cache_entry_limit_scales_with_box_tree_size() {
    assert_eq!(
      layout_cache_entry_limit_for_box_tree(1),
      DEFAULT_LAYOUT_CACHE_MAX_ENTRIES,
      "small trees should still use the baseline entry cap"
    );
    assert_eq!(
      layout_cache_entry_limit_for_box_tree(10_000),
      Some(10_000),
      "large trees should raise the cap to avoid churn"
    );
    assert_eq!(
      layout_cache_entry_limit_for_box_tree(1_000_000),
      Some(MAX_LAYOUT_CACHE_MAX_ENTRIES),
      "adaptive cap should be bounded to avoid runaway memory usage"
    );
  }

  fn flow_root_style() -> Arc<ComputedStyle> {
    let mut style = ComputedStyle::default();
    style.display = Display::FlowRoot;
    Arc::new(style)
  }

  fn block_fragment(width: f32, height: f32) -> FragmentNode {
    FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, width, height), vec![])
  }

  #[test]
  fn layout_cache_hits_on_stable_style() {
    let _guard = layout_cache_test_lock();
    layout_cache_use_epoch(1, true, true, None, None);

    let mut node = BoxNode::new_block(flow_root_style(), FormattingContextType::Block, vec![]);
    node.id = 1;
    let constraints = LayoutConstraints::definite(800.0, 600.0);
    let viewport = Size::new(800.0, 600.0);
    let fc_type = FormattingContextType::Block;

    assert!(layout_cache_lookup(&node, fc_type, &constraints, viewport).is_none());
    layout_cache_store(
      &node,
      fc_type,
      &constraints,
      &block_fragment(100.0, 50.0),
      viewport,
    );
    let hit = layout_cache_lookup(&node, fc_type, &constraints, viewport);
    assert!(hit.is_some());

    let (lookups, hits, stores, evictions, _clones) = layout_cache_stats();
    assert_eq!(lookups, 2);
    assert_eq!(hits, 1);
    assert_eq!(stores, 1);
    assert_eq!(evictions, 0);

    layout_cache_use_epoch(1, false, true, None, None);
  }

  #[test]
  fn layout_cache_entry_limit_is_enforced_in_batches() {
    let _guard = layout_cache_test_lock();
    layout_cache_use_epoch(1, true, true, None, None);

    // Use a tiny entry cap so the hysteresis window triggers quickly.
    let entry_limit = 2usize;
    LAYOUT_CACHE_ENTRY_LIMIT.with(|cell| cell.set(Some(entry_limit)));

    let preserve = LayoutCacheKeyNoStyle {
      box_id: 0,
      fc_type: FormattingContextType::Block,
      constraints_hash: 0,
      fragmentation_hash: 0,
      viewport_hash: 0,
    };
    let entry = LayoutCacheEntry {
      epoch: 1,
      fragment: Arc::new(block_fragment(1.0, 1.0)),
      style_hash: 0,
    };

    let mut map = FxHashMap::default();
    for i in 0..entry_limit.saturating_add(LAYOUT_CACHE_EVICTION_BATCH) {
      let key = LayoutCacheKeyNoStyle {
        box_id: i,
        fc_type: FormattingContextType::Block,
        constraints_hash: 0,
        fragmentation_hash: 0,
        viewport_hash: 0,
      };
      map.insert(key, entry.clone());
    }

    assert_eq!(map.len(), entry_limit + LAYOUT_CACHE_EVICTION_BATCH);
    enforce_layout_cache_entry_limit(&mut map, &preserve);
    assert_eq!(
      map.len(),
      entry_limit + LAYOUT_CACHE_EVICTION_BATCH,
      "cache should not trim until it exceeds the eviction hysteresis window"
    );

    // Add one more entry to push the cache over the hysteresis threshold.
    let extra_key = LayoutCacheKeyNoStyle {
      box_id: 99999,
      fc_type: FormattingContextType::Block,
      constraints_hash: 0,
      fragmentation_hash: 0,
      viewport_hash: 0,
    };
    map.insert(extra_key, entry);
    enforce_layout_cache_entry_limit(&mut map, &preserve);
    assert!(
      map.len() <= entry_limit,
      "expected cache trim back to cap (len={} limit={entry_limit})",
      map.len()
    );
    assert!(
      map.contains_key(&preserve),
      "cache eviction should never remove the preserved key"
    );

    layout_cache_use_epoch(1, false, true, None, None);
  }

  #[test]
  fn layout_cache_misses_on_constraint_change() {
    let _guard = layout_cache_test_lock();
    layout_cache_use_epoch(1, true, true, None, None);

    let mut node = BoxNode::new_block(flow_root_style(), FormattingContextType::Block, vec![]);
    node.id = 1;
    let viewport = Size::new(800.0, 600.0);
    let fc_type = FormattingContextType::Block;

    let constraints_a = LayoutConstraints::definite(800.0, 600.0);
    let constraints_b = LayoutConstraints::definite(640.0, 480.0);

    layout_cache_store(
      &node,
      fc_type,
      &constraints_a,
      &block_fragment(100.0, 50.0),
      viewport,
    );
    assert!(
      layout_cache_lookup(&node, fc_type, &constraints_b, viewport).is_none(),
      "changing constraints must invalidate the cached entry"
    );

    layout_cache_use_epoch(1, false, true, None, None);
  }

  #[test]
  fn layout_cache_does_not_cross_contaminate_nodes() {
    let _guard = layout_cache_test_lock();
    layout_cache_use_epoch(1, true, true, None, None);

    let mut node_a = BoxNode::new_block(flow_root_style(), FormattingContextType::Block, vec![]);
    node_a.id = 1;
    let mut node_b = BoxNode::new_block(flow_root_style(), FormattingContextType::Block, vec![]);
    node_b.id = 2;

    let constraints = LayoutConstraints::definite(800.0, 600.0);
    let viewport = Size::new(800.0, 600.0);
    let fc_type = FormattingContextType::Block;

    layout_cache_store(
      &node_a,
      fc_type,
      &constraints,
      &block_fragment(100.0, 50.0),
      viewport,
    );

    assert!(
      layout_cache_lookup(&node_b, fc_type, &constraints, viewport).is_none(),
      "layout cache entries must not be reused across different box ids"
    );

    layout_cache_use_epoch(1, false, true, None, None);
  }

  #[test]
  fn layout_cache_is_available_from_rayon_worker_threads() {
    // Regression test: layout cache enablement/epoch configuration is stored in TLS, but layout
    // itself can fan out to rayon worker threads. The cache must be usable from those workers
    // without requiring each thread to call `layout_cache_use_epoch` manually.
    let _guard = layout_cache_test_lock();
    layout_cache_use_epoch(1, true, true, None, None);

    let mut node = BoxNode::new_block(flow_root_style(), FormattingContextType::Flex, vec![]);
    node.id = 1;
    let constraints = LayoutConstraints::definite(800.0, 600.0);
    let viewport = Size::new(800.0, 600.0);

    let pool = ThreadPoolBuilder::new()
      .num_threads(1)
      .build()
      .expect("build rayon pool");

    let (tx, rx) = std::sync::mpsc::channel();
    pool.spawn(move || {
      assert!(
        layout_cache_lookup(&node, FormattingContextType::Flex, &constraints, viewport).is_none(),
        "cache should be empty before store"
      );
      let fragment = block_fragment(123.0, 45.0);
      layout_cache_store(
        &node,
        FormattingContextType::Flex,
        &constraints,
        &fragment,
        viewport,
      );
      tx.send(
        layout_cache_lookup(&node, FormattingContextType::Flex, &constraints, viewport).is_some(),
      )
      .expect("send result");
    });
    assert!(
      rx.recv_timeout(Duration::from_secs(1))
        .expect("receive worker result"),
      "expected worker thread to observe layout cache hit after store"
    );

    layout_cache_use_epoch(1, false, true, None, None);
  }

  #[test]
  fn layout_cache_overwrites_style_changes() {
    let _guard = layout_cache_test_lock();
    layout_cache_use_epoch(1, true, true, None, None);

    let mut node = BoxNode::new_block(flow_root_style(), FormattingContextType::Block, vec![]);
    node.id = 1;
    let constraints = LayoutConstraints::definite(640.0, 480.0);
    let viewport = Size::new(640.0, 480.0);
    let fc_type = FormattingContextType::Block;

    layout_cache_store(
      &node,
      fc_type,
      &constraints,
      &block_fragment(50.0, 20.0),
      viewport,
    );
    LAYOUT_RESULT_CACHE.with(|cache| assert_eq!(cache.borrow().len(), 1));

    let mut style = flow_root_style().as_ref().clone();
    style.margin_left = Some(Length::px(10.0));
    let mut changed_node = BoxNode::new_block(Arc::new(style), fc_type, vec![]);
    changed_node.id = node.id;

    assert!(layout_cache_lookup(&changed_node, fc_type, &constraints, viewport).is_none());
    layout_cache_store(
      &changed_node,
      fc_type,
      &constraints,
      &block_fragment(60.0, 30.0),
      viewport,
    );

    LAYOUT_RESULT_CACHE.with(|cache| assert_eq!(cache.borrow().len(), 1));

    let updated = layout_cache_lookup(&changed_node, fc_type, &constraints, viewport).unwrap();
    assert_eq!(updated.bounds.width(), 60.0);

    let (lookups, hits, stores, evictions, _clones) = layout_cache_stats();
    assert_eq!(lookups, 2);
    assert_eq!(hits, 1);
    assert_eq!(stores, 2);
    assert_eq!(evictions, 1);

    layout_cache_use_epoch(1, false, true, None, None);
  }

  #[test]
  fn layout_cache_evicted_on_epoch_change() {
    let _guard = layout_cache_test_lock();
    layout_cache_use_epoch(1, true, true, None, None);

    let mut node = BoxNode::new_block(flow_root_style(), FormattingContextType::Block, vec![]);
    node.id = 1;
    let constraints = LayoutConstraints::definite(320.0, 240.0);
    let viewport = Size::new(320.0, 240.0);
    let fc_type = FormattingContextType::Block;

    layout_cache_store(
      &node,
      fc_type,
      &constraints,
      &block_fragment(40.0, 10.0),
      viewport,
    );
    // Simulate a new layout run starting on another thread by bumping the global epoch.
    // Worker threads should observe the change and drop any stale TLS cache entries.
    LAYOUT_CACHE_GLOBAL_EPOCH.store(2, Ordering::Relaxed);

    assert!(layout_cache_lookup(&node, fc_type, &constraints, viewport).is_none());
    LAYOUT_RESULT_CACHE.with(|cache| assert!(cache.borrow().is_empty()));

    let (lookups, hits, stores, evictions, _clones) = layout_cache_stats();
    assert_eq!(lookups, 1);
    assert_eq!(hits, 0);
    assert_eq!(stores, 1);
    // Epoch transitions happen at layout-run boundaries where counters are reset, so we do not
    // count stale entries from previous epochs as evictions in the current run.
    assert_eq!(evictions, 0);

    layout_cache_use_epoch(1, false, true, None, None);
  }
}
