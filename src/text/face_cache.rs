use crate::text::font_db::LoadedFont;
use lru::LruCache;
use parking_lot::Mutex;
use rustc_hash::FxHasher;
use rustybuzz::Face as RbFace;
use std::hash::{BuildHasherDefault, Hash, Hasher};
use std::num::NonZeroUsize;
use std::sync::{Arc, OnceLock};

#[cfg(debug_assertions)]
use std::cell::Cell;
#[cfg(debug_assertions)]
use std::sync::atomic::{AtomicU64, Ordering};

/// Maximum number of parsed faces kept in memory at once.
///
/// The cache is keyed by the underlying Arc pointer + font index, so different
/// handles to the same font bytes share entries.
const FACE_CACHE_SIZE: usize = 256;
const RUSTYBUZZ_FACE_CACHE_SIZE: usize = 256;
const FACE_CACHE_SHARD_TARGET: usize = 32;
const FACE_CACHE_SHARD_LIMIT: usize = 16;
type FaceCacheHasher = BuildHasherDefault<FxHasher>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct FaceCacheKey {
  font_ptr: usize,
  font_index: u32,
}

impl FaceCacheKey {
  fn new(data: &Arc<Vec<u8>>, index: u32) -> Self {
    Self {
      font_ptr: Arc::as_ptr(data) as usize,
      font_index: index,
    }
  }
}

#[derive(Clone)]
pub struct CachedFace {
  data: Arc<Vec<u8>>,
  face: ttf_parser::Face<'static>,
}

impl CachedFace {
  fn parse(data: Arc<Vec<u8>>, index: u32) -> Result<Self, ttf_parser::FaceParsingError> {
    // SAFETY: the Arc keeps the font data alive for the lifetime of the cached face.
    let static_data: &'static [u8] = unsafe { std::mem::transmute::<&[u8], &'static [u8]>(&*data) };
    record_face_parse();
    let face = ttf_parser::Face::parse(static_data, index)?;
    Ok(Self { data, face })
  }

  #[inline]
  pub fn face(&self) -> &ttf_parser::Face<'static> {
    &self.face
  }

  #[inline]
  pub fn clone_face(&self) -> ttf_parser::Face<'static> {
    self.face.clone()
  }

  #[inline]
  pub fn data(&self) -> Arc<Vec<u8>> {
    Arc::clone(&self.data)
  }

  #[inline]
  pub fn has_glyph(&self, c: char) -> bool {
    self.face.glyph_index(c).is_some()
  }
}

#[derive(Clone)]
pub struct CachedRustybuzzFace {
  data: Arc<Vec<u8>>,
  face: Arc<RbFace<'static>>,
}

impl CachedRustybuzzFace {
  fn parse(data: Arc<Vec<u8>>, index: u32) -> Option<Self> {
    // SAFETY: the Arc keeps the font data alive for the lifetime of the cached face.
    let static_data: &'static [u8] = unsafe { std::mem::transmute::<&[u8], &'static [u8]>(&*data) };
    record_rustybuzz_face_parse();
    let face = RbFace::from_slice(static_data, index)?;
    Some(Self {
      data,
      face: Arc::new(face),
    })
  }

  #[inline]
  pub fn face(&self) -> Arc<RbFace<'static>> {
    Arc::clone(&self.face)
  }

  #[inline]
  pub fn clone_face(&self) -> RbFace<'static> {
    (*self.face).clone()
  }
}

/// Global parsed face cache.
///
/// The cache is sharded to reduce mutex contention during parallel shaping/layout.
/// LRU operations require mutable access, so each shard is guarded by a mutex and
/// critical sections are kept short by parsing outside the lock.
#[derive(Clone)]
struct FaceCache {
  inner: Arc<FaceCacheInner>,
}

#[derive(Debug)]
struct FaceCacheInner {
  shards: Vec<Mutex<LruCache<FaceCacheKey, Arc<CachedFace>, FaceCacheHasher>>>,
  shard_mask: usize,
}

impl FaceCache {
  fn new(capacity: usize) -> Self {
    let capacity = capacity.max(1);
    let desired_shards = (capacity / FACE_CACHE_SHARD_TARGET).clamp(1, FACE_CACHE_SHARD_LIMIT);
    let shard_count = desired_shards.next_power_of_two().min(FACE_CACHE_SHARD_LIMIT);
    let shard_count = shard_count.max(1);
    let shard_mask = shard_count - 1;

    let base = capacity / shard_count;
    let rem = capacity % shard_count;
    let mut shards = Vec::with_capacity(shard_count);
    for idx in 0..shard_count {
      let shard_cap = base + usize::from(idx < rem);
      let cap = NonZeroUsize::new(shard_cap.max(1)).unwrap();
      shards.push(Mutex::new(LruCache::with_hasher(
        cap,
        FaceCacheHasher::default(),
      )));
    }

    Self {
      inner: Arc::new(FaceCacheInner { shards, shard_mask }),
    }
  }

  #[inline]
  fn shard_index(&self, key: &FaceCacheKey) -> usize {
    let mut hasher = FxHasher::default();
    key.hash(&mut hasher);
    (hasher.finish() as usize) & self.inner.shard_mask
  }

  fn get_or_parse(&self, data: Arc<Vec<u8>>, index: u32) -> Option<Arc<CachedFace>> {
    let key = FaceCacheKey::new(&data, index);
    let shard_idx = self.shard_index(&key);

    {
      let mut cache = self.inner.shards[shard_idx].lock();
      if let Some(face) = cache.get(&key) {
        return Some(face.clone());
      }
    }

    let parsed = Arc::new(CachedFace::parse(data, index).ok()?);

    {
      let mut cache = self.inner.shards[shard_idx].lock();
      if let Some(face) = cache.get(&key) {
        return Some(face.clone());
      }
      cache.put(key, parsed.clone());
    }

    Some(parsed)
  }
}

/// Global rustybuzz face cache.
///
/// Shares the same keying and eviction strategy as the ttf-parser cache to
/// avoid reparsing HarfBuzz faces for each shaping run. Like `FaceCache`, this
/// cache is sharded to reduce lock contention under parallel shaping.
#[derive(Clone)]
struct RustybuzzFaceCache {
  inner: Arc<RustybuzzFaceCacheInner>,
}

#[derive(Debug)]
struct RustybuzzFaceCacheInner {
  shards: Vec<Mutex<LruCache<FaceCacheKey, Arc<CachedRustybuzzFace>, FaceCacheHasher>>>,
  shard_mask: usize,
}

impl RustybuzzFaceCache {
  fn new(capacity: usize) -> Self {
    let capacity = capacity.max(1);
    let desired_shards = (capacity / FACE_CACHE_SHARD_TARGET).clamp(1, FACE_CACHE_SHARD_LIMIT);
    let shard_count = desired_shards.next_power_of_two().min(FACE_CACHE_SHARD_LIMIT);
    let shard_count = shard_count.max(1);
    let shard_mask = shard_count - 1;

    let base = capacity / shard_count;
    let rem = capacity % shard_count;
    let mut shards = Vec::with_capacity(shard_count);
    for idx in 0..shard_count {
      let shard_cap = base + usize::from(idx < rem);
      let cap = NonZeroUsize::new(shard_cap.max(1)).unwrap();
      shards.push(Mutex::new(LruCache::with_hasher(
        cap,
        FaceCacheHasher::default(),
      )));
    }

    Self {
      inner: Arc::new(RustybuzzFaceCacheInner { shards, shard_mask }),
    }
  }

  #[inline]
  fn shard_index(&self, key: &FaceCacheKey) -> usize {
    let mut hasher = FxHasher::default();
    key.hash(&mut hasher);
    (hasher.finish() as usize) & self.inner.shard_mask
  }

  fn get_or_parse(&self, data: Arc<Vec<u8>>, index: u32) -> Option<Arc<CachedRustybuzzFace>> {
    let key = FaceCacheKey::new(&data, index);
    let shard_idx = self.shard_index(&key);

    {
      let mut cache = self.inner.shards[shard_idx].lock();
      if let Some(face) = cache.get(&key) {
        return Some(face.clone());
      }
    }

    let parsed = Arc::new(CachedRustybuzzFace::parse(data, index)?);

    {
      let mut cache = self.inner.shards[shard_idx].lock();
      if let Some(face) = cache.get(&key) {
        return Some(face.clone());
      }
      cache.put(key, parsed.clone());
    }

    Some(parsed)
  }
}

fn shared_face_cache() -> &'static FaceCache {
  static FACE_CACHE: OnceLock<FaceCache> = OnceLock::new();
  FACE_CACHE.get_or_init(|| FaceCache::new(FACE_CACHE_SIZE))
}

fn shared_rustybuzz_face_cache() -> &'static RustybuzzFaceCache {
  static FACE_CACHE: OnceLock<RustybuzzFaceCache> = OnceLock::new();
  FACE_CACHE.get_or_init(|| RustybuzzFaceCache::new(RUSTYBUZZ_FACE_CACHE_SIZE))
}

/// Returns a cached parsed face for a loaded font, parsing and inserting it on-demand.
#[inline]
pub fn get_ttf_face(font: &LoadedFont) -> Option<Arc<CachedFace>> {
  shared_face_cache().get_or_parse(Arc::clone(&font.data), font.index)
}

/// Returns a cached parsed face for arbitrary font data backed by an Arc.
#[inline]
pub fn get_ttf_face_with_data(data: &Arc<Vec<u8>>, index: u32) -> Option<Arc<CachedFace>> {
  shared_face_cache().get_or_parse(Arc::clone(data), index)
}

/// Returns a cached rustybuzz face for a loaded font, parsing and inserting it on-demand.
#[inline]
pub fn get_rustybuzz_face(font: &LoadedFont) -> Option<Arc<CachedRustybuzzFace>> {
  shared_rustybuzz_face_cache().get_or_parse(Arc::clone(&font.data), font.index)
}

/// Returns a cached rustybuzz face for arbitrary font data backed by an Arc.
#[inline]
pub fn get_rustybuzz_face_with_data(
  data: &Arc<Vec<u8>>,
  index: u32,
) -> Option<Arc<CachedRustybuzzFace>> {
  shared_rustybuzz_face_cache().get_or_parse(Arc::clone(data), index)
}

/// Convenience wrapper that avoids cloning the cached face when only a borrow is needed.
#[inline]
pub fn with_face<T, F: FnOnce(&ttf_parser::Face<'static>) -> T>(
  font: &LoadedFont,
  f: F,
) -> Option<T> {
  let face = get_ttf_face(font)?;
  Some(f(face.face()))
}

#[cfg(debug_assertions)]
static FACE_PARSE_COUNTER: AtomicU64 = AtomicU64::new(0);
#[cfg(debug_assertions)]
static RUSTYBUZZ_FACE_PARSE_COUNTER: AtomicU64 = AtomicU64::new(0);

#[cfg(debug_assertions)]
thread_local! {
  static TEST_FACE_PARSE_COUNTING_ENABLED: Cell<bool> = Cell::new(false);
  static TEST_FACE_PARSE_COUNTER: Cell<u64> = Cell::new(0);
  static TEST_RUSTYBUZZ_FACE_PARSE_COUNTING_ENABLED: Cell<bool> = Cell::new(false);
  static TEST_RUSTYBUZZ_FACE_PARSE_COUNTER: Cell<u64> = Cell::new(0);
}

#[inline]
fn record_face_parse() {
  #[cfg(debug_assertions)]
  {
    if std::env::var("FASTRENDER_COUNT_FACE_PARSE").is_ok() {
      FACE_PARSE_COUNTER.fetch_add(1, Ordering::Relaxed);
    }
    TEST_FACE_PARSE_COUNTING_ENABLED.with(|enabled| {
      if enabled.get() {
        TEST_FACE_PARSE_COUNTER.with(|counter| counter.set(counter.get().saturating_add(1)));
      }
    });
  }
}

#[inline]
fn record_rustybuzz_face_parse() {
  #[cfg(debug_assertions)]
  {
    if std::env::var("FASTRENDER_COUNT_RUSTYBUZZ_FACE_PARSE").is_ok() {
      RUSTYBUZZ_FACE_PARSE_COUNTER.fetch_add(1, Ordering::Relaxed);
    }
    TEST_RUSTYBUZZ_FACE_PARSE_COUNTING_ENABLED.with(|enabled| {
      if enabled.get() {
        TEST_RUSTYBUZZ_FACE_PARSE_COUNTER
          .with(|counter| counter.set(counter.get().saturating_add(1)));
      }
    });
  }
}

#[cfg(debug_assertions)]
pub struct FaceParseCountGuard {
  _private: (),
}

#[cfg(debug_assertions)]
impl FaceParseCountGuard {
  pub fn start() -> Self {
    TEST_FACE_PARSE_COUNTER.with(|counter| counter.set(0));
    TEST_FACE_PARSE_COUNTING_ENABLED.with(|enabled| enabled.set(true));
    Self { _private: () }
  }
}

#[cfg(debug_assertions)]
impl Drop for FaceParseCountGuard {
  fn drop(&mut self) {
    TEST_FACE_PARSE_COUNTING_ENABLED.with(|enabled| enabled.set(false));
  }
}

#[cfg(debug_assertions)]
pub struct RustybuzzFaceParseCountGuard {
  _private: (),
}

#[cfg(debug_assertions)]
impl RustybuzzFaceParseCountGuard {
  pub fn start() -> Self {
    TEST_RUSTYBUZZ_FACE_PARSE_COUNTER.with(|counter| counter.set(0));
    TEST_RUSTYBUZZ_FACE_PARSE_COUNTING_ENABLED.with(|enabled| enabled.set(true));
    Self { _private: () }
  }
}

#[cfg(debug_assertions)]
impl Drop for RustybuzzFaceParseCountGuard {
  fn drop(&mut self) {
    TEST_RUSTYBUZZ_FACE_PARSE_COUNTING_ENABLED.with(|enabled| enabled.set(false));
  }
}

#[cfg(debug_assertions)]
pub fn face_parse_count() -> u64 {
  TEST_FACE_PARSE_COUNTER.with(|counter| counter.get())
}

#[cfg(not(debug_assertions))]
pub fn face_parse_count() -> u64 {
  0
}

#[cfg(debug_assertions)]
pub fn rustybuzz_face_parse_count() -> u64 {
  TEST_RUSTYBUZZ_FACE_PARSE_COUNTER.with(|counter| counter.get())
}

#[cfg(not(debug_assertions))]
pub fn rustybuzz_face_parse_count() -> u64 {
  0
}

#[cfg(debug_assertions)]
pub fn reset_face_parse_counter_for_tests() {
  TEST_FACE_PARSE_COUNTER.with(|counter| counter.set(0));
}

#[cfg(not(debug_assertions))]
pub fn reset_face_parse_counter_for_tests() {}

#[cfg(debug_assertions)]
pub fn reset_rustybuzz_face_parse_counter_for_tests() {
  TEST_RUSTYBUZZ_FACE_PARSE_COUNTER.with(|counter| counter.set(0));
}

#[cfg(not(debug_assertions))]
pub fn reset_rustybuzz_face_parse_counter_for_tests() {}
