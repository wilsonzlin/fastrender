//! Disk-backed caching fetcher that shares in-memory semantics with [`CachingFetcher`].
//!
//! Enabled via the `disk_cache` crate feature.

use super::CacheAction;
use super::CachedHttpMetadata;
use super::CachedSnapshot;
use super::CachingFetcher;
use super::CachingFetcherConfig;
use super::FetchedResource;
use super::HttpCachePolicy;
use super::ResourceFetcher;
use super::ResourcePolicy;
use super::MAX_ALIAS_HOPS;
use crate::error::{Error, RenderError, RenderStage};
use crate::render_control;
use crate::Result;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::cell::RefCell;
use std::fs::{self, OpenOptions};
use std::io::{self, ErrorKind, Read, Write};
use std::path::Path;
use std::path::PathBuf;
#[cfg(test)]
use std::sync::{Arc, Barrier};
use std::thread;
use std::time::Duration;
use std::time::Instant;
use std::time::SystemTime;
use std::time::UNIX_EPOCH;

#[path = "disk_cache_index.rs"]
mod disk_cache_index;
use disk_cache_index::DiskCacheIndex;

#[derive(Debug, Clone)]
pub struct DiskCacheConfig {
  /// Maximum total bytes to keep on disk. `0` disables eviction.
  pub max_bytes: u64,
  /// Maximum age after which entries are treated as stale (revalidated/re-fetched) rather than
  /// immediately served.
  ///
  /// Stale entries remain on disk so they can still be used as a fallback when network fetches
  /// fail. When a cached response lacks HTTP cache metadata, this value is used to synthesize a
  /// freshness window so entries are treated as fresh until `stored_at + max_age` and stale
  /// afterwards.
  ///
  /// Also caps HTTP-provided freshness metadata when serving from disk.
  pub max_age: Option<Duration>,
  /// Whether to persist responses that set `Cache-Control: no-store`.
  ///
  /// When enabled, `no-store` entries are treated as always stale by the shared in-memory caching
  /// layer: they will normally trigger a network refresh but can be served as a fallback (or
  /// immediately under render deadlines when configured to serve stale entries).
  pub allow_no_store: bool,
  /// Whether to allow persisting disk cache entries while a render deadline with a timeout
  /// configured is active.
  ///
  /// Pageset renders run under a cooperative timeout; disabling writeback avoids spending render
  /// budget on disk writes but means newly-discovered subresources won't be persisted for future
  /// runs.
  pub writeback_under_deadline: bool,
  /// Maximum age of `.lock` files before they are treated as stale and removed.
  ///
  /// Pageset workers can be hard-killed (e.g. timeout) while persisting a cache entry, leaving a
  /// `.lock` file behind. Keeping this small avoids long stretches where disk caching is disabled
  /// due to a dead writer.
  pub lock_stale_after: Duration,
  /// Optional namespace used to partition the cache across differing request headers.
  ///
  /// When unset or empty, keys are hashed exactly like the legacy cache (`sha256(url)`) for
  /// backwards compatibility.
  pub namespace: Option<String>,
}

impl Default for DiskCacheConfig {
  fn default() -> Self {
    Self {
      max_bytes: 512 * 1024 * 1024,
      max_age: Some(Duration::from_secs(60 * 60 * 24 * 7)), // 7 days
      lock_stale_after: DEFAULT_LOCK_STALE_AFTER,
      allow_no_store: false,
      writeback_under_deadline: false,
      namespace: None,
    }
  }
}

const DEFAULT_LOCK_STALE_AFTER: Duration = Duration::from_secs(8);
const LOCK_WAIT_TIMEOUT: Duration = Duration::from_millis(500);
const READ_LOCK_WAIT_TIMEOUT: Duration = Duration::from_millis(200);
const READ_LOCK_RETRY_INITIAL_DELAY: Duration = Duration::from_millis(5);
const READ_LOCK_RETRY_MAX_DELAY: Duration = Duration::from_millis(50);
/// Small time buffer to avoid spending the entire render budget waiting on `.lock` files.
const LOCK_DEADLINE_BUFFER: Duration = Duration::from_millis(10);
const DISK_READ_CHUNK_SIZE: usize = 64 * 1024;
const DISK_WRITE_CHUNK_SIZE: usize = 64 * 1024;
/// Small time buffer to avoid spending the entire render budget persisting cache entries.
const DISK_WRITE_DEADLINE_BUFFER: Duration = Duration::from_millis(10);
/// Minimum chunk size used for deadline-aware disk reads.
///
/// Most disk-cache hits are tiny (CSS, meta JSON) so reading with a 64KiB scratch buffer on every
/// hit is wasteful. Keeping a minimum of 4KiB matches common page sizes and avoids pathological
/// syscalls for very small reads.
const DISK_READ_MIN_CHUNK_SIZE: usize = 4 * 1024;
/// Maximum chunk size used when reading `.bin.meta` files.
///
/// Metadata blobs are expected to be tiny; capping their per-read chunk size avoids growing the
/// reusable scratch buffer on threads that only touch metadata. (The scratch buffer is shared with
/// data reads; once it grows for larger resources it is intentionally retained for reuse.)
const DISK_META_READ_CHUNK_SIZE: usize = 8 * 1024;
const DISK_META_READ_STAGE: RenderStage = RenderStage::Paint;

thread_local! {
  /// Per-thread reusable scratch buffer for deadline-aware disk reads.
  ///
  /// The buffer grows on demand but is never shrunk to avoid repeated allocations/zeroing under
  /// parallel disk-cache hits.
  static DISK_READ_SCRATCH: RefCell<Vec<u8>> = RefCell::new(Vec::new());
}

#[derive(Debug)]
enum ReadAllWithDeadlineError {
  Io,
  Timeout(RenderError),
}

impl From<io::Error> for ReadAllWithDeadlineError {
  fn from(_err: io::Error) -> Self {
    Self::Io
  }
}

fn read_all_with_deadline<R: Read>(
  reader: &mut R,
  capacity_hint: Option<usize>,
  stage: RenderStage,
  max_chunk_size: usize,
) -> std::result::Result<Vec<u8>, ReadAllWithDeadlineError> {
  let mut bytes = match capacity_hint {
    Some(cap) => Vec::with_capacity(cap),
    None => Vec::new(),
  };

  let desired_chunk_size = capacity_hint
    .unwrap_or(max_chunk_size)
    .clamp(DISK_READ_MIN_CHUNK_SIZE, max_chunk_size);

  DISK_READ_SCRATCH.with(|scratch| {
    let mut buf = scratch.borrow_mut();
    if buf.len() < desired_chunk_size {
      buf.resize(desired_chunk_size, 0);
    }
    let mut deadline_counter = 0usize;
    loop {
      if let Err(err) = render_control::check_active_periodic(&mut deadline_counter, 1, stage) {
        return Err(ReadAllWithDeadlineError::Timeout(err));
      }
      match reader.read(&mut buf[..desired_chunk_size]) {
        Ok(0) => break,
        Ok(n) => bytes.extend_from_slice(&buf[..n]),
        Err(err) if err.kind() == ErrorKind::Interrupted => continue,
        Err(_err) => return Err(ReadAllWithDeadlineError::Io),
      }
    }
    Ok(bytes)
  })
}

fn read_path_with_deadline(
  path: &Path,
  stage: RenderStage,
  max_chunk_size: usize,
) -> std::result::Result<Vec<u8>, ReadAllWithDeadlineError> {
  let mut file = fs::File::open(path)?;
  let capacity_hint = file
    .metadata()
    .ok()
    .and_then(|meta| meta.len().try_into().ok());
  read_all_with_deadline(&mut file, capacity_hint, stage, max_chunk_size)
}

#[derive(Debug)]
enum WriteAllWithDeadlineError {
  Io,
  Timeout,
}

impl From<io::Error> for WriteAllWithDeadlineError {
  fn from(_err: io::Error) -> Self {
    Self::Io
  }
}

fn write_all_with_deadline<W: Write>(
  writer: &mut W,
  bytes: &[u8],
  stage: RenderStage,
  chunk_size: usize,
) -> std::result::Result<(), WriteAllWithDeadlineError> {
  if bytes.is_empty() {
    return Ok(());
  }

  let mut offset = 0usize;
  let mut deadline_counter = 0usize;
  while offset < bytes.len() {
    if let Err(err) = render_control::check_active_periodic(&mut deadline_counter, 1, stage) {
      let _ = err;
      return Err(WriteAllWithDeadlineError::Timeout);
    }

    let end = (offset + chunk_size).min(bytes.len());
    writer.write_all(&bytes[offset..end])?;
    offset = end;
  }
  Ok(())
}

fn write_path_with_deadline(
  path: &Path,
  bytes: &[u8],
  stage: RenderStage,
) -> std::result::Result<(), WriteAllWithDeadlineError> {
  let mut file = OpenOptions::new()
    .write(true)
    .create(true)
    .truncate(true)
    .open(path)?;
  write_all_with_deadline(&mut file, bytes, stage, DISK_WRITE_CHUNK_SIZE)?;
  Ok(())
}

fn stage_for_cached_content_type(content_type: Option<&str>) -> RenderStage {
  let mime = content_type
    .and_then(|ct| ct.split(';').next())
    .map(|ct| ct.trim().to_ascii_lowercase())
    .unwrap_or_default();

  if mime.contains("text/css") || mime.contains("font/") {
    return RenderStage::Css;
  }
  if mime.contains("html") {
    return RenderStage::DomParse;
  }
  RenderStage::Paint
}

#[derive(Debug, Serialize, Deserialize)]
struct LockFileContents {
  pid: u32,
  started_at: u64,
}

#[cfg(unix)]
fn pid_is_alive(pid: u32) -> Option<bool> {
  // `kill(0)` is special-cased to signal the current process group.
  if pid == 0 || pid > i32::MAX as u32 {
    return None;
  }

  // SAFETY: signal 0 performs error checking without sending a signal.
  let result = unsafe { libc::kill(pid as i32, 0) };
  if result == 0 {
    return Some(true);
  }
  let err = std::io::Error::last_os_error();
  match err.raw_os_error() {
    Some(code) if code == libc::ESRCH => Some(false),
    Some(code) if code == libc::EPERM => Some(true),
    _ => None,
  }
}

#[cfg(not(unix))]
fn pid_is_alive(_pid: u32) -> Option<bool> {
  None
}

#[derive(Debug, Clone)]
pub struct DiskCachingFetcher<F: ResourceFetcher> {
  memory: CachingFetcher<F>,
  cache_dir: PathBuf,
  disk_config: DiskCacheConfig,
  index: DiskCacheIndex,
  policy: Option<ResourcePolicy>,
  #[cfg(test)]
  persist_resource_hook: Option<Arc<PersistResourceHook>>,
}

#[cfg(test)]
#[derive(Debug)]
struct PersistResourceHook {
  before_index: Barrier,
  after_eviction: Barrier,
}

#[cfg(test)]
impl PersistResourceHook {
  fn new() -> Arc<Self> {
    Arc::new(Self {
      before_index: Barrier::new(2),
      after_eviction: Barrier::new(2),
    })
  }

  fn wait_before_index(&self) {
    let _ = self.before_index.wait();
  }

  fn wait_after_eviction(&self) {
    let _ = self.after_eviction.wait();
  }
}

struct EntryLock {
  path: PathBuf,
}

enum SnapshotRead {
  Hit(CachedSnapshot),
  Miss,
  Locked,
  Timeout(RenderError),
}

impl Drop for EntryLock {
  fn drop(&mut self) {
    let _ = fs::remove_file(&self.path);
  }
}

fn append_suffix(path: &Path, suffix: &str) -> PathBuf {
  let mut name = path.as_os_str().to_owned();
  name.push(suffix);
  PathBuf::from(name)
}

fn tmp_path(path: &Path) -> PathBuf {
  append_suffix(path, ".tmp")
}

fn lock_path_for(data_path: &Path) -> PathBuf {
  append_suffix(data_path, ".lock")
}

fn lock_age_from_metadata(meta: &fs::Metadata) -> Option<Duration> {
  meta
    .modified()
    .or_else(|_| meta.created())
    .ok()
    .and_then(|time| SystemTime::now().duration_since(time).ok())
}

fn clear_lock_file(lock_path: &Path) -> bool {
  match fs::remove_file(lock_path) {
    Ok(()) => true,
    Err(err) if err.kind() == ErrorKind::NotFound => true,
    Err(_) => false,
  }
}

fn hash_u64(input: &str) -> u64 {
  let mut hash: u64 = 0xcbf29ce484222325;
  for &b in input.as_bytes() {
    hash ^= u64::from(b);
    hash = hash.wrapping_mul(0x100000001b3);
  }
  hash
}

fn pseudo_rand_u64(mut x: u64) -> u64 {
  x ^= x >> 12;
  x ^= x << 25;
  x ^= x >> 27;
  x.wrapping_mul(0x2545F4914F6CDD1D)
}

fn jitter_duration(max: Duration, seed: u64) -> Duration {
  if max.is_zero() {
    return Duration::ZERO;
  }
  let max_ns = max.as_nanos();
  let denom = max_ns.saturating_add(1);
  let rand = pseudo_rand_u64(seed) as u128;
  let jitter_ns = rand % denom;
  let secs = (jitter_ns / 1_000_000_000) as u64;
  let nanos = (jitter_ns % 1_000_000_000) as u32;
  Duration::new(secs, nanos)
}
impl<F: ResourceFetcher> DiskCachingFetcher<F> {
  pub fn new(inner: F, cache_dir: impl Into<PathBuf>) -> Self {
    let memory_config = CachingFetcherConfig {
      honor_http_cache_freshness: true,
      ..CachingFetcherConfig::default()
    };
    Self::with_configs(inner, cache_dir, memory_config, DiskCacheConfig::default())
  }

  pub fn with_configs(
    inner: F,
    cache_dir: impl Into<PathBuf>,
    memory_config: CachingFetcherConfig,
    disk_config: DiskCacheConfig,
  ) -> Self {
    let cache_dir = cache_dir.into();
    let _ = fs::create_dir_all(&cache_dir);
    let index = DiskCacheIndex::new(cache_dir.clone());
    let mut memory_config = memory_config;
    memory_config.allow_no_store |= disk_config.allow_no_store;
    Self {
      memory: CachingFetcher::with_config(inner, memory_config),
      cache_dir,
      disk_config,
      index,
      policy: None,
      #[cfg(test)]
      persist_resource_hook: None,
    }
  }

  /// Apply a resource policy to both the in-memory and disk cache layers.
  ///
  /// URL allow/deny rules are enforced before reading from disk, and total byte budgets are
  /// reserved when cached entries are returned to callers.
  pub fn with_policy(mut self, policy: ResourcePolicy) -> Self {
    self.memory = self.memory.with_policy(policy.clone());
    self.policy = Some(policy);
    self
  }

  #[cfg(test)]
  fn with_persist_resource_hook(mut self, hook: Arc<PersistResourceHook>) -> Self {
    self.persist_resource_hook = Some(hook);
    self
  }

  fn should_skip_disk(&self, url: &str) -> bool {
    url.starts_with("data:")
  }

  fn disk_writeback_disabled(&self) -> bool {
    render_control::active_deadline()
      .map(|deadline| {
        deadline.timeout_limit().is_some() && !self.disk_config.writeback_under_deadline
      })
      .unwrap_or(false)
  }

  fn deadline_allows_disk_writeback(&self) -> bool {
    let Some(deadline) = render_control::active_deadline() else {
      return true;
    };
    if deadline.timeout_limit().is_none() {
      return true;
    }
    deadline
      .remaining_timeout()
      .map(|remaining| remaining > DISK_WRITE_DEADLINE_BUFFER)
      .unwrap_or(false)
  }

  fn canonical_url(&self, requested: &str, final_url: Option<&str>) -> String {
    let final_url = final_url
      .filter(|target| *target != requested)
      .and_then(|target| {
        if let Some(policy) = &self.policy {
          policy.ensure_url_allowed(target).ok()?;
        }
        Some(target)
      });

    final_url.unwrap_or(requested).to_string()
  }

  fn cache_key(&self, url: &str) -> String {
    let mut hasher = Sha256::new();
    let namespace = self.disk_config.namespace.as_deref().unwrap_or("");
    if !namespace.is_empty() {
      hasher.update(namespace.as_bytes());
      hasher.update(b"\n");
    }
    hasher.update(url.as_bytes());
    let digest = hasher.finalize();
    const HEX: &[u8; 16] = b"0123456789abcdef";
    let mut out = String::with_capacity(64);
    for &b in digest.iter() {
      out.push(HEX[(b >> 4) as usize] as char);
      out.push(HEX[(b & 0x0f) as usize] as char);
    }
    out
  }

  fn data_path(&self, url: &str) -> PathBuf {
    self.data_path_for_key(&self.cache_key(url))
  }

  fn data_path_for_key(&self, key: &str) -> PathBuf {
    self.cache_dir.join(format!("{key}.bin"))
  }

  fn meta_path_for_data(&self, data_path: &Path) -> PathBuf {
    let mut meta_path = data_path.to_path_buf();
    meta_path.set_extension("bin.meta");
    meta_path
  }

  fn alias_path(&self, url: &str) -> PathBuf {
    self.alias_path_for_key(&self.cache_key(url))
  }

  fn alias_path_for_key(&self, key: &str) -> PathBuf {
    self.cache_dir.join(format!("{key}.alias"))
  }

  fn lock_is_active(&self, data_path: &Path) -> bool {
    let lock_path = lock_path_for(data_path);
    let writeback_disabled = self.disk_writeback_disabled();
    match fs::metadata(&lock_path) {
      Ok(meta) => {
        let meta_age = lock_age_from_metadata(&meta);
        if meta_age
          .map(|age| age > self.disk_config.lock_stale_after)
          .unwrap_or(false)
        {
          return if writeback_disabled {
            false
          } else {
            !clear_lock_file(&lock_path)
          };
        }

        let contents = fs::read(&lock_path)
          .ok()
          .and_then(|bytes| serde_json::from_slice::<LockFileContents>(&bytes).ok());
        if let Some(contents) = contents {
          if let Some(false) = pid_is_alive(contents.pid) {
            return if writeback_disabled {
              false
            } else {
              !clear_lock_file(&lock_path)
            };
          }
          // On platforms/filesystems where we can't rely on mtime/ctime, fall back to the stored
          // timestamp in the lockfile itself.
          if meta_age.is_none() {
            let lock_age = Duration::from_secs(now_seconds().saturating_sub(contents.started_at));
            if lock_age > self.disk_config.lock_stale_after {
              return if writeback_disabled {
                false
              } else {
                !clear_lock_file(&lock_path)
              };
            }
          }
        }
        true
      }
      Err(err) => err.kind() != ErrorKind::NotFound,
    }
  }

  fn acquire_lock(&self, data_path: &Path) -> Option<EntryLock> {
    let lock_path = lock_path_for(data_path);
    let max_wait = self.deadline_aware_lock_wait(LOCK_WAIT_TIMEOUT);
    let deadline = Instant::now() + max_wait;

    loop {
      match OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(&lock_path)
      {
        Ok(mut file) => {
          let contents = LockFileContents {
            pid: std::process::id(),
            started_at: now_seconds(),
          };
          if let Ok(serialized) = serde_json::to_vec(&contents) {
            let _ = file.write_all(&serialized);
          }
          return Some(EntryLock { path: lock_path });
        }
        Err(err) if err.kind() == ErrorKind::AlreadyExists => {
          let now = Instant::now();
          if now >= deadline {
            return None;
          }

          // Avoid busy looping on a held lock by reusing the reader wait path (which checks PID
          // liveness once, then polls via cheap metadata/backoff).
          if !self.wait_for_unlock(data_path, deadline.saturating_duration_since(now)) {
            return None;
          }
        }
        Err(_) => return None,
      }
    }
  }

  fn deadline_aware_lock_wait(&self, desired: Duration) -> Duration {
    let Some(deadline) = render_control::active_deadline() else {
      return desired;
    };
    if deadline.timeout_limit().is_none() {
      return desired;
    }
    desired.min(
      deadline
        .remaining_timeout()
        .unwrap_or_default()
        .saturating_sub(LOCK_DEADLINE_BUFFER),
    )
  }

  fn wait_for_unlock(&self, data_path: &Path, max_wait: Duration) -> bool {
    if max_wait.is_zero() {
      return !self.lock_is_active(data_path);
    }

    let lock_path = lock_path_for(data_path);
    if !self.lock_is_active(data_path) {
      return true;
    }

    let deadline = Instant::now() + max_wait;
    let mut delay = READ_LOCK_RETRY_INITIAL_DELAY;
    let path_hash = hash_u64(&data_path.to_string_lossy());
    let mut attempt: u64 = 0;
    let mut last_age: Option<Duration> = None;
    loop {
      let now = Instant::now();
      if now >= deadline {
        return false;
      }
      match fs::metadata(&lock_path) {
        Ok(meta) => {
          let meta_age = lock_age_from_metadata(&meta);
          if let Some(age) = meta_age {
            if age > self.disk_config.lock_stale_after {
              if self.disk_writeback_disabled() {
                return true;
              }
              if clear_lock_file(&lock_path) {
                return true;
              }
            } else if let Some(prev) = last_age {
              if age < prev {
                // Lockfile likely re-created (pid changed); fall back to the full check so we can
                // clear dead PID locks promptly.
                if !self.lock_is_active(data_path) {
                  return true;
                }
              }
            }
            last_age = Some(age);
          } else if !self.lock_is_active(data_path) {
            return true;
          }
        }
        Err(err) if err.kind() == ErrorKind::NotFound => return true,
        Err(_) => {}
      }
      let remaining = deadline.saturating_duration_since(now);
      let sleep_cap = delay.min(remaining);
      if !sleep_cap.is_zero() {
        let half = sleep_cap / 2;
        let now_ns = SystemTime::now()
          .duration_since(UNIX_EPOCH)
          .map(|d| d.as_nanos() as u64)
          .unwrap_or(0);
        let seed = now_ns
          ^ path_hash
          ^ attempt.wrapping_mul(0x9E3779B97F4A7C15)
          ^ (u64::from(std::process::id()));
        let sleep_for = (half + jitter_duration(half, seed)).min(remaining);
        if !sleep_for.is_zero() {
          thread::sleep(sleep_for);
        }
      }
      delay = delay.saturating_mul(2).min(READ_LOCK_RETRY_MAX_DELAY);
      attempt = attempt.wrapping_add(1);
    }
  }

  fn remove_entry_if_unlocked(&self, key: &str, data_path: &Path, meta_path: &Path) {
    if self.disk_writeback_disabled() {
      return;
    }
    if !self.lock_is_active(data_path) {
      let _ = self.remove_entry(key, data_path, meta_path);
    }
  }

  fn read_disk_entry(&self, url: &str) -> Result<Option<(String, CachedSnapshot)>> {
    let mut disk_timer = super::start_disk_cache_diagnostics();
    let mut current = url.to_string();
    let mut hops = 0usize;

    loop {
      if let Err(err) = render_control::check_active(DISK_META_READ_STAGE) {
        super::finish_disk_cache_diagnostics(disk_timer.take());
        return Err(Error::Render(err));
      }

      let key = self.cache_key(&current);
      let data_path = self.data_path_for_key(&key);
      let meta_path = self.meta_path_for_data(&data_path);

      match self.try_read_snapshot(&key, &current, &data_path, &meta_path) {
        SnapshotRead::Hit(snapshot) => {
          let payload_bytes = match &snapshot.value {
            super::CacheValue::Resource(res) => res.bytes.len(),
            super::CacheValue::Error(_) => 0,
          };
          super::record_disk_cache_hit();
          super::record_disk_cache_bytes(payload_bytes);
          super::finish_disk_cache_diagnostics(disk_timer.take());
          return Ok(Some((current, snapshot)));
        }
        SnapshotRead::Timeout(err) => {
          super::finish_disk_cache_diagnostics(disk_timer.take());
          return Err(Error::Render(err));
        }
        SnapshotRead::Locked => {
          let max_wait = self.deadline_aware_lock_wait(READ_LOCK_WAIT_TIMEOUT);
          if !max_wait.is_zero() {
            let lock_wait_timer = super::start_disk_cache_lock_wait_diagnostics();
            let unlocked = self.wait_for_unlock(&data_path, max_wait);
            super::finish_disk_cache_lock_wait_diagnostics(lock_wait_timer);
            if unlocked {
              match self.try_read_snapshot(&key, &current, &data_path, &meta_path) {
                SnapshotRead::Hit(snapshot) => {
                  let payload_bytes = match &snapshot.value {
                    super::CacheValue::Resource(res) => res.bytes.len(),
                    super::CacheValue::Error(_) => 0,
                  };
                  super::record_disk_cache_hit();
                  super::record_disk_cache_bytes(payload_bytes);
                  super::finish_disk_cache_diagnostics(disk_timer.take());
                  return Ok(Some((current, snapshot)));
                }
                SnapshotRead::Timeout(err) => {
                  super::finish_disk_cache_diagnostics(disk_timer.take());
                  return Err(Error::Render(err));
                }
                _ => {}
              }
            }
          }
        }
        SnapshotRead::Miss => {}
      }

      let Some(next) = self.read_alias_target(&current) else {
        super::record_disk_cache_miss();
        super::finish_disk_cache_diagnostics(disk_timer.take());
        return Ok(None);
      };

      if hops >= MAX_ALIAS_HOPS || next == current {
        self.remove_alias_for(&current);
        super::record_disk_cache_miss();
        super::finish_disk_cache_diagnostics(disk_timer.take());
        return Ok(None);
      }

      current = next;
      hops += 1;
    }
  }

  fn try_read_snapshot(
    &self,
    key: &str,
    url: &str,
    data_path: &Path,
    meta_path: &Path,
  ) -> SnapshotRead {
    if self.lock_is_active(data_path) {
      return SnapshotRead::Locked;
    }

    let meta_bytes =
      match read_path_with_deadline(meta_path, DISK_META_READ_STAGE, DISK_META_READ_CHUNK_SIZE) {
        Ok(bytes) => bytes,
        Err(ReadAllWithDeadlineError::Timeout(err)) => return SnapshotRead::Timeout(err),
        Err(_) => {
          self.remove_entry_if_unlocked(key, data_path, meta_path);
          return SnapshotRead::Miss;
        }
      };
    let meta: StoredMetadata = match serde_json::from_slice(&meta_bytes) {
      Ok(meta) => meta,
      Err(_) => {
        self.remove_entry_if_unlocked(key, data_path, meta_path);
        return SnapshotRead::Miss;
      }
    };

    let read_stage = stage_for_cached_content_type(meta.content_type.as_deref());
    let bytes = match read_path_with_deadline(data_path, read_stage, DISK_READ_CHUNK_SIZE) {
      Ok(bytes) => bytes,
      Err(ReadAllWithDeadlineError::Timeout(err)) => return SnapshotRead::Timeout(err),
      Err(_) => {
        self.remove_entry_if_unlocked(key, data_path, meta_path);
        return SnapshotRead::Miss;
      }
    };
    if meta.len != bytes.len() {
      self.remove_entry_if_unlocked(key, data_path, meta_path);
      return SnapshotRead::Miss;
    }
    self
      .index
      .backfill_if_missing(key, meta.stored_at, meta.len as u64, data_path, meta_path);

    let mut resource = FetchedResource::with_final_url(
      bytes,
      meta.content_type.clone(),
      meta.final_url.clone().or_else(|| Some(url.to_string())),
    );
    resource.status = meta.status;
    resource.etag = meta.etag.clone();
    resource.last_modified = meta.last_modified.clone();

    let stored_time = secs_to_system_time(meta.stored_at).unwrap_or(SystemTime::now());
    let http_cache = meta.cache.as_ref().and_then(|c| c.to_http()).or_else(|| {
      self.disk_config.max_age.map(|max_age| CachedHttpMetadata {
        stored_at: stored_time,
        max_age: Some(max_age),
        expires: None,
        no_cache: false,
        no_store: false,
        must_revalidate: false,
      })
    });
    resource.cache_policy = meta.cache.as_ref().map(|c| c.to_policy()).or_else(|| {
      self.disk_config.max_age.map(|max_age| HttpCachePolicy {
        max_age: Some(max_age.as_secs()),
        ..Default::default()
      })
    });

    SnapshotRead::Hit(CachedSnapshot {
      value: super::CacheValue::Resource(resource),
      etag: meta.etag,
      last_modified: meta.last_modified,
      http_cache,
    })
  }

  fn read_alias_target(&self, url: &str) -> Option<String> {
    let alias_path = self.alias_path(url);
    fs::read(&alias_path)
      .ok()
      .and_then(|bytes| serde_json::from_slice::<StoredAlias>(&bytes).ok())
      .map(|alias| alias.target)
  }

  fn remove_alias_for(&self, url: &str) {
    if self.disk_writeback_disabled() {
      return;
    }
    let alias_path = self.alias_path(url);
    let _ = fs::remove_file(tmp_path(&alias_path));
    let _ = fs::remove_file(&alias_path);
  }

  fn persist_alias(&self, alias: &str, canonical: &str) {
    if self.disk_writeback_disabled() {
      return;
    }
    if alias == canonical || self.should_skip_disk(alias) || self.should_skip_disk(canonical) {
      return;
    }

    let alias_path = self.alias_path(alias);
    if let Some(parent) = alias_path.parent() {
      let _ = fs::create_dir_all(parent);
    }

    let tmp = tmp_path(&alias_path);
    let alias = StoredAlias {
      target: canonical.to_string(),
    };
    match serde_json::to_vec(&alias) {
      Ok(serialized) if fs::write(&tmp, &serialized).is_ok() => {
        let _ = fs::rename(&tmp, &alias_path);
      }
      _ => {
        let _ = fs::remove_file(&tmp);
      }
    }
  }

  fn persist_snapshot(&self, url: &str, snapshot: &CachedSnapshot) {
    if self.disk_writeback_disabled() {
      return;
    }
    if let Some(resource) = snapshot.as_resource() {
      self.persist_resource(
        url,
        &resource,
        snapshot.etag.as_deref(),
        snapshot.last_modified.as_deref(),
        snapshot.http_cache.as_ref(),
      );
    }
  }

  fn persist_resource(
    &self,
    url: &str,
    resource: &FetchedResource,
    etag: Option<&str>,
    last_modified: Option<&str>,
    http_cache: Option<&CachedHttpMetadata>,
  ) {
    if self.disk_writeback_disabled() {
      return;
    }
    if self.should_skip_disk(url) {
      return;
    }
    if !self.deadline_allows_disk_writeback() {
      return;
    }

    self.remove_alias_for(url);

    let key = self.cache_key(url);
    let data_path = self.data_path_for_key(&key);
    if let Some(parent) = data_path.parent() {
      let _ = fs::create_dir_all(parent);
    }

    let Some(entry_lock) = self.acquire_lock(&data_path) else {
      return;
    };
    if !self.deadline_allows_disk_writeback() {
      return;
    }

    let stored_at = now_seconds();
    let stored_time = UNIX_EPOCH
      .checked_add(Duration::from_secs(stored_at))
      .unwrap_or(SystemTime::now());

    let cache_metadata = http_cache.cloned().or_else(|| {
      resource
        .cache_policy
        .as_ref()
        .and_then(|p| CachedHttpMetadata::from_policy(p, stored_time))
    });
    if !self.disk_config.allow_no_store
      && (cache_metadata.as_ref().map(|m| m.no_store).unwrap_or(false)
        || resource
          .cache_policy
          .as_ref()
          .map(|p| p.no_store)
          .unwrap_or(false))
    {
      let meta_path = self.meta_path_for_data(&data_path);
      let _ = self.remove_entry(&key, &data_path, &meta_path);
      return;
    }

    let meta_path = self.meta_path_for_data(&data_path);
    let data_tmp = tmp_path(&data_path);
    let meta_tmp = tmp_path(&meta_path);
    let stage = stage_for_cached_content_type(resource.content_type.as_deref());
    let deadline_aware_write = render_control::active_deadline()
      .map(|deadline| deadline.is_enabled())
      .unwrap_or(false);

    let meta = StoredMetadata {
      url: url.to_string(),
      status: resource.status,
      content_type: resource.content_type.clone(),
      etag: etag
        .map(|s| s.to_string())
        .or_else(|| resource.etag.clone()),
      last_modified: last_modified
        .map(|s| s.to_string())
        .or_else(|| resource.last_modified.clone()),
      final_url: resource.final_url.clone().or_else(|| Some(url.to_string())),
      stored_at,
      len: resource.bytes.len(),
      cache: cache_metadata
        .as_ref()
        .and_then(StoredCacheMetadata::from_http),
    };

    let data_written = if deadline_aware_write {
      write_path_with_deadline(&data_tmp, &resource.bytes, stage).is_ok()
    } else {
      fs::write(&data_tmp, &resource.bytes).is_ok()
    };
    if !data_written {
      let _ = fs::remove_file(&data_tmp);
      let _ = fs::remove_file(&meta_tmp);
      return;
    }

    let Ok(serialized) = serde_json::to_vec(&meta) else {
      let _ = fs::remove_file(&data_tmp);
      let _ = fs::remove_file(&meta_tmp);
      return;
    };

    let meta_written = if deadline_aware_write {
      write_path_with_deadline(&meta_tmp, &serialized, stage).is_ok()
    } else {
      fs::write(&meta_tmp, &serialized).is_ok()
    };
    if !meta_written {
      let _ = fs::remove_file(&data_tmp);
      let _ = fs::remove_file(&meta_tmp);
      return;
    }

    if fs::rename(&data_tmp, &data_path).is_err() {
      let _ = fs::remove_file(&data_tmp);
      let _ = fs::remove_file(&meta_tmp);
      return;
    }

    if fs::rename(&meta_tmp, &meta_path).is_err() {
      let _ = fs::remove_file(&meta_tmp);
      let _ = self.remove_entry(&key, &data_path, &meta_path);
      return;
    }

    // The entry is now fully consistent on disk. Keep the per-entry lock until after we append
    // the insert record to `index.jsonl` so concurrent evictions don't delete freshly-written files
    // while the index still reflects the old ordering.
    #[cfg(test)]
    if let Some(hook) = self.persist_resource_hook.as_ref() {
      hook.wait_before_index();
      hook.wait_after_eviction();
    }

    self.index.record_insert(
      &key,
      stored_at,
      resource.bytes.len() as u64,
      &data_path,
      &meta_path,
    );

    // Now that the entry is journaled, release the per-entry lock before running eviction to keep
    // lock hold times small and allow oversized entries to self-evict.
    drop(entry_lock);

    self
      .index
      .evict_if_needed(self.disk_config.max_bytes, |path| {
        !self.lock_is_active(path)
      });
  }

  fn remove_entry(&self, key: &str, data_path: &Path, meta_path: &Path) -> Result<()> {
    if self.disk_writeback_disabled() {
      return Ok(());
    }
    self.index.record_removal(key, data_path, meta_path);
    let _ = fs::remove_file(tmp_path(data_path));
    let _ = fs::remove_file(tmp_path(meta_path));
    let mut alias_path = data_path.to_path_buf();
    alias_path.set_extension("alias");
    let _ = fs::remove_file(tmp_path(&alias_path));
    let _ = fs::remove_file(&alias_path);
    Ok(())
  }

  fn remove_entry_for_url(&self, url: &str) {
    if self.disk_writeback_disabled() {
      return;
    }
    let key = self.cache_key(url);
    let data_path = self.data_path_for_key(&key);
    let meta_path = self.meta_path_for_data(&data_path);
    let _ = self.remove_entry(&key, &data_path, &meta_path);
    self.remove_alias_for(url);
  }

  #[cfg(test)]
  fn debug_index_stats(&self) -> disk_cache_index::DebugStats {
    self.index.debug_stats()
  }
}

impl<F: ResourceFetcher> ResourceFetcher for DiskCachingFetcher<F> {
  fn fetch(&self, url: &str) -> Result<FetchedResource> {
    if let Some(policy) = &self.policy {
      policy.ensure_url_allowed(url)?;
    }

    let mut disk_snapshot: Option<(String, CachedSnapshot)> = None;
    let mut cached = self.memory.cached_snapshot(url);
    let should_read_disk = match cached.as_ref() {
      Some(snapshot) => matches!(snapshot.value, super::CacheValue::Error(_)),
      None => true,
    };
    if should_read_disk {
      disk_snapshot = self.read_disk_entry(url)?;
      if let Some((ref canonical, ref snapshot)) = disk_snapshot {
        self.memory.prime_cache_with_snapshot(url, snapshot.clone());
        if canonical != url {
          self.persist_alias(url, canonical);
        }
        cached = self.memory.cached_snapshot(url);
      }
    }

    let cached = cached.or_else(|| disk_snapshot.map(|(_, snapshot)| snapshot));
    let plan = self
      .memory
      .plan_cache_use(url, cached.clone(), self.disk_config.max_age);

    if let CacheAction::UseCached = plan.action {
      if let Some(snapshot) = plan.cached.as_ref() {
        if plan.is_stale {
          super::record_cache_stale_hit();
        } else {
          super::record_cache_fresh_hit();
        }
        let result = snapshot.value.as_result();
        if let Ok(ref res) = result {
          super::record_resource_cache_bytes(res.bytes.len());
          super::reserve_policy_bytes(&self.policy, res)?;
        }
        return result;
      }
    }

    let (flight, is_owner) = self.memory.join_inflight(url);
    if !is_owner {
      let inflight_timer = super::start_fetch_inflight_wait_diagnostics();
      let result = flight.wait(url);
      super::finish_fetch_inflight_wait_diagnostics(inflight_timer);
      if let Ok(ref res) = result {
        super::reserve_policy_bytes(&self.policy, res)?;
      }
      return result;
    }

    let mut inflight_guard = super::InFlightOwnerGuard::new(&self.memory, url, flight);

    let validators = match &plan.action {
      CacheAction::Validate {
        etag,
        last_modified,
      } => Some((etag.as_deref(), last_modified.as_deref())),
      _ => None,
    };

    let fetch_result = match validators {
      Some((etag, last_modified)) => {
        self
          .memory
          .inner
          .fetch_with_validation(url, etag, last_modified)
      }
      None => self.memory.inner.fetch(url),
    };

    let (mut result, charge_budget) = match fetch_result {
      Ok(res) => {
        if res.is_not_modified() {
          if let Some(snapshot) = plan.cached.as_ref() {
            let value = snapshot.value.as_result();
            if let Ok(ref ok) = value {
              let stored_at = SystemTime::now();
              let should_store = self.memory.config.allow_no_store
                || !res
                  .cache_policy
                  .as_ref()
                  .map(|p| p.no_store)
                  .unwrap_or(false);
              let http_cache = snapshot
                .http_cache
                .as_ref()
                .and_then(|meta| meta.with_updated_policy(res.cache_policy.as_ref(), stored_at))
                .or_else(|| {
                  res
                    .cache_policy
                    .as_ref()
                    .and_then(|policy| CachedHttpMetadata::from_policy(policy, stored_at))
                });

              if should_store {
                let canonical = self.memory.cache_entry(
                  url,
                  super::CacheEntry {
                    value: super::CacheValue::Resource(ok.clone()),
                    etag: res.etag.clone().or_else(|| snapshot.etag.clone()),
                    last_modified: res
                      .last_modified
                      .clone()
                      .or_else(|| snapshot.last_modified.clone()),
                    http_cache,
                  },
                  ok.final_url.as_deref(),
                );
                if let Some(snapshot) = self.memory.cached_snapshot(&canonical) {
                  self.persist_snapshot(&canonical, &snapshot);
                  if canonical != url {
                    self.persist_alias(url, &canonical);
                  }
                }
              } else {
                self.memory.remove_cached(url);
                let canonical = self.canonical_url(url, ok.final_url.as_deref());
                self.remove_entry_for_url(&canonical);
                if canonical != url {
                  self.remove_alias_for(url);
                }
              }
            }
            super::record_cache_revalidated_hit();
            if let Ok(ref ok) = value {
              super::record_resource_cache_bytes(ok.bytes.len());
            }
            let is_ok = value.is_ok();
            (value, is_ok)
          } else {
            (
              Err(Error::Other(
                "Received 304 without cached entry".to_string(),
              )),
              false,
            )
          }
        } else {
          if res
            .status
            .map(super::is_transient_http_status)
            .unwrap_or(false)
          {
            if let Some(snapshot) = plan.cached.as_ref() {
              super::record_cache_stale_hit();
              let fallback = snapshot.value.as_result();
              if let Ok(ref ok) = fallback {
                super::record_resource_cache_bytes(ok.bytes.len());
              }
              let is_ok = fallback.is_ok();
              (fallback, is_ok)
            } else {
              super::record_cache_miss();
              (Ok(res), false)
            }
          } else {
            let stored_at = SystemTime::now();
            if let Some(entry) = self.memory.build_cache_entry(&res, stored_at) {
              let canonical = self
                .memory
                .cache_entry(url, entry, res.final_url.as_deref());
              if let Some(snapshot) = self.memory.cached_snapshot(&canonical) {
                self.persist_snapshot(&canonical, &snapshot);
              } else {
                let http_cache = res
                  .cache_policy
                  .as_ref()
                  .and_then(|p| CachedHttpMetadata::from_policy(p, stored_at));
                self.persist_resource(
                  &canonical,
                  &res,
                  res.etag.as_deref(),
                  res.last_modified.as_deref(),
                  http_cache.as_ref(),
                );
              }
              if canonical != url {
                self.persist_alias(url, &canonical);
              }
            } else if res
              .cache_policy
              .as_ref()
              .map(|p| p.no_store)
              .unwrap_or(false)
              && !self.memory.config.allow_no_store
            {
              self.memory.remove_cached(url);
              let canonical = self.canonical_url(url, res.final_url.as_deref());
              self.remove_entry_for_url(&canonical);
              if canonical != url {
                self.remove_alias_for(url);
              }
            }
            super::record_cache_miss();
            (Ok(res), false)
          }
        }
      }
      Err(err) => {
        if let Some(snapshot) = plan.cached.as_ref() {
          super::record_cache_stale_hit();
          let fallback = snapshot.value.as_result();
          if let Ok(ref ok) = fallback {
            super::record_resource_cache_bytes(ok.bytes.len());
          }
          let is_ok = fallback.is_ok();
          (fallback, is_ok)
        } else {
          if self.memory.config.cache_errors {
            self.memory.cache_entry(
              url,
              super::CacheEntry {
                value: super::CacheValue::Error(err.clone()),
                etag: None,
                last_modified: None,
                http_cache: None,
              },
              None,
            );
          }
          (Err(err), false)
        }
      }
    };

    if charge_budget {
      if let Ok(ref res) = result {
        if let Err(err) = super::reserve_policy_bytes(&self.policy, res) {
          result = Err(err);
        }
      }
    }

    let notify = match &result {
      Ok(res) => super::SharedResult::Success(res.clone()),
      Err(err) => super::SharedResult::Error(err.clone()),
    };
    inflight_guard.finish(notify);

    result
  }
}

#[derive(Debug, Serialize, Deserialize)]
pub(super) struct StoredMetadata {
  url: String,
  #[serde(default, skip_serializing_if = "Option::is_none")]
  status: Option<u16>,
  content_type: Option<String>,
  etag: Option<String>,
  last_modified: Option<String>,
  final_url: Option<String>,
  stored_at: u64,
  len: usize,
  cache: Option<StoredCacheMetadata>,
}

#[derive(Debug, Serialize, Deserialize)]
struct StoredAlias {
  target: String,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub(super) struct StoredCacheMetadata {
  stored_at: u64,
  max_age: Option<u64>,
  expires: Option<u64>,
  no_cache: bool,
  no_store: bool,
  must_revalidate: bool,
}

impl StoredCacheMetadata {
  fn from_http(meta: &CachedHttpMetadata) -> Option<Self> {
    Some(Self {
      stored_at: system_time_to_secs(meta.stored_at)?,
      max_age: meta.max_age.map(|d| d.as_secs()),
      expires: meta.expires.and_then(system_time_to_secs),
      no_cache: meta.no_cache,
      no_store: meta.no_store,
      must_revalidate: meta.must_revalidate,
    })
  }

  fn to_http(&self) -> Option<CachedHttpMetadata> {
    let stored_at = secs_to_system_time(self.stored_at)?;
    Some(CachedHttpMetadata {
      stored_at,
      max_age: self.max_age.map(Duration::from_secs),
      expires: self.expires.and_then(secs_to_system_time),
      no_cache: self.no_cache,
      no_store: self.no_store,
      must_revalidate: self.must_revalidate,
    })
  }

  fn to_policy(&self) -> HttpCachePolicy {
    HttpCachePolicy {
      max_age: self.max_age,
      no_cache: self.no_cache,
      no_store: self.no_store,
      must_revalidate: self.must_revalidate,
      expires: self.expires.and_then(secs_to_system_time),
    }
  }
}

fn now_seconds() -> u64 {
  SystemTime::now()
    .duration_since(UNIX_EPOCH)
    .unwrap_or_default()
    .as_secs()
}

fn system_time_to_secs(time: SystemTime) -> Option<u64> {
  time.duration_since(UNIX_EPOCH).ok().map(|d| d.as_secs())
}

fn secs_to_system_time(secs: u64) -> Option<SystemTime> {
  UNIX_EPOCH.checked_add(Duration::from_secs(secs))
}

#[cfg(test)]
mod tests {
  use super::super::{CacheStalePolicy, HttpFetcher, ResourcePolicy};
  use super::*;
  use filetime::FileTime;
  use std::collections::VecDeque;
  use std::fs;
  use std::io;
  use std::io::{Read, Write};
  use std::net::TcpListener;
  use std::sync::atomic::AtomicUsize;
  use std::sync::atomic::Ordering;
  use std::sync::Arc;
  use std::sync::Mutex;
  use std::thread;

  fn try_bind_localhost(context: &str) -> Option<TcpListener> {
    match TcpListener::bind("127.0.0.1:0") {
      Ok(listener) => Some(listener),
      Err(err)
        if matches!(
          err.kind(),
          io::ErrorKind::PermissionDenied | io::ErrorKind::AddrNotAvailable
        ) =>
      {
        eprintln!("skipping {context}: cannot bind localhost in this environment: {err}");
        None
      }
      Err(err) => panic!("bind {context}: {err}"),
    }
  }

  #[derive(Clone)]
  struct CountingFetcher {
    count: Arc<AtomicUsize>,
  }

  impl ResourceFetcher for CountingFetcher {
    fn fetch(&self, url: &str) -> Result<FetchedResource> {
      self.count.fetch_add(1, Ordering::SeqCst);
      let mut resource = FetchedResource::new(b"hello".to_vec(), Some("text/plain".to_string()));
      resource.final_url = Some(url.to_string());
      Ok(resource)
    }
  }

  #[derive(Clone)]
  struct EmptyFetcher {
    count: Arc<AtomicUsize>,
  }

  impl ResourceFetcher for EmptyFetcher {
    fn fetch(&self, url: &str) -> Result<FetchedResource> {
      self.count.fetch_add(1, Ordering::SeqCst);
      Ok(FetchedResource::with_final_url(
        Vec::new(),
        Some("text/css".to_string()),
        Some(url.to_string()),
      ))
    }
  }

  #[derive(Clone)]
  struct FailingFetcher {
    count: Arc<AtomicUsize>,
  }

  impl ResourceFetcher for FailingFetcher {
    fn fetch(&self, _url: &str) -> Result<FetchedResource> {
      self.count.fetch_add(1, Ordering::SeqCst);
      Err(Error::Other("network error".to_string()))
    }
  }

  #[derive(Clone)]
  struct SizedFetcher {
    count: Arc<AtomicUsize>,
    size: usize,
  }

  impl ResourceFetcher for SizedFetcher {
    fn fetch(&self, url: &str) -> Result<FetchedResource> {
      self.count.fetch_add(1, Ordering::SeqCst);
      let mut resource =
        FetchedResource::new(vec![b'x'; self.size], Some("text/plain".to_string()));
      resource.final_url = Some(url.to_string());
      Ok(resource)
    }
  }

  #[test]
  fn disk_cache_read_is_deadline_aware() {
    struct SlowReader {
      remaining: usize,
      per_read: usize,
      delay: Duration,
      bytes_read: Arc<AtomicUsize>,
    }

    impl Read for SlowReader {
      fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        if self.remaining == 0 {
          return Ok(0);
        }
        thread::sleep(self.delay);
        let n = self.remaining.min(self.per_read).min(buf.len());
        buf[..n].fill(0);
        self.remaining -= n;
        self.bytes_read.fetch_add(n, Ordering::SeqCst);
        Ok(n)
      }
    }

    let total_bytes = DISK_READ_CHUNK_SIZE * 8;
    let bytes_read = Arc::new(AtomicUsize::new(0));
    let mut reader = SlowReader {
      remaining: total_bytes,
      per_read: DISK_READ_CHUNK_SIZE,
      delay: Duration::from_millis(40),
      bytes_read: Arc::clone(&bytes_read),
    };

    let deadline = render_control::RenderDeadline::new(Some(Duration::from_millis(70)), None);
    let result = render_control::with_deadline(Some(&deadline), || {
      read_all_with_deadline(
        &mut reader,
        Some(total_bytes),
        RenderStage::Css,
        DISK_READ_CHUNK_SIZE,
      )
    });

    assert!(
      matches!(
        result,
        Err(ReadAllWithDeadlineError::Timeout(
          RenderError::Timeout { .. }
        ))
      ),
      "expected deadline-aware read to abort under active deadline"
    );
    let consumed = bytes_read.load(Ordering::SeqCst);
    assert!(
      consumed > 0,
      "reader should start consuming bytes before timing out"
    );
    assert!(
      consumed < total_bytes,
      "reader should not consume entire stream"
    );
  }

  #[test]
  fn disk_cache_read_reuses_scratch_buffer_across_many_reads() {
    // Reset the per-thread scratch buffer so this test isn't sensitive to test ordering.
    DISK_READ_SCRATCH.with(|scratch| {
      *scratch.borrow_mut() = Vec::new();
    });

    let tmp = tempfile::tempdir().unwrap();
    let small_path = tmp.path().join("small.bin");
    let medium_path = tmp.path().join("medium.bin");
    let small_bytes = vec![1u8; 128];
    let medium_bytes = vec![2u8; DISK_READ_CHUNK_SIZE * 2];
    fs::write(&small_path, &small_bytes).unwrap();
    fs::write(&medium_path, &medium_bytes).unwrap();

    for _ in 0..256 {
      let bytes =
        read_path_with_deadline(&small_path, RenderStage::Css, DISK_READ_CHUNK_SIZE).unwrap();
      assert_eq!(bytes, small_bytes);
    }

    let small_capacity = DISK_READ_SCRATCH.with(|scratch| scratch.borrow().capacity());
    assert!(
      small_capacity < DISK_READ_CHUNK_SIZE,
      "expected tiny reads to keep scratch buffer < {DISK_READ_CHUNK_SIZE} bytes, got {small_capacity}"
    );

    let bytes =
      read_path_with_deadline(&medium_path, RenderStage::Css, DISK_READ_CHUNK_SIZE).unwrap();
    assert_eq!(bytes, medium_bytes);

    let medium_capacity = DISK_READ_SCRATCH.with(|scratch| scratch.borrow().capacity());
    assert!(
      medium_capacity >= DISK_READ_CHUNK_SIZE,
      "expected medium read to grow scratch buffer to at least {DISK_READ_CHUNK_SIZE} bytes, got {medium_capacity}"
    );

    // Ensure subsequent tiny reads don't trigger unexpected buffer shrinkage.
    let bytes =
      read_path_with_deadline(&small_path, RenderStage::Css, DISK_READ_CHUNK_SIZE).unwrap();
    assert_eq!(bytes, small_bytes);
    let final_capacity = DISK_READ_SCRATCH.with(|scratch| scratch.borrow().capacity());
    assert_eq!(
      final_capacity, medium_capacity,
      "scratch buffer should not shrink between reads"
    );
  }

  #[test]
  fn disk_cache_read_timeout_propagates() {
    #[derive(Clone)]
    struct PanicFetcher;

    impl ResourceFetcher for PanicFetcher {
      fn fetch(&self, _url: &str) -> Result<FetchedResource> {
        panic!("network fetch should not be reached when the render deadline is exceeded");
      }
    }

    let tmp = tempfile::tempdir().unwrap();
    let disk = DiskCachingFetcher::new(PanicFetcher, tmp.path());
    let url = "https://example.com/cached";

    let mut resource = FetchedResource::new(b"cached".to_vec(), Some("text/plain".to_string()));
    resource.final_url = Some(url.to_string());
    disk.persist_resource(url, &resource, None, None, None);

    let deadline = render_control::RenderDeadline::new(Some(Duration::from_millis(0)), None);
    let err = render_control::with_deadline(Some(&deadline), || disk.fetch(url))
      .expect_err("fetch timeout");

    match err {
      Error::Render(RenderError::Timeout { stage, .. }) => assert_eq!(stage, RenderStage::Paint),
      other => panic!("expected render timeout, got {other:?}"),
    }
  }

  #[test]
  fn disk_cache_does_not_persist_under_deadline() {
    let tmp = tempfile::tempdir().unwrap();
    let counter = Arc::new(AtomicUsize::new(0));
    let fetcher = CountingFetcher {
      count: Arc::clone(&counter),
    };
    let disk = DiskCachingFetcher::new(fetcher, tmp.path());
    let url = "https://example.com/no-persist";

    let deadline = render_control::RenderDeadline::new(Some(Duration::from_millis(500)), None);
    let res = render_control::with_deadline(Some(&deadline), || disk.fetch(url)).expect("fetch");
    assert_eq!(res.bytes, b"hello");
    assert_eq!(counter.load(Ordering::SeqCst), 1);

    let data_path = disk.data_path(url);
    let meta_path = disk.meta_path_for_data(&data_path);
    assert!(!data_path.exists());
    assert!(!meta_path.exists());
  }

  #[test]
  fn disk_cache_persists_under_deadline_when_enabled() {
    let tmp = tempfile::tempdir().unwrap();
    let counter = Arc::new(AtomicUsize::new(0));
    let fetcher = CountingFetcher {
      count: Arc::clone(&counter),
    };
    let disk = DiskCachingFetcher::with_configs(
      fetcher,
      tmp.path(),
      CachingFetcherConfig {
        honor_http_cache_freshness: true,
        ..CachingFetcherConfig::default()
      },
      DiskCacheConfig {
        writeback_under_deadline: true,
        ..DiskCacheConfig::default()
      },
    );
    let url = "https://example.com/persist-under-deadline";

    let deadline = render_control::RenderDeadline::new(Some(Duration::from_millis(500)), None);
    let res = render_control::with_deadline(Some(&deadline), || disk.fetch(url)).expect("fetch");
    assert_eq!(res.bytes, b"hello");
    assert_eq!(counter.load(Ordering::SeqCst), 1);

    let data_path = disk.data_path(url);
    let meta_path = disk.meta_path_for_data(&data_path);
    assert!(
      data_path.exists(),
      "expected .bin to be written under deadline"
    );
    assert!(
      meta_path.exists(),
      "expected .bin.meta to be written under deadline"
    );

    // A new instance should hit the persisted disk cache and avoid a second network fetch.
    let second_fetcher = CountingFetcher {
      count: Arc::clone(&counter),
    };
    let disk_again = DiskCachingFetcher::new(second_fetcher, tmp.path());
    let second =
      render_control::with_deadline(Some(&deadline), || disk_again.fetch(url)).expect("disk hit");
    assert_eq!(second.bytes, b"hello");
    assert_eq!(counter.load(Ordering::SeqCst), 1, "should hit disk cache");
  }

  #[test]
  fn disk_cache_writeback_aborts_cleanly_when_deadline_triggers_mid_persist() {
    let tmp = tempfile::tempdir().unwrap();
    let counter = Arc::new(AtomicUsize::new(0));
    let fetcher = SizedFetcher {
      count: Arc::clone(&counter),
      size: DISK_WRITE_CHUNK_SIZE * 4,
    };
    let disk = DiskCachingFetcher::with_configs(
      fetcher,
      tmp.path(),
      CachingFetcherConfig {
        honor_http_cache_freshness: true,
        ..CachingFetcherConfig::default()
      },
      DiskCacheConfig {
        writeback_under_deadline: true,
        ..DiskCacheConfig::default()
      },
    );
    let url = "https://example.com/timeout-writeback";

    let cancel_calls = Arc::new(AtomicUsize::new(0));
    let cancel_counter = Arc::clone(&cancel_calls);
    let cancel = Arc::new(move || cancel_counter.fetch_add(1, Ordering::SeqCst) >= 1);
    let deadline = render_control::RenderDeadline::new(Some(Duration::from_secs(60)), Some(cancel));

    let res = render_control::with_deadline(Some(&deadline), || disk.fetch(url)).expect("fetch");
    assert_eq!(res.bytes.len(), DISK_WRITE_CHUNK_SIZE * 4);
    assert!(
      cancel_calls.load(Ordering::SeqCst) >= 2,
      "test requires the deadline to trigger after the write starts"
    );

    let data_path = disk.data_path(url);
    let meta_path = disk.meta_path_for_data(&data_path);
    let lock_path = lock_path_for(&data_path);

    assert!(
      !lock_path.exists(),
      "cache entry lock file should not survive a deadline-triggered abort"
    );
    assert_eq!(
      data_path.exists(),
      meta_path.exists(),
      "cache entry should be either fully written or fully absent"
    );
    assert!(!data_path.exists(), "expected aborted entry to be absent");
    assert!(
      !tmp_path(&data_path).exists(),
      "expected data tmp to be removed"
    );
    assert!(
      !tmp_path(&meta_path).exists(),
      "expected meta tmp to be removed"
    );
  }

  #[test]
  fn persists_resources_across_instances() {
    let tmp = tempfile::tempdir().unwrap();
    let counter = Arc::new(AtomicUsize::new(0));
    let fetcher = CountingFetcher {
      count: Arc::clone(&counter),
    };
    let disk = DiskCachingFetcher::new(fetcher, tmp.path());
    let url = "https://example.com/resource";

    let first = disk.fetch(url).expect("first fetch");
    assert_eq!(counter.load(Ordering::SeqCst), 1);
    assert_eq!(first.content_type.as_deref(), Some("text/plain"));
    assert_eq!(first.final_url.as_deref(), Some(url));

    let second_fetcher = CountingFetcher {
      count: Arc::clone(&counter),
    };
    let disk_again = DiskCachingFetcher::new(second_fetcher, tmp.path());
    let second = disk_again.fetch(url).expect("cached fetch");
    assert_eq!(counter.load(Ordering::SeqCst), 1, "should hit disk cache");
    assert_eq!(second.content_type.as_deref(), Some("text/plain"));
    assert_eq!(second.final_url.as_deref(), Some(url));
  }

  #[test]
  fn disk_cache_persists_status_across_instances() {
    #[derive(Clone)]
    struct StatusFetcher {
      count: Arc<AtomicUsize>,
    }

    impl ResourceFetcher for StatusFetcher {
      fn fetch(&self, url: &str) -> Result<FetchedResource> {
        self.count.fetch_add(1, Ordering::SeqCst);
        let mut resource =
          FetchedResource::new(b"not-found".to_vec(), Some("text/plain".to_string()));
        resource.status = Some(404);
        resource.final_url = Some(url.to_string());
        Ok(resource)
      }
    }

    let tmp = tempfile::tempdir().unwrap();
    let counter = Arc::new(AtomicUsize::new(0));
    let url = "https://example.com/missing";

    let seed = DiskCachingFetcher::new(
      StatusFetcher {
        count: Arc::clone(&counter),
      },
      tmp.path(),
    );
    let first = seed.fetch(url).expect("first fetch");
    assert_eq!(first.bytes, b"not-found");
    assert_eq!(first.status, Some(404));
    assert_eq!(counter.load(Ordering::SeqCst), 1);

    let disk_again = DiskCachingFetcher::new(
      CountingFetcher {
        count: Arc::clone(&counter),
      },
      tmp.path(),
    );
    let second = disk_again.fetch(url).expect("cached fetch");
    assert_eq!(second.bytes, b"not-found");
    assert_eq!(second.status, Some(404));
    assert_eq!(
      counter.load(Ordering::SeqCst),
      1,
      "should hit disk cache without calling the inner fetcher"
    );
  }

  #[test]
  fn disk_cache_loads_legacy_metadata_without_status() {
    let tmp = tempfile::tempdir().unwrap();
    let url = "https://example.com/legacy-meta";
    let disk = DiskCachingFetcher::new(PanicFetcher, tmp.path());

    let bytes = b"legacy".to_vec();
    let data_path = disk.data_path(url);
    let meta_path = disk.meta_path_for_data(&data_path);
    fs::write(&data_path, &bytes).unwrap();

    let meta_json = serde_json::json!({
      "url": url,
      "content_type": "text/plain",
      "etag": null,
      "last_modified": null,
      "final_url": url,
      "stored_at": now_seconds(),
      "len": bytes.len(),
      "cache": null,
    });
    fs::write(&meta_path, serde_json::to_vec(&meta_json).unwrap()).unwrap();

    let res = disk.fetch(url).expect("disk hit");
    assert_eq!(res.bytes, bytes);
    assert_eq!(res.content_type.as_deref(), Some("text/plain"));
    assert_eq!(res.status, None);
  }

  #[test]
  fn disk_cache_persists_no_store_when_allowed() {
    #[derive(Clone)]
    struct NoStoreFetcher {
      count: Arc<AtomicUsize>,
    }

    impl ResourceFetcher for NoStoreFetcher {
      fn fetch(&self, url: &str) -> Result<FetchedResource> {
        self.count.fetch_add(1, Ordering::SeqCst);
        let mut resource = FetchedResource::new(b"cached".to_vec(), Some("text/plain".to_string()));
        resource.final_url = Some(url.to_string());
        resource.cache_policy = Some(HttpCachePolicy {
          no_store: true,
          ..Default::default()
        });
        Ok(resource)
      }
    }

    let tmp = tempfile::tempdir().unwrap();
    let counter = Arc::new(AtomicUsize::new(0));
    let url = "https://example.com/no-store";
    let seed = DiskCachingFetcher::with_configs(
      NoStoreFetcher {
        count: Arc::clone(&counter),
      },
      tmp.path(),
      CachingFetcherConfig {
        honor_http_cache_freshness: true,
        ..CachingFetcherConfig::default()
      },
      DiskCacheConfig {
        allow_no_store: true,
        ..DiskCacheConfig::default()
      },
    );
    let first = seed.fetch(url).expect("seed fetch");
    assert_eq!(first.bytes, b"cached");
    assert_eq!(counter.load(Ordering::SeqCst), 1);

    let fail_count = Arc::new(AtomicUsize::new(0));
    let offline = DiskCachingFetcher::with_configs(
      FailingFetcher {
        count: Arc::clone(&fail_count),
      },
      tmp.path(),
      CachingFetcherConfig {
        honor_http_cache_freshness: true,
        ..CachingFetcherConfig::default()
      },
      DiskCacheConfig {
        allow_no_store: true,
        ..DiskCacheConfig::default()
      },
    );
    let second = offline.fetch(url).expect("disk cache fetch");
    assert_eq!(second.bytes, b"cached");
    assert_eq!(
      fail_count.load(Ordering::SeqCst),
      1,
      "should attempt refresh but fall back to cached disk entry"
    );
  }

  #[test]
  fn disk_cache_persists_empty_resources() {
    let tmp = tempfile::tempdir().unwrap();
    let counter = Arc::new(AtomicUsize::new(0));
    let fetcher = EmptyFetcher {
      count: Arc::clone(&counter),
    };
    let disk = DiskCachingFetcher::new(fetcher, tmp.path());
    let url = "https://example.com/empty.css";

    let first = disk.fetch(url).expect("first fetch");
    assert_eq!(counter.load(Ordering::SeqCst), 1);
    assert!(first.bytes.is_empty());
    assert_eq!(first.content_type.as_deref(), Some("text/css"));
    assert_eq!(first.final_url.as_deref(), Some(url));

    let second_fetcher = EmptyFetcher {
      count: Arc::clone(&counter),
    };
    let disk_again = DiskCachingFetcher::new(second_fetcher, tmp.path());
    let second = disk_again.fetch(url).expect("cached fetch");
    assert_eq!(counter.load(Ordering::SeqCst), 1, "should hit disk cache");
    assert!(second.bytes.is_empty());
    assert_eq!(second.content_type.as_deref(), Some("text/css"));
    assert_eq!(second.final_url.as_deref(), Some(url));
  }

  #[test]
  fn disk_cache_rebuild_preserves_empty_resources() {
    let tmp = tempfile::tempdir().unwrap();
    let counter = Arc::new(AtomicUsize::new(0));
    let fetcher = EmptyFetcher {
      count: Arc::clone(&counter),
    };
    let disk = DiskCachingFetcher::new(fetcher, tmp.path());
    let url = "https://example.com/empty-rebuild.css";

    let first = disk.fetch(url).expect("first fetch");
    assert_eq!(counter.load(Ordering::SeqCst), 1);
    assert!(first.bytes.is_empty());

    // Delete the journal to force the next instance to rebuild the index from the actual files.
    fs::remove_file(tmp.path().join("index.jsonl")).expect("remove journal");

    let second_fetcher = EmptyFetcher {
      count: Arc::clone(&counter),
    };
    let disk_again = DiskCachingFetcher::new(second_fetcher, tmp.path());
    let second = disk_again.fetch(url).expect("cached fetch");
    assert_eq!(
      counter.load(Ordering::SeqCst),
      1,
      "index rebuild should not discard zero-length entries"
    );
    assert!(second.bytes.is_empty());
  }

  #[test]
  fn stale_disk_entries_are_not_deleted_and_can_fallback_on_network_error() {
    let tmp = tempfile::tempdir().unwrap();
    let calls = Arc::new(AtomicUsize::new(0));
    let disk = DiskCachingFetcher::with_configs(
      FailingFetcher {
        count: Arc::clone(&calls),
      },
      tmp.path(),
      CachingFetcherConfig {
        honor_http_cache_freshness: true,
        ..CachingFetcherConfig::default()
      },
      DiskCacheConfig {
        max_bytes: 0,
        max_age: Some(Duration::from_secs(1)),
        ..DiskCacheConfig::default()
      },
    );

    let url = "https://example.com/stale-fallback";
    let cached_bytes = b"cached".to_vec();
    let data_path = disk.data_path(url);
    let meta_path = disk.meta_path_for_data(&data_path);
    fs::write(&data_path, &cached_bytes).unwrap();
    let meta = StoredMetadata {
      url: url.to_string(),
      status: None,
      content_type: Some("text/plain".to_string()),
      etag: None,
      last_modified: None,
      final_url: Some(url.to_string()),
      stored_at: now_seconds().saturating_sub(60),
      len: cached_bytes.len(),
      cache: None,
    };
    fs::write(&meta_path, serde_json::to_vec(&meta).unwrap()).unwrap();

    let res = disk
      .fetch(url)
      .expect("should fall back to cached disk entry");
    assert_eq!(
      calls.load(Ordering::SeqCst),
      1,
      "network fetch should be attempted"
    );
    assert_eq!(res.bytes, cached_bytes);
    assert!(data_path.exists(), "stale entry should not be deleted");
    assert!(meta_path.exists(), "stale metadata should not be deleted");
  }

  #[test]
  fn disk_max_age_causes_staleness_planning_without_deleting() {
    let tmp = tempfile::tempdir().unwrap();
    let calls = Arc::new(AtomicUsize::new(0));
    let disk = DiskCachingFetcher::with_configs(
      CountingFetcher {
        count: Arc::clone(&calls),
      },
      tmp.path(),
      CachingFetcherConfig {
        honor_http_cache_freshness: true,
        ..CachingFetcherConfig::default()
      },
      DiskCacheConfig {
        max_bytes: 0,
        max_age: Some(Duration::from_secs(1)),
        ..DiskCacheConfig::default()
      },
    );

    let url = "https://example.com/stale-refresh";
    let cached_bytes = b"cached".to_vec();
    let data_path = disk.data_path(url);
    let meta_path = disk.meta_path_for_data(&data_path);
    fs::write(&data_path, &cached_bytes).unwrap();
    let meta = StoredMetadata {
      url: url.to_string(),
      status: None,
      content_type: Some("text/plain".to_string()),
      etag: None,
      last_modified: None,
      final_url: Some(url.to_string()),
      stored_at: now_seconds().saturating_sub(60),
      len: cached_bytes.len(),
      cache: None,
    };
    fs::write(&meta_path, serde_json::to_vec(&meta).unwrap()).unwrap();

    let res = disk
      .fetch(url)
      .expect("stale entry should trigger a refresh fetch");
    assert_eq!(
      calls.load(Ordering::SeqCst),
      1,
      "stale entry should trigger a network call"
    );
    assert_eq!(res.bytes, b"hello");
  }

  #[test]
  fn partitions_cache_by_namespace() {
    let tmp = tempfile::tempdir().unwrap();
    let counter = Arc::new(AtomicUsize::new(0));
    let url = "https://example.com/resource";

    let memory_config = CachingFetcherConfig {
      honor_http_cache_freshness: true,
      ..CachingFetcherConfig::default()
    };

    let first_fetcher = CountingFetcher {
      count: Arc::clone(&counter),
    };
    let disk_a = DiskCachingFetcher::with_configs(
      first_fetcher,
      tmp.path(),
      memory_config,
      DiskCacheConfig {
        namespace: Some("namespace-a".to_string()),
        ..DiskCacheConfig::default()
      },
    );
    let _ = disk_a.fetch(url).expect("first fetch");
    assert_eq!(counter.load(Ordering::SeqCst), 1);

    let second_fetcher = CountingFetcher {
      count: Arc::clone(&counter),
    };
    let disk_b = DiskCachingFetcher::with_configs(
      second_fetcher,
      tmp.path(),
      memory_config,
      DiskCacheConfig {
        namespace: Some("namespace-b".to_string()),
        ..DiskCacheConfig::default()
      },
    );
    let _ = disk_b.fetch(url).expect("second fetch");
    assert_eq!(
      counter.load(Ordering::SeqCst),
      2,
      "different namespaces should not share disk cache entries"
    );
  }

  #[test]
  fn disk_cache_does_not_persist_transient_status_responses() {
    #[derive(Clone)]
    struct TransientFetcher {
      count: Arc<AtomicUsize>,
    }

    impl ResourceFetcher for TransientFetcher {
      fn fetch(&self, url: &str) -> Result<FetchedResource> {
        self.count.fetch_add(1, Ordering::SeqCst);
        let mut resource =
          FetchedResource::new(b"transient".to_vec(), Some("text/plain".to_string()));
        resource.status = Some(503);
        resource.final_url = Some(url.to_string());
        Ok(resource)
      }
    }

    let tmp = tempfile::tempdir().unwrap();
    let counter = Arc::new(AtomicUsize::new(0));
    let url = "https://example.com/transient";

    let first_fetcher = TransientFetcher {
      count: Arc::clone(&counter),
    };
    let disk = DiskCachingFetcher::new(first_fetcher, tmp.path());
    let first = disk.fetch(url).expect("first fetch");
    assert_eq!(first.bytes, b"transient");
    assert_eq!(counter.load(Ordering::SeqCst), 1);

    let second_fetcher = TransientFetcher {
      count: Arc::clone(&counter),
    };
    let disk_again = DiskCachingFetcher::new(second_fetcher, tmp.path());
    let second = disk_again.fetch(url).expect("second fetch");
    assert_eq!(second.bytes, b"transient");
    assert_eq!(
      counter.load(Ordering::SeqCst),
      2,
      "transient responses should not be persisted to disk"
    );
  }

  #[derive(Clone)]
  struct RedirectingFetcher {
    count: Arc<AtomicUsize>,
    target: String,
  }

  impl ResourceFetcher for RedirectingFetcher {
    fn fetch(&self, _url: &str) -> Result<FetchedResource> {
      self.count.fetch_add(1, Ordering::SeqCst);
      let mut resource =
        FetchedResource::new(b"redirected".to_vec(), Some("text/plain".to_string()));
      resource.final_url = Some(self.target.clone());
      Ok(resource)
    }
  }

  #[test]
  fn disk_cache_aliases_final_url() {
    let tmp = tempfile::tempdir().unwrap();
    let calls = Arc::new(AtomicUsize::new(0));
    let target = "https://example.com/final".to_string();

    let first_fetcher = RedirectingFetcher {
      count: Arc::clone(&calls),
      target: target.clone(),
    };
    let disk = DiskCachingFetcher::new(first_fetcher, tmp.path());
    let first = disk
      .fetch("https://example.com/start")
      .expect("initial fetch");
    assert_eq!(calls.load(Ordering::SeqCst), 1);
    assert_eq!(first.final_url.as_deref(), Some(target.as_str()));

    let second_fetcher = RedirectingFetcher {
      count: Arc::clone(&calls),
      target: target.clone(),
    };
    let disk_again = DiskCachingFetcher::new(second_fetcher, tmp.path());
    let second = disk_again.fetch(&target).expect("cached alias fetch");
    assert_eq!(
      calls.load(Ordering::SeqCst),
      1,
      "final URL should hit disk without new network call"
    );
    assert_eq!(second.bytes, first.bytes);
  }

  #[test]
  fn disk_cache_hits_respect_budget() {
    let Some(listener) = try_bind_localhost("disk_cache_hits_respect_budget") else {
      return;
    };
    let addr = listener.local_addr().unwrap();
    let body = b"disk";
    let handle = thread::spawn(move || {
      if let Some(stream) = listener.incoming().next() {
        let mut stream = stream.unwrap();
        let mut buf = [0u8; 512];
        let _ = stream.read(&mut buf);
        let response = format!(
          "HTTP/1.1 200 OK\r\nContent-Length: {}\r\nContent-Type: text/plain\r\nConnection: close\r\n\r\n",
          body.len()
        );
        let _ = stream.write_all(response.as_bytes());
        let _ = stream.write_all(body);
      }
    });

    let tmp = tempfile::tempdir().unwrap();
    let policy = ResourcePolicy::default().with_total_bytes_budget(Some(5));
    let url = format!("http://{}/resource", addr);
    let first_fetcher =
      DiskCachingFetcher::new(HttpFetcher::new().with_policy(policy.clone()), tmp.path())
        .with_policy(policy.clone());
    let first = first_fetcher.fetch(&url).expect("first fetch");
    assert_eq!(first.bytes, body);
    handle.join().unwrap();

    let second_fetcher =
      DiskCachingFetcher::new(HttpFetcher::new().with_policy(policy.clone()), tmp.path())
        .with_policy(policy);
    let err = second_fetcher
      .fetch(&url)
      .expect_err("budget should block disk cache hit");
    assert!(
      err.to_string().contains("budget"),
      "unexpected error: {err:?}"
    );
  }

  #[derive(Clone, Debug)]
  struct MockResponse {
    status: u16,
    body: Vec<u8>,
    etag: Option<String>,
    last_modified: Option<String>,
    cache_policy: Option<HttpCachePolicy>,
  }

  #[derive(Clone, Debug)]
  struct MockCall {
    url: String,
    etag: Option<String>,
    last_modified: Option<String>,
  }

  #[derive(Clone)]
  struct ScriptedFetcher {
    responses: Arc<Mutex<VecDeque<MockResponse>>>,
    calls: Arc<Mutex<Vec<MockCall>>>,
  }

  impl ScriptedFetcher {
    fn new(responses: Vec<MockResponse>) -> Self {
      Self {
        responses: Arc::new(Mutex::new(VecDeque::from(responses))),
        calls: Arc::new(Mutex::new(Vec::new())),
      }
    }

    fn record_call(&self, url: &str, etag: Option<&str>, last_modified: Option<&str>) {
      self.calls.lock().unwrap().push(MockCall {
        url: url.to_string(),
        etag: etag.map(|s| s.to_string()),
        last_modified: last_modified.map(|s| s.to_string()),
      });
    }

    fn next_response(&self) -> Result<FetchedResource> {
      let mut responses = self.responses.lock().unwrap();
      let resp = responses
        .pop_front()
        .expect("scripted fetcher ran out of responses");
      let mut resource = FetchedResource::new(resp.body.clone(), Some("text/plain".to_string()));
      resource.status = Some(resp.status);
      resource.etag = resp.etag.clone();
      resource.last_modified = resp.last_modified.clone();
      resource.cache_policy = resp.cache_policy.clone();
      Ok(resource)
    }

    fn calls(&self) -> Vec<MockCall> {
      self.calls.lock().unwrap().clone()
    }
  }

  impl ResourceFetcher for ScriptedFetcher {
    fn fetch(&self, url: &str) -> Result<FetchedResource> {
      self.record_call(url, None, None);
      self.next_response()
    }

    fn fetch_with_validation(
      &self,
      url: &str,
      etag: Option<&str>,
      last_modified: Option<&str>,
    ) -> Result<FetchedResource> {
      self.record_call(url, etag, last_modified);
      self.next_response()
    }
  }

  #[test]
  fn revalidates_and_updates_disk_metadata() {
    let tmp = tempfile::tempdir().unwrap();
    let fetcher = ScriptedFetcher::new(vec![
      MockResponse {
        status: 200,
        body: b"cached".to_vec(),
        etag: Some("etag1".to_string()),
        last_modified: Some("lm1".to_string()),
        cache_policy: None,
      },
      MockResponse {
        status: 304,
        body: Vec::new(),
        etag: Some("etag2".to_string()),
        last_modified: Some("lm2".to_string()),
        cache_policy: None,
      },
    ]);

    // Force stale planning so the second fetch revalidates and updates metadata.
    let disk = DiskCachingFetcher::with_configs(
      fetcher.clone(),
      tmp.path(),
      CachingFetcherConfig {
        honor_http_cache_freshness: true,
        ..CachingFetcherConfig::default()
      },
      DiskCacheConfig {
        max_bytes: 0,
        max_age: Some(Duration::from_secs(0)),
        ..DiskCacheConfig::default()
      },
    );
    let url = "https://example.com/resource";

    let first = disk.fetch(url).expect("initial fetch");
    assert_eq!(first.bytes, b"cached");

    let second = disk.fetch(url).expect("revalidated fetch");
    assert_eq!(second.bytes, b"cached");

    let calls = fetcher.calls();
    assert_eq!(calls.len(), 2);
    assert_eq!(calls[0].etag, None);
    assert_eq!(calls[1].etag.as_deref(), Some("etag1"));
    assert_eq!(calls[1].last_modified.as_deref(), Some("lm1"));

    let data_path = disk.data_path(url);
    let meta_path = disk.meta_path_for_data(&data_path);
    let meta_bytes = fs::read(meta_path).expect("meta present");
    let meta: StoredMetadata = serde_json::from_slice(&meta_bytes).expect("valid meta");
    assert_eq!(meta.etag.as_deref(), Some("etag2"));
    assert_eq!(meta.last_modified.as_deref(), Some("lm2"));
  }

  #[test]
  fn evicts_without_full_scans_per_write() {
    let tmp = tempfile::tempdir().unwrap();
    let counter = Arc::new(AtomicUsize::new(0));
    let fetcher = SizedFetcher {
      count: Arc::clone(&counter),
      size: 32,
    };
    let disk = DiskCachingFetcher::with_configs(
      fetcher,
      tmp.path(),
      CachingFetcherConfig::default(),
      DiskCacheConfig {
        max_bytes: 96,
        max_age: None,
        ..DiskCacheConfig::default()
      },
    );

    for i in 0..8 {
      let url = format!("https://example.com/resource/{i}");
      let res = disk.fetch(&url).expect("fetch");
      assert_eq!(res.bytes.len(), 32);
    }

    let stats = disk.debug_index_stats();
    assert_eq!(
      stats.rebuilds, 1,
      "index should only rebuild once instead of scanning on every persist"
    );

    let bin_files = fs::read_dir(tmp.path())
      .unwrap()
      .flatten()
      .filter(|entry| {
        entry
          .path()
          .extension()
          .and_then(|ext| ext.to_str())
          .map(|ext| ext == "bin")
          == Some(true)
      })
      .count();
    assert!(
      bin_files < 8,
      "eviction should have removed older entries once the budget was exceeded"
    );
    assert!(
      (bin_files as u64) * 32 <= 96,
      "remaining entries should fit within the configured byte budget"
    );
  }

  #[test]
  fn refresh_is_not_evicted_between_lock_release_and_journal_insert() {
    #[derive(Clone)]
    struct PanicFetcher;

    impl ResourceFetcher for PanicFetcher {
      fn fetch(&self, _url: &str) -> Result<FetchedResource> {
        panic!("inner fetcher should not be called on a disk cache hit");
      }
    }

    #[derive(Clone)]
    struct NoopFetcher;

    impl ResourceFetcher for NoopFetcher {
      fn fetch(&self, _url: &str) -> Result<FetchedResource> {
        unreachable!("test uses persist_resource directly");
      }
    }

    let tmp = tempfile::tempdir().unwrap();
    let entry_size = 32usize;
    let max_bytes = (entry_size * 3) as u64;
    let memory_config = CachingFetcherConfig {
      honor_http_cache_freshness: true,
      ..CachingFetcherConfig::default()
    };
    let disk_config = DiskCacheConfig {
      max_bytes,
      max_age: Some(Duration::from_secs(60)),
      ..DiskCacheConfig::default()
    };

    let url_target = "https://example.com/race-target";
    let url_other1 = "https://example.com/race-other1";
    let url_other2 = "https://example.com/race-other2";

    // Seed three entries (exactly at the budget) so that a single extra insert triggers eviction.
    let seeder =
      DiskCachingFetcher::with_configs(NoopFetcher, tmp.path(), memory_config, disk_config.clone());
    for (url, byte) in [(url_target, b'a'), (url_other1, b'b'), (url_other2, b'c')] {
      let mut resource =
        FetchedResource::new(vec![byte; entry_size], Some("text/plain".to_string()));
      resource.final_url = Some(url.to_string());
      seeder.persist_resource(url, &resource, None, None, None);
    }

    let hook = PersistResourceHook::new();

    let cache_dir = tmp.path().to_path_buf();
    let writer_hook = Arc::clone(&hook);
    let writer_config = disk_config.clone();
    let writer = thread::spawn(move || {
      let disk =
        DiskCachingFetcher::with_configs(NoopFetcher, cache_dir, memory_config, writer_config)
          .with_persist_resource_hook(writer_hook);

      let mut refreshed =
        FetchedResource::new(vec![b'r'; entry_size], Some("text/plain".to_string()));
      refreshed.final_url = Some(url_target.to_string());
      disk.persist_resource(url_target, &refreshed, None, None, None);
    });

    let cache_dir = tmp.path().to_path_buf();
    let evict_hook = Arc::clone(&hook);
    let evict_config = disk_config.clone();
    let evicter = thread::spawn(move || {
      // Wait until the writer has renamed files into place but hasn't updated the index yet.
      evict_hook.wait_before_index();

      let disk =
        DiskCachingFetcher::with_configs(NoopFetcher, cache_dir, memory_config, evict_config);
      let mut resource =
        FetchedResource::new(vec![b'n'; entry_size], Some("text/plain".to_string()));
      resource.final_url = Some("https://example.com/race-new".to_string());
      disk.persist_resource(
        resource.final_url.as_ref().unwrap(),
        &resource,
        None,
        None,
        None,
      );

      evict_hook.wait_after_eviction();
    });

    writer.join().unwrap();
    evicter.join().unwrap();

    // A fresh fetcher instance should hit disk and serve the refreshed bytes without touching the
    // inner fetcher (which would panic).
    let reader =
      DiskCachingFetcher::with_configs(PanicFetcher, tmp.path(), memory_config, disk_config);
    let res = reader.fetch(url_target).expect("disk cache hit");
    assert_eq!(res.bytes, vec![b'r'; entry_size]);
  }

  #[test]
  fn rebuild_cleans_corrupt_or_partial_entries() {
    let tmp = tempfile::tempdir().unwrap();
    let dangling_data = tmp.path().join("dangling.bin");
    let dangling_meta = tmp.path().join("dangling.bin.meta");
    fs::write(&dangling_data, b"garbage").unwrap();
    // Missing metadata should be cleaned up.
    assert!(!dangling_meta.exists());

    let corrupt_data = tmp.path().join("corrupt.bin");
    let corrupt_meta = tmp.path().join("corrupt.bin.meta");
    fs::write(&corrupt_data, b"12345").unwrap();
    fs::write(&corrupt_meta, b"{not json").unwrap();

    let counter = Arc::new(AtomicUsize::new(0));
    let fetcher = CountingFetcher {
      count: Arc::clone(&counter),
    };
    let disk = DiskCachingFetcher::new(fetcher, tmp.path());
    let url = "https://example.com/real";
    let res = disk.fetch(url).expect("fetch after rebuilding index");
    assert_eq!(res.bytes, b"hello");

    assert!(
      !dangling_data.exists(),
      "dangling entries without metadata should be removed"
    );
    assert!(
      !corrupt_data.exists() && !corrupt_meta.exists(),
      "corrupt metadata entries should be purged during rebuild"
    );
  }

  #[test]
  fn removes_corrupted_entries_on_read() {
    let tmp = tempfile::tempdir().unwrap();
    let counter = Arc::new(AtomicUsize::new(0));
    let fetcher = CountingFetcher { count: counter };
    let disk = DiskCachingFetcher::new(fetcher, tmp.path());
    let url = "https://example.com/corrupted";

    let mut resource =
      FetchedResource::new(b"complete-body".to_vec(), Some("text/plain".to_string()));
    resource.final_url = Some(url.to_string());
    disk.persist_resource(url, &resource, None, None, None);

    let data_path = disk.data_path(url);
    let meta_path = disk.meta_path_for_data(&data_path);
    assert!(data_path.exists());
    assert!(meta_path.exists());

    fs::write(&data_path, b"tiny").unwrap();

    let snapshot = disk.read_disk_entry(url).expect("read_disk_entry");
    assert!(snapshot.is_none(), "corrupted entry should be discarded");
    assert!(!data_path.exists());
    assert!(!meta_path.exists());
  }

  #[test]
  fn ignores_temporary_files_during_reads() {
    let tmp = tempfile::tempdir().unwrap();
    let counter = Arc::new(AtomicUsize::new(0));
    let fetcher = CountingFetcher {
      count: Arc::clone(&counter),
    };
    let disk = DiskCachingFetcher::new(fetcher, tmp.path());
    let url = "https://example.com/tmp-files";

    let mut resource = FetchedResource::new(b"persistent".to_vec(), Some("text/plain".to_string()));
    resource.final_url = Some(url.to_string());
    disk.persist_resource(url, &resource, None, None, None);

    let data_path = disk.data_path(url);
    let meta_path = disk.meta_path_for_data(&data_path);
    fs::write(tmp_path(&data_path), b"junk").unwrap();
    fs::write(tmp_path(&meta_path), b"junk").unwrap();

    let (_, snapshot) = disk
      .read_disk_entry(url)
      .expect("read_disk_entry")
      .expect("entry should be readable");
    let cached = snapshot
      .value
      .as_result()
      .expect("resource should deserialize");
    assert_eq!(cached.bytes, b"persistent");
    assert_eq!(cached.content_type.as_deref(), Some("text/plain"));
  }

  #[test]
  fn concurrent_persists_keep_entry_valid() {
    let tmp = tempfile::tempdir().unwrap();
    let counter = Arc::new(AtomicUsize::new(0));
    let fetcher = CountingFetcher {
      count: Arc::clone(&counter),
    };
    let disk = Arc::new(DiskCachingFetcher::new(fetcher, tmp.path()));
    let url = "https://example.com/concurrent";
    let bodies: Vec<Vec<u8>> = vec![
      b"first".to_vec(),
      b"second-version".to_vec(),
      b"third-version-with-more-bytes".to_vec(),
    ];

    let mut handles = Vec::new();
    for body in bodies.clone() {
      let disk = Arc::clone(&disk);
      let url = url.to_string();
      handles.push(thread::spawn(move || {
        let mut resource = FetchedResource::new(body, Some("text/plain".to_string()));
        resource.final_url = Some(url.clone());
        disk.persist_resource(&url, &resource, None, None, None);
      }));
    }

    for handle in handles {
      handle.join().expect("thread should not panic");
    }

    let (_, snapshot) = disk
      .read_disk_entry(url)
      .expect("read_disk_entry")
      .expect("entry should be present after concurrent persists");
    let resource = snapshot
      .value
      .as_result()
      .expect("resource should deserialize");
    assert!(
      bodies.iter().any(|b| *b == resource.bytes),
      "final entry should match one of the persisted bodies"
    );
    assert_eq!(resource.content_type.as_deref(), Some("text/plain"));
    assert_eq!(resource.final_url.as_deref(), Some(url));
  }

  #[test]
  fn serves_fresh_entries_without_revalidation() {
    let tmp = tempfile::tempdir().unwrap();
    let fetcher = ScriptedFetcher::new(vec![
      MockResponse {
        status: 200,
        body: b"cached".to_vec(),
        etag: Some("etag1".to_string()),
        last_modified: Some("lm1".to_string()),
        cache_policy: Some(HttpCachePolicy {
          max_age: Some(3600),
          ..Default::default()
        }),
      },
      MockResponse {
        status: 304,
        body: Vec::new(),
        etag: None,
        last_modified: None,
        cache_policy: None,
      },
    ]);

    let disk = DiskCachingFetcher::new(fetcher.clone(), tmp.path());
    let url = "https://example.com/fresh";

    let first = disk.fetch(url).expect("initial fetch");
    assert_eq!(first.bytes, b"cached");

    let second = disk.fetch(url).expect("cached fetch");
    assert_eq!(second.bytes, b"cached");

    assert_eq!(
      fetcher.calls().len(),
      1,
      "fresh disk entries should be served without revalidation"
    );
  }

  #[derive(Clone)]
  struct PanicFetcher;

  impl ResourceFetcher for PanicFetcher {
    fn fetch(&self, _url: &str) -> Result<FetchedResource> {
      panic!("inner fetch should not be called");
    }
  }

  #[test]
  fn serves_stale_entries_without_revalidation_when_deadline_active() {
    let tmp = tempfile::tempdir().unwrap();
    let url = "https://example.com/stale";
    let disk = DiskCachingFetcher::with_configs(
      PanicFetcher,
      tmp.path(),
      CachingFetcherConfig {
        honor_http_cache_freshness: true,
        stale_policy: CacheStalePolicy::UseStaleWhenDeadline,
        ..CachingFetcherConfig::default()
      },
      DiskCacheConfig {
        max_bytes: 0,
        // Force entries to be treated as stale so the default plan would revalidate.
        max_age: Some(Duration::from_secs(0)),
        ..DiskCacheConfig::default()
      },
    );

    let mut resource = FetchedResource::new(b"cached".to_vec(), Some("text/css".to_string()));
    resource.final_url = Some(url.to_string());
    resource.etag = Some("etag1".to_string());
    disk.persist_resource(url, &resource, resource.etag.as_deref(), None, None);

    let deadline = render_control::RenderDeadline::new(Some(Duration::from_secs(1)), None);
    let fetched =
      render_control::with_deadline(Some(&deadline), || disk.fetch(url)).expect("disk hit");
    assert_eq!(fetched.bytes, b"cached");
  }

  #[test]
  fn removes_stale_lock_files() {
    let tmp = tempfile::tempdir().unwrap();
    let url = "https://example.com/stale-lock";
    let disk = DiskCachingFetcher::new(PanicFetcher, tmp.path());
    let data_path = disk.data_path(url);
    let lock_path = lock_path_for(&data_path);

    // Simulate a crash right after creating the lock file (before the writer
    // can write structured PID metadata into it).
    fs::write(&lock_path, b"").unwrap();
    filetime::set_file_mtime(&lock_path, FileTime::from_unix_time(0, 0)).unwrap();

    assert!(!disk.lock_is_active(&data_path));
    assert!(!lock_path.exists());
  }

  #[test]
  fn acquire_lock_times_out_when_entry_is_locked() {
    let tmp = tempfile::tempdir().unwrap();
    let url = "https://example.com/lock-timeout";
    let disk = DiskCachingFetcher::new(PanicFetcher, tmp.path());
    let data_path = disk.data_path(url);
    let lock_path = lock_path_for(&data_path);

    let contents = LockFileContents {
      pid: std::process::id(),
      started_at: now_seconds(),
    };
    fs::write(&lock_path, serde_json::to_vec(&contents).unwrap()).unwrap();

    let start = Instant::now();
    let lock = disk.acquire_lock(&data_path);
    let elapsed = start.elapsed();

    assert!(lock.is_none(), "should not acquire lock while it exists");
    assert!(
      elapsed < LOCK_WAIT_TIMEOUT + Duration::from_millis(250),
      "acquire_lock waited too long: {elapsed:?}"
    );
  }

  #[cfg(unix)]
  #[test]
  fn removes_lock_files_for_dead_pid_even_when_fresh() {
    let tmp = tempfile::tempdir().unwrap();
    let url = "https://example.com/dead-pid-lock";
    let disk = DiskCachingFetcher::new(PanicFetcher, tmp.path());
    let data_path = disk.data_path(url);
    let lock_path = lock_path_for(&data_path);

    // Pick a PID value that should never exist on typical systems (Linux pid_max defaults to a
    // much smaller value), so `kill(pid, 0)` deterministically returns ESRCH.
    let pid = i32::MAX as u32;
    assert_eq!(
      pid_is_alive(pid),
      Some(false),
      "test requires a PID that is no longer alive"
    );

    let contents = LockFileContents {
      pid,
      started_at: now_seconds(),
    };
    fs::write(&lock_path, serde_json::to_vec(&contents).unwrap()).unwrap();

    assert!(!disk.lock_is_active(&data_path));
    assert!(!lock_path.exists());
  }

  #[test]
  fn acquire_lock_respects_deadline_buffer() {
    let tmp = tempfile::tempdir().unwrap();
    let url = "https://example.com/deadline-buffer-lock";
    let disk = DiskCachingFetcher::new(PanicFetcher, tmp.path());
    let data_path = disk.data_path(url);
    let lock_path = lock_path_for(&data_path);

    let contents = LockFileContents {
      pid: std::process::id(),
      started_at: now_seconds(),
    };
    fs::write(&lock_path, serde_json::to_vec(&contents).unwrap()).unwrap();

    let deadline = render_control::RenderDeadline::new(Some(LOCK_DEADLINE_BUFFER / 2), None);
    let start = Instant::now();
    let lock = render_control::with_deadline(Some(&deadline), || disk.acquire_lock(&data_path));
    let elapsed = start.elapsed();

    assert!(
      lock.is_none(),
      "should not acquire lock within deadline buffer"
    );
    assert!(
      elapsed < Duration::from_millis(50),
      "acquire_lock should return quickly under tight deadlines: {elapsed:?}"
    );
  }

  #[test]
  fn stale_lock_removal_failure_is_treated_as_active() {
    let tmp = tempfile::tempdir().unwrap();
    let url = "https://example.com/unremovable-stale-lock";
    let disk = DiskCachingFetcher::new(PanicFetcher, tmp.path());
    let data_path = disk.data_path(url);
    let lock_path = lock_path_for(&data_path);

    // Make lock removal fail deterministically by creating a directory at the lock path.
    // `remove_file` can't delete directories, regardless of permissions.
    fs::create_dir(&lock_path).unwrap();
    filetime::set_file_mtime(&lock_path, FileTime::from_unix_time(0, 0)).unwrap();

    assert!(
      disk.lock_is_active(&data_path),
      "should treat stale lock as active when removal fails"
    );
    assert!(lock_path.exists(), "lock file should remain on disk");
  }

  #[test]
  fn waits_for_lock_then_reads_cached_entry() {
    let tmp = tempfile::tempdir().unwrap();
    let url = "https://example.com/locked";
    let disk = DiskCachingFetcher::new(PanicFetcher, tmp.path());

    let bytes = b"from-disk".to_vec();
    let data_path = disk.data_path(url);
    let meta_path = disk.meta_path_for_data(&data_path);
    fs::write(&data_path, &bytes).unwrap();
    let meta = StoredMetadata {
      url: url.to_string(),
      status: None,
      content_type: Some("text/plain".to_string()),
      etag: None,
      last_modified: None,
      final_url: Some(url.to_string()),
      stored_at: now_seconds(),
      len: bytes.len(),
      cache: None,
    };
    fs::write(&meta_path, serde_json::to_vec(&meta).unwrap()).unwrap();

    let lock_path = lock_path_for(&data_path);
    fs::write(&lock_path, b"").unwrap();
    let handle = thread::spawn({
      let lock_path = lock_path.clone();
      move || {
        thread::sleep(Duration::from_millis(50));
        fs::remove_file(lock_path).unwrap();
      }
    });

    let res = disk.fetch(url).expect("fetch");
    assert_eq!(res.bytes, bytes);
    assert_eq!(res.content_type.as_deref(), Some("text/plain"));
    handle.join().unwrap();
  }

  #[test]
  fn memory_hits_do_not_wait_for_disk_locks() {
    let tmp = tempfile::tempdir().unwrap();
    let counter = Arc::new(AtomicUsize::new(0));
    let disk = DiskCachingFetcher::new(
      CountingFetcher {
        count: Arc::clone(&counter),
      },
      tmp.path(),
    );
    let url = "https://example.com/memory-hit-lock";

    let first = disk.fetch(url).expect("initial fetch");
    assert_eq!(first.bytes, b"hello");
    assert_eq!(counter.load(Ordering::SeqCst), 1);

    let data_path = disk.data_path(url);
    let lock_path = lock_path_for(&data_path);
    let contents = LockFileContents {
      pid: std::process::id(),
      started_at: now_seconds(),
    };
    fs::write(&lock_path, serde_json::to_vec(&contents).unwrap()).unwrap();

    let handle = thread::spawn({
      let lock_path = lock_path.clone();
      move || {
        thread::sleep(READ_LOCK_WAIT_TIMEOUT * 2);
        let _ = fs::remove_file(lock_path);
      }
    });

    let start = Instant::now();
    let second = disk.fetch(url).expect("memory fetch");
    let elapsed = start.elapsed();
    assert_eq!(second.bytes, b"hello");
    assert_eq!(counter.load(Ordering::SeqCst), 1);
    assert!(
      elapsed < READ_LOCK_WAIT_TIMEOUT / 2,
      "memory hits should not wait on disk locks: {elapsed:?}"
    );

    handle.join().unwrap();
  }

  #[derive(Clone)]
  struct NoStoreFetcher {
    count: Arc<AtomicUsize>,
  }

  impl ResourceFetcher for NoStoreFetcher {
    fn fetch(&self, url: &str) -> Result<FetchedResource> {
      self.count.fetch_add(1, Ordering::SeqCst);
      let mut resource = FetchedResource::new(b"network".to_vec(), Some("text/plain".to_string()));
      resource.final_url = Some(url.to_string());
      resource.cache_policy = Some(HttpCachePolicy {
        no_store: true,
        ..Default::default()
      });
      Ok(resource)
    }
  }

  #[test]
  fn falls_back_to_inner_fetch_if_lock_persists() {
    let tmp = tempfile::tempdir().unwrap();
    let calls = Arc::new(AtomicUsize::new(0));
    let disk = DiskCachingFetcher::new(
      NoStoreFetcher {
        count: Arc::clone(&calls),
      },
      tmp.path(),
    );
    let url = "https://example.com/locked-miss";
    let data_path = disk.data_path(url);
    fs::write(lock_path_for(&data_path), b"").unwrap();

    let res = disk.fetch(url).expect("fetch");
    assert_eq!(calls.load(Ordering::SeqCst), 1);
    assert_eq!(res.bytes, b"network");
  }
}
