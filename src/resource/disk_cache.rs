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
use crate::error::Error;
use crate::Result;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::fs::{self, OpenOptions};
use std::io::ErrorKind;
use std::path::Path;
use std::path::PathBuf;
use std::thread;
use std::time::Duration;
use std::time::Instant;
use std::time::SystemTime;
use std::time::UNIX_EPOCH;

#[derive(Debug, Clone)]
pub struct DiskCacheConfig {
  /// Maximum total bytes to keep on disk. `0` disables eviction.
  pub max_bytes: u64,
  /// Maximum age for entries before they are treated as stale and re-fetched.
  pub max_age: Option<Duration>,
}

impl Default for DiskCacheConfig {
  fn default() -> Self {
    Self {
      max_bytes: 512 * 1024 * 1024,
      max_age: Some(Duration::from_secs(60 * 60 * 24 * 7)), // 7 days
    }
  }
}

const LOCK_STALE_AFTER: Duration = Duration::from_secs(30);
const LOCK_WAIT_TIMEOUT: Duration = Duration::from_millis(500);
const LOCK_RETRY_DELAY: Duration = Duration::from_millis(10);

#[derive(Debug, Clone)]
pub struct DiskCachingFetcher<F: ResourceFetcher> {
  memory: CachingFetcher<F>,
  cache_dir: PathBuf,
  disk_config: DiskCacheConfig,
}

struct EntryLock {
  path: PathBuf,
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

fn lock_age(lock_path: &Path) -> Option<Duration> {
  fs::metadata(lock_path)
    .ok()
    .and_then(|meta| lock_age_from_metadata(&meta))
}

impl<F: ResourceFetcher> DiskCachingFetcher<F> {
  pub fn new(inner: F, cache_dir: impl Into<PathBuf>) -> Self {
    Self::with_configs(
      inner,
      cache_dir,
      CachingFetcherConfig::default(),
      DiskCacheConfig::default(),
    )
  }

  pub fn with_configs(
    inner: F,
    cache_dir: impl Into<PathBuf>,
    memory_config: CachingFetcherConfig,
    disk_config: DiskCacheConfig,
  ) -> Self {
    let cache_dir = cache_dir.into();
    let _ = fs::create_dir_all(&cache_dir);
    Self {
      memory: CachingFetcher::with_config(inner, memory_config),
      cache_dir,
      disk_config,
    }
  }

  fn should_skip_disk(&self, url: &str) -> bool {
    url.starts_with("data:")
  }

  fn cache_key(url: &str) -> String {
    let mut hasher = Sha256::new();
    hasher.update(url.as_bytes());
    let digest = hasher.finalize();
    digest.iter().map(|b| format!("{:02x}", b)).collect()
  }

  fn data_path(&self, url: &str) -> PathBuf {
    self.cache_dir.join(format!("{}.bin", Self::cache_key(url)))
  }

  fn meta_path_for_data(&self, data_path: &Path) -> PathBuf {
    let mut meta_path = data_path.to_path_buf();
    meta_path.set_extension("bin.meta");
    meta_path
  }

  fn lock_is_active(&self, data_path: &Path) -> bool {
    let lock_path = lock_path_for(data_path);
    match fs::metadata(&lock_path) {
      Ok(meta) => {
        if let Some(age) = lock_age_from_metadata(&meta) {
          if age > LOCK_STALE_AFTER {
            let _ = fs::remove_file(&lock_path);
            return false;
          }
        }
        true
      }
      Err(err) => err.kind() != ErrorKind::NotFound,
    }
  }

  fn acquire_lock(&self, data_path: &Path) -> Option<EntryLock> {
    let lock_path = lock_path_for(data_path);
    let deadline = Instant::now() + LOCK_WAIT_TIMEOUT;

    loop {
      match OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(&lock_path)
      {
        Ok(_) => return Some(EntryLock { path: lock_path }),
        Err(err) if err.kind() == ErrorKind::AlreadyExists => {
          if let Some(age) = lock_age(&lock_path) {
            if age > LOCK_STALE_AFTER {
              let _ = fs::remove_file(&lock_path);
              continue;
            }
          }

          if Instant::now() >= deadline {
            return None;
          }

          thread::sleep(LOCK_RETRY_DELAY);
        }
        Err(_) => return None,
      }
    }
  }

  fn remove_entry_if_unlocked(&self, data_path: &Path, meta_path: &Path) {
    if !self.lock_is_active(data_path) {
      let _ = self.remove_entry(data_path, meta_path);
    }
  }

  fn read_disk_entry(&self, url: &str) -> Option<CachedSnapshot> {
    let data_path = self.data_path(url);
    let meta_path = self.meta_path_for_data(&data_path);

    if self.lock_is_active(&data_path) {
      return None;
    }

    let meta_bytes = fs::read(&meta_path).ok()?;
    let meta: StoredMetadata = match serde_json::from_slice(&meta_bytes) {
      Ok(meta) => meta,
      Err(_) => {
        self.remove_entry_if_unlocked(&data_path, &meta_path);
        return None;
      }
    };

    if let Some(max_age) = self.disk_config.max_age {
      if now_seconds().saturating_sub(meta.stored_at) > max_age.as_secs() {
        self.remove_entry_if_unlocked(&data_path, &meta_path);
        return None;
      }
    }

    let bytes = match fs::read(&data_path) {
      Ok(bytes) => bytes,
      Err(_) => {
        self.remove_entry_if_unlocked(&data_path, &meta_path);
        return None;
      }
    };
    if bytes.is_empty() || meta.len != bytes.len() {
      self.remove_entry_if_unlocked(&data_path, &meta_path);
      return None;
    }

    let mut resource = FetchedResource::with_final_url(
      bytes,
      meta.content_type.clone(),
      meta.final_url.clone().or_else(|| Some(url.to_string())),
    );
    resource.etag = meta.etag.clone();
    resource.last_modified = meta.last_modified.clone();
    resource.cache_policy = meta.cache.as_ref().map(|c| c.to_policy());

    Some(CachedSnapshot {
      value: super::CacheValue::Resource(resource),
      etag: meta.etag,
      last_modified: meta.last_modified,
      http_cache: meta.cache.and_then(|c| c.to_http()),
    })
  }

  fn persist_snapshot(&self, url: &str, snapshot: &CachedSnapshot) {
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
    if self.should_skip_disk(url) {
      return;
    }

    let data_path = self.data_path(url);
    if let Some(parent) = data_path.parent() {
      let _ = fs::create_dir_all(parent);
    }

    let Some(_lock) = self.acquire_lock(&data_path) else {
      return;
    };

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
    if cache_metadata.as_ref().map(|m| m.no_store).unwrap_or(false)
      || resource
        .cache_policy
        .as_ref()
        .map(|p| p.no_store)
        .unwrap_or(false)
    {
      let meta_path = self.meta_path_for_data(&data_path);
      let _ = self.remove_entry(&data_path, &meta_path);
      return;
    }

    let meta_path = self.meta_path_for_data(&data_path);
    let data_tmp = tmp_path(&data_path);
    let meta_tmp = tmp_path(&meta_path);

    let meta = StoredMetadata {
      url: url.to_string(),
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

    if fs::write(&data_tmp, &resource.bytes).is_err() {
      let _ = fs::remove_file(&data_tmp);
      let _ = fs::remove_file(&meta_tmp);
      return;
    }

    let Ok(serialized) = serde_json::to_vec(&meta) else {
      let _ = fs::remove_file(&data_tmp);
      let _ = fs::remove_file(&meta_tmp);
      return;
    };

    if fs::write(&meta_tmp, &serialized).is_err() {
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
      let _ = self.remove_entry(&data_path, &meta_path);
      return;
    }

    self.enforce_disk_limits();
  }

  fn remove_entry(&self, data_path: &Path, meta_path: &Path) -> Result<()> {
    let _ = fs::remove_file(data_path);
    let _ = fs::remove_file(meta_path);
    let _ = fs::remove_file(tmp_path(data_path));
    let _ = fs::remove_file(tmp_path(meta_path));
    Ok(())
  }

  fn enforce_disk_limits(&self) {
    let max_bytes = self.disk_config.max_bytes;
    if max_bytes == 0 {
      return;
    }

    let mut entries: Vec<(u64, u64, PathBuf)> = Vec::new();
    let mut total: u64 = 0;

    if let Ok(read_dir) = fs::read_dir(&self.cache_dir) {
      for entry in read_dir.flatten() {
        let path = entry.path();
        if path
          .extension()
          .and_then(|e| e.to_str())
          .map(|ext| ext == "bin")
          != Some(true)
        {
          continue;
        }

        if self.lock_is_active(&path) {
          continue;
        }

        let meta_path = self.meta_path_for_data(&path);
        let Ok(meta_bytes) = fs::read(&meta_path) else {
          let _ = self.remove_entry(&path, &meta_path);
          continue;
        };
        let Ok(meta) = serde_json::from_slice::<StoredMetadata>(&meta_bytes) else {
          let _ = self.remove_entry(&path, &meta_path);
          continue;
        };

        let size = meta.len as u64;
        entries.push((meta.stored_at, size, path));
        total = total.saturating_add(size);
      }
    }

    entries.sort_by_key(|(stored_at, _, _)| *stored_at);

    for (_, size, path) in entries {
      if total <= max_bytes {
        break;
      }
      let meta_path = self.meta_path_for_data(&path);
      if self.lock_is_active(&path) {
        continue;
      }
      let _ = self.remove_entry(&path, &meta_path);
      total = total.saturating_sub(size);
    }
  }
}

impl<F: ResourceFetcher> ResourceFetcher for DiskCachingFetcher<F> {
  fn fetch(&self, url: &str) -> Result<FetchedResource> {
    let disk_snapshot = self.read_disk_entry(url);
    if let Some(ref snapshot) = disk_snapshot {
      self.memory.prime_cache_with_snapshot(url, snapshot.clone());
    }

    let cached = self.memory.cached_snapshot(url).or(disk_snapshot);
    let plan = self
      .memory
      .plan_cache_use(url, cached.clone(), self.disk_config.max_age);

    if let CacheAction::UseCached = plan.action {
      if let Some(snapshot) = plan.cached.as_ref() {
        super::record_cache_fresh_hit();
        return snapshot.value.as_result();
      }
    }

    let (flight, is_owner) = self.memory.join_inflight(url);
    if !is_owner {
      return flight.wait();
    }

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

    let mut shared: Option<super::SharedResult> = None;

    let result = match fetch_result {
      Ok(res) => {
        if res.is_not_modified() {
          if let Some(snapshot) = plan.cached.as_ref() {
            let value = snapshot.value.as_result();
            if let Ok(ref ok) = value {
              let stored_at = SystemTime::now();
              let should_store = !res
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
                self.memory.insert_cache(
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
                );
                if let Some(snapshot) = self.memory.cached_snapshot(url) {
                  self.persist_snapshot(url, &snapshot);
                }
              } else {
                self.memory.remove_cached(url);
                let data_path = self.data_path(url);
                let meta_path = self.meta_path_for_data(&data_path);
                let _ = self.remove_entry(&data_path, &meta_path);
              }
            }
            super::record_cache_revalidated_hit();
            shared = value
              .as_ref()
              .ok()
              .map(|v| super::SharedResult::Success(v.clone()));
            value
          } else {
            Err(Error::Other(
              "Received 304 without cached entry".to_string(),
            ))
          }
        } else {
          let stored_at = SystemTime::now();
          if let Some(entry) = self.memory.build_cache_entry(&res, stored_at) {
            self.memory.insert_cache(url, entry);
          } else if res
            .cache_policy
            .as_ref()
            .map(|p| p.no_store)
            .unwrap_or(false)
          {
            self.memory.remove_cached(url);
            let data_path = self.data_path(url);
            let meta_path = self.meta_path_for_data(&data_path);
            let _ = self.remove_entry(&data_path, &meta_path);
          }
          if let Some(snapshot) = self.memory.cached_snapshot(url) {
            self.persist_snapshot(url, &snapshot);
          } else {
            let http_cache = res
              .cache_policy
              .as_ref()
              .and_then(|p| CachedHttpMetadata::from_policy(p, stored_at));
            self.persist_resource(
              url,
              &res,
              res.etag.as_deref(),
              res.last_modified.as_deref(),
              http_cache.as_ref(),
            );
          }
          super::record_cache_miss();
          shared = Some(super::SharedResult::Success(res.clone()));
          Ok(res)
        }
      }
      Err(err) => {
        if let Some(snapshot) = plan.cached.as_ref() {
          let fallback = snapshot.value.as_result();
          if let Ok(ok) = &fallback {
            shared = Some(super::SharedResult::Success(ok.clone()));
          }
          fallback
        } else {
          if self.memory.config.cache_errors {
            self.memory.insert_cache(
              url,
              super::CacheEntry {
                value: super::CacheValue::Error(err.clone()),
                etag: None,
                last_modified: None,
                http_cache: None,
              },
            );
          }
          shared = Some(super::SharedResult::Error(err.clone()));
          Err(err)
        }
      }
    };

    let notify = shared.unwrap_or_else(|| match &result {
      Ok(res) => super::SharedResult::Success(res.clone()),
      Err(err) => super::SharedResult::Error(err.clone()),
    });
    self.memory.finish_inflight(url, &flight, notify);

    result
  }
}

#[derive(Debug, Serialize, Deserialize)]
struct StoredMetadata {
  url: String,
  content_type: Option<String>,
  etag: Option<String>,
  last_modified: Option<String>,
  final_url: Option<String>,
  stored_at: u64,
  len: usize,
  cache: Option<StoredCacheMetadata>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
struct StoredCacheMetadata {
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
  use super::*;
  use std::collections::VecDeque;
  use std::sync::atomic::AtomicUsize;
  use std::sync::atomic::Ordering;
  use std::sync::Arc;
  use std::sync::Mutex;
  use std::thread;

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

  #[derive(Clone, Debug)]
  struct MockResponse {
    status: u16,
    body: Vec<u8>,
    etag: Option<String>,
    last_modified: Option<String>,
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
      },
      MockResponse {
        status: 304,
        body: Vec::new(),
        etag: Some("etag2".to_string()),
        last_modified: Some("lm2".to_string()),
      },
    ]);

    let disk = DiskCachingFetcher::new(fetcher.clone(), tmp.path());
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

    let snapshot = disk.read_disk_entry(url);
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

    let snapshot = disk.read_disk_entry(url).expect("entry should be readable");
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

    let snapshot = disk
      .read_disk_entry(url)
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
}
