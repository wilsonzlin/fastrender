//! Disk-backed caching fetcher that shares in-memory semantics with [`CachingFetcher`].
//!
//! Enabled via the `disk_cache` crate feature.

use super::CachedSnapshot;
use super::CachingFetcher;
use super::CachingFetcherConfig;
use super::FetchedResource;
use super::ResourceFetcher;
use crate::Result;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::fs;
use std::path::Path;
use std::path::PathBuf;
use std::time::Duration;
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

#[derive(Debug, Clone)]
pub struct DiskCachingFetcher<F: ResourceFetcher> {
  memory: CachingFetcher<F>,
  cache_dir: PathBuf,
  disk_config: DiskCacheConfig,
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

  fn read_disk_entry(&self, url: &str) -> Option<CachedSnapshot> {
    let data_path = self.data_path(url);
    let meta_path = self.meta_path_for_data(&data_path);

    let meta_bytes = fs::read(&meta_path).ok()?;
    let meta: StoredMetadata = serde_json::from_slice(&meta_bytes).ok()?;

    if let Some(max_age) = self.disk_config.max_age {
      if now_seconds().saturating_sub(meta.stored_at) > max_age.as_secs() {
        let _ = self.remove_entry(&data_path, &meta_path);
        return None;
      }
    }

    let bytes = fs::read(&data_path).ok()?;
    if bytes.is_empty() || meta.len != bytes.len() {
      let _ = self.remove_entry(&data_path, &meta_path);
      return None;
    }

    let mut resource = FetchedResource::with_final_url(
      bytes,
      meta.content_type.clone(),
      meta.final_url.clone().or_else(|| Some(url.to_string())),
    );
    resource.etag = meta.etag.clone();
    resource.last_modified = meta.last_modified.clone();

    Some(CachedSnapshot {
      value: super::CacheValue::Resource(resource),
      etag: meta.etag,
      last_modified: meta.last_modified,
    })
  }

  fn persist_snapshot(&self, url: &str, snapshot: &CachedSnapshot) {
    if let Some(resource) = snapshot.as_resource() {
      self.persist_resource(
        url,
        &resource,
        snapshot.etag.as_deref(),
        snapshot.last_modified.as_deref(),
      );
    }
  }

  fn persist_resource(
    &self,
    url: &str,
    resource: &FetchedResource,
    etag: Option<&str>,
    last_modified: Option<&str>,
  ) {
    if self.should_skip_disk(url) {
      return;
    }

    let data_path = self.data_path(url);
    if let Some(parent) = data_path.parent() {
      let _ = fs::create_dir_all(parent);
    }

    if fs::write(&data_path, &resource.bytes).is_err() {
      return;
    }

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
      stored_at: now_seconds(),
      len: resource.bytes.len(),
    };

    let meta_path = self.meta_path_for_data(&data_path);
    if let Ok(serialized) = serde_json::to_vec(&meta) {
      let _ = fs::write(&meta_path, &serialized);
    }

    self.enforce_disk_limits();
  }

  fn remove_entry(&self, data_path: &Path, meta_path: &Path) -> Result<()> {
    let _ = fs::remove_file(data_path);
    let _ = fs::remove_file(meta_path);
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
      let _ = self.remove_entry(&path, &meta_path);
      total = total.saturating_sub(size);
    }
  }
}

impl<F: ResourceFetcher> ResourceFetcher for DiskCachingFetcher<F> {
  fn fetch(&self, url: &str) -> Result<FetchedResource> {
    if let Some(snapshot) = self.read_disk_entry(url) {
      self
        .memory
        .prime_cache_with_resource(url, snapshot.as_resource().unwrap());
    }

    let result = self.memory.fetch(url);

    if let Some(snapshot) = self.memory.cached_snapshot(url) {
      self.persist_snapshot(url, &snapshot);
    } else if let Ok(ref resource) = result {
      self.persist_resource(
        url,
        resource,
        resource.etag.as_deref(),
        resource.last_modified.as_deref(),
      );
    }

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
}

fn now_seconds() -> u64 {
  SystemTime::now()
    .duration_since(UNIX_EPOCH)
    .unwrap_or_default()
    .as_secs()
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::error::Error;
  use std::collections::VecDeque;
  use std::sync::atomic::AtomicUsize;
  use std::sync::atomic::Ordering;
  use std::sync::Arc;
  use std::sync::Mutex;

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
}
