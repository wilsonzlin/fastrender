#[cfg(feature = "disk_cache")]
use criterion::{criterion_group, criterion_main, Criterion};
#[cfg(feature = "disk_cache")]
use fastrender::resource::{CachingFetcherConfig, DiskCacheConfig, DiskCachingFetcher};
#[cfg(feature = "disk_cache")]
use fastrender::resource::{FetchedResource, ResourceFetcher};
#[cfg(feature = "disk_cache")]
use fastrender::Result;
#[cfg(feature = "disk_cache")]
use rayon::prelude::*;
#[cfg(feature = "disk_cache")]
use std::sync::Arc;

#[cfg(feature = "disk_cache")]
#[derive(Clone)]
struct StaticFetcher {
  body: Arc<Vec<u8>>,
}

#[cfg(feature = "disk_cache")]
impl ResourceFetcher for StaticFetcher {
  fn fetch(&self, url: &str) -> Result<FetchedResource> {
    let mut res = FetchedResource::new((*self.body).clone(), Some("text/plain".to_string()));
    res.final_url = Some(url.to_string());
    Ok(res)
  }
}

#[cfg(feature = "disk_cache")]
#[derive(Clone)]
struct PanicFetcher;

#[cfg(feature = "disk_cache")]
impl ResourceFetcher for PanicFetcher {
  fn fetch(&self, _url: &str) -> Result<FetchedResource> {
    panic!("inner fetch should not be called during disk-cache hit benchmark");
  }
}

#[cfg(feature = "disk_cache")]
fn bench_disk_cache_parallel_hits(c: &mut Criterion) {
  let workers = std::cmp::max(1, num_cpus::get());
  let url_count = 16usize;
  let urls: Vec<String> = (0..url_count)
    .map(|i| format!("https://example.com/resource/{i}"))
    .collect();

  let memory_config = CachingFetcherConfig {
    honor_http_cache_freshness: true,
    ..CachingFetcherConfig::default()
  };
  let disk_config = DiskCacheConfig::default();

  let pool = rayon::ThreadPoolBuilder::new()
    .num_threads(workers)
    .build()
    .expect("build rayon pool");

  // Two scenarios:
  // - `tiny`: models common disk-cache hits (CSS, font metadata) where the old read path would
  //   allocate/zero a 64KiB scratch buffer twice per hit (meta + data).
  // - `medium`: exercises a larger payload (128KiB) to ensure throughput remains good when reads
  //   require the full 64KiB chunk size.
  //
  // Expected perf change after optimizing `read_all_with_deadline`:
  // - `tiny` should improve significantly due to fewer per-hit allocations/zeroing.
  // - `medium` should improve modestly from scratch-buffer reuse.
  let tmp_tiny = tempfile::tempdir().expect("create tempdir for tiny disk cache bench");
  let body_tiny: Arc<Vec<u8>> = Arc::new(vec![42u8; 128]);
  let seeder_tiny = DiskCachingFetcher::with_configs(
    StaticFetcher {
      body: Arc::clone(&body_tiny),
    },
    tmp_tiny.path(),
    memory_config,
    disk_config.clone(),
  );
  for url in &urls {
    let res = seeder_tiny.fetch(url).expect("seed tiny fetch");
    assert_eq!(res.bytes.len(), body_tiny.len());
  }
  drop(seeder_tiny);

  let tmp_medium = tempfile::tempdir().expect("create tempdir for medium disk cache bench");
  let body_medium: Arc<Vec<u8>> = Arc::new(vec![24u8; 128 * 1024]);
  let seeder_medium = DiskCachingFetcher::with_configs(
    StaticFetcher {
      body: Arc::clone(&body_medium),
    },
    tmp_medium.path(),
    memory_config,
    disk_config.clone(),
  );
  for url in &urls {
    let res = seeder_medium.fetch(url).expect("seed medium fetch");
    assert_eq!(res.bytes.len(), body_medium.len());
  }
  drop(seeder_medium);

  let mut group = c.benchmark_group("disk_cache");
  group.bench_function("parallel_hits_tiny_resources", |b| {
    b.iter(|| {
      pool.install(|| {
        (0..workers).into_par_iter().for_each(|_| {
          let fetcher = DiskCachingFetcher::with_configs(
            PanicFetcher,
            tmp_tiny.path(),
            memory_config,
            disk_config.clone(),
          );
          for url in &urls {
            let res = fetcher.fetch(url).expect("disk hit");
            assert_eq!(res.bytes.len(), body_tiny.len());
          }
        });
      });
    });
  });

  group.bench_function("parallel_hits_medium_resources", |b| {
    b.iter(|| {
      pool.install(|| {
        (0..workers).into_par_iter().for_each(|_| {
          let fetcher = DiskCachingFetcher::with_configs(
            PanicFetcher,
            tmp_medium.path(),
            memory_config,
            disk_config.clone(),
          );
          for url in &urls {
            let res = fetcher.fetch(url).expect("disk hit");
            assert_eq!(res.bytes.len(), body_medium.len());
          }
        });
      });
    });
  });
  group.finish();
}

#[cfg(feature = "disk_cache")]
criterion_group!(benches, bench_disk_cache_parallel_hits);
#[cfg(feature = "disk_cache")]
criterion_main!(benches);

#[cfg(not(feature = "disk_cache"))]
fn main() {}
