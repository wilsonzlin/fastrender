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

  let tmp = tempfile::tempdir().expect("create tempdir for disk cache bench");
  let body: Arc<Vec<u8>> = Arc::new(vec![42u8; 128]);

  let memory_config = CachingFetcherConfig {
    honor_http_cache_freshness: true,
    ..CachingFetcherConfig::default()
  };
  let disk_config = DiskCacheConfig::default();

  // Warm the disk cache with deterministic contents.
  let seeder = DiskCachingFetcher::with_configs(
    StaticFetcher {
      body: Arc::clone(&body),
    },
    tmp.path(),
    memory_config,
    disk_config.clone(),
  );
  for url in &urls {
    let res = seeder.fetch(url).expect("seed fetch");
    assert_eq!(res.bytes.len(), body.len());
  }
  drop(seeder);

  let pool = rayon::ThreadPoolBuilder::new()
    .num_threads(workers)
    .build()
    .expect("build rayon pool");

  let mut group = c.benchmark_group("disk_cache");
  group.bench_function("parallel_hits_small_urlset", |b| {
    b.iter(|| {
      pool.install(|| {
        (0..workers).into_par_iter().for_each(|_| {
          let fetcher = DiskCachingFetcher::with_configs(
            PanicFetcher,
            tmp.path(),
            memory_config,
            disk_config.clone(),
          );
          for url in &urls {
            let res = fetcher.fetch(url).expect("disk hit");
            assert_eq!(res.bytes.len(), body.len());
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
