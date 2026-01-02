use fastrender::image_loader::ImageCache;
use fastrender::resource::{FetchedResource, ResourceFetcher};
use std::path::PathBuf;
use std::sync::Arc;

fn avif_fixture_bytes(name: &str) -> Vec<u8> {
  let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("tests/fixtures/avif")
    .join(name);
  std::fs::read(&path).expect("read AVIF fixture")
}

#[test]
fn probe_does_not_panic_on_avif_with_trailing_bytes() {
  #[derive(Clone)]
  struct StaticFetcher {
    bytes: Vec<u8>,
  }

  impl ResourceFetcher for StaticFetcher {
    fn fetch(&self, _url: &str) -> fastrender::Result<FetchedResource> {
      Ok(FetchedResource::new(self.bytes.clone(), None))
    }
  }

  let fetcher = Arc::new(StaticFetcher {
    bytes: avif_fixture_bytes("solid_trailing.avif"),
  });
  let cache = ImageCache::with_fetcher(fetcher);
  let url = "https://example.com/image";
  let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| cache.probe(url)));
  let probe = result.expect("image probe should not panic on avif-parse debug assertions");
  let meta = probe.expect("probe should succeed after trimming trailing bytes");
  assert_eq!((meta.width, meta.height), (4, 4));
}
