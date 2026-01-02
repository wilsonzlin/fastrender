use fastrender::image_loader::ImageCache;
use std::path::PathBuf;
use url::Url;

fn avif_fixture_url(name: &str) -> String {
  let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("tests/fixtures/avif")
    .join(name);
  Url::from_file_path(path).unwrap().to_string()
}

#[test]
fn probe_does_not_panic_on_avif_with_trailing_bytes() {
  let cache = ImageCache::new();
  let url = avif_fixture_url("solid_trailing.avif");
  let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| cache.probe(&url)));
  let probe = result.expect("image probe should not panic on avif-parse debug assertions");
  let meta = probe.expect("probe should succeed after trimming trailing bytes");
  assert_eq!((meta.width, meta.height), (4, 4));
}
