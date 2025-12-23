use fastrender::error::ImageError;
use fastrender::image_loader::{CachedImage, ImageCache, ImageCacheConfig};
use fastrender::resource::{FetchedResource, ResourceFetcher};
use fastrender::Result;
use image::{DynamicImage, ImageOutputFormat, Rgba, RgbaImage};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Barrier, Mutex};
use std::thread;
use std::time::Duration;

struct CountingFetcher {
  count: AtomicUsize,
  bytes: Vec<u8>,
}

impl ResourceFetcher for CountingFetcher {
  fn fetch(&self, _url: &str) -> Result<FetchedResource> {
    self.count.fetch_add(1, Ordering::SeqCst);
    // Slow down fetch to maximize overlap between concurrent loads.
    thread::sleep(Duration::from_millis(20));
    Ok(FetchedResource::new(
      self.bytes.clone(),
      Some("image/png".to_string()),
    ))
  }
}

struct StaticFetcher {
  bytes: Vec<u8>,
}

impl ResourceFetcher for StaticFetcher {
  fn fetch(&self, _url: &str) -> Result<FetchedResource> {
    Ok(FetchedResource::new(
      self.bytes.clone(),
      Some("image/png".to_string()),
    ))
  }
}

fn small_png() -> Vec<u8> {
  vec![
    0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a, // PNG signature
    0x00, 0x00, 0x00, 0x0d, 0x49, 0x48, 0x44, 0x52, // IHDR chunk
    0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01, // 1x1
    0x08, 0x02, 0x00, 0x00, 0x00, 0x90, 0x77, 0x53, 0xde, 0x00, 0x00, 0x00, 0x0c, 0x49, 0x44, 0x41,
    0x54, 0x08, 0xd7, 0x63, 0xf8, 0xff, 0xff, 0x3f, 0x00, 0x05, 0xfe, 0x02, 0xfe, 0xdc, 0xcc, 0x59,
    0xe7, 0x00, 0x00, 0x00, 0x00, 0x49, 0x45, 0x4e, 0x44, 0xae, 0x42, 0x60, 0x82,
  ]
}

fn png_with_dimensions(width: u32, height: u32) -> Vec<u8> {
  let image = RgbaImage::from_pixel(width, height, Rgba([0, 0, 0, 0]));
  let mut bytes = Vec::new();
  DynamicImage::ImageRgba8(image)
    .write_to(&mut bytes, ImageOutputFormat::Png)
    .expect("encode png");
  bytes
}

#[test]
fn coalesces_concurrent_decodes() {
  let fetcher = Arc::new(CountingFetcher {
    count: AtomicUsize::new(0),
    bytes: small_png(),
  });
  let cache = Arc::new(ImageCache::with_fetcher(
    Arc::clone(&fetcher) as Arc<dyn ResourceFetcher>
  ));

  let workers = 8;
  let barrier = Arc::new(Barrier::new(workers));
  let results: Arc<Mutex<Vec<Arc<CachedImage>>>> = Arc::new(Mutex::new(Vec::new()));

  let mut handles = Vec::new();
  for _ in 0..workers {
    let cache = Arc::clone(&cache);
    let barrier = Arc::clone(&barrier);
    let results = Arc::clone(&results);
    handles.push(thread::spawn(move || {
      barrier.wait();
      let image = cache.load("test://image.png").expect("load image");
      results.lock().unwrap().push(image);
    }));
  }

  for handle in handles {
    handle.join().expect("thread join");
  }

  assert_eq!(fetcher.count.load(Ordering::SeqCst), 1);

  let results = results.lock().unwrap();
  let first = results.first().expect("result produced");
  for image in results.iter().skip(1) {
    assert!(Arc::ptr_eq(first, image), "expected shared decode");
  }
}

#[test]
fn rejects_image_exceeding_limits() {
  let png = png_with_dimensions(20, 20);
  let fetcher = Arc::new(StaticFetcher { bytes: png });
  let config = ImageCacheConfig::default().with_max_decoded_pixels(16);
  let cache =
    ImageCache::with_fetcher_and_config(Arc::clone(&fetcher) as Arc<dyn ResourceFetcher>, config);

  let err = cache.load("test://too-big.png").unwrap_err();
  match err {
    fastrender::Error::Image(ImageError::DecodeFailed { reason, .. }) => {
      assert!(reason.contains("pixel"), "unexpected error: {reason}");
    }
    other => panic!("unexpected error: {other:?}"),
  }
}
