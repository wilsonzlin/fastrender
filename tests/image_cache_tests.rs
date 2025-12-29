use fastrender::api::{DiagnosticsLevel, FastRender, RenderOptions};
use fastrender::error::ImageError;
use fastrender::image_loader::{CachedImage, ImageCache, ImageCacheConfig};
use fastrender::resource::{FetchedResource, ResourceFetcher};
use fastrender::Result;
use image::{DynamicImage, ImageFormat, Rgba, RgbaImage};
use std::fs;
use std::path::PathBuf;
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
  let mut cursor = std::io::Cursor::new(Vec::new());
  DynamicImage::ImageRgba8(image)
    .write_to(&mut cursor, ImageFormat::Png)
    .expect("encode png");
  cursor.into_inner()
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

  let err = match cache.load("test://too-big.png") {
    Ok(_) => panic!("expected decode failure"),
    Err(err) => err,
  };
  match err {
    fastrender::Error::Image(ImageError::DecodeFailed { reason, .. }) => {
      assert!(reason.contains("pixel"), "unexpected error: {reason}");
    }
    other => panic!("unexpected error: {other:?}"),
  }
}

#[test]
fn repeated_renders_hit_image_cache() {
  let mut renderer = FastRender::new().expect("renderer");
  let mut fixture = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
  fixture.push("tests/fixtures/bundle_page/page.html");
  let url = format!("file://{}", fixture.to_string_lossy());

  let options = RenderOptions::new()
    .with_viewport(320, 240)
    .with_diagnostics_level(DiagnosticsLevel::Basic);

  let first = renderer
    .render_url_with_options(&url, options.clone())
    .expect("first render");
  let stats1 = first
    .diagnostics
    .stats
    .as_ref()
    .expect("first stats should be recorded");
  let hits1 = stats1.resources.image_cache_hits.unwrap_or(0);
  let misses1 = stats1.resources.image_cache_misses.unwrap_or(0);
  let decode1 = stats1.resources.image_decode_ms.unwrap_or(0.0);

  let second = renderer
    .render_url_with_options(&url, options)
    .expect("second render");
  let stats2 = second
    .diagnostics
    .stats
    .as_ref()
    .expect("second stats should be recorded");
  let hits2 = stats2.resources.image_cache_hits.unwrap_or(0);
  let misses2 = stats2.resources.image_cache_misses.unwrap_or(0);
  let decode2 = stats2.resources.image_decode_ms.unwrap_or(0.0);

  assert!(
    hits2 > hits1,
    "expected second render to reuse decoded images ({} -> {})",
    hits1,
    hits2
  );
  assert!(
    misses2 <= misses1,
    "expected cache misses to stay flat or fall ({} -> {})",
    misses1,
    misses2
  );
  assert!(
    decode2 <= decode1,
    "expected second render to avoid extra decoding ({} -> {})",
    decode1,
    decode2
  );
  assert!(
    misses2 < misses1,
    "expected fewer cache misses after warming ({} -> {})",
    misses1,
    misses2
  );
}

#[test]
fn stylesheet_inlining_reports_cache_usage() {
  let mut renderer = FastRender::new().expect("renderer");
  let mut fixture = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
  fixture.push("tests/fixtures/bundle_page/page.html");
  let base = format!("file://{}", fixture.to_string_lossy());
  let html = fs::read_to_string(&fixture).expect("load html");

  let options = RenderOptions::new()
    .with_viewport(320, 240)
    .with_diagnostics_level(DiagnosticsLevel::Basic);

  let first = renderer
    .render_html_with_stylesheets(&html, &base, options.clone())
    .expect("first render");
  let stats1 = first
    .diagnostics
    .stats
    .as_ref()
    .expect("first stats should be recorded");
  let hits1 = stats1.resources.image_cache_hits.unwrap_or(0);
  let misses1 = stats1.resources.image_cache_misses.unwrap_or(0);
  let decode1 = stats1.resources.image_decode_ms.unwrap_or(0.0);

  let second = renderer
    .render_html_with_stylesheets(&html, &base, options)
    .expect("second render");
  let stats2 = second
    .diagnostics
    .stats
    .as_ref()
    .expect("second stats should be recorded");
  let hits2 = stats2.resources.image_cache_hits.unwrap_or(0);
  let misses2 = stats2.resources.image_cache_misses.unwrap_or(0);
  let decode2 = stats2.resources.image_decode_ms.unwrap_or(0.0);

  assert!(
    hits2 > hits1,
    "expected second render to reuse decoded images ({} -> {})",
    hits1,
    hits2
  );
  assert!(
    misses2 <= misses1,
    "expected cache misses to stay flat or fall ({} -> {})",
    misses1,
    misses2
  );
  assert!(
    decode2 <= decode1,
    "expected stylesheet render to avoid extra decoding ({} -> {})",
    decode1,
    decode2
  );
}
