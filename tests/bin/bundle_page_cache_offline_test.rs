#![cfg(feature = "disk_cache")]

use fastrender::resource::bundle::{Bundle, BundledFetcher};
use fastrender::resource::{
  normalize_user_agent_for_log, CachingFetcherConfig, DiskCacheConfig, DiskCachingFetcher,
  FetchedResource, DEFAULT_ACCEPT_LANGUAGE, DEFAULT_USER_AGENT,
};
use fastrender::{Error, ResourceFetcher};
use std::collections::HashMap;
use std::process::Command;
use std::sync::Arc;
use tempfile::TempDir;

fn disk_cache_namespace() -> String {
  let ua = normalize_user_agent_for_log(DEFAULT_USER_AGENT).trim();
  let lang = DEFAULT_ACCEPT_LANGUAGE.trim();
  let browser_headers_enabled = std::env::var("FASTR_HTTP_BROWSER_HEADERS")
    .ok()
    .map(|raw| {
      !matches!(
        raw.trim().to_ascii_lowercase().as_str(),
        "0" | "false" | "no" | "off"
      )
    })
    .unwrap_or(true);
  if browser_headers_enabled {
    format!("fetch-profile:contextual-v1\nuser-agent:{ua}\naccept-language:{lang}")
  } else {
    format!(
      "fetch-profile:contextual-v1\nuser-agent:{ua}\naccept-language:{lang}\nhttp-browser-headers:0"
    )
  }
}

#[derive(Clone)]
struct StaticFetcher {
  responses: Arc<HashMap<String, (Vec<u8>, &'static str)>>,
}

impl ResourceFetcher for StaticFetcher {
  fn fetch(&self, url: &str) -> Result<FetchedResource, Error> {
    let (bytes, content_type) = self
      .responses
      .get(url)
      .ok_or_else(|| Error::Other(format!("unexpected fetch: {url}")))?;
    let mut resource = FetchedResource::with_final_url(
      bytes.clone(),
      Some((*content_type).to_string()),
      Some(url.to_string()),
    );
    resource.status = Some(200);
    Ok(resource)
  }
}

#[test]
fn bundle_page_cache_captures_from_disk_cache_offline() {
  let tmp = TempDir::new().expect("tempdir");

  let html_dir = tmp.path().join("fetches/html");
  std::fs::create_dir_all(&html_dir).expect("create html dir");
  let asset_dir = tmp.path().join("fetches/assets");
  std::fs::create_dir_all(&asset_dir).expect("create asset dir");

  let stem = "example.invalid";
  let page_url = "https://example.invalid/";
  let html_path = html_dir.join(format!("{stem}.html"));
  std::fs::write(
    &html_path,
    "<!doctype html><html><head><link rel=\"stylesheet\" href=\"/a.css\"></head><body><img src=\"img.png\"></body></html>",
  )
  .expect("write html");
  std::fs::write(
    html_path.with_extension("html.meta"),
    format!("content-type: text/html\nurl: {page_url}\n"),
  )
  .expect("write meta");

  let css_url = "https://example.invalid/a.css".to_string();
  let img_url = "https://example.invalid/img.png".to_string();
  let bg_url = "https://example.invalid/bg.png".to_string();

  let mut responses: HashMap<String, (Vec<u8>, &'static str)> = HashMap::new();
  responses.insert(
    css_url.clone(),
    (
      b"body { background-image: url(\"bg.png\"); }".to_vec(),
      "text/css",
    ),
  );
  responses.insert(img_url.clone(), (b"png-bytes-1".to_vec(), "image/png"));
  responses.insert(bg_url.clone(), (b"png-bytes-2".to_vec(), "image/png"));

  let mut disk_config = DiskCacheConfig::default();
  disk_config.namespace = Some(disk_cache_namespace());
  disk_config.allow_no_store = true;

  let cache_writer = DiskCachingFetcher::with_configs(
    StaticFetcher {
      responses: Arc::new(responses),
    },
    asset_dir.clone(),
    CachingFetcherConfig::default(),
    disk_config,
  );

  cache_writer.fetch(&css_url).expect("warm css");
  cache_writer.fetch(&img_url).expect("warm img");
  cache_writer.fetch(&bg_url).expect("warm bg");

  let bundle_dir = tmp.path().join("bundle");
  let status = Command::new(env!("CARGO_BIN_EXE_bundle_page"))
    .current_dir(tmp.path())
    .args(["cache", stem, "--out"])
    .arg(bundle_dir.to_string_lossy().as_ref())
    .status()
    .expect("run bundle_page cache");

  assert!(status.success(), "bundle_page cache should succeed");

  let bundle = Bundle::load(&bundle_dir).expect("load bundle");
  assert!(
    bundle.manifest().resources.contains_key(css_url.as_str()),
    "bundle should include stylesheet"
  );
  assert!(
    bundle.manifest().resources.contains_key(img_url.as_str()),
    "bundle should include image referenced from HTML"
  );
  assert!(
    bundle.manifest().resources.contains_key(bg_url.as_str()),
    "bundle should include image referenced from CSS url()"
  );

  let fetcher = BundledFetcher::new(bundle);
  assert_eq!(
    fetcher.fetch(&css_url).expect("fetch css").bytes,
    b"body { background-image: url(\"bg.png\"); }".to_vec()
  );
  assert_eq!(
    fetcher.fetch(&img_url).expect("fetch img").bytes,
    b"png-bytes-1".to_vec()
  );
  assert_eq!(
    fetcher.fetch(&bg_url).expect("fetch bg").bytes,
    b"png-bytes-2".to_vec()
  );
}

#[test]
fn bundle_page_cache_fails_when_resource_missing() {
  let tmp = TempDir::new().expect("tempdir");

  let html_dir = tmp.path().join("fetches/html");
  std::fs::create_dir_all(&html_dir).expect("create html dir");
  let asset_dir = tmp.path().join("fetches/assets");
  std::fs::create_dir_all(&asset_dir).expect("create asset dir");

  let stem = "example.invalid";
  let page_url = "https://example.invalid/";
  let html_path = html_dir.join(format!("{stem}.html"));
  std::fs::write(
    &html_path,
    "<!doctype html><html><head><link rel=\"stylesheet\" href=\"/a.css\"></head><body><img src=\"img.png\"></body></html>",
  )
  .expect("write html");
  std::fs::write(
    html_path.with_extension("html.meta"),
    format!("content-type: text/html\nurl: {page_url}\n"),
  )
  .expect("write meta");

  let css_url = "https://example.invalid/a.css".to_string();
  let img_url = "https://example.invalid/img.png".to_string();

  let mut responses: HashMap<String, (Vec<u8>, &'static str)> = HashMap::new();
  responses.insert(css_url.clone(), (b"body {}".to_vec(), "text/css"));

  let mut disk_config = DiskCacheConfig::default();
  disk_config.namespace = Some(disk_cache_namespace());
  disk_config.allow_no_store = true;

  let cache_writer = DiskCachingFetcher::with_configs(
    StaticFetcher {
      responses: Arc::new(responses),
    },
    asset_dir.clone(),
    CachingFetcherConfig::default(),
    disk_config,
  );

  cache_writer.fetch(&css_url).expect("warm css");

  let bundle_dir = tmp.path().join("bundle");
  let status = Command::new(env!("CARGO_BIN_EXE_bundle_page"))
    .current_dir(tmp.path())
    .args(["cache", stem, "--out"])
    .arg(bundle_dir.to_string_lossy().as_ref())
    .status()
    .expect("run bundle_page cache");

  assert!(
    !status.success(),
    "expected cache capture to fail when {img_url} is missing"
  );
}

#[test]
fn bundle_page_cache_allow_missing_inserts_placeholders() {
  let tmp = TempDir::new().expect("tempdir");

  let html_dir = tmp.path().join("fetches/html");
  std::fs::create_dir_all(&html_dir).expect("create html dir");
  let asset_dir = tmp.path().join("fetches/assets");
  std::fs::create_dir_all(&asset_dir).expect("create asset dir");

  let stem = "example.invalid";
  let page_url = "https://example.invalid/";
  let html_path = html_dir.join(format!("{stem}.html"));
  std::fs::write(
    &html_path,
    "<!doctype html><html><head><link rel=\"stylesheet\" href=\"/a.css\"></head><body><img src=\"img.png\"></body></html>",
  )
  .expect("write html");
  std::fs::write(
    html_path.with_extension("html.meta"),
    format!("content-type: text/html\nurl: {page_url}\n"),
  )
  .expect("write meta");

  let css_url = "https://example.invalid/a.css".to_string();
  let missing_img_url = "https://example.invalid/img.png".to_string();

  let mut responses: HashMap<String, (Vec<u8>, &'static str)> = HashMap::new();
  responses.insert(css_url.clone(), (b"body {}".to_vec(), "text/css"));

  let mut disk_config = DiskCacheConfig::default();
  disk_config.namespace = Some(disk_cache_namespace());
  disk_config.allow_no_store = true;

  let cache_writer = DiskCachingFetcher::with_configs(
    StaticFetcher {
      responses: Arc::new(responses),
    },
    asset_dir.clone(),
    CachingFetcherConfig::default(),
    disk_config,
  );

  cache_writer.fetch(&css_url).expect("warm css");

  let bundle_dir = tmp.path().join("bundle");
  let status = Command::new(env!("CARGO_BIN_EXE_bundle_page"))
    .current_dir(tmp.path())
    .args(["cache", stem, "--out"])
    .arg(bundle_dir.to_string_lossy().as_ref())
    .arg("--allow-missing")
    .status()
    .expect("run bundle_page cache");

  assert!(
    status.success(),
    "expected cache capture to succeed with --allow-missing"
  );

  let bundle = Bundle::load(&bundle_dir).expect("load bundle");
  assert!(
    bundle
      .manifest()
      .resources
      .contains_key(missing_img_url.as_str()),
    "bundle should include placeholder for missing resource"
  );

  let fetcher = BundledFetcher::new(bundle);
  let missing = fetcher
    .fetch(&missing_img_url)
    .expect("fetch placeholder resource");
  assert!(
    missing.bytes.is_empty(),
    "placeholder bytes should be empty"
  );
}
