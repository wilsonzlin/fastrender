#![cfg(feature = "disk_cache")]

use fastrender::pageset::pageset_stem;
use fastrender::resource::CachingFetcherConfig;
use fastrender::resource::DiskCacheConfig;
use fastrender::resource::DiskCachingFetcher;
use fastrender::resource::FetchedResource;
use fastrender::resource::{
  normalize_user_agent_for_log, DEFAULT_ACCEPT_LANGUAGE, DEFAULT_USER_AGENT,
};
use fastrender::Error;
use fastrender::ResourceFetcher;
mod test_support;
use std::collections::HashMap;
use std::io;
use std::io::Read;
use std::io::Write;
use std::net::TcpListener;
use std::sync::Arc;
use std::sync::Mutex;
use std::thread;
use std::time::Duration;
use std::time::Instant;
use tempfile::TempDir;
use test_support::net::try_bind_localhost;

const MAX_WAIT: Duration = Duration::from_secs(3);

fn disk_cache_namespace() -> String {
  let ua = normalize_user_agent_for_log(DEFAULT_USER_AGENT).trim();
  let lang = DEFAULT_ACCEPT_LANGUAGE.trim();
  format!("user-agent:{ua}\naccept-language:{lang}")
}

fn spawn_server(
  listener: TcpListener,
  hits: Arc<Mutex<HashMap<String, usize>>>,
  responses: HashMap<String, (Vec<u8>, &'static str)>,
  required_paths: Vec<&'static str>,
) -> thread::JoinHandle<()> {
  thread::spawn(move || {
    let _ = listener.set_nonblocking(true);
    let start = Instant::now();
    while start.elapsed() < MAX_WAIT {
      match listener.accept() {
        Ok((mut stream, _)) => {
          let mut buf = Vec::new();
          let mut tmp = [0u8; 1024];
          loop {
            match stream.read(&mut tmp) {
              Ok(0) => break,
              Ok(n) => {
                buf.extend_from_slice(&tmp[..n]);
                if buf.windows(4).any(|w| w == b"\r\n\r\n") {
                  break;
                }
              }
              Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => {
                thread::sleep(Duration::from_millis(5));
                continue;
              }
              Err(_) => break,
            }
          }

          let req = String::from_utf8_lossy(&buf);
          let path = req
            .lines()
            .next()
            .and_then(|line| line.split_whitespace().nth(1))
            .unwrap_or("/");

          if let Ok(mut map) = hits.lock() {
            *map.entry(path.to_string()).or_insert(0) += 1;
          }

          if let Some((body, content_type)) = responses.get(path) {
            let response = format!(
              "HTTP/1.1 200 OK\r\nContent-Type: {content_type}\r\nCache-Control: max-age=3600\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
              body.len()
            );
            let _ = stream.write_all(response.as_bytes());
            let _ = stream.write_all(body);
          } else {
            let body = b"not found";
            let response = format!(
              "HTTP/1.1 404 Not Found\r\nContent-Type: text/plain\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
              body.len()
            );
            let _ = stream.write_all(response.as_bytes());
            let _ = stream.write_all(body);
          }

          if let Ok(map) = hits.lock() {
            if required_paths
              .iter()
              .all(|path| map.get(*path).copied().unwrap_or(0) > 0)
            {
              break;
            }
          }
        }
        Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => {
          thread::sleep(Duration::from_millis(5));
        }
        Err(_) => break,
      }
    }
  })
}

#[derive(Clone)]
struct FailFetcher;

impl ResourceFetcher for FailFetcher {
  fn fetch(&self, url: &str) -> Result<FetchedResource, Error> {
    Err(Error::Other(format!("network disabled: {url}")))
  }
}

#[test]
fn prefetch_assets_warms_disk_cache_with_imports_and_fonts() {
  let Some(listener) =
    try_bind_localhost("prefetch_assets_warms_disk_cache_with_imports_and_fonts")
  else {
    return;
  };
  let addr = listener.local_addr().unwrap();
  let hits = Arc::new(Mutex::new(HashMap::new()));
  let mut responses: HashMap<String, (Vec<u8>, &'static str)> = HashMap::new();
  responses.insert(
    "/a.css".to_string(),
    (
      b"@import \"/b.css\"; @font-face { font-family: test; src: url(\"/f.woff2\") }".to_vec(),
      "text/css",
    ),
  );
  responses.insert(
    "/b.css".to_string(),
    (b"body { color: black }".to_vec(), "text/css"),
  );
  responses.insert(
    "/f.woff2".to_string(),
    (b"dummy-font-bytes".to_vec(), "font/woff2"),
  );
  let handle = spawn_server(
    listener,
    Arc::clone(&hits),
    responses,
    vec!["/a.css", "/b.css", "/f.woff2"],
  );

  let tmp = TempDir::new().expect("tempdir");
  let page_url = format!("http://{}/", addr);
  let stem = pageset_stem(&page_url).expect("stem");

  let html_dir = tmp.path().join("fetches/html");
  std::fs::create_dir_all(&html_dir).expect("create html dir");

  let html_path = html_dir.join(format!("{stem}.html"));
  std::fs::write(
    &html_path,
    "<!doctype html><html><head><link rel=\"stylesheet\" href=\"/a.css\"></head><body>ok</body></html>",
  )
  .expect("write html cache");

  let meta_path = html_path.with_extension("html.meta");
  std::fs::write(&meta_path, format!("url: {page_url}\n")).expect("write html meta");

  let status = std::process::Command::new(env!("CARGO_BIN_EXE_prefetch_assets"))
    .current_dir(tmp.path())
    .env("FASTR_PAGESET_URLS", page_url.clone())
    .arg("--jobs")
    .arg("1")
    .arg("--timeout")
    .arg("5")
    .status()
    .expect("run prefetch_assets");
  assert!(status.success(), "prefetch_assets should succeed");

  handle.join().unwrap();

  let asset_dir = tmp.path().join("fetches/assets");
  assert!(asset_dir.is_dir(), "disk cache dir should be created");

  let a_css = format!("http://{}/a.css", addr);
  let b_css = format!("http://{}/b.css", addr);
  let font = format!("http://{}/f.woff2", addr);

  let offline = DiskCachingFetcher::with_configs(
    FailFetcher,
    asset_dir.clone(),
    CachingFetcherConfig {
      honor_http_cache_freshness: true,
      ..CachingFetcherConfig::default()
    },
    DiskCacheConfig {
      namespace: Some(disk_cache_namespace()),
      ..DiskCacheConfig::default()
    },
  );
  assert!(
    offline.fetch(&a_css).is_ok(),
    "a.css should be cached on disk"
  );
  assert!(
    offline.fetch(&b_css).is_ok(),
    "b.css (import) should be cached on disk"
  );
  assert!(
    offline.fetch(&font).is_ok(),
    "font url referenced by @font-face should be cached on disk"
  );
}

#[test]
fn prefetch_assets_warms_disk_cache_with_layer_imports_and_fonts() {
  let Some(listener) =
    try_bind_localhost("prefetch_assets_warms_disk_cache_with_layer_imports_and_fonts")
  else {
    return;
  };
  let addr = listener.local_addr().unwrap();
  let hits = Arc::new(Mutex::new(HashMap::new()));
  let mut responses: HashMap<String, (Vec<u8>, &'static str)> = HashMap::new();
  responses.insert(
    "/a.css".to_string(),
    (b"@import URL(\"/b.css\") layer(foo);".to_vec(), "text/css"),
  );
  responses.insert(
    "/b.css".to_string(),
    (
      b"@font-face { font-family: test2; src: url(\"f2.woff2\") } body { color: black }".to_vec(),
      "text/css",
    ),
  );
  responses.insert(
    "/f2.woff2".to_string(),
    (b"dummy-font-bytes-2".to_vec(), "font/woff2"),
  );
  let handle = spawn_server(
    listener,
    Arc::clone(&hits),
    responses,
    vec!["/a.css", "/b.css", "/f2.woff2"],
  );

  let tmp = TempDir::new().expect("tempdir");
  let page_url = format!("http://{}/", addr);
  let stem = pageset_stem(&page_url).expect("stem");

  let html_dir = tmp.path().join("fetches/html");
  std::fs::create_dir_all(&html_dir).expect("create html dir");

  let html_path = html_dir.join(format!("{stem}.html"));
  std::fs::write(
    &html_path,
    "<!doctype html><html><head><link rel=\"stylesheet\" href=\"/a.css\"></head><body>ok</body></html>",
  )
  .expect("write html cache");

  let meta_path = html_path.with_extension("html.meta");
  std::fs::write(&meta_path, format!("url: {page_url}\n")).expect("write html meta");

  let status = std::process::Command::new(env!("CARGO_BIN_EXE_prefetch_assets"))
    .current_dir(tmp.path())
    .env("FASTR_PAGESET_URLS", page_url.clone())
    .arg("--jobs")
    .arg("1")
    .arg("--timeout")
    .arg("5")
    .status()
    .expect("run prefetch_assets");
  assert!(status.success(), "prefetch_assets should succeed");

  handle.join().unwrap();

  let asset_dir = tmp.path().join("fetches/assets");
  assert!(asset_dir.is_dir(), "disk cache dir should be created");

  let a_css = format!("http://{}/a.css", addr);
  let b_css = format!("http://{}/b.css", addr);
  let font = format!("http://{}/f2.woff2", addr);

  let offline = DiskCachingFetcher::with_configs(
    FailFetcher,
    asset_dir.clone(),
    CachingFetcherConfig {
      honor_http_cache_freshness: true,
      ..CachingFetcherConfig::default()
    },
    DiskCacheConfig {
      namespace: Some(disk_cache_namespace()),
      ..DiskCacheConfig::default()
    },
  );

  assert!(
    offline.fetch(&a_css).is_ok(),
    "a.css should be cached on disk"
  );
  assert!(
    offline.fetch(&b_css).is_ok(),
    "b.css (layer import) should be cached on disk"
  );
  assert!(
    offline.fetch(&font).is_ok(),
    "font url referenced by imported stylesheet should be cached on disk"
  );
}

#[test]
fn prefetch_assets_warms_disk_cache_with_inline_style_import_and_fonts() {
  let Some(listener) =
    try_bind_localhost("prefetch_assets_warms_disk_cache_with_inline_style_import_and_fonts")
  else {
    return;
  };
  let addr = listener.local_addr().unwrap();
  let hits = Arc::new(Mutex::new(HashMap::new()));
  let mut responses: HashMap<String, (Vec<u8>, &'static str)> = HashMap::new();
  responses.insert(
    "/a.css".to_string(),
    (
      b"@font-face { font-family: test; src: url(\"/font?x=1\") }".to_vec(),
      "text/css",
    ),
  );
  responses.insert(
    "/font?x=1".to_string(),
    (b"dummy-font-query".to_vec(), "font/woff2"),
  );
  let handle = spawn_server(
    listener,
    Arc::clone(&hits),
    responses,
    vec!["/a.css", "/font?x=1"],
  );

  let tmp = TempDir::new().expect("tempdir");
  let page_url = format!("http://{}/", addr);
  let stem = pageset_stem(&page_url).expect("stem");

  let html_dir = tmp.path().join("fetches/html");
  std::fs::create_dir_all(&html_dir).expect("create html dir");

  let html_path = html_dir.join(format!("{stem}.html"));
  std::fs::write(
    &html_path,
    "<!doctype html><html><head><style>@import \"/a.css\";</style></head><body>ok</body></html>",
  )
  .expect("write html cache");

  let meta_path = html_path.with_extension("html.meta");
  std::fs::write(&meta_path, format!("url: {page_url}\n")).expect("write html meta");

  let status = std::process::Command::new(env!("CARGO_BIN_EXE_prefetch_assets"))
    .current_dir(tmp.path())
    .env("FASTR_PAGESET_URLS", page_url.clone())
    .arg("--jobs")
    .arg("1")
    .arg("--timeout")
    .arg("5")
    .status()
    .expect("run prefetch_assets");
  assert!(status.success(), "prefetch_assets should succeed");

  handle.join().unwrap();

  let asset_dir = tmp.path().join("fetches/assets");
  assert!(asset_dir.is_dir(), "disk cache dir should be created");

  let a_css = format!("http://{}/a.css", addr);
  let font = format!("http://{}/font?x=1", addr);

  let offline = DiskCachingFetcher::with_configs(
    FailFetcher,
    asset_dir.clone(),
    CachingFetcherConfig {
      honor_http_cache_freshness: true,
      ..CachingFetcherConfig::default()
    },
    DiskCacheConfig {
      namespace: Some(disk_cache_namespace()),
      ..DiskCacheConfig::default()
    },
  );
  assert!(
    offline.fetch(&a_css).is_ok(),
    "a.css (import from inline <style>) should be cached on disk"
  );
  assert!(
    offline.fetch(&font).is_ok(),
    "font url referenced by imported stylesheet should be cached on disk"
  );
}

#[test]
fn prefetch_assets_warms_disk_cache_with_shadow_root_style_import() {
  let Some(listener) =
    try_bind_localhost("prefetch_assets_warms_disk_cache_with_shadow_root_style_import")
  else {
    return;
  };
  let addr = listener.local_addr().unwrap();
  let hits = Arc::new(Mutex::new(HashMap::new()));
  let mut responses: HashMap<String, (Vec<u8>, &'static str)> = HashMap::new();
  responses.insert(
    "/shadow.css".to_string(),
    (b"body { color: black }".to_vec(), "text/css"),
  );
  let handle = spawn_server(listener, Arc::clone(&hits), responses, vec!["/shadow.css"]);

  let tmp = TempDir::new().expect("tempdir");
  let page_url = format!("http://{}/", addr);
  let stem = pageset_stem(&page_url).expect("stem");

  let html_dir = tmp.path().join("fetches/html");
  std::fs::create_dir_all(&html_dir).expect("create html dir");

  let html_path = html_dir.join(format!("{stem}.html"));
  std::fs::write(
    &html_path,
    "<!doctype html><html><body><div id=\"host\"><template shadowroot=\"open\"><style>@import \"/shadow.css\";</style></template></div></body></html>",
  )
  .expect("write html cache");

  let meta_path = html_path.with_extension("html.meta");
  std::fs::write(&meta_path, format!("url: {page_url}\n")).expect("write html meta");

  let status = std::process::Command::new(env!("CARGO_BIN_EXE_prefetch_assets"))
    .current_dir(tmp.path())
    .env("FASTR_PAGESET_URLS", page_url.clone())
    .arg("--jobs")
    .arg("1")
    .arg("--timeout")
    .arg("5")
    .status()
    .expect("run prefetch_assets");
  assert!(status.success(), "prefetch_assets should succeed");

  handle.join().unwrap();

  let asset_dir = tmp.path().join("fetches/assets");
  assert!(asset_dir.is_dir(), "disk cache dir should be created");

  let shadow_css = format!("http://{}/shadow.css", addr);

  let offline = DiskCachingFetcher::with_configs(
    FailFetcher,
    asset_dir.clone(),
    CachingFetcherConfig {
      honor_http_cache_freshness: true,
      ..CachingFetcherConfig::default()
    },
    DiskCacheConfig {
      namespace: Some(disk_cache_namespace()),
      ..DiskCacheConfig::default()
    },
  );
  assert!(
    offline.fetch(&shadow_css).is_ok(),
    "shadow-root stylesheet import should be cached on disk"
  );
}

#[test]
fn prefetch_assets_warms_disk_cache_with_html_images_and_iframes() {
  let Some(listener) =
    try_bind_localhost("prefetch_assets_warms_disk_cache_with_html_images_and_iframes")
  else {
    return;
  };
  let addr = listener.local_addr().unwrap();
  let hits = Arc::new(Mutex::new(HashMap::new()));
  let mut responses: HashMap<String, (Vec<u8>, &'static str)> = HashMap::new();
  responses.insert("/img.png".to_string(), (b"dummy-img".to_vec(), "image/png"));
  responses.insert(
    "/poster.jpg".to_string(),
    (b"dummy-poster".to_vec(), "image/jpeg"),
  );
  responses.insert(
    "/favicon.ico".to_string(),
    (b"dummy-favicon".to_vec(), "image/x-icon"),
  );
  responses.insert(
    "/manifest.json".to_string(),
    (b"{\"name\":\"test\"}".to_vec(), "application/manifest+json"),
  );
  responses.insert(
    "/frame.html".to_string(),
    (
      b"<!doctype html><html><body>frame</body></html>".to_vec(),
      "text/html",
    ),
  );
  responses.insert(
    "/object.html".to_string(),
    (
      b"<!doctype html><html><body>object</body></html>".to_vec(),
      "text/html",
    ),
  );
  responses.insert(
    "/embed.html".to_string(),
    (
      b"<!doctype html><html><body>embed</body></html>".to_vec(),
      "text/html",
    ),
  );

  let handle = spawn_server(
    listener,
    Arc::clone(&hits),
    responses,
    vec![
      "/img.png",
      "/poster.jpg",
      "/favicon.ico",
      "/manifest.json",
      "/frame.html",
      "/object.html",
      "/embed.html",
    ],
  );

  let tmp = TempDir::new().expect("tempdir");
  let page_url = format!("http://{}/", addr);
  let stem = pageset_stem(&page_url).expect("stem");

  let html_dir = tmp.path().join("fetches/html");
  std::fs::create_dir_all(&html_dir).expect("create html dir");

  let html_path = html_dir.join(format!("{stem}.html"));
  std::fs::write(
    &html_path,
    "<!doctype html><html><head>\
      <link rel=\"icon\" href=\"/favicon.ico\">\
      <link rel=\"manifest\" href=\"/manifest.json\">\
    </head><body>\
      <img src=\"/img.png\">\
      <video poster=\"/poster.jpg\"></video>\
      <iframe src=\"/frame.html\"></iframe>\
      <object data=\"/object.html\"></object>\
      <embed src=\"/embed.html\">\
    </body></html>",
  )
  .expect("write html cache");

  let meta_path = html_path.with_extension("html.meta");
  std::fs::write(&meta_path, format!("url: {page_url}\n")).expect("write html meta");

  let status = std::process::Command::new(env!("CARGO_BIN_EXE_prefetch_assets"))
    .current_dir(tmp.path())
    .env("FASTR_PAGESET_URLS", page_url.clone())
    .arg("--jobs")
    .arg("1")
    .arg("--timeout")
    .arg("5")
    .arg("--prefetch-images")
    .arg("--prefetch-iframes")
    .status()
    .expect("run prefetch_assets");
  assert!(status.success(), "prefetch_assets should succeed");

  handle.join().unwrap();

  let asset_dir = tmp.path().join("fetches/assets");
  assert!(asset_dir.is_dir(), "disk cache dir should be created");

  let img = format!("http://{}/img.png", addr);
  let poster = format!("http://{}/poster.jpg", addr);
  let favicon = format!("http://{}/favicon.ico", addr);
  let manifest = format!("http://{}/manifest.json", addr);
  let iframe_doc = format!("http://{}/frame.html", addr);
  let object_doc = format!("http://{}/object.html", addr);
  let embed_doc = format!("http://{}/embed.html", addr);

  let offline = DiskCachingFetcher::with_configs(
    FailFetcher,
    asset_dir.clone(),
    CachingFetcherConfig {
      honor_http_cache_freshness: true,
      ..CachingFetcherConfig::default()
    },
    DiskCacheConfig {
      namespace: Some(disk_cache_namespace()),
      ..DiskCacheConfig::default()
    },
  );

  for url in [
    &img,
    &poster,
    &favicon,
    &manifest,
    &iframe_doc,
    &object_doc,
    &embed_doc,
  ] {
    assert!(offline.fetch(url).is_ok(), "{url} should be cached on disk");
  }
}
