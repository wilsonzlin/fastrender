#![cfg(feature = "disk_cache")]

use fastrender::pageset::pageset_stem;
use fastrender::resource::{
  normalize_user_agent_for_log, CachingFetcherConfig, DiskCacheConfig, DiskCachingFetcher,
  FetchDestination, FetchRequest, FetchedResource, DEFAULT_ACCEPT_LANGUAGE, DEFAULT_USER_AGENT,
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
    offline
      .fetch_with_request(FetchRequest::new(&a_css, FetchDestination::Style))
      .is_ok(),
    "a.css should be cached on disk"
  );
  assert!(
    offline
      .fetch_with_request(FetchRequest::new(&b_css, FetchDestination::Style))
      .is_ok(),
    "b.css (import) should be cached on disk"
  );
  assert!(
    offline
      .fetch_with_request(FetchRequest::new(&font, FetchDestination::Font))
      .is_ok(),
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
    offline
      .fetch_with_request(FetchRequest::new(&a_css, FetchDestination::Style))
      .is_ok(),
    "a.css should be cached on disk"
  );
  assert!(
    offline
      .fetch_with_request(FetchRequest::new(&b_css, FetchDestination::Style))
      .is_ok(),
    "b.css (layer import) should be cached on disk"
  );
  assert!(
    offline
      .fetch_with_request(FetchRequest::new(&font, FetchDestination::Font))
      .is_ok(),
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
    offline
      .fetch_with_request(FetchRequest::new(&a_css, FetchDestination::Style))
      .is_ok(),
    "a.css (import from inline <style>) should be cached on disk"
  );
  assert!(
    offline
      .fetch_with_request(FetchRequest::new(&font, FetchDestination::Font))
      .is_ok(),
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
    offline
      .fetch_with_request(FetchRequest::new(&shadow_css, FetchDestination::Style))
      .is_ok(),
    "shadow-root stylesheet import should be cached on disk"
  );
}

#[test]
fn prefetch_assets_warms_disk_cache_with_html_images_iframes_and_embeds() {
  let Some(listener) =
    try_bind_localhost("prefetch_assets_warms_disk_cache_with_html_images_iframes_and_embeds")
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
      b"<!doctype html><html><head><link rel=\"stylesheet\" href=\"/frame.css\"></head><body><img src=\"/frame_img.png\">frame</body></html>".to_vec(),
      "text/html",
    ),
  );
  responses.insert(
    "/frame.css".to_string(),
    (b"body { color: black }".to_vec(), "text/css"),
  );
  responses.insert(
    "/frame_img.png".to_string(),
    (b"dummy-frame-img".to_vec(), "image/png"),
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
      "/frame.css",
      "/frame_img.png",
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
    .arg("--prefetch-embeds")
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
  let iframe_css = format!("http://{}/frame.css", addr);
  let iframe_img = format!("http://{}/frame_img.png", addr);
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

  for (url, destination) in [
    (&img, FetchDestination::Image),
    (&poster, FetchDestination::Image),
    (&favicon, FetchDestination::Image),
    (&manifest, FetchDestination::Image),
    (&iframe_doc, FetchDestination::Document),
    (&iframe_css, FetchDestination::Style),
    (&iframe_img, FetchDestination::Image),
    (&object_doc, FetchDestination::Document),
    (&embed_doc, FetchDestination::Document),
  ] {
    assert!(
      offline
        .fetch_with_request(FetchRequest::new(url, destination))
        .is_ok(),
      "{url} should be cached on disk"
    );
  }
}

#[test]
fn prefetch_assets_does_not_fetch_embeds_without_prefetch_embeds() {
  let Some(listener) = try_bind_localhost(
    "prefetch_assets_does_not_fetch_embeds_without_prefetch_embeds",
  ) else {
    return;
  };
  let addr = listener.local_addr().unwrap();
  let hits = Arc::new(Mutex::new(HashMap::new()));

  let mut responses: HashMap<String, (Vec<u8>, &'static str)> = HashMap::new();
  responses.insert(
    "/frame.html".to_string(),
    (b"<!doctype html><html><body>frame</body></html>".to_vec(), "text/html"),
  );
  responses.insert(
    "/object.bin".to_string(),
    (b"dummy-object".to_vec(), "application/octet-stream"),
  );
  responses.insert(
    "/embed.bin".to_string(),
    (b"dummy-embed".to_vec(), "application/octet-stream"),
  );
  responses.insert(
    "/media.mp4".to_string(),
    (b"dummy-media".to_vec(), "video/mp4"),
  );
  responses.insert(
    "/__shutdown__".to_string(),
    (b"shutdown".to_vec(), "text/plain"),
  );

  let handle = spawn_server(
    listener,
    Arc::clone(&hits),
    responses,
    vec!["/frame.html", "/__shutdown__"],
  );

  let tmp = TempDir::new().expect("tempdir");
  let page_url = format!("http://{}/", addr);
  let stem = pageset_stem(&page_url).expect("stem");

  let html_dir = tmp.path().join("fetches/html");
  std::fs::create_dir_all(&html_dir).expect("create html dir");

  let html_path = html_dir.join(format!("{stem}.html"));
  std::fs::write(
    &html_path,
    "<!doctype html><html><body>\
      <iframe src=\"/frame.html\"></iframe>\
      <object data=\"/object.bin\"></object>\
      <embed src=\"/embed.bin\">\
      <video><source src=\"/media.mp4\"></video>\
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
    .arg("--prefetch-iframes")
    .status()
    .expect("run prefetch_assets");
  assert!(status.success(), "prefetch_assets should succeed");

  let shutdown_url = format!("http://{}/__shutdown__", addr);
  let _ = ureq::get(&shutdown_url).call();
  handle.join().unwrap();

  let asset_dir = tmp.path().join("fetches/assets");
  assert!(asset_dir.is_dir(), "disk cache dir should be created");

  let iframe_doc = format!("http://{}/frame.html", addr);
  let object = format!("http://{}/object.bin", addr);
  let embed = format!("http://{}/embed.bin", addr);
  let media = format!("http://{}/media.mp4", addr);

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
    offline
      .fetch_with_request(FetchRequest::new(&iframe_doc, FetchDestination::Document))
      .is_ok(),
    "iframe should be cached"
  );
  assert!(
    offline
      .fetch_with_request(FetchRequest::new(&object, FetchDestination::Document))
      .is_err(),
    "object should not be cached without --prefetch-embeds"
  );
  assert!(
    offline
      .fetch_with_request(FetchRequest::new(&embed, FetchDestination::Document))
      .is_err(),
    "embed should not be cached without --prefetch-embeds"
  );
  assert!(
    offline
      .fetch_with_request(FetchRequest::new(&media, FetchDestination::Document))
      .is_err(),
    "media source should not be cached without --prefetch-embeds"
  );

  let hits = hits.lock().unwrap();
  assert_eq!(hits.get("/object.bin").copied().unwrap_or(0), 0);
  assert_eq!(hits.get("/embed.bin").copied().unwrap_or(0), 0);
  assert_eq!(hits.get("/media.mp4").copied().unwrap_or(0), 0);
}

#[test]
fn prefetch_assets_honors_base_href_for_html_discovery() {
  let Some(listener) =
    try_bind_localhost("prefetch_assets_honors_base_href_for_html_discovery")
  else {
    return;
  };
  let addr = listener.local_addr().unwrap();
  let hits = Arc::new(Mutex::new(HashMap::new()));

  let mut responses: HashMap<String, (Vec<u8>, &'static str)> = HashMap::new();
  responses.insert(
    "/base/frame.html".to_string(),
    (b"<!doctype html><html><body>frame</body></html>".to_vec(), "text/html"),
  );
  responses.insert(
    "/base/icon.png".to_string(),
    (b"dummy-icon".to_vec(), "image/png"),
  );
  responses.insert(
    "/base/poster.png".to_string(),
    (b"dummy-poster".to_vec(), "image/png"),
  );

  let handle = spawn_server(
    listener,
    Arc::clone(&hits),
    responses,
    vec!["/base/frame.html", "/base/icon.png", "/base/poster.png"],
  );

  let tmp = TempDir::new().expect("tempdir");
  let page_url = format!("http://{}/", addr);
  let stem = pageset_stem(&page_url).expect("stem");

  let html_dir = tmp.path().join("fetches/html");
  std::fs::create_dir_all(&html_dir).expect("create html dir");

  let html_path = html_dir.join(format!("{stem}.html"));
  std::fs::write(
    &html_path,
    format!(
      "<!doctype html><html><head>\
        <base href=\"http://{addr}/base/\">\
        <link rel=\"icon\" href=\"icon.png\">\
      </head><body>\
        <iframe src=\"frame.html\"></iframe>\
        <video poster=\"poster.png\"></video>\
      </body></html>"
    ),
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
    .arg("--prefetch-iframes")
    .arg("--prefetch-icons")
    .arg("--prefetch-video-posters")
    .status()
    .expect("run prefetch_assets");
  assert!(status.success(), "prefetch_assets should succeed");

  handle.join().unwrap();

  let asset_dir = tmp.path().join("fetches/assets");
  assert!(asset_dir.is_dir(), "disk cache dir should be created");

  let iframe_doc = format!("http://{}/base/frame.html", addr);
  let icon = format!("http://{}/base/icon.png", addr);
  let poster = format!("http://{}/base/poster.png", addr);

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
    offline
      .fetch_with_request(FetchRequest::new(&iframe_doc, FetchDestination::Document))
      .is_ok(),
    "iframe should be cached"
  );
  assert!(
    offline
      .fetch_with_request(FetchRequest::new(&icon, FetchDestination::Image))
      .is_ok(),
    "icon should be cached"
  );
  assert!(
    offline
      .fetch_with_request(FetchRequest::new(&poster, FetchDestination::Image))
      .is_ok(),
    "poster should be cached"
  );
}

#[test]
fn prefetch_assets_selects_srcset_candidate_and_respects_max_urls_per_element() {
  let Some(listener) = try_bind_localhost(
    "prefetch_assets_selects_srcset_candidate_and_respects_max_urls_per_element",
  ) else {
    return;
  };
  let addr = listener.local_addr().unwrap();
  let hits = Arc::new(Mutex::new(HashMap::new()));
  let mut responses: HashMap<String, (Vec<u8>, &'static str)> = HashMap::new();
  responses.insert(
    "/a1.jpg".to_string(),
    (b"dummy-a1".to_vec(), "image/jpeg"),
  );
  responses.insert(
    "/a2.jpg".to_string(),
    (b"dummy-a2".to_vec(), "image/jpeg"),
  );
  responses.insert(
    "/a3.jpg".to_string(),
    (b"dummy-a3".to_vec(), "image/jpeg"),
  );
  responses.insert(
    "/fallback.jpg".to_string(),
    (b"dummy-fallback".to_vec(), "image/jpeg"),
  );
  responses.insert(
    "/__shutdown__".to_string(),
    (b"shutdown".to_vec(), "text/plain"),
  );

  // Keep the server alive until the prefetch binary finishes, then send a
  // `/__shutdown__` request so we can assert what was (and wasn't) fetched.
  let handle = spawn_server(
    listener,
    Arc::clone(&hits),
    responses,
    vec!["/a2.jpg", "/__shutdown__"],
  );

  let tmp = TempDir::new().expect("tempdir");
  let page_url = format!("http://{}/", addr);
  let stem = pageset_stem(&page_url).expect("stem");

  let html_dir = tmp.path().join("fetches/html");
  std::fs::create_dir_all(&html_dir).expect("create html dir");

  let html_path = html_dir.join(format!("{stem}.html"));
  std::fs::write(
    &html_path,
    "<!doctype html><html><body>\
      <img src=\"/fallback.jpg\" srcset=\"/a1.jpg 1x, /a2.jpg 2x, /a3.jpg 3x\">\
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
    .arg("--dpr")
    .arg("2.0")
    .arg("--max-image-urls-per-element")
    .arg("1")
    .status()
    .expect("run prefetch_assets");
  assert!(status.success(), "prefetch_assets should succeed");

  // Trigger server shutdown so we can assert the full hit list.
  let shutdown_url = format!("http://{}/__shutdown__", addr);
  let _ = ureq::get(&shutdown_url).call();

  handle.join().unwrap();

  let asset_dir = tmp.path().join("fetches/assets");
  assert!(asset_dir.is_dir(), "disk cache dir should be created");

  let a1 = format!("http://{}/a1.jpg", addr);
  let a2 = format!("http://{}/a2.jpg", addr);
  let a3 = format!("http://{}/a3.jpg", addr);
  let fallback = format!("http://{}/fallback.jpg", addr);

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
    offline
      .fetch_with_request(FetchRequest::new(&a2, FetchDestination::Image))
      .is_ok(),
    "selected 2x candidate should be cached on disk"
  );
  assert!(
    offline
      .fetch_with_request(FetchRequest::new(&a1, FetchDestination::Image))
      .is_err(),
    "non-selected 1x candidate should not be cached (max urls per element = 1)"
  );
  assert!(
    offline
      .fetch_with_request(FetchRequest::new(&a3, FetchDestination::Image))
      .is_err(),
    "non-selected 3x candidate should not be cached (max urls per element = 1)"
  );
  assert!(
    offline
      .fetch_with_request(FetchRequest::new(&fallback, FetchDestination::Image))
      .is_err(),
    "base src should not be cached (max urls per element = 1)"
  );

  let hits = hits.lock().unwrap();
  assert_eq!(hits.get("/a2.jpg").copied().unwrap_or(0), 1);
  assert_eq!(hits.get("/a1.jpg").copied().unwrap_or(0), 0);
  assert_eq!(hits.get("/a3.jpg").copied().unwrap_or(0), 0);
  assert_eq!(hits.get("/fallback.jpg").copied().unwrap_or(0), 0);
}

#[test]
fn prefetch_assets_selects_preload_imagesrcset_candidate() {
  let Some(listener) =
    try_bind_localhost("prefetch_assets_selects_preload_imagesrcset_candidate")
  else {
    return;
  };
  let addr = listener.local_addr().unwrap();
  let hits = Arc::new(Mutex::new(HashMap::new()));
  let mut responses: HashMap<String, (Vec<u8>, &'static str)> = HashMap::new();
  responses.insert(
    "/a1.jpg".to_string(),
    (b"dummy-a1".to_vec(), "image/jpeg"),
  );
  responses.insert(
    "/a2.jpg".to_string(),
    (b"dummy-a2".to_vec(), "image/jpeg"),
  );
  responses.insert(
    "/fallback.jpg".to_string(),
    (b"dummy-fallback".to_vec(), "image/jpeg"),
  );
  responses.insert(
    "/__shutdown__".to_string(),
    (b"shutdown".to_vec(), "text/plain"),
  );

  let handle = spawn_server(
    listener,
    Arc::clone(&hits),
    responses,
    vec!["/a2.jpg", "/__shutdown__"],
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
      <link rel=\"preload\" as=\"image\" href=\"/fallback.jpg\" imagesrcset=\"/a1.jpg 1x, /a2.jpg 2x\">\
    </head><body>ok</body></html>",
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
    .arg("--dpr")
    .arg("2.0")
    .arg("--max-image-urls-per-element")
    .arg("1")
    .status()
    .expect("run prefetch_assets");
  assert!(status.success(), "prefetch_assets should succeed");

  let shutdown_url = format!("http://{}/__shutdown__", addr);
  let _ = ureq::get(&shutdown_url).call();

  handle.join().unwrap();

  let asset_dir = tmp.path().join("fetches/assets");
  assert!(asset_dir.is_dir(), "disk cache dir should be created");

  let a1 = format!("http://{}/a1.jpg", addr);
  let a2 = format!("http://{}/a2.jpg", addr);
  let fallback = format!("http://{}/fallback.jpg", addr);

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
    offline
      .fetch_with_request(FetchRequest::new(&a2, FetchDestination::Image))
      .is_ok(),
    "selected 2x preload candidate should be cached on disk"
  );
  assert!(
    offline
      .fetch_with_request(FetchRequest::new(&a1, FetchDestination::Image))
      .is_err(),
    "non-selected 1x preload candidate should not be cached (max urls per element = 1)"
  );
  assert!(
    offline
      .fetch_with_request(FetchRequest::new(&fallback, FetchDestination::Image))
      .is_err(),
    "preload href fallback should not be cached (max urls per element = 1)"
  );

  let hits = hits.lock().unwrap();
  assert_eq!(hits.get("/a2.jpg").copied().unwrap_or(0), 1);
  assert_eq!(hits.get("/a1.jpg").copied().unwrap_or(0), 0);
  assert_eq!(hits.get("/fallback.jpg").copied().unwrap_or(0), 0);
}

#[test]
fn prefetch_assets_warms_disk_cache_with_iframes_embeds_icons_and_video_posters() {
  let Some(listener) = try_bind_localhost(
    "prefetch_assets_warms_disk_cache_with_iframes_embeds_icons_and_video_posters",
  ) else {
    return;
  };
  let addr = listener.local_addr().unwrap();
  let hits = Arc::new(Mutex::new(HashMap::new()));
  let mut responses: HashMap<String, (Vec<u8>, &'static str)> = HashMap::new();

  responses.insert(
    "/frame.html".to_string(),
    (
      b"<!doctype html><html><body>frame</body></html>".to_vec(),
      "text/html",
    ),
  );
  responses.insert(
    "/poster.png".to_string(),
    (b"dummy-poster".to_vec(), "image/png"),
  );
  responses.insert(
    "/icon.png".to_string(),
    (b"dummy-icon".to_vec(), "image/png"),
  );
  responses.insert(
    "/embed.bin".to_string(),
    (b"dummy-embed".to_vec(), "application/octet-stream"),
  );
  responses.insert(
    "/object.bin".to_string(),
    (b"dummy-object".to_vec(), "application/octet-stream"),
  );
  responses.insert(
    "/img.png".to_string(),
    (b"dummy-img".to_vec(), "image/png"),
  );
  responses.insert(
    "/picture-1x.png".to_string(),
    (b"dummy-picture-1x".to_vec(), "image/png"),
  );
  responses.insert(
    "/__shutdown__".to_string(),
    (b"shutdown".to_vec(), "text/plain"),
  );

  let handle = spawn_server(
    listener,
    Arc::clone(&hits),
    responses,
    vec![
      "/frame.html",
      "/poster.png",
      "/icon.png",
      "/embed.bin",
      "/object.bin",
      "/__shutdown__",
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
    "<!doctype html><html><head><link rel=\"icon\" href=\"/icon.png\"></head><body>\
      <img src=\"/img.png\">\
      <iframe src=\"/frame.html\"></iframe>\
      <video poster=\"/poster.png\"></video>\
      <picture><source srcset=\"/picture-1x.png 1x, /picture-2x.png 2x\"><img src=\"/fallback.png\"></picture>\
      <object data=\"/object.bin\"></object>\
      <embed src=\"/embed.bin\">\
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
    .arg("--prefetch-iframes")
    .arg("--prefetch-embeds")
    .arg("--prefetch-icons")
    .arg("--prefetch-video-posters")
    .status()
    .expect("run prefetch_assets");
  assert!(status.success(), "prefetch_assets should succeed");

  // Trigger server shutdown so we can assert which subresources were fetched.
  let shutdown_url = format!("http://{}/__shutdown__", addr);
  let _ = ureq::get(&shutdown_url).call();

  handle.join().unwrap();

  let asset_dir = tmp.path().join("fetches/assets");
  assert!(asset_dir.is_dir(), "disk cache dir should be created");

  let iframe = format!("http://{}/frame.html", addr);
  let poster = format!("http://{}/poster.png", addr);
  let icon = format!("http://{}/icon.png", addr);
  let embed = format!("http://{}/embed.bin", addr);
  let object = format!("http://{}/object.bin", addr);
  let img = format!("http://{}/img.png", addr);
  let picture_1x = format!("http://{}/picture-1x.png", addr);

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
    offline
      .fetch_with_request(FetchRequest::new(&iframe, FetchDestination::Document))
      .is_ok(),
    "iframe document should be cached"
  );
  assert!(
    offline
      .fetch_with_request(FetchRequest::new(&poster, FetchDestination::Image))
      .is_ok(),
    "video poster should be cached"
  );
  assert!(
    offline
      .fetch_with_request(FetchRequest::new(&icon, FetchDestination::Image))
      .is_ok(),
    "icon should be cached"
  );
  assert!(
    offline
      .fetch_with_request(FetchRequest::new(&embed, FetchDestination::Document))
      .is_ok(),
    "embed src should be cached"
  );
  assert!(
    offline
      .fetch_with_request(FetchRequest::new(&object, FetchDestination::Document))
      .is_ok(),
    "object data should be cached"
  );
  assert!(
    offline
      .fetch_with_request(FetchRequest::new(&img, FetchDestination::Image))
      .is_err(),
    "img/src should not be cached without --prefetch-images"
  );
  assert!(
    offline
      .fetch_with_request(FetchRequest::new(&picture_1x, FetchDestination::Image))
      .is_err(),
    "picture srcset candidate should not be cached without --prefetch-images"
  );

  let hits = hits.lock().unwrap();
  assert_eq!(hits.get("/img.png").copied().unwrap_or(0), 0);
  assert_eq!(hits.get("/picture-1x.png").copied().unwrap_or(0), 0);
}

#[test]
fn prefetch_assets_respects_max_images_per_page() {
  let Some(listener) = try_bind_localhost("prefetch_assets_respects_max_images_per_page") else {
    return;
  };
  let addr = listener.local_addr().unwrap();
  let hits = Arc::new(Mutex::new(HashMap::new()));
  let mut responses: HashMap<String, (Vec<u8>, &'static str)> = HashMap::new();
  responses.insert(
    "/one.jpg".to_string(),
    (b"dummy-one".to_vec(), "image/jpeg"),
  );
  responses.insert(
    "/two.jpg".to_string(),
    (b"dummy-two".to_vec(), "image/jpeg"),
  );
  responses.insert(
    "/__shutdown__".to_string(),
    (b"shutdown".to_vec(), "text/plain"),
  );

  let handle = spawn_server(
    listener,
    Arc::clone(&hits),
    responses,
    vec!["/one.jpg", "/__shutdown__"],
  );

  let tmp = TempDir::new().expect("tempdir");
  let page_url = format!("http://{}/", addr);
  let stem = pageset_stem(&page_url).expect("stem");

  let html_dir = tmp.path().join("fetches/html");
  std::fs::create_dir_all(&html_dir).expect("create html dir");

  let html_path = html_dir.join(format!("{stem}.html"));
  std::fs::write(
    &html_path,
    "<!doctype html><html><body>\
      <img src=\"/one.jpg\">\
      <img src=\"/two.jpg\">\
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
    .arg("--max-images-per-page")
    .arg("1")
    .status()
    .expect("run prefetch_assets");
  assert!(status.success(), "prefetch_assets should succeed");

  let shutdown_url = format!("http://{}/__shutdown__", addr);
  let _ = ureq::get(&shutdown_url).call();

  handle.join().unwrap();

  let asset_dir = tmp.path().join("fetches/assets");
  assert!(asset_dir.is_dir(), "disk cache dir should be created");

  let one = format!("http://{}/one.jpg", addr);
  let two = format!("http://{}/two.jpg", addr);

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
    offline
      .fetch_with_request(FetchRequest::new(&one, FetchDestination::Image))
      .is_ok(),
    "first image should be cached on disk"
  );
  assert!(
    offline
      .fetch_with_request(FetchRequest::new(&two, FetchDestination::Image))
      .is_err(),
    "second image should not be cached (max-images-per-page=1)"
  );

  let hits = hits.lock().unwrap();
  assert_eq!(hits.get("/one.jpg").copied().unwrap_or(0), 1);
  assert_eq!(hits.get("/two.jpg").copied().unwrap_or(0), 0);
}

#[test]
fn prefetch_assets_selects_picture_source_by_media_and_dpr() {
  let Some(listener) = try_bind_localhost("prefetch_assets_selects_picture_source_by_media_and_dpr")
  else {
    return;
  };
  let addr = listener.local_addr().unwrap();
  let hits = Arc::new(Mutex::new(HashMap::new()));
  let mut responses: HashMap<String, (Vec<u8>, &'static str)> = HashMap::new();
  responses.insert(
    "/small2.jpg".to_string(),
    (b"dummy-small2".to_vec(), "image/jpeg"),
  );
  responses.insert(
    "/large2.jpg".to_string(),
    (b"dummy-large2".to_vec(), "image/jpeg"),
  );
  responses.insert(
    "/fallback.jpg".to_string(),
    (b"dummy-fallback".to_vec(), "image/jpeg"),
  );
  responses.insert(
    "/__shutdown__".to_string(),
    (b"shutdown".to_vec(), "text/plain"),
  );

  let handle = spawn_server(
    listener,
    Arc::clone(&hits),
    responses,
    vec!["/small2.jpg", "/__shutdown__"],
  );

  let tmp = TempDir::new().expect("tempdir");
  let page_url = format!("http://{}/", addr);
  let stem = pageset_stem(&page_url).expect("stem");

  let html_dir = tmp.path().join("fetches/html");
  std::fs::create_dir_all(&html_dir).expect("create html dir");

  let html_path = html_dir.join(format!("{stem}.html"));
  std::fs::write(
    &html_path,
    "<!doctype html><html><body>\
      <picture>\
        <source media=\"(max-width: 600px)\" srcset=\"/small2.jpg 2x\">\
        <source media=\"(min-width: 601px)\" srcset=\"/large2.jpg 2x\">\
        <img src=\"/fallback.jpg\">\
      </picture>\
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
    .arg("--viewport")
    .arg("500x800")
    .arg("--dpr")
    .arg("2.0")
    .arg("--max-image-urls-per-element")
    .arg("1")
    .status()
    .expect("run prefetch_assets");
  assert!(status.success(), "prefetch_assets should succeed");

  let shutdown_url = format!("http://{}/__shutdown__", addr);
  let _ = ureq::get(&shutdown_url).call();

  handle.join().unwrap();

  let asset_dir = tmp.path().join("fetches/assets");
  assert!(asset_dir.is_dir(), "disk cache dir should be created");

  let small2 = format!("http://{}/small2.jpg", addr);
  let large2 = format!("http://{}/large2.jpg", addr);
  let fallback = format!("http://{}/fallback.jpg", addr);

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
    offline
      .fetch_with_request(FetchRequest::new(&small2, FetchDestination::Image))
      .is_ok(),
    "selected picture source should be cached on disk"
  );
  assert!(
    offline
      .fetch_with_request(FetchRequest::new(&large2, FetchDestination::Image))
      .is_err(),
    "non-matching picture source should not be cached"
  );
  assert!(
    offline
      .fetch_with_request(FetchRequest::new(&fallback, FetchDestination::Image))
      .is_err(),
    "img fallback should not be cached (max urls per element = 1)"
  );

  let hits = hits.lock().unwrap();
  assert_eq!(hits.get("/small2.jpg").copied().unwrap_or(0), 1);
  assert_eq!(hits.get("/large2.jpg").copied().unwrap_or(0), 0);
  assert_eq!(hits.get("/fallback.jpg").copied().unwrap_or(0), 0);
}
