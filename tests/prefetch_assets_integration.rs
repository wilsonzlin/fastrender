#![cfg(feature = "disk_cache")]

use fastrender::pageset::pageset_stem;
use fastrender::resource::DiskCachingFetcher;
use fastrender::resource::FetchedResource;
use fastrender::Error;
use fastrender::ResourceFetcher;
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

const MAX_WAIT: Duration = Duration::from_secs(3);

fn try_bind_localhost(context: &str) -> Option<TcpListener> {
  match TcpListener::bind("127.0.0.1:0") {
    Ok(listener) => Some(listener),
    Err(err) if err.kind() == io::ErrorKind::PermissionDenied => {
      eprintln!("skipping {context}: cannot bind localhost in this environment: {err}");
      None
    }
    Err(err) => panic!("bind {context}: {err}"),
  }
}

fn spawn_server(
  listener: TcpListener,
  hits: Arc<Mutex<HashMap<String, usize>>>,
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

          match path {
            "/a.css" => {
              let body = b"@import \"/b.css\"; @font-face { src: url(\"/f.woff2\") }";
              let response = format!(
                "HTTP/1.1 200 OK\r\nContent-Type: text/css\r\nCache-Control: max-age=3600\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
                body.len()
              );
              let _ = stream.write_all(response.as_bytes());
              let _ = stream.write_all(body);
            }
            "/b.css" => {
              let body = b"body { color: black }";
              let response = format!(
                "HTTP/1.1 200 OK\r\nContent-Type: text/css\r\nCache-Control: max-age=3600\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
                body.len()
              );
              let _ = stream.write_all(response.as_bytes());
              let _ = stream.write_all(body);
            }
            "/f.woff2" => {
              let body = b"dummy-font-bytes";
              let response = format!(
                "HTTP/1.1 200 OK\r\nContent-Type: font/woff2\r\nCache-Control: max-age=3600\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
                body.len()
              );
              let _ = stream.write_all(response.as_bytes());
              let _ = stream.write_all(body);
            }
            _ => {
              let body = b"not found";
              let response = format!(
                "HTTP/1.1 404 Not Found\r\nContent-Type: text/plain\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
                body.len()
              );
              let _ = stream.write_all(response.as_bytes());
              let _ = stream.write_all(body);
            }
          }

          if let Ok(map) = hits.lock() {
            if map.get("/a.css").copied().unwrap_or(0) > 0
              && map.get("/b.css").copied().unwrap_or(0) > 0
              && map.get("/f.woff2").copied().unwrap_or(0) > 0
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
  let handle = spawn_server(listener, Arc::clone(&hits));

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

  let offline = DiskCachingFetcher::new(FailFetcher, asset_dir.clone());
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
