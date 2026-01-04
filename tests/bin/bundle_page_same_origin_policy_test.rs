use crate::test_support::net::try_bind_localhost;
use serde_json::Value;
use std::io::{Read, Write};
use std::net::{SocketAddr, TcpListener};
use std::process::Command;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, Instant};
use tempfile::TempDir;
 
struct TestServer {
  addr: SocketAddr,
  requests: Arc<Mutex<Vec<String>>>,
  shutdown: Arc<AtomicBool>,
  handle: Option<thread::JoinHandle<()>>,
}
 
impl TestServer {
  fn start(
    context: &str,
    expected: usize,
    handler: impl Fn(&str) -> (u16, &'static str, String) + Send + Sync + 'static,
  ) -> Option<Self> {
    let listener = try_bind_localhost(context)?;
    listener
      .set_nonblocking(true)
      .expect("set nonblocking listener");
    let addr = listener.local_addr().expect("addr");
    let requests = Arc::new(Mutex::new(Vec::new()));
    let requests_thread = Arc::clone(&requests);
    let shutdown = Arc::new(AtomicBool::new(false));
    let shutdown_thread = Arc::clone(&shutdown);
    let handler = Arc::new(handler);
 
    let handle = thread::spawn(move || {
      let start = Instant::now();
      // Keep the server responsive but bounded so failures don't hang CI.
      let deadline = start + Duration::from_secs(5);
      let mut served = 0usize;
      while Instant::now() < deadline {
        if shutdown_thread.load(Ordering::SeqCst) {
          break;
        }
        if served >= expected && start.elapsed() > Duration::from_millis(200) {
          break;
        }
        match listener.accept() {
          Ok((mut stream, _)) => {
            let _ = stream.set_read_timeout(Some(Duration::from_secs(1)));
            let mut buf = [0u8; 2048];
            let n = match stream.read(&mut buf) {
              Ok(n) => n,
              Err(_) => continue,
            };
            if n == 0 {
              continue;
            }
            let req = String::from_utf8_lossy(&buf[..n]);
            let path = req
              .lines()
              .next()
              .and_then(|line| line.split_whitespace().nth(1))
              .unwrap_or("/");
 
            if let Ok(mut guard) = requests_thread.lock() {
              guard.push(path.to_string());
            }
 
            let (status, content_type, body) = handler(path);
            let response = format!(
              "HTTP/1.1 {status} OK\r\nContent-Type: {content_type}\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{body}",
              body.len()
            );
            let _ = stream.write_all(response.as_bytes());
            let _ = stream.flush();
            served += 1;
          }
          Err(err) if err.kind() == std::io::ErrorKind::WouldBlock => {
            thread::sleep(Duration::from_millis(5));
          }
          Err(_) => break,
        }
      }
    });
 
    Some(Self {
      addr,
      requests,
      shutdown,
      handle: Some(handle),
    })
  }
 
  fn url(&self, path: &str) -> String {
    format!("http://{}/{}", self.addr, path.trim_start_matches('/'))
  }
 
  fn requests(&self) -> Vec<String> {
    self
      .requests
      .lock()
      .map(|guard| guard.clone())
      .unwrap_or_default()
  }
 
  fn join(mut self) {
    self.shutdown.store(true, Ordering::SeqCst);
    if let Some(handle) = self.handle.take() {
      let _ = handle.join();
    }
  }
}
 
impl Drop for TestServer {
  fn drop(&mut self) {
    self.shutdown.store(true, Ordering::SeqCst);
    if let Some(handle) = self.handle.take() {
      let _ = handle.join();
    }
  }
}
 
#[test]
fn bundle_page_same_origin_subresources_does_not_block_cross_origin_iframe_documents() {
  let Some(cross_origin) = TestServer::start(
    "bundle_page_same_origin_subresources_does_not_block_cross_origin_iframe_documents.cross_origin",
    10,
    |path| match path {
      "/frame.html" => (
        200,
        "text/html; charset=utf-8",
        "<!doctype html><html><body>frame</body></html>".to_string(),
      ),
      "/blocked.css" => (200, "text/css; charset=utf-8", "body { color: red; }".to_string()),
      _ => (404, "text/plain; charset=utf-8", "missing".to_string()),
    },
  ) else {
    return;
  };
 
  let frame_url = cross_origin.url("frame.html");
  let blocked_css_url = cross_origin.url("blocked.css");
  let frame_url_for_origin = frame_url.clone();
  let blocked_css_url_for_origin = blocked_css_url.clone();
 
  let Some(origin) = TestServer::start(
    "bundle_page_same_origin_subresources_does_not_block_cross_origin_iframe_documents.origin",
    10,
    move |path| match path {
      "/page.html" => (
        200,
        "text/html; charset=utf-8",
        format!(
          "<!doctype html><html><head><link rel=\"stylesheet\" href=\"{blocked_css_url_for_origin}\"></head><body><iframe src=\"{frame_url_for_origin}\"></iframe></body></html>",
        ),
      ),
      _ => (404, "text/plain; charset=utf-8", "missing".to_string()),
    },
  ) else {
    cross_origin.join();
    return;
  };
 
  let tmp = TempDir::new().expect("tempdir");
  let bundle_dir = tmp.path().join("bundle");
  let origin_url = origin.url("page.html");
 
  let status = Command::new(env!("CARGO_BIN_EXE_bundle_page"))
    .args([
      "fetch",
      &origin_url,
      "--out",
      bundle_dir.to_str().expect("bundle dir str"),
      "--no-render",
      "--same-origin-subresources",
    ])
    .status()
    .expect("run bundle_page fetch");
  assert!(status.success(), "bundle_page fetch should succeed");
 
  // The cross-origin stylesheet should be blocked but the iframe document should still be fetched
  // and persisted into the bundle.
  let cross_requests = cross_origin.requests();
  assert!(
    cross_requests.iter().any(|p| p == "/frame.html"),
    "expected iframe document to be fetched from cross origin, got {cross_requests:?}"
  );
  assert!(
    !cross_requests.iter().any(|p| p == "/blocked.css"),
    "expected stylesheet to be blocked by same-origin subresource policy, got {cross_requests:?}"
  );
 
  let manifest_bytes =
    std::fs::read(bundle_dir.join("bundle.json")).expect("read bundle manifest");
  let manifest: Value = serde_json::from_slice(&manifest_bytes).expect("parse bundle manifest");
  let resources = manifest["resources"].as_object().expect("resources object");
  assert!(
    resources.contains_key(&frame_url),
    "expected iframe document {frame_url} to be present in bundle resources (keys={:?})",
    resources.keys().collect::<Vec<_>>()
  );
  assert!(
    !resources.contains_key(&blocked_css_url),
    "expected blocked stylesheet {blocked_css_url} to be absent from bundle resources (keys={:?})",
    resources.keys().collect::<Vec<_>>()
  );

  let output_png = tmp.path().join("out.png");
  let status = Command::new(env!("CARGO_BIN_EXE_bundle_page"))
    .args([
      "render",
      bundle_dir.to_str().expect("bundle dir str"),
      "--out",
      output_png.to_str().expect("out png str"),
    ])
    .status()
    .expect("run bundle_page render");
  assert!(status.success(), "bundle_page render should succeed offline");
  let png_bytes = std::fs::read(&output_png).expect("read rendered png");
  assert!(!png_bytes.is_empty(), "expected output PNG to be written");

  let cross_requests_after_render = cross_origin.requests();
  assert_eq!(
    cross_requests_after_render, cross_requests,
    "expected bundle replay to be offline; server requests changed from {cross_requests:?} to {cross_requests_after_render:?}"
  );

  origin.join();
  cross_origin.join();
}
