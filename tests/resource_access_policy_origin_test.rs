use fastrender::api::{FastRender, RenderArtifactRequest, RenderOptions};
use fastrender::ResourceKind;
use std::fmt::Write;
use std::io::{Read, Write as IoWrite};
use std::net::{SocketAddr, TcpListener};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::thread;
use std::time::{Duration, Instant};

#[derive(Clone)]
struct TestResponse {
  status: u16,
  headers: Vec<(String, String)>,
  body: String,
}

struct TestServer {
  addr: SocketAddr,
  served: Arc<AtomicUsize>,
  handle: thread::JoinHandle<()>,
}

impl TestServer {
  fn start(
    expected: usize,
    handler: impl Fn(&str) -> TestResponse + Send + Sync + 'static,
  ) -> Self {
    let listener = TcpListener::bind("127.0.0.1:0").expect("bind test server");
    listener
      .set_nonblocking(true)
      .expect("set nonblocking listener");
    let addr = listener.local_addr().expect("server addr");
    let served = Arc::new(AtomicUsize::new(0));
    let served_clone = Arc::clone(&served);
    let handler = Arc::new(handler);

    let handle = thread::spawn(move || {
      let start = Instant::now();
      loop {
        if served_clone.load(Ordering::SeqCst) >= expected
          && start.elapsed() > Duration::from_millis(200)
        {
          break;
        }

        match listener.accept() {
          Ok((mut stream, _)) => {
            let mut buf = [0u8; 1024];
            let n = stream.read(&mut buf).expect("read request");
            if n == 0 {
              continue;
            }
            let request = String::from_utf8_lossy(&buf[..n]);
            let path = request
              .lines()
              .next()
              .and_then(|line| line.split_whitespace().nth(1))
              .unwrap_or("/");
            let response = handler(path);
            let mut raw = format!(
              "HTTP/1.1 {} {}\r\n",
              response.status,
              if response.status == 302 {
                "Found"
              } else {
                "OK"
              }
            );
            for (name, value) in &response.headers {
              let _ = write!(raw, "{name}: {value}\r\n");
            }
            let _ = write!(
              raw,
              "Content-Length: {}\r\n\r\n{}",
              response.body.len(),
              response.body
            );
            let _ = stream.write_all(raw.as_bytes());
            let _ = stream.flush();
            served_clone.fetch_add(1, Ordering::SeqCst);
          }
          Err(ref e) if e.kind() == std::io::ErrorKind::WouldBlock => {
            if start.elapsed() > Duration::from_millis(200)
              && served_clone.load(Ordering::SeqCst) >= expected
            {
              break;
            }
            thread::sleep(Duration::from_millis(10));
          }
          Err(_) => break,
        }
      }
    });

    Self {
      addr,
      served,
      handle,
    }
  }

  fn url(&self, path: &str) -> String {
    format!("http://{}/{}", self.addr, path.trim_start_matches('/'))
  }

  fn served(&self) -> usize {
    self.served.load(Ordering::SeqCst)
  }

  fn join(self) {
    let _ = self.handle.join();
  }
}

#[test]
fn same_origin_stylesheet_allowed() {
  let server = TestServer::start(1, |path| {
    if path == "/style.css" {
      return TestResponse {
        status: 200,
        headers: vec![("Content-Type".into(), "text/css".into())],
        body: "body { background: rgb(0, 255, 0); }".into(),
      };
    }
    TestResponse {
      status: 404,
      headers: Vec::new(),
      body: String::new(),
    }
  });

  let html = r#"<!doctype html><link rel="stylesheet" href="/style.css"><body>ok</body>"#;
  let base = format!("http://{}/", server.addr);
  let mut renderer = FastRender::builder()
    .same_origin_subresources(true)
    .build()
    .expect("renderer");

  let report = renderer
    .render_html_with_stylesheets_report(
      html,
      &base,
      RenderOptions::new(),
      RenderArtifactRequest::none(),
    )
    .expect("render");

  assert!(
    report.diagnostics.fetch_errors.is_empty(),
    "unexpected fetch errors: {:?}",
    report.diagnostics.fetch_errors
  );

  server.join();
}

#[test]
fn cross_origin_stylesheet_blocked() {
  let origin_server = TestServer::start(0, |_path| TestResponse {
    status: 404,
    headers: Vec::new(),
    body: String::new(),
  });
  let blocked_server = TestServer::start(0, |_path| TestResponse {
    status: 200,
    headers: vec![("Content-Type".into(), "text/css".into())],
    body: "body { color: red; }".into(),
  });

  let css_url = blocked_server.url("blocked.css");
  let html = format!(
    "<!doctype html><link rel=\"stylesheet\" href=\"{}\"><body>blocked</body>",
    css_url
  );
  let base = format!("http://{}/", origin_server.addr);
  let mut renderer = FastRender::builder()
    .same_origin_subresources(true)
    .build()
    .expect("renderer");

  let report = renderer
    .render_html_with_stylesheets_report(
      &html,
      &base,
      RenderOptions::new(),
      RenderArtifactRequest::none(),
    )
    .expect("render");

  assert_eq!(report.diagnostics.fetch_errors.len(), 1);
  let error = &report.diagnostics.fetch_errors[0];
  assert_eq!(error.kind, ResourceKind::Stylesheet);
  assert_eq!(error.url, css_url);
  assert!(
    error.message.contains("cross-origin") && error.message.contains(&css_url),
    "unexpected message: {}",
    error.message
  );
  assert!(
    blocked_server.served() == 0 || blocked_server.served() == 1,
    "server saw unexpected requests"
  );

  origin_server.join();
  blocked_server.join();
}

#[test]
fn redirects_to_cross_origin_are_blocked() {
  let cross_origin = TestServer::start(1, |_path| TestResponse {
    status: 200,
    headers: vec![("Content-Type".into(), "text/css".into())],
    body: "body { color: blue; }".into(),
  });

  let redirect_target = cross_origin.url("final.css");
  let redirect_origin = TestServer::start(1, move |_path| TestResponse {
    status: 302,
    headers: vec![("Location".into(), redirect_target.clone())],
    body: String::new(),
  });

  let html = format!(
    "<!doctype html><link rel=\"stylesheet\" href=\"{}\">",
    redirect_origin.url("redirect.css")
  );
  let base = format!("http://{}/", redirect_origin.addr);
  let mut renderer = FastRender::builder()
    .same_origin_subresources(true)
    .build()
    .expect("renderer");

  let report = renderer
    .render_html_with_stylesheets_report(
      &html,
      &base,
      RenderOptions::new(),
      RenderArtifactRequest::none(),
    )
    .expect("render");

  assert_eq!(report.diagnostics.fetch_errors.len(), 1);
  let error = &report.diagnostics.fetch_errors[0];
  assert_eq!(error.kind, ResourceKind::Stylesheet);
  assert_eq!(error.url, redirect_origin.url("redirect.css"));
  assert_eq!(error.final_url, Some(cross_origin.url("final.css")));
  assert!(
    error
      .message
      .contains(&format!("{} does not match", redirect_origin.addr)),
    "unexpected message: {}",
    error.message
  );

  redirect_origin.join();
  cross_origin.join();
}
