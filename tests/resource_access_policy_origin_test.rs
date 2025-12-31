use fastrender::api::{FastRender, RenderArtifactRequest, RenderOptions};
use fastrender::ResourceKind;
use std::fmt::Write as FmtWrite;
use std::io::{Read, Write};
use std::net::SocketAddr;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::Arc;
use std::thread;
use std::time::{Duration, Instant};

mod test_support;
use test_support::net::try_bind_localhost;

#[derive(Clone)]
struct TestResponse {
  status: u16,
  headers: Vec<(String, String)>,
  body: String,
}

struct TestServer {
  addr: SocketAddr,
  served: Arc<AtomicUsize>,
  shutdown: Arc<AtomicBool>,
  handle: Option<thread::JoinHandle<()>>,
}

impl TestServer {
  fn start(
    context: &str,
    expected: usize,
    handler: impl Fn(&str) -> TestResponse + Send + Sync + 'static,
  ) -> Option<Self> {
    let listener = try_bind_localhost(context)?;
    listener
      .set_nonblocking(true)
      .expect("set nonblocking listener");
    let addr = listener.local_addr().expect("server addr");
    let served = Arc::new(AtomicUsize::new(0));
    let served_clone = Arc::clone(&served);
    let handler = Arc::new(handler);
    let shutdown = Arc::new(AtomicBool::new(false));
    let shutdown_clone = Arc::clone(&shutdown);

    let handle = thread::spawn(move || {
      let start = Instant::now();
      loop {
        if shutdown_clone.load(Ordering::SeqCst) {
          break;
        }

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
            if shutdown_clone.load(Ordering::SeqCst) {
              break;
            }
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

    Some(Self {
      addr,
      served,
      shutdown,
      handle: Some(handle),
    })
  }

  fn url(&self, path: &str) -> String {
    format!("http://{}/{}", self.addr, path.trim_start_matches('/'))
  }

  fn served(&self) -> usize {
    self.served.load(Ordering::SeqCst)
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
fn same_origin_stylesheet_allowed() {
  let Some(server) = TestServer::start("same_origin_stylesheet_allowed", 1, |path| {
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
  }) else {
    return;
  };

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
  let Some(origin_server) = TestServer::start(
    "cross_origin_stylesheet_blocked.origin_server",
    0,
    |_path| TestResponse {
      status: 404,
      headers: Vec::new(),
      body: String::new(),
    },
  ) else {
    return;
  };

  let Some(blocked_server) = TestServer::start(
    "cross_origin_stylesheet_blocked.blocked_server",
    0,
    |_path| TestResponse {
      status: 200,
      headers: vec![("Content-Type".into(), "text/css".into())],
      body: "body { color: red; }".into(),
    },
  ) else {
    return;
  };

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
  let Some(cross_origin) = TestServer::start(
    "redirects_to_cross_origin_are_blocked.cross_origin",
    1,
    |_path| TestResponse {
      status: 200,
      headers: vec![("Content-Type".into(), "text/css".into())],
      body: "body { color: blue; }".into(),
    },
  ) else {
    return;
  };

  let redirect_target = cross_origin.url("final.css");
  let Some(redirect_origin) = TestServer::start(
    "redirects_to_cross_origin_are_blocked.redirect_origin",
    1,
    move |_path| TestResponse {
      status: 302,
      headers: vec![("Location".into(), redirect_target.clone())],
      body: String::new(),
    },
  ) else {
    return;
  };

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
