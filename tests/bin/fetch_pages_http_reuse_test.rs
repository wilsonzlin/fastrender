use crate::test_support::net::try_bind_localhost;
use fastrender::pageset::{cache_html_path, pageset_stem};
use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use std::process::Command;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::thread;
use std::time::{Duration, Instant};
use tempfile::TempDir;

fn parse_http_request_path(buffer: &mut Vec<u8>) -> Option<String> {
  let end = buffer
    .windows(4)
    .position(|w| w == b"\r\n\r\n")
    .map(|pos| pos + 4)?;
  let req = &buffer[..end];
  let line_end = req.windows(2).position(|w| w == b"\r\n")?;
  let line = String::from_utf8_lossy(&req[..line_end]);
  let mut parts = line.split_whitespace();
  let _method = parts.next()?;
  let path = parts.next()?.to_string();
  buffer.drain(..end);
  Some(path)
}

fn handle_connection(
  mut stream: TcpStream,
  connections: &AtomicUsize,
  requests: &AtomicUsize,
  expected_requests: usize,
  mut responder: impl FnMut(&str) -> Vec<u8>,
) {
  connections.fetch_add(1, Ordering::SeqCst);
  let _ = stream.set_read_timeout(Some(Duration::from_secs(1)));
  let mut buffer = Vec::new();
  let mut temp = [0u8; 4096];

  while requests.load(Ordering::SeqCst) < expected_requests {
    if let Some(path) = parse_http_request_path(&mut buffer) {
      let body = responder(&path);
      let headers = format!(
        "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nContent-Length: {}\r\nConnection: keep-alive\r\n\r\n",
        body.len()
      );
      let _ = stream.write_all(headers.as_bytes());
      let _ = stream.write_all(&body);
      requests.fetch_add(1, Ordering::SeqCst);
      continue;
    }

    match stream.read(&mut temp) {
      Ok(0) => break,
      Ok(n) => buffer.extend_from_slice(&temp[..n]),
      Err(err)
        if matches!(
          err.kind(),
          std::io::ErrorKind::WouldBlock | std::io::ErrorKind::TimedOut
        ) =>
      {
        break
      }
      Err(_) => break,
    }
  }
}

fn spawn_test_server(
  listener: TcpListener,
  expected_requests: usize,
  responder: impl FnMut(&str) -> Vec<u8> + Send + 'static,
) -> (Arc<AtomicUsize>, Arc<AtomicUsize>, thread::JoinHandle<()>) {
  let connections = Arc::new(AtomicUsize::new(0));
  let requests = Arc::new(AtomicUsize::new(0));
  let connections_thread = Arc::clone(&connections);
  let requests_thread = Arc::clone(&requests);

  let handle = thread::spawn(move || {
    let deadline = Instant::now() + Duration::from_secs(5);
    let _ = listener.set_nonblocking(true);
    let mut responder = responder;
    while requests_thread.load(Ordering::SeqCst) < expected_requests && Instant::now() < deadline {
      match listener.accept() {
        Ok((stream, _)) => handle_connection(
          stream,
          &connections_thread,
          &requests_thread,
          expected_requests,
          &mut responder,
        ),
        Err(err) if err.kind() == std::io::ErrorKind::WouldBlock => {
          thread::sleep(Duration::from_millis(5));
        }
        Err(_) => break,
      }
    }
  });

  (connections, requests, handle)
}

#[test]
fn fetch_pages_reuses_http_connections_sequentially() {
  let Some(listener) = try_bind_localhost("fetch_pages_reuses_http_connections_sequentially")
  else {
    return;
  };
  let addr = listener.local_addr().expect("addr");

  let (connections, requests, server) = spawn_test_server(listener, 2, |path| match path {
    "/one" => b"<html>one</html>".to_vec(),
    "/two" => b"<html>two</html>".to_vec(),
    other => format!("<html>unexpected {other}</html>").into_bytes(),
  });

  let temp = TempDir::new().expect("tempdir");
  let url_one = format!("http://{addr}/one");
  let url_two = format!("http://{addr}/two");

  let status = Command::new(env!("CARGO_BIN_EXE_fetch_pages"))
    .current_dir(temp.path())
    .env("FASTR_PAGESET_URLS", format!("{url_one},{url_two}"))
    .args(["--jobs", "1", "--refresh", "--timeout", "5"])
    .status()
    .expect("run fetch_pages");
  assert!(status.success(), "fetch_pages should succeed");

  server.join().expect("server thread");
  assert_eq!(
    requests.load(Ordering::SeqCst),
    2,
    "expected server to receive both requests"
  );
  assert_eq!(
    connections.load(Ordering::SeqCst),
    1,
    "expected a single keep-alive connection when --jobs 1"
  );
}

#[test]
fn fetch_pages_converges_multi_hop_client_redirects() {
  let Some(listener) = try_bind_localhost("fetch_pages_converges_multi_hop_client_redirects")
  else {
    return;
  };
  let addr = listener.local_addr().expect("addr");

  let (connections, requests, server) = spawn_test_server(listener, 3, |path| match path {
    "/a" => b"<meta http-equiv=\"refresh\" content=\"0;url=/b\">".to_vec(),
    "/b" => b"<script>window.location.href='/c'</script>".to_vec(),
    "/c" => b"<html>final</html>".to_vec(),
    other => format!("<html>unexpected {other}</html>").into_bytes(),
  });

  let temp = TempDir::new().expect("tempdir");
  let url_a = format!("http://{addr}/a");
  let expected_final = format!("http://{addr}/c");

  let status = Command::new(env!("CARGO_BIN_EXE_fetch_pages"))
    .current_dir(temp.path())
    .env("FASTR_PAGESET_URLS", &url_a)
    .args(["--jobs", "1", "--refresh", "--timeout", "5"])
    .status()
    .expect("run fetch_pages");
  assert!(status.success(), "fetch_pages should succeed");

  server.join().expect("server thread");
  assert!(
    connections.load(Ordering::SeqCst) >= 1,
    "server should accept at least one connection"
  );
  assert_eq!(
    requests.load(Ordering::SeqCst),
    3,
    "expected server to receive a->b->c requests"
  );

  let stem = pageset_stem(&url_a).expect("stem");
  let cache_path = temp.path().join(cache_html_path(&stem));
  let html = std::fs::read_to_string(&cache_path).expect("cached html");
  assert!(
    html.contains("final"),
    "expected cached HTML to contain final redirect target, got: {html}"
  );

  let meta_path = cache_path.with_extension("html.meta");
  let meta = std::fs::read_to_string(meta_path).expect("meta");
  assert!(
    meta.contains(&format!("url: {expected_final}")),
    "expected meta to record final url {expected_final}, got:\n{meta}"
  );
}
