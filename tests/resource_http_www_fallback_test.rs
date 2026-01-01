use fastrender::resource::{HttpFetcher, HttpRetryPolicy};
use fastrender::ResourceFetcher;
use std::io;
use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::thread;
use std::time::{Duration, Instant};
 
const MAX_WAIT: Duration = Duration::from_secs(2);
 
#[track_caller]
fn try_bind_localhost(context: &str) -> Option<TcpListener> {
  match TcpListener::bind("127.0.0.1:0") {
    Ok(listener) => Some(listener),
    Err(err)
      if matches!(
        err.kind(),
        io::ErrorKind::PermissionDenied | io::ErrorKind::AddrNotAvailable
      ) =>
    {
      let loc = std::panic::Location::caller();
      eprintln!(
        "skipping {context} ({}:{}): cannot bind localhost in this environment: {err}",
        loc.file(),
        loc.line()
      );
      None
    }
    Err(err) => {
      let loc = std::panic::Location::caller();
      panic!("bind {context} ({}:{}): {err}", loc.file(), loc.line());
    }
  }
}
 
fn read_request(stream: &mut TcpStream) -> Vec<u8> {
  let mut buf = Vec::new();
  let mut tmp = [0u8; 1024];
  let start = Instant::now();
  while start.elapsed() < MAX_WAIT {
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
      }
      Err(_) => break,
    }
  }
  buf
}
 
fn extract_host_header(req: &[u8]) -> Option<String> {
  let text = String::from_utf8_lossy(req);
  for line in text.lines() {
    let line = line.trim_end_matches('\r');
    if line.len() >= 5 && line[..5].eq_ignore_ascii_case("host:") {
      return Some(line[5..].trim().to_string());
    }
  }
  None
}
 
fn spawn_server(listener: TcpListener, port: u16) -> thread::JoinHandle<()> {
  let seen_www = Arc::new(AtomicBool::new(false));
  let seen_www_accept = Arc::clone(&seen_www);
  thread::spawn(move || {
    let _ = listener.set_nonblocking(true);
    let start = Instant::now();
    let mut last_activity = Instant::now();
    let mut joins = Vec::new();
 
    while start.elapsed() < MAX_WAIT {
      if seen_www_accept.load(Ordering::Relaxed) && last_activity.elapsed() > Duration::from_millis(200)
      {
        break;
      }
      match listener.accept() {
        Ok((mut stream, _)) => {
          last_activity = Instant::now();
          let seen_www = Arc::clone(&seen_www_accept);
          joins.push(thread::spawn(move || {
            let _ = stream.set_nonblocking(true);
            let req = read_request(&mut stream);
            let _ = stream.set_nonblocking(false);
 
            let host = extract_host_header(&req).unwrap_or_default();
            let expected_local = format!("localhost:{port}");
            let expected_www = format!("www.localhost:{port}");
 
            if host.eq_ignore_ascii_case(&expected_www) {
              seen_www.store(true, Ordering::Relaxed);
              let body = b"www-ok";
              let response = format!(
                "HTTP/1.1 200 OK\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
                body.len()
              );
              let _ = stream.write_all(response.as_bytes());
              let _ = stream.write_all(body);
              return;
            }
 
            if host.eq_ignore_ascii_case(&expected_local) || host.eq_ignore_ascii_case("localhost") {
              // Deliberately do not respond; hold the connection open long enough for the client to
              // hit its timeout so the fetcher is forced to retry with the `www.` hostname.
              thread::sleep(Duration::from_millis(450));
              return;
            }
 
            let response = "HTTP/1.1 400 Bad Request\r\nContent-Length: 0\r\nConnection: close\r\n\r\n";
            let _ = stream.write_all(response.as_bytes());
          }));
        }
        Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => {
          thread::sleep(Duration::from_millis(5));
        }
        Err(_) => break,
      }
    }
 
    for join in joins {
      let _ = join.join();
    }
  })
}
 
#[test]
fn http_fetch_www_fallback_on_timeout() {
  let Some(listener) = try_bind_localhost("http_fetch_www_fallback_on_timeout") else {
    return;
  };
  let port = listener.local_addr().unwrap().port();
  let handle = spawn_server(listener, port);
 
  let fetcher = HttpFetcher::new()
    .with_timeout(Duration::from_millis(300))
    .with_retry_policy(HttpRetryPolicy {
      max_attempts: 1,
      ..HttpRetryPolicy::default()
    });
  let url = format!("http://localhost:{port}/");
  let res = fetcher
    .fetch(&url)
    .expect("fetch should succeed after www fallback");
  assert_eq!(res.bytes, b"www-ok");
  let final_url = res.final_url.expect("final url");
  assert!(
    final_url.contains("www.localhost"),
    "expected final_url to reflect www fallback, got {final_url}"
  );
 
  handle.join().unwrap();
}
