use fastrender::resource::{CachingFetcher, CachingFetcherConfig, HttpFetcher, ResourcePolicy};
use fastrender::ResourceFetcher;
use httpdate::fmt_http_date;
use std::io;
use std::io::Read;
use std::io::Write;
use std::net::TcpListener;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, Instant, SystemTime};

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

fn spawn_server<F>(
  listener: TcpListener,
  max_requests: usize,
  mut handler: F,
) -> thread::JoinHandle<()>
where
  F: FnMut(usize, Vec<u8>, &mut std::net::TcpStream) + Send + 'static,
{
  thread::spawn(move || {
    let _ = listener.set_nonblocking(true);
    let start = Instant::now();
    let mut handled = 0;
    while handled < max_requests && start.elapsed() < MAX_WAIT {
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
          handled += 1;
          handler(handled, buf, &mut stream);
        }
        Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => {
          thread::sleep(Duration::from_millis(5));
        }
        Err(_) => break,
      }
    }
  })
}

fn caching_fetcher() -> CachingFetcher<HttpFetcher> {
  let config = CachingFetcherConfig {
    honor_http_cache_freshness: true,
    ..Default::default()
  };
  CachingFetcher::with_config(
    HttpFetcher::new().with_timeout(Duration::from_secs(2)),
    config,
  )
}

fn caching_fetcher_with_policy(policy: ResourcePolicy) -> CachingFetcher<HttpFetcher> {
  let config = CachingFetcherConfig {
    honor_http_cache_freshness: true,
    ..Default::default()
  };
  CachingFetcher::with_config(
    HttpFetcher::new()
      .with_timeout(Duration::from_secs(2))
      .with_policy(policy.clone()),
    config,
  )
  .with_policy(policy)
}

#[test]
fn cache_control_max_age_skips_revalidation() {
  let Some(listener) = try_bind_localhost("cache_control_max_age_skips_revalidation") else {
    return;
  };
  let addr = listener.local_addr().unwrap();
  let hits = Arc::new(AtomicUsize::new(0));
  let hits_server = Arc::clone(&hits);
  let handle = spawn_server(listener, 2, move |_count, _req, stream| {
    hits_server.fetch_add(1, Ordering::SeqCst);
    let body = b"fresh";
    let response = format!(
      "HTTP/1.1 200 OK\r\nCache-Control: max-age=3600\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
      body.len()
    );
    let _ = stream.write_all(response.as_bytes());
    let _ = stream.write_all(body);
  });

  let fetcher = caching_fetcher();
  let url = format!("http://{}/", addr);
  let first = fetcher.fetch(&url).expect("initial fetch");
  let second = fetcher.fetch(&url).expect("cached fetch");
  handle.join().unwrap();

  assert_eq!(first.bytes, second.bytes);
  assert_eq!(
    hits.load(Ordering::SeqCst),
    1,
    "server should only see first fetch"
  );
}

#[test]
fn etag_revalidation_uses_validators() {
  let Some(listener) = try_bind_localhost("etag_revalidation_uses_validators") else {
    return;
  };
  let addr = listener.local_addr().unwrap();
  let hits = Arc::new(AtomicUsize::new(0));
  let hits_thread = Arc::clone(&hits);
  let captured_requests = Arc::new(Mutex::new(Vec::new()));
  let captured_requests_thread = Arc::clone(&captured_requests);
  let handle = spawn_server(listener, 2, move |count, req, stream| {
    hits_thread.fetch_add(1, Ordering::SeqCst);
    captured_requests_thread.lock().unwrap().push(req.clone());
    let request = String::from_utf8_lossy(&req).to_lowercase();
    match count {
      1 => {
        let body = b"etag-body";
        let response = format!(
          "HTTP/1.1 200 OK\r\nCache-Control: max-age=0\r\nETag: \"v1\"\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
          body.len()
        );
        let _ = stream.write_all(response.as_bytes());
        let _ = stream.write_all(body);
      }
      2 => {
        assert!(
          request.contains("if-none-match: \"v1\""),
          "second request should carry validator: {request}"
        );
        let response = "HTTP/1.1 304 Not Modified\r\nCache-Control: max-age=0\r\nETag: \"v2\"\r\nContent-Length: 0\r\nConnection: close\r\n\r\n";
        let _ = stream.write_all(response.as_bytes());
      }
      _ => {
        assert!(
          request.contains("if-none-match: \"v2\""),
          "subsequent requests should reuse updated validator: {request}"
        );
        let response =
          "HTTP/1.1 304 Not Modified\r\nContent-Length: 0\r\nConnection: close\r\n\r\n";
        let _ = stream.write_all(response.as_bytes());
      }
    }
  });

  let fetcher = caching_fetcher();
  let url = format!("http://{}/etag", addr);
  let first = fetcher.fetch(&url).expect("initial fetch");
  let second = fetcher.fetch(&url).expect("revalidated fetch");
  let third = fetcher.fetch(&url).expect("revalidated again");
  handle.join().unwrap();

  assert_eq!(first.bytes, b"etag-body");
  assert_eq!(second.bytes, b"etag-body");
  assert_eq!(third.bytes, b"etag-body");
  assert_eq!(hits.load(Ordering::SeqCst), 3);
  let requests = captured_requests.lock().unwrap();
  assert!(requests.len() >= 2);
}

#[test]
fn revalidation_uses_budget_without_double_counting() {
  let Some(listener) = try_bind_localhost("revalidation_uses_budget_without_double_counting")
  else {
    return;
  };
  let addr = listener.local_addr().unwrap();
  let handle = spawn_server(listener, 2, move |count, req, stream| {
    let request = String::from_utf8_lossy(&req).to_lowercase();
    let body = b"hit";
    match count {
      1 => {
        let response = format!(
          "HTTP/1.1 200 OK\r\nCache-Control: max-age=0\r\nETag: \"budget\"\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
          body.len()
        );
        let _ = stream.write_all(response.as_bytes());
        let _ = stream.write_all(body);
      }
      _ => {
        assert!(
          request.contains("if-none-match: \"budget\""),
          "revalidation request should carry ETag: {request}"
        );
        let response = "HTTP/1.1 304 Not Modified\r\nCache-Control: max-age=0\r\nContent-Length: 0\r\nConnection: close\r\n\r\n";
        let _ = stream.write_all(response.as_bytes());
      }
    }
  });

  let policy = ResourcePolicy::default().with_total_bytes_budget(Some(6));
  let fetcher = caching_fetcher_with_policy(policy);
  let url = format!("http://{}/budget", addr);
  let first = fetcher.fetch(&url).expect("initial fetch");
  assert_eq!(first.bytes, b"hit");

  let second = fetcher.fetch(&url).expect("revalidated fetch");
  assert_eq!(second.bytes, b"hit");

  let err = fetcher
    .fetch(&url)
    .expect_err("budget exhausted after cached hits");
  assert!(
    err.to_string().contains("budget"),
    "unexpected error: {err:?}"
  );

  handle.join().unwrap();
}

#[test]
fn no_store_responses_are_not_cached() {
  let Some(listener) = try_bind_localhost("no_store_responses_are_not_cached") else {
    return;
  };
  let addr = listener.local_addr().unwrap();
  let hits = Arc::new(AtomicUsize::new(0));
  let hits_server = Arc::clone(&hits);
  let handle = spawn_server(listener, 3, move |_count, _req, stream| {
    hits_server.fetch_add(1, Ordering::SeqCst);
    let body = b"nostore";
    let response = format!(
      "HTTP/1.1 200 OK\r\nCache-Control: no-store\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
      body.len()
    );
    let _ = stream.write_all(response.as_bytes());
    let _ = stream.write_all(body);
  });

  let fetcher = caching_fetcher();
  let url = format!("http://{}/nostore", addr);
  let first = fetcher.fetch(&url).expect("first fetch");
  let second = fetcher.fetch(&url).expect("second fetch");
  handle.join().unwrap();

  assert_eq!(first.bytes, b"nostore");
  assert_eq!(second.bytes, b"nostore");
  assert_eq!(
    hits.load(Ordering::SeqCst),
    2,
    "no-store responses must not be cached"
  );
}

#[test]
fn expires_header_controls_freshness() {
  let Some(listener) = try_bind_localhost("expires_header_controls_freshness") else {
    return;
  };
  let addr = listener.local_addr().unwrap();
  let hits = Arc::new(AtomicUsize::new(0));
  let hits_server = Arc::clone(&hits);
  let expires = fmt_http_date(SystemTime::now() + Duration::from_secs(1800));
  let handle = spawn_server(listener, 2, move |_count, _req, stream| {
    hits_server.fetch_add(1, Ordering::SeqCst);
    let body = b"expires";
    let response = format!(
      "HTTP/1.1 200 OK\r\nExpires: {expires}\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
      body.len()
    );
    let _ = stream.write_all(response.as_bytes());
    let _ = stream.write_all(body);
  });

  let fetcher = caching_fetcher();
  let url = format!("http://{}/expires", addr);
  let first = fetcher.fetch(&url).expect("initial fetch");
  let second = fetcher.fetch(&url).expect("cached fetch");
  handle.join().unwrap();

  assert_eq!(first.bytes, b"expires");
  assert_eq!(second.bytes, b"expires");
  assert_eq!(
    hits.load(Ordering::SeqCst),
    1,
    "Expires should allow serving cached entry"
  );
}
