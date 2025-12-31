use fastrender::resource::{HttpFetcher, HttpRetryPolicy};
use fastrender::ResourceFetcher;
use std::io;
use std::io::{Read, Write};
use std::net::TcpListener;
use std::thread;
use std::time::{Duration, Instant};

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

fn read_request(stream: &mut std::net::TcpStream) -> Vec<u8> {
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

fn spawn_server<F>(listener: TcpListener, max_requests: usize, handler: F) -> thread::JoinHandle<()>
where
  F: Fn(usize, Vec<u8>, &mut std::net::TcpStream) + Send + Sync + 'static,
{
  let handler = std::sync::Arc::new(handler);
  thread::spawn(move || {
    let _ = listener.set_nonblocking(true);
    let start = Instant::now();
    let mut handled = 0usize;
    let mut joins = Vec::new();
    while handled < max_requests && start.elapsed() < MAX_WAIT {
      match listener.accept() {
        Ok((mut stream, _)) => {
          handled += 1;
          let handler = std::sync::Arc::clone(&handler);
          let idx = handled;
          joins.push(thread::spawn(move || {
            let _ = stream.set_nonblocking(true);
            let req = read_request(&mut stream);
            let _ = stream.set_nonblocking(false);
            handler(idx, req, &mut stream);
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

fn fast_retry_fetcher() -> HttpFetcher {
  let retry = HttpRetryPolicy {
    max_attempts: 2,
    backoff_base: Duration::from_millis(10),
    backoff_cap: Duration::from_millis(20),
    respect_retry_after: true,
  };
  HttpFetcher::new().with_retry_policy(retry)
}

#[test]
fn http_fetch_retries_on_503() {
  let Some(listener) = try_bind_localhost("http_fetch_retries_on_503") else {
    return;
  };
  let addr = listener.local_addr().unwrap();
  let handle = spawn_server(listener, 2, move |count, _req, stream| match count {
    1 => {
      let response =
        "HTTP/1.1 503 Service Unavailable\r\nContent-Length: 0\r\nConnection: close\r\n\r\n";
      let _ = stream.write_all(response.as_bytes());
    }
    _ => {
      let body = b"ok";
      let response = format!(
        "HTTP/1.1 200 OK\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
        body.len()
      );
      let _ = stream.write_all(response.as_bytes());
      let _ = stream.write_all(body);
    }
  });

  let fetcher = fast_retry_fetcher();
  let url = format!("http://{addr}/");
  let res = fetcher
    .fetch(&url)
    .expect("fetch should succeed after retry");
  assert_eq!(res.bytes, b"ok");

  handle.join().unwrap();
}

#[test]
fn http_fetch_retries_on_empty_body() {
  let Some(listener) = try_bind_localhost("http_fetch_retries_on_empty_body") else {
    return;
  };
  let addr = listener.local_addr().unwrap();
  let handle = spawn_server(listener, 2, move |count, _req, stream| match count {
    1 => {
      let response = "HTTP/1.1 200 OK\r\nContent-Length: 0\r\nConnection: close\r\n\r\n";
      let _ = stream.write_all(response.as_bytes());
    }
    _ => {
      let body = b"recovered";
      let response = format!(
        "HTTP/1.1 200 OK\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
        body.len()
      );
      let _ = stream.write_all(response.as_bytes());
      let _ = stream.write_all(body);
    }
  });

  let fetcher = fast_retry_fetcher();
  let url = format!("http://{addr}/empty");
  let res = fetcher.fetch(&url).expect("fetch should retry empty body");
  assert_eq!(res.bytes, b"recovered");

  handle.join().unwrap();
}

#[test]
fn http_fetch_retries_on_202_empty_body() {
  let Some(listener) = try_bind_localhost("http_fetch_retries_on_202_empty_body") else {
    return;
  };
  let addr = listener.local_addr().unwrap();
  let handle = spawn_server(listener, 2, move |count, _req, stream| match count {
    1 => {
      let response = "HTTP/1.1 202 Accepted\r\nContent-Length: 0\r\nConnection: close\r\n\r\n";
      let _ = stream.write_all(response.as_bytes());
    }
    _ => {
      let body = b"ready";
      let response = format!(
        "HTTP/1.1 200 OK\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
        body.len()
      );
      let _ = stream.write_all(response.as_bytes());
      let _ = stream.write_all(body);
    }
  });

  let fetcher = fast_retry_fetcher();
  let url = format!("http://{addr}/accepted");
  let res = fetcher
    .fetch(&url)
    .expect("fetch should retry 202 empty body");
  assert_eq!(res.bytes, b"ready");

  handle.join().unwrap();
}

#[test]
fn http_fetch_empty_body_error_mentions_attempts() {
  let Some(listener) = try_bind_localhost("http_fetch_empty_body_error_mentions_attempts") else {
    return;
  };
  let addr = listener.local_addr().unwrap();
  let handle = spawn_server(listener, 2, move |_count, _req, stream| {
    let response = "HTTP/1.1 202 Accepted\r\nContent-Length: 0\r\nConnection: close\r\n\r\n";
    let _ = stream.write_all(response.as_bytes());
  });

  let fetcher = fast_retry_fetcher();
  let url = format!("http://{addr}/accepted");
  let err = fetcher.fetch(&url).expect_err("fetch should fail after retries");
  assert!(
    err.to_string().contains("attempt 2/2"),
    "error should include attempt info: {err}"
  );

  handle.join().unwrap();
}

#[test]
fn http_fetch_retries_on_timeout() {
  let Some(listener) = try_bind_localhost("http_fetch_retries_on_timeout") else {
    return;
  };
  let addr = listener.local_addr().unwrap();
  let handle = spawn_server(listener, 2, move |count, _req, stream| match count {
    1 => {
      // Delay longer than the fetcher's per-attempt timeout to trigger a retry.
      thread::sleep(Duration::from_millis(250));
      let body = b"slow";
      let response = format!(
        "HTTP/1.1 200 OK\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
        body.len()
      );
      let _ = stream.write_all(response.as_bytes());
      let _ = stream.write_all(body);
    }
    _ => {
      let body = b"fast";
      let response = format!(
        "HTTP/1.1 200 OK\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
        body.len()
      );
      let _ = stream.write_all(response.as_bytes());
      let _ = stream.write_all(body);
    }
  });

  let fetcher = fast_retry_fetcher().with_timeout(Duration::from_millis(80));
  let url = format!("http://{addr}/timeout");
  let res = fetcher
    .fetch(&url)
    .expect("fetch should succeed after retrying timeout");
  assert_eq!(res.bytes, b"fast");

  handle.join().unwrap();
}
