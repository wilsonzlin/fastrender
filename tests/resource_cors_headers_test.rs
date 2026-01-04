use fastrender::resource::HttpFetcher;
use fastrender::ResourceFetcher;
mod test_support;

use std::io;
use std::io::Read;
use std::io::Write;
use std::net::TcpListener;
use std::thread;
use std::time::{Duration, Instant};
use test_support::net::try_bind_localhost;

const MAX_WAIT: Duration = Duration::from_secs(3);

fn spawn_server(listener: TcpListener) -> thread::JoinHandle<()> {
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
              }
              Err(_) => break,
            }
          }

          let body = b"ok";
          let response = format!(
            "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nAcCeSs-CoNtRoL-AlLoW-OrIgIn:   *  \r\nTiMiNg-AlLoW-OrIgIn:  https://example.com  \r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
            body.len()
          );
          let _ = stream.write_all(response.as_bytes());
          let _ = stream.write_all(body);
          return;
        }
        Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => {
          thread::sleep(Duration::from_millis(5));
        }
        Err(_) => break,
      }
    }
  })
}

#[test]
fn http_fetcher_captures_cors_response_headers() {
  let Some(listener) = try_bind_localhost("http_fetcher_captures_cors_response_headers") else {
    return;
  };
  let addr = listener.local_addr().unwrap();
  let handle = spawn_server(listener);

  let fetcher = HttpFetcher::new().with_timeout(Duration::from_secs(2));
  let url = format!("http://{addr}/cors");
  let res = fetcher.fetch(&url).expect("fetch");
  handle.join().unwrap();

  assert_eq!(res.access_control_allow_origin.as_deref(), Some("*"));
  assert_eq!(res.timing_allow_origin.as_deref(), Some("https://example.com"));
}

