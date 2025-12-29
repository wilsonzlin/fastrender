use std::{
  io::{self, Read, Write},
  net::TcpListener,
  sync::mpsc,
  thread,
  time::Duration,
};

use criterion::{criterion_group, criterion_main, Criterion};
use fastrender::resource::HttpFetcher;

fn start_keep_alive_server(
  body: &'static [u8],
) -> (String, mpsc::Sender<()>, thread::JoinHandle<()>) {
  let listener = TcpListener::bind("127.0.0.1:0").expect("bind keep-alive server");
  let addr = listener.local_addr().unwrap();
  let (stop_tx, stop_rx) = mpsc::channel::<()>();

  let handle = thread::spawn(move || {
    listener.set_nonblocking(true).unwrap();
    while stop_rx.try_recv().is_err() {
      match listener.accept() {
        Ok((mut stream, _)) => {
          let _ = stream.set_read_timeout(Some(Duration::from_millis(500)));
          loop {
            let mut buf = [0u8; 1024];
            let mut req = Vec::new();
            loop {
              match stream.read(&mut buf) {
                Ok(0) => break,
                Ok(n) => {
                  req.extend_from_slice(&buf[..n]);
                  if req.windows(4).any(|w| w == b"\r\n\r\n") {
                    break;
                  }
                }
                Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => continue,
                Err(ref e) if e.kind() == io::ErrorKind::TimedOut => break,
                Err(_) => break,
              }
            }
            if req.is_empty() {
              break;
            }
            let response = format!(
              "HTTP/1.1 200 OK\r\nContent-Length: {}\r\nConnection: keep-alive\r\n\r\n",
              body.len()
            );
            let _ = stream.write_all(response.as_bytes());
            let _ = stream.write_all(body);
          }
        }
        Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => {
          thread::sleep(Duration::from_millis(1));
        }
        Err(_) => break,
      }
    }
  });

  (format!("http://{addr}/"), stop_tx, handle)
}

fn bench_agent_pool(c: &mut Criterion) {
  let body = b"ok";
  let (url, stop_tx, handle) = start_keep_alive_server(body);
  let fetcher = HttpFetcher::new().with_timeout(Duration::from_secs(2));

  let mut group = c.benchmark_group("http_agent_pool");
  group.bench_function("pooled_agent", |b| {
    b.iter(|| {
      let res = fetcher.fetch(&url).expect("pooled fetch");
      assert_eq!(res.bytes, body);
    });
  });

  group.bench_function("fresh_agent_per_request", |b| {
    b.iter(|| {
      let agent: ureq::Agent = ureq::Agent::config_builder()
        .http_status_as_error(false)
        .timeout_global(Some(Duration::from_secs(2)))
        .build()
        .into();

      let mut resp = agent.get(&url).call().expect("one-off fetch");
      assert_eq!(resp.status(), 200);
      let mut out = Vec::new();
      resp.body_mut().read_to_end(&mut out).unwrap();
      assert_eq!(out, body);
    });
  });

  group.finish();
  let _ = stop_tx.send(());
  handle.join().unwrap();
}

criterion_group!(benches, bench_agent_pool);
criterion_main!(benches);
