use std::io::{Read, Write};
use std::net::TcpListener;
use std::thread;
use std::time::Duration;

use fastrender::api::RenderOptions;
use fastrender::error::Error;
use fastrender::render_control::{with_deadline, RenderDeadline};
use fastrender::resource::HttpFetcher;
use fastrender::ResourceFetcher;

struct EnvGuard(&'static str);

impl EnvGuard {
  fn set(key: &'static str, value: &str) -> Self {
    std::env::set_var(key, value);
    Self(key)
  }
}

impl Drop for EnvGuard {
  fn drop(&mut self) {
    std::env::remove_var(self.0);
  }
}

#[test]
fn compressed_resource_respects_render_timeout() {
  let _guard = EnvGuard::set("FASTR_TEST_RENDER_DELAY_MS", "10");
  let compressed = include_bytes!("fixtures/large_timeout_payload.gz");

  let listener = TcpListener::bind("127.0.0.1:0").expect("bind server");
  let addr = listener.local_addr().expect("local addr");
  let handle = thread::spawn(move || {
    if let Ok((mut stream, _)) = listener.accept() {
      let mut buf = [0u8; 1024];
      let _ = stream.read(&mut buf);
      let response = format!(
        "HTTP/1.1 200 OK\r\nContent-Length: {}\r\nContent-Encoding: gzip\r\nContent-Type: image/png\r\nConnection: close\r\n\r\n",
        compressed.len()
      );
      stream.write_all(response.as_bytes()).unwrap();
      stream.write_all(compressed).unwrap();
    }
  });

  let fetcher = HttpFetcher::new();
  let options = RenderOptions::default().with_timeout(Some(Duration::from_millis(40)));
  let deadline = RenderDeadline::new(options.timeout, None);
  let url = format!("http://{}/image.png", addr);

  let err = with_deadline(Some(&deadline), || fetcher.fetch(&url)).expect_err("expected timeout");

  match err {
    Error::Resource(res) => {
      assert!(
        res.message.to_ascii_lowercase().contains("decompress"),
        "unexpected resource error: {}",
        res.message
      );
    }
    other => panic!("expected resource timeout, got {other:?}"),
  }

  handle.join().unwrap();
}
