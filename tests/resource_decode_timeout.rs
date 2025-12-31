use std::io::{Read, Write};
use std::thread;
use std::time::Duration;

use fastrender::api::{FastRender, RenderOptions};
use fastrender::error::{Error, RenderError, RenderStage};
use fastrender::render_control::{with_deadline, RenderDeadline};
use fastrender::resource::HttpFetcher;
use fastrender::ResourceFetcher;

mod test_support;
use test_support::net::try_bind_localhost;

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

  let Some(listener) = try_bind_localhost("compressed_resource_respects_render_timeout") else {
    return;
  };
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
#[test]
fn renderer_times_out_while_decompressing_image() {
  let _guard = EnvGuard::set("FASTR_TEST_RENDER_DELAY_MS", "5");
  let compressed = include_bytes!("fixtures/large_timeout_payload.gz");

  let Some(listener) = try_bind_localhost("renderer_times_out_while_decompressing_image") else {
    return;
  };
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

  let mut renderer = FastRender::new().expect("renderer");
  let options = RenderOptions::default()
    .with_viewport(16, 16)
    .with_timeout(Some(Duration::from_millis(20)));
  let url = format!("http://{}/image.png", addr);
  let html = format!("<img src=\"{}\" />", url);

  let err = renderer
    .render_html_with_options(&html, options)
    .expect_err("expected render timeout");

  match err {
    Error::Resource(res) => {
      assert!(
        res.message.to_ascii_lowercase().contains("decompress"),
        "unexpected resource error: {}",
        res.message
      );
    }
    Error::Render(RenderError::Timeout { stage, .. }) => {
      assert!(
        matches!(stage, RenderStage::Layout | RenderStage::Paint),
        "expected timeout in layout/paint, got {stage:?}"
      );
    }
    other => panic!("expected timeout error, got {other:?}"),
  }

  handle.join().unwrap();
}
