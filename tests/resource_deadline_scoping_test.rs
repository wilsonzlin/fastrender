use fastrender::render_control::{with_deadline, RenderDeadline};
use fastrender::resource::HttpFetcher;
use fastrender::ResourceFetcher;
mod test_support;
use std::io::{Read, Write};
use std::thread;
use std::time::Duration;
use test_support::net::try_bind_localhost;

#[test]
fn http_fetch_uses_root_deadline_under_nested_deadline() {
  let Some(listener) = try_bind_localhost("http_fetch_uses_root_deadline_under_nested_deadline")
  else {
    return;
  };

  let addr = listener.local_addr().expect("listener addr");
  let handle = thread::spawn(move || {
    let (mut stream, _) = listener.accept().expect("accept");
    // Best-effort consume the request bytes so the client doesn't hit an immediate ECONNRESET on
    // some platforms while we sleep.
    let mut buf = [0u8; 1024];
    let _ = stream.read(&mut buf);

    // Delay longer than the inner deadline but still well within the outer deadline.
    thread::sleep(Duration::from_millis(400));

    let body = b"ok";
    let response = format!(
      "HTTP/1.1 200 OK\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
      body.len()
    );
    let _ = stream.write_all(response.as_bytes());
    let _ = stream.write_all(body);
  });

  let fetcher = HttpFetcher::new();
  let url = format!("http://{addr}/");

  let outer_deadline = RenderDeadline::new(Some(Duration::from_secs(2)), None);
  let inner_deadline = RenderDeadline::new(Some(Duration::from_millis(200)), None);

  let res = with_deadline(Some(&outer_deadline), || {
    with_deadline(Some(&inner_deadline), || {
      let active = fastrender::render_control::active_deadline().expect("active deadline");
      let root = fastrender::render_control::root_deadline().expect("root deadline");
      assert_eq!(
        active.timeout_limit(),
        Some(Duration::from_millis(200)),
        "inner deadline should be active"
      );
      assert_eq!(
        root.timeout_limit(),
        Some(Duration::from_secs(2)),
        "outer deadline should remain the root deadline"
      );
      fetcher.fetch(&url)
    })
  })
  .expect("fetch should use outer deadline instead of nested budget");
  assert_eq!(res.bytes, b"ok");

  handle.join().expect("server thread");
}
