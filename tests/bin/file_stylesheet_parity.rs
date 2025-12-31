use crate::test_support::net::try_bind_localhost;
use std::io::Read;
use std::io::Write;
use std::process::Command;
use std::thread;

use crate::test_support::net::try_bind_localhost;

#[test]
fn file_and_url_paths_render_the_same_with_external_css() {
  let tmp = tempfile::tempdir().expect("temp dir");
  let html_path = tmp.path().join("page.html");
  let css_path = tmp.path().join("style.css");

  let html = r#"<!doctype html><html><head><link rel="stylesheet" href="style.css"></head><body><div class="box"></div></body></html>"#;
  let css = r#"body { margin: 0; }
.box { width: 32px; height: 24px; background: rgb(10, 180, 20); }"#;

  std::fs::write(&html_path, html).expect("write html");
  std::fs::write(&css_path, css).expect("write css");

  let Some(listener) = try_bind_localhost("file_and_url_paths_render_the_same_with_external_css")
  else {
    return;
  };
  let addr = listener.local_addr().expect("addr");
  let html_body = html.to_string();
  let css_body = css.to_string();

  thread::spawn(move || {
    for _ in 0..2 {
      let (mut stream, _) = listener.accept().expect("accept request");
      let mut buf = [0u8; 512];
      let n = stream.read(&mut buf).expect("read request");
      let req = String::from_utf8_lossy(&buf[..n]);
      let path = if req.starts_with("GET /style.css") {
        "/style.css"
      } else {
        "/page.html"
      };

      let (body, content_type) = if path == "/style.css" {
        (css_body.clone(), "text/css")
      } else {
        (html_body.clone(), "text/html")
      };

      let response = format!(
        "HTTP/1.1 200 OK\r\nContent-Type: {}\r\nContent-Length: {}\r\n\r\n{}",
        content_type,
        body.len(),
        body
      );
      stream
        .write_all(response.as_bytes())
        .expect("write response");
    }
  });

  let http_output = tmp.path().join("url.png");
  let file_output = tmp.path().join("file.png");

  let status = Command::new(env!("CARGO_BIN_EXE_fetch_and_render"))
    .args(["--viewport", "40x30"])
    .arg(format!("http://{}/page.html", addr))
    .arg(&http_output)
    .status()
    .expect("run fetch_and_render on http");
  assert!(status.success(), "http render should succeed");

  let file_url = format!("file://{}", html_path.display());
  let status = Command::new(env!("CARGO_BIN_EXE_fetch_and_render"))
    .args(["--viewport", "40x30"])
    .arg(&file_url)
    .arg(&file_output)
    .status()
    .expect("run fetch_and_render on file");
  assert!(status.success(), "file render should succeed");

  let url_png = std::fs::read(&http_output).expect("read url png");
  let file_png = std::fs::read(&file_output).expect("read file png");
  let url_img = image::load_from_memory(&url_png)
    .expect("decode url png")
    .to_rgba8();
  let file_img = image::load_from_memory(&file_png)
    .expect("decode file png")
    .to_rgba8();

  assert_eq!(url_img.dimensions(), file_img.dimensions());
  assert_eq!(url_img.as_raw(), file_img.as_raw());
}
