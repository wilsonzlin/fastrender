//! Fetch a single page and render it to an image.
//!
//! Examples:
//!   fetch_and_render --timeout 120 --viewport 1200x800 --dpr 2.0 https://www.example.com output.png

#![allow(clippy::io_other_error)]
#![allow(clippy::redundant_closure)]
#![allow(clippy::len_zero)]
#![allow(clippy::items_after_test_module)]

mod common;

use clap::Parser;
use common::args::{parse_viewport, MediaPreferenceArgs};
use common::media_prefs::MediaPreferences;
use fastrender::css::encoding::decode_css_bytes;
use fastrender::css::loader::absolutize_css_urls;
use fastrender::css::loader::extract_css_links;
use fastrender::css::loader::extract_embedded_css_urls;
use fastrender::css::loader::infer_base_url;
use fastrender::css::loader::inject_css_into_html;
use fastrender::css::loader::inline_imports;
use fastrender::css::loader::resolve_href;
use fastrender::html::encoding::decode_html_bytes;
use fastrender::html::meta_refresh::extract_js_location_redirect;
use fastrender::html::meta_refresh::extract_meta_refresh_url;
use fastrender::resource::normalize_user_agent_for_log;
use fastrender::resource::url_to_filename;
use fastrender::resource::HttpFetcher;
use fastrender::resource::ResourceFetcher;
use fastrender::resource::DEFAULT_ACCEPT_LANGUAGE;
use fastrender::resource::DEFAULT_USER_AGENT;
use fastrender::style::media::MediaType;
use fastrender::Error;
use fastrender::FastRender;
use fastrender::Result;
use std::collections::HashSet;
use std::sync::mpsc::channel;
use std::sync::mpsc::RecvTimeoutError;
use std::sync::Arc;
use std::thread;
use std::time::Duration;
use url::Url;

const STACK_SIZE: usize = 64 * 1024 * 1024; // 64MB to avoid stack overflows on large pages

/// Fetch a single page and render it to an image
#[derive(Parser, Debug)]
#[command(name = "fetch_and_render", version, about)]
struct Args {
  /// URL to fetch and render
  url: String,

  /// Output file path (defaults to <url>.png); parent directories are created automatically
  output: Option<String>,

  /// Viewport width (deprecated, use --viewport)
  #[arg(hide = true)]
  width: Option<u32>,

  /// Viewport height (deprecated, use --viewport)
  #[arg(hide = true)]
  height: Option<u32>,

  /// Scroll X offset (deprecated, use --scroll-x)
  #[arg(hide = true)]
  scroll_x_pos: Option<u32>,

  /// Scroll Y offset (deprecated, use --scroll-y)
  #[arg(hide = true)]
  scroll_y_pos: Option<u32>,

  /// Per-page timeout in seconds (0 = no timeout)
  #[arg(long, default_value = "0")]
  timeout: u64,

  /// Viewport size as WxH (e.g., 1200x800)
  #[arg(long, value_parser = parse_viewport, default_value = "1200x800")]
  viewport: (u32, u32),

  /// Horizontal scroll offset in CSS px
  #[arg(long, default_value = "0")]
  scroll_x: u32,

  /// Vertical scroll offset in CSS px
  #[arg(long, default_value = "0")]
  scroll_y: u32,

  /// Device pixel ratio for media queries/srcset
  #[arg(long, default_value = "1.0")]
  dpr: f32,

  #[command(flatten)]
  media_prefs: MediaPreferenceArgs,

  /// Expand render target to full content size
  #[arg(long)]
  full_page: bool,

  /// Override the User-Agent header
  #[arg(long, default_value = DEFAULT_USER_AGENT)]
  user_agent: String,

  /// Override the Accept-Language header
  #[arg(long, default_value = DEFAULT_ACCEPT_LANGUAGE)]
  accept_language: String,

  /// Maximum number of external stylesheets to fetch
  #[arg(long)]
  css_limit: Option<usize>,

  /// Enable per-stage timing logs
  #[arg(long)]
  timings: bool,
}

fn fetch_bytes(
  url: &str,
  timeout: Option<Duration>,
  user_agent: &str,
  accept_language: &str,
) -> Result<(Vec<u8>, Option<String>, Option<String>)> {
  // Handle file:// URLs
  if url.starts_with("file://") {
    let path = url.strip_prefix("file://").unwrap();
    let bytes = std::fs::read(path).map_err(Error::Io)?;

    let mut meta_path = std::path::PathBuf::from(path);
    if let Some(ext) = meta_path.extension().and_then(|e| e.to_str()) {
      meta_path.set_extension(format!("{ext}.meta"));
    } else {
      meta_path.set_extension("meta");
    }
    let meta = std::fs::read_to_string(&meta_path).ok();
    let (content_type, source_url) = meta
      .as_deref()
      .map(fastrender::resource::parse_cached_html_meta)
      .unwrap_or((None, None));

    return Ok((bytes, content_type, source_url));
  }

  // Configure agent with timeout for ureq 3.x
  let config = ureq::Agent::config_builder()
    .timeout_global(timeout)
    .build();
  let agent: ureq::Agent = config.into();

  let mut current = url.to_string();
  for _ in 0..10 {
    let mut response = agent
      .get(&current)
      .header("User-Agent", user_agent)
      .header("Accept-Language", accept_language)
      .call()
      .map_err(|e| {
        Error::Io(std::io::Error::new(
          std::io::ErrorKind::Other,
          e.to_string(),
        ))
      })?;

    let status = response.status();
    if (300..400).contains(&status.as_u16()) {
      if let Some(loc) = response
        .headers()
        .get("location")
        .and_then(|h| h.to_str().ok())
      {
        if let Ok(base) = Url::parse(&current) {
          if let Ok(next) = base.join(loc) {
            current = next.to_string();
            continue;
          }
        }
        current = loc.to_string();
        continue;
      }
    }

    let content_type = response
      .headers()
      .get("content-type")
      .and_then(|h| h.to_str().ok())
      .map(|s| s.to_string());

    let bytes = response.body_mut().read_to_vec().map_err(|e| {
      Error::Io(std::io::Error::new(
        std::io::ErrorKind::Other,
        e.to_string(),
      ))
    })?;

    if bytes.is_empty() {
      return Err(Error::Io(std::io::Error::new(
        std::io::ErrorKind::UnexpectedEof,
        "Empty HTTP response body",
      )));
    }

    return Ok((bytes, content_type, Some(current)));
  }

  Err(Error::Io(std::io::Error::new(
    std::io::ErrorKind::Other,
    "Too many redirects",
  )))
}

#[cfg(test)]
mod tests {
  use super::*;
  use fastrender::css::loader::resolve_href;

  fn try_bind_localhost(context: &str) -> Option<std::net::TcpListener> {
    match std::net::TcpListener::bind("127.0.0.1:0") {
      Ok(listener) => Some(listener),
      Err(err) if err.kind() == std::io::ErrorKind::PermissionDenied => {
        eprintln!("skipping {context}: cannot bind localhost in this environment: {err}");
        None
      }
      Err(err) => panic!("bind {context}: {err}"),
    }
  }

  fn accept_request(
    listener: &std::net::TcpListener,
    timeout: std::time::Duration,
  ) -> Option<std::net::TcpStream> {
    listener
      .set_nonblocking(true)
      .expect("set listener nonblocking");
    let start = std::time::Instant::now();
    loop {
      match listener.accept() {
        Ok((stream, _)) => {
          let _ = stream.set_nonblocking(false);
          return Some(stream);
        }
        Err(ref e) if e.kind() == std::io::ErrorKind::WouldBlock => {
          if start.elapsed() >= timeout {
            return None;
          }
          std::thread::sleep(std::time::Duration::from_millis(10));
        }
        Err(e) => panic!("accept failed: {e}"),
      }
    }
  }

  #[test]
  fn resolves_relative_http_links() {
    let base = "https://example.com/a/b/page.html";
    let href = "../styles/site.css";
    let resolved = resolve_href(base, href).expect("resolved");
    assert_eq!(resolved, "https://example.com/a/styles/site.css");
  }

  #[test]
  fn resolves_protocol_relative_links() {
    let base = "https://example.com/index.html";
    let href = "//cdn.example.com/main.css";
    let resolved = resolve_href(base, href).expect("resolved");
    assert_eq!(resolved, "https://cdn.example.com/main.css");
  }

  #[test]
  fn resolves_file_scheme_links() {
    let dir = tempfile::tempdir().expect("temp dir");
    let base = format!("file://{}", dir.path().join("page.html").display());
    let href = "css/app.css";
    let resolved = resolve_href(&base, href).expect("resolved");
    assert!(resolved.ends_with("/css/app.css"), "resolved={}", resolved);
  }

  #[test]
  fn absolutizes_css_urls_against_stylesheet() {
    let css = r#"
            body { background: url("../img/bg.png") no-repeat; }
            @font-face { src: url('fonts/font.woff2'); }
        "#;
    let base = "https://example.com/assets/css/main.css";
    let out = absolutize_css_urls(css, base);
    assert!(out.contains("url(\"https://example.com/assets/img/bg.png\")"));
    assert!(out.contains("url(\"https://example.com/assets/css/fonts/font.woff2\")"));
  }

  #[test]
  fn inlines_imports_with_media_wrapping() {
    let mut fake_fetch = |url: &str| -> Result<String> {
      match url {
        "https://example.com/css/imported.css" => Ok("h1 { color: red; }".to_string()),
        _ => Err(Error::Io(std::io::Error::new(
          std::io::ErrorKind::NotFound,
          "not found",
        ))),
      }
    };
    let css = r#"@import url("https://example.com/css/imported.css") screen;"#;
    let mut seen = HashSet::new();
    let inlined = inline_imports(
      css,
      "https://example.com/css/main.css",
      &mut fake_fetch,
      &mut seen,
    );
    assert!(
      inlined.contains("@media screen"),
      "expected media wrapper: {}",
      inlined
    );
    assert!(inlined.contains("color: red"));
  }

  #[test]
  fn extracts_stylesheet_hrefs_with_resolution() {
    let html = r#"
            <html><head>
                <link rel="stylesheet" href="../styles/a.css">
                <link rel="icon" href="/favicon.ico">
            </head><body></body></html>
        "#;
    let urls = extract_css_links(
      html,
      "https://example.com/site/page.html",
      MediaType::Screen,
    );
    assert_eq!(urls, vec!["https://example.com/styles/a.css".to_string()]);
  }

  #[test]
  fn decode_html_uses_content_type_charset() {
    let encoded = encoding_rs::SHIFT_JIS.encode("abcデ").0;
    let decoded = decode_html_bytes(&encoded, Some("text/html; charset=shift_jis"));
    assert!(
      decoded.contains('デ'),
      "decoded text should include kana: {}",
      decoded
    );
  }

  #[test]
  fn decode_html_uses_meta_charset() {
    let mut encoded = encoding_rs::WINDOWS_1252
      .encode("<html><head><meta charset=\"windows-1252\"></head><body>\u{00a3}</body></html>")
      .0;
    let decoded = decode_html_bytes(&encoded, None);
    assert!(
      decoded.contains('£'),
      "decoded text should contain pound sign when meta declares charset: {}",
      decoded
    );

    encoded = encoding_rs::SHIFT_JIS
      .encode("<html><head><meta charset='shift_jis'></head><body>デ</body></html>")
      .0;
    let decoded = decode_html_bytes(&encoded, None);
    assert!(
      decoded.contains('デ'),
      "decoded text should contain kana when meta declares charset: {}",
      decoded
    );
  }

  #[test]
  fn decode_html_defaults_to_windows_1252() {
    let bytes = vec![0xa3]; // U+00A3 in Windows-1252
    let decoded = decode_html_bytes(&bytes, None);
    assert_eq!(decoded, "£");
  }

  #[test]
  fn fetch_bytes_reads_local_meta_content_type() {
    let dir = tempfile::tempdir().expect("temp dir");
    let html_path = dir.path().join("page.html");
    let meta_path = dir.path().join("page.html.meta");

    // Write Shift-JIS encoded body and matching meta
    let encoded = encoding_rs::SHIFT_JIS
      .encode("<html><body>デ</body></html>")
      .0
      .to_vec();
    std::fs::write(&html_path, &encoded).unwrap();
    std::fs::write(&meta_path, "text/html; charset=shift_jis").unwrap();

    let url = format!("file://{}", html_path.display());
    let (bytes, ct, base_url) =
      fetch_bytes(&url, None, DEFAULT_USER_AGENT, DEFAULT_ACCEPT_LANGUAGE).expect("fetch bytes");
    assert_eq!(ct.as_deref(), Some("text/html; charset=shift_jis"));
    assert!(base_url.is_none(), "legacy meta should not include a url");
    let decoded = decode_html_bytes(&bytes, ct.as_deref());
    assert!(
      decoded.contains('デ'),
      "decoded html should respect meta charset: {}",
      decoded
    );
  }

  #[test]
  fn fetch_bytes_uses_base_url_from_meta() {
    let dir = tempfile::tempdir().expect("temp dir");
    let html_path = dir.path().join("page.html");
    let meta_path = dir.path().join("page.html.meta");

    std::fs::write(&html_path, "<html><body>Hi</body></html>").unwrap();
    std::fs::write(
      &meta_path,
      "content-type: text/html\nurl: https://example.com/\n",
    )
    .unwrap();

    let url = format!("file://{}", html_path.display());
    let (_bytes, ct, base_url) =
      fetch_bytes(&url, None, DEFAULT_USER_AGENT, DEFAULT_ACCEPT_LANGUAGE).expect("fetch bytes");
    assert_eq!(ct.as_deref(), Some("text/html"));
    assert_eq!(base_url.as_deref(), Some("https://example.com/"));
  }

  #[test]
  fn fetch_bytes_sends_accept_language_header() {
    use std::io::BufRead;
    use std::io::BufReader;
    use std::io::Write;
    use std::thread;
    use std::time::Duration;

    let Some(listener) = try_bind_localhost("fetch_bytes_sends_accept_language_header") else {
      return;
    };
    let addr = listener.local_addr().unwrap();

    let handle = thread::spawn(move || {
      let (mut stream, _) = listener.accept().expect("accept request");
      let mut reader = BufReader::new(stream.try_clone().expect("clone stream"));
      let mut buf = String::new();
      // Read request headers until the blank line.
      while reader.read_line(&mut buf).map(|n| n > 0).unwrap_or(false) {
        if buf.ends_with("\r\n\r\n") || buf == "\r\n" {
          break;
        }
      }
      let response = b"HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 2\r\n\r\nOK";
      let _ = stream.write_all(response);
      buf
    });

    let url = format!("http://{}/", addr);
    let _ = fetch_bytes(
      &url,
      Some(Duration::from_secs(5)),
      DEFAULT_USER_AGENT,
      DEFAULT_ACCEPT_LANGUAGE,
    )
    .expect("fetch bytes");

    let request = handle.join().expect("request captured");
    assert!(
      request
        .to_ascii_lowercase()
        .contains("accept-language: en-us,en;q=0.9"),
      "missing Accept-Language in request: {request}"
    );
  }

  #[test]
  fn fetch_bytes_errors_on_empty_http_body() {
    use std::io::BufRead;
    use std::io::BufReader;
    use std::io::Write;
    use std::thread;
    use std::time::Duration;

    let Some(listener) = try_bind_localhost("fetch_bytes_errors_on_empty_http_body") else {
      return;
    };
    let addr = listener.local_addr().unwrap();

    let handle = thread::spawn(move || {
      let (mut stream, _) = listener.accept().expect("accept request");
      let mut reader = BufReader::new(stream.try_clone().expect("clone stream"));
      let mut buf = String::new();
      while reader.read_line(&mut buf).map(|n| n > 0).unwrap_or(false) {
        if buf.ends_with("\r\n\r\n") || buf == "\r\n" {
          break;
        }
      }
      let response = b"HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nContent-Length: 0\r\n\r\n";
      let _ = stream.write_all(response);
    });

    let url = format!("http://{}/", addr);
    let res = fetch_bytes(
      &url,
      Some(Duration::from_secs(5)),
      DEFAULT_USER_AGENT,
      DEFAULT_ACCEPT_LANGUAGE,
    );
    assert!(res.is_err(), "expected empty body error, got {res:?}");

    handle.join().expect("server thread");
  }

  #[test]
  fn render_once_follows_quoted_meta_refresh() {
    use std::io::BufRead;
    use std::io::BufReader;
    use std::io::Write;
    use std::sync::Arc;
    use std::sync::Mutex;
    use std::thread;
    use std::time::Duration;

    let Some(listener) = try_bind_localhost("render_once_follows_quoted_meta_refresh") else {
      return;
    };
    let addr = listener.local_addr().unwrap();
    let requests: Arc<Mutex<Vec<String>>> = Arc::new(Mutex::new(Vec::new()));
    let requests_clone = Arc::clone(&requests);

    let handle = thread::spawn(move || {
      if let Some(mut stream) = accept_request(&listener, Duration::from_secs(2)) {
        let mut reader = BufReader::new(stream.try_clone().expect("clone stream"));
        let mut buf = String::new();
        while reader.read_line(&mut buf).map(|n| n > 0).unwrap_or(false) {
          if buf.ends_with("\r\n\r\n") || buf == "\r\n" {
            break;
          }
        }
        requests_clone.lock().unwrap().push(buf.clone());
        let html = format!(
                    "<html><head><meta http-equiv=\"refresh\" content=\"0;URL='http://{}/next?foo=1&amp;bar=2'\"></head><body>Redirecting</body></html>",
                    addr
                );
        let response = format!(
          "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nContent-Length: {}\r\n\r\n{}",
          html.len(),
          html
        );
        let _ = stream.write_all(response.as_bytes());
      }

      if let Some(mut stream) = accept_request(&listener, Duration::from_secs(2)) {
        let mut reader = BufReader::new(stream.try_clone().expect("clone stream"));
        let mut buf = String::new();
        while reader.read_line(&mut buf).map(|n| n > 0).unwrap_or(false) {
          if buf.ends_with("\r\n\r\n") || buf == "\r\n" {
            break;
          }
        }
        requests_clone.lock().unwrap().push(buf.clone());
        let html = "<html><body>OK</body></html>";
        let response = format!(
          "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nContent-Length: {}\r\n\r\n{}",
          html.len(),
          html
        );
        let _ = stream.write_all(response.as_bytes());
      }
    });

    let url = format!("http://{}/", addr);
    let tmp = tempfile::tempdir().unwrap();
    let output = tmp.path().join("out.png");
    render_once(
      &url,
      output.to_str().unwrap(),
      200,
      200,
      0,
      0,
      1.0,
      Some(5),
      "TestAgent/1.0",
      "en-US,en;q=0.9",
      None,
    )
    .expect("render_once should follow refresh");

    handle.join().expect("server thread");
    let captured = requests.lock().unwrap().join("\n").to_ascii_lowercase();
    assert!(
      captured.contains("get / "),
      "first request should hit root: {}",
      captured
    );
    assert!(
      captured.contains("get /next?foo=1&bar=2"),
      "should follow quoted meta refresh URL: {}",
      captured
    );
    assert!(output.exists(), "output image should be written");
  }

  #[test]
  fn render_once_follows_meta_refresh_redirect() {
    use std::io::BufRead;
    use std::io::BufReader;
    use std::io::Write;
    use std::sync::Arc;
    use std::sync::Mutex;
    use std::thread;
    use std::time::Duration;

    let Some(listener) = try_bind_localhost("render_once_follows_meta_refresh_redirect") else {
      return;
    };
    let addr = listener.local_addr().unwrap();
    let requests: Arc<Mutex<Vec<String>>> = Arc::new(Mutex::new(Vec::new()));
    let requests_clone = Arc::clone(&requests);

    let handle = thread::spawn(move || {
      if let Some(mut stream) = accept_request(&listener, Duration::from_secs(2)) {
        let mut reader = BufReader::new(stream.try_clone().expect("clone stream"));
        let mut buf = String::new();
        while reader.read_line(&mut buf).map(|n| n > 0).unwrap_or(false) {
          if buf.ends_with("\r\n\r\n") || buf == "\r\n" {
            break;
          }
        }
        requests_clone.lock().unwrap().push(buf.clone());
        let html = format!(
                    "<!doctype html><html><head><noscript><meta http-equiv=\"refresh\" content=\"0; url=&quot;http://{}/next&quot;\"><style>body {{ display: none; }}</style></noscript></head><body>old</body></html>",
                    addr
                );
        let response = format!(
          "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nContent-Length: {}\r\n\r\n{}",
          html.len(),
          html
        );
        let _ = stream.write_all(response.as_bytes());
      }

      if let Some(mut stream) = accept_request(&listener, Duration::from_secs(2)) {
        let mut reader = BufReader::new(stream.try_clone().expect("clone stream"));
        let mut buf = String::new();
        while reader.read_line(&mut buf).map(|n| n > 0).unwrap_or(false) {
          if buf.ends_with("\r\n\r\n") || buf == "\r\n" {
            break;
          }
        }
        requests_clone.lock().unwrap().push(buf.clone());
        let html =
                    "<html><body><div style=\"width:100px;height:40px;background:rgb(0, 200, 0);\"></div></body></html>";
        let response = format!(
          "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nContent-Length: {}\r\n\r\n{}",
          html.len(),
          html
        );
        let _ = stream.write_all(response.as_bytes());
      }
    });

    let url = format!("http://{}/", addr);
    let tmp = tempfile::tempdir().unwrap();
    let output = tmp.path().join("refresh.png");
    render_once(
      &url,
      output.to_str().unwrap(),
      200,
      150,
      0,
      0,
      1.0,
      Some(5),
      "TestAgent/1.0",
      DEFAULT_ACCEPT_LANGUAGE,
      None,
    )
    .expect("render_once should succeed");

    handle.join().expect("server thread");
    let requests = requests.lock().unwrap();
    assert!(
      requests.iter().any(|r| r.contains("GET /next")),
      "expected follow-up /next request, got:\n{}",
      requests.join("\n---\n")
    );

    let image = image::ImageReader::open(&output)
      .expect("open output")
      .decode()
      .expect("decode png")
      .to_rgba8();
    let has_non_white = image
      .pixels()
      .any(|p| p.0[0] != 255 || p.0[1] != 255 || p.0[2] != 255);
    assert!(
      has_non_white,
      "render should include content after meta refresh"
    );
  }

  #[test]
  fn render_once_follows_js_location_redirect() {
    use std::io::BufRead;
    use std::io::BufReader;
    use std::io::Write;
    use std::sync::Arc;
    use std::sync::Mutex;
    use std::thread;
    use std::time::Duration;

    let Some(listener) = try_bind_localhost("render_once_follows_js_location_redirect") else {
      return;
    };
    let addr = listener.local_addr().unwrap();
    let requests: Arc<Mutex<Vec<String>>> = Arc::new(Mutex::new(Vec::new()));
    let requests_clone = Arc::clone(&requests);

    let handle = thread::spawn(move || {
      if let Some(mut stream) = accept_request(&listener, Duration::from_secs(2)) {
        let mut reader = BufReader::new(stream.try_clone().expect("clone stream"));
        let mut buf = String::new();
        while reader.read_line(&mut buf).map(|n| n > 0).unwrap_or(false) {
          if buf.ends_with("\r\n\r\n") || buf == "\r\n" {
            break;
          }
        }
        requests_clone.lock().unwrap().push(buf.clone());
        let html = format!(
                    "<!doctype html><html><head><style>body {{ display: none; }}</style><script>window.location.href = \"http://{}/target\";</script></head><body>old</body></html>",
                    addr
                );
        let response = format!(
          "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nContent-Length: {}\r\n\r\n{}",
          html.len(),
          html
        );
        let _ = stream.write_all(response.as_bytes());
      }

      if let Some(mut stream) = accept_request(&listener, Duration::from_secs(2)) {
        let mut reader = BufReader::new(stream.try_clone().expect("clone stream"));
        let mut buf = String::new();
        while reader.read_line(&mut buf).map(|n| n > 0).unwrap_or(false) {
          if buf.ends_with("\r\n\r\n") || buf == "\r\n" {
            break;
          }
        }
        requests_clone.lock().unwrap().push(buf.clone());
        let html =
                    "<html><body><div style=\"width:120px;height:50px;background:rgb(200, 0, 0);\"></div></body></html>";
        let response = format!(
          "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nContent-Length: {}\r\n\r\n{}",
          html.len(),
          html
        );
        let _ = stream.write_all(response.as_bytes());
      }
    });

    let url = format!("http://{}/", addr);
    let tmp = tempfile::tempdir().unwrap();
    let output = tmp.path().join("js-redirect.png");
    render_once(
      &url,
      output.to_str().unwrap(),
      220,
      160,
      0,
      0,
      1.0,
      Some(5),
      "TestAgent/1.0",
      DEFAULT_ACCEPT_LANGUAGE,
      None,
    )
    .expect("render_once should succeed");

    handle.join().expect("server thread");
    let requests = requests.lock().unwrap();
    assert!(
      requests.iter().any(|r| r.contains("GET /target")),
      "expected follow-up /target request, got:\n{}",
      requests.join("\n---\n")
    );

    let image = image::ImageReader::open(&output)
      .expect("open output")
      .decode()
      .expect("decode png")
      .to_rgba8();
    let has_non_white = image
      .pixels()
      .any(|p| p.0[0] != 255 || p.0[1] != 255 || p.0[2] != 255);
    assert!(
      has_non_white,
      "render should include content after JS redirect"
    );
  }

  #[test]
  fn render_once_fetches_assets_with_cli_headers() {
    use std::io::BufRead;
    use std::io::BufReader;
    use std::io::Write;
    use std::thread;

    const PNG_BYTES: &[u8] = b"\x89PNG\r\n\x1a\n\x00\x00\x00\rIHDR\x00\x00\x00\x01\x00\x00\x00\x01\x08\x02\x00\x00\x00\x90wS\xde\x00\x00\x00\nIDATx\x9cc```\x00\x00\x00\x04\x00\x01\r\n-\xb4\x00\x00\x00\x00IEND\xaeB`\x82";

    let Some(listener) = try_bind_localhost("render_once_fetches_assets_with_cli_headers") else {
      return;
    };
    let addr = listener.local_addr().unwrap();
    let requests: std::sync::Arc<std::sync::Mutex<Vec<String>>> =
      std::sync::Arc::new(std::sync::Mutex::new(Vec::new()));
    let requests_clone = std::sync::Arc::clone(&requests);

    let handle = thread::spawn(move || {
      for i in 0..2 {
        let (mut stream, _) = listener.accept().expect("accept request");
        let mut reader = BufReader::new(stream.try_clone().expect("clone stream"));
        let mut buf = String::new();
        while reader.read_line(&mut buf).map(|n| n > 0).unwrap_or(false) {
          if buf.ends_with("\r\n\r\n") || buf == "\r\n" {
            break;
          }
        }
        requests_clone.lock().unwrap().push(buf.clone());
        if i == 0 {
          let html = format!(
            "<html><body><img src=\"http://{}/img.png\"></body></html>",
            addr
          );
          let response = format!(
            "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nContent-Length: {}\r\n\r\n{}",
            html.len(),
            html
          );
          let _ = stream.write_all(response.as_bytes());
        } else {
          let response = format!(
            "HTTP/1.1 200 OK\r\nContent-Type: image/png\r\nContent-Length: {}\r\n\r\n",
            PNG_BYTES.len()
          );
          let _ = stream.write_all(response.as_bytes());
          let _ = stream.write_all(PNG_BYTES);
        }
      }
    });

    let url = format!("http://{}/", addr);
    let tmp = tempfile::tempdir().unwrap();
    let output = tmp.path().join("out.png");
    render_once(
      &url,
      output.to_str().unwrap(),
      200,
      200,
      0,
      0,
      1.0,
      Some(5),
      "TestAgent/1.0",
      "fr-CA,fr;q=0.7",
      None,
    )
    .expect("render_once should succeed");

    handle.join().expect("server thread");
    let combined = requests.lock().unwrap().join("\n").to_ascii_lowercase();
    assert!(
      combined.contains("accept-language: fr-ca,fr;q=0.7"),
      "asset requests should include Accept-Language: {}",
      combined
    );
    assert!(
      combined.contains("user-agent: testagent/1.0"),
      "asset requests should include User-Agent: {}",
      combined
    );
  }
}

#[allow(clippy::too_many_arguments)]
fn render_once(
  url: &str,
  output: &str,
  width: u32,
  height: u32,
  scroll_x: u32,
  scroll_y: u32,
  dpr: f32,
  timeout_secs: Option<u64>,
  user_agent: &str,
  accept_language: &str,
  css_limit: Option<usize>,
) -> Result<()> {
  println!("Fetching HTML from: {}", url);
  let timeout = timeout_secs.map(Duration::from_secs);
  let (html_bytes, html_content_type, source_url) =
    fetch_bytes(url, timeout, user_agent, accept_language)?;
  let mut html = decode_html_bytes(&html_bytes, html_content_type.as_deref());
  let mut base_hint = source_url.as_deref().unwrap_or(url).to_string();
  let mut resource_base = infer_base_url(&html, &base_hint).into_owned();

  if let Some(refresh) = extract_meta_refresh_url(&html) {
    if let Some(target) = resolve_href(&resource_base, &refresh) {
      println!("Following meta refresh to: {}", target);
      match fetch_bytes(&target, timeout, user_agent, accept_language) {
        Ok((bytes, content_type, final_url)) => {
          html = decode_html_bytes(&bytes, content_type.as_deref());
          base_hint = final_url.unwrap_or(target);
          resource_base = infer_base_url(&html, &base_hint).into_owned();
        }
        Err(e) => {
          eprintln!("Warning: failed to follow meta refresh {}: {}", target, e);
        }
      }
    }
  }

  if let Some(js_redirect) = extract_js_location_redirect(&html) {
    // Avoid following absurdly long inline redirects that will almost certainly 414 or pollute cache.
    if js_redirect.len() <= 2048 {
      if let Some(target) = resolve_href(&resource_base, &js_redirect) {
        println!("Following JS location redirect to: {}", target);
        match fetch_bytes(&target, timeout, user_agent, accept_language) {
          Ok((bytes, content_type, final_url)) => {
            html = decode_html_bytes(&bytes, content_type.as_deref());
            base_hint = final_url.unwrap_or(target);
            resource_base = infer_base_url(&html, &base_hint).into_owned();
          }
          Err(e) => {
            eprintln!("Warning: failed to follow JS redirect {}: {}", target, e);
          }
        }
      }
    } else {
      eprintln!(
        "Warning: skipping JS redirect of length {}",
        js_redirect.len()
      );
    }
  }

  println!("Extracting CSS links...");
  let mut css_links = extract_css_links(&html, &resource_base, MediaType::Screen);
  if let Some(limit) = css_limit {
    if css_links.len() > limit {
      css_links.truncate(limit);
    }
  }

  // Fallback: also scan inline content for CSS URLs that are not linked.
  let mut seen = HashSet::new();
  for link in &css_links {
    seen.insert(link.clone());
  }
  for extra in extract_embedded_css_urls(&html, &resource_base) {
    if seen.insert(extra.clone()) {
      css_links.push(extra);
    }
  }

  println!("Found {} CSS link(s)", css_links.len());

  let mut combined_css = String::new();
  let mut seen_imports = HashSet::new();
  for css_url in css_links {
    println!("Fetching CSS from: {}", css_url);
    seen_imports.insert(css_url.clone());
    match fetch_bytes(&css_url, timeout, user_agent, accept_language) {
      Ok((bytes, content_type, _)) => {
        let css_text = decode_css_bytes(&bytes, content_type.as_deref());
        let rewritten = absolutize_css_urls(&css_text, &css_url);
        let mut import_fetch = |u| {
          fetch_bytes(u, timeout, user_agent, accept_language)
            .map(|(b, ct, _)| decode_css_bytes(&b, ct.as_deref()))
        };
        let inlined = inline_imports(&rewritten, &css_url, &mut import_fetch, &mut seen_imports);
        combined_css.push_str(&inlined);
        combined_css.push('\n');
      }
      Err(e) => {
        eprintln!("Warning: Failed to fetch CSS from {}: {}", css_url, e);
      }
    }
  }

  println!("Injecting CSS into HTML...");
  let html_with_css = if !combined_css.is_empty() {
    inject_css_into_html(&html, &combined_css)
  } else {
    html
  };

  println!(
    "Rendering to image ({}x{} viewport, scroll_x={}, scroll_y={})...",
    width, height, scroll_x, scroll_y
  );
  let mut http_fetcher = HttpFetcher::new()
    .with_user_agent(user_agent.to_string())
    .with_accept_language(accept_language.to_string());
  if let Some(secs) = timeout_secs {
    http_fetcher = http_fetcher.with_timeout(Duration::from_secs(secs));
  }

  let mut renderer = FastRender::builder()
    .device_pixel_ratio(dpr)
    .apply_meta_viewport(true)
    .fetcher(Arc::new(http_fetcher) as Arc<dyn ResourceFetcher>)
    .build()?;
  renderer.set_base_url(resource_base.clone());
  let png_data = renderer.render_to_png_with_scroll(
    &html_with_css,
    width,
    height,
    scroll_x as f32,
    scroll_y as f32,
  )?;

  println!("Saving to {}...", output);
  std::fs::write(output, png_data)?;

  println!("✓ Successfully rendered {} to {}", url, output);
  println!("  Image size: {} bytes", std::fs::metadata(output)?.len());
  Ok(())
}

fn main() -> Result<()> {
  let args = Args::parse();
  let media_prefs = MediaPreferences::from(&args.media_prefs);

  // Resolve dimensions from viewport or deprecated positional args
  let (width, height) = args.viewport;
  let width = args.width.unwrap_or(width);
  let height = args.height.unwrap_or(height);
  let scroll_x = args.scroll_x_pos.unwrap_or(args.scroll_x);
  let scroll_y = args.scroll_y_pos.unwrap_or(args.scroll_y);

  let output = args
    .output
    .unwrap_or_else(|| format!("{}.png", url_to_filename(&args.url)));

  let output_path = std::path::Path::new(&output);
  if let Some(parent) = output_path.parent() {
    if !parent.as_os_str().is_empty() {
      std::fs::create_dir_all(parent)?;
    }
  }

  let timeout_secs = if args.timeout == 0 {
    None
  } else {
    Some(args.timeout)
  };

  media_prefs.apply_env();

  if args.full_page {
    std::env::set_var("FASTR_FULL_PAGE", "1");
  }

  if args.timings {
    std::env::set_var("FASTR_RENDER_TIMINGS", "1");
  }

  eprintln!(
    "User-Agent: {}\nAccept-Language: {}\nViewport: {}x{} @{}x, scroll ({}, {})\nOutput: {}",
    normalize_user_agent_for_log(&args.user_agent),
    args.accept_language,
    width,
    height,
    args.dpr,
    scroll_x,
    scroll_y,
    output
  );

  let (tx, rx) = channel();
  let url_clone = args.url.clone();
  let output_clone = output.clone();
  let user_agent = args.user_agent.clone();
  let accept_language = args.accept_language.clone();
  let css_limit = args.css_limit;
  let dpr = args.dpr;

  thread::Builder::new()
    .name("fetch_and_render-worker".to_string())
    .stack_size(STACK_SIZE)
    .spawn(move || {
      let _ = tx.send(render_once(
        &url_clone,
        &output_clone,
        width,
        height,
        scroll_x,
        scroll_y,
        dpr,
        timeout_secs,
        &user_agent,
        &accept_language,
        css_limit,
      ));
    })
    .expect("spawn render worker");

  if let Some(secs) = timeout_secs {
    match rx.recv_timeout(Duration::from_secs(secs)) {
      Ok(res) => res,
      Err(RecvTimeoutError::Timeout) => {
        eprintln!("Render timed out after {secs} seconds");
        std::process::exit(1);
      }
      Err(RecvTimeoutError::Disconnected) => {
        eprintln!("Render worker exited unexpectedly");
        std::process::exit(1);
      }
    }
  } else {
    match rx.recv() {
      Ok(res) => res,
      Err(_) => {
        eprintln!("Render worker exited unexpectedly");
        std::process::exit(1);
      }
    }
  }
}
