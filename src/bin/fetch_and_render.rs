//! Fetch a single page and render it to an image.
//!
//! Usage: fetch_and_render [--timeout SECONDS] [--viewport WxH] [--scroll-x PX] [--scroll-y PX] [--dpr FLOAT] [--prefers-reduced-transparency <value>] [--prefers-reduced-motion <value>] [--prefers-reduced-data <value>] [--prefers-contrast <value>] [--prefers-color-scheme <value>] [--full-page] [--user-agent UA] [--accept-language LANG] [--css-limit N] [--timings] <url> [output.png] [width] [height] [scroll_x] [scroll_y]
//!
//! Examples:
//!   fetch_and_render --timeout 120 --viewport 1200x800 --dpr 2.0 https://www.example.com output.png 1200 800 0 0
//!
//! Options:
//!   --timeout SECONDS   Per-page timeout (default: 0 = no timeout)
//!   --viewport WxH      Override the viewport size (e.g., 1366x768; default 1200x800)
//!   --scroll-x PX       Horizontal scroll offset in CSS px (default: 0)
//!   --scroll-y PX       Vertical scroll offset in CSS px (default: 0)
//!   --dpr FLOAT         Device pixel ratio for media queries/srcset (default: 1.0)
//!   --prefers-reduced-transparency reduce|no-preference|true|false
//!                        User media preference for reduced transparency (overrides env)
//!   --prefers-reduced-motion reduce|no-preference|true|false
//!                        User media preference for reduced motion (overrides env)
//!   --prefers-reduced-data reduce|no-preference|true|false
//!                        User media preference for reduced data (overrides env)
//!   --prefers-contrast   more|high|less|low|custom|forced|no-preference (overrides env)
//!   --prefers-color-scheme light|dark|no-preference (overrides env)
//!   --full-page         Expand the render target to the full content size (respects FASTR_FULL_PAGE env)
//!   --user-agent UA     Override the User-Agent header (default: Chrome-like)
//!   --accept-language   Override the Accept-Language header (default: en-US,en;q=0.9)
//!   --css-limit         Maximum number of external stylesheets to fetch (default: unlimited)
//!   --timings           Enable FASTR_RENDER_TIMINGS for per-stage logs

#![allow(clippy::io_other_error)]
#![allow(clippy::redundant_closure)]
#![allow(clippy::len_zero)]

use fastrender::css::encoding::decode_css_bytes;
use fastrender::css::loader::{
    absolutize_css_urls, extract_css_links, extract_embedded_css_urls, infer_base_url, inject_css_into_html,
    inline_imports, resolve_href,
};
use fastrender::html::encoding::decode_html_bytes;
use fastrender::html::meta_refresh::{extract_js_location_redirect, extract_meta_refresh_url};
use fastrender::resource::{
    normalize_user_agent_for_log, url_to_filename, HttpFetcher, ResourceFetcher, DEFAULT_ACCEPT_LANGUAGE,
    DEFAULT_USER_AGENT,
};
use fastrender::{Error, FastRender, Result};
use std::collections::HashSet;
use std::env;
use std::sync::mpsc::{channel, RecvTimeoutError};
use std::sync::Arc;
use std::thread;
use std::time::Duration;
use url::Url;

const STACK_SIZE: usize = 64 * 1024 * 1024; // 64MB to avoid stack overflows on large pages

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
    let config = ureq::Agent::config_builder().timeout_global(timeout).build();
    let agent: ureq::Agent = config.into();

    let mut current = url.to_string();
    for _ in 0..10 {
        let mut response = agent
            .get(&current)
            .header("User-Agent", user_agent)
            .header("Accept-Language", accept_language)
            .call()
            .map_err(|e| Error::Io(std::io::Error::new(std::io::ErrorKind::Other, e.to_string())))?;

        let status = response.status();
        if (300..400).contains(&status.as_u16()) {
            if let Some(loc) = response.headers().get("location").and_then(|h| h.to_str().ok()) {
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

        let bytes = response
            .body_mut()
            .read_to_vec()
            .map_err(|e| Error::Io(std::io::Error::new(std::io::ErrorKind::Other, e.to_string())))?;

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

/// Best-effort extraction of CSS URLs that appear inside inline scripts or attributes.
///
/// Some sites load their primary stylesheets dynamically and never emit a
/// `<link rel="stylesheet">` element in the static HTML. To render those pages
/// without executing JavaScript, scan the raw HTML for any substring that looks
/// like a CSS URL (ends with `.css`, possibly with a query string) and try to
/// resolve and fetch it as a stylesheet.
#[cfg(test)]
mod tests {
    use super::*;
    use fastrender::css::loader::resolve_href;

    fn accept_request(listener: &std::net::TcpListener, timeout: std::time::Duration) -> Option<std::net::TcpStream> {
        listener.set_nonblocking(true).expect("set listener nonblocking");
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
        fn fake_fetch(url: &str) -> Result<String> {
            match url {
                "https://example.com/css/imported.css" => Ok("h1 { color: red; }".to_string()),
                _ => Err(Error::Io(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    "not found",
                ))),
            }
        }
        let css = r#"@import url("https://example.com/css/imported.css") screen;"#;
        let mut seen = HashSet::new();
        let inlined = inline_imports(css, "https://example.com/css/main.css", &fake_fetch, &mut seen);
        assert!(inlined.contains("@media screen"), "expected media wrapper: {}", inlined);
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
        let urls = extract_css_links(html, "https://example.com/site/page.html");
        assert_eq!(urls, vec!["https://example.com/styles/a.css".to_string()]);
    }

    #[test]
    fn decode_html_uses_content_type_charset() {
        let encoded = encoding_rs::SHIFT_JIS.encode("abcデ").0;
        let decoded = decode_html_bytes(&encoded, Some("text/html; charset=shift_jis"));
        assert!(decoded.contains('デ'), "decoded text should include kana: {}", decoded);
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
        let bytes = vec![0xA3]; // U+00A3 in Windows-1252
        let decoded = decode_html_bytes(&bytes, None);
        assert_eq!(decoded, "£");
    }

    #[test]
    fn fetch_bytes_reads_local_meta_content_type() {
        let dir = tempfile::tempdir().expect("temp dir");
        let html_path = dir.path().join("page.html");
        let meta_path = dir.path().join("page.html.meta");

        // Write Shift-JIS encoded body and matching meta
        let encoded = encoding_rs::SHIFT_JIS.encode("<html><body>デ</body></html>").0.to_vec();
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
    fn parse_prefers_reduced_transparency_values() {
        assert_eq!(parse_prefers_reduced_transparency("reduce"), Some(true));
        assert_eq!(parse_prefers_reduced_transparency("no-preference"), Some(false));
        assert_eq!(parse_prefers_reduced_transparency("yes"), Some(true));
        assert_eq!(parse_prefers_reduced_transparency("off"), Some(false));
        assert_eq!(parse_prefers_reduced_transparency("maybe"), None);
    }

    #[test]
    fn fetch_bytes_uses_base_url_from_meta() {
        let dir = tempfile::tempdir().expect("temp dir");
        let html_path = dir.path().join("page.html");
        let meta_path = dir.path().join("page.html.meta");

        std::fs::write(&html_path, "<html><body>Hi</body></html>").unwrap();
        std::fs::write(&meta_path, "content-type: text/html\nurl: https://example.com/\n").unwrap();

        let url = format!("file://{}", html_path.display());
        let (_bytes, ct, base_url) =
            fetch_bytes(&url, None, DEFAULT_USER_AGENT, DEFAULT_ACCEPT_LANGUAGE).expect("fetch bytes");
        assert_eq!(ct.as_deref(), Some("text/html"));
        assert_eq!(base_url.as_deref(), Some("https://example.com/"));
    }

    #[test]
    fn fetch_bytes_sends_accept_language_header() {
        use std::io::{BufRead, BufReader, Write};
        use std::net::TcpListener;
        use std::thread;
        use std::time::Duration;

        let listener = TcpListener::bind("127.0.0.1:0").expect("bind listener");
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
            request.to_ascii_lowercase().contains("accept-language: en-us,en;q=0.9"),
            "missing Accept-Language in request: {request}"
        );
    }

    #[test]
    fn fetch_bytes_errors_on_empty_http_body() {
        use std::io::{BufRead, BufReader, Write};
        use std::net::TcpListener;
        use std::thread;
        use std::time::Duration;

        let listener = TcpListener::bind("127.0.0.1:0").expect("bind listener");
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
        use std::io::{BufRead, BufReader, Write};
        use std::net::TcpListener;
        use std::sync::{Arc, Mutex};
        use std::thread;
        use std::time::Duration;

        let listener = TcpListener::bind("127.0.0.1:0").expect("bind listener");
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
        use std::io::{BufRead, BufReader, Write};
        use std::net::TcpListener;
        use std::sync::{Arc, Mutex};
        use std::thread;
        use std::time::Duration;

        let listener = TcpListener::bind("127.0.0.1:0").expect("bind listener");
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
        let has_non_white = image.pixels().any(|p| p.0[0] != 255 || p.0[1] != 255 || p.0[2] != 255);
        assert!(has_non_white, "render should include content after meta refresh");
    }

    #[test]
    fn render_once_follows_js_location_redirect() {
        use std::io::{BufRead, BufReader, Write};
        use std::net::TcpListener;
        use std::sync::{Arc, Mutex};
        use std::thread;
        use std::time::Duration;

        let listener = TcpListener::bind("127.0.0.1:0").expect("bind listener");
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
        let has_non_white = image.pixels().any(|p| p.0[0] != 255 || p.0[1] != 255 || p.0[2] != 255);
        assert!(has_non_white, "render should include content after JS redirect");
    }

    #[test]
    fn render_once_fetches_assets_with_cli_headers() {
        use std::io::{BufRead, BufReader, Write};
        use std::net::TcpListener;
        use std::thread;

        const PNG_BYTES: &[u8] = b"\x89PNG\r\n\x1a\n\x00\x00\x00\rIHDR\x00\x00\x00\x01\x00\x00\x00\x01\x08\x02\x00\x00\x00\x90wS\xde\x00\x00\x00\nIDATx\x9cc```\x00\x00\x00\x04\x00\x01\r\n-\xb4\x00\x00\x00\x00IEND\xaeB`\x82";

        let listener = TcpListener::bind("127.0.0.1:0").expect("bind listener");
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
                    let html = format!("<html><body><img src=\"http://{}/img.png\"></body></html>", addr);
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

    #[test]
    fn parse_prefers_reduced_data_values() {
        assert_eq!(parse_prefers_reduced_data("reduce"), Some(true));
        assert_eq!(parse_prefers_reduced_data("no-preference"), Some(false));
        assert_eq!(parse_prefers_reduced_data("yes"), Some(true));
        assert_eq!(parse_prefers_reduced_data("off"), Some(false));
        assert_eq!(parse_prefers_reduced_data("maybe"), None);
    }

    #[test]
    fn parse_prefers_reduced_motion_values() {
        assert_eq!(parse_prefers_reduced_motion("reduce"), Some(true));
        assert_eq!(parse_prefers_reduced_motion("no-preference"), Some(false));
        assert_eq!(parse_prefers_reduced_motion("yes"), Some(true));
        assert_eq!(parse_prefers_reduced_motion("off"), Some(false));
        assert_eq!(parse_prefers_reduced_motion("maybe"), None);
    }

    #[test]
    fn parse_prefers_contrast_values() {
        assert_eq!(parse_prefers_contrast("more"), Some("more".to_string()));
        assert_eq!(parse_prefers_contrast("HIGH"), Some("high".to_string()));
        assert_eq!(parse_prefers_contrast("forced"), Some("forced".to_string()));
        assert_eq!(parse_prefers_contrast("maybe"), None);
    }

    #[test]
    fn parse_prefers_color_scheme_values() {
        assert_eq!(parse_prefers_color_scheme("dark"), Some("dark".to_string()));
        assert_eq!(parse_prefers_color_scheme("LIGHT"), Some("light".to_string()));
        assert_eq!(
            parse_prefers_color_scheme("no-preference"),
            Some("no-preference".to_string())
        );
        assert_eq!(parse_prefers_color_scheme("pink"), None);
    }

    #[test]
    fn parse_viewport_values() {
        assert_eq!(parse_viewport("1200x800"), Some((1200, 800)));
        assert_eq!(parse_viewport("800x600"), Some((800, 600)));
        assert_eq!(parse_viewport("0x600"), None);
        assert_eq!(parse_viewport("800"), None);
        assert_eq!(parse_viewport("800x"), None);
    }
}

fn usage(program: &str) {
    eprintln!(
        "Usage: {program} [--timeout SECONDS] [--viewport WxH] [--scroll-x PX] [--scroll-y PX] [--dpr FLOAT] [--prefers-reduced-transparency <value>] [--prefers-reduced-motion <value>] [--prefers-reduced-data <value>] [--prefers-contrast <value>] [--prefers-color-scheme <value>] [--full-page] [--user-agent UA] [--accept-language LANG] [--css-limit N] [--timings] <url> [output.png] [width] [height] [scroll_x] [scroll_y]"
    );
    eprintln!("Example: {program} --timeout 120 --viewport 1366x768 --dpr 2.0 https://www.example.com output.png 1200 800 0 0");
    eprintln!("  --viewport WxH      Override viewport size (default: 1200x800)");
    eprintln!("  --scroll-x PX       Horizontal scroll offset in CSS px (default: 0)");
    eprintln!("  --scroll-y PX       Vertical scroll offset in CSS px (default: 0)");
    eprintln!("  width: viewport width (default: 1200)");
    eprintln!("  height: viewport height (default: 800)");
    eprintln!("  dpr: device pixel ratio for media queries/srcset (default: 1.0)");
    eprintln!("  prefers-reduced-transparency: reduce|no-preference|true|false (overrides env)");
    eprintln!("  prefers-reduced-motion: reduce|no-preference|true|false (overrides env)");
    eprintln!("  prefers-reduced-data: reduce|no-preference|true|false (overrides env)");
    eprintln!("  prefers-contrast: more|high|less|low|custom|forced|no-preference (overrides env)");
    eprintln!("  prefers-color-scheme: light|dark|no-preference (overrides env)");
    eprintln!("  full-page: expand render target to full content size (or set FASTR_FULL_PAGE)");
    eprintln!("  user-agent: override the User-Agent header (default: Chrome-like)");
    eprintln!("  accept-language: override Accept-Language header (default: en-US,en;q=0.9)");
    eprintln!("  css-limit: maximum number of external stylesheets to fetch (default: unlimited)");
    eprintln!("  timings: set FASTR_RENDER_TIMINGS to print per-stage timings");
    eprintln!("  output: defaults to <url>.png (derived from the URL); parent directories are created");
}

fn parse_prefers_reduced_transparency(val: &str) -> Option<bool> {
    let v = val.trim().to_ascii_lowercase();
    if matches!(
        v.as_str(),
        "1" | "true" | "yes" | "on" | "reduce" | "reduced" | "prefer"
    ) {
        return Some(true);
    }
    if matches!(v.as_str(), "0" | "false" | "no" | "off" | "none" | "no-preference") {
        return Some(false);
    }
    None
}

fn parse_prefers_reduced_data(val: &str) -> Option<bool> {
    let v = val.trim().to_ascii_lowercase();
    if matches!(
        v.as_str(),
        "1" | "true" | "yes" | "on" | "reduce" | "reduced" | "prefer"
    ) {
        return Some(true);
    }
    if matches!(v.as_str(), "0" | "false" | "no" | "off" | "none" | "no-preference") {
        return Some(false);
    }
    None
}

fn parse_prefers_reduced_motion(val: &str) -> Option<bool> {
    let v = val.trim().to_ascii_lowercase();
    if matches!(
        v.as_str(),
        "1" | "true" | "yes" | "on" | "reduce" | "reduced" | "prefer" | "reduce-motion"
    ) {
        return Some(true);
    }
    if matches!(v.as_str(), "0" | "false" | "no" | "off" | "none" | "no-preference") {
        return Some(false);
    }
    None
}

fn parse_prefers_contrast(val: &str) -> Option<String> {
    let v = val.trim().to_ascii_lowercase();
    match v.as_str() {
        "more" | "high" | "less" | "low" | "custom" | "forced" | "no-preference" => Some(v),
        _ => None,
    }
}

fn parse_prefers_color_scheme(val: &str) -> Option<String> {
    let v = val.trim().to_ascii_lowercase();
    match v.as_str() {
        "light" | "dark" | "no-preference" => Some(v),
        _ => None,
    }
}

fn parse_viewport(val: &str) -> Option<(u32, u32)> {
    let parts: Vec<&str> = val.split('x').collect();
    if parts.len() != 2 {
        return None;
    }
    let w = parts.get(0)?.parse::<u32>().ok()?;
    let h = parts.get(1)?.parse::<u32>().ok()?;
    if w == 0 || h == 0 {
        return None;
    }
    Some((w, h))
}

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
    let (html_bytes, html_content_type, source_url) = fetch_bytes(url, timeout, user_agent, accept_language)?;
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
            eprintln!("Warning: skipping JS redirect of length {}", js_redirect.len());
        }
    }

    println!("Extracting CSS links...");
    let mut css_links = extract_css_links(&html, &resource_base);
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
                let inlined = inline_imports(
                    &rewritten,
                    &css_url,
                    &|u| {
                        fetch_bytes(u, timeout, user_agent, accept_language)
                            .map(|(b, ct, _)| decode_css_bytes(&b, ct.as_deref()))
                    },
                    &mut seen_imports,
                );
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
        .fetcher(Arc::new(http_fetcher) as Arc<dyn ResourceFetcher>)
        .build()?;
    renderer.set_base_url(resource_base.clone());
    let png_data =
        renderer.render_to_png_with_scroll(&html_with_css, width, height, scroll_x as f32, scroll_y as f32)?;

    println!("Saving to {}...", output);
    std::fs::write(&output, png_data)?;

    println!("✓ Successfully rendered {} to {}", url, output);
    println!("  Image size: {} bytes", std::fs::metadata(&output)?.len());
    Ok(())
}

fn main() -> Result<()> {
    let program = env::args().next().unwrap_or_else(|| "fetch_and_render".to_string());
    let mut args = env::args().skip(1);
    let mut timeout_secs: Option<u64> = None;
    let mut dpr: f32 = 1.0;
    let mut prefers_reduced_transparency: Option<bool> = None;
    let mut prefers_reduced_motion: Option<bool> = None;
    let mut prefers_reduced_data: Option<bool> = None;
    let mut prefers_contrast: Option<String> = None;
    let mut prefers_color_scheme: Option<String> = None;
    let mut positional: Vec<String> = Vec::new();
    let mut viewport: Option<(u32, u32)> = None;
    let mut scroll_override: Option<(u32, u32)> = None;
    let mut css_limit: Option<usize> = None;
    let mut full_page = false;
    let mut user_agent = DEFAULT_USER_AGENT.to_string();
    let mut accept_language = DEFAULT_ACCEPT_LANGUAGE.to_string();
    let mut enable_timings = false;
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--help" | "-h" => {
                usage(&program);
                return Ok(());
            }
            "--timeout" => {
                if let Some(val) = args.next() {
                    match val.parse::<u64>() {
                        Ok(0) => timeout_secs = None,
                        Ok(secs) => timeout_secs = Some(secs),
                        Err(_) => {}
                    }
                }
            }
            "--dpr" => {
                if let Some(val) = args.next() {
                    if let Ok(parsed) = val.parse::<f32>() {
                        if parsed.is_finite() && parsed > 0.0 {
                            dpr = parsed;
                        }
                    }
                }
            }
            "--prefers-reduced-transparency" => {
                if let Some(val) = args.next() {
                    prefers_reduced_transparency = parse_prefers_reduced_transparency(&val);
                }
            }
            "--prefers-reduced-motion" => {
                if let Some(val) = args.next() {
                    prefers_reduced_motion = parse_prefers_reduced_motion(&val);
                }
            }
            "--prefers-reduced-data" => {
                if let Some(val) = args.next() {
                    prefers_reduced_data = parse_prefers_reduced_data(&val);
                }
            }
            "--prefers-contrast" => {
                if let Some(val) = args.next() {
                    prefers_contrast = parse_prefers_contrast(&val);
                }
            }
            "--prefers-color-scheme" => {
                if let Some(val) = args.next() {
                    prefers_color_scheme = parse_prefers_color_scheme(&val);
                }
            }
            "--viewport" => {
                if let Some(val) = args.next() {
                    viewport = parse_viewport(&val).or(viewport);
                }
            }
            "--scroll-x" => {
                if let Some(val) = args.next() {
                    if let Ok(px) = val.parse::<u32>() {
                        scroll_override = Some((px, scroll_override.map(|(_, y)| y).unwrap_or(0)));
                    }
                }
            }
            "--scroll-y" => {
                if let Some(val) = args.next() {
                    if let Ok(py) = val.parse::<u32>() {
                        scroll_override = Some((scroll_override.map(|(x, _)| x).unwrap_or(0), py));
                    }
                }
            }
            "--full-page" => {
                full_page = true;
            }
            "--user-agent" => {
                if let Some(val) = args.next() {
                    if !val.trim().is_empty() {
                        user_agent = val;
                    }
                }
            }
            "--accept-language" => {
                if let Some(val) = args.next() {
                    if !val.trim().is_empty() {
                        accept_language = val;
                    }
                }
            }
            "--css-limit" => {
                if let Some(val) = args.next() {
                    if let Ok(limit) = val.parse::<usize>() {
                        css_limit = Some(limit);
                    }
                }
            }
            "--timings" => {
                enable_timings = true;
            }
            _ => {
                if arg.starts_with('-') {
                    eprintln!("Unknown option: {}", arg);
                    usage(&program);
                    std::process::exit(1);
                }
                positional.push(arg)
            }
        }
    }

    if positional.is_empty() {
        usage(&program);
        std::process::exit(1);
    }

    let url = positional.get(0).cloned().unwrap();
    let output = positional
        .get(1)
        .cloned()
        .unwrap_or_else(|| format!("{}.png", url_to_filename(&url)));
    let width = viewport
        .map(|(w, _)| w)
        .or_else(|| positional.get(2).and_then(|v| v.parse::<u32>().ok()))
        .unwrap_or(1200);
    let height = viewport
        .map(|(_, h)| h)
        .or_else(|| positional.get(3).and_then(|v| v.parse::<u32>().ok()))
        .unwrap_or(800);
    let scroll_x = scroll_override
        .map(|(x, _)| x)
        .or_else(|| positional.get(4).and_then(|v| v.parse::<u32>().ok()))
        .unwrap_or(0);
    let scroll_y = scroll_override
        .map(|(_, y)| y)
        .or_else(|| positional.get(5).and_then(|v| v.parse::<u32>().ok()))
        .unwrap_or(0);

    let output_path = std::path::Path::new(&output);
    if let Some(parent) = output_path.parent() {
        if !parent.as_os_str().is_empty() {
            std::fs::create_dir_all(parent)?;
        }
    }

    if let Some(reduce) = prefers_reduced_transparency {
        std::env::set_var(
            "FASTR_PREFERS_REDUCED_TRANSPARENCY",
            if reduce { "reduce" } else { "no-preference" },
        );
    }

    if let Some(reduce) = prefers_reduced_motion {
        std::env::set_var(
            "FASTR_PREFERS_REDUCED_MOTION",
            if reduce { "reduce" } else { "no-preference" },
        );
    }

    if let Some(reduce) = prefers_reduced_data {
        std::env::set_var(
            "FASTR_PREFERS_REDUCED_DATA",
            if reduce { "reduce" } else { "no-preference" },
        );
    }

    if let Some(contrast) = prefers_contrast {
        std::env::set_var("FASTR_PREFERS_CONTRAST", contrast);
    }

    if let Some(color_scheme) = prefers_color_scheme {
        std::env::set_var("FASTR_PREFERS_COLOR_SCHEME", color_scheme);
    }

    if full_page {
        std::env::set_var("FASTR_FULL_PAGE", "1");
    }

    if enable_timings {
        std::env::set_var("FASTR_RENDER_TIMINGS", "1");
    }

    eprintln!(
        "User-Agent: {}\nAccept-Language: {}\nViewport: {}x{} @{}x, scroll ({}, {})\nOutput: {}",
        normalize_user_agent_for_log(&user_agent),
        accept_language,
        width,
        height,
        dpr,
        scroll_x,
        scroll_y,
        output
    );

    let (tx, rx) = channel();
    let url_clone = url.clone();
    let output_clone = output.clone();
    let accept_language_clone = accept_language.clone();
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
                &accept_language_clone,
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
