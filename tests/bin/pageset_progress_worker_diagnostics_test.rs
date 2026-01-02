use crate::test_support::net::try_bind_localhost;
use serde_json::Value;
use std::fs;
use std::io::{Read, Write};
use std::net::TcpStream;
use std::process::Command;
use std::thread;
use std::time::{Duration, Instant};
use tempfile::TempDir;

#[test]
fn pageset_progress_worker_writes_diagnostics_stats() {
  let temp = TempDir::new().expect("tempdir");
  let cache_path = temp.path().join("example.html");
  fs::write(
    &cache_path,
    "<!doctype html><html><body><p>diagnostics</p></body></html>",
  )
  .expect("write html");

  let progress_path = temp.path().join("progress.json");

  let status = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .env("DISK_CACHE", "0")
    .env("NO_DISK_CACHE", "1")
    .current_dir(temp.path())
    .args([
      "worker",
      "--cache-path",
      cache_path.to_str().unwrap(),
      "--stem",
      "example",
      "--progress-path",
      progress_path.to_str().unwrap(),
      "--viewport",
      "800x600",
      "--dpr",
      "1.0",
      "--user-agent",
      "test-agent",
      "--accept-language",
      "en-US",
      "--diagnostics",
      "verbose",
      "--bundled-fonts",
    ])
    .status()
    .expect("run pageset_progress worker");

  assert!(status.success(), "worker should succeed");

  let contents = fs::read_to_string(&progress_path).expect("read progress");
  let json: Value = serde_json::from_str(&contents).expect("parse progress JSON");
  assert_eq!(json["status"], "ok", "worker should record ok status");
  let stats = json
    .get("diagnostics")
    .and_then(|d| d.get("stats"))
    .expect("progress should include diagnostics.stats");
  assert!(
    stats.get("counts").is_some(),
    "stats should include counts summary"
  );
  assert!(
    stats["counts"]["dom_nodes"].as_u64().unwrap_or(0) > 0,
    "dom_nodes count should be recorded"
  );
}

#[test]
fn pageset_progress_worker_writes_fetch_error_summary_for_ok_pages() {
  let temp = TempDir::new().expect("tempdir");
  let cache_path = temp.path().join("example.html");
  fs::write(
    &cache_path,
    r#"<!doctype html><html><body><img src="missing.png" width="10" height="10"></body></html>"#,
  )
  .expect("write html");

  let progress_path = temp.path().join("progress.json");

  let status = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .env("DISK_CACHE", "0")
    .env("NO_DISK_CACHE", "1")
    .current_dir(temp.path())
    .args([
      "worker",
      "--cache-path",
      cache_path.to_str().unwrap(),
      "--stem",
      "example",
      "--progress-path",
      progress_path.to_str().unwrap(),
      "--viewport",
      "800x600",
      "--dpr",
      "1.0",
      "--user-agent",
      "test-agent",
      "--accept-language",
      "en-US",
      "--diagnostics",
      "verbose",
      "--bundled-fonts",
    ])
    .status()
    .expect("run pageset_progress worker");

  assert!(status.success(), "worker should succeed");

  let contents = fs::read_to_string(&progress_path).expect("read progress");
  let json: Value = serde_json::from_str(&contents).expect("parse progress JSON");
  assert_eq!(json["status"], "ok", "worker should record ok status");

  let summary = json
    .get("diagnostics")
    .and_then(|d| d.get("fetch_error_summary"))
    .expect("progress should include diagnostics.fetch_error_summary");
  assert!(
    summary.get("total").and_then(|v| v.as_u64()).unwrap_or(0) > 0,
    "fetch_error_summary should report at least one error"
  );
  let samples = summary
    .get("samples")
    .and_then(|v| v.as_array())
    .expect("fetch_error_summary.samples should be an array");
  assert!(
    !samples.is_empty(),
    "fetch_error_summary.samples should be non-empty"
  );
  let first = &samples[0];
  assert_eq!(first["kind"], "Image", "sample kind should be recorded");
  assert!(
    first["url"]
      .as_str()
      .is_some_and(|url| url.contains("missing.png")),
    "sample url should include missing.png"
  );
  assert!(
    first["message"]
      .as_str()
      .is_some_and(|m| !m.trim().is_empty()),
    "sample message should be non-empty"
  );
}

#[test]
fn pageset_progress_worker_truncates_fetch_error_sample_urls() {
  let temp = TempDir::new().expect("tempdir");
  let cache_path = temp.path().join("example.html");
  let long_query = "a".repeat(600);
  let html = format!(
    r#"<!doctype html><html><body><img src="missing.png?{long_query}" width="10" height="10"></body></html>"#
  );
  fs::write(&cache_path, html).expect("write html");

  let progress_path = temp.path().join("progress.json");

  let status = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .env("DISK_CACHE", "0")
    .env("NO_DISK_CACHE", "1")
    .current_dir(temp.path())
    .args([
      "worker",
      "--cache-path",
      cache_path.to_str().unwrap(),
      "--stem",
      "example",
      "--progress-path",
      progress_path.to_str().unwrap(),
      "--viewport",
      "800x600",
      "--dpr",
      "1.0",
      "--user-agent",
      "test-agent",
      "--accept-language",
      "en-US",
      "--diagnostics",
      "verbose",
      "--bundled-fonts",
    ])
    .status()
    .expect("run pageset_progress worker");

  assert!(status.success(), "worker should succeed");

  let contents = fs::read_to_string(&progress_path).expect("read progress");
  let json: Value = serde_json::from_str(&contents).expect("parse progress JSON");
  let summary = json
    .get("diagnostics")
    .and_then(|d| d.get("fetch_error_summary"))
    .expect("progress should include diagnostics.fetch_error_summary");
  let samples = summary
    .get("samples")
    .and_then(|v| v.as_array())
    .expect("fetch_error_summary.samples should be an array");
  let first = &samples[0];
  let url = first["url"].as_str().expect("sample url should be string");
  assert!(
    url.contains("missing.png"),
    "truncated url should preserve the missing.png prefix"
  );
  assert!(
    url.chars().count() <= 240,
    "truncated url should stay within the progress URL size cap"
  );
  assert!(
    url.contains('…'),
    "truncated url should include an ellipsis marker"
  );
}

fn parse_http_request_path(stream: &mut TcpStream) -> Option<String> {
  let _ = stream.set_read_timeout(Some(Duration::from_secs(1)));
  let mut buffer = Vec::new();
  let mut temp = [0u8; 4096];
  let deadline = Instant::now() + Duration::from_secs(1);
  while Instant::now() < deadline {
    match stream.read(&mut temp) {
      Ok(0) => break,
      Ok(n) => {
        buffer.extend_from_slice(&temp[..n]);
        if let Some(end) = buffer
          .windows(4)
          .position(|w| w == b"\r\n\r\n")
          .map(|pos| pos + 4)
        {
          let req = &buffer[..end];
          let line_end = req.windows(2).position(|w| w == b"\r\n")?;
          let line = String::from_utf8_lossy(&req[..line_end]);
          let mut parts = line.split_whitespace();
          let _method = parts.next()?;
          let path = parts.next()?.to_string();
          return Some(path);
        }
      }
      Err(err)
        if matches!(
          err.kind(),
          std::io::ErrorKind::WouldBlock | std::io::ErrorKind::TimedOut
        ) =>
      {
        break
      }
      Err(_) => break,
    }
  }
  None
}

#[test]
fn pageset_progress_worker_classifies_bot_mitigation_blocks() {
  let Some(listener) =
    try_bind_localhost("pageset_progress_worker_classifies_bot_mitigation_blocks")
  else {
    return;
  };
  let addr = listener.local_addr().expect("addr");

  let server = thread::spawn(move || {
    let _ = listener.set_nonblocking(true);
    let deadline = Instant::now() + Duration::from_secs(5);
    let mut requests = 0usize;
    let mut last_request = Instant::now();
    while Instant::now() < deadline {
      if requests >= 2
        && Instant::now().saturating_duration_since(last_request) > Duration::from_millis(200)
      {
        break;
      }
      match listener.accept() {
        Ok((mut stream, _)) => {
          let Some(path) = parse_http_request_path(&mut stream) else {
            continue;
          };
          match path.as_str() {
            "/image.png" => {
              let location = format!("http://{addr}/blocked?captcha=1234");
              let response = format!(
                "HTTP/1.1 302 Found\r\nLocation: {location}\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
              );
              let _ = stream.write_all(response.as_bytes());
            }
            "/blocked?captcha=1234" => {
              let response =
                "HTTP/1.1 405 Method Not Allowed\r\nContent-Length: 0\r\nConnection: close\r\n\r\n";
              let _ = stream.write_all(response.as_bytes());
            }
            _ => {
              let response =
                "HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\nConnection: close\r\n\r\n";
              let _ = stream.write_all(response.as_bytes());
            }
          }
          requests += 1;
          last_request = Instant::now();
        }
        Err(err) if err.kind() == std::io::ErrorKind::WouldBlock => {
          thread::sleep(Duration::from_millis(5));
        }
        Err(_) => break,
      }
    }
  });

  let temp = TempDir::new().expect("tempdir");
  let cache_path = temp.path().join("example.html");
  let html = format!(
    r#"<!doctype html><html><body><img src="http://{addr}/image.png" width="10" height="10"></body></html>"#
  );
  fs::write(&cache_path, html).expect("write html");

  let progress_path = temp.path().join("progress.json");

  let status = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .env("DISK_CACHE", "0")
    .env("NO_DISK_CACHE", "1")
    .current_dir(temp.path())
    .args([
      "worker",
      "--cache-path",
      cache_path.to_str().unwrap(),
      "--stem",
      "example",
      "--progress-path",
      progress_path.to_str().unwrap(),
      "--viewport",
      "800x600",
      "--dpr",
      "1.0",
      "--user-agent",
      "test-agent",
      "--accept-language",
      "en-US",
      "--diagnostics",
      "verbose",
      "--bundled-fonts",
    ])
    .status()
    .expect("run pageset_progress worker");

  assert!(status.success(), "worker should succeed");

  server.join().expect("server thread");

  let contents = fs::read_to_string(&progress_path).expect("read progress");
  let json: Value = serde_json::from_str(&contents).expect("parse progress JSON");
  assert_eq!(json["status"], "ok", "worker should record ok status");
  assert!(
    json["failure_stage"].is_null(),
    "failure_stage should be null"
  );

  let diagnostics = json
    .get("diagnostics")
    .expect("progress should include diagnostics");
  assert!(
    diagnostics.get("fetch_error_summary").is_none(),
    "fetch_error_summary should exclude bot mitigation entries"
  );
  let summary = diagnostics
    .get("bot_mitigation_summary")
    .expect("progress should include bot_mitigation_summary");
  assert!(
    summary.get("total").and_then(|v| v.as_u64()).unwrap_or(0) > 0,
    "bot_mitigation_summary should report at least one entry"
  );
}
