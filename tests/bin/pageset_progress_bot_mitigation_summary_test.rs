use serde_json::Value;
use std::fs;
use std::io::{Read, Write};
use std::net::TcpListener;
use std::process::Command;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};
use tempfile::TempDir;

#[test]
fn pageset_progress_worker_records_bot_mitigation_summary_without_ok_with_failures() {
  let listener = TcpListener::bind("127.0.0.1:0").expect("bind localhost");
  let addr = listener.local_addr().expect("listener addr");
  listener
    .set_nonblocking(true)
    .expect("set nonblocking listener");

  let running = Arc::new(AtomicBool::new(true));
  let running_thread = Arc::clone(&running);

  let server = std::thread::spawn(move || {
    let start = Instant::now();
    while running_thread.load(Ordering::Relaxed) && start.elapsed() < Duration::from_secs(3) {
      match listener.accept() {
        Ok((mut stream, _)) => {
          let _ = stream.set_nonblocking(true);
          let mut buf = Vec::new();
          let mut tmp = [0u8; 1024];
          let read_start = Instant::now();
          loop {
            match stream.read(&mut tmp) {
              Ok(0) => break,
              Ok(n) => {
                buf.extend_from_slice(&tmp[..n]);
                if buf.windows(4).any(|w| w == b"\r\n\r\n") {
                  break;
                }
              }
              Err(err) if err.kind() == std::io::ErrorKind::WouldBlock => {
                if !running_thread.load(Ordering::Relaxed)
                  || read_start.elapsed() > Duration::from_secs(1)
                {
                  break;
                }
                std::thread::sleep(Duration::from_millis(5));
              }
              Err(_) => break,
            }
          }

          let request = String::from_utf8_lossy(&buf);
          let path = request
            .lines()
            .next()
            .and_then(|line| line.split_whitespace().nth(1))
            .unwrap_or("/");
          let has_captcha = path.to_ascii_lowercase().contains("captcha=");
          if has_captcha {
            let response =
              "HTTP/1.1 405 Method Not Allowed\r\nContent-Length: 0\r\nConnection: close\r\n\r\n";
            let _ = stream.write_all(response.as_bytes());
          } else {
            let location = format!("http://{addr}{path}?captcha=deadbeef");
            let response = format!(
              "HTTP/1.1 302 Found\r\nLocation: {location}\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
            );
            let _ = stream.write_all(response.as_bytes());
          }
        }
        Err(err) if err.kind() == std::io::ErrorKind::WouldBlock => {
          std::thread::sleep(Duration::from_millis(5));
        }
        Err(_) => break,
      }
    }
  });

  let temp = TempDir::new().expect("tempdir");
  let cache_path = temp.path().join("example.html");
  let image_url = format!("http://{addr}/asset.jpg");
  let html = format!(
    r#"<!doctype html><html><body><img src="{image_url}" width="10" height="10"></body></html>"#
  );
  fs::write(&cache_path, html).expect("write html");

  let progress_path = temp.path().join("progress.json");

  let status = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .env("DISK_CACHE", "0")
    .env("NO_DISK_CACHE", "1")
    .current_dir(temp.path())
    .env("FASTR_HTTP_BACKEND", "ureq")
    .env("FASTR_HTTP_MAX_ATTEMPTS", "1")
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
      "bot-mitigation-test-agent",
      "--accept-language",
      "en-US",
      "--diagnostics",
      "verbose",
      "--bundled-fonts",
    ])
    .status()
    .expect("run pageset_progress worker");

  running.store(false, Ordering::Relaxed);
  let _ = server.join();

  assert!(status.success(), "worker should exit successfully");

  let contents = fs::read_to_string(&progress_path).expect("read progress");
  let json: Value = serde_json::from_str(&contents).expect("parse progress JSON");
  assert_eq!(json["status"], "ok", "expected status=ok");
  assert!(
    json["failure_stage"].is_null(),
    "expected failure_stage=null"
  );

  assert!(
    json
      .get("diagnostics")
      .and_then(|d| d.get("fetch_error_summary"))
      .is_none(),
    "expected bot mitigation errors to be excluded from fetch_error_summary"
  );

  let bot = json
    .get("diagnostics")
    .and_then(|d| d.get("bot_mitigation_summary"))
    .expect("expected diagnostics.bot_mitigation_summary to be present");
  assert!(
    bot.get("total").and_then(|v| v.as_u64()).unwrap_or(0) > 0,
    "expected bot_mitigation_summary.total to be > 0"
  );
  let sample_final_url = bot
    .get("samples")
    .and_then(|v| v.as_array())
    .and_then(|arr| arr.first())
    .and_then(|v| v.get("final_url"))
    .and_then(|v| v.as_str())
    .unwrap_or_default();
  assert!(
    sample_final_url.to_ascii_lowercase().contains("captcha="),
    "expected bot_mitigation_summary sample to include captcha= final_url, got: {sample_final_url}"
  );

  let auto_notes = json
    .get("auto_notes")
    .and_then(|v| v.as_str())
    .unwrap_or("");
  assert!(
    !auto_notes.contains("ok with failures"),
    "expected ok pages with bot mitigation blocks to avoid the ok-with-failures auto_notes, got: {auto_notes}"
  );
}
