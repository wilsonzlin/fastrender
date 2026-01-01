use serde_json::Value;
use std::fs;
use std::io::Read;
use std::net::TcpListener;
use std::process::Command;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::Arc;
use std::time::Duration;
use tempfile::tempdir;

#[test]
fn pageset_progress_worker_respects_fetch_timeout_budget() {
  let listener = TcpListener::bind("127.0.0.1:0").expect("bind localhost");
  let addr = listener.local_addr().expect("listener addr");
  let running = Arc::new(AtomicBool::new(true));
  let accepted = Arc::new(AtomicUsize::new(0));
  let running_thread = Arc::clone(&running);
  let accepted_thread = Arc::clone(&accepted);
  let server = std::thread::spawn(move || {
    listener
      .set_nonblocking(true)
      .expect("set nonblocking listener");
    let mut handlers = Vec::new();
    while running_thread.load(Ordering::Relaxed) {
      match listener.accept() {
        Ok((mut stream, _)) => {
          accepted_thread.fetch_add(1, Ordering::SeqCst);
          let running_conn = Arc::clone(&running_thread);
          handlers.push(std::thread::spawn(move || {
            // Read any request bytes, then intentionally keep the socket open without responding so
            // the client hits its request timeout.
            let _ = stream.set_nonblocking(true);
            let mut buf = [0u8; 1024];
            let _ = stream.read(&mut buf);
            while running_conn.load(Ordering::Relaxed) {
              std::thread::sleep(Duration::from_millis(10));
            }
          }));
        }
        Err(err) if err.kind() == std::io::ErrorKind::WouldBlock => {
          std::thread::sleep(Duration::from_millis(5));
        }
        Err(_) => break,
      }
    }
    for handle in handlers {
      let _ = handle.join();
    }
  });

  let temp = tempdir().expect("create temp dir");
  let cache_path = temp.path().join("hanging.html");
  let refresh_url = format!("http://{addr}/hang");
  let html = format!(
    r#"<!doctype html><html><head><meta http-equiv="refresh" content="0;url={refresh_url}"></head><body><p>redirect fetch should respect timeout budget</p></body></html>"#
  );
  fs::write(&cache_path, html).expect("write cache html");
  let progress_path = temp.path().join("progress.json");
  let log_path = temp.path().join("worker.log");

  let status = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .env("FASTR_USE_BUNDLED_FONTS", "0")
    // Ensure retries are actually exercised (auto mode disables retries on the first ureq hop).
    .env("FASTR_HTTP_BACKEND", "ureq")
    .env("FASTR_HTTP_MAX_ATTEMPTS", "5")
    .args([
      "worker",
      "--cache-path",
      cache_path.to_str().unwrap(),
      "--stem",
      "hanging",
      "--progress-path",
      progress_path.to_str().unwrap(),
      "--log-path",
      log_path.to_str().unwrap(),
      "--viewport",
      "800x600",
      "--dpr",
      "1.0",
      "--user-agent",
      "pageset-progress-fetch-timeout-test",
      "--accept-language",
      "en-US",
      "--diagnostics",
      "none",
      "--timeout",
      "1",
      "--soft-timeout-ms",
      "100",
      "--bundled-fonts",
    ])
    .status()
    .expect("run pageset_progress worker with fetch timeout");

  running.store(false, Ordering::Relaxed);
  let _ = server.join();

  assert!(status.success(), "worker exited with {status:?}");

  let progress_raw = fs::read_to_string(&progress_path).expect("read progress json");
  let progress: Value = serde_json::from_str(&progress_raw).expect("parse progress json");
  let status_str = progress["status"].as_str().unwrap_or_default();
  assert_eq!(status_str, "ok", "worker should render successfully");
  let notes = progress["auto_notes"]
    .as_str()
    .unwrap_or_default()
    .to_ascii_lowercase();
  assert!(
    !notes.contains("read:"),
    "worker should have produced a render result, got auto_notes: {notes}"
  );

  let log = fs::read_to_string(&log_path).expect("read worker log");
  let log_lower = log.to_ascii_lowercase();
  let needle = "overall http timeout budget exceeded (budget=";
  assert!(
    log_lower.contains(needle),
    "expected worker log to mention budget exhaustion (budget semantics), got:\n{log}"
  );
  assert!(
    log_lower.contains("/5)"),
    "expected worker log to include an attempt suffix with max_attempts=5 (so retries are enabled), got:\n{log}"
  );
  assert!(
    accepted.load(Ordering::SeqCst) < 3,
    "expected budget-mode fetch to stop after <=2 connections; got {} connections\n{log}",
    accepted.load(Ordering::SeqCst)
  );
}
