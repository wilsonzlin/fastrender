use serde_json::Value;
use std::fs;
use std::process::Command;
use tempfile::tempdir;

#[cfg(unix)]
use std::os::unix::process::ExitStatusExt;

fn build_deep_html(depth: usize) -> String {
  let mut html = String::new();
  html.push_str("<!doctype html><html><head><title>Deep</title></head><body>");
  html.push_str(&"<div>".repeat(depth));
  html.push_str("x");
  html.push_str(&"</div>".repeat(depth));
  html.push_str("</body></html>");
  html
}

#[test]
fn pageset_progress_worker_runs_on_large_stack_thread() {
  // Deep nesting can trigger stack overflow aborts in the render pipeline when it runs on the
  // default (small) thread stack. This regression test ensures the worker uses a large-stack
  // thread like `render_pages` and `fetch_and_render`.
  let temp = tempdir().expect("tempdir");
  let cache_path = temp.path().join("deep.html");
  // Keep this below the 64MB CLI worker stack limit while still being deep enough to overflow the
  // default process stack.
  fs::write(&cache_path, build_deep_html(500)).expect("write deep html");

  let progress_path = temp.path().join("progress.json");
  let log_path = temp.path().join("worker.log");
  let stage_path = temp.path().join("worker.stage");

  let output = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .current_dir(temp.path())
    .args([
      "worker",
      "--cache-path",
      cache_path.to_str().unwrap(),
      "--stem",
      "deep",
      "--progress-path",
      progress_path.to_str().unwrap(),
      "--log-path",
      log_path.to_str().unwrap(),
      "--stage-path",
      stage_path.to_str().unwrap(),
      "--viewport",
      "800x600",
      "--dpr",
      "1.0",
      "--user-agent",
      "pageset-progress-stack-test",
      "--accept-language",
      "en-US",
      "--diagnostics",
      "none",
      // The nested DOM is large but should still complete quickly with no network IO.
      "--timeout",
      "30",
      "--bundled-fonts",
    ])
    .output()
    .expect("run pageset_progress worker on deep DOM");

  #[cfg(unix)]
  assert!(
    output.status.signal().is_none(),
    "worker exited via signal: {:?}\nstderr:\n{}",
    output.status,
    String::from_utf8_lossy(&output.stderr)
  );
  assert!(
    output.status.success(),
    "worker failed: {:?}\nstdout:\n{}\nstderr:\n{}",
    output.status,
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );

  let stderr = String::from_utf8_lossy(&output.stderr).to_ascii_lowercase();
  assert!(
    !stderr.contains("stack overflow") && !stderr.contains("overflowed its stack"),
    "worker stderr should not contain stack overflow indicators:\n{stderr}"
  );

  let progress_raw = fs::read_to_string(&progress_path).expect("read progress JSON");
  let progress: Value = serde_json::from_str(&progress_raw).expect("parse progress JSON");
  assert_eq!(progress["status"], "ok", "worker should record ok status");

  let log_contents = fs::read_to_string(&log_path).expect("read worker log");
  let log_lower = log_contents.to_ascii_lowercase();
  assert!(
    !log_lower.contains("stack overflow") && !log_lower.contains("overflowed its stack"),
    "worker log should not contain stack overflow indicators:\n{log_contents}"
  );

  let stage_contents = fs::read_to_string(&stage_path).expect("read worker stage heartbeat");
  assert!(
    !stage_contents.trim().is_empty(),
    "worker should write stage heartbeat markers"
  );
}
