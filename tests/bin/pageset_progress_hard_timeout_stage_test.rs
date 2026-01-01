use serde_json::Value;
use std::fs;
use std::process::Command;
use std::thread;
use std::time::Duration;
use tempfile::TempDir;

#[test]
fn pageset_progress_hard_timeout_populates_timeout_stage_from_heartbeat() {
  let temp = TempDir::new().expect("tempdir");
  let html_dir = temp.path().join("fetches/html");
  fs::create_dir_all(&html_dir).expect("create html dir");
  fs::write(
    html_dir.join("slow.html"),
    "<!doctype html><title>Slow</title>",
  )
  .expect("write slow html");

  let progress_dir = temp.path().join("progress");
  let log_dir = temp.path().join("logs");

  let status = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .current_dir(temp.path())
    .env("FASTR_TEST_RENDER_DELAY_MS", "2000")
    .env("FASTR_TEST_RENDER_DELAY_STEM", "slow")
    .args([
      "run",
      "--jobs",
      "1",
      "--timeout",
      "1",
      "--diagnostics",
      "none",
      "--progress-dir",
      progress_dir.to_str().expect("progress dir"),
      "--log-dir",
      log_dir.to_str().expect("log dir"),
      "--bundled-fonts",
    ])
    .status()
    .expect("run pageset_progress");

  assert!(status.success(), "pageset_progress exited with {status:?}");

  let progress_raw =
    fs::read_to_string(progress_dir.join("slow.json")).expect("read progress json");
  let progress: Value = serde_json::from_str(&progress_raw).expect("parse progress json");
  assert_eq!(progress["status"], "timeout");
  assert_eq!(progress["timeout_stage"], "dom_parse");
  assert_eq!(progress["hotspot"], "fetch");
  let auto_notes = progress["auto_notes"].as_str().unwrap_or_default();
  assert!(
    auto_notes.contains("exit:"),
    "expected auto_notes to include exit status, got: {auto_notes:?}"
  );

  let stages = &progress["stages_ms"];
  let fetch = stages["fetch"].as_f64().expect("fetch stage ms");
  assert!(
    fetch > 0.0,
    "expected fetch stage to be > 0ms, got {fetch} (stages_ms={stages:?})"
  );
  assert_eq!(stages["css"].as_f64().expect("css stage ms"), 0.0);
  assert_eq!(stages["cascade"].as_f64().expect("cascade stage ms"), 0.0);
  assert_eq!(stages["layout"].as_f64().expect("layout stage ms"), 0.0);
  assert_eq!(stages["paint"].as_f64().expect("paint stage ms"), 0.0);

  let total_ms = progress["total_ms"].as_f64().expect("total_ms");
  let sum = fetch
    + stages["css"].as_f64().unwrap()
    + stages["cascade"].as_f64().unwrap()
    + stages["layout"].as_f64().unwrap()
    + stages["paint"].as_f64().unwrap();
  assert!(
    (sum - total_ms).abs() < 1.0,
    "expected stages_ms to sum to total_ms (~{total_ms}), got {sum} (stages_ms={stages:?})"
  );
}

#[test]
fn pageset_progress_hard_timeout_falls_back_when_stage_timeline_missing() {
  let temp = TempDir::new().expect("tempdir");
  let html_dir = temp.path().join("fetches/html");
  fs::create_dir_all(&html_dir).expect("create html dir");
  fs::write(
    html_dir.join("missing_timeline.html"),
    "<!doctype html><title>Missing timeline</title>",
  )
  .expect("write html");

  let progress_dir = temp.path().join("progress");
  let log_dir = temp.path().join("logs");
  fs::create_dir_all(&log_dir).expect("create log dir");

  let mut child = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .current_dir(temp.path())
    .env("FASTR_TEST_RENDER_DELAY_MS", "2000")
    .env("FASTR_TEST_RENDER_DELAY_STEM", "missing_timeline")
    .args([
      "run",
      "--jobs",
      "1",
      "--timeout",
      "1",
      "--diagnostics",
      "none",
      "--progress-dir",
      progress_dir.to_str().expect("progress dir"),
      "--log-dir",
      log_dir.to_str().expect("log dir"),
      "--bundled-fonts",
    ])
    .spawn()
    .expect("spawn pageset_progress");

  let timeline_path = log_dir.join("missing_timeline.stage.timeline");
  // Wait for the worker to write at least one entry to the timeline, then delete it so the parent
  // must fall back to stage attribution.
  let deleter = thread::spawn(move || {
    for _ in 0..400 {
      if let Ok(meta) = fs::metadata(&timeline_path) {
        if meta.len() > 0 {
          let _ = fs::remove_file(&timeline_path);
          break;
        }
      }
      thread::sleep(Duration::from_millis(5));
    }
  });

  let status = child.wait().expect("wait pageset_progress");
  deleter.join().expect("join timeline deleter");
  assert!(status.success(), "pageset_progress exited with {status:?}");

  let progress_raw = fs::read_to_string(progress_dir.join("missing_timeline.json"))
    .expect("read missing timeline progress json");
  let progress: Value = serde_json::from_str(&progress_raw).expect("parse progress json");
  assert_eq!(progress["status"], "timeout");
  assert_eq!(progress["timeout_stage"], "dom_parse");

  let stages = &progress["stages_ms"];
  let fetch = stages["fetch"].as_f64().unwrap_or(0.0);
  assert!(
    fetch > 0.0,
    "expected fallback to attribute fetch stage, got {fetch} (stages_ms={stages:?})"
  );
  assert_eq!(stages["css"].as_f64().unwrap_or(-1.0), 0.0);
  assert_eq!(stages["cascade"].as_f64().unwrap_or(-1.0), 0.0);
  assert_eq!(stages["layout"].as_f64().unwrap_or(-1.0), 0.0);
  assert_eq!(stages["paint"].as_f64().unwrap_or(-1.0), 0.0);

  let total_ms = progress["total_ms"].as_f64().unwrap_or(0.0);
  let sum = fetch
    + stages["css"].as_f64().unwrap_or(0.0)
    + stages["cascade"].as_f64().unwrap_or(0.0)
    + stages["layout"].as_f64().unwrap_or(0.0)
    + stages["paint"].as_f64().unwrap_or(0.0);
  assert!(
    (sum - total_ms).abs() < 1.0,
    "expected stages_ms to sum to total_ms (~{total_ms}), got {sum} (stages_ms={stages:?})"
  );

  let auto_notes = progress["auto_notes"].as_str().unwrap_or_default();
  assert!(
    auto_notes.contains("exit:"),
    "expected auto_notes to include exit status, got: {auto_notes:?}"
  );
}

#[test]
#[cfg(unix)]
fn pageset_progress_hard_timeout_crash_exit_is_classified_as_panic() {
  let temp = TempDir::new().expect("tempdir");
  let html_dir = temp.path().join("fetches/html");
  fs::create_dir_all(&html_dir).expect("create html dir");
  fs::write(html_dir.join("abort.html"), "<!doctype html><title>Abort</title>")
    .expect("write html");

  let progress_dir = temp.path().join("progress");
  let log_dir = temp.path().join("logs");

  let status = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .current_dir(temp.path())
    // Slow down polling so the worker can abort between polls and still be handled by the hard
    // timeout path (instead of being collected via `try_wait`).
    .env("FASTR_TEST_PAGESET_POLL_INTERVAL_MS", "1200")
    // Abort well before the hard timeout so slow process startup can't turn this into a SIGKILL.
    .env("FASTR_TEST_WORKER_ABORT_AFTER_MS", "200")
    .env("FASTR_TEST_WORKER_ABORT_STEM", "abort")
    .args([
      "run",
      "--jobs",
      "1",
      "--timeout",
      "1",
      "--diagnostics",
      "none",
      "--progress-dir",
      progress_dir.to_str().expect("progress dir"),
      "--log-dir",
      log_dir.to_str().expect("log dir"),
      "--bundled-fonts",
    ])
    .status()
    .expect("run pageset_progress");

  assert!(status.success(), "pageset_progress exited with {status:?}");

  let progress_raw =
    fs::read_to_string(progress_dir.join("abort.json")).expect("read progress json");
  let progress: Value = serde_json::from_str(&progress_raw).expect("parse progress json");
  assert_eq!(
    progress["status"], "panic",
    "expected crash-at-timeout to be recorded as panic, got: {progress:?}"
  );
  let auto_notes = progress["auto_notes"].as_str().unwrap_or_default();
  assert!(
    auto_notes.contains("hard timeout"),
    "expected auto_notes to preserve hard timeout note, got: {auto_notes:?}"
  );
  assert!(
    auto_notes.contains("exit: signal 6"),
    "expected auto_notes to include SIGABRT exit status, got: {auto_notes:?}"
  );
  assert_eq!(progress["failure_stage"], "dom_parse");
}
