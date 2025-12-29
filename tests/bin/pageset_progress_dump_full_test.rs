use std::fs;
use std::process::Command;

use serde_json::Value;
use tempfile::tempdir;

#[test]
fn worker_writes_full_dumps_for_failures() {
  let temp = tempdir().expect("create temp dir");
  let cache_path = temp.path().join("page.html");
  fs::write(
    &cache_path,
    "<!doctype html><html><body><div>full dump</div></body></html>",
  )
  .expect("write cache html");
  let progress_path = temp.path().join("progress.json");
  let log_path = temp.path().join("worker.log");
  let dump_dir = temp.path().join("dumps");

  let status = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .env("FASTR_TEST_RENDER_DELAY_MS", "20")
    .args([
      "worker",
      "--cache-path",
      cache_path.to_str().unwrap(),
      "--stem",
      "dump",
      "--progress-path",
      progress_path.to_str().unwrap(),
      "--log-path",
      log_path.to_str().unwrap(),
      "--viewport",
      "800x600",
      "--dpr",
      "1.0",
      "--user-agent",
      "pageset-progress-test",
      "--accept-language",
      "en-US",
      "--diagnostics",
      "none",
      "--soft-timeout-ms",
      "1",
      "--dump-failures",
      "full",
      "--dump-dir",
      dump_dir.to_str().unwrap(),
      "--dump-timeout",
      "5",
      "--bundled-fonts",
    ])
    .status()
    .expect("run pageset_progress worker with dumps");

  assert!(status.success(), "worker should exit successfully");

  let progress_raw = fs::read_to_string(&progress_path).expect("read progress json");
  let progress: Value = serde_json::from_str(&progress_raw).expect("parse progress json");
  assert_ne!(
    progress["status"], "ok",
    "first render should report a failure to trigger dumps"
  );

  let dump_base = dump_dir.join("dump");
  assert!(
    dump_base.is_dir(),
    "expected dump directory {} to exist",
    dump_base.display()
  );
  let temp_path = temp.path().to_str().unwrap();
  for name in [
    "snapshot.json",
    "dom.json",
    "styled.json",
    "box_tree.json",
    "fragment_tree.json",
    "display_list.json",
  ] {
    let path = dump_base.join(name);
    assert!(path.is_file(), "expected {} to be written", path.display());
    let raw = fs::read_to_string(&path).expect("read dump json");
    let value: Value = serde_json::from_str(&raw).expect("parse dump json");
    assert!(
      value.get("schema_version").is_some(),
      "{} should include schema_version",
      name
    );
    assert!(
      !raw.contains(temp_path),
      "{} should not leak absolute paths",
      name
    );
  }
}
