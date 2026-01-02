#![cfg(not(feature = "disk_cache"))]

use std::process::Command;

#[test]
fn bundle_page_cache_accepts_cache_dir_alias_without_disk_cache_feature() {
  let tmp = tempfile::TempDir::new().expect("tempdir");
  let bundle_dir = tmp.path().join("bundle");
  let cache_dir = tmp.path().join("cache");

  let output = Command::new(env!("CARGO_BIN_EXE_bundle_page"))
    .args(["cache", "example.invalid", "--out"])
    .arg(&bundle_dir)
    .args(["--cache-dir"])
    .arg(&cache_dir)
    .output()
    .expect("run bundle_page cache");

  assert!(
    !output.status.success(),
    "expected bundle_page cache to fail without disk_cache feature"
  );

  let stderr = String::from_utf8_lossy(&output.stderr);
  assert!(
    !stderr.contains("unexpected argument '--cache-dir'"),
    "--cache-dir alias should be accepted by clap; got stderr:\n{stderr}"
  );
  assert!(
    stderr.contains("disk_cache"),
    "expected error to mention disk_cache feature, got stderr:\n{stderr}"
  );
}

