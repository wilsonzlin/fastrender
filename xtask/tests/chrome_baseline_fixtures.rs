use std::fs;
use std::path::PathBuf;
use std::process::Command;

use tempfile::tempdir;

fn repo_root() -> PathBuf {
  let crate_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
  crate_dir
    .parent()
    .expect("xtask crate should live under the workspace root")
    .to_path_buf()
}

fn write_fixture(root: &std::path::Path, stem: &str) {
  let dir = root.join(stem);
  fs::create_dir_all(&dir).expect("create fixture dir");
  fs::write(
    dir.join("index.html"),
    "<!doctype html><html><head><meta charset=\"utf-8\"></head><body>Hello</body></html>",
  )
  .expect("write index.html");
}

fn write_stub_chrome(dir: &std::path::Path) -> PathBuf {
  let path = dir.join("chrome");
  fs::write(
    &path,
    r#"#!/usr/bin/env sh
set -eu
if [ "${1:-}" = "--version" ]; then
  echo "Chromium 123.0.0.0"
  exit 0
fi

echo "stub chrome args: $@"
for arg in "$@"; do
  case "$arg" in
    --screenshot=*)
      out="${arg#--screenshot=}"
      mkdir -p "$(dirname "$out")"
      # Minimal non-empty PNG header.
      printf '\x89PNG\r\n\x1a\n' > "$out"
      ;;
  esac
done
exit 0
"#,
  )
  .expect("write stub chrome");

  #[cfg(unix)]
  {
    use std::os::unix::fs::PermissionsExt;
    let mut perms = fs::metadata(&path).expect("stat chrome").permissions();
    perms.set_mode(0o755);
    fs::set_permissions(&path, perms).expect("chmod chrome");
  }

  path
}

#[test]
fn chrome_baseline_fixtures_respects_sharding_and_writes_outputs() {
  let repo_root = repo_root();
  let temp = tempdir().expect("temp dir");
  let fixture_root = temp.path().join("fixtures");
  let out_dir = temp.path().join("out");
  let chrome_dir = temp.path().join("chrome");
  fs::create_dir_all(&chrome_dir).expect("create chrome dir");
  write_stub_chrome(&chrome_dir);

  for stem in ["a", "b", "c", "d"] {
    write_fixture(&fixture_root, stem);
  }

  let status = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .current_dir(&repo_root)
    .arg("chrome-baseline-fixtures")
    .arg("--fixture-dir")
    .arg(&fixture_root)
    .arg("--out-dir")
    .arg(&out_dir)
    .arg("--chrome-dir")
    .arg(&chrome_dir)
    .arg("--shard")
    .arg("1/2")
    .status()
    .expect("run chrome-baseline-fixtures");
  assert!(status.success(), "command exited with {status}");

  // Sorted stems are a,b,c,d and shard 1/2 selects b,d.
  assert!(out_dir.join("b.png").is_file());
  assert!(out_dir.join("b.chrome.log").is_file());
  assert!(out_dir.join("b.json").is_file());
  assert!(out_dir.join("d.png").is_file());
  assert!(out_dir.join("d.chrome.log").is_file());
  assert!(out_dir.join("d.json").is_file());

  assert!(!out_dir.join("a.png").exists());
  assert!(!out_dir.join("c.png").exists());
}

#[test]
fn chrome_baseline_fixtures_builds_expected_chrome_command_flags() {
  let repo_root = repo_root();
  let temp = tempdir().expect("temp dir");
  let fixture_root = temp.path().join("fixtures");
  let out_dir = temp.path().join("out");
  let chrome_dir = temp.path().join("chrome");
  fs::create_dir_all(&chrome_dir).expect("create chrome dir");
  write_stub_chrome(&chrome_dir);

  write_fixture(&fixture_root, "hello");

  let status = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .current_dir(&repo_root)
    .arg("chrome-baseline-fixtures")
    .arg("--fixture-dir")
    .arg(&fixture_root)
    .arg("--out-dir")
    .arg(&out_dir)
    .arg("--chrome-dir")
    .arg(&chrome_dir)
    .arg("--fixtures")
    .arg("hello")
    .status()
    .expect("run chrome-baseline-fixtures");
  assert!(status.success(), "command exited with {status}");

  let log = fs::read_to_string(out_dir.join("hello.chrome.log")).expect("read chrome log");
  assert!(
    log.contains("--headless=new"),
    "chrome args should include --headless=new; got:\n{log}"
  );
  assert!(
    log.contains("--disable-gpu")
      && log.contains("--no-sandbox")
      && log.contains("--disable-dev-shm-usage")
      && log.contains("--hide-scrollbars"),
    "chrome args should include CI-safe flags; got:\n{log}"
  );
  assert!(
    log.contains("--window-size=1200,800"),
    "chrome args should include default viewport; got:\n{log}"
  );
  assert!(
    log.contains("--force-device-scale-factor=1"),
    "chrome args should include default DPR; got:\n{log}"
  );
  assert!(
    log.contains("--screenshot="),
    "chrome args should include --screenshot=...; got:\n{log}"
  );
}

#[test]
fn chrome_baseline_fixtures_errors_when_chrome_missing() {
  let repo_root = repo_root();
  let temp = tempdir().expect("temp dir");
  let fixture_root = temp.path().join("fixtures");
  let out_dir = temp.path().join("out");
  let empty_chrome_dir = temp.path().join("empty-chrome");
  fs::create_dir_all(&empty_chrome_dir).expect("create empty chrome dir");

  write_fixture(&fixture_root, "hello");

  let output = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .current_dir(&repo_root)
    .arg("chrome-baseline-fixtures")
    .arg("--fixture-dir")
    .arg(&fixture_root)
    .arg("--out-dir")
    .arg(&out_dir)
    .arg("--chrome-dir")
    .arg(&empty_chrome_dir)
    .arg("--fixtures")
    .arg("hello")
    .output()
    .expect("run chrome-baseline-fixtures");

  assert!(
    !output.status.success(),
    "command should fail without a chrome binary"
  );

  let stderr = String::from_utf8_lossy(&output.stderr);
  assert!(
    stderr.contains("No Chrome/Chromium binary found"),
    "error should mention missing chrome; got:\n{stderr}"
  );
}
