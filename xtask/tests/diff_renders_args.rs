use std::fs;
use std::path::PathBuf;
use std::process::Command;

use tempfile::tempdir;

fn repo_root() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .parent()
    .expect("xtask crate should live under the workspace root")
    .to_path_buf()
}

#[test]
fn diff_renders_rejects_invalid_max_perceptual_distance() {
  let temp = tempdir().expect("tempdir");
  let before = temp.path().join("before.png");
  let after = temp.path().join("after.png");

  // The command should fail before reaching the underlying diff_renders executable, so the files
  // need not be valid PNGs, but creating them makes the intent obvious.
  fs::write(&before, b"").expect("write before placeholder");
  fs::write(&after, b"").expect("write after placeholder");

  let output = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .current_dir(repo_root())
    .arg("diff-renders")
    .arg("--before")
    .arg(&before)
    .arg("--after")
    .arg(&after)
    .arg("--max-perceptual-distance")
    .arg("2")
    .arg("--output")
    .arg(temp.path().join("out"))
    .output()
    .expect("run xtask diff-renders");

  assert!(
    !output.status.success(),
    "expected diff-renders to fail for invalid perceptual distance"
  );
  let stderr = String::from_utf8_lossy(&output.stderr);
  assert!(
    stderr.contains("--max-perceptual-distance must be a finite number between 0 and 1"),
    "expected error to mention perceptual distance range; got:\n{stderr}"
  );
}

#[test]
fn diff_renders_rejects_invalid_max_diff_percent() {
  let temp = tempdir().expect("tempdir");
  let before = temp.path().join("before.png");
  let after = temp.path().join("after.png");
  fs::write(&before, b"").expect("write before placeholder");
  fs::write(&after, b"").expect("write after placeholder");

  let output = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .current_dir(repo_root())
    .arg("diff-renders")
    .arg("--before")
    .arg(&before)
    .arg("--after")
    .arg(&after)
    .arg("--max-diff-percent")
    .arg("101")
    .arg("--output")
    .arg(temp.path().join("out"))
    .output()
    .expect("run xtask diff-renders");

  assert!(
    !output.status.success(),
    "expected diff-renders to fail for invalid max-diff-percent"
  );
  let stderr = String::from_utf8_lossy(&output.stderr);
  assert!(
    stderr.contains("--max-diff-percent must be a finite number between 0 and 100"),
    "expected error to mention max-diff-percent range; got:\n{stderr}"
  );
}

