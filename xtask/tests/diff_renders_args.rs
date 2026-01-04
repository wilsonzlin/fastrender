use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use tempfile::tempdir;

#[cfg(unix)]
fn make_executable(path: &Path) {
  use std::os::unix::fs::PermissionsExt;
  let mut perms = fs::metadata(path).expect("stat stub executable").permissions();
  perms.set_mode(0o755);
  fs::set_permissions(path, perms).expect("chmod stub executable");
}

#[cfg(not(unix))]
fn make_executable(_path: &Path) {}

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

#[test]
#[cfg(unix)]
fn diff_renders_fail_on_differences_propagates_exit_code() {
  let temp = tempdir().expect("tempdir");
  let bin_dir = temp.path().join("bin");
  let target_dir = temp.path().join("target");
  fs::create_dir_all(&bin_dir).expect("create stub bin dir");

  let stub_cargo = bin_dir.join("cargo");
  fs::write(
    &stub_cargo,
    r#"#!/usr/bin/env sh
set -eu

subcommand="${1:-}"

bin=""
prev=""
for arg in "$@"; do
  if [ "$prev" = "--bin" ]; then
    bin="$arg"
    break
  fi
  prev="$arg"
done

if [ "$subcommand" = "build" ] && [ "$bin" = "diff_renders" ]; then
  out="${CARGO_TARGET_DIR:-target}"
  case "$out" in
    /*) ;;
    *) out="$(pwd)/$out" ;;
  esac
  out="$out/release/diff_renders"
  mkdir -p "$(dirname "$out")"
  cat > "$out" <<'SH'
#!/usr/bin/env sh
set -eu

html=""
json=""
while [ "$#" -gt 0 ]; do
  case "$1" in
    --html) html="$2"; shift 2;;
    --json) json="$2"; shift 2;;
    *) shift;;
  esac
done

mkdir -p "$(dirname "$html")"
mkdir -p "$(dirname "$json")"
echo "<!doctype html><title>stub diff</title>" > "$html"
cat > "$json" <<'JSON'
{"totals":{"discovered":1,"processed":1,"differences":1,"missing":0,"errors":0}}
JSON
echo "1 differences over threshold" >&2
exit 1
SH
  chmod +x "$out"
  exit 0
fi

echo "stub cargo: unsupported invocation ($*)" >&2
exit 2
"#,
  )
  .expect("write stub cargo");
  make_executable(&stub_cargo);

  let path_var = std::env::var_os("PATH").unwrap_or_default();
  let mut paths = vec![bin_dir];
  paths.extend(std::env::split_paths(&path_var));
  let path = std::env::join_paths(paths).expect("join PATH");

  let before = temp.path().join("before.png");
  let after = temp.path().join("after.png");
  fs::write(&before, b"").expect("write before placeholder");
  fs::write(&after, b"").expect("write after placeholder");

  let out_dir_ok = temp.path().join("out_ok");
  let output_ok = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .current_dir(repo_root())
    .env("PATH", &path)
    .env("CARGO_TARGET_DIR", &target_dir)
    .arg("diff-renders")
    .arg("--before")
    .arg(&before)
    .arg("--after")
    .arg(&after)
    .arg("--output")
    .arg(&out_dir_ok)
    .output()
    .expect("run xtask diff-renders");
  assert!(
    output_ok.status.success(),
    "diff-renders should exit 0 by default.\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output_ok.stdout),
    String::from_utf8_lossy(&output_ok.stderr)
  );
  assert!(
    out_dir_ok.join("diff_report.json").is_file(),
    "expected report json to exist even when diffs were found"
  );

  let out_dir_fail = temp.path().join("out_fail");
  let output_fail = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .current_dir(repo_root())
    .env("PATH", &path)
    .env("CARGO_TARGET_DIR", &target_dir)
    .arg("diff-renders")
    .arg("--fail-on-differences")
    .arg("--before")
    .arg(&before)
    .arg("--after")
    .arg(&after)
    .arg("--output")
    .arg(&out_dir_fail)
    .output()
    .expect("run xtask diff-renders --fail-on-differences");
  assert!(
    !output_fail.status.success(),
    "diff-renders should exit non-zero when --fail-on-differences is set"
  );
  let stderr = String::from_utf8_lossy(&output_fail.stderr);
  assert!(
    stderr.contains("diff_renders reported differences"),
    "expected stderr to mention diff_renders differences; got:\n{stderr}"
  );
  assert!(
    out_dir_fail.join("diff_report.json").is_file(),
    "expected report json to be preserved even when failing"
  );
  assert!(
    out_dir_fail.join("diff_report.html").is_file(),
    "expected report html to be preserved even when failing"
  );
}
