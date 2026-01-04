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
#[cfg(unix)]
fn diff_renders_no_build_runs_existing_binary() {
  let temp = tempdir().expect("tempdir");
  let bin_dir = temp.path().join("bin");
  let target_dir = temp.path().join("target");
  fs::create_dir_all(&bin_dir).expect("create stub bin dir");
  fs::create_dir_all(target_dir.join("release")).expect("create stub target dir");

  // Place a stub `cargo` in PATH that fails if invoked. When `--no-build` is working correctly,
  // xtask should never spawn `cargo build`.
  let stub_cargo = bin_dir.join("cargo");
  fs::write(&stub_cargo, "#!/usr/bin/env sh\necho 'cargo should not be called' >&2\nexit 2\n")
    .expect("write stub cargo");
  make_executable(&stub_cargo);

  // Provide a stub diff_renders binary that writes the requested report files and exits 0.
  let diff_renders = target_dir.join("release").join("diff_renders");
  fs::write(
    &diff_renders,
    r#"#!/usr/bin/env sh
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
{"totals":{"discovered":1,"processed":1,"differences":0,"missing":0,"errors":0}}
JSON
exit 0
"#,
  )
  .expect("write stub diff_renders");
  make_executable(&diff_renders);

  let path_var = std::env::var_os("PATH").unwrap_or_default();
  let mut paths = vec![bin_dir];
  paths.extend(std::env::split_paths(&path_var));
  let path = std::env::join_paths(paths).expect("join PATH");

  let before = temp.path().join("before.png");
  let after = temp.path().join("after.png");
  fs::write(&before, b"").expect("write before placeholder");
  fs::write(&after, b"").expect("write after placeholder");

  let out_dir = temp.path().join("out");

  let output = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .current_dir(repo_root())
    .env("PATH", path)
    .env("CARGO_TARGET_DIR", &target_dir)
    .arg("diff-renders")
    .arg("--no-build")
    .arg("--before")
    .arg(&before)
    .arg("--after")
    .arg(&after)
    .arg("--output")
    .arg(&out_dir)
    .output()
    .expect("run xtask diff-renders --no-build");

  assert!(
    output.status.success(),
    "expected diff-renders to succeed.\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );

  assert!(
    out_dir.join("diff_report.html").is_file(),
    "missing diff_report.html"
  );
  assert!(
    out_dir.join("diff_report.json").is_file(),
    "missing diff_report.json"
  );
}

