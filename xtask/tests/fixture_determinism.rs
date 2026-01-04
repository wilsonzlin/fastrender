use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use tempfile::tempdir;

#[cfg(unix)]
fn make_executable(path: &Path) {
  use std::os::unix::fs::PermissionsExt;
  let mut perms = fs::metadata(path)
    .expect("stat stub executable")
    .permissions();
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

fn prepend_path(bin_dir: &Path) -> std::ffi::OsString {
  let path_var = std::env::var_os("PATH").unwrap_or_default();
  let mut paths = vec![bin_dir.to_path_buf()];
  paths.extend(std::env::split_paths(&path_var));
  std::env::join_paths(paths).expect("join PATH")
}

fn write_stub_cargo(bin_dir: &Path) -> PathBuf {
  let stub_cargo = bin_dir.join("cargo");
  fs::write(
    &stub_cargo,
    "#!/usr/bin/env sh\necho 'cargo should not be called' >&2\nexit 2\n",
  )
  .expect("write stub cargo");
  make_executable(&stub_cargo);
  stub_cargo
}

fn write_stub_render_fixtures(target_dir: &Path) -> PathBuf {
  let render_fixtures = target_dir.join("release").join("render_fixtures");
  fs::write(
    &render_fixtures,
    r#"#!/usr/bin/env sh
set -eu

out=""
fixtures="hello"
while [ "$#" -gt 0 ]; do
  case "$1" in
    --out-dir) out="$2"; shift 2;;
    --fixtures) fixtures="$2"; shift 2;;
    *) shift;;
  esac
done

mkdir -p "$out"
stem="$(printf "%s" "$fixtures" | cut -d',' -f1)"
printf "PNG" > "$out/$stem.png"
exit 0
"#,
  )
  .expect("write stub render_fixtures");
  make_executable(&render_fixtures);
  render_fixtures
}

fn write_stub_diff_renders(target_dir: &Path, differences: bool) -> PathBuf {
  let diff_renders = target_dir.join("release").join("diff_renders");
  let script = if differences {
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
{
  "totals": {
    "discovered": 1,
    "processed": 1,
    "matches": 0,
    "within_threshold": 0,
    "differences": 1,
    "missing": 0,
    "errors": 0,
    "shard_skipped": 0
  },
  "results": [
    {
      "name": "hello",
      "status": "diff",
      "before": "../run1/hello.png",
      "after": "../run2/hello.png",
      "diff": "diff.png",
      "metrics": {
        "pixel_diff": 1,
        "total_pixels": 1,
        "diff_percentage": 100.0,
        "perceptual_distance": 1.0
      },
      "error": null
    }
  ]
}
JSON
echo "PNG" > "$(dirname "$html")/diff.png"
exit 1
"#
  } else {
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
{
  "totals": {
    "discovered": 1,
    "processed": 1,
    "matches": 1,
    "within_threshold": 0,
    "differences": 0,
    "missing": 0,
    "errors": 0,
    "shard_skipped": 0
  },
  "results": [
    {
      "name": "hello",
      "status": "match",
      "before": null,
      "after": null,
      "diff": null,
      "metrics": {
        "pixel_diff": 0,
        "total_pixels": 1,
        "diff_percentage": 0.0,
        "perceptual_distance": 0.0
      },
      "error": null
    }
  ]
}
JSON
exit 0
"#
  };

  fs::write(&diff_renders, script).expect("write stub diff_renders");
  make_executable(&diff_renders);
  diff_renders
}

fn run_fixture_determinism(
  bin_dir: &Path,
  target_dir: &Path,
  fixtures_dir: &Path,
  out_dir: &Path,
  allow_differences: bool,
) -> std::process::Output {
  let mut cmd = Command::new(env!("CARGO_BIN_EXE_xtask"));
  cmd.current_dir(repo_root());
  cmd.env("PATH", prepend_path(bin_dir));
  cmd.env("CARGO_TARGET_DIR", target_dir);
  cmd.arg("fixture-determinism");
  cmd.arg("--no-build");
  if allow_differences {
    cmd.arg("--allow-differences");
  }
  cmd.arg("--fixtures-dir").arg(fixtures_dir);
  cmd.arg("--out-dir").arg(out_dir);
  cmd.args(["--fixtures", "hello"]);
  cmd.args(["--repeat", "2"]);
  cmd.output().expect("run cargo xtask fixture-determinism")
}

#[test]
#[cfg(unix)]
fn fixture_determinism_no_build_writes_report() {
  let temp = tempdir().expect("tempdir");
  let bin_dir = temp.path().join("bin");
  let target_dir = temp.path().join("target");
  fs::create_dir_all(&bin_dir).expect("create stub bin dir");
  fs::create_dir_all(target_dir.join("release")).expect("create stub target dir");

  write_stub_cargo(&bin_dir);
  write_stub_render_fixtures(&target_dir);
  write_stub_diff_renders(&target_dir, false);

  let fixtures_dir = temp.path().join("fixtures");
  fs::create_dir_all(&fixtures_dir).expect("create fixtures dir");
  let out_dir = temp.path().join("out");

  let output = run_fixture_determinism(&bin_dir, &target_dir, &fixtures_dir, &out_dir, false);
  assert!(
    output.status.success(),
    "expected fixture-determinism to succeed.\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );
  assert!(
    out_dir.join("report.html").is_file(),
    "missing report.html in out dir"
  );
  assert!(
    out_dir.join("report.json").is_file(),
    "missing report.json in out dir"
  );
}

#[test]
#[cfg(unix)]
fn fixture_determinism_fails_when_differences_found() {
  let temp = tempdir().expect("tempdir");
  let bin_dir = temp.path().join("bin");
  let target_dir = temp.path().join("target");
  fs::create_dir_all(&bin_dir).expect("create stub bin dir");
  fs::create_dir_all(target_dir.join("release")).expect("create stub target dir");

  write_stub_cargo(&bin_dir);
  write_stub_render_fixtures(&target_dir);
  write_stub_diff_renders(&target_dir, true);

  let fixtures_dir = temp.path().join("fixtures");
  fs::create_dir_all(&fixtures_dir).expect("create fixtures dir");
  let out_dir = temp.path().join("out");

  let output = run_fixture_determinism(&bin_dir, &target_dir, &fixtures_dir, &out_dir, false);
  assert!(
    !output.status.success(),
    "expected fixture-determinism to fail when differences are found.\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );
  assert!(
    out_dir.join("report.html").is_file(),
    "missing report.html in out dir"
  );
  assert!(
    out_dir.join("report.json").is_file(),
    "missing report.json in out dir"
  );
}

#[test]
#[cfg(unix)]
fn fixture_determinism_allow_differences_exits_zero() {
  let temp = tempdir().expect("tempdir");
  let bin_dir = temp.path().join("bin");
  let target_dir = temp.path().join("target");
  fs::create_dir_all(&bin_dir).expect("create stub bin dir");
  fs::create_dir_all(target_dir.join("release")).expect("create stub target dir");

  write_stub_cargo(&bin_dir);
  write_stub_render_fixtures(&target_dir);
  write_stub_diff_renders(&target_dir, true);

  let fixtures_dir = temp.path().join("fixtures");
  fs::create_dir_all(&fixtures_dir).expect("create fixtures dir");
  let out_dir = temp.path().join("out");

  let output = run_fixture_determinism(&bin_dir, &target_dir, &fixtures_dir, &out_dir, true);
  assert!(
    output.status.success(),
    "expected fixture-determinism to exit 0 when --allow-differences is set.\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );
  assert!(
    out_dir.join("report.html").is_file(),
    "missing report.html in out dir"
  );
  assert!(
    out_dir.join("report.json").is_file(),
    "missing report.json in out dir"
  );
}
