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
    .expect("xtask crate should live under repo root")
    .to_path_buf()
}

fn write_fixture(root: &Path, name: &str) {
  let dir = root.join(name);
  fs::create_dir_all(&dir).expect("create fixture dir");
  fs::write(dir.join("index.html"), "<!doctype html><title>fixture</title>")
    .expect("write fixture html");
}

#[test]
fn dry_run_prints_deterministic_plan_and_forwards_args() {
  let temp = tempdir().expect("tempdir");
  let fixtures_root = temp.path().join("fixtures");
  write_fixture(&fixtures_root, "a");
  write_fixture(&fixtures_root, "b");
  write_fixture(&fixtures_root, "c");
  write_fixture(&fixtures_root, "d");

  let out_dir = temp.path().join("out");
  let chrome_dir = temp.path().join("chrome-bin");
  fs::create_dir_all(&chrome_dir).expect("create chrome-bin dir");
  let fake_chrome = chrome_dir.join("chrome");
  fs::write(&fake_chrome, "#!/usr/bin/env sh\nexit 0\n").expect("write fake chrome");
  make_executable(&fake_chrome);

  let output = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .current_dir(repo_root())
    .args([
      "fixture-chrome-diff",
      "--dry-run",
      "--fixtures-dir",
      fixtures_root.to_string_lossy().as_ref(),
      "--fixtures",
      "a,b,c,d",
      "--shard",
      "1/2",
      "--out-dir",
      out_dir.to_string_lossy().as_ref(),
      "--viewport",
      "800x600",
      "--dpr",
      "2",
      "--tolerance",
      "5",
      "--max-diff-percent",
      "1.5",
      "--chrome-dir",
      chrome_dir.to_string_lossy().as_ref(),
    ])
    .output()
    .expect("run fixture-chrome-diff --dry-run");

  assert!(
    output.status.success(),
    "expected dry-run to succeed; stderr:\n{}",
    String::from_utf8_lossy(&output.stderr)
  );

  let stdout = String::from_utf8_lossy(&output.stdout);

  assert!(
    stdout.contains(&format!("out_dir: {}", out_dir.display())),
    "plan should mention out_dir; got:\n{stdout}"
  );
  assert!(
    stdout.contains(&format!("fastrender: {}", out_dir.join("fastrender").display())),
    "plan should mention fastrender dir; got:\n{stdout}"
  );
  assert!(
    stdout.contains(&format!("chrome: {}", out_dir.join("chrome").display())),
    "plan should mention chrome dir; got:\n{stdout}"
  );
  assert!(
    stdout.contains(&format!("report: {}", out_dir.join("report.html").display())),
    "plan should mention report.html path; got:\n{stdout}"
  );
  assert!(
    stdout.contains(&format!("json: {}", out_dir.join("report.json").display())),
    "plan should mention report.json path; got:\n{stdout}"
  );

  assert!(
    stdout.contains("--bin render_fixtures"),
    "plan should include render_fixtures invocation; got:\n{stdout}"
  );
  assert!(
    stdout.contains(&format!("--fixtures-dir {}", fixtures_root.display())),
    "render_fixtures should receive fixtures-dir; got:\n{stdout}"
  );
  assert!(
    stdout.contains(&format!("--out-dir {}", out_dir.join("fastrender").display())),
    "render_fixtures should receive out-dir; got:\n{stdout}"
  );
  assert!(
    stdout.contains("--viewport 800x600") && stdout.contains("--dpr 2"),
    "render_fixtures should receive viewport and dpr; got:\n{stdout}"
  );
  assert!(
    stdout.contains("--fixtures a,b,c,d") && stdout.contains("--shard 1/2"),
    "render_fixtures should receive fixtures filter and shard; got:\n{stdout}"
  );

  assert!(
    stdout.contains("chrome-baseline-fixtures"),
    "plan should include chrome-baseline-fixtures invocation; got:\n{stdout}"
  );
  assert!(
    stdout.contains(&format!("--out-dir {}", out_dir.join("chrome").display())),
    "chrome-baseline-fixtures should receive out-dir; got:\n{stdout}"
  );
  assert!(
    stdout.contains(&format!("--fixture-dir {}", fixtures_root.display())),
    "chrome-baseline-fixtures should receive fixture-dir; got:\n{stdout}"
  );
  assert!(
    stdout.contains(&format!("--chrome-dir {}", chrome_dir.display())),
    "chrome-baseline-fixtures should receive chrome-dir; got:\n{stdout}"
  );
  assert!(
    stdout.contains("--fixtures a,b,c,d") && stdout.contains("--shard 1/2"),
    "chrome-baseline-fixtures should receive fixtures filter and shard; got:\n{stdout}"
  );

  assert!(
    stdout.contains("--bin diff_renders"),
    "plan should include diff_renders invocation; got:\n{stdout}"
  );
  assert!(
    stdout.contains(&format!("--before {}", out_dir.join("chrome").display()))
      && stdout.contains(&format!("--after {}", out_dir.join("fastrender").display())),
    "diff_renders should diff chrome vs fastrender dirs; got:\n{stdout}"
  );
  assert!(
    stdout.contains(&format!("--html {}", out_dir.join("report.html").display()))
      && stdout.contains(&format!("--json {}", out_dir.join("report.json").display())),
    "diff_renders should write report.html/report.json; got:\n{stdout}"
  );
  assert!(
    stdout.contains("--tolerance 5") && stdout.contains("--max-diff-percent 1.5"),
    "diff_renders should receive tolerance + max-diff-percent; got:\n{stdout}"
  );
}

#[test]
fn dry_run_respects_no_chrome() {
  let temp = tempdir().expect("tempdir");
  let fixtures_root = temp.path().join("fixtures");
  write_fixture(&fixtures_root, "a");
  let out_dir = temp.path().join("out");

  let output = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .current_dir(repo_root())
    .args([
      "fixture-chrome-diff",
      "--dry-run",
      "--no-chrome",
      "--fixtures-dir",
      fixtures_root.to_string_lossy().as_ref(),
      "--fixtures",
      "a",
      "--out-dir",
      out_dir.to_string_lossy().as_ref(),
    ])
    .output()
    .expect("run fixture-chrome-diff --dry-run --no-chrome");

  assert!(
    output.status.success(),
    "expected dry-run to succeed; stderr:\n{}",
    String::from_utf8_lossy(&output.stderr)
  );

  let stdout = String::from_utf8_lossy(&output.stdout);
  assert!(
    !stdout.contains("chrome-baseline-fixtures"),
    "plan should skip chrome-baseline-fixtures when --no-chrome is set; got:\n{stdout}"
  );
  assert!(
    stdout.contains("--bin diff_renders"),
    "plan should still include diff_renders; got:\n{stdout}"
  );
}

#[test]
#[cfg(unix)]
fn end_to_end_runs_with_stub_cargo_and_fake_chrome() {
  let temp = tempdir().expect("tempdir");
  let fixtures_root = temp.path().join("fixtures");
  write_fixture(&fixtures_root, "a");
  write_fixture(&fixtures_root, "b");

  let out_dir = temp.path().join("out");

  let bin_dir = temp.path().join("bin");
  fs::create_dir_all(&bin_dir).expect("create stub bin dir");

  let stub_cargo = bin_dir.join("cargo");
  fs::write(
    &stub_cargo,
    r#"#!/usr/bin/env sh
set -eu

bin=""
prev=""
for arg in "$@"; do
  if [ "$prev" = "--bin" ]; then
    bin="$arg"
    break
  fi
  prev="$arg"
done

while [ "$#" -gt 0 ]; do
  if [ "$1" = "--" ]; then
    shift
    break
  fi
  shift
done

case "$bin" in
  render_fixtures)
    out=""
    fixtures=""
    while [ "$#" -gt 0 ]; do
      case "$1" in
        --out-dir) out="$2"; shift 2;;
        --fixtures) fixtures="$2"; shift 2;;
        *) shift;;
      esac
    done
    mkdir -p "$out"
    IFS=','; for name in $fixtures; do
      [ -n "$name" ] || continue
      echo "PNG" > "$out/$name.png"
    done
    exit 0
    ;;
  diff_renders)
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
    echo "{}" > "$json"
    stem="$(basename "$html")"
    stem="${stem%.*}"
    mkdir -p "$(dirname "$html")/${stem}_files/diffs"
    echo "PNG" > "$(dirname "$html")/${stem}_files/diffs/stub.png"
    echo "1 differences over threshold" >&2
    exit 1
    ;;
  *)
    echo "stub cargo: unsupported --bin $bin" >&2
    exit 2
    ;;
esac
"#,
  )
  .expect("write stub cargo");
  make_executable(&stub_cargo);

  let chrome_dir = temp.path().join("chrome-bin");
  fs::create_dir_all(&chrome_dir).expect("create chrome dir");
  let fake_chrome = chrome_dir.join("chrome");
  fs::write(
    &fake_chrome,
    r#"#!/usr/bin/env sh
set -eu

screenshot=""
for arg in "$@"; do
  case "$arg" in
    --screenshot=*) screenshot="${arg#--screenshot=}";;
  esac
done

if [ -z "$screenshot" ]; then
  echo "missing --screenshot" >&2
  exit 2
fi

mkdir -p "$(dirname "$screenshot")"
echo "PNG" > "$screenshot"
exit 0
"#,
  )
  .expect("write fake chrome");
  make_executable(&fake_chrome);

  let path_var = std::env::var_os("PATH").unwrap_or_default();
  let mut paths = vec![bin_dir];
  paths.extend(std::env::split_paths(&path_var));
  let path = std::env::join_paths(paths).expect("join PATH");

  let output = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .current_dir(repo_root())
    .env("PATH", path)
    .args([
      "fixture-chrome-diff",
      "--fixtures-dir",
      fixtures_root.to_string_lossy().as_ref(),
      "--fixtures",
      "a,b",
      "--out-dir",
      out_dir.to_string_lossy().as_ref(),
      "--viewport",
      "800x600",
      "--dpr",
      "1",
      "--tolerance",
      "0",
      "--max-diff-percent",
      "0",
      "--chrome-dir",
      chrome_dir.to_string_lossy().as_ref(),
    ])
    .output()
    .expect("run fixture-chrome-diff with stubs");

  assert!(
    output.status.success(),
    "fixture-chrome-diff should exit 0.\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );

  assert!(out_dir.join("report.html").is_file(), "missing report.html");
  assert!(out_dir.join("report.json").is_file(), "missing report.json");
  assert!(
    out_dir.join("fastrender").join("a.png").is_file(),
    "missing fastrender a.png"
  );
  assert!(
    out_dir.join("chrome").join("a.png").is_file(),
    "missing chrome a.png"
  );
  assert!(
    out_dir.join("report_files").join("diffs").is_dir(),
    "missing diff artifacts dir"
  );
}

