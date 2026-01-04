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
    .expect("xtask crate should live under repo root")
    .to_path_buf()
}

fn write_fixture(root: &Path, name: &str) {
  let dir = root.join(name);
  fs::create_dir_all(&dir).expect("create fixture dir");
  fs::write(
    dir.join("index.html"),
    "<!doctype html><title>fixture</title>",
  )
  .expect("write fixture html");
}

fn write_stub_diff_renders(target_dir: &Path) -> PathBuf {
  let bin = target_dir
    .join("release")
    .join(format!("diff_renders{}", std::env::consts::EXE_SUFFIX));
  fs::create_dir_all(bin.parent().expect("release dir")).expect("create diff_renders release dir");
  fs::write(
    &bin,
    "#!/usr/bin/env sh\necho 'error: diff_renders executed unexpectedly' >&2\nexit 1\n",
  )
  .expect("write stub diff_renders");
  make_executable(&bin);
  bin
}

#[test]
#[cfg(unix)]
fn no_fastrender_fails_on_mismatched_metadata() {
  let temp = tempdir().expect("tempdir");
  let fixtures_root = temp.path().join("fixtures");
  write_fixture(&fixtures_root, "a");

  let out_dir = temp.path().join("out");
  let fastrender_dir = out_dir.join("fastrender");
  fs::create_dir_all(&fastrender_dir).expect("create fastrender out dir");
  fs::create_dir_all(out_dir.join("chrome")).expect("create chrome out dir");

  // Ensure we don't accidentally try to build or run the real diff_renders binary if the
  // metadata validation regresses.
  let target_dir = temp.path().join("target");
  let diff_renders_exe = target_dir.join("release").join(format!(
    "diff_renders{}",
    std::env::consts::EXE_SUFFIX
  ));
  fs::create_dir_all(diff_renders_exe.parent().unwrap()).expect("create target/release dir");
  fs::write(&diff_renders_exe, "#!/usr/bin/env sh\nexit 0\n").expect("write stub diff_renders");
  make_executable(&diff_renders_exe);

  fs::write(fastrender_dir.join("a.png"), "PNG").expect("write placeholder png");
  fs::write(
    fastrender_dir.join("a.json"),
    r#"{
  "fixture": "a",
  "viewport": [123, 456],
  "dpr": 2.0,
  "media": "print",
  "timeout_secs": 99,
  "bundled_fonts": true,
  "font_dirs": [],
  "status": "ok",
  "elapsed_ms": 1
}"#,
  )
  .expect("write mismatched metadata");

  let output = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .current_dir(repo_root())
    .env("CARGO_TARGET_DIR", &target_dir)
    .args([
      "fixture-chrome-diff",
      "--no-build",
      "--no-chrome",
      "--no-fastrender",
      "--fixtures-dir",
      fixtures_root.to_string_lossy().as_ref(),
      "--fixtures",
      "a",
      "--out-dir",
      out_dir.to_string_lossy().as_ref(),
      "--viewport",
      "800x600",
      "--dpr",
      "1",
      "--media",
      "screen",
      "--timeout",
      "15",
    ])
    .output()
    .expect("run fixture-chrome-diff with mismatched metadata");

  assert!(
    !output.status.success(),
    "expected fixture-chrome-diff to fail for mismatched metadata.\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );

  let stderr = String::from_utf8_lossy(&output.stderr);
  assert!(
    stderr.contains("FastRender metadata mismatch"),
    "expected error message to mention metadata mismatch; got:\n{stderr}"
  );
  assert!(
    stderr.contains("--no-fastrender"),
    "expected error message to instruct about rerunning without --no-fastrender; got:\n{stderr}"
  );
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
  let target_dir = temp.path().join("target");
  let chrome_dir = temp.path().join("chrome-bin");
  fs::create_dir_all(&chrome_dir).expect("create chrome-bin dir");
  let fake_chrome = chrome_dir.join("chrome");
  fs::write(&fake_chrome, "#!/usr/bin/env sh\nexit 0\n").expect("write fake chrome");
  make_executable(&fake_chrome);

  let output = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .current_dir(repo_root())
    .env("CARGO_TARGET_DIR", &target_dir)
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
      "--jobs",
      "3",
      "--write-snapshot",
      "--media",
      "print",
      "--fit-canvas-to-content",
      "--timeout",
      "12",
      "--js",
      "on",
      "--tolerance",
      "5",
      "--max-diff-percent",
      "1.5",
      "--max-perceptual-distance",
      "0.4",
      "--sort-by",
      "perceptual",
      "--ignore-alpha",
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
    stdout.contains(&format!(
      "fastrender: {}",
      out_dir.join("fastrender").display()
    )),
    "plan should mention fastrender dir; got:\n{stdout}"
  );
  assert!(
    stdout.contains(&format!("chrome: {}", out_dir.join("chrome").display())),
    "plan should mention chrome dir; got:\n{stdout}"
  );
  assert!(
    stdout.contains(&format!(
      "report: {}",
      out_dir.join("report.html").display()
    )),
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
  let render_line = stdout
    .lines()
    .find(|line| line.contains("--bin render_fixtures"))
    .expect("render_fixtures command line should be printed");
  assert!(
    render_line.contains("--timeout 12"),
    "render_fixtures should receive timeout; got:\n{render_line}"
  );
  assert!(
    render_line.contains("--jobs 3"),
    "render_fixtures should receive jobs; got:\n{render_line}"
  );
  assert!(
    render_line.contains("--write-snapshot"),
    "render_fixtures should receive write-snapshot; got:\n{render_line}"
  );
  assert!(
    render_line.contains("--media print"),
    "render_fixtures should receive media; got:\n{render_line}"
  );
  assert!(
    render_line.contains("--fit-canvas-to-content"),
    "render_fixtures should receive fit-canvas-to-content; got:\n{render_line}"
  );
  assert!(
    stdout.contains(&format!("--fixtures-dir {}", fixtures_root.display())),
    "render_fixtures should receive fixtures-dir; got:\n{stdout}"
  );
  assert!(
    stdout.contains(&format!(
      "--out-dir {}",
      out_dir.join("fastrender").display()
    )),
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
  let chrome_line = stdout
    .lines()
    .find(|line| line.contains("chrome-baseline-fixtures"))
    .expect("chrome-baseline-fixtures command line should be printed");
  assert!(
    chrome_line.contains("--timeout 12"),
    "chrome-baseline-fixtures should receive timeout; got:\n{chrome_line}"
  );
  assert!(
    chrome_line.contains("--media print"),
    "chrome-baseline-fixtures should receive media; got:\n{chrome_line}"
  );
  assert!(
    chrome_line.contains("--js on"),
    "chrome-baseline-fixtures should receive js mode; got:\n{chrome_line}"
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

  let diff_renders_bin = target_dir
    .join("release")
    .join(format!("diff_renders{}", std::env::consts::EXE_SUFFIX));
  assert!(
    stdout.contains(&diff_renders_bin.display().to_string()),
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
    stdout.contains("--tolerance 5")
      && stdout.contains("--max-diff-percent 1.5")
      && stdout.contains("--ignore-alpha"),
    "diff_renders should receive tolerance + max-diff-percent + ignore-alpha; got:\n{stdout}"
  );
  assert!(
    stdout.contains("--max-perceptual-distance 0.4") && stdout.contains("--sort-by perceptual"),
    "diff_renders should receive max-perceptual-distance + sort-by; got:\n{stdout}"
  );
}

#[test]
fn dry_run_respects_no_chrome() {
  let temp = tempdir().expect("tempdir");
  let fixtures_root = temp.path().join("fixtures");
  write_fixture(&fixtures_root, "a");
  let out_dir = temp.path().join("out");
  let target_dir = temp.path().join("target");

  let output = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .current_dir(repo_root())
    .env("CARGO_TARGET_DIR", &target_dir)
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
  let diff_renders_bin = target_dir
    .join("release")
    .join(format!("diff_renders{}", std::env::consts::EXE_SUFFIX));
  assert!(
    stdout.contains(&diff_renders_bin.display().to_string()),
    "plan should still include diff_renders; got:\n{stdout}"
  );
}

#[test]
fn dry_run_respects_no_fastrender() {
  let temp = tempdir().expect("tempdir");
  let fixtures_root = temp.path().join("fixtures");
  write_fixture(&fixtures_root, "a");
  let out_dir = temp.path().join("out");

  let output = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .current_dir(repo_root())
    .args([
      "fixture-chrome-diff",
      "--dry-run",
      "--no-fastrender",
      "--fixtures-dir",
      fixtures_root.to_string_lossy().as_ref(),
      "--fixtures",
      "a",
      "--out-dir",
      out_dir.to_string_lossy().as_ref(),
    ])
    .output()
    .expect("run fixture-chrome-diff --dry-run --no-fastrender");

  assert!(
    output.status.success(),
    "expected dry-run to succeed; stderr:\n{}",
    String::from_utf8_lossy(&output.stderr)
  );

  let stdout = String::from_utf8_lossy(&output.stdout);
  assert!(
    !stdout.contains("--bin render_fixtures"),
    "plan should skip render_fixtures when --no-fastrender is set; got:\n{stdout}"
  );
  assert!(
    stdout.contains("diff_renders") && stdout.contains("--before"),
    "plan should still include diff_renders; got:\n{stdout}"
  );
}

#[test]
fn dry_run_respects_diff_only_alias() {
  let temp = tempdir().expect("tempdir");
  let fixtures_root = temp.path().join("fixtures");
  write_fixture(&fixtures_root, "a");
  let out_dir = temp.path().join("out");

  let output = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .current_dir(repo_root())
    .args([
      "fixture-chrome-diff",
      "--dry-run",
      "--diff-only",
      "--fixtures-dir",
      fixtures_root.to_string_lossy().as_ref(),
      "--fixtures",
      "a",
      "--out-dir",
      out_dir.to_string_lossy().as_ref(),
    ])
    .output()
    .expect("run fixture-chrome-diff --dry-run --diff-only");

  assert!(
    output.status.success(),
    "expected dry-run to succeed; stderr:\n{}",
    String::from_utf8_lossy(&output.stderr)
  );

  let stdout = String::from_utf8_lossy(&output.stdout);
  assert!(
    !stdout.contains("--bin render_fixtures"),
    "plan should skip render_fixtures when --diff-only is set; got:\n{stdout}"
  );
  assert!(
    !stdout.contains("chrome-baseline-fixtures"),
    "plan should skip chrome-baseline-fixtures when --diff-only is set; got:\n{stdout}"
  );
  assert!(
    stdout.contains("diff_renders") && stdout.contains("--before"),
    "plan should still include diff_renders; got:\n{stdout}"
  );
}

#[test]
fn no_chrome_fails_on_baseline_metadata_mismatch() {
  let temp = tempdir().expect("tempdir");
  let fixtures_root = temp.path().join("fixtures");
  write_fixture(&fixtures_root, "a");

  let out_dir = temp.path().join("out");
  let chrome_dir = out_dir.join("chrome");
  let fastrender_dir = out_dir.join("fastrender");
  fs::create_dir_all(&chrome_dir).expect("create chrome dir");
  fs::create_dir_all(&fastrender_dir).expect("create fastrender dir");

  fs::write(chrome_dir.join("a.png"), b"PNG").expect("write chrome png");
  fs::write(fastrender_dir.join("a.png"), b"PNG").expect("write fastrender png");
  fs::write(
    chrome_dir.join("a.json"),
    r#"{"viewport":[1040,1240],"dpr":2.0,"js":"on"}"#,
  )
  .expect("write chrome metadata");

  let target_dir = temp.path().join("target");
  write_stub_diff_renders(&target_dir);

  let output = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .current_dir(repo_root())
    .env("CARGO_TARGET_DIR", &target_dir)
    .args([
      "fixture-chrome-diff",
      "--no-chrome",
      "--no-fastrender",
      "--no-build",
      "--fixtures-dir",
      fixtures_root.to_string_lossy().as_ref(),
      "--fixtures",
      "a",
      "--out-dir",
      out_dir.to_string_lossy().as_ref(),
      "--viewport",
      "800x600",
      "--dpr",
      "1",
    ])
    .output()
    .expect("run fixture-chrome-diff with mismatched metadata");

  assert!(
    !output.status.success(),
    "expected fixture-chrome-diff to fail on baseline mismatch; stdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );

  let stderr = String::from_utf8_lossy(&output.stderr);
  assert!(
    stderr.contains("chrome baseline mismatch") && stderr.contains("Rerun without --no-chrome"),
    "expected stderr to mention baseline mismatch and remediation; got:\n{stderr}"
  );
}

#[test]
fn require_chrome_metadata_errors_when_missing() {
  let temp = tempdir().expect("tempdir");
  let fixtures_root = temp.path().join("fixtures");
  write_fixture(&fixtures_root, "a");

  let out_dir = temp.path().join("out");
  let chrome_dir = out_dir.join("chrome");
  let fastrender_dir = out_dir.join("fastrender");
  fs::create_dir_all(&chrome_dir).expect("create chrome dir");
  fs::create_dir_all(&fastrender_dir).expect("create fastrender dir");
  fs::write(chrome_dir.join("a.png"), b"PNG").expect("write chrome png");
  fs::write(fastrender_dir.join("a.png"), b"PNG").expect("write fastrender png");

  let target_dir = temp.path().join("target");
  write_stub_diff_renders(&target_dir);

  let output = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .current_dir(repo_root())
    .env("CARGO_TARGET_DIR", &target_dir)
    .args([
      "fixture-chrome-diff",
      "--no-chrome",
      "--no-fastrender",
      "--no-build",
      "--require-chrome-metadata",
      "--fixtures-dir",
      fixtures_root.to_string_lossy().as_ref(),
      "--fixtures",
      "a",
      "--out-dir",
      out_dir.to_string_lossy().as_ref(),
    ])
    .output()
    .expect("run fixture-chrome-diff with missing metadata");

  assert!(
    !output.status.success(),
    "expected fixture-chrome-diff to fail when metadata is required but missing; stdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );

  let stderr = String::from_utf8_lossy(&output.stderr);
  assert!(
    stderr.contains("missing chrome baseline metadata"),
    "expected stderr to mention missing chrome baseline metadata; got:\n{stderr}"
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
  let target_dir = temp.path().join("target");

  let bin_dir = temp.path().join("bin");
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

# The real `fixture-chrome-diff` builds the `diff_renders` binary and then executes it directly.
# Emulate that by writing a stub `diff_renders` executable into the target dir when we see a
# `cargo build --bin diff_renders` invocation.
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
echo "{}" > "$json"
stem="$(basename "$html")"
stem="${stem%.*}"
mkdir -p "$(dirname "$html")/${stem}_files/diffs"
echo "PNG" > "$(dirname "$html")/${stem}_files/diffs/stub.png"
echo "1 differences over threshold" >&2
exit 1
SH
  chmod +x "$out"
  exit 0
fi

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
    .env("PATH", &path)
    .env("CARGO_TARGET_DIR", &target_dir)
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

  let output_reuse = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .current_dir(repo_root())
    .env("PATH", &path)
    .env("CARGO_TARGET_DIR", &target_dir)
    .args([
      "fixture-chrome-diff",
      "--no-chrome",
      "--no-fastrender",
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
    ])
    .output()
    .expect("run fixture-chrome-diff with --no-chrome --no-fastrender");

  assert!(
    output_reuse.status.success(),
    "fixture-chrome-diff should exit 0 when reusing outputs.\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output_reuse.stdout),
    String::from_utf8_lossy(&output_reuse.stderr)
  );

  let output_fail = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .current_dir(repo_root())
    .env("PATH", &path)
    .env("CARGO_TARGET_DIR", &target_dir)
    .args([
      "fixture-chrome-diff",
      "--fail-on-differences",
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
    .expect("run fixture-chrome-diff with --fail-on-differences");

  assert!(
    !output_fail.status.success(),
    "fixture-chrome-diff should exit non-zero when --fail-on-differences is set.\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output_fail.stdout),
    String::from_utf8_lossy(&output_fail.stderr)
  );
  let stderr = String::from_utf8_lossy(&output_fail.stderr);
  assert!(
    stderr.contains("diff_renders reported differences"),
    "expected stderr to mention diff_renders differences; got:\n{stderr}"
  );
  assert!(out_dir.join("report.html").is_file(), "missing report.html");
  assert!(out_dir.join("report.json").is_file(), "missing report.json");
}
