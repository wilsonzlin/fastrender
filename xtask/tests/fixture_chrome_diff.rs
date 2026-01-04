use std::fs;
use std::path::PathBuf;
use std::process::Command;

#[cfg(unix)]
use std::os::unix::fs::PermissionsExt;

#[test]
fn fixture_chrome_diff_skips_chrome_with_chrome_dir() {
  let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .parent()
    .expect("xtask manifest should live in repo root/xtask")
    .to_path_buf();

  let tmp = tempfile::TempDir::new().expect("tempdir");
  let fixtures_root = tmp.path().join("fixtures");
  let fixture_name = "minimal_fixture";
  let fixture_dir = fixtures_root.join(fixture_name);
  fs::create_dir_all(&fixture_dir).expect("create fixture dir");
  fs::write(
    fixture_dir.join("index.html"),
    r#"<!doctype html>
<html>
  <head>
    <meta charset="utf-8">
    <style>
      body { margin: 0; background: white; }
      .box { width: 200px; height: 100px; background: rgb(200, 30, 30); }
    </style>
  </head>
  <body>
    <div class="box"></div>
  </body>
</html>
"#,
  )
  .expect("write fixture html");

  // Produce "after" renders with the render_fixtures binary.
  let after_dir = tmp.path().join("after");
  fs::create_dir_all(&after_dir).expect("create after dir");
  let viewport = "1200x800";
  let status = Command::new("cargo")
    .current_dir(&repo_root)
    .args([
      "run",
      "--bin",
      "render_fixtures",
      "--",
      "--fixtures-dir",
    ])
    .arg(&fixtures_root)
    .args([
      "--fixtures",
      fixture_name,
      "--out-dir",
      after_dir.to_str().unwrap(),
      "--viewport",
      viewport,
      "--dpr",
      "1.0",
      "--timeout",
      "5",
    ])
    .status()
    .expect("run render_fixtures");
  assert!(status.success(), "render_fixtures should exit 0");

  let baseline_dir = tmp.path().join("chrome");
  fs::create_dir_all(&baseline_dir).expect("create baseline dir");
  fs::copy(
    after_dir.join(format!("{fixture_name}.png")),
    baseline_dir.join(format!("{fixture_name}.png")),
  )
  .expect("copy baseline png");

  let out_dir = tmp.path().join("out");
  let status = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .current_dir(&repo_root)
    .arg("fixture-chrome-diff")
    .arg("--fixtures-root")
    .arg(&fixtures_root)
    .arg("--only")
    .arg(fixture_name)
    .arg("--chrome-dir")
    .arg(&baseline_dir)
    .arg("--out")
    .arg(&out_dir)
    .arg("--viewport")
    .arg(viewport)
    .arg("--dpr")
    .arg("1.0")
    .arg("--render-timeout")
    .arg("5")
    .arg("--tolerance")
    .arg("0")
    .arg("--max-diff-percent")
    .arg("0.0")
    .status()
    .expect("run xtask fixture-chrome-diff");

  assert!(status.success(), "fixture-chrome-diff should exit 0");
  assert!(out_dir.join("report.html").exists(), "missing report.html");
  assert!(out_dir.join("report.json").exists(), "missing report.json");
}
#[test]
#[cfg(unix)]
fn fixture_chrome_diff_invokes_chrome_baseline_with_out_dir_flag() {
  let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .parent()
    .expect("xtask manifest should live in repo root/xtask")
    .to_path_buf();

  let tmp = tempfile::TempDir::new().expect("tempdir");
  let fixtures_root = tmp.path().join("fixtures");
  let fixture_name = "minimal_fixture";
  let fixture_dir = fixtures_root.join(fixture_name);
  fs::create_dir_all(&fixture_dir).expect("create fixture dir");
  fs::write(
    fixture_dir.join("index.html"),
    r#"<!doctype html>
<html>
  <head>
    <meta charset="utf-8">
    <style>
      body { margin: 0; background: white; }
      .box { width: 200px; height: 100px; background: rgb(200, 30, 30); }
    </style>
  </head>
  <body>
    <div class="box"></div>
  </body>
</html>
"#,
  )
  .expect("write fixture html");

  let fake_bin_dir = tmp.path().join("bin");
  fs::create_dir_all(&fake_bin_dir).expect("create fake bin dir");

  // Intercepts `cargo` invocations from `fixture-chrome-diff` so we can assert that the chrome
  // baseline step uses the correct `--out-dir` flag without relying on a real Chrome binary.
  let fake_cargo = fake_bin_dir.join("cargo");
  fs::write(
    &fake_cargo,
    r#"#!/usr/bin/env bash
set -euo pipefail

case "${1:-}" in
  run)
    # cargo run --bin <bin> -- <args...>
    bin=""
    out=""
    json=""
    html=""
    prev=""
    for arg in "$@"; do
      if [[ "${prev}" == "--bin" ]]; then
        bin="${arg}"
      fi
      prev="${arg}"
    done
    if [[ "${bin}" == "render_fixtures" ]]; then
      # Create a dummy PNG so downstream steps have something to point at.
      prev=""
      for arg in "$@"; do
        if [[ "${prev}" == "--out-dir" ]]; then
          out="${arg}"
        fi
        prev="${arg}"
      done
      if [[ -z "${out}" ]]; then
        echo "fake cargo: missing --out-dir for render_fixtures" >&2
        exit 2
      fi
      mkdir -p "${out}"
      printf 'dummy' > "${out}/minimal_fixture.png"
      exit 0
    fi
    if [[ "${bin}" == "diff_renders" ]]; then
      prev=""
      for arg in "$@"; do
        if [[ "${prev}" == "--json" ]]; then
          json="${arg}"
        fi
        if [[ "${prev}" == "--html" ]]; then
          html="${arg}"
        fi
        prev="${arg}"
      done
      if [[ -z "${json}" || -z "${html}" ]]; then
        echo "fake cargo: missing --json/--html for diff_renders" >&2
        exit 2
      fi
      mkdir -p "$(dirname "${json}")"
      mkdir -p "$(dirname "${html}")"
      printf '{}' > "${json}"
      printf '<!doctype html><title>dummy</title>' > "${html}"
      exit 0
    fi
    echo "fake cargo: unexpected cargo run invocation: $*" >&2
    exit 2
    ;;
  xtask)
    # cargo xtask chrome-baseline-fixtures ...
    if [[ "${2:-}" != "chrome-baseline-fixtures" ]]; then
      echo "fake cargo: unexpected xtask invocation: $*" >&2
      exit 2
    fi
    out_dir=""
    prev=""
    for arg in "$@"; do
      if [[ "${prev}" == "--out-dir" ]]; then
        out_dir="${arg}"
      fi
      prev="${arg}"
    done
    if [[ -z "${out_dir}" ]]; then
      echo "fake cargo: expected --out-dir for chrome-baseline-fixtures" >&2
      exit 2
    fi
    mkdir -p "${out_dir}"
    printf 'dummy' > "${out_dir}/minimal_fixture.png"
    printf 'log' > "${out_dir}/minimal_fixture.chrome.log"
    exit 0
    ;;
  *)
    echo "fake cargo: unexpected invocation: $*" >&2
    exit 2
    ;;
esac
"#,
  )
  .expect("write fake cargo script");
  let mut perms = fs::metadata(&fake_cargo)
    .expect("stat fake cargo")
    .permissions();
  perms.set_mode(0o755);
  fs::set_permissions(&fake_cargo, perms).expect("chmod fake cargo");

  let out_dir = tmp.path().join("out");
  let status = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .current_dir(&repo_root)
    .env(
      "PATH",
      format!(
        "{}:{}",
        fake_bin_dir.display(),
        std::env::var("PATH").unwrap_or_default()
      ),
    )
    .arg("fixture-chrome-diff")
    .arg("--fixtures-root")
    .arg(&fixtures_root)
    .arg("--only")
    .arg(fixture_name)
    .arg("--out")
    .arg(&out_dir)
    .arg("--dpr")
    .arg("1.0")
    .arg("--render-timeout")
    .arg("5")
    .arg("--timeout")
    .arg("5")
    .arg("--tolerance")
    .arg("0")
    .arg("--max-diff-percent")
    .arg("100.0")
    .status()
    .expect("run xtask fixture-chrome-diff");

  assert!(
    status.success(),
    "fixture-chrome-diff should exit 0 (status={status})"
  );
  assert!(out_dir.join("report.html").exists(), "missing report.html");
  assert!(out_dir.join("report.json").exists(), "missing report.json");
  assert!(
    out_dir.join("chrome").join(format!("{fixture_name}.png")).exists(),
    "missing chrome baseline PNG"
  );
  assert!(
    out_dir
      .join("chrome")
      .join(format!("{fixture_name}.chrome.log"))
      .exists(),
    "missing chrome baseline log"
  );
}
