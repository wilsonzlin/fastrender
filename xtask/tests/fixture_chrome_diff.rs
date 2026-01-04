use std::fs;
use std::path::PathBuf;
use std::process::Command;

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
      "--fixtures-root",
    ])
    .arg(&fixtures_root)
    .args([
      "--only",
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
