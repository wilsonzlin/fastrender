use fastrender::image_compare::{compare_png, CompareConfig};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::TempDir;

fn write_fixture(root: &Path, stem: &str, index_html: &str) -> PathBuf {
  let dir = root.join(stem);
  fs::create_dir_all(&dir).expect("create fixture dir");
  fs::write(dir.join("index.html"), index_html).expect("write index.html");
  dir
}

fn run_render_fixtures(
  working_dir: &Path,
  fixtures_dir: &Path,
  out_dir: &Path,
  fixture: &str,
  write_snapshot: bool,
) -> Result<(), String> {
  let mut cmd = Command::new(env!("CARGO_BIN_EXE_render_fixtures"));
  cmd.current_dir(working_dir);
  cmd.args([
    "--fixtures-dir",
    fixtures_dir
      .to_str()
      .ok_or_else(|| "fixtures_dir is not valid UTF-8".to_string())?,
    "--out-dir",
    out_dir
      .to_str()
      .ok_or_else(|| "out_dir is not valid UTF-8".to_string())?,
    "--fixtures",
    fixture,
    "--viewport",
    "64x64",
    "--jobs",
    "1",
    "--timeout",
    "5",
  ]);
  if write_snapshot {
    cmd.arg("--write-snapshot");
  }
  let output = cmd
    .output()
    .map_err(|e| format!("failed to run render_fixtures: {e}"))?;

  if output.status.success() {
    return Ok(());
  }

  let stdout = String::from_utf8_lossy(&output.stdout);
  let stderr = String::from_utf8_lossy(&output.stderr);
  Err(format!(
    "render_fixtures failed with status {}.\nstdout:\n{}\nstderr:\n{}",
    output.status, stdout, stderr
  ))
}

fn run_diff_snapshots(before: &Path, after: &Path, json: &Path, html: &Path) -> Result<(), String> {
  let mut cmd = Command::new(env!("CARGO_BIN_EXE_diff_snapshots"));
  cmd.args([
    "--before",
    before
      .to_str()
      .ok_or_else(|| "before path is not valid UTF-8".to_string())?,
    "--after",
    after
      .to_str()
      .ok_or_else(|| "after path is not valid UTF-8".to_string())?,
    "--json",
    json
      .to_str()
      .ok_or_else(|| "json path is not valid UTF-8".to_string())?,
    "--html",
    html
      .to_str()
      .ok_or_else(|| "html path is not valid UTF-8".to_string())?,
  ]);
  let output = cmd
    .output()
    .map_err(|e| format!("failed to run diff_snapshots: {e}"))?;

  if output.status.success() {
    return Ok(());
  }

  let stdout = String::from_utf8_lossy(&output.stdout);
  let stderr = String::from_utf8_lossy(&output.stderr);
  Err(format!(
    "diff_snapshots failed with status {}.\nstdout:\n{}\nstderr:\n{}",
    output.status, stdout, stderr
  ))
}

fn clear_dir(path: &Path) -> Result<(), String> {
  if path.exists() {
    fs::remove_dir_all(path)
      .map_err(|e| format!("failed to remove {}: {e}", path.display()))?;
  }
  fs::create_dir_all(path).map_err(|e| format!("failed to create {}: {e}", path.display()))
}

#[test]
fn fixture_renders_are_deterministic_across_processes() {
  let fixture = "determinism_overlay_order";
  let tmp = TempDir::new().expect("tempdir");
  let fixtures_dir = tmp.path().join("fixtures");
  fs::create_dir_all(&fixtures_dir).expect("create fixtures dir");

  // Construct a fixture where paint ordering matters. If any stage relies on hash iteration order,
  // two separate processes (different hash seeds) may paint these semi-transparent layers in a
  // different order and produce different pixels.
  write_fixture(
    &fixtures_dir,
    fixture,
    r#"<!doctype html>
<html>
  <head>
    <meta charset="utf-8">
    <style>
      html, body { margin: 0; width: 64px; height: 64px; background: white; }
      .layer { position: absolute; left: 0; top: 0; width: 64px; height: 64px; opacity: 0.5; }
      #a { background: rgb(255, 0, 0); }
      #b { background: rgb(0, 255, 0); }
      #c { background: rgb(0, 0, 255); }
      #d { background: rgb(255, 255, 0); }
    </style>
  </head>
  <body>
    <div id="a" class="layer"></div>
    <div id="b" class="layer"></div>
    <div id="c" class="layer"></div>
    <div id="d" class="layer"></div>
  </body>
</html>
"#,
  );

  let run1_dir = tmp.path().join("run1");
  let run2_dir = tmp.path().join("run2");

  run_render_fixtures(tmp.path(), &fixtures_dir, &run1_dir, fixture, false)
    .unwrap_or_else(|e| panic!("first render failed: {e}"));
  run_render_fixtures(tmp.path(), &fixtures_dir, &run2_dir, fixture, false)
    .unwrap_or_else(|e| panic!("second render failed: {e}"));

  let run1_png_path = run1_dir.join(format!("{fixture}.png"));
  let run2_png_path = run2_dir.join(format!("{fixture}.png"));
  let run1_png = fs::read(&run1_png_path)
    .unwrap_or_else(|e| panic!("failed to read {}: {e}", run1_png_path.display()));
  let run2_png = fs::read(&run2_png_path)
    .unwrap_or_else(|e| panic!("failed to read {}: {e}", run2_png_path.display()));

  // Compare pixels (not raw PNG bytes) so encoding differences don't produce false positives.
  let check_config = CompareConfig::strict().with_generate_diff_image(false);
  let check = compare_png(&run2_png, &run1_png, &check_config).expect("compare pngs");
  if check.is_match() {
    return;
  }

  let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
  let artifact_root = repo_root
    .join("target")
    .join("determinism_diffs")
    .join(fixture);
  clear_dir(&artifact_root).expect("clear determinism artifacts dir");

  let expected_path = artifact_root.join(format!("{fixture}_expected.png"));
  let actual_path = artifact_root.join(format!("{fixture}_actual.png"));
  fs::write(&expected_path, &run1_png)
    .unwrap_or_else(|e| panic!("failed to write {}: {e}", expected_path.display()));
  fs::write(&actual_path, &run2_png)
    .unwrap_or_else(|e| panic!("failed to write {}: {e}", actual_path.display()));

  let diff_config = CompareConfig::strict();
  let diff = compare_png(&run2_png, &run1_png, &diff_config).expect("compare for diff");
  let diff_summary = diff.summary();
  let diff_path = artifact_root.join(format!("{fixture}_diff.png"));
  let diff_written = match diff.diff_png().expect("encode diff png") {
    Some(bytes) => {
      fs::write(&diff_path, bytes)
        .unwrap_or_else(|e| panic!("failed to write {}: {e}", diff_path.display()));
      true
    }
    None => false,
  };

  let snapshot_run1_out = artifact_root.join("run1");
  let snapshot_run2_out = artifact_root.join("run2");
  let snapshot_run1_dir = snapshot_run1_out.join(fixture);
  let snapshot_run2_dir = snapshot_run2_out.join(fixture);

  let mut snapshot_capture_error = None::<String>;
  if let Err(err) = run_render_fixtures(tmp.path(), &fixtures_dir, &snapshot_run1_out, fixture, true)
  {
    snapshot_capture_error = Some(format!("run1 snapshot capture failed: {err}"));
  } else if let Err(err) =
    run_render_fixtures(tmp.path(), &fixtures_dir, &snapshot_run2_out, fixture, true)
  {
    snapshot_capture_error = Some(format!("run2 snapshot capture failed: {err}"));
  }

  if snapshot_capture_error.is_none() {
    let run1_snapshot = snapshot_run1_dir.join("snapshot.json");
    let run2_snapshot = snapshot_run2_dir.join("snapshot.json");
    let run1_diag = snapshot_run1_dir.join("diagnostics.json");
    let run2_diag = snapshot_run2_dir.join("diagnostics.json");

    if !run1_snapshot.is_file() || !run2_snapshot.is_file() || !run1_diag.is_file() || !run2_diag.is_file()
    {
      snapshot_capture_error = Some(format!(
        "snapshot outputs missing (expected {} + {} and {} + {})",
        run1_snapshot.display(),
        run1_diag.display(),
        run2_snapshot.display(),
        run2_diag.display()
      ));
    }
  }

  let diff_snapshots_json = artifact_root.join("diff_snapshots.json");
  let diff_snapshots_html = artifact_root.join("diff_snapshots.html");

  let diff_snapshots_error = if snapshot_capture_error.is_none() {
    run_diff_snapshots(
      &snapshot_run1_dir,
      &snapshot_run2_dir,
      &diff_snapshots_json,
      &diff_snapshots_html,
    )
    .err()
  } else {
    None
  };

  let mut message = String::new();
  message.push_str(&format!(
    "Fixture '{fixture}' rendered nondeterministically across processes.\n\nPixel diff: {diff_summary}\n\nArtifacts:\n  expected: {}\n  actual:   {}\n",
    expected_path.display(),
    actual_path.display()
  ));
  if diff_written {
    message.push_str(&format!("  diff:     {}\n", diff_path.display()));
  } else {
    message.push_str("  diff:     (not generated; likely dimension mismatch)\n");
  }

  message.push_str("\nSnapshots:\n");
  message.push_str(&format!("  run1: {}\n", snapshot_run1_dir.display()));
  message.push_str(&format!("  run2: {}\n", snapshot_run2_dir.display()));

  message.push_str("\nSnapshot stage diff:\n");
  message.push_str(&format!("  html: {}\n", diff_snapshots_html.display()));
  message.push_str(&format!("  json: {}\n", diff_snapshots_json.display()));

  if let Some(err) = snapshot_capture_error {
    message.push_str(&format!("\nSnapshot capture failed:\n{err}\n"));
  }
  if let Some(err) = diff_snapshots_error {
    message.push_str(&format!("\ndiff_snapshots failed:\n{err}\n"));
  }

  panic!("{message}");
}
