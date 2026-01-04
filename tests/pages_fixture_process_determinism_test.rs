mod r#ref;

use r#ref::compare::{compare_images, load_png_from_bytes, CompareConfig};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::TempDir;

fn manifest_dir() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn artifacts_root() -> PathBuf {
  manifest_dir().join("target/determinism_diffs")
}

fn run_render_fixtures(fixtures: &[&str], out_dir: &Path) {
  let fixtures_arg = fixtures.join(",");
  let status = Command::new(env!("CARGO_BIN_EXE_render_fixtures"))
    .current_dir(manifest_dir())
    .args([
      "--fixtures",
      &fixtures_arg,
      "--jobs",
      "1",
      "--out-dir",
      out_dir.to_str().expect("out-dir utf8"),
      "--timeout",
      "10",
      "--viewport",
      "600x600",
    ])
    .status()
    .expect("spawn render_fixtures");

  assert!(
    status.success(),
    "render_fixtures failed for fixtures [{fixtures_arg}] (out_dir={})",
    out_dir.display()
  );
}

fn run_render_fixture_with_snapshot(stem: &str, out_dir: &Path) {
  let status = Command::new(env!("CARGO_BIN_EXE_render_fixtures"))
    .current_dir(manifest_dir())
    .args([
      "--fixtures",
      stem,
      "--jobs",
      "1",
      "--out-dir",
      out_dir.to_str().expect("out-dir utf8"),
      "--timeout",
      "10",
      "--viewport",
      "600x600",
      "--write-snapshot",
    ])
    .status()
    .expect("spawn render_fixtures --write-snapshot");

  assert!(
    status.success(),
    "render_fixtures --write-snapshot failed for fixture {stem} (out_dir={})",
    out_dir.display()
  );
}

fn run_diff_snapshots(before_dir: &Path, after_dir: &Path, out_dir: &Path) {
  let json_path = out_dir.join("diff_snapshots.json");
  let html_path = out_dir.join("diff_snapshots.html");
  let status = Command::new(env!("CARGO_BIN_EXE_diff_snapshots"))
    .current_dir(manifest_dir())
    .args([
      "--before",
      before_dir.to_str().expect("before-dir utf8"),
      "--after",
      after_dir.to_str().expect("after-dir utf8"),
      "--json",
      json_path.to_str().expect("json path utf8"),
      "--html",
      html_path.to_str().expect("html path utf8"),
    ])
    .status()
    .expect("spawn diff_snapshots");

  assert!(
    status.success(),
    "diff_snapshots failed (before={}, after={})",
    before_dir.display(),
    after_dir.display()
  );
}

fn compare_fixture_png(stem: &str, run1_dir: &Path, run2_dir: &Path) {
  let png1_path = run1_dir.join(format!("{stem}.png"));
  let png2_path = run2_dir.join(format!("{stem}.png"));
  let png1 = fs::read(&png1_path)
    .unwrap_or_else(|e| panic!("failed to read {}: {e}", png1_path.display()));
  let png2 = fs::read(&png2_path)
    .unwrap_or_else(|e| panic!("failed to read {}: {e}", png2_path.display()));

  let pix1 = load_png_from_bytes(&png1)
    .unwrap_or_else(|e| panic!("failed to decode {}: {e}", png1_path.display()));
  let pix2 = load_png_from_bytes(&png2)
    .unwrap_or_else(|e| panic!("failed to decode {}: {e}", png2_path.display()));

  if pix1.width() == pix2.width() && pix1.height() == pix2.height() && pix1.data() == pix2.data() {
    return;
  }

  let diff = compare_images(&pix1, &pix2, &CompareConfig::strict());

  let artifact_dir = artifacts_root().join(stem);
  if artifact_dir.exists() {
    fs::remove_dir_all(&artifact_dir)
      .unwrap_or_else(|e| panic!("failed to remove {}: {e}", artifact_dir.display()));
  }
  fs::create_dir_all(&artifact_dir)
    .unwrap_or_else(|e| panic!("failed to create {}: {e}", artifact_dir.display()));

  let expected_out = artifact_dir.join(format!("{stem}_expected.png"));
  let actual_out = artifact_dir.join(format!("{stem}_actual.png"));
  let diff_out = artifact_dir.join(format!("{stem}_diff.png"));

  fs::write(&expected_out, &png1)
    .unwrap_or_else(|e| panic!("failed to write {}: {e}", expected_out.display()));
  fs::write(&actual_out, &png2)
    .unwrap_or_else(|e| panic!("failed to write {}: {e}", actual_out.display()));
  diff
    .save_diff_image(&diff_out)
    .unwrap_or_else(|e| panic!("failed to write {}: {e}", diff_out.display()));

  // Capture pipeline snapshots and generate a stage-level snapshot diff report to make
  // process-level nondeterminism actionable.
  let snapshot_run1_out = artifact_dir.join("run1");
  let snapshot_run2_out = artifact_dir.join("run2");
  fs::create_dir_all(&snapshot_run1_out).expect("create snapshot run1 dir");
  fs::create_dir_all(&snapshot_run2_out).expect("create snapshot run2 dir");

  run_render_fixture_with_snapshot(stem, &snapshot_run1_out);
  run_render_fixture_with_snapshot(stem, &snapshot_run2_out);

  let snapshot_before_dir = snapshot_run1_out.join(stem);
  let snapshot_after_dir = snapshot_run2_out.join(stem);
  assert!(
    snapshot_before_dir.join("snapshot.json").is_file(),
    "missing snapshot.json in {}",
    snapshot_before_dir.display()
  );
  assert!(
    snapshot_after_dir.join("snapshot.json").is_file(),
    "missing snapshot.json in {}",
    snapshot_after_dir.display()
  );
  assert!(
    snapshot_before_dir.join("diagnostics.json").is_file(),
    "missing diagnostics.json in {}",
    snapshot_before_dir.display()
  );
  assert!(
    snapshot_after_dir.join("diagnostics.json").is_file(),
    "missing diagnostics.json in {}",
    snapshot_after_dir.display()
  );

  // Make diff_snapshots link the exact pixel diff artifacts.
  fs::copy(&expected_out, snapshot_before_dir.join("render.png"))
    .expect("copy expected render.png");
  fs::copy(&actual_out, snapshot_after_dir.join("render.png")).expect("copy actual render.png");

  run_diff_snapshots(&snapshot_before_dir, &snapshot_after_dir, &artifact_dir);

  eprintln!(
    "Process-level nondeterminism detected for fixture '{stem}': {:.4}% pixels differ.\n  expected: {}\n  actual:   {}\n  diff:     {}\n  snapshot run1: {}\n  snapshot run2: {}\n  diff_snapshots: {}\n  {}",
    diff.statistics.different_percent,
    expected_out.display(),
    actual_out.display(),
    diff_out.display(),
    snapshot_before_dir.display(),
    snapshot_after_dir.display(),
    artifact_dir.join("diff_snapshots.html").display(),
    diff.summary(),
  );

  panic!("fixture '{stem}' rendered differently across processes");
}

#[test]
fn pages_fixture_process_determinism_test() {
  let fixtures = ["preserve_3d_stack", "filter_backdrop_scene"];

  let run1 = TempDir::new().expect("tempdir run1");
  let run2 = TempDir::new().expect("tempdir run2");

  run_render_fixtures(&fixtures, run1.path());
  run_render_fixtures(&fixtures, run2.path());

  for stem in fixtures {
    compare_fixture_png(stem, run1.path(), run2.path());
  }
}
