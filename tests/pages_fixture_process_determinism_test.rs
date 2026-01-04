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
  fs::create_dir_all(&artifact_dir)
    .unwrap_or_else(|e| panic!("failed to create {}: {e}", artifact_dir.display()));

  let run1_out = artifact_dir.join(format!("{stem}_run1.png"));
  let run2_out = artifact_dir.join(format!("{stem}_run2.png"));
  let diff_out = artifact_dir.join(format!("{stem}_diff.png"));

  fs::write(&run1_out, &png1).unwrap_or_else(|e| panic!("failed to write {}: {e}", run1_out.display()));
  fs::write(&run2_out, &png2).unwrap_or_else(|e| panic!("failed to write {}: {e}", run2_out.display()));
  diff
    .save_diff_image(&diff_out)
    .unwrap_or_else(|e| panic!("failed to write {}: {e}", diff_out.display()));

  eprintln!(
    "Process-level nondeterminism detected for fixture '{stem}': {:.4}% pixels differ.\n  run1: {}\n  run2: {}\n  diff: {}\n  {}",
    diff.statistics.different_percent,
    run1_out.display(),
    run2_out.display(),
    diff_out.display(),
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

