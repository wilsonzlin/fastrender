use std::path::PathBuf;
use std::process::Command;

use tempfile::TempDir;

fn repo_root() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .parent()
    .expect("xtask crate should live under the workspace root")
    .to_path_buf()
}

#[test]
fn import_page_fixture_resolves_relative_paths_from_repo_root() {
  let repo_root = repo_root();

  // Use a checked-in bundle fixture, but pass the bundle path as a relative path to ensure the
  // importer resolves it relative to the repository root even when run from another directory.
  let bundle_rel = PathBuf::from("tests/fixtures/bundle_page/simple");

  let temp: TempDir = tempfile::Builder::new()
    .prefix("import-page-fixture-paths.")
    .tempdir_in(repo_root.join("target"))
    .expect("tempdir under target/");
  let output_root_abs = temp.path().join("fixtures");
  let output_root_rel = output_root_abs
    .strip_prefix(&repo_root)
    .expect("tempdir should be under repo root")
    .to_path_buf();

  let fixture_name = "synthetic_bundle_paths";

  let status = Command::new(env!("CARGO_BIN_EXE_xtask"))
    // Run the xtask binary from the xtask crate directory (not the repo root) to ensure relative
    // paths are still interpreted as repo-root relative.
    .current_dir(repo_root.join("xtask"))
    .arg("import-page-fixture")
    .arg(&bundle_rel)
    .arg(fixture_name)
    .arg("--output-root")
    .arg(&output_root_rel)
    .arg("--overwrite")
    .status()
    .expect("run importer");
  assert!(status.success(), "importer exited with {}", status);

  let index_path = output_root_abs.join(fixture_name).join("index.html");
  assert!(
    index_path.is_file(),
    "index.html should be generated at {}",
    index_path.display()
  );
}

