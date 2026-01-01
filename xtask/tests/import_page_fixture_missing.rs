use sha2::{Digest, Sha256};
use std::fs;
use std::path::PathBuf;
use std::process::Command;
use tempfile::tempdir;

fn repo_root() -> PathBuf {
  let crate_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
  crate_dir
    .parent()
    .expect("xtask crate should live under the workspace root")
    .to_path_buf()
}

#[test]
fn missing_resource_fails_without_allow_missing() {
  let repo_root = repo_root();
  let bundle_path = repo_root.join("tests/fixtures/bundle_page/missing_img");

  let temp = tempdir().expect("temp dir");
  let output_root = temp.path().join("fixtures");
  let fixture_name = "missing_img_fixture";

  let output = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .current_dir(&repo_root)
    .arg("import-page-fixture")
    .arg(&bundle_path)
    .arg(fixture_name)
    .arg("--output-root")
    .arg(&output_root)
    .arg("--overwrite")
    .output()
    .expect("run importer");

  assert!(
    !output.status.success(),
    "import should fail when bundle is missing required resources"
  );

  let stderr = String::from_utf8_lossy(&output.stderr);
  assert!(
    stderr.contains("bundle is missing required subresources"),
    "error should mention missing subresources; got:\n{stderr}"
  );
  assert!(
    stderr.contains("https://img.test/a.png"),
    "error should list the missing URL; got:\n{stderr}"
  );
}

#[test]
fn missing_resource_creates_placeholder_with_allow_missing() {
  let repo_root = repo_root();
  let bundle_path = repo_root.join("tests/fixtures/bundle_page/missing_img");

  let temp = tempdir().expect("temp dir");
  let output_root = temp.path().join("fixtures");
  let fixture_name = "missing_img_fixture_allow";

  let status = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .current_dir(&repo_root)
    .arg("import-page-fixture")
    .arg(&bundle_path)
    .arg(fixture_name)
    .arg("--output-root")
    .arg(&output_root)
    .arg("--overwrite")
    .arg("--allow-missing")
    .status()
    .expect("run importer");
  assert!(status.success(), "importer exited with {}", status);

  let fixture_dir = output_root.join(fixture_name);
  let html = fs::read_to_string(fixture_dir.join("index.html")).expect("read HTML");

  let digest = Sha256::digest(b"https://img.test/a.png");
  let hash = digest
    .iter()
    .take(16)
    .map(|b| format!("{b:02x}"))
    .collect::<String>();
  let filename = format!("missing_{hash}.png");

  assert!(
    html.contains(&format!("src=\"assets/{filename}\"")),
    "missing <img src> should be replaced with a deterministic placeholder filename"
  );

  let placeholder_path = fixture_dir.join("assets").join(&filename);
  assert!(placeholder_path.is_file(), "placeholder asset should exist");
  let bytes = fs::read(&placeholder_path).expect("read placeholder bytes");
  assert!(bytes.is_empty(), "placeholder assets should be empty");
}
