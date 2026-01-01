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
fn imports_bundle_with_html_entities_in_urls() {
  let repo_root = repo_root();
  let bundle_path = repo_root.join("tests/fixtures/bundle_page/html_entities");

  let temp = tempdir().expect("temp dir");
  let output_root = temp.path().join("fixtures");
  let fixture_name = "html_entities_fixture";

  let status = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .current_dir(&repo_root)
    .arg("import-page-fixture")
    .arg(&bundle_path)
    .arg(fixture_name)
    .arg("--output-root")
    .arg(&output_root)
    .arg("--overwrite")
    .status()
    .expect("run importer");
  assert!(status.success(), "importer exited with {}", status);

  let fixture_dir = output_root.join(fixture_name);
  let html = fs::read_to_string(fixture_dir.join("index.html")).expect("read HTML");

  let img_bytes = fs::read(bundle_path.join("resources/00000_a.png")).expect("read bundled image");
  let digest = Sha256::digest(&img_bytes);
  let hash = digest
    .iter()
    .take(16)
    .map(|b| format!("{b:02x}"))
    .collect::<String>();

  assert!(
    html.contains(&format!("src=\"assets/{hash}.png\"")),
    "importer should rewrite decoded URLs to local assets"
  );
  assert!(
    !html.contains("img.test/a.png"),
    "imported HTML should not contain the original remote URL"
  );
}

