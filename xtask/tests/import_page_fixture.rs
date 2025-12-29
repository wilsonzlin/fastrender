use fastrender::{FastRender, RenderOptions};
use std::fs;
use std::path::PathBuf;
use std::process::Command;
use tempfile::tempdir;
use url::Url;

#[test]
fn imports_bundle_into_fixture() {
  let crate_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
  let repo_root = crate_dir
    .parent()
    .expect("xtask crate should live under the workspace root");
  let bundle_path = repo_root.join("tests/fixtures/bundle_page/simple");

  let temp = tempdir().expect("temp dir");
  let output_root = temp.path().join("fixtures");
  let fixture_name = "synthetic_bundle";

  let status = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .current_dir(repo_root)
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
  let index_path = fixture_dir.join("index.html");
  assert!(index_path.is_file(), "index.html should be generated");
  assert!(
    fixture_dir.join("assets").is_dir(),
    "assets directory should exist"
  );

  // Ensure all text assets are local-only.
  for entry in walkdir::WalkDir::new(&fixture_dir) {
    let entry = entry.expect("walk output");
    if !entry.file_type().is_file() {
      continue;
    }
    if let Ok(content) = fs::read_to_string(entry.path()) {
      assert!(
        !content.contains("http://") && !content.contains("https://"),
        "remote reference persisted in {}",
        entry.path().display()
      );
    }
  }

  let html = fs::read_to_string(&index_path).expect("read generated HTML");
  let base_url = Url::from_directory_path(&fixture_dir)
    .expect("file:// base")
    .to_string();
  let mut renderer = FastRender::builder()
    .base_url(base_url)
    .build()
    .expect("build renderer");
  let pixmap = renderer
    // Match the pages_regression default viewport to ensure the fixture layout is usable there.
    .render_html_with_options(&html, RenderOptions::new().with_viewport(1040, 1240))
    .expect("render imported fixture");
  assert!(pixmap.width() > 0 && pixmap.height() > 0);
}
