use fastrender::{FastRender, RenderOptions};
use sha2::{Digest, Sha256};
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

  let html = fs::read_to_string(&index_path).expect("read generated HTML");
  assert!(
    html.contains("href=\"https://example.com/\""),
    "<a href> navigation links should remain untouched"
  );
  assert!(
    html.contains("action=\"https://example.com/submit\""),
    "<form action> should remain untouched"
  );
  assert!(
    html.contains("xmlns=\"http://www.w3.org/2000/svg\""),
    "xmlns namespaces should remain untouched"
  );
  assert!(
    html.contains("data-src=\"https://example.com/lazy.png\""),
    "data-* attributes should remain untouched"
  );

  // Ensure fetchable subresources were rewritten to local assets.
  let hash_prefix = |bytes: &[u8]| {
    let digest = Sha256::digest(bytes);
    digest
      .iter()
      .take(16)
      .map(|b| format!("{b:02x}"))
      .collect::<String>()
  };

  let logo_bytes =
    fs::read(bundle_path.join("resources/00003_logo.png")).expect("read bundled logo");
  let logo_hash = hash_prefix(&logo_bytes);
  assert!(
    html.contains(&format!("src=\"assets/{logo_hash}.png\"")),
    "<img src> should be rewritten to a local asset filename"
  );
  assert!(
    html.contains("https://fixture.example/static/logo.png") == false,
    "remote <img src> should not survive rewrite"
  );

  // Ensure we didn't create missing placeholders for navigation links or data-* attrs.
  for entry in fs::read_dir(fixture_dir.join("assets")).expect("read assets dir") {
    let entry = entry.expect("asset entry");
    let name = entry.file_name();
    let name = name.to_string_lossy();
    assert!(
      !name.starts_with("missing_"),
      "unexpected missing placeholder asset generated: {name}"
    );
  }

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
