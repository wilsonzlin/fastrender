use std::fs;
use std::path::PathBuf;
use std::process::Command;
use tempfile::tempdir;
use url::Url;

#[test]
fn recaptures_and_imports_file_fixture() {
  let crate_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
  let repo_root = crate_dir
    .parent()
    .expect("xtask crate should live under the workspace root");

  let temp = tempdir().expect("temp dir");
  let page_dir = temp.path().join("page");
  fs::create_dir_all(&page_dir).expect("create page dir");

  // Simple HTML fixture with a linked stylesheet and an image so crawl mode captures subresources.
  fs::write(
    page_dir.join("index.html"),
    r#"<!doctype html>
<html>
  <head>
    <meta charset="utf-8">
    <link rel="stylesheet" href="styles.css">
  </head>
  <body>
    <img src="image.png" alt="fixture">
  </body>
</html>
"#,
  )
  .expect("write html");
  fs::write(
    page_dir.join("styles.css"),
    "body { background-image: url('image.png'); }",
  )
  .expect("write css");
  fs::write(page_dir.join("image.png"), b"not a real png").expect("write image");

  let file_url = Url::from_file_path(page_dir.join("index.html"))
    .expect("file:// url")
    .to_string();

  let manifest_path = temp.path().join("manifest.json");
  fs::write(
    &manifest_path,
    format!(
      r#"{{
  "fixtures": [
    {{
      "name": "local_file_fixture",
      "url": "{file_url}",
      "viewport": [1200, 800],
      "dpr": 1.0
    }}
  ]
}}"#
    ),
  )
  .expect("write manifest");

  let fixtures_root = temp.path().join("fixtures");
  let bundles_root = temp.path().join("bundles");

  let status = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .current_dir(repo_root)
    .arg("recapture-page-fixtures")
    .arg("--manifest")
    .arg(&manifest_path)
    .arg("--fixtures-root")
    .arg(&fixtures_root)
    .arg("--bundle-out-dir")
    .arg(&bundles_root)
    .arg("--debug")
    .status()
    .expect("run recapture-page-fixtures");
  assert!(status.success(), "command exited with {}", status);

  let fixture_dir = fixtures_root.join("local_file_fixture");
  let index = fixture_dir.join("index.html");
  assert!(index.is_file(), "fixture index.html should exist");

  let html = fs::read_to_string(&index).expect("read imported html");
  assert!(
    !html.contains("http://") && !html.contains("https://") && !html.contains("file://"),
    "imported fixture should not contain remote/file references; got:\n{html}"
  );
  assert!(
    fixture_dir.join("assets").is_dir(),
    "assets directory should exist"
  );
}
