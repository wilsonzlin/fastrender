use serde_json::Value;
use std::fs;
use std::path::Path;
use std::process::Command;

#[test]
fn bundles_and_renders_local_fixture() {
  let tmp = tempfile::TempDir::new().expect("tempdir");
  let fixture_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/bundle_page");
  let html_path = fixture_dir.join("page.html");
  let css_path = fixture_dir.join("styles.css");
  let image_path = fixture_dir.join("image.png");
  let font_path = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fonts/ColorTestCOLR.ttf");

  let url = format!("file://{}", html_path.display());
  let bundle_dir = tmp.path().join("capture");
  let output_png = tmp.path().join("out.png");

  let status = Command::new(env!("CARGO_BIN_EXE_bundle_page"))
    .args(["fetch", &url, "--out", bundle_dir.to_str().unwrap()])
    .status()
    .expect("run bundle_page fetch");
  assert!(status.success(), "bundle capture should succeed");

  let manifest_path = bundle_dir.join("bundle.json");
  let manifest_bytes = fs::read(&manifest_path).expect("manifest bytes");
  let manifest: Value = serde_json::from_slice(&manifest_bytes).expect("parse manifest");

  let resources = manifest["resources"].as_object().expect("resources object");
  let css_url = format!("file://{}", css_path.display());
  let image_url = format!("file://{}", image_path.display());
  let font_url = format!("file://{}", font_path.display());

  assert!(
    resources.contains_key(&css_url),
    "css should be captured in manifest"
  );
  assert!(
    resources.contains_key(&image_url),
    "image should be captured in manifest"
  );
  assert!(
    resources.contains_key(&font_url),
    "font should be captured in manifest"
  );

  let viewport = manifest["render"]["viewport"]
    .as_array()
    .expect("viewport tuple");
  assert_eq!(viewport[0].as_u64(), Some(1200));
  assert_eq!(viewport[1].as_u64(), Some(800));

  let status = Command::new(env!("CARGO_BIN_EXE_bundle_page"))
    .args([
      "render",
      bundle_dir.to_str().unwrap(),
      "--out",
      output_png.to_str().unwrap(),
    ])
    .status()
    .expect("run bundle_page render");
  assert!(status.success(), "render should succeed offline");

  let png_bytes = fs::read(&output_png).expect("png output");
  assert!(!png_bytes.is_empty(), "png should be written");
}

#[test]
fn bundles_and_renders_local_fixture_without_rendering_capture() {
  let tmp = tempfile::TempDir::new().expect("tempdir");
  let fixture_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/bundle_page");
  let html_path = fixture_dir.join("page.html");
  let css_path = fixture_dir.join("styles.css");
  let image_path = fixture_dir.join("image.png");
  let font_path = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fonts/ColorTestCOLR.ttf");

  let url = format!("file://{}", html_path.display());
  let bundle_dir = tmp.path().join("capture");
  let output_png = tmp.path().join("out.png");

  let status = Command::new(env!("CARGO_BIN_EXE_bundle_page"))
    .args([
      "fetch",
      &url,
      "--out",
      bundle_dir.to_str().unwrap(),
      "--no-render",
    ])
    .status()
    .expect("run bundle_page fetch --no-render");
  assert!(status.success(), "bundle capture should succeed");

  let manifest_path = bundle_dir.join("bundle.json");
  let manifest_bytes = fs::read(&manifest_path).expect("manifest bytes");
  let manifest: Value = serde_json::from_slice(&manifest_bytes).expect("parse manifest");

  let resources = manifest["resources"].as_object().expect("resources object");
  let css_url = format!("file://{}", css_path.display());
  let image_url = format!("file://{}", image_path.display());
  let font_url = format!("file://{}", font_path.display());

  assert!(
    resources.contains_key(&css_url),
    "css should be captured in manifest"
  );
  assert!(
    resources.contains_key(&image_url),
    "image should be captured in manifest"
  );
  assert!(
    resources.contains_key(&font_url),
    "font should be captured in manifest"
  );

  let status = Command::new(env!("CARGO_BIN_EXE_bundle_page"))
    .args([
      "render",
      bundle_dir.to_str().unwrap(),
      "--out",
      output_png.to_str().unwrap(),
    ])
    .status()
    .expect("run bundle_page render");
  assert!(status.success(), "render should succeed offline");

  let png_bytes = fs::read(&output_png).expect("png output");
  assert!(!png_bytes.is_empty(), "png should be written");
}
