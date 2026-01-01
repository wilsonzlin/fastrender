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
  let imported_css_path = fixture_dir.join("imported.css");
  let imported2_css_path = fixture_dir.join("imported2.css");
  let image_path = fixture_dir.join("image.png");
  let image2_path = fixture_dir.join("image2.png");
  let source_path = fixture_dir.join("source.png");
  let iframe_html_path = fixture_dir.join("iframe.html");
  let iframe_css_path = fixture_dir.join("iframe.css");
  let iframe_image_path = fixture_dir.join("iframe_image.png");
  let iframe_bg_path = fixture_dir.join("iframe_bg.png");
  let inline_bg_path = fixture_dir.join("inline_bg.png");
  let inline_attr_path = fixture_dir.join("inline_attr.png");
  let imported_bg_path = fixture_dir.join("imported_bg.png");
  let imported2_bg_path = fixture_dir.join("imported2_bg.png");
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
  let imported_css_url = format!("file://{}", imported_css_path.display());
  let imported2_css_url = format!("file://{}", imported2_css_path.display());
  let image_url = format!("file://{}", image_path.display());
  let image2_url = format!("file://{}", image2_path.display());
  let source_url = format!("file://{}", source_path.display());
  let iframe_html_url = format!("file://{}", iframe_html_path.display());
  let iframe_css_url = format!("file://{}", iframe_css_path.display());
  let iframe_image_url = format!("file://{}", iframe_image_path.display());
  let iframe_bg_url = format!("file://{}", iframe_bg_path.display());
  let inline_bg_url = format!("file://{}", inline_bg_path.display());
  let inline_attr_url = format!("file://{}", inline_attr_path.display());
  let imported_bg_url = format!("file://{}", imported_bg_path.display());
  let imported2_bg_url = format!("file://{}", imported2_bg_path.display());
  let font_url = format!("file://{}", font_path.display());

  assert!(
    resources.contains_key(&css_url),
    "css should be captured in manifest"
  );
  assert!(
    resources.contains_key(&imported_css_url),
    "@import stylesheet should be captured in manifest"
  );
  assert!(
    resources.contains_key(&imported2_css_url),
    "recursive @import stylesheet should be captured in manifest"
  );
  assert!(
    resources.contains_key(&image_url),
    "image should be captured in manifest"
  );
  assert!(
    resources.contains_key(&image2_url),
    "srcset candidate image should be captured in manifest"
  );
  assert!(
    resources.contains_key(&source_url),
    "<source srcset> image should be captured in manifest"
  );
  assert!(
    resources.contains_key(&iframe_html_url),
    "iframe document should be captured in manifest"
  );
  assert!(
    resources.contains_key(&iframe_css_url),
    "iframe stylesheet should be captured in manifest"
  );
  assert!(
    resources.contains_key(&iframe_image_url),
    "iframe image should be captured in manifest"
  );
  assert!(
    resources.contains_key(&iframe_bg_url),
    "iframe CSS url() image should be captured in manifest"
  );
  assert!(
    resources.contains_key(&inline_bg_url),
    "<style> url() image should be captured in manifest"
  );
  assert!(
    resources.contains_key(&inline_attr_url),
    "style attribute url() image should be captured in manifest"
  );
  assert!(
    resources.contains_key(&imported_bg_url),
    "@import CSS url() image should be captured in manifest"
  );
  assert!(
    resources.contains_key(&imported2_bg_url),
    "recursive @import CSS url() image should be captured in manifest"
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
