use fastrender::resource::url_to_filename;
use std::fs;
use std::process::Command;

fn write_simple_html(dir: &tempfile::TempDir) -> (String, String) {
  let html_path = dir.path().join("page.html");
  fs::write(
    &html_path,
    "<html><body><div style=\"width:1px;height:1px;background:#000\"></div></body></html>",
  )
  .expect("write html");
  let url = format!("file://{}", html_path.display());
  let expected_output = format!("{}.png", url_to_filename(&url));
  (url, expected_output)
}

#[test]
fn default_output_derives_from_url_stem() {
  let tmp = tempfile::TempDir::new().expect("tempdir");
  let (url, expected_output) = write_simple_html(&tmp);

  let status = Command::new(env!("CARGO_BIN_EXE_fetch_and_render"))
    .arg(&url)
    .current_dir(tmp.path())
    .status()
    .expect("run fetch_and_render");

  assert!(
    status.success(),
    "fetch_and_render should succeed for file URL"
  );

  let output_path = tmp.path().join(expected_output);
  assert!(
    output_path.exists(),
    "default output file should be created"
  );
  let bytes = fs::read(&output_path).expect("read output image");
  assert!(!bytes.is_empty(), "output image should not be empty");
}

#[test]
fn output_parent_directories_are_created() {
  let tmp = tempfile::TempDir::new().expect("tempdir");
  let (url, _) = write_simple_html(&tmp);
  let nested = tmp.path().join("nested/dir/out.png");

  let status = Command::new(env!("CARGO_BIN_EXE_fetch_and_render"))
    .args([&url, nested.to_str().unwrap()])
    .current_dir(tmp.path())
    .status()
    .expect("run fetch_and_render");

  assert!(
    status.success(),
    "fetch_and_render should succeed with nested output path"
  );
  assert!(nested.exists(), "nested output file should be created");
}

#[test]
fn format_flag_changes_default_extension() {
  let tmp = tempfile::TempDir::new().expect("tempdir");
  let (url, _) = write_simple_html(&tmp);
  let expected_output = tmp.path().join(format!("{}.webp", url_to_filename(&url)));

  let status = Command::new(env!("CARGO_BIN_EXE_fetch_and_render"))
    .arg("--format")
    .arg("webp")
    .arg(&url)
    .current_dir(tmp.path())
    .status()
    .expect("run fetch_and_render");

  assert!(
    status.success(),
    "fetch_and_render should succeed when overriding format"
  );
  assert!(
    expected_output.exists(),
    "default output should follow requested format extension"
  );
}

#[cfg(feature = "disk_cache")]
#[test]
fn disk_cache_directory_is_created_when_missing() {
  let tmp = tempfile::TempDir::new().expect("tempdir");
  let (url, _) = write_simple_html(&tmp);
  let asset_dir = tmp.path().join("fetches/assets");
  assert!(
    !asset_dir.exists(),
    "asset cache directory should start absent in a fresh tempdir"
  );

  let status = Command::new(env!("CARGO_BIN_EXE_fetch_and_render"))
    .arg(&url)
    .current_dir(tmp.path())
    .status()
    .expect("run fetch_and_render");

  assert!(
    status.success(),
    "fetch_and_render should succeed for local file with disk cache enabled"
  );
  assert!(
    asset_dir.is_dir(),
    "disk cache directory should be created even when unused"
  );
}
