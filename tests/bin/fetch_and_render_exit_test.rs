use fastrender::resource::url_to_filename;
use std::fs;
use std::process::Command;

#[test]
fn fetch_and_render_exits_non_zero_when_no_args() {
  let status = Command::new(env!("CARGO_BIN_EXE_fetch_and_render"))
    .status()
    .expect("run fetch_and_render");

  assert!(
    !status.success(),
    "expected non-zero exit when no URL provided"
  );
}

#[test]
fn fetch_and_render_exits_non_zero_for_missing_file_url() {
  let tmp = tempfile::TempDir::new().expect("tempdir");
  let missing = tmp.path().join("missing.html");
  let output = tmp.path().join("out.png");

  let status = Command::new(env!("CARGO_BIN_EXE_fetch_and_render"))
    .arg(format!("file://{}", missing.display()))
    .arg(&output)
    .status()
    .expect("run fetch_and_render");

  assert!(
    !status.success(),
    "expected non-zero exit when input file is missing (got {:?})",
    status.code()
  );
}

#[test]
fn fetch_and_render_errors_on_unknown_option() {
  let status = Command::new(env!("CARGO_BIN_EXE_fetch_and_render"))
    .args(["--definitely-not-a-flag"])
    .status()
    .expect("run fetch_and_render");

  assert!(
    !status.success(),
    "expected non-zero exit when an unknown option is provided (got {:?})",
    status.code()
  );
}

#[test]
fn fetch_and_render_defaults_output_name_from_url() {
  let tmp = tempfile::TempDir::new().expect("tempdir");
  let html_path = tmp.path().join("page.html");
  fs::write(&html_path, "<!doctype html><title>Hello</title>").expect("write html");

  let url = format!("file://{}", html_path.display());
  let expected_png = tmp.path().join(format!("{}.png", url_to_filename(&url)));

  let status = Command::new(env!("CARGO_BIN_EXE_fetch_and_render"))
    .current_dir(tmp.path())
    .arg(&url)
    .status()
    .expect("run fetch_and_render");

  assert!(
    status.success(),
    "expected success, got {:?}",
    status.code()
  );
  assert!(
    expected_png.exists(),
    "expected output {:?} to exist",
    expected_png
  );
}
