use std::process::Command;

#[test]
fn inspect_frag_loads_external_css_and_respects_fetch_toggle() {
  let tmp = tempfile::tempdir().expect("temp dir");
  let html_path = tmp.path().join("page.html");
  let css_path = tmp.path().join("style.css");

  let html = r#"<!doctype html><html><head><link rel="stylesheet" href="style.css"></head><body><div>hello</div></body></html>"#;
  let css = "body { background: rgb(1, 2, 3); color: rgb(4, 5, 6); }";

  std::fs::write(&html_path, html).expect("write html");
  std::fs::write(&css_path, css).expect("write css");

  let expected = "body bg=rgba(1,2,3,1.00) color=rgba(4,5,6,1.00)";

  let output = Command::new(env!("CARGO_BIN_EXE_inspect_frag"))
    .arg(&html_path)
    .env("FASTR_FETCH_LINK_CSS", "1")
    .output()
    .expect("run inspect_frag");
  assert!(
    output.status.success(),
    "inspect_frag should succeed (enabled): stderr={}",
    String::from_utf8_lossy(&output.stderr)
  );
  let stdout = String::from_utf8_lossy(&output.stdout);
  assert!(
    stdout.contains(expected),
    "expected inspect_frag output to include external CSS styles, missing {expected}. stdout={stdout}"
  );

  let output = Command::new(env!("CARGO_BIN_EXE_inspect_frag"))
    .arg(&html_path)
    .env("FASTR_FETCH_LINK_CSS", "0")
    .output()
    .expect("run inspect_frag (disabled)");
  assert!(
    output.status.success(),
    "inspect_frag should succeed (disabled): stderr={}",
    String::from_utf8_lossy(&output.stderr)
  );
  let stdout = String::from_utf8_lossy(&output.stdout);
  assert!(
    !stdout.contains(expected),
    "expected inspect_frag output to omit external CSS styles when FASTR_FETCH_LINK_CSS=0. stdout={stdout}"
  );
}
