use serde_json::Value;
use std::fs;
use std::process::Command;
use tempfile::TempDir;

#[test]
fn bundled_font_coverage_reports_uncovered_codepoints_from_visible_text_only() {
  let temp = TempDir::new().expect("tempdir");
  let html_dir = temp.path().join("fetches/html");
  fs::create_dir_all(&html_dir).expect("create fetches/html");

  // U+13000 EGYPTIAN HIEROGLYPH A001 should be reported as uncovered (not in bundled subsets).
  // U+13001 EGYPTIAN HIEROGLYPH A002 appears only in a hidden subtree and should be ignored.
  // U+12000 CUNEIFORM SIGN A appears only in CSS `content:` and is included only when
  // `--include-css-content` is enabled.
  fs::write(
    html_dir.join("fixture.html"),
    r#"<!doctype html>
<html>
  <head>
    <meta charset="utf-8">
    <style>
      .x::before { content: "𒀀"; }
    </style>
  </head>
  <body>
    <p>Visible: 𓀀</p>
    <div hidden>Hidden: 𓀁</div>
    <script>var x="ignored";</script>
  </body>
</html>"#,
  )
  .expect("write fixture");

  let output = Command::new(env!("CARGO_BIN_EXE_bundled_font_coverage"))
    .current_dir(temp.path())
    .args(["--pages", "fixture", "--json"])
    .output()
    .expect("run bundled_font_coverage");
  assert!(
    output.status.success(),
    "expected success, got status {:?}, stderr:\n{}",
    output.status.code(),
    String::from_utf8_lossy(&output.stderr)
  );

  let report: Value = serde_json::from_slice(&output.stdout).expect("json parse");
  let uncovered = report["uncovered_codepoints"]
    .as_array()
    .expect("uncovered_codepoints array");
  let uncovered: Vec<&str> = uncovered.iter().filter_map(|v| v.as_str()).collect();

  assert!(
    uncovered.contains(&"U+13000"),
    "expected uncovered to include U+13000, got: {uncovered:?}"
  );
  assert!(
    !uncovered.contains(&"U+13001"),
    "expected hidden U+13001 to be ignored, got: {uncovered:?}"
  );
  assert!(
    !uncovered.contains(&"U+12000"),
    "expected CSS-only U+12000 to be ignored without --include-css-content, got: {uncovered:?}"
  );

  let output = Command::new(env!("CARGO_BIN_EXE_bundled_font_coverage"))
    .current_dir(temp.path())
    .args(["--pages", "fixture", "--include-css-content", "--json"])
    .output()
    .expect("run bundled_font_coverage --include-css-content");
  assert!(
    output.status.success(),
    "expected success, got status {:?}, stderr:\n{}",
    output.status.code(),
    String::from_utf8_lossy(&output.stderr)
  );

  let report: Value = serde_json::from_slice(&output.stdout).expect("json parse");
  let uncovered = report["uncovered_codepoints"]
    .as_array()
    .expect("uncovered_codepoints array");
  let uncovered: Vec<&str> = uncovered.iter().filter_map(|v| v.as_str()).collect();

  assert!(
    uncovered.contains(&"U+13000"),
    "expected uncovered to include U+13000, got: {uncovered:?}"
  );
  assert!(
    uncovered.contains(&"U+12000"),
    "expected uncovered to include U+12000 when scanning CSS content, got: {uncovered:?}"
  );
}
