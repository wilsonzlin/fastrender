use std::process::Command;

#[test]
fn bundled_fonts_report_missing_codepoint() {
  let output = Command::new(env!("CARGO_BIN_EXE_font_coverage"))
    .args(["--bundled-fonts", "--text", "\u{1AB0}"])
    .output()
    .expect("run font_coverage");

  assert!(
    output.status.success(),
    "font_coverage should exit successfully (stdout: {}, stderr: {})",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );

  let stdout = String::from_utf8_lossy(&output.stdout);
  assert!(
    stdout.contains("U+1AB0"),
    "expected output to mention missing U+1AB0; got:\n{}",
    stdout
  );
}
