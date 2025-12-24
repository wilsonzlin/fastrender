use std::process::Command;

#[test]
fn help_lists_commands() {
  let output = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .arg("--help")
    .output()
    .expect("run cargo xtask --help");

  assert!(
    output.status.success(),
    "xtask help should exit successfully"
  );

  let stdout = String::from_utf8_lossy(&output.stdout);
  assert!(
    stdout.contains("render-page")
      && stdout.contains("update-goldens")
      && stdout.contains("diff-renders"),
    "help output should mention available subcommands; got:\n{stdout}"
  );
}
