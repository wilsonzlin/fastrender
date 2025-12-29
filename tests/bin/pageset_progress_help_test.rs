use std::process::Command;

#[test]
fn pageset_progress_help_exits_success() {
  let status = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .arg("--help")
    .status()
    .expect("run pageset_progress --help");
  assert!(status.success(), "expected success for --help");
}

#[test]
fn pageset_progress_run_help_exits_success() {
  let status = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .args(["run", "--help"])
    .status()
    .expect("run pageset_progress run --help");
  assert!(status.success(), "expected success for run --help");
}
