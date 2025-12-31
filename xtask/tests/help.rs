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
      && stdout.contains("diff-renders")
      && stdout.contains("pageset")
      && stdout.contains("pageset-diff")
      && stdout.contains("perf-smoke")
      && stdout.contains("import-page-fixture"),
    "help output should mention available subcommands; got:\n{stdout}"
  );
}

#[test]
fn pageset_help_mentions_disk_cache_flag() {
  let output = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .args(["pageset", "--help"])
    .output()
    .expect("run cargo xtask pageset --help");

  assert!(
    output.status.success(),
    "pageset help should exit successfully"
  );

  let stdout = String::from_utf8_lossy(&output.stdout);
  assert!(
    stdout.contains("--no-disk-cache"),
    "pageset help should mention the disk cache opt-out; got:\n{stdout}"
  );
}

#[test]
fn pageset_help_mentions_filters() {
  let output = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .args(["pageset", "--help"])
    .output()
    .expect("run cargo xtask pageset --help");

  assert!(
    output.status.success(),
    "xtask pageset help should exit successfully"
  );

  let stdout = String::from_utf8_lossy(&output.stdout);
  assert!(
    stdout.contains("--shard") && stdout.contains("--pages"),
    "pageset help should mention sharding and page filters; got:\n{stdout}"
  );
}

#[test]
fn update_pageset_timeouts_help_mentions_strategy_flag() {
  let output = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .args(["update-pageset-timeouts", "--help"])
    .output()
    .expect("run cargo xtask update-pageset-timeouts --help");

  assert!(
    output.status.success(),
    "update-pageset-timeouts help should exit successfully"
  );

  let stdout = String::from_utf8_lossy(&output.stdout);
  assert!(
    stdout.contains("--strategy"),
    "update-pageset-timeouts help should mention the selection strategy; got:\n{stdout}"
  );
}
