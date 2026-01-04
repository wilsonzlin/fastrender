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
      && stdout.contains("chrome-baseline-fixtures")
      && stdout.contains("fixture-chrome-diff")
      && stdout.contains("pageset")
      && stdout.contains("pageset-diff")
      && stdout.contains("perf-smoke")
      && stdout.contains("validate-page-fixtures")
      && stdout.contains("recapture-page-fixtures")
      && stdout.contains("import-page-fixture"),
    "help output should mention available subcommands; got:\n{stdout}"
  );
}

#[test]
fn chrome_baseline_fixtures_help_mentions_flags() {
  let output = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .args(["chrome-baseline-fixtures", "--help"])
    .output()
    .expect("run cargo xtask chrome-baseline-fixtures --help");

  assert!(
    output.status.success(),
    "chrome-baseline-fixtures help should exit successfully"
  );

  let stdout = String::from_utf8_lossy(&output.stdout);
  assert!(
    stdout.contains("--fixture-dir")
      && stdout.contains("--fixtures-dir")
      && stdout.contains("--fixtures-root")
      && stdout.contains("--out-dir")
      && stdout.contains("--fixtures")
      && stdout.contains("--shard")
      && stdout.contains("--chrome")
      && stdout.contains("--chrome-dir")
      && stdout.contains("--viewport")
      && stdout.contains("--dpr")
      && stdout.contains("--timeout")
      && stdout.contains("--js"),
    "help output should mention key flags; got:\n{stdout}"
  );
}

#[test]
fn fixture_chrome_diff_help_mentions_flags() {
  let output = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .args(["fixture-chrome-diff", "--help"])
    .output()
    .expect("run cargo xtask fixture-chrome-diff --help");

  assert!(
    output.status.success(),
    "fixture-chrome-diff help should exit successfully"
  );

  let stdout = String::from_utf8_lossy(&output.stdout);
  assert!(
    stdout.contains("--fixtures-dir")
      && stdout.contains("--out-dir")
      && stdout.contains("--fixtures")
      && stdout.contains("--shard")
      && stdout.contains("--jobs")
      && stdout.contains("--write-snapshot")
      && stdout.contains("--viewport")
      && stdout.contains("--dpr")
      && stdout.contains("--media")
      && stdout.contains("--timeout")
      && stdout.contains("--js")
      && stdout.contains("--tolerance")
      && stdout.contains("--max-diff-percent")
      && stdout.contains("--max-perceptual-distance")
      && stdout.contains("--sort-by")
      && stdout.contains("--ignore-alpha")
      && stdout.contains("--fail-on-differences")
      && stdout.contains("--no-build")
      && stdout.contains("--no-fastrender")
      && stdout.contains("--diff-only")
      && stdout.contains("--chrome")
      && stdout.contains("--chrome-dir")
      && stdout.contains("--no-chrome"),
    "help output should mention key flags; got:\n{stdout}"
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
    stdout.contains("--no-disk-cache") && stdout.contains("--disk-cache"),
    "pageset help should mention disk cache enable/disable flags; got:\n{stdout}"
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
fn pageset_help_mentions_cascade_diagnostics() {
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
    stdout.contains("--cascade-diagnostics"),
    "pageset help should mention cascade diagnostics reruns; got:\n{stdout}"
  );
}

#[test]
fn pageset_help_mentions_allow_http_error_status() {
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
    stdout.contains("--allow-http-error-status"),
    "pageset help should mention --allow-http-error-status; got:\n{stdout}"
  );
}

#[test]
fn pageset_help_mentions_allow_collisions() {
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
    stdout.contains("--allow-collisions"),
    "pageset help should mention --allow-collisions; got:\n{stdout}"
  );
}

#[test]
fn pageset_help_mentions_timings() {
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
    stdout.contains("--timings"),
    "pageset help should mention --timings; got:\n{stdout}"
  );
}

#[test]
fn pageset_help_mentions_cache_dir() {
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
    stdout.contains("--cache-dir"),
    "pageset help should mention --cache-dir; got:\n{stdout}"
  );
}

#[test]
fn diff_renders_help_mentions_ignore_alpha() {
  let output = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .args(["diff-renders", "--help"])
    .output()
    .expect("run cargo xtask diff-renders --help");

  assert!(
    output.status.success(),
    "diff-renders help should exit successfully"
  );

  let stdout = String::from_utf8_lossy(&output.stdout);
  assert!(
    stdout.contains("--ignore-alpha") && stdout.contains("--fail-on-differences"),
    "diff-renders help should mention --ignore-alpha and --fail-on-differences; got:\n{stdout}"
  );
}

#[test]
fn pageset_help_mentions_capture_missing_failure_fixtures() {
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
    stdout.contains("--capture-missing-failure-fixtures")
      && stdout.contains("--capture-missing-failure-fixtures-out-dir")
      && stdout.contains("--capture-missing-failure-fixtures-allow-missing-resources")
      && stdout.contains("--capture-missing-failure-fixtures-overwrite"),
    "pageset help should mention the capture-missing-failure-fixtures flags; got:\n{stdout}"
  );
}

#[test]
fn pageset_help_mentions_refresh_flag() {
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
    stdout.contains("--refresh"),
    "pageset help should mention --refresh; got:\n{stdout}"
  );
}

#[test]
fn update_pageset_guardrails_help_mentions_strategy_flag() {
  let output = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .args(["update-pageset-guardrails", "--help"])
    .output()
    .expect("run cargo xtask update-pageset-guardrails --help");

  assert!(
    output.status.success(),
    "update-pageset-guardrails help should exit successfully"
  );

  let stdout = String::from_utf8_lossy(&output.stdout);
  assert!(
    stdout.contains("--strategy"),
    "update-pageset-guardrails help should mention the selection strategy; got:\n{stdout}"
  );
}

#[test]
fn update_pageset_guardrails_help_mentions_cache_dir_alias() {
  let output = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .args(["update-pageset-guardrails", "--help"])
    .output()
    .expect("run cargo xtask update-pageset-guardrails --help");

  assert!(
    output.status.success(),
    "update-pageset-guardrails help should exit successfully"
  );

  let stdout = String::from_utf8_lossy(&output.stdout);
  assert!(
    stdout.contains("aliases: --cache-dir"),
    "help output should mention the --cache-dir alias for --asset-cache-dir; got:\n{stdout}"
  );
}

#[test]
fn update_pageset_timeouts_alias_help_mentions_strategy_flag() {
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

#[test]
fn recapture_page_fixtures_help_mentions_cache_dir_alias() {
  let output = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .args(["recapture-page-fixtures", "--help"])
    .output()
    .expect("run cargo xtask recapture-page-fixtures --help");

  assert!(
    output.status.success(),
    "recapture-page-fixtures help should exit successfully"
  );

  let stdout = String::from_utf8_lossy(&output.stdout);
  assert!(
    stdout.contains("aliases: --cache-dir"),
    "help output should mention the --cache-dir alias for --asset-cache-dir; got:\n{stdout}"
  );
}

#[test]
fn update_pageset_guardrails_budgets_help_mentions_multiplier_flag() {
  let output = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .args(["update-pageset-guardrails-budgets", "--help"])
    .output()
    .expect("run cargo xtask update-pageset-guardrails-budgets --help");

  assert!(
    output.status.success(),
    "update-pageset-guardrails-budgets help should exit successfully"
  );

  let stdout = String::from_utf8_lossy(&output.stdout);
  assert!(
    stdout.contains("--multiplier"),
    "update-pageset-guardrails-budgets help should mention --multiplier; got:\n{stdout}"
  );
}

#[test]
fn update_pageset_timeout_budgets_alias_help_mentions_multiplier_flag() {
  let output = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .args(["update-pageset-timeout-budgets", "--help"])
    .output()
    .expect("run cargo xtask update-pageset-timeout-budgets --help");

  assert!(
    output.status.success(),
    "update-pageset-timeout-budgets help should exit successfully"
  );

  let stdout = String::from_utf8_lossy(&output.stdout);
  assert!(
    stdout.contains("--multiplier"),
    "update-pageset-timeout-budgets help should mention --multiplier; got:\n{stdout}"
  );
}

#[test]
fn perf_smoke_help_mentions_suites_and_regression_flags() {
  let output = Command::new(env!("CARGO_BIN_EXE_xtask"))
    .args(["perf-smoke", "--help"])
    .output()
    .expect("run cargo xtask perf-smoke --help");

  assert!(
    output.status.success(),
    "perf-smoke help should exit successfully"
  );

  let stdout = String::from_utf8_lossy(&output.stdout);
  assert!(
    stdout.contains("--suite")
      && stdout.contains("pageset-guardrails")
      && stdout.contains("--only")
      && stdout.contains("--baseline")
      && stdout.contains("--threshold")
      && stdout.contains("--count-threshold")
      && stdout.contains("--fail-on-regression")
      && stdout.contains("--fail-fast")
      && stdout.contains("--fail-on-failure")
      && stdout.contains("--no-fail-on-failure")
      && stdout.contains("--fail-on-missing-fixtures")
      && stdout.contains("--allow-missing-fixtures")
      && stdout.contains("--fail-on-budget")
      && stdout.contains("--fail-on-fetch-errors")
      && stdout.contains("--isolate")
      && stdout.contains("--no-isolate")
      && stdout.contains("--top")
      && stdout.contains("--output")
      && stdout.contains("--debug")
      && stdout.contains("EXTRA"),
    "perf-smoke help should mention suite selection and regression gating flags; got:\n{stdout}"
  );
}
