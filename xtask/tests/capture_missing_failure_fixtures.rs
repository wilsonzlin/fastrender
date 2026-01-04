use std::fs;
use tempfile::tempdir;
use xtask::capture_missing_failure_fixtures::{
  plan_capture_missing_failure_fixtures, CaptureMissingFailureFixturesArgs,
};

#[test]
fn selects_missing_failing_fixtures_and_builds_commands() {
  let temp = tempdir().expect("tempdir");
  let progress_dir = temp.path().join("progress/pages");
  fs::create_dir_all(&progress_dir).expect("create progress dir");

  fs::write(progress_dir.join("ok.json"), r#"{"status":"ok"}"#).expect("write ok progress");
  fs::write(progress_dir.join("has_fixture.json"), r#"{"status":"timeout"}"#)
    .expect("write timeout progress");
  fs::write(progress_dir.join("missing_error.json"), r#"{"status":"error"}"#)
    .expect("write error progress");
  fs::write(progress_dir.join("missing_panic.json"), r#"{"status":"panic"}"#)
    .expect("write panic progress");

  let fixtures_root = temp.path().join("fixtures");
  fs::create_dir_all(fixtures_root.join("has_fixture")).expect("create fixture dir");
  fs::write(fixtures_root.join("has_fixture/index.html"), "existing fixture")
    .expect("write fixture index");

  let bundles_out = temp.path().join("bundles");
  let cache_dir = temp.path().join("asset-cache");

  let args = CaptureMissingFailureFixturesArgs {
    progress_dir,
    fixtures_root: fixtures_root.clone(),
    bundle_out_dir: bundles_out.clone(),
    asset_cache_dir: cache_dir.clone(),
    user_agent: Some("TestAgent/1.0".to_string()),
    accept_language: Some("en-US,en".to_string()),
    viewport: Some("800x600".to_string()),
    dpr: Some("2".to_string()),
    allow_missing_resources: true,
    overwrite: true,
  };

  let plan = plan_capture_missing_failure_fixtures(&args).expect("plan fixtures");
  assert_eq!(
    plan.failing_pages_total, 3,
    "expected three failing pages (timeout/error/panic)"
  );
  assert_eq!(
    plan.fixtures_already_present, 1,
    "expected one failing page fixture to already exist"
  );
  assert_eq!(
    plan.captures.len(),
    2,
    "expected exactly two missing failure fixtures to capture"
  );
  assert_eq!(plan.captures[0].stem, "missing_error");
  assert_eq!(plan.captures[1].stem, "missing_panic");

  for capture in &plan.captures {
    let expected_bundle = bundles_out.join(format!("{}.tar", capture.stem));
    assert_eq!(
      capture.bundle_path, expected_bundle,
      "bundle path should live under the configured out dir"
    );

    let bundle_cmd = &capture.bundle_command;
    assert_eq!(bundle_cmd.program, "cargo");

    let bundle_args = bundle_cmd.args.join(" ");
    assert!(
      bundle_args.contains("--bin bundle_page -- cache"),
      "bundle_page cache invocation missing: {bundle_args}"
    );
    assert!(
      bundle_args.contains(&format!("cache {} --out", capture.stem)),
      "bundle command should pass the stem: {bundle_args}"
    );
    assert!(
      bundle_args.contains(&format!("--out {}", expected_bundle.to_string_lossy())),
      "bundle command should write to the expected bundle path: {bundle_args}"
    );
    assert!(
      bundle_args.contains(&format!(
        "--asset-cache-dir {}",
        cache_dir.to_string_lossy()
      )),
      "bundle command should use the configured asset cache dir: {bundle_args}"
    );
    assert!(
      bundle_args.contains("--user-agent TestAgent/1.0"),
      "bundle command should forward --user-agent: {bundle_args}"
    );
    assert!(
      bundle_args.contains("--accept-language en-US,en"),
      "bundle command should forward --accept-language: {bundle_args}"
    );
    assert!(
      bundle_args.contains("--allow-missing"),
      "bundle command should forward --allow-missing: {bundle_args}"
    );
    assert!(
      bundle_args.contains("--viewport 800x600"),
      "bundle command should forward --viewport: {bundle_args}"
    );
    assert!(
      bundle_args.contains("--dpr 2"),
      "bundle command should forward --dpr: {bundle_args}"
    );

    let import_cmd = &capture.import_command;
    assert_eq!(import_cmd.program, "cargo");

    let import_args = import_cmd.args.join(" ");
    assert!(
      import_args.contains("xtask import-page-fixture"),
      "import-page-fixture invocation missing: {import_args}"
    );
    assert!(
      import_args.contains(expected_bundle.to_string_lossy().as_ref()),
      "import should reference the captured bundle path: {import_args}"
    );
    assert!(
      import_args.contains(&format!(
        "--output-root {}",
        fixtures_root.to_string_lossy()
      )),
      "import should use the configured fixtures root: {import_args}"
    );
    assert!(
      import_args.contains("--overwrite"),
      "import should include --overwrite when configured: {import_args}"
    );
    assert!(
      import_args.contains("--allow-missing"),
      "import should include --allow-missing when configured: {import_args}"
    );
  }
}

