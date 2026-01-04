use serde_json::json;
use std::fs;
use tempfile::tempdir;
use xtask::capture_accuracy_fixtures::{
  plan_capture_accuracy_fixtures, CaptureAccuracyFixturesArgs,
};

fn write_progress(dir: &std::path::Path, stem: &str, status: &str, diff_percent: Option<f64>) {
  let mut progress = serde_json::Map::new();
  progress.insert(
    "url".to_string(),
    json!(format!("https://{stem}.example.test/")),
  );
  progress.insert("status".to_string(), json!(status));
  if let Some(diff) = diff_percent {
    progress.insert(
      "accuracy".to_string(),
      json!({
        "baseline": "chrome",
        "diff_percent": diff
      }),
    );
  }

  let path = dir.join(format!("{stem}.json"));
  fs::write(&path, serde_json::to_string_pretty(&progress).unwrap())
    .unwrap_or_else(|_| panic!("write {}", path.display()));
}

#[test]
fn selects_top_and_threshold_pages_and_skips_existing_fixtures() {
  let temp = tempdir().expect("tempdir");
  let progress_dir = temp.path().join("progress/pages");
  fs::create_dir_all(&progress_dir).expect("create progress dir");

  write_progress(&progress_dir, "ok_no_accuracy", "ok", None);
  write_progress(&progress_dir, "ok_low_diff", "ok", Some(0.1));
  write_progress(&progress_dir, "ok_mid_diff", "ok", Some(2.0));
  write_progress(&progress_dir, "ok_high_diff", "ok", Some(5.0));
  write_progress(&progress_dir, "ok_tie_a", "ok", Some(3.0));
  write_progress(&progress_dir, "ok_tie_b", "ok", Some(3.0));
  write_progress(&progress_dir, "timeout_page", "timeout", Some(50.0));

  let fixtures_root = temp.path().join("fixtures");
  fs::create_dir_all(fixtures_root.join("ok_tie_b")).expect("create fixture dir");
  fs::write(
    fixtures_root.join("ok_tie_b/index.html"),
    "existing fixture",
  )
  .expect("write fixture index");

  let bundles_out = temp.path().join("bundles");
  let cache_dir = temp.path().join("asset-cache");

  let args = CaptureAccuracyFixturesArgs {
    progress_dir,
    fixtures_root: fixtures_root.clone(),
    bundle_out_dir: bundles_out.clone(),
    asset_cache_dir: cache_dir.clone(),
    user_agent: Some("TestAgent/1.0".to_string()),
    accept_language: Some("en-US,en".to_string()),
    viewport: Some("800x600".to_string()),
    dpr: Some("2".to_string()),
    allow_missing_resources: true,
    overwrite: false,
    min_diff_percent: 2.5,
    top: 2,
  };

  let plan = plan_capture_accuracy_fixtures(&args).expect("plan fixtures");
  assert_eq!(plan.ok_accuracy_pages_total, 5);
  assert_eq!(plan.selected_pages_total, 3);
  assert_eq!(plan.fixtures_already_present, 1);
  assert_eq!(plan.captures.len(), 2);

  assert_eq!(plan.captures[0].stem, "ok_high_diff");
  assert_eq!(plan.captures[1].stem, "ok_tie_a");

  for capture in &plan.captures {
    let expected_bundle = bundles_out.join(format!("{}.tar", capture.stem));
    assert_eq!(capture.bundle_path, expected_bundle);

    let bundle_args = capture.bundle_command.args.join(" ");
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
      "bundle command should write to expected bundle path: {bundle_args}"
    );
    assert!(
      bundle_args.contains(&format!(
        "--asset-cache-dir {}",
        cache_dir.to_string_lossy()
      )),
      "bundle command should use configured asset cache dir: {bundle_args}"
    );
    assert!(
      bundle_args.contains("--allow-missing"),
      "bundle command should forward --allow-missing: {bundle_args}"
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
      bundle_args.contains("--viewport 800x600"),
      "bundle command should forward --viewport: {bundle_args}"
    );
    assert!(
      bundle_args.contains("--dpr 2"),
      "bundle command should forward --dpr: {bundle_args}"
    );

    let import_args = capture.import_command.args.join(" ");
    assert!(
      import_args.contains("xtask import-page-fixture"),
      "import-page-fixture invocation missing: {import_args}"
    );
    assert!(
      import_args.contains(expected_bundle.to_string_lossy().as_ref()),
      "import should reference captured bundle path: {import_args}"
    );
    assert!(
      import_args.contains(&format!(
        "--output-root {}",
        fixtures_root.to_string_lossy()
      )),
      "import should use configured fixtures root: {import_args}"
    );
    assert!(
      !import_args.contains("--overwrite"),
      "import should not include --overwrite unless configured: {import_args}"
    );
    assert!(
      import_args.contains("--allow-missing"),
      "import should include --allow-missing when configured: {import_args}"
    );
  }
}

#[test]
fn top_selection_is_deterministic_and_overwrite_includes_existing_fixtures() {
  let temp = tempdir().expect("tempdir");
  let progress_dir = temp.path().join("progress/pages");
  fs::create_dir_all(&progress_dir).expect("create progress dir");

  write_progress(&progress_dir, "top_page", "ok", Some(1.0));
  write_progress(&progress_dir, "a_tie", "ok", Some(0.4));
  write_progress(&progress_dir, "c_tie", "ok", Some(0.4));

  let fixtures_root = temp.path().join("fixtures");
  fs::create_dir_all(fixtures_root.join("top_page")).expect("create fixture dir");
  fs::write(
    fixtures_root.join("top_page/index.html"),
    "existing fixture",
  )
  .expect("write fixture index");

  let args = CaptureAccuracyFixturesArgs {
    progress_dir,
    fixtures_root: fixtures_root.clone(),
    bundle_out_dir: temp.path().join("bundles"),
    asset_cache_dir: temp.path().join("asset-cache"),
    user_agent: None,
    accept_language: None,
    viewport: None,
    dpr: None,
    allow_missing_resources: false,
    overwrite: true,
    min_diff_percent: 5.0,
    top: 2,
  };

  let plan = plan_capture_accuracy_fixtures(&args).expect("plan fixtures");
  assert_eq!(plan.ok_accuracy_pages_total, 3);
  assert_eq!(plan.selected_pages_total, 2);
  assert_eq!(
    plan.fixtures_already_present, 1,
    "top_page fixture exists, but overwrite should still capture it"
  );
  assert_eq!(plan.captures.len(), 2);

  // 1.0 diff page should be first, then the tied 0.4 pages broken by stem ordering.
  assert_eq!(plan.captures[0].stem, "top_page");
  assert_eq!(plan.captures[1].stem, "a_tie");

  let import_args = plan.captures[0].import_command.args.join(" ");
  assert!(
    import_args.contains("--overwrite"),
    "expected import command to include --overwrite when overwrite=true: {import_args}"
  );
}
