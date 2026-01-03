use std::fs;
use tempfile::TempDir;
use xtask::pageset_failure_fixtures::plan_missing_failure_fixtures;

fn write_progress(dir: &TempDir, stem: &str, status: &str) {
  let path = dir.path().join(format!("{stem}.json"));
  let json = format!(
    "{{\"url\":\"https://{stem}/\",\"status\":\"{status}\"}}",
    stem = stem,
    status = status
  );
  fs::write(path, json).expect("write progress json");
}

#[test]
fn selects_missing_fixtures_for_non_ok_pages() {
  let progress = TempDir::new().expect("progress tempdir");
  let fixtures = TempDir::new().expect("fixtures tempdir");

  // Non-json files should be ignored.
  fs::write(progress.path().join("README.md"), "ignored").expect("write readme");

  write_progress(&progress, "ok.test", "ok");
  write_progress(&progress, "timeout.test", "timeout");
  write_progress(&progress, "panic.test", "panic");
  write_progress(&progress, "error.test", "error");

  fs::create_dir_all(fixtures.path().join("timeout.test")).expect("create fixture dir");
  fs::write(fixtures.path().join("timeout.test/index.html"), "<!doctype html>")
    .expect("write fixture html");

  let plan = plan_missing_failure_fixtures(progress.path(), fixtures.path()).expect("plan");
  let failing: Vec<_> = plan.failing_pages.iter().map(|p| p.stem.as_str()).collect();
  assert_eq!(
    failing,
    vec!["error.test", "panic.test", "timeout.test"],
    "plan should include all non-ok pages (sorted)"
  );

  let existing: Vec<_> = plan
    .existing_fixtures
    .iter()
    .map(|p| p.stem.as_str())
    .collect();
  assert_eq!(existing, vec!["timeout.test"]);

  let missing: Vec<_> = plan
    .missing_fixtures
    .iter()
    .map(|p| p.stem.as_str())
    .collect();
  assert_eq!(missing, vec!["error.test", "panic.test"]);
}

