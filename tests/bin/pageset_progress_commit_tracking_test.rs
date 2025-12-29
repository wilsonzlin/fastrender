#[path = "../../src/bin/pageset_progress.rs"]
mod pageset_progress;

use pageset_progress::{current_git_sha, PageProgress};
use serde_json::json;

fn progress_with_status(
  status: &str,
  last_good_commit: &str,
  last_regression_commit: &str,
) -> PageProgress {
  serde_json::from_value(json!({
    "url": "https://example.test/",
    "status": status,
    "total_ms": 1.0,
    "stages_ms": {
      "fetch": 0.0,
      "css": 0.0,
      "cascade": 0.0,
      "layout": 0.0,
      "paint": 0.0
    },
    "notes": "",
    "hotspot": "",
    "last_good_commit": last_good_commit,
    "last_regression_commit": last_regression_commit
  }))
  .expect("progress JSON parses")
}

fn last_good_commit(progress: &PageProgress) -> String {
  serde_json::to_value(progress).expect("serialize progress")["last_good_commit"]
    .as_str()
    .unwrap_or("")
    .to_string()
}

fn last_regression_commit(progress: &PageProgress) -> String {
  serde_json::to_value(progress).expect("serialize progress")["last_regression_commit"]
    .as_str()
    .unwrap_or("")
    .to_string()
}

#[test]
fn ok_to_timeout_records_regression_commit_once() {
  let previous = progress_with_status("ok", "good123", "");
  let next = progress_with_status("timeout", "", "");
  let merged = next.merge_preserving_manual(Some(previous), Some("reg456"));

  assert_eq!(last_regression_commit(&merged), "reg456");
  assert_eq!(last_good_commit(&merged), "good123");
}

#[test]
fn timeout_to_ok_sets_last_good_commit() {
  let previous = progress_with_status("timeout", "", "reg111");
  let next = progress_with_status("ok", "", "");
  let merged = next.merge_preserving_manual(Some(previous), Some("good222"));

  assert_eq!(last_good_commit(&merged), "good222");
  assert_eq!(last_regression_commit(&merged), "reg111");
}

#[test]
fn ok_to_ok_keeps_last_good_stable() {
  let previous = progress_with_status("ok", "stable_good", "");
  let next = progress_with_status("ok", "", "");
  let merged = next.merge_preserving_manual(Some(previous), Some("newsha"));

  assert_eq!(last_good_commit(&merged), "stable_good");
  assert_eq!(last_regression_commit(&merged), "");
}

#[test]
fn timeout_to_timeout_keeps_commits_stable() {
  let previous = progress_with_status("timeout", "historic_good", "historic_regression");
  let next = progress_with_status("timeout", "", "");
  let merged = next.merge_preserving_manual(Some(previous), Some("othersha"));

  assert_eq!(last_good_commit(&merged), "historic_good");
  assert_eq!(last_regression_commit(&merged), "historic_regression");
}

#[test]
fn current_git_sha_uses_env_fallback() {
  let expected = "env-fallback-sha";
  let original_path = std::env::var("PATH").ok();
  let original_fastr = std::env::var("FASTR_GIT_SHA").ok();
  let original_github = std::env::var("GITHUB_SHA").ok();

  std::env::set_var("PATH", "");
  std::env::set_var("FASTR_GIT_SHA", expected);
  std::env::set_var("GITHUB_SHA", expected);

  let detected = current_git_sha();

  match original_path {
    Some(val) => std::env::set_var("PATH", val),
    None => std::env::remove_var("PATH"),
  }
  match original_fastr {
    Some(val) => std::env::set_var("FASTR_GIT_SHA", val),
    None => std::env::remove_var("FASTR_GIT_SHA"),
  }
  match original_github {
    Some(val) => std::env::set_var("GITHUB_SHA", val),
    None => std::env::remove_var("GITHUB_SHA"),
  }

  assert_eq!(detected.as_deref(), Some(expected));
}
