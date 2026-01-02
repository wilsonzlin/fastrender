#[path = "../../src/bin/pageset_progress.rs"]
mod pageset_progress;

use fastrender::api::{ResourceFetchError, ResourceKind};

#[test]
fn bot_mitigation_classifier_matches_expected_status_and_url_patterns() {
  assert!(
    pageset_progress::is_bot_mitigation_block(Some(405), "https://example.test/block?captcha=1234"),
    "expected 405 + captcha marker to match"
  );
  assert!(
    pageset_progress::is_bot_mitigation_block(Some(405), "https://example.test/block?CAPTCHA=1234"),
    "expected case-insensitive marker to match"
  );
  assert!(
    pageset_progress::is_bot_mitigation_block(Some(403), "https://example.test/block?captcha=1234"),
    "expected 403 + captcha marker to match"
  );
  assert!(
    pageset_progress::is_bot_mitigation_block(Some(429), "https://example.test/block?captcha=1234"),
    "expected 429 + captcha marker to match"
  );

  assert!(
    !pageset_progress::is_bot_mitigation_block(
      Some(404),
      "https://example.test/block?captcha=1234"
    ),
    "unexpected status codes should not match"
  );
  assert!(
    !pageset_progress::is_bot_mitigation_block(Some(405), "https://example.test/block"),
    "missing captcha marker should not match"
  );
}

#[test]
fn bot_mitigation_classifier_only_applies_to_subresources() {
  let blocked = ResourceFetchError {
    kind: ResourceKind::Image,
    url: "https://example.test/image.png".to_string(),
    message: "HTTP status 405".to_string(),
    status: Some(405),
    final_url: Some("https://example.test/block?captcha=1234".to_string()),
    etag: None,
    last_modified: None,
  };
  assert!(
    pageset_progress::is_bot_mitigation_fetch_error(&blocked),
    "expected image fetch to be classified as bot mitigation"
  );

  let doc_blocked = ResourceFetchError {
    kind: ResourceKind::Document,
    ..blocked
  };
  assert!(
    !pageset_progress::is_bot_mitigation_fetch_error(&doc_blocked),
    "document fetches should not be classified as bot mitigation blocks"
  );
}
