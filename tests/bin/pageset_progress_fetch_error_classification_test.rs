#[path = "../../src/bin/pageset_progress.rs"]
mod pageset_progress;

use fastrender::api::{ResourceFetchError, ResourceKind};

fn make_fetch_error(
  kind: ResourceKind,
  url: &str,
  status: Option<u16>,
  final_url: Option<&str>,
  message: &str,
) -> ResourceFetchError {
  ResourceFetchError {
    kind,
    url: url.to_string(),
    message: message.to_string(),
    status,
    final_url: final_url.map(|u| u.to_string()),
    etag: None,
    last_modified: None,
  }
}

#[test]
fn bot_mitigation_classifier_requires_status_and_marker() {
  let blocked = make_fetch_error(
    ResourceKind::Image,
    "https://example.com/image.jpg",
    Some(405),
    Some("https://example.com/image.jpg?captcha=123"),
    "HTTP status 405",
  );
  assert!(pageset_progress::is_bot_mitigation_fetch_error(&blocked));

  let blocked_font = make_fetch_error(
    ResourceKind::Font,
    "https://example.com/font.woff2",
    Some(405),
    Some("https://example.com/font.woff2?captcha=123"),
    "HTTP status 405",
  );
  assert!(pageset_progress::is_bot_mitigation_fetch_error(
    &blocked_font
  ));

  let missing_marker = make_fetch_error(
    ResourceKind::Image,
    "https://example.com/image.jpg",
    Some(405),
    None,
    "HTTP status 405",
  );
  assert!(!pageset_progress::is_bot_mitigation_fetch_error(
    &missing_marker
  ));

  let missing_marker_font = make_fetch_error(
    ResourceKind::Font,
    "https://example.com/font.woff2",
    Some(405),
    None,
    "HTTP status 405",
  );
  assert!(!pageset_progress::is_bot_mitigation_fetch_error(
    &missing_marker_font
  ));

  let missing_status = make_fetch_error(
    ResourceKind::Image,
    "https://example.com/image.jpg?captcha=123",
    None,
    None,
    "connection error",
  );
  assert!(!pageset_progress::is_bot_mitigation_fetch_error(
    &missing_status
  ));
}

#[test]
fn external_network_failure_classifier_is_narrow() {
  let dns_error = make_fetch_error(
    ResourceKind::Stylesheet,
    "https://cdn.example.com/all.css",
    None,
    None,
    "dns error: failed to lookup address information",
  );
  assert!(pageset_progress::is_external_network_failure_fetch_error(
    &dns_error
  ));

  let http_error = make_fetch_error(
    ResourceKind::Stylesheet,
    "https://cdn.example.com/all.css",
    Some(404),
    None,
    "HTTP status 404",
  );
  assert!(!pageset_progress::is_external_network_failure_fetch_error(
    &http_error
  ));

  let invalid_url = make_fetch_error(
    ResourceKind::Stylesheet,
    "not a url",
    None,
    None,
    "error sending request for url",
  );
  assert!(!pageset_progress::is_external_network_failure_fetch_error(
    &invalid_url
  ));
}
