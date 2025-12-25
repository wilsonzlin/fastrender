use fastrender::error::{Error, NavigationError, ParseError, ResourceError};

#[test]
fn parse_error_display_snapshot() {
  let err = Error::Parse(ParseError::InvalidCss {
    message: "unexpected token".to_string(),
    line: 5,
    column: 2,
  });
  assert_eq!(
    err.to_string(),
    "[parse] Invalid CSS at line 5, column 2: unexpected token"
  );
}

#[test]
fn resource_error_display_snapshot() {
  let err = Error::Resource(
    ResourceError::new("https://example.test/style.css", "not found")
      .with_status(404)
      .with_final_url("https://example.test/style.css?v=1"),
  );
  assert_eq!(
    err.to_string(),
    "[resource] https://example.test/style.css?v=1 (status 404): not found"
  );
}

#[test]
fn navigation_error_display_snapshot() {
  let err = Error::Navigation(NavigationError::FetchFailed {
    url: "https://example.test/doc".to_string(),
    reason: "backend unavailable".to_string(),
    source: None,
  });
  assert_eq!(
    err.to_string(),
    "[resource] Failed to fetch document 'https://example.test/doc': backend unavailable"
  );
}
