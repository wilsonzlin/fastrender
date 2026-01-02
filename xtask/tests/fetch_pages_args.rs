use xtask::{extract_fetch_pages_flag_overrides, FetchPagesFlagOverrides};

#[test]
fn extracts_fetch_pages_flags() {
  let extra = vec![
    "--foo".to_string(),
    "--refresh".to_string(),
    "--allow-http-error-status".to_string(),
    "--bar=baz".to_string(),
  ];

  let (filtered, overrides) = extract_fetch_pages_flag_overrides(&extra);
  assert_eq!(
    filtered,
    vec!["--foo".to_string(), "--bar=baz".to_string()],
    "fetch_pages-only flags should be stripped from the forwarded args"
  );
  assert_eq!(
    overrides,
    FetchPagesFlagOverrides {
      allow_http_error_status: true,
      refresh: true,
    }
  );
}

