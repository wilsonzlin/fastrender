use fastrender::css::loader::{
  absolutize_css_urls, infer_base_url, inline_imports_with_diagnostics, resolve_href,
  InlineImportState,
};

#[test]
fn absolutize_ignores_comments_and_strings() {
  let css = r#"
    /* url('should-not-change.png') */
    .icon::before { content: "url(in-string.png)"; }
    body { background: url(images/bg.png); }
  "#;

  let out = absolutize_css_urls(css, "https://example.com/static/main.css").unwrap();
  assert!(out.contains("/* url('should-not-change.png') */"));
  assert!(out.contains("content: \"url(in-string.png)\";"));
  assert!(out.contains("url(\"https://example.com/static/images/bg.png\")"));
}

#[test]
fn absolutize_rewrites_import_and_font_face_sources() {
  let css = r#"
    @import url('../reset.css');
    @font-face { src: url('../fonts/font.woff2') format('woff2'); }
  "#;

  let out = absolutize_css_urls(css, "https://example.com/assets/css/main.css").unwrap();
  assert!(out.contains("url(\"https://example.com/assets/reset.css\")"));
  assert!(out.contains("url(\"https://example.com/assets/fonts/font.woff2\")"));
}

#[test]
fn absolutize_rewrites_nested_image_set_urls() {
  let css = "div { background-image: image-set(url(./a.png) 1x, url('../b.png') 2x); }";
  let out = absolutize_css_urls(css, "https://example.com/styles/site.css").unwrap();
  assert!(out.contains("url(\"https://example.com/styles/a.png\")"));
  assert!(out.contains("url(\"https://example.com/b.png\")"));
}

#[test]
fn absolutize_handles_data_urls_with_parentheses() {
  let data_url = "data:image/svg+xml,<svg>text)</svg>";
  let css = format!("div {{ background: url(\"{data_url}\"); }}");
  let out = absolutize_css_urls(&css, "https://example.com/app/site.css").unwrap();
  assert!(out.contains(&format!("url(\"{data_url}\")")));
}

#[test]
fn absolutize_handles_escaped_paren_in_url_function() {
  let css = r#"div { mask: url("icons/close\).svg"); }"#;
  let out = absolutize_css_urls(css, "https://example.com/theme/style.css").unwrap();
  assert!(out.contains("url(\"https://example.com/theme/icons/close).svg\")"));
}

#[test]
fn absolutize_handles_uppercase_url_function_and_whitespace() {
  let css = "div { background-image: URL(   './img/bg.png'  ); }";
  let out = absolutize_css_urls(css, "https://example.com/css/main.css").unwrap();
  assert!(out.contains("url(\"https://example.com/css/img/bg.png\")"));
}

#[test]
fn absolutize_rewrites_inside_parenthesized_blocks() {
  let css =
    "@supports (background: url('../check.png')) { body { background: url(./inner.png); } }";
  let out = absolutize_css_urls(css, "https://example.com/styles/app.css").unwrap();
  assert!(out.contains("url(\"https://example.com/check.png\")"));
  assert!(out.contains("url(\"https://example.com/styles/inner.png\")"));
}

#[test]
fn absolutize_escapes_quotes_and_backslashes() {
  let css = r#"div { background: url("images/sp\"ace\\ path.png"); }"#;
  let out = absolutize_css_urls(css, "https://example.com/css/main.css").unwrap();
  // The resolved URL percent-encodes the quotes/backslashes/spaces, but the rewriter
  // must still emit a valid quoted url() string.
  assert!(out.contains("url(\"https://example.com/css/images/sp%22ace%5C%20path.png\")"));
}

#[test]
fn absolutize_rewrites_protocol_relative_urls() {
  let css = "body { background-image: url(//cdn.example.com/bg.png); }";
  let out = absolutize_css_urls(css, "https://example.com/a.css").unwrap();
  assert!(out.contains("url(\"https://cdn.example.com/bg.png\")"));
}

#[test]
fn absolutize_leaves_urls_inside_comments_untouched() {
  let css = "/* gradient url(foo.png) */ div { background: linear-gradient(red, blue); }";
  let out = absolutize_css_urls(css, "https://example.com/base.css").unwrap();
  assert!(out.contains("/* gradient url(foo.png) */"));
}

#[test]
fn infer_base_url_honors_relative_base_href_with_trailing_slash() {
  let html = r#"<html><head><base href="static/"></head></html>"#;
  let base = infer_base_url(html, "https://example.com/site/page.html").into_owned();
  assert_eq!(base, "https://example.com/site/static/");

  let resolved = resolve_href(&base, "css/app.css").expect("resolved stylesheet URL");
  assert_eq!(resolved, "https://example.com/site/static/css/app.css");
}

#[test]
fn infer_base_url_preserves_file_like_base_href_without_trailing_slash() {
  let html = r#"<html><head><base href="assets"></head></html>"#;
  let base = infer_base_url(html, "https://example.com/root/page.html").into_owned();
  assert_eq!(base, "https://example.com/assets");

  let resolved = resolve_href(&base, "img/logo.png").expect("resolved asset URL");
  assert_eq!(resolved, "https://example.com/img/logo.png");
}

#[test]
fn inline_imports_resolves_urls_relative_to_imported_sheet() {
  let mut state = InlineImportState::new();
  let mut fetch = |url: &str| -> fastrender::error::Result<String> {
    assert_eq!(url, "https://example.com/styles/imports/inner.css");
    Ok("body { background: url(\"./img/bg.png\"); }".to_string())
  };
  let mut diagnostics = Vec::new();
  let mut diag = |url: &str, reason: &str| diagnostics.push((url.to_string(), reason.to_string()));

  let output = inline_imports_with_diagnostics(
    "@import \"imports/inner.css\";",
    "https://example.com/styles/main.css",
    &mut fetch,
    &mut state,
    &mut diag,
    None,
  )
  .unwrap();

  assert!(
    output.contains("url(\"https://example.com/styles/imports/img/bg.png\")"),
    "url() inside imported sheet should resolve against that sheet"
  );
  assert!(diagnostics.is_empty());
}

#[test]
fn inline_imports_reports_cycles() {
  let mut state = InlineImportState::new();
  state.register_stylesheet("https://example.com/main.css");
  let mut diagnostics = Vec::new();
  let mut diag = |url: &str, reason: &str| diagnostics.push((url.to_string(), reason.to_string()));

  let out = inline_imports_with_diagnostics(
    "@import url(\"main.css\");",
    "https://example.com/main.css",
    &mut |_url| -> fastrender::error::Result<String> { unreachable!("cycle should short-circuit") },
    &mut state,
    &mut diag,
    None,
  )
  .unwrap();

  assert!(out.trim().is_empty(), "cyclic imports should be skipped");
  assert!(
    diagnostics
      .iter()
      .any(|(url, reason)| url == "https://example.com/main.css" && reason.contains("cyclic")),
    "cycle diagnostics should be reported: {:?}",
    diagnostics
  );
}
