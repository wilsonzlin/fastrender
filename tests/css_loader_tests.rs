use fastrender::css::loader::absolutize_css_urls;

#[test]
fn absolutize_ignores_comments_and_strings() {
  let css = r#"
    /* url('should-not-change.png') */
    .icon::before { content: "url(in-string.png)"; }
    body { background: url(images/bg.png); }
  "#;

  let out = absolutize_css_urls(css, "https://example.com/static/main.css");
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

  let out = absolutize_css_urls(css, "https://example.com/assets/css/main.css");
  assert!(out.contains("url(\"https://example.com/assets/reset.css\")"));
  assert!(out.contains("url(\"https://example.com/assets/fonts/font.woff2\")"));
}

#[test]
fn absolutize_rewrites_nested_image_set_urls() {
  let css = "div { background-image: image-set(url(./a.png) 1x, url('../b.png') 2x); }";
  let out = absolutize_css_urls(css, "https://example.com/styles/site.css");
  assert!(out.contains("url(\"https://example.com/styles/a.png\")"));
  assert!(out.contains("url(\"https://example.com/b.png\")"));
}

#[test]
fn absolutize_handles_data_urls_with_parentheses() {
  let data_url = "data:image/svg+xml,<svg>text)</svg>";
  let css = format!("div {{ background: url(\"{data_url}\"); }}");
  let out = absolutize_css_urls(&css, "https://example.com/app/site.css");
  assert!(out.contains(&format!("url(\"{data_url}\")")));
}

#[test]
fn absolutize_handles_escaped_paren_in_url_function() {
  let css = r#"div { mask: url("icons/close\).svg"); }"#;
  let out = absolutize_css_urls(css, "https://example.com/theme/style.css");
  assert!(out.contains("url(\"https://example.com/theme/icons/close).svg\")"));
}

#[test]
fn absolutize_handles_uppercase_url_function_and_whitespace() {
  let css = "div { background-image: URL(   './img/bg.png'  ); }";
  let out = absolutize_css_urls(css, "https://example.com/css/main.css");
  assert!(out.contains("url(\"https://example.com/css/img/bg.png\")"));
}

#[test]
fn absolutize_rewrites_inside_parenthesized_blocks() {
  let css = "@supports (background: url('../check.png')) { body { background: url(./inner.png); } }";
  let out = absolutize_css_urls(css, "https://example.com/styles/app.css");
  assert!(out.contains("url(\"https://example.com/check.png\")"));
  assert!(out.contains("url(\"https://example.com/styles/inner.png\")"));
}

#[test]
fn absolutize_escapes_quotes_and_backslashes() {
  let css = r#"div { background: url("images/sp\"ace\\ path.png"); }"#;
  let out = absolutize_css_urls(css, "https://example.com/css/main.css");
  assert!(out.contains("url(\"https://example.com/css/images/sp\\\"ace\\\\ path.png\")"));
}

#[test]
fn absolutize_rewrites_protocol_relative_urls() {
  let css = "body { background-image: url(//cdn.example.com/bg.png); }";
  let out = absolutize_css_urls(css, "https://example.com/a.css");
  assert!(out.contains("url(\"https://cdn.example.com/bg.png\")"));
}

#[test]
fn absolutize_leaves_urls_inside_comments_untouched() {
  let css = "/* gradient url(foo.png) */ div { background: linear-gradient(red, blue); }";
  let out = absolutize_css_urls(css, "https://example.com/base.css");
  assert!(out.contains("/* gradient url(foo.png) */"));
}
