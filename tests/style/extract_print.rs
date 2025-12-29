use fastrender::css::loader::extract_css_links;
use fastrender::style::media::MediaType;

#[test]
fn extract_css_links_skips_print_only_for_screen() {
  let html = r#"<link rel="stylesheet" media="print" href="https://assets.guim.co.uk/static/frontend/css/print.css">"#;
  let urls = extract_css_links(html, "https://www.theguardian.com", MediaType::Screen).unwrap();
  assert!(
    urls.is_empty(),
    "print-only should be ignored for screen context"
  );

  let print_urls =
    extract_css_links(html, "https://www.theguardian.com", MediaType::Print).unwrap();
  assert_eq!(
    print_urls,
    vec!["https://assets.guim.co.uk/static/frontend/css/print.css".to_string()]
  );
}
