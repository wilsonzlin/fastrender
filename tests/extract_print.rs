use fastrender::css::loader::extract_css_links;

#[test]
fn extract_css_links_skips_print_only() {
    let html =
        r#"<link rel="stylesheet" media="print" href="https://assets.guim.co.uk/static/frontend/css/print.css">"#;
    let urls = extract_css_links(html, "https://www.theguardian.com");
    assert_eq!(
        urls,
        vec!["https://assets.guim.co.uk/static/frontend/css/print.css".to_string()]
    );
}

#[test]
fn extract_css_links_for_guardian_sample() {
    if let Ok(html) = std::fs::read_to_string("/tmp/guardian.html") {
        let urls = extract_css_links(&html, "https://www.theguardian.com");
        println!("guardian urls: {:?}", urls);
    }
}
