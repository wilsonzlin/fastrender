use fastrender::text::color_fonts::{sanitize_svg_glyph_for_tests, MAX_SVG_GLYPH_BYTES};

#[test]
fn svg_glyph_rejects_file_image_href() {
  let svg =
    r#"<svg xmlns="http://www.w3.org/2000/svg"><image href="file:///tmp/evil.png"/></svg>"#;
  assert!(sanitize_svg_glyph_for_tests(svg.as_bytes()).is_none());
}

#[test]
fn svg_glyph_rejects_oversized_content() {
  let limit = MAX_SVG_GLYPH_BYTES;
  let mut svg = String::with_capacity(limit + 32);
  svg.push_str(r#"<svg xmlns="http://www.w3.org/2000/svg">"#);
  svg.push_str(&"a".repeat(limit + 1));
  svg.push_str("</svg>");

  assert!(svg.as_bytes().len() > limit);
  assert!(sanitize_svg_glyph_for_tests(svg.as_bytes()).is_none());
}

#[test]
fn svg_glyph_allows_fragment_use_reference() {
  let svg = r#"
    <svg xmlns="http://www.w3.org/2000/svg">
      <defs><path id="p" d="M0 0h10v10z"/></defs>
      <use href="#p"/>
    </svg>
  "#;

  assert!(sanitize_svg_glyph_for_tests(svg.as_bytes()).is_some());
}
