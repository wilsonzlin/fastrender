use fastrender::css::parser::parse_stylesheet;
use fastrender::dom;
use fastrender::style::cascade::{apply_styles_with_media, StyledNode};
use fastrender::style::media::MediaContext;

fn display(node: &StyledNode) -> String {
    node.styles.display.to_string()
}

fn first_div<'a>(node: &'a StyledNode) -> Option<&'a StyledNode> {
    if node.node.tag_name() == Some("div") {
        return Some(node);
    }
    for child in &node.children {
        if let Some(found) = first_div(child) {
            return Some(found);
        }
    }
    None
}

#[test]
fn supports_selector_true_for_supported_selector() {
    let dom = dom::parse_html(r#"<div class="foo"></div>"#).unwrap();
    let css = r#"@supports selector(.foo) { div { display: inline; } }"#;
    let stylesheet = parse_stylesheet(css).unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

    let div = first_div(&styled).expect("div");
    assert_eq!(display(div), "inline");
}

#[test]
fn supports_selector_false_for_unsupported_selector() {
    let dom = dom::parse_html(r#"<div></div>"#).unwrap();
    let css = r#"@supports selector(:has(div)) { div { display: inline; } }"#;
    let stylesheet = parse_stylesheet(css).unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

    let div = first_div(&styled).expect("div");
    assert_eq!(display(div), "block");
}

#[test]
fn supports_declaration_accepts_valid_text_orientation() {
    let dom = dom::parse_html(r#"<div></div>"#).unwrap();
    let css = r#"@supports (text-orientation: upright) { div { display: inline; } }"#;
    let stylesheet = parse_stylesheet(css).unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

    let div = first_div(&styled).expect("div");
    assert_eq!(display(div), "inline");
}

#[test]
fn supports_declaration_rejects_invalid_text_orientation() {
    let dom = dom::parse_html(r#"<div></div>"#).unwrap();
    let css = r#"@supports (text-orientation: sideways-invalid) { div { display: inline; } }"#;
    let stylesheet = parse_stylesheet(css).unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

    // The invalid value should cause the @supports condition to be false, leaving display as block.
    let div = first_div(&styled).expect("div");
    assert_eq!(display(div), "block");
}

#[test]
fn supports_declaration_accepts_text_combine_digits() {
    let dom = dom::parse_html(r#"<div></div>"#).unwrap();
    let css = r#"@supports (text-combine-upright: digits 3) { div { display: inline; } }"#;
    let stylesheet = parse_stylesheet(css).unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

    let div = first_div(&styled).expect("div");
    assert_eq!(display(div), "inline");
}

#[test]
fn supports_declaration_rejects_text_combine_invalid_digits() {
    let dom = dom::parse_html(r#"<div></div>"#).unwrap();
    let css = r#"@supports (text-combine-upright: digits 5) { div { display: inline; } }"#;
    let stylesheet = parse_stylesheet(css).unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

    let div = first_div(&styled).expect("div");
    assert_eq!(display(div), "block");
}
