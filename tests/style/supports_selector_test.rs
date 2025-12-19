use fastrender::css::parser::parse_stylesheet;
use fastrender::style::cascade::apply_styles_with_media;
use fastrender::style::media::MediaContext;
use fastrender::{dom, DomNode};

fn display(node: &DomNode) -> String {
    node.styles.display.to_string()
}

#[test]
fn supports_selector_true_for_supported_selector() {
    let dom = dom::parse_html(r#"<div class="foo"></div>"#).unwrap();
    let css = r#"@supports selector(.foo) { div { display: inline; } }"#;
    let stylesheet = parse_stylesheet("supports", css, None).unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

    assert_eq!(display(&styled.children[0]), "inline");
}

#[test]
fn supports_selector_false_for_unsupported_selector() {
    let dom = dom::parse_html(r#"<div></div>"#).unwrap();
    let css = r#"@supports selector(:has(div)) { div { display: inline; } }"#;
    let stylesheet = parse_stylesheet("supports", css, None).unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

    assert_eq!(display(&styled.children[0]), "block");
}

#[test]
fn supports_declaration_accepts_valid_text_orientation() {
    let dom = dom::parse_html(r#"<div></div>"#).unwrap();
    let css = r#"@supports (text-orientation: upright) { div { display: inline; } }"#;
    let stylesheet = parse_stylesheet("supports", css, None).unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

    assert_eq!(display(&styled.children[0]), "inline");
}

#[test]
fn supports_declaration_rejects_invalid_text_orientation() {
    let dom = dom::parse_html(r#"<div></div>"#).unwrap();
    let css = r#"@supports (text-orientation: sideways-left) { div { display: inline; } }"#;
    let stylesheet = parse_stylesheet("supports", css, None).unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

    // The invalid value should cause the @supports condition to be false, leaving display as block.
    assert_eq!(display(&styled.children[0]), "block");
}

#[test]
fn supports_declaration_accepts_text_combine_digits() {
    let dom = dom::parse_html(r#"<div></div>"#).unwrap();
    let css = r#"@supports (text-combine-upright: digits 3) { div { display: inline; } }"#;
    let stylesheet = parse_stylesheet("supports", css, None).unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

    assert_eq!(display(&styled.children[0]), "inline");
}

#[test]
fn supports_declaration_rejects_text_combine_invalid_digits() {
    let dom = dom::parse_html(r#"<div></div>"#).unwrap();
    let css = r#"@supports (text-combine-upright: digits 5) { div { display: inline; } }"#;
    let stylesheet = parse_stylesheet("supports", css, None).unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

    assert_eq!(display(&styled.children[0]), "block");
}
