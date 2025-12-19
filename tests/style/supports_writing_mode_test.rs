use fastrender::css::parser::parse_stylesheet;
use fastrender::dom;
use fastrender::style::cascade::{apply_styles_with_media, StyledNode};
use fastrender::style::media::MediaContext;

fn display(node: &StyledNode) -> String {
    node.styles.display.to_string()
}

#[test]
fn supports_declaration_accepts_writing_mode_vertical_rl() {
    let dom = dom::parse_html(r#"<div></div>"#).unwrap();
    let css = r#"@supports (writing-mode: vertical-rl) { div { display: inline; } }"#;
    let stylesheet = parse_stylesheet(css).unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

    assert_eq!(display(&styled.children[0]), "inline");
}

#[test]
fn supports_declaration_rejects_invalid_writing_mode() {
    let dom = dom::parse_html(r#"<div></div>"#).unwrap();
    let css = r#"@supports (writing-mode: sideways-up) { div { display: inline; } }"#;
    let stylesheet = parse_stylesheet(css).unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

    // Invalid value should cause the @supports block to be ignored.
    assert_eq!(display(&styled.children[0]), "block");
}
