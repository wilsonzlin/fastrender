use fastrender::css::parser::parse_stylesheet;
use fastrender::style::cascade::apply_styles_with_media;
use fastrender::style::media::MediaContext;
use fastrender::{dom, DomNode};

#[test]
fn aria_label_does_not_change_display() {
    let dom = dom::parse_html(r#"<div aria-label="foo" style="display:block">text</div>"#).unwrap();
    let stylesheet = parse_stylesheet("aria-test", r#""#, None).unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

    let node: &DomNode = &styled.children[0];
    assert_eq!(node.styles.display.to_string(), "block");
    assert_eq!(node.styles.visibility.to_string(), "visible");
}

#[test]
fn aria_labelledby_does_not_hide() {
    let dom = dom::parse_html(r#"<div aria-labelledby="id1">text</div>"#).unwrap();
    let stylesheet = parse_stylesheet("aria-test", r#""#, None).unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

    let node: &DomNode = &styled.children[0];
    assert_eq!(node.styles.visibility.to_string(), "visible");
    assert!(!node.styles.is_hidden());
}
