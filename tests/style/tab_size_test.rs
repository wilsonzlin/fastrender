use fastrender::css::parser::parse_stylesheet;
use fastrender::style::cascade::apply_styles_with_media;
use fastrender::style::media::MediaContext;
use fastrender::{dom, DomNode};

fn tab_size(node: &DomNode) -> (u16, Option<f32>) {
    (node.styles.tab_size, node.styles.tab_size_length)
}

#[test]
fn tab_size_parses_number() {
    let dom = dom::parse_html(r#"<div style="tab-size: 4">text</div>"#).unwrap();
    let stylesheet = parse_stylesheet("tab", r#""#, None).unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));
    let node = &styled.children[0];
    assert_eq!(tab_size(node), (4, None));
}

#[test]
fn tab_size_parses_length() {
    let dom = dom::parse_html(r#"<div style="tab-size: 16px">text</div>"#).unwrap();
    let stylesheet = parse_stylesheet("tab", r#""#, None).unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));
    let node = &styled.children[0];
    assert_eq!(tab_size(node), (8, Some(16.0))); // default size remains 8 when length is provided
}

#[test]
fn tab_size_inherits() {
    let dom = dom::parse_html(r#"<div style="tab-size: 2"><span>child</span></div>"#).unwrap();
    let stylesheet = parse_stylesheet("tab", r#""#, None).unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));
    let parent = &styled.children[0];
    let child = &parent.children[0];
    assert_eq!(tab_size(parent), (2, None));
    assert_eq!(tab_size(child), (2, None));
}
