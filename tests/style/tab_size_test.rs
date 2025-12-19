use fastrender::css::parser::parse_stylesheet;
use fastrender::dom;
use fastrender::style::cascade::{apply_styles_with_media, StyledNode};
use fastrender::style::media::MediaContext;
use fastrender::style::types::TabSize;
use fastrender::style::values::Length;

fn find_first<'a>(node: &'a StyledNode, tag: &str) -> Option<&'a StyledNode> {
    if let Some(name) = node.node.tag_name() {
        if name.eq_ignore_ascii_case(tag) {
            return Some(node);
        }
    }
    for child in &node.children {
        if let Some(found) = find_first(child, tag) {
            return Some(found);
        }
    }
    None
}

fn tab_size(node: &StyledNode) -> TabSize {
    node.styles.tab_size
}

#[test]
fn tab_size_parses_number() {
    let dom = dom::parse_html(r#"<div style="tab-size: 4">text</div>"#).unwrap();
    let stylesheet = parse_stylesheet("").unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));
    let node = find_first(&styled, "div").expect("div");
    assert!(matches!(tab_size(node), TabSize::Number(n) if (n - 4.0).abs() < 1e-6));
}

#[test]
fn tab_size_parses_length() {
    let dom = dom::parse_html(r#"<div style="tab-size: 16px">text</div>"#).unwrap();
    let stylesheet = parse_stylesheet("").unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));
    let node = find_first(&styled, "div").expect("div");
    assert_eq!(tab_size(node), TabSize::Length(Length::px(16.0)));
}

#[test]
fn tab_size_inherits() {
    let dom = dom::parse_html(r#"<div style="tab-size: 2"><span>child</span></div>"#).unwrap();
    let stylesheet = parse_stylesheet("").unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));
    let parent = find_first(&styled, "div").expect("div");
    let child = find_first(parent, "span").expect("span");
    assert_eq!(tab_size(parent), TabSize::Number(2.0));
    assert_eq!(tab_size(child), TabSize::Number(2.0));
}
