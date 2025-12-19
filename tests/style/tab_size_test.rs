use fastrender::css::parser::parse_stylesheet;
use fastrender::dom;
use fastrender::style::cascade::{apply_styles_with_media, StyledNode};
use fastrender::style::media::MediaContext;
use fastrender::style::types::TabSize;

fn tab_size(node: &StyledNode) -> &TabSize {
    &node.styles.tab_size
}

fn collect_by_tag<'a>(node: &'a StyledNode, tag: &str, out: &mut Vec<&'a StyledNode>) {
    if node.node.tag_name() == Some(tag) {
        out.push(node);
    }
    for child in &node.children {
        collect_by_tag(child, tag, out);
    }
}

#[test]
fn tab_size_parses_number() {
    let dom = dom::parse_html(r#"<div style="tab-size: 4">text</div>"#).unwrap();
    let stylesheet = parse_stylesheet("").unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));
    let mut divs = Vec::new();
    collect_by_tag(&styled, "div", &mut divs);
    let node = divs.first().expect("div");
    assert!(matches!(tab_size(node), TabSize::Number(n) if (n - 4.0).abs() < 0.001));
}

#[test]
fn tab_size_parses_length() {
    let dom = dom::parse_html(r#"<div style="tab-size: 16px">text</div>"#).unwrap();
    let stylesheet = parse_stylesheet("").unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));
    let mut divs = Vec::new();
    collect_by_tag(&styled, "div", &mut divs);
    let node = divs.first().expect("div");
    assert!(matches!(tab_size(node), TabSize::Length(len) if (len.to_px() - 16.0).abs() < 0.001));
}

#[test]
fn tab_size_inherits() {
    let dom = dom::parse_html(r#"<div style="tab-size: 2"><span>child</span></div>"#).unwrap();
    let stylesheet = parse_stylesheet("").unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));
    let mut divs = Vec::new();
    collect_by_tag(&styled, "div", &mut divs);
    let parent = divs.first().expect("div");
    let mut spans = Vec::new();
    collect_by_tag(&styled, "span", &mut spans);
    let child = spans.first().expect("span");
    assert!(matches!(tab_size(parent), TabSize::Number(n) if (n - 2.0).abs() < 0.001));
    assert!(matches!(tab_size(child), TabSize::Number(n) if (n - 2.0).abs() < 0.001));
}
