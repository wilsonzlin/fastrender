use fastrender::css::parser::parse_stylesheet;
use fastrender::dom;
use fastrender::style::cascade::{apply_styles_with_media, StyledNode};
use fastrender::style::media::MediaContext;

fn collect_by_tag(node: &StyledNode, tag: &str, out: &mut Vec<StyledNode>) {
    if node.node.tag_name() == Some(tag) {
        out.push(node.clone());
    }
    for child in &node.children {
        collect_by_tag(child, tag, out);
    }
}

#[test]
fn parses_integer_order() {
    let dom = dom::parse_html(r#"<div><span style="order: -2">a</span><span style="order: 5">b</span></div>"#).unwrap();
    let stylesheet = parse_stylesheet("").unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));
    let mut spans = Vec::new();
    collect_by_tag(&styled, "span", &mut spans);
    let orders: Vec<i32> = spans.iter().map(|n| n.styles.order).collect();
    assert_eq!(orders, vec![-2, 5]);
}

#[test]
fn ignores_non_integer_order() {
    let dom = dom::parse_html(r#"<div><span style="order: 1.5">a</span></div>"#).unwrap();
    let stylesheet = parse_stylesheet("").unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));
    let mut spans = Vec::new();
    collect_by_tag(&styled, "span", &mut spans);
    let order = spans.first().expect("span").styles.order;
    assert_eq!(order, 0);
}
