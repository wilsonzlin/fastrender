use fastrender::css::parser::parse_stylesheet;
use fastrender::dom;
use fastrender::style::cascade::{apply_styles_with_media, StyledNode};
use fastrender::style::media::MediaContext;
use fastrender::style::types::{TextOrientation, WritingMode};

fn find_tag(node: &StyledNode, tag: &str) -> Option<StyledNode> {
    if let fastrender::dom::DomNodeType::Element { tag_name, .. } = &node.node.node_type {
        if tag_name.eq_ignore_ascii_case(tag) {
            return Some(node.clone());
        }
    }
    for child in &node.children {
        if let Some(found) = find_tag(child, tag) {
            return Some(found);
        }
    }
    None
}

#[test]
fn text_orientation_inherits_from_parent() {
    let dom = dom::parse_html(r#"<div><span>text</span></div>"#).expect("parse html");
    let css = r#"
        div { writing-mode: vertical-rl; text-orientation: upright; }
        span { }
    "#;
    let stylesheet = parse_stylesheet(css).expect("parse css");
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

    let div = find_tag(&styled, "div").expect("div present");
    assert_eq!(div.styles.writing_mode, WritingMode::VerticalRl);
    assert_eq!(div.styles.text_orientation, TextOrientation::Upright);

    let span = find_tag(&styled, "span").expect("span present");
    // text-orientation is inherited, so the span should also be upright by default.
    assert_eq!(span.styles.text_orientation, TextOrientation::Upright);
}
