use fastrender::css::parser::parse_stylesheet;
use fastrender::dom;
use fastrender::style::cascade::{apply_styles_with_media, StyledNode};
use fastrender::style::media::MediaContext;

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

#[test]
fn text_decoration_skip_ink_inherits() {
    let dom = dom::parse_html(r#"<div style="text-decoration-skip-ink: none"><span>child</span></div>"#).unwrap();
    let stylesheet = parse_stylesheet("").unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

    let parent = find_first(&styled, "div").expect("div");
    let child = find_first(parent, "span").expect("span");

    assert_eq!(
        parent.styles.text_decoration_skip_ink,
        fastrender::style::types::TextDecorationSkipInk::None
    );
    assert_eq!(
        child.styles.text_decoration_skip_ink,
        fastrender::style::types::TextDecorationSkipInk::None
    );
}

#[test]
fn text_decoration_skip_ink_initial_resets_to_auto() {
    let dom = dom::parse_html(
        r#"<div style="text-decoration-skip-ink: all"><span style="text-decoration-skip-ink: initial">child</span></div>"#,
    )
    .unwrap();
    let stylesheet = parse_stylesheet("").unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

    let parent = find_first(&styled, "div").expect("div");
    let child = find_first(parent, "span").expect("span");

    assert_eq!(
        parent.styles.text_decoration_skip_ink,
        fastrender::style::types::TextDecorationSkipInk::All
    );
    assert_eq!(
        child.styles.text_decoration_skip_ink,
        fastrender::style::types::TextDecorationSkipInk::Auto
    );
}
