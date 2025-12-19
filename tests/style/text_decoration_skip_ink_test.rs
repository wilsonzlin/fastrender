use fastrender::css::parser::parse_stylesheet;
use fastrender::dom;
use fastrender::style::cascade::apply_styles_with_media;
use fastrender::style::media::MediaContext;

fn collect_by_tag<'a>(
    node: &'a fastrender::style::cascade::StyledNode,
    tag: &str,
    out: &mut Vec<&'a fastrender::style::cascade::StyledNode>,
) {
    if node.node.tag_name() == Some(tag) {
        out.push(node);
    }
    for child in &node.children {
        collect_by_tag(child, tag, out);
    }
}

#[test]
fn text_decoration_skip_ink_inherits() {
    let dom = dom::parse_html(r#"<div style="text-decoration-skip-ink: none"><span>child</span></div>"#).unwrap();
    let stylesheet = parse_stylesheet("").unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));
    let mut divs = Vec::new();
    collect_by_tag(&styled, "div", &mut divs);
    let parent = divs.first().expect("div");
    let mut spans = Vec::new();
    collect_by_tag(&styled, "span", &mut spans);
    let child = spans.first().expect("span");

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
    let mut divs = Vec::new();
    collect_by_tag(&styled, "div", &mut divs);
    let parent = divs.first().expect("div");
    let mut spans = Vec::new();
    collect_by_tag(&styled, "span", &mut spans);
    let child = spans.first().expect("span");

    assert_eq!(
        parent.styles.text_decoration_skip_ink,
        fastrender::style::types::TextDecorationSkipInk::All
    );
    assert_eq!(
        child.styles.text_decoration_skip_ink,
        fastrender::style::types::TextDecorationSkipInk::Auto
    );
}
