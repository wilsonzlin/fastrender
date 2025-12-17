use fastrender::css::parser::parse_stylesheet;
use fastrender::style::cascade::apply_styles_with_media;
use fastrender::style::media::MediaContext;
use fastrender::{dom, DomNode};

#[test]
fn text_decoration_skip_ink_inherits() {
    let dom = dom::parse_html(r#"<div style="text-decoration-skip-ink: none"><span>child</span></div>"#).unwrap();
    let stylesheet = parse_stylesheet("skip-ink-test", r#""#, None).unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

    let parent = &styled.children[0];
    let child = &parent.children[0];

    assert_eq!(parent.styles.text_decoration_skip_ink, fastrender::style::types::TextDecorationSkipInk::None);
    assert_eq!(child.styles.text_decoration_skip_ink, fastrender::style::types::TextDecorationSkipInk::None);
}

#[test]
fn text_decoration_skip_ink_initial_resets_to_auto() {
    let dom = dom::parse_html(
        r#"<div style="text-decoration-skip-ink: all"><span style="text-decoration-skip-ink: initial">child</span></div>"#,
    )
    .unwrap();
    let stylesheet = parse_stylesheet("skip-ink-test", r#""#, None).unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

    let parent = &styled.children[0];
    let child = &parent.children[0];

    assert_eq!(parent.styles.text_decoration_skip_ink, fastrender::style::types::TextDecorationSkipInk::All);
    assert_eq!(child.styles.text_decoration_skip_ink, fastrender::style::types::TextDecorationSkipInk::Auto);
}
