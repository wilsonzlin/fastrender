use fastrender::css::parser::parse_stylesheet;
use fastrender::style::cascade::apply_styles_with_media;
use fastrender::style::media::MediaContext;
use fastrender::{dom, DomNode};

fn bg_pos(node: &DomNode) -> (f32, f32) {
    let pos = &node.styles.background_position;
    (pos.x.to_percentage().unwrap_or(0.0), pos.y.to_percentage().unwrap_or(0.0))
}

#[test]
fn background_position_swapped_keywords_use_axes() {
    let dom = dom::parse_html(
        r#"<div style="background-position: left top"></div>
            <div style="background-position: top left"></div>"#,
    )
    .unwrap();
    let stylesheet = parse_stylesheet("bg-pos", r#""#, None).unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

    let left_top = &styled.children[0];
    let top_left = &styled.children[1];

    assert_eq!(bg_pos(left_top), (0.0, 0.0));
    assert_eq!(bg_pos(top_left), (0.0, 0.0));
}

#[test]
fn background_position_vertical_first_pair_resolves_axes() {
    let dom = dom::parse_html(r#"<div style="background-position: center bottom"></div>"#).unwrap();
    let stylesheet = parse_stylesheet("bg-pos", r#""#, None).unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

    let node = &styled.children[0];
    // center (x) bottom (y)
    assert_eq!(bg_pos(node), (50.0, 100.0));
}

#[test]
fn background_position_offsets_follow_keywords() {
    let dom = dom::parse_html(
        r#"<div style="background-position: right 10% top 20%"></div>"#,
    )
    .unwrap();
    let stylesheet = parse_stylesheet("bg-pos", r#""#, None).unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

    let node = &styled.children[0];
    // right 10% -> 90% x, top 20% -> 20% y
    assert_eq!(bg_pos(node), (90.0, 20.0));
}
