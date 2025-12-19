use fastrender::css::parser::parse_stylesheet;
use fastrender::dom;
use fastrender::style::cascade::{apply_styles_with_media, StyledNode};
use fastrender::style::media::MediaContext;
use fastrender::style::types::BackgroundPosition;
use fastrender::style::values::LengthUnit;

fn collect_by_tag<'a>(node: &'a StyledNode, tag: &str, out: &mut Vec<&'a StyledNode>) {
    if node.node.tag_name() == Some(tag) {
        out.push(node);
    }
    for child in &node.children {
        collect_by_tag(child, tag, out);
    }
}

fn divs(root: &StyledNode) -> Vec<&StyledNode> {
    let mut out = Vec::new();
    collect_by_tag(root, "div", &mut out);
    out
}

fn bg_pos(node: &StyledNode) -> (f32, f32) {
    let pos = node.styles.background_positions.first().expect("background position");
    let (x, y) = match pos {
        BackgroundPosition::Position { x, y } => (x, y),
    };

    fn to_percent(comp: &fastrender::style::types::BackgroundPositionComponent) -> f32 {
        let offset_pct = match comp.offset.unit {
            LengthUnit::Percent => comp.offset.value,
            _ => 0.0,
        };
        comp.alignment * 100.0 + offset_pct
    }

    (to_percent(x), to_percent(y))
}

#[test]
fn background_position_swapped_keywords_use_axes() {
    let dom = dom::parse_html(
        r#"<div style="background-position: left top"></div>
            <div style="background-position: top left"></div>"#,
    )
    .unwrap();
    let stylesheet = parse_stylesheet("").unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

    let divs = divs(&styled);
    let left_top = divs[0];
    let top_left = divs[1];

    assert_eq!(bg_pos(left_top), (0.0, 0.0));
    assert_eq!(bg_pos(top_left), (0.0, 0.0));
}

#[test]
fn background_position_vertical_first_pair_resolves_axes() {
    let dom = dom::parse_html(r#"<div style="background-position: center bottom"></div>"#).unwrap();
    let stylesheet = parse_stylesheet("").unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

    let node = divs(&styled)[0];
    // center (x) bottom (y)
    assert_eq!(bg_pos(node), (50.0, 100.0));
}

#[test]
fn background_position_offsets_follow_keywords() {
    let dom = dom::parse_html(r#"<div style="background-position: right 10% top 20%"></div>"#).unwrap();
    let stylesheet = parse_stylesheet("").unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

    let node = divs(&styled)[0];
    // right 10% -> 90% x, top 20% -> 20% y
    assert_eq!(bg_pos(node), (90.0, 20.0));
}

#[test]
fn background_position_defaults_to_zero_zero() {
    let dom = dom::parse_html(r#"<div style="background-position: initial"></div>"#).unwrap();
    let stylesheet = parse_stylesheet("").unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

    let node = divs(&styled)[0];
    assert_eq!(bg_pos(node), (0.0, 0.0));
}

#[test]
fn background_position_shorthand_resets_positions() {
    let dom = dom::parse_html(r#"<div style="background-position: 20% 30%; background: red;"></div>"#).unwrap();
    let stylesheet = parse_stylesheet("").unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

    let node = &styled.children[0];
    // background shorthand should reset position to initial 0% 0%
    assert_eq!(bg_pos(node), (0.0, 0.0));
}
