use fastrender::css::parser::parse_stylesheet;
use fastrender::dom;
use fastrender::style::cascade::{apply_styles_with_media, StyledNode};
use fastrender::style::media::MediaContext;
use fastrender::style::types::{BackgroundPosition, BackgroundPositionComponent};
use fastrender::style::values::LengthUnit;

fn bg_pos(node: &StyledNode) -> (BackgroundPositionComponent, BackgroundPositionComponent) {
    match node.styles.background_positions.first().copied().expect("background position") {
        BackgroundPosition::Position { x, y } => (x, y),
    }
}

fn collect_divs<'a>(node: &'a StyledNode, out: &mut Vec<&'a StyledNode>) {
    if let Some(name) = node.node.tag_name() {
        if name.eq_ignore_ascii_case("div") {
            out.push(node);
        }
    }
    for child in &node.children {
        collect_divs(child, out);
    }
}

fn all_divs(styled: &StyledNode) -> Vec<&StyledNode> {
    let mut out = Vec::new();
    collect_divs(styled, &mut out);
    out
}

fn assert_component(comp: &BackgroundPositionComponent, align: f32, offset: f32, unit: LengthUnit) {
    assert!((comp.alignment - align).abs() < 1e-6, "expected alignment {} got {}", align, comp.alignment);
    assert!((comp.offset.value - offset).abs() < 1e-6, "expected offset {} got {}", offset, comp.offset.value);
    assert_eq!(comp.offset.unit, unit);
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
    let divs = all_divs(&styled);
    let left_top = divs[0];
    let top_left = divs[1];

    let (x1, y1) = bg_pos(left_top);
    let (x2, y2) = bg_pos(top_left);
    assert_component(&x1, 0.0, 0.0, LengthUnit::Percent);
    assert_component(&y1, 0.0, 0.0, LengthUnit::Percent);
    assert_component(&x2, 0.0, 0.0, LengthUnit::Percent);
    assert_component(&y2, 0.0, 0.0, LengthUnit::Percent);
}

#[test]
fn background_position_vertical_first_pair_resolves_axes() {
    let dom = dom::parse_html(r#"<div style="background-position: center bottom"></div>"#).unwrap();
    let stylesheet = parse_stylesheet("").unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

    let node = all_divs(&styled)[0];
    // center (x) bottom (y)
    let (x, y) = bg_pos(node);
    assert_component(&x, 0.5, 0.0, LengthUnit::Percent);
    assert_component(&y, 1.0, 0.0, LengthUnit::Percent);
}

#[test]
fn background_position_offsets_follow_keywords() {
    let dom = dom::parse_html(
        r#"<div style="background-position: right 10% top 20%"></div>"#,
    )
    .unwrap();
    let stylesheet = parse_stylesheet("").unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

    let node = all_divs(&styled)[0];
    // right 10% -> 90% x, top 20% -> 20% y
    let (x, y) = bg_pos(node);
    // Right offsets flip the sign; alignment remains at the end.
    assert_component(&x, 1.0, -10.0, LengthUnit::Percent);
    assert_component(&y, 0.0, 20.0, LengthUnit::Percent);
}

#[test]
fn background_position_defaults_to_zero_zero() {
    let dom = dom::parse_html(r#"<div style="background-position: initial"></div>"#).unwrap();
    let stylesheet = parse_stylesheet("").unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

    let node = all_divs(&styled)[0];
    let (x, y) = bg_pos(node);
    assert_component(&x, 0.0, 0.0, LengthUnit::Percent);
    assert_component(&y, 0.0, 0.0, LengthUnit::Percent);
}

#[test]
fn background_position_shorthand_resets_positions() {
    let dom = dom::parse_html(
        r#"<div style="background-position: 20% 30%; background: red;"></div>"#,
    )
    .unwrap();
    let stylesheet = parse_stylesheet("").unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

    let node = all_divs(&styled)[0];
    // background shorthand should reset position to initial 0% 0%
    let (x, y) = bg_pos(node);
    assert_component(&x, 0.0, 0.0, LengthUnit::Percent);
    assert_component(&y, 0.0, 0.0, LengthUnit::Percent);
}

#[test]
fn background_position_logical_maps_inline_and_block_horizontal_tb() {
    // In horizontal-tb, inline maps to the physical x-axis and block maps to the physical y-axis.
    let dom = dom::parse_html(
        r#"<div style="background-position-inline: 10%; background-position-block: 20%"></div>"#,
    )
    .unwrap();
    let stylesheet = parse_stylesheet("").unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

    let node = all_divs(&styled)[0];
    let (x, y) = bg_pos(node);
    // Percent values are stored as offsets on the corresponding axis.
    assert_component(&x, 0.0, 10.0, LengthUnit::Percent);
    assert_component(&y, 0.0, 20.0, LengthUnit::Percent);
}

#[test]
fn background_position_logical_maps_inline_and_block_vertical_rl() {
    // In vertical-rl, inline maps to the physical y-axis (top→bottom) and block maps to the physical x-axis (right→left).
    let dom = dom::parse_html(
        r#"<div style="writing-mode: vertical-rl; background-position-inline: 25%; background-position-block: 0%"></div>"#,
    )
    .unwrap();
    let stylesheet = parse_stylesheet("").unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

    let node = all_divs(&styled)[0];
    let (x, y) = bg_pos(node);
    // Block axis applies to the physical x-axis; inline axis applies to y.
    assert_component(&x, 0.0, 0.0, LengthUnit::Percent);
    assert_component(&y, 0.0, 25.0, LengthUnit::Percent);
}
