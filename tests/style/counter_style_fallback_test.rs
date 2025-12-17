use fastrender::css::parser::parse_stylesheet;
use fastrender::style::cascade::apply_styles_with_media;
use fastrender::style::media::MediaContext;
use fastrender::{dom, DomNode};

fn list_marker_text(node: &DomNode) -> String {
    node.marker.as_ref().map(|m| m.text.clone()).unwrap_or_default()
}

#[test]
fn lower_greek_out_of_range_falls_back_to_decimal() {
    let dom = dom::parse_html(r#"<ol style="list-style-type: lower-greek" start="-1"><li></li></ol>"#).unwrap();
    let stylesheet = parse_stylesheet("counter", r#""#, None).unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));
    let li = &styled.children[0].children[0];
    assert_eq!(list_marker_text(li), "-1.");
}

#[test]
fn lower_armenian_out_of_range_falls_back_to_decimal() {
    let dom = dom::parse_html(r#"<ol style="list-style-type: lower-armenian" start="4000"><li></li></ol>"#).unwrap();
    let stylesheet = parse_stylesheet("counter", r#""#, None).unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));
    let li = &styled.children[0].children[0];
    assert_eq!(list_marker_text(li), "4000.");
}
