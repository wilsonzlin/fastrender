use fastrender::css::parser::parse_stylesheet;
use fastrender::style::cascade::apply_styles_with_media;
use fastrender::style::media::MediaContext;
use fastrender::{dom, DomNode};

#[test]
fn parses_integer_order() {
    let dom = dom::parse_html(
        r#"<div><span style="order: -2">a</span><span style="order: 5">b</span></div>"#,
    )
    .unwrap();
    let stylesheet = parse_stylesheet("order-test", r#""#, None).unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));
    let orders: Vec<i32> = styled.children[0]
        .children
        .iter()
        .map(|n| n.styles.order)
        .collect();
    assert_eq!(orders, vec![-2, 5]);
}

#[test]
fn ignores_non_integer_order() {
    let dom = dom::parse_html(r#"<div><span style="order: 1.5">a</span></div>"#).unwrap();
    let stylesheet = parse_stylesheet("order-test", r#""#, None).unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));
    let order = styled.children[0].children[0].styles.order;
    assert_eq!(order, 0);
}
