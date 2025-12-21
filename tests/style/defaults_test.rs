use fastrender::dom::{DomNode, DomNodeType, HTML_NAMESPACE};
use fastrender::style::defaults::get_default_styles_for_element;
use fastrender::style::values::Length;

fn element(tag: &str) -> DomNode {
    DomNode {
        node_type: DomNodeType::Element {
            tag_name: tag.to_string(),
            namespace: HTML_NAMESPACE.to_string(),
            attributes: Vec::new(),
        },
        children: Vec::new(),
    }
}

#[test]
fn table_cells_get_default_padding() {
    for tag in ["td", "th"] {
        let style = get_default_styles_for_element(&element(tag));
        assert_eq!(style.padding_top, Length::px(1.0));
        assert_eq!(style.padding_bottom, Length::px(1.0));
        assert_eq!(style.padding_left, Length::px(1.0));
        assert_eq!(style.padding_right, Length::px(1.0));
        assert_eq!(style.vertical_align, fastrender::style::types::VerticalAlign::Middle);
    }
}

#[test]
fn table_headers_are_bold_and_centered() {
    let td = get_default_styles_for_element(&element("td"));
    let th = get_default_styles_for_element(&element("th"));

    // td defaults preserved
    assert_eq!(th.padding_top, td.padding_top);
    assert_eq!(th.padding_bottom, td.padding_bottom);
    assert_eq!(th.padding_left, td.padding_left);
    assert_eq!(th.padding_right, td.padding_right);
    assert_eq!(th.vertical_align, td.vertical_align);

    // UA defaults for header cells (CSS2/HTML)
    assert_eq!(th.text_align, fastrender::style::types::TextAlign::Center);
    assert!(matches!(th.font_weight, fastrender::style::types::FontWeight::Bold));
}
