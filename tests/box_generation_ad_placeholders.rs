use fastrender::css::parser::parse_stylesheet;
use fastrender::style::cascade::apply_styles_with_media;
use fastrender::style::media::MediaContext;
use fastrender::tree::box_generation::generate_box_tree_with_anonymous_fixup;
use fastrender::tree::box_tree::BoxNode;
use fastrender::{dom, dom::DomNode};

fn contains_class(node: &BoxNode, class: &str) -> bool {
    if let Some(info) = &node.debug_info {
        if info.classes.iter().any(|c| c == class) {
            return true;
        }
    }
    node.children.iter().any(|child| contains_class(child, class))
}

#[test]
fn empty_ad_placeholders_are_dropped() {
    for class in ["ad-height-hold", "ad__slot", "should-hold-space"] {
        let html = format!("<div class=\"{}\"></div>", class);
        let dom: DomNode = dom::parse_html(&html).unwrap();
        let stylesheet = parse_stylesheet("").unwrap();
        let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

        let box_tree = generate_box_tree_with_anonymous_fixup(&styled);
        assert!(
            !contains_class(&box_tree.root, class),
            "empty ad placeholder {class} should not generate boxes"
        );
    }
}

#[test]
fn non_empty_ad_placeholders_are_kept() {
    let dom: DomNode = dom::parse_html(r#"<div class=\"ad-height-hold\"><span>ad content</span></div>"#).unwrap();
    let stylesheet = parse_stylesheet("").unwrap();
    let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));

    let box_tree = generate_box_tree_with_anonymous_fixup(&styled);
    assert!(contains_class(&box_tree.root, "ad-height-hold"));
}
