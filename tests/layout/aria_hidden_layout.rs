use fastrender::style::computed::Visibility;
use fastrender::tree::fragment_tree::FragmentContent;
use fastrender::FastRender;

fn collect_text_fragments(fragment: &fastrender::FragmentNode, texts: &mut Vec<String>) {
    if let FragmentContent::Text { text, .. } = &fragment.content {
        texts.push(text.clone());
    }
    for child in &fragment.children {
        collect_text_fragments(child, texts);
    }
}

#[test]
fn aria_hidden_elements_are_still_laid_out() {
    let mut renderer = FastRender::new().expect("renderer");
    let dom = renderer
        .parse_html("<div aria-hidden=\"true\">Still here<span>child text</span></div>")
        .expect("parse");
    let tree = renderer.layout_document(&dom, 200, 100).expect("layout should succeed");

    // Should produce fragments beyond the root.
    assert!(
        tree.fragment_count() > 1,
        "aria-hidden content should still generate fragments"
    );

    // Text should be present in the fragment tree.
    let mut texts = Vec::new();
    collect_text_fragments(&tree.root, &mut texts);
    assert!(
        texts.iter().any(|t| t.contains("child text")),
        "text inside aria-hidden element should remain in layout"
    );

    // None of the fragments should be hidden via visibility.
    let any_hidden = tree.iter_fragments().any(|f| {
        f.style
            .as_ref()
            .map(|s| s.visibility != Visibility::Visible)
            .unwrap_or(false)
    });
    assert!(
        !any_hidden,
        "aria-hidden should not force fragments to visibility hidden"
    );
}
