use std::sync::Arc;

use fastrender::geometry::Size;
use fastrender::layout::constraints::{AvailableSpace, LayoutConstraints};
use fastrender::layout::contexts::inline::InlineFormattingContext;
use fastrender::style::display::FormattingContextType;
use fastrender::style::types::WritingMode;
use fastrender::style::ComputedStyle;
use fastrender::tree::box_tree::{BoxNode, MarkerContent, ReplacedBox, ReplacedType};
use fastrender::tree::fragment_tree::FragmentContent;

fn marker_and_text_baselines(fragment: &fastrender::tree::fragment_tree::FragmentNode) -> (Option<f32>, Option<f32>) {
    let mut marker_base = None;
    let mut text_base = None;
    let mut stack = vec![fragment];
    while let Some(node) = stack.pop() {
        match node.content {
            FragmentContent::Text {
                is_marker,
                baseline_offset,
                ..
            } => {
                let base = node.bounds.y() + baseline_offset;
                if is_marker && marker_base.is_none() {
                    marker_base = Some(base);
                } else if !is_marker && text_base.is_none() {
                    text_base = Some(base);
                }
            }
            FragmentContent::Replaced { .. } => {
                marker_base.get_or_insert(node.bounds.y() + node.bounds.height());
            }
            _ => {}
        }
        for child in &node.children {
            stack.push(child);
        }
    }
    (marker_base, text_base)
}

#[test]
fn vertical_marker_image_aligns_baseline_with_text() {
    let ifc = InlineFormattingContext::new();

    let mut marker_style = ComputedStyle::default();
    marker_style.writing_mode = WritingMode::VerticalRl;
    marker_style.font_size = 16.0;
    let marker = BoxNode::new_marker(
        Arc::new(marker_style),
        MarkerContent::Image(ReplacedBox {
            replaced_type: ReplacedType::Image {
                src: String::new(),
                alt: None,
                sizes: None,
                srcset: Vec::new(),
            },
            intrinsic_size: Some(Size::new(10.0, 10.0)),
            aspect_ratio: Some(1.0),
        }),
    );

    let mut text_style = ComputedStyle::default();
    text_style.writing_mode = WritingMode::VerticalRl;
    let text = BoxNode::new_text(Arc::new(text_style), "vertical text".to_string());

    let mut root_style = ComputedStyle::default();
    root_style.writing_mode = WritingMode::VerticalRl;
    let root = BoxNode::new_block(
        Arc::new(root_style),
        FormattingContextType::Block,
        vec![marker, text],
    );

    let constraints = LayoutConstraints::new(AvailableSpace::Definite(200.0), AvailableSpace::Definite(200.0));
    let fragment = ifc.layout(&root, &constraints).expect("layout");
    let line = fragment.children.first().expect("line fragment");
    let (marker_base, text_base) = marker_and_text_baselines(line);
    let marker_base = marker_base.expect("marker baseline");
    let text_base = text_base.expect("text baseline");

    assert!(
        (marker_base - text_base).abs() < 0.5,
        "marker image baseline should align with text in vertical writing; marker={} text={}",
        marker_base,
        text_base
    );
}

