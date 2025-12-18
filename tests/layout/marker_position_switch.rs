use std::sync::Arc;

use fastrender::layout::constraints::LayoutConstraints;
use fastrender::layout::contexts::inline::InlineFormattingContext;
use fastrender::style::display::FormattingContextType;
use fastrender::style::types::ListStylePosition;
use fastrender::style::ComputedStyle;
use fastrender::tree::box_tree::{BoxNode, MarkerContent};
use fastrender::tree::fragment_tree::FragmentContent;

fn marker_and_text_positions(fragment: &fastrender::tree::fragment_tree::FragmentNode) -> (Option<f32>, Option<f32>) {
    let mut marker_x = None;
    let mut text_x = None;
    let mut stack = vec![fragment];
    while let Some(node) = stack.pop() {
        match node.content {
            FragmentContent::Text { is_marker, .. } => {
                if is_marker && marker_x.is_none() {
                    marker_x = Some(node.bounds.x());
                } else if !is_marker && text_x.is_none() {
                    text_x = Some(node.bounds.x());
                }
            }
            FragmentContent::Replaced { .. } => {
                if marker_x.is_none() {
                    marker_x = Some(node.bounds.x());
                }
            }
            _ => {}
        }
        for child in &node.children {
            stack.push(child);
        }
    }
    (marker_x, text_x)
}

#[test]
fn marker_switches_position_inside_vs_outside() {
    let ifc = InlineFormattingContext::new();

    let mut inside_style = ComputedStyle::default();
    inside_style.list_style_position = ListStylePosition::Inside;
    let inside_marker = BoxNode::new_marker(Arc::new(inside_style), MarkerContent::Text("•".to_string()));
    let text = BoxNode::new_text(Arc::new(ComputedStyle::default()), "content".to_string());
    let inside_root = BoxNode::new_block(
        Arc::new(ComputedStyle::default()),
        FormattingContextType::Block,
        vec![inside_marker, text.clone()],
    );
    let constraints = LayoutConstraints::definite_width(200.0);
    let inside_fragment = ifc.layout(&inside_root, &constraints).expect("inside layout");
    let inside_line = inside_fragment.children.first().expect("inside line");
    let (inside_marker_x, inside_text_x) = marker_and_text_positions(inside_line);

    let mut outside_style = ComputedStyle::default();
    outside_style.list_style_position = ListStylePosition::Outside;
    let outside_marker = BoxNode::new_marker(Arc::new(outside_style), MarkerContent::Text("•".to_string()));
    let outside_root = BoxNode::new_block(
        Arc::new(ComputedStyle::default()),
        FormattingContextType::Block,
        vec![outside_marker, text],
    );
    let outside_fragment = ifc.layout(&outside_root, &constraints).expect("outside layout");
    let outside_line = outside_fragment.children.first().expect("outside line");
    let (outside_marker_x, outside_text_x) = marker_and_text_positions(outside_line);

    let inside_marker_x = inside_marker_x.expect("inside marker x");
    let inside_text_x = inside_text_x.expect("inside text x");
    let outside_marker_x = outside_marker_x.expect("outside marker x");
    let outside_text_x = outside_text_x.expect("outside text x");

    assert!(inside_marker_x >= 0.0, "inside marker should be inline with text");
    assert!(inside_text_x > inside_marker_x, "text should follow inside marker");

    assert!(outside_marker_x < -5.0, "outside marker should be placed before text");
    assert!(outside_text_x.abs() < 0.5, "outside text should start near line origin");
}

