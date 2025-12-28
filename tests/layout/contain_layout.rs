use std::sync::Arc;

use fastrender::layout::constraints::{AvailableSpace, LayoutConstraints};
use fastrender::layout::contexts::block::BlockFormattingContext;
use fastrender::layout::formatting_context::IntrinsicSizingMode;
use fastrender::FormattingContext;
use fastrender::style::display::{Display, FormattingContextType};
use fastrender::style::types::Containment;
use fastrender::style::values::Length;
use fastrender::tree::box_tree::BoxNode;
use fastrender::ComputedStyle;

fn container_with_child(containment: Containment) -> BoxNode {
  let mut child_style = ComputedStyle::default();
  child_style.display = Display::Block;
  child_style.width = Some(Length::px(80.0));
  child_style.height = Some(Length::px(10.0));
  let child = BoxNode::new_block(Arc::new(child_style), FormattingContextType::Block, vec![]);

  let mut container_style = ComputedStyle::default();
  container_style.display = Display::Block;
  container_style.padding_left = Length::px(5.0);
  container_style.padding_right = Length::px(5.0);
  container_style.border_left_width = Length::px(1.0);
  container_style.border_right_width = Length::px(1.0);
  container_style.containment = containment;
  let container_style = Arc::new(container_style);

  BoxNode::new_block(container_style, FormattingContextType::Block, vec![child])
}

#[test]
fn layout_containment_isolates_intrinsic_width() {
  let fc = BlockFormattingContext::new();

  let baseline = container_with_child(Containment::none());
  let contained = container_with_child(Containment::with_flags(false, false, true, false, false));

  let baseline_width = fc
    .compute_intrinsic_inline_size(&baseline, IntrinsicSizingMode::MaxContent)
    .unwrap();
  let contained_width = fc
    .compute_intrinsic_inline_size(&contained, IntrinsicSizingMode::MaxContent)
    .unwrap();

  // Without containment, the child contributes 80px plus 12px of padding/borders.
  assert!((baseline_width - 92.0).abs() < 0.01);
  // Layout containment isolates intrinsic sizing, keeping only padding/border edges.
  assert!((contained_width - 12.0).abs() < 0.01);
}

#[test]
fn size_containment_clamps_auto_block_size() {
  let mut contained_style = ComputedStyle::default();
  contained_style.display = Display::Block;
  contained_style.containment = Containment::with_flags(true, false, false, false, false);
  contained_style.padding_top = Length::px(3.0);
  contained_style.padding_bottom = Length::px(3.0);
  let contained_style = Arc::new(contained_style);

  let mut child_style = ComputedStyle::default();
  child_style.display = Display::Block;
  child_style.height = Some(Length::px(40.0));
  let child_style = Arc::new(child_style);

  let child = BoxNode::new_block(
    child_style,
    FormattingContextType::Block,
    Vec::<BoxNode>::new(),
  );
  let container = BoxNode::new_block(contained_style, FormattingContextType::Block, vec![child]);

  let constraints =
    LayoutConstraints::new(AvailableSpace::Definite(120.0), AvailableSpace::Indefinite);
  let fragment = BlockFormattingContext::new()
    .layout(&container, &constraints)
    .expect("layout");

  // Size containment in the block axis ignores descendants for used size, keeping only padding.
  assert!((fragment.bounds.height() - 6.0).abs() < 0.01);
}
