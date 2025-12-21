use fastrender::layout::constraints::AvailableSpace;
use fastrender::layout::constraints::LayoutConstraints;
use fastrender::layout::contexts::grid::GridFormattingContext;
use fastrender::layout::formatting_context::FormattingContext;
use fastrender::style::display::Display;
use fastrender::style::display::FormattingContextType;
use fastrender::style::types::BoxSizing;
use fastrender::style::values::Length;
use fastrender::style::ComputedStyle;
use fastrender::tree::box_tree::BoxNode;
use std::sync::Arc;

#[test]
fn grid_item_border_box_respects_outer_width() {
  let mut container_style = ComputedStyle::default();
  container_style.display = Display::Grid;
  container_style.width = Some(Length::px(400.0));

  let mut child_style = ComputedStyle::default();
  child_style.display = Display::Block;
  child_style.box_sizing = BoxSizing::BorderBox;
  child_style.width = Some(Length::px(200.0));
  child_style.padding_left = Length::px(10.0);
  child_style.padding_right = Length::px(10.0);
  child_style.border_left_width = Length::px(5.0);
  child_style.border_right_width = Length::px(5.0);

  let edge_sum = child_style.padding_left.to_px()
    + child_style.padding_right.to_px()
    + child_style.border_left_width.to_px()
    + child_style.border_right_width.to_px();

  let container = BoxNode::new_block(
    Arc::new(container_style),
    FormattingContextType::Grid,
    vec![BoxNode::new_block(
      Arc::new(child_style.clone()),
      FormattingContextType::Block,
      vec![],
    )],
  );

  let grid_fc = GridFormattingContext::new();
  let constraints =
    LayoutConstraints::new(AvailableSpace::Definite(400.0), AvailableSpace::Indefinite);
  let fragment = grid_fc
    .layout(&container, &constraints)
    .expect("layout should succeed");

  let child = fragment.children.first().expect("child fragment");
  let width = child.bounds.width();
  assert!(
    (width - 200.0).abs() < 0.1,
    "border-box width should match authored outer width (got {width})"
  );

  let content_width = width - edge_sum;
  assert!(
    (content_width - 170.0).abs() < 0.1,
    "content box should subtract padding+borders under border-box"
  );
}
