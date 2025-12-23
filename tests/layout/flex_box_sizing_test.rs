use fastrender::layout::constraints::AvailableSpace;
use fastrender::layout::constraints::LayoutConstraints;
use fastrender::layout::contexts::flex::FlexFormattingContext;
use fastrender::layout::formatting_context::FormattingContext;
use fastrender::layout::formatting_context::IntrinsicSizingMode;
use fastrender::style::display::Display;
use fastrender::style::display::FormattingContextType;
use fastrender::style::types::BoxSizing;
use fastrender::style::types::FlexDirection;
use fastrender::style::types::Overflow;
use fastrender::style::values::Length;
use fastrender::style::ComputedStyle;
use fastrender::tree::box_tree::BoxNode;
use std::sync::Arc;

fn style_with_display(display: Display) -> ComputedStyle {
  let mut style = ComputedStyle::default();
  style.display = display;
  style
}

#[test]
fn flex_item_border_box_width_uses_content_size() {
  let mut container_style = style_with_display(Display::Flex);
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
    FormattingContextType::Flex,
    vec![BoxNode::new_block(
      Arc::new(child_style.clone()),
      FormattingContextType::Block,
      vec![],
    )],
  );

  let fc = FlexFormattingContext::new();
  let constraints =
    LayoutConstraints::new(AvailableSpace::Definite(400.0), AvailableSpace::Indefinite);
  let fragment = fc
    .layout(&container, &constraints)
    .expect("layout should succeed");

  let child = fragment.children.first().expect("child fragment");
  let width = child.bounds.width();
  assert!(
    (width - 200.0).abs() < 0.1,
    "border-box width should be honored (got {width})"
  );
  assert!((child.bounds.height() - 0.0).abs() < 0.1);
  assert!((child.bounds.x() - 0.0).abs() < 0.1);
  assert!((child.bounds.y() - 0.0).abs() < 0.1);

  let content_box_width = child.bounds.width() - edge_sum;

  assert!(
    (content_box_width - 170.0).abs() < 0.1,
    "content box should shrink by padding+borders under border-box"
  );
}

#[test]
fn flex_container_auto_height_tracks_children() {
  let mut container_style = style_with_display(Display::Flex);
  container_style.width = Some(Length::px(200.0));
  // Auto height (unset) should shrink-wrap the flex line height.

  let mut child_style = ComputedStyle::default();
  child_style.display = Display::Block;
  child_style.width = Some(Length::px(50.0));
  child_style.height = Some(Length::px(10.0));

  let container = BoxNode::new_block(
    Arc::new(container_style),
    FormattingContextType::Flex,
    vec![BoxNode::new_block(
      Arc::new(child_style),
      FormattingContextType::Block,
      vec![],
    )],
  );

  let fc = FlexFormattingContext::new();
  let fragment = fc
    .layout(
      &container,
      &LayoutConstraints::new(AvailableSpace::Definite(200.0), AvailableSpace::Indefinite),
    )
    .expect("layout should succeed");

  assert!(
    (fragment.bounds.height() - 10.0).abs() < 0.1,
    "auto-height flex container should match child height when no other items"
  );
  let child = fragment.children.first().expect("child fragment");
  assert!(
    (child.bounds.height() - 10.0).abs() < 0.1,
    "child height should be preserved without viewport clamping"
  );
}

#[test]
fn flex_item_auto_min_size_clamps_to_min_content() {
  // Row axis: min-width:auto should map to min-content inline size.
  let mut container_style = style_with_display(Display::Flex);
  container_style.width = Some(Length::px(50.0));

  let mut child_style = ComputedStyle::default();
  child_style.display = Display::Block;
  // Ensure overflow is not restricting min-content computation.
  child_style.overflow_x = Overflow::Hidden;
  child_style.overflow_y = Overflow::Hidden;

  let text = BoxNode::new_text(
    Arc::new(ComputedStyle::default()),
    "ThisIsALongUnbreakableWord".to_string(),
  );
  let child_box = BoxNode::new_block(
    Arc::new(child_style),
    FormattingContextType::Block,
    vec![text],
  );
  let child_for_intrinsic = child_box.clone();

  let container = BoxNode::new_block(
    Arc::new(container_style),
    FormattingContextType::Flex,
    vec![child_box],
  );

  let fc = FlexFormattingContext::new();
  let min_content = fc
    .compute_intrinsic_inline_size(&child_for_intrinsic, IntrinsicSizingMode::MinContent)
    .expect("min-content size");
  let fragment = fc
    .layout(
      &container,
      &LayoutConstraints::new(AvailableSpace::Definite(50.0), AvailableSpace::Indefinite),
    )
    .expect("layout should succeed");

  let child = fragment.children.first().expect("child fragment");
  assert!(
    (child.bounds.width() + 0.1) >= min_content,
    "auto min-size should use min-content width; expected >= {min_content}, got {}",
    child.bounds.width()
  );
}

#[test]
fn flex_item_auto_min_size_column_uses_block_min_content() {
  // Column axis: min-height:auto should map to min-content block size.
  let mut container_style = style_with_display(Display::Flex);
  container_style.width = Some(Length::px(200.0));
  container_style.height = Some(Length::px(10.0)); // very tight height to force shrink
  container_style.flex_direction = FlexDirection::Column;

  let mut child_style = ComputedStyle::default();
  child_style.display = Display::Block;
  child_style.overflow_x = Overflow::Hidden;
  child_style.overflow_y = Overflow::Hidden;

  let text = BoxNode::new_text(
    Arc::new(ComputedStyle::default()),
    "TallContentLine".to_string(),
  );
  let child_box = BoxNode::new_block(
    Arc::new(child_style),
    FormattingContextType::Block,
    vec![text],
  );
  let child_for_intrinsic = child_box.clone();

  let container = BoxNode::new_block(
    Arc::new(container_style),
    FormattingContextType::Flex,
    vec![child_box],
  );

  let fc = FlexFormattingContext::new();
  let min_content_block = fc
    .compute_intrinsic_block_size(&child_for_intrinsic, IntrinsicSizingMode::MinContent)
    .expect("min-content block size");
  let fragment = fc
    .layout(
      &container,
      &LayoutConstraints::new(
        AvailableSpace::Definite(200.0),
        AvailableSpace::Definite(10.0),
      ),
    )
    .expect("layout should succeed");

  let child = fragment.children.first().expect("child fragment");
  assert!(
        (child.bounds.height() + 0.1) >= min_content_block,
        "auto min-size should use min-content block-size in column axis; expected >= {min_content_block}, got {}",
        child.bounds.height()
    );
}
