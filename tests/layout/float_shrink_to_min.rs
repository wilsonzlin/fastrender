use fastrender::layout::constraints::LayoutConstraints;
use fastrender::layout::contexts::block::BlockFormattingContext;
use fastrender::style::float::Float;
use fastrender::style::values::Length;
use fastrender::BoxNode;
use fastrender::ComputedStyle;
use fastrender::FormattingContext;
use fastrender::FormattingContextType;
use std::sync::Arc;

/// Floats with `width: auto` should use the CSS shrink-to-fit formula and then
/// honor `min-width`/`max-width` caps. When the available width is smaller than
/// the authored minimum, the used width must clamp to the min-width instead of
/// collapsing to the available space.
#[test]
fn float_auto_width_honors_min_width() {
  // Container 100px wide with a single floating child that has only a min-width.
  let container_style = Arc::new(ComputedStyle::default());

  let mut float_style = ComputedStyle::default();
  float_style.float = Float::Left;
  float_style.min_width = Some(Length::px(150.0));
  let float_box = BoxNode::new_block(Arc::new(float_style), FormattingContextType::Block, vec![]);

  let container = BoxNode::new_block(container_style, FormattingContextType::Block, vec![
    float_box,
  ]);

  let bfc = BlockFormattingContext::new();
  let constraints = LayoutConstraints::definite(100.0, 1000.0);
  let fragment = bfc
    .layout(&container, &constraints)
    .expect("layout should succeed");

  // The float should clamp up to its min-width even though the available width is smaller.
  assert_eq!(fragment.children.len(), 1);
  let float_fragment = &fragment.children[0];
  assert!((float_fragment.bounds.width() - 150.0).abs() < 0.01);
}

/// Floats should also clamp shrink-to-fit results to max-width. If the preferred
/// widths exceed the authored max-width, the used width must not overflow it.
#[test]
fn float_auto_width_clamps_to_max_width() {
  // Float with text content and a max-width smaller than its intrinsic size.
  let container_style = Arc::new(ComputedStyle::default());

  let mut float_style = ComputedStyle::default();
  float_style.float = Float::Left;
  float_style.max_width = Some(Length::px(50.0));
  let text_child = BoxNode::new_text(
    Arc::new(ComputedStyle::default()),
    "Hello world".to_string(),
  );
  let float_box = BoxNode::new_block(Arc::new(float_style), FormattingContextType::Block, vec![
    text_child,
  ]);

  let container = BoxNode::new_block(container_style, FormattingContextType::Block, vec![
    float_box,
  ]);

  let bfc = BlockFormattingContext::new();
  let constraints = LayoutConstraints::definite(200.0, 1000.0);
  let fragment = bfc
    .layout(&container, &constraints)
    .expect("layout should succeed");

  assert_eq!(fragment.children.len(), 1);
  let float_fragment = &fragment.children[0];
  assert!(float_fragment.bounds.width() <= 50.0 + 0.01);
}
