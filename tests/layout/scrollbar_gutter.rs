use fastrender::layout::constraints::LayoutConstraints;
use fastrender::layout::contexts::block::BlockFormattingContext;
use fastrender::layout::utils::resolve_scrollbar_width;
use fastrender::style::types::Overflow;
use fastrender::style::types::ScrollbarGutter;
use fastrender::style::values::Length;
use fastrender::BoxNode;
use fastrender::ComputedStyle;
use fastrender::FormattingContext;
use fastrender::FormattingContextType;
use std::sync::Arc;

fn layout_with_container(style: ComputedStyle) -> (f32, f32) {
  let gutter = resolve_scrollbar_width(&style);
  let container_style = Arc::new(style);

  let mut child_style = ComputedStyle::default();
  child_style.width = Some(Length::percent(100.0));
  let child = BoxNode::new_block(Arc::new(child_style), FormattingContextType::Block, vec![]);

  let container = BoxNode::new_block(container_style, FormattingContextType::Block, vec![child]);

  let bfc = BlockFormattingContext::new();
  let constraints = LayoutConstraints::definite(100.0, 1000.0);
  let fragment = bfc
    .layout(&container, &constraints)
    .expect("layout should succeed");

  assert_eq!(fragment.children.len(), 1);
  (fragment.children[0].bounds.width(), gutter)
}

#[test]
fn scrollbar_gutter_stable_reserves_inline_end_space() {
  let mut style = ComputedStyle::default();
  style.overflow_y = Overflow::Auto;
  style.scrollbar_gutter = ScrollbarGutter {
    stable: true,
    both_edges: false,
  };

  let (child_width, gutter) = layout_with_container(style);
  assert!((child_width - (100.0 - gutter)).abs() < 1e-3);
}

#[test]
fn scrollbar_gutter_stable_both_edges_reserves_space_on_both_sides() {
  let mut style = ComputedStyle::default();
  style.overflow_y = Overflow::Auto;
  style.scrollbar_gutter = ScrollbarGutter {
    stable: true,
    both_edges: true,
  };

  let (child_width, gutter) = layout_with_container(style);
  assert!((child_width - (100.0 - gutter * 2.0)).abs() < 1e-3);
}

#[test]
fn scrollbar_gutter_auto_does_not_reserve_space_without_scroll() {
  let mut style = ComputedStyle::default();
  style.overflow_y = Overflow::Auto;

  let (child_width, gutter) = layout_with_container(style);
  assert_eq!(gutter, resolve_scrollbar_width(&ComputedStyle::default()));
  assert!((child_width - 100.0).abs() < 1e-3);
}
