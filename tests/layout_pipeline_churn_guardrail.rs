//! Regression tests for layout hot-path churn.
//!
//! These tests are intended to catch accidental reintroduction of per-measure / per-intrinsic
//! construction of heavyweight layout/text components such as `ShapingPipeline` and
//! `FormattingContextFactory`.

use std::sync::Arc;
use std::sync::Mutex;

use fastrender::layout::contexts::block::BlockFormattingContext;
use fastrender::layout::contexts::inline::InlineFormattingContext;
use fastrender::layout::taffy_integration::{taffy_perf_counters, TaffyPerfCountersGuard};
use fastrender::style::display::Display;
use fastrender::style::position::Position;
use fastrender::style::values::Length;
use fastrender::text::ShapingPipeline;
use fastrender::{
  BoxNode, BoxTree, ComputedStyle, FontContext, FormattingContext, FormattingContextFactory,
  FormattingContextType, IntrinsicSizingMode, LayoutConfig, LayoutEngine, Size,
};

/// Global guard to serialize churn-counter assertions.
///
/// These tests reset global debug/test counters (shaping pipeline creation, factory cloning, etc.).
/// The Rust test harness runs tests in parallel within the same integration test binary by
/// default, so we must prevent concurrent resets from introducing flakiness.
static CHURN_COUNTER_LOCK: Mutex<()> = Mutex::new(());

#[test]
fn layout_does_not_rebuild_shaping_pipeline_or_factory_in_hot_paths() {
  let _guard = CHURN_COUNTER_LOCK
    .lock()
    .unwrap_or_else(|err| err.into_inner());
  // Build a flex tree that forces Taffy measurement for many children. The key invariant is that
  // we should *not* be creating new shaping pipelines or formatting context factories as part of
  // repeated intrinsic sizing / measure callbacks.
  const ITEMS: usize = 256;

  let viewport = Size::new(800.0, 600.0);
  let config = LayoutConfig::for_viewport(viewport);
  let engine = LayoutEngine::with_font_context(config, FontContext::new());

  // Reset churn counters after engine creation so we measure just the layout run.
  ShapingPipeline::debug_reset_new_call_count();
  FormattingContextFactory::debug_reset_with_font_context_viewport_and_cb_call_count();
  FormattingContextFactory::debug_reset_detached_call_count();
  InlineFormattingContext::debug_reset_with_font_context_viewport_and_cb_call_count();
  InlineFormattingContext::debug_reset_with_factory_call_count();

  // Enable Taffy perf counters so we can sanity-check that we exercised the measure callback.
  let _taffy_perf_guard = TaffyPerfCountersGuard::new();

  let mut flex_style = ComputedStyle::default();
  flex_style.display = Display::Flex;
  let flex_style = Arc::new(flex_style);
  let item_style = Arc::new(ComputedStyle::default());
  let text_style = Arc::new(ComputedStyle::default());

  let mut children = Vec::with_capacity(ITEMS);
  for idx in 0..ITEMS {
    let mut text = BoxNode::new_text(Arc::clone(&text_style), "hello world".to_string());
    text.id = 10_000 + idx;

    let mut item = BoxNode::new_block(
      Arc::clone(&item_style),
      FormattingContextType::Inline,
      vec![text],
    );
    item.id = idx + 1;
    children.push(item);
  }

  let mut root = BoxNode::new_block(flex_style, FormattingContextType::Flex, children);
  root.id = 1_000_000;
  let tree = BoxTree::new(root);

  let _ = engine.layout_tree(&tree).expect("layout should succeed");

  let perf = taffy_perf_counters();
  assert!(
    perf.flex_measure_calls > ITEMS as u64,
    "expected flex measurement to run; got {:?}",
    perf
  );

  let shaping_pipeline_news = ShapingPipeline::debug_new_call_count();
  let factory_news = FormattingContextFactory::debug_with_font_context_viewport_and_cb_call_count();
  let detached_news = FormattingContextFactory::debug_detached_call_count();
  let inline_fc_news = InlineFormattingContext::debug_with_font_context_viewport_and_cb_call_count();
  let inline_fc_with_factory_news = InlineFormattingContext::debug_with_factory_call_count();

  assert!(
    shaping_pipeline_news < 20,
    "layout should not rebuild shaping pipelines in hot paths (got {shaping_pipeline_news})"
  );
  assert!(
    factory_news < 20,
    "layout should not rebuild formatting context factories in hot paths (got {factory_news})"
  );
  assert!(
    detached_news < 50,
    "layout should not churn detached factories in hot paths (got {detached_news})"
  );
  assert!(
    inline_fc_news < 20,
    "layout should not rebuild inline formatting contexts via `with_font_context_viewport_and_cb` in hot paths (got {inline_fc_news})"
  );
  assert!(
    inline_fc_with_factory_news < 20,
    "layout should not rebuild inline formatting contexts via `with_factory` in hot paths (got {inline_fc_with_factory_news})"
  );
}

#[test]
fn block_intrinsic_sizing_does_not_rebuild_shaping_pipeline_or_factory() {
  let _guard = CHURN_COUNTER_LOCK
    .lock()
    .unwrap_or_else(|err| err.into_inner());
  let viewport = Size::new(800.0, 600.0);
  let factory = FormattingContextFactory::with_font_context_and_viewport(FontContext::new(), viewport);
  let bfc = BlockFormattingContext::with_factory(factory);

  // Reset churn counters after initialization so we measure just the intrinsic sizing calls.
  ShapingPipeline::debug_reset_new_call_count();
  FormattingContextFactory::debug_reset_with_font_context_viewport_and_cb_call_count();
  FormattingContextFactory::debug_reset_detached_call_count();
  InlineFormattingContext::debug_reset_with_font_context_viewport_and_cb_call_count();
  InlineFormattingContext::debug_reset_with_factory_call_count();

  let mut text_style = ComputedStyle::default();
  text_style.display = Display::Inline;
  let text_style = Arc::new(text_style);
  let mut child_style = ComputedStyle::default();
  child_style.display = Display::Block;
  let mut child = BoxNode::new_block(
    Arc::new(child_style),
    FormattingContextType::Block,
    vec![BoxNode::new_text(text_style, "hello world".to_string())],
  );
  child.id = 0;
  let mut root_style = ComputedStyle::default();
  root_style.display = Display::Block;
  let mut root = BoxNode::new_block(Arc::new(root_style), FormattingContextType::Block, vec![child]);
  // Disable the intrinsic sizing cache so every call recomputes (including for the nested block child).
  root.id = 0;

  for _ in 0..16 {
    let _ = bfc
      .compute_intrinsic_inline_size(&root, IntrinsicSizingMode::MaxContent)
      .expect("intrinsic sizing should succeed");
  }

  assert_eq!(
    ShapingPipeline::debug_new_call_count(),
    0,
    "block intrinsic sizing should reuse the shaping pipeline"
  );
  assert_eq!(
    FormattingContextFactory::debug_with_font_context_viewport_and_cb_call_count(),
    0,
    "block intrinsic sizing should not construct new formatting context factories"
  );
  assert_eq!(
    FormattingContextFactory::debug_detached_call_count(),
    0,
    "block intrinsic sizing should not churn detached factories via recursive BlockFC construction"
  );
  assert_eq!(
    InlineFormattingContext::debug_with_font_context_viewport_and_cb_call_count(),
    0,
    "block intrinsic sizing should not construct inline formatting contexts via `with_font_context_viewport_and_cb`"
  );
  assert_eq!(
    InlineFormattingContext::debug_with_factory_call_count(),
    0,
    "block intrinsic sizing should reuse a shared inline formatting context"
  );
}

#[test]
fn flex_positioned_children_do_not_churn_factories_or_inline_contexts() {
  let _guard = CHURN_COUNTER_LOCK
    .lock()
    .unwrap_or_else(|err| err.into_inner());
  const ITEMS: usize = 256;

  let viewport = Size::new(800.0, 600.0);
  let config = LayoutConfig::for_viewport(viewport);
  let engine = LayoutEngine::with_font_context(config, FontContext::new());

  // Reset churn counters after engine creation so we measure just the layout run.
  ShapingPipeline::debug_reset_new_call_count();
  FormattingContextFactory::debug_reset_with_font_context_viewport_and_cb_call_count();
  FormattingContextFactory::debug_reset_detached_call_count();
  InlineFormattingContext::debug_reset_with_font_context_viewport_and_cb_call_count();
  InlineFormattingContext::debug_reset_with_factory_call_count();

  let mut flex_style = ComputedStyle::default();
  flex_style.display = Display::Flex;
  // Establish a positioned containing block so abs children share one CB and exercise the
  // positioned-children layout path.
  flex_style.position = Position::Relative;
  let flex_style = Arc::new(flex_style);

  let mut abs_style = ComputedStyle::default();
  abs_style.display = Display::Block;
  abs_style.position = Position::Absolute;
  abs_style.left = Some(Length::px(0.0));
  abs_style.top = Some(Length::px(0.0));
  let abs_style = Arc::new(abs_style);

  let mut text_style = ComputedStyle::default();
  text_style.display = Display::Inline;
  let text_style = Arc::new(text_style);

  let mut children = Vec::with_capacity(ITEMS);
  for idx in 0..ITEMS {
    let mut text = BoxNode::new_text(Arc::clone(&text_style), format!("abs {idx}"));
    text.id = 10_000 + idx;

    let mut abs_child = BoxNode::new_block(
      Arc::clone(&abs_style),
      FormattingContextType::Block,
      vec![text],
    );
    abs_child.id = idx + 1;
    children.push(abs_child);
  }

  let mut root = BoxNode::new_block(flex_style, FormattingContextType::Flex, children);
  root.id = 1_000_000;
  let tree = BoxTree::new(root);

  let _ = engine.layout_tree(&tree).expect("layout should succeed");

  let shaping_pipeline_news = ShapingPipeline::debug_new_call_count();
  let factory_news = FormattingContextFactory::debug_with_font_context_viewport_and_cb_call_count();
  let detached_news = FormattingContextFactory::debug_detached_call_count();
  let inline_fc_news = InlineFormattingContext::debug_with_font_context_viewport_and_cb_call_count();
  let inline_fc_with_factory_news = InlineFormattingContext::debug_with_factory_call_count();

  assert!(
    shaping_pipeline_news < 20,
    "positioned children layout should not rebuild shaping pipelines (got {shaping_pipeline_news})"
  );
  assert!(
    factory_news < 20,
    "positioned children layout should not rebuild formatting context factories (got {factory_news})"
  );
  assert!(
    detached_news < 50,
    "positioned children layout should not churn detached factories (got {detached_news})"
  );
  assert!(
    inline_fc_news < 20,
    "positioned children layout should not rebuild inline formatting contexts via `with_font_context_viewport_and_cb` (got {inline_fc_news})"
  );
  // Note: positioned boxes establish a new containing block for their descendants, so block layout
  // must rebuild `InlineFormattingContext` instances when the nearest positioned containing block
  // changes (see `BlockFormattingContext`'s `intrinsic_inline_fc` invariant). That means the raw
  // `InlineFormattingContext::with_factory` call count is expected to scale with the number of
  // positioned boxes we lay out here; this test intentionally focuses on detached factory churn
  // instead.
  let _ = inline_fc_with_factory_news;
}

#[test]
fn grid_positioned_children_do_not_churn_factories_or_inline_contexts() {
  let _guard = CHURN_COUNTER_LOCK
    .lock()
    .unwrap_or_else(|err| err.into_inner());
  const ITEMS: usize = 256;

  let viewport = Size::new(800.0, 600.0);
  let config = LayoutConfig::for_viewport(viewport);
  let engine = LayoutEngine::with_font_context(config, FontContext::new());

  // Reset churn counters after engine creation so we measure just the layout run.
  ShapingPipeline::debug_reset_new_call_count();
  FormattingContextFactory::debug_reset_with_font_context_viewport_and_cb_call_count();
  FormattingContextFactory::debug_reset_detached_call_count();
  InlineFormattingContext::debug_reset_with_font_context_viewport_and_cb_call_count();
  InlineFormattingContext::debug_reset_with_factory_call_count();

  let mut grid_style = ComputedStyle::default();
  grid_style.display = Display::Grid;
  // Establish a positioned containing block so abs children share one CB and exercise the
  // positioned-children layout path.
  grid_style.position = Position::Relative;
  grid_style.width = Some(Length::px(viewport.width));
  grid_style.height = Some(Length::px(viewport.height));
  let grid_style = Arc::new(grid_style);

  let mut abs_style = ComputedStyle::default();
  abs_style.display = Display::Block;
  abs_style.position = Position::Absolute;
  abs_style.left = Some(Length::px(0.0));
  abs_style.top = Some(Length::px(0.0));
  let abs_style = Arc::new(abs_style);

  let mut text_style = ComputedStyle::default();
  text_style.display = Display::Inline;
  let text_style = Arc::new(text_style);

  let mut children = Vec::with_capacity(ITEMS);
  for idx in 0..ITEMS {
    let mut text = BoxNode::new_text(Arc::clone(&text_style), format!("abs {idx}"));
    text.id = 20_000 + idx;

    let mut abs_child = BoxNode::new_block(
      Arc::clone(&abs_style),
      FormattingContextType::Block,
      vec![text],
    );
    abs_child.id = idx + 1;
    children.push(abs_child);
  }

  let mut root = BoxNode::new_block(grid_style, FormattingContextType::Grid, children);
  root.id = 2_000_000;
  let tree = BoxTree::new(root);

  let _ = engine.layout_tree(&tree).expect("layout should succeed");

  let shaping_pipeline_news = ShapingPipeline::debug_new_call_count();
  let factory_news = FormattingContextFactory::debug_with_font_context_viewport_and_cb_call_count();
  let detached_news = FormattingContextFactory::debug_detached_call_count();
  let inline_fc_news = InlineFormattingContext::debug_with_font_context_viewport_and_cb_call_count();
  let inline_fc_with_factory_news = InlineFormattingContext::debug_with_factory_call_count();

  assert!(
    shaping_pipeline_news < 20,
    "positioned children layout should not rebuild shaping pipelines (got {shaping_pipeline_news})"
  );
  assert!(
    factory_news < 20,
    "positioned children layout should not rebuild formatting context factories (got {factory_news})"
  );
  assert!(
    detached_news < 50,
    "positioned children layout should not churn detached factories (got {detached_news})"
  );
  assert!(
    inline_fc_news < 20,
    "positioned children layout should not rebuild inline formatting contexts via `with_font_context_viewport_and_cb` (got {inline_fc_news})"
  );
  // See the note in the flex positioned-children churn test: positioned boxes establish a new
  // containing block for their descendants, so `InlineFormattingContext::with_factory` call counts
  // are expected to scale with the number of positioned boxes.
  let _ = inline_fc_with_factory_news;
}

#[test]
fn inline_positioned_children_do_not_churn_detached_factories() {
  let _guard = CHURN_COUNTER_LOCK
    .lock()
    .unwrap_or_else(|err| err.into_inner());
  const ITEMS: usize = 256;

  let viewport = Size::new(800.0, 600.0);
  let config = LayoutConfig::for_viewport(viewport);
  let engine = LayoutEngine::with_font_context(config, FontContext::new());

  // Reset churn counters after engine creation so we measure just the layout run.
  ShapingPipeline::debug_reset_new_call_count();
  FormattingContextFactory::debug_reset_with_font_context_viewport_and_cb_call_count();
  FormattingContextFactory::debug_reset_detached_call_count();
  InlineFormattingContext::debug_reset_with_font_context_viewport_and_cb_call_count();
  InlineFormattingContext::debug_reset_with_factory_call_count();

  let mut root_style = ComputedStyle::default();
  root_style.display = Display::Block;
  root_style.position = Position::Relative;
  root_style.width = Some(Length::px(viewport.width));
  let root_style = Arc::new(root_style);

  let mut abs_style = ComputedStyle::default();
  abs_style.display = Display::Block;
  abs_style.position = Position::Absolute;
  abs_style.left = Some(Length::px(0.0));
  abs_style.top = Some(Length::px(0.0));
  let abs_style = Arc::new(abs_style);

  let mut text_style = ComputedStyle::default();
  text_style.display = Display::Inline;
  let text_style = Arc::new(text_style);

  let mut children = Vec::with_capacity(ITEMS + 1);
  let mut in_flow_text = BoxNode::new_text(Arc::clone(&text_style), "hello".to_string());
  in_flow_text.id = 30_000_000;
  children.push(in_flow_text);

  for idx in 0..ITEMS {
    let mut text = BoxNode::new_text(Arc::clone(&text_style), format!("abs {idx}"));
    text.id = 30_000 + idx;

    let mut abs_child = BoxNode::new_block(
      Arc::clone(&abs_style),
      FormattingContextType::Block,
      vec![text],
    );
    abs_child.id = idx + 1;
    children.push(abs_child);
  }

  let mut root = BoxNode::new_block(root_style, FormattingContextType::Inline, children);
  root.id = 3_000_000;
  let tree = BoxTree::new(root);

  let _ = engine.layout_tree(&tree).expect("layout should succeed");

  let shaping_pipeline_news = ShapingPipeline::debug_new_call_count();
  let factory_news = FormattingContextFactory::debug_with_font_context_viewport_and_cb_call_count();
  let detached_news = FormattingContextFactory::debug_detached_call_count();
  let inline_fc_news = InlineFormattingContext::debug_with_font_context_viewport_and_cb_call_count();
  let inline_fc_with_factory_news = InlineFormattingContext::debug_with_factory_call_count();

  assert!(
    shaping_pipeline_news < 20,
    "positioned children layout should not rebuild shaping pipelines (got {shaping_pipeline_news})"
  );
  assert!(
    factory_news < 20,
    "positioned children layout should not rebuild formatting context factories (got {factory_news})"
  );
  assert!(
    detached_news < 50,
    "positioned children layout should not churn detached factories (got {detached_news})"
  );
  assert!(
    inline_fc_news < 20,
    "positioned children layout should not rebuild inline formatting contexts via `with_font_context_viewport_and_cb` (got {inline_fc_news})"
  );
  // Positioned boxes establish a new containing block for their descendants, so we do not assert a
  // fixed upper bound on `InlineFormattingContext::with_factory` calls here.
  let _ = inline_fc_with_factory_news;
}

#[test]
fn block_positioned_children_do_not_churn_detached_factories() {
  let _guard = CHURN_COUNTER_LOCK
    .lock()
    .unwrap_or_else(|err| err.into_inner());
  const ITEMS: usize = 256;

  let viewport = Size::new(800.0, 600.0);
  let config = LayoutConfig::for_viewport(viewport);
  let engine = LayoutEngine::with_font_context(config, FontContext::new());

  // Reset churn counters after engine creation so we measure just the layout run.
  ShapingPipeline::debug_reset_new_call_count();
  FormattingContextFactory::debug_reset_with_font_context_viewport_and_cb_call_count();
  FormattingContextFactory::debug_reset_detached_call_count();
  InlineFormattingContext::debug_reset_with_font_context_viewport_and_cb_call_count();
  InlineFormattingContext::debug_reset_with_factory_call_count();

  let mut root_style = ComputedStyle::default();
  root_style.display = Display::Block;
  // Establish a positioned containing block so absolute children use the padding box CB.
  root_style.position = Position::Relative;
  root_style.width = Some(Length::px(viewport.width));
  root_style.height = Some(Length::px(viewport.height));
  let root_style = Arc::new(root_style);

  let mut abs_style = ComputedStyle::default();
  abs_style.display = Display::Block;
  abs_style.position = Position::Absolute;
  abs_style.left = Some(Length::px(0.0));
  abs_style.top = Some(Length::px(0.0));
  let abs_style = Arc::new(abs_style);

  let mut text_style = ComputedStyle::default();
  text_style.display = Display::Inline;
  let text_style = Arc::new(text_style);

  let mut inner_children = Vec::with_capacity(ITEMS);
  for idx in 0..ITEMS {
    let mut text = BoxNode::new_text(Arc::clone(&text_style), format!("abs {idx}"));
    text.id = 40_000 + idx;

    let mut abs_child = BoxNode::new_block(
      Arc::clone(&abs_style),
      FormattingContextType::Block,
      vec![text],
    );
    abs_child.id = idx + 1;
    inner_children.push(abs_child);
  }

  // Ensure the positioned-children layout runs through `layout_block_child` by nesting the
  // positioned container as an in-flow child of the root block.
  let mut inner_style = ComputedStyle::default();
  inner_style.display = Display::Block;
  inner_style.position = Position::Relative;
  inner_style.width = Some(Length::px(viewport.width));
  inner_style.height = Some(Length::px(viewport.height));
  let inner_style = Arc::new(inner_style);
  let mut inner = BoxNode::new_block(inner_style, FormattingContextType::Block, inner_children);
  inner.id = 4_000_000;

  let mut root = BoxNode::new_block(root_style, FormattingContextType::Block, vec![inner]);
  root.id = 4_000_001;
  let tree = BoxTree::new(root);

  let _ = engine.layout_tree(&tree).expect("layout should succeed");

  let shaping_pipeline_news = ShapingPipeline::debug_new_call_count();
  let factory_news = FormattingContextFactory::debug_with_font_context_viewport_and_cb_call_count();
  let detached_news = FormattingContextFactory::debug_detached_call_count();
  let inline_fc_news = InlineFormattingContext::debug_with_font_context_viewport_and_cb_call_count();
  let inline_fc_with_factory_news = InlineFormattingContext::debug_with_factory_call_count();

  assert!(
    shaping_pipeline_news < 20,
    "positioned children layout should not rebuild shaping pipelines (got {shaping_pipeline_news})"
  );
  assert!(
    factory_news < 20,
    "positioned children layout should not rebuild formatting context factories (got {factory_news})"
  );
  assert!(
    detached_news < 50,
    "positioned children layout should not churn detached factories (got {detached_news})"
  );
  assert!(
    inline_fc_news < 20,
    "positioned children layout should not rebuild inline formatting contexts via `with_font_context_viewport_and_cb` (got {inline_fc_news})"
  );
  // Positioned boxes establish a new containing block for their descendants, so we do not assert a
  // fixed upper bound on `InlineFormattingContext::with_factory` calls here.
  let _ = inline_fc_with_factory_news;
}
