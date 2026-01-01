//! Regression tests for layout hot-path churn.
//!
//! These tests are intended to catch accidental reintroduction of per-measure / per-intrinsic
//! construction of heavyweight layout/text components such as `ShapingPipeline` and
//! `FormattingContextFactory`.

use std::sync::Arc;

use fastrender::layout::contexts::block::BlockFormattingContext;
use fastrender::layout::contexts::inline::InlineFormattingContext;
use fastrender::layout::taffy_integration::{taffy_perf_counters, TaffyPerfCountersGuard};
use fastrender::style::display::Display;
use fastrender::text::ShapingPipeline;
use fastrender::{
  BoxNode, BoxTree, ComputedStyle, FontContext, FormattingContext, FormattingContextFactory,
  FormattingContextType, IntrinsicSizingMode, LayoutConfig, LayoutEngine, Size,
};

#[test]
fn layout_does_not_rebuild_shaping_pipeline_or_factory_in_hot_paths() {
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
  InlineFormattingContext::debug_reset_with_font_context_viewport_and_cb_call_count();

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
  let inline_fc_news = InlineFormattingContext::debug_with_font_context_viewport_and_cb_call_count();

  assert!(
    shaping_pipeline_news < 20,
    "layout should not rebuild shaping pipelines in hot paths (got {shaping_pipeline_news})"
  );
  assert!(
    factory_news < 20,
    "layout should not rebuild formatting context factories in hot paths (got {factory_news})"
  );
  assert!(
    inline_fc_news < 20,
    "layout should not rebuild inline formatting contexts via `with_font_context_viewport_and_cb` in hot paths (got {inline_fc_news})"
  );
}

#[test]
fn block_intrinsic_sizing_does_not_rebuild_shaping_pipeline_or_factory() {
  let viewport = Size::new(800.0, 600.0);
  let factory = FormattingContextFactory::with_font_context_and_viewport(FontContext::new(), viewport);
  let bfc = BlockFormattingContext::with_factory(factory);

  // Reset churn counters after initialization so we measure just the intrinsic sizing calls.
  ShapingPipeline::debug_reset_new_call_count();
  FormattingContextFactory::debug_reset_with_font_context_viewport_and_cb_call_count();
  InlineFormattingContext::debug_reset_with_font_context_viewport_and_cb_call_count();

  let mut text_style = ComputedStyle::default();
  text_style.display = Display::Inline;
  let text_style = Arc::new(text_style);
  let mut root = BoxNode::new_block(
    Arc::new(ComputedStyle::default()),
    FormattingContextType::Block,
    vec![BoxNode::new_text(text_style, "hello world".to_string())],
  );
  // Disable the intrinsic sizing cache so every call recomputes.
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
    InlineFormattingContext::debug_with_font_context_viewport_and_cb_call_count(),
    0,
    "block intrinsic sizing should not construct inline formatting contexts via `with_font_context_viewport_and_cb`"
  );
}
