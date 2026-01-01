//! FormattingContext factory
//!
//! Creates the appropriate FormattingContext implementation based on box properties.
//! This is the central dispatch mechanism for layout - given a box, create the
//! right formatting context to lay it out.
//!
//! # Design
//!
//! The factory uses the Strategy pattern to decouple layout algorithm selection
//! from the algorithms themselves. This allows:
//! - Adding new layout modes without modifying core code
//! - Testing individual FCs in isolation
//! - Replacing stub FCs with real implementations incrementally
//!
//! # Implementations
//!
//! - W3.T04: BlockFormattingContext
//! - W3.T06: TableFormattingContext
//! - W3.T08: FlexFormattingContext (Taffy-backed)
//! - W3.T09: GridFormattingContext (Taffy-backed)
//! - W4.T12: InlineFormattingContext

use crate::layout::contexts::block::BlockFormattingContext;
use crate::layout::contexts::flex::FlexFormattingContext;
use crate::layout::contexts::flex_cache::ShardedFlexCache;
use crate::layout::contexts::grid::GridFormattingContext;
use crate::layout::contexts::inline::InlineFormattingContext;
use crate::layout::contexts::positioned::ContainingBlock;
use crate::layout::engine::LayoutParallelism;
use crate::layout::formatting_context::FormattingContext;
use crate::layout::formatting_context::LayoutError;
use crate::layout::table::TableFormattingContext;
use crate::layout::taffy_integration::{
  taffy_template_cache_limit, taffy_template_cache_limit_for_box_tree, TaffyAdapterKind,
  TaffyNodeCache,
};
use crate::style::display::FormattingContextType;
use crate::text::font_loader::FontContext;
use crate::text::pipeline::ShapingPipeline;
use crate::tree::box_tree::BoxNode;
use std::sync::Arc;
use std::sync::OnceLock;

#[cfg(any(test, debug_assertions))]
use std::sync::atomic::{AtomicUsize, Ordering};

#[cfg(any(test, debug_assertions))]
static FACTORY_WITH_FONT_CONTEXT_VIEWPORT_AND_CB_CALLS: AtomicUsize = AtomicUsize::new(0);

#[derive(Default)]
struct CachedFormattingContexts {
  block: OnceLock<Arc<dyn FormattingContext>>,
  inline: OnceLock<Arc<dyn FormattingContext>>,
  flex: OnceLock<Arc<dyn FormattingContext>>,
  grid: OnceLock<Arc<dyn FormattingContext>>,
}

impl CachedFormattingContexts {
  fn fresh() -> Arc<Self> {
    Arc::new(Self::default())
  }
}

// =============================================================================
// Factory
// =============================================================================

/// Factory for creating FormattingContext implementations
///
/// The factory determines which formatting context to use based on a box's
/// properties and returns the appropriate implementation.
///
/// # Usage
///
/// ```
/// use fastrender::FormattingContextFactory;
/// use fastrender::LayoutConstraints;
/// use fastrender::{BoxNode, FormattingContextType};
/// use fastrender::ComputedStyle;
/// use std::sync::Arc;
///
/// let factory = FormattingContextFactory::new();
/// let style = Arc::new(ComputedStyle::default());
/// let box_node = BoxNode::new_block(style, FormattingContextType::Block, vec![]);
///
/// let fc = factory.create_for_box(&box_node).unwrap();
/// let constraints = LayoutConstraints::definite(800.0, 600.0);
/// let fragment = fc.layout(&box_node, &constraints).unwrap();
/// ```
///
/// # Design Notes
///
/// - Carries a shared `FontContext` so all formatting contexts use the same font cache
/// - Returns `Box<dyn FormattingContext>` for polymorphism
#[derive(Clone)]
pub struct FormattingContextFactory {
  font_context: FontContext,
  viewport_size: crate::geometry::Size,
  nearest_positioned_cb: ContainingBlock,
  flex_measure_cache: std::sync::Arc<ShardedFlexCache>,
  flex_layout_cache: std::sync::Arc<ShardedFlexCache>,
  flex_taffy_cache: std::sync::Arc<TaffyNodeCache>,
  grid_taffy_cache: std::sync::Arc<TaffyNodeCache>,
  shaping_pipeline: crate::text::pipeline::ShapingPipeline,
  parallelism: LayoutParallelism,
  cached_contexts: Arc<CachedFormattingContexts>,
}

impl std::fmt::Debug for FormattingContextFactory {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("FormattingContextFactory")
      .finish_non_exhaustive()
  }
}

impl FormattingContextFactory {
  /// Clone this factory while detaching the per-factory cached formatting contexts.
  ///
  /// The clone continues to share heavyweight caches (flex measurement/layout caches, taffy
  /// node caches, the shaping pipeline, and the font context), but it gets a fresh
  /// `cached_contexts` store.
  ///
  /// This is intended for embedding a `FormattingContextFactory` inside formatting contexts:
  /// cached formatting contexts (e.g. flex/grid) are themselves stored inside `cached_contexts`,
  /// so storing a factory clone that shares `cached_contexts` would create a strong reference
  /// cycle:
  ///
  /// `cached_contexts -> Arc<dyn FormattingContext> -> FormattingContextFactory -> cached_contexts`
  pub(crate) fn detached(&self) -> Self {
    let mut clone = self.clone();
    clone.reset_cached_contexts();
    clone
  }

  fn block_context(&self) -> BlockFormattingContext {
    // BlockFormattingContext stores a factory clone, so it must not share this factory's
    // cached formatting context instances or we'd create an Arc cycle:
    // `factory.cached_contexts.block -> Arc<BlockFormattingContext> -> factory`.
    BlockFormattingContext::with_factory(self.detached())
  }

  pub(crate) fn inline_context(&self) -> InlineFormattingContext {
    InlineFormattingContext::with_factory(self.detached())
  }

  fn flex_context(&self) -> FlexFormattingContext {
    FlexFormattingContext::with_factory(self.detached())
  }

  fn grid_context(&self) -> GridFormattingContext {
    GridFormattingContext::with_factory(self.detached())
  }

  fn table_context(&self) -> TableFormattingContext {
    TableFormattingContext::with_factory(self.clone())
  }

  fn reset_cached_contexts(&mut self) {
    self.cached_contexts = CachedFormattingContexts::fresh();
  }

  /// Creates a new factory
  pub fn new() -> Self {
    let viewport_size = crate::geometry::Size::new(800.0, 600.0);
    Self::with_font_context_viewport_and_cb(
      FontContext::new(),
      viewport_size,
      ContainingBlock::viewport(viewport_size),
    )
  }

  /// Creates a factory wired to a specific font context, allowing layout to share
  /// font caches with paint and callers.
  pub fn with_font_context(font_context: FontContext) -> Self {
    let viewport_size = crate::geometry::Size::new(800.0, 600.0);
    Self::with_font_context_viewport_and_cb(
      font_context,
      viewport_size,
      ContainingBlock::viewport(viewport_size),
    )
  }

  /// Creates a factory wired to a specific font context and viewport size, allowing layout to share
  /// both font caches and viewport-dependent resolution with callers.
  pub fn with_font_context_and_viewport(
    font_context: FontContext,
    viewport_size: crate::geometry::Size,
  ) -> Self {
    let cb = ContainingBlock::viewport(viewport_size);
    Self::with_font_context_viewport_and_cb(font_context, viewport_size, cb)
  }

  /// Creates a factory with explicit font context, viewport, and nearest positioned containing block.
  pub fn with_font_context_viewport_and_cb(
    font_context: FontContext,
    viewport_size: crate::geometry::Size,
    nearest_positioned_cb: ContainingBlock,
  ) -> Self {
    #[cfg(any(test, debug_assertions))]
    FACTORY_WITH_FONT_CONTEXT_VIEWPORT_AND_CB_CALLS.fetch_add(1, Ordering::Relaxed);
    let flex_cache_limit = taffy_template_cache_limit(TaffyAdapterKind::Flex);
    let grid_cache_limit = taffy_template_cache_limit(TaffyAdapterKind::Grid);
    Self::with_font_context_viewport_cb_and_cache(
      font_context,
      viewport_size,
      nearest_positioned_cb,
      std::sync::Arc::new(ShardedFlexCache::new_measure()),
      std::sync::Arc::new(ShardedFlexCache::new_layout()),
      std::sync::Arc::new(TaffyNodeCache::new(flex_cache_limit)),
      std::sync::Arc::new(TaffyNodeCache::new(grid_cache_limit)),
    )
  }

  pub(crate) fn with_font_context_viewport_cb_and_cache(
    font_context: FontContext,
    viewport_size: crate::geometry::Size,
    nearest_positioned_cb: ContainingBlock,
    flex_measure_cache: std::sync::Arc<ShardedFlexCache>,
    flex_layout_cache: std::sync::Arc<ShardedFlexCache>,
    flex_taffy_cache: std::sync::Arc<TaffyNodeCache>,
    grid_taffy_cache: std::sync::Arc<TaffyNodeCache>,
  ) -> Self {
    Self {
      font_context,
      viewport_size,
      nearest_positioned_cb,
      flex_measure_cache,
      flex_layout_cache,
      flex_taffy_cache,
      grid_taffy_cache,
      shaping_pipeline: ShapingPipeline::new(),
      parallelism: LayoutParallelism::default(),
      cached_contexts: CachedFormattingContexts::fresh(),
    }
  }

  /// Returns a copy of this factory with an updated nearest positioned containing block.
  pub fn with_positioned_cb(&self, cb: ContainingBlock) -> Self {
    let mut clone = self.clone();
    clone.nearest_positioned_cb = cb;
    clone.reset_cached_contexts();
    clone
  }

  /// Returns a copy of this factory configured with the given parallelism settings.
  pub fn with_parallelism(mut self, parallelism: LayoutParallelism) -> Self {
    self.parallelism = parallelism;
    self.reset_cached_contexts();
    self
  }

  /// Returns the active layout parallelism configuration.
  pub fn parallelism(&self) -> LayoutParallelism {
    self.parallelism
  }

  pub(crate) fn flex_measure_cache(&self) -> std::sync::Arc<ShardedFlexCache> {
    self.flex_measure_cache.clone()
  }

  pub(crate) fn flex_layout_cache(&self) -> std::sync::Arc<ShardedFlexCache> {
    self.flex_layout_cache.clone()
  }

  pub(crate) fn flex_taffy_cache(&self) -> std::sync::Arc<TaffyNodeCache> {
    self.flex_taffy_cache.clone()
  }

  pub(crate) fn grid_taffy_cache(&self) -> std::sync::Arc<TaffyNodeCache> {
    self.grid_taffy_cache.clone()
  }

  pub(crate) fn tune_taffy_template_cache_for_box_tree(&self, box_tree_nodes: usize) {
    let flex_limit =
      taffy_template_cache_limit_for_box_tree(TaffyAdapterKind::Flex, box_tree_nodes);
    self.flex_taffy_cache.grow_to(flex_limit);
    let grid_limit =
      taffy_template_cache_limit_for_box_tree(TaffyAdapterKind::Grid, box_tree_nodes);
    self.grid_taffy_cache.grow_to(grid_limit);
  }

  pub(crate) fn shaping_cache_size(&self) -> usize {
    self.shaping_pipeline.cache_len()
  }

  pub(crate) fn shaping_pipeline(&self) -> ShapingPipeline {
    self.shaping_pipeline.clone()
  }

  pub(crate) fn with_shaping_pipeline(mut self, shaping_pipeline: ShapingPipeline) -> Self {
    self.shaping_pipeline = shaping_pipeline;
    self.reset_cached_contexts();
    self
  }

  /// Clears any shared caches held by formatting contexts.
  pub fn reset_caches(&self) {
    self.flex_measure_cache.clear();
    self.flex_layout_cache.clear();
    self.flex_taffy_cache.clear();
    self.grid_taffy_cache.clear();
    self.shaping_pipeline.clear_cache();
  }

  /// Returns the font context backing formatting context construction.
  pub fn font_context(&self) -> &FontContext {
    &self.font_context
  }

  /// Returns the viewport size used for viewport-relative length resolution.
  pub fn viewport_size(&self) -> crate::geometry::Size {
    self.viewport_size
  }

  /// Returns the nearest positioned containing block threaded into newly constructed contexts.
  pub fn nearest_positioned_cb(&self) -> ContainingBlock {
    self.nearest_positioned_cb
  }

  /// Creates the appropriate FormattingContext for a box
  ///
  /// Examines the box's formatting context type and returns the
  /// corresponding FC implementation.
  ///
  /// # Errors
  ///
  /// Returns `LayoutError::UnsupportedBoxType` if the box doesn't
  /// establish a formatting context (e.g., inline boxes).
  pub fn create_for_box(
    &self,
    box_node: &BoxNode,
  ) -> Result<Box<dyn FormattingContext>, LayoutError> {
    let fc_type = box_node.formatting_context().ok_or_else(|| {
      LayoutError::UnsupportedBoxType("Box does not establish a formatting context".to_string())
    })?;

    Ok(self.create(fc_type))
  }

  /// Creates a formatting context for the specified type
  ///
  /// This is the primary factory method that maps formatting context types
  /// to their concrete implementations:
  ///
  /// - `Block` → `BlockFormattingContext` (CSS 2.1 §9.4.1)
  /// - `Inline` → `InlineFormattingContext` (CSS 2.1 §9.4.2)
  /// - `Flex` → `FlexFormattingContext` (CSS Flexbox via Taffy)
  /// - `Grid` → `GridFormattingContext` (CSS Grid via Taffy)
  /// - `Table` → `TableFormattingContext` (CSS 2.1 §17)
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::FormattingContextFactory;
  /// use fastrender::FormattingContextType;
  ///
  /// let factory = FormattingContextFactory::new();
  /// let block_fc = factory.create(FormattingContextType::Block);
  /// let flex_fc = factory.create(FormattingContextType::Flex);
  /// ```
  ///
  /// # Implementation Notes
  ///
  /// All formatting contexts are:
  /// - Stateless (no internal mutable state)
  /// - Thread-safe (`Send + Sync`)
  /// - Reusable (can be used for multiple layouts)
  pub fn create(&self, fc_type: FormattingContextType) -> Box<dyn FormattingContext> {
    match fc_type {
      FormattingContextType::Block => Box::new(self.block_context()),
      FormattingContextType::Inline => Box::new(self.inline_context()),
      FormattingContextType::Flex => Box::new(self.flex_context()),
      FormattingContextType::Grid => Box::new(self.grid_context()),
      FormattingContextType::Table => Box::new(self.table_context()),
    }
  }

  /// Returns a cached, shared formatting context for the specified type.
  ///
  /// This avoids repeated heap allocations in hot paths (e.g., Taffy measure callbacks) by
  /// reusing stateless formatting context instances per factory. Contexts inherit the factory's
  /// viewport, containing block, shaping pipeline, shared flex caches, and parallelism settings.
  pub fn get(&self, fc_type: FormattingContextType) -> Arc<dyn FormattingContext> {
    match fc_type {
      FormattingContextType::Block => self
        .cached_contexts
        .block
        .get_or_init(|| Arc::new(self.block_context()))
        .clone(),
      FormattingContextType::Inline => self
        .cached_contexts
        .inline
        .get_or_init(|| Arc::new(self.inline_context()))
        .clone(),
      FormattingContextType::Flex => self
        .cached_contexts
        .flex
        .get_or_init(|| Arc::new(self.flex_context()))
        .clone(),
      FormattingContextType::Grid => self
        .cached_contexts
        .grid
        .get_or_init(|| Arc::new(self.grid_context()))
        .clone(),
      // TableFormattingContext maintains per-layout state (structure) so keep it uncached.
      FormattingContextType::Table => Arc::new(self.table_context()),
    }
  }

  /// Invokes a closure with a cached formatting context, avoiding an explicit clone in callers.
  pub fn with_fc<R>(
    &self,
    fc_type: FormattingContextType,
    f: impl FnOnce(&dyn FormattingContext) -> R,
  ) -> R {
    let fc = self.get(fc_type);
    f(fc.as_ref())
  }

  /// Returns all supported formatting context types
  ///
  /// Useful for iteration, testing, or introspection of supported layout modes.
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::FormattingContextFactory;
  ///
  /// let factory = FormattingContextFactory::new();
  /// for &fc_type in factory.supported_types() {
  ///     let _fc = factory.create(fc_type);
  /// }
  /// ```
  pub fn supported_types(&self) -> &'static [FormattingContextType] {
    &[
      FormattingContextType::Block,
      FormattingContextType::Inline,
      FormattingContextType::Flex,
      FormattingContextType::Grid,
      FormattingContextType::Table,
    ]
  }

  /// Checks if a formatting context type is supported
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::FormattingContextFactory;
  /// use fastrender::FormattingContextType;
  ///
  /// let factory = FormattingContextFactory::new();
  /// assert!(factory.is_supported(FormattingContextType::Block));
  /// assert!(factory.is_supported(FormattingContextType::Flex));
  /// ```
  pub fn is_supported(&self, fc_type: FormattingContextType) -> bool {
    self.supported_types().contains(&fc_type)
  }
}

#[cfg(any(test, debug_assertions))]
impl FormattingContextFactory {
  #[doc(hidden)]
  pub fn debug_with_font_context_viewport_and_cb_call_count() -> usize {
    FACTORY_WITH_FONT_CONTEXT_VIEWPORT_AND_CB_CALLS.load(Ordering::Relaxed)
  }

  #[doc(hidden)]
  pub fn debug_reset_with_font_context_viewport_and_cb_call_count() {
    FACTORY_WITH_FONT_CONTEXT_VIEWPORT_AND_CB_CALLS.store(0, Ordering::Relaxed);
  }
}

impl Default for FormattingContextFactory {
  fn default() -> Self {
    Self::new()
  }
}

#[cfg(test)]
impl FormattingContextFactory {
  pub(crate) fn cached_context_is_initialized(&self, fc_type: FormattingContextType) -> bool {
    match fc_type {
      FormattingContextType::Block => self.cached_contexts.block.get().is_some(),
      FormattingContextType::Inline => self.cached_contexts.inline.get().is_some(),
      FormattingContextType::Flex => self.cached_contexts.flex.get().is_some(),
      FormattingContextType::Grid => self.cached_contexts.grid.get().is_some(),
      FormattingContextType::Table => false,
    }
  }
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
  use super::*;
  use crate::layout::constraints::LayoutConstraints;
  use crate::layout::contexts::positioned::ContainingBlock;
  use crate::layout::engine::LayoutParallelism;
  use crate::style::ComputedStyle;
  use std::sync::Arc;

  fn default_style() -> Arc<ComputedStyle> {
    Arc::new(ComputedStyle::default())
  }

  #[test]
  fn test_factory_creation() {
    let _factory = FormattingContextFactory::new();
    let _default = FormattingContextFactory::default();
  }

  #[test]
  fn test_create_block_fc() {
    let factory = FormattingContextFactory::new();
    let box_node = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);

    let fc = factory.create_for_box(&box_node).unwrap();
    let constraints = LayoutConstraints::definite(800.0, 600.0);
    let fragment = fc.layout(&box_node, &constraints).unwrap();

    // Real BlockFormattingContext uses available width
    assert!(fragment.bounds.width() > 0.0);
  }

  #[test]
  fn test_create_inline_fc() {
    let factory = FormattingContextFactory::new();
    let fc = factory.create(FormattingContextType::Inline);

    let box_node = BoxNode::new_block(default_style(), FormattingContextType::Inline, vec![]);
    let constraints = LayoutConstraints::definite(800.0, 600.0);
    let fragment = fc.layout(&box_node, &constraints).unwrap();

    // Empty inline container has no content, so width should be 0 or based on content
    assert!(fragment.bounds.width() >= 0.0);
  }

  #[test]
  fn test_create_flex_fc() {
    let factory = FormattingContextFactory::new();
    let box_node = BoxNode::new_block(default_style(), FormattingContextType::Flex, vec![]);

    let fc = factory.create_for_box(&box_node).unwrap();
    let constraints = LayoutConstraints::definite(800.0, 600.0);
    let fragment = fc.layout(&box_node, &constraints).unwrap();

    assert_eq!(fragment.bounds.width(), 800.0);
  }

  #[test]
  fn test_create_grid_fc() {
    let factory = FormattingContextFactory::new();
    let box_node = BoxNode::new_block(default_style(), FormattingContextType::Grid, vec![]);

    let fc = factory.create_for_box(&box_node).unwrap();
    let constraints = LayoutConstraints::definite(800.0, 600.0);
    let fragment = fc.layout(&box_node, &constraints).unwrap();

    assert_eq!(fragment.bounds.width(), 800.0);
  }

  #[test]
  fn test_create_table_fc() {
    let factory = FormattingContextFactory::new();
    let box_node = BoxNode::new_block(default_style(), FormattingContextType::Table, vec![]);

    let fc = factory.create_for_box(&box_node).unwrap();
    let constraints = LayoutConstraints::definite(800.0, 600.0);
    let fragment = fc.layout(&box_node, &constraints).unwrap();

    // Real TableFormattingContext returns table width based on content
    assert!(fragment.bounds.width() >= 0.0);
    assert!(fragment.bounds.height() >= 0.0);
  }

  #[test]
  fn test_inline_box_fails() {
    let factory = FormattingContextFactory::new();
    let box_node = BoxNode::new_inline(default_style(), vec![]);

    let result = factory.create_for_box(&box_node);
    assert!(result.is_err());
  }

  #[test]
  fn test_create_all_types() {
    let factory = FormattingContextFactory::new();

    for &fc_type in factory.supported_types() {
      let fc = factory.create(fc_type);
      let box_node = BoxNode::new_block(default_style(), fc_type, vec![]);
      let constraints = LayoutConstraints::definite(800.0, 600.0);
      assert!(fc.layout(&box_node, &constraints).is_ok());
    }
  }

  #[test]
  fn test_factory_reuse() {
    let factory = FormattingContextFactory::new();

    for _ in 0..10 {
      let fc = factory.create(FormattingContextType::Flex);
      let box_node = BoxNode::new_block(default_style(), FormattingContextType::Flex, vec![]);
      let constraints = LayoutConstraints::definite(800.0, 600.0);
      assert!(fc.layout(&box_node, &constraints).is_ok());
    }
  }

  #[test]
  fn test_supported_types() {
    let factory = FormattingContextFactory::new();
    let types = factory.supported_types();

    assert_eq!(types.len(), 5);
    assert!(types.contains(&FormattingContextType::Block));
    assert!(types.contains(&FormattingContextType::Inline));
    assert!(types.contains(&FormattingContextType::Flex));
    assert!(types.contains(&FormattingContextType::Grid));
    assert!(types.contains(&FormattingContextType::Table));
  }

  #[test]
  fn test_is_supported() {
    let factory = FormattingContextFactory::new();

    assert!(factory.is_supported(FormattingContextType::Block));
    assert!(factory.is_supported(FormattingContextType::Inline));
    assert!(factory.is_supported(FormattingContextType::Flex));
    assert!(factory.is_supported(FormattingContextType::Grid));
    assert!(factory.is_supported(FormattingContextType::Table));
  }

  #[test]
  fn test_factory_is_send_sync() {
    fn assert_send_sync<T: Send + Sync>() {}
    assert_send_sync::<FormattingContextFactory>();
  }

  #[test]
  fn test_created_fc_is_send_sync() {
    let factory = FormattingContextFactory::new();

    for &fc_type in factory.supported_types() {
      let fc = factory.create(fc_type);
      // This verifies that Box<dyn FormattingContext> is Send + Sync
      let _ = std::thread::spawn(move || {
        let _ = fc;
      });
    }
  }

  #[test]
  fn test_get_reuses_cached_instances() {
    let factory = FormattingContextFactory::new();
    for fc_type in [
      FormattingContextType::Block,
      FormattingContextType::Inline,
      FormattingContextType::Flex,
      FormattingContextType::Grid,
    ] {
      let a = factory.get(fc_type);
      let b = factory.get(fc_type);
      assert!(Arc::ptr_eq(&a, &b));
    }

    let table_a = factory.get(FormattingContextType::Table);
    let table_b = factory.get(FormattingContextType::Table);
    assert!(!Arc::ptr_eq(&table_a, &table_b));
  }

  #[test]
  fn test_get_does_not_cache_table_instances() {
    let factory = FormattingContextFactory::new();
    let a = factory.get(FormattingContextType::Table);
    let b = factory.get(FormattingContextType::Table);
    assert!(!Arc::ptr_eq(&a, &b));
    assert!(!factory.cached_context_is_initialized(FormattingContextType::Table));
  }

  #[test]
  fn test_get_resets_with_configuration_changes() {
    let factory = FormattingContextFactory::new();
    let original = factory.get(FormattingContextType::Block);

    let different_cb = ContainingBlock::viewport(crate::geometry::Size::new(320.0, 240.0));
    let with_cb = factory.with_positioned_cb(different_cb);
    let cb_fc = with_cb.get(FormattingContextType::Block);
    assert!(!Arc::ptr_eq(&original, &cb_fc));

    let parallel = factory.with_parallelism(LayoutParallelism::enabled(2));
    let parallel_fc = parallel.get(FormattingContextType::Block);
    assert!(!Arc::ptr_eq(&original, &parallel_fc));
  }

  #[test]
  fn test_get_does_not_leak_cached_contexts_via_factory_cycles() {
    let factory = FormattingContextFactory::new();
    let weak_cached_contexts = Arc::downgrade(&factory.cached_contexts);

    let flex = factory.get(FormattingContextType::Flex);
    let grid = factory.get(FormattingContextType::Grid);
    assert!(factory.cached_context_is_initialized(FormattingContextType::Flex));
    assert!(factory.cached_context_is_initialized(FormattingContextType::Grid));

    drop(flex);
    drop(grid);
    drop(factory);

    assert!(
      weak_cached_contexts.upgrade().is_none(),
      "cached_contexts should be freed after dropping the factory and its cached contexts",
    );
  }
}
