//! Layout Engine - Orchestrates the layout process
//!
//! The LayoutEngine is the main entry point for layout. It:
//! 1. Takes a box tree and viewport size
//! 2. Orchestrates formatting context creation via factory
//! 3. Performs layout recursively
//! 4. Returns a positioned fragment tree
//!
//! # Architecture
//!
//! The layout engine implements a clean separation of concerns:
//! - **LayoutEngine**: High-level orchestration, FC creation/dispatch, error handling
//! - **FormattingContexts**: Algorithm-specific layout logic (block, flex, grid, etc.)
//!
//! This allows formatting contexts to focus purely on layout algorithms while
//! the engine handles cross-cutting concerns.

use crate::debug::runtime;
use crate::debug::trace::TraceHandle;
use crate::error::{RenderError, RenderStage};
use crate::geometry::Size;
use crate::layout::constraints::LayoutConstraints;
use crate::layout::contexts::factory::FormattingContextFactory;
use crate::layout::formatting_context::intrinsic_cache_use_epoch;
use crate::layout::formatting_context::layout_cache_stats;
use crate::layout::formatting_context::layout_cache_use_epoch;
use crate::layout::formatting_context::IntrinsicSizingMode;
use crate::layout::formatting_context::LayoutError;
use crate::layout::fragmentation;
use crate::layout::fragmentation::FragmentationOptions;
use crate::render_control::{check_active, DeadlineGuard, RenderDeadline};
use crate::style::display::FormattingContextType;
use crate::style::{block_axis_is_horizontal, inline_axis_is_horizontal, ComputedStyle};
use crate::text::font_loader::FontContext;
use crate::tree::box_tree::BoxNode;
use crate::tree::box_tree::BoxTree;
use crate::tree::fragment_tree::FragmentNode;
use crate::tree::fragment_tree::FragmentTree;
use rayon::ThreadPool;
use rayon::ThreadPoolBuilder;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;
use std::sync::Arc;

/// Parallel layout configuration
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LayoutParallelism {
  /// Whether layout fan-out is enabled for independent subtrees.
  pub enabled: bool,
  /// Optional ceiling on rayon worker count when fan-out is enabled.
  pub max_threads: Option<usize>,
  /// Minimum number of independent work items before spawning.
  pub min_fanout: usize,
}

impl LayoutParallelism {
  pub fn disabled() -> Self {
    Self {
      enabled: false,
      max_threads: None,
      min_fanout: usize::MAX,
    }
  }

  pub fn enabled(min_fanout: usize) -> Self {
    Self {
      enabled: true,
      max_threads: None,
      min_fanout: min_fanout.max(1),
    }
  }

  pub fn with_max_threads(mut self, threads: Option<usize>) -> Self {
    self.max_threads = threads.map(|threads| threads.max(1));
    self
  }

  pub fn with_min_fanout(mut self, min_fanout: usize) -> Self {
    self.min_fanout = min_fanout.max(1);
    self
  }

  pub fn should_parallelize(&self, work_items: usize) -> bool {
    self.enabled && work_items >= self.min_fanout && rayon::current_num_threads() > 1
  }
}

impl Default for LayoutParallelism {
  fn default() -> Self {
    LayoutParallelism::disabled()
  }
}

/// Configuration for layout engine
///
/// Contains settings that affect how layout is performed.
/// Future additions might include cache size limits, parallelization settings,
/// debug/profiling flags, and feature flags.
///
/// # Examples
///
/// ```
/// use fastrender::LayoutConfig;
/// use fastrender::Size;
///
/// // Create config for specific viewport
/// let config = LayoutConfig::for_viewport(Size::new(1920.0, 1080.0));
/// assert_eq!(config.initial_containing_block.width, 1920.0);
///
/// // Or use defaults (800x600)
/// let default_config = LayoutConfig::default();
/// assert_eq!(default_config.initial_containing_block.width, 800.0);
/// ```
#[derive(Debug, Clone)]
pub struct LayoutConfig {
  /// Initial containing block size (typically the viewport)
  ///
  /// This is used as the percentage base for the root element
  /// and defines the available space for layout.
  pub initial_containing_block: Size,

  /// Enable caching of layout results (placeholder for future)
  ///
  /// When true, the engine will cache layout results for subtrees
  /// that haven't changed.
  pub enable_cache: bool,

  /// Enable incremental layout (placeholder for future)
  ///
  /// When true, only changed subtrees will be re-laid out.
  pub enable_incremental: bool,

  /// Optional fragmentation options for pagination/columns.
  pub fragmentation: Option<FragmentationOptions>,

  /// Optional identifier for profiling/logging (e.g., page name)
  pub name: Option<String>,

  /// Parallel layout configuration (off by default).
  pub parallelism: LayoutParallelism,
}

impl LayoutConfig {
  /// Creates configuration with specified initial containing block size
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::LayoutConfig;
  /// use fastrender::Size;
  ///
  /// let config = LayoutConfig::new(Size::new(1024.0, 768.0));
  /// assert_eq!(config.initial_containing_block.width, 1024.0);
  /// ```
  pub fn new(initial_containing_block: Size) -> Self {
    Self {
      initial_containing_block,
      enable_cache: false,
      enable_incremental: false,
      fragmentation: None,
      name: None,
      parallelism: LayoutParallelism::default(),
    }
  }

  /// Creates configuration for a viewport size
  ///
  /// This is a convenience constructor that creates a config
  /// with the given viewport as the initial containing block.
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::LayoutConfig;
  /// use fastrender::Size;
  ///
  /// let config = LayoutConfig::for_viewport(Size::new(1920.0, 1080.0));
  /// assert_eq!(config.initial_containing_block.width, 1920.0);
  /// assert_eq!(config.initial_containing_block.height, 1080.0);
  /// ```
  pub fn for_viewport(viewport: Size) -> Self {
    Self::new(viewport)
  }

  /// Creates a pagination-aware configuration with a given page size and gap between pages.
  ///
  /// The initial containing block is set to the provided page size and fragmentation is
  /// configured to split content at that block-size with the supplied gap.
  pub fn for_pagination(page_size: Size, gap: f32) -> Self {
    let fragmentation = FragmentationOptions::new(page_size.height).with_gap(gap);
    Self::new(page_size).with_fragmentation(fragmentation)
  }

  /// Enables fragmentation of the resulting fragment tree.
  ///
  /// `FragmentationOptions` can express pagination (single column) or multi-column
  /// fragmentation (`with_columns`) with independent gaps for fragmentainers and columns.
  pub fn with_fragmentation(mut self, fragmentation: FragmentationOptions) -> Self {
    self.fragmentation = Some(fragmentation);
    self
  }

  /// Sets an optional identifier (e.g., page name) for logging/profiling.
  pub fn with_identifier(mut self, name: impl Into<String>) -> Self {
    self.name = Some(name.into());
    self
  }

  /// Enables layout fan-out with the provided parallelism configuration.
  pub fn with_parallelism(mut self, parallelism: LayoutParallelism) -> Self {
    self.parallelism = parallelism;
    self
  }
}

impl Default for LayoutConfig {
  /// Creates default configuration with 800x600 viewport
  fn default() -> Self {
    Self::new(Size::new(800.0, 600.0))
  }
}

/// Statistics about layout operations
///
/// Tracks metrics about layout performance. Cache hit/miss counts reflect
/// intrinsic sizing cache usage, while `layout_cache_*` track subtree layout
/// cache reuse. `total_layouts` reports how many layout passes the engine has
/// orchestrated.
///
/// # Examples
///
/// ```
/// use fastrender::{LayoutConfig, LayoutEngine};
///
/// let engine = LayoutEngine::with_defaults();
/// let stats = engine.stats();
/// println!("Cache hits: {}", stats.cache_hits);
/// ```
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct LayoutStats {
  /// Number of cache hits (intrinsic sizing cache)
  pub cache_hits: usize,

  /// Number of cache misses (intrinsic sizing cache)
  pub cache_misses: usize,

  /// Number of layout cache hits (subtree layout cache)
  pub layout_cache_hits: usize,

  /// Number of layout cache misses (subtree layout cache)
  pub layout_cache_misses: usize,

  /// Number of cached entries evicted during this run.
  pub layout_cache_evictions: usize,

  /// Total number of layout operations performed
  pub total_layouts: usize,
}

/// Placeholder for layout cache (future implementation)
///
/// Will cache layout results keyed by:
/// - Box identity (pointer or ID)
/// - Constraints used for layout
/// - Style snapshot (for invalidation)
#[derive(Debug, Default)]
struct LayoutCache {
  epoch: AtomicUsize,
  runs: AtomicUsize,
}

impl LayoutCache {
  fn new() -> Self {
    Self {
      epoch: AtomicUsize::new(0),
      runs: AtomicUsize::new(0),
    }
  }

  fn start_run(&self, reset: bool) -> usize {
    let epoch = if reset {
      self.epoch.fetch_add(1, Ordering::Relaxed) + 1
    } else {
      self.epoch.load(Ordering::Relaxed).max(1)
    };
    self.runs.fetch_add(1, Ordering::Relaxed);
    epoch
  }

  fn run_count(&self) -> usize {
    self.runs.load(Ordering::Relaxed)
  }
}

/// The layout engine orchestrator
///
/// This is the main entry point for performing layout on a box tree.
///
/// # Architecture
///
/// The engine:
/// 1. Uses FormattingContextFactory to create appropriate FCs
/// 2. Calls layout() recursively on the tree
/// 3. Handles caching (future)
/// 4. Manages layout context (future)
///
/// # Examples
///
/// ```
/// use std::sync::Arc;
/// use fastrender::{LayoutEngine, LayoutConfig};
/// use fastrender::{BoxTree, BoxNode, FormattingContextType};
/// use fastrender::ComputedStyle;
/// use fastrender::Size;
///
/// let config = LayoutConfig::for_viewport(Size::new(800.0, 600.0));
/// let engine = LayoutEngine::new(config);
///
/// let style = Arc::new(ComputedStyle::default());
/// let root = BoxNode::new_block(
///     style,
///     FormattingContextType::Block,
///     vec![],
/// );
/// let box_tree = BoxTree::new(root);
///
/// let fragment_tree = engine.layout_tree(&box_tree).unwrap();
/// ```
pub struct LayoutEngine {
  /// Configuration
  config: LayoutConfig,

  /// Formatting context factory
  factory: FormattingContextFactory,

  /// Shared font context used for all layout text shaping and measurement.
  font_context: FontContext,

  /// Layout cache (placeholder for future optimization)
  cache: LayoutCache,

  /// Optional rayon pool honoring the configured thread cap.
  parallel_pool: Option<Arc<ThreadPool>>,
}

impl LayoutEngine {
  /// Creates a new layout engine with the given configuration
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::{LayoutEngine, LayoutConfig};
  /// use fastrender::Size;
  ///
  /// let config = LayoutConfig::new(Size::new(1024.0, 768.0));
  /// let engine = LayoutEngine::new(config);
  /// ```
  pub fn new(config: LayoutConfig) -> Self {
    Self::with_font_context(config, FontContext::new())
  }

  /// Creates a layout engine that uses the provided font context for all text measurement.
  ///
  /// Sharing the font context with paint keeps font fallback, cache warming, and font loading
  /// consistent across the pipeline.
  pub fn with_font_context(config: LayoutConfig, font_context: FontContext) -> Self {
    let factory = FormattingContextFactory::with_font_context_and_viewport(
      font_context.clone(),
      config.initial_containing_block,
    )
    .with_parallelism(config.parallelism);
    let parallel_pool = if config.parallelism.enabled {
      config
        .parallelism
        .max_threads
        .filter(|threads| *threads > 1)
        .and_then(|threads| ThreadPoolBuilder::new().num_threads(threads).build().ok())
        .map(Arc::new)
    } else {
      None
    };
    Self {
      config,
      factory,
      font_context,
      cache: LayoutCache::new(),
      parallel_pool,
    }
  }

  /// Creates a layout engine with default configuration
  ///
  /// Uses default viewport size of 800x600.
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::LayoutEngine;
  ///
  /// let engine = LayoutEngine::with_defaults();
  /// assert_eq!(engine.config().initial_containing_block.width, 800.0);
  /// ```
  pub fn with_defaults() -> Self {
    Self::new(LayoutConfig::default())
  }

  /// Performs layout on an entire box tree
  ///
  /// This is the main entry point for layout. It takes a box tree
  /// and returns a positioned fragment tree using the viewport size
  /// from the engine's configuration.
  ///
  /// # Arguments
  ///
  /// * `box_tree` - The box tree to layout
  ///
  /// # Returns
  ///
  /// A fragment tree with all boxes positioned
  ///
  /// # Errors
  ///
  /// Returns LayoutError if layout cannot proceed
  ///
  /// # Examples
  ///
  /// ```
  /// use std::sync::Arc;
  /// use fastrender::{LayoutEngine, LayoutConfig};
  /// use fastrender::{BoxTree, BoxNode, FormattingContextType};
  /// use fastrender::ComputedStyle;
  /// use fastrender::Size;
  ///
  /// let config = LayoutConfig::for_viewport(Size::new(1024.0, 768.0));
  /// let engine = LayoutEngine::new(config);
  ///
  /// let style = Arc::new(ComputedStyle::default());
  /// let root = BoxNode::new_block(
  ///     style,
  ///     FormattingContextType::Block,
  ///     vec![],
  /// );
  /// let box_tree = BoxTree::new(root);
  ///
  /// let fragment_tree = engine.layout_tree(&box_tree).unwrap();
  /// ```
  pub fn layout_tree(&self, box_tree: &BoxTree) -> Result<FragmentTree, LayoutError> {
    self.layout_tree_internal(box_tree, !self.config.enable_incremental, None)
  }

  /// Performs layout while emitting trace spans to the provided handle.
  pub(crate) fn layout_tree_with_trace(
    &self,
    box_tree: &BoxTree,
    trace: &TraceHandle,
  ) -> Result<FragmentTree, LayoutError> {
    self.layout_tree_internal(box_tree, !self.config.enable_incremental, Some(trace))
  }

  /// Performs layout on a box tree while observing an optional deadline.
  pub fn layout_tree_with_deadline(
    &self,
    box_tree: &BoxTree,
    deadline: Option<&RenderDeadline>,
  ) -> Result<FragmentTree, LayoutError> {
    let _guard = DeadlineGuard::install(deadline);
    if let Err(RenderError::Timeout { elapsed, .. }) = check_active(RenderStage::Layout) {
      return Err(LayoutError::Timeout { elapsed });
    }
    self.layout_tree_internal(box_tree, !self.config.enable_incremental, None)
  }

  /// Performs layout on a box tree, optionally reusing shared caches.
  ///
  /// When `reset_caches` is false, flex measurement/layout caches are preserved, allowing
  /// callers (e.g., container query second passes) to reuse expensive measurements when
  /// the box structure is stable across cascades.
  pub fn layout_tree_reuse_caches(&self, box_tree: &BoxTree) -> Result<FragmentTree, LayoutError> {
    self.layout_tree_internal(box_tree, false, None)
  }

  /// Performs layout while reusing caches and emitting trace spans.
  pub(crate) fn layout_tree_reuse_caches_with_trace(
    &self,
    box_tree: &BoxTree,
    trace: &TraceHandle,
  ) -> Result<FragmentTree, LayoutError> {
    self.layout_tree_internal(box_tree, false, Some(trace))
  }

  fn layout_tree_internal(
    &self,
    box_tree: &BoxTree,
    reset_caches: bool,
    trace: Option<&TraceHandle>,
  ) -> Result<FragmentTree, LayoutError> {
    self.run_in_pool(|| self.layout_tree_inner(box_tree, reset_caches, trace))
  }

  fn layout_tree_inner(
    &self,
    box_tree: &BoxTree,
    reset_caches: bool,
    trace: Option<&TraceHandle>,
  ) -> Result<FragmentTree, LayoutError> {
    if let Err(RenderError::Timeout { elapsed, .. }) = check_active(RenderStage::Layout) {
      return Err(LayoutError::Timeout { elapsed });
    }
    let use_cache = self.config.enable_cache;
    let reset_for_run = reset_caches || !use_cache;

    let epoch = self.cache.start_run(reset_for_run);
    layout_cache_use_epoch(epoch, use_cache, reset_for_run, self.config.fragmentation);
    intrinsic_cache_use_epoch(epoch, reset_for_run);

    if reset_for_run {
      // Reset any per-run caches so a fresh layout starts from a clean slate.
      self.factory.reset_caches();
    }

    // Create root constraints from initial containing block, preferring explicit
    // fragmentainer size when pagination is enabled.
    let icb = &self.config.initial_containing_block;
    let (constraint_width, constraint_height) = if let Some(options) = &self.config.fragmentation {
      let root_style = &box_tree.root.style;
      let wm = root_style.writing_mode;
      let _dir = root_style.direction;
      let inline_is_horizontal = inline_axis_is_horizontal(wm);
      let block_is_horizontal = block_axis_is_horizontal(wm);

      let mut available_width = icb.width;
      let mut available_height = icb.height;

      if options.column_count > 1 {
        let icb_inline = if inline_is_horizontal {
          icb.width
        } else {
          icb.height
        };
        let column_inline = fragmentation::column_inline_size(icb_inline, options);
        if inline_is_horizontal {
          available_width = column_inline;
        } else {
          available_height = column_inline;
        }
      }

      let fragmentainer_block = if options.fragmentainer_size > 0.0 {
        options.fragmentainer_size
      } else if block_is_horizontal {
        icb.width
      } else {
        icb.height
      };

      if block_is_horizontal {
        available_width = fragmentainer_block;
      } else {
        available_height = fragmentainer_block;
      }

      (available_width, available_height)
    } else {
      (icb.width, icb.height)
    };
    let constraints = LayoutConstraints::definite(constraint_width, constraint_height);

    // Layout the root box
    let root_fragment = self.layout_subtree_internal(&box_tree.root, &constraints, trace)?;

    if let Some(options) = &self.config.fragmentation {
      let default_style = ComputedStyle::default();
      let style = root_fragment.style.as_deref().unwrap_or(&default_style);
      let fragments = fragmentation::fragment_tree_for_writing_mode(
        &root_fragment,
        options,
        style.writing_mode,
        style.direction,
      );
      let mut tree = FragmentTree::from_fragments(fragments, *icb);
      tree.ensure_scroll_metadata();
      Ok(tree)
    } else {
      // Create fragment tree with viewport size
      let mut tree = FragmentTree::with_viewport(root_fragment, *icb);
      tree.ensure_scroll_metadata();
      Ok(tree)
    }
  }

  fn run_in_pool<T: Send>(&self, f: impl FnOnce() -> T + Send) -> T {
    if let Some(pool) = &self.parallel_pool {
      pool.install(f)
    } else {
      f()
    }
  }

  /// Performs layout on a subtree rooted at a box node
  ///
  /// This is the recursive layout entry point. It:
  /// 1. Determines the formatting context for this box
  /// 2. Gets the appropriate FC from the factory
  /// 3. Calls the FC's layout method
  /// 4. Returns the positioned fragment
  ///
  /// # Arguments
  ///
  /// * `box_node` - The root of the subtree to layout
  /// * `constraints` - Available space from parent
  ///
  /// # Returns
  ///
  /// A positioned fragment (and its children)
  ///
  /// # Errors
  ///
  /// Returns LayoutError if:
  /// - Box doesn't establish a formatting context (shouldn't happen)
  /// - Layout algorithm fails
  ///
  /// # Examples
  ///
  /// ```
  /// use std::sync::Arc;
  /// use fastrender::{LayoutEngine, LayoutConfig, LayoutConstraints};
  /// use fastrender::{BoxNode, FormattingContextType};
  /// use fastrender::ComputedStyle;
  /// use fastrender::Size;
  ///
  /// let config = LayoutConfig::new(Size::new(800.0, 600.0));
  /// let engine = LayoutEngine::new(config);
  ///
  /// let style = Arc::new(ComputedStyle::default());
  /// let box_node = BoxNode::new_block(
  ///     style,
  ///     FormattingContextType::Block,
  ///     vec![],
  /// );
  ///
  /// let constraints = LayoutConstraints::definite(800.0, 600.0);
  /// let fragment = engine.layout_subtree(&box_node, &constraints).unwrap();
  /// ```
  pub fn layout_subtree(
    &self,
    box_node: &BoxNode,
    constraints: &LayoutConstraints,
  ) -> Result<FragmentNode, LayoutError> {
    if let Err(RenderError::Timeout { elapsed, .. }) = check_active(RenderStage::Layout) {
      return Err(LayoutError::Timeout { elapsed });
    }
    self.layout_subtree_internal(box_node, constraints, None)
  }

  fn layout_subtree_internal(
    &self,
    box_node: &BoxNode,
    constraints: &LayoutConstraints,
    trace: Option<&TraceHandle>,
  ) -> Result<FragmentNode, LayoutError> {
    // Future: Check cache first
    // if let Some(cached) = self.check_cache(box_node, constraints) {
    //     return Ok(cached);
    // }

    // Get the formatting context type for this box
    let fc_type = box_node.formatting_context().ok_or_else(|| {
      LayoutError::MissingContext("Box does not establish a formatting context".to_string())
    })?;

    // Create the appropriate formatting context via factory
    let fc = self.factory.create(fc_type);

    // Optional slow-layout logging for debugging large pages.
    let slow_threshold_ms = runtime::runtime_toggles().u128("FASTR_LOG_SLOW_LAYOUT_MS");
    let slow_timer = slow_threshold_ms.map(|_| std::time::Instant::now());

    // Call the FC's layout method
    let span = trace.map(|t| t.span(formatting_context_span_name(fc_type), "layout"));
    let fragment = fc.layout(box_node, constraints)?;
    drop(span);

    if let (Some(threshold), Some(start)) = (slow_threshold_ms, slow_timer) {
      let elapsed = start.elapsed().as_millis();
      if elapsed >= threshold {
        let selector = box_node
          .debug_info
          .as_ref()
          .map(|d| d.to_selector())
          .unwrap_or_else(|| "<anon>".to_string());
        eprintln!(
          "[layout-slow] id={} fc={:?} ms={} selector={}",
          box_node.id, fc_type, elapsed, selector
        );
      }
    }

    // Future: Store in cache
    // self.store_in_cache(box_node, constraints, &fragment);

    Ok(fragment)
  }

  /// Computes intrinsic size for a box
  ///
  /// This is used when a parent needs to know a child's content-based size
  /// before performing layout (e.g., shrink-to-fit, auto sizing).
  ///
  /// # Arguments
  ///
  /// * `box_node` - The box to measure
  /// * `mode` - MinContent or MaxContent
  ///
  /// # Returns
  ///
  /// The intrinsic inline size (width for horizontal writing mode)
  ///
  /// # Errors
  ///
  /// Returns `LayoutError` if measurement fails
  ///
  /// # Examples
  ///
  /// ```
  /// use std::sync::Arc;
  /// use fastrender::{LayoutEngine, IntrinsicSizingMode};
  /// use fastrender::{BoxNode, FormattingContextType};
  /// use fastrender::ComputedStyle;
  ///
  /// let engine = LayoutEngine::with_defaults();
  /// let style = Arc::new(ComputedStyle::default());
  /// let box_node = BoxNode::new_block(
  ///     style,
  ///     FormattingContextType::Block,
  ///     vec![],
  /// );
  ///
  /// let size = engine.compute_intrinsic_size(
  ///     &box_node,
  ///     IntrinsicSizingMode::MinContent,
  /// ).unwrap();
  /// ```
  pub fn compute_intrinsic_size(
    &self,
    box_node: &BoxNode,
    mode: IntrinsicSizingMode,
  ) -> Result<f32, LayoutError> {
    let fc_type = box_node.formatting_context().ok_or_else(|| {
      LayoutError::MissingContext("Box does not establish a formatting context".to_string())
    })?;

    let fc = self.factory.create(fc_type);
    fc.compute_intrinsic_inline_size(box_node, mode)
  }

  /// Returns the engine's configuration
  pub fn config(&self) -> &LayoutConfig {
    &self.config
  }

  /// Returns the font context backing layout text shaping and intrinsic sizing.
  pub fn font_context(&self) -> &FontContext {
    &self.font_context
  }

  /// Returns statistics about layout operations
  ///
  /// Cache hit/miss counts reflect intrinsic sizing cache usage while
  /// `total_layouts` reports how many layout passes have been executed.
  pub fn stats(&self) -> LayoutStats {
    let (lookups, hits, _, _, _, _) = crate::layout::formatting_context::intrinsic_cache_stats();
    let cache_misses = lookups.saturating_sub(hits);
    let (layout_lookups, layout_hits, _, evictions) = layout_cache_stats();
    let layout_cache_misses = layout_lookups.saturating_sub(layout_hits);
    LayoutStats {
      cache_hits: hits,
      cache_misses,
      layout_cache_hits: layout_hits,
      layout_cache_misses,
      layout_cache_evictions: evictions,
      total_layouts: self.cache.run_count(),
    }
  }

  // Future methods (placeholders):
  //
  // pub fn invalidate_cache(&mut self, box_id: BoxId) { ... }
  // pub fn layout_incremental(&self, ...) -> Result<...> { ... }
}

fn formatting_context_span_name(fc_type: FormattingContextType) -> &'static str {
  match fc_type {
    FormattingContextType::Block => "layout_block",
    FormattingContextType::Inline => "layout_inline",
    FormattingContextType::Flex => "layout_flex",
    FormattingContextType::Grid => "layout_grid",
    FormattingContextType::Table => "layout_table",
  }
}

impl Default for LayoutEngine {
  fn default() -> Self {
    Self::new(LayoutConfig::default())
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::style::display::Display;
  use crate::style::display::FormattingContextType;
  use crate::style::ComputedStyle;
  use std::sync::Arc;

  fn default_style() -> Arc<ComputedStyle> {
    Arc::new(ComputedStyle::default())
  }

  fn flow_root_style() -> Arc<ComputedStyle> {
    let mut style = ComputedStyle::default();
    style.display = Display::FlowRoot;
    Arc::new(style)
  }

  // === LayoutConfig Tests ===

  #[test]
  fn test_layout_config_new() {
    let config = LayoutConfig::new(Size::new(1024.0, 768.0));
    assert_eq!(config.initial_containing_block.width, 1024.0);
    assert_eq!(config.initial_containing_block.height, 768.0);
    assert!(!config.enable_cache);
    assert!(!config.enable_incremental);
  }

  #[test]
  fn test_layout_config_default() {
    let config = LayoutConfig::default();
    assert_eq!(config.initial_containing_block.width, 800.0);
    assert_eq!(config.initial_containing_block.height, 600.0);
    assert!(!config.enable_cache);
  }

  #[test]
  fn test_layout_config_for_viewport() {
    let config = LayoutConfig::for_viewport(Size::new(1920.0, 1080.0));
    assert_eq!(config.initial_containing_block.width, 1920.0);
    assert_eq!(config.initial_containing_block.height, 1080.0);
  }

  // === LayoutEngine Creation Tests ===

  #[test]
  fn test_engine_new() {
    let config = LayoutConfig::new(Size::new(1024.0, 768.0));
    let engine = LayoutEngine::new(config);
    assert_eq!(engine.config().initial_containing_block.width, 1024.0);
    assert!(!engine.config().enable_cache);
  }

  #[test]
  fn test_engine_with_defaults() {
    let engine = LayoutEngine::with_defaults();
    assert_eq!(engine.config().initial_containing_block.width, 800.0);
    assert_eq!(engine.config().initial_containing_block.height, 600.0);
  }

  #[test]
  fn test_engine_default() {
    let engine = LayoutEngine::default();
    assert_eq!(engine.config().initial_containing_block.width, 800.0);
    assert!(!engine.config().enable_cache);
  }

  // === Layout Tree Tests ===

  #[test]
  fn test_layout_tree_simple() {
    let engine = LayoutEngine::with_defaults();

    let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
    let box_tree = BoxTree::new(root);

    let fragment_tree = engine.layout_tree(&box_tree).unwrap();

    assert_eq!(fragment_tree.viewport_size().width, 800.0);
    assert_eq!(fragment_tree.viewport_size().height, 600.0);
  }

  #[test]
  fn test_layout_tree_with_children() {
    let config = LayoutConfig::for_viewport(Size::new(1024.0, 768.0));
    let engine = LayoutEngine::new(config);

    let child1 = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
    let child2 = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);

    let root = BoxNode::new_block(
      default_style(),
      FormattingContextType::Block,
      vec![child1, child2],
    );
    let box_tree = BoxTree::new(root);

    let fragment_tree = engine.layout_tree(&box_tree).unwrap();

    assert_eq!(fragment_tree.viewport_size().width, 1024.0);
    assert_eq!(fragment_tree.viewport_size().height, 768.0);
  }

  #[test]
  fn test_layout_tree_custom_viewport() {
    let config = LayoutConfig::for_viewport(Size::new(1920.0, 1080.0));
    let engine = LayoutEngine::new(config);

    let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
    let box_tree = BoxTree::new(root);

    let fragment_tree = engine.layout_tree(&box_tree).unwrap();
    assert_eq!(fragment_tree.viewport_size().width, 1920.0);
    assert_eq!(fragment_tree.viewport_size().height, 1080.0);
  }

  // === Layout Subtree Tests ===

  #[test]
  fn test_layout_subtree() {
    let engine = LayoutEngine::with_defaults();

    let box_node = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);

    let constraints = LayoutConstraints::definite(640.0, 480.0);
    let fragment = engine.layout_subtree(&box_node, &constraints).unwrap();

    // Real BlockFormattingContext: empty block uses available width, 0 height
    assert_eq!(fragment.bounds.width(), 640.0);
    assert_eq!(fragment.bounds.height(), 0.0);
  }

  #[test]
  fn test_layout_subtree_different_fc_types() {
    let engine = LayoutEngine::with_defaults();
    let constraints = LayoutConstraints::definite(800.0, 600.0);

    // Test Block FC
    let block_box = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
    let _fragment = engine.layout_subtree(&block_box, &constraints).unwrap();

    // Test Flex FC
    let flex_box = BoxNode::new_block(default_style(), FormattingContextType::Flex, vec![]);
    let _fragment = engine.layout_subtree(&flex_box, &constraints).unwrap();

    // Test Grid FC
    let grid_box = BoxNode::new_block(default_style(), FormattingContextType::Grid, vec![]);
    let _fragment = engine.layout_subtree(&grid_box, &constraints).unwrap();

    // Test Table FC
    let table_box = BoxNode::new_block(default_style(), FormattingContextType::Table, vec![]);
    let _fragment = engine.layout_subtree(&table_box, &constraints).unwrap();
  }

  // === Error Handling Tests ===

  #[test]
  fn test_layout_text_box_error() {
    let engine = LayoutEngine::with_defaults();

    // Text boxes don't establish formatting contexts
    let text_box = BoxNode::new_text(default_style(), "Text".to_string());
    let constraints = LayoutConstraints::definite(800.0, 600.0);

    let result = engine.layout_subtree(&text_box, &constraints);
    assert!(result.is_err());
  }

  #[test]
  fn test_layout_inline_box_error() {
    let engine = LayoutEngine::with_defaults();

    // Regular inline boxes don't establish formatting contexts
    let inline_box = BoxNode::new_inline(default_style(), vec![]);
    let constraints = LayoutConstraints::definite(800.0, 600.0);

    let result = engine.layout_subtree(&inline_box, &constraints);
    assert!(result.is_err());
  }

  // === Intrinsic Sizing Tests ===

  #[test]
  fn test_compute_intrinsic_size_min_content() {
    let engine = LayoutEngine::with_defaults();
    let box_node = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);

    let size = engine
      .compute_intrinsic_size(&box_node, IntrinsicSizingMode::MinContent)
      .unwrap();
    // Real BlockFormattingContext: empty block has 0 intrinsic size
    assert_eq!(size, 0.0);
  }

  #[test]
  fn test_compute_intrinsic_size_max_content() {
    let engine = LayoutEngine::with_defaults();
    let box_node = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);

    let size = engine
      .compute_intrinsic_size(&box_node, IntrinsicSizingMode::MaxContent)
      .unwrap();
    // Real BlockFormattingContext: empty block has 0 intrinsic size
    assert_eq!(size, 0.0);
  }

  #[test]
  fn test_compute_intrinsic_size_error() {
    let engine = LayoutEngine::with_defaults();
    let text_box = BoxNode::new_text(default_style(), "Text".to_string());

    let result = engine.compute_intrinsic_size(&text_box, IntrinsicSizingMode::MinContent);
    assert!(result.is_err());
  }

  // === Stats Tests ===

  #[test]
  fn test_engine_stats() {
    let engine = LayoutEngine::with_defaults();
    let stats = engine.stats();

    // Cache counters are global and may be updated by other tests running in parallel.
    assert_eq!(stats.total_layouts, 0);
  }

  #[test]
  fn test_layout_stats_default() {
    let stats = LayoutStats::default();
    assert_eq!(stats.cache_hits, 0);
    assert_eq!(stats.cache_misses, 0);
    assert_eq!(stats.layout_cache_hits, 0);
    assert_eq!(stats.layout_cache_misses, 0);
    assert_eq!(stats.layout_cache_evictions, 0);
    assert_eq!(stats.total_layouts, 0);
  }

  // === Engine Reuse Tests ===

  #[test]
  fn test_layout_cache_hits_repeated_layouts() {
    crate::layout::formatting_context::intrinsic_cache_reset_counters();
    crate::layout::formatting_context::layout_cache_reset_counters();

    let mut config = LayoutConfig::for_viewport(Size::new(800.0, 600.0));
    config.enable_cache = true;
    config.enable_incremental = true;
    let engine = LayoutEngine::new(config);

    let root = BoxNode::new_block(flow_root_style(), FormattingContextType::Block, vec![]);
    let tree = BoxTree::new(root);

    engine.layout_tree(&tree).unwrap();
    let first = engine.stats();

    engine.layout_tree(&tree).unwrap();
    let second = engine.stats();

    assert!(second.layout_cache_hits > first.layout_cache_hits);
    assert!(second.layout_cache_hits > 0);
    assert!(second.layout_cache_misses >= first.layout_cache_misses);
  }

  #[test]
  fn test_layout_cache_preserves_output() {
    let mut config_disabled = LayoutConfig::for_viewport(Size::new(640.0, 480.0));
    config_disabled.enable_cache = false;
    let engine_disabled = LayoutEngine::new(config_disabled);

    let mut config_enabled = LayoutConfig::for_viewport(Size::new(640.0, 480.0));
    config_enabled.enable_cache = true;
    config_enabled.enable_incremental = true;
    let engine_enabled = LayoutEngine::new(config_enabled);

    let root = BoxNode::new_block(flow_root_style(), FormattingContextType::Block, vec![]);
    let tree_disabled = BoxTree::new(root.clone());
    let tree_enabled = BoxTree::new(root);

    let frag_disabled = engine_disabled.layout_tree(&tree_disabled).unwrap();
    let frag_enabled = engine_enabled.layout_tree(&tree_enabled).unwrap();

    assert_eq!(frag_disabled.root.bounds, frag_enabled.root.bounds);
    assert_eq!(frag_disabled.viewport_size(), frag_enabled.viewport_size());
  }

  #[test]
  fn test_engine_reuse() {
    let engine = LayoutEngine::with_defaults();

    // Reuse same engine for multiple layouts
    for _ in 0..10 {
      let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
      let box_tree = BoxTree::new(root);
      let result = engine.layout_tree(&box_tree);
      assert!(result.is_ok());
    }
  }

  #[test]
  fn test_multiple_layouts_independent() {
    let engine = LayoutEngine::with_defaults();

    // Layout two different trees
    let root1 = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
    let tree1 = BoxTree::new(root1);

    let root2 = BoxNode::new_block(default_style(), FormattingContextType::Flex, vec![]);
    let tree2 = BoxTree::new(root2);

    let frag1 = engine.layout_tree(&tree1).unwrap();
    let frag2 = engine.layout_tree(&tree2).unwrap();

    // Both should succeed
    assert!(frag1.viewport_size().width > 0.0);
    assert!(frag2.viewport_size().width > 0.0);
  }

  // === Nested FC Tests ===

  #[test]
  fn test_nested_formatting_contexts() {
    let engine = LayoutEngine::with_defaults();

    // Create nested structure: Block → Flex → Block
    let inner_block = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
    let flex_container = BoxNode::new_block(
      default_style(),
      FormattingContextType::Flex,
      vec![inner_block],
    );
    let root = BoxNode::new_block(
      default_style(),
      FormattingContextType::Block,
      vec![flex_container],
    );

    let box_tree = BoxTree::new(root);
    let fragment = engine.layout_tree(&box_tree).unwrap();

    assert!(fragment.viewport_size().width > 0.0);
  }

  #[test]
  fn test_deeply_nested_tree() {
    let engine = LayoutEngine::with_defaults();

    // Create a deeply nested tree
    let mut current = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
    for _ in 0..5 {
      current = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![current]);
    }

    let box_tree = BoxTree::new(current);
    let result = engine.layout_tree(&box_tree);
    assert!(result.is_ok());
  }
}
