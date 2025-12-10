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

use crate::geometry::Size;
use crate::layout::constraints::LayoutConstraints;
use crate::layout::contexts::factory::FormattingContextFactory;
use crate::layout::formatting_context::{IntrinsicSizingMode, LayoutError};
use crate::text::font_loader::FontContext;
use crate::tree::box_tree::{BoxNode, BoxTree};
use crate::tree::fragment_tree::{FragmentNode, FragmentTree};

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
}

impl Default for LayoutConfig {
    /// Creates default configuration with 800x600 viewport
    fn default() -> Self {
        Self::new(Size::new(800.0, 600.0))
    }
}

/// Statistics about layout operations
///
/// Tracks metrics about layout performance. Currently returns zeros
/// as caching is not yet implemented, but the API is stable for
/// future use.
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
    /// Number of cache hits (future)
    pub cache_hits: usize,

    /// Number of cache misses (future)
    pub cache_misses: usize,

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
    // Future: HashMap<CacheKey, CachedFragment>
    _placeholder: (),
}

impl LayoutCache {
    fn new() -> Self {
        Self { _placeholder: () }
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
    _cache: LayoutCache,
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
        );
        Self {
            config,
            factory,
            font_context,
            _cache: LayoutCache::new(),
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
        // Create root constraints from initial containing block
        let icb = &self.config.initial_containing_block;
        let constraints = LayoutConstraints::definite(icb.width, icb.height);

        // Layout the root box
        let root_fragment = self.layout_subtree(&box_tree.root, &constraints)?;

        // Create fragment tree with viewport size
        Ok(FragmentTree::with_viewport(root_fragment, *icb))
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
        // Future: Check cache first
        // if let Some(cached) = self.check_cache(box_node, constraints) {
        //     return Ok(cached);
        // }

        // Get the formatting context type for this box
        let fc_type = box_node
            .formatting_context()
            .ok_or_else(|| LayoutError::MissingContext("Box does not establish a formatting context".to_string()))?;

        // Create the appropriate formatting context via factory
        let fc = self.factory.create(fc_type);

        // Call the FC's layout method
        let fragment = fc.layout(box_node, constraints)?;

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
    pub fn compute_intrinsic_size(&self, box_node: &BoxNode, mode: IntrinsicSizingMode) -> Result<f32, LayoutError> {
        let fc_type = box_node
            .formatting_context()
            .ok_or_else(|| LayoutError::MissingContext("Box does not establish a formatting context".to_string()))?;

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
    /// Currently returns zeros as caching is not yet implemented,
    /// but the API is stable for future use.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::LayoutEngine;
    ///
    /// let engine = LayoutEngine::with_defaults();
    /// let stats = engine.stats();
    /// assert_eq!(stats.cache_hits, 0);
    /// ```
    pub fn stats(&self) -> LayoutStats {
        LayoutStats {
            cache_hits: 0,
            cache_misses: 0,
            total_layouts: 0,
        }
    }

    // Future methods (placeholders):
    //
    // pub fn invalidate_cache(&mut self, box_id: BoxId) { ... }
    // pub fn layout_incremental(&self, ...) -> Result<...> { ... }
}

impl Default for LayoutEngine {
    fn default() -> Self {
        Self::new(LayoutConfig::default())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::style::display::FormattingContextType;
    use crate::style::ComputedStyle;
    use std::sync::Arc;

    fn default_style() -> Arc<ComputedStyle> {
        Arc::new(ComputedStyle::default())
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

        let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![child1, child2]);
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

        // Currently all zeros (cache not implemented)
        assert_eq!(stats.cache_hits, 0);
        assert_eq!(stats.cache_misses, 0);
        assert_eq!(stats.total_layouts, 0);
    }

    #[test]
    fn test_layout_stats_default() {
        let stats = LayoutStats::default();
        assert_eq!(stats.cache_hits, 0);
        assert_eq!(stats.cache_misses, 0);
        assert_eq!(stats.total_layouts, 0);
    }

    // === Engine Reuse Tests ===

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
        let flex_container = BoxNode::new_block(default_style(), FormattingContextType::Flex, vec![inner_block]);
        let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![flex_container]);

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
