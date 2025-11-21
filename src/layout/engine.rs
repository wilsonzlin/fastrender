//! Layout Engine - Orchestrates the layout process
//!
//! The LayoutEngine is the main entry point for layout. It:
//! 1. Takes a box tree and viewport size
//! 2. Orchestrates formatting context creation via factory
//! 3. Performs layout recursively
//! 4. Returns a positioned fragment tree

use crate::geometry::Size;
use crate::layout::{AvailableSpace, FormattingContext, FormattingContextFactory, LayoutConstraints, LayoutError};
use crate::tree::{BoxNode, BoxTree, Fragment, FragmentTree};

/// Configuration for layout engine
#[derive(Debug, Clone)]
pub struct LayoutConfig {
    /// Enable caching of layout results (placeholder for future)
    pub enable_cache: bool,

    /// Enable incremental layout (placeholder for future)
    pub enable_incremental: bool,
}

impl LayoutConfig {
    /// Creates default configuration
    pub fn new() -> Self {
        Self {
            enable_cache: false,
            enable_incremental: false,
        }
    }
}

impl Default for LayoutConfig {
    fn default() -> Self {
        Self::new()
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
/// use fastrender::layout::{LayoutEngine, LayoutConfig};
/// use fastrender::tree::{BoxTree, BoxNode, FormattingContextType};
/// use fastrender::tree::box_tree::ComputedStyle;
/// use fastrender::geometry::Size;
///
/// let engine = LayoutEngine::new(LayoutConfig::new());
///
/// let style = Arc::new(ComputedStyle::default());
/// let root = BoxNode::new_block(
///     style,
///     FormattingContextType::BlockFormatting,
///     vec![],
/// );
/// let box_tree = BoxTree::new(root);
///
/// let viewport = Size::new(800.0, 600.0);
/// let fragment_tree = engine.layout_tree(&box_tree, viewport).unwrap();
/// ```
pub struct LayoutEngine {
    /// Configuration
    config: LayoutConfig,

    /// Formatting context factory
    factory: FormattingContextFactory,
    // Placeholder for future additions:
    // cache: Option<LayoutCache>,
    // context: LayoutContext,
}

impl LayoutEngine {
    /// Creates a new layout engine with the given configuration
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::layout::{LayoutEngine, LayoutConfig};
    ///
    /// let engine = LayoutEngine::new(LayoutConfig::new());
    /// ```
    pub fn new(config: LayoutConfig) -> Self {
        Self {
            config,
            factory: FormattingContextFactory::new(),
        }
    }

    /// Performs layout on an entire box tree
    ///
    /// This is the main entry point for layout. It takes a box tree
    /// and viewport size, and returns a positioned fragment tree.
    ///
    /// # Arguments
    ///
    /// * `box_tree` - The box tree to layout
    /// * `viewport` - The viewport size (available space for root)
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
    /// use fastrender::layout::{LayoutEngine, LayoutConfig};
    /// use fastrender::tree::{BoxTree, BoxNode, FormattingContextType};
    /// use fastrender::tree::box_tree::ComputedStyle;
    /// use fastrender::geometry::Size;
    ///
    /// let engine = LayoutEngine::new(LayoutConfig::new());
    ///
    /// let style = Arc::new(ComputedStyle::default());
    /// let root = BoxNode::new_block(
    ///     style,
    ///     FormattingContextType::BlockFormatting,
    ///     vec![],
    /// );
    /// let box_tree = BoxTree::new(root);
    ///
    /// let viewport = Size::new(1024.0, 768.0);
    /// let fragment_tree = engine.layout_tree(&box_tree, viewport).unwrap();
    ///
    /// assert_eq!(fragment_tree.viewport_size().width, 1024.0);
    /// ```
    pub fn layout_tree(&self, box_tree: &BoxTree, viewport: Size) -> Result<FragmentTree, LayoutError> {
        // Create root constraints from viewport
        let constraints = LayoutConstraints::with_definite_size(viewport.width, viewport.height);

        // Layout the root box
        let root_fragment = self.layout_subtree(&box_tree.root, &constraints)?;

        // Create fragment tree
        Ok(FragmentTree::new(root_fragment))
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
    /// use fastrender::layout::{LayoutEngine, LayoutConfig, LayoutConstraints};
    /// use fastrender::tree::{BoxNode, FormattingContextType};
    /// use fastrender::tree::box_tree::ComputedStyle;
    ///
    /// let engine = LayoutEngine::new(LayoutConfig::new());
    ///
    /// let style = Arc::new(ComputedStyle::default());
    /// let box_node = BoxNode::new_block(
    ///     style,
    ///     FormattingContextType::BlockFormatting,
    ///     vec![],
    /// );
    ///
    /// let constraints = LayoutConstraints::with_definite_size(800.0, 600.0);
    /// let fragment = engine.layout_subtree(&box_node, &constraints).unwrap();
    /// ```
    pub fn layout_subtree(&self, box_node: &BoxNode, constraints: &LayoutConstraints) -> Result<Fragment, LayoutError> {
        // Get the formatting context type for this box
        let fc_type = box_node
            .formatting_context()
            .ok_or_else(|| LayoutError::MissingContext("Box does not establish a formatting context".to_string()))?;

        // Create the appropriate formatting context via factory
        let fc = self.factory.create(fc_type);

        // Call the FC's layout method
        fc.layout(box_node, constraints)
    }

    /// Returns the engine's configuration
    pub fn config(&self) -> &LayoutConfig {
        &self.config
    }

    // Future methods (placeholders):
    //
    // pub fn invalidate_cache(&mut self, box_id: BoxId) { ... }
    // pub fn layout_incremental(&self, ...) -> Result<...> { ... }
    // pub fn compute_intrinsic_sizes(&self, box_node: &BoxNode) -> IntrinsicSizes { ... }
}

impl Default for LayoutEngine {
    fn default() -> Self {
        Self::new(LayoutConfig::default())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tree::FormattingContextType;
    use std::sync::Arc;

    fn default_style() -> Arc<crate::tree::box_tree::ComputedStyle> {
        Arc::new(crate::tree::box_tree::ComputedStyle::default())
    }

    #[test]
    fn test_layout_config_new() {
        let config = LayoutConfig::new();
        assert!(!config.enable_cache);
        assert!(!config.enable_incremental);
    }

    #[test]
    fn test_layout_config_default() {
        let config = LayoutConfig::default();
        assert!(!config.enable_cache);
    }

    #[test]
    fn test_engine_new() {
        let config = LayoutConfig::new();
        let engine = LayoutEngine::new(config);
        assert!(!engine.config().enable_cache);
    }

    #[test]
    fn test_engine_default() {
        let engine = LayoutEngine::default();
        assert!(!engine.config().enable_cache);
    }

    #[test]
    fn test_layout_tree_simple() {
        let engine = LayoutEngine::new(LayoutConfig::new());

        // Create simple box tree with just root
        let root = BoxNode::new_block(default_style(), FormattingContextType::BlockFormatting, vec![]);
        let box_tree = BoxTree::new(root);

        // Layout with viewport
        let viewport = Size::new(800.0, 600.0);
        let fragment_tree = engine.layout_tree(&box_tree, viewport).unwrap();

        // Check viewport size
        assert_eq!(fragment_tree.viewport_size().width, 800.0);
        assert_eq!(fragment_tree.viewport_size().height, 600.0);
    }

    #[test]
    fn test_layout_tree_with_children() {
        let engine = LayoutEngine::new(LayoutConfig::new());

        // Create box tree with children
        let child1 = BoxNode::new_block(default_style(), FormattingContextType::BlockFormatting, vec![]);
        let child2 = BoxNode::new_block(default_style(), FormattingContextType::BlockFormatting, vec![]);

        let root = BoxNode::new_block(
            default_style(),
            FormattingContextType::BlockFormatting,
            vec![child1, child2],
        );
        let box_tree = BoxTree::new(root);

        let viewport = Size::new(1024.0, 768.0);
        let fragment_tree = engine.layout_tree(&box_tree, viewport).unwrap();

        assert_eq!(fragment_tree.viewport_size().width, 1024.0);
    }

    #[test]
    fn test_layout_subtree() {
        let engine = LayoutEngine::new(LayoutConfig::new());

        let box_node = BoxNode::new_block(default_style(), FormattingContextType::BlockFormatting, vec![]);

        let constraints = LayoutConstraints::with_definite_size(640.0, 480.0);
        let fragment = engine.layout_subtree(&box_node, &constraints).unwrap();

        assert_eq!(fragment.bounds().width(), 640.0);
        assert_eq!(fragment.bounds().height(), 480.0);
    }

    #[test]
    fn test_layout_subtree_different_fc_types() {
        let engine = LayoutEngine::new(LayoutConfig::new());

        // Test block FC
        let block_box = BoxNode::new_block(default_style(), FormattingContextType::BlockFormatting, vec![]);
        let constraints = LayoutConstraints::with_definite_size(800.0, 600.0);
        let _fragment = engine.layout_subtree(&block_box, &constraints).unwrap();

        // Flex FC would be tested similarly when implemented
        // For now, the stub just returns empty fragments
    }

    #[test]
    fn test_layout_text_box_error() {
        let engine = LayoutEngine::new(LayoutConfig::new());

        // Text boxes don't establish formatting contexts
        let text_box = BoxNode::new_text(default_style(), "Text".to_string());
        let constraints = LayoutConstraints::with_definite_size(800.0, 600.0);

        let result = engine.layout_subtree(&text_box, &constraints);
        assert!(result.is_err());
    }

    #[test]
    fn test_layout_different_viewport_sizes() {
        let engine = LayoutEngine::new(LayoutConfig::new());

        let root = BoxNode::new_block(default_style(), FormattingContextType::BlockFormatting, vec![]);
        let box_tree = BoxTree::new(root);

        // Test various viewport sizes
        let viewports = vec![
            Size::new(320.0, 568.0),   // iPhone SE
            Size::new(375.0, 812.0),   // iPhone X
            Size::new(768.0, 1024.0),  // iPad
            Size::new(1920.0, 1080.0), // Desktop
        ];

        for viewport in viewports {
            let fragment_tree = engine.layout_tree(&box_tree, viewport).unwrap();
            assert_eq!(fragment_tree.viewport_size().width, viewport.width);
            assert_eq!(fragment_tree.viewport_size().height, viewport.height);
        }
    }
}
