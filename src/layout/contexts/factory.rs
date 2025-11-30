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

use crate::layout::constraints::LayoutConstraints;
use crate::layout::contexts::block::BlockFormattingContext;
use crate::layout::contexts::flex::FlexFormattingContext;
use crate::layout::contexts::grid::GridFormattingContext;
use crate::layout::contexts::inline::InlineFormattingContext;
use crate::layout::formatting_context::{FormattingContext, LayoutError};
use crate::layout::table::TableFormattingContext;
use crate::tree::{BoxNode, FormattingContextType};

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
/// use fastrender::layout::contexts::FormattingContextFactory;
/// use fastrender::layout::LayoutConstraints;
/// use fastrender::tree::{BoxNode, FormattingContextType};
/// use fastrender::tree::box_tree::ComputedStyle;
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
/// - Factory is stateless for simplicity
/// - Returns `Box<dyn FormattingContext>` for polymorphism
pub struct FormattingContextFactory;

impl FormattingContextFactory {
    /// Creates a new factory
    pub fn new() -> Self {
        Self
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
    pub fn create_for_box(&self, box_node: &BoxNode) -> Result<Box<dyn FormattingContext>, LayoutError> {
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
    /// use fastrender::layout::contexts::FormattingContextFactory;
    /// use fastrender::tree::FormattingContextType;
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
            FormattingContextType::Block => Box::new(BlockFormattingContext::new()),
            FormattingContextType::Inline => Box::new(InlineFormattingContext::new()),
            FormattingContextType::Flex => Box::new(FlexFormattingContext::new()),
            FormattingContextType::Grid => Box::new(GridFormattingContext::new()),
            FormattingContextType::Table => Box::new(TableFormattingContext::new()),
        }
    }

    /// Returns all supported formatting context types
    ///
    /// Useful for iteration, testing, or introspection of supported layout modes.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::layout::contexts::FormattingContextFactory;
    ///
    /// let factory = FormattingContextFactory::new();
    /// for fc_type in factory.supported_types() {
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
    /// use fastrender::layout::contexts::FormattingContextFactory;
    /// use fastrender::tree::FormattingContextType;
    ///
    /// let factory = FormattingContextFactory::new();
    /// assert!(factory.is_supported(FormattingContextType::Block));
    /// assert!(factory.is_supported(FormattingContextType::Flex));
    /// ```
    pub fn is_supported(&self, fc_type: FormattingContextType) -> bool {
        self.supported_types().contains(&fc_type)
    }
}

impl Default for FormattingContextFactory {
    fn default() -> Self {
        Self::new()
    }
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;
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
}
