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
pub struct FormattingContextFactory {
    _placeholder: (),
}

impl FormattingContextFactory {
    /// Creates a new factory
    pub fn new() -> Self {
        Self { _placeholder: () }
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

        Ok(self.create_specific(fc_type))
    }

    /// Creates a specific formatting context type
    ///
    /// Lower-level method for when the FC type is already known.
    /// Useful for testing or when bypassing box analysis.
    pub fn create_specific(&self, fc_type: FormattingContextType) -> Box<dyn FormattingContext> {
        match fc_type {
            FormattingContextType::Block => Box::new(BlockFormattingContext::new()),
            FormattingContextType::Inline => Box::new(InlineFormattingContext::new()),
            FormattingContextType::Flex => Box::new(FlexFormattingContext::new()),
            FormattingContextType::Grid => Box::new(GridFormattingContext::new()),
            FormattingContextType::Table => Box::new(TableFormattingContext::new()),
        }
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
    use crate::tree::box_tree::ComputedStyle;
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
        let fc = factory.create_specific(FormattingContextType::Inline);

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
    fn test_create_specific_all_types() {
        let factory = FormattingContextFactory::new();
        let types = [
            FormattingContextType::Block,
            FormattingContextType::Inline,
            FormattingContextType::Flex,
            FormattingContextType::Grid,
            FormattingContextType::Table,
        ];

        for fc_type in types {
            let fc = factory.create_specific(fc_type);
            let box_node = BoxNode::new_block(default_style(), fc_type, vec![]);
            let constraints = LayoutConstraints::definite(800.0, 600.0);
            assert!(fc.layout(&box_node, &constraints).is_ok());
        }
    }

    #[test]
    fn test_factory_reuse() {
        let factory = FormattingContextFactory::new();

        for _ in 0..10 {
            let fc = factory.create_specific(FormattingContextType::Flex);
            let box_node = BoxNode::new_block(default_style(), FormattingContextType::Flex, vec![]);
            let constraints = LayoutConstraints::definite(800.0, 600.0);
            assert!(fc.layout(&box_node, &constraints).is_ok());
        }
    }
}
