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
//! # Current State
//!
//! All FC implementations are stubs that return dummy fragments. They will be
//! replaced with real implementations in Wave 3:
//! - W3.T04: BlockFormattingContext
//! - W3.T06: TableFormattingContext
//! - W3.T08: FlexFormattingContext
//! - W4.T12: InlineFormattingContext
//! - Wave 4+: GridFormattingContext

use crate::geometry::Rect;
use crate::layout::constraints::LayoutConstraints;
use crate::layout::formatting_context::{FormattingContext, IntrinsicSizingMode, LayoutError};
use crate::tree::{BoxNode, FormattingContextType, FragmentNode};

// =============================================================================
// Stub FC Implementations
// =============================================================================

/// Stub Block Formatting Context
///
/// Placeholder until W3.T04 implements real block layout.
struct StubBlockFormattingContext;

impl FormattingContext for StubBlockFormattingContext {
    fn layout(&self, _box_node: &BoxNode, constraints: &LayoutConstraints) -> Result<FragmentNode, LayoutError> {
        let width = constraints.width().unwrap_or(100.0);
        let height = constraints.height().unwrap_or(50.0);
        Ok(FragmentNode::new_block(
            Rect::from_xywh(0.0, 0.0, width, height),
            vec![],
        ))
    }

    fn compute_intrinsic_inline_size(
        &self,
        _box_node: &BoxNode,
        _mode: IntrinsicSizingMode,
    ) -> Result<f32, LayoutError> {
        Ok(100.0)
    }
}

/// Stub Inline Formatting Context
///
/// Placeholder until W4.T12 implements real inline/text layout.
struct StubInlineFormattingContext;

impl FormattingContext for StubInlineFormattingContext {
    fn layout(&self, _box_node: &BoxNode, constraints: &LayoutConstraints) -> Result<FragmentNode, LayoutError> {
        let width = constraints.width().unwrap_or(200.0);
        let height = 20.0; // Line height stub
        Ok(FragmentNode::new_block(
            Rect::from_xywh(0.0, 0.0, width, height),
            vec![],
        ))
    }

    fn compute_intrinsic_inline_size(
        &self,
        _box_node: &BoxNode,
        _mode: IntrinsicSizingMode,
    ) -> Result<f32, LayoutError> {
        Ok(200.0)
    }
}

/// Stub Flex Formatting Context
///
/// Placeholder until W3.T08 implements flexbox (likely delegating to Taffy).
struct StubFlexFormattingContext;

impl FormattingContext for StubFlexFormattingContext {
    fn layout(&self, _box_node: &BoxNode, constraints: &LayoutConstraints) -> Result<FragmentNode, LayoutError> {
        let width = constraints.width().unwrap_or(300.0);
        let height = constraints.height().unwrap_or(100.0);
        Ok(FragmentNode::new_block(
            Rect::from_xywh(0.0, 0.0, width, height),
            vec![],
        ))
    }

    fn compute_intrinsic_inline_size(
        &self,
        _box_node: &BoxNode,
        _mode: IntrinsicSizingMode,
    ) -> Result<f32, LayoutError> {
        Ok(300.0)
    }
}

/// Stub Grid Formatting Context
///
/// Placeholder until Wave 4+ implements grid (likely delegating to Taffy).
struct StubGridFormattingContext;

impl FormattingContext for StubGridFormattingContext {
    fn layout(&self, _box_node: &BoxNode, constraints: &LayoutConstraints) -> Result<FragmentNode, LayoutError> {
        let width = constraints.width().unwrap_or(400.0);
        let height = constraints.height().unwrap_or(400.0);
        Ok(FragmentNode::new_block(
            Rect::from_xywh(0.0, 0.0, width, height),
            vec![],
        ))
    }

    fn compute_intrinsic_inline_size(
        &self,
        _box_node: &BoxNode,
        _mode: IntrinsicSizingMode,
    ) -> Result<f32, LayoutError> {
        Ok(400.0)
    }
}

/// Stub Table Formatting Context
///
/// Placeholder until W3.T06 implements table layout.
struct StubTableFormattingContext;

impl FormattingContext for StubTableFormattingContext {
    fn layout(&self, _box_node: &BoxNode, constraints: &LayoutConstraints) -> Result<FragmentNode, LayoutError> {
        let width = constraints.width().unwrap_or(500.0);
        let height = 200.0; // Arbitrary table height
        Ok(FragmentNode::new_block(
            Rect::from_xywh(0.0, 0.0, width, height),
            vec![],
        ))
    }

    fn compute_intrinsic_inline_size(
        &self,
        _box_node: &BoxNode,
        _mode: IntrinsicSizingMode,
    ) -> Result<f32, LayoutError> {
        Ok(500.0)
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
/// - Stubs will be replaced with real implementations in Wave 3+
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
            FormattingContextType::Block => Box::new(StubBlockFormattingContext),
            FormattingContextType::Inline => Box::new(StubInlineFormattingContext),
            FormattingContextType::Flex => Box::new(StubFlexFormattingContext),
            FormattingContextType::Grid => Box::new(StubGridFormattingContext),
            FormattingContextType::Table => Box::new(StubTableFormattingContext),
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
        let factory = FormattingContextFactory::new();
        let _default = FormattingContextFactory::default();
        drop(factory);
    }

    #[test]
    fn test_create_block_fc() {
        let factory = FormattingContextFactory::new();
        let box_node = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);

        let fc = factory.create_for_box(&box_node).unwrap();
        let constraints = LayoutConstraints::definite(800.0, 600.0);
        let fragment = fc.layout(&box_node, &constraints).unwrap();

        assert_eq!(fragment.bounds.width(), 800.0);
        assert_eq!(fragment.bounds.height(), 600.0);
    }

    #[test]
    fn test_create_inline_fc() {
        let factory = FormattingContextFactory::new();
        let fc = factory.create_specific(FormattingContextType::Inline);

        let box_node = BoxNode::new_block(default_style(), FormattingContextType::Inline, vec![]);
        let constraints = LayoutConstraints::definite(800.0, 600.0);
        let fragment = fc.layout(&box_node, &constraints).unwrap();

        assert_eq!(fragment.bounds.width(), 800.0);
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

        assert_eq!(fragment.bounds.width(), 800.0);
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
    fn test_intrinsic_sizing() {
        let factory = FormattingContextFactory::new();
        let fc = factory.create_specific(FormattingContextType::Block);
        let box_node = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);

        let min = fc
            .compute_intrinsic_inline_size(&box_node, IntrinsicSizingMode::MinContent)
            .unwrap();
        let max = fc
            .compute_intrinsic_inline_size(&box_node, IntrinsicSizingMode::MaxContent)
            .unwrap();

        assert_eq!(min, 100.0);
        assert_eq!(max, 100.0);
    }

    #[test]
    fn test_layout_with_indefinite_constraints() {
        let factory = FormattingContextFactory::new();
        let fc = factory.create_specific(FormattingContextType::Block);
        let box_node = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);

        let constraints = LayoutConstraints::indefinite();
        let fragment = fc.layout(&box_node, &constraints).unwrap();

        // Stub falls back to 100.0 when indefinite
        assert_eq!(fragment.bounds.width(), 100.0);
    }

    #[test]
    fn test_multiple_fc_instances() {
        let factory = FormattingContextFactory::new();
        let fc1 = factory.create_specific(FormattingContextType::Block);
        let fc2 = factory.create_specific(FormattingContextType::Block);

        let box_node = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
        let constraints = LayoutConstraints::definite(800.0, 600.0);

        let frag1 = fc1.layout(&box_node, &constraints).unwrap();
        let frag2 = fc2.layout(&box_node, &constraints).unwrap();

        assert_eq!(frag1.bounds.width(), frag2.bounds.width());
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
