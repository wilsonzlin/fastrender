//! Formatting Context Factory
//!
//! Creates appropriate formatting context instances based on box type.

use crate::layout::{FormattingContext, IntrinsicSizingMode, LayoutConstraints, LayoutError};
use crate::tree::{BoxNode, FormattingContextType, Fragment};
use std::sync::Arc;

/// Factory for creating formatting contexts
///
/// This factory determines which layout algorithm to use based on the
/// formatting context type and creates the appropriate FC instance.
pub struct FormattingContextFactory;

impl FormattingContextFactory {
    /// Creates a new factory
    pub fn new() -> Self {
        Self
    }

    /// Creates a formatting context for the given type
    ///
    /// # Arguments
    /// * `fc_type` - The type of formatting context to create
    ///
    /// # Returns
    /// A boxed formatting context instance
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::layout::contexts::FormattingContextFactory;
    /// use fastrender::tree::FormattingContextType;
    ///
    /// let factory = FormattingContextFactory::new();
    /// let fc = factory.create(FormattingContextType::BlockFormatting);
    /// ```
    pub fn create(&self, fc_type: FormattingContextType) -> Box<dyn FormattingContext> {
        match fc_type {
            FormattingContextType::BlockFormatting => Box::new(StubBlockFormattingContext),
            FormattingContextType::InlineFormatting => Box::new(StubInlineFormattingContext),
            FormattingContextType::FlexFormatting => Box::new(StubFlexFormattingContext),
            FormattingContextType::GridFormatting => Box::new(StubGridFormattingContext),
            FormattingContextType::TableFormatting => Box::new(StubTableFormattingContext),
        }
    }

    /// Creates a formatting context for a box based on its properties
    pub fn create_for_box(&self, box_node: &BoxNode) -> Option<Box<dyn FormattingContext>> {
        box_node.formatting_context().map(|fc_type| self.create(fc_type))
    }
}

impl Default for FormattingContextFactory {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// Stub Formatting Contexts
// These are placeholders that will be replaced by real implementations in Wave 3
// ============================================================================

/// Stub block formatting context
///
/// This is a placeholder implementation that will be replaced by the real
/// BlockFormattingContext in W3.T04.
struct StubBlockFormattingContext;

impl FormattingContext for StubBlockFormattingContext {
    fn layout(&self, box_node: &BoxNode, constraints: &LayoutConstraints) -> Result<Fragment, LayoutError> {
        // Stub: just create an empty fragment with constraints size
        let width = constraints.available_width.definite_value().unwrap_or(0.0);
        let height = constraints.available_height.definite_value().unwrap_or(0.0);

        Ok(Fragment::new_block(
            crate::geometry::Rect::from_xywh(0.0, 0.0, width, height),
            box_node.style.clone(),
            vec![],
        ))
    }

    fn compute_intrinsic_inline_size(
        &self,
        _box_node: &BoxNode,
        _mode: IntrinsicSizingMode,
    ) -> Result<f32, LayoutError> {
        // Stub: return zero
        Ok(0.0)
    }
}

/// Stub inline formatting context
struct StubInlineFormattingContext;

impl FormattingContext for StubInlineFormattingContext {
    fn layout(&self, box_node: &BoxNode, constraints: &LayoutConstraints) -> Result<Fragment, LayoutError> {
        let width = constraints.available_width.definite_value().unwrap_or(0.0);
        let height = 20.0; // Stub line height

        Ok(Fragment::new_line(
            crate::geometry::Rect::from_xywh(0.0, 0.0, width, height),
            16.0, // Stub baseline
            box_node.style.clone(),
            vec![],
        ))
    }

    fn compute_intrinsic_inline_size(
        &self,
        _box_node: &BoxNode,
        _mode: IntrinsicSizingMode,
    ) -> Result<f32, LayoutError> {
        Ok(0.0)
    }
}

/// Stub flex formatting context
struct StubFlexFormattingContext;

impl FormattingContext for StubFlexFormattingContext {
    fn layout(&self, box_node: &BoxNode, constraints: &LayoutConstraints) -> Result<Fragment, LayoutError> {
        let width = constraints.available_width.definite_value().unwrap_or(0.0);
        let height = constraints.available_height.definite_value().unwrap_or(0.0);

        Ok(Fragment::new_block(
            crate::geometry::Rect::from_xywh(0.0, 0.0, width, height),
            box_node.style.clone(),
            vec![],
        ))
    }

    fn compute_intrinsic_inline_size(
        &self,
        _box_node: &BoxNode,
        _mode: IntrinsicSizingMode,
    ) -> Result<f32, LayoutError> {
        Ok(0.0)
    }
}

/// Stub grid formatting context
struct StubGridFormattingContext;

impl FormattingContext for StubGridFormattingContext {
    fn layout(&self, box_node: &BoxNode, constraints: &LayoutConstraints) -> Result<Fragment, LayoutError> {
        let width = constraints.available_width.definite_value().unwrap_or(0.0);
        let height = constraints.available_height.definite_value().unwrap_or(0.0);

        Ok(Fragment::new_block(
            crate::geometry::Rect::from_xywh(0.0, 0.0, width, height),
            box_node.style.clone(),
            vec![],
        ))
    }

    fn compute_intrinsic_inline_size(
        &self,
        _box_node: &BoxNode,
        _mode: IntrinsicSizingMode,
    ) -> Result<f32, LayoutError> {
        Ok(0.0)
    }
}

/// Stub table formatting context
struct StubTableFormattingContext;

impl FormattingContext for StubTableFormattingContext {
    fn layout(&self, box_node: &BoxNode, constraints: &LayoutConstraints) -> Result<Fragment, LayoutError> {
        let width = constraints.available_width.definite_value().unwrap_or(0.0);
        let height = constraints.available_height.definite_value().unwrap_or(0.0);

        Ok(Fragment::new_block(
            crate::geometry::Rect::from_xywh(0.0, 0.0, width, height),
            box_node.style.clone(),
            vec![],
        ))
    }

    fn compute_intrinsic_inline_size(
        &self,
        _box_node: &BoxNode,
        _mode: IntrinsicSizingMode,
    ) -> Result<f32, LayoutError> {
        Ok(0.0)
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
    fn test_factory_create_block() {
        let factory = FormattingContextFactory::new();
        let fc = factory.create(FormattingContextType::BlockFormatting);

        let box_node = BoxNode::new_block(default_style(), FormattingContextType::BlockFormatting, vec![]);

        let constraints = LayoutConstraints::with_definite_size(800.0, 600.0);
        let fragment = fc.layout(&box_node, &constraints).unwrap();

        assert_eq!(fragment.bounds().width(), 800.0);
    }

    #[test]
    fn test_factory_create_inline() {
        let factory = FormattingContextFactory::new();
        let fc = factory.create(FormattingContextType::InlineFormatting);

        let box_node = BoxNode::new_inline(default_style(), vec![]);

        let constraints = LayoutConstraints::with_definite_size(800.0, 600.0);
        let _fragment = fc.layout(&box_node, &constraints).unwrap();
    }

    #[test]
    fn test_factory_create_flex() {
        let factory = FormattingContextFactory::new();
        let _fc = factory.create(FormattingContextType::FlexFormatting);
    }

    #[test]
    fn test_factory_create_grid() {
        let factory = FormattingContextFactory::new();
        let _fc = factory.create(FormattingContextType::GridFormatting);
    }

    #[test]
    fn test_factory_create_table() {
        let factory = FormattingContextFactory::new();
        let _fc = factory.create(FormattingContextType::TableFormatting);
    }

    #[test]
    fn test_factory_create_for_box() {
        let factory = FormattingContextFactory::new();
        let box_node = BoxNode::new_block(default_style(), FormattingContextType::BlockFormatting, vec![]);

        let fc = factory.create_for_box(&box_node);
        assert!(fc.is_some());
    }

    #[test]
    fn test_factory_create_for_text_box() {
        let factory = FormattingContextFactory::new();
        let box_node = BoxNode::new_text(default_style(), "Text".to_string());

        // Text boxes don't establish formatting contexts
        let fc = factory.create_for_box(&box_node);
        assert!(fc.is_none());
    }
}
