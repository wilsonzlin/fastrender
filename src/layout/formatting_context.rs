//! Formatting Context trait - core layout abstraction
//!
//! All layout algorithms implement this trait.

use crate::layout::constraints::LayoutConstraints;
use crate::tree::{BoxNode, Fragment};

/// Intrinsic sizing mode for querying content-based sizes
#[derive(Debug, Clone, Copy)]
pub enum IntrinsicSizingMode {
    /// Minimum content size (narrowest possible without overflow)
    MinContent,

    /// Maximum content size (widest without line breaking)
    MaxContent,
}

/// Common trait for all formatting contexts
///
/// This trait abstracts the layout algorithm. Every formatting context
/// (block, inline, flex, grid, table) implements this trait.
///
/// # Contract
///
/// Implementers must:
/// 1. Take a BoxNode and LayoutConstraints
/// 2. Recursively layout children using appropriate child FCs
/// 3. Return a positioned Fragment tree
/// 4. Handle both definite and indefinite sizing
/// 5. Support intrinsic size queries
///
/// # Example Implementation
///
/// See W3.T04 (BlockFormattingContext) for a complete implementation.
pub trait FormattingContext: Send + Sync {
    /// Lays out a box within this formatting context
    ///
    /// # Arguments
    /// * `box_node` - The box to layout
    /// * `constraints` - Available space and percentage bases
    ///
    /// # Returns
    /// A positioned fragment tree
    ///
    /// # Errors
    /// Returns error if layout cannot proceed (e.g., circular dependencies)
    fn layout(&self, box_node: &BoxNode, constraints: &LayoutConstraints) -> Result<Fragment, LayoutError>;

    /// Computes intrinsic size for a box
    ///
    /// Used when parent needs to know content-based size before layout.
    ///
    /// # Arguments
    /// * `box_node` - The box to measure
    /// * `mode` - MinContent or MaxContent
    ///
    /// # Returns
    /// Intrinsic size in the inline axis
    fn compute_intrinsic_inline_size(&self, box_node: &BoxNode, mode: IntrinsicSizingMode) -> Result<f32, LayoutError>;
}

/// Layout errors
#[derive(Debug)]
pub enum LayoutError {
    /// Box type not supported by this FC
    UnsupportedBoxType(String),

    /// Circular dependency in sizing
    CircularDependency,

    /// Missing required context
    MissingContext(String),
}

impl std::fmt::Display for LayoutError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnsupportedBoxType(msg) => write!(f, "Unsupported box type: {}", msg),
            Self::CircularDependency => write!(f, "Circular dependency in layout"),
            Self::MissingContext(msg) => write!(f, "Missing context: {}", msg),
        }
    }
}

impl std::error::Error for LayoutError {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::geometry::{Point, Rect, Size};
    use crate::tree::FormattingContextType;
    use std::sync::Arc;

    // Stub FC for testing trait requirements
    struct StubFormattingContext;

    impl FormattingContext for StubFormattingContext {
        fn layout(&self, box_node: &BoxNode, constraints: &LayoutConstraints) -> Result<Fragment, LayoutError> {
            // Stub: just return empty fragment
            let width = constraints.available_width.definite_value().unwrap_or(100.0);
            let height = constraints.available_height.definite_value().unwrap_or(50.0);

            Ok(Fragment::new_block(
                Rect::from_xywh(0.0, 0.0, width, height),
                box_node.style.clone(),
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

    fn default_style() -> Arc<crate::tree::box_tree::ComputedStyle> {
        Arc::new(crate::tree::box_tree::ComputedStyle::default())
    }

    #[test]
    fn test_stub_fc_implements_trait() {
        let fc = StubFormattingContext;
        let box_node = BoxNode::new_block(default_style(), FormattingContextType::BlockFormatting, vec![]);

        let constraints = LayoutConstraints::with_definite_size(800.0, 600.0);
        let fragment = fc.layout(&box_node, &constraints).unwrap();

        assert_eq!(fragment.bounds().width(), 800.0);
    }

    #[test]
    fn test_intrinsic_sizing() {
        let fc = StubFormattingContext;
        let box_node = BoxNode::new_block(default_style(), FormattingContextType::BlockFormatting, vec![]);

        let min_size = fc
            .compute_intrinsic_inline_size(&box_node, IntrinsicSizingMode::MinContent)
            .unwrap();

        assert_eq!(min_size, 100.0);
    }

    #[test]
    fn test_intrinsic_sizing_max_content() {
        let fc = StubFormattingContext;
        let box_node = BoxNode::new_block(default_style(), FormattingContextType::BlockFormatting, vec![]);

        let max_size = fc
            .compute_intrinsic_inline_size(&box_node, IntrinsicSizingMode::MaxContent)
            .unwrap();

        assert_eq!(max_size, 100.0);
    }
}
