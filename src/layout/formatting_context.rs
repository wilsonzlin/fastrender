//! Formatting Context trait - core layout abstraction
//!
//! This module defines the FormattingContext trait, which is the fundamental
//! abstraction for all layout algorithms in FastRender. Every layout mode
//! (block, inline, flex, grid, table) implements this trait.
//!
//! # The FormattingContext Abstraction
//!
//! In CSS, different display values create different "formatting contexts" with
//! completely different layout rules:
//!
//! - **Block FC**: Boxes stack vertically, margins collapse (CSS 2.1 Section 9.4.1)
//! - **Inline FC**: Boxes flow horizontally, wrap at line boundaries (CSS 2.1 Section 9.4.2)
//! - **Flex FC**: Flexbox layout algorithm (CSS Flexbox spec)
//! - **Grid FC**: Grid layout algorithm (CSS Grid spec)
//! - **Table FC**: Table layout algorithm (CSS 2.1 Section 17)
//!
//! This trait provides a uniform interface that allows the layout engine to
//! dispatch to the appropriate algorithm without knowing implementation details.
//!
//! # Benefits of This Abstraction
//!
//! - **Modularity**: Each FC is independently implementable and testable
//! - **Extensibility**: New FCs can be added without modifying core engine
//! - **Parallelization**: Different FCs can be implemented concurrently in different tasks
//! - **Clarity**: Separates concerns - each FC focuses on one layout mode
//!
//! # References
//!
//! - CSS 2.1 Section 9.4 - Normal Flow: https://www.w3.org/TR/CSS21/visuren.html#normal-flow
//! - CSS Flexbox: https://www.w3.org/TR/css-flexbox-1/
//! - CSS Grid: https://www.w3.org/TR/css-grid-1/

use crate::layout::constraints::LayoutConstraints;
use crate::tree::box_tree::BoxNode;
use crate::tree::fragment_tree::FragmentNode;

/// Intrinsic sizing mode for content-based size queries
///
/// CSS defines two fundamental intrinsic sizes:
/// - **Min-content**: The smallest size at which content fits without overflow
/// - **Max-content**: The largest size needed to fit content without wrapping
///
/// These are used for:
/// - `width: min-content` and `width: max-content` CSS properties
/// - Flexbox intrinsic sizing (`flex-basis: content`)
/// - Grid track sizing with `min-content` and `max-content`
/// - Auto table column sizing
///
/// # Examples
///
/// For a text box containing "Hello World":
/// - **MinContent**: Width of longest word ("World") - narrowest without overflow
/// - **MaxContent**: Width of entire text ("Hello World") - widest without wrapping
///
/// # Reference
///
/// CSS Sizing Module Level 3: https://www.w3.org/TR/css-sizing-3/
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntrinsicSizingMode {
    /// Minimum content size (narrowest possible without overflow)
    ///
    /// Corresponds to CSS `min-content` keyword.
    /// For text, this is typically the width of the longest word.
    MinContent,

    /// Maximum content size (widest without line breaking)
    ///
    /// Corresponds to CSS `max-content` keyword.
    /// For text, this is the width needed to fit all content on one line.
    MaxContent,
}

/// Common trait for all formatting contexts
///
/// This trait abstracts the layout algorithm. Every formatting context
/// (block, inline, flex, grid, table) implements this trait to provide
/// a uniform interface for the layout engine.
///
/// # Contract
///
/// Implementers must:
/// 1. Take a BoxNode and LayoutConstraints as input
/// 2. Recursively layout children using appropriate child formatting contexts
/// 3. Return a positioned FragmentNode tree with final sizes and positions
/// 4. Handle all AvailableSpace variants (Definite, Indefinite, MinContent, MaxContent)
/// 5. Support intrinsic size queries (min-content, max-content)
/// 6. Be stateless and reusable (no mutable state in the FC struct)
///
/// # Thread Safety
///
/// Formatting contexts must be `Send + Sync` to allow:
/// - Sharing across threads
/// - Use in trait objects (`Box<dyn FormattingContext>`)
/// - Parallel layout operations in the future
///
/// # Example Implementation
///
/// ```rust,ignore
/// use fastrender::layout::{FormattingContext, LayoutConstraints, IntrinsicSizingMode, LayoutError};
/// use fastrender::tree::{BoxNode, FragmentNode};
///
/// struct BlockFormattingContext;
///
/// impl FormattingContext for BlockFormattingContext {
///     fn layout(&self, box_node: &BoxNode, constraints: &LayoutConstraints)
///         -> Result<FragmentNode, LayoutError>
///     {
///         // 1. Compute box's own size based on constraints
///         // 2. Layout children vertically
///         // 3. Handle margin collapsing
///         // 4. Return positioned fragment
///         todo!("Implementation in W3.T04")
///     }
///
///     fn compute_intrinsic_inline_size(
///         &self,
///         box_node: &BoxNode,
///         mode: IntrinsicSizingMode,
///     ) -> Result<f32, LayoutError> {
///         // Measure content width
///         todo!("Implementation in W3.T04")
///     }
/// }
/// ```
///
/// # See Also
///
/// - W3.T04: BlockFormattingContext implementation
/// - W4.T12: InlineFormattingContext implementation
/// - W2.T10: LayoutEngine (dispatches to FCs)
pub trait FormattingContext: Send + Sync {
    /// Lays out a box within this formatting context
    ///
    /// This is the main entry point for layout. It takes a box tree node and
    /// available space, performs layout according to this FC's rules, and
    /// returns a positioned fragment tree.
    ///
    /// # Arguments
    ///
    /// * `box_node` - The box to layout (must be compatible with this FC)
    /// * `constraints` - Available space constraints
    ///
    /// # Returns
    ///
    /// A positioned fragment tree with:
    /// - Final size (width and height)
    /// - Final position (relative to parent's content box)
    /// - Laid out children (if any)
    ///
    /// # Errors
    ///
    /// Returns error if:
    /// - Box type is not supported by this formatting context
    /// - Circular dependency detected in sizing
    /// - Required context (fonts, images, etc.) is missing
    fn layout(&self, box_node: &BoxNode, constraints: &LayoutConstraints) -> Result<FragmentNode, LayoutError>;

    /// Computes intrinsic size for a box in the inline axis
    ///
    /// Used when parent needs to know content-based size before layout.
    /// This is critical for:
    /// - Auto-sizing: `width: auto` on shrink-to-fit boxes
    /// - Flexbox: `flex-basis: content`
    /// - Grid: `grid-template-columns: min-content`
    /// - Table: Auto column width calculation
    ///
    /// # Arguments
    ///
    /// * `box_node` - The box to measure
    /// * `mode` - Whether to compute min-content or max-content size
    ///
    /// # Returns
    ///
    /// Intrinsic inline size in CSS pixels. The inline axis is:
    /// - Horizontal in horizontal writing modes (most common)
    /// - Vertical in vertical writing modes
    ///
    /// # Errors
    ///
    /// Returns error if:
    /// - Content cannot be measured (missing fonts, images, etc.)
    /// - Box type not supported
    ///
    /// # Performance Note
    ///
    /// This method may be called multiple times during layout (e.g., for table
    /// column sizing). Implementations should consider caching if expensive.
    fn compute_intrinsic_inline_size(&self, box_node: &BoxNode, mode: IntrinsicSizingMode) -> Result<f32, LayoutError>;
}

/// Layout errors
///
/// Errors that can occur during layout operations.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LayoutError {
    /// Box type not supported by this formatting context
    ///
    /// For example, trying to layout a table box with BlockFormattingContext.
    UnsupportedBoxType(String),

    /// Circular dependency in sizing
    ///
    /// Occurs when box's size depends on itself, creating infinite recursion.
    /// Example: percentage height when parent height depends on child height.
    CircularDependency,

    /// Missing required context
    ///
    /// Required resources (fonts, images, etc.) are not available.
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
    use crate::geometry::Rect;
    use crate::layout::constraints::AvailableSpace;
    use crate::style::ComputedStyle;
    use crate::style::display::FormattingContextType;
    use std::sync::Arc;

    /// Stub formatting context for testing trait requirements
    ///
    /// This demonstrates the minimal implementation needed to satisfy
    /// the FormattingContext trait contract.
    #[derive(Debug)]
    struct StubFormattingContext;

    impl FormattingContext for StubFormattingContext {
        fn layout(&self, _box_node: &BoxNode, _constraints: &LayoutConstraints) -> Result<FragmentNode, LayoutError> {
            // Stub: just return a fixed-size fragment
            Ok(FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 100.0, 50.0), vec![]))
        }

        fn compute_intrinsic_inline_size(
            &self,
            _box_node: &BoxNode,
            _mode: IntrinsicSizingMode,
        ) -> Result<f32, LayoutError> {
            // Stub: return fixed width
            Ok(100.0)
        }
    }

    #[test]
    fn test_stub_fc_implements_trait() {
        let fc = StubFormattingContext;
        let style = Arc::new(ComputedStyle::default());
        let box_node = BoxNode::new_block(style, FormattingContextType::Block, vec![]);

        let constraints = LayoutConstraints::definite(800.0, 600.0);
        let fragment = fc.layout(&box_node, &constraints).unwrap();

        assert_eq!(fragment.bounds.width(), 100.0);
        assert_eq!(fragment.bounds.height(), 50.0);
    }

    #[test]
    fn test_intrinsic_sizing_min_content() {
        let fc = StubFormattingContext;
        let style = Arc::new(ComputedStyle::default());
        let box_node = BoxNode::new_block(style, FormattingContextType::Block, vec![]);

        let size = fc
            .compute_intrinsic_inline_size(&box_node, IntrinsicSizingMode::MinContent)
            .unwrap();

        assert_eq!(size, 100.0);
    }

    #[test]
    fn test_intrinsic_sizing_max_content() {
        let fc = StubFormattingContext;
        let style = Arc::new(ComputedStyle::default());
        let box_node = BoxNode::new_block(style, FormattingContextType::Block, vec![]);

        let size = fc
            .compute_intrinsic_inline_size(&box_node, IntrinsicSizingMode::MaxContent)
            .unwrap();

        assert_eq!(size, 100.0);
    }

    #[test]
    fn test_layout_with_definite_constraints() {
        let fc = StubFormattingContext;
        let style = Arc::new(ComputedStyle::default());
        let box_node = BoxNode::new_block(style, FormattingContextType::Block, vec![]);

        let constraints = LayoutConstraints::definite(1024.0, 768.0);
        let fragment = fc.layout(&box_node, &constraints).unwrap();

        // Fragment should be positioned at origin
        assert_eq!(fragment.bounds.x(), 0.0);
        assert_eq!(fragment.bounds.y(), 0.0);
    }

    #[test]
    fn test_layout_with_indefinite_constraints() {
        let fc = StubFormattingContext;
        let style = Arc::new(ComputedStyle::default());
        let box_node = BoxNode::new_block(style, FormattingContextType::Block, vec![]);

        let constraints = LayoutConstraints::indefinite();
        let fragment = fc.layout(&box_node, &constraints).unwrap();

        // Should still produce valid fragment
        assert!(fragment.bounds.width() > 0.0);
    }

    #[test]
    fn test_layout_error_display() {
        let err = LayoutError::UnsupportedBoxType("test".to_string());
        assert_eq!(err.to_string(), "Unsupported box type: test");

        let err = LayoutError::CircularDependency;
        assert_eq!(err.to_string(), "Circular dependency in layout");

        let err = LayoutError::MissingContext("fonts".to_string());
        assert_eq!(err.to_string(), "Missing context: fonts");
    }

    #[test]
    fn test_intrinsic_sizing_mode_equality() {
        assert_eq!(IntrinsicSizingMode::MinContent, IntrinsicSizingMode::MinContent);
        assert_eq!(IntrinsicSizingMode::MaxContent, IntrinsicSizingMode::MaxContent);
        assert_ne!(IntrinsicSizingMode::MinContent, IntrinsicSizingMode::MaxContent);
    }

    #[test]
    fn test_formatting_context_is_send_sync() {
        // Verify that our trait requires Send + Sync
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<StubFormattingContext>();
    }
}
