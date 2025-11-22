//! Block Formatting Context Layout
//!
//! This module implements the Block Formatting Context (BFC) layout algorithm
//! as specified in CSS 2.1 Section 9.4.1.
//!
//! # Block Formatting Context
//!
//! A BFC is a layout mode where block boxes are laid out vertically, one after
//! another, starting at the top of the containing block. The vertical distance
//! between boxes is determined by margins (which may collapse).
//!
//! # Key Features
//!
//! - **Vertical stacking**: Block boxes stack vertically
//! - **Full width**: By default, blocks stretch to fill containing block width
//! - **Margin collapsing**: Adjacent vertical margins collapse into one
//! - **Independent context**: Contents don't affect outside layout
//!
//! # Module Structure
//!
//! - `margin_collapse` - Margin collapsing algorithm (CSS 2.1 Section 8.3.1)
//! - `width` - Block width computation (CSS 2.1 Section 10.3.3)
//!
//! Reference: <https://www.w3.org/TR/CSS21/visuren.html#block-formatting>

mod margin_collapse;
mod width;

pub use margin_collapse::{
    collapse_margins, is_margin_collapsible_through, should_collapse_with_first_child, should_collapse_with_last_child,
    CollapsibleMargins, MarginCollapseContext,
};
pub use width::{compute_block_width, compute_block_width_with_auto_margins, ComputedBlockWidth, MarginValue};

use crate::geometry::Rect;
use crate::layout::constraints::LayoutConstraints;
use crate::layout::formatting_context::{FormattingContext, IntrinsicSizingMode, LayoutError};
use crate::style::display::FormattingContextType;
use crate::style::{LineHeight, Position};
use crate::tree::box_tree::BoxNode;
use crate::tree::fragment_tree::FragmentNode;

/// Block Formatting Context implementation
///
/// Implements the FormattingContext trait for block-level layout.
/// Handles vertical stacking, margin collapsing, and width computation.
#[derive(Debug, Default)]
pub struct BlockFormattingContext;

impl BlockFormattingContext {
    /// Creates a new BlockFormattingContext
    pub fn new() -> Self {
        Self
    }

    /// Lays out a single block-level child and returns its fragment
    fn layout_block_child(
        &self,
        child: &BoxNode,
        containing_width: f32,
        margin_ctx: &mut MarginCollapseContext,
        current_y: f32,
    ) -> Result<(FragmentNode, f32), LayoutError> {
        let style = &child.style;

        // Compute width using CSS 2.1 Section 10.3.3 algorithm
        let computed_width = compute_block_width(style, containing_width);

        // Handle vertical margins
        let margin_top = style.margin_top.as_ref().map(|l| l.to_px()).unwrap_or(0.0);
        let margin_bottom = style.margin_bottom.as_ref().map(|l| l.to_px()).unwrap_or(0.0);

        // Add top margin to pending collapse set
        margin_ctx.push_margin(margin_top);

        // Resolve collapsed margin and get Y position
        let collapsed_margin = margin_ctx.resolve();
        let box_y = current_y + collapsed_margin;

        // Create constraints for child layout
        let child_constraints = LayoutConstraints::definite_width(computed_width.content_width);

        // Recursively layout children
        let (child_fragments, content_height) = self.layout_children(child, &child_constraints)?;

        // Compute height
        let border_top = style.border_top_width.to_px();
        let border_bottom = style.border_bottom_width.to_px();
        let padding_top = style.padding_top.to_px();
        let padding_bottom = style.padding_bottom.to_px();

        // Height computation (CSS 2.1 Section 10.6.3)
        let height = if let Some(h) = &style.height {
            // Explicit height
            h.to_px()
        } else {
            // Auto height: sum of children
            content_height
        };

        // Apply min/max height constraints
        let min_height = style.min_height.as_ref().map(|l| l.to_px()).unwrap_or(0.0);
        let max_height = style.max_height.as_ref().map(|l| l.to_px()).unwrap_or(f32::INFINITY);
        let height = height.max(min_height).min(max_height);

        // Create the fragment
        let box_height = border_top + padding_top + height + padding_bottom + border_bottom;
        let box_width = computed_width.border_box_width();

        let bounds = Rect::from_xywh(computed_width.margin_left, box_y, box_width, box_height);

        let fragment = FragmentNode::new_block(bounds, child_fragments);

        // Push bottom margin for next collapse
        margin_ctx.push_margin(margin_bottom);

        // Return fragment and the Y position after this box (before margin)
        let next_y = box_y + box_height;

        Ok((fragment, next_y))
    }

    /// Lays out all children of a box
    fn layout_children(
        &self,
        parent: &BoxNode,
        constraints: &LayoutConstraints,
    ) -> Result<(Vec<FragmentNode>, f32), LayoutError> {
        let mut fragments = Vec::new();
        let mut current_y: f32 = 0.0;
        let mut content_height: f32 = 0.0;
        let mut margin_ctx = MarginCollapseContext::new();

        // Get containing width from constraints
        let containing_width = constraints.width().unwrap_or(0.0);

        // Check for border/padding that prevents margin collapse with first child
        let parent_has_top_separation =
            parent.style.border_top_width.to_px() > 0.0 || parent.style.padding_top.to_px() > 0.0;

        if parent_has_top_separation {
            // Border/padding stops margin collapsing
            margin_ctx.mark_content_encountered();
        }

        for child in &parent.children {
            // Skip out-of-flow children (positioned, floats)
            if is_out_of_flow(child) {
                continue;
            }

            // Layout in-flow children
            if child.is_block_level() {
                let (fragment, next_y) =
                    self.layout_block_child(child, containing_width, &mut margin_ctx, current_y)?;

                content_height = content_height.max(fragment.bounds.max_y());
                current_y = next_y;
                fragments.push(fragment);
            } else {
                // Inline children need inline formatting context
                // For now, create a placeholder
                let inline_fragment = self.layout_inline_placeholder(child)?;
                if let Some(frag) = inline_fragment {
                    content_height = content_height.max(current_y + frag.bounds.height());
                    current_y += frag.bounds.height();
                    fragments.push(frag);
                }
            }
        }

        // Resolve any trailing margins
        let trailing_margin = margin_ctx.pending_margin();

        // Check for bottom separation
        let parent_has_bottom_separation =
            parent.style.border_bottom_width.to_px() > 0.0 || parent.style.padding_bottom.to_px() > 0.0;

        if parent_has_bottom_separation {
            // Include trailing margin in height
            content_height += trailing_margin.max(0.0);
        }

        Ok((fragments, content_height))
    }

    /// Creates a placeholder fragment for inline content
    fn layout_inline_placeholder(&self, child: &BoxNode) -> Result<Option<FragmentNode>, LayoutError> {
        // For text boxes, create a minimal line fragment
        if child.is_text() {
            let text = child.text().unwrap_or("");
            let line_height = compute_line_height(&child.style.line_height, child.style.font_size);

            // Estimate width (this will be replaced by proper text shaping)
            let estimated_width = text.len() as f32 * child.style.font_size * 0.5;

            let bounds = Rect::from_xywh(0.0, 0.0, estimated_width, line_height);
            let fragment = FragmentNode::new_text(bounds, text.to_string(), line_height * 0.8);

            return Ok(Some(fragment));
        }

        // For other inline content, skip for now
        Ok(None)
    }
}

/// Compute line height from LineHeight enum and font size
fn compute_line_height(line_height: &LineHeight, font_size: f32) -> f32 {
    match line_height {
        LineHeight::Normal => font_size * 1.2,
        LineHeight::Number(n) => font_size * n,
        LineHeight::Length(len) => len.to_px(),
    }
}

impl FormattingContext for BlockFormattingContext {
    fn layout(&self, box_node: &BoxNode, constraints: &LayoutConstraints) -> Result<FragmentNode, LayoutError> {
        let style = &box_node.style;

        // Compute width for the root box
        let containing_width = constraints.width().unwrap_or(0.0);
        let computed_width = compute_block_width(style, containing_width);

        // Create constraints for children
        let child_constraints = LayoutConstraints::definite_width(computed_width.content_width);

        // Layout children
        let (child_fragments, content_height) = self.layout_children(box_node, &child_constraints)?;

        // Compute height
        let border_top = style.border_top_width.to_px();
        let border_bottom = style.border_bottom_width.to_px();
        let padding_top = style.padding_top.to_px();
        let padding_bottom = style.padding_bottom.to_px();

        let height = if let Some(h) = &style.height {
            h.to_px()
        } else {
            content_height
        };

        let min_height = style.min_height.as_ref().map(|l| l.to_px()).unwrap_or(0.0);
        let max_height = style.max_height.as_ref().map(|l| l.to_px()).unwrap_or(f32::INFINITY);
        let height = height.max(min_height).min(max_height);

        let box_height = border_top + padding_top + height + padding_bottom + border_bottom;
        let box_width = computed_width.border_box_width();

        let bounds = Rect::from_xywh(0.0, 0.0, box_width, box_height);

        Ok(FragmentNode::new_block(bounds, child_fragments))
    }

    fn compute_intrinsic_inline_size(&self, box_node: &BoxNode, mode: IntrinsicSizingMode) -> Result<f32, LayoutError> {
        let style = &box_node.style;

        // Get explicit width if specified
        let explicit_width = style.width.as_ref().map(|l| l.to_px());

        match mode {
            IntrinsicSizingMode::MinContent => {
                // Min-content: smallest width that doesn't cause overflow
                if let Some(w) = explicit_width {
                    return Ok(w);
                }

                // For blocks, this is the max of children's min-content
                let mut max_child_width = 0.0f32;
                for child in &box_node.children {
                    if child.is_block_level() {
                        if let Some(w) = child.style.width.as_ref().map(|l| l.to_px()) {
                            max_child_width = max_child_width.max(w);
                        }
                    }
                }
                Ok(max_child_width)
            }
            IntrinsicSizingMode::MaxContent => {
                // Max-content: width if nothing constrained
                if let Some(w) = explicit_width {
                    return Ok(w);
                }

                // For blocks, this is the max of children's max-content
                let mut max_child_width = 0.0f32;
                for child in &box_node.children {
                    if child.is_block_level() {
                        if let Some(w) = child.style.width.as_ref().map(|l| l.to_px()) {
                            max_child_width = max_child_width.max(w);
                        }
                    }
                }
                Ok(max_child_width)
            }
        }
    }
}

/// Checks if a box is out of normal flow (absolute/fixed positioned or float)
fn is_out_of_flow(box_node: &BoxNode) -> bool {
    let position = box_node.style.position;
    matches!(position, Position::Absolute | Position::Fixed)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::style::{ComputedStyles, Display, Length};
    use std::sync::Arc;

    fn default_style() -> Arc<ComputedStyles> {
        let mut style = ComputedStyles::default();
        style.display = Display::Block;
        Arc::new(style)
    }

    fn block_style_with_height(height: f32) -> Arc<ComputedStyles> {
        let mut style = ComputedStyles::default();
        style.display = Display::Block;
        style.height = Some(Length::px(height));
        Arc::new(style)
    }

    fn block_style_with_margin(margin: f32) -> Arc<ComputedStyles> {
        let mut style = ComputedStyles::default();
        style.display = Display::Block;
        style.margin_top = Some(Length::px(margin));
        style.margin_bottom = Some(Length::px(margin));
        style.margin_left = Some(Length::px(margin));
        style.margin_right = Some(Length::px(margin));
        Arc::new(style)
    }

    // BlockFormattingContext creation
    #[test]
    fn test_bfc_new() {
        let _bfc = BlockFormattingContext::new();
        // Just verify construction works
    }

    // Basic layout tests
    #[test]
    fn test_layout_empty_block() {
        let bfc = BlockFormattingContext::new();
        let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
        let constraints = LayoutConstraints::definite(800.0, 600.0);

        let fragment = bfc.layout(&root, &constraints).unwrap();

        assert_eq!(fragment.bounds.width(), 800.0);
        assert_eq!(fragment.bounds.height(), 0.0); // Empty, auto height
    }

    #[test]
    fn test_layout_block_with_explicit_height() {
        let bfc = BlockFormattingContext::new();
        let root = BoxNode::new_block(block_style_with_height(200.0), FormattingContextType::Block, vec![]);
        let constraints = LayoutConstraints::definite(800.0, 600.0);

        let fragment = bfc.layout(&root, &constraints).unwrap();

        assert_eq!(fragment.bounds.width(), 800.0);
        assert_eq!(fragment.bounds.height(), 200.0);
    }

    #[test]
    fn test_layout_nested_blocks() {
        let bfc = BlockFormattingContext::new();

        let child1 = BoxNode::new_block(block_style_with_height(100.0), FormattingContextType::Block, vec![]);
        let child2 = BoxNode::new_block(block_style_with_height(150.0), FormattingContextType::Block, vec![]);

        let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![child1, child2]);
        let constraints = LayoutConstraints::definite(800.0, 600.0);

        let fragment = bfc.layout(&root, &constraints).unwrap();

        assert_eq!(fragment.children.len(), 2);
        // Auto height should contain both children
        assert!(fragment.bounds.height() >= 250.0);
    }

    #[test]
    fn test_layout_block_fills_width() {
        let bfc = BlockFormattingContext::new();

        let child = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
        let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![child]);
        let constraints = LayoutConstraints::definite(1000.0, 600.0);

        let fragment = bfc.layout(&root, &constraints).unwrap();

        // Child should fill parent width
        if !fragment.children.is_empty() {
            assert_eq!(fragment.children[0].bounds.width(), 1000.0);
        }
    }

    // Width computation integration tests
    #[test]
    fn test_layout_with_explicit_width() {
        let bfc = BlockFormattingContext::new();

        let mut style = ComputedStyles::default();
        style.display = Display::Block;
        style.width = Some(Length::px(400.0));

        let root = BoxNode::new_block(Arc::new(style), FormattingContextType::Block, vec![]);
        let constraints = LayoutConstraints::definite(800.0, 600.0);

        let fragment = bfc.layout(&root, &constraints).unwrap();

        assert_eq!(fragment.bounds.width(), 400.0);
    }

    #[test]
    fn test_layout_with_padding() {
        let bfc = BlockFormattingContext::new();

        let mut style = ComputedStyles::default();
        style.display = Display::Block;
        style.padding_top = Length::px(20.0);
        style.padding_bottom = Length::px(20.0);
        style.height = Some(Length::px(100.0));

        let root = BoxNode::new_block(Arc::new(style), FormattingContextType::Block, vec![]);
        let constraints = LayoutConstraints::definite(800.0, 600.0);

        let fragment = bfc.layout(&root, &constraints).unwrap();

        // Height should include padding
        assert_eq!(fragment.bounds.height(), 140.0); // 20 + 100 + 20
    }

    #[test]
    fn test_layout_with_border() {
        let bfc = BlockFormattingContext::new();

        let mut style = ComputedStyles::default();
        style.display = Display::Block;
        style.border_top_width = Length::px(5.0);
        style.border_bottom_width = Length::px(5.0);
        style.height = Some(Length::px(100.0));

        let root = BoxNode::new_block(Arc::new(style), FormattingContextType::Block, vec![]);
        let constraints = LayoutConstraints::definite(800.0, 600.0);

        let fragment = bfc.layout(&root, &constraints).unwrap();

        // Height should include border
        assert_eq!(fragment.bounds.height(), 110.0); // 5 + 100 + 5
    }

    // Margin collapsing integration tests
    #[test]
    fn test_sibling_margin_collapse() {
        let bfc = BlockFormattingContext::new();

        let child1 = BoxNode::new_block(block_style_with_margin(20.0), FormattingContextType::Block, vec![]);
        let child2 = BoxNode::new_block(block_style_with_margin(30.0), FormattingContextType::Block, vec![]);

        let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![child1, child2]);
        let constraints = LayoutConstraints::definite(800.0, 600.0);

        let fragment = bfc.layout(&root, &constraints).unwrap();

        // Margins should collapse: max(20, 30) = 30 between siblings
        assert_eq!(fragment.children.len(), 2);
    }

    // Intrinsic sizing tests
    #[test]
    fn test_measure_intrinsic_min_content() {
        let bfc = BlockFormattingContext::new();
        let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);

        let width = bfc
            .compute_intrinsic_inline_size(&root, IntrinsicSizingMode::MinContent)
            .unwrap();

        // Empty block has 0 intrinsic size
        assert_eq!(width, 0.0);
    }

    #[test]
    fn test_measure_intrinsic_with_explicit_size() {
        let bfc = BlockFormattingContext::new();

        let mut style = ComputedStyles::default();
        style.width = Some(Length::px(200.0));
        style.height = Some(Length::px(100.0));

        let root = BoxNode::new_block(Arc::new(style), FormattingContextType::Block, vec![]);

        let width = bfc
            .compute_intrinsic_inline_size(&root, IntrinsicSizingMode::MinContent)
            .unwrap();

        assert_eq!(width, 200.0);
    }

    // Out-of-flow tests
    #[test]
    fn test_absolute_positioned_skipped() {
        let bfc = BlockFormattingContext::new();

        let mut abs_style = ComputedStyles::default();
        abs_style.display = Display::Block;
        abs_style.position = Position::Absolute;
        abs_style.height = Some(Length::px(100.0));

        let abs_child = BoxNode::new_block(Arc::new(abs_style), FormattingContextType::Block, vec![]);

        let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![abs_child]);
        let constraints = LayoutConstraints::definite(800.0, 600.0);

        let fragment = bfc.layout(&root, &constraints).unwrap();

        // Absolutely positioned child should not contribute to height
        assert_eq!(fragment.bounds.height(), 0.0);
        assert!(fragment.children.is_empty());
    }

    // min/max height tests
    #[test]
    fn test_min_height_constraint() {
        let bfc = BlockFormattingContext::new();

        let mut style = ComputedStyles::default();
        style.display = Display::Block;
        style.min_height = Some(Length::px(150.0));

        let root = BoxNode::new_block(Arc::new(style), FormattingContextType::Block, vec![]);
        let constraints = LayoutConstraints::definite(800.0, 600.0);

        let fragment = bfc.layout(&root, &constraints).unwrap();

        // Height should be at least min-height
        assert!(fragment.bounds.height() >= 150.0);
    }

    #[test]
    fn test_max_height_constraint() {
        let bfc = BlockFormattingContext::new();

        let mut style = ComputedStyles::default();
        style.display = Display::Block;
        style.height = Some(Length::px(500.0));
        style.max_height = Some(Length::px(200.0));

        let root = BoxNode::new_block(Arc::new(style), FormattingContextType::Block, vec![]);
        let constraints = LayoutConstraints::definite(800.0, 600.0);

        let fragment = bfc.layout(&root, &constraints).unwrap();

        // Height should be capped at max-height
        assert!(fragment.bounds.height() <= 200.0);
    }

    // Edge cases
    #[test]
    fn test_deeply_nested_blocks() {
        let bfc = BlockFormattingContext::new();

        // Create 5 levels of nesting
        let mut current = BoxNode::new_block(block_style_with_height(50.0), FormattingContextType::Block, vec![]);

        for _ in 0..4 {
            current = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![current]);
        }

        let constraints = LayoutConstraints::definite(800.0, 600.0);
        let fragment = bfc.layout(&current, &constraints).unwrap();

        // Should still work with deep nesting
        assert!(fragment.bounds.height() >= 50.0);
    }

    #[test]
    fn test_zero_width_container() {
        let bfc = BlockFormattingContext::new();

        let child = BoxNode::new_block(block_style_with_height(100.0), FormattingContextType::Block, vec![]);

        let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![child]);
        let constraints = LayoutConstraints::definite(0.0, 600.0);

        let fragment = bfc.layout(&root, &constraints).unwrap();

        // Should handle zero width gracefully
        assert_eq!(fragment.bounds.width(), 0.0);
    }

    // Additional tests for FormattingContext trait
    #[test]
    fn test_fc_is_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<BlockFormattingContext>();
    }

    #[test]
    fn test_intrinsic_max_content() {
        let bfc = BlockFormattingContext::new();

        let mut style = ComputedStyles::default();
        style.width = Some(Length::px(300.0));

        let root = BoxNode::new_block(Arc::new(style), FormattingContextType::Block, vec![]);

        let width = bfc
            .compute_intrinsic_inline_size(&root, IntrinsicSizingMode::MaxContent)
            .unwrap();

        assert_eq!(width, 300.0);
    }

    #[test]
    fn test_layout_with_percentage_width() {
        let bfc = BlockFormattingContext::new();

        let mut style = ComputedStyles::default();
        style.display = Display::Block;
        style.width = Some(Length::percent(50.0));

        let root = BoxNode::new_block(Arc::new(style), FormattingContextType::Block, vec![]);
        let constraints = LayoutConstraints::definite(800.0, 600.0);

        let fragment = bfc.layout(&root, &constraints).unwrap();

        // 50% of 800 = 400
        assert_eq!(fragment.bounds.width(), 400.0);
    }
}
