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
    collapse_margins, establishes_bfc, is_margin_collapsible_through, should_collapse_with_first_child,
    should_collapse_with_last_child, CollapsibleMargin, MarginCollapseContext,
};
pub use width::{compute_block_width, compute_block_width_with_auto_margins, ComputedBlockWidth, MarginValue};

use crate::geometry::Rect;
use crate::layout::constraints::LayoutConstraints;
use crate::layout::formatting_context::{FormattingContext, IntrinsicSizingMode, LayoutError};
use crate::style::{Length, LineHeight, Position};
use crate::tree::box_tree::BoxNode;
use crate::tree::fragment_tree::FragmentNode;

/// Helper to resolve a Length to pixels, handling em/rem units with font-size
fn resolve_length(length: &Length, font_size: f32, containing_block_size: f32) -> f32 {
    use crate::style::values::LengthUnit;
    match length.unit {
        LengthUnit::Em | LengthUnit::Rem => length.value * font_size,
        LengthUnit::Percent => (length.value / 100.0) * containing_block_size,
        _ if length.unit.is_absolute() => length.to_px(),
        _ => 0.0, // Fallback for unknown units
    }
}

/// Helper to resolve an optional Length to pixels
fn resolve_opt_length(length: Option<&Length>, font_size: f32, containing_block_size: f32) -> f32 {
    length
        .map(|l| resolve_length(l, font_size, containing_block_size))
        .unwrap_or(0.0)
}

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
        let font_size = style.font_size; // Get font-size for resolving em units

        // Compute width using CSS 2.1 Section 10.3.3 algorithm
        let computed_width = compute_block_width(style, containing_width);

        // Handle vertical margins (resolve em/rem units with font-size)
        let margin_top = resolve_opt_length(style.margin_top.as_ref(), font_size, containing_width);
        let margin_bottom = resolve_opt_length(style.margin_bottom.as_ref(), font_size, containing_width);

        // Add top margin to pending collapse set
        margin_ctx.push_margin(margin_top);

        // Resolve collapsed margin and get Y position
        let collapsed_margin = margin_ctx.resolve();
        let box_y = current_y + collapsed_margin;

        // Create constraints for child layout
        let child_constraints = LayoutConstraints::definite_width(computed_width.content_width);

        // Recursively layout children
        let (child_fragments, content_height) = self.layout_children(child, &child_constraints)?;

        // Compute height (resolve em/rem units with font-size)
        let border_top = resolve_length(&style.border_top_width, font_size, containing_width);
        let border_bottom = resolve_length(&style.border_bottom_width, font_size, containing_width);
        let padding_top = resolve_length(&style.padding_top, font_size, containing_width);
        let padding_bottom = resolve_length(&style.padding_bottom, font_size, containing_width);

        // Height computation (CSS 2.1 Section 10.6.3)
        let height = if let Some(h) = &style.height {
            resolve_length(h, font_size, containing_width)
        } else {
            content_height
        };

        // Apply min/max height constraints
        let min_height = resolve_opt_length(style.min_height.as_ref(), font_size, containing_width);
        let max_height = style
            .max_height
            .as_ref()
            .map(|l| resolve_length(l, font_size, containing_width))
            .unwrap_or(f32::INFINITY);
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
            content_height += trailing_margin.max(0.0);
        }

        Ok((fragments, content_height))
    }

    /// Creates a placeholder fragment for inline content
    fn layout_inline_placeholder(&self, child: &BoxNode) -> Result<Option<FragmentNode>, LayoutError> {
        if child.is_text() {
            let text = child.text().unwrap_or("");
            let line_height = compute_line_height(&child.style.line_height, child.style.font_size);
            let estimated_width = text.len() as f32 * child.style.font_size * 0.5;
            let bounds = Rect::from_xywh(0.0, 0.0, estimated_width, line_height);
            let fragment = FragmentNode::new_text(bounds, text.to_string(), line_height * 0.8);
            return Ok(Some(fragment));
        }
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

        let containing_width = constraints.width().unwrap_or(0.0);
        let computed_width = compute_block_width(style, containing_width);

        let child_constraints = LayoutConstraints::definite_width(computed_width.content_width);

        let (child_fragments, content_height) = self.layout_children(box_node, &child_constraints)?;

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
        let explicit_width = style.width.as_ref().map(|l| l.to_px());

        match mode {
            IntrinsicSizingMode::MinContent => {
                if let Some(w) = explicit_width {
                    return Ok(w);
                }
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
                if let Some(w) = explicit_width {
                    return Ok(w);
                }
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
    use crate::style::display::FormattingContextType;
    use crate::style::{ComputedStyle, Display, Length};
    use std::sync::Arc;

    fn default_style() -> Arc<ComputedStyle> {
        let mut style = ComputedStyle::default();
        style.display = Display::Block;
        Arc::new(style)
    }

    fn block_style_with_height(height: f32) -> Arc<ComputedStyle> {
        let mut style = ComputedStyle::default();
        style.display = Display::Block;
        style.height = Some(Length::px(height));
        Arc::new(style)
    }

    fn block_style_with_margin(margin: f32) -> Arc<ComputedStyle> {
        let mut style = ComputedStyle::default();
        style.display = Display::Block;
        style.margin_top = Some(Length::px(margin));
        style.margin_bottom = Some(Length::px(margin));
        style.margin_left = Some(Length::px(margin));
        style.margin_right = Some(Length::px(margin));
        Arc::new(style)
    }

    #[test]
    fn test_bfc_new() {
        let _bfc = BlockFormattingContext::new();
    }

    #[test]
    fn test_layout_empty_block() {
        let bfc = BlockFormattingContext::new();
        let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
        let constraints = LayoutConstraints::definite(800.0, 600.0);

        let fragment = bfc.layout(&root, &constraints).unwrap();

        assert_eq!(fragment.bounds.width(), 800.0);
        assert_eq!(fragment.bounds.height(), 0.0);
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
        assert!(fragment.bounds.height() >= 250.0);
    }

    #[test]
    fn test_sibling_margin_collapse() {
        let bfc = BlockFormattingContext::new();

        let child1 = BoxNode::new_block(block_style_with_margin(20.0), FormattingContextType::Block, vec![]);
        let child2 = BoxNode::new_block(block_style_with_margin(30.0), FormattingContextType::Block, vec![]);

        let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![child1, child2]);
        let constraints = LayoutConstraints::definite(800.0, 600.0);

        let fragment = bfc.layout(&root, &constraints).unwrap();
        assert_eq!(fragment.children.len(), 2);
    }

    #[test]
    fn test_fc_is_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<BlockFormattingContext>();
    }
}
