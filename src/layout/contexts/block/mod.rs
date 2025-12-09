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

pub mod margin_collapse;
pub mod width;

use crate::geometry::Rect;
use crate::layout::constraints::LayoutConstraints;
use crate::layout::contexts::block::width::MarginValue;
use crate::layout::contexts::factory::FormattingContextFactory;
use crate::layout::contexts::inline::InlineFormattingContext;
use crate::layout::formatting_context::{FormattingContext, IntrinsicSizingMode, LayoutError};
use crate::layout::utils::compute_replaced_size;
use crate::style::display::FormattingContextType;
use crate::style::position::Position;
use crate::style::values::Length;
use crate::style::ComputedStyle;
use crate::text::font_loader::FontContext;
use crate::tree::box_tree::{BoxNode, BoxType, ReplacedBox};
use crate::tree::fragment_tree::{FragmentContent, FragmentNode};

use margin_collapse::MarginCollapseContext;
use width::compute_block_width;

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
#[derive(Clone)]
pub struct BlockFormattingContext {
    font_context: FontContext,
}

impl BlockFormattingContext {
    /// Creates a new BlockFormattingContext
    pub fn new() -> Self {
        Self {
            font_context: FontContext::new(),
        }
    }

    /// Lays out a single block-level child and returns its fragment
    fn layout_block_child(
        &self,
        child: &BoxNode,
        containing_width: f32,
        margin_ctx: &mut MarginCollapseContext,
        current_y: f32,
    ) -> Result<(FragmentNode, f32), LayoutError> {
        if let BoxType::Replaced(replaced_box) = &child.box_type {
            return self.layout_replaced_child(child, replaced_box, containing_width, margin_ctx, current_y);
        }

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

        // Check if this child establishes a different formatting context
        let fc_type = child.formatting_context();

        let (child_fragments, content_height) = if let Some(fc_type) = fc_type {
            if fc_type != FormattingContextType::Block {
                // Child establishes a non-block FC - use the appropriate FC
                let factory = FormattingContextFactory::new();
                let fc = factory.create(fc_type);

                // Layout using the child's FC
                let child_frag = fc.layout(child, &child_constraints)?;
                let height = child_frag.bounds.height();

                // Return fragment wrapped in vec and height
                (vec![child_frag], height)
            } else {
                // Block FC - layout children normally
                self.layout_children(child, &child_constraints)?
            }
        } else {
            // No FC (inline, text, etc.) - layout children normally
            self.layout_children(child, &child_constraints)?
        };

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

        let fragment = FragmentNode::new_block_styled(bounds, child_fragments, child.style.clone());

        // Push bottom margin for next collapse
        margin_ctx.push_margin(margin_bottom);

        // Return fragment and the Y position after this box (before margin)
        let next_y = box_y + box_height;

        Ok((fragment, next_y))
    }

    fn layout_replaced_child(
        &self,
        child: &BoxNode,
        replaced_box: &ReplacedBox,
        containing_width: f32,
        margin_ctx: &mut MarginCollapseContext,
        current_y: f32,
    ) -> Result<(FragmentNode, f32), LayoutError> {
        let style = &child.style;
        let font_size = style.font_size;

        let used_size = compute_replaced_size(style, replaced_box);

        // Vertical margins collapse as normal blocks
        let margin_top = resolve_opt_length(style.margin_top.as_ref(), font_size, containing_width);
        let margin_bottom = resolve_opt_length(style.margin_bottom.as_ref(), font_size, containing_width);
        margin_ctx.push_margin(margin_top);
        let collapsed_margin = margin_ctx.resolve();
        let box_y = current_y + collapsed_margin;

        // Resolve padding and borders
        let padding_top = resolve_length(&style.padding_top, font_size, containing_width);
        let padding_bottom = resolve_length(&style.padding_bottom, font_size, containing_width);

        let border_top = resolve_length(&style.border_top_width, font_size, containing_width);
        let border_bottom = resolve_length(&style.border_bottom_width, font_size, containing_width);

        // Use the resolved replaced width when computing horizontal metrics
        let mut width_style = (*style).as_ref().clone();
        width_style.width = Some(Length::px(used_size.width));
        let computed_width = compute_block_width(&width_style, containing_width);

        let box_width = computed_width.border_box_width();
        let box_height = border_top + padding_top + used_size.height + padding_bottom + border_bottom;
        let bounds = Rect::from_xywh(computed_width.margin_left, box_y, box_width, box_height);

        let fragment = FragmentNode::new_with_style(
            bounds,
            FragmentContent::Replaced {
                replaced_type: replaced_box.replaced_type.clone(),
                box_id: None,
            },
            vec![],
            child.style.clone(),
        );

        margin_ctx.push_margin(margin_bottom);
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
        let mut inline_buffer: Vec<BoxNode> = Vec::new();

        // Get containing width from constraints
        let containing_width = constraints.width().unwrap_or(0.0);

        // Check for border/padding that prevents margin collapse with first child
        let parent_has_top_separation =
            parent.style.border_top_width.to_px() > 0.0 || parent.style.padding_top.to_px() > 0.0;

        if parent_has_top_separation {
            margin_ctx.mark_content_encountered();
        }

        let flush_inline_buffer = |buffer: &mut Vec<BoxNode>,
                                   fragments: &mut Vec<FragmentNode>,
                                   current_y: &mut f32,
                                   content_height: &mut f32,
                                   margin_ctx: &mut MarginCollapseContext|
         -> Result<(), LayoutError> {
            if buffer.is_empty() {
                return Ok(());
            }

            // Apply any pending collapsed margin before inline content
            let pending_margin = margin_ctx.consume_pending();
            *current_y += pending_margin;

            let inline_container = BoxNode::new_inline(parent.style.clone(), buffer.clone());
            let inline_fc = InlineFormattingContext::with_font_context(self.font_context.clone());
            let inline_constraints = LayoutConstraints::definite_width(containing_width);
            let mut inline_fragment = inline_fc.layout(&inline_container, &inline_constraints)?;

            inline_fragment.bounds = Rect::from_xywh(
                0.0,
                *current_y,
                inline_fragment.bounds.width(),
                inline_fragment.bounds.height(),
            );

            *content_height = content_height.max(inline_fragment.bounds.max_y());
            *current_y += inline_fragment.bounds.height();
            fragments.push(inline_fragment);
            buffer.clear();
            Ok(())
        };

        for child in &parent.children {
            // Skip out-of-flow children (positioned, floats)
            if is_out_of_flow(child) {
                continue;
            }

            // Layout in-flow children
            let treated_as_block = match child.box_type {
                BoxType::Replaced(_) if child.style.display.is_inline_level() => false,
                _ => child.is_block_level(),
            };

            if treated_as_block {
                flush_inline_buffer(
                    &mut inline_buffer,
                    &mut fragments,
                    &mut current_y,
                    &mut content_height,
                    &mut margin_ctx,
                )?;

                let (fragment, next_y) =
                    self.layout_block_child(child, containing_width, &mut margin_ctx, current_y)?;

                content_height = content_height.max(fragment.bounds.max_y());
                current_y = next_y;
                fragments.push(fragment);
            } else {
                inline_buffer.push(child.clone());
            }
        }

        flush_inline_buffer(
            &mut inline_buffer,
            &mut fragments,
            &mut current_y,
            &mut content_height,
            &mut margin_ctx,
        )?;

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
}

impl Default for BlockFormattingContext {
    fn default() -> Self {
        Self::new()
    }
}

impl std::fmt::Debug for BlockFormattingContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("BlockFormattingContext")
    }
}

impl FormattingContext for BlockFormattingContext {
    fn layout(&self, box_node: &BoxNode, constraints: &LayoutConstraints) -> Result<FragmentNode, LayoutError> {
        let style = &box_node.style;

        let containing_width = constraints.width().unwrap_or(0.0);
        let mut computed_width = compute_block_width(style, containing_width);

        let min_width = style
            .min_width
            .as_ref()
            .map(|l| resolve_length_for_width(*l, containing_width))
            .unwrap_or(0.0);
        let max_width = style
            .max_width
            .as_ref()
            .map(|l| resolve_length_for_width(*l, containing_width))
            .unwrap_or(f32::INFINITY);

        let clamped_content_width = computed_width.content_width.clamp(min_width, max_width);
        if clamped_content_width != computed_width.content_width {
            let (margin_left, margin_right) = recompute_margins_for_width(
                style,
                containing_width,
                clamped_content_width,
                computed_width.border_left,
                computed_width.padding_left,
                computed_width.padding_right,
                computed_width.border_right,
            );
            computed_width.content_width = clamped_content_width;
            computed_width.margin_left = margin_left;
            computed_width.margin_right = margin_right;
        }

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
        let box_width = computed_width.total_width();

        let bounds = Rect::from_xywh(0.0, 0.0, box_width, box_height);

        Ok(FragmentNode::new_block_styled(
            bounds,
            child_fragments,
            box_node.style.clone(),
        ))
    }

    fn compute_intrinsic_inline_size(&self, box_node: &BoxNode, mode: IntrinsicSizingMode) -> Result<f32, LayoutError> {
        let style = &box_node.style;
        // Honor specified widths that resolve without a containing block
        if let Some(specified) = style.width.as_ref() {
            let resolved = resolve_length_for_width(*specified, 0.0);
            // Ignore auto/relative cases that resolve to 0.0
            if resolved > 0.0 {
                return Ok(resolved);
            }
        }

        // Replaced elements fall back to their intrinsic content size plus padding/borders
        if let BoxType::Replaced(replaced_box) = &box_node.box_type {
            let size = compute_replaced_size(style, replaced_box);
            let edges = horizontal_padding_and_borders(style, size.width);
            return Ok(size.width + edges);
        }

        let factory = FormattingContextFactory::new();
        let inline_fc = InlineFormattingContext::new();

        // Inline formatting context contribution (text and inline-level children)
        let inline_width = inline_fc.compute_intrinsic_inline_size(box_node, mode).unwrap_or(0.0);

        // Block-level in-flow children contribute their own intrinsic widths
        let mut block_child_width = 0.0f32;
        for child in &box_node.children {
            if !child.is_block_level() || is_out_of_flow(child) {
                continue;
            }
            let fc_type = child.formatting_context().unwrap_or(FormattingContextType::Block);
            let fc = factory.create(fc_type);
            let child_width = fc.compute_intrinsic_inline_size(child, mode)?;
            block_child_width = block_child_width.max(child_width);
        }

        let content_width = inline_width.max(block_child_width);

        // Add this box's own padding and borders
        let edges = horizontal_padding_and_borders(style, 0.0);
        let mut width = content_width + edges;

        // Apply min/max constraints when present
        let min_width = style
            .min_width
            .as_ref()
            .map(|l| resolve_length_for_width(*l, 0.0))
            .unwrap_or(0.0);
        let max_width = style
            .max_width
            .as_ref()
            .map(|l| resolve_length_for_width(*l, 0.0))
            .unwrap_or(f32::INFINITY);
        width = width.clamp(min_width, max_width);

        Ok(width.max(0.0))
    }
}

/// Checks if a box is out of normal flow (absolute/fixed positioned or float)
fn is_out_of_flow(box_node: &BoxNode) -> bool {
    let position = box_node.style.position;
    matches!(position, Position::Absolute | Position::Fixed)
}

fn resolve_length_for_width(length: Length, percentage_base: f32) -> f32 {
    if length.unit.is_percentage() {
        length.resolve_against(percentage_base)
    } else if length.unit.is_absolute() {
        length.to_px()
    } else {
        // Relative units (em/rem/etc.) should be resolved earlier; use raw value as px fallback.
        length.value
    }
}

fn horizontal_padding_and_borders(style: &ComputedStyle, percentage_base: f32) -> f32 {
    resolve_length_for_width(style.padding_left, percentage_base)
        + resolve_length_for_width(style.padding_right, percentage_base)
        + resolve_length_for_width(style.border_left_width, percentage_base)
        + resolve_length_for_width(style.border_right_width, percentage_base)
}

fn recompute_margins_for_width(
    style: &ComputedStyle,
    containing_width: f32,
    content_width: f32,
    border_left: f32,
    padding_left: f32,
    padding_right: f32,
    border_right: f32,
) -> (f32, f32) {
    let margin_left = match &style.margin_left {
        Some(len) => MarginValue::Length(len.resolve_against(containing_width)),
        None => MarginValue::Auto,
    };
    let margin_right = match &style.margin_right {
        Some(len) => MarginValue::Length(len.resolve_against(containing_width)),
        None => MarginValue::Auto,
    };

    let borders_and_padding = border_left + padding_left + padding_right + border_right;

    match (margin_left, margin_right) {
        (MarginValue::Auto, MarginValue::Auto) => {
            let remaining = containing_width - borders_and_padding - content_width;
            let margin = (remaining / 2.0).max(0.0);
            (margin, margin)
        }
        (MarginValue::Auto, MarginValue::Length(mr)) => {
            let ml = containing_width - borders_and_padding - content_width - mr;
            (ml, mr)
        }
        (MarginValue::Length(ml), MarginValue::Auto) => {
            let mr = containing_width - borders_and_padding - content_width - ml;
            (ml, mr)
        }
        (MarginValue::Length(ml), MarginValue::Length(_mr)) => {
            let mr = containing_width - borders_and_padding - content_width - ml;
            (ml, mr)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::style::display::Display;
    use crate::style::display::FormattingContextType;
    use crate::style::values::Length;
    use crate::style::ComputedStyle;
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
