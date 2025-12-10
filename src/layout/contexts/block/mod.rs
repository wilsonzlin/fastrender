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
use crate::layout::constraints::{AvailableSpace, LayoutConstraints};
use crate::layout::contexts::block::width::MarginValue;
use crate::layout::contexts::factory::FormattingContextFactory;
use crate::layout::contexts::inline::InlineFormattingContext;
use crate::layout::float_context::{FloatContext, FloatSide};
use crate::layout::formatting_context::{FormattingContext, IntrinsicSizingMode, LayoutError};
use crate::layout::utils::{
    border_size_from_box_sizing, compute_replaced_size, content_size_from_box_sizing, resolve_font_relative_length,
    resolve_length_with_percentage,
};
use crate::style::display::FormattingContextType;
use crate::style::float::Float;
use crate::style::position::Position;
use crate::style::values::Length;
use crate::style::ComputedStyle;
use crate::text::font_loader::FontContext;
use crate::tree::box_tree::{BoxNode, BoxType, ReplacedBox};
use crate::tree::fragment_tree::{FragmentContent, FragmentNode};

use margin_collapse::MarginCollapseContext;
use width::compute_block_width;

/// Helper to resolve a Length to pixels, handling em/rem units with font-size
fn resolve_length(length: &Length, font_size: f32, containing_block_size: f32, viewport: crate::geometry::Size) -> f32 {
    use crate::style::values::LengthUnit;
    match length.unit {
        LengthUnit::Em | LengthUnit::Rem => length.value * font_size,
        LengthUnit::Percent => (length.value / 100.0) * containing_block_size,
        _ if length.unit.is_absolute() => length.to_px(),
        _ if length.unit.is_viewport_relative() => length.resolve_with_viewport(viewport.width, viewport.height),
        _ => 0.0, // Fallback for unknown units
    }
}

/// Helper to resolve an optional Length to pixels
fn resolve_opt_length(
    length: Option<&Length>,
    font_size: f32,
    containing_block_size: f32,
    viewport: crate::geometry::Size,
) -> f32 {
    length
        .map(|l| resolve_length(l, font_size, containing_block_size, viewport))
        .unwrap_or(0.0)
}

/// Block Formatting Context implementation
///
/// Implements the FormattingContext trait for block-level layout.
/// Handles vertical stacking, margin collapsing, and width computation.
#[derive(Clone)]
pub struct BlockFormattingContext {
    font_context: FontContext,
    viewport_size: crate::geometry::Size,
}

impl BlockFormattingContext {
    /// Creates a new BlockFormattingContext
    pub fn new() -> Self {
        Self::with_font_context_and_viewport(FontContext::new(), crate::geometry::Size::new(800.0, 600.0))
    }

    /// Creates a BlockFormattingContext backed by a specific font context so text
    /// measurement shares caches with the caller.
    pub fn with_font_context(font_context: FontContext) -> Self {
        Self::with_font_context_and_viewport(font_context, crate::geometry::Size::new(800.0, 600.0))
    }

    pub fn with_font_context_and_viewport(font_context: FontContext, viewport_size: crate::geometry::Size) -> Self {
        Self {
            font_context,
            viewport_size,
        }
    }

    /// Lays out a single block-level child and returns its fragment
    fn layout_block_child(
        &self,
        child: &BoxNode,
        containing_width: f32,
        constraints: &LayoutConstraints,
        margin_ctx: &mut MarginCollapseContext,
        current_y: f32,
    ) -> Result<(FragmentNode, f32), LayoutError> {
        if let BoxType::Replaced(replaced_box) = &child.box_type {
            return self.layout_replaced_child(
                child,
                replaced_box,
                containing_width,
                constraints,
                margin_ctx,
                current_y,
            );
        }

        let style = &child.style;
        let font_size = style.font_size; // Get font-size for resolving em units
        let containing_height = constraints.height();

        // Compute width using CSS 2.1 Section 10.3.3 algorithm
        let computed_width = compute_block_width(style, containing_width, self.viewport_size);

        // Handle vertical margins (resolve em/rem units with font-size)
        let margin_top = resolve_opt_length(
            style.margin_top.as_ref(),
            font_size,
            containing_width,
            self.viewport_size,
        );
        let margin_bottom = resolve_opt_length(
            style.margin_bottom.as_ref(),
            font_size,
            containing_width,
            self.viewport_size,
        );

        // Add top margin to pending collapse set
        margin_ctx.push_margin(margin_top);

        // Resolve collapsed margin and get Y position
        let collapsed_margin = margin_ctx.resolve();
        let box_y = current_y + collapsed_margin;

        // Create constraints for child layout
        let specified_height = style.height.as_ref().and_then(|h| {
            resolve_length_with_percentage(
                *h,
                containing_height,
                self.viewport_size,
                font_size,
                style.root_font_size,
            )
        });
        let child_height_space = specified_height
            .map(AvailableSpace::Definite)
            .unwrap_or(AvailableSpace::Indefinite);

        let child_constraints = LayoutConstraints::new(
            AvailableSpace::Definite(computed_width.content_width),
            child_height_space,
        );

        // Check if this child establishes a different formatting context
        let fc_type = child.formatting_context();

        let (child_fragments, content_height) = if let Some(fc_type) = fc_type {
            if fc_type != FormattingContextType::Block {
                // Child establishes a non-block FC - use the appropriate FC
                let factory = FormattingContextFactory::with_font_context_and_viewport(
                    self.font_context.clone(),
                    self.viewport_size,
                );
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
        let border_top = resolve_length_for_width(
            style.border_top_width,
            containing_width,
            style,
            &self.font_context,
            self.viewport_size,
        );
        let border_bottom = resolve_length_for_width(
            style.border_bottom_width,
            containing_width,
            style,
            &self.font_context,
            self.viewport_size,
        );
        let padding_top = resolve_length_for_width(
            style.padding_top,
            containing_width,
            style,
            &self.font_context,
            self.viewport_size,
        );
        let padding_bottom = resolve_length_for_width(
            style.padding_bottom,
            containing_width,
            style,
            &self.font_context,
            self.viewport_size,
        );

        // Height computation (CSS 2.1 Section 10.6.3)
        let height = specified_height.unwrap_or(content_height);

        // Apply min/max height constraints
        let min_height = style
            .min_height
            .as_ref()
            .and_then(|l| {
                resolve_length_with_percentage(
                    *l,
                    containing_height,
                    self.viewport_size,
                    font_size,
                    style.root_font_size,
                )
            })
            .unwrap_or(0.0);
        let max_height = style
            .max_height
            .as_ref()
            .and_then(|l| {
                resolve_length_with_percentage(
                    *l,
                    containing_height,
                    self.viewport_size,
                    font_size,
                    style.root_font_size,
                )
            })
            .unwrap_or(f32::INFINITY);
        let height = height.clamp(min_height, max_height);

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
        constraints: &LayoutConstraints,
        margin_ctx: &mut MarginCollapseContext,
        current_y: f32,
    ) -> Result<(FragmentNode, f32), LayoutError> {
        let style = &child.style;
        let font_size = style.font_size;

        let percentage_base = constraints
            .height()
            .map(|h| crate::geometry::Size::new(containing_width, h));
        let used_size = compute_replaced_size(style, replaced_box, percentage_base, self.viewport_size);

        // Vertical margins collapse as normal blocks
        let margin_top = resolve_opt_length(
            style.margin_top.as_ref(),
            font_size,
            containing_width,
            self.viewport_size,
        );
        let margin_bottom = resolve_opt_length(
            style.margin_bottom.as_ref(),
            font_size,
            containing_width,
            self.viewport_size,
        );
        margin_ctx.push_margin(margin_top);
        let collapsed_margin = margin_ctx.resolve();
        let box_y = current_y + collapsed_margin;

        // Resolve padding and borders
        let padding_top = resolve_length_for_width(
            style.padding_top,
            containing_width,
            style,
            &self.font_context,
            self.viewport_size,
        );
        let padding_bottom = resolve_length_for_width(
            style.padding_bottom,
            containing_width,
            style,
            &self.font_context,
            self.viewport_size,
        );

        let border_top = resolve_length_for_width(
            style.border_top_width,
            containing_width,
            style,
            &self.font_context,
            self.viewport_size,
        );
        let border_bottom = resolve_length_for_width(
            style.border_bottom_width,
            containing_width,
            style,
            &self.font_context,
            self.viewport_size,
        );

        // Use the resolved replaced width when computing horizontal metrics
        let mut width_style = (*style).as_ref().clone();
        width_style.width = Some(Length::px(used_size.width));
        width_style.box_sizing = crate::style::types::BoxSizing::ContentBox;
        let computed_width = compute_block_width(&width_style, containing_width, self.viewport_size);

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
        let mut float_ctx = FloatContext::new(constraints.width().unwrap_or(0.0));

        // Get containing width from constraints
        let containing_width = constraints.width().unwrap_or(0.0);
        let available_height = constraints.available_height;

        // Check for border/padding that prevents margin collapse with first child
        let parent_has_top_separation = resolve_length_for_width(
            parent.style.border_top_width,
            containing_width,
            &parent.style,
            &self.font_context,
            self.viewport_size,
        ) > 0.0
            || resolve_length_for_width(
                parent.style.padding_top,
                containing_width,
                &parent.style,
                &self.font_context,
                self.viewport_size,
            ) > 0.0;

        if parent_has_top_separation {
            margin_ctx.mark_content_encountered();
        }

        let flush_inline_buffer = |buffer: &mut Vec<BoxNode>,
                                   fragments: &mut Vec<FragmentNode>,
                                   current_y: &mut f32,
                                   content_height: &mut f32,
                                   margin_ctx: &mut MarginCollapseContext,
                                   float_ctx_ref: &mut FloatContext|
         -> Result<(), LayoutError> {
            if buffer.is_empty() {
                return Ok(());
            }

            // Apply any pending collapsed margin before inline content
            let pending_margin = margin_ctx.consume_pending();
            *current_y += pending_margin;

            let inline_container = BoxNode::new_inline(parent.style.clone(), buffer.clone());
            let inline_fc =
                InlineFormattingContext::with_font_context_and_viewport(self.font_context.clone(), self.viewport_size);
            let inline_constraints =
                LayoutConstraints::new(AvailableSpace::Definite(containing_width), available_height);
            let mut inline_fragment = inline_fc.layout_with_floats(
                &inline_container,
                &inline_constraints,
                Some(float_ctx_ref),
                *current_y,
            )?;

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
            // Skip out-of-flow positioned boxes (absolute/fixed)
            if is_out_of_flow(child) {
                continue;
            }

            // Floats are taken out of flow but still participate in this BFC's float context
            if child.style.float.is_floating() && !matches!(child.style.position, Position::Absolute | Position::Fixed)
            {
                flush_inline_buffer(
                    &mut inline_buffer,
                    &mut fragments,
                    &mut current_y,
                    &mut content_height,
                    &mut margin_ctx,
                    &mut float_ctx,
                )?;

                // Apply any pending collapsed margin before placing the float
                let pending_margin = margin_ctx.consume_pending();
                current_y += pending_margin;

                // Honor clearance against existing floats
                current_y = float_ctx.compute_clearance(current_y, child.style.clear);

                let percentage_base = containing_width;
                let margin_left = child
                    .style
                    .margin_left
                    .as_ref()
                    .map(|l| {
                        resolve_length_for_width(
                            *l,
                            percentage_base,
                            &child.style,
                            &self.font_context,
                            self.viewport_size,
                        )
                    })
                    .unwrap_or(0.0)
                    .max(0.0);
                let margin_right = child
                    .style
                    .margin_right
                    .as_ref()
                    .map(|l| {
                        resolve_length_for_width(
                            *l,
                            percentage_base,
                            &child.style,
                            &self.font_context,
                            self.viewport_size,
                        )
                    })
                    .unwrap_or(0.0)
                    .max(0.0);
                let horizontal_edges = horizontal_padding_and_borders(
                    &child.style,
                    percentage_base,
                    self.viewport_size,
                    &self.font_context,
                );

                // CSS 2.1 shrink-to-fit formula for floats
                let factory = FormattingContextFactory::with_font_context_and_viewport(
                    self.font_context.clone(),
                    self.viewport_size,
                );
                let fc_type = child.formatting_context().unwrap_or(FormattingContextType::Block);
                let fc = factory.create(fc_type);
                let preferred_min_content = fc
                    .compute_intrinsic_inline_size(child, IntrinsicSizingMode::MinContent)
                    .unwrap_or(0.0);
                let preferred_content = fc
                    .compute_intrinsic_inline_size(child, IntrinsicSizingMode::MaxContent)
                    .unwrap_or(preferred_min_content);

                let preferred_min = preferred_min_content + horizontal_edges;
                let preferred = preferred_content + horizontal_edges;

                let specified_width = child
                    .style
                    .width
                    .as_ref()
                    .map(|l| {
                        resolve_length_for_width(
                            *l,
                            percentage_base,
                            &child.style,
                            &self.font_context,
                            self.viewport_size,
                        )
                    })
                    .map(|w| border_size_from_box_sizing(w, horizontal_edges, child.style.box_sizing));

                let min_width = child
                    .style
                    .min_width
                    .as_ref()
                    .map(|l| {
                        resolve_length_for_width(
                            *l,
                            percentage_base,
                            &child.style,
                            &self.font_context,
                            self.viewport_size,
                        )
                    })
                    .map(|w| border_size_from_box_sizing(w, horizontal_edges, child.style.box_sizing))
                    .unwrap_or(0.0);
                let max_width = child
                    .style
                    .max_width
                    .as_ref()
                    .map(|l| {
                        resolve_length_for_width(
                            *l,
                            percentage_base,
                            &child.style,
                            &self.font_context,
                            self.viewport_size,
                        )
                    })
                    .map(|w| border_size_from_box_sizing(w, horizontal_edges, child.style.box_sizing))
                    .unwrap_or(f32::INFINITY);

                let available = (containing_width - margin_left - margin_right).max(0.0);
                let used_border_box = if let Some(specified) = specified_width {
                    specified.clamp(min_width, max_width)
                } else {
                    let shrink = preferred.min(available.max(preferred_min));
                    shrink.clamp(min_width, max_width)
                };

                let content_width = (used_border_box - horizontal_edges).max(0.0);

                let child_constraints =
                    LayoutConstraints::new(AvailableSpace::Definite(content_width), AvailableSpace::Indefinite);
                let child_bfc = BlockFormattingContext::with_font_context_and_viewport(
                    self.font_context.clone(),
                    self.viewport_size,
                );
                let mut fragment = child_bfc.layout(child, &child_constraints)?;

                let margin_top = child
                    .style
                    .margin_top
                    .as_ref()
                    .map(|l| {
                        resolve_length_for_width(
                            *l,
                            containing_width,
                            &child.style,
                            &self.font_context,
                            self.viewport_size,
                        )
                    })
                    .unwrap_or(0.0)
                    .max(0.0);
                let margin_bottom = child
                    .style
                    .margin_bottom
                    .as_ref()
                    .map(|l| {
                        resolve_length_for_width(
                            *l,
                            containing_width,
                            &child.style,
                            &self.font_context,
                            self.viewport_size,
                        )
                    })
                    .unwrap_or(0.0)
                    .max(0.0);
                let box_width = used_border_box;
                let float_height = margin_top + fragment.bounds.height() + margin_bottom;

                let side = match child.style.float {
                    Float::Left => FloatSide::Left,
                    Float::Right => FloatSide::Right,
                    Float::None => unreachable!(),
                };

                let (fx, fy) = float_ctx.compute_float_position(
                    side,
                    margin_left + box_width + margin_right,
                    float_height,
                    current_y,
                );

                fragment.bounds =
                    Rect::from_xywh(fx + margin_left, fy + margin_top, box_width, fragment.bounds.height());
                float_ctx.add_float_at(side, fx, fy, margin_left + box_width + margin_right, float_height);
                content_height = content_height.max(fy + float_height);
                fragments.push(fragment);
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
                    &mut float_ctx,
                )?;

                // Apply clearance for in-flow blocks against floats
                let cleared_y = float_ctx.compute_clearance(current_y, child.style.clear);
                if cleared_y > current_y {
                    margin_ctx.mark_content_encountered();
                    current_y = cleared_y;
                }

                let (fragment, next_y) =
                    self.layout_block_child(child, containing_width, constraints, &mut margin_ctx, current_y)?;

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
                    &mut float_ctx,
                )?;

        // Resolve any trailing margins
        let trailing_margin = margin_ctx.pending_margin();

        // Check for bottom separation
        let parent_has_bottom_separation = resolve_length_for_width(
            parent.style.border_bottom_width,
            containing_width,
            &parent.style,
            &self.font_context,
            self.viewport_size,
        ) > 0.0
            || resolve_length_for_width(
                parent.style.padding_bottom,
                containing_width,
                &parent.style,
                &self.font_context,
                self.viewport_size,
            ) > 0.0;

        if parent_has_bottom_separation {
            content_height += trailing_margin.max(0.0);
        }

        // Float boxes extend the formatting context height
        let float_bottom = float_ctx
            .left_floats()
            .iter()
            .chain(float_ctx.right_floats())
            .map(|f| f.bottom())
            .fold(content_height, f32::max);

        Ok((fragments, float_bottom))
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
        let containing_height = constraints.height();
        let mut computed_width = compute_block_width(style, containing_width, self.viewport_size);
        let horizontal_edges = computed_width.border_left
            + computed_width.padding_left
            + computed_width.padding_right
            + computed_width.border_right;

        let min_width = style
            .min_width
            .as_ref()
            .map(|l| resolve_length_for_width(*l, containing_width, style, &self.font_context, self.viewport_size))
            .map(|w| content_size_from_box_sizing(w, horizontal_edges, style.box_sizing))
            .unwrap_or(0.0);
        let max_width = style
            .max_width
            .as_ref()
            .map(|l| resolve_length_for_width(*l, containing_width, style, &self.font_context, self.viewport_size))
            .map(|w| content_size_from_box_sizing(w, horizontal_edges, style.box_sizing))
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
                self.viewport_size,
                &self.font_context,
            );
            computed_width.content_width = clamped_content_width;
            computed_width.margin_left = margin_left;
            computed_width.margin_right = margin_right;
        }

        let border_top = resolve_length_for_width(
            style.border_top_width,
            containing_width,
            style,
            &self.font_context,
            self.viewport_size,
        );
        let border_bottom = resolve_length_for_width(
            style.border_bottom_width,
            containing_width,
            style,
            &self.font_context,
            self.viewport_size,
        );
        let padding_top = resolve_length_for_width(
            style.padding_top,
            containing_width,
            style,
            &self.font_context,
            self.viewport_size,
        );
        let padding_bottom = resolve_length_for_width(
            style.padding_bottom,
            containing_width,
            style,
            &self.font_context,
            self.viewport_size,
        );
        let vertical_edges = border_top + padding_top + padding_bottom + border_bottom;

        let resolved_height = style
            .height
            .as_ref()
            .and_then(|h| {
                resolve_length_with_percentage(
                    *h,
                    containing_height,
                    self.viewport_size,
                    style.font_size,
                    style.root_font_size,
                )
            })
            .map(|h| content_size_from_box_sizing(h, vertical_edges, style.box_sizing));
        let child_height_space = resolved_height
            .map(|h| AvailableSpace::Definite(h.max(0.0)))
            .unwrap_or(AvailableSpace::Indefinite);

        let child_constraints = LayoutConstraints::new(
            AvailableSpace::Definite(computed_width.content_width),
            child_height_space,
        );

        let (child_fragments, content_height) = self.layout_children(box_node, &child_constraints)?;

        let min_height = style
            .min_height
            .as_ref()
            .and_then(|l| {
                resolve_length_with_percentage(
                    *l,
                    containing_height,
                    self.viewport_size,
                    style.font_size,
                    style.root_font_size,
                )
            })
            .map(|h| content_size_from_box_sizing(h, vertical_edges, style.box_sizing))
            .unwrap_or(0.0);
        let max_height = style
            .max_height
            .as_ref()
            .and_then(|l| {
                resolve_length_with_percentage(
                    *l,
                    containing_height,
                    self.viewport_size,
                    style.font_size,
                    style.root_font_size,
                )
            })
            .map(|h| content_size_from_box_sizing(h, vertical_edges, style.box_sizing))
            .unwrap_or(f32::INFINITY);

        let height = resolved_height.unwrap_or(content_height).clamp(min_height, max_height);

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
        let edges = horizontal_padding_and_borders(style, 0.0, self.viewport_size, &self.font_context);
        if let Some(specified) = style.width.as_ref() {
            let resolved = resolve_length_for_width(*specified, 0.0, style, &self.font_context, self.viewport_size);
            // Ignore auto/relative cases that resolve to 0.0
            if resolved > 0.0 {
                return Ok(border_size_from_box_sizing(resolved, edges, style.box_sizing));
            }
        }

        // Replaced elements fall back to their intrinsic content size plus padding/borders
        if let BoxType::Replaced(replaced_box) = &box_node.box_type {
            let size = compute_replaced_size(style, replaced_box, None, self.viewport_size);
            let edges = horizontal_padding_and_borders(style, size.width, self.viewport_size, &self.font_context);
            return Ok(size.width + edges);
        }

        let factory =
            FormattingContextFactory::with_font_context_and_viewport(self.font_context.clone(), self.viewport_size);
        let inline_fc =
            InlineFormattingContext::with_font_context_and_viewport(self.font_context.clone(), self.viewport_size);

        // Inline formatting context contribution (text and inline-level children).
        // Block-level children split inline runs into separate formatting contexts.
        let mut inline_width = 0.0f32;
        let mut inline_run: Vec<BoxNode> = Vec::new();
        let flush_inline_run = |run: &mut Vec<BoxNode>, widest: &mut f32| -> Result<(), LayoutError> {
            if run.is_empty() {
                return Ok(());
            }
            let inline_container = BoxNode::new_inline(box_node.style.clone(), run.clone());
            let width = inline_fc.compute_intrinsic_inline_size(&inline_container, mode)?;
            *widest = widest.max(width);
            run.clear();
            Ok(())
        };

        for child in &box_node.children {
            if is_out_of_flow(child) {
                continue;
            }

            let treated_as_block = match child.box_type {
                BoxType::Replaced(_) if child.style.display.is_inline_level() => false,
                _ => child.is_block_level(),
            };

            if treated_as_block {
                flush_inline_run(&mut inline_run, &mut inline_width)?;
            } else {
                inline_run.push(child.clone());
            }
        }
        flush_inline_run(&mut inline_run, &mut inline_width)?;

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
        let mut width = content_width + edges;

        // Apply min/max constraints when present
        let min_width = style
            .min_width
            .as_ref()
            .map(|l| resolve_length_for_width(*l, 0.0, style, &self.font_context, self.viewport_size))
            .map(|w| border_size_from_box_sizing(w, edges, style.box_sizing))
            .unwrap_or(0.0);
        let max_width = style
            .max_width
            .as_ref()
            .map(|l| resolve_length_for_width(*l, 0.0, style, &self.font_context, self.viewport_size))
            .map(|w| border_size_from_box_sizing(w, edges, style.box_sizing))
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

fn resolve_length_for_width(
    length: Length,
    percentage_base: f32,
    style: &ComputedStyle,
    font_context: &FontContext,
    viewport: crate::geometry::Size,
) -> f32 {
    if length.unit.is_percentage() {
        length.resolve_against(percentage_base)
    } else if length.unit.is_absolute() {
        length.to_px()
    } else if length.unit.is_viewport_relative() {
        length.resolve_with_viewport(viewport.width, viewport.height)
    } else {
        resolve_font_relative_length(length, style, font_context)
    }
}

fn horizontal_padding_and_borders(
    style: &ComputedStyle,
    percentage_base: f32,
    viewport: crate::geometry::Size,
    font_context: &FontContext,
) -> f32 {
    resolve_length_for_width(style.padding_left, percentage_base, style, font_context, viewport)
        + resolve_length_for_width(style.padding_right, percentage_base, style, font_context, viewport)
        + resolve_length_for_width(style.border_left_width, percentage_base, style, font_context, viewport)
        + resolve_length_for_width(style.border_right_width, percentage_base, style, font_context, viewport)
}

fn recompute_margins_for_width(
    style: &ComputedStyle,
    containing_width: f32,
    content_width: f32,
    border_left: f32,
    padding_left: f32,
    padding_right: f32,
    border_right: f32,
    viewport: crate::geometry::Size,
    font_context: &FontContext,
) -> (f32, f32) {
    let margin_left = match &style.margin_left {
        Some(len) => MarginValue::Length(resolve_length_for_width(
            *len,
            containing_width,
            style,
            font_context,
            viewport,
        )),
        None => MarginValue::Auto,
    };
    let margin_right = match &style.margin_right {
        Some(len) => MarginValue::Length(resolve_length_for_width(
            *len,
            containing_width,
            style,
            font_context,
            viewport,
        )),
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
    use crate::layout::contexts::inline::InlineFormattingContext;
    use crate::layout::formatting_context::IntrinsicSizingMode;
    use crate::style::display::Display;
    use crate::style::display::FormattingContextType;
    use crate::style::types::{ListStylePosition, ListStyleType};
    use crate::style::values::Length;
    use crate::style::ComputedStyle;
    use crate::tree::box_generation::{BoxGenerator, DOMNode};
    use crate::tree::fragment_tree::FragmentContent;
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
    fn percentage_height_uses_definite_containing_block() {
        let bfc = BlockFormattingContext::new();

        let mut child_style = ComputedStyle::default();
        child_style.display = Display::Block;
        child_style.height = Some(Length::percent(50.0));
        let child = BoxNode::new_block(Arc::new(child_style), FormattingContextType::Block, vec![]);
        let mut root_style = ComputedStyle::default();
        root_style.display = Display::Block;
        root_style.height = Some(Length::px(300.0));
        let root = BoxNode::new_block(Arc::new(root_style), FormattingContextType::Block, vec![child]);
        let constraints = LayoutConstraints::definite(200.0, 400.0);

        let fragment = bfc.layout(&root, &constraints).unwrap();
        let child_fragment = fragment.children.first().expect("child fragment");
        assert!((child_fragment.bounds.height() - 150.0).abs() < 0.1);
    }

    #[test]
    fn percentage_height_without_base_falls_back_to_auto() {
        let bfc = BlockFormattingContext::new();
        let mut child_style = ComputedStyle::default();
        child_style.display = Display::Block;
        child_style.height = Some(Length::percent(60.0));
        let child = BoxNode::new_block(Arc::new(child_style), FormattingContextType::Block, vec![]);
        let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![child]);
        let constraints = LayoutConstraints::definite_width(200.0);

        let fragment = bfc.layout(&root, &constraints).unwrap();
        let child_fragment = fragment.children.first().expect("child fragment");
        assert_eq!(child_fragment.bounds.height(), 0.0);
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

    #[test]
    fn floats_extend_height_and_clear_moves_following_block() {
        let bfc = BlockFormattingContext::new();

        let mut float_style = ComputedStyle::default();
        float_style.display = Display::Block;
        float_style.float = Float::Left;
        float_style.width = Some(Length::px(60.0));
        float_style.height = Some(Length::px(50.0));
        let float_node = BoxNode::new_block(Arc::new(float_style), FormattingContextType::Block, vec![]);

        let mut cleared_style = ComputedStyle::default();
        cleared_style.display = Display::Block;
        cleared_style.clear = crate::style::float::Clear::Left;
        cleared_style.height = Some(Length::px(10.0));
        let cleared_node = BoxNode::new_block(Arc::new(cleared_style), FormattingContextType::Block, vec![]);

        let root = BoxNode::new_block(
            default_style(),
            FormattingContextType::Block,
            vec![float_node, cleared_node],
        );
        let constraints = LayoutConstraints::definite(200.0, 400.0);

        let fragment = bfc.layout(&root, &constraints).unwrap();
        assert!(
            fragment.bounds.height() >= 60.0,
            "BFC height should include float and cleared block; got {}",
            fragment.bounds.height()
        );

        let mut float_y = None;
        let mut clear_y = None;
        for child in &fragment.children {
            if let Some(style) = &child.style {
                if style.float.is_floating() {
                    float_y = Some(child.bounds.y());
                }
                if matches!(
                    style.clear,
                    crate::style::float::Clear::Left | crate::style::float::Clear::Both
                ) {
                    clear_y = Some(child.bounds.y());
                }
            }
        }

        let float_y = float_y.expect("float fragment");
        let clear_y = clear_y.expect("cleared fragment");
        assert!(float_y.abs() < 0.01);
        assert!(
            clear_y >= 50.0,
            "cleared block should be pushed below float; got clear_y={clear_y}"
        );
    }

    #[test]
    fn inline_lines_shorten_next_to_float() {
        let bfc = BlockFormattingContext::new();

        let mut float_style = ComputedStyle::default();
        float_style.display = Display::Block;
        float_style.float = Float::Left;
        float_style.width = Some(Length::px(80.0));
        float_style.height = Some(Length::px(20.0));
        let float_node = BoxNode::new_block(Arc::new(float_style), FormattingContextType::Block, vec![]);

        let text = BoxNode::new_text(default_style(), "text".to_string());
        let root = BoxNode::new_block(
            default_style(),
            FormattingContextType::Block,
            vec![float_node, BoxNode::new_inline(default_style(), vec![text])],
        );
        let constraints = LayoutConstraints::definite(200.0, 200.0);

        let fragment = bfc.layout(&root, &constraints).unwrap();

        fn find_line(fragment: &FragmentNode) -> Option<&FragmentNode> {
            if matches!(fragment.content, FragmentContent::Line { .. }) {
                return Some(fragment);
            }
            for child in &fragment.children {
                if let Some(line) = find_line(child) {
                    return Some(line);
                }
            }
            None
        }

        let line = find_line(&fragment).expect("line fragment");
        assert!(
            line.bounds.width() <= 120.0,
            "line width should be shortened by float; got {}",
            line.bounds.width()
        );
        assert!(
            line.bounds.x() >= 79.9,
            "line should start after the float; got x={}",
            line.bounds.x()
        );
    }

    #[test]
    fn list_marker_outside_positions_marker_left_of_text() {
        let generator = BoxGenerator::new();

        let mut li_style = ComputedStyle::default();
        li_style.display = Display::ListItem;
        li_style.list_style_position = ListStylePosition::Outside;
        let li_style = Arc::new(li_style);

        let mut ul_style = ComputedStyle::default();
        ul_style.display = Display::Block;
        let ul_style = Arc::new(ul_style);

        let li = DOMNode::new_element(
            "li",
            li_style.clone(),
            vec![DOMNode::new_text("Item", li_style.clone())],
        );
        let ul = DOMNode::new_element("ul", ul_style, vec![li]);
        let box_tree = generator.generate(&ul).unwrap();

        let bfc = BlockFormattingContext::new();
        let constraints = LayoutConstraints::definite(200.0, 200.0);
        let fragment = bfc.layout(&box_tree.root, &constraints).unwrap();

        let li_fragment = fragment.children.first().expect("li fragment");
        fn find_line(fragment: &FragmentNode) -> Option<&FragmentNode> {
            if matches!(fragment.content, FragmentContent::Line { .. }) {
                return Some(fragment);
            }
            for child in &fragment.children {
                if let Some(line) = find_line(child) {
                    return Some(line);
                }
            }
            None
        }

        let line = find_line(li_fragment).expect("line fragment");

        let marker = line
            .children
            .iter()
            .find(|child| {
                child
                    .style
                    .as_ref()
                    .map(|s| s.list_style_type == ListStyleType::None)
                    .unwrap_or(false)
            })
            .expect("marker fragment");
        let text = line
            .children
            .iter()
            .find(|child| {
                child
                    .style
                    .as_ref()
                    .map(|s| s.list_style_type != ListStyleType::None)
                    .unwrap_or(false)
            })
            .expect("text fragment");

        assert!(marker.bounds.x() < 0.0);
        assert!(text.bounds.x() >= 0.0);
    }

    #[test]
    fn intrinsic_inline_size_splits_runs_around_block_children() {
        let bfc = BlockFormattingContext::new();
        let ifc = InlineFormattingContext::new();

        let text_left = BoxNode::new_text(default_style(), "unbreakable".to_string());
        let text_right = BoxNode::new_text(default_style(), "unbreakable".to_string());
        let block_child = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);

        let run_container = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![text_left.clone()]);
        let run_min = ifc
            .compute_intrinsic_inline_size(&run_container, IntrinsicSizingMode::MinContent)
            .unwrap();
        let run_max = ifc
            .compute_intrinsic_inline_size(&run_container, IntrinsicSizingMode::MaxContent)
            .unwrap();

        let root = BoxNode::new_block(
            default_style(),
            FormattingContextType::Block,
            vec![text_left, block_child, text_right],
        );
        let min_width = bfc
            .compute_intrinsic_inline_size(&root, IntrinsicSizingMode::MinContent)
            .unwrap();
        assert!(
            min_width <= run_min * 1.1,
            "min-content width should follow the widest inline run, got {min_width} vs run {run_min}"
        );

        let max_width = bfc
            .compute_intrinsic_inline_size(&root, IntrinsicSizingMode::MaxContent)
            .unwrap();
        assert!(
            max_width <= run_max * 1.1,
            "max-content width should not concatenate inline runs across blocks, got {max_width} vs run {run_max}"
        );
    }
}
