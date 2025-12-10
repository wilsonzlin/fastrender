//! Positioned Layout - CSS positioned layout implementation
//!
//! This module implements CSS positioned layout for `position: relative`,
//! `position: absolute`, `position: fixed`, and `position: sticky`.
//!
//! # CSS Specification References
//!
//! - CSS 2.1 Section 9.3: Positioning schemes
//! - CSS 2.1 Section 10.3.7: Absolutely positioned, non-replaced elements
//! - CSS 2.1 Section 10.6.4: Absolutely positioned, non-replaced elements (height)
//! - CSS Position Module Level 3: Sticky positioning
//!
//! # Key Concepts
//!
//! - **Relative positioning**: Box is laid out normally, then offset
//! - **Absolute positioning**: Box is removed from flow, positioned relative to containing block
//! - **Fixed positioning**: Like absolute, but relative to viewport
//! - **Sticky positioning**: Hybrid of relative and fixed
//!
//! # Architecture
//!
//! Positioned layout is NOT a formatting context. Instead, it's a post-process
//! that adjusts fragment positions after normal layout. The key insight:
//!
//! 1. Normal layout (block, inline, etc.) determines size and initial position
//! 2. Positioned layout then adjusts positions based on offset properties
//!
//! # Example
//!
//! ```rust,ignore
//! use fastrender::layout::contexts::positioned::PositionedLayout;
//! use fastrender::FragmentNode;
//!
//! let layout = PositionedLayout::new();
//! let adjusted_fragment = layout.apply_relative_positioning(
//!     fragment,
//!     &style,
//!     containing_block_size,
//! )?;
//! ```

use crate::geometry::{Point, Rect, Size};
use crate::layout::formatting_context::LayoutError;
use crate::layout::utils::{content_size_from_box_sizing, resolve_offset};
use crate::style::computed::PositionedStyle;
use crate::style::position::Position;
use crate::tree::fragment_tree::FragmentNode;

/// Represents a containing block for positioned elements
///
/// The containing block determines the reference rectangle for
/// positioning calculations. Different position types use different
/// containing blocks:
///
/// - **static/relative**: Content box of nearest block container ancestor
/// - **absolute**: Padding box of nearest positioned ancestor
/// - **fixed**: Viewport (initial containing block)
/// - **sticky**: Content box of nearest block container ancestor
///
/// CSS 2.1 Section 10.1: Definition of "containing block"
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ContainingBlock {
    /// The rectangle of the containing block
    ///
    /// For absolute positioning, this is the padding box.
    /// For fixed positioning, this is the viewport.
    pub rect: Rect,
}

impl ContainingBlock {
    /// Creates a new containing block from a rectangle
    pub const fn new(rect: Rect) -> Self {
        Self { rect }
    }

    /// Creates a containing block from position and size
    pub fn from_origin_size(origin: Point, size: Size) -> Self {
        Self {
            rect: Rect::new(origin, size),
        }
    }

    /// Creates a containing block representing the viewport
    ///
    /// Used for fixed positioning and as the initial containing block.
    pub fn viewport(size: Size) -> Self {
        Self {
            rect: Rect::new(Point::ZERO, size),
        }
    }

    /// Returns the width of the containing block
    pub fn width(&self) -> f32 {
        self.rect.size.width
    }

    /// Returns the height of the containing block
    pub fn height(&self) -> f32 {
        self.rect.size.height
    }

    /// Returns the origin of the containing block
    pub fn origin(&self) -> Point {
        self.rect.origin
    }
}

/// Sticky positioning constraints
///
/// Stores the offset constraints for sticky positioning.
/// During rendering, these constraints determine when the element
/// transitions from relative to fixed behavior.
///
/// CSS Position Module Level 3: <https://www.w3.org/TR/css-position-3/#sticky-pos>
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct StickyConstraints {
    /// Top constraint in pixels (if specified)
    pub top: Option<f32>,

    /// Right constraint in pixels (if specified)
    pub right: Option<f32>,

    /// Bottom constraint in pixels (if specified)
    pub bottom: Option<f32>,

    /// Left constraint in pixels (if specified)
    pub left: Option<f32>,
}

impl StickyConstraints {
    /// Creates empty sticky constraints (all None)
    pub const fn none() -> Self {
        Self {
            top: None,
            right: None,
            bottom: None,
            left: None,
        }
    }

    /// Creates sticky constraints from computed style
    pub fn from_style(style: &PositionedStyle, containing_block: &ContainingBlock) -> Self {
        Self {
            top: resolve_offset(&style.top, containing_block.height()),
            right: resolve_offset(&style.right, containing_block.width()),
            bottom: resolve_offset(&style.bottom, containing_block.height()),
            left: resolve_offset(&style.left, containing_block.width()),
        }
    }

    /// Returns true if any constraint is specified
    pub fn has_constraints(&self) -> bool {
        self.top.is_some() || self.right.is_some() || self.bottom.is_some() || self.left.is_some()
    }
}

/// Positioned layout engine
///
/// Handles CSS positioned layout (relative, absolute, fixed, sticky).
/// This is NOT a formatting context - it adjusts positions after normal layout.
///
/// # Usage
///
/// 1. Normal layout produces fragments with initial positions
/// 2. PositionedLayout adjusts positions based on CSS position property
///
/// # Thread Safety
///
/// PositionedLayout is stateless and can be shared across threads.
#[derive(Debug, Clone, Default)]
pub struct PositionedLayout;

impl PositionedLayout {
    /// Creates a new positioned layout handler
    pub const fn new() -> Self {
        Self
    }

    /// Applies relative positioning to a fragment
    ///
    /// Relative positioning offsets the fragment from its normal flow position
    /// without affecting the layout of other elements. The original space is preserved.
    ///
    /// # CSS 2.1 Section 9.3.2
    ///
    /// "Once a box has been laid out according to the normal flow, it is then
    /// offset relative to its normal position. This is called relative positioning."
    ///
    /// # Offset Resolution
    ///
    /// - If both `top` and `bottom` are specified, `top` wins (for horizontal writing modes)
    /// - If both `left` and `right` are specified, `left` wins (for LTR)
    /// - `auto` values resolve to `0`
    ///
    /// # Arguments
    ///
    /// * `fragment` - The fragment to adjust
    /// * `style` - The computed style containing offset properties
    /// * `containing_block` - The containing block for percentage resolution
    ///
    /// # Returns
    ///
    /// A new fragment with adjusted position. Children are NOT affected.
    ///
    /// # Example
    ///
    /// ```rust,ignore
    /// let layout = PositionedLayout::new();
    /// let adjusted = layout.apply_relative_positioning(
    ///     fragment,
    ///     &style,
    ///     &containing_block,
    /// )?;
    /// ```
    pub fn apply_relative_positioning(
        &self,
        fragment: &FragmentNode,
        style: &PositionedStyle,
        containing_block: &ContainingBlock,
    ) -> Result<FragmentNode, LayoutError> {
        // Only apply to position: relative
        if !style.position.is_relative() {
            return Ok(fragment.clone());
        }

        // Compute the offset from top/right/bottom/left properties
        let offset = self.compute_relative_offset(style, containing_block);

        // Apply offset to fragment position
        let new_bounds = fragment.bounds.translate(offset);

        // Create new fragment with adjusted bounds but same children
        // Note: Children positions are relative to parent, so they don't change
        Ok(FragmentNode::new(
            new_bounds,
            fragment.content.clone(),
            fragment.children.clone(),
        ))
    }

    /// Computes the offset for relative positioning
    ///
    /// CSS 2.1 Section 9.3.2:
    /// - If both `top` and `bottom` are specified, `top` wins
    /// - If both `left` and `right` are specified, `left` wins (LTR)
    fn compute_relative_offset(&self, style: &PositionedStyle, containing_block: &ContainingBlock) -> Point {
        let mut offset_x = 0.0;
        let mut offset_y = 0.0;

        let cb_width = containing_block.width();
        let cb_height = containing_block.height();

        // Vertical offset: top takes precedence over bottom
        if let Some(top) = resolve_offset(&style.top, cb_height) {
            offset_y = top;
        } else if let Some(bottom) = resolve_offset(&style.bottom, cb_height) {
            offset_y = -bottom;
        }

        // Horizontal offset: left takes precedence over right (LTR)
        if let Some(left) = resolve_offset(&style.left, cb_width) {
            offset_x = left;
        } else if let Some(right) = resolve_offset(&style.right, cb_width) {
            offset_x = -right;
        }

        Point::new(offset_x, offset_y)
    }

    /// Computes the position and size for an absolutely positioned element
    ///
    /// Absolutely positioned elements are removed from normal flow and positioned
    /// relative to their containing block.
    ///
    /// # CSS 2.1 Section 10.3.7 (width) and 10.6.4 (height)
    ///
    /// The constraint equation for horizontal positioning:
    /// `left + margin-left + border-left + padding-left + width +
    ///  padding-right + border-right + margin-right + right = containing block width`
    ///
    /// # Arguments
    ///
    /// * `style` - The computed style
    /// * `containing_block` - The containing block (padding box of positioned ancestor)
    /// * `intrinsic_size` - The intrinsic size if width/height are auto
    ///
    /// # Returns
    ///
    /// Tuple of (position, size) in containing block coordinates
    pub fn compute_absolute_position(
        &self,
        style: &PositionedStyle,
        containing_block: &ContainingBlock,
        intrinsic_size: Size,
    ) -> Result<(Point, Size), LayoutError> {
        let cb_width = containing_block.width();
        let cb_height = containing_block.height();

        // Compute horizontal position and width
        let (x, width) = self.compute_absolute_horizontal(style, cb_width, intrinsic_size.width)?;

        // Compute vertical position and height
        let (y, height) = self.compute_absolute_vertical(style, cb_height, intrinsic_size.height)?;

        // Position is relative to containing block origin
        let position = Point::new(containing_block.origin().x + x, containing_block.origin().y + y);

        Ok((position, Size::new(width, height)))
    }

    /// Computes horizontal position and width for absolutely positioned element
    ///
    /// Handles the constraint equation and various combinations of
    /// specified/auto values for left, right, width.
    fn compute_absolute_horizontal(
        &self,
        style: &PositionedStyle,
        cb_width: f32,
        intrinsic_width: f32,
    ) -> Result<(f32, f32), LayoutError> {
        let left = resolve_offset(&style.left, cb_width);
        let right = resolve_offset(&style.right, cb_width);

        // Get margin values (auto margins = 0 for absolute positioning unless overconstrained)
        let margin_left = style.margin.left;
        let margin_right = style.margin.right;

        // Get padding and border (never auto)
        let padding_left = style.padding.left;
        let padding_right = style.padding.right;
        let border_left = style.border_width.left;
        let border_right = style.border_width.right;

        let total_horizontal = padding_left + padding_right + border_left + border_right;

        // Get width value (may be auto)
        let specified_width = style
            .width
            .resolve_against(cb_width)
            .map(|w| content_size_from_box_sizing(w, total_horizontal, style.box_sizing));

        match (left, specified_width, right) {
            // Case 1: All three specified (overconstrained) - ignore right for LTR
            (Some(l), Some(w), Some(_r)) => {
                let x = l + margin_left + border_left + padding_left;
                Ok((x, w))
            }

            // Case 2: left and width specified, right is auto
            (Some(l), Some(w), None) => {
                let x = l + margin_left + border_left + padding_left;
                Ok((x, w))
            }

            // Case 3: right and width specified, left is auto
            (None, Some(w), Some(r)) => {
                let x = cb_width - r - margin_right - border_right - padding_right - w;
                Ok((x, w))
            }

            // Case 4: left and right specified, width is auto
            (Some(l), None, Some(r)) => {
                let available = cb_width - l - r - margin_left - margin_right - total_horizontal;
                let width = available.max(0.0);
                let x = l + margin_left + border_left + padding_left;
                Ok((x, width))
            }

            // Case 5: Only left specified
            (Some(l), None, None) => {
                let x = l + margin_left + border_left + padding_left;
                Ok((x, intrinsic_width))
            }

            // Case 6: Only right specified
            (None, None, Some(r)) => {
                let x = cb_width - r - margin_right - border_right - padding_right - intrinsic_width;
                Ok((x, intrinsic_width))
            }

            // Case 7: Only width specified - use static position
            (None, Some(w), None) => {
                // Static position - left edge aligns with where it would be in normal flow
                let x = margin_left + border_left + padding_left;
                Ok((x, w))
            }

            // Case 8: None specified - use static position and intrinsic width
            (None, None, None) => {
                let x = margin_left + border_left + padding_left;
                Ok((x, intrinsic_width))
            }
        }
    }

    /// Computes vertical position and height for absolutely positioned element
    fn compute_absolute_vertical(
        &self,
        style: &PositionedStyle,
        cb_height: f32,
        intrinsic_height: f32,
    ) -> Result<(f32, f32), LayoutError> {
        let top = resolve_offset(&style.top, cb_height);
        let bottom = resolve_offset(&style.bottom, cb_height);

        // Get margin values
        let margin_top = style.margin.top;
        let margin_bottom = style.margin.bottom;

        // Get padding and border
        let padding_top = style.padding.top;
        let padding_bottom = style.padding.bottom;
        let border_top = style.border_width.top;
        let border_bottom = style.border_width.bottom;

        let total_vertical = padding_top + padding_bottom + border_top + border_bottom;

        // Get height value (may be auto)
        let specified_height = style
            .height
            .resolve_against(cb_height)
            .map(|h| content_size_from_box_sizing(h, total_vertical, style.box_sizing));

        match (top, specified_height, bottom) {
            // All three specified (overconstrained) - ignore bottom
            (Some(t), Some(h), Some(_b)) => {
                let y = t + margin_top + border_top + padding_top;
                Ok((y, h))
            }

            // top and height specified
            (Some(t), Some(h), None) => {
                let y = t + margin_top + border_top + padding_top;
                Ok((y, h))
            }

            // bottom and height specified
            (None, Some(h), Some(b)) => {
                let y = cb_height - b - margin_bottom - border_bottom - padding_bottom - h;
                Ok((y, h))
            }

            // top and bottom specified, height is auto
            (Some(t), None, Some(b)) => {
                let available = cb_height - t - b - margin_top - margin_bottom - total_vertical;
                let height = available.max(0.0);
                let y = t + margin_top + border_top + padding_top;
                Ok((y, height))
            }

            // Only top specified
            (Some(t), None, None) => {
                let y = t + margin_top + border_top + padding_top;
                Ok((y, intrinsic_height))
            }

            // Only bottom specified
            (None, None, Some(b)) => {
                let y = cb_height - b - margin_bottom - border_bottom - padding_bottom - intrinsic_height;
                Ok((y, intrinsic_height))
            }

            // Only height specified - use static position
            (None, Some(h), None) => {
                let y = margin_top + border_top + padding_top;
                Ok((y, h))
            }

            // None specified - use static position and intrinsic height
            (None, None, None) => {
                let y = margin_top + border_top + padding_top;
                Ok((y, intrinsic_height))
            }
        }
    }

    /// Determines the containing block for a positioned element
    ///
    /// CSS 2.1 Section 10.1:
    /// - static/relative: Content box of nearest block container ancestor
    /// - absolute: Padding box of nearest positioned ancestor
    /// - fixed: Viewport (initial containing block)
    /// - sticky: Content box of nearest block container ancestor
    ///
    /// # Arguments
    ///
    /// * `position` - The position type of the element
    /// * `viewport_size` - The viewport size for fixed positioning
    /// * `positioned_ancestor_rect` - The padding box of nearest positioned ancestor (if any)
    /// * `block_ancestor_rect` - The content box of nearest block container ancestor
    ///
    /// # Returns
    ///
    /// The containing block for the element
    pub fn determine_containing_block(
        &self,
        position: Position,
        viewport_size: Size,
        positioned_ancestor_rect: Option<Rect>,
        block_ancestor_rect: Option<Rect>,
    ) -> ContainingBlock {
        match position {
            Position::Static | Position::Relative | Position::Sticky => {
                // Use nearest block container ancestor, or viewport if none
                block_ancestor_rect
                    .map(ContainingBlock::new)
                    .unwrap_or_else(|| ContainingBlock::viewport(viewport_size))
            }
            Position::Absolute => {
                // Use nearest positioned ancestor, or viewport if none (initial containing block)
                positioned_ancestor_rect
                    .map(ContainingBlock::new)
                    .unwrap_or_else(|| ContainingBlock::viewport(viewport_size))
            }
            Position::Fixed => {
                // Always use viewport
                ContainingBlock::viewport(viewport_size)
            }
        }
    }

    /// Computes sticky constraints for a sticky positioned element
    ///
    /// Sticky positioning is a hybrid: the element is positioned relative
    /// until it crosses a threshold, then it becomes fixed.
    ///
    /// This method computes the constraints; the actual sticky behavior
    /// is handled during rendering based on scroll position.
    ///
    /// # Arguments
    ///
    /// * `style` - The computed style
    /// * `containing_block` - The containing block for percentage resolution
    ///
    /// # Returns
    ///
    /// Sticky constraints for the renderer
    pub fn compute_sticky_constraints(
        &self,
        style: &PositionedStyle,
        containing_block: &ContainingBlock,
    ) -> StickyConstraints {
        if !style.position.is_sticky() {
            return StickyConstraints::none();
        }

        StickyConstraints::from_style(style, containing_block)
    }

    /// Checks if an element creates a stacking context
    ///
    /// A stacking context is created by:
    /// 1. Root element
    /// 2. Positioned element with z-index != auto
    /// 3. Element with opacity < 1
    /// 4. Element with transform, filter, etc.
    ///
    /// # Arguments
    ///
    /// * `style` - The computed style
    ///
    /// # Returns
    ///
    /// True if the element creates a stacking context
    pub fn creates_stacking_context(&self, style: &PositionedStyle) -> bool {
        // Positioned with z-index
        if style.position.is_positioned() && style.z_index.is_some() {
            return true;
        }

        // Opacity < 1
        if style.opacity < 1.0 {
            return true;
        }

        // TODO: Add transform, filter, isolation checks when those properties are available

        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::geometry::EdgeOffsets;
    use crate::style::computed::PositionedStyle;

    use crate::style::values::LengthOrAuto;

    fn default_style() -> PositionedStyle {
        let mut style = PositionedStyle::default();
        // Reset border width to 0 for cleaner test expectations
        // (default is medium = 3px)
        style.border_width = EdgeOffsets::ZERO;
        style
    }

    fn create_fragment(x: f32, y: f32, width: f32, height: f32) -> FragmentNode {
        FragmentNode::new_block(Rect::from_xywh(x, y, width, height), vec![])
    }

    fn create_containing_block(width: f32, height: f32) -> ContainingBlock {
        ContainingBlock::viewport(Size::new(width, height))
    }

    // ========== ContainingBlock tests ==========

    #[test]
    fn test_containing_block_creation() {
        let cb = ContainingBlock::new(Rect::from_xywh(10.0, 20.0, 100.0, 50.0));
        assert_eq!(cb.width(), 100.0);
        assert_eq!(cb.height(), 50.0);
        assert_eq!(cb.origin(), Point::new(10.0, 20.0));
    }

    #[test]
    fn test_containing_block_viewport() {
        let cb = ContainingBlock::viewport(Size::new(800.0, 600.0));
        assert_eq!(cb.width(), 800.0);
        assert_eq!(cb.height(), 600.0);
        assert_eq!(cb.origin(), Point::ZERO);
    }

    // ========== StickyConstraints tests ==========

    #[test]
    fn test_sticky_constraints_none() {
        let constraints = StickyConstraints::none();
        assert!(!constraints.has_constraints());
    }

    #[test]
    fn test_sticky_constraints_from_style() {
        let mut style = default_style();
        style.position = Position::Sticky;
        style.top = LengthOrAuto::px(10.0);
        style.left = LengthOrAuto::percent(5.0);

        let cb = create_containing_block(800.0, 600.0);
        let constraints = StickyConstraints::from_style(&style, &cb);

        assert_eq!(constraints.top, Some(10.0));
        assert_eq!(constraints.left, Some(40.0)); // 5% of 800
        assert!(constraints.has_constraints());
    }

    // ========== Relative positioning tests ==========

    #[test]
    fn test_relative_position_no_offset() {
        let layout = PositionedLayout::new();
        let fragment = create_fragment(100.0, 100.0, 200.0, 150.0);

        let mut style = default_style();
        style.position = Position::Relative;

        let cb = create_containing_block(800.0, 600.0);
        let result = layout.apply_relative_positioning(&fragment, &style, &cb).unwrap();

        // No offset specified, position should be unchanged
        assert_eq!(result.bounds.x(), 100.0);
        assert_eq!(result.bounds.y(), 100.0);
    }

    #[test]
    fn test_relative_position_with_top_left() {
        let layout = PositionedLayout::new();
        let fragment = create_fragment(100.0, 100.0, 200.0, 150.0);

        let mut style = default_style();
        style.position = Position::Relative;
        style.top = LengthOrAuto::px(20.0);
        style.left = LengthOrAuto::px(30.0);

        let cb = create_containing_block(800.0, 600.0);
        let result = layout.apply_relative_positioning(&fragment, &style, &cb).unwrap();

        // Should be offset by (30, 20)
        assert_eq!(result.bounds.x(), 130.0); // 100 + 30
        assert_eq!(result.bounds.y(), 120.0); // 100 + 20
    }

    #[test]
    fn test_relative_position_with_bottom_right() {
        let layout = PositionedLayout::new();
        let fragment = create_fragment(100.0, 100.0, 200.0, 150.0);

        let mut style = default_style();
        style.position = Position::Relative;
        style.bottom = LengthOrAuto::px(20.0);
        style.right = LengthOrAuto::px(30.0);

        let cb = create_containing_block(800.0, 600.0);
        let result = layout.apply_relative_positioning(&fragment, &style, &cb).unwrap();

        // Should be offset by (-30, -20)
        assert_eq!(result.bounds.x(), 70.0); // 100 - 30
        assert_eq!(result.bounds.y(), 80.0); // 100 - 20
    }

    #[test]
    fn test_relative_position_top_overrides_bottom() {
        let layout = PositionedLayout::new();
        let fragment = create_fragment(0.0, 0.0, 100.0, 100.0);

        let mut style = default_style();
        style.position = Position::Relative;
        style.top = LengthOrAuto::px(10.0);
        style.bottom = LengthOrAuto::px(20.0); // Should be ignored

        let cb = create_containing_block(800.0, 600.0);
        let result = layout.apply_relative_positioning(&fragment, &style, &cb).unwrap();

        // Only top should be applied
        assert_eq!(result.bounds.y(), 10.0);
    }

    #[test]
    fn test_relative_position_left_overrides_right() {
        let layout = PositionedLayout::new();
        let fragment = create_fragment(0.0, 0.0, 100.0, 100.0);

        let mut style = default_style();
        style.position = Position::Relative;
        style.left = LengthOrAuto::px(10.0);
        style.right = LengthOrAuto::px(20.0); // Should be ignored

        let cb = create_containing_block(800.0, 600.0);
        let result = layout.apply_relative_positioning(&fragment, &style, &cb).unwrap();

        // Only left should be applied
        assert_eq!(result.bounds.x(), 10.0);
    }

    #[test]
    fn test_relative_position_with_percentage() {
        let layout = PositionedLayout::new();
        let fragment = create_fragment(0.0, 0.0, 100.0, 100.0);

        let mut style = default_style();
        style.position = Position::Relative;
        style.top = LengthOrAuto::percent(10.0); // 10% of 600 = 60
        style.left = LengthOrAuto::percent(5.0); // 5% of 800 = 40

        let cb = create_containing_block(800.0, 600.0);
        let result = layout.apply_relative_positioning(&fragment, &style, &cb).unwrap();

        assert_eq!(result.bounds.x(), 40.0);
        assert_eq!(result.bounds.y(), 60.0);
    }

    #[test]
    fn test_relative_position_negative_offset() {
        let layout = PositionedLayout::new();
        let fragment = create_fragment(100.0, 100.0, 100.0, 100.0);

        let mut style = default_style();
        style.position = Position::Relative;
        style.top = LengthOrAuto::px(-20.0);
        style.left = LengthOrAuto::px(-30.0);

        let cb = create_containing_block(800.0, 600.0);
        let result = layout.apply_relative_positioning(&fragment, &style, &cb).unwrap();

        assert_eq!(result.bounds.x(), 70.0); // 100 - 30
        assert_eq!(result.bounds.y(), 80.0); // 100 - 20
    }

    #[test]
    fn test_non_relative_position_unchanged() {
        let layout = PositionedLayout::new();
        let fragment = create_fragment(100.0, 100.0, 200.0, 150.0);

        let mut style = default_style();
        style.position = Position::Static;
        style.top = LengthOrAuto::px(20.0);
        style.left = LengthOrAuto::px(30.0);

        let cb = create_containing_block(800.0, 600.0);
        let result = layout.apply_relative_positioning(&fragment, &style, &cb).unwrap();

        // Static position - offsets should not be applied
        assert_eq!(result.bounds.x(), 100.0);
        assert_eq!(result.bounds.y(), 100.0);
    }

    // ========== Absolute positioning tests ==========

    #[test]
    fn test_absolute_position_with_left_and_width() {
        let layout = PositionedLayout::new();

        let mut style = default_style();
        style.position = Position::Absolute;
        style.left = LengthOrAuto::px(50.0);
        style.width = LengthOrAuto::px(200.0);

        let cb = create_containing_block(800.0, 600.0);
        let intrinsic = Size::new(100.0, 100.0);

        let (pos, size) = layout.compute_absolute_position(&style, &cb, intrinsic).unwrap();

        assert_eq!(pos.x, 50.0);
        assert_eq!(size.width, 200.0);
    }

    #[test]
    fn test_absolute_position_with_left_and_right() {
        let layout = PositionedLayout::new();

        let mut style = default_style();
        style.position = Position::Absolute;
        style.left = LengthOrAuto::px(50.0);
        style.right = LengthOrAuto::px(50.0);
        // width is auto, should be computed

        let cb = create_containing_block(400.0, 600.0);
        let intrinsic = Size::new(100.0, 100.0);

        let (pos, size) = layout.compute_absolute_position(&style, &cb, intrinsic).unwrap();

        // Width should be: 400 - 50 - 50 = 300
        assert_eq!(pos.x, 50.0);
        assert_eq!(size.width, 300.0);
    }

    #[test]
    fn test_absolute_position_with_right_and_width() {
        let layout = PositionedLayout::new();

        let mut style = default_style();
        style.position = Position::Absolute;
        style.right = LengthOrAuto::px(50.0);
        style.width = LengthOrAuto::px(200.0);

        let cb = create_containing_block(400.0, 600.0);
        let intrinsic = Size::new(100.0, 100.0);

        let (pos, size) = layout.compute_absolute_position(&style, &cb, intrinsic).unwrap();

        // x = 400 - 50 - 200 = 150
        assert_eq!(pos.x, 150.0);
        assert_eq!(size.width, 200.0);
    }

    #[test]
    fn test_absolute_position_with_top_and_height() {
        let layout = PositionedLayout::new();

        let mut style = default_style();
        style.position = Position::Absolute;
        style.top = LengthOrAuto::px(30.0);
        style.height = LengthOrAuto::px(100.0);

        let cb = create_containing_block(800.0, 600.0);
        let intrinsic = Size::new(100.0, 50.0);

        let (pos, size) = layout.compute_absolute_position(&style, &cb, intrinsic).unwrap();

        assert_eq!(pos.y, 30.0);
        assert_eq!(size.height, 100.0);
    }

    #[test]
    fn test_absolute_position_with_top_and_bottom() {
        let layout = PositionedLayout::new();

        let mut style = default_style();
        style.position = Position::Absolute;
        style.top = LengthOrAuto::px(50.0);
        style.bottom = LengthOrAuto::px(50.0);
        // height is auto

        let cb = create_containing_block(800.0, 400.0);
        let intrinsic = Size::new(100.0, 50.0);

        let (pos, size) = layout.compute_absolute_position(&style, &cb, intrinsic).unwrap();

        // Height should be: 400 - 50 - 50 = 300
        assert_eq!(pos.y, 50.0);
        assert_eq!(size.height, 300.0);
    }

    #[test]
    fn test_absolute_position_uses_intrinsic_when_auto() {
        let layout = PositionedLayout::new();

        let style = default_style();
        let cb = create_containing_block(800.0, 600.0);
        let intrinsic = Size::new(150.0, 75.0);

        let (_pos, size) = layout.compute_absolute_position(&style, &cb, intrinsic).unwrap();

        // Should use intrinsic size when everything is auto
        assert_eq!(size.width, 150.0);
        assert_eq!(size.height, 75.0);
    }

    // ========== Containing block determination tests ==========

    #[test]
    fn test_determine_containing_block_static() {
        let layout = PositionedLayout::new();
        let viewport = Size::new(1024.0, 768.0);
        let block_rect = Rect::from_xywh(10.0, 10.0, 500.0, 400.0);

        let cb = layout.determine_containing_block(Position::Static, viewport, None, Some(block_rect));

        assert_eq!(cb.rect, block_rect);
    }

    #[test]
    fn test_determine_containing_block_absolute() {
        let layout = PositionedLayout::new();
        let viewport = Size::new(1024.0, 768.0);
        let positioned_rect = Rect::from_xywh(20.0, 20.0, 300.0, 200.0);

        let cb = layout.determine_containing_block(Position::Absolute, viewport, Some(positioned_rect), None);

        assert_eq!(cb.rect, positioned_rect);
    }

    #[test]
    fn test_determine_containing_block_absolute_no_ancestor() {
        let layout = PositionedLayout::new();
        let viewport = Size::new(1024.0, 768.0);

        let cb = layout.determine_containing_block(Position::Absolute, viewport, None, None);

        // Should fall back to viewport
        assert_eq!(cb.rect.size, viewport);
    }

    #[test]
    fn test_determine_containing_block_fixed() {
        let layout = PositionedLayout::new();
        let viewport = Size::new(1024.0, 768.0);
        let positioned_rect = Some(Rect::from_xywh(20.0, 20.0, 300.0, 200.0));
        let block_rect = Some(Rect::from_xywh(10.0, 10.0, 500.0, 400.0));

        let cb = layout.determine_containing_block(Position::Fixed, viewport, positioned_rect, block_rect);

        // Fixed always uses viewport regardless of ancestors
        assert_eq!(cb.rect.size, viewport);
        assert_eq!(cb.rect.origin, Point::ZERO);
    }

    #[test]
    fn test_determine_containing_block_sticky() {
        let layout = PositionedLayout::new();
        let viewport = Size::new(1024.0, 768.0);
        let block_rect = Rect::from_xywh(10.0, 10.0, 500.0, 400.0);

        let cb = layout.determine_containing_block(Position::Sticky, viewport, None, Some(block_rect));

        // Sticky uses block container like relative
        assert_eq!(cb.rect, block_rect);
    }

    // ========== Stacking context tests ==========

    #[test]
    fn test_creates_stacking_context_positioned_with_z_index() {
        let layout = PositionedLayout::new();

        let mut style = default_style();
        style.position = Position::Relative;
        style.z_index = Some(1);

        assert!(layout.creates_stacking_context(&style));
    }

    #[test]
    fn test_creates_stacking_context_positioned_without_z_index() {
        let layout = PositionedLayout::new();

        let mut style = default_style();
        style.position = Position::Relative;

        assert!(!layout.creates_stacking_context(&style));
    }

    #[test]
    fn test_creates_stacking_context_opacity() {
        let layout = PositionedLayout::new();

        let mut style = default_style();
        style.opacity = 0.5;

        assert!(layout.creates_stacking_context(&style));
    }

    #[test]
    fn test_creates_stacking_context_static_no_z_index() {
        let layout = PositionedLayout::new();

        let mut style = default_style();
        style.position = Position::Static;
        style.z_index = Some(5);

        // Static position doesn't create stacking context even with z-index
        assert!(!layout.creates_stacking_context(&style));
    }

    // ========== Resolve offset helper tests ==========

    #[test]
    fn test_resolve_offset_auto() {
        let value = LengthOrAuto::Auto;
        assert_eq!(resolve_offset(&value, 100.0), None);
    }

    #[test]
    fn test_resolve_offset_px() {
        let value = LengthOrAuto::px(50.0);
        assert_eq!(resolve_offset(&value, 100.0), Some(50.0));
    }

    #[test]
    fn test_resolve_offset_percent() {
        let value = LengthOrAuto::percent(25.0);
        assert_eq!(resolve_offset(&value, 200.0), Some(50.0));
    }
}
