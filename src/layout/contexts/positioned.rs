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

use crate::geometry::Point;
use crate::geometry::Rect;
use crate::geometry::Size;
use crate::layout::absolute_positioning::AbsoluteLayout;
use crate::layout::absolute_positioning::AbsoluteLayoutInput;
use crate::layout::formatting_context::LayoutError;
use crate::layout::utils::resolve_offset_for_positioned;
use crate::style::computed::PositionedStyle;
use crate::style::position::Position;
use crate::text::font_loader::FontContext;
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
  viewport_size: Size,
  inline_percentage_base: Option<f32>,
  block_percentage_base: Option<f32>,
}

impl ContainingBlock {
  /// Creates a new containing block from a rectangle
  pub const fn new(rect: Rect) -> Self {
    Self::with_viewport_and_bases(
      rect,
      rect.size,
      Some(rect.size.width),
      Some(rect.size.height),
    )
  }

  /// Creates a containing block from a rectangle and an explicit viewport size
  pub const fn with_viewport(rect: Rect, viewport_size: Size) -> Self {
    Self::with_viewport_and_bases(
      rect,
      viewport_size,
      Some(rect.size.width),
      Some(rect.size.height),
    )
  }

  /// Creates a containing block with explicit percentage bases.
  ///
  /// When the containing block's inline/block sizes are auto (content-driven),
  /// the percentage bases should be set to `None` so percentage offsets resolve
  /// to `auto` per CSS 2.1 §10.5/10.6 instead of being treated as 0px.
  pub const fn with_viewport_and_bases(
    rect: Rect,
    viewport_size: Size,
    inline_base: Option<f32>,
    block_base: Option<f32>,
  ) -> Self {
    Self {
      rect,
      viewport_size,
      inline_percentage_base: inline_base,
      block_percentage_base: block_base,
    }
  }

  /// Creates a containing block from position and size
  pub fn from_origin_size(origin: Point, size: Size) -> Self {
    Self::with_viewport_and_bases(
      Rect::new(origin, size),
      size,
      Some(size.width),
      Some(size.height),
    )
  }

  /// Creates a containing block representing the viewport
  ///
  /// Used for fixed positioning and as the initial containing block.
  pub fn viewport(size: Size) -> Self {
    Self::with_viewport_and_bases(
      Rect::new(Point::ZERO, size),
      size,
      Some(size.width),
      Some(size.height),
    )
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

  /// Returns the viewport size associated with this containing block.
  ///
  /// For viewport CBs this is the viewport; for other CBs this falls back
  /// to the rect size when no explicit viewport is available.
  pub fn viewport_size(&self) -> Size {
    self.viewport_size
  }

  /// Percentage base for inline-axis offsets (Some(width) when definite).
  pub fn inline_percentage_base(&self) -> Option<f32> {
    self.inline_percentage_base
  }

  /// Percentage base for block-axis offsets (None when block-size is auto).
  pub fn block_percentage_base(&self) -> Option<f32> {
    self.block_percentage_base
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
  pub fn from_style(
    style: &PositionedStyle,
    containing_block: &ContainingBlock,
    font_context: &FontContext,
  ) -> Self {
    let viewport = containing_block.viewport_size();
    let inline_base = containing_block.inline_percentage_base();
    let block_base = containing_block.block_percentage_base();
    Self {
      top: resolve_offset_for_positioned(&style.top, block_base, viewport, style, font_context),
      right: resolve_offset_for_positioned(
        &style.right,
        inline_base,
        viewport,
        style,
        font_context,
      ),
      bottom: resolve_offset_for_positioned(
        &style.bottom,
        block_base,
        viewport,
        style,
        font_context,
      ),
      left: resolve_offset_for_positioned(&style.left, inline_base, viewport, style, font_context),
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
#[derive(Clone)]
pub struct PositionedLayout {
  font_context: FontContext,
}

impl Default for PositionedLayout {
  fn default() -> Self {
    Self::new()
  }
}

impl PositionedLayout {
  /// Creates a new positioned layout handler
  pub fn new() -> Self {
    Self {
      font_context: FontContext::new(),
    }
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

    // Create new fragment with adjusted bounds but same children and style.
    // Note: Children positions are relative to parent, so they don't change
    let mut adjusted = fragment.clone();
    adjusted.bounds = new_bounds;
    Ok(adjusted)
  }

  /// Computes the offset for relative positioning
  ///
  /// CSS 2.1 Section 9.3.2:
  /// - If both `top` and `bottom` are specified, `top` wins
  /// - If both `left` and `right` are specified, `left` wins (LTR)
  fn compute_relative_offset(
    &self,
    style: &PositionedStyle,
    containing_block: &ContainingBlock,
  ) -> Point {
    let mut offset_x = 0.0;
    let mut offset_y = 0.0;
    let viewport = containing_block.viewport_size();
    let inline_base = containing_block.inline_percentage_base();
    let block_base = containing_block.block_percentage_base();

    // Vertical offset: top takes precedence over bottom
    if let Some(top) =
      resolve_offset_for_positioned(&style.top, block_base, viewport, style, &self.font_context)
    {
      offset_y = top;
    } else if let Some(bottom) = resolve_offset_for_positioned(
      &style.bottom,
      block_base,
      viewport,
      style,
      &self.font_context,
    ) {
      offset_y = -bottom;
    }

    // Horizontal offset: left takes precedence over right (LTR)
    if let Some(left) = resolve_offset_for_positioned(
      &style.left,
      inline_base,
      viewport,
      style,
      &self.font_context,
    ) {
      offset_x = left;
    } else if let Some(right) = resolve_offset_for_positioned(
      &style.right,
      inline_base,
      viewport,
      style,
      &self.font_context,
    ) {
      offset_x = -right;
    }

    Point::new(offset_x, offset_y)
  }

  /// Computes the position and size for an absolutely positioned element.
  ///
  /// This delegates to the shared AbsoluteLayout engine so consumers of
  /// PositionedLayout get the same shrink-to-fit and auto-margin behavior
  /// used by the main layout pipeline. The static position defaults to the
  /// origin because this helper does not yet receive the box's normal-flow
  /// position.
  pub fn compute_absolute_position(
    &self,
    style: &PositionedStyle,
    containing_block: &ContainingBlock,
    intrinsic_size: Size,
  ) -> Result<(Point, Size), LayoutError> {
    let abs = AbsoluteLayout::with_font_context(self.font_context.clone());
    let input = AbsoluteLayoutInput::new(style.clone(), intrinsic_size, Point::ZERO);
    let result = abs.layout_absolute(&input, containing_block)?;
    Ok((result.position, result.size))
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
          .map(|rect| {
            let block_base = if rect.size.height > 0.0 {
              Some(rect.size.height)
            } else {
              None
            };
            ContainingBlock::with_viewport_and_bases(
              rect,
              viewport_size,
              Some(rect.size.width),
              block_base,
            )
          })
          .unwrap_or_else(|| ContainingBlock::viewport(viewport_size))
      }
      Position::Absolute => {
        // Use nearest positioned ancestor, or viewport if none (initial containing block)
        positioned_ancestor_rect
          .map(|rect| {
            let block_base = if rect.size.height > 0.0 {
              Some(rect.size.height)
            } else {
              None
            };
            ContainingBlock::with_viewport_and_bases(
              rect,
              viewport_size,
              Some(rect.size.width),
              block_base,
            )
          })
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

    StickyConstraints::from_style(style, containing_block, &self.font_context)
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
    style.creates_stacking_context()
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::geometry::EdgeOffsets;
  use crate::layout::utils::resolve_offset;
  use crate::style::computed::PositionedStyle;
  use crate::style::values::Length;
  use crate::style::values::LengthOrAuto;
  use crate::style::values::LengthUnit;
  use crate::text::font_loader::FontContext;

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
    let constraints = StickyConstraints::from_style(&style, &cb, &FontContext::new());

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
    let result = layout
      .apply_relative_positioning(&fragment, &style, &cb)
      .unwrap();

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
    let result = layout
      .apply_relative_positioning(&fragment, &style, &cb)
      .unwrap();

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
    let result = layout
      .apply_relative_positioning(&fragment, &style, &cb)
      .unwrap();

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
    let result = layout
      .apply_relative_positioning(&fragment, &style, &cb)
      .unwrap();

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
    let result = layout
      .apply_relative_positioning(&fragment, &style, &cb)
      .unwrap();

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
    let result = layout
      .apply_relative_positioning(&fragment, &style, &cb)
      .unwrap();

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
    let result = layout
      .apply_relative_positioning(&fragment, &style, &cb)
      .unwrap();

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
    let result = layout
      .apply_relative_positioning(&fragment, &style, &cb)
      .unwrap();

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

    let (pos, size) = layout
      .compute_absolute_position(&style, &cb, intrinsic)
      .unwrap();

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
    // width is auto, should shrink to fit intrinsic size when both insets are specified

    let cb = create_containing_block(400.0, 600.0);
    let intrinsic = Size::new(100.0, 100.0);

    let (pos, size) = layout
      .compute_absolute_position(&style, &cb, intrinsic)
      .unwrap();

    // When left/right are both set and width is auto, the available space is
    // width = cb.width - left - right = 300. Shrink-to-fit clamps intrinsic to available.
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

    let (pos, size) = layout
      .compute_absolute_position(&style, &cb, intrinsic)
      .unwrap();

    // x = 400 - 50 - 200 = 150
    assert_eq!(pos.x, 150.0);
    assert_eq!(size.width, 200.0);
  }

  #[test]
  fn test_absolute_position_width_with_viewport_units() {
    let layout = PositionedLayout::new();

    let mut style = default_style();
    style.position = Position::Absolute;
    style.width = LengthOrAuto::Length(Length::new(50.0, LengthUnit::Vw));

    let cb = create_containing_block(200.0, 150.0);
    let intrinsic = Size::new(10.0, 10.0);

    let (_pos, size) = layout
      .compute_absolute_position(&style, &cb, intrinsic)
      .unwrap();

    // 50vw of a 200px viewport = 100px
    assert!((size.width - 100.0).abs() < 0.01);
  }

  #[test]
  fn test_absolute_position_width_with_font_relative_units() {
    let layout = PositionedLayout::new();

    let mut style = default_style();
    style.position = Position::Absolute;
    style.font_size = 18.0;
    style.root_font_size = 18.0;
    style.width = LengthOrAuto::Length(Length::em(2.0));

    let cb = create_containing_block(300.0, 200.0);
    let intrinsic = Size::new(5.0, 5.0);

    let (_pos, size) = layout
      .compute_absolute_position(&style, &cb, intrinsic)
      .unwrap();

    assert!((size.width - 36.0).abs() < 0.01);
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

    let (pos, size) = layout
      .compute_absolute_position(&style, &cb, intrinsic)
      .unwrap();

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

    let (pos, size) = layout
      .compute_absolute_position(&style, &cb, intrinsic)
      .unwrap();

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

    let (_pos, size) = layout
      .compute_absolute_position(&style, &cb, intrinsic)
      .unwrap();

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

    let cb =
      layout.determine_containing_block(Position::Absolute, viewport, Some(positioned_rect), None);

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

    let cb =
      layout.determine_containing_block(Position::Fixed, viewport, positioned_rect, block_rect);

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
  fn test_creates_stacking_context_transform() {
    let layout = PositionedLayout::new();

    let mut style = default_style();
    style.transform.push(crate::css::types::Transform::Rotate(
      std::f32::consts::FRAC_PI_2,
    ));

    assert!(layout.creates_stacking_context(&style));
  }

  #[test]
  fn test_creates_stacking_context_will_change() {
    let layout = PositionedLayout::new();

    let mut style = default_style();
    style.will_change =
      crate::style::types::WillChange::Hints(vec![crate::style::types::WillChangeHint::Property(
        "opacity".into(),
      )]);

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
    assert_eq!(
      resolve_offset(&value, 100.0, Size::new(800.0, 600.0), 16.0, 16.0),
      None
    );
  }

  #[test]
  fn test_resolve_offset_px() {
    let value = LengthOrAuto::px(50.0);
    assert_eq!(
      resolve_offset(&value, 100.0, Size::new(800.0, 600.0), 16.0, 16.0),
      Some(50.0)
    );
  }

  #[test]
  fn test_resolve_offset_percent() {
    let value = LengthOrAuto::percent(25.0);
    assert_eq!(
      resolve_offset(&value, 200.0, Size::new(800.0, 600.0), 16.0, 16.0),
      Some(50.0)
    );
  }
}
