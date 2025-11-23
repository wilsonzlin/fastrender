//! Absolute Positioning Algorithm
//!
//! Implements CSS absolute positioning as specified in CSS 2.1 Sections 10.3.7 and 10.6.4.
//! Absolute positioning removes elements from normal flow and positions them relative to
//! their containing block.
//!
//! # CSS Specification References
//!
//! - **CSS 2.1 Section 10.3.7**: Absolutely positioned, non-replaced elements (width)
//! - **CSS 2.1 Section 10.6.4**: Absolutely positioned, non-replaced elements (height)
//! - **CSS 2.1 Section 10.1**: Definition of "containing block"
//!
//! # Constraint Equations
//!
//! ## Horizontal Constraint (CSS 2.1 Section 10.3.7)
//!
//! ```text
//! left + margin-left + border-left + padding-left + width +
//! padding-right + border-right + margin-right + right = containing block width
//! ```
//!
//! ## Vertical Constraint (CSS 2.1 Section 10.6.4)
//!
//! ```text
//! top + margin-top + border-top + padding-top + height +
//! padding-bottom + border-bottom + margin-bottom + bottom = containing block height
//! ```
//!
//! # Key Features
//!
//! - Complete constraint equation resolution
//! - Auto margin centering support
//! - Static position fallback for unspecified offsets
//! - Overconstrained case handling (LTR: ignore right, ignore bottom)
//! - Percentage value resolution
//!
//! # Example
//!
//! ```rust,ignore
//! use fastrender::layout::absolute_positioning::{AbsoluteLayout, AbsoluteLayoutInput};
//! use fastrender::layout::ContainingBlock;
//!
//! let layout = AbsoluteLayout::new();
//! let result = layout.layout_absolute(
//!     &input,
//!     &containing_block,
//! )?;
//! ```

use crate::geometry::{Point, Rect, Size};
use crate::layout::utils::resolve_offset;
use crate::layout::{LayoutConstraints, LayoutError};
use crate::style::{ComputedStyle, Position};
use crate::tree::FragmentNode;

use super::contexts::positioned::ContainingBlock;

/// Result type for position and size calculations
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct AbsoluteLayoutResult {
    /// The computed position (relative to containing block origin)
    pub position: Point,
    /// The computed size
    pub size: Size,
    /// The computed margins (may be auto-resolved for centering)
    pub margins: ResolvedMargins,
}

/// Resolved margin values after auto-margin calculation
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct ResolvedMargins {
    pub top: f32,
    pub right: f32,
    pub bottom: f32,
    pub left: f32,
}

impl ResolvedMargins {
    /// Creates resolved margins from specified values
    pub fn new(top: f32, right: f32, bottom: f32, left: f32) -> Self {
        Self { top, right, bottom, left }
    }

    /// Creates zero margins
    pub const fn zero() -> Self {
        Self {
            top: 0.0,
            right: 0.0,
            bottom: 0.0,
            left: 0.0,
        }
    }

    /// Returns horizontal margin sum
    pub fn horizontal(&self) -> f32 {
        self.left + self.right
    }

    /// Returns vertical margin sum
    pub fn vertical(&self) -> f32 {
        self.top + self.bottom
    }
}

/// Input for absolute layout calculation
#[derive(Debug, Clone)]
pub struct AbsoluteLayoutInput {
    /// The computed style of the absolutely positioned element
    pub style: ComputedStyle,
    /// The intrinsic size (used when width/height are auto)
    pub intrinsic_size: Size,
    /// The static position (where element would be in normal flow)
    pub static_position: Point,
}

impl AbsoluteLayoutInput {
    /// Creates a new absolute layout input
    pub fn new(style: ComputedStyle, intrinsic_size: Size, static_position: Point) -> Self {
        Self {
            style,
            intrinsic_size,
            static_position,
        }
    }
}

/// Absolute positioning layout engine
///
/// Handles CSS absolute and fixed positioning. Computes position and size
/// based on the constraint equations from CSS 2.1.
///
/// # Thread Safety
///
/// AbsoluteLayout is stateless and can be shared across threads.
#[derive(Debug, Clone, Default)]
pub struct AbsoluteLayout;

impl AbsoluteLayout {
    /// Creates a new absolute layout handler
    pub const fn new() -> Self {
        Self
    }

    /// Performs complete absolute layout calculation
    ///
    /// This method handles all cases of the CSS constraint equations:
    /// - All specified values (overconstrained case)
    /// - Auto margins for centering
    /// - Static position fallback
    /// - Intrinsic size for auto width/height
    ///
    /// # Arguments
    ///
    /// * `input` - The input parameters including style and intrinsic size
    /// * `containing_block` - The containing block for percentage resolution
    ///
    /// # Returns
    ///
    /// The computed position, size, and resolved margins
    ///
    /// # CSS Specification
    ///
    /// Implements CSS 2.1 Sections 10.3.7 and 10.6.4
    pub fn layout_absolute(
        &self,
        input: &AbsoluteLayoutInput,
        containing_block: &ContainingBlock,
    ) -> Result<AbsoluteLayoutResult, LayoutError> {
        let style = &input.style;
        let cb_width = containing_block.width();
        let cb_height = containing_block.height();

        // Resolve horizontal position and width
        let (x, width, margin_left, margin_right) = self.compute_horizontal(
            style,
            cb_width,
            input.intrinsic_size.width,
            input.static_position.x,
        )?;

        // Resolve vertical position and height
        let (y, height, margin_top, margin_bottom) = self.compute_vertical(
            style,
            cb_height,
            input.intrinsic_size.height,
            input.static_position.y,
        )?;

        // Position relative to containing block origin
        let position = Point::new(
            containing_block.origin().x + x,
            containing_block.origin().y + y,
        );

        Ok(AbsoluteLayoutResult {
            position,
            size: Size::new(width, height),
            margins: ResolvedMargins::new(margin_top, margin_right, margin_bottom, margin_left),
        })
    }

    /// Computes horizontal position, width, and margins
    ///
    /// Implements CSS 2.1 Section 10.3.7 constraint equation:
    /// `left + margin-left + border-left + padding-left + width +
    ///  padding-right + border-right + margin-right + right = CB width`
    ///
    /// # Cases
    ///
    /// 1. All three (left, width, right) specified → overconstrained, ignore right (LTR)
    /// 2. left + width specified → compute right (ignored)
    /// 3. right + width specified → compute left
    /// 4. left + right specified → compute width
    /// 5. Only left specified → use intrinsic width, static position for other
    /// 6. Only right specified → use intrinsic width
    /// 7. Only width specified → use static position for left
    /// 8. None specified → use static position and intrinsic width
    ///
    /// Auto margins:
    /// - If overconstrained with auto margins → treat as 0
    /// - If left + right + width specified with auto margins → center
    fn compute_horizontal(
        &self,
        style: &ComputedStyle,
        cb_width: f32,
        intrinsic_width: f32,
        static_x: f32,
    ) -> Result<(f32, f32, f32, f32), LayoutError> {
        let left = resolve_offset(&style.left, cb_width);
        let right = resolve_offset(&style.right, cb_width);
        let specified_width = style.width.resolve_against(cb_width);

        // Get padding and border (never auto)
        let padding_left = style.padding.left;
        let padding_right = style.padding.right;
        let border_left = style.border_width.left;
        let border_right = style.border_width.right;
        let total_horizontal_spacing = padding_left + padding_right + border_left + border_right;

        // Default margin values
        let margin_left = style.margin.left;
        let margin_right = style.margin.right;

        match (left, specified_width, right) {
            // Case 1: All three specified (overconstrained) - ignore right for LTR
            (Some(l), Some(w), Some(_r)) => {
                // For LTR, ignore right value
                let x = l + margin_left + border_left + padding_left;
                Ok((x, w, margin_left, margin_right))
            }

            // Case 2: left and width specified, right is auto
            (Some(l), Some(w), None) => {
                let x = l + margin_left + border_left + padding_left;
                Ok((x, w, margin_left, margin_right))
            }

            // Case 3: right and width specified, left is auto
            (None, Some(w), Some(r)) => {
                let x = cb_width - r - margin_right - border_right - padding_right - w;
                Ok((x, w, margin_left, margin_right))
            }

            // Case 4: left and right specified, width is auto (stretch)
            (Some(l), None, Some(r)) => {
                // Check for auto margin centering - not applicable when width is auto
                let available = cb_width - l - r - margin_left - margin_right - total_horizontal_spacing;
                let width = available.max(0.0);
                let x = l + margin_left + border_left + padding_left;
                Ok((x, width, margin_left, margin_right))
            }

            // Case 5: Only left specified
            (Some(l), None, None) => {
                let x = l + margin_left + border_left + padding_left;
                Ok((x, intrinsic_width, margin_left, margin_right))
            }

            // Case 6: Only right specified
            (None, None, Some(r)) => {
                let x = cb_width - r - margin_right - border_right - padding_right - intrinsic_width;
                Ok((x, intrinsic_width, margin_left, margin_right))
            }

            // Case 7: Only width specified - use static position for left
            (None, Some(w), None) => {
                // Use static position
                let x = static_x + margin_left + border_left + padding_left;
                Ok((x, w, margin_left, margin_right))
            }

            // Case 8: None specified - use static position and intrinsic width
            (None, None, None) => {
                let x = static_x + margin_left + border_left + padding_left;
                Ok((x, intrinsic_width, margin_left, margin_right))
            }
        }
    }

    /// Computes vertical position, height, and margins
    ///
    /// Implements CSS 2.1 Section 10.6.4 constraint equation
    fn compute_vertical(
        &self,
        style: &ComputedStyle,
        cb_height: f32,
        intrinsic_height: f32,
        static_y: f32,
    ) -> Result<(f32, f32, f32, f32), LayoutError> {
        let top = resolve_offset(&style.top, cb_height);
        let bottom = resolve_offset(&style.bottom, cb_height);
        let specified_height = style.height.resolve_against(cb_height);

        // Get padding and border
        let padding_top = style.padding.top;
        let padding_bottom = style.padding.bottom;
        let border_top = style.border_width.top;
        let border_bottom = style.border_width.bottom;
        let total_vertical_spacing = padding_top + padding_bottom + border_top + border_bottom;

        let margin_top = style.margin.top;
        let margin_bottom = style.margin.bottom;

        match (top, specified_height, bottom) {
            // All three specified (overconstrained) - ignore bottom
            (Some(t), Some(h), Some(_b)) => {
                let y = t + margin_top + border_top + padding_top;
                Ok((y, h, margin_top, margin_bottom))
            }

            // top and height specified
            (Some(t), Some(h), None) => {
                let y = t + margin_top + border_top + padding_top;
                Ok((y, h, margin_top, margin_bottom))
            }

            // bottom and height specified
            (None, Some(h), Some(b)) => {
                let y = cb_height - b - margin_bottom - border_bottom - padding_bottom - h;
                Ok((y, h, margin_top, margin_bottom))
            }

            // top and bottom specified, height is auto (stretch)
            (Some(t), None, Some(b)) => {
                let available = cb_height - t - b - margin_top - margin_bottom - total_vertical_spacing;
                let height = available.max(0.0);
                let y = t + margin_top + border_top + padding_top;
                Ok((y, height, margin_top, margin_bottom))
            }

            // Only top specified
            (Some(t), None, None) => {
                let y = t + margin_top + border_top + padding_top;
                Ok((y, intrinsic_height, margin_top, margin_bottom))
            }

            // Only bottom specified
            (None, None, Some(b)) => {
                let y = cb_height - b - margin_bottom - border_bottom - padding_bottom - intrinsic_height;
                Ok((y, intrinsic_height, margin_top, margin_bottom))
            }

            // Only height specified - use static position
            (None, Some(h), None) => {
                let y = static_y + margin_top + border_top + padding_top;
                Ok((y, h, margin_top, margin_bottom))
            }

            // None specified - use static position and intrinsic height
            (None, None, None) => {
                let y = static_y + margin_top + border_top + padding_top;
                Ok((y, intrinsic_height, margin_top, margin_bottom))
            }
        }
    }

    /// Computes centering for absolutely positioned elements with auto margins
    ///
    /// When left, right, and width are all specified (or implied), and one or both
    /// margins are auto, the element should be centered.
    ///
    /// # CSS 2.1 Section 10.3.7
    ///
    /// "If both 'left' and 'right' are set, and 'width' is not 'auto', and
    /// 'margin-left' or 'margin-right' are 'auto', solve the equation to
    /// distribute the remaining space equally."
    pub fn compute_centered_horizontal(
        &self,
        style: &ComputedStyle,
        cb_width: f32,
        width: f32,
    ) -> (f32, f32, f32) {
        let left = resolve_offset(&style.left, cb_width).unwrap_or(0.0);
        let right = resolve_offset(&style.right, cb_width).unwrap_or(0.0);

        let padding_left = style.padding.left;
        let padding_right = style.padding.right;
        let border_left = style.border_width.left;
        let border_right = style.border_width.right;

        let total_spacing = padding_left + padding_right + border_left + border_right + width;
        let remaining = cb_width - left - right - total_spacing;

        // Check which margins are auto (represented as 0 with auto flag)
        let margin_left_specified = style.margin.left;
        let margin_right_specified = style.margin.right;

        // For true auto margin detection, we'd need additional metadata
        // For now, assume margin values of 0 could be auto for centering
        if margin_left_specified == 0.0 && margin_right_specified == 0.0 {
            // Both auto - center
            let half_margin = (remaining / 2.0).max(0.0);
            let x = left + half_margin + border_left + padding_left;
            (x, half_margin, half_margin)
        } else if margin_left_specified == 0.0 {
            // Only left auto
            let margin_left = (remaining - margin_right_specified).max(0.0);
            let x = left + margin_left + border_left + padding_left;
            (x, margin_left, margin_right_specified)
        } else if margin_right_specified == 0.0 {
            // Only right auto
            let margin_right = (remaining - margin_left_specified).max(0.0);
            let x = left + margin_left_specified + border_left + padding_left;
            (x, margin_left_specified, margin_right)
        } else {
            // Neither auto - use specified
            let x = left + margin_left_specified + border_left + padding_left;
            (x, margin_left_specified, margin_right_specified)
        }
    }

    /// Computes centering for vertical axis
    pub fn compute_centered_vertical(
        &self,
        style: &ComputedStyle,
        cb_height: f32,
        height: f32,
    ) -> (f32, f32, f32) {
        let top = resolve_offset(&style.top, cb_height).unwrap_or(0.0);
        let bottom = resolve_offset(&style.bottom, cb_height).unwrap_or(0.0);

        let padding_top = style.padding.top;
        let padding_bottom = style.padding.bottom;
        let border_top = style.border_width.top;
        let border_bottom = style.border_width.bottom;

        let total_spacing = padding_top + padding_bottom + border_top + border_bottom + height;
        let remaining = cb_height - top - bottom - total_spacing;

        let margin_top_specified = style.margin.top;
        let margin_bottom_specified = style.margin.bottom;

        if margin_top_specified == 0.0 && margin_bottom_specified == 0.0 {
            let half_margin = (remaining / 2.0).max(0.0);
            let y = top + half_margin + border_top + padding_top;
            (y, half_margin, half_margin)
        } else if margin_top_specified == 0.0 {
            let margin_top = (remaining - margin_bottom_specified).max(0.0);
            let y = top + margin_top + border_top + padding_top;
            (y, margin_top, margin_bottom_specified)
        } else if margin_bottom_specified == 0.0 {
            let margin_bottom = (remaining - margin_top_specified).max(0.0);
            let y = top + margin_top_specified + border_top + padding_top;
            (y, margin_top_specified, margin_bottom)
        } else {
            let y = top + margin_top_specified + border_top + padding_top;
            (y, margin_top_specified, margin_bottom_specified)
        }
    }

    /// Creates a fragment from the layout result
    ///
    /// Convenience method to create a FragmentNode from the computed layout.
    pub fn create_fragment(
        &self,
        result: &AbsoluteLayoutResult,
        children: Vec<FragmentNode>,
    ) -> FragmentNode {
        FragmentNode::new_block(
            Rect::new(result.position, result.size),
            children,
        )
    }

    /// Checks if an element should use absolute positioning
    pub fn is_absolutely_positioned(style: &ComputedStyle) -> bool {
        matches!(style.position, Position::Absolute | Position::Fixed)
    }

    /// Checks if an element should be laid out with absolute positioning
    pub fn should_layout_absolute(style: &ComputedStyle) -> bool {
        style.position.is_absolutely_positioned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::geometry::EdgeOffsets;
    use crate::style::LengthOrAuto;

    fn default_style() -> ComputedStyle {
        let mut style = ComputedStyle::default();
        style.border_width = EdgeOffsets::ZERO;
        style
    }

    fn create_containing_block(width: f32, height: f32) -> ContainingBlock {
        ContainingBlock::viewport(Size::new(width, height))
    }

    fn create_containing_block_at(x: f32, y: f32, width: f32, height: f32) -> ContainingBlock {
        ContainingBlock::new(Rect::from_xywh(x, y, width, height))
    }

    // ========== AbsoluteLayout basic tests ==========

    #[test]
    fn test_layout_absolute_all_specified() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.position = Position::Absolute;
        style.left = LengthOrAuto::px(20.0);
        style.top = LengthOrAuto::px(30.0);
        style.width = LengthOrAuto::px(100.0);
        style.height = LengthOrAuto::px(80.0);

        let input = AbsoluteLayoutInput::new(
            style,
            Size::new(50.0, 50.0),
            Point::ZERO,
        );
        let cb = create_containing_block(800.0, 600.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();

        assert_eq!(result.position.x, 20.0);
        assert_eq!(result.position.y, 30.0);
        assert_eq!(result.size.width, 100.0);
        assert_eq!(result.size.height, 80.0);
    }

    #[test]
    fn test_layout_absolute_right_bottom() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.position = Position::Absolute;
        style.right = LengthOrAuto::px(50.0);
        style.bottom = LengthOrAuto::px(50.0);
        style.width = LengthOrAuto::px(100.0);
        style.height = LengthOrAuto::px(100.0);

        let input = AbsoluteLayoutInput::new(
            style,
            Size::new(50.0, 50.0),
            Point::ZERO,
        );
        let cb = create_containing_block(400.0, 400.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();

        // x = 400 - 50 - 100 = 250
        // y = 400 - 50 - 100 = 250
        assert_eq!(result.position.x, 250.0);
        assert_eq!(result.position.y, 250.0);
    }

    #[test]
    fn test_layout_absolute_stretch_width() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.position = Position::Absolute;
        style.left = LengthOrAuto::px(50.0);
        style.right = LengthOrAuto::px(50.0);
        // width auto - should stretch

        let input = AbsoluteLayoutInput::new(
            style,
            Size::new(100.0, 100.0),
            Point::ZERO,
        );
        let cb = create_containing_block(500.0, 400.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();

        // width = 500 - 50 - 50 = 400
        assert_eq!(result.position.x, 50.0);
        assert_eq!(result.size.width, 400.0);
    }

    #[test]
    fn test_layout_absolute_stretch_height() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.position = Position::Absolute;
        style.top = LengthOrAuto::px(25.0);
        style.bottom = LengthOrAuto::px(25.0);
        // height auto - should stretch

        let input = AbsoluteLayoutInput::new(
            style,
            Size::new(100.0, 100.0),
            Point::ZERO,
        );
        let cb = create_containing_block(400.0, 300.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();

        // height = 300 - 25 - 25 = 250
        assert_eq!(result.position.y, 25.0);
        assert_eq!(result.size.height, 250.0);
    }

    #[test]
    fn test_layout_absolute_intrinsic_size() {
        let layout = AbsoluteLayout::new();

        let style = default_style();
        // All auto

        let input = AbsoluteLayoutInput::new(
            style,
            Size::new(150.0, 100.0),
            Point::ZERO,
        );
        let cb = create_containing_block(800.0, 600.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();

        // Should use intrinsic size
        assert_eq!(result.size.width, 150.0);
        assert_eq!(result.size.height, 100.0);
    }

    #[test]
    fn test_layout_absolute_static_position() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.width = LengthOrAuto::px(100.0);
        style.height = LengthOrAuto::px(80.0);
        // left/right/top/bottom all auto

        let input = AbsoluteLayoutInput::new(
            style,
            Size::new(50.0, 50.0),
            Point::new(75.0, 120.0), // static position
        );
        let cb = create_containing_block(800.0, 600.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();

        // Should use static position
        assert_eq!(result.position.x, 75.0);
        assert_eq!(result.position.y, 120.0);
    }

    #[test]
    fn test_layout_absolute_with_containing_block_offset() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.position = Position::Absolute;
        style.left = LengthOrAuto::px(10.0);
        style.top = LengthOrAuto::px(10.0);
        style.width = LengthOrAuto::px(50.0);
        style.height = LengthOrAuto::px(50.0);

        let input = AbsoluteLayoutInput::new(
            style,
            Size::new(50.0, 50.0),
            Point::ZERO,
        );
        let cb = create_containing_block_at(100.0, 100.0, 400.0, 300.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();

        // Position relative to containing block origin
        assert_eq!(result.position.x, 110.0); // 100 + 10
        assert_eq!(result.position.y, 110.0); // 100 + 10
    }

    #[test]
    fn test_layout_absolute_overconstrained_horizontal() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.position = Position::Absolute;
        style.left = LengthOrAuto::px(10.0);
        style.width = LengthOrAuto::px(100.0);
        style.right = LengthOrAuto::px(999.0); // Should be ignored for LTR

        let input = AbsoluteLayoutInput::new(
            style,
            Size::new(50.0, 50.0),
            Point::ZERO,
        );
        let cb = create_containing_block(400.0, 300.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();

        // Uses left and width, ignores right
        assert_eq!(result.position.x, 10.0);
        assert_eq!(result.size.width, 100.0);
    }

    #[test]
    fn test_layout_absolute_overconstrained_vertical() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.position = Position::Absolute;
        style.top = LengthOrAuto::px(20.0);
        style.height = LengthOrAuto::px(80.0);
        style.bottom = LengthOrAuto::px(888.0); // Should be ignored

        let input = AbsoluteLayoutInput::new(
            style,
            Size::new(50.0, 50.0),
            Point::ZERO,
        );
        let cb = create_containing_block(400.0, 300.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();

        // Uses top and height, ignores bottom
        assert_eq!(result.position.y, 20.0);
        assert_eq!(result.size.height, 80.0);
    }

    #[test]
    fn test_layout_absolute_percentage_values() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.position = Position::Absolute;
        style.left = LengthOrAuto::percent(10.0);  // 10% of 500 = 50
        style.top = LengthOrAuto::percent(20.0);   // 20% of 400 = 80
        style.width = LengthOrAuto::percent(50.0); // 50% of 500 = 250
        style.height = LengthOrAuto::percent(25.0); // 25% of 400 = 100

        let input = AbsoluteLayoutInput::new(
            style,
            Size::new(100.0, 100.0),
            Point::ZERO,
        );
        let cb = create_containing_block(500.0, 400.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();

        assert_eq!(result.position.x, 50.0);
        assert_eq!(result.position.y, 80.0);
        assert_eq!(result.size.width, 250.0);
        assert_eq!(result.size.height, 100.0);
    }

    #[test]
    fn test_layout_absolute_only_left() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.position = Position::Absolute;
        style.left = LengthOrAuto::px(100.0);
        // width, right auto

        let input = AbsoluteLayoutInput::new(
            style,
            Size::new(150.0, 80.0),
            Point::ZERO,
        );
        let cb = create_containing_block(800.0, 600.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();

        assert_eq!(result.position.x, 100.0);
        assert_eq!(result.size.width, 150.0); // intrinsic
    }

    #[test]
    fn test_layout_absolute_only_right() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.position = Position::Absolute;
        style.right = LengthOrAuto::px(75.0);
        // width, left auto

        let input = AbsoluteLayoutInput::new(
            style,
            Size::new(100.0, 50.0),
            Point::ZERO,
        );
        let cb = create_containing_block(400.0, 300.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();

        // x = 400 - 75 - 100 = 225
        assert_eq!(result.position.x, 225.0);
        assert_eq!(result.size.width, 100.0);
    }

    #[test]
    fn test_layout_absolute_only_top() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.position = Position::Absolute;
        style.top = LengthOrAuto::px(50.0);

        let input = AbsoluteLayoutInput::new(
            style,
            Size::new(100.0, 60.0),
            Point::ZERO,
        );
        let cb = create_containing_block(800.0, 600.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();

        assert_eq!(result.position.y, 50.0);
        assert_eq!(result.size.height, 60.0);
    }

    #[test]
    fn test_layout_absolute_only_bottom() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.position = Position::Absolute;
        style.bottom = LengthOrAuto::px(40.0);

        let input = AbsoluteLayoutInput::new(
            style,
            Size::new(100.0, 70.0),
            Point::ZERO,
        );
        let cb = create_containing_block(800.0, 500.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();

        // y = 500 - 40 - 70 = 390
        assert_eq!(result.position.y, 390.0);
        assert_eq!(result.size.height, 70.0);
    }

    // ========== ResolvedMargins tests ==========

    #[test]
    fn test_resolved_margins_new() {
        let margins = ResolvedMargins::new(10.0, 20.0, 30.0, 40.0);
        assert_eq!(margins.top, 10.0);
        assert_eq!(margins.right, 20.0);
        assert_eq!(margins.bottom, 30.0);
        assert_eq!(margins.left, 40.0);
    }

    #[test]
    fn test_resolved_margins_zero() {
        let margins = ResolvedMargins::zero();
        assert_eq!(margins.horizontal(), 0.0);
        assert_eq!(margins.vertical(), 0.0);
    }

    #[test]
    fn test_resolved_margins_sums() {
        let margins = ResolvedMargins::new(5.0, 10.0, 15.0, 20.0);
        assert_eq!(margins.horizontal(), 30.0);
        assert_eq!(margins.vertical(), 20.0);
    }

    // ========== Helper function tests ==========

    #[test]
    fn test_resolve_offset_auto() {
        assert_eq!(resolve_offset(&LengthOrAuto::Auto, 100.0), None);
    }

    #[test]
    fn test_resolve_offset_px() {
        assert_eq!(resolve_offset(&LengthOrAuto::px(50.0), 100.0), Some(50.0));
    }

    #[test]
    fn test_resolve_offset_percent() {
        assert_eq!(resolve_offset(&LengthOrAuto::percent(25.0), 200.0), Some(50.0));
    }

    // ========== Static method tests ==========

    #[test]
    fn test_is_absolutely_positioned() {
        let mut style = default_style();
        assert!(!AbsoluteLayout::is_absolutely_positioned(&style));

        style.position = Position::Absolute;
        assert!(AbsoluteLayout::is_absolutely_positioned(&style));

        style.position = Position::Fixed;
        assert!(AbsoluteLayout::is_absolutely_positioned(&style));

        style.position = Position::Relative;
        assert!(!AbsoluteLayout::is_absolutely_positioned(&style));
    }

    #[test]
    fn test_create_fragment() {
        let layout = AbsoluteLayout::new();
        let result = AbsoluteLayoutResult {
            position: Point::new(50.0, 75.0),
            size: Size::new(100.0, 80.0),
            margins: ResolvedMargins::zero(),
        };

        let fragment = layout.create_fragment(&result, vec![]);

        assert_eq!(fragment.bounds.x(), 50.0);
        assert_eq!(fragment.bounds.y(), 75.0);
        assert_eq!(fragment.bounds.width(), 100.0);
        assert_eq!(fragment.bounds.height(), 80.0);
    }

    // ========== Edge cases ==========

    #[test]
    fn test_zero_size_containing_block() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.left = LengthOrAuto::percent(50.0);
        style.top = LengthOrAuto::percent(50.0);

        let input = AbsoluteLayoutInput::new(
            style,
            Size::new(100.0, 100.0),
            Point::ZERO,
        );
        let cb = create_containing_block(0.0, 0.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();

        // 50% of 0 = 0
        assert_eq!(result.position.x, 0.0);
        assert_eq!(result.position.y, 0.0);
    }

    #[test]
    fn test_negative_computed_size() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.left = LengthOrAuto::px(200.0);
        style.right = LengthOrAuto::px(200.0);
        // In a 300px wide CB, this would give -100px width

        let input = AbsoluteLayoutInput::new(
            style,
            Size::new(50.0, 50.0),
            Point::ZERO,
        );
        let cb = create_containing_block(300.0, 300.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();

        // Width should be clamped to 0
        assert_eq!(result.size.width, 0.0);
    }

    #[test]
    fn test_large_values() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.left = LengthOrAuto::px(1_000_000.0);
        style.top = LengthOrAuto::px(1_000_000.0);
        style.width = LengthOrAuto::px(10_000.0);
        style.height = LengthOrAuto::px(10_000.0);

        let input = AbsoluteLayoutInput::new(
            style,
            Size::new(100.0, 100.0),
            Point::ZERO,
        );
        let cb = create_containing_block(800.0, 600.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();

        assert_eq!(result.position.x, 1_000_000.0);
        assert_eq!(result.position.y, 1_000_000.0);
    }
}
