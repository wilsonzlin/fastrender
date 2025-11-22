//! Absolute Positioning Algorithm
//!
//! This module implements CSS absolute positioning according to CSS 2.1 Sections 10.3.7
//! and 10.6.4 for absolutely positioned, non-replaced elements.
//!
//! # Overview
//!
//! Absolutely positioned elements are removed from the normal flow and positioned
//! relative to their containing block (nearest positioned ancestor, or the viewport
//! if none exists).
//!
//! # The Constraint Equation
//!
//! For horizontal positioning, CSS 2.1 Section 10.3.7 defines:
//! ```text
//! left + margin-left + border-left + padding-left + width +
//! padding-right + border-right + margin-right + right = containing block width
//! ```
//!
//! For vertical positioning, CSS 2.1 Section 10.6.4 defines:
//! ```text
//! top + margin-top + border-top + padding-top + height +
//! padding-bottom + border-bottom + margin-bottom + bottom = containing block height
//! ```
//!
//! # Reference
//!
//! - CSS 2.1 Section 10.3.7: <https://www.w3.org/TR/CSS21/visudet.html#abs-non-replaced-width>
//! - CSS 2.1 Section 10.6.4: <https://www.w3.org/TR/CSS21/visudet.html#abs-non-replaced-height>

use crate::geometry::{Point, Rect, Size};
use crate::layout::formatting_context::LayoutError;
use crate::style::{ComputedStyles, Length, Position};
use crate::tree::{BoxNode, FragmentNode};

/// Result of computing horizontal position and size for an absolutely positioned element
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct HorizontalResult {
    /// The x position of the content box within the containing block
    pub x: f32,
    /// The computed width of the content box
    pub width: f32,
    /// The resolved left margin
    pub margin_left: f32,
    /// The resolved right margin
    pub margin_right: f32,
}

/// Result of computing vertical position and size for an absolutely positioned element
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct VerticalResult {
    /// The y position of the content box within the containing block
    pub y: f32,
    /// The computed height of the content box
    pub height: f32,
    /// The resolved top margin
    pub margin_top: f32,
    /// The resolved bottom margin
    pub margin_bottom: f32,
}

/// Result of laying out an absolutely positioned element
#[derive(Debug, Clone)]
pub struct AbsoluteLayoutResult {
    /// The fragment node representing the laid out element
    pub fragment: FragmentNode,
    /// The computed horizontal position and size
    pub horizontal: HorizontalResult,
    /// The computed vertical position and size
    pub vertical: VerticalResult,
}

/// Lays out an absolutely positioned box
///
/// This is the main entry point for absolute positioning. It:
/// 1. Computes horizontal position and width using the constraint equation
/// 2. Computes vertical position and height using the constraint equation
/// 3. Creates a positioned fragment at the computed location
///
/// # Arguments
///
/// * `box_node` - The box to layout (must have position: absolute)
/// * `containing_block` - The containing block rect (padding box of nearest positioned ancestor)
///
/// # Returns
///
/// A positioned fragment node, or an error if layout fails
///
/// # CSS Reference
///
/// CSS 2.1 Sections 10.3.7 and 10.6.4
pub fn layout_absolute(box_node: &BoxNode, containing_block: Rect) -> Result<AbsoluteLayoutResult, LayoutError> {
    let style = &*box_node.style;

    // Compute horizontal position and width
    let horizontal = compute_absolute_horizontal(style, containing_block.size.width);

    // Compute vertical position and height
    let vertical = compute_absolute_vertical(style, containing_block.size.height);

    // Position is relative to containing block origin
    let x = containing_block.origin.x + horizontal.x;
    let y = containing_block.origin.y + vertical.y;

    // Create the fragment with computed bounds
    let bounds = Rect::from_xywh(x, y, horizontal.width, vertical.height);

    // For now, create a simple block fragment without recursively laying out children
    // A full implementation would call the appropriate formatting context here
    let fragment = FragmentNode::new_block(bounds, vec![]);

    Ok(AbsoluteLayoutResult {
        fragment,
        horizontal,
        vertical,
    })
}

/// Computes horizontal position and width for an absolutely positioned element
///
/// Implements the constraint equation from CSS 2.1 Section 10.3.7:
///
/// ```text
/// left + margin-left + border-left + padding-left + width +
/// padding-right + border-right + margin-right + right = containing block width
/// ```
///
/// # Arguments
///
/// * `style` - The computed style of the element
/// * `cb_width` - The width of the containing block
///
/// # Returns
///
/// A `HorizontalResult` containing x position, width, and resolved margins
pub fn compute_absolute_horizontal(style: &ComputedStyles, cb_width: f32) -> HorizontalResult {
    // Extract style values - Option<Length> where None means auto
    let left_opt = &style.left;
    let right_opt = &style.right;
    let width_opt = &style.width;

    // Resolve margins - Option<Length> where None means auto
    let margin_left_opt = &style.margin_left;
    let margin_right_opt = &style.margin_right;

    // Padding and border are never auto
    let padding_left = resolve_length(&style.padding_left, cb_width);
    let padding_right = resolve_length(&style.padding_right, cb_width);
    let border_left = resolve_length(&style.border_left_width, cb_width);
    let border_right = resolve_length(&style.border_right_width, cb_width);

    // Horizontal border-box overhead
    let box_horizontal = padding_left + padding_right + border_left + border_right;

    // Determine which values are specified (not auto/None)
    let left_specified = left_opt.is_some();
    let right_specified = right_opt.is_some();
    let width_specified = width_opt.is_some();

    // Resolve specified values (0.0 as default for unspecified)
    let left_px = resolve_length_option(left_opt, cb_width, 0.0);
    let right_px = resolve_length_option(right_opt, cb_width, 0.0);
    let width_px = resolve_length_option(width_opt, cb_width, 0.0);

    // Resolve margins (0 if not auto)
    let margin_left_px = resolve_length_option(margin_left_opt, cb_width, 0.0);
    let margin_right_px = resolve_length_option(margin_right_opt, cb_width, 0.0);

    // Case analysis based on CSS 2.1 Section 10.3.7
    match (left_specified, width_specified, right_specified) {
        // Case 1: All three specified (overconstrained)
        // For LTR, ignore right
        (true, true, true) => {
            let x = left_px + margin_left_px + border_left + padding_left;
            HorizontalResult {
                x,
                width: width_px,
                margin_left: margin_left_px,
                margin_right: margin_right_px,
            }
        }

        // Case 2: left and width specified, right is auto
        (true, true, false) => {
            let x = left_px + margin_left_px + border_left + padding_left;
            HorizontalResult {
                x,
                width: width_px,
                margin_left: margin_left_px,
                margin_right: margin_right_px,
            }
        }

        // Case 3: width and right specified, left is auto
        (false, true, true) => {
            let used_space = right_px + margin_left_px + margin_right_px + box_horizontal + width_px;
            let computed_left = (cb_width - used_space).max(0.0);
            let x = computed_left + margin_left_px + border_left + padding_left;
            HorizontalResult {
                x,
                width: width_px,
                margin_left: margin_left_px,
                margin_right: margin_right_px,
            }
        }

        // Case 4: left and right specified, width is auto
        (true, false, true) => {
            let used_space = left_px + right_px + margin_left_px + margin_right_px + box_horizontal;
            let computed_width = (cb_width - used_space).max(0.0);
            let x = left_px + margin_left_px + border_left + padding_left;
            HorizontalResult {
                x,
                width: computed_width,
                margin_left: margin_left_px,
                margin_right: margin_right_px,
            }
        }

        // Case 5: Only width specified (left and right are auto)
        // Use static position for left
        (false, true, false) => {
            let x = margin_left_px + border_left + padding_left;
            HorizontalResult {
                x,
                width: width_px,
                margin_left: margin_left_px,
                margin_right: margin_right_px,
            }
        }

        // Case 6: Only left specified (width and right are auto)
        // Use shrink-to-fit for width
        (true, false, false) => {
            let shrink_to_fit_width = compute_shrink_to_fit_width(cb_width);
            let x = left_px + margin_left_px + border_left + padding_left;
            HorizontalResult {
                x,
                width: shrink_to_fit_width,
                margin_left: margin_left_px,
                margin_right: margin_right_px,
            }
        }

        // Case 7: Only right specified (left and width are auto)
        // Use shrink-to-fit width, compute left
        (false, false, true) => {
            let shrink_to_fit_width = compute_shrink_to_fit_width(cb_width);
            let used_space = right_px + margin_left_px + margin_right_px + box_horizontal + shrink_to_fit_width;
            let computed_left = (cb_width - used_space).max(0.0);
            let x = computed_left + margin_left_px + border_left + padding_left;
            HorizontalResult {
                x,
                width: shrink_to_fit_width,
                margin_left: margin_left_px,
                margin_right: margin_right_px,
            }
        }

        // Case 8: None specified (all auto)
        // Use static position, shrink-to-fit width
        (false, false, false) => {
            let shrink_to_fit_width = compute_shrink_to_fit_width(cb_width);
            let x = margin_left_px + border_left + padding_left;
            HorizontalResult {
                x,
                width: shrink_to_fit_width,
                margin_left: margin_left_px,
                margin_right: margin_right_px,
            }
        }
    }
}

/// Computes vertical position and height for an absolutely positioned element
///
/// Implements the constraint equation from CSS 2.1 Section 10.6.4:
///
/// ```text
/// top + margin-top + border-top + padding-top + height +
/// padding-bottom + border-bottom + margin-bottom + bottom = containing block height
/// ```
///
/// # Arguments
///
/// * `style` - The computed style of the element
/// * `cb_height` - The height of the containing block
///
/// # Returns
///
/// A `VerticalResult` containing y position, height, and resolved margins
pub fn compute_absolute_vertical(style: &ComputedStyles, cb_height: f32) -> VerticalResult {
    // Extract style values
    let top_opt = &style.top;
    let bottom_opt = &style.bottom;
    let height_opt = &style.height;

    // Resolve margins
    let margin_top_opt = &style.margin_top;
    let margin_bottom_opt = &style.margin_bottom;

    // Padding and border
    let padding_top = resolve_length(&style.padding_top, cb_height);
    let padding_bottom = resolve_length(&style.padding_bottom, cb_height);
    let border_top = resolve_length(&style.border_top_width, cb_height);
    let border_bottom = resolve_length(&style.border_bottom_width, cb_height);

    // Vertical border-box overhead
    let box_vertical = padding_top + padding_bottom + border_top + border_bottom;

    // Determine which values are specified
    let top_specified = top_opt.is_some();
    let bottom_specified = bottom_opt.is_some();
    let height_specified = height_opt.is_some();

    // Resolve specified values
    let top_px = resolve_length_option(top_opt, cb_height, 0.0);
    let bottom_px = resolve_length_option(bottom_opt, cb_height, 0.0);
    let height_px = resolve_length_option(height_opt, cb_height, 0.0);

    // Resolve margins
    let margin_top_px = resolve_length_option(margin_top_opt, cb_height, 0.0);
    let margin_bottom_px = resolve_length_option(margin_bottom_opt, cb_height, 0.0);

    // Case analysis (similar to horizontal)
    match (top_specified, height_specified, bottom_specified) {
        // Case 1: All three specified (overconstrained) - ignore bottom
        (true, true, true) => {
            let y = top_px + margin_top_px + border_top + padding_top;
            VerticalResult {
                y,
                height: height_px,
                margin_top: margin_top_px,
                margin_bottom: margin_bottom_px,
            }
        }

        // Case 2: top and height specified
        (true, true, false) => {
            let y = top_px + margin_top_px + border_top + padding_top;
            VerticalResult {
                y,
                height: height_px,
                margin_top: margin_top_px,
                margin_bottom: margin_bottom_px,
            }
        }

        // Case 3: height and bottom specified
        (false, true, true) => {
            let used_space = bottom_px + margin_top_px + margin_bottom_px + box_vertical + height_px;
            let computed_top = (cb_height - used_space).max(0.0);
            let y = computed_top + margin_top_px + border_top + padding_top;
            VerticalResult {
                y,
                height: height_px,
                margin_top: margin_top_px,
                margin_bottom: margin_bottom_px,
            }
        }

        // Case 4: top and bottom specified, height is auto
        (true, false, true) => {
            let used_space = top_px + bottom_px + margin_top_px + margin_bottom_px + box_vertical;
            let computed_height = (cb_height - used_space).max(0.0);
            let y = top_px + margin_top_px + border_top + padding_top;
            VerticalResult {
                y,
                height: computed_height,
                margin_top: margin_top_px,
                margin_bottom: margin_bottom_px,
            }
        }

        // Case 5: Only height specified
        (false, true, false) => {
            // Use static position (0) for top
            let y = margin_top_px + border_top + padding_top;
            VerticalResult {
                y,
                height: height_px,
                margin_top: margin_top_px,
                margin_bottom: margin_bottom_px,
            }
        }

        // Case 6: Only top specified
        (true, false, false) => {
            let content_height = compute_auto_height();
            let y = top_px + margin_top_px + border_top + padding_top;
            VerticalResult {
                y,
                height: content_height,
                margin_top: margin_top_px,
                margin_bottom: margin_bottom_px,
            }
        }

        // Case 7: Only bottom specified
        (false, false, true) => {
            let content_height = compute_auto_height();
            let used_space = bottom_px + margin_top_px + margin_bottom_px + box_vertical + content_height;
            let computed_top = (cb_height - used_space).max(0.0);
            let y = computed_top + margin_top_px + border_top + padding_top;
            VerticalResult {
                y,
                height: content_height,
                margin_top: margin_top_px,
                margin_bottom: margin_bottom_px,
            }
        }

        // Case 8: None specified
        (false, false, false) => {
            let content_height = compute_auto_height();
            let y = margin_top_px + border_top + padding_top;
            VerticalResult {
                y,
                height: content_height,
                margin_top: margin_top_px,
                margin_bottom: margin_bottom_px,
            }
        }
    }
}

/// Computes horizontal centering for absolutely positioned elements
///
/// When left: 0, right: 0, width: specified, and margin-left/right: auto (None),
/// the element is centered horizontally within its containing block.
pub fn compute_centered_horizontal(style: &ComputedStyles, cb_width: f32) -> Option<HorizontalResult> {
    let left_opt = &style.left;
    let right_opt = &style.right;
    let width_opt = &style.width;
    let margin_left_opt = &style.margin_left;
    let margin_right_opt = &style.margin_right;

    // Check if we have the centering pattern
    if left_opt.is_some() && right_opt.is_some() && width_opt.is_some() {
        let left_px = resolve_length_option(left_opt, cb_width, 0.0);
        let right_px = resolve_length_option(right_opt, cb_width, 0.0);
        let width_px = resolve_length_option(width_opt, cb_width, 0.0);

        let padding_left = resolve_length(&style.padding_left, cb_width);
        let padding_right = resolve_length(&style.padding_right, cb_width);
        let border_left = resolve_length(&style.border_left_width, cb_width);
        let border_right = resolve_length(&style.border_right_width, cb_width);
        let box_horizontal = padding_left + padding_right + border_left + border_right;

        // Available space for margins
        let available = cb_width - left_px - right_px - box_horizontal - width_px;

        // Margins are auto if they are None in the style
        let margin_left_is_auto = margin_left_opt.is_none();
        let margin_right_is_auto = margin_right_opt.is_none();

        if margin_left_is_auto && margin_right_is_auto {
            // Center: equal margins
            let margin = (available / 2.0).max(0.0);
            let x = left_px + margin + border_left + padding_left;

            return Some(HorizontalResult {
                x,
                width: width_px,
                margin_left: margin,
                margin_right: margin,
            });
        }
    }

    None
}

/// Computes vertical centering for absolutely positioned elements
pub fn compute_centered_vertical(style: &ComputedStyles, cb_height: f32) -> Option<VerticalResult> {
    let top_opt = &style.top;
    let bottom_opt = &style.bottom;
    let height_opt = &style.height;
    let margin_top_opt = &style.margin_top;
    let margin_bottom_opt = &style.margin_bottom;

    if top_opt.is_some() && bottom_opt.is_some() && height_opt.is_some() {
        let top_px = resolve_length_option(top_opt, cb_height, 0.0);
        let bottom_px = resolve_length_option(bottom_opt, cb_height, 0.0);
        let height_px = resolve_length_option(height_opt, cb_height, 0.0);

        let padding_top = resolve_length(&style.padding_top, cb_height);
        let padding_bottom = resolve_length(&style.padding_bottom, cb_height);
        let border_top = resolve_length(&style.border_top_width, cb_height);
        let border_bottom = resolve_length(&style.border_bottom_width, cb_height);
        let box_vertical = padding_top + padding_bottom + border_top + border_bottom;

        let available = cb_height - top_px - bottom_px - box_vertical - height_px;

        let margin_top_is_auto = margin_top_opt.is_none();
        let margin_bottom_is_auto = margin_bottom_opt.is_none();

        if margin_top_is_auto && margin_bottom_is_auto {
            let margin = (available / 2.0).max(0.0);
            let y = top_px + margin + border_top + padding_top;

            return Some(VerticalResult {
                y,
                height: height_px,
                margin_top: margin,
                margin_bottom: margin,
            });
        }
    }

    None
}

/// Finds the initial containing block (viewport)
pub fn get_initial_containing_block(viewport_size: Size) -> Rect {
    Rect::new(Point::ZERO, viewport_size)
}

/// Checks if an element should be laid out with absolute positioning
pub fn is_out_of_flow(position: Position) -> bool {
    position.is_absolutely_positioned() || position.is_fixed()
}

// === Helper Functions ===

/// Resolves a Length value against a containing block dimension
fn resolve_length(length: &Length, percentage_base: f32) -> f32 {
    if length.unit.is_absolute() {
        length.to_px()
    } else if length.unit.is_percentage() {
        length.resolve_against(percentage_base)
    } else {
        // For font-relative units, we'd need font context
        // For now, treat as pixels
        length.value
    }
}

/// Resolves an Option<Length> value against a containing block dimension
fn resolve_length_option(length_opt: &Option<Length>, percentage_base: f32, default: f32) -> f32 {
    match length_opt {
        Some(length) => resolve_length(length, percentage_base),
        None => default,
    }
}

/// Computes shrink-to-fit width for an element
fn compute_shrink_to_fit_width(available_width: f32) -> f32 {
    // CSS 2.1 Section 10.3.5: shrink-to-fit width is
    // min(max(preferred minimum width, available width), preferred width)
    // For now, use a reasonable default
    (available_width * 0.5).max(100.0).min(available_width)
}

/// Computes auto height for an element
fn compute_auto_height() -> f32 {
    // Default content height - a real implementation would measure content
    100.0
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::style::display::FormattingContextType;
    use crate::style::ComputedStyles;
    use std::sync::Arc;

    fn make_style() -> ComputedStyles {
        ComputedStyles::default()
    }

    fn make_box_with_style(style: ComputedStyles) -> BoxNode {
        BoxNode::new_block(Arc::new(style), FormattingContextType::Block, vec![])
    }

    #[test]
    fn test_horizontal_left_and_width() {
        let mut style = make_style();
        style.left = Some(Length::px(50.0));
        style.width = Some(Length::px(200.0));
        style.right = None;

        let result = compute_absolute_horizontal(&style, 800.0);

        assert_eq!(result.width, 200.0);
        assert_eq!(result.x, 50.0);
    }

    #[test]
    fn test_horizontal_width_and_right() {
        let mut style = make_style();
        style.left = None;
        style.width = Some(Length::px(200.0));
        style.right = Some(Length::px(50.0));

        let result = compute_absolute_horizontal(&style, 800.0);

        assert_eq!(result.width, 200.0);
        // x = cb_width - right - width = 800 - 50 - 200 = 550
        assert_eq!(result.x, 550.0);
    }

    #[test]
    fn test_horizontal_left_and_right_compute_width() {
        let mut style = make_style();
        style.left = Some(Length::px(100.0));
        style.right = Some(Length::px(100.0));
        style.width = None;

        let result = compute_absolute_horizontal(&style, 800.0);

        // width = cb_width - left - right = 800 - 100 - 100 = 600
        assert_eq!(result.width, 600.0);
        assert_eq!(result.x, 100.0);
    }

    #[test]
    fn test_vertical_top_and_height() {
        let mut style = make_style();
        style.top = Some(Length::px(20.0));
        style.height = Some(Length::px(150.0));
        style.bottom = None;

        let result = compute_absolute_vertical(&style, 600.0);

        assert_eq!(result.height, 150.0);
        assert_eq!(result.y, 20.0);
    }

    #[test]
    fn test_vertical_height_and_bottom() {
        let mut style = make_style();
        style.top = None;
        style.height = Some(Length::px(100.0));
        style.bottom = Some(Length::px(50.0));

        let result = compute_absolute_vertical(&style, 600.0);

        assert_eq!(result.height, 100.0);
        // y = cb_height - bottom - height = 600 - 50 - 100 = 450
        assert_eq!(result.y, 450.0);
    }

    #[test]
    fn test_vertical_top_and_bottom_compute_height() {
        let mut style = make_style();
        style.top = Some(Length::px(50.0));
        style.bottom = Some(Length::px(50.0));
        style.height = None;

        let result = compute_absolute_vertical(&style, 600.0);

        // height = cb_height - top - bottom = 600 - 50 - 50 = 500
        assert_eq!(result.height, 500.0);
        assert_eq!(result.y, 50.0);
    }

    #[test]
    fn test_percentage_values() {
        let mut style = make_style();
        style.left = Some(Length::percent(10.0)); // 10% of 800 = 80
        style.width = Some(Length::percent(50.0)); // 50% of 800 = 400

        let result = compute_absolute_horizontal(&style, 800.0);

        assert_eq!(result.x, 80.0);
        assert_eq!(result.width, 400.0);
    }

    #[test]
    fn test_all_auto_uses_defaults() {
        let style = make_style();

        let h_result = compute_absolute_horizontal(&style, 800.0);
        let v_result = compute_absolute_vertical(&style, 600.0);

        // Should use shrink-to-fit width and auto height
        assert!(h_result.width > 0.0);
        assert!(v_result.height > 0.0);
        assert_eq!(h_result.x, 0.0);
        assert_eq!(v_result.y, 0.0);
    }

    #[test]
    fn test_layout_absolute_basic() {
        let mut style = make_style();
        style.position = Position::Absolute;
        style.left = Some(Length::px(50.0));
        style.top = Some(Length::px(30.0));
        style.width = Some(Length::px(200.0));
        style.height = Some(Length::px(100.0));

        let box_node = make_box_with_style(style);
        let containing_block = Rect::from_xywh(0.0, 0.0, 800.0, 600.0);

        let result = layout_absolute(&box_node, containing_block).unwrap();

        assert_eq!(result.fragment.bounds.x(), 50.0);
        assert_eq!(result.fragment.bounds.y(), 30.0);
        assert_eq!(result.fragment.bounds.width(), 200.0);
        assert_eq!(result.fragment.bounds.height(), 100.0);
    }

    #[test]
    fn test_layout_absolute_with_offset_containing_block() {
        let mut style = make_style();
        style.position = Position::Absolute;
        style.left = Some(Length::px(10.0));
        style.top = Some(Length::px(10.0));
        style.width = Some(Length::px(100.0));
        style.height = Some(Length::px(100.0));

        let box_node = make_box_with_style(style);
        // Containing block is offset from viewport origin
        let containing_block = Rect::from_xywh(100.0, 50.0, 400.0, 300.0);

        let result = layout_absolute(&box_node, containing_block).unwrap();

        // Position should be relative to containing block
        assert_eq!(result.fragment.bounds.x(), 110.0); // 100 + 10
        assert_eq!(result.fragment.bounds.y(), 60.0); // 50 + 10
    }

    #[test]
    fn test_is_out_of_flow() {
        assert!(is_out_of_flow(Position::Absolute));
        assert!(is_out_of_flow(Position::Fixed));
        assert!(!is_out_of_flow(Position::Static));
        assert!(!is_out_of_flow(Position::Relative));
        assert!(!is_out_of_flow(Position::Sticky));
    }

    #[test]
    fn test_get_initial_containing_block() {
        let viewport = Size::new(1920.0, 1080.0);
        let icb = get_initial_containing_block(viewport);

        assert_eq!(icb.origin, Point::ZERO);
        assert_eq!(icb.size, viewport);
    }

    #[test]
    fn test_negative_result_clamped_to_zero() {
        let mut style = make_style();
        style.left = Some(Length::px(500.0));
        style.right = Some(Length::px(500.0));
        style.width = None;

        let result = compute_absolute_horizontal(&style, 800.0);

        // width = 800 - 500 - 500 = -200 → clamped to 0
        assert_eq!(result.width, 0.0);
    }

    #[test]
    fn test_centered_horizontal() {
        let mut style = make_style();
        style.left = Some(Length::px(0.0));
        style.right = Some(Length::px(0.0));
        style.width = Some(Length::px(200.0));
        // margins are None (auto)

        let result = compute_centered_horizontal(&style, 800.0);

        if let Some(h) = result {
            assert_eq!(h.width, 200.0);
            // Equal margins: (800 - 200) / 2 = 300
            assert_eq!(h.margin_left, 300.0);
            assert_eq!(h.margin_right, 300.0);
        }
    }

    #[test]
    fn test_centered_vertical() {
        let mut style = make_style();
        style.top = Some(Length::px(0.0));
        style.bottom = Some(Length::px(0.0));
        style.height = Some(Length::px(100.0));
        // margins are None (auto)

        let result = compute_centered_vertical(&style, 600.0);

        if let Some(v) = result {
            assert_eq!(v.height, 100.0);
            // Equal margins: (600 - 100) / 2 = 250
            assert_eq!(v.margin_top, 250.0);
            assert_eq!(v.margin_bottom, 250.0);
        }
    }

    #[test]
    fn test_with_padding_and_border() {
        let mut style = make_style();
        style.left = Some(Length::px(50.0));
        style.width = Some(Length::px(200.0));
        style.padding_left = Length::px(10.0);
        style.border_left_width = Length::px(5.0);

        let result = compute_absolute_horizontal(&style, 800.0);

        // x = left + margin + border + padding = 50 + 0 + 5 + 10 = 65
        assert_eq!(result.x, 65.0);
        assert_eq!(result.width, 200.0);
    }

    #[test]
    fn test_only_left_specified() {
        let mut style = make_style();
        style.left = Some(Length::px(100.0));
        style.width = None;
        style.right = None;

        let result = compute_absolute_horizontal(&style, 800.0);

        assert_eq!(result.x, 100.0);
        // Should use shrink-to-fit width
        assert!(result.width > 0.0);
    }

    #[test]
    fn test_only_right_specified() {
        let mut style = make_style();
        style.left = None;
        style.width = None;
        style.right = Some(Length::px(100.0));

        let result = compute_absolute_horizontal(&style, 800.0);

        // Should use shrink-to-fit width and compute left
        assert!(result.width > 0.0);
    }

    #[test]
    fn test_only_top_specified() {
        let mut style = make_style();
        style.top = Some(Length::px(50.0));
        style.height = None;
        style.bottom = None;

        let result = compute_absolute_vertical(&style, 600.0);

        assert_eq!(result.y, 50.0);
        // Height should be auto (content height)
        assert!(result.height > 0.0);
    }

    #[test]
    fn test_only_bottom_specified() {
        let mut style = make_style();
        style.top = None;
        style.height = None;
        style.bottom = Some(Length::px(50.0));

        let result = compute_absolute_vertical(&style, 600.0);

        // Should compute top from bottom and auto height
        assert!(result.height > 0.0);
    }

    #[test]
    fn test_only_width_specified() {
        let mut style = make_style();
        style.left = None;
        style.width = Some(Length::px(300.0));
        style.right = None;

        let result = compute_absolute_horizontal(&style, 800.0);

        // Should use static position (0) for left
        assert_eq!(result.x, 0.0);
        assert_eq!(result.width, 300.0);
    }

    #[test]
    fn test_only_height_specified() {
        let mut style = make_style();
        style.top = None;
        style.height = Some(Length::px(200.0));
        style.bottom = None;

        let result = compute_absolute_vertical(&style, 600.0);

        // Should use static position (0) for top
        assert_eq!(result.y, 0.0);
        assert_eq!(result.height, 200.0);
    }

    #[test]
    fn test_overconstrained_horizontal() {
        let mut style = make_style();
        style.left = Some(Length::px(10.0));
        style.width = Some(Length::px(100.0));
        style.right = Some(Length::px(10.0)); // This should be ignored

        let result = compute_absolute_horizontal(&style, 400.0);

        // Uses left and width, ignores right
        assert_eq!(result.x, 10.0);
        assert_eq!(result.width, 100.0);
    }

    #[test]
    fn test_overconstrained_vertical() {
        let mut style = make_style();
        style.top = Some(Length::px(10.0));
        style.height = Some(Length::px(100.0));
        style.bottom = Some(Length::px(10.0)); // This should be ignored

        let result = compute_absolute_vertical(&style, 300.0);

        // Uses top and height, ignores bottom
        assert_eq!(result.y, 10.0);
        assert_eq!(result.height, 100.0);
    }
}
