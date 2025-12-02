//! Block Width Computation (CSS 2.1 Section 10.3.3)
//!
//! This module implements width computation for block-level, non-replaced
//! elements in normal flow. This is the most common case for block layout.
//!
//! # The Fundamental Constraint Equation
//!
//! From CSS 2.1 Section 10.3.3:
//! ```text
//! margin-left + border-left-width + padding-left + width +
//! padding-right + border-right-width + margin-right = containing block width
//! ```
//!
//! This equation MUST be satisfied. When values are under-specified or
//! over-specified, the spec defines resolution rules.
//!
//! # Resolution Algorithm
//!
//! 1. If `width` is not `auto`:
//!    - If both margins are `auto`: center the box (equal margins)
//!    - If only `margin-left` is `auto`: it gets the remainder
//!    - If only `margin-right` is `auto`: it gets the remainder
//!    - If neither margin is `auto` (over-constrained): ignore `margin-right` in LTR
//!
//! 2. If `width` is `auto`:
//!    - Any `auto` margins become 0
//!    - `width` fills remaining space
//!
//! Reference: <https://www.w3.org/TR/CSS21/visudet.html#Computing_widths_and_margins>

use crate::style::ComputedStyle;
use crate::style::values::Length;

/// Result of width computation
///
/// Contains all resolved horizontal box dimensions:
/// margin-left, border-left, padding-left, content-width,
/// padding-right, border-right, margin-right
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ComputedBlockWidth {
    /// Computed left margin (can be negative from over-constraining)
    pub margin_left: f32,
    /// Border left width
    pub border_left: f32,
    /// Padding left
    pub padding_left: f32,
    /// Content width
    pub content_width: f32,
    /// Padding right
    pub padding_right: f32,
    /// Border right width
    pub border_right: f32,
    /// Computed right margin (can be negative from over-constraining)
    pub margin_right: f32,
}

impl ComputedBlockWidth {
    /// Returns the total width including margins
    pub fn total_width(&self) -> f32 {
        self.margin_left
            + self.border_left
            + self.padding_left
            + self.content_width
            + self.padding_right
            + self.border_right
            + self.margin_right
    }

    /// Returns the border box width (without margins)
    pub fn border_box_width(&self) -> f32 {
        self.border_left + self.padding_left + self.content_width + self.padding_right + self.border_right
    }

    /// Returns the padding box width (content + padding)
    pub fn padding_box_width(&self) -> f32 {
        self.padding_left + self.content_width + self.padding_right
    }

    /// Returns the content box start x-offset from the margin edge
    pub fn content_offset_x(&self) -> f32 {
        self.margin_left + self.border_left + self.padding_left
    }
}

/// Marker for margin values that may be auto
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MarginValue {
    /// A specific length value
    Length(f32),
    /// The `auto` keyword
    Auto,
}

impl MarginValue {
    /// Returns the value or 0 if auto
    pub fn unwrap_or_zero(self) -> f32 {
        match self {
            Self::Length(v) => v,
            Self::Auto => 0.0,
        }
    }

    /// Returns true if this is auto
    pub fn is_auto(self) -> bool {
        matches!(self, Self::Auto)
    }
}

/// Computes the width of a block-level, non-replaced element in normal flow
///
/// Implements CSS 2.1 Section 10.3.3 width computation algorithm.
///
/// # Arguments
///
/// * `style` - The computed style of the element
/// * `containing_width` - The width of the containing block
///
/// # Returns
///
/// A `ComputedBlockWidth` containing all resolved horizontal dimensions.
pub fn compute_block_width(style: &ComputedStyle, containing_width: f32) -> ComputedBlockWidth {
    // Resolve padding (percentages relative to containing width)
    let padding_left = resolve_length(style.padding_left, containing_width);
    let padding_right = resolve_length(style.padding_right, containing_width);

    // Border widths
    let border_left = resolve_length(style.border_left_width, containing_width);
    let border_right = resolve_length(style.border_right_width, containing_width);

    // Resolve margins (may be auto - represented by None)
    let margin_left = match &style.margin_left {
        Some(len) => MarginValue::Length(len.resolve_against(containing_width)),
        None => MarginValue::Auto,
    };
    let margin_right = match &style.margin_right {
        Some(len) => MarginValue::Length(len.resolve_against(containing_width)),
        None => MarginValue::Auto,
    };

    // Resolve width (may be auto - represented by None)
    let width_value = style.width.as_ref().map(|len| len.resolve_against(containing_width));

    // Compute the resolved values using the constraint equation
    let (final_margin_left, final_width, final_margin_right) = resolve_constraint(
        containing_width,
        border_left,
        padding_left,
        width_value,
        padding_right,
        border_right,
        margin_left,
        margin_right,
    );

    ComputedBlockWidth {
        margin_left: final_margin_left,
        border_left,
        padding_left,
        content_width: final_width,
        padding_right,
        border_right,
        margin_right: final_margin_right,
    }
}

/// Computes width with explicit auto margin flags
///
/// Use this when you have explicit information about whether margins are auto.
pub fn compute_block_width_with_auto_margins(
    style: &ComputedStyle,
    containing_width: f32,
    margin_left_is_auto: bool,
    margin_right_is_auto: bool,
) -> ComputedBlockWidth {
    // Resolve padding
    let padding_left = resolve_length(style.padding_left, containing_width);
    let padding_right = resolve_length(style.padding_right, containing_width);

    // Border widths
    let border_left = resolve_length(style.border_left_width, containing_width);
    let border_right = resolve_length(style.border_right_width, containing_width);

    // Resolve margins with explicit auto flags
    let margin_left = if margin_left_is_auto {
        MarginValue::Auto
    } else {
        MarginValue::Length(
            style
                .margin_left
                .as_ref()
                .map(|l| l.resolve_against(containing_width))
                .unwrap_or(0.0),
        )
    };

    let margin_right = if margin_right_is_auto {
        MarginValue::Auto
    } else {
        MarginValue::Length(
            style
                .margin_right
                .as_ref()
                .map(|l| l.resolve_against(containing_width))
                .unwrap_or(0.0),
        )
    };

    // Resolve width
    let width_value = style.width.as_ref().map(|len| len.resolve_against(containing_width));

    // Compute the resolved values
    let (final_margin_left, final_width, final_margin_right) = resolve_constraint(
        containing_width,
        border_left,
        padding_left,
        width_value,
        padding_right,
        border_right,
        margin_left,
        margin_right,
    );

    ComputedBlockWidth {
        margin_left: final_margin_left,
        border_left,
        padding_left,
        content_width: final_width,
        padding_right,
        border_right,
        margin_right: final_margin_right,
    }
}

/// Resolves the constraint equation for block width
///
/// This is the core algorithm from CSS 2.1 Section 10.3.3.
fn resolve_constraint(
    containing_width: f32,
    border_left: f32,
    padding_left: f32,
    width: Option<f32>,
    padding_right: f32,
    border_right: f32,
    margin_left: MarginValue,
    margin_right: MarginValue,
) -> (f32, f32, f32) {
    // Sum of non-auto, non-width values
    let borders_and_padding = border_left + padding_left + padding_right + border_right;

    match width {
        Some(w) => {
            // Width is specified
            let width = w.max(0.0); // Width cannot be negative

            match (margin_left, margin_right) {
                (MarginValue::Auto, MarginValue::Auto) => {
                    // Both margins auto: CENTER the box
                    let available = containing_width - borders_and_padding - width;
                    let margin = (available / 2.0).max(0.0);
                    (margin, width, margin)
                }
                (MarginValue::Auto, MarginValue::Length(mr)) => {
                    // Only left margin auto: it gets the remainder
                    let ml = containing_width - borders_and_padding - width - mr;
                    (ml, width, mr)
                }
                (MarginValue::Length(ml), MarginValue::Auto) => {
                    // Only right margin auto: it gets the remainder
                    let mr = containing_width - borders_and_padding - width - ml;
                    (ml, width, mr)
                }
                (MarginValue::Length(ml), MarginValue::Length(_mr)) => {
                    // Neither margin is auto: OVER-CONSTRAINED
                    // Ignore margin-right (in LTR) - recompute it
                    let mr = containing_width - borders_and_padding - width - ml;
                    (ml, width, mr)
                }
            }
        }
        None => {
            // Width is auto
            // Auto margins become 0
            let ml = margin_left.unwrap_or_zero();
            let mr = margin_right.unwrap_or_zero();

            // Width fills remaining space
            let width = (containing_width - borders_and_padding - ml - mr).max(0.0);
            (ml, width, mr)
        }
    }
}

/// Resolves a Length value to pixels
fn resolve_length(length: Length, containing_width: f32) -> f32 {
    length.resolve_against(containing_width)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn default_style() -> ComputedStyle {
        ComputedStyle::default()
    }

    // ComputedBlockWidth tests
    #[test]
    fn test_computed_block_width_total() {
        let w = ComputedBlockWidth {
            margin_left: 10.0,
            border_left: 2.0,
            padding_left: 5.0,
            content_width: 100.0,
            padding_right: 5.0,
            border_right: 2.0,
            margin_right: 10.0,
        };
        assert_eq!(w.total_width(), 134.0);
    }

    #[test]
    fn test_computed_block_width_border_box() {
        let w = ComputedBlockWidth {
            margin_left: 10.0,
            border_left: 2.0,
            padding_left: 5.0,
            content_width: 100.0,
            padding_right: 5.0,
            border_right: 2.0,
            margin_right: 10.0,
        };
        assert_eq!(w.border_box_width(), 114.0);
    }

    #[test]
    fn test_computed_block_width_padding_box() {
        let w = ComputedBlockWidth {
            margin_left: 10.0,
            border_left: 2.0,
            padding_left: 5.0,
            content_width: 100.0,
            padding_right: 5.0,
            border_right: 2.0,
            margin_right: 10.0,
        };
        assert_eq!(w.padding_box_width(), 110.0);
    }

    #[test]
    fn test_computed_block_width_content_offset() {
        let w = ComputedBlockWidth {
            margin_left: 10.0,
            border_left: 2.0,
            padding_left: 5.0,
            content_width: 100.0,
            padding_right: 5.0,
            border_right: 2.0,
            margin_right: 10.0,
        };
        assert_eq!(w.content_offset_x(), 17.0);
    }

    // Width computation tests
    #[test]
    fn test_width_auto_fills_container() {
        let style = default_style();
        let result = compute_block_width(&style, 800.0);
        // width: auto, margins: 0, borders/padding: 0
        // Result should fill the container
        assert_eq!(result.content_width, 800.0);
    }

    #[test]
    fn test_width_specified_with_auto_margins_centers() {
        let mut style = default_style();
        style.width = Some(Length::px(400.0));

        let result = compute_block_width_with_auto_margins(&style, 800.0, true, true);

        // width: 400px with auto margins should center
        assert_eq!(result.content_width, 400.0);
        assert_eq!(result.margin_left, 200.0);
        assert_eq!(result.margin_right, 200.0);
    }

    #[test]
    fn test_width_specified_left_margin_auto() {
        let mut style = default_style();
        style.width = Some(Length::px(400.0));
        style.margin_right = Some(Length::px(100.0));

        let result = compute_block_width_with_auto_margins(&style, 800.0, true, false);

        // width: 400px, margin-right: 100px, margin-left: auto
        // margin-left should get the remainder: 800 - 400 - 100 = 300
        assert_eq!(result.content_width, 400.0);
        assert_eq!(result.margin_right, 100.0);
        assert_eq!(result.margin_left, 300.0);
    }

    #[test]
    fn test_width_specified_right_margin_auto() {
        let mut style = default_style();
        style.width = Some(Length::px(400.0));
        style.margin_left = Some(Length::px(100.0));

        let result = compute_block_width_with_auto_margins(&style, 800.0, false, true);

        // width: 400px, margin-left: 100px, margin-right: auto
        // margin-right should get the remainder: 800 - 400 - 100 = 300
        assert_eq!(result.content_width, 400.0);
        assert_eq!(result.margin_left, 100.0);
        assert_eq!(result.margin_right, 300.0);
    }

    #[test]
    fn test_width_over_constrained() {
        let mut style = default_style();
        style.width = Some(Length::px(400.0));
        style.margin_left = Some(Length::px(100.0));
        style.margin_right = Some(Length::px(100.0));

        let result = compute_block_width(&style, 800.0);

        // Over-constrained: width: 400px, margins: 100px each
        // Total would be 600px in 800px container
        // margin-right should be recomputed: 800 - 400 - 100 = 300
        assert_eq!(result.content_width, 400.0);
        assert_eq!(result.margin_left, 100.0);
        assert_eq!(result.margin_right, 300.0);
    }

    #[test]
    fn test_width_auto_with_margins() {
        let mut style = default_style();
        style.margin_left = Some(Length::px(50.0));
        style.margin_right = Some(Length::px(50.0));

        let result = compute_block_width(&style, 800.0);

        // width: auto, margins: 50px each
        // width should be 800 - 50 - 50 = 700
        assert_eq!(result.content_width, 700.0);
        assert_eq!(result.margin_left, 50.0);
        assert_eq!(result.margin_right, 50.0);
    }

    #[test]
    fn test_width_auto_margins_become_zero() {
        let style = default_style();

        let result = compute_block_width_with_auto_margins(&style, 800.0, true, true);

        // width: auto, both margins auto
        // auto margins should become 0, width fills container
        assert_eq!(result.content_width, 800.0);
        assert_eq!(result.margin_left, 0.0);
        assert_eq!(result.margin_right, 0.0);
    }

    #[test]
    fn test_width_with_padding() {
        let mut style = default_style();
        style.padding_left = Length::px(20.0);
        style.padding_right = Length::px(20.0);

        let result = compute_block_width(&style, 800.0);

        // width: auto, padding: 20px each side
        // content width should be 800 - 40 = 760
        assert_eq!(result.content_width, 760.0);
        assert_eq!(result.padding_left, 20.0);
        assert_eq!(result.padding_right, 20.0);
    }

    #[test]
    fn test_width_with_border() {
        let mut style = default_style();
        style.border_left_width = Length::px(5.0);
        style.border_right_width = Length::px(5.0);

        let result = compute_block_width(&style, 800.0);

        // width: auto, border: 5px each side
        // content width should be 800 - 10 = 790
        assert_eq!(result.content_width, 790.0);
        assert_eq!(result.border_left, 5.0);
        assert_eq!(result.border_right, 5.0);
    }

    #[test]
    fn test_width_percentage() {
        let mut style = default_style();
        style.width = Some(Length::percent(50.0));

        let result = compute_block_width(&style, 800.0);

        // width: 50%
        // content width should be 400
        assert_eq!(result.content_width, 400.0);
    }

    #[test]
    fn test_width_cannot_be_negative() {
        let mut style = default_style();
        style.width = Some(Length::px(100.0));
        style.padding_left = Length::px(500.0);
        style.padding_right = Length::px(500.0);

        let result = compute_block_width(&style, 800.0);

        // Absurd padding that would make content negative
        // Content width should stay at the specified value (100)
        assert_eq!(result.content_width, 100.0);
    }

    // MarginValue tests
    #[test]
    fn test_margin_value_auto() {
        let m = MarginValue::Auto;
        assert!(m.is_auto());
        assert_eq!(m.unwrap_or_zero(), 0.0);
    }

    #[test]
    fn test_margin_value_length() {
        let m = MarginValue::Length(20.0);
        assert!(!m.is_auto());
        assert_eq!(m.unwrap_or_zero(), 20.0);
    }
}
