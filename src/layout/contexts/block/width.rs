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

use crate::layout::utils::content_size_from_box_sizing;
use crate::layout::utils::resolve_length_with_percentage;
use crate::layout::utils::resolve_scrollbar_width;
use crate::style::types::Direction;
use crate::style::types::Overflow;
use crate::style::values::Length;
use crate::style::ComputedStyle;

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
    self.border_left
      + self.padding_left
      + self.content_width
      + self.padding_right
      + self.border_right
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
pub fn compute_block_width(
  style: &ComputedStyle,
  containing_width: f32,
  viewport: crate::geometry::Size,
) -> ComputedBlockWidth {
  // Resolve padding (percentages relative to containing width)
  let mut padding_left = resolve_length(
    style.padding_left,
    containing_width,
    style.font_size,
    style.root_font_size,
    viewport,
  );
  let mut padding_right = resolve_length(
    style.padding_right,
    containing_width,
    style.font_size,
    style.root_font_size,
    viewport,
  );

  // Reserve space for a vertical scrollbar when requested by overflow or scrollbar-gutter stability
  let reserve_vertical_gutter = matches!(style.overflow_y, Overflow::Scroll)
    || (style.scrollbar_gutter.stable
      && matches!(style.overflow_y, Overflow::Auto | Overflow::Scroll));
  if reserve_vertical_gutter {
    let gutter = resolve_scrollbar_width(style);
    if gutter > 0.0 {
      if style.scrollbar_gutter.both_edges {
        padding_left += gutter;
        padding_right += gutter;
      } else if style.direction == Direction::Rtl {
        padding_left += gutter;
      } else {
        padding_right += gutter;
      }
    }
  }

  // Border widths
  let border_left = resolve_length(
    style.border_left_width,
    containing_width,
    style.font_size,
    style.root_font_size,
    viewport,
  );
  let border_right = resolve_length(
    style.border_right_width,
    containing_width,
    style.font_size,
    style.root_font_size,
    viewport,
  );
  let horizontal_edges = padding_left + padding_right + border_left + border_right;

  // Resolve margins (may be auto - represented by None)
  let margin_left = match &style.margin_left {
    Some(len) => MarginValue::Length(resolve_length(
      *len,
      containing_width,
      style.font_size,
      style.root_font_size,
      viewport,
    )),
    None => MarginValue::Auto,
  };
  let margin_right = match &style.margin_right {
    Some(len) => MarginValue::Length(resolve_length(
      *len,
      containing_width,
      style.font_size,
      style.root_font_size,
      viewport,
    )),
    None => MarginValue::Auto,
  };

  // Resolve width (may be auto - represented by None)
  let width_value = style
    .width
    .as_ref()
    .map(|len| {
      resolve_length(
        *len,
        containing_width,
        style.font_size,
        style.root_font_size,
        viewport,
      )
    })
    .map(|w| content_size_from_box_sizing(w, horizontal_edges, style.box_sizing));

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
    style.direction,
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
  viewport: crate::geometry::Size,
) -> ComputedBlockWidth {
  // Resolve padding
  let padding_left = resolve_length(
    style.padding_left,
    containing_width,
    style.font_size,
    style.root_font_size,
    viewport,
  );
  let padding_right = resolve_length(
    style.padding_right,
    containing_width,
    style.font_size,
    style.root_font_size,
    viewport,
  );

  // Border widths
  let border_left = resolve_length(
    style.border_left_width,
    containing_width,
    style.font_size,
    style.root_font_size,
    viewport,
  );
  let border_right = resolve_length(
    style.border_right_width,
    containing_width,
    style.font_size,
    style.root_font_size,
    viewport,
  );
  let horizontal_edges = padding_left + padding_right + border_left + border_right;

  // Resolve margins with explicit auto flags
  let margin_left = if margin_left_is_auto {
    MarginValue::Auto
  } else {
    MarginValue::Length(
      style
        .margin_left
        .as_ref()
        .map(|l| {
          resolve_length(
            *l,
            containing_width,
            style.font_size,
            style.root_font_size,
            viewport,
          )
        })
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
        .map(|l| {
          resolve_length(
            *l,
            containing_width,
            style.font_size,
            style.root_font_size,
            viewport,
          )
        })
        .unwrap_or(0.0),
    )
  };

  // Resolve width
  let width_value = style
    .width
    .as_ref()
    .map(|len| {
      resolve_length(
        *len,
        containing_width,
        style.font_size,
        style.root_font_size,
        viewport,
      )
    })
    .map(|w| content_size_from_box_sizing(w, horizontal_edges, style.box_sizing));

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
    style.direction,
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
  direction: crate::style::types::Direction,
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
          // Ignore the end-side margin per CSS2.1 §10.3.3: right in LTR, left in RTL.
          match direction {
            crate::style::types::Direction::Ltr => {
              let mr = containing_width - borders_and_padding - width - ml;
              (ml, width, mr)
            }
            crate::style::types::Direction::Rtl => {
              let ml =
                containing_width - borders_and_padding - width - margin_right.unwrap_or_zero();
              (ml, width, margin_right.unwrap_or_zero())
            }
          }
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

/// Resolves a Length value to pixels using the element and root font sizes for font-relative units.
fn resolve_length(
  length: Length,
  containing_width: f32,
  font_size: f32,
  root_font_size: f32,
  viewport: crate::geometry::Size,
) -> f32 {
  let base = if containing_width.is_finite() {
    Some(containing_width)
  } else {
    None
  };
  resolve_length_with_percentage(length, base, viewport, font_size, root_font_size).unwrap_or(0.0)
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::geometry::Size;
  use crate::style::types::BoxSizing;
  use crate::style::types::Direction;
  use crate::style::types::Overflow;
  use crate::style::types::ScrollbarWidth;
  use crate::style::values::LengthUnit;

  fn default_style() -> ComputedStyle {
    ComputedStyle::default()
  }

  fn viewport() -> Size {
    Size::new(800.0, 600.0)
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
  fn resolves_font_relative_padding_and_rem() {
    let mut style = default_style();
    style.font_size = 10.0;
    style.root_font_size = 12.0;
    style.padding_left = Length::em(1.5);
    style.padding_right = Length::rem(1.0);

    let width = compute_block_width(&style, 200.0, viewport());
    assert!((width.padding_left - 15.0).abs() < f32::EPSILON);
    assert!((width.padding_right - 12.0).abs() < f32::EPSILON);
  }

  #[test]
  fn resolves_viewport_units_against_viewport() {
    let mut style = default_style();
    style.padding_left = Length::new(10.0, LengthUnit::Vw);
    style.padding_right = Length::new(5.0, LengthUnit::Vmin);

    let width = compute_block_width(&style, 200.0, viewport());
    assert!((width.padding_left - 80.0).abs() < f32::EPSILON);
    assert!((width.padding_right - 30.0).abs() < f32::EPSILON);
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

  #[test]
  fn scrollbar_width_reserves_inline_gutter() {
    let mut style = default_style();
    style.overflow_y = Overflow::Scroll;
    style.scrollbar_width = ScrollbarWidth::Thin;

    let result = compute_block_width(&style, 200.0, viewport());
    // Thin scrollbar (8px) should be reserved on the inline end (LTR → right).
    assert!((result.padding_right - 8.0).abs() < f32::EPSILON);
    assert!((result.content_width - 192.0).abs() < f32::EPSILON);

    style.direction = Direction::Rtl;
    let rtl = compute_block_width(&style, 200.0, viewport());
    assert!((rtl.padding_left - 8.0).abs() < f32::EPSILON);
  }

  // Width computation tests
  #[test]
  fn test_width_auto_fills_container() {
    let style = default_style();
    let result = compute_block_width(&style, 800.0, viewport());
    // width: auto, margins: 0, borders/padding: 0
    // Result should fill the container
    assert_eq!(result.content_width, 800.0);
  }

  #[test]
  fn test_width_specified_with_auto_margins_centers() {
    let mut style = default_style();
    style.width = Some(Length::px(400.0));

    let result = compute_block_width_with_auto_margins(&style, 800.0, true, true, viewport());

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

    let result = compute_block_width_with_auto_margins(&style, 800.0, true, false, viewport());

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

    let result = compute_block_width_with_auto_margins(&style, 800.0, false, true, viewport());

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

    let result = compute_block_width(&style, 800.0, viewport());

    // Over-constrained: width: 400px, margins: 100px each
    // Total would be 600px in 800px container
    // margin-right should be recomputed: 800 - 400 - 100 = 300
    assert_eq!(result.content_width, 400.0);
    assert_eq!(result.margin_left, 100.0);
    assert_eq!(result.margin_right, 300.0);
  }

  #[test]
  fn test_width_over_constrained_rtl_ignores_left_margin() {
    let mut style = default_style();
    style.direction = Direction::Rtl;
    style.width = Some(Length::px(400.0));
    style.margin_left = Some(Length::px(150.0));
    style.margin_right = Some(Length::px(50.0));

    let result = compute_block_width(&style, 800.0, viewport());

    // In RTL the left margin is dropped when over-constrained. Remaining space goes to margin-left.
    // available = 800 - 400 - borders/padding(0) - margin-right(50) = 350
    assert_eq!(result.content_width, 400.0);
    assert_eq!(result.margin_left, 350.0);
    assert_eq!(result.margin_right, 50.0);
  }

  #[test]
  fn test_width_over_constrained_ltr_keeps_left_margin() {
    let mut style = default_style();
    style.direction = Direction::Ltr;
    style.width = Some(Length::px(400.0));
    style.margin_left = Some(Length::px(150.0));
    style.margin_right = Some(Length::px(50.0));

    let result = compute_block_width(&style, 800.0, viewport());

    // In LTR the right margin is dropped when over-constrained. Remaining space goes to margin-right.
    // available = 800 - 400 - 150 = 250
    assert_eq!(result.content_width, 400.0);
    assert_eq!(result.margin_left, 150.0);
    assert_eq!(result.margin_right, 250.0);
  }

  #[test]
  fn test_width_auto_with_margins() {
    let mut style = default_style();
    style.margin_left = Some(Length::px(50.0));
    style.margin_right = Some(Length::px(50.0));

    let result = compute_block_width(&style, 800.0, viewport());

    // width: auto, margins: 50px each
    // width should be 800 - 50 - 50 = 700
    assert_eq!(result.content_width, 700.0);
    assert_eq!(result.margin_left, 50.0);
    assert_eq!(result.margin_right, 50.0);
  }

  #[test]
  fn test_width_auto_margins_become_zero() {
    let style = default_style();

    let result = compute_block_width_with_auto_margins(&style, 800.0, true, true, viewport());

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

    let result = compute_block_width(&style, 800.0, viewport());

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

    let result = compute_block_width(&style, 800.0, viewport());

    // width: auto, border: 5px each side
    // content width should be 800 - 10 = 790
    assert_eq!(result.content_width, 790.0);
    assert_eq!(result.border_left, 5.0);
    assert_eq!(result.border_right, 5.0);
  }

  #[test]
  fn width_property_includes_padding_and_border_when_border_box() {
    let mut style = default_style();
    style.box_sizing = BoxSizing::BorderBox;
    style.width = Some(Length::px(200.0));
    style.padding_left = Length::px(10.0);
    style.padding_right = Length::px(10.0);
    style.border_left_width = Length::px(5.0);
    style.border_right_width = Length::px(5.0);

    let result = compute_block_width(&style, 400.0, viewport());

    assert_eq!(result.content_width, 170.0);
    assert_eq!(result.border_box_width(), 200.0);
  }

  #[test]
  fn test_width_percentage() {
    let mut style = default_style();
    style.width = Some(Length::percent(50.0));

    let result = compute_block_width(&style, 800.0, viewport());

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

    let result = compute_block_width(&style, 800.0, viewport());

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
