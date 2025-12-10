//! Shared layout utilities
//!
//! Contains common functions used across multiple layout modules.

use crate::geometry::Size;
use crate::style::types::BoxSizing;
use crate::style::values::{Length, LengthOrAuto, LengthUnit};
use crate::style::ComputedStyle;
use crate::tree::box_tree::ReplacedBox;

/// Resolves a length using the provided percentage base, font size, and root font size.
///
/// Returns `None` when a percentage cannot be resolved due to a missing base.
pub fn resolve_length_with_percentage(
    length: Length,
    percentage_base: Option<f32>,
    viewport: Size,
    font_size: f32,
    root_font_size: f32,
) -> Option<f32> {
    if length.unit.is_percentage() {
        percentage_base.map(|b| length.resolve_against(b))
    } else if length.unit.is_viewport_relative() {
        Some(length.resolve_with_viewport(viewport.width, viewport.height))
    } else if length.unit.is_font_relative() {
        Some(resolve_font_relative(length, font_size, root_font_size))
    } else if length.unit.is_absolute() {
        Some(length.to_px())
    } else {
        // Viewport/unknown units should already have been handled earlier; fall back to raw value.
        Some(length.value)
    }
}

/// Resolves a length-or-auto value to an optional pixel value.
///
/// Used for CSS offset properties (top, right, bottom, left) in positioned layout.
///
/// # Arguments
///
/// * `value` - The length or auto value to resolve
/// * `percentage_base` - The base value for percentage resolution
///
/// # Returns
///
/// * `None` if the value is `auto`
/// * `Some(pixels)` if the value is a length
///
/// # CSS Reference
///
/// CSS 2.1 Section 10.3.7 and 10.6.4 describe how offsets are resolved
/// for absolutely positioned elements.
pub fn resolve_offset(
    value: &LengthOrAuto,
    percentage_base: f32,
    viewport: crate::geometry::Size,
    font_size: f32,
    root_font_size: f32,
) -> Option<f32> {
    match value {
        LengthOrAuto::Auto => None,
        LengthOrAuto::Length(length) => {
            if length.unit.is_percentage() {
                Some(length.resolve_against(percentage_base))
            } else if length.unit.is_absolute() {
                Some(length.to_px())
            } else if length.unit.is_viewport_relative() {
                Some(length.resolve_with_viewport(viewport.width, viewport.height))
            } else {
                Some(resolve_font_relative(*length, font_size, root_font_size))
            }
        }
    }
}

/// Converts a specified box dimension into a content-box dimension based on box-sizing.
pub fn content_size_from_box_sizing(value: f32, edges: f32, box_sizing: BoxSizing) -> f32 {
    match box_sizing {
        BoxSizing::ContentBox => value,
        BoxSizing::BorderBox => (value - edges).max(0.0),
    }
}

/// Converts a specified box dimension into a border-box dimension based on box-sizing.
pub fn border_size_from_box_sizing(value: f32, edges: f32, box_sizing: BoxSizing) -> f32 {
    match box_sizing {
        BoxSizing::ContentBox => value + edges,
        BoxSizing::BorderBox => value,
    }
}

/// Computes the used size of a replaced element based on style and intrinsic data.
///
/// Implements a simplified form of CSS 2.1 §10.3.2/§10.6.2:
/// - Specified width/height override intrinsic dimensions
/// - If only one dimension is specified and an aspect ratio is available, the other is derived
/// - If nothing is specified and no intrinsic data exists, fall back to 300x150
pub fn compute_replaced_size(
    style: &ComputedStyle,
    replaced: &ReplacedBox,
    percentage_base: Option<Size>,
    viewport: Size,
) -> Size {
    const DEFAULT_SIZE: Size = Size {
        width: 300.0,
        height: 150.0,
    };

    let intrinsic = replaced.intrinsic_size.unwrap_or(DEFAULT_SIZE);
    let intrinsic_ratio = replaced.aspect_ratio.or_else(|| {
        if intrinsic.height > 0.0 {
            Some(intrinsic.width / intrinsic.height)
        } else {
            None
        }
    });

    let width_base = percentage_base.and_then(|s| s.width.is_finite().then_some(s.width));
    let height_base = percentage_base.and_then(|s| s.height.is_finite().then_some(s.height));
    let font_size = style.font_size;
    let root_font_size = style.root_font_size;

    let resolve_for_width =
        |len: Length| resolve_replaced_length(&len, width_base, viewport, font_size, root_font_size).unwrap_or(0.0);
    let resolve_for_height =
        |len: Length| resolve_replaced_length(&len, height_base, viewport, font_size, root_font_size).unwrap_or(0.0);

    let horizontal_edges = resolve_for_width(style.padding_left)
        + resolve_for_width(style.padding_right)
        + resolve_for_width(style.border_left_width)
        + resolve_for_width(style.border_right_width);
    let vertical_edges = resolve_for_height(style.padding_top)
        + resolve_for_height(style.padding_bottom)
        + resolve_for_height(style.border_top_width)
        + resolve_for_height(style.border_bottom_width);

    let mut width = style
        .width
        .as_ref()
        .and_then(|l| resolve_replaced_length(l, width_base, viewport, font_size, root_font_size))
        .map(|w| content_size_from_box_sizing(w, horizontal_edges, style.box_sizing))
        .unwrap_or(intrinsic.width);
    let mut height = style
        .height
        .as_ref()
        .and_then(|l| resolve_replaced_length(l, height_base, viewport, font_size, root_font_size))
        .map(|h| content_size_from_box_sizing(h, vertical_edges, style.box_sizing))
        .unwrap_or(intrinsic.height);

    match (style.width.as_ref(), style.height.as_ref(), intrinsic_ratio) {
        (Some(_), None, Some(ratio)) => {
            // Width specified, derive height from aspect ratio
            height = width / ratio;
        }
        (None, Some(_), Some(ratio)) => {
            // Height specified, derive width from aspect ratio
            width = height * ratio;
        }
        (None, None, None) => {
            // No information at all, fall back to default object size
            width = DEFAULT_SIZE.width;
            height = DEFAULT_SIZE.height;
        }
        _ => {}
    }

    // Apply min/max constraints when present
    if let Some(min_w) = style
        .min_width
        .as_ref()
        .and_then(|l| resolve_replaced_length(l, width_base, viewport, font_size, root_font_size))
        .map(|w| content_size_from_box_sizing(w, horizontal_edges, style.box_sizing))
    {
        width = width.max(min_w);
    }
    if let Some(max_w) = style
        .max_width
        .as_ref()
        .and_then(|l| resolve_replaced_length(l, width_base, viewport, font_size, root_font_size))
        .map(|w| content_size_from_box_sizing(w, horizontal_edges, style.box_sizing))
    {
        width = width.min(max_w);
    }
    if let Some(min_h) = style
        .min_height
        .as_ref()
        .and_then(|l| resolve_replaced_length(l, height_base, viewport, font_size, root_font_size))
        .map(|h| content_size_from_box_sizing(h, vertical_edges, style.box_sizing))
    {
        height = height.max(min_h);
    }
    if let Some(max_h) = style
        .max_height
        .as_ref()
        .and_then(|l| resolve_replaced_length(l, height_base, viewport, font_size, root_font_size))
        .map(|h| content_size_from_box_sizing(h, vertical_edges, style.box_sizing))
    {
        height = height.min(max_h);
    }

    Size::new(width, height)
}

fn resolve_replaced_length(
    len: &Length,
    percentage_base: Option<f32>,
    viewport: Size,
    font_size: f32,
    root_font_size: f32,
) -> Option<f32> {
    if len.unit.is_percentage() {
        percentage_base.map(|b| len.resolve_against(b))
    } else if len.unit.is_viewport_relative() {
        Some(len.resolve_with_viewport(viewport.width, viewport.height))
    } else if len.unit.is_font_relative() {
        Some(resolve_font_relative(*len, font_size, root_font_size))
    } else if len.unit.is_absolute() {
        Some(len.to_px())
    } else {
        Some(len.value)
    }
}

fn resolve_font_relative(len: Length, font_size: f32, root_font_size: f32) -> f32 {
    match len.unit {
        LengthUnit::Em => len.value * font_size,
        LengthUnit::Ex => len.value * font_size * 0.5,
        LengthUnit::Ch => len.value * font_size * 0.5,
        LengthUnit::Rem => len.value * root_font_size,
        _ => len.resolve_with_font_size(font_size),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::style::types::BoxSizing;
    use crate::style::values::{Length, LengthUnit};
    use crate::style::ComputedStyle;
    use crate::tree::box_tree::ReplacedBox;

    #[test]
    fn test_resolve_offset_auto() {
        assert_eq!(
            resolve_offset(&LengthOrAuto::Auto, 100.0, Size::new(800.0, 600.0), 16.0, 16.0),
            None
        );
    }

    #[test]
    fn test_resolve_offset_pixels() {
        let value = LengthOrAuto::Length(Length::new(50.0, LengthUnit::Px));
        assert_eq!(
            resolve_offset(&value, 100.0, Size::new(800.0, 600.0), 16.0, 16.0),
            Some(50.0)
        );
    }

    #[test]
    fn test_resolve_offset_percentage() {
        let value = LengthOrAuto::Length(Length::new(25.0, LengthUnit::Percent));
        assert_eq!(
            resolve_offset(&value, 200.0, Size::new(800.0, 600.0), 16.0, 16.0),
            Some(50.0)
        );
    }

    #[test]
    fn test_resolve_offset_em_fallback() {
        let value = LengthOrAuto::Length(Length::new(2.0, LengthUnit::Em));
        assert_eq!(
            resolve_offset(&value, 100.0, Size::new(800.0, 600.0), 16.0, 16.0),
            Some(32.0)
        );
    }

    #[test]
    fn test_resolve_offset_viewport() {
        let value = LengthOrAuto::Length(Length::new(10.0, LengthUnit::Vw));
        assert_eq!(
            resolve_offset(&value, 100.0, Size::new(500.0, 400.0), 16.0, 16.0),
            Some(50.0)
        );
    }

    #[test]
    fn test_compute_replaced_size_prefers_intrinsic() {
        let mut style = ComputedStyle::default();
        style.width = None;
        style.height = None;

        let replaced = ReplacedBox {
            replaced_type: crate::tree::box_tree::ReplacedType::Image {
                src: "img".into(),
                alt: None,
            },
            intrinsic_size: Some(Size::new(640.0, 480.0)),
            aspect_ratio: None,
        };

        let size = compute_replaced_size(&style, &replaced, None, Size::new(800.0, 600.0));
        assert_eq!(size.width, 640.0);
        assert_eq!(size.height, 480.0);
    }

    #[test]
    fn test_compute_replaced_size_derives_missing_dimension() {
        let mut style = ComputedStyle::default();
        style.width = Some(Length::px(200.0));
        style.height = None;

        let replaced = ReplacedBox {
            replaced_type: crate::tree::box_tree::ReplacedType::Image {
                src: "img".into(),
                alt: None,
            },
            intrinsic_size: Some(Size::new(100.0, 50.0)),
            aspect_ratio: Some(2.0),
        };

        let size = compute_replaced_size(&style, &replaced, None, Size::new(800.0, 600.0));
        assert_eq!(size.width, 200.0);
        assert_eq!(size.height, 100.0);
    }

    #[test]
    fn compute_replaced_respects_percentage_when_base_available() {
        let mut style = ComputedStyle::default();
        style.width = Some(Length::percent(50.0));
        style.height = Some(Length::percent(25.0));
        let replaced = ReplacedBox {
            replaced_type: crate::tree::box_tree::ReplacedType::Image {
                src: "img".into(),
                alt: None,
            },
            intrinsic_size: Some(Size::new(200.0, 200.0)),
            aspect_ratio: None,
        };

        let size = compute_replaced_size(
            &style,
            &replaced,
            Some(Size::new(400.0, 300.0)),
            Size::new(800.0, 600.0),
        );
        assert!((size.width - 200.0).abs() < 0.01);
        assert!((size.height - 75.0).abs() < 0.01);
    }

    #[test]
    fn compute_replaced_ignores_percentage_without_base() {
        let mut style = ComputedStyle::default();
        style.width = Some(Length::percent(50.0));
        style.height = None;

        let replaced = ReplacedBox {
            replaced_type: crate::tree::box_tree::ReplacedType::Image {
                src: "img".into(),
                alt: None,
            },
            intrinsic_size: Some(Size::new(120.0, 80.0)),
            aspect_ratio: Some(1.5),
        };

        let size = compute_replaced_size(&style, &replaced, None, Size::new(800.0, 600.0));
        // Percentage width cannot resolve, so we fall back to intrinsic and aspect ratio for height
        assert!((size.width - 120.0).abs() < 0.01);
        assert!((size.height - 80.0).abs() < 0.01);
    }

    #[test]
    fn compute_replaced_applies_border_box_box_sizing() {
        let mut style = ComputedStyle::default();
        style.box_sizing = BoxSizing::BorderBox;
        style.width = Some(Length::px(120.0));
        style.padding_left = Length::px(10.0);
        style.padding_right = Length::px(10.0);
        style.border_left_width = Length::px(5.0);
        style.border_right_width = Length::px(5.0);

        let replaced = ReplacedBox {
            replaced_type: crate::tree::box_tree::ReplacedType::Image {
                src: "img".into(),
                alt: None,
            },
            intrinsic_size: Some(Size::new(300.0, 150.0)),
            aspect_ratio: Some(2.0),
        };

        let size = compute_replaced_size(&style, &replaced, None, Size::new(800.0, 600.0));
        assert!((size.width - 90.0).abs() < 0.01);
    }
}
