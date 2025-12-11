//! Shared layout utilities
//!
//! Contains common functions used across multiple layout modules.

use crate::geometry::Size;
use crate::style::computed::PositionedStyle;
use crate::style::types::{BoxSizing, FontStyle as CssFontStyle};
use crate::style::values::{Length, LengthOrAuto, LengthUnit};
use crate::style::ComputedStyle;
use crate::text::font_db::{FontStretch, FontStyle as FontFaceStyle};
use crate::text::font_loader::FontContext;
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
    resolve_length_with_percentage_metrics(length, percentage_base, viewport, font_size, root_font_size, None, None)
}

/// Resolves a length using percentage base, viewport, and optional metric-aware font resolution.
pub fn resolve_length_with_percentage_metrics(
    length: Length,
    percentage_base: Option<f32>,
    viewport: Size,
    font_size: f32,
    root_font_size: f32,
    style: Option<&ComputedStyle>,
    font_context: Option<&FontContext>,
) -> Option<f32> {
    if length.unit.is_percentage() {
        percentage_base.map(|b| length.resolve_against(b))
    } else if length.unit.is_viewport_relative() {
        Some(length.resolve_with_viewport(viewport.width, viewport.height))
    } else if length.unit.is_font_relative() {
        match (style, font_context) {
            (Some(style), Some(ctx)) => Some(resolve_font_relative_length(length, style, ctx)),
            _ => Some(resolve_font_relative(length, font_size, root_font_size)),
        }
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
    resolve_offset_with_metrics(value, percentage_base, viewport, font_size, root_font_size, None, None)
}

/// Metric-aware offset resolver (uses font metrics when style/context provided).
pub fn resolve_offset_with_metrics(
    value: &LengthOrAuto,
    percentage_base: f32,
    viewport: crate::geometry::Size,
    font_size: f32,
    root_font_size: f32,
    style: Option<&ComputedStyle>,
    font_context: Option<&FontContext>,
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
                match (style, font_context) {
                    (Some(style), Some(ctx)) => Some(resolve_font_relative_length(*length, style, ctx)),
                    _ => Some(resolve_font_relative(*length, font_size, root_font_size)),
                }
            }
        }
    }
}

/// Resolves offsets using a positioned style (font-relative units use font metrics).
pub fn resolve_offset_for_positioned(
    value: &LengthOrAuto,
    percentage_base: f32,
    viewport: crate::geometry::Size,
    style: &PositionedStyle,
    font_context: &FontContext,
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
                Some(resolve_font_relative_length_for_positioned(
                    *length,
                    style,
                    font_context,
                ))
            }
        }
    }
}

/// Resolves font-relative lengths (em/ex/ch/rem) using font metrics and font-size-adjust.
pub fn resolve_font_relative_length(length: Length, style: &ComputedStyle, font_context: &FontContext) -> f32 {
    resolve_font_relative_length_with_params(
        length,
        style.font_size,
        style.root_font_size,
        &style.font_family,
        style.font_weight.to_u16(),
        style.font_style,
        style.font_stretch,
        style.font_size_adjust,
        font_context,
    )
}

pub fn resolve_font_relative_length_for_positioned(
    length: Length,
    style: &PositionedStyle,
    font_context: &FontContext,
) -> f32 {
    let font_style = match style.font_style {
        crate::style::computed::FontStyle::Normal => CssFontStyle::Normal,
        crate::style::computed::FontStyle::Italic => CssFontStyle::Italic,
        crate::style::computed::FontStyle::Oblique => CssFontStyle::Oblique(None),
    };
    resolve_font_relative_length_with_params(
        length,
        style.font_size,
        style.root_font_size,
        &style.font_family,
        style.font_weight,
        font_style,
        style.font_stretch,
        style.font_size_adjust,
        font_context,
    )
}

fn resolve_font_relative_length_with_params(
    length: Length,
    font_size: f32,
    root_font_size: f32,
    font_family: &[String],
    font_weight: u16,
    font_style: CssFontStyle,
    font_stretch: crate::style::types::FontStretch,
    font_size_adjust: crate::style::types::FontSizeAdjust,
    font_context: &FontContext,
) -> f32 {
    let face_style = match font_style {
        CssFontStyle::Normal => FontFaceStyle::Normal,
        CssFontStyle::Italic => FontFaceStyle::Italic,
        CssFontStyle::Oblique(_) => FontFaceStyle::Oblique,
    };
    let face_stretch = FontStretch::from_percentage(font_stretch.to_percentage());

    let maybe_font = font_context
        .get_font_full(font_family, font_weight, face_style, face_stretch)
        .or_else(|| font_context.get_sans_serif());

    let mut desired_aspect = match font_size_adjust {
        crate::style::types::FontSizeAdjust::Number(n) if n > 0.0 => Some(n),
        _ => None,
    };

    let (used_size, x_height, ch_width) = if let Some(font) = maybe_font {
        if desired_aspect.is_none() {
            desired_aspect = match font_size_adjust {
                crate::style::types::FontSizeAdjust::FromFont => font.metrics().ok().and_then(|m| m.aspect_ratio()),
                _ => None,
            };
        }

        let used_size = if let Some(desired) = desired_aspect {
            let actual = font.metrics().ok().and_then(|m| m.aspect_ratio()).unwrap_or(0.5);
            if actual > 0.0 {
                font_size * (desired / actual)
            } else {
                font_size
            }
        } else {
            font_size
        };

        let mut x_height = None;
        let mut ch_width = None;
        if let Ok(metrics) = font.metrics() {
            let scaled = metrics.scale(used_size);
            x_height = scaled.x_height;
        }
        if let Ok(face) = font.as_ttf_face() {
            if let Some(advance) = face.glyph_index('0').and_then(|g| face.glyph_hor_advance(g)) {
                let units_per_em = face.units_per_em();
                if units_per_em != 0 {
                    let scale = used_size / (units_per_em as f32);
                    ch_width = Some(advance as f32 * scale);
                }
            }
        }
        (
            used_size,
            x_height.unwrap_or(used_size * 0.5),
            ch_width.unwrap_or(used_size * 0.5),
        )
    } else {
        let used_size = font_size;
        (used_size, used_size * 0.5, used_size * 0.5)
    };

    match length.unit {
        LengthUnit::Em => length.value * used_size,
        LengthUnit::Ex => length.value * x_height,
        LengthUnit::Ch => length.value * ch_width,
        LengthUnit::Rem => length.value * root_font_size,
        _ => length.resolve_with_font_size(font_size),
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

    let intrinsic = replaced.intrinsic_size;
    let intrinsic_w = intrinsic.and_then(|s| (s.width > 0.0).then_some(s.width));
    let intrinsic_h = intrinsic.and_then(|s| (s.height > 0.0).then_some(s.height));
    let specified_ratio = match style.aspect_ratio {
        crate::style::types::AspectRatio::Ratio(r) if r > 0.0 => Some(r),
        _ => None,
    };

    let intrinsic_ratio =
        specified_ratio
            .or_else(|| replaced.aspect_ratio)
            .or_else(|| match (intrinsic_w, intrinsic_h) {
                (Some(w), Some(h)) if h > 0.0 => Some(w / h),
                _ => None,
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

    let specified_w = style
        .width
        .as_ref()
        .and_then(|l| resolve_replaced_length(l, width_base, viewport, font_size, root_font_size))
        .map(|w| content_size_from_box_sizing(w, horizontal_edges, style.box_sizing));
    let specified_h = style
        .height
        .as_ref()
        .and_then(|l| resolve_replaced_length(l, height_base, viewport, font_size, root_font_size))
        .map(|h| content_size_from_box_sizing(h, vertical_edges, style.box_sizing));

    let mut width: f32;
    let mut height: f32;

    match (specified_w, specified_h) {
        (Some(w), Some(h)) => {
            width = w;
            height = h;
        }
        (Some(w), None) => {
            width = w;
            height = if let Some(r) = intrinsic_ratio {
                w / r
            } else if let Some(h) = intrinsic_h {
                h
            } else {
                DEFAULT_SIZE.height
            };
        }
        (None, Some(h)) => {
            height = h;
            width = if let Some(r) = intrinsic_ratio {
                h * r
            } else if let Some(w) = intrinsic_w {
                w
            } else {
                DEFAULT_SIZE.width
            };
        }
        (None, None) => match (intrinsic_w, intrinsic_h, intrinsic_ratio) {
            (Some(w), Some(h), _) => {
                width = w;
                height = h;
            }
            (Some(w), None, Some(r)) => {
                width = w;
                height = w / r;
            }
            (None, Some(h), Some(r)) => {
                height = h;
                width = h * r;
            }
            (Some(w), None, None) => {
                width = w;
                height = DEFAULT_SIZE.height;
            }
            (None, Some(h), None) => {
                width = DEFAULT_SIZE.width;
                height = h;
            }
            (None, None, Some(r)) => {
                width = DEFAULT_SIZE.width;
                height = width / r;
            }
            _ => {
                width = DEFAULT_SIZE.width;
                height = DEFAULT_SIZE.height;
            }
        },
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

    Size::new(width.max(0.0), height.max(0.0))
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
    fn test_replaced_auto_auto_ratio_only_uses_default_with_ratio() {
        let style = ComputedStyle::default();
        let replaced = ReplacedBox {
            replaced_type: crate::tree::box_tree::ReplacedType::Image {
                src: "".to_string(),
                alt: None,
            },
            intrinsic_size: None,
            aspect_ratio: Some(2.0),
        };

        let size = compute_replaced_size(&style, &replaced, None, Size::new(800.0, 600.0));
        assert_eq!(size.width, 300.0);
        assert_eq!(size.height, 150.0);
    }

    #[test]
    fn test_replaced_aspect_ratio_property_overrides_intrinsic_ratio() {
        let mut style = ComputedStyle::default();
        style.aspect_ratio = crate::style::types::AspectRatio::Ratio(1.5);

        let replaced = ReplacedBox {
            replaced_type: crate::tree::box_tree::ReplacedType::Image {
                src: "".to_string(),
                alt: None,
            },
            intrinsic_size: Some(Size::new(120.0, 0.0)),
            aspect_ratio: Some(2.0),
        };

        let size = compute_replaced_size(&style, &replaced, None, Size::new(800.0, 600.0));
        assert!((size.width - 120.0).abs() < 0.01);
        assert!((size.height - 80.0).abs() < 0.01);
    }

    #[test]
    fn test_replaced_aspect_ratio_property_with_specified_height() {
        let mut style = ComputedStyle::default();
        style.aspect_ratio = crate::style::types::AspectRatio::Ratio(2.0);
        style.height = Some(Length::px(75.0));

        let replaced = ReplacedBox {
            replaced_type: crate::tree::box_tree::ReplacedType::Image {
                src: "".to_string(),
                alt: None,
            },
            intrinsic_size: None,
            aspect_ratio: None,
        };

        let size = compute_replaced_size(&style, &replaced, None, Size::new(800.0, 600.0));
        assert!((size.width - 150.0).abs() < 0.01);
        assert!((size.height - 75.0).abs() < 0.01);
    }

    #[test]
    fn test_replaced_auto_auto_intrinsic_width_only_falls_back_height() {
        let style = ComputedStyle::default();
        let replaced = ReplacedBox {
            replaced_type: crate::tree::box_tree::ReplacedType::Image {
                src: "".to_string(),
                alt: None,
            },
            intrinsic_size: Some(Size::new(120.0, 0.0)),
            aspect_ratio: None,
        };

        let size = compute_replaced_size(&style, &replaced, None, Size::new(800.0, 600.0));
        assert_eq!(size.width, 120.0);
        assert_eq!(size.height, 150.0);
    }

    #[test]
    fn test_replaced_auto_auto_intrinsic_height_only_falls_back_width() {
        let style = ComputedStyle::default();
        let replaced = ReplacedBox {
            replaced_type: crate::tree::box_tree::ReplacedType::Image {
                src: "".to_string(),
                alt: None,
            },
            intrinsic_size: Some(Size::new(0.0, 80.0)),
            aspect_ratio: None,
        };

        let size = compute_replaced_size(&style, &replaced, None, Size::new(800.0, 600.0));
        assert_eq!(size.width, 300.0);
        assert_eq!(size.height, 80.0);
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
    fn test_replaced_min_width_clamps_content_size() {
        let mut style = ComputedStyle::default();
        style.min_width = Some(Length::px(250.0));
        style.height = Some(Length::px(50.0));
        let replaced = ReplacedBox {
            replaced_type: crate::tree::box_tree::ReplacedType::Image {
                src: "".to_string(),
                alt: None,
            },
            intrinsic_size: Some(Size::new(100.0, 50.0)),
            aspect_ratio: Some(2.0),
        };

        let size = compute_replaced_size(&style, &replaced, None, Size::new(800.0, 600.0));
        assert_eq!(size.width, 250.0);
        // Height clamps independently; we do not re-derive ratio after constraints.
        assert_eq!(size.height, 50.0);
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
