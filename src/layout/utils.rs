//! Shared layout utilities
//!
//! Contains common functions used across multiple layout modules.

use crate::geometry::Size;
use crate::style::computed::PositionedStyle;
use crate::style::types::{BoxSizing, FontStyle as CssFontStyle, ScrollbarWidth};
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
    if length.unit == LengthUnit::Calc {
        return length.resolve_with_context(
            percentage_base,
            viewport.width,
            viewport.height,
            font_size,
            root_font_size,
        );
    }
    if length.unit.is_percentage() {
        percentage_base.and_then(|b| length.resolve_against(b))
    } else if length.unit.is_viewport_relative() {
        if !viewport.width.is_finite() || !viewport.height.is_finite() {
            return None;
        }
        length.resolve_with_viewport(viewport.width, viewport.height)
    } else if length.unit.is_font_relative() {
        if !font_size.is_finite() || !root_font_size.is_finite() {
            return None;
        }
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

/// Resolves the CSS `scrollbar-width` keyword to an approximate physical width in CSS pixels.
///
/// CSS leaves the exact thickness up to the user agent. We choose representative values to
/// reserve space for scrollbars during layout:
/// - `auto`: typical platform scrollbar thickness (16px)
/// - `thin`: a slimmer scrollbar (8px)
/// - `none`: no space reserved
pub fn resolve_scrollbar_width(style: &ComputedStyle) -> f32 {
    match style.scrollbar_width {
        ScrollbarWidth::Auto => 16.0,
        ScrollbarWidth::Thin => 8.0,
        ScrollbarWidth::None => 0.0,
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
            if length.unit == LengthUnit::Calc {
                return length.resolve_with_context(
                    Some(percentage_base),
                    viewport.width,
                    viewport.height,
                    font_size,
                    root_font_size,
                );
            }
            if length.unit.is_percentage() {
                length.resolve_against(percentage_base)
            } else if length.unit.is_absolute() {
                Some(length.to_px())
            } else if length.unit.is_viewport_relative() {
                length.resolve_with_viewport(viewport.width, viewport.height)
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
    percentage_base: Option<f32>,
    viewport: crate::geometry::Size,
    style: &PositionedStyle,
    font_context: &FontContext,
) -> Option<f32> {
    match value {
        LengthOrAuto::Auto => None,
        LengthOrAuto::Length(length) => {
            if length.unit == LengthUnit::Calc {
                return length.resolve_with_context(
                    percentage_base,
                    viewport.width,
                    viewport.height,
                    style.font_size,
                    style.root_font_size,
                );
            }
            if length.unit.is_percentage() {
                percentage_base.and_then(|base| length.resolve_against(base))
            } else if length.unit.is_absolute() {
                Some(length.to_px())
            } else if length.unit.is_viewport_relative() {
                length.resolve_with_viewport(viewport.width, viewport.height)
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
        crate::style::computed::FontStyle::Oblique(angle) => CssFontStyle::Oblique(angle),
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

/// Clamps a value, tolerating inverted bounds and non-finite endpoints.
///
/// f32::clamp panics when `min > max` or either bound is NaN. This helper swaps
/// inverted bounds and treats non-finite bounds as unbounded on that side.
pub fn clamp_with_order(value: f32, min: f32, max: f32) -> f32 {
    let mut lo = min;
    let mut hi = max;

    // Treat NaN bounds as unbounded.
    let lo_finite = lo.is_finite();
    let hi_finite = hi.is_finite();
    if !lo_finite && !hi_finite {
        return value;
    } else if !lo_finite {
        return value.min(hi);
    } else if !hi_finite {
        return value.max(lo);
    }

    if lo > hi {
        std::mem::swap(&mut lo, &mut hi);
    }
    value.max(lo).min(hi)
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
        _ => length
            .resolve_with_font_size(font_size)
            .unwrap_or(length.value * font_size),
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
    let sanitize = |v: f32| if v.is_finite() && v >= 0.0 { v } else { 0.0 };
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

    let resolve_for_width = |len: Length| {
        sanitize(resolve_replaced_length(&len, width_base, viewport, font_size, root_font_size).unwrap_or(0.0))
    };
    let horizontal_edges = resolve_for_width(style.padding_left)
        + resolve_for_width(style.padding_right)
        + resolve_for_width(style.border_left_width)
        + resolve_for_width(style.border_right_width);
    // Percentages on padding/borders resolve against the containing block width in both axes.
    let vertical_edges = resolve_for_width(style.padding_top)
        + resolve_for_width(style.padding_bottom)
        + resolve_for_width(style.border_top_width)
        + resolve_for_width(style.border_bottom_width);

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
    let width_specified = specified_w.is_some();
    let height_specified = specified_h.is_some();

    let mut width: f32;
    let mut height: f32;

    match (specified_w.map(sanitize), specified_h.map(sanitize)) {
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

    // Resolve min/max constraints once so we can reuse them during aspect-ratio adjustments.
    let resolved_min_w = style
        .min_width
        .as_ref()
        .and_then(|l| resolve_replaced_length(l, width_base, viewport, font_size, root_font_size))
        .map(|w| sanitize(content_size_from_box_sizing(w, horizontal_edges, style.box_sizing)));
    let resolved_max_w = style
        .max_width
        .as_ref()
        .and_then(|l| resolve_replaced_length(l, width_base, viewport, font_size, root_font_size))
        .map(|w| sanitize(content_size_from_box_sizing(w, horizontal_edges, style.box_sizing)));
    let resolved_min_h = style
        .min_height
        .as_ref()
        .and_then(|l| resolve_replaced_length(l, height_base, viewport, font_size, root_font_size))
        .map(|h| sanitize(content_size_from_box_sizing(h, vertical_edges, style.box_sizing)));
    let resolved_max_h = style
        .max_height
        .as_ref()
        .and_then(|l| resolve_replaced_length(l, height_base, viewport, font_size, root_font_size))
        .map(|h| sanitize(content_size_from_box_sizing(h, vertical_edges, style.box_sizing)));

    // Apply min/max constraints, preserving the intrinsic aspect ratio when only one axis was authored.
    let mut width_changed = false;
    if let Some(min_w) = resolved_min_w {
        if width < min_w {
            width = min_w;
            width_changed = true;
        }
    }
    if let Some(max_w) = resolved_max_w {
        if width > max_w {
            width = max_w;
            width_changed = true;
        }
    }
    if width_changed && !height_specified {
        if let Some(ratio) = intrinsic_ratio {
            height = width / ratio;
        }
    }

    let mut height_changed = false;
    if let Some(min_h) = resolved_min_h {
        if height < min_h {
            height = min_h;
            height_changed = true;
        }
    }
    if let Some(max_h) = resolved_max_h {
        if height > max_h {
            height = max_h;
            height_changed = true;
        }
    }
    if height_changed && !width_specified {
        if let Some(ratio) = intrinsic_ratio {
            width = height * ratio;
            if let Some(min_w) = resolved_min_w {
                width = width.max(min_w);
            }
            if let Some(max_w) = resolved_max_w {
                width = width.min(max_w);
            }
        }
    }

    width = sanitize(width);
    height = sanitize(height);
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
        percentage_base.and_then(|b| len.resolve_against(b))
    } else if len.unit.is_viewport_relative() {
        len.resolve_with_viewport(viewport.width, viewport.height)
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
        _ => len.resolve_with_font_size(font_size).unwrap_or(len.value * font_size),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::style::types::{BoxSizing, ScrollbarWidth};
    use crate::style::values::{Length, LengthUnit};
    use crate::style::ComputedStyle;
    use crate::tree::box_tree::ReplacedBox;

    #[test]
    fn resolve_length_with_percentage_rejects_non_finite_contexts() {
        let viewport = Size::new(f32::NAN, 800.0);

        assert_eq!(
            resolve_length_with_percentage(Length::percent(50.0), Some(f32::NAN), viewport, 16.0, 16.0),
            None
        );

        assert_eq!(
            resolve_length_with_percentage(Length::new(10.0, LengthUnit::Vw), Some(0.0), viewport, 16.0, 16.0),
            None
        );

        assert_eq!(
            resolve_length_with_percentage(Length::em(2.0), Some(0.0), Size::new(800.0, 600.0), f32::NAN, 16.0),
            None
        );
    }

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
                sizes: None,
                srcset: Vec::new(),
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
                sizes: None,
                srcset: Vec::new(),
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
                sizes: None,
                srcset: Vec::new(),
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
                sizes: None,
                srcset: Vec::new(),
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
                sizes: None,
                srcset: Vec::new(),
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
                sizes: None,
                srcset: Vec::new(),
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
                sizes: None,
                srcset: Vec::new(),
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
                sizes: None,
                srcset: Vec::new(),
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
    fn replaced_max_width_scales_height_with_ratio() {
        let mut style = ComputedStyle::default();
        style.max_width = Some(Length::px(1200.0));
        let replaced = ReplacedBox {
            replaced_type: crate::tree::box_tree::ReplacedType::Image {
                src: "img".into(),
                alt: None,
                sizes: None,
                srcset: Vec::new(),
            },
            intrinsic_size: Some(Size::new(2400.0, 1600.0)),
            aspect_ratio: Some(2400.0 / 1600.0),
        };

        let size = compute_replaced_size(
            &style,
            &replaced,
            Some(Size::new(1200.0, 800.0)),
            Size::new(1200.0, 800.0),
        );
        assert!((size.width - 1200.0).abs() < 0.01);
        assert!((size.height - 800.0).abs() < 0.01);
    }

    #[test]
    fn replaced_max_height_scales_width_with_ratio() {
        let mut style = ComputedStyle::default();
        style.max_height = Some(Length::px(40.0));
        let replaced = ReplacedBox {
            replaced_type: crate::tree::box_tree::ReplacedType::Image {
                src: "img".into(),
                alt: None,
                sizes: None,
                srcset: Vec::new(),
            },
            intrinsic_size: Some(Size::new(100.0, 50.0)),
            aspect_ratio: Some(2.0),
        };

        let size = compute_replaced_size(&style, &replaced, None, Size::new(800.0, 600.0));
        assert!((size.width - 80.0).abs() < 0.01);
        assert!((size.height - 40.0).abs() < 0.01);
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
                sizes: None,
                srcset: Vec::new(),
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
                sizes: None,
                srcset: Vec::new(),
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
    fn compute_replaced_ignores_min_height_percentage_without_base() {
        let mut style = ComputedStyle::default();
        style.min_height = Some(Length::percent(50.0));

        let replaced = ReplacedBox {
            replaced_type: crate::tree::box_tree::ReplacedType::Image {
                src: "img".into(),
                alt: None,
                sizes: None,
                srcset: Vec::new(),
            },
            intrinsic_size: Some(Size::new(120.0, 80.0)),
            aspect_ratio: None,
        };

        let size = compute_replaced_size(&style, &replaced, None, Size::new(800.0, 600.0));
        // Without a definite height base, percentage min-height is ignored.
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
                sizes: None,
                srcset: Vec::new(),
            },
            intrinsic_size: Some(Size::new(300.0, 150.0)),
            aspect_ratio: Some(2.0),
        };

        let size = compute_replaced_size(&style, &replaced, None, Size::new(800.0, 600.0));
        assert!((size.width - 90.0).abs() < 0.01);
    }

    #[test]
    fn compute_replaced_border_box_padding_uses_width_base() {
        let mut style = ComputedStyle::default();
        style.box_sizing = BoxSizing::BorderBox;
        style.height = Some(Length::px(100.0));
        style.padding_top = Length::percent(10.0);

        let replaced = ReplacedBox {
            replaced_type: crate::tree::box_tree::ReplacedType::Image {
                src: "img".into(),
                alt: None,
                sizes: None,
                srcset: Vec::new(),
            },
            intrinsic_size: None,
            aspect_ratio: None,
        };

        let size = compute_replaced_size(
            &style,
            &replaced,
            Some(Size::new(200.0, 400.0)),
            Size::new(800.0, 600.0),
        );

        // Padding percentages resolve against the containing block width, so 10% of 200px is
        // 20px. With box-sizing:border-box and a 100px specified height, the content box is 80px.
        assert!((size.height - 80.0).abs() < 0.01);
    }

    #[test]
    fn resolves_scrollbar_width_keywords() {
        let mut style = ComputedStyle::default();
        style.scrollbar_width = ScrollbarWidth::Auto;
        assert_eq!(resolve_scrollbar_width(&style), 16.0);

        style.scrollbar_width = ScrollbarWidth::Thin;
        assert_eq!(resolve_scrollbar_width(&style), 8.0);

        style.scrollbar_width = ScrollbarWidth::None;
        assert_eq!(resolve_scrollbar_width(&style), 0.0);
    }

    #[test]
    fn clamp_with_order_handles_normal_and_inverted_bounds() {
        assert_eq!(clamp_with_order(5.0, 1.0, 10.0), 5.0);
        assert_eq!(clamp_with_order(0.0, 1.0, 10.0), 1.0);
        assert_eq!(clamp_with_order(20.0, 1.0, 10.0), 10.0);

        // Inverted bounds are swapped internally.
        assert_eq!(clamp_with_order(5.0, 10.0, 1.0), 5.0);
        assert_eq!(clamp_with_order(0.0, 10.0, 1.0), 1.0);
        assert_eq!(clamp_with_order(20.0, 10.0, 1.0), 10.0);
    }

    #[test]
    fn clamp_with_order_handles_nan_bounds_as_unbounded() {
        let nan = f32::NAN;
        // NaN min -> unbounded below
        assert_eq!(clamp_with_order(5.0, nan, 2.0), 2.0);
        // NaN max -> unbounded above
        assert_eq!(clamp_with_order(1.0, 2.0, nan), 2.0);
        assert_eq!(clamp_with_order(5.0, 2.0, nan), 5.0);
        // Both NaN -> no clamping
        assert_eq!(clamp_with_order(7.0, nan, nan), 7.0);
    }

    #[test]
    fn clamp_with_order_handles_infinite_bounds() {
        let inf = f32::INFINITY;
        let ninf = f32::NEG_INFINITY;
        assert_eq!(clamp_with_order(5.0, 2.0, inf), 5.0);
        assert_eq!(clamp_with_order(1.0, 2.0, inf), 2.0);
        // Negative infinity lower bound behaves like unbounded below
        assert_eq!(clamp_with_order(5.0, ninf, 3.0), 3.0);
        assert_eq!(clamp_with_order(1.0, ninf, 3.0), 1.0);
    }
}
