//! Baseline alignment for inline layout
//!
//! This module implements baseline alignment for inline formatting context.
//! Baseline alignment determines how inline-level boxes are vertically positioned
//! within a line box.
//!
//! # CSS Specification
//!
//! CSS 2.1 Section 10.8 - Line height calculations:
//! <https://www.w3.org/TR/CSS21/visudet.html#line-height>
//!
//! # Vertical Alignment
//!
//! The `vertical-align` property affects inline-level boxes in several ways:
//!
//! - **baseline**: Align box baseline with parent baseline (default)
//! - **middle**: Align box vertical center with parent baseline + half x-height
//! - **sub**: Lower box baseline to parent subscript position
//! - **super**: Raise box baseline to parent superscript position
//! - **text-top**: Align box top with parent's text top
//! - **text-bottom**: Align box bottom with parent's text bottom
//! - **top**: Align box top with line box top
//! - **bottom**: Align box bottom with line box bottom
//! - **length**: Raise/lower by specified amount
//! - **percentage**: Raise/lower by percentage of line-height
//!
//! # Baseline Types
//!
//! Different boxes have different baselines:
//!
//! - **Text**: Font baseline (typically ~80% from top)
//! - **Inline boxes**: Baseline of first text child
//! - **Inline-block**: Bottom margin edge (or content baseline if has in-flow content)
//! - **Replaced elements**: Bottom margin edge

use crate::geometry::Size;
use crate::style::ComputedStyle;
use crate::text::font_db::FontMetrics;

/// Vertical alignment modes for inline elements
///
/// Corresponds to CSS `vertical-align` property values.
/// Reference: CSS 2.1 Section 10.8.1
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub enum VerticalAlign {
    /// Align baseline with parent's baseline (default)
    #[default]
    Baseline,

    /// Align vertical center with parent's baseline + half x-height
    Middle,

    /// Lower baseline to parent's subscript position
    Sub,

    /// Raise baseline to parent's superscript position
    Super,

    /// Align top with parent's text top
    TextTop,

    /// Align bottom with parent's text bottom
    TextBottom,

    /// Align top with line box top
    Top,

    /// Align bottom with line box bottom
    Bottom,

    /// Raise/lower by specific length (positive = up)
    Length(f32),

    /// Raise/lower by percentage of line-height
    Percentage(f32),
}

impl VerticalAlign {
    /// Returns true if this alignment is relative to the line box (not the parent)
    pub fn is_line_relative(&self) -> bool {
        matches!(self, VerticalAlign::Top | VerticalAlign::Bottom)
    }

    /// Returns true if this alignment uses the parent's baseline
    pub fn is_baseline_relative(&self) -> bool {
        matches!(
            self,
            VerticalAlign::Baseline
                | VerticalAlign::Middle
                | VerticalAlign::Sub
                | VerticalAlign::Super
                | VerticalAlign::TextTop
                | VerticalAlign::TextBottom
                | VerticalAlign::Length(_)
                | VerticalAlign::Percentage(_)
        )
    }
}

/// Metrics for baseline calculation
///
/// Contains all measurements needed to compute baseline positioning
/// for an inline item within a line box.
#[derive(Debug, Clone, Copy)]
pub struct BaselineMetrics {
    /// Distance from top of box to baseline
    pub baseline_offset: f32,

    /// Total height of the inline box
    pub height: f32,

    /// Font ascent (above baseline)
    pub ascent: f32,

    /// Font descent (below baseline, positive value)
    pub descent: f32,

    /// Line gap (extra spacing)
    pub line_gap: f32,

    /// Line height from CSS (may differ from ascent + descent)
    pub line_height: f32,

    /// Font x-height if available (used for middle alignment)
    pub x_height: Option<f32>,
}

impl BaselineMetrics {
    /// Creates metrics from font metrics and font size
    pub fn from_font_metrics(metrics: &FontMetrics, font_size: f32, line_height: f32) -> Self {
        let scaled = metrics.scale(font_size);
        Self {
            baseline_offset: scaled.ascent,
            height: line_height,
            ascent: scaled.ascent,
            descent: scaled.descent,
            line_gap: scaled.line_gap,
            line_height,
            x_height: scaled.x_height,
        }
    }

    /// Creates metrics for a replaced element (image, etc.)
    ///
    /// Replaced elements have their baseline at the bottom margin edge.
    pub fn for_replaced(height: f32) -> Self {
        Self {
            baseline_offset: height,
            height,
            ascent: height,
            descent: 0.0,
            line_gap: 0.0,
            line_height: height,
            x_height: None,
        }
    }

    /// Creates metrics for text with explicit values
    pub fn new(baseline_offset: f32, height: f32, ascent: f32, descent: f32) -> Self {
        Self {
            baseline_offset,
            height,
            ascent,
            descent,
            line_gap: 0.0,
            line_height: height,
            // When true font metrics are unavailable, approximate x-height as half the ascent so
            // vertical-align: middle still has a reasonable fallback.
            x_height: Some(ascent * 0.5),
        }
    }

    /// Half-leading above the text content
    ///
    /// Leading is the difference between line-height and the actual content height.
    /// It's split equally above and below.
    pub fn half_leading(&self) -> f32 {
        ((self.line_height - (self.ascent + self.descent)) / 2.0).max(0.0)
    }
}

/// Accumulated baseline information for a line
///
/// Tracks the maximum ascent and descent across all items in a line
/// to compute the final line height and baseline position.
#[derive(Debug, Clone, Default)]
pub struct LineBaselineAccumulator {
    /// Maximum ascent (distance above baseline)
    pub max_ascent: f32,

    /// Maximum descent (distance below baseline)
    pub max_descent: f32,

    /// Items with vertical-align: top/bottom need separate tracking
    pub top_aligned_height: f32,
    pub bottom_aligned_height: f32,

    /// Strut height (minimum from root inline box)
    pub strut_ascent: f32,
    pub strut_descent: f32,
}

impl LineBaselineAccumulator {
    /// Creates a new accumulator with strut from the root inline box
    ///
    /// The strut is an imaginary zero-width inline box with the element's font and line height.
    /// It provides a minimum height for the line box even if it's empty.
    pub fn new(strut_metrics: &BaselineMetrics) -> Self {
        let half_leading = strut_metrics.half_leading();
        let strut_ascent = strut_metrics.ascent + half_leading;
        let strut_descent = strut_metrics.descent + half_leading;

        Self {
            max_ascent: strut_ascent,
            max_descent: strut_descent,
            top_aligned_height: 0.0,
            bottom_aligned_height: 0.0,
            strut_ascent,
            strut_descent,
        }
    }

    /// Creates an accumulator with default strut values
    pub fn with_default_strut(font_size: f32, line_height: f32) -> Self {
        // Approximate standard font metrics
        let ascent = font_size * 0.8;
        let descent = font_size * 0.2;
        let half_leading = ((line_height - font_size) / 2.0).max(0.0);

        Self {
            max_ascent: ascent + half_leading,
            max_descent: descent + half_leading,
            top_aligned_height: 0.0,
            bottom_aligned_height: 0.0,
            strut_ascent: ascent + half_leading,
            strut_descent: descent + half_leading,
        }
    }

    /// Adds an item to the accumulator with baseline-relative alignment
    ///
    /// Returns the Y offset for the item relative to the line's baseline.
    pub fn add_baseline_relative(
        &mut self,
        metrics: &BaselineMetrics,
        alignment: VerticalAlign,
        parent_metrics: Option<&BaselineMetrics>,
    ) -> f32 {
        let baseline_shift = self.compute_baseline_shift(alignment, metrics, parent_metrics);

        // Compute this item's contribution to ascent/descent
        let item_ascent = metrics.baseline_offset + baseline_shift;
        let item_descent = (metrics.height - metrics.baseline_offset) - baseline_shift;

        self.max_ascent = self.max_ascent.max(item_ascent);
        self.max_descent = self.max_descent.max(item_descent);

        baseline_shift
    }

    /// Adds a line-relative (top/bottom) aligned item
    ///
    /// These items don't affect the baseline calculation but may extend
    /// the line box height.
    pub fn add_line_relative(&mut self, metrics: &BaselineMetrics, alignment: VerticalAlign) {
        match alignment {
            VerticalAlign::Top => {
                self.top_aligned_height = self.top_aligned_height.max(metrics.height);
            }
            VerticalAlign::Bottom => {
                self.bottom_aligned_height = self.bottom_aligned_height.max(metrics.height);
            }
            _ => {}
        }
    }

    /// Computes the baseline shift for an alignment mode
    fn compute_baseline_shift(
        &self,
        alignment: VerticalAlign,
        metrics: &BaselineMetrics,
        parent_metrics: Option<&BaselineMetrics>,
    ) -> f32 {
        match alignment {
            VerticalAlign::Baseline => 0.0,

            VerticalAlign::Middle => {
                // Align center of box with parent baseline + x-height/2.
                // Spec: middle aligns the box midpoint with the parent's x-height midpoint.
                let x_height_half = parent_metrics
                    .and_then(|m| m.x_height.map(|xh| xh * 0.5))
                    .or_else(|| parent_metrics.map(|m| m.ascent * 0.5))
                    .unwrap_or(0.0);
                metrics.baseline_offset - (metrics.height * 0.5) + x_height_half
            }

            VerticalAlign::Sub => {
                // Lower baseline by ~0.3em (typical subscript offset)
                let shift = parent_metrics.map(|m| m.ascent * 0.3).unwrap_or(metrics.ascent * 0.3);
                -shift
            }

            VerticalAlign::Super => {
                // Raise baseline by ~0.4em (typical superscript offset)
                let shift = parent_metrics.map(|m| m.ascent * 0.4).unwrap_or(metrics.ascent * 0.4);
                shift
            }

            VerticalAlign::TextTop => {
                // Align top with parent's text top
                if let Some(parent) = parent_metrics {
                    parent.ascent - metrics.baseline_offset
                } else {
                    0.0
                }
            }

            VerticalAlign::TextBottom => {
                // Align bottom with parent's text bottom
                if let Some(parent) = parent_metrics {
                    -(parent.descent - (metrics.height - metrics.baseline_offset))
                } else {
                    0.0
                }
            }

            VerticalAlign::Length(len) => len,

            VerticalAlign::Percentage(pct) => {
                // Percentage of line-height
                metrics.line_height * (pct / 100.0)
            }

            // Top/Bottom are handled separately
            VerticalAlign::Top | VerticalAlign::Bottom => 0.0,
        }
    }

    /// Computes the final line height
    pub fn line_height(&self) -> f32 {
        let baseline_height = self.max_ascent + self.max_descent;
        let top_bottom_height = self.top_aligned_height.max(self.bottom_aligned_height);
        baseline_height.max(top_bottom_height)
    }

    /// Computes the baseline position from the top of the line box
    pub fn baseline_position(&self) -> f32 {
        self.max_ascent
    }

    /// Computes the Y offset for a top-aligned item
    pub fn top_aligned_offset(&self) -> f32 {
        0.0
    }

    /// Computes the Y offset for a bottom-aligned item from its top edge
    pub fn bottom_aligned_offset(&self, item_height: f32) -> f32 {
        self.line_height() - item_height
    }
}

/// Compute line height from CSS line-height value and font size
///
/// Supports CSS line-height values:
/// - `normal`: Use font's default line spacing (typically 1.2x font-size)
/// - `<number>`: Multiply font-size by the number
/// - `<length>`: Use the absolute value
/// - `<percentage>`: Multiply font-size by percentage/100
pub fn compute_line_height(style: &ComputedStyle) -> f32 {
    compute_line_height_with_metrics(style, None)
}

/// Computes line height, optionally using scaled font metrics and viewport size when available.
pub fn compute_line_height_with_metrics_viewport(
    style: &ComputedStyle,
    metrics: Option<&crate::text::font_db::ScaledMetrics>,
    viewport: Option<Size>,
) -> f32 {
    use crate::style::types::LineHeight;

    let font_size = style.font_size;
    let (vw, vh) = viewport
        .and_then(|s| {
            if s.width.is_finite() && s.height.is_finite() {
                Some((s.width, s.height))
            } else {
                None
            }
        })
        .unwrap_or((1200.0, 800.0));

    match &style.line_height {
        LineHeight::Normal => {
            if let Some(m) = metrics {
                m.line_height
            } else {
                font_size * 1.2
            }
        }
        LineHeight::Number(n) => font_size * n,
        LineHeight::Length(len) => match len.unit {
            u if u.is_absolute() => len.to_px(),
            crate::style::values::LengthUnit::Em => len.value * font_size,
            crate::style::values::LengthUnit::Rem => len.value * style.root_font_size,
            crate::style::values::LengthUnit::Ex => {
                let x_height = metrics.and_then(|m| m.x_height).unwrap_or(font_size * 0.5);
                len.value * x_height
            }
            crate::style::values::LengthUnit::Ch => len.value * font_size * 0.5,
            u if u.is_viewport_relative() => len.resolve_with_viewport(vw, vh).unwrap_or(len.value),
            _ => len.value,
        },
        LineHeight::Percentage(pct) => font_size * (pct / 100.0),
    }
}

/// Computes line height, optionally using scaled font metrics when available.
pub fn compute_line_height_with_metrics(
    style: &ComputedStyle,
    metrics: Option<&crate::text::font_db::ScaledMetrics>,
) -> f32 {
    compute_line_height_with_metrics_viewport(style, metrics, None)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::text::font_db::ScaledMetrics;

    #[test]
    fn test_vertical_align_default() {
        let align = VerticalAlign::default();
        assert_eq!(align, VerticalAlign::Baseline);
    }

    #[test]
    fn test_vertical_align_is_line_relative() {
        assert!(VerticalAlign::Top.is_line_relative());
        assert!(VerticalAlign::Bottom.is_line_relative());
        assert!(!VerticalAlign::Baseline.is_line_relative());
        assert!(!VerticalAlign::Middle.is_line_relative());
    }

    #[test]
    fn test_vertical_align_is_baseline_relative() {
        assert!(VerticalAlign::Baseline.is_baseline_relative());
        assert!(VerticalAlign::Middle.is_baseline_relative());
        assert!(VerticalAlign::Sub.is_baseline_relative());
        assert!(VerticalAlign::Super.is_baseline_relative());
        assert!(VerticalAlign::Length(5.0).is_baseline_relative());
        assert!(!VerticalAlign::Top.is_baseline_relative());
        assert!(!VerticalAlign::Bottom.is_baseline_relative());
    }

    #[test]
    fn test_baseline_metrics_from_values() {
        let metrics = BaselineMetrics::new(12.0, 16.0, 12.0, 4.0);
        assert_eq!(metrics.baseline_offset, 12.0);
        assert_eq!(metrics.height, 16.0);
        assert_eq!(metrics.ascent, 12.0);
        assert_eq!(metrics.descent, 4.0);
    }

    #[test]
    fn test_baseline_metrics_for_replaced() {
        let metrics = BaselineMetrics::for_replaced(100.0);
        assert_eq!(metrics.baseline_offset, 100.0);
        assert_eq!(metrics.height, 100.0);
    }

    #[test]
    fn test_baseline_metrics_half_leading() {
        let metrics = BaselineMetrics {
            baseline_offset: 12.0,
            height: 24.0,
            ascent: 12.0,
            descent: 4.0,
            line_gap: 0.0,
            line_height: 24.0,
            x_height: None,
        };
        // line_height=24, ascent+descent=16, leading=8, half=4
        assert_eq!(metrics.half_leading(), 4.0);
    }

    #[test]
    fn middle_alignment_prefers_parent_x_height() {
        let mut acc = LineBaselineAccumulator::new(&BaselineMetrics::new(10.0, 14.0, 10.0, 4.0));

        let parent_with_x = BaselineMetrics {
            baseline_offset: 20.0,
            height: 30.0,
            ascent: 20.0,
            descent: 10.0,
            line_gap: 0.0,
            line_height: 30.0,
            x_height: Some(6.0),
        };
        let parent_no_x = BaselineMetrics {
            x_height: None,
            ..parent_with_x
        };

        let item = BaselineMetrics {
            baseline_offset: 8.0,
            height: 10.0,
            ascent: 8.0,
            descent: 2.0,
            line_gap: 0.0,
            line_height: 10.0,
            x_height: None,
        };

        let shift_with_x = acc.add_baseline_relative(&item, VerticalAlign::Middle, Some(&parent_with_x));
        let shift_without_x = acc.add_baseline_relative(&item, VerticalAlign::Middle, Some(&parent_no_x));

        assert!(
            (shift_with_x - 6.0).abs() < 1e-3,
            "middle should use parent x-height midpoint"
        );
        assert!(
            (shift_without_x - 13.0).abs() < 1e-3,
            "fallback uses 0.5em proxy when x-height is absent"
        );
        assert!(
            shift_with_x < shift_without_x,
            "x-height should produce a smaller shift than half-ascent"
        );
    }

    #[test]
    fn middle_alignment_applies_to_replaced_elements() {
        // Replaced elements use their bottom edge as baseline; vertical-align: middle should still
        // use the parent's x-height to shift them relative to the line.
        let parent = BaselineMetrics {
            baseline_offset: 18.0,
            height: 24.0,
            ascent: 18.0,
            descent: 6.0,
            line_gap: 0.0,
            line_height: 24.0,
            x_height: Some(6.0),
        };

        let replaced = BaselineMetrics::for_replaced(12.0);
        let mut acc = LineBaselineAccumulator::new(&parent);
        let shift = acc.add_baseline_relative(&replaced, VerticalAlign::Middle, Some(&parent));

        // Expected shift: x-height/2 (3) minus half the box height (6) plus baseline_offset (12) = 9
        assert!(
            (shift - 9.0).abs() < 1e-3,
            "unexpected middle shift for replaced element: {}",
            shift
        );
    }

    #[test]
    fn middle_alignment_respects_parent_x_height_with_overflowing_inline_block() {
        // Overflow-hidden inline-block uses bottom-edge baseline; middle alignment should still
        // align its midpoint with the parent's x-height midpoint.
        let parent = BaselineMetrics {
            baseline_offset: 16.0,
            height: 22.0,
            ascent: 16.0,
            descent: 6.0,
            line_gap: 0.0,
            line_height: 22.0,
            x_height: Some(8.0),
        };
        let child = BaselineMetrics::for_replaced(20.0); // baseline at bottom edge
        let mut acc = LineBaselineAccumulator::new(&parent);
        let shift = acc.add_baseline_relative(&child, VerticalAlign::Middle, Some(&parent));
        // parent x-height midpoint = 4; child midpoint (baseline_offset - height/2) = 10; shift = 10 + 4 = 14.
        assert!(
            (shift - 14.0).abs() < 1e-3,
            "middle shift should reflect parent x-height midpoint and child height"
        );
    }

    #[test]
    fn test_line_accumulator_baseline_alignment() {
        let strut = BaselineMetrics::new(12.0, 16.0, 12.0, 4.0);
        let mut acc = LineBaselineAccumulator::new(&strut);

        let item = BaselineMetrics::new(10.0, 14.0, 10.0, 4.0);
        let shift = acc.add_baseline_relative(&item, VerticalAlign::Baseline, None);

        assert_eq!(shift, 0.0);
        // Strut should still dominate: ascent=12+half_lead, descent=4+half_lead
    }

    #[test]
    fn test_line_accumulator_taller_item() {
        let strut = BaselineMetrics::new(12.0, 16.0, 12.0, 4.0);
        let mut acc = LineBaselineAccumulator::new(&strut);

        // Add item with bigger ascent
        let item = BaselineMetrics::new(20.0, 24.0, 20.0, 4.0);
        acc.add_baseline_relative(&item, VerticalAlign::Baseline, None);

        // Line height should grow to accommodate
        assert!(acc.line_height() > 16.0);
        assert!(acc.max_ascent >= 20.0);
    }

    #[test]
    fn test_line_accumulator_top_aligned() {
        let strut = BaselineMetrics::new(12.0, 16.0, 12.0, 4.0);
        let mut acc = LineBaselineAccumulator::new(&strut);

        let item = BaselineMetrics::new(30.0, 40.0, 30.0, 10.0);
        acc.add_line_relative(&item, VerticalAlign::Top);

        assert_eq!(acc.top_aligned_height, 40.0);
    }

    #[test]
    fn test_line_accumulator_bottom_aligned() {
        let strut = BaselineMetrics::new(12.0, 16.0, 12.0, 4.0);
        let mut acc = LineBaselineAccumulator::new(&strut);

        let item = BaselineMetrics::new(30.0, 40.0, 30.0, 10.0);
        acc.add_line_relative(&item, VerticalAlign::Bottom);

        assert_eq!(acc.bottom_aligned_height, 40.0);
    }

    #[test]
    fn test_baseline_shift_super() {
        let strut = BaselineMetrics::new(12.0, 16.0, 12.0, 4.0);
        let mut acc = LineBaselineAccumulator::new(&strut);

        let parent = BaselineMetrics::new(12.0, 16.0, 12.0, 4.0);
        let item = BaselineMetrics::new(8.0, 10.0, 8.0, 2.0);
        let shift = acc.add_baseline_relative(&item, VerticalAlign::Super, Some(&parent));

        // Super should raise the baseline (positive shift)
        assert!(shift > 0.0);
    }

    #[test]
    fn test_baseline_shift_sub() {
        let strut = BaselineMetrics::new(12.0, 16.0, 12.0, 4.0);
        let mut acc = LineBaselineAccumulator::new(&strut);

        let parent = BaselineMetrics::new(12.0, 16.0, 12.0, 4.0);
        let item = BaselineMetrics::new(8.0, 10.0, 8.0, 2.0);
        let shift = acc.add_baseline_relative(&item, VerticalAlign::Sub, Some(&parent));

        // Sub should lower the baseline (negative shift)
        assert!(shift < 0.0);
    }

    #[test]
    fn test_baseline_shift_length() {
        let strut = BaselineMetrics::new(12.0, 16.0, 12.0, 4.0);
        let mut acc = LineBaselineAccumulator::new(&strut);

        let item = BaselineMetrics::new(8.0, 10.0, 8.0, 2.0);
        let shift = acc.add_baseline_relative(&item, VerticalAlign::Length(5.0), None);

        assert_eq!(shift, 5.0);
    }

    #[test]
    fn test_line_height_calculation() {
        let strut = BaselineMetrics::new(12.0, 16.0, 12.0, 4.0);
        let acc = LineBaselineAccumulator::new(&strut);

        // Line height should be at least strut height (considering half-leading)
        assert!(acc.line_height() >= 16.0);
    }

    #[test]
    fn test_empty_line_uses_strut() {
        let strut = BaselineMetrics::new(16.0, 20.0, 16.0, 4.0);
        let acc = LineBaselineAccumulator::new(&strut);

        // Even empty line has strut height
        assert!(acc.line_height() > 0.0);
        assert!(acc.baseline_position() > 0.0);
    }

    #[test]
    fn compute_line_height_prefers_scaled_metrics_for_normal() {
        let mut style = ComputedStyle::default();
        style.line_height = crate::style::types::LineHeight::Normal;
        style.font_size = 16.0;
        let metrics = ScaledMetrics {
            font_size: 16.0,
            scale: 1.0,
            ascent: 10.0,
            descent: 4.0,
            line_gap: 2.0,
            line_height: 16.0,
            x_height: Some(8.0),
            cap_height: Some(12.0),
            underline_position: 2.0,
            underline_thickness: 1.0,
        };

        assert!((compute_line_height(&style) - 19.2).abs() < 0.01);
        assert!((compute_line_height_with_metrics(&style, Some(&metrics)) - 16.0).abs() < 0.01);
    }

    #[test]
    fn compute_line_height_viewport_relative_uses_real_viewport() {
        use crate::style::values::{Length, LengthUnit};

        let mut style = ComputedStyle::default();
        style.font_size = 10.0;
        style.line_height = crate::style::types::LineHeight::Length(Length::new(10.0, LengthUnit::Vh));

        // With a 500px high viewport, 10vh should be 50px. The viewport-aware helper should use
        // the provided size instead of the 1200x800 fallback.
        let vh_500 = compute_line_height_with_metrics_viewport(&style, None, Some(Size::new(800.0, 500.0)));
        assert!((vh_500 - 50.0).abs() < 0.001);

        // And with a different viewport height, the result should change accordingly.
        let vh_900 = compute_line_height_with_metrics_viewport(&style, None, Some(Size::new(800.0, 900.0)));
        assert!((vh_900 - 90.0).abs() < 0.001);
    }
}
