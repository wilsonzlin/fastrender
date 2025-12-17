//! Baseline alignment algorithm
//!
//! This module implements baseline alignment for inline boxes according to
//! CSS 2.1 Section 10.8: Line height calculations.
//!
//! # CSS Specification
//!
//! - CSS 2.1 Section 10.8: <https://www.w3.org/TR/CSS21/visudet.html#line-height>
//! - CSS Inline Layout Module Level 3: <https://www.w3.org/TR/css-inline-3/>
//!
//! # Algorithm Overview
//!
//! 1. Compute metrics for each inline item (ascent, descent, line-height)
//! 2. Determine the line box height based on tallest content
//! 3. Position the baseline within the line box
//! 4. Align each inline box relative to the baseline using vertical-align
//!
//! # Vertical Alignment Modes
//!
//! - `baseline`: Align box baseline to line baseline (default)
//! - `top`: Align box top to line box top
//! - `bottom`: Align box bottom to line box bottom
//! - `middle`: Align box middle to line middle (baseline + x-height/2)
//! - `text-top`: Align box top to parent's text top
//! - `text-bottom`: Align box bottom to parent's text bottom
//! - `super`: Raise box by superscript offset
//! - `sub`: Lower box by subscript offset
//! - `<length>`: Raise/lower box by specified amount
//! - `<percentage>`: Raise/lower box by percentage of line-height

use crate::text::font_db::ScaledMetrics;

/// CSS vertical-align property values
///
/// Specifies how an inline box is aligned vertically within its line box.
///
/// # CSS Specification
///
/// CSS 2.1 Section 10.8.1: Leading and half-leading
/// <https://www.w3.org/TR/CSS21/visudet.html#leading>
///
/// # Examples
///
/// ```rust,ignore
/// use fastrender::layout::inline::VerticalAlign;
///
/// let align = VerticalAlign::Baseline;
/// let align = VerticalAlign::Top;
/// let align = VerticalAlign::Length(4.0); // Raise by 4px
/// ```
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum VerticalAlign {
    /// Align the baseline of the box with the baseline of the parent box.
    /// This is the default value.
    Baseline,

    /// Align the top of the box with the top of the line box.
    Top,

    /// Align the bottom of the box with the bottom of the line box.
    Bottom,

    /// Align the vertical midpoint of the box with the baseline of the parent
    /// plus half the x-height of the parent.
    Middle,

    /// Align the top of the box with the top of the parent's content area.
    TextTop,

    /// Align the bottom of the box with the bottom of the parent's content area.
    TextBottom,

    /// Raise the baseline of the box to the proper position for superscript
    /// text of the parent.
    Super,

    /// Lower the baseline of the box to the proper position for subscript
    /// text of the parent.
    Sub,

    /// Raise (positive) or lower (negative) the baseline by the given length
    /// in CSS pixels.
    Length(f32),

    /// Raise (positive) or lower (negative) the baseline by the given
    /// percentage of the line-height.
    Percentage(f32),
}

impl Default for VerticalAlign {
    fn default() -> Self {
        Self::Baseline
    }
}

impl VerticalAlign {
    /// Returns true if this alignment requires the line box dimensions
    /// (top/bottom alignment).
    #[inline]
    pub fn requires_line_box(&self) -> bool {
        matches!(self, Self::Top | Self::Bottom)
    }

    /// Returns true if this alignment is relative to the baseline.
    #[inline]
    pub fn is_baseline_relative(&self) -> bool {
        matches!(
            self,
            Self::Baseline
                | Self::Middle
                | Self::TextTop
                | Self::TextBottom
                | Self::Super
                | Self::Sub
                | Self::Length(_)
                | Self::Percentage(_)
        )
    }
}

/// Metrics for a single inline box
///
/// Contains the dimensional information needed to position an inline box
/// relative to the line baseline.
///
/// # Fields
///
/// - `ascent`: Distance from baseline to top (positive)
/// - `descent`: Distance from baseline to bottom (positive, CSS convention)
/// - `line_height`: Total height including leading
/// - `baseline_offset`: Offset from box top to baseline
/// - `x_height`: Height of lowercase 'x' (for middle alignment)
/// - `vertical_align`: How this box should be aligned
#[derive(Debug, Clone, Copy)]
pub struct InlineBoxMetrics {
    /// Distance from baseline to top of the box (positive)
    pub ascent: f32,

    /// Distance from baseline to bottom of the box (positive)
    pub descent: f32,

    /// Total line height for this box (may include leading)
    pub line_height: f32,

    /// Offset from the top of the box to the baseline
    pub baseline_offset: f32,

    /// x-height of the font (for middle alignment)
    /// None for replaced elements or if font lacks x-height data
    pub x_height: Option<f32>,

    /// Vertical alignment mode for this box
    pub vertical_align: VerticalAlign,
}

impl InlineBoxMetrics {
    /// Creates metrics for a text run from scaled font metrics
    ///
    /// # Arguments
    ///
    /// * `scaled_metrics` - Font metrics scaled to the desired font size
    /// * `vertical_align` - How this text should be aligned
    ///
    /// # Examples
    ///
    /// ```rust,ignore
    /// use fastrender::layout::inline::{InlineBoxMetrics, VerticalAlign};
    /// use fastrender::text::ScaledMetrics;
    ///
    /// let scaled = font_metrics.scale(16.0);
    /// let metrics = InlineBoxMetrics::from_text(&scaled, VerticalAlign::Baseline);
    /// ```
    pub fn from_text(scaled_metrics: &ScaledMetrics, vertical_align: VerticalAlign) -> Self {
        Self {
            ascent: scaled_metrics.ascent,
            descent: scaled_metrics.descent,
            line_height: scaled_metrics.line_height,
            baseline_offset: scaled_metrics.ascent,
            x_height: scaled_metrics.x_height,
            vertical_align,
        }
    }

    /// Creates metrics for a replaced/atomic inline element
    ///
    /// For replaced elements (images, inline-block, etc.), the baseline is
    /// typically at the bottom margin edge.
    ///
    /// # Arguments
    ///
    /// * `height` - Total height of the element
    /// * `vertical_align` - How this element should be aligned
    ///
    /// # CSS Specification
    ///
    /// CSS 2.1 Section 10.8.1: "The baseline of an 'inline-block' is the
    /// baseline of its last line box in the normal flow, unless it has
    /// either no in-flow line boxes or if its 'overflow' property has a
    /// computed value other than 'visible', in which case the baseline
    /// is the bottom margin edge."
    pub fn from_replaced(height: f32, vertical_align: VerticalAlign) -> Self {
        Self {
            ascent: height,
            descent: 0.0,
            line_height: height,
            baseline_offset: height,
            x_height: None,
            vertical_align,
        }
    }

    /// Creates metrics for an inline-block element with a baseline
    ///
    /// # Arguments
    ///
    /// * `height` - Total height of the element
    /// * `baseline_from_top` - Distance from top to the baseline
    /// * `vertical_align` - How this element should be aligned
    pub fn from_inline_block(height: f32, baseline_from_top: f32, vertical_align: VerticalAlign) -> Self {
        Self {
            ascent: baseline_from_top,
            descent: height - baseline_from_top,
            line_height: height,
            baseline_offset: baseline_from_top,
            x_height: None,
            vertical_align,
        }
    }

    /// Returns the total height of the content (ascent + descent)
    #[inline]
    pub fn content_height(&self) -> f32 {
        self.ascent + self.descent
    }

    /// Returns the half-leading (extra space from line-height)
    ///
    /// Leading is the difference between line-height and content height,
    /// split equally above and below the content area.
    #[inline]
    pub fn half_leading(&self) -> f32 {
        (self.line_height - self.content_height()) / 2.0
    }
}

impl Default for InlineBoxMetrics {
    fn default() -> Self {
        Self {
            ascent: 0.0,
            descent: 0.0,
            line_height: 0.0,
            baseline_offset: 0.0,
            x_height: None,
            vertical_align: VerticalAlign::Baseline,
        }
    }
}

/// Metrics for an entire line box
///
/// Computed from all inline boxes on the line, this represents
/// the final dimensions and baseline position of the line.
#[derive(Debug, Clone, Copy)]
pub struct LineMetrics {
    /// Total height of the line box
    pub height: f32,

    /// Position of the baseline from the top of the line box
    pub baseline: f32,

    /// Maximum ascent above the baseline
    pub max_ascent: f32,

    /// Maximum descent below the baseline
    pub max_descent: f32,

    /// Text top (parent's ascent, for text-top alignment)
    pub text_top: f32,

    /// Text bottom (parent's descent below baseline, for text-bottom alignment)
    pub text_bottom: f32,
}

impl LineMetrics {
    /// Creates line metrics from the strut (parent's font metrics)
    ///
    /// The strut defines the minimum line height and baseline position
    /// based on the parent element's font.
    ///
    /// # CSS Specification
    ///
    /// CSS 2.1 Section 10.8.1: "On a block container element whose content
    /// is composed of inline-level elements, 'line-height' specifies the
    /// minimal height of line boxes within the element."
    pub fn from_strut(strut_metrics: &ScaledMetrics) -> Self {
        let half_leading = (strut_metrics.line_height - (strut_metrics.ascent + strut_metrics.descent)) / 2.0;
        Self {
            height: strut_metrics.line_height,
            baseline: strut_metrics.ascent + half_leading,
            max_ascent: strut_metrics.ascent + half_leading,
            max_descent: strut_metrics.descent + half_leading,
            text_top: strut_metrics.ascent,
            text_bottom: strut_metrics.descent,
        }
    }

    /// Creates default line metrics with zero dimensions
    pub fn empty() -> Self {
        Self {
            height: 0.0,
            baseline: 0.0,
            max_ascent: 0.0,
            max_descent: 0.0,
            text_top: 0.0,
            text_bottom: 0.0,
        }
    }
}

/// A positioned inline box with computed Y offset
#[derive(Debug, Clone, Copy)]
pub struct PositionedInlineBox {
    /// Index of the original inline item
    pub index: usize,

    /// Y offset from the top of the line box
    pub y_offset: f32,

    /// The metrics used for this box
    pub metrics: InlineBoxMetrics,
}

/// Baseline aligner for inline layout
///
/// Computes the vertical positions of inline boxes within a line box
/// according to CSS baseline alignment rules.
///
/// # Algorithm
///
/// 1. **Initial pass**: Collect metrics for baseline-aligned items
/// 2. **Compute line height**: Determine line box height from all items
/// 3. **Position baseline**: Set baseline position within line box
/// 4. **Align items**: Position each item according to vertical-align
///
/// # Examples
///
/// ```rust,ignore
/// use fastrender::layout::inline::{BaselineAligner, InlineBoxMetrics, VerticalAlign};
///
/// let mut aligner = BaselineAligner::new();
///
/// // Add text boxes
/// aligner.add_box(InlineBoxMetrics::from_text(&font_metrics, VerticalAlign::Baseline));
/// aligner.add_box(InlineBoxMetrics::from_text(&large_font_metrics, VerticalAlign::Baseline));
///
/// // Compute alignment
/// let (line_metrics, positioned_boxes) = aligner.align();
/// ```
#[derive(Debug)]
pub struct BaselineAligner {
    /// Inline box metrics to align
    boxes: Vec<InlineBoxMetrics>,

    /// Strut metrics (parent font, defines minimum line height)
    strut: Option<ScaledMetrics>,
}

impl BaselineAligner {
    /// Creates a new baseline aligner
    pub fn new() -> Self {
        Self {
            boxes: Vec::new(),
            strut: None,
        }
    }

    /// Creates a baseline aligner with strut metrics
    ///
    /// The strut ensures a minimum line height even if the line is empty.
    ///
    /// # Arguments
    ///
    /// * `strut_metrics` - Font metrics of the parent element
    pub fn with_strut(strut_metrics: ScaledMetrics) -> Self {
        Self {
            boxes: Vec::new(),
            strut: Some(strut_metrics),
        }
    }

    /// Sets the strut metrics
    pub fn set_strut(&mut self, strut_metrics: ScaledMetrics) {
        self.strut = Some(strut_metrics);
    }

    /// Adds an inline box to be aligned
    pub fn add_box(&mut self, metrics: InlineBoxMetrics) {
        self.boxes.push(metrics);
    }

    /// Clears all boxes
    pub fn clear(&mut self) {
        self.boxes.clear();
    }

    /// Returns the number of boxes
    pub fn len(&self) -> usize {
        self.boxes.len()
    }

    /// Returns true if there are no boxes
    pub fn is_empty(&self) -> bool {
        self.boxes.is_empty()
    }

    /// Performs baseline alignment and returns line metrics and positioned boxes
    ///
    /// # Returns
    ///
    /// A tuple of `(LineMetrics, Vec<PositionedInlineBox>)` containing:
    /// - The computed line box metrics
    /// - Each inline box with its Y offset from the line box top
    ///
    /// # Algorithm
    ///
    /// 1. First pass: Compute baseline position from baseline-aligned items
    /// 2. Second pass: Compute line box extent from all items
    /// 3. Third pass: Position each item
    pub fn align(&self) -> (LineMetrics, Vec<PositionedInlineBox>) {
        if self.boxes.is_empty() && self.strut.is_none() {
            return (LineMetrics::empty(), Vec::new());
        }

        // Step 1: Initialize from strut or find initial baseline position
        let (initial_above, initial_below, text_top, text_bottom) = if let Some(ref strut) = self.strut {
            let strut_half_leading = (strut.line_height - (strut.ascent + strut.descent)) / 2.0;
            (
                strut.ascent + strut_half_leading,
                strut.descent + strut_half_leading,
                strut.ascent,
                strut.descent,
            )
        } else {
            (0.0, 0.0, 0.0, 0.0)
        };

        // Step 2: First pass - accumulate extent of baseline-aligned items
        let mut max_above_baseline = initial_above;
        let mut max_below_baseline = initial_below;

        for metrics in &self.boxes {
            if metrics.vertical_align.is_baseline_relative() {
                let offset = self.compute_baseline_offset(metrics, text_top, text_bottom);
                // When offset > 0 (box raised), more extends above baseline
                // When offset < 0 (box lowered), more extends below baseline
                let above = metrics.ascent + metrics.half_leading() + offset;
                let below = metrics.descent + metrics.half_leading() - offset;

                max_above_baseline = max_above_baseline.max(above);
                max_below_baseline = max_below_baseline.max(below);
            }
        }

        // Step 3: Initial line height from baseline-relative items
        let mut line_height = max_above_baseline + max_below_baseline;
        let baseline = max_above_baseline;

        // Step 4: Second pass - extend line height for top/bottom aligned items
        for metrics in &self.boxes {
            match metrics.vertical_align {
                VerticalAlign::Top => {
                    // Item aligns to top, may extend bottom
                    let item_height = metrics.line_height;
                    let extends_below = item_height - baseline;
                    if extends_below > max_below_baseline {
                        line_height = baseline + extends_below;
                    }
                }
                VerticalAlign::Bottom => {
                    // Item aligns to bottom, may extend top
                    let item_height = metrics.line_height;
                    let extends_above = item_height - (line_height - baseline);
                    if extends_above > max_above_baseline {
                        let new_above = extends_above;
                        line_height = new_above + max_below_baseline;
                    }
                }
                _ => {}
            }
        }

        // Ensure minimum line height from strut
        if let Some(ref strut) = self.strut {
            line_height = line_height.max(strut.line_height);
        }

        // Step 5: Third pass - position each box
        let mut positioned_boxes = Vec::with_capacity(self.boxes.len());

        for (index, metrics) in self.boxes.iter().enumerate() {
            let y_offset = self.compute_y_offset(metrics, baseline, line_height, text_top, text_bottom);

            positioned_boxes.push(PositionedInlineBox {
                index,
                y_offset,
                metrics: *metrics,
            });
        }

        let line_metrics = LineMetrics {
            height: line_height,
            baseline,
            max_ascent: max_above_baseline,
            max_descent: max_below_baseline,
            text_top,
            text_bottom,
        };

        (line_metrics, positioned_boxes)
    }

    /// Computes the baseline offset for a box based on vertical-align
    ///
    /// Returns how much to shift the box's baseline relative to the line baseline.
    /// Positive = up, negative = down.
    fn compute_baseline_offset(&self, metrics: &InlineBoxMetrics, text_top: f32, text_bottom: f32) -> f32 {
        match metrics.vertical_align {
            VerticalAlign::Baseline => 0.0,

            VerticalAlign::Middle => {
                // Align middle of box to baseline + x-height/2
                let x_height = self.strut.as_ref().and_then(|s| s.x_height).unwrap_or(0.0);
                let box_middle = (metrics.ascent - metrics.descent) / 2.0;
                x_height / 2.0 - box_middle
            }

            VerticalAlign::TextTop => {
                // Align box top to text top
                // Offset so that box.ascent aligns with text_top
                text_top - metrics.ascent
            }

            VerticalAlign::TextBottom => {
                // Align box bottom to text bottom
                // Offset so that box.descent aligns with text_bottom
                -(text_bottom - metrics.descent)
            }

            VerticalAlign::Super => {
                // Raise by superscript amount (typically ~40% of em)
                let em_size = self.strut.as_ref().map(|s| s.font_size).unwrap_or(16.0);
                em_size * 0.4
            }

            VerticalAlign::Sub => {
                // Lower by subscript amount (typically ~20% of em)
                let em_size = self.strut.as_ref().map(|s| s.font_size).unwrap_or(16.0);
                -(em_size * 0.2)
            }

            VerticalAlign::Length(px) => px,

            VerticalAlign::Percentage(pct) => {
                // Percentage of line-height
                metrics.line_height * (pct / 100.0)
            }

            // Top/Bottom are handled separately
            VerticalAlign::Top | VerticalAlign::Bottom => 0.0,
        }
    }

    /// Computes the Y offset for a box within the line box
    fn compute_y_offset(
        &self,
        metrics: &InlineBoxMetrics,
        baseline: f32,
        line_height: f32,
        text_top: f32,
        text_bottom: f32,
    ) -> f32 {
        match metrics.vertical_align {
            VerticalAlign::Top => {
                // Align top of box to top of line box
                metrics.half_leading()
            }

            VerticalAlign::Bottom => {
                // Align bottom of box to bottom of line box
                line_height - metrics.line_height + metrics.half_leading()
            }

            _ => {
                // Baseline-relative alignment
                let offset = self.compute_baseline_offset(metrics, text_top, text_bottom);
                // Y offset = baseline position - (ascent + half-leading) + adjustment
                baseline - metrics.ascent - metrics.half_leading() - offset
            }
        }
    }
}

impl Default for BaselineAligner {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Helper to create mock scaled metrics
    fn mock_scaled_metrics(font_size: f32, ascent: f32, descent: f32) -> ScaledMetrics {
        ScaledMetrics {
            font_size,
            scale: font_size / 1000.0,
            ascent,
            descent,
            line_gap: 0.0,
            line_height: ascent + descent,
            x_height: Some(ascent * 0.5),
            cap_height: Some(ascent * 0.7),
            underline_position: -descent * 0.5,
            underline_thickness: font_size * 0.05,
        }
    }

    // ==========================================================================
    // VerticalAlign tests
    // ==========================================================================

    #[test]
    fn test_vertical_align_default() {
        assert_eq!(VerticalAlign::default(), VerticalAlign::Baseline);
    }

    #[test]
    fn test_vertical_align_requires_line_box() {
        assert!(VerticalAlign::Top.requires_line_box());
        assert!(VerticalAlign::Bottom.requires_line_box());
        assert!(!VerticalAlign::Baseline.requires_line_box());
        assert!(!VerticalAlign::Middle.requires_line_box());
    }

    #[test]
    fn test_vertical_align_is_baseline_relative() {
        assert!(VerticalAlign::Baseline.is_baseline_relative());
        assert!(VerticalAlign::Middle.is_baseline_relative());
        assert!(VerticalAlign::Super.is_baseline_relative());
        assert!(VerticalAlign::Sub.is_baseline_relative());
        assert!(VerticalAlign::Length(5.0).is_baseline_relative());
        assert!(VerticalAlign::Percentage(50.0).is_baseline_relative());
        assert!(!VerticalAlign::Top.is_baseline_relative());
        assert!(!VerticalAlign::Bottom.is_baseline_relative());
    }

    // ==========================================================================
    // InlineBoxMetrics tests
    // ==========================================================================

    #[test]
    fn test_inline_box_metrics_from_text() {
        let scaled = mock_scaled_metrics(16.0, 12.0, 4.0);
        let metrics = InlineBoxMetrics::from_text(&scaled, VerticalAlign::Baseline);

        assert_eq!(metrics.ascent, 12.0);
        assert_eq!(metrics.descent, 4.0);
        assert_eq!(metrics.line_height, 16.0);
        assert_eq!(metrics.baseline_offset, 12.0);
        assert_eq!(metrics.x_height, Some(6.0));
    }

    #[test]
    fn test_inline_box_metrics_from_replaced() {
        let metrics = InlineBoxMetrics::from_replaced(50.0, VerticalAlign::Baseline);

        assert_eq!(metrics.ascent, 50.0);
        assert_eq!(metrics.descent, 0.0);
        assert_eq!(metrics.line_height, 50.0);
        assert_eq!(metrics.baseline_offset, 50.0);
        assert_eq!(metrics.x_height, None);
    }

    #[test]
    fn test_inline_box_metrics_from_inline_block() {
        let metrics = InlineBoxMetrics::from_inline_block(100.0, 80.0, VerticalAlign::Baseline);

        assert_eq!(metrics.ascent, 80.0);
        assert_eq!(metrics.descent, 20.0);
        assert_eq!(metrics.line_height, 100.0);
        assert_eq!(metrics.baseline_offset, 80.0);
    }

    #[test]
    fn test_inline_box_metrics_content_height() {
        let metrics = InlineBoxMetrics {
            ascent: 12.0,
            descent: 4.0,
            line_height: 20.0,
            baseline_offset: 12.0,
            x_height: None,
            vertical_align: VerticalAlign::Baseline,
        };

        assert_eq!(metrics.content_height(), 16.0);
    }

    #[test]
    fn test_inline_box_metrics_half_leading() {
        let metrics = InlineBoxMetrics {
            ascent: 12.0,
            descent: 4.0,
            line_height: 24.0, // 8px leading total
            baseline_offset: 12.0,
            x_height: None,
            vertical_align: VerticalAlign::Baseline,
        };

        assert_eq!(metrics.half_leading(), 4.0);
    }

    #[test]
    fn test_inline_box_metrics_no_leading() {
        let metrics = InlineBoxMetrics {
            ascent: 12.0,
            descent: 4.0,
            line_height: 16.0, // No leading
            baseline_offset: 12.0,
            x_height: None,
            vertical_align: VerticalAlign::Baseline,
        };

        assert_eq!(metrics.half_leading(), 0.0);
    }

    // ==========================================================================
    // LineMetrics tests
    // ==========================================================================

    #[test]
    fn test_line_metrics_from_strut() {
        let scaled = mock_scaled_metrics(16.0, 12.0, 4.0);
        let line_metrics = LineMetrics::from_strut(&scaled);

        assert_eq!(line_metrics.height, 16.0);
        assert_eq!(line_metrics.baseline, 12.0);
        assert_eq!(line_metrics.text_top, 12.0);
        assert_eq!(line_metrics.text_bottom, 4.0);
    }

    #[test]
    fn test_line_metrics_empty() {
        let line_metrics = LineMetrics::empty();

        assert_eq!(line_metrics.height, 0.0);
        assert_eq!(line_metrics.baseline, 0.0);
    }

    // ==========================================================================
    // BaselineAligner tests
    // ==========================================================================

    #[test]
    fn test_baseline_aligner_empty() {
        let aligner = BaselineAligner::new();
        let (line_metrics, boxes) = aligner.align();

        assert_eq!(line_metrics.height, 0.0);
        assert!(boxes.is_empty());
    }

    #[test]
    fn test_baseline_aligner_with_strut_only() {
        let strut = mock_scaled_metrics(16.0, 12.0, 4.0);
        let aligner = BaselineAligner::with_strut(strut);
        let (line_metrics, boxes) = aligner.align();

        assert_eq!(line_metrics.height, 16.0);
        assert_eq!(line_metrics.baseline, 12.0);
        assert!(boxes.is_empty());
    }

    #[test]
    fn test_baseline_aligner_single_text() {
        let strut = mock_scaled_metrics(16.0, 12.0, 4.0);
        let mut aligner = BaselineAligner::with_strut(strut.clone());

        let text_metrics = InlineBoxMetrics::from_text(&strut, VerticalAlign::Baseline);
        aligner.add_box(text_metrics);

        let (line_metrics, boxes) = aligner.align();

        assert_eq!(line_metrics.height, 16.0);
        assert_eq!(line_metrics.baseline, 12.0);
        assert_eq!(boxes.len(), 1);
        assert_eq!(boxes[0].y_offset, 0.0); // Aligned at baseline
    }

    #[test]
    fn test_baseline_aligner_different_sizes() {
        let strut = mock_scaled_metrics(16.0, 12.0, 4.0);
        let mut aligner = BaselineAligner::with_strut(strut);

        // Small text
        let small = InlineBoxMetrics {
            ascent: 8.0,
            descent: 2.0,
            line_height: 10.0,
            baseline_offset: 8.0,
            x_height: Some(4.0),
            vertical_align: VerticalAlign::Baseline,
        };

        // Large text
        let large = InlineBoxMetrics {
            ascent: 20.0,
            descent: 6.0,
            line_height: 26.0,
            baseline_offset: 20.0,
            x_height: Some(10.0),
            vertical_align: VerticalAlign::Baseline,
        };

        aligner.add_box(small);
        aligner.add_box(large);

        let (line_metrics, boxes) = aligner.align();

        // Line should be tall enough for largest content
        assert!(line_metrics.height >= 26.0);

        // Both should align to same baseline
        // Small text: baseline at 20, y_offset = 20 - 8 = 12
        // Large text: baseline at 20, y_offset = 20 - 20 = 0
        assert_eq!(boxes[0].y_offset + boxes[0].metrics.ascent, line_metrics.baseline);
        assert_eq!(boxes[1].y_offset + boxes[1].metrics.ascent, line_metrics.baseline);
    }

    #[test]
    fn test_baseline_aligner_top_aligned() {
        let strut = mock_scaled_metrics(16.0, 12.0, 4.0);
        let mut aligner = BaselineAligner::with_strut(strut);

        let top_box = InlineBoxMetrics {
            ascent: 20.0,
            descent: 5.0,
            line_height: 25.0,
            baseline_offset: 20.0,
            x_height: None,
            vertical_align: VerticalAlign::Top,
        };

        aligner.add_box(top_box);

        let (line_metrics, boxes) = aligner.align();

        // Top-aligned box should be at top of line
        assert_eq!(boxes[0].y_offset, 0.0);
        assert!(line_metrics.height >= 25.0);
    }

    #[test]
    fn test_baseline_aligner_bottom_aligned() {
        let strut = mock_scaled_metrics(16.0, 12.0, 4.0);
        let mut aligner = BaselineAligner::with_strut(strut);

        let bottom_box = InlineBoxMetrics {
            ascent: 20.0,
            descent: 5.0,
            line_height: 25.0,
            baseline_offset: 20.0,
            x_height: None,
            vertical_align: VerticalAlign::Bottom,
        };

        aligner.add_box(bottom_box);

        let (line_metrics, boxes) = aligner.align();

        // Bottom-aligned box should align to bottom
        let expected_y = line_metrics.height - 25.0;
        assert!((boxes[0].y_offset - expected_y).abs() < 0.01);
    }

    #[test]
    fn test_baseline_aligner_middle_aligned() {
        let strut = mock_scaled_metrics(16.0, 12.0, 4.0);
        let mut aligner = BaselineAligner::with_strut(strut);

        let middle_box = InlineBoxMetrics {
            ascent: 10.0,
            descent: 10.0,
            line_height: 20.0,
            baseline_offset: 10.0,
            x_height: None,
            vertical_align: VerticalAlign::Middle,
        };

        aligner.add_box(middle_box);

        let (line_metrics, boxes) = aligner.align();

        // Middle alignment centers box on baseline + x-height/2
        // The exact position depends on x-height
        assert!(boxes[0].y_offset >= 0.0);
        assert!(boxes[0].y_offset + 20.0 <= line_metrics.height);
    }

    #[test]
    fn test_baseline_aligner_super() {
        let strut = mock_scaled_metrics(16.0, 12.0, 4.0);
        let mut aligner = BaselineAligner::with_strut(strut.clone());

        let normal = InlineBoxMetrics::from_text(&strut, VerticalAlign::Baseline);
        let superscript = InlineBoxMetrics {
            ascent: 8.0,
            descent: 2.0,
            line_height: 10.0,
            baseline_offset: 8.0,
            x_height: None,
            vertical_align: VerticalAlign::Super,
        };

        aligner.add_box(normal);
        aligner.add_box(superscript);

        let (_, boxes) = aligner.align();

        // Superscript should be higher (smaller y_offset) than baseline
        assert!(boxes[1].y_offset < boxes[0].y_offset);
    }

    #[test]
    fn test_baseline_aligner_sub() {
        let strut = mock_scaled_metrics(16.0, 12.0, 4.0);
        let mut aligner = BaselineAligner::with_strut(strut.clone());

        let normal = InlineBoxMetrics::from_text(&strut, VerticalAlign::Baseline);
        let subscript = InlineBoxMetrics {
            ascent: 8.0,
            descent: 2.0,
            line_height: 10.0,
            baseline_offset: 8.0,
            x_height: None,
            vertical_align: VerticalAlign::Sub,
        };

        aligner.add_box(normal);
        aligner.add_box(subscript);

        let (_, boxes) = aligner.align();

        // Subscript should be lower (larger y_offset) than baseline
        assert!(boxes[1].y_offset > boxes[0].y_offset);
    }

    #[test]
    fn test_baseline_aligner_length_offset() {
        let strut = mock_scaled_metrics(16.0, 12.0, 4.0);
        let mut aligner = BaselineAligner::with_strut(strut.clone());

        let normal = InlineBoxMetrics::from_text(&strut, VerticalAlign::Baseline);
        let raised = InlineBoxMetrics {
            vertical_align: VerticalAlign::Length(5.0),
            ..InlineBoxMetrics::from_text(&strut, VerticalAlign::Baseline)
        };

        aligner.add_box(normal);
        aligner.add_box(raised);

        let (_, boxes) = aligner.align();

        // Raised box should be 5px higher
        assert!((boxes[0].y_offset - boxes[1].y_offset - 5.0).abs() < 0.01);
    }

    #[test]
    fn test_baseline_aligner_percentage_offset() {
        let strut = mock_scaled_metrics(16.0, 12.0, 4.0);
        let mut aligner = BaselineAligner::with_strut(strut.clone());

        let lowered = InlineBoxMetrics {
            ascent: 12.0,
            descent: 4.0,
            line_height: 20.0, // 20px line-height
            baseline_offset: 12.0,
            x_height: None,
            vertical_align: VerticalAlign::Percentage(-50.0), // Lower by 50% of line-height = 10px
        };

        aligner.add_box(lowered);

        let (_, boxes) = aligner.align();

        // Box should be lowered by 10px (50% of 20px line-height)
        assert!(boxes[0].y_offset > 0.0);
    }

    #[test]
    fn test_baseline_aligner_mixed_alignment() {
        let strut = mock_scaled_metrics(16.0, 12.0, 4.0);
        let mut aligner = BaselineAligner::with_strut(strut.clone());

        // Mix of different alignments
        let baseline_box = InlineBoxMetrics::from_text(&strut, VerticalAlign::Baseline);
        let top_box = InlineBoxMetrics::from_replaced(30.0, VerticalAlign::Top);
        let bottom_box = InlineBoxMetrics::from_replaced(25.0, VerticalAlign::Bottom);

        aligner.add_box(baseline_box);
        aligner.add_box(top_box);
        aligner.add_box(bottom_box);

        let (line_metrics, boxes) = aligner.align();

        // Top box at top
        assert_eq!(boxes[1].y_offset, 0.0);

        // Bottom box at bottom
        assert!((boxes[2].y_offset + 25.0 - line_metrics.height).abs() < 0.01);

        // Line should accommodate all boxes
        assert!(line_metrics.height >= 30.0);
        assert!(line_metrics.height >= 25.0);
    }

    #[test]
    fn test_baseline_aligner_clear() {
        let mut aligner = BaselineAligner::new();
        aligner.add_box(InlineBoxMetrics::default());
        aligner.add_box(InlineBoxMetrics::default());

        assert_eq!(aligner.len(), 2);

        aligner.clear();
        assert!(aligner.is_empty());
    }

    #[test]
    fn test_baseline_aligner_set_strut() {
        let mut aligner = BaselineAligner::new();
        assert!(aligner.strut.is_none());

        let strut = mock_scaled_metrics(16.0, 12.0, 4.0);
        aligner.set_strut(strut);

        let (line_metrics, _) = aligner.align();
        assert_eq!(line_metrics.height, 16.0);
    }

    #[test]
    fn test_replaced_element_baseline() {
        let strut = mock_scaled_metrics(16.0, 12.0, 4.0);
        let mut aligner = BaselineAligner::with_strut(strut.clone());

        // Text and image on same line
        let text = InlineBoxMetrics::from_text(&strut, VerticalAlign::Baseline);
        let image = InlineBoxMetrics::from_replaced(50.0, VerticalAlign::Baseline);

        aligner.add_box(text);
        aligner.add_box(image);

        let (line_metrics, boxes) = aligner.align();

        // Image baseline (bottom) should align with text baseline
        let text_baseline = boxes[0].y_offset + boxes[0].metrics.ascent;
        let image_baseline = boxes[1].y_offset + boxes[1].metrics.ascent;

        assert!((text_baseline - image_baseline).abs() < 0.01);

        // Line should be tall enough
        assert!(line_metrics.height >= 50.0);
    }

    #[test]
    fn test_inline_block_with_custom_baseline() {
        let strut = mock_scaled_metrics(16.0, 12.0, 4.0);
        let mut aligner = BaselineAligner::with_strut(strut.clone());

        let text = InlineBoxMetrics::from_text(&strut, VerticalAlign::Baseline);
        // Inline-block with baseline 30px from top (e.g., last line of text inside)
        let inline_block = InlineBoxMetrics::from_inline_block(50.0, 30.0, VerticalAlign::Baseline);

        aligner.add_box(text);
        aligner.add_box(inline_block);

        let (_, boxes) = aligner.align();

        // Both baselines should align
        let text_baseline = boxes[0].y_offset + boxes[0].metrics.ascent;
        let block_baseline = boxes[1].y_offset + boxes[1].metrics.ascent;

        assert!((text_baseline - block_baseline).abs() < 0.01);
    }
}
