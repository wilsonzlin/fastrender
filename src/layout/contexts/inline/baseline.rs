//! Baseline Alignment for Inline Layout
//!
//! This module implements baseline alignment as specified in CSS 2.1 Section 10.8
//! and the CSS Inline Layout Module Level 3.
//!
//! # Baseline Alignment
//!
//! When inline boxes with different heights appear on the same line, they must
//! be aligned according to their vertical-align property. The default alignment
//! is "baseline", where all items align their baselines to a shared line baseline.
//!
//! # Line Box Height Calculation
//!
//! The line box height is determined by:
//! 1. Computing the maximum ascent (above baseline) among all items
//! 2. Computing the maximum descent (below baseline) among all items
//! 3. Line box height = max_ascent + max_descent
//!
//! # References
//!
//! - CSS 2.1 Section 10.8: <https://www.w3.org/TR/CSS21/visudet.html#line-height>
//! - CSS Inline Layout: <https://www.w3.org/TR/css-inline-3/>

use super::line_builder::{InlineItem, Line, TextRun};

/// Metrics for a single inline item
///
/// Captures the key measurements needed for baseline alignment:
/// - Height: Total height of the item
/// - Baseline: Position of baseline from top edge
/// - Ascent: Distance from baseline to top (equals baseline)
/// - Descent: Distance from baseline to bottom
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ItemMetrics {
    /// Total height of the item
    pub height: f32,

    /// Baseline position from top edge
    pub baseline: f32,

    /// Ascent (distance above baseline)
    /// This is equal to `baseline`
    pub ascent: f32,

    /// Descent (distance below baseline)
    /// This is `height - baseline`
    pub descent: f32,
}

impl ItemMetrics {
    /// Creates metrics for a text run
    pub fn from_text_run(text_run: &TextRun) -> Self {
        Self {
            height: text_run.height,
            baseline: text_run.baseline,
            ascent: text_run.baseline,
            descent: text_run.height - text_run.baseline,
        }
    }

    /// Creates metrics for an atomic inline box
    pub fn from_atomic(height: f32, baseline: f32) -> Self {
        Self {
            height,
            baseline,
            ascent: baseline,
            descent: height - baseline,
        }
    }

    /// Creates metrics with given ascent and descent
    pub fn new(ascent: f32, descent: f32) -> Self {
        Self {
            height: ascent + descent,
            baseline: ascent,
            ascent,
            descent,
        }
    }

    /// Zero metrics
    pub const ZERO: Self = Self {
        height: 0.0,
        baseline: 0.0,
        ascent: 0.0,
        descent: 0.0,
    };
}

impl Default for ItemMetrics {
    fn default() -> Self {
        Self::ZERO
    }
}

/// Computes the baseline position for a line
///
/// Given a line with multiple items of varying heights, this computes
/// the position of the shared baseline from the top of the line box.
///
/// # Algorithm
///
/// 1. Find the maximum ascent (above-baseline height) among all items
/// 2. The baseline position equals the maximum ascent
///
/// This ensures all items can align their individual baselines to
/// the computed line baseline without any item's top extending
/// above the line box.
///
/// # Arguments
///
/// * `line` - The line to compute baseline for
///
/// # Returns
///
/// The baseline position from the top of the line box
pub fn compute_line_baseline(line: &Line) -> f32 {
    let mut max_ascent: f32 = 0.0;

    for item in &line.items {
        let metrics = get_item_metrics(item);
        max_ascent = max_ascent.max(metrics.ascent);
    }

    max_ascent
}

/// Computes the total height of a line box
///
/// The line box height is the sum of the maximum ascent and maximum descent
/// among all items on the line.
///
/// # Arguments
///
/// * `line` - The line to compute height for
///
/// # Returns
///
/// A tuple of (line_height, baseline_position)
pub fn compute_line_height_and_baseline(line: &Line) -> (f32, f32) {
    let mut max_ascent: f32 = 0.0;
    let mut max_descent: f32 = 0.0;

    for item in &line.items {
        let metrics = get_item_metrics(item);
        max_ascent = max_ascent.max(metrics.ascent);
        max_descent = max_descent.max(metrics.descent);
    }

    let line_height = max_ascent + max_descent;
    let baseline = max_ascent;

    (line_height, baseline)
}

/// Gets metrics for an inline item
fn get_item_metrics(item: &InlineItem) -> ItemMetrics {
    match item {
        InlineItem::Text(text_run) => ItemMetrics::from_text_run(text_run),
        InlineItem::Atomic { height, baseline, .. } => ItemMetrics::from_atomic(*height, *baseline),
        // Non-content items have no metrics
        InlineItem::StartInlineBox { .. }
        | InlineItem::EndInlineBox
        | InlineItem::SoftBreakOpportunity
        | InlineItem::HardBreak => ItemMetrics::ZERO,
    }
}

/// Aligns an item to the line baseline
///
/// Computes the Y offset needed to align an item's baseline to the
/// line's baseline.
///
/// # Arguments
///
/// * `item_metrics` - Metrics of the item to align
/// * `line_baseline` - Position of line baseline from top of line box
///
/// # Returns
///
/// The Y offset from top of line box to position this item
pub fn align_on_baseline(item_metrics: &ItemMetrics, line_baseline: f32) -> f32 {
    // Offset = line_baseline - item_ascent
    // This positions the item so its baseline matches the line baseline
    line_baseline - item_metrics.ascent
}

/// Aligns an item with vertical-align: top
///
/// Positions the item at the top of the line box.
pub fn align_top(_item_metrics: &ItemMetrics, _line_height: f32) -> f32 {
    0.0
}

/// Aligns an item with vertical-align: bottom
///
/// Positions the item at the bottom of the line box.
pub fn align_bottom(item_metrics: &ItemMetrics, line_height: f32) -> f32 {
    line_height - item_metrics.height
}

/// Aligns an item with vertical-align: middle
///
/// Centers the item vertically within the line box.
pub fn align_middle(item_metrics: &ItemMetrics, line_height: f32) -> f32 {
    (line_height - item_metrics.height) / 2.0
}

/// Aligns an item with vertical-align: text-top
///
/// Aligns the top of the item with the top of the parent's font.
/// For simplicity, this uses the line baseline + ascent.
pub fn align_text_top(item_metrics: &ItemMetrics, line_baseline: f32, _parent_font_metrics: &ItemMetrics) -> f32 {
    // Align item top with parent font top
    // Parent font top is at line_baseline - parent_ascent
    // Item top is at offset
    // For simplicity, use 0 (top of line box)
    let _ = item_metrics;
    let _ = line_baseline;
    0.0
}

/// Aligns an item with vertical-align: text-bottom
///
/// Aligns the bottom of the item with the bottom of the parent's font.
pub fn align_text_bottom(item_metrics: &ItemMetrics, line_height: f32, _parent_font_metrics: &ItemMetrics) -> f32 {
    // For simplicity, align bottom
    line_height - item_metrics.height
}

/// Aligns all items on a line according to their vertical-align properties
///
/// This function takes a line and computes the Y offset for each item
/// based on baseline alignment (the default vertical-align).
///
/// # Arguments
///
/// * `line` - The line containing items to align
/// * `line_baseline` - The computed line baseline position
///
/// # Returns
///
/// A vector of Y offsets, one for each item in the line
pub fn compute_item_offsets(line: &Line, line_baseline: f32) -> Vec<f32> {
    line.items
        .iter()
        .map(|item| {
            let metrics = get_item_metrics(item);
            if metrics.height == 0.0 {
                0.0
            } else {
                align_on_baseline(&metrics, line_baseline)
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::style::ComputedStyles;
    use std::sync::Arc;

    fn default_style() -> Arc<ComputedStyles> {
        Arc::new(ComputedStyles::default())
    }

    fn text_run(height: f32, baseline: f32) -> InlineItem {
        InlineItem::Text(TextRun {
            text: "Test".to_string(),
            width: 40.0,
            height,
            baseline,
            font_size: 16.0,
            style: default_style(),
        })
    }

    #[test]
    fn test_item_metrics_from_text_run() {
        let run = TextRun {
            text: "Test".to_string(),
            width: 40.0,
            height: 20.0,
            baseline: 16.0,
            font_size: 16.0,
            style: default_style(),
        };

        let metrics = ItemMetrics::from_text_run(&run);
        assert_eq!(metrics.height, 20.0);
        assert_eq!(metrics.baseline, 16.0);
        assert_eq!(metrics.ascent, 16.0);
        assert_eq!(metrics.descent, 4.0);
    }

    #[test]
    fn test_item_metrics_from_atomic() {
        let metrics = ItemMetrics::from_atomic(100.0, 80.0);
        assert_eq!(metrics.height, 100.0);
        assert_eq!(metrics.baseline, 80.0);
        assert_eq!(metrics.ascent, 80.0);
        assert_eq!(metrics.descent, 20.0);
    }

    #[test]
    fn test_item_metrics_new() {
        let metrics = ItemMetrics::new(16.0, 4.0);
        assert_eq!(metrics.height, 20.0);
        assert_eq!(metrics.ascent, 16.0);
        assert_eq!(metrics.descent, 4.0);
    }

    #[test]
    fn test_compute_line_baseline_single_item() {
        let mut line = Line::new();
        line.push(text_run(20.0, 16.0));

        let baseline = compute_line_baseline(&line);
        assert_eq!(baseline, 16.0);
    }

    #[test]
    fn test_compute_line_baseline_multiple_items() {
        let mut line = Line::new();
        line.push(text_run(20.0, 16.0)); // Small text
        line.push(text_run(30.0, 24.0)); // Larger text

        let baseline = compute_line_baseline(&line);
        assert_eq!(baseline, 24.0); // Maximum ascent
    }

    #[test]
    fn test_compute_line_height_and_baseline() {
        let mut line = Line::new();
        line.push(text_run(20.0, 16.0)); // ascent=16, descent=4
        line.push(text_run(25.0, 18.0)); // ascent=18, descent=7

        let (height, baseline) = compute_line_height_and_baseline(&line);
        assert_eq!(baseline, 18.0); // max ascent
        assert_eq!(height, 25.0); // 18 + 7 = 25
    }

    #[test]
    fn test_align_on_baseline() {
        let metrics = ItemMetrics::new(16.0, 4.0); // ascent=16
        let line_baseline = 20.0;

        let offset = align_on_baseline(&metrics, line_baseline);
        assert_eq!(offset, 4.0); // 20 - 16 = 4
    }

    #[test]
    fn test_align_top() {
        let metrics = ItemMetrics::new(16.0, 4.0);
        let offset = align_top(&metrics, 30.0);
        assert_eq!(offset, 0.0);
    }

    #[test]
    fn test_align_bottom() {
        let metrics = ItemMetrics::new(16.0, 4.0); // height = 20
        let offset = align_bottom(&metrics, 30.0);
        assert_eq!(offset, 10.0); // 30 - 20 = 10
    }

    #[test]
    fn test_align_middle() {
        let metrics = ItemMetrics::new(16.0, 4.0); // height = 20
        let offset = align_middle(&metrics, 40.0);
        assert_eq!(offset, 10.0); // (40 - 20) / 2 = 10
    }

    #[test]
    fn test_compute_item_offsets() {
        let mut line = Line::new();
        line.push(text_run(20.0, 16.0)); // ascent=16
        line.push(text_run(25.0, 20.0)); // ascent=20

        let line_baseline = compute_line_baseline(&line);
        let offsets = compute_item_offsets(&line, line_baseline);

        assert_eq!(offsets.len(), 2);
        assert_eq!(offsets[0], 4.0); // 20 - 16 = 4
        assert_eq!(offsets[1], 0.0); // 20 - 20 = 0
    }

    #[test]
    fn test_empty_line_baseline() {
        let line = Line::new();
        let baseline = compute_line_baseline(&line);
        assert_eq!(baseline, 0.0);
    }

    #[test]
    fn test_line_with_non_content_items() {
        let mut line = Line::new();
        line.push(InlineItem::StartInlineBox { style: default_style() });
        line.push(text_run(20.0, 16.0));
        line.push(InlineItem::EndInlineBox);

        let (height, baseline) = compute_line_height_and_baseline(&line);
        assert_eq!(baseline, 16.0);
        assert_eq!(height, 20.0);
    }

    #[test]
    fn test_atomic_baseline_alignment() {
        let mut line = Line::new();
        line.push(text_run(20.0, 16.0));
        line.push(InlineItem::Atomic {
            width: 50.0,
            height: 40.0,
            baseline: 40.0, // Bottom-aligned
            style: default_style(),
        });

        let (height, baseline) = compute_line_height_and_baseline(&line);
        // Atomic has larger ascent (40 vs 16)
        assert_eq!(baseline, 40.0);
        // Text descent is 4, atomic descent is 0
        assert_eq!(height, 44.0); // 40 + 4
    }
}
