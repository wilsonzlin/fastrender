//! Tests for baseline alignment module
//!
//! Tests the baseline alignment algorithm as specified in CSS 2.1 Section 10.8.

use fastrender::layout::inline::baseline::{BaselineAligner, InlineBoxMetrics, LineMetrics, VerticalAlign};
use fastrender::text::font_db::ScaledMetrics;

// =============================================================================
// Test Helpers
// =============================================================================

/// Creates mock scaled metrics for testing
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

/// Creates mock scaled metrics with custom line height
fn mock_scaled_metrics_with_line_height(font_size: f32, ascent: f32, descent: f32, line_height: f32) -> ScaledMetrics {
    ScaledMetrics {
        font_size,
        scale: font_size / 1000.0,
        ascent,
        descent,
        line_gap: 0.0,
        line_height,
        x_height: Some(ascent * 0.5),
        cap_height: Some(ascent * 0.7),
        underline_position: -descent * 0.5,
        underline_thickness: font_size * 0.05,
    }
}

// =============================================================================
// VerticalAlign Tests
// =============================================================================

#[test]
fn test_vertical_align_default_is_baseline() {
    let align = VerticalAlign::default();
    assert_eq!(align, VerticalAlign::Baseline);
}

#[test]
fn test_vertical_align_top_bottom_require_line_box() {
    assert!(VerticalAlign::Top.requires_line_box());
    assert!(VerticalAlign::Bottom.requires_line_box());

    // Others don't require line box
    assert!(!VerticalAlign::Baseline.requires_line_box());
    assert!(!VerticalAlign::Middle.requires_line_box());
    assert!(!VerticalAlign::TextTop.requires_line_box());
    assert!(!VerticalAlign::TextBottom.requires_line_box());
    assert!(!VerticalAlign::Super.requires_line_box());
    assert!(!VerticalAlign::Sub.requires_line_box());
    assert!(!VerticalAlign::Length(5.0).requires_line_box());
    assert!(!VerticalAlign::Percentage(50.0).requires_line_box());
}

#[test]
fn test_vertical_align_baseline_relative() {
    // These are baseline-relative
    assert!(VerticalAlign::Baseline.is_baseline_relative());
    assert!(VerticalAlign::Middle.is_baseline_relative());
    assert!(VerticalAlign::TextTop.is_baseline_relative());
    assert!(VerticalAlign::TextBottom.is_baseline_relative());
    assert!(VerticalAlign::Super.is_baseline_relative());
    assert!(VerticalAlign::Sub.is_baseline_relative());
    assert!(VerticalAlign::Length(5.0).is_baseline_relative());
    assert!(VerticalAlign::Percentage(50.0).is_baseline_relative());

    // Top/bottom are NOT baseline-relative
    assert!(!VerticalAlign::Top.is_baseline_relative());
    assert!(!VerticalAlign::Bottom.is_baseline_relative());
}

// =============================================================================
// InlineBoxMetrics Tests
// =============================================================================

#[test]
fn test_inline_box_metrics_from_text() {
    let scaled = mock_scaled_metrics(16.0, 12.0, 4.0);
    let metrics = InlineBoxMetrics::from_text(&scaled, VerticalAlign::Baseline);

    assert_eq!(metrics.ascent, 12.0);
    assert_eq!(metrics.descent, 4.0);
    assert_eq!(metrics.line_height, 16.0);
    assert_eq!(metrics.baseline_offset, 12.0);
    assert_eq!(metrics.x_height, Some(6.0));
    assert_eq!(metrics.vertical_align, VerticalAlign::Baseline);
}

#[test]
fn test_inline_box_metrics_from_replaced_element() {
    let metrics = InlineBoxMetrics::from_replaced(100.0, VerticalAlign::Baseline);

    // For replaced elements, entire height is above baseline
    assert_eq!(metrics.ascent, 100.0);
    assert_eq!(metrics.descent, 0.0);
    assert_eq!(metrics.line_height, 100.0);
    assert_eq!(metrics.baseline_offset, 100.0);
    assert_eq!(metrics.x_height, None);
}

#[test]
fn test_inline_box_metrics_from_inline_block() {
    // Inline-block with content that establishes a baseline
    let metrics = InlineBoxMetrics::from_inline_block(80.0, 60.0, VerticalAlign::Baseline);

    assert_eq!(metrics.ascent, 60.0); // Distance from top to baseline
    assert_eq!(metrics.descent, 20.0); // Distance from baseline to bottom
    assert_eq!(metrics.line_height, 80.0);
    assert_eq!(metrics.baseline_offset, 60.0);
}

#[test]
fn test_inline_box_metrics_content_height() {
    let metrics = InlineBoxMetrics {
        ascent: 15.0,
        descent: 5.0,
        line_height: 24.0,
        baseline_offset: 15.0,
        x_height: None,
        vertical_align: VerticalAlign::Baseline,
    };

    assert_eq!(metrics.content_height(), 20.0);
}

#[test]
fn test_inline_box_metrics_half_leading() {
    let metrics = InlineBoxMetrics {
        ascent: 12.0,
        descent: 4.0,
        line_height: 24.0, // 8px total leading
        baseline_offset: 12.0,
        x_height: None,
        vertical_align: VerticalAlign::Baseline,
    };

    assert_eq!(metrics.half_leading(), 4.0);
}

#[test]
fn test_inline_box_metrics_no_leading_when_line_height_equals_content() {
    let metrics = InlineBoxMetrics {
        ascent: 12.0,
        descent: 4.0,
        line_height: 16.0, // Same as content height
        baseline_offset: 12.0,
        x_height: None,
        vertical_align: VerticalAlign::Baseline,
    };

    assert_eq!(metrics.half_leading(), 0.0);
}

#[test]
fn test_inline_box_metrics_default() {
    let metrics = InlineBoxMetrics::default();

    assert_eq!(metrics.ascent, 0.0);
    assert_eq!(metrics.descent, 0.0);
    assert_eq!(metrics.line_height, 0.0);
    assert_eq!(metrics.vertical_align, VerticalAlign::Baseline);
}

// =============================================================================
// LineMetrics Tests
// =============================================================================

#[test]
fn test_line_metrics_from_strut() {
    let scaled = mock_scaled_metrics(16.0, 12.0, 4.0);
    let line_metrics = LineMetrics::from_strut(&scaled);

    assert_eq!(line_metrics.height, 16.0);
    assert_eq!(line_metrics.baseline, 12.0);
    assert_eq!(line_metrics.max_ascent, 12.0);
    assert_eq!(line_metrics.max_descent, 4.0);
    assert_eq!(line_metrics.text_top, 12.0);
    assert_eq!(line_metrics.text_bottom, 4.0);
}

#[test]
fn test_line_metrics_empty() {
    let line_metrics = LineMetrics::empty();

    assert_eq!(line_metrics.height, 0.0);
    assert_eq!(line_metrics.baseline, 0.0);
    assert_eq!(line_metrics.max_ascent, 0.0);
    assert_eq!(line_metrics.max_descent, 0.0);
}

// =============================================================================
// BaselineAligner Basic Tests
// =============================================================================

#[test]
fn test_baseline_aligner_new_is_empty() {
    let aligner = BaselineAligner::new();
    assert!(aligner.is_empty());
    assert_eq!(aligner.len(), 0);
}

#[test]
fn test_baseline_aligner_add_box() {
    let mut aligner = BaselineAligner::new();
    aligner.add_box(InlineBoxMetrics::default());
    aligner.add_box(InlineBoxMetrics::default());

    assert_eq!(aligner.len(), 2);
    assert!(!aligner.is_empty());
}

#[test]
fn test_baseline_aligner_clear() {
    let mut aligner = BaselineAligner::new();
    aligner.add_box(InlineBoxMetrics::default());
    aligner.add_box(InlineBoxMetrics::default());
    aligner.clear();

    assert!(aligner.is_empty());
}

#[test]
fn test_baseline_aligner_empty_returns_empty_results() {
    let aligner = BaselineAligner::new();
    let (line_metrics, boxes) = aligner.align();

    assert_eq!(line_metrics.height, 0.0);
    assert!(boxes.is_empty());
}

#[test]
fn test_baseline_aligner_strut_only() {
    let strut = mock_scaled_metrics(16.0, 12.0, 4.0);
    let aligner = BaselineAligner::with_strut(strut);
    let (line_metrics, boxes) = aligner.align();

    assert_eq!(line_metrics.height, 16.0);
    assert_eq!(line_metrics.baseline, 12.0);
    assert!(boxes.is_empty());
}

// =============================================================================
// Baseline Alignment Tests
// =============================================================================

#[test]
fn test_baseline_alignment_single_text() {
    let strut = mock_scaled_metrics(16.0, 12.0, 4.0);
    let mut aligner = BaselineAligner::with_strut(strut.clone());

    let text_metrics = InlineBoxMetrics::from_text(&strut, VerticalAlign::Baseline);
    aligner.add_box(text_metrics);

    let (line_metrics, boxes) = aligner.align();

    assert_eq!(line_metrics.height, 16.0);
    assert_eq!(boxes.len(), 1);

    // Box should be positioned so its baseline aligns with line baseline
    let box_baseline = boxes[0].y_offset + boxes[0].metrics.ascent;
    assert!((box_baseline - line_metrics.baseline).abs() < 0.01);
}

#[test]
fn test_baseline_alignment_same_size_text() {
    let strut = mock_scaled_metrics(16.0, 12.0, 4.0);
    let mut aligner = BaselineAligner::with_strut(strut.clone());

    // Add two boxes with same metrics
    let text1 = InlineBoxMetrics::from_text(&strut, VerticalAlign::Baseline);
    let text2 = InlineBoxMetrics::from_text(&strut, VerticalAlign::Baseline);

    aligner.add_box(text1);
    aligner.add_box(text2);

    let (line_metrics, boxes) = aligner.align();

    // Both should be at same position
    assert_eq!(boxes[0].y_offset, boxes[1].y_offset);

    // Both baselines should align
    let baseline1 = boxes[0].y_offset + boxes[0].metrics.ascent;
    let baseline2 = boxes[1].y_offset + boxes[1].metrics.ascent;
    assert!((baseline1 - baseline2).abs() < 0.01);
}

#[test]
fn test_baseline_alignment_different_sizes() {
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
        ascent: 24.0,
        descent: 8.0,
        line_height: 32.0,
        baseline_offset: 24.0,
        x_height: Some(12.0),
        vertical_align: VerticalAlign::Baseline,
    };

    aligner.add_box(small);
    aligner.add_box(large);

    let (line_metrics, boxes) = aligner.align();

    // Line should be tall enough for largest content
    assert!(line_metrics.height >= 32.0);

    // Baselines should align
    let small_baseline = boxes[0].y_offset + boxes[0].metrics.ascent;
    let large_baseline = boxes[1].y_offset + boxes[1].metrics.ascent;
    assert!((small_baseline - large_baseline).abs() < 0.01);

    // Small text should have positive y_offset (pushed down)
    assert!(boxes[0].y_offset > boxes[1].y_offset);
}

// =============================================================================
// Top/Bottom Alignment Tests
// =============================================================================

#[test]
fn test_top_alignment() {
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
fn test_bottom_alignment() {
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
    let box_bottom = boxes[0].y_offset + 25.0;
    assert!((box_bottom - line_metrics.height).abs() < 0.01);
}

#[test]
fn test_top_and_bottom_mixed() {
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

    let bottom_box = InlineBoxMetrics {
        ascent: 15.0,
        descent: 5.0,
        line_height: 20.0,
        baseline_offset: 15.0,
        x_height: None,
        vertical_align: VerticalAlign::Bottom,
    };

    aligner.add_box(top_box);
    aligner.add_box(bottom_box);

    let (line_metrics, boxes) = aligner.align();

    // Top box at top
    assert_eq!(boxes[0].y_offset, 0.0);

    // Bottom box at bottom
    let bottom_box_bottom = boxes[1].y_offset + 20.0;
    assert!((bottom_box_bottom - line_metrics.height).abs() < 0.01);
}

// =============================================================================
// Middle Alignment Tests
// =============================================================================

#[test]
fn test_middle_alignment() {
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

    // Box should fit within line
    assert!(boxes[0].y_offset >= 0.0);
    assert!(boxes[0].y_offset + 20.0 <= line_metrics.height);
}

// =============================================================================
// Super/Sub Script Tests
// =============================================================================

#[test]
fn test_superscript_alignment() {
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

    // Superscript should be raised (smaller y_offset than normal baseline position)
    assert!(boxes[1].y_offset < boxes[0].y_offset);
}

#[test]
fn test_subscript_alignment() {
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

    // Subscript should be lowered (larger y_offset than normal baseline position)
    assert!(boxes[1].y_offset > boxes[0].y_offset);
}

// =============================================================================
// Length/Percentage Offset Tests
// =============================================================================

#[test]
fn test_length_offset_positive() {
    let strut = mock_scaled_metrics(16.0, 12.0, 4.0);
    let mut aligner = BaselineAligner::with_strut(strut.clone());

    let normal = InlineBoxMetrics::from_text(&strut, VerticalAlign::Baseline);
    let raised = InlineBoxMetrics {
        ascent: 12.0,
        descent: 4.0,
        line_height: 16.0,
        baseline_offset: 12.0,
        x_height: None,
        vertical_align: VerticalAlign::Length(8.0), // Raise by 8px
    };

    aligner.add_box(normal);
    aligner.add_box(raised);

    let (_, boxes) = aligner.align();

    // Raised box should be 8px higher (smaller y_offset)
    let difference = boxes[0].y_offset - boxes[1].y_offset;
    assert!((difference - 8.0).abs() < 0.01);
}

#[test]
fn test_length_offset_negative() {
    let strut = mock_scaled_metrics(16.0, 12.0, 4.0);
    let mut aligner = BaselineAligner::with_strut(strut.clone());

    let normal = InlineBoxMetrics::from_text(&strut, VerticalAlign::Baseline);
    let lowered = InlineBoxMetrics {
        ascent: 12.0,
        descent: 4.0,
        line_height: 16.0,
        baseline_offset: 12.0,
        x_height: None,
        vertical_align: VerticalAlign::Length(-5.0), // Lower by 5px
    };

    aligner.add_box(normal);
    aligner.add_box(lowered);

    let (_, boxes) = aligner.align();

    // Lowered box should be 5px lower (larger y_offset)
    let difference = boxes[1].y_offset - boxes[0].y_offset;
    assert!((difference - 5.0).abs() < 0.01);
}

#[test]
fn test_percentage_offset() {
    let strut = mock_scaled_metrics(16.0, 12.0, 4.0);
    let mut aligner = BaselineAligner::with_strut(strut.clone());

    let normal = InlineBoxMetrics::from_text(&strut, VerticalAlign::Baseline);
    let shifted = InlineBoxMetrics {
        ascent: 12.0,
        descent: 4.0,
        line_height: 20.0, // 20px line-height
        baseline_offset: 12.0,
        x_height: None,
        vertical_align: VerticalAlign::Percentage(50.0), // Raise by 50% of 20px = 10px
    };

    aligner.add_box(normal);
    aligner.add_box(shifted);

    let (_, boxes) = aligner.align();

    // Should be raised by 10px
    let difference = boxes[0].y_offset - boxes[1].y_offset;
    assert!((difference - 10.0).abs() < 0.01);
}

// =============================================================================
// Replaced Element Tests
// =============================================================================

#[test]
fn test_image_baseline_alignment() {
    let strut = mock_scaled_metrics(16.0, 12.0, 4.0);
    let mut aligner = BaselineAligner::with_strut(strut.clone());

    let text = InlineBoxMetrics::from_text(&strut, VerticalAlign::Baseline);
    let image = InlineBoxMetrics::from_replaced(50.0, VerticalAlign::Baseline);

    aligner.add_box(text);
    aligner.add_box(image);

    let (line_metrics, boxes) = aligner.align();

    // Image baseline (at bottom) should align with text baseline
    let text_baseline = boxes[0].y_offset + boxes[0].metrics.ascent;
    let image_baseline = boxes[1].y_offset + boxes[1].metrics.ascent;

    assert!((text_baseline - image_baseline).abs() < 0.01);

    // Line should be tall enough
    assert!(line_metrics.height >= 50.0);
}

#[test]
fn test_inline_block_baseline_alignment() {
    let strut = mock_scaled_metrics(16.0, 12.0, 4.0);
    let mut aligner = BaselineAligner::with_strut(strut.clone());

    let text = InlineBoxMetrics::from_text(&strut, VerticalAlign::Baseline);
    // Inline-block with internal baseline at 40px from top
    let inline_block = InlineBoxMetrics::from_inline_block(60.0, 40.0, VerticalAlign::Baseline);

    aligner.add_box(text);
    aligner.add_box(inline_block);

    let (_, boxes) = aligner.align();

    // Both baselines should align
    let text_baseline = boxes[0].y_offset + boxes[0].metrics.ascent;
    let block_baseline = boxes[1].y_offset + boxes[1].metrics.ascent;

    assert!((text_baseline - block_baseline).abs() < 0.01);
}

// =============================================================================
// Complex Scenario Tests
// =============================================================================

#[test]
fn test_mixed_alignment_modes() {
    let strut = mock_scaled_metrics(16.0, 12.0, 4.0);
    let mut aligner = BaselineAligner::with_strut(strut.clone());

    // Normal text
    let text = InlineBoxMetrics::from_text(&strut, VerticalAlign::Baseline);

    // Top-aligned image
    let top_image = InlineBoxMetrics::from_replaced(30.0, VerticalAlign::Top);

    // Bottom-aligned image
    let bottom_image = InlineBoxMetrics::from_replaced(25.0, VerticalAlign::Bottom);

    // Superscript
    let superscript = InlineBoxMetrics {
        ascent: 6.0,
        descent: 2.0,
        line_height: 8.0,
        baseline_offset: 6.0,
        x_height: None,
        vertical_align: VerticalAlign::Super,
    };

    aligner.add_box(text);
    aligner.add_box(top_image);
    aligner.add_box(bottom_image);
    aligner.add_box(superscript);

    let (line_metrics, boxes) = aligner.align();

    // Top image at top
    assert_eq!(boxes[1].y_offset, 0.0);

    // Bottom image at bottom
    let bottom_img_bottom = boxes[2].y_offset + 25.0;
    assert!((bottom_img_bottom - line_metrics.height).abs() < 0.01);

    // Line accommodates all content
    assert!(line_metrics.height >= 30.0); // Top image
    assert!(line_metrics.height >= 25.0); // Bottom image
}

#[test]
fn test_strut_minimum_height() {
    let strut = mock_scaled_metrics(20.0, 16.0, 4.0);
    let mut aligner = BaselineAligner::with_strut(strut);

    // Small text that would result in smaller line
    let small = InlineBoxMetrics {
        ascent: 6.0,
        descent: 2.0,
        line_height: 8.0,
        baseline_offset: 6.0,
        x_height: None,
        vertical_align: VerticalAlign::Baseline,
    };

    aligner.add_box(small);

    let (line_metrics, _) = aligner.align();

    // Line should be at least as tall as strut
    assert!(line_metrics.height >= 20.0);
}

#[test]
fn test_positioned_box_index_preserved() {
    let strut = mock_scaled_metrics(16.0, 12.0, 4.0);
    let mut aligner = BaselineAligner::with_strut(strut);

    for i in 0..5 {
        let metrics = InlineBoxMetrics {
            ascent: 10.0 + i as f32,
            descent: 4.0,
            line_height: 14.0 + i as f32,
            baseline_offset: 10.0 + i as f32,
            x_height: None,
            vertical_align: VerticalAlign::Baseline,
        };
        aligner.add_box(metrics);
    }

    let (_, boxes) = aligner.align();

    // Verify indices are preserved
    for (i, positioned_box) in boxes.iter().enumerate() {
        assert_eq!(positioned_box.index, i);
    }
}

// =============================================================================
// Line Height with Leading Tests
// =============================================================================

#[test]
fn test_line_height_with_leading() {
    let strut = mock_scaled_metrics_with_line_height(16.0, 12.0, 4.0, 24.0);
    let mut aligner = BaselineAligner::with_strut(strut.clone());

    let text = InlineBoxMetrics {
        ascent: 12.0,
        descent: 4.0,
        line_height: 24.0, // 8px leading
        baseline_offset: 12.0,
        x_height: Some(6.0),
        vertical_align: VerticalAlign::Baseline,
    };

    aligner.add_box(text);

    let (line_metrics, boxes) = aligner.align();

    // Line should account for line-height with leading
    assert!(line_metrics.height >= 24.0);

    // Box should have half-leading above and below
    let half_leading = (24.0 - 16.0) / 2.0;
    assert_eq!(boxes[0].metrics.half_leading(), half_leading);
}
