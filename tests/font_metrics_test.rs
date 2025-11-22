//! Comprehensive tests for font metrics extraction
//!
//! Tests cover:
//! - Metrics extraction from various fonts
//! - Scaling metrics by font size
//! - Fallback for missing metrics
//! - Line height calculations
//! - Edge cases and boundary conditions

use fastrender::text::font::metrics::{extract_metrics, FontMetrics, ScaledFontMetrics};

// ============================================================================
// Unit Tests for FontMetrics
// ============================================================================

/// Test 1: Basic line height calculation
#[test]
fn test_line_height_basic_calculation() {
    let metrics = create_test_metrics(1000, 800, -200, 100);

    // line_height = ascent - descent + line_gap
    // = 800 - (-200) + 100 = 1100
    assert_eq!(metrics.line_height(), 1100);
}

/// Test 2: Line height with zero line gap
#[test]
fn test_line_height_zero_gap() {
    let metrics = create_test_metrics(1000, 800, -200, 0);

    // line_height = 800 - (-200) + 0 = 1000
    assert_eq!(metrics.line_height(), 1000);
}

/// Test 3: Line height with large line gap
#[test]
fn test_line_height_large_gap() {
    let metrics = create_test_metrics(1000, 800, -200, 500);

    // line_height = 800 - (-200) + 500 = 1500
    assert_eq!(metrics.line_height(), 1500);
}

/// Test 4: Scale metrics to 16px
#[test]
fn test_scale_to_16px() {
    let metrics = create_test_metrics(1000, 800, -200, 100);
    let scaled = metrics.scale(16.0);

    assert_eq!(scaled.font_size, 16.0);
    assert_eq!(scaled.units_per_em, 1000);
    assert_eq!(scaled.scale, 0.016);

    // Ascent: 800 * 0.016 = 12.8
    assert!((scaled.ascent - 12.8).abs() < 0.001);

    // Descent: made positive: 200 * 0.016 = 3.2
    assert!((scaled.descent - 3.2).abs() < 0.001);

    // Line height: 1100 * 0.016 = 17.6
    assert!((scaled.line_height - 17.6).abs() < 0.001);
}

/// Test 5: Scale metrics to 12px
#[test]
fn test_scale_to_12px() {
    let metrics = create_test_metrics(1000, 800, -200, 100);
    let scaled = metrics.scale(12.0);

    assert_eq!(scaled.font_size, 12.0);
    assert_eq!(scaled.scale, 0.012);

    // Ascent: 800 * 0.012 = 9.6
    assert!((scaled.ascent - 9.6).abs() < 0.001);
}

/// Test 6: Scale metrics to 72px (large font)
#[test]
fn test_scale_to_72px() {
    let metrics = create_test_metrics(1000, 800, -200, 100);
    let scaled = metrics.scale(72.0);

    assert_eq!(scaled.font_size, 72.0);
    assert_eq!(scaled.scale, 0.072);

    // Ascent: 800 * 0.072 = 57.6
    assert!((scaled.ascent - 57.6).abs() < 0.001);
}

/// Test 7: Scale with 2048 units per em (common for TrueType)
#[test]
fn test_scale_with_2048_upem() {
    let metrics = create_test_metrics(2048, 1800, -500, 200);
    let scaled = metrics.scale(16.0);

    let expected_scale = 16.0 / 2048.0;
    assert!((scaled.scale - expected_scale).abs() < 0.0001);

    // Ascent: 1800 * (16/2048) ≈ 14.0625
    assert!((scaled.ascent - 14.0625).abs() < 0.001);
}

/// Test 8: x-height with actual value
#[test]
fn test_x_height_with_value() {
    let mut metrics = create_test_metrics(1000, 800, -200, 100);
    metrics.x_height = Some(500);

    let scaled = metrics.scale(16.0);

    // x_height: 500 * 0.016 = 8.0
    assert!(scaled.x_height.is_some());
    assert!((scaled.x_height.unwrap() - 8.0).abs() < 0.001);
}

/// Test 9: x-height fallback when missing
#[test]
fn test_x_height_fallback() {
    let metrics = create_test_metrics(1000, 800, -200, 100);
    // x_height is None

    // Fallback should be 50% of ascent
    let fallback = metrics.x_height_or_fallback(16.0);
    let expected = 800.0 * 0.5 * (16.0 / 1000.0); // = 6.4
    assert!((fallback - expected).abs() < 0.001);
}

/// Test 10: cap-height with actual value
#[test]
fn test_cap_height_with_value() {
    let mut metrics = create_test_metrics(1000, 800, -200, 100);
    metrics.cap_height = Some(700);

    let scaled = metrics.scale(16.0);

    // cap_height: 700 * 0.016 = 11.2
    assert!(scaled.cap_height.is_some());
    assert!((scaled.cap_height.unwrap() - 11.2).abs() < 0.001);
}

/// Test 11: cap-height fallback when missing
#[test]
fn test_cap_height_fallback() {
    let metrics = create_test_metrics(1000, 800, -200, 100);
    // cap_height is None

    // Fallback should be 70% of ascent
    let fallback = metrics.cap_height_or_fallback(16.0);
    let expected = 800.0 * 0.7 * (16.0 / 1000.0); // = 8.96
    assert!((fallback - expected).abs() < 0.001);
}

/// Test 12: Normal line height calculation
#[test]
fn test_normal_line_height() {
    let metrics = create_test_metrics(1000, 800, -200, 100);

    let normal = metrics.normal_line_height(16.0);
    // line_height = 1100 * (16/1000) = 17.6
    assert!((normal - 17.6).abs() < 0.001);
}

/// Test 13: Scaled metrics baseline offset
#[test]
fn test_baseline_offset() {
    let metrics = create_test_metrics(1000, 800, -200, 100);
    let scaled = metrics.scale(16.0);

    // Baseline offset equals ascent
    assert!((scaled.baseline_offset() - scaled.ascent).abs() < 0.001);
    assert!((scaled.baseline_offset() - 12.8).abs() < 0.001);
}

/// Test 14: Scaled metrics em height
#[test]
fn test_em_height() {
    let metrics = create_test_metrics(1000, 800, -200, 100);
    let scaled = metrics.scale(16.0);

    // em_height = ascent + descent (both positive in scaled)
    // = 12.8 + 3.2 = 16.0
    assert!((scaled.em_height() - 16.0).abs() < 0.001);
}

/// Test 15: Line height with multiplier
#[test]
fn test_line_height_multiplier() {
    let metrics = create_test_metrics(1000, 800, -200, 100);
    let scaled = metrics.scale(16.0);

    let adjusted = scaled.with_line_height_multiplier(1.5);

    // line_height = font_size * 1.5 = 24.0
    assert!((adjusted.line_height - 24.0).abs() < 0.001);
    // Other metrics unchanged
    assert!((adjusted.ascent - scaled.ascent).abs() < 0.001);
}

/// Test 16: Absolute line height
#[test]
fn test_absolute_line_height() {
    let metrics = create_test_metrics(1000, 800, -200, 100);
    let scaled = metrics.scale(16.0);

    let adjusted = scaled.with_line_height(30.0);

    assert_eq!(adjusted.line_height, 30.0);
}

/// Test 17: Half leading calculation
#[test]
fn test_half_leading() {
    let metrics = create_test_metrics(1000, 800, -200, 0);
    let scaled = metrics.scale(16.0);

    // em_height = 16.0, line_height = 16.0
    // half_leading = (16.0 - 16.0) / 2 = 0
    assert!((scaled.half_leading()).abs() < 0.001);

    // With line gap
    let metrics_gap = create_test_metrics(1000, 800, -200, 200);
    let scaled_gap = metrics_gap.scale(16.0);

    // line_height = 1200 * 0.016 = 19.2
    // em_height = 16.0
    // half_leading = (19.2 - 16.0) / 2 = 1.6
    assert!((scaled_gap.half_leading() - 1.6).abs() < 0.001);
}

/// Test 18: Scaled x-height or estimate
#[test]
fn test_scaled_x_height_or_estimate() {
    let mut metrics = create_test_metrics(1000, 800, -200, 100);
    metrics.x_height = Some(500);

    let scaled = metrics.scale(16.0);
    assert!((scaled.x_height_or_estimate() - 8.0).abs() < 0.001);

    // Without x_height
    metrics.x_height = None;
    let scaled_no_x = metrics.scale(16.0);
    // Estimate: 50% of ascent = 12.8 * 0.5 = 6.4
    assert!((scaled_no_x.x_height_or_estimate() - 6.4).abs() < 0.001);
}

/// Test 19: Scaled cap-height or estimate
#[test]
fn test_scaled_cap_height_or_estimate() {
    let mut metrics = create_test_metrics(1000, 800, -200, 100);
    metrics.cap_height = Some(700);

    let scaled = metrics.scale(16.0);
    assert!((scaled.cap_height_or_estimate() - 11.2).abs() < 0.001);

    // Without cap_height
    metrics.cap_height = None;
    let scaled_no_cap = metrics.scale(16.0);
    // Estimate: 70% of ascent = 12.8 * 0.7 = 8.96
    assert!((scaled_no_cap.cap_height_or_estimate() - 8.96).abs() < 0.001);
}

/// Test 20: Underline metrics scaling
#[test]
fn test_underline_metrics_scaling() {
    let mut metrics = create_test_metrics(1000, 800, -200, 100);
    metrics.underline_position = -100;
    metrics.underline_thickness = 50;

    let scaled = metrics.scale(16.0);

    // underline_position: -100 * 0.016 = -1.6
    assert!((scaled.underline_position - (-1.6)).abs() < 0.001);
    // underline_thickness: 50 * 0.016 = 0.8
    assert!((scaled.underline_thickness - 0.8).abs() < 0.001);
}

/// Test 21: Strikeout metrics scaling when present
#[test]
fn test_strikeout_metrics_scaling() {
    let mut metrics = create_test_metrics(1000, 800, -200, 100);
    metrics.strikeout_position = Some(300);
    metrics.strikeout_size = Some(50);

    let scaled = metrics.scale(16.0);

    // strikeout_position: 300 * 0.016 = 4.8
    assert!(scaled.strikeout_position.is_some());
    assert!((scaled.strikeout_position.unwrap() - 4.8).abs() < 0.001);

    // strikeout_size: 50 * 0.016 = 0.8
    assert!(scaled.strikeout_size.is_some());
    assert!((scaled.strikeout_size.unwrap() - 0.8).abs() < 0.001);
}

/// Test 22: Strikeout metrics None when missing
#[test]
fn test_strikeout_metrics_none() {
    let metrics = create_test_metrics(1000, 800, -200, 100);
    // strikeout_position and strikeout_size are None

    let scaled = metrics.scale(16.0);

    assert!(scaled.strikeout_position.is_none());
    assert!(scaled.strikeout_size.is_none());
}

/// Test 23: Average char width scaling
#[test]
fn test_average_char_width_scaling() {
    let mut metrics = create_test_metrics(1000, 800, -200, 100);
    metrics.average_char_width = Some(500);

    let scaled = metrics.scale(16.0);

    // average_char_width: 500 * 0.016 = 8.0
    assert!(scaled.average_char_width.is_some());
    assert!((scaled.average_char_width.unwrap() - 8.0).abs() < 0.001);
}

/// Test 24: Font style flags preserved
#[test]
fn test_font_style_flags() {
    let mut metrics = create_test_metrics(1000, 800, -200, 100);
    metrics.is_bold = true;
    metrics.is_italic = true;
    metrics.is_monospace = false;

    assert!(metrics.is_bold);
    assert!(metrics.is_italic);
    assert!(!metrics.is_monospace);
}

/// Test 25: Very small font size scaling
#[test]
fn test_very_small_font_size() {
    let metrics = create_test_metrics(1000, 800, -200, 100);
    let scaled = metrics.scale(1.0);

    assert_eq!(scaled.font_size, 1.0);
    assert_eq!(scaled.scale, 0.001);
    assert!((scaled.ascent - 0.8).abs() < 0.001);
}

/// Test 26: Very large font size scaling
#[test]
fn test_very_large_font_size() {
    let metrics = create_test_metrics(1000, 800, -200, 100);
    let scaled = metrics.scale(200.0);

    assert_eq!(scaled.font_size, 200.0);
    assert_eq!(scaled.scale, 0.2);
    assert!((scaled.ascent - 160.0).abs() < 0.001);
}

/// Test 27: Edge case - zero descent
#[test]
fn test_zero_descent() {
    let metrics = create_test_metrics(1000, 800, 0, 100);
    let scaled = metrics.scale(16.0);

    assert!((scaled.descent - 0.0).abs() < 0.001);
    // line_height = 800 + 0 + 100 = 900 * 0.016 = 14.4
    assert!((scaled.line_height - 14.4).abs() < 0.001);
}

/// Test 28: ScaledFontMetrics Clone and PartialEq
#[test]
fn test_scaled_metrics_clone_and_eq() {
    let metrics = create_test_metrics(1000, 800, -200, 100);
    let scaled1 = metrics.scale(16.0);
    let scaled2 = scaled1.clone();

    assert_eq!(scaled1, scaled2);
    assert!((scaled1.ascent - scaled2.ascent).abs() < 0.001);
}

/// Test 29: FontMetrics Clone and PartialEq
#[test]
fn test_font_metrics_clone_and_eq() {
    let metrics1 = create_test_metrics(1000, 800, -200, 100);
    let metrics2 = metrics1.clone();

    assert_eq!(metrics1, metrics2);
    assert_eq!(metrics1.units_per_em, metrics2.units_per_em);
}

/// Test 30: Debug formatting
#[test]
fn test_debug_formatting() {
    let metrics = create_test_metrics(1000, 800, -200, 100);
    let debug_str = format!("{:?}", metrics);

    assert!(debug_str.contains("FontMetrics"));
    assert!(debug_str.contains("units_per_em: 1000"));
    assert!(debug_str.contains("ascent: 800"));
}

// ============================================================================
// Integration Tests with Real Font Files (if available)
// ============================================================================

/// Test with system font if available
#[test]
fn test_with_fontdb_system_font() {
    use fontdb::Database;

    let mut db = Database::new();
    db.load_system_fonts();

    // Try to find any available font
    if db.faces().count() > 0 {
        let first_face = db.faces().next().unwrap();
        let id = first_face.id;

        // Load the font data
        if let Some((font_data, face_index)) = db.with_face_data(id, |data, index| (data.to_vec(), index)) {
            let result = FontMetrics::from_font_data(&font_data, face_index);
            assert!(result.is_ok(), "Should be able to extract metrics from system font");

            let metrics = result.unwrap();
            assert!(metrics.units_per_em > 0, "units_per_em should be positive");
            assert!(metrics.ascent > 0, "ascent should be positive");
            assert!(metrics.descent < 0, "descent should be negative");
            assert!(metrics.glyph_count > 0, "should have glyphs");
        }
    }
    // If no system fonts available, test passes silently
}

/// Test extract_metrics convenience function
#[test]
fn test_extract_metrics_function() {
    use fontdb::Database;

    let mut db = Database::new();
    db.load_system_fonts();

    // Try to find any available font
    if db.faces().count() > 0 {
        let first_face = db.faces().next().unwrap();
        let id = first_face.id;

        if let Some((font_data, face_index)) = db.with_face_data(id, |data, index| (data.to_vec(), index)) {
            let face = ttf_parser::Face::parse(&font_data, face_index).unwrap();
            let result = extract_metrics(&face);
            assert!(result.is_ok());
        }
    }
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Create test FontMetrics with specified values
fn create_test_metrics(units_per_em: u16, ascent: i16, descent: i16, line_gap: i16) -> FontMetrics {
    FontMetrics {
        units_per_em,
        ascent,
        descent,
        line_gap,
        x_height: None,
        cap_height: None,
        underline_position: -100,
        underline_thickness: 50,
        strikeout_position: None,
        strikeout_size: None,
        is_bold: false,
        is_italic: false,
        is_monospace: false,
        average_char_width: None,
        glyph_count: 256,
    }
}
