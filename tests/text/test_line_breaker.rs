//! Integration tests for the greedy line breaker
//!
//! Tests the line breaking functionality with various scenarios
//! including edge cases, Unicode text, and realistic text layouts.

use fastrender::text::line_breaker::{
    break_lines, break_lines_checked, BreakOpportunity, BreakType, GlyphPosition, GreedyLineBreaker, Line, LineSegment,
};

// ============================================================================
// Helper Functions
// ============================================================================

/// Create glyphs with uniform width
fn uniform_glyphs(count: usize, width: f32) -> Vec<GlyphPosition> {
    (0..count)
        .map(|i| GlyphPosition::new(i as u32, i, width))
        .collect()
}

/// Create glyphs with varying widths
fn varying_glyphs(widths: &[f32]) -> Vec<GlyphPosition> {
    widths
        .iter()
        .enumerate()
        .map(|(i, &w)| GlyphPosition::new(i as u32, i, w))
        .collect()
}

/// Create normal break opportunities at specified positions
fn normal_breaks(positions: &[usize]) -> Vec<BreakOpportunity> {
    positions.iter().map(|&p| BreakOpportunity::normal(p)).collect()
}

/// Simulate word-shaped glyphs (for realistic testing)
/// Returns glyphs representing "Hello World" style text
fn word_like_glyphs() -> Vec<GlyphPosition> {
    // Simulates: "The quick brown fox" with spaces
    // Each "word" is 3-5 glyphs, each glyph ~10px
    let mut glyphs = Vec::new();
    let mut cluster = 0;

    // "The " (4 chars)
    for _ in 0..4 {
        glyphs.push(GlyphPosition::new(cluster as u32, cluster, 10.0));
        cluster += 1;
    }

    // "quick " (6 chars)
    for _ in 0..6 {
        glyphs.push(GlyphPosition::new(cluster as u32, cluster, 10.0));
        cluster += 1;
    }

    // "brown " (6 chars)
    for _ in 0..6 {
        glyphs.push(GlyphPosition::new(cluster as u32, cluster, 10.0));
        cluster += 1;
    }

    // "fox" (3 chars)
    for _ in 0..3 {
        glyphs.push(GlyphPosition::new(cluster as u32, cluster, 10.0));
        cluster += 1;
    }

    glyphs
}

/// Break opportunities for word boundaries in word_like_glyphs
fn word_breaks() -> Vec<BreakOpportunity> {
    vec![
        BreakOpportunity::normal(4),  // After "The "
        BreakOpportunity::normal(10), // After "quick "
        BreakOpportunity::normal(16), // After "brown "
    ]
}

// ============================================================================
// Basic Tests
// ============================================================================

#[test]
fn test_break_lines_empty_input() {
    let lines = break_lines(&[], 100.0, &[]);
    assert_eq!(lines.len(), 1, "Empty input should return one empty line");
    assert!(lines[0].is_empty(), "Line should be empty");
    assert!(lines[0].is_last, "Single line should be marked as last");
}

#[test]
fn test_break_lines_single_glyph() {
    let glyphs = uniform_glyphs(1, 10.0);
    let lines = break_lines(&glyphs, 100.0, &[]);

    assert_eq!(lines.len(), 1);
    assert_eq!(lines[0].glyph_count(), 1);
    assert_eq!(lines[0].width, 10.0);
    assert!(lines[0].is_last);
}

#[test]
fn test_break_lines_all_fit() {
    let glyphs = uniform_glyphs(5, 10.0);
    let lines = break_lines(&glyphs, 100.0, &[]);

    assert_eq!(lines.len(), 1);
    assert_eq!(lines[0].glyph_count(), 5);
    assert_eq!(lines[0].width, 50.0);
    assert!(lines[0].is_last);
}

#[test]
fn test_break_lines_exact_fit() {
    let glyphs = uniform_glyphs(10, 10.0);
    let lines = break_lines(&glyphs, 100.0, &[]);

    // Exactly 100px of glyphs in 100px width
    assert_eq!(lines.len(), 1);
    assert_eq!(lines[0].width, 100.0);
}

// ============================================================================
// Line Breaking Tests
// ============================================================================

#[test]
fn test_break_at_opportunity() {
    let glyphs = uniform_glyphs(10, 10.0); // 100px total
    let breaks = normal_breaks(&[5]); // Break after 5th glyph

    let lines = break_lines(&glyphs, 60.0, &breaks); // Max 60px

    assert_eq!(lines.len(), 2);
    assert_eq!(lines[0].glyph_count(), 5); // First 5 glyphs (50px)
    assert_eq!(lines[1].glyph_count(), 5); // Remaining 5 glyphs
    assert!(!lines[0].is_last);
    assert!(lines[1].is_last);
}

#[test]
fn test_multiple_line_breaks() {
    let glyphs = uniform_glyphs(15, 10.0); // 150px total
    let breaks = normal_breaks(&[5, 10]); // Break at 5 and 10

    let lines = break_lines(&glyphs, 55.0, &breaks); // Max 55px

    assert_eq!(lines.len(), 3);
    assert_eq!(lines[0].glyph_count(), 5);
    assert_eq!(lines[1].glyph_count(), 5);
    assert_eq!(lines[2].glyph_count(), 5);
}

#[test]
fn test_word_wrapping() {
    let glyphs = word_like_glyphs();
    let breaks = word_breaks();

    // Width that fits "The quick" but not "The quick brown"
    let lines = break_lines(&glyphs, 105.0, &breaks);

    // Should break after "The quick " (100px) since "brown" doesn't fit
    assert!(lines.len() >= 2);

    // Total glyphs should equal input
    let total: usize = lines.iter().map(|l| l.glyph_count()).sum();
    assert_eq!(total, 19);
}

// ============================================================================
// Hard Break Tests
// ============================================================================

#[test]
fn test_hard_break() {
    let glyphs = uniform_glyphs(10, 10.0);
    let breaks = vec![BreakOpportunity::hard(5)];

    let lines = break_lines(&glyphs, 200.0, &breaks); // Width larger than needed

    // Should break at position 5 regardless of width
    assert_eq!(lines.len(), 2);
    assert_eq!(lines[0].glyph_count(), 5);
    assert_eq!(lines[1].glyph_count(), 5);
}

#[test]
fn test_multiple_hard_breaks() {
    let glyphs = uniform_glyphs(9, 10.0);
    let breaks = vec![
        BreakOpportunity::hard(3),
        BreakOpportunity::hard(6),
    ];

    let lines = break_lines(&glyphs, 200.0, &breaks);

    assert_eq!(lines.len(), 3);
    assert_eq!(lines[0].glyph_count(), 3);
    assert_eq!(lines[1].glyph_count(), 3);
    assert_eq!(lines[2].glyph_count(), 3);
}

#[test]
fn test_hard_break_at_start() {
    let glyphs = uniform_glyphs(5, 10.0);
    let breaks = vec![BreakOpportunity::hard(0)];

    let lines = break_lines(&glyphs, 200.0, &breaks);

    // Hard break at start creates empty first line
    assert!(lines.len() >= 1);
}

// ============================================================================
// Overflow Tests
// ============================================================================

#[test]
fn test_no_break_opportunity_overflow() {
    let glyphs = uniform_glyphs(5, 30.0); // 150px total
    let lines = break_lines(&glyphs, 50.0, &[]); // No break opportunities

    // All on one line (overflow)
    assert_eq!(lines.len(), 1);
    assert_eq!(lines[0].width, 150.0);
    assert!(lines[0].width > lines[0].max_width);
}

#[test]
fn test_first_glyph_overflow() {
    let glyphs = vec![GlyphPosition::new(0, 0, 100.0)]; // Single wide glyph
    let lines = break_lines(&glyphs, 50.0, &[]);

    // Glyph must be included even if it overflows
    assert_eq!(lines.len(), 1);
    assert_eq!(lines[0].width, 100.0);
}

#[test]
fn test_wide_word_overflow() {
    // Simulate a very long word with no internal breaks
    let glyphs = uniform_glyphs(10, 20.0); // 200px "word"
    let breaks = vec![
        BreakOpportunity::normal(0),  // Before word
        BreakOpportunity::normal(10), // After word
    ];

    let lines = break_lines(&glyphs, 100.0, &breaks);

    // Word is wider than max_width, should still be on its own line
    assert!(!lines.is_empty());
    assert!(lines[0].width > 100.0 || lines[0].glyph_count() == 10);
}

// ============================================================================
// Hyphenation Tests
// ============================================================================

#[test]
fn test_hyphen_break() {
    let glyphs = uniform_glyphs(10, 10.0);
    let breaks = vec![BreakOpportunity::hyphen(5)];

    let lines = break_lines(&glyphs, 55.0, &breaks);

    assert_eq!(lines.len(), 2);
    assert!(lines[0].has_hyphen());
    assert!(!lines[1].has_hyphen());
}

#[test]
fn test_hyphen_flag_preserved() {
    let glyphs = uniform_glyphs(6, 10.0);
    let breaks = vec![BreakOpportunity::hyphen(3)];

    let lines = break_lines(&glyphs, 35.0, &breaks);

    // Find line with hyphen
    let hyphen_lines: Vec<_> = lines.iter().filter(|l| l.has_hyphen()).collect();
    assert_eq!(hyphen_lines.len(), 1);
}

// ============================================================================
// Emergency Break Tests
// ============================================================================

#[test]
fn test_emergency_break() {
    let glyphs = uniform_glyphs(5, 30.0);
    let breaks = vec![BreakOpportunity::emergency(3)];

    let lines = break_lines(&glyphs, 95.0, &breaks);

    // Should use emergency break
    assert!(lines.len() >= 2);
}

// ============================================================================
// Special Width Tests
// ============================================================================

#[test]
fn test_infinite_width() {
    let glyphs = uniform_glyphs(100, 10.0);
    let breaks = normal_breaks(&(1..100).collect::<Vec<_>>());

    let lines = break_lines(&glyphs, f32::INFINITY, &breaks);

    // Single line with all glyphs
    assert_eq!(lines.len(), 1);
    assert_eq!(lines[0].glyph_count(), 100);
}

#[test]
fn test_very_large_width() {
    let glyphs = uniform_glyphs(50, 10.0);
    let breaks = normal_breaks(&(1..50).collect::<Vec<_>>());

    let lines = break_lines(&glyphs, 1e10, &breaks);

    assert_eq!(lines.len(), 1);
}

#[test]
fn test_zero_width() {
    let glyphs = uniform_glyphs(5, 10.0);
    let lines = break_lines(&glyphs, 0.0, &[]);

    // Should still produce output (overflow)
    assert!(!lines.is_empty());
}

// ============================================================================
// Varying Glyph Width Tests
// ============================================================================

#[test]
fn test_varying_glyph_widths() {
    let glyphs = varying_glyphs(&[5.0, 10.0, 15.0, 20.0, 25.0]); // 75px total
    let breaks = normal_breaks(&[2, 4]);

    let lines = break_lines(&glyphs, 35.0, &breaks);

    // Verify all glyphs accounted for
    let total: usize = lines.iter().map(|l| l.glyph_count()).sum();
    assert_eq!(total, 5);
}

#[test]
fn test_zero_width_glyphs() {
    // Some glyphs (like zero-width joiners) have 0 advance
    let glyphs = varying_glyphs(&[10.0, 0.0, 10.0, 0.0, 10.0]);
    let lines = break_lines(&glyphs, 100.0, &[]);

    assert_eq!(lines[0].glyph_count(), 5);
    assert_eq!(lines[0].width, 30.0);
}

// ============================================================================
// Line Properties Tests
// ============================================================================

#[test]
fn test_line_remaining_space() {
    let glyphs = uniform_glyphs(5, 10.0); // 50px
    let lines = break_lines(&glyphs, 100.0, &[]);

    assert_eq!(lines[0].remaining_space(), 50.0);
}

#[test]
fn test_line_fill_ratio() {
    let glyphs = uniform_glyphs(5, 10.0); // 50px
    let lines = break_lines(&glyphs, 100.0, &[]);

    assert_eq!(lines[0].fill_ratio(), 0.5);
}

#[test]
fn test_line_is_last_flag() {
    let glyphs = uniform_glyphs(10, 10.0);
    let breaks = normal_breaks(&[5]);

    let lines = break_lines(&glyphs, 55.0, &breaks);

    assert_eq!(lines.len(), 2);
    assert!(!lines[0].is_last);
    assert!(lines[1].is_last);
}

// ============================================================================
// Result API Tests
// ============================================================================

#[test]
fn test_break_lines_checked_valid() {
    let glyphs = uniform_glyphs(5, 10.0);
    let result = break_lines_checked(&glyphs, 100.0, &[]);

    assert!(result.is_ok());
    assert_eq!(result.unwrap().len(), 1);
}

#[test]
fn test_break_lines_checked_negative_width() {
    let glyphs = uniform_glyphs(5, 10.0);
    let result = break_lines_checked(&glyphs, -10.0, &[]);

    assert!(result.is_err());
}

#[test]
fn test_break_lines_checked_nan_width() {
    let glyphs = uniform_glyphs(5, 10.0);
    let result = break_lines_checked(&glyphs, f32::NAN, &[]);

    assert!(result.is_err());
}

// ============================================================================
// GreedyLineBreaker Struct Tests
// ============================================================================

#[test]
fn test_greedy_line_breaker_new() {
    let breaker = GreedyLineBreaker::new();
    assert!(breaker.allow_overflow);
    assert_eq!(breaker.min_fill_ratio, 0.0);
}

#[test]
fn test_greedy_line_breaker_builder() {
    let breaker = GreedyLineBreaker::new()
        .with_overflow(false)
        .with_min_fill(0.75);

    assert!(!breaker.allow_overflow);
    assert_eq!(breaker.min_fill_ratio, 0.75);
}

#[test]
fn test_greedy_line_breaker_break_lines() {
    let breaker = GreedyLineBreaker::new();
    let glyphs = uniform_glyphs(5, 10.0);
    let lines = breaker.break_lines(&glyphs, 100.0, &[]);

    assert_eq!(lines.len(), 1);
}

#[test]
fn test_greedy_line_breaker_checked() {
    let breaker = GreedyLineBreaker::new();
    let glyphs = uniform_glyphs(5, 10.0);
    let result = breaker.break_lines_checked(&glyphs, 100.0, &[]);

    assert!(result.is_ok());
}

// ============================================================================
// Type Tests
// ============================================================================

#[test]
fn test_glyph_position_new() {
    let glyph = GlyphPosition::new(42, 10, 15.5);

    assert_eq!(glyph.glyph_id, 42);
    assert_eq!(glyph.cluster, 10);
    assert_eq!(glyph.x_advance, 15.5);
    assert_eq!(glyph.x_offset, 0.0);
    assert_eq!(glyph.y_offset, 0.0);
}

#[test]
fn test_glyph_position_with_offsets() {
    let glyph = GlyphPosition::with_offsets(42, 10, 15.5, 2.0, -3.0);

    assert_eq!(glyph.x_offset, 2.0);
    assert_eq!(glyph.y_offset, -3.0);
}

#[test]
fn test_break_opportunity_constructors() {
    let normal = BreakOpportunity::normal(5);
    assert_eq!(normal.break_type, BreakType::Normal);
    assert_eq!(normal.penalty, 10);
    assert!(!normal.is_mandatory());
    assert!(normal.is_allowed());

    let hard = BreakOpportunity::hard(10);
    assert_eq!(hard.break_type, BreakType::Hard);
    assert_eq!(hard.penalty, 0);
    assert!(hard.is_mandatory());

    let hyphen = BreakOpportunity::hyphen(15);
    assert_eq!(hyphen.break_type, BreakType::Hyphen);
    assert_eq!(hyphen.penalty, 100);

    let emergency = BreakOpportunity::emergency(20);
    assert_eq!(emergency.break_type, BreakType::Emergency);
    assert_eq!(emergency.penalty, 1000);
}

#[test]
fn test_line_segment_methods() {
    let segment = LineSegment::new(5, 10, 100.0);

    assert_eq!(segment.glyph_start, 5);
    assert_eq!(segment.glyph_count, 10);
    assert_eq!(segment.glyph_end(), 15);
    assert_eq!(segment.width, 100.0);
    assert!(!segment.is_empty());
    assert!(!segment.has_hyphen);
}

#[test]
fn test_line_methods() {
    let mut line = Line::new(200.0);

    assert!(line.is_empty());
    assert_eq!(line.glyph_count(), 0);
    assert_eq!(line.max_width, 200.0);

    line.segments.push(LineSegment::new(0, 5, 80.0));
    line.width = 80.0;

    assert!(!line.is_empty());
    assert_eq!(line.glyph_count(), 5);
    assert_eq!(line.remaining_space(), 120.0);
    assert_eq!(line.fill_ratio(), 0.4);
}

// ============================================================================
// Edge Cases
// ============================================================================

#[test]
fn test_break_at_every_position() {
    let glyphs = uniform_glyphs(10, 10.0);
    let breaks: Vec<_> = (0..10).map(BreakOpportunity::normal).collect();

    let lines = break_lines(&glyphs, 25.0, &breaks);

    // Should break every 2 glyphs
    assert!(lines.len() > 1);

    // All glyphs accounted for
    let total: usize = lines.iter().map(|l| l.glyph_count()).sum();
    assert_eq!(total, 10);
}

#[test]
fn test_break_with_no_matching_opportunities() {
    // Breaks at positions that don't match any glyph cluster
    let glyphs = uniform_glyphs(5, 20.0); // clusters 0-4
    let breaks = normal_breaks(&[100, 200, 300]); // non-existent positions

    let lines = break_lines(&glyphs, 50.0, &breaks);

    // Should overflow (no usable breaks)
    assert_eq!(lines.len(), 1);
    assert_eq!(lines[0].width, 100.0);
}

#[test]
fn test_many_glyphs_performance() {
    // Performance test with many glyphs
    let glyphs = uniform_glyphs(1000, 10.0);
    let breaks: Vec<_> = (0..1000).step_by(10).map(BreakOpportunity::normal).collect();

    let lines = break_lines(&glyphs, 95.0, &breaks);

    // Should handle without issue
    assert!(!lines.is_empty());

    // All glyphs accounted for
    let total: usize = lines.iter().map(|l| l.glyph_count()).sum();
    assert_eq!(total, 1000);
}

#[test]
fn test_single_break_at_end() {
    let glyphs = uniform_glyphs(5, 10.0);
    let breaks = normal_breaks(&[5]); // Break at very end

    let lines = break_lines(&glyphs, 45.0, &breaks);

    // Should break before the end
    assert!(lines.len() >= 1);
}

// ============================================================================
// Segment Tests
// ============================================================================

#[test]
fn test_segment_glyph_indices() {
    let glyphs = uniform_glyphs(10, 10.0);
    let breaks = normal_breaks(&[5]);

    let lines = break_lines(&glyphs, 55.0, &breaks);

    // Check segment indices
    assert_eq!(lines[0].segments[0].glyph_start, 0);
    assert_eq!(lines[0].segments[0].glyph_count, 5);
    assert_eq!(lines[0].segments[0].glyph_end(), 5);

    assert_eq!(lines[1].segments[0].glyph_start, 5);
    assert_eq!(lines[1].segments[0].glyph_count, 5);
    assert_eq!(lines[1].segments[0].glyph_end(), 10);
}

// ============================================================================
// Default Tests
// ============================================================================

#[test]
fn test_break_type_default() {
    assert_eq!(BreakType::default(), BreakType::Normal);
}

#[test]
fn test_break_opportunity_default() {
    let opp = BreakOpportunity::default();
    assert_eq!(opp.position, 0);
    assert_eq!(opp.break_type, BreakType::Normal);
}

#[test]
fn test_line_default() {
    let line = Line::default();
    assert!(line.is_empty());
    assert_eq!(line.max_width, f32::INFINITY);
}

#[test]
fn test_greedy_line_breaker_default() {
    let breaker = GreedyLineBreaker::default();
    assert!(breaker.allow_overflow);
    assert_eq!(breaker.min_fill_ratio, 0.0);
}
