//! Comprehensive tests for text justification
//!
//! Tests the text justification algorithm following CSS Text Module Level 3.

use fastrender::text::justify::apply_alignment;
use fastrender::text::justify::calculate_line_width;
use fastrender::text::justify::detect_justification_mode;
use fastrender::text::justify::is_cjk_character;
use fastrender::text::justify::justify_line;
use fastrender::text::justify::justify_line_with_options;
use fastrender::text::justify::justify_lines;
use fastrender::text::justify::mark_word_boundaries;
use fastrender::text::justify::mark_word_boundaries_by_glyph_id;
use fastrender::text::justify::GlyphPosition;
use fastrender::text::justify::JustificationMode;
use fastrender::text::justify::JustificationOptions;
use fastrender::text::justify::JustificationResult;
use fastrender::text::justify::TextAlignment;

// ============================================================================
// Helper Functions
// ============================================================================

/// Create a simple glyph for testing
fn glyph(id: u16, x: f32, advance: f32, is_word_boundary: bool) -> GlyphPosition {
  GlyphPosition::new(id, x, 0.0, advance, is_word_boundary)
}

/// Create a glyph with cluster information
fn glyph_with_cluster(
  id: u16,
  x: f32,
  advance: f32,
  is_word_boundary: bool,
  cluster: usize,
) -> GlyphPosition {
  GlyphPosition::with_cluster(id, x, 0.0, advance, 0.0, is_word_boundary, cluster)
}

/// Assert float values are approximately equal
fn assert_approx_eq(a: f32, b: f32, msg: &str) {
  assert!((a - b).abs() < 0.01, "{}: expected {}, got {}", msg, b, a);
}

// ============================================================================
// GlyphPosition Tests
// ============================================================================

#[test]
fn test_glyph_position_new() {
  let g = GlyphPosition::new(42, 10.0, 5.0, 8.0, true);

  assert_eq!(g.glyph_id, 42);
  assert_eq!(g.x, 10.0);
  assert_eq!(g.y, 5.0);
  assert_eq!(g.x_advance, 8.0);
  assert_eq!(g.y_advance, 0.0);
  assert!(g.is_word_boundary);
  assert_eq!(g.cluster, 0);
}

#[test]
fn test_glyph_position_with_cluster() {
  let g = GlyphPosition::with_cluster(1, 20.0, 3.0, 10.0, 2.0, false, 5);

  assert_eq!(g.glyph_id, 1);
  assert_eq!(g.x, 20.0);
  assert_eq!(g.y, 3.0);
  assert_eq!(g.x_advance, 10.0);
  assert_eq!(g.y_advance, 2.0);
  assert!(!g.is_word_boundary);
  assert_eq!(g.cluster, 5);
}

#[test]
fn test_glyph_position_default() {
  let g = GlyphPosition::default();

  assert_eq!(g.glyph_id, 0);
  assert_eq!(g.x, 0.0);
  assert_eq!(g.y, 0.0);
  assert_eq!(g.x_advance, 0.0);
  assert_eq!(g.y_advance, 0.0);
  assert!(!g.is_word_boundary);
  assert_eq!(g.cluster, 0);
}

#[test]
fn test_glyph_position_end_x() {
  let g = GlyphPosition::new(0, 15.0, 0.0, 25.0, false);
  assert_eq!(g.end_x(), 40.0);
}

#[test]
fn test_glyph_position_equality() {
  let g1 = GlyphPosition::new(1, 10.0, 5.0, 8.0, true);
  let g2 = GlyphPosition::new(1, 10.0, 5.0, 8.0, true);
  let g3 = GlyphPosition::new(2, 10.0, 5.0, 8.0, true);

  assert_eq!(g1, g2);
  assert_ne!(g1, g3);
}

// ============================================================================
// JustificationOptions Tests
// ============================================================================

#[test]
fn test_justification_options_default() {
  let options = JustificationOptions::default();

  assert_eq!(options.min_fill_ratio, 0.75);
  assert_eq!(options.max_word_spacing_ratio, 3.0);
  assert!(options.use_letter_spacing_fallback);
  assert_eq!(options.max_letter_spacing, 2.0);
  assert!(!options.justify_last_line);
}

#[test]
fn test_justification_options_builder() {
  let options = JustificationOptions::new()
    .with_min_fill_ratio(0.5)
    .with_max_word_spacing_ratio(2.5)
    .with_letter_spacing_fallback(false)
    .with_max_letter_spacing(3.0)
    .with_justify_last_line(true);

  assert_eq!(options.min_fill_ratio, 0.5);
  assert_eq!(options.max_word_spacing_ratio, 2.5);
  assert!(!options.use_letter_spacing_fallback);
  assert_eq!(options.max_letter_spacing, 3.0);
  assert!(options.justify_last_line);
}

#[test]
fn test_justification_options_clamp_min_fill_ratio() {
  let options = JustificationOptions::new().with_min_fill_ratio(2.0);
  assert_eq!(options.min_fill_ratio, 1.0);

  let options = JustificationOptions::new().with_min_fill_ratio(-0.5);
  assert_eq!(options.min_fill_ratio, 0.0);
}

#[test]
fn test_justification_options_clamp_max_word_spacing() {
  let options = JustificationOptions::new().with_max_word_spacing_ratio(0.5);
  assert_eq!(options.max_word_spacing_ratio, 1.0);
}

// ============================================================================
// Basic Justification Tests
// ============================================================================

#[test]
fn test_justify_line_empty() {
  let mut glyphs: Vec<GlyphPosition> = vec![];
  justify_line(&mut glyphs, 100.0, 0.0);
  assert!(glyphs.is_empty());
}

#[test]
fn test_justify_line_single_glyph() {
  let mut glyphs = vec![glyph(0, 0.0, 50.0, false)];

  justify_line(&mut glyphs, 100.0, 50.0);

  // Single glyph without word boundary - no justification
  assert_eq!(glyphs[0].x, 0.0);
}

#[test]
fn test_justify_line_no_extra_space_needed() {
  let mut glyphs = vec![glyph(0, 0.0, 100.0, false)];

  justify_line(&mut glyphs, 100.0, 100.0);

  assert_eq!(glyphs[0].x, 0.0);
}

#[test]
fn test_justify_line_target_less_than_current() {
  let mut glyphs = vec![glyph(0, 0.0, 100.0, false)];

  justify_line(&mut glyphs, 50.0, 100.0);

  // Should not shrink
  assert_eq!(glyphs[0].x, 0.0);
}

#[test]
fn test_justify_line_two_words() {
  // "Hello World" - 50px + 50px = 100px, target = 120px (ratio 0.83 > 0.75)
  let mut glyphs = vec![
    glyph(0, 0.0, 50.0, true),   // "Hello" ends word
    glyph(1, 50.0, 50.0, false), // "World"
  ];

  justify_line(&mut glyphs, 120.0, 100.0);

  // First glyph stays at 0
  assert_eq!(glyphs[0].x, 0.0);
  // Second glyph moves by extra space (20px)
  assert_approx_eq(glyphs[1].x, 70.0, "Second glyph x position");
}

#[test]
fn test_justify_line_three_words() {
  // "One Two Three" - 30px each = 90px, target = 120px
  let mut glyphs = vec![
    glyph(0, 0.0, 30.0, true),   // "One" ends word
    glyph(1, 30.0, 30.0, true),  // "Two" ends word
    glyph(2, 60.0, 30.0, false), // "Three"
  ];

  justify_line(&mut glyphs, 120.0, 90.0);

  // Extra space = 30, distributed among 2 boundaries = 15 each
  assert_eq!(glyphs[0].x, 0.0);
  assert_approx_eq(glyphs[1].x, 45.0, "Second glyph x"); // 30 + 15
  assert_approx_eq(glyphs[2].x, 90.0, "Third glyph x"); // 60 + 30
}

#[test]
fn test_justify_line_four_words() {
  // Four words, 25px each = 100px, target = 125px (ratio 0.8 > 0.75)
  let mut glyphs = vec![
    glyph(0, 0.0, 25.0, true),
    glyph(1, 25.0, 25.0, true),
    glyph(2, 50.0, 25.0, true),
    glyph(3, 75.0, 25.0, false),
  ];

  justify_line(&mut glyphs, 125.0, 100.0);

  // Extra = 25, 3 boundaries = ~8.33 each
  assert_eq!(glyphs[0].x, 0.0);
  assert_approx_eq(glyphs[1].x, 33.33, "g1"); // 25 + 8.33
  assert_approx_eq(glyphs[2].x, 66.66, "g2"); // 50 + 16.66
  assert_approx_eq(glyphs[3].x, 100.0, "g3"); // 75 + 25
}

// ============================================================================
// Letter Spacing Fallback Tests
// ============================================================================

#[test]
fn test_justify_line_letter_spacing_no_word_boundaries() {
  // No word boundaries - should use letter spacing
  // Line width 60px, target 75px (ratio 0.8 > 0.75)
  let mut glyphs = vec![
    glyph(0, 0.0, 20.0, false),
    glyph(1, 20.0, 20.0, false),
    glyph(2, 40.0, 20.0, false),
  ];

  let options = JustificationOptions::new()
    .with_letter_spacing_fallback(true)
    .with_max_letter_spacing(10.0); // Allow larger letter spacing

  let result = justify_line_with_options(&mut glyphs, 75.0, 60.0, &options);

  assert!(result.is_justified);
  assert!(result.space_per_letter > 0.0);
  assert_eq!(result.word_boundaries_used, 0);

  // Extra = 15, 2 gaps = 7.5 each
  assert_eq!(glyphs[0].x, 0.0);
  assert_approx_eq(glyphs[1].x, 27.5, "g1 letter spacing");
  assert_approx_eq(glyphs[2].x, 55.0, "g2 letter spacing");
}

#[test]
fn test_justify_line_letter_spacing_disabled() {
  // No word boundaries and letter spacing disabled
  let mut glyphs = vec![glyph(0, 0.0, 20.0, false), glyph(1, 20.0, 20.0, false)];

  let options = JustificationOptions::new().with_letter_spacing_fallback(false);

  let result = justify_line_with_options(&mut glyphs, 80.0, 40.0, &options);

  // Should not justify because no word boundaries and letter spacing disabled
  assert!(!result.is_justified);
  assert_eq!(glyphs[0].x, 0.0);
  assert_eq!(glyphs[1].x, 20.0);
}

#[test]
fn test_justify_line_max_letter_spacing() {
  // Test that letter spacing is capped
  let mut glyphs = vec![glyph(0, 0.0, 10.0, false), glyph(1, 10.0, 10.0, false)];

  let options = JustificationOptions::new()
    .with_letter_spacing_fallback(true)
    .with_max_letter_spacing(5.0);

  // Request 100px extra space, but max letter spacing is 5px
  let result = justify_line_with_options(&mut glyphs, 120.0, 20.0, &options);

  // Should be capped at max_letter_spacing
  assert!(result.space_per_letter <= 5.0);
}

// ============================================================================
// Combined Spacing Tests
// ============================================================================

#[test]
fn test_justify_line_combined_spacing() {
  // When word spacing would be extreme, use combined approach
  let mut glyphs = vec![
    glyph(0, 0.0, 10.0, true), // Very small word ending
    glyph(1, 10.0, 10.0, false),
  ];

  let options = JustificationOptions::new()
    .with_min_fill_ratio(0.1) // Allow short lines
    .with_max_word_spacing_ratio(1.5)
    .with_letter_spacing_fallback(true);

  // Large extra space that would exceed max_word_spacing_ratio
  let result = justify_line_with_options(&mut glyphs, 100.0, 20.0, &options);

  // Should use combined spacing
  assert!(result.is_justified);
}

// ============================================================================
// Minimum Fill Ratio Tests
// ============================================================================

#[test]
fn test_justify_line_below_min_fill_ratio() {
  // Line is too short - should not justify
  let mut glyphs = vec![glyph(0, 0.0, 30.0, true)];

  let options = JustificationOptions::new().with_min_fill_ratio(0.75);

  let result = justify_line_with_options(&mut glyphs, 100.0, 30.0, &options);

  // 30/100 = 0.3 < 0.75, should not justify
  assert!(!result.is_justified);
}

#[test]
fn test_justify_line_above_min_fill_ratio() {
  // Line is long enough - should justify
  let mut glyphs = vec![glyph(0, 0.0, 40.0, true), glyph(1, 40.0, 40.0, false)];

  let options = JustificationOptions::new().with_min_fill_ratio(0.75);

  let result = justify_line_with_options(&mut glyphs, 100.0, 80.0, &options);

  // 80/100 = 0.8 > 0.75, should justify
  assert!(result.is_justified);
}

// ============================================================================
// Multi-line Justification Tests
// ============================================================================

#[test]
fn test_justify_lines_basic() {
  let mut lines = vec![
    vec![glyph(0, 0.0, 40.0, true), glyph(1, 40.0, 40.0, false)],
    vec![glyph(2, 0.0, 50.0, true), glyph(3, 50.0, 30.0, false)],
  ];

  let options = JustificationOptions::new().with_justify_last_line(true);

  let results = justify_lines(&mut lines, 100.0, &options);

  assert!(results[0].is_justified);
  assert!(results[1].is_justified);
}

#[test]
fn test_justify_lines_skip_last() {
  let mut lines = vec![
    vec![glyph(0, 0.0, 40.0, true), glyph(1, 40.0, 40.0, false)],
    vec![glyph(2, 0.0, 50.0, false)], // Last line
  ];

  let options = JustificationOptions::new().with_justify_last_line(false);

  let results = justify_lines(&mut lines, 100.0, &options);

  assert!(results[0].is_justified);
  assert!(!results[1].is_justified); // Last line not justified
}

#[test]
fn test_justify_lines_empty() {
  let mut lines: Vec<Vec<GlyphPosition>> = vec![];
  let options = JustificationOptions::new();

  let results = justify_lines(&mut lines, 100.0, &options);

  assert!(results.is_empty());
}

// ============================================================================
// Line Width Calculation Tests
// ============================================================================

#[test]
fn test_calculate_line_width_empty() {
  let glyphs: Vec<GlyphPosition> = vec![];
  assert_eq!(calculate_line_width(&glyphs), 0.0);
}

#[test]
fn test_calculate_line_width_single() {
  let glyphs = vec![glyph(0, 0.0, 50.0, false)];
  assert_eq!(calculate_line_width(&glyphs), 50.0);
}

#[test]
fn test_calculate_line_width_multiple() {
  let glyphs = vec![
    glyph(0, 0.0, 30.0, false),
    glyph(1, 30.0, 40.0, false),
    glyph(2, 70.0, 30.0, false),
  ];
  assert_eq!(calculate_line_width(&glyphs), 100.0);
}

// ============================================================================
// Word Boundary Detection Tests
// ============================================================================

#[test]
fn test_mark_word_boundaries_basic() {
  // Text: "Hi There" - positions 0='H', 1='i', 2=' ', 3='T', etc.
  let mut glyphs = vec![
    glyph_with_cluster(0, 0.0, 10.0, false, 0),  // H
    glyph_with_cluster(1, 10.0, 10.0, false, 1), // i
    glyph_with_cluster(2, 20.0, 5.0, false, 2),  // space
    glyph_with_cluster(3, 25.0, 10.0, false, 3), // T
    glyph_with_cluster(4, 35.0, 10.0, false, 4), // h
    glyph_with_cluster(5, 45.0, 10.0, false, 5), // e
    glyph_with_cluster(6, 55.0, 10.0, false, 6), // r
    glyph_with_cluster(7, 65.0, 10.0, false, 7), // e
  ];

  mark_word_boundaries(&mut glyphs, "Hi There");

  // 'i' at cluster 1 should be marked as word boundary (before space)
  assert!(!glyphs[0].is_word_boundary); // H
  assert!(glyphs[1].is_word_boundary); // i - ends word before space
  assert!(!glyphs[2].is_word_boundary); // space
  assert!(!glyphs[7].is_word_boundary); // e - last char
}

#[test]
fn test_mark_word_boundaries_empty() {
  let mut glyphs: Vec<GlyphPosition> = vec![];
  mark_word_boundaries(&mut glyphs, "");
  assert!(glyphs.is_empty());
}

#[test]
fn test_mark_word_boundaries_no_spaces() {
  let mut glyphs = vec![
    glyph_with_cluster(0, 0.0, 10.0, false, 0),
    glyph_with_cluster(1, 10.0, 10.0, false, 1),
  ];

  mark_word_boundaries(&mut glyphs, "AB");

  // No spaces, no word boundaries
  assert!(!glyphs[0].is_word_boundary);
  assert!(!glyphs[1].is_word_boundary);
}

#[test]
fn test_mark_word_boundaries_by_glyph_id() {
  // Space glyph has ID 32
  let mut glyphs = vec![
    glyph(1, 0.0, 10.0, false),  // 'A'
    glyph(32, 10.0, 5.0, false), // space
    glyph(2, 15.0, 10.0, false), // 'B'
  ];

  mark_word_boundaries_by_glyph_id(&mut glyphs, &[32]);

  // Glyph before space should be marked
  assert!(glyphs[0].is_word_boundary);
  assert!(!glyphs[1].is_word_boundary);
  assert!(!glyphs[2].is_word_boundary);
}

// ============================================================================
// CJK Detection Tests
// ============================================================================

#[test]
fn test_is_cjk_character_chinese() {
  assert!(is_cjk_character('中'));
  assert!(is_cjk_character('国'));
  assert!(is_cjk_character('文'));
}

#[test]
fn test_is_cjk_character_japanese() {
  assert!(is_cjk_character('あ')); // Hiragana
  assert!(is_cjk_character('カ')); // Katakana
  assert!(is_cjk_character('漢')); // Kanji
}

#[test]
fn test_is_cjk_character_korean() {
  assert!(is_cjk_character('한'));
  assert!(is_cjk_character('글'));
}

#[test]
fn test_is_cjk_character_non_cjk() {
  assert!(!is_cjk_character('A'));
  assert!(!is_cjk_character('z'));
  assert!(!is_cjk_character('1'));
  assert!(!is_cjk_character(' '));
  assert!(!is_cjk_character('é'));
}

// ============================================================================
// Justification Mode Detection Tests
// ============================================================================

#[test]
fn test_detect_justification_mode_latin() {
  assert_eq!(
    detect_justification_mode("Hello World"),
    JustificationMode::WordSpacing
  );
}

#[test]
fn test_detect_justification_mode_cjk() {
  assert_eq!(
    detect_justification_mode("你好世界"),
    JustificationMode::LetterSpacing
  );
}

#[test]
fn test_detect_justification_mode_mixed() {
  // Significant CJK presence but not majority
  assert_eq!(
    detect_justification_mode("Hello 你好"),
    JustificationMode::Combined
  );
}

#[test]
fn test_detect_justification_mode_empty() {
  assert_eq!(
    detect_justification_mode(""),
    JustificationMode::WordSpacing
  );
}

#[test]
fn test_detect_justification_mode_whitespace_only() {
  assert_eq!(
    detect_justification_mode("   \t\n  "),
    JustificationMode::WordSpacing
  );
}

#[test]
fn test_justification_mode_uses_word_spacing() {
  assert!(JustificationMode::WordSpacing.uses_word_spacing());
  assert!(JustificationMode::Combined.uses_word_spacing());
  assert!(JustificationMode::Auto.uses_word_spacing());
  assert!(!JustificationMode::LetterSpacing.uses_word_spacing());
}

#[test]
fn test_justification_mode_uses_letter_spacing() {
  assert!(JustificationMode::LetterSpacing.uses_letter_spacing());
  assert!(JustificationMode::Combined.uses_letter_spacing());
  assert!(JustificationMode::Auto.uses_letter_spacing());
  assert!(!JustificationMode::WordSpacing.uses_letter_spacing());
}

// ============================================================================
// Text Alignment Tests
// ============================================================================

#[test]
fn test_text_alignment_from_css() {
  assert_eq!(TextAlignment::from_css("left"), Some(TextAlignment::Left));
  assert_eq!(TextAlignment::from_css("right"), Some(TextAlignment::Right));
  assert_eq!(
    TextAlignment::from_css("center"),
    Some(TextAlignment::Center)
  );
  assert_eq!(
    TextAlignment::from_css("justify"),
    Some(TextAlignment::Justify)
  );
  assert_eq!(TextAlignment::from_css("start"), Some(TextAlignment::Left));
  assert_eq!(TextAlignment::from_css("end"), Some(TextAlignment::Right));
}

#[test]
fn test_text_alignment_from_css_case_insensitive() {
  assert_eq!(TextAlignment::from_css("LEFT"), Some(TextAlignment::Left));
  assert_eq!(
    TextAlignment::from_css("Center"),
    Some(TextAlignment::Center)
  );
  assert_eq!(
    TextAlignment::from_css("JUSTIFY"),
    Some(TextAlignment::Justify)
  );
}

#[test]
fn test_text_alignment_from_css_invalid() {
  assert_eq!(TextAlignment::from_css("invalid"), None);
  assert_eq!(TextAlignment::from_css(""), None);
}

#[test]
fn test_text_alignment_needs_justification() {
  assert!(TextAlignment::Justify.needs_justification());
  assert!(!TextAlignment::Left.needs_justification());
  assert!(!TextAlignment::Right.needs_justification());
  assert!(!TextAlignment::Center.needs_justification());
}

// ============================================================================
// Apply Alignment Tests
// ============================================================================

#[test]
fn test_apply_alignment_left() {
  let mut glyphs = vec![glyph(0, 0.0, 20.0, false), glyph(1, 20.0, 30.0, false)];

  let offset = apply_alignment(&mut glyphs, 50.0, 100.0, TextAlignment::Left);

  assert_eq!(offset, 0.0);
  assert_eq!(glyphs[0].x, 0.0);
  assert_eq!(glyphs[1].x, 20.0);
}

#[test]
fn test_apply_alignment_right() {
  let mut glyphs = vec![glyph(0, 0.0, 20.0, false), glyph(1, 20.0, 30.0, false)];

  let offset = apply_alignment(&mut glyphs, 50.0, 100.0, TextAlignment::Right);

  assert_eq!(offset, 50.0);
  assert_eq!(glyphs[0].x, 50.0);
  assert_eq!(glyphs[1].x, 70.0);
}

#[test]
fn test_apply_alignment_center() {
  let mut glyphs = vec![glyph(0, 0.0, 20.0, false), glyph(1, 20.0, 30.0, false)];

  let offset = apply_alignment(&mut glyphs, 50.0, 100.0, TextAlignment::Center);

  assert_eq!(offset, 25.0);
  assert_eq!(glyphs[0].x, 25.0);
  assert_eq!(glyphs[1].x, 45.0);
}

#[test]
fn test_apply_alignment_justify_same_as_left() {
  let mut glyphs = vec![glyph(0, 0.0, 20.0, false), glyph(1, 20.0, 30.0, false)];

  let offset = apply_alignment(&mut glyphs, 50.0, 100.0, TextAlignment::Justify);

  // Justify alignment positions same as left
  assert_eq!(offset, 0.0);
  assert_eq!(glyphs[0].x, 0.0);
  assert_eq!(glyphs[1].x, 20.0);
}

// ============================================================================
// JustificationResult Tests
// ============================================================================

#[test]
fn test_justification_result_default() {
  let result = JustificationResult::default();

  assert_eq!(result.extra_space_added, 0.0);
  assert_eq!(result.space_per_word, 0.0);
  assert_eq!(result.space_per_letter, 0.0);
  assert_eq!(result.word_boundaries_used, 0);
  assert_eq!(result.letter_spaces_used, 0);
  assert!(!result.is_justified);
}

#[test]
fn test_justification_result_contains_info() {
  // Line width 120px, target 150px (ratio 0.8 > 0.75)
  let mut glyphs = vec![
    glyph(0, 0.0, 40.0, true),
    glyph(1, 40.0, 40.0, true),
    glyph(2, 80.0, 40.0, false),
  ];

  let result =
    justify_line_with_options(&mut glyphs, 150.0, 120.0, &JustificationOptions::default());

  assert!(result.is_justified);
  assert_eq!(result.extra_space_added, 30.0);
  assert_eq!(result.word_boundaries_used, 2);
  assert_approx_eq(result.space_per_word, 15.0, "space per word");
}

// ============================================================================
// Edge Cases and Error Handling
// ============================================================================

#[test]
fn test_justify_very_small_extra_space() {
  let mut glyphs = vec![glyph(0, 0.0, 99.9, true), glyph(1, 99.9, 0.05, false)];

  justify_line(&mut glyphs, 100.0, 99.95);

  // Very small extra space (0.05) should still be applied
  assert!(glyphs[1].x >= 99.9);
}

#[test]
fn test_justify_many_word_boundaries() {
  // 10 short words: 100px total, target 125px (ratio 0.8 > 0.75)
  let mut glyphs: Vec<GlyphPosition> = (0..10)
    .map(|i| {
      glyph(
        i as u16,
        (i * 10) as f32,
        10.0,
        i < 9, // All but last have word boundary
      )
    })
    .collect();

  justify_line(&mut glyphs, 125.0, 100.0);

  // Should distribute 25px among 9 boundaries = ~2.78 each
  // Last glyph at original 90px + 25px total extra = 115px
  let last_idx = glyphs.len() - 1;
  assert_approx_eq(
    glyphs[last_idx].x,
    90.0 + 25.0,
    "Last glyph with many boundaries",
  );
}

#[test]
fn test_justify_preserves_relative_positions() {
  let mut glyphs = vec![
    glyph(0, 0.0, 30.0, true),
    glyph(1, 30.0, 20.0, false),
    glyph(2, 50.0, 20.0, true),
    glyph(3, 70.0, 30.0, false),
  ];

  justify_line(&mut glyphs, 150.0, 100.0);

  // Glyphs within words should maintain relative positions
  let word1_spacing = glyphs[1].x - glyphs[0].x;
  let word2_spacing = glyphs[3].x - glyphs[2].x;

  // Word internal spacing should remain consistent with original
  assert!(word1_spacing >= 30.0); // At least original advance
  assert!(word2_spacing >= 20.0);
}

// ============================================================================
// Integration Tests
// ============================================================================

#[test]
fn test_full_justification_workflow() {
  // Simulate a full text justification workflow
  // Line width 130px, target 160px (ratio 0.81 > 0.75)
  let text = "Hello World Test";

  // Create glyphs with cluster info
  let mut glyphs = vec![
    glyph_with_cluster(0, 0.0, 40.0, false, 0),    // Hello
    glyph_with_cluster(1, 40.0, 10.0, false, 5),   // space
    glyph_with_cluster(2, 50.0, 40.0, false, 6),   // World
    glyph_with_cluster(3, 90.0, 10.0, false, 11),  // space
    glyph_with_cluster(4, 100.0, 30.0, false, 12), // Test
  ];

  // Mark word boundaries
  mark_word_boundaries(&mut glyphs, text);

  // Justify
  let options = JustificationOptions::default();
  let result = justify_line_with_options(&mut glyphs, 160.0, 130.0, &options);

  assert!(result.is_justified);
}
