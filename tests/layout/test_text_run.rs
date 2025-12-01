//! Integration tests for text run generation
//!
//! Tests the text run generation module (W4.T14) with comprehensive coverage
//! for text shaping, inline items, and line metrics.

use fastrender::layout::contexts::inline::{GlyphInfo, InlineItem, LineMetrics, TextRun, TextRunBuilder};
use fastrender::text::font_db::{FontStyle, FontWeight};
use fastrender::font_loader::FontContext;
use fastrender::geometry::Point;

// ============================================================================
// GlyphInfo Integration Tests
// ============================================================================

#[test]
fn test_glyph_info_creation() {
    let glyph = GlyphInfo::new(72, 0, 10.5);

    assert_eq!(glyph.glyph_id, 72);
    assert_eq!(glyph.cluster, 0);
    assert_eq!(glyph.x_advance, 10.5);
    assert_eq!(glyph.y_advance, 0.0);
    assert_eq!(glyph.x_offset, 0.0);
    assert_eq!(glyph.y_offset, 0.0);
}

#[test]
fn test_glyph_info_with_full_positioning() {
    let glyph = GlyphInfo::with_offsets(72, 0, 10.5, 1.0, 0.5, -0.5);

    assert_eq!(glyph.x_advance, 10.5);
    assert_eq!(glyph.y_advance, 1.0);
    assert_eq!(glyph.x_offset, 0.5);
    assert_eq!(glyph.y_offset, -0.5);
}

#[test]
fn test_glyph_info_zero_width_detection() {
    let zero_width = GlyphInfo::new(72, 0, 0.0);
    let normal = GlyphInfo::new(72, 0, 10.0);

    assert!(zero_width.is_zero_width());
    assert!(!normal.is_zero_width());
}

// ============================================================================
// TextRun Integration Tests
// ============================================================================

#[test]
fn test_text_run_empty() {
    let run = TextRun::new();

    assert!(run.is_empty());
    assert_eq!(run.glyph_count(), 0);
    assert_eq!(run.char_count(), 0);
    assert_eq!(run.width, 0.0);
}

#[test]
fn test_text_run_with_glyphs() {
    let glyphs = vec![
        GlyphInfo::new(72, 0, 10.0),   // H
        GlyphInfo::new(101, 1, 8.0),   // e
        GlyphInfo::new(108, 2, 5.0),   // l
        GlyphInfo::new(108, 3, 5.0),   // l
        GlyphInfo::new(111, 4, 8.0),   // o
    ];

    let mut run = TextRun::new();
    run.text = "Hello".to_string();
    run.glyphs = glyphs;
    run.width = 36.0;
    run.height = 20.0;
    run.ascent = 14.0;
    run.descent = 6.0;
    run.line_height = 24.0;
    run.font_size = 16.0;
    run.font_family = "TestFont".to_string();

    assert!(!run.is_empty());
    assert_eq!(run.glyph_count(), 5);
    assert_eq!(run.char_count(), 5);
    assert_eq!(run.width, 36.0);
    assert_eq!(run.baseline_offset(), 14.0);
}

#[test]
fn test_text_run_half_leading() {
    let mut run = TextRun::new();
    run.height = 20.0;
    run.line_height = 28.0;

    // Half leading = (28 - 20) / 2 = 4
    assert_eq!(run.half_leading(), 4.0);
}

#[test]
fn test_text_run_bounds_calculation() {
    let mut run = TextRun::new();
    run.width = 50.0;
    run.height = 20.0;

    let bounds = run.bounds_at(Point::new(10.0, 20.0));

    assert_eq!(bounds.x(), 10.0);
    assert_eq!(bounds.y(), 20.0);
    assert_eq!(bounds.width(), 50.0);
    assert_eq!(bounds.height(), 20.0);
}

#[test]
fn test_text_run_split_valid() {
    let glyphs = vec![
        GlyphInfo::new(72, 0, 10.0),   // H
        GlyphInfo::new(101, 1, 8.0),   // e
        GlyphInfo::new(108, 2, 5.0),   // l
        GlyphInfo::new(108, 3, 5.0),   // l
        GlyphInfo::new(111, 4, 8.0),   // o
    ];

    let mut run = TextRun::new();
    run.text = "Hello".to_string();
    run.glyphs = glyphs;
    run.width = 36.0;
    run.ascent = 14.0;
    run.descent = 6.0;
    run.height = 20.0;
    run.line_height = 24.0;
    run.font_size = 16.0;
    run.font_family = "TestFont".to_string();

    // Split at position 2 ("He" | "llo")
    let (left, right) = run.split_at(2).expect("Split should succeed");

    assert_eq!(left.text, "He");
    assert_eq!(left.glyph_count(), 2);
    assert_eq!(left.width, 18.0); // 10 + 8

    assert_eq!(right.text, "llo");
    assert_eq!(right.glyph_count(), 3);
    assert_eq!(right.width, 18.0); // 5 + 5 + 8
}

#[test]
fn test_text_run_split_invalid_positions() {
    let glyphs = vec![
        GlyphInfo::new(72, 0, 10.0),
        GlyphInfo::new(101, 1, 8.0),
    ];

    let mut run = TextRun::new();
    run.text = "He".to_string();
    run.glyphs = glyphs;

    // Can't split at beginning
    assert!(run.split_at(0).is_none());

    // Can't split at end
    assert!(run.split_at(2).is_none());

    // Can't split past end
    assert!(run.split_at(10).is_none());
}

#[test]
fn test_text_run_x_for_char() {
    let glyphs = vec![
        GlyphInfo::new(72, 0, 10.0),
        GlyphInfo::new(101, 1, 8.0),
        GlyphInfo::new(108, 2, 5.0),
    ];

    let mut run = TextRun::new();
    run.text = "Hel".to_string();
    run.glyphs = glyphs;
    run.width = 23.0;

    assert_eq!(run.x_for_char(0), Some(0.0));
    assert_eq!(run.x_for_char(1), Some(10.0));
    assert_eq!(run.x_for_char(2), Some(18.0));
    assert_eq!(run.x_for_char(3), Some(23.0));
    assert!(run.x_for_char(10).is_none());
}

// ============================================================================
// InlineItem Integration Tests
// ============================================================================

#[test]
fn test_inline_item_text() {
    let mut run = TextRun::new();
    run.text = "Test".to_string();
    run.width = 30.0;
    run.height = 20.0;

    let item = InlineItem::text(run);

    assert!(item.is_text());
    assert!(!item.is_break_opportunity());
    assert!(!item.is_atomic());
    assert_eq!(item.width(), 30.0);
    assert_eq!(item.height(), 20.0);
    assert!(item.as_text().is_some());
}

#[test]
fn test_inline_item_start_inline_box() {
    let item = InlineItem::start_inline_box(Some(1), 5.0, 2.0, 10.0);

    assert!(!item.is_text());
    assert!(!item.is_break_opportunity());
    // Width = margin_left + border_left + padding_left = 5 + 2 + 10 = 17
    assert_eq!(item.width(), 17.0);
    assert_eq!(item.height(), 0.0);
}

#[test]
fn test_inline_item_end_inline_box() {
    let item = InlineItem::end_inline_box(Some(1), 10.0, 2.0, 5.0);

    // Width = padding_right + border_right + margin_right = 10 + 2 + 5 = 17
    assert_eq!(item.width(), 17.0);
}

#[test]
fn test_inline_item_atomic() {
    let item = InlineItem::atomic(Some(1), 100.0, 50.0, 40.0, 5.0, 5.0);

    assert!(item.is_atomic());
    assert!(!item.is_text());
    // Width = margin_left + width + margin_right = 5 + 100 + 5 = 110
    assert_eq!(item.width(), 110.0);
    assert_eq!(item.height(), 50.0);
}

#[test]
fn test_inline_item_soft_break() {
    let item = InlineItem::soft_break();

    assert!(item.is_break_opportunity());
    assert!(!item.is_hard_break());
    assert!(!item.is_text());
    assert_eq!(item.width(), 0.0);
    assert_eq!(item.height(), 0.0);
}

#[test]
fn test_inline_item_hard_break() {
    let item = InlineItem::hard_break();

    assert!(item.is_break_opportunity());
    assert!(item.is_hard_break());
    assert_eq!(item.width(), 0.0);
}

// ============================================================================
// LineMetrics Integration Tests
// ============================================================================

#[test]
fn test_line_metrics_empty() {
    let metrics = LineMetrics::new();

    assert_eq!(metrics.max_ascent, 0.0);
    assert_eq!(metrics.max_descent, 0.0);
    assert_eq!(metrics.max_line_height, 0.0);
    assert_eq!(metrics.total_width, 0.0);
    assert_eq!(metrics.line_height(), 0.0);
    assert_eq!(metrics.baseline(), 0.0);
}

#[test]
fn test_line_metrics_with_text_run() {
    let mut metrics = LineMetrics::new();

    let mut run = TextRun::new();
    run.width = 50.0;
    run.ascent = 14.0;
    run.descent = 6.0;
    run.line_height = 24.0;

    metrics.add_text_run(&run);

    assert_eq!(metrics.max_ascent, 14.0);
    assert_eq!(metrics.max_descent, 6.0);
    assert_eq!(metrics.max_line_height, 24.0);
    assert_eq!(metrics.total_width, 50.0);
}

#[test]
fn test_line_metrics_multiple_runs() {
    let mut metrics = LineMetrics::new();

    // First run: smaller
    let mut run1 = TextRun::new();
    run1.width = 50.0;
    run1.ascent = 14.0;
    run1.descent = 6.0;
    run1.line_height = 24.0;

    // Second run: larger ascent and line-height
    let mut run2 = TextRun::new();
    run2.width = 30.0;
    run2.ascent = 20.0;
    run2.descent = 4.0;
    run2.line_height = 28.0;

    metrics.add_text_run(&run1);
    metrics.add_text_run(&run2);

    // Should use max values
    assert_eq!(metrics.max_ascent, 20.0);
    assert_eq!(metrics.max_descent, 6.0);  // From run1
    assert_eq!(metrics.max_line_height, 28.0);
    assert_eq!(metrics.total_width, 80.0);  // 50 + 30
}

#[test]
fn test_line_metrics_with_atomic() {
    let mut metrics = LineMetrics::new();

    // Atomic box: 100x50 with baseline at 40
    // Ascent = 40, Descent = 50 - 40 = 10
    metrics.add_atomic(50.0, 40.0, 100.0);

    assert_eq!(metrics.max_ascent, 40.0);
    assert_eq!(metrics.max_descent, 10.0);
    assert_eq!(metrics.max_line_height, 50.0);
    assert_eq!(metrics.total_width, 100.0);
}

#[test]
fn test_line_metrics_line_height_calculation() {
    let mut metrics = LineMetrics::new();

    // Add run with small natural height but large line-height
    let mut run = TextRun::new();
    run.ascent = 10.0;
    run.descent = 5.0;
    run.line_height = 30.0;

    metrics.add_text_run(&run);

    // Natural height = 15, line-height = 30
    // Line height should be max(15, 30) = 30
    assert_eq!(metrics.line_height(), 30.0);
}

#[test]
fn test_line_metrics_baseline_with_leading() {
    let mut metrics = LineMetrics::new();

    let mut run = TextRun::new();
    run.ascent = 14.0;
    run.descent = 6.0;
    run.line_height = 24.0;

    metrics.add_text_run(&run);

    // Natural height = 20, line height = 24
    // Half leading = (24 - 20) / 2 = 2
    // Baseline = 2 + 14 = 16
    assert_eq!(metrics.baseline(), 16.0);
}

// ============================================================================
// TextRunBuilder Integration Tests (require system fonts)
// ============================================================================

#[test]
fn test_text_run_builder_empty_text() {
    let font_ctx = FontContext::new();
    let builder = TextRunBuilder::new(&font_ctx);

    let run = builder
        .build("", &["sans-serif".to_string()], 400, false, 16.0)
        .expect("Empty text should succeed");

    assert!(run.is_empty());
}

#[test]
fn test_text_run_builder_basic_shaping() {
    let font_ctx = FontContext::new();

    // Skip if no fonts available (CI environment)
    if !font_ctx.has_fonts() {
        return;
    }

    let builder = TextRunBuilder::new(&font_ctx);

    let run = builder
        .build("Hello", &["sans-serif".to_string()], 400, false, 16.0)
        .expect("Shaping should succeed");

    assert!(!run.is_empty());
    assert_eq!(run.text, "Hello");
    assert!(run.width > 0.0);
    assert!(run.height > 0.0);
    assert!(run.ascent > 0.0);
    assert!(run.descent > 0.0);
    assert!(run.glyph_count() > 0);
}

#[test]
fn test_text_run_builder_different_fonts() {
    let font_ctx = FontContext::new();

    if !font_ctx.has_fonts() {
        return;
    }

    let builder = TextRunBuilder::new(&font_ctx);

    // Test with different generic families
    let families = vec!["serif".to_string()];
    let run_serif = builder.build("Test", &families, 400, false, 16.0);

    let families = vec!["monospace".to_string()];
    let run_mono = builder.build("Test", &families, 400, false, 16.0);

    // Both should succeed if fonts are available
    if let (Ok(serif), Ok(mono)) = (run_serif, run_mono) {
        // Monospace fonts should have consistent glyph widths
        // Serif fonts may have variable widths
        assert!(serif.width > 0.0);
        assert!(mono.width > 0.0);
    }
}

#[test]
fn test_text_run_builder_font_size_scaling() {
    let font_ctx = FontContext::new();

    if !font_ctx.has_fonts() {
        return;
    }

    let builder = TextRunBuilder::new(&font_ctx);
    let families = vec!["sans-serif".to_string()];

    let run_16 = builder
        .build("Test", &families, 400, false, 16.0)
        .expect("16px shaping should succeed");

    let run_32 = builder
        .build("Test", &families, 400, false, 32.0)
        .expect("32px shaping should succeed");

    // Double font size should roughly double dimensions
    assert!(run_32.width > run_16.width * 1.8);
    assert!(run_32.width < run_16.width * 2.2);
    assert!(run_32.height > run_16.height * 1.8);
    assert!(run_32.height < run_16.height * 2.2);
}

#[test]
fn test_text_run_builder_font_weights() {
    let font_ctx = FontContext::new();

    if !font_ctx.has_fonts() {
        return;
    }

    let builder = TextRunBuilder::new(&font_ctx);
    let families = vec!["sans-serif".to_string()];

    let run_normal = builder.build("Test", &families, 400, false, 16.0);
    let run_bold = builder.build("Test", &families, 700, false, 16.0);

    // Both should succeed
    assert!(run_normal.is_ok());
    assert!(run_bold.is_ok());
}

#[test]
fn test_text_run_builder_italic() {
    let font_ctx = FontContext::new();

    if !font_ctx.has_fonts() {
        return;
    }

    let builder = TextRunBuilder::new(&font_ctx);
    let families = vec!["sans-serif".to_string()];

    let run_normal = builder.build("Test", &families, 400, false, 16.0);
    let run_italic = builder.build("Test", &families, 400, true, 16.0);

    // Both should succeed
    assert!(run_normal.is_ok());
    assert!(run_italic.is_ok());
}

#[test]
fn test_text_run_builder_with_breaks() {
    let font_ctx = FontContext::new();

    if !font_ctx.has_fonts() {
        return;
    }

    let builder = TextRunBuilder::new(&font_ctx);
    let families = vec!["sans-serif".to_string()];

    let items = builder
        .build_with_breaks("Hello world", &families, 400, false, 16.0)
        .expect("Shaping with breaks should succeed");

    // Should have text items and break opportunities
    let text_count = items.iter().filter(|i| i.is_text()).count();
    let break_count = items.iter().filter(|i| i.is_break_opportunity()).count();

    // At least "Hello" and "world" as text runs
    assert!(text_count >= 2);
    // At least one break opportunity between words
    assert!(break_count >= 1);
}

#[test]
fn test_text_run_builder_unicode() {
    let font_ctx = FontContext::new();

    if !font_ctx.has_fonts() {
        return;
    }

    let builder = TextRunBuilder::new(&font_ctx);
    let families = vec!["sans-serif".to_string()];

    // Test with accented characters
    let run = builder.build("Caf\u{00E9}", &families, 400, false, 16.0);
    assert!(run.is_ok());

    // Test with various Unicode characters
    let run = builder.build("H\u{00E9}llo", &families, 400, false, 16.0);
    assert!(run.is_ok());
}

#[test]
fn test_text_run_builder_whitespace() {
    let font_ctx = FontContext::new();

    if !font_ctx.has_fonts() {
        return;
    }

    let builder = TextRunBuilder::new(&font_ctx);
    let families = vec!["sans-serif".to_string()];

    // Text with spaces should be wider than without
    let without_space = builder
        .build("AB", &families, 400, false, 16.0)
        .expect("Shaping should succeed");

    let with_space = builder
        .build("A B", &families, 400, false, 16.0)
        .expect("Shaping should succeed");

    assert!(with_space.width > without_space.width);
}

// ============================================================================
// Complex Scenario Tests
// ============================================================================

#[test]
fn test_mixed_inline_items() {
    // Simulate a line with text, inline box, and atomic content
    let mut items: Vec<InlineItem> = Vec::new();

    // Start inline box (span)
    items.push(InlineItem::start_inline_box(Some(1), 0.0, 1.0, 5.0));

    // Text inside span
    let mut run = TextRun::new();
    run.text = "Hello".to_string();
    run.width = 40.0;
    run.height = 20.0;
    items.push(InlineItem::text(run));

    // End inline box
    items.push(InlineItem::end_inline_box(Some(1), 5.0, 1.0, 0.0));

    // Space (break opportunity)
    items.push(InlineItem::soft_break());

    // Atomic inline-block
    items.push(InlineItem::atomic(Some(2), 50.0, 30.0, 25.0, 5.0, 5.0));

    // Calculate total width
    let total_width: f32 = items.iter().map(|i| i.width()).sum();

    // Start: 1 + 5 = 6
    // Text: 40
    // End: 5 + 1 = 6
    // Break: 0
    // Atomic: 5 + 50 + 5 = 60
    // Total = 6 + 40 + 6 + 0 + 60 = 112
    assert_eq!(total_width, 112.0);
}

#[test]
fn test_line_metrics_mixed_content() {
    let mut metrics = LineMetrics::new();

    // Add text run
    let mut run = TextRun::new();
    run.width = 100.0;
    run.ascent = 14.0;
    run.descent = 6.0;
    run.line_height = 24.0;
    metrics.add_text_run(&run);

    // Add atomic box (taller than text)
    metrics.add_atomic(40.0, 35.0, 50.0);

    // Max ascent should be from atomic (35)
    assert_eq!(metrics.max_ascent, 35.0);
    // Max descent should be from text (6) vs atomic (40-35=5)
    assert_eq!(metrics.max_descent, 6.0);
    // Total width
    assert_eq!(metrics.total_width, 150.0);
}

#[test]
fn test_inline_item_as_text_mut() {
    let mut run = TextRun::new();
    run.text = "Original".to_string();
    run.width = 50.0;

    let mut item = InlineItem::text(run);

    // Modify through mutable reference
    if let Some(text_run) = item.as_text_mut() {
        text_run.width = 100.0;
    }

    // Verify modification
    assert_eq!(item.width(), 100.0);
}
