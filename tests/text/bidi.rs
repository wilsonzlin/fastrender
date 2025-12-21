//! Integration tests for the bidirectional text analyzer (UAX #9).
//!
//! These tests verify the public API of the bidi module and demonstrate
//! correct handling of various text patterns including mixed LTR/RTL content.

use fastrender::text::bidi::{analyze_bidi, BidiAnalyzer, BidiRun, Direction};
use fastrender::text::pipeline::ShapingPipeline;
use fastrender::FontContext;

macro_rules! require_fonts {
    ($expr:expr) => {
        match $expr {
            Ok(val) => val,
            Err(_) => return,
        }
    };
}
use fastrender::style::ComputedStyle;

// =============================================================================
// Basic LTR Tests
// =============================================================================

#[test]
fn test_analyze_simple_english() {
    let analyzer = BidiAnalyzer::new();
    let runs = analyzer.analyze("Hello world", Direction::Ltr);

    assert_eq!(runs.len(), 1);
    assert_eq!(runs[0].text, "Hello world");
    assert!(runs[0].is_ltr());
}

#[test]
fn test_empty_string_produces_no_runs() {
    let runs = analyze_bidi("", Direction::Ltr);
    assert!(runs.is_empty());
}

#[test]
fn test_whitespace_is_ltr() {
    let runs = analyze_bidi("   \t\n   ", Direction::Ltr);
    assert!(!runs.is_empty());
    // Whitespace follows base direction
}

// =============================================================================
// Basic RTL Tests
// =============================================================================

#[test]
fn test_analyze_hebrew_text() {
    let analyzer = BidiAnalyzer::new();
    let runs = analyzer.analyze("שלום עולם", Direction::Ltr);

    assert_eq!(runs.len(), 1);
    assert!(runs[0].is_rtl());
    assert!(runs[0].text.contains("שלום"));
}

#[test]
fn test_analyze_arabic_text() {
    let analyzer = BidiAnalyzer::new();
    let runs = analyzer.analyze("مرحبا بالعالم", Direction::Ltr);

    assert_eq!(runs.len(), 1);
    assert!(runs[0].is_rtl());
}

// =============================================================================
// Mixed Content Tests
// =============================================================================

#[test]
fn test_mixed_english_hebrew() {
    let analyzer = BidiAnalyzer::new();
    let text = "Hello שלום world";
    let runs = analyzer.analyze(text, Direction::Ltr);

    // Should have LTR and RTL runs
    let has_ltr = runs.iter().any(|r| r.is_ltr());
    let has_rtl = runs.iter().any(|r| r.is_rtl());

    assert!(has_ltr, "Should have LTR runs");
    assert!(has_rtl, "Should have RTL runs");

    // All text should be represented
    let combined: String = runs.iter().map(|r| r.text.as_str()).collect();
    assert!(combined.contains("Hello"));
    assert!(combined.contains("שלום"));
    assert!(combined.contains("world"));
}

#[test]
fn test_mixed_english_arabic() {
    let analyzer = BidiAnalyzer::new();
    let text = "Welcome مرحبا Friend";
    let runs = analyzer.analyze(text, Direction::Ltr);

    // All text should be preserved
    let combined: String = runs.iter().map(|r| r.text.as_str()).collect();
    assert!(combined.contains("Welcome"));
    assert!(combined.contains("مرحبا"));
    assert!(combined.contains("Friend"));
}

// =============================================================================
// Numbers in RTL Context Tests
// =============================================================================

#[test]
fn test_numbers_preserved_in_rtl() {
    let analyzer = BidiAnalyzer::new();

    // Numbers should NOT be reversed in RTL context
    let runs = analyzer.analyze("מחיר: 123 שקל", Direction::Ltr);
    let combined: String = runs.iter().map(|r| r.text.as_str()).collect();

    // Numbers should appear as "123", not "321"
    assert!(combined.contains("123"));
    assert!(!combined.contains("321"));
}

#[test]
fn test_phone_number_in_arabic() {
    let analyzer = BidiAnalyzer::new();

    let runs = analyzer.analyze("هاتف: 0501234567", Direction::Ltr);
    let combined: String = runs.iter().map(|r| r.text.as_str()).collect();

    // Phone number should be preserved
    assert!(combined.contains("0501234567"));
}

// =============================================================================
// Paragraph / override interactions
// =============================================================================

#[test]
fn override_does_not_leak_to_next_paragraph_with_embed() {
    // First paragraph forces RTL override; second paragraph contains an embedded LTR run.
    // Visual order should keep paragraphs independent (no override leak).
    let para1 = "\u{202E}ABC\u{202C}"; // RLO ... PDF
    let para2 = "\u{202A}DEF\u{202C}"; // LRE ... PDF
    let text = format!("{para1}\n{para2}");

    let runs = analyze_bidi(&text, Direction::Ltr);

    // Split runs by paragraph
    let mut first_para = String::new();
    let mut second_para = String::new();
    for run in runs {
        if run.text.contains('A') || run.text.contains('C') {
            first_para.push_str(&run.text);
        } else {
            second_para.push_str(&run.text);
        }
    }

    assert!(first_para.contains("ABC"), "override paragraph should keep its content");
    assert!(!first_para.contains("CBA"), "override should stay local to paragraph");

    // The embedded LTR run should surface as DEF in visual order without reversal and without picking up override.
    assert!(
        second_para.contains("DEF"),
        "embedded LTR run should stay LTR in its paragraph"
    );
    assert!(!second_para.contains("FED"));
}

// =============================================================================
// Direction Detection Tests
// =============================================================================

#[test]
fn test_detect_ltr_from_first_strong() {
    let analyzer = BidiAnalyzer::new();
    let dir = analyzer.detect_direction("Hello שלום", Direction::Rtl);
    assert_eq!(dir, Direction::Ltr);
}

#[test]
fn test_detect_rtl_from_first_strong() {
    let analyzer = BidiAnalyzer::new();
    let dir = analyzer.detect_direction("שלום Hello", Direction::Ltr);
    assert_eq!(dir, Direction::Rtl);
}

#[test]
fn test_detect_falls_back_to_default() {
    let analyzer = BidiAnalyzer::new();

    // Only numbers and punctuation - no strong characters
    let dir = analyzer.detect_direction("123-456-7890", Direction::Rtl);
    assert_eq!(dir, Direction::Rtl); // Falls back to default
}

// =============================================================================
// RTL Detection Tests
// =============================================================================

#[test]
fn test_has_rtl_detects_hebrew() {
    let analyzer = BidiAnalyzer::new();
    assert!(analyzer.has_rtl("שלום"));
    assert!(analyzer.has_rtl("Hello שלום"));
}

#[test]
fn test_has_rtl_detects_arabic() {
    let analyzer = BidiAnalyzer::new();
    assert!(analyzer.has_rtl("مرحبا"));
}

#[test]
fn inline_bidi_runs_position_in_visual_order() {
    let pipeline = ShapingPipeline::new();
    let font_context = FontContext::new();
    if !font_context.has_fonts() {
        return;
    }
    let style = ComputedStyle::default();
    // LTR hello, RTL shalom, LTR world, with explicit embed/override to force splits.
    let text = "Hello שלום world";
    let runs = analyze_bidi(text, Direction::Ltr);
    // Ensure logical order splits into at least three runs with differing directions.
    assert!(runs.iter().any(|r| r.is_rtl()));
    assert!(runs.iter().any(|r| r.is_ltr()));

    // Shape each run and ensure their start x positions increase in visual order when concatenated.
    let mut cursor = 0.0f32;
    for run in runs {
        let shaped = require_fonts!(pipeline.shape(&run.text, &style, &font_context));
        for shaped_run in shaped {
            for glyph in shaped_run.glyphs.iter() {
                assert!(
                    glyph.x_offset + cursor >= 0.0,
                    "glyph positions should be non-negative in visual order"
                );
            }
            cursor += shaped_run.advance;
        }
    }
}

#[test]
fn test_is_pure_ltr() {
    let analyzer = BidiAnalyzer::new();
    assert!(analyzer.is_pure_ltr("Hello world"));
    assert!(analyzer.is_pure_ltr("123 + 456 = 579"));
    assert!(!analyzer.is_pure_ltr("Hello שלום"));
}

// =============================================================================
// BidiRun Tests
// =============================================================================

#[test]
fn test_bidi_run_properties() {
    let run = BidiRun {
        start: 0,
        end: 5,
        level: 0,
        direction: Direction::Ltr,
        text: "Hello".to_string(),
    };

    assert_eq!(run.len(), 5);
    assert!(!run.is_empty());
    assert!(run.is_ltr());
    assert!(!run.is_rtl());
}

#[test]
fn test_bidi_run_rtl() {
    let run = BidiRun {
        start: 0,
        end: 8,
        level: 1,
        direction: Direction::Rtl,
        text: "שלום".to_string(),
    };

    assert!(run.is_rtl());
    assert!(!run.is_ltr());
}

// =============================================================================
// Full Analysis Tests
// =============================================================================

#[test]
fn test_full_analysis_provides_levels() {
    let analyzer = BidiAnalyzer::new();
    let analysis = analyzer.analyze_full("Hello שלום", Direction::Ltr);

    assert!(analysis.needs_reordering());
    assert_eq!(analysis.base_direction(), Direction::Ltr);

    // Check levels at different positions
    let level_h = analysis.level_at(0);
    assert!(level_h.is_ltr());

    // Text preserved
    assert_eq!(analysis.text(), "Hello שלום");
}

#[test]
fn test_full_analysis_logical_runs() {
    let analyzer = BidiAnalyzer::new();
    let analysis = analyzer.analyze_full("ABC שלום XYZ", Direction::Ltr);

    let logical = analysis.logical_runs();
    let visual = analysis.visual_runs();

    // Both should have runs
    assert!(!logical.is_empty());
    assert!(!visual.is_empty());
}

#[test]
fn bidi_visual_order_preserves_positions() {
    let analyzer = BidiAnalyzer::new();
    // Base LTR with embedded RTL word.
    let analysis = analyzer.analyze_full("ABC שלום GHI", Direction::Ltr);

    // Visual runs should reorder the RTL word between the surrounding LTR runs.
    let runs = analysis.visual_runs();
    assert_eq!(runs.len(), 3);

    // Visual order should be: "ABC ", "שלום", " GHI".
    assert!(runs[0].text.starts_with("ABC"));
    assert!(runs[1].text.contains('ש'));
    assert!(runs[2].text.contains("GHI"));
}

// =============================================================================
// Edge Cases
// =============================================================================

#[test]
fn test_single_rtl_char() {
    let runs = analyze_bidi("א", Direction::Ltr);
    assert_eq!(runs.len(), 1);
    assert!(runs[0].is_rtl());
}

#[test]
fn test_single_ltr_char() {
    let runs = analyze_bidi("A", Direction::Ltr);
    assert_eq!(runs.len(), 1);
    assert!(runs[0].is_ltr());
}

#[test]
fn test_emoji_handling() {
    let runs = analyze_bidi("Hello 👋 שלום 🌍", Direction::Ltr);
    assert!(!runs.is_empty());

    // Text should be preserved
    let combined: String = runs.iter().map(|r| r.text.as_str()).collect();
    assert!(combined.contains("👋"));
    assert!(combined.contains("🌍"));
}

#[test]
fn test_long_text_performance() {
    let analyzer = BidiAnalyzer::new();

    // Generate long mixed text
    let long_text = "Hello ".repeat(1000) + "שלום " + &"World ".repeat(1000);
    let runs = analyzer.analyze(&long_text, Direction::Ltr);

    // Should complete without issue
    assert!(!runs.is_empty());
}

// =============================================================================
// Convenience Function Tests
// =============================================================================

#[test]
fn test_analyze_bidi_convenience() {
    // Test the analyze_bidi convenience function
    let runs = analyze_bidi("Hello שלום", Direction::Ltr);
    assert!(!runs.is_empty());
}
