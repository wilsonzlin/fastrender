//! Comprehensive tests for text shaper (W4.T03)
//!
//! Tests cover:
//! - Latin text shaping
//! - Arabic/Hebrew (complex shaping, RTL)
//! - Ligatures
//! - Kerning
//! - Combining marks
//! - RTL text
//! - Script detection
//! - Cluster tracking
//! - Hit testing

use fastrender::text::shaper::{Direction, GlyphCluster, GlyphPosition, Script, ShapedGlyphs, TextShaper};
use fastrender::text::FontContext;

// ============================================================================
// Helper functions
// ============================================================================

fn get_test_font() -> Option<fastrender::text::LoadedFont> {
    let ctx = FontContext::new();
    if !ctx.has_fonts() {
        return None;
    }
    ctx.get_sans_serif()
}

fn get_shaper_and_font() -> Option<(TextShaper, fastrender::text::LoadedFont)> {
    let font = get_test_font()?;
    Some((TextShaper::new(), font))
}

// ============================================================================
// Direction tests
// ============================================================================

#[test]
fn test_direction_default_is_ltr() {
    assert_eq!(Direction::default(), Direction::LeftToRight);
}

#[test]
fn test_direction_horizontal_detection() {
    assert!(Direction::LeftToRight.is_horizontal());
    assert!(Direction::RightToLeft.is_horizontal());
    assert!(!Direction::TopToBottom.is_horizontal());
    assert!(!Direction::BottomToTop.is_horizontal());
}

#[test]
fn test_direction_vertical_detection() {
    assert!(!Direction::LeftToRight.is_vertical());
    assert!(!Direction::RightToLeft.is_vertical());
    assert!(Direction::TopToBottom.is_vertical());
    assert!(Direction::BottomToTop.is_vertical());
}

#[test]
fn test_direction_rtl_detection() {
    assert!(!Direction::LeftToRight.is_rtl());
    assert!(Direction::RightToLeft.is_rtl());
    assert!(!Direction::TopToBottom.is_rtl());
}

// ============================================================================
// Script detection tests
// ============================================================================

#[test]
fn test_script_detect_latin_basic() {
    assert_eq!(Script::detect('A'), Script::Latin);
    assert_eq!(Script::detect('Z'), Script::Latin);
    assert_eq!(Script::detect('a'), Script::Latin);
    assert_eq!(Script::detect('z'), Script::Latin);
}

#[test]
fn test_script_detect_latin_accented() {
    assert_eq!(Script::detect('é'), Script::Latin);
    assert_eq!(Script::detect('ñ'), Script::Latin);
    assert_eq!(Script::detect('ü'), Script::Latin);
    assert_eq!(Script::detect('ç'), Script::Latin);
}

#[test]
fn test_script_detect_arabic() {
    assert_eq!(Script::detect('م'), Script::Arabic);
    assert_eq!(Script::detect('ر'), Script::Arabic);
    assert_eq!(Script::detect('ح'), Script::Arabic);
    assert_eq!(Script::detect('ب'), Script::Arabic);
    assert_eq!(Script::detect('ا'), Script::Arabic);
}

#[test]
fn test_script_detect_hebrew() {
    assert_eq!(Script::detect('א'), Script::Hebrew);
    assert_eq!(Script::detect('ב'), Script::Hebrew);
    assert_eq!(Script::detect('ג'), Script::Hebrew);
    assert_eq!(Script::detect('ש'), Script::Hebrew);
    assert_eq!(Script::detect('ל'), Script::Hebrew);
}

#[test]
fn test_script_detect_cyrillic() {
    assert_eq!(Script::detect('А'), Script::Cyrillic);
    assert_eq!(Script::detect('Б'), Script::Cyrillic);
    assert_eq!(Script::detect('В'), Script::Cyrillic);
    assert_eq!(Script::detect('Я'), Script::Cyrillic);
}

#[test]
fn test_script_detect_greek() {
    assert_eq!(Script::detect('α'), Script::Greek);
    assert_eq!(Script::detect('β'), Script::Greek);
    assert_eq!(Script::detect('Ω'), Script::Greek);
}

#[test]
fn test_script_detect_devanagari() {
    assert_eq!(Script::detect('न'), Script::Devanagari);
    assert_eq!(Script::detect('म'), Script::Devanagari);
    assert_eq!(Script::detect('स'), Script::Devanagari);
}

#[test]
fn test_script_detect_cjk() {
    assert_eq!(Script::detect('中'), Script::Han);
    assert_eq!(Script::detect('国'), Script::Han);
    assert_eq!(Script::detect('日'), Script::Han);
}

#[test]
fn test_script_detect_japanese() {
    assert_eq!(Script::detect('あ'), Script::Hiragana);
    assert_eq!(Script::detect('い'), Script::Hiragana);
    assert_eq!(Script::detect('ア'), Script::Katakana);
    assert_eq!(Script::detect('イ'), Script::Katakana);
}

#[test]
fn test_script_detect_korean() {
    assert_eq!(Script::detect('한'), Script::Hangul);
    assert_eq!(Script::detect('글'), Script::Hangul);
}

#[test]
fn test_script_detect_common() {
    assert_eq!(Script::detect('0'), Script::Common);
    assert_eq!(Script::detect('9'), Script::Common);
    assert_eq!(Script::detect('.'), Script::Common);
    assert_eq!(Script::detect(','), Script::Common);
    assert_eq!(Script::detect(' '), Script::Common);
}

#[test]
fn test_script_detect_text_latin() {
    assert_eq!(Script::detect_text("Hello, world!"), Script::Latin);
    assert_eq!(Script::detect_text("The quick brown fox"), Script::Latin);
}

#[test]
fn test_script_detect_text_arabic() {
    assert_eq!(Script::detect_text("مرحبا بالعالم"), Script::Arabic);
}

#[test]
fn test_script_detect_text_hebrew() {
    assert_eq!(Script::detect_text("שלום עולם"), Script::Hebrew);
}

#[test]
fn test_script_detect_text_cyrillic() {
    assert_eq!(Script::detect_text("Привет мир"), Script::Cyrillic);
}

#[test]
fn test_script_detect_text_mixed_defaults_to_most_common() {
    // Mixed text should return the most common script
    let mixed = "Hello Привет";
    let script = Script::detect_text(mixed);
    // Either Latin or Cyrillic is acceptable depending on implementation
    assert!(script == Script::Latin || script == Script::Cyrillic);
}

#[test]
fn test_script_default_direction() {
    assert_eq!(Script::Latin.default_direction(), Direction::LeftToRight);
    assert_eq!(Script::Arabic.default_direction(), Direction::RightToLeft);
    assert_eq!(Script::Hebrew.default_direction(), Direction::RightToLeft);
    assert_eq!(Script::Cyrillic.default_direction(), Direction::LeftToRight);
    assert_eq!(Script::Han.default_direction(), Direction::LeftToRight);
}

// ============================================================================
// GlyphPosition tests
// ============================================================================

#[test]
fn test_glyph_position_new() {
    let glyph = GlyphPosition::new(42, 5);
    assert_eq!(glyph.glyph_id, 42);
    assert_eq!(glyph.cluster, 5);
    assert_eq!(glyph.advance, 0.0);
    assert_eq!(glyph.advance_y, 0.0);
    assert_eq!(glyph.offset_x, 0.0);
    assert_eq!(glyph.offset_y, 0.0);
}

#[test]
fn test_glyph_position_with_metrics() {
    let glyph = GlyphPosition::with_metrics(100, 10, 8.5, 0.0, 0.5, -1.0);
    assert_eq!(glyph.glyph_id, 100);
    assert_eq!(glyph.cluster, 10);
    assert_eq!(glyph.advance, 8.5);
    assert_eq!(glyph.advance_y, 0.0);
    assert_eq!(glyph.offset_x, 0.5);
    assert_eq!(glyph.offset_y, -1.0);
}

#[test]
fn test_glyph_position_clone() {
    let glyph = GlyphPosition::with_metrics(42, 5, 10.0, 0.0, 0.0, 0.0);
    let cloned = glyph;
    assert_eq!(glyph, cloned);
}

// ============================================================================
// GlyphCluster tests
// ============================================================================

#[test]
fn test_glyph_cluster_new() {
    let cluster = GlyphCluster::new(0, 5, 0, 3, 25.0);
    assert_eq!(cluster.text_start, 0);
    assert_eq!(cluster.text_len, 5);
    assert_eq!(cluster.glyph_start, 0);
    assert_eq!(cluster.glyph_count, 3);
    assert_eq!(cluster.advance, 25.0);
}

#[test]
fn test_glyph_cluster_contains_text_pos_at_start() {
    let cluster = GlyphCluster::new(5, 3, 0, 1, 10.0);
    assert!(cluster.contains_text_pos(5));
}

#[test]
fn test_glyph_cluster_contains_text_pos_in_middle() {
    let cluster = GlyphCluster::new(5, 3, 0, 1, 10.0);
    assert!(cluster.contains_text_pos(6));
    assert!(cluster.contains_text_pos(7));
}

#[test]
fn test_glyph_cluster_contains_text_pos_at_end() {
    let cluster = GlyphCluster::new(5, 3, 0, 1, 10.0);
    // End is exclusive
    assert!(!cluster.contains_text_pos(8));
}

#[test]
fn test_glyph_cluster_contains_text_pos_before() {
    let cluster = GlyphCluster::new(5, 3, 0, 1, 10.0);
    assert!(!cluster.contains_text_pos(4));
}

// ============================================================================
// ShapedGlyphs tests
// ============================================================================

#[test]
fn test_shaped_glyphs_empty() {
    let shaped = ShapedGlyphs::empty();
    assert!(shaped.is_empty());
    assert_eq!(shaped.glyph_count(), 0);
    assert_eq!(shaped.total_advance, 0.0);
    assert!(shaped.text.is_empty());
}

#[test]
fn test_shaped_glyphs_cluster_at_text_pos() {
    let shaped = ShapedGlyphs {
        text: "abc".to_string(),
        glyphs: vec![
            GlyphPosition::with_metrics(1, 0, 10.0, 0.0, 0.0, 0.0),
            GlyphPosition::with_metrics(2, 1, 10.0, 0.0, 0.0, 0.0),
            GlyphPosition::with_metrics(3, 2, 10.0, 0.0, 0.0, 0.0),
        ],
        clusters: vec![
            GlyphCluster::new(0, 1, 0, 1, 10.0),
            GlyphCluster::new(1, 1, 1, 1, 10.0),
            GlyphCluster::new(2, 1, 2, 1, 10.0),
        ],
        total_advance: 30.0,
        total_advance_y: 0.0,
        direction: Direction::LeftToRight,
        script: Script::Latin,
        font_size: 16.0,
    };

    let cluster = shaped.cluster_at_text_pos(0).unwrap();
    assert_eq!(cluster.text_start, 0);

    let cluster = shaped.cluster_at_text_pos(1).unwrap();
    assert_eq!(cluster.text_start, 1);

    let cluster = shaped.cluster_at_text_pos(2).unwrap();
    assert_eq!(cluster.text_start, 2);
}

// ============================================================================
// TextShaper basic tests
// ============================================================================

#[test]
fn test_text_shaper_creation() {
    let shaper = TextShaper::new();
    // TextShaper is a zero-sized type
    assert_eq!(std::mem::size_of_val(&shaper), 0);
}

#[test]
fn test_text_shaper_default() {
    let shaper = TextShaper::default();
    assert_eq!(std::mem::size_of_val(&shaper), 0);
}

// ============================================================================
// Latin text shaping tests
// ============================================================================

#[test]
fn test_shape_latin_simple() {
    let Some((shaper, font)) = get_shaper_and_font() else {
        return; // Skip if no fonts available
    };

    let shaped = shaper
        .shape_text("Hello", &font, 16.0, Script::Latin, Direction::LeftToRight)
        .expect("Should shape Latin text");

    assert_eq!(shaped.text, "Hello");
    assert_eq!(shaped.glyphs.len(), 5);
    assert!(shaped.total_advance > 0.0);
    assert_eq!(shaped.direction, Direction::LeftToRight);
    assert_eq!(shaped.script, Script::Latin);
}

#[test]
fn test_shape_latin_empty_string() {
    let Some((shaper, font)) = get_shaper_and_font() else {
        return;
    };

    let shaped = shaper
        .shape_text("", &font, 16.0, Script::Latin, Direction::LeftToRight)
        .expect("Should handle empty string");

    assert!(shaped.is_empty());
    assert_eq!(shaped.total_advance, 0.0);
}

#[test]
fn test_shape_latin_single_char() {
    let Some((shaper, font)) = get_shaper_and_font() else {
        return;
    };

    let shaped = shaper
        .shape_text("A", &font, 16.0, Script::Latin, Direction::LeftToRight)
        .expect("Should shape single character");

    assert_eq!(shaped.glyphs.len(), 1);
    assert!(shaped.total_advance > 0.0);
    assert!(shaped.total_advance < 20.0); // Reasonable width for 16px font
}

#[test]
fn test_shape_latin_with_spaces() {
    let Some((shaper, font)) = get_shaper_and_font() else {
        return;
    };

    let shaped = shaper
        .shape_text("A B C", &font, 16.0, Script::Latin, Direction::LeftToRight)
        .expect("Should shape text with spaces");

    assert_eq!(shaped.glyphs.len(), 5); // A, space, B, space, C
    assert!(shaped.total_advance > 0.0);
}

#[test]
fn test_shape_latin_accented() {
    let Some((shaper, font)) = get_shaper_and_font() else {
        return;
    };

    let shaped = shaper
        .shape_text("café", &font, 16.0, Script::Latin, Direction::LeftToRight)
        .expect("Should shape accented Latin text");

    assert!(!shaped.is_empty());
    assert!(shaped.total_advance > 0.0);
}

#[test]
fn test_shape_latin_font_size_scaling() {
    let Some((shaper, font)) = get_shaper_and_font() else {
        return;
    };

    let shaped_16 = shaper
        .shape_text("Hello", &font, 16.0, Script::Latin, Direction::LeftToRight)
        .expect("Should shape at 16px");

    let shaped_32 = shaper
        .shape_text("Hello", &font, 32.0, Script::Latin, Direction::LeftToRight)
        .expect("Should shape at 32px");

    // Double font size should roughly double the advance
    assert!(shaped_32.total_advance > shaped_16.total_advance * 1.8);
    assert!(shaped_32.total_advance < shaped_16.total_advance * 2.2);
}

// ============================================================================
// Ligature tests
// ============================================================================

#[test]
fn test_shape_potential_ligature_fi() {
    let Some((shaper, font)) = get_shaper_and_font() else {
        return;
    };

    let shaped = shaper
        .shape_text("office", &font, 16.0, Script::Latin, Direction::LeftToRight)
        .expect("Should shape text with potential ligatures");

    // The word "office" contains "ffi" which might be ligatured
    // Glyph count may be less than character count if ligatures are applied
    assert!(!shaped.is_empty());
    assert!(shaped.glyphs.len() <= 6); // May have ligature
    assert!(shaped.total_advance > 0.0);
}

#[test]
fn test_shape_potential_ligature_ff() {
    let Some((shaper, font)) = get_shaper_and_font() else {
        return;
    };

    let shaped = shaper
        .shape_text("affect", &font, 16.0, Script::Latin, Direction::LeftToRight)
        .expect("Should shape text with ff");

    assert!(!shaped.is_empty());
    assert!(shaped.glyphs.len() <= 6); // May have ligature
}

#[test]
fn test_shape_potential_ligature_fl() {
    let Some((shaper, font)) = get_shaper_and_font() else {
        return;
    };

    let shaped = shaper
        .shape_text("flower", &font, 16.0, Script::Latin, Direction::LeftToRight)
        .expect("Should shape text with fl");

    assert!(!shaped.is_empty());
    assert!(shaped.glyphs.len() <= 6); // May have ligature
}

// ============================================================================
// Kerning tests
// ============================================================================

#[test]
fn test_shape_kerning_av() {
    let Some((shaper, font)) = get_shaper_and_font() else {
        return;
    };

    // "AV" and "VA" are common kerning pairs
    let shaped = shaper
        .shape_text("AVATAR", &font, 16.0, Script::Latin, Direction::LeftToRight)
        .expect("Should shape text with kerning pairs");

    assert!(!shaped.is_empty());
    assert_eq!(shaped.glyphs.len(), 6);
    assert!(shaped.total_advance > 0.0);
}

#[test]
fn test_shape_kerning_to() {
    let Some((shaper, font)) = get_shaper_and_font() else {
        return;
    };

    // "To" is a common kerning pair
    let shaped = shaper
        .shape_text("To", &font, 16.0, Script::Latin, Direction::LeftToRight)
        .expect("Should shape kerning pair");

    assert!(!shaped.is_empty());
    assert_eq!(shaped.glyphs.len(), 2);
}

// ============================================================================
// Arabic/RTL tests
// ============================================================================

#[test]
fn test_shape_arabic_basic() {
    let Some((shaper, font)) = get_shaper_and_font() else {
        return;
    };

    // Arabic: "مرحبا" (Hello)
    let result = shaper.shape_text("مرحبا", &font, 16.0, Script::Arabic, Direction::RightToLeft);

    // This might fail if font doesn't have Arabic glyphs, which is OK
    if let Ok(shaped) = result {
        assert!(!shaped.is_empty());
        assert_eq!(shaped.direction, Direction::RightToLeft);
        assert_eq!(shaped.script, Script::Arabic);
        // Arabic has contextual shaping - glyph count may differ from char count
        assert!(shaped.glyphs.len() > 0);
    }
}

#[test]
fn test_shape_hebrew_basic() {
    let Some((shaper, font)) = get_shaper_and_font() else {
        return;
    };

    // Hebrew: "שלום" (Hello)
    let result = shaper.shape_text("שלום", &font, 16.0, Script::Hebrew, Direction::RightToLeft);

    if let Ok(shaped) = result {
        assert!(!shaped.is_empty());
        assert_eq!(shaped.direction, Direction::RightToLeft);
        assert_eq!(shaped.script, Script::Hebrew);
    }
}

#[test]
fn test_shape_rtl_direction_property() {
    let Some((shaper, font)) = get_shaper_and_font() else {
        return;
    };

    let shaped = shaper
        .shape_text("test", &font, 16.0, Script::Latin, Direction::RightToLeft)
        .expect("Should shape with RTL direction");

    assert_eq!(shaped.direction, Direction::RightToLeft);
}

// ============================================================================
// Complex script tests
// ============================================================================

#[test]
fn test_shape_devanagari() {
    let Some((shaper, font)) = get_shaper_and_font() else {
        return;
    };

    // Devanagari: "नमस्ते" (Hello)
    let result = shaper.shape_text("नमस्ते", &font, 16.0, Script::Devanagari, Direction::LeftToRight);

    // May fail if font doesn't support Devanagari
    if let Ok(shaped) = result {
        assert!(!shaped.is_empty());
        // Devanagari has complex shaping - glyph count may differ from char count
    }
}

#[test]
fn test_shape_cyrillic() {
    let Some((shaper, font)) = get_shaper_and_font() else {
        return;
    };

    // Cyrillic: "Привет" (Hello)
    let result = shaper.shape_text("Привет", &font, 16.0, Script::Cyrillic, Direction::LeftToRight);

    if let Ok(shaped) = result {
        assert!(!shaped.is_empty());
        assert_eq!(shaped.script, Script::Cyrillic);
    }
}

// ============================================================================
// Auto-detection tests
// ============================================================================

#[test]
fn test_shape_text_auto_latin() {
    let Some((shaper, font)) = get_shaper_and_font() else {
        return;
    };

    let shaped = shaper
        .shape_text_auto("Hello", &font, 16.0)
        .expect("Should auto-detect Latin");

    assert_eq!(shaped.script, Script::Latin);
    assert_eq!(shaped.direction, Direction::LeftToRight);
}

#[test]
fn test_shape_text_auto_arabic() {
    let Some((shaper, font)) = get_shaper_and_font() else {
        return;
    };

    let result = shaper.shape_text_auto("مرحبا", &font, 16.0);

    if let Ok(shaped) = result {
        assert_eq!(shaped.script, Script::Arabic);
        assert_eq!(shaped.direction, Direction::RightToLeft);
    }
}

#[test]
fn test_shape_text_simple() {
    let Some((shaper, font)) = get_shaper_and_font() else {
        return;
    };

    let shaped = shaper
        .shape_text_simple("Hello", &font, 16.0)
        .expect("Should shape simple text");

    assert_eq!(shaped.script, Script::Latin);
    assert_eq!(shaped.direction, Direction::LeftToRight);
}

// ============================================================================
// Combining marks tests
// ============================================================================

#[test]
fn test_shape_combining_acute() {
    let Some((shaper, font)) = get_shaper_and_font() else {
        return;
    };

    // "e" followed by combining acute accent
    let text = "e\u{0301}"; // e + combining acute
    let shaped = shaper
        .shape_text(text, &font, 16.0, Script::Latin, Direction::LeftToRight)
        .expect("Should shape combining marks");

    // The combined character should form a cluster
    assert!(!shaped.is_empty());
    assert!(shaped.total_advance > 0.0);
}

#[test]
fn test_shape_precomposed_vs_combining() {
    let Some((shaper, font)) = get_shaper_and_font() else {
        return;
    };

    // Precomposed é
    let precomposed = shaper
        .shape_text("é", &font, 16.0, Script::Latin, Direction::LeftToRight)
        .expect("Should shape precomposed");

    // Combining e + acute
    let combining = shaper
        .shape_text("e\u{0301}", &font, 16.0, Script::Latin, Direction::LeftToRight)
        .expect("Should shape combining");

    // Both should have similar width
    let diff = (precomposed.total_advance - combining.total_advance).abs();
    assert!(diff < 2.0); // Allow small tolerance
}

// ============================================================================
// Cluster tracking tests
// ============================================================================

#[test]
fn test_cluster_tracking_simple() {
    let Some((shaper, font)) = get_shaper_and_font() else {
        return;
    };

    let shaped = shaper
        .shape_text("abc", &font, 16.0, Script::Latin, Direction::LeftToRight)
        .expect("Should track clusters");

    // For simple Latin, each character should be its own cluster
    assert_eq!(shaped.clusters.len(), 3);

    // Verify cluster order
    for (i, cluster) in shaped.clusters.iter().enumerate() {
        assert_eq!(cluster.text_start, i);
        assert_eq!(cluster.text_len, 1);
    }
}

#[test]
fn test_cluster_advances_sum_to_total() {
    let Some((shaper, font)) = get_shaper_and_font() else {
        return;
    };

    let shaped = shaper
        .shape_text("Hello", &font, 16.0, Script::Latin, Direction::LeftToRight)
        .expect("Should shape text");

    let cluster_sum: f32 = shaped.clusters.iter().map(|c| c.advance).sum();

    // Cluster advances should sum to total advance
    assert!((cluster_sum - shaped.total_advance).abs() < 0.01);
}

#[test]
fn test_glyph_advances_sum_to_total() {
    let Some((shaper, font)) = get_shaper_and_font() else {
        return;
    };

    let shaped = shaper
        .shape_text("Hello", &font, 16.0, Script::Latin, Direction::LeftToRight)
        .expect("Should shape text");

    let glyph_sum: f32 = shaped.glyphs.iter().map(|g| g.advance).sum();

    // Glyph advances should sum to total advance
    assert!((glyph_sum - shaped.total_advance).abs() < 0.01);
}

// ============================================================================
// Hit testing tests
// ============================================================================

#[test]
fn test_x_position_for_text_offset() {
    let Some((shaper, font)) = get_shaper_and_font() else {
        return;
    };

    let shaped = shaper
        .shape_text("abc", &font, 16.0, Script::Latin, Direction::LeftToRight)
        .expect("Should shape text");

    // Position at start should be 0
    assert_eq!(shaped.x_position_for_text_offset(0), 0.0);

    // Position at end should be total advance
    let end_pos = shaped.x_position_for_text_offset(3);
    assert!((end_pos - shaped.total_advance).abs() < 0.01);

    // Positions should be monotonically increasing
    let pos0 = shaped.x_position_for_text_offset(0);
    let pos1 = shaped.x_position_for_text_offset(1);
    let pos2 = shaped.x_position_for_text_offset(2);
    let pos3 = shaped.x_position_for_text_offset(3);

    assert!(pos0 <= pos1);
    assert!(pos1 <= pos2);
    assert!(pos2 <= pos3);
}

#[test]
fn test_text_offset_for_x_position() {
    let Some((shaper, font)) = get_shaper_and_font() else {
        return;
    };

    let shaped = shaper
        .shape_text("abc", &font, 16.0, Script::Latin, Direction::LeftToRight)
        .expect("Should shape text");

    // Click at 0 should return 0
    assert_eq!(shaped.text_offset_for_x_position(0.0), 0);

    // Click past end should return text length
    assert_eq!(shaped.text_offset_for_x_position(1000.0), 3);

    // Click at negative should return 0
    assert_eq!(shaped.text_offset_for_x_position(-10.0), 0);
}

// ============================================================================
// Width measurement tests
// ============================================================================

#[test]
fn test_measure_width() {
    let Some((shaper, font)) = get_shaper_and_font() else {
        return;
    };

    let width = shaper.measure_width("Hello", &font, 16.0).expect("Should measure width");

    assert!(width > 0.0);
    assert!(width < 100.0); // Reasonable bound for 16px "Hello"
}

#[test]
fn test_measure_width_empty() {
    let Some((shaper, font)) = get_shaper_and_font() else {
        return;
    };

    let width = shaper.measure_width("", &font, 16.0).expect("Should measure empty");

    assert_eq!(width, 0.0);
}

#[test]
fn test_measure_width_consistency_with_shape() {
    let Some((shaper, font)) = get_shaper_and_font() else {
        return;
    };

    let width = shaper.measure_width("Hello", &font, 16.0).expect("Should measure");

    let shaped = shaper.shape_text_auto("Hello", &font, 16.0).expect("Should shape");

    assert!((width - shaped.total_advance).abs() < 0.01);
}

// ============================================================================
// Unicode edge cases
// ============================================================================

#[test]
fn test_shape_emoji() {
    let Some((shaper, font)) = get_shaper_and_font() else {
        return;
    };

    // Simple emoji
    let result = shaper.shape_text_auto("👋", &font, 16.0);

    // May succeed or fail depending on font support
    // Just ensure it doesn't panic
    let _ = result;
}

#[test]
fn test_shape_mixed_scripts() {
    let Some((shaper, font)) = get_shaper_and_font() else {
        return;
    };

    // Mixed Latin and numbers
    let shaped = shaper
        .shape_text("ABC123", &font, 16.0, Script::Latin, Direction::LeftToRight)
        .expect("Should shape mixed");

    assert_eq!(shaped.glyphs.len(), 6);
}

#[test]
fn test_shape_newlines() {
    let Some((shaper, font)) = get_shaper_and_font() else {
        return;
    };

    // Text with newline
    let shaped = shaper
        .shape_text("a\nb", &font, 16.0, Script::Latin, Direction::LeftToRight)
        .expect("Should handle newlines");

    // Should have 3 glyphs (a, newline, b)
    assert_eq!(shaped.glyphs.len(), 3);
}

#[test]
fn test_shape_tabs() {
    let Some((shaper, font)) = get_shaper_and_font() else {
        return;
    };

    // Text with tab
    let shaped = shaper
        .shape_text("a\tb", &font, 16.0, Script::Latin, Direction::LeftToRight)
        .expect("Should handle tabs");

    assert!(!shaped.is_empty());
}
