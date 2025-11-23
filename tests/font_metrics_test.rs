//! Comprehensive tests for font metrics extraction
//!
//! Tests cover:
//! - Metrics extraction from system fonts
//! - Scaling metrics by font size
//! - Line height calculations
//! - Integration with FontDatabase and FontContext

use fastrender::text::{FontContext, FontDatabase, FontStyle, FontWeight};

// ============================================================================
// FontMetrics Unit Tests
// ============================================================================

/// Test: FontDatabase creation and font count
#[test]
fn test_font_database_creation() {
    let db = FontDatabase::new();
    // System should have fonts (may be 0 in minimal CI)
    let _ = db.font_count();
}

/// Test: FontDatabase empty creation
#[test]
fn test_font_database_empty() {
    let db = FontDatabase::empty();
    assert!(db.is_empty());
    assert_eq!(db.font_count(), 0);
}

/// Test: Query for generic sans-serif font
#[test]
fn test_query_sans_serif() {
    let db = FontDatabase::new();
    if db.is_empty() {
        return;
    }

    let id = db.query("sans-serif", FontWeight::NORMAL, FontStyle::Normal);
    if let Some(id) = id {
        let font = db.load_font(id);
        assert!(font.is_some());
        assert!(!font.unwrap().data.is_empty());
    }
}

/// Test: Query for generic serif font
#[test]
fn test_query_serif() {
    let db = FontDatabase::new();
    if db.is_empty() {
        return;
    }

    let id = db.query("serif", FontWeight::NORMAL, FontStyle::Normal);
    if id.is_some() {
        let font = db.load_font(id.unwrap());
        assert!(font.is_some());
    }
}

/// Test: Query for monospace font
#[test]
fn test_query_monospace() {
    let db = FontDatabase::new();
    if db.is_empty() {
        return;
    }

    let id = db.query("monospace", FontWeight::NORMAL, FontStyle::Normal);
    if id.is_some() {
        let font = db.load_font(id.unwrap());
        assert!(font.is_some());
    }
}

/// Test: Font fallback chain
#[test]
fn test_font_fallback_chain() {
    let db = FontDatabase::new();
    if db.is_empty() {
        return;
    }

    let families = vec![
        "NonExistentFont12345".to_string(),
        "AnotherNonExistent".to_string(),
        "sans-serif".to_string(),
    ];

    let id = db.resolve_family_list(&families, FontWeight::NORMAL, FontStyle::Normal);
    if id.is_some() {
        let font = db.load_font(id.unwrap());
        assert!(font.is_some());
    }
}

/// Test: Font metrics extraction from loaded font
#[test]
fn test_font_metrics_extraction() {
    let db = FontDatabase::new();
    if db.is_empty() {
        return;
    }

    if let Some(id) = db.query("sans-serif", FontWeight::NORMAL, FontStyle::Normal) {
        let font = db.load_font(id).unwrap();
        let metrics = font.metrics().expect("Should extract metrics");

        assert!(metrics.units_per_em > 0);
        assert!(metrics.ascent > 0);
        assert!(metrics.descent < 0); // Descent is typically negative
        assert!(metrics.line_height > 0);
    }
}

/// Test: Scaled metrics basic properties
#[test]
fn test_scaled_metrics() {
    let db = FontDatabase::new();
    if db.is_empty() {
        return;
    }

    if let Some(id) = db.query("sans-serif", FontWeight::NORMAL, FontStyle::Normal) {
        let font = db.load_font(id).unwrap();
        let metrics = font.metrics().expect("Should extract metrics");
        let scaled = metrics.scale(16.0);

        assert_eq!(scaled.font_size, 16.0);
        assert!(scaled.ascent > 0.0);
        assert!(scaled.descent > 0.0); // Scaled descent is positive
        assert!(scaled.line_height > 0.0);
    }
}

/// Test: Scaled metrics total height
#[test]
fn test_scaled_metrics_total_height() {
    let db = FontDatabase::new();
    if db.is_empty() {
        return;
    }

    if let Some(id) = db.query("sans-serif", FontWeight::NORMAL, FontStyle::Normal) {
        let font = db.load_font(id).unwrap();
        let metrics = font.metrics().expect("Should extract metrics");
        let scaled = metrics.scale(16.0);

        // Total height should be reasonable for 16px font
        let total = scaled.total_height();
        assert!(total > 10.0 && total < 30.0);
    }
}

/// Test: Baseline offset equals ascent
#[test]
fn test_baseline_offset() {
    let db = FontDatabase::new();
    if db.is_empty() {
        return;
    }

    if let Some(id) = db.query("sans-serif", FontWeight::NORMAL, FontStyle::Normal) {
        let font = db.load_font(id).unwrap();
        let metrics = font.metrics().expect("Should extract metrics");
        let scaled = metrics.scale(16.0);

        assert_eq!(scaled.baseline_offset(), scaled.ascent);
    }
}

/// Test: Line height with factor
#[test]
fn test_line_height_factor() {
    let db = FontDatabase::new();
    if db.is_empty() {
        return;
    }

    if let Some(id) = db.query("sans-serif", FontWeight::NORMAL, FontStyle::Normal) {
        let font = db.load_font(id).unwrap();
        let metrics = font.metrics().expect("Should extract metrics");
        let scaled = metrics.scale(16.0);
        let with_factor = scaled.with_line_height_factor(1.5);

        assert_eq!(with_factor.line_height, 24.0); // 16 * 1.5
        assert_eq!(with_factor.font_size, 16.0); // Unchanged
    }
}

/// Test: Explicit line height
#[test]
fn test_explicit_line_height() {
    let db = FontDatabase::new();
    if db.is_empty() {
        return;
    }

    if let Some(id) = db.query("sans-serif", FontWeight::NORMAL, FontStyle::Normal) {
        let font = db.load_font(id).unwrap();
        let metrics = font.metrics().expect("Should extract metrics");
        let scaled = metrics.scale(16.0);
        let with_lh = scaled.with_line_height(30.0);

        assert_eq!(with_lh.line_height, 30.0);
    }
}

/// Test: Normal line height calculation
#[test]
fn test_normal_line_height() {
    let db = FontDatabase::new();
    if db.is_empty() {
        return;
    }

    if let Some(id) = db.query("sans-serif", FontWeight::NORMAL, FontStyle::Normal) {
        let font = db.load_font(id).unwrap();
        let metrics = font.metrics().expect("Should extract metrics");

        let normal_lh = metrics.normal_line_height(16.0);
        assert!(normal_lh > 0.0);
        // Normal line height is usually around font size
        assert!(normal_lh >= 14.0 && normal_lh < 32.0);
    }
}

/// Test: Font caching
#[test]
fn test_font_caching() {
    let db = FontDatabase::new();
    if db.is_empty() {
        return;
    }

    let id = db.query("sans-serif", FontWeight::NORMAL, FontStyle::Normal);
    if let Some(id) = id {
        assert_eq!(db.cache_size(), 0);

        let font1 = db.load_font(id);
        assert!(font1.is_some());
        assert_eq!(db.cache_size(), 1);

        let font2 = db.load_font(id);
        assert!(font2.is_some());

        // Same data via Arc
        let font1 = font1.unwrap();
        let font2 = font2.unwrap();
        assert!(std::sync::Arc::ptr_eq(&font1.data, &font2.data));
    }
}

/// Test: Clear cache
#[test]
fn test_clear_cache() {
    let db = FontDatabase::new();
    if db.is_empty() {
        return;
    }

    if let Some(id) = db.query("sans-serif", FontWeight::NORMAL, FontStyle::Normal) {
        let _font = db.load_font(id);
        assert!(db.cache_size() > 0);

        db.clear_cache();
        assert_eq!(db.cache_size(), 0);
    }
}

// ============================================================================
// FontContext Tests
// ============================================================================

/// Test: FontContext creation
#[test]
fn test_font_context_creation() {
    let ctx = FontContext::new();
    let _ = ctx.font_count();
}

/// Test: FontContext empty
#[test]
fn test_font_context_empty() {
    let ctx = FontContext::empty();
    assert!(!ctx.has_fonts());
    assert_eq!(ctx.font_count(), 0);
}

/// Test: Get font with fallback
#[test]
fn test_get_font_fallback() {
    let ctx = FontContext::new();
    if !ctx.has_fonts() {
        return;
    }

    let families = vec!["NonExistentFont12345".to_string(), "sans-serif".to_string()];
    let font = ctx.get_font(&families, 400, false, false);
    if font.is_some() {
        assert!(!font.unwrap().data.is_empty());
    }
}

/// Test: Get sans-serif font
#[test]
fn test_get_sans_serif() {
    let ctx = FontContext::new();
    if !ctx.has_fonts() {
        return;
    }

    let font = ctx.get_sans_serif();
    if font.is_some() {
        assert!(!font.unwrap().family.is_empty());
    }
}

/// Test: Get serif font
#[test]
fn test_get_serif() {
    let ctx = FontContext::new();
    if !ctx.has_fonts() {
        return;
    }

    let font = ctx.get_serif();
    if font.is_some() {
        assert!(!font.unwrap().data.is_empty());
    }
}

/// Test: Get monospace font
#[test]
fn test_get_monospace() {
    let ctx = FontContext::new();
    if !ctx.has_fonts() {
        return;
    }

    let font = ctx.get_monospace();
    if font.is_some() {
        assert!(!font.unwrap().data.is_empty());
    }
}

/// Test: FontContext clone shares database
#[test]
fn test_font_context_clone() {
    let ctx1 = FontContext::new();
    let ctx2 = ctx1.clone();

    assert_eq!(ctx1.font_count(), ctx2.font_count());
}

/// Test: Get italic font
#[test]
fn test_get_font_italic() {
    let ctx = FontContext::new();
    if !ctx.has_fonts() {
        return;
    }

    let families = vec!["sans-serif".to_string()];
    let font = ctx.get_font(&families, 400, true, false);
    if font.is_some() {
        assert!(!font.unwrap().data.is_empty());
    }
}

/// Test: Get bold font
#[test]
fn test_get_font_bold() {
    let ctx = FontContext::new();
    if !ctx.has_fonts() {
        return;
    }

    let families = vec!["sans-serif".to_string()];
    let font = ctx.get_font(&families, 700, false, false);
    if font.is_some() {
        let font = font.unwrap();
        assert!(font.weight.value() >= 400);
    }
}

// ============================================================================
// FontWeight Tests
// ============================================================================

/// Test: Font weight constants
#[test]
fn test_font_weight_constants() {
    assert_eq!(FontWeight::THIN.value(), 100);
    assert_eq!(FontWeight::NORMAL.value(), 400);
    assert_eq!(FontWeight::BOLD.value(), 700);
    assert_eq!(FontWeight::BLACK.value(), 900);
}

/// Test: Font weight clamping
#[test]
fn test_font_weight_clamping() {
    assert_eq!(FontWeight::new(0).value(), 100);
    assert_eq!(FontWeight::new(50).value(), 100);
    assert_eq!(FontWeight::new(1000).value(), 900);
    assert_eq!(FontWeight::new(500).value(), 500);
}

/// Test: Font weight default
#[test]
fn test_font_weight_default() {
    assert_eq!(FontWeight::default(), FontWeight::NORMAL);
}

/// Test: Font style default
#[test]
fn test_font_style_default() {
    assert_eq!(FontStyle::default(), FontStyle::Normal);
}

// ============================================================================
// Metrics from ttf-parser Face directly
// ============================================================================

/// Test: Extract metrics from ttf-parser Face
#[test]
fn test_extract_metrics_from_face() {
    let db = FontDatabase::new();
    if db.is_empty() {
        return;
    }

    if let Some(id) = db.query("sans-serif", FontWeight::NORMAL, FontStyle::Normal) {
        let font = db.load_font(id).unwrap();
        let face = font.as_ttf_face().expect("Should parse font");

        // Verify we can access the face
        assert!(face.units_per_em() > 0);
        assert!(face.ascender() > 0);
        assert!(face.descender() < 0);
    }
}

/// Test: Different font sizes scale correctly
#[test]
fn test_scaling_different_sizes() {
    let db = FontDatabase::new();
    if db.is_empty() {
        return;
    }

    if let Some(id) = db.query("sans-serif", FontWeight::NORMAL, FontStyle::Normal) {
        let font = db.load_font(id).unwrap();
        let metrics = font.metrics().expect("Should extract metrics");

        let scaled_12 = metrics.scale(12.0);
        let scaled_16 = metrics.scale(16.0);
        let scaled_24 = metrics.scale(24.0);

        // Larger font = larger metrics
        assert!(scaled_24.ascent > scaled_16.ascent);
        assert!(scaled_16.ascent > scaled_12.ascent);

        // Scale should be proportional
        assert!((scaled_16.scale / scaled_12.scale - 16.0 / 12.0).abs() < 0.001);
        assert!((scaled_24.scale / scaled_16.scale - 24.0 / 16.0).abs() < 0.001);
    }
}
