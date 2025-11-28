//! Tests for font fallback chain implementation.
//!
//! These tests verify the font fallback chain behavior according to
//! CSS Fonts Module Level 4, Section 5.

use fastrender::text::{
    FallbackChain, FallbackChainBuilder, FamilyEntry, FontDatabase, FontStretch, FontStyle, GenericFamily,
};

// ============================================================================
// GenericFamily Tests
// ============================================================================

#[test]
fn test_generic_family_parse_serif() {
    assert_eq!(GenericFamily::parse("serif"), Some(GenericFamily::Serif));
}

#[test]
fn test_generic_family_parse_sans_serif() {
    assert_eq!(GenericFamily::parse("sans-serif"), Some(GenericFamily::SansSerif));
}

#[test]
fn test_generic_family_parse_monospace() {
    assert_eq!(GenericFamily::parse("monospace"), Some(GenericFamily::Monospace));
}

#[test]
fn test_generic_family_parse_cursive() {
    assert_eq!(GenericFamily::parse("cursive"), Some(GenericFamily::Cursive));
}

#[test]
fn test_generic_family_parse_fantasy() {
    assert_eq!(GenericFamily::parse("fantasy"), Some(GenericFamily::Fantasy));
}

#[test]
fn test_generic_family_parse_system_ui() {
    assert_eq!(GenericFamily::parse("system-ui"), Some(GenericFamily::SystemUi));
}

#[test]
fn test_generic_family_parse_emoji() {
    assert_eq!(GenericFamily::parse("emoji"), Some(GenericFamily::Emoji));
}

#[test]
fn test_generic_family_parse_case_insensitive() {
    assert_eq!(GenericFamily::parse("SERIF"), Some(GenericFamily::Serif));
    assert_eq!(GenericFamily::parse("Sans-Serif"), Some(GenericFamily::SansSerif));
    assert_eq!(GenericFamily::parse("MONOSPACE"), Some(GenericFamily::Monospace));
}

#[test]
fn test_generic_family_parse_unknown() {
    assert_eq!(GenericFamily::parse("arial"), None);
    assert_eq!(GenericFamily::parse("roboto"), None);
    assert_eq!(GenericFamily::parse(""), None);
}

#[test]
fn test_generic_family_fallback_families_not_empty() {
    assert!(!GenericFamily::Serif.fallback_families().is_empty());
    assert!(!GenericFamily::SansSerif.fallback_families().is_empty());
    assert!(!GenericFamily::Monospace.fallback_families().is_empty());
    assert!(!GenericFamily::Cursive.fallback_families().is_empty());
    assert!(!GenericFamily::Fantasy.fallback_families().is_empty());
    assert!(!GenericFamily::SystemUi.fallback_families().is_empty());
    assert!(!GenericFamily::Emoji.fallback_families().is_empty());
}

#[test]
fn test_generic_family_fallback_families_contain_expected() {
    let sans_serif_names = GenericFamily::SansSerif.fallback_families();
    assert!(sans_serif_names.contains(&"Arial"));
    assert!(sans_serif_names.contains(&"Helvetica"));

    let monospace_names = GenericFamily::Monospace.fallback_families();
    assert!(monospace_names.contains(&"Courier New"));

    let serif_names = GenericFamily::Serif.fallback_families();
    assert!(serif_names.contains(&"Times New Roman"));
}

// ============================================================================
// FallbackChain Construction Tests
// ============================================================================

#[test]
fn test_fallback_chain_new_is_empty() {
    let chain = FallbackChain::new();
    assert!(chain.is_empty());
    assert_eq!(chain.len(), 0);
}

#[test]
fn test_fallback_chain_add_family() {
    let chain = FallbackChain::new().add_family("Arial");
    assert_eq!(chain.len(), 1);
    assert!(!chain.is_empty());
}

#[test]
fn test_fallback_chain_add_multiple_families() {
    let chain = FallbackChain::new()
        .add_family("Roboto")
        .add_family("Arial")
        .add_family("Helvetica");

    assert_eq!(chain.len(), 3);
}

#[test]
fn test_fallback_chain_add_generic() {
    let chain = FallbackChain::new().add_generic(GenericFamily::SansSerif);

    assert_eq!(chain.len(), 1);
    assert!(chain.families()[0].is_generic());
}

#[test]
fn test_fallback_chain_mixed_families() {
    let chain = FallbackChain::new()
        .add_family("Custom Font")
        .add_family("Arial")
        .add_generic(GenericFamily::SansSerif);

    assert_eq!(chain.len(), 3);
    assert!(!chain.families()[0].is_generic());
    assert!(!chain.families()[1].is_generic());
    assert!(chain.families()[2].is_generic());
}

#[test]
fn test_fallback_chain_from_families() {
    let families = vec!["Roboto".to_string(), "Arial".to_string(), "sans-serif".to_string()];
    let chain = FallbackChain::from_families(&families);

    assert_eq!(chain.len(), 3);

    // First two should be named families
    match &chain.families()[0] {
        FamilyEntry::Named(name) => assert_eq!(name, "Roboto"),
        _ => panic!("Expected named family"),
    }

    // Last should be generic
    match &chain.families()[2] {
        FamilyEntry::Generic(GenericFamily::SansSerif) => {}
        _ => panic!("Expected generic SansSerif"),
    }
}

#[test]
fn test_fallback_chain_from_families_all_generic() {
    let families = vec!["serif".to_string(), "sans-serif".to_string(), "monospace".to_string()];
    let chain = FallbackChain::from_families(&families);

    assert_eq!(chain.len(), 3);
    assert!(chain.families().iter().all(|f| f.is_generic()));
}

// ============================================================================
// FallbackChain Configuration Tests
// ============================================================================

#[test]
fn test_fallback_chain_with_weight() {
    let chain = FallbackChain::new().with_weight(700);
    // Weight is private, but we can verify it doesn't panic
    assert!(chain.is_empty()); // Just verify chain is valid
}

#[test]
fn test_fallback_chain_with_weight_clamping() {
    // Weight should be clamped to 1-1000
    let chain = FallbackChain::new().with_weight(0);
    let chain2 = FallbackChain::new().with_weight(2000);
    // Both should be valid chains
    assert!(chain.is_empty());
    assert!(chain2.is_empty());
}

#[test]
fn test_fallback_chain_with_style() {
    let chain = FallbackChain::new().with_style(FontStyle::Italic);
    assert!(chain.is_empty());
}

#[test]
fn test_fallback_chain_with_stretch() {
    let chain = FallbackChain::new().with_stretch(FontStretch::Condensed);
    assert!(chain.is_empty());
}

// ============================================================================
// FallbackChainBuilder Tests
// ============================================================================

#[test]
fn test_fallback_chain_builder_family() {
    let chain = FallbackChainBuilder::new().family("Arial").family("Helvetica").build();

    assert_eq!(chain.len(), 2);
}

#[test]
fn test_fallback_chain_builder_generic() {
    let chain = FallbackChainBuilder::new().generic(GenericFamily::Serif).build();

    assert_eq!(chain.len(), 1);
}

#[test]
fn test_fallback_chain_builder_weight() {
    let chain = FallbackChainBuilder::new().family("Arial").weight(700).build();

    assert_eq!(chain.len(), 1);
}

#[test]
fn test_fallback_chain_builder_normal() {
    let chain = FallbackChainBuilder::new().family("Arial").normal().build();

    assert_eq!(chain.len(), 1);
}

#[test]
fn test_fallback_chain_builder_bold() {
    let chain = FallbackChainBuilder::new().family("Arial").bold().build();

    assert_eq!(chain.len(), 1);
}

#[test]
fn test_fallback_chain_builder_italic() {
    let chain = FallbackChainBuilder::new().family("Arial").italic().build();

    assert_eq!(chain.len(), 1);
}

#[test]
fn test_fallback_chain_builder_stretch() {
    let chain = FallbackChainBuilder::new()
        .family("Arial")
        .stretch(FontStretch::Expanded)
        .build();

    assert_eq!(chain.len(), 1);
}

#[test]
fn test_fallback_chain_builder_full_configuration() {
    let chain = FallbackChainBuilder::new()
        .family("Roboto")
        .family("Arial")
        .generic(GenericFamily::SansSerif)
        .bold()
        .italic()
        .stretch(FontStretch::Normal)
        .build();

    assert_eq!(chain.len(), 3);
}

// ============================================================================
// FamilyEntry Tests
// ============================================================================

#[test]
fn test_family_entry_named() {
    let entry = FamilyEntry::named("Arial");
    assert!(!entry.is_generic());
}

#[test]
fn test_family_entry_generic() {
    let entry = FamilyEntry::generic(GenericFamily::Serif);
    assert!(entry.is_generic());
}

// ============================================================================
// FontDatabase Tests
// ============================================================================

#[test]
fn test_font_database_empty() {
    let db = FontDatabase::empty();
    assert!(db.is_empty());
    assert_eq!(db.font_count(), 0);
}

#[test]
fn test_font_database_new_loads_system_fonts() {
    let db = FontDatabase::new();
    // System fonts may or may not be available, so we just check it doesn't panic
    let _ = db.font_count();
}

#[test]
fn test_font_database_is_emoji() {
    // Emoji characters
    assert!(FontDatabase::is_emoji('😀')); // Grinning face
    assert!(FontDatabase::is_emoji('🎉')); // Party popper
    assert!(FontDatabase::is_emoji('🚀')); // Rocket
    assert!(FontDatabase::is_emoji('❤')); // Heavy heart (dingbat)
    assert!(FontDatabase::is_emoji('🇺')); // Regional indicator

    // Non-emoji characters
    assert!(!FontDatabase::is_emoji('A'));
    assert!(!FontDatabase::is_emoji('z'));
    assert!(!FontDatabase::is_emoji('0'));
    assert!(!FontDatabase::is_emoji('!'));
    assert!(!FontDatabase::is_emoji('中'));
    assert!(!FontDatabase::is_emoji('ñ'));
    assert!(!FontDatabase::is_emoji('α'));
}

// ============================================================================
// FallbackChain Resolution Tests
// ============================================================================

#[test]
fn test_fallback_chain_resolve_empty_db() {
    let db = FontDatabase::empty();
    let chain = FallbackChain::new().add_family("Arial");

    // Should return None since no fonts are loaded
    let result = chain.resolve('A', &db);
    assert!(result.is_none());
}

#[test]
fn test_fallback_chain_resolve_default_empty_db() {
    let db = FontDatabase::empty();
    let chain = FallbackChain::new().add_family("Arial");

    let result = chain.resolve_default(&db);
    assert!(result.is_none());
}

#[test]
fn test_fallback_chain_resolve_empty_chain() {
    let db = FontDatabase::empty();
    let chain = FallbackChain::new();

    let result = chain.resolve('A', &db);
    assert!(result.is_none());
}

// ============================================================================
// FontStyle Tests
// ============================================================================

#[test]
fn test_font_style_default() {
    assert_eq!(FontStyle::default(), FontStyle::Normal);
}

// ============================================================================
// FontStretch Tests
// ============================================================================

#[test]
fn test_font_stretch_to_percentage() {
    assert_eq!(FontStretch::UltraCondensed.to_percentage(), 50.0);
    assert_eq!(FontStretch::Condensed.to_percentage(), 75.0);
    assert_eq!(FontStretch::Normal.to_percentage(), 100.0);
    assert_eq!(FontStretch::Expanded.to_percentage(), 125.0);
    assert_eq!(FontStretch::UltraExpanded.to_percentage(), 200.0);
}

#[test]
fn test_font_stretch_from_percentage() {
    assert_eq!(FontStretch::from_percentage(50.0), FontStretch::UltraCondensed);
    assert_eq!(FontStretch::from_percentage(75.0), FontStretch::Condensed);
    assert_eq!(FontStretch::from_percentage(100.0), FontStretch::Normal);
    assert_eq!(FontStretch::from_percentage(125.0), FontStretch::Expanded);
    assert_eq!(FontStretch::from_percentage(200.0), FontStretch::UltraExpanded);
}

#[test]
fn test_font_stretch_from_percentage_boundary_values() {
    // Test boundary values between stretch categories
    assert_eq!(FontStretch::from_percentage(56.0), FontStretch::UltraCondensed);
    assert_eq!(FontStretch::from_percentage(57.0), FontStretch::ExtraCondensed);
    assert_eq!(FontStretch::from_percentage(69.0), FontStretch::ExtraCondensed);
    assert_eq!(FontStretch::from_percentage(70.0), FontStretch::Condensed);
}

// ============================================================================
// Integration Tests with System Fonts
// ============================================================================

#[test]
fn test_fallback_chain_with_system_fonts() {
    let db = FontDatabase::new();

    // Skip if no system fonts (e.g., in minimal container)
    if db.is_empty() {
        return;
    }

    let chain = FallbackChain::new().add_generic(GenericFamily::SansSerif);

    // Should find a font for ASCII characters
    let result = chain.resolve('A', &db);
    // May or may not succeed depending on system fonts
    // This test just verifies no panic
    let _ = result;
}

#[test]
fn test_fallback_chain_resolve_default_with_system_fonts() {
    let db = FontDatabase::new();

    if db.is_empty() {
        return;
    }

    let chain = FallbackChain::new().add_generic(GenericFamily::SansSerif);

    let result = chain.resolve_default(&db);
    // Should find some font
    assert!(result.is_some());
}

#[test]
fn test_fallback_chain_emoji_resolution() {
    let db = FontDatabase::new();

    if db.is_empty() {
        return;
    }

    let chain = FallbackChain::new()
        .add_generic(GenericFamily::Emoji)
        .add_generic(GenericFamily::SansSerif);

    // Try to resolve an emoji
    let result = chain.resolve('😀', &db);
    // May or may not find emoji font depending on system
    let _ = result;
}

// ============================================================================
// Edge Cases
// ============================================================================

#[test]
fn test_fallback_chain_unicode_characters() {
    let db = FontDatabase::new();

    if db.is_empty() {
        return;
    }

    let chain = FallbackChain::new().add_generic(GenericFamily::SansSerif);

    // Test various Unicode characters
    let _ = chain.resolve('A', &db); // ASCII
    let _ = chain.resolve('中', &db); // CJK
    let _ = chain.resolve('ñ', &db); // Latin Extended
    let _ = chain.resolve('α', &db); // Greek
    let _ = chain.resolve('א', &db); // Hebrew
    let _ = chain.resolve('ع', &db); // Arabic
}

#[test]
fn test_fallback_chain_whitespace_characters() {
    let db = FontDatabase::new();

    if db.is_empty() {
        return;
    }

    let chain = FallbackChain::new().add_generic(GenericFamily::SansSerif);

    // Space characters should be found in most fonts
    let _ = chain.resolve(' ', &db);
    let _ = chain.resolve('\t', &db);
}

#[test]
fn test_fallback_chain_rare_characters() {
    let db = FontDatabase::empty();
    let chain = FallbackChain::new().add_family("NonExistentFont");

    // Rare Unicode character - should return None in empty db
    let result = chain.resolve('\u{10FFFD}', &db);
    assert!(result.is_none());
}
