//! Integration tests for hyphenation
//!
//! These tests verify the hyphenation functionality works correctly
//! with various words, edge cases, and configurations.

use fastrender::find_soft_hyphens;
use fastrender::is_soft_hyphen;
use fastrender::remove_soft_hyphens;
use fastrender::Hyphenator;
use fastrender::HyphensMode;
use fastrender::SupportedLanguage;

// ============================================================================
// Hyphenator Creation Tests
// ============================================================================

#[test]
fn test_hyphenator_creation_english_us() {
  let hyphenator = Hyphenator::new("en-us");
  assert!(hyphenator.is_ok(), "Should create en-US hyphenator");
  assert_eq!(hyphenator.unwrap().language(), "en-US");
}

#[test]
fn test_hyphenator_creation_english_variants() {
  // Test various English language codes
  for code in &["en", "en-US", "EN-US", "En-Us", "eng"] {
    let result = Hyphenator::new(code);
    assert!(result.is_ok(), "Should accept language code: {}", code);
  }
}

#[test]
fn test_hyphenator_creation_invalid_language() {
  let result = Hyphenator::new("invalid-lang");
  assert!(result.is_err(), "Should reject invalid language");

  let result = Hyphenator::new("xx-XX");
  assert!(result.is_err(), "Should reject unknown language code");
}

#[test]
fn test_hyphenator_creation_empty_string() {
  let result = Hyphenator::new("");
  assert!(result.is_err(), "Should reject empty language string");
}

#[test]
fn test_hyphenator_with_custom_settings() {
  let hyphenator = Hyphenator::with_settings("en-us", 3, 3, 6).unwrap();

  assert_eq!(hyphenator.left_min(), 3);
  assert_eq!(hyphenator.right_min(), 3);
  assert_eq!(hyphenator.min_word_length(), 6);
}

#[test]
fn test_hyphenator_settings_clamping() {
  // Settings should be clamped to minimum values
  let hyphenator = Hyphenator::with_settings("en-us", 0, 0, 1).unwrap();

  assert_eq!(hyphenator.left_min(), 1); // Clamped to 1
  assert_eq!(hyphenator.right_min(), 1); // Clamped to 1
  assert_eq!(hyphenator.min_word_length(), 2); // Clamped to 2
}

// ============================================================================
// Word Hyphenation Tests
// ============================================================================

#[test]
fn test_hyphenate_common_word() {
  let hyphenator = Hyphenator::new("en-us").unwrap();

  // "hyphenation" is a commonly hyphenated word
  let points = hyphenator.hyphenate("hyphenation");
  assert!(!points.is_empty(), "Should find hyphenation points");
}

#[test]
fn test_hyphenate_preserves_word() {
  let hyphenator = Hyphenator::new("en-us").unwrap();

  let word = "internationalization";
  let segments = hyphenator.hyphenate_word(word);

  // Joining segments should reconstruct the original word
  let reconstructed: String = segments.join("");
  assert_eq!(
    reconstructed, word,
    "Segments should reconstruct original word"
  );
}

#[test]
fn test_hyphenate_short_words() {
  let hyphenator = Hyphenator::new("en-us").unwrap();

  // Short words should not be hyphenated
  let short_words = ["a", "an", "the", "cat", "dog"];

  for word in &short_words {
    let points = hyphenator.hyphenate(word);
    assert!(
      points.is_empty(),
      "Short word '{}' should not be hyphenated",
      word
    );
  }
}

#[test]
fn test_hyphenate_word_segments() {
  let hyphenator = Hyphenator::new("en-us").unwrap();

  // Short words should return single segment
  let segments = hyphenator.hyphenate_word("cat");
  assert_eq!(segments.len(), 1);
  assert_eq!(segments[0], "cat");

  // Long words should return multiple segments
  let segments = hyphenator.hyphenate_word("internationalization");
  assert!(
    segments.len() > 1,
    "Long word should have multiple segments"
  );
}

#[test]
fn test_hyphenate_non_alphabetic() {
  let hyphenator = Hyphenator::new("en-us").unwrap();

  // Words with numbers should not be hyphenated
  let points = hyphenator.hyphenate("hello123");
  assert!(points.is_empty(), "Words with numbers should not hyphenate");

  // Words with hyphens should not be hyphenated
  let points = hyphenator.hyphenate("hello-world");
  assert!(points.is_empty(), "Words with hyphens should not hyphenate");

  // Words with spaces should not be hyphenated
  let points = hyphenator.hyphenate("hello world");
  assert!(points.is_empty(), "Words with spaces should not hyphenate");
}

#[test]
fn test_hyphenate_empty_string() {
  let hyphenator = Hyphenator::new("en-us").unwrap();

  let points = hyphenator.hyphenate("");
  assert!(
    points.is_empty(),
    "Empty string should have no break points"
  );
}

#[test]
fn test_hyphenate_unicode() {
  let hyphenator = Hyphenator::new("en-us").unwrap();

  // Unicode letters should be handled
  let points = hyphenator.hyphenate("café");
  // May or may not have points, but shouldn't panic
  let _ = points;
}

// ============================================================================
// Text Hyphenation Tests
// ============================================================================

#[test]
fn test_hyphenate_text_simple() {
  let hyphenator = Hyphenator::new("en-us").unwrap();

  let text = "The internationalization of hyphenation is important.";
  let results = hyphenator.hyphenate_text(text);

  // Should find hyphenation opportunities in longer words
  assert!(
    !results.is_empty(),
    "Should find hyphenation points in text"
  );
}

#[test]
fn test_hyphenate_text_no_long_words() {
  let hyphenator = Hyphenator::new("en-us").unwrap();

  let text = "The cat sat on the mat.";
  let results = hyphenator.hyphenate_text(text);

  // No long words, so no hyphenation points
  assert!(
    results.is_empty(),
    "Short words text should have no hyphenation points"
  );
}

#[test]
fn test_hyphenate_text_positions() {
  let hyphenator = Hyphenator::new("en-us").unwrap();

  let text = "internationalization";
  let results = hyphenator.hyphenate_text(text);

  // All positions should be within text bounds
  for (word_start, breaks) in &results {
    assert!(*word_start < text.len(), "Word start should be within text");
    for &break_pos in breaks {
      assert!(
        break_pos <= text.len(),
        "Break position should be within text"
      );
    }
  }
}

// ============================================================================
// can_hyphenate Tests
// ============================================================================

#[test]
fn test_can_hyphenate() {
  let hyphenator = Hyphenator::new("en-us").unwrap();

  // Long alphabetic words can be hyphenated
  assert!(hyphenator.can_hyphenate("internationalization"));
  assert!(hyphenator.can_hyphenate("hyphenation"));

  // Short words cannot
  assert!(!hyphenator.can_hyphenate("cat"));
  assert!(!hyphenator.can_hyphenate("the"));

  // Non-alphabetic words cannot
  assert!(!hyphenator.can_hyphenate("hello123"));
  assert!(!hyphenator.can_hyphenate("hello-world"));
}

// ============================================================================
// SupportedLanguage Tests
// ============================================================================

#[test]
fn test_supported_language_parse_english() {
  assert_eq!(
    SupportedLanguage::parse("en"),
    Some(SupportedLanguage::EnglishUS)
  );
  assert_eq!(
    SupportedLanguage::parse("en-US"),
    Some(SupportedLanguage::EnglishUS)
  );
  assert_eq!(
    SupportedLanguage::parse("en-GB"),
    Some(SupportedLanguage::EnglishGB)
  );
}

#[test]
fn test_supported_language_parse_german() {
  assert_eq!(
    SupportedLanguage::parse("de"),
    Some(SupportedLanguage::German)
  );
  assert_eq!(
    SupportedLanguage::parse("de-DE"),
    Some(SupportedLanguage::German)
  );
}

#[test]
fn test_supported_language_parse_french() {
  assert_eq!(
    SupportedLanguage::parse("fr"),
    Some(SupportedLanguage::French)
  );
  assert_eq!(
    SupportedLanguage::parse("fr-FR"),
    Some(SupportedLanguage::French)
  );
}

#[test]
fn test_supported_language_parse_case_insensitive() {
  assert_eq!(
    SupportedLanguage::parse("EN-US"),
    Some(SupportedLanguage::EnglishUS)
  );
  assert_eq!(
    SupportedLanguage::parse("en-us"),
    Some(SupportedLanguage::EnglishUS)
  );
  assert_eq!(
    SupportedLanguage::parse("En-Us"),
    Some(SupportedLanguage::EnglishUS)
  );
}

#[test]
fn test_supported_language_code() {
  assert_eq!(SupportedLanguage::EnglishUS.code(), "en-US");
  assert_eq!(SupportedLanguage::EnglishGB.code(), "en-GB");
  assert_eq!(SupportedLanguage::German.code(), "de-DE");
  assert_eq!(SupportedLanguage::French.code(), "fr-FR");
}

#[test]
fn test_supported_language_is_embedded() {
  // Only en-US is embedded by default
  assert!(SupportedLanguage::EnglishUS.is_embedded());
  assert!(!SupportedLanguage::German.is_embedded());
}

#[test]
fn test_supported_language_all_variants() {
  // Test that all language codes return valid language
  let languages = [
    "en", "en-us", "en-gb", "de", "fr", "es", "it", "pt", "pt-br", "pt-pt", "nl", "pl", "ru", "sv",
    "nb", "da", "fi", "hu", "cs", "sk", "hr", "ca", "tr", "el", "uk", "la",
  ];

  for lang in &languages {
    assert!(
      SupportedLanguage::parse(lang).is_some(),
      "Should recognize language: {}",
      lang
    );
  }
}

// ============================================================================
// HyphensMode Tests
// ============================================================================

#[test]
fn test_hyphens_mode_parse() {
  assert_eq!(HyphensMode::parse("none"), Some(HyphensMode::None));
  assert_eq!(HyphensMode::parse("manual"), Some(HyphensMode::Manual));
  assert_eq!(HyphensMode::parse("auto"), Some(HyphensMode::Auto));
}

#[test]
fn test_hyphens_mode_parse_case_insensitive() {
  assert_eq!(HyphensMode::parse("NONE"), Some(HyphensMode::None));
  assert_eq!(HyphensMode::parse("Manual"), Some(HyphensMode::Manual));
  assert_eq!(HyphensMode::parse("AUTO"), Some(HyphensMode::Auto));
}

#[test]
fn test_hyphens_mode_parse_with_whitespace() {
  assert_eq!(HyphensMode::parse("  none  "), Some(HyphensMode::None));
  assert_eq!(HyphensMode::parse(" manual "), Some(HyphensMode::Manual));
}

#[test]
fn test_hyphens_mode_parse_invalid() {
  assert_eq!(HyphensMode::parse("invalid"), None);
  assert_eq!(HyphensMode::parse(""), None);
  assert_eq!(HyphensMode::parse("automatic"), None);
}

#[test]
fn test_hyphens_mode_as_css() {
  assert_eq!(HyphensMode::None.as_css(), "none");
  assert_eq!(HyphensMode::Manual.as_css(), "manual");
  assert_eq!(HyphensMode::Auto.as_css(), "auto");
}

#[test]
fn test_hyphens_mode_default() {
  let mode = HyphensMode::default();
  assert_eq!(mode, HyphensMode::Manual);
}

// ============================================================================
// Soft Hyphen Tests
// ============================================================================

#[test]
fn test_is_soft_hyphen() {
  assert!(is_soft_hyphen('\u{00AD}'));
  assert!(!is_soft_hyphen('-'));
  assert!(!is_soft_hyphen('a'));
  assert!(!is_soft_hyphen(' '));
}

#[test]
fn test_find_soft_hyphens() {
  let text = "hy\u{00AD}phen\u{00AD}ation";
  let positions = find_soft_hyphens(text);

  assert_eq!(positions.len(), 2);
}

#[test]
fn test_find_soft_hyphens_none() {
  let text = "hyphenation";
  let positions = find_soft_hyphens(text);

  assert!(positions.is_empty());
}

#[test]
fn test_find_soft_hyphens_multiple() {
  // Text with multiple soft hyphens
  let text = "a\u{00AD}b\u{00AD}c\u{00AD}d";
  let positions = find_soft_hyphens(text);

  assert_eq!(positions.len(), 3);
}

#[test]
fn test_remove_soft_hyphens() {
  let text = "hy\u{00AD}phen\u{00AD}ation";
  let cleaned = remove_soft_hyphens(text);

  assert_eq!(cleaned, "hyphenation");
  assert!(!cleaned.contains('\u{00AD}'));
}

#[test]
fn test_remove_soft_hyphens_preserves_regular_hyphens() {
  let text = "well-known\u{00AD}word";
  let cleaned = remove_soft_hyphens(text);

  assert_eq!(cleaned, "well-knownword");
  assert!(cleaned.contains('-'));
}

#[test]
fn test_remove_soft_hyphens_empty() {
  let text = "";
  let cleaned = remove_soft_hyphens(text);

  assert_eq!(cleaned, "");
}

// ============================================================================
// Edge Cases and Boundary Tests
// ============================================================================

#[test]
fn test_hyphenate_single_char() {
  let hyphenator = Hyphenator::new("en-us").unwrap();

  let points = hyphenator.hyphenate("a");
  assert!(points.is_empty());
}

#[test]
fn test_hyphenate_two_chars() {
  let hyphenator = Hyphenator::new("en-us").unwrap();

  let points = hyphenator.hyphenate("ab");
  assert!(points.is_empty());
}

#[test]
fn test_hyphenate_at_min_length() {
  let hyphenator = Hyphenator::new("en-us").unwrap();

  // Exactly at minimum length (5 by default)
  let word = "abcde";
  let _ = hyphenator.hyphenate(word); // Shouldn't panic
}

#[test]
fn test_hyphenate_very_long_word() {
  let hyphenator = Hyphenator::new("en-us").unwrap();

  // Very long compound word
  let word = "supercalifragilisticexpialidocious";
  let points = hyphenator.hyphenate(word);

  // Should find multiple break points
  assert!(
    !points.is_empty(),
    "Very long word should have break points"
  );
}

#[test]
fn test_hyphenate_all_uppercase() {
  let hyphenator = Hyphenator::new("en-us").unwrap();

  let word = "HYPHENATION";
  let points = hyphenator.hyphenate(word);

  // Uppercase words should be handled (case usually normalized internally)
  let _ = points; // May or may not find points
}

#[test]
fn test_hyphenate_mixed_case() {
  let hyphenator = Hyphenator::new("en-us").unwrap();

  let word = "HyPheNaTiOn";
  let _ = hyphenator.hyphenate(word); // Shouldn't panic
}

// ============================================================================
// Consistency Tests
// ============================================================================

#[test]
fn test_hyphenate_consistent_results() {
  let hyphenator = Hyphenator::new("en-us").unwrap();

  let word = "internationalization";

  // Multiple calls should return same results
  let points1 = hyphenator.hyphenate(word);
  let points2 = hyphenator.hyphenate(word);

  assert_eq!(points1, points2, "Hyphenation should be deterministic");
}

#[test]
fn test_hyphenator_clone() {
  let hyphenator = Hyphenator::new("en-us").unwrap();
  let cloned = hyphenator.clone();

  // Both should produce same results
  let word = "internationalization";
  let points1 = hyphenator.hyphenate(word);
  let points2 = cloned.hyphenate(word);

  assert_eq!(
    points1, points2,
    "Cloned hyphenator should behave identically"
  );
}

// ============================================================================
// Settings Impact Tests
// ============================================================================

#[test]
fn test_left_min_impact() {
  let default_hyphenator = Hyphenator::new("en-us").unwrap();
  let strict_hyphenator = Hyphenator::with_settings("en-us", 4, 2, 5).unwrap();

  let word = "hyphenation";

  let default_points = default_hyphenator.hyphenate(word);
  let strict_points = strict_hyphenator.hyphenate(word);

  // Stricter left_min should result in fewer or equal break points
  assert!(
    strict_points.len() <= default_points.len(),
    "Stricter settings should not add break points"
  );
}

#[test]
fn test_min_word_length_impact() {
  let short_min = Hyphenator::with_settings("en-us", 2, 2, 3).unwrap();
  let long_min = Hyphenator::with_settings("en-us", 2, 2, 10).unwrap();

  let word = "testing"; // 7 characters

  // Word should be hyphenatable with short min
  let short_can = short_min.can_hyphenate(word);

  // Word should not be hyphenatable with long min
  let long_can = long_min.can_hyphenate(word);

  assert!(short_can, "Short min should allow hyphenation");
  assert!(!long_can, "Long min should prevent hyphenation");
}

// ============================================================================
// Error Handling Tests
// ============================================================================

#[test]
fn test_error_message_contains_language() {
  let result = Hyphenator::new("xyz-lang");
  assert!(result.is_err());

  let error = result.unwrap_err();
  let error_string = format!("{}", error);
  assert!(
    error_string.contains("xyz-lang"),
    "Error should contain language code"
  );
}
