//! Integration tests for line break opportunity finder
//!
//! These tests verify the line break module works correctly
//! with various text inputs and edge cases.

use fastrender::text::line_break::find_break_opportunities;
use fastrender::text::line_break::find_interior_breaks;
use fastrender::text::line_break::find_mandatory_breaks;
use fastrender::text::line_break::has_break_at;
use fastrender::text::line_break::BreakIterator;
use fastrender::text::line_break::BreakOpportunity;
use fastrender::text::line_break::BreakType;

// =============================================================================
// Basic English text tests
// =============================================================================

#[test]
fn test_english_sentence_breaks() {
  let text = "The quick brown fox jumps over the lazy dog.";
  let breaks = find_break_opportunities(text);

  // Should have multiple break opportunities (at spaces)
  assert!(
    breaks.len() >= 8,
    "Expected at least 8 breaks, got {}",
    breaks.len()
  );

  // All non-final breaks should be Allowed type
  let interior = find_interior_breaks(text);
  for brk in &interior {
    assert_eq!(brk.break_type, BreakType::Allowed);
  }
}

#[test]
fn test_simple_two_words() {
  let text = "Hello world";
  let breaks = find_break_opportunities(text);

  // Should have break after "Hello " (position 6) and at end (position 11)
  let break_positions: Vec<usize> = breaks.iter().map(|b| b.byte_offset).collect();
  assert!(break_positions.contains(&6), "Expected break at position 6");
  assert!(break_positions.contains(&11), "Expected break at end");
}

#[test]
fn test_no_breaks_in_single_word() {
  let text = "supercalifragilisticexpialidocious";
  let interior = find_interior_breaks(text);

  // No interior breaks in a single word without hyphens
  assert!(
    interior.is_empty(),
    "Single word should have no interior breaks"
  );
}

// =============================================================================
// Mandatory break tests (newlines)
// =============================================================================

#[test]
fn test_newline_creates_mandatory_break() {
  let text = "First line\nSecond line";
  let breaks = find_break_opportunities(text);

  // Find break after newline
  let mandatory = breaks.iter().find(|b| b.is_mandatory());
  assert!(
    mandatory.is_some(),
    "Should have mandatory break at newline"
  );

  // Position should be after "First line\n" (11 bytes)
  assert_eq!(mandatory.unwrap().byte_offset, 11);
}

#[test]
fn test_multiple_newlines() {
  let text = "Line1\nLine2\nLine3";
  let mandatory = find_mandatory_breaks(text);

  // Should have mandatory breaks at newlines (and possibly at end)
  assert!(mandatory.len() >= 2, "Expected at least 2 mandatory breaks");
}

#[test]
fn test_windows_crlf() {
  let text = "Line1\r\nLine2";
  let mandatory = find_mandatory_breaks(text);

  // CRLF should be treated as single mandatory break
  assert!(!mandatory.is_empty(), "CRLF should create mandatory break");
}

#[test]
fn test_trailing_newline() {
  let text = "Content\n";
  let mandatory = find_mandatory_breaks(text);

  assert!(
    !mandatory.is_empty(),
    "Trailing newline should create mandatory break"
  );
}

// =============================================================================
// Non-breaking characters tests
// =============================================================================

#[test]
fn test_non_breaking_space_prevents_break() {
  let text = "100\u{00A0}km"; // 100 km with NBSP
  let interior = find_interior_breaks(text);

  // NBSP should prevent breaks, so no interior breaks
  assert!(interior.is_empty(), "NBSP should prevent interior breaks");
}

#[test]
fn test_word_joiner_prevents_break() {
  let text = "don\u{2060}t"; // don't with word joiner
  let interior = find_interior_breaks(text);

  // Word joiner should prevent breaks
  assert!(
    interior.is_empty(),
    "Word joiner should prevent interior breaks"
  );
}

// =============================================================================
// CJK text tests
// =============================================================================

#[test]
fn test_chinese_characters_break() {
  let text = "你好世界"; // Hello World in Chinese
  let breaks = find_break_opportunities(text);

  // Chinese text allows breaks between characters
  // 4 characters = at least 3 interior break opportunities + 1 end
  assert!(
    breaks.len() >= 4,
    "Chinese text should have multiple breaks"
  );
}

#[test]
fn test_japanese_hiragana_breaks() {
  let text = "こんにちは"; // Hello in Japanese hiragana
  let breaks = find_break_opportunities(text);

  // Japanese hiragana allows breaks between characters
  assert!(
    breaks.len() >= 4,
    "Japanese text should have multiple breaks"
  );
}

#[test]
fn test_korean_breaks() {
  let text = "안녕하세요"; // Hello in Korean
  let breaks = find_break_opportunities(text);

  // Korean allows breaks between syllables
  assert!(breaks.len() >= 4, "Korean text should have multiple breaks");
}

#[test]
fn test_mixed_english_chinese() {
  let text = "Hello世界world";
  let breaks = find_break_opportunities(text);

  // Should have breaks around CJK portion
  assert!(breaks.len() >= 3, "Mixed text should have multiple breaks");
}

// =============================================================================
// Special characters tests
// =============================================================================

#[test]
fn test_hyphen_allows_break() {
  let text = "self-aware";
  let breaks = find_break_opportunities(text);

  // Hyphen allows break after it (position 5)
  let has_hyphen_break = breaks.iter().any(|b| b.byte_offset == 5);
  assert!(has_hyphen_break, "Should have break after hyphen");
}

#[test]
fn test_soft_hyphen_allows_break() {
  let text = "super\u{00AD}man"; // soft hyphen
  let interior = find_interior_breaks(text);

  // Soft hyphen allows break
  assert!(!interior.is_empty(), "Soft hyphen should allow break");
}

#[test]
fn test_zero_width_space_allows_break() {
  let text = "word\u{200B}word"; // zero-width space
  let interior = find_interior_breaks(text);

  // ZWSP allows break
  assert!(!interior.is_empty(), "ZWSP should allow break");
}

// =============================================================================
// URL and path tests
// =============================================================================

#[test]
fn test_url_has_breaks() {
  let text = "https://example.com/path/to/resource";
  let interior = find_interior_breaks(text);

  // URLs should have break opportunities
  assert!(!interior.is_empty(), "URL should have break opportunities");
}

#[test]
fn test_email_has_breaks() {
  let text = "user@example.com";
  let breaks = find_break_opportunities(text);

  // Email addresses have at least one break (at end of string)
  // UAX #14 may or may not provide interior breaks in email addresses
  assert!(!breaks.is_empty(), "Email should have at least one break");
}

// =============================================================================
// Emoji tests
// =============================================================================

#[test]
fn test_simple_emoji_surrounded() {
  let text = "Hi 👋 there";
  let breaks = find_break_opportunities(text);

  // Should have breaks around emoji (at spaces)
  assert!(
    breaks.len() >= 3,
    "Text with emoji should have multiple breaks"
  );
}

#[test]
fn test_emoji_sequence_no_interior_break() {
  let text = "👨‍👩‍👧‍👦"; // Family emoji with ZWJ
  let interior = find_interior_breaks(text);

  // ZWJ sequences should not have interior breaks
  assert!(
    interior.is_empty(),
    "ZWJ emoji sequence should have no interior breaks"
  );
}

#[test]
fn test_flag_emoji_no_interior_break() {
  let text = "🇺🇸"; // US flag
  let interior = find_interior_breaks(text);

  // Flag should not break between regional indicators
  assert!(
    interior.is_empty(),
    "Flag emoji should have no interior breaks"
  );
}

// =============================================================================
// API tests
// =============================================================================

#[test]
fn test_break_opportunity_constructors() {
  let mandatory = BreakOpportunity::mandatory(10);
  assert_eq!(mandatory.byte_offset, 10);
  assert_eq!(mandatory.break_type, BreakType::Mandatory);
  assert!(mandatory.is_mandatory());

  let allowed = BreakOpportunity::allowed(20);
  assert_eq!(allowed.byte_offset, 20);
  assert_eq!(allowed.break_type, BreakType::Allowed);
  assert!(allowed.is_allowed());
}

#[test]
fn test_has_break_at_function() {
  let text = "Hello world";

  // Has break after space (position 6)
  assert_eq!(has_break_at(text, 6), Some(BreakType::Allowed));

  // No break in middle of word
  assert_eq!(has_break_at(text, 3), None);

  // Has break at end (may be Allowed or Mandatory depending on context)
  assert!(has_break_at(text, 11).is_some(), "Should have break at end");
}

#[test]
fn test_break_iterator() {
  let text = "Hello world";
  let from_iter: Vec<_> = BreakIterator::new(text).collect();
  let from_fn = find_break_opportunities(text);

  assert_eq!(from_iter, from_fn, "Iterator should match function");
}

#[test]
fn test_find_mandatory_breaks() {
  let text = "Line1\nLine2 and more\nLine3";
  let mandatory = find_mandatory_breaks(text);

  // Should have mandatory breaks at newlines (and possibly at end)
  assert!(mandatory.len() >= 2, "Expected at least 2 mandatory breaks");
  for brk in &mandatory {
    assert!(brk.is_mandatory());
  }
}

#[test]
fn test_find_interior_breaks() {
  let text = "Hello world";
  let interior = find_interior_breaks(text);

  // Should not include final break at end of text
  assert_eq!(interior.len(), 1);
  assert_eq!(interior[0].byte_offset, 6);
}

// =============================================================================
// Edge cases
// =============================================================================

#[test]
fn test_empty_string() {
  let breaks = find_break_opportunities("");
  assert!(breaks.is_empty(), "Empty string should have no breaks");
}

#[test]
fn test_single_character() {
  let breaks = find_break_opportunities("a");
  assert_eq!(breaks.len(), 1, "Single char should have one break at end");
}

#[test]
fn test_only_spaces() {
  let text = "     ";
  let breaks = find_break_opportunities(text);

  // Spaces should have at least one break opportunity (at end)
  // UAX #14 may not provide breaks between spaces without content
  assert!(!breaks.is_empty(), "Spaces should have at least one break");
}

#[test]
fn test_only_newlines() {
  let text = "\n\n\n";
  let mandatory = find_mandatory_breaks(text);

  // Each newline creates a mandatory break
  assert_eq!(mandatory.len(), 3);
}

#[test]
fn test_unicode_byte_boundaries() {
  let text = "Hello 世界 world";
  let breaks = find_break_opportunities(text);

  // All breaks should be at valid UTF-8 boundaries
  for brk in &breaks {
    assert!(
      text.is_char_boundary(brk.byte_offset),
      "Break at {} is not a char boundary",
      brk.byte_offset
    );
  }
}

#[test]
fn test_breaks_are_sorted() {
  let text = "The quick brown fox jumps";
  let breaks = find_break_opportunities(text);

  // Breaks should be in ascending order
  let mut prev = 0;
  for brk in &breaks {
    assert!(
      brk.byte_offset >= prev,
      "Breaks not sorted: {} < {}",
      brk.byte_offset,
      prev
    );
    prev = brk.byte_offset;
  }
}

#[test]
fn test_breaks_within_bounds() {
  let text = "Test string here";
  let breaks = find_break_opportunities(text);

  for brk in &breaks {
    assert!(
      brk.byte_offset <= text.len(),
      "Break {} exceeds text length {}",
      brk.byte_offset,
      text.len()
    );
  }
}

// =============================================================================
// Specific character class tests
// =============================================================================

#[test]
fn test_opening_bracket() {
  let text = "Hello (world)";
  let _breaks = find_break_opportunities(text);

  // Should have break before opening bracket
  let interior = find_interior_breaks(text);
  assert!(!interior.is_empty());
}

#[test]
fn test_numbers_dont_break_internally() {
  let text = "12345";
  let interior = find_interior_breaks(text);

  // Numbers should not have interior breaks
  assert!(interior.is_empty(), "Numbers should not break internally");
}

#[test]
fn test_currency_stays_with_number() {
  let text = "$100";
  let interior = find_interior_breaks(text);

  // Currency symbol should stay with number
  assert!(interior.is_empty(), "Currency should not break from number");
}

#[test]
fn test_percent_stays_with_number() {
  let text = "100%";
  let interior = find_interior_breaks(text);

  // Percent should stay with number
  assert!(interior.is_empty(), "Percent should not break from number");
}

// =============================================================================
// Real-world text tests
// =============================================================================

#[test]
fn test_paragraph_text() {
  let text = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. \
                Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.";
  let breaks = find_break_opportunities(text);

  // Long paragraph should have many break opportunities
  assert!(breaks.len() >= 15, "Paragraph should have many breaks");
}

#[test]
fn test_mixed_script_paragraph() {
  let text = "English text, 日本語テキスト, and more English.";
  let breaks = find_break_opportunities(text);

  // Mixed script should have breaks in both scripts
  assert!(breaks.len() >= 10, "Mixed script should have many breaks");
}

#[test]
fn test_code_snippet() {
  let text = "function_name(arg1, arg2)";
  let breaks = find_break_opportunities(text);

  // Code should have some break opportunities
  assert!(!breaks.is_empty());
}
