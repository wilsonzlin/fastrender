//! Integration tests for script itemization
//!
//! These tests verify the public API of the script itemizer module.
//! They complement the unit tests in the source file.

use fastrender::text::script::is_emoji;
use fastrender::text::script::is_skin_tone_modifier;
use fastrender::text::script::is_variation_selector;
use fastrender::text::script::is_zwj;
use fastrender::text::script::itemize_scripts;
use fastrender::text::script::ScriptRun;
use unicode_script::Script;

// ============================================================================
// Single Script Tests
// ============================================================================

#[test]
fn test_single_script_latin() {
  let text = "The quick brown fox jumps over the lazy dog";
  let runs = itemize_scripts(text);

  assert_eq!(runs.len(), 1);
  assert_eq!(runs[0].script, Script::Latin);
  assert_eq!(runs[0].start, 0);
  assert_eq!(runs[0].end, text.len());
  assert_eq!(runs[0].text_slice(text), text);
}

#[test]
fn test_single_script_cyrillic() {
  let text = "Съешь ещё этих мягких французских булок";
  let runs = itemize_scripts(text);

  assert_eq!(runs.len(), 1);
  assert_eq!(runs[0].script, Script::Cyrillic);
}

#[test]
fn test_single_script_greek() {
  let text = "Ο καιρός είναι ωραίος σήμερα";
  let runs = itemize_scripts(text);

  assert_eq!(runs.len(), 1);
  assert_eq!(runs[0].script, Script::Greek);
}

#[test]
fn test_single_script_hebrew() {
  let text = "שלום עולם איך אתם היום";
  let runs = itemize_scripts(text);

  assert_eq!(runs.len(), 1);
  assert_eq!(runs[0].script, Script::Hebrew);
}

#[test]
fn test_single_script_arabic() {
  let text = "مرحبا بالعالم كيف حالكم اليوم";
  let runs = itemize_scripts(text);

  assert_eq!(runs.len(), 1);
  assert_eq!(runs[0].script, Script::Arabic);
}

#[test]
fn test_single_script_han() {
  let text = "今天天气很好我们去公园吧";
  let runs = itemize_scripts(text);

  assert_eq!(runs.len(), 1);
  assert_eq!(runs[0].script, Script::Han);
}

#[test]
fn test_single_script_hiragana() {
  let text = "あいうえおかきくけこ";
  let runs = itemize_scripts(text);

  assert_eq!(runs.len(), 1);
  assert_eq!(runs[0].script, Script::Hiragana);
}

#[test]
fn test_single_script_katakana() {
  let text = "アイウエオカキクケコ";
  let runs = itemize_scripts(text);

  assert_eq!(runs.len(), 1);
  assert_eq!(runs[0].script, Script::Katakana);
}

#[test]
fn test_single_script_hangul() {
  let text = "안녕하세요 오늘 날씨가 좋아요";
  let runs = itemize_scripts(text);

  assert_eq!(runs.len(), 1);
  assert_eq!(runs[0].script, Script::Hangul);
}

#[test]
fn test_single_script_devanagari() {
  let text = "नमस्ते आप कैसे हैं आज";
  let runs = itemize_scripts(text);

  assert_eq!(runs.len(), 1);
  assert_eq!(runs[0].script, Script::Devanagari);
}

#[test]
fn test_single_script_thai() {
  let text = "สวัสดีครับวันนี้อากาศดี";
  let runs = itemize_scripts(text);

  assert_eq!(runs.len(), 1);
  assert_eq!(runs[0].script, Script::Thai);
}

#[test]
fn test_single_script_tamil() {
  let text = "வணக்கம் எப்படி இருக்கிறீர்கள்";
  let runs = itemize_scripts(text);

  assert_eq!(runs.len(), 1);
  assert_eq!(runs[0].script, Script::Tamil);
}

// ============================================================================
// Mixed Scripts Tests
// ============================================================================

#[test]
fn test_mixed_latin_hebrew() {
  let text = "Hello שלום World";
  let runs = itemize_scripts(text);

  assert_eq!(runs.len(), 3);
  assert_eq!(runs[0].script, Script::Latin);
  assert_eq!(runs[1].script, Script::Hebrew);
  assert_eq!(runs[2].script, Script::Latin);
}

#[test]
fn test_mixed_latin_arabic() {
  let text = "Hello مرحبا World";
  let runs = itemize_scripts(text);

  assert_eq!(runs.len(), 3);
  assert_eq!(runs[0].script, Script::Latin);
  assert_eq!(runs[1].script, Script::Arabic);
  assert_eq!(runs[2].script, Script::Latin);
}

#[test]
fn test_mixed_latin_cyrillic_greek() {
  let text = "Hello Привет Γειά";
  let runs = itemize_scripts(text);

  assert_eq!(runs.len(), 3);
  assert_eq!(runs[0].script, Script::Latin);
  assert_eq!(runs[1].script, Script::Cyrillic);
  assert_eq!(runs[2].script, Script::Greek);
}

#[test]
fn test_mixed_cjk_scripts() {
  let text = "你好こんにちは안녕";
  let runs = itemize_scripts(text);

  assert_eq!(runs.len(), 3);
  assert_eq!(runs[0].script, Script::Han);
  assert_eq!(runs[1].script, Script::Hiragana);
  assert_eq!(runs[2].script, Script::Hangul);
}

#[test]
fn test_mixed_all_scripts() {
  let text = "A Яб 你 あ ア 안 א ا";
  let runs = itemize_scripts(text);

  // Should have multiple runs for different scripts
  assert!(runs.len() >= 7);
}

// ============================================================================
// Common/Inherited Character Tests
// ============================================================================

#[test]
fn test_common_punctuation_with_latin() {
  let text = "Hello, world! How are you?";
  let runs = itemize_scripts(text);

  // Punctuation (Common) should be included with Latin
  assert_eq!(runs.len(), 1);
  assert_eq!(runs[0].script, Script::Latin);
}

#[test]
fn test_common_digits_with_latin() {
  let text = "Test 12345 numbers here";
  let runs = itemize_scripts(text);

  // Digits (Common) should be included with Latin
  assert_eq!(runs.len(), 1);
  assert_eq!(runs[0].script, Script::Latin);
}

#[test]
fn test_common_symbols_with_script() {
  let text = "Price: $100 + €50 = ???";
  let runs = itemize_scripts(text);

  // Currency symbols and punctuation are Common
  assert_eq!(runs.len(), 1);
}

#[test]
fn test_only_common_characters() {
  let text = "123 456 789";
  let runs = itemize_scripts(text);

  // All Common characters default to Latin
  assert_eq!(runs.len(), 1);
  assert_eq!(runs[0].script, Script::Latin);
}

#[test]
fn test_only_punctuation() {
  let text = "... !!! ???";
  let runs = itemize_scripts(text);

  assert_eq!(runs.len(), 1);
  assert_eq!(runs[0].script, Script::Latin);
}

#[test]
fn test_common_between_different_scripts() {
  let text = "Hello 123 שלום";
  let runs = itemize_scripts(text);

  // Common digits between Latin and Hebrew
  assert_eq!(runs.len(), 2);
}

// ============================================================================
// Emoji Tests
// ============================================================================

#[test]
fn test_emoji_with_latin() {
  let text = "Hello 👋 World";
  let runs = itemize_scripts(text);

  // Emoji is Common, should merge with Latin
  assert_eq!(runs.len(), 1);
  assert_eq!(runs[0].script, Script::Latin);
}

#[test]
fn test_multiple_emoji() {
  let text = "😀😃😄😁😆";
  let runs = itemize_scripts(text);

  assert_eq!(runs.len(), 1);
}

#[test]
fn test_emoji_sequence_zwj() {
  // Family emoji: man + ZWJ + woman + ZWJ + girl + ZWJ + boy
  let text = "👨\u{200D}👩\u{200D}👧\u{200D}👦";
  let runs = itemize_scripts(text);

  // Should be single run (all Common)
  assert_eq!(runs.len(), 1);
}

#[test]
fn test_emoji_with_skin_tone() {
  let text = "👋🏽";
  let runs = itemize_scripts(text);

  assert_eq!(runs.len(), 1);
}

#[test]
fn test_emoji_between_scripts() {
  let text = "Hello 😀 שלום";
  let runs = itemize_scripts(text);

  // Should have Latin and Hebrew runs
  assert_eq!(runs.len(), 2);
}

#[test]
fn test_is_emoji_function() {
  // Common emoji
  assert!(is_emoji('😀'));
  assert!(is_emoji('👋'));
  assert!(is_emoji('🎉'));
  assert!(is_emoji('❤'));
  assert!(is_emoji('🏠'));
  assert!(is_emoji('🚀'));

  // Not emoji
  assert!(!is_emoji('A'));
  assert!(!is_emoji('1'));
  assert!(!is_emoji(' '));
  assert!(!is_emoji('字'));
}

#[test]
fn test_is_zwj_function() {
  assert!(is_zwj('\u{200D}'));
  assert!(!is_zwj('A'));
  assert!(!is_zwj(' '));
  assert!(!is_zwj('😀'));
}

#[test]
fn test_is_variation_selector_function() {
  // Text style VS15
  assert!(is_variation_selector('\u{FE0E}'));
  // Emoji style VS16
  assert!(is_variation_selector('\u{FE0F}'));

  assert!(!is_variation_selector('A'));
  assert!(!is_variation_selector('😀'));
}

#[test]
fn test_is_skin_tone_modifier_function() {
  // All Fitzpatrick skin tone modifiers
  assert!(is_skin_tone_modifier('\u{1F3FB}')); // Light
  assert!(is_skin_tone_modifier('\u{1F3FC}')); // Medium-Light
  assert!(is_skin_tone_modifier('\u{1F3FD}')); // Medium
  assert!(is_skin_tone_modifier('\u{1F3FE}')); // Medium-Dark
  assert!(is_skin_tone_modifier('\u{1F3FF}')); // Dark

  assert!(!is_skin_tone_modifier('A'));
  assert!(!is_skin_tone_modifier('😀'));
}

// ============================================================================
// Edge Cases
// ============================================================================

#[test]
fn test_empty_string() {
  let runs = itemize_scripts("");
  assert!(runs.is_empty());
}

#[test]
fn test_single_character_latin() {
  let runs = itemize_scripts("A");
  assert_eq!(runs.len(), 1);
  assert_eq!(runs[0].start, 0);
  assert_eq!(runs[0].end, 1);
  assert_eq!(runs[0].script, Script::Latin);
}

#[test]
fn test_single_character_han() {
  let text = "字";
  let runs = itemize_scripts(text);

  assert_eq!(runs.len(), 1);
  assert_eq!(runs[0].start, 0);
  assert_eq!(runs[0].end, 3); // Chinese char is 3 bytes in UTF-8
  assert_eq!(runs[0].script, Script::Han);
}

#[test]
fn test_whitespace_only() {
  let text = "   \t\n   ";
  let runs = itemize_scripts(text);

  // Whitespace is Common, defaults to Latin
  assert_eq!(runs.len(), 1);
}

#[test]
fn test_newlines_in_text() {
  let text = "Hello\nWorld";
  let runs = itemize_scripts(text);

  assert_eq!(runs.len(), 1);
  assert_eq!(runs[0].script, Script::Latin);
}

// ============================================================================
// Byte Offset Tests
// ============================================================================

#[test]
fn test_byte_offsets_ascii() {
  let text = "Hello";
  let runs = itemize_scripts(text);

  assert_eq!(runs[0].start, 0);
  assert_eq!(runs[0].end, 5);
  assert_eq!(runs[0].len(), 5);
}

#[test]
fn test_byte_offsets_multibyte() {
  // Each character has different byte lengths:
  // A = 1 byte, я = 2 bytes, 字 = 3 bytes, 😀 = 4 bytes
  let text = "Aя字";
  let runs = itemize_scripts(text);

  // Should split by script
  assert!(runs.len() >= 2);

  // Total bytes should be 1 + 2 + 3 = 6
  let total_bytes: usize = runs.iter().map(|r| r.len()).sum();
  assert_eq!(total_bytes, 6);
}

#[test]
fn test_text_slice_extraction() {
  let text = "Hello שלום World";
  let runs = itemize_scripts(text);

  // Verify each run's text_slice matches expectations
  assert!(runs[0].text_slice(text).starts_with("Hello"));
  assert!(runs[1].text_slice(text).contains("שלום"));
}

#[test]
fn test_run_boundaries_valid() {
  let text = "Test 123 テスト 한글";
  let runs = itemize_scripts(text);

  // Verify runs are contiguous and cover entire text
  let mut expected_start = 0;
  for run in &runs {
    assert_eq!(
      run.start, expected_start,
      "Run should start where previous ended"
    );
    assert!(run.end > run.start, "Run should have positive length");
    expected_start = run.end;
  }

  // Last run should end at text length
  if let Some(last) = runs.last() {
    assert_eq!(last.end, text.len());
  }
}

// ============================================================================
// ScriptRun API Tests
// ============================================================================

#[test]
fn test_script_run_new() {
  let run = ScriptRun::new(10, 20, Script::Arabic);
  assert_eq!(run.start, 10);
  assert_eq!(run.end, 20);
  assert_eq!(run.script, Script::Arabic);
}

#[test]
fn test_script_run_len() {
  let run = ScriptRun::new(5, 15, Script::Latin);
  assert_eq!(run.len(), 10);
}

#[test]
fn test_script_run_is_empty() {
  let empty = ScriptRun::new(5, 5, Script::Latin);
  let non_empty = ScriptRun::new(0, 10, Script::Latin);

  assert!(empty.is_empty());
  assert!(!non_empty.is_empty());
}

#[test]
fn test_script_run_equality() {
  let run1 = ScriptRun::new(0, 10, Script::Latin);
  let run2 = ScriptRun::new(0, 10, Script::Latin);
  let run3 = ScriptRun::new(0, 10, Script::Cyrillic);

  assert_eq!(run1, run2);
  assert_ne!(run1, run3);
}

#[test]
fn test_script_run_clone() {
  let original = ScriptRun::new(0, 10, Script::Greek);
  let cloned = original.clone();

  assert_eq!(original, cloned);
}

#[test]
fn test_script_run_debug() {
  let run = ScriptRun::new(0, 10, Script::Hebrew);
  let debug_str = format!("{:?}", run);

  assert!(debug_str.contains("ScriptRun"));
  assert!(debug_str.contains("Hebrew"));
}

// ============================================================================
// Real-World Text Tests
// ============================================================================

#[test]
fn test_url_text() {
  let text = "Visit https://example.com/path?query=value for info";
  let runs = itemize_scripts(text);

  // URL contains only ASCII/Common, should be single Latin run
  assert_eq!(runs.len(), 1);
  assert_eq!(runs[0].script, Script::Latin);
}

#[test]
fn test_email_address() {
  let text = "Contact: user@example.com";
  let runs = itemize_scripts(text);

  assert_eq!(runs.len(), 1);
  assert_eq!(runs[0].script, Script::Latin);
}

#[test]
fn test_code_snippet() {
  let text = "function foo(x) { return x * 2; }";
  let runs = itemize_scripts(text);

  assert_eq!(runs.len(), 1);
  assert_eq!(runs[0].script, Script::Latin);
}

#[test]
fn test_mathematical_expression() {
  let text = "f(x) = x² + 2x + 1";
  let runs = itemize_scripts(text);

  // All Latin/Common
  assert_eq!(runs.len(), 1);
}

#[test]
fn test_phone_number() {
  let text = "Call: +1 (555) 123-4567";
  let runs = itemize_scripts(text);

  // All Common/Latin
  assert_eq!(runs.len(), 1);
}

#[test]
fn test_currency_amounts() {
  let text = "Total: $99.99 USD";
  let runs = itemize_scripts(text);

  assert_eq!(runs.len(), 1);
}

// ============================================================================
// Japanese Mixed Writing System Tests
// ============================================================================

#[test]
fn test_japanese_hiragana_katakana_mix() {
  let text = "ひらがなカタカナ";
  let runs = itemize_scripts(text);

  // Hiragana and Katakana are different Unicode scripts
  assert_eq!(runs.len(), 2);
  assert_eq!(runs[0].script, Script::Hiragana);
  assert_eq!(runs[1].script, Script::Katakana);
}

#[test]
fn test_japanese_with_kanji() {
  let text = "漢字";
  let runs = itemize_scripts(text);

  assert_eq!(runs.len(), 1);
  assert_eq!(runs[0].script, Script::Han);
}

#[test]
fn test_japanese_all_scripts() {
  // Kanji + Hiragana + Katakana
  let text = "日本語はひらがなとカタカナがあります";
  let runs = itemize_scripts(text);

  // Should have multiple runs for different scripts
  assert!(runs.len() >= 3);
}

// ============================================================================
// Stress Tests
// ============================================================================

#[test]
fn test_alternating_scripts() {
  // Rapidly alternating between Latin and Cyrillic
  let text = "AяBбCвDгEд";
  let runs = itemize_scripts(text);

  // Each character is a different script
  assert_eq!(runs.len(), 10);
}

#[test]
fn test_long_single_script() {
  let text = "a".repeat(10000);
  let runs = itemize_scripts(&text);

  assert_eq!(runs.len(), 1);
  assert_eq!(runs[0].len(), 10000);
}

#[test]
fn test_many_script_changes() {
  // Create text with many script changes
  let mut text = String::new();
  for _ in 0..100 {
    text.push_str("Hello");
    text.push_str("Привет");
  }

  let runs = itemize_scripts(&text);

  // Should have 200 runs (100 Latin + 100 Cyrillic)
  assert_eq!(runs.len(), 200);
}
