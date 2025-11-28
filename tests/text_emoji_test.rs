//! Integration tests for emoji detection module.
//!
//! These tests verify the emoji detection functionality from an external perspective,
//! testing the public API of the `fastrender::text::emoji` module.

use fastrender::text::emoji::{
    find_emoji_sequences, is_emoji, is_emoji_modifier, is_emoji_modifier_base, is_emoji_presentation,
    is_regional_indicator, is_zwj, EmojiSequenceType,
};

// =============================================================================
// Basic Emoji Detection Tests
// =============================================================================

#[test]
fn test_basic_emoji_face() {
    // Common face emoji
    assert!(is_emoji('😀')); // Grinning face
    assert!(is_emoji('😃')); // Grinning face with big eyes
    assert!(is_emoji('😄')); // Grinning face with smiling eyes
    assert!(is_emoji('😁')); // Beaming face with smiling eyes
    assert!(is_emoji('😆')); // Grinning squinting face
    assert!(is_emoji('😅')); // Grinning face with sweat
    assert!(is_emoji('🤣')); // Rolling on the floor laughing
    assert!(is_emoji('😂')); // Face with tears of joy
}

#[test]
fn test_basic_emoji_objects() {
    assert!(is_emoji('🚀')); // Rocket
    assert!(is_emoji('🔥')); // Fire
    assert!(is_emoji('💧')); // Droplet
    assert!(is_emoji('⭐')); // Star
    assert!(is_emoji('🌍')); // Earth globe Europe-Africa
    assert!(is_emoji('🎉')); // Party popper
    assert!(is_emoji('💎')); // Gem stone
    assert!(is_emoji('🎁')); // Wrapped gift
}

#[test]
fn test_basic_emoji_animals() {
    assert!(is_emoji('🐶')); // Dog face
    assert!(is_emoji('🐱')); // Cat face
    assert!(is_emoji('🦊')); // Fox
    assert!(is_emoji('🦁')); // Lion
    assert!(is_emoji('🐯')); // Tiger face
    assert!(is_emoji('🦄')); // Unicorn
}

#[test]
fn test_non_emoji_characters() {
    assert!(!is_emoji('A'));
    assert!(!is_emoji('a'));
    assert!(!is_emoji('Z'));
    assert!(!is_emoji('!'));
    assert!(!is_emoji('@'));
    assert!(!is_emoji(' '));
    assert!(!is_emoji('\n'));
    assert!(!is_emoji('\t'));
}

#[test]
fn test_non_emoji_unicode() {
    assert!(!is_emoji('中')); // Chinese character
    assert!(!is_emoji('日')); // Japanese character
    assert!(!is_emoji('한')); // Korean character
    assert!(!is_emoji('ñ')); // Spanish letter
    assert!(!is_emoji('ü')); // German umlaut
    assert!(!is_emoji('α')); // Greek letter
    assert!(!is_emoji('π')); // Greek pi
    assert!(!is_emoji('∑')); // Mathematical sum
}

// =============================================================================
// Emoji with Modifiers (Skin Tone) Tests
// =============================================================================

#[test]
fn test_skin_tone_modifiers() {
    assert!(is_emoji_modifier('\u{1F3FB}')); // Light skin tone
    assert!(is_emoji_modifier('\u{1F3FC}')); // Medium-light skin tone
    assert!(is_emoji_modifier('\u{1F3FD}')); // Medium skin tone
    assert!(is_emoji_modifier('\u{1F3FE}')); // Medium-dark skin tone
    assert!(is_emoji_modifier('\u{1F3FF}')); // Dark skin tone
}

#[test]
fn test_modifier_bases() {
    // Hand gestures that accept skin tone modifiers
    assert!(is_emoji_modifier_base('👋')); // Waving hand
    assert!(is_emoji_modifier_base('👍')); // Thumbs up
    assert!(is_emoji_modifier_base('👎')); // Thumbs down
    assert!(is_emoji_modifier_base('👏')); // Clapping hands

    // Non-modifier bases
    assert!(!is_emoji_modifier_base('🚀')); // Rocket can't have skin tone
    assert!(!is_emoji_modifier_base('🔥')); // Fire can't have skin tone
    assert!(!is_emoji_modifier_base('A')); // Letter
}

#[test]
fn test_emoji_sequence_with_skin_tone() {
    // Waving hand with medium skin tone: 👋🏽
    let seqs = find_emoji_sequences("👋🏽");
    assert_eq!(seqs.len(), 1);
    assert_eq!(seqs[0].chars.len(), 2);
    assert_eq!(seqs[0].chars[0], '👋');
    assert!(is_emoji_modifier(seqs[0].chars[1]));
    assert_eq!(seqs[0].sequence_type, EmojiSequenceType::WithModifier);
}

#[test]
fn test_multiple_skin_toned_emoji() {
    // Multiple hand emoji with different skin tones
    let seqs = find_emoji_sequences("👋🏻👋🏽👋🏿");
    assert_eq!(seqs.len(), 3);
    for seq in &seqs {
        assert_eq!(seq.sequence_type, EmojiSequenceType::WithModifier);
        assert_eq!(seq.chars.len(), 2);
    }
}

// =============================================================================
// ZWJ Sequence Tests
// =============================================================================

#[test]
fn test_zwj_character() {
    assert!(is_zwj('\u{200D}'));
    assert!(!is_zwj('a'));
    assert!(!is_zwj('😀'));
}

#[test]
fn test_zwj_sequence_couple() {
    // Man + ZWJ + Woman
    let seqs = find_emoji_sequences("👨‍👩");
    assert_eq!(seqs.len(), 1);
    assert_eq!(seqs[0].sequence_type, EmojiSequenceType::ZwjSequence);
    assert!(seqs[0].chars.contains(&'\u{200D}')); // Contains ZWJ
}

#[test]
fn test_zwj_sequence_family() {
    // Family with three members: 👨‍👩‍👧
    let seqs = find_emoji_sequences("👨‍👩‍👧");
    assert_eq!(seqs.len(), 1);
    assert_eq!(seqs[0].sequence_type, EmojiSequenceType::ZwjSequence);
}

#[test]
fn test_zwj_sequence_family_four() {
    // Family with four members: 👨‍👩‍👧‍👦
    let seqs = find_emoji_sequences("👨‍👩‍👧‍👦");
    assert_eq!(seqs.len(), 1);
    assert_eq!(seqs[0].sequence_type, EmojiSequenceType::ZwjSequence);
    // Should contain 4 people emoji + 3 ZWJ = 7 characters
    assert!(seqs[0].chars.len() >= 7);
}

#[test]
fn test_zwj_sequence_professional() {
    // Health worker: 🧑‍⚕️ (person + ZWJ + medical symbol + VS16)
    // This tests professional ZWJ sequences
    let text = "🧑‍⚕️";
    let seqs = find_emoji_sequences(text);
    assert!(!seqs.is_empty());
}

// =============================================================================
// Text vs Emoji Presentation Tests
// =============================================================================

#[test]
fn test_emoji_presentation_default_emoji() {
    // These default to emoji presentation
    assert!(is_emoji_presentation('😀')); // Face
    assert!(is_emoji_presentation('🚀')); // Rocket
    assert!(is_emoji_presentation('🔥')); // Fire
    assert!(is_emoji_presentation('💎')); // Gem
}

#[test]
fn test_text_presentation_default() {
    // These are emoji but default to text presentation
    // They need VS16 (U+FE0F) to render as emoji
    assert!(!is_emoji_presentation('#')); // Number sign
    assert!(!is_emoji_presentation('*')); // Asterisk
    assert!(!is_emoji_presentation('0')); // Digit zero
    assert!(!is_emoji_presentation('©')); // Copyright
    assert!(!is_emoji_presentation('®')); // Registered
}

#[test]
fn test_variation_selector_changes_presentation() {
    // Heart with VS16 (emoji presentation): ❤️
    let seqs = find_emoji_sequences("❤️");
    assert_eq!(seqs.len(), 1);
    // Should contain VS16
    assert!(seqs[0].chars.contains(&'\u{FE0F}'));
}

// =============================================================================
// Regional Indicators (Flags) Tests
// =============================================================================

#[test]
fn test_regional_indicators() {
    assert!(is_regional_indicator('🇦')); // Regional indicator A
    assert!(is_regional_indicator('🇧')); // Regional indicator B
    assert!(is_regional_indicator('🇺')); // Regional indicator U
    assert!(is_regional_indicator('🇸')); // Regional indicator S
    assert!(is_regional_indicator('🇿')); // Regional indicator Z

    // Regular letters are not regional indicators
    assert!(!is_regional_indicator('A'));
    assert!(!is_regional_indicator('U'));
    assert!(!is_regional_indicator('S'));
}

#[test]
fn test_flag_sequence_us() {
    // US flag: 🇺🇸
    let seqs = find_emoji_sequences("🇺🇸");
    assert_eq!(seqs.len(), 1);
    assert_eq!(seqs[0].chars.len(), 2);
    assert_eq!(seqs[0].sequence_type, EmojiSequenceType::FlagSequence);
}

#[test]
fn test_flag_sequence_gb() {
    // GB flag: 🇬🇧
    let seqs = find_emoji_sequences("🇬🇧");
    assert_eq!(seqs.len(), 1);
    assert_eq!(seqs[0].sequence_type, EmojiSequenceType::FlagSequence);
}

#[test]
fn test_flag_sequence_jp() {
    // Japan flag: 🇯🇵
    let seqs = find_emoji_sequences("🇯🇵");
    assert_eq!(seqs.len(), 1);
    assert_eq!(seqs[0].sequence_type, EmojiSequenceType::FlagSequence);
}

#[test]
fn test_multiple_flags() {
    // Multiple flags in sequence
    let seqs = find_emoji_sequences("🇺🇸🇬🇧🇯🇵🇩🇪🇫🇷");
    assert_eq!(seqs.len(), 5); // 5 flags
    for seq in &seqs {
        assert_eq!(seq.sequence_type, EmojiSequenceType::FlagSequence);
        assert_eq!(seq.chars.len(), 2); // Each flag is 2 regional indicators
    }
}

#[test]
fn test_single_regional_indicator() {
    // Single regional indicator (not a complete flag)
    let seqs = find_emoji_sequences("🇺");
    assert_eq!(seqs.len(), 1);
    assert_eq!(seqs[0].chars.len(), 1);
    // Should be detected but not as a flag sequence
    assert_eq!(seqs[0].sequence_type, EmojiSequenceType::Single);
}

// =============================================================================
// Keycap Sequence Tests
// =============================================================================

#[test]
fn test_keycap_sequence() {
    // Keycap one: 1️⃣ = 1 + VS16 + Combining Enclosing Keycap
    let seqs = find_emoji_sequences("1️⃣");
    assert_eq!(seqs.len(), 1);
    assert_eq!(seqs[0].sequence_type, EmojiSequenceType::KeycapSequence);
}

#[test]
fn test_keycap_sequence_hash() {
    // Keycap hash: #️⃣
    let seqs = find_emoji_sequences("#️⃣");
    assert_eq!(seqs.len(), 1);
    assert_eq!(seqs[0].sequence_type, EmojiSequenceType::KeycapSequence);
}

#[test]
fn test_keycap_sequence_asterisk() {
    // Keycap asterisk: *️⃣
    let seqs = find_emoji_sequences("*️⃣");
    assert_eq!(seqs.len(), 1);
    assert_eq!(seqs[0].sequence_type, EmojiSequenceType::KeycapSequence);
}

// =============================================================================
// Mixed Content Tests
// =============================================================================

#[test]
fn test_emoji_in_text() {
    let seqs = find_emoji_sequences("Hello 🚀 World");
    assert_eq!(seqs.len(), 1);
    assert_eq!(seqs[0].chars, vec!['🚀']);
}

#[test]
fn test_multiple_emoji_in_text() {
    let seqs = find_emoji_sequences("I ❤️ coding 🚀 in Rust 🦀");
    assert_eq!(seqs.len(), 3);
}

#[test]
fn test_emoji_at_start() {
    let seqs = find_emoji_sequences("🎉 Party time!");
    assert_eq!(seqs.len(), 1);
    assert_eq!(seqs[0].start, 0);
}

#[test]
fn test_emoji_at_end() {
    let seqs = find_emoji_sequences("Great job! 👍");
    assert_eq!(seqs.len(), 1);
}

#[test]
fn test_consecutive_emoji() {
    let seqs = find_emoji_sequences("🔥🔥🔥");
    assert_eq!(seqs.len(), 3);
}

#[test]
fn test_no_emoji() {
    let seqs = find_emoji_sequences("Hello World!");
    assert!(seqs.is_empty());
}

#[test]
fn test_empty_string() {
    let seqs = find_emoji_sequences("");
    assert!(seqs.is_empty());
}

// =============================================================================
// Byte Position Tests
// =============================================================================

#[test]
fn test_emoji_byte_positions() {
    let text = "Hi 🚀!";
    let seqs = find_emoji_sequences(text);
    assert_eq!(seqs.len(), 1);

    // "Hi " is 3 bytes, rocket emoji is 4 bytes UTF-8
    assert_eq!(seqs[0].start, 3);
    assert_eq!(seqs[0].end, 7);
    assert_eq!(seqs[0].byte_len(), 4);

    // Verify we can slice the original text
    assert_eq!(seqs[0].as_str(text), "🚀");
}

#[test]
fn test_emoji_sequence_byte_positions_with_modifier() {
    let text = "👋🏽";
    let seqs = find_emoji_sequences(text);
    assert_eq!(seqs.len(), 1);

    // Waving hand is 4 bytes, skin tone modifier is 4 bytes
    assert_eq!(seqs[0].start, 0);
    assert_eq!(seqs[0].byte_len(), 8);
    assert_eq!(seqs[0].as_str(text), "👋🏽");
}

#[test]
fn test_flag_byte_positions() {
    let text = "USA: 🇺🇸";
    let seqs = find_emoji_sequences(text);
    assert_eq!(seqs.len(), 1);

    // Each regional indicator is 4 bytes, so flag is 8 bytes
    assert_eq!(seqs[0].byte_len(), 8);
    assert_eq!(seqs[0].as_str(text), "🇺🇸");
}

// =============================================================================
// Edge Cases
// =============================================================================

#[test]
fn test_emoji_followed_by_text() {
    let seqs = find_emoji_sequences("🔥hot");
    assert_eq!(seqs.len(), 1);
    assert_eq!(seqs[0].chars, vec!['🔥']);
}

#[test]
fn test_text_followed_by_emoji() {
    let seqs = find_emoji_sequences("hot🔥");
    assert_eq!(seqs.len(), 1);
    assert_eq!(seqs[0].chars, vec!['🔥']);
}

#[test]
fn test_modifier_without_base() {
    // Skin tone modifier alone (unusual but valid)
    let seqs = find_emoji_sequences("\u{1F3FD}");
    assert_eq!(seqs.len(), 1);
}

#[test]
fn test_zwj_at_end() {
    // Dangling ZWJ at end
    let seqs = find_emoji_sequences("👨\u{200D}");
    // Should detect the emoji but ZWJ is ignored if not followed by emoji
    assert!(!seqs.is_empty());
}

#[test]
fn test_unicode_text_with_emoji() {
    // Mix of CJK text and emoji
    let seqs = find_emoji_sequences("你好 🌍 世界");
    assert_eq!(seqs.len(), 1);
    assert_eq!(seqs[0].chars, vec!['🌍']);
}

#[test]
fn test_arabic_text_with_emoji() {
    let seqs = find_emoji_sequences("مرحبا 👋 العالم");
    assert_eq!(seqs.len(), 1);
    assert_eq!(seqs[0].chars, vec!['👋']);
}

// =============================================================================
// EmojiSequence API Tests
// =============================================================================

#[test]
fn test_emoji_sequence_len() {
    let seqs = find_emoji_sequences("👋🏽");
    assert_eq!(seqs[0].len(), 2); // Base + modifier

    let seqs = find_emoji_sequences("🚀");
    assert_eq!(seqs[0].len(), 1); // Single

    let seqs = find_emoji_sequences("🇺🇸");
    assert_eq!(seqs[0].len(), 2); // Two regional indicators
}

#[test]
fn test_emoji_sequence_is_empty() {
    let seqs = find_emoji_sequences("🚀");
    assert!(!seqs[0].is_empty());
}

#[test]
fn test_emoji_sequence_type_variants() {
    // Single
    let seqs = find_emoji_sequences("🚀");
    assert_eq!(seqs[0].sequence_type, EmojiSequenceType::Single);

    // WithModifier
    let seqs = find_emoji_sequences("👋🏽");
    assert_eq!(seqs[0].sequence_type, EmojiSequenceType::WithModifier);

    // FlagSequence
    let seqs = find_emoji_sequences("🇺🇸");
    assert_eq!(seqs[0].sequence_type, EmojiSequenceType::FlagSequence);

    // ZwjSequence
    let seqs = find_emoji_sequences("👨‍👩");
    assert_eq!(seqs[0].sequence_type, EmojiSequenceType::ZwjSequence);

    // KeycapSequence
    let seqs = find_emoji_sequences("1️⃣");
    assert_eq!(seqs[0].sequence_type, EmojiSequenceType::KeycapSequence);
}
