//! Emoji detection and handling
//!
//! This module provides Unicode-compliant emoji detection based on
//! Unicode Technical Standard #51 (Unicode Emoji).
//!
//! # Overview
//!
//! Emoji detection is essential for:
//! - Selecting appropriate fonts (color emoji fonts)
//! - Handling emoji sequences correctly (ZWJ sequences, skin tones)
//! - Proper text segmentation and rendering
//!
//! # Unicode Properties
//!
//! This module implements detection based on these Unicode properties:
//! - **Emoji** - Characters recommended for use as emoji
//! - **Emoji_Presentation** - Characters that default to emoji presentation
//! - **Emoji_Modifier_Base** - Characters that can receive skin tone modifiers
//! - **Emoji_Modifier** - Skin tone modifier characters (Fitzpatrick scales)
//! - **Emoji_Component** - Characters used in emoji sequences
//!
//! # References
//!
//! - Unicode Technical Standard #51: <https://www.unicode.org/reports/tr51/>
//! - Unicode Emoji Data: <https://unicode.org/Public/emoji/latest/>

/// An emoji sequence found in text
///
/// Emoji can be composed of multiple codepoints:
/// - Base emoji + skin tone modifier (e.g., 👋🏽)
/// - ZWJ sequences (e.g., 👨‍👩‍👧‍👦)
/// - Flag sequences using regional indicators (e.g., 🇺🇸)
/// - Keycap sequences (e.g., 1️⃣)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EmojiSequence {
  /// Start byte index in the original text
  pub start: usize,
  /// End byte index in the original text (exclusive)
  pub end: usize,
  /// Characters that make up this emoji sequence
  pub chars: Vec<char>,
  /// The type of emoji sequence
  pub sequence_type: EmojiSequenceType,
}

impl EmojiSequence {
  /// Returns the text slice represented by this sequence
  pub fn as_str<'a>(&self, text: &'a str) -> &'a str {
    &text[self.start..self.end]
  }

  /// Returns the number of Unicode codepoints in the sequence
  pub fn len(&self) -> usize {
    self.chars.len()
  }

  /// Returns true if the sequence is empty
  pub fn is_empty(&self) -> bool {
    self.chars.is_empty()
  }

  /// Returns the byte length of the sequence
  pub fn byte_len(&self) -> usize {
    self.end - self.start
  }
}

/// Type of emoji sequence
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EmojiSequenceType {
  /// Single emoji character
  Single,
  /// Emoji with skin tone modifier (e.g., 👋🏽)
  WithModifier,
  /// ZWJ (Zero Width Joiner) sequence (e.g., 👨‍👩‍👧)
  ZwjSequence,
  /// Flag sequence using regional indicators (e.g., 🇺🇸)
  FlagSequence,
  /// Keycap sequence (e.g., 1️⃣)
  KeycapSequence,
  /// Tag sequence (e.g., subdivision flags 🏴󠁧󠁢󠁥󠁮󠁧󠁿)
  TagSequence,
}

// =============================================================================
// Core Emoji Detection Functions
// =============================================================================

/// Check if a character has the Unicode Emoji property
///
/// This returns true for characters that are recommended for use as emoji,
/// including those that may have text presentation by default.
///
/// # Examples
///
/// ```
/// use fastrender::text::emoji::is_emoji;
///
/// assert!(is_emoji('😀'));  // Grinning face
/// assert!(is_emoji('🚀'));  // Rocket
/// assert!(is_emoji('❤'));   // Heavy heart (dingbat)
/// assert!(is_emoji('#'));   // Number sign (keycap base)
/// assert!(!is_emoji('A'));  // Regular letter
/// ```
pub fn is_emoji(c: char) -> bool {
  let cp = c as u32;

  // Check various emoji ranges based on Unicode 15.0 Emoji data

  // Emoticons (U+1F600 - U+1F64F)
  if (0x1f600..=0x1f64f).contains(&cp) {
    return true;
  }

  // Miscellaneous Symbols and Pictographs (U+1F300 - U+1F5FF)
  if (0x1f300..=0x1f5ff).contains(&cp) {
    return true;
  }

  // Transport and Map Symbols (U+1F680 - U+1F6FF)
  if (0x1f680..=0x1f6ff).contains(&cp) {
    return true;
  }

  // Supplemental Symbols and Pictographs (U+1F900 - U+1F9FF)
  if (0x1f900..=0x1f9ff).contains(&cp) {
    return true;
  }

  // Symbols and Pictographs Extended-A (U+1FA00 - U+1FA6F)
  if (0x1fa00..=0x1fa6f).contains(&cp) {
    return true;
  }

  // Symbols and Pictographs Extended-B (U+1FA70 - U+1FAFF)
  if (0x1fa70..=0x1faff).contains(&cp) {
    return true;
  }

  // Dingbats (U+2700 - U+27BF) - many are emoji
  if (0x2700..=0x27bf).contains(&cp) {
    return true;
  }

  // Miscellaneous Symbols (U+2600 - U+26FF)
  if (0x2600..=0x26ff).contains(&cp) {
    return true;
  }

  // Miscellaneous Symbols and Arrows (U+2B00 - U+2BFF) - includes ⭐ star
  if (0x2b00..=0x2bff).contains(&cp) {
    return true;
  }

  // Regional Indicator Symbols (U+1F1E0 - U+1F1FF) - for flags
  if (0x1f1e0..=0x1f1ff).contains(&cp) {
    return true;
  }

  // Skin tone modifiers (U+1F3FB - U+1F3FF)
  if (0x1f3fb..=0x1f3ff).contains(&cp) {
    return true;
  }

  // Zero Width Joiner (used in emoji sequences)
  if cp == 0x200d {
    return true;
  }

  // Variation selectors
  if cp == 0xfe0e || cp == 0xfe0f {
    return true;
  }

  // Keycap base characters (0-9, *, #)
  if matches!(c, '0'..='9' | '*' | '#') {
    return true;
  }

  // Combining Enclosing Keycap
  if cp == 0x20e3 {
    return true;
  }

  // Copyright, Registered, Trade Mark
  if matches!(cp, 0x00a9 | 0x00ae | 0x2122) {
    return true;
  }

  // Various single-character emoji
  if is_misc_emoji(cp) {
    return true;
  }

  // Tag characters (for subdivision flags)
  if (0xe0020..=0xe007f).contains(&cp) {
    return true;
  }

  false
}

/// Check if a character has the Emoji_Presentation property
///
/// Characters with this property are displayed as emoji by default
/// (without requiring a variation selector). Other emoji characters
/// may default to text presentation.
///
/// # Examples
///
/// ```
/// use fastrender::text::emoji::is_emoji_presentation;
///
/// assert!(is_emoji_presentation('😀'));  // Always emoji
/// assert!(is_emoji_presentation('🚀'));  // Always emoji
/// assert!(!is_emoji_presentation('#'));  // Text by default
/// assert!(!is_emoji_presentation('©'));  // Text by default
/// ```
pub fn is_emoji_presentation(c: char) -> bool {
  let cp = c as u32;

  // Most emoji in these ranges default to emoji presentation

  // Emoticons (all default to emoji presentation)
  if (0x1f600..=0x1f64f).contains(&cp) {
    return true;
  }

  // Most of Miscellaneous Symbols and Pictographs
  if (0x1f300..=0x1f5ff).contains(&cp) {
    return true;
  }

  // Transport and Map Symbols
  if (0x1f680..=0x1f6ff).contains(&cp) {
    return true;
  }

  // Supplemental Symbols and Pictographs
  if (0x1f900..=0x1f9ff).contains(&cp) {
    return true;
  }

  // Symbols and Pictographs Extended-A
  if (0x1fa00..=0x1fa6f).contains(&cp) {
    return true;
  }

  // Symbols and Pictographs Extended-B
  if (0x1fa70..=0x1faff).contains(&cp) {
    return true;
  }

  // Regional Indicators (for flags)
  if (0x1f1e0..=0x1f1ff).contains(&cp) {
    return true;
  }

  // Skin tone modifiers
  if (0x1f3fb..=0x1f3ff).contains(&cp) {
    return true;
  }

  // Some Miscellaneous Symbols that default to emoji
  // These are specific codepoints that have Emoji_Presentation=Yes
  if is_emoji_presentation_misc_symbols(cp) {
    return true;
  }

  // Some Dingbats that default to emoji presentation
  if is_emoji_presentation_dingbats(cp) {
    return true;
  }

  false
}

/// Check if a character is an emoji modifier (skin tone)
///
/// These are the Fitzpatrick skin tone modifiers that can be appended
/// to emoji to change their skin tone.
///
/// # Examples
///
/// ```
/// use fastrender::text::emoji::is_emoji_modifier;
///
/// assert!(is_emoji_modifier('\u{1F3FB}')); // Light skin tone
/// assert!(is_emoji_modifier('\u{1F3FF}')); // Dark skin tone
/// assert!(!is_emoji_modifier('A'));
/// ```
pub fn is_emoji_modifier(c: char) -> bool {
  let cp = c as u32;
  (0x1f3fb..=0x1f3ff).contains(&cp)
}

/// Check if a character is an emoji modifier base
///
/// These are emoji that can be modified with skin tone modifiers.
/// Examples include hand gestures, people emoji, etc.
///
/// # Examples
///
/// ```
/// use fastrender::text::emoji::is_emoji_modifier_base;
///
/// assert!(is_emoji_modifier_base('👋')); // Waving hand
/// assert!(is_emoji_modifier_base('👍')); // Thumbs up
/// assert!(!is_emoji_modifier_base('🚀')); // Rocket (not a modifier base)
/// ```
pub fn is_emoji_modifier_base(c: char) -> bool {
  let cp = c as u32;

  // Hand gestures (U+1F44x range)
  if (0x1f44a..=0x1f44f).contains(&cp) {
    return true;
  }

  // More hand signs
  if matches!(
    cp,
    0x1F440 // Eyes
        | 0x1F442 // Ear
        | 0x1F443 // Nose
        | 0x1F446..=0x1f450 // Pointing, hands
  ) {
    return true;
  }

  // People and body parts
  if (0x1f466..=0x1f478).contains(&cp) {
    return true;
  }

  // More people emoji
  if (0x1f47c..=0x1f47c).contains(&cp)
    || (0x1f481..=0x1f487).contains(&cp)
    || (0x1f4aa..=0x1f4aa).contains(&cp)
  {
    return true;
  }

  // People (U+1F574-U+1F575)
  if matches!(cp, 0x1f574..=0x1f575) {
    return true;
  }

  // Person walking, standing, etc.
  if (0x1f6a3..=0x1f6a3).contains(&cp)
    || (0x1f6b4..=0x1f6b6).contains(&cp)
    || (0x1f6c0..=0x1f6c0).contains(&cp)
    || (0x1f6cc..=0x1f6cc).contains(&cp)
  {
    return true;
  }

  // Hands and gestures (Supplemental)
  if (0x1f90c..=0x1f90f).contains(&cp)
    || (0x1f918..=0x1f91f).contains(&cp)
    || (0x1f926..=0x1f926).contains(&cp)
    || (0x1f930..=0x1f939).contains(&cp)
    || (0x1f93c..=0x1f93e).contains(&cp)
    || (0x1f977..=0x1f977).contains(&cp)
    || (0x1f9b5..=0x1f9b6).contains(&cp)
    || (0x1f9b8..=0x1f9b9).contains(&cp)
    || (0x1f9bb..=0x1f9bb).contains(&cp)
    || (0x1f9cd..=0x1f9cf).contains(&cp)
    || (0x1f9d1..=0x1f9dd).contains(&cp)
    || (0x1fac3..=0x1fac5).contains(&cp)
    || (0x1faf0..=0x1faf8).contains(&cp)
  {
    return true;
  }

  // Index pointing (various)
  if matches!(cp, 0x261d | 0x26f9 | 0x270a..=0x270d) {
    return true;
  }

  false
}

/// Check if a character is a regional indicator
///
/// Regional indicators are used in pairs to form flag emoji.
///
/// # Examples
///
/// ```
/// use fastrender::text::emoji::is_regional_indicator;
///
/// assert!(is_regional_indicator('🇺')); // Regional indicator U
/// assert!(is_regional_indicator('🇸')); // Regional indicator S
/// assert!(!is_regional_indicator('U')); // Regular letter
/// ```
pub fn is_regional_indicator(c: char) -> bool {
  let cp = c as u32;
  (0x1f1e0..=0x1f1ff).contains(&cp)
}

/// Check if a character is a Zero Width Joiner (ZWJ)
///
/// ZWJ is used to join multiple emoji into a single glyph.
pub fn is_zwj(c: char) -> bool {
  c == '\u{200D}'
}

/// Check if a character is a variation selector
///
/// - U+FE0E (VS15): Text presentation selector
/// - U+FE0F (VS16): Emoji presentation selector
pub fn is_variation_selector(c: char) -> bool {
  c == '\u{FE0E}' || c == '\u{FE0F}'
}

/// Check if a character is the emoji presentation selector (VS16)
pub fn is_emoji_variation_selector(c: char) -> bool {
  c == '\u{FE0F}'
}

/// Check if a character is the text presentation selector (VS15)
pub fn is_text_variation_selector(c: char) -> bool {
  c == '\u{FE0E}'
}

/// Check if a character is a combining enclosing keycap
pub fn is_combining_enclosing_keycap(c: char) -> bool {
  c == '\u{20E3}'
}

/// Check if a character is a keycap base (can form keycap sequences)
///
/// Keycap bases are: 0-9, *, #
pub fn is_keycap_base(c: char) -> bool {
  matches!(c, '0'..='9' | '*' | '#')
}

/// Check if a character is a tag character (for subdivision flags)
pub fn is_tag_character(c: char) -> bool {
  let cp = c as u32;
  (0xe0020..=0xe007f).contains(&cp)
}

/// Check if a character is the cancel tag (ends tag sequences)
pub fn is_cancel_tag(c: char) -> bool {
  c == '\u{E007F}'
}

/// Check if a character is the black flag (starts tag sequences)
pub fn is_tag_base(c: char) -> bool {
  c == '\u{1F3F4}'
}

// =============================================================================
// Emoji Sequence Detection
// =============================================================================

/// Find all emoji sequences in text
///
/// This function identifies emoji sequences including:
/// - Single emoji characters
/// - Emoji with skin tone modifiers
/// - ZWJ (Zero Width Joiner) sequences
/// - Flag sequences (regional indicators)
/// - Keycap sequences
/// - Tag sequences (subdivision flags)
///
/// # Examples
///
/// ```
/// use fastrender::text::emoji::find_emoji_sequences;
///
/// // Single emoji
/// let seqs = find_emoji_sequences("Hello 🚀 World");
/// assert_eq!(seqs.len(), 1);
/// assert_eq!(seqs[0].chars, vec!['🚀']);
///
/// // Emoji with skin tone
/// let seqs = find_emoji_sequences("👋🏽");
/// assert_eq!(seqs.len(), 1);
/// assert_eq!(seqs[0].chars.len(), 2);
///
/// // ZWJ sequence (family)
/// let seqs = find_emoji_sequences("👨‍👩‍👧");
/// assert_eq!(seqs.len(), 1);
/// ```
pub fn find_emoji_sequences(text: &str) -> Vec<EmojiSequence> {
  let mut sequences = Vec::new();
  let mut chars = text.char_indices().peekable();

  while let Some((start_idx, ch)) = chars.next() {
    // Check for keycap sequences first (0-9, *, #)
    if is_keycap_base(ch) {
      if let Some(seq) = try_parse_keycap_sequence(&mut chars, start_idx, ch, text) {
        sequences.push(seq);
        continue;
      }
      // Not a keycap sequence, skip (not emoji on its own without VS16)
      continue;
    }

    // Check for tag sequences (subdivision flags like 🏴󠁧󠁢󠁥󠁮󠁧󠁿)
    if is_tag_base(ch) {
      if let Some(seq) = try_parse_tag_sequence(&mut chars, start_idx, ch, text) {
        sequences.push(seq);
        continue;
      }
      // Just the black flag alone
      sequences.push(EmojiSequence {
        start: start_idx,
        end: start_idx + ch.len_utf8(),
        chars: vec![ch],
        sequence_type: EmojiSequenceType::Single,
      });
      continue;
    }

    // Check for regional indicator (potential flag sequence)
    if is_regional_indicator(ch) {
      if let Some(seq) = try_parse_flag_sequence(&mut chars, start_idx, ch, text) {
        sequences.push(seq);
        continue;
      }
      // Single regional indicator - not meaningful alone but still emoji
      sequences.push(EmojiSequence {
        start: start_idx,
        end: start_idx + ch.len_utf8(),
        chars: vec![ch],
        sequence_type: EmojiSequenceType::Single,
      });
      continue;
    }

    // Skip non-emoji characters
    if !is_emoji(ch) || is_variation_selector(ch) || is_zwj(ch) {
      continue;
    }

    // Start building an emoji sequence
    let mut seq_chars = vec![ch];
    let mut end_idx = start_idx + ch.len_utf8();
    let mut seq_type = EmojiSequenceType::Single;

    // Look for continuation (modifiers, ZWJ, variation selectors)
    while let Some(&(next_idx, next_ch)) = chars.peek() {
      // Variation selector
      if is_variation_selector(next_ch) {
        seq_chars.push(next_ch);
        end_idx = next_idx + next_ch.len_utf8();
        chars.next();
        continue;
      }

      // Skin tone modifier
      if is_emoji_modifier(next_ch) && can_take_modifier(&seq_chars) {
        seq_chars.push(next_ch);
        end_idx = next_idx + next_ch.len_utf8();
        seq_type = EmojiSequenceType::WithModifier;
        chars.next();
        continue;
      }

      // ZWJ sequence
      if is_zwj(next_ch) {
        // Look ahead for emoji after ZWJ
        chars.next(); // consume ZWJ

        if let Some(&(after_zwj_idx, after_zwj_ch)) = chars.peek() {
          if is_emoji(after_zwj_ch) && !is_variation_selector(after_zwj_ch) {
            seq_chars.push(next_ch); // Add ZWJ
            seq_chars.push(after_zwj_ch);
            end_idx = after_zwj_idx + after_zwj_ch.len_utf8();
            seq_type = EmojiSequenceType::ZwjSequence;
            chars.next(); // consume emoji after ZWJ
            continue;
          }
        }

        // ZWJ not followed by emoji - we already consumed it, so just break
        break;
      }

      // No more continuations
      break;
    }

    sequences.push(EmojiSequence {
      start: start_idx,
      end: end_idx,
      chars: seq_chars,
      sequence_type: seq_type,
    });
  }

  sequences
}

/// Try to parse a keycap sequence (e.g., 1️⃣)
#[allow(unused_assignments)]
fn try_parse_keycap_sequence(
  chars: &mut std::iter::Peekable<std::str::CharIndices>,
  start_idx: usize,
  base: char,
  _text: &str,
) -> Option<EmojiSequence> {
  let mut seq_chars = vec![base];
  let mut end_idx = start_idx + base.len_utf8();

  // Check for VS16 (emoji presentation)
  if let Some(&(_, next_ch)) = chars.peek() {
    if is_emoji_variation_selector(next_ch) {
      seq_chars.push(next_ch);
      end_idx += next_ch.len_utf8();
      chars.next();
    }
  }

  // Check for combining enclosing keycap
  if let Some(&(idx, next_ch)) = chars.peek() {
    if is_combining_enclosing_keycap(next_ch) {
      seq_chars.push(next_ch);
      end_idx = idx + next_ch.len_utf8();
      chars.next();

      return Some(EmojiSequence {
        start: start_idx,
        end: end_idx,
        chars: seq_chars,
        sequence_type: EmojiSequenceType::KeycapSequence,
      });
    }
  }

  // Not a complete keycap sequence
  None
}

/// Try to parse a flag sequence (two regional indicators)
fn try_parse_flag_sequence(
  chars: &mut std::iter::Peekable<std::str::CharIndices>,
  start_idx: usize,
  first: char,
  _text: &str,
) -> Option<EmojiSequence> {
  if let Some(&(second_idx, second)) = chars.peek() {
    if is_regional_indicator(second) {
      chars.next();
      return Some(EmojiSequence {
        start: start_idx,
        end: second_idx + second.len_utf8(),
        chars: vec![first, second],
        sequence_type: EmojiSequenceType::FlagSequence,
      });
    }
  }
  None
}

/// Try to parse a tag sequence (subdivision flags like 🏴󠁧󠁢󠁥󠁮󠁧󠁿)
fn try_parse_tag_sequence(
  chars: &mut std::iter::Peekable<std::str::CharIndices>,
  start_idx: usize,
  base: char,
  _text: &str,
) -> Option<EmojiSequence> {
  let mut seq_chars = vec![base];
  let mut end_idx = start_idx + base.len_utf8();
  let mut found_tags = false;

  // Collect tag characters
  while let Some(&(idx, ch)) = chars.peek() {
    if is_tag_character(ch) {
      seq_chars.push(ch);
      end_idx = idx + ch.len_utf8();
      found_tags = true;
      chars.next();

      // Check for cancel tag (end of sequence)
      if is_cancel_tag(ch) {
        break;
      }
    } else {
      break;
    }
  }

  if found_tags {
    Some(EmojiSequence {
      start: start_idx,
      end: end_idx,
      chars: seq_chars,
      sequence_type: EmojiSequenceType::TagSequence,
    })
  } else {
    None
  }
}

/// Check if the current sequence can take a skin tone modifier
fn can_take_modifier(seq_chars: &[char]) -> bool {
  // Get the last non-variation-selector emoji
  for &ch in seq_chars.iter().rev() {
    if !is_variation_selector(ch) && !is_zwj(ch) {
      return is_emoji_modifier_base(ch);
    }
  }
  false
}

// =============================================================================
// Helper Functions for Unicode Ranges
// =============================================================================

/// Check for miscellaneous single-character emoji
fn is_misc_emoji(cp: u32) -> bool {
  matches!(
      cp,
      // Arrows and symbols
      0x2194..=0x2199
      | 0x21A9..=0x21AA
      // Watch and hourglass
      | 0x231A..=0x231B
      // Keyboard
      | 0x2328
      // Eject
      | 0x23CF
      // Play buttons, etc.
      | 0x23E9..=0x23F3
      | 0x23F8..=0x23FA
      // Medical symbol
      | 0x2695
      // Scales and other symbols
      | 0x2696..=0x2697
      // Fleur-de-lis
      | 0x269B..=0x269C
      // Atom, etc.
      | 0x2699
      // Ballot box
      | 0x2611
      // Checkmark
      | 0x2714
      // Multiplication X
      | 0x2716
      // Star
      | 0x2733..=0x2734
      // Question marks
      | 0x2753..=0x2755
      // Exclamation
      | 0x2757
      // Star of David
      | 0x2721
      // Peace
      | 0x262E
      // Yin Yang
      | 0x262F
      // Latin Cross
      | 0x271D
      // Star and crescent
      | 0x262A
      // Om
      | 0x1F549
      // Wheel of dharma
      | 0x2638
      // Heavy heart
      | 0x2764
      // Orange heart, etc.
      | 0x1F9E1
      // Recycling
      | 0x267B
      // Trident
      | 0x1F531
      // Male/Female
      | 0x2640
      | 0x2642
      // Infinity
      | 0x267E
      // Sparkle
      | 0x2747
      // Bangbang
      | 0x203C
      // Interrobang
      | 0x2049
      // Wavy dash
      | 0x3030
      // Part alternation mark
      | 0x303D
      // Circled M
      | 0x24C2
      // Information
      | 0x2139
      // Letter symbols
      | 0x1F170..=0x1F171
      | 0x1F17E..=0x1F17F
      | 0x1F18E
      | 0x1F191..=0x1F19A
      // Circled letters
      | 0x1F1E6..=0x1F1FF
  )
}

/// Check for Miscellaneous Symbols with emoji presentation
fn is_emoji_presentation_misc_symbols(cp: u32) -> bool {
  matches!(
      cp,
      0x2614 // Umbrella with rain drops
      | 0x2615 // Hot beverage
      | 0x2648..=0x2653 // Zodiac signs
      | 0x267F // Wheelchair
      | 0x2693 // Anchor
      | 0x26A1 // High voltage
      | 0x26AA..=0x26AB // Circles
      | 0x26BD..=0x26BE // Soccer, baseball
      | 0x26C4..=0x26C5 // Snowman, sun
      | 0x26CE // Ophiuchus
      | 0x26D4 // No entry
      | 0x26EA // Church
      | 0x26F2..=0x26F3 // Fountain, golf
      | 0x26F5 // Sailboat
      | 0x26FA // Tent
      | 0x26FD // Fuel pump
  )
}

/// Check for Dingbats with emoji presentation
fn is_emoji_presentation_dingbats(cp: u32) -> bool {
  matches!(
      cp,
      0x2702 // Scissors
      | 0x2705 // Check mark
      | 0x2708..=0x270D // Airplane to writing hand
      | 0x270F // Pencil
      | 0x2712 // Black nib
      | 0x2714 // Check mark
      | 0x2716 // Multiplication X
      | 0x271D // Latin cross
      | 0x2721 // Star of David
      | 0x2728 // Sparkles
      | 0x2733..=0x2734 // Eight spoked asterisk
      | 0x2744 // Snowflake
      | 0x2747 // Sparkle
      | 0x274C // Cross mark
      | 0x274E // Cross mark with X
      | 0x2753..=0x2755 // Question marks
      | 0x2757 // Exclamation mark
      | 0x2763..=0x2764 // Heart exclamation, heart
      | 0x2795..=0x2797 // Plus, minus, divide
      | 0x27A1 // Right arrow
      | 0x27B0 // Curly loop
      | 0x27BF // Double curly loop
  )
}

#[cfg(test)]
mod tests {
  use super::*;

  // =========================================================================
  // is_emoji tests
  // =========================================================================

  #[test]
  fn test_is_emoji_emoticons() {
    assert!(is_emoji('😀')); // Grinning face
    assert!(is_emoji('😁')); // Beaming face
    assert!(is_emoji('😂')); // Face with tears of joy
    assert!(is_emoji('🤣')); // Rolling on floor laughing
    assert!(is_emoji('😭')); // Loudly crying face
  }

  #[test]
  fn test_is_emoji_symbols_pictographs() {
    assert!(is_emoji('🌍')); // Earth globe
    assert!(is_emoji('🌙')); // Crescent moon
    assert!(is_emoji('🌈')); // Rainbow
    assert!(is_emoji('🔥')); // Fire
    assert!(is_emoji('💧')); // Droplet
  }

  #[test]
  fn test_is_emoji_transport() {
    assert!(is_emoji('🚀')); // Rocket
    assert!(is_emoji('🚗')); // Automobile
    assert!(is_emoji('✈')); // Airplane
    assert!(is_emoji('🛸')); // Flying saucer
  }

  #[test]
  fn test_is_emoji_supplemental() {
    assert!(is_emoji('🤖')); // Robot
    assert!(is_emoji('🦊')); // Fox
    assert!(is_emoji('🧠')); // Brain
  }

  #[test]
  fn test_is_emoji_dingbats() {
    assert!(is_emoji('✂')); // Scissors
    assert!(is_emoji('✈')); // Airplane
    assert!(is_emoji('✉')); // Envelope
    assert!(is_emoji('✏')); // Pencil
  }

  #[test]
  fn test_is_emoji_misc_symbols() {
    assert!(is_emoji('☀')); // Sun
    assert!(is_emoji('☁')); // Cloud
    assert!(is_emoji('☂')); // Umbrella
    assert!(is_emoji('☃')); // Snowman
    assert!(is_emoji('☔')); // Umbrella with rain
  }

  #[test]
  fn test_is_emoji_regional_indicators() {
    assert!(is_emoji('🇦')); // Regional indicator A
    assert!(is_emoji('🇿')); // Regional indicator Z
    assert!(is_emoji('🇺')); // Regional indicator U
    assert!(is_emoji('🇸')); // Regional indicator S
  }

  #[test]
  fn test_is_emoji_modifiers() {
    assert!(is_emoji('\u{1F3FB}')); // Light skin tone
    assert!(is_emoji('\u{1F3FC}')); // Medium-light skin tone
    assert!(is_emoji('\u{1F3FD}')); // Medium skin tone
    assert!(is_emoji('\u{1F3FE}')); // Medium-dark skin tone
    assert!(is_emoji('\u{1F3FF}')); // Dark skin tone
  }

  #[test]
  fn test_is_emoji_zwj() {
    assert!(is_emoji('\u{200D}')); // Zero Width Joiner
  }

  #[test]
  fn test_is_emoji_variation_selectors() {
    assert!(is_emoji('\u{FE0E}')); // VS15 (text presentation)
    assert!(is_emoji('\u{FE0F}')); // VS16 (emoji presentation)
  }

  #[test]
  fn test_is_emoji_keycap_bases() {
    assert!(is_emoji('0'));
    assert!(is_emoji('9'));
    assert!(is_emoji('*'));
    assert!(is_emoji('#'));
  }

  #[test]
  fn test_is_emoji_non_emoji() {
    assert!(!is_emoji('A'));
    assert!(!is_emoji('z'));
    assert!(!is_emoji('!'));
    assert!(!is_emoji(' '));
    assert!(!is_emoji('中'));
    assert!(!is_emoji('ñ'));
    assert!(!is_emoji('α'));
  }

  // =========================================================================
  // is_emoji_presentation tests
  // =========================================================================

  #[test]
  fn test_is_emoji_presentation_emoticons() {
    assert!(is_emoji_presentation('😀'));
    assert!(is_emoji_presentation('😂'));
    assert!(is_emoji_presentation('🥹'));
  }

  #[test]
  fn test_is_emoji_presentation_transport() {
    assert!(is_emoji_presentation('🚀'));
    assert!(is_emoji_presentation('🚗'));
    assert!(is_emoji_presentation('🛸'));
  }

  #[test]
  fn test_is_emoji_presentation_false_for_text_default() {
    // These are emoji but default to text presentation
    assert!(!is_emoji_presentation('#'));
    assert!(!is_emoji_presentation('*'));
    assert!(!is_emoji_presentation('0'));
    // Copyright, registered, trademark default to text
    assert!(!is_emoji_presentation('©'));
    assert!(!is_emoji_presentation('®'));
  }

  // =========================================================================
  // Modifier tests
  // =========================================================================

  #[test]
  fn test_is_emoji_modifier() {
    assert!(is_emoji_modifier('\u{1F3FB}'));
    assert!(is_emoji_modifier('\u{1F3FC}'));
    assert!(is_emoji_modifier('\u{1F3FD}'));
    assert!(is_emoji_modifier('\u{1F3FE}'));
    assert!(is_emoji_modifier('\u{1F3FF}'));

    assert!(!is_emoji_modifier('A'));
    assert!(!is_emoji_modifier('😀'));
  }

  #[test]
  fn test_is_emoji_modifier_base() {
    assert!(is_emoji_modifier_base('👋')); // Waving hand
    assert!(is_emoji_modifier_base('👍')); // Thumbs up
    assert!(is_emoji_modifier_base('👎')); // Thumbs down
    assert!(is_emoji_modifier_base('👏')); // Clapping hands
    assert!(is_emoji_modifier_base('🤝')); // Handshake

    assert!(!is_emoji_modifier_base('🚀')); // Rocket
    assert!(!is_emoji_modifier_base('🔥')); // Fire
    assert!(!is_emoji_modifier_base('A'));
  }

  // =========================================================================
  // Regional indicator tests
  // =========================================================================

  #[test]
  fn test_is_regional_indicator() {
    assert!(is_regional_indicator('🇦'));
    assert!(is_regional_indicator('🇺'));
    assert!(is_regional_indicator('🇸'));
    assert!(is_regional_indicator('🇿'));

    assert!(!is_regional_indicator('A'));
    assert!(!is_regional_indicator('U'));
    assert!(!is_regional_indicator('😀'));
  }

  // =========================================================================
  // Sequence detection tests
  // =========================================================================

  #[test]
  fn test_find_emoji_sequences_single() {
    let seqs = find_emoji_sequences("Hello 🚀 World");
    assert_eq!(seqs.len(), 1);
    assert_eq!(seqs[0].chars, vec!['🚀']);
    assert_eq!(seqs[0].sequence_type, EmojiSequenceType::Single);
  }

  #[test]
  fn test_find_emoji_sequences_multiple() {
    let seqs = find_emoji_sequences("🔥💧🌍");
    assert_eq!(seqs.len(), 3);
    assert_eq!(seqs[0].chars, vec!['🔥']);
    assert_eq!(seqs[1].chars, vec!['💧']);
    assert_eq!(seqs[2].chars, vec!['🌍']);
  }

  #[test]
  fn test_find_emoji_sequences_with_modifier() {
    let seqs = find_emoji_sequences("👋🏽");
    assert_eq!(seqs.len(), 1);
    assert_eq!(seqs[0].chars.len(), 2);
    assert_eq!(seqs[0].chars[0], '👋');
    assert!(is_emoji_modifier(seqs[0].chars[1]));
    assert_eq!(seqs[0].sequence_type, EmojiSequenceType::WithModifier);
  }

  #[test]
  fn test_find_emoji_sequences_flag() {
    let seqs = find_emoji_sequences("🇺🇸");
    assert_eq!(seqs.len(), 1);
    assert_eq!(seqs[0].chars.len(), 2);
    assert_eq!(seqs[0].sequence_type, EmojiSequenceType::FlagSequence);
  }

  #[test]
  fn test_find_emoji_sequences_multiple_flags() {
    let seqs = find_emoji_sequences("🇺🇸🇬🇧🇯🇵");
    assert_eq!(seqs.len(), 3);
    assert!(seqs
      .iter()
      .all(|s| s.sequence_type == EmojiSequenceType::FlagSequence));
  }

  #[test]
  fn test_find_emoji_sequences_zwj_simple() {
    // Man + ZWJ + Woman = Couple
    let seqs = find_emoji_sequences("👨‍👩");
    assert_eq!(seqs.len(), 1);
    assert_eq!(seqs[0].sequence_type, EmojiSequenceType::ZwjSequence);
    assert!(seqs[0].chars.contains(&'\u{200D}')); // Contains ZWJ
  }

  #[test]
  fn test_find_emoji_sequences_zwj_family() {
    // Family: man + ZWJ + woman + ZWJ + girl
    let seqs = find_emoji_sequences("👨‍👩‍👧");
    assert_eq!(seqs.len(), 1);
    assert_eq!(seqs[0].sequence_type, EmojiSequenceType::ZwjSequence);
  }

  #[test]
  fn test_find_emoji_sequences_zwj_longer() {
    // Longer ZWJ sequence: 👨‍👩‍👧‍👦
    let seqs = find_emoji_sequences("👨‍👩‍👧‍👦");
    assert_eq!(seqs.len(), 1);
    assert_eq!(seqs[0].sequence_type, EmojiSequenceType::ZwjSequence);
  }

  #[test]
  fn test_find_emoji_sequences_keycap() {
    // 1️⃣ = 1 + VS16 + Combining Enclosing Keycap
    let seqs = find_emoji_sequences("1️⃣");
    assert_eq!(seqs.len(), 1);
    assert_eq!(seqs[0].sequence_type, EmojiSequenceType::KeycapSequence);
  }

  #[test]
  fn test_find_emoji_sequences_empty() {
    let seqs = find_emoji_sequences("Hello World");
    assert!(seqs.is_empty());
  }

  #[test]
  fn test_find_emoji_sequences_mixed_text() {
    let seqs = find_emoji_sequences("I ❤️ coding 🚀");
    assert_eq!(seqs.len(), 2);
  }

  #[test]
  fn test_emoji_sequence_as_str() {
    let text = "Hello 🚀 World";
    let seqs = find_emoji_sequences(text);
    assert_eq!(seqs.len(), 1);
    assert_eq!(seqs[0].as_str(text), "🚀");
  }

  #[test]
  fn test_emoji_sequence_byte_positions() {
    let text = "Hi 🚀!";
    let seqs = find_emoji_sequences(text);
    assert_eq!(seqs.len(), 1);
    // "Hi " is 3 bytes, rocket is 4 bytes
    assert_eq!(seqs[0].start, 3);
    assert_eq!(seqs[0].end, 7);
    assert_eq!(seqs[0].byte_len(), 4);
  }

  // =========================================================================
  // Edge case tests
  // =========================================================================

  #[test]
  fn test_single_regional_indicator() {
    let seqs = find_emoji_sequences("🇺");
    assert_eq!(seqs.len(), 1);
    assert_eq!(seqs[0].chars.len(), 1);
    assert_eq!(seqs[0].sequence_type, EmojiSequenceType::Single);
  }

  #[test]
  fn test_modifier_without_base() {
    // Skin tone modifier alone should still be detected as emoji
    let seqs = find_emoji_sequences("\u{1F3FD}");
    assert_eq!(seqs.len(), 1);
    assert_eq!(seqs[0].chars.len(), 1);
  }

  #[test]
  fn test_zwj_at_end_of_string() {
    // Emoji followed by ZWJ with nothing after
    let seqs = find_emoji_sequences("👨\u{200D}");
    assert_eq!(seqs.len(), 1);
    // Should just be the man emoji, ZWJ is dangling
  }

  #[test]
  fn test_variation_selector_emoji_presentation() {
    // Heart with VS16 (emoji presentation)
    let seqs = find_emoji_sequences("❤️");
    assert_eq!(seqs.len(), 1);
    assert!(seqs[0].chars.contains(&'\u{FE0F}'));
  }

  #[test]
  fn test_helper_functions() {
    assert!(is_zwj('\u{200D}'));
    assert!(!is_zwj('a'));

    assert!(is_variation_selector('\u{FE0E}'));
    assert!(is_variation_selector('\u{FE0F}'));
    assert!(!is_variation_selector('a'));

    assert!(is_emoji_variation_selector('\u{FE0F}'));
    assert!(!is_emoji_variation_selector('\u{FE0E}'));

    assert!(is_text_variation_selector('\u{FE0E}'));
    assert!(!is_text_variation_selector('\u{FE0F}'));

    assert!(is_keycap_base('#'));
    assert!(is_keycap_base('5'));
    assert!(!is_keycap_base('A'));

    assert!(is_combining_enclosing_keycap('\u{20E3}'));
    assert!(!is_combining_enclosing_keycap('a'));
  }
}
