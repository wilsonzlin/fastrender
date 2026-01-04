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

#[cfg(test)]
thread_local! {
  static EMOJI_SEQUENCE_SPAN_CALLS: std::cell::Cell<usize> = std::cell::Cell::new(0);
}

#[cfg(test)]
#[doc(hidden)]
pub(crate) fn debug_reset_emoji_sequence_span_calls() {
  EMOJI_SEQUENCE_SPAN_CALLS.with(|calls| calls.set(0));
}

#[cfg(test)]
#[doc(hidden)]
pub(crate) fn debug_emoji_sequence_span_calls() -> usize {
  EMOJI_SEQUENCE_SPAN_CALLS.with(|calls| calls.get())
}

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct EmojiSequenceSpan {
  pub(crate) start: usize,
  pub(crate) end: usize,
  pub(crate) sequence_type: EmojiSequenceType,
}

// =============================================================================
// Core Emoji Detection Functions
// =============================================================================

/// Check if a character should be treated as emoji for rendering/fallback.
///
/// This is based on Unicode's emoji data (UTS #51, `emoji-data.txt`):
/// - `Emoji=Yes`
/// - `Emoji_Component=Yes` (ZWJ, VS16, tag chars, skin tone modifiers, keycap mark, ...)
///
/// ## Renderer convenience
///
/// `emoji-data.txt` does not include U+FE0E (VS15) in `Emoji_Component`, but it is used inside
/// emoji/text presentation sequences. The renderer treats VS15 as emoji so the whole sequence
/// stays on the emoji fallback path.
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
  if c.is_ascii() {
    return matches!(c, '0'..='9' | '*' | '#');
  }
  let cp = c as u32;
  if cp == 0xfe0e {
    return true;
  }

  super::emoji_tables::is_emoji(cp)
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
  if c.is_ascii() {
    return false;
  }
  let cp = c as u32;
  super::emoji_tables::is_emoji_presentation(cp)
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
  if c.is_ascii() {
    return false;
  }
  super::emoji_tables::is_emoji_modifier(c as u32)
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
  if c.is_ascii() {
    return false;
  }
  super::emoji_tables::is_emoji_modifier_base(c as u32)
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
pub(crate) fn find_emoji_sequence_spans(text: &str) -> Vec<EmojiSequenceSpan> {
  #[cfg(test)]
  EMOJI_SEQUENCE_SPAN_CALLS.with(|calls| calls.set(calls.get() + 1));

  if text.is_ascii() {
    return Vec::new();
  }

  let mut sequences = Vec::new();
  let mut chars = text.char_indices().peekable();

  while let Some((start_idx, ch)) = chars.next() {
    // Check for keycap sequences first (0-9, *, #).
    if is_keycap_base(ch) {
      let mut end_idx = start_idx + ch.len_utf8();

      // Check for VS16 (emoji presentation).
      if let Some(&(idx, next_ch)) = chars.peek() {
        if is_emoji_variation_selector(next_ch) {
          end_idx = idx + next_ch.len_utf8();
          chars.next();
        }
      }

      // Check for combining enclosing keycap.
      if let Some(&(idx, next_ch)) = chars.peek() {
        if is_combining_enclosing_keycap(next_ch) {
          end_idx = idx + next_ch.len_utf8();
          chars.next();
          sequences.push(EmojiSequenceSpan {
            start: start_idx,
            end: end_idx,
            sequence_type: EmojiSequenceType::KeycapSequence,
          });
        }
      }

      // Not a keycap sequence; skip (not emoji on its own without a keycap suffix).
      continue;
    }

    // Check for tag sequences (subdivision flags like 🏴).
    if is_tag_base(ch) {
      let mut end_idx = start_idx + ch.len_utf8();
      let mut found_tags = false;

      while let Some(&(idx, next_ch)) = chars.peek() {
        if is_tag_character(next_ch) {
          found_tags = true;
          end_idx = idx + next_ch.len_utf8();
          chars.next();
          if is_cancel_tag(next_ch) {
            break;
          }
        } else {
          break;
        }
      }

      sequences.push(EmojiSequenceSpan {
        start: start_idx,
        end: if found_tags {
          end_idx
        } else {
          start_idx + ch.len_utf8()
        },
        sequence_type: if found_tags {
          EmojiSequenceType::TagSequence
        } else {
          EmojiSequenceType::Single
        },
      });
      continue;
    }

    // Check for regional indicator (potential flag sequence).
    if is_regional_indicator(ch) {
      if let Some(&(second_idx, second)) = chars.peek() {
        if is_regional_indicator(second) {
          chars.next();
          sequences.push(EmojiSequenceSpan {
            start: start_idx,
            end: second_idx + second.len_utf8(),
            sequence_type: EmojiSequenceType::FlagSequence,
          });
          continue;
        }
      }
      sequences.push(EmojiSequenceSpan {
        start: start_idx,
        end: start_idx + ch.len_utf8(),
        sequence_type: EmojiSequenceType::Single,
      });
      continue;
    }

    // Skip non-emoji characters.
    if !is_emoji(ch) || is_variation_selector(ch) || is_zwj(ch) {
      continue;
    }

    let mut end_idx = start_idx + ch.len_utf8();
    let mut seq_type = EmojiSequenceType::Single;
    let mut last_non_vs_or_zwj = ch;

    while let Some(&(next_idx, next_ch)) = chars.peek() {
      if is_variation_selector(next_ch) {
        end_idx = next_idx + next_ch.len_utf8();
        chars.next();
        continue;
      }

      if is_emoji_modifier(next_ch) && is_emoji_modifier_base(last_non_vs_or_zwj) {
        end_idx = next_idx + next_ch.len_utf8();
        seq_type = EmojiSequenceType::WithModifier;
        last_non_vs_or_zwj = next_ch;
        chars.next();
        continue;
      }

      if is_zwj(next_ch) {
        chars.next(); // consume ZWJ

        if let Some(&(after_zwj_idx, after_zwj_ch)) = chars.peek() {
          if is_emoji(after_zwj_ch) && !is_variation_selector(after_zwj_ch) {
            end_idx = after_zwj_idx + after_zwj_ch.len_utf8();
            seq_type = EmojiSequenceType::ZwjSequence;
            last_non_vs_or_zwj = after_zwj_ch;
            chars.next(); // consume emoji after ZWJ
            continue;
          }
        }

        // ZWJ not followed by emoji - we already consumed it, so just break.
        break;
      }

      break;
    }

    sequences.push(EmojiSequenceSpan {
      start: start_idx,
      end: end_idx,
      sequence_type: seq_type,
    });
  }

  sequences
}

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
  fn test_is_emoji_play_button() {
    // U+25B6 is Emoji=Yes but was previously missed by our ad-hoc ranges.
    assert!(is_emoji('▶'));
    assert!(!is_emoji_presentation('▶'));
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
  fn test_is_emoji_presentation_misc_symbols_and_arrows() {
    assert!(is_emoji_presentation('⭐'));
    assert!(is_emoji_presentation('⬆'));
    assert!(is_emoji_presentation('⬇'));
  }

  #[test]
  fn test_is_emoji_presentation_false_for_text_default() {
    // These are emoji but default to text presentation
    assert!(!is_emoji_presentation('#'));
    assert!(!is_emoji_presentation('*'));
    assert!(!is_emoji_presentation('0'));
    assert!(!is_emoji_presentation('▶'));
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
    assert!(is_emoji_modifier_base('🏌')); // Person golfing (was previously missed by ad-hoc ranges)

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
  fn test_find_emoji_sequence_spans_matches_sequences() {
    let cases = [
      "Hello 🚀 World",
      "👋🏽",
      "🇺🇸",
      "👨\u{200D}👩",
      "1️⃣",
      "❤️",
      "🏴",
      "Hello World",
    ];

    for text in cases {
      let spans = find_emoji_sequence_spans(text);
      let seqs = find_emoji_sequences(text);
      assert_eq!(spans.len(), seqs.len(), "text={text}");
      for (span, seq) in spans.iter().zip(seqs.iter()) {
        assert_eq!(span.start, seq.start, "text={text}");
        assert_eq!(span.end, seq.end, "text={text}");
        assert_eq!(span.sequence_type, seq.sequence_type, "text={text}");
      }
    }
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
  fn test_find_emoji_sequences_with_modifier_golfer() {
    // U+1F3CC is Emoji_Modifier_Base in Unicode, but we previously missed it with ad-hoc ranges.
    let seqs = find_emoji_sequences("🏌🏽");
    assert_eq!(seqs.len(), 1);
    assert_eq!(seqs[0].chars, vec!['🏌', '🏽']);
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
