//! Script Itemization
//!
//! Splits text into runs of the same Unicode script (Latin, Arabic, Devanagari, etc.).
//! This is a critical step in the text shaping pipeline, as different scripts require
//! different fonts and shaping behavior.
//!
//! # Overview
//!
//! Script itemization identifies boundaries where the script changes in text.
//! Each run can then be shaped with an appropriate font that supports that script.
//!
//! # Unicode Script Property
//!
//! Every Unicode character has a script property defined by UAX #24:
//! - **Explicit scripts**: Latin, Arabic, Hebrew, Devanagari, Han, etc.
//! - **Common**: Punctuation, digits, symbols (can appear in any script)
//! - **Inherited**: Combining marks (inherit script from base character)
//!
//! # Algorithm
//!
//! The algorithm groups consecutive characters with the same resolved script:
//! 1. For explicit scripts: use the script directly
//! 2. For Common/Inherited: resolve to adjacent script (prefer previous)
//! 3. When script changes: start a new run
//!
//! # Example
//!
//! ```rust
//! use fastrender::text::script::{itemize_scripts, ScriptRun};
//! use unicode_script::Script;
//!
//! let text = "Hello world";
//! let runs = itemize_scripts(text);
//! assert_eq!(runs.len(), 1);
//! assert_eq!(runs[0].script, Script::Latin);
//! ```
//!
//! # References
//!
//! - Unicode Script Property (UAX #24): https://www.unicode.org/reports/tr24/
//! - Unicode Standard Annex #24: https://www.unicode.org/reports/tr24/

use unicode_script::{Script, UnicodeScript};

/// A run of text with a uniform Unicode script.
///
/// Each `ScriptRun` represents a contiguous sequence of characters
/// that share the same script (after resolution of Common/Inherited).
///
/// # Fields
///
/// - `start`: Byte offset of the first character in the original text
/// - `end`: Byte offset after the last character (exclusive)
/// - `script`: The Unicode script for this run
///
/// # Note on Byte Offsets
///
/// The `start` and `end` fields are byte offsets into the UTF-8 encoded string,
/// NOT character indices. This is consistent with Rust's string slicing.
///
/// # Example
///
/// ```rust
/// use fastrender::text::script::{itemize_scripts, ScriptRun};
///
/// let text = "Hello";
/// let runs = itemize_scripts(text);
/// assert_eq!(runs[0].start, 0);
/// assert_eq!(runs[0].end, 5); // 5 bytes for "Hello"
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScriptRun {
    /// Byte offset of the start of this run in the original text.
    pub start: usize,

    /// Byte offset of the end of this run (exclusive) in the original text.
    pub end: usize,

    /// The Unicode script for this run.
    pub script: Script,
}

impl ScriptRun {
    /// Creates a new script run.
    ///
    /// # Arguments
    ///
    /// * `start` - Byte offset of the start
    /// * `end` - Byte offset of the end (exclusive)
    /// * `script` - The Unicode script
    ///
    /// # Example
    ///
    /// ```rust
    /// use fastrender::text::script::ScriptRun;
    /// use unicode_script::Script;
    ///
    /// let run = ScriptRun::new(0, 5, Script::Latin);
    /// assert_eq!(run.text_slice("Hello"), "Hello");
    /// ```
    pub fn new(start: usize, end: usize, script: Script) -> Self {
        Self { start, end, script }
    }

    /// Returns the length of this run in bytes.
    ///
    /// # Example
    ///
    /// ```rust
    /// use fastrender::text::script::ScriptRun;
    /// use unicode_script::Script;
    ///
    /// let run = ScriptRun::new(0, 10, Script::Latin);
    /// assert_eq!(run.len(), 10);
    /// ```
    #[inline]
    pub fn len(&self) -> usize {
        self.end - self.start
    }

    /// Returns true if this run is empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }

    /// Extracts the text slice for this run from the original text.
    ///
    /// # Arguments
    ///
    /// * `text` - The original text that was itemized
    ///
    /// # Returns
    ///
    /// The substring corresponding to this run.
    ///
    /// # Panics
    ///
    /// Panics if the byte offsets are out of bounds or not on character boundaries.
    ///
    /// # Example
    ///
    /// ```rust
    /// use fastrender::text::script::{itemize_scripts, ScriptRun};
    ///
    /// let text = "Hello world";
    /// let runs = itemize_scripts(text);
    /// assert_eq!(runs[0].text_slice(text), "Hello world");
    /// ```
    #[inline]
    pub fn text_slice<'a>(&self, text: &'a str) -> &'a str {
        &text[self.start..self.end]
    }
}

/// Itemizes text into runs of the same Unicode script.
///
/// This function analyzes the input text and splits it into contiguous runs
/// where each run contains characters of the same script (after resolution
/// of Common and Inherited scripts).
///
/// # Algorithm Details
///
/// 1. **Explicit Scripts**: Characters with an explicit script (Latin, Arabic, etc.)
///    use that script directly.
///
/// 2. **Common Script**: Characters like punctuation, digits, and whitespace
///    are resolved to the surrounding script. They are merged with the previous
///    run if one exists.
///
/// 3. **Inherited Script**: Combining marks and other inherited characters
///    are resolved to the base character's script.
///
/// 4. **Script Boundaries**: A new run starts when the resolved script changes.
///
/// # Arguments
///
/// * `text` - The text to itemize
///
/// # Returns
///
/// A vector of `ScriptRun` structs, ordered by their position in the text.
/// Returns an empty vector for empty input.
///
/// # Examples
///
/// ## Single Script
///
/// ```rust
/// use fastrender::text::script::itemize_scripts;
/// use unicode_script::Script;
///
/// let runs = itemize_scripts("Hello world");
/// assert_eq!(runs.len(), 1);
/// assert_eq!(runs[0].script, Script::Latin);
/// ```
///
/// ## Mixed Scripts
///
/// ```rust
/// use fastrender::text::script::itemize_scripts;
/// use unicode_script::Script;
///
/// // "Hello" (Latin) + " " (Common) + "שלום" (Hebrew)
/// let runs = itemize_scripts("Hello שלום");
/// assert_eq!(runs.len(), 2);
/// assert_eq!(runs[0].script, Script::Latin);
/// assert_eq!(runs[1].script, Script::Hebrew);
/// ```
///
/// ## CJK Text
///
/// ```rust
/// use fastrender::text::script::itemize_scripts;
/// use unicode_script::Script;
///
/// let runs = itemize_scripts("你好世界");
/// assert_eq!(runs.len(), 1);
/// assert_eq!(runs[0].script, Script::Han);
/// ```
pub fn itemize_scripts(text: &str) -> Vec<ScriptRun> {
    if text.is_empty() {
        return Vec::new();
    }

    let mut runs: Vec<ScriptRun> = Vec::new();
    let mut current_start: usize = 0;
    let mut current_script: Option<Script> = None;
    let mut pending_common_start: Option<usize> = None;
    let mut pending_common_end: usize = 0;

    for (byte_idx, ch) in text.char_indices() {
        let char_script = ch.script();
        let char_end = byte_idx + ch.len_utf8();

        match char_script {
            Script::Common | Script::Inherited => {
                // Common/Inherited characters don't determine script
                // Track them for later resolution
                if pending_common_start.is_none() {
                    pending_common_start = Some(byte_idx);
                }
                pending_common_end = char_end;
            }
            script => {
                // Explicit script character
                let resolved_script = resolve_script(script);

                if let Some(current) = current_script {
                    if current == resolved_script {
                        // Same script: include any pending common chars in current run
                        pending_common_start = None;
                    } else {
                        // Script changed: finish previous run
                        // Include pending common chars in the PREVIOUS run (not the new one)
                        let run_end = if pending_common_start.is_some() {
                            pending_common_end
                        } else {
                            byte_idx
                        };
                        if run_end > current_start {
                            runs.push(ScriptRun::new(current_start, run_end, current));
                        }

                        // Start new run at current character (NOT including pending common)
                        current_start = byte_idx;
                        current_script = Some(resolved_script);
                        pending_common_start = None;
                    }
                } else {
                    // First explicit script character
                    // Any leading common chars become part of this run
                    current_script = Some(resolved_script);
                    pending_common_start = None;
                }
            }
        }
    }

    // Finalize the last run
    if let Some(script) = current_script {
        // Include any trailing common/inherited chars
        let run_end = if pending_common_start.is_some() {
            pending_common_end
        } else {
            text.len()
        };
        runs.push(ScriptRun::new(current_start, run_end, script));
    } else if !text.is_empty() {
        // Text is entirely Common/Inherited - use Latin as default
        runs.push(ScriptRun::new(0, text.len(), Script::Latin));
    }

    runs
}

/// Resolves a script, handling special cases.
///
/// Some scripts may need special handling (e.g., Han/Hiragana/Katakana
/// are often mixed in Japanese text). This function can be extended
/// to handle such cases.
#[inline]
fn resolve_script(script: Script) -> Script {
    // For now, use the script as-is
    // Future: Could merge Han/Hiragana/Katakana for Japanese text
    script
}

/// Checks if a character is an emoji that should be treated specially.
///
/// Emoji often have Script::Common but may need special font handling.
/// This function identifies emoji characters for potential special treatment.
///
/// # Arguments
///
/// * `ch` - The character to check
///
/// # Returns
///
/// `true` if the character is an emoji base or modifier
///
/// # Note
///
/// This is a simplified emoji detection that covers the most common emoji ranges.
/// For comprehensive emoji detection, consider using a dedicated crate like `unic-emoji-char`.
#[inline]
pub fn is_emoji(ch: char) -> bool {
    // Emoji are typically in these ranges:
    // - U+1F300..U+1F9FF (Main emoji blocks)
    // - U+1FA00..U+1FAFF (Extended emoji)
    // - U+2600..U+27BF (Misc symbols and Dingbats)
    // - U+231A..U+23FF (Technical symbols with emoji)
    // - U+25AA..U+25FE (Geometric shapes with emoji)
    // - U+2B05..U+2B55 (Arrows and shapes with emoji)
    // - U+3030, U+303D, U+3297, U+3299 (CJK symbols with emoji)
    matches!(ch,
        '\u{1F300}'..='\u{1F9FF}' |  // Misc Symbols, Emoticons, Transport, etc.
        '\u{1FA00}'..='\u{1FAFF}' |  // Extended-A and Chess Symbols
        '\u{2600}'..='\u{27BF}'   |  // Misc symbols and Dingbats
        '\u{231A}'..='\u{23FF}'   |  // Watch, Hourglass, Media symbols
        '\u{25AA}'..='\u{25FE}'   |  // Geometric shapes (squares, buttons)
        '\u{2934}'..='\u{2935}'   |  // Arrows
        '\u{2B05}'..='\u{2B55}'   |  // Arrows, squares, stars
        '\u{3030}' |                 // Wavy dash
        '\u{303D}' |                 // Part alternation mark
        '\u{3297}' |                 // Circled Ideograph Congratulation
        '\u{3299}'                   // Circled Ideograph Secret
    )
}

/// Checks if a character is a Zero Width Joiner (ZWJ).
///
/// ZWJ is used in emoji sequences to combine multiple emoji into one
/// (e.g., family emoji, skin tone variations).
#[inline]
pub fn is_zwj(ch: char) -> bool {
    ch == '\u{200D}'
}

/// Checks if a character is a variation selector.
///
/// Variation selectors modify the presentation of the preceding character
/// (e.g., text vs emoji presentation).
#[inline]
pub fn is_variation_selector(ch: char) -> bool {
    matches!(ch, '\u{FE00}'..='\u{FE0F}')
}

/// Checks if a character is a skin tone modifier.
///
/// Skin tone modifiers (Fitzpatrick scale) change the skin color of
/// certain emoji.
#[inline]
pub fn is_skin_tone_modifier(ch: char) -> bool {
    matches!(ch, '\u{1F3FB}'..='\u{1F3FF}')
}

#[cfg(test)]
mod tests {
    use super::*;

    // ============================================================================
    // Basic Single Script Tests
    // ============================================================================

    #[test]
    fn test_empty_text() {
        let runs = itemize_scripts("");
        assert!(runs.is_empty());
    }

    #[test]
    fn test_single_latin_word() {
        let runs = itemize_scripts("Hello");
        assert_eq!(runs.len(), 1);
        assert_eq!(runs[0].start, 0);
        assert_eq!(runs[0].end, 5);
        assert_eq!(runs[0].script, Script::Latin);
    }

    #[test]
    fn test_latin_sentence() {
        let text = "Hello world";
        let runs = itemize_scripts(text);
        assert_eq!(runs.len(), 1);
        assert_eq!(runs[0].script, Script::Latin);
        assert_eq!(runs[0].text_slice(text), "Hello world");
    }

    #[test]
    fn test_cyrillic() {
        let text = "Привет мир";
        let runs = itemize_scripts(text);
        assert_eq!(runs.len(), 1);
        assert_eq!(runs[0].script, Script::Cyrillic);
    }

    #[test]
    fn test_greek() {
        let text = "Γειά σου κόσμε";
        let runs = itemize_scripts(text);
        assert_eq!(runs.len(), 1);
        assert_eq!(runs[0].script, Script::Greek);
    }

    #[test]
    fn test_hebrew() {
        let text = "שלום עולם";
        let runs = itemize_scripts(text);
        assert_eq!(runs.len(), 1);
        assert_eq!(runs[0].script, Script::Hebrew);
    }

    #[test]
    fn test_arabic() {
        let text = "مرحبا بالعالم";
        let runs = itemize_scripts(text);
        assert_eq!(runs.len(), 1);
        assert_eq!(runs[0].script, Script::Arabic);
    }

    #[test]
    fn test_han_chinese() {
        let text = "你好世界";
        let runs = itemize_scripts(text);
        assert_eq!(runs.len(), 1);
        assert_eq!(runs[0].script, Script::Han);
    }

    #[test]
    fn test_hiragana() {
        let text = "こんにちは";
        let runs = itemize_scripts(text);
        assert_eq!(runs.len(), 1);
        assert_eq!(runs[0].script, Script::Hiragana);
    }

    #[test]
    fn test_katakana() {
        let text = "コンニチハ";
        let runs = itemize_scripts(text);
        assert_eq!(runs.len(), 1);
        assert_eq!(runs[0].script, Script::Katakana);
    }

    #[test]
    fn test_hangul() {
        let text = "안녕하세요";
        let runs = itemize_scripts(text);
        assert_eq!(runs.len(), 1);
        assert_eq!(runs[0].script, Script::Hangul);
    }

    #[test]
    fn test_devanagari() {
        let text = "नमस्ते";
        let runs = itemize_scripts(text);
        assert_eq!(runs.len(), 1);
        assert_eq!(runs[0].script, Script::Devanagari);
    }

    #[test]
    fn test_thai() {
        let text = "สวัสดี";
        let runs = itemize_scripts(text);
        assert_eq!(runs.len(), 1);
        assert_eq!(runs[0].script, Script::Thai);
    }

    // ============================================================================
    // Mixed Scripts Tests
    // ============================================================================

    #[test]
    fn test_latin_hebrew_mixed() {
        let text = "Hello שלום";
        let runs = itemize_scripts(text);
        assert_eq!(runs.len(), 2);
        assert_eq!(runs[0].script, Script::Latin);
        assert_eq!(runs[1].script, Script::Hebrew);
    }

    #[test]
    fn test_latin_arabic_mixed() {
        let text = "Hello مرحبا";
        let runs = itemize_scripts(text);
        assert_eq!(runs.len(), 2);
        assert_eq!(runs[0].script, Script::Latin);
        assert_eq!(runs[1].script, Script::Arabic);
    }

    #[test]
    fn test_latin_cyrillic_mixed() {
        let text = "Hello Привет world";
        let runs = itemize_scripts(text);
        assert_eq!(runs.len(), 3);
        assert_eq!(runs[0].script, Script::Latin);
        assert_eq!(runs[1].script, Script::Cyrillic);
        assert_eq!(runs[2].script, Script::Latin);
    }

    #[test]
    fn test_multiple_scripts() {
        let text = "Hello שלום мир 你好";
        let runs = itemize_scripts(text);
        assert_eq!(runs.len(), 4);
        assert_eq!(runs[0].script, Script::Latin);
        assert_eq!(runs[1].script, Script::Hebrew);
        assert_eq!(runs[2].script, Script::Cyrillic);
        assert_eq!(runs[3].script, Script::Han);
    }

    // ============================================================================
    // Common/Inherited Character Tests
    // ============================================================================

    #[test]
    fn test_common_punctuation_within_script() {
        let text = "Hello, world!";
        let runs = itemize_scripts(text);
        // Punctuation should be included with Latin
        assert_eq!(runs.len(), 1);
        assert_eq!(runs[0].script, Script::Latin);
    }

    #[test]
    fn test_common_digits_within_latin() {
        let text = "Test 123 numbers";
        let runs = itemize_scripts(text);
        assert_eq!(runs.len(), 1);
        assert_eq!(runs[0].script, Script::Latin);
    }

    #[test]
    fn test_digits_between_scripts() {
        let text = "Hello 123 שלום";
        let runs = itemize_scripts(text);
        // Digits should be assigned to surrounding script
        assert_eq!(runs.len(), 2);
    }

    #[test]
    fn test_only_common_chars() {
        let text = "123 456";
        let runs = itemize_scripts(text);
        // All Common - should default to Latin
        assert_eq!(runs.len(), 1);
        assert_eq!(runs[0].script, Script::Latin);
    }

    #[test]
    fn test_only_punctuation() {
        let text = "...!!!???";
        let runs = itemize_scripts(text);
        assert_eq!(runs.len(), 1);
        assert_eq!(runs[0].script, Script::Latin);
    }

    #[test]
    fn test_whitespace_only() {
        let text = "   ";
        let runs = itemize_scripts(text);
        // Whitespace is Common, defaults to Latin
        assert_eq!(runs.len(), 1);
    }

    // ============================================================================
    // Emoji Tests
    // ============================================================================

    #[test]
    fn test_simple_emoji() {
        let text = "Hello 👋 world";
        let runs = itemize_scripts(text);
        // Emoji is Common, should merge with Latin
        assert_eq!(runs.len(), 1);
        assert_eq!(runs[0].script, Script::Latin);
    }

    #[test]
    fn test_emoji_only() {
        let text = "👋😀🎉";
        let runs = itemize_scripts(text);
        // All Common emoji - defaults to Latin
        assert_eq!(runs.len(), 1);
    }

    #[test]
    fn test_emoji_detection() {
        assert!(is_emoji('😀'));
        assert!(is_emoji('👋'));
        assert!(is_emoji('🎉'));
        assert!(is_emoji('❤'));
        assert!(!is_emoji('A'));
        assert!(!is_emoji('1'));
    }

    #[test]
    fn test_zwj_detection() {
        assert!(is_zwj('\u{200D}'));
        assert!(!is_zwj('A'));
    }

    #[test]
    fn test_variation_selector_detection() {
        assert!(is_variation_selector('\u{FE0F}'));
        assert!(is_variation_selector('\u{FE0E}'));
        assert!(!is_variation_selector('A'));
    }

    #[test]
    fn test_skin_tone_modifier_detection() {
        assert!(is_skin_tone_modifier('\u{1F3FB}')); // Light skin tone
        assert!(is_skin_tone_modifier('\u{1F3FF}')); // Dark skin tone
        assert!(!is_skin_tone_modifier('A'));
    }

    // ============================================================================
    // Edge Cases
    // ============================================================================

    #[test]
    fn test_single_char() {
        let runs = itemize_scripts("A");
        assert_eq!(runs.len(), 1);
        assert_eq!(runs[0].start, 0);
        assert_eq!(runs[0].end, 1);
    }

    #[test]
    fn test_single_multibyte_char() {
        let text = "字";
        let runs = itemize_scripts(text);
        assert_eq!(runs.len(), 1);
        assert_eq!(runs[0].start, 0);
        assert_eq!(runs[0].end, 3); // Chinese char is 3 bytes
        assert_eq!(runs[0].script, Script::Han);
    }

    #[test]
    fn test_byte_offsets_multibyte() {
        let text = "Aя字"; // 1 byte + 2 bytes + 3 bytes = 6 bytes
        let runs = itemize_scripts(text);

        // Should have 3 runs: Latin, Cyrillic, Han
        assert_eq!(runs.len(), 3);

        assert_eq!(runs[0].start, 0);
        assert_eq!(runs[0].end, 1); // 'A' is 1 byte
        assert_eq!(runs[0].script, Script::Latin);

        assert_eq!(runs[1].start, 1);
        assert_eq!(runs[1].end, 3); // 'я' is 2 bytes
        assert_eq!(runs[1].script, Script::Cyrillic);

        assert_eq!(runs[2].start, 3);
        assert_eq!(runs[2].end, 6); // '字' is 3 bytes
        assert_eq!(runs[2].script, Script::Han);
    }

    #[test]
    fn test_script_run_text_slice() {
        let text = "Hello שלום";
        let runs = itemize_scripts(text);

        assert_eq!(runs[0].text_slice(text), "Hello ");
        assert_eq!(runs[1].text_slice(text), "שלום");
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

    // ============================================================================
    // Japanese Mixed Writing Tests
    // ============================================================================

    #[test]
    fn test_japanese_mixed_hiragana_katakana() {
        let text = "ひらがなカタカナ";
        let runs = itemize_scripts(text);
        // Hiragana and Katakana are different scripts
        assert_eq!(runs.len(), 2);
        assert_eq!(runs[0].script, Script::Hiragana);
        assert_eq!(runs[1].script, Script::Katakana);
    }

    #[test]
    fn test_japanese_with_kanji() {
        let text = "漢字とひらがな";
        let runs = itemize_scripts(text);
        // Han (Kanji), then Hiragana
        assert!(runs.len() >= 2);
    }

    // ============================================================================
    // Complex Real-World Tests
    // ============================================================================

    #[test]
    fn test_url_in_text() {
        let text = "Visit https://example.com for more info";
        let runs = itemize_scripts(text);
        // Should be single Latin run (URL chars are Common/Latin)
        assert_eq!(runs.len(), 1);
        assert_eq!(runs[0].script, Script::Latin);
    }

    #[test]
    fn test_code_snippet() {
        let text = "function() { return x + 1; }";
        let runs = itemize_scripts(text);
        assert_eq!(runs.len(), 1);
        assert_eq!(runs[0].script, Script::Latin);
    }

    #[test]
    fn test_math_expression() {
        let text = "x² + y² = z²";
        let runs = itemize_scripts(text);
        assert_eq!(runs.len(), 1);
    }
}
