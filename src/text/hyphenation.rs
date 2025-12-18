//! Word hyphenation for improved line breaking
//!
//! This module provides language-aware hyphenation using dictionary-based
//! patterns. It integrates with the line breaking system to allow words
//! to be broken at syllable boundaries with hyphens.
//!
//! # Overview
//!
//! Hyphenation is the process of dividing words at line breaks by inserting
//! hyphens at appropriate syllable boundaries. This is essential for:
//! - Justified text (reducing large gaps between words)
//! - Narrow columns (fitting text in limited width)
//! - Professional typography
//!
//! # Algorithm
//!
//! This implementation uses the TeX hyphenation algorithm by Frank Liang,
//! which uses pattern matching to find valid hyphenation points. The patterns
//! are language-specific and capture common hyphenation rules.
//!
//! # Supported Languages
//!
//! Currently supported (with embedded patterns):
//! - English (US) - embedded by default
//!
//! Additional languages can be added by enabling crate features.
//!
//! # Example
//!
//! ```rust,ignore
//! use fastrender::text::Hyphenator;
//!
//! // Create hyphenator for English
//! let hyphenator = Hyphenator::new("en-us").unwrap();
//!
//! // Find hyphenation points
//! let points = hyphenator.hyphenate("hyphenation");
//! // Returns positions where hyphens can be inserted: [2, 6, 8]
//! // "hy-phen-a-tion"
//! ```
//!
//! # CSS Properties
//!
//! This module supports the CSS `hyphens` property:
//! - `hyphens: none` - No hyphenation
//! - `hyphens: manual` - Only at `&shy;` (soft hyphen, U+00AD)
//! - `hyphens: auto` - Automatic hyphenation based on language
//!
//! # References
//!
//! - [CSS Text Module Level 3 - Hyphenation](https://www.w3.org/TR/css-text-3/#hyphenation)
//! - [TeX Hyphenation Patterns](http://www.tug.org/docs/liang/)
//! - [Unicode Line Breaking Algorithm (UAX #14)](https://www.unicode.org/reports/tr14/)

use crate::error::{Result, TextError};
use hyphenation::{Language, Load, Standard};
use std::collections::HashMap;
use std::sync::{Arc, Mutex, OnceLock};

static PATTERN_CACHE: OnceLock<Mutex<HashMap<SupportedLanguage, Arc<HyphenationPatterns>>>> = OnceLock::new();

/// Hyphenation patterns for a specific language
///
/// This struct wraps the hyphenation dictionary and provides
/// an interface for finding hyphenation points in words.
#[derive(Clone)]
pub struct HyphenationPatterns {
    /// The language for these patterns
    language: SupportedLanguage,

    /// The underlying hyphenation dictionary
    dictionary: Standard,
}

impl HyphenationPatterns {
    /// Create new hyphenation patterns for a language
    ///
    /// # Arguments
    ///
    /// * `language` - The language to load patterns for
    ///
    /// # Errors
    ///
    /// Returns an error if the language patterns cannot be loaded.
    pub fn new(language: SupportedLanguage) -> Result<Self> {
        let lang = language.as_hyphenation_language();
        let dictionary = Standard::from_embedded(lang).map_err(|e| TextError::HyphenationFailed {
            language: language.code().to_string(),
            reason: format!("Failed to load embedded patterns: {:?}", e),
        })?;

        Ok(Self { language, dictionary })
    }

    /// Get the language code for these patterns
    pub fn language_code(&self) -> &str {
        self.language.code()
    }

    /// Check if this language requires special handling
    pub fn needs_special_handling(&self) -> bool {
        // Some languages have special rules beyond pattern matching
        matches!(self.language, SupportedLanguage::German | SupportedLanguage::Dutch)
    }
}

fn cached_patterns(language: SupportedLanguage) -> Result<Arc<HyphenationPatterns>> {
    let cache = PATTERN_CACHE.get_or_init(|| Mutex::new(HashMap::new()));
    if let Some(existing) = cache.lock().expect("pattern cache poisoned").get(&language).cloned() {
        return Ok(existing);
    }

    let loaded = Arc::new(HyphenationPatterns::new(language)?);
    let mut guard = cache.lock().expect("pattern cache poisoned");
    guard.insert(language, Arc::clone(&loaded));
    Ok(loaded)
}

impl std::fmt::Debug for HyphenationPatterns {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HyphenationPatterns")
            .field("language", &self.language)
            .finish_non_exhaustive()
    }
}

/// Supported languages for hyphenation
///
/// Each variant corresponds to a language with available hyphenation
/// patterns. The patterns are based on TeX hyphenation dictionaries.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SupportedLanguage {
    /// English (United States)
    EnglishUS,
    /// English (United Kingdom)
    EnglishGB,
    /// German (1996 reform spelling)
    German,
    /// French
    French,
    /// Spanish
    Spanish,
    /// Italian
    Italian,
    /// Portuguese (Brazil)
    PortugueseBrazil,
    /// Portuguese (Portugal)
    PortuguesePortugal,
    /// Dutch
    Dutch,
    /// Polish
    Polish,
    /// Russian
    Russian,
    /// Swedish
    Swedish,
    /// Norwegian (Bokmål)
    NorwegianBokmal,
    /// Danish
    Danish,
    /// Finnish
    Finnish,
    /// Hungarian
    Hungarian,
    /// Czech
    Czech,
    /// Slovak
    Slovak,
    /// Croatian
    Croatian,
    /// Catalan
    Catalan,
    /// Turkish
    Turkish,
    /// Greek (Modern)
    Greek,
    /// Ukrainian
    Ukrainian,
    /// Latin
    Latin,
}

impl SupportedLanguage {
    /// Parse a language code into a supported language
    ///
    /// Accepts BCP 47 language tags (e.g., "en-US", "de-DE") and
    /// ISO 639-1 codes (e.g., "en", "de").
    ///
    /// # Arguments
    ///
    /// * `code` - The language code to parse
    ///
    /// # Returns
    ///
    /// Returns `Some(language)` if the code is recognized, `None` otherwise.
    ///
    /// # Examples
    ///
    /// ```rust,ignore
    /// use fastrender::text::SupportedLanguage;
    ///
    /// assert_eq!(SupportedLanguage::parse("en-US"), Some(SupportedLanguage::EnglishUS));
    /// assert_eq!(SupportedLanguage::parse("de"), Some(SupportedLanguage::German));
    /// ```
    pub fn parse(code: &str) -> Option<Self> {
        let code_lower = code.to_lowercase();
        let code_lower = code_lower.as_str();

        match code_lower {
            // English
            "en" | "en-us" | "eng" => Some(Self::EnglishUS),
            "en-gb" | "en-uk" => Some(Self::EnglishGB),

            // German
            "de" | "de-de" | "de-at" | "de-ch" | "deu" | "ger" => Some(Self::German),

            // French
            "fr" | "fr-fr" | "fr-ca" | "fr-be" | "fr-ch" | "fra" | "fre" => Some(Self::French),

            // Spanish
            "es" | "es-es" | "es-mx" | "es-ar" | "spa" => Some(Self::Spanish),

            // Italian
            "it" | "it-it" | "ita" => Some(Self::Italian),

            // Portuguese
            "pt" | "pt-br" => Some(Self::PortugueseBrazil),
            "pt-pt" => Some(Self::PortuguesePortugal),

            // Dutch
            "nl" | "nl-nl" | "nl-be" | "nld" | "dut" => Some(Self::Dutch),

            // Polish
            "pl" | "pl-pl" | "pol" => Some(Self::Polish),

            // Russian
            "ru" | "ru-ru" | "rus" => Some(Self::Russian),

            // Swedish
            "sv" | "sv-se" | "swe" => Some(Self::Swedish),

            // Norwegian
            "nb" | "nb-no" | "no" | "nor" | "nob" => Some(Self::NorwegianBokmal),

            // Danish
            "da" | "da-dk" | "dan" => Some(Self::Danish),

            // Finnish
            "fi" | "fi-fi" | "fin" => Some(Self::Finnish),

            // Hungarian
            "hu" | "hu-hu" | "hun" => Some(Self::Hungarian),

            // Czech
            "cs" | "cs-cz" | "ces" | "cze" => Some(Self::Czech),

            // Slovak
            "sk" | "sk-sk" | "slk" | "slo" => Some(Self::Slovak),

            // Croatian
            "hr" | "hr-hr" | "hrv" => Some(Self::Croatian),

            // Catalan
            "ca" | "ca-es" | "cat" => Some(Self::Catalan),

            // Turkish
            "tr" | "tr-tr" | "tur" => Some(Self::Turkish),

            // Greek
            "el" | "el-gr" | "ell" | "gre" => Some(Self::Greek),

            // Ukrainian
            "uk" | "uk-ua" | "ukr" => Some(Self::Ukrainian),

            // Latin
            "la" | "lat" => Some(Self::Latin),

            _ => None,
        }
    }

    /// Get the BCP 47 language code
    pub fn code(&self) -> &'static str {
        match self {
            Self::EnglishUS => "en-US",
            Self::EnglishGB => "en-GB",
            Self::German => "de-DE",
            Self::French => "fr-FR",
            Self::Spanish => "es-ES",
            Self::Italian => "it-IT",
            Self::PortugueseBrazil => "pt-BR",
            Self::PortuguesePortugal => "pt-PT",
            Self::Dutch => "nl-NL",
            Self::Polish => "pl-PL",
            Self::Russian => "ru-RU",
            Self::Swedish => "sv-SE",
            Self::NorwegianBokmal => "nb-NO",
            Self::Danish => "da-DK",
            Self::Finnish => "fi-FI",
            Self::Hungarian => "hu-HU",
            Self::Czech => "cs-CZ",
            Self::Slovak => "sk-SK",
            Self::Croatian => "hr-HR",
            Self::Catalan => "ca-ES",
            Self::Turkish => "tr-TR",
            Self::Greek => "el-GR",
            Self::Ukrainian => "uk-UA",
            Self::Latin => "la",
        }
    }

    /// Get the corresponding hyphenation crate Language enum value
    fn as_hyphenation_language(self) -> Language {
        match self {
            Self::EnglishUS => Language::EnglishUS,
            Self::EnglishGB => Language::EnglishGB,
            Self::German => Language::German1996,
            Self::French => Language::French,
            Self::Spanish => Language::Spanish,
            Self::Italian => Language::Italian,
            Self::PortugueseBrazil => Language::Portuguese, // hyphenation crate uses generic Portuguese
            Self::PortuguesePortugal => Language::Portuguese,
            Self::Dutch => Language::Dutch,
            Self::Polish => Language::Polish,
            Self::Russian => Language::Russian,
            Self::Swedish => Language::Swedish,
            Self::NorwegianBokmal => Language::NorwegianBokmal,
            Self::Danish => Language::Danish,
            Self::Finnish => Language::Finnish,
            Self::Hungarian => Language::Hungarian,
            Self::Czech => Language::Czech,
            Self::Slovak => Language::Slovak,
            Self::Croatian => Language::Croatian,
            Self::Catalan => Language::Catalan,
            Self::Turkish => Language::Turkish,
            Self::Greek => Language::GreekMono,
            Self::Ukrainian => Language::Ukrainian,
            Self::Latin => Language::Latin,
        }
    }

    /// Check if patterns are embedded for this language
    ///
    /// Only English (US) patterns are embedded by default.
    /// Other languages require enabling crate features.
    pub fn is_embedded(&self) -> bool {
        // Only en-US is embedded by default in our configuration
        matches!(self, Self::EnglishUS)
    }
}

/// Word hyphenator
///
/// The main entry point for hyphenation. Provides methods to find
/// hyphenation points in words based on language-specific patterns.
///
/// # Example
///
/// ```rust,ignore
/// use fastrender::text::Hyphenator;
///
/// let hyphenator = Hyphenator::new("en-us").unwrap();
///
/// // Get hyphenation points
/// let points = hyphenator.hyphenate("internationalization");
/// println!("Break points: {:?}", points);
///
/// // Get hyphenated segments
/// let segments = hyphenator.hyphenate_word("internationalization");
/// println!("Segments: {:?}", segments);
/// ```
#[derive(Debug, Clone)]
pub struct Hyphenator {
    /// Hyphenation patterns
    patterns: Arc<HyphenationPatterns>,

    /// Minimum characters before first hyphen (left hyphen min)
    left_min: usize,

    /// Minimum characters after last hyphen (right hyphen min)
    right_min: usize,

    /// Minimum word length for hyphenation
    min_word_length: usize,
}

impl Hyphenator {
    /// Default minimum characters before first hyphen
    pub const DEFAULT_LEFT_MIN: usize = 2;

    /// Default minimum characters after last hyphen
    pub const DEFAULT_RIGHT_MIN: usize = 2;

    /// Default minimum word length for hyphenation
    pub const DEFAULT_MIN_WORD_LENGTH: usize = 5;

    /// Create a new hyphenator for a language
    ///
    /// # Arguments
    ///
    /// * `language` - Language code (BCP 47 or ISO 639-1)
    ///
    /// # Errors
    ///
    /// Returns an error if the language is not supported or patterns
    /// cannot be loaded.
    ///
    /// # Examples
    ///
    /// ```rust,ignore
    /// use fastrender::text::Hyphenator;
    ///
    /// let hyphenator = Hyphenator::new("en-us").unwrap();
    /// ```
    pub fn new(language: &str) -> Result<Self> {
        let lang = SupportedLanguage::parse(language).ok_or_else(|| TextError::HyphenationFailed {
            language: language.to_string(),
            reason: "Unsupported language".to_string(),
        })?;

        let patterns = cached_patterns(lang)?;

        Ok(Self {
            patterns,
            left_min: Self::DEFAULT_LEFT_MIN,
            right_min: Self::DEFAULT_RIGHT_MIN,
            min_word_length: Self::DEFAULT_MIN_WORD_LENGTH,
        })
    }

    /// Create hyphenator with custom settings
    ///
    /// # Arguments
    ///
    /// * `language` - Language code
    /// * `left_min` - Minimum characters before first hyphen
    /// * `right_min` - Minimum characters after last hyphen
    /// * `min_word_length` - Minimum word length for hyphenation
    ///
    /// # Errors
    ///
    /// Returns an error if the language is not supported.
    pub fn with_settings(language: &str, left_min: usize, right_min: usize, min_word_length: usize) -> Result<Self> {
        let mut hyphenator = Self::new(language)?;
        hyphenator.left_min = left_min.max(1);
        hyphenator.right_min = right_min.max(1);
        hyphenator.min_word_length = min_word_length.max(2);
        Ok(hyphenator)
    }

    /// Get the language code for this hyphenator
    pub fn language(&self) -> &str {
        self.patterns.language_code()
    }

    /// Find hyphenation points in a word
    ///
    /// Returns a vector of byte positions where the word can be
    /// hyphenated. Each position indicates where a hyphen can be
    /// inserted (after that many bytes from the start).
    ///
    /// # Arguments
    ///
    /// * `word` - The word to hyphenate
    ///
    /// # Returns
    ///
    /// A vector of byte positions for valid hyphenation points.
    /// Empty if the word is too short or cannot be hyphenated.
    ///
    /// # Examples
    ///
    /// ```rust,ignore
    /// use fastrender::text::Hyphenator;
    ///
    /// let hyphenator = Hyphenator::new("en-us").unwrap();
    ///
    /// let points = hyphenator.hyphenate("hyphenation");
    /// // Returns byte positions for "hy-phen-a-tion"
    /// ```
    pub fn hyphenate(&self, word: &str) -> Vec<usize> {
        // Check minimum word length
        let char_count = word.chars().count();
        if char_count < self.min_word_length {
            return Vec::new();
        }

        // Check if word is purely alphabetic
        if !word.chars().all(|c| c.is_alphabetic()) {
            return Vec::new();
        }

        // Use the hyphenation crate to find break points
        use hyphenation::Hyphenator as HyphenatorTrait;

        let hyphenated = self.patterns.dictionary.hyphenate(word);
        let breaks: Vec<usize> = hyphenated.breaks.to_vec();

        // Filter breaks based on left_min and right_min
        self.filter_break_points(word, breaks)
    }

    /// Filter break points based on minimum constraints
    fn filter_break_points(&self, word: &str, mut breaks: Vec<usize>) -> Vec<usize> {
        let char_indices: Vec<(usize, char)> = word.char_indices().collect();
        let char_count = char_indices.len();

        // Convert character positions to constraints
        let left_boundary = if self.left_min < char_count {
            char_indices[self.left_min].0
        } else {
            word.len()
        };

        let right_boundary = if self.right_min < char_count {
            char_indices[char_count - self.right_min].0
        } else {
            0
        };

        // Filter breaks
        breaks.retain(|&pos| pos >= left_boundary && pos <= right_boundary && word.is_char_boundary(pos));

        breaks
    }

    /// Hyphenate a word into segments
    ///
    /// Returns the word split at hyphenation points.
    ///
    /// # Arguments
    ///
    /// * `word` - The word to hyphenate
    ///
    /// # Returns
    ///
    /// A vector of word segments. If the word cannot be hyphenated,
    /// returns a single-element vector with the original word.
    ///
    /// # Examples
    ///
    /// ```rust,ignore
    /// use fastrender::text::Hyphenator;
    ///
    /// let hyphenator = Hyphenator::new("en-us").unwrap();
    ///
    /// let segments = hyphenator.hyphenate_word("hyphenation");
    /// // Returns ["hy", "phen", "a", "tion"]
    /// ```
    pub fn hyphenate_word<'a>(&self, word: &'a str) -> Vec<&'a str> {
        let breaks = self.hyphenate(word);

        if breaks.is_empty() {
            return vec![word];
        }

        let mut segments = Vec::with_capacity(breaks.len() + 1);
        let mut start = 0;

        for &break_pos in &breaks {
            if break_pos > start && break_pos <= word.len() {
                segments.push(&word[start..break_pos]);
                start = break_pos;
            }
        }

        // Add the remaining part
        if start < word.len() {
            segments.push(&word[start..]);
        }

        segments
    }

    /// Find hyphenation points in text (multiple words)
    ///
    /// Returns a vector of tuples containing the word start position
    /// and a vector of hyphenation positions within that word.
    ///
    /// # Arguments
    ///
    /// * `text` - The text to analyze
    ///
    /// # Returns
    ///
    /// A vector of `(word_start, break_positions)` tuples.
    pub fn hyphenate_text(&self, text: &str) -> Vec<(usize, Vec<usize>)> {
        let mut results = Vec::new();

        for (word_start, word) in self.split_words(text) {
            let breaks = self.hyphenate(word);
            if !breaks.is_empty() {
                // Convert word-relative positions to text-relative
                let text_breaks: Vec<usize> = breaks.iter().map(|&pos| word_start + pos).collect();
                results.push((word_start, text_breaks));
            }
        }

        results
    }

    /// Split text into words with their positions
    fn split_words<'a>(&self, text: &'a str) -> Vec<(usize, &'a str)> {
        let mut words = Vec::new();
        let mut word_start: Option<usize> = None;

        for (idx, ch) in text.char_indices() {
            if ch.is_alphabetic() {
                if word_start.is_none() {
                    word_start = Some(idx);
                }
            } else if let Some(start) = word_start {
                words.push((start, &text[start..idx]));
                word_start = None;
            }
        }

        // Handle last word
        if let Some(start) = word_start {
            words.push((start, &text[start..]));
        }

        words
    }

    /// Check if a word can be hyphenated
    ///
    /// This is a quick check that doesn't compute actual break points.
    pub fn can_hyphenate(&self, word: &str) -> bool {
        let char_count = word.chars().count();
        char_count >= self.min_word_length && word.chars().all(|c| c.is_alphabetic())
    }

    /// Get the minimum characters before first hyphen
    pub fn left_min(&self) -> usize {
        self.left_min
    }

    /// Get the minimum characters after last hyphen
    pub fn right_min(&self) -> usize {
        self.right_min
    }

    /// Get the minimum word length for hyphenation
    pub fn min_word_length(&self) -> usize {
        self.min_word_length
    }
}

/// CSS `hyphens` property values
///
/// Controls how words are hyphenated when text wraps.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum HyphensMode {
    /// No hyphenation, even at soft hyphens
    None,

    /// Only hyphenate at soft hyphens (U+00AD)
    #[default]
    Manual,

    /// Automatic hyphenation based on language
    Auto,
}

impl HyphensMode {
    /// Parse from CSS value
    pub fn parse(value: &str) -> Option<Self> {
        match value.trim().to_lowercase().as_str() {
            "none" => Some(Self::None),
            "manual" => Some(Self::Manual),
            "auto" => Some(Self::Auto),
            _ => None,
        }
    }

    /// Convert to CSS value
    pub fn as_css(&self) -> &'static str {
        match self {
            Self::None => "none",
            Self::Manual => "manual",
            Self::Auto => "auto",
        }
    }
}

/// Check if a character is a soft hyphen (U+00AD)
///
/// Soft hyphens are invisible characters that indicate where a word
/// may be broken with a hyphen if needed for line breaking.
#[inline]
pub fn is_soft_hyphen(c: char) -> bool {
    c == '\u{00AD}'
}

/// Find soft hyphen positions in text
///
/// Returns byte positions of all soft hyphens in the text.
pub fn find_soft_hyphens(text: &str) -> Vec<usize> {
    text.char_indices()
        .filter(|(_, c)| is_soft_hyphen(*c))
        .map(|(pos, _)| pos)
        .collect()
}

/// Remove soft hyphens from text
///
/// Returns a new string with all soft hyphens removed.
pub fn remove_soft_hyphens(text: &str) -> String {
    text.chars().filter(|c| !is_soft_hyphen(*c)).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_supported_language_parse() {
        assert_eq!(SupportedLanguage::parse("en-US"), Some(SupportedLanguage::EnglishUS));
        assert_eq!(SupportedLanguage::parse("en"), Some(SupportedLanguage::EnglishUS));
        assert_eq!(SupportedLanguage::parse("EN-US"), Some(SupportedLanguage::EnglishUS));
        assert_eq!(SupportedLanguage::parse("de"), Some(SupportedLanguage::German));
        assert_eq!(SupportedLanguage::parse("invalid"), None);
    }

    #[test]
    fn test_supported_language_code() {
        assert_eq!(SupportedLanguage::EnglishUS.code(), "en-US");
        assert_eq!(SupportedLanguage::German.code(), "de-DE");
    }

    #[test]
    fn test_hyphenator_creation() {
        let hyphenator = Hyphenator::new("en-us");
        assert!(hyphenator.is_ok());

        let hyphenator = Hyphenator::new("invalid-language");
        assert!(hyphenator.is_err());
    }

    #[test]
    fn test_hyphenator_language() {
        let hyphenator = Hyphenator::new("en-us").unwrap();
        assert_eq!(hyphenator.language(), "en-US");
    }

    #[test]
    fn test_hyphenate_short_word() {
        let hyphenator = Hyphenator::new("en-us").unwrap();

        // Words shorter than min_word_length should not be hyphenated
        let points = hyphenator.hyphenate("the");
        assert!(points.is_empty());

        let points = hyphenator.hyphenate("word");
        assert!(points.is_empty());
    }

    #[test]
    fn test_hyphenate_long_word() {
        let hyphenator = Hyphenator::new("en-us").unwrap();

        let points = hyphenator.hyphenate("hyphenation");
        // Should find hyphenation points
        assert!(!points.is_empty());
    }

    #[test]
    fn test_hyphenate_non_alphabetic() {
        let hyphenator = Hyphenator::new("en-us").unwrap();

        // Words with non-alphabetic characters should not be hyphenated
        let points = hyphenator.hyphenate("test123");
        assert!(points.is_empty());

        let points = hyphenator.hyphenate("hello-world");
        assert!(points.is_empty());
    }

    #[test]
    fn test_hyphenate_word() {
        let hyphenator = Hyphenator::new("en-us").unwrap();

        let segments = hyphenator.hyphenate_word("hyphenation");
        // Should return multiple segments
        assert!(segments.len() > 1);

        // Joining segments should give original word
        let joined: String = segments.join("");
        assert_eq!(joined, "hyphenation");
    }

    #[test]
    fn test_hyphenate_word_short() {
        let hyphenator = Hyphenator::new("en-us").unwrap();

        let segments = hyphenator.hyphenate_word("cat");
        // Short word should return single segment
        assert_eq!(segments.len(), 1);
        assert_eq!(segments[0], "cat");
    }

    #[test]
    fn filter_break_points_rejects_non_char_boundaries() {
        let hyphenator = Hyphenator::new("en-us").unwrap();
        let word = "al😊pha";

        // Simulate a break landing inside the emoji's UTF-8 sequence (offset 3)
        let filtered = hyphenator.filter_break_points(word, vec![2, 3, 7]);

        assert!(filtered.iter().all(|&pos| word.is_char_boundary(pos)));
        assert!(!filtered.contains(&3), "non-char-boundary break should be dropped");
        assert!(filtered.contains(&2) && filtered.contains(&7));
    }

    #[test]
    fn test_hyphenate_text() {
        let hyphenator = Hyphenator::new("en-us").unwrap();

        let results = hyphenator.hyphenate_text("The internationalization of hyphenation.");
        // Should find hyphenation points in longer words
        assert!(!results.is_empty());
    }

    #[test]
    fn test_can_hyphenate() {
        let hyphenator = Hyphenator::new("en-us").unwrap();

        assert!(!hyphenator.can_hyphenate("cat")); // Too short
        assert!(hyphenator.can_hyphenate("hyphenation")); // Long enough
        assert!(!hyphenator.can_hyphenate("test123")); // Non-alphabetic
    }

    #[test]
    fn test_hyphenator_with_settings() {
        let hyphenator = Hyphenator::with_settings("en-us", 3, 3, 6).unwrap();

        assert_eq!(hyphenator.left_min(), 3);
        assert_eq!(hyphenator.right_min(), 3);
        assert_eq!(hyphenator.min_word_length(), 6);
    }

    #[test]
    fn test_hyphens_mode_parse() {
        assert_eq!(HyphensMode::parse("none"), Some(HyphensMode::None));
        assert_eq!(HyphensMode::parse("manual"), Some(HyphensMode::Manual));
        assert_eq!(HyphensMode::parse("auto"), Some(HyphensMode::Auto));
        assert_eq!(HyphensMode::parse("invalid"), None);
    }

    #[test]
    fn test_hyphens_mode_as_css() {
        assert_eq!(HyphensMode::None.as_css(), "none");
        assert_eq!(HyphensMode::Manual.as_css(), "manual");
        assert_eq!(HyphensMode::Auto.as_css(), "auto");
    }

    #[test]
    fn test_soft_hyphen() {
        assert!(is_soft_hyphen('\u{00AD}'));
        assert!(!is_soft_hyphen('-'));
        assert!(!is_soft_hyphen('a'));
    }

    #[test]
    fn test_find_soft_hyphens() {
        let text = "hy\u{00AD}phen\u{00AD}ation";
        let positions = find_soft_hyphens(text);
        assert_eq!(positions.len(), 2);
    }

    #[test]
    fn test_remove_soft_hyphens() {
        let text = "hy\u{00AD}phen\u{00AD}ation";
        let cleaned = remove_soft_hyphens(text);
        assert_eq!(cleaned, "hyphenation");
    }

    #[test]
    fn test_hyphenation_patterns_debug() {
        let patterns = HyphenationPatterns::new(SupportedLanguage::EnglishUS).unwrap();
        let debug = format!("{:?}", patterns);
        assert!(debug.contains("HyphenationPatterns"));
        assert!(debug.contains("EnglishUS"));
    }

    #[test]
    fn test_default_hyphens_mode() {
        let mode = HyphensMode::default();
        assert_eq!(mode, HyphensMode::Manual);
    }

    #[test]
    fn test_hyphenator_defaults() {
        assert_eq!(Hyphenator::DEFAULT_LEFT_MIN, 2);
        assert_eq!(Hyphenator::DEFAULT_RIGHT_MIN, 2);
        assert_eq!(Hyphenator::DEFAULT_MIN_WORD_LENGTH, 5);
    }
}
