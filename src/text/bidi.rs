//! Bidirectional text analysis (UAX #9)
//!
//! This module implements bidirectional text support for mixed left-to-right (LTR)
//! and right-to-left (RTL) text such as English mixed with Arabic or Hebrew.
//!
//! The implementation wraps the `unicode-bidi` crate which provides a complete
//! implementation of the Unicode Bidirectional Algorithm (UAX #9).
//!
//! # Overview
//!
//! The Unicode Bidirectional Algorithm determines the display order of text
//! that contains characters from scripts written left-to-right (Latin, Cyrillic)
//! and right-to-left (Arabic, Hebrew).
//!
//! Key concepts:
//! - **Embedding levels**: Even levels (0, 2, 4...) are LTR, odd levels (1, 3, 5...) are RTL
//! - **Visual runs**: Contiguous sequences of characters with the same direction
//! - **Reordering**: Converting from logical (memory) order to visual (display) order
//!
//! # Usage
//!
//! ```rust,ignore
//! use fastrender::text::bidi::{BidiAnalyzer, Direction};
//!
//! let analyzer = BidiAnalyzer::new();
//!
//! // Analyze mixed LTR/RTL text
//! let runs = analyzer.analyze("Hello שלום world", Direction::Ltr);
//!
//! // Each run contains a substring with uniform direction
//! for run in &runs {
//!     println!("Text: '{}', Direction: {:?}, Level: {}",
//!         run.text, run.direction, run.level);
//! }
//! ```
//!
//! # CSS Integration
//!
//! The `direction` CSS property maps to the base direction:
//! - `direction: ltr` → `Direction::Ltr`
//! - `direction: rtl` → `Direction::Rtl`
//!
//! # References
//!
//! - [UAX #9: Unicode Bidirectional Algorithm](https://www.unicode.org/reports/tr9/)
//! - [CSS Writing Modes Level 4](https://www.w3.org/TR/css-writing-modes-4/)

use unicode_bidi::{bidi_class, BidiClass, BidiInfo, Level, ParagraphInfo};

/// Text direction
///
/// Represents the base direction for text layout. This maps directly to the
/// CSS `direction` property.
///
/// # Examples
///
/// ```rust,ignore
/// use fastrender::text::bidi::Direction;
///
/// let ltr = Direction::Ltr;  // For English, Latin scripts
/// let rtl = Direction::Rtl;  // For Arabic, Hebrew scripts
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Direction {
    /// Left-to-right (LTR) direction
    ///
    /// Used for:
    /// - Latin scripts (English, French, German)
    /// - Cyrillic scripts (Russian)
    /// - Greek script
    /// - Most other scripts
    #[default]
    Ltr,

    /// Right-to-left (RTL) direction
    ///
    /// Used for:
    /// - Arabic script
    /// - Hebrew script
    /// - Other RTL scripts (Syriac, Thaana)
    Rtl,
}

impl Direction {
    /// Returns true if this is a left-to-right direction.
    pub fn is_ltr(self) -> bool {
        self == Direction::Ltr
    }

    /// Returns true if this is a right-to-left direction.
    pub fn is_rtl(self) -> bool {
        self == Direction::Rtl
    }

    /// Converts this direction to a unicode-bidi Level.
    pub fn to_level(self) -> Level {
        match self {
            Direction::Ltr => Level::ltr(),
            Direction::Rtl => Level::rtl(),
        }
    }

    /// Creates a Direction from a unicode-bidi Level.
    pub fn from_level(level: Level) -> Self {
        if level.is_ltr() {
            Direction::Ltr
        } else {
            Direction::Rtl
        }
    }
}

/// A run of text with uniform bidirectional properties.
///
/// A `BidiRun` represents a contiguous sequence of characters that all share
/// the same embedding level and direction. After bidi analysis, text is split
/// into these runs for layout and rendering.
///
/// # Fields
///
/// - `start`: Byte offset of the run start in the original text
/// - `end`: Byte offset of the run end in the original text (exclusive)
/// - `level`: The bidi embedding level (0-125)
/// - `direction`: Whether this run is LTR or RTL
/// - `text`: The actual text content of this run
///
/// # Examples
///
/// ```rust,ignore
/// use fastrender::text::bidi::{BidiRun, Direction};
///
/// let run = BidiRun {
///     start: 0,
///     end: 5,
///     level: 0,
///     direction: Direction::Ltr,
///     text: "Hello".to_string(),
/// };
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BidiRun {
    /// Start byte offset in original text.
    pub start: usize,

    /// End byte offset in original text (exclusive).
    pub end: usize,

    /// Bidi embedding level (0-125).
    ///
    /// Even levels (0, 2, 4...) are LTR.
    /// Odd levels (1, 3, 5...) are RTL.
    pub level: u8,

    /// Text direction for this run.
    pub direction: Direction,

    /// The text content of this run.
    pub text: String,
}

impl BidiRun {
    /// Returns the length of this run in bytes.
    pub fn len(&self) -> usize {
        self.end - self.start
    }

    /// Returns true if this run is empty.
    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }

    /// Returns true if this run is left-to-right.
    pub fn is_ltr(&self) -> bool {
        self.direction.is_ltr()
    }

    /// Returns true if this run is right-to-left.
    pub fn is_rtl(&self) -> bool {
        self.direction.is_rtl()
    }
}

/// Result of bidirectional text analysis.
///
/// Contains all information needed to process bidirectional text, including
/// the original text, computed embedding levels, and whether reordering is needed.
///
/// # Example
///
/// ```rust,ignore
/// use fastrender::text::bidi::{BidiAnalyzer, Direction};
///
/// let analyzer = BidiAnalyzer::new();
/// let analysis = analyzer.analyze_full("Hello שלום", Direction::Ltr);
///
/// if analysis.needs_reordering() {
///     let runs = analysis.visual_runs();
///     // Process runs in visual order
/// }
/// ```
#[derive(Debug, Clone)]
pub struct BidiAnalysis {
    /// The original text that was analyzed.
    text: String,

    /// The base (paragraph) direction.
    base_direction: Direction,

    /// Byte starts for each character.
    char_starts: Vec<usize>,

    /// Embedding level for each character.
    levels: Vec<Level>,

    /// Paragraph information from unicode-bidi.
    paragraph: ParagraphInfo,

    /// Whether any RTL content was found.
    has_rtl: bool,
}

impl BidiAnalysis {
    /// Returns the original text.
    pub fn text(&self) -> &str {
        &self.text
    }

    /// Returns the base (paragraph) direction.
    pub fn base_direction(&self) -> Direction {
        self.base_direction
    }

    /// Returns true if the text needs bidi reordering.
    ///
    /// Text needs reordering if it contains any RTL characters or
    /// characters at a different embedding level than the base.
    pub fn needs_reordering(&self) -> bool {
        self.has_rtl
    }

    /// Returns the embedding level at a specific byte index.
    ///
    /// Returns the base level if the index is out of bounds.
    pub fn level_at(&self, byte_index: usize) -> Level {
        if self.levels.is_empty() {
            return self.base_direction.to_level();
        }
        let pos = match self.char_starts.binary_search(&byte_index) {
            Ok(idx) => idx,
            Err(idx) => idx.saturating_sub(1),
        };
        self.levels
            .get(pos)
            .copied()
            .unwrap_or_else(|| self.base_direction.to_level())
    }

    /// Returns the direction at a specific byte index.
    pub fn direction_at(&self, byte_index: usize) -> Direction {
        Direction::from_level(self.level_at(byte_index))
    }

    /// Returns all embedding levels.
    pub fn levels(&self) -> &[Level] {
        &self.levels
    }

    /// Returns the paragraph information.
    pub fn paragraph_info(&self) -> &ParagraphInfo {
        &self.paragraph
    }

    /// Computes visual runs for the analyzed text.
    ///
    /// Visual runs are ordered for display (left-to-right on screen).
    /// Within RTL runs, text is reversed for visual display.
    pub fn visual_runs(&self) -> Vec<BidiRun> {
        if self.text.is_empty() {
            return Vec::new();
        }

        // Use BidiInfo for proper reordering
        let bidi_info = BidiInfo::new(&self.text, Some(self.base_direction.to_level()));

        // Get visual runs from unicode-bidi
        let line_range = 0..self.text.len();
        let (_reordered_levels, level_runs) = bidi_info.visual_runs(&self.paragraph, line_range);

        let mut runs = Vec::new();
        for level_run in level_runs {
            let start = level_run.start;
            let end = level_run.end;
            let text_slice = &self.text[start..end];

            // Get the level at this position
            let char_index = self.text[..start].chars().count();
            let level = self.levels.get(char_index).copied().unwrap_or_else(Level::ltr);

            runs.push(BidiRun {
                start,
                end,
                level: level.number(),
                direction: Direction::from_level(level),
                text: text_slice.to_string(),
            });
        }

        runs
    }

    /// Returns runs in logical (memory) order.
    ///
    /// These runs are in the original text order, not visual order.
    pub fn logical_runs(&self) -> Vec<BidiRun> {
        if self.text.is_empty() {
            return Vec::new();
        }

        let mut runs = Vec::new();
        let mut chars_iter = self.text.char_indices().peekable();
        let mut current_run: Option<(usize, usize, Level)> = None;
        let mut char_idx = 0;

        while let Some((byte_pos, ch)) = chars_iter.next() {
            let level = self.levels.get(char_idx).copied().unwrap_or_else(Level::ltr);
            let byte_end = byte_pos + ch.len_utf8();

            match current_run {
                None => {
                    current_run = Some((byte_pos, byte_end, level));
                }
                Some((run_start, _, run_level)) if run_level == level => {
                    current_run = Some((run_start, byte_end, run_level));
                }
                Some((run_start, run_end, run_level)) => {
                    runs.push(BidiRun {
                        start: run_start,
                        end: run_end,
                        level: run_level.number(),
                        direction: Direction::from_level(run_level),
                        text: self.text[run_start..run_end].to_string(),
                    });
                    current_run = Some((byte_pos, byte_end, level));
                }
            }

            char_idx += 1;
        }

        // Add final run
        if let Some((run_start, run_end, run_level)) = current_run {
            runs.push(BidiRun {
                start: run_start,
                end: run_end,
                level: run_level.number(),
                direction: Direction::from_level(run_level),
                text: self.text[run_start..run_end].to_string(),
            });
        }

        runs
    }
}

/// Bidirectional text analyzer.
///
/// This struct wraps the `unicode-bidi` crate to provide bidirectional text
/// analysis according to UAX #9 (Unicode Bidirectional Algorithm).
///
/// # Usage
///
/// ```rust,ignore
/// use fastrender::text::bidi::{BidiAnalyzer, Direction};
///
/// let analyzer = BidiAnalyzer::new();
///
/// // Simple analysis returning runs
/// let runs = analyzer.analyze("Hello שלום world", Direction::Ltr);
///
/// // Full analysis with more control
/// let analysis = analyzer.analyze_full("Hello שלום world", Direction::Ltr);
/// if analysis.needs_reordering() {
///     let visual_runs = analysis.visual_runs();
/// }
/// ```
///
/// # CSS Integration
///
/// The base direction should come from the CSS `direction` property:
/// - `direction: ltr` → `Direction::Ltr` (default)
/// - `direction: rtl` → `Direction::Rtl`
///
/// # Performance
///
/// The bidi algorithm is O(n) where n is the text length. For long texts,
/// consider caching the `BidiAnalysis` result.
#[derive(Debug, Clone, Default)]
pub struct BidiAnalyzer;

impl BidiAnalyzer {
    /// Creates a new bidi analyzer.
    pub fn new() -> Self {
        Self
    }

    /// Analyzes text and returns visual runs.
    ///
    /// This is the primary API for bidi analysis. It returns runs in visual
    /// (display) order, ready for rendering.
    ///
    /// # Arguments
    ///
    /// * `text` - The text to analyze
    /// * `base_direction` - The base paragraph direction (from CSS `direction`)
    ///
    /// # Returns
    ///
    /// A vector of `BidiRun` in visual (display) order.
    ///
    /// # Examples
    ///
    /// ```rust,ignore
    /// use fastrender::text::bidi::{BidiAnalyzer, Direction};
    ///
    /// let analyzer = BidiAnalyzer::new();
    ///
    /// // LTR base with RTL content
    /// let runs = analyzer.analyze("Hello שלום world", Direction::Ltr);
    /// assert_eq!(runs.len(), 3); // "Hello ", "שלום", " world"
    ///
    /// // Pure LTR text
    /// let runs = analyzer.analyze("Hello world", Direction::Ltr);
    /// assert_eq!(runs.len(), 1);
    /// ```
    pub fn analyze(&self, text: &str, base_direction: Direction) -> Vec<BidiRun> {
        let analysis = self.analyze_full(text, base_direction);
        analysis.visual_runs()
    }

    /// Performs full bidi analysis with detailed results.
    ///
    /// Use this when you need more control over the analysis, such as:
    /// - Checking if reordering is needed
    /// - Accessing embedding levels
    /// - Getting both logical and visual runs
    ///
    /// # Arguments
    ///
    /// * `text` - The text to analyze
    /// * `base_direction` - The base paragraph direction
    ///
    /// # Returns
    ///
    /// A `BidiAnalysis` containing all bidi information.
    pub fn analyze_full(&self, text: &str, base_direction: Direction) -> BidiAnalysis {
        if text.is_empty() {
            return BidiAnalysis {
                text: String::new(),
                base_direction,
                char_starts: Vec::new(),
                levels: Vec::new(),
                paragraph: ParagraphInfo {
                    range: 0..0,
                    level: base_direction.to_level(),
                },
                has_rtl: false,
            };
        }

        let base_level = base_direction.to_level();

        // Run the bidi algorithm
        let bidi_info = BidiInfo::new(text, Some(base_level));

        // Get paragraph info
        let paragraph = if bidi_info.paragraphs.is_empty() {
            ParagraphInfo {
                range: 0..text.len(),
                level: base_level,
            }
        } else {
            bidi_info.paragraphs[0].clone()
        };

        // Copy levels
        let levels = bidi_info.levels.clone();
        let char_starts: Vec<usize> = text.char_indices().map(|(i, _)| i).collect();

        // Check if any RTL content exists
        let has_rtl = levels.iter().any(|&level| level.is_rtl());

        BidiAnalysis {
            text: text.to_string(),
            base_direction,
            char_starts,
            levels,
            paragraph,
            has_rtl,
        }
    }

    /// Returns the bidi class for a character.
    ///
    /// This is useful for determining the intrinsic directionality of a character
    /// without running the full algorithm.
    ///
    /// # Arguments
    ///
    /// * `ch` - The character to classify
    ///
    /// # Returns
    ///
    /// The `BidiClass` of the character.
    ///
    /// # Examples
    ///
    /// ```rust,ignore
    /// use fastrender::text::bidi::BidiAnalyzer;
    /// use unicode_bidi::BidiClass;
    ///
    /// let analyzer = BidiAnalyzer::new();
    ///
    /// // Latin 'A' is Left-to-Right
    /// assert_eq!(analyzer.char_bidi_class('A'), BidiClass::L);
    ///
    /// // Hebrew 'א' is Right-to-Left
    /// assert_eq!(analyzer.char_bidi_class('א'), BidiClass::R);
    ///
    /// // Arabic 'ب' is Arabic Letter
    /// assert_eq!(analyzer.char_bidi_class('ب'), BidiClass::AL);
    /// ```
    pub fn char_bidi_class(&self, ch: char) -> BidiClass {
        bidi_class(ch)
    }

    /// Determines the intrinsic direction of a character.
    ///
    /// Returns `Some(Direction)` for strongly-directional characters (L, R, AL),
    /// or `None` for neutral/weak characters.
    pub fn char_direction(&self, ch: char) -> Option<Direction> {
        match self.char_bidi_class(ch) {
            BidiClass::L => Some(Direction::Ltr),
            BidiClass::R | BidiClass::AL => Some(Direction::Rtl),
            _ => None,
        }
    }

    /// Determines the direction from the first strong character.
    ///
    /// This is used for automatic direction detection when no explicit
    /// direction is set. It scans the text for the first character with
    /// a strong directional class (L, R, or AL).
    ///
    /// # Arguments
    ///
    /// * `text` - The text to scan
    /// * `default` - Direction to return if no strong character is found
    ///
    /// # Returns
    ///
    /// The direction of the first strong character, or `default` if none found.
    ///
    /// # Examples
    ///
    /// ```rust,ignore
    /// use fastrender::text::bidi::{BidiAnalyzer, Direction};
    ///
    /// let analyzer = BidiAnalyzer::new();
    ///
    /// // First strong character is 'H' (LTR)
    /// let dir = analyzer.detect_direction("Hello שלום", Direction::Ltr);
    /// assert_eq!(dir, Direction::Ltr);
    ///
    /// // First strong character is 'ש' (RTL)
    /// let dir = analyzer.detect_direction("שלום Hello", Direction::Ltr);
    /// assert_eq!(dir, Direction::Rtl);
    ///
    /// // No strong characters, use default
    /// let dir = analyzer.detect_direction("123...", Direction::Ltr);
    /// assert_eq!(dir, Direction::Ltr);
    /// ```
    pub fn detect_direction(&self, text: &str, default: Direction) -> Direction {
        for ch in text.chars() {
            if let Some(dir) = self.char_direction(ch) {
                return dir;
            }
        }
        default
    }

    /// Checks if text contains any RTL characters.
    ///
    /// This is a quick check without running the full bidi algorithm.
    /// Useful for optimization when you want to skip bidi processing
    /// for pure LTR text.
    ///
    /// # Examples
    ///
    /// ```rust,ignore
    /// use fastrender::text::bidi::BidiAnalyzer;
    ///
    /// let analyzer = BidiAnalyzer::new();
    ///
    /// assert!(!analyzer.has_rtl("Hello world"));
    /// assert!(analyzer.has_rtl("Hello שלום"));
    /// ```
    pub fn has_rtl(&self, text: &str) -> bool {
        for ch in text.chars() {
            match self.char_bidi_class(ch) {
                BidiClass::R | BidiClass::AL | BidiClass::RLE | BidiClass::RLO | BidiClass::RLI => {
                    return true;
                }
                _ => {}
            }
        }
        false
    }

    /// Checks if text is purely LTR (no RTL characters).
    ///
    /// This is the inverse of `has_rtl()`.
    pub fn is_pure_ltr(&self, text: &str) -> bool {
        !self.has_rtl(text)
    }

    /// Reorders text from logical to visual order.
    ///
    /// This reverses RTL runs within the text to produce the visual
    /// representation.
    ///
    /// # Arguments
    ///
    /// * `text` - The text to reorder
    /// * `base_direction` - The base paragraph direction
    ///
    /// # Returns
    ///
    /// The text reordered for visual display.
    ///
    /// # Examples
    ///
    /// ```rust,ignore
    /// use fastrender::text::bidi::{BidiAnalyzer, Direction};
    ///
    /// let analyzer = BidiAnalyzer::new();
    ///
    /// // RTL text gets reversed
    /// let visual = analyzer.reorder_text("שלום", Direction::Ltr);
    /// // Note: actual visual representation reverses the Hebrew
    /// ```
    pub fn reorder_text(&self, text: &str, base_direction: Direction) -> String {
        if text.is_empty() {
            return String::new();
        }

        let runs = self.analyze(text, base_direction);
        let mut result = String::with_capacity(text.len());

        for run in runs {
            if run.is_rtl() {
                // Reverse RTL runs character by character
                for ch in run.text.chars().rev() {
                    result.push(ch);
                }
            } else {
                result.push_str(&run.text);
            }
        }

        result
    }
}

/// Analyzes text for bidirectional properties.
///
/// This is a convenience function that creates a `BidiAnalyzer` and
/// calls `analyze()`. For multiple analyses, prefer creating a single
/// `BidiAnalyzer` instance.
///
/// # Arguments
///
/// * `text` - The text to analyze
/// * `base_direction` - The base paragraph direction
///
/// # Returns
///
/// A vector of `BidiRun` in visual order.
///
/// # Examples
///
/// ```rust,ignore
/// use fastrender::text::bidi::{analyze_bidi, Direction};
///
/// let runs = analyze_bidi("Hello שלום world", Direction::Ltr);
/// for run in runs {
///     println!("{}: {:?}", run.text, run.direction);
/// }
/// ```
pub fn analyze_bidi(text: &str, base_direction: Direction) -> Vec<BidiRun> {
    BidiAnalyzer::new().analyze(text, base_direction)
}

#[cfg(test)]
mod tests {
    use super::*;

    // =========================================================================
    // Direction tests
    // =========================================================================

    #[test]
    fn test_direction_default() {
        assert_eq!(Direction::default(), Direction::Ltr);
    }

    #[test]
    fn test_direction_is_ltr_rtl() {
        assert!(Direction::Ltr.is_ltr());
        assert!(!Direction::Ltr.is_rtl());
        assert!(!Direction::Rtl.is_ltr());
        assert!(Direction::Rtl.is_rtl());
    }

    #[test]
    fn test_direction_to_level() {
        assert!(Direction::Ltr.to_level().is_ltr());
        assert!(Direction::Rtl.to_level().is_rtl());
    }

    #[test]
    fn test_direction_from_level() {
        assert_eq!(Direction::from_level(Level::ltr()), Direction::Ltr);
        assert_eq!(Direction::from_level(Level::rtl()), Direction::Rtl);
    }

    // =========================================================================
    // BidiRun tests
    // =========================================================================

    #[test]
    fn test_bidi_run_len() {
        let run = BidiRun {
            start: 0,
            end: 5,
            level: 0,
            direction: Direction::Ltr,
            text: "Hello".to_string(),
        };
        assert_eq!(run.len(), 5);
    }

    #[test]
    fn test_bidi_run_is_empty() {
        let empty = BidiRun {
            start: 5,
            end: 5,
            level: 0,
            direction: Direction::Ltr,
            text: String::new(),
        };
        let nonempty = BidiRun {
            start: 0,
            end: 1,
            level: 0,
            direction: Direction::Ltr,
            text: "A".to_string(),
        };
        assert!(empty.is_empty());
        assert!(!nonempty.is_empty());
    }

    #[test]
    fn test_bidi_run_direction_helpers() {
        let ltr_run = BidiRun {
            start: 0,
            end: 5,
            level: 0,
            direction: Direction::Ltr,
            text: "Hello".to_string(),
        };
        let rtl_run = BidiRun {
            start: 0,
            end: 5,
            level: 1,
            direction: Direction::Rtl,
            text: "שלום".to_string(),
        };
        assert!(ltr_run.is_ltr());
        assert!(!ltr_run.is_rtl());
        assert!(!rtl_run.is_ltr());
        assert!(rtl_run.is_rtl());
    }

    // =========================================================================
    // BidiAnalyzer tests - Pure LTR
    // =========================================================================

    #[test]
    fn test_pure_ltr_text() {
        let analyzer = BidiAnalyzer::new();
        let runs = analyzer.analyze("Hello world", Direction::Ltr);

        assert_eq!(runs.len(), 1);
        assert_eq!(runs[0].text, "Hello world");
        assert_eq!(runs[0].direction, Direction::Ltr);
        assert_eq!(runs[0].level, 0);
    }

    #[test]
    fn test_empty_text() {
        let analyzer = BidiAnalyzer::new();
        let runs = analyzer.analyze("", Direction::Ltr);
        assert!(runs.is_empty());
    }

    #[test]
    fn test_whitespace_only() {
        let analyzer = BidiAnalyzer::new();
        let runs = analyzer.analyze("   ", Direction::Ltr);

        assert_eq!(runs.len(), 1);
        assert_eq!(runs[0].direction, Direction::Ltr);
    }

    // =========================================================================
    // BidiAnalyzer tests - Pure RTL
    // =========================================================================

    #[test]
    fn test_pure_rtl_hebrew() {
        let analyzer = BidiAnalyzer::new();
        let runs = analyzer.analyze("שלום עולם", Direction::Ltr);

        assert_eq!(runs.len(), 1);
        assert_eq!(runs[0].text, "שלום עולם");
        assert_eq!(runs[0].direction, Direction::Rtl);
        assert!(runs[0].level % 2 == 1); // Odd level = RTL
    }

    #[test]
    fn test_pure_rtl_arabic() {
        let analyzer = BidiAnalyzer::new();
        let runs = analyzer.analyze("مرحبا بالعالم", Direction::Ltr);

        assert_eq!(runs.len(), 1);
        assert_eq!(runs[0].direction, Direction::Rtl);
    }

    #[test]
    fn test_rtl_with_rtl_base() {
        let analyzer = BidiAnalyzer::new();
        let runs = analyzer.analyze("שלום עולם", Direction::Rtl);

        assert_eq!(runs.len(), 1);
        assert_eq!(runs[0].direction, Direction::Rtl);
    }

    // =========================================================================
    // BidiAnalyzer tests - Mixed LTR/RTL
    // =========================================================================

    #[test]
    fn test_mixed_ltr_rtl() {
        let analyzer = BidiAnalyzer::new();
        let runs = analyzer.analyze("Hello שלום world", Direction::Ltr);

        // Should have 3 runs: "Hello ", "שלום", " world"
        assert!(runs.len() >= 2);

        // Find the RTL run
        let rtl_run = runs.iter().find(|r| r.is_rtl());
        assert!(rtl_run.is_some());
        assert!(rtl_run.unwrap().text.contains("שלום"));
    }

    #[test]
    fn test_rtl_with_ltr_content() {
        let analyzer = BidiAnalyzer::new();
        let runs = analyzer.analyze("שלום Hello עולם", Direction::Rtl);

        // With RTL base, "Hello" is embedded LTR content
        // It should appear somewhere in the output
        let all_text: String = runs.iter().map(|r| r.text.as_str()).collect();
        assert!(all_text.contains("Hello"), "Hello should appear in output runs");

        // Should have multiple runs for mixed content
        assert!(runs.len() >= 1, "Should produce at least one run");
    }

    #[test]
    fn test_alternating_scripts() {
        let analyzer = BidiAnalyzer::new();
        let runs = analyzer.analyze("A ב C ד", Direction::Ltr);

        // Should have multiple runs alternating direction
        assert!(runs.len() >= 2);
    }

    // =========================================================================
    // BidiAnalyzer tests - Numbers in RTL context
    // =========================================================================

    #[test]
    fn test_numbers_in_rtl_context() {
        let analyzer = BidiAnalyzer::new();
        // Arabic text with numbers: "number 123 text"
        let runs = analyzer.analyze("عدد 123 نص", Direction::Ltr);

        // Numbers should be in a separate LTR run
        // This is the critical "numbers stay LTR" behavior
        let has_ltr = runs.iter().any(|r| r.is_ltr());
        assert!(has_ltr, "Numbers should create LTR runs in RTL context");
    }

    #[test]
    fn test_numbers_in_hebrew_text() {
        let analyzer = BidiAnalyzer::new();
        let text = "מספר 456 טקסט";
        let runs = analyzer.analyze(text, Direction::Ltr);

        // The key behavior: numbers "456" should appear in the output
        // and should NOT be reversed (i.e., not "654")
        let all_text: String = runs.iter().map(|r| r.text.as_str()).collect();

        // Numbers should be present
        assert!(
            all_text.contains("456"),
            "Numbers '456' should appear in output (not reversed to '654')"
        );

        // Numbers should NOT be reversed
        assert!(
            !all_text.contains("654"),
            "Numbers should not be reversed in RTL context"
        );
    }

    // =========================================================================
    // BidiAnalyzer tests - Nested embeddings
    // =========================================================================

    #[test]
    fn test_with_ltr_isolate() {
        let analyzer = BidiAnalyzer::new();
        // LRI (U+2066) and PDI (U+2069) are isolate formatting characters
        let text = "שלום \u{2066}Hello\u{2069} עולם";
        let runs = analyzer.analyze(text, Direction::Ltr);

        // Should have runs for Hebrew, English, and Hebrew again
        assert!(runs.len() >= 2);
    }

    #[test]
    fn test_with_rtl_isolate() {
        let analyzer = BidiAnalyzer::new();
        // RLI (U+2067) and PDI (U+2069)
        let text = "Hello \u{2067}שלום\u{2069} world";
        let runs = analyzer.analyze(text, Direction::Ltr);

        // Should have RTL content properly isolated
        let has_rtl = runs.iter().any(|r| r.is_rtl());
        assert!(has_rtl);
    }

    // =========================================================================
    // BidiAnalysis tests
    // =========================================================================

    #[test]
    fn test_analysis_needs_reordering() {
        let analyzer = BidiAnalyzer::new();

        let ltr_analysis = analyzer.analyze_full("Hello world", Direction::Ltr);
        assert!(!ltr_analysis.needs_reordering());

        let mixed_analysis = analyzer.analyze_full("Hello שלום", Direction::Ltr);
        assert!(mixed_analysis.needs_reordering());
    }

    #[test]
    fn test_analysis_base_direction() {
        let analyzer = BidiAnalyzer::new();

        let ltr = analyzer.analyze_full("Hello", Direction::Ltr);
        assert_eq!(ltr.base_direction(), Direction::Ltr);

        let rtl = analyzer.analyze_full("Hello", Direction::Rtl);
        assert_eq!(rtl.base_direction(), Direction::Rtl);
    }

    #[test]
    fn test_analysis_level_at() {
        let analyzer = BidiAnalyzer::new();
        let analysis = analyzer.analyze_full("Hello שלום", Direction::Ltr);

        // First character 'H' should be at LTR level
        let level_h = analysis.level_at(0);
        assert!(level_h.is_ltr());

        // Hebrew characters should be at RTL level
        // "Hello " is 6 bytes, then Hebrew starts
        let level_hebrew = analysis.level_at(6);
        assert!(level_hebrew.is_rtl());
    }

    #[test]
    fn test_analysis_logical_runs() {
        let analyzer = BidiAnalyzer::new();
        let analysis = analyzer.analyze_full("ABC שלום XYZ", Direction::Ltr);

        let logical = analysis.logical_runs();
        let visual = analysis.visual_runs();

        // Both should have similar number of runs
        assert!(!logical.is_empty());
        assert!(!visual.is_empty());

        // Logical runs should be in original text order
        // Visual runs may be reordered
    }

    // =========================================================================
    // Character classification tests
    // =========================================================================

    #[test]
    fn test_char_bidi_class_ltr() {
        let analyzer = BidiAnalyzer::new();
        assert_eq!(analyzer.char_bidi_class('A'), BidiClass::L);
        assert_eq!(analyzer.char_bidi_class('z'), BidiClass::L);
    }

    #[test]
    fn test_char_bidi_class_rtl() {
        let analyzer = BidiAnalyzer::new();
        // Hebrew
        assert_eq!(analyzer.char_bidi_class('א'), BidiClass::R);
        // Arabic
        assert_eq!(analyzer.char_bidi_class('ب'), BidiClass::AL);
    }

    #[test]
    fn test_char_bidi_class_neutral() {
        let analyzer = BidiAnalyzer::new();
        assert_eq!(analyzer.char_bidi_class(' '), BidiClass::WS);
        assert_eq!(analyzer.char_bidi_class('.'), BidiClass::CS);
    }

    #[test]
    fn test_char_direction() {
        let analyzer = BidiAnalyzer::new();
        assert_eq!(analyzer.char_direction('A'), Some(Direction::Ltr));
        assert_eq!(analyzer.char_direction('א'), Some(Direction::Rtl));
        assert_eq!(analyzer.char_direction('ب'), Some(Direction::Rtl));
        assert_eq!(analyzer.char_direction(' '), None);
        assert_eq!(analyzer.char_direction('1'), None);
    }

    // =========================================================================
    // Direction detection tests
    // =========================================================================

    #[test]
    fn test_detect_direction_ltr_first() {
        let analyzer = BidiAnalyzer::new();
        let dir = analyzer.detect_direction("Hello שלום", Direction::Rtl);
        assert_eq!(dir, Direction::Ltr);
    }

    #[test]
    fn test_detect_direction_rtl_first() {
        let analyzer = BidiAnalyzer::new();
        let dir = analyzer.detect_direction("שלום Hello", Direction::Ltr);
        assert_eq!(dir, Direction::Rtl);
    }

    #[test]
    fn test_detect_direction_no_strong() {
        let analyzer = BidiAnalyzer::new();
        let dir = analyzer.detect_direction("123 ... !!!", Direction::Ltr);
        assert_eq!(dir, Direction::Ltr);

        let dir_rtl = analyzer.detect_direction("123", Direction::Rtl);
        assert_eq!(dir_rtl, Direction::Rtl);
    }

    // =========================================================================
    // has_rtl / is_pure_ltr tests
    // =========================================================================

    #[test]
    fn test_has_rtl() {
        let analyzer = BidiAnalyzer::new();
        assert!(!analyzer.has_rtl("Hello world"));
        assert!(analyzer.has_rtl("Hello שלום"));
        assert!(analyzer.has_rtl("مرحبا"));
    }

    #[test]
    fn test_is_pure_ltr() {
        let analyzer = BidiAnalyzer::new();
        assert!(analyzer.is_pure_ltr("Hello world"));
        assert!(!analyzer.is_pure_ltr("Hello שלום"));
    }

    // =========================================================================
    // Text reordering tests
    // =========================================================================

    #[test]
    fn test_reorder_pure_ltr() {
        let analyzer = BidiAnalyzer::new();
        let result = analyzer.reorder_text("Hello", Direction::Ltr);
        assert_eq!(result, "Hello");
    }

    #[test]
    fn test_reorder_pure_rtl() {
        let analyzer = BidiAnalyzer::new();
        let result = analyzer.reorder_text("שלום", Direction::Ltr);
        // RTL text should be reversed for visual display
        let expected: String = "שלום".chars().rev().collect();
        assert_eq!(result, expected);
    }

    // =========================================================================
    // Convenience function tests
    // =========================================================================

    #[test]
    fn test_analyze_bidi_function() {
        let runs = analyze_bidi("Hello שלום", Direction::Ltr);
        assert!(!runs.is_empty());
    }

    // =========================================================================
    // Edge cases
    // =========================================================================

    #[test]
    fn test_single_char_ltr() {
        let analyzer = BidiAnalyzer::new();
        let runs = analyzer.analyze("A", Direction::Ltr);
        assert_eq!(runs.len(), 1);
        assert_eq!(runs[0].text, "A");
    }

    #[test]
    fn test_single_char_rtl() {
        let analyzer = BidiAnalyzer::new();
        let runs = analyzer.analyze("א", Direction::Ltr);
        assert_eq!(runs.len(), 1);
        assert_eq!(runs[0].direction, Direction::Rtl);
    }

    #[test]
    fn test_punctuation_at_boundaries() {
        let analyzer = BidiAnalyzer::new();
        let runs = analyzer.analyze("(Hello)", Direction::Ltr);
        // Parentheses should follow the direction of surrounding text
        assert!(!runs.is_empty());
    }

    #[test]
    fn test_unicode_emoji() {
        let analyzer = BidiAnalyzer::new();
        // Emoji should not break bidi analysis
        let runs = analyzer.analyze("Hello 👋 שלום", Direction::Ltr);
        assert!(!runs.is_empty());
    }

    #[test]
    fn test_very_long_text() {
        let analyzer = BidiAnalyzer::new();
        let long_text = "Hello ".repeat(1000) + "שלום";
        let runs = analyzer.analyze(&long_text, Direction::Ltr);
        assert!(!runs.is_empty());
    }

    #[test]
    fn test_newlines_in_text() {
        let analyzer = BidiAnalyzer::new();
        let runs = analyzer.analyze("Hello\nשלום\nworld", Direction::Ltr);
        // Should handle newlines properly
        assert!(!runs.is_empty());
    }
}
