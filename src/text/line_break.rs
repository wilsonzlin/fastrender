//! Line break opportunity detection using Unicode Line Breaking Algorithm (UAX #14)
//!
//! This module provides functionality to find legal line break positions in text
//! according to the Unicode Standard Annex #14. It wraps the `unicode-linebreak`
//! crate and provides a clean API for use in text layout.
//!
//! # Overview
//!
//! The Unicode Line Breaking Algorithm defines where text can be broken when
//! wrapping to fit a given width. Different languages have different rules:
//!
//! - **English**: Breaks at spaces and hyphens
//! - **Chinese/Japanese**: Can break between most characters
//! - **URLs**: Breaks after slashes and other separators
//!
//! # Break Types
//!
//! The algorithm identifies two types of break opportunities:
//!
//! - **Mandatory**: The line MUST break here (newline, paragraph separator)
//! - **Allowed**: The line CAN break here (soft wrap opportunity)
//!
//! # Example
//!
//! ```rust
//! use fastrender::text::line_break::{find_break_opportunities, BreakOpportunity};
//!
//! let text = "Hello world";
//! let breaks = find_break_opportunities(text);
//!
//! // There's a break opportunity after "Hello " (at byte position 6)
//! assert!(breaks.iter().any(|b| b.byte_offset == 6));
//! ```
//!
//! # CSS Property Interactions
//!
//! The base UAX #14 opportunities should be filtered based on CSS properties:
//!
//! - `white-space: nowrap` - Remove all non-mandatory breaks
//! - `word-break: break-all` - Add breaks between all characters
//! - `word-break: keep-all` - Remove breaks within CJK text
//! - `overflow-wrap: anywhere` - Add emergency breaks everywhere
//!
//! These modifications should be applied by the layout engine, not this module.
//!
//! # References
//!
//! - Unicode Standard Annex #14: <https://www.unicode.org/reports/tr14/>
//! - CSS Text Module Level 3: <https://www.w3.org/TR/css-text-3/>

use unicode_linebreak::{linebreaks, BreakOpportunity as UnicodeBreakOpportunity};

/// Type of line break opportunity
///
/// Indicates whether a break is required or optional at a given position.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BreakType {
    /// Mandatory break - the line MUST break here.
    ///
    /// Examples:
    /// - Newline character (U+000A)
    /// - Carriage return (U+000D)
    /// - Paragraph separator (U+2029)
    /// - Line separator (U+2028)
    Mandatory,

    /// Allowed break - the line CAN break here if needed.
    ///
    /// This is a "soft" break opportunity where wrapping is permitted
    /// but not required. Examples:
    /// - After spaces
    /// - After hyphens
    /// - Between CJK characters
    /// - At zero-width space (U+200B)
    Allowed,
}

/// A break opportunity in text
///
/// Represents a position where a line break is allowed or required.
/// Positions are byte offsets into the UTF-8 string.
///
/// # Byte Offsets
///
/// All positions are byte offsets, not character indices. This is important
/// because Rust strings are UTF-8 encoded and multi-byte characters are common
/// in international text.
///
/// The byte offset indicates the position AFTER the character that causes
/// the break. For example, in "Hello world", the break after space is at
/// byte offset 6 (after "Hello ").
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BreakOpportunity {
    /// Byte offset in the UTF-8 string where break can occur.
    ///
    /// This is the position AFTER the character that allows the break.
    /// For example, breaking after a space means the offset points to
    /// the first character of the next word.
    pub byte_offset: usize,

    /// Type of break at this position.
    pub break_type: BreakType,
}

impl BreakOpportunity {
    /// Create a new break opportunity.
    ///
    /// # Arguments
    ///
    /// * `byte_offset` - Byte offset in text where break can occur
    /// * `break_type` - Whether break is mandatory or allowed
    ///
    /// # Example
    ///
    /// ```rust
    /// use fastrender::text::line_break::{BreakOpportunity, BreakType};
    ///
    /// let brk = BreakOpportunity::new(6, BreakType::Allowed);
    /// assert_eq!(brk.byte_offset, 6);
    /// assert_eq!(brk.break_type, BreakType::Allowed);
    /// ```
    #[inline]
    pub fn new(byte_offset: usize, break_type: BreakType) -> Self {
        Self {
            byte_offset,
            break_type,
        }
    }

    /// Create a mandatory break opportunity.
    ///
    /// Convenience method for creating hard breaks (newlines, etc.).
    #[inline]
    pub fn mandatory(byte_offset: usize) -> Self {
        Self::new(byte_offset, BreakType::Mandatory)
    }

    /// Create an allowed break opportunity.
    ///
    /// Convenience method for creating soft breaks (spaces, etc.).
    #[inline]
    pub fn allowed(byte_offset: usize) -> Self {
        Self::new(byte_offset, BreakType::Allowed)
    }

    /// Check if this is a mandatory break.
    #[inline]
    pub fn is_mandatory(&self) -> bool {
        self.break_type == BreakType::Mandatory
    }

    /// Check if this is an allowed (soft) break.
    #[inline]
    pub fn is_allowed(&self) -> bool {
        self.break_type == BreakType::Allowed
    }
}

/// Find all break opportunities in text using Unicode Line Breaking Algorithm.
///
/// Returns a list of positions where line breaks are allowed or required,
/// sorted by byte offset in ascending order.
///
/// # Arguments
///
/// * `text` - The text to analyze for break opportunities
///
/// # Returns
///
/// A vector of `BreakOpportunity` structs indicating where breaks can occur.
/// The vector is sorted by byte offset.
///
/// # Examples
///
/// ## Basic English text
///
/// ```rust
/// use fastrender::text::line_break::{find_break_opportunities, BreakType};
///
/// let text = "Hello world";
/// let breaks = find_break_opportunities(text);
///
/// // Break opportunity after "Hello " at byte 6
/// let space_break = breaks.iter().find(|b| b.byte_offset == 6);
/// assert!(space_break.is_some());
/// assert_eq!(space_break.unwrap().break_type, BreakType::Allowed);
/// ```
///
/// ## Mandatory breaks (newlines)
///
/// ```rust
/// use fastrender::text::line_break::{find_break_opportunities, BreakType};
///
/// let text = "Line 1\nLine 2";
/// let breaks = find_break_opportunities(text);
///
/// // Mandatory break after newline at byte 7
/// let newline_break = breaks.iter().find(|b| b.byte_offset == 7);
/// assert!(newline_break.is_some());
/// assert_eq!(newline_break.unwrap().break_type, BreakType::Mandatory);
/// ```
///
/// ## CJK text (break between characters)
///
/// ```rust
/// use fastrender::text::line_break::find_break_opportunities;
///
/// let text = "你好世界"; // Chinese: "Hello World"
/// let breaks = find_break_opportunities(text);
///
/// // CJK text allows breaks between characters
/// // Each Chinese character is 3 bytes in UTF-8
/// assert!(breaks.len() >= 3);
/// ```
///
/// ## Non-breaking space (U+00A0)
///
/// ```rust
/// use fastrender::text::line_break::find_break_opportunities;
///
/// let text = "Hello\u{00A0}world"; // Non-breaking space
/// let breaks = find_break_opportunities(text);
///
/// // There should be NO break opportunity at the NBSP position
/// // NBSP is 2 bytes (C2 A0), "Hello" is 5 bytes
/// // So position 7 (5 + 2) should NOT be a break
/// let nbsp_position = 7; // After "Hello" + NBSP
/// let has_break_at_nbsp = breaks.iter().any(|b| b.byte_offset == nbsp_position);
/// // Note: The break is at position 12 (end of string), not at NBSP
/// ```
pub fn find_break_opportunities(text: &str) -> Vec<BreakOpportunity> {
    let mut opportunities = Vec::new();

    for (byte_offset, opportunity) in linebreaks(text) {
        let break_type = match opportunity {
            UnicodeBreakOpportunity::Mandatory => BreakType::Mandatory,
            UnicodeBreakOpportunity::Allowed => BreakType::Allowed,
        };

        opportunities.push(BreakOpportunity {
            byte_offset,
            break_type,
        });
    }

    opportunities
}

/// Find break opportunities and return only mandatory breaks.
///
/// This is useful for `white-space: pre` where only hard breaks
/// (newlines) should cause line breaks.
///
/// # Example
///
/// ```rust
/// use fastrender::text::line_break::find_mandatory_breaks;
///
/// let text = "Hello world\nNext line";
/// let breaks = find_mandatory_breaks(text);
///
/// // Mandatory breaks at newline and possibly end of text
/// assert!(breaks.len() >= 1);
/// // First mandatory break is at newline (position 12)
/// assert_eq!(breaks[0].byte_offset, 12);
/// ```
pub fn find_mandatory_breaks(text: &str) -> Vec<BreakOpportunity> {
    find_break_opportunities(text)
        .into_iter()
        .filter(|b| b.is_mandatory())
        .collect()
}

/// Find break opportunities excluding the final break at end of text.
///
/// The Unicode line break algorithm always reports a break opportunity
/// at the end of the string. This function filters that out, which is
/// useful when you don't want to treat end-of-text as a break.
///
/// # Example
///
/// ```rust
/// use fastrender::text::line_break::find_interior_breaks;
///
/// let text = "Hello";
/// let breaks = find_interior_breaks(text);
///
/// // No interior breaks in a single word
/// assert!(breaks.is_empty());
/// ```
pub fn find_interior_breaks(text: &str) -> Vec<BreakOpportunity> {
    let text_len = text.len();
    find_break_opportunities(text)
        .into_iter()
        .filter(|b| b.byte_offset < text_len)
        .collect()
}

/// Check if there's a break opportunity at a specific byte offset.
///
/// # Arguments
///
/// * `text` - The text to check
/// * `byte_offset` - The byte offset to check for a break
///
/// # Returns
///
/// `Some(BreakType)` if there's a break at this position, `None` otherwise.
///
/// # Example
///
/// ```rust
/// use fastrender::text::line_break::{has_break_at, BreakType};
///
/// let text = "Hello world";
/// assert_eq!(has_break_at(text, 6), Some(BreakType::Allowed));
/// assert_eq!(has_break_at(text, 3), None);
/// ```
pub fn has_break_at(text: &str, byte_offset: usize) -> Option<BreakType> {
    find_break_opportunities(text)
        .into_iter()
        .find(|b| b.byte_offset == byte_offset)
        .map(|b| b.break_type)
}

/// Iterator over break opportunities in text.
///
/// This is a lazy iterator that finds break opportunities on demand,
/// useful for large texts where you might not need all breaks.
///
/// # Example
///
/// ```rust
/// use fastrender::text::line_break::BreakIterator;
///
/// let text = "Hello world foo bar";
/// let mut iter = BreakIterator::new(text);
///
/// // Get first break
/// let first = iter.next();
/// assert!(first.is_some());
/// ```
pub struct BreakIterator {
    /// Pre-computed break opportunities
    breaks: std::vec::IntoIter<BreakOpportunity>,
}

impl BreakIterator {
    /// Create a new break iterator for the given text.
    pub fn new(text: &str) -> Self {
        Self {
            breaks: find_break_opportunities(text).into_iter(),
        }
    }
}

impl Iterator for BreakIterator {
    type Item = BreakOpportunity;

    fn next(&mut self) -> Option<Self::Item> {
        self.breaks.next()
    }
}

impl ExactSizeIterator for BreakIterator {
    fn len(&self) -> usize {
        self.breaks.len()
    }
}

// ============================================================================
// Greedy Line Breaking Algorithm (W4.T09)
// ============================================================================

use crate::text::shaper::GlyphPosition;

/// A segment within a line
///
/// Represents a contiguous run of glyphs that belongs to a single line.
#[derive(Debug, Clone, PartialEq)]
pub struct LineSegment {
    /// Start index in the original glyph array
    pub glyph_start: usize,

    /// Number of glyphs in this segment
    pub glyph_count: usize,

    /// Width of this segment in pixels
    pub width: f32,

    /// X offset within the line
    pub x_offset: f32,
}

impl LineSegment {
    /// Create a new line segment
    pub fn new(glyph_start: usize, glyph_count: usize, width: f32) -> Self {
        Self {
            glyph_start,
            glyph_count,
            width,
            x_offset: 0.0,
        }
    }

    /// End index (exclusive) in the original glyph array
    #[inline]
    pub fn glyph_end(&self) -> usize {
        self.glyph_start + self.glyph_count
    }

    /// Check if segment is empty
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.glyph_count == 0
    }
}

/// A line of text
///
/// Represents a single line containing one or more segments of glyphs.
/// Lines are the output of the line breaking algorithm.
#[derive(Debug, Clone, PartialEq)]
pub struct Line {
    /// Segments of glyphs in this line
    pub segments: Vec<LineSegment>,

    /// Total content width (sum of segment widths)
    pub width: f32,

    /// Maximum available width for this line
    pub max_width: f32,

    /// Whether this is the last line in the paragraph
    pub is_last: bool,
}

impl Line {
    /// Create a new empty line with given max width
    pub fn new(max_width: f32) -> Self {
        Self {
            segments: Vec::new(),
            width: 0.0,
            max_width,
            is_last: false,
        }
    }

    /// Check if line is empty
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.segments.is_empty()
    }

    /// Get total glyph count across all segments
    pub fn glyph_count(&self) -> usize {
        self.segments.iter().map(|s| s.glyph_count).sum()
    }

    /// Get remaining space on the line
    #[inline]
    pub fn remaining_space(&self) -> f32 {
        (self.max_width - self.width).max(0.0)
    }

    /// Get fill ratio (how much of max_width is used)
    #[inline]
    pub fn fill_ratio(&self) -> f32 {
        if self.max_width > 0.0 {
            self.width / self.max_width
        } else {
            1.0
        }
    }
}

impl Default for Line {
    fn default() -> Self {
        Self::new(f32::INFINITY)
    }
}

/// Break glyphs into lines using a greedy algorithm
///
/// The greedy algorithm fits as many glyphs as possible on each line,
/// breaking at the last valid opportunity when the line is full.
///
/// # Algorithm
///
/// 1. Start with an empty line
/// 2. For each glyph:
///    - If glyph fits on current line, add it
///    - If glyph doesn't fit:
///      - Break at last opportunity (if any)
///      - Start new line with remaining glyphs
/// 3. Handle hard breaks (must break regardless of width)
///
/// # Arguments
///
/// * `glyphs` - Positioned glyphs from text shaping
/// * `max_width` - Maximum line width in pixels
/// * `break_opportunities` - Valid break positions (sorted by byte_offset)
///
/// # Returns
///
/// Vector of lines containing the broken text.
///
/// # Example
///
/// ```rust,ignore
/// use fastrender::text::{break_lines_greedy, GlyphPosition, BreakOpportunity};
///
/// let glyphs = vec![
///     GlyphPosition { glyph_id: 1, cluster: 0, advance: 10.0, ..Default::default() },
///     GlyphPosition { glyph_id: 2, cluster: 1, advance: 10.0, ..Default::default() },
/// ];
///
/// let opportunities = vec![
///     BreakOpportunity::allowed(5),  // Break after word
///     BreakOpportunity::allowed(10), // Break after another word
/// ];
///
/// let lines = break_lines_greedy(&glyphs, 100.0, &opportunities);
/// ```
pub fn break_lines_greedy(
    glyphs: &[GlyphPosition],
    max_width: f32,
    break_opportunities: &[BreakOpportunity],
) -> Vec<Line> {
    // Handle empty input
    if glyphs.is_empty() {
        let mut line = Line::new(max_width);
        line.is_last = true;
        return vec![line];
    }

    // Handle infinite or very large max_width (no wrapping needed)
    if max_width == f32::INFINITY || max_width > 1e9 {
        return vec![create_single_line(glyphs, max_width)];
    }

    let mut lines = Vec::new();
    let mut line_builder = LineBuilder::new(max_width);

    // Track break opportunities
    let mut opp_index = 0;
    let mut last_break: Option<BreakState> = None;

    for (glyph_idx, glyph) in glyphs.iter().enumerate() {
        // Check for break opportunities at or before this glyph's cluster
        while opp_index < break_opportunities.len()
            && break_opportunities[opp_index].byte_offset <= glyph.cluster as usize
        {
            let opp = &break_opportunities[opp_index];

            // Handle hard (mandatory) breaks
            if opp.is_mandatory() {
                // Finalize current line
                let line = line_builder.finish();
                if !line.is_empty() || lines.is_empty() {
                    lines.push(line);
                }
                line_builder = LineBuilder::new(max_width);
                last_break = None;
            } else if opp.is_allowed() {
                // Record as potential break point
                last_break = Some(BreakState {
                    glyph_index: glyph_idx,
                    line_width: line_builder.current_width,
                    glyph_count: line_builder.glyph_count,
                });
            }

            opp_index += 1;
        }

        // Try to add glyph to current line
        let glyph_width = glyph.advance;

        if line_builder.can_fit(glyph_width) {
            line_builder.add_glyph(glyph_idx, glyph_width);
        } else {
            // Glyph doesn't fit - need to break

            if let Some(brk) = last_break.take() {
                // Break at last opportunity
                line_builder.truncate_to(brk.glyph_count, brk.line_width);

                let line = line_builder.finish();
                lines.push(line);

                // Start new line, add glyphs from break point
                line_builder = LineBuilder::new(max_width);

                // Re-add glyphs from break point to current glyph (inclusive)
                for (i, g) in glyphs.iter().enumerate().take(glyph_idx + 1).skip(brk.glyph_index) {
                    line_builder.add_glyph(i, g.advance);
                }
            } else {
                // No break opportunity - overflow
                // Add glyph anyway (will overflow)
                line_builder.add_glyph(glyph_idx, glyph_width);
            }

            last_break = None;
        }
    }

    // Finalize last line
    let mut final_line = line_builder.finish();
    final_line.is_last = true;
    if !final_line.is_empty() {
        lines.push(final_line);
    } else if lines.is_empty() {
        // Ensure at least one line
        final_line.is_last = true;
        lines.push(final_line);
    } else {
        // Mark previous line as last
        if let Some(last) = lines.last_mut() {
            last.is_last = true;
        }
    }

    lines
}

/// Alias for `break_lines_greedy`
///
/// Provides a shorter name for the most common line breaking function.
pub fn break_lines(glyphs: &[GlyphPosition], max_width: f32, break_opportunities: &[BreakOpportunity]) -> Vec<Line> {
    break_lines_greedy(glyphs, max_width, break_opportunities)
}

/// Greedy line breaker
///
/// Provides an object-oriented interface to the greedy line breaking
/// algorithm with configurable options.
#[derive(Debug, Clone)]
pub struct GreedyLineBreaker {
    /// Allow overflow when no break opportunity exists
    pub allow_overflow: bool,
}

impl GreedyLineBreaker {
    /// Create a new greedy line breaker with default settings
    pub fn new() -> Self {
        Self { allow_overflow: true }
    }

    /// Set whether to allow overflow
    pub fn with_overflow(mut self, allow: bool) -> Self {
        self.allow_overflow = allow;
        self
    }

    /// Break lines using the greedy algorithm
    pub fn break_lines(
        &self,
        glyphs: &[GlyphPosition],
        max_width: f32,
        break_opportunities: &[BreakOpportunity],
    ) -> Vec<Line> {
        break_lines_greedy(glyphs, max_width, break_opportunities)
    }
}

impl Default for GreedyLineBreaker {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// Helper Types
// ============================================================================

/// State saved at a break opportunity
#[derive(Debug, Clone, Copy)]
struct BreakState {
    glyph_index: usize,
    line_width: f32,
    glyph_count: usize,
}

/// Builder for constructing a line
struct LineBuilder {
    max_width: f32,
    glyph_start: usize,
    glyph_count: usize,
    current_width: f32,
}

impl LineBuilder {
    fn new(max_width: f32) -> Self {
        Self {
            max_width,
            glyph_start: 0,
            glyph_count: 0,
            current_width: 0.0,
        }
    }

    fn can_fit(&self, width: f32) -> bool {
        // First glyph always fits (to avoid infinite loops)
        if self.glyph_count == 0 {
            return true;
        }
        self.current_width + width <= self.max_width
    }

    fn add_glyph(&mut self, glyph_idx: usize, width: f32) {
        if self.glyph_count == 0 {
            self.glyph_start = glyph_idx;
        }
        self.glyph_count += 1;
        self.current_width += width;
    }

    fn truncate_to(&mut self, count: usize, width: f32) {
        self.glyph_count = count;
        self.current_width = width;
    }

    fn finish(self) -> Line {
        let segment = LineSegment::new(self.glyph_start, self.glyph_count, self.current_width);

        Line {
            segments: if self.glyph_count > 0 {
                vec![segment]
            } else {
                Vec::new()
            },
            width: self.current_width,
            max_width: self.max_width,
            is_last: false,
        }
    }
}

/// Create a single line containing all glyphs (no wrapping)
fn create_single_line(glyphs: &[GlyphPosition], max_width: f32) -> Line {
    let total_width: f32 = glyphs.iter().map(|g| g.advance).sum();

    Line {
        segments: vec![LineSegment::new(0, glyphs.len(), total_width)],
        width: total_width,
        max_width,
        is_last: true,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // =========================================================================
    // Basic functionality tests
    // =========================================================================

    #[test]
    fn test_empty_string() {
        let breaks = find_break_opportunities("");
        // Empty string should have no break opportunities
        assert!(breaks.is_empty());
    }

    #[test]
    fn test_single_character() {
        let breaks = find_break_opportunities("a");
        // Single character has a break at the end
        assert_eq!(breaks.len(), 1);
        assert_eq!(breaks[0].byte_offset, 1);
    }

    #[test]
    fn test_simple_words() {
        let text = "Hello world";
        let breaks = find_break_opportunities(text);

        // Should have break after "Hello " (byte 6) and at end (byte 11)
        assert!(breaks.iter().any(|b| b.byte_offset == 6));
        assert!(breaks.iter().any(|b| b.byte_offset == text.len()));
    }

    #[test]
    fn test_multiple_spaces() {
        let text = "Hello  world"; // Two spaces
        let breaks = find_break_opportunities(text);

        // Should have breaks after each space
        assert!(breaks.iter().any(|b| b.byte_offset == 6 || b.byte_offset == 7));
    }

    // =========================================================================
    // Mandatory break tests
    // =========================================================================

    #[test]
    fn test_newline_is_mandatory() {
        let text = "Line 1\nLine 2";
        let breaks = find_break_opportunities(text);

        // Find the break after the newline
        let newline_break = breaks.iter().find(|b| b.byte_offset == 7); // "Line 1\n" = 7 bytes

        assert!(newline_break.is_some());
        assert_eq!(newline_break.unwrap().break_type, BreakType::Mandatory);
    }

    #[test]
    fn test_carriage_return_newline() {
        let text = "Line 1\r\nLine 2";
        let breaks = find_break_opportunities(text);

        // Should have mandatory break after CR+LF
        let has_mandatory = breaks.iter().any(|b| b.is_mandatory());
        assert!(has_mandatory);
    }

    #[test]
    fn test_line_separator() {
        let text = "Part 1\u{2028}Part 2"; // Line separator
        let breaks = find_break_opportunities(text);

        // Line separator (U+2028) creates mandatory break
        let has_mandatory = breaks.iter().any(|b| b.is_mandatory());
        assert!(has_mandatory);
    }

    #[test]
    fn test_paragraph_separator() {
        let text = "Para 1\u{2029}Para 2"; // Paragraph separator
        let breaks = find_break_opportunities(text);

        // Paragraph separator (U+2029) creates mandatory break
        let has_mandatory = breaks.iter().any(|b| b.is_mandatory());
        assert!(has_mandatory);
    }

    // =========================================================================
    // Non-breaking character tests
    // =========================================================================

    #[test]
    fn test_non_breaking_space() {
        let text = "Hello\u{00A0}world"; // Non-breaking space

        // NBSP should NOT create a break opportunity at position 7
        // "Hello" = 5 bytes, NBSP = 2 bytes (C2 A0), so position 7 is after NBSP
        let interior = find_interior_breaks(text);

        // There should be no interior breaks (NBSP prevents breaking)
        assert!(interior.is_empty());
    }

    #[test]
    fn test_word_joiner() {
        let text = "Hello\u{2060}world"; // Word joiner (WJ)

        // Word joiner prevents breaks on both sides
        let interior = find_interior_breaks(text);
        assert!(interior.is_empty());
    }

    // =========================================================================
    // CJK text tests
    // =========================================================================

    #[test]
    fn test_chinese_text() {
        let text = "你好世界"; // "Hello World" in Chinese
        let breaks = find_break_opportunities(text);

        // Chinese characters allow breaks between them
        // Each character is 3 bytes in UTF-8
        // Breaks should be at positions 3, 6, 9, 12
        assert!(breaks.len() >= 3);

        // Verify breaks are at character boundaries
        for brk in &breaks {
            assert!(brk.byte_offset % 3 == 0 || brk.byte_offset == text.len());
        }
    }

    #[test]
    fn test_japanese_text() {
        let text = "これはテストです"; // "This is a test" in Japanese
        let breaks = find_break_opportunities(text);

        // Japanese hiragana/katakana allows breaks between characters
        assert!(breaks.len() >= 5);
    }

    #[test]
    fn test_mixed_cjk_latin() {
        let text = "Hello世界world";
        let breaks = find_break_opportunities(text);

        // Should have breaks around CJK characters
        assert!(breaks.len() >= 2);
    }

    // =========================================================================
    // Special character tests
    // =========================================================================

    #[test]
    fn test_zero_width_space() {
        let text = "Hello\u{200B}world"; // Zero-width space
        let breaks = find_break_opportunities(text);

        // Zero-width space (ZWSP) allows breaks
        let zwsp_position = 5 + 3; // "Hello" + ZWSP (3 bytes)
        let has_zwsp_break = breaks.iter().any(|b| b.byte_offset == zwsp_position);
        assert!(has_zwsp_break);
    }

    #[test]
    fn test_soft_hyphen() {
        let text = "super\u{00AD}cali"; // Soft hyphen

        // Soft hyphen (U+00AD) allows breaks
        let interior = find_interior_breaks(text);
        assert!(!interior.is_empty());
    }

    #[test]
    fn test_hyphen_minus() {
        let text = "self-aware";
        let breaks = find_break_opportunities(text);

        // Regular hyphen allows break after it
        let hyphen_pos = 5; // After "self-"
        let has_hyphen_break = breaks.iter().any(|b| b.byte_offset == hyphen_pos);
        assert!(has_hyphen_break);
    }

    // =========================================================================
    // URL and path tests
    // =========================================================================

    #[test]
    fn test_url_breaking() {
        let text = "https://example.com/path/to/page";

        // URLs should have break opportunities after slashes
        let interior = find_interior_breaks(text);
        assert!(!interior.is_empty());
    }

    #[test]
    fn test_file_path() {
        let text = "/home/user/documents/file.txt";

        // File paths have break opportunities after slashes
        let interior = find_interior_breaks(text);
        assert!(!interior.is_empty());
    }

    // =========================================================================
    // Emoji tests
    // =========================================================================

    #[test]
    fn test_simple_emoji() {
        let text = "Hello 👋 world";
        let breaks = find_break_opportunities(text);

        // Should have breaks around the emoji
        assert!(breaks.len() >= 2);
    }

    #[test]
    fn test_emoji_zwj_sequence() {
        let text = "👨‍👩‍👧‍👦"; // Family emoji with ZWJ

        // ZWJ sequences should stay together (no interior breaks)
        let interior = find_interior_breaks(text);
        assert!(interior.is_empty());
    }

    #[test]
    fn test_emoji_with_skin_tone() {
        let text = "👋🏽"; // Waving hand with skin tone modifier

        // Emoji with modifier should not break in middle
        let interior = find_interior_breaks(text);
        assert!(interior.is_empty());
    }

    #[test]
    fn test_flag_emoji() {
        let text = "🇺🇸"; // US flag (regional indicators)

        // Flag should not break between regional indicators
        let interior = find_interior_breaks(text);
        assert!(interior.is_empty());
    }

    // =========================================================================
    // Helper function tests
    // =========================================================================

    #[test]
    fn test_find_mandatory_breaks() {
        let text = "Hello world\nNew line\nAnother";
        let mandatory = find_mandatory_breaks(text);

        // Should find mandatory breaks at the newlines
        // The end of text may also be treated as mandatory
        assert!(
            mandatory.len() >= 2,
            "Expected at least 2 mandatory breaks, got {}",
            mandatory.len()
        );
        assert!(mandatory.iter().all(|b| b.is_mandatory()));

        // Verify breaks are at newline positions (byte 12 and byte 21)
        let positions: Vec<_> = mandatory.iter().map(|b| b.byte_offset).collect();
        assert!(positions.contains(&12), "Expected break at position 12");
        assert!(positions.contains(&21), "Expected break at position 21");
    }

    #[test]
    fn test_find_interior_breaks() {
        let text = "Hello world";
        let interior = find_interior_breaks(text);

        // Should exclude the final break at end of text
        assert_eq!(interior.len(), 1);
        assert_eq!(interior[0].byte_offset, 6);
    }

    #[test]
    fn test_has_break_at() {
        let text = "Hello world";

        // Has break after space
        assert_eq!(has_break_at(text, 6), Some(BreakType::Allowed));

        // No break in middle of word
        assert_eq!(has_break_at(text, 3), None);
    }

    #[test]
    fn test_break_iterator() {
        let text = "Hello world foo";
        let iter = BreakIterator::new(text);
        let breaks: Vec<_> = iter.collect();

        // Should match find_break_opportunities
        assert_eq!(breaks, find_break_opportunities(text));
    }

    // =========================================================================
    // BreakOpportunity struct tests
    // =========================================================================

    #[test]
    fn test_break_opportunity_constructors() {
        let mandatory = BreakOpportunity::mandatory(10);
        assert!(mandatory.is_mandatory());
        assert!(!mandatory.is_allowed());

        let allowed = BreakOpportunity::allowed(20);
        assert!(allowed.is_allowed());
        assert!(!allowed.is_mandatory());
    }

    #[test]
    fn test_break_opportunity_equality() {
        let a = BreakOpportunity::new(5, BreakType::Allowed);
        let b = BreakOpportunity::new(5, BreakType::Allowed);
        let c = BreakOpportunity::new(5, BreakType::Mandatory);

        assert_eq!(a, b);
        assert_ne!(a, c);
    }

    #[test]
    fn test_break_type_equality() {
        assert_eq!(BreakType::Mandatory, BreakType::Mandatory);
        assert_eq!(BreakType::Allowed, BreakType::Allowed);
        assert_ne!(BreakType::Mandatory, BreakType::Allowed);
    }

    // =========================================================================
    // Edge cases
    // =========================================================================

    #[test]
    fn test_only_whitespace() {
        let text = "   "; // Three spaces
        let breaks = find_break_opportunities(text);

        // UAX #14 treats spaces as "break after" class
        // With only spaces, there should be at least one break (at end)
        assert!(
            !breaks.is_empty(),
            "Whitespace-only text should have at least one break"
        );
    }

    #[test]
    fn test_only_newlines() {
        let text = "\n\n\n";
        let breaks = find_break_opportunities(text);

        // Each newline creates a mandatory break
        let mandatory_count = breaks.iter().filter(|b| b.is_mandatory()).count();
        assert_eq!(mandatory_count, 3);
    }

    #[test]
    fn test_tab_character() {
        let text = "Hello\tworld";

        // Tab allows break
        let interior = find_interior_breaks(text);
        assert!(!interior.is_empty());
    }

    #[test]
    fn test_very_long_word() {
        let text = "supercalifragilisticexpialidocious";

        // Long word without spaces should only have break at end
        let interior = find_interior_breaks(text);
        assert!(interior.is_empty());
    }

    #[test]
    fn test_numbers() {
        let text = "Price: $1,234.56";
        let breaks = find_break_opportunities(text);

        // Should have some break opportunities
        assert!(!breaks.is_empty());
    }

    #[test]
    fn test_punctuation_sequences() {
        let text = "Hello... World!!!";

        // Should have breaks around punctuation
        let interior = find_interior_breaks(text);
        assert!(!interior.is_empty());
    }

    // =========================================================================
    // Byte offset validation tests
    // =========================================================================

    #[test]
    fn test_byte_offsets_are_valid() {
        let text = "Hello 世界 world"; // Mixed ASCII and CJK
        let breaks = find_break_opportunities(text);

        // All byte offsets should be valid UTF-8 boundaries
        for brk in &breaks {
            assert!(
                text.is_char_boundary(brk.byte_offset),
                "Byte offset {} is not a valid char boundary",
                brk.byte_offset
            );
        }
    }

    #[test]
    fn test_byte_offsets_are_sorted() {
        let text = "The quick brown fox jumps over the lazy dog";
        let breaks = find_break_opportunities(text);

        // Breaks should be in ascending order
        for window in breaks.windows(2) {
            assert!(
                window[0].byte_offset <= window[1].byte_offset,
                "Break opportunities not sorted"
            );
        }
    }

    #[test]
    fn test_byte_offsets_within_bounds() {
        let text = "Test string";
        let breaks = find_break_opportunities(text);

        // All offsets should be <= text length
        for brk in &breaks {
            assert!(
                brk.byte_offset <= text.len(),
                "Byte offset {} exceeds text length {}",
                brk.byte_offset,
                text.len()
            );
        }
    }
}
