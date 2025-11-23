//! Greedy line breaking algorithm
//!
//! This module implements line breaking for text layout according to
//! CSS Text Module Level 3 and Unicode Line Breaking Algorithm (UAX #14).
//!
//! # Overview
//!
//! The greedy line breaking algorithm works as follows:
//! 1. Scan glyphs from left to right, accumulating width
//! 2. Track break opportunities as they are encountered
//! 3. When the accumulated width exceeds max_width, break at the last opportunity
//! 4. Start a new line and repeat
//!
//! # CSS Specification
//!
//! This implements the core of CSS line breaking:
//! - <https://www.w3.org/TR/css-text-3/#line-breaking>
//!
//! # Example
//!
//! ```rust,ignore
//! use fastrender::text::line_breaker::{break_lines, GlyphPosition, BreakOpportunity, BreakType};
//!
//! let glyphs = vec![
//!     GlyphPosition { glyph_id: 1, cluster: 0, x_advance: 10.0, x_offset: 0.0, y_offset: 0.0 },
//!     GlyphPosition { glyph_id: 2, cluster: 1, x_advance: 10.0, x_offset: 0.0, y_offset: 0.0 },
//! ];
//!
//! let opportunities = vec![
//!     BreakOpportunity { position: 0, penalty: 0, break_type: BreakType::Normal },
//! ];
//!
//! let lines = break_lines(&glyphs, 100.0, &opportunities);
//! ```

use crate::error::{Result, TextError};

// ============================================================================
// Type Definitions
// ============================================================================

/// Position and metrics for a single glyph
///
/// Represents a shaped glyph with its advance width and offsets.
/// This is the input to the line breaking algorithm.
///
/// # Example
///
/// ```rust,ignore
/// use fastrender::text::line_breaker::GlyphPosition;
///
/// let glyph = GlyphPosition {
///     glyph_id: 42,
///     cluster: 0,
///     x_advance: 12.5,
///     x_offset: 0.0,
///     y_offset: 0.0,
/// };
/// ```
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct GlyphPosition {
    /// Glyph ID in the font (font-specific identifier)
    pub glyph_id: u32,

    /// Text cluster index (byte offset in original text)
    ///
    /// Used to map glyphs back to characters for break opportunity matching.
    /// Multiple glyphs may share the same cluster (ligatures, complex scripts).
    pub cluster: usize,

    /// Horizontal advance width in pixels
    ///
    /// The distance to move the pen after drawing this glyph.
    pub x_advance: f32,

    /// Horizontal offset from current position in pixels
    ///
    /// Used for kerning and complex positioning.
    pub x_offset: f32,

    /// Vertical offset from baseline in pixels
    ///
    /// Positive values move the glyph up.
    pub y_offset: f32,
}

impl GlyphPosition {
    /// Create a new glyph position with default offsets
    ///
    /// # Arguments
    ///
    /// * `glyph_id` - Font-specific glyph identifier
    /// * `cluster` - Text cluster index
    /// * `x_advance` - Horizontal advance width
    pub fn new(glyph_id: u32, cluster: usize, x_advance: f32) -> Self {
        Self {
            glyph_id,
            cluster,
            x_advance,
            x_offset: 0.0,
            y_offset: 0.0,
        }
    }

    /// Create a new glyph position with all values
    pub fn with_offsets(glyph_id: u32, cluster: usize, x_advance: f32, x_offset: f32, y_offset: f32) -> Self {
        Self {
            glyph_id,
            cluster,
            x_advance,
            x_offset,
            y_offset,
        }
    }
}

/// Type of line break
///
/// Categorizes break points by their origin and behavior.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BreakType {
    /// Normal break (whitespace, UAX #14 allowed break)
    Normal,

    /// Hyphenation break (requires inserting hyphen)
    Hyphen,

    /// Hard break (mandatory: newline, <br>)
    ///
    /// Must break here, regardless of line width.
    Hard,

    /// Emergency break (overflow, no better option)
    ///
    /// Only used when overflow-wrap: anywhere or break-word is set.
    Emergency,
}

impl Default for BreakType {
    fn default() -> Self {
        Self::Normal
    }
}

/// A break opportunity in the text
///
/// Represents a position where a line break can occur, with an
/// associated penalty for breaking there.
///
/// # Penalty Values
///
/// - `0` - Mandatory break (must break here)
/// - `1-100` - Good break (word boundaries)
/// - `100-1000` - Acceptable break (hyphenation)
/// - `1000+` - Poor break (emergency only)
/// - `i32::MAX` - No break allowed
///
/// # Example
///
/// ```rust,ignore
/// use fastrender::text::line_breaker::{BreakOpportunity, BreakType};
///
/// // Good break at space
/// let space_break = BreakOpportunity {
///     position: 5,
///     penalty: 10,
///     break_type: BreakType::Normal,
/// };
///
/// // Mandatory break (newline)
/// let hard_break = BreakOpportunity {
///     position: 10,
///     penalty: 0,
///     break_type: BreakType::Hard,
/// };
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BreakOpportunity {
    /// Byte position in original text (cluster index)
    ///
    /// The break occurs *before* the character at this position.
    pub position: usize,

    /// Penalty for breaking at this position
    ///
    /// Lower values indicate better break points.
    pub penalty: i32,

    /// Type of break
    pub break_type: BreakType,
}

impl BreakOpportunity {
    /// Create a normal break opportunity
    pub fn normal(position: usize) -> Self {
        Self {
            position,
            penalty: 10,
            break_type: BreakType::Normal,
        }
    }

    /// Create a hard (mandatory) break opportunity
    pub fn hard(position: usize) -> Self {
        Self {
            position,
            penalty: 0,
            break_type: BreakType::Hard,
        }
    }

    /// Create a hyphenation break opportunity
    pub fn hyphen(position: usize) -> Self {
        Self {
            position,
            penalty: 100,
            break_type: BreakType::Hyphen,
        }
    }

    /// Create an emergency break opportunity
    pub fn emergency(position: usize) -> Self {
        Self {
            position,
            penalty: 1000,
            break_type: BreakType::Emergency,
        }
    }

    /// Check if this is a mandatory (hard) break
    #[inline]
    pub fn is_mandatory(&self) -> bool {
        self.break_type == BreakType::Hard
    }

    /// Check if breaking is allowed (penalty < MAX)
    #[inline]
    pub fn is_allowed(&self) -> bool {
        self.penalty < i32::MAX
    }
}

impl Default for BreakOpportunity {
    fn default() -> Self {
        Self::normal(0)
    }
}

/// A segment of glyphs within a line
///
/// Represents a contiguous run of glyphs from the input that
/// belongs to a single line.
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

    /// Whether this segment ends with a hyphen
    pub has_hyphen: bool,
}

impl LineSegment {
    /// Create a new line segment
    pub fn new(glyph_start: usize, glyph_count: usize, width: f32) -> Self {
        Self {
            glyph_start,
            glyph_count,
            width,
            x_offset: 0.0,
            has_hyphen: false,
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
///
/// # Example
///
/// ```rust,ignore
/// use fastrender::text::line_breaker::{Line, LineSegment};
///
/// let line = Line {
///     segments: vec![LineSegment::new(0, 5, 100.0)],
///     width: 100.0,
///     max_width: 200.0,
///     is_last: false,
/// };
///
/// assert_eq!(line.segments.len(), 1);
/// assert_eq!(line.remaining_space(), 100.0);
/// ```
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

    /// Check if any segment has a hyphen
    pub fn has_hyphen(&self) -> bool {
        self.segments.iter().any(|s| s.has_hyphen)
    }
}

impl Default for Line {
    fn default() -> Self {
        Self::new(f32::INFINITY)
    }
}

// ============================================================================
// Line Breaking Algorithm
// ============================================================================

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
/// * `break_opportunities` - Valid break positions (sorted by position)
///
/// # Returns
///
/// Vector of lines containing the broken text.
///
/// # Example
///
/// ```rust,ignore
/// use fastrender::text::line_breaker::{break_lines, GlyphPosition, BreakOpportunity};
///
/// let glyphs = vec![
///     GlyphPosition::new(1, 0, 10.0),
///     GlyphPosition::new(2, 1, 10.0),
///     // ... more glyphs
/// ];
///
/// let opportunities = vec![
///     BreakOpportunity::normal(5),  // Break after word
///     BreakOpportunity::normal(10), // Break after another word
/// ];
///
/// let lines = break_lines(&glyphs, 100.0, &opportunities);
/// ```
pub fn break_lines(glyphs: &[GlyphPosition], max_width: f32, break_opportunities: &[BreakOpportunity]) -> Vec<Line> {
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
        while opp_index < break_opportunities.len() && break_opportunities[opp_index].position <= glyph.cluster {
            let opp = &break_opportunities[opp_index];

            // Handle hard (mandatory) breaks
            if opp.is_mandatory() {
                // Finalize current line
                let line = line_builder.finish(false);
                if !line.is_empty() || lines.is_empty() {
                    lines.push(line);
                }
                line_builder = LineBuilder::new(max_width);
                last_break = None;
            } else if opp.is_allowed() {
                // Record as potential break point
                last_break = Some(BreakState {
                    opportunity: *opp,
                    glyph_index: glyph_idx,
                    line_width: line_builder.current_width,
                    glyph_count: line_builder.glyph_count,
                });
            }

            opp_index += 1;
        }

        // Try to add glyph to current line
        let glyph_width = glyph.x_advance;

        if line_builder.can_fit(glyph_width) {
            line_builder.add_glyph(glyph_idx, glyph_width);
        } else {
            // Glyph doesn't fit - need to break

            if let Some(brk) = last_break.take() {
                // Break at last opportunity
                line_builder.truncate_to(brk.glyph_count, brk.line_width);

                // Check if break was hyphenation
                let has_hyphen = brk.opportunity.break_type == BreakType::Hyphen;
                let line = line_builder.finish(has_hyphen);
                lines.push(line);

                // Start new line, add glyphs from break point
                line_builder = LineBuilder::new(max_width);

                // Re-add glyphs from break point to current glyph (inclusive)
                for (i, g) in glyphs
                    .iter()
                    .enumerate()
                    .take(glyph_idx + 1)
                    .skip(brk.glyph_index)
                {
                    line_builder.add_glyph(i, g.x_advance);
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
    let mut final_line = line_builder.finish(false);
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

/// Break glyphs into lines with Result return type
///
/// Same as `break_lines` but returns a Result for consistency with
/// other FastRender APIs.
///
/// # Errors
///
/// Returns an error if:
/// - max_width is negative or NaN
pub fn break_lines_checked(
    glyphs: &[GlyphPosition],
    max_width: f32,
    break_opportunities: &[BreakOpportunity],
) -> Result<Vec<Line>> {
    if max_width.is_nan() || max_width < 0.0 {
        return Err(TextError::LineBreakingFailed {
            reason: format!("Invalid max_width: {}", max_width),
        }
        .into());
    }

    Ok(break_lines(glyphs, max_width, break_opportunities))
}

// ============================================================================
// Helper Types
// ============================================================================

/// State saved at a break opportunity
#[derive(Debug, Clone, Copy)]
struct BreakState {
    opportunity: BreakOpportunity,
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

    fn finish(self, has_hyphen: bool) -> Line {
        let mut segment = LineSegment::new(self.glyph_start, self.glyph_count, self.current_width);
        segment.has_hyphen = has_hyphen;

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
    let total_width: f32 = glyphs.iter().map(|g| g.x_advance).sum();

    Line {
        segments: vec![LineSegment::new(0, glyphs.len(), total_width)],
        width: total_width,
        max_width,
        is_last: true,
    }
}

// ============================================================================
// Greedy Line Breaker Struct (Alternative API)
// ============================================================================

/// Greedy line breaker
///
/// Provides an object-oriented interface to the greedy line breaking
/// algorithm with configurable options.
///
/// # Example
///
/// ```rust,ignore
/// use fastrender::text::line_breaker::{GreedyLineBreaker, GlyphPosition, BreakOpportunity};
///
/// let breaker = GreedyLineBreaker::new();
///
/// let glyphs = vec![/* ... */];
/// let opportunities = vec![/* ... */];
///
/// let lines = breaker.break_lines(&glyphs, 200.0, &opportunities);
/// ```
#[derive(Debug, Clone)]
pub struct GreedyLineBreaker {
    /// Allow overflow when no break opportunity exists
    pub allow_overflow: bool,

    /// Minimum line fill ratio before allowing break (0.0 - 1.0)
    ///
    /// Lines shorter than this ratio will not break unless forced.
    pub min_fill_ratio: f32,
}

impl GreedyLineBreaker {
    /// Create a new greedy line breaker with default settings
    pub fn new() -> Self {
        Self {
            allow_overflow: true,
            min_fill_ratio: 0.0,
        }
    }

    /// Set whether to allow overflow
    pub fn with_overflow(mut self, allow: bool) -> Self {
        self.allow_overflow = allow;
        self
    }

    /// Set minimum fill ratio
    pub fn with_min_fill(mut self, ratio: f32) -> Self {
        self.min_fill_ratio = ratio.clamp(0.0, 1.0);
        self
    }

    /// Break lines using the greedy algorithm
    pub fn break_lines(
        &self,
        glyphs: &[GlyphPosition],
        max_width: f32,
        break_opportunities: &[BreakOpportunity],
    ) -> Vec<Line> {
        break_lines(glyphs, max_width, break_opportunities)
    }

    /// Break lines with Result return type
    pub fn break_lines_checked(
        &self,
        glyphs: &[GlyphPosition],
        max_width: f32,
        break_opportunities: &[BreakOpportunity],
    ) -> Result<Vec<Line>> {
        break_lines_checked(glyphs, max_width, break_opportunities)
    }
}

impl Default for GreedyLineBreaker {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// Unit Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    // Helper to create glyphs for testing
    fn make_glyphs(widths: &[f32]) -> Vec<GlyphPosition> {
        widths
            .iter()
            .enumerate()
            .map(|(i, &w)| GlyphPosition::new(i as u32, i, w))
            .collect()
    }

    // Helper to create break opportunities
    fn make_breaks(positions: &[usize]) -> Vec<BreakOpportunity> {
        positions.iter().map(|&p| BreakOpportunity::normal(p)).collect()
    }

    #[test]
    fn test_empty_input() {
        let lines = break_lines(&[], 100.0, &[]);
        assert_eq!(lines.len(), 1);
        assert!(lines[0].is_empty());
        assert!(lines[0].is_last);
    }

    #[test]
    fn test_single_glyph() {
        let glyphs = make_glyphs(&[10.0]);
        let lines = break_lines(&glyphs, 100.0, &[]);

        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].segments.len(), 1);
        assert_eq!(lines[0].width, 10.0);
        assert!(lines[0].is_last);
    }

    #[test]
    fn test_all_fit_one_line() {
        let glyphs = make_glyphs(&[10.0, 10.0, 10.0, 10.0, 10.0]);
        let lines = break_lines(&glyphs, 100.0, &[]);

        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].width, 50.0);
        assert_eq!(lines[0].glyph_count(), 5);
        assert!(lines[0].is_last);
    }

    #[test]
    fn test_break_at_opportunity() {
        // 5 glyphs, each 20px wide = 100px total
        // Max width 60px, break after 3rd glyph (position 3)
        let glyphs = make_glyphs(&[20.0, 20.0, 20.0, 20.0, 20.0]);
        let breaks = make_breaks(&[3]);

        let lines = break_lines(&glyphs, 60.0, &breaks);

        assert_eq!(lines.len(), 2);
        assert_eq!(lines[0].glyph_count(), 3);
        assert_eq!(lines[0].width, 60.0);
        assert!(!lines[0].is_last);

        assert_eq!(lines[1].glyph_count(), 2);
        assert_eq!(lines[1].width, 40.0);
        assert!(lines[1].is_last);
    }

    #[test]
    fn test_multiple_breaks() {
        // 10 glyphs, each 15px wide
        // Max width 50px = ~3 glyphs per line
        // Breaks at positions 3, 6, 9
        let glyphs = make_glyphs(&[15.0; 10]);
        let breaks = make_breaks(&[3, 6, 9]);

        let lines = break_lines(&glyphs, 50.0, &breaks);

        assert_eq!(lines.len(), 4);
        assert_eq!(lines[0].glyph_count(), 3);
        assert_eq!(lines[1].glyph_count(), 3);
        assert_eq!(lines[2].glyph_count(), 3);
        assert_eq!(lines[3].glyph_count(), 1);
        assert!(lines[3].is_last);
    }

    #[test]
    fn test_hard_break() {
        let glyphs = make_glyphs(&[10.0, 10.0, 10.0, 10.0]);
        let breaks = vec![BreakOpportunity::hard(2)];

        let lines = break_lines(&glyphs, 100.0, &breaks);

        // Should break at position 2 regardless of width
        assert_eq!(lines.len(), 2);
        assert_eq!(lines[0].glyph_count(), 2);
        assert_eq!(lines[1].glyph_count(), 2);
    }

    #[test]
    fn test_no_break_overflow() {
        // No break opportunities - should overflow
        let glyphs = make_glyphs(&[30.0, 30.0, 30.0]);
        let lines = break_lines(&glyphs, 50.0, &[]);

        // All on one line (overflow)
        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].width, 90.0);
        assert!(lines[0].width > lines[0].max_width);
    }

    #[test]
    fn test_infinite_width() {
        let glyphs = make_glyphs(&[100.0; 100]);
        let breaks = make_breaks(&(0..100).collect::<Vec<_>>());

        let lines = break_lines(&glyphs, f32::INFINITY, &breaks);

        // Should be single line
        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].glyph_count(), 100);
    }

    #[test]
    fn test_hyphen_break() {
        let glyphs = make_glyphs(&[20.0, 20.0, 20.0, 20.0]);
        let breaks = vec![BreakOpportunity::hyphen(2)];

        let lines = break_lines(&glyphs, 50.0, &breaks);

        assert_eq!(lines.len(), 2);
        assert!(lines[0].has_hyphen());
        assert!(!lines[1].has_hyphen());
    }

    #[test]
    fn test_glyph_position_creation() {
        let glyph = GlyphPosition::new(42, 5, 12.5);
        assert_eq!(glyph.glyph_id, 42);
        assert_eq!(glyph.cluster, 5);
        assert_eq!(glyph.x_advance, 12.5);
        assert_eq!(glyph.x_offset, 0.0);
        assert_eq!(glyph.y_offset, 0.0);
    }

    #[test]
    fn test_glyph_position_with_offsets() {
        let glyph = GlyphPosition::with_offsets(42, 5, 12.5, 1.0, -2.0);
        assert_eq!(glyph.x_offset, 1.0);
        assert_eq!(glyph.y_offset, -2.0);
    }

    #[test]
    fn test_break_opportunity_constructors() {
        let normal = BreakOpportunity::normal(5);
        assert_eq!(normal.break_type, BreakType::Normal);
        assert!(!normal.is_mandatory());

        let hard = BreakOpportunity::hard(10);
        assert_eq!(hard.break_type, BreakType::Hard);
        assert!(hard.is_mandatory());

        let hyphen = BreakOpportunity::hyphen(15);
        assert_eq!(hyphen.break_type, BreakType::Hyphen);
        assert_eq!(hyphen.penalty, 100);

        let emergency = BreakOpportunity::emergency(20);
        assert_eq!(emergency.break_type, BreakType::Emergency);
        assert_eq!(emergency.penalty, 1000);
    }

    #[test]
    fn test_line_segment() {
        let segment = LineSegment::new(5, 10, 150.0);
        assert_eq!(segment.glyph_start, 5);
        assert_eq!(segment.glyph_count, 10);
        assert_eq!(segment.glyph_end(), 15);
        assert_eq!(segment.width, 150.0);
        assert!(!segment.is_empty());
    }

    #[test]
    fn test_line_methods() {
        let mut line = Line::new(200.0);
        assert!(line.is_empty());
        assert_eq!(line.glyph_count(), 0);

        line.segments.push(LineSegment::new(0, 5, 80.0));
        line.segments.push(LineSegment::new(5, 3, 40.0));
        line.width = 120.0;

        assert!(!line.is_empty());
        assert_eq!(line.glyph_count(), 8);
        assert_eq!(line.remaining_space(), 80.0);
        assert_eq!(line.fill_ratio(), 0.6);
    }

    #[test]
    fn test_greedy_line_breaker_struct() {
        let breaker = GreedyLineBreaker::new();
        assert!(breaker.allow_overflow);
        assert_eq!(breaker.min_fill_ratio, 0.0);

        let breaker = breaker.with_overflow(false).with_min_fill(0.5);
        assert!(!breaker.allow_overflow);
        assert_eq!(breaker.min_fill_ratio, 0.5);
    }

    #[test]
    fn test_break_lines_checked_valid() {
        let glyphs = make_glyphs(&[10.0, 10.0]);
        let result = break_lines_checked(&glyphs, 100.0, &[]);
        assert!(result.is_ok());
    }

    #[test]
    fn test_break_lines_checked_negative_width() {
        let glyphs = make_glyphs(&[10.0]);
        let result = break_lines_checked(&glyphs, -10.0, &[]);
        assert!(result.is_err());
    }

    #[test]
    fn test_break_lines_checked_nan_width() {
        let glyphs = make_glyphs(&[10.0]);
        let result = break_lines_checked(&glyphs, f32::NAN, &[]);
        assert!(result.is_err());
    }

    #[test]
    fn test_varying_glyph_widths() {
        // Simulate a realistic text with varying glyph widths
        let glyphs = make_glyphs(&[8.0, 10.0, 5.0, 12.0, 6.0, 9.0, 11.0, 7.0, 10.0, 8.0]);
        let breaks = make_breaks(&[3, 6, 9]); // Word boundaries

        let lines = break_lines(&glyphs, 30.0, &breaks);

        // Verify all glyphs are accounted for
        let total_glyphs: usize = lines.iter().map(|l| l.glyph_count()).sum();
        assert_eq!(total_glyphs, 10);

        // Last line should be marked
        assert!(lines.last().unwrap().is_last);
    }

    #[test]
    fn test_first_glyph_wider_than_max() {
        // First glyph is wider than max_width - should still be included
        let glyphs = make_glyphs(&[100.0, 10.0, 10.0]);
        let breaks = make_breaks(&[1, 2]);

        let lines = break_lines(&glyphs, 50.0, &breaks);

        // First glyph on its own line (overflow)
        assert!(lines.len() >= 1);
        assert!(lines[0].width >= 100.0);
    }

    #[test]
    fn test_consecutive_hard_breaks() {
        let glyphs = make_glyphs(&[10.0, 10.0, 10.0, 10.0]);
        let breaks = vec![
            BreakOpportunity::hard(1),
            BreakOpportunity::hard(2),
            BreakOpportunity::hard(3),
        ];

        let lines = break_lines(&glyphs, 100.0, &breaks);

        // Should create multiple lines even though they fit
        assert!(lines.len() > 1);
    }

    #[test]
    fn test_line_is_last_flag() {
        let glyphs = make_glyphs(&[10.0, 10.0, 10.0, 10.0]);
        let breaks = make_breaks(&[2]);

        let lines = break_lines(&glyphs, 25.0, &breaks);

        // Only the last line should have is_last = true
        for (i, line) in lines.iter().enumerate() {
            if i == lines.len() - 1 {
                assert!(line.is_last, "Last line should have is_last = true");
            } else {
                assert!(!line.is_last, "Non-last line should have is_last = false");
            }
        }
    }
}
