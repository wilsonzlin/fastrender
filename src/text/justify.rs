//! Text justification
//!
//! This module implements text justification according to CSS Text Module Level 3.
//! It distributes extra space between words and/or characters to make lines
//! flush with both margins.
//!
//! # CSS Reference
//!
//! - [CSS Text Module Level 3 - Alignment and Justification](https://www.w3.org/TR/css-text-3/#justification)
//!
//! # Overview
//!
//! Text justification is the process of distributing extra space in a line
//! to make it exactly fill the available width. This is typically done by:
//!
//! 1. **Word spacing** - Adding extra space between words (most common)
//! 2. **Letter spacing** - Adding extra space between characters (fallback for CJK or single-word lines)
//! 3. **Combined** - Using both word and letter spacing
//!
//! # Algorithm
//!
//! The justification algorithm:
//!
//! 1. Calculate the extra space needed: `target_width - current_width`
//! 2. Count expansion opportunities (word boundaries and/or characters)
//! 3. Distribute space evenly across opportunities
//! 4. Adjust glyph positions accordingly
//!
//! # Example
//!
//! ```rust,ignore
//! use fastrender::text::justify::{justify_line, GlyphPosition, JustificationOptions};
//!
//! let mut glyphs = vec![
//!     GlyphPosition::new(0, 0.0, 0.0, 50.0, true),   // "Hello" with space after
//!     GlyphPosition::new(1, 50.0, 0.0, 50.0, false), // "World" no space after
//! ];
//!
//! justify_line(&mut glyphs, 150.0, 100.0);
//! // Now glyphs are repositioned with 50px distributed between them
//! ```

use crate::error::{Result, TextError};

/// Position and metrics for a single glyph in a line
///
/// This type represents a glyph with its position and advance width,
/// used for text layout and justification calculations.
///
/// # Fields
///
/// - `glyph_id` - Unique identifier for the glyph in its font
/// - `x` - Horizontal position from the start of the line
/// - `y` - Vertical offset from the baseline
/// - `x_advance` - Horizontal advance to the next glyph
/// - `y_advance` - Vertical advance (typically 0 for horizontal text)
/// - `is_word_boundary` - True if this glyph ends a word (followed by whitespace)
///
/// # Example
///
/// ```rust,ignore
/// use fastrender::text::justify::GlyphPosition;
///
/// let glyph = GlyphPosition::new(42, 10.0, 0.0, 8.5, true);
/// assert_eq!(glyph.glyph_id, 42);
/// assert!(glyph.is_word_boundary);
/// ```
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct GlyphPosition {
    /// Glyph ID in the font
    pub glyph_id: u16,

    /// X position from start of line
    pub x: f32,

    /// Y offset from baseline
    pub y: f32,

    /// Horizontal advance width
    pub x_advance: f32,

    /// Vertical advance (typically 0 for horizontal text)
    pub y_advance: f32,

    /// Whether this glyph ends a word (followed by space or end of text)
    pub is_word_boundary: bool,

    /// Cluster index in original text (for mapping back to characters)
    pub cluster: usize,
}

impl GlyphPosition {
    /// Create a new glyph position
    ///
    /// # Arguments
    ///
    /// * `glyph_id` - Glyph ID in the font
    /// * `x` - X position from start of line
    /// * `y` - Y offset from baseline
    /// * `x_advance` - Horizontal advance width
    /// * `is_word_boundary` - Whether this glyph ends a word
    ///
    /// # Example
    ///
    /// ```rust,ignore
    /// use fastrender::text::justify::GlyphPosition;
    ///
    /// let glyph = GlyphPosition::new(42, 0.0, 0.0, 10.0, false);
    /// ```
    #[must_use]
    pub fn new(glyph_id: u16, x: f32, y: f32, x_advance: f32, is_word_boundary: bool) -> Self {
        Self {
            glyph_id,
            x,
            y,
            x_advance,
            y_advance: 0.0,
            is_word_boundary,
            cluster: 0,
        }
    }

    /// Create a glyph position with all fields specified
    ///
    /// # Arguments
    ///
    /// * `glyph_id` - Glyph ID in the font
    /// * `x` - X position from start of line
    /// * `y` - Y offset from baseline
    /// * `x_advance` - Horizontal advance width
    /// * `y_advance` - Vertical advance (typically 0)
    /// * `is_word_boundary` - Whether this glyph ends a word
    /// * `cluster` - Index in original text
    #[must_use]
    pub fn with_cluster(
        glyph_id: u16,
        x: f32,
        y: f32,
        x_advance: f32,
        y_advance: f32,
        is_word_boundary: bool,
        cluster: usize,
    ) -> Self {
        Self {
            glyph_id,
            x,
            y,
            x_advance,
            y_advance,
            is_word_boundary,
            cluster,
        }
    }

    /// Get the end X position of this glyph (x + x_advance)
    #[must_use]
    pub fn end_x(&self) -> f32 {
        self.x + self.x_advance
    }
}

impl Default for GlyphPosition {
    fn default() -> Self {
        Self {
            glyph_id: 0,
            x: 0.0,
            y: 0.0,
            x_advance: 0.0,
            y_advance: 0.0,
            is_word_boundary: false,
            cluster: 0,
        }
    }
}

/// Options for controlling text justification behavior
///
/// These options allow fine-tuning of how extra space is distributed
/// when justifying text.
///
/// # Example
///
/// ```rust,ignore
/// use fastrender::text::justify::JustificationOptions;
///
/// let options = JustificationOptions::default()
///     .with_letter_spacing_fallback(true)
///     .with_max_word_spacing_ratio(2.0);
/// ```
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct JustificationOptions {
    /// Minimum ratio of line fill before justifying (0.0 to 1.0)
    /// Lines shorter than this ratio of target width won't be justified
    pub min_fill_ratio: f32,

    /// Maximum ratio of extra space to add per word boundary
    /// Prevents excessive word spacing
    pub max_word_spacing_ratio: f32,

    /// Whether to use letter spacing as fallback when word spacing alone
    /// would be too extreme or when there are no word boundaries
    pub use_letter_spacing_fallback: bool,

    /// Maximum extra space to add per character when using letter spacing
    pub max_letter_spacing: f32,

    /// Whether to justify the last line of a paragraph
    /// (typically false per CSS text-align-last default)
    pub justify_last_line: bool,
}

impl Default for JustificationOptions {
    fn default() -> Self {
        Self {
            min_fill_ratio: 0.75,
            max_word_spacing_ratio: 3.0,
            use_letter_spacing_fallback: true,
            max_letter_spacing: 2.0,
            justify_last_line: false,
        }
    }
}

impl JustificationOptions {
    /// Create new justification options with default values
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Set minimum fill ratio for justification
    ///
    /// Lines shorter than `ratio * target_width` won't be justified.
    #[must_use]
    pub fn with_min_fill_ratio(mut self, ratio: f32) -> Self {
        self.min_fill_ratio = ratio.clamp(0.0, 1.0);
        self
    }

    /// Set maximum word spacing ratio
    ///
    /// Prevents word spacing from exceeding `ratio * normal_space_width`.
    #[must_use]
    pub fn with_max_word_spacing_ratio(mut self, ratio: f32) -> Self {
        self.max_word_spacing_ratio = ratio.max(1.0);
        self
    }

    /// Set whether to use letter spacing as fallback
    #[must_use]
    pub fn with_letter_spacing_fallback(mut self, enabled: bool) -> Self {
        self.use_letter_spacing_fallback = enabled;
        self
    }

    /// Set maximum letter spacing
    #[must_use]
    pub fn with_max_letter_spacing(mut self, max: f32) -> Self {
        self.max_letter_spacing = max.max(0.0);
        self
    }

    /// Set whether to justify the last line
    #[must_use]
    pub fn with_justify_last_line(mut self, justify: bool) -> Self {
        self.justify_last_line = justify;
        self
    }
}

/// Result of a justification operation
///
/// Contains information about how space was distributed.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct JustificationResult {
    /// Total extra space added
    pub extra_space_added: f32,

    /// Extra space added per word boundary
    pub space_per_word: f32,

    /// Extra space added per character (letter spacing)
    pub space_per_letter: f32,

    /// Number of word boundaries used for distribution
    pub word_boundaries_used: usize,

    /// Number of letter spaces used for distribution
    pub letter_spaces_used: usize,

    /// Whether the line was successfully justified
    pub is_justified: bool,
}

impl Default for JustificationResult {
    fn default() -> Self {
        Self {
            extra_space_added: 0.0,
            space_per_word: 0.0,
            space_per_letter: 0.0,
            word_boundaries_used: 0,
            letter_spaces_used: 0,
            is_justified: false,
        }
    }
}

/// Justify a line of text by distributing extra space among words
///
/// This is the main entry point for text justification. It modifies the
/// glyph positions in-place to fill the target width.
///
/// # Arguments
///
/// * `glyphs` - Mutable slice of glyph positions to modify
/// * `target_width` - The width the line should fill after justification
/// * `current_width` - The current width of the line before justification
///
/// # Algorithm
///
/// 1. Calculate extra space needed: `target_width - current_width`
/// 2. Count word boundaries (glyphs with `is_word_boundary == true`)
/// 3. Distribute extra space evenly across word boundaries
/// 4. If no word boundaries exist, use letter spacing as fallback
///
/// # Example
///
/// ```rust,ignore
/// use fastrender::text::justify::{justify_line, GlyphPosition};
///
/// let mut glyphs = vec![
///     GlyphPosition::new(0, 0.0, 0.0, 50.0, true),
///     GlyphPosition::new(1, 50.0, 0.0, 50.0, false),
/// ];
///
/// justify_line(&mut glyphs, 150.0, 100.0);
/// // First glyph stays at x=0
/// // Second glyph moves to x=100 (50 + 50 extra space)
/// ```
///
/// # Notes
///
/// - If `target_width <= current_width`, no changes are made
/// - If there are no word boundaries and only one glyph, no changes are made
/// - The function uses letter spacing as a fallback for lines without word boundaries
pub fn justify_line(glyphs: &mut [GlyphPosition], target_width: f32, current_width: f32) {
    let _ = justify_line_with_options(glyphs, target_width, current_width, &JustificationOptions::default());
}

/// Justify a line of text with custom options
///
/// This is the advanced version of `justify_line` that allows customization
/// of justification behavior through `JustificationOptions`.
///
/// # Arguments
///
/// * `glyphs` - Mutable slice of glyph positions to modify
/// * `target_width` - The width the line should fill after justification
/// * `current_width` - The current width of the line before justification
/// * `options` - Justification options controlling the behavior
///
/// # Returns
///
/// A `JustificationResult` containing information about how space was distributed.
///
/// # Example
///
/// ```rust,ignore
/// use fastrender::text::justify::{justify_line_with_options, GlyphPosition, JustificationOptions};
///
/// let mut glyphs = vec![
///     GlyphPosition::new(0, 0.0, 0.0, 50.0, true),
///     GlyphPosition::new(1, 50.0, 0.0, 50.0, false),
/// ];
///
/// let options = JustificationOptions::default()
///     .with_min_fill_ratio(0.5);
///
/// let result = justify_line_with_options(&mut glyphs, 150.0, 100.0, &options);
/// assert!(result.is_justified);
/// ```
#[must_use]
pub fn justify_line_with_options(
    glyphs: &mut [GlyphPosition],
    target_width: f32,
    current_width: f32,
    options: &JustificationOptions,
) -> JustificationResult {
    let mut result = JustificationResult::default();

    // Early return if nothing to do
    if glyphs.is_empty() || target_width <= current_width {
        return result;
    }

    let extra_space = target_width - current_width;

    // Check if line is too short to justify
    if current_width < target_width * options.min_fill_ratio {
        return result;
    }

    // Count word boundaries (glyphs that end words)
    let word_boundaries: usize = glyphs.iter().filter(|g| g.is_word_boundary).count();

    // If we have word boundaries, distribute space among them
    if word_boundaries > 0 {
        let space_per_boundary = extra_space / (word_boundaries as f32);

        // Check if word spacing would be too extreme
        // We consider it extreme if each boundary would get more than max_word_spacing_ratio
        // times the average glyph advance
        let avg_advance: f32 = if glyphs.is_empty() {
            0.0
        } else {
            glyphs.iter().map(|g| g.x_advance).sum::<f32>() / glyphs.len() as f32
        };

        let max_space = avg_advance * options.max_word_spacing_ratio;

        if space_per_boundary <= max_space || !options.use_letter_spacing_fallback {
            // Use word spacing distribution
            result = distribute_space_at_word_boundaries(glyphs, extra_space, word_boundaries);
        } else {
            // Combine word and letter spacing
            result = distribute_space_combined(glyphs, extra_space, word_boundaries, options);
        }
    } else if options.use_letter_spacing_fallback && glyphs.len() > 1 {
        // No word boundaries - use letter spacing as fallback
        result = distribute_space_between_letters(glyphs, extra_space, options);
    }

    result
}

/// Distribute extra space at word boundaries
///
/// This function adds extra space after each glyph that marks a word boundary.
/// It adjusts positions of all subsequent glyphs accordingly.
fn distribute_space_at_word_boundaries(
    glyphs: &mut [GlyphPosition],
    extra_space: f32,
    word_boundaries: usize,
) -> JustificationResult {
    if word_boundaries == 0 || glyphs.is_empty() {
        return JustificationResult::default();
    }

    let space_per_boundary = extra_space / (word_boundaries as f32);
    let mut cumulative_offset = 0.0;

    for glyph in glyphs.iter_mut() {
        // Apply cumulative offset to this glyph's position
        glyph.x += cumulative_offset;

        // If this glyph ends a word, add extra space after it
        if glyph.is_word_boundary {
            cumulative_offset += space_per_boundary;
        }
    }

    JustificationResult {
        extra_space_added: extra_space,
        space_per_word: space_per_boundary,
        space_per_letter: 0.0,
        word_boundaries_used: word_boundaries,
        letter_spaces_used: 0,
        is_justified: true,
    }
}

/// Distribute extra space between all letters
///
/// This function adds extra space between every glyph (letter spacing).
/// Used when there are no word boundaries or as a supplement.
fn distribute_space_between_letters(
    glyphs: &mut [GlyphPosition],
    extra_space: f32,
    options: &JustificationOptions,
) -> JustificationResult {
    if glyphs.len() <= 1 {
        return JustificationResult::default();
    }

    // Number of gaps between glyphs
    let gaps = glyphs.len() - 1;
    let mut space_per_gap = extra_space / (gaps as f32);

    // Cap letter spacing if it would be too extreme
    if space_per_gap > options.max_letter_spacing {
        space_per_gap = options.max_letter_spacing;
    }

    let actual_extra_space = space_per_gap * (gaps as f32);

    let mut cumulative_offset = 0.0;

    for (i, glyph) in glyphs.iter_mut().enumerate() {
        // Apply cumulative offset to this glyph's position
        glyph.x += cumulative_offset;

        // Add space after each glyph except the last
        if i < gaps {
            cumulative_offset += space_per_gap;
        }
    }

    JustificationResult {
        extra_space_added: actual_extra_space,
        space_per_word: 0.0,
        space_per_letter: space_per_gap,
        word_boundaries_used: 0,
        letter_spaces_used: gaps,
        is_justified: actual_extra_space > 0.0,
    }
}

/// Distribute extra space using both word spacing and letter spacing
///
/// This function combines word spacing and letter spacing to achieve
/// better distribution when word spacing alone would be excessive.
fn distribute_space_combined(
    glyphs: &mut [GlyphPosition],
    extra_space: f32,
    word_boundaries: usize,
    options: &JustificationOptions,
) -> JustificationResult {
    if glyphs.is_empty() {
        return JustificationResult::default();
    }

    // Calculate average advance to determine reasonable word spacing
    let avg_advance: f32 = glyphs.iter().map(|g| g.x_advance).sum::<f32>() / glyphs.len() as f32;
    let max_word_space = avg_advance * options.max_word_spacing_ratio;

    // Calculate how much we can distribute via word spacing
    let max_word_space_total = max_word_space * (word_boundaries as f32);
    let word_space_portion = extra_space.min(max_word_space_total);
    let remaining_space = extra_space - word_space_portion;

    let space_per_word = if word_boundaries > 0 {
        word_space_portion / (word_boundaries as f32)
    } else {
        0.0
    };

    // Distribute remaining space via letter spacing
    let letter_gaps = if glyphs.len() > 1 { glyphs.len() - 1 } else { 0 };
    let mut space_per_letter = if letter_gaps > 0 {
        remaining_space / (letter_gaps as f32)
    } else {
        0.0
    };

    // Cap letter spacing
    if space_per_letter > options.max_letter_spacing {
        space_per_letter = options.max_letter_spacing;
    }

    let actual_letter_space = space_per_letter * (letter_gaps as f32);
    let total_extra_space = word_space_portion + actual_letter_space;

    // Apply both types of spacing
    let mut cumulative_offset = 0.0;

    for (i, glyph) in glyphs.iter_mut().enumerate() {
        glyph.x += cumulative_offset;

        // Add letter spacing after each glyph except the last
        if i < letter_gaps {
            cumulative_offset += space_per_letter;
        }

        // Add word spacing after word boundaries
        if glyph.is_word_boundary {
            cumulative_offset += space_per_word;
        }
    }

    JustificationResult {
        extra_space_added: total_extra_space,
        space_per_word,
        space_per_letter,
        word_boundaries_used: word_boundaries,
        letter_spaces_used: letter_gaps,
        is_justified: total_extra_space > 0.0,
    }
}

/// Justify multiple lines of text
///
/// This function justifies a collection of lines, respecting the `justify_last_line`
/// option for the final line.
///
/// # Arguments
///
/// * `lines` - Mutable slice of line glyphs (each line is a Vec of GlyphPosition)
/// * `target_width` - The width each line should fill
/// * `options` - Justification options
///
/// # Returns
///
/// A vector of `JustificationResult` for each line.
///
/// # Example
///
/// ```rust,ignore
/// use fastrender::text::justify::{justify_lines, GlyphPosition, JustificationOptions};
///
/// let mut lines = vec![
///     vec![GlyphPosition::new(0, 0.0, 0.0, 50.0, true)],
///     vec![GlyphPosition::new(1, 0.0, 0.0, 40.0, false)], // Last line
/// ];
///
/// let options = JustificationOptions::default();
/// let results = justify_lines(&mut lines, 100.0, &options);
///
/// // First line is justified, last line may not be (depending on options)
/// ```
pub fn justify_lines(
    lines: &mut [Vec<GlyphPosition>],
    target_width: f32,
    options: &JustificationOptions,
) -> Vec<JustificationResult> {
    let num_lines = lines.len();

    lines
        .iter_mut()
        .enumerate()
        .map(|(i, line)| {
            let is_last = i == num_lines.saturating_sub(1);

            // Skip last line if not configured to justify it
            if is_last && !options.justify_last_line {
                return JustificationResult::default();
            }

            let current_width = calculate_line_width(line);
            justify_line_with_options(line, target_width, current_width, options)
        })
        .collect()
}

/// Calculate the total width of a line of glyphs
///
/// # Arguments
///
/// * `glyphs` - Slice of glyph positions
///
/// # Returns
///
/// The total width of the line (sum of all x_advance values).
#[must_use]
pub fn calculate_line_width(glyphs: &[GlyphPosition]) -> f32 {
    glyphs.iter().map(|g| g.x_advance).sum()
}

/// Mark word boundaries in a sequence of glyphs based on whitespace
///
/// This helper function analyzes the cluster indices and marks glyphs
/// that precede whitespace characters as word boundaries.
///
/// # Arguments
///
/// * `glyphs` - Mutable slice of glyph positions to mark
/// * `text` - The original text that was shaped
///
/// # Example
///
/// ```rust,ignore
/// use fastrender::text::justify::{mark_word_boundaries, GlyphPosition};
///
/// let mut glyphs = vec![
///     GlyphPosition::with_cluster(0, 0.0, 0.0, 10.0, 0.0, false, 0), // 'H'
///     GlyphPosition::with_cluster(1, 10.0, 0.0, 10.0, 0.0, false, 1), // 'i'
///     GlyphPosition::with_cluster(2, 20.0, 0.0, 5.0, 0.0, false, 2), // ' '
///     GlyphPosition::with_cluster(3, 25.0, 0.0, 10.0, 0.0, false, 3), // 'T'
/// ];
///
/// mark_word_boundaries(&mut glyphs, "Hi There");
/// // glyphs[1] (or [2] depending on how space is handled) will have is_word_boundary = true
/// ```
pub fn mark_word_boundaries(glyphs: &mut [GlyphPosition], text: &str) {
    if glyphs.is_empty() || text.is_empty() {
        return;
    }

    let chars: Vec<char> = text.chars().collect();

    for glyph in glyphs.iter_mut() {
        let cluster = glyph.cluster;

        // Check if this glyph is a whitespace character
        let is_whitespace = chars.get(cluster).is_some_and(|c| c.is_whitespace());

        // Check if the next character is whitespace
        let next_is_whitespace = chars.get(cluster + 1).is_some_and(|c| c.is_whitespace());

        // A word boundary is when the current glyph is not whitespace but is followed by whitespace
        if !is_whitespace && next_is_whitespace {
            glyph.is_word_boundary = true;
        }
    }
}

/// Determine word boundaries from whitespace glyph positions
///
/// This function marks word boundaries by detecting space glyphs
/// in the glyph sequence.
///
/// # Arguments
///
/// * `glyphs` - Mutable slice of glyph positions
/// * `space_glyph_ids` - Set of glyph IDs that represent whitespace
pub fn mark_word_boundaries_by_glyph_id(glyphs: &mut [GlyphPosition], space_glyph_ids: &[u16]) {
    if glyphs.is_empty() {
        return;
    }

    // Mark glyph before each space as a word boundary
    for i in 0..glyphs.len() {
        let is_space = space_glyph_ids.contains(&glyphs[i].glyph_id);

        // If this is a space and there's a preceding non-space glyph
        if is_space && i > 0 && !space_glyph_ids.contains(&glyphs[i - 1].glyph_id) {
            glyphs[i - 1].is_word_boundary = true;
        }
    }
}

/// Text justification mode
///
/// Determines how justification is applied to text.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum JustificationMode {
    /// Justify by distributing space between words only
    #[default]
    WordSpacing,

    /// Justify by distributing space between all characters
    LetterSpacing,

    /// Justify using both word and letter spacing
    Combined,

    /// Automatic: use word spacing for Latin text, letter spacing for CJK
    Auto,
}

impl JustificationMode {
    /// Check if this mode uses word spacing
    #[must_use]
    pub fn uses_word_spacing(self) -> bool {
        matches!(self, Self::WordSpacing | Self::Combined | Self::Auto)
    }

    /// Check if this mode uses letter spacing
    #[must_use]
    pub fn uses_letter_spacing(self) -> bool {
        matches!(self, Self::LetterSpacing | Self::Combined | Self::Auto)
    }
}

/// Check if a character is CJK (Chinese, Japanese, Korean)
///
/// CJK text typically uses letter-spacing justification rather than
/// word-spacing, as these scripts don't use spaces between words.
#[must_use]
pub fn is_cjk_character(ch: char) -> bool {
    matches!(ch,
        '\u{4E00}'..='\u{9FFF}'     // CJK Unified Ideographs
        | '\u{3400}'..='\u{4DBF}'   // CJK Unified Ideographs Extension A
        | '\u{20000}'..='\u{2A6DF}' // CJK Unified Ideographs Extension B
        | '\u{F900}'..='\u{FAFF}'   // CJK Compatibility Ideographs
        | '\u{3040}'..='\u{309F}'   // Hiragana
        | '\u{30A0}'..='\u{30FF}'   // Katakana
        | '\u{AC00}'..='\u{D7AF}'   // Hangul Syllables
    )
}

/// Determine the appropriate justification mode for text
///
/// Analyzes the text content to choose between word spacing (for Latin/Western text)
/// and letter spacing (for CJK text).
///
/// # Arguments
///
/// * `text` - The text to analyze
///
/// # Returns
///
/// The recommended `JustificationMode` for the text.
#[must_use]
pub fn detect_justification_mode(text: &str) -> JustificationMode {
    let cjk_count = text.chars().filter(|&c| is_cjk_character(c)).count();
    let total_chars = text.chars().filter(|c| !c.is_whitespace()).count();

    if total_chars == 0 {
        return JustificationMode::WordSpacing;
    }

    let cjk_ratio = cjk_count as f32 / total_chars as f32;

    // If more than 50% CJK, use letter spacing
    if cjk_ratio > 0.5 {
        JustificationMode::LetterSpacing
    } else if cjk_ratio > 0.1 {
        // Mixed content - use combined
        JustificationMode::Combined
    } else {
        // Primarily Latin/Western text
        JustificationMode::WordSpacing
    }
}

/// Apply alignment to a line of glyphs
///
/// This function shifts all glyphs to achieve the specified alignment
/// within the available width.
///
/// # Arguments
///
/// * `glyphs` - Mutable slice of glyph positions
/// * `line_width` - Current width of the line
/// * `available_width` - Total available width for the line
/// * `alignment` - Text alignment to apply
///
/// # Returns
///
/// The x offset applied to all glyphs.
pub fn apply_alignment(
    glyphs: &mut [GlyphPosition],
    line_width: f32,
    available_width: f32,
    alignment: TextAlignment,
) -> f32 {
    let offset = match alignment {
        TextAlignment::Left | TextAlignment::Justify => 0.0,
        TextAlignment::Right => available_width - line_width,
        TextAlignment::Center => (available_width - line_width) / 2.0,
    };

    for glyph in glyphs.iter_mut() {
        glyph.x += offset;
    }

    offset
}

/// Text alignment mode
///
/// Corresponds to CSS `text-align` property values.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum TextAlignment {
    /// Align text to the left (default for LTR text)
    #[default]
    Left,

    /// Align text to the right
    Right,

    /// Center text horizontally
    Center,

    /// Justify text (distribute space to fill width)
    Justify,
}

impl TextAlignment {
    /// Create from a CSS text-align value string
    #[must_use]
    pub fn from_css(value: &str) -> Option<Self> {
        match value.trim().to_lowercase().as_str() {
            "left" | "start" => Some(Self::Left),
            "right" | "end" => Some(Self::Right),
            "center" => Some(Self::Center),
            "justify" => Some(Self::Justify),
            _ => None,
        }
    }

    /// Check if this alignment requires justification
    #[must_use]
    pub fn needs_justification(self) -> bool {
        matches!(self, Self::Justify)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_glyph(id: u16, x: f32, advance: f32, is_word_boundary: bool) -> GlyphPosition {
        GlyphPosition::new(id, x, 0.0, advance, is_word_boundary)
    }

    #[test]
    fn test_justify_line_basic() {
        // Two words, one word boundary between them
        // Line width 100px, target 120px (ratio 0.83 > 0.75)
        let mut glyphs = vec![
            make_glyph(0, 0.0, 50.0, true),   // "Hello" ends with space
            make_glyph(1, 50.0, 50.0, false), // "World"
        ];

        justify_line(&mut glyphs, 120.0, 100.0);

        // First glyph should stay at 0
        assert_eq!(glyphs[0].x, 0.0);
        // Second glyph should be pushed right by 20 (the extra space)
        assert!((glyphs[1].x - 70.0).abs() < 0.01);
    }

    #[test]
    fn test_justify_line_no_change_when_already_full() {
        let mut glyphs = vec![make_glyph(0, 0.0, 100.0, false)];

        justify_line(&mut glyphs, 100.0, 100.0);

        assert_eq!(glyphs[0].x, 0.0);
    }

    #[test]
    fn test_justify_line_no_change_when_empty() {
        let mut glyphs: Vec<GlyphPosition> = vec![];

        justify_line(&mut glyphs, 100.0, 0.0);

        assert!(glyphs.is_empty());
    }

    #[test]
    fn test_justify_line_multiple_word_boundaries() {
        // Three words: "Hello World Test"
        let mut glyphs = vec![
            make_glyph(0, 0.0, 30.0, true),   // "Hello" + space
            make_glyph(1, 30.0, 30.0, true),  // "World" + space
            make_glyph(2, 60.0, 30.0, false), // "Test"
        ];

        justify_line(&mut glyphs, 120.0, 90.0);

        // Extra space is 30, distributed among 2 word boundaries = 15 each
        assert_eq!(glyphs[0].x, 0.0);
        assert!((glyphs[1].x - 45.0).abs() < 0.01); // 30 + 15
        assert!((glyphs[2].x - 90.0).abs() < 0.01); // 60 + 30
    }

    #[test]
    fn test_justify_line_letter_spacing_fallback() {
        // No word boundaries - should use letter spacing
        // Line width 60px, target 75px (ratio 0.8 > 0.75)
        let mut glyphs = vec![
            make_glyph(0, 0.0, 20.0, false),
            make_glyph(1, 20.0, 20.0, false),
            make_glyph(2, 40.0, 20.0, false),
        ];

        let options = JustificationOptions::default()
            .with_letter_spacing_fallback(true)
            .with_max_letter_spacing(10.0); // Allow larger letter spacing

        let result = justify_line_with_options(&mut glyphs, 75.0, 60.0, &options);

        assert!(result.is_justified);
        assert!(result.space_per_letter > 0.0);
        // Extra space is 15, distributed among 2 gaps = 7.5 each
        assert_eq!(glyphs[0].x, 0.0);
        assert!((glyphs[1].x - 27.5).abs() < 0.01); // 20 + 7.5
        assert!((glyphs[2].x - 55.0).abs() < 0.01); // 40 + 15
    }

    #[test]
    fn test_justify_line_too_short() {
        // Line is too short (less than min_fill_ratio) - should not justify
        let mut glyphs = vec![make_glyph(0, 0.0, 30.0, true)];

        let options = JustificationOptions::default().with_min_fill_ratio(0.75);

        let result = justify_line_with_options(&mut glyphs, 100.0, 30.0, &options);

        assert!(!result.is_justified);
        assert_eq!(glyphs[0].x, 0.0);
    }

    #[test]
    fn test_calculate_line_width() {
        let glyphs = vec![
            make_glyph(0, 0.0, 30.0, false),
            make_glyph(1, 30.0, 40.0, false),
            make_glyph(2, 70.0, 30.0, false),
        ];

        assert_eq!(calculate_line_width(&glyphs), 100.0);
    }

    #[test]
    fn test_glyph_position_default() {
        let glyph = GlyphPosition::default();

        assert_eq!(glyph.glyph_id, 0);
        assert_eq!(glyph.x, 0.0);
        assert_eq!(glyph.y, 0.0);
        assert_eq!(glyph.x_advance, 0.0);
        assert!(!glyph.is_word_boundary);
    }

    #[test]
    fn test_glyph_position_end_x() {
        let glyph = GlyphPosition::new(0, 10.0, 0.0, 20.0, false);
        assert_eq!(glyph.end_x(), 30.0);
    }

    #[test]
    fn test_justification_options_builder() {
        let options = JustificationOptions::new()
            .with_min_fill_ratio(0.5)
            .with_max_word_spacing_ratio(2.5)
            .with_letter_spacing_fallback(false)
            .with_max_letter_spacing(3.0)
            .with_justify_last_line(true);

        assert_eq!(options.min_fill_ratio, 0.5);
        assert_eq!(options.max_word_spacing_ratio, 2.5);
        assert!(!options.use_letter_spacing_fallback);
        assert_eq!(options.max_letter_spacing, 3.0);
        assert!(options.justify_last_line);
    }

    #[test]
    fn test_justify_lines_last_line() {
        let mut lines = vec![
            vec![make_glyph(0, 0.0, 50.0, true), make_glyph(1, 50.0, 30.0, false)],
            vec![make_glyph(2, 0.0, 40.0, false)], // Last line
        ];

        let options = JustificationOptions::default().with_justify_last_line(false);

        let results = justify_lines(&mut lines, 100.0, &options);

        // First line should be justified
        assert!(results[0].is_justified);
        // Last line should NOT be justified
        assert!(!results[1].is_justified);
    }

    #[test]
    fn test_is_cjk_character() {
        // CJK characters
        assert!(is_cjk_character('中'));
        assert!(is_cjk_character('日'));
        assert!(is_cjk_character('한'));
        assert!(is_cjk_character('あ'));
        assert!(is_cjk_character('カ'));

        // Non-CJK characters
        assert!(!is_cjk_character('A'));
        assert!(!is_cjk_character('1'));
        assert!(!is_cjk_character(' '));
    }

    #[test]
    fn test_detect_justification_mode() {
        // Latin text
        assert_eq!(detect_justification_mode("Hello World"), JustificationMode::WordSpacing);

        // CJK text
        assert_eq!(detect_justification_mode("你好世界"), JustificationMode::LetterSpacing);

        // Mixed text
        assert_eq!(detect_justification_mode("Hello 你好"), JustificationMode::Combined);
    }

    #[test]
    fn test_text_alignment_from_css() {
        assert_eq!(TextAlignment::from_css("left"), Some(TextAlignment::Left));
        assert_eq!(TextAlignment::from_css("right"), Some(TextAlignment::Right));
        assert_eq!(TextAlignment::from_css("center"), Some(TextAlignment::Center));
        assert_eq!(TextAlignment::from_css("justify"), Some(TextAlignment::Justify));
        assert_eq!(TextAlignment::from_css("invalid"), None);
    }

    #[test]
    fn test_apply_alignment() {
        let mut glyphs = vec![make_glyph(0, 0.0, 30.0, false), make_glyph(1, 30.0, 20.0, false)];

        // Test center alignment
        let offset = apply_alignment(&mut glyphs, 50.0, 100.0, TextAlignment::Center);
        assert_eq!(offset, 25.0);
        assert_eq!(glyphs[0].x, 25.0);
        assert_eq!(glyphs[1].x, 55.0);
    }

    #[test]
    fn test_mark_word_boundaries() {
        let mut glyphs = vec![
            GlyphPosition::with_cluster(0, 0.0, 0.0, 10.0, 0.0, false, 0),
            GlyphPosition::with_cluster(1, 10.0, 0.0, 10.0, 0.0, false, 1),
            GlyphPosition::with_cluster(2, 20.0, 0.0, 5.0, 0.0, false, 2), // space
            GlyphPosition::with_cluster(3, 25.0, 0.0, 10.0, 0.0, false, 3),
        ];

        mark_word_boundaries(&mut glyphs, "Hi There");

        // Character at index 1 ('i') should be marked as word boundary
        // because it's followed by a space
        assert!(glyphs[1].is_word_boundary);
        assert!(!glyphs[0].is_word_boundary);
        assert!(!glyphs[2].is_word_boundary);
        assert!(!glyphs[3].is_word_boundary);
    }

    #[test]
    fn test_justification_result_default() {
        let result = JustificationResult::default();

        assert_eq!(result.extra_space_added, 0.0);
        assert_eq!(result.space_per_word, 0.0);
        assert_eq!(result.space_per_letter, 0.0);
        assert!(!result.is_justified);
    }
}
