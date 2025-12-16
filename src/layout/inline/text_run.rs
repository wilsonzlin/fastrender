//! Text Run Generation for Inline Layout
//!
//! This module generates text runs from shaped glyphs for use in inline layout.
//! Text runs are the fundamental unit of text in inline formatting contexts.
//!
//! # Overview
//!
//! A text run represents a sequence of text in the same font and style, with
//! positioned glyphs ready for rendering. Text runs are created by shaping text
//! using the font system and are consumed by the inline formatting context for
//! line breaking and baseline alignment.
//!
//! # CSS Specification
//!
//! - CSS Inline Layout Module Level 3: <https://www.w3.org/TR/css-inline-3/>
//! - CSS Text Module Level 3: <https://www.w3.org/TR/css-text-3/>
//!
//! # Example
//!
//! ```rust,ignore
//! use fastrender::layout::inline::text_run::{TextRunBuilder, TextRun};
//! use fastrender::text::font_loader::FontContext;
//!
//! let font_ctx = FontContext::new();
//! let builder = TextRunBuilder::new(&font_ctx);
//!
//! // Create a text run from shaped text
//! let run = builder.build_from_text(
//!     "Hello, world!",
//!     &["sans-serif".to_string()],
//!     400,    // font weight
//!     false,  // italic
//!     16.0,   // font size
//! );
//! ```

use crate::error::{Error, FontError, Result};
use crate::geometry::{Point, Rect};
use crate::text::font_db::{FontStyle, FontWeight, LoadedFont, ScaledMetrics};
use crate::text::font_loader::FontContext;
use rustybuzz::{Direction, Face, UnicodeBuffer};
use std::sync::Arc;

// ============================================================================
// GlyphInfo - Individual Glyph Information
// ============================================================================

/// Information about a single positioned glyph
///
/// Contains all the information needed to render a glyph at its correct
/// position within a text run.
///
/// # Fields
///
/// - `glyph_id`: The font-specific glyph identifier
/// - `cluster`: Maps back to character index in source text
/// - `x_advance`: Horizontal advance to next glyph
/// - `y_advance`: Vertical advance (usually 0 for horizontal text)
/// - `x_offset`: Horizontal positioning offset
/// - `y_offset`: Vertical positioning offset
///
/// # Example
///
/// ```rust,ignore
/// use fastrender::layout::inline::text_run::GlyphInfo;
///
/// let glyph = GlyphInfo {
///     glyph_id: 72,       // 'H' in font
///     cluster: 0,         // First character
///     x_advance: 10.5,    // Move 10.5 pixels right
///     y_advance: 0.0,     // No vertical movement
///     x_offset: 0.0,      // No horizontal offset
///     y_offset: 0.0,      // No vertical offset
/// };
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct GlyphInfo {
    /// Glyph ID in the font (font-specific identifier)
    pub glyph_id: u16,

    /// Cluster index mapping back to source character(s)
    ///
    /// Multiple glyphs can map to the same cluster (ligatures),
    /// or one cluster can map to multiple characters (complex scripts).
    pub cluster: u32,

    /// Horizontal advance width in pixels
    ///
    /// This is how far to move the pen horizontally after drawing this glyph.
    pub x_advance: f32,

    /// Vertical advance in pixels
    ///
    /// Usually 0 for horizontal text. Non-zero for vertical writing modes.
    pub y_advance: f32,

    /// Horizontal offset from the pen position
    ///
    /// Applied before drawing the glyph. Used for kerning adjustments
    /// and combining marks.
    pub x_offset: f32,

    /// Vertical offset from the pen position
    ///
    /// Applied before drawing the glyph. Used for superscript, subscript,
    /// and combining marks.
    pub y_offset: f32,
}

impl GlyphInfo {
    /// Creates a new glyph info with default offsets
    pub fn new(glyph_id: u16, cluster: u32, x_advance: f32) -> Self {
        Self {
            glyph_id,
            cluster,
            x_advance,
            y_advance: 0.0,
            x_offset: 0.0,
            y_offset: 0.0,
        }
    }

    /// Creates a glyph info with full positioning information
    pub fn with_offsets(
        glyph_id: u16,
        cluster: u32,
        x_advance: f32,
        y_advance: f32,
        x_offset: f32,
        y_offset: f32,
    ) -> Self {
        Self {
            glyph_id,
            cluster,
            x_advance,
            y_advance,
            x_offset,
            y_offset,
        }
    }

    /// Returns true if this glyph has zero advance (e.g., combining marks)
    pub fn is_zero_width(&self) -> bool {
        self.x_advance == 0.0 && self.y_advance == 0.0
    }
}

impl Default for GlyphInfo {
    fn default() -> Self {
        Self {
            glyph_id: 0,
            cluster: 0,
            x_advance: 0.0,
            y_advance: 0.0,
            x_offset: 0.0,
            y_offset: 0.0,
        }
    }
}

// ============================================================================
// TextRun - A Run of Shaped Text
// ============================================================================

/// A run of shaped text with positioned glyphs
///
/// A text run is a contiguous sequence of text in the same font, size, and style.
/// It contains the shaped glyphs with their positions, along with metrics needed
/// for line layout and baseline alignment.
///
/// # Properties
///
/// - All glyphs use the same font
/// - All text has the same style (weight, style, size)
/// - Glyphs are positioned relative to the run origin
/// - Contains metrics for baseline alignment
///
/// # Usage in Inline Layout
///
/// Text runs are the atomic units that the inline formatting context places
/// on lines. They can be split at soft wrap opportunities for line breaking.
///
/// # Example
///
/// ```rust,ignore
/// use fastrender::layout::inline::text_run::TextRun;
///
/// // A text run represents "Hello" shaped with a specific font
/// let run = TextRun {
///     text: "Hello".to_string(),
///     glyphs: vec![/* shaped glyphs */],
///     width: 45.0,
///     height: 20.0,
///     ascent: 14.0,
///     descent: 6.0,
///     line_height: 24.0,
///     font_size: 16.0,
///     // ...
/// };
/// ```
#[derive(Debug, Clone)]
pub struct TextRun {
    /// The original text content
    pub text: String,

    /// Positioned glyphs from text shaping
    pub glyphs: Vec<GlyphInfo>,

    /// Total width of the run in pixels
    pub width: f32,

    /// Total height of the run (ascent + descent) in pixels
    pub height: f32,

    /// Ascent: distance from baseline to top of em box
    pub ascent: f32,

    /// Descent: distance from baseline to bottom of em box (positive value)
    pub descent: f32,

    /// Line height for this run (may include leading)
    pub line_height: f32,

    /// Font size in pixels
    pub font_size: f32,

    /// Font family name
    pub font_family: String,

    /// Font weight
    pub font_weight: FontWeight,

    /// Font style (normal, italic, oblique)
    pub font_style: FontStyle,

    /// Reference to the loaded font data (shared via Arc)
    pub font_data: Option<Arc<Vec<u8>>>,

    /// Font face index within the font file
    pub font_index: u32,
}

impl TextRun {
    /// Creates a new empty text run
    pub fn new() -> Self {
        Self {
            text: String::new(),
            glyphs: Vec::new(),
            width: 0.0,
            height: 0.0,
            ascent: 0.0,
            descent: 0.0,
            line_height: 0.0,
            font_size: 0.0,
            font_family: String::new(),
            font_weight: FontWeight::NORMAL,
            font_style: FontStyle::Normal,
            font_data: None,
            font_index: 0,
        }
    }

    /// Creates a text run from pre-computed values
    pub fn from_parts(text: String, glyphs: Vec<GlyphInfo>, metrics: &ScaledMetrics, font: &LoadedFont) -> Self {
        // Calculate total width from glyph advances
        let width: f32 = glyphs.iter().map(|g| g.x_advance).sum();

        Self {
            text,
            glyphs,
            width,
            height: metrics.ascent + metrics.descent,
            ascent: metrics.ascent,
            descent: metrics.descent,
            line_height: metrics.line_height,
            font_size: metrics.font_size,
            font_family: font.family.clone(),
            font_weight: font.weight,
            font_style: font.style,
            font_data: Some(Arc::clone(&font.data)),
            font_index: font.index,
        }
    }

    /// Returns true if this text run is empty
    pub fn is_empty(&self) -> bool {
        self.text.is_empty() || self.glyphs.is_empty()
    }

    /// Returns the number of glyphs in this run
    pub fn glyph_count(&self) -> usize {
        self.glyphs.len()
    }

    /// Returns the number of characters in the source text
    pub fn char_count(&self) -> usize {
        self.text.chars().count()
    }

    /// Computes the bounding rectangle for this run at a given position
    pub fn bounds_at(&self, origin: Point) -> Rect {
        Rect::from_xywh(origin.x, origin.y, self.width, self.height)
    }

    /// Returns the baseline offset from the top of the run
    ///
    /// This is equal to the ascent and is used for positioning
    /// text within a line box.
    pub fn baseline_offset(&self) -> f32 {
        self.ascent
    }

    /// Computes the half-leading for line height calculation
    ///
    /// Leading is the extra space above and below the text box
    /// when line-height differs from the natural text height.
    ///
    /// CSS 2.1 Section 10.8.1: Leading is split evenly above and below.
    pub fn half_leading(&self) -> f32 {
        (self.line_height - self.height) / 2.0
    }

    /// Splits this text run at a character boundary
    ///
    /// Returns (left, right) where left contains characters [0, at)
    /// and right contains characters [at, len).
    ///
    /// # Arguments
    ///
    /// * `at` - Character index to split at (0-based)
    ///
    /// # Returns
    ///
    /// A tuple of (left_run, right_run), or None if split is invalid.
    pub fn split_at(&self, at: usize) -> Option<(TextRun, TextRun)> {
        let char_count = self.char_count();
        if at == 0 || at >= char_count {
            return None;
        }

        // Find byte offset for character boundary
        let byte_offset = self.text.char_indices().nth(at).map(|(i, _)| i)?;

        // Split text
        let left_text = self.text[..byte_offset].to_string();
        let right_text = self.text[byte_offset..].to_string();

        // Find glyph split point using cluster indices
        let cluster_at = at as u32;
        let glyph_split = self
            .glyphs
            .iter()
            .position(|g| g.cluster >= cluster_at)
            .unwrap_or(self.glyphs.len());

        let left_glyphs: Vec<GlyphInfo> = self.glyphs[..glyph_split].to_vec();
        let right_glyphs: Vec<GlyphInfo> = self.glyphs[glyph_split..]
            .iter()
            .map(|g| GlyphInfo {
                cluster: g.cluster - cluster_at,
                ..*g
            })
            .collect();

        // Calculate widths
        let left_width: f32 = left_glyphs.iter().map(|g| g.x_advance).sum();
        let right_width: f32 = right_glyphs.iter().map(|g| g.x_advance).sum();

        let left_run = TextRun {
            text: left_text,
            glyphs: left_glyphs,
            width: left_width,
            ..self.clone()
        };

        let right_run = TextRun {
            text: right_text,
            glyphs: right_glyphs,
            width: right_width,
            ..self.clone()
        };

        Some((left_run, right_run))
    }

    /// Finds the character index at a given x position
    ///
    /// Returns the character index that contains the given x position,
    /// useful for hit testing and text selection.
    ///
    /// # Arguments
    ///
    /// * `x` - X position relative to run origin
    ///
    /// # Returns
    ///
    /// Character index, or None if x is outside the run.
    pub fn char_at_x(&self, x: f32) -> Option<usize> {
        if x < 0.0 || x > self.width {
            return None;
        }

        let mut current_x = 0.0;
        for glyph in &self.glyphs {
            let glyph_mid = current_x + glyph.x_advance / 2.0;
            if x < glyph_mid {
                return Some(glyph.cluster as usize);
            }
            current_x += glyph.x_advance;
        }

        Some(self.char_count().saturating_sub(1))
    }

    /// Returns the x position of a character by index
    ///
    /// # Arguments
    ///
    /// * `char_index` - Character index (0-based)
    ///
    /// # Returns
    ///
    /// X position of the character start, or None if index is invalid.
    pub fn x_for_char(&self, char_index: usize) -> Option<f32> {
        if char_index > self.char_count() {
            return None;
        }

        let mut x = 0.0;
        let target_cluster = char_index as u32;

        for glyph in &self.glyphs {
            if glyph.cluster >= target_cluster {
                return Some(x);
            }
            x += glyph.x_advance;
        }

        Some(x)
    }
}

impl Default for TextRun {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// InlineItem - Items in Inline Formatting Context
// ============================================================================

/// An item that participates in inline formatting
///
/// Inline items are the building blocks of inline layout. They represent
/// different types of content that flow horizontally in an inline formatting
/// context: text runs, inline boxes, atomic inline-level boxes, and break
/// opportunities.
///
/// # CSS Specification
///
/// CSS 2.1 Section 9.2.2: Inline-level elements generate inline-level boxes.
///
/// # Variants
///
/// - `Text`: A text run with shaped glyphs
/// - `StartInlineBox`: Opening of an inline box (e.g., `<span>`)
/// - `EndInlineBox`: Closing of an inline box
/// - `Atomic`: An atomic inline-level box (inline-block, replaced element)
/// - `SoftBreakOpportunity`: A point where lines can break (e.g., after space)
/// - `HardBreak`: A forced line break (e.g., `<br>`)
#[derive(Debug, Clone)]
pub enum InlineItem {
    /// A run of shaped text
    ///
    /// Text runs are the most common inline items. They contain shaped
    /// glyphs that are ready to be positioned on a line.
    Text(TextRun),

    /// Start of an inline box
    ///
    /// Marks the beginning of an inline box like `<span>`. The inline box
    /// may split across multiple lines, with each line segment receiving
    /// its own padding/border/margin.
    StartInlineBox {
        /// Unique identifier for the inline box
        box_id: Option<usize>,

        /// Left margin in pixels
        margin_left: f32,

        /// Left border width in pixels
        border_left: f32,

        /// Left padding in pixels
        padding_left: f32,
    },

    /// End of an inline box
    ///
    /// Marks the end of an inline box. Must be paired with a StartInlineBox.
    EndInlineBox {
        /// Unique identifier for the inline box (matches StartInlineBox)
        box_id: Option<usize>,

        /// Right padding in pixels
        padding_right: f32,

        /// Right border width in pixels
        border_right: f32,

        /// Right margin in pixels
        margin_right: f32,
    },

    /// An atomic inline-level box
    ///
    /// Atomic boxes (inline-block, replaced elements like `<img>`) are laid
    /// out internally as blocks but participate in inline layout as single
    /// indivisible units. They cannot be split across lines.
    Atomic {
        /// Unique identifier for the box
        box_id: Option<usize>,

        /// Width of the atomic box
        width: f32,

        /// Height of the atomic box
        height: f32,

        /// Baseline offset from top (for vertical-align: baseline)
        baseline: f32,

        /// Left margin
        margin_left: f32,

        /// Right margin
        margin_right: f32,
    },

    /// A soft wrap opportunity
    ///
    /// Marks a point where the line can break if needed. Typically occurs
    /// after spaces, hyphens, and other break characters.
    ///
    /// CSS Text Level 3: "A soft wrap opportunity is a position in the text
    /// where content can be wrapped to the next line."
    SoftBreakOpportunity,

    /// A hard line break
    ///
    /// Forces a line break at this point (e.g., `<br>` element).
    HardBreak,
}

impl InlineItem {
    /// Creates a text item from a text run
    pub fn text(run: TextRun) -> Self {
        InlineItem::Text(run)
    }

    /// Creates a start inline box item
    pub fn start_inline_box(box_id: Option<usize>, margin_left: f32, border_left: f32, padding_left: f32) -> Self {
        InlineItem::StartInlineBox {
            box_id,
            margin_left,
            border_left,
            padding_left,
        }
    }

    /// Creates an end inline box item
    pub fn end_inline_box(box_id: Option<usize>, padding_right: f32, border_right: f32, margin_right: f32) -> Self {
        InlineItem::EndInlineBox {
            box_id,
            padding_right,
            border_right,
            margin_right,
        }
    }

    /// Creates an atomic inline item
    pub fn atomic(
        box_id: Option<usize>,
        width: f32,
        height: f32,
        baseline: f32,
        margin_left: f32,
        margin_right: f32,
    ) -> Self {
        InlineItem::Atomic {
            box_id,
            width,
            height,
            baseline,
            margin_left,
            margin_right,
        }
    }

    /// Creates a soft break opportunity
    pub fn soft_break() -> Self {
        InlineItem::SoftBreakOpportunity
    }

    /// Creates a hard break
    pub fn hard_break() -> Self {
        InlineItem::HardBreak
    }

    /// Returns the width of this item (0 for break opportunities)
    pub fn width(&self) -> f32 {
        match self {
            InlineItem::Text(run) => run.width,
            InlineItem::StartInlineBox {
                margin_left,
                border_left,
                padding_left,
                ..
            } => margin_left + border_left + padding_left,
            InlineItem::EndInlineBox {
                padding_right,
                border_right,
                margin_right,
                ..
            } => padding_right + border_right + margin_right,
            InlineItem::Atomic {
                width,
                margin_left,
                margin_right,
                ..
            } => margin_left + width + margin_right,
            InlineItem::SoftBreakOpportunity | InlineItem::HardBreak => 0.0,
        }
    }

    /// Returns the height of this item (0 for break opportunities)
    pub fn height(&self) -> f32 {
        match self {
            InlineItem::Text(run) => run.height,
            InlineItem::Atomic { height, .. } => *height,
            _ => 0.0,
        }
    }

    /// Returns true if this item is a break opportunity
    pub fn is_break_opportunity(&self) -> bool {
        matches!(self, InlineItem::SoftBreakOpportunity | InlineItem::HardBreak)
    }

    /// Returns true if this item forces a line break
    pub fn is_hard_break(&self) -> bool {
        matches!(self, InlineItem::HardBreak)
    }

    /// Returns true if this item is a text run
    pub fn is_text(&self) -> bool {
        matches!(self, InlineItem::Text(_))
    }

    /// Returns true if this item is an atomic box
    pub fn is_atomic(&self) -> bool {
        matches!(self, InlineItem::Atomic { .. })
    }

    /// Returns the text run if this is a text item
    pub fn as_text(&self) -> Option<&TextRun> {
        match self {
            InlineItem::Text(run) => Some(run),
            _ => None,
        }
    }

    /// Returns mutable reference to text run if this is a text item
    pub fn as_text_mut(&mut self) -> Option<&mut TextRun> {
        match self {
            InlineItem::Text(run) => Some(run),
            _ => None,
        }
    }
}

// ============================================================================
// TextRunBuilder - Builder for Creating Text Runs
// ============================================================================

/// Builder for creating text runs from text strings
///
/// TextRunBuilder handles the text shaping process, converting text strings
/// into positioned glyphs using the font system.
///
/// # Example
///
/// ```rust,ignore
/// use fastrender::layout::inline::text_run::TextRunBuilder;
/// use fastrender::text::font_loader::FontContext;
///
/// let font_ctx = FontContext::new();
/// let builder = TextRunBuilder::new(&font_ctx);
///
/// let run = builder.build(
///     "Hello, world!",
///     &["Arial".to_string(), "sans-serif".to_string()],
///     400,    // weight
///     false,  // italic
///     16.0,   // font size
/// )?;
///
/// println!("Run width: {}px", run.width);
/// ```
pub struct TextRunBuilder<'a> {
    font_context: &'a FontContext,
}

impl<'a> TextRunBuilder<'a> {
    /// Creates a new text run builder
    pub fn new(font_context: &'a FontContext) -> Self {
        Self { font_context }
    }

    /// Builds a text run from a text string
    ///
    /// # Arguments
    ///
    /// * `text` - The text content to shape
    /// * `families` - Font family names in priority order
    /// * `weight` - Font weight (100-900)
    /// * `italic` - Whether to use italic style
    /// * `font_size` - Font size in pixels
    ///
    /// # Returns
    ///
    /// A shaped TextRun, or an error if shaping fails.
    pub fn build(&self, text: &str, families: &[String], weight: u16, italic: bool, font_size: f32) -> Result<TextRun> {
        // Handle empty text
        if text.is_empty() {
            return Ok(TextRun::new());
        }

        // Get font
        let font = self
            .font_context
            .get_font_full(
                families,
                weight,
                if italic {
                    crate::text::font_db::FontStyle::Italic
                } else {
                    crate::text::font_db::FontStyle::Normal
                },
                crate::text::font_db::FontStretch::Normal,
            )
            .ok_or(Error::Font(FontError::NoFontsAvailable))?;

        // Get metrics
        let metrics = font.metrics().map(|m| m.scale(font_size)).map_err(|e| {
            Error::Font(FontError::LoadFailed {
                family: font.family.clone(),
                reason: format!("Failed to get metrics: {:?}", e),
            })
        })?;

        // Shape the text
        let glyphs = self.shape_text(text, &font, font_size)?;

        Ok(TextRun::from_parts(text.to_string(), glyphs, &metrics, &font))
    }

    /// Builds a text run using a pre-loaded font
    ///
    /// Use this when you already have a loaded font and want to avoid
    /// the font resolution step.
    pub fn build_with_font(&self, text: &str, font: &LoadedFont, font_size: f32) -> Result<TextRun> {
        // Handle empty text
        if text.is_empty() {
            return Ok(TextRun::new());
        }

        // Get metrics
        let metrics = font.metrics().map(|m| m.scale(font_size)).map_err(|e| {
            Error::Font(FontError::LoadFailed {
                family: font.family.clone(),
                reason: format!("Failed to get metrics: {:?}", e),
            })
        })?;

        // Shape the text
        let glyphs = self.shape_text(text, font, font_size)?;

        Ok(TextRun::from_parts(text.to_string(), glyphs, &metrics, font))
    }

    /// Shapes text using HarfBuzz (via rustybuzz)
    fn shape_text(&self, text: &str, font: &LoadedFont, font_size: f32) -> Result<Vec<GlyphInfo>> {
        // Get rustybuzz face
        let rb_face = Face::from_slice(&font.data, font.index).ok_or_else(|| {
            Error::Font(FontError::LoadFailed {
                family: font.family.clone(),
                reason: "Failed to create rustybuzz face".to_string(),
            })
        })?;

        // Calculate scale factor
        let units_per_em = rb_face.units_per_em() as f32;
        let scale = font_size / units_per_em;

        // Create and fill buffer
        let mut buffer = UnicodeBuffer::new();
        buffer.push_str(text);
        buffer.set_direction(Direction::LeftToRight);

        // Shape the text
        let output = rustybuzz::shape(&rb_face, &[], buffer);
        let glyph_infos = output.glyph_infos();
        let glyph_positions = output.glyph_positions();

        // Convert to our glyph format
        let mut glyphs = Vec::with_capacity(glyph_infos.len());
        for (info, pos) in glyph_infos.iter().zip(glyph_positions.iter()) {
            glyphs.push(GlyphInfo {
                glyph_id: info.glyph_id as u16,
                cluster: info.cluster,
                x_advance: pos.x_advance as f32 * scale,
                y_advance: pos.y_advance as f32 * scale,
                x_offset: pos.x_offset as f32 * scale,
                y_offset: pos.y_offset as f32 * scale,
            });
        }

        Ok(glyphs)
    }

    /// Creates text runs split at word boundaries
    ///
    /// Returns a vector of text runs, one per word, with soft break
    /// opportunities between them.
    ///
    /// # Arguments
    ///
    /// * `text` - The text content to shape
    /// * `families` - Font family names
    /// * `weight` - Font weight
    /// * `italic` - Italic flag
    /// * `font_size` - Font size in pixels
    ///
    /// # Returns
    ///
    /// A vector of InlineItems containing text runs and break opportunities.
    pub fn build_with_breaks(
        &self,
        text: &str,
        families: &[String],
        weight: u16,
        italic: bool,
        font_size: f32,
    ) -> Result<Vec<InlineItem>> {
        let mut items = Vec::new();

        // Get font once for all words
        let font = self
            .font_context
            .get_font_full(
                families,
                weight,
                if italic {
                    crate::text::font_db::FontStyle::Italic
                } else {
                    crate::text::font_db::FontStyle::Normal
                },
                crate::text::font_db::FontStretch::Normal,
            )
            .ok_or(Error::Font(FontError::NoFontsAvailable))?;

        // Split by whitespace while tracking positions
        let mut last_end = 0;
        for (start, word) in text.match_indices(|c: char| !c.is_whitespace()) {
            // Handle whitespace before this word
            if start > last_end {
                let whitespace = &text[last_end..start];
                // Create run for whitespace
                let ws_run = self.build_with_font(whitespace, &font, font_size)?;
                if !ws_run.is_empty() {
                    items.push(InlineItem::Text(ws_run));
                }
                // Add break opportunity after whitespace
                items.push(InlineItem::SoftBreakOpportunity);
            }

            // Create run for the word
            let word_run = self.build_with_font(word, &font, font_size)?;
            if !word_run.is_empty() {
                items.push(InlineItem::Text(word_run));
            }

            last_end = start + word.len();
        }

        // Handle trailing whitespace
        if last_end < text.len() {
            let trailing = &text[last_end..];
            let trailing_run = self.build_with_font(trailing, &font, font_size)?;
            if !trailing_run.is_empty() {
                items.push(InlineItem::Text(trailing_run));
            }
        }

        Ok(items)
    }
}

// ============================================================================
// LineMetrics - Metrics for a Line of Text
// ============================================================================

/// Metrics computed for a line of inline items
///
/// Contains the aggregated metrics needed for baseline alignment and
/// line height calculation.
///
/// NOTE: This is an internal helper type for tests. The canonical LineMetrics
/// type for inline layout is in the baseline module.
#[cfg(test)]
#[derive(Debug, Clone, Default)]
pub(crate) struct LineMetrics {
    /// Maximum ascent of all items on the line
    pub max_ascent: f32,

    /// Maximum descent of all items on the line
    pub max_descent: f32,

    /// Maximum line height of all items
    pub max_line_height: f32,

    /// Total width of all items on the line
    pub total_width: f32,
}

#[cfg(test)]
impl LineMetrics {
    /// Creates new empty line metrics
    pub fn new() -> Self {
        Self::default()
    }

    /// Updates metrics with a text run
    pub fn add_text_run(&mut self, run: &TextRun) {
        self.max_ascent = self.max_ascent.max(run.ascent);
        self.max_descent = self.max_descent.max(run.descent);
        self.max_line_height = self.max_line_height.max(run.line_height);
        self.total_width += run.width;
    }

    /// Updates metrics with an atomic box
    pub fn add_atomic(&mut self, height: f32, baseline: f32, width: f32) {
        let ascent = baseline;
        let descent = height - baseline;
        self.max_ascent = self.max_ascent.max(ascent);
        self.max_descent = self.max_descent.max(descent);
        self.max_line_height = self.max_line_height.max(height);
        self.total_width += width;
    }

    /// Returns the computed line height
    ///
    /// Line height is the maximum of:
    /// - Sum of max ascent and descent
    /// - Maximum line-height of any item
    pub fn line_height(&self) -> f32 {
        (self.max_ascent + self.max_descent).max(self.max_line_height)
    }

    /// Returns the baseline position from the top of the line
    pub fn baseline(&self) -> f32 {
        // Baseline is positioned at max_ascent from top
        // Plus half-leading if line-height > natural height
        let natural_height = self.max_ascent + self.max_descent;
        let line_height = self.line_height();
        let half_leading = (line_height - natural_height) / 2.0;
        half_leading + self.max_ascent
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    // ========================================================================
    // GlyphInfo Tests
    // ========================================================================

    #[test]
    fn test_glyph_info_new() {
        let glyph = GlyphInfo::new(72, 0, 10.5);

        assert_eq!(glyph.glyph_id, 72);
        assert_eq!(glyph.cluster, 0);
        assert_eq!(glyph.x_advance, 10.5);
        assert_eq!(glyph.y_advance, 0.0);
        assert_eq!(glyph.x_offset, 0.0);
        assert_eq!(glyph.y_offset, 0.0);
    }

    #[test]
    fn test_glyph_info_with_offsets() {
        let glyph = GlyphInfo::with_offsets(72, 0, 10.5, 1.0, 0.5, -0.5);

        assert_eq!(glyph.glyph_id, 72);
        assert_eq!(glyph.x_advance, 10.5);
        assert_eq!(glyph.y_advance, 1.0);
        assert_eq!(glyph.x_offset, 0.5);
        assert_eq!(glyph.y_offset, -0.5);
    }

    #[test]
    fn test_glyph_info_is_zero_width() {
        let zero_width = GlyphInfo::new(72, 0, 0.0);
        let normal = GlyphInfo::new(72, 0, 10.0);

        assert!(zero_width.is_zero_width());
        assert!(!normal.is_zero_width());
    }

    #[test]
    fn test_glyph_info_default() {
        let glyph = GlyphInfo::default();

        assert_eq!(glyph.glyph_id, 0);
        assert_eq!(glyph.cluster, 0);
        assert_eq!(glyph.x_advance, 0.0);
    }

    // ========================================================================
    // TextRun Tests
    // ========================================================================

    #[test]
    fn test_text_run_new() {
        let run = TextRun::new();

        assert!(run.is_empty());
        assert_eq!(run.glyph_count(), 0);
        assert_eq!(run.char_count(), 0);
        assert_eq!(run.width, 0.0);
    }

    #[test]
    fn test_text_run_default() {
        let run = TextRun::default();
        assert!(run.is_empty());
    }

    #[test]
    fn test_text_run_basic() {
        let glyphs = vec![
            GlyphInfo::new(72, 0, 10.0), // H
            GlyphInfo::new(101, 1, 8.0), // e
            GlyphInfo::new(108, 2, 5.0), // l
            GlyphInfo::new(108, 3, 5.0), // l
            GlyphInfo::new(111, 4, 8.0), // o
        ];

        let run = TextRun {
            text: "Hello".to_string(),
            glyphs,
            width: 36.0,
            height: 20.0,
            ascent: 14.0,
            descent: 6.0,
            line_height: 24.0,
            font_size: 16.0,
            font_family: "TestFont".to_string(),
            font_weight: FontWeight::NORMAL,
            font_style: FontStyle::Normal,
            font_data: None,
            font_index: 0,
        };

        assert!(!run.is_empty());
        assert_eq!(run.glyph_count(), 5);
        assert_eq!(run.char_count(), 5);
        assert_eq!(run.width, 36.0);
        assert_eq!(run.baseline_offset(), 14.0);
    }

    #[test]
    fn test_text_run_half_leading() {
        let run = TextRun {
            height: 20.0,
            line_height: 28.0,
            ..TextRun::new()
        };

        assert_eq!(run.half_leading(), 4.0);
    }

    #[test]
    fn test_text_run_bounds_at() {
        let run = TextRun {
            width: 50.0,
            height: 20.0,
            ..TextRun::new()
        };

        let bounds = run.bounds_at(Point::new(10.0, 20.0));

        assert_eq!(bounds.x(), 10.0);
        assert_eq!(bounds.y(), 20.0);
        assert_eq!(bounds.width(), 50.0);
        assert_eq!(bounds.height(), 20.0);
    }

    #[test]
    fn test_text_run_split_at() {
        let glyphs = vec![
            GlyphInfo::new(72, 0, 10.0), // H
            GlyphInfo::new(101, 1, 8.0), // e
            GlyphInfo::new(108, 2, 5.0), // l
            GlyphInfo::new(108, 3, 5.0), // l
            GlyphInfo::new(111, 4, 8.0), // o
        ];

        let run = TextRun {
            text: "Hello".to_string(),
            glyphs,
            width: 36.0,
            height: 20.0,
            ascent: 14.0,
            descent: 6.0,
            line_height: 24.0,
            font_size: 16.0,
            font_family: "TestFont".to_string(),
            font_weight: FontWeight::NORMAL,
            font_style: FontStyle::Normal,
            font_data: None,
            font_index: 0,
        };

        // Split at position 2 ("He" | "llo")
        let (left, right) = run.split_at(2).expect("Split should succeed");

        assert_eq!(left.text, "He");
        assert_eq!(left.glyph_count(), 2);
        assert_eq!(left.width, 18.0); // 10 + 8

        assert_eq!(right.text, "llo");
        assert_eq!(right.glyph_count(), 3);
        assert_eq!(right.width, 18.0); // 5 + 5 + 8
    }

    #[test]
    fn test_text_run_split_at_invalid() {
        let run = TextRun {
            text: "Hello".to_string(),
            glyphs: vec![
                GlyphInfo::new(72, 0, 10.0),
                GlyphInfo::new(101, 1, 8.0),
                GlyphInfo::new(108, 2, 5.0),
                GlyphInfo::new(108, 3, 5.0),
                GlyphInfo::new(111, 4, 8.0),
            ],
            ..TextRun::new()
        };

        // Can't split at 0
        assert!(run.split_at(0).is_none());

        // Can't split at end
        assert!(run.split_at(5).is_none());

        // Can't split past end
        assert!(run.split_at(10).is_none());
    }

    #[test]
    fn test_text_run_x_for_char() {
        let glyphs = vec![
            GlyphInfo::new(72, 0, 10.0),
            GlyphInfo::new(101, 1, 8.0),
            GlyphInfo::new(108, 2, 5.0),
        ];

        let run = TextRun {
            text: "Hel".to_string(),
            glyphs,
            width: 23.0,
            ..TextRun::new()
        };

        assert_eq!(run.x_for_char(0), Some(0.0));
        assert_eq!(run.x_for_char(1), Some(10.0));
        assert_eq!(run.x_for_char(2), Some(18.0));
        assert_eq!(run.x_for_char(3), Some(23.0));
        assert!(run.x_for_char(10).is_none());
    }

    // ========================================================================
    // InlineItem Tests
    // ========================================================================

    #[test]
    fn test_inline_item_text() {
        let run = TextRun {
            text: "Test".to_string(),
            width: 30.0,
            height: 20.0,
            ..TextRun::new()
        };

        let item = InlineItem::text(run.clone());

        assert!(item.is_text());
        assert!(!item.is_break_opportunity());
        assert_eq!(item.width(), 30.0);
        assert_eq!(item.height(), 20.0);
        assert!(item.as_text().is_some());
    }

    #[test]
    fn test_inline_item_start_inline_box() {
        let item = InlineItem::start_inline_box(Some(1), 5.0, 2.0, 10.0);

        assert!(!item.is_text());
        assert!(!item.is_break_opportunity());
        assert_eq!(item.width(), 17.0); // 5 + 2 + 10
    }

    #[test]
    fn test_inline_item_end_inline_box() {
        let item = InlineItem::end_inline_box(Some(1), 10.0, 2.0, 5.0);

        assert_eq!(item.width(), 17.0); // 10 + 2 + 5
    }

    #[test]
    fn test_inline_item_atomic() {
        let item = InlineItem::atomic(Some(1), 100.0, 50.0, 40.0, 5.0, 5.0);

        assert!(item.is_atomic());
        assert_eq!(item.width(), 110.0); // 5 + 100 + 5
        assert_eq!(item.height(), 50.0);
    }

    #[test]
    fn test_inline_item_soft_break() {
        let item = InlineItem::soft_break();

        assert!(item.is_break_opportunity());
        assert!(!item.is_hard_break());
        assert_eq!(item.width(), 0.0);
        assert_eq!(item.height(), 0.0);
    }

    #[test]
    fn test_inline_item_hard_break() {
        let item = InlineItem::hard_break();

        assert!(item.is_break_opportunity());
        assert!(item.is_hard_break());
    }

    // ========================================================================
    // LineMetrics Tests
    // ========================================================================

    #[test]
    fn test_line_metrics_new() {
        let metrics = LineMetrics::new();

        assert_eq!(metrics.max_ascent, 0.0);
        assert_eq!(metrics.max_descent, 0.0);
        assert_eq!(metrics.max_line_height, 0.0);
        assert_eq!(metrics.total_width, 0.0);
    }

    #[test]
    fn test_line_metrics_add_text_run() {
        let mut metrics = LineMetrics::new();

        let run = TextRun {
            width: 50.0,
            ascent: 14.0,
            descent: 6.0,
            line_height: 24.0,
            ..TextRun::new()
        };

        metrics.add_text_run(&run);

        assert_eq!(metrics.max_ascent, 14.0);
        assert_eq!(metrics.max_descent, 6.0);
        assert_eq!(metrics.max_line_height, 24.0);
        assert_eq!(metrics.total_width, 50.0);
    }

    #[test]
    fn test_line_metrics_add_multiple_runs() {
        let mut metrics = LineMetrics::new();

        let run1 = TextRun {
            width: 50.0,
            ascent: 14.0,
            descent: 6.0,
            line_height: 24.0,
            ..TextRun::new()
        };

        let run2 = TextRun {
            width: 30.0,
            ascent: 20.0, // Larger
            descent: 4.0,
            line_height: 28.0, // Larger
            ..TextRun::new()
        };

        metrics.add_text_run(&run1);
        metrics.add_text_run(&run2);

        assert_eq!(metrics.max_ascent, 20.0);
        assert_eq!(metrics.max_descent, 6.0);
        assert_eq!(metrics.max_line_height, 28.0);
        assert_eq!(metrics.total_width, 80.0);
    }

    #[test]
    fn test_line_metrics_add_atomic() {
        let mut metrics = LineMetrics::new();

        // Atomic box: 100x50 with baseline at 40
        metrics.add_atomic(50.0, 40.0, 100.0);

        assert_eq!(metrics.max_ascent, 40.0);
        assert_eq!(metrics.max_descent, 10.0);
        assert_eq!(metrics.max_line_height, 50.0);
        assert_eq!(metrics.total_width, 100.0);
    }

    #[test]
    fn test_line_metrics_line_height() {
        let mut metrics = LineMetrics::new();

        // Add run with small natural height but large line-height
        let run = TextRun {
            ascent: 10.0,
            descent: 5.0,
            line_height: 30.0,
            ..TextRun::new()
        };

        metrics.add_text_run(&run);

        // Line height should be the larger of natural height and line-height
        assert_eq!(metrics.line_height(), 30.0);
    }

    #[test]
    fn test_line_metrics_baseline() {
        let mut metrics = LineMetrics::new();

        let run = TextRun {
            ascent: 14.0,
            descent: 6.0,
            line_height: 24.0,
            ..TextRun::new()
        };

        metrics.add_text_run(&run);

        // Natural height = 20, line height = 24
        // Half leading = 2
        // Baseline = 2 + 14 = 16
        assert_eq!(metrics.baseline(), 16.0);
    }

    // ========================================================================
    // TextRunBuilder Tests (require fonts)
    // ========================================================================

    #[test]
    fn test_text_run_builder_empty_text() {
        let font_ctx = FontContext::new();
        let builder = TextRunBuilder::new(&font_ctx);

        let run = builder
            .build("", &["sans-serif".to_string()], 400, false, 16.0)
            .expect("Empty text should succeed");

        assert!(run.is_empty());
    }

    #[test]
    fn test_text_run_builder_basic() {
        let font_ctx = FontContext::new();

        // Skip if no fonts available
        if !font_ctx.has_fonts() {
            return;
        }

        let builder = TextRunBuilder::new(&font_ctx);

        let run = match builder.build("Hello", &["sans-serif".to_string()], 400, false, 16.0) {
            Ok(run) => run,
            Err(crate::error::Error::Font(_)) => return, // Skip if fonts unavailable
            Err(e) => panic!("Unexpected error: {}", e),
        };

        assert!(!run.is_empty());
        assert_eq!(run.text, "Hello");
        assert!(run.width > 0.0);
        assert!(run.height > 0.0);
        assert!(run.glyph_count() > 0);
    }

    #[test]
    fn test_text_run_builder_with_breaks() {
        let font_ctx = FontContext::new();

        // Skip if no fonts available
        if !font_ctx.has_fonts() {
            return;
        }

        let builder = TextRunBuilder::new(&font_ctx);

        let items = match builder.build_with_breaks("Hello world", &["sans-serif".to_string()], 400, false, 16.0) {
            Ok(items) => items,
            Err(crate::error::Error::Font(_)) => return, // Skip if fonts unavailable
            Err(e) => panic!("Unexpected error: {}", e),
        };

        // Should have text items and break opportunities
        let text_count = items.iter().filter(|i| i.is_text()).count();
        let break_count = items.iter().filter(|i| i.is_break_opportunity()).count();

        assert!(text_count >= 2); // At least "Hello" and "world"
        assert!(break_count >= 1); // At least one break opportunity
    }

    #[test]
    fn test_text_run_builder_font_size_scaling() {
        let font_ctx = FontContext::new();

        // Skip if no fonts available
        if !font_ctx.has_fonts() {
            return;
        }

        let builder = TextRunBuilder::new(&font_ctx);

        let run_16 = match builder.build("Test", &["sans-serif".to_string()], 400, false, 16.0) {
            Ok(run) => run,
            Err(crate::error::Error::Font(_)) => return, // Skip if fonts unavailable
            Err(e) => panic!("Unexpected error: {}", e),
        };

        let run_32 = match builder.build("Test", &["sans-serif".to_string()], 400, false, 32.0) {
            Ok(run) => run,
            Err(crate::error::Error::Font(_)) => return, // Skip if fonts unavailable
            Err(e) => panic!("Unexpected error: {}", e),
        };

        // Double font size should roughly double width
        assert!(run_32.width > run_16.width * 1.8);
        assert!(run_32.width < run_16.width * 2.2);
    }
}
