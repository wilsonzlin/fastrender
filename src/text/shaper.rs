//! Text Shaping using HarfBuzz (via rustybuzz)
//!
//! This module provides text shaping functionality that converts Unicode text
//! into positioned glyphs. Text shaping is essential for proper rendering of:
//!
//! - Complex scripts (Arabic, Devanagari, Thai, etc.)
//! - Ligatures (fi → ﬁ, ff → ﬀ)
//! - Kerning and glyph positioning
//! - Bidirectional text (RTL/LTR mixing)
//!
//! # Architecture
//!
//! The shaping pipeline:
//! 1. Create a `TextShaper` instance
//! 2. Call `shape_text()` with text, font, and properties
//! 3. Get back `ShapedGlyphs` with positioned glyph information
//!
//! # Example
//!
//! ```rust,ignore
//! use fastrender::text::shaper::{TextShaper, Direction, Script};
//! use fastrender::text::FontContext;
//!
//! let ctx = FontContext::new();
//! let shaper = TextShaper::new();
//!
//! if let Some(font) = ctx.get_sans_serif() {
//!     let shaped = shaper.shape_text(
//!         "Hello, world!",
//!         &font,
//!         16.0,
//!         Script::Latin,
//!         Direction::LeftToRight,
//!     ).unwrap();
//!
//!     println!("Total advance: {}px", shaped.total_advance);
//!     for glyph in &shaped.glyphs {
//!         println!("Glyph {}: advance={}", glyph.glyph_id, glyph.advance);
//!     }
//! }
//! ```
//!
//! # CSS/Unicode Specification References
//!
//! - [OpenType Specification](https://learn.microsoft.com/en-us/typography/opentype/spec/)
//! - [UAX #24: Unicode Script Property](https://www.unicode.org/reports/tr24/)
//! - [CSS Text Module Level 3](https://www.w3.org/TR/css-text-3/)

use crate::error::{Result, TextError};
use crate::text::font_db::LoadedFont;
use rustybuzz::{Face, UnicodeBuffer};
use std::sync::Arc;

// ============================================================================
// Direction
// ============================================================================

/// Text direction for shaping
///
/// Determines the primary direction of text flow, which affects how
/// glyphs are positioned and how bidirectional text is handled.
///
/// # CSS Specification
///
/// Maps to CSS `direction` property:
/// - `ltr` → `Direction::LeftToRight`
/// - `rtl` → `Direction::RightToLeft`
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum Direction {
    /// Left-to-right text (English, French, etc.)
    #[default]
    LeftToRight,
    /// Right-to-left text (Arabic, Hebrew, etc.)
    RightToLeft,
    /// Top-to-bottom (vertical CJK)
    TopToBottom,
    /// Bottom-to-top
    BottomToTop,
}

impl Direction {
    /// Convert to rustybuzz Direction
    fn to_rustybuzz(self) -> rustybuzz::Direction {
        match self {
            Direction::LeftToRight => rustybuzz::Direction::LeftToRight,
            Direction::RightToLeft => rustybuzz::Direction::RightToLeft,
            Direction::TopToBottom => rustybuzz::Direction::TopToBottom,
            Direction::BottomToTop => rustybuzz::Direction::BottomToTop,
        }
    }

    /// Check if this is a horizontal direction
    #[inline]
    pub fn is_horizontal(self) -> bool {
        matches!(self, Direction::LeftToRight | Direction::RightToLeft)
    }

    /// Check if this is a vertical direction
    #[inline]
    pub fn is_vertical(self) -> bool {
        matches!(self, Direction::TopToBottom | Direction::BottomToTop)
    }

    /// Check if this is a right-to-left direction
    #[inline]
    pub fn is_rtl(self) -> bool {
        self == Direction::RightToLeft
    }
}

// ============================================================================
// Script
// ============================================================================

/// Unicode script for text shaping
///
/// The script determines which shaping rules to apply. Different scripts
/// have different requirements:
/// - Latin: Simple left-to-right, kerning, basic ligatures
/// - Arabic: Right-to-left, contextual forms, mandatory ligatures
/// - Devanagari: Complex vowel positioning, consonant clusters
///
/// # UAX #24
///
/// Based on Unicode Script Property (UAX #24):
/// <https://www.unicode.org/reports/tr24/>
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum Script {
    /// Common characters (punctuation, digits)
    Common,
    /// Inherited script properties
    Inherited,
    /// Unknown or unassigned script
    #[default]
    Unknown,
    /// Latin script (A-Z, accented characters)
    Latin,
    /// Greek script
    Greek,
    /// Cyrillic script (Russian, etc.)
    Cyrillic,
    /// Arabic script (right-to-left, contextual shaping)
    Arabic,
    /// Hebrew script (right-to-left)
    Hebrew,
    /// Devanagari script (Hindi, Sanskrit)
    Devanagari,
    /// Bengali script
    Bengali,
    /// Tamil script
    Tamil,
    /// Thai script
    Thai,
    /// Han script (CJK ideographs)
    Han,
    /// Hiragana (Japanese)
    Hiragana,
    /// Katakana (Japanese)
    Katakana,
    /// Hangul (Korean)
    Hangul,
}

impl Script {
    /// Convert to ISO 15924 four-character tag for rustybuzz
    ///
    /// Returns the ISO 15924 script tag as a 4-byte array that can be
    /// used to create a rustybuzz Tag for script setting.
    pub fn iso15924_tag(self) -> [u8; 4] {
        match self {
            Script::Common => *b"Zyyy",
            Script::Inherited => *b"Zinh",
            Script::Unknown => *b"Zzzz",
            Script::Latin => *b"Latn",
            Script::Greek => *b"Grek",
            Script::Cyrillic => *b"Cyrl",
            Script::Arabic => *b"Arab",
            Script::Hebrew => *b"Hebr",
            Script::Devanagari => *b"Deva",
            Script::Bengali => *b"Beng",
            Script::Tamil => *b"Taml",
            Script::Thai => *b"Thai",
            Script::Han => *b"Hani",
            Script::Hiragana => *b"Hira",
            Script::Katakana => *b"Kana",
            Script::Hangul => *b"Hang",
        }
    }

    /// Convert to rustybuzz Script using ISO 15924 tag
    fn to_rustybuzz(self) -> Option<rustybuzz::Script> {
        let tag_bytes = self.iso15924_tag();
        let tag = rustybuzz::ttf_parser::Tag::from_bytes(&tag_bytes);
        rustybuzz::Script::from_iso15924_tag(tag)
    }

    /// Detect the script of a character
    ///
    /// Uses Unicode character properties to determine script.
    pub fn detect(c: char) -> Self {
        let cp = c as u32;

        // Basic Latin
        if (0x0041..=0x007A).contains(&cp) || (0x00C0..=0x024F).contains(&cp) {
            return Script::Latin;
        }

        // Greek
        if (0x0370..=0x03FF).contains(&cp) || (0x1F00..=0x1FFF).contains(&cp) {
            return Script::Greek;
        }

        // Cyrillic
        if (0x0400..=0x052F).contains(&cp) {
            return Script::Cyrillic;
        }

        // Arabic
        if (0x0600..=0x06FF).contains(&cp)
            || (0x0750..=0x077F).contains(&cp)
            || (0x08A0..=0x08FF).contains(&cp)
            || (0xFB50..=0xFDFF).contains(&cp)
            || (0xFE70..=0xFEFF).contains(&cp)
        {
            return Script::Arabic;
        }

        // Hebrew
        if (0x0590..=0x05FF).contains(&cp) || (0xFB1D..=0xFB4F).contains(&cp) {
            return Script::Hebrew;
        }

        // Devanagari
        if (0x0900..=0x097F).contains(&cp) || (0xA8E0..=0xA8FF).contains(&cp) {
            return Script::Devanagari;
        }

        // Bengali
        if (0x0980..=0x09FF).contains(&cp) {
            return Script::Bengali;
        }

        // Tamil
        if (0x0B80..=0x0BFF).contains(&cp) {
            return Script::Tamil;
        }

        // Thai
        if (0x0E00..=0x0E7F).contains(&cp) {
            return Script::Thai;
        }

        // CJK (Han)
        if (0x4E00..=0x9FFF).contains(&cp)
            || (0x3400..=0x4DBF).contains(&cp)
            || (0x20000..=0x2A6DF).contains(&cp)
            || (0xF900..=0xFAFF).contains(&cp)
        {
            return Script::Han;
        }

        // Hiragana
        if (0x3040..=0x309F).contains(&cp) {
            return Script::Hiragana;
        }

        // Katakana
        if (0x30A0..=0x30FF).contains(&cp) || (0x31F0..=0x31FF).contains(&cp) {
            return Script::Katakana;
        }

        // Hangul
        if (0xAC00..=0xD7AF).contains(&cp)
            || (0x1100..=0x11FF).contains(&cp)
            || (0x3130..=0x318F).contains(&cp)
        {
            return Script::Hangul;
        }

        // Common (punctuation, digits, symbols)
        if c.is_ascii_punctuation() || c.is_ascii_digit() || c.is_whitespace() {
            return Script::Common;
        }

        Script::Unknown
    }

    /// Detect the predominant script in a string
    pub fn detect_text(text: &str) -> Self {
        let mut script_counts = [0u32; 16];

        for c in text.chars() {
            let script = Self::detect(c);
            let idx = match script {
                Script::Common => 0,
                Script::Inherited => 1,
                Script::Unknown => 2,
                Script::Latin => 3,
                Script::Greek => 4,
                Script::Cyrillic => 5,
                Script::Arabic => 6,
                Script::Hebrew => 7,
                Script::Devanagari => 8,
                Script::Bengali => 9,
                Script::Tamil => 10,
                Script::Thai => 11,
                Script::Han => 12,
                Script::Hiragana => 13,
                Script::Katakana => 14,
                Script::Hangul => 15,
            };
            script_counts[idx] += 1;
        }

        // Find script with highest count (excluding Common/Inherited/Unknown)
        let mut max_idx = 3; // Default to Latin
        let mut max_count = 0;

        for (idx, &count) in script_counts.iter().enumerate().skip(3) {
            if count > max_count {
                max_count = count;
                max_idx = idx;
            }
        }

        match max_idx {
            3 => Script::Latin,
            4 => Script::Greek,
            5 => Script::Cyrillic,
            6 => Script::Arabic,
            7 => Script::Hebrew,
            8 => Script::Devanagari,
            9 => Script::Bengali,
            10 => Script::Tamil,
            11 => Script::Thai,
            12 => Script::Han,
            13 => Script::Hiragana,
            14 => Script::Katakana,
            15 => Script::Hangul,
            _ => Script::Latin,
        }
    }

    /// Get the default direction for this script
    pub fn default_direction(self) -> Direction {
        match self {
            Script::Arabic | Script::Hebrew => Direction::RightToLeft,
            _ => Direction::LeftToRight,
        }
    }
}

// ============================================================================
// GlyphPosition
// ============================================================================

/// Position and metrics for a single shaped glyph
///
/// After shaping, each glyph has:
/// - A glyph ID (index in the font)
/// - Positioning information (offsets and advances)
/// - Cluster information (mapping back to source text)
///
/// # Coordinate System
///
/// - `x_offset`, `y_offset`: Offset from the current pen position
/// - `advance`: How much to move the pen after drawing this glyph
///
/// For horizontal text:
/// - `advance` moves the pen right (LTR) or left (RTL)
/// - `x_offset`/`y_offset` adjust glyph placement for combining marks, etc.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct GlyphPosition {
    /// Glyph ID in the font (index into glyph table)
    pub glyph_id: u32,

    /// Cluster index (maps back to character position in source text)
    ///
    /// Multiple glyphs can share a cluster (ligatures) or
    /// multiple characters can map to one glyph.
    pub cluster: u32,

    /// Horizontal advance (pixels) - how much to move pen after this glyph
    pub advance: f32,

    /// Vertical advance (pixels) - for vertical text
    pub advance_y: f32,

    /// X offset from pen position (pixels)
    pub offset_x: f32,

    /// Y offset from pen position (pixels)
    pub offset_y: f32,
}

impl GlyphPosition {
    /// Creates a new glyph position with default values
    pub fn new(glyph_id: u32, cluster: u32) -> Self {
        Self {
            glyph_id,
            cluster,
            advance: 0.0,
            advance_y: 0.0,
            offset_x: 0.0,
            offset_y: 0.0,
        }
    }

    /// Creates a glyph position with all values specified
    pub fn with_metrics(
        glyph_id: u32,
        cluster: u32,
        advance: f32,
        advance_y: f32,
        offset_x: f32,
        offset_y: f32,
    ) -> Self {
        Self {
            glyph_id,
            cluster,
            advance,
            advance_y,
            offset_x,
            offset_y,
        }
    }
}

// ============================================================================
// GlyphCluster
// ============================================================================

/// Information about a glyph cluster
///
/// A cluster maps characters to glyphs. The relationship can be:
/// - 1:1 - One character → one glyph (most common for Latin)
/// - N:1 - Multiple characters → one glyph (ligatures: "fi" → ﬁ)
/// - 1:N - One character → multiple glyphs (diacritics, complex scripts)
///
/// Clusters are essential for:
/// - Hit testing (finding which character was clicked)
/// - Text selection (selecting character boundaries)
/// - Cursor positioning
#[derive(Debug, Clone, PartialEq)]
pub struct GlyphCluster {
    /// Start byte offset in original text
    pub text_start: usize,

    /// Byte length in original text
    pub text_len: usize,

    /// Start index in glyphs array
    pub glyph_start: usize,

    /// Number of glyphs in this cluster
    pub glyph_count: usize,

    /// Total advance width of this cluster
    pub advance: f32,
}

impl GlyphCluster {
    /// Creates a new cluster
    pub fn new(text_start: usize, text_len: usize, glyph_start: usize, glyph_count: usize, advance: f32) -> Self {
        Self {
            text_start,
            text_len,
            glyph_start,
            glyph_count,
            advance,
        }
    }

    /// Check if this cluster contains a text position
    #[inline]
    pub fn contains_text_pos(&self, pos: usize) -> bool {
        pos >= self.text_start && pos < self.text_start + self.text_len
    }
}

// ============================================================================
// ShapedGlyphs
// ============================================================================

/// Result of text shaping - positioned glyphs ready for rendering
///
/// Contains all information needed to render the shaped text:
/// - Glyph positions and IDs
/// - Cluster information for hit testing
/// - Total metrics (width, advance)
///
/// # Example
///
/// ```rust,ignore
/// let shaped = shaper.shape_text("Hello", &font, 16.0, Script::Latin, Direction::LeftToRight)?;
///
/// // Iterate glyphs for rendering
/// let mut x = 0.0;
/// for glyph in &shaped.glyphs {
///     let glyph_x = x + glyph.offset_x;
///     let glyph_y = glyph.offset_y;
///     // Draw glyph at (glyph_x, glyph_y)
///     x += glyph.advance;
/// }
/// ```
#[derive(Debug, Clone)]
pub struct ShapedGlyphs {
    /// The original text that was shaped
    pub text: String,

    /// Shaped glyph positions
    pub glyphs: Vec<GlyphPosition>,

    /// Glyph clusters (character → glyph mapping)
    pub clusters: Vec<GlyphCluster>,

    /// Total horizontal advance (width of shaped text)
    pub total_advance: f32,

    /// Total vertical advance (for vertical text)
    pub total_advance_y: f32,

    /// Direction the text was shaped with
    pub direction: Direction,

    /// Script the text was shaped with
    pub script: Script,

    /// Font size used for shaping (pixels)
    pub font_size: f32,
}

impl ShapedGlyphs {
    /// Creates an empty ShapedGlyphs instance
    pub fn empty() -> Self {
        Self {
            text: String::new(),
            glyphs: Vec::new(),
            clusters: Vec::new(),
            total_advance: 0.0,
            total_advance_y: 0.0,
            direction: Direction::LeftToRight,
            script: Script::Latin,
            font_size: 0.0,
        }
    }

    /// Returns the number of glyphs
    #[inline]
    pub fn glyph_count(&self) -> usize {
        self.glyphs.len()
    }

    /// Returns true if there are no glyphs
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.glyphs.is_empty()
    }

    /// Find the cluster containing a text position
    pub fn cluster_at_text_pos(&self, pos: usize) -> Option<&GlyphCluster> {
        self.clusters.iter().find(|c| c.contains_text_pos(pos))
    }

    /// Get the X position for a text offset
    ///
    /// Useful for cursor positioning.
    pub fn x_position_for_text_offset(&self, offset: usize) -> f32 {
        if offset == 0 {
            return 0.0;
        }

        let mut x = 0.0;
        for cluster in &self.clusters {
            if offset <= cluster.text_start {
                break;
            }
            if offset >= cluster.text_start + cluster.text_len {
                x += cluster.advance;
            } else {
                // Offset is within this cluster - interpolate
                let fraction = (offset - cluster.text_start) as f32 / cluster.text_len as f32;
                x += cluster.advance * fraction;
                break;
            }
        }
        x
    }

    /// Get the text offset for an X position
    ///
    /// Useful for hit testing (click → character).
    pub fn text_offset_for_x_position(&self, x: f32) -> usize {
        if x <= 0.0 {
            return 0;
        }

        let mut current_x = 0.0;
        for cluster in &self.clusters {
            let cluster_end_x = current_x + cluster.advance;
            if x < cluster_end_x {
                // Click is within this cluster
                let fraction = (x - current_x) / cluster.advance;
                if fraction < 0.5 {
                    return cluster.text_start;
                } else {
                    return cluster.text_start + cluster.text_len;
                }
            }
            current_x = cluster_end_x;
        }

        // Past the end
        self.text.len()
    }
}

// ============================================================================
// TextShaper
// ============================================================================

/// Text shaper using HarfBuzz (via rustybuzz)
///
/// The `TextShaper` handles the conversion of Unicode text to positioned glyphs.
/// It uses rustybuzz (a pure Rust port of HarfBuzz) for shaping.
///
/// # Thread Safety
///
/// `TextShaper` is stateless and can be shared across threads. Each call
/// to `shape_text` is independent.
///
/// # Example
///
/// ```rust,ignore
/// use fastrender::text::shaper::{TextShaper, Direction, Script};
///
/// let shaper = TextShaper::new();
///
/// // Shape Latin text
/// let shaped = shaper.shape_text(
///     "Hello",
///     &font,
///     16.0,
///     Script::Latin,
///     Direction::LeftToRight,
/// )?;
///
/// // Shape Arabic text (RTL)
/// let shaped = shaper.shape_text(
///     "مرحبا",
///     &font,
///     16.0,
///     Script::Arabic,
///     Direction::RightToLeft,
/// )?;
/// ```
#[derive(Debug, Clone, Default)]
pub struct TextShaper;

impl TextShaper {
    /// Creates a new text shaper
    pub fn new() -> Self {
        Self
    }

    /// Shape text with full control over shaping parameters
    ///
    /// This is the main shaping method that converts Unicode text to positioned glyphs.
    ///
    /// # Arguments
    ///
    /// * `text` - The Unicode text to shape
    /// * `font` - The loaded font to use for shaping
    /// * `font_size` - Font size in pixels
    /// * `script` - Unicode script of the text
    /// * `direction` - Text direction (LTR, RTL, etc.)
    ///
    /// # Returns
    ///
    /// `ShapedGlyphs` containing positioned glyph information.
    ///
    /// # Errors
    ///
    /// Returns `TextError::ShapingFailed` if the font cannot be parsed.
    ///
    /// # Example
    ///
    /// ```rust,ignore
    /// let shaped = shaper.shape_text(
    ///     "Hello, world!",
    ///     &font,
    ///     16.0,
    ///     Script::Latin,
    ///     Direction::LeftToRight,
    /// )?;
    /// ```
    pub fn shape_text(
        &self,
        text: &str,
        font: &LoadedFont,
        font_size: f32,
        script: Script,
        direction: Direction,
    ) -> Result<ShapedGlyphs> {
        // Handle empty text
        if text.is_empty() {
            return Ok(ShapedGlyphs {
                text: String::new(),
                glyphs: Vec::new(),
                clusters: Vec::new(),
                total_advance: 0.0,
                total_advance_y: 0.0,
                direction,
                script,
                font_size,
            });
        }

        // Create rustybuzz face from font data
        let rb_face = Face::from_slice(&font.data, font.index).ok_or_else(|| TextError::ShapingFailed {
            text: text.chars().take(50).collect(),
            reason: "Failed to create HarfBuzz face from font data".to_string(),
        })?;

        // Calculate scale factor
        let units_per_em = rb_face.units_per_em() as f32;
        let scale = font_size / units_per_em;

        // Create and configure the buffer
        let mut buffer = UnicodeBuffer::new();
        buffer.push_str(text);
        buffer.set_direction(direction.to_rustybuzz());

        // Set script if conversion succeeds
        if let Some(rb_script) = script.to_rustybuzz() {
            buffer.set_script(rb_script);
        }

        // Shape the text
        let output = rustybuzz::shape(&rb_face, &[], buffer);

        // Extract glyph information
        let glyph_infos = output.glyph_infos();
        let glyph_positions = output.glyph_positions();

        let mut glyphs = Vec::with_capacity(glyph_infos.len());
        let mut total_advance = 0.0f32;
        let mut total_advance_y = 0.0f32;

        for (info, pos) in glyph_infos.iter().zip(glyph_positions.iter()) {
            let advance = pos.x_advance as f32 * scale;
            let advance_y = pos.y_advance as f32 * scale;
            let offset_x = pos.x_offset as f32 * scale;
            let offset_y = pos.y_offset as f32 * scale;

            glyphs.push(GlyphPosition {
                glyph_id: info.glyph_id,
                cluster: info.cluster,
                advance,
                advance_y,
                offset_x,
                offset_y,
            });

            total_advance += advance;
            total_advance_y += advance_y;
        }

        // Build cluster information
        let clusters = self.build_clusters(text, &glyphs);

        Ok(ShapedGlyphs {
            text: text.to_string(),
            glyphs,
            clusters,
            total_advance,
            total_advance_y,
            direction,
            script,
            font_size,
        })
    }

    /// Shape text with automatic script detection
    ///
    /// Automatically detects the script and direction from the text content.
    ///
    /// # Example
    ///
    /// ```rust,ignore
    /// // Automatically detects Arabic script and RTL direction
    /// let shaped = shaper.shape_text_auto("مرحبا", &font, 16.0)?;
    /// ```
    pub fn shape_text_auto(&self, text: &str, font: &LoadedFont, font_size: f32) -> Result<ShapedGlyphs> {
        let script = Script::detect_text(text);
        let direction = script.default_direction();
        self.shape_text(text, font, font_size, script, direction)
    }

    /// Shape text with default Latin/LTR settings
    ///
    /// Convenience method for simple Latin text.
    pub fn shape_text_simple(&self, text: &str, font: &LoadedFont, font_size: f32) -> Result<ShapedGlyphs> {
        self.shape_text(text, font, font_size, Script::Latin, Direction::LeftToRight)
    }

    /// Build cluster information from glyphs
    fn build_clusters(&self, text: &str, glyphs: &[GlyphPosition]) -> Vec<GlyphCluster> {
        if glyphs.is_empty() {
            return Vec::new();
        }

        let text_bytes = text.as_bytes();
        let mut clusters = Vec::new();
        let mut current_cluster_start_byte = glyphs[0].cluster as usize;
        let mut current_glyph_start = 0;
        let mut current_advance = 0.0f32;

        for (glyph_idx, glyph) in glyphs.iter().enumerate() {
            let cluster_byte = glyph.cluster as usize;

            if cluster_byte != current_cluster_start_byte {
                // New cluster - finish the current one
                let text_len = self.cluster_text_len(text_bytes, current_cluster_start_byte, cluster_byte);
                clusters.push(GlyphCluster::new(
                    current_cluster_start_byte,
                    text_len,
                    current_glyph_start,
                    glyph_idx - current_glyph_start,
                    current_advance,
                ));

                current_cluster_start_byte = cluster_byte;
                current_glyph_start = glyph_idx;
                current_advance = 0.0;
            }

            current_advance += glyph.advance;
        }

        // Add the last cluster
        let text_len = text.len().saturating_sub(current_cluster_start_byte);
        clusters.push(GlyphCluster::new(
            current_cluster_start_byte,
            text_len,
            current_glyph_start,
            glyphs.len() - current_glyph_start,
            current_advance,
        ));

        clusters
    }

    /// Calculate text length for a cluster
    fn cluster_text_len(&self, _text_bytes: &[u8], start: usize, end: usize) -> usize {
        // Use abs_diff for RTL text where clusters may be in reverse order
        end.abs_diff(start)
    }

    /// Measure text width without full shaping
    ///
    /// This is a convenience method that shapes text and returns just the width.
    /// For repeated measurements, consider caching the full `ShapedGlyphs`.
    pub fn measure_width(&self, text: &str, font: &LoadedFont, font_size: f32) -> Result<f32> {
        let shaped = self.shape_text_auto(text, font, font_size)?;
        Ok(shaped.total_advance)
    }
}

// ============================================================================
// OpenType Feature Tags
// ============================================================================

/// Common OpenType feature tags
///
/// These can be used to enable/disable specific shaping features.
/// Future enhancement: Add feature support to shape_text.
#[allow(dead_code)]
pub mod features {
    /// Standard ligatures (fi, fl, ff, ffi, ffl)
    pub const LIGA: &str = "liga";
    /// Contextual ligatures
    pub const CLIG: &str = "clig";
    /// Discretionary ligatures
    pub const DLIG: &str = "dlig";
    /// Kerning
    pub const KERN: &str = "kern";
    /// Small caps
    pub const SMCP: &str = "smcp";
    /// All small caps
    pub const C2SC: &str = "c2sc";
    /// Oldstyle figures
    pub const ONUM: &str = "onum";
    /// Lining figures
    pub const LNUM: &str = "lnum";
    /// Tabular figures
    pub const TNUM: &str = "tnum";
    /// Proportional figures
    pub const PNUM: &str = "pnum";
}

#[cfg(test)]
mod tests {
    use super::*;

    // ========================================================================
    // Direction tests
    // ========================================================================

    #[test]
    fn test_direction_default() {
        assert_eq!(Direction::default(), Direction::LeftToRight);
    }

    #[test]
    fn test_direction_is_horizontal() {
        assert!(Direction::LeftToRight.is_horizontal());
        assert!(Direction::RightToLeft.is_horizontal());
        assert!(!Direction::TopToBottom.is_horizontal());
        assert!(!Direction::BottomToTop.is_horizontal());
    }

    #[test]
    fn test_direction_is_vertical() {
        assert!(!Direction::LeftToRight.is_vertical());
        assert!(!Direction::RightToLeft.is_vertical());
        assert!(Direction::TopToBottom.is_vertical());
        assert!(Direction::BottomToTop.is_vertical());
    }

    #[test]
    fn test_direction_is_rtl() {
        assert!(!Direction::LeftToRight.is_rtl());
        assert!(Direction::RightToLeft.is_rtl());
    }

    // ========================================================================
    // Script tests
    // ========================================================================

    #[test]
    fn test_script_default() {
        assert_eq!(Script::default(), Script::Unknown);
    }

    #[test]
    fn test_script_detect_latin() {
        assert_eq!(Script::detect('A'), Script::Latin);
        assert_eq!(Script::detect('z'), Script::Latin);
        assert_eq!(Script::detect('é'), Script::Latin);
    }

    #[test]
    fn test_script_detect_arabic() {
        assert_eq!(Script::detect('م'), Script::Arabic);
        assert_eq!(Script::detect('ر'), Script::Arabic);
    }

    #[test]
    fn test_script_detect_hebrew() {
        assert_eq!(Script::detect('א'), Script::Hebrew);
        assert_eq!(Script::detect('ש'), Script::Hebrew);
    }

    #[test]
    fn test_script_detect_cyrillic() {
        assert_eq!(Script::detect('А'), Script::Cyrillic);
        assert_eq!(Script::detect('Я'), Script::Cyrillic);
    }

    #[test]
    fn test_script_detect_han() {
        assert_eq!(Script::detect('中'), Script::Han);
        assert_eq!(Script::detect('国'), Script::Han);
    }

    #[test]
    fn test_script_detect_hiragana() {
        assert_eq!(Script::detect('あ'), Script::Hiragana);
        assert_eq!(Script::detect('い'), Script::Hiragana);
    }

    #[test]
    fn test_script_detect_katakana() {
        assert_eq!(Script::detect('ア'), Script::Katakana);
        assert_eq!(Script::detect('イ'), Script::Katakana);
    }

    #[test]
    fn test_script_detect_common() {
        assert_eq!(Script::detect('1'), Script::Common);
        assert_eq!(Script::detect('.'), Script::Common);
        assert_eq!(Script::detect(' '), Script::Common);
    }

    #[test]
    fn test_script_detect_text() {
        assert_eq!(Script::detect_text("Hello, world!"), Script::Latin);
        assert_eq!(Script::detect_text("مرحبا"), Script::Arabic);
        assert_eq!(Script::detect_text("שלום"), Script::Hebrew);
        assert_eq!(Script::detect_text("Привет"), Script::Cyrillic);
    }

    #[test]
    fn test_script_default_direction() {
        assert_eq!(Script::Latin.default_direction(), Direction::LeftToRight);
        assert_eq!(Script::Arabic.default_direction(), Direction::RightToLeft);
        assert_eq!(Script::Hebrew.default_direction(), Direction::RightToLeft);
        assert_eq!(Script::Han.default_direction(), Direction::LeftToRight);
    }

    // ========================================================================
    // GlyphPosition tests
    // ========================================================================

    #[test]
    fn test_glyph_position_new() {
        let glyph = GlyphPosition::new(42, 5);
        assert_eq!(glyph.glyph_id, 42);
        assert_eq!(glyph.cluster, 5);
        assert_eq!(glyph.advance, 0.0);
        assert_eq!(glyph.offset_x, 0.0);
    }

    #[test]
    fn test_glyph_position_with_metrics() {
        let glyph = GlyphPosition::with_metrics(42, 5, 10.5, 0.0, 1.0, -2.0);
        assert_eq!(glyph.glyph_id, 42);
        assert_eq!(glyph.cluster, 5);
        assert_eq!(glyph.advance, 10.5);
        assert_eq!(glyph.offset_x, 1.0);
        assert_eq!(glyph.offset_y, -2.0);
    }

    // ========================================================================
    // GlyphCluster tests
    // ========================================================================

    #[test]
    fn test_glyph_cluster_new() {
        let cluster = GlyphCluster::new(0, 3, 0, 2, 15.0);
        assert_eq!(cluster.text_start, 0);
        assert_eq!(cluster.text_len, 3);
        assert_eq!(cluster.glyph_start, 0);
        assert_eq!(cluster.glyph_count, 2);
        assert_eq!(cluster.advance, 15.0);
    }

    #[test]
    fn test_glyph_cluster_contains_text_pos() {
        let cluster = GlyphCluster::new(5, 3, 0, 1, 10.0);
        assert!(!cluster.contains_text_pos(4));
        assert!(cluster.contains_text_pos(5));
        assert!(cluster.contains_text_pos(6));
        assert!(cluster.contains_text_pos(7));
        assert!(!cluster.contains_text_pos(8));
    }

    // ========================================================================
    // ShapedGlyphs tests
    // ========================================================================

    #[test]
    fn test_shaped_glyphs_empty() {
        let shaped = ShapedGlyphs::empty();
        assert!(shaped.is_empty());
        assert_eq!(shaped.glyph_count(), 0);
        assert_eq!(shaped.total_advance, 0.0);
    }

    #[test]
    fn test_shaped_glyphs_x_position_for_text_offset() {
        let shaped = ShapedGlyphs {
            text: "abc".to_string(),
            glyphs: vec![
                GlyphPosition::with_metrics(1, 0, 10.0, 0.0, 0.0, 0.0),
                GlyphPosition::with_metrics(2, 1, 10.0, 0.0, 0.0, 0.0),
                GlyphPosition::with_metrics(3, 2, 10.0, 0.0, 0.0, 0.0),
            ],
            clusters: vec![
                GlyphCluster::new(0, 1, 0, 1, 10.0),
                GlyphCluster::new(1, 1, 1, 1, 10.0),
                GlyphCluster::new(2, 1, 2, 1, 10.0),
            ],
            total_advance: 30.0,
            total_advance_y: 0.0,
            direction: Direction::LeftToRight,
            script: Script::Latin,
            font_size: 16.0,
        };

        assert_eq!(shaped.x_position_for_text_offset(0), 0.0);
        assert_eq!(shaped.x_position_for_text_offset(1), 10.0);
        assert_eq!(shaped.x_position_for_text_offset(2), 20.0);
        assert_eq!(shaped.x_position_for_text_offset(3), 30.0);
    }

    #[test]
    fn test_shaped_glyphs_text_offset_for_x_position() {
        let shaped = ShapedGlyphs {
            text: "abc".to_string(),
            glyphs: vec![
                GlyphPosition::with_metrics(1, 0, 10.0, 0.0, 0.0, 0.0),
                GlyphPosition::with_metrics(2, 1, 10.0, 0.0, 0.0, 0.0),
                GlyphPosition::with_metrics(3, 2, 10.0, 0.0, 0.0, 0.0),
            ],
            clusters: vec![
                GlyphCluster::new(0, 1, 0, 1, 10.0),
                GlyphCluster::new(1, 1, 1, 1, 10.0),
                GlyphCluster::new(2, 1, 2, 1, 10.0),
            ],
            total_advance: 30.0,
            total_advance_y: 0.0,
            direction: Direction::LeftToRight,
            script: Script::Latin,
            font_size: 16.0,
        };

        assert_eq!(shaped.text_offset_for_x_position(0.0), 0);
        assert_eq!(shaped.text_offset_for_x_position(4.0), 0); // Before middle of first cluster
        assert_eq!(shaped.text_offset_for_x_position(6.0), 1); // After middle of first cluster
        assert_eq!(shaped.text_offset_for_x_position(15.0), 2);
        assert_eq!(shaped.text_offset_for_x_position(100.0), 3); // Past end
    }

    // ========================================================================
    // TextShaper tests
    // ========================================================================

    #[test]
    fn test_text_shaper_new() {
        let shaper = TextShaper::new();
        // Just verify it can be created
        assert_eq!(std::mem::size_of_val(&shaper), 0); // Zero-sized type
    }

    #[test]
    fn test_text_shaper_default() {
        let shaper = TextShaper::default();
        assert_eq!(std::mem::size_of_val(&shaper), 0);
    }
}
