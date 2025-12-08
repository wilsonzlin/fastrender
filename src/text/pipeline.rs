//! Text Shaping Pipeline
//!
//! Coordinates bidi analysis, script itemization, and text shaping into a unified process.
//!
//! # Architecture
//!
//! The shaping pipeline processes text through multiple stages:
//!
//! ```text
//! Input Text → Bidi Analysis → Script Itemization → Font Matching → Shaping → Output
//! ```
//!
//! Each stage handles a specific aspect of text processing:
//!
//! 1. **Bidi Analysis**: Determines text direction (LTR/RTL) using UAX #9
//! 2. **Script Itemization**: Splits text into runs of the same script
//! 3. **Font Matching**: Assigns fonts to each run with fallback support
//! 4. **Shaping**: Converts characters to positioned glyphs using HarfBuzz
//!
//! # Example
//!
//! ```rust,ignore
//! use fastrender::text::pipeline::{ShapingPipeline, Direction};
//! use fastrender::text::FontContext;
//! use fastrender::ComputedStyle;
//!
//! let mut pipeline = ShapingPipeline::new();
//! let font_context = FontContext::new();
//! let style = ComputedStyle::default();
//!
//! let shaped_runs = pipeline.shape("Hello, world!", &style, &font_context)?;
//! for run in shaped_runs {
//!     println!("Run: {} glyphs, {}px advance", run.glyphs.len(), run.advance);
//! }
//! ```
//!
//! # Unicode Compliance
//!
//! This implementation aims to support:
//! - **UAX #9**: Unicode Bidirectional Algorithm
//! - **UAX #24**: Script Property
//! - **OpenType GSUB/GPOS**: Glyph substitution and positioning
//!
//! # References
//!
//! - Unicode Bidirectional Algorithm (UAX #9): <https://www.unicode.org/reports/tr9/>
//! - HarfBuzz documentation: <https://harfbuzz.github.io/>
//! - rustybuzz documentation: <https://docs.rs/rustybuzz/>

use crate::error::{Result, TextError};
use crate::style::types::{Direction as CssDirection, FontStyle as CssFontStyle};
use crate::style::ComputedStyle;
use crate::text::font_db::{FontStyle, LoadedFont};
use crate::text::font_loader::FontContext;
use rustybuzz::{Direction as HbDirection, Face, UnicodeBuffer};
use std::sync::Arc;
use unicode_bidi::{BidiInfo, Level};

// ============================================================================
// Core Types
// ============================================================================

/// Text direction for layout and rendering.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Direction {
    /// Left-to-right text (English, most European languages)
    #[default]
    LeftToRight,
    /// Right-to-left text (Arabic, Hebrew)
    RightToLeft,
}

impl Direction {
    /// Creates direction from a bidi level.
    ///
    /// Even levels (0, 2, 4...) are LTR, odd levels (1, 3, 5...) are RTL.
    #[inline]
    pub fn from_level(level: Level) -> Self {
        if level.is_ltr() {
            Self::LeftToRight
        } else {
            Self::RightToLeft
        }
    }

    /// Converts to rustybuzz Direction.
    #[inline]
    pub fn to_harfbuzz(self) -> HbDirection {
        match self {
            Self::LeftToRight => HbDirection::LeftToRight,
            Self::RightToLeft => HbDirection::RightToLeft,
        }
    }

    /// Returns true if this is left-to-right.
    #[inline]
    pub fn is_ltr(self) -> bool {
        matches!(self, Self::LeftToRight)
    }

    /// Returns true if this is right-to-left.
    #[inline]
    pub fn is_rtl(self) -> bool {
        matches!(self, Self::RightToLeft)
    }
}

/// Unicode script category for text itemization.
///
/// Based on Unicode Standard Annex #24 (Script Property).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum Script {
    /// Common script (punctuation, numbers, symbols)
    Common,
    /// Inherited script (combining marks)
    Inherited,
    /// Unknown/unassigned script
    Unknown,
    /// Latin script (English, most European languages)
    #[default]
    Latin,
    /// Arabic script
    Arabic,
    /// Hebrew script
    Hebrew,
    /// Greek script
    Greek,
    /// Cyrillic script (Russian, etc.)
    Cyrillic,
    /// Devanagari script (Hindi, Sanskrit)
    Devanagari,
    /// Bengali script
    Bengali,
    /// Tamil script
    Tamil,
    /// Thai script
    Thai,
    /// Han script (Chinese, Japanese kanji, Korean hanja)
    Han,
    /// Hiragana script (Japanese)
    Hiragana,
    /// Katakana script (Japanese)
    Katakana,
    /// Hangul script (Korean)
    Hangul,
}

impl Script {
    /// Detects the script of a character.
    ///
    /// Uses Unicode character ranges for script detection.
    pub fn detect(c: char) -> Self {
        let cp = c as u32;

        // ASCII and Basic Latin
        if (0x0000..=0x007F).contains(&cp) {
            if c.is_ascii_alphabetic() {
                return Self::Latin;
            }
            return Self::Common;
        }

        // Latin Extended
        if (0x0080..=0x024F).contains(&cp) || (0x1E00..=0x1EFF).contains(&cp) {
            return Self::Latin;
        }

        // Greek
        if (0x0370..=0x03FF).contains(&cp) || (0x1F00..=0x1FFF).contains(&cp) {
            return Self::Greek;
        }

        // Cyrillic
        if (0x0400..=0x04FF).contains(&cp)
            || (0x0500..=0x052F).contains(&cp)
            || (0x2DE0..=0x2DFF).contains(&cp)
            || (0xA640..=0xA69F).contains(&cp)
        {
            return Self::Cyrillic;
        }

        // Hebrew
        if (0x0590..=0x05FF).contains(&cp) || (0xFB1D..=0xFB4F).contains(&cp) {
            return Self::Hebrew;
        }

        // Arabic
        if (0x0600..=0x06FF).contains(&cp)
            || (0x0750..=0x077F).contains(&cp)
            || (0x08A0..=0x08FF).contains(&cp)
            || (0xFB50..=0xFDFF).contains(&cp)
            || (0xFE70..=0xFEFF).contains(&cp)
        {
            return Self::Arabic;
        }

        // Devanagari
        if (0x0900..=0x097F).contains(&cp) || (0xA8E0..=0xA8FF).contains(&cp) {
            return Self::Devanagari;
        }

        // Bengali
        if (0x0980..=0x09FF).contains(&cp) {
            return Self::Bengali;
        }

        // Tamil
        if (0x0B80..=0x0BFF).contains(&cp) {
            return Self::Tamil;
        }

        // Thai
        if (0x0E00..=0x0E7F).contains(&cp) {
            return Self::Thai;
        }

        // Hangul (Korean)
        if (0x1100..=0x11FF).contains(&cp)
            || (0x3130..=0x318F).contains(&cp)
            || (0xA960..=0xA97F).contains(&cp)
            || (0xAC00..=0xD7AF).contains(&cp)
            || (0xD7B0..=0xD7FF).contains(&cp)
        {
            return Self::Hangul;
        }

        // Hiragana
        if (0x3040..=0x309F).contains(&cp) {
            return Self::Hiragana;
        }

        // Katakana
        if (0x30A0..=0x30FF).contains(&cp) || (0x31F0..=0x31FF).contains(&cp) {
            return Self::Katakana;
        }

        // CJK (Han)
        if (0x4E00..=0x9FFF).contains(&cp)
            || (0x3400..=0x4DBF).contains(&cp)
            || (0x20000..=0x2A6DF).contains(&cp)
            || (0x2A700..=0x2B73F).contains(&cp)
            || (0x2B740..=0x2B81F).contains(&cp)
            || (0x2B820..=0x2CEAF).contains(&cp)
            || (0x2CEB0..=0x2EBEF).contains(&cp)
            || (0x30000..=0x3134F).contains(&cp)
            || (0xF900..=0xFAFF).contains(&cp)
            || (0x2F800..=0x2FA1F).contains(&cp)
        {
            return Self::Han;
        }

        // Combining marks (Inherited)
        if (0x0300..=0x036F).contains(&cp) || (0x1AB0..=0x1AFF).contains(&cp) || (0x1DC0..=0x1DFF).contains(&cp) {
            return Self::Inherited;
        }

        // General punctuation, symbols, numbers
        if (0x2000..=0x206F).contains(&cp)
            || (0x2070..=0x209F).contains(&cp)
            || (0x20A0..=0x20CF).contains(&cp)
            || (0x2100..=0x214F).contains(&cp)
        {
            return Self::Common;
        }

        Self::Unknown
    }

    /// Returns true if this script can merge with any script.
    ///
    /// Common and Inherited scripts can merge with surrounding scripts.
    #[inline]
    pub fn is_neutral(self) -> bool {
        matches!(self, Self::Common | Self::Inherited | Self::Unknown)
    }

    /// Converts to rustybuzz Script using ISO 15924 tags.
    ///
    /// Returns None for scripts that should be auto-detected.
    pub fn to_harfbuzz(self) -> Option<rustybuzz::Script> {
        // Use ISO 15924 4-letter tags
        let tag: Option<[u8; 4]> = match self {
            Self::Latin => Some(*b"Latn"),
            Self::Arabic => Some(*b"Arab"),
            Self::Hebrew => Some(*b"Hebr"),
            Self::Greek => Some(*b"Grek"),
            Self::Cyrillic => Some(*b"Cyrl"),
            Self::Devanagari => Some(*b"Deva"),
            Self::Bengali => Some(*b"Beng"),
            Self::Tamil => Some(*b"Taml"),
            Self::Thai => Some(*b"Thai"),
            Self::Han => Some(*b"Hani"),
            Self::Hiragana => Some(*b"Hira"),
            Self::Katakana => Some(*b"Kana"),
            Self::Hangul => Some(*b"Hang"),
            Self::Common | Self::Inherited | Self::Unknown => None,
        };

        tag.and_then(|t| {
            let tag = rustybuzz::ttf_parser::Tag::from_bytes(&t);
            rustybuzz::Script::from_iso15924_tag(tag)
        })
    }
}

// ============================================================================
// Bidi Analysis
// ============================================================================

/// Result of bidirectional text analysis.
///
/// Contains the bidi levels for each character and metadata about
/// whether reordering is needed.
#[derive(Debug)]
pub struct BidiAnalysis {
    /// The original text being analyzed (reserved for future RTL improvements).
    #[allow(dead_code)]
    text: String,
    /// Bidi levels for each byte position (matching BidiInfo).
    levels: Vec<Level>,
    /// Base paragraph level.
    base_level: Level,
    /// Whether the text contains RTL content requiring reordering.
    needs_reordering: bool,
}

impl BidiAnalysis {
    /// Analyzes text for bidirectional properties.
    ///
    /// Uses the Unicode Bidirectional Algorithm (UAX #9) to determine
    /// text direction at each position.
    ///
    /// # Arguments
    ///
    /// * `text` - The text to analyze
    /// * `style` - ComputedStyle containing CSS direction property
    pub fn analyze(text: &str, style: &ComputedStyle) -> Self {
        // Determine base direction from CSS direction property (inherited, initial LTR)
        let base_level = match style.direction {
            CssDirection::Ltr => Level::ltr(),
            CssDirection::Rtl => Level::rtl(),
        };

        // Handle empty text
        if text.is_empty() {
            return Self {
                text: String::new(),
                levels: Vec::new(),
                base_level,
                needs_reordering: false,
            };
        }

        // Run Unicode bidi algorithm
        let bidi_info = BidiInfo::new(text, Some(base_level));

        // Check if any RTL content exists
        let needs_reordering = bidi_info.levels.iter().any(|&level| level.is_rtl());

        // Clone owned text for storage
        let text = text.to_string();
        let levels = bidi_info.levels.clone();

        Self {
            text,
            levels,
            base_level,
            needs_reordering,
        }
    }

    /// Returns true if reordering is needed for display.
    #[inline]
    pub fn needs_reordering(&self) -> bool {
        self.needs_reordering
    }

    /// Gets the bidi level at a byte index.
    #[inline]
    pub fn level_at(&self, index: usize) -> Level {
        self.levels.get(index).copied().unwrap_or(self.base_level)
    }

    /// Gets the direction at a byte index.
    #[inline]
    pub fn direction_at(&self, index: usize) -> Direction {
        Direction::from_level(self.level_at(index))
    }

    /// Returns the base paragraph level.
    #[inline]
    pub fn base_level(&self) -> Level {
        self.base_level
    }

    /// Returns the base direction.
    #[inline]
    pub fn base_direction(&self) -> Direction {
        Direction::from_level(self.base_level)
    }
}

// ============================================================================
// Script Itemization
// ============================================================================

/// A run of text with uniform properties.
///
/// After itemization, text is split into runs where each run has:
/// - Consistent script (Latin, Arabic, etc.)
/// - Consistent direction (LTR or RTL)
/// - Consistent bidi level
#[derive(Debug, Clone)]
pub struct ItemizedRun {
    /// Start byte index in original text.
    pub start: usize,
    /// End byte index in original text (exclusive).
    pub end: usize,
    /// The text content of this run.
    pub text: String,
    /// Script for this run.
    pub script: Script,
    /// Text direction for this run.
    pub direction: Direction,
    /// Bidi embedding level.
    pub level: u8,
}

impl ItemizedRun {
    /// Returns the length of this run in bytes.
    #[inline]
    pub fn len(&self) -> usize {
        self.end - self.start
    }

    /// Returns true if this run is empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.start >= self.end
    }
}

/// Itemizes text into runs of uniform script and direction.
pub fn itemize_text(text: &str, bidi: &BidiAnalysis) -> Vec<ItemizedRun> {
    if text.is_empty() {
        return Vec::new();
    }

    let mut runs = Vec::new();
    let mut current_start = 0;
    let mut current_text = String::new();
    let mut current_script: Option<Script> = None;
    let mut current_direction: Option<Direction> = None;
    let mut current_level: Option<u8> = None;

    for (idx, ch) in text.char_indices() {
        let char_script = Script::detect(ch);
        let char_direction = bidi.direction_at(idx);
        let char_level = bidi.level_at(idx).number();

        // Resolve neutral scripts based on context
        let resolved_script = if char_script.is_neutral() {
            current_script.unwrap_or(Script::Latin)
        } else {
            char_script
        };

        // Check if we need to start a new run
        let needs_new_run = match (current_script, current_direction, current_level) {
            (None, _, _) => false, // First character, no current run
            (Some(script), Some(dir), Some(level)) => {
                // New run if direction, level, or script changes
                dir != char_direction || level != char_level || (!char_script.is_neutral() && script != resolved_script)
            }
            _ => false,
        };

        if needs_new_run {
            // Finish current run
            if let (Some(script), Some(direction), Some(level)) = (current_script, current_direction, current_level) {
                runs.push(ItemizedRun {
                    start: current_start,
                    end: idx,
                    text: std::mem::take(&mut current_text),
                    script,
                    direction,
                    level,
                });
            }
            current_start = idx;
        }

        // Update current run properties
        if !char_script.is_neutral() {
            current_script = Some(resolved_script);
        } else if current_script.is_none() {
            current_script = Some(Script::Latin);
        }
        current_direction = Some(char_direction);
        current_level = Some(char_level);
        current_text.push(ch);
    }

    // Finish last run
    if !current_text.is_empty() {
        if let (Some(script), Some(direction), Some(level)) = (current_script, current_direction, current_level) {
            runs.push(ItemizedRun {
                start: current_start,
                end: text.len(),
                text: current_text,
                script,
                direction,
                level,
            });
        }
    }

    runs
}

// ============================================================================
// Font Matching
// ============================================================================

/// A run of text with an assigned font.
#[derive(Debug, Clone)]
pub struct FontRun {
    /// The text content.
    pub text: String,
    /// Start byte index in original text.
    pub start: usize,
    /// End byte index in original text.
    pub end: usize,
    /// The assigned font.
    pub font: Arc<LoadedFont>,
    /// Script for this run.
    pub script: Script,
    /// Text direction.
    pub direction: Direction,
    /// Bidi level.
    pub level: u8,
    /// Font size in pixels.
    pub font_size: f32,
}

/// Assigns fonts to itemized runs.
///
/// Uses the font context to find appropriate fonts for each script,
/// falling back through the font family list as needed.
pub fn assign_fonts(runs: &[ItemizedRun], style: &ComputedStyle, font_context: &FontContext) -> Result<Vec<FontRun>> {
    let mut font_runs = Vec::with_capacity(runs.len());

    for run in runs {
        // Try to get a font from the family list
        let font = get_font_for_run(run, style, font_context)?;

        font_runs.push(FontRun {
            text: run.text.clone(),
            start: run.start,
            end: run.end,
            font: Arc::new(font),
            script: run.script,
            direction: run.direction,
            level: run.level,
            font_size: style.font_size,
        });
    }

    Ok(font_runs)
}

/// Gets a font for a specific run.
fn get_font_for_run(run: &ItemizedRun, style: &ComputedStyle, font_context: &FontContext) -> Result<LoadedFont> {
    // Convert CSS font style to our font style
    let font_style = match style.font_style {
        CssFontStyle::Normal => FontStyle::Normal,
        CssFontStyle::Italic => FontStyle::Italic,
        CssFontStyle::Oblique => FontStyle::Oblique,
    };

    // Try the font family list first
    if let Some(font) = font_context.get_font(
        &style.font_family,
        style.font_weight.to_u16(),
        font_style == FontStyle::Italic,
        font_style == FontStyle::Oblique,
    ) {
        // Verify the font has glyphs for at least the first character
        if let Some(ch) = run.text.chars().next() {
            if let Ok(face) = font.as_ttf_face() {
                if face.glyph_index(ch).is_some() {
                    return Ok(font);
                }
            }
        }
        // If no verification possible, use it anyway
        return Ok(font);
    }

    // Try generic fallbacks based on script
    let generic_families = match run.script {
        Script::Arabic | Script::Hebrew => vec!["sans-serif".to_string()],
        Script::Han | Script::Hiragana | Script::Katakana => vec!["sans-serif".to_string()],
        Script::Devanagari | Script::Bengali | Script::Tamil | Script::Thai => vec!["sans-serif".to_string()],
        _ => vec!["sans-serif".to_string()],
    };

    if let Some(font) = font_context.get_font(
        &generic_families,
        style.font_weight.to_u16(),
        font_style == FontStyle::Italic,
        font_style == FontStyle::Oblique,
    ) {
        return Ok(font);
    }

    // Last resort: try to get any sans-serif font
    font_context.get_sans_serif().ok_or_else(|| {
        TextError::ShapingFailed {
            text: run.text.clone(),
            reason: "No suitable font found".to_string(),
        }
        .into()
    })
}

// ============================================================================
// Text Shaping
// ============================================================================

/// Information about a single glyph.
#[derive(Debug, Clone, Copy)]
pub struct GlyphPosition {
    /// Glyph ID in the font.
    pub glyph_id: u32,
    /// Cluster index (maps to character position in original text).
    pub cluster: u32,
    /// X position relative to run start.
    pub x_offset: f32,
    /// Y offset from baseline.
    pub y_offset: f32,
    /// Horizontal advance (distance to next glyph).
    pub x_advance: f32,
    /// Vertical advance (usually 0 for horizontal text).
    pub y_advance: f32,
}

/// A shaped run of text, ready for rendering.
#[derive(Debug, Clone)]
pub struct ShapedRun {
    /// The original text.
    pub text: String,
    /// Start byte index in original text.
    pub start: usize,
    /// End byte index in original text.
    pub end: usize,
    /// Positioned glyphs.
    pub glyphs: Vec<GlyphPosition>,
    /// Text direction.
    pub direction: Direction,
    /// Bidi level.
    pub level: u8,
    /// Total horizontal advance of this run.
    pub advance: f32,
    /// Font used for this run.
    pub font: Arc<LoadedFont>,
    /// Font size in pixels.
    pub font_size: f32,
}

impl ShapedRun {
    /// Returns true if this run is empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.glyphs.is_empty()
    }

    /// Returns the number of glyphs.
    #[inline]
    pub fn glyph_count(&self) -> usize {
        self.glyphs.len()
    }
}

/// Shapes a single font run into positioned glyphs.
fn shape_font_run(run: &FontRun) -> Result<ShapedRun> {
    // Create rustybuzz face from font data
    let rb_face = Face::from_slice(&run.font.data, run.font.index).ok_or_else(|| TextError::ShapingFailed {
        text: run.text.clone(),
        reason: "Failed to create HarfBuzz face".to_string(),
    })?;

    // Create Unicode buffer
    let mut buffer = UnicodeBuffer::new();
    buffer.push_str(&run.text);

    // Set buffer properties
    buffer.set_direction(run.direction.to_harfbuzz());
    if let Some(script) = run.script.to_harfbuzz() {
        buffer.set_script(script);
    }
    // TODO: Set language from style when available

    // Shape the text
    let output = rustybuzz::shape(&rb_face, &[], buffer);

    // Calculate scale factor
    let units_per_em = rb_face.units_per_em() as f32;
    let scale = run.font_size / units_per_em;

    // Extract glyph information
    let glyph_infos = output.glyph_infos();
    let glyph_positions = output.glyph_positions();

    let mut glyphs = Vec::with_capacity(glyph_infos.len());
    let mut x_position = 0.0_f32;

    for (info, pos) in glyph_infos.iter().zip(glyph_positions.iter()) {
        let x_offset = x_position + (pos.x_offset as f32 * scale);
        let y_offset = pos.y_offset as f32 * scale;
        let x_advance = pos.x_advance as f32 * scale;
        let y_advance = pos.y_advance as f32 * scale;

        glyphs.push(GlyphPosition {
            glyph_id: info.glyph_id,
            cluster: info.cluster,
            x_offset,
            y_offset,
            x_advance,
            y_advance,
        });

        x_position += x_advance;
    }

    Ok(ShapedRun {
        text: run.text.clone(),
        start: run.start,
        end: run.end,
        glyphs,
        direction: run.direction,
        level: run.level,
        advance: x_position,
        font: Arc::clone(&run.font),
        font_size: run.font_size,
    })
}

// ============================================================================
// Shaping Pipeline
// ============================================================================

/// The main text shaping pipeline.
///
/// Coordinates bidi analysis, script itemization, font matching, and text shaping
/// into a unified process.
///
/// # Example
///
/// ```rust,ignore
/// let pipeline = ShapingPipeline::new();
/// let font_context = FontContext::new();
/// let style = ComputedStyle::default();
///
/// let runs = pipeline.shape("Hello, world!", &style, &font_context)?;
/// ```
#[derive(Debug, Default, Clone)]
pub struct ShapingPipeline {
    // Reserved for future caching/configuration
    _private: (),
}

impl ShapingPipeline {
    /// Creates a new shaping pipeline.
    pub fn new() -> Self {
        Self { _private: () }
    }

    /// Shapes text into positioned glyphs.
    ///
    /// This is the main entry point for text shaping. It performs:
    /// 1. Bidi analysis
    /// 2. Script itemization
    /// 3. Font matching
    /// 4. Text shaping
    ///
    /// # Arguments
    ///
    /// * `text` - The text to shape
    /// * `style` - Computed style containing font properties
    /// * `font_context` - Font context for font resolution
    ///
    /// # Returns
    ///
    /// A vector of shaped runs, ready for rendering.
    ///
    /// # Errors
    ///
    /// Returns an error if font matching or shaping fails.
    pub fn shape(&self, text: &str, style: &ComputedStyle, font_context: &FontContext) -> Result<Vec<ShapedRun>> {
        // Handle empty text
        if text.is_empty() {
            return Ok(Vec::new());
        }

        // Step 1: Bidi analysis
        let bidi = BidiAnalysis::analyze(text, style);

        // Step 2: Script itemization
        let itemized_runs = itemize_text(text, &bidi);

        // Step 3: Font matching
        let font_runs = assign_fonts(&itemized_runs, style, font_context)?;

        // Step 4: Shape each run
        let mut shaped_runs = Vec::with_capacity(font_runs.len());
        for run in &font_runs {
            let shaped = shape_font_run(run)?;
            shaped_runs.push(shaped);
        }

        // Step 5: Reorder for bidi if needed
        if bidi.needs_reordering() {
            reorder_runs(&mut shaped_runs);
        }

        Ok(shaped_runs)
    }

    /// Shapes text with explicit direction override.
    ///
    /// Useful when the CSS direction property is known.
    pub fn shape_with_direction(
        &self,
        text: &str,
        style: &ComputedStyle,
        font_context: &FontContext,
        _base_direction: Direction,
    ) -> Result<Vec<ShapedRun>> {
        // For now, delegate to shape() - direction is determined by bidi analysis
        // TODO: Implement CSS direction property support
        self.shape(text, style, font_context)
    }

    /// Measures the total advance width of shaped text.
    ///
    /// Convenience method that shapes text and returns only the width.
    pub fn measure_width(&self, text: &str, style: &ComputedStyle, font_context: &FontContext) -> Result<f32> {
        let runs = self.shape(text, style, font_context)?;
        Ok(runs.iter().map(|r| r.advance).sum())
    }
}

/// Reorders shaped runs for bidi display.
///
/// Implements visual reordering based on bidi levels.
fn reorder_runs(runs: &mut [ShapedRun]) {
    if runs.is_empty() {
        return;
    }

    // Find the maximum level
    let max_level = runs.iter().map(|r| r.level).max().unwrap_or(0);

    // Reverse runs at each level, from highest to lowest
    for level in (1..=max_level).rev() {
        let mut start: Option<usize> = None;

        for i in 0..runs.len() {
            if runs[i].level >= level {
                if start.is_none() {
                    start = Some(i);
                }
            } else if let Some(s) = start {
                runs[s..i].reverse();
                start = None;
            }
        }

        // Handle end of runs
        if let Some(s) = start {
            runs[s..].reverse();
        }
    }
}

// ============================================================================
// Cluster Mapping
// ============================================================================

/// Maps between character positions and glyph positions.
///
/// Needed for hit testing, selection, and cursor positioning.
#[derive(Debug, Clone)]
pub struct ClusterMap {
    /// Maps character index to glyph index.
    char_to_glyph: Vec<usize>,
    /// Maps glyph index to character index.
    glyph_to_char: Vec<usize>,
}

impl ClusterMap {
    /// Builds a cluster map from a shaped run.
    pub fn from_shaped_run(run: &ShapedRun) -> Self {
        let char_count = run.text.chars().count();
        let glyph_count = run.glyphs.len();

        let mut char_to_glyph = vec![0; char_count];
        let mut glyph_to_char = vec![0; glyph_count];

        // Build glyph to char mapping from cluster info
        for (glyph_idx, glyph) in run.glyphs.iter().enumerate() {
            // Find the character index for this cluster
            let mut char_idx = 0;
            let mut byte_idx = 0;
            for ch in run.text.chars() {
                if byte_idx >= glyph.cluster as usize {
                    break;
                }
                byte_idx += ch.len_utf8();
                char_idx += 1;
            }
            glyph_to_char[glyph_idx] = char_idx.min(char_count.saturating_sub(1));
        }

        // Build char to glyph mapping
        for (glyph_idx, &char_idx) in glyph_to_char.iter().enumerate() {
            if char_idx < char_count {
                char_to_glyph[char_idx] = glyph_idx;
            }
        }

        Self {
            char_to_glyph,
            glyph_to_char,
        }
    }

    /// Gets the glyph index for a character index.
    pub fn glyph_for_char(&self, char_idx: usize) -> Option<usize> {
        self.char_to_glyph.get(char_idx).copied()
    }

    /// Gets the character index for a glyph index.
    pub fn char_for_glyph(&self, glyph_idx: usize) -> Option<usize> {
        self.glyph_to_char.get(glyph_idx).copied()
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_direction_from_level() {
        assert!(Direction::from_level(Level::ltr()).is_ltr());
        assert!(Direction::from_level(Level::rtl()).is_rtl());
    }

    #[test]
    fn test_direction_to_harfbuzz() {
        assert_eq!(Direction::LeftToRight.to_harfbuzz(), HbDirection::LeftToRight);
        assert_eq!(Direction::RightToLeft.to_harfbuzz(), HbDirection::RightToLeft);
    }

    #[test]
    fn test_script_detection_latin() {
        assert_eq!(Script::detect('A'), Script::Latin);
        assert_eq!(Script::detect('z'), Script::Latin);
        assert_eq!(Script::detect('é'), Script::Latin);
    }

    #[test]
    fn test_script_detection_arabic() {
        assert_eq!(Script::detect('م'), Script::Arabic);
        assert_eq!(Script::detect('ر'), Script::Arabic);
    }

    #[test]
    fn test_script_detection_hebrew() {
        assert_eq!(Script::detect('ש'), Script::Hebrew);
        assert_eq!(Script::detect('ל'), Script::Hebrew);
    }

    #[test]
    fn test_script_detection_greek() {
        assert_eq!(Script::detect('α'), Script::Greek);
        assert_eq!(Script::detect('Ω'), Script::Greek);
    }

    #[test]
    fn test_script_detection_cyrillic() {
        assert_eq!(Script::detect('А'), Script::Cyrillic);
        assert_eq!(Script::detect('я'), Script::Cyrillic);
    }

    #[test]
    fn test_script_detection_cjk() {
        assert_eq!(Script::detect('中'), Script::Han);
        assert_eq!(Script::detect('あ'), Script::Hiragana);
        assert_eq!(Script::detect('カ'), Script::Katakana);
        assert_eq!(Script::detect('한'), Script::Hangul);
    }

    #[test]
    fn test_script_detection_common() {
        assert_eq!(Script::detect(' '), Script::Common);
        assert_eq!(Script::detect('1'), Script::Common);
        assert_eq!(Script::detect('.'), Script::Common);
    }

    #[test]
    fn test_bidi_analysis_ltr() {
        let style = ComputedStyle::default();
        let bidi = BidiAnalysis::analyze("Hello", &style);

        assert!(!bidi.needs_reordering());
        assert!(bidi.base_direction().is_ltr());
    }

    #[test]
    fn test_bidi_analysis_rtl() {
        let style = ComputedStyle::default();
        let bidi = BidiAnalysis::analyze("שלום", &style);

        assert!(bidi.needs_reordering());
    }

    #[test]
    fn test_bidi_analysis_mixed() {
        let style = ComputedStyle::default();
        let bidi = BidiAnalysis::analyze("Hello שלום World", &style);

        assert!(bidi.needs_reordering());
    }

    #[test]
    fn test_bidi_analysis_empty() {
        let style = ComputedStyle::default();
        let bidi = BidiAnalysis::analyze("", &style);

        assert!(!bidi.needs_reordering());
    }

    #[test]
    fn test_itemize_single_script() {
        let style = ComputedStyle::default();
        let bidi = BidiAnalysis::analyze("Hello", &style);
        let runs = itemize_text("Hello", &bidi);

        assert_eq!(runs.len(), 1);
        assert_eq!(runs[0].text, "Hello");
        assert_eq!(runs[0].script, Script::Latin);
    }

    #[test]
    fn test_itemize_mixed_scripts() {
        let style = ComputedStyle::default();
        let text = "Hello שלום";
        let bidi = BidiAnalysis::analyze(text, &style);
        let runs = itemize_text(text, &bidi);

        // Should have at least 2 runs (Latin and Hebrew)
        assert!(runs.len() >= 2);
    }

    #[test]
    fn test_itemize_empty() {
        let style = ComputedStyle::default();
        let bidi = BidiAnalysis::analyze("", &style);
        let runs = itemize_text("", &bidi);

        assert!(runs.is_empty());
    }

    #[test]
    fn test_script_is_neutral() {
        assert!(Script::Common.is_neutral());
        assert!(Script::Inherited.is_neutral());
        assert!(Script::Unknown.is_neutral());
        assert!(!Script::Latin.is_neutral());
        assert!(!Script::Arabic.is_neutral());
    }

    #[test]
    fn test_pipeline_new() {
        let pipeline = ShapingPipeline::new();
        // Should not panic
        let _ = pipeline;
    }

    #[test]
    fn test_itemized_run_len() {
        let run = ItemizedRun {
            start: 0,
            end: 5,
            text: "Hello".to_string(),
            script: Script::Latin,
            direction: Direction::LeftToRight,
            level: 0,
        };

        assert_eq!(run.len(), 5);
        assert!(!run.is_empty());
    }

    #[test]
    fn test_itemized_run_empty() {
        let run = ItemizedRun {
            start: 5,
            end: 5,
            text: String::new(),
            script: Script::Latin,
            direction: Direction::LeftToRight,
            level: 0,
        };

        assert_eq!(run.len(), 0);
        assert!(run.is_empty());
    }

    #[test]
    fn test_script_to_harfbuzz() {
        // Specific scripts should return Some
        assert!(Script::Latin.to_harfbuzz().is_some());
        assert!(Script::Arabic.to_harfbuzz().is_some());
        assert!(Script::Hebrew.to_harfbuzz().is_some());
        // Common/neutral scripts should return None (auto-detect)
        assert!(Script::Common.to_harfbuzz().is_none());
        assert!(Script::Inherited.to_harfbuzz().is_none());
    }

    #[test]
    fn test_reorder_runs_empty() {
        let mut runs: Vec<ShapedRun> = Vec::new();
        reorder_runs(&mut runs);
        assert!(runs.is_empty());
    }
}
