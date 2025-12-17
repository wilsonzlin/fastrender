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
use crate::style::types::{
    Direction as CssDirection, EastAsianVariant, EastAsianWidth, FontKerning, FontLanguageOverride, FontSizeAdjust,
    FontStyle as CssFontStyle, FontVariant, FontVariantCaps, FontVariantEmoji, FontVariantPosition, NumericFigure,
    NumericFraction, NumericSpacing,
};
use crate::style::ComputedStyle;
use crate::text::emoji;
use crate::text::font_db::{FontStretch as DbFontStretch, FontStyle, LoadedFont};
use crate::text::font_loader::FontContext;
use rustybuzz::{Direction as HbDirection, Face, Feature, Language as HbLanguage, UnicodeBuffer, Variation};
use std::collections::HashSet;
use std::iter::FromIterator;
use std::str::FromStr;
use std::sync::Arc;
use ttf_parser::Face as ParserFace;
use ttf_parser::Tag;
use unicode_bidi::{BidiInfo, Level};
use unicode_vo::{char_orientation, Orientation as VerticalOrientation};

pub(crate) const DEFAULT_OBLIQUE_ANGLE_DEG: f32 = 14.0;

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

fn has_native_small_caps(style: &ComputedStyle, font_context: &FontContext) -> bool {
    let needs_c2sc = matches!(style.font_variant_caps, FontVariantCaps::AllSmallCaps);
    if let Some(font) = font_context.get_font_full(
        &style.font_family,
        style.font_weight.to_u16(),
        match style.font_style {
            CssFontStyle::Normal => FontStyle::Normal,
            CssFontStyle::Italic => FontStyle::Italic,
            CssFontStyle::Oblique(_) => FontStyle::Oblique,
        },
        DbFontStretch::from_percentage(style.font_stretch.to_percentage()),
    ) {
        let has_smcp = font_context.supports_feature(&font, *b"smcp");
        let has_c2sc = !needs_c2sc || font_context.supports_feature(&font, *b"c2sc");
        return has_smcp && has_c2sc;
    }

    false
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

/// Explicit bidi context passed in by layout when `unicode-bidi` establishes
/// embedding/override/isolation scopes that should affect the entire text run
/// without injecting control characters.
#[derive(Clone, Copy, Debug)]
pub struct ExplicitBidiContext {
    pub level: Level,
    pub override_all: bool,
}

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
    /// Paragraph boundaries in byte offsets with their base level.
    paragraphs: Vec<ParagraphBoundary>,
    /// Byte offsets for each character in the source text.
    char_starts: Vec<usize>,
    /// Base paragraph level.
    base_level: Level,
    /// Whether the text contains RTL content requiring reordering.
    needs_reordering: bool,
}

/// Paragraph boundaries derived from bidi analysis.
#[derive(Debug, Clone, Copy)]
pub struct ParagraphBoundary {
    pub start_byte: usize,
    pub end_byte: usize,
    pub level: Level,
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
        let base_direction = match style.direction {
            CssDirection::Ltr => Direction::LeftToRight,
            CssDirection::Rtl => Direction::RightToLeft,
        };
        Self::analyze_with_base(text, style, base_direction, None)
    }

    /// Analyzes text for bidirectional properties with an explicit base direction.
    ///
    /// This mirrors CSS paragraph base resolution while allowing callers (e.g. layout)
    /// to supply the containing block's resolved direction when it differs from the
    /// style value.
    pub fn analyze_with_base(
        text: &str,
        style: &ComputedStyle,
        base_direction: Direction,
        explicit: Option<ExplicitBidiContext>,
    ) -> Self {
        // Determine base direction from CSS direction property (inherited, initial LTR)
        let mut base_level = match base_direction {
            Direction::LeftToRight => Level::ltr(),
            Direction::RightToLeft => Level::rtl(),
        };
        let mut override_all = false;
        if let Some(ctx) = explicit {
            base_level = ctx.level;
            override_all = ctx.override_all;
        }

        // Handle empty text
        if text.is_empty() {
            return Self {
                text: String::new(),
                levels: Vec::new(),
                paragraphs: Vec::new(),
                char_starts: Vec::new(),
                base_level,
                needs_reordering: false,
            };
        }

        // Run Unicode bidi algorithm
        let base_override = match style.unicode_bidi {
            crate::style::types::UnicodeBidi::Plaintext => None,
            _ => Some(base_level),
        };
        let bidi_info = BidiInfo::new(text, base_override);

        // Check if any RTL content exists
        let mut needs_reordering = bidi_info.levels.iter().any(|&level| level.is_rtl());

        // Clone owned text for storage
        let text = text.to_string();
        let mut levels = bidi_info.levels.clone();
        let char_starts: Vec<usize> = text.char_indices().map(|(idx, _)| idx).collect();
        let para_level = bidi_info.paragraphs.first().map(|p| p.level).unwrap_or(base_level);
        let base_level = match style.unicode_bidi {
            crate::style::types::UnicodeBidi::Plaintext => para_level,
            _ => base_level,
        };

        // CSS bidi overrides force all characters to the element's direction.
        use crate::style::types::UnicodeBidi;
        if override_all
            || matches!(
                style.unicode_bidi,
                UnicodeBidi::BidiOverride | UnicodeBidi::IsolateOverride
            )
        {
            levels = vec![base_level; levels.len()];
            needs_reordering = false;
        }

        let text_len = text.len();
        let paragraphs = bidi_info
            .paragraphs
            .iter()
            .map(|p| {
                let start_byte = *char_starts.get(p.range.start).unwrap_or(&text_len);
                let end_byte = if p.range.end < char_starts.len() {
                    char_starts[p.range.end]
                } else {
                    text_len
                };
                ParagraphBoundary {
                    start_byte,
                    end_byte,
                    level: p.level,
                }
            })
            .collect();

        Self {
            text,
            levels,
            paragraphs,
            char_starts,
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
        if self.levels.is_empty() {
            return self.base_level;
        }
        let pos = match self.char_starts.binary_search(&index) {
            Ok(idx) => idx,
            Err(idx) => idx.saturating_sub(1),
        };
        self.levels.get(pos).copied().unwrap_or(self.base_level)
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

    /// Paragraph boundaries detected during bidi analysis.
    #[inline]
    pub fn paragraphs(&self) -> &[ParagraphBoundary] {
        &self.paragraphs
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

fn split_itemized_runs_by_paragraph(runs: Vec<ItemizedRun>, paragraphs: &[ParagraphBoundary]) -> Vec<ItemizedRun> {
    if runs.is_empty() || paragraphs.is_empty() {
        return runs;
    }

    let mut out = Vec::with_capacity(runs.len());
    let mut para_iter = paragraphs.iter().peekable();

    for mut run in runs {
        while let Some(para) = para_iter.peek().copied() {
            // Advance paragraph iterator if run starts after the current paragraph.
            if run.start >= para.end_byte {
                para_iter.next();
                continue;
            }

            let para_end = para.end_byte;
            if run.end <= para_end {
                out.push(run);
                break;
            }

            // Split the run at the paragraph boundary.
            let split_point = para_end;
            let split_offset = split_point - run.start;
            let (left, right) = split_run_at(&run, split_offset);
            out.push(left);
            run = right;
            para_iter.next();
        }
    }

    out
}

fn split_run_at(run: &ItemizedRun, split_offset: usize) -> (ItemizedRun, ItemizedRun) {
    let left_text = run.text[..split_offset].to_string();
    let right_text = run.text[split_offset..].to_string();
    let left = ItemizedRun {
        start: run.start,
        end: run.start + split_offset,
        text: left_text,
        script: run.script,
        direction: run.direction,
        level: run.level,
    };
    let right = ItemizedRun {
        start: run.start + split_offset,
        end: run.end,
        text: right_text,
        script: run.script,
        direction: run.direction,
        level: run.level,
    };
    (left, right)
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
    /// Synthetic bold stroke width in pixels (0 = none).
    pub synthetic_bold: f32,
    /// Synthetic oblique shear factor (tan(angle); 0 = none).
    pub synthetic_oblique: f32,
    /// Script for this run.
    pub script: Script,
    /// Text direction.
    pub direction: Direction,
    /// Bidi level.
    pub level: u8,
    /// Font size in pixels.
    pub font_size: f32,
    /// Additional baseline shift in pixels (positive raises text).
    pub baseline_shift: f32,
    /// BCP47 language tag (lowercased).
    pub language: String,
    /// OpenType features to apply for this run.
    pub features: Vec<Feature>,
    /// Font variation settings to apply for this run.
    pub variations: Vec<Variation>,

    /// Optional rotation hint for vertical writing modes.
    pub rotation: RunRotation,

    /// Whether this run should participate in vertical shaping (inline progression vertical).
    pub vertical: bool,
}

/// Collects OpenType features from computed style.
fn collect_opentype_features(style: &ComputedStyle) -> Vec<Feature> {
    let mut features = Vec::new();
    let lig = style.font_variant_ligatures;
    let numeric = &style.font_variant_numeric;
    let east = &style.font_variant_east_asian;
    let position = style.font_variant_position;
    let caps = style.font_variant_caps;
    let alternates = &style.font_variant_alternates;

    let push_toggle = |features: &mut Vec<Feature>, tag: [u8; 4], enabled: bool| {
        features.push(Feature {
            tag: Tag::from_bytes(&tag),
            value: if enabled { 1 } else { 0 },
            start: 0,
            end: u32::MAX,
        });
    };

    // font-variant-ligatures keywords map to OpenType features
    push_toggle(&mut features, *b"liga", lig.common);
    push_toggle(&mut features, *b"clig", lig.common);
    push_toggle(&mut features, *b"dlig", lig.discretionary);
    push_toggle(&mut features, *b"hlig", lig.historical);
    push_toggle(&mut features, *b"calt", lig.contextual);

    // font-variant-numeric mappings
    match numeric.figure {
        NumericFigure::Lining => push_toggle(&mut features, *b"lnum", true),
        NumericFigure::Oldstyle => push_toggle(&mut features, *b"onum", true),
        NumericFigure::Normal => {}
    }
    match numeric.spacing {
        NumericSpacing::Proportional => push_toggle(&mut features, *b"pnum", true),
        NumericSpacing::Tabular => push_toggle(&mut features, *b"tnum", true),
        NumericSpacing::Normal => {}
    }
    match numeric.fraction {
        NumericFraction::Diagonal => push_toggle(&mut features, *b"frac", true),
        NumericFraction::Stacked => push_toggle(&mut features, *b"afrc", true),
        NumericFraction::Normal => {}
    }
    if numeric.ordinal {
        push_toggle(&mut features, *b"ordn", true);
    }
    if numeric.slashed_zero {
        push_toggle(&mut features, *b"zero", true);
    }

    // font-variant-east-asian
    if let Some(variant) = east.variant {
        match variant {
            EastAsianVariant::Jis78 => push_toggle(&mut features, *b"jp78", true),
            EastAsianVariant::Jis83 => push_toggle(&mut features, *b"jp83", true),
            EastAsianVariant::Jis90 => push_toggle(&mut features, *b"jp90", true),
            EastAsianVariant::Jis04 => push_toggle(&mut features, *b"jp04", true),
            EastAsianVariant::Simplified => push_toggle(&mut features, *b"smpl", true),
            EastAsianVariant::Traditional => push_toggle(&mut features, *b"trad", true),
        }
    }
    if let Some(width) = east.width {
        match width {
            EastAsianWidth::FullWidth => push_toggle(&mut features, *b"fwid", true),
            EastAsianWidth::ProportionalWidth => push_toggle(&mut features, *b"pwid", true),
        }
    }
    if east.ruby {
        push_toggle(&mut features, *b"ruby", true);
    }

    match caps {
        FontVariantCaps::Normal => {}
        FontVariantCaps::SmallCaps => push_toggle(&mut features, *b"smcp", true),
        FontVariantCaps::AllSmallCaps => {
            push_toggle(&mut features, *b"smcp", true);
            push_toggle(&mut features, *b"c2sc", true);
        }
        FontVariantCaps::PetiteCaps => push_toggle(&mut features, *b"pcap", true),
        FontVariantCaps::AllPetiteCaps => {
            push_toggle(&mut features, *b"pcap", true);
            push_toggle(&mut features, *b"c2pc", true);
        }
        FontVariantCaps::Unicase => push_toggle(&mut features, *b"unic", true),
        FontVariantCaps::TitlingCaps => push_toggle(&mut features, *b"titl", true),
    }

    match position {
        FontVariantPosition::Normal => {}
        FontVariantPosition::Sub => push_toggle(&mut features, *b"subs", true),
        FontVariantPosition::Super => push_toggle(&mut features, *b"sups", true),
    }

    if alternates.historical_forms {
        push_toggle(&mut features, *b"hist", true);
    }
    if let Some(set) = alternates.stylistic {
        if let Some(tag) = number_tag(b"ss", set) {
            push_toggle(&mut features, tag, true);
        }
    }
    for set in &alternates.stylesets {
        if let Some(tag) = number_tag(b"ss", *set) {
            push_toggle(&mut features, tag, true);
        }
    }
    for cv in &alternates.character_variants {
        if let Some(tag) = number_tag(b"cv", *cv) {
            let tag = Tag::from_bytes(&tag);
            features.push(Feature {
                tag,
                value: 1,
                start: 0,
                end: u32::MAX,
            });
        }
    }
    if let Some(swash) = alternates.swash {
        // CSS doesn't distinguish swash types; map to swsh.
        let tag = Tag::from_bytes(b"swsh");
        features.retain(|f| f.tag != tag);
        features.push(Feature {
            tag,
            value: swash as u32,
            start: 0,
            end: u32::MAX,
        });
    }
    if let Some(orn) = alternates.ornaments {
        let tag = Tag::from_bytes(b"ornm");
        features.retain(|f| f.tag != tag);
        features.push(Feature {
            tag,
            value: orn as u32,
            start: 0,
            end: u32::MAX,
        });
    }

    match style.font_kerning {
        FontKerning::Auto => {}
        FontKerning::Normal => push_toggle(&mut features, *b"kern", true),
        FontKerning::None => push_toggle(&mut features, *b"kern", false),
    }

    // Low-level font-feature-settings override defaults and prior toggles.
    for setting in &style.font_feature_settings {
        let tag = Tag::from_bytes(&setting.tag);
        features.retain(|f| f.tag != tag);
        features.push(Feature {
            tag,
            value: setting.value,
            start: 0,
            end: u32::MAX,
        });
    }

    features
}

fn font_aspect_ratio(font: &LoadedFont) -> Option<f32> {
    font.metrics().ok().and_then(|m| m.aspect_ratio())
}

/// Returns the author-preferred aspect ratio for font-size-adjust, if any.
pub fn preferred_font_aspect(style: &ComputedStyle, font_context: &FontContext) -> Option<f32> {
    match style.font_size_adjust {
        FontSizeAdjust::None => None,
        FontSizeAdjust::Number(n) if n > 0.0 => Some(n),
        FontSizeAdjust::Number(_) => None,
        FontSizeAdjust::FromFont => {
            let font_style = match style.font_style {
                CssFontStyle::Normal => FontStyle::Normal,
                CssFontStyle::Italic => FontStyle::Italic,
                CssFontStyle::Oblique(_) => FontStyle::Oblique,
            };
            let font_stretch = DbFontStretch::from_percentage(style.font_stretch.to_percentage());
            font_context
                .get_font_full(&style.font_family, style.font_weight.to_u16(), font_style, font_stretch)
                .and_then(|font| font_aspect_ratio(&font))
        }
    }
}

/// Computes the used font size after applying font-size-adjust.
pub fn compute_adjusted_font_size(style: &ComputedStyle, font: &LoadedFont, preferred_aspect: Option<f32>) -> f32 {
    let base_size = style.font_size;
    let desired_aspect = match style.font_size_adjust {
        FontSizeAdjust::None => None,
        FontSizeAdjust::Number(n) if n > 0.0 => Some(n),
        FontSizeAdjust::Number(_) => None,
        FontSizeAdjust::FromFont => preferred_aspect.or_else(|| font_aspect_ratio(font)),
    };

    if let Some(desired) = desired_aspect {
        let actual = font_aspect_ratio(font).unwrap_or(0.5);
        if actual > 0.0 {
            return base_size * (desired / actual);
        }
    }

    base_size
}

/// Assigns fonts to itemized runs.
///
/// Uses the font context to find appropriate fonts for each script,
/// falling back through the font family list as needed.
pub fn assign_fonts(runs: &[ItemizedRun], style: &ComputedStyle, font_context: &FontContext) -> Result<Vec<FontRun>> {
    let features = collect_opentype_features(style);
    let authored_variations: Vec<Variation> = style
        .font_variation_settings
        .iter()
        .map(|v| Variation {
            tag: Tag::from_bytes(&v.tag),
            value: v.value,
        })
        .collect();
    let preferred_aspect = preferred_font_aspect(style, font_context);
    let (font_style, requested_oblique) = match style.font_style {
        CssFontStyle::Normal => (FontStyle::Normal, None),
        CssFontStyle::Italic => (FontStyle::Italic, None),
        CssFontStyle::Oblique(angle) => (FontStyle::Oblique, Some(angle.unwrap_or(DEFAULT_OBLIQUE_ANGLE_DEG))),
    };
    let font_stretch = DbFontStretch::from_percentage(style.font_stretch.to_percentage());
    let families = build_family_entries(style);

    let mut font_runs = Vec::new();
    for run in runs {
        let mut current: Option<(Arc<LoadedFont>, f32, f32, f32, f32, usize)> = None; // (font, bold, oblique, size, baseline_shift, start)
        let mut iter = run.text.char_indices().peekable();

        while let Some((byte_idx, ch)) = iter.next() {
            let next_peek = iter.peek().cloned();
            let next_idx = next_peek.map(|(i, _)| i).unwrap_or_else(|| run.text.len());
            if (emoji::is_variation_selector(ch) || emoji::is_zwj(ch)) && current.is_some() {
                if iter.peek().is_none() {
                    if let Some((font, bold, oblique, size, shift, start)) = current.take() {
                        push_font_run(
                            &mut font_runs,
                            run,
                            start,
                            next_idx,
                            font,
                            bold,
                            oblique,
                            size,
                            shift,
                            &features,
                            &authored_variations,
                            style,
                        );
                    }
                }
                continue;
            }
            let emoji_pref = emoji_preference_with_selector(ch, next_peek.map(|(_, c)| c), style.font_variant_emoji);
            let mut picker = FontPreferencePicker::new(emoji_pref);
            let font = resolve_font_for_char(
                ch,
                &families,
                style.font_weight.to_u16(),
                font_style,
                requested_oblique,
                font_stretch,
                font_context,
                &mut picker,
            )
            .ok_or_else(|| TextError::ShapingFailed {
                text: run.text.clone(),
                reason: format!("No suitable font found for character U+{:04X}", ch as u32),
            })?;
            let used_font_size = compute_adjusted_font_size(style, &font, preferred_aspect);
            let (mut synthetic_bold, synthetic_oblique) = compute_synthetic_styles(style, &font);
            if style.font_size > 0.0 {
                synthetic_bold *= used_font_size / style.font_size;
            }
            let font_arc = Arc::new(font);
            let (position_scale, baseline_shift) =
                synthetic_position_adjustment(style, &font_arc, used_font_size, font_context);
            let run_font_size = used_font_size * position_scale;

            let same_as_current = current.as_ref().map_or(false, |(f, b, o, size, shift, _)| {
                Arc::ptr_eq(&f.data, &font_arc.data)
                    && f.index == font_arc.index
                    && f.weight == font_arc.weight
                    && f.style == font_arc.style
                    && f.stretch == font_arc.stretch
                    && (*b - synthetic_bold).abs() < f32::EPSILON
                    && (*o - synthetic_oblique).abs() < f32::EPSILON
                    && (*size - run_font_size).abs() < f32::EPSILON
                    && (*shift - baseline_shift).abs() < f32::EPSILON
            });

            if !same_as_current {
                if let Some((font, bold, oblique, size, shift, start)) = current.take() {
                    push_font_run(
                        &mut font_runs,
                        run,
                        start,
                        byte_idx,
                        font,
                        bold,
                        oblique,
                        size,
                        shift,
                        &features,
                        &authored_variations,
                        style,
                    );
                }
                current = Some((
                    font_arc,
                    synthetic_bold,
                    synthetic_oblique,
                    run_font_size,
                    baseline_shift,
                    byte_idx,
                ));
            }

            if iter.peek().is_none() {
                if let Some((font, bold, oblique, size, shift, start)) = current.take() {
                    push_font_run(
                        &mut font_runs,
                        run,
                        start,
                        next_idx,
                        font,
                        bold,
                        oblique,
                        size,
                        shift,
                        &features,
                        &authored_variations,
                        style,
                    );
                }
            }
        }
    }

    Ok(font_runs)
}

fn is_vertical_typographic_mode(mode: crate::style::types::WritingMode) -> bool {
    matches!(
        mode,
        crate::style::types::WritingMode::VerticalRl | crate::style::types::WritingMode::VerticalLr
    )
}

fn is_sideways_writing_mode(mode: crate::style::types::WritingMode) -> bool {
    matches!(
        mode,
        crate::style::types::WritingMode::SidewaysRl | crate::style::types::WritingMode::SidewaysLr
    )
}

fn apply_vertical_text_orientation(
    runs: Vec<FontRun>,
    orientation: crate::style::types::TextOrientation,
) -> Vec<FontRun> {
    use crate::style::types::TextOrientation;

    match orientation {
        TextOrientation::Sideways | TextOrientation::SidewaysRight => runs
            .into_iter()
            .map(|mut run| {
                run.rotation = RunRotation::Cw90;
                run
            })
            .collect(),
        TextOrientation::Upright => runs
            .into_iter()
            .map(|mut run| {
                run.rotation = RunRotation::None;
                run.vertical = true;
                run
            })
            .collect(),
        TextOrientation::Mixed => runs.into_iter().flat_map(split_run_by_vertical_orientation).collect(),
    }
}

fn apply_sideways_text_orientation(runs: Vec<FontRun>) -> Vec<FontRun> {
    runs.into_iter()
        .map(|mut run| {
            run.rotation = RunRotation::Cw90;
            run
        })
        .collect()
}

fn split_run_by_vertical_orientation(run: FontRun) -> Vec<FontRun> {
    if run.text.is_empty() {
        return Vec::new();
    }

    let mut segments: Vec<FontRun> = Vec::new();
    let mut iter = run.text.char_indices();
    let (first_idx, first_char) = match iter.next() {
        Some(pair) => pair,
        None => return segments,
    };
    let mut current_rotation = rotation_for_mixed_char(first_char);
    let mut current_start = first_idx;

    for (idx, ch) in iter {
        let rotation = rotation_for_mixed_char(ch);
        if rotation != current_rotation {
            push_oriented_segment(&run, current_start, idx, current_rotation, &mut segments);
            current_start = idx;
            current_rotation = rotation;
        }
    }

    push_oriented_segment(&run, current_start, run.text.len(), current_rotation, &mut segments);
    segments
}

fn rotation_for_mixed_char(ch: char) -> RunRotation {
    match char_orientation(ch) {
        VerticalOrientation::Upright | VerticalOrientation::TransformedOrUpright => RunRotation::None,
        VerticalOrientation::Rotated | VerticalOrientation::TransformedOrRotated => RunRotation::Cw90,
    }
}

fn push_oriented_segment(run: &FontRun, start: usize, end: usize, rotation: RunRotation, out: &mut Vec<FontRun>) {
    if start >= end {
        return;
    }

    let mut segment = run.clone();
    segment.text = run.text[start..end].to_string();
    segment.start = run.start + start;
    segment.end = run.start + end;
    segment.rotation = rotation;
    segment.vertical = matches!(rotation, RunRotation::None);
    out.push(segment);
}

fn requested_slant_angle(style: &ComputedStyle) -> Option<f32> {
    match style.font_style {
        CssFontStyle::Normal => None,
        CssFontStyle::Italic => Some(DEFAULT_OBLIQUE_ANGLE_DEG),
        CssFontStyle::Oblique(angle) => Some(angle.unwrap_or(DEFAULT_OBLIQUE_ANGLE_DEG)),
    }
}

fn compute_synthetic_styles(style: &ComputedStyle, font: &LoadedFont) -> (f32, f32) {
    let mut synthetic_bold = 0.0;
    let mut synthetic_oblique = 0.0;

    if style.font_synthesis.weight && style.font_weight.to_u16() > font.weight.value() && !font_has_axis(font, *b"wght")
    {
        let delta = (style.font_weight.to_u16() as f32 - font.weight.value() as f32).max(0.0);
        let strength = (delta / 400.0).clamp(0.0, 1.0);
        synthetic_bold = style.font_size * 0.04 * strength;
    }

    if style.font_synthesis.style && matches!(font.style, FontStyle::Normal) {
        let (has_slnt_axis, has_ital_axis) = if let Ok(face) = font.as_ttf_face() {
            let mut has_slnt = false;
            let mut has_ital = false;
            for axis in face.variation_axes() {
                if axis.tag == Tag::from_bytes(b"slnt") {
                    has_slnt = true;
                } else if axis.tag == Tag::from_bytes(b"ital") {
                    has_ital = true;
                }
            }
            (has_slnt, has_ital)
        } else {
            (false, false)
        };

        let variation_covers_slant = match style.font_style {
            CssFontStyle::Oblique(_) => has_slnt_axis || has_ital_axis,
            CssFontStyle::Italic => has_ital_axis || has_slnt_axis,
            CssFontStyle::Normal => false,
        };

        if !variation_covers_slant {
            if let Some(angle) = requested_slant_angle(style) {
                synthetic_oblique = angle.to_radians().tan();
            }
        }
    }

    (synthetic_bold, synthetic_oblique)
}

fn font_has_axis(font: &LoadedFont, tag: [u8; 4]) -> bool {
    if let Ok(face) = font.as_ttf_face() {
        let target = Tag::from_bytes(&tag);
        return face.variation_axes().into_iter().any(|axis| axis.tag == target);
    }
    false
}

fn synthetic_position_adjustment(
    style: &ComputedStyle,
    font: &LoadedFont,
    base_font_size: f32,
    font_context: &FontContext,
) -> (f32, f32) {
    if !style.font_synthesis.position {
        return (1.0, 0.0);
    }

    const SYNTHETIC_SCALE: f32 = 0.8;
    const SUPER_SHIFT: f32 = 0.34;
    const SUB_SHIFT: f32 = -0.2;

    match style.font_variant_position {
        crate::style::types::FontVariantPosition::Normal => (1.0, 0.0),
        crate::style::types::FontVariantPosition::Super => {
            if font_context.supports_feature(font, *b"sups") {
                (1.0, 0.0)
            } else if let Some((scale, shift)) =
                os2_position_metrics(font, base_font_size, crate::style::types::FontVariantPosition::Super)
            {
                (scale, shift)
            } else {
                (SYNTHETIC_SCALE, base_font_size * SUPER_SHIFT)
            }
        }
        crate::style::types::FontVariantPosition::Sub => {
            if font_context.supports_feature(font, *b"subs") {
                (1.0, 0.0)
            } else if let Some((scale, shift)) =
                os2_position_metrics(font, base_font_size, crate::style::types::FontVariantPosition::Sub)
            {
                (scale, shift)
            } else {
                (SYNTHETIC_SCALE, base_font_size * SUB_SHIFT)
            }
        }
    }
}

fn os2_position_metrics(
    font: &LoadedFont,
    base_font_size: f32,
    position: crate::style::types::FontVariantPosition,
) -> Option<(f32, f32)> {
    let face = ParserFace::parse(&font.data, font.index).ok()?;
    let os2 = face.tables().os2?;
    let units = face.units_per_em() as f32;

    match position {
        crate::style::types::FontVariantPosition::Super => {
            let metrics = os2.superscript_metrics();
            if metrics.y_size > 0 {
                let scale = (metrics.y_size as f32 / units).clamp(0.3, 1.2);
                let shift = metrics.y_offset as f32 * (base_font_size / units);
                Some((scale, shift))
            } else {
                None
            }
        }
        crate::style::types::FontVariantPosition::Sub => {
            let metrics = os2.subscript_metrics();
            if metrics.y_size > 0 {
                let scale = (metrics.y_size as f32 / units).clamp(0.3, 1.2);
                let shift = -(metrics.y_offset as f32 * (base_font_size / units));
                Some((scale, shift))
            } else {
                None
            }
        }
        crate::style::types::FontVariantPosition::Normal => None,
    }
}

pub(crate) fn slope_preference_order(style: FontStyle) -> &'static [FontStyle] {
    match style {
        FontStyle::Normal => &[FontStyle::Normal],
        FontStyle::Italic => &[FontStyle::Italic, FontStyle::Oblique, FontStyle::Normal],
        FontStyle::Oblique => &[FontStyle::Oblique, FontStyle::Italic, FontStyle::Normal],
    }
}

pub(crate) fn weight_preference_order(weight: u16) -> Vec<u16> {
    let desired = weight.clamp(1, 1000);
    let mut candidates: Vec<u16> = (1..=9).map(|i| i * 100).collect();
    if !candidates.contains(&desired) {
        candidates.push(desired);
    }
    candidates.sort_by(|a, b| {
        weight_order_key(*a, desired)
            .cmp(&weight_order_key(*b, desired))
            .then_with(|| a.cmp(b))
    });
    candidates.dedup();
    candidates
}

pub(crate) fn stretch_preference_order(stretch: DbFontStretch) -> Vec<DbFontStretch> {
    let target = stretch.to_percentage();
    let mut variants = [
        DbFontStretch::UltraCondensed,
        DbFontStretch::ExtraCondensed,
        DbFontStretch::Condensed,
        DbFontStretch::SemiCondensed,
        DbFontStretch::Normal,
        DbFontStretch::SemiExpanded,
        DbFontStretch::Expanded,
        DbFontStretch::ExtraExpanded,
        DbFontStretch::UltraExpanded,
    ];
    variants.sort_by(|a, b| {
        let ka = stretch_order_key(a.to_percentage(), target);
        let kb = stretch_order_key(b.to_percentage(), target);
        match ka.0.cmp(&kb.0) {
            std::cmp::Ordering::Equal => ka.1.partial_cmp(&kb.1).unwrap_or(std::cmp::Ordering::Equal),
            other => other,
        }
    });
    variants.to_vec()
}

fn weight_order_key(candidate: u16, desired: u16) -> (u8, i32) {
    let desired = desired.clamp(1, 1000) as i32;
    let candidate = candidate as i32;

    if (400..=500).contains(&desired) {
        if candidate >= desired && candidate <= 500 {
            return (0, (candidate - desired).abs());
        }
        if candidate < desired {
            return (1, (desired - candidate).abs());
        }
        return (2, (candidate - desired).abs());
    }

    if desired < 400 {
        if candidate <= desired {
            return (0, (desired - candidate).abs());
        }
        return (1, (candidate - desired).abs());
    }

    // desired > 500
    if candidate >= desired {
        (0, (candidate - desired).abs())
    } else {
        (1, (desired - candidate).abs())
    }
}

fn stretch_order_key(candidate: f32, desired: f32) -> (u8, f32) {
    if (candidate - desired).abs() < f32::EPSILON {
        return (0, 0.0);
    }

    if desired <= 100.0 {
        if candidate <= desired {
            return (0, (desired - candidate).abs());
        }
        return (1, (candidate - desired).abs());
    }

    // desired > 100
    if candidate >= desired {
        (0, (candidate - desired).abs())
    } else {
        (1, (desired - candidate).abs())
    }
}

fn is_bidi_control_char(ch: char) -> bool {
    matches!(
        ch,
        '\u{202a}' // LRE
            | '\u{202b}' // RLE
            | '\u{202c}' // PDF
            | '\u{202d}' // LRO
            | '\u{202e}' // RLO
            | '\u{2066}' // LRI
            | '\u{2067}' // RLI
            | '\u{2068}' // FSI
            | '\u{2069}' // PDI
    )
}

#[cfg(test)]
fn is_emoji_dominant(text: &str) -> bool {
    let mut saw_emoji = false;
    for c in text.chars() {
        if crate::text::emoji::is_emoji(c) {
            saw_emoji = true;
            continue;
        }
        if c.is_whitespace() || matches!(c, '\u{200d}' | '\u{fe0f}' | '\u{fe0e}') {
            continue;
        }
        return false;
    }
    saw_emoji
}

fn font_is_emoji_font(font: &LoadedFont) -> bool {
    let name = font.family.to_lowercase();
    name.contains("emoji") || name.contains("color") || name.contains("twemoji") || name.contains("symbola")
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum EmojiPreference {
    PreferEmoji,
    AvoidEmoji,
    Neutral,
}

#[derive(Default)]
struct FontPreferencePicker {
    prefer_emoji: bool,
    avoid_emoji: bool,
    first_emoji: Option<(LoadedFont, usize)>,
    first_text: Option<(LoadedFont, usize)>,
    first_emoji_any: Option<(LoadedFont, usize)>,
    first_text_any: Option<(LoadedFont, usize)>,
    order: usize,
}

impl FontPreferencePicker {
    fn new(pref: EmojiPreference) -> Self {
        Self {
            prefer_emoji: matches!(pref, EmojiPreference::PreferEmoji),
            avoid_emoji: matches!(pref, EmojiPreference::AvoidEmoji),
            ..Self::default()
        }
    }

    fn bump_order(&mut self) -> usize {
        let idx = self.order;
        self.order += 1;
        idx
    }

    fn record_any(&mut self, font: &LoadedFont, is_emoji_font: bool, idx: usize) {
        if is_emoji_font {
            if self.first_emoji_any.is_none() {
                self.first_emoji_any = Some((font.clone(), idx));
            }
        } else if self.first_text_any.is_none() {
            self.first_text_any = Some((font.clone(), idx));
        }
    }

    fn consider(&mut self, font: LoadedFont, is_emoji_font: bool, idx: usize) -> Option<LoadedFont> {
        if is_emoji_font {
            if self.first_emoji.is_none() {
                self.first_emoji = Some((font.clone(), idx));
            }
            if self.prefer_emoji && !self.avoid_emoji {
                return Some(font);
            }
            if !self.prefer_emoji && !self.avoid_emoji {
                return Some(font);
            }
            // avoid_emoji => keep as fallback if nothing else has a glyph
        } else {
            if self.first_text.is_none() {
                self.first_text = Some((font.clone(), idx));
            }
            if self.avoid_emoji {
                return Some(font);
            }
            if !self.prefer_emoji {
                return Some(font);
            }
            // prefer_emoji => keep searching for an emoji font with coverage
        }

        None
    }

    fn finish(&mut self) -> Option<LoadedFont> {
        let first_emoji = self.first_emoji.take().or_else(|| self.first_emoji_any.take());
        let first_text = self.first_text.take().or_else(|| self.first_text_any.take());

        if self.prefer_emoji && !self.avoid_emoji {
            first_emoji.map(|(f, _)| f).or_else(|| first_text.map(|(f, _)| f))
        } else if self.avoid_emoji {
            first_text.map(|(f, _)| f).or_else(|| first_emoji.map(|(f, _)| f))
        } else {
            match (first_text, first_emoji) {
                (Some((text, ti)), Some((emoji, ei))) => {
                    if ti <= ei {
                        Some(text)
                    } else {
                        Some(emoji)
                    }
                }
                (Some((text, _)), None) => Some(text),
                (None, Some((emoji, _))) => Some(emoji),
                (None, None) => None,
            }
        }
    }
}

fn emoji_preference_for_char(ch: char, variant: FontVariantEmoji) -> EmojiPreference {
    match variant {
        FontVariantEmoji::Emoji => EmojiPreference::PreferEmoji,
        FontVariantEmoji::Text => EmojiPreference::AvoidEmoji,
        FontVariantEmoji::Unicode => {
            if emoji::is_emoji_presentation(ch) {
                EmojiPreference::PreferEmoji
            } else {
                EmojiPreference::AvoidEmoji
            }
        }
        FontVariantEmoji::Normal => {
            if emoji::is_emoji_presentation(ch) {
                EmojiPreference::PreferEmoji
            } else {
                EmojiPreference::Neutral
            }
        }
    }
}

fn emoji_preference_with_selector(ch: char, next: Option<char>, variant: FontVariantEmoji) -> EmojiPreference {
    if let Some(sel) = next {
        if sel == '\u{FE0F}' {
            return EmojiPreference::PreferEmoji;
        }
        if sel == '\u{FE0E}' {
            return EmojiPreference::AvoidEmoji;
        }
    }

    let base_pref = emoji_preference_for_char(ch, variant);

    if let Some('\u{200d}') = next {
        if emoji::is_emoji(ch) || emoji::is_emoji_presentation(ch) {
            return EmojiPreference::PreferEmoji;
        }
    }

    base_pref
}

fn build_family_entries(style: &ComputedStyle) -> Vec<crate::text::font_fallback::FamilyEntry> {
    use crate::text::font_fallback::FamilyEntry;
    let mut entries = Vec::new();
    for family in &style.font_family {
        if let Some(generic) = crate::text::font_db::GenericFamily::parse(family) {
            entries.push(FamilyEntry::Generic(generic));
        } else {
            entries.push(FamilyEntry::Named(family.clone()));
        }
    }

    if matches!(style.font_variant_emoji, FontVariantEmoji::Emoji)
        && !entries.iter().any(|e| {
            matches!(
                e,
                crate::text::font_fallback::FamilyEntry::Generic(crate::text::font_db::GenericFamily::Emoji)
            )
        })
    {
        entries.insert(
            0,
            crate::text::font_fallback::FamilyEntry::Generic(crate::text::font_db::GenericFamily::Emoji),
        );
    }

    if !entries.iter().any(|e| {
        matches!(
            e,
            crate::text::font_fallback::FamilyEntry::Generic(crate::text::font_db::GenericFamily::SansSerif)
        )
    }) {
        entries.push(crate::text::font_fallback::FamilyEntry::Generic(
            crate::text::font_db::GenericFamily::SansSerif,
        ));
    }

    entries
}

fn resolve_font_for_char(
    ch: char,
    families: &[crate::text::font_fallback::FamilyEntry],
    weight: u16,
    style: FontStyle,
    oblique_angle: Option<f32>,
    stretch: DbFontStretch,
    font_context: &FontContext,
    picker: &mut FontPreferencePicker,
) -> Option<LoadedFont> {
    use crate::text::font_fallback::FamilyEntry;
    use fontdb::Family;
    let db = font_context.database();
    let is_emoji = crate::text::font_db::FontDatabase::is_emoji(ch);
    let weight_preferences = weight_preference_order(weight);
    let slope_preferences = slope_preference_order(style);
    let stretch_preferences = stretch_preference_order(stretch);
    let math_families = font_context.math_family_names();
    for entry in families {
        if let FamilyEntry::Generic(crate::text::font_db::GenericFamily::Math) = entry {
            for family in &math_families {
                if let Some(font) =
                    font_context.match_web_font_for_char(family, weight, style, stretch, oblique_angle, ch)
                {
                    return Some(font);
                }
                for stretch_choice in &stretch_preferences {
                    for slope in slope_preferences {
                        for weight_choice in &weight_preferences {
                            let query = fontdb::Query {
                                families: &[Family::Name(family.as_str())],
                                weight: fontdb::Weight(*weight_choice),
                                stretch: (*stretch_choice).into(),
                                style: (*slope).into(),
                            };
                            if let Some(id) = db.inner().query(&query) {
                                if let Some(font) = db.load_font(id) {
                                    let is_emoji_font = font_is_emoji_font(&font);
                                    let idx = picker.bump_order();
                                    picker.record_any(&font, is_emoji_font, idx);
                                    if db.has_glyph(id, ch) {
                                        if let Some(font) = picker.consider(font, is_emoji_font, idx) {
                                            return Some(font);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        if let FamilyEntry::Named(name) = entry {
            if let Some(font) = font_context.match_web_font_for_char(name, weight, style, stretch, oblique_angle, ch) {
                return Some(font);
            }
            if font_context.is_web_family_declared(name) {
                continue;
            }
        }

        for stretch_choice in &stretch_preferences {
            for slope in slope_preferences {
                for weight_choice in &weight_preferences {
                    let query = match entry {
                        FamilyEntry::Named(name) => fontdb::Query {
                            families: &[Family::Name(name)],
                            weight: fontdb::Weight(*weight_choice),
                            stretch: (*stretch_choice).into(),
                            style: (*slope).into(),
                        },
                        FamilyEntry::Generic(generic) => fontdb::Query {
                            families: &[generic.to_fontdb()],
                            weight: fontdb::Weight(*weight_choice),
                            stretch: (*stretch_choice).into(),
                            style: (*slope).into(),
                        },
                    };

                    if let Some(id) = db.inner().query(&query) {
                        if let Some(font) = db.load_font(id) {
                            let is_emoji_font = font_is_emoji_font(&font);
                            let idx = picker.bump_order();
                            picker.record_any(&font, is_emoji_font, idx);
                            if db.has_glyph(id, ch) {
                                if let Some(font) = picker.consider(font, is_emoji_font, idx) {
                                    return Some(font);
                                }
                            }
                        }
                    }
                }
            }
        }

        if let FamilyEntry::Generic(generic) = entry {
            for name in generic.fallback_families() {
                for weight_choice in &weight_preferences {
                    for slope in slope_preferences {
                        for stretch_choice in &stretch_preferences {
                            let query = fontdb::Query {
                                families: &[Family::Name(name)],
                                weight: fontdb::Weight(*weight_choice),
                                stretch: (*stretch_choice).into(),
                                style: (*slope).into(),
                            };
                            if let Some(id) = db.inner().query(&query) {
                                if let Some(font) = db.load_font(id) {
                                    let is_emoji_font = font_is_emoji_font(&font);
                                    let idx = picker.bump_order();
                                    picker.record_any(&font, is_emoji_font, idx);
                                    if db.has_glyph(id, ch) {
                                        if let Some(font) = picker.consider(font, is_emoji_font, idx) {
                                            return Some(font);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    if is_emoji && !picker.avoid_emoji {
        for id in db.find_emoji_fonts() {
            if let Some(font) = db.load_font(id) {
                let idx = picker.bump_order();
                picker.record_any(&font, true, idx);
                if db.has_glyph(id, ch) {
                    if let Some(font) = picker.consider(font, true, idx) {
                        return Some(font);
                    }
                }
            }
        }
    }

    for face in db.faces() {
        if let Some(font) = db.load_font(face.id) {
            let is_emoji_font = font_is_emoji_font(&font);
            let idx = picker.bump_order();
            picker.record_any(&font, is_emoji_font, idx);
            if db.has_glyph(face.id, ch) {
                if let Some(font) = picker.consider(font, is_emoji_font, idx) {
                    return Some(font);
                }
            }
        }
    }

    picker.finish()
}

#[allow(clippy::too_many_arguments)]
fn push_font_run(
    out: &mut Vec<FontRun>,
    run: &ItemizedRun,
    start: usize,
    end: usize,
    font: Arc<LoadedFont>,
    synthetic_bold: f32,
    synthetic_oblique: f32,
    font_size: f32,
    baseline_shift: f32,
    features: &[Feature],
    authored_variations: &[Variation],
    style: &ComputedStyle,
) {
    let mut variations = authored_variations.to_vec();
    if let Ok(face) = font.as_ttf_face() {
        let axes: Vec<_> = face.variation_axes().into_iter().collect();
        if !axes.is_empty() {
            let mut set_tags: HashSet<Tag> = HashSet::from_iter(variations.iter().map(|v| v.tag));
            let wght_tag = Tag::from_bytes(b"wght");
            let wdth_tag = Tag::from_bytes(b"wdth");
            let opsz_tag = Tag::from_bytes(b"opsz");
            let ital_tag = Tag::from_bytes(b"ital");
            let slnt_tag = Tag::from_bytes(b"slnt");
            let has_slnt_axis = axes.iter().any(|a| a.tag == slnt_tag);
            let has_ital_axis = axes.iter().any(|a| a.tag == ital_tag);
            let slnt_angle = match style.font_style {
                CssFontStyle::Oblique(angle) => Some(angle.unwrap_or(DEFAULT_OBLIQUE_ANGLE_DEG)),
                CssFontStyle::Italic if !has_ital_axis => Some(DEFAULT_OBLIQUE_ANGLE_DEG),
                _ => None,
            };
            let ital_needed = matches!(style.font_style, CssFontStyle::Italic)
                || (matches!(style.font_style, CssFontStyle::Oblique(_)) && !has_slnt_axis);

            for axis in axes {
                if axis.tag == wght_tag && !set_tags.contains(&wght_tag) {
                    variations.push(Variation {
                        tag: wght_tag,
                        value: style.font_weight.to_u16() as f32,
                    });
                    set_tags.insert(wght_tag);
                } else if axis.tag == wdth_tag && !set_tags.contains(&wdth_tag) {
                    variations.push(Variation {
                        tag: wdth_tag,
                        value: style.font_stretch.to_percentage(),
                    });
                    set_tags.insert(wdth_tag);
                } else if axis.tag == opsz_tag
                    && !set_tags.contains(&opsz_tag)
                    && matches!(style.font_optical_sizing, crate::style::types::FontOpticalSizing::Auto)
                {
                    variations.push(Variation {
                        tag: opsz_tag,
                        value: font_size,
                    });
                    set_tags.insert(opsz_tag);
                } else if axis.tag == slnt_tag && !set_tags.contains(&slnt_tag) {
                    if let Some(angle) = slnt_angle {
                        let slnt_value = (-angle).clamp(axis.min_value, axis.max_value);
                        variations.push(Variation {
                            tag: slnt_tag,
                            value: slnt_value,
                        });
                        set_tags.insert(slnt_tag);
                    }
                } else if axis.tag == ital_tag && !set_tags.contains(&ital_tag) {
                    if ital_needed {
                        variations.push(Variation {
                            tag: ital_tag,
                            value: 1.0,
                        });
                        set_tags.insert(ital_tag);
                    } else if matches!(style.font_style, CssFontStyle::Normal) {
                        variations.push(Variation {
                            tag: ital_tag,
                            value: 0.0,
                        });
                        set_tags.insert(ital_tag);
                    }
                }
            }
        }
    }

    let language = match &style.font_language_override {
        FontLanguageOverride::Normal => style.language.clone(),
        FontLanguageOverride::Override(tag) => tag.clone(),
    };

    let segment = &run.text[start..end];
    out.push(FontRun {
        text: segment.to_string(),
        start: run.start + start,
        end: run.start + end,
        font,
        synthetic_bold,
        synthetic_oblique,
        script: run.script,
        direction: run.direction,
        level: run.level,
        font_size,
        baseline_shift,
        language,
        features: features.to_vec(),
        variations,
        rotation: RunRotation::None,
        vertical: false,
    });
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
    /// Additional baseline shift in pixels (positive raises text).
    pub baseline_shift: f32,
    /// Language set on the shaping buffer (if provided).
    pub language: Option<HbLanguage>,
    /// Synthetic bold stroke width in pixels (0 = none).
    pub synthetic_bold: f32,
    /// Synthetic oblique shear factor (tan(angle); 0 = none).
    pub synthetic_oblique: f32,
    /// Optional rotation to apply when painting.
    pub rotation: RunRotation,

    /// Optional additional scale factor (1.0 = none).
    pub scale: f32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RunRotation {
    None,
    Ccw90,
    Cw90,
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
    let mut rb_face = Face::from_slice(&run.font.data, run.font.index).ok_or_else(|| TextError::ShapingFailed {
        text: run.text.clone(),
        reason: "Failed to create HarfBuzz face".to_string(),
    })?;
    if !run.variations.is_empty() {
        rb_face.set_variations(&run.variations);
    }

    // Create Unicode buffer
    let mut buffer = UnicodeBuffer::new();
    buffer.push_str(&run.text);

    let mut language: Option<HbLanguage> = None;

    // Set buffer properties
    buffer.set_direction(run.direction.to_harfbuzz());
    if let Some(script) = run.script.to_harfbuzz() {
        buffer.set_script(script);
    }
    let lang_tag = run.language.trim();
    if !lang_tag.is_empty() {
        if let Ok(lang) = HbLanguage::from_str(lang_tag) {
            buffer.set_language(lang.clone());
            language = Some(lang);
        }
    }

    // Shape the text
    let output = rustybuzz::shape(&rb_face, &run.features, buffer);

    // Calculate scale factor
    let units_per_em = rb_face.units_per_em() as f32;
    let scale = run.font_size / units_per_em;

    // Extract glyph information
    let glyph_infos = output.glyph_infos();
    let glyph_positions = output.glyph_positions();

    let mut glyphs = Vec::with_capacity(glyph_infos.len());
    let mut x_position = 0.0_f32;

    for (info, pos) in glyph_infos.iter().zip(glyph_positions.iter()) {
        let cluster = info.cluster as usize;
        let is_bidi_control = run
            .text
            .get(cluster..)
            .and_then(|s| s.chars().next())
            .map_or(false, is_bidi_control_char);

        let x_advance = pos.x_advance as f32 * scale;
        if !is_bidi_control {
            let x_offset = x_position + (pos.x_offset as f32 * scale);
            let y_offset = pos.y_offset as f32 * scale + run.baseline_shift;
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
        baseline_shift: run.baseline_shift,
        language,
        synthetic_bold: run.synthetic_bold,
        synthetic_oblique: run.synthetic_oblique,
        rotation: RunRotation::None,
        scale: 1.0,
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
    cache: std::sync::Arc<std::sync::Mutex<std::collections::HashMap<ShapingCacheKey, std::sync::Arc<Vec<ShapedRun>>>>>,
}

#[derive(Clone, Eq, Debug)]
struct ShapingCacheKey {
    text: std::sync::Arc<str>,
    style_hash: u64,
    font_generation: u64,
}

fn shaping_style_hash(style: &ComputedStyle) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut hasher = std::collections::hash_map::DefaultHasher::new();

    std::mem::discriminant(&style.direction).hash(&mut hasher);
    std::mem::discriminant(&style.unicode_bidi).hash(&mut hasher);
    std::mem::discriminant(&style.writing_mode).hash(&mut hasher);
    std::mem::discriminant(&style.text_orientation).hash(&mut hasher);
    style.language.hash(&mut hasher);
    style.font_family.hash(&mut hasher);
    style.font_size.to_bits().hash(&mut hasher);
    style.font_weight.to_u16().hash(&mut hasher);
    style.font_stretch.to_percentage().to_bits().hash(&mut hasher);

    match style.font_style {
        CssFontStyle::Normal => 0u8.hash(&mut hasher),
        CssFontStyle::Italic => 1u8.hash(&mut hasher),
        CssFontStyle::Oblique(angle) => {
            2u8.hash(&mut hasher);
            angle.unwrap_or_default().to_bits().hash(&mut hasher);
        }
    }

    std::mem::discriminant(&style.font_variant).hash(&mut hasher);
    std::mem::discriminant(&style.font_variant_caps).hash(&mut hasher);
    std::mem::discriminant(&style.font_variant_emoji).hash(&mut hasher);
    std::mem::discriminant(&style.font_variant_position).hash(&mut hasher);
    std::mem::discriminant(&style.font_variant_numeric.figure).hash(&mut hasher);
    std::mem::discriminant(&style.font_variant_numeric.spacing).hash(&mut hasher);
    std::mem::discriminant(&style.font_variant_numeric.fraction).hash(&mut hasher);
    (style.font_variant_numeric.ordinal as u8).hash(&mut hasher);
    (style.font_variant_numeric.slashed_zero as u8).hash(&mut hasher);

    (style.font_variant_ligatures.common as u8).hash(&mut hasher);
    (style.font_variant_ligatures.discretionary as u8).hash(&mut hasher);
    (style.font_variant_ligatures.historical as u8).hash(&mut hasher);
    (style.font_variant_ligatures.contextual as u8).hash(&mut hasher);

    style.font_variant_alternates.historical_forms.hash(&mut hasher);
    style.font_variant_alternates.stylistic.hash(&mut hasher);
    style.font_variant_alternates.stylesets.hash(&mut hasher);

    match style.font_variant_east_asian.variant {
        Some(var) => {
            1u8.hash(&mut hasher);
            std::mem::discriminant(&var).hash(&mut hasher);
        }
        None => 0u8.hash(&mut hasher),
    }
    match style.font_variant_east_asian.width {
        Some(width) => {
            1u8.hash(&mut hasher);
            std::mem::discriminant(&width).hash(&mut hasher);
        }
        None => 0u8.hash(&mut hasher),
    }
    (style.font_variant_east_asian.ruby as u8).hash(&mut hasher);

    std::mem::discriminant(&style.font_kerning).hash(&mut hasher);
    (style.font_synthesis.weight as u8).hash(&mut hasher);
    (style.font_synthesis.style as u8).hash(&mut hasher);
    (style.font_synthesis.small_caps as u8).hash(&mut hasher);
    (style.font_synthesis.position as u8).hash(&mut hasher);
    std::mem::discriminant(&style.font_optical_sizing).hash(&mut hasher);

    match style.font_language_override.clone() {
        FontLanguageOverride::Normal => 0u8.hash(&mut hasher),
        FontLanguageOverride::Override(tag) => {
            1u8.hash(&mut hasher);
            tag.hash(&mut hasher);
        }
    }

    match style.font_size_adjust {
        FontSizeAdjust::None => 0u8.hash(&mut hasher),
        FontSizeAdjust::Number(v) => {
            1u8.hash(&mut hasher);
            v.to_bits().hash(&mut hasher);
        }
        FontSizeAdjust::FromFont => 2u8.hash(&mut hasher),
    }

    for setting in &style.font_feature_settings {
        setting.tag.hash(&mut hasher);
        setting.value.hash(&mut hasher);
    }
    for setting in &style.font_variation_settings {
        setting.tag.hash(&mut hasher);
        setting.value.to_bits().hash(&mut hasher);
    }

    hasher.finish()
}

impl std::hash::Hash for ShapingCacheKey {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.style_hash.hash(state);
        self.font_generation.hash(state);
        self.text.hash(state);
    }
}

impl PartialEq for ShapingCacheKey {
    fn eq(&self, other: &Self) -> bool {
        self.style_hash == other.style_hash
            && self.font_generation == other.font_generation
            && self.text.as_ref() == other.text.as_ref()
    }
}

impl ShapingPipeline {
    /// Creates a new shaping pipeline.
    pub fn new() -> Self {
        Self {
            cache: std::sync::Arc::new(std::sync::Mutex::new(std::collections::HashMap::new())),
        }
    }

    /// Clears the shaping cache. Useful when reusing a pipeline across multiple documents.
    pub fn clear_cache(&self) {
        if let Ok(mut cache) = self.cache.lock() {
            cache.clear();
        }
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
        self.shape_core(text, style, font_context, None, None)
    }

    fn shape_core(
        &self,
        text: &str,
        style: &ComputedStyle,
        font_context: &FontContext,
        base_direction: Option<Direction>,
        explicit_bidi: Option<ExplicitBidiContext>,
    ) -> Result<Vec<ShapedRun>> {
        // Handle empty text
        if text.is_empty() {
            return Ok(Vec::new());
        }

        if matches!(style.font_variant, FontVariant::SmallCaps)
            || matches!(
                style.font_variant_caps,
                FontVariantCaps::SmallCaps | FontVariantCaps::AllSmallCaps
            )
        {
            if !has_native_small_caps(style, font_context) && style.font_synthesis.small_caps {
                return self.shape_small_caps(text, style, font_context, base_direction, explicit_bidi);
            }
        }

        let cache_text = std::sync::Arc::<str>::from(text);
        let style_hash = shaping_style_hash(style);
        let font_generation = font_context.font_generation();
        let cache_key = ShapingCacheKey {
            text: cache_text.clone(),
            style_hash,
            font_generation,
        };
        if let Ok(cache) = self.cache.lock() {
            if let Some(cached) = cache.get(&cache_key) {
                return Ok((**cached).clone());
            }
        }

        // Step 1: Bidi analysis
        let mut resolved_base_dir = base_direction.unwrap_or_else(|| match style.direction {
            crate::style::types::Direction::Ltr => Direction::LeftToRight,
            crate::style::types::Direction::Rtl => Direction::RightToLeft,
        });
        let mut bidi_context = explicit_bidi;

        // text-orientation: upright sets the used direction to ltr and treats all characters as
        // strong LTR for bidi reordering in vertical typographic modes per CSS Writing Modes.
        if is_vertical_typographic_mode(style.writing_mode)
            && matches!(style.text_orientation, crate::style::types::TextOrientation::Upright)
        {
            resolved_base_dir = Direction::LeftToRight;
            let level = Level::ltr();
            let mut ctx = bidi_context.unwrap_or(ExplicitBidiContext {
                level,
                override_all: false,
            });
            ctx.level = level;
            ctx.override_all = true;
            bidi_context = Some(ctx);
        }

        let bidi = BidiAnalysis::analyze_with_base(text, style, resolved_base_dir, bidi_context);

        // Step 2: Script itemization
        let itemized_runs = itemize_text(text, &bidi);
        let itemized_runs = split_itemized_runs_by_paragraph(itemized_runs, bidi.paragraphs());

        // Step 3: Font matching
        let font_runs = assign_fonts(&itemized_runs, style, font_context)?;

        // Step 4: Shape each run, applying vertical text-orientation when needed.
        let mut font_runs = font_runs;
        if is_vertical_typographic_mode(style.writing_mode) {
            font_runs = apply_vertical_text_orientation(font_runs, style.text_orientation);
        } else if is_sideways_writing_mode(style.writing_mode) {
            font_runs = apply_sideways_text_orientation(font_runs);
        }

        let mut shaped_runs = Vec::with_capacity(font_runs.len());
        for run in &font_runs {
            let mut shaped = shape_font_run(run)?;
            shaped.rotation = run.rotation;
            shaped_runs.push(shaped);
        }

        // Step 5: Reorder for bidi if needed
        if bidi.needs_reordering() {
            reorder_runs(&mut shaped_runs, bidi.paragraphs());
        }

        if let Ok(mut cache) = self.cache.lock() {
            cache.insert(cache_key, std::sync::Arc::new(shaped_runs.clone()));
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
        base_direction: Direction,
    ) -> Result<Vec<ShapedRun>> {
        self.shape_core(text, style, font_context, Some(base_direction), None)
    }

    /// Shapes text with an explicit bidi context (embedding level/override).
    pub fn shape_with_context(
        &self,
        text: &str,
        style: &ComputedStyle,
        font_context: &FontContext,
        base_direction: Direction,
        bidi_context: Option<ExplicitBidiContext>,
    ) -> Result<Vec<ShapedRun>> {
        self.shape_core(text, style, font_context, Some(base_direction), bidi_context)
    }

    /// Measures the total advance width of shaped text.
    ///
    /// Convenience method that shapes text and returns only the width.
    pub fn measure_width(&self, text: &str, style: &ComputedStyle, font_context: &FontContext) -> Result<f32> {
        let runs = self.shape(text, style, font_context)?;
        Ok(runs.iter().map(|r| r.advance).sum())
    }

    fn shape_small_caps(
        &self,
        text: &str,
        style: &ComputedStyle,
        font_context: &FontContext,
        base_direction: Option<Direction>,
        explicit_bidi: Option<ExplicitBidiContext>,
    ) -> Result<Vec<ShapedRun>> {
        const SMALL_CAPS_SCALE: f32 = 0.8;

        let mut runs = Vec::new();
        let mut segment_start: usize = 0;
        let mut buffer = String::new();
        let mut current_small = None;
        let all_small = matches!(style.font_variant_caps, FontVariantCaps::AllSmallCaps);

        for (idx, ch) in text.char_indices() {
            let is_small = ch.is_lowercase() || (all_small && ch.is_uppercase());
            if let Some(flag) = current_small {
                if flag != is_small {
                    self.flush_small_caps_segment(
                        &mut runs,
                        &buffer,
                        segment_start,
                        flag,
                        style,
                        font_context,
                        SMALL_CAPS_SCALE,
                        base_direction,
                        explicit_bidi,
                    )?;
                    buffer.clear();
                    segment_start = idx;
                    current_small = Some(is_small);
                }
            } else {
                current_small = Some(is_small);
            }

            if is_small {
                for up in ch.to_uppercase() {
                    buffer.push(up);
                }
            } else {
                buffer.push(ch);
            }
        }

        if !buffer.is_empty() {
            self.flush_small_caps_segment(
                &mut runs,
                &buffer,
                segment_start,
                current_small.unwrap_or(false),
                style,
                font_context,
                SMALL_CAPS_SCALE,
                base_direction,
                explicit_bidi,
            )?;
        }

        Ok(runs)
    }

    fn flush_small_caps_segment(
        &self,
        out: &mut Vec<ShapedRun>,
        segment_text: &str,
        base_offset: usize,
        is_small: bool,
        style: &ComputedStyle,
        font_context: &FontContext,
        scale: f32,
        base_direction: Option<Direction>,
        explicit_bidi: Option<ExplicitBidiContext>,
    ) -> Result<()> {
        let mut seg_style = style.clone();
        seg_style.font_variant = FontVariant::Normal;
        seg_style.font_variant_caps = FontVariantCaps::Normal;
        if is_small {
            seg_style.font_size *= scale;
        }
        let mut shaped = self.shape_core(segment_text, &seg_style, font_context, base_direction, explicit_bidi)?;
        for run in &mut shaped {
            run.start += base_offset;
            run.end += base_offset;
        }
        out.extend(shaped);
        Ok(())
    }
}

/// Reorders shaped runs for bidi display.
///
/// Implements visual reordering based on bidi levels.
fn reorder_runs(runs: &mut [ShapedRun], paragraphs: &[ParagraphBoundary]) {
    if runs.is_empty() {
        return;
    }

    fn reorder_slice(slice: &mut [ShapedRun]) {
        if slice.is_empty() {
            return;
        }

        let max_level = slice.iter().map(|r| r.level).max().unwrap_or(0);
        for level in (1..=max_level).rev() {
            let mut start: Option<usize> = None;
            for i in 0..slice.len() {
                if slice[i].level >= level {
                    start.get_or_insert(i);
                } else if let Some(s) = start {
                    slice[s..i].reverse();
                    start = None;
                }
            }
            if let Some(s) = start {
                slice[s..].reverse();
            }
        }
    }

    if paragraphs.is_empty() {
        reorder_slice(runs);
        return;
    }

    let mut idx = 0;
    for para in paragraphs {
        while idx < runs.len() && runs[idx].end <= para.start_byte {
            idx += 1;
        }
        let mut end = idx;
        while end < runs.len() && runs[end].start < para.end_byte {
            end += 1;
        }
        if idx < end {
            reorder_slice(&mut runs[idx..end]);
        }
        idx = end;
    }

    if idx < runs.len() {
        reorder_slice(&mut runs[idx..]);
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
    use crate::css::types::{FontFaceRule, FontFaceSource};
    use crate::style::types::{
        EastAsianVariant, EastAsianWidth, FontFeatureSetting, FontKerning, FontStretch, FontVariantLigatures,
        FontVariationSetting, FontWeight, NumericFigure, NumericFraction, NumericSpacing, TextOrientation, WritingMode,
    };
    use crate::text::font_db::FontDatabase;
    use crate::text::font_db::{
        FontStretch as DbFontStretch, FontStyle as DbFontStyle, FontWeight as DbFontWeight, GenericFamily,
    };
    use crate::text::font_fallback::FamilyEntry;
    use std::fs;
    use std::path::Path;
    use std::sync::Arc;
    use ttf_parser::name_id;
    use unicode_bidi::Level;
    use url::Url;

    fn load_variable_font() -> (FontContext, String) {
        let font_path = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/fonts/RobotoFlex-VF.ttf");
        let data = fs::read(&font_path).expect("variable font fixture");
        let face = ttf_parser::Face::parse(&data, 0).expect("parse fixture font");
        let family = face
            .names()
            .into_iter()
            .find_map(|name| {
                if name.name_id == name_id::FAMILY {
                    name.to_string()
                } else {
                    None
                }
            })
            .expect("font family name");

        let mut db = FontDatabase::empty();
        db.load_font_data(data).expect("load font into database");
        let ctx = FontContext::with_database(Arc::new(db));
        (ctx, family)
    }

    fn load_font_fixture(path: &Path) -> (Vec<u8>, String) {
        let data = fs::read(path).expect("read font fixture");
        let face = ttf_parser::Face::parse(&data, 0).expect("parse font fixture");
        let family = face
            .names()
            .into_iter()
            .find_map(|name| {
                if name.name_id == name_id::FAMILY {
                    name.to_string()
                } else {
                    None
                }
            })
            .unwrap_or_else(|| {
                path.file_stem()
                    .map(|s| s.to_string_lossy().to_string())
                    .unwrap_or_else(|| "Unknown".to_string())
            });
        (data, family)
    }

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
    fn bidi_analysis_uses_char_indices() {
        let style = ComputedStyle::default();
        let text = "a\u{05d0}\u{05d1}"; // a + two Hebrew letters
        let bidi = BidiAnalysis::analyze(text, &style);

        let indices: Vec<_> = text.char_indices().collect();
        let a_level = bidi.level_at(indices[0].0);
        let hebrew_level = bidi.level_at(indices[1].0);

        assert!(a_level.is_ltr(), "latin char should be LTR");
        assert!(hebrew_level.is_rtl(), "hebrew char should be RTL");
    }

    #[test]
    fn bidi_override_forces_base_direction() {
        let mut style = ComputedStyle::default();
        style.direction = CssDirection::Rtl;
        style.unicode_bidi = crate::style::types::UnicodeBidi::BidiOverride;
        let text = "abc";
        let bidi = BidiAnalysis::analyze(text, &style);

        let indices: Vec<_> = text.char_indices().collect();
        for (byte_idx, _) in indices {
            assert!(bidi.level_at(byte_idx).is_rtl(), "override should force RTL level");
        }
        assert!(!bidi.needs_reordering(), "override should not request reordering");
    }

    #[test]
    fn bidi_plaintext_uses_first_strong_for_base() {
        let mut style = ComputedStyle::default();
        style.unicode_bidi = crate::style::types::UnicodeBidi::Plaintext;

        let rtl_text = "שלום abc";
        let bidi_rtl = BidiAnalysis::analyze(rtl_text, &style);
        assert!(
            bidi_rtl.base_level().is_rtl(),
            "plaintext should pick RTL base from first strong"
        );

        let ltr_text = "abc שלום";
        let bidi_ltr = BidiAnalysis::analyze(ltr_text, &style);
        assert!(
            bidi_ltr.base_level().is_ltr(),
            "plaintext should pick LTR base from first strong"
        );
    }

    #[test]
    fn bidi_controls_do_not_contribute_advance() {
        let style = ComputedStyle::default();
        let ctx = FontContext::new();
        let pipeline = ShapingPipeline::new();
        let clean = pipeline.shape("abc", &style, &ctx).expect("shape clean");
        // Inject an isolate sequence around b.
        let isolated = pipeline
            .shape("a\u{2067}b\u{2069}c", &style, &ctx)
            .expect("shape with controls");
        let clean_adv: f32 = clean.iter().map(|r| r.advance).sum();
        let iso_adv: f32 = isolated.iter().map(|r| r.advance).sum();
        assert!(
            (clean_adv - iso_adv).abs() < 0.1,
            "bidi controls should not change advance ({} vs {})",
            clean_adv,
            iso_adv
        );
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
    fn shaping_sets_language_from_style() {
        let mut style = ComputedStyle::default();
        style.language = "tr-TR".to_string();
        let pipeline = ShapingPipeline::new();
        let font_ctx = FontContext::new();

        let runs = pipeline.shape("i", &style, &font_ctx).expect("shape succeeds");
        assert!(!runs.is_empty());
        assert_eq!(runs[0].language.as_ref().map(|l| l.as_str()), Some("tr-tr"));
    }

    #[test]
    fn font_size_adjust_number_scales_font_run() {
        let font_ctx = FontContext::new();
        let Some(font) = font_ctx.get_sans_serif() else { return };
        let Some(aspect) = font.metrics().ok().and_then(|m| m.aspect_ratio()) else {
            return;
        };

        let mut style = ComputedStyle::default();
        style.font_family = vec![font.family.clone()];
        style.font_size = 20.0;
        let desired = aspect * 2.0;
        style.font_size_adjust = FontSizeAdjust::Number(desired);

        let pipeline = ShapingPipeline::new();
        let runs = pipeline.shape("Hello", &style, &font_ctx).expect("shape succeeds");
        assert!(!runs.is_empty());

        let expected = style.font_size * (desired / aspect);
        assert!((runs[0].font_size - expected).abs() < 0.01);
    }

    #[test]
    fn font_size_adjust_from_font_defaults_to_base_font() {
        let font_ctx = FontContext::new();
        let Some(font) = font_ctx.get_sans_serif() else { return };
        let Some(aspect) = font.metrics().ok().and_then(|m| m.aspect_ratio()) else {
            return;
        };

        let mut style = ComputedStyle::default();
        style.font_family = vec![font.family.clone()];
        style.font_size = 18.0;
        style.font_size_adjust = FontSizeAdjust::FromFont;

        let pipeline = ShapingPipeline::new();
        let runs = pipeline.shape("Hello", &style, &font_ctx).expect("shape succeeds");
        assert!(!runs.is_empty());

        // Using the same font as the reference should preserve the base size.
        assert!((runs[0].font_size - style.font_size).abs() < 0.01);
        // Sanity: aspect was available to avoid the test vacuously passing.
        assert!(aspect > 0.0);
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
        reorder_runs(&mut runs, &[]);
        assert!(runs.is_empty());
    }

    #[test]
    fn itemization_splits_runs_at_paragraph_boundaries() {
        let mut style = ComputedStyle::default();
        style.direction = CssDirection::Ltr;
        let text = "abc\nאבג";

        let bidi = BidiAnalysis::analyze(text, &style);
        let runs = itemize_text(text, &bidi);
        let runs = split_itemized_runs_by_paragraph(runs, bidi.paragraphs());

        assert_eq!(runs.len(), 2, "should yield one run per paragraph in this case");
        let paras = bidi.paragraphs();
        assert_eq!(paras.len(), 2);
        assert_eq!(runs[0].start, paras[0].start_byte);
        assert_eq!(runs[0].end, paras[0].end_byte);
        assert_eq!(runs[1].start, paras[1].start_byte);
        assert_eq!(runs[1].end, paras[1].end_byte);
    }

    #[test]
    fn bidi_analysis_records_paragraph_boundaries() {
        let mut style = ComputedStyle::default();
        style.direction = CssDirection::Ltr;
        let text = "abc\nאבג";

        let analysis = BidiAnalysis::analyze(text, &style);
        let paragraphs = analysis.paragraphs();
        assert_eq!(paragraphs.len(), 2);
        assert_eq!(paragraphs[0].start_byte, 0);
        assert!(paragraphs[0].end_byte > paragraphs[0].start_byte);
        assert_eq!(paragraphs[1].start_byte, paragraphs[0].end_byte);
        assert_eq!(paragraphs[1].end_byte, text.len());
    }

    #[test]
    fn reorder_runs_respects_paragraph_boundaries() {
        fn run(start: usize, end: usize, level: u8) -> ShapedRun {
            ShapedRun {
                text: String::new(),
                start,
                end,
                glyphs: Vec::new(),
                direction: if level % 2 == 0 {
                    Direction::LeftToRight
                } else {
                    Direction::RightToLeft
                },
                level,
                advance: 0.0,
                font: Arc::new(LoadedFont {
                    family: "Test".to_string(),
                    data: Arc::new(Vec::new()),
                    index: 0,
                    weight: DbFontWeight::NORMAL,
                    style: DbFontStyle::Normal,
                    stretch: DbFontStretch::Normal,
                }),
                font_size: 16.0,
                baseline_shift: 0.0,
                language: None,
                synthetic_bold: 0.0,
                synthetic_oblique: 0.0,
                rotation: RunRotation::None,
                scale: 1.0,
            }
        }

        // Two paragraphs whose runs all share the same level; reordering should not swap paragraphs.
        let mut runs = vec![run(0, 3, 1), run(3, 6, 1)];
        let paragraphs = vec![
            ParagraphBoundary {
                start_byte: 0,
                end_byte: 3,
                level: Level::ltr(),
            },
            ParagraphBoundary {
                start_byte: 3,
                end_byte: 6,
                level: Level::ltr(),
            },
        ];

        reorder_runs(&mut runs, &paragraphs);
        assert_eq!(runs[0].start, 0);
        assert_eq!(runs[1].start, 3);
    }

    #[test]
    fn shape_with_direction_uses_explicit_base() {
        let mut style = ComputedStyle::default();
        style.unicode_bidi = crate::style::types::UnicodeBidi::Normal;
        let ctx = FontContext::new();
        let text = "!?";

        let shaped = ShapingPipeline::new()
            .shape_with_direction(text, &style, &ctx, Direction::RightToLeft)
            .expect("shape_with_direction");

        assert_eq!(shaped.len(), 1);
        let run = &shaped[0];
        assert_eq!(run.direction, Direction::RightToLeft);
        assert_eq!(run.level % 2, 1, "bidi level should reflect RTL base");
    }

    #[test]
    fn small_caps_shapes_lowercase_with_scaled_size() {
        let mut style = ComputedStyle::default();
        style.font_variant = FontVariant::SmallCaps;
        style.font_size = 20.0;
        let ctx = FontContext::new();
        let shaped = ShapingPipeline::new().shape("Abc", &style, &ctx).unwrap();
        assert!(shaped.iter().any(|r| (r.font_size - 16.0).abs() < 0.1));
        assert!(shaped.iter().any(|r| (r.font_size - 20.0).abs() < 0.1));
    }

    #[test]
    fn font_synthesis_none_disables_synthetic_small_caps() {
        let mut style = ComputedStyle::default();
        style.font_variant_caps = FontVariantCaps::SmallCaps;
        style.font_size = 18.0;
        style.font_synthesis.small_caps = false;
        let ctx = FontContext::new();
        let shaped = ShapingPipeline::new().shape("Abc", &style, &ctx).unwrap();
        assert_eq!(shaped.len(), 1, "synthetic small-caps should not split runs");
        assert!(shaped.iter().all(|r| (r.font_size - 18.0).abs() < 0.1));
    }

    #[test]
    fn synthetic_super_position_applies_without_feature() {
        let pipeline = ShapingPipeline::new();
        let mut style = ComputedStyle::default();
        style.font_variant_position = FontVariantPosition::Super;
        style.font_size = 20.0;

        let ctx = FontContext::new();
        if !ctx.has_fonts() {
            return;
        }

        let runs = match pipeline.shape("x", &style, &ctx) {
            Ok(r) => r,
            Err(_) => return,
        };
        if runs.is_empty() {
            return;
        }
        let run = &runs[0];
        if ctx.supports_feature(&run.font, *b"sups") {
            // Genuine superscript support means no synthesis needed.
            return;
        }

        assert!(
            run.font_size < style.font_size,
            "synthetic superscript should shrink font size"
        );
        assert!(
            run.glyphs.iter().any(|g| g.y_offset > 0.0),
            "synthetic superscript should raise glyphs"
        );
    }

    #[test]
    fn font_synthesis_position_none_disables_synthetic_shift() {
        let pipeline = ShapingPipeline::new();
        let mut style = ComputedStyle::default();
        style.font_variant_position = FontVariantPosition::Super;
        style.font_synthesis.position = false;

        let ctx = FontContext::new();
        if !ctx.has_fonts() {
            return;
        }

        let runs = match pipeline.shape("x", &style, &ctx) {
            Ok(r) => r,
            Err(_) => return,
        };
        if runs.is_empty() {
            return;
        }
        let run = &runs[0];
        if ctx.supports_feature(&run.font, *b"sups") {
            // Real superscript glyphs remain allowed.
            return;
        }

        assert!((run.font_size - style.font_size).abs() < 0.01);
        assert!(
            run.glyphs.iter().all(|g| g.y_offset.abs() < 0.01),
            "synthesis disabled should keep glyphs on the baseline"
        );
    }

    #[test]
    fn font_variation_settings_adjust_variable_font_axes() {
        let (ctx, family) = load_variable_font();
        let mut style = ComputedStyle::default();
        style.font_family = vec![family];
        style.font_size = 16.0;

        let pipeline = ShapingPipeline::new();
        let default_run = pipeline.shape("mmmm", &style, &ctx).expect("default run");

        style.font_variation_settings = vec![FontVariationSetting {
            tag: *b"wdth",
            value: 75.0,
        }];
        let narrow_run = pipeline.shape("mmmm", &style, &ctx).expect("narrow run");

        style.font_variation_settings = vec![FontVariationSetting {
            tag: *b"wdth",
            value: 130.0,
        }];
        let wide_run = pipeline.shape("mmmm", &style, &ctx).expect("wide run");

        let default_advance: f32 = default_run.iter().map(|r| r.advance).sum();
        let narrow_advance: f32 = narrow_run.iter().map(|r| r.advance).sum();
        let wide_advance: f32 = wide_run.iter().map(|r| r.advance).sum();

        assert!(
            narrow_advance < default_advance,
            "wdth axis should shrink glyph advances"
        );
        assert!(wide_advance > default_advance, "wdth axis should widen glyph advances");
    }

    #[test]
    fn shaping_cache_resets_when_fonts_change() {
        let roboto_path = Path::new(env!("CARGO_MANIFEST_DIR")).join("resources/fonts/Roboto-Regular.ttf");
        let (roboto_data, roboto_family) = load_font_fixture(&roboto_path);

        let mut db = FontDatabase::empty();
        db.load_font_data(roboto_data).expect("load fallback font");
        let ctx = FontContext::with_database(Arc::new(db));

        let mut style = ComputedStyle::default();
        style.font_family = vec!["Webby".to_string(), roboto_family.clone()];
        style.font_size = 16.0;

        let pipeline = ShapingPipeline::new();
        let fallback_runs = match pipeline.shape("mmmm", &style, &ctx) {
            Ok(runs) => runs,
            Err(_) => return,
        };
        if fallback_runs.is_empty() {
            return;
        }
        let fallback_font = &fallback_runs[0].font;
        assert_eq!(fallback_font.family, roboto_family);

        let web_font_path = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/fonts/STIXTwoMath-Regular.otf");
        let web_url = Url::from_file_path(&web_font_path).expect("file url");
        let face = FontFaceRule {
            family: Some("Webby".to_string()),
            sources: vec![FontFaceSource::Url(web_url.to_string())],
            ..Default::default()
        };
        ctx.load_web_fonts(&[face], None).expect("load web font");

        let web_runs = match pipeline.shape("mmmm", &style, &ctx) {
            Ok(runs) => runs,
            Err(_) => return,
        };
        if web_runs.is_empty() {
            return;
        }
        let web_font = &web_runs[0].font;
        assert_eq!(web_font.family, "Webby");
        assert!(
            !Arc::ptr_eq(&web_font.data, &fallback_font.data)
                || (web_runs[0].advance - fallback_runs[0].advance).abs() > 0.1,
            "web font selection should invalidate cached shaping"
        );
    }

    #[test]
    fn font_optical_sizing_none_skips_opsz_axis() {
        let (ctx, family) = load_variable_font();
        let mut style = ComputedStyle::default();
        style.font_family = vec![family];
        style.font_size = 20.0;
        let runs_auto = assign_fonts(
            &[ItemizedRun {
                text: "mmmm".to_string(),
                start: 0,
                end: 4,
                script: Script::Latin,
                direction: Direction::LeftToRight,
                level: 0,
            }],
            &style,
            &ctx,
        )
        .expect("assign fonts auto");
        let opsz_tag = Tag::from_bytes(b"opsz");
        assert!(runs_auto[0].variations.iter().any(|v| v.tag == opsz_tag));

        style.font_optical_sizing = crate::style::types::FontOpticalSizing::None;
        let runs_none = assign_fonts(
            &[ItemizedRun {
                text: "mmmm".to_string(),
                start: 0,
                end: 4,
                script: Script::Latin,
                direction: Direction::LeftToRight,
                level: 0,
            }],
            &style,
            &ctx,
        )
        .expect("assign fonts none");
        assert!(runs_none[0].variations.iter().all(|v| v.tag != opsz_tag));
    }

    #[test]
    fn wght_axis_prevents_synthetic_bold() {
        let (ctx, family) = load_variable_font();
        let mut style = ComputedStyle::default();
        style.font_family = vec![family];
        style.font_weight = FontWeight::Number(900);

        let runs = ShapingPipeline::new()
            .shape("m", &style, &ctx)
            .expect("shape variable font");
        if runs.is_empty() {
            return;
        }
        assert!(
            runs.iter().all(|r| r.synthetic_bold.abs() < f32::EPSILON),
            "wght axis should satisfy requested weight without synthetic bolding"
        );
    }

    #[test]
    fn font_language_override_sets_language_tag() {
        let ctx = FontContext::new();
        let mut style = ComputedStyle::default();
        style.font_family = vec!["serif".to_string()];
        style.font_language_override = crate::style::types::FontLanguageOverride::Override("SRB".to_string());

        let runs = ShapingPipeline::new()
            .shape("text", &style, &ctx)
            .expect("shape with override");
        assert!(runs
            .iter()
            .all(|r| r.language.as_ref().map(|l| l.as_str()) == Some("srb")));
    }

    #[test]
    fn math_generic_prefers_math_fonts() {
        let font_path = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/fonts/STIXTwoMath-Regular.otf");
        let (math_data, math_family) = load_font_fixture(&font_path);

        let mut db = FontDatabase::empty();
        db.load_font_data(math_data).expect("load math font");
        let ctx = FontContext::with_database(Arc::new(db));

        let mut style = ComputedStyle::default();
        style.font_family = vec!["math".to_string()];
        style.font_size = 18.0;

        let runs = ShapingPipeline::new()
            .shape("∑", &style, &ctx)
            .expect("shape with math generic");
        if runs.is_empty() {
            return;
        }
        assert_eq!(runs[0].font.family, math_family);
    }

    #[test]
    fn math_generic_prefers_web_math_fonts() {
        let font_path = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/fonts/STIXTwoMath-Regular.otf");
        let font_url = Url::from_file_path(&font_path).expect("file url");
        let face = FontFaceRule {
            family: Some("WebMath".to_string()),
            sources: vec![FontFaceSource::Url(font_url.to_string())],
            ..Default::default()
        };

        let db = FontDatabase::empty();
        let ctx = FontContext::with_database(Arc::new(db));
        ctx.load_web_fonts(&[face], None).expect("load web math");

        let mut style = ComputedStyle::default();
        style.font_family = vec!["math".to_string()];
        style.font_size = 16.0;

        let runs = ShapingPipeline::new()
            .shape("∑", &style, &ctx)
            .expect("shape with math generic");
        if runs.is_empty() {
            return;
        }
        assert_eq!(runs[0].font.family, "WebMath");
    }

    #[test]
    fn math_generic_falls_back_without_math_fonts() {
        let roboto_path = Path::new(env!("CARGO_MANIFEST_DIR")).join("resources/fonts/Roboto-Regular.ttf");
        let (roboto_data, roboto_family) = load_font_fixture(&roboto_path);

        let mut db = FontDatabase::empty();
        db.load_font_data(roboto_data).expect("load fallback font");
        let ctx = FontContext::with_database(Arc::new(db));

        let mut style = ComputedStyle::default();
        style.font_family = vec!["math".to_string(), roboto_family.clone()];
        style.font_size = 16.0;

        let runs = ShapingPipeline::new()
            .shape("∑", &style, &ctx)
            .expect("shape without math font");
        if runs.is_empty() {
            return;
        }
        assert_eq!(runs[0].font.family, roboto_family);
    }

    #[test]
    fn unicode_range_limits_web_font_usage() {
        let font_path = Path::new(env!("CARGO_MANIFEST_DIR")).join("resources/fonts/Roboto-Regular.ttf");
        let font_url = Url::from_file_path(&font_path).expect("file url");
        let face = FontFaceRule {
            family: Some("RangeFace".to_string()),
            sources: vec![FontFaceSource::Url(font_url.to_string())],
            unicode_ranges: vec![(0x0041, 0x005A)],
            ..Default::default()
        };

        let ctx = FontContext::new();
        ctx.load_web_fonts(&[face], None).expect("load web font");

        let families = vec![
            FamilyEntry::Named("RangeFace".to_string()),
            FamilyEntry::Generic(GenericFamily::SansSerif),
        ];

        let mut picker = FontPreferencePicker::new(EmojiPreference::Neutral);
        let upper = resolve_font_for_char(
            'A',
            &families,
            400,
            DbFontStyle::Normal,
            None,
            DbFontStretch::Normal,
            &ctx,
            &mut picker,
        )
        .expect("font for uppercase");
        assert_eq!(upper.family, "RangeFace");

        let mut picker = FontPreferencePicker::new(EmojiPreference::Neutral);
        let lower = resolve_font_for_char(
            'a',
            &families,
            400,
            DbFontStyle::Normal,
            None,
            DbFontStretch::Normal,
            &ctx,
            &mut picker,
        )
        .expect("fallback font");
        assert_ne!(lower.family, "RangeFace");
    }

    #[test]
    fn font_stretch_maps_to_wdth_axis_when_present() {
        let (ctx, family) = load_variable_font();
        let mut style = ComputedStyle::default();
        style.font_family = vec![family.clone()];
        style.font_size = 16.0;

        let pipeline = ShapingPipeline::new();

        let base = pipeline.shape("mmmm", &style, &ctx).unwrap();
        let base_adv: f32 = base.iter().map(|r| r.advance).sum();

        style.font_stretch = FontStretch::from_percentage(75.0);
        let narrow = pipeline.shape("mmmm", &style, &ctx).unwrap();
        let narrow_adv: f32 = narrow.iter().map(|r| r.advance).sum();

        style.font_stretch = FontStretch::from_percentage(125.0);
        let wide = pipeline.shape("mmmm", &style, &ctx).unwrap();
        let wide_adv: f32 = wide.iter().map(|r| r.advance).sum();

        assert!(
            narrow_adv < base_adv,
            "narrow stretch should reduce advance with wdth axis"
        );
        assert!(
            wide_adv > base_adv,
            "wide stretch should increase advance with wdth axis"
        );
        assert!(
            (wide_adv - narrow_adv) > 1.0,
            "wdth axis should produce a noticeable difference"
        );
    }

    #[test]
    fn authored_font_variations_override_auto_axes() {
        let (ctx, family) = load_variable_font();
        let mut style = ComputedStyle::default();
        style.font_family = vec![family.clone()];
        style.font_size = 16.0;

        let pipeline = ShapingPipeline::new();

        style.font_stretch = FontStretch::from_percentage(75.0);
        style.font_variation_settings = vec![FontVariationSetting {
            tag: *b"wdth",
            value: 100.0,
        }];
        let forced = pipeline.shape("mmmm", &style, &ctx).unwrap();
        let forced_adv: f32 = forced.iter().map(|r| r.advance).sum();

        style.font_variation_settings.clear();
        style.font_stretch = FontStretch::Normal;
        let baseline = pipeline.shape("mmmm", &style, &ctx).unwrap();
        let baseline_adv: f32 = baseline.iter().map(|r| r.advance).sum();

        assert!(
            (forced_adv - baseline_adv).abs() < 0.25,
            "authored wdth should override font-stretch mapping"
        );
    }

    #[test]
    fn oblique_angle_maps_to_slnt_axis_with_clamp() {
        let (ctx, family) = load_variable_font();
        let mut style = ComputedStyle::default();
        style.font_family = vec![family.clone()];
        style.font_size = 16.0;
        style.font_style = CssFontStyle::Oblique(Some(20.0));

        let runs = assign_fonts(
            &[ItemizedRun {
                text: "mmmm".to_string(),
                start: 0,
                end: 4,
                script: Script::Latin,
                direction: Direction::LeftToRight,
                level: 0,
            }],
            &style,
            &ctx,
        )
        .expect("assign fonts");
        let slnt_tag = Tag::from_bytes(b"slnt");
        let slnt_value = runs[0]
            .variations
            .iter()
            .find(|v| v.tag == slnt_tag)
            .map(|v| v.value)
            .expect("slnt axis should be populated for oblique styles");

        let font_path = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/fonts/RobotoFlex-VF.ttf");
        let data = fs::read(&font_path).expect("fixture font data");
        let face = ttf_parser::Face::parse(&data, 0).expect("parse fixture font");
        let axis = face
            .variation_axes()
            .into_iter()
            .find(|a| a.tag == slnt_tag)
            .expect("fixture font exposes slnt axis");
        let expected = (-20.0_f32).clamp(axis.min_value, axis.max_value);
        assert!((slnt_value - expected).abs() < 0.001);
    }

    #[test]
    fn authored_slnt_variation_preserves_authored_value() {
        let (ctx, family) = load_variable_font();
        let mut style = ComputedStyle::default();
        style.font_family = vec![family];
        style.font_size = 16.0;
        style.font_style = CssFontStyle::Oblique(Some(10.0));
        style.font_variation_settings = vec![FontVariationSetting {
            tag: *b"slnt",
            value: -5.0,
        }];

        let runs = assign_fonts(
            &[ItemizedRun {
                text: "mmmm".to_string(),
                start: 0,
                end: 4,
                script: Script::Latin,
                direction: Direction::LeftToRight,
                level: 0,
            }],
            &style,
            &ctx,
        )
        .expect("assign fonts");
        let slnt_tag = Tag::from_bytes(b"slnt");
        let slnt_value = runs[0]
            .variations
            .iter()
            .find(|v| v.tag == slnt_tag)
            .map(|v| v.value)
            .expect("authored slnt should survive auto mapping");

        assert!((slnt_value + 5.0).abs() < 0.001);
    }

    #[test]
    fn oblique_prefers_slnt_axis_over_ital() {
        let (ctx, family) = load_variable_font();
        let mut style = ComputedStyle::default();
        style.font_family = vec![family.clone()];
        style.font_size = 16.0;
        style.font_style = CssFontStyle::Oblique(Some(12.0));

        let runs = assign_fonts(
            &[ItemizedRun {
                text: "mmmm".to_string(),
                start: 0,
                end: 4,
                script: Script::Latin,
                direction: Direction::LeftToRight,
                level: 0,
            }],
            &style,
            &ctx,
        )
        .expect("assign fonts");

        let slnt_tag = Tag::from_bytes(b"slnt");
        let ital_tag = Tag::from_bytes(b"ital");
        let tags: Vec<_> = runs[0].variations.iter().map(|v| v.tag).collect();
        assert!(tags.contains(&slnt_tag), "slnt axis should be populated");
        let font_path = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/fonts/RobotoFlex-VF.ttf");
        let data = fs::read(&font_path).expect("fixture font data");
        let face = ttf_parser::Face::parse(&data, 0).expect("parse fixture font");
        let has_ital_axis = face.variation_axes().into_iter().any(|a| a.tag == ital_tag);
        if has_ital_axis {
            assert!(
                !tags.contains(&ital_tag),
                "ital axis should not be set when slnt axis is available for oblique"
            );
        }
        let axis = face
            .variation_axes()
            .into_iter()
            .find(|a| a.tag == slnt_tag)
            .expect("fixture font exposes slnt axis");
        let slnt_value = runs[0]
            .variations
            .iter()
            .find(|v| v.tag == slnt_tag)
            .map(|v| v.value)
            .unwrap();
        let expected = (-12.0_f32).clamp(axis.min_value, axis.max_value);
        assert!((slnt_value - expected).abs() < 0.001);
    }

    #[test]
    fn italic_maps_to_available_axis() {
        let (ctx, family) = load_variable_font();
        let mut style = ComputedStyle::default();
        style.font_family = vec![family.clone()];
        style.font_size = 16.0;
        style.font_style = CssFontStyle::Italic;

        let runs = assign_fonts(
            &[ItemizedRun {
                text: "mmmm".to_string(),
                start: 0,
                end: 4,
                script: Script::Latin,
                direction: Direction::LeftToRight,
                level: 0,
            }],
            &style,
            &ctx,
        )
        .expect("assign fonts");
        let ital_tag = Tag::from_bytes(b"ital");
        let slnt_tag = Tag::from_bytes(b"slnt");
        let font_path = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/fonts/RobotoFlex-VF.ttf");
        let data = fs::read(&font_path).expect("fixture font data");
        let face = ttf_parser::Face::parse(&data, 0).expect("parse fixture font");
        let has_ital_axis = face.variation_axes().into_iter().any(|a| a.tag == ital_tag);

        if has_ital_axis {
            let ital_value = runs[0].variations.iter().find(|v| v.tag == ital_tag).map(|v| v.value);
            assert_eq!(ital_value, Some(1.0));
            assert!(
                runs[0].variations.iter().all(|v| v.tag != slnt_tag),
                "italic should not use slnt when ital axis is present"
            );
        } else {
            let slnt_value = runs[0]
                .variations
                .iter()
                .find(|v| v.tag == slnt_tag)
                .map(|v| v.value)
                .expect("italic should map to slnt when ital axis is absent");
            let axis = face
                .variation_axes()
                .into_iter()
                .find(|a| a.tag == slnt_tag)
                .expect("fixture font exposes slnt axis");
            let expected = (-DEFAULT_OBLIQUE_ANGLE_DEG).clamp(axis.min_value, axis.max_value);
            assert!((slnt_value - expected).abs() < 0.001);
        }
    }

    #[test]
    fn slnt_axis_disables_synthetic_slant() {
        let (ctx, family) = load_variable_font();
        let mut style = ComputedStyle::default();
        style.font_family = vec![family.clone()];
        style.font_size = 16.0;
        style.font_style = CssFontStyle::Oblique(Some(10.0));

        let font = ctx
            .get_font_full(
                &style.font_family,
                style.font_weight.to_u16(),
                DbFontStyle::Normal,
                DbFontStretch::from_percentage(style.font_stretch.to_percentage()),
            )
            .expect("load font with slnt axis");
        let (_, synthetic_slant) = compute_synthetic_styles(&style, &font);
        assert_eq!(synthetic_slant, 0.0, "slnt axis should satisfy slant without synthesis");
    }

    #[test]
    fn slope_preferences_follow_css_slope_order() {
        use crate::text::font_db::FontStyle as DbStyle;
        assert_eq!(slope_preference_order(DbStyle::Normal), &[DbStyle::Normal]);
        assert_eq!(
            slope_preference_order(DbStyle::Italic),
            &[DbStyle::Italic, DbStyle::Oblique, DbStyle::Normal]
        );
        assert_eq!(
            slope_preference_order(DbStyle::Oblique),
            &[DbStyle::Oblique, DbStyle::Italic, DbStyle::Normal]
        );
    }

    #[test]
    fn weight_preferences_prefer_closest_with_bias() {
        let order_350 = weight_preference_order(350);
        assert!(order_350.iter().position(|w| *w == 300) < order_350.iter().position(|w| *w == 400));
        let order_600 = weight_preference_order(600);
        assert!(order_600.iter().position(|w| *w == 700) < order_600.iter().position(|w| *w == 500));
        let order_450 = weight_preference_order(450);
        assert!(
            order_450.iter().position(|w| *w == 500) < order_450.iter().position(|w| *w == 400),
            "for weights between 400-500, heavier weights up to 500 should be preferred first"
        );
    }

    #[test]
    fn stretch_preferences_follow_css_ordering() {
        use crate::text::font_db::FontStretch as DbStretch;
        let order_narrow = stretch_preference_order(DbStretch::SemiCondensed);
        assert!(
            order_narrow.iter().position(|s| *s == DbStretch::Condensed)
                < order_narrow.iter().position(|s| *s == DbStretch::Normal),
            "when desired stretch is below 100%, narrower widths are tried before wider ones"
        );

        let order_wide = stretch_preference_order(DbStretch::Expanded);
        assert!(
            order_wide.iter().position(|s| *s == DbStretch::ExtraExpanded)
                < order_wide.iter().position(|s| *s == DbStretch::Normal),
            "when desired stretch is above 100%, wider widths are tried before narrower ones"
        );
    }

    #[test]
    fn vertical_sideways_text_marks_runs_rotated() {
        let mut style = ComputedStyle::default();
        style.writing_mode = crate::style::types::WritingMode::VerticalRl;
        style.text_orientation = crate::style::types::TextOrientation::Sideways;
        let ctx = FontContext::new();
        let shaped = ShapingPipeline::new().shape("Abc", &style, &ctx).unwrap();
        assert!(shaped.iter().all(|r| r.rotation == RunRotation::Cw90));
    }

    #[test]
    fn sideways_writing_rotates_all_runs_regardless_of_text_orientation() {
        let mut style = ComputedStyle::default();
        style.writing_mode = crate::style::types::WritingMode::SidewaysRl;
        style.text_orientation = crate::style::types::TextOrientation::Upright;
        let ctx = FontContext::new();
        let shaped = ShapingPipeline::new().shape("Abc本", &style, &ctx).unwrap();
        assert!(
            shaped.iter().all(|r| r.rotation == RunRotation::Cw90),
            "sideways writing should set horizontal typographic mode and rotate text regardless of text-orientation"
        );
    }

    #[test]
    fn sideways_writing_uses_horizontal_metrics() {
        let mut style = ComputedStyle::default();
        style.writing_mode = crate::style::types::WritingMode::SidewaysLr;
        style.text_orientation = crate::style::types::TextOrientation::Mixed;
        let ctx = FontContext::new();
        let shaped = ShapingPipeline::new().shape("Abc", &style, &ctx).unwrap();
        assert!(
            shaped.iter().all(|r| r.rotation == RunRotation::Cw90),
            "sideways writing should rotate text using horizontal metrics regardless of text-orientation"
        );
    }

    #[test]
    fn upright_in_vertical_forces_ltr_bidi_and_preserves_order() {
        let mut style = ComputedStyle::default();
        style.direction = crate::style::types::Direction::Rtl;
        style.writing_mode = crate::style::types::WritingMode::VerticalRl;
        style.text_orientation = crate::style::types::TextOrientation::Upright;

        let text = "אבג abc";
        let ctx = FontContext::new();
        let shaped = ShapingPipeline::new().shape(text, &style, &ctx).unwrap();
        assert!(
            shaped.iter().all(|r| r.direction == Direction::LeftToRight),
            "upright should force LTR direction for all runs in vertical text"
        );
        let reconstructed: String = shaped.iter().flat_map(|r| r.text.chars()).collect();
        assert_eq!(
            reconstructed, text,
            "upright should avoid bidi reordering and preserve textual order"
        );
    }

    #[test]
    fn vertical_mixed_rotates_non_upright_segments() {
        let mut style = ComputedStyle::default();
        style.writing_mode = WritingMode::VerticalRl;
        style.text_orientation = TextOrientation::Mixed;
        let ctx = FontContext::new();
        let shaped = ShapingPipeline::new().shape("A。B本", &style, &ctx).unwrap();
        assert!(shaped.iter().any(|r| r.rotation == RunRotation::Cw90));
        assert!(shaped.iter().any(|r| r.rotation == RunRotation::None));
    }

    #[test]
    fn vertical_upright_forces_all_runs_upright() {
        let mut style = ComputedStyle::default();
        style.writing_mode = WritingMode::VerticalLr;
        style.text_orientation = TextOrientation::Upright;
        let ctx = FontContext::new();
        let shaped = ShapingPipeline::new().shape("Abc本", &style, &ctx).unwrap();
        assert!(shaped.iter().all(|r| r.rotation == RunRotation::None));
    }

    #[test]
    fn collect_features_respects_ligature_variants_and_overrides() {
        let mut style = ComputedStyle::default();
        style.font_variant_ligatures = FontVariantLigatures {
            common: false,
            discretionary: true,
            historical: false,
            contextual: false,
        };
        style.font_feature_settings = vec![FontFeatureSetting {
            tag: *b"liga",
            value: 1,
        }];

        let feats = collect_opentype_features(&style);
        let mut seen: std::collections::HashMap<[u8; 4], u32> = std::collections::HashMap::new();
        for f in feats {
            seen.insert(f.tag.to_bytes(), f.value);
        }
        assert_eq!(seen.get(b"liga"), Some(&1));
        assert_eq!(seen.get(b"clig"), Some(&0));
        assert_eq!(seen.get(b"dlig"), Some(&1));
        assert_eq!(seen.get(b"calt"), Some(&0));
    }

    #[test]
    fn collect_features_includes_numeric_variants_and_kerning() {
        let mut style = ComputedStyle::default();
        style.font_variant_numeric.figure = NumericFigure::Oldstyle;
        style.font_variant_numeric.spacing = NumericSpacing::Tabular;
        style.font_variant_numeric.fraction = NumericFraction::Stacked;
        style.font_variant_numeric.ordinal = true;
        style.font_variant_numeric.slashed_zero = true;
        style.font_kerning = FontKerning::None;
        style.font_variant_east_asian.variant = Some(EastAsianVariant::Jis04);
        style.font_variant_east_asian.width = Some(EastAsianWidth::FullWidth);
        style.font_variant_east_asian.ruby = true;
        style.font_variant_position = FontVariantPosition::Super;

        let feats = collect_opentype_features(&style);
        let mut seen: std::collections::HashMap<[u8; 4], u32> = std::collections::HashMap::new();
        for f in feats {
            seen.insert(f.tag.to_bytes(), f.value);
        }
        assert_eq!(seen.get(b"onum"), Some(&1));
        assert_eq!(seen.get(b"tnum"), Some(&1));
        assert_eq!(seen.get(b"afrc"), Some(&1));
        assert_eq!(seen.get(b"ordn"), Some(&1));
        assert_eq!(seen.get(b"zero"), Some(&1));
        assert_eq!(seen.get(b"kern"), Some(&0));
        assert_eq!(seen.get(b"jp04"), Some(&1));
        assert_eq!(seen.get(b"fwid"), Some(&1));
        assert_eq!(seen.get(b"ruby"), Some(&1));
        assert_eq!(seen.get(b"sups"), Some(&1));
    }

    #[test]
    fn collect_features_includes_caps_variants() {
        let mut style = ComputedStyle::default();
        style.font_variant_caps = FontVariantCaps::AllSmallCaps;

        let feats = collect_opentype_features(&style);
        let mut seen: std::collections::HashMap<[u8; 4], u32> = std::collections::HashMap::new();
        for f in feats {
            seen.insert(f.tag.to_bytes(), f.value);
        }
        assert_eq!(seen.get(b"smcp"), Some(&1));
        assert_eq!(seen.get(b"c2sc"), Some(&1));

        style.font_variant_caps = FontVariantCaps::AllPetiteCaps;
        let feats = collect_opentype_features(&style);
        let mut seen: std::collections::HashMap<[u8; 4], u32> = std::collections::HashMap::new();
        for f in feats {
            seen.insert(f.tag.to_bytes(), f.value);
        }
        assert_eq!(seen.get(b"pcap"), Some(&1));
        assert_eq!(seen.get(b"c2pc"), Some(&1));

        style.font_variant_caps = FontVariantCaps::Unicase;
        let feats = collect_opentype_features(&style);
        assert!(feats.iter().any(|f| f.tag.to_bytes() == *b"unic"));
    }

    #[test]
    fn collect_features_includes_alternates() {
        let mut style = ComputedStyle::default();
        style.font_variant_alternates.historical_forms = true;
        style.font_variant_alternates.stylistic = Some(3);
        style.font_variant_alternates.stylesets = vec![1, 2];
        style.font_variant_alternates.character_variants = vec![4];
        style.font_variant_alternates.swash = Some(1);
        style.font_variant_alternates.ornaments = Some(2);

        let feats = collect_opentype_features(&style);
        let mut seen: std::collections::HashMap<[u8; 4], u32> = std::collections::HashMap::new();
        for f in feats {
            seen.insert(f.tag.to_bytes(), f.value);
        }
        assert_eq!(seen.get(b"hist"), Some(&1));
        assert_eq!(seen.get(b"ss03"), Some(&1));
        assert_eq!(seen.get(b"ss01"), Some(&1));
        assert_eq!(seen.get(b"ss02"), Some(&1));
        assert_eq!(seen.get(b"cv04"), Some(&1));
        assert_eq!(seen.get(b"swsh"), Some(&1));
        assert_eq!(seen.get(b"ornm"), Some(&2));
    }

    #[test]
    fn emoji_dominant_detection_respects_non_emoji_content() {
        assert!(is_emoji_dominant("😀 😀"));
        assert!(is_emoji_dominant("😀\u{200d}\u{1f9d1}"));
        assert!(!is_emoji_dominant("😀a"));
    }

    #[test]
    fn emoji_font_classifier_matches_family_names() {
        let emoji_font = LoadedFont {
            data: Arc::new(Vec::new()),
            index: 0,
            family: "Noto Color Emoji".into(),
            weight: DbFontWeight::NORMAL,
            style: DbFontStyle::Normal,
            stretch: DbFontStretch::Normal,
        };
        let text_font = LoadedFont {
            data: Arc::new(Vec::new()),
            index: 0,
            family: "Roboto".into(),
            weight: DbFontWeight::NORMAL,
            style: DbFontStyle::Normal,
            stretch: DbFontStretch::Normal,
        };
        assert!(font_is_emoji_font(&emoji_font));
        assert!(!font_is_emoji_font(&text_font));
    }

    #[test]
    fn variation_selector_stays_with_current_font_run() {
        let ctx = FontContext::new();
        let style = ComputedStyle::default();
        let runs = assign_fonts(
            &[ItemizedRun {
                text: "A\u{fe0f}".to_string(),
                start: 0,
                end: "A\u{fe0f}".len(),
                script: Script::Latin,
                direction: Direction::LeftToRight,
                level: 0,
            }],
            &style,
            &ctx,
        )
        .expect("assign fonts");
        assert_eq!(runs.len(), 1, "variation selector should not split font runs");
        assert_eq!(runs[0].text, "A\u{fe0f}");
    }

    #[test]
    fn emoji_preference_matches_variant_and_character() {
        assert_eq!(
            emoji_preference_for_char('😀', FontVariantEmoji::Emoji),
            EmojiPreference::PreferEmoji
        );
        assert_eq!(
            emoji_preference_for_char('😀', FontVariantEmoji::Text),
            EmojiPreference::AvoidEmoji
        );
        assert_eq!(
            emoji_preference_for_char('😀', FontVariantEmoji::Unicode),
            EmojiPreference::PreferEmoji
        );
        // '#' defaults to text presentation.
        assert_eq!(
            emoji_preference_for_char('#', FontVariantEmoji::Unicode),
            EmojiPreference::AvoidEmoji
        );
    }

    #[test]
    fn emoji_variation_selectors_override_property_preference() {
        assert_eq!(
            emoji_preference_with_selector('😀', Some('\u{fe0e}'), FontVariantEmoji::Emoji),
            EmojiPreference::AvoidEmoji
        );
        assert_eq!(
            emoji_preference_with_selector('😀', Some('\u{fe0f}'), FontVariantEmoji::Text),
            EmojiPreference::PreferEmoji
        );
    }

    #[test]
    fn zwj_sequences_prefer_emoji_fonts() {
        assert_eq!(
            emoji_preference_with_selector('👩', Some('\u{200d}'), FontVariantEmoji::Text),
            EmojiPreference::PreferEmoji
        );
    }

    fn dummy_font(name: &str) -> LoadedFont {
        LoadedFont {
            data: Arc::new(Vec::new()),
            index: 0,
            family: name.into(),
            weight: DbFontWeight::NORMAL,
            style: DbFontStyle::Normal,
            stretch: DbFontStretch::Normal,
        }
    }

    #[test]
    fn emoji_preference_picker_prefers_emoji_when_available() {
        let text_font = dummy_font("Example Text");
        let emoji_font = dummy_font("Noto Color Emoji");
        let mut picker = FontPreferencePicker::new(EmojiPreference::PreferEmoji);
        let idx = picker.bump_order();
        assert!(picker
            .consider(text_font.clone(), font_is_emoji_font(&text_font), idx)
            .is_none());
        let idx = picker.bump_order();
        let chosen = picker
            .consider(emoji_font.clone(), font_is_emoji_font(&emoji_font), idx)
            .expect("should pick emoji font");
        assert_eq!(chosen.family, emoji_font.family);
    }

    #[test]
    fn emoji_preference_picker_falls_back_to_text_when_no_emoji_font() {
        let text_font = dummy_font("Example Text");
        let mut picker = FontPreferencePicker::new(EmojiPreference::PreferEmoji);
        let idx = picker.bump_order();
        assert!(picker
            .consider(text_font.clone(), font_is_emoji_font(&text_font), idx)
            .is_none());
        let chosen = picker.finish().expect("fallback text font");
        assert_eq!(chosen.family, text_font.family);
    }

    #[test]
    fn emoji_preference_picker_avoids_emoji_but_allows_fallback() {
        let emoji_font = dummy_font("Twemoji");
        let mut picker = FontPreferencePicker::new(EmojiPreference::AvoidEmoji);
        let idx = picker.bump_order();
        assert!(picker
            .consider(emoji_font.clone(), font_is_emoji_font(&emoji_font), idx)
            .is_none());
        let chosen = picker.finish().expect("fallback emoji font");
        assert_eq!(chosen.family, emoji_font.family);
    }
}
fn number_tag(prefix: &[u8; 2], n: u8) -> Option<[u8; 4]> {
    if n == 0 {
        return None;
    }
    let mut tag = [b' ', b' ', b' ', b' '];
    tag[0] = prefix[0];
    tag[1] = prefix[1];
    let tens = (n / 10) % 10;
    let ones = n % 10;
    tag[2] = b'0' + tens;
    tag[3] = b'0' + ones;
    Some(tag)
}
