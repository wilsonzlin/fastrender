# Phase 3: Text Shaping

**Duration:** Week 2 of Phase 3 (7-10 days)
**Prerequisites:**
- Font system complete (03-font-system.md)
- Font loading and metrics working
**Dependencies:**
- rustybuzz crate for HarfBuzz-compatible shaping
- unicode-bidi for bidirectional text algorithm
- unicode-script for script detection
- FontContext and FontFace types
**Output:** Complete text shaping pipeline with bidi, script itemization, and complex script support

## Objectives

Implement a comprehensive text shaping system that converts Unicode text strings into positioned glyphs, handling:

- **Unicode Bidirectional Algorithm (UAX #9)** - Right-to-left text (Arabic, Hebrew)
- **Script Itemization** - Identifying runs of text in the same script
- **Text Shaping with HarfBuzz** - Complex scripts, ligatures, contextual forms
- **Emoji and Color Fonts** - Support for emoji and color font formats
- **Glyph Positioning** - Accurate glyph advances and positioning

This is **critical for international text support** and proper rendering of any non-trivial text.

## Context

Text shaping is the process of converting a string of Unicode codepoints into positioned glyphs:

**Input:**
`"Hello"` → `[U+0048, U+0065, U+006C, U+006C, U+006F]`

**Output:**
`[Glyph(id=72, x=0.0, advance=8.5), Glyph(id=101, x=8.5, advance=7.2), ...]`

**Why is this complex?**

1. **One-to-many mapping:** Single character → multiple glyphs (e.g., ä → a + combining diacritic)
2. **Many-to-one mapping:** Multiple characters → single glyph (ligatures: fi → ﬁ)
3. **Contextual shaping:** Glyph shape depends on neighbors (Arabic letters)
4. **Bidirectional text:** Mix of LTR and RTL (English + Arabic)
5. **Complex scripts:** Devanagari, Thai, etc. have complex positioning rules

**From Unicode Standard:**
> "The Unicode Bidirectional Algorithm (UAX #9) specifies how to resolve the directional properties of text containing characters from scripts written left-to-right, right-to-left, and mixed bidirectional text."

## The Problem V1 Has

V1 has no text shaping:
- Cannot render RTL text (Arabic, Hebrew)
- No ligature support
- No support for complex scripts (Devanagari, Thai, etc.)
- Simple character-by-character layout only
- No proper handling of combining marks
- Cannot render emoji properly

This makes V1 completely unusable for international text.

## The Solution

Implement a multi-stage text shaping pipeline:

1. **Unicode Bidirectional Algorithm** - Reorder text for RTL
2. **Script Itemization** - Split text into script runs
3. **Font Itemization** - Assign fonts to each run (with fallback)
4. **Text Shaping** - Use rustybuzz to shape each run
5. **Glyph Assembly** - Combine shaped runs into final glyph sequence

Use battle-tested libraries:
- **rustybuzz** - Pure Rust HarfBuzz implementation
- **unicode-bidi** - UAX #9 implementation
- **unicode-script** - Script detection

## CSS/Unicode Specification References

**Primary:**
- **Unicode Bidirectional Algorithm (UAX #9):**
  - https://www.unicode.org/reports/tr9/
- **Unicode Line Breaking Algorithm (UAX #14):**
  - https://www.unicode.org/reports/tr14/
- **Unicode Text Segmentation (UAX #29):**
  - https://www.unicode.org/reports/tr29/
- **OpenType Specification:**
  - https://learn.microsoft.com/en-us/typography/opentype/spec/

**CSS References:**
- **CSS Writing Modes Level 4:**
  - https://www.w3.org/TR/css-writing-modes-4/
- **CSS Text Module Level 3:**
  - https://www.w3.org/TR/css-text-3/

**Key Concepts:**
- **Bidi levels** - Embedding levels for text direction
- **Script runs** - Contiguous text in same script
- **Shaping** - Converting characters to glyphs with positioning
- **Clusters** - Mapping from characters to glyphs

## Step-by-Step Implementation

### Step 1: Create Text Shaping Module (Day 1 Morning)

```bash
mkdir -p /home/user/fastrender/src/text/shaping
touch /home/user/fastrender/src/text/shaping/mod.rs
touch /home/user/fastrender/src/text/shaping/bidi.rs
touch /home/user/fastrender/src/text/shaping/itemize.rs
touch /home/user/fastrender/src/text/shaping/shaper.rs
touch /home/user/fastrender/src/text/shaping/emoji.rs
```

**File: `Cargo.toml` (add dependencies)**

```toml
[dependencies]
# Text shaping
rustybuzz = "0.11"
unicode-bidi = "0.3"
unicode-script = "0.5"
unicode-properties = "0.1"

# Emoji detection
unic-emoji-char = "0.9"
```

**File: `src/text/shaping/mod.rs`**

```rust
//! Text shaping
//!
//! Converts Unicode text to positioned glyphs using HarfBuzz (via rustybuzz).
//!
//! Pipeline:
//! 1. Bidirectional reordering (UAX #9)
//! 2. Script itemization
//! 3. Font itemization (with fallback)
//! 4. Text shaping (HarfBuzz)
//! 5. Glyph assembly

pub mod bidi;
pub mod itemize;
pub mod shaper;
pub mod emoji;

pub use bidi::BidiAnalyzer;
pub use itemize::{ScriptItemizer, ItemizedRun};
pub use shaper::{TextShaper, ShapedText, GlyphInfo, GlyphCluster};
pub use emoji::EmojiDetector;

use crate::text::font::{FontFace, FontLoader};
use crate::style::ComputedStyle;
use crate::error::Result;
use std::sync::Arc;

/// Text shaping pipeline
///
/// Main entry point for text shaping.
pub struct ShapingPipeline {
    bidi: BidiAnalyzer,
    itemizer: ScriptItemizer,
    shaper: TextShaper,
}

impl ShapingPipeline {
    pub fn new() -> Self {
        Self {
            bidi: BidiAnalyzer::new(),
            itemizer: ScriptItemizer::new(),
            shaper: TextShaper::new(),
        }
    }

    /// Shape text with style
    ///
    /// This is the main method called from inline layout.
    pub fn shape(
        &self,
        text: &str,
        style: &ComputedStyle,
        font_loader: &FontLoader,
    ) -> Result<Vec<ShapedText>> {
        // Step 1: Bidirectional analysis
        let bidi_info = self.bidi.analyze(text, style)?;

        // Step 2: Script itemization
        // This splits text into runs of the same script
        let script_runs = self.itemizer.itemize(text, &bidi_info)?;

        // Step 3: Font itemization
        // Assign fonts to each run (with fallback for missing glyphs)
        let font_runs = self.assign_fonts(&script_runs, style, font_loader)?;

        // Step 4: Shape each run
        let mut shaped_runs = Vec::new();
        for run in font_runs {
            let shaped = self.shaper.shape_run(&run)?;
            shaped_runs.push(shaped);
        }

        // Step 5: Reorder for bidi if needed
        if bidi_info.needs_reordering() {
            shaped_runs = self.bidi.reorder_shaped_runs(shaped_runs, &bidi_info)?;
        }

        Ok(shaped_runs)
    }

    /// Assign fonts to script runs
    fn assign_fonts(
        &self,
        runs: &[ItemizedRun],
        style: &ComputedStyle,
        font_loader: &FontLoader,
    ) -> Result<Vec<FontRun>> {
        let mut font_runs = Vec::new();

        for run in runs {
            // Try to load font for this run
            let font = font_loader.load_from_family_list(
                &style.font_family,
                style.font_weight.to_number(),
                style.font_style,
                style.font_stretch,
            );

            if let Some(font) = font {
                font_runs.push(FontRun {
                    text: run.text.clone(),
                    font: Arc::new(font),
                    script: run.script,
                    direction: run.direction,
                    level: run.level,
                });
            } else {
                return Err(crate::error::Error::Font(
                    format!("No font found for text: {}", run.text)
                ));
            }
        }

        Ok(font_runs)
    }
}

/// A run of text with assigned font
#[derive(Debug, Clone)]
pub struct FontRun {
    pub text: String,
    pub font: Arc<FontFace>,
    pub script: unicode_script::Script,
    pub direction: Direction,
    pub level: u8,
}

/// Text direction
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Direction {
    LeftToRight,
    RightToLeft,
}

impl Default for ShapingPipeline {
    fn default() -> Self {
        Self::new()
    }
}
```

### Step 2: Implement Bidirectional Algorithm (Day 1-2)

**File: `src/text/shaping/bidi.rs`**

```rust
//! Bidirectional text support
//!
//! Implements Unicode Bidirectional Algorithm (UAX #9)
//! https://www.unicode.org/reports/tr9/

use super::Direction;
use crate::style::ComputedStyle;
use crate::error::Result;
use unicode_bidi::{BidiInfo, Level};

/// Bidirectional analyzer
pub struct BidiAnalyzer;

impl BidiAnalyzer {
    pub fn new() -> Self {
        Self
    }

    /// Analyze text for bidirectional properties
    ///
    /// UAX #9: The Unicode Bidirectional Algorithm
    pub fn analyze(&self, text: &str, style: &ComputedStyle) -> Result<BidiAnalysis> {
        // Get base direction from CSS
        let base_level = self.css_direction_to_level(style);

        // Run Unicode bidi algorithm
        let bidi_info = BidiInfo::new(text, Some(base_level));

        // Check if reordering is needed
        let needs_reordering = bidi_info.levels.iter().any(|&level| level.number() > 0);

        Ok(BidiAnalysis {
            bidi_info,
            base_level,
            needs_reordering,
        })
    }

    /// Reorder visual runs
    ///
    /// After shaping, we may need to reorder runs for display.
    pub fn reorder_shaped_runs<T>(
        &self,
        runs: Vec<T>,
        analysis: &BidiAnalysis,
    ) -> Result<Vec<T>> {
        if !analysis.needs_reordering {
            return Ok(runs);
        }

        // Get visual ordering
        let line = analysis.bidi_info.reorder_line(
            &analysis.bidi_info.levels,
            0..analysis.bidi_info.text.len(),
        );

        // Reorder runs according to visual order
        // TODO: Implement proper reordering based on line.runs()

        Ok(runs)
    }

    /// Convert CSS direction to bidi level
    fn css_direction_to_level(&self, style: &ComputedStyle) -> Level {
        // CSS direction property determines base direction
        match style.direction {
            crate::style::Direction::Ltr => Level::ltr(),
            crate::style::Direction::Rtl => Level::rtl(),
        }
    }

    /// Get direction for a level
    pub fn level_to_direction(level: Level) -> Direction {
        if level.is_ltr() {
            Direction::LeftToRight
        } else {
            Direction::RightToLeft
        }
    }
}

/// Result of bidirectional analysis
#[derive(Debug)]
pub struct BidiAnalysis {
    /// Bidi information from unicode-bidi
    pub bidi_info: BidiInfo<'static>,

    /// Base embedding level
    pub base_level: Level,

    /// Whether reordering is needed
    pub needs_reordering: bool,
}

impl BidiAnalysis {
    /// Check if reordering is needed
    pub fn needs_reordering(&self) -> bool {
        self.needs_reordering
    }

    /// Get embedding level at character index
    pub fn level_at(&self, index: usize) -> Level {
        self.bidi_info.levels.get(index).copied()
            .unwrap_or(self.base_level)
    }

    /// Get direction at character index
    pub fn direction_at(&self, index: usize) -> Direction {
        BidiAnalyzer::level_to_direction(self.level_at(index))
    }
}

impl Default for BidiAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}
```

### Step 3: Implement Script Itemization (Day 2-3)

**File: `src/text/shaping/itemize.rs`**

```rust
//! Script itemization
//!
//! Splits text into runs of the same script.
//! Required for proper font selection and text shaping.

use super::{Direction, bidi::BidiAnalysis};
use crate::error::Result;
use unicode_script::{Script, UnicodeScript};

/// Script itemizer
pub struct ScriptItemizer;

impl ScriptItemizer {
    pub fn new() -> Self {
        Self
    }

    /// Itemize text into script runs
    ///
    /// Each run contains text in a single script (Latin, Arabic, etc.)
    /// with consistent directionality.
    pub fn itemize(&self, text: &str, bidi: &BidiAnalysis) -> Result<Vec<ItemizedRun>> {
        let mut runs = Vec::new();
        let mut current_run: Option<RunBuilder> = None;

        for (idx, ch) in text.char_indices() {
            let script = ch.script();
            let direction = bidi.direction_at(idx);
            let level = bidi.level_at(idx);

            // Check if we need to start a new run
            let needs_new_run = current_run.as_ref().map_or(true, |run| {
                // New run if script changes (unless Common/Inherited)
                if !self.scripts_compatible(run.script, script) {
                    return true;
                }

                // New run if direction changes
                if run.direction != direction {
                    return true;
                }

                // New run if bidi level changes
                if run.level != level.number() {
                    return true;
                }

                false
            });

            if needs_new_run {
                // Finish current run
                if let Some(builder) = current_run.take() {
                    runs.push(builder.build());
                }

                // Start new run
                current_run = Some(RunBuilder {
                    start: idx,
                    end: idx + ch.len_utf8(),
                    script: self.resolve_script(script),
                    direction,
                    level: level.number(),
                    text: String::new(),
                });
            }

            // Add character to current run
            if let Some(ref mut run) = current_run {
                run.text.push(ch);
                run.end = idx + ch.len_utf8();
            }
        }

        // Finish last run
        if let Some(builder) = current_run {
            runs.push(builder.build());
        }

        Ok(runs)
    }

    /// Check if scripts are compatible for same run
    fn scripts_compatible(&self, a: Script, b: Script) -> bool {
        // Common and Inherited scripts can merge with any script
        if a == Script::Common || a == Script::Inherited {
            return true;
        }
        if b == Script::Common || b == Script::Inherited {
            return true;
        }

        // Otherwise must be same script
        a == b
    }

    /// Resolve script (handle Common/Inherited)
    fn resolve_script(&self, script: Script) -> Script {
        // TODO: Properly resolve Common/Inherited based on adjacent scripts
        // For now, treat them as Latin
        match script {
            Script::Common | Script::Inherited => Script::Latin,
            _ => script,
        }
    }
}

/// Builder for itemized run
struct RunBuilder {
    start: usize,
    end: usize,
    script: Script,
    direction: Direction,
    level: u8,
    text: String,
}

impl RunBuilder {
    fn build(self) -> ItemizedRun {
        ItemizedRun {
            start: self.start,
            end: self.end,
            script: self.script,
            direction: self.direction,
            level: self.level,
            text: self.text,
        }
    }
}

/// A run of text with uniform properties
#[derive(Debug, Clone)]
pub struct ItemizedRun {
    /// Start byte index in original text
    pub start: usize,

    /// End byte index in original text
    pub end: usize,

    /// Script for this run
    pub script: Script,

    /// Text direction
    pub direction: Direction,

    /// Bidi embedding level
    pub level: u8,

    /// Text content of this run
    pub text: String,
}

impl Default for ScriptItemizer {
    fn default() -> Self {
        Self::new()
    }
}
```

### Step 4: Implement Text Shaping (Day 3-5)

**File: `src/text/shaping/shaper.rs`**

```rust
//! Text shaping using HarfBuzz (via rustybuzz)
//!
//! Converts text to positioned glyphs.

use super::FontRun;
use crate::text::font::FontFace;
use crate::error::{Result, Error};
use rustybuzz::{Face as HbFace, UnicodeBuffer, Direction as HbDirection};
use std::sync::Arc;

/// Text shaper
///
/// Uses HarfBuzz (rustybuzz) to shape text.
pub struct TextShaper;

impl TextShaper {
    pub fn new() -> Self {
        Self
    }

    /// Shape a font run
    pub fn shape_run(&self, run: &FontRun) -> Result<ShapedText> {
        // Create HarfBuzz face from font data
        let hb_face = HbFace::from_slice(&run.font.data, run.font.index)
            .ok_or_else(|| Error::Font("Failed to create HarfBuzz face".into()))?;

        // Create unicode buffer
        let mut buffer = UnicodeBuffer::new();
        buffer.push_str(&run.text);

        // Set buffer properties
        buffer.set_direction(self.direction_to_hb(run.direction));
        buffer.set_script(self.script_to_hb(run.script));
        // TODO: Set language from style

        // Shape!
        let output = rustybuzz::shape(&hb_face, &[], buffer);

        // Extract glyph information
        let glyph_infos = output.glyph_infos();
        let glyph_positions = output.glyph_positions();

        let mut glyphs = Vec::new();
        let mut x_offset = 0.0;
        let mut clusters = Vec::new();
        let mut current_cluster = None;

        for (info, pos) in glyph_infos.iter().zip(glyph_positions.iter()) {
            let glyph = GlyphInfo {
                glyph_id: info.glyph_id,
                cluster: info.cluster as usize,
                x_offset,
                y_offset: pos.y_offset as f32,
                x_advance: pos.x_advance as f32,
                y_advance: pos.y_advance as f32,
            };

            // Track clusters (character → glyph mapping)
            let cluster_id = info.cluster as usize;
            if current_cluster.as_ref().map_or(true, |c: &GlyphCluster| c.cluster != cluster_id) {
                if let Some(cluster) = current_cluster.take() {
                    clusters.push(cluster);
                }
                current_cluster = Some(GlyphCluster {
                    cluster: cluster_id,
                    glyph_start: glyphs.len(),
                    glyph_count: 0,
                    text_start: cluster_id,
                    text_len: 0,
                });
            }

            if let Some(ref mut cluster) = current_cluster {
                cluster.glyph_count += 1;
            }

            x_offset += pos.x_advance as f32;
            glyphs.push(glyph);
        }

        // Add last cluster
        if let Some(cluster) = current_cluster {
            clusters.push(cluster);
        }

        Ok(ShapedText {
            text: run.text.clone(),
            font: Arc::clone(&run.font),
            glyphs,
            clusters,
            advance: x_offset,
        })
    }

    /// Convert our direction to HarfBuzz direction
    fn direction_to_hb(&self, dir: super::Direction) -> HbDirection {
        match dir {
            super::Direction::LeftToRight => HbDirection::LeftToRight,
            super::Direction::RightToLeft => HbDirection::RightToLeft,
        }
    }

    /// Convert unicode-script Script to HarfBuzz script
    fn script_to_hb(&self, script: unicode_script::Script) -> rustybuzz::Script {
        // Convert script enum to 4-byte tag
        // HarfBuzz uses ISO 15924 script codes
        match script {
            unicode_script::Script::Latin => rustybuzz::Script::Latin,
            unicode_script::Script::Arabic => rustybuzz::Script::Arabic,
            unicode_script::Script::Hebrew => rustybuzz::Script::Hebrew,
            unicode_script::Script::Devanagari => rustybuzz::Script::Devanagari,
            unicode_script::Script::Bengali => rustybuzz::Script::Bengali,
            unicode_script::Script::Cyrillic => rustybuzz::Script::Cyrillic,
            unicode_script::Script::Greek => rustybuzz::Script::Greek,
            unicode_script::Script::Han => rustybuzz::Script::Han,
            unicode_script::Script::Hiragana => rustybuzz::Script::Hiragana,
            unicode_script::Script::Katakana => rustybuzz::Script::Katakana,
            unicode_script::Script::Hangul => rustybuzz::Script::Hangul,
            unicode_script::Script::Thai => rustybuzz::Script::Thai,
            // Add more as needed
            _ => rustybuzz::Script::Unknown,
        }
    }
}

/// Shaped text
///
/// Result of text shaping - positioned glyphs ready for rendering.
#[derive(Debug, Clone)]
pub struct ShapedText {
    /// Original text
    pub text: String,

    /// Font used for shaping
    pub font: Arc<FontFace>,

    /// Shaped glyphs
    pub glyphs: Vec<GlyphInfo>,

    /// Glyph clusters (character → glyph mapping)
    pub clusters: Vec<GlyphCluster>,

    /// Total horizontal advance
    pub advance: f32,
}

/// Information about a single glyph
#[derive(Debug, Clone, Copy)]
pub struct GlyphInfo {
    /// Glyph ID in font
    pub glyph_id: u32,

    /// Cluster index (maps to character position)
    pub cluster: usize,

    /// X offset from pen position
    pub x_offset: f32,

    /// Y offset from pen position
    pub y_offset: f32,

    /// Horizontal advance (how much to move pen after this glyph)
    pub x_advance: f32,

    /// Vertical advance (usually 0 for horizontal text)
    pub y_advance: f32,
}

/// Glyph cluster
///
/// Maps characters to glyphs. One cluster can contain:
/// - One character → one glyph (most common)
/// - One character → multiple glyphs (e.g., ä → a + combining diacritic)
/// - Multiple characters → one glyph (ligatures: fi → ﬁ)
#[derive(Debug, Clone)]
pub struct GlyphCluster {
    /// Cluster index (character position)
    pub cluster: usize,

    /// First glyph index in ShapedText.glyphs
    pub glyph_start: usize,

    /// Number of glyphs in this cluster
    pub glyph_count: usize,

    /// Start position in text
    pub text_start: usize,

    /// Length in text (bytes)
    pub text_len: usize,
}

impl Default for TextShaper {
    fn default() -> Self {
        Self::new()
    }
}
```

### Step 5: Emoji Support (Day 5-6)

**File: `src/text/shaping/emoji.rs`**

```rust
//! Emoji detection and handling

use unic_emoji_char::{is_emoji, is_emoji_presentation};

/// Emoji detector
pub struct EmojiDetector;

impl EmojiDetector {
    pub fn new() -> Self {
        Self
    }

    /// Check if character is emoji
    pub fn is_emoji(&self, ch: char) -> bool {
        is_emoji(ch)
    }

    /// Check if character has emoji presentation
    pub fn is_emoji_presentation(&self, ch: char) -> bool {
        is_emoji_presentation(ch)
    }

    /// Detect emoji sequences
    ///
    /// Emoji can be composed of multiple codepoints:
    /// - Skin tone modifiers (U+1F3FB..U+1F3FF)
    /// - ZWJ sequences (👨‍👩‍👧‍👦)
    /// - Flag sequences (🇺🇸)
    pub fn detect_emoji_sequences(&self, text: &str) -> Vec<EmojiSequence> {
        let mut sequences = Vec::new();
        let mut chars = text.char_indices().peekable();

        while let Some((idx, ch)) = chars.next() {
            if !self.is_emoji(ch) {
                continue;
            }

            // Found emoji - check if it's part of a sequence
            let mut end = idx + ch.len_utf8();
            let mut sequence_chars = vec![ch];

            // Check for modifiers and ZWJ sequences
            while let Some(&(next_idx, next_ch)) = chars.peek() {
                if self.is_modifier(next_ch) || self.is_zwj(next_ch) {
                    sequence_chars.push(next_ch);
                    end = next_idx + next_ch.len_utf8();
                    chars.next();
                } else if self.is_emoji(next_ch) && !sequence_chars.is_empty() &&
                          sequence_chars.last() == Some(&'\u{200D}') {
                    // ZWJ followed by another emoji
                    sequence_chars.push(next_ch);
                    end = next_idx + next_ch.len_utf8();
                    chars.next();
                } else {
                    break;
                }
            }

            sequences.push(EmojiSequence {
                start: idx,
                end,
                chars: sequence_chars,
            });
        }

        sequences
    }

    /// Check if character is a skin tone modifier
    fn is_modifier(&self, ch: char) -> bool {
        matches!(ch, '\u{1F3FB}'..='\u{1F3FF}')
    }

    /// Check if character is Zero Width Joiner
    fn is_zwj(&self, ch: char) -> bool {
        ch == '\u{200D}'
    }

    /// Check if character is variation selector
    fn is_variation_selector(&self, ch: char) -> bool {
        matches!(ch, '\u{FE00}'..='\u{FE0F}')
    }
}

/// An emoji sequence
#[derive(Debug, Clone)]
pub struct EmojiSequence {
    /// Start byte index
    pub start: usize,

    /// End byte index
    pub end: usize,

    /// Characters in sequence
    pub chars: Vec<char>,
}

impl Default for EmojiDetector {
    fn default() -> Self {
        Self::new()
    }
}
```

### Step 6: Integration Tests (Day 7-8)

**File: `tests/text/shaping_test.rs`**

```rust
//! Tests for text shaping

use fastrender::text::shaping::*;
use fastrender::text::font::*;
use fastrender::style::ComputedStyle;

#[test]
fn test_simple_latin_shaping() {
    let pipeline = ShapingPipeline::new();
    let loader = FontLoader::new();
    let style = default_style();

    let shaped = pipeline.shape("Hello", &style, &loader).unwrap();

    assert!(!shaped.is_empty());
    assert_eq!(shaped[0].text, "Hello");
    assert_eq!(shaped[0].glyphs.len(), 5); // 5 characters → 5 glyphs
}

#[test]
fn test_ligature_shaping() {
    let pipeline = ShapingPipeline::new();
    let loader = FontLoader::new();
    let style = default_style();

    // "fi" may become a ligature (ﬁ) depending on font
    let shaped = pipeline.shape("office", &style, &loader).unwrap();

    assert!(!shaped.is_empty());
    // Number of glyphs may be less than characters due to ligatures
    assert!(shaped[0].glyphs.len() <= 6);
}

#[test]
fn test_arabic_shaping() {
    let pipeline = ShapingPipeline::new();
    let loader = FontLoader::new();
    let mut style = default_style();
    style.direction = crate::style::Direction::Rtl;

    // Arabic text: "مرحبا" (Hello)
    let shaped = pipeline.shape("مرحبا", &style, &loader).unwrap();

    assert!(!shaped.is_empty());

    // Arabic has contextual shaping - glyph forms depend on position
    // Characters may produce different glyphs than their isolated forms
    let run = &shaped[0];
    assert!(run.glyphs.len() > 0);
}

#[test]
fn test_hebrew_shaping() {
    let pipeline = ShapingPipeline::new();
    let loader = FontLoader::new();
    let mut style = default_style();
    style.direction = crate::style::Direction::Rtl;

    // Hebrew text: "שלום" (Hello)
    let shaped = pipeline.shape("שלום", &style, &loader).unwrap();

    assert!(!shaped.is_empty());
}

#[test]
fn test_mixed_ltr_rtl() {
    let pipeline = ShapingPipeline::new();
    let loader = FontLoader::new();
    let style = default_style();

    // Mixed English and Arabic
    let text = "Hello مرحبا World";
    let shaped = pipeline.shape(text, &style, &loader).unwrap();

    assert!(!shaped.is_empty());
    // Should create multiple runs for different scripts/directions
}

#[test]
fn test_devanagari_shaping() {
    let pipeline = ShapingPipeline::new();
    let loader = FontLoader::new();
    let style = default_style();

    // Devanagari text: "नमस्ते" (Hello)
    let shaped = pipeline.shape("नमस्ते", &style, &loader).unwrap();

    assert!(!shaped.is_empty());
    // Devanagari has complex shaping rules
    // Character count != glyph count
}

#[test]
fn test_emoji_shaping() {
    let pipeline = ShapingPipeline::new();
    let loader = FontLoader::new();
    let style = default_style();

    // Simple emoji
    let shaped = pipeline.shape("Hello 👋 World", &style, &loader).unwrap();

    assert!(!shaped.is_empty());
}

#[test]
fn test_emoji_zwj_sequence() {
    let detector = EmojiDetector::new();

    // Family emoji: 👨‍👩‍👧‍👦 (man, woman, girl, boy with ZWJ)
    let text = "👨‍👩‍👧‍👦";
    let sequences = detector.detect_emoji_sequences(text);

    assert_eq!(sequences.len(), 1);
    // Should detect as single sequence, not multiple emoji
}

#[test]
fn test_emoji_skin_tone() {
    let detector = EmojiDetector::new();

    // Waving hand with skin tone: 👋🏽
    let text = "👋🏽";
    let sequences = detector.detect_emoji_sequences(text);

    assert_eq!(sequences.len(), 1);
    assert_eq!(sequences[0].chars.len(), 2); // Base + modifier
}

#[test]
fn test_script_itemization() {
    let itemizer = ScriptItemizer::new();
    let bidi = BidiAnalyzer::new();
    let style = default_style();

    // Mixed scripts
    let text = "Hello שלום мир";
    let analysis = bidi.analyze(text, &style).unwrap();
    let runs = itemizer.itemize(text, &analysis).unwrap();

    // Should split into separate runs for Latin, Hebrew, Cyrillic
    assert!(runs.len() >= 3);
}

#[test]
fn test_bidi_analysis() {
    let bidi = BidiAnalyzer::new();
    let mut style = default_style();
    style.direction = crate::style::Direction::Ltr;

    let text = "Hello مرحبا";
    let analysis = bidi.analyze(text, &style).unwrap();

    // Should detect mixed directionality
    assert!(analysis.needs_reordering());
}

#[test]
fn test_glyph_clusters() {
    let pipeline = ShapingPipeline::new();
    let loader = FontLoader::new();
    let style = default_style();

    let shaped = pipeline.shape("café", &style, &loader).unwrap();
    let run = &shaped[0];

    // Check clusters are present
    assert!(!run.clusters.is_empty());

    // Each cluster should have valid glyph indices
    for cluster in &run.clusters {
        assert!(cluster.glyph_start < run.glyphs.len());
        assert!(cluster.glyph_count > 0);
    }
}

#[test]
fn test_glyph_advances() {
    let pipeline = ShapingPipeline::new();
    let loader = FontLoader::new();
    let style = default_style();

    let shaped = pipeline.shape("Hello", &style, &loader).unwrap();
    let run = &shaped[0];

    // Check advances are positive
    for glyph in &run.glyphs {
        assert!(glyph.x_advance >= 0.0);
    }

    // Total advance should be sum of individual advances
    let total: f32 = run.glyphs.iter().map(|g| g.x_advance).sum();
    assert!((total - run.advance).abs() < 0.1);
}

// Helper functions
fn default_style() -> ComputedStyle {
    ComputedStyle {
        font_family: vec!["Arial".to_string(), "sans-serif".to_string()],
        font_weight: crate::style::FontWeight::Normal,
        font_style: FontStyle::Normal,
        font_stretch: FontStretch::Normal,
        direction: crate::style::Direction::Ltr,
        ..Default::default()
    }
}
```

## Acceptance Criteria

- [ ] Simple Latin text shapes correctly
- [ ] Ligatures work (fi → ﬁ, ff → ﬀ, etc.)
- [ ] Arabic text shapes with contextual forms
- [ ] Hebrew text shapes correctly
- [ ] Mixed LTR/RTL text is handled correctly
- [ ] Bidi reordering works (UAX #9)
- [ ] Script itemization splits text into proper runs
- [ ] Devanagari and other complex scripts shape correctly
- [ ] Emoji render correctly
- [ ] Emoji ZWJ sequences work (👨‍👩‍👧‍👦)
- [ ] Emoji skin tone modifiers work (👋🏽)
- [ ] Glyph clusters track character-to-glyph mapping
- [ ] Glyph advances are correct
- [ ] All tests pass: `cargo test shaping`
- [ ] Code follows 10-code-standards.md

## Common Pitfalls

### Pitfall 1: Not Handling Bidi Correctly

**Wrong:**
```rust
// Shaping without bidi analysis
let shaped = shaper.shape(text, font);
// RTL text will be backwards!
```

**Right:**
```rust
// Always run bidi analysis first
let bidi = analyzer.analyze(text, style)?;
let runs = itemizer.itemize(text, &bidi)?;
// Then shape each run
```

### Pitfall 2: Ignoring Script Boundaries

**Wrong:**
```rust
// Shape entire text with one font
let shaped = shaper.shape("Hello שלום", font);
// Font might not have glyphs for both scripts!
```

**Right:**
```rust
// Itemize by script first
let runs = itemizer.itemize(text, &bidi)?;
// Assign appropriate font to each run
let font_runs = assign_fonts(&runs, style, loader)?;
```

### Pitfall 3: Breaking Emoji Sequences

**Wrong:**
```rust
// Treating each character as separate
for ch in text.chars() {
    shape(ch); // Breaks 👨‍👩‍👧‍👦 into separate emoji!
}
```

**Right:**
```rust
// Detect emoji sequences first
let sequences = detector.detect_emoji_sequences(text);
// Shape each sequence as a unit
```

### Pitfall 4: Not Preserving Clusters

**Wrong:**
```rust
// Only storing glyphs without cluster info
struct ShapedText {
    glyphs: Vec<GlyphInfo>,
}
// Lost character-to-glyph mapping!
```

**Right:**
```rust
// Always preserve cluster information
struct ShapedText {
    glyphs: Vec<GlyphInfo>,
    clusters: Vec<GlyphCluster>,
}
// Needed for hit testing, selection, etc.
```

## Performance Considerations

1. **Cache shaped text** - Shaping is expensive, cache results
2. **Batch by script** - Shape entire runs at once, not character-by-character
3. **Reuse HarfBuzz faces** - Don't recreate for each shaping operation
4. **Lazy emoji detection** - Only detect emoji sequences when needed
5. **Incremental shaping** - For large documents, shape visible portions first

## Integration Points

The text shaping system integrates with:

1. **Font system** - Gets fonts for shaping
2. **Inline layout** - Provides shaped glyphs for line breaking
3. **Text rendering** - Shaped glyphs are rendered to screen
4. **Hit testing** - Cluster information maps screen positions to characters
5. **Text selection** - Clusters enable character selection

## Unicode Compliance

This implementation aims to support:

- **UAX #9:** Unicode Bidirectional Algorithm ✓
- **UAX #14:** Line Breaking (see 03-line-breaking.md)
- **UAX #29:** Text Segmentation (grapheme clusters)
- **UAX #24:** Script Property
- **OpenType GSUB/GPOS:** Glyph substitution and positioning ✓

## Next Steps

After text shaping is complete:
- **03-line-breaking.md** - Line breaking algorithm
- Integration with inline layout for actual text rendering
- Hit testing and text selection

## References

- **Unicode Bidirectional Algorithm (UAX #9):** https://www.unicode.org/reports/tr9/
- **Unicode Script Property (UAX #24):** https://www.unicode.org/reports/tr24/
- **OpenType Specification:** https://learn.microsoft.com/en-us/typography/opentype/spec/
- **HarfBuzz documentation:** https://harfbuzz.github.io/
- **rustybuzz documentation:** https://docs.rs/rustybuzz/
- **unicode-bidi documentation:** https://docs.rs/unicode-bidi/
- **CSS Writing Modes Level 4:** https://www.w3.org/TR/css-writing-modes-4/
- **Text Rendering Hates You:** https://gankra.github.io/blah/text-hates-you/

---

**Last Updated:** 2025-11-19
**Status:** Ready for Implementation
