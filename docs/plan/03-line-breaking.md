# Phase 3: Line Breaking

**Duration:** Week 3 of Phase 3 (7-10 days)
**Prerequisites:**
- Font system complete (03-font-system.md)
- Text shaping complete (03-text-shaping.md)
- Inline layout framework in place
**Dependencies:**
- unicode-linebreak crate for UAX #14
- hyphenation crate for hyphenation support
- ShapedText and GlyphInfo types
- FontMetrics for measurement
**Output:** Complete line breaking system with word wrapping, hyphenation, and justification

## Objectives

Implement a comprehensive line breaking system that handles:

- **Unicode Line Breaking Algorithm (UAX #14)** - Correct break opportunities
- **Word Wrapping** - Soft wraps at word boundaries
- **Hyphenation** - Breaking words with hyphens when needed
- **Text Alignment** - left, right, center, justify
- **Text Justification** - Distributing space for justified text
- **Overflow Control** - word-break, overflow-wrap properties
- **Widow/Orphan Control** - Preventing single lines at page breaks

This is the **final piece of the text rendering puzzle** and enables proper multi-line text layout.

## Context

Line breaking is the process of taking a continuous string of text and breaking it into lines that fit within a given width:

**Input:**
```
Text: "The quick brown fox jumps over the lazy dog"
Width: 200px
```

**Output:**
```
Line 1: "The quick brown"
Line 2: "fox jumps over"
Line 3: "the lazy dog"
```

**Why is this complex?**

1. **Break opportunities vary by language** - English breaks at spaces, Chinese can break almost anywhere
2. **Hyphenation rules are language-specific** - German: "Freundschaftsbezeugung" → "Freund-schafts-be-zeu-gung"
3. **Justification requires space distribution** - Stretching/shrinking whitespace and glyphs
4. **CJK line breaking is different** - No spaces, different rules
5. **Some sequences can't break** - U+00A0 (non-breaking space), ZWSP, etc.

**From CSS Text Module Level 3:**
> "Line breaking is the process of breaking inline-level content into line boxes. It encompasses line breaking as described in Unicode Standard Annex #14."

## The Problem V1 Has

V1 has minimal line breaking:
- Only breaks at spaces
- No hyphenation
- No justification
- No CJK support
- Incorrect breaking for complex text
- No respect for break properties (word-break, overflow-wrap)

This makes V1 unusable for international text and proper typography.

## The Solution

Implement a multi-stage line breaking system:

1. **Unicode Line Breaking (UAX #14)** - Find break opportunities
2. **Greedy Line Breaking** - Fit as much text per line as possible
3. **Optimal Line Breaking** - Knuth-Plass algorithm for better results (optional)
4. **Hyphenation** - Break words with hyphens
5. **Justification** - Distribute space for justified text
6. **Alignment** - Position text within line boxes

Use proven libraries:
- **unicode-linebreak** - UAX #14 implementation
- **hyphenation** - Language-specific hyphenation patterns

## CSS/Unicode Specification References

**Primary:**
- **Unicode Line Breaking Algorithm (UAX #14):**
  - https://www.unicode.org/reports/tr14/
- **CSS Text Module Level 3:**
  - https://www.w3.org/TR/css-text-3/
- **CSS Text Module Level 4:**
  - https://www.w3.org/TR/css-text-4/

**Key Sections:**
- **UAX #14:** Line breaking properties and algorithm
- **CSS Text 5:** White Space and Wrapping
- **CSS Text 6:** Line Breaking and Word Boundaries
- **CSS Text 7:** Alignment and Justification
- **Knuth-Plass:** Breaking Paragraphs into Lines (1981)

**Key Concepts:**
- **Break class** - Line breaking behavior of characters
- **Break opportunity** - Position where line can break
- **Soft hyphen** - U+00AD, breaks with hyphen
- **Non-breaking space** - U+00A0, prevents break
- **Zero-width space** - U+200B, allows break

## Step-by-Step Implementation

### Step 1: Create Line Breaking Module (Day 1 Morning)

```bash
mkdir -p /home/user/fastrender/src/text/linebreak
touch /home/user/fastrender/src/text/linebreak/mod.rs
touch /home/user/fastrender/src/text/linebreak/breaks.rs
touch /home/user/fastrender/src/text/linebreak/greedy.rs
touch /home/user/fastrender/src/text/linebreak/optimal.rs
touch /home/user/fastrender/src/text/linebreak/hyphenate.rs
touch /home/user/fastrender/src/text/linebreak/justify.rs
```

**File: `Cargo.toml` (add dependencies)**

```toml
[dependencies]
# Line breaking
unicode-linebreak = "0.1"

# Hyphenation (optional but recommended)
hyphenation = "0.8"

# For Knuth-Plass optimal breaking
# We'll implement ourselves using dynamic programming
```

**File: `src/text/linebreak/mod.rs`**

```rust
//! Line breaking
//!
//! Implements line breaking according to CSS Text Module Level 3
//! and Unicode Line Breaking Algorithm (UAX #14).

pub mod breaks;
pub mod greedy;
pub mod optimal;
pub mod hyphenate;
pub mod justify;

pub use breaks::{BreakOpportunityFinder, BreakOpportunity};
pub use greedy::GreedyLineBreaker;
pub use optimal::OptimalLineBreaker;
pub use hyphenate::Hyphenator;
pub use justify::Justifier;

use crate::text::shaping::ShapedText;
use crate::style::ComputedStyle;
use crate::error::Result;

/// Line breaking pipeline
///
/// Main entry point for line breaking.
pub struct LineBreaker {
    break_finder: BreakOpportunityFinder,
    hyphenator: Option<Hyphenator>,
    use_optimal: bool,
}

impl LineBreaker {
    /// Create new line breaker
    pub fn new() -> Self {
        Self {
            break_finder: BreakOpportunityFinder::new(),
            hyphenator: None,
            use_optimal: false,
        }
    }

    /// Create with hyphenation support
    pub fn with_hyphenation(language: &str) -> Result<Self> {
        Ok(Self {
            break_finder: BreakOpportunityFinder::new(),
            hyphenator: Some(Hyphenator::new(language)?),
            use_optimal: false,
        })
    }

    /// Enable optimal line breaking (Knuth-Plass algorithm)
    pub fn enable_optimal(&mut self) {
        self.use_optimal = true;
    }

    /// Break shaped text into lines
    ///
    /// This is the main method called from inline layout.
    pub fn break_lines(
        &self,
        shaped: &[ShapedText],
        max_width: f32,
        style: &ComputedStyle,
    ) -> Result<Vec<Line>> {
        // Step 1: Find break opportunities
        let opportunities = self.find_opportunities(shaped, style)?;

        // Step 2: Add hyphenation opportunities if enabled
        let opportunities = if let Some(ref hyphenator) = self.hyphenator {
            self.add_hyphenation_opportunities(opportunities, shaped, hyphenator)?
        } else {
            opportunities
        };

        // Step 3: Break into lines
        let lines = if self.use_optimal {
            let breaker = OptimalLineBreaker::new();
            breaker.break_lines(shaped, &opportunities, max_width, style)?
        } else {
            let breaker = GreedyLineBreaker::new();
            breaker.break_lines(shaped, &opportunities, max_width, style)?
        };

        // Step 4: Apply justification if needed
        let lines = if style.text_align == TextAlign::Justify {
            let justifier = Justifier::new();
            justifier.justify_lines(lines, max_width, style)?
        } else {
            lines
        };

        Ok(lines)
    }

    /// Find break opportunities in shaped text
    fn find_opportunities(
        &self,
        shaped: &[ShapedText],
        style: &ComputedStyle,
    ) -> Result<Vec<BreakOpportunity>> {
        let mut opportunities = Vec::new();

        for run in shaped {
            let run_opportunities = self.break_finder.find_opportunities(
                &run.text,
                style,
            )?;
            opportunities.extend(run_opportunities);
        }

        Ok(opportunities)
    }

    /// Add hyphenation opportunities
    fn add_hyphenation_opportunities(
        &self,
        mut opportunities: Vec<BreakOpportunity>,
        shaped: &[ShapedText],
        hyphenator: &Hyphenator,
    ) -> Result<Vec<BreakOpportunity>> {
        for run in shaped {
            let hyphen_opportunities = hyphenator.find_hyphenation_points(&run.text)?;
            for point in hyphen_opportunities {
                opportunities.push(BreakOpportunity {
                    position: point,
                    penalty: 100, // Prefer regular breaks over hyphenation
                    break_type: BreakType::Hyphen,
                });
            }
        }

        // Sort by position
        opportunities.sort_by_key(|o| o.position);

        Ok(opportunities)
    }
}

/// A line of text
#[derive(Debug, Clone)]
pub struct Line {
    /// Shaped text segments in this line
    pub segments: Vec<LineSegment>,

    /// Total width of content (before justification)
    pub width: f32,

    /// Available width for this line
    pub max_width: f32,

    /// Whether this is the last line in a paragraph
    pub is_last: bool,
}

/// A segment of text in a line
#[derive(Debug, Clone)]
pub struct LineSegment {
    /// Reference to shaped text
    pub shaped: ShapedText,

    /// Start glyph index in shaped text
    pub glyph_start: usize,

    /// Number of glyphs from shaped text
    pub glyph_count: usize,

    /// X offset in line
    pub x_offset: f32,

    /// Width of this segment
    pub width: f32,

    /// Whether this segment ends with a hyphen
    pub has_hyphen: bool,
}

/// Type of line break
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BreakType {
    /// Normal break (whitespace)
    Normal,

    /// Hyphenation break
    Hyphen,

    /// Hard break (newline, <br>)
    Hard,

    /// Emergency break (overflow, no other option)
    Emergency,
}

impl Default for LineBreaker {
    fn default() -> Self {
        Self::new()
    }
}
```

### Step 2: Implement Break Opportunity Finding (Day 1-2)

**File: `src/text/linebreak/breaks.rs`**

```rust
//! Break opportunity detection
//!
//! Uses Unicode Line Breaking Algorithm (UAX #14)

use super::BreakType;
use crate::style::ComputedStyle;
use crate::error::Result;
use unicode_linebreak::{BreakOpportunity as UnicodeBreakOpp, linebreaks};

/// Break opportunity finder
///
/// Finds positions where lines can break according to UAX #14
/// and CSS text properties.
pub struct BreakOpportunityFinder;

impl BreakOpportunityFinder {
    pub fn new() -> Self {
        Self
    }

    /// Find break opportunities in text
    ///
    /// Returns positions where line can break, with penalties.
    pub fn find_opportunities(
        &self,
        text: &str,
        style: &ComputedStyle,
    ) -> Result<Vec<BreakOpportunity>> {
        let mut opportunities = Vec::new();

        // Use unicode-linebreak for UAX #14
        for (idx, opp) in linebreaks(text).enumerate() {
            // Convert UAX #14 opportunity to our type
            let (break_type, penalty) = self.classify_break(opp, style);

            opportunities.push(BreakOpportunity {
                position: idx,
                penalty,
                break_type,
            });
        }

        // Apply CSS word-break and overflow-wrap
        self.apply_css_rules(&mut opportunities, text, style)?;

        Ok(opportunities)
    }

    /// Classify break opportunity
    fn classify_break(
        &self,
        opp: UnicodeBreakOpp,
        style: &ComputedStyle,
    ) -> (BreakType, i32) {
        use unicode_linebreak::BreakOpportunity as UBO;

        match opp {
            UBO::Mandatory => (BreakType::Hard, 0),
            UBO::Allowed => (BreakType::Normal, 10),
            UBO::NoBreak => (BreakType::Normal, i32::MAX), // Effectively no break
        }
    }

    /// Apply CSS line breaking rules
    fn apply_css_rules(
        &self,
        opportunities: &mut Vec<BreakOpportunity>,
        text: &str,
        style: &ComputedStyle,
    ) -> Result<()> {
        match style.word_break {
            WordBreak::Normal => {
                // Default UAX #14 behavior (already applied)
            }
            WordBreak::BreakAll => {
                // Allow breaks between any characters
                self.add_break_all_opportunities(opportunities, text);
            }
            WordBreak::KeepAll => {
                // Don't break between characters (CJK, Thai, etc.)
                self.remove_script_breaks(opportunities, text);
            }
            WordBreak::BreakWord => {
                // Like break-all but only if overflow
                // (handled during line breaking, not here)
            }
        }

        match style.overflow_wrap {
            OverflowWrap::Normal => {
                // Only break at allowed break points
            }
            OverflowWrap::Anywhere | OverflowWrap::BreakWord => {
                // Allow breaks anywhere if needed
                self.add_anywhere_opportunities(opportunities, text);
            }
        }

        Ok(())
    }

    /// Add break opportunities between all characters (word-break: break-all)
    fn add_break_all_opportunities(&self, opportunities: &mut Vec<BreakOpportunity>, text: &str) {
        for (idx, _) in text.char_indices() {
            // Add opportunity if not already present
            if !opportunities.iter().any(|o| o.position == idx) {
                opportunities.push(BreakOpportunity {
                    position: idx,
                    penalty: 50, // Moderate penalty
                    break_type: BreakType::Normal,
                });
            }
        }

        opportunities.sort_by_key(|o| o.position);
    }

    /// Remove breaks within scripts (word-break: keep-all)
    fn remove_script_breaks(&self, opportunities: &mut Vec<BreakOpportunity>, text: &str) {
        // Increase penalty for breaks within CJK text
        for opp in opportunities.iter_mut() {
            if self.is_in_keepall_script(text, opp.position) {
                opp.penalty = i32::MAX; // Don't break
            }
        }
    }

    /// Add emergency break opportunities (overflow-wrap: anywhere)
    fn add_anywhere_opportunities(&self, opportunities: &mut Vec<BreakOpportunity>, text: &str) {
        for (idx, _) in text.char_indices() {
            if !opportunities.iter().any(|o| o.position == idx) {
                opportunities.push(BreakOpportunity {
                    position: idx,
                    penalty: 1000, // High penalty, only use if needed
                    break_type: BreakType::Emergency,
                });
            }
        }

        opportunities.sort_by_key(|o| o.position);
    }

    /// Check if position is in keep-all script
    fn is_in_keepall_script(&self, text: &str, position: usize) -> bool {
        if let Some(ch) = text[position..].chars().next() {
            self.is_keepall_script_char(ch)
        } else {
            false
        }
    }

    /// Check if character is in keep-all script (CJK, Thai, etc.)
    fn is_keepall_script_char(&self, ch: char) -> bool {
        use unicode_script::{Script, UnicodeScript};

        matches!(
            ch.script(),
            Script::Han | Script::Hiragana | Script::Katakana | Script::Hangul | Script::Thai
        )
    }
}

/// A break opportunity
#[derive(Debug, Clone, Copy)]
pub struct BreakOpportunity {
    /// Byte position in text
    pub position: usize,

    /// Penalty for breaking here (lower = better)
    /// 0 = mandatory break
    /// 1-100 = good break
    /// 100-1000 = acceptable break
    /// 1000+ = poor break
    /// i32::MAX = no break
    pub penalty: i32,

    /// Type of break
    pub break_type: BreakType,
}

impl Default for BreakOpportunityFinder {
    fn default() -> Self {
        Self::new()
    }
}
```

### Step 3: Implement Greedy Line Breaking (Day 2-3)

**File: `src/text/linebreak/greedy.rs`**

```rust
//! Greedy line breaking algorithm
//!
//! Simple first-fit algorithm: fit as much text as possible on each line.

use super::{Line, LineSegment, BreakOpportunity, BreakType};
use crate::text::shaping::ShapedText;
use crate::style::ComputedStyle;
use crate::error::Result;

/// Greedy line breaker
///
/// Breaks lines using a simple greedy algorithm:
/// - Add words/glyphs to current line while they fit
/// - When something doesn't fit, break at last opportunity
/// - Start new line and repeat
pub struct GreedyLineBreaker;

impl GreedyLineBreaker {
    pub fn new() -> Self {
        Self
    }

    /// Break lines using greedy algorithm
    pub fn break_lines(
        &self,
        shaped: &[ShapedText],
        opportunities: &[BreakOpportunity],
        max_width: f32,
        style: &ComputedStyle,
    ) -> Result<Vec<Line>> {
        let mut lines = Vec::new();
        let mut current_line = LineBuilder::new(max_width);
        let mut last_break: Option<BreakOpportunity> = None;

        // Flatten shaped text into single sequence with break opportunities
        let items = self.build_item_sequence(shaped, opportunities)?;

        for item in items {
            match item {
                Item::Glyph { run_idx, glyph_idx, advance } => {
                    // Try to add glyph to current line
                    if current_line.width + advance <= max_width {
                        current_line.add_glyph(run_idx, glyph_idx, advance);
                    } else {
                        // Doesn't fit - break at last opportunity
                        if let Some(brk) = last_break {
                            // Finish current line
                            let line = current_line.finish(shaped, false);
                            lines.push(line);

                            // Start new line
                            current_line = LineBuilder::new(max_width);
                            current_line.add_glyph(run_idx, glyph_idx, advance);
                        } else {
                            // No break opportunity - overflow
                            // Add to line anyway (overflow)
                            current_line.add_glyph(run_idx, glyph_idx, advance);
                        }
                        last_break = None;
                    }
                }
                Item::BreakOpportunity(opp) => {
                    // Remember as potential break point
                    if opp.break_type == BreakType::Hard {
                        // Hard break - must break here
                        let line = current_line.finish(shaped, false);
                        lines.push(line);
                        current_line = LineBuilder::new(max_width);
                        last_break = None;
                    } else if opp.penalty < i32::MAX {
                        last_break = Some(opp);
                    }
                }
            }
        }

        // Finish last line
        if !current_line.is_empty() {
            let line = current_line.finish(shaped, true);
            lines.push(line);
        }

        Ok(lines)
    }

    /// Build sequence of glyphs and break opportunities
    fn build_item_sequence(
        &self,
        shaped: &[ShapedText],
        opportunities: &[BreakOpportunity],
    ) -> Result<Vec<Item>> {
        let mut items = Vec::new();
        let mut opp_iter = opportunities.iter().peekable();

        for (run_idx, run) in shaped.iter().enumerate() {
            for (glyph_idx, glyph) in run.glyphs.iter().enumerate() {
                // Check for break opportunities at this position
                while let Some(&&opp) = opp_iter.peek() {
                    if opp.position <= glyph.cluster {
                        items.push(Item::BreakOpportunity(opp));
                        opp_iter.next();
                    } else {
                        break;
                    }
                }

                // Add glyph
                items.push(Item::Glyph {
                    run_idx,
                    glyph_idx,
                    advance: glyph.x_advance,
                });
            }
        }

        // Add remaining break opportunities
        for &opp in opp_iter {
            items.push(Item::BreakOpportunity(opp));
        }

        Ok(items)
    }
}

/// Item in sequence (glyph or break opportunity)
#[derive(Debug, Clone, Copy)]
enum Item {
    Glyph {
        run_idx: usize,
        glyph_idx: usize,
        advance: f32,
    },
    BreakOpportunity(BreakOpportunity),
}

/// Builder for a single line
struct LineBuilder {
    max_width: f32,
    width: f32,
    segments: Vec<SegmentBuilder>,
}

impl LineBuilder {
    fn new(max_width: f32) -> Self {
        Self {
            max_width,
            width: 0.0,
            segments: Vec::new(),
        }
    }

    fn add_glyph(&mut self, run_idx: usize, glyph_idx: usize, advance: f32) {
        // Check if we need to start a new segment
        if self.segments.is_empty() || self.segments.last().unwrap().run_idx != run_idx {
            self.segments.push(SegmentBuilder {
                run_idx,
                glyph_start: glyph_idx,
                glyph_count: 0,
                width: 0.0,
            });
        }

        // Add to current segment
        let segment = self.segments.last_mut().unwrap();
        segment.glyph_count += 1;
        segment.width += advance;
        self.width += advance;
    }

    fn is_empty(&self) -> bool {
        self.segments.is_empty()
    }

    fn finish(self, shaped: &[ShapedText], is_last: bool) -> Line {
        let mut line_segments = Vec::new();
        let mut x_offset = 0.0;

        for segment_builder in self.segments {
            let shaped_run = &shaped[segment_builder.run_idx];

            line_segments.push(LineSegment {
                shaped: shaped_run.clone(),
                glyph_start: segment_builder.glyph_start,
                glyph_count: segment_builder.glyph_count,
                x_offset,
                width: segment_builder.width,
                has_hyphen: false,
            });

            x_offset += segment_builder.width;
        }

        Line {
            segments: line_segments,
            width: self.width,
            max_width: self.max_width,
            is_last,
        }
    }
}

/// Builder for a line segment
struct SegmentBuilder {
    run_idx: usize,
    glyph_start: usize,
    glyph_count: usize,
    width: f32,
}

impl Default for GreedyLineBreaker {
    fn default() -> Self {
        Self::new()
    }
}
```

### Step 4: Implement Optimal Line Breaking (Day 4-5)

**File: `src/text/linebreak/optimal.rs`**

```rust
//! Optimal line breaking using Knuth-Plass algorithm
//!
//! Reference: "Breaking Paragraphs into Lines" by Knuth and Plass (1981)
//! https://www.eprg.org/G53DOC/pdfs/knuth-plass-breaking.pdf

use super::{Line, LineSegment, BreakOpportunity, BreakType};
use crate::text::shaping::ShapedText;
use crate::style::ComputedStyle;
use crate::error::Result;

/// Optimal line breaker
///
/// Uses dynamic programming to find the optimal set of breakpoints
/// that minimizes the total penalty for the paragraph.
pub struct OptimalLineBreaker {
    /// Penalty for consecutive hyphens
    double_hyphen_penalty: i32,

    /// Penalty for final line being too short
    final_line_penalty: i32,

    /// Fitness class differences penalty
    fitness_penalty: i32,
}

impl OptimalLineBreaker {
    pub fn new() -> Self {
        Self {
            double_hyphen_penalty: 1000,
            final_line_penalty: 100,
            fitness_penalty: 100,
        }
    }

    /// Break lines optimally
    pub fn break_lines(
        &self,
        shaped: &[ShapedText],
        opportunities: &[BreakOpportunity],
        max_width: f32,
        style: &ComputedStyle,
    ) -> Result<Vec<Line>> {
        // Build nodes (positions where we can break)
        let nodes = self.build_nodes(shaped, opportunities)?;

        // Run dynamic programming to find optimal path
        let breakpoints = self.find_optimal_breakpoints(&nodes, max_width)?;

        // Build lines from breakpoints
        let lines = self.build_lines_from_breakpoints(shaped, &nodes, &breakpoints, max_width)?;

        Ok(lines)
    }

    /// Build nodes from shaped text and opportunities
    fn build_nodes(
        &self,
        shaped: &[ShapedText],
        opportunities: &[BreakOpportunity],
    ) -> Result<Vec<Node>> {
        let mut nodes = vec![Node {
            position: 0,
            width: 0.0,
            penalty: 0,
            break_type: BreakType::Hard,
        }];

        // TODO: Build nodes from glyphs and opportunities

        Ok(nodes)
    }

    /// Find optimal breakpoints using dynamic programming
    fn find_optimal_breakpoints(&self, nodes: &[Node], max_width: f32) -> Result<Vec<usize>> {
        let n = nodes.len();

        // DP state: minimum total penalty to reach each node
        let mut min_penalty = vec![i32::MAX; n];
        min_penalty[0] = 0;

        // Backpointers for reconstructing path
        let mut prev = vec![None; n];

        // For each node
        for i in 0..n {
            if min_penalty[i] == i32::MAX {
                continue; // Unreachable
            }

            // Try breaking to each subsequent node
            for j in (i + 1)..n {
                let line_width = nodes[j].width - nodes[i].width;

                if line_width > max_width {
                    // Line too long - stop searching from this start
                    break;
                }

                // Compute penalty for this line
                let line_penalty = self.compute_line_penalty(
                    line_width,
                    max_width,
                    nodes[j].penalty,
                    j == n - 1,
                );

                // Update if better path
                let total_penalty = min_penalty[i] + line_penalty;
                if total_penalty < min_penalty[j] {
                    min_penalty[j] = total_penalty;
                    prev[j] = Some(i);
                }
            }
        }

        // Reconstruct path
        let mut breakpoints = Vec::new();
        let mut current = n - 1;

        while let Some(p) = prev[current] {
            breakpoints.push(current);
            current = p;
        }

        breakpoints.reverse();

        Ok(breakpoints)
    }

    /// Compute penalty for a single line
    fn compute_line_penalty(
        &self,
        line_width: f32,
        max_width: f32,
        break_penalty: i32,
        is_last: bool,
    ) -> i32 {
        // Badness based on how well the line fills the space
        let ratio = line_width / max_width;

        let badness = if ratio < 0.9 {
            // Line is too short
            ((1.0 - ratio) * 100.0) as i32
        } else if ratio > 1.0 {
            // Line is too long (shouldn't happen if checking properly)
            10000
        } else {
            // Good line
            0
        };

        // Special handling for last line
        let final_penalty = if is_last && ratio < 0.75 {
            self.final_line_penalty
        } else {
            0
        };

        badness + break_penalty + final_penalty
    }

    /// Build lines from breakpoints
    fn build_lines_from_breakpoints(
        &self,
        shaped: &[ShapedText],
        nodes: &[Node],
        breakpoints: &[usize],
        max_width: f32,
    ) -> Result<Vec<Line>> {
        let mut lines = Vec::new();

        // TODO: Build actual line segments

        Ok(lines)
    }
}

/// A node in the line breaking graph
#[derive(Debug, Clone)]
struct Node {
    /// Position in text (glyph index)
    position: usize,

    /// Cumulative width up to this point
    width: f32,

    /// Penalty for breaking at this point
    penalty: i32,

    /// Type of break
    break_type: BreakType,
}

impl Default for OptimalLineBreaker {
    fn default() -> Self {
        Self::new()
    }
}
```

### Step 5: Implement Hyphenation (Day 6)

**File: `src/text/linebreak/hyphenate.rs`**

```rust
//! Hyphenation support
//!
//! Uses hyphenation crate for language-specific hyphenation.

use crate::error::{Result, Error};
use hyphenation::{Hyphenator as HyphenationDict, Load, Language, Standard};

/// Hyphenator
///
/// Finds hyphenation points in words.
pub struct Hyphenator {
    dict: Standard,
}

impl Hyphenator {
    /// Create hyphenator for language
    pub fn new(language: &str) -> Result<Self> {
        let lang = Self::parse_language(language)?;
        let dict = Standard::from_embedded(lang)
            .map_err(|e| Error::Text(format!("Failed to load hyphenation dict: {:?}", e)))?;

        Ok(Self { dict })
    }

    /// Find hyphenation points in text
    pub fn find_hyphenation_points(&self, text: &str) -> Result<Vec<usize>> {
        let mut points = Vec::new();

        // Split into words
        for (word_start, word) in self.split_words(text) {
            // Get hyphenation points for this word
            let word_points = self.dict.hyphenate(word);

            // Convert word-relative positions to text-relative
            for point in word_points.breaks {
                points.push(word_start + point);
            }
        }

        Ok(points)
    }

    /// Split text into words with positions
    fn split_words<'a>(&self, text: &'a str) -> Vec<(usize, &'a str)> {
        let mut words = Vec::new();
        let mut word_start = None;

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

    /// Parse language code
    fn parse_language(lang: &str) -> Result<Language> {
        match lang.to_lowercase().as_str() {
            "en" | "en-us" => Ok(Language::EnglishUS),
            "en-gb" => Ok(Language::EnglishGB),
            "de" | "de-de" => Ok(Language::German1996),
            "fr" | "fr-fr" => Ok(Language::French),
            "es" | "es-es" => Ok(Language::Spanish),
            "it" | "it-it" => Ok(Language::Italian),
            "pt" | "pt-br" => Ok(Language::PortugueseBrazil),
            "ru" | "ru-ru" => Ok(Language::Russian),
            "nl" | "nl-nl" => Ok(Language::Dutch),
            "pl" | "pl-pl" => Ok(Language::Polish),
            "sv" | "sv-se" => Ok(Language::Swedish),
            _ => Err(Error::Text(format!("Unsupported language: {}", lang))),
        }
    }
}
```

### Step 6: Implement Justification (Day 7-8)

**File: `src/text/linebreak/justify.rs`**

```rust
//! Text justification
//!
//! Distributes space to make lines flush with both margins.

use super::{Line, LineSegment};
use crate::style::ComputedStyle;
use crate::error::Result;

/// Justifier
///
/// Adjusts spacing in lines to achieve justified alignment.
pub struct Justifier;

impl Justifier {
    pub fn new() -> Self {
        Self
    }

    /// Justify lines
    pub fn justify_lines(
        &self,
        mut lines: Vec<Line>,
        max_width: f32,
        style: &ComputedStyle,
    ) -> Result<Vec<Line>> {
        for line in &mut lines {
            // Don't justify last line (unless CSS says otherwise)
            if line.is_last && style.text_align_last != Some(TextAlign::Justify) {
                continue;
            }

            // Don't justify if line is too short (< 75% of width)
            if line.width < max_width * 0.75 {
                continue;
            }

            self.justify_line(line, max_width)?;
        }

        Ok(lines)
    }

    /// Justify a single line
    fn justify_line(&self, line: &mut Line, max_width: f32) -> Result<()> {
        // Calculate how much space we need to add
        let extra_space = max_width - line.width;

        if extra_space <= 0.0 {
            return Ok(()); // Already full or too long
        }

        // Count word boundaries (spaces between words)
        let word_boundaries = self.count_word_boundaries(line);

        if word_boundaries == 0 {
            return Ok(()); // Single word, can't justify
        }

        // Distribute space evenly across word boundaries
        let space_per_boundary = extra_space / (word_boundaries as f32);

        // Adjust segment positions
        let mut current_x = 0.0;
        let mut boundaries_passed = 0;

        for segment in &mut line.segments {
            segment.x_offset = current_x;

            // Check if this segment ends with a word boundary
            if self.segment_ends_with_space(segment) {
                boundaries_passed += 1;
                current_x += segment.width + space_per_boundary;
            } else {
                current_x += segment.width;
            }
        }

        line.width = max_width;

        Ok(())
    }

    /// Count word boundaries in line
    fn count_word_boundaries(&self, line: &Line) -> usize {
        line.segments.iter()
            .filter(|seg| self.segment_ends_with_space(seg))
            .count()
    }

    /// Check if segment ends with space
    fn segment_ends_with_space(&self, segment: &LineSegment) -> bool {
        // Check if last glyph in segment is a space
        // TODO: Properly check glyph properties
        false // Placeholder
    }

    /// Distribute space using letter-spacing (fallback)
    ///
    /// Used when there are no word boundaries (e.g., CJK text)
    fn justify_with_letter_spacing(&self, line: &mut Line, max_width: f32) -> Result<()> {
        let extra_space = max_width - line.width;

        // Count total glyphs
        let total_glyphs: usize = line.segments.iter()
            .map(|seg| seg.glyph_count)
            .sum();

        if total_glyphs <= 1 {
            return Ok(());
        }

        let space_per_glyph = extra_space / ((total_glyphs - 1) as f32);

        // Adjust glyph advances
        // TODO: Modify glyph advances in shaped text

        Ok(())
    }
}

impl Default for Justifier {
    fn default() -> Self {
        Self::new()
    }
}
```

### Step 7: Comprehensive Tests (Day 9-10)

**File: `tests/text/linebreak_test.rs`**

```rust
//! Tests for line breaking

use fastrender::text::linebreak::*;
use fastrender::text::shaping::*;
use fastrender::style::*;

#[test]
fn test_simple_line_break() {
    let breaker = LineBreaker::new();

    let text = "The quick brown fox";
    let shaped = shape_text(text);

    let lines = breaker.break_lines(&shaped, 100.0, &default_style()).unwrap();

    // Should create multiple lines
    assert!(lines.len() > 1);
}

#[test]
fn test_hard_break() {
    let breaker = LineBreaker::new();

    let text = "Line 1\nLine 2";
    let shaped = shape_text(text);

    let lines = breaker.break_lines(&shaped, 1000.0, &default_style()).unwrap();

    // Should create exactly 2 lines due to hard break
    assert_eq!(lines.len(), 2);
}

#[test]
fn test_no_break_space() {
    let finder = BreakOpportunityFinder::new();

    let text = "Hello\u{00A0}World"; // Non-breaking space
    let opps = finder.find_opportunities(text, &default_style()).unwrap();

    // Should not have break opportunity at non-breaking space
    let has_break_at_nbsp = opps.iter()
        .any(|o| o.position == 5 && o.penalty < i32::MAX);

    assert!(!has_break_at_nbsp);
}

#[test]
fn test_word_break_break_all() {
    let finder = BreakOpportunityFinder::new();

    let mut style = default_style();
    style.word_break = WordBreak::BreakAll;

    let text = "Hello";
    let opps = finder.find_opportunities(text, &style).unwrap();

    // Should allow breaks between all characters
    assert!(opps.len() >= 5);
}

#[test]
fn test_word_break_keep_all() {
    let finder = BreakOpportunityFinder::new();

    let mut style = default_style();
    style.word_break = WordBreak::KeepAll;

    let text = "你好世界"; // Chinese text
    let opps = finder.find_opportunities(text, &style).unwrap();

    // Should not allow breaks within CJK text
    let has_cjk_break = opps.iter()
        .any(|o| o.penalty < i32::MAX);

    assert!(!has_cjk_break);
}

#[test]
fn test_hyphenation() {
    let breaker = LineBreaker::with_hyphenation("en-us").unwrap();

    let text = "supercalifragilisticexpialidocious";
    let shaped = shape_text(text);

    let lines = breaker.break_lines(&shaped, 100.0, &default_style()).unwrap();

    // Should hyphenate long word
    assert!(lines.len() > 1);

    // Some segment should have hyphen
    let has_hyphen = lines.iter()
        .flat_map(|line| &line.segments)
        .any(|seg| seg.has_hyphen);

    assert!(has_hyphen);
}

#[test]
fn test_justification() {
    let breaker = LineBreaker::new();

    let text = "The quick brown fox jumps over the lazy dog";
    let shaped = shape_text(text);

    let mut style = default_style();
    style.text_align = TextAlign::Justify;

    let lines = breaker.break_lines(&shaped, 200.0, &style).unwrap();

    // All lines except last should be exactly max_width
    for line in &lines[..lines.len() - 1] {
        assert!((line.width - 200.0).abs() < 0.1);
    }
}

#[test]
fn test_text_align_left() {
    let text = "Hello";
    let shaped = shape_text(text);
    let lines = vec![create_simple_line(&shaped, 100.0, 200.0)];

    let aligned = apply_alignment(&lines, TextAlign::Left);

    // First segment should start at x=0
    assert_eq!(aligned[0].segments[0].x_offset, 0.0);
}

#[test]
fn test_text_align_right() {
    let text = "Hello";
    let shaped = shape_text(text);
    let lines = vec![create_simple_line(&shaped, 100.0, 200.0)];

    let aligned = apply_alignment(&lines, TextAlign::Right);

    // First segment should start at x = max_width - line_width
    assert_eq!(aligned[0].segments[0].x_offset, 100.0);
}

#[test]
fn test_text_align_center() {
    let text = "Hello";
    let shaped = shape_text(text);
    let lines = vec![create_simple_line(&shaped, 100.0, 200.0)];

    let aligned = apply_alignment(&lines, TextAlign::Center);

    // First segment should start at x = (max_width - line_width) / 2
    assert_eq!(aligned[0].segments[0].x_offset, 50.0);
}

#[test]
fn test_overflow_wrap_anywhere() {
    let finder = BreakOpportunityFinder::new();

    let mut style = default_style();
    style.overflow_wrap = OverflowWrap::Anywhere;

    let text = "verylongwordwithnobreaks";
    let opps = finder.find_opportunities(text, &style).unwrap();

    // Should have emergency break opportunities everywhere
    assert!(opps.len() >= text.len());
}

#[test]
fn test_cjk_line_breaking() {
    let finder = BreakOpportunityFinder::new();

    let text = "这是一个测试"; // Chinese text
    let opps = finder.find_opportunities(text, &default_style()).unwrap();

    // Should allow breaks between CJK characters
    assert!(opps.len() > 0);
}

// Helper functions
fn shape_text(text: &str) -> Vec<ShapedText> {
    // TODO: Actually shape text
    vec![]
}

fn default_style() -> ComputedStyle {
    ComputedStyle {
        word_break: WordBreak::Normal,
        overflow_wrap: OverflowWrap::Normal,
        text_align: TextAlign::Left,
        ..Default::default()
    }
}

fn create_simple_line(shaped: &[ShapedText], width: f32, max_width: f32) -> Line {
    Line {
        segments: vec![],
        width,
        max_width,
        is_last: false,
    }
}

fn apply_alignment(lines: &[Line], align: TextAlign) -> Vec<Line> {
    // TODO: Apply alignment
    lines.to_vec()
}
```

## Acceptance Criteria

- [ ] Unicode line breaking (UAX #14) works correctly
- [ ] Breaks at word boundaries (spaces)
- [ ] Hard breaks work (<br>, \n)
- [ ] Non-breaking spaces prevent breaks (U+00A0)
- [ ] Soft hyphens work (U+00AD)
- [ ] Zero-width spaces allow breaks (U+200B)
- [ ] word-break: break-all works
- [ ] word-break: keep-all works
- [ ] overflow-wrap: anywhere works
- [ ] Hyphenation works for supported languages
- [ ] Text justification distributes space correctly
- [ ] Text alignment works (left, right, center, justify)
- [ ] CJK line breaking works
- [ ] Greedy line breaking produces valid lines
- [ ] Optimal line breaking produces better results than greedy
- [ ] All tests pass: `cargo test linebreak`
- [ ] Code follows 10-code-standards.md

## Common Pitfalls

### Pitfall 1: Breaking at Wrong Positions

**Wrong:**
```rust
// Breaking at every space
for ch in text.chars() {
    if ch == ' ' {
        break_here(); // Wrong! Spaces aren't the only break points
    }
}
```

**Right:**
```rust
// Use UAX #14 for correct break opportunities
let opportunities = linebreaks(text);
```

### Pitfall 2: Not Handling Non-Breaking Spaces

**Wrong:**
```rust
// Treating all spaces the same
if ch.is_whitespace() {
    allow_break(); // Wrong! U+00A0 is whitespace but non-breaking
}
```

**Right:**
```rust
// Check actual break properties
if ch == ' ' {
    allow_break();
} else if ch == '\u{00A0}' {
    no_break();
}
```

### Pitfall 3: Justifying Last Line

**Wrong:**
```rust
// Justifying all lines including last
for line in lines {
    justify(line);
}
```

**Right:**
```rust
// Don't justify last line (unless text-align-last says so)
for line in &lines[..lines.len()-1] {
    justify(line);
}
```

### Pitfall 4: Breaking Emoji Sequences

**Wrong:**
```rust
// Breaking at every character
for ch in text.chars() {
    if line_full() {
        break_here(); // Might break 👨‍👩‍👧‍👦 in the middle!
    }
}
```

**Right:**
```rust
// Detect grapheme clusters first
let clusters = detect_grapheme_clusters(text);
// Only break between clusters
```

## Performance Considerations

1. **Cache break opportunities** - UAX #14 analysis is expensive
2. **Use greedy for long documents** - Optimal breaking is O(n²)
3. **Lazy hyphenation** - Only hyphenate when needed
4. **Reuse shaped text** - Don't re-shape for different widths
5. **Incremental reflow** - Only rebreak changed paragraphs

## Integration Points

The line breaking system integrates with:

1. **Text shaping** - Uses shaped text as input
2. **Inline layout** - Provides lines for layout
3. **Font system** - Gets metrics for measurement
4. **Text rendering** - Lines are rendered to screen
5. **Hit testing** - Line structure enables click-to-position

## Next Steps

After line breaking is complete:
- **Integration with inline layout** - Use lines in actual layout
- **Text rendering** - Render glyphs to screen
- **Hit testing and selection** - Map screen positions to characters
- **Editing support** - Cursor positioning, text insertion

## References

- **Unicode Line Breaking (UAX #14):** https://www.unicode.org/reports/tr14/
- **CSS Text Module Level 3:** https://www.w3.org/TR/css-text-3/
- **CSS Text Module Level 4:** https://www.w3.org/TR/css-text-4/
- **Knuth-Plass Algorithm:** "Breaking Paragraphs into Lines" (1981)
- **unicode-linebreak documentation:** https://docs.rs/unicode-linebreak/
- **hyphenation documentation:** https://docs.rs/hyphenation/
- **Line Breaking in East Asian Scripts:** https://www.w3.org/TR/jlreq/
- **CSS Text Layout: Typography on the Web:** Various articles

---

**Last Updated:** 2025-11-19
**Status:** Ready for Implementation
