//! Line building for inline formatting context
//!
//! This module handles the construction of line boxes from inline-level content.
//! It manages line breaking, inline item accumulation, and fragment positioning.
//!
//! # CSS Specification
//!
//! CSS 2.1 Section 9.4.2 - Inline formatting contexts:
//! <https://www.w3.org/TR/CSS21/visuren.html#inline-formatting>
//!
//! # Line Box Construction
//!
//! The line builder accumulates inline items (text runs, inline boxes, replaced elements)
//! and breaks them into lines when:
//!
//! 1. A mandatory line break occurs (newline character, `<br>`)
//! 2. Content exceeds available width and a break opportunity exists
//! 3. The inline content ends
//!
//! # Algorithm Overview
//!
//! ```text
//! For each inline item:
//!   1. Measure item width
//!   2. If item fits on current line -> add to line
//!   3. If item doesn't fit:
//!      a. Find break opportunity within item (for text)
//!      b. If no break found, check if line is empty
//!      c. If line empty, force item onto line (overflow)
//!      d. Otherwise, finalize line and start new one
//!   4. After all items, finalize last line
//! ```

use super::baseline::{BaselineMetrics, LineBaselineAccumulator, VerticalAlign};
use crate::geometry::Size;
use crate::style::types::{Direction, UnicodeBidi};
use crate::style::ComputedStyle;
use crate::text::font_loader::FontContext;
use crate::text::line_break::{BreakOpportunity, BreakType};
use crate::text::pipeline::{ShapedRun, ShapingPipeline};
use crate::tree::box_tree::ReplacedType;
use crate::tree::fragment_tree::FragmentNode;
use std::collections::BTreeMap;
use std::sync::Arc;
use unicode_bidi::{BidiInfo, Level};

/// An item in the inline formatting context
///
/// Represents different types of content that can appear inline.
#[derive(Debug, Clone)]
pub enum InlineItem {
    /// Shaped text ready for layout
    Text(TextItem),

    /// An inline box (span, a, em, etc.) with children
    InlineBox(InlineBoxItem),

    /// An inline-block box (atomic inline)
    InlineBlock(InlineBlockItem),

    /// A replaced element (img, canvas, etc.)
    Replaced(ReplacedItem),
}

impl InlineItem {
    /// Returns the width of this item
    pub fn width(&self) -> f32 {
        match self {
            InlineItem::Text(t) => t.advance,
            InlineItem::InlineBox(b) => b.width(),
            InlineItem::InlineBlock(b) => b.width,
            InlineItem::Replaced(r) => r.width,
        }
    }

    /// Returns baseline metrics for this item
    pub fn baseline_metrics(&self) -> BaselineMetrics {
        match self {
            InlineItem::Text(t) => t.metrics,
            InlineItem::InlineBox(b) => b.metrics,
            InlineItem::InlineBlock(b) => b.metrics,
            InlineItem::Replaced(r) => r.metrics,
        }
    }

    /// Returns the vertical alignment for this item
    pub fn vertical_align(&self) -> VerticalAlign {
        match self {
            InlineItem::Text(t) => t.vertical_align,
            InlineItem::InlineBox(b) => b.vertical_align,
            InlineItem::InlineBlock(b) => b.vertical_align,
            InlineItem::Replaced(r) => r.vertical_align,
        }
    }

    /// Returns true if this item can be broken (for text)
    pub fn is_breakable(&self) -> bool {
        matches!(self, InlineItem::Text(_))
    }

    pub fn direction(&self) -> Direction {
        match self {
            InlineItem::Text(t) => t.style.direction,
            InlineItem::InlineBox(b) => b.direction,
            InlineItem::InlineBlock(b) => b.direction,
            InlineItem::Replaced(r) => r.direction,
        }
    }

    pub fn unicode_bidi(&self) -> UnicodeBidi {
        match self {
            InlineItem::Text(t) => t.style.unicode_bidi,
            InlineItem::InlineBox(b) => b.unicode_bidi,
            InlineItem::InlineBlock(b) => b.unicode_bidi,
            InlineItem::Replaced(r) => r.unicode_bidi,
        }
    }
}

/// A shaped text item
#[derive(Debug, Clone)]
pub struct TextItem {
    /// The shaped runs (bidi/script/font-aware)
    pub runs: Vec<ShapedRun>,

    /// Total horizontal advance
    pub advance: f32,

    /// Baseline metrics
    pub metrics: BaselineMetrics,

    /// Vertical alignment
    pub vertical_align: VerticalAlign,

    /// Break opportunities within this text
    pub break_opportunities: Vec<BreakOpportunity>,

    /// Original text for fragment creation
    pub text: String,

    /// Font size used
    pub font_size: f32,

    /// Computed style for this text run
    pub style: Arc<ComputedStyle>,
    /// Cumulative advances at cluster boundaries (text order)
    cluster_advances: Vec<ClusterBoundary>,
}

#[derive(Debug, Clone)]
struct ClusterBoundary {
    /// Byte offset in the source text where this cluster starts
    byte_offset: usize,
    /// Advance width from the start of the item up to and including this cluster
    advance: f32,
}

impl TextItem {
    /// Creates a new text item
    pub fn new(
        runs: Vec<ShapedRun>,
        text: String,
        metrics: BaselineMetrics,
        break_opportunities: Vec<BreakOpportunity>,
        style: Arc<ComputedStyle>,
    ) -> Self {
        let cluster_advances = Self::compute_cluster_advances(&runs, text.len(), style.font_size);
        let aligned_breaks = Self::align_breaks_to_clusters(break_opportunities, &cluster_advances, text.len());
        let advance: f32 = cluster_advances
            .last()
            .map(|c| c.advance)
            .unwrap_or_else(|| runs.iter().map(|r| r.advance).sum());
        let font_size = style.font_size;
        Self {
            runs,
            advance,
            metrics,
            vertical_align: VerticalAlign::Baseline,
            break_opportunities: aligned_breaks,
            text,
            font_size,
            style,
            cluster_advances,
        }
    }

    /// Derive baseline metrics from shaped runs and CSS line-height
    pub fn metrics_from_runs(runs: &[ShapedRun], line_height: f32, fallback_font_size: f32) -> BaselineMetrics {
        let mut ascent: f32 = 0.0;
        let mut descent: f32 = 0.0;
        let mut line_gap: f32 = 0.0;

        for run in runs {
            if let Ok(metrics) = run.font.metrics() {
                let scaled = metrics.scale(run.font_size);
                ascent = ascent.max(scaled.ascent);
                descent = descent.max(scaled.descent);
                line_gap = line_gap.max(scaled.line_gap);
            }
        }

        if ascent == 0.0 && descent == 0.0 {
            ascent = fallback_font_size * 0.8;
            descent = fallback_font_size * 0.2;
        }

        BaselineMetrics {
            baseline_offset: ascent,
            height: line_height,
            ascent,
            descent,
            line_gap,
            line_height,
        }
    }

    /// Sets the vertical alignment
    pub fn with_vertical_align(mut self, align: VerticalAlign) -> Self {
        self.vertical_align = align;
        self
    }

    /// Splits this text item at a byte offset, returning (before, after)
    ///
    /// This is used for line breaking within text.
    pub fn split_at(
        &self,
        byte_offset: usize,
        shaper: &ShapingPipeline,
        font_context: &FontContext,
    ) -> Option<(TextItem, TextItem)> {
        let split_offset = self.cluster_boundary_at_or_before(byte_offset).map(|b| b.byte_offset)?;

        if split_offset == 0 || split_offset >= self.text.len() {
            return None;
        }

        // Split the text
        let (before_text, after_text) = self.text.split_at(split_offset);

        let (before_runs, after_runs) = self.split_runs_preserving_shaping(split_offset).or_else(|| {
            let before_runs = shaper.shape(before_text, &self.style, font_context).ok()?;
            let after_runs = shaper.shape(after_text, &self.style, font_context).ok()?;
            Some((before_runs, after_runs))
        })?;

        let line_height = self.metrics.line_height;
        let before_metrics = TextItem::metrics_from_runs(&before_runs, line_height, self.font_size);
        let after_metrics = TextItem::metrics_from_runs(&after_runs, line_height, self.font_size);

        let before_breaks = self
            .break_opportunities
            .iter()
            .filter(|b| b.byte_offset <= split_offset)
            .copied()
            .collect();
        let after_breaks = self
            .break_opportunities
            .iter()
            .filter(|b| b.byte_offset > split_offset)
            .map(|b| BreakOpportunity::new(b.byte_offset - split_offset, b.break_type))
            .collect();

        // Create new items using freshly shaped runs
        let before_item = TextItem::new(
            before_runs,
            before_text.to_string(),
            before_metrics,
            before_breaks,
            self.style.clone(),
        )
        .with_vertical_align(self.vertical_align);

        let after_item = TextItem::new(
            after_runs,
            after_text.to_string(),
            after_metrics,
            after_breaks,
            self.style.clone(),
        )
        .with_vertical_align(self.vertical_align);

        Some((before_item, after_item))
    }

    /// Gets the horizontal advance at a given byte offset
    pub fn advance_at_offset(&self, byte_offset: usize) -> f32 {
        if byte_offset == 0 {
            return 0.0;
        }
        if byte_offset >= self.text.len() {
            return self.advance;
        }

        if let Some(boundary) = self.cluster_boundary_at_or_before(byte_offset) {
            return boundary.advance;
        }

        if self.runs.is_empty() {
            let char_count = self.text.chars().count().max(1);
            let offset_chars = self.text[..byte_offset].chars().count();
            return self.advance * (offset_chars as f32 / char_count as f32);
        }

        // Fallback: linear approximation if cluster data is missing
        let char_count = self.text.chars().count().max(1);
        let offset_chars = self.text[..byte_offset].chars().count();
        self.advance * (offset_chars as f32 / char_count as f32)
    }

    /// Finds the best break point that fits within max_width
    pub fn find_break_point(&self, max_width: f32) -> Option<usize> {
        // Find the last break opportunity that fits
        let mut best_break = None;

        for brk in &self.break_opportunities {
            let width_at_break = self.advance_at_offset(brk.byte_offset);
            if width_at_break <= max_width {
                best_break = Some(brk.byte_offset);
            } else {
                break;
            }
        }

        best_break
    }

    fn cluster_boundary_at_or_before(&self, byte_offset: usize) -> Option<ClusterBoundary> {
        if self.cluster_advances.is_empty() {
            return None;
        }

        let mut idx = match self
            .cluster_advances
            .binary_search_by_key(&byte_offset, |c| c.byte_offset)
        {
            Ok(i) => i,
            Err(i) => i.checked_sub(1)?,
        };

        // If there are multiple entries for the same offset, keep the one with the largest advance
        while idx + 1 < self.cluster_advances.len()
            && self.cluster_advances[idx + 1].byte_offset == self.cluster_advances[idx].byte_offset
        {
            idx += 1;
        }

        self.cluster_advances.get(idx).cloned()
    }

    fn compute_cluster_advances(runs: &[ShapedRun], text_len: usize, fallback_font_size: f32) -> Vec<ClusterBoundary> {
        if runs.is_empty() || text_len == 0 {
            return Vec::new();
        }

        let mut sorted_runs: Vec<&ShapedRun> = runs.iter().collect();
        sorted_runs.sort_by_key(|r| r.start);

        let mut advances = Vec::new();
        let mut cumulative = 0.0;

        for run in sorted_runs {
            let mut cluster_widths: BTreeMap<usize, f32> = BTreeMap::new();
            for glyph in &run.glyphs {
                let offset = run.start + glyph.cluster as usize;
                *cluster_widths.entry(offset).or_default() += glyph.x_advance;
            }

            if cluster_widths.is_empty() {
                // If shaping produced no glyphs, fall back to treating the entire run as a single cluster
                cluster_widths.insert(run.start, run.advance.max(0.0));
            }

            let last_offset = *cluster_widths.keys().next_back().unwrap_or(&run.end);
            if last_offset < run.end {
                cluster_widths.entry(run.end).or_insert(0.0);
            }

            for (offset, width) in cluster_widths {
                cumulative += width;
                advances.push(ClusterBoundary {
                    byte_offset: offset.min(text_len),
                    advance: cumulative,
                });
            }
        }

        // Deduplicate by byte offset, keeping the greatest advance so that cumulative width remains monotonic
        let mut deduped: Vec<ClusterBoundary> = Vec::new();
        for boundary in advances {
            if let Some(last) = deduped.last_mut() {
                if last.byte_offset == boundary.byte_offset {
                    if boundary.advance > last.advance {
                        last.advance = boundary.advance;
                    }
                    continue;
                }
            }
            deduped.push(boundary);
        }

        if deduped.last().map(|b| b.byte_offset < text_len).unwrap_or(false) {
            deduped.push(ClusterBoundary {
                byte_offset: text_len,
                advance: deduped.last().map(|b| b.advance).unwrap_or(0.0),
            });
        }

        if deduped.is_empty() {
            deduped.push(ClusterBoundary {
                byte_offset: text_len,
                advance: fallback_font_size,
            });
        }

        deduped
    }

    fn align_breaks_to_clusters(
        breaks: Vec<BreakOpportunity>,
        clusters: &[ClusterBoundary],
        text_len: usize,
    ) -> Vec<BreakOpportunity> {
        if clusters.is_empty() || text_len == 0 {
            return breaks;
        }

        let mut aligned: Vec<BreakOpportunity> = Vec::new();
        for brk in breaks {
            let clamped_offset = brk.byte_offset.min(text_len);
            let aligned_offset = Self::cluster_offset_at_or_before(clamped_offset, clusters);
            if let Some(offset) = aligned_offset {
                if let Some(last) = aligned.last_mut() {
                    if last.byte_offset == offset {
                        if brk.break_type == BreakType::Mandatory {
                            last.break_type = BreakType::Mandatory;
                        }
                        continue;
                    }
                }

                aligned.push(BreakOpportunity::new(offset, brk.break_type));
            }
        }

        aligned
    }

    fn cluster_offset_at_or_before(target: usize, clusters: &[ClusterBoundary]) -> Option<usize> {
        if clusters.is_empty() {
            return None;
        }

        match clusters.binary_search_by_key(&target, |c| c.byte_offset) {
            Ok(idx) => clusters.get(idx).map(|c| c.byte_offset),
            Err(idx) => clusters.get(idx.checked_sub(1)?).map(|c| c.byte_offset),
        }
    }

    /// Splits existing shaped runs at a cluster boundary without reshaping, preserving ligatures/glyph IDs.
    fn split_runs_preserving_shaping(&self, split_offset: usize) -> Option<(Vec<ShapedRun>, Vec<ShapedRun>)> {
        let mut before_runs = Vec::new();
        let mut after_runs = Vec::new();

        for run in &self.runs {
            if split_offset <= run.start {
                // Entire run goes to the after side
                after_runs.push(run.clone());
                continue;
            }
            if split_offset >= run.end {
                // Entire run goes to the before side
                before_runs.push(run.clone());
                continue;
            }

            // Split within this run
            let local = split_offset - run.start;

            // Guard against invalid UTF-8 boundaries
            if local > run.text.len() {
                return None;
            }

            let (left_text, right_text) = run.text.split_at(local);

            let mut left_glyphs = Vec::new();
            let mut right_glyphs = Vec::new();

            for glyph in &run.glyphs {
                if (glyph.cluster as usize) < local {
                    left_glyphs.push(*glyph);
                } else {
                    right_glyphs.push(*glyph);
                }
            }

            let left_advance: f32 = left_glyphs.iter().map(|g| g.x_advance).sum();

            // Adjust right glyph offsets/clusters to be relative to the split
            for glyph in &mut right_glyphs {
                glyph.x_offset -= left_advance;
                glyph.cluster = glyph.cluster.saturating_sub(local as u32);
            }

            if !left_glyphs.is_empty() {
                let mut left_run = run.clone();
                left_run.text = left_text.to_string();
                left_run.end = split_offset;
                left_run.glyphs = left_glyphs;
                left_run.advance = left_advance;
                before_runs.push(left_run);
            }

            if !right_glyphs.is_empty() {
                let mut right_run = run.clone();
                right_run.text = right_text.to_string();
                right_run.start = split_offset;
                right_run.glyphs = right_glyphs;
                right_run.advance = right_run.glyphs.iter().map(|g| g.x_advance).sum();
                after_runs.push(right_run);
            }
        }

        Some((before_runs, after_runs))
    }
}

/// An inline box item (non-atomic, contains children)
#[derive(Debug, Clone)]
pub struct InlineBoxItem {
    /// Child items within this inline box
    pub children: Vec<InlineItem>,

    /// Opening edge width (left border + padding)
    pub start_edge: f32,

    /// Closing edge width (right border + padding)
    pub end_edge: f32,

    /// Baseline metrics for this box
    pub metrics: BaselineMetrics,

    /// Vertical alignment
    pub vertical_align: VerticalAlign,

    /// Index for fragment creation
    pub box_index: usize,

    /// Bidi direction of this inline box
    pub direction: Direction,

    /// unicode-bidi behavior
    pub unicode_bidi: UnicodeBidi,
}

impl InlineBoxItem {
    /// Creates a new inline box item
    pub fn new(
        start_edge: f32,
        end_edge: f32,
        metrics: BaselineMetrics,
        box_index: usize,
        direction: Direction,
        unicode_bidi: UnicodeBidi,
    ) -> Self {
        Self {
            children: Vec::new(),
            start_edge,
            end_edge,
            metrics,
            vertical_align: VerticalAlign::Baseline,
            box_index,
            direction,
            unicode_bidi,
        }
    }

    /// Adds a child item
    pub fn add_child(&mut self, child: InlineItem) {
        self.children.push(child);
    }

    /// Returns the total width of this inline box
    pub fn width(&self) -> f32 {
        let content_width: f32 = self.children.iter().map(|c| c.width()).sum();
        self.start_edge + content_width + self.end_edge
    }
}

/// An inline-block item (atomic inline)
#[derive(Debug, Clone)]
pub struct InlineBlockItem {
    /// The laid-out fragment
    pub fragment: FragmentNode,

    /// Width of the inline-block
    pub width: f32,

    /// Height of the inline-block
    pub height: f32,

    /// Baseline metrics
    pub metrics: BaselineMetrics,

    /// Vertical alignment
    pub vertical_align: VerticalAlign,

    /// Bidi direction
    pub direction: Direction,

    /// unicode-bidi behavior
    pub unicode_bidi: UnicodeBidi,
}

impl InlineBlockItem {
    /// Creates a new inline-block item
    pub fn new(fragment: FragmentNode, direction: Direction, unicode_bidi: UnicodeBidi) -> Self {
        let width = fragment.bounds.width();
        let height = fragment.bounds.height();
        let metrics = BaselineMetrics::for_replaced(height);

        Self {
            fragment,
            width,
            height,
            metrics,
            vertical_align: VerticalAlign::Baseline,
            direction,
            unicode_bidi,
        }
    }

    /// Sets the vertical alignment
    pub fn with_vertical_align(mut self, align: VerticalAlign) -> Self {
        self.vertical_align = align;
        self
    }
}

/// A replaced element item (img, canvas, etc.)
#[derive(Debug, Clone)]
pub struct ReplacedItem {
    /// Width of the element
    pub width: f32,

    /// Height of the element
    pub height: f32,

    /// Baseline metrics
    pub metrics: BaselineMetrics,

    /// Vertical alignment
    pub vertical_align: VerticalAlign,

    /// Original replaced type (img, video, etc.)
    pub replaced_type: ReplacedType,

    /// Computed style for painting
    pub style: Arc<ComputedStyle>,

    /// Bidi direction
    pub direction: Direction,

    /// unicode-bidi behavior
    pub unicode_bidi: UnicodeBidi,
}

impl ReplacedItem {
    /// Creates a new replaced item
    pub fn new(size: Size, replaced_type: ReplacedType, style: Arc<ComputedStyle>) -> Self {
        let metrics = BaselineMetrics::for_replaced(size.height);
        Self {
            width: size.width,
            height: size.height,
            metrics,
            vertical_align: VerticalAlign::Baseline,
            replaced_type,
            direction: style.direction,
            unicode_bidi: style.unicode_bidi,
            style,
        }
    }

    /// Sets the vertical alignment
    pub fn with_vertical_align(mut self, align: VerticalAlign) -> Self {
        self.vertical_align = align;
        self
    }
}

/// A positioned item within a line
#[derive(Debug, Clone)]
pub struct PositionedItem {
    /// The inline item
    pub item: InlineItem,

    /// X position relative to line start
    pub x: f32,

    /// Y offset from line baseline (positive = down)
    pub baseline_offset: f32,
}

/// A completed line box
#[derive(Debug, Clone)]
pub struct Line {
    /// Positioned items in this line
    pub items: Vec<PositionedItem>,

    /// Total width used by items
    pub width: f32,

    /// Line height
    pub height: f32,

    /// Baseline position from top of line box
    pub baseline: f32,

    /// Whether this line ends with a hard break
    pub ends_with_hard_break: bool,
}

impl Line {
    /// Creates an empty line
    pub fn new() -> Self {
        Self {
            items: Vec::new(),
            width: 0.0,
            height: 0.0,
            baseline: 0.0,
            ends_with_hard_break: false,
        }
    }

    /// Returns true if the line has no items
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }
}

impl Default for Line {
    fn default() -> Self {
        Self::new()
    }
}

/// Builder for constructing lines from inline items
///
/// Handles line breaking and item positioning.
pub struct LineBuilder {
    /// Available width for lines
    available_width: f32,

    /// Current line being built
    current_line: Line,

    /// Current X position
    current_x: f32,

    /// Completed lines
    lines: Vec<Line>,

    /// Baseline accumulator for current line
    baseline_acc: LineBaselineAccumulator,

    /// Default strut metrics (from containing block)
    strut_metrics: BaselineMetrics,

    /// Shaping pipeline for text splitting
    shaper: ShapingPipeline,

    /// Font context for reshaping during line breaks
    font_context: FontContext,

    /// Base paragraph level for bidi ordering (None = auto/first strong)
    base_level: Option<Level>,
}

impl LineBuilder {
    /// Creates a new line builder
    pub fn new(
        available_width: f32,
        strut_metrics: BaselineMetrics,
        shaper: ShapingPipeline,
        font_context: FontContext,
        base_level: Option<Level>,
    ) -> Self {
        Self {
            available_width,
            current_line: Line::new(),
            current_x: 0.0,
            lines: Vec::new(),
            baseline_acc: LineBaselineAccumulator::new(&strut_metrics),
            strut_metrics,
            shaper,
            font_context,
            base_level,
        }
    }

    /// Adds an inline item to the builder
    pub fn add_item(&mut self, item: InlineItem) {
        let item_width = item.width();

        // Check if item fits
        if self.current_x + item_width <= self.available_width || self.current_line.is_empty() {
            // Item fits (or line is empty, so we must take it)
            self.place_item(item);
        } else if item.is_breakable() {
            // Try to break the item
            self.add_breakable_item(item);
        } else {
            // Item doesn't fit and can't be broken - start new line
            self.finish_line();
            self.place_item(item);
        }
    }

    /// Adds a breakable item (text), handling line breaking
    fn add_breakable_item(&mut self, item: InlineItem) {
        if let InlineItem::Text(text_item) = item {
            let remaining_width = (self.available_width - self.current_x).max(0.0);

            if let Some(break_point) = text_item.find_break_point(remaining_width) {
                // Split at break point
                if let Some((before, after)) = text_item.split_at(break_point, &self.shaper, &self.font_context) {
                    // Place the part that fits
                    if before.advance > 0.0 {
                        self.place_item(InlineItem::Text(before));
                    }

                    // Start new line for the rest
                    self.finish_line();

                    // Add remaining text (may need further breaking)
                    self.add_item(InlineItem::Text(after));
                } else {
                    // If splitting fails, fall back to placing the whole item
                    if self.current_line.is_empty() {
                        self.place_item(InlineItem::Text(text_item));
                    } else {
                        self.finish_line();
                        self.place_item(InlineItem::Text(text_item));
                    }
                }
            } else {
                // No break point found within remaining width
                if self.current_line.is_empty() {
                    // Line is empty, must place item (overflow)
                    self.place_item(InlineItem::Text(text_item));
                } else {
                    // Start new line and try again
                    self.finish_line();
                    self.add_item(InlineItem::Text(text_item));
                }
            }
        }
    }

    /// Places an item on the current line without breaking
    fn place_item(&mut self, item: InlineItem) {
        let metrics = item.baseline_metrics();
        let vertical_align = item.vertical_align();

        // Calculate baseline offset
        let baseline_offset = if vertical_align.is_line_relative() {
            self.baseline_acc.add_line_relative(&metrics, vertical_align);
            0.0 // Will be adjusted in finalization
        } else {
            self.baseline_acc.add_baseline_relative(&metrics, vertical_align, None)
        };

        let positioned = PositionedItem {
            item,
            x: self.current_x,
            baseline_offset,
        };

        self.current_x += positioned.item.width();
        self.current_line.items.push(positioned);
    }

    /// Forces a line break (e.g., from mandatory break)
    pub fn force_break(&mut self) {
        self.current_line.ends_with_hard_break = true;
        self.finish_line();
    }

    /// Finishes the current line and starts a new one
    fn finish_line(&mut self) {
        if !self.current_line.is_empty() {
            // Reorder items visually according to the bidi algorithm (CSS Text 8.2)
            self.reorder_line_for_bidi();

            // Calculate final line metrics
            self.current_line.width = self.current_x;
            self.current_line.height = self.baseline_acc.line_height();
            self.current_line.baseline = self.baseline_acc.baseline_position();

            // Adjust Y positions for top/bottom aligned items
            for positioned in &mut self.current_line.items {
                let align = positioned.item.vertical_align();
                match align {
                    VerticalAlign::Top => {
                        positioned.baseline_offset =
                            positioned.item.baseline_metrics().baseline_offset - self.current_line.baseline;
                    }
                    VerticalAlign::Bottom => {
                        let metrics = positioned.item.baseline_metrics();
                        positioned.baseline_offset = self.current_line.height
                            - metrics.height
                            - (self.current_line.baseline - metrics.baseline_offset);
                    }
                    _ => {}
                }
            }

            self.lines.push(std::mem::take(&mut self.current_line));
        }

        // Reset for new line
        self.current_x = 0.0;
        self.baseline_acc = LineBaselineAccumulator::new(&self.strut_metrics);
        self.current_line = Line::new();
    }

    /// Reorders the current line's items into visual order based on the Unicode Bidi algorithm.
    ///
    /// This models CSS Text 3 bidi reordering for inline-level boxes. It treats non-text items as
    /// object replacement characters (U+FFFC) to ensure they participate in reordering.
    fn reorder_line_for_bidi(&mut self) {
        if self.current_line.items.is_empty() {
            return;
        }

        // Build the logical text for this line and map byte ranges back to item indices.
        let mut logical_text = String::new();
        let mut spans: Vec<(usize, std::ops::Range<usize>)> = Vec::with_capacity(self.current_line.items.len());

        for (idx, positioned) in self.current_line.items.iter().enumerate() {
            let dir = positioned.item.direction();
            let ub = positioned.item.unicode_bidi();
            let content_start;
            let content_end;

            match ub {
                UnicodeBidi::Normal => {
                    content_start = logical_text.len();
                    self.push_item_content(&positioned.item, &mut logical_text);
                    content_end = logical_text.len();
                }
                UnicodeBidi::Embed => {
                    logical_text.push(if dir == Direction::Rtl { '\u{202b}' } else { '\u{202a}' });
                    content_start = logical_text.len();
                    self.push_item_content(&positioned.item, &mut logical_text);
                    content_end = logical_text.len();
                    logical_text.push('\u{202c}'); // PDF
                }
                UnicodeBidi::BidiOverride => {
                    logical_text.push(if dir == Direction::Rtl { '\u{202e}' } else { '\u{202d}' });
                    content_start = logical_text.len();
                    self.push_item_content(&positioned.item, &mut logical_text);
                    content_end = logical_text.len();
                    logical_text.push('\u{202c}'); // PDF
                }
                UnicodeBidi::Isolate => {
                    logical_text.push(if dir == Direction::Rtl { '\u{2067}' } else { '\u{2066}' });
                    content_start = logical_text.len();
                    self.push_item_content(&positioned.item, &mut logical_text);
                    content_end = logical_text.len();
                    logical_text.push('\u{2069}'); // PDI
                }
                UnicodeBidi::IsolateOverride => {
                    logical_text.push(if dir == Direction::Rtl { '\u{2067}' } else { '\u{2066}' });
                    logical_text.push(if dir == Direction::Rtl { '\u{202e}' } else { '\u{202d}' });
                    content_start = logical_text.len();
                    self.push_item_content(&positioned.item, &mut logical_text);
                    content_end = logical_text.len();
                    logical_text.push('\u{202c}'); // PDF
                    logical_text.push('\u{2069}'); // PDI
                }
                UnicodeBidi::Plaintext => {
                    logical_text.push('\u{2068}'); // FSI
                    content_start = logical_text.len();
                    self.push_item_content(&positioned.item, &mut logical_text);
                    content_end = logical_text.len();
                    logical_text.push('\u{2069}'); // PDI
                }
            }

            if content_start < content_end {
                spans.push((idx, content_start..content_end));
            }
        }

        if logical_text.is_empty() {
            return;
        }

        let bidi = BidiInfo::new(&logical_text, self.base_level);
        let para = match bidi.paragraphs.first() {
            Some(p) => p,
            None => return,
        };
        let line_range = para.range.clone();
        let mut visual_indices: Vec<usize> = Vec::with_capacity(self.current_line.items.len());

        let (reordered_levels, runs) = bidi.visual_runs(para, line_range);
        for run in runs {
            let mut run_indices: Vec<usize> = spans
                .iter()
                .filter(|(_, range)| range.start < run.end && range.end > run.start)
                .map(|(idx, _)| *idx)
                .collect();

            let char_index = logical_text[..run.start].chars().count();
            let level = reordered_levels.get(char_index).copied().unwrap_or_else(Level::ltr);
            if level.is_rtl() {
                run_indices.reverse();
            }

            visual_indices.extend(run_indices);
        }

        if visual_indices.len() != self.current_line.items.len() {
            // Fallback to logical order if reordering failed
            return;
        }

        let mut x = 0.0;
        let mut reordered: Vec<PositionedItem> = Vec::with_capacity(self.current_line.items.len());
        for idx in visual_indices {
            let mut item = self.current_line.items[idx].clone();
            item.x = x;
            x += item.item.width();
            reordered.push(item);
        }

        self.current_line.items = reordered;
        self.current_x = x;
    }

    fn push_item_content(&self, item: &InlineItem, buffer: &mut String) {
        match item {
            InlineItem::Text(t) => buffer.push_str(&t.text),
            _ => {
                let dir = item.direction();
                let (lri, pdi) = match dir {
                    Direction::Rtl => ('\u{2067}', '\u{2069}'), // RLI ... PDI
                    _ => ('\u{2066}', '\u{2069}'),              // LRI ... PDI
                };
                buffer.push(lri);
                buffer.push('\u{FFFC}');
                buffer.push(pdi);
            }
        }
    }

    /// Finishes building and returns all lines
    pub fn finish(mut self) -> Vec<Line> {
        // Finish any remaining line
        self.finish_line();
        self.lines
    }

    /// Returns the current line width
    pub fn current_width(&self) -> f32 {
        self.current_x
    }

    /// Returns true if current line is empty
    pub fn is_current_line_empty(&self) -> bool {
        self.current_line.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::geometry::Rect;
    use crate::style::ComputedStyle;
    use crate::text::font_loader::FontContext;
    use crate::text::line_break::find_break_opportunities;
    use crate::text::pipeline::ShapingPipeline;
    use std::sync::Arc;

    fn make_strut_metrics() -> BaselineMetrics {
        BaselineMetrics::new(12.0, 16.0, 12.0, 4.0)
    }

    fn make_builder(width: f32) -> LineBuilder {
        let strut = make_strut_metrics();
        LineBuilder::new(
            width,
            strut,
            ShapingPipeline::new(),
            FontContext::new(),
            Some(Level::ltr()),
        )
    }

    fn make_text_item(text: &str, advance: f32) -> TextItem {
        let style = Arc::new(ComputedStyle::default());
        let mut cluster_advances = Vec::new();
        if !text.is_empty() {
            let step = advance / text.len() as f32;
            for i in 1..=text.len() {
                cluster_advances.push(ClusterBoundary {
                    byte_offset: i,
                    advance: step * i as f32,
                });
            }
        }
        TextItem {
            runs: Vec::new(),
            advance,
            metrics: make_strut_metrics(),
            vertical_align: VerticalAlign::Baseline,
            break_opportunities: find_break_opportunities(text),
            text: text.to_string(),
            font_size: 16.0,
            style: style.clone(),
            cluster_advances,
        }
    }

    #[test]
    fn test_line_builder_single_item_fits() {
        let mut builder = make_builder(100.0);

        let item = make_text_item("Hello", 50.0);
        builder.add_item(InlineItem::Text(item));

        let lines = builder.finish();
        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].items.len(), 1);
        assert!(lines[0].width <= 100.0);
    }

    #[test]
    fn test_line_builder_multiple_items_fit() {
        let mut builder = make_builder(200.0);

        builder.add_item(InlineItem::Text(make_text_item("Hello", 50.0)));
        builder.add_item(InlineItem::Text(make_text_item(" ", 5.0)));
        builder.add_item(InlineItem::Text(make_text_item("World", 50.0)));

        let lines = builder.finish();
        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].items.len(), 3);
    }

    #[test]
    fn test_line_builder_item_exceeds_width() {
        let mut builder = make_builder(80.0);

        builder.add_item(InlineItem::Text(make_text_item("Hello", 50.0)));
        builder.add_item(InlineItem::Text(make_text_item("World", 50.0)));

        let lines = builder.finish();
        // Second item should go to new line
        assert_eq!(lines.len(), 2);
    }

    #[test]
    fn test_line_builder_force_break() {
        let mut builder = make_builder(200.0);

        builder.add_item(InlineItem::Text(make_text_item("Hello", 50.0)));
        builder.force_break();
        builder.add_item(InlineItem::Text(make_text_item("World", 50.0)));

        let lines = builder.finish();
        assert_eq!(lines.len(), 2);
        assert!(lines[0].ends_with_hard_break);
    }

    #[test]
    fn test_line_builder_empty_result() {
        let builder = make_builder(100.0);

        let lines = builder.finish();
        assert!(lines.is_empty());
    }

    #[test]
    fn test_line_has_baseline() {
        let mut builder = make_builder(200.0);

        builder.add_item(InlineItem::Text(make_text_item("Hello", 50.0)));

        let lines = builder.finish();
        assert_eq!(lines.len(), 1);
        assert!(lines[0].baseline > 0.0);
        assert!(lines[0].height > 0.0);
    }

    #[test]
    fn test_replaced_item() {
        let mut builder = make_builder(200.0);

        let replaced = ReplacedItem::new(
            Size::new(100.0, 50.0),
            ReplacedType::Image { src: String::new() },
            Arc::new(ComputedStyle::default()),
        );
        builder.add_item(InlineItem::Replaced(replaced));

        let lines = builder.finish();
        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].items[0].item.width(), 100.0);
    }

    #[test]
    fn test_inline_block_item() {
        let mut builder = make_builder(200.0);

        let fragment = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 80.0, 40.0), vec![]);
        let inline_block = InlineBlockItem::new(fragment, Direction::Ltr, UnicodeBidi::Normal);
        builder.add_item(InlineItem::InlineBlock(inline_block));

        let lines = builder.finish();
        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].items[0].item.width(), 80.0);
    }

    #[test]
    fn test_overflow_on_empty_line() {
        let mut builder = make_builder(30.0);

        // Item too wide but line is empty, so it must fit
        builder.add_item(InlineItem::Text(make_text_item("VeryLongWord", 100.0)));

        let lines = builder.finish();
        assert_eq!(lines.len(), 1);
        assert!(lines[0].width > 30.0); // Overflow allowed
    }

    #[test]
    fn test_positioned_item_x_position() {
        let mut builder = make_builder(200.0);

        builder.add_item(InlineItem::Text(make_text_item("Hello", 50.0)));
        builder.add_item(InlineItem::Text(make_text_item(" ", 5.0)));
        builder.add_item(InlineItem::Text(make_text_item("World", 50.0)));

        let lines = builder.finish();
        assert_eq!(lines[0].items[0].x, 0.0);
        assert_eq!(lines[0].items[1].x, 50.0);
        assert_eq!(lines[0].items[2].x, 55.0);
    }

    #[test]
    fn test_text_item_break_opportunities() {
        let item = make_text_item("Hello World Test", 160.0);

        // Should have break opportunities after spaces
        assert!(!item.break_opportunities.is_empty());
    }

    #[test]
    fn test_vertical_align_default() {
        let item = make_text_item("Test", 40.0);
        assert_eq!(item.vertical_align, VerticalAlign::Baseline);
    }

    #[test]
    fn test_line_default() {
        let line = Line::default();
        assert!(line.is_empty());
        assert_eq!(line.width, 0.0);
    }
}
