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

use super::baseline::BaselineMetrics;
use super::baseline::LineBaselineAccumulator;
use super::baseline::VerticalAlign;
use crate::debug::runtime;
use crate::geometry::Size;
use crate::layout::inline::float_integration::InlineFloatIntegration;
use crate::layout::inline::float_integration::LineSpaceOptions;
use crate::style::display::Display;
use crate::style::types::Direction;
use crate::style::types::ListStylePosition;
use crate::style::types::OverflowWrap;
use crate::style::types::TextWrap;
use crate::style::types::UnicodeBidi;
use crate::style::types::WhiteSpace;
use crate::style::types::WordBreak;
use crate::style::ComputedStyle;
use crate::text::font_loader::FontContext;
use crate::text::justify::InlineAxis;
use crate::text::line_break::BreakOpportunity;
use crate::text::line_break::BreakType;
use crate::text::pipeline::shaping_style_hash;
use crate::text::pipeline::ExplicitBidiContext;
use crate::text::pipeline::ShapedRun;
use crate::text::pipeline::ShapingPipeline;
use crate::tree::box_tree::ReplacedType;
use crate::tree::fragment_tree::FragmentContent;
use crate::tree::fragment_tree::FragmentNode;
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::hash::Hash;
use std::hash::Hasher;
use std::ops::Range;
use std::sync::Arc;
use unicode_bidi::BidiInfo;
use unicode_bidi::Level;

fn pipeline_dir_from_style(dir: Direction) -> crate::text::pipeline::Direction {
  match dir {
    Direction::Ltr => crate::text::pipeline::Direction::LeftToRight,
    Direction::Rtl => crate::text::pipeline::Direction::RightToLeft,
  }
}

/// An item in the inline formatting context
///
/// Represents different types of content that can appear inline.
#[derive(Debug, Clone)]
pub enum InlineItem {
  /// Shaped text ready for layout
  Text(TextItem),

  /// A tab character that expands to the next tab stop
  Tab(TabItem),

  /// An inline box (span, a, em, etc.) with children
  InlineBox(InlineBoxItem),

  /// An inline-block box (atomic inline)
  InlineBlock(InlineBlockItem),

  /// A ruby annotation container (ruby, ruby-base, etc.)
  Ruby(RubyItem),

  /// A replaced element (img, canvas, etc.)
  Replaced(ReplacedItem),

  /// A floating box encountered in the inline stream
  Floating(FloatingItem),

  /// Zero-sized anchor for absolute/fixed positioned children
  StaticPositionAnchor(StaticPositionAnchor),
}

impl InlineItem {
  /// Returns the width of this item
  pub fn width(&self) -> f32 {
    match self {
      InlineItem::Text(t) => t.advance_for_layout,
      InlineItem::Tab(t) => t.width(),
      InlineItem::InlineBox(b) => b.width(),
      InlineItem::InlineBlock(b) => b.total_width(),
      InlineItem::Ruby(r) => r.width(),
      InlineItem::Replaced(r) => r.total_width(),
      InlineItem::Floating(_) => 0.0,
      InlineItem::StaticPositionAnchor(_) => 0.0,
    }
  }

  /// Returns the intrinsic width excluding margins (border/padding included)
  pub fn intrinsic_width(&self) -> f32 {
    match self {
      InlineItem::Text(t) => t.advance_for_layout,
      InlineItem::Tab(t) => t.width(),
      InlineItem::InlineBox(b) => b.width(),
      InlineItem::InlineBlock(b) => b.width,
      InlineItem::Ruby(r) => r.intrinsic_width(),
      InlineItem::Replaced(r) => r.intrinsic_width(),
      InlineItem::Floating(_) => 0.0,
      InlineItem::StaticPositionAnchor(_) => 0.0,
    }
  }

  /// Returns baseline metrics for this item
  pub fn baseline_metrics(&self) -> BaselineMetrics {
    match self {
      InlineItem::Text(t) => t.metrics,
      InlineItem::Tab(t) => t.metrics,
      InlineItem::InlineBox(b) => b.metrics,
      InlineItem::InlineBlock(b) => b.metrics,
      InlineItem::Ruby(r) => r.metrics,
      InlineItem::Replaced(r) => r.metrics,
      InlineItem::Floating(f) => f.metrics,
      InlineItem::StaticPositionAnchor(_) => StaticPositionAnchor::metrics(),
    }
  }

  /// Returns the vertical alignment for this item
  pub fn vertical_align(&self) -> VerticalAlign {
    match self {
      InlineItem::Text(t) => t.vertical_align,
      InlineItem::Tab(t) => t.vertical_align,
      InlineItem::InlineBox(b) => b.vertical_align,
      InlineItem::InlineBlock(b) => b.vertical_align,
      InlineItem::Ruby(r) => r.vertical_align,
      InlineItem::Replaced(r) => r.vertical_align,
      InlineItem::Floating(f) => f.vertical_align,
      InlineItem::StaticPositionAnchor(_) => VerticalAlign::Baseline,
    }
  }

  /// Returns true if this item can be broken (for text)
  pub fn is_breakable(&self) -> bool {
    matches!(self, InlineItem::Text(_) | InlineItem::InlineBox(_))
  }

  pub fn direction(&self) -> Direction {
    match self {
      InlineItem::Text(t) => t.style.direction,
      InlineItem::Tab(t) => t.direction,
      InlineItem::InlineBox(b) => b.direction,
      InlineItem::InlineBlock(b) => b.direction,
      InlineItem::Ruby(r) => r.direction,
      InlineItem::Replaced(r) => r.direction,
      InlineItem::Floating(f) => f.direction,
      InlineItem::StaticPositionAnchor(a) => a.direction,
    }
  }

  pub fn unicode_bidi(&self) -> UnicodeBidi {
    match self {
      InlineItem::Text(t) => t.style.unicode_bidi,
      InlineItem::Tab(t) => t.unicode_bidi,
      InlineItem::InlineBox(b) => b.unicode_bidi,
      InlineItem::InlineBlock(b) => b.unicode_bidi,
      InlineItem::Ruby(r) => r.unicode_bidi,
      InlineItem::Replaced(r) => r.unicode_bidi,
      InlineItem::Floating(f) => f.unicode_bidi,
      InlineItem::StaticPositionAnchor(a) => a.unicode_bidi,
    }
  }

  /// Resolves the width of this item when placed at `start_x`.
  ///
  /// Tabs depend on their starting position to find the next tab stop; other items are fixed width.
  pub fn resolve_width_at(mut self, start_x: f32) -> (Self, f32) {
    match &mut self {
      InlineItem::Tab(tab) => {
        let width = tab.resolve_width(start_x);
        (self, width)
      }
      _ => {
        let width = self.width();
        (self, width)
      }
    }
  }
}

/// Inline placeholder that marks where a positioned element would appear in flow.
#[derive(Debug, Clone)]
pub struct StaticPositionAnchor {
  pub box_id: usize,
  pub direction: Direction,
  pub unicode_bidi: UnicodeBidi,
  pub running: Option<RunningInfo>,
}

impl StaticPositionAnchor {
  pub fn new(box_id: usize, direction: Direction, unicode_bidi: UnicodeBidi) -> Self {
    Self {
      box_id,
      direction,
      unicode_bidi,
      running: None,
    }
  }

  pub fn metrics() -> BaselineMetrics {
    BaselineMetrics::new(0.0, 0.0, 0.0, 0.0)
  }

  pub fn with_running(mut self, running: RunningInfo) -> Self {
    self.running = Some(running);
    self
  }
}

#[derive(Debug, Clone)]
pub struct RunningInfo {
  pub name: String,
  pub snapshot: FragmentNode,
  pub style: Arc<ComputedStyle>,
}

fn allows_soft_wrap(style: &ComputedStyle) -> bool {
  !matches!(style.white_space, WhiteSpace::Nowrap | WhiteSpace::Pre)
    && !matches!(style.text_wrap, TextWrap::NoWrap)
}

pub(crate) fn log_line_width_enabled() -> bool {
  runtime::runtime_toggles().truthy("FASTR_LOG_LINE_WIDTH")
}

/// A shaped text item
#[derive(Debug, Clone)]
pub struct TextItem {
  /// Identifier for the source text box node (0 when unknown/anonymous).
  pub box_id: usize,

  /// The shaped runs (bidi/script/font-aware)
  pub runs: Vec<ShapedRun>,

  /// Total horizontal advance
  pub advance: f32,
  /// Horizontal advance used for layout (may differ for markers)
  pub advance_for_layout: f32,

  /// Baseline metrics
  pub metrics: BaselineMetrics,

  /// Vertical alignment
  pub vertical_align: VerticalAlign,

  /// Break opportunities within this text
  pub break_opportunities: Vec<BreakOpportunity>,
  /// Offsets of forced breaks inserted during normalization (e.g., newlines)
  pub forced_break_offsets: Vec<usize>,
  /// Earliest mandatory break in `break_opportunities` (if any).
  first_mandatory_break: Option<BreakOpportunity>,

  /// Original text for fragment creation
  pub text: String,

  /// Font size used
  pub font_size: f32,

  /// Computed style for this text run
  pub style: Arc<ComputedStyle>,
  /// Base paragraph direction used for shaping
  pub base_direction: Direction,
  /// Explicit bidi context used during shaping (embedding level + override flag).
  pub explicit_bidi: Option<ExplicitBidiContext>,
  /// Whether this text item is a list marker
  pub is_marker: bool,
  /// Additional paint offset applied at fragment creation (used for outside markers)
  pub paint_offset: f32,
  /// Cumulative advances at cluster boundaries (text order)
  cluster_advances: Vec<ClusterBoundary>,
  /// Range of the original source text represented by this item.
  source_range: Range<usize>,
  /// Stable hash of the source text used for ephemeral caches.
  source_id: u64,
}

/// A floating box placeholder encountered in the inline stream
#[derive(Debug, Clone)]
pub struct FloatingItem {
  pub box_node: crate::tree::box_tree::BoxNode,
  pub metrics: BaselineMetrics,
  pub vertical_align: VerticalAlign,
  pub direction: Direction,
  pub unicode_bidi: UnicodeBidi,
}

#[derive(Debug, Clone)]
struct ClusterBoundary {
  /// Byte offset in the source text where this cluster starts
  byte_offset: usize,
  /// Advance width from the start of the item up to and including this cluster
  advance: f32,
  /// Index of the shaped run that owns this cluster boundary.
  run_index: Option<usize>,
  /// Glyph index at which the boundary occurs (exclusive).
  glyph_end: Option<usize>,
  /// Advance within the run up to this boundary.
  run_advance: f32,
}

#[derive(Debug, Default)]
pub(crate) struct ReshapeCache {
  runs: HashMap<ReshapeCacheKey, Arc<Vec<ShapedRun>>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ReshapeCacheKey {
  style_hash: u64,
  font_generation: u64,
  text_id: u64,
  range_start: usize,
  range_end: usize,
  base_direction_rtl: bool,
  explicit_bidi: Option<(u8, bool)>,
  letter_spacing_bits: u32,
  word_spacing_bits: u32,
}

impl TextItem {
  /// Creates a new text item
  pub fn new(
    runs: Vec<ShapedRun>,
    text: String,
    metrics: BaselineMetrics,
    break_opportunities: Vec<BreakOpportunity>,
    forced_break_offsets: Vec<usize>,
    style: Arc<ComputedStyle>,
    base_direction: Direction,
  ) -> Self {
    let cluster_advances = Self::compute_cluster_advances(&runs, text.as_str(), style.font_size);
    let aligned_breaks =
      Self::align_breaks_to_clusters(break_opportunities, &cluster_advances, text.len());
    let first_mandatory_break = Self::first_mandatory_break(&aligned_breaks);
    let advance: f32 = cluster_advances
      .last()
      .map(|c| c.advance)
      .unwrap_or_else(|| runs.iter().map(|r| r.advance).sum());
    let font_size = style.font_size;
    let source_id = Self::hash_text(&text);
    let source_range = 0..text.len();
    Self {
      box_id: 0,
      runs,
      advance,
      advance_for_layout: advance,
      metrics,
      vertical_align: VerticalAlign::Baseline,
      break_opportunities: aligned_breaks,
      forced_break_offsets,
      first_mandatory_break,
      text,
      font_size,
      style,
      base_direction,
      explicit_bidi: None,
      is_marker: false,
      paint_offset: 0.0,
      cluster_advances,
      source_range,
      source_id,
    }
  }

  /// Byte offsets for each grapheme cluster boundary within the item.
  pub fn cluster_byte_offsets(&self) -> Vec<usize> {
    if self.text.is_empty() {
      return Vec::new();
    }

    let mut offsets: Vec<usize> = self
      .runs
      .iter()
      .flat_map(|run| {
        let mut run_offsets = Vec::new();

        if run.glyphs.is_empty() {
          run_offsets.push(Self::previous_char_boundary_in_text(&self.text, run.start));
          run_offsets.push(Self::previous_char_boundary_in_text(&self.text, run.end));
          return run_offsets;
        }

        for glyph in &run.glyphs {
          let raw_offset = run.start.saturating_add(glyph.cluster as usize);
          run_offsets.push(Self::previous_char_boundary_in_text(&self.text, raw_offset));
        }
        run_offsets.push(Self::previous_char_boundary_in_text(&self.text, run.end));
        run_offsets
      })
      .collect();

    if offsets.is_empty() {
      offsets = crate::text::segmentation::segment_grapheme_clusters(&self.text);
    } else {
      offsets.push(0);
      offsets.push(self.text.len());
      offsets.sort_unstable();
      offsets.dedup();
    }

    offsets
  }

  /// Add allowed break opportunities at every cluster boundary.
  pub fn add_breaks_at_clusters(&mut self) {
    if self.text.is_empty() {
      return;
    }
    let additional: Vec<BreakOpportunity> = self
      .cluster_byte_offsets()
      .into_iter()
      .filter(|offset| *offset > 0 && *offset < self.text.len())
      .map(|offset| BreakOpportunity::new(offset, BreakType::Allowed))
      .collect();
    self.break_opportunities.extend(additional);
    self.break_opportunities.sort_by_key(|b| b.byte_offset);
    self.break_opportunities.dedup_by(|a, b| {
      if a.byte_offset != b.byte_offset {
        return false;
      }
      if let BreakType::Mandatory = a.break_type {
        *b = *a;
      }
      true
    });
    self.first_mandatory_break = Self::first_mandatory_break(&self.break_opportunities);
  }

  pub fn recompute_cluster_advances(&mut self) {
    self.cluster_advances = Self::compute_cluster_advances(&self.runs, &self.text, self.font_size);
  }

  /// Derive baseline metrics from shaped runs and CSS line-height
  pub fn metrics_from_runs(
    runs: &[ShapedRun],
    line_height: f32,
    fallback_font_size: f32,
  ) -> BaselineMetrics {
    let mut ascent: f32 = 0.0;
    let mut descent: f32 = 0.0;
    let mut line_gap: f32 = 0.0;
    let mut x_height: Option<f32> = None;

    for run in runs {
      if let Ok(metrics) = run.font.metrics() {
        let scaled = metrics.scale(run.font_size);
        ascent = ascent.max(scaled.ascent);
        descent = descent.max(scaled.descent);
        line_gap = line_gap.max(scaled.line_gap);
        if x_height.is_none() {
          x_height = scaled.x_height;
        }
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
      x_height,
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
  pub(crate) fn split_at(
    &self,
    byte_offset: usize,
    insert_hyphen: bool,
    shaper: &ShapingPipeline,
    font_context: &FontContext,
    reshape_cache: &mut ReshapeCache,
  ) -> Option<(TextItem, TextItem)> {
    const INSERTED_HYPHEN: char = '\u{2010}'; // CSS hyphenation hyphen
    let text_len = self.text.len();
    if byte_offset == 0 || byte_offset >= text_len {
      return None;
    }

    let target_offset = if self.text.is_char_boundary(byte_offset) {
      byte_offset
    } else {
      Self::previous_char_boundary_in_text(&self.text, byte_offset)
    };

    if target_offset == 0 || target_offset >= text_len {
      return None;
    }

    let split_offset = target_offset;

    if split_offset == 0 || split_offset >= text_len || !self.text.is_char_boundary(split_offset) {
      return None;
    }

    // Split the text
    let before_text = self.text.get(..split_offset)?;
    let after_text = self.text.get(split_offset..)?;

    let (mut before_runs, after_runs) =
      self
        .split_runs_preserving_shaping(split_offset)
        .or_else(|| {
          let before_runs = reshape_cache.shape(self, 0..split_offset, shaper, font_context)?;
          let after_runs =
            reshape_cache.shape(self, split_offset..text_len, shaper, font_context)?;
          Some((before_runs, after_runs))
        })?;

    let before_text_owned: Option<String> = if insert_hyphen {
      let mut hyphen_buf = [0u8; 3];
      let hyphen_text = INSERTED_HYPHEN.encode_utf8(&mut hyphen_buf);
      let offset = before_text.len();
      let mut hyphen_runs = shaper
        .shape_with_context(
          hyphen_text,
          &self.style,
          font_context,
          pipeline_dir_from_style(self.base_direction),
          self.explicit_bidi,
        )
        .ok()?;
      TextItem::apply_spacing_to_runs(
        &mut hyphen_runs,
        hyphen_text,
        self.style.letter_spacing,
        self.style.word_spacing,
      );

      for run in &mut hyphen_runs {
        run.start += offset;
        run.end += offset;
        for glyph in &mut run.glyphs {
          glyph.cluster = glyph.cluster.saturating_add(offset as u32);
        }
      }

      before_runs.extend(hyphen_runs);
      let mut owned = before_text.to_string();
      owned.push_str(hyphen_text);
      Some(owned)
    } else {
      None
    };

    let line_height = self.metrics.line_height;
    let before_metrics = TextItem::metrics_from_runs(&before_runs, line_height, self.font_size);
    let after_metrics = TextItem::metrics_from_runs(&after_runs, line_height, self.font_size);

    let mut before_item = TextItem::new(
      before_runs,
      before_text_owned
        .clone()
        .unwrap_or_else(|| before_text.to_string()),
      before_metrics,
      self
        .break_opportunities
        .iter()
        .filter(|b| b.byte_offset <= split_offset)
        .copied()
        .collect(),
      self
        .forced_break_offsets
        .iter()
        .copied()
        .filter(|o| *o <= split_offset)
        .collect(),
      self.style.clone(),
      self.base_direction,
    )
    .with_vertical_align(self.vertical_align);
    before_item.box_id = self.box_id;
    before_item.explicit_bidi = self.explicit_bidi;
    before_item.source_id = self.source_id;
    before_item.source_range = self.source_range.start..self.source_range.start + split_offset;

    let mut after_item = TextItem::new(
      after_runs,
      after_text.to_string(),
      after_metrics,
      self
        .break_opportunities
        .iter()
        .filter(|b| b.byte_offset > split_offset)
        .map(|b| {
          BreakOpportunity::with_hyphen(b.byte_offset - split_offset, b.break_type, b.adds_hyphen)
        })
        .collect(),
      self
        .forced_break_offsets
        .iter()
        .copied()
        .filter(|o| *o > split_offset)
        .map(|o| o - split_offset)
        .collect(),
      self.style.clone(),
      self.base_direction,
    )
    .with_vertical_align(self.vertical_align);
    after_item.box_id = self.box_id;
    after_item.explicit_bidi = self.explicit_bidi;
    after_item.source_id = self.source_id;
    after_item.source_range = self.source_range.start + split_offset..self.source_range.end;

    if before_item.advance <= 0.0 || after_item.advance <= 0.0 {
      let before_runs = reshape_cache.shape(self, 0..split_offset, shaper, font_context)?;
      let after_runs = reshape_cache.shape(self, split_offset..text_len, shaper, font_context)?;
      let before_metrics = TextItem::metrics_from_runs(&before_runs, line_height, self.font_size);
      let after_metrics = TextItem::metrics_from_runs(&after_runs, line_height, self.font_size);
      before_item = TextItem::new(
        before_runs,
        before_text_owned.unwrap_or_else(|| before_text.to_string()),
        before_metrics,
        self
          .break_opportunities
          .iter()
          .filter(|b| b.byte_offset <= split_offset)
          .copied()
          .collect(),
        self
          .forced_break_offsets
          .iter()
          .copied()
          .filter(|o| *o <= split_offset)
          .collect(),
        self.style.clone(),
        self.base_direction,
      )
      .with_vertical_align(self.vertical_align);
      before_item.box_id = self.box_id;
      before_item.explicit_bidi = self.explicit_bidi;
      before_item.source_id = self.source_id;
      before_item.source_range = self.source_range.start..self.source_range.start + split_offset;

      after_item = TextItem::new(
        after_runs,
        after_text.to_string(),
        after_metrics,
        self
          .break_opportunities
          .iter()
          .filter(|b| b.byte_offset > split_offset)
          .map(|b| {
            BreakOpportunity::with_hyphen(b.byte_offset - split_offset, b.break_type, b.adds_hyphen)
          })
          .collect(),
        self
          .forced_break_offsets
          .iter()
          .copied()
          .filter(|o| *o > split_offset)
          .map(|o| o - split_offset)
          .collect(),
        self.style.clone(),
        self.base_direction,
      )
      .with_vertical_align(self.vertical_align);
      after_item.box_id = self.box_id;
      after_item.explicit_bidi = self.explicit_bidi;
      after_item.source_id = self.source_id;
      after_item.source_range = self.source_range.start + split_offset..self.source_range.end;
    }

    if self.is_marker {
      before_item.is_marker = true;
      after_item.is_marker = true;
      before_item.paint_offset = self.paint_offset;
      after_item.paint_offset = self.paint_offset;
      before_item.advance_for_layout = self.advance_for_layout.min(before_item.advance_for_layout);
      after_item.advance_for_layout =
        (self.advance_for_layout - before_item.advance_for_layout).max(0.0);
    }

    Some((before_item, after_item))
  }

  /// Applies letter- and word-spacing to shaped runs.
  ///
  /// Spacing is added after each cluster (except the final cluster).
  /// Word spacing stacks on top of letter spacing for space-like clusters.
  pub(crate) fn apply_spacing_to_runs(
    runs: &mut [ShapedRun],
    text: &str,
    letter_spacing: f32,
    word_spacing: f32,
  ) {
    if runs.is_empty() || text.is_empty() {
      return;
    }
    if letter_spacing == 0.0 && word_spacing == 0.0 {
      return;
    }

    #[derive(Debug)]
    struct ClusterRef {
      run_idx: usize,
      glyph_end: usize,
      offset: usize,
      is_space: bool,
    }

    let mut clusters: Vec<ClusterRef> = Vec::new();

    for (run_idx, run) in runs.iter().enumerate() {
      if run.glyphs.is_empty() {
        continue;
      }

      let mut idx = 0;
      while idx < run.glyphs.len() {
        let cluster_value = run.glyphs[idx].cluster;
        while idx < run.glyphs.len() && run.glyphs[idx].cluster == cluster_value {
          idx += 1;
        }
        let glyph_end = idx;

        let offset = run.start.saturating_add(cluster_value as usize);
        let ch = text.get(offset..).and_then(|s| s.chars().next());
        let is_space = matches!(ch, Some(' ') | Some('\u{00A0}') | Some('\t'));

        clusters.push(ClusterRef {
          run_idx,
          glyph_end,
          offset,
          is_space,
        });
      }
    }

    clusters.sort_by_key(|c| c.offset);

    if clusters.is_empty() {
      return;
    }

    let mut run_cluster_extras: Vec<Vec<(usize, f32)>> = vec![Vec::new(); runs.len()];

    for (i, cluster) in clusters.iter().enumerate() {
      if i == clusters.len() - 1 {
        break; // No spacing after the last cluster
      }

      let mut extra = letter_spacing;
      if cluster.is_space {
        extra += word_spacing;
      }

      if extra == 0.0 {
        continue;
      }

      let last_glyph_idx = cluster.glyph_end.saturating_sub(1);
      run_cluster_extras[cluster.run_idx].push((last_glyph_idx, extra));
    }

    for (run_idx, run) in runs.iter_mut().enumerate() {
      if run.glyphs.is_empty() {
        continue;
      }

      let axis = run_inline_axis(run);
      let mut extra_by_glyph = vec![0.0; run.glyphs.len()];
      for (glyph_idx, extra) in run_cluster_extras
        .get(run_idx)
        .map(|v| v.as_slice())
        .unwrap_or(&[])
      {
        if *glyph_idx < extra_by_glyph.len() {
          extra_by_glyph[*glyph_idx] += *extra;
        }
      }

      let mut cumulative_shift = 0.0;
      let mut new_advance = 0.0;

      for (idx, glyph) in run.glyphs.iter_mut().enumerate() {
        let extra = extra_by_glyph[idx];
        glyph.x_offset += cumulative_shift;
        if extra != 0.0 {
          add_inline_advance(glyph, axis, extra);
        }
        cumulative_shift += extra;
        new_advance += glyph_inline_advance(glyph, axis);
      }

      run.advance = new_advance;
    }
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
  pub fn find_break_point(&self, max_width: f32) -> Option<BreakOpportunity> {
    if let Some(mandatory) = self.first_mandatory_break {
      if self.advance_at_offset(mandatory.byte_offset) <= max_width {
        return Some(mandatory);
      }
    }

    let mut best_break = self
      .offset_for_width(max_width)
      .and_then(|offset| self.allowed_break_at_or_before(offset));
    if best_break.is_some() || !allows_soft_wrap(self.style.as_ref()) {
      return best_break;
    }

    let can_overflow_break_word = matches!(
      self.style.word_break,
      WordBreak::BreakWord | WordBreak::Anywhere
    );
    let can_overflow_break_by_wrap = self.style.overflow_wrap == OverflowWrap::BreakWord;

    if (can_overflow_break_word || can_overflow_break_by_wrap) && best_break.is_none() {
      if let Some(offset) = self.offset_for_width(max_width) {
        if offset > 0 {
          best_break = Some(BreakOpportunity::allowed(offset));
        }
      }
    }

    best_break
  }

  fn offset_for_width(&self, max_width: f32) -> Option<usize> {
    if self.cluster_advances.is_empty() {
      return None;
    }

    let idx = self
      .cluster_advances
      .partition_point(|b| b.advance <= max_width);
    if idx == 0 {
      return None;
    }

    self.cluster_advances.get(idx - 1).map(|b| b.byte_offset)
  }

  fn allowed_break_at_or_before(&self, byte_offset: usize) -> Option<BreakOpportunity> {
    if self.break_opportunities.is_empty() {
      return None;
    }

    let mut idx = self
      .break_opportunities
      .partition_point(|b| b.byte_offset <= byte_offset);
    while idx > 0 {
      idx -= 1;
      let brk = self.break_opportunities[idx];
      if matches!(brk.break_type, BreakType::Allowed) {
        return Some(brk);
      }
    }

    None
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

  fn cluster_boundary_exact(&self, byte_offset: usize) -> Option<&ClusterBoundary> {
    if self.cluster_advances.is_empty() {
      return None;
    }

    let mut idx = self
      .cluster_advances
      .binary_search_by_key(&byte_offset, |c| c.byte_offset)
      .ok()?;
    while idx + 1 < self.cluster_advances.len()
      && self.cluster_advances[idx + 1].byte_offset == byte_offset
    {
      idx += 1;
    }

    self.cluster_advances.get(idx)
  }

  fn compute_cluster_advances(
    runs: &[ShapedRun],
    text: &str,
    fallback_font_size: f32,
  ) -> Vec<ClusterBoundary> {
    let text_len = text.len();
    if text_len == 0 {
      return Vec::new();
    }
    if runs.is_empty() {
      let estimated = (text.chars().count() as f32) * fallback_font_size * 0.5;
      return vec![ClusterBoundary {
        byte_offset: text_len,
        advance: estimated,
        run_index: None,
        glyph_end: None,
        run_advance: estimated,
      }];
    }

    let mut run_indices: Vec<usize> = (0..runs.len()).collect();
    run_indices.sort_by_key(|idx| runs[*idx].start);

    let mut advances = Vec::new();
    let mut cumulative = 0.0;

    for run_idx in run_indices {
      let run = &runs[run_idx];
      let axis = run_inline_axis(run);
      if run.glyphs.is_empty() {
        if run.advance > 0.0 {
          let run_advance = run.advance.max(0.0);
          cumulative += run_advance;
          advances.push(ClusterBoundary {
            byte_offset: Self::previous_char_boundary_in_text(text, run.end).min(text_len),
            advance: cumulative,
            run_index: Some(run_idx),
            glyph_end: None,
            run_advance,
          });
        }
        continue;
      }

      let mut glyph_idx = 0;
      let mut run_advance = 0.0;
      while glyph_idx < run.glyphs.len() {
        let cluster_value = run.glyphs[glyph_idx].cluster as usize;
        let mut cluster_width = 0.0;
        while glyph_idx < run.glyphs.len()
          && run.glyphs[glyph_idx].cluster as usize == cluster_value
        {
          cluster_width += glyph_inline_advance(&run.glyphs[glyph_idx], axis);
          glyph_idx += 1;
        }

        run_advance += cluster_width;
        cumulative += cluster_width;
        let offset =
          Self::previous_char_boundary_in_text(text, run.start + cluster_value).min(text_len);
        advances.push(ClusterBoundary {
          byte_offset: offset,
          advance: cumulative,
          run_index: Some(run_idx),
          glyph_end: Some(glyph_idx),
          run_advance,
        });
      }

      let run_end = Self::previous_char_boundary_in_text(text, run.end).min(text_len);
      if advances
        .last()
        .map(|b| b.byte_offset != run_end)
        .unwrap_or(true)
      {
        advances.push(ClusterBoundary {
          byte_offset: run_end,
          advance: cumulative,
          run_index: Some(run_idx),
          glyph_end: Some(run.glyphs.len()),
          run_advance,
        });
      }
    }

    // Deduplicate by byte offset, keeping the greatest advance so that cumulative width remains monotonic
    let mut deduped: Vec<ClusterBoundary> = Vec::new();
    for boundary in advances {
      if let Some(last) = deduped.last_mut() {
        if last.byte_offset == boundary.byte_offset {
          if boundary.advance > last.advance
            || last.run_index.is_none()
            || (last.glyph_end.is_none() && boundary.glyph_end.is_some())
          {
            *last = boundary;
          }
          continue;
        }
      }
      deduped.push(boundary);
    }

    if deduped
      .last()
      .map(|b| b.byte_offset < text_len)
      .unwrap_or(false)
    {
      deduped.push(ClusterBoundary {
        byte_offset: text_len,
        advance: deduped.last().map(|b| b.advance).unwrap_or(0.0),
        run_index: None,
        glyph_end: None,
        run_advance: deduped
          .last()
          .map(|b| b.run_advance)
          .unwrap_or(fallback_font_size),
      });
    }

    if deduped.is_empty() {
      deduped.push(ClusterBoundary {
        byte_offset: text_len,
        advance: fallback_font_size,
        run_index: None,
        glyph_end: None,
        run_advance: fallback_font_size,
      });
    }

    deduped
  }

  fn previous_char_boundary_in_text(text: &str, offset: usize) -> usize {
    if offset >= text.len() {
      return text.len();
    }
    if text.is_char_boundary(offset) {
      return offset;
    }

    text
      .char_indices()
      .take_while(|(idx, _)| *idx < offset)
      .map(|(idx, _)| idx)
      .last()
      .unwrap_or(0)
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
            last.adds_hyphen |= brk.adds_hyphen;
            continue;
          }
        }

        aligned.push(BreakOpportunity::with_hyphen(
          offset,
          brk.break_type,
          brk.adds_hyphen,
        ));
      }
    }

    aligned
  }

  fn first_mandatory_break(breaks: &[BreakOpportunity]) -> Option<BreakOpportunity> {
    breaks
      .iter()
      .find(|b| matches!(b.break_type, BreakType::Mandatory))
      .copied()
  }

  fn hash_text(text: &str) -> u64 {
    let mut hasher = DefaultHasher::new();
    text.hash(&mut hasher);
    hasher.finish()
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
  fn split_runs_preserving_shaping(
    &self,
    split_offset: usize,
  ) -> Option<(Vec<ShapedRun>, Vec<ShapedRun>)> {
    let boundary = self.cluster_boundary_exact(split_offset)?;
    let run_idx = boundary.run_index?;
    let glyph_split = boundary.glyph_end?;
    let run = self.runs.get(run_idx)?;

    if split_offset < run.start || split_offset > run.end {
      return None;
    }

    let local = split_offset.checked_sub(run.start)?;
    if local > run.text.len() || !run.text.is_char_boundary(local) {
      return None;
    }

    let left_text = run.text.get(..local)?;
    let right_text = run.text.get(local..)?;

    let mut before_runs: Vec<ShapedRun> = self.runs.iter().take(run_idx).cloned().collect();
    let mut after_runs: Vec<ShapedRun> = self
      .runs
      .iter()
      .skip(run_idx + 1)
      .cloned()
      .map(|mut r| {
        r.start = r.start.saturating_sub(split_offset);
        r.end = r.end.saturating_sub(split_offset);
        r
      })
      .collect();

    let left_glyphs = run.glyphs[..glyph_split.min(run.glyphs.len())].to_vec();
    let mut right_glyphs = run.glyphs[glyph_split.min(run.glyphs.len())..].to_vec();

    let run_axis = run_inline_axis(run);
    let left_advance = boundary.run_advance;

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
      right_run.start = 0;
      right_run.end = right_run.text.len();
      right_run.glyphs = right_glyphs;
      right_run.advance = right_run
        .glyphs
        .iter()
        .map(|g| glyph_inline_advance(g, run_axis))
        .sum();
      after_runs.insert(0, right_run);
    }

    Some((before_runs, after_runs))
  }
}

impl ReshapeCache {
  pub(crate) fn clear(&mut self) {
    self.runs.clear();
  }

  fn shape(
    &mut self,
    item: &TextItem,
    range: Range<usize>,
    shaper: &ShapingPipeline,
    font_context: &FontContext,
  ) -> Option<Vec<ShapedRun>> {
    let text_slice = item.text.get(range.clone())?;
    let key = ReshapeCacheKey {
      style_hash: shaping_style_hash(item.style.as_ref()),
      font_generation: font_context.font_generation(),
      text_id: item.source_id,
      range_start: item.source_range.start + range.start,
      range_end: item.source_range.start + range.end,
      base_direction_rtl: matches!(item.base_direction, Direction::Rtl),
      explicit_bidi: item
        .explicit_bidi
        .map(|ctx| (ctx.level.number(), ctx.override_all)),
      letter_spacing_bits: item.style.letter_spacing.to_bits(),
      word_spacing_bits: item.style.word_spacing.to_bits(),
    };

    if let Some(cached) = self.runs.get(&key) {
      return Some((**cached).clone());
    }

    let mut runs = shaper
      .shape_with_context(
        text_slice,
        &item.style,
        font_context,
        pipeline_dir_from_style(item.base_direction),
        item.explicit_bidi,
      )
      .ok()?;
    TextItem::apply_spacing_to_runs(
      &mut runs,
      text_slice,
      item.style.letter_spacing,
      item.style.word_spacing,
    );
    self.runs.insert(key, Arc::new(runs.clone()));
    Some(runs)
  }
}

fn run_inline_axis(run: &ShapedRun) -> InlineAxis {
  if run.glyphs.iter().any(|g| g.y_advance.abs() > f32::EPSILON) {
    InlineAxis::Vertical
  } else {
    InlineAxis::Horizontal
  }
}

fn glyph_inline_advance(glyph: &crate::text::pipeline::GlyphPosition, axis: InlineAxis) -> f32 {
  match axis {
    InlineAxis::Horizontal => glyph.x_advance,
    InlineAxis::Vertical => {
      if glyph.y_advance.abs() > f32::EPSILON {
        glyph.y_advance
      } else {
        glyph.x_advance
      }
    }
  }
}

fn add_inline_advance(
  glyph: &mut crate::text::pipeline::GlyphPosition,
  axis: InlineAxis,
  delta: f32,
) {
  match axis {
    InlineAxis::Horizontal => glyph.x_advance += delta,
    InlineAxis::Vertical => glyph.y_advance += delta,
  }
}

/// An inline box item (non-atomic, contains children)
#[derive(Debug, Clone)]
pub struct InlineBoxItem {
  /// Identifier for the source box node (0 when unknown/anonymous)
  pub box_id: usize,

  /// Child items within this inline box
  pub children: Vec<InlineItem>,

  /// Opening edge width (left border + padding)
  pub start_edge: f32,

  /// Closing edge width (right border + padding)
  pub end_edge: f32,

  /// Vertical offset applied to children (padding + borders on top)
  pub content_offset_y: f32,

  /// Border widths used to derive the padding box for positioned descendants.
  pub border_left: f32,
  pub border_right: f32,
  pub border_top: f32,
  pub border_bottom: f32,

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

  /// Style for painting backgrounds/borders
  pub style: Arc<ComputedStyle>,
}

impl InlineBoxItem {
  /// Creates a new inline box item
  pub fn new(
    start_edge: f32,
    end_edge: f32,
    content_offset_y: f32,
    metrics: BaselineMetrics,
    style: Arc<ComputedStyle>,
    box_index: usize,
    direction: Direction,
    unicode_bidi: UnicodeBidi,
  ) -> Self {
    Self {
      box_id: 0,
      children: Vec::new(),
      start_edge,
      end_edge,
      content_offset_y,
      border_left: 0.0,
      border_right: 0.0,
      border_top: 0.0,
      border_bottom: 0.0,
      metrics,
      vertical_align: VerticalAlign::Baseline,
      box_index,
      direction,
      unicode_bidi,
      style,
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

  /// Horizontal margins
  pub margin_left: f32,
  pub margin_right: f32,

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
  pub fn new(
    fragment: FragmentNode,
    direction: Direction,
    unicode_bidi: UnicodeBidi,
    margin_left: f32,
    margin_right: f32,
    has_line_baseline: bool,
  ) -> Self {
    let width = fragment.bounds.width();
    let height = fragment.bounds.height();
    let mut first_baseline: Option<f32> = None;
    let mut last_baseline: Option<f32> = None;
    if has_line_baseline {
      collect_line_baselines(&fragment, 0.0, &mut first_baseline, &mut last_baseline);
    }

    let chosen_baseline = if let Some(style) = fragment.style.as_ref() {
      if matches!(style.display, Display::Table | Display::InlineTable) {
        first_baseline.or(last_baseline)
      } else {
        last_baseline
      }
    } else {
      last_baseline
    };

    let metrics = chosen_baseline.map_or_else(
      || BaselineMetrics::for_replaced(height),
      |baseline| {
        let upper = height.max(0.0);
        let clamped_baseline = baseline.max(0.0).min(upper);
        let descent = (height - clamped_baseline).max(0.0);
        BaselineMetrics::new(clamped_baseline, height, clamped_baseline, descent)
      },
    );

    Self {
      fragment,
      width,
      height,
      margin_left,
      margin_right,
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

  pub fn total_width(&self) -> f32 {
    self.margin_left + self.width + self.margin_right
  }
}

/// A laid out ruby segment (base + optional annotations)
#[derive(Debug, Clone)]
pub struct RubySegmentLayout {
  /// Base inline items for this segment
  pub base_items: Vec<InlineItem>,
  /// Optional annotation above the base
  pub annotation_top: Option<Vec<InlineItem>>,
  /// Optional annotation below the base
  pub annotation_bottom: Option<Vec<InlineItem>>,
  /// Metrics for the base line
  pub base_metrics: BaselineMetrics,
  /// Content height for the base line
  pub base_height: f32,
  /// Width of the base content
  pub base_width: f32,
  /// Metrics for the top annotation
  pub top_metrics: Option<BaselineMetrics>,
  /// Metrics for the bottom annotation
  pub bottom_metrics: Option<BaselineMetrics>,
  /// Width of the top annotation line
  pub top_width: f32,
  /// Width of the bottom annotation line
  pub bottom_width: f32,
  /// Total width for this segment (max of base/annotations)
  pub width: f32,
  /// Total height for this segment
  pub height: f32,
  /// Baseline offset for the base relative to the segment top
  pub baseline_offset: f32,
  /// Height occupied by the top annotation
  pub top_height: f32,
  /// Height occupied by the bottom annotation
  pub bottom_height: f32,
  /// Horizontal offset for the base within the segment
  pub base_x: f32,
  /// Horizontal offset for the top annotation
  pub top_x: f32,
  /// Optional spacing distribution for the top annotation line
  pub top_spacing: Option<RubyLineSpacing>,
  /// Horizontal offset for the bottom annotation
  pub bottom_x: f32,
  /// Optional spacing distribution for the bottom annotation line
  pub bottom_spacing: Option<RubyLineSpacing>,
  /// Horizontal offset of this segment within the ruby container
  pub offset_x: f32,
  /// Vertical offset applied to align baselines between segments
  pub offset_y: f32,
}

/// Spacing distribution for ruby annotation lines when using space-between/space-around alignment.
#[derive(Debug, Clone, Copy)]
pub struct RubyLineSpacing {
  /// Leading padding before the first item
  pub leading: f32,
  /// Gap inserted between consecutive annotation items
  pub gap: f32,
}

/// A ruby inline item representing a <ruby> container
#[derive(Debug, Clone)]
pub struct RubyItem {
  /// Segments that compose the ruby container
  pub segments: Vec<RubySegmentLayout>,
  /// Box start edge (padding+border)
  pub start_edge: f32,
  /// Box end edge (padding+border)
  pub end_edge: f32,
  /// Top padding/border inset
  pub content_offset_y: f32,
  /// Metrics for the ruby box
  pub metrics: BaselineMetrics,
  /// Horizontal margins
  pub margin_left: f32,
  pub margin_right: f32,
  /// Optional source box id
  pub box_id: Option<usize>,
  /// Fragment index for split ruby boxes
  pub fragment_index: usize,
  /// Vertical alignment
  pub vertical_align: VerticalAlign,
  /// Bidi direction
  pub direction: Direction,
  /// unicode-bidi behavior
  pub unicode_bidi: UnicodeBidi,
  /// Style used for painting backgrounds/borders
  pub style: Arc<ComputedStyle>,
}

impl RubyItem {
  pub fn width(&self) -> f32 {
    self.margin_left + self.intrinsic_width() + self.margin_right
  }

  pub fn intrinsic_width(&self) -> f32 {
    let segment_width: f32 = self.segments.iter().map(|s| s.width).sum();
    self.start_edge + segment_width + self.end_edge
  }
}

fn collect_line_baselines(
  fragment: &FragmentNode,
  y_offset: f32,
  first: &mut Option<f32>,
  last: &mut Option<f32>,
) {
  let current_offset = y_offset + fragment.bounds.y();
  if let Some(baseline) = fragment.baseline {
    let absolute = current_offset + baseline;
    if first.is_none() {
      *first = Some(absolute);
    }
    *last = Some(absolute);
  }
  if let FragmentContent::Line { baseline } = fragment.content {
    let absolute = current_offset + baseline;
    if first.is_none() {
      *first = Some(absolute);
    }
    *last = Some(absolute);
  }
  for child in fragment.children.iter() {
    collect_line_baselines(child, current_offset, first, last);
  }
}

/// A replaced element item (img, canvas, etc.)
#[derive(Debug, Clone)]
pub struct ReplacedItem {
  /// Width of the element
  pub width: f32,

  /// Height of the element
  pub height: f32,

  /// Horizontal margins
  pub margin_left: f32,
  pub margin_right: f32,

  /// Baseline metrics
  pub metrics: BaselineMetrics,

  /// Vertical alignment
  pub vertical_align: VerticalAlign,

  /// Horizontal advance used for layout (may differ for list markers)
  pub layout_advance: f32,

  /// Paint offset applied at fragment creation (used for outside markers)
  pub paint_offset: f32,

  /// True if this replaced item represents a list marker
  pub is_marker: bool,

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
  pub fn new(
    size: Size,
    replaced_type: ReplacedType,
    style: Arc<ComputedStyle>,
    margin_left: f32,
    margin_right: f32,
  ) -> Self {
    let metrics = BaselineMetrics::for_replaced(size.height);
    Self {
      width: size.width,
      height: size.height,
      margin_left,
      margin_right,
      metrics,
      vertical_align: VerticalAlign::Baseline,
      layout_advance: size.width + margin_left + margin_right,
      paint_offset: 0.0,
      is_marker: false,
      replaced_type,
      direction: style.direction,
      unicode_bidi: style.unicode_bidi,
      style,
    }
  }

  /// Overrides baseline metrics for replaced content.
  pub fn with_metrics(mut self, metrics: BaselineMetrics) -> Self {
    self.metrics = metrics;
    self
  }

  /// Sets the vertical alignment
  pub fn with_vertical_align(mut self, align: VerticalAlign) -> Self {
    self.vertical_align = align;
    self
  }

  /// Marks this replaced item as a list marker and adjusts layout/paint offsets accordingly.
  pub fn as_marker(mut self, gap: f32, position: ListStylePosition, direction: Direction) -> Self {
    let extent = self.width + gap;
    let sign = if direction == Direction::Rtl {
      1.0
    } else {
      -1.0
    };
    if matches!(position, ListStylePosition::Outside) {
      self.layout_advance = 0.0;
      self.paint_offset = sign * extent;
    } else {
      self.layout_advance = extent;
      self.paint_offset = 0.0;
    }
    self.margin_left = 0.0;
    self.margin_right = 0.0;
    self.is_marker = true;
    self
  }

  pub fn total_width(&self) -> f32 {
    self.layout_advance
  }

  pub fn intrinsic_width(&self) -> f32 {
    if self.is_marker {
      self.layout_advance
    } else {
      self.width
    }
  }
}

/// A tab character that expands to the next tab stop.
#[derive(Debug, Clone)]
pub struct TabItem {
  metrics: BaselineMetrics,
  vertical_align: VerticalAlign,
  style: Arc<ComputedStyle>,
  tab_interval: f32,
  resolved_width: f32,
  direction: Direction,
  unicode_bidi: UnicodeBidi,
  allow_wrap: bool,
}

impl TabItem {
  pub fn new(
    style: Arc<ComputedStyle>,
    metrics: BaselineMetrics,
    tab_interval: f32,
    allow_wrap: bool,
  ) -> Self {
    let direction = style.direction;
    let unicode_bidi = style.unicode_bidi;
    Self {
      metrics,
      vertical_align: VerticalAlign::Baseline,
      style,
      tab_interval,
      resolved_width: 0.0,
      direction,
      unicode_bidi,
      allow_wrap,
    }
  }

  pub fn with_vertical_align(mut self, align: VerticalAlign) -> Self {
    self.vertical_align = align;
    self
  }

  pub fn resolve_width(&mut self, start_x: f32) -> f32 {
    if !self.tab_interval.is_finite() || self.tab_interval <= 0.0 {
      self.resolved_width = 0.0;
      return 0.0;
    }
    let remainder = start_x.rem_euclid(self.tab_interval);
    let width = if remainder == 0.0 {
      self.tab_interval
    } else {
      self.tab_interval - remainder
    };
    self.resolved_width = width;
    width
  }

  pub fn width(&self) -> f32 {
    self.used_width()
  }

  pub fn interval(&self) -> f32 {
    self.tab_interval
  }

  pub fn style(&self) -> Arc<ComputedStyle> {
    self.style.clone()
  }

  pub fn metrics(&self) -> BaselineMetrics {
    self.metrics
  }

  pub fn allow_wrap(&self) -> bool {
    self.allow_wrap
  }

  pub fn set_direction(&mut self, direction: Direction) {
    self.direction = direction;
  }

  fn used_width(&self) -> f32 {
    if self.resolved_width > 0.0 {
      self.resolved_width
    } else {
      self.tab_interval.max(0.0)
    }
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

  /// Resolved paragraph direction for this line (LTR/RTL from bidi base level)
  pub resolved_direction: Direction,

  /// Authored text-indent applied to this line in the inline-start direction (can be negative)
  pub indent: f32,

  /// Available width for this line after float shortening
  pub available_width: f32,

  /// Width of the line box (same as available width for now)
  pub box_width: f32,

  /// Horizontal offset of the line box start (used when floats shorten and shift the line)
  pub left_offset: f32,

  /// Vertical offset of the line box top within the inline formatting context
  pub y_offset: f32,

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
      resolved_direction: Direction::Ltr,
      indent: 0.0,
      available_width: 0.0,
      box_width: 0.0,
      left_offset: 0.0,
      y_offset: 0.0,
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
pub struct LineBuilder<'a> {
  /// Available width for the first line
  first_line_width: f32,
  /// Available width for subsequent lines
  subsequent_line_width: f32,
  /// Authored text-indent length (can be negative)
  indent: f32,
  /// Whether hanging indentation is enabled
  indent_hanging: bool,
  /// Whether indentation applies after forced breaks
  indent_each_line: bool,
  /// Whether the next line to start is a paragraph start (first or after hard break)
  next_line_is_para_start: bool,
  /// Float-aware line width provider
  float_integration: Option<InlineFloatIntegration<'a>>,
  /// Current line space when floats are present
  current_line_space: Option<crate::layout::inline::float_integration::LineSpace>,
  /// Accumulated y offset for lines when floats shorten width
  current_y: f32,
  /// Absolute y offset of this inline formatting context within the containing block
  float_base_y: f32,

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

  /// Cache for reshaping repeated substrings within this layout.
  reshape_cache: ReshapeCache,

  /// Base paragraph level for bidi ordering (None = auto/first strong)
  base_level: Option<Level>,

  /// Root unicode-bidi on the paragraph container.
  root_unicode_bidi: UnicodeBidi,
  /// Root direction on the paragraph container.
  root_direction: Direction,
}

impl<'a> LineBuilder<'a> {
  fn compute_indent_for_line(&self, is_para_start: bool, is_first_line: bool) -> f32 {
    if self.indent == 0.0 {
      return 0.0;
    }
    if is_first_line {
      if self.indent_hanging {
        0.0
      } else {
        self.indent
      }
    } else {
      let apply_indent = self.indent_hanging || (self.indent_each_line && is_para_start);
      if apply_indent {
        self.indent
      } else {
        0.0
      }
    }
  }

  fn start_new_line(&mut self) {
    let is_first_line = self.lines.is_empty();
    let para_start = self.next_line_is_para_start;
    self.next_line_is_para_start = false;
    let indent_for_line = self.compute_indent_for_line(para_start, is_first_line);
    let base_width = if is_first_line {
      self.first_line_width
    } else {
      self.subsequent_line_width
    };

    if let Some(integration) = self.float_integration.as_ref() {
      let space = integration.find_line_space(
        self.float_base_y + self.current_y,
        LineSpaceOptions::default().line_height(self.strut_metrics.line_height),
      );
      self.current_line_space = Some(space);
      self.current_line.available_width = space.width;
      self.current_line.box_width = space.width;
      self.current_line.left_offset = space.left_edge;
      self.current_y = self.current_y.max((space.y - self.float_base_y).max(0.0));
    } else {
      self.current_line_space = None;
      self.current_line.available_width = base_width;
      self.current_line.box_width = base_width;
      self.current_line.left_offset = 0.0;
    }
    self.current_line.indent = indent_for_line;
  }

  /// Creates a new line builder
  pub fn new(
    first_line_width: f32,
    subsequent_line_width: f32,
    start_is_para_start: bool,
    _text_wrap: TextWrap,
    indent: f32,
    indent_hanging: bool,
    indent_each_line: bool,
    strut_metrics: BaselineMetrics,
    shaper: ShapingPipeline,
    font_context: FontContext,
    base_level: Option<Level>,
    root_unicode_bidi: UnicodeBidi,
    root_direction: Direction,
    float_integration: Option<InlineFloatIntegration<'a>>,
    float_base_y: f32,
  ) -> Self {
    let initial_direction = if let Some(level) = base_level {
      if level.is_rtl() {
        Direction::Rtl
      } else {
        Direction::Ltr
      }
    } else {
      Direction::Ltr
    };
    let mut builder = Self {
      first_line_width,
      subsequent_line_width,
      indent,
      indent_hanging,
      indent_each_line,
      next_line_is_para_start: start_is_para_start,
      float_integration,
      current_line_space: None,
      current_y: 0.0,
      float_base_y,
      current_line: Line {
        resolved_direction: initial_direction,
        ..Line::new()
      },
      current_x: 0.0,
      lines: Vec::new(),
      baseline_acc: LineBaselineAccumulator::new(&strut_metrics),
      strut_metrics,
      shaper,
      font_context,
      reshape_cache: ReshapeCache::default(),
      base_level,
      root_unicode_bidi,
      root_direction,
    };

    builder.start_new_line();

    builder
  }

  fn current_line_width(&self) -> f32 {
    self.current_line.available_width
  }

  /// Adds an inline item to the builder
  pub fn add_item(&mut self, item: InlineItem) {
    let (item, item_width) = item.resolve_width_at(self.current_x);
    let line_width = self.current_line_width();

    let kind = match &item {
      InlineItem::Text(_) => "text",
      InlineItem::Tab(_) => "tab",
      InlineItem::InlineBox(_) => "inline-box",
      InlineItem::InlineBlock(_) => "inline-block",
      InlineItem::Ruby(_) => "ruby",
      InlineItem::Replaced(_) => "replaced",
      InlineItem::Floating(_) => "floating",
      InlineItem::StaticPositionAnchor(_) => "anchor",
    };

    if log_line_width_enabled() {
      eprintln!(
        "[line-add] kind={} line_width={:.2} current_x={:.2} item_width={:.2} breakable={}",
        kind,
        line_width,
        self.current_x,
        item_width,
        item.is_breakable()
      );
    }

    // Check if item fits
    if self.current_x + item_width <= line_width {
      // Item fits
      self.place_item_with_width(item, item_width);
    } else if item.is_breakable() {
      // Try to break the item even on an empty line; oversized items should
      // still honor break opportunities instead of overflowing the line.
      self.add_breakable_item(item);
    } else {
      // Item doesn't fit and can't be broken
      if self.current_line.is_empty() {
        // No break possible; overflow this line
        self.place_item_with_width(item, item_width);
      } else {
        self.finish_line();
        let (resolved, width) = item.resolve_width_at(self.current_x);
        self.place_item_with_width(resolved, width);
      }
    }
  }

  /// Adds a breakable item (text or inline box), handling line breaking
  fn add_breakable_item(&mut self, item: InlineItem) {
    if let InlineItem::Text(text_item) = item {
      let remaining_width = (self.current_line_width() - self.current_x).max(0.0);

      if log_line_width_enabled() {
        eprintln!(
          "[line-width] remaining {:.2} advance {:.2} breaks {}",
          remaining_width,
          text_item.advance_for_layout,
          text_item.break_opportunities.len()
        );
      }

      let mut break_opportunity = text_item.find_break_point(remaining_width);
      if break_opportunity.is_none() && self.current_line.is_empty() {
        // No break fits within the remaining width, but the line is empty.
        // Split at the earliest opportunity to avoid keeping multiple words
        // on an overflowing line.
        break_opportunity = text_item.break_opportunities.first().copied();
        if break_opportunity.is_none()
          && allows_soft_wrap(text_item.style.as_ref())
          && matches!(
            text_item.style.word_break,
            WordBreak::BreakWord | WordBreak::Anywhere
          )
        {
          if let Some((idx, _)) = text_item.text.char_indices().nth(1) {
            break_opportunity = Some(BreakOpportunity::allowed(idx));
          }
        }
      }

      if let Some(break_opportunity) = break_opportunity {
        // Split at break point
        if let Some((before, after)) = text_item.split_at(
          break_opportunity.byte_offset,
          break_opportunity.adds_hyphen,
          &self.shaper,
          &self.font_context,
          &mut self.reshape_cache,
        ) {
          // Place the part that fits
          if before.advance_for_layout > 0.0 {
            let width = before.advance_for_layout;
            self.place_item_with_width(InlineItem::Text(before), width);
          }

          // Start new line for the rest
          if matches!(break_opportunity.break_type, BreakType::Mandatory) {
            self.current_line.ends_with_hard_break = true;
          }
          self.finish_line();

          // Add remaining text (may need further breaking)
          self.add_item(InlineItem::Text(after));
        } else {
          // If splitting fails, fall back to placing the whole item
          let width = text_item.advance_for_layout;
          if self.current_line.is_empty() {
            self.place_item_with_width(InlineItem::Text(text_item), width);
          } else {
            self.finish_line();
            self.place_item_with_width(InlineItem::Text(text_item), width);
          }
        }
      } else {
        // No break point found within remaining width
        if self.current_line.is_empty() || !allows_soft_wrap(text_item.style.as_ref()) {
          // Wrapping is disabled or nothing is on the line; overflow in place.
          let width = text_item.advance_for_layout;
          self.place_item_with_width(InlineItem::Text(text_item), width);
        } else {
          // Start new line and try again
          self.finish_line();
          self.add_item(InlineItem::Text(text_item));
        }
      }
    } else if let InlineItem::InlineBox(inline_box) = item {
      let remaining_width = (self.current_line_width() - self.current_x).max(0.0);
      let total_width = inline_box.width();

      if total_width <= remaining_width {
        self.place_item_with_width(InlineItem::InlineBox(inline_box), total_width);
        return;
      }

      // If nothing has been placed yet, flatten the inline box and lay out its children so they can break.
      if !self.current_line.is_empty() {
        self.finish_line();
      }

      if inline_box.start_edge > 0.0 {
        self.current_x += inline_box.start_edge;
      }
      for child in inline_box.children {
        self.add_item(child);
      }
      if inline_box.end_edge > 0.0 {
        self.current_x += inline_box.end_edge;
      }
    }
  }

  /// Places an item on the current line without breaking
  fn place_item_with_width(&mut self, item: InlineItem, item_width: f32) {
    let metrics = item.baseline_metrics();
    let vertical_align = item.vertical_align();

    // Calculate baseline offset
    let baseline_offset = if vertical_align.is_line_relative() {
      self
        .baseline_acc
        .add_line_relative(&metrics, vertical_align);
      0.0 // Will be adjusted in finalization
    } else {
      // Baseline-relative alignments (e.g., middle/sub/super/text-top) depend on the
      // parent's font metrics. The strut represents the parent inline box, so thread it
      // through to compute x-height/ascent-based offsets correctly.
      self
        .baseline_acc
        .add_baseline_relative(&metrics, vertical_align, Some(&self.strut_metrics))
    };

    let positioned = PositionedItem {
      item,
      x: self.current_x,
      baseline_offset,
    };

    self.current_x += item_width;
    self.current_line.items.push(positioned);
  }

  pub(crate) fn split_text_item(
    &mut self,
    item: &TextItem,
    byte_offset: usize,
    insert_hyphen: bool,
  ) -> Option<(TextItem, TextItem)> {
    item.split_at(
      byte_offset,
      insert_hyphen,
      &self.shaper,
      &self.font_context,
      &mut self.reshape_cache,
    )
  }

  /// Forces a line break (e.g., from mandatory break)
  pub fn force_break(&mut self) {
    self.current_line.ends_with_hard_break = true;
    self.finish_line();
  }

  /// Finishes the current line and starts a new one
  fn finish_line(&mut self) {
    if !self.current_line.is_empty() {
      // Calculate final line metrics
      self.current_line.width = self.current_x;
      self.current_line.height = self.baseline_acc.line_height();
      self.current_line.baseline = self.baseline_acc.baseline_position();
      self.current_line.y_offset = self.current_y;
      let line_width = self.current_line_width();
      self.current_line.available_width = line_width;
      if self.current_line_space.is_some() {
        self.current_line.box_width = self
          .current_line_space
          .map(|space| space.width)
          .unwrap_or(line_width);
      } else {
        self.current_line.box_width = line_width;
      }

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

      let ended_hard = self.current_line.ends_with_hard_break;
      let finished_height = self.current_line.height;
      self.lines.push(std::mem::take(&mut self.current_line));
      self.current_y += finished_height;
      self.next_line_is_para_start = ended_hard;
    }

    // Reset for new line
    self.current_x = 0.0;
    self.baseline_acc = LineBaselineAccumulator::new(&self.strut_metrics);
    let next_direction = if let Some(level) = self.base_level {
      if level.is_rtl() {
        Direction::Rtl
      } else {
        Direction::Ltr
      }
    } else {
      Direction::Ltr
    };
    self.current_line = Line {
      resolved_direction: next_direction,
      ..Line::new()
    };
    self.start_new_line();
  }

  /// Run bidi reordering across all built lines, respecting paragraph boundaries.
  ///
  /// The Unicode Bidi algorithm operates at paragraph scope; explicit embeddings/isolates can span
  /// line breaks. We therefore resolve bidi across each paragraph (lines separated by hard breaks)
  /// and then reorder each line using the paragraph-level embedding results.
  fn reorder_lines_for_bidi(&mut self) {
    if self.lines.is_empty() {
      return;
    }

    let mut ranges = Vec::new();
    let mut start = 0usize;
    for (idx, line) in self.lines.iter().enumerate() {
      if line.ends_with_hard_break {
        ranges.push((start, idx + 1));
        start = idx + 1;
      }
    }

    if start < self.lines.len() {
      ranges.push((start, self.lines.len()));
    }

    let base_level = self.base_level;
    let shaper = self.shaper.clone();
    let font_context = self.font_context.clone();

    for (start, end) in ranges {
      let paragraph_level = if self.root_unicode_bidi == UnicodeBidi::Plaintext
        || Self::paragraph_all_plaintext(&self.lines[start..end])
      {
        None
      } else {
        base_level
      };
      reorder_paragraph(
        &mut self.lines[start..end],
        paragraph_level,
        self.root_unicode_bidi,
        self.root_direction,
        &shaper,
        &font_context,
      );
    }
  }

  fn paragraph_all_plaintext(lines: &[Line]) -> bool {
    let mut saw_plaintext = false;
    let all_plain = lines.iter().all(|line| {
      line
        .items
        .iter()
        .all(|p| Self::item_allows_plaintext(&p.item, &mut saw_plaintext))
    });
    all_plain && saw_plaintext
  }

  fn item_allows_plaintext(item: &InlineItem, saw_plaintext: &mut bool) -> bool {
    use crate::style::types::UnicodeBidi;
    match item {
      InlineItem::Text(t) => {
        if matches!(t.style.unicode_bidi, UnicodeBidi::Plaintext) {
          *saw_plaintext = true;
          true
        } else {
          false
        }
      }
      InlineItem::InlineBox(b) => {
        if matches!(b.unicode_bidi, UnicodeBidi::Plaintext) {
          *saw_plaintext = true;
          true
        } else {
          b.children
            .iter()
            .all(|c| Self::item_allows_plaintext(c, saw_plaintext))
        }
      }
      InlineItem::Floating(f) => {
        if matches!(f.unicode_bidi, UnicodeBidi::Plaintext) {
          *saw_plaintext = true;
          true
        } else {
          false
        }
      }
      InlineItem::Ruby(r) => {
        if matches!(r.unicode_bidi, UnicodeBidi::Plaintext) {
          *saw_plaintext = true;
          true
        } else {
          r.segments.iter().all(|seg| {
            seg
              .base_items
              .iter()
              .all(|c| Self::item_allows_plaintext(c, saw_plaintext))
          })
        }
      }
      InlineItem::InlineBlock(_) | InlineItem::Replaced(_) | InlineItem::Tab(_) => true,
      InlineItem::StaticPositionAnchor(_) => true,
    }
  }

  /// Finishes building and returns all lines
  pub fn finish(mut self) -> Vec<Line> {
    // Finish any remaining line
    self.finish_line();
    self.reorder_lines_for_bidi();
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

/// Resolve bidi at paragraph scope and reorder each line using the paragraph embedding levels.
fn reorder_paragraph(
  lines: &mut [Line],
  base_level: Option<Level>,
  root_unicode_bidi: UnicodeBidi,
  root_direction: Direction,
  shaper: &ShapingPipeline,
  font_context: &FontContext,
) {
  if lines.is_empty() {
    return;
  }

  #[derive(Clone, PartialEq, Eq)]
  struct BidiScope {
    unicode_bidi: UnicodeBidi,
    direction: Direction,
    open: &'static [char],
    close: &'static [char],
  }

  #[derive(Clone, Copy)]
  struct ContentChar {
    leaf_index: usize,
    local_start: usize,
    local_end: usize,
    bidi_byte_index: usize,
  }

  #[derive(Clone, Copy)]
  struct VisualSegment {
    leaf_index: usize,
    local_start: usize,
    local_end: usize,
    level: Level,
  }

  fn scope_for(unicode_bidi: UnicodeBidi, direction: Direction) -> Option<BidiScope> {
    use UnicodeBidi::*;

    const LRE: char = '\u{202A}';
    const RLE: char = '\u{202B}';
    const LRO: char = '\u{202D}';
    const RLO: char = '\u{202E}';
    const PDF: char = '\u{202C}';
    const LRI: char = '\u{2066}';
    const RLI: char = '\u{2067}';
    const PDI: char = '\u{2069}';

    let (open, close) = match unicode_bidi {
      Normal => return None,
      Plaintext => return None,
      Embed => {
        if matches!(direction, Direction::Rtl) {
          (&[RLE][..], &[PDF][..])
        } else {
          (&[LRE][..], &[PDF][..])
        }
      }
      BidiOverride => {
        if matches!(direction, Direction::Rtl) {
          (&[RLO][..], &[PDF][..])
        } else {
          (&[LRO][..], &[PDF][..])
        }
      }
      Isolate => {
        if matches!(direction, Direction::Rtl) {
          (&[RLI][..], &[PDI][..])
        } else {
          (&[LRI][..], &[PDI][..])
        }
      }
      IsolateOverride => {
        if matches!(direction, Direction::Rtl) {
          (&[RLI, RLO][..], &[PDF, PDI][..])
        } else {
          (&[LRI, LRO][..], &[PDF, PDI][..])
        }
      }
    };

    Some(BidiScope {
      unicode_bidi,
      direction,
      open,
      close,
    })
  }

  let mut box_counter = 0usize;
  let mut line_leaves: Vec<Vec<BidiLeaf>> = Vec::with_capacity(lines.len());
  for line in lines.iter() {
    let mut leaves = Vec::new();
    for positioned in &line.items {
      flatten_positioned_item(positioned, &mut Vec::new(), &mut box_counter, &mut leaves);
    }
    line_leaves.push(leaves);
  }

  let mut paragraph_leaves: Vec<ParagraphLeaf> = Vec::new();
  let mut leaf_contexts: Vec<Vec<(UnicodeBidi, Direction)>> = Vec::new();
  let mut paragraph_text = String::new();
  let mut open_scopes: Vec<BidiScope> = Vec::new();
  let mut content_map: Vec<ContentChar> = Vec::new();
  let mut line_ranges: Vec<std::ops::Range<usize>> = Vec::with_capacity(lines.len());
  let mut content_index = 0usize;
  let root_context = (root_unicode_bidi, root_direction);

  for leaves in &line_leaves {
    let line_start = content_index;
    for leaf in leaves {
      let leaf_index = paragraph_leaves.len();
      let mut stack: Vec<(UnicodeBidi, Direction)> = vec![root_context];
      stack.extend(leaf.box_stack.iter().map(|c| (c.unicode_bidi, c.direction)));
      stack.push((leaf.item.unicode_bidi(), leaf.item.direction()));
      leaf_contexts.push(stack.clone());
      paragraph_leaves.push(ParagraphLeaf {
        leaf: leaf.clone(),
        bidi_context: None,
      });

      let desired_scopes: Vec<BidiScope> = stack
        .iter()
        .filter_map(|(ub, dir)| scope_for(*ub, *dir))
        .collect();
      let common = desired_scopes
        .iter()
        .zip(open_scopes.iter())
        .take_while(|(a, b)| *a == *b)
        .count();

      for scope in open_scopes.drain(common..).rev() {
        for ch in scope.close {
          paragraph_text.push(*ch);
        }
      }

      for scope in desired_scopes.iter().skip(common) {
        for ch in scope.open {
          paragraph_text.push(*ch);
        }
        open_scopes.push(scope.clone());
      }

      match &leaf.item {
        InlineItem::Text(t) => {
          for (byte_idx, ch) in t.text.char_indices() {
            let bidi_byte_index = paragraph_text.len();
            content_map.push(ContentChar {
              leaf_index,
              local_start: byte_idx,
              local_end: byte_idx + ch.len_utf8(),
              bidi_byte_index,
            });
            paragraph_text.push(ch);
            content_index += 1;
          }
        }
        InlineItem::Tab(_) => {
          let bidi_byte_index = paragraph_text.len();
          content_map.push(ContentChar {
            leaf_index,
            local_start: 0,
            local_end: 0,
            bidi_byte_index,
          });
          paragraph_text.push('\t');
          content_index += 1;
        }
        InlineItem::InlineBox(_) => {}
        _ => {
          let bidi_byte_index = paragraph_text.len();
          content_map.push(ContentChar {
            leaf_index,
            local_start: 0,
            local_end: 0,
            bidi_byte_index,
          });
          paragraph_text.push('\u{FFFC}');
          content_index += 1;
        }
      }
    }
    line_ranges.push(line_start..content_index);
  }

  for scope in open_scopes.iter().rev() {
    for ch in scope.close {
      paragraph_text.push(*ch);
    }
  }

  if content_map.is_empty() {
    return;
  }

  let resolved_base = if let Some(level) = base_level {
    level
  } else if paragraph_text.is_empty() {
    Level::ltr()
  } else {
    let info = BidiInfo::new(&paragraph_text, None);
    info
      .paragraphs
      .first()
      .map(|p| p.level)
      .unwrap_or_else(Level::ltr)
  };

  let bidi = unicode_bidi::BidiInfo::new(&paragraph_text, Some(resolved_base));
  let paragraph_direction = if resolved_base.is_rtl() {
    Direction::Rtl
  } else {
    Direction::Ltr
  };
  for line in lines.iter_mut() {
    line.resolved_direction = paragraph_direction;
  }

  for (leaf, stack) in paragraph_leaves.iter_mut().zip(leaf_contexts.iter()) {
    leaf.bidi_context =
      crate::layout::contexts::inline::explicit_bidi_context(paragraph_direction, stack);
  }

  let mut content_levels: Vec<Level> = Vec::with_capacity(content_map.len());
  for entry in &content_map {
    let lvl = bidi
      .levels
      .get(entry.bidi_byte_index)
      .copied()
      .unwrap_or(resolved_base);
    content_levels.push(lvl);
  }

  let push_segment = |entry: &ContentChar, level: Level, segments: &mut Vec<VisualSegment>| {
    if let Some(last) = segments.last_mut() {
      if last.leaf_index == entry.leaf_index && last.level == level {
        if last.local_end == entry.local_start {
          last.local_end = entry.local_end;
          return;
        }
        if entry.local_end == last.local_start {
          last.local_start = entry.local_start;
          return;
        }
      }
    }

    segments.push(VisualSegment {
      leaf_index: entry.leaf_index,
      local_start: entry.local_start,
      local_end: entry.local_end,
      level,
    });
  };

  for (line_idx, line_range) in line_ranges.into_iter().enumerate() {
    if line_range.is_empty() {
      continue;
    }

    let slice_levels = &content_levels[line_range.clone()];
    let order = unicode_bidi::BidiInfo::reorder_visual(slice_levels);
    let mut segments: Vec<VisualSegment> = Vec::new();
    for visual_idx in order {
      let content_idx = line_range.start + visual_idx;
      if let Some(entry) = content_map.get(content_idx) {
        let lvl = content_levels
          .get(content_idx)
          .copied()
          .unwrap_or(resolved_base);
        push_segment(entry, lvl, &mut segments);
      }
    }

    if segments.is_empty() {
      continue;
    }

    let mut visual_fragments: Vec<BidiLeaf> = Vec::new();
    for seg in segments {
      if let Some(para_leaf) = paragraph_leaves.get(seg.leaf_index) {
        let mut frag = para_leaf.leaf.clone();
        if let InlineItem::Text(text_item) = &para_leaf.leaf.item {
          if let Some(sliced) = slice_text_item(
            text_item,
            seg.local_start..seg.local_end,
            shaper,
            font_context,
            paragraph_direction,
            para_leaf.bidi_context,
          ) {
            frag.item = InlineItem::Text(sliced);
          } else {
            continue;
          }
        }
        visual_fragments.push(frag);
      }
    }

    let mut box_positions: HashMap<usize, (usize, usize)> = HashMap::new();
    for (vis_pos, frag) in visual_fragments.iter().enumerate() {
      for ctx in &frag.box_stack {
        box_positions
          .entry(ctx.id)
          .and_modify(|entry| entry.1 = vis_pos)
          .or_insert((vis_pos, vis_pos));
      }
    }

    fn coalesce_inline_boxes(items: Vec<PositionedItem>) -> Vec<PositionedItem> {
      let mut out: Vec<PositionedItem> = Vec::new();
      for item in items {
        if let Some(last) = out.last_mut() {
          if let (InlineItem::InlineBox(prev), InlineItem::InlineBox(mut curr)) =
            (&mut last.item, item.item.clone())
          {
            if prev.box_index == curr.box_index {
              prev.children.append(&mut curr.children);
              prev.end_edge = curr.end_edge;
              continue;
            }
          }
        }
        out.push(item);
      }
      out
    }

    let mut reordered: Vec<PositionedItem> = Vec::new();
    for (vis_pos, frag) in visual_fragments.into_iter().enumerate() {
      let mut item = frag.item;

      for ctx in frag.box_stack.iter().rev() {
        let (first, last) = box_positions
          .get(&ctx.id)
          .copied()
          .unwrap_or((vis_pos, vis_pos));
        let start_edge = if vis_pos == first {
          ctx.start_edge
        } else {
          0.0
        };
        let end_edge = if vis_pos == last { ctx.end_edge } else { 0.0 };

        let mut inline_box = InlineBoxItem::new(
          start_edge,
          end_edge,
          ctx.content_offset_y,
          ctx.metrics,
          ctx.style.clone(),
          ctx.box_index,
          ctx.direction,
          ctx.unicode_bidi,
        );
        inline_box.box_id = ctx.box_id;
        inline_box.vertical_align = ctx.vertical_align;
        inline_box.add_child(item);
        item = InlineItem::InlineBox(inline_box);
      }

      let positioned = PositionedItem {
        item,
        x: 0.0,
        baseline_offset: frag.baseline_offset,
      };
      reordered.push(positioned);
    }

    let mut reordered = coalesce_inline_boxes(reordered);
    let mut x = 0.0;
    for positioned in &mut reordered {
      positioned.x = x;
      x += positioned.item.width();
    }

    let width: f32 = reordered.iter().map(|p| p.item.width()).sum();
    let line = &mut lines[line_idx];
    line.width = width;
    line.items = reordered;
  }
}

fn slice_text_item(
  item: &TextItem,
  range: std::ops::Range<usize>,
  pipeline: &ShapingPipeline,
  font_context: &FontContext,
  base_direction: Direction,
  bidi_context: Option<crate::text::pipeline::ExplicitBidiContext>,
) -> Option<TextItem> {
  if range.start >= range.end || range.end > item.text.len() {
    return None;
  }

  // Fast path for synthetic items used in tests that don't carry shaped runs.
  if item.runs.is_empty() {
    let advance_at = |byte_offset: usize| -> f32 {
      item
        .cluster_advances
        .iter()
        .rev()
        .find(|b| b.byte_offset <= byte_offset)
        .map(|b| b.advance)
        .unwrap_or(0.0)
    };
    let start_adv = advance_at(range.start);
    let end_adv = advance_at(range.end);
    let width = (end_adv - start_adv).max(0.0);

    let cluster_advances = item
      .cluster_advances
      .iter()
      .filter(|b| b.byte_offset >= range.start && b.byte_offset <= range.end)
      .map(|b| ClusterBoundary {
        byte_offset: b.byte_offset - range.start,
        advance: (b.advance - start_adv).max(0.0),
        run_index: b.run_index,
        glyph_end: b.glyph_end,
        run_advance: (b.run_advance - start_adv).max(0.0),
      })
      .collect();

    let breaks: Vec<BreakOpportunity> = item
      .break_opportunities
      .iter()
      .filter(|b| b.byte_offset >= range.start && b.byte_offset <= range.end)
      .map(|b| {
        BreakOpportunity::with_hyphen(b.byte_offset - range.start, b.break_type, b.adds_hyphen)
      })
      .collect();
    let first_mandatory_break = TextItem::first_mandatory_break(&breaks);
    let forced = item
      .forced_break_offsets
      .iter()
      .copied()
      .filter(|o| *o >= range.start && *o <= range.end)
      .map(|o| o - range.start)
      .collect();

    return Some(TextItem {
      box_id: item.box_id,
      runs: Vec::new(),
      advance: width,
      advance_for_layout: if item.is_marker {
        item.advance_for_layout.min(width)
      } else {
        width
      },
      metrics: item.metrics,
      vertical_align: item.vertical_align,
      break_opportunities: breaks,
      forced_break_offsets: forced,
      first_mandatory_break,
      text: item.text[range.clone()].to_string(),
      font_size: item.font_size,
      style: item.style.clone(),
      base_direction,
      explicit_bidi: bidi_context,
      is_marker: item.is_marker,
      paint_offset: item.paint_offset,
      cluster_advances,
      source_range: item.source_range.start + range.start..item.source_range.start + range.end,
      source_id: item.source_id,
    });
  }

  let slice_text = &item.text[range.clone()];
  let mut runs = pipeline
    .shape_with_context(
      slice_text,
      &item.style,
      font_context,
      pipeline_dir_from_style(base_direction),
      bidi_context,
    )
    .ok()?;
  TextItem::apply_spacing_to_runs(
    &mut runs,
    slice_text,
    item.style.letter_spacing,
    item.style.word_spacing,
  );

  let metrics = TextItem::metrics_from_runs(&runs, item.metrics.line_height, item.font_size);
  let breaks = item
    .break_opportunities
    .iter()
    .filter(|b| b.byte_offset >= range.start && b.byte_offset <= range.end)
    .map(|b| {
      BreakOpportunity::with_hyphen(b.byte_offset - range.start, b.break_type, b.adds_hyphen)
    })
    .collect();
  let forced = item
    .forced_break_offsets
    .iter()
    .copied()
    .filter(|o| *o >= range.start && *o <= range.end)
    .map(|o| o - range.start)
    .collect();

  let mut new_item = TextItem::new(
    runs,
    slice_text.to_string(),
    metrics,
    breaks,
    forced,
    item.style.clone(),
    base_direction,
  )
  .with_vertical_align(item.vertical_align);
  new_item.box_id = item.box_id;
  new_item.explicit_bidi = bidi_context;
  new_item.source_id = item.source_id;
  new_item.source_range =
    item.source_range.start + range.start..item.source_range.start + range.end;
  if item.is_marker {
    new_item.is_marker = true;
    new_item.paint_offset = item.paint_offset;
    new_item.advance_for_layout = item.advance_for_layout.min(new_item.advance);
  }

  Some(new_item)
}

#[derive(Clone)]
struct BoxContext {
  id: usize,
  box_id: usize,
  start_edge: f32,
  end_edge: f32,
  content_offset_y: f32,
  metrics: BaselineMetrics,
  vertical_align: VerticalAlign,
  box_index: usize,
  direction: Direction,
  unicode_bidi: UnicodeBidi,
  style: Arc<ComputedStyle>,
}

#[derive(Clone)]
struct BidiLeaf {
  item: InlineItem,
  baseline_offset: f32,
  box_stack: Vec<BoxContext>,
}

#[derive(Clone)]
struct ParagraphLeaf {
  leaf: BidiLeaf,
  bidi_context: Option<crate::text::pipeline::ExplicitBidiContext>,
}

fn flatten_positioned_item(
  positioned: &PositionedItem,
  box_stack: &mut Vec<BoxContext>,
  box_counter: &mut usize,
  leaves: &mut Vec<BidiLeaf>,
) {
  match &positioned.item {
    InlineItem::InlineBox(inline_box) => {
      let id = *box_counter;
      *box_counter += 1;
      let ctx = BoxContext {
        id,
        box_id: inline_box.box_id,
        start_edge: inline_box.start_edge,
        end_edge: inline_box.end_edge,
        content_offset_y: inline_box.content_offset_y,
        metrics: inline_box.metrics,
        vertical_align: inline_box.vertical_align,
        box_index: inline_box.box_index,
        direction: inline_box.direction,
        unicode_bidi: inline_box.unicode_bidi,
        style: inline_box.style.clone(),
      };
      box_stack.push(ctx);
      for child in &inline_box.children {
        let child_positioned = PositionedItem {
          item: child.clone(),
          x: positioned.x,
          baseline_offset: positioned.baseline_offset,
        };
        flatten_positioned_item(&child_positioned, box_stack, box_counter, leaves);
      }
      box_stack.pop();
    }
    _ => {
      leaves.push(BidiLeaf {
        item: positioned.item.clone(),
        baseline_offset: positioned.baseline_offset,
        box_stack: box_stack.clone(),
      });
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::geometry::Rect;
  use crate::layout::contexts::inline::explicit_bidi_context;
  use crate::style::types::FontKerning;
  use crate::style::ComputedStyle;
  use crate::text::font_loader::FontContext;
  use crate::text::line_break::find_break_opportunities;
  use crate::text::pipeline::ShapingPipeline;
  use std::sync::Arc;
  use unicode_bidi::level;
  use unicode_bidi::BidiInfo;
  use unicode_bidi::Level;

  fn make_strut_metrics() -> BaselineMetrics {
    BaselineMetrics::new(12.0, 16.0, 12.0, 4.0)
  }

  fn make_builder(width: f32) -> LineBuilder<'static> {
    let strut = make_strut_metrics();
    LineBuilder::new(
      width,
      width,
      true,
      TextWrap::Auto,
      0.0,
      false,
      false,
      strut,
      ShapingPipeline::new(),
      FontContext::new(),
      Some(Level::ltr()),
      UnicodeBidi::Normal,
      Direction::Ltr,
      None,
      0.0,
    )
  }

  fn pipeline_dir_from_style(dir: Direction) -> crate::text::pipeline::Direction {
    match dir {
      Direction::Ltr => crate::text::pipeline::Direction::LeftToRight,
      Direction::Rtl => crate::text::pipeline::Direction::RightToLeft,
    }
  }

  fn make_builder_with_base(width: f32, base: Level) -> LineBuilder<'static> {
    let strut = make_strut_metrics();
    LineBuilder::new(
      width,
      width,
      true,
      TextWrap::Auto,
      0.0,
      false,
      false,
      strut,
      ShapingPipeline::new(),
      FontContext::new(),
      Some(base),
      UnicodeBidi::Normal,
      Direction::Ltr,
      None,
      0.0,
    )
  }

  fn make_text_item(text: &str, advance: f32) -> TextItem {
    make_text_item_with_bidi(text, advance, UnicodeBidi::Normal)
  }

  fn make_text_item_with_bidi(text: &str, advance: f32, ub: UnicodeBidi) -> TextItem {
    let mut style = ComputedStyle::default();
    style.unicode_bidi = ub;
    let style = Arc::new(style);
    let mut cluster_advances = Vec::new();
    if !text.is_empty() {
      let step = advance / text.len() as f32;
      for i in 1..=text.len() {
        cluster_advances.push(ClusterBoundary {
          byte_offset: i,
          advance: step * i as f32,
          run_index: None,
          glyph_end: None,
          run_advance: step * i as f32,
        });
      }
    }
    let breaks = find_break_opportunities(text);
    TextItem {
      box_id: 0,
      runs: Vec::new(),
      advance,
      advance_for_layout: advance,
      metrics: make_strut_metrics(),
      vertical_align: VerticalAlign::Baseline,
      break_opportunities: breaks.clone(),
      forced_break_offsets: Vec::new(),
      first_mandatory_break: TextItem::first_mandatory_break(&breaks),
      text: text.to_string(),
      font_size: 16.0,
      style: style.clone(),
      base_direction: crate::style::types::Direction::Ltr,
      explicit_bidi: None,
      is_marker: false,
      paint_offset: 0.0,
      cluster_advances,
      source_range: 0..text.len(),
      source_id: TextItem::hash_text(text),
    }
  }

  fn old_find_break_point(item: &TextItem, max_width: f32) -> Option<BreakOpportunity> {
    let mut mandatory_break: Option<BreakOpportunity> = None;
    let mut allowed_break: Option<BreakOpportunity> = None;

    for brk in &item.break_opportunities {
      let width_at_break = item.advance_at_offset(brk.byte_offset);
      if width_at_break <= max_width {
        match brk.break_type {
          BreakType::Mandatory => {
            if mandatory_break.is_none() {
              mandatory_break = Some(*brk);
            }
          }
          BreakType::Allowed => allowed_break = Some(*brk),
        }
      } else {
        break;
      }
    }

    let mut best_break = mandatory_break.or(allowed_break);
    if best_break.is_some() || !allows_soft_wrap(item.style.as_ref()) {
      return best_break;
    }

    let can_overflow_break_word = matches!(
      item.style.word_break,
      WordBreak::BreakWord | WordBreak::Anywhere
    );
    let can_overflow_break_by_wrap = item.style.overflow_wrap == OverflowWrap::BreakWord;

    if can_overflow_break_word || can_overflow_break_by_wrap {
      for (idx, _) in item.text.char_indices().skip(1) {
        let width_at_break = item.advance_at_offset(idx);
        if width_at_break <= max_width {
          best_break = Some(BreakOpportunity::allowed(idx));
        } else {
          break;
        }
      }
    }

    best_break
  }

  fn glyph_signature_sequence(item: &TextItem) -> Vec<(usize, u8, bool)> {
    item
      .runs
      .iter()
      .flat_map(|run| {
        let rtl = run.direction.is_rtl();
        run.glyphs.iter().map(move |glyph| {
          (
            item.source_range.start + run.start + glyph.cluster as usize,
            run.level,
            rtl,
          )
        })
      })
      .collect()
  }

  fn force_split_fallback(item: &mut TextItem) {
    for boundary in &mut item.cluster_advances {
      boundary.run_index = None;
      boundary.glyph_end = None;
    }
  }

  fn make_shaped_text_item(
    text: &str,
    style: Arc<ComputedStyle>,
    base_direction: Direction,
    explicit_bidi: Option<crate::text::pipeline::ExplicitBidiContext>,
    pipeline: &ShapingPipeline,
    font_context: &FontContext,
  ) -> TextItem {
    let mut runs = pipeline
      .shape_with_context(
        text,
        &style,
        font_context,
        pipeline_dir_from_style(base_direction),
        explicit_bidi,
      )
      .expect("text shaping should succeed in tests");
    TextItem::apply_spacing_to_runs(&mut runs, text, style.letter_spacing, style.word_spacing);
    let metrics = TextItem::metrics_from_runs(&runs, 16.0, style.font_size);
    let breaks = find_break_opportunities(text);
    let mut item = TextItem::new(
      runs,
      text.to_string(),
      metrics,
      breaks,
      Vec::new(),
      style,
      base_direction,
    );
    item.explicit_bidi = explicit_bidi;
    item
  }

  fn assert_split_matches_original(
    item: &TextItem,
    split_offset: usize,
    before: &TextItem,
    after: &TextItem,
  ) {
    const EPS: f32 = 0.05;
    assert!(
      (before.advance + after.advance - item.advance).abs() < EPS,
      "split advances should add up: {} + {} vs {}",
      before.advance,
      after.advance,
      item.advance
    );

    let signatures = glyph_signature_sequence(item);
    let expected_before: Vec<(usize, u8, bool)> = signatures
      .iter()
      .copied()
      .filter(|(cluster, _, _)| *cluster < split_offset)
      .collect();
    let expected_after: Vec<(usize, u8, bool)> = signatures
      .iter()
      .copied()
      .filter(|(cluster, _, _)| *cluster >= split_offset)
      .collect();

    assert_eq!(glyph_signature_sequence(before), expected_before);
    assert_eq!(glyph_signature_sequence(after), expected_after);
  }

  #[test]
  fn reshape_cache_preserves_base_direction_and_explicit_bidi() {
    let pipeline = ShapingPipeline::new();
    let font_context = FontContext::new();
    let mut style = ComputedStyle::default();
    style.direction = Direction::Ltr;
    style.font_kerning = FontKerning::None;
    let style = Arc::new(style);
    let mut reshape_cache = ReshapeCache::default();

    // Explicit override case: should reorder Latin text in RTL override.
    let text = "abcd";
    let explicit = explicit_bidi_context(
      Direction::Rtl,
      &[(UnicodeBidi::BidiOverride, Direction::Rtl)],
    )
    .expect("expected explicit bidi context");
    let mut overridden = make_shaped_text_item(
      text,
      style.clone(),
      Direction::Rtl,
      Some(explicit),
      &pipeline,
      &font_context,
    );
    overridden.box_id = 1;
    assert!(
      overridden.runs.iter().all(|run| run.direction.is_rtl()),
      "explicit bidi override should force RTL runs"
    );

    force_split_fallback(&mut overridden);
    let (before, after) = overridden
      .split_at(2, false, &pipeline, &font_context, &mut reshape_cache)
      .expect("split should succeed");
    let ctx_key = |item: &TextItem| {
      item
        .explicit_bidi
        .map(|ctx| (ctx.level.number(), ctx.override_all))
    };
    assert_eq!(ctx_key(&before), ctx_key(&overridden));
    assert_eq!(ctx_key(&after), ctx_key(&overridden));
    assert_split_matches_original(&overridden, 2, &before, &after);

    // Same text/style/base-direction but without explicit override should not reuse the cached override shaping.
    pipeline.clear_cache();
    let mut plain = make_shaped_text_item(
      text,
      style.clone(),
      Direction::Rtl,
      None,
      &pipeline,
      &font_context,
    );
    force_split_fallback(&mut plain);
    let (before_plain, after_plain) = plain
      .split_at(2, false, &pipeline, &font_context, &mut reshape_cache)
      .expect("split should succeed");
    assert_split_matches_original(&plain, 2, &before_plain, &after_plain);

    // Base-direction case: mixed LTR/RTL content should shape differently when the paragraph base differs.
    pipeline.clear_cache();
    let mixed = "ABC אבג";
    let split_offset = mixed
      .find('ב')
      .expect("expected hebrew letter in test string");
    let mut rtl_base = make_shaped_text_item(
      mixed,
      style.clone(),
      Direction::Rtl,
      None,
      &pipeline,
      &font_context,
    );
    pipeline.clear_cache();
    let mut ltr_base = make_shaped_text_item(
      mixed,
      style.clone(),
      Direction::Ltr,
      None,
      &pipeline,
      &font_context,
    );
    assert_ne!(
      glyph_signature_sequence(&rtl_base),
      glyph_signature_sequence(&ltr_base),
      "base direction should affect bidi levels/directions"
    );

    force_split_fallback(&mut rtl_base);
    let (before_rtl, after_rtl) = rtl_base
      .split_at(
        split_offset,
        false,
        &pipeline,
        &font_context,
        &mut reshape_cache,
      )
      .expect("split should succeed");
    assert_split_matches_original(&rtl_base, split_offset, &before_rtl, &after_rtl);

    pipeline.clear_cache();
    force_split_fallback(&mut ltr_base);
    let (before_ltr, after_ltr) = ltr_base
      .split_at(
        split_offset,
        false,
        &pipeline,
        &font_context,
        &mut reshape_cache,
      )
      .expect("split should succeed");
    assert_split_matches_original(&ltr_base, split_offset, &before_ltr, &after_ltr);
  }

  #[test]
  fn break_opportunities_stay_on_char_boundaries() {
    let text = "‘bruises and blood’ in ‘Christy’ fights";

    let style = Arc::new(ComputedStyle::default());
    let shaper = ShapingPipeline::new();
    let font_context = FontContext::new();
    let mut runs = shaper.shape(text, &style, &font_context).unwrap();
    TextItem::apply_spacing_to_runs(&mut runs, text, style.letter_spacing, style.word_spacing);
    let metrics = TextItem::metrics_from_runs(&runs, style.font_size, style.font_size);

    let item = TextItem::new(
      runs,
      text.to_string(),
      metrics,
      find_break_opportunities(text),
      Vec::new(),
      style,
      Direction::Ltr,
    );

    let mut cache = ReshapeCache::default();
    for brk in &item.break_opportunities {
      assert!(
        item.text.is_char_boundary(brk.byte_offset),
        "Break at {} is not a char boundary",
        brk.byte_offset
      );
      if brk.byte_offset == 0 || brk.byte_offset >= item.text.len() {
        continue;
      }

      assert!(
        item
          .split_at(brk.byte_offset, false, &shaper, &font_context, &mut cache)
          .is_some(),
        "Split failed at {}",
        brk.byte_offset
      );
    }
  }

  #[test]
  fn split_at_mid_codepoint_aligns_to_char_boundary() {
    let text = "a😊b";

    let style = Arc::new(ComputedStyle::default());
    let shaper = ShapingPipeline::new();
    let font_context = FontContext::new();
    let mut runs = shaper.shape(text, &style, &font_context).unwrap();
    TextItem::apply_spacing_to_runs(&mut runs, text, style.letter_spacing, style.word_spacing);
    let metrics = TextItem::metrics_from_runs(&runs, style.font_size, style.font_size);

    let item = TextItem::new(
      runs,
      text.to_string(),
      metrics,
      find_break_opportunities(text),
      Vec::new(),
      style,
      Direction::Ltr,
    );

    let mut cache = ReshapeCache::default();
    // Byte offset 2 is inside the emoji; split_at should clamp to the previous char boundary.
    let (before, after) = item
      .split_at(2, false, &shaper, &font_context, &mut cache)
      .expect("split_at should succeed even at mid-codepoint offsets");
    assert_eq!(before.text, "a");
    assert_eq!(after.text, "😊b");
  }

  #[test]
  fn split_at_is_safe_on_non_char_boundaries() {
    let item = make_text_item("â€œenhancedâ€", 20.0);
    let pipeline = ShapingPipeline::new();
    let font_ctx = FontContext::new();
    let mut cache = ReshapeCache::default();
    assert!(item
      .split_at(1, false, &pipeline, &font_ctx, &mut cache)
      .is_none());
  }

  #[test]
  fn break_point_selection_matches_linear_scan() {
    let widths = [0.0, 0.5, 1.0, 2.0, 3.5, 10.0];
    let mut cases: Vec<TextItem> = Vec::new();

    let mut allowed_only = make_text_item("aaaa", 4.0);
    allowed_only.break_opportunities = vec![
      BreakOpportunity::allowed(1),
      BreakOpportunity::allowed(2),
      BreakOpportunity::allowed(3),
    ];
    allowed_only.first_mandatory_break =
      TextItem::first_mandatory_break(&allowed_only.break_opportunities);
    cases.push(allowed_only);

    let mut mandatory_first = make_text_item("aaaa", 4.0);
    mandatory_first.break_opportunities =
      vec![BreakOpportunity::mandatory(2), BreakOpportunity::allowed(3)];
    mandatory_first.first_mandatory_break =
      TextItem::first_mandatory_break(&mandatory_first.break_opportunities);
    cases.push(mandatory_first);

    let mut break_word = make_text_item("abcdef", 6.0);
    break_word.break_opportunities.clear();
    Arc::make_mut(&mut break_word.style).word_break = WordBreak::BreakWord;
    break_word.first_mandatory_break = None;
    cases.push(break_word);

    let mut overflow_wrap = make_text_item("abcdef", 6.0);
    overflow_wrap.break_opportunities.clear();
    Arc::make_mut(&mut overflow_wrap.style).overflow_wrap = OverflowWrap::BreakWord;
    overflow_wrap.first_mandatory_break = None;
    cases.push(overflow_wrap);

    for (idx, item) in cases.into_iter().enumerate() {
      for width in &widths {
        let expected = old_find_break_point(&item, *width);
        let actual = item.find_break_point(*width);
        assert_eq!(
          actual, expected,
          "case {} text {} width {}",
          idx, item.text, width
        );
      }
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
      ReplacedType::Image {
        src: String::new(),
        alt: None,
        sizes: None,
        srcset: Vec::new(),
        picture_sources: Vec::new(),
      },
      Arc::new(ComputedStyle::default()),
      0.0,
      0.0,
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
    let inline_block = InlineBlockItem::new(
      fragment,
      Direction::Ltr,
      UnicodeBidi::Normal,
      0.0,
      0.0,
      true,
    );
    builder.add_item(InlineItem::InlineBlock(inline_block));

    let lines = builder.finish();
    assert_eq!(lines.len(), 1);
    assert_eq!(lines[0].items[0].item.width(), 80.0);
  }

  #[test]
  fn inline_block_baseline_prefers_last_line_box() {
    // Create an inline-block fragment that contains a line box at y=5 with baseline 8 (relative to the line box).
    let line = FragmentNode::new_line(Rect::from_xywh(0.0, 5.0, 60.0, 10.0), 8.0, vec![]);
    let fragment = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 80.0, 20.0), vec![line]);

    let inline_block = InlineBlockItem::new(
      fragment,
      Direction::Ltr,
      UnicodeBidi::Normal,
      0.0,
      0.0,
      true,
    );

    // Baseline should be derived from the line (5 + 8 = 13) rather than the bottom border edge (20).
    assert!((inline_block.metrics.baseline_offset - 13.0).abs() < 0.001);
    assert!((inline_block.metrics.descent - 7.0).abs() < 0.001);
  }

  #[test]
  fn inline_block_baseline_falls_back_when_overflow_clips() {
    // Even with a line box present, non-visible overflow forces the baseline to the bottom margin edge.
    let line = FragmentNode::new_line(Rect::from_xywh(0.0, 2.0, 40.0, 8.0), 6.0, vec![]);
    let fragment = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 50.0, 16.0), vec![line]);
    let inline_block = InlineBlockItem::new(
      fragment,
      Direction::Ltr,
      UnicodeBidi::Normal,
      0.0,
      0.0,
      false, // overflow != visible
    );

    assert!((inline_block.metrics.baseline_offset - 16.0).abs() < 0.001);
    assert!((inline_block.metrics.descent - 0.0).abs() < 0.001);
  }

  #[test]
  fn inline_block_baseline_falls_back_when_no_lines() {
    // Overflow visible but no in-flow line boxes: baseline should be the bottom edge.
    let fragment = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 30.0, 12.0), vec![]);
    let inline_block = InlineBlockItem::new(
      fragment,
      Direction::Ltr,
      UnicodeBidi::Normal,
      0.0,
      0.0,
      true,
    );

    assert!((inline_block.metrics.baseline_offset - 12.0).abs() < 0.001);
    assert!((inline_block.metrics.descent - 0.0).abs() < 0.001);
  }

  #[test]
  fn inline_table_baseline_uses_first_row_baseline() {
    // Simulate an inline-table fragment with two lines; baseline should come from the first line.
    let line1 = FragmentNode::new(
      Rect::from_xywh(0.0, 0.0, 20.0, 10.0),
      FragmentContent::Line { baseline: 4.0 },
      vec![],
    );
    let line2 = FragmentNode::new(
      Rect::from_xywh(0.0, 10.0, 20.0, 10.0),
      FragmentContent::Line { baseline: 6.0 },
      vec![],
    );
    let mut style = ComputedStyle::default();
    style.display = Display::InlineTable;
    let fragment = FragmentNode::new_with_style(
      Rect::from_xywh(0.0, 0.0, 20.0, 20.0),
      FragmentContent::Block { box_id: None },
      vec![line1, line2],
      Arc::new(style),
    );
    let inline_block = InlineBlockItem::new(
      fragment,
      Direction::Ltr,
      UnicodeBidi::Normal,
      0.0,
      0.0,
      true,
    );

    assert!((inline_block.metrics.baseline_offset - 4.0).abs() < 0.001);
    assert!((inline_block.metrics.descent - 16.0).abs() < 0.001);
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
  fn vertical_align_middle_uses_parent_strut_metrics() {
    let mut item = make_text_item("Test", 40.0).with_vertical_align(VerticalAlign::Middle);
    // Give the item predictable metrics to compare against the parent strut (which has x-height 6).
    item.metrics = BaselineMetrics::new(12.0, 16.0, 12.0, 4.0);

    let mut builder = make_builder(200.0);
    builder.add_item(InlineItem::Text(item));
    let lines = builder.finish();

    // With parent x-height 6, middle shift = 12 - 8 + 3 = 7.
    let first = &lines[0].items[0];
    assert!((first.baseline_offset - 7.0).abs() < 1e-3);
  }

  #[test]
  fn test_line_default() {
    let line = Line::default();
    assert!(line.is_empty());
    assert_eq!(line.width, 0.0);
  }

  #[test]
  fn bidi_runs_use_byte_indices_for_levels() {
    // Hebrew characters are multi-byte; the RTL byte length must not confuse run-level lookup.
    let mut builder = make_builder(200.0);

    builder.add_item(InlineItem::Text(make_text_item("א", 10.0)));
    builder.add_item(InlineItem::Text(make_text_item("a", 10.0)));
    builder.add_item(InlineItem::Text(make_text_item("b", 10.0)));

    let lines = builder.finish();
    assert_eq!(lines.len(), 1);
    let texts: Vec<String> = lines[0]
      .items
      .iter()
      .map(|p| match &p.item {
        InlineItem::Text(t) => t.text.clone(),
        _ => String::new(),
      })
      .collect();

    assert_eq!(
      texts,
      vec!["א".to_string(), "a".to_string(), "b".to_string()]
    );
  }

  #[test]
  fn bidi_mixed_direction_splits_text_item() {
    let mut builder = make_builder(200.0);

    builder.add_item(InlineItem::Text(make_text_item("abc אבג", 70.0)));

    let lines = builder.finish();
    assert_eq!(lines.len(), 1);
    let texts: Vec<String> = lines[0]
      .items
      .iter()
      .map(|p| match &p.item {
        InlineItem::Text(t) => t.text.clone(),
        _ => String::new(),
      })
      .collect();

    assert_eq!(texts, vec!["abc ".to_string(), "אבג".to_string()]);
  }

  #[test]
  fn bidi_plaintext_chooses_first_strong_base_direction() {
    let mut builder = make_builder_with_base(200.0, Level::rtl());
    builder.add_item(InlineItem::Text(make_text_item_with_bidi(
      "abc אבג",
      70.0,
      UnicodeBidi::Plaintext,
    )));

    let lines = builder.finish();
    assert_eq!(lines.len(), 1);
    let texts: Vec<String> = lines[0]
      .items
      .iter()
      .map(|p| match &p.item {
        InlineItem::Text(t) => t.text.clone(),
        _ => String::new(),
      })
      .collect();

    // Base was RTL, but plaintext forces first-strong (LTR here), so visual order stays logical LTR then RTL.
    assert_eq!(texts, vec!["abc ".to_string(), "אבג".to_string()]);
  }

  #[test]
  fn bidi_plaintext_inline_preserves_paragraph_base_direction() {
    let mut builder = make_builder_with_base(200.0, Level::ltr());
    builder.add_item(InlineItem::Text(make_text_item_with_bidi(
      "אבג",
      30.0,
      UnicodeBidi::Plaintext,
    )));
    builder.add_item(InlineItem::Text(make_text_item(" xyz", 30.0)));

    let lines = builder.finish();
    assert_eq!(lines.len(), 1);
    assert_eq!(lines[0].resolved_direction, Direction::Ltr);
    let texts: Vec<String> = lines[0]
      .items
      .iter()
      .map(|p| match &p.item {
        InlineItem::Text(t) => t.text.clone(),
        _ => String::new(),
      })
      .collect();

    assert_eq!(texts, vec!["אבג".to_string(), " xyz".to_string()]);
  }

  #[test]
  fn bidi_plaintext_sets_base_direction_on_items() {
    let mut items = vec![
      InlineItem::Text(make_text_item_with_bidi(
        "אבג",
        30.0,
        UnicodeBidi::Plaintext,
      )),
      InlineItem::Tab(TabItem::new(
        Arc::new(ComputedStyle::default()),
        BaselineMetrics::new(10.0, 12.0, 8.0, 2.0),
        8.0,
        true,
      )),
    ];

    crate::layout::contexts::inline::apply_plaintext_paragraph_direction(
      &mut items,
      Direction::Rtl,
    );

    let dir_text = match &items[0] {
      InlineItem::Text(t) => t.base_direction,
      _ => Direction::Ltr,
    };
    let dir_tab = match &items[1] {
      InlineItem::Tab(t) => t.direction,
      _ => Direction::Ltr,
    };

    assert_eq!(dir_text, Direction::Rtl);
    assert_eq!(dir_tab, Direction::Rtl);
  }

  #[test]
  fn bidi_isolate_inline_box_prevents_surrounding_reordering() {
    let mut builder = make_builder(200.0);

    builder.add_item(InlineItem::Text(make_text_item("ABC ", 40.0)));

    let mut inline_box = InlineBoxItem::new(
      0.0,
      0.0,
      0.0,
      make_strut_metrics(),
      Arc::new(ComputedStyle::default()),
      0,
      Direction::Rtl,
      UnicodeBidi::Isolate,
    );
    inline_box.add_child(InlineItem::Text(make_text_item("א", 10.0)));
    inline_box.add_child(InlineItem::Text(make_text_item("ב", 10.0)));
    inline_box.add_child(InlineItem::Text(make_text_item("ג", 10.0)));
    builder.add_item(InlineItem::InlineBox(inline_box));

    builder.add_item(InlineItem::Text(make_text_item(" DEF", 40.0)));

    let lines = builder.finish();
    assert_eq!(lines.len(), 1);
    let texts: Vec<String> = lines[0]
      .items
      .iter()
      .map(|p| match &p.item {
        InlineItem::Text(t) => t.text.clone(),
        InlineItem::InlineBox(b) => b
          .children
          .iter()
          .filter_map(|c| match c {
            InlineItem::Text(t) => Some(t.text.clone()),
            _ => None,
          })
          .collect::<String>(),
        _ => String::new(),
      })
      .collect();

    assert_eq!(
      texts,
      vec!["ABC ".to_string(), "גבא".to_string(), " DEF".to_string()]
    );
  }

  #[test]
  fn bidi_isolate_wraps_multiple_leaves_once() {
    let mut builder = make_builder(200.0);

    let mut inline_box = InlineBoxItem::new(
      0.0,
      0.0,
      0.0,
      make_strut_metrics(),
      Arc::new(ComputedStyle::default()),
      0,
      Direction::Rtl,
      UnicodeBidi::Isolate,
    );
    inline_box.add_child(InlineItem::Text(make_text_item("א", 10.0)));
    inline_box.add_child(InlineItem::Text(make_text_item("ב", 10.0)));
    inline_box.add_child(InlineItem::Text(make_text_item("ג", 10.0)));

    builder.add_item(InlineItem::Text(make_text_item("L", 10.0)));
    builder.add_item(InlineItem::InlineBox(inline_box));
    builder.add_item(InlineItem::Text(make_text_item(" R", 10.0)));

    let lines = builder.finish();
    assert_eq!(lines.len(), 1);
    let texts: Vec<String> = lines[0]
      .items
      .iter()
      .map(|p| match &p.item {
        InlineItem::Text(t) => t.text.clone(),
        InlineItem::InlineBox(b) => b
          .children
          .iter()
          .filter_map(|c| match c {
            InlineItem::Text(t) => Some(t.text.clone()),
            _ => None,
          })
          .collect::<String>(),
        _ => String::new(),
      })
      .collect();

    // Children stay adjacent and isolate prevents surrounding runs from interleaving; RTL order places the later
    // child earlier in visual order.
    assert_eq!(
      texts,
      vec!["L".to_string(), "גבא".to_string(), " R".to_string()]
    );
  }

  #[test]
  fn bidi_isolate_override_reverses_child_order() {
    let mut builder = make_builder(200.0);

    builder.add_item(InlineItem::Text(make_text_item("A ", 10.0)));

    let mut inline_box = InlineBoxItem::new(
      0.0,
      0.0,
      0.0,
      make_strut_metrics(),
      Arc::new(ComputedStyle::default()),
      0,
      Direction::Rtl,
      UnicodeBidi::IsolateOverride,
    );
    inline_box.add_child(InlineItem::Text(make_text_item("a", 10.0)));
    inline_box.add_child(InlineItem::Text(make_text_item("b", 10.0)));
    inline_box.add_child(InlineItem::Text(make_text_item("c", 10.0)));
    builder.add_item(InlineItem::InlineBox(inline_box));

    builder.add_item(InlineItem::Text(make_text_item(" C", 10.0)));

    let lines = builder.finish();
    assert_eq!(lines.len(), 1);
    let texts: Vec<String> = lines[0]
      .items
      .iter()
      .map(|p| match &p.item {
        InlineItem::Text(t) => t.text.clone(),
        InlineItem::InlineBox(b) => b
          .children
          .iter()
          .filter_map(|c| match c {
            InlineItem::Text(t) => Some(t.text.clone()),
            _ => None,
          })
          .collect::<String>(),
        _ => String::new(),
      })
      .collect();

    assert_eq!(
      texts,
      vec!["A ".to_string(), "cba".to_string(), " C".to_string()]
    );
  }

  #[test]
  fn explicit_bidi_context_resets_override_on_isolate() {
    let ctx = explicit_bidi_context(
      Direction::Ltr,
      &[
        (UnicodeBidi::BidiOverride, Direction::Rtl),
        (UnicodeBidi::Isolate, Direction::Ltr),
      ],
    )
    .expect("should compute explicit context");
    assert!(!ctx.override_all, "override should not leak past isolates");
    assert!(
      ctx.level.number() % 2 == 0,
      "isolate should push an even level for LTR"
    );
  }

  #[test]
  fn explicit_bidi_context_sets_override_for_isolate_override() {
    let ctx = explicit_bidi_context(
      Direction::Ltr,
      &[(UnicodeBidi::IsolateOverride, Direction::Ltr)],
    )
    .expect("should compute explicit context");
    assert!(
      ctx.override_all,
      "isolate-override should force overriding status"
    );
    assert!(ctx.level.number() % 2 == 0);
  }

  #[test]
  fn excessive_embedding_depth_is_clamped() {
    // Build a deeply nested set of inline boxes that exceed unicode_bidi's max depth.
    let mut inner = InlineItem::Text(make_text_item("abc", 30.0));
    for idx in 0..(level::MAX_EXPLICIT_DEPTH as usize + 8) {
      let mut box_item = InlineBoxItem::new(
        0.0,
        0.0,
        0.0,
        make_strut_metrics(),
        Arc::new(ComputedStyle::default()),
        idx,
        Direction::Ltr,
        UnicodeBidi::Embed,
      );
      box_item.add_child(inner);
      inner = InlineItem::InlineBox(box_item);
    }

    let positioned = PositionedItem {
      item: inner,
      x: 0.0,
      baseline_offset: 0.0,
    };
    let mut line = Line::new();
    line.items.push(positioned);
    let mut lines = vec![line];

    reorder_paragraph(
      &mut lines,
      Some(Level::ltr()),
      UnicodeBidi::Normal,
      Direction::Ltr,
      &ShapingPipeline::new(),
      &FontContext::new(),
    );

    assert!(
      !lines[0].items.is_empty(),
      "reordering should still produce items even when depth exceeds the limit"
    );
    let width: f32 = lines[0].items.iter().map(|p| p.item.width()).sum();
    assert!(
      width > 0.0,
      "items should keep their width after reordering"
    );
  }

  fn collect_text(item: &InlineItem, out: &mut String) {
    match item {
      InlineItem::Text(t) => out.push_str(&t.text),
      InlineItem::InlineBox(b) => {
        for child in &b.children {
          collect_text(child, out);
        }
      }
      _ => {}
    }
  }

  #[test]
  fn suppressed_controls_do_not_duplicate_text() {
    // When embeddings are suppressed (depth clamp), the logical text should remain intact.
    let mut inner = InlineItem::Text(make_text_item("abc", 30.0));
    for idx in 0..(level::MAX_EXPLICIT_DEPTH as usize + 8) {
      let mut box_item = InlineBoxItem::new(
        0.0,
        0.0,
        0.0,
        make_strut_metrics(),
        Arc::new(ComputedStyle::default()),
        idx + 1,
        Direction::Ltr,
        UnicodeBidi::Embed,
      );
      box_item.add_child(inner);
      inner = InlineItem::InlineBox(box_item);
    }

    let mut line = Line::new();
    line.items.push(PositionedItem {
      item: inner,
      x: 0.0,
      baseline_offset: 0.0,
    });
    let mut lines = vec![line];

    reorder_paragraph(
      &mut lines,
      Some(Level::ltr()),
      UnicodeBidi::Normal,
      Direction::Ltr,
      &ShapingPipeline::new(),
      &FontContext::new(),
    );

    let mut collected = String::new();
    for item in &lines[0].items {
      collect_text(&item.item, &mut collected);
    }
    assert_eq!(collected, "abc");
  }

  #[test]
  fn bidi_plaintext_on_inline_box_forces_first_strong() {
    let mut builder = make_builder_with_base(200.0, Level::rtl());

    let mut inline_box = InlineBoxItem::new(
      0.0,
      0.0,
      0.0,
      make_strut_metrics(),
      Arc::new(ComputedStyle::default()),
      0,
      Direction::Ltr,
      UnicodeBidi::Plaintext,
    );
    inline_box.add_child(InlineItem::Text(make_text_item("abc אבג", 70.0)));
    builder.add_item(InlineItem::InlineBox(inline_box));

    let lines = builder.finish();
    assert_eq!(lines.len(), 1);
    let texts: Vec<String> = lines[0]
      .items
      .iter()
      .flat_map(|p| match &p.item {
        InlineItem::InlineBox(b) => b
          .children
          .iter()
          .filter_map(|c| match c {
            InlineItem::Text(t) => Some(t.text.clone()),
            _ => None,
          })
          .collect::<Vec<_>>(),
        InlineItem::Text(t) => vec![t.text.clone()],
        _ => vec![],
      })
      .collect();

    assert_eq!(texts, vec!["abc ".to_string(), "אבג".to_string()]);
  }

  #[test]
  fn bidi_nested_isolates_close_properly() {
    let mut builder = make_builder(200.0);

    builder.add_item(InlineItem::Text(make_text_item("L", 10.0)));

    let mut inner = InlineBoxItem::new(
      0.0,
      0.0,
      0.0,
      make_strut_metrics(),
      Arc::new(ComputedStyle::default()),
      1,
      Direction::Ltr,
      UnicodeBidi::Isolate,
    );
    inner.add_child(InlineItem::Text(make_text_item("מ", 10.0)));

    let mut outer = InlineBoxItem::new(
      0.0,
      0.0,
      0.0,
      make_strut_metrics(),
      Arc::new(ComputedStyle::default()),
      0,
      Direction::Rtl,
      UnicodeBidi::Isolate,
    );
    outer.add_child(InlineItem::InlineBox(inner));
    outer.add_child(InlineItem::Text(make_text_item("א", 10.0)));

    builder.add_item(InlineItem::InlineBox(outer));
    builder.add_item(InlineItem::Text(make_text_item(" R", 10.0)));

    let lines = builder.finish();
    assert_eq!(lines.len(), 1);
    let texts: Vec<String> = lines[0]
      .items
      .iter()
      .map(|p| match &p.item {
        InlineItem::Text(t) => t.text.clone(),
        InlineItem::InlineBox(b) => b
          .children
          .iter()
          .filter_map(|c| match c {
            InlineItem::Text(t) => Some(t.text.clone()),
            InlineItem::InlineBox(inner) => Some(
              inner
                .children
                .iter()
                .filter_map(|c| match c {
                  InlineItem::Text(t) => Some(t.text.clone()),
                  _ => None,
                })
                .collect::<String>(),
            ),
            _ => None,
          })
          .collect::<String>(),
        _ => String::new(),
      })
      .collect();

    assert_eq!(
      texts,
      vec!["L".to_string(), "אמ".to_string(), " R".to_string()]
    );
  }

  #[test]
  fn bidi_plaintext_isolate_keeps_paragraph_base() {
    let mut builder = make_builder(200.0);

    builder.add_item(InlineItem::Text(make_text_item("A ", 10.0)));
    builder.add_item(InlineItem::Text(make_text_item_with_bidi(
      "אבג",
      15.0,
      UnicodeBidi::Plaintext,
    )));
    builder.add_item(InlineItem::Text(make_text_item(" C", 10.0)));

    let lines = builder.finish();
    assert_eq!(lines.len(), 1);
    let texts: Vec<String> = lines[0]
      .items
      .iter()
      .map(|p| match &p.item {
        InlineItem::Text(t) => t.text.clone(),
        _ => String::new(),
      })
      .collect();

    assert_eq!(
      texts,
      vec!["A ".to_string(), "אבג".to_string(), " C".to_string()]
    );
  }

  #[test]
  fn bidi_plaintext_uses_first_strong_rtl_when_text_starts_rtl() {
    let mut builder = make_builder_with_base(200.0, Level::ltr());
    builder.add_item(InlineItem::Text(make_text_item_with_bidi(
      "אבג abc",
      70.0,
      UnicodeBidi::Plaintext,
    )));

    let lines = builder.finish();
    assert_eq!(lines.len(), 1);
    let texts: Vec<String> = lines[0]
      .items
      .iter()
      .map(|p| match &p.item {
        InlineItem::Text(t) => t.text.clone(),
        _ => String::new(),
      })
      .collect();

    // Base came from first strong RTL; visual order (left-to-right positions) places the LTR run left.
    assert_eq!(texts, vec!["abc".to_string(), "אבג ".to_string()]);
  }

  #[test]
  fn bidi_plaintext_paragraph_base_only_when_present() {
    // First paragraph uses plaintext and should pick first-strong (LTR) despite RTL base.
    let mut builder = make_builder_with_base(200.0, Level::rtl());
    builder.add_item(InlineItem::Text(make_text_item_with_bidi(
      "abc אבג",
      70.0,
      UnicodeBidi::Plaintext,
    )));
    builder.force_break();

    // Second paragraph has no plaintext and keeps the RTL base.
    builder.add_item(InlineItem::Text(make_text_item("abc ", 30.0)));
    builder.add_item(InlineItem::Text(make_text_item("אבג", 30.0)));

    let lines = builder.finish();
    assert_eq!(lines.len(), 2);

    let para1: Vec<String> = lines[0]
      .items
      .iter()
      .map(|p| match &p.item {
        InlineItem::Text(t) => t.text.clone(),
        _ => String::new(),
      })
      .collect();
    assert_eq!(para1, vec!["abc ".to_string(), "אבג".to_string()]);

    let para2: Vec<String> = lines[1]
      .items
      .iter()
      .map(|p| match &p.item {
        InlineItem::Text(t) => t.text.clone(),
        _ => String::new(),
      })
      .collect();
    assert_eq!(
      para2,
      vec!["אבג".to_string(), " ".to_string(), "abc".to_string()]
    );
  }

  #[test]
  fn bidi_nested_isolate_override_reorders_only_inner_scope() {
    // Outer isolate keeps its children grouped; the inner isolate-override reverses its content.
    let mut builder = make_builder(200.0);

    builder.add_item(InlineItem::Text(make_text_item("L", 10.0)));

    let mut inner = InlineBoxItem::new(
      0.0,
      0.0,
      0.0,
      make_strut_metrics(),
      Arc::new(ComputedStyle::default()),
      1,
      Direction::Rtl,
      UnicodeBidi::IsolateOverride,
    );
    inner.add_child(InlineItem::Text(make_text_item("x", 10.0)));
    inner.add_child(InlineItem::Text(make_text_item("y", 10.0)));

    let mut outer = InlineBoxItem::new(
      0.0,
      0.0,
      0.0,
      make_strut_metrics(),
      Arc::new(ComputedStyle::default()),
      0,
      Direction::Rtl,
      UnicodeBidi::Isolate,
    );
    outer.add_child(InlineItem::Text(make_text_item("a", 10.0)));
    outer.add_child(InlineItem::InlineBox(inner));
    outer.add_child(InlineItem::Text(make_text_item("b", 10.0)));

    builder.add_item(InlineItem::InlineBox(outer));
    builder.add_item(InlineItem::Text(make_text_item(" R", 10.0)));

    let lines = builder.finish();
    assert_eq!(lines.len(), 1);

    let texts: Vec<String> = lines[0]
      .items
      .iter()
      .map(|p| match &p.item {
        InlineItem::Text(t) => t.text.clone(),
        InlineItem::InlineBox(b) => b
          .children
          .iter()
          .filter_map(|c| match c {
            InlineItem::Text(t) => Some(t.text.clone()),
            InlineItem::InlineBox(inner) => Some(
              inner
                .children
                .iter()
                .filter_map(|c| match c {
                  InlineItem::Text(t) => Some(t.text.clone()),
                  _ => None,
                })
                .collect::<String>(),
            ),
            _ => None,
          })
          .collect::<String>(),
        _ => String::new(),
      })
      .collect();

    let expected_inner = reorder_with_controls(
      &format!(
        "{}a{}{}xy{}{}b{}",
        '\u{2067}', // RLI (outer isolate)
        '\u{2067}', // RLI (inner isolate)
        '\u{202e}', // RLO
        '\u{202c}', // PDF
        '\u{2069}', // PDI (inner isolate)
        '\u{2069}'  // PDI (outer isolate)
      ),
      Some(Level::ltr()),
    );
    assert_eq!(
      texts,
      vec!["L".to_string(), expected_inner, " R".to_string()]
    );
  }

  #[test]
  fn bidi_isolate_override_keeps_inner_isolate_atomic() {
    // An isolate-override should reverse its own content while keeping nested isolates grouped.
    let mut builder = make_builder(200.0);

    builder.add_item(InlineItem::Text(make_text_item("L", 10.0)));

    let mut inner = InlineBoxItem::new(
      0.0,
      0.0,
      0.0,
      make_strut_metrics(),
      Arc::new(ComputedStyle::default()),
      1,
      Direction::Ltr,
      UnicodeBidi::Isolate,
    );
    inner.add_child(InlineItem::Text(make_text_item("BD", 20.0)));

    let mut outer = InlineBoxItem::new(
      0.0,
      0.0,
      0.0,
      make_strut_metrics(),
      Arc::new(ComputedStyle::default()),
      0,
      Direction::Rtl,
      UnicodeBidi::IsolateOverride,
    );
    outer.add_child(InlineItem::Text(make_text_item("A", 10.0)));
    outer.add_child(InlineItem::InlineBox(inner));
    outer.add_child(InlineItem::Text(make_text_item("C", 10.0)));

    builder.add_item(InlineItem::InlineBox(outer));
    builder.add_item(InlineItem::Text(make_text_item(" R", 10.0)));

    let lines = builder.finish();
    assert_eq!(lines.len(), 1);

    let actual: String = lines[0]
      .items
      .iter()
      .map(|p| flatten_text(&p.item))
      .collect();

    let logical = format!(
      "L{}{}A{}BD{}C{}{} R",
      '\u{2067}', // RLI (rtl isolate)
      '\u{202e}', // RLO (rtl override)
      '\u{2066}', // LRI (ltr isolate)
      '\u{2069}', // PDI
      '\u{202c}', // PDF
      '\u{2069}', // PDI
    );
    let expected = reorder_with_controls(&logical, Some(Level::ltr()));
    assert_eq!(actual, expected);
  }

  #[test]
  fn bidi_override_does_not_apply_inside_isolate() {
    let mut builder = make_builder(200.0);

    let mut inner = InlineBoxItem::new(
      0.0,
      0.0,
      0.0,
      make_strut_metrics(),
      Arc::new(ComputedStyle::default()),
      1,
      Direction::Ltr,
      UnicodeBidi::Isolate,
    );
    inner.add_child(InlineItem::Text(make_text_item("א", 10.0)));
    inner.add_child(InlineItem::Text(make_text_item("ב", 10.0)));

    let mut outer = InlineBoxItem::new(
      0.0,
      0.0,
      0.0,
      make_strut_metrics(),
      Arc::new(ComputedStyle::default()),
      0,
      Direction::Rtl,
      UnicodeBidi::BidiOverride,
    );
    outer.add_child(InlineItem::Text(make_text_item("a", 10.0)));
    outer.add_child(InlineItem::InlineBox(inner));
    outer.add_child(InlineItem::Text(make_text_item("b", 10.0)));

    builder.add_item(InlineItem::InlineBox(outer));

    let lines = builder.finish();
    assert_eq!(lines.len(), 1);
    let mut visual = String::new();
    for positioned in &lines[0].items {
      collect_text(&positioned.item, &mut visual);
    }

    let logical = "\u{202E}a\u{2066}אב\u{2069}b\u{202C}";
    let info = BidiInfo::new(logical, Some(Level::ltr()));
    let para = &info.paragraphs[0];
    let expected: String = info
      .reorder_line(para, para.range.clone())
      .chars()
      .filter(|c| !is_bidi_control(*c))
      .collect();

    assert_eq!(visual, expected);
  }

  #[test]
  fn bidi_override_parent_preserves_isolate_contents() {
    let mut builder = make_builder(200.0);

    let mut inner = InlineBoxItem::new(
      0.0,
      0.0,
      0.0,
      make_strut_metrics(),
      Arc::new(ComputedStyle::default()),
      1,
      Direction::Ltr,
      UnicodeBidi::Isolate,
    );
    inner.add_child(InlineItem::Text(make_text_item("א", 10.0)));
    inner.add_child(InlineItem::Text(make_text_item("ב", 10.0)));

    let mut outer = InlineBoxItem::new(
      0.0,
      0.0,
      0.0,
      make_strut_metrics(),
      Arc::new(ComputedStyle::default()),
      0,
      Direction::Rtl,
      UnicodeBidi::BidiOverride,
    );
    outer.add_child(InlineItem::Text(make_text_item("x", 10.0)));
    outer.add_child(InlineItem::InlineBox(inner));
    outer.add_child(InlineItem::Text(make_text_item("y", 10.0)));

    builder.add_item(InlineItem::InlineBox(outer));

    let lines = builder.finish();
    assert_eq!(lines.len(), 1);
    let mut visual = String::new();
    for positioned in &lines[0].items {
      collect_text(&positioned.item, &mut visual);
    }

    let logical = "\u{202E}x\u{2066}אב\u{2069}y\u{202C}";
    let info = BidiInfo::new(logical, Some(Level::ltr()));
    let para = &info.paragraphs[0];
    let expected: String = info
      .reorder_line(para, para.range.clone())
      .chars()
      .filter(|c| !is_bidi_control(*c))
      .collect();

    assert_eq!(visual, expected);
  }

  #[test]
  fn bidi_override_allows_embed_to_reset_override() {
    // Outer override should force RTL ordering for its direct children, but an inner
    // unicode-bidi: embed establishes a fresh embedding level without the override so its
    // content keeps logical order.
    let mut builder = make_builder(200.0);

    builder.add_item(InlineItem::Text(make_text_item("L ", 10.0)));

    let mut inner = InlineBoxItem::new(
      0.0,
      0.0,
      0.0,
      make_strut_metrics(),
      Arc::new(ComputedStyle::default()),
      1,
      Direction::Ltr,
      UnicodeBidi::Embed,
    );
    inner.add_child(InlineItem::Text(make_text_item("a", 10.0)));
    inner.add_child(InlineItem::Text(make_text_item("b", 10.0)));
    inner.add_child(InlineItem::Text(make_text_item("c", 10.0)));

    let mut outer = InlineBoxItem::new(
      0.0,
      0.0,
      0.0,
      make_strut_metrics(),
      Arc::new(ComputedStyle::default()),
      0,
      Direction::Rtl,
      UnicodeBidi::BidiOverride,
    );
    outer.add_child(InlineItem::Text(make_text_item("X", 10.0)));
    outer.add_child(InlineItem::InlineBox(inner));
    outer.add_child(InlineItem::Text(make_text_item("Y", 10.0)));

    builder.add_item(InlineItem::InlineBox(outer));
    builder.add_item(InlineItem::Text(make_text_item("R", 10.0)));

    let lines = builder.finish();
    assert_eq!(lines.len(), 1);

    let actual: String = lines[0]
      .items
      .iter()
      .map(|p| flatten_text(&p.item))
      .collect();

    let logical = format!(
      "L {}X{}abc{}Y{}R",
      '\u{202e}', // RLO
      '\u{202a}', // LRE
      '\u{202c}', // PDF
      '\u{202c}'  // PDF
    );
    let expected = reorder_with_controls(&logical, Some(Level::ltr()));
    assert_eq!(actual, expected);
  }

  #[test]
  fn bidi_override_does_not_cross_paragraph_boundary() {
    // An override in the first paragraph should not affect the following paragraph; embeds
    // in later paragraphs should resolve independently.
    let mut builder = make_builder(200.0);

    let mut para1 = InlineBoxItem::new(
      0.0,
      0.0,
      0.0,
      make_strut_metrics(),
      Arc::new(ComputedStyle::default()),
      0,
      Direction::Rtl,
      UnicodeBidi::BidiOverride,
    );
    para1.add_child(InlineItem::Text(make_text_item("A", 10.0)));
    para1.add_child(InlineItem::Text(make_text_item("B", 10.0)));
    para1.add_child(InlineItem::Text(make_text_item("C", 10.0)));
    builder.add_item(InlineItem::InlineBox(para1));
    builder.force_break();

    let mut para2 = InlineBoxItem::new(
      0.0,
      0.0,
      0.0,
      make_strut_metrics(),
      Arc::new(ComputedStyle::default()),
      1,
      Direction::Ltr,
      UnicodeBidi::Embed,
    );
    para2.add_child(InlineItem::Text(make_text_item("XYZ", 30.0)));
    builder.add_item(InlineItem::InlineBox(para2));

    let lines = builder.finish();
    assert_eq!(lines.len(), 2);

    let actual_para1: String = lines[0]
      .items
      .iter()
      .map(|p| flatten_text(&p.item))
      .collect();
    let expected_para1 = reorder_with_controls(
      &format!("{}ABC{}", '\u{202e}', '\u{202c}'),
      Some(Level::ltr()),
    );
    assert_eq!(actual_para1, expected_para1);

    let actual_para2: String = lines[1]
      .items
      .iter()
      .map(|p| flatten_text(&p.item))
      .collect();
    let expected_para2 = reorder_with_controls(
      &format!("{}XYZ{}", '\u{202a}', '\u{202c}'),
      Some(Level::ltr()),
    );
    assert_eq!(actual_para2, expected_para2);
  }

  #[test]
  fn bidi_override_stops_at_forced_break() {
    // An explicit override without a terminator should not leak across a hard break.
    let mut builder = make_builder(200.0);

    // First paragraph uses an override to reverse ABC.
    let mut para1 = InlineBoxItem::new(
      0.0,
      0.0,
      0.0,
      make_strut_metrics(),
      Arc::new(ComputedStyle::default()),
      0,
      Direction::Rtl,
      UnicodeBidi::BidiOverride,
    );
    para1.add_child(InlineItem::Text(make_text_item("A", 10.0)));
    para1.add_child(InlineItem::Text(make_text_item("B", 10.0)));
    para1.add_child(InlineItem::Text(make_text_item("C", 10.0)));
    builder.add_item(InlineItem::InlineBox(para1));
    builder.force_break();

    // Second paragraph is plain LTR.
    builder.add_item(InlineItem::Text(make_text_item("DEF", 30.0)));

    let lines = builder.finish();
    assert_eq!(lines.len(), 2);

    let para1_text: String = lines[0]
      .items
      .iter()
      .map(|p| flatten_text(&p.item))
      .collect();
    let expected_para1 = reorder_with_controls(
      &format!("{}ABC{}", '\u{202e}', '\u{202c}'),
      Some(Level::ltr()),
    );
    assert_eq!(para1_text, expected_para1);

    let para2_text: String = lines[1]
      .items
      .iter()
      .map(|p| flatten_text(&p.item))
      .collect();
    assert_eq!(para2_text, "DEF".to_string());
  }

  #[test]
  fn bidi_override_and_embed_across_paragraphs() {
    // Paragraph 1 uses an override; paragraph 2 uses an embed. Each should reorder independently.
    let mut builder = make_builder(200.0);

    let mut para1 = InlineBoxItem::new(
      0.0,
      0.0,
      0.0,
      make_strut_metrics(),
      Arc::new(ComputedStyle::default()),
      0,
      Direction::Rtl,
      UnicodeBidi::BidiOverride,
    );
    para1.add_child(InlineItem::Text(make_text_item("A", 10.0)));
    para1.add_child(InlineItem::Text(make_text_item("B", 10.0)));
    para1.add_child(InlineItem::Text(make_text_item("C", 10.0)));
    builder.add_item(InlineItem::InlineBox(para1));
    builder.force_break();

    let mut para2 = InlineBoxItem::new(
      0.0,
      0.0,
      0.0,
      make_strut_metrics(),
      Arc::new(ComputedStyle::default()),
      1,
      Direction::Rtl,
      UnicodeBidi::Embed,
    );
    para2.add_child(InlineItem::Text(make_text_item("XYZ", 30.0)));
    builder.add_item(InlineItem::InlineBox(para2));

    let lines = builder.finish();
    assert_eq!(lines.len(), 2);

    let para1_text: String = lines[0]
      .items
      .iter()
      .map(|p| flatten_text(&p.item))
      .collect();
    let expected_para1 = reorder_with_controls(
      &format!("{}ABC{}", '\u{202e}', '\u{202c}'),
      Some(Level::ltr()),
    );
    assert_eq!(para1_text, expected_para1);

    let para2_text: String = lines[1]
      .items
      .iter()
      .map(|p| flatten_text(&p.item))
      .collect();
    let expected_para2 = reorder_with_controls(
      &format!("{}XYZ{}", '\u{202b}', '\u{202c}'),
      Some(Level::ltr()),
    );
    assert_eq!(para2_text, expected_para2);
  }

  #[test]
  fn bidi_override_mixed_controls_across_paragraphs() {
    // Mixed override/embedding should not leak past a forced break; each paragraph reorders per its controls.
    let mut builder = make_builder(200.0);

    // First paragraph: RLO forces RTL, then an embedded LTR segment.
    let mut para1 = InlineBoxItem::new(
      0.0,
      0.0,
      0.0,
      make_strut_metrics(),
      Arc::new(ComputedStyle::default()),
      0,
      Direction::Rtl,
      UnicodeBidi::BidiOverride,
    );
    para1.add_child(InlineItem::Text(make_text_item("A", 10.0)));
    para1.add_child(InlineItem::Text(make_text_item("B", 10.0)));
    para1.add_child(InlineItem::Text(make_text_item_with_bidi(
      "cd",
      20.0,
      UnicodeBidi::Embed,
    )));
    builder.add_item(InlineItem::InlineBox(para1));
    builder.force_break();

    // Second paragraph: plain LTR embed.
    let mut para2 = InlineBoxItem::new(
      0.0,
      0.0,
      0.0,
      make_strut_metrics(),
      Arc::new(ComputedStyle::default()),
      1,
      Direction::Ltr,
      UnicodeBidi::Embed,
    );
    para2.add_child(InlineItem::Text(make_text_item("EF", 20.0)));
    builder.add_item(InlineItem::InlineBox(para2));

    let lines = builder.finish();
    assert_eq!(lines.len(), 2);

    let para1_text: String = lines[0]
      .items
      .iter()
      .map(|p| flatten_text(&p.item))
      .collect();
    let expected_para1 = reorder_with_controls(
      &format!(
        "{}AB{}{}cd{}",
        '\u{202e}', '\u{202c}', '\u{202b}', '\u{202c}'
      ),
      Some(Level::ltr()),
    );
    assert_eq!(para1_text, expected_para1);

    let para2_text: String = lines[1]
      .items
      .iter()
      .map(|p| flatten_text(&p.item))
      .collect();
    let expected_para2 = reorder_with_controls(
      &format!("{}EF{}", '\u{202a}', '\u{202c}'),
      Some(Level::ltr()),
    );
    assert_eq!(para2_text, expected_para2);
  }

  #[test]
  fn bidi_isolate_does_not_affect_following_paragraph() {
    // An isolate in the first paragraph should not alter the base direction of the next paragraph.
    let mut builder = make_builder(200.0);

    let mut para1 = InlineBoxItem::new(
      0.0,
      0.0,
      0.0,
      make_strut_metrics(),
      Arc::new(ComputedStyle::default()),
      0,
      Direction::Rtl,
      UnicodeBidi::Isolate,
    );
    para1.add_child(InlineItem::Text(make_text_item("A", 10.0)));
    para1.add_child(InlineItem::Text(make_text_item("B", 10.0)));
    para1.add_child(InlineItem::Text(make_text_item("C", 10.0)));
    builder.add_item(InlineItem::InlineBox(para1));
    builder.force_break();

    builder.add_item(InlineItem::Text(make_text_item("DEF", 30.0)));

    let lines = builder.finish();
    assert_eq!(lines.len(), 2);

    let para1_text: String = lines[0]
      .items
      .iter()
      .map(|p| flatten_text(&p.item))
      .collect();
    let expected_para1 = reorder_with_controls(
      &format!("{}ABC{}", '\u{2067}', '\u{2069}'),
      Some(Level::ltr()),
    );
    assert_eq!(para1_text, expected_para1);

    let para2_text: String = lines[1]
      .items
      .iter()
      .map(|p| flatten_text(&p.item))
      .collect();
    assert_eq!(para2_text, "DEF".to_string());
  }

  fn nested_inline_box_with_depth(
    depth: usize,
    ub: UnicodeBidi,
    direction: Direction,
    child: InlineItem,
  ) -> InlineItem {
    let mut current = child;
    for idx in 0..depth {
      let mut inline_box = InlineBoxItem::new(
        0.0,
        0.0,
        0.0,
        make_strut_metrics(),
        Arc::new(ComputedStyle::default()),
        idx,
        direction,
        ub,
      );
      inline_box.add_child(current);
      current = InlineItem::InlineBox(inline_box);
    }
    current
  }

  #[test]
  fn bidi_contexts_beyond_max_depth_are_ignored() {
    // Build a chain of isolate boxes deeper than MAX_EXPLICIT_DEPTH and ensure we still reorder safely.
    let mut builder = make_builder_with_base(200.0, Level::ltr());
    let deep = nested_inline_box_with_depth(
      unicode_bidi::level::MAX_EXPLICIT_DEPTH as usize + 5,
      UnicodeBidi::Isolate,
      Direction::Rtl,
      InlineItem::Text(make_text_item("abc", 30.0)),
    );
    builder.add_item(deep);

    let lines = builder.finish();
    assert_eq!(lines.len(), 1);
    let mut collected = String::new();
    for positioned in &lines[0].items {
      collect_text(&positioned.item, &mut collected);
    }
    assert_eq!(collected, "abc");
  }

  fn reorder_with_controls(text: &str, base: Option<Level>) -> String {
    let bidi = BidiInfo::new(text, base);
    let para = &bidi.paragraphs[0];
    bidi
      .reorder_line(para, para.range.clone())
      .chars()
      .filter(|c| {
        !matches!(
          c,
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
      })
      .collect()
  }

  fn flatten_text(item: &InlineItem) -> String {
    match item {
      InlineItem::Text(t) => t.text.clone(),
      InlineItem::Tab(_) => "\t".to_string(),
      InlineItem::InlineBox(b) => b.children.iter().map(flatten_text).collect(),
      InlineItem::Ruby(r) => r
        .segments
        .iter()
        .map(|seg| seg.base_items.iter().map(flatten_text).collect::<String>())
        .collect(),
      InlineItem::InlineBlock(_)
      | InlineItem::Replaced(_)
      | InlineItem::Floating(_)
      | InlineItem::StaticPositionAnchor(_) => String::from("\u{FFFC}"),
    }
  }

  fn is_bidi_control(c: char) -> bool {
    matches!(
      c,
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

  #[test]
  fn bidi_isolate_spans_children_as_single_context() {
    // Logical text with a single RTL isolate containing both child segments.
    let expected = reorder_with_controls("A \u{2067}XY\u{2069} Z", Some(Level::ltr()));

    let mut builder = make_builder(200.0);
    builder.add_item(InlineItem::Text(make_text_item("A ", 20.0)));

    let mut inline_box = InlineBoxItem::new(
      0.0,
      0.0,
      0.0,
      make_strut_metrics(),
      Arc::new(ComputedStyle::default()),
      0,
      Direction::Rtl,
      UnicodeBidi::Isolate,
    );
    inline_box.add_child(InlineItem::Text(make_text_item("X", 10.0)));
    inline_box.add_child(InlineItem::Text(make_text_item("Y", 10.0)));
    builder.add_item(InlineItem::InlineBox(inline_box));

    builder.add_item(InlineItem::Text(make_text_item(" Z", 20.0)));

    let lines = builder.finish();
    assert_eq!(lines.len(), 1);
    let actual: String = lines[0]
      .items
      .iter()
      .map(|p| flatten_text(&p.item))
      .collect();

    assert_eq!(actual, expected);
  }

  #[test]
  fn bidi_isolate_resolves_neutrals_across_children() {
    // Single isolate should resolve neutrals using surrounding strong types inside the isolate.
    let expected = "A באX Z".to_string();

    let mut builder = make_builder(200.0);
    builder.add_item(InlineItem::Text(make_text_item("A ", 20.0)));

    let mut inline_box = InlineBoxItem::new(
      0.0,
      0.0,
      0.0,
      make_strut_metrics(),
      Arc::new(ComputedStyle::default()),
      0,
      Direction::Rtl,
      UnicodeBidi::Isolate,
    );
    inline_box.add_child(InlineItem::Text(make_text_item("X", 10.0)));
    inline_box.add_child(InlineItem::Text(make_text_item("א", 10.0)));
    inline_box.add_child(InlineItem::Text(make_text_item("ב", 10.0)));
    builder.add_item(InlineItem::InlineBox(inline_box));

    builder.add_item(InlineItem::Text(make_text_item(" Z", 20.0)));

    let lines = builder.finish();
    assert_eq!(lines.len(), 1);
    let actual: String = lines[0]
      .items
      .iter()
      .map(|p| flatten_text(&p.item))
      .collect();

    assert_eq!(actual, expected);
  }

  #[test]
  fn bidi_explicit_embedding_survives_line_wrap() {
    // Explicit embedding (RLE) without a terminator should keep later lines at the same level.
    let mut builder = make_builder(40.0);
    let first = "\u{202b}abc ";
    builder.add_item(InlineItem::Text(make_text_item(first, 40.0)));
    builder.add_item(InlineItem::Text(make_text_item("DEF", 30.0)));

    let lines = builder.finish();
    assert_eq!(lines.len(), 2);

    let logical = format!("{}{}", first, "DEF");
    let bidi = BidiInfo::new(&logical, Some(Level::ltr()));
    let para = &bidi.paragraphs[0];
    let line_ranges = [0..first.len(), first.len()..logical.len()];
    let expected: Vec<String> = line_ranges
      .iter()
      .map(|range| {
        bidi
          .reorder_line(para, range.clone())
          .chars()
          .filter(|c| !is_bidi_control(*c))
          .collect()
      })
      .collect();

    let actual: Vec<String> = lines
      .iter()
      .map(|line| {
        line
          .items
          .iter()
          .map(|p| flatten_text(&p.item))
          .collect::<String>()
          .chars()
          .filter(|c| !is_bidi_control(*c))
          .collect()
      })
      .collect();

    assert_eq!(actual, expected);
  }

  #[test]
  fn letter_spacing_increases_advance() {
    let mut style = ComputedStyle::default();
    let text = "abc";
    let font_ctx = FontContext::new();
    let pipeline = ShapingPipeline::new();

    let base_runs = pipeline
      .shape_with_direction(
        text,
        &style,
        &font_ctx,
        pipeline_dir_from_style(Direction::Ltr),
      )
      .expect("shape");
    let base_width: f32 = base_runs.iter().map(|r| r.advance).sum();

    style.letter_spacing = 2.0;
    let mut spaced_runs = pipeline
      .shape_with_direction(
        text,
        &style,
        &font_ctx,
        pipeline_dir_from_style(Direction::Ltr),
      )
      .expect("shape");
    TextItem::apply_spacing_to_runs(
      &mut spaced_runs,
      text,
      style.letter_spacing,
      style.word_spacing,
    );
    let spaced_width: f32 = spaced_runs.iter().map(|r| r.advance).sum();

    let expected_extra = style.letter_spacing * (text.chars().count().saturating_sub(1) as f32);
    assert!((spaced_width - base_width - expected_extra).abs() < 0.01);
  }

  #[test]
  fn word_spacing_applies_to_spaces() {
    let mut style = ComputedStyle::default();
    style.letter_spacing = 1.0;
    style.word_spacing = 3.0;
    let text = "a b";
    let font_ctx = FontContext::new();
    let pipeline = ShapingPipeline::new();

    let base_runs = pipeline
      .shape_with_direction(
        text,
        &ComputedStyle::default(),
        &font_ctx,
        pipeline_dir_from_style(Direction::Ltr),
      )
      .expect("shape");
    let base_width: f32 = base_runs.iter().map(|r| r.advance).sum();

    let mut spaced_runs = pipeline
      .shape_with_direction(
        text,
        &style,
        &font_ctx,
        pipeline_dir_from_style(Direction::Ltr),
      )
      .expect("shape");
    TextItem::apply_spacing_to_runs(
      &mut spaced_runs,
      text,
      style.letter_spacing,
      style.word_spacing,
    );
    let spaced_width: f32 = spaced_runs.iter().map(|r| r.advance).sum();

    let char_gaps = text.chars().count().saturating_sub(1) as f32;
    let space_count = text
      .chars()
      .filter(|c| matches!(c, ' ' | '\u{00A0}' | '\t'))
      .count() as f32;
    let expected_extra = style.letter_spacing * char_gaps + style.word_spacing * space_count;

    assert!((spaced_width - base_width - expected_extra).abs() < 0.01);
  }

  #[test]
  fn split_at_can_insert_hyphen() {
    let font_ctx = FontContext::new();
    let pipeline = ShapingPipeline::new();
    let style = Arc::new(ComputedStyle::default());

    let runs = pipeline
      .shape_with_direction(
        "abc",
        &style,
        &font_ctx,
        pipeline_dir_from_style(Direction::Ltr),
      )
      .expect("shape");
    let metrics = TextItem::metrics_from_runs(&runs, 16.0, style.font_size);
    let breaks = vec![BreakOpportunity::with_hyphen(1, BreakType::Allowed, true)];
    let item = TextItem::new(
      runs,
      "abc".to_string(),
      metrics,
      breaks,
      Vec::new(),
      style,
      Direction::Ltr,
    );

    let mut cache = ReshapeCache::default();
    let (before, after) = item
      .split_at(1, true, &pipeline, &font_ctx, &mut cache)
      .expect("split succeeds");

    assert_eq!(before.text, format!("a{}", '\u{2010}'));
    assert_eq!(after.text, "bc");
    let combined = before.text.replace('\u{2010}', "") + &after.text;
    assert_eq!(combined, "abc");
  }

  #[test]
  fn split_at_round_trips_text() {
    let font_ctx = FontContext::new();
    let pipeline = ShapingPipeline::new();
    let style = Arc::new(ComputedStyle::default());
    let text = "wrap this 😊 text";

    let runs = pipeline
      .shape_with_direction(
        text,
        &style,
        &font_ctx,
        pipeline_dir_from_style(Direction::Ltr),
      )
      .expect("shape");
    let metrics = TextItem::metrics_from_runs(&runs, 16.0, style.font_size);
    let item = TextItem::new(
      runs,
      text.to_string(),
      metrics,
      find_break_opportunities(text),
      Vec::new(),
      style,
      Direction::Ltr,
    );

    let split_offset = text.find('😊').unwrap();
    let mut cache = ReshapeCache::default();
    let (before, after) = item
      .split_at(split_offset, false, &pipeline, &font_ctx, &mut cache)
      .expect("split succeeds");

    assert_eq!(before.text + &after.text, text);
  }

  #[test]
  fn split_at_rejects_non_char_boundary_offsets() {
    let font_ctx = FontContext::new();
    let pipeline = ShapingPipeline::new();
    let style = Arc::new(ComputedStyle::default());

    let text = "a😊b";
    let runs = pipeline
      .shape_with_direction(
        text,
        &style,
        &font_ctx,
        pipeline_dir_from_style(Direction::Ltr),
      )
      .expect("shape");
    let metrics = TextItem::metrics_from_runs(&runs, 16.0, style.font_size);
    let item = TextItem::new(
      runs,
      text.to_string(),
      metrics,
      Vec::new(),
      Vec::new(),
      style.clone(),
      Direction::Ltr,
    );

    // Offset 2 lands inside the multi-byte emoji; split_at should clamp to the previous
    // boundary rather than panicking.
    assert!(!text.is_char_boundary(2));
    let mut cache = ReshapeCache::default();
    let (before, after) = item
      .split_at(2, false, &pipeline, &font_ctx, &mut cache)
      .expect("clamps to prior boundary");
    assert_eq!(before.text, "a");
    assert_eq!(after.text, "😊b");

    // Explicit break offsets should also be rejected when they land mid-codepoint.
    let euro_text = "€uro";
    let euro_runs = pipeline
      .shape_with_direction(
        euro_text,
        &style,
        &font_ctx,
        pipeline_dir_from_style(Direction::Ltr),
      )
      .expect("shape");
    let euro_metrics = TextItem::metrics_from_runs(&euro_runs, 16.0, style.font_size);
    let breaks = vec![BreakOpportunity::new(1, BreakType::Allowed)];
    let euro_item = TextItem::new(
      euro_runs,
      euro_text.to_string(),
      euro_metrics,
      breaks,
      Vec::new(),
      style,
      Direction::Ltr,
    );

    let mut euro_cache = ReshapeCache::default();
    assert!(euro_item
      .split_at(1, false, &pipeline, &font_ctx, &mut euro_cache)
      .is_none());
  }

  #[test]
  fn previous_char_boundary_handles_multibyte_offsets() {
    let text = "a😊b";
    // Offsets that land in the middle of a multibyte codepoint should step back to its start.
    assert_eq!(TextItem::previous_char_boundary_in_text(text, 2), 1);
    assert_eq!(TextItem::previous_char_boundary_in_text(text, 4), 1);

    // Offsets at boundaries remain unchanged.
    assert_eq!(TextItem::previous_char_boundary_in_text(text, 0), 0);
    assert_eq!(TextItem::previous_char_boundary_in_text(text, 1), 1);
    assert_eq!(TextItem::previous_char_boundary_in_text(text, 5), 5);
  }

  #[test]
  fn previous_char_boundary_clamps_past_end() {
    let text = "éclair";
    assert_eq!(
      TextItem::previous_char_boundary_in_text(text, text.len() + 10),
      text.len()
    );
  }

  #[test]
  fn split_at_handles_non_char_boundary_offsets() {
    let font_ctx = FontContext::new();
    let pipeline = ShapingPipeline::new();
    let style = Arc::new(ComputedStyle::default());
    let text = "‘bruises and blood’ in ‘Christy’ fights";

    let runs = pipeline
      .shape_with_direction(
        text,
        &style,
        &font_ctx,
        pipeline_dir_from_style(Direction::Ltr),
      )
      .expect("shape");
    let metrics = TextItem::metrics_from_runs(&runs, 16.0, style.font_size);
    let item = TextItem::new(
      runs,
      text.to_string(),
      metrics,
      Vec::new(),
      Vec::new(),
      style,
      Direction::Ltr,
    );

    let mut cache = ReshapeCache::default();
    let (before, after) = item
      .split_at(22, false, &pipeline, &font_ctx, &mut cache)
      .expect("split succeeds");

    assert_eq!(format!("{}{}", before.text, after.text), text);
    assert!(after.text.starts_with('’'));
  }
}
