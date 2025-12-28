//! Pagination helpers that honor CSS @page rules and margin boxes.

use std::cmp::Ordering;
use std::collections::HashMap;
use std::sync::Arc;

use crate::css::types::{CollectedPageRule, PageMarginArea};
use crate::geometry::{Point, Rect, Size};
use crate::layout::axis::{FragmentAxes, PhysicalAxis};
use crate::layout::engine::{LayoutConfig, LayoutEngine};
use crate::layout::formatting_context::{
  layout_style_fingerprint, set_fragmentainer_block_size_hint, LayoutError,
};
use crate::layout::fragmentation::{
  clip_node_with_axes, collect_atomic_ranges_with_axes, collect_forced_boundaries_with_axes,
  normalize_atomic_ranges, normalize_fragment_margins_with_axes, propagate_fragment_metadata,
  resolve_fragmentation_boundaries_with_axes, AtomicRange, ForcedBoundary, FragmentationContext,
};
use crate::layout::running_strings::{collect_string_set_events, StringSetEvent};
use crate::style::content::{
  ContentContext, ContentItem, ContentValue, CounterStyle, RunningElementSelect,
  RunningStringValues,
};
use crate::style::display::{Display, FormattingContextType};
use crate::style::page::{resolve_page_style, PageSide, ResolvedPageStyle};
use crate::style::position::Position;
use crate::style::types::WritingMode;
use crate::style::{block_axis_is_horizontal, ComputedStyle};
use crate::text::font_loader::FontContext;
use crate::tree::box_tree::{BoxNode, BoxTree, ReplacedType};
use crate::tree::fragment_tree::{FragmentContent, FragmentNode};

/// Controls how paginated pages are positioned in the fragment tree.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PageStacking {
  /// Translate each page along the block axis so they don't overlap.
  ///
  /// The provided gap is inserted between successive pages (clamped to >= 0).
  Stacked { gap: f32 },
  /// Leave all pages at the origin so they can be painted independently.
  Untranslated,
}

/// Options for pagination.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct PaginateOptions {
  pub stacking: PageStacking,
}

impl Default for PaginateOptions {
  fn default() -> Self {
    Self {
      stacking: PageStacking::Stacked { gap: 0.0 },
    }
  }
}

const EPSILON: f32 = 0.01;

fn page_block_size(style: &ResolvedPageStyle, axes: FragmentAxes) -> f32 {
  match axes.block_axis() {
    PhysicalAxis::X => style.content_size.width,
    PhysicalAxis::Y => style.content_size.height,
  }
}

fn page_side_for_index(page_index: usize) -> PageSide {
  if (page_index + 1) % 2 == 0 {
    PageSide::Left
  } else {
    PageSide::Right
  }
}

fn required_page_side(boundaries: &[ForcedBoundary], pos: f32) -> Option<PageSide> {
  boundaries
    .iter()
    .find(|b| (b.position - pos).abs() < EPSILON)
    .and_then(|b| b.page_side)
}

fn dedup_forced_boundaries(mut boundaries: Vec<ForcedBoundary>) -> Vec<ForcedBoundary> {
  boundaries.sort_by(|a, b| {
    a.position
      .partial_cmp(&b.position)
      .unwrap_or(std::cmp::Ordering::Equal)
  });

  let mut deduped: Vec<ForcedBoundary> = Vec::new();
  for boundary in boundaries.drain(..) {
    if let Some(last) = deduped.last_mut() {
      if (last.position - boundary.position).abs() < EPSILON {
        if last.page_side.is_none() {
          last.page_side = boundary.page_side;
        }
        continue;
      }
    }
    deduped.push(boundary);
  }
  deduped
}

#[derive(Debug, Clone)]
struct CachedLayout {
  root: FragmentNode,
  total_block_size: f32,
  forced_boundaries: Vec<ForcedBoundary>,
  atomic_ranges: Vec<AtomicRange>,
  page_name_spans: Vec<PageNameSpan>,
  boundaries: Vec<f32>,
}

impl CachedLayout {
  fn from_root(
    root: FragmentNode,
    style: &ResolvedPageStyle,
    fallback_page_name: Option<&str>,
    axes: FragmentAxes,
  ) -> Self {
    let mut spans = Vec::new();
    let root_block_size = axes.block_size(&root.logical_bounds());
    collect_page_name_spans(&root, 0.0, root_block_size, axes, &mut spans);
    spans.sort_by(|a, b| {
      a.start
        .partial_cmp(&b.start)
        .unwrap_or(std::cmp::Ordering::Equal)
    });

    let mut forced = collect_forced_boundaries_with_axes(&root, 0.0, axes);
    forced.extend(
      page_name_boundaries(&spans, fallback_page_name)
        .into_iter()
        .map(|position| ForcedBoundary {
          position,
          page_side: None,
        }),
    );
    let mut atomic_ranges = Vec::new();
    let fragmentainer = page_block_size(style, axes).max(EPSILON);
    collect_atomic_ranges_with_axes(
      &root,
      0.0,
      axes,
      &mut atomic_ranges,
      FragmentationContext::Page,
      Some(fragmentainer),
    );
    normalize_atomic_ranges(&mut atomic_ranges);

    let mut boundaries = resolve_fragmentation_boundaries_with_axes(
      &root,
      fragmentainer,
      FragmentationContext::Page,
      axes,
    );

    let content_block_size = axes.block_size(&root.logical_bounding_box());
    let mut total_block_size = if content_block_size > EPSILON {
      content_block_size
    } else {
      fragmentainer
    };
    boundaries.extend(forced.iter().map(|b| b.position));
    if total_block_size.is_finite() && total_block_size > 0.0 {
      boundaries.push(total_block_size);
    }
    boundaries.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
    boundaries.dedup_by(|a, b| (*a - *b).abs() < EPSILON);
    if let Some(last) = boundaries.last().copied() {
      total_block_size = total_block_size.max(last);
    }
    forced.push(ForcedBoundary {
      position: total_block_size,
      page_side: None,
    });
    forced = dedup_forced_boundaries(forced);
    if let Some(last) = boundaries.last().copied() {
      if (last - total_block_size).abs() > EPSILON {
        boundaries.push(total_block_size);
      }
    }

    Self {
      root,
      total_block_size,
      forced_boundaries: forced,
      atomic_ranges,
      page_name_spans: spans,
      boundaries,
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct PageLayoutKey {
  width_bits: u64,
  height_bits: u64,
  style_hash: u64,
  font_generation: u64,
}

impl PageLayoutKey {
  fn new(style: &ResolvedPageStyle, style_hash: u64, font_generation: u64) -> Self {
    Self {
      width_bits: style.content_size.width.to_bits() as u64,
      height_bits: style.content_size.height.to_bits() as u64,
      style_hash,
      font_generation,
    }
  }
}

/// Split a laid out fragment tree into pages using the provided @page rules.
///
/// When @page rules change the content size between pages (e.g., :left/:right or named pages),
/// each page is re-laid out against its resolved page style so line wrapping matches the used
/// page box. Layouts are cached per page style to avoid redundant work when the same style is
/// reused (e.g., multiple :right pages).
pub fn paginate_fragment_tree(
  box_tree: &BoxTree,
  initial_layout: Option<(&ResolvedPageStyle, &FragmentNode)>,
  rules: &[CollectedPageRule<'_>],
  fallback_page_size: Size,
  font_ctx: &FontContext,
  root_style: &Arc<ComputedStyle>,
  root_font_size: f32,
  initial_page_name: Option<String>,
  enable_layout_cache: bool,
) -> Result<Vec<FragmentNode>, LayoutError> {
  let log_running_elements =
    crate::debug::runtime::runtime_toggles().truthy("FASTR_LOG_RUNNING_ELEMENTS");
  if rules.is_empty() {
    if let Some((_, root)) = initial_layout {
      return Ok(vec![root.clone()]);
    }

    let mut config = LayoutConfig::for_viewport(fallback_page_size);
    config.enable_cache = enable_layout_cache;
    let engine = LayoutEngine::with_font_context(config, font_ctx.clone());
    let tree = engine.layout_tree(box_tree)?;
    return Ok(vec![tree.root]);
  }

  let axes =
    FragmentAxes::from_writing_mode_and_direction(root_style.writing_mode, root_style.direction);
  let style_hash = layout_style_fingerprint(root_style);
  let font_generation = font_ctx.font_generation();
  let mut layouts: HashMap<PageLayoutKey, CachedLayout> = HashMap::new();
  let base_style_for_margins = Some(root_style.as_ref());
  let fallback_page_name = initial_page_name.as_deref();

  if let Some((style, root)) = initial_layout {
    let key = PageLayoutKey::new(style, style_hash, font_generation);
    layouts
      .entry(key)
      .or_insert_with(|| CachedLayout::from_root(root.clone(), style, fallback_page_name, axes));
  }

  let base_style = resolve_page_style(
    rules,
    0,
    initial_page_name.as_deref(),
    PageSide::Right,
    false,
    fallback_page_size,
    root_font_size,
    base_style_for_margins,
  );
  let base_key = PageLayoutKey::new(&base_style, style_hash, font_generation);
  let base_layout = layout_for_style(
    &base_style,
    base_key,
    &mut layouts,
    box_tree,
    font_ctx,
    fallback_page_name,
    enable_layout_cache,
    axes,
  )?;
  let base_total_block_size = base_layout.total_block_size.max(EPSILON);
  let base_spans = base_layout.page_name_spans.clone();
  let base_forced = base_layout.forced_boundaries.clone();
  let base_root = base_layout.root.clone();

  let mut string_set_events = collect_string_set_events(&base_root, box_tree);
  string_set_events.sort_by(|a, b| a.abs_y.partial_cmp(&b.abs_y).unwrap_or(Ordering::Equal));
  let mut string_event_idx = 0usize;
  let mut string_set_carry: HashMap<String, String> = HashMap::new();

  let mut pages: Vec<(
    FragmentNode,
    ResolvedPageStyle,
    HashMap<String, RunningStringValues>,
    HashMap<String, Vec<FragmentNode>>,
  )> = Vec::new();
  let mut boundary_index = 0usize;
  let base_boundaries = if base_layout.boundaries.is_empty() {
    vec![0.0, base_total_block_size]
  } else {
    base_layout.boundaries.clone()
  };
  let mut page_index = 0usize;

  while boundary_index + 1 < base_boundaries.len() {
    let start_in_base = base_boundaries
      .get(boundary_index)
      .copied()
      .unwrap_or(base_total_block_size);
    let base_end = base_boundaries
      .get(boundary_index + 1)
      .copied()
      .unwrap_or(base_total_block_size);
    let mut end_in_base = start_in_base;
    let mut page_name =
      page_name_for_position(&base_spans, start_in_base, initial_page_name.as_deref());
    let side = page_side_for_index(page_index);
    let required_side = required_page_side(&base_forced, start_in_base);
    let is_blank_page = required_side.map_or(false, |required| required != side);

    let mut page_style = resolve_page_style(
      rules,
      page_index,
      page_name.as_deref(),
      side,
      is_blank_page,
      fallback_page_size,
      root_font_size,
      base_style_for_margins,
    );
    let mut key = PageLayoutKey::new(&page_style, style_hash, font_generation);
    let mut layout = layout_for_style(
      &page_style,
      key,
      &mut layouts,
      box_tree,
      font_ctx,
      fallback_page_name,
      enable_layout_cache,
      axes,
    )?;

    if layout.total_block_size <= EPSILON {
      break;
    }

    let mut fixed_fragments = Vec::new();
    collect_fixed_fragments(&layout.root, Point::ZERO, &mut fixed_fragments);
    let mut page_root = FragmentNode::new_block_styled(
      Rect::from_xywh(
        0.0,
        0.0,
        page_style.total_size.width,
        page_style.total_size.height,
      ),
      Vec::new(),
      Arc::new(page_style.page_style.clone()),
    );
    let mut page_running_elements: HashMap<String, Vec<FragmentNode>> = HashMap::new();

    if !is_blank_page {
      let layout_boundaries = if layout.boundaries.is_empty() {
        vec![0.0, layout.total_block_size]
      } else {
        layout.boundaries.clone()
      };
      let mut start = layout_boundaries
        .get(boundary_index)
        .copied()
        .unwrap_or(layout.total_block_size);
      let mut end = layout_boundaries
        .get(boundary_index + 1)
        .copied()
        .unwrap_or(layout.total_block_size);
      let actual_page_name =
        page_name_for_position(&layout.page_name_spans, start, initial_page_name.as_deref());
      if actual_page_name != page_name {
        page_name = actual_page_name;
        page_style = resolve_page_style(
          rules,
          page_index,
          page_name.as_deref(),
          side,
          is_blank_page,
          fallback_page_size,
          root_font_size,
          base_style_for_margins,
        );
        key = PageLayoutKey::new(&page_style, style_hash, font_generation);
        layout = layout_for_style(
          &page_style,
          key,
          &mut layouts,
          box_tree,
          font_ctx,
          fallback_page_name,
          enable_layout_cache,
          axes,
        )?;
        let layout_boundaries = if layout.boundaries.is_empty() {
          vec![0.0, layout.total_block_size]
        } else {
          layout.boundaries.clone()
        };
        start = layout_boundaries
          .get(boundary_index)
          .copied()
          .unwrap_or(layout.total_block_size);
        end = layout_boundaries
          .get(boundary_index + 1)
          .copied()
          .unwrap_or(layout.total_block_size);
      }

      if start >= layout.total_block_size - EPSILON {
        break;
      }

      end = end.min(layout.total_block_size);
      let root_block_size = axes.block_size(&layout.root.bounds);
      let clipped = clip_node_with_axes(
        &layout.root,
        start,
        end,
        0.0,
        start,
        root_block_size,
        axes,
        page_index,
        0,
        FragmentationContext::Page,
      );
      if let Some(mut content) = clipped {
        strip_fixed_fragments(&mut content);
        let content_block_size = axes.block_size(&content.bounds);
        normalize_fragment_margins_with_axes(
          &mut content,
          boundary_index == 0,
          end >= layout.total_block_size - 0.01,
          content_block_size,
          axes,
        );
        content.bounds = Rect::from_xywh(
          content.bounds.x(),
          content.bounds.y(),
          page_style.content_size.width,
          content.bounds.height(),
        );
        translate_fragment(
          &mut content,
          page_style.content_origin.x,
          page_style.content_origin.y,
        );
        page_running_elements =
          collect_running_elements_for_page(&content, content_block_size, axes);
        if log_running_elements {
          let mut counts: HashMap<String, usize> = HashMap::new();
          fn collect(node: &FragmentNode, out: &mut HashMap<String, usize>) {
            if let FragmentContent::RunningAnchor { name, .. } = &node.content {
              *out.entry(name.clone()).or_insert(0) += 1;
            }
            for child in &node.children {
              collect(child, out);
            }
          }
          fn first_text(node: &FragmentNode) -> Option<String> {
            match &node.content {
              FragmentContent::Text { text, .. } => Some(text.clone()),
              _ => {
                for child in &node.children {
                  if let Some(found) = first_text(child) {
                    return Some(found);
                  }
                }
                None
              }
            }
          }
          collect(&content, &mut counts);
          let mut previews: HashMap<String, Vec<String>> = HashMap::new();
          for (name, snapshots) in &page_running_elements {
            let mut texts = Vec::new();
            for snap in snapshots {
              if let Some(text) = first_text(snap) {
                let preview: String = text.chars().take(80).collect();
                texts.push(preview);
              }
            }
            previews.insert(name.clone(), texts);
          }
          eprintln!(
            "[paginate-running] page={} anchors={:?} selected={:?}",
            page_index, counts, previews
          );
        }
        page_root.children.push(content);
      }

      end_in_base = base_end;
    }

    for mut fixed in fixed_fragments {
      translate_fragment(
        &mut fixed,
        page_style.content_origin.x,
        page_style.content_origin.y,
      );
      page_root.children.push(fixed);
    }

    let page_strings = running_strings_for_page(
      &string_set_events,
      &mut string_event_idx,
      &mut string_set_carry,
      start_in_base,
      end_in_base,
    );

    pages.push((page_root, page_style, page_strings, page_running_elements));
    if !is_blank_page {
      boundary_index += 1;
    }
    page_index += 1;
  }

  if pages.is_empty() {
    return Ok(vec![base_root]);
  }

  let count = pages.len();
  let mut page_roots = Vec::with_capacity(count);
  let mut running_element_state: HashMap<String, FragmentNode> = HashMap::new();
  for (idx, (mut page, style, running_strings, running_elements)) in pages.into_iter().enumerate() {
    page.children.extend(build_margin_box_fragments(
      &style,
      font_ctx,
      idx,
      count,
      &running_strings,
      &running_elements,
      &running_element_state,
    ));
    for (name, anchors) in &running_elements {
      if let Some(last) = anchors.last() {
        running_element_state.insert(name.clone(), last.clone());
      }
    }
    propagate_fragment_metadata(&mut page, idx, count);
    page_roots.push(page);
  }

  Ok(page_roots)
}

/// Split a laid out fragment tree into pages using the provided @page rules with options.
pub fn paginate_fragment_tree_with_options(
  box_tree: &BoxTree,
  initial_layout: Option<(&ResolvedPageStyle, &FragmentNode)>,
  rules: &[CollectedPageRule<'_>],
  fallback_page_size: Size,
  font_ctx: &FontContext,
  root_style: &Arc<ComputedStyle>,
  root_font_size: f32,
  initial_page_name: Option<String>,
  enable_layout_cache: bool,
  options: PaginateOptions,
) -> Result<Vec<FragmentNode>, LayoutError> {
  let mut pages = paginate_fragment_tree(
    box_tree,
    initial_layout,
    rules,
    fallback_page_size,
    font_ctx,
    root_style,
    root_font_size,
    initial_page_name,
    enable_layout_cache,
  )?;

  apply_page_stacking(&mut pages, root_style.writing_mode, options.stacking);

  Ok(pages)
}

#[derive(Debug, Clone)]
struct PageNameSpan {
  start: f32,
  end: f32,
  name: String,
}

fn collect_page_name_spans(
  node: &FragmentNode,
  parent_abs_start: f32,
  parent_block_size: f32,
  axes: FragmentAxes,
  spans: &mut Vec<PageNameSpan>,
) {
  let logical = node.logical_bounds();
  let start = axes.abs_block_start(&logical, parent_abs_start, parent_block_size);
  let block_size = axes.block_size(&logical);
  let end = start + block_size;

  if let Some(style) = node.style.as_ref() {
    if let Some(name) = &style.page {
      spans.push(PageNameSpan {
        start,
        end,
        name: name.clone(),
      });
    }
  }

  for child in &node.children {
    collect_page_name_spans(child, start, block_size, axes, spans);
  }
}

fn page_name_for_position(
  spans: &[PageNameSpan],
  pos: f32,
  fallback: Option<&str>,
) -> Option<String> {
  if let Some(span) = spans.iter().find(|s| pos < s.end && pos >= s.start) {
    return Some(span.name.clone());
  }

  fallback.map(|s| s.to_string())
}

fn page_name_boundaries(spans: &[PageNameSpan], fallback: Option<&str>) -> Vec<f32> {
  if spans.is_empty() {
    return Vec::new();
  }

  let mut points: Vec<f32> = spans.iter().flat_map(|s| [s.start, s.end]).collect();
  points.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
  points.dedup_by(|a, b| (*a - *b).abs() < EPSILON);

  let mut boundaries = Vec::new();
  for pos in points {
    let before_pos = if pos > EPSILON { pos - EPSILON } else { 0.0 };
    let after_pos = pos + EPSILON;
    let before = page_name_for_position(spans, before_pos, fallback);
    let after = page_name_for_position(spans, after_pos, fallback);
    if before != after {
      boundaries.push(pos);
    }
  }

  boundaries
}

fn apply_page_stacking(
  pages: &mut [FragmentNode],
  writing_mode: WritingMode,
  stacking: PageStacking,
) {
  let PageStacking::Stacked { gap } = stacking else {
    return;
  };

  let gap = gap.max(0.0);
  let horizontal = block_axis_is_horizontal(writing_mode);
  let mut offset = 0.0;
  let mut previous_extent: Option<f32> = None;

  for page in pages.iter_mut() {
    if let Some(extent) = previous_extent {
      offset += extent + gap;
    }

    translate_fragment(
      page,
      if horizontal { offset } else { 0.0 },
      if horizontal { 0.0 } else { offset },
    );

    previous_extent = Some(if horizontal {
      page.bounds.width()
    } else {
      page.bounds.height()
    });
  }
}

fn running_strings_for_page(
  events: &[StringSetEvent],
  idx: &mut usize,
  carry: &mut HashMap<String, String>,
  start: f32,
  end: f32,
) -> HashMap<String, RunningStringValues> {
  let start_boundary = start - EPSILON;
  while *idx < events.len() && events[*idx].abs_y < start_boundary {
    let event = &events[*idx];
    carry.insert(event.name.clone(), event.value.clone());
    *idx += 1;
  }

  let mut snapshot = HashMap::new();
  for (name, value) in carry.iter() {
    snapshot.insert(
      name.clone(),
      RunningStringValues {
        start: Some(value.clone()),
        first: None,
        last: None,
      },
    );
  }

  while *idx < events.len() && events[*idx].abs_y < end {
    let event = &events[*idx];
    let entry = snapshot
      .entry(event.name.clone())
      .or_insert_with(|| RunningStringValues {
        start: carry.get(&event.name).cloned(),
        first: None,
        last: None,
      });
    if entry.first.is_none() {
      entry.first = Some(event.value.clone());
    }
    entry.last = Some(event.value.clone());
    carry.insert(event.name.clone(), event.value.clone());
    *idx += 1;
  }

  snapshot
}

fn collect_running_elements_for_page(
  root: &FragmentNode,
  parent_block_size: f32,
  axes: FragmentAxes,
) -> HashMap<String, Vec<FragmentNode>> {
  let mut occurrences: HashMap<String, Vec<(f32, FragmentNode)>> = HashMap::new();
  collect_running_element_occurrences(
    root,
    Point::ZERO,
    0.0,
    parent_block_size,
    axes,
    &mut occurrences,
  );

  let mut out: HashMap<String, Vec<FragmentNode>> = HashMap::new();
  for (name, mut entries) in occurrences {
    entries.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap_or(Ordering::Equal));
    let mut snapshots = Vec::with_capacity(entries.len());
    for (_, mut snapshot) in entries {
      strip_running_anchor_fragments(&mut snapshot);
      clear_running_position(&mut snapshot);
      let offset = Point::new(-snapshot.bounds.x(), -snapshot.bounds.y());
      snapshots.push(snapshot.translate(offset));
    }
    out.insert(name, snapshots);
  }

  out
}

fn collect_running_element_occurrences(
  node: &FragmentNode,
  origin: Point,
  abs_block_start: f32,
  parent_block_size: f32,
  axes: FragmentAxes,
  out: &mut HashMap<String, Vec<(f32, FragmentNode)>>,
) {
  let abs_origin = Point::new(origin.x + node.bounds.x(), origin.y + node.bounds.y());
  let node_abs_block = axes.abs_block_start(&node.bounds, abs_block_start, parent_block_size);
  let node_block_size = axes.block_size(&node.bounds);

  if let FragmentContent::RunningAnchor { name, snapshot } = &node.content {
    out
      .entry(name.clone())
      .or_default()
      .push((node_abs_block, (**snapshot).clone()));
  } else if node.content.is_block() || node.content.is_inline() || node.content.is_replaced() {
    if let Some(name) = node
      .style
      .as_deref()
      .and_then(|style| style.running_position.as_ref())
    {
      out
        .entry(name.clone())
        .or_default()
        .push((node_abs_block, node.clone()));
    }
  }

  for child in &node.children {
    collect_running_element_occurrences(
      child,
      abs_origin,
      node_abs_block,
      node_block_size,
      axes,
      out,
    );
  }
}

fn strip_running_anchor_fragments(node: &mut FragmentNode) {
  let mut kept: Vec<FragmentNode> = Vec::with_capacity(node.children.len());
  for mut child in node.children.drain(..) {
    if matches!(child.content, FragmentContent::RunningAnchor { .. }) {
      continue;
    }
    strip_running_anchor_fragments(&mut child);
    kept.push(child);
  }
  node.children = kept;
}

fn clear_running_position(node: &mut FragmentNode) {
  if let Some(style) = node.style.as_deref() {
    if style.running_position.is_some() {
      let mut owned = style.clone();
      owned.running_position = None;
      node.style = Some(Arc::new(owned));
    }
  }
  for child in &mut node.children {
    clear_running_position(child);
  }
}

fn select_running_element(
  ident: &str,
  select: RunningElementSelect,
  page_anchors: &HashMap<String, Vec<FragmentNode>>,
  running_state: &HashMap<String, FragmentNode>,
) -> Option<FragmentNode> {
  if let Some(list) = page_anchors.get(ident) {
    if !list.is_empty() {
      return match select {
        RunningElementSelect::Last => list.last().cloned(),
        RunningElementSelect::First | RunningElementSelect::Start => list.first().cloned(),
      };
    }
  }
  running_state.get(ident).cloned()
}

fn translate_fragment(node: &mut FragmentNode, dx: f32, dy: f32) {
  node.bounds = Rect::from_xywh(
    node.bounds.x() + dx,
    node.bounds.y() + dy,
    node.bounds.width(),
    node.bounds.height(),
  );
  if let Some(logical) = node.logical_override {
    node.logical_override = Some(Rect::from_xywh(
      logical.x() + dx,
      logical.y() + dy,
      logical.width(),
      logical.height(),
    ));
  }
}

fn is_fixed_fragment(fragment: &FragmentNode) -> bool {
  fragment
    .style
    .as_deref()
    .is_some_and(|style| style.position == Position::Fixed)
}

fn strip_fixed_fragments(node: &mut FragmentNode) {
  let mut kept = Vec::with_capacity(node.children.len());
  for mut child in node.children.drain(..) {
    if is_fixed_fragment(&child) {
      continue;
    }
    strip_fixed_fragments(&mut child);
    kept.push(child);
  }
  node.children = kept;
}

fn collect_fixed_fragments(node: &FragmentNode, origin: Point, out: &mut Vec<FragmentNode>) {
  if is_fixed_fragment(node) {
    let mut cloned = node.clone();
    translate_fragment(&mut cloned, origin.x, origin.y);
    out.push(cloned);
    return;
  }

  let next_origin = Point::new(origin.x + node.bounds.x(), origin.y + node.bounds.y());
  for child in &node.children {
    collect_fixed_fragments(child, next_origin, out);
  }
}

fn build_margin_box_fragments(
  style: &ResolvedPageStyle,
  font_ctx: &FontContext,
  page_index: usize,
  page_count: usize,
  running_strings: &HashMap<String, RunningStringValues>,
  running_elements: &HashMap<String, Vec<FragmentNode>>,
  running_state: &HashMap<String, FragmentNode>,
) -> Vec<FragmentNode> {
  let mut fragments = Vec::new();

  for (area, box_style) in &style.margin_boxes {
    if matches!(
      box_style.content_value,
      ContentValue::None | ContentValue::Normal
    ) {
      continue;
    }
    if matches!(box_style.display, Display::None) {
      continue;
    }

    if let Some(bounds) = margin_box_bounds(*area, style) {
      if bounds.width() <= 0.0 || bounds.height() <= 0.0 {
        continue;
      }

      let style_arc = Arc::new(box_style.clone());
      if let ContentValue::Items(items) = &box_style.content_value {
        if items.len() == 1 {
          if let ContentItem::Element { ident, select } = &items[0] {
            if let Some(snapshot) =
              select_running_element(ident, *select, running_elements, running_state)
            {
              fragments.push(FragmentNode::new_block_styled(
                bounds,
                vec![snapshot],
                style_arc,
              ));
              continue;
            }
          }
        }
      }
      let children = build_margin_box_children(
        box_style,
        page_index,
        page_count,
        running_strings,
        &style_arc,
      );
      let root = BoxNode::new_block(style_arc.clone(), FormattingContextType::Block, children);
      let box_tree = BoxTree::new(root);

      let config = LayoutConfig::new(Size::new(bounds.width(), bounds.height()));
      let engine = LayoutEngine::with_font_context(config, font_ctx.clone());
      if let Ok(mut tree) = engine.layout_tree(&box_tree) {
        tree.root.bounds = Rect::from_xywh(
          tree.root.bounds.x(),
          tree.root.bounds.y(),
          bounds.width(),
          bounds.height(),
        );
        tree.root.scroll_overflow = Rect::from_xywh(
          tree.root.scroll_overflow.x(),
          tree.root.scroll_overflow.y(),
          tree.root.scroll_overflow.width().max(bounds.width()),
          tree.root.scroll_overflow.height().max(bounds.height()),
        );
        translate_fragment(&mut tree.root, bounds.x(), bounds.y());
        fragments.push(tree.root);
      }
    }
  }

  fragments
}

fn build_margin_box_children(
  box_style: &ComputedStyle,
  page_index: usize,
  page_count: usize,
  running_strings: &HashMap<String, RunningStringValues>,
  style: &Arc<ComputedStyle>,
) -> Vec<BoxNode> {
  let mut children: Vec<BoxNode> = Vec::new();
  let mut context = ContentContext::new();
  context.set_quotes(box_style.quotes.clone());
  context.set_running_strings(running_strings.clone());
  context.set_counter(
    "page",
    page_index.saturating_add(1).min(i32::MAX as usize) as i32,
  );
  context.set_counter("pages", page_count.min(i32::MAX as usize) as i32);

  let mut text_buf = String::new();
  let flush_text = |buf: &mut String, out: &mut Vec<BoxNode>, style: &Arc<ComputedStyle>| {
    if !buf.is_empty() {
      out.push(BoxNode::new_text(style.clone(), buf.clone()));
      buf.clear();
    }
  };

  match &box_style.content_value {
    ContentValue::Items(items) => {
      for item in items {
        match item {
          ContentItem::String(s) => text_buf.push_str(s),
          ContentItem::Attr { name, fallback, .. } => {
            if let Some(val) = context.get_attribute(name) {
              text_buf.push_str(val);
            } else if let Some(fb) = fallback {
              text_buf.push_str(fb);
            }
          }
          ContentItem::Counter { name, style } => {
            let value = context.get_counter(name);
            let formatted = box_style
              .counter_styles
              .format_value(value, style.clone().unwrap_or(CounterStyle::Decimal.into()));
            text_buf.push_str(&formatted);
          }
          ContentItem::Counters {
            name,
            separator,
            style,
          } => {
            let values = context.get_counters(name);
            let style_name = style.clone().unwrap_or(CounterStyle::Decimal.into());
            if values.is_empty() {
              text_buf.push_str(&box_style.counter_styles.format_value(0, style_name));
            } else {
              let formatted: Vec<String> = values
                .iter()
                .map(|v| {
                  box_style
                    .counter_styles
                    .format_value(*v, style_name.clone())
                })
                .collect();
              text_buf.push_str(&formatted.join(separator));
            }
          }
          ContentItem::StringReference { name, kind } => {
            text_buf.push_str(context.get_running_string(name, *kind).unwrap_or(""));
          }
          ContentItem::OpenQuote => {
            text_buf.push_str(context.open_quote());
            context.push_quote();
          }
          ContentItem::CloseQuote => {
            text_buf.push_str(context.close_quote());
            context.pop_quote();
          }
          ContentItem::NoOpenQuote => context.push_quote(),
          ContentItem::NoCloseQuote => context.pop_quote(),
          ContentItem::Url(url) => {
            flush_text(&mut text_buf, &mut children, style);
            children.push(BoxNode::new_replaced(
              style.clone(),
              ReplacedType::Image {
                src: url.clone(),
                alt: None,
                srcset: Vec::new(),
                sizes: None,
                picture_sources: Vec::new(),
              },
              None,
              None,
            ));
          }
          ContentItem::Element { .. } => {
            flush_text(&mut text_buf, &mut children, style);
          }
        }
      }
    }
    ContentValue::None | ContentValue::Normal => {}
  }

  flush_text(&mut text_buf, &mut children, style);
  children
}

fn layout_for_style<'a>(
  style: &ResolvedPageStyle,
  key: PageLayoutKey,
  cache: &'a mut HashMap<PageLayoutKey, CachedLayout>,
  box_tree: &BoxTree,
  font_ctx: &FontContext,
  fallback_page_name: Option<&str>,
  enable_layout_cache: bool,
  axes: FragmentAxes,
) -> Result<&'a CachedLayout, LayoutError> {
  if !cache.contains_key(&key) {
    let mut config = LayoutConfig::for_viewport(style.content_size);
    config.enable_cache = enable_layout_cache;
    let engine = LayoutEngine::with_font_context(config, font_ctx.clone());
    let _hint = set_fragmentainer_block_size_hint(Some(page_block_size(style, axes)));
    let layout_tree = engine.layout_tree(box_tree)?;
    let layout = CachedLayout::from_root(layout_tree.root, style, fallback_page_name, axes);
    cache.insert(key, layout);
  }

  Ok(cache.get(&key).expect("layout cache just populated"))
}

fn margin_box_bounds(area: PageMarginArea, style: &ResolvedPageStyle) -> Option<Rect> {
  let trimmed_width = style.page_size.width - 2.0 * style.trim;
  let trimmed_height = style.page_size.height - 2.0 * style.trim;
  let origin_x = style.bleed + style.trim;
  let origin_y = style.bleed + style.trim;
  let ml = style.margin_left;
  let mr = style.margin_right;
  let mt = style.margin_top;
  let mb = style.margin_bottom;

  let top_width = trimmed_width - ml - mr;
  let side_height = trimmed_height - mt - mb;

  let rect = |x: f32, y: f32, w: f32, h: f32| -> Option<Rect> {
    if w <= 0.0 || h <= 0.0 {
      None
    } else {
      Some(Rect::from_xywh(x, y, w, h))
    }
  };

  match area {
    PageMarginArea::TopLeftCorner => rect(origin_x, origin_y, ml, mt),
    PageMarginArea::TopLeft => rect(origin_x + ml, origin_y, top_width / 3.0, mt),
    PageMarginArea::TopCenter => rect(
      origin_x + ml + top_width / 3.0,
      origin_y,
      top_width / 3.0,
      mt,
    ),
    PageMarginArea::TopRight => rect(
      origin_x + ml + 2.0 * top_width / 3.0,
      origin_y,
      top_width / 3.0,
      mt,
    ),
    PageMarginArea::TopRightCorner => rect(origin_x + trimmed_width - mr, origin_y, mr, mt),
    PageMarginArea::RightTop => rect(
      origin_x + trimmed_width - mr,
      origin_y + mt,
      mr,
      side_height / 3.0,
    ),
    PageMarginArea::RightMiddle => rect(
      origin_x + trimmed_width - mr,
      origin_y + mt + side_height / 3.0,
      mr,
      side_height / 3.0,
    ),
    PageMarginArea::RightBottom => rect(
      origin_x + trimmed_width - mr,
      origin_y + mt + 2.0 * side_height / 3.0,
      mr,
      side_height / 3.0,
    ),
    PageMarginArea::BottomRightCorner => rect(
      origin_x + trimmed_width - mr,
      origin_y + trimmed_height - mb,
      mr,
      mb,
    ),
    PageMarginArea::BottomRight => rect(
      origin_x + ml + 2.0 * top_width / 3.0,
      origin_y + trimmed_height - mb,
      top_width / 3.0,
      mb,
    ),
    PageMarginArea::BottomCenter => rect(
      origin_x + ml + top_width / 3.0,
      origin_y + trimmed_height - mb,
      top_width / 3.0,
      mb,
    ),
    PageMarginArea::BottomLeft => rect(
      origin_x + ml,
      origin_y + trimmed_height - mb,
      top_width / 3.0,
      mb,
    ),
    PageMarginArea::BottomLeftCorner => rect(origin_x, origin_y + trimmed_height - mb, ml, mb),
    PageMarginArea::LeftBottom => rect(
      origin_x,
      origin_y + mt + 2.0 * side_height / 3.0,
      ml,
      side_height / 3.0,
    ),
    PageMarginArea::LeftMiddle => rect(
      origin_x,
      origin_y + mt + side_height / 3.0,
      ml,
      side_height / 3.0,
    ),
    PageMarginArea::LeftTop => rect(origin_x, origin_y + mt, ml, side_height / 3.0),
  }
}
