//! Pagination helpers that honor CSS @page rules and margin boxes.

use std::collections::HashMap;
use std::sync::Arc;

use crate::css::types::{CollectedPageRule, PageMarginArea};
use crate::geometry::{Point, Rect, Size};
use crate::layout::engine::{LayoutConfig, LayoutEngine};
use crate::layout::formatting_context::{
  layout_style_fingerprint, set_fragmentainer_block_size_hint, LayoutError,
};
use crate::layout::fragmentation::{
  clip_node, collect_forced_boundaries, propagate_fragment_metadata,
};
use crate::style::content::{ContentContext, ContentGenerator};
use crate::style::page::{resolve_page_style, PageSide, ResolvedPageStyle};
use crate::style::position::Position;
use crate::style::types::WritingMode;
use crate::style::{block_axis_is_horizontal, ComputedStyle};
use crate::text::font_loader::FontContext;
use crate::text::pipeline::ShapingPipeline;
use crate::tree::box_tree::BoxTree;
use crate::tree::fragment_tree::FragmentNode;

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

#[derive(Debug, Clone)]
struct CachedLayout {
  root: FragmentNode,
  total_height: f32,
  forced_boundaries: Vec<f32>,
  page_name_spans: Vec<PageNameSpan>,
}

impl CachedLayout {
  fn from_root(root: FragmentNode, style: &ResolvedPageStyle) -> Self {
    let mut spans = Vec::new();
    collect_page_name_spans(&root, 0.0, &mut spans);
    spans.sort_by(|a, b| {
      a.start
        .partial_cmp(&b.start)
        .unwrap_or(std::cmp::Ordering::Equal)
    });

    let mut forced = collect_forced_boundaries(&root, 0.0);
    let total_height = root
      .logical_bounding_box()
      .height()
      .max(style.content_size.height);
    forced.push(total_height);
    forced.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
    forced.dedup_by(|a, b| (*a - *b).abs() < EPSILON);

    Self {
      root,
      total_height,
      forced_boundaries: forced,
      page_name_spans: spans,
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

  let style_hash = layout_style_fingerprint(root_style);
  let font_generation = font_ctx.font_generation();
  let counter_styles = root_style.counter_styles.clone();
  let mut layouts: HashMap<PageLayoutKey, CachedLayout> = HashMap::new();

  if let Some((style, root)) = initial_layout {
    let key = PageLayoutKey::new(style, style_hash, font_generation);
    layouts
      .entry(key)
      .or_insert_with(|| CachedLayout::from_root(root.clone(), style));
  }

  let base_style = resolve_page_style(
    rules,
    0,
    initial_page_name.as_deref(),
    PageSide::Right,
    fallback_page_size,
    root_font_size,
  );
  let base_key = PageLayoutKey::new(&base_style, style_hash, font_generation);
  let base_layout = layout_for_style(
    &base_style,
    base_key,
    &mut layouts,
    box_tree,
    font_ctx,
    enable_layout_cache,
  )?;
  let base_total_height = base_layout.total_height.max(EPSILON);
  let base_spans = base_layout.page_name_spans.clone();
  let base_root = base_layout.root.clone();

  let mut pages: Vec<(FragmentNode, ResolvedPageStyle)> = Vec::new();
  let mut consumed_base = 0.0f32;
  let mut page_index = 0usize;

  loop {
    let start_in_base = consumed_base;
    let mut page_name =
      page_name_for_position(&base_spans, start_in_base, initial_page_name.as_deref());
    let side = if (page_index + 1) % 2 == 0 {
      PageSide::Left
    } else {
      PageSide::Right
    };

    let mut page_style = resolve_page_style(
      rules,
      page_index,
      page_name.as_deref(),
      side,
      fallback_page_size,
      root_font_size,
    );
    let mut key = PageLayoutKey::new(&page_style, style_hash, font_generation);
    let mut layout = layout_for_style(
      &page_style,
      key,
      &mut layouts,
      box_tree,
      font_ctx,
      enable_layout_cache,
    )?;

    let mut total_height = layout.total_height;
    if total_height <= EPSILON {
      break;
    }

    let mut start = ((consumed_base / base_total_height) * total_height).min(total_height);
    let actual_page_name =
      page_name_for_position(&layout.page_name_spans, start, initial_page_name.as_deref());
    if actual_page_name != page_name {
      page_name = actual_page_name;
      page_style = resolve_page_style(
        rules,
        page_index,
        page_name.as_deref(),
        side,
        fallback_page_size,
        root_font_size,
      );
      key = PageLayoutKey::new(&page_style, style_hash, font_generation);
      layout = layout_for_style(
        &page_style,
        key,
        &mut layouts,
        box_tree,
        font_ctx,
        enable_layout_cache,
      )?;
      total_height = layout.total_height;
      start = ((consumed_base / base_total_height) * total_height).min(total_height);
    }

    if start >= total_height - EPSILON {
      break;
    }

    let page_block = page_style.content_size.height.max(1.0);
    let mut end = (start + page_block).min(total_height);
    if let Some(boundary) = layout
      .forced_boundaries
      .iter()
      .copied()
      .find(|b| *b > start + EPSILON && *b < end - EPSILON)
    {
      end = boundary;
    }

    if end <= start + EPSILON {
      end = (start + page_block).min(total_height);
    }

    let clipped = clip_node(&layout.root, start, end, 0.0, start, page_index, 0);
    let mut fixed_fragments = Vec::new();
    collect_fixed_fragments(&layout.root, Point::ZERO, &mut fixed_fragments);
    let mut page_root = FragmentNode::new_block(
      Rect::from_xywh(
        0.0,
        0.0,
        page_style.total_size.width,
        page_style.total_size.height,
      ),
      Vec::new(),
    );

    if let Some(mut content) = clipped {
      strip_fixed_fragments(&mut content);
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
      page_root.children.push(content);
    }

    for mut fixed in fixed_fragments {
      translate_fragment(
        &mut fixed,
        page_style.content_origin.x,
        page_style.content_origin.y,
      );
      page_root.children.push(fixed);
    }

    pages.push((page_root, page_style));
    let base_advance = (end / total_height) * base_total_height;
    consumed_base = (consumed_base + base_advance).min(base_total_height);
    page_index += 1;

    if consumed_base >= base_total_height - EPSILON {
      break;
    }
  }

  if pages.is_empty() {
    return Ok(vec![base_root]);
  }

  let count = pages.len();
  let mut page_roots = Vec::with_capacity(count);
  for (idx, (mut page, style)) in pages.into_iter().enumerate() {
    page
      .children
      .extend(build_margin_box_fragments(
        &style,
        font_ctx,
        counter_styles.clone(),
        idx,
        count,
      ));
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

fn collect_page_name_spans(node: &FragmentNode, abs_start: f32, spans: &mut Vec<PageNameSpan>) {
  let logical = node.logical_bounds();
  let start = abs_start + logical.y();
  let end = start + logical.height();

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
    collect_page_name_spans(child, start, spans);
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

  if let Some(next) = spans.iter().filter(|s| s.start >= pos).min_by(|a, b| {
    a.start
      .partial_cmp(&b.start)
      .unwrap_or(std::cmp::Ordering::Equal)
  }) {
    return Some(next.name.clone());
  }

  fallback.map(|s| s.to_string())
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
  counter_styles: Arc<CounterStyleRegistry>,
  page_index: usize,
  page_count: usize,
) -> Vec<FragmentNode> {
  let mut fragments = Vec::new();
  let generator = ContentGenerator::with_counter_styles(counter_styles);
  let shaper = ShapingPipeline::new();
  let mut context = ContentContext::new();
  context.set_counter("page", (page_index + 1) as i32);
  context.set_counter("pages", page_count as i32);

  for (area, box_style) in &style.margin_boxes {
    if let Some(bounds) = margin_box_bounds(*area, style) {
      if bounds.width() <= 0.0 || bounds.height() <= 0.0 {
        continue;
      }

      let text = generator.generate(&box_style.content_value, &mut context);
      let mut children = Vec::new();
      if !text.is_empty() {
        let runs = shaper.shape(&text, box_style, font_ctx).unwrap_or_default();
        let text_w = runs.iter().map(|run| run.advance).sum::<f32>().max(0.0);
        let font_size = box_style.font_size.max(1.0);
        let text_h = font_size * 1.2;
        let baseline = font_size * 0.9;
        let text_x = bounds.x() + (bounds.width() - text_w).max(0.0) / 2.0;
        let text_y = bounds.y() + (bounds.height() - text_h).max(0.0) / 2.0;
        let text_bounds = Rect::from_xywh(text_x, text_y, text_w, text_h);
        children.push(FragmentNode::new_text_shaped(
          text_bounds,
          text,
          baseline,
          runs,
          Arc::new(box_style.clone()),
        ));
      }

      fragments.push(FragmentNode::new_block_styled(
        bounds,
        children,
        Arc::new(box_style.clone()),
      ));
    }
  }

  fragments
}

fn layout_for_style<'a>(
  style: &ResolvedPageStyle,
  key: PageLayoutKey,
  cache: &'a mut HashMap<PageLayoutKey, CachedLayout>,
  box_tree: &BoxTree,
  font_ctx: &FontContext,
  enable_layout_cache: bool,
) -> Result<&'a CachedLayout, LayoutError> {
  if !cache.contains_key(&key) {
    let mut config = LayoutConfig::for_viewport(style.content_size);
    config.enable_cache = enable_layout_cache;
    let engine = LayoutEngine::with_font_context(config, font_ctx.clone());
    let _hint = set_fragmentainer_block_size_hint(Some(style.content_size.height));
    let layout_tree = engine.layout_tree(box_tree)?;
    let layout = CachedLayout::from_root(layout_tree.root, style);
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
