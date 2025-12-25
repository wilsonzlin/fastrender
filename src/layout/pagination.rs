//! Pagination helpers that honor CSS @page rules and margin boxes.

use std::sync::Arc;

use crate::css::types::CollectedPageRule;
use crate::geometry::{Point, Rect, Size};
use crate::layout::fragmentation::{
  build_break_plan, clip_node, propagate_fragment_metadata, select_fragmentainer_boundary,
  fragmentation_axis, BlockAxis,
};
use crate::style::content::{ContentContext, ContentGenerator};
use crate::style::page::{resolve_page_style, PageSide, ResolvedPageStyle};
use crate::style::position::Position;
use crate::style::ComputedStyle;
use crate::text::font_loader::FontContext;
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

#[derive(Clone)]
struct FixedFragment {
  origin: Point,
  fragment: FragmentNode,
}

/// Split a laid out fragment tree into pages using the provided @page rules.
pub fn paginate_fragment_tree(
  root: &FragmentNode,
  rules: &[CollectedPageRule<'_>],
  fallback_page_size: Size,
  font_ctx: &FontContext,
  root_font_size: f32,
  initial_page_name: Option<String>,
) -> Vec<FragmentNode> {
  paginate_fragment_tree_with_options(
    root,
    rules,
    fallback_page_size,
    font_ctx,
    root_font_size,
    initial_page_name,
    PaginateOptions::default(),
  )
}

/// Split a laid out fragment tree into pages using the provided @page rules with options.
pub fn paginate_fragment_tree_with_options(
  root: &FragmentNode,
  rules: &[CollectedPageRule<'_>],
  fallback_page_size: Size,
  font_ctx: &FontContext,
  root_font_size: f32,
  initial_page_name: Option<String>,
  options: PaginateOptions,
) -> Vec<FragmentNode> {
  if rules.is_empty() {
    return vec![root.clone()];
  }

  let axis = fragmentation_axis(root);
  let mut spans = Vec::new();
  let root_block_size = axis.block_size(&root.bounds);
  collect_page_name_spans(root, 0.0, root_block_size, &axis, &mut spans);
  spans.sort_by(|a, b| {
    a.start
      .partial_cmp(&b.start)
      .unwrap_or(std::cmp::Ordering::Equal)
  });

  let fallback_block_size = if axis.horizontal {
    fallback_page_size.width
  } else {
    fallback_page_size.height
  };
  let total_block = axis
    .block_size(&root.bounding_box())
    .max(fallback_block_size);
  let break_plan = build_break_plan(root, 0.0, total_block);

  let fixed_fragments = collect_fixed_fragments(root, &axis, root_block_size);

  let mut pages = Vec::new();
  let mut pos = 0.0;
  let mut page_index = 0;

  while pos < total_block - 0.01 {
    let page_name = page_name_for_position(&spans, pos, initial_page_name.as_deref());
    let side = if (page_index + 1) % 2 == 0 {
      PageSide::Left
    } else {
      PageSide::Right
    };

    let style = resolve_page_style(
      rules,
      page_index,
      page_name.as_deref(),
      side,
      fallback_page_size,
      root_font_size,
    );

    let page_block = if axis.horizontal {
      style.content_size.width
    } else {
      style.content_size.height
    }
    .max(1.0);
    let end = select_fragmentainer_boundary(pos, page_block, total_block, &break_plan);

    if end <= pos + 0.01 {
      break;
    }

    let clipped = clip_node(
      root,
      pos,
      end,
      0.0,
      pos,
      root_block_size,
      end - pos,
      &axis,
      page_index,
      0,
    );
    let mut page_root = FragmentNode::new_block(
      Rect::from_xywh(0.0, 0.0, style.total_size.width, style.total_size.height),
      Vec::new(),
    );

    if let Some(mut content) = clipped {
      strip_fixed_fragments(&mut content);
      content.bounds = Rect::from_xywh(
        content.bounds.x(),
        content.bounds.y(),
        style.content_size.width,
        content.bounds.height(),
      );
      translate_fragment(&mut content, style.content_origin.x, style.content_origin.y);
      page_root.children.push(content);
    }

    for fixed in &fixed_fragments {
      let mut repeated = fixed.fragment.clone();
      translate_fragment(
        &mut repeated,
        style.content_origin.x,
        style.content_origin.y,
      );
      page_root.children.push(repeated);
    }

    page_root
      .children
      .extend(build_margin_box_fragments(&style, font_ctx));

    pages.push(page_root);
    pos = end;
    page_index += 1;
  }

  if pages.is_empty() {
    return vec![root.clone()];
  }

  apply_page_stacking(&mut pages, &axis, options.stacking);

  let count = pages.len();
  for (idx, page) in pages.iter_mut().enumerate() {
    propagate_fragment_metadata(page, idx, count);
  }

  pages
}

#[derive(Debug, Clone)]
struct PageNameSpan {
  start: f32,
  end: f32,
  name: String,
}

fn collect_page_name_spans(
  node: &FragmentNode,
  abs_start: f32,
  parent_block_size: f32,
  axis: &BlockAxis,
  spans: &mut Vec<PageNameSpan>,
) {
  let node_block_size = axis.block_size(&node.bounds);
  let start = abs_start + axis.block_offset_in_parent(&node.bounds, parent_block_size);
  let end = start + node_block_size;

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
    collect_page_name_spans(child, start, node_block_size, axis, spans);
  }
}

fn collect_fixed_fragments(
  root: &FragmentNode,
  axis: &BlockAxis,
  root_block_size: f32,
) -> Vec<FixedFragment> {
  let mut fixed = Vec::new();
  collect_fixed_fragments_inner(root, axis, root_block_size, 0.0, 0.0, &mut fixed);
  fixed
}

fn collect_fixed_fragments_inner(
  node: &FragmentNode,
  axis: &BlockAxis,
  parent_block_size: f32,
  abs_x: f32,
  abs_y: f32,
  out: &mut Vec<FixedFragment>,
) {
  let (node_abs_x, node_abs_y) = {
    let offset_block = axis.block_offset_in_parent(&node.bounds, parent_block_size);
    let inline_offset = if axis.horizontal {
      node.bounds.y()
    } else {
      node.bounds.x()
    };
    if axis.horizontal {
      (abs_x + offset_block, abs_y + inline_offset)
    } else {
      (abs_x + inline_offset, abs_y + offset_block)
    }
  };

  if node
    .style
    .as_ref()
    .is_some_and(|style| style.position == Position::Fixed)
  {
    let mut cloned = node.clone();
    cloned.bounds = Rect::from_xywh(
      node_abs_x,
      node_abs_y,
      node.bounds.width(),
      node.bounds.height(),
    );
    cloned.scroll_overflow =
      Rect::from_xywh(0.0, 0.0, cloned.bounds.width(), cloned.bounds.height());
    cloned.fragmentainer_index = 0;
    cloned.fragment_count = 1;
    cloned.fragment_index = 0;
    out.push(FixedFragment {
      origin: Point::new(node_abs_x, node_abs_y),
      fragment: cloned,
    });
    return;
  }

  let node_block_size = axis.block_size(&node.bounds);
  for child in &node.children {
    collect_fixed_fragments_inner(child, axis, node_block_size, node_abs_x, node_abs_y, out);
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

fn is_fixed_fragment(node: &FragmentNode) -> bool {
  node
    .style
    .as_ref()
    .map(|style| matches!(style.position, Position::Fixed))
    .unwrap_or(false)
}

fn strip_fixed_fragments(node: &mut FragmentNode) {
  node.children.retain(|child| !is_fixed_fragment(child));
  for child in &mut node.children {
    strip_fixed_fragments(child);
  }
}

fn apply_page_stacking(pages: &mut [FragmentNode], axis: &BlockAxis, stacking: PageStacking) {
  let PageStacking::Stacked { gap } = stacking else {
    return;
  };

  let gap = gap.max(0.0);
  let mut offset = 0.0;
  let mut last_extent = None;

  for page in pages.iter_mut() {
    if let Some(extent) = last_extent {
      offset += extent + gap;
    }

    if axis.horizontal {
      translate_fragment(page, offset, 0.0);
    } else {
      translate_fragment(page, 0.0, offset);
    }

    last_extent = Some(axis.block_size(&page.bounds));
  }
}

fn translate_fragment(node: &mut FragmentNode, dx: f32, dy: f32) {
  node.bounds = Rect::from_xywh(
    node.bounds.x() + dx,
    node.bounds.y() + dy,
    node.bounds.width(),
    node.bounds.height(),
  );
  for child in &mut node.children {
    translate_fragment(child, dx, dy);
  }
}

fn build_margin_box_fragments(
  style: &ResolvedPageStyle,
  _font_ctx: &FontContext,
) -> Vec<FragmentNode> {
  let mut fragments = Vec::new();
  let generator = ContentGenerator::new();
  let mut context = ContentContext::new();

  for (area, box_style) in &style.margin_boxes {
    if let Some(bounds) = margin_box_bounds(*area, style) {
      if bounds.width() <= 0.0 || bounds.height() <= 0.0 {
        continue;
      }

      let text = generator.generate(&box_style.content_value, &mut context);
      let mut children = Vec::new();
      if !text.is_empty() {
        let (text_w, text_h, baseline) = measure_text(&text, box_style);
        let text_x = bounds.x() + (bounds.width() - text_w).max(0.0) / 2.0;
        let text_y = bounds.y() + (bounds.height() - text_h).max(0.0) / 2.0;
        let text_bounds = Rect::from_xywh(text_x, text_y, text_w, text_h);
        children.push(FragmentNode::new_text_shaped(
          text_bounds,
          text,
          baseline,
          Vec::new(),
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

fn margin_box_bounds(
  area: crate::css::types::PageMarginArea,
  style: &ResolvedPageStyle,
) -> Option<Rect> {
  let trimmed_width = style.page_size.width - 2.0 * style.trim;
  let trimmed_height = style.page_size.height - 2.0 * style.trim;
  let origin_x = style.bleed + style.trim;
  let origin_y = style.bleed + style.trim;

  match area {
    crate::css::types::PageMarginArea::TopLeft => Some(Rect::from_xywh(
      origin_x,
      origin_y,
      style.margin_left,
      style.margin_top,
    )),
    crate::css::types::PageMarginArea::TopCenter => Some(Rect::from_xywh(
      origin_x + style.margin_left,
      origin_y,
      trimmed_width - style.margin_left - style.margin_right,
      style.margin_top,
    )),
    crate::css::types::PageMarginArea::TopRight => Some(Rect::from_xywh(
      origin_x + trimmed_width - style.margin_right,
      origin_y,
      style.margin_right,
      style.margin_top,
    )),
    crate::css::types::PageMarginArea::BottomLeft => Some(Rect::from_xywh(
      origin_x,
      origin_y + trimmed_height - style.margin_bottom,
      style.margin_left,
      style.margin_bottom,
    )),
    crate::css::types::PageMarginArea::BottomCenter => Some(Rect::from_xywh(
      origin_x + style.margin_left,
      origin_y + trimmed_height - style.margin_bottom,
      trimmed_width - style.margin_left - style.margin_right,
      style.margin_bottom,
    )),
    crate::css::types::PageMarginArea::BottomRight => Some(Rect::from_xywh(
      origin_x + trimmed_width - style.margin_right,
      origin_y + trimmed_height - style.margin_bottom,
      style.margin_right,
      style.margin_bottom,
    )),
  }
}

fn measure_text(text: &str, style: &ComputedStyle) -> (f32, f32, f32) {
  let font_size = style.font_size.max(1.0);
  let width = text.chars().count() as f32 * font_size * 0.6;
  let height = font_size * 1.2;
  let baseline = font_size * 0.9;
  (width, height, baseline)
}
