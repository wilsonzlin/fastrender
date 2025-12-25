//! Pagination helpers that honor CSS @page rules and margin boxes.

use std::sync::Arc;

use crate::css::types::CollectedPageRule;
use crate::geometry::{Rect, Size};
use crate::layout::constraints::LayoutConstraints;
use crate::layout::engine::{LayoutConfig, LayoutEngine};
use crate::layout::fragmentation::{
  clip_node, collect_forced_boundaries, propagate_fragment_metadata,
};
use crate::style::content::{ContentContext, ContentGenerator};
use crate::style::display::FormattingContextType;
use crate::style::page::{resolve_page_style, PageSide, ResolvedPageStyle};
use crate::style::types::BoxSizing;
use crate::style::values::Length;
use crate::style::ComputedStyle;
use crate::text::font_loader::FontContext;
use crate::tree::box_tree::BoxNode;
use crate::tree::fragment_tree::FragmentNode;

/// Split a laid out fragment tree into pages using the provided @page rules.
pub fn paginate_fragment_tree(
  root: &FragmentNode,
  rules: &[CollectedPageRule<'_>],
  fallback_page_size: Size,
  font_ctx: &FontContext,
  root_font_size: f32,
  initial_page_name: Option<String>,
) -> Vec<FragmentNode> {
  if rules.is_empty() {
    return vec![root.clone()];
  }

  let mut spans = Vec::new();
  collect_page_name_spans(root, 0.0, &mut spans);
  spans.sort_by(|a, b| {
    a.start
      .partial_cmp(&b.start)
      .unwrap_or(std::cmp::Ordering::Equal)
  });

  let mut forced = collect_forced_boundaries(root, 0.0);
  let total_height = root.bounding_box().height().max(fallback_page_size.height);
  forced.push(total_height);
  forced.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
  forced.dedup_by(|a, b| (*a - *b).abs() < 0.01);

  let mut pages = Vec::new();
  let mut pos = 0.0;
  let mut page_index = 0;

  while pos < total_height - 0.01 {
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

    let page_block = style.content_size.height.max(1.0);
    let mut end = (pos + page_block).min(total_height);
    if let Some(boundary) = forced
      .iter()
      .copied()
      .find(|b| *b > pos + 0.01 && *b < end - 0.01)
    {
      end = boundary;
    }

    if end <= pos + 0.01 {
      break;
    }

    let clipped = clip_node(root, pos, end, 0.0, pos, page_index, 0);
    let mut page_root = FragmentNode::new_block(
      Rect::from_xywh(0.0, 0.0, style.total_size.width, style.total_size.height),
      Vec::new(),
    );

    if let Some(mut content) = clipped {
      content.bounds = Rect::from_xywh(
        content.bounds.x(),
        content.bounds.y(),
        style.content_size.width,
        content.bounds.height(),
      );
      translate_fragment(&mut content, style.content_origin.x, style.content_origin.y);
      page_root.children.push(content);
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

fn collect_page_name_spans(node: &FragmentNode, abs_start: f32, spans: &mut Vec<PageNameSpan>) {
  let start = abs_start + node.bounds.y();
  let end = start + node.bounds.height();

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
  font_ctx: &FontContext,
) -> Vec<FragmentNode> {
  let mut fragments = Vec::new();
  let generator = ContentGenerator::new();
  let mut context = ContentContext::new();

  for (area, box_style) in &style.margin_boxes {
    if let Some(bounds) = margin_box_bounds(*area, style) {
      if bounds.width() <= 0.0 || bounds.height() <= 0.0 {
        continue;
      }

      let mut root_style: ComputedStyle = box_style.clone();
      root_style.width = Some(Length::px(bounds.width()));
      root_style.height = Some(Length::px(bounds.height()));
      root_style.box_sizing = BoxSizing::BorderBox;
      root_style.margin_top = Some(Length::px(0.0));
      root_style.margin_right = Some(Length::px(0.0));
      root_style.margin_bottom = Some(Length::px(0.0));
      root_style.margin_left = Some(Length::px(0.0));
      let root_style = Arc::new(root_style);

      let mut children = Vec::new();
      if ContentGenerator::is_text_only(&box_style.content_value) {
        let text = generator.generate(&box_style.content_value, &mut context);
        children.push(BoxNode::new_text(Arc::new(box_style.clone()), text));
      }

      let root_box = BoxNode::new_block(root_style.clone(), FormattingContextType::Block, children);
      let layout_config = LayoutConfig::new(Size::new(bounds.width(), bounds.height()));
      let engine = LayoutEngine::with_font_context(layout_config, font_ctx.clone());
      let constraints = LayoutConstraints::definite(bounds.width(), bounds.height());

      if let Ok(mut fragment) = engine.layout_subtree(&root_box, &constraints) {
        if fragment.style.is_none() {
          fragment.style = Some(root_style.clone());
        }
        translate_fragment(&mut fragment, bounds.x(), bounds.y());
        fragments.push(fragment);
      }
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
