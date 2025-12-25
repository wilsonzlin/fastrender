//! Pagination helpers that honor CSS @page rules and margin boxes.

use std::sync::Arc;

use crate::css::types::CollectedPageRule;
use crate::geometry::{Rect, Size};
use crate::layout::fragmentation::{
  clip_node, collect_forced_boundaries, propagate_fragment_metadata,
};
use crate::style::content::{ContentContext, ContentGenerator};
use crate::style::page::{resolve_page_style, PageSide, ResolvedPageStyle};
use crate::style::ComputedStyle;
use crate::text::font_loader::FontContext;
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

  let page_plans = build_page_plans(
    &spans,
    &forced,
    total_height,
    rules,
    fallback_page_size,
    root_font_size,
    initial_page_name.as_deref(),
  );

  if page_plans.is_empty() {
    return vec![root.clone()];
  }

  let page_count = page_plans.len();
  let mut pages = Vec::with_capacity(page_count);

  for plan in &page_plans {
    let mut page_root = FragmentNode::new_block(
      Rect::from_xywh(
        0.0,
        0.0,
        plan.style.total_size.width,
        plan.style.total_size.height,
      ),
      Vec::new(),
    );

    if let Some(mut content) = clip_node(
      root,
      plan.start,
      plan.end,
      0.0,
      plan.start,
      plan.page_index,
      page_count,
    ) {
      content.bounds = Rect::from_xywh(
        content.bounds.x(),
        content.bounds.y(),
        plan.style.content_size.width,
        content.bounds.height(),
      );
      translate_fragment(
        &mut content,
        plan.style.content_origin.x,
        plan.style.content_origin.y,
      );
      page_root.children.push(content);
    }

    page_root.children.extend(build_margin_box_fragments(
      &plan.style,
      font_ctx,
      plan.page_index,
      page_count,
    ));

    pages.push(page_root);
  }

  for (idx, page) in pages.iter_mut().enumerate() {
    propagate_fragment_metadata(page, idx, page_count);
  }

  pages
}

#[derive(Debug, Clone)]
struct PagePlan {
  start: f32,
  end: f32,
  style: ResolvedPageStyle,
  page_index: usize,
}

fn build_page_plans(
  spans: &[PageNameSpan],
  forced: &[f32],
  total_height: f32,
  rules: &[CollectedPageRule<'_>],
  fallback_page_size: Size,
  root_font_size: f32,
  initial_page_name: Option<&str>,
) -> Vec<PagePlan> {
  let mut plans = Vec::new();
  let mut pos = 0.0;
  let mut page_index = 0;

  while pos < total_height - 0.01 {
    let page_name = page_name_for_position(spans, pos, initial_page_name);
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

    plans.push(PagePlan {
      start: pos,
      end,
      style,
      page_index,
    });

    pos = end;
    page_index += 1;
  }

  plans
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
  _font_ctx: &FontContext,
  page_index: usize,
  page_count: usize,
) -> Vec<FragmentNode> {
  let mut fragments = Vec::new();
  let generator = ContentGenerator::new();

  for (area, box_style) in &style.margin_boxes {
    if let Some(bounds) = margin_box_bounds(*area, style) {
      if bounds.width() <= 0.0 || bounds.height() <= 0.0 {
        continue;
      }

      let mut context = ContentContext::new();
      context.set_counter("page", (page_index + 1) as i32);
      context.set_counter("pages", page_count as i32);

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
