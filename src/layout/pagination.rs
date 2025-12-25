//! Pagination helpers that honor CSS @page rules and margin boxes.

use std::sync::Arc;

use crate::css::types::CollectedPageRule;
use crate::geometry::{Rect, Size};
use crate::layout::engine::{LayoutConfig, LayoutEngine};
use crate::layout::fragmentation::{
  clip_node, collect_forced_boundaries, propagate_fragment_metadata,
};
use crate::style::content::{ContentContext, ContentItem, ContentValue, CounterStyle};
use crate::style::display::FormattingContextType;
use crate::style::page::{resolve_page_style, PageSide, ResolvedPageStyle};
use crate::style::ComputedStyle;
use crate::text::font_loader::FontContext;
use crate::tree::box_tree::{BoxNode, BoxTree, ReplacedType};
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
  let mut page_styles = Vec::new();
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

    page_styles.push(style.clone());
    pages.push(page_root);
    pos = end;
    page_index += 1;
  }

  if pages.is_empty() {
    return vec![root.clone()];
  }

  let count = pages.len();
  for (idx, (page, style)) in pages.iter_mut().zip(page_styles.iter()).enumerate() {
    page
      .children
      .extend(build_margin_box_fragments(style, font_ctx, idx, count));
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
  page_index: usize,
  page_count: usize,
) -> Vec<FragmentNode> {
  let mut fragments = Vec::new();

  for (area, box_style) in &style.margin_boxes {
    if let Some(bounds) = margin_box_bounds(*area, style) {
      if bounds.width() <= 0.0 || bounds.height() <= 0.0 {
        continue;
      }

      let style_arc = Arc::new(box_style.clone());
      let children = build_margin_box_children(box_style, page_index, page_count, &style_arc);
      let root = BoxNode::new_block(style_arc.clone(), FormattingContextType::Block, children);
      let box_tree = BoxTree::new(root);

      let config = LayoutConfig::new(Size::new(bounds.width(), bounds.height()));
      let engine = LayoutEngine::with_font_context(config, font_ctx.clone());
      if let Ok(mut tree) = engine.layout_tree(&box_tree) {
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
  style: &Arc<ComputedStyle>,
) -> Vec<BoxNode> {
  let mut children: Vec<BoxNode> = Vec::new();
  let mut context = ContentContext::new();
  context.set_quotes(box_style.quotes.clone());
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
              text_buf.push_str(&val);
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
            let replaced = BoxNode::new_replaced(
              style.clone(),
              ReplacedType::Image {
                src: url.clone(),
                alt: None,
                sizes: None,
                srcset: Vec::new(),
                picture_sources: Vec::new(),
              },
              None,
              None,
            );
            children.push(replaced);
          }
        }
      }
    }
    ContentValue::None | ContentValue::Normal => {}
  }

  flush_text(&mut text_buf, &mut children, style);

  children
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
