//! Block Formatting Context Layout
//!
//! This module implements the Block Formatting Context (BFC) layout algorithm
//! as specified in CSS 2.1 Section 9.4.1.
//!
//! # Block Formatting Context
//!
//! A BFC is a layout mode where block boxes are laid out vertically, one after
//! another, starting at the top of the containing block. The vertical distance
//! between boxes is determined by margins (which may collapse).
//!
//! # Key Features
//!
//! - **Vertical stacking**: Block boxes stack vertically
//! - **Full width**: By default, blocks stretch to fill containing block width
//! - **Margin collapsing**: Adjacent vertical margins collapse into one
//! - **Independent context**: Contents don't affect outside layout
//!
//! # Module Structure
//!
//! - `margin_collapse` - Margin collapsing algorithm (CSS 2.1 Section 8.3.1)
//! - `width` - Block width computation (CSS 2.1 Section 10.3.3)
//!
//! Reference: <https://www.w3.org/TR/CSS21/visuren.html#block-formatting>

pub mod margin_collapse;
pub mod width;

use crate::geometry::Point;
use crate::geometry::Rect;
use crate::geometry::Size;
use crate::layout::constraints::AvailableSpace;
use crate::layout::constraints::LayoutConstraints;
use crate::layout::contexts::block::width::MarginValue;
use crate::layout::contexts::factory::FormattingContextFactory;
use crate::layout::contexts::inline::InlineFormattingContext;
use crate::layout::contexts::positioned::ContainingBlock;
use crate::layout::contexts::positioned::PositionedLayout;
use crate::layout::float_context::FloatContext;
use crate::layout::float_context::FloatSide;
use crate::layout::formatting_context::count_block_intrinsic_call;
use crate::layout::formatting_context::intrinsic_cache_lookup;
use crate::layout::formatting_context::intrinsic_cache_store;
use crate::layout::formatting_context::FormattingContext;
use crate::layout::formatting_context::IntrinsicSizingMode;
use crate::layout::formatting_context::LayoutError;
use crate::layout::profile::layout_timer;
use crate::layout::profile::LayoutKind;
use crate::layout::utils::border_size_from_box_sizing;
use crate::layout::utils::compute_replaced_size;
use crate::layout::utils::content_size_from_box_sizing;
use crate::layout::utils::resolve_length_with_percentage;
use crate::layout::utils::resolve_length_with_percentage_metrics;
use crate::layout::utils::resolve_scrollbar_width;
use crate::style::block_axis_is_horizontal;
use crate::style::block_axis_positive;
use crate::style::display::Display;
use crate::style::display::FormattingContextType;
use crate::style::float::Float;
use crate::style::position::Position;
use crate::style::inline_axis_is_horizontal;
use crate::style::inline_axis_positive;
use crate::style::types::BorderStyle;
use crate::style::types::ColumnFill;
use crate::style::types::ColumnSpan;
use crate::style::types::Overflow;
use crate::style::values::Length;
use crate::style::ComputedStyle;
use crate::style::PhysicalSide;
use crate::text::font_loader::FontContext;
use crate::tree::box_tree::BoxNode;
use crate::tree::box_tree::BoxType;
use crate::tree::box_tree::ReplacedBox;
use crate::tree::fragment_tree::FragmentContent;
use crate::tree::fragment_tree::FragmentNode;
use margin_collapse::should_collapse_with_first_child;
use margin_collapse::should_collapse_with_last_child;
use margin_collapse::MarginCollapseContext;
use std::collections::HashMap;
use std::sync::Arc;
use std::sync::OnceLock;
use std::time::Instant;
use width::compute_block_width;

#[derive(Clone)]
struct PositionedCandidate {
  node: BoxNode,
  source: ContainingBlockSource,
  static_position: Option<Point>,
}

#[derive(Clone)]
enum ContainingBlockSource {
  ParentPadding,
  Explicit(ContainingBlock),
}

/// Helper to resolve a Length to pixels, handling em/rem units with font-size
fn resolve_length(
  length: &Length,
  font_size: f32,
  containing_block_size: f32,
  viewport: crate::geometry::Size,
) -> f32 {
  use crate::style::values::LengthUnit;
  match length.unit {
    LengthUnit::Em | LengthUnit::Rem => length.value * font_size,
    LengthUnit::Percent => (length.value / 100.0) * containing_block_size,
    _ if length.unit.is_absolute() => length.to_px(),
    _ if length.unit.is_viewport_relative() => length
      .resolve_with_viewport(viewport.width, viewport.height)
      .unwrap_or_else(|| length.to_px()),
    _ => 0.0, // Fallback for unknown units
  }
}

/// Helper to resolve an optional Length to pixels
fn resolve_opt_length(
  length: Option<&Length>,
  font_size: f32,
  containing_block_size: f32,
  viewport: crate::geometry::Size,
) -> f32 {
  length
    .map(|l| resolve_length(l, font_size, containing_block_size, viewport))
    .unwrap_or(0.0)
}

fn axis_sides(horizontal: bool, positive: bool) -> (PhysicalSide, PhysicalSide) {
  match (horizontal, positive) {
    (true, true) => (PhysicalSide::Left, PhysicalSide::Right),
    (true, false) => (PhysicalSide::Right, PhysicalSide::Left),
    (false, true) => (PhysicalSide::Top, PhysicalSide::Bottom),
    (false, false) => (PhysicalSide::Bottom, PhysicalSide::Top),
  }
}

fn inline_axis_sides(style: &ComputedStyle) -> (PhysicalSide, PhysicalSide) {
  axis_sides(
    inline_axis_is_horizontal(style.writing_mode),
    inline_axis_positive(style.writing_mode, style.direction),
  )
}

fn block_axis_sides(style: &ComputedStyle) -> (PhysicalSide, PhysicalSide) {
  axis_sides(
    block_axis_is_horizontal(style.writing_mode),
    block_axis_positive(style.writing_mode),
  )
}

/// Block Formatting Context implementation
///
/// Implements the FormattingContext trait for block-level layout.
/// Handles vertical stacking, margin collapsing, and width computation.
#[derive(Clone)]
pub struct BlockFormattingContext {
  font_context: FontContext,
  viewport_size: crate::geometry::Size,
  nearest_positioned_cb: ContainingBlock,
  /// When true, treat the root box as a flex item for width resolution (auto margins resolve to
  /// 0 and specified margins stay fixed instead of being rebalanced to satisfy the block width
  /// equation). This is only meant for the flex-item root; descendants revert to normal block
  /// behavior.
  flex_item_mode: bool,
}

impl BlockFormattingContext {
  /// Creates a new BlockFormattingContext
  pub fn new() -> Self {
    let viewport = crate::geometry::Size::new(800.0, 600.0);
    Self::with_font_context_viewport_and_cb(
      FontContext::new(),
      viewport,
      ContainingBlock::viewport(viewport),
    )
  }

  /// Creates a BlockFormattingContext backed by a specific font context so text
  /// measurement shares caches with the caller.
  pub fn with_font_context(font_context: FontContext) -> Self {
    let viewport = crate::geometry::Size::new(800.0, 600.0);
    Self::with_font_context_viewport_and_cb(
      font_context,
      viewport,
      ContainingBlock::viewport(viewport),
    )
  }

  pub fn with_font_context_and_viewport(
    font_context: FontContext,
    viewport_size: crate::geometry::Size,
  ) -> Self {
    let cb = ContainingBlock::viewport(viewport_size);
    Self::with_font_context_viewport_and_cb(font_context, viewport_size, cb)
  }

  pub fn with_font_context_viewport_and_cb(
    font_context: FontContext,
    viewport_size: crate::geometry::Size,
    nearest_positioned_cb: ContainingBlock,
  ) -> Self {
    Self {
      font_context,
      viewport_size,
      nearest_positioned_cb,
      flex_item_mode: false,
    }
  }

  /// Creates a BlockFormattingContext configured for laying out a flex item root. Margin
  /// resolution follows the flexbox hypothetical size rules (auto margins → 0; specified margins
  /// remain as authored).
  pub fn for_flex_item_with_font_context_viewport_and_cb(
    font_context: FontContext,
    viewport_size: crate::geometry::Size,
    nearest_positioned_cb: ContainingBlock,
  ) -> Self {
    Self {
      font_context,
      viewport_size,
      nearest_positioned_cb,
      flex_item_mode: true,
    }
  }

  /// Lays out a single block-level child and returns its fragment
  #[allow(clippy::cognitive_complexity)]
  fn layout_block_child(
    &self,
    parent: &BoxNode,
    child: &BoxNode,
    containing_width: f32,
    constraints: &LayoutConstraints,
    margin_ctx: &mut MarginCollapseContext,
    current_y: f32,
    nearest_positioned_cb: &ContainingBlock,
  ) -> Result<(FragmentNode, f32), LayoutError> {
    static DUMP_CHILD_Y: OnceLock<bool> = OnceLock::new();
    static LOG_WIDE_FLEX: OnceLock<bool> = OnceLock::new();
    static LOG_NARROW_FLEX: OnceLock<bool> = OnceLock::new();
    let dump_child_y = *DUMP_CHILD_Y.get_or_init(|| {
      std::env::var("FASTR_DUMP_CELL_CHILD_Y")
        .map(|v| v != "0" && !v.eq_ignore_ascii_case("false"))
        .unwrap_or(false)
    });
    let log_wide_flex = *LOG_WIDE_FLEX.get_or_init(|| {
      std::env::var("FASTR_LOG_WIDE_FLEX")
        .map(|v| v != "0" && !v.eq_ignore_ascii_case("false"))
        .unwrap_or(false)
    });
    if let BoxType::Replaced(replaced_box) = &child.box_type {
      return self.layout_replaced_child(
        child,
        replaced_box,
        containing_width,
        constraints,
        margin_ctx,
        current_y,
        nearest_positioned_cb,
      );
    }

    let style = &child.style;
    let font_size = style.font_size; // Get font-size for resolving em units
    let containing_height = constraints.height();

    // Handle block-axis margins (resolve em/rem units with font-size)
    let block_sides = block_axis_sides(style);
    let margin_top = resolve_margin_side(
      style,
      block_sides.0,
      containing_width,
      &self.font_context,
      self.viewport_size,
    );
    let margin_bottom = resolve_margin_side(
      style,
      block_sides.1,
      containing_width,
      &self.font_context,
      self.viewport_size,
    );

    // Add top margin to pending collapse set
    margin_ctx.push_margin(margin_top);

    // Resolve collapsed margin and get Y position
    let collapsed_margin = margin_ctx.resolve();
    let box_y = current_y + collapsed_margin;
    if dump_child_y && matches!(child.style.display, Display::Table) {
      eprintln!(
                "block child margins: display={:?} current_y={:.2} margin_top={:.2} collapsed={:.2} box_y={:.2}",
                child.style.display, current_y, margin_top, collapsed_margin, box_y
            );
    }

    // Create constraints for child layout
    let specified_height = style.height.as_ref().and_then(|h| {
      resolve_length_with_percentage(
        *h,
        containing_height,
        self.viewport_size,
        font_size,
        style.root_font_size,
      )
    });
    let child_height_space = specified_height
      .map(AvailableSpace::Definite)
      .unwrap_or(AvailableSpace::Indefinite);

    // Compute width using CSS 2.1 Section 10.3.3 algorithm
    let inline_sides = inline_axis_sides(style);
    let inline_positive = inline_axis_positive(style.writing_mode, style.direction);
    let mut computed_width = compute_block_width(
      style,
      containing_width,
      self.viewport_size,
      inline_sides,
      inline_positive,
    );
    static LOG_BLOCK_WIDE: OnceLock<bool> = OnceLock::new();
    if *LOG_BLOCK_WIDE.get_or_init(|| {
      std::env::var("FASTR_LOG_BLOCK_WIDE")
        .map(|v| v != "0")
        .unwrap_or(false)
    }) && computed_width.total_width() > containing_width + 0.5
    {
      let selector = child
        .debug_info
        .as_ref()
        .map(|d| d.to_selector())
        .unwrap_or_else(|| "<child>".to_string());
      eprintln!(
                "[block-wide] id={} selector={} containing_w={:.1} content_w={:.1} total_w={:.1} width_decl={:?} min_w={:?} max_w={:?} margins=({:.1},{:.1})",
                child.id,
                selector,
                containing_width,
                computed_width.content_width,
                computed_width.total_width(),
                style.width,
                style.min_width,
                style.max_width,
                computed_width.margin_left,
                computed_width.margin_right,
            );
    }
    if style.width.is_none() {
      if let (crate::style::types::AspectRatio::Ratio(ratio), Some(h)) =
        (style.aspect_ratio, specified_height)
      {
        if ratio > 0.0 {
          computed_width.content_width = h * ratio;
        }
      }
    }

    if style.shrink_to_fit_inline_size && style.width.is_none() {
      let horizontal_edges = computed_width.border_left
        + computed_width.padding_left
        + computed_width.padding_right
        + computed_width.border_right;
      let margin_left = style
        .margin_left
        .as_ref()
        .map(|l| {
          resolve_length_for_width(
            *l,
            containing_width,
            style,
            &self.font_context,
            self.viewport_size,
          )
        })
        .unwrap_or(0.0);
      let margin_right = style
        .margin_right
        .as_ref()
        .map(|l| {
          resolve_length_for_width(
            *l,
            containing_width,
            style,
            &self.font_context,
            self.viewport_size,
          )
        })
        .unwrap_or(0.0);

      let factory = FormattingContextFactory::with_font_context_viewport_and_cb(
        self.font_context.clone(),
        self.viewport_size,
        *nearest_positioned_cb,
      );
      let fc_type = child
        .formatting_context()
        .unwrap_or(FormattingContextType::Block);
      let fc = factory.create(fc_type);
      let preferred_min_content =
        fc.compute_intrinsic_inline_size(child, IntrinsicSizingMode::MinContent)?;
      let preferred_content = fc
        .compute_intrinsic_inline_size(child, IntrinsicSizingMode::MaxContent)
        .unwrap_or(preferred_min_content);

      let preferred_min = preferred_min_content + horizontal_edges;
      let preferred = preferred_content + horizontal_edges;
      let available = (containing_width - margin_left - margin_right).max(0.0);
      let shrink_border_box = preferred.min(available.max(preferred_min));
      let shrink_content = (shrink_border_box - horizontal_edges).max(0.0);
      let (margin_left, margin_right) = recompute_margins_for_width(
        style,
        containing_width,
        shrink_content,
        computed_width.border_left,
        computed_width.padding_left,
        computed_width.padding_right,
        computed_width.border_right,
        self.viewport_size,
        &self.font_context,
      );
      computed_width.content_width = shrink_content;
      computed_width.margin_left = margin_left;
      computed_width.margin_right = margin_right;
    }

    let child_constraints = LayoutConstraints::new(
      AvailableSpace::Definite(computed_width.content_width),
      child_height_space,
    );

    // Check if this child establishes a different formatting context
    let fc_type = child.formatting_context();
    static LOG_FLEX_CHILD: OnceLock<bool> = OnceLock::new();
    static LOG_FLEX_CHILD_IDS: OnceLock<Vec<usize>> = OnceLock::new();
    let log_flex_child = *LOG_FLEX_CHILD.get_or_init(|| {
      std::env::var("FASTR_LOG_FLEX_CHILD")
        .map(|v| v != "0")
        .unwrap_or(false)
    });
    let log_flex_child_ids = LOG_FLEX_CHILD_IDS.get_or_init(|| {
      std::env::var("FASTR_LOG_FLEX_CHILD_IDS")
        .ok()
        .map(|s| {
          s.split(',')
            .filter_map(|tok| tok.trim().parse::<usize>().ok())
            .collect()
        })
        .unwrap_or_default()
    });

    if matches!(
      fc_type,
      Some(FormattingContextType::Flex | FormattingContextType::Grid)
    ) {
      if log_flex_child || log_flex_child_ids.contains(&child.id) {
        let child_selector = child
          .debug_info
          .as_ref()
          .map(|d| d.to_selector())
          .unwrap_or_else(|| "<child>".to_string());
        eprintln!(
                    "[flex-child-constraint] parent_id={} child_id={} child_sel={} containing={:.1} content_w={:.1} total_w={:.1} constraint_w={:?} margins=({:.1},{:.1}) width={:?} min_w={:?} max_w={:?} viewport_w={:.1} style_margins=({:?},{:?}) parent_style_width={:?} parent_min_w={:?} parent_max_w={:?}",
                    parent.id,
                    child.id,
                    child_selector,
                    containing_width,
                    computed_width.content_width,
                    computed_width.total_width(),
                    child_constraints.width(),
                    computed_width.margin_left,
                    computed_width.margin_right,
                    child.style.width,
                    child.style.min_width,
                    child.style.max_width,
                    self.viewport_size.width,
                    child.style.margin_left,
                    child.style.margin_right,
                    parent.style.width,
                    parent.style.min_width,
                    parent.style.max_width,
                );
      }
      if log_wide_flex {
        let content_w = computed_width.content_width;
        let total_w = computed_width.total_width();
        let constraint_w = child_constraints.width();
        if content_w > self.viewport_size.width + 0.5
          || total_w > self.viewport_size.width + 0.5
          || constraint_w
            .map(|w| w > self.viewport_size.width + 0.5)
            .unwrap_or(false)
          || content_w > containing_width + 0.5
          || total_w > containing_width + 0.5
        {
          let selector = child
            .debug_info
            .as_ref()
            .map(|d| d.to_selector())
            .unwrap_or_else(|| "<anonymous>".to_string());
          eprintln!(
                        "[flex-constraint-wide] parent_id={} child_id={:?} selector={} containing={:.1} content_w={:.1} total_w={:.1} constraint_w={:?} margins=({:.1},{:.1}) width={:?} min_w={:?} max_w={:?} viewport_w={:.1}",
                        parent.id,
                        child.id,
                        selector,
                        containing_width,
                        content_w,
                        total_w,
                    constraint_w,
                    computed_width.margin_left,
                    computed_width.margin_right,
                    child.style.width,
                    child.style.min_width,
                    child.style.max_width,
                    self.viewport_size.width,
                );
        }
      }
      if *LOG_NARROW_FLEX.get_or_init(|| {
        std::env::var("FASTR_LOG_NARROW_FLEX")
          .map(|v| v != "0")
          .unwrap_or(false)
      }) && computed_width.content_width < 150.0
      {
        // Compute how much auto margins and percentage padding/borders left for content.
        let horiz_edges = computed_width.border_left
          + computed_width.padding_left
          + computed_width.padding_right
          + computed_width.border_right;
        let selector = child
          .debug_info
          .as_ref()
          .map(|d| d.to_selector())
          .unwrap_or_else(|| "<anonymous>".to_string());
        eprintln!(
                    "[flex-constraint-narrow] child_id={:?} selector={} containing={:.1} content_w={:.1} total_w={:.1} constraint_w={:?} margins=({:.1},{:.1}) width={:?} min_w={:?} max_w={:?} viewport_w={:.1} edges={:.1} auto_width={:?}",
                    child.id,
                    selector,
                    containing_width,
                    computed_width.content_width,
                    computed_width.total_width(),
                    child_constraints.width(),
                    computed_width.margin_left,
                    computed_width.margin_right,
                    child.style.width,
                    child.style.min_width,
                    child.style.max_width,
                    self.viewport_size.width,
                    horiz_edges,
                    child.style.width.is_none(),
                );
      }
    }

    let (mut child_fragments, content_height, positioned_children) = if let Some(fc_type) = fc_type
    {
      if fc_type != FormattingContextType::Block {
        // Child establishes a non-block FC - use the appropriate FC
        let factory = FormattingContextFactory::with_font_context_viewport_and_cb(
          self.font_context.clone(),
          self.viewport_size,
          *nearest_positioned_cb,
        );
        let fc = factory.create(fc_type);

        let log_skinny = std::env::var("FASTR_LOG_SKINNY_FLEX")
          .map(|v| v != "0")
          .unwrap_or(false);
        if log_skinny && computed_width.content_width <= 1.0 {
          let selector = child
            .debug_info
            .as_ref()
            .map(|d| d.to_selector())
            .unwrap_or_else(|| "<anon>".to_string());
          eprintln!(
                        "[skinny-flex-constraint] id={} selector={} fc={:?} containing_w={:.1} width={:.1} margins=({:.1},{:.1}) min_w={:?} max_w={:?}",
                        child.id,
                        selector,
                        fc_type,
                        containing_width,
                        computed_width.content_width,
                        computed_width.margin_left,
                        computed_width.margin_right,
                        child.style.min_width,
                        child.style.max_width
                    );
        }

        // Layout using the child's FC
        let child_frag = fc.layout(child, &child_constraints)?;
        let height = child_frag.bounds.height();

        // Return fragment wrapped in vec and height
        (vec![child_frag], height, Vec::new())
      } else {
        // Block FC - layout children normally
        self.layout_children(child, &child_constraints, nearest_positioned_cb)?
      }
    } else {
      // No FC (inline, text, etc.) - layout children normally
      self.layout_children(child, &child_constraints, nearest_positioned_cb)?
    };

    // Compute height (resolve em/rem units with font-size)
    let border_top = resolve_border_side(
      style,
      block_sides.0,
      containing_width,
      &self.font_context,
      self.viewport_size,
    );
    let border_bottom = resolve_border_side(
      style,
      block_sides.1,
      containing_width,
      &self.font_context,
      self.viewport_size,
    );
    let mut padding_top = resolve_padding_side(
      style,
      block_sides.0,
      containing_width,
      &self.font_context,
      self.viewport_size,
    );
    let mut padding_bottom = resolve_padding_side(
      style,
      block_sides.1,
      containing_width,
      &self.font_context,
      self.viewport_size,
    );
    let reserve_horizontal_gutter = matches!(style.overflow_x, Overflow::Scroll)
      || (style.scrollbar_gutter.stable
        && matches!(style.overflow_x, Overflow::Auto | Overflow::Scroll));
    if reserve_horizontal_gutter {
      let gutter = resolve_scrollbar_width(style);
      if style.scrollbar_gutter.both_edges {
        padding_top += gutter;
      }
      padding_bottom += gutter;
    }

    // Height computation (CSS 2.1 Section 10.6.3) with aspect-ratio adjustment (CSS Sizing L4)
    let mut height = specified_height.unwrap_or(content_height);
    if specified_height.is_none() {
      if let crate::style::types::AspectRatio::Ratio(ratio) = style.aspect_ratio {
        if ratio > 0.0 && computed_width.content_width.is_finite() {
          let ratio_height = computed_width.content_width / ratio;
          // Do not shrink below content-based height
          height = height.max(ratio_height);
        }
      }
    }

    // Apply min/max height constraints
    let min_height = style
      .min_height
      .as_ref()
      .and_then(|l| {
        resolve_length_with_percentage(
          *l,
          containing_height,
          self.viewport_size,
          font_size,
          style.root_font_size,
        )
      })
      .unwrap_or(0.0);
    let max_height = style
      .max_height
      .as_ref()
      .and_then(|l| {
        resolve_length_with_percentage(
          *l,
          containing_height,
          self.viewport_size,
          font_size,
          style.root_font_size,
        )
      })
      .unwrap_or(f32::INFINITY);
    let max_height = if max_height.is_finite() && max_height < min_height {
      min_height
    } else {
      max_height
    };
    let height = crate::layout::utils::clamp_with_order(height, min_height, max_height);

    // Create the fragment
    let box_height = border_top + padding_top + height + padding_bottom + border_bottom;
    let box_width = computed_width.border_box_width();

    // Layout out-of-flow positioned children against this block's padding box.
    if !positioned_children.is_empty() {
      let abs = crate::layout::absolute_positioning::AbsoluteLayout::with_font_context(
        self.font_context.clone(),
      );
      let cb_block_base = if specified_height.is_some() {
        Some(height + padding_top + padding_bottom)
      } else {
        None
      };
      let padding_origin = Point::new(
        computed_width.border_left + computed_width.padding_left,
        border_top + padding_top,
      );
      let padding_size = Size::new(
        computed_width.content_width + computed_width.padding_left + computed_width.padding_right,
        height + padding_top + padding_bottom,
      );
      let padding_rect = Rect::new(padding_origin, padding_size);
      let parent_padding_cb = ContainingBlock::with_viewport_and_bases(
        padding_rect,
        self.viewport_size,
        Some(padding_size.width),
        cb_block_base,
      );

      for PositionedCandidate {
        node: pos_child,
        source,
        static_position,
      } in positioned_children
      {
        let original_style = pos_child.style.clone();
        if trace_positioned_ids().contains(&pos_child.id) {
          eprintln!(
                        "[block-positioned-layout] parent_id={} child_id={} padding_rect=({:.1},{:.1},{:.1},{:.1})",
                        parent.id,
                        pos_child.id,
                        padding_rect.x(),
                        padding_rect.y(),
                        padding_rect.width(),
                        padding_rect.height()
                    );
        }
        let cb = match source {
          ContainingBlockSource::ParentPadding => parent_padding_cb,
          ContainingBlockSource::Explicit(cb) => cb,
        };
        let factory = FormattingContextFactory::with_font_context_viewport_and_cb(
          self.font_context.clone(),
          self.viewport_size,
          cb,
        );
        // Layout the child as if it were in normal flow to obtain its intrinsic size.
        let mut layout_child = pos_child.clone();
        let mut style = (*layout_child.style).clone();
        style.position = Position::Relative;
        style.top = None;
        style.right = None;
        style.bottom = None;
        style.left = None;
        layout_child.style = Arc::new(style);

        let fc_type = layout_child
          .formatting_context()
          .unwrap_or(FormattingContextType::Block);
        let fc = factory.create(fc_type);
        let height_available = cb.block_percentage_base().or(cb_block_base);
        let child_height_space = height_available
          .map(AvailableSpace::Definite)
          .unwrap_or(AvailableSpace::Indefinite);
        let child_constraints = LayoutConstraints::new(
          AvailableSpace::Definite(padding_size.width),
          child_height_space,
        );
        let mut child_fragment = fc.layout(&layout_child, &child_constraints)?;

        // Resolve positioned style against the containing block.
        let positioned_style = crate::layout::absolute_positioning::resolve_positioned_style(
          &pos_child.style,
          &cb,
          self.viewport_size,
          &self.font_context,
        );

        // Static position starts at the containing block origin; AbsoluteLayout will add
        // padding/border offsets, so use the content origin when no flow position was
        // recorded.
        let static_pos = static_position.unwrap_or(Point::ZERO);
        let preferred_min_inline = fc
          .compute_intrinsic_inline_size(&layout_child, IntrinsicSizingMode::MinContent)
          .ok();
        let preferred_inline = fc
          .compute_intrinsic_inline_size(&layout_child, IntrinsicSizingMode::MaxContent)
          .ok();
        let preferred_min_block = fc
          .compute_intrinsic_block_size(&layout_child, IntrinsicSizingMode::MinContent)
          .ok();
        let preferred_block = fc
          .compute_intrinsic_block_size(&layout_child, IntrinsicSizingMode::MaxContent)
          .ok();

        let mut input = crate::layout::absolute_positioning::AbsoluteLayoutInput::new(
          positioned_style,
          child_fragment.bounds.size,
          static_pos,
        );
        input.is_replaced = pos_child.is_replaced();
        input.preferred_min_inline_size = preferred_min_inline;
        input.preferred_inline_size = preferred_inline;
        input.preferred_min_block_size = preferred_min_block;
        input.preferred_block_size = preferred_block;

        let result = abs.layout_absolute(&input, &cb)?;
        let needs_relayout = (result.size.width - child_fragment.bounds.width()).abs() > 0.01
          || (result.size.height - child_fragment.bounds.height()).abs() > 0.01;
        if needs_relayout {
          let relayout_constraints = LayoutConstraints::new(
            AvailableSpace::Definite(result.size.width),
            AvailableSpace::Definite(result.size.height),
          );
          let mut relayout_child = layout_child.clone();
          let mut relayout_style = (*relayout_child.style).clone();
          relayout_style.width = Some(crate::style::values::Length::px(result.size.width));
          relayout_style.height = Some(crate::style::values::Length::px(result.size.height));
          relayout_child.style = Arc::new(relayout_style);
          child_fragment = fc.layout(&relayout_child, &relayout_constraints)?;
        }
        child_fragment.bounds = Rect::new(result.position, result.size);
        child_fragment.style = Some(original_style);
        if trace_positioned_ids().contains(&pos_child.id) {
          let (text_count, total) = count_text_fragments(&child_fragment);
          let mut snippets = Vec::new();
          collect_first_texts(&child_fragment, &mut snippets, 3);
          eprintln!(
                        "[block-positioned-placed] child_id={} pos=({:.1},{:.1}) size=({:.1},{:.1}) texts={}/{} first_texts={:?}",
                        pos_child.id,
                        result.position.x,
                        result.position.y,
                        result.size.width,
                        result.size.height,
                        text_count,
                        total,
                        snippets
                    );
        }
        child_fragments.push(child_fragment);
      }
    }

    let bounds = Rect::from_xywh(computed_width.margin_left, box_y, box_width, box_height);

    let fragment = FragmentNode::new_with_style(
      bounds,
      crate::tree::fragment_tree::FragmentContent::Block {
        box_id: Some(child.id),
      },
      child_fragments,
      child.style.clone(),
    );

    // Push bottom margin for next collapse
    margin_ctx.push_margin(margin_bottom);

    // Return fragment and the Y position after this box (before margin)
    let next_y = box_y + box_height;

    Ok((fragment, next_y))
  }

  fn layout_replaced_child(
    &self,
    child: &BoxNode,
    replaced_box: &ReplacedBox,
    containing_width: f32,
    constraints: &LayoutConstraints,
    margin_ctx: &mut MarginCollapseContext,
    current_y: f32,
    _nearest_positioned_cb: &ContainingBlock,
  ) -> Result<(FragmentNode, f32), LayoutError> {
    let style = &child.style;
    let font_size = style.font_size;

    // Percentages on replaced elements resolve against the containing block size (width/height
    // when available). Even if the block height is indefinite, we still have a valid width
    // percentage base, which allows max-width: 100% (UA default) to clamp oversized images.
    let percentage_base = Some(crate::geometry::Size::new(
      containing_width,
      constraints.height().unwrap_or(f32::NAN),
    ));
    static LOG_WIDE_FLEX: OnceLock<bool> = OnceLock::new();
    let mut used_size =
      compute_replaced_size(style, replaced_box, percentage_base, self.viewport_size);
    // As a final guard, honor resolved min/max constraints against the containing block width/height.
    let resolved_max_w = style.max_width.as_ref().map(|l| {
      resolve_length_for_width(
        *l,
        containing_width,
        style,
        &self.font_context,
        self.viewport_size,
      )
    });
    if let Some(max_w) = resolved_max_w {
      used_size.width = used_size.width.min(max_w);
    }
    let resolved_min_w = style.min_width.as_ref().map(|l| {
      resolve_length_for_width(
        *l,
        containing_width,
        style,
        &self.font_context,
        self.viewport_size,
      )
    });
    if let Some(min_w) = resolved_min_w {
      used_size.width = used_size.width.max(min_w);
    }
    let log_wide_flex = *LOG_WIDE_FLEX.get_or_init(|| {
      std::env::var("FASTR_LOG_WIDE_FLEX")
        .map(|v| v != "0" && !v.eq_ignore_ascii_case("false"))
        .unwrap_or(false)
    });
    if log_wide_flex && used_size.width > containing_width + 0.5 {
      let selector = child
        .debug_info
        .as_ref()
        .map(|d| d.to_selector())
        .unwrap_or_else(|| "<anonymous>".to_string());
      eprintln!(
                "[replaced-wide] child_id={:?} selector={} used_w={:.1} used_h={:.1} containing_w={:.1} max_w={:?} min_w={:?}",
                child.id,
                selector,
                used_size.width,
                used_size.height,
                containing_width,
                resolved_max_w,
                resolved_min_w
            );
    }

    // Vertical margins collapse as normal blocks
    let margin_top = resolve_opt_length(
      style.margin_top.as_ref(),
      font_size,
      containing_width,
      self.viewport_size,
    );
    let margin_bottom = resolve_opt_length(
      style.margin_bottom.as_ref(),
      font_size,
      containing_width,
      self.viewport_size,
    );
    margin_ctx.push_margin(margin_top);
    let collapsed_margin = margin_ctx.resolve();
    let box_y = current_y + collapsed_margin;

    // Resolve padding and borders
    let mut padding_top = resolve_length_for_width(
      style.padding_top,
      containing_width,
      style,
      &self.font_context,
      self.viewport_size,
    );
    let mut padding_bottom = resolve_length_for_width(
      style.padding_bottom,
      containing_width,
      style,
      &self.font_context,
      self.viewport_size,
    );
    let reserve_horizontal_gutter = matches!(style.overflow_x, Overflow::Scroll)
      || (style.scrollbar_gutter.stable
        && matches!(style.overflow_x, Overflow::Auto | Overflow::Scroll));
    if reserve_horizontal_gutter {
      let gutter = resolve_scrollbar_width(style);
      if style.scrollbar_gutter.both_edges {
        padding_top += gutter;
      }
      padding_bottom += gutter;
    }

    let border_top = resolve_length_for_width(
      style.border_top_width,
      containing_width,
      style,
      &self.font_context,
      self.viewport_size,
    );
    let border_bottom = resolve_length_for_width(
      style.border_bottom_width,
      containing_width,
      style,
      &self.font_context,
      self.viewport_size,
    );

    // Use the resolved replaced width when computing horizontal metrics
    let mut width_style = (*style).as_ref().clone();
    width_style.width = Some(Length::px(used_size.width));
    width_style.box_sizing = crate::style::types::BoxSizing::ContentBox;
    let inline_sides = inline_axis_sides(style);
    let inline_positive = inline_axis_positive(style.writing_mode, style.direction);
    let computed_width = compute_block_width(
      &width_style,
      containing_width,
      self.viewport_size,
      inline_sides,
      inline_positive,
    );

    let box_width = computed_width.border_box_width();
    let box_height = border_top + padding_top + used_size.height + padding_bottom + border_bottom;
    let bounds = Rect::from_xywh(computed_width.margin_left, box_y, box_width, box_height);

    let fragment = FragmentNode::new_with_style(
      bounds,
      FragmentContent::Replaced {
        replaced_type: replaced_box.replaced_type.clone(),
        box_id: None,
      },
      vec![],
      child.style.clone(),
    );

    margin_ctx.push_margin(margin_bottom);
    let next_y = box_y + box_height;

    Ok((fragment, next_y))
  }

  /// Lays out all children of a box
  #[allow(clippy::cognitive_complexity)]
  fn layout_children(
    &self,
    parent: &BoxNode,
    constraints: &LayoutConstraints,
    nearest_positioned_cb: &ContainingBlock,
  ) -> Result<(Vec<FragmentNode>, f32, Vec<PositionedCandidate>), LayoutError> {
    static DUMP_CELL_CHILD_Y: OnceLock<bool> = OnceLock::new();
    let dump_cell_child_y = *DUMP_CELL_CHILD_Y.get_or_init(|| {
      std::env::var("FASTR_DUMP_CELL_CHILD_Y")
        .map(|v| v != "0" && !v.eq_ignore_ascii_case("false"))
        .unwrap_or(false)
    });
    let mut fragments = Vec::new();
    let mut current_y: f32 = 0.0;
    let mut content_height: f32 = 0.0;
    let mut margin_ctx = MarginCollapseContext::new();
    let mut inline_buffer: Vec<BoxNode> = Vec::new();
    let mut positioned_children: Vec<PositionedCandidate> = Vec::new();
    let collapse_with_parent_top = should_collapse_with_first_child(&parent.style);
    let establishes_absolute_cb = parent.style.position.is_positioned()
      || !parent.style.transform.is_empty()
      || parent.style.perspective.is_some();
    let establishes_fixed_cb = !parent.style.transform.is_empty() || parent.style.perspective.is_some();
    if !collapse_with_parent_top {
      margin_ctx.mark_content_encountered();
    }
    static TRACE_ENV_RAW_LOGGED: OnceLock<bool> = OnceLock::new();
    if let Ok(val) = std::env::var("FASTR_TRACE_BOXES") {
      TRACE_ENV_RAW_LOGGED.get_or_init(|| {
        eprintln!("[trace-box-env-raw] {}", val);
        true
      });
    }
    let trace_boxes: Vec<usize> = std::env::var("FASTR_TRACE_BOXES")
      .ok()
      .and_then(|s| {
        let ids: Vec<_> = s
          .split(',')
          .filter_map(|p| p.trim().parse::<usize>().ok())
          .collect();
        if ids.is_empty() {
          None
        } else {
          Some(ids)
        }
      })
      .unwrap_or_default();
    static TRACE_BOXES_LOGGED: OnceLock<bool> = OnceLock::new();
    if !trace_boxes.is_empty() {
      TRACE_BOXES_LOGGED.get_or_init(|| {
        eprintln!("[trace-box-env] ids={:?}", trace_boxes);
        true
      });
    }
    let progress_ms = std::env::var("FASTR_LOG_BLOCK_PROGRESS_MS")
      .ok()
      .and_then(|v| v.parse().ok())
      .unwrap_or(0);
    let progress_ids = std::env::var("FASTR_LOG_BLOCK_PROGRESS_IDS")
      .ok()
      .and_then(|s| {
        let ids: Vec<_> = s
          .split(',')
          .filter_map(|tok| tok.trim().parse::<usize>().ok())
          .collect();
        if ids.is_empty() {
          None
        } else {
          Some(ids)
        }
      });
    let progress_match = std::env::var("FASTR_LOG_BLOCK_PROGRESS_MATCH")
      .ok()
      .and_then(|s| {
        let subs: Vec<_> = s
          .split(',')
          .filter_map(|tok| {
            let trimmed = tok.trim();
            if trimmed.is_empty() {
              None
            } else {
              Some(trimmed.to_string())
            }
          })
          .collect();
        if subs.is_empty() {
          None
        } else {
          Some(subs)
        }
      });
    let filters_set = progress_ids.is_some() || progress_match.is_some();
    let passes_filters = |node: &BoxNode| -> bool {
      let id_ok = progress_ids
        .as_ref()
        .map(|ids| ids.contains(&node.id))
        .unwrap_or(false);
      let match_ok = progress_match
        .as_ref()
        .map(|subs| {
          subs.iter().any(|sub| {
            node
              .debug_info
              .as_ref()
              .map(|d| d.to_selector().contains(sub))
              .unwrap_or(false)
          })
        })
        .unwrap_or(false);
      if !filters_set {
        true
      } else {
        id_ok || match_ok
      }
    };
    let should_log_progress = progress_ms > 0 && passes_filters(parent);
    let progress_ms = if should_log_progress { progress_ms } else { 0 };
    let progress_max = if progress_ms > 0 {
      std::env::var("FASTR_LOG_BLOCK_PROGRESS_MAX")
        .ok()
        .and_then(|v| v.parse::<u32>().ok())
        .unwrap_or(10)
    } else {
      0
    };
    static TOTAL_CAP: OnceLock<Option<u32>> = OnceLock::new();
    static TOTAL_COUNT: OnceLock<std::sync::atomic::AtomicU32> = OnceLock::new();
    let total_cap = TOTAL_CAP
      .get_or_init(|| {
        std::env::var("FASTR_LOG_BLOCK_PROGRESS_TOTAL_MAX")
          .ok()
          .and_then(|v| v.parse::<u32>().ok())
          .or(Some(50))
      })
      .to_owned();
    let total_counter = TOTAL_COUNT.get_or_init(|| std::sync::atomic::AtomicU32::new(0));

    let within_total_cap = total_cap
      .map(|cap| total_counter.load(std::sync::atomic::Ordering::Relaxed) < cap)
      .unwrap_or(true);

    if progress_ms > 0 && within_total_cap {
      eprintln!(
        "[block-progress-start] parent_id={} children={} threshold_ms={}",
        parent.id,
        parent.children.len(),
        progress_ms
      );
      if total_cap.is_some() {
        total_counter.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
      }
    }
    let parent_selector = if progress_ms > 0 {
      parent
        .debug_info
        .as_ref()
        .map(|d| d.to_selector())
        .unwrap_or_else(|| "<anon>".to_string())
    } else {
      String::new()
    };
    let progress_start = Instant::now();
    let mut progress_last = if progress_ms > 0 {
      progress_start
        .checked_sub(std::time::Duration::from_millis(
          progress_ms.min(u128::from(u64::MAX)) as u64,
        ))
        .unwrap_or(progress_start)
    } else {
      progress_start
    };
    let mut progress_count: u32 = 0;
    let mut progress_capped = false;

    // Get containing width from constraints, but guard against collapsed/indefinite widths that
    // would zero out percentage sizing for descendants. Mirror the root-width fallback used in
    // `layout` so children still see a usable containing block when the parent was laid out with
    // a near-zero available width (common when flex measurement feeds 0px constraints). When the
    // available inline size is intrinsic/indefinite (min-/max-content probes), avoid inflating
    // the base to the viewport — leave it at 0 unless the caller provided a definite percentage
    // base.
    let intrinsic_width = matches!(
      constraints.available_width,
      AvailableSpace::MinContent | AvailableSpace::MaxContent | AvailableSpace::Indefinite
    );
    let containing_width = constraints
      .inline_percentage_base
      .or_else(|| constraints.width())
      .map(|w| w.min(self.viewport_size.width));
    let mut containing_width = containing_width.unwrap_or(if intrinsic_width {
      0.0
    } else {
      self.viewport_size.width
    });
    if !intrinsic_width && containing_width <= 1.0 {
      let width_is_absolute = parent
        .style
        .width
        .as_ref()
        .map(|l| l.unit.is_absolute())
        .unwrap_or(false);
      if !width_is_absolute {
        containing_width = self.viewport_size.width;
      }
    }
    let mut float_ctx = FloatContext::new(containing_width);
    let available_height = constraints.available_height;
    let relative_cb = ContainingBlock::with_viewport(
      Rect::new(
        Point::ZERO,
        Size::new(containing_width, constraints.height().unwrap_or(0.0)),
      ),
      self.viewport_size,
    );
    // Check for border/padding that prevents margin collapse with first child
    let parent_has_top_separation = resolve_length_for_width(
      parent.style.border_top_width,
      containing_width,
      &parent.style,
      &self.font_context,
      self.viewport_size,
    ) > 0.0
      || resolve_length_for_width(
        parent.style.padding_top,
        containing_width,
        &parent.style,
        &self.font_context,
        self.viewport_size,
      ) > 0.0;

    if parent_has_top_separation {
      margin_ctx.mark_content_encountered();
    }

    let flush_inline_buffer = |buffer: &mut Vec<BoxNode>,
                               fragments: &mut Vec<FragmentNode>,
                               current_y: &mut f32,
                               content_height: &mut f32,
                               margin_ctx: &mut MarginCollapseContext,
                               float_ctx_ref: &mut FloatContext|
     -> Result<(), LayoutError> {
      if buffer.is_empty() {
        return Ok(());
      }

      // If the buffer contains any block-level boxes (or only collapsible whitespace),
      // lay each out separately to avoid creating an inline formatting context that spans
      // mixed block content or empty lines.
      let has_block = buffer.iter().any(|b| b.is_block_level());
      let all_whitespace = buffer.iter().all(|b| match &b.box_type {
        BoxType::Text(text) => text.text.trim().is_empty(),
        _ => false,
      });
      if has_block || all_whitespace {
        for child in buffer.drain(..) {
          let pending_margin = margin_ctx.consume_pending();
          *current_y += pending_margin;
          let (fragment, next_y) = self.layout_block_child(
            parent,
            &child,
            containing_width,
            constraints,
            margin_ctx,
            *current_y,
            nearest_positioned_cb,
          )?;
          *content_height = content_height.max(fragment.bounds.max_y());
          *current_y = next_y;
          let mut fragment = fragment;
          if child.style.position.is_relative() {
            let positioned_style = crate::layout::absolute_positioning::resolve_positioned_style(
              &child.style,
              &relative_cb,
              self.viewport_size,
              &self.font_context,
            );
            fragment = PositionedLayout::new().apply_relative_positioning(
              &fragment,
              &positioned_style,
              &relative_cb,
            )?;
          }
          fragments.push(fragment);
        }
        return Ok(());
      }

      // Apply any pending collapsed margin before inline content
      let pending_margin = margin_ctx.consume_pending();
      *current_y += pending_margin;

      let inline_container = BoxNode::new_inline(parent.style.clone(), buffer.clone());
      // If the inline container would start below the current cursor because of pending
      // margins, advance to that baseline first.
      let inline_y = *current_y;
      let inline_fc = InlineFormattingContext::with_font_context_viewport_and_cb(
        self.font_context.clone(),
        self.viewport_size,
        *nearest_positioned_cb,
      );
      let inline_constraints =
        LayoutConstraints::new(AvailableSpace::Definite(containing_width), available_height);
      let mut inline_fragment = inline_fc.layout_with_floats(
        &inline_container,
        &inline_constraints,
        Some(float_ctx_ref),
        inline_y,
      )?;

      inline_fragment.bounds = Rect::from_xywh(
        0.0,
        inline_y,
        inline_fragment.bounds.width(),
        inline_fragment.bounds.height(),
      );

      *content_height = content_height.max(inline_fragment.bounds.max_y());
      *current_y += inline_fragment.bounds.height();
      fragments.push(inline_fragment);
      buffer.clear();
      Ok(())
    };

    for (child_idx, child) in parent.children.iter().enumerate() {
      if progress_ms > 0 {
        if let Some(cap) = total_cap {
          let current = total_counter.load(std::sync::atomic::Ordering::Relaxed);
          if current >= cap {
            continue;
          }
        }
        if progress_count < progress_max || progress_max == 0 {
          let now = Instant::now();
          if now.duration_since(progress_last).as_millis() >= progress_ms {
            let child_selector = child
              .debug_info
              .as_ref()
              .map(|d| d.to_selector())
              .unwrap_or_else(|| "<anon>".to_string());
            eprintln!(
                            "[block-progress] parent_id={} child={}/{} elapsed_ms={} selector={} child_selector={}",
                            parent.id,
                            child_idx,
                            parent.children.len(),
                            now.duration_since(progress_start).as_millis(),
                            parent_selector,
                            child_selector
                        );
            progress_last = now;
            if progress_max > 0 {
              progress_count += 1;
            }
            if total_cap.is_some() {
              total_counter.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            }
          }
        } else if !progress_capped {
          eprintln!(
            "[block-progress-cap] parent_id={} selector={} max_logs={}",
            parent.id, parent_selector, progress_max
          );
          progress_capped = true;
        }
      }
      // Skip collapsible whitespace text in block formatting contexts (CSS 2.1 §16.6).
      if let BoxType::Text(text_box) = &child.box_type {
        if text_box.text.trim().is_empty()
          && !matches!(
            child.style.white_space,
            crate::style::types::WhiteSpace::Pre | crate::style::types::WhiteSpace::PreWrap
          )
        {
          if trace_positioned_ids().contains(&child.id)
            || trace_block_text_ids().contains(&child.id)
          {
            eprintln!(
              "[block-text-skip] id={} selector={:?} raw={:?}",
              child.id,
              child.debug_info.as_ref().map(|d| d.to_selector()),
              text_box.text
            );
          }
          continue;
        }
        if trace_block_text_ids().contains(&child.id) {
          eprintln!(
            "[block-text] id={} selector={:?} text={:?} white_space={:?}",
            child.id,
            child.debug_info.as_ref().map(|d| d.to_selector()),
            text_box.text,
            child.style.white_space
          );
        }
      }

      // Skip out-of-flow positioned boxes (absolute/fixed)
      if is_out_of_flow(child) {
        if trace_positioned_ids().contains(&child.id) {
          eprintln!(
            "[block-positioned] parent_id={} child_id={} selector={} pos={:?}",
            parent.id,
            child.id,
            child
              .debug_info
              .as_ref()
              .map(|d| d.to_selector())
              .unwrap_or_else(|| "<anon>".into()),
            child.style.position
          );
        }
        let pending_margin = margin_ctx.pending_margin();
        // Hypothetical box for static position: use normal block width resolution to include auto margins.
        let hypo_width = compute_block_width(
          &child.style,
          containing_width,
          self.viewport_size,
          inline_axis_sides(&child.style),
          inline_axis_positive(child.style.writing_mode, child.style.direction),
        );
        let static_x = hypo_width.margin_left;
        let static_y = current_y + pending_margin;
        let static_position = Some(Point::new(static_x, static_y));
        let source = match child.style.position {
          Position::Fixed => {
            if establishes_fixed_cb {
              ContainingBlockSource::ParentPadding
            } else {
              ContainingBlockSource::Explicit(ContainingBlock::viewport(self.viewport_size))
            }
          }
          Position::Absolute => {
            if establishes_absolute_cb {
              ContainingBlockSource::ParentPadding
            } else {
              ContainingBlockSource::Explicit(*nearest_positioned_cb)
            }
          }
          _ => ContainingBlockSource::Explicit(*nearest_positioned_cb),
        };
        positioned_children.push(PositionedCandidate {
          node: child.clone(),
          source,
          static_position,
        });
        continue;
      }

      // Floats are taken out of flow but still participate in this BFC's float context
      if child.style.float.is_floating()
        && !matches!(child.style.position, Position::Absolute | Position::Fixed)
      {
        flush_inline_buffer(
          &mut inline_buffer,
          &mut fragments,
          &mut current_y,
          &mut content_height,
          &mut margin_ctx,
          &mut float_ctx,
        )?;

        // Apply any pending collapsed margin before placing the float
        let pending_margin = margin_ctx.consume_pending();
        current_y += pending_margin;

        // Honor clearance against existing floats
        current_y = float_ctx.compute_clearance(current_y, child.style.clear);

        let percentage_base = containing_width;
        let margin_left = child
          .style
          .margin_left
          .as_ref()
          .map(|l| {
            resolve_length_for_width(
              *l,
              percentage_base,
              &child.style,
              &self.font_context,
              self.viewport_size,
            )
          })
          .unwrap_or(0.0)
          .max(0.0);
        let margin_right = child
          .style
          .margin_right
          .as_ref()
          .map(|l| {
            resolve_length_for_width(
              *l,
              percentage_base,
              &child.style,
              &self.font_context,
              self.viewport_size,
            )
          })
          .unwrap_or(0.0)
          .max(0.0);
        let horizontal_edges = horizontal_padding_and_borders(
          &child.style,
          percentage_base,
          self.viewport_size,
          &self.font_context,
        );

        // CSS 2.1 shrink-to-fit formula for floats
        let factory = FormattingContextFactory::with_font_context_viewport_and_cb(
          self.font_context.clone(),
          self.viewport_size,
          *nearest_positioned_cb,
        );
        let fc_type = child
          .formatting_context()
          .unwrap_or(FormattingContextType::Block);
        let fc = factory.create(fc_type);
        let preferred_min_content = fc
          .compute_intrinsic_inline_size(child, IntrinsicSizingMode::MinContent)
          .unwrap_or(0.0);
        let preferred_content = fc
          .compute_intrinsic_inline_size(child, IntrinsicSizingMode::MaxContent)
          .unwrap_or(preferred_min_content);

        let preferred_min = preferred_min_content + horizontal_edges;
        let preferred = preferred_content + horizontal_edges;

        let specified_width = child
          .style
          .width
          .as_ref()
          .map(|l| {
            resolve_length_for_width(
              *l,
              percentage_base,
              &child.style,
              &self.font_context,
              self.viewport_size,
            )
          })
          .map(|w| border_size_from_box_sizing(w, horizontal_edges, child.style.box_sizing));

        let min_width = child
          .style
          .min_width
          .as_ref()
          .map(|l| {
            resolve_length_for_width(
              *l,
              percentage_base,
              &child.style,
              &self.font_context,
              self.viewport_size,
            )
          })
          .map(|w| border_size_from_box_sizing(w, horizontal_edges, child.style.box_sizing))
          .unwrap_or(0.0);
        let max_width = child
          .style
          .max_width
          .as_ref()
          .map(|l| {
            resolve_length_for_width(
              *l,
              percentage_base,
              &child.style,
              &self.font_context,
              self.viewport_size,
            )
          })
          .map(|w| border_size_from_box_sizing(w, horizontal_edges, child.style.box_sizing))
          .unwrap_or(f32::INFINITY);

        let (_, float_available_width) = float_ctx.available_width_at_y(current_y);
        let available = (float_available_width - margin_left - margin_right).max(0.0);
        let used_border_box = if let Some(specified) = specified_width {
          crate::layout::utils::clamp_with_order(specified, min_width, max_width)
        } else {
          let shrink = preferred.min(available.max(preferred_min));
          crate::layout::utils::clamp_with_order(shrink, min_width, max_width)
        };

        let content_width = (used_border_box - horizontal_edges).max(0.0);

        let child_constraints = LayoutConstraints::new(
          AvailableSpace::Definite(content_width),
          AvailableSpace::Indefinite,
        );
        let child_bfc = BlockFormattingContext::with_font_context_viewport_and_cb(
          self.font_context.clone(),
          self.viewport_size,
          *nearest_positioned_cb,
        );
        let mut fragment = child_bfc.layout(child, &child_constraints)?;

        let margin_top = child
          .style
          .margin_top
          .as_ref()
          .map(|l| {
            resolve_length_for_width(
              *l,
              containing_width,
              &child.style,
              &self.font_context,
              self.viewport_size,
            )
          })
          .unwrap_or(0.0)
          .max(0.0);
        let margin_bottom = child
          .style
          .margin_bottom
          .as_ref()
          .map(|l| {
            resolve_length_for_width(
              *l,
              containing_width,
              &child.style,
              &self.font_context,
              self.viewport_size,
            )
          })
          .unwrap_or(0.0)
          .max(0.0);
        let box_width = used_border_box;
        let float_height = margin_top + fragment.bounds.height() + margin_bottom;

        let side = match child.style.float {
          Float::Left => FloatSide::Left,
          Float::Right => FloatSide::Right,
          Float::None => unreachable!(),
        };

        let (fx, fy) = float_ctx.compute_float_position(
          side,
          margin_left + box_width + margin_right,
          float_height,
          current_y,
        );

        fragment.bounds = Rect::from_xywh(
          fx + margin_left,
          fy + margin_top,
          box_width,
          fragment.bounds.height(),
        );
        float_ctx.add_float_at(
          side,
          fx,
          fy,
          margin_left + box_width + margin_right,
          float_height,
        );
        content_height = content_height.max(fy + float_height);
        if child.style.position.is_relative() {
          let positioned_style = crate::layout::absolute_positioning::resolve_positioned_style(
            &child.style,
            &relative_cb,
            self.viewport_size,
            &self.font_context,
          );
          fragment = PositionedLayout::new().apply_relative_positioning(
            &fragment,
            &positioned_style,
            &relative_cb,
          )?;
        }
        fragments.push(fragment);
        continue;
      }

      // Layout in-flow children
      let treated_as_block = child.is_block_level()
        || matches!(child.box_type, BoxType::Replaced(_) if !child.style.display.is_inline_level());

      if treated_as_block {
        flush_inline_buffer(
          &mut inline_buffer,
          &mut fragments,
          &mut current_y,
          &mut content_height,
          &mut margin_ctx,
          &mut float_ctx,
        )?;

        // Apply clearance for in-flow blocks against floats
        let cleared_y = float_ctx.compute_clearance(current_y, child.style.clear);
        if cleared_y > current_y {
          margin_ctx.mark_content_encountered();
          current_y = cleared_y;
        }

        if dump_cell_child_y && matches!(parent.style.display, Display::TableCell) {
          eprintln!(
            "cell child layout: parent_id={} child_idx={} child_display={:?} current_y={:.2}",
            parent.id, child_idx, child.style.display, current_y
          );
        }
        if !trace_boxes.is_empty() && trace_boxes.contains(&child.id) {
          eprintln!(
                    "[trace-box-pre] id={} display={:?} width={:?} min=({:?},{:?}) max=({:?},{:?}) margin=({:?},{:?})",
                    child.id,
                    child.style.display,
                    child.style.width,
                    child.style.min_width,
                    child.style.min_height,
                    child.style.max_width,
                    child.style.max_height,
                    child.style.margin_left,
                    child.style.margin_right,
                );
        }

        let (fragment, next_y) = self.layout_block_child(
          parent,
          child,
          containing_width,
          constraints,
          &mut margin_ctx,
          current_y,
          nearest_positioned_cb,
        )?;

        if dump_cell_child_y && matches!(parent.style.display, Display::TableCell) {
          let b = fragment.bounds;
          eprintln!(
                        "cell child placed: parent_id={} child_id={} display={:?} current_y={:.2} frag=({:.2},{:.2},{:.2},{:.2}) next_y={:.2}",
                        parent.id,
                        child.id,
                        child.style.display,
                        current_y,
                        b.x(),
                        b.y(),
                        b.width(),
                        b.height(),
                        next_y
                    );
        }
        if !trace_boxes.is_empty() && trace_boxes.contains(&child.id) {
          eprintln!(
                        "[trace-box] id={} display={:?} width={:?} height={:?} min=({:?},{:?}) max=({:?},{:?}) at y={:.2} -> next_y={:.2}",
                        child.id,
                        child.style.display,
                        child.style.width,
                        child.style.height,
                        child.style.min_width,
                        child.style.min_height,
                        child.style.max_width,
                        child.style.max_height,
                        current_y,
                        next_y
                    );
        }

        content_height = content_height.max(fragment.bounds.max_y());
        current_y = next_y;
        let mut fragment = fragment;
        if child.style.position.is_relative() {
          let positioned_style = crate::layout::absolute_positioning::resolve_positioned_style(
            &child.style,
            &relative_cb,
            self.viewport_size,
            &self.font_context,
          );
          fragment = PositionedLayout::new().apply_relative_positioning(
            &fragment,
            &positioned_style,
            &relative_cb,
          )?;
        }
        fragments.push(fragment);
      } else {
        // Inline-level non-replaced elements should still respect block/inline splits:
        // if this inline itself establishes a block formatting context (e.g., display:block
        // on an inline ancestor), flush the buffer and lay it out as a block.
        if child.is_block_level() {
          flush_inline_buffer(
            &mut inline_buffer,
            &mut fragments,
            &mut current_y,
            &mut content_height,
            &mut margin_ctx,
            &mut float_ctx,
          )?;
          let (fragment, next_y) = self.layout_block_child(
            parent,
            child,
            containing_width,
            constraints,
            &mut margin_ctx,
            current_y,
            nearest_positioned_cb,
          )?;
          content_height = content_height.max(fragment.bounds.max_y());
          current_y = next_y;
          let mut fragment = fragment;
          if child.style.position.is_relative() {
            let positioned_style = crate::layout::absolute_positioning::resolve_positioned_style(
              &child.style,
              &relative_cb,
              self.viewport_size,
              &self.font_context,
            );
            fragment = PositionedLayout::new().apply_relative_positioning(
              &fragment,
              &positioned_style,
              &relative_cb,
            )?;
          }
          fragments.push(fragment);
        } else {
          inline_buffer.push(child.clone());
        }
      }
    }

    flush_inline_buffer(
      &mut inline_buffer,
      &mut fragments,
      &mut current_y,
      &mut content_height,
      &mut margin_ctx,
      &mut float_ctx,
    )?;

    // Resolve any trailing margins
    let trailing_margin = margin_ctx.pending_margin();
    let allow_collapse_last = should_collapse_with_last_child(&parent.style);

    // Check for bottom separation
    let parent_has_bottom_separation = resolve_length_for_width(
      parent.style.border_bottom_width,
      containing_width,
      &parent.style,
      &self.font_context,
      self.viewport_size,
    ) > 0.0
      || resolve_length_for_width(
        parent.style.padding_bottom,
        containing_width,
        &parent.style,
        &self.font_context,
        self.viewport_size,
      ) > 0.0;

    if !allow_collapse_last || parent_has_bottom_separation {
      content_height += trailing_margin.max(0.0);
    }

    // Float boxes extend the formatting context height
    let float_bottom = float_ctx
      .left_floats()
      .iter()
      .chain(float_ctx.right_floats())
      .map(|f| f.bottom())
      .fold(content_height, f32::max);

    Ok((fragments, float_bottom, positioned_children))
  }

  fn is_multicol_container(style: &ComputedStyle) -> bool {
    style.column_count.unwrap_or(1) > 1 || style.column_width.is_some()
  }

  fn compute_column_geometry(
    &self,
    style: &ComputedStyle,
    available_inline: f32,
  ) -> (usize, f32, f32) {
    let gap = resolve_length_for_width(
      style.column_gap,
      available_inline,
      style,
      &self.font_context,
      self.viewport_size,
    )
    .max(0.0);

    let specified_width = style.column_width.as_ref().map(|l| {
      resolve_length_for_width(
        *l,
        available_inline,
        style,
        &self.font_context,
        self.viewport_size,
      )
    });
    let specified_count = style.column_count.unwrap_or(0).max(0);

    if specified_count > 0 {
      let count = specified_count as usize;
      let width = ((available_inline - gap * (count.saturating_sub(1) as f32)) / count as f32)
        .max(0.0);
      let used_width = specified_width.map(|w| w.min(width)).unwrap_or(width);
      return (count.max(1), used_width, gap);
    }

    if let Some(spec_width) = specified_width.filter(|w| *w > 0.0) {
      let denom = spec_width + gap;
      let mut count = if denom > 0.0 {
        ((available_inline + gap) / denom).floor() as usize
      } else {
        1
      };
      if count == 0 {
        count = 1;
      }
      let used_width = ((available_inline - gap * (count.saturating_sub(1) as f32))
        / count as f32)
        .max(0.0);
      return (count, used_width, gap);
    }

    (1, available_inline.max(0.0), gap)
  }

  fn layout_column_segment(
    &self,
    parent: &BoxNode,
    children: &[BoxNode],
    column_count: usize,
    column_width: f32,
    column_gap: f32,
    available_height: AvailableSpace,
    nearest_positioned_cb: &ContainingBlock,
  ) -> Result<(Vec<FragmentNode>, f32, Vec<PositionedCandidate>), LayoutError> {
    if children.is_empty() {
      return Ok((Vec::new(), 0.0, Vec::new()));
    }

    if column_count <= 1 {
      let mut parent_clone = parent.clone();
      parent_clone.children = children.to_vec();
      return self.layout_children(
        &parent_clone,
        &LayoutConstraints::new(AvailableSpace::Definite(column_width), available_height),
        nearest_positioned_cb,
      );
    }

    let mut parent_clone = parent.clone();
    parent_clone.children = children.to_vec();
    let column_constraints = LayoutConstraints::new(
      AvailableSpace::Definite(column_width),
      available_height,
    );
    let (flow_fragments, flow_height, flow_positioned) =
      self.layout_children(&parent_clone, &column_constraints, nearest_positioned_cb)?;

    let balanced_height = if column_count > 0 {
      (flow_height / column_count as f32).ceil()
    } else {
      flow_height
    };
    let mut column_height = match parent.style.column_fill {
      ColumnFill::Auto => match available_height {
        AvailableSpace::Definite(h) => h.max(balanced_height),
        _ => balanced_height,
      },
      ColumnFill::Balance => balanced_height,
    };
    if !column_height.is_finite() || column_height <= 0.0 {
      column_height = flow_height.max(0.0);
    }

    let mut fragments = Vec::new();
    let mut column_heights = vec![0.0f32; column_count.max(1)];
    let mut prev_flow_y = 0.0;
    let mut col_idx: usize = 0;
    let mut col_height = 0.0;

    for mut fragment in flow_fragments {
      let delta = fragment.bounds.y() - prev_flow_y;
      let height = fragment.bounds.height();
      if col_height + delta + height > column_height && col_idx + 1 < column_count {
        col_idx += 1;
        col_height = 0.0;
      }
      let new_y = col_height + delta;
      let new_x = col_idx as f32 * (column_width + column_gap) + fragment.bounds.x();
      fragment.bounds = Rect::from_xywh(new_x, new_y, fragment.bounds.width(), height);
      col_height = new_y + height;
      column_heights[col_idx] = column_heights[col_idx].max(col_height);
      prev_flow_y += delta + height;
      fragments.push(fragment);
    }

    let mut positioned_children = Vec::new();
    for mut positioned in flow_positioned {
      if let Some(pos) = positioned.static_position {
        if column_height > 0.0 {
          let col = ((pos.y / column_height).floor() as usize).min(column_count - 1);
          let translated = Point::new(
            pos.x + col as f32 * (column_width + column_gap),
            pos.y - column_height * col as f32,
          );
          positioned.static_position = Some(translated);
        }
      }
      positioned_children.push(positioned);
    }

    let mut segment_height = column_heights.iter().copied().fold(0.0, f32::max);
    if segment_height == 0.0 {
      segment_height = flow_height;
    }

    if column_count > 1
      && column_gap > 0.0
      && !matches!(parent.style.column_rule_style, BorderStyle::None | BorderStyle::Hidden)
    {
      let mut rule_width = resolve_length_for_width(
        parent.style.column_rule_width,
        column_width,
        &parent.style,
        &self.font_context,
        self.viewport_size,
      )
      .min(column_gap)
      .max(0.0);
      if rule_width > 0.0 {
        let color = parent.style.column_rule_color.unwrap_or(parent.style.color);
        let mut rule_style = ComputedStyle::default();
        rule_style.background_color = color;
        let rule_style = Arc::new(rule_style);
        if rule_width > column_gap {
          rule_width = column_gap;
        }
        for i in 1..column_count {
          let gap_start = column_width * i as f32 + column_gap * (i as f32 - 1.0);
          let x = gap_start + (column_gap - rule_width) / 2.0;
          let bounds = Rect::from_xywh(x.max(0.0), 0.0, rule_width, segment_height);
          fragments.push(FragmentNode::new_block_styled(
            bounds,
            Vec::new(),
            rule_style.clone(),
          ));
        }
      }
    }

    Ok((fragments, segment_height, positioned_children))
  }

  fn layout_multicolumn(
    &self,
    parent: &BoxNode,
    constraints: &LayoutConstraints,
    nearest_positioned_cb: &ContainingBlock,
    available_inline: f32,
  ) -> Result<(Vec<FragmentNode>, f32, Vec<PositionedCandidate>), LayoutError> {
    let (column_count, column_width, column_gap) =
      self.compute_column_geometry(&parent.style, available_inline);
    if column_count <= 1 {
      return self.layout_children(parent, constraints, nearest_positioned_cb);
    }

    let mut fragments = Vec::new();
    let mut positioned_children = Vec::new();
    let mut y_offset = 0.0;
    let mut idx = 0;

    while idx < parent.children.len() {
      let next_span = parent.children[idx..]
        .iter()
        .position(|c| c.style.column_span == ColumnSpan::All)
        .map(|p| p + idx);
      let end = next_span.unwrap_or(parent.children.len());

      if end > idx {
        let (mut seg_fragments, seg_height, mut seg_positioned) = self.layout_column_segment(
          parent,
          &parent.children[idx..end],
          column_count,
          column_width,
          column_gap,
          constraints.available_height,
          nearest_positioned_cb,
        )?;
        for frag in &mut seg_fragments {
          frag.bounds = Rect::from_xywh(
            frag.bounds.x(),
            frag.bounds.y() + y_offset,
            frag.bounds.width(),
            frag.bounds.height(),
          );
        }
        for positioned in &mut seg_positioned {
          if let Some(pos) = positioned.static_position {
            positioned.static_position = Some(Point::new(pos.x, pos.y + y_offset));
          }
        }
        fragments.extend(seg_fragments);
        positioned_children.extend(seg_positioned);
        y_offset += seg_height;
      }

      if let Some(span_idx) = next_span {
        let mut span_parent = parent.clone();
        span_parent.children = vec![parent.children[span_idx].clone()];
        let span_constraints = LayoutConstraints::new(
          AvailableSpace::Definite(available_inline),
          constraints.available_height,
        );
        let (mut span_fragments, span_height, mut span_positioned) = self.layout_children(
          &span_parent,
          &span_constraints,
          nearest_positioned_cb,
        )?;
        for frag in &mut span_fragments {
          frag.bounds = Rect::from_xywh(
            frag.bounds.x(),
            frag.bounds.y() + y_offset,
            frag.bounds.width(),
            frag.bounds.height(),
          );
        }
        for positioned in &mut span_positioned {
          if let Some(pos) = positioned.static_position {
            positioned.static_position = Some(Point::new(pos.x, pos.y + y_offset));
          }
        }
        fragments.extend(span_fragments);
        positioned_children.extend(span_positioned);
        y_offset += span_height;
        idx = span_idx + 1;
      } else {
        break;
      }
    }

    Ok((fragments, y_offset, positioned_children))
  }
}

impl Default for BlockFormattingContext {
  fn default() -> Self {
    Self::new()
  }
}

impl std::fmt::Debug for BlockFormattingContext {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_str("BlockFormattingContext")
  }
}

impl FormattingContext for BlockFormattingContext {
  #[allow(clippy::cognitive_complexity)]
  fn layout(
    &self,
    box_node: &BoxNode,
    constraints: &LayoutConstraints,
  ) -> Result<FragmentNode, LayoutError> {
    let _profile = layout_timer(LayoutKind::Block);
    let style = &box_node.style;
    let log_skinny = std::env::var("FASTR_LOG_SKINNY_FLEX")
      .map(|v| v != "0")
      .unwrap_or(false);
    let inline_percentage_base = match constraints.available_width {
      AvailableSpace::Definite(_) => constraints
        .inline_percentage_base
        .or_else(|| constraints.width())
        .unwrap_or(self.viewport_size.width),
      AvailableSpace::MinContent | AvailableSpace::MaxContent | AvailableSpace::Indefinite => {
        constraints.inline_percentage_base.unwrap_or(0.0)
      }
    };
    // When the containing block inline size is intrinsic/indefinite (min-/max-content probes),
    // percentage widths behave as `auto` per CSS sizing. Strip percentage width/min/max hints
    // so intrinsic sizing does not resolve them against an unrelated base (e.g., viewport).
    let use_percent_as_auto = matches!(
      constraints.available_width,
      AvailableSpace::MinContent | AvailableSpace::MaxContent | AvailableSpace::Indefinite
    );
    let _style_for_width_owned: Option<ComputedStyle>;
    let style_for_width: &ComputedStyle = if use_percent_as_auto {
      let mut s: ComputedStyle = (**style).clone();
      if matches!(s.width, Some(len) if len.unit.is_percentage()) {
        s.width = None;
      }
      if matches!(s.min_width, Some(len) if len.unit.is_percentage()) {
        s.min_width = None;
      }
      if matches!(s.max_width, Some(len) if len.unit.is_percentage()) {
        s.max_width = None;
      }
      _style_for_width_owned = Some(s);
      _style_for_width_owned.as_ref().unwrap()
    } else {
      _style_for_width_owned = None;
      style
    };

    // When available width is indefinite/max-content, try to derive a reasonable containing
    // width from the element's own sizing hints (max-width/width/min-width) before falling
    // back to the viewport. The base for percentages must be the parent’s containing width
    // (the constraint) rather than the viewport; otherwise centered/narrow wrappers (e.g.,
    // 400px max-width zones) inflate to 1200px during intrinsic probes.
    let preferred_containing_width = |percentage_base: f32| {
      let resolve = |len: &Length| {
        resolve_length_for_width(
          *len,
          percentage_base,
          style,
          &self.font_context,
          self.viewport_size,
        )
      };
      style
        .max_width
        .as_ref()
        .map(resolve)
        .or_else(|| style.width.as_ref().map(resolve))
        .or_else(|| style.min_width.as_ref().map(resolve))
    };

    // Replaced elements laid out as standalone formatting contexts: compute their used size
    // directly instead of running the block width algorithm (which would treat the specified
    // width as the used content width without honoring max-width).
    if let BoxType::Replaced(replaced_box) = &box_node.box_type {
      let mut containing_width = inline_percentage_base;
      if containing_width <= 1.0 {
        let width_is_absolute = style
          .width
          .as_ref()
          .map(|l| l.unit.is_absolute())
          .unwrap_or(false);
        if !width_is_absolute {
          containing_width = self.viewport_size.width;
        }
      }
      let containing_height = constraints.height();
      let percentage_base = Some(crate::geometry::Size::new(
        containing_width,
        containing_height.unwrap_or(f32::NAN),
      ));
      let mut used_size =
        compute_replaced_size(style, replaced_box, percentage_base, self.viewport_size);
      if let Some(max_w) = style.max_width.as_ref().map(|l| {
        resolve_length_for_width(
          *l,
          containing_width,
          style,
          &self.font_context,
          self.viewport_size,
        )
      }) {
        used_size.width = used_size.width.min(max_w);
      }
      if let Some(min_w) = style.min_width.as_ref().map(|l| {
        resolve_length_for_width(
          *l,
          containing_width,
          style,
          &self.font_context,
          self.viewport_size,
        )
      }) {
        used_size.width = used_size.width.max(min_w);
      }
      if log_skinny && containing_width <= 1.0 {
        let selector = box_node
          .debug_info
          .as_ref()
          .map(|d| d.to_selector())
          .unwrap_or_else(|| "<anon>".to_string());
        eprintln!(
                    "[skinny-block-constraint] id={} selector={} replaced containing_w={:.2} used_w={:.2} min_w={:?} max_w={:?}",
                    box_node.id, selector, containing_width, used_size.width, style.min_width, style.max_width
                );
      }
      if let Some(ch) = containing_height {
        if let Some(max_h) = style.max_height.as_ref().and_then(|l| {
          resolve_length_with_percentage(
            *l,
            Some(ch),
            self.viewport_size,
            style.font_size,
            style.root_font_size,
          )
        }) {
          used_size.height = used_size.height.min(max_h);
        }
        if let Some(min_h) = style.min_height.as_ref().and_then(|l| {
          resolve_length_with_percentage(
            *l,
            Some(ch),
            self.viewport_size,
            style.font_size,
            style.root_font_size,
          )
        }) {
          used_size.height = used_size.height.max(min_h);
        }
      }

      let bounds = Rect::new(
        Point::new(0.0, 0.0),
        Size::new(used_size.width.max(0.0), used_size.height.max(0.0)),
      );
      return Ok(FragmentNode::new_with_style(
        bounds,
        crate::tree::fragment_tree::FragmentContent::Replaced {
          replaced_type: replaced_box.replaced_type.clone(),
          box_id: Some(box_node.id),
        },
        vec![],
        box_node.style.clone(),
      ));
    }

    let intrinsic_width_mode = matches!(
      constraints.available_width,
      AvailableSpace::MaxContent | AvailableSpace::MinContent | AvailableSpace::Indefinite
    );
    let mut containing_width = match constraints.available_width {
      AvailableSpace::Definite(w) => w,
      // In-flow blocks use the containing block’s inline size; shrink-to-fit contexts should
      // feed a definite width in constraints. When the available width is indefinite/max/min
      // content, prefer the element’s own sizing hints (resolved against the parent
      // containing width when known) before falling back to the viewport.
      AvailableSpace::MaxContent | AvailableSpace::MinContent | AvailableSpace::Indefinite => {
        preferred_containing_width(inline_percentage_base).unwrap_or(inline_percentage_base)
      }
    }
    .min(self.viewport_size.width);
    if containing_width <= 1.0 && !intrinsic_width_mode {
      let width_is_absolute = style
        .width
        .as_ref()
        .map(|l| l.unit.is_absolute())
        .unwrap_or(false);
      if !width_is_absolute {
        containing_width = self.viewport_size.width;
      }
    }
    static LOG_SMALL_BLOCK: OnceLock<bool> = OnceLock::new();
    if *LOG_SMALL_BLOCK.get_or_init(|| {
      std::env::var("FASTR_LOG_SMALL_BLOCK")
        .map(|v| v != "0")
        .unwrap_or(false)
    }) && containing_width < 150.0
    {
      let selector = box_node
        .debug_info
        .as_ref()
        .map(|d| d.to_selector())
        .unwrap_or_else(|| "<anonymous>".to_string());
      eprintln!(
                "[block-small] id={} selector={} containing_w={:.1} avail_w={:?} width_decl={:?} min_w={:?} max_w={:?}",
                box_node.id,
                selector,
                containing_width,
                constraints.available_width,
                style.width,
                style.min_width,
                style.max_width,
            );
    }
    let containing_height = constraints.height();
    // For flex items, prefer the max-content contribution instead of filling the available
    // width when width is auto (CSS Flexbox §4.5: auto main size uses the max-content size).
    // This avoids the block constraint equation forcing auto margins/auto widths to span the
    // containing block during flex item hypothetical sizing.
    let flex_pref_border = if self.flex_item_mode && style_for_width.width.is_none() {
      let intrinsic_mode = match constraints.available_width {
        AvailableSpace::MinContent => IntrinsicSizingMode::MinContent,
        _ => IntrinsicSizingMode::MaxContent,
      };
      Some(self.compute_intrinsic_inline_size(box_node, intrinsic_mode)?)
    } else {
      None
    };

    let inline_sides = inline_axis_sides(style);
    let inline_positive = inline_axis_positive(style.writing_mode, style.direction);

    let mut computed_width = compute_block_width(
      style_for_width,
      containing_width,
      self.viewport_size,
      inline_sides,
      inline_positive,
    );
    if style.shrink_to_fit_inline_size && style_for_width.width.is_none() {
      static LOG_SHRINK: OnceLock<bool> = OnceLock::new();
      let log_shrink = *LOG_SHRINK.get_or_init(|| {
        std::env::var("FASTR_LOG_SHRINK_TO_FIT")
          .map(|v| v != "0" && !v.eq_ignore_ascii_case("false"))
          .unwrap_or(false)
      });
      let horizontal_edges = computed_width.border_left
        + computed_width.padding_left
        + computed_width.padding_right
        + computed_width.border_right;
      let margin_left = style
        .margin_left
        .as_ref()
        .map(|l| {
          resolve_length_for_width(
            *l,
            containing_width,
            style,
            &self.font_context,
            self.viewport_size,
          )
        })
        .unwrap_or(0.0);
      let margin_right = style
        .margin_right
        .as_ref()
        .map(|l| {
          resolve_length_for_width(
            *l,
            containing_width,
            style,
            &self.font_context,
            self.viewport_size,
          )
        })
        .unwrap_or(0.0);

      let factory = FormattingContextFactory::with_font_context_viewport_and_cb(
        self.font_context.clone(),
        self.viewport_size,
        self.nearest_positioned_cb,
      );
      let fc_type = box_node
        .formatting_context()
        .unwrap_or(FormattingContextType::Block);
      let fc = factory.create(fc_type);
      let preferred_min_content =
        fc.compute_intrinsic_inline_size(box_node, IntrinsicSizingMode::MinContent)?;
      let preferred_content = fc
        .compute_intrinsic_inline_size(box_node, IntrinsicSizingMode::MaxContent)
        .unwrap_or(preferred_min_content);

      let preferred_min = preferred_min_content + horizontal_edges;
      let preferred = preferred_content + horizontal_edges;
      let available = (containing_width - margin_left - margin_right).max(0.0);
      let shrink_border_box = preferred.min(available.max(preferred_min));
      let shrink_content = (shrink_border_box - horizontal_edges).max(0.0);
      if log_shrink {
        let selector = box_node
          .debug_info
          .as_ref()
          .map(|d| d.to_selector())
          .unwrap_or_else(|| "<anon>".to_string());
        eprintln!(
                    "[shrink-to-fit] id={} selector={} preferred_min={:.1} preferred={:.1} available={:.1} content={:.1} edges={:.1}",
                    box_node.id, selector, preferred_min, preferred, available, shrink_content, horizontal_edges
                );
      }
      let (margin_left, margin_right) = recompute_margins_for_width(
        style,
        containing_width,
        shrink_content,
        computed_width.border_left,
        computed_width.padding_left,
        computed_width.padding_right,
        computed_width.border_right,
        self.viewport_size,
        &self.font_context,
      );
      computed_width.content_width = shrink_content;
      computed_width.margin_left = margin_left;
      computed_width.margin_right = margin_right;
    }
    // When asked for intrinsic max-/min-content sizes, override the constraint equation with
    // the corresponding intrinsic inline size so flex/inline shrink-to-fit measurements don't
    // default to the full containing block width.
    if matches!(
      constraints.available_width,
      AvailableSpace::MinContent | AvailableSpace::MaxContent
    ) {
      let intrinsic_mode = match constraints.available_width {
        AvailableSpace::MinContent => IntrinsicSizingMode::MinContent,
        _ => IntrinsicSizingMode::MaxContent,
      };
      if let Ok(intrinsic_border) = self.compute_intrinsic_inline_size(box_node, intrinsic_mode) {
        let horizontal_edges = computed_width.border_left
          + computed_width.padding_left
          + computed_width.padding_right
          + computed_width.border_right;
        let intrinsic_content = (intrinsic_border - horizontal_edges).max(0.0);
        computed_width.content_width = intrinsic_content;
      }
    }
    let horizontal_edges = computed_width.border_left
      + computed_width.padding_left
      + computed_width.padding_right
      + computed_width.border_right;
    if let Some(pref_border) = flex_pref_border {
      let pref_content = (pref_border - horizontal_edges).max(0.0);
      computed_width.content_width = pref_content;
    }

    let min_width = style_for_width
      .min_width
      .as_ref()
      .map(|l| {
        resolve_length_for_width(
          *l,
          containing_width,
          style_for_width,
          &self.font_context,
          self.viewport_size,
        )
      })
      .map(|w| content_size_from_box_sizing(w, horizontal_edges, style.box_sizing))
      .unwrap_or(0.0);
    let max_width = style_for_width
      .max_width
      .as_ref()
      .map(|l| {
        resolve_length_for_width(
          *l,
          containing_width,
          style_for_width,
          &self.font_context,
          self.viewport_size,
        )
      })
      .map(|w| content_size_from_box_sizing(w, horizontal_edges, style.box_sizing))
      .unwrap_or(f32::INFINITY);

    // CSS 2.1 §10.4: if the computed min-width exceeds max-width, max-width is set to min-width.
    let max_width = if max_width.is_finite() && max_width < min_width {
      min_width
    } else {
      max_width
    };

    let mut clamped_content_width =
      crate::layout::utils::clamp_with_order(computed_width.content_width, min_width, max_width);
    if clamped_content_width > self.viewport_size.width {
      clamped_content_width = self.viewport_size.width;
    }
    static LOG_WIDE_BLOCK: OnceLock<bool> = OnceLock::new();
    let log_wide_block = *LOG_WIDE_BLOCK.get_or_init(|| {
      std::env::var("FASTR_LOG_WIDE_FLEX")
        .map(|v| v != "0" && !v.eq_ignore_ascii_case("false"))
        .unwrap_or(false)
    });
    if log_wide_block && computed_width.content_width > self.viewport_size.width + 0.5 {
      let selector = box_node
        .debug_info
        .as_ref()
        .map(|d| d.to_selector())
        .unwrap_or_else(|| "<anonymous>".to_string());
      eprintln!(
                "[block-wide] box_id={:?} selector={} display={:?} containing={:.1} content_w={:.1} total_w={:.1} width={:?} min_w={:?} max_w={:?} viewport_w={:.1} avail_w={:?} margins=({:.1},{:.1})",
                box_node.id,
                selector,
                style.display,
                containing_width,
                computed_width.content_width,
                computed_width.total_width(),
                style.width,
                style.min_width,
                style.max_width,
                self.viewport_size.width,
                constraints.available_width,
                computed_width.margin_left,
                computed_width.margin_right,
            );
    }
    if self.flex_item_mode {
      // Flex items use their specified margins when computing hypothetical sizes; auto
      // margins resolve to 0 instead of being rebalanced to satisfy the block constraint
      // equation. Keep the clamped content width but avoid recomputing margins.
      computed_width.content_width = clamped_content_width;
      let resolved_ml = style
        .margin_left
        .as_ref()
        .map(|l| {
          resolve_length_for_width(
            *l,
            containing_width,
            style,
            &self.font_context,
            self.viewport_size,
          )
        })
        .unwrap_or(0.0);
      let resolved_mr = style
        .margin_right
        .as_ref()
        .map(|l| {
          resolve_length_for_width(
            *l,
            containing_width,
            style,
            &self.font_context,
            self.viewport_size,
          )
        })
        .unwrap_or(0.0);
      computed_width.margin_left = resolved_ml;
      computed_width.margin_right = resolved_mr;
    } else {
      if clamped_content_width != computed_width.content_width {
        let (margin_left, margin_right) = recompute_margins_for_width(
          style,
          containing_width,
          clamped_content_width,
          computed_width.border_left,
          computed_width.padding_left,
          computed_width.padding_right,
          computed_width.border_right,
          self.viewport_size,
          &self.font_context,
        );
        computed_width.content_width = clamped_content_width;
        computed_width.margin_left = margin_left;
        computed_width.margin_right = margin_right;
      }
    }

    let border_top = resolve_length_for_width(
      style.border_top_width,
      containing_width,
      style,
      &self.font_context,
      self.viewport_size,
    );
    let border_bottom = resolve_length_for_width(
      style.border_bottom_width,
      containing_width,
      style,
      &self.font_context,
      self.viewport_size,
    );
    let mut padding_top = resolve_length_for_width(
      style.padding_top,
      containing_width,
      style,
      &self.font_context,
      self.viewport_size,
    );
    let mut padding_bottom = resolve_length_for_width(
      style.padding_bottom,
      containing_width,
      style,
      &self.font_context,
      self.viewport_size,
    );
    // Reserve space for a horizontal scrollbar when requested by overflow or scrollbar-gutter stability.
    let reserve_horizontal_gutter = matches!(style.overflow_x, Overflow::Scroll)
      || (style.scrollbar_gutter.stable
        && matches!(style.overflow_x, Overflow::Auto | Overflow::Scroll));
    if reserve_horizontal_gutter {
      let gutter = resolve_scrollbar_width(style);
      if style.scrollbar_gutter.both_edges {
        padding_top += gutter;
      }
      padding_bottom += gutter;
    }
    let vertical_edges = border_top + padding_top + padding_bottom + border_bottom;

    let resolved_height = style
      .height
      .as_ref()
      .and_then(|h| {
        resolve_length_with_percentage(
          *h,
          containing_height,
          self.viewport_size,
          style.font_size,
          style.root_font_size,
        )
      })
      .map(|h| content_size_from_box_sizing(h, vertical_edges, style.box_sizing));
    let child_height_space = resolved_height
      .map(|h| AvailableSpace::Definite(h.max(0.0)))
      .unwrap_or(AvailableSpace::Indefinite);

    let child_constraints = LayoutConstraints::new(
      AvailableSpace::Definite(computed_width.content_width),
      child_height_space,
    );

    let padding_origin = Point::new(
      computed_width.border_left + computed_width.padding_left,
      border_top + padding_top,
    );
    let content_height_base = resolved_height.unwrap_or(0.0).max(0.0);
    let padding_size = Size::new(
      computed_width.content_width + computed_width.padding_left + computed_width.padding_right,
      content_height_base + padding_top + padding_bottom,
    );
    let cb_block_base = resolved_height.map(|h| h.max(0.0) + padding_top + padding_bottom);
    let establishes_positioned_cb = style.position.is_positioned()
      || !style.transform.is_empty()
      || style.perspective.is_some();
    let nearest_cb = if establishes_positioned_cb {
      ContainingBlock::with_viewport_and_bases(
        Rect::new(padding_origin, padding_size),
        self.viewport_size,
        Some(padding_size.width),
        cb_block_base,
      )
    } else {
      self.nearest_positioned_cb
    };

    let mut child_ctx = self.clone();
    child_ctx.flex_item_mode = false;
    child_ctx.nearest_positioned_cb = nearest_cb;
    let use_columns = Self::is_multicol_container(style);
    let (mut child_fragments, mut content_height, positioned_children) = if use_columns {
      child_ctx.layout_multicolumn(
        box_node,
        &child_constraints,
        &nearest_cb,
        computed_width.content_width,
      )?
    } else {
      child_ctx.layout_children(box_node, &child_constraints, &nearest_cb)?
    };
    if style.containment.size {
      content_height = 0.0;
    }

    let min_height = style
      .min_height
      .as_ref()
      .and_then(|l| {
        resolve_length_with_percentage(
          *l,
          containing_height,
          self.viewport_size,
          style.font_size,
          style.root_font_size,
        )
      })
      .map(|h| content_size_from_box_sizing(h, vertical_edges, style.box_sizing))
      .unwrap_or(0.0);
    let max_height = style
      .max_height
      .as_ref()
      .and_then(|l| {
        resolve_length_with_percentage(
          *l,
          containing_height,
          self.viewport_size,
          style.font_size,
          style.root_font_size,
        )
      })
      .map(|h| content_size_from_box_sizing(h, vertical_edges, style.box_sizing))
      .unwrap_or(f32::INFINITY);

    let max_height = if max_height.is_finite() && max_height < min_height {
      min_height
    } else {
      max_height
    };
    let height = crate::layout::utils::clamp_with_order(
      resolved_height.unwrap_or(content_height),
      min_height,
      max_height,
    );

    let box_height = border_top + padding_top + height + padding_bottom + border_bottom;
    // For root/layout entry points, keep fragment bounds scoped to the border box so margins
    // don’t inflate measured sizes (e.g., when flex items are measured via a block FC). The
    // margin space stays outside the fragment’s local coordinates, matching the child layout
    // path in `layout_block_child`.
    let box_width = computed_width.border_box_width();

    // Layout out-of-flow positioned children against this block's padding box.
    let padding_origin = Point::new(
      computed_width.border_left + computed_width.padding_left,
      border_top + padding_top,
    );
    let padding_size = Size::new(
      computed_width.content_width + computed_width.padding_left + computed_width.padding_right,
      height + padding_top + padding_bottom,
    );
    let padding_rect = Rect::new(padding_origin, padding_size);

    if !positioned_children.is_empty() {
      let abs = crate::layout::absolute_positioning::AbsoluteLayout::with_font_context(
        self.font_context.clone(),
      );
      let cb_block_base = if resolved_height.is_some() {
        Some(padding_size.height)
      } else {
        None
      };
      let parent_padding_cb = ContainingBlock::with_viewport_and_bases(
        padding_rect,
        self.viewport_size,
        Some(padding_size.width),
        cb_block_base,
      );

      for PositionedCandidate {
        node: child,
        source,
        static_position,
      } in positioned_children
      {
        let cb = match source {
          ContainingBlockSource::ParentPadding => parent_padding_cb,
          ContainingBlockSource::Explicit(cb) => cb,
        };
        let factory = FormattingContextFactory::with_font_context_viewport_and_cb(
          self.font_context.clone(),
          self.viewport_size,
          cb,
        );
        // Layout the child as if it were in normal flow to obtain its intrinsic size.
        let mut layout_child = child.clone();
        let mut style = (*layout_child.style).clone();
        style.position = Position::Relative;
        style.top = None;
        style.right = None;
        style.bottom = None;
        style.left = None;
        layout_child.style = Arc::new(style);

        let fc_type = layout_child
          .formatting_context()
          .unwrap_or(FormattingContextType::Block);
        let fc = factory.create(fc_type);
        let child_height_space = cb_block_base
          .map(AvailableSpace::Definite)
          .unwrap_or(AvailableSpace::Indefinite);
        let child_constraints = LayoutConstraints::new(
          AvailableSpace::Definite(padding_size.width),
          child_height_space,
        );
        let mut child_fragment = fc.layout(&layout_child, &child_constraints)?;

        // Resolve positioned style against the containing block.
        let positioned_style = crate::layout::absolute_positioning::resolve_positioned_style(
          &child.style,
          &cb,
          self.viewport_size,
          &self.font_context,
        );

        // Static position starts at the containing block origin; AbsoluteLayout will add
        // padding/border offsets, so use the content origin when no flow position was
        // recorded.
        let static_pos = static_position.unwrap_or(Point::ZERO);
        let preferred_min_inline = fc
          .compute_intrinsic_inline_size(&layout_child, IntrinsicSizingMode::MinContent)
          .ok();
        let preferred_inline = fc
          .compute_intrinsic_inline_size(&layout_child, IntrinsicSizingMode::MaxContent)
          .ok();
        let preferred_min_block = fc
          .compute_intrinsic_block_size(&layout_child, IntrinsicSizingMode::MinContent)
          .ok();
        let preferred_block = fc
          .compute_intrinsic_block_size(&layout_child, IntrinsicSizingMode::MaxContent)
          .ok();

        let mut input = crate::layout::absolute_positioning::AbsoluteLayoutInput::new(
          positioned_style,
          child_fragment.bounds.size,
          static_pos,
        );
        input.is_replaced = child.is_replaced();
        input.preferred_min_inline_size = preferred_min_inline;
        input.preferred_inline_size = preferred_inline;
        input.preferred_min_block_size = preferred_min_block;
        input.preferred_block_size = preferred_block;

        let result = abs.layout_absolute(&input, &cb)?;
        let needs_relayout = (result.size.width - child_fragment.bounds.width()).abs() > 0.01
          || (result.size.height - child_fragment.bounds.height()).abs() > 0.01;
        if needs_relayout {
          let relayout_constraints = LayoutConstraints::new(
            AvailableSpace::Definite(result.size.width),
            AvailableSpace::Definite(result.size.height),
          );
          let mut relayout_child = layout_child.clone();
          let mut relayout_style = (*relayout_child.style).clone();
          relayout_style.width = Some(crate::style::values::Length::px(result.size.width));
          relayout_style.height = Some(crate::style::values::Length::px(result.size.height));
          relayout_child.style = Arc::new(relayout_style);
          child_fragment = fc.layout(&relayout_child, &relayout_constraints)?;
        }
        child_fragment.bounds = Rect::new(result.position, result.size);
        child_fragment.style = Some(child.style.clone());
        if trace_positioned_ids().contains(&child.id) {
          let (text_count, total) = count_text_fragments(&child_fragment);
          eprintln!(
                        "[block-positioned-placed] child_id={} pos=({:.1},{:.1}) size=({:.1},{:.1}) texts={}/{}",
                        child.id,
                        result.position.x,
                        result.position.y,
                        result.size.width,
                        result.size.height,
                        text_count,
                        total
                    );
        }
        child_fragments.push(child_fragment);
      }
    }

    let bounds = Rect::from_xywh(computed_width.margin_left, 0.0, box_width, box_height);

    let mut fragment = FragmentNode::new_with_style(
      bounds,
      crate::tree::fragment_tree::FragmentContent::Block {
        box_id: Some(box_node.id),
      },
      child_fragments,
      box_node.style.clone(),
    );

    // Apply relative positioning after normal flow layout (CSS 2.1 §9.4.3).
    if style.position.is_relative() {
      let block_base = containing_height;
      let containing_block = ContainingBlock::with_viewport_and_bases(
        Rect::new(
          Point::ZERO,
          Size::new(containing_width, containing_height.unwrap_or(0.0)),
        ),
        self.viewport_size,
        Some(containing_width),
        block_base,
      );
      let positioned_style = crate::layout::absolute_positioning::resolve_positioned_style(
        style,
        &containing_block,
        self.viewport_size,
        &self.font_context,
      );
      fragment = PositionedLayout::new().apply_relative_positioning(
        &fragment,
        &positioned_style,
        &containing_block,
      )?;
    }

    Ok(fragment)
  }

  fn compute_intrinsic_inline_size(
    &self,
    box_node: &BoxNode,
    mode: IntrinsicSizingMode,
  ) -> Result<f32, LayoutError> {
    count_block_intrinsic_call();
    if let Some(cached) = intrinsic_cache_lookup(box_node, mode) {
      return Ok(cached);
    }

    let style = &box_node.style;
    let edges = horizontal_padding_and_borders(style, 0.0, self.viewport_size, &self.font_context);
    // Honor specified widths that resolve without a containing block
    if let Some(specified) = style.width.as_ref() {
      let resolved = resolve_length_for_width(
        *specified,
        0.0,
        style,
        &self.font_context,
        self.viewport_size,
      );
      // Ignore auto/relative cases that resolve to 0.0
      if resolved > 0.0 {
        let result = border_size_from_box_sizing(resolved, edges, style.box_sizing);
        intrinsic_cache_store(box_node, mode, result);
        return Ok(result);
      }
    }

    if style.containment.size || style.containment.inline_size {
      intrinsic_cache_store(box_node, mode, edges);
      return Ok(edges);
    }

    // Replaced elements fall back to their intrinsic content size plus padding/borders
    if let BoxType::Replaced(replaced_box) = &box_node.box_type {
      let size = compute_replaced_size(style, replaced_box, None, self.viewport_size);
      let edges =
        horizontal_padding_and_borders(style, size.width, self.viewport_size, &self.font_context);
      let result = size.width + edges;
      intrinsic_cache_store(box_node, mode, result);
      return Ok(result);
    }

    let factory = FormattingContextFactory::with_font_context_viewport_and_cb(
      self.font_context.clone(),
      self.viewport_size,
      self.nearest_positioned_cb,
    );
    let inline_fc = InlineFormattingContext::with_font_context_viewport_and_cb(
      self.font_context.clone(),
      self.viewport_size,
      self.nearest_positioned_cb,
    );
    let mut inline_run_cache: HashMap<(Vec<usize>, IntrinsicSizingMode), f32> = HashMap::new();

    // Inline formatting context contribution (text and inline-level children).
    // Block-level children split inline runs into separate formatting contexts.
    static LOG_IDS: OnceLock<Vec<usize>> = OnceLock::new();
    let log_ids = LOG_IDS.get_or_init(|| {
      std::env::var("FASTR_LOG_INTRINSIC_IDS")
        .ok()
        .map(|s| {
          s.split(',')
            .filter_map(|tok| tok.trim().parse::<usize>().ok())
            .collect()
        })
        .unwrap_or_default()
    });
    let log_children = !log_ids.is_empty() && log_ids.contains(&box_node.id);

    let mut inline_width = 0.0f32;
    let mut inline_run: Vec<BoxNode> = Vec::new();
    let mut flush_inline_run =
      |run: &mut Vec<BoxNode>, widest: &mut f32| -> Result<(), LayoutError> {
        if run.is_empty() {
          return Ok(());
        }
        let key = run.iter().map(|c| c.id()).collect::<Vec<_>>();
        if let Some(width) = inline_run_cache.get(&(key.clone(), mode)) {
          if log_children {
            eprintln!(
              "[intrinsic-inline-run-cache] parent_id={} mode={:?} ids={:?} width={:.2}",
              box_node.id, mode, key, width
            );
          }
          *widest = widest.max(*width);
          run.clear();
          return Ok(());
        }

        let inline_container = BoxNode::new_inline(box_node.style.clone(), run.clone());
        let width = inline_fc.compute_intrinsic_inline_size(&inline_container, mode)?;
        inline_run_cache.insert((key, mode), width);
        if log_children {
          let ids: Vec<usize> = run.iter().map(|c| c.id()).collect();
          eprintln!(
            "[intrinsic-inline-run] parent_id={} mode={:?} ids={:?} width={:.2}",
            box_node.id, mode, ids, width
          );
        }
        *widest = widest.max(width);
        run.clear();
        Ok(())
      };

    let mut inline_child_debug: Vec<(usize, Display)> = Vec::new();
    for child in &box_node.children {
      if is_out_of_flow(child) {
        continue;
      }

      // Floats are out-of-flow for intrinsic sizing; they shouldn't contribute to the
      // parent’s min/max-content inline size.
      if child.style.float.is_floating() {
        continue;
      }

      let treated_as_block = match child.box_type {
        BoxType::Replaced(_) if child.style.display.is_inline_level() => false,
        _ => child.is_block_level(),
      };

      if treated_as_block {
        flush_inline_run(&mut inline_run, &mut inline_width)?;
      } else {
        if log_children {
          inline_child_debug.push((child.id, child.style.display));
        }
        inline_run.push(child.clone());
      }
    }
    flush_inline_run(&mut inline_run, &mut inline_width)?;

    // Block-level in-flow children contribute their own intrinsic widths
    let mut block_child_width = 0.0f32;
    for child in &box_node.children {
      if !child.is_block_level() || is_out_of_flow(child) {
        continue;
      }

      // Ignore floats when computing intrinsic inline sizes; they are out-of-flow.
      if child.style.float.is_floating() {
        continue;
      }

      let fc_type = child
        .formatting_context()
        .unwrap_or(FormattingContextType::Block);
      let fc = factory.create(fc_type);
      let child_width = fc.compute_intrinsic_inline_size(child, mode)?;
      block_child_width = block_child_width.max(child_width);
      if log_children {
        let sel = child
          .debug_info
          .as_ref()
          .map(|d| d.to_selector())
          .unwrap_or_else(|| "<anon>".to_string());
        let disp = child.style.display;
        eprintln!(
                    "[intrinsic-child] parent_id={} child_id={} selector={} display={:?} mode={:?} width={:.2}",
                    box_node.id, child.id, sel, disp, mode, child_width
                );
      }
    }

    let content_width = inline_width.max(block_child_width);

    // Add this box's own padding and borders
    let mut width = content_width + edges;

    // Apply min/max constraints to the border box
    let min_width = style
      .min_width
      .map(|l| resolve_length_for_width(l, 0.0, style, &self.font_context, self.viewport_size))
      .map(|w| border_size_from_box_sizing(w, edges, style.box_sizing))
      .unwrap_or(0.0);
    let max_width = style
      .max_width
      .map(|l| resolve_length_for_width(l, 0.0, style, &self.font_context, self.viewport_size))
      .map(|w| border_size_from_box_sizing(w, edges, style.box_sizing))
      .unwrap_or(f32::INFINITY);
    let (min_width, max_width) = if max_width < min_width {
      (min_width, min_width)
    } else {
      (min_width, max_width)
    };
    width = crate::layout::utils::clamp_with_order(width, min_width, max_width);

    let clamped = width.max(0.0);
    // Optional tracing for over-large intrinsic widths.
    if !log_ids.is_empty() && log_ids.contains(&box_node.id) {
      let selector = box_node
        .debug_info
        .as_ref()
        .map(|d| d.to_selector())
        .unwrap_or_else(|| "<anon>".to_string());
      if !inline_child_debug.is_empty() {
        eprintln!(
          "[intrinsic-inline-children] parent_id={} ids={:?}",
          box_node.id, inline_child_debug
        );
      }
      eprintln!(
                    "[intrinsic-width] id={} selector={} mode={:?} inline_width={:.2} block_width={:.2} content_width={:.2} edges={:.2} min={:.2} max={:.2} result={:.2}",
                    box_node.id,
                selector,
                mode,
                inline_width,
                block_child_width,
                content_width,
                edges,
                min_width,
                max_width,
                clamped
            );
    }
    intrinsic_cache_store(box_node, mode, clamped);
    Ok(clamped)
  }
}

/// Checks if a box is out of normal flow (absolute/fixed positioned or float)
fn is_out_of_flow(box_node: &BoxNode) -> bool {
  let position = box_node.style.position;
  matches!(position, Position::Absolute | Position::Fixed)
}

fn count_text_fragments(fragment: &FragmentNode) -> (usize, usize) {
  fn walk(node: &FragmentNode, text: &mut usize, total: &mut usize) {
    *total += 1;
    if matches!(node.content, FragmentContent::Text { .. }) {
      *text += 1;
    }
    for child in &node.children {
      walk(child, text, total);
    }
  }

  let mut text = 0;
  let mut total = 0;
  walk(fragment, &mut text, &mut total);
  (text, total)
}

fn collect_first_texts(fragment: &FragmentNode, out: &mut Vec<String>, limit: usize) {
  fn walk(node: &FragmentNode, out: &mut Vec<String>, limit: usize) {
    if out.len() >= limit {
      return;
    }
    if let FragmentContent::Text { text, .. } = &node.content {
      out.push(text.clone());
      if out.len() >= limit {
        return;
      }
    }
    for child in &node.children {
      walk(child, out, limit);
      if out.len() >= limit {
        return;
      }
    }
  }

  walk(fragment, out, limit);
}

fn trace_positioned_ids() -> &'static Vec<usize> {
  static IDS: std::sync::OnceLock<Vec<usize>> = std::sync::OnceLock::new();
  IDS.get_or_init(|| {
    std::env::var("FASTR_TRACE_POSITIONED")
      .ok()
      .map(|v| {
        v.split(',')
          .filter_map(|tok| tok.trim().parse::<usize>().ok())
          .collect::<Vec<_>>()
      })
      .unwrap_or_default()
  })
}

fn trace_block_text_ids() -> &'static Vec<usize> {
  static IDS: std::sync::OnceLock<Vec<usize>> = std::sync::OnceLock::new();
  IDS.get_or_init(|| {
    std::env::var("FASTR_TRACE_BLOCK_TEXT")
      .ok()
      .map(|v| {
        v.split(',')
          .filter_map(|tok| tok.trim().parse::<usize>().ok())
          .collect::<Vec<_>>()
      })
      .unwrap_or_default()
  })
}

fn resolve_length_for_width(
  length: Length,
  percentage_base: f32,
  style: &ComputedStyle,
  font_context: &FontContext,
  viewport: crate::geometry::Size,
) -> f32 {
  let base = if percentage_base.is_finite() {
    Some(percentage_base)
  } else {
    None
  };
  resolve_length_with_percentage_metrics(
    length,
    base,
    viewport,
    style.font_size,
    style.root_font_size,
    Some(style),
    Some(font_context),
  )
  .unwrap_or(0.0)
}

fn horizontal_padding_and_borders(
  style: &ComputedStyle,
  percentage_base: f32,
  viewport: crate::geometry::Size,
  font_context: &FontContext,
) -> f32 {
  resolve_length_for_width(
    style.padding_left,
    percentage_base,
    style,
    font_context,
    viewport,
  ) + resolve_length_for_width(
    style.padding_right,
    percentage_base,
    style,
    font_context,
    viewport,
  ) + resolve_length_for_width(
    style.border_left_width,
    percentage_base,
    style,
    font_context,
    viewport,
  ) + resolve_length_for_width(
    style.border_right_width,
    percentage_base,
    style,
    font_context,
    viewport,
  )
}

fn recompute_margins_for_width(
  style: &ComputedStyle,
  containing_width: f32,
  content_width: f32,
  border_left: f32,
  padding_left: f32,
  padding_right: f32,
  border_right: f32,
  viewport: crate::geometry::Size,
  font_context: &FontContext,
) -> (f32, f32) {
  let margin_left = match &style.margin_left {
    Some(len) => MarginValue::Length(resolve_length_for_width(
      *len,
      containing_width,
      style,
      font_context,
      viewport,
    )),
    None => MarginValue::Auto,
  };
  let margin_right = match &style.margin_right {
    Some(len) => MarginValue::Length(resolve_length_for_width(
      *len,
      containing_width,
      style,
      font_context,
      viewport,
    )),
    None => MarginValue::Auto,
  };

  let borders_and_padding = border_left + padding_left + padding_right + border_right;

  match (margin_left, margin_right) {
    (MarginValue::Auto, MarginValue::Auto) => {
      let remaining = containing_width - borders_and_padding - content_width;
      let margin = (remaining / 2.0).max(0.0);
      (margin, margin)
    }
    (MarginValue::Auto, MarginValue::Length(mr)) => {
      let ml = containing_width - borders_and_padding - content_width - mr;
      (ml, mr)
    }
    (MarginValue::Length(ml), MarginValue::Auto) => {
      let mr = containing_width - borders_and_padding - content_width - ml;
      (ml, mr)
    }
    (MarginValue::Length(ml), MarginValue::Length(_mr)) => {
      let mr = containing_width - borders_and_padding - content_width - ml;
      (ml, mr)
    }
  }
}
fn resolve_margin_side(
  style: &ComputedStyle,
  side: PhysicalSide,
  percentage_base: f32,
  font_context: &FontContext,
  viewport: crate::geometry::Size,
) -> f32 {
  let length = match side {
    PhysicalSide::Top => style.margin_top,
    PhysicalSide::Right => style.margin_right,
    PhysicalSide::Bottom => style.margin_bottom,
    PhysicalSide::Left => style.margin_left,
  };
  length
    .map(|l| resolve_length_for_width(l, percentage_base, style, font_context, viewport))
    .unwrap_or(0.0)
}

fn resolve_padding_side(
  style: &ComputedStyle,
  side: PhysicalSide,
  percentage_base: f32,
  font_context: &FontContext,
  viewport: crate::geometry::Size,
) -> f32 {
  let length = match side {
    PhysicalSide::Top => style.padding_top,
    PhysicalSide::Right => style.padding_right,
    PhysicalSide::Bottom => style.padding_bottom,
    PhysicalSide::Left => style.padding_left,
  };
  resolve_length_for_width(length, percentage_base, style, font_context, viewport)
}

fn resolve_border_side(
  style: &ComputedStyle,
  side: PhysicalSide,
  percentage_base: f32,
  font_context: &FontContext,
  viewport: crate::geometry::Size,
) -> f32 {
  let length = match side {
    PhysicalSide::Top => style.border_top_width,
    PhysicalSide::Right => style.border_right_width,
    PhysicalSide::Bottom => style.border_bottom_width,
    PhysicalSide::Left => style.border_left_width,
  };
  resolve_length_for_width(length, percentage_base, style, font_context, viewport)
}
#[cfg(test)]
mod tests {
  use super::*;
  use crate::layout::contexts::inline::InlineFormattingContext;
  use crate::layout::formatting_context::IntrinsicSizingMode;
  use crate::style::display::Display;
  use crate::style::display::FormattingContextType;
  use crate::style::position::Position;
  use crate::style::types::ListStylePosition;
  use crate::style::types::ListStyleType;
  use crate::style::types::Overflow;
  use crate::style::types::ScrollbarWidth;
  use crate::style::values::Length;
  use crate::style::ComputedStyle;
  use crate::text::font_loader::FontContext;
  use crate::tree::box_generation::BoxGenerator;
  use crate::tree::box_generation::DOMNode;
  use crate::tree::fragment_tree::FragmentContent;
  use std::sync::Arc;

  fn default_style() -> Arc<ComputedStyle> {
    let mut style = ComputedStyle::default();
    style.display = Display::Block;
    Arc::new(style)
  }

  fn block_style_with_height(height: f32) -> Arc<ComputedStyle> {
    let mut style = ComputedStyle::default();
    style.display = Display::Block;
    style.height = Some(Length::px(height));
    Arc::new(style)
  }

  #[test]
  fn horizontal_scrollbar_reserves_gutter_height() {
    let mut style = ComputedStyle::default();
    style.display = Display::Block;
    style.overflow_x = Overflow::Scroll;
    style.scrollbar_width = ScrollbarWidth::Thin;

    let node = BoxNode::new_block(Arc::new(style), FormattingContextType::Block, vec![]);
    let fc = BlockFormattingContext::with_font_context_viewport_and_cb(
      FontContext::new(),
      Size::new(200.0, 200.0),
      ContainingBlock::viewport(Size::new(200.0, 200.0)),
    );
    let constraints =
      LayoutConstraints::new(AvailableSpace::Definite(200.0), AvailableSpace::Indefinite);
    let fragment = fc.layout(&node, &constraints).unwrap();

    assert!((fragment.bounds.height() - 8.0).abs() < 0.01);
  }

  fn block_style_with_margin(margin: f32) -> Arc<ComputedStyle> {
    let mut style = ComputedStyle::default();
    style.display = Display::Block;
    style.margin_top = Some(Length::px(margin));
    style.margin_bottom = Some(Length::px(margin));
    style.margin_left = Some(Length::px(margin));
    style.margin_right = Some(Length::px(margin));
    Arc::new(style)
  }

  #[test]
  fn test_bfc_new() {
    let _bfc = BlockFormattingContext::new();
  }

  #[test]
  fn test_layout_empty_block() {
    let bfc = BlockFormattingContext::new();
    let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
    let constraints = LayoutConstraints::definite(800.0, 600.0);

    let fragment = bfc.layout(&root, &constraints).unwrap();

    assert_eq!(fragment.bounds.width(), 800.0);
    assert_eq!(fragment.bounds.height(), 0.0);
  }

  #[test]
  fn test_layout_block_with_explicit_height() {
    let bfc = BlockFormattingContext::new();
    let root = BoxNode::new_block(
      block_style_with_height(200.0),
      FormattingContextType::Block,
      vec![],
    );
    let constraints = LayoutConstraints::definite(800.0, 600.0);

    let fragment = bfc.layout(&root, &constraints).unwrap();

    assert_eq!(fragment.bounds.width(), 800.0);
    assert_eq!(fragment.bounds.height(), 200.0);
  }

  #[test]
  fn legend_auto_width_shrinks_to_content() {
    let mut legend_style = ComputedStyle::default();
    legend_style.display = Display::Block;
    legend_style.shrink_to_fit_inline_size = true;

    let mut legend_child_style = ComputedStyle::default();
    legend_child_style.display = Display::Block;
    legend_child_style.width = Some(Length::px(80.0));
    legend_child_style.height = Some(Length::px(10.0));
    let legend_child = BoxNode::new_block(
      Arc::new(legend_child_style),
      FormattingContextType::Block,
      vec![],
    );

    let legend = BoxNode::new_block(Arc::new(legend_style), FormattingContextType::Block, vec![
      legend_child,
    ]);

    let mut sibling_style = ComputedStyle::default();
    sibling_style.display = Display::Block;
    let sibling = BoxNode::new_block(
      Arc::new(sibling_style),
      FormattingContextType::Block,
      vec![],
    );

    let fc = BlockFormattingContext::with_font_context_viewport_and_cb(
      FontContext::new(),
      Size::new(200.0, 200.0),
      ContainingBlock::viewport(Size::new(200.0, 200.0)),
    );
    let constraints =
      LayoutConstraints::new(AvailableSpace::Definite(200.0), AvailableSpace::Indefinite);
    let solo = fc.layout(&legend, &constraints).expect("legend layout");
    assert!(
      (solo.bounds.width() - 80.0).abs() < 0.1,
      "legend root should shrink to its contents; got width {}",
      solo.bounds.width()
    );

    let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![
      legend, sibling,
    ]);
    let fragment = fc.layout(&root, &constraints).expect("block layout");

    assert_eq!(
      fragment.children.len(),
      2,
      "root should produce two children"
    );
    let legend_fragment = &fragment.children[0];
    assert!(
      legend_fragment
        .style
        .as_ref()
        .map(|s| s.shrink_to_fit_inline_size)
        .unwrap_or(false),
      "legend fragment should carry shrink-to-fit flag"
    );
    assert!(
      (legend_fragment.bounds.width() - 80.0).abs() < 0.1,
      "legend should shrink to its contents; got width {}",
      legend_fragment.bounds.width()
    );
    assert!(
      legend_fragment.bounds.x().abs() < 0.01,
      "legend should start at the origin"
    );

    let sibling_fragment = &fragment.children[1];
    assert!(
      (sibling_fragment.bounds.width() - 200.0).abs() < 0.1,
      "normal block should span the containing width; got width {}",
      sibling_fragment.bounds.width()
    );
    assert!(
      sibling_fragment.bounds.x().abs() < 0.01,
      "sibling should start at the container origin; got {}",
      sibling_fragment.bounds.x()
  );
}

  #[test]
  fn test_layout_nested_blocks() {
    let bfc = BlockFormattingContext::new();

    let child1 = BoxNode::new_block(
      block_style_with_height(100.0),
      FormattingContextType::Block,
      vec![],
    );
    let child2 = BoxNode::new_block(
      block_style_with_height(150.0),
      FormattingContextType::Block,
      vec![],
    );

    let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![
      child1, child2,
    ]);
    let constraints = LayoutConstraints::definite(800.0, 600.0);

    let fragment = bfc.layout(&root, &constraints).unwrap();

    assert_eq!(fragment.children.len(), 2);
    assert!(fragment.bounds.height() >= 250.0);
  }

  #[test]
  fn relative_block_offsets_fragment_without_affecting_flow_size() {
    let mut relative_style = ComputedStyle::default();
    relative_style.display = Display::Block;
    relative_style.position = Position::Relative;
    relative_style.left = Some(Length::px(30.0));
    relative_style.top = Some(Length::px(20.0));
    relative_style.width = Some(Length::px(100.0));
    relative_style.height = Some(Length::px(40.0));

    let child = BoxNode::new_block(
      Arc::new(relative_style),
      FormattingContextType::Block,
      vec![],
    );
    let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![child]);
    let constraints = LayoutConstraints::definite(300.0, 200.0);

    let fragment = BlockFormattingContext::new()
      .layout(&root, &constraints)
      .unwrap();

    assert_eq!(fragment.bounds.height(), 40.0);
    let child_fragment = fragment.children.first().expect("child");
    assert_eq!(child_fragment.bounds.width(), 100.0);
    assert_eq!(child_fragment.bounds.height(), 40.0);
    assert_eq!(child_fragment.bounds.x(), 30.0);
    assert_eq!(child_fragment.bounds.y(), 20.0);
  }

  #[test]
  fn relative_block_percentage_offsets_use_containing_block() {
    let mut relative_style = ComputedStyle::default();
    relative_style.display = Display::Block;
    relative_style.position = Position::Relative;
    relative_style.left = Some(Length::percent(50.0)); // 50% of 200 = 100
    relative_style.top = Some(Length::percent(25.0)); // 25% of 120 = 30
    relative_style.width = Some(Length::px(40.0));
    relative_style.height = Some(Length::px(10.0));

    let child = BoxNode::new_block(
      Arc::new(relative_style),
      FormattingContextType::Block,
      vec![],
    );
    let mut root_style = ComputedStyle::default();
    root_style.display = Display::Block;
    root_style.height = Some(Length::px(120.0));
    let root = BoxNode::new_block(Arc::new(root_style), FormattingContextType::Block, vec![
      child,
    ]);
    let constraints = LayoutConstraints::definite(200.0, 200.0);

    let fragment = BlockFormattingContext::new()
      .layout(&root, &constraints)
      .unwrap();

    let child_fragment = fragment.children.first().expect("child");
    assert_eq!(child_fragment.bounds.x(), 100.0);
    assert_eq!(child_fragment.bounds.y(), 30.0);
  }

  #[test]
  fn percentage_height_uses_definite_containing_block() {
    let bfc = BlockFormattingContext::new();

    let mut child_style = ComputedStyle::default();
    child_style.display = Display::Block;
    child_style.height = Some(Length::percent(50.0));
    let child = BoxNode::new_block(Arc::new(child_style), FormattingContextType::Block, vec![]);
    let mut root_style = ComputedStyle::default();
    root_style.display = Display::Block;
    root_style.height = Some(Length::px(300.0));
    let root = BoxNode::new_block(Arc::new(root_style), FormattingContextType::Block, vec![
      child,
    ]);
    let constraints = LayoutConstraints::definite(200.0, 400.0);

    let fragment = bfc.layout(&root, &constraints).unwrap();
    let child_fragment = fragment.children.first().expect("child fragment");
    assert!((child_fragment.bounds.height() - 150.0).abs() < 0.1);
  }

  #[test]
  fn aspect_ratio_sets_auto_height() {
    let bfc = BlockFormattingContext::new();

    let mut child_style = ComputedStyle::default();
    child_style.display = Display::Block;
    child_style.aspect_ratio = crate::style::types::AspectRatio::Ratio(2.0);
    let child = BoxNode::new_block(Arc::new(child_style), FormattingContextType::Block, vec![]);

    let mut root_style = ComputedStyle::default();
    root_style.display = Display::Block;
    let root = BoxNode::new_block(Arc::new(root_style), FormattingContextType::Block, vec![
      child,
    ]);
    let constraints = LayoutConstraints::definite(200.0, 400.0);

    let fragment = bfc.layout(&root, &constraints).unwrap();
    let child_fragment = fragment.children.first().expect("child fragment");
    assert_eq!(child_fragment.bounds.height(), 100.0);
  }

  #[test]
  fn percentage_height_without_base_falls_back_to_auto() {
    let bfc = BlockFormattingContext::new();
    let mut child_style = ComputedStyle::default();
    child_style.display = Display::Block;
    child_style.height = Some(Length::percent(60.0));
    let child = BoxNode::new_block(Arc::new(child_style), FormattingContextType::Block, vec![]);
    let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![child]);
    let constraints = LayoutConstraints::definite_width(200.0);

    let fragment = bfc.layout(&root, &constraints).unwrap();
    let child_fragment = fragment.children.first().expect("child fragment");
    assert_eq!(child_fragment.bounds.height(), 0.0);
  }

  #[test]
  fn test_sibling_margin_collapse() {
    let bfc = BlockFormattingContext::new();

    let child1 = BoxNode::new_block(
      block_style_with_margin(20.0),
      FormattingContextType::Block,
      vec![],
    );
    let child2 = BoxNode::new_block(
      block_style_with_margin(30.0),
      FormattingContextType::Block,
      vec![],
    );

    let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![
      child1, child2,
    ]);
    let constraints = LayoutConstraints::definite(800.0, 600.0);

    let fragment = bfc.layout(&root, &constraints).unwrap();
    assert_eq!(fragment.children.len(), 2);
  }

  #[test]
  fn test_fc_is_send_sync() {
    fn assert_send_sync<T: Send + Sync>() {}
    assert_send_sync::<BlockFormattingContext>();
  }

  #[test]
  fn floats_extend_height_and_clear_moves_following_block() {
    let bfc = BlockFormattingContext::new();

    let mut float_style = ComputedStyle::default();
    float_style.display = Display::Block;
    float_style.float = Float::Left;
    float_style.width = Some(Length::px(60.0));
    float_style.height = Some(Length::px(50.0));
    let float_node =
      BoxNode::new_block(Arc::new(float_style), FormattingContextType::Block, vec![]);

    let mut cleared_style = ComputedStyle::default();
    cleared_style.display = Display::Block;
    cleared_style.clear = crate::style::float::Clear::Left;
    cleared_style.height = Some(Length::px(10.0));
    let cleared_node = BoxNode::new_block(
      Arc::new(cleared_style),
      FormattingContextType::Block,
      vec![],
    );

    let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![
      float_node,
      cleared_node,
    ]);
    let constraints = LayoutConstraints::definite(200.0, 400.0);

    let fragment = bfc.layout(&root, &constraints).unwrap();
    assert!(
      fragment.bounds.height() >= 60.0,
      "BFC height should include float and cleared block; got {}",
      fragment.bounds.height()
    );

    let mut float_y = None;
    let mut clear_y = None;
    for child in &fragment.children {
      if let Some(style) = &child.style {
        if style.float.is_floating() {
          float_y = Some(child.bounds.y());
        }
        if matches!(
          style.clear,
          crate::style::float::Clear::Left | crate::style::float::Clear::Both
        ) {
          clear_y = Some(child.bounds.y());
        }
      }
    }

    let float_y = float_y.expect("float fragment");
    let clear_y = clear_y.expect("cleared fragment");
    assert!(float_y.abs() < 0.01);
    assert!(
      clear_y >= 50.0,
      "cleared block should be pushed below float; got clear_y={clear_y}"
    );
  }

  #[test]
  fn inline_lines_shorten_next_to_float() {
    let bfc = BlockFormattingContext::new();

    let mut float_style = ComputedStyle::default();
    float_style.display = Display::Block;
    float_style.float = Float::Left;
    float_style.width = Some(Length::px(80.0));
    float_style.height = Some(Length::px(20.0));
    let float_node =
      BoxNode::new_block(Arc::new(float_style), FormattingContextType::Block, vec![]);

    let text = BoxNode::new_text(default_style(), "text".to_string());
    let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![
      float_node,
      BoxNode::new_inline(default_style(), vec![text]),
    ]);
    let constraints = LayoutConstraints::definite(200.0, 200.0);

    let fragment = bfc.layout(&root, &constraints).unwrap();

    fn find_line(fragment: &FragmentNode) -> Option<&FragmentNode> {
      if matches!(fragment.content, FragmentContent::Line { .. }) {
        return Some(fragment);
      }
      for child in &fragment.children {
        if let Some(line) = find_line(child) {
          return Some(line);
        }
      }
      None
    }

    let line = find_line(&fragment).expect("line fragment");
    assert!(
      line.bounds.width() <= 120.0,
      "line width should be shortened by float; got {}",
      line.bounds.width()
    );
    assert!(
      line.bounds.x() >= 79.9,
      "line should start after the float; got x={}",
      line.bounds.x()
    );
  }

  #[test]
  fn float_auto_width_shrinks_to_available_space_next_to_float() {
    let bfc = BlockFormattingContext::new();

    let mut wide_style = ComputedStyle::default();
    wide_style.display = Display::Block;
    wide_style.float = Float::Left;
    wide_style.width = Some(Length::px(120.0));
    wide_style.height = Some(Length::px(20.0));
    let wide_float = BoxNode::new_block(Arc::new(wide_style), FormattingContextType::Block, vec![]);

    let mut auto_style = ComputedStyle::default();
    auto_style.display = Display::Block;
    auto_style.float = Float::Left;
    let text = BoxNode::new_text(default_style(), "word ".repeat(20));
    let auto_float = BoxNode::new_block(Arc::new(auto_style), FormattingContextType::Block, vec![
      BoxNode::new_inline(default_style(), vec![text]),
    ]);

    let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![
      wide_float, auto_float,
    ]);
    let constraints =
      LayoutConstraints::new(AvailableSpace::Definite(200.0), AvailableSpace::Indefinite);

    let fragment = bfc.layout(&root, &constraints).unwrap();

    let floats: Vec<_> = fragment
      .children
      .iter()
      .filter(|child| {
        child
          .style
          .as_ref()
          .map(|s| s.float.is_floating())
          .unwrap_or(false)
      })
      .collect();

    assert_eq!(floats.len(), 2);

    let mut wide = None;
    let mut auto = None;
    for float in floats {
      if (float.bounds.width() - 120.0).abs() < 0.5 {
        wide = Some(float);
      } else {
        auto = Some(float);
      }
    }

    let wide = wide.expect("wide float fragment");
    let auto = auto.expect("auto float fragment");

    assert!(
      auto.bounds.y() < 0.01,
      "auto float should stay alongside the existing float; got y={}",
      auto.bounds.y()
    );
    assert!(
      (auto.bounds.x() - wide.bounds.width()).abs() < 0.5,
      "auto float should start after the first float; got x={}",
      auto.bounds.x()
    );
    assert!(
      auto.bounds.width() <= 90.0,
      "auto float should shrink to the available 80px space; got {}",
      auto.bounds.width()
    );
  }

  #[test]
  fn list_marker_outside_positions_marker_left_of_text() {
    let generator = BoxGenerator::new();

    let mut li_style = ComputedStyle::default();
    li_style.display = Display::ListItem;
    li_style.list_style_position = ListStylePosition::Outside;
    let li_style = Arc::new(li_style);

    let mut ul_style = ComputedStyle::default();
    ul_style.display = Display::Block;
    let ul_style = Arc::new(ul_style);

    let li = DOMNode::new_element("li", li_style.clone(), vec![DOMNode::new_text(
      "Item",
      li_style.clone(),
    )]);
    let ul = DOMNode::new_element("ul", ul_style, vec![li]);
    let box_tree = generator.generate(&ul).unwrap();

    let bfc = BlockFormattingContext::new();
    let constraints = LayoutConstraints::definite(200.0, 200.0);
    let fragment = bfc.layout(&box_tree.root, &constraints).unwrap();

    let li_fragment = fragment.children.first().expect("li fragment");
    fn find_line(fragment: &FragmentNode) -> Option<&FragmentNode> {
      if matches!(fragment.content, FragmentContent::Line { .. }) {
        return Some(fragment);
      }
      for child in &fragment.children {
        if let Some(line) = find_line(child) {
          return Some(line);
        }
      }
      None
    }

    let line = find_line(li_fragment).expect("line fragment");

    let marker = line
      .children
      .iter()
      .find(|child| {
        child
          .style
          .as_ref()
          .map(|s| s.list_style_type == ListStyleType::None)
          .unwrap_or(false)
      })
      .expect("marker fragment");
    let text = line
      .children
      .iter()
      .find(|child| {
        child
          .style
          .as_ref()
          .map(|s| s.list_style_type != ListStyleType::None)
          .unwrap_or(false)
      })
      .expect("text fragment");

    assert!(marker.bounds.x() < 0.0);
    assert!(text.bounds.x() >= 0.0);
  }

  #[test]
  fn intrinsic_inline_size_splits_runs_around_block_children() {
    let bfc = BlockFormattingContext::new();
    let ifc = InlineFormattingContext::new();

    let text_left = BoxNode::new_text(default_style(), "unbreakable".to_string());
    let text_right = BoxNode::new_text(default_style(), "unbreakable".to_string());
    let block_child = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);

    let run_container = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![
      text_left.clone(),
    ]);
    let run_min = ifc
      .compute_intrinsic_inline_size(&run_container, IntrinsicSizingMode::MinContent)
      .unwrap();
    let run_max = ifc
      .compute_intrinsic_inline_size(&run_container, IntrinsicSizingMode::MaxContent)
      .unwrap();

    let root = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![
      text_left,
      block_child,
      text_right,
    ]);
    let min_width = bfc
      .compute_intrinsic_inline_size(&root, IntrinsicSizingMode::MinContent)
      .unwrap();
    assert!(
      min_width <= run_min * 1.1,
      "min-content width should follow the widest inline run, got {min_width} vs run {run_min}"
    );

    let max_width = bfc
      .compute_intrinsic_inline_size(&root, IntrinsicSizingMode::MaxContent)
      .unwrap();
    assert!(
            max_width <= run_max * 1.1,
            "max-content width should not concatenate inline runs across blocks, got {max_width} vs run {run_max}"
        );
  }

  #[test]
  fn size_containment_zeroes_intrinsic_inline_contribution() {
    let mut style = (*default_style()).clone();
    style.containment =
      crate::style::types::Containment::with_flags(true, false, false, false, false);
    style.padding_left = Length::px(4.0);
    style.padding_right = Length::px(4.0);
    style.border_left_width = Length::px(2.0);
    style.border_right_width = Length::px(2.0);
    let container = BoxNode::new_block(Arc::new(style), FormattingContextType::Block, vec![
      BoxNode::new_text(default_style(), "superlongword".to_string()),
    ]);

    let bfc = BlockFormattingContext::new();
    let max = bfc
      .compute_intrinsic_inline_size(&container, IntrinsicSizingMode::MaxContent)
      .unwrap();
    let min = bfc
      .compute_intrinsic_inline_size(&container, IntrinsicSizingMode::MinContent)
      .unwrap();

    assert!((max - 12.0).abs() < 0.001);
    assert!((min - 12.0).abs() < 0.001);
  }

  #[test]
  fn absolutely_positioned_child_uses_padding_containing_block() {
    let mut parent_style = ComputedStyle::default();
    parent_style.display = Display::Block;
    parent_style.width = Some(Length::px(200.0));
    parent_style.padding_left = Length::px(10.0);
    parent_style.padding_top = Length::px(10.0);
    let mut child_style = ComputedStyle::default();
    child_style.display = Display::Block;
    child_style.position = Position::Absolute;
    child_style.left = Some(Length::px(5.0));
    child_style.top = Some(Length::px(7.0));
    child_style.width = Some(Length::px(50.0));
    child_style.height = Some(Length::px(20.0));

    let child = BoxNode::new_block(Arc::new(child_style), FormattingContextType::Block, vec![]);
    let parent = BoxNode::new_block(Arc::new(parent_style), FormattingContextType::Block, vec![
      child,
    ]);

    let fc = BlockFormattingContext::new();
    let constraints = LayoutConstraints::definite(300.0, 300.0);
    let fragment = fc.layout(&parent, &constraints).unwrap();

    assert_eq!(fragment.children.len(), 1);
    let child_frag = &fragment.children[0];
    assert_eq!(child_frag.bounds.x(), 5.0);
    assert_eq!(child_frag.bounds.y(), 7.0);
    assert_eq!(child_frag.bounds.width(), 50.0);
    assert_eq!(child_frag.bounds.height(), 20.0);
  }

  #[test]
  fn absolute_children_inside_block_descendants_are_laid_out() {
    // Regression: positioned children collected during block child layout were dropped.
    let mut root_style = ComputedStyle::default();
    root_style.display = Display::Block;
    root_style.width = Some(Length::px(400.0));

    let mut middle_style = ComputedStyle::default();
    middle_style.display = Display::Block;
    middle_style.width = Some(Length::px(200.0));
    middle_style.padding_left = Length::px(10.0);
    middle_style.padding_top = Length::px(10.0);
    middle_style.height = Some(Length::px(80.0));

    let mut abs_style = ComputedStyle::default();
    abs_style.display = Display::Block;
    abs_style.position = Position::Absolute;
    abs_style.left = Some(Length::px(5.0));
    abs_style.top = Some(Length::px(7.0));
    abs_style.width = Some(Length::px(30.0));
    abs_style.height = Some(Length::px(12.0));

    let abs_child = BoxNode::new_block(Arc::new(abs_style), FormattingContextType::Block, vec![]);
    let middle = BoxNode::new_block(Arc::new(middle_style), FormattingContextType::Block, vec![
      abs_child,
    ]);
    let root = BoxNode::new_block(Arc::new(root_style), FormattingContextType::Block, vec![
      middle,
    ]);

    let fc = BlockFormattingContext::new();
    let constraints = LayoutConstraints::definite(500.0, 500.0);
    let fragment = fc.layout(&root, &constraints).unwrap();

    assert_eq!(fragment.children.len(), 1);
    let middle_frag = &fragment.children[0];
    assert_eq!(
      middle_frag.children.len(),
      1,
      "positioned child should be laid out"
    );
    let abs_frag = &middle_frag.children[0];
    // Positioned child should still be included; coordinates are resolved relative to the
    // containing block origin (padding box in our implementation).
    assert_eq!(abs_frag.bounds.x(), 5.0);
    assert_eq!(abs_frag.bounds.y(), 7.0);
    assert_eq!(abs_frag.bounds.width(), 30.0);
    assert_eq!(abs_frag.bounds.height(), 12.0);
  }
}
