//! Display List Optimization
//!
//! This module provides advanced optimization passes for display lists:
//!
//! - **Viewport culling**: Skip items outside the visible area
//! - **Transparent item removal**: Remove fully transparent items
//! - **No-op removal**: Remove identity transforms, 1.0 opacity, normal blend
//! - **Adjacent fill merging**: Combine adjacent fills with the same color
//!
//! The optimizer builds on the basic `DisplayList::cull()` and `DisplayList::optimize()`
//! methods with additional passes and configurable behavior.
//!
//! # Example
//!
//! ```rust,ignore
//! use fastrender::paint::{DisplayList, DisplayListOptimizer, OptimizationConfig};
//! use fastrender::Rect;
//!
//! let list = DisplayList::new();
//! // ... build display list ...
//!
//! let viewport = Rect::from_xywh(0.0, 0.0, 800.0, 600.0);
//! let optimizer = DisplayListOptimizer::new();
//! let (optimized, stats) = optimizer.optimize(list, viewport);
//!
//! println!("Reduced items by {:.1}%", stats.reduction_percentage());
//! ```

use crate::geometry::Rect;
use crate::paint::display_list::BlendMode;
use crate::paint::display_list::DisplayItem;
use crate::paint::display_list::DisplayList;
use crate::paint::display_list::FillRectItem;
use crate::paint::display_list::ResolvedFilter;
use crate::paint::display_list::StackingContextItem;

// ============================================================================
// Optimization Configuration
// ============================================================================

/// Configuration for display list optimization passes
///
/// Each pass can be individually enabled or disabled. All passes are enabled
/// by default for maximum optimization.
#[derive(Debug, Clone)]
pub struct OptimizationConfig {
  /// Enable viewport culling (remove items outside viewport)
  pub enable_culling: bool,

  /// Enable transparent item removal
  pub enable_transparent_removal: bool,

  /// Enable adjacent fill merging
  pub enable_fill_merging: bool,

  /// Enable no-op removal (identity transforms, 1.0 opacity, etc.)
  pub enable_noop_removal: bool,
}

impl Default for OptimizationConfig {
  fn default() -> Self {
    Self {
      enable_culling: true,
      enable_transparent_removal: true,
      enable_fill_merging: true,
      enable_noop_removal: true,
    }
  }
}

// ============================================================================
// Optimization Statistics
// ============================================================================

/// Statistics from the optimization pass
///
/// Provides insight into what the optimizer did and how effective it was.
#[derive(Debug, Clone, Default)]
pub struct OptimizationStats {
  /// Original number of items before optimization
  pub original_count: usize,

  /// Final number of items after optimization
  pub final_count: usize,

  /// Items removed by viewport culling
  pub culled_count: usize,

  /// Transparent items removed
  pub transparent_removed: usize,

  /// Items merged into fewer items
  pub merged_count: usize,

  /// No-op items removed (identity transforms, etc.)
  pub noop_removed: usize,
}

impl OptimizationStats {
  /// Calculate the reduction percentage
  ///
  /// Returns the percentage of items removed (0-100).
  pub fn reduction_percentage(&self) -> f32 {
    if self.original_count == 0 {
      return 0.0;
    }
    let removed = self.original_count - self.final_count;
    (removed as f32 / self.original_count as f32) * 100.0
  }
}

// ============================================================================
// Display List Optimizer
// ============================================================================

/// Advanced display list optimizer
///
/// Applies configurable optimization passes to reduce the number of display
/// items while preserving visual correctness.
///
/// # Optimization Order
///
/// 1. Transparent removal - removes invisible items early
/// 2. No-op removal - removes identity operations
/// 3. Viewport culling - removes items outside visible area
/// 4. Fill merging - combines adjacent same-color fills
///
/// # Example
///
/// ```rust,ignore
/// let optimizer = DisplayListOptimizer::with_config(OptimizationConfig {
///     enable_culling: true,
///     enable_transparent_removal: true,
///     enable_fill_merging: false, // Disable merging
///     enable_noop_removal: true,
/// });
/// let (optimized, stats) = optimizer.optimize(list, viewport);
/// ```
#[derive(Debug, Clone)]
pub struct DisplayListOptimizer {
  config: OptimizationConfig,
}

impl DisplayListOptimizer {
  /// Create a new optimizer with default configuration
  pub fn new() -> Self {
    Self {
      config: OptimizationConfig::default(),
    }
  }

  /// Create an optimizer with custom configuration
  pub fn with_config(config: OptimizationConfig) -> Self {
    Self { config }
  }

  /// Optimize a display list for a given viewport
  ///
  /// Returns the optimized display list and statistics about what was optimized.
  pub fn optimize(&self, list: DisplayList, viewport: Rect) -> (DisplayList, OptimizationStats) {
    let original_count = list.len();
    let mut items: Vec<DisplayItem> = list.items().to_vec();
    let mut stats = OptimizationStats {
      original_count,
      ..Default::default()
    };

    // Pass 1: Remove transparent items
    if self.config.enable_transparent_removal {
      let before = items.len();
      items = self.remove_transparent_items(items);
      stats.transparent_removed = before - items.len();
    }

    // Pass 2: Remove no-op operations
    if self.config.enable_noop_removal {
      let before = items.len();
      items = self.remove_noop_items(items);
      stats.noop_removed = before - items.len();
    }

    // Pass 3: Cull items outside viewport
    if self.config.enable_culling {
      let before = items.len();
      items = self.cull_items(items, viewport);
      stats.culled_count = before - items.len();
    }

    // Pass 4: Merge adjacent fills
    if self.config.enable_fill_merging {
      let before = items.len();
      items = self.merge_adjacent_fills(items);
      stats.merged_count = before - items.len();
    }

    stats.final_count = items.len();
    (DisplayList::from_items(items), stats)
  }

  /// Remove fully transparent items
  fn remove_transparent_items(&self, items: Vec<DisplayItem>) -> Vec<DisplayItem> {
    items
      .into_iter()
      .filter(|item| !Self::is_transparent(item))
      .collect()
  }

  /// Check if an item is fully transparent
  fn is_transparent(item: &DisplayItem) -> bool {
    match item {
      DisplayItem::FillRect(item) => item.color.a == 0.0,
      DisplayItem::StrokeRect(item) => item.color.a == 0.0,
      DisplayItem::FillRoundedRect(item) => item.color.a == 0.0,
      DisplayItem::StrokeRoundedRect(item) => item.color.a == 0.0,
      DisplayItem::Text(item) => item.color.a == 0.0,
      DisplayItem::TextDecoration(item) => item.decorations.iter().all(|d| {
        d.color.a == 0.0
          || (d.underline.is_none() && d.overline.is_none() && d.line_through.is_none())
      }),
      DisplayItem::BoxShadow(item) => item.color.a == 0.0,
      DisplayItem::PushOpacity(item) => item.opacity <= 0.0,
      _ => false,
    }
  }

  /// Remove no-op operations (identity transforms, 1.0 opacity, normal blend)
  fn remove_noop_items(&self, items: Vec<DisplayItem>) -> Vec<DisplayItem> {
    let mut result = Vec::with_capacity(items.len());
    let mut skip_next_pop_opacity = 0;
    let mut skip_next_pop_transform = 0;
    let mut skip_next_pop_blend = 0;

    for item in items {
      match &item {
        DisplayItem::PushOpacity(op) if op.opacity >= 1.0 => {
          skip_next_pop_opacity += 1;
          continue;
        }
        DisplayItem::PopOpacity if skip_next_pop_opacity > 0 => {
          skip_next_pop_opacity -= 1;
          continue;
        }
        DisplayItem::PushTransform(t) if t.transform.is_identity() => {
          skip_next_pop_transform += 1;
          continue;
        }
        DisplayItem::PopTransform if skip_next_pop_transform > 0 => {
          skip_next_pop_transform -= 1;
          continue;
        }
        DisplayItem::PushBlendMode(b) if b.mode == BlendMode::Normal => {
          skip_next_pop_blend += 1;
          continue;
        }
        DisplayItem::PopBlendMode if skip_next_pop_blend > 0 => {
          skip_next_pop_blend -= 1;
          continue;
        }
        _ => {}
      }
      result.push(item);
    }

    result
  }

  /// Cull items outside the viewport
  #[allow(clippy::cognitive_complexity)]
  fn cull_items(&self, items: Vec<DisplayItem>, viewport: Rect) -> Vec<DisplayItem> {
    let mut result = Vec::with_capacity(items.len());
    let mut transform_stack: Vec<bool> = Vec::new();
    let mut active_transform_depth = 0usize;
    let mut context_stack: Vec<ContextRecord> = Vec::new();
    let mut active_effect_contexts = 0usize;
    let mut clip_stack: Vec<ClipRecord> = Vec::new();

    let refresh_context_clipping = |contexts: &mut [ContextRecord], clips: &[ClipRecord]| {
      let clipped = clips.iter().any(|c| c.can_cull);
      for ctx in contexts {
        ctx.clipped_by_clip = clipped;
      }
    };

    for item in items {
      let mut include_item = false;
      let mut item_bounds: Option<Rect> = None;

      match &item {
        DisplayItem::PushTransform(t) => {
          let non_identity = !t.transform.is_identity();
          transform_stack.push(non_identity);
          if non_identity {
            active_transform_depth += 1;
            for clip in &mut clip_stack {
              clip.can_cull = false;
            }
            refresh_context_clipping(&mut context_stack, &clip_stack);
          }
          include_item = true;
        }
        DisplayItem::PopTransform => {
          if transform_stack.pop().unwrap_or(false) && active_transform_depth > 0 {
            active_transform_depth -= 1;
          }
          include_item = true;
        }
        DisplayItem::PushStackingContext(sc) => {
          let effects = !sc.filters.is_empty() || !sc.backdrop_filters.is_empty();
          if effects {
            active_effect_contexts += 1;
            for clip in &mut clip_stack {
              clip.can_cull = false;
            }
            refresh_context_clipping(&mut context_stack, &clip_stack);
          }
          let has_transform = sc.transform.is_some_and(|t| !t.is_identity());
          if has_transform {
            active_transform_depth += 1;
            for clip in &mut clip_stack {
              clip.can_cull = false;
            }
            refresh_context_clipping(&mut context_stack, &clip_stack);
          }
          context_stack.push(ContextRecord {
            start_index: result.len(),
            bounds: None,
            item: sc.clone(),
            transform_active: active_transform_depth > 0,
            has_transform,
            has_effects: effects,
            clipped_by_clip: clip_stack.iter().any(|c| c.can_cull),
          });
          include_item = true;
        }
        DisplayItem::PopStackingContext => {
          include_item = true;
          if let Some(record) = context_stack.pop() {
            if record.has_transform && active_transform_depth > 0 {
              active_transform_depth -= 1;
            }
            if record.has_effects && active_effect_contexts > 0 {
              active_effect_contexts -= 1;
            }
            let keep = if record.transform_active {
              true
            } else if record.clipped_by_clip {
              false
            } else {
              let ctx_bounds = self.stacking_context_bounds(&record.item, record.bounds);
              viewport.intersects(ctx_bounds)
            };
            if keep {
              if let Some(parent) = context_stack.last_mut() {
                if !parent.transform_active && !parent.clipped_by_clip {
                  let ctx_bounds = self.stacking_context_bounds(&record.item, record.bounds);
                  parent.bounds = Some(match parent.bounds {
                    Some(b) => b.union(ctx_bounds),
                    None => ctx_bounds,
                  });
                }
              }
            } else {
              result.truncate(record.start_index);
              continue;
            }
          }
        }
        DisplayItem::PushClip(clip) => {
          let clip_bounds = match &clip.shape {
            crate::paint::display_list::ClipShape::Rect { rect, .. } => *rect,
            crate::paint::display_list::ClipShape::Path { path } => path.bounds(),
          };
          let can_cull = !viewport.intersects(clip_bounds)
            && active_effect_contexts == 0
            && active_transform_depth == 0;
          clip_stack.push(ClipRecord {
            start_index: result.len(),
            can_cull,
          });
          refresh_context_clipping(&mut context_stack, &clip_stack);
          include_item = true;
        }
        DisplayItem::PopClip => {
          if let Some(record) = clip_stack.pop() {
            if record.can_cull {
              result.truncate(record.start_index);
              continue;
            }
          }
          refresh_context_clipping(&mut context_stack, &clip_stack);
          include_item = true;
        }
        _ => {}
      }

      let force_active = active_transform_depth > 0 || active_effect_contexts > 0;

      if !include_item {
        if force_active {
          include_item = true;
        } else {
          item_bounds = self.item_bounds(&item);
          include_item = match (item_bounds, &item) {
            (Some(bounds), _) => viewport.intersects(bounds),
            (None, _) => matches!(
              item,
              DisplayItem::PushClip(_)
                | DisplayItem::PopClip
                | DisplayItem::PushOpacity(_)
                | DisplayItem::PopOpacity
                | DisplayItem::PushTransform(_)
                | DisplayItem::PopTransform
                | DisplayItem::PushBlendMode(_)
                | DisplayItem::PopBlendMode
                | DisplayItem::PushStackingContext(_)
                | DisplayItem::PopStackingContext
            ),
          };
        }
      }

      if include_item {
        result.push(item.clone());
        if active_transform_depth == 0 && !clip_stack.iter().any(|c| c.can_cull) {
          if item_bounds.is_none() {
            item_bounds = self.item_bounds(result.last().unwrap());
          }
          if let Some(bounds) = item_bounds {
            if let Some(context) = context_stack.last_mut() {
              if !context.transform_active && !context.clipped_by_clip {
                context.bounds = Some(match context.bounds {
                  Some(b) => b.union(bounds),
                  None => bounds,
                });
              }
            }
          }
        }
      }
    }

    // Balance any orphaned pops
    self.balance_stack_operations(result)
  }

  /// Ensure push/pop operations are balanced
  fn balance_stack_operations(&self, items: Vec<DisplayItem>) -> Vec<DisplayItem> {
    let mut clip_depth = 0i32;
    let mut opacity_depth = 0i32;
    let mut transform_depth = 0i32;
    let mut blend_depth = 0i32;
    let mut stacking_depth = 0i32;

    // Count depths
    for item in &items {
      match item {
        DisplayItem::PushClip(_) => clip_depth += 1,
        DisplayItem::PopClip => clip_depth -= 1,
        DisplayItem::PushOpacity(_) => opacity_depth += 1,
        DisplayItem::PopOpacity => opacity_depth -= 1,
        DisplayItem::PushTransform(_) => transform_depth += 1,
        DisplayItem::PopTransform => transform_depth -= 1,
        DisplayItem::PushBlendMode(_) => blend_depth += 1,
        DisplayItem::PopBlendMode => blend_depth -= 1,
        DisplayItem::PushStackingContext(_) => stacking_depth += 1,
        DisplayItem::PopStackingContext => stacking_depth -= 1,
        _ => {}
      }
    }

    let mut result = items;

    // Add missing pops
    while clip_depth > 0 {
      result.push(DisplayItem::PopClip);
      clip_depth -= 1;
    }
    while opacity_depth > 0 {
      result.push(DisplayItem::PopOpacity);
      opacity_depth -= 1;
    }
    while transform_depth > 0 {
      result.push(DisplayItem::PopTransform);
      transform_depth -= 1;
    }
    while blend_depth > 0 {
      result.push(DisplayItem::PopBlendMode);
      blend_depth -= 1;
    }
    while stacking_depth > 0 {
      result.push(DisplayItem::PopStackingContext);
      stacking_depth -= 1;
    }

    result
  }

  /// Merge adjacent fills with the same color
  fn merge_adjacent_fills(&self, items: Vec<DisplayItem>) -> Vec<DisplayItem> {
    if items.len() < 2 {
      return items;
    }

    let mut result = Vec::with_capacity(items.len());
    let mut pending_fill: Option<FillRectItem> = None;

    for item in items {
      match item {
        DisplayItem::FillRect(current) => {
          if let Some(prev) = pending_fill.take() {
            if let Some(merged) = Self::try_merge_fills(&prev, &current) {
              pending_fill = Some(merged);
            } else {
              result.push(DisplayItem::FillRect(prev));
              pending_fill = Some(current);
            }
          } else {
            pending_fill = Some(current);
          }
        }
        other => {
          if let Some(prev) = pending_fill.take() {
            result.push(DisplayItem::FillRect(prev));
          }
          result.push(other);
        }
      }
    }

    if let Some(prev) = pending_fill {
      result.push(DisplayItem::FillRect(prev));
    }

    result
  }

  fn item_bounds(&self, item: &DisplayItem) -> Option<Rect> {
    match item {
      DisplayItem::FillRect(i) => Some(i.rect),
      DisplayItem::StrokeRect(i) => Some(i.rect.inflate(i.width / 2.0)),
      DisplayItem::FillRoundedRect(i) => Some(i.rect),
      DisplayItem::StrokeRoundedRect(i) => Some(i.rect.inflate(i.width / 2.0)),
      DisplayItem::Text(item) => Some(crate::paint::display_list::text_bounds(item)),
      DisplayItem::TextDecoration(item) => Some(item.bounds),
      DisplayItem::Image(i) => Some(i.dest_rect),
      DisplayItem::BoxShadow(i) => {
        if i.inset {
          Some(i.rect)
        } else {
          let blur_outset = i.blur_radius.abs() * 3.0;
          let spread = i.spread_radius;
          let shadow_rect = Rect::from_xywh(
            i.rect.x() + i.offset.x - spread,
            i.rect.y() + i.offset.y - spread,
            i.rect.width() + spread * 2.0,
            i.rect.height() + spread * 2.0,
          )
          .inflate(blur_outset);
          Some(shadow_rect)
        }
      }
      DisplayItem::Border(b) => {
        let max_w = b
          .top
          .width
          .max(b.right.width)
          .max(b.bottom.width)
          .max(b.left.width);
        Some(b.rect.inflate(max_w * 0.5))
      }
      DisplayItem::LinearGradient(i) => Some(i.rect),
      DisplayItem::RadialGradient(i) => Some(i.rect),
      DisplayItem::ConicGradient(i) => Some(i.rect),
      _ => None,
    }
  }

  fn stacking_context_bounds(&self, item: &StackingContextItem, children: Option<Rect>) -> Rect {
    let (l, t, r, b) = Self::filter_outset(&item.filters);
    let (bl, bt, br, bb) = Self::filter_outset(&item.backdrop_filters);
    let expand_left = l.max(bl);
    let expand_top = t.max(bt);
    let expand_right = r.max(br);
    let expand_bottom = b.max(bb);

    let mut bounds = children.unwrap_or(item.bounds);
    if expand_left > 0.0 || expand_top > 0.0 || expand_right > 0.0 || expand_bottom > 0.0 {
      bounds = Rect::from_xywh(
        bounds.min_x() - expand_left,
        bounds.min_y() - expand_top,
        bounds.width() + expand_left + expand_right,
        bounds.height() + expand_top + expand_bottom,
      );
    }

    if let Some(transform) = item.transform {
      bounds = transform.transform_rect(bounds);
    }

    bounds
  }

  fn filter_outset(filters: &[ResolvedFilter]) -> (f32, f32, f32, f32) {
    let mut left: f32 = 0.0;
    let mut top: f32 = 0.0;
    let mut right: f32 = 0.0;
    let mut bottom: f32 = 0.0;

    for filter in filters {
      match *filter {
        ResolvedFilter::Blur(radius) => {
          let delta = radius.abs() * 3.0;
          left = left.max(delta);
          right = right.max(delta);
          top = top.max(delta);
          bottom = bottom.max(delta);
        }
        ResolvedFilter::DropShadow {
          offset_x,
          offset_y,
          blur_radius,
          spread,
          ..
        } => {
          let delta = blur_radius.abs() * 3.0 + spread.max(0.0);
          left = left.max(delta - offset_x.min(0.0));
          right = right.max(delta + offset_x.max(0.0));
          top = top.max(delta - offset_y.min(0.0));
          bottom = bottom.max(delta + offset_y.max(0.0));
        }
        _ => {}
      }
    }

    (left, top, right, bottom)
  }

  /// Try to merge two fill rects
  ///
  /// Fills can be merged if they have the same color and share an edge.
  fn try_merge_fills(a: &FillRectItem, b: &FillRectItem) -> Option<FillRectItem> {
    // Must have same color
    if a.color != b.color {
      return None;
    }

    // Check if horizontally adjacent (same height, touching edges)
    if (a.rect.min_y() - b.rect.min_y()).abs() < f32::EPSILON
      && (a.rect.height() - b.rect.height()).abs() < f32::EPSILON
    {
      // a is to the left of b
      if (a.rect.max_x() - b.rect.min_x()).abs() < f32::EPSILON {
        return Some(FillRectItem {
          rect: Rect::from_xywh(
            a.rect.min_x(),
            a.rect.min_y(),
            a.rect.width() + b.rect.width(),
            a.rect.height(),
          ),
          color: a.color,
        });
      }
      // b is to the left of a
      if (b.rect.max_x() - a.rect.min_x()).abs() < f32::EPSILON {
        return Some(FillRectItem {
          rect: Rect::from_xywh(
            b.rect.min_x(),
            a.rect.min_y(),
            a.rect.width() + b.rect.width(),
            a.rect.height(),
          ),
          color: a.color,
        });
      }
    }

    // Check if vertically adjacent (same width, touching edges)
    if (a.rect.min_x() - b.rect.min_x()).abs() < f32::EPSILON
      && (a.rect.width() - b.rect.width()).abs() < f32::EPSILON
    {
      // a is above b
      if (a.rect.max_y() - b.rect.min_y()).abs() < f32::EPSILON {
        return Some(FillRectItem {
          rect: Rect::from_xywh(
            a.rect.min_x(),
            a.rect.min_y(),
            a.rect.width(),
            a.rect.height() + b.rect.height(),
          ),
          color: a.color,
        });
      }
      // b is above a
      if (b.rect.max_y() - a.rect.min_y()).abs() < f32::EPSILON {
        return Some(FillRectItem {
          rect: Rect::from_xywh(
            a.rect.min_x(),
            b.rect.min_y(),
            a.rect.width(),
            a.rect.height() + b.rect.height(),
          ),
          color: a.color,
        });
      }
    }

    None
  }
}

#[derive(Clone)]
struct ContextRecord {
  start_index: usize,
  bounds: Option<Rect>,
  item: StackingContextItem,
  transform_active: bool,
  has_transform: bool,
  has_effects: bool,
  clipped_by_clip: bool,
}

#[derive(Clone)]
struct ClipRecord {
  start_index: usize,
  can_cull: bool,
}

impl Default for DisplayListOptimizer {
  fn default() -> Self {
    Self::new()
  }
}

// ============================================================================
// Convenience Functions
// ============================================================================

/// Optimize a display list with default settings
pub fn optimize(list: DisplayList, viewport: Rect) -> DisplayList {
  let optimizer = DisplayListOptimizer::new();
  optimizer.optimize(list, viewport).0
}

/// Optimize a display list and return statistics
pub fn optimize_with_stats(list: DisplayList, viewport: Rect) -> (DisplayList, OptimizationStats) {
  let optimizer = DisplayListOptimizer::new();
  optimizer.optimize(list, viewport)
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
  use super::*;
  use crate::paint::display_list::OpacityItem;
  use crate::style::color::Rgba;

  fn make_fill_rect(x: f32, y: f32, w: f32, h: f32, color: Rgba) -> DisplayItem {
    DisplayItem::FillRect(FillRectItem {
      rect: Rect::from_xywh(x, y, w, h),
      color,
    })
  }

  #[test]
  fn test_transparent_removal() {
    let mut list = DisplayList::new();
    list.push(make_fill_rect(0.0, 0.0, 100.0, 100.0, Rgba::RED));
    list.push(make_fill_rect(10.0, 10.0, 50.0, 50.0, Rgba::TRANSPARENT));
    list.push(make_fill_rect(20.0, 20.0, 30.0, 30.0, Rgba::BLUE));

    let viewport = Rect::from_xywh(0.0, 0.0, 200.0, 200.0);
    let (optimized, stats) = optimize_with_stats(list, viewport);

    assert_eq!(stats.transparent_removed, 1);
    assert_eq!(optimized.len(), 2);
  }

  #[test]
  fn test_viewport_culling() {
    let mut list = DisplayList::new();
    list.push(make_fill_rect(10.0, 10.0, 50.0, 50.0, Rgba::RED));
    list.push(make_fill_rect(500.0, 10.0, 50.0, 50.0, Rgba::GREEN));
    list.push(make_fill_rect(90.0, 10.0, 50.0, 50.0, Rgba::BLUE));

    let viewport = Rect::from_xywh(0.0, 0.0, 100.0, 100.0);
    let (optimized, stats) = optimize_with_stats(list, viewport);

    assert_eq!(stats.culled_count, 1);
    assert_eq!(optimized.len(), 2);
  }

  #[test]
  fn test_noop_removal() {
    let mut list = DisplayList::new();
    list.push(DisplayItem::PushOpacity(OpacityItem { opacity: 1.0 }));
    list.push(make_fill_rect(0.0, 0.0, 100.0, 100.0, Rgba::RED));
    list.push(DisplayItem::PopOpacity);

    let viewport = Rect::from_xywh(0.0, 0.0, 200.0, 200.0);
    let (optimized, stats) = optimize_with_stats(list, viewport);

    assert_eq!(stats.noop_removed, 2);
    assert_eq!(optimized.len(), 1);
  }

  #[test]
  fn test_fill_merging_horizontal() {
    let mut list = DisplayList::new();
    list.push(make_fill_rect(0.0, 0.0, 50.0, 100.0, Rgba::RED));
    list.push(make_fill_rect(50.0, 0.0, 50.0, 100.0, Rgba::RED));

    let viewport = Rect::from_xywh(0.0, 0.0, 200.0, 200.0);
    let (optimized, stats) = optimize_with_stats(list, viewport);

    assert_eq!(stats.merged_count, 1);
    assert_eq!(optimized.len(), 1);

    if let DisplayItem::FillRect(item) = &optimized.items()[0] {
      assert_eq!(item.rect.width(), 100.0);
    } else {
      panic!("Expected FillRect");
    }
  }

  #[test]
  fn test_fill_merging_vertical() {
    let mut list = DisplayList::new();
    list.push(make_fill_rect(0.0, 0.0, 100.0, 50.0, Rgba::RED));
    list.push(make_fill_rect(0.0, 50.0, 100.0, 50.0, Rgba::RED));

    let viewport = Rect::from_xywh(0.0, 0.0, 200.0, 200.0);
    let (optimized, stats) = optimize_with_stats(list, viewport);

    assert_eq!(stats.merged_count, 1);
    assert_eq!(optimized.len(), 1);

    if let DisplayItem::FillRect(item) = &optimized.items()[0] {
      assert_eq!(item.rect.height(), 100.0);
    } else {
      panic!("Expected FillRect");
    }
  }

  #[test]
  fn test_no_merge_different_colors() {
    let mut list = DisplayList::new();
    list.push(make_fill_rect(0.0, 0.0, 50.0, 100.0, Rgba::RED));
    list.push(make_fill_rect(50.0, 0.0, 50.0, 100.0, Rgba::BLUE));

    let viewport = Rect::from_xywh(0.0, 0.0, 200.0, 200.0);
    let (optimized, _) = optimize_with_stats(list, viewport);

    assert_eq!(optimized.len(), 2);
  }

  #[test]
  fn test_optimization_stats() {
    let mut list = DisplayList::new();
    for i in 0..10 {
      list.push(make_fill_rect(i as f32 * 20.0, 0.0, 15.0, 15.0, Rgba::RED));
    }

    let viewport = Rect::from_xywh(0.0, 0.0, 100.0, 100.0);
    let (_, stats) = optimize_with_stats(list, viewport);

    assert_eq!(stats.original_count, 10);
    assert!(stats.reduction_percentage() >= 0.0);
  }

  #[test]
  fn test_config_disable_passes() {
    let mut list = DisplayList::new();
    list.push(make_fill_rect(0.0, 0.0, 100.0, 100.0, Rgba::TRANSPARENT));

    let config = OptimizationConfig {
      enable_transparent_removal: false,
      ..Default::default()
    };
    let optimizer = DisplayListOptimizer::with_config(config);
    let (optimized, stats) = optimizer.optimize(list, Rect::from_xywh(0.0, 0.0, 200.0, 200.0));

    assert_eq!(stats.transparent_removed, 0);
    assert_eq!(optimized.len(), 1);
  }
}
