//! Display List Optimization
//!
//! This module provides advanced optimization passes for display lists:
//!
//! - **Viewport culling**: Skip items outside the visible area
//! - **Transparent item removal**: Remove fully transparent items and opacity scopes
//! - **No-op removal**: Remove identity transforms, 1.0 opacity, normal blend, and no-effect stacking contexts
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

use crate::geometry::{Point, Rect};
use crate::paint::display_list::BlendMode;
use crate::paint::display_list::DisplayItem;
use crate::paint::display_list::DisplayList;
use crate::paint::display_list::DisplayListView;
use crate::paint::display_list::FillRectItem;
use crate::paint::display_list::StackingContextItem;
use crate::paint::display_list::Transform2D;
use crate::paint::display_list::Transform3D;
use crate::paint::filter_outset::filter_outset_with_bounds;
use crate::paint::homography::Homography;

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

  /// Extract only the items that intersect the viewport while preserving stack
  /// operations needed for correct rasterization order.
  pub fn intersect(&self, list: &DisplayList, viewport: Rect) -> DisplayList {
    let items = list.items().to_vec();
    DisplayList::from_items(self.cull_items(items, viewport))
  }

  /// Extract indices of items that intersect the viewport while preserving stack operations.
  pub fn intersect_indices(&self, items: &[DisplayItem], viewport: Rect) -> Vec<usize> {
    let mut result = Vec::with_capacity(items.len());
    let mut transform_state = TransformState::default();
    let mut transform_stack: Vec<TransformState> = Vec::new();
    let mut context_stack: Vec<ContextRecord> = Vec::new();
    let mut clip_stack: Vec<ClipRecord> = Vec::new();
    let mut filter_stack: Vec<f32> = Vec::new();

    let refresh_context_clipping = |contexts: &mut [ContextRecord], clips: &[ClipRecord]| {
      let clipped = clips.iter().any(|c| c.can_cull);
      for ctx in contexts {
        ctx.clipped_by_clip = clipped;
      }
    };

    for (index, item) in items.iter().enumerate() {
      let mut include_item = false;
      let mut transformed_bounds: Option<Rect> = None;

      match item {
        DisplayItem::PushTransform(t) => {
          transform_stack.push(transform_state.clone());
          let mapping = Self::transform_mapping(&t.transform);
          transform_state.current = transform_state.current.multiply(&mapping);
          include_item = true;
        }
        DisplayItem::PopTransform => {
          if let Some(previous) = transform_stack.pop() {
            transform_state = previous;
          }
          include_item = true;
        }
        DisplayItem::PushStackingContext(sc) => {
          let pushed_transform = sc.transform.is_some() || sc.child_perspective.is_some();
          let mut context_transform = transform_state.current.clone();
          if let Some(transform) = sc.transform.as_ref() {
            let mapping = Self::transform_mapping(transform);
            context_transform = context_transform.multiply(&mapping);
          }
          let context_culling_disabled = transform_state.culling_disabled()
            || matches!(context_transform, TransformMapping::Unsupported);
          if pushed_transform {
            transform_stack.push(transform_state.clone());
            if !clip_stack.is_empty() {
              for clip in &mut clip_stack {
                clip.can_cull = false;
              }
              refresh_context_clipping(&mut context_stack, &clip_stack);
            }
            transform_state.current = context_transform.clone();
            if sc.child_perspective.is_some() {
              transform_state.suppress_culling = true;
            }
          }
          let filters_outset = filter_outset_with_bounds(&sc.filters, 1.0, Some(sc.bounds));
          let backdrop_outset =
            filter_outset_with_bounds(&sc.backdrop_filters, 1.0, Some(sc.bounds));
          let max_outset = filters_outset.max_side().max(backdrop_outset.max_side());
          let world_outset = if matches!(context_transform, TransformMapping::Unsupported) {
            0.0
          } else if max_outset == 0.0 {
            0.0
          } else {
            max_outset * Self::transform_scale_factor(&context_transform, sc.bounds)
          };
          filter_stack.push(world_outset);

          let transform_for_bounds = if matches!(context_transform, TransformMapping::Unsupported) {
            None
          } else {
            Some(context_transform.clone())
          };

          context_stack.push(ContextRecord {
            start_index: result.len(),
            bounds: None,
            item: sc.clone(),
            transform_for_bounds,
            clipped_by_clip: clip_stack.iter().any(|c| c.can_cull),
            culling_disabled: context_culling_disabled,
            pushed_transform,
          });
          include_item = true;
        }
        DisplayItem::PopStackingContext => {
          include_item = true;
          if let Some(record) = context_stack.pop() {
            if record.pushed_transform {
              if let Some(previous) = transform_stack.pop() {
                transform_state = previous;
              }
            }
            filter_stack.pop();

            if record.culling_disabled {
              // Keep conservatively when transforms prevented culling
            } else if record.clipped_by_clip {
              result.truncate(record.start_index);
              refresh_context_clipping(&mut context_stack, &clip_stack);
              continue;
            } else {
              let ctx_bounds = self.stacking_context_bounds(
                &record.item,
                record.bounds,
                record.transform_for_bounds.as_ref(),
              );
              let keep = if record.item.mask.is_some() {
                true
              } else {
                ctx_bounds
                  .map(|bounds| viewport.intersects(bounds))
                  .unwrap_or(true)
              };
              if keep {
                if let Some(parent) = context_stack.last_mut() {
                  if !parent.culling_disabled && clip_stack.iter().all(|c| !c.can_cull) {
                    if let Some(ctx_bounds) = ctx_bounds {
                      parent.bounds = Some(match parent.bounds {
                        Some(bounds) => bounds.union(ctx_bounds),
                        None => ctx_bounds,
                      });
                    }
                  }
                }
              } else {
                result.truncate(record.start_index);
                refresh_context_clipping(&mut context_stack, &clip_stack);
                continue;
              }
            }
          }
        }
        DisplayItem::PushClip(clip) => {
          let local_bounds = match &clip.shape {
            crate::paint::display_list::ClipShape::Rect { rect, .. } => *rect,
            crate::paint::display_list::ClipShape::Path { path } => path.bounds(),
          };
          let can_cull = if transform_state.culling_disabled() {
            false
          } else if let Some(mut world_bounds) =
            Self::transform_rect_any(&transform_state.current, local_bounds)
          {
            let inflate = filter_stack.iter().copied().sum::<f32>();
            if inflate > 0.0 {
              world_bounds = world_bounds.inflate(inflate);
            }
            !viewport.intersects(world_bounds)
          } else {
            false
          };
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
              refresh_context_clipping(&mut context_stack, &clip_stack);
              continue;
            }
          }
          refresh_context_clipping(&mut context_stack, &clip_stack);
          include_item = true;
        }
        _ => {}
      }

      if !include_item {
        if transform_state.culling_disabled() {
          include_item = true;
        } else if let Some(local_bounds) = self.item_bounds(item) {
          if let Some(mut bounds) = Self::transform_rect_any(&transform_state.current, local_bounds)
          {
            let inflate = filter_stack.iter().copied().sum::<f32>();
            if inflate > 0.0 {
              bounds = bounds.inflate(inflate);
            }
            include_item = viewport.intersects(bounds);
            transformed_bounds = Some(bounds);
          } else {
            include_item = true;
          }
        } else {
          include_item = item.is_stack_operation();
        }
      }

      if include_item {
        result.push(index);
        if transformed_bounds.is_none() && !transform_state.culling_disabled() {
          if let Some(local_bounds) = self.item_bounds(item) {
            if let Some(mut bounds) =
              Self::transform_rect_any(&transform_state.current, local_bounds)
            {
              let inflate = filter_stack.iter().copied().sum::<f32>();
              if inflate > 0.0 {
                bounds = bounds.inflate(inflate);
              }
              transformed_bounds = Some(bounds);
            }
          }
        }
        if let Some(bounds) = transformed_bounds {
          if !transform_state.culling_disabled() && clip_stack.iter().all(|c| !c.can_cull) {
            if let Some(context) = context_stack.last_mut() {
              context.bounds = Some(match context.bounds {
                Some(existing) => existing.union(bounds),
                None => bounds,
              });
            }
          }
        }
      }
    }

    result
  }

  /// Build a display list view containing only the items that intersect the viewport.
  pub fn intersect_view<'a>(
    &self,
    items: &'a [DisplayItem],
    viewport: Rect,
  ) -> DisplayListView<'a> {
    let indices = self.intersect_indices(items, viewport);
    let tail = self.balance_stack_tail(items, &indices);
    DisplayListView::new(items, indices, tail)
  }

  /// Remove fully transparent items
  fn remove_transparent_items(&self, items: Vec<DisplayItem>) -> Vec<DisplayItem> {
    let mut result = Vec::with_capacity(items.len());
    let mut skip_opacity_depth: usize = 0;

    for item in items {
      if skip_opacity_depth > 0 {
        match &item {
          DisplayItem::PushOpacity(_) => skip_opacity_depth += 1,
          DisplayItem::PopOpacity => {
            skip_opacity_depth -= 1;
          }
          _ => {}
        }
        continue;
      }

      if let DisplayItem::PushOpacity(opacity) = &item {
        if opacity.opacity <= 0.0 {
          skip_opacity_depth = 1;
          continue;
        }
      }

      if Self::is_transparent(&item) {
        continue;
      }

      result.push(item);
    }

    result
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
      _ => false,
    }
  }

  /// Remove no-op operations (identity transforms, 1.0 opacity, normal blend)
  fn remove_noop_items(&self, items: Vec<DisplayItem>) -> Vec<DisplayItem> {
    let mut result = Vec::with_capacity(items.len());
    let mut opacity_stack: Vec<bool> = Vec::new();
    let mut transform_stack: Vec<bool> = Vec::new();
    let mut blend_stack: Vec<bool> = Vec::new();
    let mut stacking_context_stack: Vec<bool> = Vec::new();

    for item in items {
      match &item {
        DisplayItem::PushOpacity(op) => {
          let removed = op.opacity >= 1.0;
          opacity_stack.push(removed);
          if removed {
            continue;
          }
        }
        DisplayItem::PopOpacity => {
          if let Some(removed) = opacity_stack.pop() {
            if removed {
              continue;
            }
          }
        }
        DisplayItem::PushTransform(t) => {
          let removed = t.transform.is_identity();
          transform_stack.push(removed);
          if removed {
            continue;
          }
        }
        DisplayItem::PopTransform => {
          if let Some(removed) = transform_stack.pop() {
            if removed {
              continue;
            }
          }
        }
        DisplayItem::PushBlendMode(b) => {
          let removed = b.mode == BlendMode::Normal;
          blend_stack.push(removed);
          if removed {
            continue;
          }
        }
        DisplayItem::PopBlendMode => {
          if let Some(removed) = blend_stack.pop() {
            if removed {
              continue;
            }
          }
        }
        DisplayItem::PushStackingContext(sc) => {
          let removed = Self::is_noop_stacking_context(sc);
          stacking_context_stack.push(removed);
          if removed {
            continue;
          }
        }
        DisplayItem::PopStackingContext => {
          if let Some(removed) = stacking_context_stack.pop() {
            if removed {
              continue;
            }
          }
        }
        _ => {}
      }
      result.push(item);
    }

    result
  }

  fn is_noop_stacking_context(item: &StackingContextItem) -> bool {
    item.child_perspective.is_none()
      && item.transform.is_none()
      && item.filters.is_empty()
      && item.backdrop_filters.is_empty()
      && item.mask.is_none()
      && item.mix_blend_mode == BlendMode::Normal
      && !item.is_isolated
      && item.radii.is_zero()
  }

  /// Cull items outside the viewport
  #[allow(clippy::cognitive_complexity)]
  fn cull_items(&self, items: Vec<DisplayItem>, viewport: Rect) -> Vec<DisplayItem> {
    let mut result = Vec::with_capacity(items.len());
    let mut transform_state = TransformState::default();
    let mut transform_stack: Vec<TransformState> = Vec::new();
    let mut context_stack: Vec<ContextRecord> = Vec::new();
    let mut clip_stack: Vec<ClipRecord> = Vec::new();
    let mut filter_stack: Vec<f32> = Vec::new();

    let refresh_context_clipping = |contexts: &mut [ContextRecord], clips: &[ClipRecord]| {
      let clipped = clips.iter().any(|c| c.can_cull);
      for ctx in contexts {
        ctx.clipped_by_clip = clipped;
      }
    };

    for item in items {
      let mut include_item = false;
      let mut transformed_bounds: Option<Rect> = None;
      let culled_by_clip = clip_stack.last().map(|c| c.can_cull).unwrap_or(false);

      match &item {
        DisplayItem::PushTransform(t) => {
          transform_stack.push(transform_state.clone());
          let mapping = Self::transform_mapping(&t.transform);
          transform_state.current = transform_state.current.multiply(&mapping);
          include_item = true;
        }
        DisplayItem::PopTransform => {
          if let Some(previous) = transform_stack.pop() {
            transform_state = previous;
          }
          include_item = true;
        }
        DisplayItem::PushStackingContext(sc) => {
          let pushed_transform = sc.transform.is_some() || sc.child_perspective.is_some();
          let mut context_transform = transform_state.current.clone();
          if let Some(transform) = sc.transform.as_ref() {
            let mapping = Self::transform_mapping(transform);
            context_transform = context_transform.multiply(&mapping);
          }
          let context_culling_disabled = transform_state.culling_disabled()
            || matches!(context_transform, TransformMapping::Unsupported);
          if pushed_transform {
            transform_stack.push(transform_state.clone());
            if !clip_stack.is_empty() {
              for clip in &mut clip_stack {
                clip.can_cull = false;
              }
              refresh_context_clipping(&mut context_stack, &clip_stack);
            }
            transform_state.current = context_transform.clone();
            if sc.child_perspective.is_some() {
              // Perspective applies to descendants, not the stacking context plane itself.
              // Keep the mapping so the root can still be culled while avoiding aggressive
              // culling within the projected subtree.
              transform_state.suppress_culling = true;
            }
          }
          let filters_outset = filter_outset_with_bounds(&sc.filters, 1.0, Some(sc.bounds));
          let backdrop_outset =
            filter_outset_with_bounds(&sc.backdrop_filters, 1.0, Some(sc.bounds));
          let max_outset = filters_outset.max_side().max(backdrop_outset.max_side());
          let world_outset = if matches!(context_transform, TransformMapping::Unsupported) {
            0.0
          } else if max_outset == 0.0 {
            0.0
          } else {
            max_outset * Self::transform_scale_factor(&context_transform, sc.bounds)
          };
          filter_stack.push(world_outset);

          let transform_for_bounds = if matches!(context_transform, TransformMapping::Unsupported) {
            None
          } else {
            Some(context_transform.clone())
          };

          context_stack.push(ContextRecord {
            start_index: result.len(),
            bounds: None,
            item: sc.clone(),
            transform_for_bounds,
            clipped_by_clip: clip_stack.iter().any(|c| c.can_cull),
            culling_disabled: context_culling_disabled,
            pushed_transform,
          });
          include_item = true;
        }
        DisplayItem::PopStackingContext => {
          include_item = true;
          if let Some(record) = context_stack.pop() {
            if record.pushed_transform {
              if let Some(previous) = transform_stack.pop() {
                transform_state = previous;
              }
            }
            filter_stack.pop();

            if record.culling_disabled {
              // Keep conservatively when transforms prevented culling
            } else if record.clipped_by_clip {
              result.truncate(record.start_index);
              refresh_context_clipping(&mut context_stack, &clip_stack);
              continue;
            } else {
              let ctx_bounds = self.stacking_context_bounds(
                &record.item,
                record.bounds,
                record.transform_for_bounds.as_ref(),
              );
              let keep = if record.item.mask.is_some() {
                true
              } else {
                ctx_bounds
                  .map(|bounds| viewport.intersects(bounds))
                  .unwrap_or(true)
              };
              if keep {
                if let Some(parent) = context_stack.last_mut() {
                  if !parent.culling_disabled && clip_stack.iter().all(|c| !c.can_cull) {
                    if let Some(ctx_bounds) = ctx_bounds {
                      parent.bounds = Some(match parent.bounds {
                        Some(bounds) => bounds.union(ctx_bounds),
                        None => ctx_bounds,
                      });
                    }
                  }
                }
              } else {
                result.truncate(record.start_index);
                refresh_context_clipping(&mut context_stack, &clip_stack);
                continue;
              }
            }
          }
        }
        DisplayItem::PushClip(clip) => {
          let local_bounds = match &clip.shape {
            crate::paint::display_list::ClipShape::Rect { rect, .. } => *rect,
            crate::paint::display_list::ClipShape::Path { path } => path.bounds(),
          };
          let can_cull = if transform_state.culling_disabled() {
            false
          } else if let Some(mut world_bounds) =
            Self::transform_rect_any(&transform_state.current, local_bounds)
          {
            let inflate = filter_stack.iter().copied().sum::<f32>();
            if inflate > 0.0 {
              world_bounds = world_bounds.inflate(inflate);
            }
            !viewport.intersects(world_bounds)
          } else {
            false
          };
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
              refresh_context_clipping(&mut context_stack, &clip_stack);
              continue;
            }
          }
          refresh_context_clipping(&mut context_stack, &clip_stack);
          include_item = true;
        }
        _ => {}
      }

      if culled_by_clip {
        include_item = false;
        transformed_bounds = None;
      }

      if !include_item {
        if transform_state.culling_disabled() {
          include_item = true;
        } else if let Some(local_bounds) = self.item_bounds(&item) {
          if let Some(mut bounds) = Self::transform_rect_any(&transform_state.current, local_bounds)
          {
            let inflate = filter_stack.iter().copied().sum::<f32>();
            if inflate > 0.0 {
              bounds = bounds.inflate(inflate);
            }
            include_item = viewport.intersects(bounds);
            transformed_bounds = Some(bounds);
          } else {
            include_item = true;
          }
        } else {
          include_item = item.is_stack_operation();
        }
      }

      if include_item {
        result.push(item.clone());
        if transformed_bounds.is_none() && !transform_state.culling_disabled() {
          if let Some(local_bounds) = self.item_bounds(result.last().unwrap()) {
            if let Some(mut bounds) =
              Self::transform_rect_any(&transform_state.current, local_bounds)
            {
              let inflate = filter_stack.iter().copied().sum::<f32>();
              if inflate > 0.0 {
                bounds = bounds.inflate(inflate);
              }
              transformed_bounds = Some(bounds);
            }
          }
        }
        if let Some(bounds) = transformed_bounds {
          if !transform_state.culling_disabled() && clip_stack.iter().all(|c| !c.can_cull) {
            if let Some(context) = context_stack.last_mut() {
              context.bounds = Some(match context.bounds {
                Some(existing) => existing.union(bounds),
                None => bounds,
              });
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

  /// Generate the missing pop operations needed to balance the provided item indices.
  pub fn balance_stack_tail(&self, items: &[DisplayItem], indices: &[usize]) -> Vec<DisplayItem> {
    let mut clip_depth = 0i32;
    let mut opacity_depth = 0i32;
    let mut transform_depth = 0i32;
    let mut blend_depth = 0i32;
    let mut stacking_depth = 0i32;

    for &idx in indices {
      if let Some(item) = items.get(idx) {
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
    }

    let mut tail = Vec::new();
    while clip_depth > 0 {
      tail.push(DisplayItem::PopClip);
      clip_depth -= 1;
    }
    while opacity_depth > 0 {
      tail.push(DisplayItem::PopOpacity);
      opacity_depth -= 1;
    }
    while transform_depth > 0 {
      tail.push(DisplayItem::PopTransform);
      transform_depth -= 1;
    }
    while blend_depth > 0 {
      tail.push(DisplayItem::PopBlendMode);
      blend_depth -= 1;
    }
    while stacking_depth > 0 {
      tail.push(DisplayItem::PopStackingContext);
      stacking_depth -= 1;
    }

    tail
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
    item.bounds()
  }

  fn transform_mapping(transform: &Transform3D) -> TransformMapping {
    let homography = Homography::from_transform3d_z0(transform);
    if let Some(affine) = homography.to_affine() {
      if affine.inverse().is_some() {
        return TransformMapping::Affine(affine);
      }
    }

    if transform.project_point_2d(0.0, 0.0).is_none()
      || transform.project_point_2d(1.0, 0.0).is_none()
      || transform.project_point_2d(0.0, 1.0).is_none()
    {
      return TransformMapping::Unsupported;
    }

    if homography.is_invertible() {
      TransformMapping::Projective(homography)
    } else {
      TransformMapping::Unsupported
    }
  }

  fn transform_rect_any(mapping: &TransformMapping, rect: Rect) -> Option<Rect> {
    match mapping {
      TransformMapping::Affine(transform) => Some(transform.transform_rect(rect)),
      TransformMapping::Projective(homography) => homography.map_rect_aabb(rect),
      TransformMapping::Unsupported => None,
    }
  }

  fn transform_scale_factor(transform: &TransformMapping, reference: Rect) -> f32 {
    match transform {
      TransformMapping::Affine(transform) => {
        let scale_x = (transform.a * transform.a + transform.b * transform.b).sqrt();
        let scale_y = (transform.c * transform.c + transform.d * transform.d).sqrt();
        scale_x.max(scale_y)
      }
      TransformMapping::Projective(homography) => {
        let samples = [
          reference.origin,
          Point::new(reference.max_x(), reference.min_y()),
          Point::new(reference.min_x(), reference.max_y()),
          Point::new(reference.max_x(), reference.max_y()),
          Point::new(
            reference.min_x() + reference.width() * 0.5,
            reference.min_y() + reference.height() * 0.5,
          ),
        ];
        let mut max_scale: f32 = 1.0;
        for p in samples {
          let base = match homography.map_point(p) {
            Some(p) => p,
            None => return f32::INFINITY,
          };
          let dx = match homography.map_point(Point::new(p.x + 1.0, p.y)) {
            Some(p) => p,
            None => return f32::INFINITY,
          };
          let dy = match homography.map_point(Point::new(p.x, p.y + 1.0)) {
            Some(p) => p,
            None => return f32::INFINITY,
          };
          let scale_x = ((dx.x - base.x).powi(2) + (dx.y - base.y).powi(2)).sqrt();
          let scale_y = ((dy.x - base.x).powi(2) + (dy.y - base.y).powi(2)).sqrt();
          max_scale = max_scale.max(scale_x.max(scale_y));
        }
        max_scale
      }
      TransformMapping::Unsupported => 1.0,
    }
  }

  fn stacking_context_bounds(
    &self,
    item: &StackingContextItem,
    children: Option<Rect>,
    transform: Option<&TransformMapping>,
  ) -> Option<Rect> {
    let filters_outset = filter_outset_with_bounds(&item.filters, 1.0, Some(item.bounds));
    let backdrop_outset = filter_outset_with_bounds(&item.backdrop_filters, 1.0, Some(item.bounds));
    let expand_left = filters_outset.left.max(backdrop_outset.left);
    let expand_top = filters_outset.top.max(backdrop_outset.top);
    let expand_right = filters_outset.right.max(backdrop_outset.right);
    let expand_bottom = filters_outset.bottom.max(backdrop_outset.bottom);

    let mut bounds = match children {
      Some(bounds) => bounds,
      None => match transform {
        Some(transform) => Self::transform_rect_any(transform, item.bounds)?,
        None => item.bounds,
      },
    };
    if expand_left > 0.0 || expand_top > 0.0 || expand_right > 0.0 || expand_bottom > 0.0 {
      let scale = transform
        .map(|t| Self::transform_scale_factor(t, item.bounds))
        .unwrap_or(1.0);
      bounds = Rect::from_xywh(
        bounds.min_x() - expand_left * scale,
        bounds.min_y() - expand_top * scale,
        bounds.width() + (expand_left + expand_right) * scale,
        bounds.height() + (expand_top + expand_bottom) * scale,
      );
    }

    Some(bounds)
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
  transform_for_bounds: Option<TransformMapping>,
  clipped_by_clip: bool,
  culling_disabled: bool,
  pushed_transform: bool,
}

#[derive(Clone)]
enum TransformMapping {
  Affine(Transform2D),
  Projective(Homography),
  Unsupported,
}

impl TransformMapping {
  fn identity() -> Self {
    Self::Affine(Transform2D::identity())
  }

  fn as_homography(&self) -> Option<Homography> {
    match self {
      TransformMapping::Affine(t) => Some(Homography::from_affine(t)),
      TransformMapping::Projective(h) => Some(*h),
      TransformMapping::Unsupported => None,
    }
  }

  fn multiply(&self, other: &TransformMapping) -> TransformMapping {
    match (self, other) {
      (TransformMapping::Unsupported, _) | (_, TransformMapping::Unsupported) => {
        TransformMapping::Unsupported
      }
      (TransformMapping::Affine(a), TransformMapping::Affine(b)) => {
        TransformMapping::Affine(a.multiply(b))
      }
      (lhs, rhs) => {
        if let (Some(lhs), Some(rhs)) = (lhs.as_homography(), rhs.as_homography()) {
          let combined = lhs.multiply(&rhs);
          if let Some(affine) = combined.to_affine() {
            if affine.inverse().is_some() {
              return TransformMapping::Affine(affine);
            }
          }
          if combined.is_invertible() {
            TransformMapping::Projective(combined)
          } else {
            TransformMapping::Unsupported
          }
        } else {
          TransformMapping::Unsupported
        }
      }
    }
  }
}

#[derive(Clone)]
struct TransformState {
  current: TransformMapping,
  suppress_culling: bool,
}

impl TransformState {
  fn culling_disabled(&self) -> bool {
    self.suppress_culling || matches!(self.current, TransformMapping::Unsupported)
  }
}

impl Default for TransformState {
  fn default() -> Self {
    Self {
      current: TransformMapping::identity(),
      suppress_culling: false,
    }
  }
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
  use crate::paint::display_list::{
    text_bounds, BorderRadii, ClipItem, ClipShape, GlyphInstance, ImageData, MaskReferenceRects,
    OpacityItem, ResolvedFilter, ResolvedMask, ResolvedMaskImage, ResolvedMaskLayer,
    StackingContextItem, TextItem, Transform3D, TransformItem,
  };
  use crate::paint::filter_outset::filter_outset;
  use crate::style::color::Rgba;
  use crate::style::types::{
    BackfaceVisibility, BackgroundPosition, BackgroundPositionComponent, BackgroundRepeat,
    BackgroundSize, BackgroundSizeComponent, MaskClip, MaskComposite, MaskMode, MaskOrigin,
    TransformStyle,
  };
  use crate::style::values::Length;
  use std::f32::consts::FRAC_PI_4;

  fn make_fill_rect(x: f32, y: f32, w: f32, h: f32, color: Rgba) -> DisplayItem {
    DisplayItem::FillRect(FillRectItem {
      rect: Rect::from_xywh(x, y, w, h),
      color,
    })
  }

  fn assert_balanced(items: &[DisplayItem]) {
    let mut clip = 0i32;
    let mut opacity = 0i32;
    let mut transform = 0i32;
    let mut blend = 0i32;
    let mut stacking = 0i32;

    for item in items {
      match item {
        DisplayItem::PushClip(_) => clip += 1,
        DisplayItem::PopClip => clip -= 1,
        DisplayItem::PushOpacity(_) => opacity += 1,
        DisplayItem::PopOpacity => opacity -= 1,
        DisplayItem::PushTransform(_) => transform += 1,
        DisplayItem::PopTransform => transform -= 1,
        DisplayItem::PushBlendMode(_) => blend += 1,
        DisplayItem::PopBlendMode => blend -= 1,
        DisplayItem::PushStackingContext(_) => stacking += 1,
        DisplayItem::PopStackingContext => stacking -= 1,
        _ => {}
      }

      assert!(clip >= 0 && opacity >= 0 && transform >= 0 && blend >= 0 && stacking >= 0);
    }

    assert_eq!(clip, 0);
    assert_eq!(opacity, 0);
    assert_eq!(transform, 0);
    assert_eq!(blend, 0);
    assert_eq!(stacking, 0);
  }

  #[test]
  fn filter_outset_accumulates_blur_chain() {
    let filters = vec![ResolvedFilter::Blur(2.0), ResolvedFilter::Blur(2.0)];
    let (l, t, r, b) = filter_outset(&filters, 1.0).as_tuple();
    assert!(
      (l - 12.0).abs() < 0.01
        && (t - 12.0).abs() < 0.01
        && (r - 12.0).abs() < 0.01
        && (b - 12.0).abs() < 0.01,
      "expected double blur radius to inflate by ~12px, got {l},{t},{r},{b}"
    );
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
  fn test_transparent_opacity_scope_removed() {
    let mut list = DisplayList::new();
    list.push(DisplayItem::PushOpacity(OpacityItem { opacity: 0.0 }));
    list.push(make_fill_rect(0.0, 0.0, 50.0, 50.0, Rgba::RED));
    list.push(DisplayItem::PopOpacity);

    let viewport = Rect::from_xywh(0.0, 0.0, 200.0, 200.0);
    let (optimized, stats) = optimize_with_stats(list, viewport);

    assert_eq!(stats.transparent_removed, 3);
    assert_eq!(optimized.len(), 0);
    assert_balanced(optimized.items());
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
  fn dense_text_near_edge_not_culled_with_cached_bounds() {
    let glyphs: Vec<GlyphInstance> = (0..200)
      .map(|i| GlyphInstance {
        glyph_id: i as u32,
        offset: Point::new(-70.0 + i as f32 * 2.0, 0.0),
        advance: 2.0,
      })
      .collect();
    let mut text = TextItem {
      origin: Point::new(150.0, 50.0),
      cached_bounds: None,
      glyphs,
      color: Rgba::BLACK,
      palette_index: 0,
      shadows: Vec::new(),
      font_size: 20.0,
      advance_width: 400.0,
      font: None,
      font_id: None,
      variations: Vec::new(),
      synthetic_bold: 0.0,
      synthetic_oblique: 0.0,
      emphasis: None,
      decorations: Vec::new(),
    };
    let bounds = text_bounds(&text);
    text.cached_bounds = Some(bounds);
    let mut list = DisplayList::new();
    list.push(DisplayItem::Text(text));

    let viewport = Rect::from_xywh(0.0, 0.0, 100.0, 100.0);
    let (optimized, stats) = optimize_with_stats(list, viewport);

    assert_eq!(stats.culled_count, 0);
    assert!(
      matches!(optimized.items(), [DisplayItem::Text(_)]),
      "text near viewport edge should be retained"
    );
  }

  #[test]
  fn test_offscreen_clipped_scope_with_transform_is_culled() {
    let mut list = DisplayList::new();
    list.push(DisplayItem::PushClip(ClipItem {
      shape: ClipShape::Rect {
        rect: Rect::from_xywh(1000.0, 1000.0, 50.0, 50.0),
        radii: None,
      },
    }));
    list.push(DisplayItem::PushTransform(TransformItem {
      transform: Transform3D::translate(-1000.0, -1000.0, 0.0),
    }));
    list.push(make_fill_rect(1000.0, 1000.0, 30.0, 30.0, Rgba::GREEN));
    list.push(DisplayItem::PopTransform);
    list.push(DisplayItem::PopClip);

    let viewport = Rect::from_xywh(0.0, 0.0, 100.0, 100.0);
    let (optimized, stats) = optimize_with_stats(list, viewport);

    assert_eq!(optimized.len(), 0);
    assert_eq!(stats.culled_count, 5);
    assert_balanced(optimized.items());
  }

  #[test]
  fn blur_chain_outset_prevents_culling() {
    let filters = vec![ResolvedFilter::Blur(2.0), ResolvedFilter::Blur(2.0)];
    let mut list = DisplayList::new();
    list.push(DisplayItem::PushStackingContext(StackingContextItem {
      z_index: 0,
      creates_stacking_context: true,
      bounds: Rect::from_xywh(108.0, 10.0, 2.0, 2.0),
      plane_rect: Rect::from_xywh(108.0, 10.0, 2.0, 2.0),
      mix_blend_mode: BlendMode::Normal,
      is_isolated: false,
      transform: None,
      child_perspective: None,
      transform_style: TransformStyle::Flat,
      backface_visibility: BackfaceVisibility::Visible,
      filters: filters.clone(),
      backdrop_filters: Vec::new(),
      radii: BorderRadii::ZERO,
      mask: None,
    }));
    list.push(make_fill_rect(108.0, 10.0, 2.0, 2.0, Rgba::RED));
    list.push(DisplayItem::PopStackingContext);

    let viewport = Rect::from_xywh(0.0, 0.0, 100.0, 100.0);
    let (optimized, stats) = optimize_with_stats(list, viewport);

    assert_eq!(
      stats.culled_count, 0,
      "blurred content should not be culled"
    );
    assert_eq!(optimized.len(), 3);
    assert!(optimized
      .items()
      .iter()
      .any(|item| matches!(item, DisplayItem::FillRect(_))));
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
  fn test_noop_stacking_context_removed() {
    let mut list = DisplayList::new();
    let stacking_context = StackingContextItem {
      z_index: 0,
      creates_stacking_context: true,
      bounds: Rect::from_xywh(0.0, 0.0, 50.0, 50.0),
      plane_rect: Rect::from_xywh(0.0, 0.0, 50.0, 50.0),
      mix_blend_mode: BlendMode::Normal,
      is_isolated: false,
      transform: None,
      child_perspective: None,
      transform_style: TransformStyle::Flat,
      backface_visibility: BackfaceVisibility::Visible,
      filters: vec![],
      backdrop_filters: vec![],
      radii: BorderRadii::ZERO,
      mask: None,
    };

    list.push(DisplayItem::PushStackingContext(stacking_context));
    list.push(make_fill_rect(10.0, 10.0, 20.0, 20.0, Rgba::BLUE));
    list.push(DisplayItem::PopStackingContext);

    let viewport = Rect::from_xywh(0.0, 0.0, 100.0, 100.0);
    let (optimized, stats) = optimize_with_stats(list, viewport);

    assert_eq!(stats.noop_removed, 2);
    assert_eq!(optimized.len(), 1);
    assert!(matches!(optimized.items()[0], DisplayItem::FillRect(_)));
    assert_balanced(optimized.items());
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
  fn stacking_context_with_mask_not_culled() {
    let bounds = Rect::from_xywh(500.0, 500.0, 10.0, 10.0);
    let layer = ResolvedMaskLayer {
      image: ResolvedMaskImage::Raster(ImageData::new_pixels(1, 1, vec![255, 255, 255, 255])),
      repeat: BackgroundRepeat::no_repeat(),
      position: BackgroundPosition::Position {
        x: BackgroundPositionComponent {
          alignment: 0.0,
          offset: Length::percent(0.0),
        },
        y: BackgroundPositionComponent {
          alignment: 0.0,
          offset: Length::percent(0.0),
        },
      },
      size: BackgroundSize::Explicit(BackgroundSizeComponent::Auto, BackgroundSizeComponent::Auto),
      origin: MaskOrigin::BorderBox,
      clip: MaskClip::BorderBox,
      mode: MaskMode::Alpha,
      composite: MaskComposite::Add,
    };
    let mask = ResolvedMask {
      layers: vec![layer],
      color: Rgba::BLACK,
      font_size: 16.0,
      root_font_size: 16.0,
      rects: MaskReferenceRects {
        border: bounds,
        padding: bounds,
        content: bounds,
      },
    };

    let mut list = DisplayList::new();
    list.push(DisplayItem::PushStackingContext(StackingContextItem {
      z_index: 0,
      creates_stacking_context: true,
      bounds,
      plane_rect: bounds,
      mix_blend_mode: BlendMode::Normal,
      is_isolated: false,
      transform: None,
      child_perspective: None,
      transform_style: TransformStyle::Flat,
      backface_visibility: BackfaceVisibility::Visible,
      filters: Vec::new(),
      backdrop_filters: Vec::new(),
      radii: BorderRadii::ZERO,
      mask: Some(mask),
    }));
    list.push(DisplayItem::PopStackingContext);

    let viewport = Rect::from_xywh(0.0, 0.0, 100.0, 100.0);
    let (optimized, _stats) = optimize_with_stats(list, viewport);

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

  #[test]
  fn test_translated_rect_culled_under_transform() {
    let mut list = DisplayList::new();
    list.push(DisplayItem::PushTransform(TransformItem {
      transform: Transform3D::translate(300.0, 0.0, 0.0),
    }));
    list.push(make_fill_rect(0.0, 0.0, 50.0, 50.0, Rgba::RED));
    list.push(DisplayItem::PopTransform);

    let viewport = Rect::from_xywh(0.0, 0.0, 100.0, 100.0);
    let (optimized, stats) = optimize_with_stats(list, viewport);

    assert_eq!(stats.culled_count, 1);
    assert!(!optimized
      .items()
      .iter()
      .any(|item| matches!(item, DisplayItem::FillRect(_))));
  }

  #[test]
  fn test_rotated_rect_kept_when_intersecting() {
    let mut list = DisplayList::new();
    list.push(DisplayItem::PushTransform(TransformItem {
      transform: Transform3D::rotate_z(FRAC_PI_4),
    }));
    list.push(make_fill_rect(0.0, 0.0, 30.0, 30.0, Rgba::BLUE));
    list.push(DisplayItem::PopTransform);

    let viewport = Rect::from_xywh(0.0, 0.0, 40.0, 40.0);
    let (optimized, stats) = optimize_with_stats(list, viewport);

    assert_eq!(stats.culled_count, 0);
    assert!(optimized
      .items()
      .iter()
      .any(|item| matches!(item, DisplayItem::FillRect(_))));
  }

  #[test]
  fn test_perspective_transform_culls_offscreen_rect() {
    let mut transform = Transform3D::identity();
    transform.m[3] = 0.001;
    transform.m[12] = 500.0;

    let mut list = DisplayList::new();
    list.push(DisplayItem::PushTransform(TransformItem { transform }));
    list.push(make_fill_rect(0.0, 0.0, 50.0, 50.0, Rgba::GREEN));
    list.push(DisplayItem::PopTransform);

    let viewport = Rect::from_xywh(0.0, 0.0, 100.0, 100.0);
    let (optimized, stats) = optimize_with_stats(list, viewport);

    assert_eq!(stats.culled_count, 1);
    assert!(!optimized
      .items()
      .iter()
      .any(|item| matches!(item, DisplayItem::FillRect(_))));
  }

  #[test]
  fn test_degenerate_homography_falls_back_conservative() {
    let mut transform = Transform3D::identity();
    transform.m[3] = -0.002;
    transform.m[12] = 500.0;

    let mut list = DisplayList::new();
    list.push(DisplayItem::PushTransform(TransformItem { transform }));
    list.push(make_fill_rect(500.0, 0.0, 50.0, 50.0, Rgba::BLUE));
    list.push(DisplayItem::PopTransform);

    let viewport = Rect::from_xywh(0.0, 0.0, 100.0, 100.0);
    let (optimized, stats) = optimize_with_stats(list, viewport);

    assert_eq!(stats.culled_count, 0);
    assert!(optimized
      .items()
      .iter()
      .any(|item| matches!(item, DisplayItem::FillRect(_))));
  }
}
