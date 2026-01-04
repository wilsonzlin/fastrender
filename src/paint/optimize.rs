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

use crate::error::{Error, RenderError, RenderStage, Result};
use crate::geometry::{Point, Rect};
use crate::paint::display_list::BlendMode;
use crate::paint::display_list::DisplayItem;
use crate::paint::display_list::DisplayList;
use crate::paint::display_list::DisplayListView;
use crate::paint::display_list::FillRectItem;
use crate::paint::display_list::StackingContextItem;
use crate::paint::display_list::Transform3D;
use crate::paint::filter_outset::filter_outset_with_bounds;
use crate::render_control::{check_active, check_active_periodic};
use crate::style::types::TransformStyle;

const DEADLINE_STRIDE: usize = 256;

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
    let removed = self.original_count.saturating_sub(self.final_count);
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
    match self.optimize_checked(&list, viewport) {
      Ok((optimized, stats)) => (optimized, stats),
      Err(_err) => {
        // Optimization is best-effort. When rendering under a deadline (e.g. pageset),
        // `check_active` can abort optimization early; callers should still be able to
        // rasterize the unoptimized list instead of panicking.
        (
          list,
          OptimizationStats {
            original_count,
            final_count: original_count,
            ..Default::default()
          },
        )
      }
    }
  }

  /// Optimize a display list with deadline awareness.
  pub fn optimize_checked(
    &self,
    list: &DisplayList,
    viewport: Rect,
  ) -> Result<(DisplayList, OptimizationStats)> {
    let original_count = list.len();
    check_active(RenderStage::Paint).map_err(Error::Render)?;

    let items = list.items();
    let mut indices: Vec<usize> = (0..items.len()).collect();
    let mut scratch_indices: Vec<usize> = Vec::with_capacity(indices.len());
    let mut stats = OptimizationStats {
      original_count,
      ..Default::default()
    };
    let mut deadline_counter = 0usize;

    // Pass 1: Remove transparent items
    if self.config.enable_transparent_removal {
      let before = indices.len();
      self
        .remove_transparent_indices_checked(
          &items,
          &indices,
          &mut scratch_indices,
          &mut deadline_counter,
        )
        .map_err(Error::Render)?;
      std::mem::swap(&mut indices, &mut scratch_indices);
      stats.transparent_removed = before - indices.len();
    }

    // Pass 2: Remove no-op operations
    if self.config.enable_noop_removal {
      let before = indices.len();
      self
        .remove_noop_indices_checked(
          &items,
          &indices,
          &mut scratch_indices,
          &mut deadline_counter,
        )
        .map_err(Error::Render)?;
      std::mem::swap(&mut indices, &mut scratch_indices);
      stats.noop_removed = before - indices.len();
    }

    // Pass 3: Cull items outside viewport
    if self.config.enable_culling {
      let before = indices.len();
      self
        .intersect_indices_filtered_checked(
          &items,
          &indices,
          viewport,
          &mut scratch_indices,
          &mut deadline_counter,
        )
        .map_err(Error::Render)?;
      std::mem::swap(&mut indices, &mut scratch_indices);
      stats.culled_count = before - indices.len();
    }

    let tail = self
      .balance_stack_tail(items, &indices)
      .map_err(Error::Render)?;
    let mut out = Vec::with_capacity(indices.len() + tail.len());

    if self.config.enable_fill_merging {
      let mut pending_fill: Option<FillRectItem> = None;
      for &idx in &indices {
        check_active_periodic(&mut deadline_counter, DEADLINE_STRIDE, RenderStage::Paint)
          .map_err(Error::Render)?;
        match &items[idx] {
          DisplayItem::FillRect(fill) => {
            if let Some(prev) = pending_fill.take() {
              if let Some(merged) = Self::try_merge_fills(&prev, fill) {
                pending_fill = Some(merged);
                stats.merged_count += 1;
              } else {
                out.push(DisplayItem::FillRect(prev));
                pending_fill = Some(fill.clone());
              }
            } else {
              pending_fill = Some(fill.clone());
            }
          }
          other => {
            if let Some(prev) = pending_fill.take() {
              out.push(DisplayItem::FillRect(prev));
            }
            out.push(other.clone());
          }
        }
      }
      if let Some(prev) = pending_fill.take() {
        out.push(DisplayItem::FillRect(prev));
      }
    } else {
      for &idx in &indices {
        check_active_periodic(&mut deadline_counter, DEADLINE_STRIDE, RenderStage::Paint)
          .map_err(Error::Render)?;
        out.push(items[idx].clone());
      }
    }

    out.extend(tail);

    stats.final_count = out.len();
    Ok((DisplayList::from_items(out), stats))
  }

  /// Extract only the items that intersect the viewport while preserving stack
  /// operations needed for correct rasterization order.
  pub fn intersect(&self, list: &DisplayList, viewport: Rect) -> DisplayList {
    self
      .intersect_checked(list, viewport)
      .unwrap_or_else(|_| DisplayList::new())
  }

  pub fn intersect_checked(&self, list: &DisplayList, viewport: Rect) -> Result<DisplayList> {
    let items = list.items();
    let indices = self
      .intersect_indices(items, viewport)
      .map_err(Error::Render)?;
    let tail = self
      .balance_stack_tail(items, &indices)
      .map_err(Error::Render)?;
    let mut out = Vec::with_capacity(indices.len() + tail.len());
    out.extend(indices.into_iter().map(|idx| items[idx].clone()));
    out.extend(tail);
    Ok(DisplayList::from_items(out))
  }

  /// Extract indices of items that intersect the viewport while preserving stack operations.
  pub fn intersect_indices(
    &self,
    items: &[DisplayItem],
    viewport: Rect,
  ) -> std::result::Result<Vec<usize>, RenderError> {
    let mut result = Vec::with_capacity(items.len());
    let mut deadline_counter = 0usize;
    self.intersect_indices_iter_checked(
      items.iter().enumerate(),
      viewport,
      &mut result,
      &mut deadline_counter,
    )?;
    Ok(result)
  }

  fn intersect_indices_filtered_checked(
    &self,
    items: &[DisplayItem],
    indices: &[usize],
    viewport: Rect,
    out: &mut Vec<usize>,
    deadline_counter: &mut usize,
  ) -> std::result::Result<(), RenderError> {
    self.intersect_indices_iter_checked(
      indices.iter().map(|&idx| (idx, &items[idx])),
      viewport,
      out,
      deadline_counter,
    )
  }

  fn intersect_indices_iter_checked<'a, I>(
    &self,
    iter: I,
    viewport: Rect,
    result: &mut Vec<usize>,
    deadline_counter: &mut usize,
  ) -> std::result::Result<(), RenderError>
  where
    I: IntoIterator<Item = (usize, &'a DisplayItem)>,
  {
    result.clear();
    let mut transform_state = TransformState::default();
    let mut transform_stack: Vec<TransformState> = Vec::new();
    let mut context_stack: Vec<ContextRecord> = Vec::new();
    let mut clip_stack: Vec<ClipRecord> = Vec::new();
    let mut filter_stack: Vec<f32> = Vec::new();
    // Running aggregates to avoid repeatedly scanning the stacks.
    let mut filter_outset_sum = 0.0f32;
    let mut active_cull_clips: usize = 0;
    let mut clips_can_cull_any = false;

    let refresh_context_clipping = |contexts: &mut [ContextRecord], clips_can_cull_any: bool| {
      for ctx in contexts {
        ctx.clipped_by_clip = clips_can_cull_any;
      }
    };

    let disable_clip_culling =
      |clip_stack: &mut [ClipRecord], active_cull_clips: &mut usize| -> bool {
        if *active_cull_clips == 0 {
          return false;
        }
        let mut disabled = 0usize;
        for clip in clip_stack {
          if clip.can_cull {
            clip.can_cull = false;
            disabled += 1;
          }
        }
        if disabled > 0 {
          *active_cull_clips = active_cull_clips.saturating_sub(disabled);
          return true;
        }
        false
      };

    for (index, item) in iter {
      check_active_periodic(deadline_counter, DEADLINE_STRIDE, RenderStage::Paint)?;
      let mut include_item = false;
      let mut transformed_bounds: Option<Rect> = None;
      let culled_by_clip = clip_stack.last().map(|c| c.can_cull).unwrap_or(false);

      match item {
        DisplayItem::PushTransform(t) => {
          transform_stack.push(transform_state.clone());
          if !clip_stack.is_empty()
            && disable_clip_culling(&mut clip_stack, &mut active_cull_clips)
          {
            clips_can_cull_any = active_cull_clips > 0;
            refresh_context_clipping(&mut context_stack, clips_can_cull_any);
          }
          transform_state.current_3d = transform_state.current_3d.multiply(&t.transform);
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
          let mut context_transform = transform_state.current_3d;
          if let Some(transform) = sc.transform.as_ref() {
            context_transform = context_transform.multiply(transform);
          }
          if let Some(perspective) = sc.child_perspective.as_ref() {
            context_transform = context_transform.multiply(perspective);
          }
          if pushed_transform {
            transform_stack.push(transform_state.clone());
            if !clip_stack.is_empty()
              && disable_clip_culling(&mut clip_stack, &mut active_cull_clips)
            {
              clips_can_cull_any = active_cull_clips > 0;
              refresh_context_clipping(&mut context_stack, clips_can_cull_any);
            }
            transform_state.current_3d = context_transform;
          }
          let filters_outset = filter_outset_with_bounds(&sc.filters, 1.0, Some(sc.bounds));
          let backdrop_outset =
            filter_outset_with_bounds(&sc.backdrop_filters, 1.0, Some(sc.bounds));
          let max_outset = filters_outset.max_side().max(backdrop_outset.max_side());
          let world_outset = if max_outset == 0.0 {
            0.0
          } else {
            max_outset * Self::transform_scale_factor(&context_transform, sc.bounds)
          };
          filter_stack.push(world_outset);
          filter_outset_sum += world_outset;

          context_stack.push(ContextRecord {
            start_index: result.len(),
            bounds: None,
            item: sc.clone(),
            transform_for_bounds: Some(context_transform),
            clipped_by_clip: clips_can_cull_any,
            culling_disabled: false,
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
            if let Some(outset) = filter_stack.pop() {
              filter_outset_sum -= outset;
            }

            if record.culling_disabled {
              // Keep conservatively when transforms prevented culling
              if let Some(parent) = context_stack.last_mut() {
                parent.culling_disabled = true;
              }
            } else if record.clipped_by_clip {
              result.truncate(record.start_index);
              refresh_context_clipping(&mut context_stack, clips_can_cull_any);
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
                if ctx_bounds.is_none() {
                  if let Some(parent) = context_stack.last_mut() {
                    parent.culling_disabled = true;
                  }
                }
                if let Some(parent) = context_stack.last_mut() {
                  if !parent.culling_disabled && active_cull_clips == 0 {
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
                refresh_context_clipping(&mut context_stack, clips_can_cull_any);
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
          let can_cull = if let Some(mut world_bounds) =
            Self::transform_rect_3d_z0(&transform_state.current_3d, local_bounds)
          {
            let inflate = filter_outset_sum;
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
          if can_cull {
            active_cull_clips += 1;
          }
          clips_can_cull_any = active_cull_clips > 0;
          refresh_context_clipping(&mut context_stack, clips_can_cull_any);
          include_item = true;
        }
        DisplayItem::PopClip => {
          if let Some(record) = clip_stack.pop() {
            if record.can_cull {
              active_cull_clips = active_cull_clips.saturating_sub(1);
            }
            clips_can_cull_any = active_cull_clips > 0;
            refresh_context_clipping(&mut context_stack, clips_can_cull_any);
            if record.can_cull {
              result.truncate(record.start_index);
              continue;
            }
          }
          clips_can_cull_any = active_cull_clips > 0;
          include_item = true;
        }
        _ => {}
      }

      if culled_by_clip {
        include_item = false;
        transformed_bounds = None;
      }

      if !include_item {
        if let Some(local_bounds) = self.item_bounds(item) {
          if let Some(mut bounds) =
            Self::transform_rect_3d_z0(&transform_state.current_3d, local_bounds)
          {
            let inflate = filter_outset_sum;
            if inflate > 0.0 {
              bounds = bounds.inflate(inflate);
            }
            include_item = viewport.intersects(bounds);
            transformed_bounds = Some(bounds);
          } else {
            include_item = true;
            if let Some(context) = context_stack.last_mut() {
              context.culling_disabled = true;
            }
          }
        } else {
          include_item = item.is_stack_operation();
        }
      }

      if include_item {
        result.push(index);
        if transformed_bounds.is_none() {
          if let Some(local_bounds) = self.item_bounds(item) {
            if let Some(mut bounds) =
              Self::transform_rect_3d_z0(&transform_state.current_3d, local_bounds)
            {
              let inflate = filter_outset_sum;
              if inflate > 0.0 {
                bounds = bounds.inflate(inflate);
              }
              transformed_bounds = Some(bounds);
            } else if let Some(context) = context_stack.last_mut() {
              context.culling_disabled = true;
            }
          }
        }
        if let Some(bounds) = transformed_bounds {
          if active_cull_clips == 0 {
            if let Some(context) = context_stack.last_mut() {
              if !context.culling_disabled {
                context.bounds = Some(match context.bounds {
                  Some(existing) => existing.union(bounds),
                  None => bounds,
                });
              }
            }
          }
        }
      }
    }

    Ok(())
  }

  /// Build a display list view containing only the items that intersect the viewport.
  pub fn intersect_view<'a>(
    &self,
    items: &'a [DisplayItem],
    viewport: Rect,
  ) -> std::result::Result<DisplayListView<'a>, RenderError> {
    let indices = self.intersect_indices(items, viewport)?;
    let tail = self.balance_stack_tail(items, &indices)?;
    Ok(DisplayListView::new(items, indices, tail))
  }

  /// Remove fully transparent items (and fully transparent opacity scopes) from an index list.
  fn remove_transparent_indices_checked(
    &self,
    items: &[DisplayItem],
    indices: &[usize],
    out: &mut Vec<usize>,
    counter: &mut usize,
  ) -> std::result::Result<(), RenderError> {
    out.clear();
    out.reserve(indices.len());

    let mut skip_opacity_depth: usize = 0;

    for &idx in indices {
      check_active_periodic(counter, DEADLINE_STRIDE, RenderStage::Paint)?;
      let item = &items[idx];

      if skip_opacity_depth > 0 {
        match item {
          DisplayItem::PushOpacity(_) => skip_opacity_depth += 1,
          DisplayItem::PopOpacity => skip_opacity_depth -= 1,
          _ => {}
        }
        continue;
      }

      if let DisplayItem::PushOpacity(opacity) = item {
        if opacity.opacity <= 0.0 {
          skip_opacity_depth = 1;
          continue;
        }
      }

      if Self::is_transparent(item) {
        continue;
      }

      out.push(idx);
    }

    Ok(())
  }

  /// Check if an item is fully transparent
  fn is_transparent(item: &DisplayItem) -> bool {
    match item {
      DisplayItem::FillRect(item) => item.color.a == 0.0,
      DisplayItem::StrokeRect(item) => item.color.a == 0.0,
      DisplayItem::FillRoundedRect(item) => item.color.a == 0.0,
      DisplayItem::StrokeRoundedRect(item) => item.color.a == 0.0,
      DisplayItem::Text(item) => {
        // Text can still paint shadows/emphasis even when the fill is fully transparent.
        // Treat the item as transparent only when *all* contributions are invisible.
        if item.color.a > 0.0 {
          return false;
        }
        if item.shadows.iter().any(|shadow| shadow.color.a > 0.0) {
          return false;
        }
        if item.emphasis.as_ref().is_some_and(|emphasis| emphasis.color.a > 0.0) {
          return false;
        }
        true
      }
      DisplayItem::Outline(item) => {
        item.width <= 0.0
          || matches!(
            item.style,
            crate::style::types::BorderStyle::None | crate::style::types::BorderStyle::Hidden
          )
          || item.color.a == 0.0
      }
      DisplayItem::Border(border) => {
        let invisible = |side: &crate::paint::display_list::BorderSide| {
          side.width <= 0.0
            || matches!(
              side.style,
              crate::style::types::BorderStyle::None | crate::style::types::BorderStyle::Hidden
            )
            || side.color.a == 0.0
        };
        invisible(&border.top)
          && invisible(&border.right)
          && invisible(&border.bottom)
          && invisible(&border.left)
      }
      DisplayItem::TextDecoration(item) => item.decorations.iter().all(|d| {
        d.color.a == 0.0
          || (d.underline.is_none() && d.overline.is_none() && d.line_through.is_none())
      }),
      DisplayItem::BoxShadow(item) => item.color.a == 0.0,
      _ => false,
    }
  }

  /// Remove no-op operations (identity transforms, 1.0 opacity, normal blend)
  fn remove_noop_indices_checked(
    &self,
    items: &[DisplayItem],
    indices: &[usize],
    out: &mut Vec<usize>,
    counter: &mut usize,
  ) -> std::result::Result<(), RenderError> {
    out.clear();
    out.reserve(indices.len());

    let mut opacity_stack: Vec<bool> = Vec::new();
    let mut transform_stack: Vec<bool> = Vec::new();
    let mut blend_stack: Vec<bool> = Vec::new();
    let mut stacking_context_stack: Vec<bool> = Vec::new();
    let mut culled_transform_depth = 0usize;
    let mut culled_clip_depth = 0usize;

    for &idx in indices {
      check_active_periodic(counter, DEADLINE_STRIDE, RenderStage::Paint)?;
      let item = &items[idx];
      if culled_transform_depth > 0 {
        match item {
          DisplayItem::PushTransform(_) => culled_transform_depth += 1,
          DisplayItem::PopTransform => culled_transform_depth -= 1,
          _ => {}
        }
        continue;
      }
      if culled_clip_depth > 0 {
        match item {
          DisplayItem::PushClip(_) => culled_clip_depth += 1,
          DisplayItem::PopClip => culled_clip_depth -= 1,
          _ => {}
        }
        continue;
      }
      match item {
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
          let collapses = Self::transform_collapses(&t.transform);
          if collapses {
            culled_transform_depth = 1;
            continue;
          }
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
        DisplayItem::PushClip(clip) => {
          if Self::clip_is_empty(clip) {
            culled_clip_depth = 1;
            continue;
          }
        }
        DisplayItem::PopClip => {}
        _ => {}
      }
      out.push(idx);
    }

    Ok(())
  }

  fn is_noop_stacking_context(item: &StackingContextItem) -> bool {
    item.z_index == 0
      && matches!(item.transform_style, TransformStyle::Flat)
      && item.child_perspective.is_none()
      && item.transform.is_none()
      && item.filters.is_empty()
      && item.backdrop_filters.is_empty()
      && item.mask.is_none()
      && item.mix_blend_mode == BlendMode::Normal
      && !item.is_isolated
      && item.radii.is_zero()
  }

  /// Generate the missing pop operations needed to balance the provided item indices.
  pub fn balance_stack_tail(
    &self,
    items: &[DisplayItem],
    indices: &[usize],
  ) -> std::result::Result<Vec<DisplayItem>, RenderError> {
    #[derive(Copy, Clone, Debug, Eq, PartialEq)]
    enum StackEntry {
      Clip,
      Opacity,
      Transform,
      Blend,
      StackingContext,
    }

    impl StackEntry {
      fn pop_item(self) -> DisplayItem {
        match self {
          StackEntry::Clip => DisplayItem::PopClip,
          StackEntry::Opacity => DisplayItem::PopOpacity,
          StackEntry::Transform => DisplayItem::PopTransform,
          StackEntry::Blend => DisplayItem::PopBlendMode,
          StackEntry::StackingContext => DisplayItem::PopStackingContext,
        }
      }
    }

    let mut stack: Vec<StackEntry> = Vec::new();

    let pop_entry = |stack: &mut Vec<StackEntry>, entry: StackEntry| {
      match stack.last() {
        Some(top) if *top == entry => {
          stack.pop();
        }
        _ => {
          // The indices stream should be nesting-correct; however, views can be sliced, and
          // consumers may pass in index lists that start/end mid-scope. Recover conservatively by
          // unwinding to the last matching push (treating any inner scopes as already closed).
          if let Some(pos) = stack.iter().rposition(|e| *e == entry) {
            stack.truncate(pos);
          }
        }
      }
    };

    let mut counter = 0usize;
    for &idx in indices {
      check_active_periodic(&mut counter, DEADLINE_STRIDE, RenderStage::Paint)?;
      if let Some(item) = items.get(idx) {
        match item {
          DisplayItem::PushClip(_) => stack.push(StackEntry::Clip),
          DisplayItem::PopClip => pop_entry(&mut stack, StackEntry::Clip),
          DisplayItem::PushOpacity(_) => stack.push(StackEntry::Opacity),
          DisplayItem::PopOpacity => pop_entry(&mut stack, StackEntry::Opacity),
          DisplayItem::PushTransform(_) => stack.push(StackEntry::Transform),
          DisplayItem::PopTransform => pop_entry(&mut stack, StackEntry::Transform),
          DisplayItem::PushBlendMode(_) => stack.push(StackEntry::Blend),
          DisplayItem::PopBlendMode => pop_entry(&mut stack, StackEntry::Blend),
          DisplayItem::PushStackingContext(_) => stack.push(StackEntry::StackingContext),
          DisplayItem::PopStackingContext => pop_entry(&mut stack, StackEntry::StackingContext),
          _ => {}
        }
      }
    }

    let mut tail = Vec::with_capacity(stack.len());
    while let Some(entry) = stack.pop() {
      tail.push(entry.pop_item());
    }

    Ok(tail)
  }

  fn item_bounds(&self, item: &DisplayItem) -> Option<Rect> {
    item.bounds()
  }

  fn clip_bounds(clip: &crate::paint::display_list::ClipItem) -> Rect {
    match &clip.shape {
      crate::paint::display_list::ClipShape::Rect { rect, .. } => *rect,
      crate::paint::display_list::ClipShape::Path { path } => path.bounds(),
    }
  }

  fn clip_is_empty(clip: &crate::paint::display_list::ClipItem) -> bool {
    let bounds = Self::clip_bounds(clip);
    bounds.width() <= 0.0
      || bounds.height() <= 0.0
      || !bounds.x().is_finite()
      || !bounds.y().is_finite()
  }

  fn transform_collapses(transform: &Transform3D) -> bool {
    let unit = Rect::from_xywh(0.0, 0.0, 1.0, 1.0);
    let corners = [
      (unit.min_x(), unit.min_y()),
      (unit.max_x(), unit.min_y()),
      (unit.max_x(), unit.max_y()),
      (unit.min_x(), unit.max_y()),
    ];
    let mut min_x = f32::INFINITY;
    let mut min_y = f32::INFINITY;
    let mut max_x = f32::NEG_INFINITY;
    let mut max_y = f32::NEG_INFINITY;

    for (x, y) in corners {
      let Some(p) = transform.project_point_2d(x, y) else {
        return false;
      };
      min_x = min_x.min(p.x);
      min_y = min_y.min(p.y);
      max_x = max_x.max(p.x);
      max_y = max_y.max(p.y);
    }

    let width = max_x - min_x;
    let height = max_y - min_y;
    !(width > 0.0 && height > 0.0 && width.is_finite() && height.is_finite())
  }

  fn transform_rect_3d_z0(transform: &Transform3D, rect: Rect) -> Option<Rect> {
    if transform.is_identity() {
      return Some(rect);
    }
    let corners = [
      (rect.min_x(), rect.min_y()),
      (rect.max_x(), rect.min_y()),
      (rect.max_x(), rect.max_y()),
      (rect.min_x(), rect.max_y()),
    ];
    let mut min_x = f32::INFINITY;
    let mut min_y = f32::INFINITY;
    let mut max_x = f32::NEG_INFINITY;
    let mut max_y = f32::NEG_INFINITY;

    for (x, y) in corners {
      let (tx, ty, _tz, tw) = transform.transform_point(x, y, 0.0);
      if !tx.is_finite()
        || !ty.is_finite()
        || !tw.is_finite()
        || tw.abs() < Transform3D::MIN_PROJECTIVE_W
        || tw <= 0.0
      {
        return None;
      }
      let px = tx / tw;
      let py = ty / tw;
      if !px.is_finite() || !py.is_finite() {
        return None;
      }
      min_x = min_x.min(px);
      min_y = min_y.min(py);
      max_x = max_x.max(px);
      max_y = max_y.max(py);
    }

    let width = max_x - min_x;
    let height = max_y - min_y;
    if width <= 0.0 || height <= 0.0 {
      return None;
    }
    Some(Rect::from_xywh(min_x, min_y, width, height))
  }

  fn transform_scale_factor(transform: &Transform3D, reference: Rect) -> f32 {
    if transform.is_identity() {
      return 1.0;
    }
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
      let base = match transform.project_point_2d(p.x, p.y) {
        Some(p) => p,
        None => return f32::INFINITY,
      };
      let dx = match transform.project_point_2d(p.x + 1.0, p.y) {
        Some(p) => p,
        None => return f32::INFINITY,
      };
      let dy = match transform.project_point_2d(p.x, p.y + 1.0) {
        Some(p) => p,
        None => return f32::INFINITY,
      };
      let scale_x = ((dx.x - base.x).powi(2) + (dx.y - base.y).powi(2)).sqrt();
      let scale_y = ((dy.x - base.x).powi(2) + (dy.y - base.y).powi(2)).sqrt();
      max_scale = max_scale.max(scale_x.max(scale_y));
    }
    max_scale
  }

  fn stacking_context_bounds(
    &self,
    item: &StackingContextItem,
    children: Option<Rect>,
    transform: Option<&Transform3D>,
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
        Some(transform) => Self::transform_rect_3d_z0(transform, item.bounds)?,
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
  transform_for_bounds: Option<Transform3D>,
  clipped_by_clip: bool,
  culling_disabled: bool,
  pushed_transform: bool,
}

#[derive(Clone)]
struct TransformState {
  current_3d: Transform3D,
}

impl Default for TransformState {
  fn default() -> Self {
    Self {
      current_3d: Transform3D::identity(),
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
    StackingContextItem, TextItem, TextShadowItem, Transform3D, TransformItem,
  };
  use crate::paint::filter_outset::filter_outset;
  use crate::style::color::Rgba;
  use crate::style::types::{
    BackfaceVisibility, BackgroundPosition, BackgroundPositionComponent, BackgroundRepeat,
    BackgroundSize, BackgroundSizeComponent, MaskClip, MaskComposite, MaskMode, MaskOrigin,
    TransformStyle,
  };
  use crate::style::values::Length;
  use std::f32::consts::{FRAC_PI_3, FRAC_PI_4};

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
  fn balance_stack_tail_respects_clip_stacking_context_nesting() {
    let optimizer = DisplayListOptimizer::new();

    let items = vec![
      DisplayItem::PushClip(ClipItem {
        shape: ClipShape::Rect {
          rect: Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
          radii: None,
        },
      }),
      DisplayItem::PushStackingContext(StackingContextItem {
        z_index: 0,
        creates_stacking_context: true,
        bounds: Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
        plane_rect: Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
        mix_blend_mode: BlendMode::Normal,
        is_isolated: false,
        transform: None,
        child_perspective: None,
        transform_style: TransformStyle::Flat,
        backface_visibility: BackfaceVisibility::Visible,
        filters: Vec::new(),
        backdrop_filters: Vec::new(),
        radii: BorderRadii::ZERO,
        mask: None,
      }),
      make_fill_rect(0.0, 0.0, 5.0, 5.0, Rgba::RED),
    ];
    let indices = vec![0, 1, 2];

    let tail = optimizer.balance_stack_tail(&items, &indices).unwrap();
    assert!(matches!(
      tail.as_slice(),
      [DisplayItem::PopStackingContext, DisplayItem::PopClip]
    ));

    let view = DisplayListView::new(&items, indices, tail);
    let out: Vec<DisplayItem> = view.iter().cloned().collect();
    assert_balanced(&out);
  }

  #[test]
  fn balance_stack_tail_respects_clip_opacity_nesting() {
    let optimizer = DisplayListOptimizer::new();

    let items = vec![
      DisplayItem::PushClip(ClipItem {
        shape: ClipShape::Rect {
          rect: Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
          radii: None,
        },
      }),
      DisplayItem::PushOpacity(OpacityItem { opacity: 0.5 }),
      make_fill_rect(0.0, 0.0, 5.0, 5.0, Rgba::RED),
    ];
    let indices = vec![0, 1, 2];

    let tail = optimizer.balance_stack_tail(&items, &indices).unwrap();
    assert!(matches!(
      tail.as_slice(),
      [DisplayItem::PopOpacity, DisplayItem::PopClip]
    ));

    let view = DisplayListView::new(&items, indices, tail);
    let out: Vec<DisplayItem> = view.iter().cloned().collect();
    assert_balanced(&out);
  }

  #[test]
  fn balance_stack_tail_empty_for_already_balanced_input() {
    let optimizer = DisplayListOptimizer::new();

    let items = vec![
      DisplayItem::PushClip(ClipItem {
        shape: ClipShape::Rect {
          rect: Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
          radii: None,
        },
      }),
      make_fill_rect(0.0, 0.0, 5.0, 5.0, Rgba::RED),
      DisplayItem::PopClip,
    ];
    let indices = vec![0, 1, 2];

    let tail = optimizer.balance_stack_tail(&items, &indices).unwrap();
    assert!(tail.is_empty());

    let view = DisplayListView::new(&items, indices, tail);
    let out: Vec<DisplayItem> = view.iter().cloned().collect();
    assert_balanced(&out);
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
  fn transparent_text_with_shadow_not_removed() {
    let glyphs = vec![GlyphInstance {
      glyph_id: 1,
      cluster: 0,
      x_offset: 0.0,
      y_offset: 0.0,
      x_advance: 10.0,
      y_advance: 0.0,
    }];
    let mut list = DisplayList::new();
    list.push(DisplayItem::Text(TextItem {
      origin: Point::new(0.0, 20.0),
      glyphs,
      color: Rgba::TRANSPARENT,
      shadows: vec![TextShadowItem {
        offset: Point::new(4.0, 4.0),
        blur_radius: 0.0,
        color: Rgba::RED,
      }],
      font_size: 20.0,
      advance_width: 10.0,
      ..Default::default()
    }));

    let viewport = Rect::from_xywh(0.0, 0.0, 200.0, 200.0);
    let (optimized, stats) = optimize_with_stats(list, viewport);

    assert_eq!(stats.transparent_removed, 0);
    assert!(matches!(optimized.items(), [DisplayItem::Text(_)]));
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
        cluster: 0,
        x_offset: -70.0 + i as f32 * 2.0,
        y_offset: 0.0,
        x_advance: 2.0,
        y_advance: 0.0,
      })
      .collect();
    let mut text = TextItem {
      origin: Point::new(150.0, 50.0),
      cached_bounds: None,
      glyphs,
      color: Rgba::BLACK,
      font_size: 20.0,
      advance_width: 400.0,
      ..Default::default()
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
  fn child_perspective_offscreen_parent_not_culled_when_child_moves_into_view() {
    let mut list = DisplayList::new();
    list.push(DisplayItem::PushStackingContext(StackingContextItem {
      z_index: 0,
      creates_stacking_context: true,
      bounds: Rect::from_xywh(500.0, 0.0, 100.0, 100.0),
      plane_rect: Rect::from_xywh(500.0, 0.0, 100.0, 100.0),
      mix_blend_mode: BlendMode::Normal,
      is_isolated: false,
      transform: None,
      child_perspective: Some(Transform3D::perspective(200.0)),
      transform_style: TransformStyle::Flat,
      backface_visibility: BackfaceVisibility::Visible,
      filters: Vec::new(),
      backdrop_filters: Vec::new(),
      radii: BorderRadii::ZERO,
      mask: None,
    }));
    list.push(DisplayItem::PushStackingContext(StackingContextItem {
      z_index: 0,
      creates_stacking_context: true,
      bounds: Rect::from_xywh(500.0, 0.0, 50.0, 50.0),
      plane_rect: Rect::from_xywh(500.0, 0.0, 50.0, 50.0),
      mix_blend_mode: BlendMode::Normal,
      is_isolated: false,
      transform: Some(Transform3D::translate(-500.0, 0.0, 0.0)),
      child_perspective: None,
      transform_style: TransformStyle::Flat,
      backface_visibility: BackfaceVisibility::Visible,
      filters: Vec::new(),
      backdrop_filters: Vec::new(),
      radii: BorderRadii::ZERO,
      mask: None,
    }));
    list.push(make_fill_rect(500.0, 0.0, 50.0, 50.0, Rgba::GREEN));
    list.push(DisplayItem::PopStackingContext);
    list.push(DisplayItem::PopStackingContext);

    let viewport = Rect::from_xywh(0.0, 0.0, 100.0, 100.0);
    let (optimized, _stats) = optimize_with_stats(list, viewport);

    assert!(
      optimized
        .items()
        .iter()
        .any(|item| matches!(item, DisplayItem::FillRect(_))),
      "child_perspective stacking contexts should not be culled solely based on parent bounds"
    );
    assert_balanced(optimized.items());
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
  fn preserve_3d_stacking_context_not_removed_as_noop() {
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
      transform_style: TransformStyle::Preserve3d,
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

    assert_eq!(stats.noop_removed, 0);
    assert_eq!(optimized.len(), 3);
    assert!(
      matches!(
        optimized.items(),
        [
          DisplayItem::PushStackingContext(_),
          DisplayItem::FillRect(_),
          DisplayItem::PopStackingContext
        ]
      ),
      "preserve-3d stacking context should be retained"
    );
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
      viewport: None,
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
  fn child_perspective_enables_viewport_culling() {
    let mut list = DisplayList::new();
    list.push(DisplayItem::PushStackingContext(StackingContextItem {
      z_index: 0,
      creates_stacking_context: true,
      bounds: Rect::from_xywh(0.0, 0.0, 400.0, 200.0),
      plane_rect: Rect::from_xywh(0.0, 0.0, 400.0, 200.0),
      mix_blend_mode: BlendMode::Normal,
      is_isolated: false,
      transform: None,
      child_perspective: Some(Transform3D::perspective(200.0)),
      transform_style: TransformStyle::Flat,
      backface_visibility: BackfaceVisibility::Visible,
      filters: Vec::new(),
      backdrop_filters: Vec::new(),
      radii: BorderRadii::ZERO,
      mask: None,
    }));

    list.push(DisplayItem::PushStackingContext(StackingContextItem {
      z_index: 0,
      creates_stacking_context: true,
      bounds: Rect::from_xywh(0.0, 0.0, 400.0, 200.0),
      plane_rect: Rect::from_xywh(0.0, 0.0, 400.0, 200.0),
      mix_blend_mode: BlendMode::Normal,
      is_isolated: false,
      transform: Some(Transform3D::translate(0.0, 0.0, -100.0)),
      child_perspective: None,
      transform_style: TransformStyle::Flat,
      backface_visibility: BackfaceVisibility::Visible,
      filters: Vec::new(),
      backdrop_filters: Vec::new(),
      radii: BorderRadii::ZERO,
      mask: None,
    }));

    list.push(make_fill_rect(150.0, 10.0, 20.0, 20.0, Rgba::RED));
    list.push(make_fill_rect(300.0, 10.0, 20.0, 20.0, Rgba::GREEN));
    list.push(DisplayItem::PopStackingContext);
    list.push(DisplayItem::PopStackingContext);

    let viewport = Rect::from_xywh(0.0, 0.0, 100.0, 100.0);
    let optimizer = DisplayListOptimizer::new();
    let view = optimizer
      .intersect_view(list.items(), viewport)
      .expect("intersect_view failed");

    let out: Vec<DisplayItem> = view.iter().cloned().collect();
    let fill_rects: Vec<Rect> = out
      .iter()
      .filter_map(|item| match item {
        DisplayItem::FillRect(fill) => Some(fill.rect),
        _ => None,
      })
      .collect();

    assert!(
      fill_rects
        .iter()
        .any(|rect| (rect.x() - 150.0).abs() < f32::EPSILON),
      "rect at x=150 should be kept; got {fill_rects:?}"
    );
    assert!(
      !fill_rects
        .iter()
        .any(|rect| (rect.x() - 300.0).abs() < f32::EPSILON),
      "rect at x=300 should be culled; got {fill_rects:?}"
    );
    assert_eq!(fill_rects.len(), 1, "exactly one fill rect should remain");
    assert!(
      out.len() < list.len(),
      "culling should reduce the slice size"
    );
    assert_balanced(&out);
  }

  #[test]
  fn child_perspective_projects_rotated_child_into_view_and_still_culls_other_children() {
    let mut list = DisplayList::new();
    list.push(DisplayItem::PushStackingContext(StackingContextItem {
      z_index: 0,
      creates_stacking_context: true,
      bounds: Rect::from_xywh(0.0, 0.0, 4000.0, 100.0),
      plane_rect: Rect::from_xywh(0.0, 0.0, 4000.0, 100.0),
      mix_blend_mode: BlendMode::Normal,
      is_isolated: false,
      transform: None,
      child_perspective: Some(Transform3D::perspective(200.0)),
      transform_style: TransformStyle::Flat,
      backface_visibility: BackfaceVisibility::Visible,
      filters: Vec::new(),
      backdrop_filters: Vec::new(),
      radii: BorderRadii::ZERO,
      mask: None,
    }));

    let child_transform = Some(Transform3D::rotate_y(FRAC_PI_3));

    list.push(DisplayItem::PushStackingContext(StackingContextItem {
      z_index: 0,
      creates_stacking_context: true,
      bounds: Rect::from_xywh(0.0, 0.0, 4000.0, 100.0),
      plane_rect: Rect::from_xywh(0.0, 0.0, 4000.0, 100.0),
      mix_blend_mode: BlendMode::Normal,
      is_isolated: false,
      transform: child_transform,
      child_perspective: None,
      transform_style: TransformStyle::Flat,
      backface_visibility: BackfaceVisibility::Visible,
      filters: Vec::new(),
      backdrop_filters: Vec::new(),
      radii: BorderRadii::ZERO,
      mask: None,
    }));
    list.push(make_fill_rect(300.0, 0.0, 50.0, 50.0, Rgba::RED));
    list.push(DisplayItem::PopStackingContext);

    list.push(DisplayItem::PushStackingContext(StackingContextItem {
      z_index: 0,
      creates_stacking_context: true,
      bounds: Rect::from_xywh(0.0, 0.0, 4000.0, 100.0),
      plane_rect: Rect::from_xywh(0.0, 0.0, 4000.0, 100.0),
      mix_blend_mode: BlendMode::Normal,
      is_isolated: false,
      transform: child_transform,
      child_perspective: None,
      transform_style: TransformStyle::Flat,
      backface_visibility: BackfaceVisibility::Visible,
      filters: Vec::new(),
      backdrop_filters: Vec::new(),
      radii: BorderRadii::ZERO,
      mask: None,
    }));
    list.push(make_fill_rect(3000.0, 0.0, 50.0, 50.0, Rgba::BLUE));
    list.push(DisplayItem::PopStackingContext);

    list.push(DisplayItem::PopStackingContext);

    let viewport = Rect::from_xywh(0.0, 0.0, 100.0, 100.0);
    let (optimized, _stats) = optimize_with_stats(list, viewport);

    assert!(optimized.items().iter().any(|item| matches!(
      item,
      DisplayItem::FillRect(fill) if fill.rect.x() == 300.0
    )));
    assert!(!optimized.items().iter().any(|item| matches!(
      item,
      DisplayItem::FillRect(fill) if fill.rect.x() == 3000.0
    )));
    assert_balanced(optimized.items());
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
