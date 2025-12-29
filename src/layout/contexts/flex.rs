//! Flexbox Formatting Context (via Taffy)
//!
//! This module implements the Flexbox layout algorithm by delegating to the Taffy library.
//! Taffy is a battle-tested layout library that implements the CSS Flexbox specification.
//!
//! # Design
//!
//! The FlexFormattingContext acts as a thin wrapper around Taffy's flexbox implementation:
//! 1. Convert BoxNode tree to Taffy tree (with Taffy styles)
//! 2. Run Taffy's `compute_layout()` algorithm
//! 3. Convert Taffy's layout results back to FragmentNode tree
//!
//! # Why Taffy?
//!
//! - Complete CSS Flexbox spec compliance
//! - Well-tested against Web Platform Tests
//! - Active maintenance by Dioxus team
//! - Saves months of implementation time
//!
//! # References
//!
//! - CSS Flexible Box Layout Module Level 1: <https://www.w3.org/TR/css-flexbox-1/>
//! - Taffy documentation: <https://docs.rs/taffy/>

use crate::geometry::Point;
use crate::geometry::Rect;
use crate::geometry::Size;
use crate::layout::absolute_positioning::resolve_positioned_style;
use crate::layout::absolute_positioning::AbsoluteLayout;
use crate::layout::absolute_positioning::AbsoluteLayoutInput;
use crate::layout::constraints::AvailableSpace as CrateAvailableSpace;
use crate::layout::constraints::LayoutConstraints;
use crate::layout::contexts::block::BlockFormattingContext;
use crate::layout::contexts::factory::FormattingContextFactory;
use crate::layout::contexts::flex_cache::{find_layout_cache_fragment, ShardedFlexCache};
use crate::layout::contexts::positioned::ContainingBlock;
use crate::layout::engine::LayoutParallelism;
use crate::layout::flex_profile::record_node_measure_hit;
use crate::layout::flex_profile::record_node_measure_store;
use crate::layout::flex_profile::DimState;
use crate::layout::flex_profile::{self};
use crate::layout::formatting_context::count_flex_intrinsic_call;
use crate::layout::formatting_context::intrinsic_cache_lookup;
use crate::layout::formatting_context::intrinsic_cache_store;
use crate::layout::formatting_context::layout_cache_lookup;
use crate::layout::formatting_context::layout_cache_store;
use crate::layout::formatting_context::FormattingContext;
use crate::layout::formatting_context::IntrinsicSizingMode;
use crate::layout::formatting_context::LayoutError;
use crate::layout::fragment_clone_profile::{self, CloneSite, CloneStats};
use crate::layout::profile::layout_timer;
use crate::layout::profile::LayoutKind;
use crate::layout::taffy_integration::{record_taffy_invocation, TaffyAdapterKind};
use crate::layout::utils::resolve_length_with_percentage_metrics;
use crate::layout::utils::resolve_scrollbar_width;
use crate::render_control::{active_deadline, check_active, with_deadline};
use crate::style::display::Display;
use crate::style::display::FormattingContextType;
use crate::style::position::Position;
use crate::style::types::AlignContent;
use crate::style::types::AlignItems;
use crate::style::types::AspectRatio;
use crate::style::types::BoxSizing;
use crate::style::types::Direction;
use crate::style::types::FlexBasis;
use crate::style::types::FlexDirection;
use crate::style::types::FlexWrap;
use crate::style::types::JustifyContent;
use crate::style::types::Overflow as CssOverflow;
use crate::style::types::WritingMode;
use crate::style::values::Length;
use crate::style::values::LengthUnit;
use crate::style::ComputedStyle;
use crate::text::font_loader::FontContext;
use crate::tree::box_tree::BoxNode;
use crate::tree::fragment_tree::FragmentContent;
use crate::tree::fragment_tree::FragmentNode;
use crate::{error::RenderError, error::RenderStage};
use rayon::prelude::*;
use std::collections::hash_map::DefaultHasher;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::hash::Hash;
use std::hash::Hasher;
use std::mem;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::OnceLock;

static LOG_CHILD_IDS: std::sync::OnceLock<Vec<usize>> = std::sync::OnceLock::new();

use taffy::prelude::*;
use taffy::style::Overflow as TaffyOverflow;
use taffy::TaffyTree;

fn translate_fragment_tree(fragment: &mut FragmentNode, delta: Point) {
  fragment.bounds = Rect::new(
    Point::new(fragment.bounds.x() + delta.x, fragment.bounds.y() + delta.y),
    fragment.bounds.size,
  );
  if let Some(logical) = fragment.logical_override {
    fragment.logical_override = Some(Rect::new(
      Point::new(logical.x() + delta.x, logical.y() + delta.y),
      logical.size,
    ));
  }
  for child in fragment.children_mut() {
    translate_fragment_tree(child, delta);
  }
}

fn normalize_fragment_origin(fragment: &mut FragmentNode) {
  let origin = fragment.bounds.origin;
  if origin.x != 0.0 || origin.y != 0.0 {
    translate_fragment_tree(fragment, Point::new(-origin.x, -origin.y));
  }
}

#[derive(Clone)]
struct CachedFragmentTemplate {
  template: Arc<FragmentNode>,
}

impl CachedFragmentTemplate {
  fn new(template: Arc<FragmentNode>) -> Self {
    Self { template }
  }

  fn fragment(&self) -> &FragmentNode {
    &self.template
  }

  fn place(&self, bounds: Rect) -> PlacedFragment {
    PlacedFragment::new(self.template.clone(), bounds)
  }
}

#[derive(Clone)]
struct PlacedFragment {
  bounds: Rect,
  template: Arc<FragmentNode>,
}

impl PlacedFragment {
  fn new(template: Arc<FragmentNode>, bounds: Rect) -> Self {
    Self { bounds, template }
  }

  fn materialize(&self) -> FragmentNode {
    let mut cloned = (*self.template).clone();
    cloned.bounds = self.bounds;
    flex_profile::record_fragment_materialize();
    cloned
  }
}

#[derive(Clone)]
struct PositionedCandidate {
  child: BoxNode,
  layout_child: BoxNode,
  cb: ContainingBlock,
  fragment: FragmentNode,
  positioned_style: crate::style::computed::PositionedStyle,
  preferred_min_inline: Option<f32>,
  preferred_inline: Option<f32>,
  preferred_min_block: Option<f32>,
  preferred_block: Option<f32>,
  is_replaced: bool,
}

fn trace_flex_text_ids() -> Vec<usize> {
  crate::debug::runtime::runtime_toggles()
    .usize_list("FASTR_TRACE_FLEX_TEXT")
    .unwrap_or_default()
}

fn count_fragment_stats(fragment: &FragmentNode, stats: &mut CloneStats) {
  stats.nodes += 1;
  match &fragment.content {
    FragmentContent::Text { text, shaped, .. } => {
      stats.text_fragments += 1;
      stats.text_bytes += text.len() as u64;
      if shaped.is_some() {
        stats.shaped_texts += 1;
      }
    }
    FragmentContent::RunningAnchor { snapshot, .. } => {
      count_fragment_stats(snapshot, stats);
    }
    _ => {}
  }
  for child in &fragment.children {
    count_fragment_stats(child, stats);
  }
}

fn record_fragment_clone(site: CloneSite, fragment: &FragmentNode) {
  if !fragment_clone_profile::fragment_clone_profile_enabled() {
    return;
  }
  let mut stats = CloneStats::default();
  count_fragment_stats(fragment, &mut stats);
  fragment_clone_profile::record_fragment_clone(site, &stats);
}

fn fragment_first_baseline(fragment: &FragmentNode) -> Option<f32> {
  if let Some(baseline) = fragment.baseline {
    return Some(baseline);
  }

  match &fragment.content {
    FragmentContent::Line { baseline } => Some(*baseline),
    FragmentContent::Text {
      baseline_offset, ..
    } => Some(*baseline_offset),
    FragmentContent::Replaced { .. } => Some(fragment.bounds.height()),
    _ => {
      for child in fragment.children.iter() {
        if let Some(baseline) = fragment_first_baseline(child) {
          return Some(child.bounds.y() - fragment.bounds.y() + baseline);
        }
      }
      None
    }
  }
}

#[derive(Clone, Copy)]
enum Axis {
  Horizontal,
  Vertical,
}

/// Flexbox Formatting Context
///
/// Delegates layout to Taffy's flexbox algorithm. This is a stateless struct
/// that creates a fresh Taffy tree for each layout operation to avoid state issues.
///
/// # Thread Safety
///
/// This struct is `Send + Sync` as required by the `FormattingContext` trait.
/// Each layout operation creates its own TaffyTree instance, ensuring thread safety.
///
/// # Example
///
/// ```ignore
/// use fastrender::layout::contexts::FlexFormattingContext;
/// use fastrender::LayoutConstraints;
/// use fastrender::tree::BoxNode;
///
/// let fc = FlexFormattingContext::new();
/// let constraints = LayoutConstraints::definite(800.0, 600.0);
/// let fragment = fc.layout(&box_node, &constraints)?;
/// ```
#[derive(Clone)]
pub struct FlexFormattingContext {
  /// Shared factory used to create child formatting contexts without losing shared caches.
  factory: FormattingContextFactory,
  /// Viewport size used for resolving viewport-relative units inside Taffy conversion.
  viewport_size: Size,
  font_context: FontContext,
  nearest_positioned_cb: ContainingBlock,
  parallelism: LayoutParallelism,
  measured_fragments: Arc<ShardedFlexCache>,
  layout_fragments: Arc<ShardedFlexCache>,
}

const MAX_MEASURE_CACHE_PER_NODE: usize = 256;
const MAX_LAYOUT_CACHE_PER_NODE: usize = 128;

impl FlexFormattingContext {
  /// Creates a new FlexFormattingContext
  pub fn new() -> Self {
    let viewport = Size::new(800.0, 600.0);
    Self::with_viewport_and_cb(
      viewport,
      ContainingBlock::viewport(viewport),
      FontContext::new(),
      Arc::new(ShardedFlexCache::new_measure()),
      Arc::new(ShardedFlexCache::new_layout()),
    )
  }

  pub fn with_viewport(viewport_size: Size) -> Self {
    Self::with_viewport_and_cb(
      viewport_size,
      ContainingBlock::viewport(viewport_size),
      FontContext::new(),
      Arc::new(ShardedFlexCache::new_measure()),
      Arc::new(ShardedFlexCache::new_layout()),
    )
  }

  pub fn with_viewport_and_cb(
    viewport_size: Size,
    nearest_positioned_cb: ContainingBlock,
    font_context: FontContext,
    measured_fragments: Arc<ShardedFlexCache>,
    layout_fragments: Arc<ShardedFlexCache>,
  ) -> Self {
    let factory = FormattingContextFactory::with_font_context_viewport_cb_and_cache(
      font_context.clone(),
      viewport_size,
      nearest_positioned_cb,
      measured_fragments.clone(),
      layout_fragments.clone(),
    );
    Self {
      factory,
      viewport_size,
      font_context,
      nearest_positioned_cb,
      parallelism: LayoutParallelism::default(),
      measured_fragments,
      layout_fragments,
    }
  }

  pub(crate) fn with_factory(factory: FormattingContextFactory) -> Self {
    let viewport_size = factory.viewport_size();
    let nearest_positioned_cb = factory.nearest_positioned_cb();
    let font_context = factory.font_context().clone();
    let measured_fragments = factory.flex_measure_cache();
    let layout_fragments = factory.flex_layout_cache();
    let parallelism = factory.parallelism();
    Self {
      factory,
      viewport_size,
      font_context,
      nearest_positioned_cb,
      parallelism,
      measured_fragments,
      layout_fragments,
    }
  }

  pub fn with_parallelism(mut self, parallelism: LayoutParallelism) -> Self {
    self.parallelism = parallelism;
    self.factory = self.factory.clone().with_parallelism(parallelism);
    self
  }

  fn child_factory(&self) -> FormattingContextFactory {
    self.factory.clone()
  }

  fn child_factory_for_cb(&self, cb: ContainingBlock) -> FormattingContextFactory {
    self.factory.with_positioned_cb(cb)
  }
}

impl Default for FlexFormattingContext {
  fn default() -> Self {
    Self::new()
  }
}

impl std::fmt::Debug for FlexFormattingContext {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("FlexFormattingContext")
      .finish_non_exhaustive()
  }
}

impl FormattingContext for FlexFormattingContext {
  /// Lays out a flex container and its children using Taffy
  ///
  /// # Process
  ///
  /// 1. Build a Taffy tree from the BoxNode tree
  /// 2. Set available space constraints
  /// 3. Run Taffy's compute_layout()
  /// 4. Convert Taffy layout results back to FragmentNode tree
  fn layout(
    &self,
    box_node: &BoxNode,
    constraints: &LayoutConstraints,
  ) -> Result<FragmentNode, LayoutError> {
    debug_assert!(
      matches!(
        box_node.formatting_context(),
        Some(FormattingContextType::Flex)
      ),
      "FlexFormattingContext must only layout flex containers",
    );
    let _profile = layout_timer(LayoutKind::Flex);
    if let Err(RenderError::Timeout { elapsed, .. }) = check_active(RenderStage::Layout) {
      return Err(LayoutError::Timeout { elapsed });
    }
    let build_timer = flex_profile::timer();
    let mut constraints = *constraints;
    let container_inline_base = constraints
      .inline_percentage_base
      .or_else(|| constraints.width());
    if matches!(constraints.available_width, CrateAvailableSpace::Indefinite) {
      let fallback = container_inline_base.unwrap_or(self.viewport_size.width);
      constraints.available_width = CrateAvailableSpace::Definite(fallback);
      if constraints.inline_percentage_base.is_none() {
        constraints.inline_percentage_base = container_inline_base.or(Some(fallback));
      }
    }
    // Keep block axis as provided; many flex containers legitimately size-to-content.

    let has_running_children = box_node
      .children
      .iter()
      .any(|child| child.style.running_position.is_some());
    if !has_running_children {
      if let Some(cached) = layout_cache_lookup(
        box_node,
        FormattingContextType::Flex,
        &constraints,
        self.viewport_size,
      ) {
        return Ok(cached);
      }
    }

    // Reuse full layout fragments when the same flex container is laid out repeatedly with
    // identical available sizes (common on carousel-heavy pages). This is scoped per layout
    // run via the factory cache reset.
    let toggles = crate::debug::runtime::runtime_toggles();
    let disable_cache = toggles.truthy("FASTR_DISABLE_FLEX_CACHE") || has_running_children;
    let layout_cache_entry = if disable_cache {
      None
    } else {
      layout_cache_key(&constraints, self.viewport_size).map(|k| (flex_cache_key(box_node), k))
    };

    let _trace_text_ids = trace_flex_text_ids();
    if let Some((cache_key, key)) = layout_cache_entry {
      if let Some((_, fragment)) = self.layout_fragments.get(cache_key, &key) {
        flex_profile::record_layout_cache_hit();
        record_fragment_clone(CloneSite::FlexLayoutCacheHit, fragment.as_ref());
        return Ok((*fragment).clone());
      }
      let target_w = constraints.width().unwrap_or(self.viewport_size.width);
      let target_h = constraints.height().unwrap_or(self.viewport_size.height);
      if let Some((stored_size, fragment)) = self
        .layout_fragments
        .find_fragment(cache_key, Size::new(target_w.max(0.0), target_h.max(0.0)))
      {
        self.layout_fragments.insert(
          cache_key,
          key,
          (stored_size, std::sync::Arc::clone(&fragment)),
          MAX_LAYOUT_CACHE_PER_NODE,
        );
        flex_profile::record_layout_cache_hit();
        record_fragment_clone(CloneSite::FlexLayoutCacheHit, fragment.as_ref());
        return Ok((*fragment).clone());
      }
    }

    // Create a fresh Taffy tree for this layout
    let mut taffy_tree: TaffyTree<*const BoxNode> = TaffyTree::new();

    // Partition children: out-of-flow abs/fixed are handled after flex layout per CSS positioning.
    let mut in_flow_children: Vec<(usize, &BoxNode)> = Vec::new();
    let mut positioned_children = Vec::new();
    let mut running_children: Vec<(usize, BoxNode)> = Vec::new();
    for (idx, child) in box_node.children.iter().enumerate() {
      if child.style.running_position.is_some() {
        // Running elements do not participate in flex layout; instead, capture a snapshot at the
        // position the element would have occupied in flow.
        running_children.push((idx, child.clone()));
        continue;
      }
      match child.style.position {
        crate::style::position::Position::Absolute | crate::style::position::Position::Fixed => {
          positioned_children.push(child.clone());
        }
        _ => in_flow_children.push((idx, child)),
      }
    }
    in_flow_children.sort_by(|(a_idx, a), (b_idx, b)| {
      a.style
        .order
        .cmp(&b.style.order)
        .then_with(|| a_idx.cmp(b_idx))
    });
    let in_flow_children: Vec<&BoxNode> = in_flow_children
      .into_iter()
      .map(|(_, child)| child)
      .collect();

    // Phase 1: Build Taffy tree from in-flow children
    let mut node_map: HashMap<*const BoxNode, NodeId> = HashMap::new();
    let root_node = self.build_taffy_tree_children(
      &mut taffy_tree,
      box_node,
      &in_flow_children,
      &mut node_map,
    )?;
    flex_profile::record_build_time(build_timer);

    // Block-level flex containers with `width:auto` fill the available inline space. Root flex
    // nodes have no parent for percentage resolution, so translate the definite available width
    // into an explicit Taffy size to ensure flex-shrink/grow runs against the correct line size.
    if box_node.style.width.is_none() && matches!(box_node.style.display, Display::Flex) {
      if let CrateAvailableSpace::Definite(w) = constraints.available_width {
        if let Ok(existing) = taffy_tree.style(root_node) {
          let mut updated = existing.clone();
          updated.size.width = Dimension::length(w.max(0.0));
          taffy_tree
            .set_style(root_node, updated)
            .map_err(|e| LayoutError::MissingContext(format!("Taffy error: {:?}", e)))?;
        }
      }
    }

    // Phase 2: Compute layout using Taffy
    let available_space = self.constraints_to_available_space(&constraints);
    let viewport_size = self.viewport_size;
    let nearest_positioned_cb = self.nearest_positioned_cb;
    let measured_fragments = self.measured_fragments.clone();
    let base_factory = self.child_factory();
    let factory = base_factory.clone();
    let flex_item_block_fc: Arc<dyn FormattingContext> = Arc::new(
      BlockFormattingContext::for_flex_item_with_font_context_viewport_and_cb(
        self.font_context.clone(),
        viewport_size,
        nearest_positioned_cb,
      )
      .with_parallelism(self.parallelism),
    );
    let this = self.clone();
    let mut pass_cache: HashMap<
      u64,
      HashMap<(Option<u32>, Option<u32>), (Size, std::sync::Arc<FragmentNode>)>,
    > = HashMap::new();
    let compute_timer = flex_profile::timer();
    let log_root = toggles.truthy("FASTR_LOG_FLEX_ROOT");
    if log_root {
      eprintln!(
        "[flex-root] id={} selector={} avail=({:?},{:?}) known=({:?},{:?}) viewport=({:.1},{:.1})",
        box_node.id,
        box_node
          .debug_info
          .as_ref()
          .map(|d| d.to_selector())
          .unwrap_or_else(|| "<anon>".to_string()),
        available_space.width,
        available_space.height,
        constraints.width(),
        constraints.height(),
        self.viewport_size.width,
        self.viewport_size.height,
      );
    }
    let log_constraint_raw = toggles
      .get("FASTR_LOG_FLEX_CONSTRAINTS")
      .map(|v| v.to_string());
    let log_constraint_ids = toggles
      .usize_list("FASTR_LOG_FLEX_CONSTRAINTS")
      .unwrap_or_default();
    let log_constraint_limit = toggles.usize_with_default("FASTR_LOG_FLEX_CONSTRAINTS_MAX", 10);
    let log_first_n = toggles.usize_with_default("FASTR_LOG_FLEX_FIRST_N", 0);
    let abort_after_first = toggles.truthy("FASTR_ABORT_FLEX_AFTER_FIRST_N");
    if log_constraint_raw.is_some() {
      eprintln!(
        "[flex-constraints-env] raw={:?} ids={:?} max={}",
        log_constraint_raw, log_constraint_ids, log_constraint_limit
      );
    }
    if log_first_n > 0 {
      eprintln!(
        "[flex-first-env] n={} abort={}",
        log_first_n, abort_after_first
      );
    }
    let log_skinny = toggles.truthy("FASTR_LOG_SKINNY_FLEX");
    let log_small_avail = toggles.truthy("FASTR_LOG_SMALL_FLEX");
    let log_measure_ids = toggles
      .usize_list("FASTR_LOG_FLEX_MEASURE_IDS")
      .unwrap_or_default();
    let log_node_keys = toggles
      .usize_list("FASTR_LOG_FLEX_NODE_KEYS")
      .unwrap_or_default();
    let log_node_keys_max = toggles.usize_with_default("FASTR_LOG_FLEX_NODE_KEYS_MAX", 10);
    let log_large_avail = toggles.f64("FASTR_LOG_LARGE_FLEX").map(|v| v as f32);
    static LOG_NODE_KEYS_COUNTS: std::sync::OnceLock<
      std::sync::Mutex<std::collections::HashMap<usize, usize>>,
    > = std::sync::OnceLock::new();
    static LOG_LARGE_AVAIL_COUNTS: std::sync::OnceLock<
      std::sync::Mutex<std::collections::HashMap<usize, usize>>,
    > = std::sync::OnceLock::new();
    static LOG_CONSTRAINT_COUNTS: std::sync::OnceLock<
      std::sync::Mutex<std::collections::HashMap<usize, usize>>,
    > = std::sync::OnceLock::new();
    static LOG_FIRST_N_COUNTER: std::sync::OnceLock<std::sync::Mutex<usize>> =
      std::sync::OnceLock::new();
    record_taffy_invocation(TaffyAdapterKind::Flex);
    let measure_toggles = toggles.clone();
    taffy_tree
            .compute_layout_with_measure(
                root_node,
                available_space,
                move |mut known_dimensions, mut avail, _node_id, node_context, _style| {
                    let toggles = measure_toggles.as_ref();
                    // Treat zero/near-zero definite sizes as absent to avoid pathological
                    // measurement probes when Taffy propagates a 0px available size. This
                    // aligns with constraints_from_taffy, which promotes tiny definites to
                    // Indefinite/MaxContent.
                    if let Some(w) = known_dimensions.width {
                        if w <= 1.0 && matches!(avail.width, AvailableSpace::Definite(v) if v <= 1.0) {
                            known_dimensions.width = None;
                            avail.width = AvailableSpace::MaxContent;
                        }
                    }
                    if let AvailableSpace::Definite(w) = avail.width {
                        if w <= 1.0 {
                            avail.width = AvailableSpace::MaxContent;
                        }
                    }
                    if let Some(h) = known_dimensions.height {
                        if h <= 1.0 && matches!(avail.height, AvailableSpace::Definite(v) if v <= 1.0) {
                            known_dimensions.height = None;
                            avail.height = AvailableSpace::MaxContent;
                        }
                    }
                    if let AvailableSpace::Definite(h) = avail.height {
                        if h <= 1.0 {
                            avail.height = AvailableSpace::MaxContent;
                        }
                    }

                    flex_profile::record_measure_lookup();
                    let measure_timer = flex_profile::timer();
                    if let Some(node_ptr) = node_context.as_ref().map(|p| **p) {
                        let box_node = unsafe { &*node_ptr };
                        if known_dimensions.width == Some(0.0)
                            && matches!(avail.width, AvailableSpace::Definite(0.0))
                            && box_node.style.width.is_none()
                        {
                            known_dimensions.width = None;
                        }
                        if known_dimensions.height == Some(0.0)
                            && matches!(avail.height, AvailableSpace::Definite(0.0))
                            && box_node.style.height.is_none()
                        {
                            known_dimensions.height = None;
                        }
                        if matches!(avail.width, AvailableSpace::Definite(v) if v == 0.0)
                            && known_dimensions.width.is_none()
                            && box_node.style.width.is_none()
                        {
                            avail.width = AvailableSpace::MaxContent;
                        }
                        if matches!(avail.height, AvailableSpace::Definite(v) if v == 0.0)
                            && known_dimensions.height.is_none()
                            && box_node.style.height.is_none()
                        {
                            avail.height = AvailableSpace::MaxContent;
                        }
                        if log_small_avail {
                            if let AvailableSpace::Definite(w) = avail.width {
                                if w > 0.0 && w <= 100.0 {
                                    let selector = box_node
                                        .debug_info
                                        .as_ref()
                                        .map(|d| d.to_selector())
                                        .unwrap_or_else(|| "<anon>".to_string());
                                    eprintln!(
                                        "[flex-avail-small] id={} selector={} known_w={:?} known_h={:?} avail_w={:?} avail_h={:?} width_decl={:?} min_w={:?} max_w={:?}",
                                        box_node.id,
                                        selector,
                                        known_dimensions.width,
                                        known_dimensions.height,
                                        avail.width,
                                        avail.height,
                                        box_node.style.width,
                                        box_node.style.min_width,
                                        box_node.style.max_width,
                                    );
                                }
                            }
                        }
                    }
                    let w_state = if known_dimensions.width.is_some() {
                        DimState::Known
                    } else if matches!(
                        avail.width,
                        AvailableSpace::Definite(_) | AvailableSpace::MinContent | AvailableSpace::MaxContent
                    ) {
                        DimState::Definite
                    } else {
                        DimState::Other
                    };
                    let h_state = if known_dimensions.height.is_some() {
                        DimState::Known
                    } else if matches!(
                        avail.height,
                        AvailableSpace::Definite(_) | AvailableSpace::MinContent | AvailableSpace::MaxContent
                    ) {
                        DimState::Definite
                    } else {
                        DimState::Other
                    };
                    flex_profile::record_measure_bucket(w_state, h_state);
                    let drop_available_height = known_dimensions.height.is_some()
                        || node_context
                            .as_ref()
                            .map(|ptr| {
                                let box_ptr: *const BoxNode = **ptr;
                                let box_node = unsafe { &*box_ptr };
                                !height_depends_on_available_height(&box_node.style)
                            })
                            .unwrap_or(false);
                    let key = measure_cache_key(&known_dimensions, &avail, viewport_size, drop_available_height);
                    let bucket = match (w_state, h_state) {
                        (DimState::Known, DimState::Known) => 0,
                        (DimState::Known, DimState::Definite) => 1,
                        (DimState::Known, DimState::Other) => 2,
                        (DimState::Definite, DimState::Known) => 3,
                        (DimState::Definite, DimState::Definite) => 4,
                        (DimState::Definite, DimState::Other) => 5,
                        (DimState::Other, DimState::Known) => 6,
                        (DimState::Other, DimState::Definite) => 7,
                        (DimState::Other, DimState::Other) => 8,
                    };
                    flex_profile::record_histogram(bucket, key);
                    let node_ptr = node_context.as_ref().map(|p| **p);
                    if let Some(ptr) = node_ptr {
                        let box_node = unsafe { &*ptr };
                        flex_profile::record_node_lookup(box_node.id, key);
                        if !log_node_keys.is_empty() && log_node_keys.contains(&box_node.id) {
                            let counts = LOG_NODE_KEYS_COUNTS
                                .get_or_init(|| std::sync::Mutex::new(std::collections::HashMap::new()));
                            if let Ok(mut guard) = counts.lock() {
                                let entry = guard.entry(box_node.id).or_insert(0);
                                if *entry < log_node_keys_max {
                                    *entry += 1;
                                    let selector = box_node
                                        .debug_info
                                        .as_ref()
                                        .map(|d| d.to_selector())
                                        .unwrap_or_else(|| "<anon>".to_string());
                                    eprintln!(
                                        "[flex-node-key] id={} selector={} lookup={} bucket={} known=({:?},{:?}) avail=({:?},{:?}) key=({:?},{:?})",
                                        box_node.id,
                                        selector,
                                        *entry,
                                        bucket,
                                        known_dimensions.width,
                                        known_dimensions.height,
                                        avail.width,
                                        avail.height,
                                        key.0,
                                        key.1
                                    );
                                    if *entry == log_node_keys_max {
                                        eprintln!(
                                            "[flex-node-key-cap] id={} selector={} cap_reached={}",
                                            box_node.id, selector, log_node_keys_max
                                        );
                                    }
                                }
                            }
                        }
                        if let Some(threshold) = log_large_avail {
                            let mut log = false;
                            if let AvailableSpace::Definite(w) = avail.width {
                                if w > threshold {
                                    log = true;
                                }
                            }
                            if let AvailableSpace::Definite(h) = avail.height {
                                if h > threshold {
                                    log = true;
                                }
                            }
                            if log {
                                let counts = LOG_LARGE_AVAIL_COUNTS
                                    .get_or_init(|| std::sync::Mutex::new(std::collections::HashMap::new()));
                                if let Ok(mut guard) = counts.lock() {
                                    let entry = guard.entry(box_node.id).or_insert(0);
                                    if *entry < 5 {
                                        *entry += 1;
                                        let selector = box_node
                                            .debug_info
                                            .as_ref()
                                            .map(|d| d.to_selector())
                                            .unwrap_or_else(|| "<anon>".to_string());
                                        eprintln!(
                                            "[flex-large-avail] id={} selector={} lookup={} known=({:?},{:?}) avail=({:?},{:?}) key=({:?},{:?}) threshold={}",
                                            box_node.id,
                                            selector,
                                            *entry,
                                            known_dimensions.width,
                                            known_dimensions.height,
                                            avail.width,
                                            avail.height,
                                            key.0,
                                        key.1,
                                        threshold
                                    );
                                }
                        }
                    }
                        let mut logged_first = false;
                        if log_first_n > 0 {
                            let counter = LOG_FIRST_N_COUNTER
                                .get_or_init(|| std::sync::Mutex::new(0));
                            if let Ok(mut guard) = counter.lock() {
                                if *guard < log_first_n {
                                    *guard += 1;
                                    logged_first = true;
                                    let selector = box_node
                                        .debug_info
                                        .as_ref()
                                        .map(|d| d.to_selector())
                                        .unwrap_or_else(|| "<anon>".to_string());
                                    eprintln!(
                                        "[flex-first] seq={} id={} selector={} known=({:?},{:?}) avail=({:?},{:?}) key=({:?},{:?})",
                                        *guard,
                                        box_node.id,
                                        selector,
                                        known_dimensions.width,
                                        known_dimensions.height,
                                        avail.width,
                                        avail.height,
                                        key.0,
                                        key.1
                                    );
                                    assert!(
                                        !(abort_after_first && *guard >= log_first_n),
                                        "[flex-first-abort] seq={}",
                                        *guard
                                    );
                                }
                            }
                        }
                        if logged_first || (!log_constraint_ids.is_empty() && log_constraint_ids.contains(&box_node.id)) {
                            let counts = LOG_CONSTRAINT_COUNTS
                                .get_or_init(|| std::sync::Mutex::new(std::collections::HashMap::new()));
                            if let Ok(mut guard) = counts.lock() {
                                let entry = guard.entry(box_node.id).or_insert(0);
                                if *entry < log_constraint_limit {
                                    *entry += 1;
                                    let selector = box_node
                                        .debug_info
                                        .as_ref()
                                        .map(|d| d.to_selector())
                                        .unwrap_or_else(|| "<anon>".to_string());
                                    eprintln!(
                                        "[flex-constraints] id={} selector={} lookup={} known=({:?},{:?}) avail=({:?},{:?}) key=({:?},{:?})",
                                        box_node.id,
                                        selector,
                                        *entry,
                                        known_dimensions.width,
                                        known_dimensions.height,
                                        avail.width,
                                        avail.height,
                                        key.0,
                                        key.1
                                    );
                                    if *entry == log_constraint_limit {
                                        eprintln!(
                                            "[flex-constraints-cap] id={} selector={} cap_reached={}",
                                            box_node.id, selector, log_constraint_limit
                                        );
                                    }
                                }
                            }
                        }
                    }
                    }
                    if let (Some(w), Some(h)) = (known_dimensions.width, known_dimensions.height) {
                        let size = taffy::geometry::Size { width: w, height: h };
                        flex_profile::record_measure_time(measure_timer);
                        return size;
                    }
                    static TRACE_COUNT: OnceLock<Mutex<usize>> = OnceLock::new();
                    static LOG_MEASURE_COUNTS: OnceLock<Mutex<HashMap<usize, usize>>> = OnceLock::new();
                    let trace_enabled = toggles.truthy("FASTR_TRACE_FLEX");
                    static LOG_MEASURE_FIRST_COUNT: OnceLock<Mutex<usize>> = OnceLock::new();
                    let log_measure_first =
                        toggles.usize_with_default("FASTR_LOG_FLEX_MEASURE_FIRST_N", 0);
                    let log_measure_first_above =
                        toggles.u128("FASTR_LOG_FLEX_MEASURE_FIRST_N_MS");

                    let fallback_size = |known: Option<f32>, avail_dim: AvailableSpace| {
                        known.unwrap_or(match avail_dim {
                            AvailableSpace::Definite(v) => v,
                            _ => 0.0,
                        })
                    };
                    let Some(box_ptr) = node_ptr else {
                        let size = taffy::geometry::Size {
                            width: fallback_size(known_dimensions.width, avail.width),
                            height: fallback_size(known_dimensions.height, avail.height),
                        };
                        flex_profile::record_measure_time(measure_timer);
                        return size;
                    };
                    let box_node = unsafe { &*box_ptr };
                    if log_measure_first > 0 {
                        if let Some(threshold) = log_measure_first_above {
                            let elapsed = measure_timer.map(|s| s.elapsed().as_millis()).unwrap_or(0);
                            if elapsed >= threshold {
                                let seq = {
                                    let mut count =
                                        LOG_MEASURE_FIRST_COUNT.get_or_init(|| Mutex::new(0)).lock().unwrap();
                                    (*count < log_measure_first).then(|| {
                                        *count += 1;
                                        *count
                                    })
                                };
                                if let Some(seq) = seq {
                                    let selector = box_node
                                        .debug_info
                                        .as_ref()
                                        .map(|d| d.to_selector())
                                        .unwrap_or_else(|| "<anon>".to_string());
                                    eprintln!(
                                        "[flex-measure-first] seq={} id={} selector={} elapsed_ms={} known=({:?},{:?}) avail=({:?},{:?})",
                                        seq,
                                        box_node.id,
                                        selector,
                                        elapsed,
                                        known_dimensions.width,
                                        known_dimensions.height,
                                        avail.width,
                                        avail.height,
                                    );
                                }
                            }
                        }
                    }
                    if trace_enabled {
                        let mut remaining = TRACE_COUNT.get_or_init(|| Mutex::new(50)).lock().unwrap();
                        if *remaining > 0 {
                            eprintln!(
                                "flex-trace node={:?} display={:?} known=({:?},{:?}) avail=({:?},{:?}) flex=({}, {}, {:?})",
                                box_node
                                    .debug_info
                                    .as_ref()
                                    .and_then(|d| d.tag_name.clone())
                                    .unwrap_or_else(|| "?".into()),
                                box_node.style.display,
                                known_dimensions.width,
                                known_dimensions.height,
                                avail.width,
                                avail.height,
                                box_node.style.flex_grow,
                                box_node.style.flex_shrink,
                                box_node.style.flex_basis
                            );
                            *remaining -= 1;
                        }
                    }

                    // When Taffy asks for the min-content contribution of a flex item, ignore
                    // author-specified widths so auto min-size falls back to the content-driven
                    // minimum (per CSS Flexbox §4.5). Keep min-width/max-width intact so explicit
                    // constraints still apply.
                    let mut cloned_style: Option<ComputedStyle> = None;
                    let mut alt_box: Option<BoxNode> = None;
                    // When the available inline size is intrinsic (min-/max-content), percentage
                    // widths/min/max can't be resolved (§10.5). Treat them as auto so intrinsic
                    // sizing uses content-driven sizes instead of forcing 100% of an unknown base.
                    let avail_is_intrinsic =
                        matches!(avail.width, AvailableSpace::MinContent | AvailableSpace::MaxContent);
                    if avail_is_intrinsic {
                        let style = cloned_style.get_or_insert_with(|| (*box_node.style).clone());
                        if matches!(style.width, Some(len) if len.unit.is_percentage()) {
                            style.width = None;
                        }
                        if matches!(style.min_width, Some(len) if len.unit.is_percentage()) {
                            style.min_width = None;
                        }
                        if matches!(style.max_width, Some(len) if len.unit.is_percentage()) {
                            style.max_width = None;
                        }
                    }
                    // Flexbox automatic minimum sizes use the min-content size suggestion, which is
                    // content-driven (specified sizes are handled separately by Taffy). When Taffy
                    // requests a min-content measurement, clear authored sizes on that axis so the
                    // formatting context can compute the content size suggestion instead of
                    // echoing a fixed `width`/`height`.
                    if matches!(avail.width, AvailableSpace::MinContent) && known_dimensions.width.is_none() {
                        let style = cloned_style.get_or_insert_with(|| (*box_node.style).clone());
                        style.width = None;
                        style.min_width = None;
                        style.max_width = None;
                    }
                    if matches!(avail.height, AvailableSpace::MinContent | AvailableSpace::MaxContent) {
                        let style = cloned_style.get_or_insert_with(|| (*box_node.style).clone());
                        if matches!(style.height, Some(len) if len.unit.is_percentage()) {
                            style.height = None;
                        }
                        if matches!(style.min_height, Some(len) if len.unit.is_percentage()) {
                            style.min_height = None;
                        }
                        if matches!(style.max_height, Some(len) if len.unit.is_percentage()) {
                            style.max_height = None;
                        }
                    }
                    if matches!(avail.height, AvailableSpace::MinContent) && known_dimensions.height.is_none() {
                        let style = cloned_style.get_or_insert_with(|| (*box_node.style).clone());
                        style.height = None;
                        style.min_height = None;
                        style.max_height = None;
                    }
                    if known_dimensions.width.is_none()
                        && matches!(avail.width, AvailableSpace::MinContent)
                        && matches!(box_node.style.flex_basis, crate::style::types::FlexBasis::Auto)
                        && matches!(box_node.style.width, Some(w) if !w.unit.is_absolute() && !w.unit.is_viewport_relative())
                    {
                        // For auto min-size with non-definite widths (percent/font-relative),
                        // remeasure without the authored width so the intrinsic content drives
                        // the min-content contribution. Keep definite lengths/viewport units
                        // intact so fixed/viewport-spanning items preserve their authored size.
                        let style = cloned_style.get_or_insert_with(|| (*box_node.style).clone());
                        style.width = None;
                    }
                    let measure_box: &BoxNode = if let Some(style) = cloned_style {
                        let mut cloned = box_node.clone();
                        cloned.style = Arc::new(style);
                        &*alt_box.insert(cloned)
                    } else {
                        box_node
                    };
                    let cache_key = flex_cache_key(measure_box);
                    if let Some((size, _)) = pass_cache.get(&cache_key).and_then(|m| m.get(&key)).cloned() {
                        record_node_measure_hit(measure_box.id);
                        flex_profile::record_measure_hit();
                        flex_profile::record_measure_bucket_hit(w_state, h_state);
                        flex_profile::record_measure_time(measure_timer);
                        return taffy::geometry::Size { width: size.width, height: size.height };
                    }
                    if let Some(entry) = pass_cache.get(&cache_key) {
                        let target_w = fallback_size(known_dimensions.width, avail.width);
                        let target_h = fallback_size(known_dimensions.height, avail.height);
                        if let Some((stored_size, frag)) =
                            find_layout_cache_fragment(entry, Size::new(target_w, target_h))
                        {
                            record_node_measure_hit(measure_box.id);
                            flex_profile::record_measure_hit();
                            flex_profile::record_measure_bucket_hit(w_state, h_state);
                            flex_profile::record_measure_time(measure_timer);
                            pass_cache
                                .entry(cache_key)
                                .or_default()
                                .entry(key)
                                .or_insert_with(|| (stored_size, std::sync::Arc::clone(&frag)));
                            return taffy::geometry::Size {
                                width: stored_size.width,
                                height: stored_size.height,
                            };
                        }
                    }
                    if let Some((size, frag)) = measured_fragments.get(cache_key, &key) {
                        pass_cache
                            .entry(cache_key)
                            .or_default()
                            .entry(key)
                            .or_insert_with(|| (size, std::sync::Arc::clone(&frag)));
                        record_node_measure_hit(measure_box.id);
                        flex_profile::record_measure_hit();
                        flex_profile::record_measure_time(measure_timer);
                        return taffy::geometry::Size { width: size.width, height: size.height };
                    }
                    let target_w = fallback_size(known_dimensions.width, avail.width);
                    let target_h = fallback_size(known_dimensions.height, avail.height);
                    if let Some((stored_size, frag)) =
                        measured_fragments.find_fragment(cache_key, Size::new(target_w, target_h))
                    {
                        record_node_measure_hit(measure_box.id);
                        flex_profile::record_measure_hit();
                        flex_profile::record_measure_time(measure_timer);
                        measured_fragments.insert(
                            cache_key,
                            key,
                            (stored_size, std::sync::Arc::clone(&frag)),
                            MAX_MEASURE_CACHE_PER_NODE,
                        );
                        pass_cache
                            .entry(cache_key)
                            .or_default()
                            .entry(key)
                            .or_insert_with(|| (stored_size, std::sync::Arc::clone(&frag)));
                        return taffy::geometry::Size {
                            width: stored_size.width,
                            height: stored_size.height,
                        };
                    }
                    let fc_type = measure_box.formatting_context().unwrap_or(FormattingContextType::Block);
                    // For auto flex-basis + auto width items, prefer intrinsic max-content sizing
                    // instead of forcing the container's definite width into the measurement
                    // constraints. This matches CSS Flexbox §4.5 (auto main size uses max-content).
                    let mut avail = avail;
                    if known_dimensions.width.is_none()
                        && matches!(avail.width, AvailableSpace::Definite(_))
                        && matches!(measure_box.style.flex_basis, crate::style::types::FlexBasis::Auto)
                        && measure_box.style.width.is_none()
                    {
                        avail.width = AvailableSpace::MaxContent;
                    }
                    if !log_measure_ids.is_empty() && log_measure_ids.contains(&measure_box.id) {
                        let seq = LOG_MEASURE_COUNTS
                            .get_or_init(|| Mutex::new(HashMap::new()))
                            .lock()
                            .ok()
                            .and_then(|mut counts| {
                                let entry = counts.entry(measure_box.id).or_insert(0);
                                (*entry < 3).then(|| {
                                    *entry += 1;
                                    *entry
                                })
                            });
                        if let Some(seq) = seq {
                            let selector = measure_box
                                .debug_info
                                .as_ref()
                                .map(|d| d.to_selector())
                                .unwrap_or_else(|| "<anon>".to_string());
                            eprintln!(
                                "[flex-measure] seq={} id={} selector={} display={:?} basis={:?} width_decl={:?} avail_w={:?} known_w={:?} avail_after={:?}",
                                seq,
                                measure_box.id,
                                selector,
                                measure_box.style.display,
                                measure_box.style.flex_basis,
                                measure_box.style.width,
                                avail.width,
                                known_dimensions.width,
                                avail.width,
                            );
                        }
                    }
                    let constraints = this.constraints_from_taffy(known_dimensions, avail, None);
                    if log_small_avail {
                        if let CrateAvailableSpace::Definite(w) = constraints.available_width {
                            if w > 0.0 && w <= 100.0 {
                                let selector = measure_box
                                    .debug_info
                                    .as_ref()
                                    .map(|d| d.to_selector())
                                    .unwrap_or_else(|| "<anon>".to_string());
                                eprintln!(
                                    "[flex-avail-small] id={} selector={} known_w={:?} known_h={:?} avail_w={:?} avail_h={:?} constraint_w={:?} constraint_h={:?} width_decl={:?} min_w={:?} max_w={:?} fc={:?}",
                                    measure_box.id,
                                    selector,
                                    known_dimensions.width,
                                    known_dimensions.height,
                                    avail.width,
                                    avail.height,
                                    constraints.available_width,
                                    constraints.available_height,
                                    measure_box.style.width,
                                    measure_box.style.min_width,
                                    measure_box.style.max_width,
                                    fc_type,
                                );
                            }
                        }
                    }
                    if log_skinny {
                        let mut cw_log = None;
                        if let CrateAvailableSpace::Definite(w) = constraints.available_width {
                            if w <= 1.0 {
                                cw_log = Some(w);
                            }
                        }
                        if let Some(w) = cw_log {
                            let selector = measure_box
                                .debug_info
                                .as_ref()
                                .map(|d| d.to_selector())
                                .unwrap_or_else(|| "<anon>".to_string());
                            eprintln!(
                                "[skinny-flex-root-constraint] id={} selector={} known_w={:?} avail_w={:?} constraint_w={:.1} width_decl={:?}",
                                measure_box.id, selector, known_dimensions.width, avail.width, w, measure_box.style.width
                            );
                        }
                    }

                    // Replaced elements don't establish a formatting context; compute their
                    // intrinsic/used size directly to avoid block layout inflating widths.
                    if let crate::tree::box_tree::BoxType::Replaced(replaced_box) = &measure_box.box_type {
                        let avail_width = known_dimensions.width.or(match avail.width {
                            AvailableSpace::Definite(w) => Some(w),
                            _ => None,
                        }).unwrap_or(this.viewport_size.width);
                        let avail_height = known_dimensions.height.or(match avail.height {
                            AvailableSpace::Definite(h) => Some(h),
                            _ => None,
                        }).unwrap_or(this.viewport_size.height);
                        let percentage_base = Some(Size::new(avail_width, avail_height));
                        let size = crate::layout::utils::compute_replaced_size(
                            &measure_box.style,
                            replaced_box,
                            percentage_base,
                            this.viewport_size,
                        );
                        flex_profile::record_measure_time(measure_timer);
                        return taffy::geometry::Size {
                            width: size.width,
                            height: size.height,
                        };
                    }

                    let fc: Arc<dyn FormattingContext> = if matches!(fc_type, FormattingContextType::Block) {
                        flex_item_block_fc.clone()
                    } else {
                        factory.get(fc_type)
                    };

                    let intrinsic_inline_hint = if matches!(
                        constraints.available_width,
                        CrateAvailableSpace::MaxContent | CrateAvailableSpace::MinContent
                    ) {
                        fc.compute_intrinsic_inline_size(measure_box, IntrinsicSizingMode::MaxContent)
                            .ok()
                    } else {
                        None
                    };
                    let node_timer = flex_profile::node_timer();
                    let selector_for_profile = node_timer
                        .as_ref()
                        .and_then(|_| measure_box.debug_info.as_ref().map(|d| d.to_selector()));
                    let mut fragment = match fc.layout(measure_box, &constraints) {
                         Ok(f) => {
                             flex_profile::record_node_layout(
                                 measure_box.id,
                                selector_for_profile.as_deref(),
                                node_timer,
                            );
                            f
                        }
                        Err(_) => {
                            flex_profile::record_node_layout(
                                measure_box.id,
                                selector_for_profile.as_deref(),
                                node_timer,
                            );
                            let size = taffy::geometry::Size {
                                width: fallback_size(known_dimensions.width, avail.width),
                                height: fallback_size(known_dimensions.height, avail.height),
                            };
                            flex_profile::record_measure_time(measure_timer);
                            return size;
                        }
                    };

                    let percentage_base = match avail.width {
                        AvailableSpace::Definite(w) => w,
                        _ => constraints.width().unwrap_or_else(|| fragment.bounds.width()),
                    };
                    let mut content_size = this.content_box_size(&fragment, &box_node.style, percentage_base);
                    // Guard against zero-sized measurements when the fragment actually has content.
                    let intrinsic_size = Self::fragment_subtree_size(&fragment);
                    let eps = 0.01;
                    if content_size.width <= eps && intrinsic_size.width > eps {
                        content_size.width = intrinsic_size.width;
                    }
                    if content_size.height <= eps && intrinsic_size.height > eps {
                        content_size.height = intrinsic_size.height;
                    }
                    let descendant_span = Self::fragment_descendant_span(&fragment);
                    if matches!(
                        constraints.available_width,
                        CrateAvailableSpace::MaxContent | CrateAvailableSpace::MinContent
                    ) && measure_box.style.width.is_none()
                        && measure_box.style.min_width.is_none()
                        && measure_box.style.max_width.is_none()
                    {
                        if let Some(span) = descendant_span {
                            if span.width > eps
                                && content_size.width > span.width + 0.5
                                && span.width < this.viewport_size.width - eps
                            {
                                content_size.width = span.width;
                            }
                            if span.height > eps && content_size.height > span.height + 0.5 {
                                content_size.height = span.height;
                            }
                        }
                    }

                    // Respect author min/max sizing when available, and clamp runaway intrinsic
                    // sizes when Taffy requests max-content/min-content space without a definite
                    // constraint. This prevents flex items from ballooning to multi-thousand-pixel
                    // widths that then propagate through min-content sizing.
                    let percentage_base_w = match avail.width {
                        AvailableSpace::Definite(w) => Some(w),
                        _ => known_dimensions.width,
                    };
                    let percentage_base_h = match avail.height {
                        AvailableSpace::Definite(h) => Some(h),
                        _ => known_dimensions.height,
                    };
                    let resolve_if_base = |len: &Length, base: Option<f32>| {
                        base.map(|b| this.resolve_length_for_width(*len, b, &measure_box.style))
                    };
                    let resolved_max_w = measure_box.style.max_width.as_ref().and_then(|l| resolve_if_base(l, percentage_base_w));
                    let resolved_min_w = measure_box.style.min_width.as_ref().and_then(|l| resolve_if_base(l, percentage_base_w));
                    let resolved_max_h = measure_box.style.max_height.as_ref().and_then(|l| resolve_if_base(l, percentage_base_h));
                    let resolved_min_h = measure_box.style.min_height.as_ref().and_then(|l| resolve_if_base(l, percentage_base_h));
                    let mut max_w_bound = resolved_max_w.unwrap_or_else(|| match avail.width {
                        AvailableSpace::Definite(w) => w.min(this.viewport_size.width),
                        _ => this.viewport_size.width,
                    });
                    let min_w_bound = resolved_min_w.unwrap_or(0.0);
                    if max_w_bound < min_w_bound {
                        max_w_bound = min_w_bound;
                    }
                    content_size.width = crate::layout::utils::clamp_with_order(
                        content_size.width,
                        min_w_bound,
                        max_w_bound,
                    );

                    let mut max_h_bound = resolved_max_h.unwrap_or(match avail.height {
                        AvailableSpace::Definite(h) => h,
                        _ => this.viewport_size.height,
                    });
                    let min_h_bound = resolved_min_h.unwrap_or(0.0);
                    if max_h_bound < min_h_bound {
                        max_h_bound = min_h_bound;
                    }
                    content_size.height = crate::layout::utils::clamp_with_order(
                        content_size.height,
                        min_h_bound,
                        max_h_bound,
                    );

                    if log_skinny
                        && (content_size.width <= 1.0
                            || intrinsic_size.width <= 1.0
                            || matches!(avail.width, AvailableSpace::Definite(w) if w <= 1.0))
                    {
                        let selector = measure_box
                            .debug_info
                            .as_ref()
                            .map(|d| d.to_selector())
                            .unwrap_or_else(|| "<anon>".to_string());
                        eprintln!(
                            "[skinny-flex-measure] id={} selector={} known=({:?},{:?}) avail=({:?},{:?}) content=({:.2},{:.2}) intrinsic=({:.2},{:.2}) min=({:.2},{:.2}) max=({:.2},{:.2})",
                            measure_box.id,
                            selector,
                            known_dimensions.width,
                            known_dimensions.height,
                            avail.width,
                            avail.height,
                            content_size.width,
                            content_size.height,
                            intrinsic_size.width,
                            intrinsic_size.height,
                            min_w_bound,
                            min_h_bound,
                            max_w_bound,
                            max_h_bound,
                        );
                    }

                     if !log_measure_ids.is_empty() && log_measure_ids.contains(&measure_box.id) {
                         let seq = LOG_MEASURE_COUNTS
                             .get_or_init(|| Mutex::new(HashMap::new()))
                             .lock()
                            .ok()
                            .and_then(|mut counts| {
                                let entry = counts.entry(measure_box.id).or_insert(0);
                                (*entry < 3).then(|| {
                                    *entry += 1;
                                    *entry
                                })
                            });
                        if let Some(seq) = seq {
                            let selector = measure_box
                                .debug_info
                                .as_ref()
                                .map(|d| d.to_selector())
                                .unwrap_or_else(|| "<anon>".to_string());
                            eprintln!(
                                "[flex-measure-result] seq={} id={} selector={} avail=({:?},{:?}) known=({:?},{:?}) constraints=({:?},{:?}) content=({:.2},{:.2}) intrinsic=({:.2},{:.2}) min=({:.2},{:.2}) max=({:.2},{:.2}) inline_hint={:?}",
                                seq,
                                measure_box.id,
                                selector,
                                avail.width,
                                avail.height,
                                known_dimensions.width,
                                known_dimensions.height,
                                constraints.available_width,
                                constraints.available_height,
                                content_size.width,
                                content_size.height,
                                intrinsic_size.width,
                                intrinsic_size.height,
                                min_w_bound,
                                min_h_bound,
                                max_w_bound,
                                max_h_bound,
                                intrinsic_inline_hint,
                             );
                         }
                     }

                    let stored_size =
                      Size::new(content_size.width.max(0.0), content_size.height.max(0.0));
                    let pass_entry_vacant = node_ptr.is_some()
                      && !pass_cache
                        .get(&cache_key)
                        .map(|entry| entry.contains_key(&key))
                        .unwrap_or(false);
                    let mut fragment = Some(fragment);
                    let mut normalized_fragment: Option<std::sync::Arc<FragmentNode>> = None;
                    let mut normalize_for_cache = |fragment: &mut Option<FragmentNode>,
                                                   normalized_fragment: &mut Option<
                        std::sync::Arc<FragmentNode>,
                      >| {
                        normalized_fragment
                          .get_or_insert_with(|| {
                            let mut fragment = fragment.take().expect("fragment already normalized");
                            normalize_fragment_origin(&mut fragment);
                            std::sync::Arc::new(fragment)
                          })
                          .clone()
                      };

                    if measured_fragments.get(cache_key, &key).is_none() {
                      let normalized_fragment =
                        normalize_for_cache(&mut fragment, &mut normalized_fragment);
                      let inserted = measured_fragments.insert(
                        cache_key,
                        key,
                        (stored_size, normalized_fragment),
                        MAX_MEASURE_CACHE_PER_NODE,
                      );
                      flex_profile::record_measure_store(inserted);
                      if inserted {
                        record_node_measure_store(measure_box.id);
                      }
                    } else {
                      flex_profile::record_measure_store(false);
                    }
                    if pass_entry_vacant {
                      let entry = pass_cache.entry(cache_key).or_default();
                      if let Entry::Vacant(e) = entry.entry(key) {
                        let normalized_fragment =
                          normalize_for_cache(&mut fragment, &mut normalized_fragment);
                        e.insert((stored_size, normalized_fragment));
                        record_node_measure_store(measure_box.id);
                      }
                    }

                    let size = taffy::geometry::Size {
                        width: content_size.width.max(0.0),
                        height: content_size.height.max(0.0),
                    };
                    flex_profile::record_measure_time(measure_timer);
                    size
                },
            )
            .map_err(|e| LayoutError::MissingContext(format!("Taffy layout failed: {:?}", e)))?;
    flex_profile::record_compute_time(compute_timer);
    if toggles.truthy("FASTR_DEBUG_FLEX_CHILD") {
      if let Ok(style) = taffy_tree.style(root_node) {
        eprintln!(
          "[flex-taffy-root-style] size=({:?},{:?}) min=({:?},{:?}) max=({:?},{:?})",
          style.size.width,
          style.size.height,
          style.min_size.width,
          style.min_size.height,
          style.max_size.width,
          style.max_size.height,
        );
      }
      if let Ok(layout) = taffy_tree.layout(root_node) {
        eprintln!(
          "[flex-taffy-root-layout] size=({:.2},{:.2}) loc=({:.2},{:.2})",
          layout.size.width, layout.size.height, layout.location.x, layout.location.y,
        );
      }
    }

    // Phase 3: Convert Taffy layout back to FragmentNode
    let convert_timer = flex_profile::timer();
    let mut fragment = self.taffy_to_fragment(
      &taffy_tree,
      root_node,
      root_node,
      box_node,
      &node_map,
      &constraints,
    )?;
    // Respect align-items/align-self in the container's coordinate system (parent axes), even
    // when the child uses a different writing mode. Taffy resolves alignment in its own
    // axis space; here we remap the cross-axis position to match the parent's writing-mode.
    if matches!(box_node.style.display, Display::Flex | Display::InlineFlex)
      && !fragment.children.is_empty()
    {
      let inline_is_horizontal = matches!(box_node.style.writing_mode, WritingMode::HorizontalTb);
      let block_is_horizontal = !inline_is_horizontal;
      let main_is_inline = matches!(
        box_node.style.flex_direction,
        FlexDirection::Row | FlexDirection::RowReverse
      );
      let main_is_horizontal = if main_is_inline {
        inline_is_horizontal
      } else {
        block_is_horizontal
      };
      let cross_is_horizontal = if main_is_inline {
        block_is_horizontal
      } else {
        inline_is_horizontal
      };
      let inline_positive = self.inline_axis_positive(&box_node.style);
      let block_positive = self.block_axis_positive(&box_node.style);
      let cross_positive = if main_is_inline {
        block_positive
      } else {
        inline_positive
      };

      let cross_size = if cross_is_horizontal {
        fragment.bounds.width()
      } else {
        fragment.bounds.height()
      };

      let taffy_dir =
        self.flex_direction_to_taffy(&box_node.style, inline_positive, block_positive);
      let main_grows_positive = matches!(
        taffy_dir,
        taffy::style::FlexDirection::Row | taffy::style::FlexDirection::Column
      );

      let mut line_indices: Vec<usize> = Vec::with_capacity(fragment.children.len());
      if matches!(box_node.style.flex_wrap, FlexWrap::NoWrap) {
        line_indices.resize(fragment.children.len(), 0);
      } else {
        let mut current_line = 0usize;
        let mut prev_main: Option<f32> = None;
        let wrap_break_eps = 0.5;
        for child in fragment.children.iter() {
          let main_pos = if main_is_horizontal {
            child.bounds.x()
          } else {
            child.bounds.y()
          };
          if let Some(prev) = prev_main {
            let delta = main_pos - prev;
            if (main_grows_positive && delta < -wrap_break_eps)
              || (!main_grows_positive && delta > wrap_break_eps)
            {
              current_line += 1;
            }
          }
          line_indices.push(current_line);
          prev_main = Some(main_pos);
        }
      }

      #[derive(Clone, Copy, Default)]
      struct LineBaselineData {
        cross_start: f32,
        max_above: f32,
        max_below: f32,
        baseline: f32,
        has_baseline: bool,
      }

      #[derive(Clone, Copy)]
      struct BaselineItemMetrics {
        line_index: usize,
        baseline_pos: f32,
        baseline_offset: f32,
        cross_size: f32,
      }

      let mut baseline_items: Vec<Option<BaselineItemMetrics>> =
        vec![None; fragment.children.len()];
      let line_count = line_indices.iter().copied().max().unwrap_or(0) + 1;
      let mut line_cross_starts = vec![f32::INFINITY; line_count];

      for idx in 0..fragment.children.len() {
        let child_node = &in_flow_children[idx];
        let align = child_node
          .style
          .align_self
          .unwrap_or(box_node.style.align_items);

        if matches!(align, AlignItems::Baseline) {
          let child_fragment = &fragment.children[idx];
          let cross_start = if cross_is_horizontal {
            child_fragment.bounds.x()
          } else {
            child_fragment.bounds.y()
          };
          let cross_size_child = if cross_is_horizontal {
            child_fragment.bounds.width()
          } else {
            child_fragment.bounds.height()
          };
          let mut baseline_offset =
            fragment_first_baseline(child_fragment).unwrap_or(cross_size_child);
          if !baseline_offset.is_finite() {
            baseline_offset = cross_size_child;
          }
          let baseline_offset = baseline_offset.clamp(0.0, cross_size_child);
          let baseline_pos = cross_start + baseline_offset;
          let line_idx = *line_indices.get(idx).unwrap_or(&0);
          if line_idx >= line_cross_starts.len() {
            line_cross_starts.resize(line_idx + 1, f32::INFINITY);
          }
          line_cross_starts[line_idx] = line_cross_starts[line_idx].min(cross_start);
          baseline_items[idx] = Some(BaselineItemMetrics {
            line_index: line_idx,
            baseline_pos,
            baseline_offset,
            cross_size: cross_size_child,
          });
        }
      }

      let mut line_baselines = vec![LineBaselineData::default(); line_cross_starts.len()];
      for (idx, start) in line_cross_starts.iter().enumerate() {
        line_baselines[idx].cross_start = if start.is_finite() { *start } else { 0.0 };
      }

      for metrics in baseline_items.iter().flatten() {
        let line_idx = metrics.line_index;
        if line_idx >= line_baselines.len() {
          line_baselines.resize(line_idx + 1, LineBaselineData::default());
        }
        let line_start = line_baselines[line_idx].cross_start;
        let above = metrics.baseline_pos - line_start;
        let below = metrics.cross_size - metrics.baseline_offset;
        let line = &mut line_baselines[line_idx];
        line.max_above = line.max_above.max(above);
        line.max_below = line.max_below.max(below);
        line.has_baseline = true;
      }

      for line in line_baselines.iter_mut() {
        if line.has_baseline {
          line.cross_start = line.cross_start.max(0.0);
          line.baseline = line.cross_start + line.max_above;
        }
      }

      for (idx, (child_node, child_fragment)) in in_flow_children
        .iter()
        .zip(fragment.children_mut().iter_mut())
        .enumerate()
      {
        let align = child_node
          .style
          .align_self
          .unwrap_or(box_node.style.align_items);

        if let Some(metrics) = baseline_items[idx] {
          if let Some(line) = line_baselines.get(metrics.line_index) {
            if line.has_baseline {
              let delta = line.baseline - metrics.baseline_pos;
              if cross_is_horizontal {
                translate_fragment_tree(child_fragment, Point::new(delta, 0.0));
              } else {
                translate_fragment_tree(child_fragment, Point::new(0.0, delta));
              }
              continue;
            }
          }
        }

        let child_cross = if cross_is_horizontal {
          child_fragment.bounds.width()
        } else {
          child_fragment.bounds.height()
        };
        let pos = match align {
          AlignItems::Start | AlignItems::SelfStart | AlignItems::FlexStart => {
            if cross_positive {
              0.0
            } else {
              (cross_size - child_cross).max(0.0)
            }
          }
          AlignItems::End | AlignItems::SelfEnd | AlignItems::FlexEnd => {
            if cross_positive {
              (cross_size - child_cross).max(0.0)
            } else {
              0.0
            }
          }
          AlignItems::Center => (cross_size - child_cross) / 2.0,
          AlignItems::Stretch => 0.0,
          AlignItems::Baseline => {
            if cross_positive {
              0.0
            } else {
              (cross_size - child_cross).max(0.0)
            }
          }
        };

        if cross_is_horizontal {
          child_fragment.bounds.origin.x = pos;
        } else {
          child_fragment.bounds.origin.y = pos;
        }
      }
      if toggles.truthy("FASTR_DEBUG_FLEX_CHILD") {
        eprintln!(
          "[flex-container] bounds=({:.2},{:.2},{:.2},{:.2})",
          fragment.bounds.x(),
          fragment.bounds.y(),
          fragment.bounds.width(),
          fragment.bounds.height()
        );
        for (idx, child) in fragment.children.iter().enumerate() {
          eprintln!(
            "[flex-child-after-align] idx={} bounds=({:.2},{:.2},{:.2},{:.2})",
            idx,
            child.bounds.x(),
            child.bounds.y(),
            child.bounds.width(),
            child.bounds.height()
          );
        }
      }
    }
    // Clamp the container width to the definite available width when provided. A block-level
    // flex container with auto width should not expand to its max-content size; it fills the
    // containing block width. This prevents oversized flex containers from ballooning when
    // child intrinsic sizes are large.
    if let CrateAvailableSpace::Definite(w) = constraints.available_width {
      let clamped_w = w.min(self.viewport_size.width);
      fragment.bounds = Rect::new(
        fragment.bounds.origin,
        Size::new(clamped_w, fragment.bounds.height()),
      );
    } else if fragment.bounds.width() > self.viewport_size.width {
      // When width is indefinite, default to filling the viewport rather than expanding to
      // the max-content of children (which can explode with wide carousels). Block-level
      // flex containers with auto width should behave as width: auto in normal flow, i.e.
      // fill the containing block (viewport at root).
      fragment.bounds = Rect::new(
        fragment.bounds.origin,
        Size::new(self.viewport_size.width, fragment.bounds.height()),
      );
    }
    // Keep child layout positions intact even when they overflow the container; overflow handling
    // is a paint concern (via overflow clipping). Only sanitize clearly invalid or runaway values.
    if fragment.bounds.width().is_finite() && fragment.bounds.height().is_finite() {
      let max_w = fragment.bounds.width().max(0.0);
      let max_h = fragment.bounds.height().max(0.0);
      let runaway_x = max_w.max(1.0) * 20.0;
      let runaway_y = max_h.max(1.0) * 20.0;
      for child in fragment.children_mut() {
        let mut x = child.bounds.x();
        let mut y = child.bounds.y();
        let mut w = child.bounds.width();
        let mut h = child.bounds.height();
        let mut changed = false;

        if !x.is_finite() {
          x = 0.0;
          changed = true;
        }
        if !y.is_finite() {
          y = 0.0;
          changed = true;
        }
        if !w.is_finite() || w < 0.0 {
          w = 0.0;
          changed = true;
        }
        if !h.is_finite() || h < 0.0 {
          h = 0.0;
          changed = true;
        }

        let max_x = x + w;
        let max_y = y + h;
        if x.abs() > runaway_x || max_x.abs() > runaway_x {
          w = w.min(max_w);
          x = x.clamp(0.0, (max_w - w).max(0.0));
          changed = true;
        }
        if y.abs() > runaway_y || max_y.abs() > runaway_y {
          h = h.min(max_h);
          y = y.clamp(0.0, (max_h - h).max(0.0));
          changed = true;
        }

        if changed {
          child.bounds = Rect::new(Point::new(x, y), Size::new(w, h));
        }
      }
    }

    let log_wide = toggles.truthy("FASTR_LOG_WIDE_FLEX");
    let log_skinny_frag = toggles.truthy("FASTR_LOG_SKINNY_FLEX");
    let log_target_ids = toggles.usize_list("FASTR_LOG_FLEX_IDS").unwrap_or_default();
    if log_wide || log_skinny_frag || !log_target_ids.is_empty() {
      let avail_w = constraints.width();
      if fragment.bounds.width() > self.viewport_size.width + 0.5
        || avail_w
          .map(|w| w > self.viewport_size.width + 0.5)
          .unwrap_or(false)
      {
        let selector = box_node
          .debug_info
          .as_ref()
          .map(|d| d.to_selector())
          .unwrap_or_else(|| "<anon>".to_string());
        let child_ids: Vec<usize> = box_node.children.iter().take(5).map(|c| c.id).collect();
        let style = &box_node.style;
        eprintln!(
                    "[flex-wide] box_id={:?} tag={:?} selector={} avail_w={:?} avail_h={:?} frag_w={:.1} frag_h={:.1} viewport_w={:.1} display={:?} width={:?} min_w={:?} max_w={:?} margins=({:.1},{:.1}) children_first5={:?}",
                    box_node.id,
                    box_node
                        .debug_info
                        .as_ref()
                        .and_then(|d| d.tag_name.clone())
                        .unwrap_or_default(),
                    selector,
                    avail_w,
                    constraints.height(),
                    fragment.bounds.width(),
                    fragment.bounds.height(),
                    self.viewport_size.width,
                    style.display,
                    style.width,
                    style.min_width,
                    style.max_width,
                    style.margin_left.map(|l| l.to_px()).unwrap_or(0.0),
                    style.margin_right.map(|l| l.to_px()).unwrap_or(0.0),
                    child_ids,
                );
        if log_target_ids.contains(&box_node.id) {
          eprintln!(
                        "[flex-target] id={} selector={} avail_w={:?} avail_h={:?} frag_w={:.1} frag_h={:.1} bounds=({:.1},{:.1}) display={:?} width={:?} min_w={:?} max_w={:?} margins=({:.1},{:.1})",
                        box_node.id,
                        selector,
                        constraints.width(),
                        constraints.height(),
                        fragment.bounds.width(),
                        fragment.bounds.height(),
                        fragment.bounds.x(),
                        fragment.bounds.y(),
                        box_node.style.display,
                        box_node.style.width,
                        box_node.style.min_width,
                        box_node.style.max_width,
                        box_node.style.margin_left.map(|l| l.to_px()).unwrap_or(0.0),
                        box_node.style.margin_right.map(|l| l.to_px()).unwrap_or(0.0),
                    );
        }
      }
      if log_skinny_frag && fragment.bounds.width() <= 1.0 {
        eprintln!(
                    "[skinny-flex-frag] box_id={:?} tag={:?} avail_w={:?} avail_h={:?} frag_w={:.2} frag_h={:.2} display={:?}",
                    box_node.id,
                    box_node
                        .debug_info
                        .as_ref()
                        .and_then(|d| d.tag_name.clone())
                        .unwrap_or_default(),
                    avail_w,
                    constraints.height(),
                    fragment.bounds.width(),
                    fragment.bounds.height(),
                    box_node.style.display
                );
      }
      if log_target_ids.contains(&box_node.id) {
        let selector = box_node
          .debug_info
          .as_ref()
          .map(|d| d.to_selector())
          .unwrap_or_else(|| "<anon>".to_string());
        eprintln!(
                    "[flex-target] id={} selector={} avail_w={:?} avail_h={:?} frag_w={:.1} frag_h={:.1} bounds=({:.1},{:.1}) display={:?} width={:?} min_w={:?} max_w={:?} margins=({:.1},{:.1})",
                    box_node.id,
                    selector,
                    constraints.width(),
                    constraints.height(),
                    fragment.bounds.width(),
                    fragment.bounds.height(),
                    fragment.bounds.x(),
                    fragment.bounds.y(),
                    box_node.style.display,
                    box_node.style.width,
                    box_node.style.min_width,
                    box_node.style.max_width,
                    box_node.style.margin_left.map(|l| l.to_px()).unwrap_or(0.0),
                    box_node.style.margin_right.map(|l| l.to_px()).unwrap_or(0.0),
                );
      }
    }
    flex_profile::record_convert_time(convert_timer);

    if !disable_cache {
      if let Some((cache_key, key)) = layout_cache_entry {
        let size = fragment.bounds.size;
        self.layout_fragments.insert(
          cache_key,
          key,
          (size, std::sync::Arc::new(fragment.clone())),
          MAX_LAYOUT_CACHE_PER_NODE,
        );
        flex_profile::record_layout_cache_store();
      }
    }

    // Phase 4: Position out-of-flow abs/fixed children against this flex container.
    if !positioned_children.is_empty() {
      let positioned_factory = base_factory.clone();
      let abs = AbsoluteLayout::with_font_context(self.font_context.clone());
      let padding_left = self.resolve_length_for_width(
        box_node.style.padding_left,
        constraints.width().unwrap_or(0.0),
        &box_node.style,
      );
      let padding_top = self.resolve_length_for_width(
        box_node.style.padding_top,
        constraints.width().unwrap_or(0.0),
        &box_node.style,
      );
      let border_left = self.resolve_length_for_width(
        box_node.style.border_left_width,
        constraints.width().unwrap_or(0.0),
        &box_node.style,
      );
      let border_top = self.resolve_length_for_width(
        box_node.style.border_top_width,
        constraints.width().unwrap_or(0.0),
        &box_node.style,
      );
      let border_right = self.resolve_length_for_width(
        box_node.style.border_right_width,
        constraints.width().unwrap_or(0.0),
        &box_node.style,
      );
      let border_bottom = self.resolve_length_for_width(
        box_node.style.border_bottom_width,
        constraints.width().unwrap_or(0.0),
        &box_node.style,
      );

      let padding_origin = Point::new(border_left + padding_left, border_top + padding_top);
      let padding_size = Size::new(
        fragment.bounds.width() - border_left - border_right,
        fragment.bounds.height() - border_top - border_bottom,
      );
      let padding_rect = Rect::new(padding_origin, padding_size);

      let block_base = if box_node.style.height.is_some() {
        Some(padding_rect.size.height)
      } else {
        None
      };
      let establishes_abs_cb = box_node.style.position.is_positioned()
        || !box_node.style.transform.is_empty()
        || box_node.style.perspective.is_some()
        || box_node.style.containment.layout
        || box_node.style.containment.paint;
      let establishes_fixed_cb = !box_node.style.transform.is_empty()
        || box_node.style.perspective.is_some()
        || box_node.style.containment.layout
        || box_node.style.containment.paint;
      let padding_cb = ContainingBlock::with_viewport_and_bases(
        padding_rect,
        self.viewport_size,
        Some(padding_rect.size.width),
        block_base,
      );
      let cb_for_absolute = if establishes_abs_cb {
        padding_cb
      } else {
        self.nearest_positioned_cb
      };

      let mut positioned_candidates: Vec<PositionedCandidate> = Vec::new();
      for child in positioned_children {
        let cb = match child.style.position {
          Position::Fixed => {
            if establishes_fixed_cb {
              padding_cb
            } else {
              ContainingBlock::viewport(self.viewport_size)
            }
          }
          Position::Absolute => cb_for_absolute,
          _ => cb_for_absolute,
        };

        // Layout child as static to obtain intrinsic size.
        let mut layout_child = child.clone();
        let mut style = (*layout_child.style).clone();
        style.position = crate::style::position::Position::Relative;
        style.top = None;
        style.right = None;
        style.bottom = None;
        style.left = None;
        // Keep a distinct style Arc so cache keys that hash the style fingerprint do not share
        // entries with the real positioned variant.
        layout_child.style = Arc::new(style);

        let fc_type = layout_child
          .formatting_context()
          .unwrap_or(crate::style::display::FormattingContextType::Block);
        let fc = positioned_factory.with_positioned_cb(cb).create(fc_type);
        let child_constraints = LayoutConstraints::new(
          CrateAvailableSpace::Definite(padding_rect.size.width),
          block_base
            .map(CrateAvailableSpace::Definite)
            .unwrap_or(CrateAvailableSpace::Indefinite),
        );
        let child_fragment = fc.layout(&layout_child, &child_constraints)?;

        let positioned_style =
          resolve_positioned_style(&child.style, &cb, self.viewport_size, &self.font_context);
        let needs_inline_intrinsics = positioned_style.width.is_auto()
          && (positioned_style.left.is_auto()
            || positioned_style.right.is_auto()
            || child.is_replaced());
        let needs_block_intrinsics = positioned_style.height.is_auto()
          && (positioned_style.top.is_auto() || positioned_style.bottom.is_auto());
        let preferred_min_inline = if needs_inline_intrinsics {
          fc.compute_intrinsic_inline_size(&layout_child, IntrinsicSizingMode::MinContent)
            .ok()
        } else {
          None
        };
        let preferred_inline = if needs_inline_intrinsics {
          fc.compute_intrinsic_inline_size(&layout_child, IntrinsicSizingMode::MaxContent)
            .ok()
        } else {
          None
        };
        let preferred_min_block = if needs_block_intrinsics {
          fc.compute_intrinsic_block_size(&layout_child, IntrinsicSizingMode::MinContent)
            .ok()
        } else {
          None
        };
        let preferred_block = if needs_block_intrinsics {
          fc.compute_intrinsic_block_size(&layout_child, IntrinsicSizingMode::MaxContent)
            .ok()
        } else {
          None
        };

        positioned_candidates.push(PositionedCandidate {
          child: child.clone(),
          layout_child,
          cb,
          fragment: child_fragment,
          positioned_style,
          preferred_min_inline,
          preferred_inline,
          preferred_min_block,
          preferred_block,
          is_replaced: child.is_replaced(),
        });
      }

      let static_positions = self
        .compute_static_positions_for_abs_children(
          box_node,
          &fragment,
          &in_flow_children,
          &positioned_candidates,
          padding_origin,
        )
        .unwrap_or_default();

      for candidate in positioned_candidates {
        let mut input = AbsoluteLayoutInput::new(
          candidate.positioned_style,
          candidate.fragment.bounds.size,
          static_positions
            .get(&candidate.child.id)
            .copied()
            .unwrap_or(Point::ZERO),
        );
        input.is_replaced = candidate.is_replaced;
        input.preferred_min_inline_size = candidate.preferred_min_inline;
        input.preferred_inline_size = candidate.preferred_inline;
        input.preferred_min_block_size = candidate.preferred_min_block;
        input.preferred_block_size = candidate.preferred_block;
        let result = abs.layout_absolute(&input, &candidate.cb)?;
        let mut child_fragment = candidate.fragment;
        let needs_relayout = (result.size.width - child_fragment.bounds.width()).abs() > 0.01
          || (result.size.height - child_fragment.bounds.height()).abs() > 0.01;
        if needs_relayout {
          let fc_type = candidate
            .layout_child
            .formatting_context()
            .unwrap_or(crate::style::display::FormattingContextType::Block);
          let fc = positioned_factory
            .with_positioned_cb(candidate.cb)
            .create(fc_type);
          let relayout_constraints = LayoutConstraints::new(
            CrateAvailableSpace::Definite(result.size.width),
            CrateAvailableSpace::Definite(result.size.height),
          );
          let mut relayout_child = candidate.layout_child.clone();
          let mut relayout_style = (*relayout_child.style).clone();
          relayout_style.width = Some(Length::px(result.size.width));
          relayout_style.height = Some(Length::px(result.size.height));
          relayout_child.style = Arc::new(relayout_style);
          child_fragment = fc.layout(&relayout_child, &relayout_constraints)?;
        }
        child_fragment.bounds = Rect::new(result.position, result.size);
        child_fragment.style = Some(candidate.child.style.clone());
        fragment.children_mut().push(child_fragment);
      }
    }

    if !running_children.is_empty() {
      let mut id_to_bounds: HashMap<usize, Rect> = HashMap::new();
      for child in fragment.children.iter() {
        let Some(box_id) = (match &child.content {
          FragmentContent::Block { box_id }
          | FragmentContent::Inline { box_id, .. }
          | FragmentContent::Text { box_id, .. }
          | FragmentContent::Replaced { box_id, .. } => *box_id,
          FragmentContent::Line { .. } | FragmentContent::RunningAnchor { .. } => None,
        }) else {
          continue;
        };
        id_to_bounds.entry(box_id).or_insert(child.bounds);
      }

      let snapshot_factory = base_factory.clone();
      for (order, (running_idx, running_child)) in running_children.into_iter().enumerate() {
        let Some(name) = running_child.style.running_position.clone() else {
          continue;
        };

        let mut anchor_x = 0.0f32;
        let mut anchor_y = 0.0f32;
        if let Some((_, next_child)) = box_node
          .children
          .iter()
          .enumerate()
          .filter(|(idx, child)| {
            *idx > running_idx
              && child.style.running_position.is_none()
              && !matches!(child.style.position, Position::Absolute | Position::Fixed)
          })
          .min_by_key(|(idx, _)| *idx)
        {
          if let Some(bounds) = id_to_bounds.get(&next_child.id) {
            anchor_x = bounds.x();
            anchor_y = bounds.y();
          }
        }

        let mut snapshot_node = running_child.clone();
        let mut snapshot_style = snapshot_node.style.as_ref().clone();
        snapshot_style.running_position = None;
        snapshot_style.position = Position::Static;
        snapshot_node.style = Arc::new(snapshot_style);

        let fc_type = snapshot_node
          .formatting_context()
          .unwrap_or(FormattingContextType::Block);
        let fc = snapshot_factory.create(fc_type);
        let snapshot_constraints = LayoutConstraints::new(
          CrateAvailableSpace::Definite(fragment.bounds.width()),
          CrateAvailableSpace::Indefinite,
        );
        if let Ok(snapshot_fragment) = fc.layout(&snapshot_node, &snapshot_constraints) {
          let anchor_bounds =
            Rect::from_xywh(anchor_x, anchor_y + (order as f32) * 1e-4, 0.0, 0.01);
          let mut anchor = FragmentNode::new_running_anchor(anchor_bounds, name, snapshot_fragment);
          anchor.style = Some(running_child.style.clone());
          fragment.children_mut().push(anchor);
        }
      }
    }

    if !disable_cache {
      layout_cache_store(
        box_node,
        FormattingContextType::Flex,
        &constraints,
        &fragment,
        self.viewport_size,
      );
    }

    Ok(fragment)
  }

  /// Computes intrinsic size by running Taffy with appropriate constraints
  fn compute_intrinsic_inline_size(
    &self,
    box_node: &BoxNode,
    mode: IntrinsicSizingMode,
  ) -> Result<f32, LayoutError> {
    count_flex_intrinsic_call();
    if let Some(cached) = intrinsic_cache_lookup(box_node, mode) {
      return Ok(cached);
    }

    let style = &box_node.style;
    if style.containment.isolates_inline_size() {
      let edges = self.horizontal_edges_px(style).unwrap_or(0.0);
      intrinsic_cache_store(box_node, mode, edges.max(0.0));
      return Ok(edges.max(0.0));
    }

    // Approximate intrinsic inline size from flex items per CSS flexbox intrinsic sizing rules:
    // - Row axis: sum of item min/max-content contributions
    // - Column axis: max of item contributions
    let factory = Arc::new(self.child_factory());
    let is_row_axis = matches!(
      style.flex_direction,
      FlexDirection::Row | FlexDirection::RowReverse
    );

    let compute_child_contribution = |child: &BoxNode| -> Result<Option<f32>, LayoutError> {
      if matches!(child.style.position, Position::Absolute | Position::Fixed) {
        return Ok(None);
      }

      let fc_type = child
        .formatting_context()
        .unwrap_or(FormattingContextType::Block);
      let fc = factory.create(fc_type);
      let child_inline = fc.compute_intrinsic_inline_size(child, mode)?;

      // Include horizontal margins when they resolve without a containing block.
      let style = &child.style;
      let margin_left = style
        .margin_left
        .as_ref()
        .map(|l| self.resolve_length_for_width(*l, 0.0, style))
        .unwrap_or(0.0);
      let margin_right = style
        .margin_right
        .as_ref()
        .map(|l| self.resolve_length_for_width(*l, 0.0, style))
        .unwrap_or(0.0);
      let child_total = child_inline + margin_left + margin_right;
      Ok(Some(child_total))
    };

    let contributions = if self.parallelism.should_parallelize(box_node.children.len()) {
      let deadline = active_deadline();
      box_node
        .children
        .par_iter()
        .map(|child| {
          with_deadline(deadline.as_ref(), || {
            crate::layout::engine::debug_record_parallel_work();
            compute_child_contribution(child)
          })
        })
        .collect::<Result<Vec<_>, _>>()?
    } else {
      box_node
        .children
        .iter()
        .map(compute_child_contribution)
        .collect::<Result<Vec<_>, _>>()?
    };

    let mut contribution = 0.0f32;
    for child_total in contributions.into_iter().flatten() {
      if is_row_axis {
        contribution += child_total;
      } else {
        contribution = contribution.max(child_total);
      }
    }

    let edges = self.horizontal_edges_px(style).unwrap_or(0.0);
    let width = (contribution + edges).max(0.0);
    intrinsic_cache_store(box_node, mode, width);
    Ok(width)
  }
}

fn height_depends_on_available_height(style: &ComputedStyle) -> bool {
  let height_depends = style.height.as_ref().is_some_and(Length::has_percentage);
  let min_depends = style
    .min_height
    .as_ref()
    .is_some_and(Length::has_percentage);
  let max_depends = style
    .max_height
    .as_ref()
    .is_some_and(Length::has_percentage);
  let flex_basis_depends =
    matches!(style.flex_basis, FlexBasis::Length(len) if len.has_percentage());

  height_depends || min_depends || max_depends || flex_basis_depends
}

fn measure_cache_key(
  known: &taffy::geometry::Size<Option<f32>>,
  avail: &taffy::geometry::Size<AvailableSpace>,
  viewport: Size,
  drop_available_height: bool,
) -> (Option<u32>, Option<u32>) {
  fn quantize(val: f32) -> f32 {
    // Quantize measure keys to merge near-duplicate probes without visibly affecting layout.
    // Use progressively coarser steps as sizes grow to curb key cardinality on large pages
    // while keeping typical sizes precise. Thresholds favor tighter precision for small
    // items while aggressively merging large, near-identical probes common on wide carousels.
    let abs = val.abs();
    let step = if abs > 4096.0 {
      64.0
    } else if abs > 2048.0 {
      32.0
    } else if abs > 1024.0 {
      16.0
    } else if abs > 512.0 {
      8.0
    } else if abs > 256.0 {
      4.0
    } else {
      2.0
    };
    (val / step).round() * step
  }

  fn normalize_available(space: AvailableSpace) -> AvailableSpace {
    match space {
      AvailableSpace::Definite(w) if w <= 1.0 => AvailableSpace::MaxContent,
      AvailableSpace::Definite(w) => AvailableSpace::Definite(quantize(w)),
      other => other,
    }
  }

  let mut known = known.clone();
  let avail = taffy::geometry::Size {
    width: normalize_available(avail.width),
    height: normalize_available(avail.height),
  };
  if let Some(w) = known.width {
    if w <= 1.0 && matches!(avail.width, AvailableSpace::MaxContent) {
      known.width = None;
    }
  }
  if let Some(h) = known.height {
    if h <= 1.0 && matches!(avail.height, AvailableSpace::MaxContent) {
      known.height = None;
    }
  }

  let width_is_intrinsic = known.width.is_none()
    && matches!(
      avail.width,
      AvailableSpace::MinContent | AvailableSpace::MaxContent
    );

  let width = if let Some(w) = known.width {
    Some(quantize(w))
  } else {
    match avail.width {
      AvailableSpace::Definite(w) => Some(quantize(w)),
      AvailableSpace::MinContent => Some(quantize(-viewport.width.max(0.0) - 1.0)),
      AvailableSpace::MaxContent => Some(quantize(-viewport.width.max(0.0) - 2.0)),
    }
  };
  let ignore_height = drop_available_height || width_is_intrinsic;
  let height = if let Some(h) = known.height {
    Some(quantize(h))
  } else if ignore_height {
    // Ignore available block-size differences when the measurement does not depend on the
    // containing block height (or when probing intrinsic inline sizes).
    None
  } else {
    match avail.height {
      AvailableSpace::Definite(h) => Some(quantize(h)),
      AvailableSpace::MinContent => Some(quantize(-viewport.height.max(0.0) - 1.0)),
      AvailableSpace::MaxContent => Some(quantize(-viewport.height.max(0.0) - 2.0)),
    }
  };

  (width.map(f32::to_bits), height.map(f32::to_bits))
}

fn hash_enum_discriminant<T>(value: &T, hasher: &mut DefaultHasher) {
  mem::discriminant(value).hash(hasher);
}

fn hash_length(len: &Length, hasher: &mut DefaultHasher) {
  hash_enum_discriminant(&len.unit, hasher);
  len.value.to_bits().hash(hasher);
  // Treat calc lengths as distinct from raw by hashing a marker.
  if len.calc.is_some() {
    1u8.hash(hasher);
  } else {
    0u8.hash(hasher);
  }
}

fn hash_option_length(len: &Option<Length>, hasher: &mut DefaultHasher) {
  match len {
    Some(l) => {
      1u8.hash(hasher);
      hash_length(l, hasher);
    }
    None => 0u8.hash(hasher),
  }
}

fn hash_flex_basis(basis: &crate::style::types::FlexBasis, hasher: &mut DefaultHasher) {
  match basis {
    crate::style::types::FlexBasis::Auto => 0u8.hash(hasher),
    crate::style::types::FlexBasis::Length(l) => {
      1u8.hash(hasher);
      hash_length(l, hasher);
    }
  }
}

fn flex_style_fingerprint(style: &ComputedStyle) -> u64 {
  let mut h = DefaultHasher::new();
  hash_enum_discriminant(&style.display, &mut h);
  hash_enum_discriminant(&style.position, &mut h);
  hash_enum_discriminant(&style.box_sizing, &mut h);
  hash_option_length(&style.width, &mut h);
  hash_option_length(&style.height, &mut h);
  hash_option_length(&style.min_width, &mut h);
  hash_option_length(&style.max_width, &mut h);
  hash_option_length(&style.min_height, &mut h);
  hash_option_length(&style.max_height, &mut h);
  hash_option_length(&style.margin_top, &mut h);
  hash_option_length(&style.margin_right, &mut h);
  hash_option_length(&style.margin_bottom, &mut h);
  hash_option_length(&style.margin_left, &mut h);
  hash_length(&style.padding_top, &mut h);
  hash_length(&style.padding_right, &mut h);
  hash_length(&style.padding_bottom, &mut h);
  hash_length(&style.padding_left, &mut h);
  hash_length(&style.border_top_width, &mut h);
  hash_length(&style.border_right_width, &mut h);
  hash_length(&style.border_bottom_width, &mut h);
  hash_length(&style.border_left_width, &mut h);
  hash_enum_discriminant(&style.overflow_x, &mut h);
  hash_enum_discriminant(&style.overflow_y, &mut h);
  hash_enum_discriminant(&style.scrollbar_width, &mut h);
  hash_enum_discriminant(&style.flex_direction, &mut h);
  hash_enum_discriminant(&style.flex_wrap, &mut h);
  hash_enum_discriminant(&style.justify_content, &mut h);
  hash_enum_discriminant(&style.align_items, &mut h);
  hash_enum_discriminant(&style.align_content, &mut h);
  match style.align_self {
    Some(v) => {
      1u8.hash(&mut h);
      hash_enum_discriminant(&v, &mut h);
    }
    None => 0u8.hash(&mut h),
  }
  hash_enum_discriminant(&style.justify_items, &mut h);
  match style.justify_self {
    Some(v) => {
      1u8.hash(&mut h);
      hash_enum_discriminant(&v, &mut h);
    }
    None => 0u8.hash(&mut h),
  }
  style.flex_grow.to_bits().hash(&mut h);
  style.flex_shrink.to_bits().hash(&mut h);
  hash_flex_basis(&style.flex_basis, &mut h);
  hash_enum_discriminant(&style.aspect_ratio, &mut h);
  // Intrinsic/text sizing influences: include font size/line height basics.
  style.font_size.to_bits().hash(&mut h);
  style.root_font_size.to_bits().hash(&mut h);
  hash_enum_discriminant(&style.line_height, &mut h);
  h.finish()
}

fn flex_cache_key(box_node: &BoxNode) -> u64 {
  let mut h = DefaultHasher::new();
  box_node.styled_node_id.hash(&mut h);
  // Anonymous boxes (no originating styled node) must not share cached fragments across
  // different instances: their descendants may differ (e.g. different `<img src>`), and
  // reusing would clone the wrong subtree.
  if box_node.styled_node_id.is_none() {
    box_node.id.hash(&mut h);
  }
  let fingerprint = flex_style_fingerprint(&box_node.style);
  fingerprint.hash(&mut h);
  // Incorporate a simplified key for common carousel templates to merge otherwise identical
  // repeated items without formatting selectors (which allocates).
  if let Some(debug) = &box_node.debug_info {
    debug.hash_components(&mut h);
  }
  h.finish()
}

fn layout_cache_key(
  constraints: &LayoutConstraints,
  viewport: Size,
) -> Option<(Option<u32>, Option<u32>)> {
  fn quantize(val: f32) -> f32 {
    let abs = val.abs();
    let step = if abs > 4096.0 {
      64.0
    } else if abs > 2048.0 {
      32.0
    } else if abs > 1024.0 {
      16.0
    } else if abs > 512.0 {
      8.0
    } else if abs > 256.0 {
      4.0
    } else {
      2.0
    };
    (val / step).round() * step
  }

  let map_space = |space: CrateAvailableSpace, vp: f32, neg_offset: f32| -> Option<u32> {
    match space {
      CrateAvailableSpace::Definite(v) => Some(quantize(v).to_bits()),
      CrateAvailableSpace::MinContent => Some(quantize(-vp - neg_offset).to_bits()),
      CrateAvailableSpace::MaxContent => Some(quantize(-vp - (neg_offset + 1.0)).to_bits()),
      CrateAvailableSpace::Indefinite => None,
    }
  };

  let w = map_space(constraints.available_width, viewport.width.max(0.0), 1.0);
  let h = map_space(constraints.available_height, viewport.height.max(0.0), 1.0);
  Some((w, h))
}

impl FlexFormattingContext {
  /// Builds a Taffy tree from a BoxNode tree
  ///
  /// Returns the root NodeId and populates the node_map for later lookups.
  #[allow(dead_code)]
  fn build_taffy_tree(
    &self,
    taffy_tree: &mut TaffyTree<*const BoxNode>,
    box_node: &BoxNode,
    node_map: &mut HashMap<*const BoxNode, NodeId>,
  ) -> Result<NodeId, LayoutError> {
    self.build_taffy_tree_inner(taffy_tree, box_node, node_map, true, None, None)
  }

  /// Builds a Taffy tree from a BoxNode tree using an explicit set of root children
  /// (used to exclude out-of-flow children).
  fn build_taffy_tree_children(
    &self,
    taffy_tree: &mut TaffyTree<*const BoxNode>,
    box_node: &BoxNode,
    root_children: &[&BoxNode],
    node_map: &mut HashMap<*const BoxNode, NodeId>,
  ) -> Result<NodeId, LayoutError> {
    self.build_taffy_tree_inner(
      taffy_tree,
      box_node,
      node_map,
      true,
      None,
      Some(root_children),
    )
  }

  /// Internal tree builder that tracks whether we're at the root
  fn build_taffy_tree_inner(
    &self,
    taffy_tree: &mut TaffyTree<*const BoxNode>,
    box_node: &BoxNode,
    node_map: &mut HashMap<*const BoxNode, NodeId>,
    is_root: bool,
    containing_flex: Option<&ComputedStyle>,
    root_children: Option<&[&BoxNode]>,
  ) -> Result<NodeId, LayoutError> {
    // Convert style to Taffy style
    let taffy_style = self.computed_style_to_taffy(box_node, is_root, containing_flex);

    // Create Taffy node
    let children_iter: Vec<&BoxNode> = if is_root {
      root_children
        .map(|c| c.to_vec())
        .unwrap_or_else(|| box_node.children.iter().collect())
    } else {
      Vec::new()
    };

    let taffy_node = if is_root {
      let mut taffy_children = Vec::with_capacity(children_iter.len());
      for child in children_iter {
        let next_containing_flex =
          if is_root || matches!(box_node.style.display, Display::Flex | Display::InlineFlex) {
            Some(&box_node.style)
          } else {
            None
          };
        let child_node = self.build_taffy_tree_inner(
          taffy_tree,
          child,
          node_map,
          false,
          next_containing_flex.map(|s| &**s),
          None,
        )?;
        taffy_children.push(child_node);
      }

      if taffy_children.is_empty() {
        taffy_tree.new_leaf(taffy_style).map_err(|e| {
          LayoutError::MissingContext(format!("Failed to create Taffy leaf: {:?}", e))
        })?
      } else {
        taffy_tree
          .new_with_children(taffy_style, &taffy_children)
          .map_err(|e| {
            LayoutError::MissingContext(format!("Failed to create Taffy node: {:?}", e))
          })?
      }
    } else {
      taffy_tree
        .new_leaf_with_context(taffy_style, box_node as *const BoxNode)
        .map_err(|e| LayoutError::MissingContext(format!("Failed to create Taffy leaf: {:?}", e)))?
    };

    // Record mapping for later fragment conversion
    node_map.insert(box_node as *const BoxNode, taffy_node);

    Ok(taffy_node)
  }

  /// Converts our ComputedStyle to Taffy's Style
  ///
  /// The `is_root` flag indicates if this is the root flex container.
  /// For the root, we use Flex display; for children, we use Block.
  fn computed_style_to_taffy(
    &self,
    box_node: &BoxNode,
    is_root: bool,
    containing_flex: Option<&ComputedStyle>,
  ) -> taffy::style::Style {
    let style = &box_node.style;
    let inline_positive_container = self.inline_axis_positive(style);
    let block_positive_container = self.block_axis_positive(style);
    let inline_is_horizontal_container = matches!(style.writing_mode, WritingMode::HorizontalTb);
    let main_is_inline = matches!(
      style.flex_direction,
      FlexDirection::Row | FlexDirection::RowReverse
    );
    let cross_positive_container = if main_is_inline {
      block_positive_container
    } else {
      inline_positive_container
    };
    let main_positive_container = match style.flex_direction {
      FlexDirection::Row => inline_positive_container,
      FlexDirection::RowReverse => !inline_positive_container,
      FlexDirection::Column => block_positive_container,
      FlexDirection::ColumnReverse => !block_positive_container,
    };

    // Flex items align to the parent flex container's axes, not their own writing-mode/direction.
    let axis_source = containing_flex.unwrap_or(style);
    let inline_positive_item = self.inline_axis_positive(axis_source);
    let block_positive_item = self.block_axis_positive(axis_source);
    let cross_positive_item = if main_is_inline {
      block_positive_item
    } else {
      inline_positive_item
    };

    let reserve_scroll_x = style.scrollbar_gutter.stable
      && matches!(style.overflow_x, CssOverflow::Auto | CssOverflow::Scroll);
    let reserve_scroll_y = style.scrollbar_gutter.stable
      && matches!(style.overflow_y, CssOverflow::Auto | CssOverflow::Scroll);
    let map_overflow = |value: CssOverflow, reserve: bool| match value {
      // Taffy lacks an Auto variant; treat it like Visible unless scrollbar-gutter requests stability.
      CssOverflow::Visible | CssOverflow::Auto => {
        if reserve {
          TaffyOverflow::Scroll
        } else {
          TaffyOverflow::Visible
        }
      }
      CssOverflow::Hidden => TaffyOverflow::Hidden,
      CssOverflow::Scroll => TaffyOverflow::Scroll,
      CssOverflow::Clip => TaffyOverflow::Clip,
    };

    let mut min_width_dimension =
      self.length_option_to_dimension_box_sizing(style.min_width.as_ref(), style, Axis::Horizontal);
    let mut min_height_dimension =
      self.length_option_to_dimension_box_sizing(style.min_height.as_ref(), style, Axis::Vertical);

    // Flexbox automatic minimum size (min-width/min-height: auto) applies on the flex item's *main*
    // axis (driven by the containing flex container), not the item's own flex-direction. Taffy
    // treats `auto` as zero, so compute the content-based minimum size to prevent shrink-to-zero
    // flex items on real pages. Flexbox specifies that scroll containers use a 0 automatic minimum.
    if !is_root {
      if let Some(container) = containing_flex {
        let container_inline_is_horizontal =
          matches!(container.writing_mode, WritingMode::HorizontalTb);
        let container_main_is_inline = matches!(
          container.flex_direction,
          FlexDirection::Row | FlexDirection::RowReverse
        );
        let container_main_is_horizontal = if container_inline_is_horizontal {
          container_main_is_inline
        } else {
          !container_main_is_inline
        };

        if container_main_is_horizontal {
          if min_width_dimension == Dimension::AUTO
            && matches!(style.overflow_x, CssOverflow::Visible)
          {
            if let Ok(min_content) =
              self.compute_intrinsic_inline_size(box_node, IntrinsicSizingMode::MinContent)
            {
              if min_content.is_finite() && min_content > 0.0 {
                min_width_dimension = Dimension::length(min_content);
              }
            }
          }
        } else if min_height_dimension == Dimension::AUTO
          && matches!(style.overflow_y, CssOverflow::Visible)
        {
          if let Ok(min_content) =
            self.compute_intrinsic_block_size(box_node, IntrinsicSizingMode::MinContent)
          {
            if min_content.is_finite() && min_content > 0.0 {
              min_height_dimension = Dimension::length(min_content);
            }
          }
        }
      }
    }

    taffy::style::Style {
      // Display mode - only root is Flex, children are Block (flex items)
      display: self.display_to_taffy(style, is_root),

      // Flex container properties
      flex_direction: self.flex_direction_to_taffy(
        style,
        inline_positive_container,
        block_positive_container,
      ),
      flex_wrap: self.flex_wrap_to_taffy(style.flex_wrap),
      justify_content: self
        .justify_content_to_taffy(style.justify_content, main_positive_container),
      align_items: self.align_items_to_taffy(style.align_items, cross_positive_container),
      align_content: self.align_content_to_taffy(style.align_content, cross_positive_container),
      align_self: self.align_self_to_taffy(style.align_self, cross_positive_item),
      justify_self: self.align_self_to_taffy(style.justify_self, inline_positive_item),
      justify_items: self.align_items_to_taffy(style.justify_items, inline_positive_container),

      // Gap
      gap: taffy::geometry::Size {
        // Column gap follows the inline axis; row gap follows the block axis.
        width: if inline_is_horizontal_container {
          self.length_to_taffy_lp(&style.grid_column_gap, style)
        } else {
          self.length_to_taffy_lp(&style.grid_row_gap, style)
        },
        height: if inline_is_horizontal_container {
          self.length_to_taffy_lp(&style.grid_row_gap, style)
        } else {
          self.length_to_taffy_lp(&style.grid_column_gap, style)
        },
      },

      // Flex item properties
      flex_grow: style.flex_grow,
      flex_shrink: style.flex_shrink,
      flex_basis: self.flex_basis_to_taffy(&style.flex_basis, style),

      // Sizing - for root flex container without explicit size, use 100%
      // to fill the available space (block-level behavior)
      size: self.compute_size(style, is_root),
      min_size: taffy::geometry::Size {
        width: min_width_dimension,
        height: min_height_dimension,
      },
      max_size: taffy::geometry::Size {
        width: self.length_option_to_dimension_box_sizing(
          style.max_width.as_ref(),
          style,
          Axis::Horizontal,
        ),
        height: self.length_option_to_dimension_box_sizing(
          style.max_height.as_ref(),
          style,
          Axis::Vertical,
        ),
      },

      // Spacing
      padding: taffy::geometry::Rect {
        left: self.length_to_taffy_lp(&style.padding_left, style),
        right: self.length_to_taffy_lp(&style.padding_right, style),
        top: self.length_to_taffy_lp(&style.padding_top, style),
        bottom: self.length_to_taffy_lp(&style.padding_bottom, style),
      },
      margin: taffy::geometry::Rect {
        left: self.length_option_to_lpa(style.margin_left.as_ref(), style),
        right: self.length_option_to_lpa(style.margin_right.as_ref(), style),
        top: self.length_option_to_lpa(style.margin_top.as_ref(), style),
        bottom: self.length_option_to_lpa(style.margin_bottom.as_ref(), style),
      },
      border: taffy::geometry::Rect {
        left: self.length_to_taffy_lp(&style.border_left_width, style),
        right: self.length_to_taffy_lp(&style.border_right_width, style),
        top: self.length_to_taffy_lp(&style.border_top_width, style),
        bottom: self.length_to_taffy_lp(&style.border_bottom_width, style),
      },
      aspect_ratio: self.aspect_ratio_to_taffy(style.aspect_ratio),

      overflow: taffy::geometry::Point {
        x: map_overflow(style.overflow_x, reserve_scroll_x),
        y: map_overflow(style.overflow_y, reserve_scroll_y),
      },
      scrollbar_width: resolve_scrollbar_width(style),

      ..Default::default()
    }
  }

  /// Computes the size for a node
  ///
  /// For the root flex container without explicit size, we use 100% to fill
  /// available space (simulating block-level behavior).
  fn compute_size(&self, style: &ComputedStyle, is_root: bool) -> taffy::geometry::Size<Dimension> {
    let is_block_level_root = is_root && matches!(style.display, Display::Flex);
    let width = match style.width.as_ref() {
      Some(len) => self.dimension_for_box_sizing(len, style, Axis::Horizontal),
      None if is_block_level_root => {
        // Root flex container without explicit width: expand to fill
        // available space (100% of containing block)
        Dimension::percent(1.0)
      }
      None => Dimension::auto(),
    };

    let height = match style.height.as_ref() {
      Some(len) => self.dimension_for_box_sizing(len, style, Axis::Vertical),
      None => Dimension::auto(), // Height always auto unless specified
    };

    taffy::geometry::Size { width, height }
  }

  /// Converts Taffy layout back to FragmentNode tree
  #[allow(clippy::only_used_in_recursion)]
  fn taffy_to_fragment(
    &self,
    taffy_tree: &TaffyTree<*const BoxNode>,
    taffy_node: NodeId,
    root_id: NodeId,
    box_node: &BoxNode,
    node_map: &HashMap<*const BoxNode, NodeId>,
    constraints: &LayoutConstraints,
  ) -> Result<FragmentNode, LayoutError> {
    fn accumulate_bounds(node: &FragmentNode, offset: Point, min: &mut Point, max: &mut Point) {
      let abs = Rect::from_xywh(
        node.bounds.x() + offset.x,
        node.bounds.y() + offset.y,
        node.bounds.width(),
        node.bounds.height(),
      );
      min.x = min.x.min(abs.x());
      min.y = min.y.min(abs.y());
      max.x = max.x.max(abs.max_x());
      max.y = max.y.max(abs.max_y());
      let next = Point::new(offset.x + node.bounds.x(), offset.y + node.bounds.y());
      for child in node.children.iter() {
        accumulate_bounds(child, next, min, max);
      }
    }

    fn fragment_subtree_size(node: &FragmentNode) -> Size {
      let mut min = Point::new(0.0, 0.0);
      let mut max = Point::new(0.0, 0.0);
      accumulate_bounds(node, Point::ZERO, &mut min, &mut max);
      Size::new((max.x - min.x).max(0.0), (max.y - min.y).max(0.0))
    }

    fn find_cached_fragment(
      cache: &HashMap<(Option<u32>, Option<u32>), (Size, std::sync::Arc<FragmentNode>)>,
      target_size: Size,
    ) -> Option<(Size, std::sync::Arc<FragmentNode>)> {
      let mut best = None;
      let mut best_score = f32::MAX;
      let (eps_w, eps_h) = cache_tolerances(target_size);
      // Match cached fragments within a tolerance so quantized measurements (2–8px)
      // can be reused without relayout.
      for (size, frag) in cache.values() {
        if !size.width.is_finite() || !size.height.is_finite() {
          continue;
        }
        let dw = (size.width - target_size.width).abs();
        let dh = (size.height - target_size.height).abs();
        if dw <= eps_w && dh <= eps_h {
          let score = dw + dh;
          if score < best_score {
            best_score = score;
            best = Some((*size, frag.clone()));
          }
        }
      }
      best
    }

    // Get layout from Taffy
    let layout = taffy_tree
      .layout(taffy_node)
      .map_err(|e| LayoutError::MissingContext(format!("Failed to get Taffy layout: {:?}", e)))?;

    // Create fragment rect (Taffy uses relative coordinates)
    let mut rect = Rect::new(
      Point::new(layout.location.x, layout.location.y),
      Size::new(layout.size.width, layout.size.height),
    );
    if taffy_node == root_id {
      // When Taffy collapses the flex container to ~0px (often after a bad intrinsic probe),
      // fall back to the definite available width so children aren't clamped to a 0–1px line.
      let rect_eps = 0.01;
      if !rect.width().is_finite() || rect.width() <= rect_eps {
        if let Some(def_w) = constraints
          .width()
          .filter(|w| w.is_finite() && *w > rect_eps)
        {
          rect.size.width = def_w;
        } else if let Some(base) = constraints.inline_percentage_base.filter(|w| *w > rect_eps) {
          rect.size.width = base;
        } else {
          rect.size.width = self.viewport_size.width;
        }
      }
      // Clamp overly wide layouts back to the definite available inline size so children
      // are reflowed within the container instead of inheriting a runaway max-content span.
      if let Some(def_w) = constraints
        .width()
        .filter(|w| w.is_finite() && *w > rect_eps)
      {
        if rect.size.width > def_w + rect_eps {
          rect.size.width = def_w;
        }
      }
    }

    // Convert children by re-running layout with the definite sizes Taffy resolved.
    // This preserves the fully laid-out fragment trees (text, inline content) instead of
    // reconstructing empty boxes from the cached measure results.
    let mut children: Vec<FragmentNode>;
    let factory = self.child_factory();
    let measured_fragments = self.measured_fragments.clone();
    let main_axis_is_row = matches!(
      box_node.style.flex_direction,
      crate::style::types::FlexDirection::Row | crate::style::types::FlexDirection::RowReverse
    );
    let allow_overflow_fallback = !matches!(box_node.style.flex_wrap, FlexWrap::NoWrap)
      && if main_axis_is_row {
        matches!(box_node.style.overflow_x, CssOverflow::Visible)
      } else {
        matches!(box_node.style.overflow_y, CssOverflow::Visible)
      };
    let toggles = crate::debug::runtime::runtime_toggles();
    let mut fallback_cursor_x = 0.0;
    let mut fallback_cursor_y = 0.0;
    let mut last_layout_x: Option<f32> = None;
    let mut last_layout_y: Option<f32> = None;
    let mut manual_row_positions = false;
    let mut manual_col_positions = false;
    let wrap_eps = 0.5;
    let mut unordered_children: Vec<(i32, usize, FragmentNode)> =
      Vec::with_capacity(box_node.children.len());
    #[cfg(test)]
    eprintln!(
      "[flex-debug-node-map-len] box_id={} node_map_len={}",
      box_node.id,
      node_map.len()
    );
    for (dom_idx, child_box) in box_node.children.iter().enumerate() {
      if let Some(&child_taffy) = node_map.get(&(child_box as *const BoxNode)) {
        let child_layout = taffy_tree.layout(child_taffy).map_err(|e| {
          LayoutError::MissingContext(format!("Failed to get Taffy layout: {:?}", e))
        })?;
        let child_loc_x = child_layout.location.x - rect.origin.x;
        let child_loc_y = child_layout.location.y - rect.origin.y;
        let eps = 0.01;
        let rect_w = rect.width();
        let rect_h = rect.height();
        let mut layout_width = child_layout.size.width;
        let mut layout_height = child_layout.size.height;
        let raw_layout_width = layout_width;
        let raw_layout_height = layout_height;
        let target_width = if raw_layout_width > eps {
          raw_layout_width
        } else if rect_w.is_finite() && rect_w > eps {
          rect_w
        } else {
          self.viewport_size.width
        };
        let target_height = if raw_layout_height > eps {
          raw_layout_height
        } else if rect_h.is_finite() && rect_h > eps {
          rect_h
        } else {
          self.viewport_size.height
        };
        if toggles.truthy("FASTR_DEBUG_FLEX_CHILD") {
          eprintln!(
                        "[flex-child-layout] id={} selector={} layout=({:.2},{:.2}) loc=({:.2},{:.2}) grow={} shrink={} basis={:?}",
                        child_box.id,
                        child_box
                            .debug_info
                            .as_ref()
                            .map(|d| d.to_selector())
                            .unwrap_or_else(|| "<anon>".to_string()),
                        layout_width,
                        layout_height,
                        child_loc_x,
                        child_loc_y,
                        child_box.style.flex_grow,
                        child_box.style.flex_shrink,
                        child_box.style.flex_basis
                    );
        }

        // Guard against zero-sized cross axes coming from overly tight flex constraints
        // (e.g., items measured with 0px available space). When the flex container has
        // a real cross size, fall back to that (or the resolved specified size) so
        // percentage/auto widths don't collapse to zero.
        if !main_axis_is_row && layout_width <= eps {
          let explicit_zero_width = child_box
            .style
            .width
            .as_ref()
            .map(|l| l.unit.is_absolute() && l.value.abs() <= eps && !l.unit.is_percentage())
            .unwrap_or(false);
          if !explicit_zero_width {
            let base = if rect_w.is_finite() && rect_w > eps {
              rect_w
            } else {
              self.viewport_size.width
            };
            if let Some(specified) = child_box
              .style
              .width
              .as_ref()
              .map(|l| self.resolve_length_for_width(*l, base, &child_box.style))
            {
              if specified > eps {
                layout_width = specified;
              }
            }
            if layout_width <= eps && base > eps {
              layout_width = base;
            }
          }
        }
        if main_axis_is_row && layout_height <= eps {
          let explicit_zero_height = child_box
            .style
            .height
            .as_ref()
            .map(|l| l.unit.is_absolute() && l.value.abs() <= eps && !l.unit.is_percentage())
            .unwrap_or(false);
          if !explicit_zero_height {
            let base = if rect_h.is_finite() && rect_h > eps {
              rect_h
            } else {
              self.viewport_size.height
            };
            if let Some(specified) = child_box
              .style
              .height
              .as_ref()
              .map(|l| self.resolve_length_for_width(*l, base, &child_box.style))
            {
              if specified > eps {
                layout_height = specified;
              }
            }
            if layout_height <= eps && base > eps {
              layout_height = base;
            }
          }
        }
        if rect_w.is_finite() && rect_w > eps {
          layout_width = layout_width.min(rect_w);
        }
        if rect_h.is_finite() && rect_h > eps {
          layout_height = layout_height.min(rect_h);
        }
        let needs_intrinsic_main = (main_axis_is_row && raw_layout_width <= eps)
          || (!main_axis_is_row && raw_layout_height <= eps);

        let mut reused: Option<PlacedFragment> = None;
        if !needs_intrinsic_main {
          if let Some((stored_size, frag)) = measured_fragments.find_fragment(
            flex_cache_key(child_box),
            Size::new(target_width, target_height),
          ) {
            record_fragment_clone(CloneSite::FlexMeasureReuse, frag.as_ref());
            let template = CachedFragmentTemplate::new(frag);
            let intrinsic_size = Self::fragment_subtree_size(template.fragment());
            let mut resolved_width = layout_width;
            let mut resolved_height = layout_height;
            if resolved_width <= eps && intrinsic_size.width > eps {
              resolved_width = intrinsic_size.width;
            }
            if resolved_height <= eps && intrinsic_size.height > eps {
              resolved_height = intrinsic_size.height;
            }
            if resolved_width <= eps {
              resolved_width = stored_size.width.max(resolved_width);
            }
            if resolved_height <= eps {
              resolved_height = stored_size.height.max(resolved_height);
            }
            if resolved_width <= eps {
              resolved_width = target_width;
            }
            if resolved_height <= eps {
              resolved_height = target_height;
            }
            if rect.width().is_finite() && rect.width() > 0.0 {
              resolved_width = resolved_width.min(rect.width());
            }
            if rect.height().is_finite() && rect.height() > 0.0 {
              resolved_height = resolved_height.min(rect.height());
            }
            let mut origin_x = child_loc_x;
            let mut origin_y = child_loc_y;
            if main_axis_is_row && rect.height().is_finite() {
              let limit = rect.height().max(1.0) * 5.0;
              if origin_y.abs() > limit {
                origin_y = rect.origin.y;
              }
            }
            if resolved_width <= eps && main_axis_is_row {
              manual_row_positions = true;
            }
            if resolved_height <= eps && !main_axis_is_row {
              manual_col_positions = true;
            }
            if allow_overflow_fallback
              && main_axis_is_row
              && rect_w.is_finite()
              && rect_w > wrap_eps
            {
              let runaway = child_loc_x.abs() > rect_w * 2.0;
              if runaway {
                manual_row_positions = true;
                fallback_cursor_x = rect.origin.x;
              }
            }
            if main_axis_is_row {
              let same_row = last_layout_y
                .map(|prev_y| (child_loc_y - prev_y).abs() < wrap_eps)
                .unwrap_or(true);
              if same_row {
                if allow_overflow_fallback {
                  if child_loc_x > rect.width() + wrap_eps {
                    manual_row_positions = true;
                    fallback_cursor_x = rect.origin.x;
                  }
                  if child_loc_x < rect.origin.x - rect.width().abs().max(wrap_eps) {
                    manual_row_positions = true;
                    fallback_cursor_x = rect.origin.x;
                  }
                  if let Some(prev) = last_layout_x {
                    if child_loc_x <= prev + 0.1 {
                      manual_row_positions = true;
                    }
                  }
                }
                if last_layout_x.is_none() && !manual_row_positions {
                  fallback_cursor_x = child_loc_x;
                }
              } else {
                manual_row_positions = false;
                fallback_cursor_x = child_loc_x;
              }
              let use_manual_row = allow_overflow_fallback && manual_row_positions;
              if use_manual_row {
                let cap_base = if rect.width().is_finite() && rect.width() > wrap_eps {
                  rect.width()
                } else {
                  self.viewport_size.width
                };
                let cap = cap_base.max(wrap_eps) * 2.0 + rect.origin.x;
                if fallback_cursor_x + resolved_width > cap {
                  fallback_cursor_x = rect.origin.x;
                  last_layout_x = None;
                }
              }
              if use_manual_row {
                origin_x = fallback_cursor_x;
                fallback_cursor_x += resolved_width;
              } else {
                fallback_cursor_x = child_loc_x + resolved_width;
                last_layout_x = Some(child_loc_x);
                last_layout_y = Some(child_loc_y);
              }
            } else {
              if let Some(prev) = last_layout_y {
                if child_loc_y <= prev + 0.1 {
                  manual_col_positions = true;
                }
              } else {
                fallback_cursor_y = child_loc_y;
              }
              if manual_col_positions {
                origin_y = fallback_cursor_y;
                fallback_cursor_y += resolved_height;
              } else {
                fallback_cursor_y = child_loc_y + resolved_height;
                last_layout_y = Some(child_loc_y);
              }
            }
            let log_child_ids = toggles
              .usize_list("FASTR_LOG_FLEX_CHILD_IDS")
              .unwrap_or_default();
            let log_child = !log_child_ids.is_empty()
              && (log_child_ids.contains(&child_box.id) || log_child_ids.contains(&box_node.id));
            if log_child {
              let selector = child_box
                .debug_info
                .as_ref()
                .map(|d| d.to_selector())
                .unwrap_or_else(|| "<anon>".to_string());
              eprintln!(
                                    "[flex-child-reuse] parent_id={} child_id={} selector={} layout=({:.2},{:.2}) loc=({:.2},{:.2}) resolved=({:.2},{:.2}) rect_w={:.2} manual_row={} cursor_x={:.2} flex=({:.2},{:.2},{:?}) width={:?} min_w={:?} max_w={:?}",
                                    box_node.id,
                                    child_box.id,
                                    selector,
                                    layout_width,
                                    layout_height,
                                    child_loc_x,
                                    child_loc_y,
                                    resolved_width,
                                    resolved_height,
                                    rect.width(),
                                    manual_row_positions,
                                    fallback_cursor_x,
                                    child_box.style.flex_grow,
                                    child_box.style.flex_shrink,
                                    child_box.style.flex_basis,
                                    child_box.style.width,
                                    child_box.style.min_width,
                                    child_box.style.max_width
                                );
            }
            let bounds = Rect::new(
              Point::new(origin_x, origin_y),
              Size::new(resolved_width, resolved_height),
            );
            reused = Some(template.place(bounds));
          }
        }

        let mut final_fragment: Option<FragmentNode> = None;
        if let Some(child_fragment) = reused {
          final_fragment = Some(child_fragment.materialize());
        } else if let crate::tree::box_tree::BoxType::Replaced(replaced_box) = &child_box.box_type {
          let bounds = Rect::new(
            Point::new(child_loc_x, child_loc_y),
            Size::new(layout_width, layout_height),
          );
          let fragment = FragmentNode::new_with_style(
            bounds,
            crate::tree::fragment_tree::FragmentContent::Replaced {
              replaced_type: replaced_box.replaced_type.clone(),
              box_id: Some(child_box.id),
            },
            vec![],
            child_box.style.clone(),
          );
          final_fragment = Some(fragment);
        } else {
          let fc_type = child_box
            .formatting_context()
            .unwrap_or(FormattingContextType::Block);
          let fc = factory.get(fc_type);
          let node_timer = flex_profile::node_timer();
          let selector_for_profile = node_timer
            .as_ref()
            .and_then(|_| child_box.debug_info.as_ref().map(|d| d.to_selector()));
          // Preserve the flex-resolved size by overriding the child's used width/height
          // with the Taffy result before laying it out in its own formatting context.
          let mut layout_child = child_box.clone();
          let mut layout_style = (*layout_child.style).clone();
          let size_eps = 0.01;
          if raw_layout_width.is_finite() && raw_layout_width > size_eps {
            layout_style.width = Some(Length::px(raw_layout_width));
          } else {
            layout_style.width = None;
            layout_style.min_width = None;
            layout_style.max_width = None;
          }
          if raw_layout_height.is_finite() && raw_layout_height > size_eps {
            layout_style.height = Some(Length::px(raw_layout_height));
          } else {
            layout_style.height = None;
            layout_style.min_height = None;
            layout_style.max_height = None;
          }
          layout_child.style = std::sync::Arc::new(layout_style);
          let rect_main_def = if main_axis_is_row { rect_w } else { rect_h };
          let child_constraints = if needs_intrinsic_main {
            let width = if main_axis_is_row {
              if raw_layout_width > eps {
                CrateAvailableSpace::Definite(raw_layout_width)
              } else if rect_main_def.is_finite() && rect_main_def > eps {
                CrateAvailableSpace::Definite(rect_main_def)
              } else {
                CrateAvailableSpace::MaxContent
              }
            } else if raw_layout_width > eps {
              CrateAvailableSpace::Definite(layout_width)
            } else {
              CrateAvailableSpace::MaxContent
            };
            let height = if main_axis_is_row {
              if raw_layout_height > eps {
                CrateAvailableSpace::Definite(raw_layout_height)
              } else {
                CrateAvailableSpace::MaxContent
              }
            } else if layout_height > eps {
              CrateAvailableSpace::Definite(layout_height)
            } else if rect_main_def.is_finite() && rect_main_def > eps {
              CrateAvailableSpace::Definite(rect_main_def)
            } else {
              CrateAvailableSpace::MaxContent
            };
            LayoutConstraints::new(width, height)
          } else {
            LayoutConstraints::new(
              CrateAvailableSpace::Definite(layout_width),
              CrateAvailableSpace::Definite(layout_height),
            )
          };
          let mut child_fragment = fc.layout(&layout_child, &child_constraints)?;
          flex_profile::record_node_layout(
            child_box.id,
            selector_for_profile.as_deref(),
            node_timer,
          );
          let intrinsic_size = Self::fragment_subtree_size(&child_fragment);

          if !trace_flex_text_ids().is_empty() && trace_flex_text_ids().contains(&child_box.id) {
            let mut text_count = 0;
            fn walk(node: &FragmentNode, count: &mut usize) {
              if matches!(node.content, FragmentContent::Text { .. }) {
                *count += 1;
              }
              for child in node.children.iter() {
                walk(child, count);
              }
            }
            walk(&child_fragment, &mut text_count);
            let selector = child_box
              .debug_info
              .as_ref()
              .map(|d| d.to_selector())
              .unwrap_or_else(|| "<anon>".to_string());
            eprintln!(
              "[flex-child-text] id={} selector={} text_fragments={} size=({:.1},{:.1})",
              child_box.id,
              selector,
              text_count,
              child_fragment.bounds.width(),
              child_fragment.bounds.height()
            );
          }

          // If Taffy produced ~0 main-size, remeasure with max-content and use that result.
          if (main_axis_is_row && layout_width <= eps)
            || (!main_axis_is_row && layout_height <= eps)
          {
            let mc_constraints = if main_axis_is_row {
              LayoutConstraints::new(
                CrateAvailableSpace::MaxContent,
                CrateAvailableSpace::Definite(if layout_height > eps {
                  layout_height
                } else {
                  intrinsic_size.height
                }),
              )
            } else {
              LayoutConstraints::new(
                CrateAvailableSpace::Definite(if layout_width > eps {
                  layout_width
                } else {
                  intrinsic_size.width
                }),
                CrateAvailableSpace::MaxContent,
              )
            };
            let mc_timer = flex_profile::node_timer();
            let mc_selector = mc_timer
              .as_ref()
              .and_then(|_| child_box.debug_info.as_ref().map(|d| d.to_selector()));
            if let Ok(mut mc_fragment) = fc.layout(&layout_child, &mc_constraints) {
              flex_profile::record_node_layout(child_box.id, mc_selector.as_deref(), mc_timer);
              let mut mc_size = Self::fragment_subtree_size(&mc_fragment);
              if rect.width().is_finite() && rect.width() > 0.0 {
                mc_size.width = mc_size.width.min(rect.width());
              }
              if rect.height().is_finite() && rect.height() > 0.0 {
                mc_size.height = mc_size.height.min(rect.height());
              }
              let mut origin = Point::new(child_loc_x, child_loc_y);
              if main_axis_is_row {
                let same_row = last_layout_y
                  .map(|prev_y| (child_loc_y - prev_y).abs() < wrap_eps)
                  .unwrap_or(true);
                if !same_row || last_layout_x.is_none() {
                  fallback_cursor_x = child_loc_x;
                }
                origin.x = fallback_cursor_x;
                fallback_cursor_x += mc_size.width;
                last_layout_x = Some(child_loc_x);
                last_layout_y = Some(child_loc_y);
              } else {
                let same_col = last_layout_x
                  .map(|prev_x| (child_loc_x - prev_x).abs() < wrap_eps)
                  .unwrap_or(true);
                if !same_col || last_layout_y.is_none() {
                  fallback_cursor_y = child_loc_y;
                }
                origin.y = fallback_cursor_y;
                fallback_cursor_y += mc_size.height;
                last_layout_x = Some(child_loc_x);
                last_layout_y = Some(child_loc_y);
              }
              if !main_axis_is_row && layout_width > eps {
                origin.x = child_loc_x;
              }
              mc_fragment.bounds = Rect::new(origin, mc_size);
              final_fragment = Some(mc_fragment);
            }
          }

          if final_fragment.is_none() {
            // Position the child using the Taffy-computed coordinates (relative to parent).
            let mut resolved_width = layout_width;
            let mut resolved_height = layout_height;
            if resolved_width <= eps && intrinsic_size.width > eps {
              resolved_width = intrinsic_size.width;
            }
            if resolved_height <= eps && intrinsic_size.height > eps {
              resolved_height = intrinsic_size.height;
            }
            if rect.width().is_finite() && rect.width() > 0.0 {
              resolved_width = resolved_width.min(rect.width());
            }
            if rect.height().is_finite() && rect.height() > 0.0 {
              resolved_height = resolved_height.min(rect.height());
            }
            let mut origin_x = child_loc_x;
            let mut origin_y = child_loc_y;
            if main_axis_is_row && rect.height().is_finite() {
              let limit = rect.height().max(1.0) * 5.0;
              if origin_y.abs() > limit {
                origin_y = rect.origin.y;
              }
            }
            if resolved_width <= eps && main_axis_is_row {
              manual_row_positions = true;
            }
            if resolved_height <= eps && !main_axis_is_row {
              manual_col_positions = true;
            }
            if main_axis_is_row && rect_w.is_finite() && rect_w > wrap_eps {
              let runaway = child_loc_x.abs() > rect_w * 2.0;
              if runaway {
                manual_row_positions = true;
                fallback_cursor_x = rect.origin.x;
              }
            }
            if main_axis_is_row {
              let same_row = last_layout_y
                .map(|prev_y| (child_loc_y - prev_y).abs() < wrap_eps)
                .unwrap_or(true);
              if same_row {
                if allow_overflow_fallback {
                  if child_loc_x > rect.width() + wrap_eps {
                    manual_row_positions = true;
                    fallback_cursor_x = rect.origin.x;
                  }
                  if child_loc_x < rect.origin.x - rect.width().abs().max(wrap_eps) {
                    manual_row_positions = true;
                    fallback_cursor_x = rect.origin.x;
                  }
                  if let Some(prev) = last_layout_x {
                    if child_loc_x <= prev + 0.1 {
                      manual_row_positions = true;
                    }
                  }
                }
                if last_layout_x.is_none() && !manual_row_positions {
                  fallback_cursor_x = child_loc_x;
                }
              } else {
                manual_row_positions = false;
                fallback_cursor_x = child_loc_x;
              }
              let use_manual_row = allow_overflow_fallback && manual_row_positions;
              if use_manual_row {
                let cap_base = if rect.width().is_finite() && rect.width() > wrap_eps {
                  rect.width()
                } else {
                  self.viewport_size.width
                };
                let cap = cap_base.max(wrap_eps) * 2.0 + rect.origin.x;
                if fallback_cursor_x + resolved_width > cap {
                  fallback_cursor_x = rect.origin.x;
                  last_layout_x = None;
                }
              }
              if use_manual_row {
                origin_x = fallback_cursor_x;
                fallback_cursor_x += resolved_width;
              } else {
                fallback_cursor_x = child_loc_x + resolved_width;
                last_layout_x = Some(child_loc_x);
                last_layout_y = Some(child_loc_y);
              }
            } else {
              if let Some(prev) = last_layout_y {
                if child_loc_y <= prev + 0.1 {
                  manual_col_positions = true;
                }
              } else {
                fallback_cursor_y = child_loc_y;
              }
              if manual_col_positions {
                origin_y = fallback_cursor_y;
                fallback_cursor_y += resolved_height;
              } else {
                fallback_cursor_y = child_loc_y + resolved_height;
                last_layout_y = Some(child_loc_y);
              }
            }
            let log_child_ids = toggles
              .usize_list("FASTR_LOG_FLEX_CHILD_IDS")
              .unwrap_or_default();
            let log_child = !log_child_ids.is_empty()
              && (log_child_ids.contains(&child_box.id) || log_child_ids.contains(&box_node.id));
            if log_child {
              let selector = child_box
                .debug_info
                .as_ref()
                .map(|d| d.to_selector())
                .unwrap_or_else(|| "<anon>".to_string());
              eprintln!(
                                "[flex-child-place] parent_id={} child_id={} selector={} layout=({:.2},{:.2}) loc=({:.2},{:.2}) resolved=({:.2},{:.2}) rect_w={:.2} manual_row={} cursor_x={:.2} flex=({:.2},{:.2},{:?}) width={:?} min_w={:?} max_w={:?}",
                                box_node.id,
                                child_box.id,
                                selector,
                                layout_width,
                                layout_height,
                                child_loc_x,
                                child_loc_y,
                                resolved_width,
                                resolved_height,
                                rect.width(),
                                manual_row_positions,
                                fallback_cursor_x,
                                child_box.style.flex_grow,
                                child_box.style.flex_shrink,
                                child_box.style.flex_basis,
                                child_box.style.width,
                                child_box.style.min_width,
                                child_box.style.max_width
                            );
            }
            child_fragment.bounds = Rect::new(
              Point::new(origin_x, origin_y),
              Size::new(resolved_width, resolved_height),
            );
            final_fragment = Some(child_fragment);
          }
        }

        if let Some(fragment) = final_fragment {
          unordered_children.push((child_box.style.order, dom_idx, fragment));
        }
      }
      #[cfg(test)]
      if node_map.get(&(child_box as *const BoxNode)).is_none() {
        eprintln!(
          "[flex-debug-missing-child] box_id={} child_ptr={:p}",
          box_node.id, child_box
        );
      }
    }
    unordered_children.sort_by(|a, b| a.0.cmp(&b.0).then_with(|| a.1.cmp(&b.1)));
    children = unordered_children
      .into_iter()
      .map(|(_, _, frag)| frag)
      .collect();
    #[cfg(test)]
    if children.is_empty() {
      let keys: Vec<usize> = node_map.keys().map(|k| *k as usize).collect();
      let child_ptrs: Vec<usize> = box_node
        .children
        .iter()
        .map(|c| c as *const _ as usize)
        .collect();
      eprintln!(
        "[flex-debug-empty-children] box_id={} keys={:?} child_ptrs={:?}",
        box_node.id, keys, child_ptrs
      );
    }

    // If Taffy reported non-increasing positions along the main axis, fall back to a simple
    // manual placement using the resolved fragment widths/heights to avoid overlapping items.
    static OVERFLOW_COUNTS: OnceLock<Mutex<std::collections::HashMap<usize, usize>>> =
      OnceLock::new();
    let log_overflow = toggles.truthy("FASTR_LOG_FLEX_OVERFLOW");
    let log_overflow_ids = toggles
      .usize_list("FASTR_LOG_FLEX_OVERFLOW_IDS")
      .unwrap_or_default();
    if main_axis_is_row {
      // Re-run manual row placement when Taffy positions items beyond the container width.
      // Even when overflow is expected (wide items), their starting position should remain
      // at the row origin rather than drifting far to the right.
      let container_w = if rect.width().is_finite() && rect.width() > wrap_eps {
        rect.width()
      } else {
        self.viewport_size.width
      };
      let mut max_child_x = children
        .iter()
        .map(|c| c.bounds.max_x())
        .fold(0.0, f32::max);
      let log_shrink_ids = toggles
        .usize_list("FASTR_LOG_FLEX_SHRINK_IDS")
        .unwrap_or_default();
      let log_this_shrink = !log_shrink_ids.is_empty() && log_shrink_ids.contains(&box_node.id);

      if allow_overflow_fallback && max_child_x > container_w + 0.5 {
        let available = container_w.max(1.0).min(self.viewport_size.width);
        // Apply flex-shrink distribution to bring the total width back to the available span.
        let mut total_main = 0.0;
        let mut total_weight = 0.0;
        let mut child_count = 0usize;
        let mut box_lookup: std::collections::HashMap<usize, &BoxNode> =
          std::collections::HashMap::new();
        for child in &box_node.children {
          box_lookup.insert(child.id, child);
        }
        let mut frag_box_ids: Vec<(usize, usize)> = Vec::new();
        for (idx, frag) in children.iter().enumerate() {
          if let Some(box_id) = match &frag.content {
            FragmentContent::Block { box_id }
            | FragmentContent::Inline { box_id, .. }
            | FragmentContent::Text { box_id, .. }
            | FragmentContent::Replaced { box_id, .. } => *box_id,
            FragmentContent::RunningAnchor { .. } => None,
            FragmentContent::Line { .. } => None,
          } {
            frag_box_ids.push((idx, box_id));
          }
        }
        for (frag_idx, box_id) in &frag_box_ids {
          if let Some(child_node) = box_lookup.get(box_id) {
            let child_fragment = &children[*frag_idx];
            let base = child_fragment.bounds.width();
            let weight = child_node.style.flex_shrink.max(0.0) * base;
            total_main += base;
            total_weight += weight;
            child_count += 1;
          }
        }
        if log_this_shrink {
          let widths: Vec<f32> = children.iter().map(|c| c.bounds.width()).collect();
          eprintln!(
            "[flex-shrink-pre] id={} avail_w={:.1} total={:.1} widths={:?}",
            box_node.id, available, total_main, widths
          );
        }

        if total_main > available + 0.01 {
          let deficit = total_main - available;
          let count = child_count.max(1) as f32;
          let mut cursor_x = 0.0;
          let mut cursor_y = 0.0;
          let mut row_h = 0.0;
          for (frag_idx, box_id) in &frag_box_ids {
            if let Some(child_node) = box_lookup.get(box_id) {
              let child_fragment = &mut children[*frag_idx];
              let base = child_fragment.bounds.width();
              let weight = child_node.style.flex_shrink.max(0.0) * base;
              let share = if total_weight > 0.0 {
                deficit * (weight / total_weight)
              } else {
                deficit / count
              };
              let w = (base - share).max(0.0).min(available);
              let h = child_fragment.bounds.height();
              if cursor_x + w > available + 0.1
                && cursor_x > 0.0
                && !matches!(box_node.style.flex_wrap, FlexWrap::NoWrap)
              {
                cursor_x = 0.0;
                cursor_y += row_h;
                row_h = 0.0;
              }
              child_fragment.bounds = Rect::new(Point::new(cursor_x, cursor_y), Size::new(w, h));
              cursor_x += w;
              row_h = row_h.max(h);
            }
          }
        } else {
          // Even without shrink, ensure sequential placement within the line.
          let mut cursor_x = 0.0;
          let mut cursor_y = 0.0;
          let mut row_h = 0.0;
          for child in &mut children {
            let w = child.bounds.width().min(available);
            let h = child.bounds.height();
            if cursor_x + w > available + 0.1
              && cursor_x > 0.0
              && !matches!(box_node.style.flex_wrap, FlexWrap::NoWrap)
            {
              cursor_x = 0.0;
              cursor_y += row_h;
              row_h = 0.0;
            }
            child.bounds = Rect::new(Point::new(cursor_x, cursor_y), Size::new(w, h));
            cursor_x += w;
            row_h = row_h.max(h);
          }
        }

        if log_this_shrink {
          let widths: Vec<f32> = children.iter().map(|c| c.bounds.width()).collect();
          eprintln!(
            "[flex-shrink-post] id={} avail_w={:.1} total={:.1} widths={:?}",
            box_node.id,
            available,
            widths.iter().sum::<f32>(),
            widths
          );
        }
      }
      // Log overflow after any redistribution/reflow so diagnostics reflect final placement.
      max_child_x = children
        .iter()
        .map(|c| c.bounds.max_x())
        .fold(0.0, f32::max);
      // If all children have been pushed far to the left (beyond 2× the container width),
      // shift them back so the leftmost child starts at the origin. This guards against
      // runaway negative positions from broken intrinsic sizing or cached fragments.
      let min_child_x = children
        .iter()
        .map(|c| c.bounds.x())
        .fold(f32::INFINITY, f32::min);
      if min_child_x.is_finite() {
        let clamp_width = container_w.max(self.viewport_size.width).max(1.0) * 2.0;
        if min_child_x < -clamp_width {
          let dx = -min_child_x;
          for child in &mut children {
            child.bounds = Rect::new(
              Point::new(child.bounds.x() + dx, child.bounds.y()),
              child.bounds.size,
            );
          }
          max_child_x = children
            .iter()
            .map(|c| c.bounds.max_x())
            .fold(0.0, f32::max);
        }
      }
      let should_log =
        log_overflow || (!log_overflow_ids.is_empty() && log_overflow_ids.contains(&box_node.id));
      if should_log && max_child_x > container_w + 0.5 {
        let log_allowed = {
          let mut counts = OVERFLOW_COUNTS
            .get_or_init(|| Mutex::new(std::collections::HashMap::new()))
            .lock()
            .ok();
          counts
            .as_mut()
            .map(|map| {
              let counter = map.entry(box_node.id).or_insert(0);
              let allowed = *counter < 2;
              *counter += 1;
              allowed
            })
            .unwrap_or(true)
        };
        if log_allowed {
          let selector = box_node
            .debug_info
            .as_ref()
            .map(|d| d.to_selector())
            .unwrap_or_else(|| "<anon>".to_string());
          eprintln!(
                        "[flex-overflow-row] id={} selector={} container_w={:.1} max_child_x={:.1} child_count={}",
                        box_node.id,
                        selector,
                        container_w,
                        max_child_x,
                        children.len()
                    );
          for (idx, child) in children.iter().enumerate().take(12) {
            let frag_box_id = match &child.content {
              crate::tree::fragment_tree::FragmentContent::Block { box_id }
              | crate::tree::fragment_tree::FragmentContent::Inline { box_id, .. } => {
                box_id.clone()
              }
              crate::tree::fragment_tree::FragmentContent::Replaced { box_id, .. } => {
                box_id.clone()
              }
              _ => None,
            };
            let child_sel = box_node
              .children
              .get(idx)
              .and_then(|b| b.debug_info.as_ref().map(|d| d.to_selector()))
              .unwrap_or_else(|| "<anon>".to_string());
            eprintln!(
                            "  [flex-overflow-row-child] parent_id={} idx={} child_id={:?} sel={} x={:.1} w={:.1} max_x={:.1}",
                            box_node.id,
                            idx,
                            frag_box_id.or_else(|| box_node.children.get(idx).map(|b| b.id)),
                            child_sel,
                            child.bounds.x(),
                            child.bounds.width(),
                            child.bounds.max_x(),
                        );
          }
        }
      }
    } else {
      let container_h = rect.height();
      let max_child_y = children
        .iter()
        .map(|c| c.bounds.max_y())
        .fold(0.0, f32::max);
      let should_log =
        log_overflow || (!log_overflow_ids.is_empty() && log_overflow_ids.contains(&box_node.id));
      if should_log && max_child_y > container_h * 1.5 {
        let log_allowed = {
          let mut counts = OVERFLOW_COUNTS
            .get_or_init(|| Mutex::new(std::collections::HashMap::new()))
            .lock()
            .ok();
          counts
            .as_mut()
            .map(|map| {
              let counter = map.entry(box_node.id).or_insert(0);
              let allowed = *counter < 2;
              *counter += 1;
              allowed
            })
            .unwrap_or(true)
        };
        if log_allowed {
          let selector = box_node
            .debug_info
            .as_ref()
            .map(|d| d.to_selector())
            .unwrap_or_else(|| "<anon>".to_string());
          eprintln!(
                        "[flex-overflow-col] id={} selector={} container_h={:.1} max_child_y={:.1} child_count={}",
                        box_node.id,
                        selector,
                        container_h,
                        max_child_y,
                        children.len()
                    );
          for (idx, child) in children.iter().enumerate().take(12) {
            let frag_box_id = match &child.content {
              crate::tree::fragment_tree::FragmentContent::Block { box_id }
              | crate::tree::fragment_tree::FragmentContent::Inline { box_id, .. } => {
                box_id.clone()
              }
              crate::tree::fragment_tree::FragmentContent::Replaced { box_id, .. } => {
                box_id.clone()
              }
              _ => None,
            };
            let child_sel = box_node
              .children
              .get(idx)
              .and_then(|b| b.debug_info.as_ref().map(|d| d.to_selector()))
              .unwrap_or_else(|| "<anon>".to_string());
            eprintln!(
                            "  [flex-overflow-col-child] parent_id={} idx={} child_id={:?} sel={} y={:.1} h={:.1} max_y={:.1}",
                            box_node.id,
                            idx,
                            frag_box_id.or_else(|| box_node.children.get(idx).map(|b| b.id)),
                            child_sel,
                            child.bounds.y(),
                            child.bounds.height(),
                            child.bounds.max_y(),
                        );
          }
        }
      }
      if max_child_y > container_h * 1.5 {
        let available = container_h.max(1.0).min(self.viewport_size.height);
        let mut cursor_y = 0.0;
        for child in &mut children {
          let h = child.bounds.height().min(available);
          child.bounds = Rect::new(
            Point::new(child.bounds.x(), cursor_y),
            Size::new(child.bounds.width(), h),
          );
          cursor_y += h;
        }
      }
      // Guard against cross-axis drift when column flex containers produce children far
      // outside the container width. Clamp children back to the start edge to avoid
      // dropping content offscreen when Taffy returns oversized x positions.
      let container_w = rect.width();
      let max_child_x = children
        .iter()
        .map(|c| c.bounds.max_x())
        .fold(0.0, f32::max);
      if max_child_x > container_w + 0.5 {
        let available = container_w.max(1.0).min(self.viewport_size.width);
        for child in &mut children {
          let w = child.bounds.width().min(available);
          child.bounds = Rect::new(
            Point::new(0.0, child.bounds.y()),
            Size::new(w, child.bounds.height()),
          );
        }
      }
    }

    if !children.is_empty() {
      if main_axis_is_row {
        let mut non_increasing = false;
        let mut last_x = children[0].bounds.x();
        for child in children.iter().skip(1) {
          let x = child.bounds.x();
          if x <= last_x + 0.1 {
            non_increasing = true;
            break;
          }
          last_x = x;
        }
        if non_increasing {
          let mut cursor = children[0].bounds.x();
          for child in &mut children {
            child.bounds = Rect::new(Point::new(cursor, child.bounds.y()), child.bounds.size);
            cursor += child.bounds.width();
          }
        }
      } else {
        let mut non_increasing = false;
        let mut last_y = children[0].bounds.y();
        for child in children.iter().skip(1) {
          let y = child.bounds.y();
          if y <= last_y + 0.1 {
            non_increasing = true;
            break;
          }
          last_y = y;
        }
        if non_increasing {
          let mut cursor = children[0].bounds.y();
          for child in &mut children {
            child.bounds = Rect::new(Point::new(child.bounds.x(), cursor), child.bounds.size);
            cursor += child.bounds.height();
          }
        }
      }
    }

    // If a wrapping row still overflows the container (e.g., items sized to 100% that Taffy
    // keeps on the same line), reflow the children into sequential rows within the container
    // width. This mirrors flex-wrap behaviour when items exceed the line length.
    if main_axis_is_row
      && !matches!(box_node.style.flex_wrap, FlexWrap::NoWrap)
      && rect.width().is_finite()
      && rect.width() > 0.0
    {
      let max_child_x = children
        .iter()
        .map(|c| c.bounds.max_x())
        .fold(f32::NEG_INFINITY, f32::max);
      let min_child_x = children
        .iter()
        .map(|c| c.bounds.x())
        .fold(f32::INFINITY, f32::min);
      if max_child_x > rect.width() + 0.5 || min_child_x < -0.5 {
        let avail = rect.width();
        let start_y = children.iter().map(|c| c.bounds.y()).fold(0.0, f32::min);
        let mut cursor_x = 0.0;
        let mut cursor_y = start_y;
        let mut row_height: f32 = 0.0;
        for child in &mut children {
          let mut w = child.bounds.width().min(avail);
          let h = child.bounds.height();
          if cursor_x + w > avail + 0.01 {
            cursor_y += row_height;
            cursor_x = 0.0;
            row_height = 0.0;
          }
          if w <= 0.0 {
            w = 0.0;
          }
          child.bounds = Rect::new(Point::new(cursor_x, cursor_y), Size::new(w, h));
          cursor_x += w;
          row_height = row_height.max(h);
        }
        let new_height = (cursor_y + row_height - rect.origin.y).max(rect.height());
        rect = Rect::new(rect.origin, Size::new(rect.width(), new_height));
      }
    }

    // Taffy occasionally returns row layouts where every child starts well past the
    // container's end (e.g., due to upstream constraint collapse). Flex rows, even
    // with nowrap, should still start at the line's start edge; the overflow should
    // come from the total span, not an initial offset. If all children are entirely
    // to the right of the container, translate them back so the first child starts
    // at the container origin.
    if main_axis_is_row
      && matches!(box_node.style.flex_wrap, FlexWrap::NoWrap)
      && !children.is_empty()
    {
      let min_child_x = children
        .iter()
        .map(|c| c.bounds.x())
        .fold(f32::INFINITY, f32::min);
      if min_child_x.is_finite() && min_child_x > rect.width() {
        for child in &mut children {
          child.bounds = Rect::new(
            Point::new(child.bounds.x() - min_child_x, child.bounds.y()),
            child.bounds.size,
          );
        }
      }
    }

    // Guard against pathological horizontal drift: if every child in a row sits far beyond
    // the container (multiple widths away), translate them back so the leftmost child starts
    // at the origin while preserving relative spacing.
    if main_axis_is_row && !children.is_empty() {
      let min_x = children
        .iter()
        .map(|c| c.bounds.x())
        .fold(f32::INFINITY, f32::min);
      let max_x = children
        .iter()
        .map(|c| c.bounds.max_x())
        .fold(f32::NEG_INFINITY, f32::max);
      let drift_limit = rect.width().max(1.0) * 4.0;
      if min_x.is_finite() && min_x > drift_limit && max_x.is_finite() {
        let dx = min_x;
        let log_drift = toggles.truthy("FASTR_LOG_FLEX_DRIFT");
        if log_drift {
          let selector = box_node
            .debug_info
            .as_ref()
            .map(|d| d.to_selector())
            .unwrap_or_else(|| "<anon>".to_string());
          eprintln!(
                        "[flex-drift-clamp] id={} selector={} min_x={:.1} max_x={:.1} rect_w={:.1} dx={:.1} children={}",
                        box_node.id,
                        selector,
                        min_x,
                        max_x,
                        rect.width(),
                        dx,
                        children.len()
                    );
        }
        for child in &mut children {
          child.bounds = Rect::new(
            Point::new(child.bounds.x() - dx, child.bounds.y()),
            child.bounds.size,
          );
        }
      } else if min_x.is_finite()
        && max_x.is_finite()
        && rect.width().is_finite()
        && min_x > rect.width() * 0.5
        && (max_x - min_x) <= rect.width() * 1.5
      {
        // Children sit noticeably to the right but still span roughly the container width.
        // Re-center them within the container to avoid an empty viewport.
        let span = max_x - min_x;
        let target_min = ((rect.width() - span).max(0.0)) / 2.0;
        let dx = min_x - target_min;
        for child in &mut children {
          child.bounds = Rect::new(
            Point::new(child.bounds.x() - dx, child.bounds.y()),
            child.bounds.size,
          );
        }
      }
    }

    // Final clamp: only adjust children that overflow the container bounds. Keep normal in-bounds
    // items unchanged to avoid shrinking legitimate content (e.g., images taller than the parent).
    if rect.width() > 0.0 && rect.height() > 0.0 {
      let max_w = rect.width().min(self.viewport_size.width.max(0.0));
      let max_h = rect.height();
      let eps = 0.5;
      let allow_row_overflow =
        main_axis_is_row && matches!(box_node.style.flex_wrap, FlexWrap::NoWrap);
      if allow_row_overflow {
        // Preserve horizontal overflow (nowrap per spec), but avoid collapsing items at x=0.
        // Clamp only the vertical axis and translate the row rightwards if it starts negative.
        let mut min_x = f32::INFINITY;
        let mut min_y = f32::INFINITY;
        let mut max_y = f32::NEG_INFINITY;
        for child in &mut children {
          min_x = min_x.min(child.bounds.x());
          min_y = min_y.min(child.bounds.y());
          max_y = max_y.max(child.bounds.max_y());

          let x = child.bounds.x();
          let mut y = child.bounds.y();
          let mut h = child.bounds.height();
          let w = child.bounds.width();
          let overflow_y = y < -eps || y + h > max_h + eps;
          if overflow_y {
            h = h.min(max_h);
            y = y.clamp(0.0, (max_h - h).max(0.0));
            child.bounds = Rect::new(Point::new(x, y), Size::new(w, h));
          }
        }
        if min_x.is_finite() && min_x < -eps {
          let dx = -min_x;
          for child in &mut children {
            child.bounds = Rect::new(
              Point::new(child.bounds.x() + dx, child.bounds.y()),
              child.bounds.size,
            );
          }
        } else if min_x.is_finite() && min_x > max_w + eps {
          // All children start past the container's inline end; flex rows still start at the
          // line's origin even when overflow is allowed, so translate them back to x=0.
          let dx = -min_x;
          for child in &mut children {
            child.bounds = Rect::new(
              Point::new(child.bounds.x() + dx, child.bounds.y()),
              child.bounds.size,
            );
          }
        }
        if min_y.is_finite() && min_y < -eps {
          let dy = -min_y;
          for child in &mut children {
            child.bounds = Rect::new(
              Point::new(child.bounds.x(), child.bounds.y() + dy),
              child.bounds.size,
            );
          }
        }
      } else {
        let mut min_x = f32::INFINITY;
        let mut max_x = f32::NEG_INFINITY;
        let mut min_y = f32::INFINITY;
        let mut max_y = f32::NEG_INFINITY;
        for child in &mut children {
          min_x = min_x.min(child.bounds.x());
          max_x = max_x.max(child.bounds.max_x());
          min_y = min_y.min(child.bounds.y());
          max_y = max_y.max(child.bounds.max_y());

          let mut x = child.bounds.x();
          let mut y = child.bounds.y();
          let mut w = child.bounds.width();
          let mut h = child.bounds.height();
          let overflow_x = x < -eps || x + w > max_w + eps;
          let overflow_y = y < -eps || y + h > max_h + eps;
          if overflow_x {
            w = w.min(max_w);
            x = x.clamp(0.0, (max_w - w).max(0.0));
          }
          if overflow_y {
            h = h.min(max_h);
            y = y.clamp(0.0, (max_h - h).max(0.0));
          }
          if overflow_x || overflow_y {
            child.bounds = Rect::new(Point::new(x, y), Size::new(w, h));
          }
        }

        // If all children sit entirely outside the container (e.g., pushed to x>max_w or y>max_h),
        // translate them back so the left/top of the tightest bounding box starts at the container origin.
        if min_x.is_finite() && min_y.is_finite() && (min_x >= max_w - eps || min_y >= max_h - eps)
        {
          let dx = if min_x.is_finite() {
            -min_x.max(0.0)
          } else {
            0.0
          };
          let dy = if min_y.is_finite() {
            -min_y.max(0.0)
          } else {
            0.0
          };
          for child in &mut children {
            child.bounds = Rect::new(
              Point::new(child.bounds.x() + dx, child.bounds.y() + dy),
              child.bounds.size,
            );
          }
        } else {
          // If children extend far to the right but still overlap the container,
          // shift them left so the leftmost child is at 0 while preserving relative gaps.
          if min_x.is_finite() && min_x > eps {
            let dx = -min_x;
            for child in &mut children {
              child.bounds = Rect::new(
                Point::new(child.bounds.x() + dx, child.bounds.y()),
                child.bounds.size,
              );
            }
          }
        }
      }
    }

    Ok(FragmentNode::new_with_style(
      rect,
      crate::tree::fragment_tree::FragmentContent::Block {
        box_id: Some(box_node.id),
      },
      children,
      box_node.style.clone(),
    ))
  }

  /// Converts layout constraints to Taffy available space
  fn constraints_to_available_space(
    &self,
    constraints: &LayoutConstraints,
  ) -> taffy::geometry::Size<AvailableSpace> {
    taffy::geometry::Size {
      width: match constraints.available_width {
        CrateAvailableSpace::Definite(w) => AvailableSpace::Definite(w),
        CrateAvailableSpace::MinContent => AvailableSpace::MinContent,
        CrateAvailableSpace::MaxContent => AvailableSpace::MaxContent,
        CrateAvailableSpace::Indefinite => AvailableSpace::MaxContent,
      },
      height: match constraints.available_height {
        CrateAvailableSpace::Definite(h) => AvailableSpace::Definite(h),
        CrateAvailableSpace::MinContent => AvailableSpace::MinContent,
        CrateAvailableSpace::MaxContent => AvailableSpace::MaxContent,
        CrateAvailableSpace::Indefinite => AvailableSpace::MaxContent,
      },
    }
  }

  /// Converts Taffy's available space and known dimensions into this crate's constraints.
  fn constraints_from_taffy(
    &self,
    known: taffy::geometry::Size<Option<f32>>,
    available: taffy::geometry::Size<AvailableSpace>,
    inline_percentage_base: Option<f32>,
  ) -> LayoutConstraints {
    let clamp_def_width = |w: f32| w.min(self.viewport_size.width);
    let width = match (known.width, available.width) {
      (Some(w), _) => CrateAvailableSpace::Definite(clamp_def_width(w)),
      (_, AvailableSpace::Definite(w)) => {
        if w <= 1.0 {
          CrateAvailableSpace::Indefinite
        } else {
          CrateAvailableSpace::Definite(clamp_def_width(w))
        }
      }
      (_, AvailableSpace::MinContent) => CrateAvailableSpace::MinContent,
      (_, AvailableSpace::MaxContent) => CrateAvailableSpace::MaxContent,
    };
    let height = match (known.height, available.height) {
      (Some(h), _) => CrateAvailableSpace::Definite(h),
      (_, AvailableSpace::Definite(h)) => {
        if h <= 1.0 {
          CrateAvailableSpace::Indefinite
        } else {
          CrateAvailableSpace::Definite(h)
        }
      }
      (_, AvailableSpace::MinContent) => CrateAvailableSpace::MinContent,
      (_, AvailableSpace::MaxContent) => CrateAvailableSpace::MaxContent,
    };

    let mut constraints = LayoutConstraints::new(width, height);
    constraints.inline_percentage_base = constraints
      .inline_percentage_base
      .or(inline_percentage_base)
      .or(match available.width {
        AvailableSpace::Definite(w) => Some(w),
        _ => None,
      });
    constraints
  }

  fn fragment_subtree_size(fragment: &FragmentNode) -> Size {
    fn walk(node: &FragmentNode, offset: Point, min: &mut Point, max: &mut Point) {
      let origin = Point::new(node.bounds.x() + offset.x, node.bounds.y() + offset.y);
      let bounds = Rect::new(origin, node.bounds.size);
      min.x = min.x.min(bounds.x());
      min.y = min.y.min(bounds.y());
      max.x = max.x.max(bounds.max_x());
      max.y = max.y.max(bounds.max_y());
      for child in node.children.iter() {
        walk(child, origin, min, max);
      }
    }
    let mut min = Point::new(0.0, 0.0);
    let mut max = Point::new(0.0, 0.0);
    walk(fragment, Point::ZERO, &mut min, &mut max);
    Size::new((max.x - min.x).max(0.0), (max.y - min.y).max(0.0))
  }

  /// Returns the tight bounds of all descendants, excluding the root node’s own bounds.
  fn fragment_descendant_span(fragment: &FragmentNode) -> Option<Size> {
    fn walk(
      node: &FragmentNode,
      offset: Point,
      min: &mut Point,
      max: &mut Point,
      found: &mut bool,
    ) {
      for child in node.children.iter() {
        let origin = Point::new(child.bounds.x() + offset.x, child.bounds.y() + offset.y);
        let bounds = Rect::new(origin, child.bounds.size);
        *found = true;
        min.x = min.x.min(bounds.x());
        min.y = min.y.min(bounds.y());
        max.x = max.x.max(bounds.max_x());
        max.y = max.y.max(bounds.max_y());
        walk(child, origin, min, max, found);
      }
    }
    let mut min = Point::new(f32::INFINITY, f32::INFINITY);
    let mut max = Point::new(f32::NEG_INFINITY, f32::NEG_INFINITY);
    let mut found = false;
    walk(fragment, Point::ZERO, &mut min, &mut max, &mut found);
    if !found {
      None
    } else {
      Some(Size::new(
        (max.x - min.x).max(0.0),
        (max.y - min.y).max(0.0),
      ))
    }
  }

  /// Returns the content-box size for a laid-out fragment, stripping padding and borders.
  fn content_box_size(
    &self,
    fragment: &FragmentNode,
    style: &ComputedStyle,
    percentage_base: f32,
  ) -> Size {
    let padding_left = self.resolve_length_for_width(style.padding_left, percentage_base, style);
    let padding_right = self.resolve_length_for_width(style.padding_right, percentage_base, style);
    let padding_top = self.resolve_length_for_width(style.padding_top, percentage_base, style);
    let padding_bottom =
      self.resolve_length_for_width(style.padding_bottom, percentage_base, style);

    let border_left =
      self.resolve_length_for_width(style.border_left_width, percentage_base, style);
    let border_right =
      self.resolve_length_for_width(style.border_right_width, percentage_base, style);
    let border_top = self.resolve_length_for_width(style.border_top_width, percentage_base, style);
    let border_bottom =
      self.resolve_length_for_width(style.border_bottom_width, percentage_base, style);

    let content_width =
      (fragment.bounds.width() - padding_left - padding_right - border_left - border_right)
        .max(0.0);
    let content_height =
      (fragment.bounds.height() - padding_top - padding_bottom - border_top - border_bottom)
        .max(0.0);

    Size::new(content_width, content_height)
  }

  fn compute_static_positions_for_abs_children(
    &self,
    box_node: &BoxNode,
    fragment: &FragmentNode,
    in_flow_children: &[&BoxNode],
    positioned: &[PositionedCandidate],
    padding_origin: Point,
  ) -> Result<HashMap<usize, Point>, LayoutError> {
    let mut positions = HashMap::new();
    if positioned.is_empty() {
      return Ok(positions);
    }

    let mut inflow_sizes: HashMap<usize, Size> = HashMap::new();
    for child in fragment.children.iter() {
      if let Some(box_id) = match &child.content {
        FragmentContent::Block { box_id }
        | FragmentContent::Inline { box_id, .. }
        | FragmentContent::Text { box_id, .. }
        | FragmentContent::Replaced { box_id, .. } => *box_id,
        _ => None,
      } {
        inflow_sizes.insert(box_id, child.bounds.size);
      }
    }

    let mut positioned_index: HashMap<usize, usize> = HashMap::new();
    for (idx, candidate) in positioned.iter().enumerate() {
      positioned_index.insert(candidate.child.id, idx);
    }

    let mut taffy: TaffyTree<*const BoxNode> = TaffyTree::new();
    let mut child_nodes = Vec::new();
    let mut node_lookup: HashMap<usize, NodeId> = HashMap::new();

    let mut ordered_children: Vec<(i32, usize)> = Vec::new();
    for (idx, child) in box_node.children.iter().enumerate() {
      if child.style.running_position.is_some() {
        continue;
      }
      ordered_children.push((child.style.order, idx));
    }
    ordered_children.sort_by(|(a_order, a_idx), (b_order, b_idx)| {
      a_order.cmp(b_order).then_with(|| a_idx.cmp(b_idx))
    });

    for (_, child_idx) in ordered_children {
      let child = &box_node.children[child_idx];
      if let Some(&pos_idx) = positioned_index.get(&child.id) {
        let candidate = &positioned[pos_idx];
        let mut style =
          self.computed_style_to_taffy(&candidate.layout_child, false, Some(&box_node.style));
        style.size.width = Dimension::length(candidate.fragment.bounds.width().max(0.0));
        style.size.height = Dimension::length(candidate.fragment.bounds.height().max(0.0));
        let node = taffy.new_leaf(style).map_err(|e| {
          LayoutError::MissingContext(format!("Failed to create Taffy leaf: {:?}", e))
        })?;
        node_lookup.insert(candidate.child.id, node);
        child_nodes.push(node);
      } else {
        let size = inflow_sizes
          .get(&child.id)
          .cloned()
          .unwrap_or(Size::new(0.0, 0.0));
        let mut style = self.computed_style_to_taffy(child, false, Some(&box_node.style));
        style.size.width = Dimension::length(size.width.max(0.0));
        style.size.height = Dimension::length(size.height.max(0.0));
        let node = taffy.new_leaf(style).map_err(|e| {
          LayoutError::MissingContext(format!("Failed to create Taffy leaf: {:?}", e))
        })?;
        child_nodes.push(node);
      }
    }

    let mut root_style = self.computed_style_to_taffy(box_node, true, None);
    root_style.size.width = Dimension::length(fragment.bounds.width());
    root_style.size.height = Dimension::length(fragment.bounds.height());
    let root = taffy
      .new_with_children(root_style, &child_nodes)
      .map_err(|e| LayoutError::MissingContext(format!("Failed to create Taffy root: {:?}", e)))?;

    taffy
      .compute_layout(
        root,
        taffy::geometry::Size {
          width: AvailableSpace::Definite(fragment.bounds.width()),
          height: AvailableSpace::Definite(fragment.bounds.height()),
        },
      )
      .map_err(|e| LayoutError::MissingContext(format!("Taffy layout failed: {:?}", e)))?;

    for candidate in positioned {
      if let Some(node_id) = node_lookup.get(&candidate.child.id) {
        if let Ok(layout) = taffy.layout(*node_id) {
          positions.insert(
            candidate.child.id,
            Point::new(
              layout.location.x - padding_origin.x,
              layout.location.y - padding_origin.y,
            ),
          );
        }
      }
    }

    Ok(positions)
  }

  // ==========================================================================
  // Type conversion helpers
  // ==========================================================================

  fn display_to_taffy(&self, style: &ComputedStyle, is_root: bool) -> taffy::style::Display {
    // Root container is always Flex (that's why we're using FlexFormattingContext)
    // Children use their actual display mode, defaulting to Block for flex items
    if is_root {
      taffy::style::Display::Flex
    } else {
      // For children within a flex container, check if they're nested flex/grid
      match style.display {
        Display::Flex | Display::InlineFlex => taffy::style::Display::Flex,
        Display::Grid | Display::InlineGrid => taffy::style::Display::Grid,
        Display::None => taffy::style::Display::None,
        // Regular items become flex items with block-level sizing
        _ => taffy::style::Display::Block,
      }
    }
  }

  fn flex_direction_to_taffy(
    &self,
    style: &ComputedStyle,
    inline_forward_positive: bool,
    block_forward_positive: bool,
  ) -> taffy::style::FlexDirection {
    let inline_is_horizontal = matches!(style.writing_mode, WritingMode::HorizontalTb);
    let block_is_horizontal = !inline_is_horizontal;

    match style.flex_direction {
      FlexDirection::Row => {
        if inline_is_horizontal {
          if inline_forward_positive {
            taffy::style::FlexDirection::Row
          } else {
            taffy::style::FlexDirection::RowReverse
          }
        } else if inline_forward_positive {
          taffy::style::FlexDirection::Column
        } else {
          taffy::style::FlexDirection::ColumnReverse
        }
      }
      FlexDirection::RowReverse => {
        if inline_is_horizontal {
          if inline_forward_positive {
            taffy::style::FlexDirection::RowReverse
          } else {
            taffy::style::FlexDirection::Row
          }
        } else if inline_forward_positive {
          taffy::style::FlexDirection::ColumnReverse
        } else {
          taffy::style::FlexDirection::Column
        }
      }
      FlexDirection::Column => {
        if block_is_horizontal {
          if block_forward_positive {
            taffy::style::FlexDirection::Row
          } else {
            taffy::style::FlexDirection::RowReverse
          }
        } else if block_forward_positive {
          taffy::style::FlexDirection::Column
        } else {
          taffy::style::FlexDirection::ColumnReverse
        }
      }
      FlexDirection::ColumnReverse => {
        if block_is_horizontal {
          if block_forward_positive {
            taffy::style::FlexDirection::RowReverse
          } else {
            taffy::style::FlexDirection::Row
          }
        } else if block_forward_positive {
          taffy::style::FlexDirection::ColumnReverse
        } else {
          taffy::style::FlexDirection::Column
        }
      }
    }
  }

  fn flex_wrap_to_taffy(&self, wrap: FlexWrap) -> taffy::style::FlexWrap {
    match wrap {
      FlexWrap::NoWrap => taffy::style::FlexWrap::NoWrap,
      FlexWrap::Wrap => taffy::style::FlexWrap::Wrap,
      FlexWrap::WrapReverse => taffy::style::FlexWrap::WrapReverse,
    }
  }

  fn inline_axis_positive(&self, style: &ComputedStyle) -> bool {
    match style.writing_mode {
      WritingMode::HorizontalTb => style.direction != Direction::Rtl,
      WritingMode::VerticalRl
      | WritingMode::VerticalLr
      | WritingMode::SidewaysRl
      | WritingMode::SidewaysLr => true,
    }
  }

  fn block_axis_positive(&self, style: &ComputedStyle) -> bool {
    match style.writing_mode {
      WritingMode::VerticalRl | WritingMode::SidewaysRl => false,
      _ => true,
    }
  }

  fn justify_content_to_taffy(
    &self,
    justify: JustifyContent,
    axis_positive: bool,
  ) -> Option<taffy::style::JustifyContent> {
    Some(match justify {
      JustifyContent::FlexStart => {
        if axis_positive {
          taffy::style::JustifyContent::FlexStart
        } else {
          taffy::style::JustifyContent::FlexEnd
        }
      }
      JustifyContent::FlexEnd => {
        if axis_positive {
          taffy::style::JustifyContent::FlexEnd
        } else {
          taffy::style::JustifyContent::FlexStart
        }
      }
      JustifyContent::Center => taffy::style::JustifyContent::Center,
      JustifyContent::SpaceBetween => taffy::style::JustifyContent::SpaceBetween,
      JustifyContent::SpaceAround => taffy::style::JustifyContent::SpaceAround,
      JustifyContent::SpaceEvenly => taffy::style::JustifyContent::SpaceEvenly,
    })
  }

  fn align_items_to_taffy(
    &self,
    align: AlignItems,
    axis_positive: bool,
  ) -> Option<taffy::style::AlignItems> {
    Some(match align {
      AlignItems::Start | AlignItems::SelfStart => {
        if axis_positive {
          taffy::style::AlignItems::Start
        } else {
          taffy::style::AlignItems::End
        }
      }
      AlignItems::End | AlignItems::SelfEnd => {
        if axis_positive {
          taffy::style::AlignItems::End
        } else {
          taffy::style::AlignItems::Start
        }
      }
      AlignItems::FlexStart => taffy::style::AlignItems::FlexStart,
      AlignItems::FlexEnd => taffy::style::AlignItems::FlexEnd,
      AlignItems::Center => taffy::style::AlignItems::Center,
      AlignItems::Baseline => taffy::style::AlignItems::Baseline,
      AlignItems::Stretch => taffy::style::AlignItems::Stretch,
    })
  }

  fn align_self_to_taffy(
    &self,
    align: Option<AlignItems>,
    axis_positive: bool,
  ) -> Option<taffy::style::AlignItems> {
    align.and_then(|a| self.align_items_to_taffy(a, axis_positive))
  }

  fn align_content_to_taffy(
    &self,
    align: AlignContent,
    axis_positive: bool,
  ) -> Option<taffy::style::AlignContent> {
    Some(match align {
      AlignContent::FlexStart => {
        if axis_positive {
          taffy::style::AlignContent::FlexStart
        } else {
          taffy::style::AlignContent::FlexEnd
        }
      }
      AlignContent::FlexEnd => {
        if axis_positive {
          taffy::style::AlignContent::FlexEnd
        } else {
          taffy::style::AlignContent::FlexStart
        }
      }
      AlignContent::Center => taffy::style::AlignContent::Center,
      AlignContent::SpaceBetween => taffy::style::AlignContent::SpaceBetween,
      AlignContent::SpaceEvenly => taffy::style::AlignContent::SpaceEvenly,
      AlignContent::SpaceAround => taffy::style::AlignContent::SpaceAround,
      AlignContent::Stretch => taffy::style::AlignContent::Stretch,
    })
  }

  fn flex_basis_to_taffy(&self, basis: &FlexBasis, style: &ComputedStyle) -> Dimension {
    match basis {
      FlexBasis::Auto => Dimension::auto(),
      FlexBasis::Length(len) => self.length_to_dimension(len, style),
    }
  }

  fn horizontal_edges_px(&self, style: &ComputedStyle) -> Option<f32> {
    let left = self.resolve_length_px(&style.padding_left, style)?;
    let right = self.resolve_length_px(&style.padding_right, style)?;
    let bl = self.resolve_length_px(&style.border_left_width, style)?;
    let br = self.resolve_length_px(&style.border_right_width, style)?;
    Some(left + right + bl + br)
  }

  fn vertical_edges_px(&self, style: &ComputedStyle) -> Option<f32> {
    let top = self.resolve_length_px(&style.padding_top, style)?;
    let bottom = self.resolve_length_px(&style.padding_bottom, style)?;
    let bt = self.resolve_length_px(&style.border_top_width, style)?;
    let bb = self.resolve_length_px(&style.border_bottom_width, style)?;
    Some(top + bottom + bt + bb)
  }

  fn resolve_length_px(&self, len: &Length, style: &ComputedStyle) -> Option<f32> {
    match len.unit {
      LengthUnit::Percent => None,
      _ if len.unit.is_absolute() => Some(len.to_px()),
      u if u.is_viewport_relative() => {
        len.resolve_with_viewport(self.viewport_size.width, self.viewport_size.height)
      }
      LengthUnit::Rem => Some(len.value * style.root_font_size),
      LengthUnit::Em => Some(len.value * style.font_size),
      LengthUnit::Ex => Some(len.value * style.font_size * 0.5),
      LengthUnit::Ch => Some(len.value * style.font_size * 0.5),
      _ => None,
    }
  }

  fn dimension_for_box_sizing(&self, len: &Length, style: &ComputedStyle, axis: Axis) -> Dimension {
    if style.box_sizing == BoxSizing::ContentBox {
      if let Some(edges) = match axis {
        Axis::Horizontal => self.horizontal_edges_px(style),
        Axis::Vertical => self.vertical_edges_px(style),
      } {
        if let Some(px) = self.resolve_length_px(len, style) {
          return Dimension::length((px + edges).max(0.0));
        }
      }
    }
    self.length_to_dimension(len, style)
  }

  fn length_to_dimension(&self, len: &Length, style: &ComputedStyle) -> Dimension {
    match len.unit {
      LengthUnit::Px => Dimension::length(len.to_px()),
      LengthUnit::Percent => Dimension::percent(len.value / 100.0),
      _ => {
        if let Some(px) = self.resolve_length_px(len, style) {
          Dimension::length(px)
        } else {
          Dimension::length(len.to_px())
        }
      }
    }
  }

  fn length_option_to_dimension_box_sizing(
    &self,
    len: Option<&Length>,
    style: &ComputedStyle,
    axis: Axis,
  ) -> Dimension {
    match len {
      Some(l) => self.dimension_for_box_sizing(l, style, axis),
      None => Dimension::auto(),
    }
  }

  #[allow(dead_code)]
  fn length_option_to_dimension(&self, len: Option<&Length>, style: &ComputedStyle) -> Dimension {
    match len {
      Some(l) => self.length_to_dimension(l, style),
      None => Dimension::auto(),
    }
  }

  fn length_to_taffy_lp(&self, len: &Length, style: &ComputedStyle) -> LengthPercentage {
    match len.unit {
      LengthUnit::Percent => LengthPercentage::percent(len.value / 100.0),
      _ => {
        if let Some(px) = self.resolve_length_px(len, style) {
          LengthPercentage::length(px)
        } else {
          LengthPercentage::length(len.to_px())
        }
      }
    }
  }

  fn length_option_to_lpa(
    &self,
    len: Option<&Length>,
    style: &ComputedStyle,
  ) -> LengthPercentageAuto {
    match len {
      Some(l) => match l.unit {
        LengthUnit::Percent => LengthPercentageAuto::percent(l.value / 100.0),
        _ => {
          if let Some(px) = self.resolve_length_px(l, style) {
            LengthPercentageAuto::length(px)
          } else {
            LengthPercentageAuto::length(l.to_px())
          }
        }
      },
      None => LengthPercentageAuto::auto(),
    }
  }

  fn aspect_ratio_to_taffy(&self, aspect_ratio: AspectRatio) -> Option<f32> {
    match aspect_ratio {
      AspectRatio::Auto => None,
      AspectRatio::Ratio(ratio) => Some(ratio),
    }
  }

  fn resolve_length_for_width(
    &self,
    length: Length,
    percentage_base: f32,
    style: &ComputedStyle,
  ) -> f32 {
    let base = if percentage_base.is_finite() {
      Some(percentage_base)
    } else {
      None
    };
    resolve_length_with_percentage_metrics(
      length,
      base,
      self.viewport_size,
      style.font_size,
      style.root_font_size,
      Some(style),
      Some(&self.font_context),
    )
    .unwrap_or(0.0)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::style::display::Display;
  use crate::style::display::FormattingContextType;
  use crate::style::position::Position;
  use crate::style::types::AlignItems;
  use crate::style::types::AspectRatio;
  use crate::style::types::FlexWrap;
  use crate::style::types::Overflow;
  use crate::style::types::ScrollbarWidth;
  use crate::style::values::Length;
  use crate::style::values::LengthOrAuto;
  use crate::tree::box_tree::ReplacedType;
  use crate::tree::debug::{track_to_selector_calls, DebugInfo};
  use std::sync::Arc;

  fn create_flex_style() -> Arc<ComputedStyle> {
    let mut style = ComputedStyle::default();
    style.display = Display::Flex;
    style.flex_direction = FlexDirection::Row;
    Arc::new(style)
  }

  fn create_item_style(width: f32, height: f32) -> Arc<ComputedStyle> {
    let mut style = ComputedStyle::default();
    style.width = Some(Length::px(width));
    style.height = Some(Length::px(height));
    Arc::new(style)
  }

  fn create_item_style_with_grow(width: f32, height: f32, grow: f32) -> Arc<ComputedStyle> {
    let mut style = ComputedStyle::default();
    style.width = Some(Length::px(width));
    style.height = Some(Length::px(height));
    style.flex_grow = grow;
    Arc::new(style)
  }

  fn baseline_position(fragment: &FragmentNode) -> f32 {
    let offset = fragment_first_baseline(fragment).expect("fragment has no baseline");
    fragment.bounds.y() + offset
  }

  #[test]
  fn taffy_style_maps_overflow_and_scrollbar_width() {
    let mut style = ComputedStyle::default();
    style.display = Display::Flex;
    style.overflow_x = Overflow::Scroll;
    style.overflow_y = Overflow::Hidden;
    style.scrollbar_width = ScrollbarWidth::Thin;

    let node = BoxNode::new_block(Arc::new(style), FormattingContextType::Flex, vec![]);
    let fc = FlexFormattingContext::new();
    let taffy_style = fc.computed_style_to_taffy(&node, true, None);

    assert_eq!(
      taffy_style.scrollbar_width,
      resolve_scrollbar_width(&node.style)
    );
    assert_eq!(taffy_style.overflow.x, TaffyOverflow::Scroll);
    assert_eq!(taffy_style.overflow.y, TaffyOverflow::Hidden);
  }

  #[test]
  fn flex_style_fingerprint_accounts_for_scrollbar_and_overflow() {
    let mut style_a = ComputedStyle::default();
    style_a.display = Display::Flex;
    style_a.overflow_x = Overflow::Hidden;
    style_a.scrollbar_width = ScrollbarWidth::Auto;

    let mut style_b = style_a.clone();
    style_b.scrollbar_width = ScrollbarWidth::Thin;

    let mut style_c = style_a.clone();
    style_c.overflow_x = Overflow::Scroll;

    let fp_a = super::flex_style_fingerprint(&style_a);
    let fp_b = super::flex_style_fingerprint(&style_b);
    let fp_c = super::flex_style_fingerprint(&style_c);

    assert_ne!(
      fp_a, fp_b,
      "scrollbar width should affect flex style fingerprint"
    );
    assert_ne!(fp_a, fp_c, "overflow should affect flex style fingerprint");
  }

  #[test]
  fn flex_cache_key_distinguishes_anonymous_boxes() {
    let style = Arc::new(ComputedStyle::default());

    let mut a = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![]);
    let mut b = BoxNode::new_block(style, FormattingContextType::Block, vec![]);
    a.id = 1;
    b.id = 2;
    a.styled_node_id = None;
    b.styled_node_id = None;
    a.debug_info = None;
    b.debug_info = None;

    assert_ne!(
      super::flex_cache_key(&a),
      super::flex_cache_key(&b),
      "anonymous boxes must not share flex cache keys"
    );
  }

  #[test]
  fn flex_cache_key_avoids_selector_allocations() {
    let style = Arc::new(ComputedStyle::default());
    let node = BoxNode::new_block(style, FormattingContextType::Block, vec![]).with_debug_info(
      DebugInfo::new(
        Some("div".to_string()),
        Some("carousel".to_string()),
        vec!["item".to_string(), "active".to_string()],
      ),
    );
    let (_key, selector_calls) = track_to_selector_calls(|| super::flex_cache_key(&node));
    assert_eq!(
      selector_calls, 0,
      "flex cache key should not format debug selectors"
    );
  }

  #[test]
  fn flex_does_not_shrink_below_min_width_when_overflowing() {
    let fc = FlexFormattingContext::new();

    let mut container_style = ComputedStyle::default();
    container_style.display = Display::Flex;
    container_style.flex_direction = FlexDirection::Row;
    container_style.flex_wrap = FlexWrap::NoWrap;
    container_style.overflow_x = Overflow::Scroll;
    container_style.width = Some(Length::px(400.0));

    let mut items = Vec::new();
    for _ in 0..5 {
      let mut item_style = ComputedStyle::default();
      item_style.width = Some(Length::px(300.0));
      item_style.height = Some(Length::px(50.0));
      item_style.min_width = Some(Length::px(152.0));
      item_style.flex_shrink = 1.0;
      items.push(BoxNode::new_block(
        Arc::new(item_style),
        FormattingContextType::Block,
        vec![],
      ));
    }

    let container = BoxNode::new_block(
      Arc::new(container_style),
      FormattingContextType::Flex,
      items,
    );
    let fragment = fc
      .layout(&container, &LayoutConstraints::definite(400.0, 200.0))
      .unwrap();

    for (idx, child) in fragment.children.iter().enumerate() {
      assert!(
        child.bounds.width() >= 151.9,
        "child {idx} shrank below min-width: {:.2}",
        child.bounds.width()
      );
    }
  }

  #[test]
  fn measure_cache_coerces_tiny_definite_to_max_content_key() {
    use crate::geometry::Size as GeoSize;
    use taffy::style::AvailableSpace;

    let viewport = GeoSize::new(1200.0, 800.0);
    let tiny_known = taffy::geometry::Size {
      width: Some(0.5),
      height: None,
    };
    let tiny_avail = taffy::geometry::Size {
      width: AvailableSpace::Definite(0.5),
      height: AvailableSpace::Definite(100.0),
    };
    let tiny_key = super::measure_cache_key(&tiny_known, &tiny_avail, viewport, false);

    let max_key = super::measure_cache_key(
      &taffy::geometry::Size {
        width: None,
        height: None,
      },
      &taffy::geometry::Size {
        width: AvailableSpace::MaxContent,
        height: AvailableSpace::Definite(100.0),
      },
      viewport,
      false,
    );

    assert_eq!(tiny_key.0, max_key.0);
  }

  #[test]
  fn measure_cache_quantizes_definite_available_sizes() {
    use crate::geometry::Size as GeoSize;
    use taffy::style::AvailableSpace;

    let viewport = GeoSize::new(1200.0, 800.0);
    let known = taffy::geometry::Size {
      width: None,
      height: None,
    };

    let key_a = super::measure_cache_key(
      &known,
      &taffy::geometry::Size {
        width: AvailableSpace::Definite(300.3),
        height: AvailableSpace::Definite(150.7),
      },
      viewport,
      false,
    );

    let key_b = super::measure_cache_key(
      &known,
      &taffy::geometry::Size {
        width: AvailableSpace::Definite(300.6),
        height: AvailableSpace::Definite(150.4),
      },
      viewport,
      false,
    );

    assert_eq!(
      key_a, key_b,
      "quantized definite availables should reuse the same cache key"
    );

    // Also ensure a tiny tolerance exists so very similar targets can merge.
    let key_c = super::measure_cache_key(
      &known,
      &taffy::geometry::Size {
        width: AvailableSpace::Definite(300.0),
        height: AvailableSpace::Definite(151.0),
      },
      viewport,
      false,
    );
    assert_eq!(key_a.0, key_c.0);
  }

  #[test]
  fn measured_fragments_normalize_and_reuse_fragments() {
    let measured_fragments = Arc::new(ShardedFlexCache::new_measure());
    let layout_fragments = Arc::new(ShardedFlexCache::new_layout());
    let viewport = Size::new(200.0, 200.0);
    let mut fc = FlexFormattingContext::with_viewport_and_cb(
      viewport,
      ContainingBlock::viewport(viewport),
      FontContext::new(),
      measured_fragments.clone(),
      layout_fragments.clone(),
    );

    let mut child_style = ComputedStyle::default();
    child_style.display = Display::Block;
    child_style.position = Position::Relative;
    child_style.left = LengthOrAuto::px(9.0);
    child_style.top = LengthOrAuto::px(11.0);
    child_style.width = Some(Length::px(40.0));
    child_style.height = Some(Length::px(20.0));
    let grandchild = BoxNode::new_block(
      Arc::new(ComputedStyle::default()),
      FormattingContextType::Block,
      vec![],
    );
    let mut child = BoxNode::new_block(
      Arc::new(child_style),
      FormattingContextType::Block,
      vec![grandchild],
    );
    child.id = 2;

    let mut container_style = ComputedStyle::default();
    container_style.display = Display::Flex;
    container_style.flex_direction = FlexDirection::Row;
    container_style.width = Some(Length::px(120.0));
    container_style.height = Some(Length::px(60.0));
    let mut container = BoxNode::new_block(
      Arc::new(container_style),
      FormattingContextType::Flex,
      vec![child.clone()],
    );
    container.id = 1;

    let constraints = LayoutConstraints::definite(120.0, 60.0);
    let first_fragment = fc.layout(&container, &constraints).unwrap();
    let first_child = &first_fragment.children[0];
    assert!(
      first_child.bounds.x() != 0.0 || first_child.bounds.y() != 0.0,
      "relative positioning should offset the measured fragment"
    );
    let expected_origin = first_child.bounds.origin;

    let cache_key = flex_cache_key(&child);
    let cached = measured_fragments.find_fragment(
      cache_key,
      Size::new(first_child.bounds.width(), first_child.bounds.height()),
    );
    let (_stored_size, cached_fragment) = cached.expect("child fragment cached");
    assert_eq!(cached_fragment.bounds.origin, Point::new(0.0, 0.0));

    let shard_hits_before: u64 = measured_fragments
      .shard_stats()
      .into_iter()
      .map(|s| s.hits)
      .sum();

    // Avoid layout cache hits so reuse flows through the measured fragment cache.
    layout_fragments.clear();

    let second_fragment = fc.layout(&container, &constraints).unwrap();
    let second_child = &second_fragment.children[0];
    let shard_hits_after: u64 = measured_fragments
      .shard_stats()
      .into_iter()
      .map(|s| s.hits)
      .sum();

    assert!(
      shard_hits_after > shard_hits_before,
      "measurement cache should be hit on reuse"
    );
    assert_eq!(second_child.bounds.origin, expected_origin);
  }

  #[test]
  fn test_flex_context_creation() {
    let _fc = FlexFormattingContext::new();
    let _fc_default = FlexFormattingContext::default();
    // Both methods should create valid contexts
    // (PhantomData<()> is zero-sized, so we just verify creation works)
  }

  #[test]
  fn absolute_child_is_positioned_against_flex_padding_box() {
    let mut container_style = ComputedStyle::default();
    container_style.display = Display::Flex;
    container_style.position = Position::Relative;
    container_style.padding_left = Length::px(10.0);
    container_style.padding_top = Length::px(8.0);
    container_style.padding_right = Length::px(10.0);
    container_style.padding_bottom = Length::px(8.0);

    let mut abs_style = ComputedStyle::default();
    abs_style.display = Display::Block;
    abs_style.position = Position::Absolute;
    abs_style.left = Some(Length::px(5.0));
    abs_style.top = Some(Length::px(7.0));
    abs_style.width = Some(Length::px(20.0));
    abs_style.height = Some(Length::px(10.0));

    let abs_child = BoxNode::new_block(Arc::new(abs_style), FormattingContextType::Block, vec![]);
    let container = BoxNode::new_block(
      Arc::new(container_style),
      FormattingContextType::Flex,
      vec![abs_child],
    );

    let fc = FlexFormattingContext::with_viewport(Size::new(200.0, 200.0));
    let constraints = LayoutConstraints::definite(100.0, 100.0);
    let fragment = fc.layout(&container, &constraints).unwrap();

    assert_eq!(fragment.children.len(), 1);
    let abs_fragment = &fragment.children[0];
    assert_eq!(abs_fragment.bounds.x(), 15.0);
    assert_eq!(abs_fragment.bounds.y(), 15.0);
    assert_eq!(abs_fragment.bounds.width(), 20.0);
    assert_eq!(abs_fragment.bounds.height(), 10.0);
  }

  #[test]
  fn absolute_child_inherits_positioned_containing_block_from_ancestor() {
    let mut container_style = ComputedStyle::default();
    container_style.display = Display::Flex;
    container_style.position = Position::Static;

    let mut abs_style = ComputedStyle::default();
    abs_style.display = Display::Block;
    abs_style.position = Position::Absolute;
    abs_style.left = Some(Length::px(5.0));
    abs_style.top = Some(Length::px(7.0));
    abs_style.width = Some(Length::px(10.0));
    abs_style.height = Some(Length::px(6.0));

    let abs_child = BoxNode::new_block(Arc::new(abs_style), FormattingContextType::Block, vec![]);
    let container = BoxNode::new_block(
      Arc::new(container_style),
      FormattingContextType::Flex,
      vec![abs_child],
    );

    let cb_rect = Rect::from_xywh(20.0, 30.0, 150.0, 150.0);
    let viewport = Size::new(300.0, 300.0);
    let cb = ContainingBlock::with_viewport(cb_rect, viewport);
    let fc = FlexFormattingContext::with_viewport_and_cb(
      viewport,
      cb,
      FontContext::new(),
      std::sync::Arc::new(ShardedFlexCache::new_measure()),
      std::sync::Arc::new(ShardedFlexCache::new_layout()),
    );
    let constraints = LayoutConstraints::definite(100.0, 100.0);
    let fragment = fc.layout(&container, &constraints).unwrap();

    assert_eq!(fragment.children.len(), 1);
    let abs_fragment = &fragment.children[0];
    assert_eq!(abs_fragment.bounds.x(), 25.0);
    assert_eq!(abs_fragment.bounds.y(), 37.0);
  }

  #[test]
  fn test_basic_flex_row_layout() {
    let fc = FlexFormattingContext::new();

    // Create flex container with 3 children
    let item1 = BoxNode::new_block(
      create_item_style(100.0, 50.0),
      FormattingContextType::Block,
      vec![],
    );
    let item2 = BoxNode::new_block(
      create_item_style(100.0, 50.0),
      FormattingContextType::Block,
      vec![],
    );
    let item3 = BoxNode::new_block(
      create_item_style(100.0, 50.0),
      FormattingContextType::Block,
      vec![],
    );

    let container = BoxNode::new_block(
      create_flex_style(),
      FormattingContextType::Flex,
      vec![item1, item2, item3],
    );

    let constraints = LayoutConstraints::definite(400.0, 600.0);
    let fragment = fc.layout(&container, &constraints).unwrap();

    // Check that children are laid out horizontally
    assert_eq!(fragment.children.len(), 3);

    // Items should be positioned at x=0, 100, 200
    assert_eq!(fragment.children[0].bounds.x(), 0.0);
    assert_eq!(fragment.children[1].bounds.x(), 100.0);
    assert_eq!(fragment.children[2].bounds.x(), 200.0);

    // All items should have same y position
    assert_eq!(fragment.children[0].bounds.y(), 0.0);
    assert_eq!(fragment.children[1].bounds.y(), 0.0);
    assert_eq!(fragment.children[2].bounds.y(), 0.0);
  }

  #[test]
  fn test_flex_column_layout() {
    let fc = FlexFormattingContext::new();

    let mut container_style = ComputedStyle::default();
    container_style.display = Display::Flex;
    container_style.flex_direction = FlexDirection::Column;

    let item1 = BoxNode::new_block(
      create_item_style(100.0, 50.0),
      FormattingContextType::Block,
      vec![],
    );
    let item2 = BoxNode::new_block(
      create_item_style(100.0, 75.0),
      FormattingContextType::Block,
      vec![],
    );
    let item3 = BoxNode::new_block(
      create_item_style(100.0, 25.0),
      FormattingContextType::Block,
      vec![],
    );

    let container = BoxNode::new_block(
      Arc::new(container_style),
      FormattingContextType::Flex,
      vec![item1, item2, item3],
    );

    let constraints = LayoutConstraints::definite(400.0, 600.0);
    let fragment = fc.layout(&container, &constraints).unwrap();

    // Check that children are laid out vertically
    assert_eq!(fragment.children.len(), 3);

    // Items should be positioned at y=0, 50, 125
    assert_eq!(fragment.children[0].bounds.y(), 0.0);
    assert_eq!(fragment.children[1].bounds.y(), 50.0);
    assert_eq!(fragment.children[2].bounds.y(), 125.0);

    // All items should have same x position
    assert_eq!(fragment.children[0].bounds.x(), 0.0);
    assert_eq!(fragment.children[1].bounds.x(), 0.0);
    assert_eq!(fragment.children[2].bounds.x(), 0.0);
  }

  #[test]
  fn test_flex_grow() {
    let fc = FlexFormattingContext::new();

    // Two items: one with flex-grow: 1, one without
    let item1 = BoxNode::new_block(
      create_item_style_with_grow(100.0, 50.0, 1.0),
      FormattingContextType::Block,
      vec![],
    );
    let item2 = BoxNode::new_block(
      create_item_style(100.0, 50.0),
      FormattingContextType::Block,
      vec![],
    );

    let container = BoxNode::new_block(
      create_flex_style(),
      FormattingContextType::Flex,
      vec![item1, item2],
    );

    let constraints = LayoutConstraints::definite(400.0, 600.0);
    let fragment = fc.layout(&container, &constraints).unwrap();

    // First item should grow to fill available space: base widths are both 100, one grows with flex-grow:1.
    // Remaining space is distributed according to flex-grow factors, so item1 ends up at 300 and item2 at 100.
    assert_eq!(fragment.children[0].bounds.width(), 300.0);
    assert_eq!(fragment.children[1].bounds.width(), 100.0);
  }

  #[test]
  fn test_flex_shrink() {
    let fc = FlexFormattingContext::new();

    // Two items, total wider than container
    let item1 = BoxNode::new_block(
      create_item_style(250.0, 50.0),
      FormattingContextType::Block,
      vec![],
    );
    let item2 = BoxNode::new_block(
      create_item_style(250.0, 50.0),
      FormattingContextType::Block,
      vec![],
    );

    let container = BoxNode::new_block(
      create_flex_style(),
      FormattingContextType::Flex,
      vec![item1, item2],
    );

    // Container only 400px wide, but items total 500px
    let constraints = LayoutConstraints::definite(400.0, 600.0);
    let fragment = fc.layout(&container, &constraints).unwrap();

    // Items should shrink equally (default flex-shrink: 1)
    // Deficit: 500 - 400 = 100
    // Each shrinks by 50
    assert_eq!(fragment.children[0].bounds.width(), 200.0);
    assert_eq!(fragment.children[1].bounds.width(), 200.0);
  }

  #[test]
  fn test_justify_content_space_between() {
    let fc = FlexFormattingContext::new();

    let mut container_style = ComputedStyle::default();
    container_style.display = Display::Flex;
    container_style.flex_direction = FlexDirection::Row;
    container_style.justify_content = JustifyContent::SpaceBetween;

    let item1 = BoxNode::new_block(
      create_item_style(100.0, 50.0),
      FormattingContextType::Block,
      vec![],
    );
    let item2 = BoxNode::new_block(
      create_item_style(100.0, 50.0),
      FormattingContextType::Block,
      vec![],
    );
    let item3 = BoxNode::new_block(
      create_item_style(100.0, 50.0),
      FormattingContextType::Block,
      vec![],
    );

    let container = BoxNode::new_block(
      Arc::new(container_style),
      FormattingContextType::Flex,
      vec![item1, item2, item3],
    );

    let constraints = LayoutConstraints::definite(500.0, 600.0);
    let fragment = fc.layout(&container, &constraints).unwrap();

    // Space between: first at start, last at end, equal spacing between
    // Items: 100 + 100 + 100 = 300
    // Container: 500
    // Space: 200
    // Gaps: 2 (between 3 items)
    // Gap size: 100
    assert_eq!(fragment.children[0].bounds.x(), 0.0);
    assert_eq!(fragment.children[1].bounds.x(), 200.0); // 100 + 100 gap
    assert_eq!(fragment.children[2].bounds.x(), 400.0); // 200 + 100 width + 100 gap
  }

  #[test]
  fn test_align_items_center() {
    let fc = FlexFormattingContext::new();

    let mut container_style = ComputedStyle::default();
    container_style.display = Display::Flex;
    container_style.flex_direction = FlexDirection::Row;
    container_style.align_items = AlignItems::Center;
    container_style.height = Some(Length::px(100.0));

    // Different height items
    let item1 = BoxNode::new_block(
      create_item_style(100.0, 50.0),
      FormattingContextType::Block,
      vec![],
    );
    let item2 = BoxNode::new_block(
      create_item_style(100.0, 100.0),
      FormattingContextType::Block,
      vec![],
    );
    let item3 = BoxNode::new_block(
      create_item_style(100.0, 74.0),
      FormattingContextType::Block,
      vec![],
    );

    let container = BoxNode::new_block(
      Arc::new(container_style),
      FormattingContextType::Flex,
      vec![item1, item2, item3],
    );

    let constraints = LayoutConstraints::definite(400.0, 200.0);
    let fragment = fc.layout(&container, &constraints).unwrap();

    // Container height is 100px, items should be vertically centered
    // Taffy may round to integer pixels, so we check approximate values
    assert_eq!(fragment.children[0].bounds.y(), 25.0); // (100 - 50) / 2
    assert_eq!(fragment.children[1].bounds.y(), 0.0); // Tallest, at top
    assert_eq!(fragment.children[2].bounds.y(), 13.0); // (100 - 74) / 2 = 13
  }

  #[test]
  fn flex_align_items_baseline_aligns_text() {
    let fc = FlexFormattingContext::new();

    let mut container_style = ComputedStyle::default();
    container_style.display = Display::Flex;
    container_style.flex_direction = FlexDirection::Row;
    container_style.align_items = AlignItems::Baseline;
    container_style.height = Some(Length::px(80.0));

    let mut small_text_style = ComputedStyle::default();
    small_text_style.font_size = 12.0;
    let small_text_style = Arc::new(small_text_style);
    let small_text = BoxNode::new_text(small_text_style.clone(), "small".to_string());
    let small_inline = BoxNode::new_inline(small_text_style.clone(), vec![small_text]);
    let small_item = BoxNode::new_block(
      Arc::new(ComputedStyle::default()),
      FormattingContextType::Block,
      vec![small_inline],
    );

    let mut large_text_style = ComputedStyle::default();
    large_text_style.font_size = 24.0;
    let large_text_style = Arc::new(large_text_style);
    let large_text = BoxNode::new_text(large_text_style.clone(), "Large".to_string());
    let large_inline = BoxNode::new_inline(large_text_style.clone(), vec![large_text]);
    let large_item = BoxNode::new_block(
      Arc::new(ComputedStyle::default()),
      FormattingContextType::Block,
      vec![large_inline],
    );

    let container = BoxNode::new_block(
      Arc::new(container_style),
      FormattingContextType::Flex,
      vec![small_item, large_item],
    );

    let constraints = LayoutConstraints::definite(200.0, 200.0);
    let fragment = fc.layout(&container, &constraints).unwrap();

    let small_baseline = baseline_position(&fragment.children[0]);
    let large_baseline = baseline_position(&fragment.children[1]);
    assert!(
      (small_baseline - large_baseline).abs() < 0.5,
      "baselines misaligned: {:.2} vs {:.2}",
      small_baseline,
      large_baseline
    );
  }

  #[test]
  fn flex_align_items_baseline_handles_replaced_fallback() {
    let fc = FlexFormattingContext::new();

    let mut container_style = ComputedStyle::default();
    container_style.display = Display::Flex;
    container_style.flex_direction = FlexDirection::Row;
    container_style.align_items = AlignItems::Baseline;

    let mut text_style = ComputedStyle::default();
    text_style.font_size = 16.0;
    let text_style = Arc::new(text_style);
    let text = BoxNode::new_text(text_style.clone(), "Text".to_string());
    let inline = BoxNode::new_inline(text_style.clone(), vec![text]);
    let text_item = BoxNode::new_block(
      Arc::new(ComputedStyle::default()),
      FormattingContextType::Block,
      vec![inline],
    );

    let mut replaced_style = ComputedStyle::default();
    replaced_style.width = Some(Length::px(20.0));
    replaced_style.height = Some(Length::px(10.0));
    let replaced =
      BoxNode::new_replaced(Arc::new(replaced_style), ReplacedType::Canvas, None, None);

    let container = BoxNode::new_block(
      Arc::new(container_style),
      FormattingContextType::Flex,
      vec![text_item, replaced],
    );

    let constraints = LayoutConstraints::definite(200.0, 100.0);
    let fragment = fc.layout(&container, &constraints).unwrap();

    let text_baseline = baseline_position(&fragment.children[0]);
    let replaced_baseline = baseline_position(&fragment.children[1]);
    assert!(
      (text_baseline - replaced_baseline).abs() < 0.5,
      "replaced baseline not aligned: {:.2} vs {:.2}",
      text_baseline,
      replaced_baseline
    );
  }

  #[test]
  fn flex_baseline_alignment_is_per_line() {
    let fc = FlexFormattingContext::new();

    let mut container_style = ComputedStyle::default();
    container_style.display = Display::Flex;
    container_style.flex_direction = FlexDirection::Row;
    container_style.align_items = AlignItems::Baseline;
    container_style.flex_wrap = FlexWrap::Wrap;
    container_style.width = Some(Length::px(120.0));

    let make_item = |font_size: f32, width: f32| {
      let mut text_style = ComputedStyle::default();
      text_style.font_size = font_size;
      let text_style = Arc::new(text_style);
      let text = BoxNode::new_text(text_style.clone(), "Wrap".to_string());
      let inline = BoxNode::new_inline(text_style.clone(), vec![text]);
      let mut item_style = ComputedStyle::default();
      item_style.width = Some(Length::px(width));
      BoxNode::new_block(
        Arc::new(item_style),
        FormattingContextType::Block,
        vec![inline],
      )
    };

    let item1 = make_item(12.0, 60.0);
    let item2 = make_item(18.0, 50.0);
    let item3 = make_item(16.0, 80.0);

    let container = BoxNode::new_block(
      Arc::new(container_style),
      FormattingContextType::Flex,
      vec![item1, item2, item3],
    );

    let constraints = LayoutConstraints::definite(200.0, 200.0);
    let fragment = fc.layout(&container, &constraints).unwrap();

    assert!(
      fragment.children.len() == 3,
      "expected three flex items, got {}",
      fragment.children.len()
    );
    let line1_first = baseline_position(&fragment.children[0]);
    let line1_second = baseline_position(&fragment.children[1]);
    assert!(
      (line1_first - line1_second).abs() < 0.5,
      "first line baselines differ: {:.2} vs {:.2}",
      line1_first,
      line1_second
    );

    let line2_baseline = baseline_position(&fragment.children[2]);
    assert!(
      (line2_baseline - line1_first).abs() > 0.5,
      "baselines leaked across lines: {:.2} vs {:.2}",
      line2_baseline,
      line1_first
    );
    assert!(
      fragment.children[2].bounds.y() > fragment.children[0].bounds.y(),
      "wrapped item should appear on a new line"
    );
  }

  #[test]
  fn flex_align_self_overrides_parent_align_items() {
    let fc = FlexFormattingContext::new();

    let mut container_style = ComputedStyle::default();
    container_style.display = Display::Flex;
    container_style.flex_direction = FlexDirection::Row;
    container_style.align_items = AlignItems::Center;
    container_style.height = Some(Length::px(100.0));

    let mut item_style = ComputedStyle::default();
    item_style.height = Some(Length::px(20.0));
    item_style.align_self = Some(AlignItems::FlexEnd);

    let item = BoxNode::new_block(Arc::new(item_style), FormattingContextType::Block, vec![]);

    let container = BoxNode::new_block(
      Arc::new(container_style),
      FormattingContextType::Flex,
      vec![item],
    );

    let constraints = LayoutConstraints::definite(100.0, 100.0);
    let fragment = fc.layout(&container, &constraints).unwrap();

    // Parent would center to y=40; align-self:end should place it at y=80
    assert_eq!(fragment.children[0].bounds.y(), 80.0);
  }

  #[test]
  fn writing_mode_vertical_treats_row_as_column() {
    let fc = FlexFormattingContext::new();

    let mut style = ComputedStyle::default();
    style.display = Display::Flex;
    style.flex_direction = FlexDirection::Row;
    style.writing_mode = crate::style::types::WritingMode::VerticalRl;

    let child1 = BoxNode::new_block(
      create_item_style(20.0, 10.0),
      FormattingContextType::Block,
      vec![],
    );
    let child2 = BoxNode::new_block(
      create_item_style(20.0, 10.0),
      FormattingContextType::Block,
      vec![],
    );

    let container = BoxNode::new_block(
      Arc::new(style),
      FormattingContextType::Flex,
      vec![child1, child2],
    );
    let constraints = LayoutConstraints::definite(100.0, 100.0);
    let fragment = fc.layout(&container, &constraints).unwrap();

    assert_eq!(fragment.children[0].bounds.y(), 0.0);
    assert_eq!(fragment.children[1].bounds.y(), 10.0);
  }

  #[test]
  fn writing_mode_vertical_align_start_maps_to_block_start() {
    let fc = FlexFormattingContext::new();

    let mut style = ComputedStyle::default();
    style.display = Display::Flex;
    style.flex_direction = FlexDirection::Row;
    style.writing_mode = crate::style::types::WritingMode::VerticalRl;
    style.align_items = AlignItems::Start;
    style.width = Some(Length::px(100.0));

    let child = BoxNode::new_block(
      create_item_style(20.0, 10.0),
      FormattingContextType::Block,
      vec![],
    );
    let container = BoxNode::new_block(Arc::new(style), FormattingContextType::Flex, vec![child]);
    let constraints = LayoutConstraints::definite(100.0, 100.0);
    let fragment = fc.layout(&container, &constraints).unwrap();

    // Block axis start for vertical-rl is the right edge, so x should be at 80
    assert_eq!(fragment.children[0].bounds.x(), 80.0);
  }

  #[test]
  fn writing_mode_vertical_row_justify_start_and_end_follow_inline_axis() {
    let fc = FlexFormattingContext::new();

    let mut style = ComputedStyle::default();
    style.display = Display::Flex;
    style.flex_direction = FlexDirection::Row; // inline axis (vertical in vertical-rl)
    style.writing_mode = crate::style::types::WritingMode::VerticalRl;
    style.justify_content = JustifyContent::FlexStart;
    style.height = Some(Length::px(100.0));

    let mut end_style = style.clone();
    end_style.justify_content = JustifyContent::FlexEnd;

    let child1 = BoxNode::new_block(
      create_item_style(10.0, 10.0),
      FormattingContextType::Block,
      vec![],
    );
    let child2 = BoxNode::new_block(
      create_item_style(10.0, 10.0),
      FormattingContextType::Block,
      vec![],
    );

    let container = BoxNode::new_block(
      Arc::new(style),
      FormattingContextType::Flex,
      vec![child1, child2],
    );

    let constraints = LayoutConstraints::definite(100.0, 100.0);
    let fragment = fc.layout(&container, &constraints).unwrap();

    // Inline axis is vertical; flex-start packs at the top.
    assert_eq!(fragment.children[0].bounds.y(), 0.0);
    assert_eq!(fragment.children[1].bounds.y(), 10.0);

    // Now flex-end should pack to the bottom of the inline axis.
    let end_container = BoxNode::new_block(
      Arc::new(end_style),
      FormattingContextType::Flex,
      vec![
        BoxNode::new_block(
          create_item_style(10.0, 10.0),
          FormattingContextType::Block,
          vec![],
        ),
        BoxNode::new_block(
          create_item_style(10.0, 10.0),
          FormattingContextType::Block,
          vec![],
        ),
      ],
    );
    let end_fragment = fc.layout(&end_container, &constraints).unwrap();
    assert_eq!(end_fragment.children[0].bounds.y(), 80.0);
    assert_eq!(end_fragment.children[1].bounds.y(), 90.0);
  }

  #[test]
  fn flex_item_alignment_uses_parent_axes_not_item_writing_mode() {
    let fc = FlexFormattingContext::new();

    let mut container_style = ComputedStyle::default();
    container_style.display = Display::Flex;
    container_style.flex_direction = FlexDirection::Row; // becomes vertical under vertical-rl
    container_style.writing_mode = crate::style::types::WritingMode::VerticalRl;
    container_style.align_items = AlignItems::Center;
    container_style.width = Some(Length::px(100.0));

    let mut child1_style = ComputedStyle::default();
    child1_style.width = Some(Length::px(10.0));
    child1_style.height = Some(Length::px(10.0));
    child1_style.align_self = Some(AlignItems::Start);
    // Different writing mode should not affect axis interpretation
    child1_style.writing_mode = crate::style::types::WritingMode::HorizontalTb;
    let child1 = BoxNode::new_block(Arc::new(child1_style), FormattingContextType::Block, vec![]);

    let mut child2_style = ComputedStyle::default();
    child2_style.width = Some(Length::px(10.0));
    child2_style.height = Some(Length::px(10.0));
    child2_style.align_self = Some(AlignItems::End);
    child2_style.writing_mode = crate::style::types::WritingMode::HorizontalTb;
    let child2 = BoxNode::new_block(Arc::new(child2_style), FormattingContextType::Block, vec![]);

    let container = BoxNode::new_block(
      Arc::new(container_style),
      FormattingContextType::Flex,
      vec![child1, child2],
    );

    let constraints = LayoutConstraints::definite(100.0, 200.0);
    let fragment = fc.layout(&container, &constraints).unwrap();

    // Cross axis is horizontal with start at the right edge for vertical-rl.
    assert_eq!(fragment.children[0].bounds.x(), 90.0);
    assert_eq!(fragment.children[1].bounds.x(), 0.0);
    // Main axis is vertical; children stack along y.
    assert_eq!(fragment.children[0].bounds.y(), 0.0);
    assert_eq!(fragment.children[1].bounds.y(), 10.0);
  }

  #[test]
  fn flex_item_aspect_ratio_sets_width_from_height() {
    let fc = FlexFormattingContext::new();

    let mut container_style = ComputedStyle::default();
    container_style.display = Display::Flex;
    container_style.align_items = AlignItems::FlexStart;

    let mut item_style = ComputedStyle::default();
    item_style.height = Some(Length::px(40.0));
    item_style.aspect_ratio = AspectRatio::Ratio(2.0);
    let item = BoxNode::new_block(Arc::new(item_style), FormattingContextType::Block, vec![]);

    let container = BoxNode::new_block(
      Arc::new(container_style),
      FormattingContextType::Flex,
      vec![item],
    );

    let constraints = LayoutConstraints::definite(200.0, 200.0);
    let fragment = fc.layout(&container, &constraints).unwrap();

    assert_eq!(fragment.children[0].bounds.width(), 80.0);
    assert_eq!(fragment.children[0].bounds.height(), 40.0);
  }

  #[test]
  fn flex_item_aspect_ratio_sets_height_from_width() {
    let fc = FlexFormattingContext::new();

    let mut container_style = ComputedStyle::default();
    container_style.display = Display::Flex;
    container_style.align_items = AlignItems::FlexStart;

    let mut item_style = ComputedStyle::default();
    item_style.width = Some(Length::px(120.0));
    item_style.aspect_ratio = AspectRatio::Ratio(3.0);
    let item = BoxNode::new_block(Arc::new(item_style), FormattingContextType::Block, vec![]);

    let container = BoxNode::new_block(
      Arc::new(container_style),
      FormattingContextType::Flex,
      vec![item],
    );

    let constraints = LayoutConstraints::definite(300.0, 200.0);
    let fragment = fc.layout(&container, &constraints).unwrap();

    assert_eq!(fragment.children[0].bounds.width(), 120.0);
    assert_eq!(fragment.children[0].bounds.height(), 40.0);
  }

  #[test]
  fn test_intrinsic_sizing_max_content() {
    let fc = FlexFormattingContext::new();

    let item1 = BoxNode::new_block(
      create_item_style(100.0, 50.0),
      FormattingContextType::Block,
      vec![],
    );
    let item2 = BoxNode::new_block(
      create_item_style(150.0, 50.0),
      FormattingContextType::Block,
      vec![],
    );

    let container = BoxNode::new_block(
      create_flex_style(),
      FormattingContextType::Flex,
      vec![item1, item2],
    );

    let width = fc
      .compute_intrinsic_inline_size(&container, IntrinsicSizingMode::MaxContent)
      .unwrap();

    // Max-content width should be sum of children widths (row direction)
    assert_eq!(width, 250.0);
  }

  #[test]
  fn test_nested_flex() {
    let fc = FlexFormattingContext::new();

    // Inner flex container with two items
    let inner_item1 = BoxNode::new_block(
      create_item_style(50.0, 30.0),
      FormattingContextType::Block,
      vec![],
    );
    let inner_item2 = BoxNode::new_block(
      create_item_style(50.0, 30.0),
      FormattingContextType::Block,
      vec![],
    );

    let inner_container = BoxNode::new_block(
      create_flex_style(),
      FormattingContextType::Flex,
      vec![inner_item1, inner_item2],
    );

    // Outer flex container
    let outer_item = BoxNode::new_block(
      create_item_style(100.0, 50.0),
      FormattingContextType::Block,
      vec![],
    );

    let outer_container = BoxNode::new_block(
      create_flex_style(),
      FormattingContextType::Flex,
      vec![inner_container, outer_item],
    );

    let constraints = LayoutConstraints::definite(400.0, 600.0);
    let fragment = fc.layout(&outer_container, &constraints).unwrap();

    // Outer container has 2 children
    assert_eq!(fragment.children.len(), 2);

    // First child (inner container) should have 2 children
    assert_eq!(fragment.children[0].children.len(), 2);

    // Inner items should be laid out horizontally within their container
    assert_eq!(fragment.children[0].children[0].bounds.x(), 0.0);
    assert_eq!(fragment.children[0].children[1].bounds.x(), 50.0);
  }

  #[test]
  fn test_empty_flex_container() {
    let fc = FlexFormattingContext::new();

    let container = BoxNode::new_block(create_flex_style(), FormattingContextType::Flex, vec![]);

    let constraints = LayoutConstraints::definite(400.0, 600.0);
    let fragment = fc.layout(&container, &constraints).unwrap();

    assert_eq!(fragment.children.len(), 0);
  }

  #[test]
  fn test_flex_formatting_context_is_send_sync() {
    fn assert_send_sync<T: Send + Sync>() {}
    assert_send_sync::<FlexFormattingContext>();
  }

  #[test]
  fn test_style_conversion_flex_direction() {
    let fc = FlexFormattingContext::new();

    assert_eq!(
      fc.flex_direction_to_taffy(&ComputedStyle::default(), true, true),
      taffy::style::FlexDirection::Row
    );
    let mut row_rev = ComputedStyle::default();
    row_rev.flex_direction = FlexDirection::RowReverse;
    assert_eq!(
      fc.flex_direction_to_taffy(&row_rev, true, true),
      taffy::style::FlexDirection::RowReverse
    );

    let mut col = ComputedStyle::default();
    col.flex_direction = FlexDirection::Column;
    assert_eq!(
      fc.flex_direction_to_taffy(&col, true, true),
      taffy::style::FlexDirection::Column
    );

    let mut col_rev = ComputedStyle::default();
    col_rev.flex_direction = FlexDirection::ColumnReverse;
    assert_eq!(
      fc.flex_direction_to_taffy(&col_rev, true, true),
      taffy::style::FlexDirection::ColumnReverse
    );
  }

  #[test]
  fn test_style_conversion_flex_wrap() {
    let fc = FlexFormattingContext::new();

    assert_eq!(
      fc.flex_wrap_to_taffy(FlexWrap::NoWrap),
      taffy::style::FlexWrap::NoWrap
    );
    assert_eq!(
      fc.flex_wrap_to_taffy(FlexWrap::Wrap),
      taffy::style::FlexWrap::Wrap
    );
    assert_eq!(
      fc.flex_wrap_to_taffy(FlexWrap::WrapReverse),
      taffy::style::FlexWrap::WrapReverse
    );
  }

  #[test]
  fn test_length_conversion() {
    let fc = FlexFormattingContext::new();
    let style = ComputedStyle::default();

    // Pixel values
    let len_px = Length::px(100.0);
    assert_eq!(
      fc.length_to_dimension(&len_px, &style),
      Dimension::length(100.0)
    );

    // Percentage values
    let len_percent = Length::percent(50.0);
    assert_eq!(
      fc.length_to_dimension(&len_percent, &style),
      Dimension::percent(0.5)
    ); // 50% = 0.5

    // Auto (None)
    assert_eq!(
      fc.length_option_to_dimension(None, &style),
      Dimension::auto()
    );
  }

  #[test]
  fn sharded_flex_cache_supports_parallel_layouts() {
    use crate::debug::runtime::{set_runtime_toggles, RuntimeToggles};
    let mut toggles = std::collections::HashMap::new();
    toggles.insert("FASTR_FLEX_PROFILE".to_string(), "1".to_string());
    let guard = set_runtime_toggles(std::sync::Arc::new(RuntimeToggles::from_map(toggles)));

    let measure_cache = Arc::new(ShardedFlexCache::new_measure());
    let layout_cache = Arc::new(ShardedFlexCache::new_layout());
    let viewport = Size::new(640.0, 480.0);
    let fc = FlexFormattingContext::with_viewport_and_cb(
      viewport,
      ContainingBlock::viewport(viewport),
      FontContext::new(),
      measure_cache.clone(),
      layout_cache.clone(),
    )
    .with_parallelism(LayoutParallelism::enabled(1));

    let item = |w, h| {
      BoxNode::new_block(
        create_item_style(w, h),
        FormattingContextType::Block,
        vec![],
      )
    };
    let container = BoxNode::new_block(
      create_flex_style(),
      FormattingContextType::Flex,
      vec![item(40.0, 20.0), item(60.0, 24.0), item(30.0, 18.0)],
    );
    let constraints = LayoutConstraints::definite(300.0, 200.0);

    let expected = fc.layout(&container, &constraints).unwrap();
    let expected_size = expected.bounds.size;
    let shared_fc = Arc::new(fc);

    let results: Vec<Size> = (0..24)
      .into_par_iter()
      .map(|_| {
        shared_fc
          .layout(&container, &constraints)
          .map(|frag| frag.bounds.size)
          .unwrap()
      })
      .collect();
    for size in results {
      assert_eq!(size.width, expected_size.width);
      assert_eq!(size.height, expected_size.height);
    }

    let layout_stats = layout_cache.shard_stats();
    let layout_hits: u64 = layout_stats.iter().map(|s| s.hits).sum();
    let layout_misses: u64 = layout_stats.iter().map(|s| s.misses).sum();
    assert!(layout_hits > 0, "layout cache should record shard hits");
    assert!(layout_misses > 0, "layout cache should record shard misses");

    let measure_stats = measure_cache.shard_stats();
    let measure_lookups: u64 = measure_stats.iter().map(|s| s.hits + s.misses).sum();
    assert!(measure_lookups > 0, "measure cache should see lookups");

    drop(guard);
  }
}
