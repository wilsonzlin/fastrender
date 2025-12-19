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

use std::collections::hash_map::{DefaultHasher, Entry};
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::mem;
use std::sync::{Arc, Mutex, OnceLock};

use crate::geometry::{Point, Rect, Size};
use crate::layout::absolute_positioning::{resolve_positioned_style, AbsoluteLayout, AbsoluteLayoutInput};
use crate::layout::constraints::{AvailableSpace as CrateAvailableSpace, LayoutConstraints};
use crate::layout::contexts::block::BlockFormattingContext;
use crate::layout::contexts::factory::FormattingContextFactory;
use crate::layout::contexts::positioned::ContainingBlock;
use crate::layout::flex_profile::{self, record_node_measure_hit, record_node_measure_store, DimState};
use crate::layout::formatting_context::{
    count_flex_intrinsic_call, intrinsic_cache_lookup, intrinsic_cache_store, FormattingContext, IntrinsicSizingMode,
    LayoutError,
};
use crate::layout::profile::{layout_timer, LayoutKind};
use crate::layout::utils::{resolve_length_with_percentage_metrics, resolve_scrollbar_width};
use crate::style::display::{Display, FormattingContextType};
use crate::style::position::Position;
use crate::style::types::{
    AlignContent, AlignItems, AspectRatio, BoxSizing, Direction, FlexBasis, FlexDirection, FlexWrap, JustifyContent,
    Overflow as CssOverflow, WritingMode,
};
use crate::style::values::{Length, LengthUnit};
use crate::style::ComputedStyle;
use crate::text::font_loader::FontContext;
use crate::tree::box_tree::BoxNode;
use crate::tree::fragment_tree::{FragmentContent, FragmentNode};

static LOG_CHILD_IDS: std::sync::OnceLock<Vec<usize>> = std::sync::OnceLock::new();

use taffy::prelude::*;
use taffy::style::Overflow as TaffyOverflow;
use taffy::TaffyTree;

fn translate_fragment_tree(fragment: &mut FragmentNode, delta: Point) {
    fragment.bounds = Rect::new(
        Point::new(fragment.bounds.x() + delta.x, fragment.bounds.y() + delta.y),
        fragment.bounds.size,
    );
    for child in &mut fragment.children {
        translate_fragment_tree(child, delta);
    }
}

fn normalize_fragment_origin(fragment: &FragmentNode) -> FragmentNode {
    let mut normalized = fragment.clone();
    let origin = normalized.bounds.origin;
    if origin.x != 0.0 || origin.y != 0.0 {
        translate_fragment_tree(&mut normalized, Point::new(-origin.x, -origin.y));
    }
    normalized
}

fn trace_flex_text_ids() -> &'static Vec<usize> {
    static IDS: OnceLock<Vec<usize>> = OnceLock::new();
    IDS.get_or_init(|| {
        std::env::var("FASTR_TRACE_FLEX_TEXT")
            .ok()
            .map(|v| {
                v.split(',')
                    .filter_map(|tok| tok.trim().parse::<usize>().ok())
                    .collect()
            })
            .unwrap_or_default()
    })
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
    /// Viewport size used for resolving viewport-relative units inside Taffy conversion.
    viewport_size: Size,
    font_context: FontContext,
    nearest_positioned_cb: ContainingBlock,
    measured_fragments: Arc<Mutex<FlexMeasureCache>>,
    layout_fragments:
        Arc<Mutex<HashMap<u64, HashMap<(Option<u32>, Option<u32>), (Size, std::sync::Arc<FragmentNode>)>>>>,
}

pub(crate) type FlexMeasureCache =
    HashMap<u64, HashMap<(Option<u32>, Option<u32>), (Size, std::sync::Arc<FragmentNode>)>>;

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
            Arc::new(Mutex::new(HashMap::new())),
            Arc::new(Mutex::new(HashMap::new())),
        )
    }

    pub fn with_viewport(viewport_size: Size) -> Self {
        Self::with_viewport_and_cb(
            viewport_size,
            ContainingBlock::viewport(viewport_size),
            FontContext::new(),
            Arc::new(Mutex::new(HashMap::new())),
            Arc::new(Mutex::new(HashMap::new())),
        )
    }

    pub fn with_viewport_and_cb(
        viewport_size: Size,
        nearest_positioned_cb: ContainingBlock,
        font_context: FontContext,
        measured_fragments: Arc<Mutex<FlexMeasureCache>>,
        layout_fragments: Arc<
            Mutex<HashMap<u64, HashMap<(Option<u32>, Option<u32>), (Size, std::sync::Arc<FragmentNode>)>>>,
        >,
    ) -> Self {
        Self {
            viewport_size,
            font_context,
            nearest_positioned_cb,
            measured_fragments,
            layout_fragments,
        }
    }
}

impl Default for FlexFormattingContext {
    fn default() -> Self {
        Self::new()
    }
}

impl std::fmt::Debug for FlexFormattingContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FlexFormattingContext").finish_non_exhaustive()
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
    fn layout(&self, box_node: &BoxNode, constraints: &LayoutConstraints) -> Result<FragmentNode, LayoutError> {
        let _profile = layout_timer(LayoutKind::Flex);
        let build_timer = flex_profile::timer();
        let mut constraints = *constraints;
        let container_inline_base = constraints.inline_percentage_base.or_else(|| constraints.width());
        if !constraints.is_width_definite() {
            let fallback = container_inline_base.unwrap_or(self.viewport_size.width);
            constraints.available_width = CrateAvailableSpace::Definite(fallback);
            if constraints.inline_percentage_base.is_none() {
                constraints.inline_percentage_base = container_inline_base.or(Some(fallback));
            }
        }
        // Keep block axis as provided; many flex containers legitimately size-to-content.

        // Reuse full layout fragments when the same flex container is laid out repeatedly with
        // identical available sizes (common on carousel-heavy pages). This is scoped per layout
        // run via the factory cache reset.
        let disable_cache = std::env::var("FASTR_DISABLE_FLEX_CACHE")
            .map(|v| v != "0")
            .unwrap_or(false);
        let layout_cache_entry = if disable_cache {
            None
        } else {
            layout_cache_key(&constraints, self.viewport_size).map(|k| (flex_cache_key(box_node), k))
        };

        let _trace_text_ids = trace_flex_text_ids();
        if let Some((cache_key, key)) = layout_cache_entry {
            if let Ok(mut cache) = self.layout_fragments.lock() {
                if let Some((_, fragment)) = cache.get(&cache_key).and_then(|m| m.get(&key)).cloned() {
                    flex_profile::record_layout_cache_hit();
                    return Ok((*fragment).clone());
                }
                // Fall back to a tolerance-based lookup when the quantized key differs slightly.
                if let Some(entry) = cache.get(&cache_key) {
                    let target_w = constraints.width().unwrap_or(self.viewport_size.width);
                    let target_h = constraints.height().unwrap_or(self.viewport_size.height);
                    if let Some((stored_size, fragment)) =
                        find_layout_cache_fragment(entry, Size::new(target_w.max(0.0), target_h.max(0.0)))
                    {
                        // Seed the exact key for faster hits next time.
                        let cached = cache.entry(cache_key).or_default();
                        cached
                            .entry(key)
                            .or_insert_with(|| (stored_size, std::sync::Arc::clone(&fragment)));
                        flex_profile::record_layout_cache_hit();
                        return Ok((*fragment).clone());
                    }
                }
            }
        }

        // Create a fresh Taffy tree for this layout
        let mut taffy_tree: TaffyTree<*const BoxNode> = TaffyTree::new();

        // Partition children: out-of-flow abs/fixed are handled after flex layout per CSS positioning.
        let mut in_flow_children: Vec<(usize, &BoxNode)> = Vec::new();
        let mut positioned_children = Vec::new();
        for (idx, child) in box_node.children.iter().enumerate() {
            match child.style.position {
                crate::style::position::Position::Absolute | crate::style::position::Position::Fixed => {
                    positioned_children.push(child.clone());
                }
                _ => in_flow_children.push((idx, child)),
            }
        }
        in_flow_children
            .sort_by(|(a_idx, a), (b_idx, b)| a.style.order.cmp(&b.style.order).then_with(|| a_idx.cmp(b_idx)));
        let in_flow_children: Vec<&BoxNode> = in_flow_children.into_iter().map(|(_, child)| child).collect();

        // Phase 1: Build Taffy tree from in-flow children
        let mut node_map: HashMap<*const BoxNode, NodeId> = HashMap::new();
        let root_node = self.build_taffy_tree_children(&mut taffy_tree, box_node, &in_flow_children, &mut node_map)?;
        flex_profile::record_build_time(build_timer);

        // Phase 2: Compute layout using Taffy
        let available_space = self.constraints_to_available_space(&constraints);
        let parent_inline_base = constraints.inline_percentage_base.or(container_inline_base);
        let viewport_size = self.viewport_size;
        let nearest_positioned_cb = self.nearest_positioned_cb;
        let measured_fragments = self.measured_fragments.clone();
        let factory = FormattingContextFactory::with_font_context_viewport_cb_and_cache(
            self.font_context.clone(),
            viewport_size,
            nearest_positioned_cb,
            measured_fragments.clone(),
            self.layout_fragments.clone(),
        );
        let this = self.clone();
        let mut pass_cache: HashMap<u64, HashMap<(Option<u32>, Option<u32>), (Size, std::sync::Arc<FragmentNode>)>> =
            HashMap::new();
        let mut pass_stores: HashMap<u64, usize> = HashMap::new();
        let mut pass_hits: HashMap<u64, usize> = HashMap::new();
        let compute_timer = flex_profile::timer();
        static LOG_ROOT: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
        if *LOG_ROOT.get_or_init(|| std::env::var("FASTR_LOG_FLEX_ROOT").map(|v| v != "0").unwrap_or(false)) {
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
        static LOG_SKINNY: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
        static LOG_SMALL_AVAIL: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
        static LOG_MEASURE_IDS: std::sync::OnceLock<Vec<usize>> = std::sync::OnceLock::new();
        static LOG_NODE_KEYS: std::sync::OnceLock<Vec<usize>> = std::sync::OnceLock::new();
        static LOG_NODE_KEYS_MAX: std::sync::OnceLock<usize> = std::sync::OnceLock::new();
        static LOG_NODE_KEYS_COUNTS: std::sync::OnceLock<std::sync::Mutex<std::collections::HashMap<usize, usize>>> =
            std::sync::OnceLock::new();
        static LOG_LARGE_AVAIL: std::sync::OnceLock<Option<f32>> = std::sync::OnceLock::new();
        static LOG_LARGE_AVAIL_COUNTS: std::sync::OnceLock<std::sync::Mutex<std::collections::HashMap<usize, usize>>> =
            std::sync::OnceLock::new();
        static LOG_CONSTRAINT_IDS: std::sync::OnceLock<Vec<usize>> = std::sync::OnceLock::new();
        static LOG_CONSTRAINT_COUNTS: std::sync::OnceLock<std::sync::Mutex<std::collections::HashMap<usize, usize>>> =
            std::sync::OnceLock::new();
        static LOG_CONSTRAINT_LIMIT: std::sync::OnceLock<usize> = std::sync::OnceLock::new();
        static LOG_CONSTRAINT_IDS_LOGGED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
        static LOG_FIRST_N: std::sync::OnceLock<usize> = std::sync::OnceLock::new();
        static LOG_FIRST_N_COUNTER: std::sync::OnceLock<std::sync::Mutex<usize>> = std::sync::OnceLock::new();
        static ABORT_AFTER_FIRST: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
        static LOG_CONSTRAINT_RAW: std::sync::OnceLock<Option<String>> = std::sync::OnceLock::new();
        let log_constraint_raw = LOG_CONSTRAINT_RAW
            .get_or_init(|| std::env::var("FASTR_LOG_FLEX_CONSTRAINTS").ok())
            .clone();
        let log_constraint_ids = LOG_CONSTRAINT_IDS.get_or_init(|| {
            log_constraint_raw
                .as_ref()
                .map(|s| {
                    s.split(',')
                        .filter_map(|tok| tok.trim().parse::<usize>().ok())
                        .collect()
                })
                .unwrap_or_default()
        });
        let log_constraint_limit = *LOG_CONSTRAINT_LIMIT.get_or_init(|| {
            std::env::var("FASTR_LOG_FLEX_CONSTRAINTS_MAX")
                .ok()
                .and_then(|v| v.parse().ok())
                .unwrap_or(10)
        });
        let log_first_n = *LOG_FIRST_N.get_or_init(|| {
            std::env::var("FASTR_LOG_FLEX_FIRST_N")
                .ok()
                .and_then(|v| v.parse().ok())
                .unwrap_or(0)
        });
        let abort_after_first = *ABORT_AFTER_FIRST.get_or_init(|| {
            std::env::var("FASTR_ABORT_FLEX_AFTER_FIRST_N")
                .map(|v| v != "0" && !v.eq_ignore_ascii_case("false"))
                .unwrap_or(false)
        });
        if log_constraint_raw.is_some() {
            LOG_CONSTRAINT_IDS_LOGGED.get_or_init(|| {
                eprintln!(
                    "[flex-constraints-env] raw={:?} ids={:?} max={}",
                    log_constraint_raw, log_constraint_ids, log_constraint_limit
                );
                true
            });
        }
        static LOG_FIRST_ENV: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
        if log_first_n > 0 {
            LOG_FIRST_ENV.get_or_init(|| {
                eprintln!("[flex-first-env] n={} abort={}", log_first_n, abort_after_first);
                true
            });
        }
        let log_skinny = *LOG_SKINNY.get_or_init(|| {
            std::env::var("FASTR_LOG_SKINNY_FLEX")
                .map(|v| v != "0")
                .unwrap_or(false)
        });
        let log_small_avail =
            *LOG_SMALL_AVAIL.get_or_init(|| std::env::var("FASTR_LOG_SMALL_FLEX").map(|v| v != "0").unwrap_or(false));
        let log_measure_ids = LOG_MEASURE_IDS.get_or_init(|| {
            std::env::var("FASTR_LOG_FLEX_MEASURE_IDS")
                .ok()
                .map(|s| {
                    s.split(',')
                        .filter_map(|tok| tok.trim().parse::<usize>().ok())
                        .collect()
                })
                .unwrap_or_default()
        });
        let log_node_keys = LOG_NODE_KEYS.get_or_init(|| {
            std::env::var("FASTR_LOG_FLEX_NODE_KEYS")
                .ok()
                .map(|s| {
                    s.split(',')
                        .filter_map(|tok| tok.trim().parse::<usize>().ok())
                        .collect()
                })
                .unwrap_or_default()
        });
        let log_node_keys_max = *LOG_NODE_KEYS_MAX.get_or_init(|| {
            std::env::var("FASTR_LOG_FLEX_NODE_KEYS_MAX")
                .ok()
                .and_then(|v| v.parse::<usize>().ok())
                .filter(|v| *v > 0)
                .unwrap_or(10)
        });
        let log_large_avail = *LOG_LARGE_AVAIL.get_or_init(|| {
            std::env::var("FASTR_LOG_LARGE_FLEX")
                .ok()
                .and_then(|v| v.parse::<f32>().ok())
                .filter(|v| *v > 0.0)
        });
        taffy_tree
            .compute_layout_with_measure(
                root_node,
                available_space,
                move |known_dimensions, mut avail, _node_id, node_context, _style| {
                    // Treat zero/near-zero definite sizes as absent to avoid pathological
                    // measurement probes when Taffy propagates a 0px available size. This
                    // aligns with constraints_from_taffy, which promotes tiny definites to
                    // Indefinite/MaxContent.
                    let mut known_dimensions = known_dimensions;
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
                    let mut known_dimensions = known_dimensions;
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
                    static TRACE_FLAG: OnceLock<bool> = OnceLock::new();
                    static TRACE_COUNT: OnceLock<Mutex<usize>> = OnceLock::new();
                    static LOG_MEASURE_COUNTS: OnceLock<Mutex<HashMap<usize, usize>>> = OnceLock::new();
                    let trace_enabled = *TRACE_FLAG.get_or_init(|| {
                        std::env::var("FASTR_TRACE_FLEX")
                            .map(|v| v != "0" && !v.eq_ignore_ascii_case("false"))
                            .unwrap_or(false)
                    });
                    static LOG_MEASURE_FIRST: OnceLock<usize> = OnceLock::new();
                    static LOG_MEASURE_FIRST_COUNT: OnceLock<Mutex<usize>> = OnceLock::new();
                    static LOG_MEASURE_FIRST_ABOVE: OnceLock<Option<u128>> = OnceLock::new();
                    let log_measure_first = *LOG_MEASURE_FIRST
                        .get_or_init(|| {
                            std::env::var("FASTR_LOG_FLEX_MEASURE_FIRST_N")
                                .ok()
                                .and_then(|v| v.parse().ok())
                                .unwrap_or(0)
                        });
                    let log_measure_first_above = *LOG_MEASURE_FIRST_ABOVE
                        .get_or_init(|| std::env::var("FASTR_LOG_FLEX_MEASURE_FIRST_N_MS").ok().and_then(|v| v.parse().ok()));

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
                                let mut count = LOG_MEASURE_FIRST_COUNT.get_or_init(|| Mutex::new(0)).lock().unwrap();
                                if *count < log_measure_first {
                                    *count += 1;
                                    let selector = box_node
                                        .debug_info
                                        .as_ref()
                                        .map(|d| d.to_selector())
                                        .unwrap_or_else(|| "<anon>".to_string());
                                    eprintln!(
                                        "[flex-measure-first] seq={} id={} selector={} elapsed_ms={} known=({:?},{:?}) avail=({:?},{:?})",
                                        *count,
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
                        *pass_hits.entry(cache_key).or_insert(0) += 1;
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
                    if let Ok(mut cache) = measured_fragments.lock() {
                        if let Some((size, frag)) = cache.get(&cache_key).and_then(|m| m.get(&key)).cloned() {
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
                        // Try a tolerance-based match when the exact key misses so quantized probes
                        // can reuse earlier measurements without re-laying out the subtree.
                        let target_w = fallback_size(known_dimensions.width, avail.width);
                        let target_h = fallback_size(known_dimensions.height, avail.height);
                        if let Some(entry) = cache.get(&cache_key) {
                            if let Some((stored_size, frag)) =
                                find_layout_cache_fragment(entry, Size::new(target_w, target_h))
                            {
                                record_node_measure_hit(measure_box.id);
                                flex_profile::record_measure_hit();
                                flex_profile::record_measure_time(measure_timer);
                                let cached = cache.entry(cache_key).or_default();
                                cached
                                    .entry(key)
                                    .or_insert_with(|| (stored_size, std::sync::Arc::clone(&frag)));
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
                        let mut counts = LOG_MEASURE_COUNTS.get_or_init(|| Mutex::new(HashMap::new())).lock().unwrap();
                        let entry = counts.entry(measure_box.id).or_insert(0);
                        let selector = measure_box
                            .debug_info
                            .as_ref()
                            .map(|d| d.to_selector())
                            .unwrap_or_else(|| "<anon>".to_string());
                        if *entry < 3 {
                            eprintln!(
                                "[flex-measure] id={} selector={} display={:?} basis={:?} width_decl={:?} avail_w={:?} known_w={:?} avail_after={:?}",
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
                        *entry += 1;
                    }
                    let constraints = this.constraints_from_taffy(known_dimensions, avail, parent_inline_base);
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

                    let fc: Box<dyn FormattingContext> = if matches!(fc_type, FormattingContextType::Block) {
                        Box::new(BlockFormattingContext::for_flex_item_with_font_context_viewport_and_cb(
                            this.font_context.clone(),
                            viewport_size,
                            nearest_positioned_cb,
                        ))
                    } else {
                        factory.create(fc_type)
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
                    let fragment = match fc.layout(measure_box, &constraints) {
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

                    let mut max_h_bound = resolved_max_h.unwrap_or_else(|| match avail.height {
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
                        let mut counts = LOG_MEASURE_COUNTS.get_or_init(|| Mutex::new(HashMap::new())).lock().unwrap();
                        let entry = counts.entry(measure_box.id).or_insert(0);
                        if *entry < 3 {
                            let selector = measure_box
                                .debug_info
                                .as_ref()
                                .map(|d| d.to_selector())
                                .unwrap_or_else(|| "<anon>".to_string());
                            eprintln!(
                                "[flex-measure-result] id={} selector={} avail=({:?},{:?}) known=({:?},{:?}) constraints=({:?},{:?}) content=({:.2},{:.2}) intrinsic=({:.2},{:.2}) min=({:.2},{:.2}) max=({:.2},{:.2}) inline_hint={:?}",
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
                        *entry += 1;
                    }

                    let normalized_fragment = std::sync::Arc::new(normalize_fragment_origin(&fragment));

                    if let Ok(mut map) = measured_fragments.lock() {
                        let entry = map.entry(cache_key).or_default();
                        if entry.len() >= MAX_MEASURE_CACHE_PER_NODE {
                            if let Some(first_key) = entry.keys().next().copied() {
                                entry.remove(&first_key);
                            }
                        }
                        if let Entry::Vacant(e) = entry.entry(key) {
                            let stored_size =
                                Size::new(content_size.width.max(0.0), content_size.height.max(0.0));
                            e.insert((stored_size, normalized_fragment.clone()));
                            flex_profile::record_measure_store(true);
                            record_node_measure_store(measure_box.id);
                        } else {
                            flex_profile::record_measure_store(false);
                        }
                    }
                        if let Some(_ptr) = node_ptr {
                            let entry = pass_cache.entry(cache_key).or_default();
                            if let Entry::Vacant(e) = entry.entry(key) {
                                let stored_size = Size::new(content_size.width.max(0.0), content_size.height.max(0.0));
                                e.insert((stored_size, normalized_fragment.clone()));
                                *pass_stores.entry(cache_key).or_insert(0) += 1;
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

        // Phase 3: Convert Taffy layout back to FragmentNode
        let convert_timer = flex_profile::timer();
        let mut fragment =
            self.taffy_to_fragment(&taffy_tree, root_node, root_node, box_node, &node_map, &constraints)?;
        // Respect align-items/align-self in the container's coordinate system (parent axes), even
        // when the child uses a different writing mode. Taffy resolves alignment in its own
        // axis space; here we remap the cross-axis position to match the parent's writing-mode.
        if matches!(box_node.style.display, Display::Flex | Display::InlineFlex) && !fragment.children.is_empty() {
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

            // If Taffy failed to shrink items enough (common with zero-availability fallbacks), redistribute
            // the deficit along the main axis using the flex-shrink weights.
            let available_main = if main_is_horizontal {
                constraints.width().unwrap_or_else(|| fragment.bounds.width())
            } else {
                constraints.height().unwrap_or_else(|| fragment.bounds.height())
            };
            if available_main.is_finite() {
                let mut total_main = 0.0;
                let mut total_weight = 0.0;
                let child_count = fragment.children.len().max(1) as f32;
                let mut box_lookup: std::collections::HashMap<usize, &BoxNode> = std::collections::HashMap::new();
                for child in &box_node.children {
                    box_lookup.insert(child.id, child);
                }
                let mut frag_box_ids: Vec<(usize, usize)> = Vec::new();
                for (idx, frag) in fragment.children.iter().enumerate() {
                    if let Some(box_id) = match &frag.content {
                        FragmentContent::Block { box_id }
                        | FragmentContent::Inline { box_id, .. }
                        | FragmentContent::Text { box_id, .. }
                        | FragmentContent::Replaced { box_id, .. } => *box_id,
                        FragmentContent::Line { .. } => None,
                    } {
                        frag_box_ids.push((idx, box_id));
                    }
                }
                for (frag_idx, box_id) in &frag_box_ids {
                    if let Some(child_node) = box_lookup.get(box_id) {
                        let child_fragment = &fragment.children[*frag_idx];
                        let base = if main_is_horizontal {
                            child_fragment.bounds.width()
                        } else {
                            child_fragment.bounds.height()
                        };
                        let weight = child_node.style.flex_shrink.max(0.0) * base;
                        total_main += base;
                        total_weight += weight;
                    }
                }
                if total_main > available_main + 0.01 {
                    let deficit = total_main - available_main;
                    let mut cursor = 0.0;
                    for (frag_idx, box_id) in &frag_box_ids {
                        if let Some(child_node) = box_lookup.get(box_id) {
                            let child_fragment = &mut fragment.children[*frag_idx];
                            let base = if main_is_horizontal {
                                child_fragment.bounds.width()
                            } else {
                                child_fragment.bounds.height()
                            };
                            let weight = child_node.style.flex_shrink.max(0.0) * base;
                            let share = if total_weight > 0.0 {
                                deficit * (weight / total_weight)
                            } else {
                                deficit / child_count
                            };
                            let new_size = (base - share).max(0.0);
                            if main_is_horizontal {
                                child_fragment.bounds = Rect::new(
                                    Point::new(cursor, child_fragment.bounds.y()),
                                    Size::new(new_size, child_fragment.bounds.height()),
                                );
                            } else {
                                child_fragment.bounds = Rect::new(
                                    Point::new(child_fragment.bounds.x(), cursor),
                                    Size::new(child_fragment.bounds.width(), new_size),
                                );
                            }
                            cursor += new_size;
                        }
                    }
                }
            }

            let cross_size = if cross_is_horizontal {
                fragment.bounds.width()
            } else {
                fragment.bounds.height()
            };

            for (child_node, child_fragment) in in_flow_children.iter().zip(fragment.children.iter_mut()) {
                let align = child_node.style.align_self.unwrap_or(box_node.style.align_items);
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
            if std::env::var("FASTR_DEBUG_FLEX_CHILD")
                .map(|v| v != "0" && !v.eq_ignore_ascii_case("false"))
                .unwrap_or(false)
            {
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
            fragment.bounds = Rect::new(fragment.bounds.origin, Size::new(clamped_w, fragment.bounds.height()));
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
        // If we clamped the container width/height, ensure children do not sit far outside the
        // new bounds (e.g., when Taffy laid out a very wide row). Reuse the overflow clamp used
        // earlier but against the updated fragment bounds.
        if fragment.bounds.width().is_finite() && fragment.bounds.height().is_finite() {
            let max_w = fragment.bounds.width().max(0.0);
            let max_h = fragment.bounds.height().max(0.0);
            let eps = 0.5;
            for child in &mut fragment.children {
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
        }

        static LOG_WIDE: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
        static LOG_SKINNY_FRAG: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
        static LOG_TARGET_IDS: std::sync::OnceLock<Vec<usize>> = std::sync::OnceLock::new();
        let log_wide = *LOG_WIDE.get_or_init(|| std::env::var("FASTR_LOG_WIDE_FLEX").is_ok());
        let log_skinny_frag = *LOG_SKINNY_FRAG.get_or_init(|| {
            std::env::var("FASTR_LOG_SKINNY_FLEX")
                .map(|v| v != "0")
                .unwrap_or(false)
        });
        let log_target_ids = LOG_TARGET_IDS.get_or_init(|| {
            std::env::var("FASTR_LOG_FLEX_IDS")
                .ok()
                .map(|s| {
                    s.split(',')
                        .filter_map(|tok| tok.trim().parse::<usize>().ok())
                        .collect()
                })
                .unwrap_or_default()
        });
        if log_wide || log_skinny_frag || !log_target_ids.is_empty() {
            let avail_w = constraints.width();
            if fragment.bounds.width() > self.viewport_size.width + 0.5
                || avail_w.map(|w| w > self.viewport_size.width + 0.5).unwrap_or(false)
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
                if let Ok(mut cache) = self.layout_fragments.lock() {
                    let entry = cache.entry(cache_key).or_default();
                    if entry.len() >= MAX_LAYOUT_CACHE_PER_NODE {
                        if let Some(first_key) = entry.keys().next().copied() {
                            entry.remove(&first_key);
                        }
                    }
                    let size = fragment.bounds.size;
                    entry
                        .entry(key)
                        .or_insert_with(|| (size, std::sync::Arc::new(fragment.clone())));
                    flex_profile::record_layout_cache_store();
                }
            }
        }

        // Phase 4: Position out-of-flow abs/fixed children against this flex container.
        if !positioned_children.is_empty() {
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
            let cb = if box_node.style.position.is_positioned() {
                ContainingBlock::with_viewport_and_bases(
                    padding_rect,
                    self.viewport_size,
                    Some(padding_rect.size.width),
                    block_base,
                )
            } else {
                self.nearest_positioned_cb
            };

            let factory = crate::layout::contexts::factory::FormattingContextFactory::with_font_context_viewport_and_cb(
                self.font_context.clone(),
                self.viewport_size,
                cb,
            );

            for child in positioned_children {
                let original_style = child.style.clone();
                // Layout child as static to obtain intrinsic size.
                let mut layout_child = child.clone();
                let mut style = (*layout_child.style).clone();
                style.position = crate::style::position::Position::Relative;
                style.top = None;
                style.right = None;
                style.bottom = None;
                style.left = None;
                layout_child.style = Arc::new(style);

                let fc_type = layout_child
                    .formatting_context()
                    .unwrap_or(crate::style::display::FormattingContextType::Block);
                let fc = factory.create(fc_type);
                let child_constraints = LayoutConstraints::new(
                    CrateAvailableSpace::Definite(padding_rect.size.width),
                    block_base
                        .map(CrateAvailableSpace::Definite)
                        .unwrap_or(CrateAvailableSpace::Indefinite),
                );
                let mut child_fragment = fc.layout(&layout_child, &child_constraints)?;

                let positioned_style =
                    resolve_positioned_style(&child.style, &cb, self.viewport_size, &self.font_context);
                // Static position should start at the padding box origin; AbsoluteLayout will
                // add padding/border offsets, so use the content origin here to avoid double
                // counting padding.
                let static_pos = Point::ZERO;
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
                let mut input = AbsoluteLayoutInput::new(positioned_style, child_fragment.bounds.size, static_pos);
                input.is_replaced = child.is_replaced();
                input.preferred_min_inline_size = preferred_min_inline;
                input.preferred_inline_size = preferred_inline;
                input.preferred_min_block_size = preferred_min_block;
                input.preferred_block_size = preferred_block;
                let result = abs.layout_absolute(&input, &cb)?;
                child_fragment.bounds = Rect::new(result.position, result.size);
                child_fragment.style = Some(original_style);
                fragment.children.push(child_fragment);
            }
        }

        Ok(fragment)
    }

    /// Computes intrinsic size by running Taffy with appropriate constraints
    fn compute_intrinsic_inline_size(&self, box_node: &BoxNode, mode: IntrinsicSizingMode) -> Result<f32, LayoutError> {
        count_flex_intrinsic_call();
        if let Some(cached) = intrinsic_cache_lookup(box_node, mode) {
            return Ok(cached);
        }

        let style = &box_node.style;
        if style.containment.size || style.containment.inline_size {
            let edges = self.horizontal_edges_px(style).unwrap_or(0.0);
            intrinsic_cache_store(box_node, mode, edges.max(0.0));
            return Ok(edges.max(0.0));
        }

        // Approximate intrinsic inline size from flex items per CSS flexbox intrinsic sizing rules:
        // - Row axis: sum of item min/max-content contributions
        // - Column axis: max of item contributions
        let factory = FormattingContextFactory::with_font_context_viewport_and_cb(
            self.font_context.clone(),
            self.viewport_size,
            self.nearest_positioned_cb,
        );
        let is_row_axis = matches!(style.flex_direction, FlexDirection::Row | FlexDirection::RowReverse);

        let mut contribution = 0.0f32;
        for child in &box_node.children {
            if matches!(child.style.position, Position::Absolute | Position::Fixed) {
                continue;
            }

            let fc_type = child.formatting_context().unwrap_or(FormattingContextType::Block);
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
    let min_depends = style.min_height.as_ref().is_some_and(Length::has_percentage);
    let max_depends = style.max_height.as_ref().is_some_and(Length::has_percentage);
    let flex_basis_depends = matches!(style.flex_basis, FlexBasis::Length(len) if len.has_percentage());

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

    let width_is_intrinsic =
        known.width.is_none() && matches!(avail.width, AvailableSpace::MinContent | AvailableSpace::MaxContent);

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
    let fingerprint = flex_style_fingerprint(&box_node.style);
    fingerprint.hash(&mut h);
    // Incorporate a simplified key for common carousel templates to merge otherwise identical
    // repeated items. Using debug_info selector avoids deep tree hashing while keeping keys
    // distinct across component types.
    if let Some(sel) = box_node.debug_info.as_ref().map(|d| d.to_selector()) {
        sel.hash(&mut h);
    }
    h.finish()
}

fn layout_cache_key(constraints: &LayoutConstraints, viewport: Size) -> Option<(Option<u32>, Option<u32>)> {
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

fn cache_tolerances(target_size: Size) -> (f32, f32) {
    let band = |v: f32| -> f32 {
        if v > 4096.0 {
            32.0
        } else if v > 2048.0 {
            16.0
        } else if v > 1024.0 {
            8.0
        } else if v > 512.0 {
            6.0
        } else {
            4.0
        }
    };

    // Ensure we never accept exact zero tolerances (which would disable reuse) and clamp to a
    // small minimum for small probes to still allow merging nearly identical sizes.
    let min_eps = 1.0;

    // Bias tolerance slightly toward the larger axis so extremely wide/tall probes coalesce more.
    let eps_w = f32::max(
        f32::max(band(target_size.width), band(target_size.height) * 0.75),
        min_eps,
    );
    let eps_h = f32::max(
        f32::max(band(target_size.height), band(target_size.width) * 0.5),
        min_eps,
    );

    (eps_w, eps_h)
}

fn find_layout_cache_fragment(
    cache: &HashMap<(Option<u32>, Option<u32>), (Size, std::sync::Arc<FragmentNode>)>,
    target_size: Size,
) -> Option<(Size, std::sync::Arc<FragmentNode>)> {
    let mut best: Option<(Size, std::sync::Arc<FragmentNode>)> = None;
    let mut best_score = f32::MAX;
    // Allow small deltas so quantized sizes can still reuse a prior layout. Loosen tolerance
    // for very large targets (wide carousels) to merge near-identical probes that differ by
    // a handful of pixels while keeping smaller layouts precise.
    let (eps_w, eps_h) = cache_tolerances(target_size);
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
        self.build_taffy_tree_inner(taffy_tree, box_node, node_map, true, None, Some(root_children))
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
                taffy_tree
                    .new_leaf(taffy_style)
                    .map_err(|e| LayoutError::MissingContext(format!("Failed to create Taffy leaf: {:?}", e)))?
            } else {
                taffy_tree
                    .new_with_children(taffy_style, &taffy_children)
                    .map_err(|e| LayoutError::MissingContext(format!("Failed to create Taffy node: {:?}", e)))?
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
        let main_is_inline = matches!(style.flex_direction, FlexDirection::Row | FlexDirection::RowReverse);
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

        let reserve_scroll_x =
            style.scrollbar_gutter.stable && matches!(style.overflow_x, CssOverflow::Auto | CssOverflow::Scroll);
        let reserve_scroll_y =
            style.scrollbar_gutter.stable && matches!(style.overflow_y, CssOverflow::Auto | CssOverflow::Scroll);
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

        // Flexbox default minimum size is 'auto', which maps to the min-content size for the inline axis
        // when the author hasn't provided an explicit min-width. Taffy treats `auto` as zero, so we
        // compute the min-content inline size ourselves to avoid zero-width flex items.
        if !is_root {
            match style.flex_direction {
                FlexDirection::Row | FlexDirection::RowReverse => {
                    if min_width_dimension == Dimension::AUTO {
                        if let Ok(min_content) =
                            self.compute_intrinsic_inline_size(box_node, IntrinsicSizingMode::MinContent)
                        {
                            if min_content.is_finite() && min_content > 0.0 {
                                min_width_dimension = Dimension::length(min_content);
                            }
                        }
                    }
                }
                FlexDirection::Column | FlexDirection::ColumnReverse => {
                    if min_height_dimension == Dimension::AUTO {
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
        }

        taffy::style::Style {
            // Display mode - only root is Flex, children are Block (flex items)
            display: self.display_to_taffy(style, is_root),

            // Flex container properties
            flex_direction: self.flex_direction_to_taffy(style, inline_positive_container, block_positive_container),
            flex_wrap: self.flex_wrap_to_taffy(style.flex_wrap),
            justify_content: self.justify_content_to_taffy(style.justify_content, main_positive_container),
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
                width: self.length_option_to_dimension_box_sizing(style.max_width.as_ref(), style, Axis::Horizontal),
                height: self.length_option_to_dimension_box_sizing(style.max_height.as_ref(), style, Axis::Vertical),
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
        let width = match style.width.as_ref() {
            Some(len) => self.dimension_for_box_sizing(len, style, Axis::Horizontal),
            None if is_root => {
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
            for child in &node.children {
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
            for (_key, (size, frag)) in cache {
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
                if let Some(def_w) = constraints.width().filter(|w| w.is_finite() && *w > rect_eps) {
                    rect.size.width = def_w;
                } else if let Some(base) = constraints.inline_percentage_base.filter(|w| *w > rect_eps) {
                    rect.size.width = base;
                } else {
                    rect.size.width = self.viewport_size.width;
                }
            }
            // Clamp overly wide layouts back to the definite available inline size so children
            // are reflowed within the container instead of inheriting a runaway max-content span.
            if let Some(def_w) = constraints.width().filter(|w| w.is_finite() && *w > rect_eps) {
                if rect.size.width > def_w + rect_eps {
                    rect.size.width = def_w;
                }
            }
        }

        // Convert children by re-running layout with the definite sizes Taffy resolved.
        // This preserves the fully laid-out fragment trees (text, inline content) instead of
        // reconstructing empty boxes from the cached measure results.
        let mut children: Vec<FragmentNode>;
        let factory = crate::layout::contexts::factory::FormattingContextFactory::with_font_context_viewport_and_cb(
            self.font_context.clone(),
            self.viewport_size,
            self.nearest_positioned_cb,
        );
        let measured_fragments = self.measured_fragments.clone();
        let main_axis_is_row = matches!(
            box_node.style.flex_direction,
            crate::style::types::FlexDirection::Row | crate::style::types::FlexDirection::RowReverse
        );
        let mut fallback_cursor_x = 0.0;
        let mut fallback_cursor_y = 0.0;
        let mut last_layout_x: Option<f32> = None;
        let mut last_layout_y: Option<f32> = None;
        let mut manual_row_positions = false;
        let mut manual_col_positions = false;
        let wrap_eps = 0.5;
        let mut unordered_children: Vec<(i32, usize, FragmentNode)> = Vec::with_capacity(box_node.children.len());
        #[cfg(test)]
        eprintln!(
            "[flex-debug-node-map-len] box_id={} node_map_len={}",
            box_node.id,
            node_map.len()
        );
        for (dom_idx, child_box) in box_node.children.iter().enumerate() {
            if let Some(&child_taffy) = node_map.get(&(child_box as *const BoxNode)) {
                let child_layout = taffy_tree
                    .layout(child_taffy)
                    .map_err(|e| LayoutError::MissingContext(format!("Failed to get Taffy layout: {:?}", e)))?;
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
                if std::env::var("FASTR_DEBUG_FLEX_CHILD")
                    .map(|v| v != "0" && !v.eq_ignore_ascii_case("false"))
                    .unwrap_or(false)
                {
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
                let needs_intrinsic_main =
                    (main_axis_is_row && raw_layout_width <= eps) || (!main_axis_is_row && raw_layout_height <= eps);

                let mut reused = None;
                if !needs_intrinsic_main {
                    if let Ok(cache) = measured_fragments.lock() {
                        if let Some(entry) = cache.get(&flex_cache_key(child_box)) {
                            if let Some((stored_size, frag)) =
                                find_cached_fragment(entry, Size::new(target_width, target_height))
                            {
                                let mut cloned = (*frag).clone();
                                let intrinsic_size = fragment_subtree_size(&cloned);
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
                                        } else if !manual_row_positions {
                                            fallback_cursor_x = child_loc_x;
                                        }
                                    } else {
                                        manual_row_positions = false;
                                        fallback_cursor_x = child_loc_x;
                                    }
                                    if manual_row_positions {
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
                                    if manual_row_positions {
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
                                let log_child_ids = LOG_CHILD_IDS.get_or_init(|| {
                                    std::env::var("FASTR_LOG_FLEX_CHILD_IDS")
                                        .ok()
                                        .map(|s| {
                                            s.split(',')
                                                .filter_map(|tok| tok.trim().parse::<usize>().ok())
                                                .collect()
                                        })
                                        .unwrap_or_default()
                                });
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
                                cloned.bounds = Rect::new(
                                    Point::new(origin_x, origin_y),
                                    Size::new(resolved_width, resolved_height),
                                );
                                reused = Some(cloned);
                            }
                        }
                    }
                }

                let mut final_fragment: Option<FragmentNode> = None;
                if let Some(child_fragment) = reused {
                    final_fragment = Some(child_fragment);
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
                    let fc_type = child_box.formatting_context().unwrap_or(FormattingContextType::Block);
                    let fc = factory.create(fc_type);
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
                    let inline_base = if rect_w.is_finite() && rect_w > eps {
                        Some(rect_w)
                    } else {
                        None
                    };
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
                        LayoutConstraints::new(width, height).with_inline_percentage_base(inline_base)
                    } else {
                        LayoutConstraints::new(
                            CrateAvailableSpace::Definite(layout_width),
                            CrateAvailableSpace::Definite(layout_height),
                        )
                        .with_inline_percentage_base(inline_base)
                    };
                    let mut child_fragment = fc.layout(&layout_child, &child_constraints)?;
                    flex_profile::record_node_layout(child_box.id, selector_for_profile.as_deref(), node_timer);
                    let intrinsic_size = fragment_subtree_size(&child_fragment);

                    if !trace_flex_text_ids().is_empty() && trace_flex_text_ids().contains(&child_box.id) {
                        let mut text_count = 0;
                        fn walk(node: &FragmentNode, count: &mut usize) {
                            if matches!(node.content, FragmentContent::Text { .. }) {
                                *count += 1;
                            }
                            for child in &node.children {
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
                    if (main_axis_is_row && layout_width <= eps) || (!main_axis_is_row && layout_height <= eps) {
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
                        }
                        .with_inline_percentage_base(inline_base);
                        let mc_timer = flex_profile::node_timer();
                        let mc_selector = mc_timer
                            .as_ref()
                            .and_then(|_| child_box.debug_info.as_ref().map(|d| d.to_selector()));
                        if let Ok(mut mc_fragment) = fc.layout(&layout_child, &mc_constraints) {
                            flex_profile::record_node_layout(child_box.id, mc_selector.as_deref(), mc_timer);
                            let mut mc_size = fragment_subtree_size(&mc_fragment);
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
                                } else if !manual_row_positions {
                                    fallback_cursor_x = child_loc_x;
                                }
                            } else {
                                manual_row_positions = false;
                                fallback_cursor_x = child_loc_x;
                            }
                            if manual_row_positions {
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
                            if manual_row_positions {
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
                        let log_child_ids = LOG_CHILD_IDS.get_or_init(|| {
                            std::env::var("FASTR_LOG_FLEX_CHILD_IDS")
                                .ok()
                                .map(|s| {
                                    s.split(',')
                                        .filter_map(|tok| tok.trim().parse::<usize>().ok())
                                        .collect()
                                })
                                .unwrap_or_default()
                        });
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
        children = unordered_children.into_iter().map(|(_, _, frag)| frag).collect();
        #[cfg(test)]
        if children.is_empty() {
            let keys: Vec<usize> = node_map.keys().map(|k| *k as usize).collect();
            let child_ptrs: Vec<usize> = box_node.children.iter().map(|c| c as *const _ as usize).collect();
            eprintln!(
                "[flex-debug-empty-children] box_id={} keys={:?} child_ptrs={:?}",
                box_node.id, keys, child_ptrs
            );
        }

        // If Taffy reported non-increasing positions along the main axis, fall back to a simple
        // manual placement using the resolved fragment widths/heights to avoid overlapping items.
        static LOG_OVERFLOW: OnceLock<bool> = OnceLock::new();
        static LOG_OVERFLOW_IDS: OnceLock<Vec<usize>> = OnceLock::new();
        static OVERFLOW_COUNTS: OnceLock<Mutex<std::collections::HashMap<usize, usize>>> = OnceLock::new();
        let log_overflow = *LOG_OVERFLOW.get_or_init(|| {
            std::env::var("FASTR_LOG_FLEX_OVERFLOW")
                .map(|v| v != "0")
                .unwrap_or(false)
        });
        let log_overflow_ids = LOG_OVERFLOW_IDS.get_or_init(|| {
            std::env::var("FASTR_LOG_FLEX_OVERFLOW_IDS")
                .ok()
                .map(|s| {
                    s.split(',')
                        .filter_map(|tok| tok.trim().parse::<usize>().ok())
                        .collect()
                })
                .unwrap_or_default()
        });
        if main_axis_is_row {
            // Re-run manual row placement when Taffy positions items beyond the container width.
            // Even when overflow is expected (wide items), their starting position should remain
            // at the row origin rather than drifting far to the right.
            let container_w = if rect.width().is_finite() && rect.width() > wrap_eps {
                rect.width()
            } else {
                self.viewport_size.width
            };
            let mut max_child_x = children.iter().map(|c| c.bounds.max_x()).fold(0.0, f32::max);
            static LOG_SHRINK_IDS: OnceLock<Vec<usize>> = OnceLock::new();
            let log_shrink_ids = LOG_SHRINK_IDS.get_or_init(|| {
                std::env::var("FASTR_LOG_FLEX_SHRINK_IDS")
                    .ok()
                    .map(|s| {
                        s.split(',')
                            .filter_map(|tok| tok.trim().parse::<usize>().ok())
                            .collect()
                    })
                    .unwrap_or_default()
            });
            let log_this_shrink = !log_shrink_ids.is_empty() && log_shrink_ids.contains(&box_node.id);

            if max_child_x > container_w + 0.5 {
                let available = container_w.max(1.0).min(self.viewport_size.width);
                // Apply flex-shrink distribution to bring the total width back to the available span.
                let mut total_main = 0.0;
                let mut total_weight = 0.0;
                let mut child_count = 0usize;
                let mut box_lookup: std::collections::HashMap<usize, &BoxNode> = std::collections::HashMap::new();
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
            max_child_x = children.iter().map(|c| c.bounds.max_x()).fold(0.0, f32::max);
            // If all children have been pushed far to the left (beyond 2× the container width),
            // shift them back so the leftmost child starts at the origin. This guards against
            // runaway negative positions from broken intrinsic sizing or cached fragments.
            let min_child_x = children.iter().map(|c| c.bounds.x()).fold(f32::INFINITY, f32::min);
            if min_child_x.is_finite() {
                let clamp_width = container_w.max(self.viewport_size.width).max(1.0) * 2.0;
                if min_child_x < -clamp_width {
                    let dx = -min_child_x;
                    for child in &mut children {
                        child.bounds =
                            Rect::new(Point::new(child.bounds.x() + dx, child.bounds.y()), child.bounds.size);
                    }
                    max_child_x = children.iter().map(|c| c.bounds.max_x()).fold(0.0, f32::max);
                }
            }
            let should_log = log_overflow || (!log_overflow_ids.is_empty() && log_overflow_ids.contains(&box_node.id));
            if should_log && max_child_x > container_w + 0.5 {
                let mut counts = OVERFLOW_COUNTS
                    .get_or_init(|| Mutex::new(std::collections::HashMap::new()))
                    .lock()
                    .ok();
                let log_allowed = counts
                    .as_mut()
                    .map(|map| {
                        let counter = map.entry(box_node.id).or_insert(0);
                        let allowed = *counter < 2;
                        *counter += 1;
                        allowed
                    })
                    .unwrap_or(true);
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
                            | crate::tree::fragment_tree::FragmentContent::Inline { box_id, .. } => box_id.clone(),
                            crate::tree::fragment_tree::FragmentContent::Replaced { box_id, .. } => box_id.clone(),
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
            let max_child_y = children.iter().map(|c| c.bounds.max_y()).fold(0.0, f32::max);
            let should_log = log_overflow || (!log_overflow_ids.is_empty() && log_overflow_ids.contains(&box_node.id));
            if should_log && max_child_y > container_h * 1.5 {
                let mut counts = OVERFLOW_COUNTS
                    .get_or_init(|| Mutex::new(std::collections::HashMap::new()))
                    .lock()
                    .ok();
                let log_allowed = counts
                    .as_mut()
                    .map(|map| {
                        let counter = map.entry(box_node.id).or_insert(0);
                        let allowed = *counter < 2;
                        *counter += 1;
                        allowed
                    })
                    .unwrap_or(true);
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
                            | crate::tree::fragment_tree::FragmentContent::Inline { box_id, .. } => box_id.clone(),
                            crate::tree::fragment_tree::FragmentContent::Replaced { box_id, .. } => box_id.clone(),
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
            let max_child_x = children.iter().map(|c| c.bounds.max_x()).fold(0.0, f32::max);
            if max_child_x > container_w + 0.5 {
                let available = container_w.max(1.0).min(self.viewport_size.width);
                for child in &mut children {
                    let w = child.bounds.width().min(available);
                    child.bounds = Rect::new(Point::new(0.0, child.bounds.y()), Size::new(w, child.bounds.height()));
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
            let min_child_x = children.iter().map(|c| c.bounds.x()).fold(f32::INFINITY, f32::min);
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
        if main_axis_is_row && matches!(box_node.style.flex_wrap, FlexWrap::NoWrap) && !children.is_empty() {
            let min_child_x = children.iter().map(|c| c.bounds.x()).fold(f32::INFINITY, f32::min);
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
            let min_x = children.iter().map(|c| c.bounds.x()).fold(f32::INFINITY, f32::min);
            let max_x = children
                .iter()
                .map(|c| c.bounds.max_x())
                .fold(f32::NEG_INFINITY, f32::max);
            let drift_limit = rect.width().max(1.0) * 4.0;
            if min_x.is_finite() && min_x > drift_limit && max_x.is_finite() {
                let dx = min_x;
                static LOG_DRIFT: OnceLock<bool> = OnceLock::new();
                let log_drift =
                    *LOG_DRIFT.get_or_init(|| std::env::var("FASTR_LOG_FLEX_DRIFT").map(|v| v != "0").unwrap_or(false));
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
                    child.bounds = Rect::new(Point::new(child.bounds.x() - dx, child.bounds.y()), child.bounds.size);
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
                    child.bounds = Rect::new(Point::new(child.bounds.x() - dx, child.bounds.y()), child.bounds.size);
                }
            }
        }

        // Final clamp: only adjust children that overflow the container bounds. Keep normal in-bounds
        // items unchanged to avoid shrinking legitimate content (e.g., images taller than the parent).
        if rect.width() > 0.0 && rect.height() > 0.0 {
            let max_w = rect.width().min(self.viewport_size.width.max(0.0));
            let max_h = rect.height();
            let eps = 0.5;
            let allow_row_overflow = main_axis_is_row && matches!(box_node.style.flex_wrap, FlexWrap::NoWrap);
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
                        child.bounds =
                            Rect::new(Point::new(child.bounds.x() + dx, child.bounds.y()), child.bounds.size);
                    }
                } else if min_x.is_finite() && min_x > max_w + eps {
                    // All children start past the container's inline end; flex rows still start at the
                    // line's origin even when overflow is allowed, so translate them back to x=0.
                    let dx = -min_x;
                    for child in &mut children {
                        child.bounds =
                            Rect::new(Point::new(child.bounds.x() + dx, child.bounds.y()), child.bounds.size);
                    }
                }
                if min_y.is_finite() && min_y < -eps {
                    let dy = -min_y;
                    for child in &mut children {
                        child.bounds =
                            Rect::new(Point::new(child.bounds.x(), child.bounds.y() + dy), child.bounds.size);
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
                if min_x.is_finite() && min_y.is_finite() && (min_x >= max_w - eps || min_y >= max_h - eps) {
                    let dx = if min_x.is_finite() { -min_x.max(0.0) } else { 0.0 };
                    let dy = if min_y.is_finite() { -min_y.max(0.0) } else { 0.0 };
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
                            child.bounds =
                                Rect::new(Point::new(child.bounds.x() + dx, child.bounds.y()), child.bounds.size);
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
    fn constraints_to_available_space(&self, constraints: &LayoutConstraints) -> taffy::geometry::Size<AvailableSpace> {
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
        if constraints.inline_percentage_base.is_none() {
            constraints.inline_percentage_base = inline_percentage_base.or_else(|| match available.width {
                AvailableSpace::Definite(w) => Some(w),
                _ => None,
            });
        }
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
            for child in &node.children {
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
        fn walk(node: &FragmentNode, offset: Point, min: &mut Point, max: &mut Point, found: &mut bool) {
            for child in &node.children {
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
            Some(Size::new((max.x - min.x).max(0.0), (max.y - min.y).max(0.0)))
        }
    }

    /// Returns the content-box size for a laid-out fragment, stripping padding and borders.
    fn content_box_size(&self, fragment: &FragmentNode, style: &ComputedStyle, percentage_base: f32) -> Size {
        let padding_left = self.resolve_length_for_width(style.padding_left, percentage_base, style);
        let padding_right = self.resolve_length_for_width(style.padding_right, percentage_base, style);
        let padding_top = self.resolve_length_for_width(style.padding_top, percentage_base, style);
        let padding_bottom = self.resolve_length_for_width(style.padding_bottom, percentage_base, style);

        let border_left = self.resolve_length_for_width(style.border_left_width, percentage_base, style);
        let border_right = self.resolve_length_for_width(style.border_right_width, percentage_base, style);
        let border_top = self.resolve_length_for_width(style.border_top_width, percentage_base, style);
        let border_bottom = self.resolve_length_for_width(style.border_bottom_width, percentage_base, style);

        let content_width =
            (fragment.bounds.width() - padding_left - padding_right - border_left - border_right).max(0.0);
        let content_height =
            (fragment.bounds.height() - padding_top - padding_bottom - border_top - border_bottom).max(0.0);

        Size::new(content_width, content_height)
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
            WritingMode::VerticalRl | WritingMode::VerticalLr | WritingMode::SidewaysRl | WritingMode::SidewaysLr => {
                true
            }
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

    fn align_items_to_taffy(&self, align: AlignItems, axis_positive: bool) -> Option<taffy::style::AlignItems> {
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

    fn align_self_to_taffy(&self, align: Option<AlignItems>, axis_positive: bool) -> Option<taffy::style::AlignItems> {
        align.and_then(|a| self.align_items_to_taffy(a, axis_positive))
    }

    fn align_content_to_taffy(&self, align: AlignContent, axis_positive: bool) -> Option<taffy::style::AlignContent> {
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

    fn length_option_to_lpa(&self, len: Option<&Length>, style: &ComputedStyle) -> LengthPercentageAuto {
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

    fn resolve_length_for_width(&self, length: Length, percentage_base: f32, style: &ComputedStyle) -> f32 {
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
    use crate::style::types::{AlignItems, AspectRatio, Overflow, ScrollbarWidth};
    use crate::style::values::Length;
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

        assert_eq!(taffy_style.scrollbar_width, resolve_scrollbar_width(&node.style));
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

        assert_ne!(fp_a, fp_b, "scrollbar width should affect flex style fingerprint");
        assert_ne!(fp_a, fp_c, "overflow should affect flex style fingerprint");
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
        let container = BoxNode::new_block(Arc::new(container_style), FormattingContextType::Flex, vec![abs_child]);

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
        let container = BoxNode::new_block(Arc::new(container_style), FormattingContextType::Flex, vec![abs_child]);

        let cb_rect = Rect::from_xywh(20.0, 30.0, 150.0, 150.0);
        let viewport = Size::new(300.0, 300.0);
        let cb = ContainingBlock::with_viewport(cb_rect, viewport);
        let fc = FlexFormattingContext::with_viewport_and_cb(
            viewport,
            cb,
            FontContext::new(),
            std::sync::Arc::new(std::sync::Mutex::new(FlexMeasureCache::new())),
            std::sync::Arc::new(std::sync::Mutex::new(std::collections::HashMap::new())),
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
        let item1 = BoxNode::new_block(create_item_style(100.0, 50.0), FormattingContextType::Block, vec![]);
        let item2 = BoxNode::new_block(create_item_style(100.0, 50.0), FormattingContextType::Block, vec![]);
        let item3 = BoxNode::new_block(create_item_style(100.0, 50.0), FormattingContextType::Block, vec![]);

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

        let item1 = BoxNode::new_block(create_item_style(100.0, 50.0), FormattingContextType::Block, vec![]);
        let item2 = BoxNode::new_block(create_item_style(100.0, 75.0), FormattingContextType::Block, vec![]);
        let item3 = BoxNode::new_block(create_item_style(100.0, 25.0), FormattingContextType::Block, vec![]);

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
        let item2 = BoxNode::new_block(create_item_style(100.0, 50.0), FormattingContextType::Block, vec![]);

        let container = BoxNode::new_block(create_flex_style(), FormattingContextType::Flex, vec![item1, item2]);

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
        let item1 = BoxNode::new_block(create_item_style(250.0, 50.0), FormattingContextType::Block, vec![]);
        let item2 = BoxNode::new_block(create_item_style(250.0, 50.0), FormattingContextType::Block, vec![]);

        let container = BoxNode::new_block(create_flex_style(), FormattingContextType::Flex, vec![item1, item2]);

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

        let item1 = BoxNode::new_block(create_item_style(100.0, 50.0), FormattingContextType::Block, vec![]);
        let item2 = BoxNode::new_block(create_item_style(100.0, 50.0), FormattingContextType::Block, vec![]);
        let item3 = BoxNode::new_block(create_item_style(100.0, 50.0), FormattingContextType::Block, vec![]);

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
        let item1 = BoxNode::new_block(create_item_style(100.0, 50.0), FormattingContextType::Block, vec![]);
        let item2 = BoxNode::new_block(create_item_style(100.0, 100.0), FormattingContextType::Block, vec![]);
        let item3 = BoxNode::new_block(create_item_style(100.0, 74.0), FormattingContextType::Block, vec![]);

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

        let container = BoxNode::new_block(Arc::new(container_style), FormattingContextType::Flex, vec![item]);

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

        let child1 = BoxNode::new_block(create_item_style(20.0, 10.0), FormattingContextType::Block, vec![]);
        let child2 = BoxNode::new_block(create_item_style(20.0, 10.0), FormattingContextType::Block, vec![]);

        let container = BoxNode::new_block(Arc::new(style), FormattingContextType::Flex, vec![child1, child2]);
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

        let child = BoxNode::new_block(create_item_style(20.0, 10.0), FormattingContextType::Block, vec![]);
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

        let child1 = BoxNode::new_block(create_item_style(10.0, 10.0), FormattingContextType::Block, vec![]);
        let child2 = BoxNode::new_block(create_item_style(10.0, 10.0), FormattingContextType::Block, vec![]);

        let container = BoxNode::new_block(Arc::new(style), FormattingContextType::Flex, vec![child1, child2]);

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
                BoxNode::new_block(create_item_style(10.0, 10.0), FormattingContextType::Block, vec![]),
                BoxNode::new_block(create_item_style(10.0, 10.0), FormattingContextType::Block, vec![]),
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

        let container = BoxNode::new_block(Arc::new(container_style), FormattingContextType::Flex, vec![item]);

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

        let container = BoxNode::new_block(Arc::new(container_style), FormattingContextType::Flex, vec![item]);

        let constraints = LayoutConstraints::definite(300.0, 200.0);
        let fragment = fc.layout(&container, &constraints).unwrap();

        assert_eq!(fragment.children[0].bounds.width(), 120.0);
        assert_eq!(fragment.children[0].bounds.height(), 40.0);
    }

    #[test]
    fn test_intrinsic_sizing_max_content() {
        let fc = FlexFormattingContext::new();

        let item1 = BoxNode::new_block(create_item_style(100.0, 50.0), FormattingContextType::Block, vec![]);
        let item2 = BoxNode::new_block(create_item_style(150.0, 50.0), FormattingContextType::Block, vec![]);

        let container = BoxNode::new_block(create_flex_style(), FormattingContextType::Flex, vec![item1, item2]);

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
        let inner_item1 = BoxNode::new_block(create_item_style(50.0, 30.0), FormattingContextType::Block, vec![]);
        let inner_item2 = BoxNode::new_block(create_item_style(50.0, 30.0), FormattingContextType::Block, vec![]);

        let inner_container = BoxNode::new_block(
            create_flex_style(),
            FormattingContextType::Flex,
            vec![inner_item1, inner_item2],
        );

        // Outer flex container
        let outer_item = BoxNode::new_block(create_item_style(100.0, 50.0), FormattingContextType::Block, vec![]);

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

        assert_eq!(fc.flex_wrap_to_taffy(FlexWrap::NoWrap), taffy::style::FlexWrap::NoWrap);
        assert_eq!(fc.flex_wrap_to_taffy(FlexWrap::Wrap), taffy::style::FlexWrap::Wrap);
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
        assert_eq!(fc.length_to_dimension(&len_px, &style), Dimension::length(100.0));

        // Percentage values
        let len_percent = Length::percent(50.0);
        assert_eq!(fc.length_to_dimension(&len_percent, &style), Dimension::percent(0.5)); // 50% = 0.5

        // Auto (None)
        assert_eq!(fc.length_option_to_dimension(None, &style), Dimension::auto());
    }
}
