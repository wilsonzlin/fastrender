//! CSS Cascade and Style Application
//!
//! This module handles applying CSS rules to DOM nodes to produce
//! computed styles. It implements the CSS cascade algorithm.
//!
//! Reference: CSS Cascading and Inheritance Level 4
//! <https://www.w3.org/TR/css-cascade-4/>

use crate::css::parser::{parse_declarations, parse_stylesheet};
use crate::css::selectors::{PseudoElement, TextDirection};
use crate::css::types::{ContainerCondition, CssImportLoader, Declaration, StyleRule, StyleSheet};
use crate::dom::{resolve_first_strong_direction, with_target_fragment, DomNode, ElementRef};
use crate::geometry::Size;
use crate::style::defaults::{get_default_styles_for_element, parse_color_attribute, parse_dimension_attribute};
use crate::style::display::Display;
use crate::style::grid::finalize_grid_placement;
use crate::style::media::MediaContext;
use crate::style::properties::{apply_declaration_with_base, resolve_pending_logical_properties, with_image_set_dpr};
use crate::style::types::ContainerType;
use crate::style::values::{Length, LengthUnit};
use crate::style::{normalize_language_tag, ComputedStyle, Direction};
use selectors::context::{QuirksMode, SelectorCaches};
use selectors::matching::{matches_selector, MatchingContext, MatchingMode};
use selectors::parser::Selector;
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::OnceLock;
use std::time::Instant;

/// User-agent stylesheet containing default browser styles
const USER_AGENT_STYLESHEET: &str = include_str!("../user_agent.css");

// Optional cascade profiling for large-page performance analysis.
// Enable with FASTR_CASCADE_PROFILE=1.
static CASCADE_PROFILE_NODES: AtomicU64 = AtomicU64::new(0);
static CASCADE_PROFILE_RULE_CANDIDATES: AtomicU64 = AtomicU64::new(0);
static CASCADE_PROFILE_RULE_MATCHES: AtomicU64 = AtomicU64::new(0);
static CASCADE_PROFILE_FIND_TIME_NS: AtomicU64 = AtomicU64::new(0);
static CASCADE_PROFILE_DECL_TIME_NS: AtomicU64 = AtomicU64::new(0);
static CASCADE_PROFILE_PSEUDO_TIME_NS: AtomicU64 = AtomicU64::new(0);
static CASCADE_PROFILE_ENABLED: OnceLock<bool> = OnceLock::new();

fn cascade_profile_enabled() -> bool {
    *CASCADE_PROFILE_ENABLED.get_or_init(|| {
        std::env::var("FASTR_CASCADE_PROFILE")
            .map(|v| v != "0")
            .unwrap_or(false)
    })
}

fn reset_cascade_profile() {
    CASCADE_PROFILE_NODES.store(0, Ordering::Relaxed);
    CASCADE_PROFILE_RULE_CANDIDATES.store(0, Ordering::Relaxed);
    CASCADE_PROFILE_RULE_MATCHES.store(0, Ordering::Relaxed);
    CASCADE_PROFILE_FIND_TIME_NS.store(0, Ordering::Relaxed);
    CASCADE_PROFILE_DECL_TIME_NS.store(0, Ordering::Relaxed);
    CASCADE_PROFILE_PSEUDO_TIME_NS.store(0, Ordering::Relaxed);
}

fn log_cascade_profile(elapsed_ms: f64) {
    let nodes = CASCADE_PROFILE_NODES.load(Ordering::Relaxed);
    let candidates = CASCADE_PROFILE_RULE_CANDIDATES.load(Ordering::Relaxed);
    let matches = CASCADE_PROFILE_RULE_MATCHES.load(Ordering::Relaxed);
    let find_time_ns = CASCADE_PROFILE_FIND_TIME_NS.load(Ordering::Relaxed);
    let decl_time_ns = CASCADE_PROFILE_DECL_TIME_NS.load(Ordering::Relaxed);
    let pseudo_time_ns = CASCADE_PROFILE_PSEUDO_TIME_NS.load(Ordering::Relaxed);
    let find_ms = find_time_ns as f64 / 1_000_000.0;
    let decl_ms = decl_time_ns as f64 / 1_000_000.0;
    let pseudo_ms = pseudo_time_ns as f64 / 1_000_000.0;
    let avg_candidates = if nodes > 0 {
        candidates as f64 / nodes as f64
    } else {
        0.0
    };
    let avg_matches = if nodes > 0 { matches as f64 / nodes as f64 } else { 0.0 };
    eprintln!(
        "cascade profile: total_ms={:.2} nodes={} candidates={} matches={} avg_candidates={:.1} avg_matches={:.1} find_ms={:.2} decl_ms={:.2} pseudo_ms={:.2}",
        elapsed_ms, nodes, candidates, matches, avg_candidates, avg_matches, find_ms, decl_ms, pseudo_ms
    );
}

fn record_node_visit(node: &DomNode) {
    if cascade_profile_enabled() && node.is_element() {
        CASCADE_PROFILE_NODES.fetch_add(1, Ordering::Relaxed);
    }
}

fn record_matching_stats(candidates: usize, matches: usize, elapsed: Option<std::time::Duration>) {
    if !cascade_profile_enabled() {
        return;
    }
    CASCADE_PROFILE_RULE_CANDIDATES.fetch_add(candidates as u64, Ordering::Relaxed);
    CASCADE_PROFILE_RULE_MATCHES.fetch_add(matches as u64, Ordering::Relaxed);
    if let Some(dur) = elapsed {
        CASCADE_PROFILE_FIND_TIME_NS.fetch_add(dur.as_nanos() as u64, Ordering::Relaxed);
    }
}

fn record_declaration_time(elapsed: std::time::Duration) {
    if cascade_profile_enabled() {
        CASCADE_PROFILE_DECL_TIME_NS.fetch_add(elapsed.as_nanos() as u64, Ordering::Relaxed);
    }
}

fn record_pseudo_time(elapsed: std::time::Duration) {
    if cascade_profile_enabled() {
        CASCADE_PROFILE_PSEUDO_TIME_NS.fetch_add(elapsed.as_nanos() as u64, Ordering::Relaxed);
    }
}

/// The origin of a style rule for cascade ordering.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum StyleOrigin {
    UserAgent,
    Author,
    Inline,
}

impl StyleOrigin {
    fn rank(self) -> u8 {
        match self {
            StyleOrigin::UserAgent => 0,
            StyleOrigin::Author | StyleOrigin::Inline => 1,
        }
    }
}

/// A style rule annotated with its origin and document order.
#[derive(Clone)]
struct CascadeRule<'a> {
    origin: StyleOrigin,
    order: usize,
    rule: &'a StyleRule,
    layer_order: Vec<u32>,
    container_conditions: Vec<ContainerCondition>,
}

/// Simple index over the rightmost compound selector to prune rule matching.
struct IndexedSelector<'a> {
    rule_idx: usize,
    selector: &'a Selector<crate::css::selectors::FastRenderSelectorImpl>,
    specificity: u32,
}

struct PseudoBuckets {
    by_id: HashMap<String, Vec<usize>>,
    by_class: HashMap<String, Vec<usize>>,
    by_tag: HashMap<String, Vec<usize>>,
    by_attr: HashMap<String, Vec<usize>>,
    universal: Vec<usize>,
}

impl PseudoBuckets {
    fn new() -> Self {
        Self {
            by_id: HashMap::new(),
            by_class: HashMap::new(),
            by_tag: HashMap::new(),
            by_attr: HashMap::new(),
            universal: Vec::new(),
        }
    }
}

struct RuleIndex<'a> {
    rules: Vec<CascadeRule<'a>>,
    selectors: Vec<IndexedSelector<'a>>,
    by_id: HashMap<String, Vec<usize>>,
    by_class: HashMap<String, Vec<usize>>,
    by_tag: HashMap<String, Vec<usize>>,
    by_attr: HashMap<String, Vec<usize>>,
    universal: Vec<usize>,
    pseudo_selectors: Vec<IndexedSelector<'a>>,
    pseudo_buckets: HashMap<PseudoElement, PseudoBuckets>,
}

fn selector_key(selector: &Selector<crate::css::selectors::FastRenderSelectorImpl>) -> SelectorKey {
    use selectors::parser::Component;

    let mut id: Option<String> = None;
    let mut class: Option<String> = None;
    let mut tag: Option<String> = None;
    let mut attr: Option<String> = None;

    let mut iter = selector.iter();
    loop {
        if let Some(component) = iter.next() {
            match component {
                Component::ID(ident) => id = Some(ident.0.clone()),
                Component::Class(cls) => class = Some(cls.0.clone()),
                Component::LocalName(local) => tag = Some(local.lower_name.0.clone()),
                Component::AttributeInNoNamespaceExists { local_name_lower, .. } => {
                    attr.get_or_insert(local_name_lower.0.clone());
                }
                Component::AttributeInNoNamespace { local_name, .. } => {
                    attr.get_or_insert(local_name.0.clone());
                }
                Component::AttributeOther(other) => {
                    if other.namespace.is_none() {
                        attr.get_or_insert(other.local_name_lower.0.clone());
                    }
                }
                _ => {}
            }
            continue;
        }
        // Finished the rightmost sequence; stop before traversing combinators.
        let _ = iter.next_sequence();
        break;
    }

    if let Some(id) = id {
        SelectorKey::Id(id)
    } else if let Some(cls) = class {
        SelectorKey::Class(cls)
    } else if let Some(tag) = tag {
        SelectorKey::Tag(tag)
    } else if let Some(attr) = attr {
        SelectorKey::Attribute(attr)
    } else {
        SelectorKey::Universal
    }
}

#[derive(Debug)]
enum SelectorKey {
    Id(String),
    Class(String),
    Tag(String),
    Attribute(String),
    Universal,
}

impl<'a> RuleIndex<'a> {
    fn new(rules: Vec<CascadeRule<'a>>) -> Self {
        let mut index = RuleIndex {
            rules: Vec::new(),
            selectors: Vec::new(),
            by_id: HashMap::new(),
            by_class: HashMap::new(),
            by_tag: HashMap::new(),
            by_attr: HashMap::new(),
            universal: Vec::new(),
            pseudo_selectors: Vec::new(),
            pseudo_buckets: HashMap::new(),
        };

        for rule in rules {
            let rule_idx = index.rules.len();
            index.rules.push(rule);
            let stored_rule = &index.rules[rule_idx];

            for selector in stored_rule.rule.selectors.slice().iter() {
                if let Some(pe) = selector.pseudo_element() {
                    let bucket = index
                        .pseudo_buckets
                        .entry(pe.clone())
                        .or_insert_with(PseudoBuckets::new);
                    let selector_idx = index.pseudo_selectors.len();
                    index.pseudo_selectors.push(IndexedSelector {
                        rule_idx,
                        selector,
                        specificity: selector.specificity(),
                    });
                    match selector_key(selector) {
                        SelectorKey::Id(id) => bucket.by_id.entry(id).or_default().push(selector_idx),
                        SelectorKey::Class(cls) => bucket.by_class.entry(cls).or_default().push(selector_idx),
                        SelectorKey::Tag(tag) => bucket.by_tag.entry(tag).or_default().push(selector_idx),
                        SelectorKey::Attribute(attr) => bucket.by_attr.entry(attr).or_default().push(selector_idx),
                        SelectorKey::Universal => bucket.universal.push(selector_idx),
                    }
                    continue;
                }

                let selector_idx = index.selectors.len();
                index.selectors.push(IndexedSelector {
                    rule_idx,
                    selector,
                    specificity: selector.specificity(),
                });
                match selector_key(selector) {
                    SelectorKey::Id(id) => index.by_id.entry(id).or_default().push(selector_idx),
                    SelectorKey::Class(cls) => index.by_class.entry(cls).or_default().push(selector_idx),
                    SelectorKey::Tag(tag) => index.by_tag.entry(tag).or_default().push(selector_idx),
                    SelectorKey::Attribute(attr) => index.by_attr.entry(attr).or_default().push(selector_idx),
                    SelectorKey::Universal => index.universal.push(selector_idx),
                }
            }
        }

        index
    }

    fn has_pseudo_rules(&self, pseudo: &PseudoElement) -> bool {
        self.pseudo_buckets.contains_key(pseudo)
    }

    fn selector_candidates(&self, node: &DomNode, out: &mut Vec<usize>) {
        out.clear();
        if !node.is_element() {
            return;
        }

        if let Some(id) = node.get_attribute("id") {
            if let Some(list) = self.by_id.get(&id) {
                out.extend(list.iter().copied());
            }
        }

        if let Some(class_attr) = node.get_attribute("class") {
            for cls in class_attr.split_whitespace() {
                if !cls.is_empty() {
                    if let Some(list) = self.by_class.get(cls) {
                        out.extend(list.iter().copied());
                    }
                }
            }
        }

        if let Some(tag) = node.tag_name() {
            if let Some(list) = self.by_tag.get(tag) {
                out.extend(list.iter().copied());
            }
        }

        for (name, _) in node.attributes_iter() {
            let key = name.to_ascii_lowercase();
            if let Some(list) = self.by_attr.get(&key) {
                out.extend(list.iter().copied());
            }
        }

        out.extend(self.universal.iter().copied());
    }

    fn pseudo_candidates(&self, node: &DomNode, pseudo: &PseudoElement, out: &mut Vec<usize>) {
        out.clear();
        if !node.is_element() {
            return;
        }

        let Some(bucket) = self.pseudo_buckets.get(pseudo) else {
            return;
        };

        if let Some(id) = node.get_attribute("id") {
            if let Some(list) = bucket.by_id.get(&id) {
                out.extend(list.iter().copied());
            }
        }

        if let Some(class_attr) = node.get_attribute("class") {
            for cls in class_attr.split_whitespace() {
                if !cls.is_empty() {
                    if let Some(list) = bucket.by_class.get(cls) {
                        out.extend(list.iter().copied());
                    }
                }
            }
        }

        if let Some(tag) = node.tag_name() {
            if let Some(list) = bucket.by_tag.get(tag) {
                out.extend(list.iter().copied());
            }
        }

        for (name, _) in node.attributes_iter() {
            let key = name.to_ascii_lowercase();
            if let Some(list) = bucket.by_attr.get(&key) {
                out.extend(list.iter().copied());
            }
        }

        out.extend(bucket.universal.iter().copied());
    }
}

/// A matched rule with the specificity of the selector that applied.
#[derive(Clone)]
struct MatchedRule<'a> {
    origin: StyleOrigin,
    specificity: u32,
    order: usize,
    layer_order: Vec<u32>,
    declarations: Cow<'a, [Declaration]>,
}

/// A single declaration annotated with cascade ordering keys.
struct MatchedDeclaration<'a> {
    important: bool,
    origin: StyleOrigin,
    specificity: u32,
    rule_order: usize,
    decl_order: usize,
    layer_order: Vec<u32>,
    declaration: Cow<'a, Declaration>,
}

const INLINE_SPECIFICITY: u32 = 1 << 30;
const INLINE_RULE_ORDER: usize = usize::MAX / 2;

/// A styled DOM node with computed CSS styles
#[derive(Debug, Clone)]
pub struct StyledNode {
    /// Stable identifier for this node within the DOM traversal (pre-order index).
    pub node_id: usize,
    pub node: DomNode,
    pub styles: ComputedStyle,
    /// Styles for ::before pseudo-element (if content is set)
    pub before_styles: Option<ComputedStyle>,
    /// Styles for ::after pseudo-element (if content is set)
    pub after_styles: Option<ComputedStyle>,
    /// Styles for ::marker pseudo-element (list items only)
    pub marker_styles: Option<ComputedStyle>,
    pub children: Vec<StyledNode>,
}

fn count_styled_nodes(node: &StyledNode) -> usize {
    1 + node.children.iter().map(count_styled_nodes).sum::<usize>()
}

/// Captured container size and metadata for container query evaluation.
#[derive(Debug, Clone)]
pub struct ContainerQueryInfo {
    pub inline_size: f32,
    pub block_size: f32,
    pub container_type: ContainerType,
    pub names: Vec<String>,
    pub font_size: f32,
}

/// Map of styled-node ids to their resolved container query metrics.
#[derive(Debug, Clone)]
pub struct ContainerQueryContext {
    pub base_media: MediaContext,
    pub containers: HashMap<usize, ContainerQueryInfo>,
}

impl ContainerQueryContext {
    /// Returns true when all container conditions match against the nearest qualifying containers.
    pub fn matches(&self, node_id: usize, ancestor_ids: &[usize], conditions: &[ContainerCondition]) -> bool {
        if conditions.is_empty() {
            return true;
        }

        for condition in conditions {
            let container = match self.find_container(node_id, ancestor_ids, condition) {
                Some(info) => info,
                None => return false,
            };

            let mut ctx = self.base_media.clone();
            ctx.viewport_width = container.inline_size;
            ctx.viewport_height = match container.container_type {
                ContainerType::InlineSize => f32::NAN,
                _ => container.block_size,
            };
            ctx.base_font_size = container.font_size;
            if !ctx.evaluate(&condition.query) {
                return false;
            }
        }

        true
    }

    fn find_container<'a>(
        &'a self,
        node_id: usize,
        ancestor_ids: &[usize],
        condition: &ContainerCondition,
    ) -> Option<&'a ContainerQueryInfo> {
        // Per spec the query container is the nearest ancestor that establishes a container;
        // allow the current element itself if it is a container.
        let mut search: Vec<usize> = Vec::with_capacity(ancestor_ids.len() + 1);
        search.extend_from_slice(ancestor_ids);
        search.push(node_id);

        for &candidate in search.iter().rev() {
            let info = match self.containers.get(&candidate) {
                Some(info) => info,
                None => continue,
            };

            match info.container_type {
                ContainerType::Size | ContainerType::InlineSize => {}
                _ => continue,
            }

            if let Some(name) = &condition.name {
                if !info.names.iter().any(|n| n == name) {
                    continue;
                }
            }

            return Some(info);
        }

        None
    }
}

/// Apply styles to a DOM tree with default viewport (desktop)
///
/// This is the main entry point for CSS cascade. It uses a default
/// desktop viewport size for media query evaluation.
pub fn apply_styles(dom: &DomNode, stylesheet: &StyleSheet) -> StyledNode {
    apply_styles_with_target(dom, stylesheet, None)
}

/// Apply styles with an explicit target fragment for :target matching.
pub fn apply_styles_with_target(dom: &DomNode, stylesheet: &StyleSheet, target_fragment: Option<&str>) -> StyledNode {
    // Use default desktop viewport
    let media_ctx = MediaContext::screen(1200.0, 800.0);
    apply_styles_with_media_and_target(dom, stylesheet, &media_ctx, target_fragment)
}

/// Apply styles to a DOM tree with a specific media context
///
/// This allows controlling viewport size and other media features
/// for responsive rendering.
pub fn apply_styles_with_media(dom: &DomNode, stylesheet: &StyleSheet, media_ctx: &MediaContext) -> StyledNode {
    apply_styles_with_media_and_target(dom, stylesheet, media_ctx, None)
}

pub fn apply_styles_with_media_and_target(
    dom: &DomNode,
    stylesheet: &StyleSheet,
    media_ctx: &MediaContext,
    target_fragment: Option<&str>,
) -> StyledNode {
    apply_styles_with_media_target_and_imports(
        dom,
        stylesheet,
        media_ctx,
        target_fragment,
        None,
        None::<&str>,
        None,
        None,
        None,
    )
}

/// Apply styles with media context, optional :target, and optional import loader/base URL.
pub fn apply_styles_with_media_target_and_imports(
    dom: &DomNode,
    stylesheet: &StyleSheet,
    media_ctx: &MediaContext,
    target_fragment: Option<&str>,
    import_loader: Option<&dyn CssImportLoader>,
    base_url: Option<&str>,
    container_ctx: Option<&ContainerQueryContext>,
    container_scope: Option<&HashSet<usize>>,
    reuse_map: Option<&HashMap<usize, *const StyledNode>>,
) -> StyledNode {
    let profile_enabled = cascade_profile_enabled();
    let profile_start = profile_enabled.then(|| Instant::now());
    if profile_enabled {
        reset_cascade_profile();
    }
    let log_reuse = std::env::var("FASTR_LOG_CONTAINER_REUSE")
        .map(|v| v != "0" && !v.eq_ignore_ascii_case("false"))
        .unwrap_or(false);
    let mut reuse_counter: usize = 0;

    // Parse user-agent stylesheet
    let ua_stylesheet = parse_stylesheet(USER_AGENT_STYLESHEET).unwrap_or_else(|_| StyleSheet::new());

    // Resolve imports if a loader is provided
    let author_sheet = if let Some(loader) = import_loader {
        stylesheet.resolve_imports(loader, base_url, media_ctx)
    } else {
        stylesheet.clone()
    };

    // Collect applicable rules from both stylesheets
    // User-agent rules come first (lower priority)
    let ua_rules = ua_stylesheet.collect_style_rules(media_ctx);
    let author_rules = author_sheet.collect_style_rules(media_ctx);

    let mut all_rules: Vec<CascadeRule<'_>> = Vec::with_capacity(ua_rules.len() + author_rules.len());
    for (order, rule) in ua_rules.iter().enumerate() {
        all_rules.push(CascadeRule {
            origin: StyleOrigin::UserAgent,
            order,
            rule: rule.rule,
            layer_order: rule.layer_order.clone(),
            container_conditions: rule.container_conditions.clone(),
        });
    }
    let offset = all_rules.len();
    for (idx, rule) in author_rules.iter().enumerate() {
        all_rules.push(CascadeRule {
            origin: StyleOrigin::Author,
            order: offset + idx,
            rule: rule.rule,
            layer_order: rule.layer_order.clone(),
            container_conditions: rule.container_conditions.clone(),
        });
    }

    let rule_index = RuleIndex::new(all_rules);
    let styled = with_target_fragment(target_fragment, || {
        with_image_set_dpr(media_ctx.device_pixel_ratio, || {
            let mut selector_caches = SelectorCaches::default();
            let mut node_counter: usize = 1;
            let mut ancestor_ids: Vec<usize> = Vec::new();
            let mut reuse_counter_opt = if container_scope.is_some() || reuse_map.is_some() {
                Some(&mut reuse_counter)
            } else {
                None
            };
            apply_styles_internal(
                dom,
                &rule_index,
                &mut selector_caches,
                &ComputedStyle::default(),
                &ComputedStyle::default(),
                16.0,
                16.0,
                Size::new(media_ctx.viewport_width, media_ctx.viewport_height),
                &mut node_counter,
                &mut ancestor_ids,
                container_ctx,
                container_scope,
                reuse_map,
                reuse_counter_opt.as_deref_mut(),
            )
        })
    });

    if let (true, Some(start)) = (profile_enabled, profile_start) {
        log_cascade_profile(start.elapsed().as_secs_f64() * 1000.0);
    }
    if log_reuse {
        fn count_nodes(node: &StyledNode) -> usize {
            1 + node.children.iter().map(count_nodes).sum::<usize>()
        }
        let total = count_nodes(&styled);
        eprintln!(
            "[container-reuse] total_nodes={} reused_nodes={} reused_pct={:.1}%",
            total,
            reuse_counter,
            if total > 0 {
                (reuse_counter as f64 / total as f64) * 100.0
            } else {
                0.0
            }
        );
    }

    styled
}

fn append_presentational_hints<'a>(
    node: &DomNode,
    ancestors: &[&DomNode],
    parent_direction: Direction,
    matching_rules: &mut Vec<MatchedRule<'a>>,
) {
    if let Some(presentational_rule) = dir_presentational_hint(node, parent_direction, 0) {
        matching_rules.push(presentational_rule);
    }
    if let Some(presentational_rule) = list_type_presentational_hint(node, 1) {
        matching_rules.push(presentational_rule);
    }
    if let Some(presentational_rule) = alignment_presentational_hint(node, 2) {
        matching_rules.push(presentational_rule);
    }
    if let Some(presentational_rule) = dimension_presentational_hint(node, 3) {
        matching_rules.push(presentational_rule);
    }
    if let Some(presentational_rule) = bgcolor_presentational_hint(node, 4) {
        matching_rules.push(presentational_rule);
    }
    if let Some(presentational_rule) = border_presentational_hint(node, ancestors, 5) {
        matching_rules.push(presentational_rule);
    }
    if let Some(presentational_rule) = bordercolor_presentational_hint(node, 6) {
        matching_rules.push(presentational_rule);
    }
    if let Some(presentational_rule) = cellspacing_presentational_hint(node, 7) {
        matching_rules.push(presentational_rule);
    }
    if let Some(presentational_rule) = cellpadding_presentational_hint(node, ancestors, 8) {
        matching_rules.push(presentational_rule);
    }
    if let Some(presentational_rule) = replaced_alignment_presentational_hint(node, 9) {
        matching_rules.push(presentational_rule);
    }
    if let Some(presentational_rule) = nowrap_presentational_hint(node, 10) {
        matching_rules.push(presentational_rule);
    }
}

fn ua_default_rules(node: &DomNode, parent_direction: Direction) -> Vec<MatchedRule<'static>> {
    let mut rules = Vec::new();
    let tag = match node.tag_name() {
        Some(t) => t.to_ascii_lowercase(),
        None => return rules,
    };

    let mut add_rule = |css: &str, order: usize| {
        rules.push(MatchedRule {
            origin: StyleOrigin::UserAgent,
            specificity: 1, // tag selector specificity
            order,
            layer_order: vec![u32::MAX],
            declarations: Cow::Owned(parse_declarations(css)),
        })
    };

    match tag.as_str() {
        "a" if node.get_attribute("href").is_some() => {
            add_rule("color: rgb(0, 0, 238); text-decoration: underline; cursor: pointer;", 0);
        }
        "bdi" if node.get_attribute("dir").is_none() => {
            let resolved = resolve_first_strong_direction(node).map(|d| match d {
                TextDirection::Ltr => crate::style::types::Direction::Ltr,
                TextDirection::Rtl => crate::style::types::Direction::Rtl,
            });
            let dir_value = match resolved.unwrap_or(parent_direction) {
                crate::style::types::Direction::Rtl => "rtl",
                crate::style::types::Direction::Ltr => "ltr",
            };
            add_rule(
                &format!("unicode-bidi: isolate; direction: {};", dir_value),
                0,
            );
        }
        "bdo" if node.get_attribute("dir").is_none() => {
            add_rule("unicode-bidi: bidi-override; direction: ltr;", 0);
        }
        "h1" => add_rule("font-size: 2em; font-weight: bolder; margin-top: 0.67em; margin-bottom: 0.67em;", 0),
        "h2" => add_rule("font-size: 1.5em; font-weight: bolder; margin-top: 0.83em; margin-bottom: 0.83em;", 0),
        "h3" => add_rule("font-size: 1.17em; font-weight: bolder; margin-top: 1em; margin-bottom: 1em;", 0),
        "h4" => add_rule("font-size: 1em; font-weight: bolder; margin-top: 1.33em; margin-bottom: 1.33em;", 0),
        "h5" => add_rule("font-size: 0.83em; font-weight: bolder; margin-top: 1.67em; margin-bottom: 1.67em;", 0),
        "h6" => add_rule("font-size: 0.67em; font-weight: bolder; margin-top: 2.33em; margin-bottom: 2.33em;", 0),
        "p" => add_rule("margin-top: 1em; margin-bottom: 1em;", 0),
        "pre" => add_rule(
            "font-family: monospace; white-space: pre; margin-top: 1em; margin-bottom: 1em;",
            0,
        ),
        "code" | "kbd" | "samp" | "tt" => add_rule("font-family: monospace;", 0),
        "blockquote" => add_rule(
            "margin-top: 1em; margin-bottom: 1em; margin-left: 40px; margin-right: 40px;",
            0,
        ),
        "ul" => add_rule(
            "margin-top: 1em; margin-bottom: 1em; padding-left: 40px; list-style-type: disc;",
            0,
        ),
        "menu" | "dir" => add_rule(
            "display: block; margin-top: 1em; margin-bottom: 1em; padding-left: 40px; list-style-type: disc;",
            0,
        ),
        "ol" => add_rule(
            "margin-top: 1em; margin-bottom: 1em; padding-left: 40px; list-style-type: decimal;",
            0,
        ),
        "dd" => add_rule("margin-left: 40px;", 0),
        "hr" => add_rule(
            "box-sizing: content-box; height: 1px; margin: 0.5em auto; border: 1px inset; border-color: currentColor; color: gray;",
            0,
        ),
        "small" => add_rule("font-size: smaller;", 0),
        "big" => add_rule("font-size: larger;", 0),
        "sub" => add_rule("font-size: smaller; vertical-align: sub;", 0),
        "sup" => add_rule("font-size: smaller; vertical-align: super;", 0),
        "mark" => add_rule("background-color: yellow; color: black;", 0),
        "address" | "cite" | "dfn" | "var" | "em" | "i" => add_rule("font-style: italic;", 0),
        "ins" => add_rule("text-decoration: underline;", 0),
        "del" => add_rule("text-decoration: line-through;", 0),
        "q" => add_rule("quotes: '“' '”' '‘' '’';", 0),
        "details" => add_rule("display: block;", 0),
        "summary" => add_rule(
            "display: list-item; cursor: pointer; list-style-type: disclosure-closed; list-style-position: outside;",
            0,
        ),
        "u" => add_rule("text-decoration: underline;", 0),
        "s" | "strike" => add_rule("text-decoration: line-through;", 0),
        "center" => add_rule("text-align: center;", 0),
        "th" => add_rule("font-weight: bolder; text-align: center;", 0),
        "option" => add_rule("display: block; white-space: pre;", 0),
        "optgroup" => add_rule("display: block; white-space: pre; font-weight: bolder;", 0),
        "fieldset" => add_rule(
            "display: block; margin: 0.35em 2px 0.625em; padding: 0.35em 0.75em 0.625em; border: 2px groove rgb(192, 192, 192);",
            0,
        ),
        "legend" => add_rule("display: block; padding-left: 2px; padding-right: 2px;", 0),
        "textarea" => {
            add_rule(
                "font: inherit; color: inherit; border: 2px inset rgb(204,204,204); background: white; padding: 2px 3px; box-sizing: border-box; display: inline-block; cursor: text;",
                0,
            );
        }
        "input" => {
            let input_type = node
                .get_attribute("type")
                .map(|t| t.to_ascii_lowercase())
                .unwrap_or_else(|| "text".to_string());
            if input_type == "hidden" {
                rules.push(MatchedRule {
                    origin: StyleOrigin::UserAgent,
                    specificity: 11, // input[type=\"hidden\"] should outrank generic UA form rules
                    order: 0,
                    layer_order: vec![u32::MAX],
                    declarations: Cow::Owned(parse_declarations("display: none;")),
                });
                return rules;
            }
            add_rule(
                "font: inherit; color: inherit; border: 2px inset rgb(204,204,204); background: white; padding: 2px 3px; box-sizing: border-box; display: inline-block;",
                0,
            );
            if matches!(
                input_type.as_str(),
                "text" | "search" | "email" | "url" | "tel" | "password" | "number"
            ) {
                add_rule("cursor: text;", 1);
            } else if matches!(input_type.as_str(), "button" | "submit" | "reset") {
                add_rule("cursor: pointer;", 1);
            }
        }
        "select" | "button" => add_rule(
            "font: inherit; color: inherit; border: 2px inset rgb(204,204,204); background: white; padding: 2px 3px; box-sizing: border-box; display: inline-block;",
            0,
        ),
        "nobr" => add_rule("white-space: nowrap;", 0),
        _ => {}
    }

    if (tag == "abbr" || tag == "acronym") && node.get_attribute("title").is_some() {
        add_rule(
            "border-bottom-width: 1px; border-bottom-style: dotted; border-bottom-color: currentColor; cursor: help;",
            1,
        );
    }

    rules
}

fn apply_styles_internal(
    node: &DomNode,
    rules: &RuleIndex<'_>,
    selector_caches: &mut SelectorCaches,
    parent_styles: &ComputedStyle,
    parent_ua_styles: &ComputedStyle,
    root_font_size: f32,
    ua_root_font_size: f32,
    viewport: Size,
    node_counter: &mut usize,
    ancestor_ids: &mut Vec<usize>,
    container_ctx: Option<&ContainerQueryContext>,
    container_scope: Option<&HashSet<usize>>,
    reuse_map: Option<&HashMap<usize, *const StyledNode>>,
    reuse_counter: Option<&mut usize>,
) -> StyledNode {
    record_node_visit(node);

    let mut ua_styles = get_default_styles_for_element(node);
    inherit_styles(&mut ua_styles, parent_ua_styles);

    let mut ancestors: Vec<&DomNode> = Vec::new();
    let node_id = *node_counter;
    *node_counter += 1;
    let mut matching_rules = find_matching_rules(
        node,
        rules,
        selector_caches,
        &ancestors,
        ancestor_ids,
        node_id,
        container_ctx,
    );
    matching_rules.extend(ua_default_rules(node, parent_styles.direction));
    let ua_matches: Vec<_> = matching_rules
        .iter()
        .filter(|r| r.origin == StyleOrigin::UserAgent)
        .cloned()
        .collect();
    let prof = cascade_profile_enabled();
    let decl_start = prof.then(|| Instant::now());
    apply_cascaded_declarations(
        &mut ua_styles,
        ua_matches,
        None,
        parent_ua_styles,
        parent_ua_styles.font_size,
        ua_root_font_size,
        viewport,
        &ComputedStyle::default(),
        |_| true,
    );
    if let (true, Some(start)) = (prof, decl_start) {
        record_declaration_time(start.elapsed());
    }

    if let Some(lang) = node
        .get_attribute("lang")
        .or_else(|| node.get_attribute("xml:lang"))
        .filter(|l| !l.is_empty())
    {
        ua_styles.language = normalize_language_tag(&lang);
    }

    finalize_grid_placement(&mut ua_styles);
    resolve_match_parent_text_align(&mut ua_styles, parent_ua_styles, ancestors.is_empty());
    resolve_match_parent_text_align_last(&mut ua_styles, parent_ua_styles, ancestors.is_empty());
    resolve_relative_font_weight(&mut ua_styles, parent_ua_styles);
    propagate_text_decorations(&mut ua_styles, parent_ua_styles);

    let is_root = ancestors.is_empty();
    let current_ua_root_font_size = if is_root {
        ua_styles.font_size
    } else {
        ua_root_font_size
    };
    ua_styles.root_font_size = current_ua_root_font_size;
    resolve_line_height_length(&mut ua_styles, viewport);
    resolve_absolute_lengths(&mut ua_styles, current_ua_root_font_size, viewport);

    let mut styles = get_default_styles_for_element(node);

    // Inherit styles from parent
    inherit_styles(&mut styles, parent_styles);

    // Apply matching CSS rules and inline styles with full cascade ordering
    append_presentational_hints(node, ancestors.as_slice(), parent_styles.direction, &mut matching_rules);
    let inline_decls = node.get_attribute("style").as_deref().map(parse_declarations);
    let decl_start = prof.then(|| Instant::now());
    apply_cascaded_declarations(
        &mut styles,
        matching_rules,
        inline_decls,
        parent_styles,
        parent_styles.font_size,
        root_font_size,
        viewport,
        &ua_styles,
        |_| true,
    );
    if let (true, Some(start)) = (prof, decl_start) {
        record_declaration_time(start.elapsed());
    }

    // Apply language from attributes (inherits by default)
    if let Some(lang) = node
        .get_attribute("lang")
        .or_else(|| node.get_attribute("xml:lang"))
        .filter(|l| !l.is_empty())
    {
        styles.language = normalize_language_tag(&lang);
    }

    // Finalize grid placement - resolve named grid lines
    finalize_grid_placement(&mut styles);
    resolve_match_parent_text_align(&mut styles, parent_styles, ancestors.is_empty());
    resolve_match_parent_text_align_last(&mut styles, parent_styles, ancestors.is_empty());
    resolve_relative_font_weight(&mut styles, parent_styles);
    propagate_text_decorations(&mut styles, parent_styles);

    // Root font size for rem resolution: the document root sets the value for all descendants.
    let is_root = ancestors.is_empty();
    let current_root_font_size = if is_root { styles.font_size } else { root_font_size };
    styles.root_font_size = current_root_font_size;
    resolve_line_height_length(&mut styles, viewport);

    // Compute pseudo-element styles
    let pseudo_start = prof.then(|| Instant::now());
    let before_styles = if rules.has_pseudo_rules(&PseudoElement::Before) {
        compute_pseudo_element_styles(
            node,
            rules,
            selector_caches,
            ancestors.as_slice(),
            &styles,
            &ua_styles,
            current_root_font_size,
            current_ua_root_font_size,
            viewport,
            &PseudoElement::Before,
        )
    } else {
        None
    };
    let after_styles = if rules.has_pseudo_rules(&PseudoElement::After) {
        compute_pseudo_element_styles(
            node,
            rules,
            selector_caches,
            ancestors.as_slice(),
            &styles,
            &ua_styles,
            current_root_font_size,
            current_ua_root_font_size,
            viewport,
            &PseudoElement::After,
        )
    } else {
        None
    };
    let marker_styles = compute_marker_styles(
        node,
        rules,
        selector_caches,
        ancestors.as_slice(),
        &styles,
        &ua_styles,
        current_root_font_size,
        current_ua_root_font_size,
        viewport,
    );
    if let (true, Some(start)) = (prof, pseudo_start) {
        record_pseudo_time(start.elapsed());
    }

    // Recursively style children (passing current node in ancestors)
    let mut children = Vec::with_capacity(node.children.len());
    let mut reuse_counter = reuse_counter;
    ancestors.push(node);
    ancestor_ids.push(node_id);
    for child in &node.children {
        let child_reuse = reuse_counter.as_deref_mut();
        let child_styled = apply_styles_internal_with_ancestors(
            child,
            rules,
            selector_caches,
            &styles,
            &ua_styles,
            current_root_font_size,
            current_ua_root_font_size,
            viewport,
            &mut ancestors,
            node_counter,
            ancestor_ids,
            container_ctx,
            container_scope,
            reuse_map,
            child_reuse,
        );
        children.push(child_styled);
    }
    ancestors.pop();
    ancestor_ids.pop();

    StyledNode {
        node_id,
        node: node.clone(),
        styles,
        before_styles,
        after_styles,
        marker_styles,
        children,
    }
}

fn apply_styles_internal_with_ancestors<'a>(
    node: &'a DomNode,
    rules: &RuleIndex<'_>,
    selector_caches: &mut SelectorCaches,
    parent_styles: &ComputedStyle,
    parent_ua_styles: &ComputedStyle,
    root_font_size: f32,
    ua_root_font_size: f32,
    viewport: Size,
    ancestors: &mut Vec<&'a DomNode>,
    node_counter: &mut usize,
    ancestor_ids: &mut Vec<usize>,
    container_ctx: Option<&ContainerQueryContext>,
    container_scope: Option<&HashSet<usize>>,
    reuse_map: Option<&HashMap<usize, *const StyledNode>>,
    reuse_counter: Option<&mut usize>,
) -> StyledNode {
    record_node_visit(node);

    let mut ua_styles = get_default_styles_for_element(node);
    inherit_styles(&mut ua_styles, parent_ua_styles);

    let node_id = *node_counter;
    *node_counter += 1;

    // If this node lies outside any container scope during a container-query recascade,
    // reuse the prior styled subtree to avoid recomputing unaffected branches.
    if let (Some(scope), Some(map)) = (container_scope, reuse_map) {
        if !scope.contains(&node_id) {
            if let Some(ptr) = map.get(&node_id) {
                // Safety: pointers are produced from the previous styled tree, which is
                // alive for the duration of this call. We clone to produce an owned subtree.
                // This bypasses rule matching and recursion for unaffected nodes.
                let reused_ref = unsafe { &**ptr };
                let reused_size = count_styled_nodes(reused_ref);
                if reused_size > 1 {
                    // Advance the traversal counter so sibling nodes keep the same ids
                    // they had in the first cascade, maintaining alignment with the
                    // reuse map and container scope built from that pass.
                    let advance = reused_size.saturating_sub(1);
                    *node_counter = (*node_counter).saturating_add(advance);
                }
                let reused = reused_ref.clone();
                if let Some(counter) = reuse_counter {
                    *counter += reused_size;
                }
                return reused;
            }
        }
    }

    let mut matching_rules = find_matching_rules(
        node,
        rules,
        selector_caches,
        ancestors.as_slice(),
        ancestor_ids,
        node_id,
        container_ctx,
    );
    matching_rules.extend(ua_default_rules(node, parent_styles.direction));
    let ua_matches: Vec<_> = matching_rules
        .iter()
        .filter(|r| r.origin == StyleOrigin::UserAgent)
        .cloned()
        .collect();
    let prof = cascade_profile_enabled();
    let decl_start = prof.then(|| Instant::now());
    apply_cascaded_declarations(
        &mut ua_styles,
        ua_matches,
        None,
        parent_ua_styles,
        parent_ua_styles.font_size,
        ua_root_font_size,
        viewport,
        &ComputedStyle::default(),
        |_| true,
    );
    if let (true, Some(start)) = (prof, decl_start) {
        record_declaration_time(start.elapsed());
    }

    if let Some(lang) = node
        .get_attribute("lang")
        .or_else(|| node.get_attribute("xml:lang"))
        .filter(|l| !l.is_empty())
    {
        ua_styles.language = normalize_language_tag(&lang);
    }

    finalize_grid_placement(&mut ua_styles);
    resolve_match_parent_text_align(&mut ua_styles, parent_ua_styles, ancestors.is_empty());
    resolve_match_parent_text_align_last(&mut ua_styles, parent_ua_styles, ancestors.is_empty());
    resolve_relative_font_weight(&mut ua_styles, parent_ua_styles);
    propagate_text_decorations(&mut ua_styles, parent_ua_styles);

    let is_root = ancestors.is_empty();
    let current_ua_root_font_size = if is_root {
        ua_styles.font_size
    } else {
        ua_root_font_size
    };
    ua_styles.root_font_size = current_ua_root_font_size;
    resolve_line_height_length(&mut ua_styles, viewport);

    let mut styles = get_default_styles_for_element(node);

    // Inherit styles from parent
    inherit_styles(&mut styles, parent_styles);

    // Apply matching CSS rules and inline styles with full cascade ordering
    append_presentational_hints(node, ancestors.as_slice(), parent_styles.direction, &mut matching_rules);
    let inline_decls = node.get_attribute("style").as_deref().map(parse_declarations);
    let decl_start = prof.then(|| Instant::now());
    apply_cascaded_declarations(
        &mut styles,
        matching_rules,
        inline_decls,
        parent_styles,
        parent_styles.font_size,
        root_font_size,
        viewport,
        &ua_styles,
        |_| true,
    );
    if let (true, Some(start)) = (prof, decl_start) {
        record_declaration_time(start.elapsed());
    }

    // Finalize grid placement - resolve named grid lines
    finalize_grid_placement(&mut styles);
    resolve_match_parent_text_align(&mut styles, parent_styles, ancestors.is_empty());
    resolve_match_parent_text_align_last(&mut styles, parent_styles, ancestors.is_empty());
    resolve_relative_font_weight(&mut styles, parent_styles);
    propagate_text_decorations(&mut styles, parent_styles);

    let current_root_font_size = if is_root { styles.font_size } else { root_font_size };
    styles.root_font_size = current_root_font_size;
    resolve_line_height_length(&mut styles, viewport);
    resolve_absolute_lengths(&mut styles, current_root_font_size, viewport);

    let pseudo_start = prof.then(|| Instant::now());
    let before_styles = if rules.has_pseudo_rules(&PseudoElement::Before) {
        compute_pseudo_element_styles(
            node,
            rules,
            selector_caches,
            ancestors.as_slice(),
            &styles,
            &ua_styles,
            current_root_font_size,
            current_ua_root_font_size,
            viewport,
            &PseudoElement::Before,
        )
    } else {
        None
    };
    let after_styles = if rules.has_pseudo_rules(&PseudoElement::After) {
        compute_pseudo_element_styles(
            node,
            rules,
            selector_caches,
            ancestors.as_slice(),
            &styles,
            &ua_styles,
            current_root_font_size,
            current_ua_root_font_size,
            viewport,
            &PseudoElement::After,
        )
    } else {
        None
    };
    let marker_styles = compute_marker_styles(
        node,
        rules,
        selector_caches,
        ancestors.as_slice(),
        &styles,
        &ua_styles,
        current_root_font_size,
        current_ua_root_font_size,
        viewport,
    );
    if let (true, Some(start)) = (prof, pseudo_start) {
        record_pseudo_time(start.elapsed());
    }

    let mut children = Vec::with_capacity(node.children.len());
    let mut reuse_counter = reuse_counter;

    ancestors.push(node);
    ancestor_ids.push(node_id);
    for child in &node.children {
        let child_reuse = reuse_counter.as_deref_mut();
        let child_styled = apply_styles_internal_with_ancestors(
            child,
            rules,
            selector_caches,
            &styles,
            &ua_styles,
            current_root_font_size,
            current_ua_root_font_size,
            viewport,
            ancestors,
            node_counter,
            ancestor_ids,
            container_ctx,
            container_scope,
            reuse_map,
            child_reuse,
        );
        children.push(child_styled);
    }
    ancestors.pop();
    ancestor_ids.pop();

    StyledNode {
        node_id,
        node: node.clone(),
        styles,
        before_styles,
        after_styles,
        marker_styles,
        children,
    }
}

fn inherit_styles(styles: &mut ComputedStyle, parent: &ComputedStyle) {
    // Reset cascade bookkeeping for the new style; logical pending state should not inherit.
    styles.logical.reset();
    // Typography properties inherit
    styles.font_family = parent.font_family.clone();
    styles.font_size = parent.font_size;
    styles.root_font_size = parent.root_font_size;
    styles.font_weight = parent.font_weight;
    styles.font_style = parent.font_style;
    styles.font_variant = parent.font_variant;
    styles.font_variant_caps = parent.font_variant_caps;
    styles.font_variant_alternates = parent.font_variant_alternates.clone();
    styles.font_variant_numeric = parent.font_variant_numeric;
    styles.font_variant_east_asian = parent.font_variant_east_asian;
    styles.font_variant_ligatures = parent.font_variant_ligatures;
    styles.font_variant_position = parent.font_variant_position;
    styles.font_size_adjust = parent.font_size_adjust;
    styles.font_synthesis = parent.font_synthesis;
    styles.font_feature_settings = parent.font_feature_settings.clone();
    styles.font_optical_sizing = parent.font_optical_sizing;
    styles.font_variation_settings = parent.font_variation_settings.clone();
    styles.font_language_override = parent.font_language_override.clone();
    styles.font_variant_emoji = parent.font_variant_emoji;
    styles.font_stretch = parent.font_stretch;
    styles.font_kerning = parent.font_kerning;
    styles.line_height = parent.line_height.clone();
    styles.direction = parent.direction;
    styles.text_align = parent.text_align;
    styles.text_align_last = parent.text_align_last;
    styles.text_justify = parent.text_justify;
    styles.text_indent = parent.text_indent;
    styles.text_decoration_skip_ink = parent.text_decoration_skip_ink;
    styles.text_underline_offset = parent.text_underline_offset;
    styles.text_underline_position = parent.text_underline_position;
    styles.text_emphasis_style = parent.text_emphasis_style.clone();
    styles.text_emphasis_color = parent.text_emphasis_color;
    styles.text_emphasis_position = parent.text_emphasis_position;
    styles.text_transform = parent.text_transform;
    styles.text_combine_upright = parent.text_combine_upright;
    // text-orientation is non-inherited; leave as initial value
    styles.writing_mode = parent.writing_mode;
    styles.letter_spacing = parent.letter_spacing;
    styles.word_spacing = parent.word_spacing;
    styles.justify_items = parent.justify_items;
    styles.visibility = parent.visibility;
    styles.white_space = parent.white_space;
    styles.line_break = parent.line_break;
    styles.tab_size = parent.tab_size;
    styles.caption_side = parent.caption_side;
    styles.empty_cells = parent.empty_cells;
    styles.hyphens = parent.hyphens;
    styles.word_break = parent.word_break;
    styles.overflow_wrap = parent.overflow_wrap;
    styles.language = parent.language.clone();
    styles.list_style_type = parent.list_style_type.clone();
    styles.list_style_position = parent.list_style_position;
    styles.list_style_image = parent.list_style_image.clone();
    styles.quotes = parent.quotes.clone();
    styles.cursor = parent.cursor;
    styles.cursor_images = parent.cursor_images.clone();

    // Color inherits
    styles.color = parent.color;

    // CSS Custom Properties inherit
    styles.custom_properties = parent.custom_properties.clone();
}

/// Resolves line-height lengths to absolute pixels using computed font sizes and viewport metrics.
fn resolve_line_height_length(style: &mut ComputedStyle, viewport: Size) {
    use crate::style::types::LineHeight;

    if let LineHeight::Length(len) = style.line_height {
        let px = match len.unit {
            u if u.is_absolute() => len.to_px(),
            LengthUnit::Em => len.value * style.font_size,
            LengthUnit::Rem => len.value * style.root_font_size,
            LengthUnit::Ex => len.value * style.font_size * 0.5,
            LengthUnit::Ch => len.value * style.font_size * 0.5,
            u if u.is_viewport_relative() => len
                .resolve_with_viewport(viewport.width, viewport.height)
                .unwrap_or(len.value),
            _ => len.value,
        };

        style.line_height = LineHeight::Length(Length::px(px));
    }
}

fn resolve_match_parent_text_align(styles: &mut ComputedStyle, parent: &ComputedStyle, is_root: bool) {
    use crate::style::types::TextAlign;
    if !matches!(styles.text_align, TextAlign::MatchParent) {
        return;
    }
    if is_root {
        styles.text_align = TextAlign::Start;
        return;
    }
    // Behaves like inherit, but start/end become physical based on the parent's direction.
    let inherited = match parent.text_align {
        TextAlign::MatchParent => {
            if matches!(parent.direction, crate::style::types::Direction::Rtl) {
                TextAlign::End
            } else {
                TextAlign::Start
            }
        }
        other => other,
    };
    styles.text_align = match inherited {
        TextAlign::Start => {
            if matches!(parent.direction, crate::style::types::Direction::Rtl) {
                TextAlign::Right
            } else {
                TextAlign::Left
            }
        }
        TextAlign::End => {
            if matches!(parent.direction, crate::style::types::Direction::Rtl) {
                TextAlign::Left
            } else {
                TextAlign::Right
            }
        }
        other => other,
    };
}

fn resolve_match_parent_text_align_last(styles: &mut ComputedStyle, parent: &ComputedStyle, is_root: bool) {
    use crate::style::types::TextAlignLast;
    if !matches!(styles.text_align_last, TextAlignLast::MatchParent) {
        return;
    }
    if is_root {
        styles.text_align_last = TextAlignLast::Start;
        return;
    }
    let inherited = match parent.text_align_last {
        TextAlignLast::MatchParent => {
            if matches!(parent.direction, crate::style::types::Direction::Rtl) {
                TextAlignLast::End
            } else {
                TextAlignLast::Start
            }
        }
        other => other,
    };
    styles.text_align_last = match inherited {
        TextAlignLast::Start => {
            if matches!(parent.direction, crate::style::types::Direction::Rtl) {
                TextAlignLast::Right
            } else {
                TextAlignLast::Left
            }
        }
        TextAlignLast::End => {
            if matches!(parent.direction, crate::style::types::Direction::Rtl) {
                TextAlignLast::Left
            } else {
                TextAlignLast::Right
            }
        }
        other => other,
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::css::parser::parse_declarations;
    use crate::css::parser::parse_stylesheet;
    use crate::css::types::CssImportLoader;
    use crate::css::types::StyleSheet;
    use crate::dom::DomNodeType;
    use crate::style::color::Rgba;
    use crate::style::computed::Visibility;
    use crate::style::display::Display;
    use crate::style::float::Float;
    use crate::style::properties::apply_declaration;
    use crate::style::types::{
        BorderCollapse, BorderStyle, Direction, LineBreak, ListStylePosition, ListStyleType, TextCombineUpright,
        TextDecorationLine, TextUnderlineOffset, TextUnderlinePosition, UnicodeBidi, WhiteSpace, WillChange,
        WillChangeHint,
    };
    use crate::style::values::Length;
    use crate::style::ComputedStyle;
    use crate::style::CursorKeyword;

    fn element_with_style(style: &str) -> DomNode {
        DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("style".to_string(), style.to_string())],
            },
            children: vec![],
        }
    }

    fn child_font_weight(parent_style: &str, child_style: &str) -> u16 {
        let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("style".to_string(), parent_style.to_string())],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "span".to_string(),
                    attributes: vec![("style".to_string(), child_style.to_string())],
                },
                children: vec![],
            }],
        };
        let styled = apply_styles(&parent, &StyleSheet::new());
        styled.children.first().expect("child").styles.font_weight.to_u16()
    }

    fn element_with_id_and_class(id: &str, class: &str, style: Option<&str>) -> DomNode {
        let mut attributes = vec![
            ("id".to_string(), id.to_string()),
            ("class".to_string(), class.to_string()),
        ];
        if let Some(style) = style {
            attributes.push(("style".to_string(), style.to_string()));
        }
        DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes,
            },
            children: vec![],
        }
    }

    #[test]
    fn important_overrides_more_specific_normal_declarations() {
        let dom = element_with_id_and_class("target", "item", None);
        let stylesheet = parse_stylesheet(
            r#"
            #target { color: red; }
            .item { color: blue !important; }
        "#,
        )
        .unwrap();

        let styled = apply_styles(&dom, &stylesheet);
        assert_eq!(styled.styles.color, Rgba::rgb(0, 0, 255));
    }

    #[test]
    fn author_important_beats_inline_normal() {
        let dom = element_with_id_and_class("target", "item", Some("color: green;"));
        let stylesheet = parse_stylesheet(".item { color: red !important; }").unwrap();
        let styled = apply_styles(&dom, &stylesheet);
        assert_eq!(styled.styles.color, Rgba::rgb(255, 0, 0));
    }

    #[test]
    fn inline_important_outranks_author_important_through_specificity() {
        let dom = element_with_id_and_class("target", "item", Some("color: green !important;"));
        let stylesheet = parse_stylesheet(".item { color: blue !important; }").unwrap();
        let styled = apply_styles(&dom, &stylesheet);
        assert_eq!(styled.styles.color, Rgba::rgb(0, 128, 0));
    }

    #[test]
    fn type_and_class_selector_applies_properties() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "span".to_string(),
                attributes: vec![("class".to_string(), "pagetop".to_string())],
            },
            children: vec![],
        };
        let stylesheet = parse_stylesheet(
            r#"
            .pagetop { line-height: 12px; }
            span.pagetop { display: block; margin: 3px 5px; }
        "#,
        )
        .unwrap();

        let styled = apply_styles(&dom, &stylesheet);
        assert!(matches!(styled.styles.display, Display::Block));
        assert_eq!(styled.styles.margin_top, Some(Length::px(3.0)));
        assert_eq!(styled.styles.margin_right, Some(Length::px(5.0)));
        assert_eq!(styled.styles.margin_bottom, Some(Length::px(3.0)));
        assert_eq!(styled.styles.margin_left, Some(Length::px(5.0)));
    }

    #[test]
    fn revert_falls_back_to_ua_for_non_inherited_property() {
        let dom = element_with_id_and_class("target", "box", None);
        let stylesheet = parse_stylesheet(
            r#"
            .box { display: inline; }
            #target { display: revert; }
        "#,
        )
        .unwrap();

        let styled = apply_styles(&dom, &stylesheet);
        assert!(matches!(styled.styles.display, Display::Block));
    }

    #[test]
    fn revert_ignores_author_inherited_value() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("style".to_string(), "color: rgb(1, 2, 3);".to_string())],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "span".to_string(),
                    attributes: vec![("id".to_string(), "target".to_string())],
                },
                children: vec![],
            }],
        };
        let stylesheet = parse_stylesheet("#target { color: revert; }").unwrap();

        let styled = apply_styles(&dom, &stylesheet);
        let child = styled.children.first().expect("child");
        assert_eq!(child.styles.color, ComputedStyle::default().color);
        assert_ne!(child.styles.color, styled.styles.color);
    }

    #[test]
    fn text_combine_upright_inherits() {
        let mut parent = ComputedStyle::default();
        parent.text_combine_upright = TextCombineUpright::Digits(3);
        let mut child = ComputedStyle::default();
        inherit_styles(&mut child, &parent);
        assert_eq!(parent.text_combine_upright, TextCombineUpright::Digits(3));
        assert_eq!(child.text_combine_upright, TextCombineUpright::Digits(3));
    }

    #[test]
    fn layer_order_controls_cascade_priority() {
        let dom = element_with_id_and_class("target", "", None);
        let stylesheet = parse_stylesheet(
            r#"
            @layer base { #target { color: rgb(1, 2, 3); } }
            @layer theme { #target { color: rgb(4, 5, 6); } }
        "#,
        )
        .unwrap();

        let styled = apply_styles(&dom, &stylesheet);
        assert_eq!(styled.styles.color, Rgba::rgb(4, 5, 6));
    }

    #[test]
    fn blockless_layer_prelude_defines_order() {
        let dom = element_with_id_and_class("target", "", None);
        let stylesheet = parse_stylesheet(
            r#"
            @layer base, theme;
            @layer base { #target { color: rgb(1, 2, 3); } }
            @layer theme { #target { color: rgb(9, 8, 7); } }
        "#,
        )
        .unwrap();

        let styled = apply_styles(&dom, &stylesheet);
        assert_eq!(styled.styles.color, Rgba::rgb(9, 8, 7));
    }

    #[test]
    fn unlayered_rules_override_layered() {
        let dom = element_with_id_and_class("target", "", None);
        let stylesheet = parse_stylesheet(
            r#"
            @layer base { #target { color: rgb(1, 1, 1); } }
            #target { color: rgb(2, 3, 4); }
        "#,
        )
        .unwrap();

        let styled = apply_styles(&dom, &stylesheet);
        assert_eq!(styled.styles.color, Rgba::rgb(2, 3, 4));
    }

    #[test]
    fn revert_layer_restores_previous_layer_value() {
        let dom = element_with_id_and_class("target", "", None);
        let stylesheet = parse_stylesheet(
            r#"
            @layer base { #target { color: rgb(10, 20, 30); } }
            @layer theme { #target { color: rgb(200, 100, 50); color: revert-layer; } }
        "#,
        )
        .unwrap();

        let styled = apply_styles(&dom, &stylesheet);
        assert_eq!(styled.styles.color, Rgba::rgb(10, 20, 30));
    }

    #[test]
    fn dotted_layer_names_create_nested_paths() {
        let dom = element_with_id_and_class("target", "", None);
        let stylesheet = parse_stylesheet(
            r#"
            @layer ui.controls { #target { color: rgb(1, 2, 3); } }
            @layer ui { #target { color: rgb(9, 9, 9); } }
        "#,
        )
        .unwrap();

        let styled = apply_styles(&dom, &stylesheet);
        // The child layer ui.controls should override its parent ui declarations.
        assert_eq!(styled.styles.color, Rgba::rgb(1, 2, 3));
    }

    #[test]
    fn revert_layer_uses_nearest_layer_base() {
        let dom = element_with_id_and_class("target", "", None);
        let stylesheet = parse_stylesheet(
            r#"
            @layer theme {
              #target { color: rgb(1, 2, 3); }
              @layer accents {
                #target { color: rgb(10, 20, 30); color: revert-layer; }
              }
            }
        "#,
        )
        .unwrap();

        let styled = apply_styles(&dom, &stylesheet);
        assert_eq!(styled.styles.color, Rgba::rgb(1, 2, 3));
    }

    #[test]
    fn all_initial_resets_properties_but_preserves_direction() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![(
                    "style".to_string(),
                    "direction: rtl; unicode-bidi: bidi-override; color: red; background-color: blue; all: initial;"
                        .to_string(),
                )],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        let defaults = ComputedStyle::default();
        assert_eq!(styled.styles.color, defaults.color);
        assert_eq!(styled.styles.background_color, defaults.background_color);
        assert!(matches!(styled.styles.direction, crate::style::types::Direction::Rtl));
        assert!(matches!(
            styled.styles.unicode_bidi,
            crate::style::types::UnicodeBidi::BidiOverride
        ));
    }

    #[test]
    fn all_inherit_copies_non_inherited_properties() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("style".to_string(), "display: inline;".to_string())],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "span".to_string(),
                    attributes: vec![
                        ("id".to_string(), "target".to_string()),
                        ("style".to_string(), "all: inherit;".to_string()),
                    ],
                },
                children: vec![],
            }],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        let child = styled.children.first().expect("child");
        assert!(matches!(child.styles.display, Display::Inline));
    }

    #[test]
    fn all_revert_ignores_author_inheritance() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("style".to_string(), "color: rgb(10, 20, 30);".to_string())],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "span".to_string(),
                    attributes: vec![("style".to_string(), "color: green; all: revert;".to_string())],
                },
                children: vec![],
            }],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        let child = styled.children.first().expect("child");
        assert_eq!(child.styles.color, ComputedStyle::default().color);
    }

    #[test]
    fn navigation_visibility_respects_author_stylesheet() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "nav".to_string(),
                    attributes: vec![],
                },
                children: vec![],
            }],
        };
        let stylesheet = parse_stylesheet("nav { visibility: hidden; }").unwrap();

        let styled = apply_styles(&dom, &stylesheet);
        let nav = styled.children.first().expect("nav");
        assert!(matches!(nav.styles.visibility, Visibility::Hidden));
    }

    #[test]
    fn navigation_visibility_respects_authored_hidden_value() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "nav".to_string(),
                    attributes: vec![
                        ("style".to_string(), "visibility: hidden;".to_string()),
                        ("role".to_string(), "navigation".to_string()),
                    ],
                },
                children: vec![],
            }],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        let nav = styled.children.first().expect("nav");
        assert!(matches!(nav.styles.visibility, Visibility::Hidden));
    }

    #[test]
    fn ad_slot_classes_do_not_collapse_without_css_rules() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "div".to_string(),
                    attributes: vec![(
                        "class".to_string(),
                        "ad-slot ad-slot-header ad-slot-header__wrapper".to_string(),
                    )],
                },
                children: vec![],
            }],
        };
        let stylesheet = parse_stylesheet(
            "div.ad-slot { min-height: 24px; padding-top: 8px; margin-top: 4px; background: rgb(1, 2, 3); }",
        )
        .unwrap();

        let styled = apply_styles(&dom, &stylesheet);
        let ad = styled.children.first().expect("ad slot");
        assert!(matches!(ad.styles.display, Display::Block));
        assert!(matches!(ad.styles.visibility, Visibility::Visible));
        assert_eq!(ad.styles.min_height, Some(Length::px(24.0)));
        assert_eq!(ad.styles.padding_top, Length::px(8.0));
        assert_eq!(ad.styles.margin_top, Some(Length::px(4.0)));
        assert_eq!(ad.styles.background_color, Rgba::rgb(1, 2, 3));
    }

    #[test]
    fn font_variation_settings_inherit() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![(
                    "style".to_string(),
                    r#"font-variation-settings: "wght" 600;"#.to_string(),
                )],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "span".to_string(),
                    attributes: vec![],
                },
                children: vec![],
            }],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        let child = styled.children.first().expect("child");
        assert_eq!(child.styles.font_variation_settings.len(), 1);
        assert_eq!(child.styles.font_variation_settings[0].tag, *b"wght");
        assert!((child.styles.font_variation_settings[0].value - 600.0).abs() < 0.001);
    }

    #[test]
    fn font_language_override_inherits() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("style".to_string(), r#"font-language-override: "SRB";"#.to_string())],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "span".to_string(),
                    attributes: vec![],
                },
                children: vec![],
            }],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        let child = styled.children.first().expect("child");
        assert!(matches!(
            child.styles.font_language_override,
            crate::style::types::FontLanguageOverride::Override(ref tag) if tag == "SRB"
        ));
    }

    #[test]
    fn imports_are_resolved_when_loader_present() {
        struct Loader;
        impl CssImportLoader for Loader {
            fn load(&self, _url: &str) -> crate::error::Result<String> {
                Ok("#target { color: rgb(1, 2, 3); }".to_string())
            }
        }

        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("id".to_string(), "target".to_string())],
            },
            children: vec![],
        };
        let stylesheet = parse_stylesheet(r#"@import url("import.css");"#).unwrap();
        let media_ctx = MediaContext::screen(800.0, 600.0);
        let styled = apply_styles_with_media_target_and_imports(
            &dom,
            &stylesheet,
            &media_ctx,
            None,
            Some(&Loader),
            Some("https://example.com/page.css"),
            None,
            None,
            None,
        );
        assert_eq!(styled.styles.color, Rgba::rgb(1, 2, 3));
    }

    #[test]
    fn dir_attribute_sets_direction_and_isolate() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("dir".to_string(), "rtl".to_string())],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert!(matches!(styled.styles.direction, crate::style::types::Direction::Rtl));
        assert!(matches!(
            styled.styles.unicode_bidi,
            crate::style::types::UnicodeBidi::Isolate
        ));
    }

    #[test]
    fn author_css_overrides_presentational_dir() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("dir".to_string(), "rtl".to_string())],
            },
            children: vec![],
        };

        let stylesheet = parse_stylesheet("div { direction: ltr; unicode-bidi: normal; }").unwrap();
        let styled = apply_styles(&dom, &stylesheet);
        assert!(matches!(styled.styles.direction, crate::style::types::Direction::Ltr));
        assert!(matches!(
            styled.styles.unicode_bidi,
            crate::style::types::UnicodeBidi::Normal
        ));
    }

    #[test]
    fn image_set_selection_uses_media_context_dpr() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };

        let stylesheet =
            parse_stylesheet("div { background-image: image-set(url(\"lo.png\") 1x, url(\"hi.png\") 2x); }").unwrap();
        let media = MediaContext::screen(800.0, 600.0).with_device_pixel_ratio(2.0);
        let styled = apply_styles_with_media(&dom, &stylesheet, &media);

        assert!(matches!(
            styled.styles.background_layers.get(0).and_then(|l| l.image.as_ref()),
            Some(crate::style::types::BackgroundImage::Url(url)) if url == "hi.png"
        ));
    }

    #[test]
    fn inline_style_overrides_presentational_dir() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![
                    ("dir".to_string(), "rtl".to_string()),
                    ("style".to_string(), "direction: ltr;".to_string()),
                ],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert!(matches!(styled.styles.direction, crate::style::types::Direction::Ltr));
    }

    #[test]
    fn replaced_dimensions_use_presentational_hints() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "img".to_string(),
                attributes: vec![
                    ("width".to_string(), "320".to_string()),
                    ("height".to_string(), "240".to_string()),
                ],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert_eq!(styled.styles.width, Some(Length::px(320.0)));
        assert_eq!(styled.styles.height, Some(Length::px(240.0)));
    }

    #[test]
    fn author_css_overrides_replaced_presentational_dimensions() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "img".to_string(),
                attributes: vec![
                    ("width".to_string(), "640".to_string()),
                    ("height".to_string(), "480".to_string()),
                ],
            },
            children: vec![],
        };

        let stylesheet = parse_stylesheet("img { width: 120px; height: auto; }").unwrap();
        let styled = apply_styles(&dom, &stylesheet);
        assert_eq!(styled.styles.width, Some(Length::px(120.0)));
        assert!(styled.styles.height.is_none());
    }

    #[test]
    fn html_defaults_to_white_background() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "html".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert_eq!(styled.styles.background_color, Rgba::WHITE);
    }

    #[test]
    fn body_defaults_to_ua_margin() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "body".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        let expected = Some(Length::px(8.0));
        assert_eq!(styled.styles.margin_top, expected);
        assert_eq!(styled.styles.margin_right, expected);
        assert_eq!(styled.styles.margin_bottom, expected);
        assert_eq!(styled.styles.margin_left, expected);
    }

    #[test]
    fn anchor_defaults_to_ua_link_style() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "a".to_string(),
                attributes: vec![("href".to_string(), "https://example.com".to_string())],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert_eq!(styled.styles.color, Rgba::from_rgba8(0, 0, 238, 255));
        assert!(styled
            .styles
            .text_decoration
            .lines
            .contains(TextDecorationLine::UNDERLINE));
        assert_eq!(styled.styles.cursor, crate::style::types::CursorKeyword::Pointer);
    }

    #[test]
    fn author_css_overrides_anchor_ua_style() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "a".to_string(),
                attributes: vec![("href".to_string(), "https://example.com".to_string())],
            },
            children: vec![],
        };

        let stylesheet =
            parse_stylesheet("a { color: rgb(255, 0, 0); text-decoration: none; cursor: default; }").unwrap();
        let styled = apply_styles(&dom, &stylesheet);
        assert_eq!(styled.styles.color, Rgba::rgb(255, 0, 0));
        assert!(styled.styles.text_decoration.lines.is_empty());
        assert!(styled.styles.applied_text_decorations.is_empty());
        assert_eq!(styled.styles.cursor, crate::style::types::CursorKeyword::Default);
    }

    #[test]
    fn heading_ua_defaults_apply() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "h1".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert!((styled.styles.font_size - 32.0).abs() < 0.01);
        assert_eq!(styled.styles.font_weight.to_u16(), 700);
        assert_eq!(styled.styles.margin_top, Some(Length::em(0.67)));
        assert_eq!(styled.styles.margin_bottom, Some(Length::em(0.67)));
    }

    #[test]
    fn paragraph_ua_defaults_apply() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "p".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert_eq!(styled.styles.margin_top, Some(Length::em(1.0)));
        assert_eq!(styled.styles.margin_bottom, Some(Length::em(1.0)));
    }

    #[test]
    fn preformatted_ua_defaults_apply() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "pre".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert_eq!(styled.styles.white_space, crate::style::types::WhiteSpace::Pre);
        assert_eq!(styled.styles.font_family[0], "monospace");
        assert_eq!(styled.styles.margin_top, Some(Length::em(1.0)));
        assert_eq!(styled.styles.margin_bottom, Some(Length::em(1.0)));
    }

    #[test]
    fn blockquote_ua_defaults_apply() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "blockquote".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert_eq!(styled.styles.margin_top, Some(Length::em(1.0)));
        assert_eq!(styled.styles.margin_bottom, Some(Length::em(1.0)));
        assert_eq!(styled.styles.margin_left, Some(Length::px(40.0)));
        assert_eq!(styled.styles.margin_right, Some(Length::px(40.0)));
    }

    #[test]
    fn list_defaults_apply() {
        let ul = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "ul".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };
        let ul_styled = apply_styles(&ul, &StyleSheet::new());
        assert_eq!(
            ul_styled.styles.list_style_type,
            crate::style::types::ListStyleType::Disc
        );
        assert_eq!(ul_styled.styles.padding_left, Length::px(40.0));
        assert_eq!(ul_styled.styles.margin_top, Some(Length::em(1.0)));
        assert_eq!(ul_styled.styles.margin_bottom, Some(Length::em(1.0)));

        let li = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "li".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };
        let li_styled = apply_styles(&li, &StyleSheet::new());
        assert_eq!(li_styled.styles.display, Display::ListItem);

        let ol = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "ol".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };
        let ol_styled = apply_styles(&ol, &StyleSheet::new());
        assert_eq!(
            ol_styled.styles.list_style_type,
            crate::style::types::ListStyleType::Decimal
        );
        assert_eq!(ol_styled.styles.padding_left, Length::px(40.0));

        for tag in ["menu", "dir"] {
            let dom = DomNode {
                node_type: DomNodeType::Element {
                    tag_name: tag.to_string(),
                    attributes: vec![],
                },
                children: vec![],
            };
            let styled = apply_styles(&dom, &StyleSheet::new());
            assert_eq!(styled.styles.display, Display::Block);
            assert_eq!(styled.styles.list_style_type, crate::style::types::ListStyleType::Disc);
            assert_eq!(styled.styles.padding_left, Length::px(40.0));
        }
    }

    #[test]
    fn definition_list_defaults_apply() {
        let dd = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "dd".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };

        let styled = apply_styles(&dd, &StyleSheet::new());
        assert_eq!(styled.styles.margin_left, Some(Length::px(40.0)));
    }

    #[test]
    fn horizontal_rule_defaults_apply() {
        let hr = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "hr".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };

        let styled = apply_styles(&hr, &StyleSheet::new());
        assert_eq!(styled.styles.height, Some(Length::px(1.0)));
        assert_eq!(styled.styles.box_sizing, crate::style::types::BoxSizing::ContentBox);
        assert_eq!(styled.styles.border_top_width, Length::px(1.0));
        assert_eq!(styled.styles.border_top_style, crate::style::types::BorderStyle::Inset);
        let gray = Rgba::from_rgba8(128, 128, 128, 255);
        assert_eq!(styled.styles.color, gray);
        assert_eq!(styled.styles.border_top_color, gray);
    }

    #[test]
    fn bdi_defaults_to_isolate_with_auto_direction() {
        let bdi = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "bdi".to_string(),
                attributes: vec![],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Text {
                    content: "مرحبا".to_string(),
                },
                children: vec![],
            }],
        };

        let styled = apply_styles(&bdi, &StyleSheet::new());
        assert_eq!(styled.styles.unicode_bidi, UnicodeBidi::Isolate);
        assert_eq!(styled.styles.direction, Direction::Rtl);
    }

    #[test]
    fn bdo_dir_attribute_uses_bidi_override() {
        let bdo = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "bdo".to_string(),
                attributes: vec![("dir".to_string(), "rtl".to_string())],
            },
            children: vec![],
        };

        let styled = apply_styles(&bdo, &StyleSheet::new());
        assert_eq!(styled.styles.direction, Direction::Rtl);
        assert_eq!(styled.styles.unicode_bidi, UnicodeBidi::BidiOverride);
    }

    #[test]
    fn bdo_without_dir_defaults_to_override_ltr() {
        let bdo = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "bdo".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };

        let styled = apply_styles(&bdo, &StyleSheet::new());
        assert_eq!(styled.styles.direction, Direction::Ltr);
        assert_eq!(styled.styles.unicode_bidi, UnicodeBidi::BidiOverride);
    }

    #[test]
    fn dir_auto_inherits_parent_direction_when_no_strong_characters() {
        let child = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("dir".to_string(), "auto".to_string())],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Text {
                    content: "1234".to_string(), // neutrals only
                },
                children: vec![],
            }],
        };
        let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("dir".to_string(), "rtl".to_string())],
            },
            children: vec![child],
        };

        let styled = apply_styles(&parent, &StyleSheet::new());
        let child_styles = &styled.children[0].styles;
        assert_eq!(child_styles.direction, Direction::Rtl);
        assert_eq!(child_styles.unicode_bidi, UnicodeBidi::Isolate);
    }

    #[test]
    fn bdi_auto_direction_uses_parent_when_no_strong_characters() {
        let bdi = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "bdi".to_string(),
                attributes: vec![],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Text {
                    content: "--".to_string(), // no strong directional characters
                },
                children: vec![],
            }],
        };
        let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("dir".to_string(), "rtl".to_string())],
            },
            children: vec![bdi],
        };

        let styled = apply_styles(&parent, &StyleSheet::new());
        let bdi_styles = &styled.children[0].styles;
        assert_eq!(bdi_styles.direction, Direction::Rtl);
        assert_eq!(bdi_styles.unicode_bidi, UnicodeBidi::Isolate);
    }

    #[test]
    fn small_and_big_defaults_apply() {
        let small = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "small".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };
        let big = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "big".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };

        let small_styled = apply_styles(&small, &StyleSheet::new());
        let big_styled = apply_styles(&big, &StyleSheet::new());
        let parent = 16.0;
        assert!(
            small_styled.styles.font_size < parent,
            "small font size {} should be less than parent {}",
            small_styled.styles.font_size,
            parent
        );
        assert!(
            (small_styled.styles.font_size * 1.2 - parent).abs() < 0.1,
            "small font size {} not near 1/1.2 of parent {}",
            small_styled.styles.font_size,
            parent
        );
        assert!(
            big_styled.styles.font_size > parent,
            "big font size {} should be greater than parent {}",
            big_styled.styles.font_size,
            parent
        );
        assert!(
            (big_styled.styles.font_size / 1.2 - parent).abs() < 0.1,
            "big font size {} not near 1.2 of parent {}",
            big_styled.styles.font_size,
            parent
        );
    }

    #[test]
    fn sub_and_sup_defaults_apply() {
        let sub = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "sub".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };
        let sup = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "sup".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };

        let sub_styled = apply_styles(&sub, &StyleSheet::new());
        let sup_styled = apply_styles(&sup, &StyleSheet::new());
        assert_eq!(
            sub_styled.styles.vertical_align,
            crate::style::types::VerticalAlign::Sub
        );
        assert_eq!(
            sup_styled.styles.vertical_align,
            crate::style::types::VerticalAlign::Super
        );
        let expected = 16.0 / 1.2;
        assert!((sub_styled.styles.font_size - expected).abs() < 0.01);
        assert!((sup_styled.styles.font_size - expected).abs() < 0.01);
    }

    #[test]
    fn mark_defaults_apply() {
        let mark = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "mark".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };

        let styled = apply_styles(&mark, &StyleSheet::new());
        assert_eq!(styled.styles.background_color, Rgba::from_rgba8(255, 255, 0, 255));
        assert_eq!(styled.styles.color, Rgba::BLACK);
    }

    #[test]
    fn semantic_font_style_defaults_apply() {
        for tag in ["address", "cite", "dfn", "var", "em", "i"] {
            let dom = DomNode {
                node_type: DomNodeType::Element {
                    tag_name: tag.to_string(),
                    attributes: vec![],
                },
                children: vec![],
            };
            let styled = apply_styles(&dom, &StyleSheet::new());
            assert_eq!(styled.styles.font_style, crate::style::types::FontStyle::Italic);
        }
    }

    #[test]
    fn ins_and_del_defaults_apply() {
        let ins = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "ins".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };
        let del = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "del".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };

        let ins_styled = apply_styles(&ins, &StyleSheet::new());
        assert!(ins_styled
            .styles
            .text_decoration
            .lines
            .contains(TextDecorationLine::UNDERLINE));

        let del_styled = apply_styles(&del, &StyleSheet::new());
        assert!(del_styled
            .styles
            .text_decoration
            .lines
            .contains(TextDecorationLine::LINE_THROUGH));
    }

    #[test]
    fn abbr_with_title_defaults_apply() {
        let abbr = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "abbr".to_string(),
                attributes: vec![("title".to_string(), "World Health Organization".to_string())],
            },
            children: vec![],
        };

        let styled = apply_styles(&abbr, &StyleSheet::new());
        assert_eq!(styled.styles.border_bottom_width, Length::px(1.0));
        assert_eq!(
            styled.styles.border_bottom_style,
            crate::style::types::BorderStyle::Dotted
        );
        assert_eq!(styled.styles.cursor, crate::style::types::CursorKeyword::Help);
    }

    #[test]
    fn underline_and_strike_defaults_apply() {
        let u = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "u".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };
        let s = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "s".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };
        let strike = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "strike".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };

        let u_styled = apply_styles(&u, &StyleSheet::new());
        assert!(u_styled
            .styles
            .text_decoration
            .lines
            .contains(TextDecorationLine::UNDERLINE));

        for dom in [s, strike] {
            let styled = apply_styles(&dom, &StyleSheet::new());
            assert!(styled
                .styles
                .text_decoration
                .lines
                .contains(TextDecorationLine::LINE_THROUGH));
        }
    }

    #[test]
    fn center_defaults_apply() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "center".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert_eq!(styled.styles.text_align, crate::style::types::TextAlign::Center);
    }

    #[test]
    fn form_control_defaults_apply() {
        let text_input = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "input".to_string(),
                attributes: vec![("type".to_string(), "text".to_string())],
            },
            children: vec![],
        };
        let submit_input = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "input".to_string(),
                attributes: vec![("type".to_string(), "submit".to_string())],
            },
            children: vec![],
        };
        let hidden_input = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "input".to_string(),
                attributes: vec![("type".to_string(), "hidden".to_string())],
            },
            children: vec![],
        };
        let textarea = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "textarea".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };
        let select = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "select".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };
        let button = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "button".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };
        let option = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "option".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };
        let optgroup = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "optgroup".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };
        let fieldset = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "fieldset".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };
        let legend = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "legend".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };
        let details = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "details".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };
        let summary = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "summary".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };
        let details_with_summary = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "details".to_string(),
                attributes: vec![("open".to_string(), "".to_string())],
            },
            children: vec![summary.clone()],
        };
        let details_closed_with_content = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "details".to_string(),
                attributes: vec![],
            },
            children: vec![
                summary.clone(),
                DomNode {
                    node_type: DomNodeType::Element {
                        tag_name: "div".to_string(),
                        attributes: vec![],
                    },
                    children: vec![],
                },
            ],
        };
        let details_open_with_content = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "details".to_string(),
                attributes: vec![("open".to_string(), "".to_string())],
            },
            children: details_closed_with_content.children.clone(),
        };

        for dom in [&text_input, &submit_input, &textarea, &select, &button] {
            let styled = apply_styles(dom, &StyleSheet::new());
            assert!(styled.styles.border_top_width.to_px() >= 1.0);
            assert_ne!(styled.styles.border_top_style, crate::style::types::BorderStyle::None);
            assert_eq!(styled.styles.background_color, Rgba::WHITE);
            assert_eq!(styled.styles.display, Display::InlineBlock);
        }

        let styled_textarea = apply_styles(&textarea, &StyleSheet::new());
        assert_eq!(styled_textarea.styles.display, Display::InlineBlock);
        assert_eq!(styled_textarea.styles.cursor, crate::style::types::CursorKeyword::Text);

        let styled_text_input = apply_styles(&text_input, &StyleSheet::new());
        assert_eq!(
            styled_text_input.styles.cursor,
            crate::style::types::CursorKeyword::Text
        );

        let styled_submit = apply_styles(&submit_input, &StyleSheet::new());
        assert_eq!(styled_submit.styles.cursor, crate::style::types::CursorKeyword::Pointer);

        let styled_hidden = apply_styles(&hidden_input, &StyleSheet::new());
        assert_eq!(styled_hidden.styles.display, Display::None);

        let styled_option = apply_styles(&option, &StyleSheet::new());
        assert_eq!(styled_option.styles.display, Display::Block);
        assert_eq!(styled_option.styles.white_space, crate::style::types::WhiteSpace::Pre);

        let styled_optgroup = apply_styles(&optgroup, &StyleSheet::new());
        assert_eq!(styled_optgroup.styles.display, Display::Block);
        assert_eq!(styled_optgroup.styles.white_space, crate::style::types::WhiteSpace::Pre);
        assert!(styled_optgroup.styles.font_weight.to_u16() >= 700);

        let styled_fieldset = apply_styles(&fieldset, &StyleSheet::new());
        assert_eq!(styled_fieldset.styles.display, Display::Block);
        assert!(styled_fieldset.styles.border_top_width.to_px() >= 1.5);
        assert_ne!(
            styled_fieldset.styles.border_top_style,
            crate::style::types::BorderStyle::None
        );
        assert!(
            styled_fieldset
                .styles
                .padding_left
                .resolve_with_font_size(styled_fieldset.styles.font_size)
                .unwrap_or(0.0)
                > 0.0
        );

        let styled_legend = apply_styles(&legend, &StyleSheet::new());
        assert_eq!(styled_legend.styles.display, Display::Block);
        assert!(
            styled_legend
                .styles
                .padding_left
                .resolve_with_font_size(styled_legend.styles.font_size)
                .unwrap_or(0.0)
                > 0.0
        );
        assert!(
            styled_legend
                .styles
                .padding_right
                .resolve_with_font_size(styled_legend.styles.font_size)
                .unwrap_or(0.0)
                > 0.0
        );

        let styled_details = apply_styles(&details, &StyleSheet::new());
        assert_eq!(styled_details.styles.display, Display::Block);

        let styled_summary = apply_styles(&summary, &StyleSheet::new());
        assert_eq!(styled_summary.styles.display, Display::ListItem);
        assert_eq!(
            styled_summary.styles.cursor,
            crate::style::types::CursorKeyword::Pointer
        );
        assert_eq!(
            styled_summary.styles.list_style_type,
            crate::style::types::ListStyleType::DisclosureClosed
        );

        let styled_open_details = apply_styles(&details_with_summary, &StyleSheet::new());
        let styled_open_summary = &styled_open_details.children[0];
        assert_eq!(
            styled_open_summary.styles.list_style_type,
            crate::style::types::ListStyleType::DisclosureOpen
        );

        let styled_closed_details = apply_styles(&details_closed_with_content, &StyleSheet::new());
        assert_eq!(styled_closed_details.children[0].styles.display, Display::ListItem);
        assert_eq!(styled_closed_details.children[1].styles.display, Display::None);

        let styled_open_details_with_content = apply_styles(&details_open_with_content, &StyleSheet::new());
        assert_eq!(
            styled_open_details_with_content.children[1].styles.display,
            Display::Block
        );
    }

    #[test]
    fn table_header_defaults_apply() {
        let th = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "th".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };

        let styled = apply_styles(&th, &StyleSheet::new());
        assert_eq!(styled.styles.font_weight.to_u16(), 700);
        assert_eq!(styled.styles.text_align, crate::style::types::TextAlign::Center);
    }

    #[test]
    fn q_defaults_apply_quotes() {
        let q = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "q".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };

        let styled = apply_styles(&q, &StyleSheet::new());
        assert_eq!(
            styled.styles.quotes,
            vec![("“".into(), "”".into()), ("‘".into(), "’".into())]
        );
    }

    #[test]
    fn presentational_dimensions_apply_to_descendants() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "img".to_string(),
                    attributes: vec![
                        ("width".to_string(), "120".to_string()),
                        ("height".to_string(), "60".to_string()),
                    ],
                },
                children: vec![],
            }],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        let img = &styled.children[0];
        assert_eq!(img.styles.width, Some(Length::px(120.0)));
        assert_eq!(img.styles.height, Some(Length::px(60.0)));
    }

    #[test]
    fn bgcolor_presentational_hint_applies() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("bgcolor".to_string(), "#ff0000".to_string())],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert_eq!(styled.styles.background_color, Rgba::rgb(255, 0, 0));
    }

    #[test]
    fn author_css_overrides_bgcolor_presentational_hint() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("bgcolor".to_string(), "#ff0000".to_string())],
            },
            children: vec![],
        };

        let stylesheet = parse_stylesheet("div { background-color: rgb(0, 0, 255); }").unwrap();
        let styled = apply_styles(&dom, &stylesheet);
        assert_eq!(styled.styles.background_color, Rgba::rgb(0, 0, 255));
    }

    #[test]
    fn bgcolor_presentational_hint_applies_to_descendants() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "td".to_string(),
                    attributes: vec![("bgcolor".to_string(), "#ff6600".to_string())],
                },
                children: vec![],
            }],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        let cell = &styled.children[0];
        assert_eq!(cell.styles.background_color, Rgba::rgb(255, 102, 0));
    }

    #[test]
    fn cellspacing_presentational_hint_sets_border_spacing() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "table".to_string(),
                attributes: vec![("cellspacing".to_string(), "0".to_string())],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert_eq!(styled.styles.border_spacing_horizontal, Length::px(0.0));
        assert_eq!(styled.styles.border_spacing_vertical, Length::px(0.0));
    }

    #[test]
    fn table_ua_default_border_spacing_is_two_px() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "table".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert_eq!(styled.styles.border_spacing_horizontal, Length::px(2.0));
        assert_eq!(styled.styles.border_spacing_vertical, Length::px(2.0));
    }

    #[test]
    fn author_css_overrides_cellspacing_presentational_hint() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "table".to_string(),
                attributes: vec![("cellspacing".to_string(), "12".to_string())],
            },
            children: vec![],
        };

        let stylesheet = parse_stylesheet("table { border-spacing: 3px 5px; }").unwrap();
        let styled = apply_styles(&dom, &stylesheet);
        assert_eq!(styled.styles.border_spacing_horizontal, Length::px(3.0));
        assert_eq!(styled.styles.border_spacing_vertical, Length::px(5.0));
    }

    #[test]
    fn cellpadding_presentational_hint_applies_to_cells() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "table".to_string(),
                attributes: vec![("cellpadding".to_string(), "10".to_string())],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "tr".to_string(),
                    attributes: vec![],
                },
                children: vec![DomNode {
                    node_type: DomNodeType::Element {
                        tag_name: "td".to_string(),
                        attributes: vec![],
                    },
                    children: vec![],
                }],
            }],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        let cell = &styled.children[0].children[0];
        assert_eq!(cell.styles.padding_left, Length::px(10.0));
        assert_eq!(cell.styles.padding_right, Length::px(10.0));
        assert_eq!(cell.styles.padding_top, Length::px(10.0));
        assert_eq!(cell.styles.padding_bottom, Length::px(10.0));
    }

    #[test]
    fn author_css_overrides_cellpadding_presentational_hint() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "table".to_string(),
                attributes: vec![("cellpadding".to_string(), "8".to_string())],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "tr".to_string(),
                    attributes: vec![],
                },
                children: vec![DomNode {
                    node_type: DomNodeType::Element {
                        tag_name: "td".to_string(),
                        attributes: vec![],
                    },
                    children: vec![],
                }],
            }],
        };

        let stylesheet = parse_stylesheet("td { padding: 2px; }").unwrap();
        let styled = apply_styles(&dom, &stylesheet);
        let cell = &styled.children[0].children[0];
        assert_eq!(cell.styles.padding_left, Length::px(2.0));
        assert_eq!(cell.styles.padding_right, Length::px(2.0));
        assert_eq!(cell.styles.padding_top, Length::px(2.0));
        assert_eq!(cell.styles.padding_bottom, Length::px(2.0));
    }

    #[test]
    fn bordercolor_presentational_hint_sets_border_color() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "table".to_string(),
                attributes: vec![("bordercolor".to_string(), "#00ff00".to_string())],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "tr".to_string(),
                    attributes: vec![],
                },
                children: vec![DomNode {
                    node_type: DomNodeType::Element {
                        tag_name: "td".to_string(),
                        attributes: vec![("bordercolor".to_string(), "rgb(255, 0, 0)".to_string())],
                    },
                    children: vec![],
                }],
            }],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert_eq!(styled.styles.border_top_color, Rgba::rgb(0, 255, 0));
        let cell = &styled.children[0].children[0];
        assert_eq!(cell.styles.border_top_color, Rgba::rgb(255, 0, 0));
    }

    #[test]
    fn author_css_overrides_bordercolor_presentational_hint() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "table".to_string(),
                attributes: vec![("bordercolor".to_string(), "#00ff00".to_string())],
            },
            children: vec![],
        };

        let stylesheet = parse_stylesheet("table { border-color: blue; }").unwrap();
        let styled = apply_styles(&dom, &stylesheet);
        assert_eq!(styled.styles.border_top_color, Rgba::rgb(0, 0, 255));
    }

    #[test]
    fn border_presentational_hint_sets_table_border_and_collapse() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "table".to_string(),
                attributes: vec![("border".to_string(), "3".to_string())],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "tr".to_string(),
                    attributes: vec![],
                },
                children: vec![DomNode {
                    node_type: DomNodeType::Element {
                        tag_name: "td".to_string(),
                        attributes: vec![],
                    },
                    children: vec![],
                }],
            }],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert_eq!(styled.styles.border_top_width, Length::px(3.0));
        assert_eq!(styled.styles.border_left_style, BorderStyle::Solid);
        assert!(matches!(styled.styles.border_collapse, BorderCollapse::Collapse));

        let cell = &styled.children[0].children[0];
        assert_eq!(cell.styles.border_top_width, Length::px(3.0));
        assert_eq!(cell.styles.border_left_style, BorderStyle::Solid);
    }

    #[test]
    fn author_css_overrides_border_presentational_hint() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "table".to_string(),
                attributes: vec![("border".to_string(), "3".to_string())],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "tr".to_string(),
                    attributes: vec![],
                },
                children: vec![DomNode {
                    node_type: DomNodeType::Element {
                        tag_name: "td".to_string(),
                        attributes: vec![],
                    },
                    children: vec![],
                }],
            }],
        };

        let stylesheet =
            parse_stylesheet("table { border: 1px dotted red; border-collapse: separate; } td { border: 0; }").unwrap();
        let styled = apply_styles(&dom, &stylesheet);
        assert_eq!(styled.styles.border_top_width, Length::px(1.0));
        assert_eq!(styled.styles.border_left_style, BorderStyle::Dotted);
        assert!(matches!(styled.styles.border_collapse, BorderCollapse::Separate));

        let cell = &styled.children[0].children[0];
        assert_eq!(cell.styles.border_top_width, Length::px(0.0));
        assert!(matches!(cell.styles.border_left_style, BorderStyle::None));
    }

    #[test]
    fn cursor_inherits_and_can_be_overridden() {
        let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("style".to_string(), "cursor: move;".to_string())],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "span".to_string(),
                    attributes: vec![],
                },
                children: vec![],
            }],
        };

        let styled = apply_styles(&parent, &StyleSheet::new());
        let child = styled.children.first().expect("child");
        assert_eq!(child.styles.cursor, CursorKeyword::Move);

        let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("style".to_string(), "cursor: move;".to_string())],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "span".to_string(),
                    attributes: vec![("style".to_string(), "cursor: pointer;".to_string())],
                },
                children: vec![],
            }],
        };
        let styled = apply_styles(&parent, &StyleSheet::new());
        let child = styled.children.first().expect("child");
        assert_eq!(child.styles.cursor, CursorKeyword::Pointer);
    }

    #[test]
    fn ol_type_attribute_sets_list_style_type() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "ol".to_string(),
                attributes: vec![("type".to_string(), "A".to_string())],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert!(matches!(
            styled.styles.list_style_type,
            crate::style::types::ListStyleType::UpperAlpha
        ));
    }

    #[test]
    fn li_type_attribute_overrides_parent_style() {
        let child = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "li".to_string(),
                attributes: vec![("type".to_string(), "i".to_string())],
            },
            children: vec![],
        };
        let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "ol".to_string(),
                attributes: vec![("type".to_string(), "A".to_string())],
            },
            children: vec![child],
        };

        let styled = apply_styles(&parent, &StyleSheet::new());
        let li = styled.children.first().expect("li");
        assert!(matches!(
            li.styles.list_style_type,
            crate::style::types::ListStyleType::LowerRoman
        ));
    }

    #[test]
    fn ul_type_attribute_uses_presentational_hint() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "ul".to_string(),
                attributes: vec![("type".to_string(), "circle".to_string())],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert!(matches!(
            styled.styles.list_style_type,
            crate::style::types::ListStyleType::Circle
        ));
    }

    #[test]
    fn author_css_overrides_type_presentational_hint() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "ol".to_string(),
                attributes: vec![("type".to_string(), "I".to_string())],
            },
            children: vec![],
        };

        let stylesheet = parse_stylesheet("ol { list-style-type: lower-alpha; }").unwrap();
        let styled = apply_styles(&dom, &stylesheet);
        assert!(matches!(
            styled.styles.list_style_type,
            crate::style::types::ListStyleType::LowerAlpha
        ));
    }

    #[test]
    fn align_attribute_sets_text_align_on_table_cells() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "td".to_string(),
                attributes: vec![("align".to_string(), "right".to_string())],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert!(matches!(
            styled.styles.text_align,
            crate::style::types::TextAlign::Right
        ));
    }

    #[test]
    fn valign_attribute_sets_vertical_align_on_cells() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "td".to_string(),
                attributes: vec![("valign".to_string(), "middle".to_string())],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert!(matches!(
            styled.styles.vertical_align,
            crate::style::types::VerticalAlign::Middle
        ));
    }

    #[test]
    fn table_cells_default_to_middle_alignment() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "td".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert!(matches!(
            styled.styles.vertical_align,
            crate::style::types::VerticalAlign::Middle
        ));
    }

    #[test]
    fn css_overrides_align_presentational_hint() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "td".to_string(),
                attributes: vec![
                    ("align".to_string(), "center".to_string()),
                    ("style".to_string(), "text-align: left;".to_string()),
                ],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert!(matches!(styled.styles.text_align, crate::style::types::TextAlign::Left));
    }

    #[test]
    fn align_attribute_maps_on_block_elements() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("align".to_string(), "center".to_string())],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert!(matches!(
            styled.styles.text_align,
            crate::style::types::TextAlign::Center
        ));
    }

    #[test]
    fn align_attribute_on_table_centers_box() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "table".to_string(),
                attributes: vec![("align".to_string(), "center".to_string())],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert!(matches!(
            styled.styles.text_align,
            crate::style::types::TextAlign::Center
        ));
        assert!(styled.styles.margin_left.is_none());
        assert!(styled.styles.margin_right.is_none());
    }

    #[test]
    fn align_attribute_on_table_left_collapses_right_margin() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "table".to_string(),
                attributes: vec![("align".to_string(), "left".to_string())],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert!(matches!(styled.styles.text_align, crate::style::types::TextAlign::Left));
        assert_eq!(styled.styles.margin_left, Some(Length::px(0.0)));
        assert!(styled.styles.margin_right.is_none());
    }

    #[test]
    fn align_attribute_on_table_right_collapses_left_margin() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "table".to_string(),
                attributes: vec![("align".to_string(), "right".to_string())],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert!(matches!(
            styled.styles.text_align,
            crate::style::types::TextAlign::Right
        ));
        assert!(styled.styles.margin_left.is_none());
        assert_eq!(styled.styles.margin_right, Some(Length::px(0.0)));
    }

    #[test]
    fn align_attribute_on_image_sets_float() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "img".to_string(),
                attributes: vec![("align".to_string(), "left".to_string())],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert!(matches!(styled.styles.float, crate::style::float::Float::Left));
    }

    #[test]
    fn align_attribute_on_image_sets_vertical_align() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "img".to_string(),
                attributes: vec![("align".to_string(), "middle".to_string())],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert!(matches!(
            styled.styles.vertical_align,
            crate::style::types::VerticalAlign::Middle
        ));
    }

    #[test]
    fn author_css_overrides_presentational_image_align() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "img".to_string(),
                attributes: vec![
                    ("align".to_string(), "right".to_string()),
                    ("style".to_string(), "float: left; vertical-align: top;".to_string()),
                ],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert!(matches!(styled.styles.float, crate::style::float::Float::Left));
        assert!(matches!(
            styled.styles.vertical_align,
            crate::style::types::VerticalAlign::Top
        ));
    }

    #[test]
    fn center_element_defaults_to_center_alignment() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "center".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert!(matches!(
            styled.styles.text_align,
            crate::style::types::TextAlign::Center
        ));
    }

    #[test]
    fn table_cells_wrap_by_default() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "td".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert!(matches!(
            styled.styles.white_space,
            crate::style::types::WhiteSpace::Normal
        ));
    }

    #[test]
    fn nowrap_attribute_sets_white_space_nowrap() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "td".to_string(),
                attributes: vec![("nowrap".to_string(), "".to_string())],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert!(matches!(
            styled.styles.white_space,
            crate::style::types::WhiteSpace::Nowrap
        ));
    }

    #[test]
    fn author_css_overrides_nowrap_presentational_hint() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "td".to_string(),
                attributes: vec![
                    ("nowrap".to_string(), "".to_string()),
                    ("style".to_string(), "white-space: normal;".to_string()),
                ],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert!(matches!(
            styled.styles.white_space,
            crate::style::types::WhiteSpace::Normal
        ));
    }

    #[test]
    fn nobr_element_defaults_to_nowrap() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "nobr".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert!(matches!(
            styled.styles.white_space,
            crate::style::types::WhiteSpace::Nowrap
        ));
    }

    #[test]
    fn text_align_shorthand_resets_text_align_last_to_auto() {
        let dom = element_with_style("text-align-last: right; text-align: center;");
        let styled = apply_styles(&dom, &StyleSheet::new());
        assert!(matches!(
            styled.styles.text_align,
            crate::style::types::TextAlign::Center
        ));
        assert!(matches!(
            styled.styles.text_align_last,
            crate::style::types::TextAlignLast::Auto
        ));
    }

    #[test]
    fn text_align_all_does_not_reset_text_align_last() {
        let dom = element_with_style("text-align-last: right; text-align-all: justify;");
        let styled = apply_styles(&dom, &StyleSheet::new());
        assert!(matches!(
            styled.styles.text_align,
            crate::style::types::TextAlign::Justify
        ));
        assert!(matches!(
            styled.styles.text_align_last,
            crate::style::types::TextAlignLast::Right
        ));
    }

    #[test]
    fn will_change_parses_hint_list() {
        let dom = element_with_style("will-change: transform, opacity");
        let styled = apply_styles(&dom, &StyleSheet::new());
        match &styled.styles.will_change {
            WillChange::Hints(hints) => {
                assert_eq!(
                    hints,
                    &vec![
                        WillChangeHint::Property("transform".to_string()),
                        WillChangeHint::Property("opacity".to_string())
                    ]
                );
            }
            other => panic!("expected hints, got {other:?}"),
        }
    }

    #[test]
    fn will_change_accepts_auto_and_rejects_invalid() {
        let auto = element_with_style("will-change: auto");
        let styled_auto = apply_styles(&auto, &StyleSheet::new());
        assert!(matches!(styled_auto.styles.will_change, WillChange::Auto));

        let invalid = element_with_style("will-change: auto, transform");
        let styled_invalid = apply_styles(&invalid, &StyleSheet::new());
        assert!(matches!(styled_invalid.styles.will_change, WillChange::Auto));
    }

    #[test]
    fn text_decoration_propagates_to_descendants() {
        let child = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "span".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };
        let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("style".to_string(), "text-decoration: underline;".to_string())],
            },
            children: vec![child],
        };

        let styled = apply_styles(&parent, &StyleSheet::new());
        let child_styles = &styled.children[0].styles;
        assert_eq!(child_styles.applied_text_decorations.len(), 1);
        assert!(child_styles.applied_text_decorations[0]
            .decoration
            .lines
            .contains(TextDecorationLine::UNDERLINE));
    }

    #[test]
    fn underline_offset_and_position_inherit() {
        let child = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "span".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };
        let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![(
                    "style".to_string(),
                    "text-underline-offset: 2px; text-underline-position: under right;".to_string(),
                )],
            },
            children: vec![child],
        };

        let styled = apply_styles(&parent, &StyleSheet::new());
        assert!(
            matches!(styled.styles.text_underline_position, TextUnderlinePosition::UnderRight),
            "parent got {:?}",
            styled.styles.text_underline_position
        );
        let child_styles = &styled.children[0].styles;
        match child_styles.text_underline_offset {
            TextUnderlineOffset::Length(l) => assert!((l.to_px() - 2.0).abs() < 0.01),
            other => panic!("expected underline offset to inherit, got {:?}", other),
        }
        assert!(
            matches!(child_styles.text_underline_position, TextUnderlinePosition::UnderRight),
            "got {:?}",
            child_styles.text_underline_position
        );
    }

    #[test]
    fn text_emphasis_inherits() {
        let child = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "span".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };
        let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![(
                    "style".to_string(),
                    "text-emphasis-style: open dot; text-emphasis-color: red; text-emphasis-position: under left;"
                        .to_string(),
                )],
            },
            children: vec![child],
        };

        let styled = apply_styles(&parent, &StyleSheet::new());
        let child_styles = &styled.children[0].styles;
        assert!(matches!(
            child_styles.text_emphasis_style,
            crate::style::types::TextEmphasisStyle::Mark {
                fill: crate::style::types::TextEmphasisFill::Open,
                shape: crate::style::types::TextEmphasisShape::Dot
            }
        ));
        assert_eq!(child_styles.text_emphasis_color, Some(Rgba::RED));
        assert!(matches!(
            child_styles.text_emphasis_position,
            crate::style::types::TextEmphasisPosition::UnderLeft
        ));
    }

    #[test]
    fn line_break_inherits() {
        let child = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "span".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };
        let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("style".to_string(), "line-break: anywhere;".to_string())],
            },
            children: vec![child],
        };

        let styled = apply_styles(&parent, &StyleSheet::new());
        let child_styles = &styled.children[0].styles;
        assert_eq!(child_styles.line_break, LineBreak::Anywhere);
    }

    #[test]
    fn grid_templates_do_not_inherit() {
        let child = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };
        let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![(
                    "style".to_string(),
                    "display: grid; \
                     grid-template-columns: [a] 50px [b] 1fr [c]; \
                     grid-template-rows: [top] 10px [middle] auto [bottom]; \
                     grid-template-areas: \"hero sidebar\" \"footer footer\";"
                        .to_string(),
                )],
            },
            children: vec![child],
        };

        let styled = apply_styles(&parent, &StyleSheet::new());
        let parent_styles = &styled.styles;
        assert_eq!(parent_styles.grid_template_columns.len(), 2);
        assert_eq!(parent_styles.grid_column_line_names.len(), 3);
        assert_eq!(parent_styles.grid_template_rows.len(), 2);
        assert_eq!(parent_styles.grid_row_line_names.len(), 3);
        assert_eq!(parent_styles.grid_template_areas.len(), 2);

        let child_styles = &styled.children[0].styles;
        assert!(child_styles.grid_template_columns.is_empty());
        assert!(child_styles.grid_template_rows.is_empty());
        assert!(child_styles.grid_template_areas.is_empty());
        assert!(child_styles.grid_column_line_names.is_empty());
        assert!(child_styles.grid_row_line_names.is_empty());
        assert!(child_styles.grid_column_names.is_empty());
        assert!(child_styles.grid_row_names.is_empty());
    }

    #[test]
    fn image_rendering_does_not_inherit() {
        let child = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "span".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };
        let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("style".to_string(), "image-rendering: pixelated;".to_string())],
            },
            children: vec![child],
        };

        let styled = apply_styles(&parent, &StyleSheet::new());
        assert!(matches!(
            styled.styles.image_rendering,
            crate::style::types::ImageRendering::Pixelated
        ));
        assert!(matches!(
            styled.children[0].styles.image_rendering,
            crate::style::types::ImageRendering::Auto
        ));
    }

    #[test]
    fn text_decoration_none_breaks_propagation() {
        let child = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "span".to_string(),
                attributes: vec![("style".to_string(), "text-decoration: none;".to_string())],
            },
            children: vec![],
        };
        let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("style".to_string(), "text-decoration: underline;".to_string())],
            },
            children: vec![child],
        };

        let styled = apply_styles(&parent, &StyleSheet::new());
        let child_styles = &styled.children[0].styles;
        assert!(child_styles.applied_text_decorations.is_empty());
    }

    #[test]
    fn text_decoration_adds_to_parent_decoration() {
        let child = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "span".to_string(),
                attributes: vec![("style".to_string(), "text-decoration: overline;".to_string())],
            },
            children: vec![],
        };
        let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("style".to_string(), "text-decoration: underline;".to_string())],
            },
            children: vec![child],
        };

        let styled = apply_styles(&parent, &StyleSheet::new());
        let child_styles = &styled.children[0].styles;
        assert_eq!(child_styles.applied_text_decorations.len(), 2);
        assert!(child_styles.applied_text_decorations[0]
            .decoration
            .lines
            .contains(TextDecorationLine::UNDERLINE));
        assert!(child_styles.applied_text_decorations[1]
            .decoration
            .lines
            .contains(TextDecorationLine::OVERLINE));
    }

    #[test]
    fn text_align_justify_all_sets_last_line_justify() {
        let dom = element_with_style("text-align-last: right; text-align: justify-all;");
        let styled = apply_styles(&dom, &StyleSheet::new());
        assert!(matches!(
            styled.styles.text_align,
            crate::style::types::TextAlign::JustifyAll
        ));
        assert!(matches!(
            styled.styles.text_align_last,
            crate::style::types::TextAlignLast::Justify
        ));
    }

    #[test]
    fn text_align_match_parent_maps_start_end_using_parent_direction() {
        let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("style".to_string(), "direction: rtl; text-align: start;".to_string())],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "span".to_string(),
                    attributes: vec![("style".to_string(), "text-align: match-parent;".to_string())],
                },
                children: vec![],
            }],
        };
        let styled = apply_styles(&parent, &StyleSheet::new());
        let child = styled.children.first().expect("child");
        assert!(matches!(child.styles.text_align, crate::style::types::TextAlign::Right));
    }

    #[test]
    fn text_align_match_parent_inherits_parent_alignment() {
        let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("style".to_string(), "text-align: center;".to_string())],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "span".to_string(),
                    attributes: vec![("style".to_string(), "text-align: match-parent;".to_string())],
                },
                children: vec![],
            }],
        };
        let styled = apply_styles(&parent, &StyleSheet::new());
        let child = styled.children.first().expect("child");
        assert!(matches!(
            child.styles.text_align,
            crate::style::types::TextAlign::Center
        ));
    }

    #[test]
    fn text_align_last_match_parent_maps_start_end_using_parent_direction() {
        let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![(
                    "style".to_string(),
                    "direction: rtl; text-align-last: start;".to_string(),
                )],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "span".to_string(),
                    attributes: vec![("style".to_string(), "text-align-last: match-parent;".to_string())],
                },
                children: vec![],
            }],
        };

        let styled = apply_styles(&parent, &StyleSheet::new());
        let child = styled.children.first().expect("child");
        assert!(matches!(
            child.styles.text_align_last,
            crate::style::types::TextAlignLast::Right
        ));
    }

    #[test]
    fn inherit_keyword_applies_to_non_inherited_width() {
        let child = element_with_style("width: inherit;");
        let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("style".to_string(), "width: 80px;".to_string())],
            },
            children: vec![child],
        };

        let styled = apply_styles(&parent, &StyleSheet::new());
        let child_styles = &styled.children[0].styles;
        assert_eq!(child_styles.width, Some(Length::px(80.0)));
    }

    #[test]
    fn unset_keyword_inherits_for_inherited_properties() {
        let child = element_with_style("color: unset;");
        let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("style".to_string(), "color: red;".to_string())],
            },
            children: vec![child],
        };

        let styled = apply_styles(&parent, &StyleSheet::new());
        let child_styles = &styled.children[0].styles;
        assert_eq!(child_styles.color, Rgba::RED);
    }

    #[test]
    fn unset_keyword_resets_non_inherited_border_style() {
        let child = element_with_style("border-style: unset; border-width: 4px;");
        let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![(
                    "style".to_string(),
                    "border-style: solid; border-width: 2px;".to_string(),
                )],
            },
            children: vec![child],
        };

        let styled = apply_styles(&parent, &StyleSheet::new());
        let child_styles = &styled.children[0].styles;
        assert!(matches!(child_styles.border_top_style, BorderStyle::None));
        assert!(matches!(child_styles.border_right_style, BorderStyle::None));
        assert!(matches!(child_styles.border_bottom_style, BorderStyle::None));
        assert!(matches!(child_styles.border_left_style, BorderStyle::None));
    }

    #[test]
    fn initial_keyword_resets_background_color() {
        let child = element_with_style("background-color: initial;");
        let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("style".to_string(), "background-color: red;".to_string())],
            },
            children: vec![child],
        };

        let styled = apply_styles(&parent, &StyleSheet::new());
        let child_styles = &styled.children[0].styles;
        assert_eq!(child_styles.background_color, Rgba::TRANSPARENT);
    }

    #[test]
    fn text_align_match_parent_sets_last_to_match_parent_and_resolves() {
        let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![(
                    "style".to_string(),
                    "direction: rtl; text-align-last: start;".to_string(),
                )],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "span".to_string(),
                    attributes: vec![("style".to_string(), "text-align: match-parent;".to_string())],
                },
                children: vec![],
            }],
        };

        let styled = apply_styles(&parent, &StyleSheet::new());
        let child = styled.children.first().expect("child");
        assert!(matches!(child.styles.text_align, crate::style::types::TextAlign::Right));
        assert!(matches!(
            child.styles.text_align_last,
            crate::style::types::TextAlignLast::Right
        ));
    }

    #[test]
    fn root_match_parent_text_align_last_computes_to_start() {
        let dom = element_with_style("direction: rtl; text-align-last: match-parent;");
        let styled = apply_styles(&dom, &StyleSheet::new());
        assert!(matches!(
            styled.styles.text_align_last,
            crate::style::types::TextAlignLast::Start
        ));
    }

    #[test]
    fn list_style_inherits_from_parent() {
        let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "ul".to_string(),
                attributes: vec![(
                    "style".to_string(),
                    "list-style-type: square; list-style-position: inside;".to_string(),
                )],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "li".to_string(),
                    attributes: vec![("style".to_string(), "color: red;".to_string())],
                },
                children: vec![],
            }],
        };

        let styled = apply_styles(&parent, &StyleSheet::new());
        let li = styled.children.first().expect("li");
        assert!(matches!(li.styles.list_style_type, ListStyleType::Square));
        assert!(matches!(li.styles.list_style_position, ListStylePosition::Inside));
    }

    #[test]
    fn marker_pseudo_resets_box_model_and_forces_inline_display() {
        let lone_li = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "li".to_string(),
                attributes: vec![("style".to_string(), "color: red;".to_string())],
            },
            children: vec![],
        };
        assert_eq!(lone_li.get_attribute("style"), Some("color: red;".to_string()));
        let decls = parse_declarations("color: red;");
        assert_eq!(decls.len(), 1);
        if let crate::css::types::PropertyValue::Color(c) = decls[0].value {
            assert_eq!(c, Rgba::RED);
        } else {
            panic!("color did not parse");
        }
        let mut manual = get_default_styles_for_element(&lone_li);
        inherit_styles(&mut manual, &ComputedStyle::default());
        let fs = manual.font_size;
        for decl in parse_declarations("color: red;") {
            apply_declaration(&mut manual, &decl, &ComputedStyle::default(), fs, fs);
        }
        assert_eq!(manual.color, Rgba::RED);
        let lone_styled = apply_styles(&lone_li, &StyleSheet::new());
        assert_eq!(lone_styled.styles.color, Rgba::RED);

        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "ul".to_string(),
                attributes: vec![],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "li".to_string(),
                    attributes: vec![("style".to_string(), "color: red;".to_string())],
                },
                children: vec![],
            }],
        };

        let stylesheet = parse_stylesheet(
            r#"
            li::marker {
                color: red;
                display: block;
                padding: 10px;
                margin-left: 12px;
                background: blue;
                text-decoration: underline;
                text-indent: 40px;
            }
        "#,
        )
        .unwrap();

        let styled = apply_styles(&dom, &stylesheet);
        let li = styled.children.first().expect("li");
        assert_eq!(li.styles.color, Rgba::RED);
        let marker = li.marker_styles.as_ref().expect("marker styles");

        assert_eq!(marker.color, Rgba::RED);
        assert!(matches!(marker.display, Display::Inline));
        assert!(marker.padding_left.is_zero());
        assert!(marker.padding_right.is_zero());
        assert!(marker.margin_left.unwrap().is_zero());
        assert_eq!(marker.background_color, Rgba::TRANSPARENT);
        assert_eq!(marker.text_transform, crate::style::types::TextTransform::none());
        assert!(marker
            .text_decoration
            .lines
            .contains(crate::style::types::TextDecorationLine::UNDERLINE));
        assert_eq!(marker.applied_text_decorations.len(), 1);
        assert!(matches!(marker.text_align, crate::style::types::TextAlign::Start));
        assert_eq!(marker.text_indent, crate::style::types::TextIndent::default());
        assert!(matches!(marker.float, Float::None));
        assert!(marker.transform.is_empty());
        assert_eq!(marker.opacity, 1.0);
    }

    #[test]
    fn marker_style_inherits_from_list_item_not_parent() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("style".to_string(), "color: blue;".to_string())],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "ul".to_string(),
                    attributes: vec![],
                },
                children: vec![DomNode {
                    node_type: DomNodeType::Element {
                        tag_name: "li".to_string(),
                        attributes: vec![("style".to_string(), "color: red;".to_string())],
                    },
                    children: vec![],
                }],
            }],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        let ul = styled.children.first().expect("ul");
        let li = ul.children.first().expect("li");
        let marker = li.marker_styles.as_ref().expect("marker styles");
        assert_eq!(
            marker.color,
            Rgba::RED,
            "marker should inherit color from list item, not its parent"
        );
    }

    #[test]
    fn marker_rule_cannot_override_list_style_type_or_image() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "ul".to_string(),
                attributes: vec![],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "li".to_string(),
                    attributes: vec![("style".to_string(), "list-style-type: decimal;".to_string())],
                },
                children: vec![],
            }],
        };

        let stylesheet = parse_stylesheet(
            r#"
            li::marker {
                list-style-type: square;
                list-style-image: url(bullet.png);
            }
        "#,
        )
        .unwrap();

        let styled = apply_styles(&dom, &stylesheet);
        let li = styled.children.first().expect("li");
        let marker = li.marker_styles.as_ref().expect("marker styles");
        // list-style declarations on ::marker are ignored; marker retains the list-item value.
        assert!(
            matches!(marker.list_style_type, ListStyleType::Decimal),
            "marker should ignore list-style-type on ::marker"
        );
        assert!(
            matches!(marker.list_style_image, crate::style::types::ListStyleImage::None),
            "marker should ignore list-style-image on ::marker"
        );
    }

    #[test]
    fn marker_allows_text_combine_and_typography_but_ignores_non_text_box_properties() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "ul".to_string(),
                attributes: vec![],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "li".to_string(),
                    attributes: vec![("style".to_string(), "letter-spacing: 0px;".to_string())],
                },
                children: vec![],
            }],
        };

        let stylesheet = parse_stylesheet(
            r#"
            li::marker {
                text-combine-upright: all;
                letter-spacing: 2px;
                visibility: hidden;
            }
        "#,
        )
        .unwrap();

        let media_ctx = MediaContext::screen(1200.0, 800.0);
        let rule_refs: Vec<CascadeRule<'_>> = stylesheet
            .collect_style_rules(&media_ctx)
            .into_iter()
            .enumerate()
            .map(|(order, rule)| CascadeRule {
                origin: StyleOrigin::Author,
                order,
                rule: rule.rule,
                layer_order: rule.layer_order,
                container_conditions: rule.container_conditions.clone(),
            })
            .collect();
        let rule_index = RuleIndex::new(rule_refs);
        assert_eq!(rule_index.rules.len(), 1, "should collect the authored li::marker rule");
        let ancestors: Vec<&DomNode> = vec![&dom]; // ul ancestor for the li
        let element_ref = build_element_ref_chain(&dom.children[0], &ancestors);
        let mut caches = SelectorCaches::default();
        let mut context = MatchingContext::new(
            MatchingMode::ForStatelessPseudoElement,
            None,
            &mut caches,
            QuirksMode::NoQuirks,
            selectors::matching::NeedsSelectorFlags::No,
            selectors::matching::MatchingForInvalidation::No,
        );
        let selector = rule_index.rules[0]
            .rule
            .selectors
            .slice()
            .first()
            .expect("selector in rule");
        assert!(
            matches_selector(selector, 0, None, &element_ref, &mut context),
            "selector should match the originating element"
        );
        let mut caches = SelectorCaches::default();
        let marker_matches = find_pseudo_element_rules(
            &dom.children[0],
            &rule_index,
            &mut caches,
            &ancestors,
            &PseudoElement::Marker,
        );
        assert_eq!(marker_matches.len(), 1, "marker rules should match li::marker");

        let styled = apply_styles(&dom, &stylesheet);
        let li = styled.children.first().expect("li");
        let marker = li.marker_styles.as_ref().expect("marker styles");

        assert!(
            marker_allows_property("text-combine-upright"),
            "filter should allow text-combine-upright"
        );
        assert!(
            (marker.letter_spacing - 2.0).abs() < f32::EPSILON,
            "letter-spacing should apply to marker contents (got {})",
            marker.letter_spacing
        );
        assert_eq!(
            marker.text_combine_upright,
            TextCombineUpright::All,
            "text-combine-upright should be honored on ::marker"
        );
        assert!(
            matches!(marker.visibility, Visibility::Visible),
            "non-text properties like visibility should be ignored for marker boxes"
        );
    }

    #[test]
    fn marker_ua_defaults_match_css_lists() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "ul".to_string(),
                attributes: vec![],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "li".to_string(),
                    attributes: vec![],
                },
                children: vec![],
            }],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        let li = styled.children.first().expect("li");
        let marker = li.marker_styles.as_ref().expect("marker styles");

        assert!(
            matches!(marker.unicode_bidi, UnicodeBidi::Isolate),
            "marker should default unicode-bidi to isolate"
        );
        assert!(
            matches!(marker.white_space, WhiteSpace::Pre),
            "marker should default white-space to pre"
        );
        assert_eq!(
            marker.font_variant_numeric.spacing,
            crate::style::types::NumericSpacing::Tabular,
            "marker should default to tabular numbers"
        );
        assert_eq!(
            marker.text_transform,
            crate::style::types::TextTransform::none(),
            "marker should default text-transform to none"
        );
    }

    #[test]
    fn marker_rejects_alignment_and_indent_declarations() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "ul".to_string(),
                attributes: vec![],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "li".to_string(),
                    attributes: vec![],
                },
                children: vec![],
            }],
        };

        let stylesheet = parse_stylesheet(
            r#"
            li::marker {
                text-align: center;
                text-indent: 40px;
            }
        "#,
        )
        .unwrap();

        let styled = apply_styles(&dom, &stylesheet);
        let li = styled.children.first().expect("li");
        let marker = li.marker_styles.as_ref().expect("marker styles");

        assert!(
            matches!(marker.text_align, crate::style::types::TextAlign::Start),
            "text-align should be ignored on ::marker"
        );
        assert_eq!(
            marker.text_indent,
            crate::style::types::TextIndent::default(),
            "text-indent should be ignored on ::marker"
        );

        let stylesheet = parse_stylesheet(
            r#"
            li::marker {
                text-decoration: underline;
                text-shadow: 1px 1px red;
            }
        "#,
        )
        .unwrap();
        let styled = apply_styles(&dom, &stylesheet);
        let li = styled.children.first().expect("li");
        let marker = li.marker_styles.as_ref().expect("marker styles");

        assert!(
            marker
                .text_decoration
                .lines
                .contains(crate::style::types::TextDecorationLine::UNDERLINE),
            "authored text decorations should apply to ::marker"
        );
        assert!(
            marker.applied_text_decorations.len() == 1,
            "resolved text decorations should be captured for ::marker"
        );
        assert_eq!(marker.text_shadow.len(), 1, "text-shadow should apply to ::marker");
    }

    #[test]
    fn marker_alignment_does_not_apply_or_inherit() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "ul".to_string(),
                attributes: vec![],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "li".to_string(),
                    attributes: vec![(
                        "style".to_string(),
                        "text-align: center; text-align-last: right; text-indent: 2em;".to_string(),
                    )],
                },
                children: vec![],
            }],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        let li = styled.children.first().expect("li");
        let marker = li.marker_styles.as_ref().expect("marker styles");

        assert!(
            matches!(marker.text_align, crate::style::types::TextAlign::Start),
            "::marker should not inherit text-align from the list item"
        );
        assert!(
            matches!(marker.text_align_last, crate::style::types::TextAlignLast::Auto),
            "::marker should keep default text-align-last"
        );
        assert_eq!(
            marker.text_indent,
            crate::style::types::TextIndent::default(),
            "::marker should not inherit text-indent"
        );
    }

    #[test]
    fn marker_allows_font_shorthand() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "ul".to_string(),
                attributes: vec![],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "li".to_string(),
                    attributes: vec![],
                },
                children: vec![],
            }],
        };

        let stylesheet = parse_stylesheet(
            r#"
            li::marker {
                font: italic 700 18px/2 "Example";
            }
        "#,
        )
        .unwrap();

        let styled = apply_styles(&dom, &stylesheet);
        let li = styled.children.first().expect("li");
        let marker = li.marker_styles.as_ref().expect("marker styles");

        assert_eq!(marker.font_size, 18.0);
        assert!(matches!(marker.font_style, crate::style::types::FontStyle::Italic));
        assert_eq!(marker.font_weight.to_u16(), 700);
        assert_eq!(marker.font_family.first().map(|s| s.as_str()), Some("Example"));
    }

    #[test]
    fn marker_author_overrides_ua_defaults() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "ul".to_string(),
                attributes: vec![],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "li".to_string(),
                    attributes: vec![("style".to_string(), "color: black;".to_string())],
                },
                children: vec![],
            }],
        };

        let stylesheet = parse_stylesheet(
            r#"
            li::marker {
                white-space: normal;
                unicode-bidi: normal;
                text-transform: uppercase;
            }
        "#,
        )
        .unwrap();

        let styled = apply_styles(&dom, &stylesheet);
        let li = styled.children.first().expect("li");
        let marker = li.marker_styles.as_ref().expect("marker styles");

        assert!(
            matches!(marker.white_space, WhiteSpace::Normal),
            "author white-space should override UA default"
        );
        assert!(
            matches!(marker.unicode_bidi, UnicodeBidi::Normal),
            "author unicode-bidi should override UA default"
        );
        assert_eq!(
            marker.text_transform,
            crate::style::types::TextTransform::with_case(crate::style::types::CaseTransform::Uppercase),
            "author text-transform should override UA default"
        );
    }

    #[test]
    fn marker_allows_text_emphasis_properties() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "ul".to_string(),
                attributes: vec![],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "li".to_string(),
                    attributes: vec![("style".to_string(), "color: black;".to_string())],
                },
                children: vec![],
            }],
        };

        let stylesheet = parse_stylesheet(
            r#"
            li::marker {
                text-emphasis-style: open dot;
                text-emphasis-color: red;
                text-emphasis-position: under right;
            }
        "#,
        )
        .unwrap();

        let styled = apply_styles(&dom, &stylesheet);
        let li = styled.children.first().expect("li");
        let marker = li.marker_styles.as_ref().expect("marker styles");

        assert_eq!(
            marker.text_emphasis_style,
            crate::style::types::TextEmphasisStyle::Mark {
                fill: crate::style::types::TextEmphasisFill::Open,
                shape: crate::style::types::TextEmphasisShape::Dot
            },
            "text-emphasis-style should apply to ::marker"
        );
        assert_eq!(
            marker.text_emphasis_color,
            Some(Rgba::RED),
            "text-emphasis-color should apply to ::marker"
        );
        assert_eq!(
            marker.text_emphasis_position,
            crate::style::types::TextEmphasisPosition::UnderRight,
            "text-emphasis-position should apply to ::marker"
        );
    }

    #[test]
    fn marker_allows_cursor_property() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "ul".to_string(),
                attributes: vec![],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "li".to_string(),
                    attributes: vec![],
                },
                children: vec![],
            }],
        };

        let stylesheet = parse_stylesheet(
            r#"
            li::marker {
                cursor: move;
            }
        "#,
        )
        .unwrap();

        let styled = apply_styles(&dom, &stylesheet);
        let li = styled.children.first().expect("li");
        let marker = li.marker_styles.as_ref().expect("marker styles");

        assert!(
            matches!(marker.cursor, crate::style::types::CursorKeyword::Move),
            "cursor should be honored on ::marker"
        );
    }

    #[test]
    fn font_weight_relative_keywords_follow_css_fonts_table() {
        assert_eq!(child_font_weight("font-weight: 50;", "font-weight: bolder;"), 400);
        assert_eq!(child_font_weight("font-weight: 50;", "font-weight: lighter;"), 50);

        assert_eq!(child_font_weight("font-weight: 500;", "font-weight: bolder;"), 700);
        assert_eq!(child_font_weight("font-weight: 500;", "font-weight: lighter;"), 100);

        assert_eq!(child_font_weight("font-weight: 650;", "font-weight: bolder;"), 900);
        assert_eq!(child_font_weight("font-weight: 650;", "font-weight: lighter;"), 400);

        assert_eq!(child_font_weight("font-weight: 800;", "font-weight: bolder;"), 900);
        assert_eq!(child_font_weight("font-weight: 800;", "font-weight: lighter;"), 700);

        assert_eq!(child_font_weight("font-weight: 950;", "font-weight: bolder;"), 950);
        assert_eq!(child_font_weight("font-weight: 950;", "font-weight: lighter;"), 700);
    }

    #[test]
    fn out_of_range_font_weight_is_ignored() {
        let dom = element_with_style("font-weight: 1200;");
        let styled = apply_styles(&dom, &StyleSheet::new());
        assert_eq!(styled.styles.font_weight.to_u16(), 400);
    }
}

fn find_matching_rules<'a>(
    node: &DomNode,
    rules: &'a RuleIndex<'a>,
    selector_caches: &mut SelectorCaches,
    ancestors: &[&DomNode],
    ancestor_ids: &[usize],
    node_id: usize,
    container_ctx: Option<&ContainerQueryContext>,
) -> Vec<MatchedRule<'a>> {
    if !node.is_element() {
        return Vec::new();
    }
    let profiling = cascade_profile_enabled();
    let start = profiling.then(|| Instant::now());
    let mut candidates = Vec::new();
    rules.selector_candidates(node, &mut candidates);
    if candidates.is_empty() {
        return Vec::new();
    }
    let mut matches: Vec<MatchedRule<'a>> = Vec::new();
    let mut match_indices: HashMap<usize, usize> = HashMap::with_capacity(candidates.len());

    // Build ElementRef chain with proper parent links
    let element_ref = build_element_ref_chain(node, ancestors);

    // Create selector caches and matching context
    let mut context = MatchingContext::new(
        MatchingMode::Normal,
        None,
        selector_caches,
        QuirksMode::NoQuirks,
        selectors::matching::NeedsSelectorFlags::No,
        selectors::matching::MatchingForInvalidation::No,
    );

    for &selector_idx in &candidates {
        let indexed = &rules.selectors[selector_idx];
        let selector = indexed.selector;
        if matches_selector(selector, 0, None, &element_ref, &mut context) {
            let spec = indexed.specificity;
            if let Some(&pos) = match_indices.get(&indexed.rule_idx) {
                if spec > matches[pos].specificity {
                    matches[pos].specificity = spec;
                }
            } else {
                let rule = &rules.rules[indexed.rule_idx];
                if !rule.container_conditions.is_empty() {
                    match container_ctx {
                        Some(ctx) if ctx.matches(node_id, ancestor_ids, &rule.container_conditions) => {}
                        _ => continue,
                    }
                }
                let pos = matches.len();
                match_indices.insert(indexed.rule_idx, pos);
                matches.push(MatchedRule {
                    origin: rule.origin,
                    specificity: spec,
                    order: rule.order,
                    layer_order: rule.layer_order.clone(),
                    declarations: Cow::Borrowed(&rule.rule.declarations),
                });
            }
        }
    }

    // Sort by specificity (lower specificity first, so later rules override), then document order.
    matches.sort_by(|a, b| a.specificity.cmp(&b.specificity).then(a.order.cmp(&b.order)));

    if profiling {
        record_matching_stats(candidates.len(), matches.len(), start.map(|s| s.elapsed()));
    }

    matches
}

/// Find rules that match an element with a specific pseudo-element
fn find_pseudo_element_rules<'a>(
    node: &DomNode,
    rules: &'a RuleIndex<'a>,
    selector_caches: &mut SelectorCaches,
    ancestors: &[&DomNode],
    pseudo: &PseudoElement,
) -> Vec<MatchedRule<'a>> {
    if !node.is_element() {
        return Vec::new();
    }
    let profiling = cascade_profile_enabled();
    let start = profiling.then(|| Instant::now());
    let mut candidates = Vec::new();
    rules.pseudo_candidates(node, pseudo, &mut candidates);
    if candidates.is_empty() {
        return Vec::new();
    }
    let mut matches: Vec<MatchedRule<'a>> = Vec::new();
    let mut match_indices: HashMap<usize, usize> = HashMap::with_capacity(candidates.len());

    // Build ElementRef chain with proper parent links
    let element_ref = build_element_ref_chain(node, ancestors);

    // Create selector caches and matching context
    let mut context = MatchingContext::new(
        MatchingMode::ForStatelessPseudoElement,
        None,
        selector_caches,
        QuirksMode::NoQuirks,
        selectors::matching::NeedsSelectorFlags::No,
        selectors::matching::MatchingForInvalidation::No,
    );

    for &selector_idx in &candidates {
        let indexed = &rules.pseudo_selectors[selector_idx];
        let selector = indexed.selector;
        if matches_selector(selector, 0, None, &element_ref, &mut context) {
            let spec = indexed.specificity;
            if let Some(&pos) = match_indices.get(&indexed.rule_idx) {
                if spec > matches[pos].specificity {
                    matches[pos].specificity = spec;
                }
            } else {
                let rule = &rules.rules[indexed.rule_idx];
                let pos = matches.len();
                match_indices.insert(indexed.rule_idx, pos);
                matches.push(MatchedRule {
                    origin: rule.origin,
                    specificity: spec,
                    order: rule.order,
                    layer_order: rule.layer_order.clone(),
                    declarations: Cow::Borrowed(&rule.rule.declarations),
                });
            }
        }
    }

    matches.sort_by(|a, b| a.specificity.cmp(&b.specificity).then(a.order.cmp(&b.order)));

    if profiling {
        record_matching_stats(candidates.len(), matches.len(), start.map(|s| s.elapsed()));
    }

    matches
}

fn apply_cascaded_declarations<'a, F>(
    styles: &mut ComputedStyle,
    matched_rules: Vec<MatchedRule<'a>>,
    inline_declarations: Option<Vec<Declaration>>,
    parent_styles: &ComputedStyle,
    parent_font_size: f32,
    root_font_size: f32,
    viewport: Size,
    revert_base_styles: &ComputedStyle,
    filter: F,
) where
    F: Fn(&Declaration) -> bool,
{
    let mut flattened: Vec<MatchedDeclaration<'a>> = Vec::new();
    let mut total_decls = 0usize;
    for rule in matched_rules.iter() {
        total_decls += rule.declarations.len();
    }
    if let Some(inline) = inline_declarations.as_ref() {
        total_decls += inline.len();
    }
    flattened.reserve(total_decls);

    for rule in matched_rules {
        let origin = rule.origin;
        let specificity = rule.specificity;
        let order = rule.order;
        let layer_order = rule.layer_order.clone();

        match rule.declarations {
            Cow::Borrowed(decls) => {
                for (decl_order, declaration) in decls.iter().enumerate() {
                    flattened.push(MatchedDeclaration {
                        important: declaration.important,
                        origin,
                        specificity,
                        rule_order: order,
                        decl_order,
                        layer_order: layer_order.clone(),
                        declaration: Cow::Borrowed(declaration),
                    });
                }
            }
            Cow::Owned(decls) => {
                for (decl_order, declaration) in decls.into_iter().enumerate() {
                    flattened.push(MatchedDeclaration {
                        important: declaration.important,
                        origin,
                        specificity,
                        rule_order: order,
                        decl_order,
                        layer_order: layer_order.clone(),
                        declaration: Cow::Owned(declaration),
                    });
                }
            }
        }
    }

    if let Some(inline) = inline_declarations {
        for (decl_order, declaration) in inline.into_iter().enumerate() {
            flattened.push(MatchedDeclaration {
                important: declaration.important,
                origin: StyleOrigin::Inline,
                specificity: INLINE_SPECIFICITY,
                rule_order: INLINE_RULE_ORDER,
                decl_order,
                layer_order: vec![u32::MAX],
                declaration: Cow::Owned(declaration),
            });
        }
    }

    fn cmp_layer_order(a: &[u32], b: &[u32]) -> std::cmp::Ordering {
        for (ai, bi) in a.iter().zip(b.iter()) {
            if ai != bi {
                return ai.cmp(bi);
            }
        }
        a.len().cmp(&b.len())
    }

    flattened.sort_by(|a, b| {
        a.important
            .cmp(&b.important)
            .then(a.origin.rank().cmp(&b.origin.rank()))
            .then_with(|| cmp_layer_order(&a.layer_order, &b.layer_order))
            .then(a.specificity.cmp(&b.specificity))
            .then(a.rule_order.cmp(&b.rule_order))
            .then(a.decl_order.cmp(&b.decl_order))
    });

    let defaults = ComputedStyle::default();
    let mut layer_snapshots: HashMap<Vec<u32>, ComputedStyle> = HashMap::new();

    let mut apply_entry = |entry: &MatchedDeclaration<'_>| {
        if !filter(entry.declaration.as_ref()) {
            return;
        }
        let revert_base = match entry.origin {
            StyleOrigin::UserAgent => &defaults,
            StyleOrigin::Author | StyleOrigin::Inline => revert_base_styles,
        };
        let layer_base = layer_snapshots
            .entry(entry.layer_order.clone())
            .or_insert_with(|| styles.clone());
        apply_declaration_with_base(
            styles,
            entry.declaration.as_ref(),
            parent_styles,
            revert_base,
            Some(&*layer_base),
            parent_font_size,
            root_font_size,
            viewport,
        );
    };

    // First apply custom properties so later var() resolutions can see them
    for entry in flattened.iter().filter(|e| e.declaration.property.starts_with("--")) {
        apply_entry(entry);
    }
    // Then apply all other declarations in cascade order
    for entry in flattened.iter().filter(|e| !e.declaration.property.starts_with("--")) {
        apply_entry(entry);
    }
    resolve_pending_logical_properties(styles);

    resolve_absolute_lengths(styles, root_font_size, viewport);
}

fn dir_presentational_hint(
    node: &DomNode,
    fallback_direction: Direction,
    order: usize,
) -> Option<MatchedRule<'static>> {
    let tag = node.tag_name().map(|t| t.to_ascii_lowercase());
    let is_bdo = matches!(tag.as_deref(), Some("bdo"));
    let unicode_bidi_value = if is_bdo { "bidi-override" } else { "isolate" };
    let dir = node.get_attribute("dir")?;
    let dir = dir.trim().to_ascii_lowercase();
    match dir.as_str() {
        "ltr" | "rtl" => {
            let css = format!("direction: {}; unicode-bidi: {};", dir, unicode_bidi_value);
            let declarations = parse_declarations(&css);
            Some(MatchedRule {
                origin: StyleOrigin::Author,
                specificity: 0,
                order,
                layer_order: vec![u32::MAX],
                declarations: Cow::Owned(declarations),
            })
        }
        "auto" => {
            let resolved = resolve_first_strong_direction(node).map(|d| match d {
                TextDirection::Ltr => crate::style::types::Direction::Ltr,
                TextDirection::Rtl => crate::style::types::Direction::Rtl,
            });
            let dir_value = match resolved.unwrap_or(fallback_direction) {
                crate::style::types::Direction::Rtl => "rtl",
                crate::style::types::Direction::Ltr => "ltr",
            };
            let declarations = parse_declarations(&format!(
                "direction: {}; unicode-bidi: {};",
                dir_value, unicode_bidi_value
            ));
            Some(MatchedRule {
                origin: StyleOrigin::Author,
                specificity: 0,
                order,
                layer_order: vec![u32::MAX],
                declarations: Cow::Owned(declarations),
            })
        }
        _ => None,
    }
}

fn list_type_presentational_hint(node: &DomNode, order: usize) -> Option<MatchedRule<'static>> {
    let tag = node.tag_name()?.to_ascii_lowercase();
    let ty = node.get_attribute("type")?;
    let mapped = match tag.as_str() {
        "ol" | "li" => map_ol_type(&ty),
        "ul" => map_ul_type(&ty),
        _ => None,
    }?;
    let declarations = parse_declarations(&format!("list-style-type: {};", mapped));
    Some(MatchedRule {
        origin: StyleOrigin::Author,
        specificity: 0,
        order,
        layer_order: vec![u32::MAX],
        declarations: Cow::Owned(declarations),
    })
}

fn map_ol_type(value: &str) -> Option<&'static str> {
    match value.trim() {
        "1" => Some("decimal"),
        "a" => Some("lower-alpha"),
        "A" => Some("upper-alpha"),
        "i" => Some("lower-roman"),
        "I" => Some("upper-roman"),
        _ => None,
    }
}

fn map_ul_type(value: &str) -> Option<&'static str> {
    match value.trim().to_ascii_lowercase().as_str() {
        "disc" => Some("disc"),
        "circle" => Some("circle"),
        "square" => Some("square"),
        _ => None,
    }
}

fn alignment_presentational_hint(node: &DomNode, order: usize) -> Option<MatchedRule<'static>> {
    let tag = node.tag_name()?.to_ascii_lowercase();
    let mut declarations = String::new();

    if let Some(align) = node
        .get_attribute("align")
        .or_else(|| (tag == "center").then(|| "center".to_string()))
    {
        if let Some(mapped) = map_align(&align) {
            if matches!(
                tag.as_str(),
                "td" | "th" | "tr" | "table" | "div" | "p" | "h1" | "h2" | "h3" | "h4" | "h5" | "h6" | "center"
            ) {
                declarations.push_str(&format!("text-align: {};", mapped));
            }
            if tag == "table" {
                match mapped {
                    "center" => declarations.push_str("margin-left: auto; margin-right: auto;"),
                    "right" => declarations.push_str("margin-left: auto; margin-right: 0;"),
                    "left" => declarations.push_str("margin-left: 0; margin-right: auto;"),
                    _ => {}
                }
            }
        }
    }

    if matches!(tag.as_str(), "td" | "th" | "tr") {
        if let Some(valign) = node.get_attribute("valign") {
            if let Some(mapped) = map_valign(&valign) {
                declarations.push_str(&format!("vertical-align: {};", mapped));
            }
        }
    }

    if declarations.is_empty() {
        return None;
    }

    Some(MatchedRule {
        origin: StyleOrigin::Author,
        specificity: 0,
        order,
        layer_order: vec![u32::MAX],
        declarations: Cow::Owned(parse_declarations(&declarations)),
    })
}

fn dimension_presentational_hint(node: &DomNode, order: usize) -> Option<MatchedRule<'static>> {
    let width = node
        .get_attribute("width")
        .and_then(|width| parse_dimension_attribute(&width));
    let height = node
        .get_attribute("height")
        .and_then(|height| parse_dimension_attribute(&height));

    if width.is_none() && height.is_none() {
        return None;
    }

    let mut css = String::new();
    if let Some(w) = width {
        css.push_str(&format!("width: {};", w));
    }
    if let Some(h) = height {
        css.push_str(&format!("height: {};", h));
    }

    Some(MatchedRule {
        origin: StyleOrigin::Author,
        specificity: 0,
        order,
        layer_order: vec![u32::MAX],
        declarations: Cow::Owned(parse_declarations(&css)),
    })
}

fn bgcolor_presentational_hint(node: &DomNode, order: usize) -> Option<MatchedRule<'static>> {
    let color = node
        .get_attribute("bgcolor")
        .and_then(|color| parse_color_attribute(&color))?;
    let css = format!("background-color: {};", color);
    Some(MatchedRule {
        origin: StyleOrigin::Author,
        specificity: 0,
        order,
        layer_order: vec![u32::MAX],
        declarations: Cow::Owned(parse_declarations(&css)),
    })
}

fn bordercolor_presentational_hint(node: &DomNode, order: usize) -> Option<MatchedRule<'static>> {
    let color = node
        .get_attribute("bordercolor")
        .and_then(|color| parse_color_attribute(&color))?;
    let css = format!("border-color: {};", color);
    Some(MatchedRule {
        origin: StyleOrigin::Author,
        specificity: 0,
        order,
        layer_order: vec![u32::MAX],
        declarations: Cow::Owned(parse_declarations(&css)),
    })
}

fn resolve_absolute_lengths(styles: &mut ComputedStyle, root_font_size: f32, viewport: Size) {
    let resolve_len = |len: Length| -> Length {
        match len.unit {
            u if u.is_absolute() => Length::px(len.to_px()),
            LengthUnit::Em => Length::px(len.value * styles.font_size),
            LengthUnit::Ex => Length::px(len.value * styles.font_size * 0.5),
            LengthUnit::Ch => Length::px(len.value * styles.font_size * 0.5),
            LengthUnit::Rem => Length::px(len.value * root_font_size),
            u if u.is_viewport_relative() => len
                .resolve_with_viewport(viewport.width, viewport.height)
                .map(Length::px)
                .unwrap_or(len),
            _ => len,
        }
    };

    styles.border_top_width = resolve_len(styles.border_top_width);
    styles.border_right_width = resolve_len(styles.border_right_width);
    styles.border_bottom_width = resolve_len(styles.border_bottom_width);
    styles.border_left_width = resolve_len(styles.border_left_width);
    styles.border_top_left_radius = resolve_len(styles.border_top_left_radius);
    styles.border_top_right_radius = resolve_len(styles.border_top_right_radius);
    styles.border_bottom_right_radius = resolve_len(styles.border_bottom_right_radius);
    styles.border_bottom_left_radius = resolve_len(styles.border_bottom_left_radius);
    styles.outline_width = resolve_len(styles.outline_width);
    styles.outline_offset = resolve_len(styles.outline_offset);
    styles.border_spacing_horizontal = resolve_len(styles.border_spacing_horizontal);
    styles.border_spacing_vertical = resolve_len(styles.border_spacing_vertical);

    // Percentage radii remain relative and are handled during paint/layout.
}

fn table_border_value(node: &DomNode) -> Option<Length> {
    node.get_attribute("border")
        .and_then(|val| parse_dimension_attribute(&val))
        .filter(|len| len.unit.is_absolute())
}

fn find_table_border_value(ancestors: &[&DomNode], node: &DomNode) -> Option<Length> {
    if let Some(len) = table_border_value(node) {
        return Some(len);
    }
    for ancestor in ancestors.iter().rev() {
        if let Some(tag) = ancestor.tag_name() {
            if tag.eq_ignore_ascii_case("table") {
                return table_border_value(ancestor);
            }
        }
    }
    None
}

fn border_presentational_hint(node: &DomNode, ancestors: &[&DomNode], order: usize) -> Option<MatchedRule<'static>> {
    let tag = node.tag_name()?.to_ascii_lowercase();
    let border_len = match tag.as_str() {
        "table" => table_border_value(node),
        "td" | "th" | "tr" => find_table_border_value(ancestors, node),
        _ => None,
    }?;

    let mut css = format!("border: {} solid;", border_len);
    if tag == "table" && border_len.to_px() > 0.0 {
        css.push_str("border-collapse: collapse;");
    }

    Some(MatchedRule {
        origin: StyleOrigin::Author,
        specificity: 0,
        order,
        layer_order: vec![u32::MAX],
        declarations: Cow::Owned(parse_declarations(&css)),
    })
}

fn cellspacing_presentational_hint(node: &DomNode, order: usize) -> Option<MatchedRule<'static>> {
    let tag = node.tag_name()?.to_ascii_lowercase();
    if tag != "table" {
        return None;
    }
    let spacing = node.get_attribute("cellspacing")?;
    let length = parse_dimension_attribute(&spacing)?;
    let css = format!("border-spacing: {} {};", length, length);
    Some(MatchedRule {
        origin: StyleOrigin::Author,
        specificity: 0,
        order,
        layer_order: vec![u32::MAX],
        declarations: Cow::Owned(parse_declarations(&css)),
    })
}

fn find_cellpadding(ancestors: &[&DomNode], node: &DomNode) -> Option<Length> {
    if let Some(value) = node.get_attribute("cellpadding") {
        if let Some(len) = parse_dimension_attribute(&value) {
            return Some(len);
        }
    }

    for ancestor in ancestors.iter().rev() {
        if let Some(tag) = ancestor.tag_name() {
            if tag.eq_ignore_ascii_case("table") {
                if let Some(value) = ancestor.get_attribute("cellpadding") {
                    if let Some(len) = parse_dimension_attribute(&value) {
                        return Some(len);
                    }
                }
                break;
            }
        }
    }
    None
}

fn cellpadding_presentational_hint(
    node: &DomNode,
    ancestors: &[&DomNode],
    order: usize,
) -> Option<MatchedRule<'static>> {
    let tag = node.tag_name()?.to_ascii_lowercase();
    if tag != "td" && tag != "th" {
        return None;
    }
    let padding = find_cellpadding(ancestors, node)?;
    let css = format!("padding: {};", padding);
    Some(MatchedRule {
        origin: StyleOrigin::Author,
        specificity: 0,
        order,
        layer_order: vec![u32::MAX],
        declarations: Cow::Owned(parse_declarations(&css)),
    })
}

fn map_align(value: &str) -> Option<&'static str> {
    match value.trim().to_ascii_lowercase().as_str() {
        "left" => Some("left"),
        "center" => Some("center"),
        "right" => Some("right"),
        "justify" => Some("justify"),
        _ => None,
    }
}

fn map_valign(value: &str) -> Option<&'static str> {
    match value.trim().to_ascii_lowercase().as_str() {
        "top" => Some("top"),
        "middle" => Some("middle"),
        "bottom" => Some("bottom"),
        "baseline" => Some("baseline"),
        "texttop" | "text-top" => Some("text-top"),
        "absmiddle" => Some("middle"),
        "absbottom" | "textbottom" | "text-bottom" => Some("text-bottom"),
        _ => None,
    }
}

fn replaced_alignment_presentational_hint(node: &DomNode, order: usize) -> Option<MatchedRule<'static>> {
    let tag = node.tag_name()?.to_ascii_lowercase();
    // HTML presentational align on replaced elements (e.g., img, object) maps to float/vertical-align.
    if !matches!(
        tag.as_str(),
        "img" | "object" | "embed" | "iframe" | "applet" | "video" | "canvas"
    ) {
        return None;
    }

    let align = node.get_attribute("align")?;
    let align_lower = align.trim().to_ascii_lowercase();
    let mut declarations = String::new();
    match align_lower.as_str() {
        "left" => declarations.push_str("float: left;"),
        "right" => declarations.push_str("float: right;"),
        _ => {}
    }
    if let Some(valign) = map_valign(&align_lower) {
        declarations.push_str(&format!("vertical-align: {};", valign));
    }

    if declarations.is_empty() {
        return None;
    }

    Some(MatchedRule {
        origin: StyleOrigin::Author,
        specificity: 0,
        order,
        layer_order: vec![u32::MAX],
        declarations: Cow::Owned(parse_declarations(&declarations)),
    })
}

fn nowrap_presentational_hint(node: &DomNode, order: usize) -> Option<MatchedRule<'static>> {
    let tag = node.tag_name()?.to_ascii_lowercase();
    if tag != "td" && tag != "th" {
        return None;
    }
    if node.get_attribute("nowrap").is_none() {
        return None;
    }

    Some(MatchedRule {
        origin: StyleOrigin::Author,
        specificity: 0,
        order,
        layer_order: vec![u32::MAX],
        declarations: Cow::Owned(parse_declarations("white-space: nowrap;")),
    })
}

fn resolve_relative_font_weight(styles: &mut ComputedStyle, parent: &ComputedStyle) {
    let parent_weight = parent.font_weight.to_u16();
    styles.font_weight = styles.font_weight.resolve_relative(parent_weight);
}

fn propagate_text_decorations(styles: &mut ComputedStyle, parent: &ComputedStyle) {
    styles.applied_text_decorations = parent.applied_text_decorations.clone();
    if styles.text_decoration_line_specified && styles.text_decoration.lines.is_empty() {
        styles.applied_text_decorations.clear();
    }
    if !styles.text_decoration.lines.is_empty() {
        styles
            .applied_text_decorations
            .push(crate::style::types::ResolvedTextDecoration {
                decoration: styles.text_decoration.clone(),
                skip_ink: styles.text_decoration_skip_ink,
                underline_offset: styles.text_underline_offset,
                underline_position: styles.text_underline_position,
            });
    }
}

/// Build an ElementRef with ancestor context
fn build_element_ref_chain<'a>(node: &'a DomNode, ancestors: &'a [&'a DomNode]) -> ElementRef<'a> {
    if ancestors.is_empty() {
        return ElementRef::new(node);
    }

    // Create ElementRef with all ancestors
    ElementRef::with_ancestors(node, ancestors)
}

/// Compute styles for a pseudo-element (::before or ::after)
///
/// Returns Some(ComputedStyle) if the pseudo-element should be generated
/// (i.e., has content property set to something other than 'none' or 'normal')
fn compute_pseudo_element_styles(
    node: &DomNode,
    rules: &RuleIndex<'_>,
    selector_caches: &mut SelectorCaches,
    ancestors: &[&DomNode],
    parent_styles: &ComputedStyle,
    ua_parent_styles: &ComputedStyle,
    root_font_size: f32,
    ua_root_font_size: f32,
    viewport: Size,
    pseudo: &PseudoElement,
) -> Option<ComputedStyle> {
    if !rules.has_pseudo_rules(pseudo) {
        return None;
    }
    // Find rules matching this pseudo-element
    let matching_rules = find_pseudo_element_rules(node, rules, selector_caches, ancestors, pseudo);

    if matching_rules.is_empty() {
        return None;
    }

    let ua_matches: Vec<_> = matching_rules
        .iter()
        .filter(|r| r.origin == StyleOrigin::UserAgent)
        .cloned()
        .collect();

    let mut ua_styles = ComputedStyle::default();
    ua_styles.display = Display::Inline;
    inherit_styles(&mut ua_styles, ua_parent_styles);
    apply_cascaded_declarations(
        &mut ua_styles,
        ua_matches,
        None,
        ua_parent_styles,
        ua_parent_styles.font_size,
        ua_root_font_size,
        viewport,
        &ComputedStyle::default(),
        |_| true,
    );
    resolve_match_parent_text_align(&mut ua_styles, ua_parent_styles, false);
    resolve_match_parent_text_align_last(&mut ua_styles, ua_parent_styles, false);
    resolve_relative_font_weight(&mut ua_styles, ua_parent_styles);
    propagate_text_decorations(&mut ua_styles, ua_parent_styles);
    ua_styles.root_font_size = ua_root_font_size;
    resolve_line_height_length(&mut ua_styles, viewport);
    resolve_absolute_lengths(&mut ua_styles, ua_root_font_size, viewport);

    // Start with default inline styles (pseudo-elements default to display: inline)
    let mut styles = ComputedStyle::default();
    styles.display = Display::Inline;

    // Inherit from parent element
    inherit_styles(&mut styles, parent_styles);

    // Apply matching declarations
    apply_cascaded_declarations(
        &mut styles,
        matching_rules,
        None,
        parent_styles,
        parent_styles.font_size,
        root_font_size,
        viewport,
        &ua_styles,
        |_| true,
    );
    resolve_match_parent_text_align(&mut styles, parent_styles, false);
    resolve_match_parent_text_align_last(&mut styles, parent_styles, false);
    resolve_relative_font_weight(&mut styles, parent_styles);
    propagate_text_decorations(&mut styles, parent_styles);
    styles.root_font_size = root_font_size;
    resolve_line_height_length(&mut styles, viewport);
    resolve_absolute_lengths(&mut styles, root_font_size, viewport);

    // Check if content property generates content
    // Per CSS spec, ::before/::after only generate boxes if content is not 'none' or 'normal'
    if matches!(
        styles.content_value,
        crate::style::content::ContentValue::None | crate::style::content::ContentValue::Normal
    ) {
        return None;
    }

    Some(styles)
}

fn compute_marker_styles(
    node: &DomNode,
    rules: &RuleIndex<'_>,
    selector_caches: &mut SelectorCaches,
    ancestors: &[&DomNode],
    list_item_styles: &ComputedStyle,
    ua_list_item_styles: &ComputedStyle,
    root_font_size: f32,
    ua_root_font_size: f32,
    viewport: Size,
) -> Option<ComputedStyle> {
    if list_item_styles.display != Display::ListItem {
        return None;
    }

    let matching_rules = if rules.has_pseudo_rules(&PseudoElement::Marker) {
        find_pseudo_element_rules(node, rules, selector_caches, ancestors, &PseudoElement::Marker)
    } else {
        Vec::new()
    };
    let ua_matches: Vec<_> = matching_rules
        .iter()
        .filter(|r| r.origin == StyleOrigin::UserAgent)
        .cloned()
        .collect();

    let mut ua_styles = ComputedStyle::default();
    ua_styles.display = Display::Inline;
    inherit_styles(&mut ua_styles, ua_list_item_styles);

    // UA defaults per CSS Lists 3: isolate, tabular numbers, pre white-space, no transforms.
    ua_styles.unicode_bidi = crate::style::types::UnicodeBidi::Isolate;
    ua_styles.font_variant_numeric.spacing = crate::style::types::NumericSpacing::Tabular;
    ua_styles.white_space = crate::style::types::WhiteSpace::Pre;
    ua_styles.text_transform = crate::style::types::TextTransform::none();

    apply_cascaded_declarations(
        &mut ua_styles,
        ua_matches,
        None,
        ua_list_item_styles,
        ua_list_item_styles.font_size,
        ua_root_font_size,
        viewport,
        &ComputedStyle::default(),
        |_| true,
    );
    resolve_match_parent_text_align(&mut ua_styles, ua_list_item_styles, false);
    resolve_match_parent_text_align_last(&mut ua_styles, ua_list_item_styles, false);
    resolve_relative_font_weight(&mut ua_styles, ua_list_item_styles);
    propagate_text_decorations(&mut ua_styles, ua_list_item_styles);
    ua_styles.root_font_size = ua_root_font_size;
    resolve_line_height_length(&mut ua_styles, viewport);
    resolve_absolute_lengths(&mut ua_styles, ua_root_font_size, viewport);
    reset_marker_box_properties(&mut ua_styles);
    ua_styles.display = Display::Inline;

    let mut styles = ComputedStyle::default();
    styles.display = Display::Inline;
    inherit_styles(&mut styles, list_item_styles);

    // UA defaults per CSS Lists 3: isolate, tabular numbers, pre white-space, no transforms.
    styles.unicode_bidi = crate::style::types::UnicodeBidi::Isolate;
    styles.font_variant_numeric.spacing = crate::style::types::NumericSpacing::Tabular;
    styles.white_space = crate::style::types::WhiteSpace::Pre;
    styles.text_transform = crate::style::types::TextTransform::none();

    apply_cascaded_declarations(
        &mut styles,
        matching_rules,
        None,
        list_item_styles,
        list_item_styles.font_size,
        root_font_size,
        viewport,
        &ua_styles,
        |decl| marker_allows_property(&decl.property),
    );
    resolve_match_parent_text_align(&mut styles, list_item_styles, false);
    resolve_match_parent_text_align_last(&mut styles, list_item_styles, false);
    resolve_relative_font_weight(&mut styles, list_item_styles);
    propagate_text_decorations(&mut styles, list_item_styles);
    styles.root_font_size = root_font_size;
    resolve_line_height_length(&mut styles, viewport);
    resolve_absolute_lengths(&mut styles, root_font_size, viewport);

    reset_marker_box_properties(&mut styles);
    styles.display = Display::Inline;
    Some(styles)
}

pub(crate) fn reset_marker_box_properties(styles: &mut ComputedStyle) {
    let defaults = ComputedStyle::default();
    styles.position = defaults.position;
    styles.top = None;
    styles.right = None;
    styles.bottom = None;
    styles.left = None;
    styles.z_index = defaults.z_index;

    styles.width = None;
    styles.height = None;
    styles.min_width = None;
    styles.min_height = None;
    styles.max_width = None;
    styles.max_height = None;

    styles.margin_top = defaults.margin_top;
    styles.margin_right = defaults.margin_right;
    styles.margin_bottom = defaults.margin_bottom;
    styles.margin_left = defaults.margin_left;

    styles.padding_top = defaults.padding_top;
    styles.padding_right = defaults.padding_right;
    styles.padding_bottom = defaults.padding_bottom;
    styles.padding_left = defaults.padding_left;

    styles.border_top_width = defaults.border_top_width;
    styles.border_right_width = defaults.border_right_width;
    styles.border_bottom_width = defaults.border_bottom_width;
    styles.border_left_width = defaults.border_left_width;

    styles.border_top_style = defaults.border_top_style;
    styles.border_right_style = defaults.border_right_style;
    styles.border_bottom_style = defaults.border_bottom_style;
    styles.border_left_style = defaults.border_left_style;

    styles.border_top_color = defaults.border_top_color;
    styles.border_right_color = defaults.border_right_color;
    styles.border_bottom_color = defaults.border_bottom_color;
    styles.border_left_color = defaults.border_left_color;

    styles.border_top_left_radius = defaults.border_top_left_radius;
    styles.border_top_right_radius = defaults.border_top_right_radius;
    styles.border_bottom_left_radius = defaults.border_bottom_left_radius;
    styles.border_bottom_right_radius = defaults.border_bottom_right_radius;

    styles.background_color = defaults.background_color;
    styles.background_images = defaults.background_images.clone();
    styles.background_positions = defaults.background_positions.clone();
    styles.background_sizes = defaults.background_sizes.clone();
    styles.background_repeats = defaults.background_repeats.clone();
    styles.background_attachments = defaults.background_attachments.clone();
    styles.background_origins = defaults.background_origins.clone();
    styles.background_clips = defaults.background_clips.clone();
    styles.rebuild_background_layers();
    styles.object_fit = defaults.object_fit;
    styles.object_position = defaults.object_position.clone();

    styles.box_shadow.clear();
    styles.filter.clear();
    styles.backdrop_filter.clear();
    styles.clip_path = defaults.clip_path.clone();
    styles.mix_blend_mode = defaults.mix_blend_mode;
    styles.isolation = defaults.isolation;
    styles.transform.clear();
    styles.transform_box = defaults.transform_box;
    styles.transform_origin = defaults.transform_origin.clone();
    styles.overflow_x = defaults.overflow_x;
    styles.overflow_y = defaults.overflow_y;
    styles.opacity = defaults.opacity;
    // Markers should not carry table/layout-specific state
    styles.border_spacing_horizontal = defaults.border_spacing_horizontal;
    styles.border_spacing_vertical = defaults.border_spacing_vertical;
    styles.border_collapse = defaults.border_collapse;
    styles.table_layout = defaults.table_layout;
    styles.caption_side = defaults.caption_side;
    styles.empty_cells = defaults.empty_cells;

    // Text alignment/indentation properties do not apply to ::marker (CSS Pseudo/Lists).
    styles.text_align = defaults.text_align;
    styles.text_align_last = defaults.text_align_last;
    styles.text_justify = defaults.text_justify;
    styles.text_indent = defaults.text_indent;
    styles.text_overflow = defaults.text_overflow;
}

fn marker_allows_property(property: &str) -> bool {
    let p = property.to_ascii_lowercase();

    // Marker boxes honor a limited set of box-level properties
    // (CSS Lists 3 § 3.1.1).
    if matches!(
        p.as_str(),
        "content" | "direction" | "unicode-bidi" | "text-combine-upright"
    ) {
        return true;
    }

    // Cursor is explicitly allowed for ::marker in the CSS Pseudo tests.
    if p == "cursor" {
        return true;
    }

    // Font shorthand is allowed; individual font-* longhands are covered below.
    if p == "font" {
        return true;
    }

    // Animations/transitions are explicitly permitted.
    if p.starts_with("animation") || p.starts_with("transition") {
        return true;
    }

    // Inheritable text-affecting properties apply to the marker's contents.
    if p.starts_with("font-") {
        return true;
    }

    matches!(
        p.as_str(),
        "color"
            | "white-space"
            | "line-height"
            | "letter-spacing"
            | "word-spacing"
            | "text-transform"
            | "text-emphasis"
            | "text-emphasis-style"
            | "text-emphasis-color"
            | "text-emphasis-position"
            | "text-decoration"
            | "text-decoration-line"
            | "text-decoration-style"
            | "text-decoration-color"
            | "text-decoration-thickness"
            | "text-decoration-skip-ink"
            | "text-underline-offset"
            | "text-underline-position"
            | "text-shadow"
            | "line-break"
            | "word-break"
            | "overflow-wrap"
            | "hyphens"
            | "tab-size"
    )
}
