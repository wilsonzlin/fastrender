use crate::debug::runtime;
use crate::tree::fragment_tree::FragmentNode;
use std::sync::atomic::{AtomicU64, Ordering};

#[derive(Default, Clone, Copy)]
pub struct CloneStats {
  pub nodes: u64,
  pub text_fragments: u64,
  pub text_bytes: u64,
  pub shaped_texts: u64,
}

#[derive(Default, Clone, Copy)]
pub struct CloneSiteStats {
  pub events: u64,
  pub nodes: u64,
  pub text_fragments: u64,
  pub text_bytes: u64,
  pub shaped_texts: u64,
}

#[derive(Default, Clone, Copy)]
pub struct FragmentCloneProfileStats {
  pub layout_cache_hit: CloneSiteStats,
  pub flex_layout_cache_hit: CloneSiteStats,
  pub flex_measure_reuse: CloneSiteStats,
  pub grid_measure_reuse: CloneSiteStats,
}

#[derive(Clone, Copy)]
pub enum CloneSite {
  LayoutCacheHit,
  FlexLayoutCacheHit,
  FlexMeasureReuse,
  GridMeasureReuse,
}

impl CloneSite {
  const ALL: [CloneSite; 4] = [
    CloneSite::LayoutCacheHit,
    CloneSite::FlexLayoutCacheHit,
    CloneSite::FlexMeasureReuse,
    CloneSite::GridMeasureReuse,
  ];

  fn name(self) -> &'static str {
    match self {
      CloneSite::LayoutCacheHit => "layout_cache_hit",
      CloneSite::FlexLayoutCacheHit => "flex_layout_cache_hit",
      CloneSite::FlexMeasureReuse => "flex_measure_reuse",
      CloneSite::GridMeasureReuse => "grid_measure_reuse",
    }
  }
}

struct Counters {
  events: AtomicU64,
  nodes: AtomicU64,
  text_fragments: AtomicU64,
  text_bytes: AtomicU64,
  shaped_texts: AtomicU64,
}

impl Counters {
  const fn new() -> Self {
    Self {
      events: AtomicU64::new(0),
      nodes: AtomicU64::new(0),
      text_fragments: AtomicU64::new(0),
      text_bytes: AtomicU64::new(0),
      shaped_texts: AtomicU64::new(0),
    }
  }

  fn snapshot(&self) -> CloneSiteStats {
    CloneSiteStats {
      events: self.events.load(Ordering::Relaxed),
      nodes: self.nodes.load(Ordering::Relaxed),
      text_fragments: self.text_fragments.load(Ordering::Relaxed),
      text_bytes: self.text_bytes.load(Ordering::Relaxed),
      shaped_texts: self.shaped_texts.load(Ordering::Relaxed),
    }
  }

  fn reset(&self) {
    self.events.store(0, Ordering::Relaxed);
    self.nodes.store(0, Ordering::Relaxed);
    self.text_fragments.store(0, Ordering::Relaxed);
    self.text_bytes.store(0, Ordering::Relaxed);
    self.shaped_texts.store(0, Ordering::Relaxed);
  }
}

static LAYOUT_CACHE_COUNTERS: Counters = Counters::new();
static FLEX_LAYOUT_COUNTERS: Counters = Counters::new();
static FLEX_MEASURE_COUNTERS: Counters = Counters::new();
static GRID_MEASURE_COUNTERS: Counters = Counters::new();

fn enabled() -> bool {
  runtime::runtime_toggles().truthy("FASTR_PROFILE_FRAGMENT_CLONES")
}

pub fn fragment_clone_profile_enabled() -> bool {
  enabled()
}

fn counters_for_site(site: CloneSite) -> &'static Counters {
  match site {
    CloneSite::LayoutCacheHit => &LAYOUT_CACHE_COUNTERS,
    CloneSite::FlexLayoutCacheHit => &FLEX_LAYOUT_COUNTERS,
    CloneSite::FlexMeasureReuse => &FLEX_MEASURE_COUNTERS,
    CloneSite::GridMeasureReuse => &GRID_MEASURE_COUNTERS,
  }
}

pub fn reset_fragment_clone_profile() {
  if !enabled() {
    return;
  }
  for site in CloneSite::ALL {
    counters_for_site(site).reset();
  }
}

fn count_fragment_stats(fragment: &FragmentNode, stats: &mut CloneStats) {
  stats.nodes += 1;
  match &fragment.content {
    crate::tree::fragment_tree::FragmentContent::Text { text, shaped, .. } => {
      stats.text_fragments += 1;
      stats.text_bytes += text.len() as u64;
      if shaped.is_some() {
        stats.shaped_texts += 1;
      }
    }
    crate::tree::fragment_tree::FragmentContent::RunningAnchor { snapshot, .. } => {
      count_fragment_stats(snapshot, stats);
    }
    _ => {}
  }
  for child in &fragment.children {
    count_fragment_stats(child, stats);
  }
}

pub fn collect_fragment_clone_stats(fragment: &FragmentNode) -> CloneStats {
  let mut stats = CloneStats::default();
  count_fragment_stats(fragment, &mut stats);
  stats
}

pub fn record_fragment_clone_from_fragment(site: CloneSite, fragment: &FragmentNode) {
  if !enabled() {
    return;
  }
  let stats = collect_fragment_clone_stats(fragment);
  record_fragment_clone(site, &stats);
}

pub fn record_fragment_clone(site: CloneSite, stats: &CloneStats) {
  if !enabled() {
    return;
  }
  let counters = counters_for_site(site);
  counters.events.fetch_add(1, Ordering::Relaxed);
  counters.nodes.fetch_add(stats.nodes, Ordering::Relaxed);
  counters
    .text_fragments
    .fetch_add(stats.text_fragments, Ordering::Relaxed);
  counters
    .text_bytes
    .fetch_add(stats.text_bytes, Ordering::Relaxed);
  counters
    .shaped_texts
    .fetch_add(stats.shaped_texts, Ordering::Relaxed);
}

pub fn record_fragment_reuse_without_clone(site: CloneSite) {
  if !enabled() {
    return;
  }
  counters_for_site(site)
    .events
    .fetch_add(1, Ordering::Relaxed);
}

pub fn fragment_clone_profile_stats() -> FragmentCloneProfileStats {
  if !enabled() {
    return FragmentCloneProfileStats::default();
  }
  FragmentCloneProfileStats {
    layout_cache_hit: counters_for_site(CloneSite::LayoutCacheHit).snapshot(),
    flex_layout_cache_hit: counters_for_site(CloneSite::FlexLayoutCacheHit).snapshot(),
    flex_measure_reuse: counters_for_site(CloneSite::FlexMeasureReuse).snapshot(),
    grid_measure_reuse: counters_for_site(CloneSite::GridMeasureReuse).snapshot(),
  }
}

pub fn log_fragment_clone_profile() {
  if !enabled() {
    return;
  }
  let stats = fragment_clone_profile_stats();
  let sites = [
    (CloneSite::LayoutCacheHit, stats.layout_cache_hit),
    (CloneSite::FlexLayoutCacheHit, stats.flex_layout_cache_hit),
    (CloneSite::FlexMeasureReuse, stats.flex_measure_reuse),
    (CloneSite::GridMeasureReuse, stats.grid_measure_reuse),
  ];
  let total_events = sites.iter().map(|(_, s)| s.events).sum::<u64>();
  let total_nodes = sites.iter().map(|(_, s)| s.nodes).sum::<u64>();
  let total_texts = sites.iter().map(|(_, s)| s.text_fragments).sum::<u64>();
  let total_text_bytes = sites.iter().map(|(_, s)| s.text_bytes).sum::<u64>();
  let total_shaped = sites.iter().map(|(_, s)| s.shaped_texts).sum::<u64>();

  let site_parts = sites
    .iter()
    .map(|(site, s)| {
      format!(
        "{}: events={} nodes={} text_frags={} text_bytes={} shaped_texts={}",
        site.name(),
        s.events,
        s.nodes,
        s.text_fragments,
        s.text_bytes,
        s.shaped_texts
      )
    })
    .collect::<Vec<_>>()
    .join(" | ");

  eprintln!(
    "fragment clone profile (FASTR_PROFILE_FRAGMENT_CLONES=1): total_events={} total_nodes={} total_text_frags={} total_text_bytes={} shaped_texts={} | {}",
    total_events, total_nodes, total_texts, total_text_bytes, total_shaped, site_parts
  );
}
