use crate::debug::runtime;
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
  pub flex_layout_cache_hit: CloneSiteStats,
  pub flex_measure_reuse: CloneSiteStats,
  pub grid_measure_reuse: CloneSiteStats,
}

#[derive(Clone, Copy)]
pub enum CloneSite {
  FlexLayoutCacheHit,
  FlexMeasureReuse,
  GridMeasureReuse,
}

impl CloneSite {
  const ALL: [CloneSite; 3] = [
    CloneSite::FlexLayoutCacheHit,
    CloneSite::FlexMeasureReuse,
    CloneSite::GridMeasureReuse,
  ];

  fn name(self) -> &'static str {
    match self {
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
  let total_events = stats.flex_layout_cache_hit.events
    + stats.flex_measure_reuse.events
    + stats.grid_measure_reuse.events;
  let total_nodes = stats.flex_layout_cache_hit.nodes
    + stats.flex_measure_reuse.nodes
    + stats.grid_measure_reuse.nodes;
  let total_texts = stats.flex_layout_cache_hit.text_fragments
    + stats.flex_measure_reuse.text_fragments
    + stats.grid_measure_reuse.text_fragments;
  let total_text_bytes = stats.flex_layout_cache_hit.text_bytes
    + stats.flex_measure_reuse.text_bytes
    + stats.grid_measure_reuse.text_bytes;
  let total_shaped = stats.flex_layout_cache_hit.shaped_texts
    + stats.flex_measure_reuse.shaped_texts
    + stats.grid_measure_reuse.shaped_texts;

  let site_parts = [
    (CloneSite::FlexLayoutCacheHit, stats.flex_layout_cache_hit),
    (CloneSite::FlexMeasureReuse, stats.flex_measure_reuse),
    (CloneSite::GridMeasureReuse, stats.grid_measure_reuse),
  ]
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
