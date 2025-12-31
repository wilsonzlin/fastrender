use crate::debug::runtime;
use crate::layout::float_context::{float_profile_stats, reset_float_profile_counters};
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;
use std::time::Duration;
use std::time::Instant;

#[derive(Copy, Clone, Debug, Default)]
pub struct LayoutProfileSnapshot {
  pub block_ms: f64,
  pub block_calls: u64,
  pub inline_ms: f64,
  pub inline_calls: u64,
  pub flex_ms: f64,
  pub flex_calls: u64,
  pub grid_ms: f64,
  pub grid_calls: u64,
  pub table_ms: f64,
  pub table_calls: u64,
  pub absolute_ms: f64,
  pub absolute_calls: u64,
}

impl std::ops::AddAssign for LayoutProfileSnapshot {
  fn add_assign(&mut self, other: Self) {
    self.block_ms += other.block_ms;
    self.block_calls += other.block_calls;
    self.inline_ms += other.inline_ms;
    self.inline_calls += other.inline_calls;
    self.flex_ms += other.flex_ms;
    self.flex_calls += other.flex_calls;
    self.grid_ms += other.grid_ms;
    self.grid_calls += other.grid_calls;
    self.table_ms += other.table_ms;
    self.table_calls += other.table_calls;
    self.absolute_ms += other.absolute_ms;
    self.absolute_calls += other.absolute_calls;
  }
}

#[derive(Copy, Clone, Debug)]
pub enum LayoutKind {
  Block,
  Inline,
  Flex,
  Grid,
  Table,
  Absolute,
}

impl LayoutKind {
  fn as_usize(self) -> usize {
    match self {
      LayoutKind::Block => 0,
      LayoutKind::Inline => 1,
      LayoutKind::Flex => 2,
      LayoutKind::Grid => 3,
      LayoutKind::Table => 4,
      LayoutKind::Absolute => 5,
    }
  }

  fn name(self) -> &'static str {
    match self {
      LayoutKind::Block => "block",
      LayoutKind::Inline => "inline",
      LayoutKind::Flex => "flex",
      LayoutKind::Grid => "grid",
      LayoutKind::Table => "table",
      LayoutKind::Absolute => "absolute",
    }
  }
}

const KIND_COUNT: usize = 6;

static TIME_NS: [AtomicU64; KIND_COUNT] = [
  AtomicU64::new(0),
  AtomicU64::new(0),
  AtomicU64::new(0),
  AtomicU64::new(0),
  AtomicU64::new(0),
  AtomicU64::new(0),
];
static CALLS: [AtomicU64; KIND_COUNT] = [
  AtomicU64::new(0),
  AtomicU64::new(0),
  AtomicU64::new(0),
  AtomicU64::new(0),
  AtomicU64::new(0),
  AtomicU64::new(0),
];

fn enabled() -> bool {
  runtime::runtime_toggles().truthy("FASTR_LAYOUT_PROFILE")
}

pub fn layout_profile_enabled() -> bool {
  enabled()
}

pub fn reset_layout_profile() {
  for entry in TIME_NS.iter() {
    entry.store(0, Ordering::Relaxed);
  }
  for entry in CALLS.iter() {
    entry.store(0, Ordering::Relaxed);
  }
  reset_float_profile_counters();
}

pub fn start_timer(kind: LayoutKind) -> Option<(LayoutKind, Instant)> {
  if enabled() {
    Some((kind, Instant::now()))
  } else {
    None
  }
}

pub fn end_timer(timer: Option<(LayoutKind, Instant)>) {
  if let Some((kind, start)) = timer {
    let idx = kind.as_usize();
    let elapsed = start.elapsed();
    TIME_NS[idx].fetch_add(elapsed.as_nanos() as u64, Ordering::Relaxed);
    CALLS[idx].fetch_add(1, Ordering::Relaxed);
  }
}

pub struct LayoutTimerGuard(Option<(LayoutKind, Instant)>);

impl Drop for LayoutTimerGuard {
  fn drop(&mut self) {
    end_timer(self.0.take());
  }
}

pub fn layout_timer(kind: LayoutKind) -> LayoutTimerGuard {
  LayoutTimerGuard(start_timer(kind))
}

pub fn layout_profile_snapshot() -> LayoutProfileSnapshot {
  if !enabled() {
    return LayoutProfileSnapshot::default();
  }
  LayoutProfileSnapshot {
    block_ms: TIME_NS[LayoutKind::Block.as_usize()].load(Ordering::Relaxed) as f64 / 1_000_000.0,
    block_calls: CALLS[LayoutKind::Block.as_usize()].load(Ordering::Relaxed),
    inline_ms: TIME_NS[LayoutKind::Inline.as_usize()].load(Ordering::Relaxed) as f64 / 1_000_000.0,
    inline_calls: CALLS[LayoutKind::Inline.as_usize()].load(Ordering::Relaxed),
    flex_ms: TIME_NS[LayoutKind::Flex.as_usize()].load(Ordering::Relaxed) as f64 / 1_000_000.0,
    flex_calls: CALLS[LayoutKind::Flex.as_usize()].load(Ordering::Relaxed),
    grid_ms: TIME_NS[LayoutKind::Grid.as_usize()].load(Ordering::Relaxed) as f64 / 1_000_000.0,
    grid_calls: CALLS[LayoutKind::Grid.as_usize()].load(Ordering::Relaxed),
    table_ms: TIME_NS[LayoutKind::Table.as_usize()].load(Ordering::Relaxed) as f64 / 1_000_000.0,
    table_calls: CALLS[LayoutKind::Table.as_usize()].load(Ordering::Relaxed),
    absolute_ms: TIME_NS[LayoutKind::Absolute.as_usize()].load(Ordering::Relaxed) as f64
      / 1_000_000.0,
    absolute_calls: CALLS[LayoutKind::Absolute.as_usize()].load(Ordering::Relaxed),
  }
}

pub fn log_layout_profile(total: Duration) {
  if !enabled() {
    return;
  }
  let total_ms = total.as_secs_f64() * 1000.0;
  eprintln!(
    "layout profile: total_ms={:.2} (FASTR_LAYOUT_PROFILE=1)",
    total_ms
  );
  let mut parts = Vec::new();
  for (kind_idx, kind) in [
    (0, LayoutKind::Block),
    (1, LayoutKind::Inline),
    (2, LayoutKind::Flex),
    (3, LayoutKind::Grid),
    (4, LayoutKind::Table),
    (5, LayoutKind::Absolute),
  ] {
    let time = TIME_NS[kind_idx].load(Ordering::Relaxed) as f64 / 1_000_000.0;
    let calls = CALLS[kind_idx].load(Ordering::Relaxed);
    if time > 0.0 || calls > 0 {
      parts.push(format!(
        "{}_ms={:.2} {}_calls={} {}_avg_ms={:.3}",
        kind.name(),
        time,
        kind.name(),
        calls,
        kind.name(),
        if calls > 0 { time / calls as f64 } else { 0.0 }
      ));
    }
  }
  let float_stats = float_profile_stats();
  if float_stats.width_queries > 0
    || float_stats.range_queries > 0
    || float_stats.boundary_steps > 0
  {
    parts.push(format!(
      "float_width_queries={} float_range_queries={} float_boundaries={}",
      float_stats.width_queries, float_stats.range_queries, float_stats.boundary_steps
    ));
  }
  eprintln!(
    "layout profile (inclusive): total_ms={:.2} {}",
    total_ms,
    parts.join(" ")
  );
}
