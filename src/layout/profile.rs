use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::OnceLock;
use std::time::{Duration, Instant};

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

static ENABLED: OnceLock<bool> = OnceLock::new();
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
    *ENABLED.get_or_init(|| std::env::var("FASTR_LAYOUT_PROFILE").map(|v| v != "0").unwrap_or(false))
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

pub fn log_layout_profile(total: Duration) {
    if !enabled() {
        return;
    }
    let total_ms = total.as_secs_f64() * 1000.0;
    eprintln!("layout profile: total_ms={:.2} (FASTR_LAYOUT_PROFILE=1)", total_ms);
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
    eprintln!(
        "layout profile (inclusive): total_ms={:.2} {}",
        total_ms,
        parts.join(" ")
    );
}
