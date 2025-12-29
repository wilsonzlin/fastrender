# Profiling on Linux (FastRender playbook)

This doc is about **getting actionable profiles quickly** for FastRender’s Rust binaries on Linux.

## TL;DR (fast path)

- **Update pageset scoreboard** (safe timeouts/panic containment):

```bash
cargo run --release --bin pageset_progress -- run --timeout 5
```

- Convenience wrappers (see `scripts/`):
  - `scripts/profile_samply.sh <stem>`
  - `scripts/profile_perf.sh <stem>`
  - `scripts/samply_summary.py <profile.json.gz>` (terminal summary)

- **CPU profile (best UX)** with Samply (Firefox Profiler UI):

```bash
# Build a symbolized binary (release-like but profiled)
CARGO_PROFILE_RELEASE_DEBUG=1 CARGO_PROFILE_RELEASE_STRIP=none \
  RUSTFLAGS="-C force-frame-pointers=yes" \
  cargo build --release --bin pageset_progress

# Terminal-only friendly: save to file, don't auto-open a browser.
samply record --save-only --no-open -o target/pageset/profiles/example.profile.json.gz -- \
  target/release/pageset_progress run --jobs 1 --pages example.com --timeout 5

# View later (on a machine with a browser):
samply load target/pageset/profiles/example.profile.json.gz
```

## 0. Prereqs / gotchas

### Perf permissions

Both `perf` and `samply` rely on Linux perf events. On many distros you need to relax perf permissions:

```bash
cat /proc/sys/kernel/perf_event_paranoid
```

Common temporary setting (more permissive):

```bash
echo -1 | sudo tee /proc/sys/kernel/perf_event_paranoid
```

If you can’t change system settings, you can still use some tools, but call stacks and sampling may be limited.

### Symbols matter (this repo strips release builds)

`Cargo.toml` sets `[profile.release] strip = true`, which is great for shipping but **bad for profiling**.
Use env overrides while profiling:

```bash
CARGO_PROFILE_RELEASE_DEBUG=1 CARGO_PROFILE_RELEASE_STRIP=none cargo build --release
```

For better stacks with `perf --call-graph fp`, also force frame pointers:

```bash
RUSTFLAGS="-C force-frame-pointers=yes" cargo build --release
```

## 1. CPU profiling

### Option A: Samply (recommended for iteration)

Install:

```bash
cargo install --locked samply
```

Record a profile:

```bash
samply record --save-only --no-open -o target/pageset/profiles/example.profile.json.gz -- \
  target/release/pageset_progress run --jobs 1 --pages example.com --timeout 5
```

Open it later (on a machine with a browser):

```bash
samply load target/pageset/profiles/example.profile.json.gz
```

Samply uses the Firefox Profiler UI. Use it to find:
- **dominant stacks** (where time goes)
- **wide vs deep** hotspots (fan-out vs single heavy function)
- **allocator / parsing churn** (many small frames repeated)

### Option B: perf (baseline + lowest-level control)

Quick counters:

```bash
perf stat -d -d -- target/release/pageset_progress run --jobs 1 --pages example.com --timeout 5
```

Sampled CPU profile (DWARF call stacks):

```bash
perf record -F 99 --call-graph dwarf -- \
  target/release/pageset_progress run --jobs 1 --pages example.com --timeout 5
perf report
```

Notes:
- `--call-graph dwarf` has overhead; for longer runs prefer frame pointers (`--call-graph fp`) + `-C force-frame-pointers=yes`.
- For “what’s hot” triage, `perf stat` + `perf report` is often enough.

### Option C: cargo-flamegraph (quick flamegraph PNG/SVG)

Install:

```bash
cargo install cargo-flamegraph
```

Run (builds + records + generates a flamegraph):

```bash
CARGO_PROFILE_RELEASE_DEBUG=1 CARGO_PROFILE_RELEASE_STRIP=none \
  cargo flamegraph --bin pageset_progress -- run --jobs 1 --pages example.com --timeout 5
```

## 2. Trace-style profiling (pipeline spans)

FastRender can emit Chrome-trace JSON. This is great when you need **stage attribution** and a timeline.

- **One-off trace**: run a trace rerender via `pageset_progress`:

```bash
cargo run --release --bin pageset_progress -- run --timeout 5 --trace-failures
```

- **Inspect**: open the trace in either:
  - `chrome://tracing` (Chrome trace viewer)
  - [Perfetto UI](https://ui.perfetto.dev) (works with Chrome trace JSON too)

See also `docs/perf-logging.md` (`FASTR_TRACE_OUT`).

## 3. Memory / allocation profiling

When performance issues look like “lots of allocations / big peak RSS”, use:

- `heaptrack` (fast-ish, very actionable)
- `valgrind --tool=massif` (slow, but can show peak growth)

Example with heaptrack:

```bash
heaptrack target/release/pageset_progress run --jobs 1 --pages example.com --timeout 5
```

## 4. What to do with profiles (workflow)

1. **Reproduce**: isolate to one cached page stem (`--pages <stem>`).
2. **Attribute**: use `RenderDiagnostics.stats` (stage buckets) + trace spans.
3. **Confirm**: use `samply`/`perf` to identify exact hot functions.
4. **Fix root cause**: algorithm/data-structure, not clamps/skips.
5. **Re-measure**: update `progress/pages/<stem>.json` and rerun profiles if still hot.

