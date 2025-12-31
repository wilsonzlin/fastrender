# Instrumentation & logging (how to stay systematic)

FastRender has three layers of “what’s going on?” tooling:

1. **Structured diagnostics** (`RenderDiagnostics.stats`) — per-stage timings and counts.
2. **Trace timelines** (Chrome trace JSON) — spans across parse/style/layout/paint.
3. **Targeted logs** (`FASTR_*` env vars) — scoped, opt-in debug output.

The goal is to make performance and correctness work **measurable and attributable**.

## “Use the standard stuff” (metrics + tracing)

We already ship lightweight, purpose-built mechanisms (`RenderDiagnostics.stats` and Chrome-trace JSON).
For new instrumentation/logging, **strongly consider** using:

- `tracing` (events + spans) for structured logs and scoped timing
- `metrics` for counters/histograms/gauges when you want aggregations

If you add these, wire them so they’re easy to turn on/off and don’t spam by default (env-gated).

## 1) Structured diagnostics (timings + counts)

Most CLI tooling can capture `RenderDiagnostics.stats` by enabling a diagnostics level:

- `basic`: stage timings + high-level counters
- `verbose`: more expensive details (profiling stats, extra counts)

`pageset_progress` writes a stage-bucket summary into `progress/pages/<stem>.json`:

- `fetch`: html decode + DOM parse
- `css`: stylesheet inlining + css parse
- `cascade`: selector matching + box tree generation
- `layout`: layout engine
- `paint`: display-list build/optimize + rasterize (verbose captures `paint.gradient_ms` +
  `paint.gradient_pixels` for gradient-heavy pages)

When diagnostics are enabled, `RenderDiagnostics.stats.resources` also captures lightweight
resource/cache counters to explain slow fetch-heavy pagesets (e.g. cache misses vs revalidation,
disk cache reads, single-flight inflight waits, and total network fetch time). These are surfaced in
`pageset_progress report --verbose-stats` and stored under `diagnostics.stats` in the per-page JSON
when present.

Common fields in `stats.resources`:
- `resource_cache_*` (hits/misses/bytes when serving from the in-process cache)
- `fetch_inflight_wait*` (time/count waiting on single-flight in-flight fetches)
- `disk_cache_*` (disk-backed cache hits/misses/bytes/time + lock wait breakdown)
- `network_fetch*` (HTTP fetch count/bytes/time for actual network work)

If you need the full detail, check the per-page log (`target/pageset/logs/<stem>.log`) or rerun with `--diagnostics verbose`.

## 2) Tracing (timelines you can “see”)

FastRender can write Chrome trace JSON which opens in `chrome://tracing` or [Perfetto UI](https://ui.perfetto.dev).

Ways to produce traces:

- `pageset_progress --trace-failures` or `--trace-slow-ms <ms>` to generate traces under `target/pageset/traces/`.
- `FASTR_TRACE_OUT=/tmp/trace.json` with `render_pages` / `fetch_and_render` (see `docs/perf-logging.md`).

### Adding new spans

The pipeline already emits high-level spans. When you find a hotspot, add a tighter span around the suspect code:

- Name spans after **what** the code does (not the function name).
- Put them in the right category: `"parse"`, `"style"`, `"layout"`, `"paint"`, `"resource"`.
- Keep spans cheap: don’t allocate in the name unless you must.

## 3) Deadlines (timeouts that actually work)

`RenderOptions::timeout` uses a cooperative deadline system: hot loops must **check** for timeouts.

When you introduce or touch a loop that can scale with DOM size, make it deadline-aware by sprinkling:

```rust
// In a hot loop:
fastrender::render_control::check_active_periodic(&mut counter, 1024, RenderStage::Layout)?;
```

(`check_active_periodic` was added specifically to make this low-friction.)

Rule of thumb:
- `stride = 1024` (or higher) for very hot loops
- smaller stride when work per iteration is heavy

## 4) Logging (be loud when it helps)

Logging is intentionally controlled via `FASTR_*` env vars so normal runs stay clean.

Before adding a new ad-hoc `eprintln!`, check `docs/env-vars.md` — many toggles already exist.

When adding new logs:
- Gate them behind a clearly named toggle (`FASTR_LOG_<SUBSYSTEM>=1`).
- Make the log line actionable (ids, counts, sizes, elapsed ms).
- Prefer **structured counters/timings** when possible; logs are for explaining *why*.

## Suggested workflow for an agent

1. Update scoreboard: run `pageset_progress`.
2. Pick the worst page (timeout/panic/slowest).
3. Use stage buckets to decide where to dig.
4. Get a CPU profile (`samply`/`perf`) or a trace.
5. Fix root cause + add a regression + keep deadlines alive in loops.
