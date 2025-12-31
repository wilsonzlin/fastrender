# FastRender (agent guide)

FastRender is an internal HTML/CSS renderer in Rust. The goal is to render real pages into pixel-correct raster output while staying spec-faithful.

`docs/index.md` is the internal wiki entry point.

## Start here (copy/paste loops)

**Update the pageset scoreboard (fetch HTML → prefetch subresources → render → write `progress/pages/*.json`):**

```bash
scripts/pageset.sh
```

Or (same thing, via the preferred wrapper):

```bash
cargo xtask pageset
```

Pageset wrappers enable the disk-backed subresource cache (writes to `fetches/assets/`) by default for faster, repeatable runs. They also prefetch external stylesheets and font subresources into that cache before rendering so the 5s render budget isn't spent on first-run network fetches. Set `NO_DISK_CACHE=1` or `DISK_CACHE=0` (or pass `--no-disk-cache`) to fall back to in-memory fetch caching. Both wrappers default to bundled fonts to avoid slow or non-deterministic system font discovery.

Disk cache tuning knobs (useful for reproducibility when long-lived caches would otherwise age out):
- `FASTR_DISK_CACHE_MAX_AGE_SECS=0` pins cached subresources (never expire purely due to age).
- `FASTR_DISK_CACHE_MAX_BYTES=<bytes>` increases the on-disk cache eviction budget (0 disables eviction).

**Profile one page (terminal-only; writes a profile file + prints a summary):**

```bash
scripts/profile_samply.sh example.com --timeout 5
```

**Render artifacts / layout introspection:**

```bash
cargo run --release --bin inspect_frag -- --help
```

## North-star KPI (what we optimize for)

**The page set in `src/bin/fetch_pages.rs` is the KPI.**

Everything else is subordinate:
- If those pages don’t render, we are failing.
- If they render but take seconds/minutes, we are failing.
- If they regress, we are failing.

This project needs relentless focus: **PAGESET ACCURACY (pixel-correct output) + PERFORMANCE**.

### Planner scope (strict, for now)

For now, the planner’s universe is **ONLY** the pages listed in `src/bin/fetch_pages.rs`:
- **Only**: do not chase work that is not motivated by a pageset failure/slowdown.
- **Exactly**: do not add/remove pages or substitute “representative” pages. The list is the KPI.
- **All of them**: the goal is not to make *one* page great — it’s to make the *entire pageset* render accurately and fast.

You may temporarily focus on a single pageset page (or a minimized repro derived from one) to debug, but always return to full pageset runs and update `progress/pages/*.json`.

## Culture / mindset (read this twice)

- **Accuracy is the product**: our job is to make the pageset render *correct pixels* (layout, paint, text, images). **Performance and data are tools** to reach that goal faster, not goals we chase for their own sake.
- **Accuracy is correctness**: wrong/blank/missing output is a bug. Treat “it’s probably JS” as a hypothesis to disprove, not a conclusion.
- **Performance is correctness**: a renderer that times out or loops is wrong.
- **No vanity work**: changes that don’t improve pageset accuracy, eliminate a crash/timeout, or reduce uncertainty for an imminent fix are not acceptable. Instrumentation that never leads to a fix is waste.
- **Ruthless triage**: if you can’t turn a symptom into a task with a measurable outcome quickly, stop and split the work.
- **Accountability**: progress must be visible, comparable, and committed. Regressions must be obvious.
- **Worship data (in service of accuracy)**: we don’t “feel” performance or correctness — we measure it. Prefer evidence like `progress/pages/*.json` deltas, timings, traces, logs, and tests (unit/prop/fuzz/regressions). Use standard instrumentation when helpful (`tracing` spans/events, `metrics` counters/histograms). Data is only valuable if it helps us ship more correct pageset renders.
- **No shortcuts**: the hard budgets are NOT permission to ship hacks, workarounds, TODOs, partial implementations, or “close enough” behavior. They are pressure to **think deeply** and **grind** until the solution is both **correct and fast** (fix root causes; don’t paper over them).

## Hard budgets (non-negotiable)

- **Hard timeout**: every pageset render must finish in **< 5 seconds**. If it doesn’t, it’s a bug.
- **Target**: pages should render in **< 100ms** (longer-term goal, but we track the gap).
- **Panic boundaries**: the render runner must treat panics as P0 and record them as failures (no “it crashed” handwaving).

Budgets are boundaries, not a strategy. Do not “meet” the budget by skipping work, clamping away correctness, or degrading output. Meet the budget by fixing algorithms, data structures, and invariants.

## Core guardrails (still true)

- **Spec-first correctness**: implement CSS/HTML algorithms as written; “incomplete but correct” beats “complete but wrong”.
- **No JS**: this is not a browser and does not execute author JavaScript. **JS is not an excuse**: most “top sites” still ship meaningful HTML/CSS; treat missing content as a renderer bug until you have strong evidence it’s truly script-only.
- **No page-specific hacks by default**: if a compatibility behavior is needed for the pageset, it must be:
  - explicitly gated (e.g., `CompatProfile` / `DomCompatibilityMode`),
  - small and generic,
  - covered by regressions, and
  - documented under `docs/notes/`.
- **Pipeline is staged**: parse → style → box tree → layout → paint. Avoid post-layout pixel nudges.
- **Tables are native** (not flex/grid).
- **Taffy is flex/grid only** (vendored under `vendor/taffy/`).

## The operating model (how planners and workers should behave)

### Main planner: pageset-first triage loop

The main planner should **continuously**:
1. **Fetch all pages** (cache) using `fetch_pages`.
2. **Render all pages** with a **hard timeout of 5s** and panic containment.
3. **Record metrics** per page (see “Progress artifacts”).
4. **Triage quickly**, classify failures, and split clean work:
   - cascade too slow vs layout loop vs paint explosion vs resource decode churn vs fetch/CSS failures
5. Spawn subplanners/workers with **tight scopes and measurable goals**.

Do NOT assign “one worker per page” long-term. Pages overlap in root causes. Split by **failure class / hotspot**.

### Workers: what “done” means

A worker task is only “done” if it produces at least one of:
- A pageset page transitions **timeout → render** (under 5s)
- A pageset page gets **materially faster** (lower total_ms or dominant stage_ms)
- A **panic/crash** is eliminated (with regression)
- A correctness fix that causes an observable improvement (ideally captured by snapshots/fixtures) on the pageset

If you can’t show a measurable delta, you are not done. Stop, re-scope, or pick a different task.

## Progress artifacts (committed scoreboard)

We need anchors. We track the pageset in-repo so progress/regressions are visible and undeniable.

Run `pageset_progress` to auto-create/update **one file per page** (avoid hot-spot merge conflicts):
- `progress/pages/<normalized>.json`
- `<normalized>` should match the cache filename stem (use the same normalization as the CLIs: strip scheme + leading `www.`, sanitize via `normalize_page_name`). Colliding URLs append a deterministic `--deadbeef`-style suffix to keep cache/progress filenames unique. Refresh `fetches/html` if you still have stale `www.`-prefixed cache filenames.

### Minimal schema (keep it small and stable)

**Source of truth is code**: `src/bin/pageset_progress.rs` (`PageProgress`). The JSON below is documentation; the runner writes these files.

Each file should be tiny (no huge logs, no raw HTML):

```json
{
  "url": "https://example.com/",
  "status": "ok|timeout|panic|error",
  "total_ms": 123.4,
  "stages_ms": { "fetch": 0.0, "css": 0.0, "cascade": 0.0, "layout": 0.0, "paint": 0.0 },
  "notes": "short, durable explanation of current blocker",
  "hotspot": "cascade|layout|paint|fetch|decode|unknown",
  "failure_stage": "dom_parse|css|cascade|layout|paint|null",
  "timeout_stage": "dom_parse|css|cascade|layout|paint|null",
  "last_good_commit": "abcdef0",
  "last_regression_commit": "1234567"
}
```

When diagnostics are enabled, the runner may also attach `diagnostics.stats` (the structured `RenderStats` summary: timings, counts, cascade/layout/paint/resources) for successful renders so `pageset_progress report --verbose-stats` stays useful. Keep it tiny — no raw logs or traces.

Rules:
- Keep key ordering stable and formatting consistent.
- **Don’t hand-author these files**. They are written by tooling. If the schema needs to change, change the code first.
- If you must edit anything by hand, keep it to durable human fields like `notes` / `last_*` (the runner should preserve them).
- Don’t commit machine-local paths, enormous traces, or blobs.

`failure_stage` and `timeout_stage` expose the renderer’s structured diagnostics for triage
without scraping notes. They remain `null` in placeholders or when the renderer doesn’t report a
stage (e.g., hard-killed workers).

## How to spend time (priority order)

1. **Panics / crashes**
2. **Timeouts / loops** (must get under 5s)
3. **Pageset accuracy failures** (missing content, wrong layout/paint, unreadable text). Fix root causes and add regressions where possible.
4. **Big-stage hotspots** (cascade/layout/paint dominating) when they block pageset renders or iteration speed.
5. Spec expansion **only when it moves pageset accuracy/perf**

## Tooling (use this to stay systematic)

- Binaries: `fetch_pages`, `render_pages`, `fetch_and_render`, `pageset_progress` (see `docs/cli.md`)
- Scripts (terminal-friendly): `scripts/pageset.sh`, `scripts/profile_samply.sh`, `scripts/profile_perf.sh`, `scripts/samply_summary.py`
- Inspection: `inspect_frag`
- Profiling/debug: `docs/env-vars.md`, `docs/debugging.md`, `docs/perf-logging.md`, `docs/profiling-linux.md`, `docs/instrumentation.md`
- Prefer adding timers/counters that attribute time to stages and hotspots over “stare at PNGs”.

## Documentation policy (post-docs-cleanup)

- `docs/` is the curated wiki and must reflect repo reality.
- Don’t add scratchpads/status logs. Use the committed `progress/` scoreboard plus focused `docs/notes/*` writeups when a decision is durable.
