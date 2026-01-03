# FastRender (agent guide)

FastRender is an HTML/CSS renderer in Rust. Our mission is **spec-faithful, pixel-correct raster output** for real pages.

This guide is the “how we work” contract for agents. It is intentionally opinionated.

`docs/index.md` is the internal wiki entry point.

## The goal (90/10)

- **90%: accuracy + capability** — implement missing spec features and correct algorithms so the **pageset renders correctly**.
- **10%: performance + infra** — only as needed to **avoid timeouts/loops**, keep iteration fast, and keep evidence trustworthy.

Perf is not the product. Correct pixels are the product.

## Scope (strict)

**The only KPI is the page set in `src/bin/fetch_pages.rs`.**

- **Only** work motivated by a pageset failure/incorrectness.
- **Exactly** those pages (no substitutions).
- **All** of them (we’re building a renderer, not a demo).

## Definition of done (what “counts”)

A task only “counts” if it produces at least one of:

- **Accuracy**: a pageset page becomes more correct **with evidence** (see below).
- **Capability**: a missing feature is implemented in a spec-faithful way **because it blocks a pageset page**, and you add a regression (fixture/WPT/offline page) where possible.
- **Stability**: a panic/crash is eliminated (with a regression).
- **Termination**: a loop/timeout is fixed (must get under the 5s hard budget).

If you can’t show a measurable/evidenced delta, you are not done.

### Evidence requirement (accuracy work)

For accuracy tasks, evidence should usually be one of:

- **Offline repro + golden/regression** (preferred): a minimized fixture, imported page fixture, or WPT-style reftest with an updated/added expected image.
- **Chrome-vs-FastRender diff** on **offline fixtures** (preferred for early triage): a deterministic report under `target/` (see the fixture triage loop below).
- **Chrome-vs-FastRender diff** on the **same cached HTML/bundle** (acceptable best-effort triage): a report under `target/` that demonstrates improvement, but may be non-deterministic due to live subresources.

“Looks better” without artifacts is not evidence.

## Start here (copy/paste)

**Run the main pageset loop (fetch → prefetch → render → write `progress/pages/*.json`):**

```bash
scripts/pageset.sh
# or:
cargo xtask pageset
```

**Inspect the committed scoreboard:**

```bash
cargo run --release --bin pageset_progress -- report --top 15
cargo run --release --bin pageset_progress -- report --pages example.com
```

**Accuracy triage loop (compare against a correct engine):**

```bash
# Deterministic fixture evidence loop (preferred):
# - Renders are offline/repeatable (no network).
# - Uses bundled fonts so output is stable across machines.
# - Chrome baselines are local-only artifacts (not committed).
# - Defaults match the pageset viewport/DPR (1200x800 @ 1.0) unless you override them.
#
# 1) Render the fixture(s) with FastRender:
cargo run --release --bin render_fixtures
#
# 2) Generate Chrome baseline PNGs for the same fixture(s):
cargo xtask chrome-baseline-fixtures
#
# 3) Generate a combined Chrome-vs-FastRender diff report:
cargo xtask fixture-chrome-diff
#
# Cached-pages Chrome loop (best-effort; non-deterministic):
# Live subresources can change, so treat this as rapid triage only.
#
# Ubuntu one-time setup (python + fonts + chrome/chromium):
scripts/install_chrome_baseline_deps_ubuntu.sh

# Ensure cached HTML exists:
cargo run --release --bin fetch_pages

# Baseline screenshot from cached HTML (JS disabled by default):
scripts/chrome_baseline.sh example.com

# FastRender render for the same cached HTML:
cargo run --release --bin render_pages -- --pages example.com

# Diff:
cargo run --release --bin diff_renders -- \
  --before fetches/chrome_renders \
  --after fetches/renders \
  --html target/chrome_vs_fastrender.html
```

## Hard guardrails (still non‑negotiable)

- **No panics** in pageset runs (panic containment is not an excuse).
- **Hard timeout**: each pageset render must finish in **< 5 seconds**. If it doesn’t, it’s a bug.
- **No JS**: FastRender does not execute author JavaScript. “It’s probably JS” is a hypothesis to disprove, not a conclusion.
- **No page-specific hacks by default**. If compatibility behavior is needed, it must be:
  - explicitly gated (`CompatProfile` / `DomCompatibilityMode`),
  - generic/small,
  - covered by regressions,
  - documented under `docs/notes/`.
- **Spec-first**: implement CSS/HTML algorithms as written; “incomplete but correct” beats “complete but wrong”.
- **Pipeline stays staged**: parse → style → box tree → layout → paint. Avoid post-layout pixel nudges.
- **Tables are native**, Taffy is **flex/grid only** (vendored under `vendor/taffy/`).

## The accuracy workflow (how we make pages look right)

When a pageset page renders but is wrong:

1. **Reproduce from cache** (`fetches/html/<stem>.html`) with a fixed viewport/DPR.
2. **Get a correct-engine baseline** (Chrome screenshot from cached HTML).
3. **Render FastRender** for the same cached HTML.
4. **Diff** and classify the error:
   - missing content / wrong display/visibility
   - wrong layout geometry (block/inline/table/flex/grid/positioning)
   - wrong stacking/clip/transform/filter
   - text metrics/shaping/line breaks/bidi/font fallback
   - images/SVG/replaced sizing/object-fit/srcset selection
   - resource failures (CSS/images/fonts blocked/missing)
5. **Freeze a deterministic repro**:
   - Prefer: bundle → offline page fixture → golden/regression.
6. **Implement the missing spec behavior** (no hacks), then rerun the diff and the pageset.

## What to implement (pageset-driven capability buildout)

Assume pageset failures are often caused by **big missing pieces**, not “micro tweaks”.
When you see a gap, implement it properly:

- **CSS values + computed style**: parsing/serialization, shorthands, calc/var, percentage bases, initial/inherit/unset, correct error handling.
- **Selectors + cascade**: correct specificity, `:has()`/`:nth-*` semantics, shadow DOM selectors when used, UA defaults that match modern expectations.
- **Layout correctness**:
  - block/inline formatting contexts (line breaking, bidi ordering, baseline alignment),
  - tables (CSS 2.1 17.x), including spans/border models,
  - flex/grid correctness (delegated to Taffy but with correct inputs/constraints and intrinsic sizing),
  - positioned layout (containing blocks, shrink-to-fit, percentage bases).
- **Painting correctness**:
  - stacking contexts, z-index, clipping/overflow, border-radius,
  - transforms/opacity/filters/masks when encountered by pageset,
  - background/border/outlines/shadows and their spec ordering.
- **Text rendering correctness**: font fallback, shaping, variation/color fonts, decorations, vertical writing modes, ligatures, emoji.
- **Replaced elements/media**: sizing rules, object-fit/position, responsive images, SVG embedding, poster/icon selection.

Always tie the work back to a pageset page and add a regression where possible.

## Perf/infra policy (supporting only)

Perf/infra is allowed when it:

- prevents timeouts/loops,
- makes a correctness investigation possible (better diffs/diagnostics),
- materially reduces iteration time on a pageset-blocking bug.

Otherwise: stop and implement the missing behavior.

## Progress artifacts (committed scoreboard)

We track the pageset in-repo so progress/regressions are visible.

- `progress/pages/<stem>.json` is written by tooling (`src/bin/pageset_progress.rs`, `PageProgress`).
- Don’t hand-author these files; only edit durable human fields like `notes` / `last_*` if needed.

Minimal documented shape (source of truth is code):

```json
{
  "url": "https://example.com/",
  "config": {
    "disk_cache_enabled": true,
    "http_browser_headers_enabled": true
  },
  "status": "ok|timeout|panic|error",
  "total_ms": 123.4,
  "stages_ms": { "fetch": 0.0, "css": 0.0, "cascade": 0.0, "box_tree": 0.0, "layout": 0.0, "paint": 0.0 },
  "notes": "short, durable explanation of current blocker",
  "auto_notes": "machine-generated last-run diagnostics (overwritten each run)",
  "hotspot": "fetch|css|cascade|box_tree|layout|paint|unknown",
  "failure_stage": "dom_parse|css|cascade|box_tree|layout|paint|null",
  "timeout_stage": "dom_parse|css|cascade|box_tree|layout|paint|null",
  "last_good_commit": "abcdef0",
  "last_regression_commit": "1234567"
}
```

`stages_ms` is a coarse **wall-time** attribution derived from the worker stage heartbeat timeline
when available and rescaled so buckets sum (within rounding error) to `total_ms`.

## Tooling (entry points)

- **Pageset loop**: `scripts/pageset.sh`, `cargo xtask pageset`
- **Scoreboard**: `pageset_progress report`
- **Correct-engine baseline (cached pages; best-effort)**: `scripts/chrome_baseline.sh` (+ Ubuntu deps helper)
- **Render outputs**: `render_pages` (writes `fetches/renders/*.png`)
- **Diffs**: `diff_renders`
- **Deterministic fixture evidence**: `render_fixtures`, `cargo xtask chrome-baseline-fixtures`, `cargo xtask fixture-chrome-diff`
- **Offline repros**: `bundle_page` + `cargo xtask import-page-fixture`
- **Inspection**: `inspect_frag`
- **Debug/perf docs**: `docs/debugging.md`, `docs/env-vars.md`, `docs/instrumentation.md`, `docs/profiling-linux.md`

## Documentation policy

- `docs/` is curated and must reflect reality.
- Don’t add scratchpads/status logs. Use the committed `progress/` scoreboard plus focused `docs/notes/*` writeups for durable decisions.
