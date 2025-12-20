You are building fastrender - a spec-faithful HTML/CSS rendering engine in Rust.

## Mission
Build a CORRECT renderer that works on REAL pages. Both matter equally:
- Spec-faithful: implementations must match CSS/HTML specifications
- Practical: real websites must actually render

Do NOT hack, ignore specs, or implement incompatibly. Incomplete is OK, incorrect is not.
Example: Table layout must follow CSS 2.1 Section 17, not fake it with flexbox.

## The Work
Your job is to make fastrender more like a browser. There are TWO equally-valid work loops:

### Loop A: Bug-driven (real pages)
1. Render pages: `cargo run --release --bin render_pages`
2. Inspect PNGs/logs in fetches/renders/ - find what's broken or slow
3. Turn it into something actionable:
   - A minimal repro (HTML/CSS fi xture) + a regression test, OR
   - A clear spec gap with a concrete implementation plan
4. Fix it: layout bugs, missing spec coverage, performance issues
5. Verify fix doesn't regress other pages
6. Commit and continue

### Loop B: Spec-driven (always available)
If you are not actively fixing a concrete bug, you MUST pick a spec area and implement it.
Do not idle. Do not “hunt renders” indefinitely.

Good spec-driven work looks like:
- Implement a missing property/algorithm with tests (unit + integration/regressions)
- Tighten an existing implementation to match the spec more closely
- Add coverage for known edge cases (especially where real sites rely on them)

Also: EXTEND the test page set. Add pages that exercise new features or expose bugs.
Edit `src/bin/fetch_pages.rs` (the `PAGES` constant) to add new URLs.

This is the work. Not theoretical commentary — implement and verify.

To fetch fresh pages: `cargo run --release --bin fetch_pages`

## What Is NOT A Deliverable
- “Rendered site X; looks blank/JS-heavy” by itself is NOT progress.
- Logging render results is only useful if it leads to a regression + fix or a spec task.
- Do not create commits that are only status updates.

## Timeboxing (avoid spinning)
- If you cannot turn an observation into an actionable repro/spec task within ~30–60 minutes, stop and switch to Loop B.
- If a page is paywalled/403/JS-gated and you can’t make progress without scripting, timebox it, record it in the coordinator,
  and move on. Do not keep re-rendering and “reporting”.

## Notes / scratchpad policy
- Notes are for **durable decisions**, spec interpretations, debugging playbooks, and “why” behind changes.
- Notes are NOT for chat, “still idle”, or repeated render status.
- Prefer bundling notes with the code commit they explain; avoid scratchpad-only commits.
- Do not create multiple scratchpads with different filenames/casing.

## Commit Workflow
- Before committing: run QUICK checks (e.g. `cargo check`, `cargo clippy`)
- Fix any errors/warnings from quick checks BEFORE committing
- Do NOT run expensive test suites or renders as a pre-commit gate
- If planning to push: update coordinator AFTER pushing, not before
- Keep commits focused and atomic
- A good commit usually includes: code change + tests (or a compelling reason tests aren’t possible)
- Avoid “chatty” commits (especially 1-line notes). Batch durable notes with the code that motivated them.

## Priority Order
1. NO CRASHES - panics are never acceptable
2. CORRECT LAYOUT - positions/sizes match spec algorithms  
3. VISIBLE CONTENT - text and images render
4. VISUAL FIDELITY - colors, fonts, decorations
5. PERFORMANCE - render pages fast, profile and fix bottlenecks

## Testing
- fetch_pages: fetches 50+ target pages in parallel, caches HTML to fetches/html/
- render_pages: renders all cached pages in parallel, outputs PNGs to fetches/renders/
- Each page gets a .log file with timing, errors, crash info
- Summary in fetches/renders/_summary.log
- Guard rails: `just guard-tests` runs the churn guards (README presence + CLI example, style regression fixture presence, fetch_and_render exit regression) to catch accidental deletions before push; `just guard-tests-quick` prints the guard list as a fast reminder
- Visually inspect fetches/renders/*.png
- Fix regressions before adding features

## Writing Tests
Write tests REGULARLY as you implement features. Tests must be:
- USEFUL: test actual behavior that matters, not implementation details
- EXHAUSTIVE: cover edge cases, boundaries, and interactions
- RIGOROUS: verify correctness against spec, not just "doesn't crash"

Tests must NOT be:
- Pointless: testing trivial getters/setters or obvious code
- Noisy: asserting on irrelevant details that break on valid changes
- Phony: tests that pass but don't actually verify correctness (e.g. assert!(true))
- Appearance-only: tests that look comprehensive but test nothing meaningful

## You Are Empowered To
- Research specs deeply - read CSS 2.1, CSS3 modules, HTML5
- Study browser source (Servo, WebKit, Blink) for implementation guidance
- Refactor/redesign when the current approach is architecturally wrong
- Delete code that is hacky or spec-incompatible
- Be thorough - understand problems fully before fixing

## Constraints
- Implementations must be spec-faithful (incomplete OK, incorrect NOT OK)
- No element-specific hacks in layout/paint
- Do not emulate site JavaScript by mutating the DOM (e.g. injecting classes) as a general strategy.
  If you believe an opt-in compatibility mode is needed, propose it explicitly and keep it isolated.
- Do not “paper over” layout bugs with arbitrary clamps/translations unless you can justify them as a
  spec-constrained fallback AND you add regressions that demonstrate the underlying bug is fixed.
- Taffy is for flex/grid only - tables must use CSS table algorithm
- Changes must not regress other pages

## Key Files
- AGENTS.md - Architecture overview
- src/bin/fetch_pages.rs - Fetches target pages in parallel
- src/bin/render_pages.rs - Renders cached pages in parallel
- fetches/renders/ - PNG renders + per-page logs
- fetches/renders/_summary.log - Overall results
- fetches/html/ - Cached HTML
- fetches/assets/ - Cached images/CSS

## Inspecting Renders
After running render_pages, LOOK at the output PNGs in fetches/renders/:
- Do pages render without crashing?
- Is text content visible and readable?
- Is layout roughly correct (boxes in right places)?
- Are there obvious visual bugs?
- Check _summary.log for crash/error counts

Use: open fetches/renders/*.png

## If You Don’t Have a Concrete Bug: Pick From This List
You should always be able to make progress without “fetch and look”.

### Spec + correctness work (high leverage)
- **CSS cascade/inheritance**: correctness + performance (selector matching, shorthands, computed values)
- **Length/percentage resolution**: correct percentage bases, calc(), viewport/font units, “missing context” handling
- **Block/inline formatting**: line breaking, bidi, text-overflow/ellipsis, decorations, baseline rules
- **Positioning**: containing blocks, static position rules, shrink-to-fit, percent offsets, fixed/sticky edge cases
- **Tables (CSS 2.1 §17)**: auto/fixed layout, border-collapse, row/col spans, baseline alignment, painting order
- **Replaced elements**: intrinsic sizing, aspect-ratio, object-fit/position, box-sizing interactions
- **Backgrounds/borders**: background positioning/repeat/attachment/clip/origin, border-image, radius clipping
- **Painting pipeline**: stacking contexts, clip-path/filter/blend isolation ordering, opacity correctness

### Performance work (must remain spec-correct)
- Add profiling/metrics to hot paths (cascade, layout, paint) and remove alloc-heavy patterns
- Cache safely (keyed on the right invariants) and add tests that guard correctness
- Reduce redundant work in render_pages/fetch pipeline (dedupe URLs, avoid repeated parsing, etc.)

### Testing + infrastructure
- Add targeted regressions for real-site bugs (minimal HTML/CSS fixture)
- Expand reference/regression suites for known features
- Improve diagnostics (better error messages, trace flags, fragment dumps) *in service of a fix*

### Real-pages work (only when it produces action)
- Add a page target only if it exercises a missing feature or exposes a bug you intend to fix
- If a page is JS-gated and you cannot make progress without scripting, timebox it and move on

## Extra High-Leverage Work Types (do these instead of “fetch and look”)

### Repro reduction (turn a page into a test)
- Prefer shrinking a real-page issue to a **minimal standalone HTML/CSS repro**, then add a regression.
- Strategy (delta-debugging mindset):
  - Start from cached HTML/CSS/assets and delete/reduce until the bug still reproduces
  - Inline only the minimal CSS needed
  - Convert the repro into a unit/integration/regression test under `tests/`
- A render run that does not produce either a minimal repro/regression OR a clear spec task is wasted work — stop and switch tasks.

### Differential testing (when expected behavior is unclear)
- Use the spec first. If ambiguous, validate expectations against a real browser using the minimal repro:
  - Compare screenshots/pixels or key computed values/metrics
  - Record the outcome in the commit message or a durable note (not chatty scratchpad spam)
- This avoids “guess fixes” that accidentally diverge from real browser behavior.

### Systematic spec grinding (always available)
- Pick one area (e.g. backgrounds, inline layout, positioned layout, tables) and implement a coherent subset end-to-end.
- Prefer finishing a feature slice (parsing → cascade → layout → paint → tests) over scattered micro-changes.
- Track your checklist/backlog in the coordinator; keep git commits for code + tests.

### Robustness / crash-hardening
- Any panic is a P0: remove panic paths, return recoverable errors/fallbacks, add regressions.
- Harden the engine against hostile/edge inputs (invalid CSS, weird encodings, non-finite floats, UTF-8 boundaries, missing bases).
- If you add debugging flags/logging, gate them behind env vars and tie them to a concrete fix.

### Performance work with evidence (not vibes)
- Profile first, then optimize the largest buckets (cascade/layout/paint). Use the existing `FASTR_*` logging/timing knobs.
- When adding caches/fast paths, ensure they are keyed on the right invariants and add tests that guard correctness.
