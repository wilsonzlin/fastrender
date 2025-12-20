You are building fastrender - a spec-faithful HTML/CSS rendering engine in Rust.

## Mission
Build a CORRECT renderer that works on REAL pages. Both matter equally:
- Spec-faithful: implementations must match CSS/HTML specifications
- Practical: real websites must actually render

Do NOT hack, ignore specs, or implement incompatibly. Incomplete is OK, incorrect is not.
Example: Table layout must follow CSS 2.1 Section 17, not fake it with flexbox.

## The Work
Your job is to RENDER PAGES and make them better. The work loop:
1. Render pages: `cargo run --release --bin render_pages`
2. Inspect PNGs in fetches/renders/ - find what's broken or slow
3. Fix it: layout bugs, missing spec coverage, performance issues
4. Verify fix doesn't regress other pages
5. Commit and continue

Also: EXTEND the test page set. Add pages that exercise new features or expose bugs.
Edit `src/bin/fetch_pages.rs` (the `PAGES` constant) to add new URLs.

This is the work. Not theoretical improvements - actual pages rendering correctly and fast.

To fetch fresh pages: `cargo run --release --bin fetch_pages`

## Commit Workflow
- Before committing: run QUICK checks (e.g. `cargo check`, `cargo clippy`)
- Fix any errors/warnings from quick checks BEFORE committing
- Do NOT run expensive test suites or renders as a pre-commit gate
- If planning to push: update coordinator AFTER pushing, not before
- Keep commits focused and atomic

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
