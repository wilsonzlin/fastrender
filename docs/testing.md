# Testing

FastRender’s test suite is primarily Rust unit/integration tests plus a small set of visual fixture tests.

## Core tests

- Run everything: `cargo test --quiet`

## Fonts

Text shaping and many rendering/layout tests assume at least one usable font is available.
CI forces bundled, license-compatible fixtures (`tests/fixtures/fonts/`) so goldens stay
deterministic across platforms. Set `FASTR_USE_BUNDLED_FONTS=1` locally to match CI output.

- When relying on platform fonts, install a basic package (e.g. `fonts-dejavu-core` on
  Ubuntu/Debian). Desktop OSes typically have usable defaults preinstalled.
- The public API exposes `FastRenderConfig::with_font_sources(FontConfig::...)` to pin renders
  to bundled fonts or add additional font directories when needed.

## Style regression harness

- Run: `cargo test --quiet --test style_tests`

This harness covers targeted style/cascade/layout regressions.

## Fixture renders (goldens)

`tests/fixtures_test.rs` renders HTML fixtures under `tests/fixtures/html/` and writes/reads golden PNGs under `tests/fixtures/golden/`.

- Run fixtures: `cargo test fixtures`
- (Re)generate goldens: `UPDATE_GOLDEN=1 cargo test fixtures`
- To refresh a single fixture (faster): `UPDATE_GOLDEN=1 cargo test test_fixture_<name> -- --exact`

Rendered output is compared pixel-by-pixel against the checked-in PNG goldens. Failures write artifacts under `target/fixtures_diffs/<fixture>_{actual,expected,diff}.png` for debugging.

Comparisons are strict by default. To allow small local differences (fonts, GPU, AA), set a tolerance env var:

- `FIXTURE_TOLERANCE=5` (per-channel tolerance)
- `FIXTURE_MAX_DIFFERENT_PERCENT=0.5` (percent of pixels allowed to differ)
- `FIXTURE_FUZZY=1` (preset: tolerance 10, up to 1% different, no alpha compare, max perceptual distance 0.05)
- `FIXTURE_IGNORE_ALPHA=1` (ignore alpha differences even without fuzzy)
- `FIXTURE_MAX_PERCEPTUAL_DISTANCE=0.05` (allow minor perceptual differences using SSIM-style scoring)

New columns/transform/form fixtures ship with checked-in goldens; keep these up to date when adjusting layouts.

## Offline page regression suite

- Run: `cargo test pages_regression`
- Refresh goldens: `UPDATE_PAGES_GOLDEN=1 cargo test pages_regression`

This suite renders a curated set of realistic pages under `tests/pages/fixtures/` (flex/grid/table, multicol, pagination, masks/filters, SVG, writing modes, form controls, plus a positioned-child regression) and compares them against goldens in `tests/pages/golden/`.

Artifacts for failures land in `target/pages_diffs/<page>_{actual,expected,diff}.png`. Comparison defaults to strict pixel matching but respects the same knobs as the fixture harness with `PAGES_TOLERANCE`, `PAGES_MAX_DIFFERENT_PERCENT`, `PAGES_FUZZY=1`, `PAGES_IGNORE_ALPHA=1`, and `PAGES_MAX_PERCEPTUAL_DISTANCE=0.05`.

### Chrome baselines + evidence reports (offline fixtures)

When doing accuracy work, it’s often useful to compare an offline fixture render against Chrome using the **same** deterministic inputs (no network). Chrome baselines are **local-only artifacts** and are not committed.

```bash
# One-command evidence report (runs FastRender render + Chrome baseline + diff):
cargo xtask fixture-chrome-diff
# Report: target/fixture_chrome_diff/report.html
# Defaults to the curated pages_regression fixture set from tests/pages_regression_test.rs.
# Pass --all-fixtures to render everything under tests/pages/fixtures instead.

# 1) Render the offline fixture(s) with FastRender (offline; bundled fonts):
cargo run --release --bin render_fixtures -- --out-dir target/fixture_chrome_diff/fastrender

# 2) Produce local Chrome baseline PNGs for those fixture(s):
cargo xtask chrome-baseline-fixtures --out-dir target/fixture_chrome_diff/chrome

# 3) Generate a combined Chrome-vs-FastRender HTML report under target/.
# (Re-runs steps 1-2 by default; pass `--no-chrome` to reuse `target/fixture_chrome_diff/chrome`;
# pass `--no-build` to reuse an existing `target/release/diff_renders` binary.)
cargo xtask fixture-chrome-diff
```

Defaults are aligned with the `pages_regression` suite (`viewport=1040x1240`, `dpr=1.0`) unless you override them.

Artifacts and PR guidance:

- Report: `target/fixture_chrome_diff/report.html` (plus `report.json` and per-fixture PNG/log/metadata artifacts under `target/fixture_chrome_diff/{chrome,fastrender,...}`).
- Re-run without invoking Chrome (reuse existing renders under `target/fixture_chrome_diff/chrome`): `cargo xtask fixture-chrome-diff --no-chrome`.
- Exit non-zero when diffs are found (useful for gating local scripts): `cargo xtask fixture-chrome-diff --fail-on-differences`.
- Write FastRender pipeline snapshots for later `diff_snapshots`: `cargo xtask fixture-chrome-diff --write-snapshot` (writes under `target/fixture_chrome_diff/fastrender/<fixture>/snapshot.json`).
- **Do not commit** Chrome baseline PNGs or diff reports; they are local artifacts. Attach the generated report directory (or at least `report.html` + the referenced PNGs) to your PR description instead.
- **Do commit** new/updated fixtures under `tests/pages/fixtures/<fixture>/` when they are part of the regression story.

#### Comparing two Chrome-vs-FastRender reports (delta)

When iterating on correctness, it’s often useful to quantify “did accuracy vs Chrome improve overall?” between two runs. You can compare two `diff_renders` reports (including `fixture-chrome-diff`’s `report.json`) with `compare_diff_reports`:

```bash
# On a baseline commit:
cargo xtask fixture-chrome-diff --out-dir target/fixture_chrome_diff_before

# On your current commit:
cargo xtask fixture-chrome-diff --out-dir target/fixture_chrome_diff_after

# Summarize deltas (improvements/regressions) between the two reports:
cargo run --release --bin compare_diff_reports -- \
  --baseline target/fixture_chrome_diff_before/report.json \
  --new target/fixture_chrome_diff_after/report.json \
  --json target/fixture_chrome_diff_delta/report.json \
  --html target/fixture_chrome_diff_delta/report.html

# Optional gating (exit non-zero on regressions):
#   --fail-on-regression --regression-threshold-percent 0.05
```

#### CI option (no local Chrome required)

If you can’t install Chrome/Chromium locally, the repository provides an **optional** GitHub Actions workflow that generates the same deterministic fixture-vs-Chrome diff report and uploads it as an artifact:

- Workflow: `.github/workflows/chrome_fixture_diff.yml`
- Artifact: `fixture_chrome_diff_ci`
- Report path inside the artifact: `target/fixture_chrome_diff_ci/report.html`

This is intended as a convenient way to attach evidence to PRs without requiring every contributor to have Chrome installed locally.

### Importing new offline page fixtures

Use `bundle_page` to capture a page once, then convert that bundle into a deterministic fixture consumable by `pages_regression`:

1. Capture a bundle:
   - Online (network): `cargo run --release --bin bundle_page -- fetch <url> --out /tmp/capture.tar` (or a directory path)
      - If a page crashes or times out during capture, add `--no-render` to crawl HTML + CSS for subresources without doing a full render.
   - Offline (from warmed pageset caches): `cargo run --release --features disk_cache --bin bundle_page -- cache <stem> --out /tmp/capture.tar`
      - Reads HTML from `fetches/html/<stem>.html` and subresources from the disk-backed cache under `fetches/assets/` (override with `--asset-cache-dir` / `--cache-dir`).
2. Import: `cargo xtask import-page-fixture /tmp/capture.tar <fixture_name> [--output-root tests/pages/fixtures --overwrite --dry-run]`
3. Validate the imported fixture is fully offline (no fetchable `http(s)` URLs left behind): `cargo xtask validate-page-fixtures --only <fixture_name>`
4. Add the new fixture to `tests/pages_regression_test.rs` and generate a golden if you want it covered by the suite.

The importer rewrites all HTML/CSS references to hashed files under `assets/` and refuses to leave `http(s)` URLs behind, so the resulting directory is fully offline. A synthetic bundle for testing lives under `tests/fixtures/bundle_page/simple`, and `tests/pages/fixtures/bundle_import_example/` shows the expected output produced by the importer.

Tip: if you already have a warmed pageset disk cache, `cargo xtask pageset --capture-missing-failure-fixtures` can automatically capture/import missing fixtures for pages that currently fail in `progress/pages/*.json` (it uses `bundle_page cache` + `cargo xtask import-page-fixture` under the hood).

## WPT harness (local, visual)

There is a self-contained WPT-style runner under `tests/wpt/` for local “render and compare” tests. It does not talk to upstream WPT and never fetches from the network.

- Run: `cargo xtask test wpt` (or `cargo test --quiet wpt_local_suite_passes -- --exact`)
- Each rendered document is given a per-document `file://` base URL (the test HTML path for the test render, and the reference HTML path for the reference render) so relative resources like `support/*.css`, images, and fonts resolve reliably regardless of the current working directory.
- `WptRunnerBuilder::build()` defaults to an offline renderer (`ResourcePolicy` with `http/https` disabled). Advanced callers can still inject a custom renderer via `.renderer(...)`.

- Discovery reads sidecar metadata next to tests:
  - `.html.ini` files set expectations (`expected: FAIL`), `disabled` reasons, timeouts, viewport, and DPR.
  - `<link rel="match" | rel="mismatch">` inside HTML declares reftest references without touching the manifest.
  - The legacy `tests/wpt/manifest.toml` is still honored; set `HarnessConfig::with_discovery_mode(DiscoveryMode::MetadataOnly)` to ignore it when adding new offline WPT dumps.
- The `wpt_local_suite_passes` smoke-test suite generates expected images under `target/wpt-expected/` by default so local runs don’t require checked-in PNGs. Reftests compare against the reference HTML and do not require a PNG.
- The repo also contains `tests/wpt/expected/` for curated baselines; harness configurations that point `expected_dir` there can use it for stricter gating.
- Artifacts always land in `target/wpt-output/<id>/{actual,expected,diff}.png` with a filterable `report.html`.
- Viewport/DPR are fixed per-test from metadata. CI can pin fonts for deterministic renders via `HarnessConfig::with_font_dir`/`WptRunnerBuilder::font_dir` (for example, point at `tests/fonts/`).
- The runner supports parallel execution and per-test timeouts (see `HarnessConfig`).
- Comparisons use the shared image comparison module (same as fixtures/ref tests) with configurable tolerance, alpha handling, pixel difference thresholds, and perceptual distance thresholds to reduce platform noise.

### WPT importer (offline)

Use `import_wpt` to bring small slices of upstream WPT into `tests/wpt/tests/` without curating each support file by hand. The importer is entirely file-based and rewrites absolute URLs so tests work offline.

- Example (against a local WPT checkout): `cargo run --bin import_wpt -- --wpt-root ~/code/wpt --suite css/css-text/white-space --out tests/wpt/tests`
- `--suite` can be repeated and supports directories, individual files, and globs (e.g. `--suite css/css-text/* --suite html/semantics/forms/the-input-element/input-type-number.html`).
- Preview changes without writing: add `--dry-run`
- Update existing files/manifest entries: add `--overwrite`
- Control metadata: `--manifest <path>` overrides the default `tests/wpt/manifest.toml`; `--no-manifest` skips updates
- Allow leaving network URLs in imported HTML/CSS (not recommended; disables offline validation): `--allow-network`
- Enable extra strict offline validation (optional): `--strict-offline`
  - In addition to the default “no fetchable network URLs” checks, this scans the rewritten HTML/CSS for any remaining `http(s)://` or protocol-relative (`//`) URL strings (excluding `data:` URLs) and fails the import if any are found.
  - Useful for catching unusual leftover references that the targeted validators may miss (for example, network-looking strings outside typical `src=`/`href=`/`srcset=`/CSS `url()` contexts).
- Offline behavior (important for deterministic tests):
  - Root-relative URLs (e.g. `/resources/foo.png`) and `web-platform.test` URLs are rewritten to file-relative paths inside the imported tree.
  - Rewrites cover common fetchable HTML/CSS URL contexts, including:
    - HTML: `src`, fetchable `href` contexts such as `<link href=...>` (navigation links like `<a href=...>` are ignored), `poster`, `object[data]`, `srcset`, and `imagesrcset`.
    - SVG: `href` / `xlink:href` on fetchable elements (e.g. `<image>`, `<use>`, `<feImage>`); navigation links like `<a xlink:href>` are ignored.
    - CSS: `url(...)` and `@import`.
  - Sidecar metadata files (e.g. `.html.ini`) are copied alongside tests when present so the local WPT harness can apply expectations/viewport/DPR.
  - The importer is strict: missing referenced files fail the import rather than silently leaving network URLs behind.

A tiny synthetic WPT-like tree lives under `tests/wpt/_import_testdata/` and is exercised in CI. You can sanity check the importer locally via:

```
cargo run --bin import_wpt -- \
  --wpt-root tests/wpt/_import_testdata/wpt \
  --suite css/simple \
  --out /tmp/fastrender-wpt-import \
  --manifest /tmp/fastrender-wpt-import/manifest.toml
```

## Fuzzing (CSS parsing and selectors)

Structured fuzzers live under `fuzz/` and target crash-prone areas in CSS parsing, selector matching, and custom property resolution.

- Install tooling once: `cargo install cargo-fuzz`
- Quick local run with a short time budget: `cargo fuzz run css_parser -- -runs=1000`
- Selector matching and variable/calc parsing are covered by the `selectors` and `vars_and_calc` targets.
- Seed corpora with real-world CSS live in `tests/fuzz_corpus/`; pass them as extra corpus paths (e.g. `cargo fuzz run selectors fuzz/corpus/selectors tests/fuzz_corpus`).

The fuzz harnesses are optimized for fast iterations and can be run in CI with a low `-max_total_time` when needed.
