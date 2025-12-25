# Testing

FastRender’s test suite is primarily Rust unit/integration tests plus a small set of visual fixture tests.

## Core tests

- Run everything: `cargo test --quiet`

## Fonts

Text shaping and many rendering/layout tests assume at least one system font is available.

- Linux containers: install a basic font package (e.g. `fonts-dejavu-core` on Ubuntu/Debian).
- Desktop OSes typically have usable defaults preinstalled.

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
- `FIXTURE_FUZZY=1` (preset: tolerance 10, up to 1% different, no alpha compare)
- `FIXTURE_IGNORE_ALPHA=1` (ignore alpha differences even without fuzzy)
- `FIXTURE_MAX_PERCEPTUAL_DISTANCE=0.05` (allow minor perceptual differences using SSIM-style scoring)

New columns/transform/form fixtures ship with checked-in goldens; keep these up to date when adjusting layouts.

## WPT harness (local, visual)

There is a self-contained WPT-style runner under `tests/wpt/` for local “render and compare” tests. It does not talk to upstream WPT and never fetches from the network.

- Discovery reads sidecar metadata next to tests:
  - `.html.ini` files set expectations (`expected: FAIL`), `disabled` reasons, timeouts, viewport, and DPR.
  - `<link rel="match" | rel="mismatch">` inside HTML declares reftest references without touching the manifest.
  - The legacy `tests/wpt/manifest.toml` is still honored; set `HarnessConfig::with_discovery_mode(DiscoveryMode::MetadataOnly)` to ignore it when adding new offline WPT dumps.
- Visual baselines live under `tests/wpt/expected/` following the same relative path structure. Missing images are auto-generated on first run; set `UPDATE_WPT_EXPECTED=1 cargo test wpt_local_suite_passes -- --exact` to refresh everything. Reftests compare against the reference HTML and do not require a PNG.
- Artifacts always land in `target/wpt-output/<id>/{actual,expected,diff}.png` with a filterable `report.html`.
- Viewport/DPR are fixed per-test from metadata. CI can pin fonts for deterministic renders via `HarnessConfig::with_font_dir`/`WptRunnerBuilder::font_dir` (for example, point at `tests/fonts/`).
- The runner supports parallel execution and per-test timeouts (see `HarnessConfig`).
- Comparisons use the shared image comparison module (same as fixtures/ref tests) with configurable tolerance, alpha handling, pixel difference thresholds, and perceptual distance thresholds to reduce platform noise.

### WPT importer (offline)

Use `import_wpt` to bring small slices of upstream WPT into `tests/wpt/tests/` without curating each support file by hand. The importer is entirely file-based and rewrites absolute URLs so tests work offline.

- Example (against a local WPT checkout): `cargo run --bin import_wpt -- --wpt-root ~/code/wpt --suite css/css-text/white-space --out tests/wpt/tests`
- Preview changes without writing: add `--dry-run`
- Update existing files/manifest entries: add `--overwrite`
- Control metadata: `--manifest <path>` overrides the default `tests/wpt/manifest.toml`; `--no-manifest` skips updates

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
