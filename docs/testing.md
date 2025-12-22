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

New columns/transform/form fixtures ship with checked-in goldens; keep these up to date when adjusting layouts.

## WPT harness (local, visual)

There is a self-contained WPT-style runner under `tests/wpt/` for local “render and compare” tests. It does not talk to upstream WPT and never fetches from the network.

- Tests are declared in `tests/wpt/manifest.toml` with per-test viewport, DPR, expected outcome, and type (`visual`, `reftest`, `crashtest`). New HTML fixtures belong under `tests/wpt/tests/` and must be added to the manifest.
- Expected images live under `tests/wpt/expected/` following the same relative path structure. Missing images are auto-generated on first run; set `UPDATE_WPT_EXPECTED=1 cargo test wpt_local_suite_passes -- --exact` to refresh everything.
- Artifacts land in `target/wpt-output/<relative>.actual.png` and `<relative>.diff.png` with a `report.md` summary linking expected/actual/diff for failures.
- The runner supports parallel execution and per-test timeouts (see `HarnessConfig`).

## Fuzzing (CSS parsing and selectors)

Structured fuzzers live under `fuzz/` and target crash-prone areas in CSS parsing, selector matching, and custom property resolution.

- Install tooling once: `cargo install cargo-fuzz`
- Quick local run with a short time budget: `cargo fuzz run css_parser -- -runs=1000`
- Selector matching and variable/calc parsing are covered by the `selectors` and `vars_and_calc` targets.
- Seed corpora with real-world CSS live in `tests/fuzz_corpus/`; pass them as extra corpus paths (e.g. `cargo fuzz run selectors fuzz/corpus/selectors tests/fuzz_corpus`).

The fuzz harnesses are optimized for fast iterations and can be run in CI with a low `-max_total_time` when needed.
