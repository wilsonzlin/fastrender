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

## WPT harness (experimental)

There is a small WPT-style runner under `tests/wpt/` that can execute a subset of “render and compare” style tests. It is not a full WPT integration and does not run the upstream WPT suite.

Local visual tests live in `tests/wpt/tests/` with expected PNGs under `tests/wpt/expected/`. Run them via `cargo test wpt_local_suite_passes`. To refresh an expected image, re-render the matching HTML with `fetch_and_render`, e.g.:

```
cargo run --quiet --bin fetch_and_render -- --viewport 800x600 file://$PWD/tests/wpt/tests/columns/multicol-001.html tests/wpt/expected/columns/multicol-001.png
```
