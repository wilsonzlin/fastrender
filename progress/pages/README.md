# `progress/pages/*.json`

This directory contains the **committed pageset scoreboard**: one tiny JSON file per cached page stem.

- Bootstrap with `pageset_progress sync` (`cargo run --release --bin pageset_progress -- sync [--prune]`) to materialize one placeholder per official pageset URL, even on a fresh checkout with no caches.
- `sync` writes minimal `status: error` entries with `auto_notes: "not run"` or `auto_notes: "missing cache"`; `--prune` removes files for URLs no longer in the pageset list.
- `pageset_progress run` updates these files after caches exist.
- `pageset_progress migrate` rewrites existing progress files without rendering, applying legacy schema migrations (e.g. splitting mixed legacy `notes` into durable `notes` + machine `auto_notes`) and reserializing deterministically.
- Each `<stem>.json` should match the cached HTML filename stem under `fetches/html/<stem>.html` (same normalization as `fetch_pages`).
- Keep these files small and stable (no raw HTML, no machine-local paths, no traces).
- When diagnostics are enabled, successful renders may include `diagnostics.stats` (structured `RenderStats` timing/count summaries) to power `pageset_progress report --verbose-stats`. No giant blobs or logs.
  - `diagnostics.stats.resources` may include resource/cache breakdowns used for pageset triage:
    - `fetch_counts` (by resource kind)
    - `image_cache_hits` / `image_cache_misses`
    - `resource_cache_fresh_hits` / `resource_cache_stale_hits` / `resource_cache_revalidated_hits` / `resource_cache_misses` / `resource_cache_bytes`
    - `disk_cache_hits` / `disk_cache_misses` / `disk_cache_bytes` / `disk_cache_ms` / `disk_cache_lock_waits` / `disk_cache_lock_wait_ms` (disk-backed subresource cache reads)
    - `fetch_inflight_waits` / `fetch_inflight_wait_ms` (single-flight de-dup wait time)
    - `network_fetches` / `network_fetch_bytes` / `network_fetch_ms` (HTTP fetches performed by the underlying fetcher)
- These are auto-generated; don't hand-edit them except for durable human fields like `notes`/`last_*` when needed.
- `notes` is intended for durable human explanations; `auto_notes` is machine-generated last-run diagnostics and is rewritten on each run.
- Renderer-provided `failure_stage`/`timeout_stage` fields stay `null` on placeholders and are populated directly from diagnostics during runs for programmatic triage.
- `diagnostics.stats.cascade` can include selector-level counters (rule candidates/matches, bloom
  fast rejects, time splits, and `:has()` counters) when cascade profiling is enabled:
  - `FASTR_CASCADE_PROFILE=1` for ad-hoc renders, or
  - `pageset_progress run --cascade-diagnostics` to re-run slow cascade pages/timeouts and merge
    the resulting cascade counters into these committed progress artifacts.

`pageset_progress report --verbose-stats` prints these per-page resource breakdowns plus aggregated
totals and top-N rankings (network/inflight/disk) to speed up performance triage.

See `AGENTS.md` for the intent and schema.
