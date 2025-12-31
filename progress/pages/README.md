# `progress/pages/*.json`

This directory contains the **committed pageset scoreboard**: one tiny JSON file per cached page stem.

- Bootstrap with `pageset_progress sync` (`cargo run --release --bin pageset_progress -- sync [--prune]`) to materialize one placeholder per official pageset URL, even on a fresh checkout with no caches.
- `sync` writes minimal `status: error` entries with `auto_notes: "not run"` or `auto_notes: "missing cache"`; `--prune` removes files for URLs no longer in the pageset list.
- `pageset_progress run` updates these files after caches exist.
- Each `<stem>.json` should match the cached HTML filename stem under `fetches/html/<stem>.html` (same normalization as `fetch_pages`).
- Keep these files small and stable (no raw HTML, no machine-local paths, no traces).
- When diagnostics are enabled, successful renders may include `diagnostics.stats` (structured `RenderStats` timing/count summaries) to power `pageset_progress report --verbose-stats`. No giant blobs or logs.
  - `diagnostics.stats.resources` may include resource/cache breakdowns used for pageset triage:
    - `fetch_counts` (by resource kind)
    - `image_cache_hits` / `image_cache_misses`
    - `resource_cache_fresh_hits` / `resource_cache_stale_hits` / `resource_cache_revalidated_hits` / `resource_cache_misses` / `resource_cache_bytes`
    - `disk_cache_hits` / `disk_cache_misses` / `disk_cache_bytes` / `disk_cache_ms` (disk-backed subresource cache reads)
    - `network_fetches` / `network_fetch_bytes` / `network_fetch_ms` (HTTP fetches performed by the underlying fetcher)
- These are auto-generated; don't hand-edit them except for durable human fields like `notes`/`last_*` when needed.
- `notes` is intended for durable human explanations; `auto_notes` is machine-generated last-run diagnostics and is rewritten on each run.
- Renderer-provided `failure_stage`/`timeout_stage` fields stay `null` on placeholders and are populated directly from diagnostics during runs for programmatic triage.

See `AGENTS.md` for the intent and schema.
