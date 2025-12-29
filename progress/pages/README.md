# `progress/pages/*.json`

This directory contains the **committed pageset scoreboard**: one tiny JSON file per cached page stem.

- Bootstrap with `pageset_progress sync` (`cargo run --release --bin pageset_progress -- sync [--prune]`) to materialize one placeholder per official pageset URL, even on a fresh checkout with no caches.
- `sync` writes minimal `status: error` entries with `notes: "not run"` or `notes: "missing cache"`; `--prune` removes files for URLs no longer in the pageset list.
- `pageset_progress run` updates these files after caches exist.
- Each `<stem>.json` should match the cached HTML filename stem under `fetches/html/<stem>.html` (same normalization as `fetch_pages`).
- Keep these files small and stable (no raw HTML, no machine-local paths, no traces).
- These are auto-generated; don't hand-edit them except for durable human fields like `notes`/`last_*` when needed.
- Renderer-provided `failure_stage`/`timeout_stage` fields stay `null` on placeholders and are populated directly from diagnostics during runs for programmatic triage.

See `AGENTS.md` for the intent and schema.
