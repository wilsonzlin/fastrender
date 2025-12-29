# `progress/pages/*.json`

This directory contains the **committed pageset scoreboard**: one tiny JSON file per cached page stem.

- Files are written by `pageset_progress` (`cargo run --release --bin pageset_progress -- run ...`).
- Each `<stem>.json` should match the cached HTML filename stem under `fetches/html/<stem>.html`.
- Keep these files small and stable (no raw HTML, no machine-local paths, no traces).

See `AGENTS.md` for the intent and schema.

