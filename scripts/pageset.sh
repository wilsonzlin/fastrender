#!/usr/bin/env bash
set -euo pipefail

# Convenience wrapper for the main planner loop:
#   fetch_pages -> pageset_progress
#
# Environment overrides:
#   JOBS=8 FETCH_TIMEOUT=30 RENDER_TIMEOUT=5

JOBS="${JOBS:-$(nproc)}"
FETCH_TIMEOUT="${FETCH_TIMEOUT:-30}"
RENDER_TIMEOUT="${RENDER_TIMEOUT:-5}"

echo "Fetching pages (jobs=${JOBS}, timeout=${FETCH_TIMEOUT}s)..."
cargo run --release --bin fetch_pages -- --jobs "${JOBS}" --timeout "${FETCH_TIMEOUT}"

echo "Updating progress/pages (jobs=${JOBS}, hard timeout=${RENDER_TIMEOUT}s)..."
cargo run --release --bin pageset_progress -- run --jobs "${JOBS}" --timeout "${RENDER_TIMEOUT}" "$@"

