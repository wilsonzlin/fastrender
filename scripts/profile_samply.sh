#!/usr/bin/env bash
set -euo pipefail

PAGE_STEM="${1:-}"
if [[ -z "${PAGE_STEM}" ]]; then
  echo "usage: $0 <page-stem> [pageset_progress args...]"
  echo "example: $0 example.com --timeout 5"
  exit 2
fi
shift || true

if ! command -v samply >/dev/null 2>&1; then
  echo "missing 'samply' (install with: cargo install --locked samply)"
  exit 1
fi

# Terminal-only / headless friendly: write profiles to disk and don't auto-open a browser.
OUT_DIR="${OUT_DIR:-target/pageset/profiles}"
mkdir -p "${OUT_DIR}"
OUT_FILE="${OUT_DIR}/${PAGE_STEM}-$(date +%Y%m%d-%H%M%S).profile.json.gz"

# Build a symbolized release binary suitable for profiling.
export CARGO_PROFILE_RELEASE_DEBUG=1
export CARGO_PROFILE_RELEASE_STRIP=none
export RUSTFLAGS="${RUSTFLAGS:--C force-frame-pointers=yes}"

cargo build --release --bin pageset_progress

samply record --save-only --no-open -o "${OUT_FILE}" -- \
  target/release/pageset_progress run --jobs 1 --pages "${PAGE_STEM}" "$@"

echo "Wrote: ${OUT_FILE}"
echo "To view later: samply load ${OUT_FILE}"

if command -v python3 >/dev/null 2>&1; then
  echo
  echo "Summary (terminal-friendly):"
  python3 scripts/samply_summary.py "${OUT_FILE}" --top 25 || true
fi

