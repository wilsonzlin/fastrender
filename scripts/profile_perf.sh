#!/usr/bin/env bash
set -euo pipefail

if [[ "$#" -eq 0 ]]; then
  echo "usage: $0 <page-stem> [pageset_progress args...]"
  echo "   or: $0 --from-progress progress/pages --only-failures [pageset_progress args...]"
  echo "example: $0 example.com --timeout 5"
  echo "stems match fetch_pages normalization (strip scheme + leading www.)"
  exit 2
fi

PAGE_STEM="${1:-}"
if [[ "${PAGE_STEM}" == --* ]]; then
  PAGE_ARGS=("$@")
else
  shift || true
  PAGE_ARGS=(--pages "${PAGE_STEM}" "$@")
fi

if ! command -v perf >/dev/null 2>&1; then
  echo "missing 'perf' (install your distro's linux-tools / perf package)"
  exit 1
fi

# Build a symbolized release binary suitable for profiling.
export CARGO_PROFILE_RELEASE_DEBUG=1
export CARGO_PROFILE_RELEASE_STRIP=none

FEATURE_ARGS=(--features disk_cache)
cargo build --release "${FEATURE_ARGS[@]}" --bin pageset_progress

perf record -F 99 --call-graph dwarf -- \
  target/release/pageset_progress run --jobs 1 "${PAGE_ARGS[@]}"

echo "Run: perf report"
