#!/usr/bin/env bash
set -euo pipefail

PAGE_STEM="${1:-}"
if [[ -z "${PAGE_STEM}" ]]; then
  echo "usage: $0 <page-stem> [pageset_progress args...]"
  echo "example: $0 example.com --timeout 5"
  exit 2
fi
shift || true

if ! command -v perf >/dev/null 2>&1; then
  echo "missing 'perf' (install your distro's linux-tools / perf package)"
  exit 1
fi

# Build a symbolized release binary suitable for profiling.
export CARGO_PROFILE_RELEASE_DEBUG=1
export CARGO_PROFILE_RELEASE_STRIP=none

cargo build --release --bin pageset_progress

perf record -F 99 --call-graph dwarf -- \
  target/release/pageset_progress run --jobs 1 --pages "${PAGE_STEM}" "$@"

echo "Run: perf report"

