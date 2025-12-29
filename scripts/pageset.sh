#!/usr/bin/env bash
set -euo pipefail

# Convenience wrapper for the main planner loop:
#   fetch_pages -> pageset_progress
#
# Environment overrides:
#   JOBS=8 FETCH_TIMEOUT=30 RENDER_TIMEOUT=5 DISK_CACHE=0

JOBS="${JOBS:-$(nproc)}"
FETCH_TIMEOUT="${FETCH_TIMEOUT:-30}"
RENDER_TIMEOUT="${RENDER_TIMEOUT:-5}"
USE_DISK_CACHE="${DISK_CACHE:-1}"

ARGS=()
PARSE_FLAGS=1
for arg in "$@"; do
  if [[ "${PARSE_FLAGS}" -eq 1 ]]; then
    case "${arg}" in
      --no-disk-cache)
        USE_DISK_CACHE=0
        continue
        ;;
      --disk-cache)
        USE_DISK_CACHE=1
        continue
        ;;
      --)
        PARSE_FLAGS=0
        ARGS+=("${arg}")
        continue
        ;;
    esac
  fi
  ARGS+=("${arg}")
done

FEATURE_ARGS=()
if [[ "${USE_DISK_CACHE}" != 0 ]]; then
  FEATURE_ARGS=(--features disk_cache)
fi

echo "Fetching pages (jobs=${JOBS}, timeout=${FETCH_TIMEOUT}s, disk_cache=${USE_DISK_CACHE})..."
cargo run --release "${FEATURE_ARGS[@]}" --bin fetch_pages -- --jobs "${JOBS}" --timeout "${FETCH_TIMEOUT}"

echo "Updating progress/pages (jobs=${JOBS}, hard timeout=${RENDER_TIMEOUT}s, disk_cache=${USE_DISK_CACHE})..."
cargo run --release "${FEATURE_ARGS[@]}" --bin pageset_progress -- run --jobs "${JOBS}" --timeout "${RENDER_TIMEOUT}" "${ARGS[@]}"
