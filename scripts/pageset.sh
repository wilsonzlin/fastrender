#!/usr/bin/env bash
set -euo pipefail

# Convenience wrapper for the main planner loop:
#   fetch_pages -> pageset_progress
#
# Environment overrides:
#   JOBS=8 FETCH_TIMEOUT=30 RENDER_TIMEOUT=5 DISK_CACHE=0 NO_DISK_CACHE=1
#   RAYON_NUM_THREADS=4 FASTR_LAYOUT_PARALLEL=off|on|auto
#   FASTR_DISK_CACHE_MAX_BYTES=... FASTR_DISK_CACHE_MAX_AGE_SECS=... (0 = never expire)

TOTAL_CPUS="$(nproc)"
JOBS="${JOBS:-${TOTAL_CPUS}}"
FETCH_TIMEOUT="${FETCH_TIMEOUT:-30}"
RENDER_TIMEOUT="${RENDER_TIMEOUT:-5}"
USE_DISK_CACHE="${DISK_CACHE:-1}"

if [[ "${JOBS}" -lt 1 ]]; then
  echo "JOBS must be > 0" >&2
  exit 2
fi

# pageset_progress runs up to JOBS worker processes in parallel (one per page). The renderer
# itself can also use Rayon threads (e.g., layout fan-out). Without a cap, enabling layout
# parallelism by default can oversubscribe CPUs catastrophically (JOBS * nproc threads).
THREADS_PER_WORKER=$((TOTAL_CPUS / JOBS))
if [[ "${THREADS_PER_WORKER}" -lt 1 ]]; then
  THREADS_PER_WORKER=1
fi

if [[ -z "${RAYON_NUM_THREADS:-}" ]]; then
  export RAYON_NUM_THREADS="${THREADS_PER_WORKER}"
fi
if [[ -z "${FASTR_LAYOUT_PARALLEL:-}" ]]; then
  export FASTR_LAYOUT_PARALLEL=auto
fi

if [[ -n "${NO_DISK_CACHE:-}" ]]; then
  USE_DISK_CACHE=0
fi

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

if [[ "${USE_DISK_CACHE}" != 0 ]]; then
  echo "Prefetching CSS assets (jobs=${JOBS}, timeout=${FETCH_TIMEOUT}s)..."
  cargo run --release "${FEATURE_ARGS[@]}" --bin prefetch_assets -- --jobs "${JOBS}" --timeout "${FETCH_TIMEOUT}"
fi

echo "Updating progress/pages (jobs=${JOBS}, hard timeout=${RENDER_TIMEOUT}s, disk_cache=${USE_DISK_CACHE}, rayon_threads=${RAYON_NUM_THREADS}, layout_parallel=${FASTR_LAYOUT_PARALLEL})..."
cargo run --release "${FEATURE_ARGS[@]}" --bin pageset_progress -- run --jobs "${JOBS}" --timeout "${RENDER_TIMEOUT}" --bundled-fonts "${ARGS[@]}"
