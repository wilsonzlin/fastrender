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
  PAGE_LABEL="${PROFILE_LABEL:-pageset}"
  PAGE_ARGS=("$@")
else
  PAGE_LABEL="${PROFILE_LABEL:-${PAGE_STEM}}"
  shift || true
  PAGE_ARGS=(--pages "${PAGE_STEM}" "$@")
fi

if ! command -v samply >/dev/null 2>&1; then
  echo "missing 'samply' (install with: cargo install --locked samply)"
  exit 1
fi

# Terminal-only / headless friendly: write profiles to disk and don't auto-open a browser.
OUT_DIR="${OUT_DIR:-target/pageset/profiles}"
mkdir -p "${OUT_DIR}"
OUT_FILE="${OUT_DIR}/${PAGE_LABEL}-$(date +%Y%m%d-%H%M%S).profile.json.gz"

# Build a symbolized release binary suitable for profiling.
export CARGO_PROFILE_RELEASE_DEBUG=1
export CARGO_PROFILE_RELEASE_STRIP=none
export RUSTFLAGS="${RUSTFLAGS:--C force-frame-pointers=yes}"

FEATURE_ARGS=(--features disk_cache)
cargo build --release "${FEATURE_ARGS[@]}" --bin pageset_progress

BIN_PATH="target/release/pageset_progress"
# For reliable post-hoc symbolication, keep the exact binary that produced the sampled addresses.
# We prefer a hardlink (cheap; preserves the old inode if `cargo build` replaces the binary later).
# If hardlinking fails (e.g. cross-filesystem), set `PROFILE_COPY_BINARY=1` to force a full copy.
BIN_SNAPSHOT="${OUT_FILE%.profile.json.gz}.pageset_progress"
if [[ "${PROFILE_SAVE_BINARY:-1}" != "0" ]]; then
  if ln "${BIN_PATH}" "${BIN_SNAPSHOT}" 2>/dev/null; then
    echo "Saved profiled binary (hardlink): ${BIN_SNAPSHOT}"
  elif [[ "${PROFILE_COPY_BINARY:-0}" == "1" ]]; then
    cp "${BIN_PATH}" "${BIN_SNAPSHOT}"
    echo "Saved profiled binary (copy): ${BIN_SNAPSHOT}"
  else
    echo "Note: could not hardlink ${BIN_PATH} -> ${BIN_SNAPSHOT} (set PROFILE_COPY_BINARY=1 to copy)"
  fi
fi

samply record --save-only --no-open -o "${OUT_FILE}" -- \
  "${BIN_PATH}" run --jobs 1 "${PAGE_ARGS[@]}"

echo "Wrote: ${OUT_FILE}"
echo "To view later: samply load ${OUT_FILE}"

if command -v python3 >/dev/null 2>&1; then
  echo
  echo "Summary (terminal-friendly):"
  # `samply` writes raw `0x...` frames into the JSON; symbolize them via llvm-addr2line using the
  # exact binary that was profiled (rebuilt binaries won't match the recorded addresses).
  ADDR2LINE_BIN="${BIN_PATH}"
  if [[ -f "${BIN_SNAPSHOT}" ]]; then
    ADDR2LINE_BIN="${BIN_SNAPSHOT}"
  fi
  python3 scripts/samply_summary.py "${OUT_FILE}" --top 25 --addr2line-binary "${ADDR2LINE_BIN}" || true
fi
