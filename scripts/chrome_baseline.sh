#!/usr/bin/env bash
set -euo pipefail

# Render cached `fetch_pages` HTML in headless Chrome/Chromium and write PNG screenshots.
#
# This is intentionally "good enough" ground-truth to compare against FastRender output.
# It loads the cached HTML from `fetches/html/*.html` but injects a `<base href=...>` using
# the `*.html.meta` sidecar so relative subresources resolve against the original page URL.
#
# Defaults are chosen to align with `render_pages` / `pageset_progress` defaults:
#   viewport=1200x800, dpr=1.0
#
# Example:
#   cargo run --release --bin fetch_pages
#   scripts/chrome_baseline.sh
#   cargo run --release --bin render_pages
#   cargo run --release --bin diff_renders -- \
#     --before fetches/chrome_renders \
#     --after fetches/renders \
#     --json target/chrome_vs_fastrender/report.json \
#     --html target/chrome_vs_fastrender/report.html
#
# Notes:
# - This script does NOT make the run fully deterministic (live subresources can change).
# - It tries to be robust in container/CI-like environments by passing common headless flags.

# Always run relative paths from the repository root, even if the script is invoked from a
# subdirectory.
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "${REPO_ROOT}"

usage() {
  cat <<'EOF'
usage: scripts/chrome_baseline.sh [options] [--] [page_stem...]

Options:
  --html-dir <dir>     Directory containing cached HTML (default: fetches/html)
  --out-dir <dir>      Directory to write PNGs/logs (default: fetches/chrome_renders)
  --viewport <WxH>     Viewport size (default: 1200x800)
  --dpr <float>        Device pixel ratio (default: 1.0)
  --timeout <secs>     Per-page hard timeout (default: 15)
  --shard <index>/<total>
                      Process only a deterministic shard of selected cached pages (0-based)
  --chrome <path>      Chrome/Chromium binary (default: auto-detect)
  --js <on|off>        Enable JavaScript (default: off)
  -h, --help           Show help

Filtering:
  If you pass positional arguments, they are treated as cache stems (file stems),
  and only those pages will be rendered.

Environment (optional):
  HTML_DIR, OUT_DIR, VIEWPORT, DPR, TIMEOUT, SHARD, CHROME_BIN, JS

Output:
  <out-dir>/<stem>.png        Screenshot
  <out-dir>/<stem>.chrome.log Chrome stdout/stderr for debugging
  <out-dir>/<stem>.json       JSON metadata (viewport/DPR/JS/headless mode/input hash/etc.)

EOF
}

HTML_DIR="${HTML_DIR:-fetches/html}"
OUT_DIR="${OUT_DIR:-fetches/chrome_renders}"
VIEWPORT="${VIEWPORT:-1200x800}"
DPR="${DPR:-1.0}"
TIMEOUT="${TIMEOUT:-15}"
SHARD="${SHARD:-}"
CHROME_BIN="${CHROME_BIN:-}"
JS="${JS:-off}"
HEADLESS_FLAG="--headless=new"

FILTERS=()
PARSE_FLAGS=1
while [[ $# -gt 0 ]]; do
  if [[ "${PARSE_FLAGS}" -eq 1 ]]; then
    case "$1" in
      -h|--help)
        usage
        exit 0
        ;;
      --html-dir)
        HTML_DIR="${2:-}"; shift 2 ;;
      --out-dir)
        OUT_DIR="${2:-}"; shift 2 ;;
      --viewport)
        VIEWPORT="${2:-}"; shift 2 ;;
      --dpr)
        DPR="${2:-}"; shift 2 ;;
      --timeout)
        TIMEOUT="${2:-}"; shift 2 ;;
      --shard)
        SHARD="${2:-}"; shift 2 ;;
      --chrome)
        CHROME_BIN="${2:-}"; shift 2 ;;
      --js)
        JS="${2:-}"; shift 2 ;;
      --)
        PARSE_FLAGS=0
        shift
        continue
        ;;
    esac
  fi

  FILTERS+=("$1")
  shift
done

if ! [[ "${VIEWPORT}" =~ ^[0-9]+x[0-9]+$ ]]; then
  echo "invalid --viewport: ${VIEWPORT} (expected WxH like 1200x800)" >&2
  exit 2
fi
VIEWPORT_W="${VIEWPORT%x*}"
VIEWPORT_H="${VIEWPORT#*x}"

case "${JS,,}" in
  on|off) ;;
  *)
    echo "invalid --js: ${JS} (expected on|off)" >&2
    exit 2
    ;;
esac

if ! command -v python3 >/dev/null 2>&1; then
  echo "python3 is required for HTML base-tag injection." >&2
  exit 2
fi

CHROME=""
if [[ -n "${CHROME_BIN}" ]]; then
  CHROME="${CHROME_BIN}"
elif command -v google-chrome-stable >/dev/null 2>&1; then
  CHROME="google-chrome-stable"
elif command -v google-chrome >/dev/null 2>&1; then
  CHROME="google-chrome"
elif command -v chromium >/dev/null 2>&1; then
  CHROME="chromium"
elif command -v chromium-browser >/dev/null 2>&1; then
  CHROME="chromium-browser"
fi

if [[ -z "${CHROME}" ]]; then
  echo "No Chrome/Chromium binary found." >&2
  echo "Install one (e.g. google-chrome or chromium) or pass --chrome /path/to/chrome." >&2
  exit 2
fi

if [[ ! -d "${HTML_DIR}" ]]; then
  echo "HTML dir not found: ${HTML_DIR}" >&2
  echo "Run: cargo run --release --bin fetch_pages" >&2
  exit 1
fi

mkdir -p "${OUT_DIR}"

# Best-effort Chrome version string, recorded in per-page metadata for traceability.
CHROME_VERSION="$("${CHROME}" --version 2>/dev/null | head -n 1 | tr -d '\r' || true)"

# Snap-packaged Chromium runs under strict confinement (AppArmor + mount namespaces).
# In that configuration, `/tmp` is private to the snap, and Chromium may be unable to
# write screenshots to arbitrary repo paths. Use a temp dir under the snap's common
# directory when available so the screenshot is visible to the host process.
CHROME_PATH="$(command -v "${CHROME}" || true)"
TMP_TEMPLATE=""
if [[ "${CHROME_PATH}" == /snap/bin/chromium* ]]; then
  SNAP_COMMON_DIR="${HOME}/snap/chromium/common"
  mkdir -p "${SNAP_COMMON_DIR}" 2>/dev/null || true
  if [[ -d "${SNAP_COMMON_DIR}" ]]; then
    TMP_TEMPLATE="${SNAP_COMMON_DIR}/fastrender-chrome-baseline.XXXXXX"
  fi
fi

if [[ -n "${TMP_TEMPLATE}" ]]; then
  TMP_ROOT="$(mktemp -d "${TMP_TEMPLATE}")"
else
  TMP_ROOT="$(mktemp -d)"
fi
cleanup() {
  rm -rf "${TMP_ROOT}"
}
trap cleanup EXIT

declare -A WANT=()
if [[ "${#FILTERS[@]}" -gt 0 ]]; then
  for f in "${FILTERS[@]}"; do
    WANT["${f}"]=1
  done
fi

shopt -s nullglob
HTML_FILES=("${HTML_DIR}"/*.html)
if [[ "${#HTML_FILES[@]}" -eq 0 ]]; then
  echo "No cached HTML found under ${HTML_DIR}/*.html" >&2
  echo "Run: cargo run --release --bin fetch_pages" >&2
  exit 1
fi

AVAILABLE_STEMS=()
declare -A AVAILABLE=()
for html in "${HTML_FILES[@]}"; do
  stem="$(basename "${html}" .html)"
  AVAILABLE_STEMS+=("${stem}")
  AVAILABLE["${stem}"]=1
done

if [[ "${#FILTERS[@]}" -gt 0 ]]; then
  missing=()
  declare -A missing_seen=()
  for stem in "${FILTERS[@]}"; do
    if [[ -z "${AVAILABLE[${stem}]:-}" && -z "${missing_seen[${stem}]:-}" ]]; then
      missing_seen["${stem}"]=1
      missing+=("${stem}")
    fi
  done
  if [[ "${#missing[@]}" -gt 0 ]]; then
    echo "No cached HTML found for requested page stem(s): ${missing[*]}" >&2
    echo "Available stems live under ${HTML_DIR}/*.html (run fetch_pages first)." >&2
    exit 1
  fi
fi

if [[ -n "${SHARD}" ]]; then
  if ! [[ "${SHARD}" =~ ^[0-9]+/[0-9]+$ ]]; then
    echo "invalid --shard: ${SHARD} (expected index/total like 0/4)" >&2
    exit 2
  fi
  SHARD_INDEX="${SHARD%%/*}"
  SHARD_TOTAL="${SHARD#*/}"
  if [[ "${SHARD_TOTAL}" -lt 1 ]]; then
    echo "invalid --shard: ${SHARD} (total must be >= 1)" >&2
    exit 2
  fi
  if [[ "${SHARD_INDEX}" -ge "${SHARD_TOTAL}" ]]; then
    echo "invalid --shard: ${SHARD} (index must be < total)" >&2
    exit 2
  fi

  if [[ "${#FILTERS[@]}" -gt 0 ]]; then
    mapfile -t MATCHED_SORTED < <(printf '%s\n' "${FILTERS[@]}" | sort -u)
  else
    mapfile -t MATCHED_SORTED < <(printf '%s\n' "${AVAILABLE_STEMS[@]}" | sort -u)
  fi

  MATCHED_COUNT="${#MATCHED_SORTED[@]}"

  SHARDED=()
  for i in "${!MATCHED_SORTED[@]}"; do
    if (( i % SHARD_TOTAL == SHARD_INDEX )); then
      SHARDED+=("${MATCHED_SORTED[$i]}")
    fi
  done
  if [[ "${#SHARDED[@]}" -eq 0 ]]; then
    echo "Shard ${SHARD_INDEX}/${SHARD_TOTAL} selected no cached pages (${MATCHED_COUNT} matched before sharding). Nothing to do." >&2
    exit 1
  fi

  FILTERS=("${SHARDED[@]}")
  WANT=()
  for stem in "${FILTERS[@]}"; do
    WANT["${stem}"]=1
  done
fi

fail=0
ok=0
total=0

echo "Chrome: ${CHROME}"
echo "Input:  ${HTML_DIR}"
echo "Output: ${OUT_DIR}"
echo "Viewport: ${VIEWPORT}  DPR: ${DPR}  JS: ${JS,,}  Timeout: ${TIMEOUT}s"
if [[ -n "${SHARD}" ]]; then
  echo "Shard: ${SHARD}"
fi
echo

for html_path in "${HTML_FILES[@]}"; do
  stem="$(basename "${html_path}" .html)"
  if [[ "${#WANT[@]}" -gt 0 && -z "${WANT[${stem}]:-}" ]]; then
    continue
  fi
  total=$((total + 1))

  meta_path="${html_path}.meta"
  base_url=""
  if [[ -f "${meta_path}" ]]; then
    while IFS= read -r line; do
      case "${line}" in
        url:\ *)
          base_url="${line#url: }"
          break
          ;;
      esac
    done < "${meta_path}"
  fi

  patched_dir="${TMP_ROOT}/html"
  mkdir -p "${patched_dir}"
  patched_html="${patched_dir}/${stem}.html"

  disable_js="0"
  if [[ "${JS,,}" == "off" ]]; then
    disable_js="1"
  fi

  html_sha256="$(python3 - "${html_path}" "${patched_html}" "${base_url}" "${disable_js}" <<'PY'
import sys
import hashlib

in_path = sys.argv[1]
out_path = sys.argv[2]
base_url = sys.argv[3].strip()
disable_js = False
if len(sys.argv) >= 5:
    disable_js = sys.argv[4].strip() == "1"

data = open(in_path, "rb").read()
sha256 = hashlib.sha256(data).hexdigest()
if not base_url and not disable_js:
    open(out_path, "wb").write(data)
    print(sha256, end="")
    sys.exit(0)

lower = data.lower()

def insert_after_open_tag(tag: bytes, insertion: bytes):
    start = 0
    while True:
        idx = lower.find(tag, start)
        if idx == -1:
            return None
        after = lower[idx + len(tag): idx + len(tag) + 1]
        if after and after not in b">\t\r\n /":
            start = idx + len(tag)
            continue
        end = lower.find(b">", idx)
        if end == -1:
            return None
        end += 1
        return data[:end] + b"\n" + insertion + data[end:]

def insert_after_doctype(insertion: bytes):
    tag = b"<!doctype"
    start = 0
    while True:
        idx = lower.find(tag, start)
        if idx == -1:
            return None
        after = lower[idx + len(tag): idx + len(tag) + 1]
        if after and after not in b">\t\r\n ":
            start = idx + len(tag)
            continue
        end = lower.find(b">", idx)
        if end == -1:
            return None
        end += 1
        return data[:end] + b"\n" + insertion + data[end:]

inserts = []
if base_url:
    inserts.append(f'<base href="{base_url}">'.encode("utf-8") + b"\n")
if disable_js:
    # Best-effort JS disable: inject a CSP that blocks script execution.
    # This is more portable than Chromium flag hacks and matches our "no JS" renderer model.
    inserts.append(b"<meta http-equiv=\"Content-Security-Policy\" content=\"script-src 'none';\">\n")

insertion = b"".join(inserts)
if not insertion:
    open(out_path, "wb").write(data)
    print(sha256, end="")
    sys.exit(0)

out = insert_after_open_tag(b"<head", insertion)
if out is None:
    wrapped = b"<head>\n" + insertion + b"</head>\n"
    out = insert_after_open_tag(b"<html", wrapped)
if out is None:
    # Do not insert before the doctype (that would force quirks mode in Chrome).
    out = insert_after_doctype(insertion)
if out is None:
    out = insertion + data

open(out_path, "wb").write(out)
print(sha256, end="")
PY
)"

  url="file://${patched_html}"
  png_path="${OUT_DIR}/${stem}.png"
  chrome_log="${OUT_DIR}/${stem}.chrome.log"
  metadata_path="${OUT_DIR}/${stem}.json"
  tmp_png_dir="${TMP_ROOT}/screenshots"
  mkdir -p "${tmp_png_dir}"
  tmp_png_path="${tmp_png_dir}/${stem}.png"

  profile_dir="${TMP_ROOT}/profile-${stem}"
  mkdir -p "${profile_dir}"

  chrome_args=(
    "${HEADLESS_FLAG}"
    --no-sandbox
    --disable-dev-shm-usage
    --disable-gpu
    --hide-scrollbars
    --window-size="${VIEWPORT_W},${VIEWPORT_H}"
    --force-device-scale-factor="${DPR}"
    --disable-web-security
    --allow-file-access-from-files
    # Reduce background network noise (update checks, DNS prefetch, etc). This should not affect
    # normal page subresource loads.
    --disable-background-networking
    --dns-prefetch-disable
    --no-first-run
    --no-default-browser-check
    --disable-component-update
    --disable-default-apps
    --disable-sync
    --user-data-dir="${profile_dir}"
    # Snap-packaged Chromium can be sandboxed from writing to arbitrary repo paths.
    # Always write the screenshot to a temp directory and then copy it into OUT_DIR.
    --screenshot="${tmp_png_path}"
  )

  # Use `timeout` if available; otherwise run without a hard kill.
  ran_ok=0
  if command -v timeout >/dev/null 2>&1; then
    if timeout "${TIMEOUT}s" "${CHROME}" "${chrome_args[@]}" "${url}" >"${chrome_log}" 2>&1; then
      ran_ok=1
    fi
  else
    if "${CHROME}" "${chrome_args[@]}" "${url}" >"${chrome_log}" 2>&1; then
      ran_ok=1
    fi
  fi

  if [[ "${ran_ok}" -ne 1 || ! -s "${tmp_png_path}" ]]; then
    if [[ "${HEADLESS_FLAG}" == "--headless=new" ]]; then
      log="$(cat "${chrome_log}" 2>/dev/null || true)"
      log_lower="${log,,}"
      headless_new_unsupported=0
      if [[ "${log_lower}" == *"--headless=new"* ]]; then
        if [[ "${log_lower}" == *"unknown flag"* || "${log_lower}" == *"unrecognized option"* || "${log_lower}" == *"unknown option"* ]]; then
          headless_new_unsupported=1
        fi
      fi
      if [[ "${headless_new_unsupported}" -eq 1 ]]; then
        HEADLESS_FLAG="--headless"
        chrome_args[0]="${HEADLESS_FLAG}"
        rm -f "${tmp_png_path}"
        printf "\n\n# Retrying with --headless\n" >>"${chrome_log}" 2>/dev/null || true
        ran_ok=0
        if command -v timeout >/dev/null 2>&1; then
          if timeout "${TIMEOUT}s" "${CHROME}" "${chrome_args[@]}" "${url}" >>"${chrome_log}" 2>&1; then
            ran_ok=1
          fi
        else
          if "${CHROME}" "${chrome_args[@]}" "${url}" >>"${chrome_log}" 2>&1; then
            ran_ok=1
          fi
        fi
      fi
    fi
  fi

  if [[ "${ran_ok}" -eq 1 && -s "${tmp_png_path}" ]]; then
    cp -f "${tmp_png_path}" "${png_path}"
    headless_mode="legacy"
    if [[ "${HEADLESS_FLAG}" == "--headless=new" ]]; then
      headless_mode="new"
    fi
    python3 - "${metadata_path}" "${stem}" "${VIEWPORT_W}" "${VIEWPORT_H}" "${DPR}" "${JS,,}" "${headless_mode}" "${CHROME_VERSION}" "${base_url}" "${html_sha256}" <<'PY'
import json
import sys
from pathlib import Path

out_path = Path(sys.argv[1])
stem = sys.argv[2]
w = int(sys.argv[3])
h = int(sys.argv[4])
dpr = float(sys.argv[5])
js = sys.argv[6]
headless = sys.argv[7]
chrome_version = sys.argv[8].strip()
base_url = sys.argv[9].strip()
html_sha256 = sys.argv[10].strip()

data = {
    "stem": stem,
    "viewport": [w, h],
    "dpr": dpr,
    "js": js,
    "headless": headless,
    "html_sha256": html_sha256,
}
if chrome_version:
    data["chrome_version"] = chrome_version
if base_url:
    data["base_url"] = base_url

out_path.write_text(json.dumps(data, indent=2, sort_keys=True) + "\n")
PY
    ok=$((ok + 1))
    echo "✓ ${stem}"
  else
    fail=$((fail + 1))
    if [[ "${ran_ok}" -eq 1 ]]; then
      echo "✗ ${stem} (no screenshot produced; see ${chrome_log})" >&2
    else
      echo "✗ ${stem} (failed; see ${chrome_log})" >&2
    fi
  fi
done

echo
echo "Done: ${ok} ok, ${fail} failed (out of ${total})"
echo "PNGs:  ${OUT_DIR}/*.png"
echo "Logs:  ${OUT_DIR}/*.chrome.log"
echo "Meta:  ${OUT_DIR}/*.json"

if [[ "${fail}" -gt 0 ]]; then
  exit 1
fi
