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
#     --before fetches/chrome_renders --after fetches/renders --html target/chrome_vs_fastrender.html
#
# Notes:
# - This script does NOT make the run fully deterministic (live subresources can change).
# - It tries to be robust in container/CI-like environments by passing common headless flags.

usage() {
  cat <<'EOF'
usage: scripts/chrome_baseline.sh [options] [--] [page_stem...]

Options:
  --html-dir <dir>     Directory containing cached HTML (default: fetches/html)
  --out-dir <dir>      Directory to write PNGs/logs (default: fetches/chrome_renders)
  --viewport <WxH>     Viewport size (default: 1200x800)
  --dpr <float>        Device pixel ratio (default: 1.0)
  --timeout <secs>     Per-page hard timeout (default: 15)
  --chrome <path>      Chrome/Chromium binary (default: auto-detect)
  --js <on|off>        Enable JavaScript (default: off)
  -h, --help           Show help

Filtering:
  If you pass positional arguments, they are treated as cache stems (file stems),
  and only those pages will be rendered.

Environment (optional):
  HTML_DIR, OUT_DIR, VIEWPORT, DPR, TIMEOUT, CHROME_BIN, JS

Output:
  <out-dir>/<stem>.png        Screenshot
  <out-dir>/<stem>.chrome.log Chrome stdout/stderr for debugging

EOF
}

HTML_DIR="${HTML_DIR:-fetches/html}"
OUT_DIR="${OUT_DIR:-fetches/chrome_renders}"
VIEWPORT="${VIEWPORT:-1200x800}"
DPR="${DPR:-1.0}"
TIMEOUT="${TIMEOUT:-15}"
CHROME_BIN="${CHROME_BIN:-}"
JS="${JS:-off}"

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

TMP_ROOT="$(mktemp -d)"
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

fail=0
ok=0
total=0

echo "Chrome: ${CHROME}"
echo "Input:  ${HTML_DIR}"
echo "Output: ${OUT_DIR}"
echo "Viewport: ${VIEWPORT}  DPR: ${DPR}  JS: ${JS,,}  Timeout: ${TIMEOUT}s"
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

  python3 - "${html_path}" "${patched_html}" "${base_url}" <<'PY'
import sys

in_path = sys.argv[1]
out_path = sys.argv[2]
base_url = sys.argv[3].strip()

data = open(in_path, "rb").read()
if not base_url:
    open(out_path, "wb").write(data)
    sys.exit(0)

lower = data.lower()

def insert_after_tag(tag: bytes, insertion: bytes):
    idx = lower.find(tag)
    if idx == -1:
        return None
    end = lower.find(b">", idx)
    if end == -1:
        return None
    end += 1
    return data[:end] + b"\n" + insertion + data[end:]

base_tag = (f'<base href="{base_url}">'.encode("utf-8") + b"\n")

out = insert_after_tag(b"<head", base_tag)
if out is None:
    out = insert_after_tag(b"<html", b"<head>\n" + base_tag + b"</head>\n")
if out is None:
    out = b"<head>\n" + base_tag + b"</head>\n" + data

open(out_path, "wb").write(out)
PY

  url="file://${patched_html}"
  png_path="${OUT_DIR}/${stem}.png"
  chrome_log="${OUT_DIR}/${stem}.chrome.log"

  profile_dir="${TMP_ROOT}/profile-${stem}"
  mkdir -p "${profile_dir}"

  chrome_args=(
    --headless=new
    --no-sandbox
    --disable-dev-shm-usage
    --disable-gpu
    --hide-scrollbars
    --window-size="${VIEWPORT_W},${VIEWPORT_H}"
    --force-device-scale-factor="${DPR}"
    --disable-web-security
    --allow-file-access-from-files
    --user-data-dir="${profile_dir}"
    --screenshot="${png_path}"
  )

  if [[ "${JS,,}" == "off" ]]; then
    chrome_args+=(--blink-settings=scriptEnabled=false)
  fi

  # Use `timeout` if available; otherwise run without a hard kill.
  if command -v timeout >/dev/null 2>&1; then
    if timeout "${TIMEOUT}s" "${CHROME}" "${chrome_args[@]}" "${url}" >"${chrome_log}" 2>&1; then
      ok=$((ok + 1))
      echo "✓ ${stem}"
    else
      fail=$((fail + 1))
      echo "✗ ${stem} (failed; see ${chrome_log})" >&2
    fi
  else
    if "${CHROME}" "${chrome_args[@]}" "${url}" >"${chrome_log}" 2>&1; then
      ok=$((ok + 1))
      echo "✓ ${stem}"
    else
      fail=$((fail + 1))
      echo "✗ ${stem} (failed; see ${chrome_log})" >&2
    fi
  fi
done

echo
echo "Done: ${ok} ok, ${fail} failed (out of ${total})"
echo "PNGs:  ${OUT_DIR}/*.png"
echo "Logs:  ${OUT_DIR}/*.chrome.log"

if [[ "${fail}" -gt 0 ]]; then
  exit 1
fi

