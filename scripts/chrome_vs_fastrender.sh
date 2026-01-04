#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
usage: scripts/chrome_vs_fastrender.sh [options]

Generate a cached-pages evidence bundle:
  fetches/html/*.html → Chrome screenshot → FastRender render_pages → diff_renders

Options:
  --pages <comma-separated>       Pages to render (e.g. example.com,github.com)
  --viewport <WxH>                Viewport size (default: 1200x800)
  --dpr <float>                   Device pixel ratio (default: 1.0)
  --chrome-timeout <secs>         Chrome per-page timeout (default: 15)
  --render-timeout <secs>         FastRender per-page timeout (default: 5)
  --out <dir>                     Output directory (default: target/chrome_vs_fastrender)

Diff options:
  --tolerance <u8>                Per-channel tolerance (default: 0)
  --max-diff-percent <f64>         Max differing pixel percent (default: 0.0)

Chrome options:
  --js <on|off>                   Enable JavaScript in Chrome (default: off)

  -h, --help                      Show help

Output layout:
  <out>/chrome/*.png
  <out>/fastrender/*.png
  <out>/diff_report.json
  <out>/diff_report.html

EOF
}

PAGES=""
VIEWPORT="1200x800"
DPR="1.0"
CHROME_TIMEOUT="15"
RENDER_TIMEOUT="5"
OUT_DIR="target/chrome_vs_fastrender"
TOLERANCE="0"
MAX_DIFF_PERCENT="0.0"
JS="off"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --pages)
      PAGES="${2:-}"; shift 2 ;;
    --viewport)
      VIEWPORT="${2:-}"; shift 2 ;;
    --dpr)
      DPR="${2:-}"; shift 2 ;;
    --chrome-timeout)
      CHROME_TIMEOUT="${2:-}"; shift 2 ;;
    --render-timeout)
      RENDER_TIMEOUT="${2:-}"; shift 2 ;;
    --out)
      OUT_DIR="${2:-}"; shift 2 ;;
    --tolerance)
      TOLERANCE="${2:-}"; shift 2 ;;
    --max-diff-percent)
      MAX_DIFF_PERCENT="${2:-}"; shift 2 ;;
    --js)
      JS="${2:-}"; shift 2 ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "unknown option: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
done

if ! [[ "${VIEWPORT}" =~ ^[0-9]+x[0-9]+$ ]]; then
  echo "invalid --viewport: ${VIEWPORT} (expected WxH like 1200x800)" >&2
  exit 2
fi

case "${JS,,}" in
  on|off) ;;
  *)
    echo "invalid --js: ${JS} (expected on|off)" >&2
    exit 2
    ;;
esac

if ! command -v python3 >/dev/null 2>&1; then
  echo "python3 is required (used by scripts/chrome_baseline.sh)." >&2
  exit 2
fi

if [[ ! -d fetches/html ]]; then
  echo "Cached HTML not found: fetches/html" >&2
  echo "Run: cargo run --release --bin fetch_pages" >&2
  exit 1
fi

shopt -s nullglob
HTML_FILES=(fetches/html/*.html)
if [[ "${#HTML_FILES[@]}" -eq 0 ]]; then
  echo "No cached HTML found under fetches/html/*.html" >&2
  echo "Run: cargo run --release --bin fetch_pages" >&2
  exit 1
fi

mkdir -p "${OUT_DIR}"

if [[ "${OUT_DIR}" == "/" || -z "${OUT_DIR}" ]]; then
  echo "refusing to use unsafe --out: ${OUT_DIR}" >&2
  exit 2
fi

rm -rf "${OUT_DIR}/chrome" "${OUT_DIR}/fastrender"

NORMALIZED_PAGES=()
PAGES_JOINED=""
if [[ -n "${PAGES}" ]]; then
  mapfile -t NORMALIZED_PAGES < <(python3 - "${PAGES}" <<'PY'
import sys
import urllib.parse

def sanitize_filename(inp: str) -> str:
    trimmed = inp.rstrip("/")
    sanitized = trimmed.replace("/", "_")
    sanitized = "".join(c if (c.isalnum() or c in "._-") else "_" for c in sanitized)
    result = sanitized.rstrip("_.")
    if not result:
        result = sanitized
    while result.endswith("_"):
        result = result[:-1]
    return result

def normalize_page_name(raw: str):
    trimmed = raw.strip()
    if not trimmed:
        return None

    try:
        parsed = urllib.parse.urlsplit(trimmed)
        if parsed.scheme and parsed.netloc:
            host = (parsed.hostname or "").lower()
            if host.startswith("www."):
                host = host[4:]
            host = host.rstrip(".")
            stem = host + (parsed.path or "")
            if parsed.query:
                stem += "?" + parsed.query
            return sanitize_filename(stem)
    except Exception:
        pass

    without_scheme = trimmed
    lowered = trimmed.lower()
    for scheme in ("https://", "http://"):
        if lowered.startswith(scheme):
            without_scheme = trimmed[len(scheme):]
            break

    without_www = without_scheme
    if without_www.lower().startswith("www."):
        without_www = without_www[4:]

    if "/" in without_www:
        host, rest = without_www.split("/", 1)
        rest = "/" + rest
    elif "_" in without_www:
        host, rest = without_www.split("_", 1)
        rest = "_" + rest
    else:
        host, rest = without_www, ""

    host = host.rstrip(".")
    lowered = host.lower() + rest
    no_fragment = lowered.split("#", 1)[0]
    return sanitize_filename(no_fragment)

raw = sys.argv[1]
out = []
for part in raw.split(","):
    norm = normalize_page_name(part)
    if norm:
        out.append(norm)
for item in out:
    print(item)
PY
  )
  if [[ "${#NORMALIZED_PAGES[@]}" -gt 0 ]]; then
    PAGES_JOINED="$(IFS=,; echo "${NORMALIZED_PAGES[*]}")"
  fi
fi

echo "Output: ${OUT_DIR}"
echo "Viewport: ${VIEWPORT}  DPR: ${DPR}"
if [[ -n "${PAGES_JOINED}" ]]; then
  echo "Pages: ${PAGES_JOINED}"
else
  echo "Pages: (all cached)"
fi
echo

echo "== Chrome baseline =="
chrome_cmd=(
  scripts/chrome_baseline.sh
  --out-dir "${OUT_DIR}/chrome"
  --viewport "${VIEWPORT}"
  --dpr "${DPR}"
  --timeout "${CHROME_TIMEOUT}"
  --js "${JS}"
)
if [[ "${#NORMALIZED_PAGES[@]}" -gt 0 ]]; then
  chrome_cmd+=(--)
  chrome_cmd+=("${NORMALIZED_PAGES[@]}")
fi
"${chrome_cmd[@]}"
echo

echo "== FastRender render_pages =="
render_cmd=(
  cargo run --release --bin render_pages --
  --timeout "${RENDER_TIMEOUT}"
  --viewport "${VIEWPORT}"
  --dpr "${DPR}"
  --cache-dir fetches/assets
  --out-dir "${OUT_DIR}/fastrender"
)
if [[ -n "${PAGES_JOINED}" ]]; then
  render_cmd+=(--pages "${PAGES_JOINED}")
fi
"${render_cmd[@]}"
echo

echo "== Diff =="
DIFF_STATUS=0
# `diff_renders` exits 1 when diffs are found; keep going so we can always print the report path,
# then propagate the exit code at the end for scripting/CI.

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DIFF_BIN="${REPO_ROOT}/target/release/diff_renders"

# Build the binary explicitly so cargo doesn't print a scary "process didn't exit successfully"
# message when exit code 1 indicates diffs (not an execution failure).
(
  cd "${REPO_ROOT}"
  cargo build --release --bin diff_renders
)

"${DIFF_BIN}" --before "${OUT_DIR}/chrome" \
  --after "${OUT_DIR}/fastrender" \
  --tolerance "${TOLERANCE}" \
  --max-diff-percent "${MAX_DIFF_PERCENT}" \
  --json "${OUT_DIR}/diff_report.json" \
  --html "${OUT_DIR}/diff_report.html" || DIFF_STATUS=$?

echo
echo "Report: ${OUT_DIR}/diff_report.html"
exit "${DIFF_STATUS}"
