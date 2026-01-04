#!/usr/bin/env bash
set -euo pipefail

# One-command "cached HTML -> Chrome baseline -> FastRender -> diff report" loop.
#
# This workflow is intentionally best-effort and may be non-deterministic because cached pages can
# still load live subresources via the injected `<base href=...>` (or absolute URLs).

usage() {
  cat <<'EOF'
usage: scripts/chrome_vs_fastrender.sh [options] [--] [page_stem...]

Options:
  --viewport <WxH>     Viewport size (default: 1200x800)
  --dpr <float>        Device pixel ratio (default: 1.0)
  --jobs <n>           Parallelism forwarded to render_pages
  --timeout <secs>     Per-page hard timeout forwarded to both renderers (default: 15)
  --out-dir <dir>      Base output directory (default: target/chrome_vs_fastrender)
  --chrome <path>      Chrome/Chromium binary forwarded to chrome_baseline.sh
  --js <on|off>        JavaScript toggle forwarded to chrome_baseline.sh (default: off)

Compatibility / extras:
  --pages <csv>        Alias for passing stems as positional args (comma-separated). For values
                       that look like URLs, we attempt to normalize to cached stems so Chrome and
                       FastRender render the same set.
  --shard <index>/<total>
                       Only process a deterministic shard of the selected cached pages (0-based)
  --chrome-timeout <s> Override Chrome timeout only (default: --timeout)
  --render-timeout <s> Override FastRender timeout only (default: --timeout)
  --out <dir>          Alias for --out-dir
  --no-chrome          Reuse existing <out>/chrome output (skip chrome_baseline.sh)
  --no-fastrender      Reuse existing <out>/fastrender output (skip render_pages)
  --tolerance <u8>     Forwarded to diff_renders
  --max-diff-percent <f64>
                         Forwarded to diff_renders
  --max-perceptual-distance <f64>
                         Forwarded to diff_renders
  --ignore-alpha       Forwarded to diff_renders (ignore alpha differences)
  --sort-by <mode>     Forwarded to diff_renders (pixel|percent|perceptual)
  --fail-on-differences
                        Exit non-zero when diff_renders reports differences (default: keep report and exit 0)

Output layout:
  <out>/chrome/        Chrome PNGs/logs
  <out>/fastrender/    FastRender PNGs/logs/diagnostics
  <out>/report.html    diff_renders HTML report
  <out>/report.json    diff_renders JSON report

Filtering:
  Positional args are treated as cached page stems (from fetches/html/*.html) and forwarded to both
  renderers. Use `--` to pass stems that begin with `-`.
EOF
}

VIEWPORT="1200x800"
DPR="1.0"
JOBS=""
TIMEOUT="15"
CHROME_TIMEOUT=""
RENDER_TIMEOUT=""
OUT_DIR="target/chrome_vs_fastrender"
CHROME_BIN=""
JS="off"
NO_CHROME=0
NO_FASTRENDER=0
TOLERANCE=""
MAX_DIFF_PERCENT=""
MAX_PERCEPTUAL_DISTANCE=""
IGNORE_ALPHA=0
SORT_BY=""
FAIL_ON_DIFFERENCES=0
SHARD=""
PAGES_CSV=""

FILTERS=()
PARSE_FLAGS=1
while [[ $# -gt 0 ]]; do
  if [[ "${PARSE_FLAGS}" -eq 1 ]]; then
    case "$1" in
      -h|--help)
        usage
        exit 0
        ;;
      --viewport)
        VIEWPORT="${2:-}"; shift 2; continue ;;
      --dpr)
        DPR="${2:-}"; shift 2; continue ;;
      --jobs)
        JOBS="${2:-}"; shift 2; continue ;;
      --timeout)
        TIMEOUT="${2:-}"; shift 2; continue ;;
      --chrome-timeout)
        CHROME_TIMEOUT="${2:-}"; shift 2; continue ;;
      --render-timeout)
        RENDER_TIMEOUT="${2:-}"; shift 2; continue ;;
      --out-dir|--out)
        OUT_DIR="${2:-}"; shift 2; continue ;;
      --no-chrome)
        NO_CHROME=1; shift; continue ;;
      --no-fastrender)
        NO_FASTRENDER=1; shift; continue ;;
      --chrome)
        CHROME_BIN="${2:-}"; shift 2; continue ;;
      --js)
        JS="${2:-}"; shift 2; continue ;;
      --tolerance)
        TOLERANCE="${2:-}"; shift 2; continue ;;
      --max-diff-percent)
        MAX_DIFF_PERCENT="${2:-}"; shift 2; continue ;;
      --max-perceptual-distance)
        MAX_PERCEPTUAL_DISTANCE="${2:-}"; shift 2; continue ;;
      --ignore-alpha)
        IGNORE_ALPHA=1; shift; continue ;;
      --sort-by)
        SORT_BY="${2:-}"; shift 2; continue ;;
      --fail-on-differences)
        FAIL_ON_DIFFERENCES=1; shift; continue ;;
      --pages)
        PAGES_CSV="${2:-}"; shift 2; continue ;;
      --shard)
        SHARD="${2:-}"; shift 2; continue ;;
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

if [[ -z "${CHROME_TIMEOUT}" ]]; then
  CHROME_TIMEOUT="${TIMEOUT}"
fi
if [[ -z "${RENDER_TIMEOUT}" ]]; then
  RENDER_TIMEOUT="${TIMEOUT}"
fi

if [[ -z "${OUT_DIR}" || "${OUT_DIR}" == "/" ]]; then
  echo "refusing to use unsafe --out-dir: ${OUT_DIR}" >&2
  exit 2
fi

if ! [[ "${VIEWPORT}" =~ ^[0-9]+x[0-9]+$ ]]; then
  echo "invalid --viewport: ${VIEWPORT} (expected WxH like 1200x800)" >&2
  exit 2
fi

if [[ -n "${JOBS}" ]]; then
  if ! [[ "${JOBS}" =~ ^[0-9]+$ ]] || [[ "${JOBS}" -lt 1 ]]; then
    echo "invalid --jobs: ${JOBS} (expected integer >= 1)" >&2
    exit 2
  fi
fi

for secs in "${TIMEOUT}" "${CHROME_TIMEOUT}" "${RENDER_TIMEOUT}"; do
  if ! [[ "${secs}" =~ ^[0-9]+$ ]] || [[ "${secs}" -lt 1 ]]; then
    echo "invalid timeout: ${secs} (expected integer >= 1)" >&2
    exit 2
  fi
done

case "${JS,,}" in
  on|off) ;;
  *)
    echo "invalid --js: ${JS} (expected on|off)" >&2
    exit 2
    ;;
esac

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "${ROOT}"

if [[ -n "${PAGES_CSV}" ]]; then
  if ! command -v python3 >/dev/null 2>&1; then
    echo "python3 is required when using --pages (used for best-effort stem normalization)." >&2
    exit 2
  fi
  mapfile -t NORMALIZED_PAGES < <(python3 - "${PAGES_CSV}" <<'PY'
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
  FILTERS+=("${NORMALIZED_PAGES[@]}")
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

  AVAILABLE_STEMS=()
  for html in "${HTML_FILES[@]}"; do
    AVAILABLE_STEMS+=("$(basename "${html}" .html)")
  done

  MATCHED=()
  if [[ "${#FILTERS[@]}" -gt 0 ]]; then
    declare -A AVAILABLE=()
    for stem in "${AVAILABLE_STEMS[@]}"; do
      AVAILABLE["${stem}"]=1
    done
    for stem in "${FILTERS[@]}"; do
      if [[ -n "${AVAILABLE[${stem}]:-}" ]]; then
        MATCHED+=("${stem}")
      fi
    done
  else
    MATCHED=("${AVAILABLE_STEMS[@]}")
  fi

  mapfile -t MATCHED_SORTED < <(printf '%s\n' "${MATCHED[@]}" | sort -u)
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
fi

CHROME_OUT="${OUT_DIR}/chrome"
FASTRENDER_OUT="${OUT_DIR}/fastrender"
REPORT_HTML="${OUT_DIR}/report.html"
REPORT_JSON="${OUT_DIR}/report.json"

mkdir -p "${OUT_DIR}"
if [[ "${NO_CHROME}" -eq 1 ]]; then
  if [[ ! -d "${CHROME_OUT}" ]]; then
    echo "--no-chrome was set, but Chrome output dir does not exist: ${CHROME_OUT}" >&2
    exit 1
  fi
else
  rm -rf "${CHROME_OUT}"
  mkdir -p "${CHROME_OUT}"
fi

if [[ "${NO_FASTRENDER}" -eq 1 ]]; then
  if [[ ! -d "${FASTRENDER_OUT}" ]]; then
    echo "--no-fastrender was set, but FastRender output dir does not exist: ${FASTRENDER_OUT}" >&2
    exit 1
  fi
else
  rm -rf "${FASTRENDER_OUT}"
  mkdir -p "${FASTRENDER_OUT}"
fi

chrome_args=(
  --out-dir "${CHROME_OUT}"
  --viewport "${VIEWPORT}"
  --dpr "${DPR}"
  --timeout "${CHROME_TIMEOUT}"
  --js "${JS}"
)
if [[ -n "${CHROME_BIN}" ]]; then
  chrome_args+=(--chrome "${CHROME_BIN}")
fi

render_args=(
  --out-dir "${FASTRENDER_OUT}"
  --viewport "${VIEWPORT}"
  --dpr "${DPR}"
  --timeout "${RENDER_TIMEOUT}"
)
if [[ -n "${JOBS}" ]]; then
  render_args+=(--jobs "${JOBS}")
fi

diff_args=(
  --before "${CHROME_OUT}"
  --after "${FASTRENDER_OUT}"
  --html "${REPORT_HTML}"
  --json "${REPORT_JSON}"
)
if [[ -n "${TOLERANCE}" ]]; then
  diff_args+=(--tolerance "${TOLERANCE}")
fi
if [[ -n "${MAX_DIFF_PERCENT}" ]]; then
  diff_args+=(--max-diff-percent "${MAX_DIFF_PERCENT}")
fi
if [[ -n "${MAX_PERCEPTUAL_DISTANCE}" ]]; then
  diff_args+=(--max-perceptual-distance "${MAX_PERCEPTUAL_DISTANCE}")
fi
if [[ "${IGNORE_ALPHA}" -eq 1 ]]; then
  diff_args+=(--ignore-alpha)
fi
if [[ -n "${SORT_BY}" ]]; then
  diff_args+=(--sort-by "${SORT_BY}")
fi

echo "Output: ${OUT_DIR}"
echo "Viewport: ${VIEWPORT}  DPR: ${DPR}  JS: ${JS,,}"
if [[ "${#FILTERS[@]}" -gt 0 ]]; then
  if [[ -n "${SHARD}" ]]; then
    echo "Pages: ${#FILTERS[@]} pages (shard ${SHARD})"
  else
    echo "Pages: ${FILTERS[*]}"
  fi
else
  echo "Pages: (all cached)"
fi
echo

echo "==> Chrome baseline: ${CHROME_OUT}"
chrome_status=0
if [[ "${NO_CHROME}" -eq 1 ]]; then
  echo "(skipping chrome_baseline.sh; reusing ${CHROME_OUT})"
else
  set +e
  if [[ "${#FILTERS[@]}" -gt 0 ]]; then
    scripts/chrome_baseline.sh "${chrome_args[@]}" -- "${FILTERS[@]}"
  else
    scripts/chrome_baseline.sh "${chrome_args[@]}"
  fi
  chrome_status=$?
  set -e
fi

echo
echo "==> FastRender: ${FASTRENDER_OUT}"
fastrender_status=0
if [[ "${NO_FASTRENDER}" -eq 1 ]]; then
  echo "(skipping render_pages; reusing ${FASTRENDER_OUT})"
else
  set +e
  if [[ "${#FILTERS[@]}" -gt 0 ]]; then
    cargo run --release --bin render_pages -- "${render_args[@]}" -- "${FILTERS[@]}"
  else
    cargo run --release --bin render_pages -- "${render_args[@]}"
  fi
  fastrender_status=$?
  set -e
fi

echo
echo "==> Diff report: ${REPORT_HTML}"

TARGET_DIR="${CARGO_TARGET_DIR:-target}"
if [[ "${TARGET_DIR}" != /* ]]; then
  TARGET_DIR="${ROOT}/${TARGET_DIR}"
fi
DIFF_BIN="${TARGET_DIR}/release/diff_renders"
if [[ -f "${DIFF_BIN}.exe" ]]; then
  DIFF_BIN="${DIFF_BIN}.exe"
fi

set +e
cargo build --release --bin diff_renders
"${DIFF_BIN}" "${diff_args[@]}"
diff_status=$?
set -e

if [[ "${diff_status}" -eq 1 && "${FAIL_ON_DIFFERENCES}" -eq 0 ]]; then
  if [[ -f "${REPORT_JSON}" ]]; then
    echo "diff_renders reported differences; keeping report and exiting 0 (pass --fail-on-differences to fail)." >&2
    diff_status=0
  fi
fi

echo
echo "Report: ${REPORT_HTML}"
if [[ "${chrome_status}" -ne 0 ]]; then
  echo "Warning: chrome_baseline.sh exited with ${chrome_status}" >&2
fi
if [[ "${fastrender_status}" -ne 0 ]]; then
  echo "Warning: render_pages exited with ${fastrender_status}" >&2
fi

exit_code=0
if [[ "${chrome_status}" -ne 0 ]]; then
  exit_code="${chrome_status}"
elif [[ "${fastrender_status}" -ne 0 ]]; then
  exit_code="${fastrender_status}"
elif [[ "${diff_status}" -ne 0 ]]; then
  exit_code="${diff_status}"
fi
exit "${exit_code}"
