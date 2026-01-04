#!/usr/bin/env bash
set -euo pipefail

# One-command offline fixture diff loop:
#   1) Capture Chrome baselines for local `tests/pages/fixtures/*/index.html` pages
#   2) Render the same fixtures with FastRender (`render_fixtures`)
#   3) Diff the two directories (`diff_renders`) and write an HTML report

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "${REPO_ROOT}"

usage() {
  cat <<'EOF'
usage: scripts/chrome_vs_fastrender_fixtures.sh [options] [--] [fixture_glob...]

Options:
  --fixtures-dir <dir>      Fixture root (default: tests/pages/fixtures)
  --chrome-out-dir <dir>    Chrome PNG/log output (default: target/chrome_fixture_renders)
  --fastr-out-dir <dir>     FastRender PNG/log output (default: target/fastrender_fixture_renders)
  --report-html <path>      HTML diff report output (default: target/chrome_vs_fastrender_fixtures.html)
  --report-json <path>      JSON diff report output (default: target/chrome_vs_fastrender_fixtures.json)
  --viewport <WxH>          Viewport size (default: 1040x1240)
  --dpr <float>             Device pixel ratio (default: 1.0)
  --media <screen|print>    Media type for FastRender (default: screen)
  --timeout <secs>          Per-fixture timeout (Chrome + FastRender) (default: 15)
  --chrome <path>           Chrome/Chromium binary (default: auto-detect)
  --js <on|off>             Enable JavaScript in Chrome (default: off)
  --tolerance <0-255>       Pixel diff tolerance (passed to diff_renders)
  --max-diff-percent <f64>  Allowed diff percent (passed to diff_renders)
  --max-perceptual-distance <f64>
                           Allowed perceptual distance (passed to diff_renders)
  --no-clean                Do not delete previous output dirs under target/
  -h, --help                Show help

Filtering:
  Positional args are fixture directory globs (same matching as chrome_fixture_baseline.sh).
  If omitted, defaults to the fixtures listed in tests/pages_regression_test.rs.

EOF
}

FIXTURES_DIR="${FIXTURES_DIR:-tests/pages/fixtures}"
CHROME_OUT_DIR="${CHROME_OUT_DIR:-target/chrome_fixture_renders}"
FASTR_OUT_DIR="${FASTR_OUT_DIR:-target/fastrender_fixture_renders}"
REPORT_HTML="${REPORT_HTML:-target/chrome_vs_fastrender_fixtures.html}"
REPORT_JSON="${REPORT_JSON:-target/chrome_vs_fastrender_fixtures.json}"
VIEWPORT="${VIEWPORT:-1040x1240}"
DPR="${DPR:-1.0}"
MEDIA="${MEDIA:-screen}"
TIMEOUT="${TIMEOUT:-15}"
CHROME_BIN="${CHROME_BIN:-}"
JS="${JS:-off}"
TOLERANCE=""
MAX_DIFF_PERCENT=""
MAX_PERCEPTUAL_DISTANCE=""
CLEAN=1

FILTERS=()
PARSE_FLAGS=1
while [[ $# -gt 0 ]]; do
  if [[ "${PARSE_FLAGS}" -eq 1 ]]; then
    case "$1" in
      -h|--help)
        usage
        exit 0
        ;;
      --fixtures-dir)
        FIXTURES_DIR="${2:-}"; shift 2 ;;
      --chrome-out-dir)
        CHROME_OUT_DIR="${2:-}"; shift 2 ;;
      --fastr-out-dir)
        FASTR_OUT_DIR="${2:-}"; shift 2 ;;
      --report-html)
        REPORT_HTML="${2:-}"; shift 2 ;;
      --report-json)
        REPORT_JSON="${2:-}"; shift 2 ;;
      --viewport)
        VIEWPORT="${2:-}"; shift 2 ;;
      --dpr)
        DPR="${2:-}"; shift 2 ;;
      --media)
        MEDIA="${2:-}"; shift 2 ;;
      --timeout)
        TIMEOUT="${2:-}"; shift 2 ;;
      --chrome)
        CHROME_BIN="${2:-}"; shift 2 ;;
      --js)
        JS="${2:-}"; shift 2 ;;
      --tolerance)
        TOLERANCE="${2:-}"; shift 2 ;;
      --max-diff-percent)
        MAX_DIFF_PERCENT="${2:-}"; shift 2 ;;
      --max-perceptual-distance)
        MAX_PERCEPTUAL_DISTANCE="${2:-}"; shift 2 ;;
      --no-clean)
        CLEAN=0; shift ;;
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

refuse_unsafe_path() {
  local label="$1"
  local value="$2"
  if [[ -z "${value}" || "${value}" == "/" ]]; then
    echo "refusing to use unsafe ${label}: ${value}" >&2
    exit 2
  fi
}

refuse_unsafe_path "fixtures dir" "${FIXTURES_DIR}"
refuse_unsafe_path "chrome out dir" "${CHROME_OUT_DIR}"
refuse_unsafe_path "fastrender out dir" "${FASTR_OUT_DIR}"
refuse_unsafe_path "report html path" "${REPORT_HTML}"
refuse_unsafe_path "report json path" "${REPORT_JSON}"

discover_default_fixtures() {
  local out=()
  if [[ -f tests/pages_regression_test.rs ]]; then
    # Extract `html: "name/index.html"` entries from the regression suite.
    mapfile -t out < <(
      (
        grep -Eo 'html:[[:space:]]*"[^"]+"' tests/pages_regression_test.rs 2>/dev/null || true
      ) | sed -E 's/.*"([^"]+)".*/\1/' \
        | awk -F/ '{print $1}' \
        | awk '!seen[$0]++'
    )
  fi

  if [[ "${#out[@]}" -eq 0 ]]; then
    # Fallback: all fixture dirs containing index.html at depth 1.
    mapfile -t out < <(find "${FIXTURES_DIR}" -mindepth 2 -maxdepth 2 -type f -name index.html -print \
      | sed -E 's#/index\\.html$##' \
      | xargs -n1 basename \
      | sort -u)
  fi

  printf '%s\n' "${out[@]}"
}

resolve_fixtures() {
  local -a patterns=("$@")
  local -a fixtures=()
  declare -A seen=()

  if [[ "${#patterns[@]}" -eq 0 ]]; then
    mapfile -t fixtures < <(discover_default_fixtures)
  else
    shopt -s nullglob
    for pat in "${patterns[@]}"; do
      local matched=0
      for dir in "${FIXTURES_DIR}"/${pat}; do
        if [[ -d "${dir}" && -f "${dir}/index.html" ]]; then
          local name
          name="$(basename "${dir}")"
          if [[ -z "${seen[${name}]:-}" ]]; then
            seen["${name}"]=1
            fixtures+=("${name}")
          fi
          matched=1
        fi
      done
      if [[ "${matched}" -eq 0 ]]; then
        echo "no fixtures matched pattern: ${pat}" >&2
        exit 1
      fi
    done
  fi

  printf '%s\n' "${fixtures[@]}" | sort -u
}

mapfile -t FIXTURES < <(resolve_fixtures "${FILTERS[@]}")
if [[ "${#FIXTURES[@]}" -eq 0 ]]; then
  echo "No fixtures selected." >&2
  exit 1
fi

if [[ "${CLEAN}" -eq 1 ]]; then
  rm -rf "${CHROME_OUT_DIR}" "${FASTR_OUT_DIR}"
fi

chrome_status=0
fastr_status=0
diff_status=0

echo "== Chrome baseline =="
chrome_args=(
  scripts/chrome_fixture_baseline.sh
  --fixtures-dir "${FIXTURES_DIR}"
  --out-dir "${CHROME_OUT_DIR}"
  --viewport "${VIEWPORT}"
  --dpr "${DPR}"
  --timeout "${TIMEOUT}"
)
if [[ -n "${CHROME_BIN}" ]]; then
  chrome_args+=(--chrome "${CHROME_BIN}")
fi
chrome_args+=(--js "${JS}" -- "${FIXTURES[@]}")

if "${chrome_args[@]}"; then
  :
else
  chrome_status=$?
fi

echo
echo "== FastRender fixtures =="
if cargo run --release --bin render_fixtures -- \
  --fixtures-dir "${FIXTURES_DIR}" \
  --out-dir "${FASTR_OUT_DIR}" \
  --fixtures "$(IFS=,; echo "${FIXTURES[*]}")" \
  --viewport "${VIEWPORT}" \
  --dpr "${DPR}" \
  --media "${MEDIA}" \
  --timeout "${TIMEOUT}"; then
  :
else
  fastr_status=$?
fi

echo
echo "== Diff report =="
diff_args=(
  --before "${CHROME_OUT_DIR}"
  --after "${FASTR_OUT_DIR}"
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

TARGET_DIR="${CARGO_TARGET_DIR:-target}"
if [[ "${TARGET_DIR}" != /* ]]; then
  TARGET_DIR="${REPO_ROOT}/${TARGET_DIR}"
fi
DIFF_BIN="${TARGET_DIR}/release/diff_renders"
if [[ -f "${DIFF_BIN}.exe" ]]; then
  DIFF_BIN="${DIFF_BIN}.exe"
fi

if cargo build --release --bin diff_renders && "${DIFF_BIN}" "${diff_args[@]}"; then
  :
else
  diff_status=$?
fi

echo
echo "Outputs:"
echo "  Chrome PNGs:     ${CHROME_OUT_DIR}/"
echo "  FastRender PNGs: ${FASTR_OUT_DIR}/"
echo "  Diff report:     ${REPORT_HTML}"
echo "  Diff JSON:       ${REPORT_JSON}"

if [[ "${chrome_status}" -ne 0 || "${fastr_status}" -ne 0 || "${diff_status}" -ne 0 ]]; then
  exit 1
fi
