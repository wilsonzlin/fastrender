#!/usr/bin/env bash
set -euo pipefail

# Render offline page fixtures (`tests/pages/fixtures/*/index.html`) in headless Chrome/Chromium
# and write PNG screenshots.
#
# This is the deterministic/offline counterpart to `scripts/chrome_baseline.sh`:
# - This script is a thin wrapper around `cargo xtask chrome-baseline-fixtures`.
# - Targets self-contained fixtures (HTML + local assets) under `tests/pages/fixtures/`
# - Loads fixtures via `file://` and injects:
#   - `<base href="file://.../fixture/">` so patched HTML can live in a temp dir
#   - A CSP that disables JS by default (matching FastRender's no-JS model)
#   - A CSP `default-src file: data:` restriction so http/https subresources cannot load
#
# Outputs land in `target/chrome_fixture_renders/` by default (PNG + chrome log + JSON metadata).

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "${REPO_ROOT}"

usage() {
  cat <<'EOF'
usage: scripts/chrome_fixture_baseline.sh [options] [--] [fixture_glob...]

Options:
  --fixtures-dir <dir>  Fixture root (default: tests/pages/fixtures)
  --out-dir <dir>       Directory to write PNGs/logs (default: target/chrome_fixture_renders)
  --viewport <WxH>      Viewport size (default: 1040x1240)
  --dpr <float>         Device pixel ratio (default: 1.0)
  --timeout <secs>      Per-fixture hard timeout (default: 15)
  --chrome <path>       Chrome/Chromium binary (default: auto-detect)
  --js <on|off>         Enable JavaScript (default: off)
  -h, --help            Show help

Filtering:
  Positional arguments are treated as fixture directory globs (matched against
  <fixtures-dir>/<glob>/index.html). Example: grid_news, subgrid_*

  With no positional filters, defaults to the fixtures listed in
  tests/pages_regression_test.rs (fallback: all <fixtures-dir>/*/index.html).

Output:
  <out-dir>/<fixture>.png        Screenshot
  <out-dir>/<fixture>.chrome.log Chrome stdout/stderr for debugging
  <out-dir>/<fixture>.json       JSON metadata (viewport/DPR/JS/headless mode/etc.)

EOF
}

FIXTURES_DIR="${FIXTURES_DIR:-tests/pages/fixtures}"
OUT_DIR="${OUT_DIR:-target/chrome_fixture_renders}"
VIEWPORT="${VIEWPORT:-1040x1240}"
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
      --fixtures-dir)
        FIXTURES_DIR="${2:-}"; shift 2 ;;
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
  echo "invalid --viewport: ${VIEWPORT} (expected WxH like 1040x1240)" >&2
  exit 2
fi

case "${JS,,}" in
  on|off) ;;
  *)
    echo "invalid --js: ${JS} (expected on|off)" >&2
    exit 2
    ;;
esac

if [[ ! -d "${FIXTURES_DIR}" ]]; then
  echo "Fixtures dir not found: ${FIXTURES_DIR}" >&2
  exit 1
fi

mkdir -p "${OUT_DIR}"

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

echo "Fixtures: ${FIXTURES_DIR}"
echo "Output:   ${OUT_DIR}"
echo "Viewport: ${VIEWPORT}  DPR: ${DPR}  JS: ${JS,,}  Timeout: ${TIMEOUT}s"
echo

xtask_args=(
  chrome-baseline-fixtures
  --fixture-dir "${FIXTURES_DIR}"
  --out-dir "${OUT_DIR}"
  --viewport "${VIEWPORT}"
  --dpr "${DPR}"
  --timeout "${TIMEOUT}"
  --js "${JS,,}"
)
if [[ -n "${CHROME_BIN}" ]]; then
  xtask_args+=(--chrome "${CHROME_BIN}")
fi
xtask_args+=(-- "${FIXTURES[@]}")

cargo xtask "${xtask_args[@]}"
