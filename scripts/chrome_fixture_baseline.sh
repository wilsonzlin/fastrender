#!/usr/bin/env bash
set -euo pipefail

# Render offline page fixtures (`tests/pages/fixtures/*/index.html`) in headless Chrome/Chromium
# and write PNG screenshots.
#
# This is the deterministic/offline counterpart to `scripts/chrome_baseline.sh`:
# - Targets self-contained fixtures (HTML + local assets) under `tests/pages/fixtures/`
# - Loads fixtures via `file://` and injects:
#   - `<base href="file://.../fixture/">` so patched HTML can live in a temp dir
#   - A CSP that disables JS by default (matching FastRender's no-JS model)
#   - A CSP `default-src file: data:` restriction so http/https subresources cannot load
#
# Outputs land in `target/chrome_fixture_renders/` by default.

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
  echo "python3 is required for HTML patching." >&2
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

if [[ ! -d "${FIXTURES_DIR}" ]]; then
  echo "Fixtures dir not found: ${FIXTURES_DIR}" >&2
  exit 1
fi

mkdir -p "${OUT_DIR}"

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
    TMP_TEMPLATE="${SNAP_COMMON_DIR}/fastrender-chrome-fixtures.XXXXXX"
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

discover_default_fixtures() {
  local out=()
  if [[ -f tests/pages_regression_test.rs ]]; then
    # Extract `html: "name/index.html"` entries from the regression suite.
    mapfile -t out < <(python3 - <<'PY'
import re
from pathlib import Path

src = Path("tests/pages_regression_test.rs")
try:
    text = src.read_text(encoding="utf-8")
except Exception:
    raise SystemExit(0)

paths = re.findall(r'html:\s*"([^"]+)"', text)
seen = set()
names = []
for p in paths:
    name = p.split("/", 1)[0]
    if name and name not in seen:
        seen.add(name)
        names.append(name)
for n in names:
    print(n)
PY
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

fail=0
ok=0

echo "Chrome: ${CHROME}"
echo "Fixtures: ${FIXTURES_DIR}"
echo "Output:   ${OUT_DIR}"
echo "Viewport: ${VIEWPORT}  DPR: ${DPR}  JS: ${JS,,}  Timeout: ${TIMEOUT}s"
echo

patched_dir="${TMP_ROOT}/html"
mkdir -p "${patched_dir}"

tmp_png_dir="${TMP_ROOT}/screenshots"
mkdir -p "${tmp_png_dir}"

for fixture in "${FIXTURES[@]}"; do
  fixture_dir="${FIXTURES_DIR}/${fixture}"
  html_path="${fixture_dir}/index.html"
  if [[ ! -f "${html_path}" ]]; then
    echo "✗ ${fixture} (missing ${html_path})" >&2
    fail=$((fail + 1))
    continue
  fi

  fixture_abs="$(python3 -c 'import pathlib,sys; print(pathlib.Path(sys.argv[1]).resolve())' "${fixture_dir}")"
  base_url="file://${fixture_abs}/"

  patched_html="${patched_dir}/${fixture}.html"

  disable_js="0"
  if [[ "${JS,,}" == "off" ]]; then
    disable_js="1"
  fi

  python3 - "${html_path}" "${patched_html}" "${base_url}" "${disable_js}" <<'PY'
import sys

in_path = sys.argv[1]
out_path = sys.argv[2]
base_url = sys.argv[3].strip()
disable_js = False
if len(sys.argv) >= 5:
    disable_js = sys.argv[4].strip() == "1"

data = open(in_path, "rb").read()
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

inserts = []
if base_url:
    inserts.append(f'<base href="{base_url}">'.encode("utf-8") + b"\n")

# Enforce a deterministic/offline page load: allow only file/data subresources.
# If JS is enabled, allow inline/file scripts for experimentation; otherwise block scripts.
if disable_js:
    csp = "default-src file: data:; style-src file: data: 'unsafe-inline'; script-src 'none';"
else:
    csp = "default-src file: data:; style-src file: data: 'unsafe-inline'; script-src file: data: 'unsafe-inline' 'unsafe-eval';"
inserts.append(f'<meta http-equiv="Content-Security-Policy" content="{csp}">'.encode("utf-8") + b"\n")

insertion = b"".join(inserts)

out = insert_after_tag(b"<head", insertion)
if out is None:
    out = insert_after_tag(b"<html", b"<head>\n" + insertion + b"</head>\n")
if out is None:
    out = b"<head>\n" + insertion + b"</head>\n" + data

open(out_path, "wb").write(out)
PY

  url="file://${patched_html}"
  png_path="${OUT_DIR}/${fixture}.png"
  chrome_log="${OUT_DIR}/${fixture}.chrome.log"
  tmp_png_path="${tmp_png_dir}/${fixture}.png"

  profile_dir="${TMP_ROOT}/profile-${fixture}"
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
    --disable-background-networking
    --no-first-run
    --no-default-browser-check
    --disable-component-update
    --disable-default-apps
    --disable-sync
    --host-resolver-rules="MAP * ~NOTFOUND, EXCLUDE localhost"
    --user-data-dir="${profile_dir}"
    # Always write the screenshot to a temp directory and then copy it into OUT_DIR.
    --screenshot="${tmp_png_path}"
  )

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

  if [[ "${ran_ok}" -eq 1 && -s "${tmp_png_path}" ]]; then
    cp -f "${tmp_png_path}" "${png_path}"
    ok=$((ok + 1))
    echo "✓ ${fixture}"
  else
    fail=$((fail + 1))
    if [[ "${ran_ok}" -eq 1 ]]; then
      echo "✗ ${fixture} (no screenshot produced; see ${chrome_log})" >&2
    else
      echo "✗ ${fixture} (failed; see ${chrome_log})" >&2
    fi
  fi
done

echo
echo "Done: ${ok} ok, ${fail} failed (out of ${#FIXTURES[@]})"
echo "PNGs:  ${OUT_DIR}/*.png"
echo "Logs:  ${OUT_DIR}/*.chrome.log"

if [[ "${fail}" -gt 0 ]]; then
  exit 1
fi

