#!/usr/bin/env bash
set -euo pipefail

# Run any command under OS-enforced resource limits.
#
# Prefer `prlimit` when available (hard limits). Fall back to `ulimit` otherwise.
#
# Examples:
#   scripts/run_limited.sh --as 8G --cpu 60 -- cargo bench --bench selector_bloom_bench
#   LIMIT_AS=12G scripts/run_limited.sh -- cargo run --release --bin pageset_progress -- run --timeout 5

usage() {
  cat <<'EOF'
usage: scripts/run_limited.sh [--as <size>] [--rss <size>] [--stack <size>] [--cpu <secs>] -- <command...>

Limits:
  --as <size>     Address-space (virtual memory) limit. Example: 8G, 4096M.
  --rss <size>    Resident set size limit (advisory on many kernels).
  --stack <size>  Stack size limit.
  --cpu <secs>    CPU time limit (seconds).

Environment defaults (optional):
  LIMIT_AS, LIMIT_RSS, LIMIT_STACK, LIMIT_CPU

Notes:
  - `--as` is the most reliable “hard memory ceiling” on Linux.
  - If `prlimit` is missing, we fall back to `ulimit`. In that mode, size strings without a
    suffix are interpreted as MiB.
EOF
}

to_kib() {
  local raw="${1:-}"
  raw="${raw//[[:space:]]/}"
  raw="${raw,,}"

  # Accept common suffixes: k, m, g, t (optionally with b/ib).
  raw="${raw%ib}"
  raw="${raw%b}"

  if [[ "${raw}" =~ ^[0-9]+$ ]]; then
    # Fallback: treat as MiB (human-friendly for ulimit -v/-s which expect KiB).
    echo $((raw * 1024))
    return 0
  fi

  if [[ "${raw}" =~ ^([0-9]+)([kmgt])$ ]]; then
    local n="${BASH_REMATCH[1]}"
    local unit="${BASH_REMATCH[2]}"
    case "${unit}" in
      k) echo $((n)) ;;
      m) echo $((n * 1024)) ;;
      g) echo $((n * 1024 * 1024)) ;;
      t) echo $((n * 1024 * 1024 * 1024)) ;;
      *) return 1 ;;
    esac
    return 0
  fi

  return 1
}

AS="${LIMIT_AS:-8G}"
RSS="${LIMIT_RSS:-}"
STACK="${LIMIT_STACK:-}"
CPU="${LIMIT_CPU:-}"

while [[ $# -gt 0 ]]; do
  case "$1" in
    -h|--help)
      usage
      exit 0
      ;;
    --as)
      AS="${2:-}"; shift 2 ;;
    --rss)
      RSS="${2:-}"; shift 2 ;;
    --stack)
      STACK="${2:-}"; shift 2 ;;
    --cpu)
      CPU="${2:-}"; shift 2 ;;
    --no-as)
      AS=""; shift ;;
    --no-rss)
      RSS=""; shift ;;
    --no-stack)
      STACK=""; shift ;;
    --no-cpu)
      CPU=""; shift ;;
    --)
      shift
      break
      ;;
    *)
      # No more wrapper flags; treat rest as the command.
      break
      ;;
  esac
done

if [[ $# -lt 1 ]]; then
  usage
  exit 2
fi

cmd=("$@")

if command -v prlimit >/dev/null 2>&1; then
  pl=(prlimit)
  if [[ -n "${AS}" && "${AS}" != "0" ]]; then
    pl+=(--as="${AS}")
  fi
  if [[ -n "${RSS}" && "${RSS}" != "0" ]]; then
    pl+=(--rss="${RSS}")
  fi
  if [[ -n "${STACK}" && "${STACK}" != "0" ]]; then
    pl+=(--stack="${STACK}")
  fi
  if [[ -n "${CPU}" && "${CPU}" != "0" ]]; then
    pl+=(--cpu="${CPU}")
  fi
  exec "${pl[@]}" -- "${cmd[@]}"
fi

# Fallback: ulimit. (Not all resources are enforceable; RSS is typically ignored.)
if [[ -n "${AS}" && "${AS}" != "0" ]]; then
  as_kib="$(to_kib "${AS}")" || {
    echo "invalid --as size: ${AS}" >&2
    exit 2
  }
  ulimit -v "${as_kib}"
fi
if [[ -n "${STACK}" && "${STACK}" != "0" ]]; then
  stack_kib="$(to_kib "${STACK}")" || {
    echo "invalid --stack size: ${STACK}" >&2
    exit 2
  }
  ulimit -s "${stack_kib}"
fi
if [[ -n "${CPU}" && "${CPU}" != "0" ]]; then
  if ! [[ "${CPU}" =~ ^[0-9]+$ ]]; then
    echo "invalid --cpu seconds: ${CPU}" >&2
    exit 2
  fi
  ulimit -t "${CPU}"
fi

exec "${cmd[@]}"

