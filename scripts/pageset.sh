#!/usr/bin/env bash
set -euo pipefail

# Determine the effective CPU budget for default `JOBS`/Rayon thread budgeting.
#
# We prefer `nproc` when available because it respects cpusets/affinity. On Linux, also honor cgroup
# CPU quotas when possible so the default parallelism doesn't oversubscribe in containers/CI.
detect_total_cpus() {
  local cpus
  if command -v nproc >/dev/null 2>&1; then
    cpus="$(nproc)"
  else
    cpus="$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 1)"
  fi

  # cgroup v2: `cpu.max` is "<quota> <period>" where quota is "max" when unbounded.
  #
  # Quotas are hierarchical: the effective CPU budget is the minimum quota found in the current
  # cgroup and all its ancestors. Walk upwards so we honor quotas applied at higher-level slices.
  if [[ -r /sys/fs/cgroup/cpu.max ]]; then
    local cgroup_path="/"
    if [[ -r /proc/self/cgroup ]]; then
      cgroup_path="$(awk -F: '$1=="0" && $2=="" {print $3; exit}' /proc/self/cgroup 2>/dev/null || echo "/")"
      if [[ -z "${cgroup_path}" ]]; then
        cgroup_path="/"
      fi
    fi

    local dir="/sys/fs/cgroup${cgroup_path}"
    if [[ "${dir}" == "/sys/fs/cgroup/" ]]; then
      dir="/sys/fs/cgroup"
    fi

    local best_quota_cpus=""
    while true; do
      if [[ -r "${dir}/cpu.max" ]]; then
        local quota period
        read -r quota period < "${dir}/cpu.max" || true
        if [[ "${quota:-}" != "max" && "${quota:-0}" -gt 0 && "${period:-0}" -gt 0 ]]; then
          local quota_cpus=$(((quota + period - 1) / period))
          if [[ "${quota_cpus}" -gt 0 ]]; then
            if [[ -z "${best_quota_cpus}" || "${quota_cpus}" -lt "${best_quota_cpus}" ]]; then
              best_quota_cpus="${quota_cpus}"
            fi
          fi
        fi
      fi

      if [[ "${dir}" == "/sys/fs/cgroup" ]]; then
        break
      fi

      local next_dir="${dir%/*}"
      if [[ -z "${next_dir}" || "${next_dir}" == "${dir}" ]]; then
        break
      fi
      dir="${next_dir}"
    done

    if [[ -n "${best_quota_cpus}" && "${best_quota_cpus}" -gt 0 && "${best_quota_cpus}" -lt "${cpus}" ]]; then
      cpus="${best_quota_cpus}"
    fi
  # cgroup v1: `cpu.cfs_quota_us` is -1 when unbounded.
  #
  # Like cgroup v2, quotas can be applied on ancestor cgroups. Parse the per-controller cgroup path
  # from `/proc/self/cgroup` and walk up to the mountpoint, taking the minimum quota observed.
  elif [[ -r /sys/fs/cgroup/cpu/cpu.cfs_quota_us || -r /sys/fs/cgroup/cpu,cpuacct/cpu.cfs_quota_us ]]; then
    local mountpoint=""
    for candidate in /sys/fs/cgroup/cpu /sys/fs/cgroup/cpu,cpuacct; do
      if [[ -r "${candidate}/cpu.cfs_quota_us" && -r "${candidate}/cpu.cfs_period_us" ]]; then
        mountpoint="${candidate}"
        break
      fi
    done

    if [[ -n "${mountpoint}" ]]; then
      local cgroup_path="/"
      if [[ -r /proc/self/cgroup ]]; then
        cgroup_path="$(awk -F: '$2 ~ /(^|,)cpu(,|$)/ {print $3; exit}' /proc/self/cgroup 2>/dev/null || echo "/")"
        if [[ -z "${cgroup_path}" ]]; then
          cgroup_path="/"
        fi
      fi

      local dir="${mountpoint}${cgroup_path}"
      if [[ "${dir}" == "${mountpoint}/" ]]; then
        dir="${mountpoint}"
      fi

      local best_quota_cpus=""
      while true; do
        if [[ -r "${dir}/cpu.cfs_quota_us" && -r "${dir}/cpu.cfs_period_us" ]]; then
          local quota period
          quota="$(cat "${dir}/cpu.cfs_quota_us" 2>/dev/null || echo -1)"
          period="$(cat "${dir}/cpu.cfs_period_us" 2>/dev/null || echo 0)"
          if [[ "${quota}" -gt 0 && "${period}" -gt 0 ]]; then
            local quota_cpus=$(((quota + period - 1) / period))
            if [[ "${quota_cpus}" -gt 0 ]]; then
              if [[ -z "${best_quota_cpus}" || "${quota_cpus}" -lt "${best_quota_cpus}" ]]; then
                best_quota_cpus="${quota_cpus}"
              fi
            fi
          fi
        fi

        if [[ "${dir}" == "${mountpoint}" ]]; then
          break
        fi

        local next_dir="${dir%/*}"
        if [[ -z "${next_dir}" || "${next_dir}" == "${dir}" ]]; then
          break
        fi
        dir="${next_dir}"
      done

      if [[ -n "${best_quota_cpus}" && "${best_quota_cpus}" -gt 0 && "${best_quota_cpus}" -lt "${cpus}" ]]; then
        cpus="${best_quota_cpus}"
      fi
    fi
  fi

  if [[ -z "${cpus}" || "${cpus}" -lt 1 ]]; then
    cpus=1
  fi
  echo "${cpus}"
}

# Convenience wrapper for the main planner loop:
#   fetch_pages -> prefetch_assets -> pageset_progress
#
# Environment overrides:
#   JOBS=8 FETCH_TIMEOUT=30 RENDER_TIMEOUT=5 DISK_CACHE=0 NO_DISK_CACHE=1
#   RAYON_NUM_THREADS=4 FASTR_LAYOUT_PARALLEL=off|on|auto
#   USER_AGENT=... ACCEPT_LANGUAGE=... VIEWPORT=... DPR=...
#   FASTR_DISK_CACHE_MAX_BYTES=... FASTR_DISK_CACHE_MAX_AGE_SECS=... (0 = never expire)
#   FASTR_DISK_CACHE_LOCK_STALE_SECS=... (seconds before `.lock` files are treated as stale)
#   FASTR_DISK_CACHE_ALLOW_NO_STORE=0|1 (do not override via wrapper defaults when set)
#
# Wrapper flags (accepted even if placed after `--`):
#   --jobs/-j N --fetch-timeout SECS --render-timeout SECS --cache-dir DIR --no-fetch
#   --disk-cache --no-disk-cache
#
# Extra arguments are forwarded to `pageset_progress run`. Use `--` to separate them from the
# wrapper flags, e.g.:
#   scripts/pageset.sh -- --pages example.com --disk-cache-max-age-secs 0
#
# Note: `--pages` and `--shard` are also forwarded to `fetch_pages` and `prefetch_assets` so that
# one-page debugging runs don't spend time fetching/prefetching the entire pageset.
#
# Note: `--allow-http-error-status` is forwarded to `fetch_pages` so pageset runs can cache
# transient 4xx/5xx pages for debugging without breaking `pageset_progress` arg parsing.
#
# Note: `--refresh` is forwarded to `fetch_pages` so pageset runs can re-fetch cached HTML in one
# command.
#
# Note: `--allow-collisions` is forwarded to `fetch_pages` so temporary pageset stem collisions can
# be investigated without editing the pageset list.
#
# Note: `--timings` is forwarded to `fetch_pages` so callers can inspect document fetch durations.
#
# Note: when disk cache is enabled (so `prefetch_assets` runs) and `prefetch_assets` supports
# `--prefetch-fonts` / `--prefetch-images` / `--prefetch-iframes` (alias `--prefetch-documents`) /
# `--prefetch-embeds` / `--prefetch-icons` / `--prefetch-video-posters` / `--prefetch-css-url-assets` /
# `--max-discovered-assets-per-page` / `--max-images-per-page` / `--max-image-urls-per-element`,
# these flags are intercepted by the wrapper and forwarded to `prefetch_assets` (not
# `pageset_progress`) so users can override the wrapper defaults without breaking `pageset_progress`
# arg parsing.

TOTAL_CPUS="$(detect_total_cpus)"
JOBS="${JOBS:-${TOTAL_CPUS}}"
FETCH_TIMEOUT="${FETCH_TIMEOUT:-30}"
RENDER_TIMEOUT="${RENDER_TIMEOUT:-5}"
USE_DISK_CACHE="${DISK_CACHE:-1}"
CACHE_DIR="fetches/assets"
NO_FETCH=0

if [[ -n "${NO_DISK_CACHE:-}" ]]; then
  USE_DISK_CACHE=0
fi

ARGS=()
while [[ $# -gt 0 ]]; do
  arg="$1"
  # Be forgiving if callers accidentally place wrapper flags after `--` (intended for
  # pageset_progress args). The underlying binaries do not recognize these flags, so it's always
  # safe to treat them as wrapper flags.
  case "${arg}" in
    --no-disk-cache)
      USE_DISK_CACHE=0
      shift
      continue
      ;;
    --disk-cache)
      USE_DISK_CACHE=1
      shift
      continue
      ;;
    --no-fetch)
      NO_FETCH=1
      shift
      continue
      ;;
    --jobs|-j)
      if [[ $# -lt 2 || "$2" == -* ]]; then
        echo "${arg} requires a value" >&2
        exit 2
      fi
      JOBS="$2"
      shift 2
      continue
      ;;
    --jobs=*)
      JOBS="${arg#--jobs=}"
      shift
      continue
      ;;
    -j*)
      JOBS="${arg#-j}"
      if [[ -z "${JOBS}" ]]; then
        echo "${arg} requires a value" >&2
        exit 2
      fi
      shift
      continue
      ;;
    --fetch-timeout)
      if [[ $# -lt 2 || "$2" == -* ]]; then
        echo "${arg} requires a value" >&2
        exit 2
      fi
      FETCH_TIMEOUT="$2"
      shift 2
      continue
      ;;
    --fetch-timeout=*)
      FETCH_TIMEOUT="${arg#--fetch-timeout=}"
      shift
      continue
      ;;
    --render-timeout)
      if [[ $# -lt 2 || "$2" == -* ]]; then
        echo "${arg} requires a value" >&2
        exit 2
      fi
      RENDER_TIMEOUT="$2"
      shift 2
      continue
      ;;
    --render-timeout=*)
      RENDER_TIMEOUT="${arg#--render-timeout=}"
      shift
      continue
      ;;
    --cache-dir)
      if [[ $# -lt 2 || "$2" == -* ]]; then
        echo "${arg} requires a value" >&2
        exit 2
      fi
      CACHE_DIR="$2"
      shift 2
      continue
      ;;
    --cache-dir=*)
      CACHE_DIR="${arg#--cache-dir=}"
      shift
      continue
      ;;
    --)
      shift
      continue
      ;;
  esac
  ARGS+=("${arg}")
  shift
done

if ! [[ "${JOBS}" =~ ^[0-9]+$ ]] || [[ "${JOBS}" -lt 1 ]]; then
  echo "JOBS must be an integer > 0" >&2
  exit 2
fi
if ! [[ "${FETCH_TIMEOUT}" =~ ^[0-9]+$ ]]; then
  echo "FETCH_TIMEOUT must be an integer >= 0" >&2
  exit 2
fi
if ! [[ "${RENDER_TIMEOUT}" =~ ^[0-9]+$ ]]; then
  echo "RENDER_TIMEOUT must be an integer >= 0" >&2
  exit 2
fi
if [[ -z "${CACHE_DIR}" ]]; then
  echo "CACHE_DIR must be non-empty" >&2
  exit 2
fi

# pageset_progress runs up to JOBS worker processes in parallel (one per page). The renderer
# itself can also use Rayon threads (e.g., layout fan-out). Without a cap, enabling layout
# parallelism by default can oversubscribe CPUs catastrophically (JOBS * total_cpus threads).
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

if [[ "${USE_DISK_CACHE}" != 0 ]]; then
  export DISK_CACHE=1
  unset NO_DISK_CACHE || true
else
  export DISK_CACHE=0
  export NO_DISK_CACHE=1
fi

USER_AGENT_ARG="${USER_AGENT:-}"
ACCEPT_LANGUAGE_ARG="${ACCEPT_LANGUAGE:-}"
VIEWPORT_ARG="${VIEWPORT:-}"
DPR_ARG="${DPR:-}"

USER_AGENT_IN_ARGS=0
ACCEPT_LANGUAGE_IN_ARGS=0
VIEWPORT_IN_ARGS=0
DPR_IN_ARGS=0

for ((i=0; i < ${#ARGS[@]}; i++)); do
  arg="${ARGS[$i]}"
  case "${arg}" in
    --user-agent)
      USER_AGENT_IN_ARGS=1
      if [[ $((i + 1)) -lt ${#ARGS[@]} ]]; then
        USER_AGENT_ARG="${ARGS[$((i + 1))]}"
        i=$((i + 1))
      fi
      ;;
    --user-agent=*)
      USER_AGENT_IN_ARGS=1
      USER_AGENT_ARG="${arg#--user-agent=}"
      ;;
    --accept-language)
      ACCEPT_LANGUAGE_IN_ARGS=1
      if [[ $((i + 1)) -lt ${#ARGS[@]} ]]; then
        ACCEPT_LANGUAGE_ARG="${ARGS[$((i + 1))]}"
        i=$((i + 1))
      fi
      ;;
    --accept-language=*)
      ACCEPT_LANGUAGE_IN_ARGS=1
      ACCEPT_LANGUAGE_ARG="${arg#--accept-language=}"
      ;;
    --viewport)
      VIEWPORT_IN_ARGS=1
      if [[ $((i + 1)) -lt ${#ARGS[@]} ]]; then
        VIEWPORT_ARG="${ARGS[$((i + 1))]}"
        i=$((i + 1))
      fi
      ;;
    --viewport=*)
      VIEWPORT_IN_ARGS=1
      VIEWPORT_ARG="${arg#--viewport=}"
      ;;
    --dpr)
      DPR_IN_ARGS=1
      if [[ $((i + 1)) -lt ${#ARGS[@]} ]]; then
        DPR_ARG="${ARGS[$((i + 1))]}"
        i=$((i + 1))
      fi
      ;;
    --dpr=*)
      DPR_IN_ARGS=1
      DPR_ARG="${arg#--dpr=}"
      ;;
  esac
done

FETCH_KNOB_ARGS=()
PREFETCH_KNOB_ARGS=()
PAGESET_KNOB_ARGS=()
FETCH_EXTRA_ARGS=()

if [[ -n "${USER_AGENT_ARG}" ]]; then
  FETCH_KNOB_ARGS+=(--user-agent "${USER_AGENT_ARG}")
  PREFETCH_KNOB_ARGS+=(--user-agent "${USER_AGENT_ARG}")
  if [[ "${USER_AGENT_IN_ARGS}" -eq 0 ]]; then
    PAGESET_KNOB_ARGS+=(--user-agent "${USER_AGENT_ARG}")
  fi
fi

if [[ -n "${ACCEPT_LANGUAGE_ARG}" ]]; then
  FETCH_KNOB_ARGS+=(--accept-language "${ACCEPT_LANGUAGE_ARG}")
  PREFETCH_KNOB_ARGS+=(--accept-language "${ACCEPT_LANGUAGE_ARG}")
  if [[ "${ACCEPT_LANGUAGE_IN_ARGS}" -eq 0 ]]; then
    PAGESET_KNOB_ARGS+=(--accept-language "${ACCEPT_LANGUAGE_ARG}")
  fi
fi

if [[ -n "${VIEWPORT_ARG}" ]]; then
  PREFETCH_KNOB_ARGS+=(--viewport "${VIEWPORT_ARG}")
  if [[ "${VIEWPORT_IN_ARGS}" -eq 0 ]]; then
    PAGESET_KNOB_ARGS+=(--viewport "${VIEWPORT_ARG}")
  fi
fi

if [[ -n "${DPR_ARG}" ]]; then
  PREFETCH_KNOB_ARGS+=(--dpr "${DPR_ARG}")
  if [[ "${DPR_IN_ARGS}" -eq 0 ]]; then
    PAGESET_KNOB_ARGS+=(--dpr "${DPR_ARG}")
  fi
fi

DISK_CACHE_ARGS=()
for ((i=0; i < ${#ARGS[@]}; i++)); do
  arg="${ARGS[$i]}"
  case "${arg}" in
    --disk-cache-*=*)
      DISK_CACHE_ARGS+=("${arg}")
      ;;
    --disk-cache-*)
      DISK_CACHE_ARGS+=("${arg}")
      if [[ $((i + 1)) -lt ${#ARGS[@]} ]]; then
        next="${ARGS[$((i + 1))]}"
        if [[ "${next}" != -* ]]; then
          DISK_CACHE_ARGS+=("${next}")
          i=$((i + 1))
        fi
      fi
      ;;
  esac
done

SELECTION_ARGS=()
for ((i=0; i < ${#ARGS[@]}; i++)); do
  arg="${ARGS[$i]}"
  case "${arg}" in
    --pages|--pages=*)
      SELECTION_ARGS+=("${arg}")
      if [[ "${arg}" == "--pages" && $((i + 1)) -lt ${#ARGS[@]} ]]; then
        next="${ARGS[$((i + 1))]}"
        if [[ "${next}" != -* ]]; then
          SELECTION_ARGS+=("${next}")
          i=$((i + 1))
        fi
      fi
      ;;
    --shard|--shard=*)
      SELECTION_ARGS+=("${arg}")
      if [[ "${arg}" == "--shard" && $((i + 1)) -lt ${#ARGS[@]} ]]; then
        next="${ARGS[$((i + 1))]}"
        if [[ "${next}" != -* ]]; then
          SELECTION_ARGS+=("${next}")
          i=$((i + 1))
        fi
      fi
      ;;
  esac
done

EXTRA_DISK_CACHE_ARGS=()
if [[ "${USE_DISK_CACHE}" != 0 ]]; then
  DISK_CACHE_ALLOW_NO_STORE_IN_ARGS=0
  for arg in "${DISK_CACHE_ARGS[@]}"; do
    case "${arg}" in
      --disk-cache-allow-no-store|--disk-cache-allow-no-store=*)
        DISK_CACHE_ALLOW_NO_STORE_IN_ARGS=1
        break
        ;;
    esac
  done

  if [[ -z "${FASTR_DISK_CACHE_ALLOW_NO_STORE:-}" && "${DISK_CACHE_ALLOW_NO_STORE_IN_ARGS}" -eq 0 ]]; then
    EXTRA_DISK_CACHE_ARGS+=(--disk-cache-allow-no-store)
  fi
fi

PREFETCH_ASSET_ARGS=()
PAGESET_ARGS=()
PREFETCH_FONTS_IN_ARGS=0
PREFETCH_IMAGES_IN_ARGS=0
PREFETCH_IFRAMES_IN_ARGS=0
PREFETCH_EMBEDS_IN_ARGS=0
PREFETCH_ICONS_IN_ARGS=0
PREFETCH_VIDEO_POSTERS_IN_ARGS=0
PREFETCH_CSS_URL_ASSETS_IN_ARGS=0
MAX_DISCOVERED_ASSETS_IN_ARGS=0
MAX_IMAGES_PER_PAGE_IN_ARGS=0
MAX_IMAGE_URLS_PER_ELEMENT_IN_ARGS=0
PREFETCH_ASSETS_SUPPORT_PREFETCH_FONTS=0
PREFETCH_ASSETS_SUPPORT_PREFETCH_IMAGES=0
PREFETCH_ASSETS_SUPPORT_PREFETCH_IFRAMES=0
PREFETCH_ASSETS_SUPPORT_PREFETCH_EMBEDS=0
PREFETCH_ASSETS_SUPPORT_PREFETCH_ICONS=0
PREFETCH_ASSETS_SUPPORT_PREFETCH_VIDEO_POSTERS=0
PREFETCH_ASSETS_SUPPORT_PREFETCH_CSS_URL_ASSETS=0
PREFETCH_ASSETS_SUPPORT_MAX_DISCOVERED_ASSETS=0
PREFETCH_ASSETS_SUPPORT_MAX_IMAGES_PER_PAGE=0
PREFETCH_ASSETS_SUPPORT_MAX_IMAGE_URLS_PER_ELEMENT=0
PREFETCH_ASSETS_SOURCE="src/bin/prefetch_assets.rs"
if [[ -f "${PREFETCH_ASSETS_SOURCE}" ]]; then
  if grep -q "prefetch_fonts" "${PREFETCH_ASSETS_SOURCE}"; then
    PREFETCH_ASSETS_SUPPORT_PREFETCH_FONTS=1
  fi
  if grep -q "prefetch_images" "${PREFETCH_ASSETS_SOURCE}"; then
    PREFETCH_ASSETS_SUPPORT_PREFETCH_IMAGES=1
  fi
  if grep -q "prefetch_iframes" "${PREFETCH_ASSETS_SOURCE}"; then
    PREFETCH_ASSETS_SUPPORT_PREFETCH_IFRAMES=1
  fi
  if grep -q "prefetch_embeds" "${PREFETCH_ASSETS_SOURCE}"; then
    PREFETCH_ASSETS_SUPPORT_PREFETCH_EMBEDS=1
  fi
  if grep -q "prefetch_icons" "${PREFETCH_ASSETS_SOURCE}"; then
    PREFETCH_ASSETS_SUPPORT_PREFETCH_ICONS=1
  fi
  if grep -q "prefetch_video_posters" "${PREFETCH_ASSETS_SOURCE}"; then
    PREFETCH_ASSETS_SUPPORT_PREFETCH_VIDEO_POSTERS=1
  fi
  if grep -q "prefetch_css_url_assets" "${PREFETCH_ASSETS_SOURCE}"; then
    PREFETCH_ASSETS_SUPPORT_PREFETCH_CSS_URL_ASSETS=1
  fi
  if grep -q "max_discovered_assets_per_page" "${PREFETCH_ASSETS_SOURCE}"; then
    PREFETCH_ASSETS_SUPPORT_MAX_DISCOVERED_ASSETS=1
  fi
  if grep -q "max_images_per_page" "${PREFETCH_ASSETS_SOURCE}"; then
    PREFETCH_ASSETS_SUPPORT_MAX_IMAGES_PER_PAGE=1
  fi
  if grep -q "max_image_urls_per_element" "${PREFETCH_ASSETS_SOURCE}"; then
    PREFETCH_ASSETS_SUPPORT_MAX_IMAGE_URLS_PER_ELEMENT=1
  fi
fi

for ((i=0; i < ${#ARGS[@]}; i++)); do
  arg="${ARGS[$i]}"
  case "${arg}" in
    --pages|--pages=*)
      PAGESET_ARGS+=("${arg}")
      if [[ "${arg}" == "--pages" && $((i + 1)) -lt ${#ARGS[@]} ]]; then
        next="${ARGS[$((i + 1))]}"
        if [[ "${next}" != -* ]]; then
          PAGESET_ARGS+=("${next}")
          i=$((i + 1))
        fi
      fi
      ;;
    --shard|--shard=*)
      PAGESET_ARGS+=("${arg}")
      if [[ "${arg}" == "--shard" && $((i + 1)) -lt ${#ARGS[@]} ]]; then
        next="${ARGS[$((i + 1))]}"
        if [[ "${next}" != -* ]]; then
          PAGESET_ARGS+=("${next}")
          i=$((i + 1))
        fi
      fi
      ;;
    --prefetch-fonts|--prefetch-fonts=*)
      if [[ "${PREFETCH_ASSETS_SUPPORT_PREFETCH_FONTS}" -eq 1 ]]; then
        PREFETCH_FONTS_IN_ARGS=1
        PREFETCH_ASSET_ARGS+=("${arg}")
        if [[ "${arg}" == "--prefetch-fonts" && $((i + 1)) -lt ${#ARGS[@]} ]]; then
          next="${ARGS[$((i + 1))]}"
          if [[ "${next}" != -* ]]; then
            PREFETCH_ASSET_ARGS+=("${next}")
            i=$((i + 1))
          fi
        fi
      else
        PAGESET_ARGS+=("${arg}")
      fi
      ;;
    --prefetch-images|--prefetch-images=*)
      if [[ "${PREFETCH_ASSETS_SUPPORT_PREFETCH_IMAGES}" -eq 1 ]]; then
        PREFETCH_IMAGES_IN_ARGS=1
        PREFETCH_ASSET_ARGS+=("${arg}")
        if [[ "${arg}" == "--prefetch-images" && $((i + 1)) -lt ${#ARGS[@]} ]]; then
          next="${ARGS[$((i + 1))]}"
          if [[ "${next}" != -* ]]; then
            PREFETCH_ASSET_ARGS+=("${next}")
            i=$((i + 1))
          fi
        fi
      else
        PAGESET_ARGS+=("${arg}")
      fi
      ;;
    --prefetch-iframes|--prefetch-iframes=*|--prefetch-documents|--prefetch-documents=*)
      if [[ "${PREFETCH_ASSETS_SUPPORT_PREFETCH_IFRAMES}" -eq 1 ]]; then
        PREFETCH_IFRAMES_IN_ARGS=1
        PREFETCH_ASSET_ARGS+=("${arg}")
        if [[ ("${arg}" == "--prefetch-iframes" || "${arg}" == "--prefetch-documents") && $((i + 1)) -lt ${#ARGS[@]} ]]; then
          next="${ARGS[$((i + 1))]}"
          if [[ "${next}" != -* ]]; then
            PREFETCH_ASSET_ARGS+=("${next}")
            i=$((i + 1))
          fi
        fi
      else
        PAGESET_ARGS+=("${arg}")
      fi
      ;;
    --prefetch-embeds|--prefetch-embeds=*)
      if [[ "${PREFETCH_ASSETS_SUPPORT_PREFETCH_EMBEDS}" -eq 1 ]]; then
        PREFETCH_EMBEDS_IN_ARGS=1
        PREFETCH_ASSET_ARGS+=("${arg}")
        if [[ "${arg}" == "--prefetch-embeds" && $((i + 1)) -lt ${#ARGS[@]} ]]; then
          next="${ARGS[$((i + 1))]}"
          if [[ "${next}" != -* ]]; then
            PREFETCH_ASSET_ARGS+=("${next}")
            i=$((i + 1))
          fi
        fi
      else
        PAGESET_ARGS+=("${arg}")
      fi
      ;;
    --prefetch-icons|--prefetch-icons=*)
      if [[ "${PREFETCH_ASSETS_SUPPORT_PREFETCH_ICONS}" -eq 1 ]]; then
        PREFETCH_ICONS_IN_ARGS=1
        PREFETCH_ASSET_ARGS+=("${arg}")
        if [[ "${arg}" == "--prefetch-icons" && $((i + 1)) -lt ${#ARGS[@]} ]]; then
          next="${ARGS[$((i + 1))]}"
          if [[ "${next}" != -* ]]; then
            PREFETCH_ASSET_ARGS+=("${next}")
            i=$((i + 1))
          fi
        fi
      else
        PAGESET_ARGS+=("${arg}")
      fi
      ;;
    --prefetch-video-posters|--prefetch-video-posters=*)
      if [[ "${PREFETCH_ASSETS_SUPPORT_PREFETCH_VIDEO_POSTERS}" -eq 1 ]]; then
        PREFETCH_VIDEO_POSTERS_IN_ARGS=1
        PREFETCH_ASSET_ARGS+=("${arg}")
        if [[ "${arg}" == "--prefetch-video-posters" && $((i + 1)) -lt ${#ARGS[@]} ]]; then
          next="${ARGS[$((i + 1))]}"
          if [[ "${next}" != -* ]]; then
            PREFETCH_ASSET_ARGS+=("${next}")
            i=$((i + 1))
          fi
        fi
      else
        PAGESET_ARGS+=("${arg}")
      fi
      ;;
    --max-images-per-page|--max-images-per-page=*)
      if [[ "${PREFETCH_ASSETS_SUPPORT_MAX_IMAGES_PER_PAGE}" -eq 1 ]]; then
        MAX_IMAGES_PER_PAGE_IN_ARGS=1
        PREFETCH_ASSET_ARGS+=("${arg}")
        if [[ "${arg}" == "--max-images-per-page" && $((i + 1)) -lt ${#ARGS[@]} ]]; then
          next="${ARGS[$((i + 1))]}"
          if [[ "${next}" != -* ]]; then
            PREFETCH_ASSET_ARGS+=("${next}")
            i=$((i + 1))
          fi
        fi
      else
        PAGESET_ARGS+=("${arg}")
      fi
      ;;
    --max-image-urls-per-element|--max-image-urls-per-element=*)
      if [[ "${PREFETCH_ASSETS_SUPPORT_MAX_IMAGE_URLS_PER_ELEMENT}" -eq 1 ]]; then
        MAX_IMAGE_URLS_PER_ELEMENT_IN_ARGS=1
        PREFETCH_ASSET_ARGS+=("${arg}")
        if [[ "${arg}" == "--max-image-urls-per-element" && $((i + 1)) -lt ${#ARGS[@]} ]]; then
          next="${ARGS[$((i + 1))]}"
          if [[ "${next}" != -* ]]; then
            PREFETCH_ASSET_ARGS+=("${next}")
            i=$((i + 1))
          fi
        fi
      else
        PAGESET_ARGS+=("${arg}")
      fi
      ;;
    --max-discovered-assets-per-page|--max-discovered-assets-per-page=*)
      if [[ "${PREFETCH_ASSETS_SUPPORT_MAX_DISCOVERED_ASSETS}" -eq 1 ]]; then
        MAX_DISCOVERED_ASSETS_IN_ARGS=1
        PREFETCH_ASSET_ARGS+=("${arg}")
        if [[ "${arg}" == "--max-discovered-assets-per-page" && $((i + 1)) -lt ${#ARGS[@]} ]]; then
          next="${ARGS[$((i + 1))]}"
          if [[ "${next}" != -* ]]; then
            PREFETCH_ASSET_ARGS+=("${next}")
            i=$((i + 1))
          fi
        fi
      else
        PAGESET_ARGS+=("${arg}")
      fi
      ;;
    --prefetch-css-url-assets|--prefetch-css-url-assets=*)
      if [[ "${PREFETCH_ASSETS_SUPPORT_PREFETCH_CSS_URL_ASSETS}" -eq 1 ]]; then
        PREFETCH_CSS_URL_ASSETS_IN_ARGS=1
        PREFETCH_ASSET_ARGS+=("${arg}")
        if [[ "${arg}" == "--prefetch-css-url-assets" && $((i + 1)) -lt ${#ARGS[@]} ]]; then
          next="${ARGS[$((i + 1))]}"
          if [[ "${next}" != -* ]]; then
            PREFETCH_ASSET_ARGS+=("${next}")
            i=$((i + 1))
          fi
        fi
      else
        PAGESET_ARGS+=("${arg}")
      fi
      ;;
    --allow-http-error-status|--allow-http-error-status=*)
      FETCH_EXTRA_ARGS+=("${arg}")
      ;;
    --allow-collisions)
      FETCH_EXTRA_ARGS+=("${arg}")
      ;;
    --refresh)
      FETCH_EXTRA_ARGS+=("${arg}")
      ;;
    --timings)
      FETCH_EXTRA_ARGS+=("${arg}")
      ;;
    *)
      PAGESET_ARGS+=("${arg}")
      ;;
  esac
done

if [[ "${NO_FETCH}" -ne 0 ]]; then
  for arg in "${FETCH_EXTRA_ARGS[@]}"; do
    if [[ "${arg}" == "--refresh" ]]; then
      echo "--refresh cannot be used with --no-fetch" >&2
      exit 2
    fi
  done
fi

FEATURE_ARGS=()
if [[ "${USE_DISK_CACHE}" != 0 ]]; then
  FEATURE_ARGS=(--features disk_cache)
fi

if [[ "${NO_FETCH}" -eq 0 ]]; then
  echo "Fetching pages (jobs=${JOBS}, timeout=${FETCH_TIMEOUT}s, disk_cache=${USE_DISK_CACHE})..."
  cargo run --release "${FEATURE_ARGS[@]}" --bin fetch_pages -- --jobs "${JOBS}" --timeout "${FETCH_TIMEOUT}" "${SELECTION_ARGS[@]}" "${FETCH_KNOB_ARGS[@]}" "${FETCH_EXTRA_ARGS[@]}"
else
  echo "Skipping fetch_pages (--no-fetch); using existing cached HTML."
fi

if [[ "${USE_DISK_CACHE}" != 0 ]]; then
  echo "Prefetching assets (jobs=${JOBS}, timeout=${FETCH_TIMEOUT}s, cache_dir=${CACHE_DIR})..."
  if [[ "${PREFETCH_ASSETS_SUPPORT_PREFETCH_IMAGES}" -eq 1 && "${PREFETCH_IMAGES_IN_ARGS}" -eq 0 ]]; then
    PREFETCH_ASSET_ARGS+=(--prefetch-images)
  fi
  if [[ "${PREFETCH_ASSETS_SUPPORT_PREFETCH_CSS_URL_ASSETS}" -eq 1 && "${PREFETCH_CSS_URL_ASSETS_IN_ARGS}" -eq 0 ]]; then
    PREFETCH_ASSET_ARGS+=(--prefetch-css-url-assets)
  fi
  cargo run --release "${FEATURE_ARGS[@]}" --bin prefetch_assets -- --jobs "${JOBS}" --timeout "${FETCH_TIMEOUT}" --cache-dir "${CACHE_DIR}" "${SELECTION_ARGS[@]}" "${PREFETCH_KNOB_ARGS[@]}" "${PREFETCH_ASSET_ARGS[@]}" "${DISK_CACHE_ARGS[@]}" "${EXTRA_DISK_CACHE_ARGS[@]}"
fi

echo "Updating progress/pages (jobs=${JOBS}, hard timeout=${RENDER_TIMEOUT}s, disk_cache=${USE_DISK_CACHE}, cache_dir=${CACHE_DIR}, rayon_threads=${RAYON_NUM_THREADS}, layout_parallel=${FASTR_LAYOUT_PARALLEL})..."
cargo run --release "${FEATURE_ARGS[@]}" --bin pageset_progress -- run --jobs "${JOBS}" --timeout "${RENDER_TIMEOUT}" --bundled-fonts --cache-dir "${CACHE_DIR}" "${PAGESET_KNOB_ARGS[@]}" "${PAGESET_ARGS[@]}" "${EXTRA_DISK_CACHE_ARGS[@]}"
