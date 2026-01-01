#!/usr/bin/env bash
set -euo pipefail

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
# Extra arguments are forwarded to `pageset_progress run`. Use `--` to separate them from the
# wrapper flags, e.g.:
#   scripts/pageset.sh -- --pages example.com --disk-cache-max-age-secs 0
#
# Note: when disk cache is enabled (so `prefetch_assets` runs) and `prefetch_assets` supports
# `--prefetch-images` / `--prefetch-css-url-assets` / `--prefetch-iframes` (alias
# `--prefetch-documents`) / `--max-discovered-assets-per-page` / `--max-images-per-page` /
# `--max-image-urls-per-element`, these flags are intercepted by the wrapper and forwarded to
# `prefetch_assets` (not `pageset_progress`) so users can override the wrapper defaults without
# breaking `pageset_progress` arg parsing.

TOTAL_CPUS="$(nproc)"
JOBS="${JOBS:-${TOTAL_CPUS}}"
FETCH_TIMEOUT="${FETCH_TIMEOUT:-30}"
RENDER_TIMEOUT="${RENDER_TIMEOUT:-5}"
USE_DISK_CACHE="${DISK_CACHE:-1}"

if [[ "${JOBS}" -lt 1 ]]; then
  echo "JOBS must be > 0" >&2
  exit 2
fi

# pageset_progress runs up to JOBS worker processes in parallel (one per page). The renderer
# itself can also use Rayon threads (e.g., layout fan-out). Without a cap, enabling layout
# parallelism by default can oversubscribe CPUs catastrophically (JOBS * nproc threads).
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

if [[ -n "${NO_DISK_CACHE:-}" ]]; then
  USE_DISK_CACHE=0
fi

ARGS=()
PARSE_FLAGS=1
for arg in "$@"; do
  if [[ "${PARSE_FLAGS}" -eq 1 ]]; then
    case "${arg}" in
      --no-disk-cache)
        USE_DISK_CACHE=0
        continue
        ;;
      --disk-cache)
        USE_DISK_CACHE=1
        continue
        ;;
      --)
        PARSE_FLAGS=0
        continue
        ;;
    esac
  fi
  ARGS+=("${arg}")
done

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
PREFETCH_IMAGES_IN_ARGS=0
PREFETCH_IFRAMES_IN_ARGS=0
PREFETCH_CSS_URL_ASSETS_IN_ARGS=0
MAX_DISCOVERED_ASSETS_IN_ARGS=0
MAX_IMAGES_PER_PAGE_IN_ARGS=0
MAX_IMAGE_URLS_PER_ELEMENT_IN_ARGS=0
PREFETCH_ASSETS_SUPPORT_PREFETCH_IMAGES=0
PREFETCH_ASSETS_SUPPORT_PREFETCH_IFRAMES=0
PREFETCH_ASSETS_SUPPORT_PREFETCH_CSS_URL_ASSETS=0
PREFETCH_ASSETS_SUPPORT_MAX_DISCOVERED_ASSETS=0
PREFETCH_ASSETS_SUPPORT_MAX_IMAGES_PER_PAGE=0
PREFETCH_ASSETS_SUPPORT_MAX_IMAGE_URLS_PER_ELEMENT=0
PREFETCH_ASSETS_SOURCE="src/bin/prefetch_assets.rs"
if [[ -f "${PREFETCH_ASSETS_SOURCE}" ]]; then
  if grep -q "prefetch_images" "${PREFETCH_ASSETS_SOURCE}"; then
    PREFETCH_ASSETS_SUPPORT_PREFETCH_IMAGES=1
  fi
  if grep -q "prefetch_iframes" "${PREFETCH_ASSETS_SOURCE}"; then
    PREFETCH_ASSETS_SUPPORT_PREFETCH_IFRAMES=1
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
    *)
      PAGESET_ARGS+=("${arg}")
      ;;
  esac
done

FEATURE_ARGS=()
if [[ "${USE_DISK_CACHE}" != 0 ]]; then
  FEATURE_ARGS=(--features disk_cache)
fi

echo "Fetching pages (jobs=${JOBS}, timeout=${FETCH_TIMEOUT}s, disk_cache=${USE_DISK_CACHE})..."
cargo run --release "${FEATURE_ARGS[@]}" --bin fetch_pages -- --jobs "${JOBS}" --timeout "${FETCH_TIMEOUT}" "${FETCH_KNOB_ARGS[@]}"

if [[ "${USE_DISK_CACHE}" != 0 ]]; then
  echo "Prefetching assets (jobs=${JOBS}, timeout=${FETCH_TIMEOUT}s)..."
  if [[ "${PREFETCH_ASSETS_SUPPORT_PREFETCH_IMAGES}" -eq 1 && "${PREFETCH_IMAGES_IN_ARGS}" -eq 0 ]]; then
    PREFETCH_ASSET_ARGS+=(--prefetch-images)
  fi
  if [[ "${PREFETCH_ASSETS_SUPPORT_PREFETCH_CSS_URL_ASSETS}" -eq 1 && "${PREFETCH_CSS_URL_ASSETS_IN_ARGS}" -eq 0 ]]; then
    PREFETCH_ASSET_ARGS+=(--prefetch-css-url-assets)
  fi
  cargo run --release "${FEATURE_ARGS[@]}" --bin prefetch_assets -- --jobs "${JOBS}" --timeout "${FETCH_TIMEOUT}" "${PREFETCH_KNOB_ARGS[@]}" "${PREFETCH_ASSET_ARGS[@]}" "${DISK_CACHE_ARGS[@]}" "${EXTRA_DISK_CACHE_ARGS[@]}"
fi

echo "Updating progress/pages (jobs=${JOBS}, hard timeout=${RENDER_TIMEOUT}s, disk_cache=${USE_DISK_CACHE}, rayon_threads=${RAYON_NUM_THREADS}, layout_parallel=${FASTR_LAYOUT_PARALLEL})..."
cargo run --release "${FEATURE_ARGS[@]}" --bin pageset_progress -- run --jobs "${JOBS}" --timeout "${RENDER_TIMEOUT}" --bundled-fonts "${PAGESET_KNOB_ARGS[@]}" "${PAGESET_ARGS[@]}" "${EXTRA_DISK_CACHE_ARGS[@]}"
