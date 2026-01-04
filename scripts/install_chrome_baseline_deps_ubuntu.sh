#!/usr/bin/env bash
set -euo pipefail

# Install dependencies for `scripts/chrome_baseline.sh` on Ubuntu.
#
# Installs:
# - python3 (used to inject <base href=...> into cached HTML)
# - fonts (to reduce missing glyphs / weird fallbacks)
# - poppler-utils (pdftoppm, used for print-to-PNG conversion when diffing paginated output)
# - a Chrome/Chromium binary (headless screenshots)
#
# Usage:
#   scripts/install_chrome_baseline_deps_ubuntu.sh
#
# Notes:
# - This prefers apt, but may fall back to snap for Chromium on Ubuntu.
# - In container environments without systemd, snap installs may fail.

usage() {
  cat <<'EOF'
usage: scripts/install_chrome_baseline_deps_ubuntu.sh

This script installs the Ubuntu dependencies needed to run:
  scripts/chrome_baseline.sh

It installs:
  - python3
  - fonts-dejavu-core, fonts-noto-core, fonts-noto-color-emoji
  - poppler-utils (pdftoppm)
  - chromium (or uses an existing google-chrome / chromium install)

Environment:
  CHROME_BIN=/path/to/chrome  (if you already have Chrome installed elsewhere)

EOF
}

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
  usage
  exit 0
fi

SUDO=""
if [[ "$(id -u)" != "0" ]]; then
  if command -v sudo >/dev/null 2>&1; then
    SUDO="sudo"
  else
    echo "error: need root or sudo" >&2
    exit 2
  fi
fi

# Soft guard: warn if not Ubuntu.
if [[ -r /etc/os-release ]]; then
  # shellcheck disable=SC1091
  . /etc/os-release
  if [[ "${ID:-}" != "ubuntu" ]]; then
    echo "warning: this script is tuned for Ubuntu (detected ID=${ID:-unknown}); continuing..." >&2
  fi
fi

export DEBIAN_FRONTEND=noninteractive

echo "Updating apt indices..."
${SUDO} apt-get update -y

echo "Installing python + fonts..."
${SUDO} apt-get install -y \
  python3 \
  fonts-dejavu-core \
  fonts-noto-core \
  fonts-noto-color-emoji \
  poppler-utils

have_browser() {
  command -v google-chrome-stable >/dev/null 2>&1 \
    || command -v google-chrome >/dev/null 2>&1 \
    || command -v chromium >/dev/null 2>&1 \
    || command -v chromium-browser >/dev/null 2>&1
}

if have_browser; then
  echo "Chrome/Chromium already installed."
else
  echo "Installing Chromium..."

  # Try apt first (may install a snap-backed chromium on Ubuntu).
  set +e
  ${SUDO} apt-get install -y chromium
  apt_status=$?
  set -e

  if [[ "${apt_status}" -ne 0 ]]; then
    set +e
    ${SUDO} apt-get install -y chromium-browser
    apt_status=$?
    set -e
  fi

  if [[ "${apt_status}" -ne 0 ]]; then
    echo "Apt install failed; trying snap-based Chromium..." >&2
    ${SUDO} apt-get install -y snapd

    if command -v systemctl >/dev/null 2>&1; then
      # Best-effort: snap needs its services running.
      ${SUDO} systemctl enable --now snapd >/dev/null 2>&1 || true
      ${SUDO} systemctl enable --now snapd.socket >/dev/null 2>&1 || true
    fi

    ${SUDO} snap install chromium
  fi
fi

if have_browser; then
  echo
  echo "Done."
  echo "Next:"
  echo "  cargo run --release --bin fetch_pages"
  echo "  scripts/chrome_baseline.sh"
else
  echo "error: failed to install Chrome/Chromium automatically." >&2
  echo "Install a browser and ensure one of these is in PATH:" >&2
  echo "  google-chrome-stable | google-chrome | chromium | chromium-browser" >&2
  echo "Then run: scripts/chrome_baseline.sh" >&2
  exit 1
fi
