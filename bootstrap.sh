#!/usr/bin/env bash
# Bootstrap a fresh machine to the point where mise tasks can be run.
#
# Usage:
#   git clone <dotfiles-repo> ~/dev/dotfiles
#   cd ~/dev/dotfiles
#   ./bootstrap.sh
#
# After this script: run `mise run install` (all platforms) and, for system
# config, the explicit tiers `mise run system:{bootstrap,common,machine}`.

set -euo pipefail

# ---------------------------------------------------------------------------
# Platform detection
# ---------------------------------------------------------------------------

OS="$(uname -s)"

if [[ "$OS" == "Darwin" ]]; then
    PLATFORM="darwin"
elif [[ "$OS" == "OpenBSD" ]]; then
    PLATFORM="openbsd"
elif [[ -f /etc/os-release ]]; then
    # shellcheck source=/dev/null
    source /etc/os-release
    PLATFORM="${ID:-unknown}"
else
    echo "Error: cannot detect platform (no /etc/os-release, not macOS/OpenBSD)" >&2
    exit 1
fi

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

fetch() {
    # Usage: fetch <url> | sh
    if command -v curl &>/dev/null; then
        curl -fsSL "$1"
    elif command -v wget &>/dev/null; then
        wget -qO- "$1"
    else
        echo "Error: neither curl nor wget found — install one and retry." >&2
        exit 1
    fi
}

install_mise() {
    if command -v mise &>/dev/null; then
        echo "mise already installed, skipping."
        return
    fi
    echo "==> Installing mise..."
    fetch https://mise.run | sh
    export PATH="$HOME/.local/bin:$PATH"
}

# ---------------------------------------------------------------------------
# Gentoo
# ---------------------------------------------------------------------------

if [[ "$PLATFORM" == "gentoo" ]]; then
    echo "==> Gentoo bootstrap"
    install_mise
    echo ""
    echo "Done. Next steps (each tier is explicit):"
    echo "  mise run system:bootstrap   # tier 1: verify base tools"
    echo "  mise run system:common      # tier 2: gentoo/<role> -> /etc"
    echo "  mise run system:machine     # tier 3: this host -> /etc"
    echo "  mise run install            # link chezmoi source and apply dotfiles"

# ---------------------------------------------------------------------------
# OpenBSD
# ---------------------------------------------------------------------------
# NB: base OpenBSD has no bash, so this script can't run until bash is added.
# On a blank box run the tier-1 script directly first:
#   doas sh system/openbsd/bootstrap.sh
# then `sh system/lib/deploy.sh {common,machine}` (or the mise tasks if present).

elif [[ "$PLATFORM" == "openbsd" ]]; then
    echo "==> OpenBSD bootstrap"
    sh system/openbsd/bootstrap.sh
    echo ""
    echo "Done. Next steps (mise if installed, else run deploy.sh directly):"
    echo "  sh system/lib/deploy.sh common     # tier 2: openbsd/server -> /etc"
    echo "  sh system/lib/deploy.sh machine    # tier 3: this host -> /etc"

# ---------------------------------------------------------------------------
# macOS
# ---------------------------------------------------------------------------

elif [[ "$PLATFORM" == "darwin" ]]; then
    echo "==> macOS bootstrap"

    if ! command -v brew &>/dev/null; then
        echo "==> Installing Homebrew..."
        /bin/bash -c "$(fetch https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
        [[ -x /opt/homebrew/bin/brew ]] && eval "$(/opt/homebrew/bin/brew shellenv)"
    fi

    install_mise

    echo ""
    echo "Done. Next steps:"
    echo "  mise run install   # link chezmoi source and apply dotfiles"

# ---------------------------------------------------------------------------

else
    echo "Error: unsupported platform '$PLATFORM'" >&2
    exit 1
fi
