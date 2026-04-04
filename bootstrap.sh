#!/usr/bin/env bash
# Bootstrap a fresh machine to the point where mise tasks can be run.
#
# Usage:
#   git clone <dotfiles-repo> ~/dev/dotfiles
#   cd ~/dev/dotfiles
#   ./bootstrap.sh
#
# After this script: run `mise run install` (all platforms) and, on Gentoo,
# `mise run gentoo:install` to deploy system config.

set -euo pipefail

# ---------------------------------------------------------------------------
# Platform detection
# ---------------------------------------------------------------------------

OS="$(uname -s)"

if [[ "$OS" == "Darwin" ]]; then
    PLATFORM="darwin"
elif [[ -f /etc/os-release ]]; then
    # shellcheck source=/dev/null
    source /etc/os-release
    PLATFORM="${ID:-unknown}"
else
    echo "Error: cannot detect platform (no /etc/os-release, not macOS)" >&2
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
    echo "Done. Next steps:"
    echo "  mise run gentoo:install   # deploy /etc and overlay"
    echo "  mise run install          # link chezmoi source and apply dotfiles"

# ---------------------------------------------------------------------------
# macOS — TODO: complete in a separate session on the Mac
# ---------------------------------------------------------------------------

elif [[ "$PLATFORM" == "darwin" ]]; then
    echo "==> macOS bootstrap"

    # TODO: install Homebrew
    # if ! command -v brew &>/dev/null; then
    #     echo "==> Installing Homebrew..."
    #     /bin/bash -c "$(fetch https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    #     # Apple Silicon: add brew to PATH for the rest of this script
    #     [[ -x /opt/homebrew/bin/brew ]] && eval "$(/opt/homebrew/bin/brew shellenv)"
    # fi

    # TODO: install mise
    # install_mise

    # TODO: next steps message
    # echo ""
    # echo "Done. Next steps:"
    # echo "  mise run install   # link chezmoi source and apply dotfiles"

    echo "macOS bootstrap not yet implemented." >&2
    exit 1

# ---------------------------------------------------------------------------

else
    echo "Error: unsupported platform '$PLATFORM'" >&2
    exit 1
fi
