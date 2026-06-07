#!/bin/sh
# Tier 1 — Gentoo bare-metal bootstrap.
#
# Assumes a working stage3 with portage. Ensures the tools the tier-2/3 deploy
# needs are present; it does not emerge anything itself (that needs root and is
# the operator's call) — it only reports what is missing.
set -eu

echo "==> Gentoo tier-1 bootstrap"

missing=
for tool in rsync git; do
    command -v "$tool" >/dev/null 2>&1 || missing="$missing $tool"
done
if [ -n "$missing" ]; then
    echo "Missing tools:$missing" >&2
    echo "Install them, e.g.: sudo emerge net-misc/rsync dev-vcs/git" >&2
    exit 1
fi

echo "Tier 1 OK. Next:"
echo "  mise run system:common    # tier 2 (gentoo/desktop)"
echo "  mise run system:machine   # tier 3 (this host)"
