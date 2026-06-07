#!/bin/sh
# Tier 1 — OpenBSD bare-metal bootstrap.
#
# Installs the tools the tier-2/3 deploy needs. The deploy uses GNU rsync
# features (--chown, --filter=merge, --files-from) that base openrsync lacks,
# so the rsync package is required. Run as your user; it escalates with doas.
set -eu

echo "==> OpenBSD tier-1 bootstrap"

pkgs=""
for tool in rsync git; do
    command -v "$tool" >/dev/null 2>&1 || pkgs="$pkgs $tool"
done
if [ -n "$pkgs" ]; then
    echo "Installing:$pkgs"
    # shellcheck disable=SC2086
    doas pkg_add -I $pkgs
fi

echo "Tier 1 OK. Next (mise if present, else the deploy script directly):"
echo "  sh system/lib/deploy.sh common     # tier 2 (openbsd/server)"
echo "  sh system/lib/deploy.sh machine    # tier 3 (this host)"
