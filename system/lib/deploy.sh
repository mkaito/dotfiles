#!/bin/sh
# Layered system-config deploy. Resolves the current host to an (os, role) via
# system/hosts/<hostname>, then deploys tiers to /etc:
#
#   tier 2 (common)  = system/<os>/<role>/etc
#   tier 3 (machine) = system/<os>/<hostname>/etc
#
# Tiers are applied as separate, explicit steps. Deploy is sequential per layer
# with NO --delete: machine files overlay common files, and removals are not
# propagated automatically (use `diff` to spot drift, prune /etc by hand).
#
# POSIX sh on purpose: this runs directly where mise is unavailable
#   (e.g. OpenBSD)  ->  sh system/lib/deploy.sh <subcommand>
# The mise tasks under .mise/tasks/system/ are thin wrappers over it.
#
# Usage: deploy.sh {bootstrap|common|machine|pull|diff}

set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
REPO=$(CDPATH= cd -- "$SCRIPT_DIR/../.." && pwd)
SYSTEM="$REPO/system"

HOST=$(hostname 2>/dev/null | cut -d. -f1)
MANIFEST="$SYSTEM/hosts/$HOST"
[ -f "$MANIFEST" ] || { echo "error: no host manifest at $MANIFEST" >&2; exit 1; }

OS=""; ROLE=""
while IFS='=' read -r k v; do
    case "$k" in
        os)   OS=$(printf '%s' "$v" | tr -d ' ') ;;
        role) ROLE=$(printf '%s' "$v" | tr -d ' ') ;;
    esac
done < "$MANIFEST"
[ -n "$OS" ] && [ -n "$ROLE" ] || { echo "error: $MANIFEST must set os= and role=" >&2; exit 1; }

# Sanity-check the manifest OS against the running kernel.
case "$(uname -s)" in
    Linux)   [ "$OS" = gentoo ]  || echo "warn: manifest os=$OS but running Linux" >&2 ;;
    OpenBSD) [ "$OS" = openbsd ] || echo "warn: manifest os=$OS but running OpenBSD" >&2 ;;
esac

case "$OS" in
    openbsd) ESC=doas ;;
    *)       ESC=sudo ;;
esac

FILTER="$SYSTEM/$OS/rsync-filter"
COMMON_DIR="$SYSTEM/$OS/$ROLE"
MACHINE_DIR="$SYSTEM/$OS/$HOST"

# Deploy one tier's etc/ onto /etc (root-owned, no delete). The filter (a single
# arg that must not be word-split) keeps the repo's own .gitignore out of /etc.
deploy_tier() {
    dir=$1
    if [ ! -d "$dir/etc" ]; then echo "==> $dir: no etc/, skipping"; return 0; fi
    echo "==> deploying $dir/etc -> /etc"
    if [ -f "$FILTER" ]; then
        $ESC rsync -a --chown=root:root --filter="merge $FILTER" "$dir/etc/" /etc/
    else
        $ESC rsync -a --chown=root:root "$dir/etc/" /etc/
    fi
}

# Drop paths matching the rsync-filter's `exclude` rules, so reverse operations
# (pull/diff) don't probe files that deploy never pushes (e.g. .gitignore,
# repos.conf/local.conf). Matches basename or path tail.
_drop_excluded() {
    [ -f "$FILTER" ] || { cat; return; }
    pat=$(awk '/^exclude /{p=$2; sub(/\/$/,"",p); gsub(/\./,"\\.",p);
                printf "%s(^|/)%s($|/)", sep, p; sep="|"}' "$FILTER")
    [ -n "$pat" ] || { cat; return; }
    grep -vE "$pat"
}

# Write the list of a tier's deployable files (paths relative to its etc/).
_tier_filelist() {
    ( cd "$1/etc" && find . -type f | sed 's|^\./||' ) | _drop_excluded
}

# Pull live /etc changes back into a tier — only files the tier already tracks.
pull_tier() {
    dir=$1
    [ -d "$dir/etc" ] || return 0
    echo "==> pulling /etc -> $dir/etc (tracked files only)"
    tmp=$(mktemp)
    _tier_filelist "$dir" > "$tmp"
    rsync -a --no-owner --no-group --files-from="$tmp" /etc/ "$dir/etc/"
    rm -f "$tmp"
}

# Itemized two-way diff for a tier, bounded to its tracked files.
diff_tier() {
    dir=$1
    [ -d "$dir/etc" ] || return 0
    tmp=$(mktemp)
    _tier_filelist "$dir" > "$tmp"
    echo "-- $dir: repo -> /etc (would be applied):"
    rsync -anic --no-group --files-from="$tmp" "$dir/etc/" /etc/ | grep -E '^[><c*]' || echo "   (none)"
    echo "-- $dir: /etc -> repo (drift; capture with pull):"
    rsync -anic --no-group --files-from="$tmp" /etc/ "$dir/etc/" | grep -E '^[><c*]' || echo "   (none)"
    rm -f "$tmp"
}

cmd=${1:-}
case "$cmd" in
    bootstrap)
        shift
        bs="$SYSTEM/$OS/bootstrap.sh"
        [ -x "$bs" ] || { echo "error: no $bs" >&2; exit 1; }
        exec "$bs" "$@"
        ;;
    common)  deploy_tier "$COMMON_DIR" ;;
    machine) deploy_tier "$MACHINE_DIR" ;;
    pull)
        [ "$(id -u)" -eq 0 ] && { echo "error: run pull as your user, not root" >&2; exit 1; }
        pull_tier "$COMMON_DIR"; pull_tier "$MACHINE_DIR"
        echo "Done. Review with git diff, then commit."
        ;;
    diff)    diff_tier "$COMMON_DIR"; diff_tier "$MACHINE_DIR" ;;
    *) echo "usage: deploy.sh {bootstrap|common|machine|pull|diff}" >&2; exit 2 ;;
esac
