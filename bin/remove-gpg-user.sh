#!/usr/bin/env bash
#
# Script to remove GPG key from git-crypt
#
# It will re-initialize git-crypt for the repository and re-add all keys except
# the one requested for removal.
#
# Note: You still need to change all your secrets to fully protect yourself.
# Removing a user will prevent them from reading future changes but they will
# still have a copy of the data up to the point of their removal.
#
# Use:
#  ./remove-gpg-user.sh [FULL_GPG_FINGERPRINT [FULL_GPG_FINGERPRINT]]
#
# E.g.:
#  ./remove-gpg-user.sh 3BC18383F838C0B815B961480F8CAF5467D ABCD8383F838C0B815B961480F8CAF5467D
#
# The script will create multiple commits to your repo. Feel free to squash them
# all down to one.
#
# Based on https://github.com/AGWA/git-crypt/issues/47#issuecomment-212734882
#
#
set -euo pipefail
IFS=$'\n\t'

if [ -z "$1" ]
then
    echo " Use:"
    echo "  ./remove-gpg-user.sh [FULL_GPG_FINGERPRINT]"
    echo ""
    echo " E.g.:"
    echo "  ./remove-gpg-user.sh 3BC18383F838C0B815B961480F8CAF5467D"
    exit;
fi

TMPDIR=$(mktemp -d)
CURRENT_DIR=$(git rev-parse --show-toplevel)
BASENAME="$(basename "$PWD")"

# Cleanup even on error
function cleanup {
  rm -rf "$TMPDIR"
}

trap cleanup EXIT

# Unlock the directory, we need to copy encrypted versions of the files
git crypt unlock

# Work on copy.
cp -rp "$PWD" "$TMPDIR"


pushd "$TMPDIR/$BASENAME"

# Remove encrypted files and git-crypt
(git crypt status -e || true) | head --lines=-5 - >| encrypted-files
awk '{print $2}' encrypted-files | xargs git rm
git commit -m "Remove encrypted files"
git rm -r .git-crypt
git commit -m "Remove git-crypt"
rm -rf .git/git-crypt

# Re-initialize git crypt
git crypt init

# Set up production key
git crypt init -k production
declare -a prodkeys
prodkeys=(36F09E2FAB2D0F1ED02618FBC3F7D2B9DFA946A1 6EFD1053ADB6ABF50DF64792A36E70F9DC014A15 C1A66CB37D1610720E11DE793A7302D87411E61E)
for k in "${prodkeys[@]}"; do
  git crypt add-gpg-user -k production --trusted "$k"
done

# Add existing users, except the
for keyfilename in "$CURRENT_DIR"/.git-crypt/keys/default/0/*gpg; do
    bn="$(basename "$keyfilename")"
    key=${bn%.*}
    for k in "$@"; do [[ "$k" == "*$key*" ]] && continue; done
    git crypt add-gpg-user --trusted "$key"
done

cd "$CURRENT_DIR"
awk '{print $2}' "$TMPDIR/$BASENAME/encrypted-files" | while read -r i
do
  cp -rp --parents "$i" "$TMPDIR/$BASENAME";
done

cd "$TMPDIR/$BASENAME"
awk '{print $2}' encrypted-files | while read -r i
do
  git add "$i"
done

git commit -a -m "New encrypted files"
popd

git crypt lock -k production
git crypt lock
git pull "$TMPDIR/$BASENAME"

