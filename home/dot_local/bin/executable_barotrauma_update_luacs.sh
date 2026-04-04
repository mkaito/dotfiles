#!/usr/bin/env bash
set -euo pipefail

# Configuration
REPO="evilfactory/LuaCsForBarotrauma"
DOWNLOAD_DIR="$HOME/Downloads"

# Detect platform and set artifact + target directory
case "$OSTYPE" in
  linux*)
    ARTIFACT="luacsforbarotrauma_patch_linux_client.zip"
    TARGET="$HOME/.steam/steam/steamapps/common/Barotrauma"
    ;;
  darwin*)
    ARTIFACT="luacsforbarotrauma_patch_mac_client.zip"
    TARGET="$HOME/Library/Application Support/Steam/steamapps/common/Barotrauma/Barotrauma.app/Contents/MacOS"
    ;;
  *)
    echo "Unsupported OS: $OSTYPE" >&2
    exit 1
    ;;
esac

# Prerequisites
for cmd in gh jq unzip; do
  command -v "$cmd" >/dev/null || { echo "Install '$cmd' first." >&2; exit 1; }
done

# Ensure GitHub CLI is authenticated
if gh auth status 2>&1 | grep -q 'You are not logged into'; then
  gh auth login
fi

# Verify installation path
[[ -d "$TARGET" ]] || { echo "Barotrauma not found at: $TARGET" >&2; exit 1; }

# Get latest release info
tag=$(gh release view latest --repo "$REPO" --json tagName --jq .tagName)
published=$(gh release view latest --repo "$REPO" --json publishedAt --jq .publishedAt)
date=${published%%T*}

# Get commit SHA
full_sha=$(gh api "repos/$REPO/git/ref/tags/$tag" --jq .object.sha)
short_sha=${full_sha:0:7}

# Build filenames
new_file="luacsforbarotrauma_patch_${OSTYPE#darwin*linux*}_${date}_${short_sha}.zip"
dest="$DOWNLOAD_DIR/$new_file"

# Download if missing
if [[ ! -f "$dest" ]]; then
  gh release download "$tag" --repo "$REPO" --pattern "$ARTIFACT" --dir "$DOWNLOAD_DIR"
  mv "$DOWNLOAD_DIR/$ARTIFACT" "$dest"
else
  echo "Already have: $new_file"
fi

# Apply patch
unzip -o "$dest" -d "$TARGET" && echo "Patched: $TARGET" || { echo "Unzip failed." >&2; exit 1; }
