#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

REPO="evilfactory/LuaCsForBarotrauma"
ARTIFACT_NAME="luacsforbarotrauma_patch_mac_client.zip"
DOWNLOAD_DIR="$HOME/Downloads"
BAROTRAUMA_DIR="$HOME/Library/Application Support/Steam/steamapps/common/Barotrauma"
TARGET_DIR="$BAROTRAUMA_DIR/Barotrauma.app/Contents/MacOS"

if ! command -v gh &>/dev/null; then
  echo "** Error: 'gh' command not found. Please install GitHub CLI." >&2
  exit 1
fi

if ! command -v jq &>/dev/null; then
  echo "** Error: 'jq' command not found. Please install jq." >&2
  exit 1
fi

if gh auth status 2>&1 | grep -q "You are not logged into any GitHub hosts"; then
  gh auth login
fi

if [[ ! -d "$BAROTRAUMA_DIR" ]]; then
  echo "** Error: Barotrauma is not installed. Directory '$BAROTRAUMA_DIR' does not exist." >&2
  exit 1
fi

LATEST_RELEASE=$(gh release view latest --repo "$REPO" --json tagName,publishedAt | jq -r '. | .tagName + " " + .publishedAt')

TAG=$(echo "$LATEST_RELEASE" | awk '{print $1}')
DATE=$(echo "$LATEST_RELEASE" | awk '{print $2}' | sed 's/T.*//')

COMMIT_SHA=$(gh api "repos/$REPO/git/ref/tags/$TAG" --jq '.object.sha')

SHORT_SHA=$(echo "$COMMIT_SHA" | cut -c1-7)

NEW_FILENAME="luacsforbarotrauma_patch_mac_client_${DATE}_${SHORT_SHA}.zip"

if [[ -f "$DOWNLOAD_DIR/$NEW_FILENAME" ]]; then
  echo "** File $DOWNLOAD_DIR/$NEW_FILENAME already exists. Skipping download."
else
  gh release download "$TAG" --repo "$REPO" --pattern "$ARTIFACT_NAME" --dir "$DOWNLOAD_DIR" --clobber

  mv "$DOWNLOAD_DIR/$ARTIFACT_NAME" "$DOWNLOAD_DIR/$NEW_FILENAME"
fi

if unzip -o "$DOWNLOAD_DIR/$NEW_FILENAME" -d "$TARGET_DIR"; then
  echo "** Download and unpack complete. Files have been moved to $TARGET_DIR"
else
  echo "** Error: Failed to unzip $NEW_FILENAME" >&2
  exit 1
fi
