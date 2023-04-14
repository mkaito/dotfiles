#!/usr/bin/env bash
set -euo pipefail

# Constants
DATA_WINDOWS='/mnt/windows/Users/chris/AppData/Roaming/SpaceEngineers'
DATA_LINUX="$HOME/Games/Saved Games/SpaceEngineers"
STEAM_ID='76561198021305673'

# *.?????? seem to be IDE temp files, which fail to be created on Linux
RSYNC_OPTS=(-rlt --delete-after)

# ensure partition is mounted
ensure_mounted() {
  [[ -d $DATA_WINDOWS ]] || sudo mount /mnt/windows
}

# ensure directory exists
ensure_dir() {
    [[ -d $1 ]] || mkdir -p "$1"
}

# Pull data
se_pull() {
    ensure_mounted

    ensure_dir "$DATA_LINUX/Saves/$STEAM_ID"
    rsync "${RSYNC_OPTS[@]}" "$DATA_WINDOWS/Saves/$STEAM_ID/" "$DATA_LINUX/Saves/$STEAM_ID"

    ensure_dir "$DATA_LINUX/IngameScripts/local"
    rsync "${RSYNC_OPTS[@]}" "$DATA_WINDOWS/IngameScripts/local/" "$DATA_LINUX/IngameScripts/local/"

    ensure_dir "$DATA_LINUX/Blueprints/local"
    rsync "${RSYNC_OPTS[@]}" "$DATA_WINDOWS/Blueprints/local/" "$DATA_LINUX/Blueprints/local/"
}

# Push data
se_push() {
    ensure_mounted

    ensure_dir "$DATA_WINDOWS/Saves/$STEAM_ID"
    rsync "${RSYNC_OPTS[@]}" "$DATA_LINUX/Saves/$STEAM_ID/" "$DATA_WINDOWS/Saves/$STEAM_ID/"

    ensure_dir "$DATA_WINDOWS/IngameScripts/local"
    rsync "${RSYNC_OPTS[@]}" "$DATA_LINUX/IngameScripts/local/" "$DATA_WINDOWS/IngameScripts/local/"

    ensure_dir "$DATA_WINDOWS/Blueprints/local"
    rsync "${RSYNC_OPTS[@]}" "$DATA_LINUX/Blueprints/local/" "$DATA_WINDOWS/Blueprints/local/"
}

# Parse $1
case "$1" in
  pull)
    se_pull
    ;;
  push)
    se_push
    ;;
  *)
    echo 'Invalid argument. Must be one of push|pull'
    exit 1
esac
