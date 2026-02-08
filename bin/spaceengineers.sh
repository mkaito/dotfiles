#!/usr/bin/env bash
set -euo pipefail

# Constants
DATA_WINDOWS='/mnt/windows/Users/chris/AppData/Roaming/SpaceEngineers'
DATA_LINUX="$HOME/Games/Saved Games/SpaceEngineers"
COMPDATA_PATH="$HOME/.local/share/Steam/steamapps/compatdata/244850/pfx/drive_c/users/steamuser/AppData/Roaming/SpaceEngineers"
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

    ensure_dir "$DATA_LINUX/Mods"
    rsync "${RSYNC_OPTS[@]}" "$DATA_WINDOWS/Mods/" "$DATA_LINUX/Mods/"
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

    ensure_dir "$DATA_WINDOWS/Mods"
    rsync "${RSYNC_OPTS[@]}" "$DATA_LINUX/Mods/" "$DATA_WINDOWS/Mods/"
}

se_link() {
    if [[ ! -e $COMPDATA_PATH ]]; then 
        echo 'Compdata does not exist. Run game at least once via Steam.'
        exit 1
    fi

    if [[ -L $COMPDATA_PATH ]]; then 
        echo 'Compdata already linked. Remove manually and run again.'
        exit 1
    fi

    rm -rf "$COMPDATA_PATH"
    ln -sf "$DATA_LINUX" "$COMPDATA_PATH"
}

se_prefix() {
    #DELETE LINE FOR DEFAULT WINE CONFIG
    export WINE=$HOME/.var/app/com.valvesoftware.Steam/.local/share/Steam/compatibilitytools.d/GE-Proton10-11/files/bin/wine64;

    #SPACE ENGINEERS PREFIX
    export WINEPREFIX=$HOME/.var/app/com.valvesoftware.Steam/.local/share/Steam/steamapps/compatdata/244850/pfx;

    mkdir -p $HOME/SpaceEngineers_appdata_copy;
    cp -r $WINEPREFIX/drive_c/users/steamuser/AppData/Roaming/SpaceEngineers $HOME/SpaceEngineers_appdata_copy; 
    $WINE reg delete "HKLM\\Software\\Wow6432Node\\Microsoft\\NET Framework Setup\\NDP\\v4" /f | true;
    winetricks -q dotnet48
}

# Parse $1
case "$1" in
pull)
    se_pull
    ;;
push)
    se_push
    ;;
link)
    se_link
    ;;
prefix)
    se_prefix
    ;;
*)
    echo 'Invalid argument. Must be one of push|pull'
    exit 1
    ;;
esac
