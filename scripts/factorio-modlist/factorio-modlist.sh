#!/usr/bin/env bash

SCRIPT_DIR="$(cd -P -- "$(dirname -- "$(readlink "$0" || echo "$0")")" && pwd -P)"

jq -r -f "$SCRIPT_DIR/script.jq" "$HOME/Library/Application Support/factorio/mods/mod-list.json"
