#!/usr/bin/env bash

jq -r '
  def exclude: ["base", "space-age", "quality", "elevated-rails"];
  .mods[]
    | select(.enabled)
    | .name as $name
    | select(exclude | index($name) | not)
    | "- \($name): https://mods.com/\($name)"
' "$HOME/Library/Application Support/factorio/mods/mod-list.json"
