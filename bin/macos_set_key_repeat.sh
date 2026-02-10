#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

defaults delete -g ApplePressAndHoldEnabled || true

enable_apps=(
  com.apple.TextEdit
  com.barebones.bbedit
)

disable_apps=(
  com.factorio
  com.microsoft.VSCode
  com.apple.Terminal
  com.mitchellh.ghostty
  org.alacritty
  com.todesktop.230313mzl4w4u92
)

# 4. Apply per-app settings
for bundle in "${enable_apps[@]}"; do
  defaults write "$bundle" ApplePressAndHoldEnabled -bool true
done

for bundle in "${disable_apps[@]}"; do
  defaults write "$bundle" ApplePressAndHoldEnabled -bool false
done

echo "ApplePressAndHoldEnabled settings updated. Restart affected apps to apply."
