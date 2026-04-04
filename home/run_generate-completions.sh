#!/bin/sh
set -e

completion_dir="$HOME/.config/zsh/completion"

command -v rustup >/dev/null 2>&1 && rustup completions zsh > "$completion_dir/_rustup"
command -v mise   >/dev/null 2>&1 && mise completion zsh   > "$completion_dir/_mise"
