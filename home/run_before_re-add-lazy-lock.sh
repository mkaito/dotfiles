#!/bin/sh
# Auto-import nvim lazy-lock.json: nvim updates it on every plugin sync,
# so re-add from disk before apply to avoid "has changed" conflicts.
chezmoi re-add ~/.config/nvim/lazy-lock.json 2>/dev/null || true
