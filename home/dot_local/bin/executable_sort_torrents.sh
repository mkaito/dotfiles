#!/usr/bin/env zsh
set -euo pipefail

# shellcheck source=/dev/null
. "$HOME/dev/dotfiles/zsh/_alias"

NAMES=( \
  "Meikyuu Black Company/Season 1" \
  "Kobayashi-san Chi no Maid Dragon/Season 2" \
  "Vivy/Season 1" \
  "Tensura Nikki/Season 1" \
  "Slime Taoshite 300-nen/Season 1" \
  "Eighty Six/Season 1" \
  "Hortensia Saga/Season 1" \
  "Mushoku Tensei/Season 1" \
  "Dr. Stone/Season 2" \
  "Mahouka Koukou no Rettousei/Season 2" \
  "Re.Zero kara Hajimeru Isekai Seikatsu/Season 3" \
  "Spy x Family/Season 1" \
  "Machikado Mazoku/Season 2" \
  "Tate no Yuusha no Nariagari/Season 2" \
  "RPG Fudousan/Season 1" \
  "Yuusha, Yamemasu/Season 1" \
  "Shokei Shoujo no Virgin Road/Season 1" \
  "Aharen-san wa Hakarenai/Season 1" \
  "Isekai Shokudou/Season 2" \
  "Komi-san wa, Comyushou desu./Season 1" \
)

for n in "${NAMES[@]}"; do
  name="$(echo "$n" | cut -d'/' -f1)"
  ttl | grep "$name" | ttm "$HOME/media/anime/$n" || true
done

find "$HOME/media/anime" -empty -type d -delete
