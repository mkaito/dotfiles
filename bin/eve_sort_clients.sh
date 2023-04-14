#!/usr/bin/env bash
set -euo pipefail

declare -A CHARS
CHARS['Kaito Okimura']='EVE-Ratting'
CHARS['Teiko Okimura']='EVE-Ratting'
CHARS['Sanjo Okimura']='EVE-Explo'
CHARS['Shijo Okimura']='Shijo'
CHARS['Kounoko Okimura']='Kounoko'
CHARS['Meigo Okimura']='Meigo-Gokku-Kaito'
CHARS['Gokku Okimura']='Meigo-Gokku-Kaito'
CHARS['Shootsalot Otsada']='Idiot'

i3dump="$(i3-msg -t get_tree)"

for c in "${!CHARS[@]}"; do
    id=$(echo "$i3dump" | jq -c "recurse(.nodes[]) | select(.window_properties.title==\"EVE - $c\") | .id")
    if [[ -n $id ]]; then
        echo -n "Found $c at $id, assigning to workspace ${CHARS[$c]}... "
        i3-msg "[con_id=\"$id\"] move to workspace ${CHARS[$c]}"
    else
        echo "Did not find running client for $c, skipping. Remember to log in your characters first!"
    fi
done
