#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

need() { command -v "$1" >/dev/null 2>&1 || { echo "Missing: $1" >&2; exit 1; }; }
need chdman
need find
need mktemp

log() { printf '%s\n' "$*"; }

is_dvd_chd() {
  chdman info -i "$1" 2>/dev/null | grep -q "Tag='DVD"
}

verify_chd() {
  chdman verify -i "$1" >/dev/null
}

# Make a temp dir in the same folder as the target, so final rename is atomic on the same filesystem.
mktemp_near() {
  local target="$1"
  local dir
  dir="$(dirname -- "$target")"
  mktemp -d -- "${dir}/.chd-repack.XXXXXX"
}

rebuild_chd_in_place() {
  local chd="$1"
  local tmp out cue iso
  tmp="$(mktemp_near "$chd")"
  out="${tmp}/out.chd"
  cue="${tmp}/disc.cue"
  iso="${tmp}/disc.iso"

  if is_dvd_chd "$chd"; then
    chdman extractdvd -i "$chd" -o "$iso"
    # Preferred: createcd (often smaller; commonly works with NetherSX2-classic)
    if ! chdman createcd -i "$iso" -o "$out" 2>/dev/null; then
      # Fallback: createdvd must use zlib for NetherSX2-classic 3668
      chdman createdvd -c zlib -i "$iso" -o "$out"
    fi
  else
    chdman extractcd -i "$chd" -o "$cue"
    chdman createcd -i "$cue" -o "$out"
  fi

  verify_chd "$out"

  mv -f -- "$out" "$chd"
  rm -rf -- "$tmp"
  log "Rebuilt CHD: $chd"
}

convert_iso_in_place() {
  local iso="$1"
  local chd="${iso%.*}.chd"
  local tmp out
  tmp="$(mktemp_near "$chd")"
  out="${tmp}/out.chd"

  # Preferred: createcd (space + compatibility)
  if ! chdman createcd -i "$iso" -o "$out" 2>/dev/null; then
    # Fallback: createdvd with zlib for NetherSX2-classic 3668
    chdman createdvd -c zlib -i "$iso" -o "$out"
  fi

  verify_chd "$out"

  mv -f -- "$out" "$chd"
  rm -f -- "$iso"
  rm -rf -- "$tmp"
  log "Converted ISO -> CHD: $chd"
}

export -f rebuild_chd_in_place convert_iso_in_place is_dvd_chd verify_chd mktemp_near log

# 1) Rebuild all .chd under cwd (extract + reconvert in-place)
# while IFS= read -r -d '' f; do
#   rebuild_chd_in_place "$f"
# done < <(find . -type f -name '*.chd' -print0)

# 2) Convert all .iso under cwd to .chd (in-place: replace .iso with .chd)
while IFS= read -r -d '' f; do
  convert_iso_in_place "$f"
done < <(find . -type f -name '*.iso' -print0)
