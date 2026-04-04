# Disk/media management
alias mnt='udisksctl mount -b'
alias umnt='udisksctl unmount -b'

# Clipboard encryption (X11/xclip)
function clipencrypt() {
  local -a recipients
  while [[ -n $1 ]]; do
    recipients+=( '-r' "$1" )
    shift
  done
  xclip -o -selection clipboard | gpg -eao - "${(@)recipients}" | xclip -i -selection clipboard
}
alias ce=clipencrypt

# Transmission
compdef _gnu_generic transmission-remote
alias tt=transmission-remote
alias ttl='tt -l'

function tts {
  local torrents
  if [[ ! -t 0 ]]; then
    torrents=()
    while read line; do
      torrents+=( $(echo $line | grep -Po '^\s*\d+' | tr -d ' ') )
    done
    torrents=${(j:,:)torrents}
    echo "Starting torrents: ${torrents}."
  elif [[ -n "$1" ]]; then
    torrents="$1"
  fi
  tt -t${torrents:-all} -s
}

function ttS {
  local torrents
  if [[ ! -t 0 ]]; then
    torrents=()
    while read line; do
      torrents+=( $(echo $line | grep -Po '^\s*\d+' | tr -d ' ') )
    done
    torrents=${(j:,:)torrents}
    echo "Stopping torrents: ${torrents}."
  elif [[ -n "$1" ]]; then
    torrents="$1"
  fi
  tt -t${torrents:-all} -S ${2}
}

function ttr {
  local torrents
  if [[ ! -t 0 ]]; then
    torrents=()
    while read line; do
      torrents+=( $(echo $line | grep -Po '^\s*\d+' | tr -d ' ') )
    done
    torrents=${(j:,:)torrents}
    echo "Removing torrents: ${torrents}."
  elif [[ -n "$1" ]]; then
    torrents="$1"
  fi
  [[ -z "$torrents" ]] && echo "Won't remove all torrents!" && return 2
  tt -t${torrents} -r
}

function ttm {
  local torrents
  local target

  [[ -z "$1" ]] && echo "Please specify a target folder" && return 2
  mkdir -p "$1"
  target=$(realpath "$1")

  if [[ ! -t 0 ]]; then
    torrents=()
    while read line; do
      torrents+=( $(echo $line | grep -Po '^\s*\d+' | tr -d ' ') )
    done
    torrents=${(j:,:)torrents}
    echo "Moving torrents: ${torrents} -> ${target}."
  elif [[ -n "$1" ]]; then
    torrents="$1"
    target=$(realpath "$2")
  fi

  [[ -z "$torrents" ]] && echo "Won't move all torrents!" && return 2
  tt "-t${torrents}" --move "${target}"
}

function ttf {
  local pattern='*'
  [[ -n "$1" ]] && pattern="$1"
  ttl | grep -Ei "$pattern"
}

function tta {
  local -a args
  [[ -n "$1" ]] && args+=("-a" "$1")
  [[ -n "$2" ]] && args+=("-w" "$(realpath $2)")
  tt "$args[@]"
}

# vim:ft=zsh
