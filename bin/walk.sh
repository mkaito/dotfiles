#!/bin/zsh

function climb() {
  n=$PWD
  [[ -z "$1" ]] && return 65 # = Bad arguments

  while [[ -d "$n" ]]; do
    p="${n}/${1}"

    if [[ -f "$p" ]]; then
      echo "$p"
      return 0
    fi

    n=${n%/*}
  done

  return 1
}
