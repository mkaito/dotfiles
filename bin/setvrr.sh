#!/bin/bash

set -euo pipefail

function do_freesync_for_app {
  if ps -C "$1" >& /dev/null; then
    echo "Attaching Freesync property to: $1"
    MYPID=$(ps -C "$1" | tail -n 1 | awk '{print $1}')
    echo "PID: $MYPID"

    if [ "$MYPID" != "" ] ; then

      XDO=$(xdotool search --sync --onlyvisible --pid "$MYPID")

      echo "WIDs: $XDO" | xargs echo

      for WID in $XDO ; do
        xprop -f _VARIABLE_REFRESH 32c -set _VARIABLE_REFRESH 1 -id "$WID"
        # xprop -id "$WID" | grep '_VARIABLE_REFRESH'
      done
    fi
  else
    echo "Process not found: $1"
  fi
}

do_freesync_for_app SC2_x64.exe
