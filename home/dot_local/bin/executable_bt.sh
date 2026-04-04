#!/usr/bin/env bash
set -euo pipefail

BT="$(command -v bluetoothctl)"
HEADPHONES='14:3F:A6:F0:4D:19'
EARPLUGS='38:18:4C:A6:21:A3'

connect() {
    "$BT" connect "$TARGET"
}

disconnect() {
    "$BT" disconnect "$TARGET"
}

reconnect() {
    disconnect
    sleep 2
    connect
}

case "${2:-wh}" in
    wh|headphones)
        TARGET="$HEADPHONES"
        ;;
    wf|earplugs)
        TARGET="$EARPLUGS"
        ;;
esac

case "${1:-empty}" in
    c|connect)
        connect
        ;;

    d|disconnect)
        disconnect
        ;;

    r|reconnect)
        reconnect
        ;;
esac
