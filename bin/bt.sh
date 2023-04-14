#!/usr/bin/env bash
set -euo pipefail

BT="$(command -v bluetoothctl)"
HEADPHONES='CC:98:8B:35:1C:10'
EARPLUGS='38:18:4C:A6:21:A3'

connect() {
    sudo systemctl restart bluetooth
    systemctl --user start pulseaudio
    "$BT" connect "$TARGET"
}

disconnect() {
    "$BT" disconnect "$TARGET"
}

reconnect() {
    disconnect; connect
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
