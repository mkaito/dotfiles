#!/bin/sh
playlist=$(mpc lsplaylists | rofi -dmenu -p "Playlist")
[ -n "$playlist" ] && mpc clear && mpc load "$playlist" && mpc play
