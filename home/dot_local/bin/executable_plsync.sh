#!/usr/bin/env zsh

# Takes all my playlists from ~/.mpd/playlists, fixes them up, and creates a
# folder for each, along with the music they reference.

# The sync stage requires an sshd server to run on your phone, as well as the rsync executable.
#   - http://linux.wxs.ro/2011/08/05/rsync-your-android/

MPD_MUSIC_ROOT="${HOME}/media/music"			# Root of your MPD library
MPD_PLAYLIST_ROOT="${HOME}/.mpd/playlists"	# MPD playlist folder
SYNC_MUSIC_ROOT="/media/sde1/Music"	# Staging folder (receives all files prior to sync)
PLAYLIST_ROOT="${SYNC_MUSIC_ROOT}/Playlists"	# Where to place playlist files (m3u) for sync
PHONE_MUSIC_ROOT="/sdcard/Music"		# Where the music will live on your phone
PHONE_ADDRESS="Nexus"				# Address to access your phone over ssh for sync

# Output stuff to stdout if $DEBUG evaluates to anything
# $ DEBUG=1 plsync.sh
function debug()
{
    [[ ! -z "$DEBUG" ]] && echo $*
}

# Sync stuff with phone over rsync+ssh
function sync() {
    rsync -vzrL --delete-after --no-perms --no-t ${SYNC_MUSIC_ROOT}/ ${PHONE_ADDRESS}:${PHONE_MUSIC_ROOT}/
}

# Ask for confirmation on something
function confirm()
{
    echo -n "$@ [y/N] "
    read answer
    for response in y Y yes YES Yes Sure sure SURE OK ok Ok kk; do
	[[ "_$answer" == "_$response" ]] && return 0
    done

    return 1
}

[[ ! -d "$PLAYLIST_ROOT" ]] && mkdir -p "$PLAYLIST_ROOT"

for pl in ~/.mpd/playlists/*.m3u; do
    plname="$(basename ${pl})"
    plname="${plname%%.*}"
    plfolder="${SYNC_MUSIC_ROOT}/${plname}"
    plfile="${PLAYLIST_ROOT}/${plname}.m3u"

    [[ -f "$plfile" ]] && rm -f "$plfile"
    [[ -d "$plfolder" ]] && rm -rf "$plfolder"
    [[ ! -d  "$plfolder" ]] && mkdir -p "$plfolder" && touch "$plfile"

    while read line; do
        f="${MPD_MUSIC_ROOT}/${line}"
        bn=$(basename "$f")

        if [[ -f "$f" && ! -f "${plfolder}/${bn}" ]]; then
            ln -s "$f" "$plfolder"
            echo "${plname}/${bn}" >> "$plfile"
	      fi

    done < "$pl"
done

# confirm "Would you like to sync with your phone? (Make sure sshd runs on the device)" && sync
