#!/usr/bin/env bash
set -euo pipefail

export WINE='/opt/wine-tkg-staging-fsync-opt-git-6.12.1.r4.g092f3b1e/bin/wine'
# export WINE="$(command -v wine)"

export WINEPREFIX="$HOME/.local/share/wineprefixes/starcitizen"
export WINEDEBUG='-all'

export WINEDLLOVERRIDES='libglesv2=n;nvapi,nvapi64='
export WINEESYNC=1
export WINEFSYNC=1
export WINEFSYNC_FUTEX2=1

export MANGOHUD=1
export MANGOHUD_CONFIGFILE="$HOME/dev/dotfiles/starcitizen/mangohud.conf"

export DXVK_HUD=0
export DXVK_CONFIG_FILE="$HOME/dev/dotfiles/starcitizen/dxvk.conf"
export DXVK_STATE_CACHE=1
export WINE_FULLSCREEN_INTEGER_SCALING=1

export __GL_SHADER_DISK_CACHE=1
export __GL_SHADER_DISK_CACHE_SIZE=1073741824
export __GL_THREADED_OPTIMIZATIONS=1

INSTALLER_URL='https://install.robertsspaceindustries.com/star-citizen/RSI-Setup-1.4.10.exe'
INSTALLER_MD5='cea517ab9794527ee0c18ef0d7b7e3a4'
LAUNCHER='C:\Program Files\Roberts Space Industries\RSI Launcher\RSI Launcher.exe'

case "${1:-default}" in
    install)
        INSTALLER="$HOME/.cache/$(basename $INSTALLER_URL)"

        ## Do not overwrite prefix. Run reset first.
        if [[ -d "$WINEPREFIX" ]]; then
            echo "Destination $WINEPREFIX exists. Aborting. Run $(basename "$0") reset."
            exit 1
        fi

        winetricks --force -q dxvk corefonts vcrun2019 win10

        if [[ ! -f "$INSTALLER" ]]; then
            curl -o "$INSTALLER" "$INSTALLER_URL"
        fi

        if [[ -f "$INSTALLER" ]]; then
            MD5=$(md5sum "$INSTALLER")
            if [[ "$MD5" = "$INSTALLER_MD5  $INSTALLER" ]]; then
                "$WINE" "$INSTALLER"
            else
                echo "Installer did not pass md5 check. Removing and downloading again."

                rm "$INSTALLER"
                curl -o "$INSTALLER" "$INSTALLER_URL"
                MD5=$(md5sum "$INSTALLER")
                if [[ "$MD5" = "$INSTALLER_MD5  $INSTALLER" ]]; then
                    "$WINE" "$INSTALLER"
                else
                    echo "Downloaded installer again, still did not pass md5 check. Aborting."
                fi
            fi
        else
            echo "The installation file could not be found at $INSTALLER"
        fi
        ;;

    reset)
        echo "this will completely remove $WINEPREFIX. This can not be undone."
        read -p "Are you sure? " -n 1 -r

        if [[ $REPLY =~ ^[Yy]$ ]]; then
            echo
            echo -n "Nuking wineprefix... "
            rm -rf "$WINEPREFIX"
            echo "Done."
        else
            echo "Aborting."
        fi
        ;;

    symlink)
        ln -sf "$HOME/dev/dotfiles/starcitizen/USER.cfg" \
           "$WINEPREFIX/drive_c/Program Files/Roberts Space Industries/StarCitizen/LIVE/USER.cfg"
        if [[ -d "$WINEPREFIX/drive_c/Program Files/Roberts Space Industries/StarCitizen/PTU" ]]; then
            ln -sf "$HOME/dev/dotfiles/starcitizen/USER.cfg" \
            "$WINEPREFIX/drive_c/Program Files/Roberts Space Industries/StarCitizen/PTU/USER.cfg"
        fi
        ;;

    clearcache)
        rm -rf "$WINEPREFIX/drive_c/Program Files/Roberts Space Industries/StarCitizen/LIVE/USER/Client/0/shaders"
        rm -f  "$WINEPREFIX/drive_c/Program Files/Roberts Space Industries/StarCitizen/LIVE/StarCitizen.dxvk-cache"
        rm -rf "$HOME/.cache/mesa_shader_cache"

        rm -rf "$WINEPREFIX/drive_c/Program Files/Roberts Space Industries/StarCitizen/PTU/USER/Client/0/shaders"
        rm -f  "$WINEPREFIX/drive_c/Program Files/Roberts Space Industries/StarCitizen/PTU/StarCitizen.dxvk-cache"
        ;;

    version)
        echo "Wine path: ${WINE%%/bin/wine}"
        echo "Wine version: $($WINE --version)"
        ;;

    config)
        exec "${WINE%/*}/winecfg"
        ;;

    winetricks)
        shift
        exec winetricks "$@"
        ;;

    kill)
        pkill -fi -KILL "system32|windows|wineserver|wineconsole|awesomium|winetricks|winedbg|explorer.exe|RSI Launcher.exe|StarCitizen.exe"
        ;;

    run)
        shift
        exec "$WINE" "$@"
        ;;

    default)
        exec "$WINE" "$LAUNCHER" --use-gl=osmesa
        ;;

    *)
        echo 'Unknown command'
        ;;
esac

# vim:ft=sh
