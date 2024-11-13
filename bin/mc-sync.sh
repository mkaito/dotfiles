#!/usr/bin/env bash

# shellcheck disable=SC1090  # Disable 'Can't follow non-constant source' warning

set -euo pipefail
ROOT="$(dirname "$(realpath "$0")")"
RSYNCOPTS=(-avzzpPL -e ssh --delete)
USE_SYSTEMD=true
taction='push'
full_sync=false
TUSER=""
THOST=""
TPATH=""

usage() {
    echo "Usage: $0 [command] [options]"
    echo "Commands:"
    echo "  init      Initialize configuration and start script"
    echo "  push|pull Perform synchronization operations"
    echo "Options for init:"
    echo "  -f, --force   Overwrite existing files"
    echo "Options for push|pull:"
    echo "  -i, --instance=INSTANCE   Set the Minecraft instance name."
    echo "  -H, --host=HOST           Set the target hostname or SSH alias."
    echo "  --no-systemd              Do not attempt to stop/start the service using systemd."
    echo "  --full                    Perform a full sync, ignoring rsyncignore."
    echo "  -h, --help                Display this help message."
}

init() {
    local force=0
    while [ $# -gt 0 ]; do
        case "$1" in
            -f|--force)
                force=1
                shift
                ;;
            *)
                echo "Unknown option for init: $1"
                exit 1
                ;;
        esac
    done

    # .mc-syncrc file
    if [[ -f .mc-syncrc ]] && [[ $force -eq 0 ]]; then
        echo ".mc-syncrc already exists. Use -f to overwrite."
    else
        echo "instance=$(basename "$PWD")" > .mc-syncrc
        echo "host=stargazer" >> .mc-syncrc
        echo ".mc-syncrc created with default settings."
    fi

    # start.sh file
    if [[ -f start.sh ]] && [[ $force -eq 0 ]]; then
        echo "start.sh already exists. Use -f to overwrite."
    else
        local forge_jar
        forge_jar=$(find . -maxdepth 1 -name 'forge-*.jar' | head -n 1)
        if [[ -z $forge_jar ]]; then
            echo "No forge-*.jar file found. start.sh not created."
        else
            echo '#!/usr/bin/env bash' > start.sh
            echo "\"\${JAVA:-java}\" -d64 -server \$JVMOPTS -jar ${forge_jar#./} nogui" >> start.sh
            chmod +x start.sh
            echo "start.sh created."
        fi
    fi
}

parse_config() {
    if [[ -f .mc-syncrc ]]; then
        while IFS='=' read -r key value; do
            case "$key" in
                instance) TUSER="$value" ;;
                host) THOST="$value" ;;
            esac
        done < .mc-syncrc
    else
        echo "Error: .mc-syncrc file not found. Aborting."
        exit 1
    fi
    TPATH="::$TUSER/"
}

parse_args() {
    while [ $# -gt 0 ]; do
        case "$1" in
            -i|--instance)
                TUSER="${1#*=}"
                shift
                ;;
            -H|--host)
                THOST="${1#*=}"
                shift
                ;;
            --no-systemd)
                USE_SYSTEMD=false
                shift
                ;;
            --full)
                full_sync=true
                shift
                ;;
            push|pull)
                taction=$1
                shift
                ;;
            -h|--help)
                usage
                exit 0
                ;;
            *)
                echo "Unknown argument: $1"
                usage
                exit 1
                ;;
        esac
    done
}

perform_sync() {
    if [[ $USE_SYSTEMD == true ]]; then
        # shellcheck disable=SC2029
        ssh "$THOST" sudo systemctl stop "$TUSER"
    fi

    if [[ $full_sync == false ]]; then
        RSYNCOPTS+=(--exclude-from="$ROOT"/rsyncignore)
    fi

    if [[ $taction == push ]]; then
        # shellcheck disable=SC2087
        rsync "${RSYNCOPTS[@]}" ./ "${TUSER}@${THOST}${TPATH}"
    elif [[ $taction == pull ]]; then
        # shellcheck disable=SC2087
        rsync "${RSYNCOPTS[@]}" "${TUSER}@${THOST}${TPATH}" ./
    fi

    if [[ $USE_SYSTEMD == true ]]; then
        # shellcheck disable=SC2029
        ssh "$THOST" sudo systemctl start "$TUSER"
    fi
}

main() {
    case "$1" in
        init)
            shift
            init "$@"
            ;;
        push|pull)
            if [[ ! -f start.sh ]]; then
                echo 'Could not find start.sh in current folder. Aborting.'
                exit 1
            fi
            parse_config
            parse_args "$@"
            perform_sync
            ;;
        *)
            usage
            exit 1
            ;;
    esac
}

main "$@"
