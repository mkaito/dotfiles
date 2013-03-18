#!/usr/bin/env bash

if [[ ! -z "$DEBUG" ]]; then
    set -x; set -v;
fi

##
# Argument processing
if [[ "$#" == "0" ]]; then
    
    # Check for .mfdl file for manga name
    if [ -f ".mfdl" ]; then
        name=$(cat .mfdl)
    else
        echo "I can download manga, but I can't read your mind."
        exit 1
    fi
fi

declare -a requested_chapters

for arg in "$@"
do
    case $arg in
        help|"--help"|"-h")
            echo "Usage:"
            echo "    mfdl.sh manga_name"
            echo "    mfdl.sh manga_name 7"
            echo "    mfdl.sh manga_name 7 5 12 8"
            echo
            echo "By default, if mfdl sees a chapter archive already exists"
            echo "for the chapter you're trying to download, it simply skips"
            echo "it silently. Pass the --force or -f flag to override this."
            echo
            echo "You can download multiple chapters easily:"
            echo "   for c in {1..15}; do; mfdl.sh coppelion $c; done"
            echo "   for c in (1 4 7 12); do; mfdl.sh coppelion $c; done"
            echo "Where 'coppelion' is the manga slug to download."
            ;;
        "--force"|"-f")
            FORCE=yes
            ;;
        [1-9]*)
            requested_chapters[${#requested_chapters[*]}]=$arg
            ;;
        *)
            name=$arg
            ;;
    esac
done

# (Re)Write .mfdl file with manga name for next time.
truncate --size 0 .mfdl
echo $name > .mfdl

MF_BASE_URL="http://www.mangafox.com/manga"

declare -a chapters
function get_chapters() {
    local c
    for c in $(curl -s "$MF_BASE_URL/$name" | sed -rne '\|tips| s|.*href="(.+)" title=".*|\1|p'); do
        chapters["$(uri_to_chapter_num "${c}")"]="${c}"
    done
}

function uri_to_chapter_num() {
    echo "$1" | sed -rne 's|.*/v[0-9]{1,2}/c0{0,2}([0-9]{1,2})/1.html$|\1|p'
}

function check_manga() {
    curl -sI "$MF_BASE_URL/$name" | grep -i "200 OK" > /dev/null
}

function pages_for_uri() {
    curl -s "$1" | sed -e 's|>|>\n|g' | sed -rne '\|<option| s|.*value="([0-9]{1,2})".*|\1|p' | awk ' !x[$0]++'
}

function page_base_url() {
    echo "$1" | sed -rne 's|(.*/).*|\1|p'
}

function image_for_page() {
    curl -s "$1" | sed 's|>|>\n|g' | grep 'id="image"' | sed -rn 's|.*src="(.*)" width.*|\1|p'
}

# Check for available chapters
get_chapters

if [[ ! -z "$DEBUG" ]]; then
    echo "============================================================"
    echo "Manga requested: $name"
    echo -n "Manga available: "
    check_manga && echo "Yes" || echo "No"
    chn="$((${#chapters[*]}+1))"
    echo "Number of available chapters: ${chn}"
    echo "Number of chapters requested: ${#requested_chapters[@]}"
fi

if [[ "${#requested_chapters[*]}" == "0" ]]; then
    echo "Downloading all available chapters"
    requested_chapters=$( seq ${#chapters[*]} )
fi

for ch in ${requested_chapters[*]}; do
    # Store downloads in a tmp folder
    tmp=`mktemp -d`

    # Build archive name
    fn="$name-$(printf "%.3i" $ch).cbz"

    # Check if we have to and want to overwrite an archive
    [ -f "$fn" -a "$FORCE" != "yes" ] && continue
    
    cc="${chapters[$ch]}"
    echo "* Chapter $ch:"
    pages=`pages_for_uri $cc`

    echo -n "Download: "
    for p in $pages; do
        
        image=$(image_for_page "$(page_base_url $cc)${p}.html")
        curl -sL -o "$tmp/$(basename $image)" $image

        case "$?" in
            0)
                echo -n "."
                ;;
            6)
                echo -n "?"
                ;;
            *)
                echo -n "*"
                ;;
        esac

    done

    echo
    echo -n "Packaging into ${fn}..."
    zip -jq9 $fn $tmp/*
    echo " Done!"

    rm -rf $tmp
done
