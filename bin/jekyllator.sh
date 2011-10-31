#!/bin/zsh

BLOG_PATH=${HOME}/dev/blog;
DRAFTS_PATH=${BLOG_PATH}/_drafts;
POSTS_PATH=${BLOG_PATH}/_posts;
FILE_EXT=".mkd"
# "be" is a shell alias for "bundle execute". Replace for your way to get jekyll serving.
JEKYLL_EX="cd ${BLOG_PATH} && be jekyll";
JEKYLL_SERVE="${JEKYLL_EX} --auto --server 8080 && ${BROWSER} http://localhost:8080";

# {{{ Functions

##
# Echo string if $DEBUG evaluates to aything.
# Execute with @DEBUG=1 blog.sh@ to get debug information printed.
function debug() {
    [[ ! -z "$DEBUG" ]] && echo "$*";
}

##
# Generate a url-safe version of the given string.
# Will clean extraneous whitespace, remove symbols, replace accent characters
# with their plain variant, and lower case the string.
function slugify() {
    [[ -z "$1" ]] && return 65;
    # s=`echo $1 | sed -e 's/\s\{1,\}/ /g' | sed -e 's/[^a-zA-Z ]//g' | sed 's/^[ \t]*//;s/[ \t]*$//g' | sed 's/ /-/g' | tr '[A-Z]' '[a-z]'`;
    s=`echo $1 | sed -e 's/\s\{1,\}/ /g' | sed 's/^[ \t]*//;s/[ \t]*$//g' | sed 's/ /-/g' | tr '[A-Z]' '[a-z]'`;
    debug "1: $s";

    # TODO: Accent replacement
    typeset -A accents; # key value key value key value
    accents=( 'á' 'a' 'à' 'a' 'â' 'a' 'ä' 'a' 'ã' 'a'
        'é' 'e' 'è' 'e' 'ê' 'e' 'ë' 'e'
        'í' 'i' 'ì' 'i' 'î' 'i' 'ï' 'i'
        'ó' 'o' 'ò' 'o' 'ô' 'o' 'ö' 'o' 'õ' 'o'
        'ú' 'u' 'ù' 'u' 'û' 'u' 'ü' 'u'
        'ç' 'c' 'ñ' 'n')

    for k in ${(k)accents}; do;
	debug "Replacing $k with ${accents[$k]}";
        s=`echo $s | sed -e "s/${k}/${accents[$k]}/g"`;
    done;

    debug "2: $s";
    
    echo "$s";
}

##
# Remove extraneous whitespace from string.
function clnstr() {
    [[ -z "$1" ]] && return 65;
    echo $1 | sed -e 's/\s\{1,\}/ /g' | sed 's/^[ \t]*//;s/[ \t]*$//g';
}

function confirm()
{
    echo -n "$@ [y/N] "
    read answer
    for response in y Y yes YES Yes Sure sure SURE OK ok Ok kk; do
        [[ "_$answer" == "_$response" ]] && return 0
    done

    return 1
}
# }}}

# {{{ Main processing block
[[ -z "$1" ]] && echo "Please provide a subcommand: create, publish, update, help" && exit 65;

case $1 in
    create)
        shift;
        slugstr=$(slugify "$*");
        titlestr=$(clnstr "$*");
        filestr="${slugstr}${FILE_EXT}"

        if [[ ! -f ${DRAFTS_PATH}/${filestr} ]]; then
            if (confirm "Create and edit file with $EDITOR: $filestr?"); then
                echo "---\ntitle: ${titlestr}\nbyline:\nmeta: \nlayout: post\npublished: false\n---" > ${DRAFTS_PATH}/${filestr};
                $EDITOR "${DRAFTS_PATH}/${filestr}";
            fi
        else
            echo "File already exists: ${filestr}";
            if (confirm "Would you like to edit it with $EDITOR now?"); then
                $EDITOR "${DRAFTS_PATH}/${filestr}";
            fi;
            exit 1;
        fi;
        ;;
    publish)
    	shift
	ppath=$(readlink -f $1)
        [[ ! -f "$ppath" ]] && echo "File not fount." && exit 65;
    	# File must be in drafts path
    	if echo "$ppath" | egrep "^${DRAFTS_PATH}" > /dev/null; then
    	    draftstr=$(basename "$ppath")
    	    pubstr="$(date +%Y-%m-%d)-$draftstr"
    	    [[ -f "${POSTS_PATH}/${pubstr}" ]] && echo "ERROR: A published post with this file name already exists." && exit 1
    	    if (confirm "Move ${draftstr} to ${pubstr} ?"); then
		if egrep 'published' $ppath > /dev/null; then
		    mv $ppath "${POSTS_PATH}/${pubstr}"
		    sed -i 's/published:\sfalse/published: true/' "${POSTS_PATH}/${pubstr}"
		else
		    gawk 'NR>1 && $ppath=="---" {printf"published: true\n"}{print}' $ppath > "${POSTS_PATH}/${pubstr}"
		    rm $ppath
		fi
    	    fi
    	else
    	    echo "File is not in configured drafts folder."
    	    echo "For security reasons, the file needs to be in the correct drafts folder before it can be moved."
    	    exit 1;
    	fi
        ;;
    update)
    	shift
    	[[ ! -f "$1" ]] && echo "File not fount." && exit 65;
    	# Date component is always 11 characters long.
    	ostr=$(basename "$1"); bstr=$ostr[12,-1]
    	nstr="$(date +%Y-%m-%d)-$bstr"
    	if (confirm "Move $ostr to $nstr ?"); then
    	    mv $1 "${POSTS_PATH}/${nstr}"
    	fi
        ;;
    slugify)
    	shift
        slugify "$*"
        ;;
    *)
        shift
        echo "This script helps you manage your Jekyll post collection. Get more help with help <command>"
	echo
        if [[ -z "$1" ]]; then
	    echo "Notes: "
	    echo "    Files in the drafts folder are expected to contain the post component of the permalink, but not the date."
	    echo "    Files already published won't be republished. To update the date component use $(basename $0) update [file]."
            echo
            echo "Available subcommands:"
            echo "    create        Creates a new draft with the given title."
            echo "    publish       Publishes the draft given as argument. Expects a file path."
            echo "    update        Updates the date in the given post file name and content. Expects a file path."
            echo "    slufigy       Create a slug from the given string."
            echo "    help          You're seeing it."
            echo
        else
            case $1 in
                create)
		    echo "[Create]"
                    echo " Create a new draft in your drafts folder. Everything after 'create' is assumed to be part of the title."
                    echo
                    echo "     $(basename $0) create This is the blog title"
                    echo
                    ;;
                publish)
		    echo "[Publish]"
                    echo " Given the path to a draft file, publish it by moving it to the _posts folder and prepending today's date to the file name."
		    echo " If the file contains a published attribute, it will be swapped to true. If not, it will be inserted and set to true."
                    echo
                    echo "     $(basename $0) publish /path/to/draft/file.mkd"
                    echo
                    ;;
                update)
		    echo "[Update]"
                    echo " Given the path to a published post file, update the date component of the filename to the current date"
                    echo
                    echo "    $(basename $0) update /path/to/published/post.mkd"
                    echo
                    ;;
		slugify)
		    echo "[Slugify]"
		    echo " Generate a url-safe version of the given string."
		    echo "     Will clean extraneous whitespace, remove symbols, replace accent characters"
		    echo "     with their plain variant, and lower case the string."
		    echo "     FIXME: Does not support capitalized accented characters."
		    echo
		    echo "       $(basename $0) slugify ï wânt tó slûgïfy thìs stríñg."
		    echo "       => i-want-to-slugify-this-string."
		    echo
		    ;;
                *)
                    echo "No help available for $1"
                    echo
                    ;;
            esac
        fi
        ;;
esac;
# }}}
