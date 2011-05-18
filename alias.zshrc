# {{{ Aliases

# Eve online
alias eve='WINEPREFIX=~/.wine_64 wine explorer /desktop=0,1440x888 "C:\EVE\eve.exe"'
alias eve1='WINEPREFIX=~/.wine_64 wine explorer /desktop=1,1440x888 "C:\EVE\eve.exe"'
function killeve() {
	 ps aux | grep -iE '(wine|exefile|ccpbrowser|system32|eve)' | awk '{print $2}' | xargs kill
 }
function minertimer () {
    [ "$1" ] && t="$1"  || t=120
    echo "Setting recurring alert every ${t} seconds."
    while true; do
    	  sleep "$t";
	  beep;
    done
}

# Archive utilities
function compress() {
  if [[ ! -z "$1" ]]; then
    fn="$1"
    shift
  else
    echo "Please provide an archive name"
    return 65
  fi

  # Process flags
  while [[ $1 == -* ]]; do

  done

  if [[ -z "$1" ]]; then
    echo "Please provide a list of files or directories to compress"
    return 65
  fi

  case $fn in
  *.tar.gz)     tar czf $fn $@                              ;;
  *.tar.bz2)    tar cjf $fn $@                              ;;
  *.zip)        zip -r $fn $@                               ;;
  *.7z)         7z a -t7z -m0=lzma -mx=9 -mfb=64 $fn $@     ;;
  *)            echo "Please provide a valid archive kind: *.tar.gz, *.tar.bz2, *.zip, *.7z"; return 65 ;;
  esac
}

function extract() {
  while [ ! -z "$1" ]; do
    if [ -f "$1" ]; then
      case "$1" in
        *.tbz2 | *.tar.bz2) tar -xjf  "$1"     ;;
        *.txz | *.tar.xz)   tar -xJf  "$1"     ;;
        *.tgz | *.tar.gz)   tar -xzf  "$1"     ;;
        *.tar | *.cbt)      tar -xf   "$1"     ;;
        *.zip | *.cbz)      unzip      "$1"     ;;
        *.rar | *.cbr)      unrar x    "$1"     ;;
        *.arj)              unarj x    "$1"     ;;
        *.ace)              unace x    "$1"     ;;
        *.bz2)              bunzip2    "$1"     ;;
        *.xz)               unxz       "$1"     ;;
        *.gz)               gunzip     "$1"     ;;
        *.7z)               7z x       "$1"     ;;
        *.Z)                uncompress "$1"     ;;
        *.gpg)       gpg2 -d "$1" | tar -xvzf - ;;
        *) echo 'Error: I have no idea wtf you tried to feed me there.' ;;
      esac
    else
      echo 'Error: "$1" is not a valid file for extraction'
    fi

    shift
  done
}

# function calc () { awk "BEGIN { print $* }" }
function calc () { echo "$@" | bc -l ; }

# Fix file and folder permissions recursively under .
alias fixperm="find . -type d -exec chmod 755 {} \;; find . -type f -exec chmod 644 {} \;"

# FFMpeg screen capturing
function capture() {
  ffmpeg -y -f x11grab -r 12 -s $2 -i :0.0+$3 -vcodec libx264 -vpre ultrafast -crf 22 -threads 0 x11grab.$1
}

function wincap() {
  capture `xwininfo | grep 'geometry' | awk '{print $2}' | awk '{ BEGIN { FS="[+-]" } print $1 $2,$3; }'`
}

# Get my public ip
function pubip() {
  curl -s http://checkip.dyndns.org/ | grep -o "[[:digit:].]\+"
}

# aptitude shorthands
alias ag='sudo aptitude'
alias agi='ag install'
alias agu='ag update'
alias agp='ag purge'
alias agg='ag safe-upgrade'
alias ags='aptitude search'

# Yaourt shorthands
alias ya='yaourt'
alias yas='ya -S'
alias yass='ya -Ss'
alias yasy='ya -Sy'
alias yasyy='ya -Syy'
alias yard='ya -Rd'
alias yasyu='ya -Syu'
alias pacleandep='pacman -Qdtq | sudo xargs pacman -Rs'

# ls shorthands
alias ls='ls -F --color=always'
alias ll='ls -lh'
alias la='ls -a'
alias lla='la -lh'

# Moving around
alias ...='cd ..; cd ..'

# Turn the display off
alias lcdoff='xset dpms force off'

# Set the keyboard up
alias setkb='setxkbmap es; xmodmap ~/.xmodmap'

# Coding facility aliases
alias ann="egrep '# TODO|# FIXME' * -RnT | sed -e 's/\s\+/ /g'"
alias jek="jekyll --auto --server"
alias bjek="bundle exec jekyll --auto --server"
alias drafts='grep -ln --color=always "published: false" ~/dev/blog/_posts/*'
alias be='bundle exec'
alias ber='be rake'

# Get keycode for buttons
alias getkeycode="xev | grep -A2 --line-buffered '^KeyRelease' | sed -n '/keycode /s/^.*keycode \([0-9]*\).* (.*, \(.*\)).*$/\1 \2/p'"

# Emacs
alias ec="emacsclient --create-frame --no-wait"
alias ect="emacsclient --tty"
alias org='emacs -name org --funcall org-agenda-list &'

# Netcfg
alias n="sudo netcfg"
alias na="n -a"
alias nu="na && n"

# }}}

# vim:ft=sh
