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

# function calc () { awk "BEGIN { print $* }" }
function calc () { echo "$@" | bc -l ; }

# FFMpeg screen capturing
function capture() {
  ffmpeg -y -f x11grab -r 12 -s $2 -i :0.0+$3 -vcodec libx264 -vpre ultrafast -crf 22 -threads 0 x11grab.$1
}

function wincap() {
  capture `xwininfo | grep 'geometry' | awk '{print $2}' | awk '{ BEGIN { FS="[+-]" } print $1 $2,$3; }'`
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

# ls shorthands
alias ls='ls -F --color=always'
alias ll='ls -lh'
alias la='ls -a'
alias lla='la -lh'

# Turn the display off
alias lcdoff='xset dpms force off'

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
