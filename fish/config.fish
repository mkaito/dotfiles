# Disable greeting message on load
set -U fish_greeting
eval sh /usr/share/base16-shell/base16-grayscale.dark.sh

set -gx PAGER "less"
set -gx READNULLCMD "$PAGER"
set -gx EDITOR "emacsclient -c"
set -gx VISUAL "emacsclient -c"
set -gx ALTERNATE_EDITOR ””
set -gx BROWSER "firefox"
set -gx XTERM "$HOME/dev/bin/terminal"
set -gx TERMINAL "$XTERM"
set -gx PERL_SIGNALS "unsafe"
set -gx SUDO_EDITOR "emacsclient"
set -gx PATH $HOME/dev/bin $GOPATH/bin $HOME/perl5/bin $HOME/.cabal/bin $HOME/node_modules/.bin $PATH
set -gx LEDGER_FILE "$HOME/media/sync/ledger/personal.ledger"

# Set up rbenv
set -gx RBENV_ROOT $HOME/.rbenv
status --is-interactive; and . (rbenv init - | psub)

# Colorize man pages
set -xU LESS_TERMCAP_mb (printf "\e[01;31m")      # begin blinking
set -xU LESS_TERMCAP_md (printf "\e[01;31m")      # begin bold
set -xU LESS_TERMCAP_me (printf "\e[0m")          # end mode
set -xU LESS_TERMCAP_se (printf "\e[0m")          # end standout-mode
set -xU LESS_TERMCAP_so (printf "\e[01;44;33m")   # begin standout-mode - info box
set -xU LESS_TERMCAP_ue (printf "\e[0m")          # end underline
set -xU LESS_TERMCAP_us (printf "\e[01;32m")      # begin underline

# Miscelaneous abbreviations
abbr -a yas='yaourt -S'
abbr -a yass='yaourt -Ss'
abbr -a yard='yaourt -Rd'
abbr -a yasyu='yaourt -Syu'
abbr -a yasyy='yaourt -Syy'
abbr -a ss='sudo systemctl'
abbr -a ssu='systemctl --user'
abbr -a ec='emacsclient -c -n'
abbr -a e='emacsclient -n'
abbr -a ect='emacsclient -nw'
abbr -a p='pass -c'
abbr -a ll='ls -lh'
abbr -a lla='ll -a'
abbr -a pacleandeps='sudo pacman -Rns (pacman -Qdtq)'
abbr -a be='bundle exec'
abbr -a be='bundle exec rake'
