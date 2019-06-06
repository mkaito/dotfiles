# Disable greeting message on load
set -U fish_greeting
# eval sh /usr/share/base16-shell/base16-eighties.dark.sh
# source /usr/share/fish/config-fisherman.fish

# set -gx PAGER "less"
# set -gx READNULLCMD "$PAGER"
# set -gx EDITOR "emacsclient"
# set -gx GIT_EDITOR $EDITOR
# set -gx SUDO_EDITOR $EDITOR
# set -gx VISUAL "emacsclient -c"
# set -gx ALTERNATE_EDITOR ””
# set -gx BROWSER "qutebrowser.sh"
# set -gx XTERM "$HOME/dev/bin/terminal"
# set -gx TERMINAL "$XTERM"
# set -gx PERL_SIGNALS "unsafe"
# set -gx PATH $HOME/dev/bin $GOPATH/bin $HOME/perl5/bin $HOME/.cabal/bin $HOME/node_modules/.bin $PATH
# set -gx LEDGER_FILE "$HOME/media/sync/ledger/personal.ledger"

# Set up rbenv
# set -gx RBENV_ROOT $HOME/.rbenv
# status --is-interactive; and . (rbenv init - | psub)

# Colorize man pages
# set -xU LESS_TERMCAP_mb (printf "\e[01;31m")      # begin blinking
# set -xU LESS_TERMCAP_md (printf "\e[01;31m")      # begin bold
# set -xU LESS_TERMCAP_me (printf "\e[0m")          # end mode
# set -xU LESS_TERMCAP_se (printf "\e[0m")          # end standout-mode
# set -xU LESS_TERMCAP_so (printf "\e[01;44;33m")   # begin standout-mode - info box
# set -xU LESS_TERMCAP_ue (printf "\e[0m")          # end underline
# set -xU LESS_TERMCAP_us (printf "\e[01;32m")      # begin underline

# Miscelaneous abbreviations
alias yas='yaourt -S'
alias yass='yaourt -Ss'
alias yard='yaourt -Rd'
alias yasyu='yaourt -Syu'
alias yasyy='yaourt -Syy'
alias ss='sudo systemctl'
alias ssu='systemctl --user'
alias ec='emacsclient -c -n'
alias e='emacsclient -n'
alias ect='emacsclient -nw'
alias p='pass -c'
alias ll='ls -lh'
alias lla='ll -a'
alias pacleandeps='sudo pacman -Rns (pacman -Qdtq)'
alias be='bundle exec'
alias ber='bundle exec rake'
alias vim='nvim'
